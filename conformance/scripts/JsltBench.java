// Times benchmark workloads with Java JSLT, using the same method as
// crates/engine/examples/bench_runner.rs.
//
// Usage: java -cp <jslt-all.jar>:<classes> JsltBench DIR [--out DIR] [--warmup-ms N] [--measure-ms N] [NAME...]
//   DIR holds <name>.jslt and <name>.json files (written by `bench_runner --dump DIR`).
//
// Prints one `name<TAB>median_ns<TAB>min_ns` line per workload.

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.schibsted.spt.data.jslt.Expression;
import com.schibsted.spt.data.jslt.Parser;
import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class JsltBench {
  private static final long BATCH_TARGET_NS = 20_000_000L;
  private static volatile Object sink;

  public static void main(String[] args) throws Exception {
    File dir = new File(args[0]);
    File out = null;
    long warmupNs = 1_000_000_000L;
    long measureNs = 3_000_000_000L;
    List<String> names = new ArrayList<>();
    for (int i = 1; i < args.length; i++) {
      switch (args[i]) {
        case "--out": out = new File(args[++i]); break;
        case "--warmup-ms": warmupNs = Long.parseLong(args[++i]) * 1_000_000L; break;
        case "--measure-ms": measureNs = Long.parseLong(args[++i]) * 1_000_000L; break;
        default: names.add(args[i]);
      }
    }
    if (names.isEmpty()) {
      String[] files = dir.list((d, n) -> n.endsWith(".jslt"));
      Arrays.sort(files);
      for (String f : files)
        names.add(f.substring(0, f.length() - ".jslt".length()));
    }

    ObjectMapper mapper = new ObjectMapper();
    for (String name : names) {
      String program = read(new File(dir, name + ".jslt"));
      JsonNode input = mapper.readTree(new File(dir, name + ".json"));
      Expression expr = Parser.compileString(program);

      if (out != null) {
        out.mkdirs();
        Files.write(new File(out, name + ".out.json").toPath(),
                    mapper.writeValueAsBytes(expr.apply(input)));
      }

      long[] result = time(expr, input, warmupNs, measureNs);
      System.out.println(name + "\t" + result[0] + "\t" + result[1]);
    }
  }

  // Runs for warmupNs, then in batches of about BATCH_TARGET_NS for measureNs.
  // Returns the median and minimum nanoseconds per call over the batches.
  private static long[] time(Expression expr, JsonNode input, long warmupNs, long measureNs) {
    long start = System.nanoTime();
    long calls = 0;
    while (System.nanoTime() - start < warmupNs || calls == 0) {
      sink = expr.apply(input);
      calls++;
    }
    long perCall = (System.nanoTime() - start) / calls;
    long batch = Math.max(1, BATCH_TARGET_NS / Math.max(1, perCall));

    List<Long> samples = new ArrayList<>();
    start = System.nanoTime();
    while (System.nanoTime() - start < measureNs || samples.size() < 5) {
      long t = System.nanoTime();
      for (long i = 0; i < batch; i++)
        sink = expr.apply(input);
      samples.add((System.nanoTime() - t) / batch);
    }
    samples.sort(null);
    return new long[] { samples.get(samples.size() / 2), samples.get(0) };
  }

  private static String read(File f) throws Exception {
    return new String(Files.readAllBytes(f.toPath()), StandardCharsets.UTF_8);
  }
}
