//! Benchmark workloads shared by the criterion bench and the `bench_runner` example.
//!
//! Each workload is a program plus a deterministic input, so runs are comparable over time and
//! with Java JSLT.

use serde_json::{json, Value};

pub struct Workload {
    pub name: &'static str,
    pub program: &'static str,
    pub input: fn() -> Value,
}

pub const EVENT_COUNT: usize = 1_000;

pub fn all() -> Vec<Workload> {
    vec![
        Workload { name: "transform", program: include_str!("transform.jslt"), input: events },
        Workload { name: "matcher", program: include_str!("matcher.jslt"), input: events },
        Workload { name: "aggregate", program: include_str!("aggregate.jslt"), input: events },
        Workload { name: "strings", program: include_str!("strings.jslt"), input: events },
        Workload { name: "time", program: include_str!("time.jslt"), input: events },
        Workload {
            name: "large_object",
            program: include_str!("large_object.jslt"),
            input: events,
        },
        Workload { name: "identity", program: include_str!("identity.jslt"), input: events },
        Workload { name: "numeric", program: include_str!("numeric.jslt"), input: numbers },
        Workload { name: "fib", program: include_str!("fib.jslt"), input: || json!({"n": 20}) },
        Workload { name: "queens", program: include_str!("queens.jslt"), input: || json!({}) },
        Workload { name: "cdp", program: include_str!("cdp.jslt"), input: cdp_input },
    ]
}

/// Small linear congruential generator, so inputs are identical on every run.
struct Lcg(u64);

impl Lcg {
    fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
        self.0 >> 33
    }
    fn below(&mut self, n: u64) -> u64 {
        self.next() % n
    }
}

/// `{"events": [...]}` with `EVENT_COUNT` user events.
pub fn events() -> Value {
    const TYPES: [&str; 4] = ["view", "click", "purchase", "signup"];
    const FIRST: [&str; 5] = ["Ada", "Linus", "Grace", "Alan", "Barbara"];
    const LAST: [&str; 5] = ["Lovelace", "Torvalds", "Hopper", "Turing", "Liskov"];
    let mut rng = Lcg(42);
    let events: Vec<Value> = (0..EVENT_COUNT)
        .map(|i| {
            let uid = rng.below(200);
            let first = FIRST[rng.below(5) as usize];
            let last = LAST[rng.below(5) as usize];
            let items: Vec<Value> = (0..1 + rng.below(4))
                .map(|j| {
                    json!({
                        "sku": format!("SKU-{}", rng.below(500)),
                        "qty": 1 + rng.below(5),
                        "price": (rng.below(10_000) as f64) / 100.0 + j as f64,
                    })
                })
                .collect();
            let attrs: serde_json::Map<String, Value> =
                (0..5).map(|k| (format!("k{k}"), json!(format!("v{}", rng.below(50))))).collect();
            json!({
                "id": format!("evt-{i:06}"),
                "ts": format!(
                    "2024-{:02}-{:02}T{:02}:{:02}:{:02}Z",
                    1 + rng.below(12),
                    1 + rng.below(28),
                    rng.below(24),
                    rng.below(60),
                    rng.below(60)
                ),
                "type": TYPES[rng.below(4) as usize],
                "amount": (rng.below(10_000) as f64) / 100.0,
                "user": {
                    "id": uid,
                    "name": format!(" {first} {last} "),
                    "email": format!("{first}.{last}{uid}@Example.COM"),
                    "tags": (0..rng.below(4)).map(|t| format!("tag{t}")).collect::<Vec<_>>(),
                },
                "items": items,
                "attrs": attrs,
            })
        })
        .collect();
    json!({ "events": events })
}

/// `{"numbers": [...]}` with 10 000 integers.
pub fn numbers() -> Value {
    let mut rng = Lcg(7);
    json!({ "numbers": (0..10_000).map(|_| rng.below(1_000)).collect::<Vec<_>>() })
}

/// The input of the upstream `user-external2cdp.jslt` test.
pub fn cdp_input() -> Value {
    serde_json::from_str(include_str!(
        "../../../../conformance/java-jslt/core/src/test/resources/user-external.json"
    ))
    .expect("valid user-external.json")
}
