use ast::Program;

/// Temporary stub: parse a JSLT program from source into an AST Program.
/// Not implemented yet — returns an error. Exists so tests can compile.
pub fn parse_program(_src: &str) -> Result<Program, String> {
    Err("parser not implemented".into())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn norm_ws(s: &str) -> String {
        // Collapse all runs of whitespace to a single space and trim
        let mut out = String::new();
        let mut prev_space = false;
        for ch in s.chars() {
            if ch.is_whitespace() {
                if !prev_space {
                    out.push(' ');
                    prev_space = true;
                }
            } else {
                prev_space = false;
                out.push(ch);
            }
        }
        out.trim().to_string()
    }

    #[test]
    #[ignore]
    fn round_trip_pretty_parse_pretty_is_stable_simple() {
        // Once parse_program is implemented, this ensures pretty-print stability ignoring whitespace.
        let src = "let a = 1;\n${a} + 2";
        let program = parse_program(src).expect("parse ok");
        let p1 = format!("{}", program);
        let program2 = parse_program(&p1).expect("parse ok 2");
        let p2 = format!("{}", program2);
        assert_eq!(norm_ws(&p1), norm_ws(&p2));
    }

    #[test]
    #[ignore]
    fn round_trip_with_objects_arrays_and_calls() {
        let src = "def f(x) x\nlet a = 1, b = \"s\";\n{for ([1,2,3]) \"k\": f(${a})}[0]";
        let program = parse_program(src).expect("parse ok");
        let p1 = format!("{}", program);
        let program2 = parse_program(&p1).expect("parse ok 2");
        let p2 = format!("{}", program2);
        assert_eq!(norm_ws(&p1), norm_ws(&p2));
    }
}
