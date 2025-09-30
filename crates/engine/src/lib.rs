use ast::Program;
use interp::binder::{BindError, BoundProgram};
use interp::{apply, bind, EvalConfig, RuntimeError};
use parser::Parser;
use serde_json::Value;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum EngineError {
    #[error("parse error: {0}")]
    Parse(#[from] parser::ParseError),
    #[error("bind error: {0}")]
    Bind(#[from] BindError),
    #[error("runtime error: {0}")]
    Runtime(#[from] RuntimeError),
}

#[derive(Debug, Clone)]
pub struct CompiledProgram {
    bound: BoundProgram,
}

impl CompiledProgram {
    pub fn apply(&self, input: &Value, cfg: Option<EvalConfig>) -> Result<Value, EngineError> {
        Ok(apply(&self.bound, input, cfg)?)
    }
}

pub fn compile(src: &str) -> Result<CompiledProgram, EngineError> {
    let mut parser = Parser::new(src)?;
    let ast: Program = parser.parse_program()?;
    let bound = bind(&ast)?;
    Ok(CompiledProgram { bound })
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn end_to_end_smoke_compile_and_apply() {
        // Program: { "sum": .a + .b, "greet": "hi " + .name }
        let src = r#"
        {
          "sum": .a + .b,
          "greet": "hi " + .name
        }
        "#;
        let prog = compile(src).expect("compile");
        let input = json!({"a": 2, "b": 3, "name": "alice"});
        let out = prog.apply(&input, None).expect("apply");
        assert_eq!(out, json!({"sum": 5.0, "greet": "hi alice"}));
    }
}
