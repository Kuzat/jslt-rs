use std::collections::HashMap;

use ast::{Expression, LiteralNode, ObjectNode};
use interpreter::Interpreter;
use lexer::Lexer;
use parser::Parser;

pub mod ast;
pub mod error_handling;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod runtime;
pub mod standard_library;

#[derive(Debug)]
pub enum JsltError {
    LexerError(lexer::LexerError),
    ParserError(parser::ParseError),
    InterpreterError(interpreter::InterpreterError),
}

// Contains a compiled JSLT expression. The expressions can be applied to JSON strings.
pub struct Jslt {
    interpreter: Interpreter,
    program: Box<Expression>,
}

impl Jslt {
    // Compiles the JSLT expression string into an AST function that accepts a JSON string and returns a JSON string.
    pub fn compile(jslt: &str) -> Jslt {
        let mut lexer = Lexer::new(jslt);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new(tokens);

        Jslt {
            interpreter: Interpreter::new(),
            program: parser.parse().unwrap(),
        }
    }

    pub fn apply(&mut self, json_input: &str) -> Result<String, JsltError> {
        // Convert the json_input into a Expression
        let json_input = Parser::single_expression(json_input.to_string()).unwrap();
        
        // Call the interpreter with the expression and input
        let output = self
            .interpreter
            .interpret(*self.program.to_owned(), *json_input)
            .unwrap();

        // Convert the output into a json string
        serde_json::to_string(&output).map_err(|e| {
            JsltError::InterpreterError(interpreter::InterpreterError::RuntimeError(e.to_string()))
        })
    }
}
