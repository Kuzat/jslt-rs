use crate::runtime::Runtime;
use crate::{ast::Expression, lexer::LexerError, parser::ParseError};

#[derive(Debug, PartialEq)]
pub enum InterpreterError {
    LexerError(LexerError),
    ParserError(ParseError),
    UndefinedVariable(String),
    RuntimeError(String),
}

impl From<LexerError> for InterpreterError {
    fn from(error: LexerError) -> Self {
        InterpreterError::LexerError(error)
    }
}

impl From<ParseError> for InterpreterError {
    fn from(error: ParseError) -> Self {
        InterpreterError::ParserError(error)
    }
}

pub struct Interpreter {
    runtime: Runtime,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            runtime: Runtime::new(),
        }
    }

    pub fn interpret(
        &mut self,
        input: Expression,
        json_input: Expression,
    ) -> Result<Expression, InterpreterError> {
        // Sets the json_input as the root variable
        self.runtime.set_variable("root", Box::new(json_input));

        match input {
            Expression::StringLiteral(node) => Ok(Expression::StringLiteral(node)),
            Expression::NumberLiteral(node) => Ok(Expression::NumberLiteral(node)),
            Expression::BooleanLiteral(node) => Ok(Expression::BooleanLiteral(node)),
            Expression::NullLiteral => Ok(Expression::NullLiteral),
            Expression::Identifier(node) => {
                let value = self.runtime.get_variable(&node.name);
                match value {
                    Some(value) => Ok(*value.clone()),
                    None => Err(InterpreterError::UndefinedVariable(node.name)),
                }
            }
            Expression::Object(node) => Ok(Expression::Object(node)),
            Expression::Array(node) => Ok(Expression::Array(node)),
            // Should be evaluated in a new environment with the arguments set as variables
            Expression::Function(node) => Ok(Expression::Function(node)),
        }
    }
}
