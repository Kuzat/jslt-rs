use std::collections::HashMap;

use crate::ast::ObjectNode;
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

        // Evaluate the input
        self.evaluate(input)
    }

    fn evaluate(&mut self, input: Expression) -> Result<Expression, InterpreterError> {
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
            Expression::Object(node) => {
                // interpret all the properties
                let mut properties = HashMap::new();
                for (key, value) in node.properties {
                    let interpreted_value = self.evaluate(*value)?;
                    properties.insert(key, Box::new(interpreted_value));
                }

                Ok(Expression::Object(ObjectNode {
                    properties: properties,
                }))
            }
            Expression::Array(node) => Ok(Expression::Array(node)),
            // TODO: Should be evaluated in a new environment with the arguments set as variables
            Expression::Function(node) => Ok(Expression::Function(node)),
            // TODO: Should be evaluated to the value of the property
            Expression::ObjectPropertyAccess(node) => {
                // TODO: Should also check if we are accessing a object or the root object
                // For now we assume we are accessing the root object
                let object = self.runtime.get_variable("root").unwrap();
                let property_value = self.runtime.get_object_property(object, &node.property);
                property_value.map(|value| *value.clone()).ok_or_else(|| {
                    InterpreterError::RuntimeError(format!(
                        "Property {} does not exist on object",
                        node.property
                    ))
                })
            }
        }
    }
}
