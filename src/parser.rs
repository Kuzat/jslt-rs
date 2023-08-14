use std::collections::HashMap;

use crate::{
    ast::{ArrayNode, Expression, IdentifierNode, LiteralNode, ObjectNode},
    lexer::Token,
};

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedToken(Token),
    UnexpectedEndOfInput,
}

pub struct Parser {
    pub tokens: Vec<Token>,
    pub current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Box<Expression>, ParseError> {
        self.expression()
    }

    fn advance(&mut self) {
        self.current += 1;
    }

    fn expression(&mut self) -> Result<Box<Expression>, ParseError> {
        match &self.tokens[self.current] {
            Token::LeftBrace => self.object(),
            Token::LeftBracket => self.array(),
            Token::Identifier(value) => self.identifier(value.to_string()),
            Token::StringLiteral(value) => self.string_literal(value.to_string()),
            Token::NumberLiteral(value) => self.number_literal(value.to_owned()),
            Token::BooleanLiteral(value) => self.boolean_literal(value.to_owned()),
            Token::NullLiteral => self.null_literal(),
            _ => Err(ParseError::UnexpectedToken(
                self.tokens[self.current].clone(),
            )),
        }
    }

    fn property(&mut self) -> Result<(String, Box<Expression>), ParseError> {
        let key = match &self.tokens[self.current] {
            Token::StringLiteral(value) => value.to_string(),
            _ => {
                return Err(ParseError::UnexpectedToken(
                    self.tokens[self.current].clone(),
                ))
            }
        };

        self.advance();

        match &self.tokens[self.current] {
            Token::Colon => {
                self.advance();
                Ok((key, self.expression()?))
            }
            _ => Err(ParseError::UnexpectedToken(
                self.tokens[self.current].clone(),
            )),
        }
    }

    fn object(&mut self) -> Result<Box<Expression>, ParseError> {
        let mut properties: HashMap<String, Box<Expression>> = HashMap::new();

        // Skip the left brace.
        self.advance();

        // Handle empty object case.
        if let Token::RightBrace = &self.tokens[self.current] {
            return Ok(Box::new(Expression::Object(ObjectNode { properties })));
        }

        // Handle the first property.
        // This is a special case because we don't have a comma token to match on first property.
        let (key, value) = self.property()?;
        properties.insert(key, value);

        loop {
            self.advance();
            match &self.tokens[self.current] {
                Token::RightBrace => break,
                Token::Comma => {
                    self.advance();
                    let (key, value) = self.property()?;
                    properties.insert(key, value);
                }
                Token::EndOfInput => return Err(ParseError::UnexpectedEndOfInput),
                _ => {
                    return Err(ParseError::UnexpectedToken(
                        self.tokens[self.current].clone(),
                    ))
                }
            }
        }

        Ok(Box::new(Expression::Object(ObjectNode { properties })))
    }

    fn array(&mut self) -> Result<Box<Expression>, ParseError> {
        let mut elements: Vec<Box<Expression>> = Vec::new();

        // Skip the left bracket.
        self.advance();

        // Handle the empty array case.
        if let Token::RightBracket = &self.tokens[self.current] {
            return Ok(Box::new(Expression::Array(ArrayNode { elements })));
        }

        // Handle the first element.
        // This is a special case because we don't have a comma token to match on first element.
        elements.push(self.expression()?);
        
        loop {
            self.advance();
            match &self.tokens[self.current] {
                Token::RightBracket => break,
                Token::Comma => {
                    self.advance();
                    elements.push(self.expression()?);
                }
                Token::EndOfInput => return Err(ParseError::UnexpectedEndOfInput),
                _ => {
                    return Err(ParseError::UnexpectedToken(
                        self.tokens[self.current].clone(),
                    ))
                }
            }
        }

        Ok(Box::new(Expression::Array(ArrayNode { elements })))
    }

    fn identifier(&self, value: String) -> Result<Box<Expression>, ParseError> {
        Ok(Box::new(Expression::Identifier(IdentifierNode {
            name: value,
        })))
    }

    fn string_literal(&self, value: String) -> Result<Box<Expression>, ParseError> {
        Ok(Box::new(Expression::StringLiteral(LiteralNode { value })))
    }

    fn number_literal(&self, value: i64) -> Result<Box<Expression>, ParseError> {
        Ok(Box::new(Expression::NumberLiteral(LiteralNode { value })))
    }

    fn boolean_literal(&self, value: bool) -> Result<Box<Expression>, ParseError> {
        Ok(Box::new(Expression::BooleanLiteral(LiteralNode { value })))
    }

    fn null_literal(&self) -> Result<Box<Expression>, ParseError> {
        Ok(Box::new(Expression::NullLiteral))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn empty_parser() -> Parser {
        Parser {
            tokens: vec![],
            current: 0,
        }
    }

    #[test]
    fn test_number_literal() {
        let parser = empty_parser();
        let result = parser.number_literal(42);
        assert!(result.is_ok());
        let expression = result.unwrap();
        assert_eq!(
            *expression,
            Expression::NumberLiteral(LiteralNode { value: 42 })
        );
    }

    #[test]
    fn test_boolean_literal() {
        let parser = empty_parser();
        let result = parser.boolean_literal(true);
        assert!(result.is_ok());
        let expression = result.unwrap();
        assert_eq!(
            *expression,
            Expression::BooleanLiteral(LiteralNode { value: true })
        );
    }

    #[test]
    fn test_null_literal() {
        let parser = empty_parser();
        let result = parser.null_literal();
        assert!(result.is_ok());
        let expression = result.unwrap();
        assert_eq!(*expression, Expression::NullLiteral);
    }

    #[test]
    fn test_string_literal() {
        let parser = empty_parser();
        let result = parser.string_literal("foo".to_string());
        assert!(result.is_ok());
        let expression = result.unwrap();
        assert_eq!(
            *expression,
            Expression::StringLiteral(LiteralNode {
                value: "foo".to_string()
            })
        );
    }

    #[test]
    fn test_identifier() {
        let parser = empty_parser();
        let result = parser.identifier("foo".to_string());
        assert!(result.is_ok());
        let expression = result.unwrap();
        assert_eq!(
            *expression,
            Expression::Identifier(IdentifierNode {
                name: "foo".to_string()
            })
        );
    }

    #[test]
    fn test_empty_array() {
        let mut parser = Parser::new(vec![Token::LeftBracket, Token::RightBracket]);
        let result = parser.array();
        let expression = result.unwrap();
        assert_eq!(
            *expression,
            Expression::Array(ArrayNode { elements: vec![] })
        );
    }

    #[test]
    fn test_array_with_one_element() {
        let mut parser = Parser::new(vec![
            Token::LeftBracket,
            Token::NumberLiteral(1),
            Token::RightBracket,
        ]);
        let result = parser.array();
        let expression = result.unwrap();
        assert_eq!(
            *expression,
            Expression::Array(ArrayNode {
                elements: vec![Box::new(Expression::NumberLiteral(LiteralNode {
                    value: 1
                }))]
            })
        );
    }

    #[test]
    fn test_array_with_multiple_elements_of_same_type() {
        let mut parser = Parser::new(vec![
            Token::LeftBracket,
            Token::NumberLiteral(1),
            Token::Comma,
            Token::NumberLiteral(2),
            Token::Comma,
            Token::NumberLiteral(3),
            Token::RightBracket,
        ]);
        let result = parser.array();
        let expression = result.unwrap();
        assert_eq!(
            *expression,
            Expression::Array(ArrayNode {
                elements: vec![
                    Box::new(Expression::NumberLiteral(LiteralNode { value: 1 })),
                    Box::new(Expression::NumberLiteral(LiteralNode { value: 2 })),
                    Box::new(Expression::NumberLiteral(LiteralNode { value: 3 })),
                ]
            })
        );
    }

    #[test]
    fn test_array_with_multiple_elements_of_different_types() {
        let mut parser = Parser::new(vec![
            Token::LeftBracket,
            Token::NumberLiteral(1),
            Token::Comma,
            Token::StringLiteral("foo".to_string()),
            Token::Comma,
            Token::BooleanLiteral(true),
            Token::RightBracket,
        ]);
        let result = parser.array();
        let expression = result.unwrap();
        assert_eq!(
            *expression,
            Expression::Array(ArrayNode {
                elements: vec![
                    Box::new(Expression::NumberLiteral(LiteralNode { value: 1 })),
                    Box::new(Expression::StringLiteral(LiteralNode {
                        value: "foo".to_string()
                    })),
                    Box::new(Expression::BooleanLiteral(LiteralNode { value: true })),
                ]
            })
        );
    }
}
