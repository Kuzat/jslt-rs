use std::collections::HashMap;

use crate::{
    ast::{
        ArrayNode, Expression, IdentifierNode, LiteralNode, ObjectNode, ObjectPropertyAccessNode,
    },
    lexer::{Lexer, Token},
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

    pub fn single_expression(source: String) -> Result<Box<Expression>, ParseError> {
        let mut lexer = Lexer::new(&source);
        let tokens = lexer.lex().map_err(|_| ParseError::UnexpectedEndOfInput)?;
        let mut parser = Parser::new(tokens);

        parser.expression()
    }

    pub fn parse(&mut self) -> Result<Box<Expression>, ParseError> {
        // Should return the last expression in the program
        let mut last_expression = self.expression()?;

        while self.current < self.tokens.len() - 1 {
            self.advance();
            match &self.tokens[self.current] {
                Token::EndOfInput => break,
                _ => last_expression = self.expression()?,
            }
        }

        Ok(last_expression)
    }

    fn advance(&mut self) {
        self.current += 1;
    }

    fn peek(&self, n: usize) -> &Token {
        if self.current + n >= self.tokens.len() {
            return &Token::EndOfInput;
        }
        &self.tokens[self.current + n]
    }

    fn expression(&mut self) -> Result<Box<Expression>, ParseError> {
        match &self.tokens[self.current] {
            Token::LeftBrace => self.object(),
            Token::LeftBracket => self.array(),
            Token::Dollar => self.variable_access(),
            Token::Let => self.variable_assignment(),
            Token::Dot => self.object_property_access(Box::new(Expression::RootObject)),
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
                ));
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
                    ));
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
                    ));
                }
            }
        }

        Ok(Box::new(Expression::Array(ArrayNode { elements })))
    }

    fn identifier(&mut self, value: String) -> Result<Box<Expression>, ParseError> {
        // if followed by a dot then it is an object property access
        if self.peek(1) == &Token::Dot {
            return self.object_property_access(Box::new(Expression::Identifier(IdentifierNode {
                name: value,
            })));
        }

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

    fn object_property_access(
        &mut self,
        object: Box<Expression>,
    ) -> Result<Box<Expression>, ParseError> {
        let property = match self.peek(1) {
            Token::Identifier(value) => value.to_string(),
            Token::StringLiteral(value) => value.to_string(),
            // If it just a single dot then it is the root object, so if the next token is a comma or closing brace then it is the end of the object
            Token::RightBrace | Token::Comma => return Ok(object),
            _ => {
                return Err(ParseError::UnexpectedToken(
                    self.tokens[self.current].clone(),
                ));
            }
        };
        self.advance();

        let property_access =
            Box::new(Expression::ObjectPropertyAccess(ObjectPropertyAccessNode {
                object,
                property,
            }));

        // check if there are further property accesses
        if self.peek(1) == &Token::Dot {
            self.advance();
            self.object_property_access(property_access)
        } else {
            Ok(property_access)
        }
    }
    fn variable_access(&self) -> Result<Box<Expression>, ParseError> {
        todo!()
    }
    fn variable_assignment(&mut self) -> Result<Box<Expression>, ParseError> {
        // Let <identifier> = <expression>
        self.advance();
        let identifier = match &self.tokens[self.current] {
            Token::Identifier(value) => value.to_string(),
            _ => {
                return Err(ParseError::UnexpectedToken(
                    self.tokens[self.current].clone(),
                ));
            }
        };

        self.advance();
        match &self.tokens[self.current] {
            Token::Equal => self.advance(),
            _ => {
                return Err(ParseError::UnexpectedToken(
                    self.tokens[self.current].clone(),
                ));
            }
        }

        let expression = self.expression()?;

        Ok(Box::new(Expression::VariableAssignment(
            IdentifierNode { name: identifier },
            expression,
        )))
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
        let mut parser = empty_parser();
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

    #[test]
    fn test_object_property_access() {
        let mut parser = Parser::new(vec![Token::Dot, Token::Identifier("foo".to_string())]);
        let result = parser.object_property_access(Box::new(Expression::RootObject));
        let expression = result.unwrap();
        assert_eq!(
            *expression,
            Expression::ObjectPropertyAccess(ObjectPropertyAccessNode {
                object: Box::new(Expression::RootObject),
                property: "foo".to_string(),
            })
        );
    }

    #[test]
    fn test_multiple_object_property_access() {
        let mut parser = Parser::new(vec![
            Token::Dot,
            Token::Identifier("foo".to_string()),
            Token::Dot,
            Token::Identifier("bar".to_string()),
        ]);
        let result = parser.object_property_access(Box::new(Expression::RootObject));
        let expression = result.unwrap();
        assert_eq!(
            *expression,
            Expression::ObjectPropertyAccess(ObjectPropertyAccessNode {
                object: Box::new(Expression::ObjectPropertyAccess(ObjectPropertyAccessNode {
                    object: Box::new(Expression::RootObject),
                    property: "foo".to_string(),
                })),
                property: "bar".to_string(),
            })
        );
    }

    #[test]
    fn test_object_property_access_with_string_literal() {
        let mut parser = Parser::new(vec![Token::Dot, Token::StringLiteral("foo".to_string())]);
        let result = parser.object_property_access(Box::new(Expression::RootObject));
        let expression = result.unwrap();
        assert_eq!(
            *expression,
            Expression::ObjectPropertyAccess(ObjectPropertyAccessNode {
                object: Box::new(Expression::RootObject),
                property: "foo".to_string(),
            })
        );
    }

    // TODO: Need to revaluate how parse works. It should return a single expression last expression.
    #[test]
    fn test_parse_should_return_last_expression() {
        let mut parser = Parser::new(vec![
            Token::LeftBracket,
            Token::NumberLiteral(1),
            Token::Comma,
            Token::NumberLiteral(2),
            Token::Comma,
            Token::NumberLiteral(3),
            Token::RightBracket,
            Token::LeftBrace,
            Token::StringLiteral("foo".to_string()),
            Token::Colon,
            Token::NumberLiteral(1),
            Token::RightBrace,
        ]);
        let result = parser.parse();
        let expression = result.unwrap();
        assert_eq!(
            *expression,
            Expression::Object(ObjectNode {
                properties: {
                    let mut properties: HashMap<String, Box<Expression>> = HashMap::new();
                    properties.insert(
                        "foo".to_string(),
                        Box::new(Expression::NumberLiteral(LiteralNode { value: 1 })),
                    );
                    properties
                }
            })
        );
    }
}
