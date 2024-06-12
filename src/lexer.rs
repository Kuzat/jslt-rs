use std::iter::Peekable;
use std::str::Chars;

// Implement just the token neccessary for the parser to work with literals
// and variables. The rest of the tokens will be implemented as needed.
// Example jslt file with just literals:
// {
//     "foo": "bar",
//     "baz": 1,
//     "qux": true,
//     "quux": null,
//     "corge": [1, 2, 3],
//     "grault": {
//         "garply": "waldo"
//     },
// }
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    StringLiteral(String),
    NumberLiteral(i64),
    BooleanLiteral(bool),
    NullLiteral,
    Identifier(String),
    RightBrace,   // }
    LeftBrace,    // {
    RightBracket, // ]
    LeftBracket,  // [
    Colon,        // :
    Comma,        // ,
    Dot,          // .
    Equal,        // =
    Let,          // let
    Dollar,       // $
    EndOfInput,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LexerError {
    UnexpectedCharacter(char),
    NumberParseError(std::num::IntErrorKind),
    UnexpectedEndOfInput,
}

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            input: input.chars().peekable(),
        }
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();
        loop {
            match self.next_token() {
                Ok(Token::EndOfInput) => break,
                Ok(token) => tokens.push(token),
                Err(error) => {
                    dbg!(tokens);
                    return Err(error)
                },
            }
        }

        Ok(tokens)
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        match self.input.peek() {
            Some(&c) => match c {
                // skip whitespace
                ' ' | '\t' | '\n' | '\r' => {
                    self.input.next();
                    self.next_token()
                }
                '{' => {
                    self.input.next();
                    Ok(Token::LeftBrace)
                }
                '}' => {
                    self.input.next();
                    Ok(Token::RightBrace)
                }
                '[' => {
                    self.input.next();
                    Ok(Token::LeftBracket)
                }
                ']' => {
                    self.input.next();
                    Ok(Token::RightBracket)
                }
                '=' => {
                    self.input.next();
                    Ok(Token::Equal)
                }
                '$' => {
                    self.input.next();
                    Ok(Token::Dollar)
                }
                ':' => {
                    self.input.next();
                    Ok(Token::Colon)
                }
                ',' => {
                    self.input.next();
                    Ok(Token::Comma)
                }
                '.' => {
                    self.input.next();
                    Ok(Token::Dot)
                }
                '"' => {
                    self.input.next();
                    self.read_string_literal()
                }
                'a'..='z' | 'A'..='Z' | '_' => self.read_identifier(),
                '0'..='9' => self.read_number_literal(),
                _ => Err(LexerError::UnexpectedCharacter(c)),
            },
            None => Ok(Token::EndOfInput),
        }
    }

    fn read_identifier(&mut self) -> Result<Token, LexerError> {
        let mut identifier = String::new();
        while let Some(&c) = self.input.peek() {
            match c {
                'a'..='z' | 'A'..='Z' | '_' => {
                    identifier.push(c);
                    self.input.next();
                }
                _ => break,
            }
        }

        match identifier.as_str() {
            "true" => Ok(Token::BooleanLiteral(true)),
            "false" => Ok(Token::BooleanLiteral(false)),
            "null" => Ok(Token::NullLiteral),
            "let" => Ok(Token::Let),
            _ => Ok(Token::Identifier(identifier)),
        }
    }

    fn read_string_literal(&mut self) -> Result<Token, LexerError> {
        let mut literal = String::new();
        loop {
            match self.input.next() {
                Some('"') => break,
                Some(c) => literal.push(c),
                None => return Err(LexerError::UnexpectedEndOfInput),
            }
        }

        Ok(Token::StringLiteral(literal))
    }

    fn read_number_literal(&mut self) -> Result<Token, LexerError> {
        let mut literal = String::new();
        while let Some(&c) = self.input.peek() {
            match c {
                '0'..='9' => {
                    literal.push(c);
                    self.input.next();
                }
                'a'..='z' | 'A'..='Z' | '_' => return Err(LexerError::UnexpectedCharacter(c)),
                _ => break,
            }
        }

        // Parse int and return error if it fails
        Ok(Token::NumberLiteral(literal.parse::<i64>().map_err(
            |e| LexerError::NumberParseError(e.kind().clone()),
        )?))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token_string_literal() {
        let mut lexer = Lexer::new("\"foo\"");
        assert_eq!(
            lexer.next_token().unwrap(),
            Token::StringLiteral("foo".to_string())
        );
    }

    #[test]
    fn test_next_token_string_literal_should_fail_with_no_closing_quote() {
        let mut lexer = Lexer::new("\"foo");
        assert_eq!(
            lexer.next_token().unwrap_err(),
            LexerError::UnexpectedEndOfInput
        );
    }

    #[test]
    fn test_next_token_number_literal() {
        let mut lexer = Lexer::new("1");
        assert_eq!(lexer.next_token().unwrap(), Token::NumberLiteral(1));
    }

    #[test]
    fn test_next_token_number_literal_should_fail_with_non_digit() {
        let mut lexer = Lexer::new("1a");
        assert_eq!(
            lexer.next_token().unwrap_err(),
            LexerError::UnexpectedCharacter('a')
        );
    }

    #[test]
    fn test_next_token_number_literal_should_fail_with_unparsable_digit() {
        let mut lexer = Lexer::new("10000000000000000000000000000000000000000000000000000");
        assert_eq!(
            lexer.next_token().unwrap_err(),
            LexerError::NumberParseError(std::num::IntErrorKind::PosOverflow)
        );
    }

    #[test]
    fn test_next_token_boolean_literal() {
        let mut lexer = Lexer::new("true");
        assert_eq!(lexer.next_token().unwrap(), Token::BooleanLiteral(true));
    }

    #[test]
    fn test_next_token_null_literal() {
        let mut lexer = Lexer::new("null");
        assert_eq!(lexer.next_token().unwrap(), Token::NullLiteral);
    }

    #[test]
    fn test_next_token_identifier() {
        let mut lexer = Lexer::new("foo");
        assert_eq!(
            lexer.next_token().unwrap(),
            Token::Identifier("foo".to_string())
        );
    }

    #[test]
    fn test_next_token_with_multiple_tokens() {
        let mut lexer = Lexer::new(r#"{"foo": 1}"#);
        assert_eq!(lexer.next_token().unwrap(), Token::LeftBrace);
        assert_eq!(
            lexer.next_token().unwrap(),
            Token::StringLiteral("foo".to_string())
        );
        assert_eq!(lexer.next_token().unwrap(), Token::Colon);
        assert_eq!(lexer.next_token().unwrap(), Token::NumberLiteral(1));
        assert_eq!(lexer.next_token().unwrap(), Token::RightBrace);
    }

    #[test]
    fn test_next_token_dot() {
        let mut lexer = Lexer::new(".");
        assert_eq!(lexer.next_token().unwrap(), Token::Dot);
    }
}
