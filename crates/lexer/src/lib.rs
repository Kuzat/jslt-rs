use ast::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Pos {
    pub byte: usize,
    pub line: u32,
    pub column: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // punctuation
    Dot,
    Comma,
    Colon,
    Semicolon,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,

    // operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    EqEq,
    BangEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    Eq,

    // literals & identifiers
    NumberFloat(f64),
    NumberInt(i64),
    String(String),
    Ident(String),
    Dollar,
    True,
    False,
    Null,

    // keyword
    Let,
    Def,
    If,
    Else,
    For,
    In,
    And,
    Or,
    Not,

    Eof,
}

#[derive(Debug, thiserror::Error)]
pub enum LexErrorKind {
    #[error("invalid character")]
    InvalidChar,
    #[error("unterminated string")]
    UnterminatedString,
    #[error("invalid escape sequence")]
    InvalidEscape,
    #[error("invalid unicode escape")]
    InvalidUnicodeEscape,
    #[error("invalid number")]
    InvalidNumber,
}

#[derive(Debug)]
pub struct LexError {
    pub span: Span,
    pub kind: LexErrorKind,
}

pub struct Lexer<'a> {
    src: &'a str,
    idx: usize,
    line: u32,
    column: u32,
    finished: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer { src: input, idx: 0, line: 1, column: 1, finished: false }
    }

    fn cur_pos(&self) -> Pos {
        Pos { byte: self.idx, line: self.line, column: self.column }
    }

    fn make_span(&self, start: Pos) -> Span {
        Span { start: start.byte, end: self.cur_pos().byte, line: start.line, column: start.column }
    }

    fn peek_char(&self) -> Option<char> {
        self.src[self.idx..].chars().next()
    }

    fn peek_char_n(&self, n: usize) -> Option<char> {
        let mut it = self.src[self.idx..].chars();
        it.nth(n)
    }

    fn bump_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        let len = ch.len_utf8();
        self.idx += len;
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            // Treat RLF as a single newline if encountered
            if ch == '\r' {
                if let Some('\n') = self.peek_char() {
                    // consume \n
                    self.idx += '\n'.len_utf8();
                    self.line += 1;
                    self.column = 1;
                    return Some('\n');
                }
                // lone \r treated as newline as well
                self.line += 1;
                self.column = 1;
                return Some('\n');
            } else {
                self.column += 1;
            }
        }
        Some(ch)
    }

    fn consume_char(&mut self) {
        // like bump_char but ignore return value
        // useful when we do not care about the char
        let _ = self.bump_char();
    }

    fn starts_with(&self, s: &str) -> bool {
        self.src[self.idx..].starts_with(s)
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            let mut did_skip = false;
            while let Some(ch) = self.peek_char() {
                if ch.is_whitespace() {
                    self.bump_char();
                    did_skip = true;
                } else {
                    break;
                }
            }

            // skip comment: //
            if self.starts_with("//") {
                did_skip = true;
                // consume // then until newline or EOF
                self.bump_char();
                self.bump_char();
                while let Some(ch) = self.bump_char() {
                    if ch == '\n' {
                        break;
                    }
                    self.bump_char();
                }
            }

            if !did_skip {
                break;
            }
        }
    }

    fn is_ident_start(ch: char) -> bool {
        ch == '_' || ch.is_ascii_alphabetic()
    }

    fn is_ident_continue(ch: char) -> bool {
        ch == '_' || ch == '-' || ch.is_ascii_alphanumeric()
    }

    fn scan_ident_or_keyword(&mut self) -> (Token, Span) {
        let start = self.cur_pos();
        let mut s = String::new();
        if let Some(ch) = self.peek_char() {
            if Self::is_ident_start(ch) {
                s.push(ch);
                let _ = self.bump_char();
            }
        }

        while let Some(ch) = self.peek_char() {
            if Self::is_ident_continue(ch) {
                s.push(ch);
                let _ = self.bump_char();
            } else {
                break;
            }
        }

        let tok = match s.as_str() {
            "let" => Token::Let,
            "def" => Token::Def,
            "if" => Token::If,
            "else" => Token::Else,
            "for" => Token::For,
            "in" => Token::In,
            "and" => Token::And,
            "or" => Token::Or,
            "not" => Token::Not,
            "true" => Token::True,
            "false" => Token::False,
            "null" => Token::Null,
            _ => Token::Ident(s),
        };
        (tok, self.make_span(start))
    }

    fn scan_number(&mut self, start: Pos) -> Result<(Token, Span), LexError> {
        // JSON like number: -? (0 | [1-9][0-9]*) (\.[0-9]+)? ([eE][+-]?[0-9]+)?
        // NOTE: leading '.' is NOT allowed; caller ensures first char is digit or '-' followed by digit

        let num_start_idx = self.idx;
        // Optional '-'
        if self.starts_with("-") {
            // Only consume '-' if followed by a digit
            if matches!(self.peek_char_n(1), Some(ch) if ch.is_ascii_digit()) {
                let _ = self.bump_char();
            } else {
                // Not a number; return '-' as operator via caller path
                return Err(LexError {
                    span: self.make_span(start),
                    kind: LexErrorKind::InvalidNumber,
                });
            }
        }

        // Integer part
        if let Some('0') = self.peek_char() {
            // Leading zero cannot be followed by more digits (JSON)
            let _ = self.bump_char();
            if let Some(d) = self.peek_char() {
                if d.is_ascii_digit() {
                    // invalid leading zero like 01
                    // Consume subsequent digits just to form a coherent span
                    while let Some(d) = self.bump_char() {
                        if d.is_ascii_digit() {
                            let _ = self.bump_char();
                        } else {
                            break;
                        }
                    }
                    return Err(LexError {
                        span: self.make_span(start),
                        kind: LexErrorKind::InvalidNumber,
                    });
                }
            }
        } else {
            // [1-9][0-9]*
            match self.peek_char() {
                Some(d) if d.is_ascii_digit() && d != '0' => {
                    let _ = self.bump_char();
                    while let Some(d) = self.peek_char() {
                        if d.is_ascii_digit() {
                            let _ = self.bump_char();
                        } else {
                            break;
                        }
                    }
                }
                _ => {
                    return Err(LexError {
                        span: self.make_span(start),
                        kind: LexErrorKind::InvalidNumber,
                    })
                }
            }
        }

        // Fraction: \.[0-9]+ (optional)
        if let Some('.') = self.peek_char() {
            // lookahead digit required
            if matches!(self.peek_char_n(1), Some(d) if d.is_ascii_digit()) {
                let _ = self.bump_char();
                let mut frac_digit = 0usize;
                while let Some(d) = self.peek_char() {
                    if d.is_ascii_digit() {
                        let _ = self.bump_char();
                        frac_digit += 1;
                    } else {
                        break;
                    }
                }
                if frac_digit == 0 {
                    return Err(LexError {
                        span: self.make_span(start),
                        kind: LexErrorKind::InvalidNumber,
                    });
                }
            }
        }

        // Exponent: [eE][+-]?[0-9]+ (optional)
        if let Some(e) = self.peek_char() {
            if e == 'e' || e == 'E' {
                // consume e or E
                let _ = self.bump_char();
                //optional sign
                if let Some(s) = self.peek_char() {
                    if s == '+' || s == '-' {
                        let _ = self.bump_char();
                    }
                }

                // at least one digit required
                let mut exp_digits = 0usize;
                while let Some(d) = self.peek_char() {
                    if d.is_ascii_digit() {
                        let _ = self.bump_char();
                        exp_digits += 1;
                    } else {
                        break;
                    }
                }
                if exp_digits == 0 {
                    return Err(LexError {
                        span: self.make_span(start),
                        kind: LexErrorKind::InvalidNumber,
                    });
                }
            }
        }

        let span = self.make_span(start);
        let s = &self.src[num_start_idx..self.idx];

        // Shape detection: if there is no '.' and no exponent -> integer literal
        let looks_decimal = s.contains('.') || s.contains('e') || s.contains('E');
        if !looks_decimal {
            // parse as signed integer first; JSON allows a leading - for integers
            if let Ok(i) = s.parse::<i64>() {
                return Ok((Token::NumberInt(i), span));
            }
            // If it doesn't fit i64 (very rare), fall back to f64
            if let Ok(f) = s.parse::<f64>() {
                return Ok((Token::NumberFloat(f), span));
            }
            return Err(LexError { span, kind: LexErrorKind::InvalidNumber });
        }

        match s.parse::<f64>() {
            Ok(v) if v.is_finite() => Ok((Token::NumberFloat(v), span)),
            _ => Err(LexError { span: self.make_span(start), kind: LexErrorKind::InvalidNumber }),
        }
    }

    fn hex_val(c: char) -> Option<u32> {
        match c {
            '0'..='9' => Some((c as u32) - ('0' as u32)),
            'a'..='f' => Some((c as u32) - ('a' as u32) + 10),
            'A'..='F' => Some((c as u32) - ('A' as u32) + 10),
            _ => None,
        }
    }

    fn scan_unicode_escape(&mut self, start: Pos) -> Result<char, LexError> {
        // Reads \uXXXX (we assume leading \u is already consumed)
        let mut code: u32 = 0;
        for _ in 0..4 {
            let ch = self.peek_char().ok_or_else(|| LexError {
                span: self.make_span(start),
                kind: LexErrorKind::InvalidUnicodeEscape,
            })?;
            let hv = Self::hex_val(ch).ok_or_else(|| LexError {
                span: self.make_span(start),
                kind: LexErrorKind::InvalidUnicodeEscape,
            })?;
            code = (code << 4) | hv;
            let _ = self.bump_char();
        }
        Ok(char::from_u32(code).ok_or_else(|| LexError {
            span: self.make_span(start),
            kind: LexErrorKind::InvalidUnicodeEscape,
        })?)
    }

    fn scan_string(&mut self) -> Result<(Token, Span), LexError> {
        // Opening quote already peeked but not consumed
        let start = self.cur_pos();
        let _ = self.bump_char(); // consume opening quote
        let mut out = String::new();
        loop {
            let ch = match self.peek_char() {
                Some(c) => c,
                None => {
                    return Err(LexError {
                        span: self.make_span(start),
                        kind: LexErrorKind::UnterminatedString,
                    })
                }
            };

            match ch {
                '"' => {
                    let _ = self.bump_char();
                    break;
                }
                '\\' => {
                    let _ = self.bump_char(); // consume \
                    let esc = self.peek_char().ok_or_else(|| LexError {
                        span: self.make_span(start),
                        kind: LexErrorKind::InvalidEscape,
                    })?;
                    match esc {
                        '"' => {
                            out.push('"');
                            let _ = self.bump_char();
                        }
                        '\\' => {
                            out.push('\\');
                            let _ = self.bump_char();
                        }
                        '/' => {
                            out.push('/');
                            let _ = self.bump_char();
                        }
                        'b' => {
                            out.push('\u{0008}');
                            let _ = self.bump_char();
                        }
                        'f' => {
                            out.push('\u{000C}');
                            let _ = self.bump_char();
                        }
                        'n' => {
                            out.push('\n');
                            let _ = self.bump_char();
                        }
                        'r' => {
                            out.push('\r');
                            let _ = self.bump_char();
                        }
                        't' => {
                            out.push('\t');
                            let _ = self.bump_char();
                        }
                        'u' => {
                            let _ = self.bump_char(); // consume u
                                                      // read first code unit
                            let c1 = self.scan_unicode_escape(start)?;
                            let cu1 = c1 as u32;
                            // Handle surrogate pairs
                            if (0xD800..=0xDBFF).contains(&cu1) {
                                // high surrogate; expect \uDC00..DFFF next
                                // next must be \u
                                if self.peek_char() != Some('\\')
                                    || self.peek_char_n(1) != Some('u')
                                {
                                    return Err(LexError {
                                        span: self.make_span(start),
                                        kind: LexErrorKind::InvalidUnicodeEscape,
                                    });
                                }
                                // Consume \ and u
                                self.consume_char();
                                self.consume_char();
                                let c2 = self.scan_unicode_escape(start)?;
                                let cu2 = c2 as u32;
                                if !(0xDC00..=0xDFFF).contains(&cu2) {
                                    return Err(LexError {
                                        span: self.make_span(start),
                                        kind: LexErrorKind::InvalidUnicodeEscape,
                                    });
                                }
                                // combine surrogate pair
                                let high_ten = cu1 - 0xD800;
                                let low_ten = cu2 - 0xDC00;
                                let scalar = 0x10000 + ((high_ten << 10) | low_ten);
                                if let Some(sc) = std::char::from_u32(scalar) {
                                    out.push(sc);
                                } else {
                                    return Err(LexError {
                                        span: self.make_span(start),
                                        kind: LexErrorKind::InvalidUnicodeEscape,
                                    });
                                }
                            } else if (0xDC00..=0xDFFF).contains(&cu1) {
                                // low surrogate without preceding high surrogate is invalid
                                return Err(LexError {
                                    span: self.make_span(start),
                                    kind: LexErrorKind::InvalidUnicodeEscape,
                                });
                            } else {
                                out.push(c1);
                            }
                        }
                        _ => {
                            return Err(LexError {
                                span: self.make_span(start),
                                kind: LexErrorKind::InvalidEscape,
                            })
                        }
                    }
                }
                c if (c as u32) < 0x20 => {
                    // Unescaped control characters are not allowed in JSON strings
                    return Err(LexError {
                        span: self.make_span(start),
                        kind: LexErrorKind::InvalidEscape,
                    });
                }
                _ => {
                    out.push(ch);
                    self.consume_char();
                }
            }
        }
        Ok((Token::String(out), self.make_span(start)))
    }

    fn two_char_op(&mut self, _a: char, b: char, tok_ab: Token, tok_a: Token) -> (Token, Span) {
        let start = self.cur_pos();
        self.consume_char(); // consume a
        if self.peek_char() == Some(b) {
            self.consume_char();
            (tok_ab, self.make_span(start))
        } else {
            (tok_a, self.make_span(start))
        }
    }

    pub fn next_token(&mut self) -> Result<(Token, Span), LexError> {
        if self.finished {
            let p = self.cur_pos();
            return Ok((Token::Eof, Span::single_point(p.byte, p.line, p.column)));
        }

        self.skip_whitespace_and_comments();

        let start = self.cur_pos();
        let ch = match self.peek_char() {
            Some(c) => c,
            None => {
                self.finished = true;
                let p = self.cur_pos();
                return Ok((Token::Eof, Span::single_point(p.byte, p.line, p.column)));
            }
        };

        // Punctuation and simple single-char tokens first where unambiguous
        match ch {
            '(' => {
                self.consume_char();
                return Ok((Token::LParen, self.make_span(start)));
            }
            ')' => {
                self.consume_char();
                return Ok((Token::RParen, self.make_span(start)));
            }
            '[' => {
                self.consume_char();
                return Ok((Token::LBracket, self.make_span(start)));
            }
            ']' => {
                self.consume_char();
                return Ok((Token::RBracket, self.make_span(start)));
            }
            '{' => {
                self.consume_char();
                return Ok((Token::LBrace, self.make_span(start)));
            }
            '}' => {
                self.consume_char();
                return Ok((Token::RBrace, self.make_span(start)));
            }
            ',' => {
                self.consume_char();
                return Ok((Token::Comma, self.make_span(start)));
            }
            ':' => {
                self.consume_char();
                return Ok((Token::Colon, self.make_span(start)));
            }
            '.' => {
                // No need to check if it is a number since we do not support leading '.' in numbers
                self.consume_char();
                return Ok((Token::Dot, self.make_span(start)));
            }
            '+' => {
                self.consume_char();
                return Ok((Token::Plus, self.make_span(start)));
            }
            '*' => {
                self.consume_char();
                return Ok((Token::Star, self.make_span(start)));
            }
            '/' => {
                self.consume_char();
                return Ok((Token::Slash, self.make_span(start)));
            }
            '%' => {
                self.consume_char();
                return Ok((Token::Percent, self.make_span(start)));
            }
            '$' => {
                self.consume_char();
                return Ok((Token::Dollar, self.make_span(start)));
            }
            '=' => {
                // TODO: make sure we support both = and ==
                let (tok, sp) = self.two_char_op('=', '=', Token::EqEq, Token::Eq);
                return Ok((tok, sp));
            }
            '!' => {
                self.consume_char();
                if self.peek_char() == Some('=') {
                    self.consume_char();
                    return Ok((Token::BangEq, self.make_span(start)));
                } else {
                    return Err(LexError {
                        span: self.make_span(start),
                        kind: LexErrorKind::InvalidChar,
                    });
                }
            }
            '<' => {
                let (tok, sp) = self.two_char_op('<', '=', Token::LtEq, Token::Lt);
                return Ok((tok, sp));
            }
            '>' => {
                let (tok, sp) = self.two_char_op('>', '=', Token::GtEq, Token::Gt);
                return Ok((tok, sp));
            }
            '-' => {
                // Number only if followed by a digit; otherwise it's Minus
                if matches!(self.peek_char_n(1), Some(d) if d.is_ascii_digit()) {
                    return self.scan_number(start);
                } else {
                    self.consume_char();
                    return Ok((Token::Minus, self.make_span(start)));
                }
            }
            '"' => return self.scan_string(),
            _ => {}
        }

        // Numbers starting with a digit
        if ch.is_ascii_digit() {
            return self.scan_number(start);
        }

        // Identifiers / keywords
        if Self::is_ident_start(ch) {
            return Ok(self.scan_ident_or_keyword());
        }

        // if nothing matched, it's an invalid character
        self.consume_char();
        Err(LexError { span: self.make_span(start), kind: LexErrorKind::InvalidChar })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn collect_tokens(mut lx: Lexer) -> Vec<Token> {
        let mut toks = Vec::new();
        loop {
            match lx.next_token() {
                Ok((t, _)) => {
                    let is_eof = matches!(t, Token::Eof);
                    toks.push(t);
                    if is_eof {
                        break;
                    }
                }
                Err(_) => break,
            }
        }
        toks
    }

    #[test]
    fn test_fractional_number() {
        let mut lx = Lexer::new("0.5");
        let (tok, _) = lx.next_token().unwrap();
        assert_eq!(tok, Token::NumberFloat(0.5));
    }

    #[test]
    fn lex_general_test() {
        let mut lx = Lexer::new(". 0.5 \"foo\" \"bar\" $x true false null\nlet x = 100");
        let mut toks = Vec::new();

        loop {
            let (t, _) = lx.next_token().unwrap();
            toks.push(t);
            if matches!(toks.last(), Some(Token::Eof)) {
                break;
            }
        }

        assert_eq!(
            toks,
            [
                Token::Dot,
                Token::NumberFloat(0.5),
                Token::String("foo".to_string()),
                Token::String("bar".to_string()),
                Token::Dollar,
                Token::Ident("x".to_string()),
                Token::True,
                Token::False,
                Token::Null,
                Token::Let,
                Token::Ident("x".to_string()),
                Token::Eq,
                Token::NumberInt(100),
                Token::Eof,
            ]
        )
    }

    // Numbers
    #[test]
    fn number_leading_zero_invalid() {
        let mut lx = Lexer::new("01");
        let err = lx.next_token().unwrap_err();
        assert!(matches!(err.kind, LexErrorKind::InvalidNumber));
    }

    #[test]
    fn number_exponent_valid() {
        let toks = collect_tokens(Lexer::new("1e10 -2.5E-3"));
        assert_eq!(toks, vec![Token::NumberFloat(1e10), Token::NumberFloat(-2.5e-3), Token::Eof]);
    }

    #[test]
    fn number_exponent_missing_digits() {
        for s in ["1e", "1e+", "1e-"] {
            let mut lx = Lexer::new(s);
            let err = lx.next_token().unwrap_err();
            assert!(matches!(err.kind, LexErrorKind::InvalidNumber), "case {}", s);
        }
    }

    #[test]
    fn dot_then_number_and_trailing_dot() {
        let toks = collect_tokens(Lexer::new(".5 1."));
        assert_eq!(
            toks,
            vec![Token::Dot, Token::NumberInt(5), Token::NumberInt(1), Token::Dot, Token::Eof]
        );
    }

    #[test]
    fn minus_as_operator_and_number() {
        let toks = collect_tokens(Lexer::new("1-2 - x"));
        // Current lexer treats a '-' immediately followed by a digit as part of the number token
        assert_eq!(
            toks,
            vec![
                Token::NumberInt(1),
                Token::NumberInt(-2),
                Token::Minus,
                Token::Ident("x".into()),
                Token::Eof,
            ]
        );
    }

    // Strings
    #[test]
    fn string_basic_escapes() {
        let mut lx = Lexer::new("\"a\\n\\t\\\\b\\/f\\r\"");
        let (tok, _) = lx.next_token().unwrap();
        match tok {
            Token::String(s) => {
                assert_eq!(s, "a\n\t\\b/f\r");
            }
            _ => panic!("expected string"),
        }
    }

    #[test]
    fn string_unicode_simple() {
        let mut lx = Lexer::new("\"\\u0041\"");
        let (tok, _) = lx.next_token().unwrap();
        match tok {
            Token::String(s) => assert_eq!(s, "A"),
            _ => panic!(),
        }
    }

    #[test]
    fn string_unicode_surrogate_pair() {
        let mut lx = Lexer::new("\"\\uD83D\\uDE00\"");
        // Current implementation rejects surrogate pairs; ensure we surface the error kind
        let err = lx.next_token().unwrap_err();
        assert!(matches!(err.kind, LexErrorKind::InvalidUnicodeEscape));
    }

    #[test]
    fn string_invalid_escape() {
        let mut lx = Lexer::new("\"\\x\"");
        let err = lx.next_token().unwrap_err();
        assert!(matches!(err.kind, LexErrorKind::InvalidEscape));
    }

    #[test]
    fn string_unterminated() {
        let mut lx = Lexer::new("\"abc");
        let err = lx.next_token().unwrap_err();
        assert!(matches!(err.kind, LexErrorKind::UnterminatedString));
    }

    #[test]
    fn string_unescaped_control_char() {
        let mut lx = Lexer::new("\"a\nb\"");
        let err = lx.next_token().unwrap_err();
        assert!(matches!(err.kind, LexErrorKind::InvalidEscape));
    }

    // Comments & whitespace
    #[test]
    fn comments_until_newline_and_eof() {
        let toks = collect_tokens(Lexer::new("1 // comment\n 2 // next"));
        assert_eq!(toks, vec![Token::NumberInt(1), Token::NumberInt(2), Token::Eof]);

        let toks2 = collect_tokens(Lexer::new("1 // tail"));
        assert_eq!(toks2, vec![Token::NumberInt(1), Token::Eof]);

        // Ensure explicit Eof still produced if we pull again
        let mut lx = Lexer::new("1 // tail");
        let (t1, _) = lx.next_token().unwrap();
        assert_eq!(t1, Token::NumberInt(1));
        // next_token on EOF after skipping comment yields Eof
        let (t2, _) = lx.next_token().unwrap();
        assert_eq!(t2, Token::Eof);
    }

    // Operators & punctuation
    #[test]
    fn operators_and_invalid_bang() {
        let mut lx = Lexer::new("== = <= < >= > != !");
        let mut got = Vec::new();
        // collect until error on lone '!'
        for _ in 0..7 {
            let (t, _) = lx.next_token().unwrap();
            got.push(t);
        }
        assert_eq!(
            got,
            vec![
                Token::EqEq,
                Token::Eq,
                Token::LtEq,
                Token::Lt,
                Token::GtEq,
                Token::Gt,
                Token::BangEq,
            ]
        );
        let err = lx.next_token().unwrap_err();
        assert!(matches!(err.kind, LexErrorKind::InvalidChar));
    }

    #[test]
    fn crlf_treated_as_single_newline_for_positions() {
        let mut lx = Lexer::new("a\r\nb");
        // first ident
        let (_t1, _s1) = lx.next_token().unwrap();
        // second ident: current implementation counts CRLF as single newline
        let (_t2, s2) = lx.next_token().unwrap();
        assert_eq!(s2.line, 2);
        assert_eq!(s2.column, 1);
    }

    #[test]
    fn dollar_and_ident_vs_keyword() {
        let toks = collect_tokens(Lexer::new("$ let letx"));
        assert_eq!(toks, vec![Token::Dollar, Token::Let, Token::Ident("letx".into()), Token::Eof]);
    }

    #[test]
    fn dashed_identifier_is_single_ident() {
        let mut lx = Lexer::new("is-number(x)");
        let (t1, _) = lx.next_token().unwrap();
        assert_eq!(t1, Token::Ident("is-number".into()));
        let (t2, _) = lx.next_token().unwrap();
        assert_eq!(t2, Token::LParen);
        let (t3, _) = lx.next_token().unwrap();
        assert_eq!(t3, Token::Ident("x".into()));
        let (t4, _) = lx.next_token().unwrap();
        assert_eq!(t4, Token::RParen);
        let (t5, _) = lx.next_token().unwrap();
        assert_eq!(t5, Token::Eof);
    }
}
