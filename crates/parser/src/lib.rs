use ast::{
    BinaryOp, Binding, Def, Expr, Ident, Import, Let, MemberKey, NumericKind, ObjectEntry,
    ObjectKey, Program, Span, Trivia, TriviaCollection, UnaryOp,
};
use lexer::{LexErrorKind, LexStep, Lexer, Token};
use std::mem;
use thiserror::Error;

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Error)]
pub enum ParseErrorKind {
    #[error("unexpected token: found {found:?}, expected {expected}")]
    Unexpected { found: Token, expected: &'static str },

    #[error("expected identifier")]
    ExpectedIdent,

    #[error("expected expression")]
    ExpectedExpr,

    #[error("unterminated construct: {context}")]
    Unterminated { context: &'static str },

    #[error("lexer error: {0}")]
    Lex(#[from] LexErrorKind),
}

#[derive(Debug, Error)]
#[error("{kind} at {span:?}")]
pub struct ParseError {
    pub span: Span,
    pub kind: ParseErrorKind,
}

impl ParseError {
    pub fn unexpected(span: Span, found: Token, expected: &'static str) -> ParseError {
        ParseError { span, kind: ParseErrorKind::Unexpected { found, expected } }
    }
    pub fn expected_ident(span: Span) -> ParseError {
        ParseError { span, kind: ParseErrorKind::ExpectedIdent }
    }
    pub fn expected_expr(span: Span) -> ParseError {
        ParseError { span, kind: ParseErrorKind::ExpectedExpr }
    }
    pub fn unterminated(span: Span, context: &'static str) -> ParseError {
        ParseError { span, kind: ParseErrorKind::Unterminated { context } }
    }
}

#[derive(Debug, Error)]
pub struct ParseErrors {
    pub errors: Vec<ParseError>,
}

impl std::fmt::Display for ParseErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for err in &self.errors {
            writeln!(f, "{}", err)?;
        }
        Ok(())
    }
}

#[derive(Debug, Default)]
pub struct ImportHeaderParse {
    pub imports: Vec<Import>,
    pub errors: Vec<ParseError>,
}

/// Parse only the top import block (`import "...\" as alias`) and stop at
/// the first non-import token.
///
/// This is useful for tools (like the LSP) that need import-level diagnostics
/// even when the rest of the file has syntax errors.
pub fn parse_import_header(input: &str) -> ImportHeaderParse {
    let mut parser = match Parser::new(input) {
        Ok(parser) => parser,
        Err(err) => {
            return ImportHeaderParse { imports: Vec::new(), errors: vec![err] };
        }
    };

    let mut imports = Vec::new();
    while let Token::Import = parser.cur.tok {
        match parser.parse_import_stmt() {
            Ok(import) => imports.push(import),
            Err(err) => {
                parser.errors.push(err);
                parser.synchronize();
            }
        }
    }

    ImportHeaderParse { imports, errors: std::mem::take(&mut parser.errors) }
}

#[derive(Clone)]
struct Tok {
    tok: Token,
    span: Span,
}

#[derive(Clone)]
struct PendingComment {
    text: String,
    span: Span,
}

pub struct Parser<'a> {
    lx: Lexer<'a>,
    cur: Tok,
    peeked: Option<Tok>,
    errors: Vec<ParseError>,
    prev_span: Span,
    /// Buffer for comments that haven't been attached to a node yet
    pending_comments: Vec<PendingComment>,
}

enum ListRecovery {
    Continue,
    Break,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Result<Self, ParseError> {
        let mut lx = Lexer::new(input);
        let (first, initial_comments, initial_errors) = next_token_with_comments(&mut lx);
        let prev_span = first.span;
        Ok(Parser {
            lx,
            cur: first,
            peeked: None,
            errors: initial_errors,
            prev_span,
            pending_comments: initial_comments,
        })
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseErrors> {
        // Collect file header comments
        let program_trivia = self.take_leading_trivia();

        let mut imports = Vec::new();
        let mut defs = Vec::new();
        let mut lets = Vec::new();

        // import must come first
        while let Token::Import = self.cur.tok {
            match self.parse_import_stmt() {
                Ok(import) => imports.push(import),
                Err(err) => {
                    self.errors.push(err);
                    self.synchronize();
                }
            }
        }

        // Consume any number of top-level def/let
        loop {
            match self.cur.tok {
                Token::Def => match self.parse_def() {
                    Ok(def) => defs.push(def),
                    Err(err) => {
                        self.errors.push(err);
                        self.synchronize();
                    }
                },
                Token::Let => match self.parse_let_stmt() {
                    Ok(l) => lets.push(l),
                    Err(err) => {
                        self.errors.push(err);
                        self.synchronize();
                    }
                },
                _ => break,
            }
        }

        // Optional final expression
        let maybe_expr = match self.cur.tok {
            Token::Eof => None,
            _ => match self.parse_if_or_expr() {
                Ok(expr) => Some(expr),
                Err(err) => {
                    self.errors.push(err);
                    self.synchronize();
                    None
                }
            },
        };

        // Check if we accumulated any errors
        if !self.errors.is_empty() {
            return Err(ParseErrors { errors: mem::take(&mut self.errors) });
        }

        // Expect EOF
        let (t, s) = (self.cur.tok.clone(), self.cur.span);
        if !matches!(t, Token::Eof) {
            return Err(ParseErrors {
                errors: vec![ParseError::unexpected(self.prev_span, t, "end of file")],
            });
        }

        // Span: if body exists, use its span; otherwise use single-point at EOF
        let span = if let Some(ref expr) = maybe_expr { expr.span() } else { s };

        Ok(Program { imports, defs, lets, body: maybe_expr, span, trivia: program_trivia })
    }

    /// Synchronize after an error by skipping tokens until we reach a safe recovery point.
    ///
    /// Recovery points are:
    /// - Statement boundaries (def, let, import)
    /// - Block boundaries (}, ])
    /// - End of file
    fn synchronize(&mut self) {
        // Skip tokens until we find a statement start or block end
        loop {
            match self.cur.tok {
                Token::Eof => return,
                Token::Def | Token::Let | Token::Import => return,
                Token::RBrace | Token::RBracket | Token::RParen => {
                    // Consume the closing bracket and continue
                    let _ = self.bump();
                    return;
                }
                _ => {
                    // Skip token and continue
                    if self.bump().is_err() {
                        // If we hit a lexer error during sync just stop
                        return;
                    }
                }
            }
        }
    }

    /// Collect pending comments as leading trivia
    fn take_leading_trivia(&mut self) -> Option<TriviaCollection> {
        if self.pending_comments.is_empty() {
            return None;
        }

        let leading = self
            .pending_comments
            .drain(..)
            .map(|comment| Trivia::LineComment(comment.text))
            .collect();

        Some(TriviaCollection::with_leading(leading))
    }

    /// Collect comments on the same line as an anchor offset as trailing trivia.
    fn take_inline_trailing_trivia(&mut self, anchor_offset: usize) -> Option<TriviaCollection> {
        if self.pending_comments.is_empty() {
            return None;
        }

        let anchor_line = self.line_for_offset(anchor_offset);
        let trailing_count = self
            .pending_comments
            .iter()
            .take_while(|comment| self.line_for_offset(comment.span.start) == anchor_line)
            .count();

        if trailing_count == 0 {
            return None;
        }

        let trailing = self
            .pending_comments
            .drain(..trailing_count)
            .map(|comment| Trivia::LineComment(comment.text))
            .collect();
        Some(TriviaCollection::with_trailing(trailing))
    }

    /// Convert byte offset to 0-based source line.
    fn line_for_offset(&self, offset: usize) -> usize {
        let source = self.lx.source();
        let clamped = offset.min(source.len());
        source[..clamped].bytes().filter(|b| *b == b'\n').count()
    }

    /// Count blank lines between previous token and current token
    /// A blank line is defined as two consecutive newlines (one newline terminates a line,
    /// another creates a blank line)
    fn count_blank_lines_since_last(&self) -> usize {
        let source = self.lx.source();

        // Get the end of the previous token
        let prev_end = self.prev_span.end;

        // Get the start of current token
        let cur_start = self.cur.span.start;

        if cur_start <= prev_end || cur_start >= source.len() {
            return 0;
        }

        // Extract the text between tokens
        let between = &source[prev_end..cur_start];

        // Count consecutive newlines
        let mut newline_count: usize = 0;
        for ch in between.chars() {
            if ch == '\n' {
                newline_count += 1;
            }
        }

        // Two newlines = one blank line, three newlines = two blank lines, etc.
        // One newline = no blank lines (just the normal line terminator)
        newline_count.saturating_sub(1)
    }

    fn parse_import_stmt(&mut self) -> ParseResult<Import> {
        let start = self.cur.span;
        let trivia = self.take_leading_trivia();

        self.expect(Token::Import, "'import'")?;
        // expect string path
        let (path, path_span) = match &self.cur.tok {
            Token::String(s) => {
                let sp = self.cur.span;
                let p = s.clone();
                self.bump()?;
                (p, sp)
            }
            _ => {
                return Err(ParseError::unexpected(
                    self.prev_span,
                    self.cur.tok.clone(),
                    "string literal",
                ))
            }
        };

        // expect "as"
        self.expect(Token::As, "'as'")?;

        // create alias ident. It can be multiple ident seprated by colon token. So we need to parse
        // until no more colon tokens
        let mut alias_ident = self.expect_ident()?;
        while self.eat(&Token::Colon) {
            alias_ident.name.push(':');
            let ident = self.expect_ident()?;
            alias_ident.name.push_str(&ident.name);
            alias_ident.span = Span::join(alias_ident.span, ident.span);
        }

        let end = alias_ident.span;

        Ok(Import {
            path,
            alias: alias_ident.name,
            span: Span::join(start, Span::join(path_span, end)),
            trivia,
        })
    }

    fn parse_def(&mut self) -> ParseResult<Def> {
        let start = self.cur.span;
        let trivia = self.take_leading_trivia();
        self.expect(Token::Def, "'def'")?;
        let name = self.expect_ident()?;
        self.expect(Token::LParen, "'(' after function name")?;
        let mut params = Vec::new();
        if !self.at(&Token::RParen) {
            let p = self.expect_ident()?;
            params.push(p);
            while let ListRecovery::Continue = self.recover_list_separator(
                "',' or ')' after parameter",
                "function parameter list",
                |tok| matches!(tok, Token::Ident(_)),
                |tok| matches!(tok, Token::RParen),
            )? {
                let p = self.expect_ident()?;
                params.push(p);
            }
        }
        self.expect(Token::RParen, "')' after parameters")?;
        // Optional let block inside def: zero or more let statements before body expr
        let mut lets = Vec::new();
        while self.at(&Token::Let) {
            let l = self.parse_let_stmt()?;
            lets.push(l);
        }
        let body_trivia = self.take_leading_trivia();
        let body = self.parse_if_or_expr()?;
        let body_trailing_trivia = self.take_inline_trailing_trivia(body.span().end);
        let end = body.span();
        Ok(Def {
            name,
            params,
            lets,
            body,
            span: Span::join(start, end),
            trivia,
            body_trivia,
            body_trailing_trivia,
        })
    }

    fn parse_let_stmt(&mut self) -> ParseResult<Let> {
        let start = self.cur.span;
        let trivia = self.take_leading_trivia();
        self.expect(Token::Let, "'let'")?;
        let mut bindings = Vec::new();

        let name = self.expect_ident()?;
        let name_span = name.span;
        self.expect(Token::Eq, "'=' after let binding")?;
        let expr = self.parse_if_or_expr()?;
        let expr_span = expr.span();
        bindings.push(Binding { name, value: expr, span: Span::join(name_span, expr_span) });

        let span = Span::join(start, bindings.last().unwrap().span);
        Ok(Let { bindings, span, trivia })
    }

    fn parse_if_or_expr(&mut self) -> ParseResult<Expr> {
        if self.at(&Token::If) {
            let start = self.cur.span;
            self.bump()?; // 'if'
            self.expect_with_recovery(
                Token::LParen,
                "'(' after if",
                "if expression",
                Self::is_expr_start,
            )?;
            let cond = self.parse_if_or_expr()?;
            self.expect_with_recovery(
                Token::RParen,
                "')' after if condition",
                "if condition",
                |tok| matches!(tok, Token::Let | Token::If) || Self::is_expr_start(tok),
            )?;
            let then_expr = self.parse_lets_then_expr()?;
            let else_expr = if self.at(&Token::Else) {
                self.bump()?; // 'else'
                Some(Box::new(self.parse_lets_then_expr()?))
            } else {
                None
            };
            let end_span = else_expr.as_ref().map(|e| e.span()).unwrap_or_else(|| then_expr.span());
            let span = Span::join(start, end_span);
            Ok(Expr::If {
                cond: Box::new(cond),
                then_br: Box::new(then_expr),
                else_br: else_expr,
                span,
            })
        } else {
            self.parse_or_expr()
        }
    }

    // Parse zero or more leading let-statements followed by an expression. If no lets,
    // returns the expression directly; otherwise returns a LetBlock.
    fn parse_lets_then_expr(&mut self) -> ParseResult<Expr> {
        let mut lets = Vec::new();
        while self.at(&Token::Let) {
            let l = self.parse_let_stmt()?;
            lets.push(l);
        }
        let body = self.parse_if_or_expr()?;
        if lets.is_empty() {
            Ok(body)
        } else {
            let span = Span::join(lets.first().unwrap().span, body.span());
            Ok(Expr::LetBlock { lets, body: Box::new(body), span })
        }
    }

    fn parse_or_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_and_expr()?;
        while self.at(&Token::Or) {
            let _op_span = self.cur.span;
            self.bump()?;
            let right = match self.parse_and_expr() {
                Ok(expr) => expr,
                Err(err) => {
                    self.errors.push(err);
                    self.synchronize_expression();
                    break;
                }
            };
            let span = Span::join(left.span(), right.span());
            left = Expr::Binary {
                op: BinaryOp::Or,
                left: Box::new(left),
                right: Box::new(right),
                span,
            };
        }
        Ok(left)
    }

    fn parse_and_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_cmp_expr()?;
        while self.at(&Token::And) {
            let _op_span = self.cur.span;
            self.bump()?;
            let right = match self.parse_cmp_expr() {
                Ok(expr) => expr,
                Err(err) => {
                    self.errors.push(err);
                    self.synchronize_expression();
                    break;
                }
            };
            let span = Span::join(left.span(), right.span());
            left = Expr::Binary {
                op: BinaryOp::And,
                left: Box::new(left),
                right: Box::new(right),
                span,
            };
        }
        Ok(left)
    }

    fn parse_cmp_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_add_expr()?;
        loop {
            let op = match self.cur.tok {
                Token::Lt => Some(BinaryOp::Lt),
                Token::LtEq => Some(BinaryOp::Le),
                Token::Gt => Some(BinaryOp::Gt),
                Token::GtEq => Some(BinaryOp::Ge),
                Token::EqEq => Some(BinaryOp::Eq),
                Token::BangEq => Some(BinaryOp::Ne),
                _ => None,
            };
            if let Some(op) = op {
                self.bump()?;
                let right = match self.parse_add_expr() {
                    Ok(expr) => expr,
                    Err(err) => {
                        self.errors.push(err);
                        self.synchronize_expression();
                        break;
                    }
                };
                let span = Span::join(left.span(), right.span());
                left = Expr::Binary { op, left: Box::new(left), right: Box::new(right), span };
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn parse_add_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_mul_expr()?;
        loop {
            let op = match self.cur.tok {
                Token::Plus => Some(BinaryOp::Add),
                Token::Minus => Some(BinaryOp::Sub),
                _ => None,
            };
            if let Some(op) = op {
                self.bump()?;
                let right = match self.parse_mul_expr() {
                    Ok(expr) => expr,
                    Err(err) => {
                        self.errors.push(err);
                        self.synchronize_expression();
                        break;
                    }
                };
                let span = Span::join(left.span(), right.span());
                left = Expr::Binary { op, left: Box::new(left), right: Box::new(right), span };
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn parse_mul_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_unary_expr()?;
        loop {
            let op = match self.cur.tok {
                Token::Star => Some(BinaryOp::Mul),
                Token::Slash => Some(BinaryOp::Div),
                Token::Percent => Some(BinaryOp::Rem),
                _ => None,
            };
            if let Some(op) = op {
                self.bump()?;
                let right = match self.parse_unary_expr() {
                    Ok(expr) => expr,
                    Err(err) => {
                        self.errors.push(err);
                        self.synchronize_expression();
                        break;
                    }
                };
                let span = Span::join(left.span(), right.span());
                left = Expr::Binary { op, left: Box::new(left), right: Box::new(right), span };
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn parse_unary_expr(&mut self) -> ParseResult<Expr> {
        match &self.cur.tok {
            Token::Minus => {
                let start = self.cur.span;
                self.bump()?;
                let expr = self.parse_unary_expr()?;
                Ok(Expr::Unary {
                    op: UnaryOp::Neg,
                    expr: Box::new(expr.clone()),
                    span: Span::join(start, expr.span()),
                })
            }
            Token::Not => {
                let start = self.cur.span;
                self.bump()?;
                let expr = self.parse_unary_expr()?;
                Ok(Expr::Unary {
                    op: UnaryOp::Not,
                    expr: Box::new(expr.clone()),
                    span: Span::join(start, expr.span()),
                })
            }
            _ => self.parse_postfix_expr(),
        }
    }

    fn parse_postfix_expr(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_primary()?;

        loop {
            match self.cur.tok {
                // Support ".a" and ".\"key\"" directly after leading '.'
                Token::Ident(ref s) => {
                    if let Expr::This(_) = expr {
                        let key_span = self.cur.span;
                        let key = MemberKey::Ident(Ident { name: s.clone(), span: key_span });
                        self.bump()?;
                        let span = Span::join(expr.span(), self.cur.span);
                        expr = Expr::Member { target: Box::new(expr), key, span };
                        continue;
                    } else {
                        break;
                    }
                }
                Token::String(ref s) => {
                    if let Expr::This(_) = expr {
                        let key_span = self.cur.span;
                        let key = MemberKey::Str { value: s.clone(), span: key_span };
                        self.bump()?;
                        let span = Span::join(expr.span(), self.cur.span);
                        expr = Expr::Member { target: Box::new(expr), key, span };
                        continue;
                    } else {
                        break;
                    }
                }
                Token::Dot => {
                    // member: . ident | . "string"
                    self.bump()?; // ".'
                    match &self.cur.tok {
                        Token::Ident(ref s) => {
                            let key =
                                MemberKey::Ident(Ident { name: s.clone(), span: self.cur.span });
                            self.bump()?;
                            let span = Span::join(expr.span(), self.cur.span);
                            expr = Expr::Member { target: Box::new(expr), key, span };
                        }
                        Token::String(ref s) => {
                            let key = MemberKey::Str { value: s.clone(), span: self.cur.span };
                            self.bump()?;
                            let span = Span::join(expr.span(), self.cur.span);
                            expr = Expr::Member { target: Box::new(expr), key, span };
                        }
                        _ => {
                            self.errors.push(ParseError::unexpected(
                                self.prev_span,
                                self.cur.tok.clone(),
                                "identifier or string after '.'",
                            ));
                            break;
                        }
                    }
                }
                Token::LBracket => {
                    // If the current expr is a complete literal (array/object), or following Group expr,
                    // do NOT treat the following '[' as an index/slice; it's the next expression.
                    match expr {
                        Expr::ArrayLiteral { .. }
                        | Expr::ObjectLiteral { .. }
                        | Expr::Group { .. } => {
                            break;
                        }
                        _ => {}
                    }
                    // index_or_slice: '[' [expr] [':' [expr] ']'
                    let start = self.cur.span;
                    self.bump()?; // '['
                                  // optional first expr
                    let mut first: Option<Expr> = None;
                    if !self.at(&Token::RBracket) && !self.at(&Token::Colon) {
                        match self.parse_if_or_expr() {
                            Ok(expr) => first = Some(expr),
                            Err(err) => {
                                self.errors.push(err);
                                self.synchronize_expression();
                            }
                        }
                    }
                    if self.at(&Token::Colon) {
                        // slice: [':' [expr] ']'
                        self.bump()?; // ':'
                        let mut second: Option<Expr> = None;
                        if !self.at(&Token::RBracket) {
                            match self.parse_if_or_expr() {
                                Ok(expr) => second = Some(expr),
                                Err(err) => {
                                    self.errors.push(err);
                                    self.synchronize_expression();
                                }
                            }
                        }
                        let end_span = self.expect_with_recovery(
                            Token::RBracket,
                            "']' for slice",
                            "slice expression",
                            Self::is_expr_boundary,
                        )?;
                        let span = Span::join(start, end_span);
                        expr = Expr::Slice {
                            target: Box::new(expr),
                            start: first.map(Box::new),
                            end: second.map(Box::new),
                            span,
                        };
                    } else {
                        // index: must have first
                        let first_expr = match first {
                            Some(e) => e,
                            None => {
                                self.errors.push(ParseError::expected_expr(self.cur.span));
                                let _ = self.expect_with_recovery(
                                    Token::RBracket,
                                    "']' for index",
                                    "index expression",
                                    Self::is_expr_boundary,
                                )?;
                                continue;
                            }
                        };
                        let end_span = self.expect_with_recovery(
                            Token::RBracket,
                            "']' for index",
                            "index expression",
                            Self::is_expr_boundary,
                        )?;
                        let span = Span::join(start, end_span);
                        expr = Expr::Index {
                            target: Box::new(expr),
                            index: Box::new(first_expr),
                            span,
                        };
                    }
                }
                Token::LParen => {
                    // call: '(' [args] ')'
                    let start = self.cur.span;
                    self.bump()?; // '('
                    let mut args = Vec::new();
                    if !self.at(&Token::RParen) {
                        match self.parse_if_or_expr() {
                            Ok(arg) => args.push(arg),
                            Err(err) => {
                                self.errors.push(err);
                                self.synchronize_expression();
                            }
                        }
                        while let ListRecovery::Continue = self.recover_list_separator(
                            "',' or ')' after argument",
                            "function call arguments",
                            Self::is_expr_start,
                            |tok| matches!(tok, Token::RParen),
                        )? {
                            match self.parse_if_or_expr() {
                                Ok(arg) => args.push(arg),
                                Err(err) => {
                                    self.errors.push(err);
                                    self.synchronize_expression();
                                }
                            }
                        }
                    }
                    let end_span = self.expect_with_recovery(
                        Token::RParen,
                        "')' to close call",
                        "function call",
                        Self::is_expr_boundary,
                    )?;
                    let span = Span::join(start, end_span);
                    expr = Expr::Call { callee: Box::new(expr), args, span };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> ParseResult<Expr> {
        match &self.cur.tok {
            Token::Null => {
                let s = self.cur.span;
                self.bump()?;
                Ok(Expr::Null(s))
            }
            Token::True => {
                let s = self.cur.span;
                self.bump()?;
                Ok(Expr::Bool { value: true, span: s })
            }
            Token::False => {
                let s = self.cur.span;
                self.bump()?;
                Ok(Expr::Bool { value: false, span: s })
            }
            Token::NumberFloat(n) => {
                let s = self.cur.span;
                let v = *n;
                self.bump()?;
                Ok(Expr::Number { lexeme: v.to_string(), kind: NumericKind::Float, span: s })
            }
            Token::NumberInt(n) => {
                let s = self.cur.span;
                let v = *n;
                self.bump()?;
                Ok(Expr::Number { lexeme: v.to_string(), kind: NumericKind::Int, span: s })
            }
            Token::String(st) => {
                let s = self.cur.span;
                let v = st.clone();
                self.bump()?;
                Ok(Expr::String { value: v, span: s })
            }
            Token::Dollar => {
                let dollar_span = self.cur.span;
                self.bump()?;
                match &self.cur.tok {
                    Token::Ident(name) => {
                        let name_span = self.cur.span;
                        let span = Span::join(dollar_span, name_span);
                        let v = name.clone();
                        self.bump()?;
                        Ok(Expr::Variable { name: Ident { name: v, span } })
                    }
                    _ => Err(ParseError::expected_ident(dollar_span)),
                }
            }
            Token::Dot => {
                // this
                let s = self.cur.span;
                self.bump()?;
                Ok(Expr::This(s))
            }
            Token::LParen => {
                self.bump()?;
                let inner = match self.parse_if_or_expr() {
                    Ok(expr) => expr,
                    Err(err) => {
                        self.errors.push(err);
                        self.synchronize_expression();
                        Expr::Null(self.prev_span)
                    }
                };
                let end_span = self.expect_with_recovery(
                    Token::RParen,
                    "')' to close group",
                    "group expression",
                    Self::is_expr_boundary,
                )?;
                let span = Span::join(inner.span(), end_span);
                Ok(Expr::Group { expr: Box::new(inner), span })
            }
            Token::LBracket => self.parse_array_like(),
            Token::LBrace => self.parse_object_like(),
            _ => {
                // bare identifiers is a function refernece or call target in some ASTs;
                // per spec, identifiers alone are not variables (only $ident)
                // We still allow bare ident as "function refernce" primary so call can follow.
                if let Token::Ident(name) = &self.cur.tok {
                    let s = self.cur.span;
                    let mut v = name.clone();
                    self.bump()?;

                    // if is namespace call we need to check for colon too
                    if self.eat(&Token::Colon) {
                        v.push(':');
                        let ident = self.expect_ident()?;
                        v.push_str(&ident.name);
                    }

                    Ok(Expr::FunctionRef { name: v, span: s })
                } else {
                    Err(ParseError::expected_expr(self.prev_span))
                }
            }
        }
    }

    fn parse_array_like(&mut self) -> ParseResult<Expr> {
        let start = self.cur.span;
        self.bump()?; // '['

        // comprehension or literal?
        if self.at(&Token::For) {
            self.bump()?; // 'for'
            self.expect(Token::LParen, "'(' after for")?;
            let seq = self.parse_if_or_expr()?;
            self.expect(Token::RParen, "')' after sequence")?;
            let body = self.parse_lets_then_expr()?;
            let filter = if self.at(&Token::If) {
                self.bump()?; // 'if'
                Some(Box::new(self.parse_if_or_expr()?))
            } else {
                None
            };
            let end_span = self.expect(Token::RBracket, "']' to close array comp")?;
            let span = Span::join(start, end_span);
            Ok(Expr::ArrayFor { seq: Box::new(seq), body: Box::new(body), filter, span })
        } else {
            let mut elems = Vec::new();
            if !self.at(&Token::RBracket) {
                match self.parse_if_or_expr() {
                    Ok(elem) => elems.push(elem),
                    Err(err) => {
                        self.errors.push(err);
                        self.synchronize_expression();
                    }
                }
                while let ListRecovery::Continue = self.recover_list_separator(
                    "',' or ']' after array element",
                    "array literal",
                    Self::is_expr_start,
                    |tok| matches!(tok, Token::RBracket),
                )? {
                    match self.parse_if_or_expr() {
                        Ok(elem) => elems.push(elem),
                        Err(err) => {
                            self.errors.push(err);
                            self.synchronize_expression();
                        }
                    }
                }
            }
            let end_span = self.expect(Token::RBracket, "']' to close array")?;
            let span = Span::join(start, end_span);
            Ok(Expr::ArrayLiteral { elements: elems, span })
        }
    }

    fn parse_object_like(&mut self) -> ParseResult<Expr> {
        let start = self.cur.span;
        let pending_before_lbrace = self.pending_comments.len();
        self.bump()?; // '{'

        if self.at(&Token::For) {
            self.bump()?; // 'for'
            self.expect(Token::LParen, "'(' after 'for'")?;
            let seq = self.parse_if_or_expr()?;
            self.expect(Token::RParen, "')' after sequence")?;
            // zero or more lets usable in both key and value
            let mut shared_lets = Vec::new();
            while self.at(&Token::Let) {
                let l = self.parse_let_stmt()?;
                shared_lets.push(l);
            }
            let key_inner = self.parse_if_or_expr()?;
            let key = if shared_lets.is_empty() {
                key_inner
            } else {
                let span = Span::join(shared_lets.first().unwrap().span, key_inner.span());
                Expr::LetBlock { lets: shared_lets.clone(), body: Box::new(key_inner), span }
            };
            self.expect(Token::Colon, ":' after object comp key")?;
            let value_inner = self.parse_if_or_expr()?;
            let value = if shared_lets.is_empty() {
                value_inner
            } else {
                let span = Span::join(shared_lets.first().unwrap().span, value_inner.span());
                Expr::LetBlock { lets: shared_lets, body: Box::new(value_inner), span }
            };
            let filter = if self.at(&Token::If) {
                self.bump()?; // 'if'
                Some(Box::new(self.parse_if_or_expr()?))
            } else {
                None
            };
            let end_span = self.expect(Token::RBrace, "'}' to close object comp")?;
            let span = Span::join(start, end_span);
            Ok(Expr::ObjectFor {
                seq: Box::new(seq),
                key: Box::new(key),
                value: Box::new(value),
                filter,
                span,
            })
        } else {
            // Optional let-block inside object literal before entries
            let mut leading_lets = Vec::new();
            while self.at(&Token::Let) {
                let l = self.parse_let_stmt()?;
                leading_lets.push(l);
            }

            // Preserve comments that appeared before the '{' itself as object-level trivia.
            let trivia = if pending_before_lbrace > 0 {
                let leading = self
                    .pending_comments
                    .drain(..pending_before_lbrace)
                    .map(|comment| Trivia::LineComment(comment.text))
                    .collect();
                Some(TriviaCollection::with_leading(leading))
            } else {
                None
            };

            let mut entries = Vec::new();
            if !self.at(&Token::RBrace) {
                loop {
                    // Count blank lines before this entry
                    let blank_lines_before = self.count_blank_lines_since_last();

                    // Collect comments before this entry
                    let entry_trivia = self.take_leading_trivia();

                    // entry = key ':' expr | '*' ('-' key)* ':' expr
                    let entry = match &self.cur.tok {
                        Token::Star => {
                            let star_span = self.cur.span;
                            self.bump()?;
                            let mut exclude_keys = Vec::new();
                            while self.eat(&Token::Minus) {
                                let excluded_key = match &self.cur.tok {
                                    Token::Ident(id) => {
                                        let name = id.clone();
                                        self.bump()?;
                                        name
                                    }
                                    Token::String(s) => {
                                        let name = s.clone();
                                        self.bump()?;
                                        name
                                    }
                                    _ => {
                                        return Err(ParseError::unexpected(
                                            self.prev_span,
                                            self.cur.tok.clone(),
                                            "identifier or string after '-' in object wildcard",
                                        ))
                                    }
                                };
                                exclude_keys.push(excluded_key);
                                while self.eat(&Token::Comma) {
                                    let excluded_key = match &self.cur.tok {
                                        Token::Ident(id) => {
                                            let name = id.clone();
                                            self.bump()?;
                                            name
                                        }
                                        Token::String(s) => {
                                            let name = s.clone();
                                            self.bump()?;
                                            name
                                        }
                                        _ => {
                                            return Err(ParseError::unexpected(
                                                self.prev_span,
                                                self.cur.tok.clone(),
                                                "identifier or string after ',' in object wildcard exclusion",
                                            ))
                                        }
                                    };
                                    exclude_keys.push(excluded_key);
                                }
                            }
                            self.expect(Token::Colon, "':' after '*'")?;
                            let v = match self.parse_if_or_expr() {
                                Ok(expr) => expr,
                                Err(err) => {
                                    self.errors.push(err);
                                    self.synchronize_expression();
                                    Expr::Null(self.prev_span)
                                }
                            };
                            let span = Span::join(star_span, v.span());
                            ObjectEntry::Spread {
                                value: v,
                                exclude_keys,
                                span,
                                trivia: entry_trivia,
                                blank_lines_before,
                            }
                        }
                        Token::String(s) => {
                            let kspan = self.cur.span;
                            let key = ObjectKey::Str { value: s.clone(), span: self.cur.span };
                            self.bump()?;
                            self.expect(Token::Colon, "':' after object key")?;
                            let v = match self.parse_if_or_expr() {
                                Ok(expr) => expr,
                                Err(err) => {
                                    self.errors.push(err);
                                    self.synchronize_expression();
                                    Expr::Null(self.prev_span)
                                }
                            };
                            let span = Span::join(kspan, v.span());
                            ObjectEntry::Pair {
                                key,
                                value: v,
                                span,
                                trivia: entry_trivia,
                                blank_lines_before,
                            }
                        }
                        Token::Ident(id) => {
                            let kspan = self.cur.span;
                            let key =
                                ObjectKey::Ident(Ident { name: id.clone(), span: self.cur.span });
                            self.bump()?;
                            self.expect(Token::Colon, "':' after object key")?;
                            let v = match self.parse_if_or_expr() {
                                Ok(expr) => expr,
                                Err(err) => {
                                    self.errors.push(err);
                                    self.synchronize_expression();
                                    Expr::Null(self.prev_span)
                                }
                            };
                            let span = Span::join(kspan, v.span());
                            ObjectEntry::Pair {
                                key,
                                value: v,
                                span,
                                trivia: entry_trivia,
                                blank_lines_before,
                            }
                        }
                        _ => {
                            return Err(ParseError::unexpected(
                                self.prev_span,
                                self.cur.tok.clone(),
                                "object key (identifier or string) or '*'",
                            ))
                        }
                    };
                    entries.push(entry);
                    // JSLT allows a trailing comma in object literals.
                    if self.eat(&Token::Comma) {
                        if self.at(&Token::RBrace) {
                            break;
                        }
                        continue;
                    }
                    match self.recover_list_separator(
                        "',' or '}' after object entry",
                        "object literal",
                        |tok| matches!(tok, Token::String(_) | Token::Ident(_) | Token::Star),
                        |tok| matches!(tok, Token::RBrace),
                    )? {
                        ListRecovery::Continue => continue,
                        ListRecovery::Break => break,
                    }
                }
            }
            // Preserve comments that appear after the last entry and before '}'.
            let trailing_trivia = self.take_leading_trivia();
            let end_span = self.expect(Token::RBrace, "'}' to close object")?;
            let span = Span::join(start, end_span);
            let obj = Expr::ObjectLiteral { entries, span, trivia, trailing_trivia };
            if leading_lets.is_empty() {
                Ok(obj)
            } else {
                let lspan = leading_lets.first().unwrap().span;
                let span2 = Span::join(lspan, obj.span());
                Ok(Expr::LetBlock { lets: leading_lets, body: Box::new(obj), span: span2 })
            }
        }
    }

    // token utilities
    fn at(&self, t: &Token) -> bool {
        mem::discriminant(&self.cur.tok) == mem::discriminant(t)
    }

    fn eat(&mut self, t: &Token) -> bool {
        if self.at(t) {
            // If bump fails (lexer error), we return false and let the error be caught
            // in the next token operation.
            self.bump().is_ok()
        } else {
            false
        }
    }

    fn bump(&mut self) -> ParseResult<Tok> {
        let old = if let Some(pk) = self.peeked.take() {
            mem::replace(&mut self.cur, pk)
        } else {
            let (nt, comments, lex_errors) = next_token_with_comments(&mut self.lx);
            self.pending_comments.extend(comments);
            self.errors.extend(lex_errors);
            mem::replace(&mut self.cur, nt)
        };
        self.prev_span = old.span;
        Ok(old)
    }

    fn expect(&mut self, t: Token, expected: &'static str) -> ParseResult<Span> {
        if mem::discriminant(&self.cur.tok) == mem::discriminant(&t) {
            let s = self.cur.span;
            self.bump()?;
            Ok(s)
        } else {
            Err(ParseError::unexpected(self.prev_span, self.cur.tok.clone(), expected))
        }
    }

    fn expect_ident(&mut self) -> ParseResult<Ident> {
        match &self.cur.tok {
            Token::Ident(s) => {
                let ident = Ident { name: s.clone(), span: self.cur.span };
                self.bump()?;
                Ok(ident)
            }
            _ => Err(ParseError::expected_ident(self.prev_span)),
        }
    }

    fn is_expr_start(tok: &Token) -> bool {
        matches!(
            tok,
            Token::If
                | Token::Null
                | Token::True
                | Token::False
                | Token::NumberFloat(_)
                | Token::NumberInt(_)
                | Token::String(_)
                | Token::Dollar
                | Token::Dot
                | Token::LParen
                | Token::LBracket
                | Token::LBrace
                | Token::Ident(_)
                | Token::Not
                | Token::Minus
        )
    }

    fn recover_list_separator<F, G>(
        &mut self,
        expected: &'static str,
        context: &'static str,
        is_item_start: F,
        is_end: G,
    ) -> ParseResult<ListRecovery>
    where
        F: Fn(&Token) -> bool,
        G: Fn(&Token) -> bool,
    {
        if self.eat(&Token::Comma) {
            return Ok(ListRecovery::Continue);
        }
        if is_end(&self.cur.tok) {
            return Ok(ListRecovery::Break);
        }

        self.errors.push(ParseError::unexpected(self.prev_span, self.cur.tok.clone(), expected));
        if is_item_start(&self.cur.tok) {
            return Ok(ListRecovery::Continue);
        }

        while !matches!(self.cur.tok, Token::Comma | Token::Eof)
            && !is_end(&self.cur.tok)
            && !is_item_start(&self.cur.tok)
        {
            self.bump()?;
        }

        if self.eat(&Token::Comma) {
            return Ok(ListRecovery::Continue);
        }
        if is_item_start(&self.cur.tok) {
            return Ok(ListRecovery::Continue);
        }
        if is_end(&self.cur.tok) {
            return Ok(ListRecovery::Break);
        }
        if matches!(self.cur.tok, Token::Eof) {
            return Err(ParseError::unterminated(self.prev_span, context));
        }
        Ok(ListRecovery::Break)
    }

    fn is_expr_boundary(tok: &Token) -> bool {
        matches!(
            tok,
            Token::Or
                | Token::And
                | Token::Lt
                | Token::LtEq
                | Token::Gt
                | Token::GtEq
                | Token::EqEq
                | Token::BangEq
                | Token::Plus
                | Token::Minus
                | Token::Star
                | Token::Slash
                | Token::Percent
                | Token::Comma
                | Token::Colon
                | Token::RParen
                | Token::RBracket
                | Token::RBrace
                | Token::Else
                | Token::Eof
        )
    }

    fn is_hard_expr_boundary(tok: &Token) -> bool {
        matches!(
            tok,
            Token::Comma
                | Token::Colon
                | Token::RParen
                | Token::RBracket
                | Token::RBrace
                | Token::Else
                | Token::Def
                | Token::Let
                | Token::Import
                | Token::Eof
        )
    }

    fn synchronize_expression(&mut self) {
        let mut advanced = false;
        while !Self::is_hard_expr_boundary(&self.cur.tok) {
            if self.bump().is_err() {
                break;
            }
            advanced = true;
        }
        if !advanced && !Self::is_hard_expr_boundary(&self.cur.tok) {
            let _ = self.bump();
            while !Self::is_hard_expr_boundary(&self.cur.tok) {
                if self.bump().is_err() {
                    break;
                }
            }
        }
    }

    fn expect_with_recovery<F>(
        &mut self,
        t: Token,
        expected: &'static str,
        context: &'static str,
        is_recovery_boundary: F,
    ) -> ParseResult<Span>
    where
        F: Fn(&Token) -> bool,
    {
        if self.at(&t) {
            let s = self.cur.span;
            self.bump()?;
            return Ok(s);
        }

        self.errors.push(ParseError::unexpected(self.prev_span, self.cur.tok.clone(), expected));
        if is_recovery_boundary(&self.cur.tok) {
            return Ok(self.prev_span);
        }

        while !self.at(&t)
            && !matches!(self.cur.tok, Token::Eof)
            && !is_recovery_boundary(&self.cur.tok)
        {
            self.bump()?;
        }

        if self.at(&t) {
            let s = self.cur.span;
            self.bump()?;
            return Ok(s);
        }

        if matches!(self.cur.tok, Token::Eof) {
            return Err(ParseError::unterminated(self.prev_span, context));
        }

        Ok(self.prev_span)
    }
}

/// Get the next non-comment token and collect any comments encountered
fn next_token_with_comments(lx: &mut Lexer<'_>) -> (Tok, Vec<PendingComment>, Vec<ParseError>) {
    let mut comments = Vec::new();
    let mut errors = Vec::new();
    loop {
        match lx.next_step() {
            LexStep::Token(Token::Comment(comment), span) => {
                comments.push(PendingComment { text: comment, span });
            }
            LexStep::Token(t, s) => return (Tok { tok: t, span: s }, comments, errors),
            LexStep::Error(le) => {
                errors.push(ParseError { span: le.span, kind: ParseErrorKind::Lex(le.kind) });
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_nested_let_statement_succeeds() {
        let input = r#"
let nested = [for ($obj)
     let outerkey = (.key)
     [for ($x) $outerkey]
       // [for (flatten-object(array(.value))) {
       //   "key" : $outerkey + "_" + .key,
       //   "value" : if (is-object(.value)) flatten-object(.value) else .value
       // }]
     if (is-object(.value))]
"#;
        let mut parser = Parser::new(input).expect("lexer should initialize");
        let res = parser.parse_program();
        if let Err(e) = &res {
            eprintln!("Parse error: {:?}", e);
        }
        assert!(res.is_ok(), "expected parsing to succeed");
    }
}
