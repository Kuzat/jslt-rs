use ast::{
    BinaryOp, Binding, Def, Expr, Ident, Import, Let, MemberKey, NumericKind, ObjectEntry,
    ObjectKey, Program, Span, UnaryOp,
};
use lexer::{LexErrorKind, Lexer, Token};
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

#[derive(Clone)]
struct Tok {
    tok: Token,
    span: Span,
}

pub struct Parser<'a> {
    lx: Lexer<'a>,
    cur: Tok,
    peeked: Option<Tok>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Result<Self, ParseError> {
        let mut lx = Lexer::new(input);
        let first = next_token(&mut lx)?;
        Ok(Parser { lx, cur: first, peeked: None })
    }

    pub fn parse_program(&mut self) -> ParseResult<Program> {
        let mut imports = Vec::new();
        let mut defs = Vec::new();
        let mut lets = Vec::new();

        println!("Parsing program");
        // import must come first
        while let Token::Import = self.cur.tok {
            imports.push(self.parse_import_stmt()?);
        }

        // Consume any number of top-level def/let
        loop {
            match self.cur.tok {
                Token::Def => defs.push(self.parse_def()?),
                Token::Let => {
                    let l = self.parse_let_stmt()?;
                    lets.push(l)
                }
                _ => break,
            }
        }

        // Optional final expression
        let maybe_expr = match self.cur.tok {
            Token::Eof => None,
            _ => Some(self.parse_if_or_expr()?),
        };

        // Expect EOF
        let (t, s) = (self.cur.tok.clone(), self.cur.span);
        if !matches!(t, Token::Eof) {
            return Err(ParseError::unexpected(s, t, "end of input"));
        }

        // Span: if body exists, use its span; otherwise use single-point at EOF
        let span = if let Some(ref expr) = maybe_expr { expr.span() } else { s };

        // Program span is the expr span (def/lets can be recorded separately in AST if needed)
        Ok(Program { imports, defs, lets, body: maybe_expr, span })
    }

    fn parse_import_stmt(&mut self) -> ParseResult<Import> {
        let start = self.cur.span;

        self.expect(Token::Import, "'import'")?;
        // expect string path
        let (path, path_span) = match &self.cur.tok {
            Token::String(s) => {
                let sp = self.cur.span;
                let p = s.clone();
                self.bump();
                (p, sp)
            }
            _ => {
                return Err(ParseError::unexpected(
                    self.cur.span,
                    self.cur.tok.clone(),
                    "string literal",
                ))
            }
        };

        // expect "as"
        match &self.cur.tok {
            Token::As => {
                self.bump();
            }
            _ => {
                return Err(ParseError::unexpected(
                    self.cur.span,
                    self.cur.tok.clone(),
                    "'as' after import path",
                ))
            }
        }

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
        })
    }

    fn parse_def(&mut self) -> ParseResult<Def> {
        let start = self.cur.span;
        self.expect(Token::Def, "'def'")?;
        let name = self.expect_ident()?;
        self.expect(Token::LParen, "'(' after function name")?;
        let mut params = Vec::new();
        if !self.at(&Token::RParen) {
            let p = self.expect_ident()?;
            params.push(p);
            while self.eat(&Token::Comma) {
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
        let body = self.parse_if_or_expr()?;
        let end = body.span();
        Ok(Def { name, params, lets, body, span: Span::join(start, end) })
    }

    fn parse_let_stmt(&mut self) -> ParseResult<Let> {
        let start = self.cur.span;
        self.expect(Token::Let, "'let'")?;
        let mut bindings = Vec::new();

        let name = self.expect_ident()?;
        let name_span = name.span;
        self.expect(Token::Eq, "'=' after let binding")?;
        let expr = self.parse_if_or_expr()?;
        let expr_span = expr.span();
        bindings.push(Binding { name, value: expr, span: Span::join(name_span, expr_span) });

        let span = Span::join(start, bindings.last().unwrap().span);
        Ok(Let { bindings, span })
    }

    fn parse_if_or_expr(&mut self) -> ParseResult<Expr> {
        if self.at(&Token::If) {
            println!("Parsing if_or_expr with token {:?}", self.cur.tok);
            let start = self.cur.span;
            self.bump(); // 'if'
            self.expect(Token::LParen, "'(' after if")?;
            let cond = self.parse_if_or_expr()?;
            self.expect(Token::RParen, "')' after if condition")?;
            println!("parse if_or_expr then branch at token {:?}", self.cur.tok);
            let then_expr = self.parse_lets_then_expr()?;
            self.expect(Token::Else, "'else'")?;
            let else_expr = self.parse_lets_then_expr()?;
            let span = Span::join(start, else_expr.span());
            Ok(Expr::If {
                cond: Box::new(cond),
                then_br: Box::new(then_expr),
                else_br: Box::new(else_expr),
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
            self.bump();
            let right = self.parse_and_expr()?;
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
            self.bump();
            let right = self.parse_cmp_expr()?;
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
                self.bump();
                let right = self.parse_add_expr()?;
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
                self.bump();
                let right = self.parse_mul_expr()?;
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
                self.bump();
                let right = self.parse_unary_expr()?;
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
                self.bump();
                let expr = self.parse_unary_expr()?;
                Ok(Expr::Unary {
                    op: UnaryOp::Neg,
                    expr: Box::new(expr.clone()),
                    span: Span::join(start, expr.span()),
                })
            }
            Token::Not => {
                let start = self.cur.span;
                self.bump();
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
                        self.bump();
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
                        self.bump();
                        let span = Span::join(expr.span(), self.cur.span);
                        expr = Expr::Member { target: Box::new(expr), key, span };
                        continue;
                    } else {
                        break;
                    }
                }
                Token::Dot => {
                    // member: . ident | . "string"
                    self.bump(); // ".'
                    match &self.cur.tok {
                        Token::Ident(ref s) => {
                            let key =
                                MemberKey::Ident(Ident { name: s.clone(), span: self.cur.span });
                            self.bump();
                            let span = Span::join(expr.span(), self.cur.span);
                            expr = Expr::Member { target: Box::new(expr), key, span };
                        }
                        Token::String(ref s) => {
                            let key = MemberKey::Str { value: s.clone(), span: self.cur.span };
                            self.bump();
                            let span = Span::join(expr.span(), self.cur.span);
                            expr = Expr::Member { target: Box::new(expr), key, span };
                        }
                        _ => {
                            let (t, s) = (self.cur.tok.clone(), self.cur.span);
                            return Err(ParseError::unexpected(
                                s,
                                t,
                                "identifier or string after '.'",
                            ));
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
                    self.bump(); // '['
                                 // optional first expr
                    let mut first: Option<Expr> = None;
                    if !self.at(&Token::RBracket) && !self.at(&Token::Colon) {
                        first = Some(self.parse_if_or_expr()?);
                    }
                    if self.at(&Token::Colon) {
                        // slice: [':' [expr] ']'
                        self.bump(); // ':'
                        let mut second: Option<Expr> = None;
                        if !self.at(&Token::RBracket) {
                            second = Some(self.parse_if_or_expr()?);
                        }
                        let end_span = self.expect(Token::RBracket, "']' for slice")?;
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
                                return Err(ParseError::expected_expr(self.cur.span));
                            }
                        };
                        let end_span = self.expect(Token::RBracket, "']' for index")?;
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
                    self.bump(); // '('
                    let mut args = Vec::new();
                    if !self.at(&Token::RParen) {
                        let arg = self.parse_if_or_expr()?;
                        args.push(arg);
                        while self.eat(&Token::Comma) {
                            let arg = self.parse_if_or_expr()?;
                            args.push(arg);
                        }
                    }
                    let end_span = self.expect(Token::RParen, "')' to close call")?;
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
                self.bump();
                Ok(Expr::Null(s))
            }
            Token::True => {
                let s = self.cur.span;
                self.bump();
                Ok(Expr::Bool { value: true, span: s })
            }
            Token::False => {
                let s = self.cur.span;
                self.bump();
                Ok(Expr::Bool { value: false, span: s })
            }
            Token::NumberFloat(n) => {
                let s = self.cur.span;
                let v = *n;
                self.bump();
                Ok(Expr::Number { lexeme: v.to_string(), kind: NumericKind::Float, span: s })
            }
            Token::NumberInt(n) => {
                let s = self.cur.span;
                let v = *n;
                self.bump();
                Ok(Expr::Number { lexeme: v.to_string(), kind: NumericKind::Int, span: s })
            }
            Token::String(st) => {
                let s = self.cur.span;
                let v = st.clone();
                self.bump();
                Ok(Expr::String { value: v, span: s })
            }
            Token::Dollar => {
                let dollar_span = self.cur.span;
                self.bump();
                match &self.cur.tok {
                    Token::Ident(name) => {
                        let name_span = self.cur.span;
                        let span = Span::join(dollar_span, name_span);
                        let v = name.clone();
                        self.bump();
                        Ok(Expr::Variable { name: Ident { name: v, span } })
                    }
                    _ => Err(ParseError::expected_ident(dollar_span)),
                }
            }
            Token::Dot => {
                // this
                let s = self.cur.span;
                self.bump();
                Ok(Expr::This(s))
            }
            Token::LParen => {
                self.bump();
                let inner = self.parse_if_or_expr()?;
                let end_span = self.expect(Token::RParen, "')' to close group")?;
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
                    self.bump();

                    // if is namespace call we need to check for colon too
                    if self.eat(&Token::Colon) {
                        v.push(':');
                        let ident = self.expect_ident()?;
                        v.push_str(&ident.name);
                    }

                    Ok(Expr::FunctionRef { name: v, span: s })
                } else {
                    Err(ParseError::expected_expr(self.cur.span))
                }
            }
        }
    }

    fn parse_array_like(&mut self) -> ParseResult<Expr> {
        let start = self.cur.span;
        self.bump(); // '['

        // comprehension or literal?
        if self.at(&Token::For) {
            self.bump(); // 'for'
            self.expect(Token::LParen, "'(' after for")?;
            let seq = self.parse_if_or_expr()?;
            self.expect(Token::RParen, "')' after sequence")?;
            println!("seq: {:?}", seq);
            let body = self.parse_lets_then_expr()?;
            let filter = if self.at(&Token::If) {
                self.bump(); // 'if'
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
                elems.push(self.parse_if_or_expr()?);
                while self.eat(&Token::Comma) {
                    elems.push(self.parse_if_or_expr()?);
                }
            }
            let end_span = self.expect(Token::RBracket, "']' to close array")?;
            let span = Span::join(start, end_span);
            Ok(Expr::ArrayLiteral { elements: elems, span })
        }
    }

    fn parse_object_like(&mut self) -> ParseResult<Expr> {
        let start = self.cur.span;
        self.bump(); // '{'

        if self.at(&Token::For) {
            self.bump(); // 'for'
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
                self.bump(); // 'if'
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

            let mut entries = Vec::new();
            if !self.at(&Token::RBrace) {
                loop {
                    // entry = key ':' expr | '*' ':' expr
                    let entry = match &self.cur.tok {
                        Token::Star => {
                            let star_span = self.cur.span;
                            self.bump();
                            self.expect(Token::Colon, "':' after '*'")?;
                            let v = self.parse_if_or_expr()?;
                            let span = Span::join(star_span, v.span());
                            ObjectEntry::Spread { value: v, span }
                        }
                        Token::String(s) => {
                            let kspan = self.cur.span;
                            let key = ObjectKey::Str { value: s.clone(), span: self.cur.span };
                            self.bump();
                            self.expect(Token::Colon, "':' after object key")?;
                            let v = self.parse_if_or_expr()?;
                            let span = Span::join(kspan, v.span());
                            ObjectEntry::Pair { key, value: v, span }
                        }
                        Token::Ident(id) => {
                            let kspan = self.cur.span;
                            let key =
                                ObjectKey::Ident(Ident { name: id.clone(), span: self.cur.span });
                            self.bump();
                            self.expect(Token::Colon, "':' after object key")?;
                            let v = self.parse_if_or_expr()?;
                            let span = Span::join(kspan, v.span());
                            ObjectEntry::Pair { key, value: v, span }
                        }
                        _ => {
                            return Err(ParseError::unexpected(
                                self.cur.span,
                                self.cur.tok.clone(),
                                "object key (identifier or string) or '*'",
                            ))
                        }
                    };
                    entries.push(entry);
                    if self.eat(&Token::Comma) {
                        // continue reading entries
                        continue;
                    } else {
                        break;
                    }
                }
            }
            let end_span = self.expect(Token::RBrace, "'}' to close object")?;
            let span = Span::join(start, end_span);
            let obj = Expr::ObjectLiteral { entries, span };
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
            let _ = self.bump();
            true
        } else {
            false
        }
    }

    fn bump(&mut self) -> Tok {
        let old = if let Some(pk) = self.peeked.take() {
            mem::replace(&mut self.cur, pk)
        } else {
            let nt = next_token(&mut self.lx).unwrap_or_else(|e| {
                // Convert lexer error into stream of EoF + store error for later:
                // But we reutrn error at call sites, so here we panic only if this function
                // is used wrong. To keep Result flow, prefer not to call bump when lexer errored.
                panic!("lexer error bubbled intp bump(): {e:?}")
            });
            mem::replace(&mut self.cur, nt)
        };
        old
    }

    fn expect(&mut self, t: Token, expected: &'static str) -> ParseResult<Span> {
        if mem::discriminant(&self.cur.tok) == mem::discriminant(&t) {
            let s = self.cur.span;
            self.bump();
            Ok(s)
        } else {
            Err(ParseError::unexpected(self.cur.span, self.cur.tok.clone(), expected))
        }
    }

    fn expect_ident(&mut self) -> ParseResult<Ident> {
        match &self.cur.tok {
            Token::Ident(s) => {
                let ident = Ident { name: s.clone(), span: self.cur.span };
                self.bump();
                Ok(ident)
            }
            _ => Err(ParseError::expected_ident(self.cur.span)),
        }
    }
}

fn next_token(lx: &mut Lexer<'_>) -> Result<Tok, ParseError> {
    match lx.next_token() {
        Ok((t, s)) => Ok(Tok { tok: t, span: s }),
        Err(le) => Err(ParseError { span: le.span, kind: ParseErrorKind::Lex(le.kind) }),
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
