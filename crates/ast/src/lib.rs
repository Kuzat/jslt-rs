use std::fmt;
use std::fmt::Formatter;

// Basic span info needed by parser and diagnostics
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize, // byte offset (inclusive)
    pub end: usize,   // byte offset (exclusive)
    pub line: u32,    // (1-based)
    pub column: u32,  // (1-based)
}

impl Span {
    pub fn join(a: Span, b: Span) -> Span {
        Span {
            start: a.start.min(b.start),
            end: a.end.max(b.end),
            line: a.line, // best-effort; for code frames we'll use start/end offsets anyway
            column: a.column,
        }
    }
}

// Program root: defs, lets, and the final expression
#[derive(Debug, Clone)]
pub struct Program {
    pub defs: Vec<Def>,
    pub lets: Vec<Let>,
    pub body: Expr,
    pub span: Span,
}

// def name(param1, param2, ...) expr
#[derive(Debug, Clone)]
pub struct Def {
    pub name: Ident,
    pub params: Vec<Ident>,
    pub body: Expr,
    pub span: Span,
}

// let a = expr; b = expr;
#[derive(Debug, Clone)]
pub struct Let {
    pub bindings: Vec<Binding>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub name: Ident,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Expr {
    // Literals / base
    Null(Span),
    Bool {
        value: bool,
        span: Span,
    },
    Number {
        lexeme: String,
        span: Span,
    },
    String {
        value: String,
        span: Span,
    },
    This(Span), // "."
    Variable {
        name: Ident,
    }, // $ + ident

    // Control
    If {
        cond: Box<Expr>,
        then_br: Box<Expr>,
        else_br: Box<Expr>,
        span: Span,
    },
    // Unary / Binary
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
        span: Span,
    },
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },

    // Postfix chainable operations (member/index/call)
    Member {
        target: Box<Expr>,
        // "ident" or "string" per spec; we store both options
        key: MemberKey,
        span: Span,
    },
    Index {
        target: Box<Expr>,
        index: Box<Expr>,
        span: Span,
    },
    Slice {
        target: Box<Expr>,
        start: Option<Box<Expr>>,
        end: Option<Box<Expr>>,
        span: Span,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
        span: Span,
    },

    // Arrays
    ArrayLiteral {
        elements: Vec<Expr>,
        span: Span,
    },
    ArrayFor {
        seq: Box<Expr>,            // for (seq)
        body: Box<Expr>,           // expr
        filter: Option<Box<Expr>>, // optional "if cond"
        span: Span,
    },

    // objects
    ObjectLiteral {
        entries: Vec<ObjectEntry>,
        span: Span,
    },
    ObjectFor {
        seq: Box<Expr>,            // for (seq)
        key: Box<Expr>,            // key expr (must evaluaate to string later)
        value: Box<Expr>,          // value expr
        filter: Option<Box<Expr>>, // optional "if cond"
        span: Span,
    },

    // Parenthesized
    Group {
        expr: Box<Expr>,
        span: Span,
    },
}

#[derive(Debug, Clone)]
pub enum MemberKey {
    Ident(Ident),
    Str { value: String, span: Span },
}

#[derive(Debug, Clone)]
pub enum ObjectEntry {
    // key: expr
    Pair { key: ObjectKey, value: Expr, span: Span },
    // *: expr
    Spread { value: Expr, span: Span },
}

#[derive(Debug, Clone)]
pub enum ObjectKey {
    Ident(Ident),
    Str { value: String, span: Span },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Not, // "!"
    Neg, // "-"
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Mul,
    Div,
    Rem, // * / %
    Add,
    Sub, // + -
    Lt,
    Le,
    Gt,
    Ge, // < <= > >=
    Eq,
    Ne, // == !=
    And,
    Or, // and or
}

impl Expr {
    pub fn span(&self) -> Span {
        use Expr::*;
        match self {
            Null(s) => *s,
            Bool { span, .. } => *span,
            Number { span, .. } => *span,
            String { span, .. } => *span,
            This(s) => *s,
            Variable { name } => name.span,
            If { span, .. } => *span,
            Unary { span, .. } => *span,
            Binary { span, .. } => *span,
            Member { span, .. } => *span,
            Index { span, .. } => *span,
            Slice { span, .. } => *span,
            Call { span, .. } => *span,
            ArrayLiteral { span, .. } => *span,
            ArrayFor { span, .. } => *span,
            ObjectLiteral { span, .. } => *span,
            ObjectFor { span, .. } => *span,
            Group { span, .. } => *span,
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for d in &self.defs {
            writeln!(f, "{d}")?;
        }
        if !self.lets.is_empty() {
            for l in &self.lets {
                writeln!(f, "{l}")?;
            }
        }
        write!(f, "{}", self.body)
    }
}

impl fmt::Display for Def {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "def {}(", self.name)?;
        for (i, p) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{p}")?;
        }
        write!(f, ") ")?;
        write!(f, "{}", self.body)
    }
}

impl fmt::Display for Let {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "let ")?;
        for (i, b) in self.bindings.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{b}")?;
        }
        write!(f, ";")
    }
}

impl fmt::Display for Binding {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}", self.name, self.value)
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // Identifiers are validate by lexer/parser; we just print the name.
        write!(f, "{}", self.name)
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Pretty { f }.expr(self, Prec::Lowest)
    }
}

// Precedence levels
#[derive(Clone, Copy, PartialEq, PartialOrd)]
enum Prec {
    Lowest,
    If,
    Or,
    And,
    Cmp,
    Add,
    Mul,
    Unary,
    Postfix,
}

struct Pretty<'a, 'b> {
    f: &'a mut Formatter<'b>,
}

impl<'a, 'b> Pretty<'a, 'b> {
    fn expr(&mut self, e: &Expr, parent: Prec) -> fmt::Result {
        use BinaryOp::*;
        use Expr::*;

        match e {
            Null(_s) => write!(self.f, "null"),
            Bool { value, .. } => write!(self.f, "{}", value),
            Number { lexeme, .. } => write!(self.f, "{}", lexeme),
            String { value, .. } => self.string(value),
            This(_s) => write!(self.f, "."),
            Variable { name } => write!(self.f, "${}", name),

            Group { expr, .. } => {
                write!(self.f, "(")?;
                self.expr(expr, Prec::Lowest)?;
                write!(self.f, ")")
            }

            If { cond, then_br, else_br, .. } => {
                // if has the lowest precedence among expressions
                let me = Prec::If;
                let need_paren = parent > me;
                if need_paren {
                    write!(self.f, "(")?;
                }
                write!(self.f, "if (")?;
                self.expr(cond, Prec::Lowest)?;
                write!(self.f, ") ")?;
                self.expr(then_br, Prec::Lowest)?;
                write!(self.f, " else ")?;
                self.expr(else_br, Prec::Lowest)?;
                if need_paren {
                    write!(self.f, ")")?;
                }
                Ok(())
            }

            Unary { op, expr, .. } => {
                let me = Prec::Unary;
                let need_paren = parent > me;
                if need_paren {
                    write!(self.f, "(")?;
                }
                match op {
                    UnaryOp::Not => write!(self.f, "not ")?,
                    UnaryOp::Neg => write!(self.f, "-")?,
                }
                // For unary, child precedence must be strictly higher than Unary to avoid ambiguity
                self.expr(expr, Prec::Postfix)?;
                if need_paren {
                    write!(self.f, ")")?;
                }
                Ok(())
            }

            Binary { op, left, right, .. } => {
                let (me, kw) = match op {
                    Mul => (Prec::Mul, "*"),
                    Div => (Prec::Mul, "/"),
                    Rem => (Prec::Mul, "%"),
                    Add => (Prec::Add, "+"),
                    Sub => (Prec::Add, "-"),
                    Lt => (Prec::Cmp, "<"),
                    Le => (Prec::Cmp, "<="),
                    Gt => (Prec::Cmp, ">"),
                    Ge => (Prec::Cmp, ">="),
                    Eq => (Prec::Cmp, "=="),
                    Ne => (Prec::Cmp, "!="),
                    And => (Prec::And, "and"),
                    Or => (Prec::Or, "or"),
                };
                let need_paren = parent > me;
                if need_paren {
                    write!(self.f, "(")?;
                }
                self.expr(left, me)?;
                write!(self.f, " {} ", kw)?;
                // left-associative operators: pass strictly higher precedence to right child
                self.expr(right, me)?;
                if need_paren {
                    write!(self.f, ")")?;
                }
                Ok(())
            }

            Member { target, key, .. } => {
                self.expr(target, Prec::Postfix)?;
                write!(self.f, ".")?;
                match key {
                    MemberKey::Ident(id) => write!(self.f, "{id}"),
                    MemberKey::Str { value, .. } => self.string(value),
                }
            }

            Index { target, index, .. } => {
                self.expr(target, Prec::Postfix)?;
                write!(self.f, "[")?;
                self.expr(index, Prec::Lowest)?;
                write!(self.f, "]")
            }

            Slice { target, start, end, .. } => {
                self.expr(target, Prec::Postfix)?;
                write!(self.f, "[")?;
                if let Some(s) = start {
                    self.expr(s, Prec::Lowest)?;
                }
                write!(self.f, ":")?;
                if let Some(e2) = end {
                    self.expr(e2, Prec::Lowest)?;
                }

                write!(self.f, "]")
            }

            Call { callee, args, .. } => {
                self.expr(callee, Prec::Postfix)?;
                write!(self.f, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(self.f, ", ")?;
                    }
                    self.expr(arg, Prec::Lowest)?;
                }
                write!(self.f, ")")
            }

            ArrayLiteral { elements, .. } => {
                write!(self.f, "[")?;
                for (i, e) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(self.f, ", ")?;
                    }
                    self.expr(e, Prec::Lowest)?;
                }
                write!(self.f, "]")
            }

            ArrayFor { seq, body, filter, .. } => {
                write!(self.f, "[for (")?;
                self.expr(seq, Prec::Lowest)?;
                write!(self.f, ") ")?;
                self.expr(body, Prec::Lowest)?;
                if let Some(fx) = filter {
                    write!(self.f, " if ")?;
                    self.expr(fx, Prec::Lowest)?;
                }
                write!(self.f, "]")
            }

            ObjectLiteral { entries, .. } => {
                write!(self.f, "{{")?;
                for (i, e) in entries.iter().enumerate() {
                    if i > 0 {
                        write!(self.f, ", ")?;
                    }
                    match e {
                        ObjectEntry::Pair { key, value, .. } => {
                            self.obj_key(key)?;
                            write!(self.f, ": ")?;
                            self.expr(value, Prec::Lowest)?;
                        }
                        ObjectEntry::Spread { value, .. } => {
                            write!(self.f, "*: ")?;
                            self.expr(value, Prec::Lowest)?;
                        }
                    }
                }
                write!(self.f, "}}")
            }

            ObjectFor { seq, key, value, filter, .. } => {
                write!(self.f, "{{for (")?;
                self.expr(seq, Prec::Lowest)?;
                write!(self.f, ") ")?;
                self.expr(key, Prec::Lowest)?;
                write!(self.f, ": ")?;
                self.expr(value, Prec::Lowest)?;
                if let Some(fx) = filter {
                    write!(self.f, " if ")?;
                    self.expr(fx, Prec::Lowest)?;
                }
                write!(self.f, "}}")
            }
        }
    }

    fn obj_key(&mut self, k: &ObjectKey) -> fmt::Result {
        match k {
            ObjectKey::Ident(id) => write!(self.f, "{id}"),
            ObjectKey::Str { value, .. } => self.string(value),
        }
    }

    fn string(&mut self, s: &str) -> fmt::Result {
        // Emit JSON escapes as per spec.md
        write!(self.f, "\"")?;
        for ch in s.chars() {
            match ch {
                '"' => write!(self.f, "\\\"")?,
                '\\' => write!(self.f, "\\\\")?,
                '\u{08}' => write!(self.f, "\\b")?,
                '\u{0C}' => write!(self.f, "\\f")?,
                '\n' => write!(self.f, "\\n")?,
                '\r' => write!(self.f, "\\r")?,
                '\t' => write!(self.f, "\\t")?,
                c if c.is_control() => {
                    let code = c as u32;
                    write!(self.f, "\\u{:04X}", code)?;
                }
                c => write!(self.f, "{}", c)?,
            }
        }
        write!(self.f, "\"")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {}
}
