use std::fmt;
use std::fmt::Formatter;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize, // byte offset (inclusive)
    pub end: usize,   // byte offset (exclusive)

    pub line: u32,   // (1-based)
    pub column: u32, // (1-based)
}

impl Span {
    pub fn new(start: usize, end: usize, line: u32, column: u32) -> Span {
        Span { start, end, line, column }
    }

    // A zero-length span at a point (useful for EOF or missing tokens)
    pub fn single_point(at: usize, line: u32, column: u32) -> Span {
        Span { start: at, end: at, line, column }
    }

    pub fn join(a: Span, b: Span) -> Span {
        if a.start <= b.start {
            Span { start: a.start, end: a.end.max(b.end), line: a.line, column: a.column }
        } else {
            Span { start: b.start, end: a.end.max(b.end), line: b.line, column: b.column }
        }
    }

    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    pub fn len_bytes(&self) -> usize {
        self.end.saturating_sub(self.start)
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
pub enum NumericKind {
    Int,
    Float,
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
        kind: NumericKind,
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
        key: Box<Expr>,            // key expr (must evaluate to string later)
        value: Box<Expr>,          // value expr
        filter: Option<Box<Expr>>, // optional "if cond"
        span: Span,
    },

    // Parenthesized
    Group {
        expr: Box<Expr>,
        span: Span,
    },

    // Function reference: bare identifiers (e.g. foo) - resolved in binder
    FunctionRef {
        name: String,
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
            FunctionRef { span, .. } => *span,
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
        Ok(())
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
            Variable { name } => write!(self.f, "${}", name.name),

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
                // Special case: member access on this
                if let This(_) = target.as_ref() {
                    write!(self.f, ".")?;
                } else {
                    self.expr(target, Prec::Postfix)?;
                    write!(self.f, ".")?;
                }
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

            FunctionRef { name, .. } => {
                write!(self.f, "{}", name)
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

    fn sp() -> Span {
        Span { start: 0, end: 0, line: 1, column: 1 }
    }

    fn id(name: &str) -> Ident {
        Ident { name: name.to_string(), span: sp() }
    }

    fn num(n: &str) -> Expr {
        let kind = if n.contains('.') || n.contains('e') || n.contains('E') {
            NumericKind::Float
        } else {
            NumericKind::Int
        };
        Expr::Number { lexeme: n.to_string(), kind, span: sp() }
    }

    fn str_(s: &str) -> Expr {
        Expr::String { value: s.to_string(), span: sp() }
    }

    fn bool_(b: bool) -> Expr {
        Expr::Bool { value: b, span: sp() }
    }

    fn var(name: &str) -> Expr {
        Expr::Variable { name: id(name) }
    }

    fn this() -> Expr {
        Expr::This(sp())
    }

    fn unary_not(e: Expr) -> Expr {
        Expr::Unary { op: UnaryOp::Not, expr: Box::new(e), span: sp() }
    }

    fn unary_neg(e: Expr) -> Expr {
        Expr::Unary { op: UnaryOp::Neg, expr: Box::new(e), span: sp() }
    }

    fn bin(op: BinaryOp, l: Expr, r: Expr) -> Expr {
        Expr::Binary { op, left: Box::new(l), right: Box::new(r), span: sp() }
    }

    fn member_ident(target: Expr, key: &str) -> Expr {
        Expr::Member { target: Box::new(target), key: MemberKey::Ident(id(key)), span: sp() }
    }

    fn member_str(target: Expr, key: &str) -> Expr {
        Expr::Member {
            target: Box::new(target),
            key: MemberKey::Str { value: key.to_string(), span: sp() },
            span: sp(),
        }
    }

    fn index(target: Expr, i: Expr) -> Expr {
        Expr::Index { target: Box::new(target), index: Box::new(i), span: sp() }
    }

    fn slice(target: Expr, start: Option<Expr>, end: Option<Expr>) -> Expr {
        Expr::Slice {
            target: Box::new(target),
            start: start.map(Box::new),
            end: end.map(Box::new),
            span: sp(),
        }
    }

    fn call(callee: Expr, args: Vec<Expr>) -> Expr {
        Expr::Call { callee: Box::new(callee), args, span: sp() }
    }

    fn group(e: Expr) -> Expr {
        Expr::Group { expr: Box::new(e), span: sp() }
    }

    #[test]
    fn print_literals_and_variables() {
        assert_eq!(format!("{}", Expr::Null(sp())), "null");
        assert_eq!(format!("{}", bool_(true)), "true");
        assert_eq!(format!("{}", bool_(false)), "false");
        assert_eq!(format!("{}", num("0")), "0");
        assert_eq!(format!("{}", num("123.45")), "123.45");
        assert_eq!(format!("{}", str_("a\\b\"c\n")), "\"a\\\\b\\\"c\\n\"");
        assert_eq!(format!("{}", this()), ".");
        assert_eq!(format!("{}", var("foo")), "$foo");
    }

    #[test]
    fn parentheses_and_precedence() {
        // 1 + 2 * 3 -> 1 + 2 * 3
        let e = bin(BinaryOp::Add, num("1"), bin(BinaryOp::Mul, num("2"), num("3")));
        assert_eq!(format!("{}", e), "1 + 2 * 3");

        // (1 + 2) * 3
        let e = bin(BinaryOp::Mul, group(bin(BinaryOp::Add, num("1"), num("2"))), num("3"));
        assert_eq!(format!("{}", e), "(1 + 2) * 3");

        // -(1 + 2)
        let e = unary_neg(group(bin(BinaryOp::Add, num("1"), num("2"))));
        assert_eq!(format!("{}", e), "-(1 + 2)");

        // not a and (b or c)
        let e = bin(BinaryOp::And, unary_not(var("a")), bin(BinaryOp::Or, var("b"), var("c")));
        assert_eq!(format!("{}", e), "not $a and ($b or $c)");

        // Comparison precedence wrt add
        let e = bin(BinaryOp::Lt, bin(BinaryOp::Add, num("1"), num("2")), num("3"));
        assert_eq!(format!("{}", e), "1 + 2 < 3");

        // If expression associates low and gets parens when nested
        let e = Expr::If {
            cond: Box::new(bool_(true)),
            then_br: Box::new(num("1")),
            else_br: Box::new(num("2")),
            span: sp(),
        };
        let wrapped = bin(BinaryOp::Add, e.clone(), num("1"));
        assert_eq!(format!("{}", wrapped), "(if (true) 1 else 2) + 1");
    }

    #[test]
    fn members_calls_indexing_and_slicing() {
        // Member by ident and quoted string
        assert_eq!(format!("{}", member_ident(var("a"), "b")), "$a.b");
        assert_eq!(format!("{}", member_str(var("a"), "x y")), "$a.\"x y\"");
        assert_eq!(format!("{}", member_str(var("a"), "quote:\"")), "$a.\"quote:\\\"\"");

        // Calls
        let call_expr = call(member_ident(var("f"), "g"), vec![num("1"), str_("s"), var("x")]);
        assert_eq!(format!("{}", call_expr), "$f.g(1, \"s\", $x)");

        // Indexing
        assert_eq!(format!("{}", index(var("a"), num("0"))), "$a[0]");

        // Slicing variants
        assert_eq!(format!("{}", slice(var("a"), Some(num("1")), Some(num("2")))), "$a[1:2]");
        assert_eq!(format!("{}", slice(var("a"), None, Some(num("2")))), "$a[:2]");
        assert_eq!(format!("{}", slice(var("a"), Some(num("1")), None)), "$a[1:]");
        assert_eq!(format!("{}", slice(var("a"), None, None)), "$a[:]");
    }

    #[test]
    fn arrays_and_objects_with_quoted_keys_and_spread() {
        // Array literal
        let arr =
            Expr::ArrayLiteral { elements: vec![num("1"), str_("two"), bool_(false)], span: sp() };
        assert_eq!(format!("{}", arr), "[1, \"two\", false]");

        // Object literal with ident key, quoted key, and spread
        let obj = Expr::ObjectLiteral {
            entries: vec![
                ObjectEntry::Pair { key: ObjectKey::Ident(id("a")), value: num("1"), span: sp() },
                ObjectEntry::Pair {
                    key: ObjectKey::Str { value: "x y".to_string(), span: sp() },
                    value: str_("v"),
                    span: sp(),
                },
                ObjectEntry::Spread { value: member_ident(var("$"), "rest"), span: sp() },
            ],
            span: sp(),
        };
        assert_eq!(format!("{}", obj), "{a: 1, \"x y\": \"v\", *: $$.rest}");
    }

    #[test]
    fn for_comprehensions_arrays_and_objects() {
        // [for (seq) body]
        let arr_for_simple = Expr::ArrayFor {
            seq: Box::new(var("seq")),
            body: Box::new(var("x")),
            filter: None,
            span: sp(),
        };
        assert_eq!(format!("{}", arr_for_simple), "[for ($seq) $x]");

        // [for (seq) body if cond]
        let arr_for_filter = Expr::ArrayFor {
            seq: Box::new(var("seq")),
            body: Box::new(num("1")),
            filter: Some(Box::new(bool_(true))),
            span: sp(),
        };
        assert_eq!(format!("{}", arr_for_filter), "[for ($seq) 1 if true]");

        // {for (seq) key: value}
        let obj_for_simple = Expr::ObjectFor {
            seq: Box::new(var("seq")),
            key: Box::new(str_("k")),
            value: Box::new(var("v")),
            filter: None,
            span: sp(),
        };
        assert_eq!(format!("{}", obj_for_simple), "{for ($seq) \"k\": $v}");

        // {for (seq) key: value if cond}
        let obj_for_filter = Expr::ObjectFor {
            seq: Box::new(var("seq")),
            key: Box::new(member_ident(var("a"), "b")),
            value: Box::new(num("2")),
            filter: Some(Box::new(bin(BinaryOp::Eq, var("x"), num("3")))),
            span: sp(),
        };
        assert_eq!(format!("{}", obj_for_filter), "{for ($seq) $a.b: 2 if $x == 3}");
    }

    #[test]
    fn program_with_defs_lets_and_body() {
        let def = Def {
            name: id("f"),
            params: vec![id("x"), id("y")],
            body: bin(BinaryOp::Add, var("x"), var("y")),
            span: sp(),
        };
        let let_stmt = Let {
            bindings: vec![
                Binding { name: id("a"), value: num("1"), span: sp() },
                Binding { name: id("b"), value: str_("s"), span: sp() },
            ],
            span: sp(),
        };
        let prog = Program {
            defs: vec![def],
            lets: vec![let_stmt],
            body: bin(BinaryOp::Mul, var("a"), num("10")),
            span: sp(),
        };
        let rendered = format!("{}", prog);
        let expected = "def f(x, y) $x + $y\nlet a = 1, b = \"s\"\n$a * 10";
        assert_eq!(rendered, expected);
    }
}
