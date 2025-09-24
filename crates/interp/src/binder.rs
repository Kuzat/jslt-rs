use ast::{Program, Span};
use std::collections::{BTreeSet, HashMap};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(pub usize);

// Where a variable comes from at runetime
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedVar {
    // In the current frame: slot index
    Local(usize),
    //Captured from an outer frame at 'depth' (1 = direct parent, 2 = grandparent, etc)
    Captured { depth: usize, slot: usize },
    // Special case for the '.'
    This,
}

// Bound program mirrors the source program but replaces unresolved names with handles.
#[derive(Debug, Clone)]
pub struct BoundProgram {
    pub functions: Vec<BoundFunction>,
    pub lets: Vec<(String, BoundExpr)>,
    pub body: BoundExpr,
}

// Bound function holds closure metadata and the bound body
#[derive(Debug, Clone)]
pub struct BoundFunction {
    pub id: FunctionId,
    pub name: String,
    pub params: Vec<String>,
    // Captures in deterministic order the evaluator can materialize (e.g., Vec<ResolvedVarPlan>)
    // We store a stable list of (from_depth, slot) pairs in the order of first encounter.
    pub captures: Vec<CaptureSpec>,
    pub body: BoundExpr,
}

// A capture is always from some outer scope; depth >= 1
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct CaptureSpec {
    pub depth: usize,
    pub slot: usize,
}

#[derive(Debug, Clone)]
pub enum BoundExpr {
    Null,
    Bool(bool),
    Number(f64),
    String(String),

    // Context '.'
    This,

    // Variables '$name'
    Var(ResolvedVar),

    // Function calls 'f(a, b)'
    Call(FunctionId, Vec<BoundExpr>),

    // Unary
    Not(Box<BoundExpr>),
    Neg(Box<BoundExpr>),

    // Binary ops
    Add(Box<BoundExpr>, Box<BoundExpr>),
    Sub(Box<BoundExpr>, Box<BoundExpr>),
    Mul(Box<BoundExpr>, Box<BoundExpr>),
    Div(Box<BoundExpr>, Box<BoundExpr>),
    Mod(Box<BoundExpr>, Box<BoundExpr>),
    Eq(Box<BoundExpr>, Box<BoundExpr>),
    Ne(Box<BoundExpr>, Box<BoundExpr>),
    Lt(Box<BoundExpr>, Box<BoundExpr>),
    Le(Box<BoundExpr>, Box<BoundExpr>),
    Gt(Box<BoundExpr>, Box<BoundExpr>),
    Ge(Box<BoundExpr>, Box<BoundExpr>),
    And(Box<BoundExpr>, Box<BoundExpr>),
    Or(Box<BoundExpr>, Box<BoundExpr>),

    // Conditional
    If {
        cond: Box<BoundExpr>,
        then_br: Box<BoundExpr>,
        else_br: Box<BoundExpr>,
    },

    // postfix: member, index/slice,  comprehensions
    ArrayLiteral(Vec<BoundExpr>),
    ObjectLiteral(Vec<(BoundExpr, BoundExpr)>, Option<Box<BoundExpr>> /* spread */),

    // Array comprehensions
    ArrayFor {
        seq: Box<BoundExpr>,
        elem: Box<BoundExpr>,
        filter: Option<Box<BoundExpr>>,
    },

    ObjectFor {
        seq: Box<BoundExpr>,
        key: Box<BoundExpr>,
        value: Box<BoundExpr>,
        filter: Option<Box<BoundExpr>>,
    },

    // member access: .k or ."quoted"
    Member(Box<BoundExpr>, ObjectKey),

    // Indexing [i] and slicing [start:end] - we keep both; evaluator will implement semantics
    Index(Box<BoundExpr>, Box<BoundExpr>),
    Slice {
        target: Box<BoundExpr>,
        start: Option<Box<BoundExpr>>,
        end: Option<Box<BoundExpr>>,
    },
}

#[derive(Debug, Clone)]
pub enum ObjectKey {
    Ident(String),
    String(String),
}

#[derive(Debug, thiserror::Error)]
pub enum BindError {
    #[error("unknown variable: `{name}` at {span:?}, did you mean: {suggestions:?}")]
    UnknownVariable { name: String, span: Span, suggestions: Vec<String> },
    #[error("unknown function: `{name}` at {span:?}, did you mean: {suggestions:?}")]
    UnknownFunction { name: String, span: Span, suggestions: Vec<String> },
    #[error("attempted to call a non-function expression at {span:?}")]
    NonFunctionCallee { span: Span },
}

fn suggest<'a>(name: &str, pool: impl IntoIterator<Item = &'a str>) -> Vec<String> {
    let mut scored: Vec<(usize, String)> =
        pool.into_iter().map(|cand| (edit_distance(name, cand), cand.to_string())).collect();
    scored.sort_by_key(|(d, s)| (*d, s.clone()));
    scored.into_iter().take(3).filter(|(d, _)| *d <= 3).map(|(_, s)| s).collect()
}

// small Levenshtein (no deps).
fn edit_distance(a: &str, b: &str) -> usize {
    let a: Vec<char> = a.chars().collect();
    let b: Vec<char> = b.chars().collect();
    let (m, n) = (a.len(), b.len());
    if m == 0 {
        return n;
    }
    if n == 0 {
        return m;
    }
    let mut dp = vec![0usize; n + 1];
    for j in 0..=n {
        dp[j] = j;
    }
    for i in 1..=m {
        let mut prev = dp[0];
        dp[0] = i;
        for j in 1..=n {
            let temp = dp[j];
            let cost = if a[i - 1] == b[j - 1] { 0 } else { 1 };
            dp[j] = (dp[j] + 1).min(dp[j - 1] + 1).min(prev + cost);
            prev = temp;
        }
    }
    dp[n]
}

#[derive(Debug, Default)]
struct VarFrame {
    // Order matters: slot is index in 'order'
    order: Vec<String>,
    // Quick lookup by name -> slot
    slots: HashMap<String, usize>,
}

impl VarFrame {
    fn new() -> Self {
        Self { order: Vec::new(), slots: HashMap::new() }
    }
    fn define(&mut self, name: &str) -> usize {
        if let Some(&slot) = self.slots.get(name) {
            return slot;
        }
        let slot = self.order.len();
        self.order.push(name.to_string());
        self.slots.insert(name.to_string(), slot);
        slot
    }
    fn lookup(&self, name: &str) -> Option<usize> {
        self.slots.get(name).copied()
    }
}

/// Two namespaces per spec: variables (let/params) and functions (def)
#[derive(Debug)]
struct Env<'a> {
    // variable frames: innermost at the back
    var_stack: Vec<VarFrame>,

    // Function namespace: grows as we see 'def' at top level or inner scopes
    fun_ns: HashMap<String, FunctionId>,

    // For deterministic suggestions
    fun_names_sorted: BTreeSet<String>,

    // For capture detection: during resolution of a function body, we collect outer captures used
    // key = a unique id currently being bound
    current_fun: Option<FunctionId>,
    // For each function being bound, a set of (depth, slot) captures discovered
    fun_captures: HashMap<FunctionId, BTreeSet<CaptureSpec>>,

    // Span resolver needs original program info
    // Pass them through visit methods. Kept here as a marker for design; not used directly.
    _phantom: std::marker::PhantomData<&'a ()>,
}

impl<'a> Env<'a> {
    fn new() -> Self {
        Self {
            var_stack: vec![VarFrame::new()],
            fun_ns: HashMap::new(),
            fun_names_sorted: BTreeSet::new(),
            current_fun: None,
            fun_captures: HashMap::new(),
            _phantom: std::marker::PhantomData,
        }
    }

    fn push_vars(&mut self) {
        self.var_stack.push(VarFrame::new());
    }
    fn pop_vars(&mut self) {
        self.var_stack.pop();
    }

    fn define_var(&mut self, name: &str) -> usize {
        let frame = self.var_stack.last_mut().expect("at least one frame");
        frame.define(name)
    }

    fn lookup_var(&mut self, name: &str) -> Option<ResolvedVar> {
        // Search from innermost to outermost
        for (depth, frame) in self.var_stack.iter().rev().enumerate() {
            if let Some(slot) = frame.lookup(name) {
                return if depth == 0 {
                    Some(ResolvedVar::Local(slot))
                } else {
                    // Record capture for current function if any
                    if let Some(fid) = self.current_fun {
                        self.fun_captures
                            .entry(fid)
                            .or_default()
                            .insert(CaptureSpec { depth, slot });
                    }
                    Some(ResolvedVar::Captured { depth, slot })
                };
            }
        }
        None
    }

    fn define_fun(&mut self, name: &str, id: FunctionId) {
        self.fun_ns.insert(name.to_string(), id);
        self.fun_names_sorted.insert(name.to_string());
    }

    fn lookup_fun(&self, name: &str) -> Option<FunctionId> {
        self.fun_ns.get(name).copied()
    }

    fn begin_fun(&mut self, id: FunctionId) {
        assert!(self.current_fun.is_none());
        self.current_fun = Some(id);
        self.fun_captures.entry(id).or_default();
    }

    fn end_fun(&mut self, id: FunctionId) -> Vec<CaptureSpec> {
        assert_eq!(self.current_fun, Some(id));
        self.current_fun = None;
        let mut caps: Vec<CaptureSpec> =
            self.fun_captures.remove(&id).unwrap_or_default().into_iter().collect();
        // Deterministic order: by (depth asc, slot asc)
        caps.sort_by_key(|c| (c.depth, c.slot));
        caps
    }

    fn var_suggestions(&self, miss: &str) -> Vec<String> {
        // Gather all visible variables names for suggestions
        let mut pool = BTreeSet::new();
        for f in &self.var_stack {
            for n in &f.order {
                pool.insert(n.clone());
            }
        }
        suggest(miss, pool.iter().map(|s| s.as_str()))
    }

    fn fun_suggestions(&self, miss: &str) -> Vec<String> {
        suggest(miss, self.fun_names_sorted.iter().map(|s| s.as_str()))
    }
}

pub struct Binder {
    env: Env<'static>,
    functions: Vec<BoundFunction>,
    next_fun_id: usize,
}

impl Binder {
    pub fn new() -> Self {
        Self { env: Env::new(), functions: Vec::new(), next_fun_id: 0 }
    }

    fn alloc_fun_id(&mut self) -> FunctionId {
        let id = FunctionId(self.next_fun_id);
        self.next_fun_id += 1;
        id
    }

    // Entry point
    pub fn bind_program(&mut self, p: &ast::Program) -> Result<BoundProgram, BindError> {
        // 1) Pre-declare all top-level functions so they are visible after their decl point only.
        //    we choose "visible after declaration": We fill as we go through defs in order.
        //    If you want "all defs visible everywhere" semantics, first pass could collect names.

        for d in &p.defs {
            let id = self.alloc_fun_id();
            self.env.define_fun(&d.name.name, id);
            // We do NOT bind bodies yet; we do it in a second pass to allow forward usage only if desired
            // The spec doesn't require hoisting; we'll bind in order so earlier defs are visible to later code.
        }

        // Pre-declare all top-level lets so they are visible after their decl point only.
        for l in &p.lets {
            for b in &l.bindings {
                let _slot = self.env.define_var(&b.name.name);
            }
        }

        // 2) Bind defs in order, producing closure captures and bound bodies
        for d in &p.defs {
            let fid = self.env.lookup_fun(&d.name.name).expect("just inserted");

            // New var scope for function params; track captures into outer frames
            self.env.begin_fun(fid);
            self.env.push_vars();
            for param in &d.params {
                self.env.define_var(&param.name);
            }
            let body = self.bind_expr(&d.body)?;
            let captures = self.env.end_fun(fid);
            self.env.pop_vars();

            self.functions.push(BoundFunction {
                id: fid,
                name: d.name.name.clone(),
                params: d.params.iter().map(|p| p.name.clone()).collect(),
                captures,
                body,
            });
        }

        // 3) Bind top-level lets values now that their names/slots exists
        let mut lets = Vec::new();
        for l in &p.lets {
            for b in &l.bindings {
                let bound_value = self.bind_expr(&b.value)?;
                lets.push((b.name.name.clone(), bound_value));
            }
        }

        // 4) bind program body
        let body = self.bind_expr(&p.body)?;

        Ok(BoundProgram { functions: self.functions.clone(), lets, body })
    }

    fn bind_expr(&mut self, e: &ast::Expr) -> Result<BoundExpr, BindError> {
        use ast::Expr;
        match e {
            Expr::Null(_s) => Ok(BoundExpr::Null),
            Expr::Bool { value, .. } => Ok(BoundExpr::Bool(*value)),
            Expr::Number { lexeme, span } => {
                // Parse once here; numbers are runetime f64 in the interpreter
                match lexeme.parse::<f64>() {
                    Ok(v) => Ok(BoundExpr::Number(v)),
                    Err(e) => {
                        // Parser/lexer should have validated; still, report a spanful binder error if it slips through
                        Err(BindError::UnknownVariable {
                            name: format!("invalid-number: {}", lexeme),
                            span: *span,
                            suggestions: vec![],
                        })
                    }
                }
            }
            Expr::String { value, .. } => Ok(BoundExpr::String(value.clone())),
            Expr::This(_) => Ok(BoundExpr::This),

            Expr::Variable { name } => {
                if let Some(v) = self.env.lookup_var(&name.name) {
                    Ok(BoundExpr::Var(v))
                } else {
                    Err(BindError::UnknownVariable {
                        name: name.name.clone(),
                        span: name.span,
                        suggestions: self.env.var_suggestions(&name.name),
                    })
                }
            }

            Expr::If { cond, then_br, else_br, .. } => Ok(BoundExpr::If {
                cond: Box::new(self.bind_expr(cond)?),
                then_br: Box::new(self.bind_expr(then_br)?),
                else_br: Box::new(self.bind_expr(else_br)?),
            }),

            Expr::Unary { op, expr, .. } => {
                use ast::UnaryOp;
                let inner = Box::new(self.bind_expr(expr)?);
                Ok(match op {
                    UnaryOp::Not => BoundExpr::Not(inner),
                    UnaryOp::Neg => BoundExpr::Neg(inner),
                })
            }

            Expr::Binary { op, left, right, .. } => {
                use ast::BinaryOp::*;
                let l = Box::new(self.bind_expr(left)?);
                let r = Box::new(self.bind_expr(right)?);
                Ok(match op {
                    Mul => BoundExpr::Mul(l, r),
                    Div => BoundExpr::Div(l, r),
                    Rem => BoundExpr::Mod(l, r),
                    Add => BoundExpr::Add(l, r),
                    Sub => BoundExpr::Sub(l, r),
                    Lt => BoundExpr::Lt(l, r),
                    Le => BoundExpr::Le(l, r),
                    Gt => BoundExpr::Gt(l, r),
                    Ge => BoundExpr::Ge(l, r),
                    Eq => BoundExpr::Eq(l, r),
                    Ne => BoundExpr::Ne(l, r),
                    And => BoundExpr::And(l, r),
                    Or => BoundExpr::Or(l, r),
                })
            }

            Expr::Member { target, key, .. } => {
                let t = Box::new(self.bind_expr(target)?);
                let bk = match key {
                    ast::MemberKey::Ident(id) => ObjectKey::Ident(id.name.clone()),
                    ast::MemberKey::Str { value, .. } => ObjectKey::String(value.clone()),
                };
                Ok(BoundExpr::Member(t, bk))
            }

            Expr::Index { target, index, .. } => Ok(BoundExpr::Index(
                Box::new(self.bind_expr(target)?),
                Box::new(self.bind_expr(index)?),
            )),

            Expr::Slice { target, start, end, .. } => Ok(BoundExpr::Slice {
                target: Box::new(self.bind_expr(target)?),
                start: match start {
                    Some(s) => Some(Box::new(self.bind_expr(s)?)),
                    None => None,
                },
                end: match end {
                    Some(e) => Some(Box::new(self.bind_expr(e)?)),
                    None => None,
                },
            }),

            Expr::Call { callee, args, span } => {
                // Only bare identifiers are functions; they parse as FunctionRef
                if let Expr::FunctionRef { name, span } = callee.as_ref() {
                    if let Some(fid) = self.env.lookup_fun(name) {
                        let mut bargs = Vec::with_capacity(args.len());
                        for a in args {
                            bargs.push(self.bind_expr(a)?);
                        }
                        Ok(BoundExpr::Call(fid, bargs))
                    } else {
                        Err(BindError::UnknownFunction {
                            name: name.clone(),
                            span: *span,
                            suggestions: self.env.fun_suggestions(&name),
                        })
                    }
                } else {
                    // Jslt does not have first-class functions; reject non-bare-id callees at bind time
                    Err(BindError::NonFunctionCallee { span: *span })
                }
            }

            Expr::ArrayLiteral { elements, .. } => {
                let mut out = Vec::with_capacity(elements.len());
                for e in elements {
                    out.push(self.bind_expr(e)?);
                }
                Ok(BoundExpr::ArrayLiteral(out))
            }

            Expr::ArrayFor { seq, body, filter, .. } => {
                let bseq = self.bind_expr(seq)?;
                let bbody = self.bind_expr(body)?;
                let bfilter =
                    if let Some(f) = filter { Some(Box::new(self.bind_expr(f)?)) } else { None };
                Ok(BoundExpr::ArrayFor {
                    seq: Box::new(bseq),
                    elem: Box::new(bbody),
                    filter: bfilter,
                })
            }

            Expr::ObjectLiteral { entries, .. } => {
                // Convert to a pair list (key-expr, value-expr). Keys are syntactic (ident or string)
                // we emit keys as string expressions so evaluator can compute object construction.
                let mut pairs: Vec<(BoundExpr, BoundExpr)> = Vec::with_capacity(entries.len());
                let mut spread: Option<Box<BoundExpr>> = None;
                for ent in entries {
                    match ent {
                        ast::ObjectEntry::Pair { key, value, .. } => {
                            let kexpr = match key {
                                ast::ObjectKey::Ident(id) => BoundExpr::String(id.name.clone()),
                                ast::ObjectKey::Str { value, .. } => {
                                    BoundExpr::String(value.clone())
                                }
                            };
                            let vexpr = self.bind_expr(value)?;
                            pairs.push((kexpr, vexpr));
                        }
                        ast::ObjectEntry::Spread { value, .. } => {
                            // Only one spread supported in this bound form; if multiple are allowed,
                            // convert to a list and adjust BoundExpr to carry a Vec
                            spread = Some(Box::new(self.bind_expr(value)?));
                        }
                    }
                }
                Ok(BoundExpr::ObjectLiteral(pairs, spread))
            }

            Expr::ObjectFor { seq, key, value, filter, .. } => {
                let bseq = self.bind_expr(seq)?;
                let bkey = self.bind_expr(key)?;
                let bvalue = self.bind_expr(value)?;
                let bfilter =
                    if let Some(f) = filter { Some(Box::new(self.bind_expr(f)?)) } else { None };
                Ok(BoundExpr::ObjectFor {
                    seq: Box::new(bseq),
                    key: Box::new(bkey),
                    value: Box::new(bvalue),
                    filter: bfilter,
                })
            }

            Expr::Group { expr, .. } => Ok(self.bind_expr(expr)?),

            Expr::FunctionRef { name, span } => {
                // Standalone reference appears (rare); resolve into a call-site only normally.
                // If it shows up as a value, reject here: not first-class. Report as unknown function
                Err(BindError::UnknownFunction {
                    name: name.clone(),
                    span: *span,
                    suggestions: self.env.fun_suggestions(&name),
                })
            }
        }
    }
}
