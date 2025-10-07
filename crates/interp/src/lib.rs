use crate::binder::{
    BindError, Binder, BoundExpr, BoundFunction, BoundProgram, CaptureSpec, FunctionId, ObjectKey,
    ResolvedVar,
};
use ast::{Program, Span};
use serde_json::{json, Map, Value};
use std::collections::HashMap;
use stdlib::Registry;
use thiserror::Error;
use value::JsltValue;

pub mod binder;

pub fn bind(program: &Program) -> Result<BoundProgram, BindError> {
    let mut b = Binder::new();
    b.bind_program(program)
}

#[derive(Debug, Clone)]
pub struct EvalConfig {
    // Max number of expression steps (nodes) to evaluate. None = unlimited
    pub max_steps: Option<u64>,
    // Max recursion depth (function calls). None = unlimited
    pub max_call_depth: Option<usize>,
}

impl Default for EvalConfig {
    fn default() -> Self {
        Self { max_steps: Some(1_000_000), max_call_depth: Some(1_000) }
    }
}

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error("type error at {span:?}: {msg}")]
    TypeError { msg: String, span: Span },

    #[error("division by zero at {span:?}:")]
    DivByZero { span: Span },

    #[error("function arity  mismatch for `{name}` at {span:?}: expected {expected}, got {got}")]
    ArityMismatch { name: String, expected: usize, got: usize, span: Span },

    #[error("unknown function id {0:?}")]
    UnknownFunction(FunctionId),

    #[error("evaluation budget exceeded (max steps = {0})")]
    BudgetExceeded(u64),

    #[error("recursion depth exceeded (max depth = {0})")]
    RecursionExceeded(usize),

    #[error("unexpected NaN/Inf at {span:?}:")]
    NonFiniteNumber { span: Span },

    #[error("internal error: {0}")]
    Internal(String),
}

#[derive(Debug, Clone)]
struct Closure {
    fun: BoundFunction,
    captures: Vec<JsltValue>,
    capture_map: HashMap<(usize, usize), usize>, // (depth, slot) -> index in captures
}

#[derive(Debug, Clone)]
struct Frame {
    locals: Vec<JsltValue>, // parameters and potential future locals
    this_val: JsltValue,    // current context '.'
    active_fun: Option<FunctionId>,
}

struct Evaluator<'p> {
    prog: &'p BoundProgram,
    closures: HashMap<FunctionId, Closure>,
    cfg: EvalConfig,
    steps: u64,
    call_depth: usize,
    stack: Vec<Frame>,
    registry: Registry,
    builtin_count: usize,
}

impl<'p> Evaluator<'p> {
    fn new(prog: &'p BoundProgram, cfg: EvalConfig) -> Self {
        // Initialize builtin registry once. ids 0..builtin_coutn must match binding seeding
        let registry = Registry::with_default();
        let builtin_count = prog.builtin_count;

        Self {
            prog,
            closures: HashMap::new(),
            cfg,
            steps: 0,
            call_depth: 0,
            stack: Vec::new(),
            registry,
            builtin_count,
        }
    }

    fn bump_steps(&mut self) -> Result<(), RuntimeError> {
        self.steps += 1;
        if let Some(max) = self.cfg.max_steps {
            if self.steps > max {
                return Err(RuntimeError::BudgetExceeded(max));
            }
        }
        Ok(())
    }

    fn push_frame(&mut self, frame: Frame) {
        self.stack.push(frame);
    }
    fn pop_frame(&mut self) {
        self.stack.pop();
    }

    fn current_frame(&self) -> &Frame {
        self.stack.last().expect("frame present")
    }
    fn current_frame_mut(&mut self) -> &mut Frame {
        self.stack.last_mut().expect("frame present")
    }

    fn with_call_depth<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<T, RuntimeError>,
    ) -> Result<T, RuntimeError> {
        self.call_depth += 1;
        if let Some(max) = self.cfg.max_call_depth {
            if self.call_depth > max {
                return Err(RuntimeError::RecursionExceeded(max));
            }
        }
        let r = f(self);
        self.call_depth -= 1;
        r
    }

    fn materialize_closures_from_top(&mut self) -> Result<(), RuntimeError> {
        // Build closure after top-level lets evaluated.
        let mut closures = HashMap::new();
        for f in &self.prog.functions {
            let mut captures = Vec::with_capacity(f.captures.len());
            let mut capture_map = HashMap::new();
            for (i, cap) in f.captures.iter().enumerate() {
                let val = self.lookup_captures_from_stack(*cap)?;
                capture_map.insert((cap.depth, cap.slot), i);
                captures.push(val);
            }
            closures.insert(f.id, Closure { fun: f.clone(), captures, capture_map });
        }
        self.closures = closures;
        Ok(())
    }

    fn lookup_captures_from_stack(&self, cap: CaptureSpec) -> Result<JsltValue, RuntimeError> {
        if self.stack.is_empty() {
            return Err(RuntimeError::Internal("no frames for capture resolution".into()));
        }
        // Depth 1 => top frame, 2 => previous frame, etc.
        let idx = self.stack.len().checked_sub(cap.depth).ok_or_else(|| {
            RuntimeError::Internal(format!("invalid capture depth {}", cap.depth))
        })?;
        let frame = &self.stack[idx];
        let slot = cap.slot;
        let v = frame
            .locals
            .get(slot)
            .cloned()
            .ok_or_else(|| RuntimeError::Internal(format!("invalid capture slot {}", slot)))?;
        Ok(v)
    }

    fn eval_program(mut self, input: &Value) -> Result<Value, RuntimeError> {
        self.bump_steps()?;

        // Top-Level frame with lets as locals; '.' is input
        let mut top = Frame {
            locals: Vec::new(),
            this_val: JsltValue::from_json(input.clone()),
            active_fun: None,
        };
        top.locals.resize(self.prog.lets.len(), JsltValue::null());
        self.push_frame(top);

        // Evaluate lets in order (binder assigned slots in the same order we pushed here
        for (i, (_name, expr)) in self.prog.lets.iter().enumerate() {
            let v = self.eval_expr(expr)?;
            self.current_frame_mut().locals[i] = v;
        }

        // Build closure after top-level lets evaluated.
        self.materialize_closures_from_top()?;

        // Evaluate body with '.' = input
        let result = self.eval_expr(&self.prog.body)?.into_json();

        self.pop_frame();
        Ok(result)
    }

    fn eval_expr(&mut self, e: &BoundExpr) -> Result<JsltValue, RuntimeError> {
        self.bump_steps()?;
        use BoundExpr::*;
        match e {
            Null(_) => Ok(JsltValue::null()),
            Bool(b, _) => Ok(JsltValue::from_json(Value::Bool(*b))),
            NumberFloat(n, sp) => {
                if !n.is_finite() {
                    return Err(RuntimeError::NonFiniteNumber { span: *sp });
                }
                Ok(JsltValue::number_f64(*n))
            }
            NumberInt(n, _) => {
                Ok(JsltValue::number_i64(*n))
            }
            String(s, _) => Ok(JsltValue::from_json(Value::String(s.clone()))),
            This(_) => Ok(self.current_frame().this_val.clone()),

            Var(ResolvedVar::This, _) => Ok(self.current_frame().this_val.clone()),
            Var(ResolvedVar::Local(slot), _) => {
                let f = self.current_frame();
                let v = f.locals.get(*slot).cloned().ok_or_else(|| {
                    RuntimeError::Internal(format!("invalid local slot {}", slot))
                })?;
                Ok(v)
            }
            Var(ResolvedVar::Captured { depth, slot }, _) => {
                // Resolve via active function's closure
                let fid = self.current_frame().active_fun.ok_or_else(|| {
                    RuntimeError::Internal("captured var without active function".into())
                })?;
                let clo = self.closures.get(&fid).ok_or(RuntimeError::UnknownFunction(fid))?;
                let idx = clo.capture_map.get(&(*depth, *slot)).copied().ok_or_else(|| {
                    RuntimeError::Internal(format!(
                        "missing caputre mapping for {:?}:{:?}",
                        *depth, *slot
                    ))
                })?;
                let val = clo.captures.get(idx).cloned().ok_or_else(|| {
                    RuntimeError::Internal(format!("capture index OOB for {:?}", (*depth, *slot)))
                })?;
                Ok(val)
            }

            If { cond, then_br, else_br, .. } => {
                let c = self.eval_expr(cond)?;
                if c.truthy() {
                    self.eval_expr(then_br)
                } else {
                    self.eval_expr(else_br)
                }
            }

            Not(inner, _) => {
                let v = self.eval_expr(inner)?;
                Ok(JsltValue::from_json(Value::Bool(!v.truthy())))
            }
            Neg(inner, sp) => {
                let v = self.eval_expr(inner)?;
                let n = v.as_f64_checked().ok_or_else(|| RuntimeError::TypeError {
                    msg: "unary '-' expects number".into(),
                    span: *sp,
                })?;
                let res = -n;
                if !res.is_finite() {
                    return Err(RuntimeError::NonFiniteNumber { span: *sp });
                }
                Ok(JsltValue::from_json(json!(res)))
            }

            Add(l, r, sp) => self.eval_add(l, r, *sp),
            Sub(l, r, sp) => self.eval_num_binop(l, r, *sp, |a, b| a - b),
            Mul(l, r, sp) => self.eval_num_binop(l, r, *sp, |a, b| a * b),
            Div(l, r, sp) => {
                let lv = self.eval_expr(l)?;
                let rv = self.eval_expr(r)?;
                let a = lv.as_f64_checked().ok_or_else(|| RuntimeError::TypeError {
                    msg: "division expects number".into(),
                    span: *sp,
                })?;
                let b = rv.as_f64_checked().ok_or_else(|| RuntimeError::TypeError {
                    msg: "division expects number".into(),
                    span: *sp,
                })?;
                if b == 0.0 {
                    return Err(RuntimeError::DivByZero { span: *sp });
                }
                let res = a / b;
                if !res.is_finite() {
                    return Err(RuntimeError::NonFiniteNumber { span: *sp });
                }
                Ok(JsltValue::from_json(json!(res)))
            }
            Mod(l, r, sp) => {
                let lv = self.eval_expr(l)?;
                let rv = self.eval_expr(r)?;
                let a = lv.as_f64_checked().ok_or_else(|| RuntimeError::TypeError {
                    msg: "modulo expects number".into(),
                    span: *sp,
                })?;
                let b = rv.as_f64_checked().ok_or_else(|| RuntimeError::TypeError {
                    msg: "modulo expects number".into(),
                    span: *sp,
                })?;
                if b == 0.0 {
                    return Err(RuntimeError::DivByZero { span: *sp });
                }
                let res = a % b;
                if !res.is_finite() {
                    return Err(RuntimeError::NonFiniteNumber { span: *sp });
                }
                Ok(JsltValue::from_json(json!(res)))
            }

            Eq(l, r, _) => {
                let lv = self.eval_expr(l)?;
                let rv = self.eval_expr(r)?;
                Ok(JsltValue::from_json(Value::Bool(lv.deep_eq(&rv))))
            }
            Ne(l, r, _) => {
                let lv = self.eval_expr(l)?;
                let rv = self.eval_expr(r)?;
                Ok(JsltValue::from_json(Value::Bool(!lv.deep_eq(&rv))))
            }
            Lt(l, r, sp) => self.eval_cmp(l, r, *sp, |o| o.is_lt()),
            Le(l, r, sp) => self.eval_cmp(l, r, *sp, |o| o.is_le()),
            Gt(l, r, sp) => self.eval_cmp(l, r, *sp, |o| o.is_gt()),
            Ge(l, r, sp) => self.eval_cmp(l, r, *sp, |o| o.is_ge()),

            And(l, r, _) => {
                let lv = self.eval_expr(l)?;
                if !lv.truthy() {
                    Ok(lv) // short-circuit
                } else {
                    self.eval_expr(r)
                }
            }
            Or(l, r, _) => {
                let lv = self.eval_expr(l)?;
                if lv.truthy() {
                    Ok(lv) // short-circuit
                } else {
                    self.eval_expr(r)
                }
            }

            ArrayLiteral(elems, _) => {
                let mut out = Vec::with_capacity(elems.len());
                for e in elems {
                    out.push(self.eval_expr(e)?.into_json());
                }
                Ok(JsltValue::from_json(Value::Array(out)))
            }

            ObjectLiteral(pairs, spread, sp) => {
                let mut obj = Map::new();
                if let Some(sv) = spread {
                    let spread_val = self.eval_expr(sv)?;
                    match spread_val.as_json() {
                        Value::Null => { /* no-op */ }
                        Value::Object(m) => {
                            for (k, v) in m {
                                obj.insert(k.clone(), v.clone());
                            }
                        }
                        _ => {
                            return Err(RuntimeError::TypeError {
                                msg: "object spread expects object or null".into(),
                                span: *sp,
                            });
                        }
                    }
                }
                for (kexpr, vexpr) in pairs {
                    let key = self.eval_expr(kexpr)?;
                    let s = match key.as_json() {
                        Value::String(s) => s.clone(),
                        _ => {
                            return Err(RuntimeError::TypeError {
                                msg: "object key must be a  string".into(),
                                span: kexpr.span(),
                            });
                        }
                    };
                    let val = self.eval_expr(vexpr)?;
                    obj.insert(s, val.into_json());
                }
                Ok(JsltValue::from_json(Value::Object(obj)))
            }

            ArrayFor { seq, elem, filter, .. } => {
                let seq_val = self.eval_expr(seq)?;
                let arr: Vec<Value> = match seq_val.as_json() {
                    Value::Array(a) => a.clone(),
                    _ => Vec::new(),
                };
                let mut out = Vec::with_capacity(arr.len());
                let caller_frame = self.current_frame().clone();
                for item in arr {
                    self.push_frame(Frame {
                        locals: caller_frame.locals.clone(),
                        this_val: JsltValue::from_json(item),
                        active_fun: caller_frame.active_fun,
                    });
                    let pass =
                        if let Some(f) = filter { self.eval_expr(f)?.truthy() } else { true };
                    if pass {
                        out.push(self.eval_expr(elem)?.into_json());
                    }
                    self.pop_frame();
                }
                Ok(JsltValue::from_json(Value::Array(out)))
            }

            ObjectFor { seq, key, value, filter, .. } => {
                let seq_val = self.eval_expr(seq)?;
                let arr: Vec<Value> = match seq_val.as_json() {
                    Value::Array(a) => a.clone(),
                    _ => Vec::new(),
                };
                let mut out = Map::new();
                let caller_frame = self.current_frame().clone();
                for item in arr {
                    self.push_frame(Frame {
                        locals: caller_frame.locals.clone(),
                        this_val: JsltValue::from_json(item),
                        active_fun: caller_frame.active_fun,
                    });
                    let pass =
                        if let Some(f) = filter { self.eval_expr(f)?.truthy() } else { true };
                    if pass {
                        let k = self.eval_expr(key)?;
                        let kstr = match k.as_json() {
                            Value::String(s) => s.clone(),
                            _ => {
                                return Err(RuntimeError::TypeError {
                                    msg: "object key must be a  string".into(),
                                    span: key.span(),
                                });
                            }
                        };
                        let v = self.eval_expr(value)?;
                        out.insert(kstr, v.into_json());
                    }
                    self.pop_frame();
                }
                Ok(JsltValue::from_json(Value::Object(out)))
            }

            Member(target, key, _) => {
                let tv = self.eval_expr(target)?;
                Ok(obj_get(&tv, key))
            }

            Index(target, index, _) => {
                let tv = self.eval_expr(target)?;
                match tv.as_json() {
                    Value::Array(_) | Value::String(_) => {
                        let iv = self.eval_expr(index)?;
                        if let Some(i) = iv.as_f64_checked() {
                            Ok(tv.index(i.trunc() as i64))
                        } else {
                            Ok(JsltValue::null())
                        }
                    }
                    _ => Ok(JsltValue::null()),
                }
            }

            Slice { target, start, end, .. } => {
                let tv = self.eval_expr(target)?;
                match tv.as_json() {
                    Value::Array(_) | Value::String(_) => {
                        let sidx = if let Some(s) = start {
                            let sv = self.eval_expr(s)?;
                            match sv.as_f64_checked() {
                                Some(f) => Some(f.trunc() as i64),
                                None => return Ok(JsltValue::null()),
                            }
                        } else {
                            None
                        };
                        let eidx = if let Some(e2) = end {
                            let ev = self.eval_expr(e2)?;
                            match ev.as_f64_checked() {
                                Some(f) => Some(f.trunc() as i64),
                                None => return Ok(JsltValue::null()),
                            }
                        } else {
                            None
                        };
                        Ok(tv.slice(sidx, eidx))
                    }
                    _ => Ok(JsltValue::null()),
                }
            }

            Call { id, args, span } => {
                // Evaluate args left to right once for either builtin or user functions
                let mut evaluated_args = Vec::with_capacity(args.len());
                for a in args {
                    evaluated_args.push(self.eval_expr(a)?);
                }

                // Dispatch to stdlib if id is within builtin range, else user function
                if id.0 < self.builtin_count {
                    // builtin function call
                    match self.registry.call_by_id(id.0, &evaluated_args) {
                        Ok(v) => Ok(v),
                        Err(err) => {
                            // Best-effort to include function name in message
                            let fname = self
                                .registry
                                .get_by_id(id.0)
                                .map(|f| f.name())
                                .unwrap_or("<builtin>");
                            Err(map_stdlib_error(err, fname, *span))
                        }
                    }
                } else {
                    // user defined function
                    let (fun_id, fun_body, expected_params) = {
                        let clo =
                            self.closures.get(id).ok_or(RuntimeError::UnknownFunction(*id))?;
                        (clo.fun.id, clo.fun.body.clone(), clo.fun.params.len())
                    };

                    if evaluated_args.len() != expected_params {
                        return Err(RuntimeError::ArityMismatch {
                            name: self
                                .closures
                                .get(id)
                                .map(|c| c.fun.name.clone())
                                .unwrap_or_else(|| "<function>".to_string()),
                            expected: expected_params,
                            got: evaluated_args.len(),
                            span: *span,
                        });
                    }
                    // New frame: '.' carries through from caller
                    let caller_this = self.current_frame().this_val.clone();
                    let new_frame = Frame {
                        locals: evaluated_args,
                        this_val: caller_this,
                        active_fun: Some(fun_id),
                    };
                    self.with_call_depth(|me| {
                        me.push_frame(new_frame);
                        let result = me.eval_expr(&fun_body)?;
                        me.pop_frame();
                        Ok(result)
                    })
                }
            }
        }
    }

    fn eval_add(
        &mut self,
        l: &BoundExpr,
        r: &BoundExpr,
        span: Span,
    ) -> Result<JsltValue, RuntimeError> {
        let lv = self.eval_expr(l)?;
        let rv = self.eval_expr(r)?;
        match (lv.as_json(), rv.as_json()) {
            (Value::Number(a), Value::Number(b)) => {
                let (af, bf) = (a.as_f64().unwrap(), b.as_f64().unwrap());
                let res = af + bf;
                if !res.is_finite() {
                    return Err(RuntimeError::NonFiniteNumber { span });
                }
                Ok(JsltValue::from_json(json!(res)))
            }
            (Value::String(a), Value::String(b)) => {
                let mut s = String::with_capacity(a.len() + b.len());
                s.push_str(a);
                s.push_str(b);
                Ok(JsltValue::from_json(Value::String(s)))
            }
            _ => Err(RuntimeError::TypeError {
                msg: "addition expects number+number or string+string".into(),
                span,
            }),
        }
    }

    fn eval_num_binop(
        &mut self,
        l: &BoundExpr,
        r: &BoundExpr,
        span: Span,
        op: impl FnOnce(f64, f64) -> f64,
    ) -> Result<JsltValue, RuntimeError> {
        let lv = self.eval_expr(l)?;
        let rv = self.eval_expr(r)?;
        let a = lv.as_f64_checked().ok_or_else(|| RuntimeError::TypeError {
            msg: "numeric operation expects number".into(),
            span,
        })?;
        let b = rv.as_f64_checked().ok_or_else(|| RuntimeError::TypeError {
            msg: "numeric operation expects number".into(),
            span,
        })?;
        let res = op(a, b);
        if !res.is_finite() {
            return Err(RuntimeError::NonFiniteNumber { span });
        }
        Ok(JsltValue::from_json(json!(res)))
    }

    fn eval_cmp(
        &mut self,
        l: &BoundExpr,
        r: &BoundExpr,
        span: Span,
        pred: impl Fn(std::cmp::Ordering) -> bool,
    ) -> Result<JsltValue, RuntimeError> {
        let lv = self.eval_expr(l)?;
        let rv = self.eval_expr(r)?;
        let res = match (lv.as_json(), rv.as_json()) {
            (Value::Number(a), Value::Number(b)) => {
                let (af, bf) = (a.as_f64().unwrap(), b.as_f64().unwrap());
                pred(af.partial_cmp(&bf).ok_or(RuntimeError::NonFiniteNumber { span })?)
            }
            (Value::String(a), Value::String(b)) => pred(a.cmp(b)),
            _ => {
                return Err(RuntimeError::TypeError {
                    msg: "comparison expects number vs number or string vs string".into(),
                    span,
                })
            }
        };
        Ok(JsltValue::from_json(Value::Bool(res)))
    }
}

// Member access with null propagation
fn obj_get(target: &JsltValue, key: &ObjectKey) -> JsltValue {
    let k = match key {
        ObjectKey::Ident(s, _) => s,
        ObjectKey::String(s, _) => s,
    };
    match target.as_json() {
        Value::Object(m) => {
            m.get(k).cloned().map(JsltValue::from_json).unwrap_or(JsltValue::null())
        }
        _ => JsltValue::null(),
    }
}

// Public entry point: evaluate a bound program against input JSON.
pub fn apply(
    bound: &BoundProgram,
    input: &Value,
    cfg: Option<EvalConfig>,
) -> Result<Value, RuntimeError> {
    let cfg = cfg.unwrap_or_default();
    let ev = Evaluator::new(bound, cfg);
    ev.eval_program(input)
}

// Convenience: bind + apply from AST program
pub fn eval(
    program: &Program,
    input: &Value,
    cfg: Option<EvalConfig>,
) -> Result<Value, Box<dyn std::error::Error>> {
    let bound = bind(program)?;
    Ok(apply(&bound, input, cfg)?)
}

fn map_stdlib_error(err: stdlib::StdlibError, fname: &str, span: Span) -> RuntimeError {
    match err {
        stdlib::StdlibError::Arity { expected, got } => RuntimeError::TypeError {
            msg: format!("{} expects {} arguments, got {}", fname, expected, got),
            span,
        },
        stdlib::StdlibError::Type(msg) => RuntimeError::TypeError { msg, span },
        stdlib::StdlibError::Semantic(msg) => {
            RuntimeError::Internal(format!("builtin `{}` failed: {}", fname, msg))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use binder::{
        BoundExpr as B, BoundFunction, CaptureSpec, FunctionId, ObjectKey as OK, ResolvedVar as RV,
    };
    use serde_json::json;

    fn sp() -> ast::Span {
        // Minimal span for tests
        ast::Span { start: 0, end: 0, line: 1, column: 1 }
    }

    // Helper to quickly build a BoundProgram with lets/body only.
    fn prog_with(lets: Vec<(&str, B)>, body: B, functions: Vec<BoundFunction>) -> BoundProgram {
        BoundProgram {
            functions,
            lets: lets.into_iter().map(|(n, e)| (n.to_string(), e)).collect(),
            body,
            builtin_count: 0,
        }
    }

    #[test]
    fn identity_dot_returns_input() {
        let p = prog_with(vec![], B::This(sp()), vec![]);
        let input = json!({"a": 1});
        let out = apply(&p, &input, None).unwrap();
        assert_eq!(out, input);
    }

    #[test]
    fn member_access_and_null_propagation() {
        // .a on an object
        let body = B::Member(Box::new(B::This(sp())), OK::Ident("a".into(), sp()), sp());
        let p = prog_with(vec![], body, vec![]);
        let input = json!({"a": 2, "b": 3});
        let out = apply(&p, &input, None).unwrap();
        assert_eq!(out, json!(2));

        // .a on non-object → null
        let input2 = json!(true);
        let out2 = apply(&p, &input2, None).unwrap();
        assert_eq!(out2, json!(null));

        // missing member → null
        let input3 = json!({"x": 1});
        let out3 = apply(&p, &input3, None).unwrap();
        assert_eq!(out3, json!(null));
    }

    #[test]
    fn index_and_slice_semantics() {
        // String indexing and slicing
        let body_idx = B::Index(Box::new(B::This(sp())), Box::new(B::NumberInt(1, sp())), sp());
        let p_idx = prog_with(vec![], body_idx, vec![]);
        let out_idx = apply(&p_idx, &json!("héllo"), None).unwrap();
        assert_eq!(out_idx, json!("é"));

        let body_slice = B::Slice {
            target: Box::new(B::This(sp())),
            start: Some(Box::new(B::NumberInt(1, sp()))),
            end: Some(Box::new(B::NumberInt(4, sp()))),
            span: sp(),
        };
        let p_slice = prog_with(vec![], body_slice, vec![]);
        let out_slice = apply(&p_slice, &json!("hello"), None).unwrap();
        assert_eq!(out_slice, json!("ell"));

        // Array indexing negative and OOB → null
        let body_idx_neg = B::Index(Box::new(B::This(sp())), Box::new(B::NumberInt(-1, sp())), sp());
        let p_idx_neg = prog_with(vec![], body_idx_neg, vec![]);
        let out_idx_neg = apply(&p_idx_neg, &json!([1, 2, 3]), None).unwrap();
        assert_eq!(out_idx_neg, json!(3));

        let body_idx_oob = B::Index(Box::new(B::This(sp())), Box::new(B::NumberInt(99, sp())), sp());
        let p_idx_oob = prog_with(vec![], body_idx_oob, vec![]);
        let out_idx_oob = apply(&p_idx_oob, &json!([1, 2, 3]), None).unwrap();
        assert_eq!(out_idx_oob, json!(null));
    }

    #[test]
    fn arithmetic_and_type_errors() {
        // 1 + 2
        let body_add = B::Add(Box::new(B::NumberInt(1, sp())), Box::new(B::NumberInt(2, sp())), sp());
        let p_add = prog_with(vec![], body_add, vec![]);
        let out_add = apply(&p_add, &json!(null), None).unwrap();
        assert_eq!(out_add, json!(3.0));

        // "a" + "b" -> "ab"
        let body_cat = B::Add(
            Box::new(B::String("a".into(), sp())),
            Box::new(B::String("b".into(), sp())),
            sp(),
        );
        let p_cat = prog_with(vec![], body_cat, vec![]);
        let out_cat = apply(&p_cat, &json!(null), None).unwrap();
        assert_eq!(out_cat, json!("ab"));

        // "a" + 1 -> type error
        let body_bad =
            B::Add(Box::new(B::String("a".into(), sp())), Box::new(B::NumberInt(1, sp())), sp());
        let p_bad = prog_with(vec![], body_bad, vec![]);
        let err = apply(&p_bad, &json!(null), None).unwrap_err();
        match err {
            RuntimeError::TypeError { .. } => {}
            _ => panic!("expected TypeError, got {err:?}"),
        }
    }

    #[test]
    fn comparisons_and_equality() {
        // Deep object equality order-insensitive
        // But to compare a vs b, use lets to bind both and compare vars
        // let l0 = a; let l1 = b; l0 == l1
        // Build body: Eq( Var(Local(0)), Var(Local(1)) )
        // First run with input = a, then overwrite second let with b via a second program
        // However evaluator binds lets from expressions; here both lets read '.' so both become input.
        // Instead, run two separate programs:
        // Program p_ab: lets ["l0"=. , "l1"=.], but we won't use this trick.
        // Simpler: directly compare with bound literals:
        // The above is awkward. Let's instead test equality through evaluator's deep_eq by constructing JsltValue via ObjectLiteral:
        let body_obj_left = B::ObjectLiteral(
            vec![
                (B::String("x".into(), sp()), B::NumberInt(1, sp())),
                (
                    B::String("y".into(), sp()),
                    B::ArrayLiteral(vec![B::Bool(true, sp()), B::Null(sp())], sp()),
                ),
            ],
            None,
            sp(),
        );
        let body_obj_right = B::ObjectLiteral(
            vec![
                (
                    B::String("y".into(), sp()),
                    B::ArrayLiteral(vec![B::Bool(true, sp()), B::Null(sp())], sp()),
                ),
                (B::String("x".into(), sp()), B::NumberInt(1, sp())),
            ],
            None,
            sp(),
        );
        let body_eq_objs = B::Eq(Box::new(body_obj_left), Box::new(body_obj_right), sp());
        let p_eq = prog_with(vec![], body_eq_objs, vec![]);
        let out = apply(&p_eq, &json!(null), None).unwrap();
        assert_eq!(out, json!(true));

        // Mixed-type comparison should error
        let p_cmp_err = prog_with(
            vec![],
            B::Lt(Box::new(B::String("a".into(), sp())), Box::new(B::NumberFloat(1.0, sp())), sp()),
            vec![],
        );
        let err = apply(&p_cmp_err, &json!(null), None).unwrap_err();
        match err {
            RuntimeError::TypeError { .. } => {}
            _ => panic!("expected TypeError, got {err:?}"),
        }
    }

    #[test]
    fn logical_short_circuiting() {
        // true or (1/0) must short-circuit and not fail
        let body_div_zero =
            B::Div(Box::new(B::NumberFloat(1.0, sp())), Box::new(B::NumberFloat(0.0, sp())), sp());
        let body_or = B::Or(Box::new(B::Bool(true, sp())), Box::new(body_div_zero), sp());
        let p_or = prog_with(vec![], body_or, vec![]);
        let out_or = apply(&p_or, &json!(null), None).unwrap();
        assert_eq!(out_or, json!(true));

        // false and (1/0) must short-circuit and not fail
        let body_and = B::And(
            Box::new(B::Bool(false, sp())),
            Box::new(B::Div(Box::new(B::NumberFloat(1.0, sp())), Box::new(B::NumberFloat(0.0, sp())), sp())),
            sp(),
        );
        let p_and = prog_with(vec![], body_and, vec![]);
        let out_and = apply(&p_and, &json!(null), None).unwrap();
        assert_eq!(out_and, json!(false));
    }

    #[test]
    fn truthiness_rules() {
        // if (.) 1 else 2 with different inputs
        let body = B::If {
            cond: Box::new(B::This(sp())),
            then_br: Box::new(B::NumberInt(1, sp())),
            else_br: Box::new(B::NumberInt(2, sp())),
            span: sp(),
        };
        let p = prog_with(vec![], body, vec![]);

        assert_eq!(apply(&p, &json!(null), None).unwrap(), json!(2)); // null -> falsey
        assert_eq!(apply(&p, &json!(false), None).unwrap(), json!(2)); // false -> falsey
        assert_eq!(apply(&p, &json!(true), None).unwrap(), json!(1)); // true -> truthy
        assert_eq!(apply(&p, &json!(0), None).unwrap(), json!(2)); // 0 -> falsey
        assert_eq!(apply(&p, &json!(2), None).unwrap(), json!(1)); // non-zero -> truthy
        assert_eq!(apply(&p, &json!(""), None).unwrap(), json!(2)); // empty string falsey
        assert_eq!(apply(&p, &json!("x"), None).unwrap(), json!(1)); // non-empty string truthy
        assert_eq!(apply(&p, &json!([]), None).unwrap(), json!(2)); // empty array falsey
        assert_eq!(apply(&p, &json!([0]), None).unwrap(), json!(1)); // non-empty array truthy
        assert_eq!(apply(&p, &json!({}), None).unwrap(), json!(2)); // empty object falsey
        assert_eq!(apply(&p, &json!({"a":1}), None).unwrap(), json!(1)); // non-empty object truthy
    }

    #[test]
    fn function_call_with_capture() {
        // let x = 2; def addx(y) x + y; addx(3) => 5
        // Top-level let slot 0 is 'x'
        let lets = vec![("x", B::NumberInt(2, sp()))];

        // Function body: Add(Var Captured(depth=1, slot=0), Var Local(0))  // x + y
        let fid = FunctionId(0);
        let fun = BoundFunction {
            id: fid,
            name: "addx".to_string(),
            params: vec!["y".into()],
            captures: vec![CaptureSpec { depth: 1, slot: 0 }],
            body: B::Add(
                Box::new(B::Var(RV::Captured { depth: 1, slot: 0 }, sp())),
                Box::new(B::Var(RV::Local(0), sp())),
                sp(),
            ),
        };

        // Call addx(3)
        let body = B::Call { id: fid, args: vec![B::NumberInt(3, sp())], span: sp() };
        let p = prog_with(lets, body, vec![fun]);

        let out = apply(&p, &json!(null), None).unwrap();
        assert_eq!(out, json!(5.0));
    }

    #[test]
    fn evaluation_budget_and_recursion_limit() {
        // Budget: set to very low value to trigger BudgetExceeded
        let p = prog_with(
            vec![],
            // Build a nested expression to exceed steps quickly: (((((((1+1)+1)+1)+1)+1)+1)
            B::Add(
                Box::new(B::Add(
                    Box::new(B::Add(
                        Box::new(B::NumberInt(1, sp())),
                        Box::new(B::NumberInt(1, sp())),
                        sp(),
                    )),
                    Box::new(B::Add(
                        Box::new(B::NumberInt(1, sp())),
                        Box::new(B::NumberInt(1, sp())),
                        sp(),
                    )),
                    sp(),
                )),
                Box::new(B::Add(
                    Box::new(B::Add(
                        Box::new(B::NumberInt(1, sp())),
                        Box::new(B::NumberInt(1, sp())),
                        sp(),
                    )),
                    Box::new(B::Add(
                        Box::new(B::NumberInt(1, sp())),
                        Box::new(B::NumberInt(2, sp())),
                        sp(),
                    )),
                    sp(),
                )),
                sp(),
            ),
            vec![],
        );
        let cfg = EvalConfig { max_steps: Some(3), max_call_depth: Some(100) };
        let err = apply(&p, &json!(null), Some(cfg)).unwrap_err();
        match err {
            RuntimeError::BudgetExceeded(_) => {}
            _ => panic!("expected BudgetExceeded, got {err:?}"),
        }

        // Recursion: function that calls itself unconditionally
        let fid = FunctionId(1);
        let self_call = B::Call { id: fid, args: vec![], span: sp() };
        let fun = BoundFunction {
            id: fid,
            name: "recur".into(),
            params: vec![],
            captures: vec![],
            body: self_call,
        };
        let body = B::Call { id: fid, args: vec![], span: sp() };
        let p2 = prog_with(vec![], body, vec![fun]);

        let cfg2 = EvalConfig { max_steps: Some(10_000), max_call_depth: Some(8) };
        let err2 = apply(&p2, &json!(null), Some(cfg2)).unwrap_err();
        match err2 {
            RuntimeError::RecursionExceeded(_) => {}
            _ => panic!("expected RecursionExceeded, got {err2:?}"),
        }
    }
}
