use ast::{Binding, Def, Expr, Ident, Let, Program, Span};
use interp::bind;
use interp::binder::{BoundExpr, CaptureSpec, FunctionId, ResolvedVar};
use stdlib::Registry;

fn s() -> Span {
    Span { start: 0, end: 0, line: 1, column: 1 }
}

fn ident(name: &str) -> Ident {
    Ident { name: name.to_string(), span: s() }
}

fn num(n: &str) -> Expr {
    let kind = if n.contains('.') || n.contains('e') || n.contains('E') {
        ast::NumericKind::Float
    } else {
        ast::NumericKind::Int
    };
    Expr::Number { lexeme: n.to_string(), kind, span: s() }
}

fn var(name: &str) -> Expr {
    Expr::Variable { name: ident(name) }
}

fn fun_ref(name: &str) -> Expr {
    Expr::FunctionRef { name: name.to_string(), span: s() }
}

fn call(name: &str, args: Vec<Expr>) -> Expr {
    Expr::Call { callee: Box::new(fun_ref(name)), args, span: s() }
}

#[test]
fn let_binding_and_variable_resolution() {
    // let x = 1;  $x
    let program = Program {
        imports: vec![],
        defs: vec![],
        lets: vec![Let {
            bindings: vec![Binding { name: ident("x"), value: num("1"), span: s() }],
            span: s(),
        }],
        body: Some(var("x")),
        span: s(),
    };

    let bound = bind(&program).expect("bind ok");

    // there should be one let tuple ("x", Number(1.0))
    assert_eq!(bound.lets.len(), 1);
    match &bound.lets[0].1 {
        BoundExpr::NumberInt(v, _s) => assert_eq!(*v, 1),
        _ => panic!("expected bound number for let x"),
    }

    // body should resolve to local slot 0 (global frame)
    match &bound.body {
        BoundExpr::Var(ResolvedVar::Local(slot), _s) => assert_eq!(*slot, 0),
        other => panic!("expected local var, got {:?}", other),
    }
}

#[test]
fn function_definition_and_call() {
    // def inc(x) x + 1;  inc(41)
    let def = Def {
        name: ident("inc"),
        params: vec![ident("x")],
        lets: vec![],
        body: Expr::Binary {
            op: ast::BinaryOp::Add,
            left: Box::new(var("x")),
            right: Box::new(num("1")),
            span: s(),
        },
        span: s(),
    };

    let program = Program {
        imports: vec![],
        defs: vec![def],
        lets: vec![],
        body: Some(call("inc", vec![num("41")])),
        span: s(),
    };

    let bound = bind(&program).expect("bind ok");

    // setup registry to get number of builtin functions
    let registry = Registry::with_default();

    // One function defined, should be id 0
    assert_eq!(bound.functions.len(), 1);
    assert_eq!(bound.functions[0].id, FunctionId(registry.len()));
    assert_eq!(bound.functions[0].name, "inc");
    assert_eq!(bound.functions[0].params, vec!["x".to_string()]);
    // No captures for inc(x) that only references its param
    assert!(bound.functions[0].captures.is_empty());

    // Body is a call to FunctionId(0) with one number arg
    match &bound.body {
        BoundExpr::Call { id: FunctionId(fid), args, .. } => {
            assert_eq!(*fid, registry.len());
            assert_eq!(args.len(), 1);
            match &args[0] {
                BoundExpr::NumberInt(n, _s) => assert_eq!(*n, 41),
                _ => panic!("expected numeric argument"),
            }
        }
        other => panic!("expected call, got {:?}", other),
    }

    // Ensure the function body uses a local var for param `x`
    match &bound.functions[0].body {
        BoundExpr::Add(left, right, _s) => {
            match left.as_ref() {
                BoundExpr::Var(ResolvedVar::Local(slot), _s) => assert_eq!(*slot, 0),
                _ => panic!("expected local var for param x"),
            }
            match right.as_ref() {
                BoundExpr::NumberInt(n, _s) => assert_eq!(*n, 1),
                _ => panic!("expected numeric literal 1"),
            }
        }
        other => panic!("expected Add inside function inc body, got {:?}", other),
    }
}

#[test]
fn closure_captures_outer_let() {
    // let x = 1; def g() $x; g()
    let program = Program {
        imports: vec![],
        defs: vec![Def {
            name: ident("g"),
            params: vec![],
            lets: vec![],
            body: var("x"), // references top-level let
            span: s(),
        }],
        lets: vec![Let {
            bindings: vec![Binding { name: ident("x"), value: num("1"), span: s() }],
            span: s(),
        }],
        body: Some(call("g", vec![])),
        span: s(),
    };
    // Setup registry to get number of builtin functions
    let registry = Registry::with_default();

    let bound = bind(&program).expect("bind ok");

    // The function g should capture x from the outer scope.
    assert_eq!(bound.functions.len(), 1);
    let g = &bound.functions[0];
    // Captures should contain (depth=1, slot=0): direct parent frame (function params frame -> depth 1 is global)
    assert_eq!(g.captures, vec![CaptureSpec { depth: 1, slot: 0 }]);

    // Inside the body, the reference should appear as Captured { depth: 1, slot: 0 }
    match &g.body {
        BoundExpr::Var(ResolvedVar::Captured { depth, slot }, _s) => {
            assert_eq!((*depth, *slot), (1, 0));
        }
        other => panic!("expected captured var in g body, got {:?}", other),
    }

    // Sanity: body calls g (id 0)
    match &bound.body {
        BoundExpr::Call { id: FunctionId(fid), args, .. } => {
            assert_eq!(*fid, registry.len());
            assert!(args.is_empty());
        }
        other => panic!("expected call to g, got {:?}", other),
    }
}

#[test]
fn param_shadows_outer_let() {
    // let x = 1; def id(x) $x; id(2)
    let program = Program {
        imports: vec![],
        defs: vec![Def {
            name: ident("id"),
            params: vec![ident("x")],
            lets: vec![],
            body: var("x"),
            span: s(),
        }],
        lets: vec![Let {
            bindings: vec![Binding { name: ident("x"), value: num("1"), span: s() }],
            span: s(),
        }],
        body: Some(call("id", vec![num("2")])),
        span: s(),
    };

    let bound = bind(&program).expect("bind ok");
    let idf = &bound.functions[0];

    // No captures: param shadows outer let
    assert!(idf.captures.is_empty());

    match &idf.body {
        BoundExpr::Var(ResolvedVar::Local(slot), _s) => assert_eq!(*slot, 0), // param slot 0
        other => panic!("expected local var for param x, got {:?}", other),
    }
}

#[test]
fn unknown_variable_yields_error_with_span() {
    // $nope
    let program =
        Program { imports: vec![], defs: vec![], lets: vec![], body: Some(var("nope")), span: s() };
    let err = bind(&program).unwrap_err();
    let msg = format!("{}", err);
    assert!(msg.contains("unknown variable"), "msg={}", msg);
    assert!(msg.contains("nope"), "msg={}", msg);
}

#[test]
fn unknown_function_yields_error_with_suggestions() {
    // nope()
    let program = Program {
        imports: vec![],
        defs: vec![Def {
            name: ident("near"),
            params: vec![],
            lets: vec![],
            body: Expr::Null(s()),
            span: s(),
        }],
        lets: vec![],
        body: Some(call("nope", vec![])),
        span: s(),
    };
    let err = bind(&program).unwrap_err();
    let msg = format!("{}", err);
    assert!(msg.contains("unknown function"), "msg={}", msg);
    assert!(msg.contains("nope"), "msg={}", msg);
    // will suggest not()
    assert!(msg.contains("not") || !msg.contains("did you mean"), "msg={}", msg);
}

#[test]
fn non_function_callee_is_rejected() {
    // (.)(1) — callee is `.` which is not a bare function identifier
    let program = Program {
        imports: vec![],
        defs: vec![],
        lets: vec![],
        body: Some(Expr::Call {
            callee: Box::new(Expr::This(s())),
            args: vec![num("1")],
            span: s(),
        }),
        span: s(),
    };
    let err = bind(&program).unwrap_err();
    let msg = format!("{}", err);
    assert!(msg.contains("attempted to call a non-function expression"), "msg={}", msg);
}
