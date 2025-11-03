# JSLT‑Rust Compatibility Spec v0.1 (Working Draft)

> Goal: a tiny, dependency‑light Rust implementation of the JSLT language that is source‑compatible with the established JSLT syntax and behavior where documented, while making ambiguous areas explicit. This document doubles as the public spec for users and the contract for implementors.

**Status:** Draft for iteration. Sections marked **OPEN** call out items to confirm against the Java reference via differential tests.

---

## 1. Non‑Goals & Scope

* Provide a **precise grammar** (EBNF) and **evaluation semantics** for expressions, declarations (`def`, `let`), and comprehensions.
* Define a minimal **data model** and **operator semantics** sufficient for a fully conforming interpreter.
* Specify deterministic behavior and error reporting.
* Leave the standard library catalog to a separate doc; include only the **dispatch contract** and core categories.

**Non‑goals:** Streaming/DOM trade‑offs, optimizer details, and full stdlib coverage; those live in implementation notes.

---

## 2. Data Model

JSLT operates on a JSON‑like value lattice:

* **null** — the absence of value.
* **boolean** — `true` / `false`.
* **number** — IEEE‑754 double (no NaN/Infinity in user space; see Errors).
* **string** — immutable, UTF‑8 text; indices count **Unicode scalar values** (code points), not bytes.
* **array** — ordered list of values.
* **object** — mapping from **string keys** to values. Duplicate keys in literals: **last wins**.

> **OPEN:** Confirm deep‑equality nuances for numbers (e.g., `-0` vs `0`), and whether the reference ever exposes NaN/Infinity. Proposed: disallow NaN/Inf; any operation yielding them is a runtime error.

---

## 3. Lexical Structure

### 3.1 Source text & encoding

* Source files are UTF‑8.

### 3.2 Whitespace & comments

* `whitespace := space | tab | CR | LF`.
* Line comments: `//` to end‑of‑line.
* Whitespace/comments may appear between any two tokens unless stated.

### 3.3 Identifiers

* `ident-start := ASCII letter | _`
* `ident-continue := ASCII letter | digit | _ | -`
* Identifiers are case‑sensitive.
* **Reserved words:** `def`, `let`, `if`, `else`, `for`, `and`, `or`, `not`, `null`, `true`, `false`.

### 3.4 Literals

* **Numbers** support decimal and exponent forms: `0`, `12`, `3.14`, `.5`, `1.`, `1e6`, `2.5E-3`.

  * A leading `.` followed by a digit is not valid a number literal as reflecting the orignal JSLT;
* **Strings** use double quotes `"…"` and JSON escapes: `\" \\ \/ \b \f \n \r \t \uXXXX`.

  * Invalid escapes and unpaired surrogates are errors.

---

## 4. Grammar (EBNF)

The grammar below is **authoritative**. Formatting and examples later clarify intent. Ambiguities are resolved here in favor of unambiguous parsing.

```ebnf
# ---------- Top level ----------
file         = ws,
               [ import_block, nl+ ],
               [ stmt_block ],
               [ nl+, expr ],
               ws, eof ;
# - If expr is present ⇒ "program" (runnable).
# - If expr is absent  ⇒ "module" (namespace-only).

import_block = import_stmt, { nl+, import_stmt } ;
import_stmt  = "import", ws1, string, ws1, "as", ws1, ident ;

stmt_block   = stmt, { nl+, stmt } ;
stmt         = def_expr | let_stmt ;

# ---------- Definitions & lets ----------
def_expr     = "def", ws1, ident, ws, "(", [ params ], ")", ws, def_body, ws ;
params       = ident, { ws, ",", ws, ident } ;

# 0+ let statements (single-binding each), then a newline, then the body expr
def_body     = [ let_block, nl+ ], expr ;
let_block    = let_stmt, { nl+, let_stmt } ;

# SINGLE binding only
let_stmt     = "let", ws1, ident, ws, "=", ws, expr ;

# ---------- Expressions ----------
expr         = if_expr | or_expr ;

if_expr      = "if", ws1, "(", expr, ")", ws, expr, ws1, "else", ws1, expr ;

or_expr      = and_expr, { ws1, "or", ws1, and_expr } ;
and_expr     = cmp_expr, { ws1, "and", ws1, cmp_expr } ;
cmp_expr     = add_expr, { ws, ("==" | "!=" | "<" | "<=" | ">" | ">="), ws, add_expr } ;
add_expr     = mul_expr, { ws, ("+" | "-"), ws, mul_expr } ;
mul_expr     = unary,    { ws, ("*" | "/" | "%"), ws, unary } ;
unary        = [ ("not" | "-"), ws1 ], postfix ;

postfix      = primary, { member | index_or_slice | call } ;
member       = ws, ".", ws, ( ident | string ) ;
index_or_slice
             = ws, "[", ws, [ expr ], ws, [ ":", ws, [ expr ] ], ws, "]" ;
call         = ws, "(", ws, [ args ], ws, ")" ;
args         = expr, { ws, ",", ws, expr } ;

# Allow:
# - named calls:         add(1,1)
# - namespace functions: ns:add(1,1)
# - module-as-function:  ns("test")
primary      = literal
             | variable
             | this
             | name_ref
             | ns_qualified
             | array
             | object
             | "(", ws, expr, ws, ")" ;

name_ref     = ident ;
ns_qualified = ident, ws, ":", ws, ident ;

literal      = "null" | "true" | "false" | number | string ;
this         = "." ;
variable     = "$", ident ;

array        = "[", ws,
                 ( "for", ws, "(", expr, ")", ws, expr, [ ws1, "if", ws1, expr ]
                 | [ expr, { ws, ",", ws, expr } ] ),
               ws, "]" ;

object       = "{", ws,
                 ( "for", ws, "(", expr, ")", ws, expr, ws, ":", ws, expr, [ ws1, "if", ws1, expr ]
                 | [ object_entry, { ws, ",", ws, object_entry } ] ),
               ws, "}" ;

object_entry = ( object_key, ws, ":", ws, expr ) | ( "*", ws, ":", ws, expr ) ;
object_key   = string | ident ;

ident        = ident_start, { ident_continue } ;
ident_start  = letter | "_" ;
ident_continue = letter | "_" | digit | "-" ;

number       = ( "0" | nonzero, { digit } ), [ ".", digit, { digit } ], [ exponent ]
             | ".", digit, { digit }, [ exponent ] ;
exponent     = ("e" | "E"), [ "+" | "-" ], digit, { digit } ;
string       = '"', { string_char }, '"' ;

# ---------- Whitespace & newlines ----------
space       = " " | "\t" | "\r" ;
ws          = { space } ;
ws1         = space, ws ;

eol_comment = "//", { not_newline }, "\n" ;
linebreak   = "\n" | eol_comment ;
nl          = ws, linebreak, ws ;
nl+         = nl, { nl } ;

eof         = /* end of input */ ;
```

**Notes**

* `program` permits multiple `def`/`let` declarations prior to the final expression; this matches common template usage.
* `let` uses **semicolon separators** to avoid ambiguity. Newline‑as‑separator can be added as an extension (see §11).
* `member` allows quoted keys for cases like `.actor."spt:userId"`.
* Comprehensions accept an optional trailing filter: `if <expr>`.

---

## 5. Operator Semantics

### 5.1 Precedence & associativity

From highest to lowest (all left‑associative unless stated):

1. Member/index/call: `.` `[...]` `(...)`
2. Unary: `-` (negation), `not` (logical not)
3. Multiplicative: `*` `/` `%`
4. Additive: `+` `-`
5. Comparisons: `<` `<=` `>` `>=` `==` `!=`
6. Logical: `and` `or`
7. Conditional: `if (...) … else …` (ternary‑like expression)

### 5.2 Types & coercion

* Operators are **type‑direct**. No implicit string↔number coercion except where stated.
* `+`

  * number + number → numeric sum
  * string + string → concatenation
  * any other combination → **Type error**
* `-`, `*`, `/`, `%` → numbers only.
* Comparisons

  * number vs number → numeric compare
  * string vs string → lexicographic (Unicode code point order)
  * boolean vs boolean → `==`/`!=` only
  * mixed types → **Type error** (except equality against `null`, see below)
* Equality `==`/`!=`

  * Same‑type values use deep equality (arrays/objects compare recursively; object order irrelevant).
  * Comparing with `null`: `x == null` is `true` iff `x` is `null`.

> **OPEN:** If the Java reference performs cross‑type coercions in any of these operators, mirror that. Until confirmed, the Rust spec stays strict to reduce surprise.

### 5.3 Truthiness

Used by `if`, `and`, `or` and filters:

* `false` and `null` → falsey
* `true` → truthy
* numbers → falsey iff exactly `0`
* strings → falsey iff empty
* arrays/objects → truthy iff non‑empty

> **OPEN:** Verify string/collection truthiness in the reference. Adjust if it treats only booleans as booleans.

### 5.4 Short‑circuiting

* `a and b` evaluates `b` only if `a` is truthy.
* `a or b` evaluates `b` only if `a` is falsey.

### 5.5 Null propagation

* Member access `.k` on `null` or non‑object → `null`.
* Indexing `[i]` on `null` or non‑array/non‑string → `null`.
* Slicing `[start:end]` on `null` or non‑array/non‑string → `null`.
* Function calls **do not** null‑propagate: calling a non‑function is a runtime error.

> **OPEN:** Confirm these propagation rules; some dialects throw on non‑object member access.

---

## 6. Context value and variables

* `.` evaluates to the **current context value**.
* The program’s initial context is the input JSON document provided to the evaluator.
* **Variables** are referenced as `$name` and introduced only via `let` bindings or function parameters.
* **Functions** are referenced by bare identifiers and defined via `def`.

### 6.1 Scopes

* `let` introduces **lexical** bindings visible in its body and nested expressions.
* Function parameters shadow outer variables.
* `def` defined at top level introduces a function binding visible **after** its declaration; inner `def` is allowed but discouraged (implementation may hoist or keep block‑local).

> **OPEN:** Confirm whether inner `def` is legal in the reference. Safe default: allow and scope to declaration region.

---

## 7. Collections: indexing, slicing, comprehensions

### 7.1 Indexing

* Arrays: zero‑based integer index. Negative indices count from the end (`-1` last element).
* Strings: index by code point. Negative indices allowed.
* Out‑of‑range → `null`.

### 7.2 Slicing `[start:end]`

* Applies to arrays and strings.
* `start` and `end` are optional; missing `start`=0, missing `end`=length.
* Negative indices offset from end.
* Result uses **half‑open** range `[start, end)`.

### 7.3 Comprehensions

* **Array:** `[for (seq) expr]` optionally `[for (seq) expr if cond]`

  * `seq` is any expression evaluating to an array; each element becomes the new context.
  * The body `expr` evaluates per element; `if cond` filters by truthiness.
* **Object:** `{for (seq) key : value}` optionally `{for (seq) key : value if cond}`

  * `key` must evaluate to a string; `value` yields the mapped value.
  * Duplicate keys: last assignment wins.

> **OPEN:** Confirm whether `seq` may be object (iterate values) or must be array. Proposed: arrays only; add `values(obj)` as stdlib to iterate objects.

---

## 8. Functions

### 8.1 Definition

* `def name(param1, param2, ...) expr`

  * Defines a pure function. The expression is the return value.
  * Functions close over the lexical environment at definition time.

### 8.2 Call semantics

* Arguments are evaluated left‑to‑right.
* Arity must match the definition (**OPEN:** whether varargs exist in reference stdlib).
* Passing `null` is allowed; function bodies decide how to treat it.

### 8.3 Standard Library (dispatch contract)

Rust implementation exposes built‑ins via a registry:

* **Name:** ASCII identifier, case‑sensitive.
* **Arity:** fixed or range (e.g., `split(s, sep)`; `join(arr, sep?)`).
* **Purity:** no side effects; deterministic.
* **Categories (non‑exhaustive):** string ops, number ops, boolean, array/object utils, regex, time/formatting.

> Full stdlib signatures will be maintained in `stdlib.md` and versioned alongside this spec.

---

## 9. Errors & Diagnostics

### 9.1 Parse errors

* Report the **first** syntax error with a span (`start`, `end`, `line`, `col`) and a message.

### 9.2 Runtime errors

* Type mismatch (e.g., string `+` number).
* Unknown variable/function.
* Function arity mismatch.
* Division by zero.
* Invalid string escape / malformed Unicode.
* Numeric overflow / NaN / Infinity from an operation.

Each error carries a span if originating from source; otherwise, from the nearest enclosing expression.

### 9.3 Evaluation limits

* Implementations **should** support an evaluation budget (max steps) and recursion depth guard; exceeding limits yields a runtime error.

---

## 10. Determinism & Side Effects

* Evaluation is **pure** and deterministic given the same input and stdlib. Any nondeterministic built‑ins (e.g., `random()`) must be opt‑in and documented.

---

## 11. Extensions (non‑normative, off by default)

Implementations may offer these as **feature flags** without breaking core compatibility:

* Newline‑as‑separator in `let` bindings.
* Trailing commas in arrays/objects/args.
* Hex/Binary integer literal sugar (still stored as number).
* Additional string literal forms (raw strings).
* Object spread shorthand beyond `* : expr` (e.g., `...expr`).

---

## 12. Examples (normative)

### 12.1 Minimal template

```
let user = .user;
let id = $user.id;
$ user.name
```

### 12.2 Member with quoted key

```
.actor."spt:userId"
```

### 12.3 Array comprehension with filter

```
[ for (.items) .price if .inStock ]
```

### 12.4 Object comprehension from array

```
{ for (.users) .id : .name }
```

### 12.5 Slicing and null propagation

```
.items[1:4]
.missing.field.name   // => null
"hello"[1:4]         // => "ell"
```

### 12.6 Functions and `def`

```
def upcase(s) string(upper(s))
[ for (.names) upcase(.) ]
```

> **All examples are normative** with respect to parsing; evaluation outcomes depend on stdlib definitions (`upper`, `string` here).

---

## 13. Conformance & Testing

* **Golden files:** Each release of this spec ships a corpus of `input.json`, `program.jslt`, and `expected.json` fixtures.
* **Differential tests:** Implementations should compare against the Java reference on the corpus; any divergence must be documented.
* **Fuzzing:** Parser fuzzing and property tests on evaluator (e.g., slice/idempotence identities) are recommended.

---

## 14. Versioning & Compatibility

* This spec is versioned `MAJOR.MINOR`. MINOR changes clarify behavior or add optional extensions; MAJOR changes break semantics or grammar.
* The Rust implementation must expose its supported spec version at runtime.

---

## 15. Appendix A — Token set (informative)

* Punctuators: `.` `,` `:` `;` `(` `)` `[` `]` `{` `}` `*`
* Operators: `+` `-` `*` `/` `%` `==` `!=` `<` `<=` `>` `>=`
* Keywords: `def` `let` `if` `else` `for` `and` `or` `not` `null` `true` `false`
* Literals: number, string
* Identifiers, Variables (`$ident`)

---

## 16. Appendix B — Open Questions (to settle via reference tests)

1. Exact **truthiness** rules for strings/arrays/objects.
2. **Null propagation** on member/index for non‑matching types.
3. Whether `def` is permitted **inside** expressions and its scope/hoisting rules.
4. Semantics of **negative indices** and out‑of‑range handling.
5. Any **cross‑type comparisons** implicitly allowed by the reference.
6. Comprehension `seq` over **objects** (values? entries?) vs arrays only.
7. Handling of **duplicate object literal keys** (last wins vs error) and object equality order.

---

## 17. Appendix C — Implementation Notes (Rust) (non‑normative)

* Use a hand‑rolled lexer; treat `.` followed by digit as NUMBER, else as THIS.
* Pratt parser mirrors the precedence table; keep spans on every AST node for LSP.
* Value layer can start with `serde_json::Value` behind a facade to permit zero‑copy later.
* Expose a small C ABI: `jslt_compile`, `jslt_eval`, `jslt_free`, `jslt_last_error`.
* LSP hooks (tower‑lsp): publish diagnostics with spans; offer completion of in‑scope idents/built‑ins.

---

*End of v0.1 draft. Iterate freely; we’ll lock semantics after differential testing against the Java reference.*

