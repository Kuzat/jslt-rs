# JSLT Diagnostics

This document describes language-server diagnostic codes.

## jslt_parse

General parser/syntax diagnostic emitted for malformed JSLT source.

## jslt_import_not_found

An `import "..." as ...` target could not be resolved on disk.

## jslt_bind_unknown_function

A function name could not be resolved during binding.

## jslt_bind_unknown_variable

A variable is used but not declared by a `let` or a function parameter. Like Java JSLT, the
program still compiles: the value must be passed in when the program runs (for example
`CompiledProgram::apply_with_variables` or `jslt --var name=JSON`). This is a warning.

## jslt_bind_non_function_callee

A call expression attempted to invoke a non-function value.

## jslt_bind_duplicate_variable

A variable was declared more than once in the same list of `let` statements.

## jslt_bind_matcher_not_allowed

An object matcher (`* : expr`) was used inside an array, a function declaration, the value of a
computed key, or an object that has computed keys.

## jslt_bind_duplicate_key

An object literal sets the same static key more than once.

## jslt_module

General module-level error not otherwise classified.
