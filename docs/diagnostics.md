# JSLT Diagnostics

This document describes language-server diagnostic codes.

## jslt_parse

General parser/syntax diagnostic emitted for malformed JSLT source.

## jslt_import_not_found

An `import "..." as ...` target could not be resolved on disk.

## jslt_bind_unknown_function

A function name could not be resolved during binding.

## jslt_bind_unknown_variable

A variable name could not be resolved during binding.

## jslt_bind_non_function_callee

A call expression attempted to invoke a non-function value.

## jslt_module

General module-level error not otherwise classified.
