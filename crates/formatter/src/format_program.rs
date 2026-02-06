//! Top-level program formatting

use crate::format_expr::format_expr;
use crate::format_stmt::{format_def, format_import, format_let};
use crate::format_trivia::format_leading_trivia;
use crate::writer::Writer;
use ast::Program;

/// Format a complete JSLT program
pub fn format_program(writer: &mut Writer, program: &Program) {
    // Format file header comments
    if let Some(ref trivia) = program.trivia {
        format_leading_trivia(writer, trivia);
    }

    // Format imports (one per line)
    for import in &program.imports {
        format_import(writer, import);
        writer.newline();
    }

    // Add blank line after imports if there are any and there are other statements
    if !program.imports.is_empty()
        && (!program.defs.is_empty() || !program.lets.is_empty() || program.body.is_some())
    {
        writer.newline();
    }

    // Format defs (one per line, with blank line between)
    for (i, def) in program.defs.iter().enumerate() {
        if i > 0 {
            writer.newline();
        }
        format_def(writer, def);
        writer.newline();
    }

    // Add blank line after defs if there are any and there are lets or body
    if !program.defs.is_empty() && (!program.lets.is_empty() || program.body.is_some()) {
        writer.newline();
    }

    // Format let statements
    for let_stmt in &program.lets {
        format_let(writer, let_stmt);
        writer.newline();
    }

    // Add blank line after lets if there are any and there is a body
    if !program.lets.is_empty() && program.body.is_some() {
        writer.newline();
    }

    // Format body expression
    if let Some(body) = &program.body {
        format_expr(writer, body);
    }
}
