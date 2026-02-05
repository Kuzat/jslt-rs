//! Statement formatting (imports, defs, lets)

use crate::format_expr::format_expr;
use crate::writer::Writer;
use ast::{Binding, Def, Import, Let};

/// Format an import statement
pub fn format_import(writer: &mut Writer, import: &Import) {
    writer.write("import \"");
    writer.write(&import.path);
    writer.write("\" as ");
    writer.write(&import.alias);
}

/// Format a function definition
pub fn format_def(writer: &mut Writer, def: &Def) {
    writer.write("def ");
    writer.write(&def.name.name);
    writer.write("(");

    for (i, param) in def.params.iter().enumerate() {
        if i > 0 {
            writer.write(", ");
        }
        writer.write(&param.name);
    }

    writer.write(")");

    // If there are let statements in the def, format them on new lines
    if !def.lets.is_empty() {
        writer.newline();
        writer.increase_indent();
        for let_stmt in &def.lets {
            format_let(writer, let_stmt);
            writer.newline();
        }
        writer.decrease_indent();
    } else {
        writer.write(" ");
    }

    format_expr(writer, &def.body);
}

/// Format a let statement
pub fn format_let(writer: &mut Writer, let_stmt: &Let) {
    writer.write("let ");

    for (i, binding) in let_stmt.bindings.iter().enumerate() {
        if i > 0 {
            writer.write(", ");
        }
        format_binding(writer, binding);
    }
}

/// Format a single binding
fn format_binding(writer: &mut Writer, binding: &Binding) {
    writer.write(&binding.name.name);
    writer.write(" = ");
    format_expr(writer, &binding.value);
}
