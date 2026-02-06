//! Expression formatting

use crate::format_stmt::format_let;
use crate::format_trivia::format_leading_trivia;
use crate::writer::Writer;
use ast::{BinaryOp, Expr, MemberKey, ObjectEntry, ObjectKey, UnaryOp};

/// Format an expression
pub fn format_expr(writer: &mut Writer, expr: &Expr) {
    match expr {
        Expr::Null(_) => writer.write("null"),
        Expr::Bool { value, .. } => writer.write(&value.to_string()),
        Expr::Number { lexeme, .. } => writer.write(lexeme),
        Expr::String { value, .. } => format_string(writer, value),
        Expr::This(_) => writer.write("."),
        Expr::Variable { name } => {
            writer.write("$");
            writer.write(&name.name);
        }

        Expr::If { cond, then_br, else_br, .. } => {
            writer.write("if (");
            format_expr(writer, cond);
            writer.write(")");
            writer.newline();
            writer.increase_indent();
            format_expr(writer, then_br);
            writer.decrease_indent();
            writer.newline();
            writer.write("else");
            writer.newline();
            writer.increase_indent();
            format_expr(writer, else_br);
            writer.decrease_indent();
        }

        Expr::Unary { op, expr, .. } => {
            match op {
                UnaryOp::Not => writer.write("not "),
                UnaryOp::Neg => writer.write("-"),
            }
            format_expr(writer, expr);
        }

        Expr::Binary { op, left, right, .. } => {
            format_expr(writer, left);
            writer.write(" ");
            writer.write(binary_op_str(*op));
            writer.write(" ");
            format_expr(writer, right);
        }

        Expr::Member { target, key, .. } => {
            // Special case: member access on this (.foo) should not double the dot
            if !matches!(target.as_ref(), Expr::This(_)) {
                format_expr(writer, target);
            }
            writer.write(".");
            match key {
                MemberKey::Ident(id) => writer.write(&id.name),
                MemberKey::Str { value, .. } => format_string(writer, value),
            }
        }

        Expr::Index { target, index, .. } => {
            format_expr(writer, target);
            writer.write("[");
            format_expr(writer, index);
            writer.write("]");
        }

        Expr::Slice { target, start, end, .. } => {
            format_expr(writer, target);
            writer.write("[");
            if let Some(s) = start {
                format_expr(writer, s);
            }
            writer.write(":");
            if let Some(e) = end {
                format_expr(writer, e);
            }
            writer.write("]");
        }

        Expr::Call { callee, args, .. } => {
            format_expr(writer, callee);
            writer.write("(");
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    writer.write(", ");
                }
                format_expr(writer, arg);
            }
            writer.write(")");
        }

        Expr::ArrayLiteral { elements, .. } => {
            format_array(writer, elements);
        }

        Expr::ArrayFor { seq, body, filter, .. } => {
            writer.write("[for (");
            format_expr(writer, seq);
            writer.write(") ");
            format_expr(writer, body);
            if let Some(f) = filter {
                writer.write(" if ");
                format_expr(writer, f);
            }
            writer.write("]");
        }

        Expr::ObjectLiteral { entries, trivia, trailing_trivia, .. } => {
            format_object(writer, entries, trivia.as_ref(), trailing_trivia.as_ref());
        }

        Expr::ObjectFor { seq, key, value, filter, .. } => {
            writer.write("{for (");
            format_expr(writer, seq);
            writer.write(") ");
            format_expr(writer, key);
            writer.write(": ");
            format_expr(writer, value);
            if let Some(f) = filter {
                writer.write(" if ");
                format_expr(writer, f);
            }
            writer.write("}");
        }

        Expr::LetBlock { lets, body, .. } => {
            for let_stmt in lets {
                format_let(writer, let_stmt);
                writer.write(" ");
            }
            format_expr(writer, body);
        }

        Expr::Group { expr, .. } => {
            writer.write("(");
            format_expr(writer, expr);
            writer.write(")");
        }

        Expr::FunctionRef { name, .. } => {
            writer.write(name);
        }
    }
}

/// Format an array literal
fn format_array(writer: &mut Writer, elements: &[Expr]) {
    if elements.is_empty() {
        writer.write("[]");
        return;
    }

    // Try single-line first
    let single_line = format_array_single_line(elements);
    if writer.fits_on_line(&single_line) {
        writer.write(&single_line);
    } else {
        // Multi-line format
        writer.write("[");
        writer.newline();
        writer.increase_indent();
        for (i, elem) in elements.iter().enumerate() {
            format_expr(writer, elem);
            if i < elements.len() - 1 {
                writer.write(",");
            }
            writer.newline();
        }
        writer.decrease_indent();
        writer.write("]");
    }
}

/// Format an object literal
fn format_object(
    writer: &mut Writer,
    entries: &[ObjectEntry],
    trivia: Option<&ast::TriviaCollection>,
    trailing_trivia: Option<&ast::TriviaCollection>,
) {
    if let Some(t) = trivia {
        format_leading_trivia(writer, t);
    }

    if entries.is_empty() && trailing_trivia.is_none() {
        writer.write("{}");
        return;
    }

    // Check if any entry has comments or blank lines - if so, force multi-line
    let has_comments_or_blanks = trailing_trivia.is_some()
        || entries.iter().any(|entry| match entry {
        ObjectEntry::Pair { trivia, blank_lines_before, .. } | ObjectEntry::Spread { trivia, blank_lines_before, .. } => {
            trivia.is_some() || *blank_lines_before > 0
        }
    });

    // Try single-line first (but not if there are comments or blank lines)
    let single_line = format_object_single_line(entries);
    if !has_comments_or_blanks && writer.fits_on_line(&single_line) {
        writer.write(&single_line);
    } else {
        // Multi-line format
        writer.write("{");
        writer.newline();
        writer.increase_indent();
        for (i, entry) in entries.iter().enumerate() {
            match entry {
                ObjectEntry::Pair { key, value, trivia, blank_lines_before, .. } => {
                    // Add blank lines before this entry (skip for first entry)
                    // Comments in trivia are already rendered as their own lines.
                    if i > 0 {
                        let blank_count = if let Some(t) = trivia {
                            blank_lines_before.saturating_sub(t.leading.len())
                        } else {
                            *blank_lines_before
                        };
                        for _ in 0..blank_count {
                            writer.newline();
                        }
                    }

                    // Format leading comments
                    if let Some(t) = trivia {
                        format_leading_trivia(writer, t);
                    }
                    format_object_key(writer, key);
                    writer.write(": ");
                    format_expr(writer, value);
                }
                ObjectEntry::Spread { value, trivia, blank_lines_before, .. } => {
                    // Add blank lines before this entry (skip for first entry)
                    // Comments in trivia are already rendered as their own lines.
                    if i > 0 {
                        let blank_count = if let Some(t) = trivia {
                            blank_lines_before.saturating_sub(t.leading.len())
                        } else {
                            *blank_lines_before
                        };
                        for _ in 0..blank_count {
                            writer.newline();
                        }
                    }

                    // Format leading comments
                    if let Some(t) = trivia {
                        format_leading_trivia(writer, t);
                    }
                    writer.write("*: ");
                    format_expr(writer, value);
                }
            }
            if i < entries.len() - 1 {
                writer.write(",");
            }
            writer.newline();
        }
        if let Some(t) = trailing_trivia {
            format_leading_trivia(writer, t);
        }
        writer.decrease_indent();
        writer.write("}");
    }
}

/// Format object key
fn format_object_key(writer: &mut Writer, key: &ObjectKey) {
    match key {
        ObjectKey::Ident(id) => writer.write(&id.name),
        ObjectKey::Str { value, .. } => format_string(writer, value),
    }
}

/// Format a string literal with proper escaping
fn format_string(writer: &mut Writer, s: &str) {
    writer.write("\"");
    for ch in s.chars() {
        match ch {
            '"' => writer.write("\\\""),
            '\\' => writer.write("\\\\"),
            '\u{08}' => writer.write("\\b"),
            '\u{0C}' => writer.write("\\f"),
            '\n' => writer.write("\\n"),
            '\r' => writer.write("\\r"),
            '\t' => writer.write("\\t"),
            c if c.is_control() => {
                writer.write(&format!("\\u{:04X}", c as u32));
            }
            c => writer.write(&c.to_string()),
        }
    }
    writer.write("\"");
}

/// Get string representation of binary operator
fn binary_op_str(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Mul => "*",
        BinaryOp::Div => "/",
        BinaryOp::Rem => "%",
        BinaryOp::Add => "+",
        BinaryOp::Sub => "-",
        BinaryOp::Lt => "<",
        BinaryOp::Le => "<=",
        BinaryOp::Gt => ">",
        BinaryOp::Ge => ">=",
        BinaryOp::Eq => "==",
        BinaryOp::Ne => "!=",
        BinaryOp::And => "and",
        BinaryOp::Or => "or",
    }
}

/// Try to format array on a single line (for checking if it fits)
fn format_array_single_line(elements: &[Expr]) -> String {
    let mut result = String::from("[");
    for (i, elem) in elements.iter().enumerate() {
        if i > 0 {
            result.push_str(", ");
        }
        result.push_str(&format!("{}", elem));
    }
    result.push(']');
    result
}

/// Try to format object on a single line (for checking if it fits)
fn format_object_single_line(entries: &[ObjectEntry]) -> String {
    let mut result = String::from("{");
    for (i, entry) in entries.iter().enumerate() {
        if i > 0 {
            result.push_str(", ");
        }
        match entry {
            ObjectEntry::Pair { key, value, .. } => {
                result.push_str(&format_object_key_string(key));
                result.push_str(": ");
                result.push_str(&format!("{}", value));
            }
            ObjectEntry::Spread { value, .. } => {
                result.push_str("*: ");
                result.push_str(&format!("{}", value));
            }
        }
    }
    result.push('}');
    result
}

fn format_object_key_string(key: &ObjectKey) -> String {
    match key {
        ObjectKey::Ident(id) => id.name.clone(),
        ObjectKey::Str { value, .. } => format!("\"{}\"", value),
    }
}
