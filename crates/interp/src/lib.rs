use ast::Program;
use crate::binder::{BindError, Binder, BoundProgram};

pub mod binder;

pub fn bind(program: &Program) -> Result<BoundProgram, BindError> {
    let mut b = Binder::new();
    b.bind_program(program)
}

