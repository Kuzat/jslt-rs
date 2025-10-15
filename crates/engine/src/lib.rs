use ast::Program;
use interp::binder::{BindError, Binder, BoundProgram};
use interp::{apply, apply_with_modules, bind, EvalConfig, RuntimeError};
use parser::Parser;
use serde_json::Value;
use std::collections::HashMap;
use std::fs;
use std::fs::write;
use std::path::Path;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum EngineError {
    #[error("parse error: {0}")]
    Parse(#[from] parser::ParseError),
    #[error("bind error: {0}")]
    Bind(#[from] BindError),
    #[error("runtime error: {0}")]
    Runtime(#[from] RuntimeError),
    #[error("io error: {0}")]
    ModuleError(String),
}

pub struct ModuleLoader {
    stack: Vec<String>,
    seen: HashMap<String, BoundProgram>,
}

impl ModuleLoader {
    pub fn new() -> Self {
        ModuleLoader { stack: vec![], seen: HashMap::new() }
    }

    fn push(&mut self, key: &str) -> Result<(), String> {
        if self.stack.iter().any(|p| p == key) {
            let mut cycle = self.stack.clone();
            cycle.push(key.to_string());
            return Err(format!("cyclic import detected: {}", cycle.join(" -> ")));
        }
        self.stack.push(key.to_string());
        Ok(())
    }

    fn pop(&mut self) {
        let _ = self.stack.pop();
    }

    fn read_to_string(&self, path: &Path) -> Result<String, Box<dyn std::error::Error>> {
        Ok(fs::read_to_string(path)?)
    }

    fn canonical_key(base_dir: &Path, import_path: &str) -> String {
        // Resolve "import_path" relative to base_dir, normalize to string key
        let p = base_dir.join(import_path);
        p.to_string_lossy().to_string()
    }

    // Load a module by path (relative to base_dir of the file that references it),
    // parse, recursively load its imports, bind it with a fresh Binder, and cache it.
    pub fn load_module(
        &mut self,
        base_dir: &Path,
        import_path: &str,
    ) -> Result<(String, BoundProgram, bool, Vec<(String, String)>), EngineError> {
        let key = Self::canonical_key(base_dir, import_path);

        if let Some(bp) = self.seen.get(&key) {
            // Already compiled. callable = “has body”; we don’t store AST here, so
            // we assume importer will wire callable from the child AST on its own call.
            // To return callable here, we need to re-parse to know body presence.
            // Instead, we re-parse quickly to inspect body presence and imports list (cheap).
            let src = self
                .read_to_string(Path::new(&key))
                .map_err(|e| EngineError::ModuleError(format!("{:?}", e)))?;
            let mut p = Parser::new(&src)?;
            let ast = p.parse_program()?;
            let callable = ast.body.is_some();
            let imports: Vec<(String, String)> =
                ast.imports.iter().map(|i| (i.alias.clone(), i.path.clone())).collect();
            return Ok((key, bp.clone(), callable, imports));
        }

        self.push(&key).map_err(|e| EngineError::ModuleError(format!("{:?}", e)))?;

        // Parse the child module
        let src = self
            .read_to_string(Path::new(&key))
            .map_err(|e| EngineError::ModuleError(format!("{:?}", e)))?;
        let mut parser = Parser::new(&src)?;
        let ast = parser.parse_program()?;

        // Recursively load and bind its imports first
        let mut child_binder = Binder::new();
        for imp in &ast.imports {
            let child_base = Path::new(&key).parent().unwrap_or(Path::new("."));
            let (child_key, child_bound, child_callable, grandkids) =
                self.load_module(child_base, &imp.path)?;
            child_binder.register_imported_module(&child_key, child_bound, child_callable);
            // Wire the alias in the child binder
            child_binder.wire_import_alias(&imp.alias, &child_key);
            // The grandkids are already folded into child_bound; nothing else to do here.
            let _ = grandkids; // silence warning; kept for clarity.
        }

        // Bind the child module itself (its imports are in child_binder)
        let bound = child_binder.bind_program(&ast)?;
        let callable = ast.body.is_some();
        let imports: Vec<(String, String)> =
            ast.imports.iter().map(|i| (i.alias.clone(), i.path.clone())).collect();

        self.seen.insert(key.clone(), bound.clone());
        self.pop();

        Ok((key, bound, callable, imports))
    }
}

#[derive(Debug, Clone)]
pub struct CompiledProgram {
    bound: BoundProgram,
    // Add: all loaded child modules (key -> BoumdProgram) and whether callable
    modules: HashMap<String, (BoundProgram, bool)>,
}

impl CompiledProgram {
    pub fn apply(&self, input: &Value, cfg: Option<EvalConfig>) -> Result<Value, EngineError> {
        Ok(apply_with_modules(&self.bound, &self.modules, input, cfg)?)
    }
}

pub fn compile(src: &str) -> Result<CompiledProgram, EngineError> {
    // Compile with the current directory as the import path
    compile_with_import_path(src, ".")
}

pub fn compile_with_import_path(
    main_src: &str,
    main_path: &str,
) -> Result<CompiledProgram, EngineError> {
    // Parse main
    let mut parser = Parser::new(main_src)?;
    let ast: Program = parser.parse_program()?;

    // Prepare binder for main, and loader
    let mut binder = Binder::new();
    let mut loader = ModuleLoader::new();

    // Accumulate all children for runtime init
    let mut all_children: HashMap<String, (BoundProgram, bool)> = HashMap::new();

    // Load and register each import (recursive)
    let base_dir = Path::new(main_path).parent().unwrap_or(Path::new("."));
    for imp in &ast.imports {
        let (key, bound_child, callable, _children) = loader.load_module(base_dir, &imp.path)?;
        all_children.insert(key.clone(), (bound_child.clone(), callable));
        binder.register_imported_module(&key, bound_child, callable);
        binder.wire_import_alias(&imp.alias, &key);
    }

    // Bind main
    let bound = binder.bind_program(&ast)?;
    Ok(CompiledProgram { bound, modules: all_children })
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn end_to_end_smoke_compile_and_apply() {
        // Program: { "sum": .a + .b, "greet": "hi " + .name }
        let src = r#"
        {
          "sum": .a + .b,
          "greet": "hi " + .name
        }
        "#;
        let prog = compile(src).expect("compile");
        let input = json!({"a": 2, "b": 3, "name": "alice"});
        let out = prog.apply(&input, None).expect("apply");
        assert_eq!(out, json!({"sum": 5.0, "greet": "hi alice"}));
    }
}
