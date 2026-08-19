use crate::context::{AnalysisSnapshot, ScopeId, Symbol, SymbolKind};
use crate::state::JsltLanguageServer;
use ast::Span;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use tower_lsp::lsp_types::Url;

/// File extensions treated as JSLT sources during workspace discovery.
const JSLT_EXTENSIONS: [&str; 2] = ["jslt", "jstl2"];
/// Guard rails so an unexpectedly large workspace root cannot stall startup.
const MAX_INDEXED_FILES: usize = 2_000;
const MAX_WALK_DEPTH: usize = 16;
const SKIPPED_DIRS: [&str; 4] = ["target", "node_modules", "dist", "build"];

/// An `import` header entry with its module path resolved to a workspace file.
#[derive(Debug, Clone)]
pub(crate) struct ResolvedImport {
    pub(crate) alias: String,
    pub(crate) path: String,
    #[allow(dead_code)]
    pub(crate) span: Span,
    pub(crate) target: Option<Url>,
}

/// A single JSLT file in the workspace together with its analysis.
#[derive(Debug, Clone)]
pub(crate) struct IndexedFile {
    pub(crate) uri: Url,
    pub(crate) snapshot: AnalysisSnapshot,
    pub(crate) imports: Vec<ResolvedImport>,
}

impl IndexedFile {
    /// The alias this file uses for `target`, if it imports it.
    pub(crate) fn alias_for(&self, target: &Url) -> Option<&str> {
        self.imports
            .iter()
            .find(|imp| imp.target.as_ref() == Some(target))
            .map(|imp| imp.alias.as_str())
    }

    /// A top-level `def` in this file, which is what importers can call.
    pub(crate) fn exported_function(&self, name: &str) -> Option<&Symbol> {
        self.snapshot.symbols.iter().find(|symbol| {
            symbol.kind == SymbolKind::Function && symbol.name == name && symbol.scope == ScopeId(0)
        })
    }
}

/// Workspace-wide file index and module graph.
///
/// Entries are keyed by canonicalized URI so that a file reached through an
/// `import` resolves to the same entry as the buffer opened in the editor.
#[derive(Debug, Default)]
pub(crate) struct WorkspaceIndex {
    roots: Vec<PathBuf>,
    files: HashMap<Url, IndexedFile>,
}

impl WorkspaceIndex {
    pub(crate) fn set_roots(&mut self, roots: Vec<PathBuf>) {
        self.roots = roots;
    }

    pub(crate) fn len(&self) -> usize {
        self.files.len()
    }

    /// Walk the configured roots and index every JSLT file found on disk.
    ///
    /// Returns the number of newly indexed files. Files already indexed from an
    /// editor buffer are left alone, since that content is more current.
    pub(crate) fn discover(&mut self) -> usize {
        let mut paths = Vec::new();
        for root in self.roots.clone() {
            collect_jslt_files(&root, 0, &mut paths);
        }

        let mut indexed = 0;
        for path in paths {
            let Some(uri) = file_uri(&path) else {
                continue;
            };
            if self.files.contains_key(&uri) {
                continue;
            }
            let Ok(text) = fs::read_to_string(&path) else {
                continue;
            };
            self.index_text(&uri, &text, None);
            indexed += 1;
        }
        indexed
    }

    /// Analyze `text` as the current content of `uri` and index the result.
    pub(crate) fn index_text(&mut self, uri: &Url, text: &str, version: Option<i32>) {
        let snapshot = JsltLanguageServer::build_analysis_snapshot(uri, text, version);
        self.index_snapshot(uri, snapshot);
    }

    /// Index an already-computed snapshot, refreshing this file's import edges.
    pub(crate) fn index_snapshot(&mut self, uri: &Url, snapshot: AnalysisSnapshot) {
        let imports = snapshot
            .imports
            .iter()
            .map(|imp| ResolvedImport {
                alias: imp.alias.clone(),
                path: imp.path.clone(),
                span: imp.span,
                target: JsltLanguageServer::resolve_import_uri(uri, &imp.path),
            })
            .collect();

        let uri = canonical_uri(uri);
        self.files.insert(uri.clone(), IndexedFile { uri, snapshot, imports });
    }

    /// Re-read a file from disk, dropping any unsaved editor content.
    ///
    /// Used when a document is closed; a file that no longer exists is removed
    /// from the index entirely.
    pub(crate) fn refresh_from_disk(&mut self, uri: &Url) {
        let key = canonical_uri(uri);
        match key.to_file_path().ok().and_then(|path| fs::read_to_string(path).ok()) {
            Some(text) => self.index_text(&key, &text, None),
            None => {
                self.files.remove(&key);
            }
        }
    }

    pub(crate) fn get(&self, uri: &Url) -> Option<&IndexedFile> {
        self.files.get(&canonical_uri(uri))
    }

    /// Index `uri` from disk if it has not been seen yet.
    pub(crate) fn ensure_indexed(&mut self, uri: &Url) -> Option<&IndexedFile> {
        let key = canonical_uri(uri);
        if !self.files.contains_key(&key) {
            let path = key.to_file_path().ok()?;
            let text = fs::read_to_string(path).ok()?;
            self.index_text(&key, &text, None);
        }
        self.files.get(&key)
    }

    /// Files that import `uri`, paired with the alias each one uses for it.
    pub(crate) fn dependents_of(&self, uri: &Url) -> Vec<(Url, String)> {
        let target = canonical_uri(uri);
        let mut out: Vec<(Url, String)> = self
            .files
            .values()
            .filter(|file| file.uri != target)
            .flat_map(|file| {
                file.imports
                    .iter()
                    .filter(|imp| imp.target.as_ref() == Some(&target))
                    .map(|imp| (file.uri.clone(), imp.alias.clone()))
            })
            .collect();

        out.sort();
        out.dedup();
        out
    }
}

/// Canonicalize a `file:` URI so symlinked and relative paths compare equal.
///
/// Falls back to the original URI for non-file or not-yet-existing paths.
pub(crate) fn canonical_uri(uri: &Url) -> Url {
    uri.to_file_path()
        .ok()
        .and_then(|path| path.canonicalize().ok())
        .and_then(|path| Url::from_file_path(path).ok())
        .unwrap_or_else(|| uri.clone())
}

fn file_uri(path: &Path) -> Option<Url> {
    let canonical = path.canonicalize().ok()?;
    Url::from_file_path(canonical).ok()
}

fn collect_jslt_files(dir: &Path, depth: usize, out: &mut Vec<PathBuf>) {
    if depth > MAX_WALK_DEPTH || out.len() >= MAX_INDEXED_FILES {
        return;
    }

    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };

    for entry in entries.flatten() {
        if out.len() >= MAX_INDEXED_FILES {
            return;
        }

        let path = entry.path();
        let Some(name) = path.file_name().and_then(|name| name.to_str()) else {
            continue;
        };

        if path.is_dir() {
            if name.starts_with('.') || SKIPPED_DIRS.contains(&name) {
                continue;
            }
            collect_jslt_files(&path, depth + 1, out);
        } else if path
            .extension()
            .and_then(|ext| ext.to_str())
            .is_some_and(|ext| JSLT_EXTENSIONS.contains(&ext))
        {
            out.push(path);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    fn write(dir: &Path, name: &str, contents: &str) -> Url {
        let path = dir.join(name);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("create dir");
        }
        fs::write(&path, contents).expect("write file");
        file_uri(&path).expect("uri")
    }

    fn temp_dir(name: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!("jslt-lsp-workspace-{}", name));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).expect("create temp dir");
        dir.canonicalize().expect("canonical temp dir")
    }

    #[test]
    fn discover_indexes_jslt_files_and_skips_ignored_dirs() {
        let dir = temp_dir("discover");
        write(&dir, "main.jslt", "1");
        write(&dir, "nested/other.jstl2", "2");
        write(&dir, "target/ignored.jslt", "3");
        write(&dir, "notes.txt", "not jslt");

        let mut index = WorkspaceIndex::default();
        index.set_roots(vec![dir.clone()]);
        let indexed = index.discover();

        assert_eq!(indexed, 2);
        assert_eq!(index.len(), 2);
        assert!(index.get(&file_uri(&dir.join("main.jslt")).expect("uri")).is_some());
        assert!(index.get(&file_uri(&dir.join("target/ignored.jslt")).expect("uri")).is_none());

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn module_graph_links_importers_to_their_targets() {
        let dir = temp_dir("graph");
        let module = write(&dir, "utils.jslt", "def double(x)\n  $x * 2\n\ndouble(.value)\n");
        let main = write(&dir, "main.jslt", "import \"utils.jslt\" as utils\n\nutils:double(.n)\n");

        let mut index = WorkspaceIndex::default();
        index.set_roots(vec![dir.clone()]);
        index.discover();

        let dependents = index.dependents_of(&module);
        assert_eq!(dependents, vec![(main.clone(), "utils".to_string())]);

        let importer = index.get(&main).expect("indexed importer");
        assert_eq!(importer.alias_for(&module), Some("utils"));

        let target = index.get(&module).expect("indexed module");
        assert!(target.exported_function("double").is_some());
        assert!(target.exported_function("missing").is_none());

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn reindexing_a_buffer_refreshes_import_edges() {
        let dir = temp_dir("reindex");
        let module = write(&dir, "utils.jslt", "def double(x)\n  $x * 2\n\ndouble(.value)\n");
        let main = write(&dir, "main.jslt", "1\n");

        let mut index = WorkspaceIndex::default();
        index.set_roots(vec![dir.clone()]);
        index.discover();
        assert!(index.dependents_of(&module).is_empty());

        index.index_text(&main, "import \"utils.jslt\" as utils\n\nutils:double(.n)\n", Some(2));
        assert_eq!(index.dependents_of(&module), vec![(main, "utils".to_string())]);

        let _ = fs::remove_dir_all(&dir);
    }
}
