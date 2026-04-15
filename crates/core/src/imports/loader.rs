//! DFS module graph loader with partial-init cycle handling.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::pipeline::directives::parse_to_pipeline_inputs;

use super::errors::ImportError;
use super::registry::{ImportCycle, ModuleData, ModuleRegistry};
use super::resolve::{resolve_import_path, resolve_imports_for};

/// Load a module graph starting from an entry file.
///
/// Performs a DFS traversal of `@import` directives, parsing each file exactly
/// once (canonical path dedup). Cyclic imports are allowed (Python-style
/// partial-init: a module is registered before its imports are processed).
/// Returns a `ModuleRegistry` with all modules and resolved imports.
///
/// Tranche AU.4.1: each file's tape walk now lands directly in
/// `ModulePipelineData` (AST + directive maps + imports list) — the
/// `ParsedGrammar` middle step is gone.
pub fn load_module_graph(entry: &Path) -> Result<ModuleRegistry, ImportError> {
    let entry = entry
        .canonicalize()
        .map_err(|_| ImportError::FileNotFound {
            path: entry.to_path_buf(),
            imported_from: PathBuf::from("<entry>"),
        })?;

    let mut registry = ModuleRegistry {
        modules: HashMap::new(),
        resolved_imports: HashMap::new(),
        errors: Vec::new(),
        cycles: Vec::new(),
    };

    let mut visited: HashSet<PathBuf> = HashSet::new();
    let mut loading: HashSet<PathBuf> = HashSet::new();
    let mut active_chain: Vec<PathBuf> = Vec::new();

    load_recursive(
        &entry,
        &PathBuf::from("<entry>"),
        &mut registry,
        &mut visited,
        &mut loading,
        &mut active_chain,
    );

    // Phase 2: resolve imports for every visited module.
    let paths: Vec<PathBuf> = visited.iter().cloned().collect();
    for path in &paths {
        resolve_imports_for(path, &mut registry);
    }

    Ok(registry)
}

fn load_recursive(
    path: &Path,
    imported_from: &Path,
    registry: &mut ModuleRegistry,
    visited: &mut HashSet<PathBuf>,
    loading: &mut HashSet<PathBuf>,
    active_chain: &mut Vec<PathBuf>,
) {
    if loading.contains(path) {
        let start = active_chain
            .iter()
            .position(|p| p.as_path() == path)
            .unwrap_or(0);
        let mut chain = active_chain[start..].to_vec();
        chain.push(path.to_path_buf());
        registry.cycles.push(ImportCycle {
            path: path.to_path_buf(),
            chain,
        });
        return;
    }

    // Already parsed and fully resolved.
    if visited.contains(path) {
        return;
    }

    // Read and parse the file.
    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(_) => {
            registry.errors.push(ImportError::FileNotFound {
                path: path.to_path_buf(),
                imported_from: imported_from.to_path_buf(),
            });
            return;
        }
    };

    // Parse straight into pipeline-shaped containers. The walker
    // leaks the source so the borrowed spans inside the AST /
    // directives become `'static`, which matches the ownership
    // contract the rest of the compile pipeline already assumes.
    //
    // SAFETY: leaking the source is acceptable because
    // `load_module_graph()` is only called from:
    //   1. The proc-macro derive path (`bbnf-derive`), where the
    //      process exits after compilation.
    //   2. Integration tests, where the leaked memory is reclaimed
    //      at process exit.
    // The LSP does NOT use this function — it uses `self_cell` in
    // `analysis/src/state/parsing.rs` for safe self-referential
    // ownership without leaking.
    let (ast, directives, imports) = match parse_to_pipeline_inputs(&source) {
        Some(t) => t,
        None => {
            registry.errors.push(ImportError::ParseError {
                path: path.to_path_buf(),
                message: "Failed to parse grammar".to_string(),
            });
            return;
        }
    };

    // Extract local rule names — keys are already &str in the AST.
    let local_rule_names: Vec<String> = ast.keys().map(|k| k.to_string()).collect();

    // Register BEFORE recursing (partial-init, like Python module loading).
    // This allows cyclic imports to find the module already registered.
    visited.insert(path.to_path_buf());
    loading.insert(path.to_path_buf());
    active_chain.push(path.to_path_buf());

    let data = crate::pipeline::directives::ModulePipelineData {
        source,
        ast,
        directives,
        imports: imports.clone(),
        local_rule_names,
    };
    registry
        .modules
        .insert(path.to_path_buf(), ModuleData::from_pipeline_data(data));

    // Recursively load imports. Cycles find the file already in visited and return.
    let dir = path.parent().unwrap_or_else(|| {
        panic!(
            "module path `{}` has no parent directory for import resolution",
            path.display()
        )
    });
    let import_paths: Vec<PathBuf> = imports
        .iter()
        .map(|imp| resolve_import_path(dir, &imp.path))
        .collect();

    for import_path in import_paths {
        match import_path.canonicalize() {
            Ok(canonical) => {
                load_recursive(&canonical, path, registry, visited, loading, active_chain);
            }
            Err(_) => {
                registry.errors.push(ImportError::FileNotFound {
                    path: import_path,
                    imported_from: path.to_path_buf(),
                });
            }
        }
    }

    let popped = active_chain.pop().unwrap_or_else(|| {
        panic!(
            "active import chain underflow while leaving `{}`",
            path.display()
        )
    });
    assert_eq!(
        popped.as_path(),
        path,
        "active import chain mismatch while leaving `{}`",
        path.display()
    );
    assert!(
        loading.remove(path),
        "loading-set mismatch while leaving `{}`",
        path.display()
    );
}
