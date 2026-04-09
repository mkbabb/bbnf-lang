//! Per-file selective import resolution and path helpers.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::graph::deps;

use super::errors::ImportError;
use super::registry::{ModuleData, ModuleRegistry, ResolvedImport};

pub(super) fn resolve_imports_for(path: &Path, registry: &mut ModuleRegistry) {
    let module = match registry.modules.get(path) {
        Some(m) => m,
        None => return,
    };

    let dir = path.parent().unwrap_or_else(|| {
        panic!(
            "module path `{}` has no parent directory for import resolution",
            path.display()
        )
    });
    let mut resolved: Vec<ResolvedImport> = Vec::new();
    // Track which names have been imported and from where (for conflict detection).
    let mut imported_names: HashMap<String, PathBuf> = HashMap::new();

    // Clone the imports to avoid borrow issues.
    let imports: Vec<_> = module
        .grammar
        .imports
        .iter()
        .map(|imp| {
            (
                resolve_import_path(dir, &imp.path),
                imp.items.as_ref().map(|items| {
                    items
                        .iter()
                        .map(|i| i.name.to_string())
                        .collect::<Vec<String>>()
                }),
            )
        })
        .collect();

    for (import_path, items) in imports {
        let canonical = match import_path.canonicalize() {
            Ok(c) => c,
            Err(_) => {
                registry.errors.push(ImportError::FileNotFound {
                    path: import_path.clone(),
                    imported_from: path.to_path_buf(),
                });
                continue;
            }
        };

        let target = match registry.modules.get(&canonical) {
            Some(m) => m,
            None => {
                registry.errors.push(ImportError::ParseError {
                    path: canonical.clone(),
                    message:
                        "import graph invariant violation: canonical module missing from registry"
                            .to_string(),
                });
                continue;
            }
        };

        let rule_names: Vec<String> = if let Some(items) = items {
            // Selective import: verify each named rule exists, then unfurl
            // transitive local dependencies.
            let mut verified = Vec::new();
            for name in &items {
                if target.local_rule_names.contains(name) {
                    verified.push(name.clone());
                } else {
                    registry.errors.push(ImportError::MissingRule {
                        rule_name: name.clone(),
                        path: canonical.clone(),
                        imported_from: path.to_path_buf(),
                    });
                }
            }
            // Expand with transitive deps.
            let mut expanded = HashSet::new();
            for name in &verified {
                for dep in transitive_local_deps(name, target) {
                    expanded.insert(dep);
                }
            }
            expanded.into_iter().collect()
        } else {
            // Glob import: all local rules.
            target.local_rule_names.clone()
        };

        // Check for name conflicts.
        for name in &rule_names {
            if let Some(prev_source) = imported_names.get(name) {
                registry.errors.push(ImportError::NameConflict {
                    rule_name: name.clone(),
                    source_a: prev_source.clone(),
                    source_b: canonical.clone(),
                    imported_from: path.to_path_buf(),
                });
            } else {
                imported_names.insert(name.clone(), canonical.clone());
            }
        }

        resolved.push(ResolvedImport {
            source: canonical,
            rule_names,
        });
    }

    registry
        .resolved_imports
        .insert(path.to_path_buf(), resolved);
}

/// Compute the transitive closure of local dependencies starting from `rule_name`
/// within the given module. Returns a set of all rule names that `rule_name`
/// transitively depends on (including itself).
fn transitive_local_deps(rule_name: &str, module: &ModuleData) -> HashSet<String> {
    let mut result = HashSet::new();
    let mut queue = vec![rule_name.to_string()];

    while let Some(name) = queue.pop() {
        if result.contains(&name) {
            continue;
        }
        result.insert(name.clone());

        // Look up the rule by string key in the new AST.
        if let Some(entry) = module.grammar.rules.get(name.as_str()) {
            let mut refs = indexmap::IndexSet::new();
            deps::collect_nonterminal_refs(entry.rhs, &mut refs);
            for r in refs {
                let r_owned = r.to_string();
                if module.local_rule_names.contains(&r_owned) && !result.contains(&r_owned) {
                    queue.push(r_owned);
                }
            }
        }
    }

    result
}

/// Resolve an import path relative to the importing file's directory.
pub(super) fn resolve_import_path(dir: &Path, import_path: &str) -> PathBuf {
    let mut path = dir.join(import_path);
    // Append .bbnf if no extension given.
    if path.extension().is_none() {
        path.set_extension("bbnf");
    }
    path
}
