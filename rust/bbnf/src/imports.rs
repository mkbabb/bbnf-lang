//! Import resolution for BBNF grammars.
//!
//! Provides a `ModuleRegistry` that loads a graph of `.bbnf` files connected by
//! `@import` directives. Each file is parsed once, and imports are resolved to
//! produce a per-file namespace of visible rules.

use std::collections::{HashMap, HashSet};
use std::fmt;
use std::path::{Path, PathBuf};

use crate::grammar::{BBNFGrammar, ParsedGrammar};

// ---------------------------------------------------------------------------
// Error types
// ---------------------------------------------------------------------------

/// Errors that can occur during import resolution.
#[derive(Debug)]
pub enum ImportError {
    /// File could not be read.
    FileNotFound {
        path: PathBuf,
        imported_from: PathBuf,
    },
    /// Circular import chain detected.
    CircularImport {
        /// The path that closes the cycle.
        path: PathBuf,
        /// The chain of paths leading to the cycle (first = entry).
        chain: Vec<PathBuf>,
    },
    /// A selective import names a rule that doesn't exist in the target file.
    MissingRule {
        rule_name: String,
        path: PathBuf,
        imported_from: PathBuf,
    },
    /// Two imports define the same rule name.
    NameConflict {
        rule_name: String,
        source_a: PathBuf,
        source_b: PathBuf,
        imported_from: PathBuf,
    },
    /// Parse error in a dependent file.
    ParseError {
        path: PathBuf,
        message: String,
    },
}

impl fmt::Display for ImportError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ImportError::FileNotFound { path, imported_from } => {
                write!(f, "File not found: `{}` (imported from `{}`)",
                    path.display(), imported_from.display())
            }
            ImportError::CircularImport { path, chain } => {
                let chain_str: Vec<String> = chain.iter().map(|p| p.display().to_string()).collect();
                write!(f, "Circular import: `{}` (chain: {} → {})",
                    path.display(), chain_str.join(" → "), path.display())
            }
            ImportError::MissingRule { rule_name, path, imported_from } => {
                write!(f, "Rule `{}` not found in `{}` (imported from `{}`)",
                    rule_name, path.display(), imported_from.display())
            }
            ImportError::NameConflict { rule_name, source_a, source_b, imported_from } => {
                write!(f, "Name conflict: rule `{}` is imported from both `{}` and `{}` in `{}`",
                    rule_name, source_a.display(), source_b.display(), imported_from.display())
            }
            ImportError::ParseError { path, message } => {
                write!(f, "Parse error in `{}`: {}", path.display(), message)
            }
        }
    }
}

impl std::error::Error for ImportError {}

// ---------------------------------------------------------------------------
// Module registry
// ---------------------------------------------------------------------------

/// Per-file module data after parsing.
#[derive(Debug)]
pub struct ModuleData {
    /// Source text (owned).
    pub source: String,
    /// The parsed grammar.
    pub grammar: ParsedGrammar<'static>,
    /// Names of rules defined locally in this file.
    pub local_rule_names: Vec<String>,
}

/// A resolved import: which rules are visible and where they come from.
#[derive(Debug, Clone)]
pub struct ResolvedImport {
    /// Source file path.
    pub source: PathBuf,
    /// Rule names imported from this source.
    pub rule_names: Vec<String>,
}

/// Registry of all loaded modules in an import graph.
#[derive(Debug)]
pub struct ModuleRegistry {
    /// Canonical path → module data.
    modules: HashMap<PathBuf, ModuleData>,
    /// Canonical path → resolved imports (which rules are visible from imports).
    resolved_imports: HashMap<PathBuf, Vec<ResolvedImport>>,
    /// All errors encountered during loading.
    pub errors: Vec<ImportError>,
}

impl ModuleRegistry {
    /// Get a module's data by canonical path.
    pub fn get_module(&self, path: &Path) -> Option<&ModuleData> {
        self.modules.get(path)
    }

    /// Get the resolved imports for a file.
    pub fn get_resolved_imports(&self, path: &Path) -> Option<&[ResolvedImport]> {
        self.resolved_imports.get(path).map(|v| v.as_slice())
    }

    /// Get all imported rule names for a file (flattened).
    pub fn imported_rule_names(&self, path: &Path) -> HashSet<String> {
        let mut names = HashSet::new();
        if let Some(imports) = self.resolved_imports.get(path) {
            for imp in imports {
                for name in &imp.rule_names {
                    names.insert(name.clone());
                }
            }
        }
        names
    }

    /// Get all canonical paths in the registry.
    pub fn paths(&self) -> impl Iterator<Item = &PathBuf> {
        self.modules.keys()
    }
}

// ---------------------------------------------------------------------------
// Loading algorithm
// ---------------------------------------------------------------------------

/// Load a module graph starting from an entry file.
///
/// Performs a DFS traversal of `@import` directives, parsing each file exactly
/// once (canonical path dedup). Cyclic imports are allowed (Python-style
/// partial-init: a module is registered before its imports are processed).
/// Returns a `ModuleRegistry` with all modules and resolved imports.
pub fn load_module_graph(entry: &Path) -> Result<ModuleRegistry, ImportError> {
    let entry = entry.canonicalize().map_err(|_| ImportError::FileNotFound {
        path: entry.to_path_buf(),
        imported_from: PathBuf::from("<entry>"),
    })?;

    let mut registry = ModuleRegistry {
        modules: HashMap::new(),
        resolved_imports: HashMap::new(),
        errors: Vec::new(),
    };

    let mut visited: HashSet<PathBuf> = HashSet::new();

    load_recursive(
        &entry,
        &PathBuf::from("<entry>"),
        &mut registry,
        &mut visited,
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
) {
    // Already parsed (or currently being parsed — cycle). Return harmlessly.
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

    // Parse using grammar_with_imports.
    // SAFETY: We leak the source string to get 'static lifetime for the AST.
    // This is intentional — the ModuleRegistry owns all module data for the
    // lifetime of the workspace. In production, use an arena allocator instead.
    let source_static: &'static str = Box::leak(source.clone().into_boxed_str());
    let parser = BBNFGrammar::grammar_with_imports();
    let (result, _parser_state) = parser.parse_return_state(source_static);

    let parsed = match result {
        Some(g) => g,
        None => {
            registry.errors.push(ImportError::ParseError {
                path: path.to_path_buf(),
                message: "Failed to parse grammar".to_string(),
            });
            return;
        }
    };

    // Extract local rule names.
    let local_rule_names: Vec<String> = parsed.rules.keys().filter_map(|expr| {
        if let crate::grammar::Expression::Nonterminal(tok) = expr {
            Some(tok.value.to_string())
        } else {
            None
        }
    }).collect();

    // Register BEFORE recursing (partial-init, like Python module loading).
    // This allows cyclic imports to find the module already registered.
    visited.insert(path.to_path_buf());
    registry.modules.insert(path.to_path_buf(), ModuleData {
        source,
        grammar: parsed,
        local_rule_names,
    });

    // Recursively load imports. Cycles find the file already in visited and return.
    let dir = path.parent().unwrap_or(Path::new("."));
    // Collect import paths first to avoid borrow issues with registry.
    let import_paths: Vec<PathBuf> = registry.modules.get(path)
        .unwrap()
        .grammar
        .imports
        .iter()
        .map(|imp| resolve_import_path(dir, &imp.path))
        .collect();

    for import_path in import_paths {
        match import_path.canonicalize() {
            Ok(canonical) => {
                load_recursive(
                    &canonical,
                    path,
                    registry,
                    visited,
                );
            }
            Err(_) => {
                registry.errors.push(ImportError::FileNotFound {
                    path: import_path,
                    imported_from: path.to_path_buf(),
                });
            }
        }
    }
}

fn resolve_imports_for(path: &Path, registry: &mut ModuleRegistry) {
    let module = match registry.modules.get(path) {
        Some(m) => m,
        None => return,
    };

    let dir = path.parent().unwrap_or(Path::new("."));
    let mut resolved: Vec<ResolvedImport> = Vec::new();
    // Track which names have been imported and from where (for conflict detection).
    let mut imported_names: HashMap<String, PathBuf> = HashMap::new();

    // Clone the imports to avoid borrow issues.
    let imports: Vec<_> = module.grammar.imports.iter().map(|imp| {
        (
            resolve_import_path(dir, &imp.path),
            imp.items.as_ref().map(|items| {
                items.iter().map(|i| i.to_string()).collect::<Vec<String>>()
            }),
        )
    }).collect();

    for (import_path, items) in imports {
        let canonical = match import_path.canonicalize() {
            Ok(c) => c,
            Err(_) => continue, // Already reported as FileNotFound.
        };

        let target = match registry.modules.get(&canonical) {
            Some(m) => m,
            None => continue, // Already reported.
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

    registry.resolved_imports.insert(path.to_path_buf(), resolved);
}

/// Compute the transitive closure of local dependencies starting from `rule_name`
/// within the given module. Returns a set of all rule names that `rule_name`
/// transitively depends on (including itself).
fn transitive_local_deps(rule_name: &str, module: &ModuleData) -> HashSet<String> {
    let mut deps = HashSet::new();
    let mut queue = vec![rule_name.to_string()];

    while let Some(name) = queue.pop() {
        if deps.contains(&name) {
            continue;
        }
        deps.insert(name.clone());

        // Find the rule in the module's grammar.
        let rule_expr = module.grammar.rules.iter().find_map(|(lhs, rhs)| {
            if let crate::grammar::Expression::Nonterminal(tok) = lhs {
                if tok.value == name {
                    return Some(rhs);
                }
            }
            None
        });

        if let Some(expr) = rule_expr {
            let refs = collect_nonterminal_refs(expr);
            for r in refs {
                if module.local_rule_names.contains(&r) && !deps.contains(&r) {
                    queue.push(r);
                }
            }
        }
    }

    deps
}

/// Collect all nonterminal references from an expression.
fn collect_nonterminal_refs(expr: &crate::grammar::Expression) -> Vec<String> {
    use crate::grammar::Expression;
    let mut refs = Vec::new();
    match expr {
        Expression::Nonterminal(tok) => {
            refs.push(tok.value.to_string());
        }
        Expression::Alternation(items) | Expression::Concatenation(items) => {
            for item in &items.value {
                refs.extend(collect_nonterminal_refs(item));
            }
        }
        Expression::Group(inner)
        | Expression::Optional(inner)
        | Expression::OptionalWhitespace(inner)
        | Expression::Many(inner)
        | Expression::Many1(inner) => {
            refs.extend(collect_nonterminal_refs(&inner.value));
        }
        Expression::Skip(a, b)
        | Expression::Next(a, b)
        | Expression::Minus(a, b) => {
            refs.extend(collect_nonterminal_refs(&a.value));
            refs.extend(collect_nonterminal_refs(&b.value));
        }
        Expression::ProductionRule(lhs, rhs) => {
            refs.extend(collect_nonterminal_refs(lhs));
            refs.extend(collect_nonterminal_refs(rhs));
        }
        Expression::MappedExpression((a, b)) => {
            refs.extend(collect_nonterminal_refs(&a.value));
            refs.extend(collect_nonterminal_refs(&b.value));
        }
        Expression::DebugExpression((inner, _)) => {
            refs.extend(collect_nonterminal_refs(&inner.value));
        }
        Expression::Rule(lhs, rhs) => {
            refs.extend(collect_nonterminal_refs(lhs));
            if let Some(r) = rhs {
                refs.extend(collect_nonterminal_refs(r));
            }
        }
        _ => {} // Literal, Regex, Epsilon, MappingFn
    }
    refs
}

/// Resolve an import path relative to the importing file's directory.
fn resolve_import_path(dir: &Path, import_path: &str) -> PathBuf {
    let mut path = dir.join(import_path);
    // Append .bbnf if no extension given.
    if path.extension().is_none() {
        path.set_extension("bbnf");
    }
    path
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    fn setup_test_dir() -> tempfile::TempDir {
        tempfile::tempdir().expect("Failed to create temp dir")
    }

    #[test]
    fn test_glob_import() {
        let dir = setup_test_dir();
        let base_path = dir.path().join("base.bbnf");
        let main_path = dir.path().join("main.bbnf");

        fs::write(&base_path, "number = /[0-9]+/;\nstring = /[a-z]+/;").unwrap();
        fs::write(&main_path, r#"@import "base.bbnf";
value = number | string;"#).unwrap();

        let registry = load_module_graph(&main_path).unwrap();
        assert!(registry.errors.is_empty(), "Errors: {:?}", registry.errors);

        let imported = registry.imported_rule_names(&main_path.canonicalize().unwrap());
        assert!(imported.contains("number"));
        assert!(imported.contains("string"));
    }

    #[test]
    fn test_selective_import() {
        let dir = setup_test_dir();
        let base_path = dir.path().join("base.bbnf");
        let main_path = dir.path().join("main.bbnf");

        fs::write(&base_path, "number = /[0-9]+/;\nstring = /[a-z]+/;\nident = /[a-zA-Z_]+/;").unwrap();
        fs::write(&main_path, r#"@import { number, string } from "base.bbnf";
value = number | string;"#).unwrap();

        let registry = load_module_graph(&main_path).unwrap();
        assert!(registry.errors.is_empty(), "Errors: {:?}", registry.errors);

        let imported = registry.imported_rule_names(&main_path.canonicalize().unwrap());
        assert!(imported.contains("number"));
        assert!(imported.contains("string"));
        assert!(!imported.contains("ident"), "ident should not be imported");
    }

    #[test]
    fn test_diamond_dependency() {
        let dir = setup_test_dir();
        let common = dir.path().join("common.bbnf");
        let a = dir.path().join("a.bbnf");
        let b = dir.path().join("b.bbnf");
        let main = dir.path().join("main.bbnf");

        fs::write(&common, "ws = /\\s+/;").unwrap();
        fs::write(&a, r#"@import "common.bbnf";
a = ws;"#).unwrap();
        fs::write(&b, r#"@import "common.bbnf";
b = ws;"#).unwrap();
        fs::write(&main, r#"@import "a.bbnf";
@import "b.bbnf";
entry = a | b;"#).unwrap();

        let registry = load_module_graph(&main).unwrap();
        // common.bbnf should be loaded only once (dedup).
        assert!(registry.errors.is_empty(), "Errors: {:?}", registry.errors);
    }

    #[test]
    fn test_circular_import_allowed() {
        let dir = setup_test_dir();
        let a = dir.path().join("a.bbnf");
        let b = dir.path().join("b.bbnf");

        fs::write(&a, r#"@import "b.bbnf";
a = /x/;"#).unwrap();
        fs::write(&b, r#"@import "a.bbnf";
b = /y/;"#).unwrap();

        let registry = load_module_graph(&a).unwrap();
        // Cyclic imports are now allowed (Python-style partial-init).
        assert!(registry.errors.is_empty(), "Errors: {:?}", registry.errors);
        // Both modules should be loaded.
        let a_canon = a.canonicalize().unwrap();
        let b_canon = b.canonicalize().unwrap();
        assert!(registry.get_module(&a_canon).is_some(), "Module a should be loaded");
        assert!(registry.get_module(&b_canon).is_some(), "Module b should be loaded");
        // A can see B's rules.
        let imported = registry.imported_rule_names(&a_canon);
        assert!(imported.contains("b"), "A should see B's rule 'b'");
    }

    #[test]
    fn test_missing_file() {
        let dir = setup_test_dir();
        let main = dir.path().join("main.bbnf");
        fs::write(&main, r#"@import "nonexistent.bbnf";
value = /x/;"#).unwrap();

        let registry = load_module_graph(&main).unwrap();
        let file_errors: Vec<_> = registry.errors.iter().filter(|e| matches!(e, ImportError::FileNotFound { .. })).collect();
        assert!(!file_errors.is_empty(), "Expected file not found error");
    }

    #[test]
    fn test_missing_rule_in_selective_import() {
        let dir = setup_test_dir();
        let base = dir.path().join("base.bbnf");
        let main = dir.path().join("main.bbnf");

        fs::write(&base, "number = /[0-9]+/;").unwrap();
        fs::write(&main, r#"@import { number, nonexistent } from "base.bbnf";
value = number;"#).unwrap();

        let registry = load_module_graph(&main).unwrap();
        let missing_errors: Vec<_> = registry.errors.iter().filter(|e| matches!(e, ImportError::MissingRule { .. })).collect();
        assert!(!missing_errors.is_empty(), "Expected missing rule error");
    }

    #[test]
    fn test_non_transitive() {
        let dir = setup_test_dir();
        let c = dir.path().join("c.bbnf");
        let b = dir.path().join("b.bbnf");
        let a = dir.path().join("a.bbnf");

        fs::write(&c, "c_rule = /c/;").unwrap();
        fs::write(&b, r#"@import "c.bbnf";
b_rule = c_rule;"#).unwrap();
        fs::write(&a, r#"@import "b.bbnf";
a_rule = b_rule;"#).unwrap();

        let registry = load_module_graph(&a).unwrap();
        let imported = registry.imported_rule_names(&a.canonicalize().unwrap());
        // A imports B, B imports C. A should see b_rule but NOT c_rule.
        assert!(imported.contains("b_rule"), "Should import b_rule");
        assert!(!imported.contains("c_rule"), "Should NOT see c_rule (non-transitive)");
    }

    #[test]
    fn test_three_way_cycle() {
        let dir = setup_test_dir();
        let a = dir.path().join("a.bbnf");
        let b = dir.path().join("b.bbnf");
        let c = dir.path().join("c.bbnf");

        fs::write(&a, r#"@import "b.bbnf";
a = /x/;"#).unwrap();
        fs::write(&b, r#"@import "c.bbnf";
b = /y/;"#).unwrap();
        fs::write(&c, r#"@import "a.bbnf";
c = /z/;"#).unwrap();

        let registry = load_module_graph(&a).unwrap();
        assert!(registry.errors.is_empty(), "Errors: {:?}", registry.errors);
        assert!(registry.get_module(&a.canonicalize().unwrap()).is_some());
        assert!(registry.get_module(&b.canonicalize().unwrap()).is_some());
        assert!(registry.get_module(&c.canonicalize().unwrap()).is_some());
    }

    #[test]
    fn test_self_import() {
        let dir = setup_test_dir();
        let a = dir.path().join("a.bbnf");
        fs::write(&a, r#"@import "a.bbnf";
a = /x/;"#).unwrap();

        let registry = load_module_graph(&a).unwrap();
        assert!(registry.errors.is_empty(), "Errors: {:?}", registry.errors);
        assert!(registry.get_module(&a.canonicalize().unwrap()).is_some());
    }

    #[test]
    fn test_selective_transitive_unfurling() {
        let dir = setup_test_dir();
        let base = dir.path().join("base.bbnf");
        let main = dir.path().join("main.bbnf");

        // percentage depends on number and percentageUnit
        fs::write(&base, r#"number = /[0-9]+/;
percentageUnit = "%";
percentage = number, percentageUnit;"#).unwrap();
        fs::write(&main, r#"@import { percentage } from "base.bbnf";
value = percentage;"#).unwrap();

        let registry = load_module_graph(&main).unwrap();
        assert!(registry.errors.is_empty(), "Errors: {:?}", registry.errors);

        let imported = registry.imported_rule_names(&main.canonicalize().unwrap());
        assert!(imported.contains("percentage"), "Should import percentage");
        assert!(imported.contains("number"), "Should unfurl transitive dep: number");
        assert!(imported.contains("percentageUnit"), "Should unfurl transitive dep: percentageUnit");
    }
}
