//! Module registry types and accessors.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::types::ParsedGrammar;

use super::errors::ImportError;

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

/// Recorded cycle encountered during module loading.
#[derive(Debug, Clone)]
pub struct ImportCycle {
    /// The path that closes the cycle.
    pub path: PathBuf,
    /// The active load chain when the cycle was encountered.
    pub chain: Vec<PathBuf>,
}

/// Registry of all loaded modules in an import graph.
#[derive(Debug)]
pub struct ModuleRegistry {
    /// Canonical path -> module data.
    pub(super) modules: HashMap<PathBuf, ModuleData>,
    /// Canonical path -> resolved imports (which rules are visible from imports).
    pub(super) resolved_imports: HashMap<PathBuf, Vec<ResolvedImport>>,
    /// All errors encountered during loading.
    pub errors: Vec<ImportError>,
    /// Import cycles encountered during loading.
    pub cycles: Vec<ImportCycle>,
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
