//! Module registry types and accessors.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use super::errors::ImportError;
use crate::pipeline::directives::ModulePipelineData;

/// Per-file module data after parsing.
///
/// Tranche AU.4.1: the pre-existing `ParsedGrammar` grab bag was
/// excised from the compile hot path. Each module now holds its
/// pipeline-shaped outputs directly in [`ModulePipelineData`] —
/// `(AST, DirectiveMaps, imports)` — skipping the observational
/// `GrammarExtract` middle step entirely.
pub struct ModuleData {
    /// Pipeline-shaped parse: AST + directive maps + imports list.
    pub(crate) data: ModulePipelineData,
}

impl ModuleData {
    /// Wrap pipeline-shaped parse outputs for storage in the registry.
    pub(crate) fn from_pipeline_data(data: ModulePipelineData) -> Self {
        Self { data }
    }

    /// Access the pipeline-shaped parse outputs.
    pub(crate) fn pipeline_data(&self) -> &ModulePipelineData {
        &self.data
    }

    /// Names of rules defined locally in this file.
    pub fn local_rule_names(&self) -> &[String] {
        &self.data.local_rule_names
    }
}

impl std::fmt::Debug for ModuleData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ModuleData")
            .field("local_rule_names", &self.data.local_rule_names)
            .field("source_len", &self.data.source.len())
            .field("rules", &self.data.ast.len())
            .field("imports", &self.data.imports.len())
            .finish()
    }
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
