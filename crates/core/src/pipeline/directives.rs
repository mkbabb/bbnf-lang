//! Pipeline-internal directive aggregation.
//!
//! `DirectiveMaps` is the compile-pipeline-facing container populated
//! directly from the BBNF struct-direct parse (see
//! [`crate::grammar::host::extract_for_pipeline`]). The walker lands
//! directive rows straight in this struct instead of round-tripping
//! through `Vec<ImportDirective>` / `Vec<RecoverDirective>` / …
//! aggregates.
//!
//! AZ-II.cutover.D — every directive whose payload is a body
//! expression (the recover-directive's sync expression) carries a
//! [`BbnfView`] focused on the body inside the owning
//! [`BbnfDocument`].

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use indexmap::IndexMap;

use crate::imports::load_module_graph;
use crate::lower::DirectiveSet;
use crate::pipeline::CompileError;
use crate::runtime::bbnf::{BbnfDocument, BbnfView};
use crate::types::{AST, ImportDirective, RuleEntry};

#[derive(Default)]
pub(crate) struct DirectiveMaps<'a> {
    recover_map: HashMap<String, BbnfView<'a, 'a>>,
    pretty_map: HashMap<String, Vec<String>>,
    ws_pattern: Option<String>,
    token_set: HashSet<String>,
    debug_set: HashSet<String>,
    debug_all: bool,
    host_map: HashMap<String, Option<String>>,
}

impl<'a> DirectiveMaps<'a> {
    // ---- mutators used by the host-side walker sink in `grammar::host` ----

    pub(crate) fn recover_map_mut(
        &mut self,
    ) -> &mut HashMap<String, BbnfView<'a, 'a>> {
        &mut self.recover_map
    }
    pub(crate) fn pretty_map_mut(&mut self) -> &mut HashMap<String, Vec<String>> {
        &mut self.pretty_map
    }
    pub(crate) fn token_set_mut(&mut self) -> &mut HashSet<String> {
        &mut self.token_set
    }
    pub(crate) fn debug_set_mut(&mut self) -> &mut HashSet<String> {
        &mut self.debug_set
    }
    pub(crate) fn host_map_mut(&mut self) -> &mut HashMap<String, Option<String>> {
        &mut self.host_map
    }
    pub(crate) fn set_debug_all(&mut self, value: bool) {
        self.debug_all = value;
    }
    pub(crate) fn set_ws_pattern(&mut self, value: String) {
        self.ws_pattern = Some(value);
    }

    pub(crate) fn as_directive_set(&self) -> DirectiveSet<'_> {
        DirectiveSet {
            recovers: (!self.recover_map.is_empty()).then_some(&self.recover_map),
            pretties: (!self.pretty_map.is_empty()).then_some(&self.pretty_map),
            ws_pattern: self.ws_pattern.as_deref(),
            token_rules: (!self.token_set.is_empty()).then_some(&self.token_set),
            debug_rules: (!self.debug_set.is_empty()).then_some(&self.debug_set),
            debug_all: self.debug_all,
            host_fns: (!self.host_map.is_empty()).then_some(&self.host_map),
        }
    }
}

/// Parse a grammar source string and immediately project it into
/// pipeline-shaped containers — AST, DirectiveMaps, imports — by
/// walking the BBNF struct-direct document exactly once.
///
/// Leaks the source so the returned containers can borrow `'static`
/// slices, matching the ownership contract the rest of the compile
/// pipeline already assumes (see `grammar::parse`).
pub(crate) fn parse_to_pipeline_inputs(
    source: &str,
) -> Option<(
    AST<'static>,
    DirectiveMaps<'static>,
    Vec<ImportDirective<'static>>,
)> {
    use crate::grammar::{self, host};

    let input: &'static str = Box::leak(source.to_owned().into_boxed_str());
    let document = grammar::generated::BbnfBootstrap::parse(input).ok()?;
    let document: &'static BbnfDocument<'static> = Box::leak(Box::new(document));
    Some(host::extract_for_pipeline(document))
}

/// Per-module data retained by the import graph loader. Replaces the
/// pre-AU.4.1 `ModuleData { grammar: ParsedGrammar }` shape: each
/// module now holds its pipeline-shaped outputs directly so
/// `merge_module` can splice them in without reparsing.
pub(crate) struct ModulePipelineData {
    pub(crate) source: String,
    pub(crate) ast: AST<'static>,
    pub(crate) directives: DirectiveMaps<'static>,
    pub(crate) imports: Vec<ImportDirective<'static>>,
    pub(crate) local_rule_names: Vec<String>,
}

pub(crate) struct MergedStaticGrammar {
    pub(crate) ast: AST<'static>,
    pub(crate) directives: DirectiveMaps<'static>,
}

pub(crate) fn load_merged_paths(paths: &[PathBuf]) -> Result<MergedStaticGrammar, CompileError> {
    if paths.is_empty() {
        return Err(CompileError::Import(
            "no grammar paths were provided to the derive pipeline".to_string(),
        ));
    }

    let mut ast = IndexMap::new();
    let mut directives = DirectiveMaps::default();

    for path in paths {
        let registry =
            load_module_graph(path).map_err(|err| CompileError::Import(err.to_string()))?;
        if !registry.errors.is_empty() {
            let msg = registry
                .errors
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("\n");
            return Err(CompileError::Import(msg));
        }

        let entry = path.canonicalize().unwrap_or_else(|_| path.clone());
        let mut module_paths: Vec<PathBuf> = registry.paths().cloned().collect();
        module_paths.sort();

        for module_path in module_paths.iter().filter(|candidate| **candidate != entry) {
            let module = registry.get_module(module_path).ok_or_else(|| {
                CompileError::Import(format!("missing module for `{}`", module_path.display()))
            })?;
            merge_module(module, &mut ast, &mut directives);
        }

        if let Some(module) = registry.get_module(&entry) {
            merge_module(module, &mut ast, &mut directives);
        } else {
            return Err(CompileError::Import(format!(
                "missing entry module for `{}`",
                entry.display()
            )));
        }
    }

    Ok(MergedStaticGrammar { ast, directives })
}

fn merge_module(
    module: &crate::imports::ModuleData,
    ast: &mut AST<'static>,
    directives: &mut DirectiveMaps<'static>,
) {
    let data = module.pipeline_data();
    // Splice directive maps — the loader already populated them
    // pipeline-shaped, so merging is a straight HashMap/HashSet union.
    for (k, v) in &data.directives.recover_map {
        directives.recover_map.insert(k.clone(), *v);
    }
    for (k, v) in &data.directives.pretty_map {
        directives.pretty_map.insert(k.clone(), v.clone());
    }
    if let Some(pat) = &data.directives.ws_pattern {
        directives.ws_pattern = Some(pat.clone());
    }
    for name in &data.directives.token_set {
        directives.token_set.insert(name.clone());
    }
    for (k, v) in &data.directives.host_map {
        directives.host_map.insert(k.clone(), v.clone());
    }
    if data.directives.debug_all {
        directives.debug_all = true;
    }
    for name in &data.directives.debug_set {
        directives.debug_set.insert(name.clone());
    }

    for (&name, entry) in &data.ast {
        ast.insert(name, RuleEntry {
            name_span: entry.name_span,
            rhs: entry.rhs,
        });
    }
}
