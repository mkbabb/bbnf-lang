mod ast_utils;
mod diagnostics;
mod parsing;
pub mod types;

pub use ast_utils::compute_expression_end_pub;
pub use types::*;

#[cfg(test)]
pub use diagnostics::analyze;

use std::collections::HashSet;

use bbnf::analysis::{calculate_ast_deps, get_nonterminal_name};
use bbnf::types::{Expression, AST};

use crate::analysis::LineIndex;

use diagnostics::analyze_from_cache;
use parsing::{OwnedAst, parse_once};

/// Stored per-document: raw text + analyzed info + precomputed line index + cached AST.
pub struct DocumentState {
    pub text: String,
    pub info: DocumentInfo,
    /// Precomputed line-start offsets for O(log n) position conversion.
    pub line_index: LineIndex,
    /// Cached parsed AST (self-referential: borrows from its own String).
    ast_cell: OwnedAst,
    /// Cached rule names from previous analysis (for structural change detection).
    prev_rule_names: Option<Vec<String>>,
    /// Cached dependency graph from previous analysis (for dep graph diffing).
    prev_dep_edges: Option<HashSet<(String, String)>>,
}

impl DocumentState {
    pub fn new(text: String) -> Self {
        let line_index = LineIndex::new(&text);
        // Parse once: the OwnedAst captures the AST, and we extract diagnostics
        // via a Cell to avoid double-parsing.
        let parse_diag = std::cell::Cell::new(None::<ParseDiagnostics>);
        let ast_cell = OwnedAst::new(text.clone(), |src| {
            let (cached, diag) = parse_once(src);
            parse_diag.set(Some(diag));
            cached
        });
        let diag = parse_diag.into_inner().unwrap();

        // Cache rule names and dep edges for incremental updates.
        let (rule_names, dep_edges) = ast_cell
            .borrow_dependent()
            .as_ref()
            .map(|cached| {
                let names: Vec<String> = cached
                    .ast
                    .keys()
                    .filter_map(|k| {
                        if let Expression::Nonterminal(tok) = k {
                            Some(tok.value.to_string())
                        } else {
                            None
                        }
                    })
                    .collect();

                let deps = calculate_ast_deps(&cached.ast);
                let mut edges = HashSet::new();
                for (lhs, dep_set) in &deps {
                    if let Some(lhs_name) = get_nonterminal_name(lhs) {
                        for dep in dep_set {
                            if let Some(dep_name) = get_nonterminal_name(dep) {
                                edges.insert((lhs_name.to_string(), dep_name.to_string()));
                            }
                        }
                    }
                }
                (Some(names), Some(edges))
            })
            .unwrap_or((None, None));

        let info = analyze_from_cache(
            &text,
            &line_index,
            ast_cell.borrow_dependent().as_ref(),
            &diag,
        );
        Self {
            text,
            info,
            line_index,
            ast_cell,
            prev_rule_names: rule_names,
            prev_dep_edges: dep_edges,
        }
    }

    pub fn update(&mut self, text: String) {
        // Fast path: if text unchanged from the last parsed source, skip everything.
        // Compare against the ast_cell's owned string (the source the AST was built from),
        // not self.text, because the server's did_change handler may have already mutated
        // self.text via apply_incremental_changes before calling update().
        if *self.ast_cell.borrow_owner() == text {
            return;
        }

        let line_index = LineIndex::new(&text);
        let parse_diag = std::cell::Cell::new(None::<ParseDiagnostics>);
        let ast_cell = OwnedAst::new(text.clone(), |src| {
            let (cached, diag) = parse_once(src);
            parse_diag.set(Some(diag));
            cached
        });
        let diag = parse_diag.into_inner().unwrap();

        // Check for structural changes (rule count/names differ).
        let new_rule_names: Vec<String> = ast_cell
            .borrow_dependent()
            .as_ref()
            .map(|cached| {
                cached
                    .ast
                    .keys()
                    .filter_map(|k| {
                        if let Expression::Nonterminal(tok) = k {
                            Some(tok.value.to_string())
                        } else {
                            None
                        }
                    })
                    .collect()
            })
            .unwrap_or_default();

        let _structural_change = self
            .prev_rule_names
            .as_ref()
            .map(|prev| prev != &new_rule_names)
            .unwrap_or(true);

        // Always do full analysis for now (the caching of prev_rule_names
        // and prev_dep_edges enables future selective re-analysis optimizations).
        self.info = analyze_from_cache(
            &text,
            &line_index,
            ast_cell.borrow_dependent().as_ref(),
            &diag,
        );

        // Cache for next update.
        self.prev_rule_names = Some(new_rule_names);
        // Build dep edge set for next update's diffing.
        if let Some(cached) = ast_cell.borrow_dependent().as_ref() {
            let deps = calculate_ast_deps(&cached.ast);
            let mut edges = HashSet::new();
            for (lhs, dep_set) in &deps {
                if let Some(lhs_name) = get_nonterminal_name(lhs) {
                    for dep in dep_set {
                        if let Some(dep_name) = get_nonterminal_name(dep) {
                            edges.insert((lhs_name.to_string(), dep_name.to_string()));
                        }
                    }
                }
            }
            self.prev_dep_edges = Some(edges);
        }

        self.line_index = line_index;
        self.ast_cell = ast_cell;
        self.text = text;
    }

    /// Access the cached AST (borrows from internally-owned text).
    pub fn ast(&self) -> Option<&AST<'_>> {
        self.ast_cell.borrow_dependent().as_ref().map(|c| &c.ast)
    }
}
