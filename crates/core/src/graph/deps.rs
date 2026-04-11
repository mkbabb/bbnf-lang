//! Dependency graph construction from grammar AST.
//!
//! Tranche AC.2: rewritten against the tape-first view surface.
//! Identifier references are collected by walking `children()` on a
//! [`BbnfBootstrapNodeView`] and matching on [`BbnfBootstrapRuleKind`].

use indexmap::{IndexMap, IndexSet};

use crate::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};
use crate::types::AST;

/// Rule name → set of referenced rule names.
///
/// `IndexMap` and `IndexSet` are used (not `HashMap`/`HashSet`)
/// because both Tarjan SCC and Kahn topological sort iterate this
/// graph and the iteration order influences the order in which
/// rules end up in the lowered IR — and therefore the order of
/// generated enum variants. Insertion-order semantics keep
/// codegen output byte-stable across runs.
pub type Dependencies<'a> = IndexMap<&'a str, IndexSet<&'a str>>;

/// Build a dependency graph from the grammar AST.
pub fn calculate_ast_deps<'a>(ast: &AST<'a>) -> Dependencies<'a> {
    let mut deps = Dependencies::new();
    for (&name, entry) in ast.iter() {
        let mut refs = IndexSet::new();
        collect_nonterminal_refs(entry.rhs, &mut refs);
        deps.insert(name, refs);
    }
    deps
}

/// Recursively collect nonterminal (identifier) references from a
/// bootstrap view node.
///
/// Value expression sub-variants share types with grammar
/// sub-variants (e.g., `value_atom_0` is also used for `(rhs)` parens
/// in `term`), so we cannot skip them by variant name — the walk
/// safely descends and identifier nodes inside value expressions
/// will be collected, but in valid grammars value expressions don't
/// contain grammar references.
pub fn collect_nonterminal_refs<'a>(
    node: BbnfBootstrapNodeView<'a>,
    refs: &mut IndexSet<&'a str>,
) {
    match node.rule_kind() {
        // Identifier reference: collect the name.
        BbnfBootstrapRuleKind::identifier => {
            refs.insert(node.span_text());
        }
        // term_1: identifier with optional call args. Collect the
        // head identifier, then walk the optional arg list for
        // nested references.
        BbnfBootstrapRuleKind::term_1 => {
            if let Some(ident) = node.child(0) {
                refs.insert(ident.span_text());
            }
            for child in node.children() {
                collect_nonterminal_refs(child, refs);
            }
        }
        // All other variants: delegate structural recursion.
        _ => {
            for child in node.children() {
                collect_nonterminal_refs(child, refs);
            }
        }
    }
}
