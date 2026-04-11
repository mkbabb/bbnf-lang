//! Dependency graph construction from grammar AST.
//!
//! Tranche AC.2: rewritten against the tape-first view surface.
//! Identifier references are collected by walking `children()` on a
//! [`BbnfBootstrapNodeView`] and matching on [`BbnfBootstrapRuleKind`].
//!
//! Tranche AF.0: shape-agnostic structural walk — every semantic
//! handler dispatches on canonical `rule_kind` names only. Sub-
//! variant wrapper kinds (`term_N`) that structural-mode dedup may
//! or may not produce are never referenced; the generic descent
//! into `children()` collects identifier nodes regardless of which
//! wrapper compounds the optimizer has elided around them.

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
/// The walk dispatches on canonical `rule_kind` only:
///
/// - `identifier` → record the name as a reference.
/// - anything else → recurse into `children()`.
///
/// This covers every grammar-level term shape without naming any
/// dedup-collapsible sub-variant wrapper. A `term` compound that
/// under structural mode has children `(identifier, call_args?)`
/// still has its head `identifier` collected by the generic recursion
/// in a single pass (the identifier is a direct child and its
/// rule_kind matches the first arm); any nested references inside
/// the call args are collected by the recursive descent.
///
/// Value expression sub-variants share types with grammar
/// sub-variants (e.g., paren-grouped shapes), so we cannot skip
/// them by variant name — the walk safely descends and identifier
/// nodes inside value expressions will be collected, but in valid
/// grammars value expressions don't contain grammar references.
pub fn collect_nonterminal_refs<'a>(
    node: BbnfBootstrapNodeView<'a>,
    refs: &mut IndexSet<&'a str>,
) {
    if node.rule_kind() == BbnfBootstrapRuleKind::identifier {
        refs.insert(node.span_text());
        return;
    }
    for child in node.children() {
        collect_nonterminal_refs(child, refs);
    }
}
