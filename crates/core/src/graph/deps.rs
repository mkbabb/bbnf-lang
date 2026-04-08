//! Dependency graph construction from grammar AST.

use std::collections::{HashMap, HashSet};

use crate::grammar::generated::{BbnfBootstrapEnum, BbnfBootstrapEnumVisitor};
use crate::types::AST;

/// Rule name → set of referenced rule names.
pub type Dependencies<'a> = HashMap<&'a str, HashSet<&'a str>>;

/// Build a dependency graph from the grammar AST.
pub fn calculate_ast_deps<'a>(ast: &AST<'a>) -> Dependencies<'a> {
    let mut deps = HashMap::new();
    for (&name, entry) in ast.iter() {
        let mut collector = RefCollector {
            refs: HashSet::new(),
        };
        collector.visit(entry.rhs);
        deps.insert(name, collector.refs);
    }
    deps
}

/// Visitor that collects nonterminal identifier references from a grammar RHS.
struct RefCollector<'a> {
    refs: HashSet<&'a str>,
}

impl<'a> BbnfBootstrapEnumVisitor<'a> for RefCollector<'a> {
    type Output = ();

    fn visit(&mut self, node: &'a BbnfBootstrapEnum<'a>) {
        match node {
            // Identifier reference: collect the name.
            BbnfBootstrapEnum::identifier(s) => {
                self.refs.insert(s.as_str());
            }
            // term_1: identifier with optional call args.
            BbnfBootstrapEnum::term_1((ident, _call_args)) => {
                self.refs.insert(crate::grammar::generated::BbnfBootstrapEnum::span_text(ident));
                self.walk(node);
            }
            // All other variants: delegate to generated walk for structural recursion.
            //
            // Note: value expression sub-variants share types with grammar
            // sub-variants (e.g., `value_atom_0` is also used for `(rhs)` parens
            // in `term`), so we cannot skip them by variant name.
            // The walk safely descends; identifier nodes inside value expressions
            // will be collected, but value expressions don't contain grammar refs
            // in valid grammars.
            _ => {
                self.walk(node);
            }
        }
    }
}

/// Recursively collect nonterminal (identifier) references from a bootstrap AST node.
///
/// Backward-compat wrapper around the visitor-based implementation.
pub fn collect_nonterminal_refs<'a>(
    node: &'a BbnfBootstrapEnum<'a>,
    refs: &mut HashSet<&'a str>,
) {
    let mut collector = RefCollector {
        refs: std::mem::take(refs),
    };
    collector.visit(node);
    *refs = collector.refs;
}
