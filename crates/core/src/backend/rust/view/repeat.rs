//! View emission for `MustTape` Repeat rules.
//!
//! A Repeat rule's view exposes `.iter()` — an iterator over child
//! cursors wrapped in the appropriate view type — and `.len()` for
//! the element count. When the repeat body is a `Ref` to a named
//! rule, the iterator yields that rule's typed view; otherwise it
//! yields `NodeView`.

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

/// Emit typed Repeat accessors for a rule whose body is a `Repeat`.
///
/// Returns an `impl` block with `.iter()`, `.len()`, `.is_empty()`,
/// and `.get(i)` methods.
pub fn emit_repeat_accessors(
    rule: &IrRule,
    rule_name: &str,
    ir: &GrammarIR,
    grammar_name: &str,
) -> TokenStream {
    let view_ident = format_ident!("{}View", rule_name);

    let inner = match &rule.body {
        IrNode::Repeat { inner, .. } => inner.as_ref(),
        _ => return quote! {},
    };

    // Determine the element view type from the repeat body.
    let elem_view_ident = resolve_elem_view(inner, ir, grammar_name);

    quote! {
        impl<'p> #view_ident<'p> {
            /// Iterator over each repetition element as a typed view.
            #[inline]
            pub fn iter(&self) -> impl ::core::iter::Iterator<Item = #elem_view_ident<'p>> + 'p {
                let input = self.input;
                self.cursor.children().map(move |c| #elem_view_ident::from_cursor(c, input))
            }

            /// The number of elements in this repetition.
            #[inline]
            pub fn len(&self) -> usize {
                self.cursor.child_count()
            }

            /// Whether this repetition matched zero elements.
            #[inline]
            pub fn is_empty(&self) -> bool {
                self.len() == 0
            }

            /// The i-th element as a typed view, if present.
            #[inline]
            pub fn get(&self, i: usize) -> ::core::option::Option<#elem_view_ident<'p>> {
                self.cursor.child(i).map(|c| #elem_view_ident::from_cursor(c, self.input))
            }
        }
    }
}

/// Resolve the element view type from the repeat inner node.
///
/// - `Ref(rule_id)` → `<Rule>View`
/// - `Map { inner: Ref(..), .. }` → `<Rule>View`
/// - Fallback → `<Grammar>NodeView`
fn resolve_elem_view(node: &IrNode, ir: &GrammarIR, grammar_name: &str) -> proc_macro2::Ident {
    let inner = peel_repeat_wrappers(node);
    match inner {
        IrNode::Ref(rule_id) => {
            let target = &ir.rules[*rule_id as usize];
            if target.meta.is_transparent {
                format_ident!("{}NodeView", grammar_name)
            } else {
                let name = ir.get_string(target.name);
                format_ident!("{}View", name)
            }
        }
        _ => format_ident!("{}NodeView", grammar_name),
    }
}

/// Peel through Map, OptionalWhitespace, Skip, Next wrappers.
fn peel_repeat_wrappers(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. }
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Next(_, inner)
        | IrNode::Negate(inner) => peel_repeat_wrappers(inner),
        IrNode::Skip(kept, _) => peel_repeat_wrappers(kept),
        other => other,
    }
}
