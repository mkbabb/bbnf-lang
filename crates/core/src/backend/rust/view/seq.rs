//! View emission for `MustTape` Seq rules.
//!
//! A Seq rule's view wraps a compound `TapeCursor<'p>` whose
//! children are the individually-pushed sub-records. This module
//! generates typed positional child accessors (`.child_0()`,
//! `.child_1()`, ...) that return the appropriate view type for
//! each position, plus named accessors (`.identifier()`,
//! `.expression()`, ...) when the child is a `Ref` to a named rule.

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

/// Emit typed Seq child accessors for a rule whose body is a `Seq`.
///
/// Returns an `impl` block with `.child_N()` positional accessors
/// and, when available, named accessors derived from `Ref` targets.
pub fn emit_seq_accessors(
    rule: &IrRule,
    rule_name: &str,
    ir: &GrammarIR,
    grammar_name: &str,
) -> TokenStream {
    let view_ident = format_ident!("{}View", rule_name);

    let children = match &rule.body {
        IrNode::Seq(children) => children,
        _ => return quote! {},
    };

    // Collect the effective children: Skip/Next expose the kept side,
    // Map exposes the inner, OptionalWhitespace is skipped.
    let effective: Vec<(usize, &IrNode)> = children
        .iter()
        .enumerate()
        .filter(|(_, child)| !matches!(child, IrNode::OptionalWhitespace(_)))
        .collect();

    if effective.is_empty() {
        return quote! {};
    }

    let mut methods = Vec::new();
    let mut seen_names: std::collections::HashSet<String> = Default::default();

    for (tape_idx, &(_, child)) in effective.iter().enumerate() {
        let idx_lit = tape_idx;
        let positional_ident = format_ident!("child_{}", tape_idx);

        // Determine child view type based on the child node shape.
        let (child_view_ty, named_ident) = resolve_child_view(child, ir, grammar_name);

        let doc_pos = format!("Child at position {} as a typed view.", tape_idx);
        methods.push(quote! {
            #[doc = #doc_pos]
            #[inline]
            pub fn #positional_ident(&self) -> ::core::option::Option<#child_view_ty<'p>> {
                self.cursor.child(#idx_lit).map(|c| #child_view_ty::from_cursor(c, self.input))
            }
        });

        // Named accessor when the child is a Ref to a named rule.
        if let Some(name) = named_ident {
            if seen_names.insert(name.clone()) {
                let name_ident = format_ident!("{}", name);
                let (named_view_ty, _) = resolve_child_view(child, ir, grammar_name);
                let doc_named = format!("The `{}` child as a typed view.", name);
                methods.push(quote! {
                    #[doc = #doc_named]
                    #[inline]
                    pub fn #name_ident(&self) -> ::core::option::Option<#named_view_ty<'p>> {
                        self.cursor.child(#idx_lit).map(|c| #named_view_ty::from_cursor(c, self.input))
                    }
                });
            }
        }
    }

    // `.num_children()` convenience.
    let count = effective.len();
    methods.push(quote! {
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            #count
        }
    });

    if methods.is_empty() {
        return quote! {};
    }

    quote! {
        #[allow(dead_code)]
        impl<'p> #view_ident<'p> {
            #(#methods)*
        }
    }
}

/// Resolve the view type ident and optional named accessor for a
/// child node. Returns `(ViewTypeIdent, Option<accessor_name>)`.
///
/// - `Ref(rule_id)` → `<RuleName>View` + `Some("rule_name")`
/// - Everything else → `<Grammar>NodeView` + `None`
fn resolve_child_view(
    node: &IrNode,
    ir: &GrammarIR,
    grammar_name: &str,
) -> (proc_macro2::Ident, Option<String>) {
    // Peel through Map/OptionalWhitespace/Skip/Next to find the
    // innermost structurally meaningful node.
    let inner = peel_wrappers(node);

    match inner {
        IrNode::Ref(rule_id) => {
            let target_rule = &ir.rules[*rule_id as usize];
            if target_rule.meta.is_transparent {
                // Transparent rules don't have their own view type;
                // fall back to the generic NodeView.
                let nv = format_ident!("{}NodeView", grammar_name);
                (nv, None)
            } else {
                let name = ir.get_string(target_rule.name);
                let view_ident = format_ident!("{}View", name);
                (view_ident, Some(name.to_string()))
            }
        }
        _ => {
            let nv = format_ident!("{}NodeView", grammar_name);
            (nv, None)
        }
    }
}

/// Peel through Map, OptionalWhitespace, Skip (keep left), and
/// Next (keep right) wrappers to find the structurally meaningful
/// inner node.
fn peel_wrappers(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. }
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Next(_, inner)
        | IrNode::Negate(inner) => peel_wrappers(inner),
        IrNode::Skip(kept, _) => peel_wrappers(kept),
        other => other,
    }
}
