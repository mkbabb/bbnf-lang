//! View emission for `MustTape` Alt rules.
//!
//! An Alt rule's view exposes `.as_<variant>()` discriminated
//! accessors that test `variant_idx` and return the chosen branch's
//! child view wrapped in `Option`. Heterogeneous Alts with
//! sub-variant coercion also generate sub-variant accessors.

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

/// Emit typed Alt variant accessors for a rule whose body is an `Alt`.
///
/// Returns an `impl` block with `.as_<variant>()` methods that
/// dispatch on `variant_idx` and `.is_<variant>()` discriminator
/// predicates.
pub fn emit_alt_accessors(
    rule: &IrRule,
    rule_name: &str,
    ir: &GrammarIR,
    grammar_name: &str,
) -> TokenStream {
    let view_ident = format_ident!("{}View", rule_name);
    let node_view_ident = format_ident!("{}NodeView", grammar_name);

    let branches = match &rule.body {
        IrNode::Alt(branches, _) => branches,
        _ => return quote! {},
    };

    let mut methods = Vec::new();

    // Per-branch accessors based on the branch's inner shape.
    for (branch_idx, branch) in branches.iter().enumerate() {
        let idx_u8 = branch_idx as u8;

        // Try to derive a meaningful name from the branch node.
        let (variant_name, view_ty) =
            resolve_branch_identity(&branch.node, branch_idx, ir, grammar_name);

        let as_ident = format_ident!("as_{}", variant_name);
        let is_ident = format_ident!("is_{}", variant_name);
        let doc_as = format!(
            "If variant `{}` (branch {}) was chosen, return its child view.",
            variant_name, branch_idx
        );
        let doc_is = format!(
            "Returns `true` if variant `{}` (branch {}) was chosen.",
            variant_name, branch_idx
        );

        methods.push(quote! {
            #[doc = #doc_as]
            #[inline]
            pub fn #as_ident(&self) -> ::core::option::Option<#view_ty<'p>> {
                if self.variant_idx() == #idx_u8 {
                    self.cursor.child(0).map(|c| #view_ty::from_cursor(c, self.input))
                } else {
                    None
                }
            }

            #[doc = #doc_is]
            #[inline]
            pub fn #is_ident(&self) -> bool {
                self.variant_idx() == #idx_u8
            }
        });
    }

    // Sub-variant accessors from heterogeneous coercion.
    for sv in &rule.meta.sub_variants {
        let sv_name = ir.get_string(sv.variant_name);
        let as_ident = format_ident!("as_{}", sv_name);
        let is_ident = format_ident!("is_{}", sv_name);

        // Sub-variants use the NodeView since they may wrap
        // different structural types.
        let doc_sv = format!(
            "If sub-variant `{}` was chosen (branch {}), return its child view.",
            sv_name, sv.branch_index
        );
        let branch_idx = sv.branch_index as u8;

        methods.push(quote! {
            #[doc = #doc_sv]
            #[inline]
            pub fn #as_ident(&self) -> ::core::option::Option<#node_view_ident<'p>> {
                if self.variant_idx() == #branch_idx {
                    self.cursor.child(0).map(|c| #node_view_ident::from_cursor(c, self.input))
                } else {
                    None
                }
            }

            #[inline]
            pub fn #is_ident(&self) -> bool {
                self.variant_idx() == #branch_idx
            }
        });
    }

    // `.chosen()` — generic accessor that always returns the single
    // child regardless of which branch was taken.
    methods.push(quote! {
        /// The chosen branch's child as a generic node view,
        /// regardless of which variant was selected.
        #[inline]
        pub fn chosen(&self) -> ::core::option::Option<#node_view_ident<'p>> {
            self.cursor.child(0).map(|c| #node_view_ident::from_cursor(c, self.input))
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

/// Derive a branch name and view type from the branch node shape.
///
/// - `Ref(rule_id)` → rule name + `<Rule>View`
/// - `Map { inner: Ref(..), .. }` → rule name + `<Rule>View`
/// - Fallback → `branch_<idx>` + generic `NodeView`
fn resolve_branch_identity(
    node: &IrNode,
    branch_idx: usize,
    ir: &GrammarIR,
    grammar_name: &str,
) -> (String, proc_macro2::Ident) {
    let inner = peel_map_wrappers(node);
    match inner {
        IrNode::Ref(rule_id) => {
            let target = &ir.rules[*rule_id as usize];
            if target.meta.is_transparent {
                let fallback_name = format!("branch_{}", branch_idx);
                let nv = format_ident!("{}NodeView", grammar_name);
                (fallback_name, nv)
            } else {
                let name = ir.get_string(target.name);
                let view_ident = format_ident!("{}View", name);
                (name.to_string(), view_ident)
            }
        }
        _ => {
            let fallback_name = format!("branch_{}", branch_idx);
            let nv = format_ident!("{}NodeView", grammar_name);
            (fallback_name, nv)
        }
    }
}

/// Peel through Map and OptionalWhitespace to find the inner node.
fn peel_map_wrappers(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            peel_map_wrappers(inner)
        }
        other => other,
    }
}
