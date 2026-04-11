//! Shared helpers used across the Rust schema emitter sub-modules.
//!
//! Post-Tranche AC.2 rewrite: schema helpers emit impls on tape-backed
//! view types rather than on an owning enum. The helpers here resolve
//! the view ident for a rule, look up a rule's codegen-assigned
//! variant discriminator, and generate the small stereotypical
//! TokenStream fragments used by directive/identifier accessors.

#![allow(dead_code)]

use quote::format_ident;

use super::super::super::model::{CstSchema, VariantCategory, VariantDescriptor};

/// The codegen-assigned variant discriminator for a variant.
///
/// Rule records carry `variant_idx` on their tape record; schema
/// accessors match on this byte to decide which rule a cursor
/// points at. The discriminator is the variant's position within
/// `schema.variants` — non-transparent rules first (in declaration
/// order), then sub-variants, then `Recovered`, then `__Phantom`.
/// Agent 1's `generate_views` wires the same numbering into the
/// tape emission epilogues so the two halves line up.
#[inline]
pub(super) fn variant_idx_for(schema: &CstSchema, variant_name: &str) -> Option<u8> {
    let idx = schema
        .variants
        .iter()
        .position(|v| v.name == variant_name)?;
    u8::try_from(idx).ok()
}

/// Find a variant by category + rule name. Returns the variant and
/// its index in the schema variant table (the tape-level
/// `variant_idx`).
#[inline]
pub(super) fn find_variant<'a>(
    schema: &'a CstSchema,
    rule_name: &str,
) -> Option<(u8, &'a VariantDescriptor)> {
    let (i, v) = schema
        .variants
        .iter()
        .enumerate()
        .find(|(_, v)| v.name == rule_name)?;
    let idx = u8::try_from(i).ok()?;
    Some((idx, v))
}

/// Generate the `<RuleName>View` ident for a rule variant.
#[inline]
pub(super) fn view_ident_for(rule_name: &str) -> syn::Ident {
    format_ident!("{}View", rule_name)
}

/// The root rule is the first non-transparent, non-synthetic rule
/// variant in declaration order. Schema helper impls anchored on
/// "the grammar" (e.g. directive accessors) are emitted against
/// this view type.
pub(super) fn root_rule_name(schema: &CstSchema) -> Option<&str> {
    schema
        .variants
        .iter()
        .find(|v| {
            matches!(
                v.category,
                VariantCategory::Composite
                    | VariantCategory::Transparent
                    | VariantCategory::Terminal
                    | VariantCategory::Directive(_)
            )
        })
        .map(|v| v.name.as_str())
}

