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
/// points at. The discriminator is the variant's `rule_id` (the
/// same value `emit_rule_function_impl` stamps into the compound
/// record's `variant_idx` byte via `rule.id as u8`). Sub-variants
/// (heterogeneous-alt coercion) return `None` — they are never
/// the top-level variant_idx of a rule record.
#[inline]
pub(super) fn variant_idx_for(schema: &CstSchema, variant_name: &str) -> Option<u8> {
    let variant = schema.variants.iter().find(|v| v.name == variant_name)?;
    let rule_id = variant.rule_id?;
    u8::try_from(rule_id & 0xFF).ok()
}

/// Find a variant by category + rule name. Returns the variant and
/// its `rule_id`-based `variant_idx`.
#[inline]
pub(super) fn find_variant<'a>(
    schema: &'a CstSchema,
    rule_name: &str,
) -> Option<(u8, &'a VariantDescriptor)> {
    let variant = schema.variants.iter().find(|v| v.name == rule_name)?;
    let rule_id = variant.rule_id?;
    let idx = u8::try_from(rule_id & 0xFF).ok()?;
    Some((idx, variant))
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

