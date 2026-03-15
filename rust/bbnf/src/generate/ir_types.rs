//! IR type system bridge: TypeDesc → syn::Type conversion + IrCodegenCtx.
//!
//! Provides the context object and type conversion utilities consumed by
//! all IR codegen modules.

use std::collections::{HashMap, HashSet};

use bbnf_ir::{GrammarIR, RuleId, TypeDesc};

use syn::{parse_quote, Type};

use super::types::ParserAttributes;

/// Central context for IR-based code generation.
///
/// Holds all data needed by `ir_codegen`, `ir_span`, `ir_enums`, and `ir_pretty`
/// to emit TokenStream output. Replaces `GeneratedGrammarAttributes` + `CacheBundle`.
pub struct IrCodegenCtx<'a> {
    pub ir: &'a GrammarIR,
    /// Parser struct name (e.g., `Json`).
    pub ident: &'a syn::Ident,
    /// Enum name (e.g., `JsonEnum`).
    pub enum_ident: syn::Ident,
    /// `JsonEnum<'a>`.
    pub enum_type: Type,
    /// `Box<JsonEnum<'a>>`.
    pub boxed_enum_type: Type,
    /// Parser container attributes.
    pub parser_attrs: &'a ParserAttributes,
    /// Span-eligible rules that successfully produce `_sp()` methods.
    pub sp_method_rules: HashSet<String>,
    /// Pre-computed syn::Type per rule (from IR TypeDesc).
    pub rule_types: HashMap<RuleId, Type>,
}

impl<'a> IrCodegenCtx<'a> {
    /// Build context from GrammarIR + parser attributes.
    /// Type map is populated from `ir.types`.
    pub fn new(
        ir: &'a GrammarIR,
        ident: &'a syn::Ident,
        parser_attrs: &'a ParserAttributes,
    ) -> Self {
        let enum_ident = quote::format_ident!("{}Enum", ident);
        let enum_type: Type = parse_quote!(#enum_ident<'a>);
        let boxed_enum_type: Type = parse_quote!(Box<#enum_ident<'a>>);

        // Build type map from IR types, using a temporary ctx for conversion.
        let mut rule_types = HashMap::new();
        for (rule_id, type_desc) in &ir.types {
            let ty = type_desc_to_syn_raw(type_desc, &enum_type, &boxed_enum_type, ir);
            rule_types.insert(*rule_id, ty);
        }

        Self {
            ir,
            ident,
            enum_ident,
            enum_type,
            boxed_enum_type,
            parser_attrs,
            sp_method_rules: HashSet::new(),
            rule_types,
        }
    }

    /// Get the syn::Type for a rule's output.
    pub fn rule_return_type(&self, rule_id: RuleId) -> Type {
        let rule = &self.ir.rules[rule_id as usize];
        if rule.meta.is_transparent {
            self.boxed_enum_type.clone()
        } else {
            self.enum_type.clone()
        }
    }

    /// Get the inferred type for a rule (pre-codegen wrapping).
    pub fn rule_body_type(&self, rule_id: RuleId) -> Type {
        self.rule_types
            .get(&rule_id)
            .cloned()
            .unwrap_or_else(|| self.boxed_enum_type.clone())
    }

    /// Get the recover sentinel expression for a rule.
    pub fn recover_sentinel(&self, rule_id: RuleId) -> proc_macro2::TokenStream {
        let rule = &self.ir.rules[rule_id as usize];
        let enum_ident = &self.enum_ident;
        if rule.meta.is_transparent {
            quote::quote! { Box::new(#enum_ident::Recovered) }
        } else {
            quote::quote! { #enum_ident::Recovered }
        }
    }

    /// Check if a rule name is in the sp_method_rules set.
    pub fn has_sp_method(&self, name: &str) -> bool {
        self.sp_method_rules.contains(name)
    }

    /// Resolve a rule name, following aliases.
    pub fn resolve_rule_name(&self, rule_id: RuleId) -> &str {
        let rule = &self.ir.rules[rule_id as usize];
        if let Some(alias_id) = rule.meta.is_alias {
            self.ir.get_string(self.ir.rules[alias_id as usize].name)
        } else {
            self.ir.get_string(rule.name)
        }
    }
}

/// Convert an IR `TypeDesc` to a `syn::Type`.
pub fn type_desc_to_syn(desc: &TypeDesc, ctx: &IrCodegenCtx<'_>) -> Type {
    type_desc_to_syn_raw(desc, &ctx.enum_type, &ctx.boxed_enum_type, ctx.ir)
}

/// Convert TypeDesc → syn::Type without requiring full IrCodegenCtx (for bootstrapping).
fn type_desc_to_syn_raw(
    desc: &TypeDesc,
    enum_type: &Type,
    boxed_enum_type: &Type,
    ir: &GrammarIR,
) -> Type {
    match desc {
        TypeDesc::Span => parse_quote!(::parse_that::Span<'a>),
        TypeDesc::Option(inner) => {
            let inner = type_desc_to_syn_raw(inner, enum_type, boxed_enum_type, ir);
            parse_quote!(Option<#inner>)
        }
        TypeDesc::Vec(inner) => {
            let inner = type_desc_to_syn_raw(inner, enum_type, boxed_enum_type, ir);
            parse_quote!(Vec<#inner>)
        }
        TypeDesc::Tuple(elems) => {
            if elems.is_empty() {
                parse_quote!(())
            } else {
                let types: Vec<_> = elems
                    .iter()
                    .map(|e| type_desc_to_syn_raw(e, enum_type, boxed_enum_type, ir))
                    .collect();
                parse_quote!((#(#types),*))
            }
        }
        TypeDesc::BoxedEnum => boxed_enum_type.clone(),
        TypeDesc::Enum => enum_type.clone(),
        TypeDesc::Named(sid) => {
            let type_str = ir.get_string(*sid);
            syn::parse_str(type_str)
                .unwrap_or_else(|e| panic!("Failed to parse type `{}`: {}", type_str, e))
        }
    }
}

/// Check whether a TypeDesc is Span.
pub fn type_desc_is_span(desc: &TypeDesc) -> bool {
    matches!(desc, TypeDesc::Span)
}

/// Check whether a `syn::Type` is `parse_that::Span` (with or without leading `::`).
///
/// Uses structural `syn::Ident` comparison on path segments — no string
/// serialization involved. Matches both `parse_that::Span<'a>` and
/// `::parse_that::Span<'a>`.
pub fn type_is_span(ty: &syn::Type) -> bool {
    if let syn::Type::Path(type_path) = ty {
        let segments = &type_path.path.segments;
        if segments.len() != 2 {
            return false;
        }
        segments[0].ident == "parse_that" && segments[1].ident == "Span"
    } else {
        false
    }
}
