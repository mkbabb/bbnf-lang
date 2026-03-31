//! IR type system bridge: TypeDesc → syn::Type conversion + IrCodegenCtx.
//!
//! Provides the context object and type conversion utilities consumed by
//! all IR codegen modules.

use std::collections::{HashMap, HashSet};

use bbnf_ir::{GrammarIR, RuleId, TypeDesc};

use proc_macro2::TokenStream;
use quote::format_ident;
use syn::{parse_quote, Type};

/// Container-level parser attributes parsed from `#[parser(...)]`.
#[derive(Clone, Debug, Default)]
pub struct ParserAttributes {
    pub paths: Vec<std::path::PathBuf>,
    pub ignore_whitespace: bool,
    pub debug: bool,
    pub use_string: bool,
    pub remove_left_recursion: bool,
    pub prettify: bool,
    pub skip_recover: bool,
    pub arena: bool,
    /// Span-only monolithic parse mode: all rules return `Span<'a>`, zero allocations.
    /// Requires that the grammar has no custom Map functions (all rules are span-compatible).
    pub span: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StorageMode {
    Owned,
    Arena,
}

/// Central context for IR-based code generation.
pub struct IrCodegenCtx<'a> {
    pub ir: &'a GrammarIR,
    /// Parser struct name (e.g., `Json`).
    pub ident: &'a syn::Ident,
    /// Enum name (e.g., `JsonEnum` / `JsonArenaEnum`).
    pub enum_ident: syn::Ident,
    /// `JsonEnum<'a>` / `JsonArenaEnum<'a>`.
    pub enum_type: Type,
    /// `Box<JsonEnum<'a>>` / `&'a JsonArenaEnum<'a>`.
    pub boxed_enum_type: Type,
    pub storage_mode: StorageMode,
    /// Parser container attributes.
    pub parser_attrs: &'a ParserAttributes,
    /// Span-eligible rules that successfully produce `_sp()` methods.
    pub sp_method_rules: HashSet<String>,
    /// Pre-computed syn::Type per rule (from IR TypeDesc).
    pub rule_types: HashMap<RuleId, Type>,
    /// Rule IDs with fused number scan+convert. These rules produce `(Span, f64)`
    /// instead of `Span` in the arena enum. Only set for arena codegen context.
    pub fused_number_rules: HashSet<RuleId>,
}

impl<'a> IrCodegenCtx<'a> {
    pub fn new(
        ir: &'a GrammarIR,
        ident: &'a syn::Ident,
        parser_attrs: &'a ParserAttributes,
        storage_mode: StorageMode,
    ) -> Self {
        let enum_ident = match storage_mode {
            StorageMode::Owned => quote::format_ident!("{}Enum", ident),
            StorageMode::Arena => quote::format_ident!("{}ArenaEnum", ident),
        };
        let enum_type: Type = parse_quote!(#enum_ident<'a>);
        let boxed_enum_type: Type = match storage_mode {
            StorageMode::Owned => parse_quote!(Box<#enum_ident<'a>>),
            StorageMode::Arena => parse_quote!(&'a #enum_ident<'a>),
        };

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
            storage_mode,
            parser_attrs,
            sp_method_rules: HashSet::new(),
            rule_types,
            fused_number_rules: HashSet::new(),
        }
    }

    #[inline]
    pub fn uses_arena(&self) -> bool {
        self.storage_mode == StorageMode::Arena
    }

    pub fn rule_return_type(&self, rule_id: RuleId) -> Type {
        let rule = &self.ir.rules[rule_id as usize];
        if rule.meta.is_transparent {
            self.boxed_enum_type.clone()
        } else {
            self.enum_type.clone()
        }
    }

    pub fn rule_body_type(&self, rule_id: RuleId) -> Type {
        self.rule_types
            .get(&rule_id)
            .cloned()
            .unwrap_or_else(|| self.boxed_enum_type.clone())
    }

    pub fn recover_sentinel(&self, rule_id: RuleId) -> TokenStream {
        let rule = &self.ir.rules[rule_id as usize];
        let enum_ident = &self.enum_ident;
        if rule.meta.is_transparent {
            match self.storage_mode {
                StorageMode::Owned => quote::quote! { Box::new(#enum_ident::Recovered) },
                StorageMode::Arena => {
                    let recovered_ident = self.recovered_static_ident();
                    quote::quote! { &#recovered_ident }
                }
            }
        } else {
            quote::quote! { #enum_ident::Recovered }
        }
    }

    pub fn has_sp_method(&self, name: &str) -> bool {
        self.sp_method_rules.contains(name)
    }

    pub fn resolve_rule_name(&self, rule_id: RuleId) -> &str {
        let rule = &self.ir.rules[rule_id as usize];
        if let Some(alias_id) = rule.meta.is_alias {
            self.ir.get_string(self.ir.rules[alias_id as usize].name)
        } else {
            self.ir.get_string(rule.name)
        }
    }

    pub fn rule_method_ident(&self, rule_id: RuleId) -> syn::Ident {
        self.method_ident_for_name(self.resolve_rule_name(rule_id))
    }

    pub fn method_ident_for_name(&self, name: &str) -> syn::Ident {
        match self.storage_mode {
            StorageMode::Owned => format_ident!("{}", name),
            StorageMode::Arena => format_ident!("{}_arena", name),
        }
    }

    pub fn unboxed_method_ident_for_name(&self, name: &str) -> syn::Ident {
        match self.storage_mode {
            StorageMode::Owned => format_ident!("{}_unboxed", name),
            StorageMode::Arena => format_ident!("{}_arena_unboxed", name),
        }
    }

    pub fn wrap_recur_expr_with_state(
        &self,
        expr: TokenStream,
        state_ident: &syn::Ident,
    ) -> TokenStream {
        match self.storage_mode {
            StorageMode::Owned => quote::quote! { Box::new(#expr) },
            StorageMode::Arena => {
                let helper_ident = self.arena_helper_ident();
                quote::quote! {{
                    let __arena_alloc = #helper_ident(#state_ident).alloc(#expr);
                    &*__arena_alloc
                }}
            }
        }
    }

    pub fn wrap_recur_expr(&self, expr: TokenStream) -> TokenStream {
        let state_ident = format_ident!("state");
        self.wrap_recur_expr_with_state(expr, &state_ident)
    }

    pub fn wrap_recur_map_with_state(
        &self,
        parser: TokenStream,
        body: TokenStream,
        state_ident: &syn::Ident,
    ) -> TokenStream {
        match self.storage_mode {
            StorageMode::Owned => quote::quote! { #parser.map(|x| #body) },
            StorageMode::Arena => quote::quote! {
                #parser.map_with_ctx(|x, #state_ident| #body)
            },
        }
    }

    pub fn recovered_static_ident(&self) -> syn::Ident {
        format_ident!("__{}_RECOVERED", self.enum_ident)
    }

    pub fn arena_helper_ident(&self) -> syn::Ident {
        format_ident!("__{}_arena", self.enum_ident)
    }

    /// Emit code that boxes/allocs a value expression into the `boxed_enum_type`.
    ///
    /// - Arena mode: `&*helper(state).alloc(expr)`
    /// - Owned mode: `Box::new(expr)`
    pub fn emit_box_alloc(&self, value_expr: &TokenStream) -> TokenStream {
        match self.storage_mode {
            StorageMode::Arena => {
                let helper = self.arena_helper_ident();
                quote::quote! { &*#helper(state).alloc(#value_expr) }
            }
            StorageMode::Owned => {
                quote::quote! { Box::new(#value_expr) }
            }
        }
    }

    /// Emit code that boxes/allocs a value via a let binding + alloc.
    ///
    /// - Arena mode: `let __alloc = helper(state).alloc(expr); &*__alloc`
    /// - Owned mode: `Box::new(expr)`
    ///
    /// The let-binding form is needed in Arena mode to extend the borrow lifetime.
    pub fn emit_box_alloc_let(&self, value_expr: &TokenStream) -> TokenStream {
        match self.storage_mode {
            StorageMode::Arena => {
                let helper = self.arena_helper_ident();
                quote::quote! {
                    let __alloc = #helper(state).alloc(#value_expr);
                    &*__alloc
                }
            }
            StorageMode::Owned => {
                quote::quote! { Box::new(#value_expr) }
            }
        }
    }
}

pub fn type_desc_to_syn(desc: &TypeDesc, ctx: &IrCodegenCtx<'_>) -> Type {
    type_desc_to_syn_raw(desc, &ctx.enum_type, &ctx.boxed_enum_type, ctx.ir)
}

fn type_desc_to_syn_raw(
    desc: &TypeDesc,
    enum_type: &Type,
    boxed_enum_type: &Type,
    ir: &GrammarIR,
) -> Type {
    match desc {
        TypeDesc::Span => parse_quote!(::parse_that::Span<'a>),
        TypeDesc::F64 => parse_quote!(f64),
        TypeDesc::U32 => parse_quote!(u32),
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

pub fn type_desc_is_span(desc: &TypeDesc) -> bool {
    matches!(desc, TypeDesc::Span)
}

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
