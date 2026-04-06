//! IR type system bridge: TypeDesc → syn::Type conversion + IrCodegenCtx.
//!
//! Provides the context object and type conversion utilities consumed by
//! all IR codegen modules.

use std::collections::{HashMap, HashSet};

use bbnf_ir::{GrammarIR, RuleId, TypeDesc};

use proc_macro2::TokenStream;
use quote::format_ident;
use syn::{Type, parse_quote};

/// Container-level parser attributes parsed from `#[parser(...)]`.
#[derive(Clone, Debug, Default)]
pub struct ParserAttributes {
    pub paths: Vec<std::path::PathBuf>,
    pub debug: bool,
    pub remove_left_recursion: bool,
    pub prettify: bool,
    pub skip_recover: bool,
    pub emit: bool,
}

/// Central context for IR-based code generation.
///
/// Slab is the only storage mode — all data-producing codegen uses slab allocation.
pub struct IrCodegenCtx<'a> {
    pub ir: &'a GrammarIR,
    /// Parser struct name (e.g., `Json`).
    pub ident: &'a syn::Ident,
    /// Enum name (e.g., `JsonParserEnum`).
    pub enum_ident: syn::Ident,
    /// `JsonParserEnum<'a>`.
    pub enum_type: Type,
    /// `&'a JsonParserEnum<'a>`.
    pub boxed_enum_type: Type,
    /// Parser container attributes.
    pub parser_attrs: &'a ParserAttributes,
    /// Whether prettify codegen is enabled after backend preparation.
    pub effective_prettify: bool,
    /// Span-eligible rules that successfully produce `_sp()` methods.
    pub sp_method_rules: HashSet<String>,
    /// Pre-computed syn::Type per rule (from IR TypeDesc).
    pub rule_types: HashMap<RuleId, Type>,
    /// Rule IDs with fused number scan+convert. These rules produce `(Span, f64)`
    /// instead of `Span` in the slab enum. Only set for slab codegen context.
    pub fused_number_rules: HashSet<RuleId>,
    /// Rules that match the operator-chain hot path shape
    /// `Seq(head, Repeat(Seq(op, rhs)))`.
    pub operator_chain_rules: HashSet<RuleId>,
    /// Distinct Vec element TypeDescs for scratch Vec generation (slab mode only).
    /// Each entry gets a scratch field `__s{index}` and collect method `__c{index}`.
    pub scratch_types: Vec<TypeDesc>,
    /// Precomputed TypeDesc → variant name map from all rules' sub_variants.
    /// Non-Span types are globally unique (validated by `validate_sub_variant_uniqueness`),
    /// so first-seen wins and O(1) lookup replaces O(n*m) linear search.
    pub global_sub_variants: HashMap<TypeDesc, String>,
}

impl<'a> IrCodegenCtx<'a> {
    pub fn new(
        ir: &'a GrammarIR,
        ident: &'a syn::Ident,
        parser_attrs: &'a ParserAttributes,
        effective_prettify: bool,
    ) -> Self {
        let enum_ident = quote::format_ident!("{}Enum", ident);
        let enum_type: Type = parse_quote!(#enum_ident<'a>);
        let boxed_enum_type: Type = parse_quote!(&'a #enum_ident<'a>);

        // Always use slab slices — TypeMap records seq_preserve_spans
        // so codegen Seq emission respects span preservation for exact type alignment.
        let mut rule_types = HashMap::new();
        for (rule_id, type_desc) in &ir.types {
            let ty = type_desc_to_syn_raw(
                type_desc,
                &enum_type,
                &boxed_enum_type,
                ir,
                true, // always slab slices
            );
            rule_types.insert(*rule_id, ty);
        }

        // Read distinct Vec element types from TypeMap (precomputed by project_types).
        let scratch_types = ir
            .type_map
            .as_ref()
            .map(|m| m.scratch_types().to_vec())
            .unwrap_or_default();

        // Build global sub-variant lookup: TypeDesc → variant_name.
        // First-seen wins; non-Span types are globally unique per validation.
        let mut global_sub_variants = HashMap::new();
        for rule in &ir.rules {
            for sv in &rule.meta.sub_variants {
                global_sub_variants
                    .entry(sv.ty.clone())
                    .or_insert_with(|| ir.get_string(sv.variant_name).to_string());
            }
        }

        Self {
            ir,
            ident,
            enum_ident,
            enum_type,
            boxed_enum_type,
            parser_attrs,
            effective_prettify,
            sp_method_rules: HashSet::new(),
            rule_types,
            fused_number_rules: HashSet::new(),
            operator_chain_rules: HashSet::new(),
            scratch_types,
            global_sub_variants,
        }
    }

    /// Look up the project_node type for a sub-expression from the TypeMap.
    /// Panics on miss — the TypeMap must cover all nodes the codegen queries.
    pub fn node_type(&self, node: &bbnf_ir::IrNode) -> TypeDesc {
        self.ir
            .type_map
            .as_ref()
            .expect("TypeMap not populated")
            .node_type(node)
            .cloned()
            .unwrap_or_else(|| {
                panic!(
                    "TypeMap node_type miss: {:?} at {:p}",
                    std::mem::discriminant(node),
                    node
                );
            })
    }

    /// Look up the project_node_in_vec type for a sub-expression from the TypeMap.
    pub fn vec_elem_type(&self, node: &bbnf_ir::IrNode) -> TypeDesc {
        self.ir
            .type_map
            .as_ref()
            .expect("TypeMap not populated")
            .vec_elem_type(node)
            .cloned()
            .unwrap_or_else(|| {
                panic!(
                    "TypeMap vec_elem_type miss: {:?} at {:p}",
                    std::mem::discriminant(node),
                    node
                );
            })
    }

    /// Look up the precomputed Seq child types from the TypeMap.
    pub fn seq_child_types(&self, children: &[bbnf_ir::IrNode]) -> Option<Vec<TypeDesc>> {
        self.ir.type_map.as_ref().and_then(|m| {
            m.seq_child_types_by_ptr(children.as_ptr() as usize)
                .map(|s| s.to_vec())
        })
    }

    /// Look up the result type of a Seq (post-compression, post-flattening).
    pub fn seq_result_type(&self, children: &[bbnf_ir::IrNode]) -> Option<TypeDesc> {
        self.ir
            .type_map
            .as_ref()
            .and_then(|m| m.seq_result_type(children.as_ptr() as usize).cloned())
    }

    pub fn seq_preserve_spans(&self, children: &[bbnf_ir::IrNode]) -> bool {
        self.ir
            .type_map
            .as_ref()
            .is_some_and(|m| m.seq_preserve_spans(children.as_ptr() as usize))
    }

    /// Look up the scratch Vec index for a given collection element TypeDesc.
    /// Panics if the element type wasn't pre-registered (should never happen
    /// if `collect_vec_element_types` is correct).
    pub fn scratch_index_for_elem(&self, elem_desc: &TypeDesc) -> usize {
        self.scratch_types
            .iter()
            .position(|t| t == elem_desc)
            .unwrap_or_else(|| {
                panic!(
                    "scratch type not registered for {:?}; known types: {:?}",
                    elem_desc, self.scratch_types
                )
            })
    }

    /// Emit the scratch accessor ident for a given index: `__s0`, `__s1`, etc.
    pub fn scratch_accessor(&self, idx: usize) -> syn::Ident {
        format_ident!("__s{}", idx)
    }

    /// Emit the collect method ident for a given index: `__c0`, `__c1`, etc.
    pub fn collect_accessor(&self, idx: usize) -> syn::Ident {
        format_ident!("__c{}", idx)
    }

    /// Get the context struct ident: `__JsonEnumCtx`.
    pub fn alloc_ctx_ident(&self) -> syn::Ident {
        format_ident!("__{}Ctx", self.enum_ident)
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
            let recovered_ident = self.recovered_static_ident();
            quote::quote! { &#recovered_ident }
        } else {
            quote::quote! { #enum_ident::Recovered }
        }
    }

    /// Emit alloc via AllocCtx: `.slab().alloc()`.
    fn alloc_tokens(&self, helper_call: TokenStream, value: &TokenStream) -> TokenStream {
        quote::quote! { #helper_call.slab().alloc(#value) }
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
        format_ident!("{}", name)
    }

    pub fn unboxed_method_ident_for_name(&self, name: &str) -> syn::Ident {
        format_ident!("{}_unboxed", name)
    }

    pub fn wrap_recur_expr_with_state(
        &self,
        expr: TokenStream,
        state_ident: &syn::Ident,
    ) -> TokenStream {
        let helper_ident = self.alloc_helper_ident();
        let helper_call = quote::quote! { #helper_ident(#state_ident) };
        let alloc = self.alloc_tokens(helper_call, &quote::quote! { #expr });
        quote::quote! {{
            let __slab_alloc = #alloc;
            &*__slab_alloc
        }}
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
        quote::quote! {
            #parser.map_with_ctx(|x, #state_ident| #body)
        }
    }

    pub fn recovered_static_ident(&self) -> syn::Ident {
        format_ident!("__{}_RECOVERED", self.enum_ident)
    }

    pub fn alloc_helper_ident(&self) -> syn::Ident {
        format_ident!("__{}_alloc", self.enum_ident)
    }

    /// Emit code that allocs a value expression into the `boxed_enum_type`.
    ///
    /// `&*helper(state).slab().alloc(expr)`.
    pub fn emit_alloc(&self, value_expr: &TokenStream) -> TokenStream {
        let helper = self.alloc_helper_ident();
        let helper_call = quote::quote! { #helper(state) };
        let alloc = self.alloc_tokens(helper_call, value_expr);
        quote::quote! { &*#alloc }
    }

    /// Emit code that allocs a value via a let binding + alloc.
    ///
    /// `let __alloc = helper(state).slab().alloc(expr); &*__alloc`
    ///
    /// The let-binding form extends the borrow lifetime.
    pub fn emit_alloc_let(&self, value_expr: &TokenStream) -> TokenStream {
        let helper = self.alloc_helper_ident();
        let helper_call = quote::quote! { #helper(state) };
        let alloc = self.alloc_tokens(helper_call, value_expr);
        quote::quote! {
            let __alloc = #alloc;
            &*__alloc
        }
    }

    pub fn collection_builder_type_from_elem_desc(&self, elem_desc: &TypeDesc) -> Type {
        let elem_ty = type_desc_to_syn_raw(
            elem_desc,
            &self.enum_type,
            &self.boxed_enum_type,
            self.ir,
            false, // collection builder is always Vec (the BUILD type, not the final type)
        );
        parse_quote!(Vec<#elem_ty>)
    }

    pub fn rule_body_desc(&self, rule_id: RuleId) -> Option<&TypeDesc> {
        self.ir
            .types
            .iter()
            .find_map(|(id, ty)| (*id == rule_id).then_some(ty))
    }

    // NOTE: emit_scratch_*, generate_alloc_ctx are in alloc_emit.rs (separate impl block).
}

pub fn type_desc_to_syn(desc: &TypeDesc, ctx: &IrCodegenCtx<'_>) -> Type {
    type_desc_to_syn_raw(
        desc,
        &ctx.enum_type,
        &ctx.boxed_enum_type,
        ctx.ir,
        true, // always slab slices
    )
}

fn type_desc_to_syn_raw(
    desc: &TypeDesc,
    enum_type: &Type,
    boxed_enum_type: &Type,
    ir: &GrammarIR,
    use_slices: bool,
) -> Type {
    match desc {
        TypeDesc::Span => parse_quote!(::parse_that::Span<'a>),
        TypeDesc::F64 => parse_quote!(f64),
        TypeDesc::U32 => parse_quote!(u32),
        TypeDesc::Option(inner) => {
            let inner = type_desc_to_syn_raw(inner, enum_type, boxed_enum_type, ir, use_slices);
            parse_quote!(Option<#inner>)
        }
        TypeDesc::Vec(inner) => {
            let inner_ty = type_desc_to_syn_raw(inner, enum_type, boxed_enum_type, ir, use_slices);
            if use_slices {
                parse_quote!(&'a [#inner_ty])
            } else {
                parse_quote!(Vec<#inner_ty>)
            }
        }
        TypeDesc::Tuple(elems) => {
            if elems.is_empty() {
                parse_quote!(())
            } else {
                let types: Vec<_> = elems
                    .iter()
                    .map(|e| type_desc_to_syn_raw(e, enum_type, boxed_enum_type, ir, use_slices))
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
