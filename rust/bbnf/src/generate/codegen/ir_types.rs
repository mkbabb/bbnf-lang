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

/// Central context for IR-based code generation.
///
/// Arena is the only storage mode — all data-producing codegen uses arena allocation.
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
    /// Span-eligible rules that successfully produce `_sp()` methods.
    pub sp_method_rules: HashSet<String>,
    /// Pre-computed syn::Type per rule (from IR TypeDesc).
    pub rule_types: HashMap<RuleId, Type>,
    /// Rule IDs with fused number scan+convert. These rules produce `(Span, f64)`
    /// instead of `Span` in the arena enum. Only set for arena codegen context.
    pub fused_number_rules: HashSet<RuleId>,
    /// Distinct Vec element TypeDescs for scratch Vec generation (arena mode only).
    /// Each entry gets a scratch field `__s{index}` and collect method `__c{index}`.
    pub scratch_types: Vec<TypeDesc>,
}

impl<'a> IrCodegenCtx<'a> {
    pub fn new(
        ir: &'a GrammarIR,
        ident: &'a syn::Ident,
        parser_attrs: &'a ParserAttributes,
    ) -> Self {
        let enum_ident = quote::format_ident!("{}Enum", ident);
        let enum_type: Type = parse_quote!(#enum_ident<'a>);
        let boxed_enum_type: Type = parse_quote!(&'a #enum_ident<'a>);

        // Arena slices for non-prettify. Prettify needs Vec until the codegen's
        // Seq emission respects pretty_preserve/cyclic_context for exact type alignment.
        let use_slices = !parser_attrs.prettify;
        let mut rule_types = HashMap::new();
        for (rule_id, type_desc) in &ir.types {
            let ty = type_desc_to_syn_raw(
                type_desc,
                &enum_type,
                &boxed_enum_type,
                ir,
                use_slices,
            );
            rule_types.insert(*rule_id, ty);
        }

        // Collect distinct Vec element types for scratch Vec generation.
        let scratch_types = collect_vec_element_types(ir);

        Self {
            ir,
            ident,
            enum_ident,
            enum_type,
            boxed_enum_type,
            parser_attrs,
            sp_method_rules: HashSet::new(),
            rule_types,
            fused_number_rules: HashSet::new(),
            scratch_types,
        }
    }

    /// Look up the infer_node type for a sub-expression from the InferMap.
    /// Panics on miss — the InferMap must cover all nodes the codegen queries.
    pub fn infer_node_type(&self, node: &bbnf_ir::IrNode) -> TypeDesc {
        self.ir
            .infer_map
            .as_ref()
            .expect("InferMap not populated")
            .node_type(node)
            .cloned()
            .unwrap_or_else(|| {
                panic!(
                    "InferMap node_type miss: {:?} at {:p}",
                    std::mem::discriminant(node),
                    node
                );
            })
    }

    /// Look up the infer_node_in_vec type for a sub-expression from the InferMap.
    pub fn infer_vec_elem_type(&self, node: &bbnf_ir::IrNode) -> TypeDesc {
        self.ir
            .infer_map
            .as_ref()
            .expect("InferMap not populated")
            .vec_elem_type(node)
            .cloned()
            .unwrap_or_else(|| {
                panic!(
                    "InferMap vec_elem_type miss: {:?} at {:p}",
                    std::mem::discriminant(node),
                    node
                );
            })
    }

    /// Look up the precomputed Seq child types from the InferMap.
    pub fn infer_seq_child_types(&self, children: &[bbnf_ir::IrNode]) -> Option<Vec<TypeDesc>> {
        self.ir
            .infer_map
            .as_ref()
            .and_then(|m| {
                m.seq_child_types_by_ptr(children.as_ptr() as usize)
                    .map(|s| s.to_vec())
            })
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

    /// Get the context struct ident: `__JsonArenaCtx`.
    pub fn arena_ctx_ident(&self) -> syn::Ident {
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

    /// Get the Vec inner TypeDesc for the current rule from ir.types.
    /// Only matches top-level Vec (not Vec nested in Tuple).
    pub fn current_rule_vec_inner(&self, rule_id: Option<RuleId>) -> Option<&TypeDesc> {
        let rid = rule_id?;
        let td = self.ir.types.iter().find(|(id, _)| *id == rid).map(|(_, t)| t)?;
        match td {
            TypeDesc::Vec(inner) => Some(inner.as_ref()),
            _ => None,
        }
    }

    /// Emit arena alloc. ArenaCtx: `.arena().alloc()`. BumpArena (prettify): `.alloc()`.
    fn arena_alloc_tokens(&self, helper_call: TokenStream, value: &TokenStream) -> TokenStream {
        if !self.parser_attrs.prettify {
            quote::quote! { #helper_call.arena().alloc(#value) }
        } else {
            quote::quote! { #helper_call.alloc(#value) }
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
        let helper_ident = self.arena_helper_ident();
        let helper_call = quote::quote! { #helper_ident(#state_ident) };
        let alloc = self.arena_alloc_tokens(helper_call, &quote::quote! { #expr });
        quote::quote! {{
            let __arena_alloc = #alloc;
            &*__arena_alloc
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

    pub fn arena_helper_ident(&self) -> syn::Ident {
        format_ident!("__{}_arena", self.enum_ident)
    }

    /// Emit code that allocs a value expression into the `boxed_enum_type`.
    ///
    /// `&*helper(state).arena().alloc(expr)` (or `.alloc()` for prettify).
    pub fn emit_box_alloc(&self, value_expr: &TokenStream) -> TokenStream {
        let helper = self.arena_helper_ident();
        let helper_call = quote::quote! { #helper(state) };
        let alloc = self.arena_alloc_tokens(helper_call, value_expr);
        quote::quote! { &*#alloc }
    }

    /// Emit code that allocs a value via a let binding + alloc.
    ///
    /// `let __alloc = helper(state).arena().alloc(expr); &*__alloc`
    ///
    /// The let-binding form extends the borrow lifetime.
    pub fn emit_box_alloc_let(&self, value_expr: &TokenStream) -> TokenStream {
        let helper = self.arena_helper_ident();
        let helper_call = quote::quote! { #helper(state) };
        let alloc = self.arena_alloc_tokens(helper_call, value_expr);
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

    /// Emit scratch-based collection init: records scratch depth.
    pub fn emit_scratch_init(&self, elem_desc: &TypeDesc, depth_var: &syn::Ident) -> TokenStream {
        let idx = self.scratch_index_for_elem(elem_desc);
        let s_fn = self.scratch_accessor(idx);
        let helper = self.arena_helper_ident();
        quote::quote! {
            let #depth_var = #helper(state).#s_fn().len();
        }
    }

    /// Emit scratch push for a value expression.
    pub fn emit_scratch_push(&self, elem_desc: &TypeDesc, value_expr: &TokenStream) -> TokenStream {
        let idx = self.scratch_index_for_elem(elem_desc);
        let s_fn = self.scratch_accessor(idx);
        let helper = self.arena_helper_ident();
        quote::quote! { #helper(state).#s_fn().push(#value_expr) }
    }

    /// Emit scratch collect: copies scratch[depth..] to arena slice, truncates.
    pub fn emit_scratch_collect(&self, elem_desc: &TypeDesc, depth_var: &syn::Ident) -> TokenStream {
        let idx = self.scratch_index_for_elem(elem_desc);
        let c_fn = self.collect_accessor(idx);
        let helper = self.arena_helper_ident();
        quote::quote! { #helper(state).#c_fn(#depth_var) }
    }

    /// Emit scratch truncate on failure path.
    pub fn emit_scratch_truncate(
        &self,
        elem_desc: &TypeDesc,
        depth_var: &syn::Ident,
    ) -> TokenStream {
        let idx = self.scratch_index_for_elem(elem_desc);
        let s_fn = self.scratch_accessor(idx);
        let helper = self.arena_helper_ident();
        quote::quote! { #helper(state).#s_fn().truncate(#depth_var); }
    }

    /// Emit scratch len - depth expression (element count).
    pub fn emit_scratch_count(
        &self,
        elem_desc: &TypeDesc,
        depth_var: &syn::Ident,
    ) -> TokenStream {
        let idx = self.scratch_index_for_elem(elem_desc);
        let s_fn = self.scratch_accessor(idx);
        let helper = self.arena_helper_ident();
        quote::quote! { (#helper(state).#s_fn().len() - #depth_var) }
    }

    /// Emit scratch extend from a slice (for Seq flattening).
    pub fn emit_scratch_extend_slice(
        &self,
        elem_desc: &TypeDesc,
        slice_expr: &TokenStream,
    ) -> TokenStream {
        let idx = self.scratch_index_for_elem(elem_desc);
        let s_fn = self.scratch_accessor(idx);
        let helper = self.arena_helper_ident();
        quote::quote! { #helper(state).#s_fn().extend_from_slice(#slice_expr) }
    }

    /// Generate the arena context struct definition + impl + helper function.
    ///
    /// Returns (struct_def, helper_fn) as TokenStreams.
    pub fn generate_arena_ctx(&self) -> (TokenStream, TokenStream) {
        let ctx_ident = self.arena_ctx_ident();
        let enum_ident = &self.enum_ident;
        let helper_ident = self.arena_helper_ident();

        // Generate scratch fields, accessors, and collect methods.
        let mut fields = vec![];
        let mut accessors = vec![];
        let mut new_fields = vec![];

        for (i, elem_td) in self.scratch_types.iter().enumerate() {
            let s_ident = self.scratch_accessor(i);
            let c_ident = self.collect_accessor(i);
            let elem_ty = type_desc_to_syn(elem_td, self);

            fields.push(quote::quote! {
                #s_ident: ::std::cell::UnsafeCell<Vec<#elem_ty>>
            });

            new_fields.push(quote::quote! {
                #s_ident: ::std::cell::UnsafeCell::new(Vec::with_capacity(64))
            });

            accessors.push(quote::quote! {
                #[inline(always)]
                #[allow(non_snake_case)]
                fn #s_ident(&self) -> &mut Vec<#elem_ty> {
                    unsafe { &mut *self.#s_ident.get() }
                }

                #[inline(always)]
                #[allow(non_snake_case)]
                fn #c_ident(&'a self, depth: usize) -> &'a [#elem_ty] {
                    let s = self.#s_ident();
                    let slice = self.__arena.alloc_slice_clone(&s[depth..]);
                    s.truncate(depth);
                    slice
                }
            });
        }

        let struct_def = quote::quote! {
            #[allow(non_camel_case_types)]
            struct #ctx_ident<'a> {
                __arena: ::parse_that::BumpArena<#enum_ident<'a>>,
                #(#fields),*
            }

            #[allow(non_snake_case)]
            impl<'a> #ctx_ident<'a> {
                fn with_capacity(n: usize) -> Self {
                    Self {
                        __arena: ::parse_that::BumpArena::with_capacity(n),
                        #(#new_fields),*
                    }
                }

                #[inline(always)]
                fn arena(&'a self) -> &'a ::parse_that::BumpArena<#enum_ident<'a>> {
                    &self.__arena
                }

                #(#accessors)*
            }
        };

        let helper_fn = quote::quote! {
            #[allow(non_snake_case)]
            #[inline(always)]
            fn #helper_ident<'a>(
                state: &::parse_that::ParserState<'a>,
            ) -> &'a #ctx_ident<'a> {
                debug_assert!(
                    !state.context_ptr.is_null(),
                    "arena parser requires parse_with_context()"
                );
                unsafe {
                    &*(state.context_ptr as *const #ctx_ident<'a>)
                }
            }
        };

        (struct_def, helper_fn)
    }
}

/// Collect distinct Vec element types from both IR types and per-rule inference.
///
/// Uses both sources to handle the divergence between the IR pass (which uses
/// `pretty_preserve=true` / `cyclic_context=true` for some rules) and codegen
/// (which uses `false` / `false`). Both sets of types are registered so that
/// scratch lookups work regardless of which inference the callsite uses.
fn collect_vec_element_types(ir: &GrammarIR) -> Vec<TypeDesc> {
    use bbnf_ir::IrNode;
    use bbnf_ir::passes::{InferCtx, infer_node_in_vec};

    let mut seen = Vec::new();

    // Source 1: from ir.types (matches the IR pass).
    for (_rule_id, td) in &ir.types {
        collect_vec_inner(td, &mut seen);
    }

    // Source 2: from per-node inference with codegen flags (pretty_preserve=false,
    // cyclic_context=false) to catch any types that diverge from ir.types.
    let cache: HashMap<RuleId, TypeDesc> = ir.types.iter().cloned().collect();
    let acyclic: HashSet<RuleId> = ir
        .rules
        .iter()
        .filter(|r| !r.meta.is_cyclic)
        .map(|r| r.id)
        .collect();
    let infer_ctx = InferCtx {
        ir,
        cache: &cache,
        acyclic_rules: &acyclic,
        cyclic_context: false,
        pretty_preserve: false,
        recorder: None,
    };

    fn walk_node(node: &IrNode, ctx: &InferCtx<'_>, out: &mut Vec<TypeDesc>) {
        match node {
            IrNode::Repeat { inner, lo, hi } => {
                if !(*lo == 0 && *hi == 1) {
                    let elem_ty = infer_node_in_vec(inner, ctx);
                    if elem_ty != TypeDesc::Span && !out.contains(&elem_ty) {
                        out.push(elem_ty);
                    }
                }
                walk_node(inner, ctx, out);
            }
            IrNode::Seq(children) => {
                for child in children {
                    walk_node(child, ctx, out);
                }
            }
            IrNode::Alt(branches, _) => {
                for b in branches {
                    walk_node(&b.node, ctx, out);
                }
            }
            IrNode::Skip(l, r) | IrNode::Next(l, r) | IrNode::Minus(l, r) => {
                walk_node(l, ctx, out);
                walk_node(r, ctx, out);
            }
            IrNode::OptionalWhitespace(inner)
            | IrNode::Map { inner, .. }
            | IrNode::Negate(inner) => {
                walk_node(inner, ctx, out);
            }
            _ => {}
        }
    }

    for rule in &ir.rules {
        walk_node(&rule.body, &infer_ctx, &mut seen);
    }
    seen
}

fn collect_vec_inner(td: &TypeDesc, out: &mut Vec<TypeDesc>) {
    match td {
        TypeDesc::Vec(inner) => {
            if !out.contains(inner.as_ref()) {
                out.push(inner.as_ref().clone());
            }
            collect_vec_inner(inner, out);
        }
        TypeDesc::Option(inner) => collect_vec_inner(inner, out),
        TypeDesc::Tuple(elems) => {
            for e in elems {
                collect_vec_inner(e, out);
            }
        }
        _ => {}
    }
}

pub fn type_desc_to_syn(desc: &TypeDesc, ctx: &IrCodegenCtx<'_>) -> Type {
    type_desc_to_syn_raw(
        desc,
        &ctx.enum_type,
        &ctx.boxed_enum_type,
        ctx.ir,
        !ctx.parser_attrs.prettify,
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
