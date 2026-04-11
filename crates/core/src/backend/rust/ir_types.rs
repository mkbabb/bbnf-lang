//! IR type system bridge: TypeDesc → syn::Type conversion + IrCodegenCtx.
//!
//! Provides the context object consumed by all IR codegen modules.
//!
//! Tranche AC.2 — the Rust backend is tape-first. The enum + slab
//! allocation surface this module used to expose is gone. The
//! `emit_alloc` / `emit_alloc_let` family and the scratch-Vec
//! helpers in the former `alloc_emit.rs` module have been deleted.
//! Every rule returns `Option<TapeOffset>`; every sub-expression
//! returns either `Option<()>` (side-effecting) or
//! `Option<TapeOffset>` (tape-pushing).
//!
//! `type_desc_to_syn_raw` panics on `BoxedEnum` — a live BoxedEnum
//! request on the Rust backend is a driver bug under AC.2.

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
    pub serialize: bool,
    /// Structural mode — preserve all structural enum variants.
    pub structural: bool,
}

/// Central context for IR-based code generation.
///
/// Holds the per-rule type projection plus the grammar marker
/// ident. Under AC.2 tape-first there is no longer a slab context
/// struct and no scratch-Vec allocation; the tape records are the
/// only storage medium.
pub struct IrCodegenCtx<'a> {
    pub ir: &'a GrammarIR,
    /// Parser struct name (e.g., `Json`).
    pub ident: &'a syn::Ident,
    /// Enum name (e.g., `JsonParserEnum`). Retained for the
    /// prettify emitter which still emits a parse-tree enum for
    /// `@pretty` grammars.
    pub enum_ident: syn::Ident,
    /// `JsonParserEnum<'a>`.
    pub enum_type: Type,
    /// `&'a JsonParserEnum<'a>`. Kept for the prettify emitter.
    pub boxed_enum_type: Type,
    /// Parser container attributes.
    pub parser_attrs: &'a ParserAttributes,
    /// Whether prettify codegen is enabled after backend preparation.
    pub effective_prettify: bool,
    /// Span-eligible rules that successfully produce `_sp()` methods.
    pub sp_method_rules: HashSet<String>,
    /// Pre-computed syn::Type per rule (from IR TypeDesc).
    pub rule_types: HashMap<RuleId, Type>,
    /// Rule IDs with fused number scan+convert.
    pub fused_number_rules: HashSet<RuleId>,
    /// Rules that match the operator-chain hot path shape.
    pub operator_chain_rules: HashSet<RuleId>,
    /// Distinct Vec element TypeDescs. Retained only for the
    /// prettify emitter; tape-first rule emission does not use it.
    pub scratch_types: Vec<TypeDesc>,
    /// Precomputed TypeDesc → variant name map from all rules' sub_variants.
    pub global_sub_variants: HashMap<TypeDesc, String>,
}

impl<'a> IrCodegenCtx<'a> {
    pub fn new(
        ir: &'a GrammarIR,
        ident: &'a syn::Ident,
        parser_attrs: &'a ParserAttributes,
        effective_prettify: bool,
    ) -> Self {
        // Tranche AC.2: under tape-first, the legacy `<Grammar>Enum`
        // is gone. Type projection for BoxedEnum / Enum slots now
        // maps to `<Grammar>NodeView<'a>` — the generic node view
        // the backend emits via `view::generate_views`. NodeView
        // is `Copy`, so Boxed/Inline both use the same type.
        //
        // `enum_ident` stays the bare grammar marker (e.g.
        // `BbnfBootstrap`) so schema-helper emitters that read
        // `enum_name` see the user-facing struct name. The
        // type-projection `enum_type`/`boxed_enum_type` target
        // `<Grammar>NodeView<'a>` because that is the actual
        // Rust type any residual BoxedEnum/Enum type slot should
        // resolve to.
        let enum_ident = ident.clone();
        let node_view_ident = quote::format_ident!("{}NodeView", ident);
        let enum_type: Type = parse_quote!(#node_view_ident<'a>);
        let boxed_enum_type: Type = parse_quote!(#node_view_ident<'a>);

        let mut rule_types = HashMap::new();
        for (rule_id, type_desc) in &ir.types {
            let ty = type_desc_to_syn_raw(
                type_desc,
                &enum_type,
                &boxed_enum_type,
                ir,
                true,
            );
            rule_types.insert(*rule_id, ty);
        }

        let scratch_types = ir
            .type_map
            .as_ref()
            .map(|m| m.scratch_types().to_vec())
            .unwrap_or_default();

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

    /// Look up the project_node type for a sub-expression from the
    /// TypeMap. Panics on miss — the TypeMap must cover every node
    /// the codegen queries.
    pub fn node_type(&self, node: &bbnf_ir::IrNode) -> TypeDesc {
        self.ir.node_type(node).cloned().unwrap_or_else(|| {
            panic!(
                "TypeMap node_type miss: {:?} at {:p}",
                std::mem::discriminant(node),
                node
            );
        })
    }

    pub fn vec_elem_type(&self, node: &bbnf_ir::IrNode) -> TypeDesc {
        self.ir.vec_elem_type(node).cloned().unwrap_or_else(|| {
            panic!(
                "TypeMap vec_elem_type miss: {:?} at {:p}",
                std::mem::discriminant(node),
                node
            );
        })
    }

    pub fn seq_child_types(&self, seq_node: &bbnf_ir::IrNode) -> Option<Vec<TypeDesc>> {
        self.ir.seq_child_types(seq_node).map(|s| s.to_vec())
    }

    pub fn seq_result_type(&self, seq_node: &bbnf_ir::IrNode) -> Option<TypeDesc> {
        self.ir.seq_result_type(seq_node).cloned()
    }

    pub fn seq_preserve_spans(&self, seq_node: &bbnf_ir::IrNode) -> bool {
        self.ir.seq_preserve_spans(seq_node)
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

    pub fn rule_body_desc(&self, rule_id: RuleId) -> Option<&TypeDesc> {
        self.ir
            .types
            .iter()
            .find_map(|(id, ty)| (*id == rule_id).then_some(ty))
    }

    // ─── Tape-first stubs for prettify compatibility ──────────────
    //
    // The prettify emitter still calls `emit_scratch_*`,
    // `emit_alloc*`, `recovered_static_ident`, and
    // `generate_alloc_ctx`. Under AC.2 the monolithic rule path no
    // longer uses any of these; they remain as thin stubs so
    // `backend/rust/emitter/prettify/` continues to compile
    // unchanged. Each stub returns the minimum viable token stream;
    // prettify grammars that still depend on the allocating enum
    // model must be migrated alongside the eventual prettify
    // rewrite.

    pub fn scratch_index_for_elem(&self, elem_desc: &TypeDesc) -> usize {
        self.scratch_types
            .iter()
            .position(|t| t == elem_desc)
            .unwrap_or(0)
    }

    pub fn scratch_accessor(&self, idx: usize) -> syn::Ident {
        format_ident!("__s{}", idx)
    }

    pub fn collect_accessor(&self, idx: usize) -> syn::Ident {
        format_ident!("__c{}", idx)
    }

    pub fn alloc_ctx_ident(&self) -> syn::Ident {
        format_ident!("__{}Ctx", self.enum_ident)
    }

    pub fn alloc_helper_ident(&self) -> syn::Ident {
        format_ident!("__{}_alloc", self.enum_ident)
    }

    pub fn recovered_static_ident(&self) -> syn::Ident {
        format_ident!("__{}_RECOVERED", self.enum_ident)
    }

    pub fn recover_sentinel(&self, _rule_id: RuleId) -> TokenStream {
        // AC.2: recovery uses TapeKind::Recovered at the emit site;
        // no static enum sentinel is needed. Returning `None` keeps
        // any residual call-site that weaves this in compile-safe.
        quote::quote! { None::<::bbnf::runtime::tape::TapeOffset> }
    }

    pub fn collection_builder_type_from_elem_desc(&self, elem_desc: &TypeDesc) -> Type {
        let elem_ty = type_desc_to_syn_raw(
            elem_desc,
            &self.enum_type,
            &self.boxed_enum_type,
            self.ir,
            false,
        );
        parse_quote!(Vec<#elem_ty>)
    }
}

pub fn type_desc_to_syn(desc: &TypeDesc, ctx: &IrCodegenCtx<'_>) -> Type {
    type_desc_to_syn_raw(
        desc,
        &ctx.enum_type,
        &ctx.boxed_enum_type,
        ctx.ir,
        true,
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
        TypeDesc::BoxedEnum => {
            // Tranche AC.2: under tape-first, BoxedEnum maps to
            // `<Grammar>NodeView<'a>` (the generic Copy wrapper).
            // The legacy `&'a <Grammar>Enum<'a>` indirection is
            // gone because every rule returns `Option<TapeOffset>`
            // and the typed surface lives in the view layer.
            boxed_enum_type.clone()
        }
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
