//! View emission for `MustTape` Alt rules.
//!
//! An Alt rule's view exposes `.as_<variant>()` discriminated
//! accessors that test `meta_idx` (the branch index) and return the
//! chosen branch's child view wrapped in `Option`. Heterogeneous
//! Alts with sub-variant coercion also generate sub-variant accessors.
//!
//! AQ.6.C — typed-enum dispatch: when every branch of an Alt has a
//! payload-eligible projected type (scalar via
//! [`TypeDesc::is_scalar_payload`] OR aggregate via
//! `ir.payload_layouts`), the generator emits an additional
//! `<RuleName>Value` enum and a `.value()` accessor that returns
//! the chosen branch's typed value via `match meta_idx`. The
//! enum supplements the existing accessors — it does not replace
//! them, so consumers can still walk the cursor tree directly.

use bbnf_ir::passes::PayloadLayout;
use bbnf_ir::{GrammarIR, IrNode, IrRule, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

/// Emit typed Alt variant accessors for a rule whose body is an `Alt`.
///
/// Returns an `impl` block with `.as_<variant>()` methods that
/// dispatch on `meta_idx` (branch index) and `.is_<variant>()` discriminator
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
    let mut seen_names = std::collections::HashSet::new();

    // Per-branch accessors based on the branch's inner shape.
    for (branch_idx, branch) in branches.iter().enumerate() {
        let idx_u8 = branch_idx as u8;

        // Try to derive a meaningful name from the branch node.
        // Disambiguate on collision by appending the branch index.
        let (mut variant_name, view_ty) =
            resolve_branch_identity(&branch.node, branch_idx, ir, grammar_name);

        if !seen_names.insert(variant_name.clone()) {
            variant_name = format!("{}_{}", variant_name, branch_idx);
            seen_names.insert(variant_name.clone());
        }

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
                if self.cursor.meta_idx() == #idx_u8 {
                    self.cursor.child(0).map(|c| #view_ty::from_cursor(c, self.input))
                } else {
                    None
                }
            }

            #[doc = #doc_is]
            #[inline]
            pub fn #is_ident(&self) -> bool {
                self.cursor.meta_idx() == #idx_u8
            }
        });
    }

    // Sub-variant accessors from heterogeneous coercion.
    let mut sv_counter = 0u32;
    for sv in &rule.meta.sub_variants {
        let sv_base = ir.get_string(sv.variant_name).to_string();
        let mut sv_name = sv_base.clone();
        while !seen_names.insert(sv_name.clone()) {
            sv_counter += 1;
            sv_name = format!("{}_sv{}", sv_base, sv_counter);
        }
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
                if self.cursor.meta_idx() == #branch_idx {
                    self.cursor.child(0).map(|c| #node_view_ident::from_cursor(c, self.input))
                } else {
                    None
                }
            }

            #[inline]
            pub fn #is_ident(&self) -> bool {
                self.cursor.meta_idx() == #branch_idx
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

    // AQ.6.C — typed enum view. Append the `<RuleName>Value` enum
    // type definition + `.value()` accessor when every branch is
    // payload-eligible. The accessor lives in the same `impl` block
    // as the discriminated variant accessors so consumers see one
    // coherent surface.
    let typed_enum = emit_typed_enum_value_accessor(rule, rule_name, branches, ir, grammar_name);

    if methods.is_empty() && typed_enum.is_empty() {
        return quote! {};
    }

    quote! {
        impl<'p> #view_ident<'p> {
            #(#methods)*
        }
        #typed_enum
    }
}

/// AQ.6.C — emit the `<RuleName>Value` typed enum + `.value()`
/// accessor when every branch of the Alt has a payload-eligible
/// projected type.
///
/// Returns an empty `TokenStream` when the rule's branches are not
/// uniformly payload-eligible (the fallback discriminated-accessor
/// surface above remains the user's primary API).
fn emit_typed_enum_value_accessor(
    rule: &IrRule,
    rule_name: &str,
    branches: &[bbnf_ir::AltBranch],
    ir: &GrammarIR,
    grammar_name: &str,
) -> TokenStream {
    let view_ident = format_ident!("{}View", rule_name);
    let enum_ident = format_ident!("{}Value", rule_name);

    // Gather per-branch (variant_name, projected TypeDesc) pairs.
    //
    // AR.2.2: peel through Map/Regex wrappers to find the payload
    // shape even after fuse_single_use has inlined branches. A bare
    // `Ref(rid)` looks up the target rule; an inlined `Map { inner,
    // return_type }` or `IrNode::Regex` infers the shape directly.
    //
    // AR.2.3: mixed payload + cursor branches. Instead of bailing on
    // the first non-payload-eligible branch, collect all branches:
    // payload-eligible ones get typed reads, non-eligible ones get
    // cursor-wrapped sub-views. The enum only emits when at least
    // one branch IS payload-eligible (otherwise the standard
    // discriminated-accessor surface suffices).
    let mut variants: Vec<(String, BranchVariant)> = Vec::with_capacity(branches.len());
    let mut seen = std::collections::HashSet::new();
    let mut has_any_payload = false;
    for (idx, branch) in branches.iter().enumerate() {
        let inner = peel_map_wrappers(&branch.node);
        let (variant_name, variant) = match inner {
            IrNode::Ref(rule_id) => {
                let target = &ir.rules[*rule_id as usize];
                let name = ir.get_string(target.name).to_string();
                match branch_value_shape(*rule_id, ir) {
                    Some(shape) => (name, BranchVariant::Payload(shape)),
                    None => (name, BranchVariant::Cursor),
                }
            }
            // AR.2.2: inlined Map — resolve return_type from FnDescriptor
            IrNode::Map { fn_id, .. } => {
                let td = ir.fns.get(*fn_id as usize).and_then(|fd| match fd {
                    bbnf_ir::FnDescriptor::Expr { return_type: Some(td), .. } => Some(td),
                    bbnf_ir::FnDescriptor::NumberConvert => Some(&TypeDesc::F64),
                    _ => None,
                });
                match td {
                    Some(td) if td.needs_payload_slot() => {
                        let name = format!("branch_{}", idx);
                        (name, BranchVariant::Payload(BranchValueShape::Scalar(td.clone())))
                    }
                    _ => {
                        let name = format!("branch_{}", idx);
                        (name, BranchVariant::Cursor)
                    }
                }
            }
            _ => {
                let name = format!("branch_{}", idx);
                (name, BranchVariant::Cursor)
            }
        };
        if matches!(variant, BranchVariant::Payload(_)) {
            has_any_payload = true;
        }
        let mut name = variant_name;
        if !seen.insert(name.clone()) {
            name = format!("{}_{}", name, idx);
            seen.insert(name.clone());
        }
        variants.push((name, variant));
    }

    if !has_any_payload {
        return TokenStream::new();
    }

    // Enum type definition — AR.2.3: mixed payload + cursor variants.
    let node_view_ident = format_ident!("{}NodeView", grammar_name);

    let enum_variants: Vec<TokenStream> = variants
        .iter()
        .map(|(name, variant)| {
            let v_ident = format_ident!("{}", name);
            match variant {
                BranchVariant::Payload(BranchValueShape::Scalar(td)) if matches!(td, TypeDesc::Span) => {
                    quote! { #v_ident(&'p str) }
                }
                BranchVariant::Payload(BranchValueShape::Scalar(td)) => {
                    let ty_ident = format_ident!(
                        "{}",
                        td.rust_ident().expect("scalar TypeDesc has rust_ident"),
                    );
                    quote! { #v_ident(#ty_ident) }
                }
                BranchVariant::Payload(BranchValueShape::Aggregate(layout)) => {
                    let field_tys: Vec<TokenStream> = layout
                        .fields
                        .iter()
                        .map(|f| aggregate_field_type_for_enum(&f.ty))
                        .collect();
                    quote! { #v_ident(( #(#field_tys),* )) }
                }
                BranchVariant::Cursor => {
                    // Non-payload-eligible branch — wrap in the
                    // grammar's generic NodeView for cursor access.
                    quote! { #v_ident(#node_view_ident<'p>) }
                }
            }
        })
        .collect();

    // Per-branch dispatch — payload branches decode typed values;
    // cursor branches wrap the child in a NodeView.
    let dispatch_arms: Vec<TokenStream> = branches
        .iter()
        .enumerate()
        .zip(variants.iter())
        .map(|((branch_idx, _), (name, variant))| {
            let idx_u8 = branch_idx as u8;
            let v_ident = format_ident!("{}", name);
            match variant {
                BranchVariant::Payload(shape) => {
                    let reader = match shape {
                        BranchValueShape::Scalar(td) => emit_scalar_value_decode(td),
                        BranchValueShape::Aggregate(layout) => emit_aggregate_value_decode(layout),
                    };
                    quote! {
                        #idx_u8 => {
                            let __cursor = self.cursor.child(0).unwrap_or(self.cursor);
                            let __rec = __cursor.record();
                            let __tape = __cursor.tape();
                            let __value = #reader;
                            Some(#enum_ident::#v_ident(__value))
                        }
                    }
                }
                BranchVariant::Cursor => {
                    quote! {
                        #idx_u8 => {
                            let __child = self.cursor.child(0)?;
                            Some(#enum_ident::#v_ident(
                                #node_view_ident::from_cursor(__child, self.input)
                            ))
                        }
                    }
                }
            }
        })
        .collect();

    // Determine whether the enum needs a lifetime parameter. It does
    // when any variant borrows from `'p` — i.e. Cursor branches
    // (which wrap `NodeView<'p>`) or Scalar Span branches (which
    // carry `&'p str`). Pure-primitive / pure-aggregate enums need
    // no lifetime; emitting an unused `'p` triggers E0392.
    let needs_lifetime = variants.iter().any(|(_, v)| matches!(
        v,
        BranchVariant::Cursor
        | BranchVariant::Payload(BranchValueShape::Scalar(TypeDesc::Span))
    ));

    let _ = rule;

    if needs_lifetime {
        quote! {
            /// Typed value enum — payload-eligible branches carry typed
            /// values directly; non-eligible branches wrap a cursor view.
            #[derive(Clone, Debug)]
            pub enum #enum_ident<'p> {
                #(#enum_variants,)*
            }

            impl<'p> #view_ident<'p> {
                /// Decode the chosen branch's value. Payload-eligible
                /// branches return typed scalars/aggregates; other
                /// branches return cursor-wrapped sub-views.
                #[inline]
                pub fn value(&self) -> ::core::option::Option<#enum_ident<'p>> {
                    match self.cursor.meta_idx() {
                        #(#dispatch_arms,)*
                        _ => None,
                    }
                }
            }
        }
    } else {
        quote! {
            /// Typed value enum — all branches carry owned scalars or
            /// aggregates (no borrows).
            #[derive(Clone, Debug)]
            pub enum #enum_ident {
                #(#enum_variants,)*
            }

            impl<'p> #view_ident<'p> {
                /// Decode the chosen branch's value.
                #[inline]
                pub fn value(&self) -> ::core::option::Option<#enum_ident> {
                    match self.cursor.meta_idx() {
                        #(#dispatch_arms,)*
                        _ => None,
                    }
                }
            }
        }
    }
}

/// AQ.6.C — typed-enum payload classification for a single Alt
/// branch's referenced rule.
enum BranchValueShape {
    Scalar(TypeDesc),
    Aggregate(PayloadLayout),
}

/// AR.2.3 — per-branch variant: either a typed payload read or a
/// cursor-wrapped sub-view for non-payload-eligible branches.
enum BranchVariant {
    Payload(BranchValueShape),
    Cursor,
}

/// AQ.6.C — classify a branch's typed value shape. Returns `None`
/// when the branch's projected type is not payload-eligible (the
/// caller falls back to the cursor-wrapped accessor surface).
fn branch_value_shape(rule_id: bbnf_ir::RuleId, ir: &GrammarIR) -> Option<BranchValueShape> {
    if let Some(layout) = ir.payload_layouts.get(&rule_id) {
        return Some(BranchValueShape::Aggregate(layout.clone()));
    }
    let td = ir
        .types
        .iter()
        .find_map(|(rid, t)| (*rid == rule_id).then_some(t.clone()))?;
    if td.is_scalar_payload() {
        return Some(BranchValueShape::Scalar(td));
    }
    None
}

/// AQ.6.C — emit the typed read for a scalar payload branch.
fn emit_scalar_value_decode(td: &TypeDesc) -> TokenStream {
    // AS.2: Span payloads decode as `&'p str` by indexing into the input.
    if matches!(td, TypeDesc::Span) {
        return quote! {
            match __tape.payload_Span(__rec) {
                Some((lo, hi)) => &self.input[lo as usize..hi as usize],
                None => {
                    let (lo, hi) = __cursor.span();
                    &self.input[lo as usize..hi as usize]
                }
            }
        };
    }
    let ident = td.rust_ident().expect("scalar TypeDesc has rust_ident");
    let payload_fn = format_ident!("payload_{}", ident);
    let ty_ident = format_ident!("{}", ident);
    quote! {
        __tape.#payload_fn(__rec).unwrap_or(<#ty_ident as ::core::default::Default>::default())
    }
}

/// AQ.6.C — emit the typed tuple read for an aggregate-payload branch.
fn emit_aggregate_value_decode(layout: &PayloadLayout) -> TokenStream {
    let total_bytes = layout.total_bytes as usize;
    let field_zeros: Vec<TokenStream> = layout
        .fields
        .iter()
        .map(|f| match f.ty {
            TypeDesc::Span => quote! { (0_u32, 0_u32) },
            TypeDesc::F64 => quote! { 0.0_f64 },
            TypeDesc::Bool => quote! { false },
            TypeDesc::I8 => quote! { 0_i8 },
            TypeDesc::U8 => quote! { 0_u8 },
            TypeDesc::I16 => quote! { 0_i16 },
            TypeDesc::U16 => quote! { 0_u16 },
            TypeDesc::I32 => quote! { 0_i32 },
            TypeDesc::U32 => quote! { 0_u32 },
            TypeDesc::I64 => quote! { 0_i64 },
            TypeDesc::U64 => quote! { 0_u64 },
            _ => quote! { ::core::default::Default::default() },
        })
        .collect();
    let field_reads: Vec<TokenStream> = layout
        .fields
        .iter()
        .map(|f| {
            let offset = f.offset as usize;
            let size = f
                .ty
                .payload_size_bytes()
                .expect("aggregate field has payload_size") as usize;
            let end = offset + size;
            match f.ty {
                TypeDesc::Bool => quote! { __bytes[#offset] != 0 },
                TypeDesc::I8 => quote! { __bytes[#offset] as i8 },
                TypeDesc::U8 => quote! { __bytes[#offset] },
                TypeDesc::F64 => quote! {
                    f64::from_le_bytes(<[u8; 8]>::try_from(&__bytes[#offset..#end]).unwrap())
                },
                TypeDesc::I16 => quote! {
                    i16::from_le_bytes(<[u8; 2]>::try_from(&__bytes[#offset..#end]).unwrap())
                },
                TypeDesc::U16 => quote! {
                    u16::from_le_bytes(<[u8; 2]>::try_from(&__bytes[#offset..#end]).unwrap())
                },
                TypeDesc::I32 => quote! {
                    i32::from_le_bytes(<[u8; 4]>::try_from(&__bytes[#offset..#end]).unwrap())
                },
                TypeDesc::U32 => quote! {
                    u32::from_le_bytes(<[u8; 4]>::try_from(&__bytes[#offset..#end]).unwrap())
                },
                TypeDesc::I64 => quote! {
                    i64::from_le_bytes(<[u8; 8]>::try_from(&__bytes[#offset..#end]).unwrap())
                },
                TypeDesc::U64 => quote! {
                    u64::from_le_bytes(<[u8; 8]>::try_from(&__bytes[#offset..#end]).unwrap())
                },
                TypeDesc::Span => quote! {
                    {
                        let __raw = u64::from_le_bytes(
                            <[u8; 8]>::try_from(&__bytes[#offset..#end]).unwrap()
                        );
                        (__raw as u32, (__raw >> 32) as u32)
                    }
                },
                _ => unreachable!("aggregate layout planner only admits scalar TypeDescs"),
            }
        })
        .collect();
    quote! {
        match __tape.payload_bytes(__rec, #total_bytes) {
            Some(__bytes) => ( #(#field_reads),* ),
            None => ( #(#field_zeros),* ),
        }
    }
}

/// Emit the Rust type token for an aggregate field in a typed-enum
/// variant. `TypeDesc::Span` fields are stored as packed `(u32, u32)`
/// pairs — NOT as `parse_that::Span<'_>`.
fn aggregate_field_type_for_enum(td: &TypeDesc) -> TokenStream {
    if matches!(td, TypeDesc::Span) {
        return quote! { (u32, u32) };
    }
    let ident = td
        .rust_ident()
        .expect("aggregate field has rust_ident");
    let ty = format_ident!("{}", ident);
    quote! { #ty }
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
