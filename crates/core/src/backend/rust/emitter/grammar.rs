//! Grammar-level emission for the Rust backend — AX.W0b shape-dispatch era.
//!
//! Post-W0b the Rust backend emits no per-rule parse functions and
//! no walker. `emit_rule_function_impl` is retained as an empty
//! shim so the driver's call pipeline compiles; sibling per-rule
//! emitter modules were dismantled in AW-I.W4β. The `parse()` entry
//! point emitted by `emit_grammar_impl` routes through the
//! shape dispatcher unconditionally.
//!
//! `materialization_for_rule_pub` is preserved because the driver's
//! `pre_compile_rule_body` hook consults it to set up AM.3 tape
//! surgery context.

use bbnf_ir::passes::{MaterializationClass, PayloadLayout};
use bbnf_ir::{GrammarIR, IrRule, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::backend::driver::analysis::BackendAnalysis;
use crate::backend::rust::view::named_types::RustNamedTypes;
use bbnf_ir::passes::NamedTypeResolver;

use super::dfa_codegen;
use super::{RustEmitCtx, RustEmitter};

/// AY.W6.2 — grammar-derived direct-to-struct admission.
///
/// Walks every non-transparent rule and admits it to the direct-to-
/// struct projection surface whenever grammar-derived type-inference
/// facts prove the rule's child sequence projects cleanly onto a
/// fixed-layout scalar tuple. The admission driver is
/// [`GrammarIR::payload_layouts`] — populated upstream by
/// [`bbnf_ir::passes::compute_payload_layouts_with_resolver`] — which
/// already reflects:
///
/// - multi-field `TypeDesc::Tuple(scalar_fields)` projections (CSS L4
///   `length` / `angle` / `time` / `frequency` / `resolution` / `flex`
///   / `percentage`, JSON `pair`, CSS L4 `dimension`s, …),
/// - backend-resolved `TypeDesc::Named(sid)` projections via
///   [`RustNamedTypes`] (CSS L4 `colorFn` — `(u8, f64, f64, f64, f64)`;
///   JSON `string` — `(u32, u32)`),
/// - bare-`Span` token rules admitted as single-field aggregates
///   (Sheets identifiers / literals, BBNF identifiers / comments, …),
/// - scalar-Alt rules (Sheets `add_op` / `mul_op` / `unary_prefix` /
///   `compare_op` / `boolean`, CSS L4 `*Unit`s, …).
///
/// All four admission arms are unified in the IR layout pass; the
/// emitter here consumes the resulting
/// [`PayloadLayout`] directly. There is no grammar-name dispatch; no
/// hand-enumerated binding table; no per-grammar branch. The
/// admission fact is the grammar-derived layout.
///
/// # Emitted artefacts per grammar
///
/// 1. A `pub struct <Grammar><RuleCamel>Projection { field_<i>: <Ty>,
///    … }` per admitted rule. Field types mirror the layout's
///    [`PayloadField::ty`] in declaration order; field offsets map
///    to the layout's byte offsets. The struct is `Copy + Clone +
///    Debug`; zero-copy for `Span` via a `(u32, u32)` pair
///    representation and owned for non-Span scalars.
/// 2. A `PROJECTION_DIRECT_TO_STRUCT` const listing
///    `(rule_name, struct_name)` per admission. `struct_name` is
///    always the synthesised `<Grammar><RuleCamel>Projection` struct
///    emitted alongside this const; the grammar-declared `-> Name`
///    binding rides on the parallel `PROJECTION_NAMED_BINDINGS` slice
///    and on the struct's `NAMED_BINDING` associated const. Downstream
///    consumers consult the list for introspection + wire-contract
///    tests.
/// 3. Per-admission marker functions: `__grammar_projection_<rule>()`
///    returning the `(rule_name, field_count, named_binding)` triple.
///    One marker per admission; the legacy resolver-shim surface
///    retired at AY-II.W0.d (every admission now emits a runnable
///    materialiser, not just a marker).
fn emit_direct_to_struct_projection(ir: &GrammarIR, grammar_name: &str) -> TokenStream {
    let resolver = RustNamedTypes::from_ir(ir);
    let admissions = collect_projection_admissions(ir, &resolver);
    let grammar_prefix = to_upper_camel(grammar_name);

    // `PROJECTION_DIRECT_TO_STRUCT` entries. Post-AY-II.W0.d the
    // projection label ALWAYS names the synthesised struct
    // `<Grammar><RuleCamel>Projection` — no resolver-bound name dispatch.
    // Downstream consumers introspect via the rule-name key; the grammar-
    // declared `-> Name` binding (when present) rides on
    // `PROJECTION_NAMED_BINDINGS` below.
    let entries: Vec<TokenStream> = admissions
        .iter()
        .map(|a| {
            let rule_lit = proc_macro2::Literal::string(&a.rule_name);
            let struct_ident = a.struct_ident(&grammar_prefix);
            let struct_name = struct_ident.to_string();
            let bind_lit = proc_macro2::Literal::string(&struct_name);
            quote! { (#rule_lit, #bind_lit) }
        })
        .collect();
    let count = entries.len();

    // AY-II.W0.d — parallel metadata slice: grammar-declared `-> Name`
    // bindings, indexed in lockstep with `PROJECTION_DIRECT_TO_STRUCT`.
    // An entry of `""` signals "no named binding" (the admission came
    // from the pure scalar-tuple / KV-pair / bare-Span / scalar-Alt arm);
    // an entry like `"Color"` records that the grammar author spelt
    // `-> Color`. Readable by the typed-accessor surface tests + any
    // downstream consumer wanting the semantic-type hint.
    let named_binding_entries: Vec<TokenStream> = admissions
        .iter()
        .map(|a| {
            let bind_lit = proc_macro2::Literal::string(
                a.named_binding.as_deref().unwrap_or(""),
            );
            quote! { #bind_lit }
        })
        .collect();

    // AY-II.W0.d — parallel slice: materialiser function names,
    // indexed in lockstep with `PROJECTION_DIRECT_TO_STRUCT`. Canonical
    // evidence that every admission has a matching
    // `materialize_projection_<rule>_<Grammar>` fn emitted by
    // `shapes/value_materialize.rs`. The test surface asserts
    // `PROJECTION_DIRECT_TO_STRUCT.len() == PROJECTION_MATERIALIZERS.len()`
    // per grammar; a regression that drops the materialiser for any
    // admission surfaces as a length mismatch.
    let materializer_entries: Vec<TokenStream> = admissions
        .iter()
        .map(|a| {
            let fn_name = format!(
                "materialize_projection_{}_{}",
                sanitise_ident(&a.rule_name),
                grammar_name,
            );
            let fn_lit = proc_macro2::Literal::string(&fn_name);
            quote! { #fn_lit }
        })
        .collect();

    // AY-II.W0.d — parallel slice: consumer surface names. Every
    // admitted rule participates in the grammar's `<Grammar>Value`
    // enum via a variant whose name matches the rule name (per
    // `view/value.rs::collect_variant_classes`). The slice records
    // the fully-qualified consumer `<Grammar>Value::<RuleName>`
    // identifier — the wire-contract totality test reflects on both
    // the const length AND the matching `<Grammar>Value` variant
    // existence, catching both "admission without consumer" and
    // "consumer without admission" regressions.
    let consumer_entries: Vec<TokenStream> = admissions
        .iter()
        .map(|a| {
            let consumer_name = format!("{}Value::{}", grammar_name, a.rule_name);
            let consumer_lit = proc_macro2::Literal::string(&consumer_name);
            quote! { #consumer_lit }
        })
        .collect();

    let struct_defs = emit_projection_structs(grammar_name, &admissions);
    let projection_markers = emit_grammar_projection_markers(&admissions);

    // `PROJECTION_DIRECT_TO_STRUCT` emits unconditionally — every grammar
    // carries the const (length 0 when no rule admitted), so downstream
    // consumers (wire-contract tests, `<Grammar>::PROJECTION_DIRECT_TO_STRUCT`
    // associated-const aliasing) never hit a "missing item" resolution
    // error. The per-admission struct / marker streams are inherently
    // empty when admissions is empty; they emit nothing.
    quote! {
        #struct_defs

        /// AY-II.W0.d — per-grammar direct-to-struct projection
        /// admissions, derived from `ir.payload_layouts` + the
        /// `RustNamedTypes` resolver.
        ///
        /// Each `(rule_name, struct_name)` pair identifies a
        /// non-transparent rule whose projection admits direct-to-
        /// struct storage. `struct_name` is ALWAYS the synthesised
        /// `<Grammar><RuleCamel>Projection` struct emitted alongside
        /// this const — no resolver-bound name dispatch.
        pub const PROJECTION_DIRECT_TO_STRUCT: &[(&str, &str); #count] = &[
            #(#entries),*
        ];

        /// AY-II.W0.d — grammar-declared `-> Name` bindings, indexed in
        /// lockstep with `PROJECTION_DIRECT_TO_STRUCT`. Empty string for
        /// admissions that did not spell a named type.
        #[doc(hidden)]
        pub const PROJECTION_NAMED_BINDINGS: &[&str; #count] = &[
            #(#named_binding_entries),*
        ];

        /// AY-II.W0.d — canonical evidence that every admission has a
        /// matching `materialize_projection_<rule>_<Grammar>` fn.
        /// Indexed in lockstep with `PROJECTION_DIRECT_TO_STRUCT`; the
        /// wire-contract totality test asserts both slices share the
        /// same length per grammar.
        #[doc(hidden)]
        pub const PROJECTION_MATERIALIZERS: &[&str; #count] = &[
            #(#materializer_entries),*
        ];

        /// AY-II.W0.d — canonical evidence that every admission has a
        /// matching `<Grammar>Value::<RuleName>` enum variant
        /// (production consumer). Indexed in lockstep with
        /// `PROJECTION_DIRECT_TO_STRUCT`.
        #[doc(hidden)]
        pub const PROJECTION_CONSUMERS: &[&str; #count] = &[
            #(#consumer_entries),*
        ];

        #projection_markers
    }
}

/// AY-II.W0.d — grammar-derived field kind for a projection struct.
///
/// The scalar-only aggregate admission (packed in
/// `PayloadData::Aggregate` / `LargeAggregate`) mandates every field be
/// `is_scalar_payload()`. Richer admissions — backend-resolver-named
/// rules like CSS L4 `colorFn` whose resolver shape contains `BoxedEnum`
/// or nested `Tuple` fields — fall outside the packed buffer's
/// invariant; they project a child-cursor handle per non-scalar field
/// instead. The emitter routes by field kind; the projection struct's
/// field types mirror the kind directly.
#[derive(Clone, Debug)]
pub(crate) enum ProjectionFieldKind {
    /// Packed scalar field at a byte offset within
    /// `PayloadData::Aggregate`. Type is any `is_scalar_payload()`.
    Scalar { ty: TypeDesc, offset: u8 },
    /// Child-cursor handle at the given position among the compound's
    /// direct children. The struct field is a `<Grammar>NodeView<'p>`;
    /// the materialiser fetches `view.child(child_idx)`. AY-II.W0'.c
    /// retires the dead `ty: TypeDesc` slot that W0.d staged for a
    /// W2-era typed-variant consumer that never landed — the
    /// composer re-adds it when the consumer lands.
    CursorChild { child_idx: usize },
}

/// AY-II.W0.d — grammar-derived field layout for a projection struct.
///
/// A [`PayloadLayout`]-backed admission surfaces as a sequence of
/// [`ProjectionFieldKind::Scalar`] fields mirroring the layout's
/// packed buffer; a resolver-named admission without a scalar-only
/// layout surfaces as a mix of `Scalar` (for `is_scalar_payload()`
/// fields) and `CursorChild` (for compound fields).
#[derive(Clone, Debug)]
pub(crate) struct ProjectionFieldPlan {
    pub(crate) fields: Vec<ProjectionFieldKind>,
    /// Total bytes occupied by the packed portion of the payload.
    /// `0` when every field is a `CursorChild` (no aggregate buffer).
    pub(crate) packed_bytes: u8,
    /// True when at least one field is a `CursorChild` — the
    /// projection struct gains a `'p` lifetime parameter and the
    /// materialiser walks `view.child(i)` per child slot.
    pub(crate) has_cursor_fields: bool,
}

/// AY-II.W0.d — one admitted direct-to-struct projection.
///
/// Every admission carries a grammar-derived [`ProjectionFieldPlan`]
/// whose fields are emitted 1:1 into the synthesised
/// `<Grammar><RuleCamel>Projection` struct, the matching
/// `materialize_projection_<rule>_<Grammar>` fn, and the
/// `PROJECTION_DIRECT_TO_STRUCT` entry. The `named_binding` slot
/// carries the grammar-declared `-> Name` label (`"Color"`, `"String"`,
/// …) when a `TypeDesc::Named(sid)` drove the admission; it is
/// metadata only, never a dispatch predicate.
#[derive(Clone, Debug)]
pub(crate) struct ProjectionAdmission {
    /// Grammar rule name (matches `ir.get_string(rule.name)`).
    rule_name: String,
    /// Grammar-declared type binding for a `-> Name` annotation;
    /// `None` when the admission came from the plain layout arm.
    named_binding: Option<String>,
    /// Field plan — scalar offsets for packed-buffer fields, child
    /// indices for cursor-backed fields.
    plan: ProjectionFieldPlan,
}

impl ProjectionAdmission {
    /// Borrow the rule name.
    pub(crate) fn rule_name(&self) -> &str {
        &self.rule_name
    }

    /// Borrow the field plan.
    pub(crate) fn plan(&self) -> &ProjectionFieldPlan {
        &self.plan
    }

    /// Grammar-declared `-> Name` binding or empty string.
    pub(crate) fn named_binding_str(&self) -> &str {
        self.named_binding.as_deref().unwrap_or("")
    }

    /// Synthesised projection struct name — `<Grammar><RuleCamel>Projection`.
    pub(crate) fn struct_ident(&self, grammar_prefix: &str) -> syn::Ident {
        format_ident!(
            "{}{}Projection",
            grammar_prefix,
            to_upper_camel(&self.rule_name),
        )
    }
}

/// AY-II.W0.d — unified admission walk.
///
/// Produces the ordered list of admitted projections. The walk is
/// deterministic: rule order mirrors `ir.rules` declaration order;
/// each rule contributes at most one admission. A rule admits when
/// either (a) `ir.payload_layouts` carries a non-empty
/// [`PayloadLayout`] for it (scalar packed admission), or (b) the
/// rule's type is `TypeDesc::Named(sid)` and the backend resolver
/// returns a tuple shape (rich resolver-backed admission — the layout
/// pass may have declined because of non-scalar fields, but the
/// resolver still knows the declared shape). Scalar fields in the
/// rich admission get a packed offset assignment; non-scalar fields
/// become `CursorChild` handles in body-declaration order.
pub(crate) fn collect_projection_admissions(
    ir: &GrammarIR,
    resolver: &RustNamedTypes<'_>,
) -> Vec<ProjectionAdmission> {
    let mut admissions = Vec::new();
    for rule in &ir.rules {
        if rule.meta.is_transparent {
            continue;
        }
        let rule_name = ir.get_string(rule.name).to_string();

        let type_desc = ir
            .types
            .iter()
            .find_map(|(id, ty)| (*id == rule.id).then_some(ty));

        // Admission arm 1 — scalar-only packed buffer via the layout
        // pass. Every field is `is_scalar_payload()`; the materialiser
        // reads `payload_bytes` at the admitted offsets.
        if let Some(layout) = ir.payload_layouts.get(&rule.id) {
            if !layout.fields.is_empty() {
                let plan = plan_from_payload_layout(layout);
                let named_binding = type_desc
                    .and_then(|td| match td {
                        TypeDesc::Named(sid)
                            if resolver.resolve_named(*sid).is_some() =>
                        {
                            Some(ir.get_string(*sid).to_string())
                        }
                        _ => None,
                    });
                admissions.push(ProjectionAdmission {
                    rule_name,
                    named_binding,
                    plan,
                });
                continue;
            }
        }

        // Admission arm 2 — resolver-backed rich projection. The
        // layout pass declined (non-scalar fields present in the
        // resolver's tuple shape), but the backend resolver still
        // knows the grammar-declared field sequence. Emit the
        // projection with scalar fields packed and non-scalar fields
        // as cursor handles, so the totality invariant holds across
        // every grammar-declared `-> Name` admission. This is the
        // AY-II.W0.d closure of AUDIT-B §4's 2-shim gap:
        // post-W0.d every admission emits one struct + one
        // materialiser + one consumer.
        if let Some(TypeDesc::Named(sid)) = type_desc {
            if let Some(TypeDesc::Tuple(fields)) = resolver.resolve_named(*sid) {
                if !fields.is_empty() {
                    let plan = plan_from_resolver_tuple(&fields);
                    let binding_name = ir.get_string(*sid).to_string();
                    admissions.push(ProjectionAdmission {
                        rule_name,
                        named_binding: Some(binding_name),
                        plan,
                    });
                    continue;
                }
            }
        }
    }
    admissions
}

/// AY-II.W0.d — synthesise a [`ProjectionFieldPlan`] from a scalar-only
/// [`PayloadLayout`]. Every field is `Scalar` with its layout offset.
fn plan_from_payload_layout(layout: &PayloadLayout) -> ProjectionFieldPlan {
    let fields = layout
        .fields
        .iter()
        .map(|f| ProjectionFieldKind::Scalar {
            ty: f.ty.clone(),
            offset: f.offset,
        })
        .collect();
    ProjectionFieldPlan {
        fields,
        packed_bytes: layout.total_bytes,
        has_cursor_fields: false,
    }
}

/// AY-II.W0.d — synthesise a [`ProjectionFieldPlan`] from a
/// resolver-provided tuple shape. Scalar fields pack into the
/// aggregate buffer using the same natural-alignment walk
/// [`bbnf_ir::passes::plan_layout_with_cap`] uses; non-scalar fields
/// project to [`ProjectionFieldKind::CursorChild`] handles in
/// declaration order. The emitted materialiser reads scalars from
/// the packed buffer when one is present and walks the compound's
/// direct children to populate cursor fields.
fn plan_from_resolver_tuple(fields: &[TypeDesc]) -> ProjectionFieldPlan {
    let mut kinds = Vec::with_capacity(fields.len());
    let mut packed: u8 = 0;
    let mut child_cursor: usize = 0;
    let mut any_cursor = false;
    for ty in fields {
        if ty.is_scalar_payload() {
            let size = ty.payload_size_bytes().unwrap_or(0);
            let align = ty.payload_align_bytes().unwrap_or(1).max(1);
            let offset = (packed + align - 1) & !(align - 1);
            kinds.push(ProjectionFieldKind::Scalar {
                ty: ty.clone(),
                offset,
            });
            packed = offset.saturating_add(size);
        } else {
            kinds.push(ProjectionFieldKind::CursorChild {
                child_idx: child_cursor,
            });
            child_cursor += 1;
            any_cursor = true;
        }
    }
    ProjectionFieldPlan {
        fields: kinds,
        packed_bytes: packed,
        has_cursor_fields: any_cursor,
    }
}

/// AY-II.W0.d — emit the `pub struct <Grammar><RuleCamel>Projection`
/// definitions for every admission. Post-W0.d a struct is emitted
/// uniformly for every rule in `collect_projection_admissions`: both
/// scalar-packed layout admissions and resolver-backed rich admissions
/// emit through this one path. Rich admissions (cursor-child fields
/// present) gain a `'p` lifetime parameter and lose the `Copy` marker;
/// packed admissions stay plain-data.
fn emit_projection_structs(
    grammar_name: &str,
    admissions: &[ProjectionAdmission],
) -> TokenStream {
    let mut structs: Vec<TokenStream> = Vec::new();
    let grammar_prefix = to_upper_camel(grammar_name);
    let grammar_node_view = format_ident!("{}NodeView", grammar_name);
    for admission in admissions {
        let struct_ident = admission.struct_ident(&grammar_prefix);
        let rule_name_lit = proc_macro2::Literal::string(&admission.rule_name);
        let named_binding_lit = proc_macro2::Literal::string(
            admission.named_binding_str(),
        );
        let plan = admission.plan();
        let total_bytes_lit = proc_macro2::Literal::u8_unsuffixed(plan.packed_bytes);
        let field_count_lit = proc_macro2::Literal::usize_unsuffixed(plan.fields.len());
        let fields: Vec<TokenStream> = plan
            .fields
            .iter()
            .enumerate()
            .map(|(idx, kind)| emit_projection_field(idx, kind, &grammar_node_view))
            .collect();

        // Rich projections with cursor children carry a lifetime;
        // Copy cannot coexist with `NodeView<'p>` field types, so the
        // derive list splits by kind. Packed-only admissions retain
        // the legacy `Copy + Clone + Debug` triple.
        let (decl_generics, impl_generics, derive_attrs) = if plan.has_cursor_fields {
            (
                quote! { <'p> },
                quote! { <'p> },
                quote! {
                    #[derive(::core::clone::Clone, ::core::fmt::Debug)]
                },
            )
        } else {
            (
                quote! {},
                quote! {},
                quote! {
                    #[derive(
                        ::core::marker::Copy,
                        ::core::clone::Clone,
                        ::core::fmt::Debug,
                    )]
                },
            )
        };
        let impl_target = if plan.has_cursor_fields {
            quote! { #struct_ident<'p> }
        } else {
            quote! { #struct_ident }
        };

        structs.push(quote! {
            /// AY-II.W0.d — grammar-derived direct-to-struct projection.
            ///
            /// Emitted storage for a rule whose child sequence projects
            /// onto a fixed-layout tuple. Packed admissions read every
            /// field from `Tape::payload_bytes` at scalar offsets; rich
            /// (resolver-backed) admissions mix scalar payload reads with
            /// per-child cursor handles — the materialiser walks
            /// `view.child(i)` at the admitted `CHILD_INDICES` to
            /// populate cursor fields.
            ///
            /// `NAMED_BINDING` is `""` when the admission came from a
            /// pure layout arm; non-empty when the grammar author spelt
            /// a `-> Name` annotation. Consumers that want a semantic-
            /// type hint (e.g. CSS `"Color"`) read this const.
            #derive_attrs
            #[doc(hidden)]
            pub struct #struct_ident #decl_generics {
                #(#fields),*
            }

            impl #impl_generics #impl_target {
                /// Grammar-declared rule that projects into this
                /// struct. Matches the `rule_name` entry in
                /// `PROJECTION_DIRECT_TO_STRUCT`.
                #[doc(hidden)]
                pub const RULE_NAME: &'static str = #rule_name_lit;

                /// Grammar-declared `-> Name` binding; empty string
                /// when the admission came from a pure layout arm.
                #[doc(hidden)]
                pub const NAMED_BINDING: &'static str = #named_binding_lit;

                /// Number of fields (scalar + cursor) the layout pass
                /// admitted for this projection.
                #[doc(hidden)]
                pub const FIELD_COUNT: usize = #field_count_lit;

                /// Total bytes the projection's packed portion occupies
                /// in the aggregate payload buffer; `0` when every
                /// field is a cursor handle.
                #[doc(hidden)]
                pub const TOTAL_BYTES: u8 = #total_bytes_lit;
            }
        });
    }
    quote! { #(#structs)* }
}

/// AY-II.W0.d — emit one field of a grammar-derived projection struct.
///
/// Scalar kinds map to their Rust primitive (`Span` projects to a
/// `(u32, u32)` pair so packed-only structs stay `Copy`). Cursor kinds
/// map to the grammar's `<Grammar>NodeView<'p>` handle, opting the
/// containing struct out of `Copy` in exchange for full child access.
fn emit_projection_field(
    idx: usize,
    kind: &ProjectionFieldKind,
    node_view_ident: &syn::Ident,
) -> TokenStream {
    let field_ident = format_ident!("field_{}", idx);
    match kind {
        ProjectionFieldKind::Scalar { ty, offset } => {
            let offset_lit = proc_macro2::Literal::u8_unsuffixed(*offset);
            let ty_tokens = projection_field_type(ty);
            quote! {
                /// Grammar-declared scalar field at packed-buffer offset
                #[doc = concat!("`", stringify!(#offset_lit), "` (bytes).")]
                pub #field_ident: #ty_tokens
            }
        }
        ProjectionFieldKind::CursorChild { child_idx, .. } => {
            let child_idx_lit = proc_macro2::Literal::usize_unsuffixed(*child_idx);
            quote! {
                /// Grammar-declared compound child at cursor position
                #[doc = concat!("`", stringify!(#child_idx_lit), "` (child index).")]
                pub #field_ident: #node_view_ident<'p>
            }
        }
    }
}

/// AY.W6.2 — Rust backend type for a layout field.
///
/// Span projects to `(u32, u32)` (offset + length) so the struct is
/// `Copy` without a lifetime; every other scalar maps to its
/// natural Rust primitive via [`TypeDesc::rust_ident`].
fn projection_field_type(ty: &TypeDesc) -> TokenStream {
    match ty {
        TypeDesc::Span => quote! { (u32, u32) },
        other => {
            let ident = other
                .rust_ident()
                .expect(
                    "AY.W6.2: grammar-derived projection field type \
                     must map to a Rust scalar via TypeDesc::rust_ident",
                );
            let ty_ident = format_ident!("{}", ident);
            quote! { #ty_ident }
        }
    }
}

/// AY-II.W0.d — emit `__grammar_projection_<rule>` markers for every
/// admitted rule. One marker per admission — the resolver-backed arm
/// (Named + resolver hit) and the pure layout arm share identical
/// marker shape. The returned `(rule_name, field_count,
/// named_binding)` triple lets the `cargo expand` hard gate verify
/// admission without re-inspecting the IR.
fn emit_grammar_projection_markers(
    admissions: &[ProjectionAdmission],
) -> TokenStream {
    let markers: Vec<TokenStream> = admissions
        .iter()
        .map(|a| {
            let fn_ident = format_ident!(
                "__grammar_projection_{}",
                sanitise_ident(&a.rule_name),
            );
            let rule_lit = proc_macro2::Literal::string(&a.rule_name);
            let count_lit = proc_macro2::Literal::usize_unsuffixed(
                a.plan.fields.len(),
            );
            let binding_lit = proc_macro2::Literal::string(
                a.named_binding_str(),
            );
            quote! {
                /// AY-II.W0.d marker — structural evidence that the
                /// layout pass + resolver admitted this rule for
                /// direct-to-struct projection. The returned
                /// `(rule_name, field_count, named_binding)` triple
                /// exposes the admitted shape to the `cargo expand`
                /// hard gate without requiring a runtime compilation.
                #[doc(hidden)]
                #[inline(always)]
                pub fn #fn_ident() -> (&'static str, usize, &'static str) {
                    (#rule_lit, #count_lit, #binding_lit)
                }
            }
        })
        .collect();
    quote! { #(#markers)* }
}

/// AY.W6.2 — upper-camel-case a rule/grammar name for ident
/// synthesis. Preserves existing upper-case starts; title-cases
/// lower-case first chars.
fn to_upper_camel(name: &str) -> String {
    let mut out = String::with_capacity(name.len());
    let mut upper_next = true;
    for ch in name.chars() {
        if ch == '_' || ch == '-' || ch == '.' {
            upper_next = true;
            continue;
        }
        if upper_next {
            out.extend(ch.to_uppercase());
            upper_next = false;
        } else {
            out.push(ch);
        }
    }
    out
}

/// AY.W6.2 — sanitise a rule name into a lowercase Rust ident slug.
/// Non-alphanumeric characters become underscores; leading digits are
/// prefixed with `r_` so the resulting ident is valid.
fn sanitise_ident(name: &str) -> String {
    let mut out = String::with_capacity(name.len());
    for (idx, ch) in name.chars().enumerate() {
        if ch.is_ascii_alphanumeric() {
            if idx == 0 && ch.is_ascii_digit() {
                out.push_str("r_");
            }
            out.extend(ch.to_lowercase());
        } else {
            out.push('_');
        }
    }
    if out.is_empty() {
        out.push('_');
    }
    out
}

/// AW-V.W3.2 — emit the per-grammar shared helpers the shape fns
/// consume — JSON string escape decoder, number fallback, etc.
/// Emitted once per grammar; unused helpers are dead-code-eliminated
/// by LLVM.
fn emit_shape_helpers(grammar_ident_str: &str, ir: &GrammarIR) -> TokenStream {
    use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
    let grammar_suffix = super::shapes::sanitise_grammar(grammar_ident_str);
    let mut helpers: Vec<TokenStream> = Vec::new();
    // String escape helper — emit when the grammar has any
    // String-shape rule.
    if ir
        .rules
        .iter()
        .any(|r| matches!(ir.shape_assignments.get(r.id), ShapeTag::String))
    {
        helpers.push(super::shapes::string::emit_escape_helper(&grammar_suffix));
        // AW-V.W3-bench-fix — visitor-path escape helper (separate fn
        // so visitor-generic instantiation lives alongside the tape
        // path without reusing the monomorphised tape-path body).
        helpers.push(super::shapes::string::emit_visitor_escape_helper(
            &grammar_suffix,
        ));
    }
    // Number fallback helper — emit when the grammar has any
    // Number-shape rule.
    if ir
        .rules
        .iter()
        .any(|r| matches!(ir.shape_assignments.get(r.id), ShapeTag::Number))
    {
        helpers.push(super::shapes::number::emit_number_fallback_helper());
        // AW-V.W3-bench-fix — aarch64 NEON fraction SIMD accumulator.
        // Mirrors the prototype's `simd_str2int`; canada.json's
        // 15-digit fractions amortise across the 16-byte stripe.
        helpers.push(
            super::shapes::number::emit_number_simd_fraction_helper(),
        );
    }
    quote! { #(#helpers)* }
}

impl RustEmitter {
    pub(super) fn emit_fused_number_rule_impl(
        &mut self,
        rule: &IrRule,
        _ir: &GrammarIR,
        ctx: &mut RustEmitCtx,
    ) -> Option<TokenStream> {
        if rule.meta.is_transparent {
            return None;
        }
        // AQ.6.A: when payload_type is F64, capture the scanned
        // value into `__payload_f64` so the epilogue can store it
        // via `PayloadData::WideScalar` (AU.6.7). Otherwise discard
        // as before.
        //
        // `fused_number_rules` is exclusively the strict numeric
        // shape (`reject_leading_zero: true`), so unconditionally use
        // `scan_number_strict_f64`.
        if ctx.has_payload_type(&TypeDesc::F64) {
            let tag_set = ctx.payload_tag(&TypeDesc::F64).map(|tag| {
                quote! { __payload_tag = #tag; }
            });
            Some(quote! {
                match ::parse_that::scan_number_strict_f64(state) {
                    Some(__v) => { __payload_f64 = __v; #tag_set __has_payload = true; Some(()) }
                    None => None,
                }
            })
        } else {
            // AU.6.5 no-value-discard: return the scanner's
            // `Option<f64>` directly; enclosing callers match via
            // `Some(_)` which is payload-agnostic.
            Some(quote! {
                ::parse_that::scan_number_strict_f64(state)
            })
        }
    }

    /// Look up the materialization class for a rule's body node.
    ///
    /// Identity-bearing rules — the grammar entry and any rule with
    /// `preserve_identity` set — always resolve to `MustTape`
    /// regardless of what the bottom-up classifier assigned: the
    /// generated `parse()` helper dispatches through the entry's
    /// `__<name>` function by name, and `preserve_identity` rules
    /// are structural-mode guarantees that each named rule has a
    /// standalone callable. Without this override the emitter would
    /// skip their function bodies and downstream references would
    /// dangle.
    ///
    /// Public via [`Self::materialization_for_rule_pub`] for the
    /// `pre_compile_rule_body` hook in `mod.rs`.
    fn materialization_for_rule(
        ir: &GrammarIR,
        rule: &IrRule,
    ) -> MaterializationClass {
        // `preserve_identity` rules must always push a compound.
        // The entry rule is NOT forced — its body classification
        // determines whether it uses push_leaf (TapeSpanOnly) or
        // push_compound (MustTape). Both produce a TapeOffset
        // valid for `Parsed::root_offset`. The view layer reads
        // variant_idx from flags, which both paths store.
        if rule.meta.preserve_identity {
            return MaterializationClass::MustTape;
        }
        // `ir.materialization` is keyed by `NodeId` via `ir.dag`.
        // A rule without a dag-mapped body defaults to `MustTape`
        // — the safest choice because it preserves every child.
        if let Some(dag) = ir.dag.as_ref() {
            if let Some(node_id) = dag.node_for(&rule.body) {
                if let Some(class) = ir.materialization.get(&node_id) {
                    return *class;
                }
            }
        }
        MaterializationClass::MustTape
    }

    /// Public accessor for `materialization_for_rule`, used by
    /// `pre_compile_rule_body` in `mod.rs` for AM.3 tape surgery
    /// context setup.
    pub(in crate::backend::rust) fn materialization_for_rule_pub(
        ir: &GrammarIR,
        rule: &IrRule,
    ) -> MaterializationClass {
        Self::materialization_for_rule(ir, rule)
    }

    /// AW-I.W4α: per-rule function emission is a no-op.
    ///
    /// The Rust backend's `parse()` dispatches through the DTA
    /// walker wholesale (see [`Self::emit_grammar_impl`]), so the
    /// per-rule `__<name>` function bodies previously assembled here
    /// are dead surface. The driver still calls into this method
    /// once per rule; returning an empty token stream drops the
    /// body without disturbing the call pipeline. W4β dismantles
    /// the sibling emitter modules that fed this path.
    pub(super) fn emit_rule_function_impl(
        &mut self,
        _rule: &IrRule,
        _body: TokenStream,
        _sync_body: Option<TokenStream>,
        _ir: &GrammarIR,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        TokenStream::new()
    }

    pub(super) fn emit_type_definitions_impl(
        &mut self,
        ir: &GrammarIR,
        _analysis: &BackendAnalysis,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Tranche AC.2 — view types replace the allocating enum.
        // `generate_views` emits one `<Rule>View<'tape>` per non-
        // transparent rule plus the `Root` trait binding.
        let ir_ctx = ctx.ir_ctx();
        let views = crate::backend::rust::view::generate_views(ir, ir_ctx);

        // AW-IV.W3.5a — direct-to-struct view-layer consumer wiring.
        //
        // The AW-III.W6.4 universal binding table lives at
        // `crates/core/src/backend/rust/view/named_types.rs::BINDINGS`
        // with a passing parity suite (JSON Value, BBNF AST, Sheets
        // formula, CSS Color). Pre-W3.5a the view emitter admitted
        // only hardcoded "Color" | "ColorMix" names via a match in
        // `view::leaves::emit_aggregate_accessors`; the resolver
        // shipped but wasn't called on the per-grammar hot path.
        //
        // W3.5a threads `resolve_named_type` into the top-level view
        // emission: for every non-transparent rule whose `TypeDesc`
        // is `Named(sid)` and whose interned name resolves via the
        // universal `BINDINGS` table, the direct-to-struct projection
        // is emitted inline on the view. The mechanism is universal
        // — JSON Value, BBNF AST, Sheets formula, CSS Color all enter
        // the fast path via the same resolver call; the shape of the
        // emitted projection comes from the binding row's
        // `NamedTypeBinding::fields`.
        //
        // The `emit_direct_to_struct_projection` pass walks every
        // rule, consults the resolver, and emits one `as_<name>()`
        // shim per admitted rule. The top-level grammar-entry rule
        // gets additional root-view wiring so downstream consumers
        // can project the full parse directly without traversing the
        // tape cursor tree manually.
        let direct_to_struct =
            emit_direct_to_struct_projection(ir, ir_ctx.ident.to_string().as_str());

        // AY.W3b.1 — `<Grammar>Value` enum + `impl ValueRoot` + narrow
        // `impl PathQuery<T>` impls. Emitted per-grammar via
        // TypeDesc-equivalence-class collapse (one variant per
        // non-transparent rule today; the collapse map widens in a
        // follow-on without disturbing consumers).
        let grammar_name_s = ir_ctx.ident.to_string();
        let value_surface = crate::backend::rust::view::emit_value_surface(
            ir,
            grammar_name_s.as_str(),
        );

        // AY.W3b.2 — json-prototype per-shape inline fns. The BEAT-
        // sonic lever: five `#[inline(always)]` per-shape fns +
        // the root dispatcher, monomorphised at the
        // `parsed.to_value()` call site so LLVM inlines the entire
        // tree-build into a single flat function.
        let materialize_fns = super::shapes::value_materialize::emit_materialize_fns(
            ir,
            grammar_name_s.as_str(),
        );

        quote! {
            #views
            #direct_to_struct
            #value_surface
            #materialize_fns
        }
    }

    pub(super) fn emit_grammar_impl(
        &mut self,
        type_defs: TokenStream,
        rule_functions: Vec<TokenStream>,
        ir: &GrammarIR,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let ir_ctx = ctx.ir_ctx();
        let ident = ir_ctx.ident;
        let parser_attrs = ir_ctx.parser_attrs;

        // Grammar string const array.
        let grammar_arr =
            crate::backend::rust::ir_enums::generate_grammar_arr(parser_attrs, ident);

        // Tranche AV Phase 1 — consolidated per-grammar fingerprint.
        // Lowers `GrammarIR::profile()` to a single `const
        // GRAMMAR_PROFILE: GrammarProfile = GrammarProfile { ... };`
        // literal emitted alongside the grammar string array.
        let profile = ir.profile();
        let grammar_profile = super::profile::emit_grammar_profile(&profile);

        // AW-IV.W1.4-aggro — regex-scan adapter. The shape emitters
        // consume it for every Regex / WsTrim site; its dispatch arms
        // splice inline DFA bodies so no chain of `fn` calls survives
        // on the hot path. Lifts the DTA once to read the pattern
        // set; the table is NOT emitted as runtime data.
        let grammar_name = ident.to_string();
        let dta_walker_table = bbnf_ir::passes::lift_dta(ir);
        let regex_scan_adapter = dfa_codegen::emit_regex_scan_adapter(
            grammar_name.as_str(),
            ir,
            &dta_walker_table,
        );

        // AW-III.W6.2 — emit PHF keyword tables for every literal-led
        // Alt whose mined branch count exceeds the threshold. The
        // emitted statics live at module scope alongside GRAMMAR_PROFILE
        // so the specialised walker's `AltLinear` arm (or future
        // ClassifyByte specialisations) can consult them via a binary
        // search helper fn. Per §6, the mechanism runs over every
        // grammar's Alt space; per-grammar impact varies with the
        // mined branch counts.
        let keyword_phf_tables = {
            let mut tables: Vec<(u32, &[super::keyword_dispatch::LiteralBranch])> = Vec::new();
            // Allocate owned branch buffers per rule so the borrow
            // lives long enough for emit_keyword_tables's consumption.
            let mut owned: Vec<(u32, Vec<super::keyword_dispatch::LiteralBranch>)> = Vec::new();
            for rule in &ir.rules {
                if rule.meta.is_transparent {
                    continue;
                }
                let Some(dag) = ir.dag.as_ref() else { continue };
                let Some(body_id) = dag.node_for(&rule.body) else { continue };
                if let Some(branches) = ir.keyword_branches.get(&body_id) {
                    let lits: Vec<super::keyword_dispatch::LiteralBranch> = branches
                        .iter()
                        .map(|kb| super::keyword_dispatch::LiteralBranch {
                            bytes: kb.bytes.clone(),
                            branch_idx: kb.branch_idx,
                        })
                        .collect();
                    owned.push((rule.id, lits));
                }
            }
            for (rid, lits) in owned.iter() {
                tables.push((*rid, lits.as_slice()));
            }
            super::keyword_dispatch::emit_keyword_tables(
                ident.to_string().as_str(),
                tables,
            )
        };

        // AW-III.W6.5 — per-grammar Pratt precedence LUT. Mines every
        // DtaState::ShuntingYard chain's operators from the lifted
        // DTA table and emits a packed `const PRECEDENCE_LUT: [u8; 256]`
        // plus a sparse `PRECEDENCE_ENTRIES` slice. Consulted inline by
        // the shape-dispatch Pratt body.
        let precedence_lut = {
            let chain_facts =
                bbnf_ir::passes::collect_operator_chains(ir, &dta_walker_table);
            super::precedence::emit_precedence_lut(
                ident.to_string().as_str(),
                &chain_facts,
            )
        };

        // Debug trace depth counter (only emitted if any rule
        // uses @debug).
        let has_debug = ir.debug_all || ir.rules.iter().any(|r| r.meta.directives.debug);
        let depth_counter = if has_debug {
            crate::backend::rust::trace::emit_depth_counter()
        } else {
            quote! {}
        };

        let extra = &self.extra_impl_methods;

        // AW-V.W3.2 — per-shape emitter modules.
        //
        // Walks the IR's ShapeAssignments (populated by the W3.1
        // classifier) and emits one `parse_<shape>_<grammar>_<rule>`
        // function per shape-classified rule, plus the top-level
        // `parse_<grammar>_<root>` dispatcher. The emitted stream
        // lives alongside `#dta_walker`; rules without shape match
        // continue to route through `__dta_walker_inline::run` per
        // the AX cold-path replay contract.
        //
        // When every non-transparent rule in the grammar has a W3-
        // active shape classification (JSON after W3.1 ships), the
        // grammar's `parse()` entry routes through the shape
        // dispatcher — eliminating the structural scan + PSI +
        // walker tax on the hot path. Grammars with unshaped rules
        // (CSS / Sheets / BBNF until W4 extends the detectors)
        // continue to call `dta_run_<grammar>`.
        let shape_emitters = super::shapes::emit_shapes_for_grammar(
            ident.to_string().as_str(),
            ir,
        );
        let shape_helpers = emit_shape_helpers(ident.to_string().as_str(), ir);
        // AX.W0b — every grammar routes through the shape dispatcher
        // post-W0a.2.h; the gate predicates retired with the walker.
        let shape_dispatcher_ident = super::shapes::root_rule_name(ir).map(|root| {
            super::shapes::dispatcher_fn_ident(ident.to_string().as_str(), &root)
        });
        // AW-V.W3-bench-fix — visitor-path dispatcher ident.
        let visitor_dispatcher_ident = super::shapes::root_rule_name(ir).map(|root| {
            super::shapes::visitor_dispatcher_fn_ident(
                ident.to_string().as_str(),
                &root,
            )
        });

        // AW-I.W3: `parse()` dispatches through `dta_run` wholesale.
        // The per-rule `rule_functions` stream and the trailing_ws /
        // root_fn_ident / with_capacity scaffolding previously woven
        // into the legacy body are retired — the DTA walker owns EOF,
        // root dispatch, and capacity derivation. `rule_functions` is
        // intentionally accepted (the upstream pipeline still compiles
        // per-rule fragments) and discarded here; W4β removes the
        // upstream compilation step once the sibling emitter modules
        // are deleted.
        //
        // AW-IV.W1.4-aggro — the DtaDfaScanner ZST + RegexScanner impl
        // + DTA_SCANNER const all delete. The walker emitter splices
        // the DFA's `loop { match state { ... } }` body directly into
        // every Regex / WsTrim / boundary-ws site at the source level;
        // no separately-emitted `__dfa_match_*` fn exists. The
        // `#regex_scan_adapter` below is the SOLE out-of-line
        // regex-related fn emitted per grammar — used by cold-path
        // replay callers (`try_branch`, `handle_repeat_failure_bounded`)
        // that dispatch by pattern string; its dispatch arms also
        // splice inline DFA bodies, so the fn-call boundary that
        // AW-III's runtime DFA interpreter imposed (31.92% self-time
        // on JSON twitter) is gone from the hot path entirely.
        let _ = rule_functions;

        // AY-II.W0'.a — parse() routes through the shape dispatcher
        // against a single `FusedBuilder` that owns both the tape
        // column family and the paired value-frame arena. Every shape
        // emitter's `begin_compound` / `end_compound` / `push_leaf_*`
        // call stamps BOTH column families atomically inside the fused
        // builder, and `builder.finish_fused::<Self>(root_off.0)` hands
        // back one `FusedOutput<Self>` holding the finalised `Tape` +
        // the grammar-bound `ValueFramesOutput<Self>`.
        // `Parsed::new_fused_output` consumes the fused output directly
        // — no second finish call, no separate value allocation.
        let _ = visitor_dispatcher_ident;
        let parse_body = {
            let dispatcher = shape_dispatcher_ident
                .as_ref()
                .expect("shape dispatcher gated on root_rule_name");
            let support_mod_ident = quote::format_ident!(
                "__shape_support_{}",
                super::shapes::sanitise_grammar(ident.to_string().as_str()),
            );
            quote! {
                let __input_bytes = input.as_bytes();
                // AY.W1-fix — `ScanState::new()` constructs the
                // per-parse SIMD scratch (whitespace bitmap cache only).
                // AY.W1.3's eager `scan_structural` call retired here
                // after AYW1-twitter-regression-diag identified the
                // O(N) scan cost as ~50% of twitter parse self-time
                // for negligible probe benefit. The substrate stays in
                // the tape crate awaiting AY.W4's regex-scan
                // specialisation — which can wire it through CTNS-style
                // predicates that deliver material savings. Tape
                // capacity falls back to the per-grammar density
                // estimate via `GRAMMAR_PROFILE.capacity_for`.
                let mut state = #support_mod_ident::ScanState::new();
                // B5.W1 — single unified [`Tape<()>`] allocation.
                // The substrate owns both the structural columns and
                // the paired value-frame arena; every `begin_compound`
                // / `end_compound` / `push_leaf_*` writes to both in
                // lockstep, and `finish(root)` returns the finalised
                // [`Tape<()>`] with sib_skip / span_hi / child_off
                // back-patched.
                let mut tape = crate::runtime::tape::Tape::<()>::with_capacity(
                    GRAMMAR_PROFILE.capacity_for(input.len()),
                );
                let root_off = {
                    let mut pos: usize = 0;
                    let off = #dispatcher(
                        __input_bytes,
                        &mut pos,
                        &mut state,
                        &mut tape,
                    )
                    .map_err(|e| match e {
                        crate::runtime::tape::DtaError::Syntax { offset, .. } => {
                            crate::runtime::ParseErr::Syntax {
                                offset,
                                rule: None,
                            }
                        }
                        crate::runtime::tape::DtaError::UnexpectedEnd { offset } => {
                            crate::runtime::ParseErr::Syntax {
                                offset,
                                rule: None,
                            }
                        }
                        crate::runtime::tape::DtaError::InvalidState { .. } => {
                            crate::runtime::ParseErr::Syntax {
                                offset: 0,
                                rule: None,
                            }
                        }
                    })?;
                    // Trailing whitespace.
                    let _ = #support_mod_ident::skip_space(
                        __input_bytes, &mut pos, &mut state,
                    );
                    if pos != input.len() {
                        return Err(crate::runtime::ParseErr::Syntax {
                            offset: pos as u32,
                            rule: None,
                        });
                    }
                    off
                };
                // B5.W1 — `Tape::finish_fused::<Self>(root)` runs
                // Stage-C finalisation, stamps the root offset, and
                // re-binds the phantom `R` from `()` to `Self` so
                // projection-time consumers (Parsed::to_value) see
                // the grammar-bound substrate. Layout-identical
                // transmute is sound because `R` is `PhantomData<fn()
                // -> R>`.
                let tape = tape
                    .finish_fused::<Self>(root_off.0)
                    .map_err(crate::runtime::ParseErr::Tape)?;
                ::core::result::Result::Ok(
                    crate::runtime::Parsed::new(tape, input, root_off),
                )
            }
        };

        // AY-II.W0'.a — visitor-generic parse entry retired. The
        // fused parse above IS the visitor lane — every shape
        // emitter's push goes through the fused builder's atomic
        // tape + value stamping. The separate visitor-trait-bounded
        // entry duplicated the dispatcher body against an external
        // visitor trait the fused projection path supersedes;
        // retaining it would violate invariant §5 (fused pipeline is
        // real) and invariant §7 (consumer totality — every surface
        // has a production consumer). The visitor trait hierarchy
        // remains in `tape::visitor` for test fixtures that exercise
        // the trait API directly; `TapeVisitor` now emits via the
        // fused builder, so those consumers are not orphaned.

        quote! {
            use ::parse_that::*;

            #grammar_arr

            #grammar_profile

            // AW-III.W6.2 — PHF keyword tables for literal-led Alts.
            // Emitted at module scope per rule whose Alt body has
            // literal-led branches ≥ PHF_MIN_BRANCHES; consulted by
            // downstream AltLinear / ClassifyByte call sites.
            #keyword_phf_tables

            // AW-III.W6.5 — Pratt precedence LUT. Dense `[u8; 256]`
            // packed byte layout + sparse metadata slice for two-byte
            // operators. Consulted by the shape-dispatch Pratt body.
            #precedence_lut

            // AW-IV.W1.4-aggro — per-grammar regex-scan adapter.
            // Dispatches on pointer-equality of the interned pattern
            // `&'static str` statics (`__DTA_REGEX_K` / `__DTA_WS_K`);
            // each matched arm splices the corresponding DFA's loop
            // body inline. Consumed by shape emitters whose Regex /
            // WsTrim arms splice its dispatch in-line.
            #regex_scan_adapter

            // AW-V.W3.2 — per-shape emitter modules + helpers.
            #shape_helpers
            #shape_emitters

            #type_defs

            impl #ident {
                #depth_counter
                #extra

                /// AW-IV.W1.δ — associated-constant accessor for the
                /// grammar's consolidated codegen fingerprint. Alias
                /// of the module-scope `GRAMMAR_PROFILE` const; the
                /// underlying bytes live in `.rodata` once. Downstream
                /// consumers (wire-contract tests, per-grammar
                /// introspection, cross-grammar harnesses) use
                /// `<Grammar>::GRAMMAR_PROFILE` to disambiguate when
                /// multiple grammars coexist in the same test file —
                /// the module-scope `pub use ...::*` would otherwise
                /// collide on the unqualified `GRAMMAR_PROFILE` name.
                pub const GRAMMAR_PROFILE: crate::runtime::tape::GrammarProfile =
                    GRAMMAR_PROFILE;

                /// AY.W6.2 — associated-constant accessor for the
                /// grammar's direct-to-struct projection admission
                /// list. Alias of the module-scope
                /// `PROJECTION_DIRECT_TO_STRUCT` slice; downstream
                /// consumers that coexist with multiple grammars in
                /// one test binary read via
                /// `<Grammar>::PROJECTION_DIRECT_TO_STRUCT` to
                /// disambiguate.
                pub const PROJECTION_DIRECT_TO_STRUCT: &'static [(&'static str, &'static str)] =
                    PROJECTION_DIRECT_TO_STRUCT;

                /// AY-II.W0.d — grammar-declared `-> Name` bindings
                /// per admission. Indexed in lockstep with
                /// `PROJECTION_DIRECT_TO_STRUCT`; empty string when
                /// the admission came from a pure layout arm.
                #[doc(hidden)]
                pub const PROJECTION_NAMED_BINDINGS: &'static [&'static str] =
                    PROJECTION_NAMED_BINDINGS;

                /// AY-II.W0.d — materialiser function names per
                /// admission. Canonical wire-contract evidence that
                /// every `PROJECTION_DIRECT_TO_STRUCT` entry has a
                /// matching `materialize_projection_<rule>_<Grammar>`
                /// fn in the emitter output.
                #[doc(hidden)]
                pub const PROJECTION_MATERIALIZERS: &'static [&'static str] =
                    PROJECTION_MATERIALIZERS;

                /// AY-II.W0.d — production consumer names per
                /// admission. Each entry identifies the
                /// `<Grammar>Value::<RuleName>` variant that consumes
                /// the admitted rule at runtime.
                #[doc(hidden)]
                pub const PROJECTION_CONSUMERS: &'static [&'static str] =
                    PROJECTION_CONSUMERS;

                /// Parse an input string and return a zero-copy
                /// `Parsed<'_, Self>` that borrows the input directly.
                ///
                /// AY-II.W0'.a: `parse()` routes through the shape
                /// dispatcher against a single `FusedBuilder`. The
                /// hot path here:
                ///
                /// 1. Allocate a sized `FusedBuilder` — owns both
                ///    tape + value-frame substrates in one handle.
                /// 2. Call the shape dispatcher, which decomposes
                ///    into per-shape bodies inlined at the call
                ///    site. Every compound / leaf push stamps both
                ///    column families atomically.
                /// 3. Finalise via `FusedBuilder::finish_fused::<Self>`
                ///    — returns `FusedOutput<Self>` holding tape +
                ///    value, handed to `Parsed::new_fused_output` directly.
                pub fn parse(
                    input: &str,
                ) -> ::core::result::Result<
                    crate::runtime::Parsed<'_, Self>,
                    crate::runtime::ParseErr,
                > {
                    #parse_body
                }
            }
        }
    }
}
