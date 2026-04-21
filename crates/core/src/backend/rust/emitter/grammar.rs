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

use bbnf_ir::passes::{MaterializationClass, PayloadField, PayloadLayout};
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
///    either the resolver-derived `Named(sid)` label (legacy
///    `"Color"`, `"String"`) or the synthesised `<RuleCamel>Projection`
///    for grammar-derived layouts. Downstream consumers consult the
///    list for introspection + wire-contract tests.
/// 3. Per-admission marker shims: `__named_type_shim_<name>` for
///    resolver-backed named types (kept as structural evidence of the
///    legacy path) and `__grammar_projection_<rule>` for layout-
///    derived admissions (new; proof the broader mechanism fired).
fn emit_direct_to_struct_projection(ir: &GrammarIR, grammar_name: &str) -> TokenStream {
    let resolver = RustNamedTypes::from_ir(ir);
    let admissions = collect_projection_admissions(ir, &resolver);

    let entries: Vec<TokenStream> = admissions
        .iter()
        .map(|a| {
            let rule_lit = proc_macro2::Literal::string(&a.rule_name);
            let bind_lit = proc_macro2::Literal::string(&a.projection_label);
            quote! { (#rule_lit, #bind_lit) }
        })
        .collect();
    let count = entries.len();

    let struct_defs = emit_projection_structs(grammar_name, &admissions);
    let resolver_shims = emit_resolver_shims(&admissions);
    let grammar_projection_shims = emit_grammar_projection_shims(&admissions);

    // `PROJECTION_DIRECT_TO_STRUCT` emits unconditionally — every grammar
    // carries the const (length 0 when no rule admitted), so downstream
    // consumers (wire-contract tests, `<Grammar>::PROJECTION_DIRECT_TO_STRUCT`
    // associated-const aliasing) never hit a "missing item" resolution
    // error. The per-admission struct / shim streams are inherently
    // empty when admissions is empty; they emit nothing.
    quote! {
        #struct_defs

        /// AY.W6.2 — per-grammar direct-to-struct projection
        /// admissions, derived from `ir.payload_layouts` + the
        /// `RustNamedTypes` resolver.
        ///
        /// Each `(rule_name, projection_label)` pair identifies a
        /// non-transparent rule whose projection admits direct-to-
        /// struct storage. `projection_label` is either a resolver-
        /// bound name (e.g. `"Color"`, `"String"`) or the synthesised
        /// `<RuleCamel>Projection` struct emitted alongside this const.
        /// The list is the emitted evidence that the layout pass +
        /// resolver fired for this grammar; `cargo expand` surfaces
        /// it alongside the GRAMMAR_PROFILE literal.
        pub const PROJECTION_DIRECT_TO_STRUCT: &[(&str, &str); #count] = &[
            #(#entries),*
        ];

        #resolver_shims
        #grammar_projection_shims
    }
}

/// AY.W6.2 — source of a direct-to-struct projection admission.
///
/// Discriminates the two admission arms so the emitter can route
/// each to the right downstream artefact without re-inspecting the
/// IR. Both arms are grammar-derived; neither is name-dispatched.
#[derive(Clone, Debug)]
enum ProjectionSource {
    /// Admitted through [`RustNamedTypes::resolve_named`] on a
    /// `TypeDesc::Named(sid)` rule. The projection label is the
    /// resolver-bound name (`"Color"`, `"String"`, …). Emits a
    /// `__named_type_shim_<name>` marker.
    ResolverNamed { binding_name: String },
    /// Admitted through `ir.payload_layouts` — a grammar-declared
    /// scalar tuple projection whose fields are enumerated by the
    /// layout pass. Emits a `<RuleCamel>Projection` struct + a
    /// `__grammar_projection_<rule>` marker.
    GrammarLayout { layout: PayloadLayout },
}

/// AY.W6.2 — one admitted direct-to-struct projection.
///
/// Carries the rule identity, the admission source, and the
/// `projection_label` used in `PROJECTION_DIRECT_TO_STRUCT`.
#[derive(Clone, Debug)]
struct ProjectionAdmission {
    rule_name: String,
    projection_label: String,
    source: ProjectionSource,
}

/// AY.W6.2 — unified admission walk.
///
/// Produces the ordered list of admitted projections. The walk is
/// deterministic: rule order mirrors `ir.rules` declaration order;
/// each rule contributes at most one admission (the resolver arm
/// dominates the layout arm when both would fire, since the
/// resolver-bound layout IS the grammar-declared `Named(sid)`
/// projection — collapsing both to one entry prevents double-
/// counting).
fn collect_projection_admissions(
    ir: &GrammarIR,
    resolver: &RustNamedTypes<'_>,
) -> Vec<ProjectionAdmission> {
    let mut admissions = Vec::new();
    for rule in &ir.rules {
        if rule.meta.is_transparent {
            continue;
        }
        let rule_name = ir.get_string(rule.name).to_string();

        // Arm 1 — `TypeDesc::Named(sid)` with a resolver hit. Mirrors
        // the pre-W6.2 admission; kept as the legacy entry point so
        // downstream consumers reading `("colorFn", "Color")` /
        // `("string", "String")` continue working. Admission here
        // suppresses the broader layout arm for the same rule.
        let type_desc = ir
            .types
            .iter()
            .find_map(|(id, ty)| (*id == rule.id).then_some(ty));
        if let Some(TypeDesc::Named(sid)) = type_desc {
            if resolver.resolve_named(*sid).is_some() {
                let binding_name = ir.get_string(*sid).to_string();
                admissions.push(ProjectionAdmission {
                    rule_name,
                    projection_label: binding_name.clone(),
                    source: ProjectionSource::ResolverNamed { binding_name },
                });
                continue;
            }
        }

        // Arm 2 — grammar-derived layout. Admit any rule whose
        // layout pass produced a [`PayloadLayout`] with at least one
        // scalar field. Rules whose layout exists but is empty are
        // not projected (they have no direct-to-struct shape to
        // admit); rules without a layout entry fall through to the
        // compound / scalar paths upstream.
        if let Some(layout) = ir.payload_layouts.get(&rule.id) {
            if layout.fields.is_empty() {
                continue;
            }
            let projection_label = format!(
                "{}Projection",
                to_upper_camel(&rule_name),
            );
            admissions.push(ProjectionAdmission {
                rule_name,
                projection_label,
                source: ProjectionSource::GrammarLayout { layout: layout.clone() },
            });
        }
    }
    admissions
}

/// AY.W6.2 — emit the `pub struct <Grammar><RuleCamel>Projection`
/// definitions for every layout-derived admission. Resolver-derived
/// admissions keep their legacy `__named_type_shim_*` surface; the
/// resolver's backend-specific shape is already consumed by the
/// payload layout pipeline at analyze-grammar time.
fn emit_projection_structs(
    grammar_name: &str,
    admissions: &[ProjectionAdmission],
) -> TokenStream {
    let mut structs: Vec<TokenStream> = Vec::new();
    let grammar_prefix = to_upper_camel(grammar_name);
    for admission in admissions {
        let ProjectionSource::GrammarLayout { layout } = &admission.source else {
            continue;
        };
        let struct_ident = format_ident!(
            "{}{}Projection",
            grammar_prefix,
            to_upper_camel(&admission.rule_name),
        );
        let rule_name_lit = proc_macro2::Literal::string(&admission.rule_name);
        let total_bytes = layout.total_bytes;
        let fields: Vec<TokenStream> = layout
            .fields
            .iter()
            .enumerate()
            .map(|(idx, field)| emit_projection_field(idx, field))
            .collect();
        let field_count = fields.len();
        let field_count_lit = proc_macro2::Literal::usize_unsuffixed(field_count);
        let total_bytes_lit = proc_macro2::Literal::u8_unsuffixed(total_bytes);
        structs.push(quote! {
            /// AY.W6.2 — grammar-derived direct-to-struct projection.
            ///
            /// Emitted storage for a rule whose child sequence
            /// projects onto a fixed-layout scalar tuple per the IR
            /// payload-layout pass. The struct's fields mirror the
            /// layout's scalar fields in declaration order; the
            /// `TOTAL_BYTES` + `FIELD_COUNT` associated consts expose
            /// the admitted shape to downstream consumers (wire-
            /// contract tests, debug readback) without re-inspecting
            /// the IR.
            ///
            /// The struct is emitted regardless of whether the parser
            /// chooses the aggregate-write path at runtime for this
            /// rule; the runtime consumer inspects the layout through
            /// `Tape::payload_bytes` at the admitted byte offsets.
            /// The struct's presence in the expanded emitter output
            /// is the structural proof that the layout pass admitted
            /// this rule (AY.W6.2 hard-gate `cargo expand` evidence).
            #[derive(::core::marker::Copy, ::core::clone::Clone, ::core::fmt::Debug)]
            #[doc(hidden)]
            pub struct #struct_ident {
                #(#fields),*
            }

            impl #struct_ident {
                /// Grammar-declared rule that projects into this
                /// struct. Matches the `rule_name` entry in
                /// `PROJECTION_DIRECT_TO_STRUCT`.
                #[doc(hidden)]
                pub const RULE_NAME: &'static str = #rule_name_lit;

                /// Number of scalar fields the layout pass admitted
                /// for this projection.
                #[doc(hidden)]
                pub const FIELD_COUNT: usize = #field_count_lit;

                /// Total bytes the projection occupies in the
                /// packed payload buffer. Matches
                /// `PayloadLayout::total_bytes` at admission time.
                #[doc(hidden)]
                pub const TOTAL_BYTES: u8 = #total_bytes_lit;
            }
        });
    }
    quote! { #(#structs)* }
}

/// AY.W6.2 — emit one field of a grammar-derived projection struct.
///
/// The field name is `field_<idx>`; the type is the Rust backend
/// mapping of the layout field's scalar [`TypeDesc`]. Span fields
/// project to `(u32, u32)` pairs so the struct stays plain-data
/// (`Copy`); the view layer unpacks back to a `&'input str` at
/// consumption time via the input slice.
fn emit_projection_field(idx: usize, field: &PayloadField) -> TokenStream {
    let field_ident = format_ident!("field_{}", idx);
    let offset_lit = proc_macro2::Literal::u8_unsuffixed(field.offset);
    let ty_tokens = projection_field_type(&field.ty);
    quote! {
        /// Grammar-declared field at layout offset
        #[doc = concat!("`", stringify!(#offset_lit), "` (bytes).")]
        pub #field_ident: #ty_tokens
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

/// AY.W6.2 — emit `__named_type_shim_<name>` markers for the
/// resolver-backed admissions. One shim per distinct admitted name.
fn emit_resolver_shims(admissions: &[ProjectionAdmission]) -> TokenStream {
    let mut names: Vec<String> = admissions
        .iter()
        .filter_map(|a| match &a.source {
            ProjectionSource::ResolverNamed { binding_name } => {
                Some(binding_name.clone())
            }
            ProjectionSource::GrammarLayout { .. } => None,
        })
        .collect();
    names.sort();
    names.dedup();
    let shims: Vec<TokenStream> = names
        .iter()
        .map(|name| {
            let fn_ident = format_ident!(
                "__named_type_shim_{}",
                name.to_ascii_lowercase()
            );
            let name_lit = proc_macro2::Literal::string(name);
            quote! {
                /// AX.W1r.1 marker — structural evidence that
                /// `RustNamedTypes::resolve_named` admitted this
                /// grammar-declared named type. One shim is emitted
                /// per distinct admitted name; the body is
                /// compile-time constant and the function is never
                /// called.
                #[doc(hidden)]
                #[inline(always)]
                pub fn #fn_ident() -> &'static str {
                    #name_lit
                }
            }
        })
        .collect();
    quote! { #(#shims)* }
}

/// AY.W6.2 — emit `__grammar_projection_<rule>` markers for the
/// layout-derived admissions. One marker per admitted rule.
/// Structural evidence that the layout pass produced a payload shape
/// for this rule; the hard gate inspects the shim count as proof
/// that the broader mechanism fired.
fn emit_grammar_projection_shims(
    admissions: &[ProjectionAdmission],
) -> TokenStream {
    let shims: Vec<TokenStream> = admissions
        .iter()
        .filter_map(|a| match &a.source {
            ProjectionSource::GrammarLayout { layout } => {
                Some((a.rule_name.clone(), layout.fields.len()))
            }
            ProjectionSource::ResolverNamed { .. } => None,
        })
        .map(|(rule_name, field_count)| {
            let fn_ident = format_ident!(
                "__grammar_projection_{}",
                sanitise_ident(&rule_name),
            );
            let rule_lit = proc_macro2::Literal::string(&rule_name);
            let count_lit =
                proc_macro2::Literal::usize_unsuffixed(field_count);
            quote! {
                /// AY.W6.2 marker — structural evidence that
                /// `ir.payload_layouts` admitted this rule for direct-
                /// to-struct projection. The returned `(rule_name,
                /// field_count)` pair exposes the layout shape to the
                /// `cargo expand` hard gate without requiring a
                /// runtime compilation.
                #[doc(hidden)]
                #[inline(always)]
                pub fn #fn_ident() -> (&'static str, usize) {
                    (#rule_lit, #count_lit)
                }
            }
        })
        .collect();
    quote! { #(#shims)* }
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

        // AX.W0b — parse() routes through the shape dispatcher
        // uniformly; the walker fallback retired with W0b.A.
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
                let mut builder =
                    ::bbnf::runtime::tape::TapeBuilder::with_capacity(
                        GRAMMAR_PROFILE.capacity_for(input.len()),
                    );
                // AY-II.W0.b — fused pipeline allocation. W0.c publishes
                // the ValueBuilder type at
                // crates/core/src/runtime/value_builder.rs. The parse
                // entry constructs tape + value surface in a single
                // walk; every shape emitter's compound/leaf push writes
                // to both builders in lockstep. Dispatcher invocation
                // threads value_builder alongside builder so per-Ref
                // calls propagate both surfaces.
                let mut value_builder =
                    ::bbnf::runtime::value_builder::ValueBuilder::<Self>::new(
                        GRAMMAR_PROFILE.capacity_for(input.len()),
                    );
                let root_off = {
                    let mut pos: usize = 0;
                    let off = #dispatcher(
                        __input_bytes,
                        &mut pos,
                        &mut state,
                        &mut builder,
                    )
                    .map_err(|e| match e {
                        ::bbnf::runtime::tape::DtaError::Syntax { offset, .. } => {
                            ::bbnf::runtime::ParseErr::Syntax {
                                offset,
                                rule: None,
                            }
                        }
                        ::bbnf::runtime::tape::DtaError::UnexpectedEnd { offset } => {
                            ::bbnf::runtime::ParseErr::Syntax {
                                offset,
                                rule: None,
                            }
                        }
                        ::bbnf::runtime::tape::DtaError::InvalidState { .. } => {
                            ::bbnf::runtime::ParseErr::Syntax {
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
                        return Err(::bbnf::runtime::ParseErr::Syntax {
                            offset: pos as u32,
                            rule: None,
                        });
                    }
                    off
                };
                let tape = builder
                    .finish()
                    .map_err(::bbnf::runtime::ParseErr::Tape)?;
                // AY-II.W0.b — value_builder.finish() projects the
                // constructed <Grammar>Value surface into a slab-backed
                // handle; Parsed::new_fused carries both tape + value
                // so Parsed::to_value() returns the already-constructed
                // value without reparsing. W0.c owns the Parsed signature
                // change; emitter threads the fused handle here.
                let value = value_builder.finish();
                ::core::result::Result::Ok(
                    ::bbnf::runtime::Parsed::new_fused(
                        tape, input, root_off, value,
                    ),
                )
            }
        };

        // AW-V.W3-bench-fix — `parse_with_visitor::<V>` method body.
        // When the grammar has full shape coverage, expose a
        // visitor-generic parse entry that bypasses the tape entirely.
        // The visitor is monomorphised at the call site; per-shape
        // bodies inline into one tight dispatcher, matching the
        // prototype's perf shape.
        //
        // AX.W0b — visitor emission stays gated on W4-absence; grammars
        // carrying W4 rules skip visitor emission (visitor activation
        // for W4 grammars is a future tranche deliverable). The
        // shape-dispatch gate retired alongside the walker per W0b.
        let parse_with_visitor_body = if !super::shapes::has_w4_classified(ir) {
            let visitor_dispatcher = visitor_dispatcher_ident
                .as_ref()
                .expect("use_shape_dispatch gated on root_rule_name");
            let support_mod_ident = quote::format_ident!(
                "__shape_support_{}",
                super::shapes::sanitise_grammar(ident.to_string().as_str()),
            );
            Some(quote! {
                /// AW-V.W3-bench-fix — visitor-generic parse entry.
                ///
                /// Bypasses the tape entirely; `V` is monomorphised at
                /// the call site so the per-shape parse bodies inline
                /// into one tight dispatcher. Matches the shape of
                /// `json_prototype::parse_json::<V>`.
                ///
                /// `V` must implement every per-shape visitor sub-trait
                /// the grammar drives (`ObjectVisitor`, `ArrayVisitor`,
                /// `StringVisitor`, `NumberVisitor`, `KeywordVisitor`)
                /// so each method invocation resolves statically at
                /// monomorphisation time. See `tape::visitor` for
                /// the trait hierarchy and the shipped `TapeVisitor` /
                /// `ValueVisitor` implementations.
                pub fn parse_with_visitor<V>(
                    input: &str,
                    visitor: &mut V,
                ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
                where
                    V: ::bbnf::runtime::tape::ObjectVisitor
                        + ::bbnf::runtime::tape::ArrayVisitor
                        + ::bbnf::runtime::tape::StringVisitor
                        + ::bbnf::runtime::tape::NumberVisitor
                        + ::bbnf::runtime::tape::KeywordVisitor,
                {
                    let bytes = input.as_bytes();
                    let mut pos: usize = 0;
                    // AY.W1-fix — `ScanState::new()` carries only the
                    // whitespace bitmap cache. AY.W1.3's eager
                    // structural-scan call retired (see
                    // AYW1-twitter-regression-diag).
                    let mut state = #support_mod_ident::ScanState::new();
                    #visitor_dispatcher(bytes, &mut pos, &mut state, visitor)?;
                    // Trailing whitespace tolerant.
                    let _ = #support_mod_ident::skip_space(bytes, &mut pos, &mut state);
                    if pos != input.len() {
                        return Err(::bbnf::runtime::ParseErr::Syntax {
                            offset: pos as u32, rule: None,
                        });
                    }
                    Ok(())
                }
            })
        } else {
            None
        };

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
                pub const GRAMMAR_PROFILE: ::bbnf::runtime::tape::GrammarProfile =
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

                /// Parse an input string and return a zero-copy
                /// `Parsed<'_, Self>` that borrows the input directly.
                ///
                /// AX.W0b: `parse()` routes through the shape
                /// dispatcher. The hot path here:
                ///
                /// 1. Allocate a sized `TapeBuilder`.
                /// 2. Call the shape dispatcher, which decomposes into
                ///    per-shape bodies inlined at the call site.
                /// 3. Finalise via `TapeBuilder::finish`.
                pub fn parse(
                    input: &str,
                ) -> ::core::result::Result<
                    ::bbnf::runtime::Parsed<'_, Self>,
                    ::bbnf::runtime::ParseErr,
                > {
                    #parse_body
                }

                #parse_with_visitor_body
            }
        }
    }
}
