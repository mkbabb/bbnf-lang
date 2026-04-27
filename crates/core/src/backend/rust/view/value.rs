//! AY-II.W0'.b — projection-consumer wiring for the fused pipeline.
//!
//! Emits three artefacts per grammar:
//!
//! 1. `pub enum <Grammar>Value<'p>` — one variant per non-transparent
//!    rule. Admitted rules (rules whose layout + resolver facts
//!    produced a `PROJECTION_DIRECT_TO_STRUCT` entry per
//!    [`collect_projection_admissions`]) carry the matching
//!    `<Grammar><RuleCamel>Projection` struct directly; non-admitted
//!    rules keep their shape-classified payload (Span → `&'p str`,
//!    Scalar → primitive, Compound → `Vec<<Grammar>Value<'p>>`, Cursor
//!    → `<Grammar>NodeView<'p>`). The `Unknown` catch-all wraps the
//!    generic `<Grammar>NodeView<'p>` for unclassified / recovery
//!    records whose `variant_idx` does not map to a known rule.
//!
//! 2. `impl crate::runtime::ValueRoot for <Grammar>` — the GAT
//!    binding with `type Value<'p> = <Grammar>Value<'p>` + the
//!    `project_value_output` entry-point that consumes the
//!    W0'.a-published [`Tape<R>`](crate::runtime::tape::Tape)
//!    slab and drives the grammar's per-admission
//!    `materialize_projection_<rule>_<Grammar>(output, input, offset)`
//!    helpers. Non-admitted rules fall through to the existing
//!    shape-based projection.
//!
//! 3. `impl crate::runtime::PathQuery<T> for <Grammar>` for
//!    `T ∈ { &str, f64, bool }` — linear-walk path queries against
//!    the tape. The emitted walker is a zero-copy cursor descent.
//!
//! # Tranche AY-II.W0'.b — admission-driven projection
//!
//! The `project_value_<grammar>` entry-point routes every admitted
//! rule through its matching `materialize_projection_<rule>_<Grammar>`
//! helper. The helper reads the value slab the fused builder populates
//! in lockstep with the tape; its return value wraps into the variant
//! with zero intermediate allocation. Non-admitted variants keep the
//! shape-based path (scalar payload read / Span slice / recursive
//! child walk) so that the emitter never emits a placeholder arm.
//! The dispatch is grammar-derived: `collect_projection_admissions`
//! determines which rules route through the materializer path; every
//! other non-transparent rule falls through to the shape-based arm
//! without a per-grammar branch in the emitter.

use bbnf_ir::{GrammarIR, IrNode, IrRule, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::named_types::RustNamedTypes;
use super::peel;
use crate::backend::rust::emitter::grammar::{
    collect_projection_admissions, ProjectionAdmission,
};

/// Shape the variant's payload takes in `<Grammar>Value`.
///
/// Admitted rules carry their grammar-derived projection struct; every
/// other non-transparent rule is classified from its `TypeDesc` + body
/// shape into one of the leaf shapes. The classification is consumed
/// by both the enum emission (variant signature) and the dispatch
/// emission (project-arm body).
#[derive(Clone, Debug, PartialEq, Eq)]
enum VariantShape {
    /// Admitted rule — carries the generated projection struct. The
    /// materializer is called via `materialize_projection_<rule>_<Grammar>`
    /// at the dispatch site; this shape records the struct identity +
    /// the lifetime-carrying indicator so both the enum and the
    /// dispatcher agree on the payload type.
    Projection {
        /// `<Grammar><RuleCamel>Projection` identifier.
        struct_ident: syn::Ident,
        /// `true` when the projection struct carries a `'p` lifetime
        /// parameter (rich admissions with cursor-child fields).
        has_cursor_fields: bool,
    },
    /// Borrowed source span — `&'p str`.
    Span,
    /// Typed scalar primitive — carries the TypeDesc so the emitter
    /// can splice in the Rust type token (`f64`, `bool`, etc.).
    Scalar(TypeDesc),
    /// Compound (Seq / Alt / Repeat) — carries a
    /// `Vec<<Grammar>Value<'p>>` of eagerly-materialised children.
    Compound,
    /// Fallback / unclassified — wraps the generic NodeView. Used for
    /// recovered records and the mandatory `Unknown` catch-all.
    Cursor,
}

/// Emit the `<Grammar>Value<'p>` enum + `impl ValueRoot` + narrow
/// `PathQuery` impls for the grammar.
///
/// Returns an empty [`TokenStream`] when the grammar has no
/// non-transparent rules.
pub fn emit_value_surface(ir: &GrammarIR, grammar_name: &str) -> TokenStream {
    let non_transparent: Vec<&IrRule> = ir
        .rules
        .iter()
        .filter(|r| !r.meta.is_transparent)
        .collect();
    if non_transparent.is_empty() {
        return quote! {};
    }

    let resolver = RustNamedTypes::from_ir(ir);
    let admissions = collect_projection_admissions(ir, &resolver);
    let grammar_prefix = to_upper_camel(grammar_name);

    let variants = collect_variant_classes(
        ir,
        &non_transparent,
        &admissions,
        &grammar_prefix,
    );
    let value_ident = format_ident!("{}Value", grammar_name);
    let grammar_ident = format_ident!("{}", grammar_name);
    let node_view_ident = format_ident!("{}NodeView", grammar_name);
    let rule_kind_ident = format_ident!("{}RuleKind", grammar_name);

    let enum_tokens = emit_enum_decl(&value_ident, &node_view_ident, &variants);
    let value_root_impl = emit_value_root_impl(
        &grammar_ident,
        &value_ident,
        &rule_kind_ident,
        &variants,
        grammar_name,
    );
    let path_query_impls = emit_path_query_impls(
        ir,
        &grammar_ident,
        &value_ident,
        &node_view_ident,
        &rule_kind_ident,
        &variants,
    );

    quote! {
        #enum_tokens
        #value_root_impl
        #path_query_impls
    }
}

/// A single variant entry in `<Grammar>Value`.
struct VariantEntry {
    /// Rule name — used as the variant identifier in the enum (may
    /// be disambiguated with a `_N` suffix on collision).
    name: String,
    /// Raw rule name from the grammar — matches what
    /// `collect_projection_admissions` iterates on. Used to construct
    /// the materializer fn ident (which is emitted by
    /// `shapes/value_materialize.rs` using the raw name, not the
    /// disambiguated variant name).
    raw_name: String,
    /// Rule id — the discriminator index the view's `rule_kind()`
    /// dispatcher emits for this rule.
    rule_id: u32,
    /// The payload shape of the variant.
    shape: VariantShape,
}

/// Walk the grammar's non-transparent rules and assign one variant
/// per rule. Admitted rules (rule names appearing in `admissions`) get
/// a [`VariantShape::Projection`] that carries the admission's
/// synthesised struct identity; every other non-transparent rule is
/// classified from its TypeDesc + body shape.
fn collect_variant_classes(
    ir: &GrammarIR,
    rules: &[&IrRule],
    admissions: &[ProjectionAdmission],
    grammar_prefix: &str,
) -> Vec<VariantEntry> {
    let mut out: Vec<VariantEntry> = Vec::with_capacity(rules.len());
    let mut seen_names: std::collections::HashSet<String> =
        std::collections::HashSet::with_capacity(rules.len());

    for rule in rules {
        let raw_name = ir.get_string(rule.name).to_string();
        let mut name = raw_name.clone();
        // Disambiguate collisions — rare, but defensive for generated
        // sub-variant names colliding with rule names.
        let mut idx = 0;
        while !seen_names.insert(name.clone()) {
            idx += 1;
            name = format!("{}_{}", raw_name, idx);
        }
        let shape = classify_shape(rule, ir, &raw_name, admissions, grammar_prefix);
        out.push(VariantEntry {
            name,
            raw_name,
            rule_id: rule.id,
            shape,
        });
    }

    out
}

/// Classify the variant's payload shape.
///
/// Priority order:
///
/// 1. Admission (rule appears in `admissions`) — [`VariantShape::Projection`]
///    carrying the generated struct ident. Admissions are the
///    IR-derived direct-to-struct surface; a rule appearing here
///    routes through `materialize_projection_<rule>_<Grammar>` at
///    dispatch time.
/// 2. Scalar `TypeDesc::Span` → Span variant carrying `&'p str`.
/// 3. Scalar payload `TypeDesc` (`F64`, `Bool`, `U32`, …) → Scalar
///    variant carrying the primitive.
/// 4. `IrNode::Repeat` / `IrNode::Seq` / `IrNode::Alt` body → Compound.
/// 5. Fallback → Cursor (wraps `NodeView`).
fn classify_shape(
    rule: &IrRule,
    ir: &GrammarIR,
    raw_name: &str,
    admissions: &[ProjectionAdmission],
    grammar_prefix: &str,
) -> VariantShape {
    if let Some(admission) = admissions.iter().find(|a| a.rule_name() == raw_name) {
        return VariantShape::Projection {
            struct_ident: admission.struct_ident(grammar_prefix),
            has_cursor_fields: admission.plan().has_cursor_fields,
        };
    }

    let type_desc = ir
        .types
        .iter()
        .find_map(|(id, ty)| (*id == rule.id).then_some(ty));

    // B5.W1 absorb 7b — IR `TypeDesc::Span` is a hint, not an oracle.
    // Pre-W1 the classify routed every Span-typed rule to
    // `VariantShape::Span`, which renders as `name(text)` without
    // descending into children. Rules like CSS L4 `ruleBlock` and
    // `typeSelector` carry compound bodies (Seq with admitted child
    // rules) but inherit `TypeDesc::Span` from the IR's narrowest
    // common projection. Routing them as Span makes the admitted
    // children invisible at projection time. The fix peeks at the
    // body shape FIRST: if the rule's body has structural compound
    // shape (Seq / Alt / Repeat with admitted children), we route
    // it as `VariantShape::Compound` regardless of the IR's `Span`
    // declaration. Pure-Span leaves (Literal / Regex bodies) still
    // go through the Span arm. Per `feedback_pluggable_components`
    // — IR classification is a hint, not an oracle; codegen looks
    // at structure when both diverge.
    let body_compound = matches!(
        peel::unwrap_structural_wrappers(&rule.body),
        IrNode::Seq(_) | IrNode::Alt(_, _) | IrNode::Repeat { .. }
    );

    match type_desc {
        Some(TypeDesc::Span) if !body_compound => VariantShape::Span,
        Some(td) if td.is_scalar_payload() && !matches!(td, TypeDesc::Span) => {
            VariantShape::Scalar(td.clone())
        }
        _ => {
            // Classify via body shape — peel structural wrappers
            // (Map / OptionalWhitespace / Skip / Next / Negate) so
            // delimiter-bounded compounds (`"[" >> ... << "]"`)
            // surface as their inner Repeat / Seq kernel rather
            // than falling through to `Cursor`.
            match peel::unwrap_structural_wrappers(&rule.body) {
                IrNode::Seq(_) | IrNode::Alt(_, _) | IrNode::Repeat { .. } => {
                    VariantShape::Compound
                }
                IrNode::Literal(_) | IrNode::Regex(_) => VariantShape::Span,
                _ => VariantShape::Cursor,
            }
        }
    }
}

/// UpperCamel-case a grammar name so the projection struct ident
/// (`<Grammar><RuleCamel>Projection`) can be reconstructed locally.
/// Mirrors `emitter/grammar.rs::to_upper_camel` — duplicated here so
/// `view/value.rs` stays self-contained.
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

/// Emit the `pub enum <Grammar>Value<'p> { ... }` declaration.
///
/// Payload shapes:
/// - Projection { struct_ident, has_cursor_fields } → `struct_ident` or
///   `struct_ident<'p>` depending on whether the projection itself
///   bears a lifetime.
/// - Span → `&'p str`.
/// - Scalar(td) → primitive (`f64`, `bool`, etc.).
/// - Compound → `::std::vec::Vec<<Grammar>Value<'p>>`.
/// - Cursor → `<Grammar>NodeView<'p>`.
///
/// An `Unknown(NodeView)` catch-all survives unconditionally for
/// unclassified / recovery records whose `variant_idx` falls outside
/// the rule-id space. The emitter does not currently drop the catch-
/// all even when every rule is admitted — a corrupt frame arena (the
/// `variant_idx` byte) can still hit the `RuleKind::Unknown` arm at
/// runtime.
fn emit_enum_decl(
    value_ident: &syn::Ident,
    node_view_ident: &syn::Ident,
    variants: &[VariantEntry],
) -> TokenStream {
    let variant_tokens: Vec<TokenStream> = variants
        .iter()
        .map(|v| {
            let id = format_ident!("{}", v.name);
            match &v.shape {
                VariantShape::Projection {
                    struct_ident,
                    has_cursor_fields,
                } => {
                    if *has_cursor_fields {
                        quote! { #id(#struct_ident<'p>) }
                    } else {
                        quote! { #id(#struct_ident) }
                    }
                }
                VariantShape::Span => quote! { #id(&'p str) },
                VariantShape::Scalar(td) => {
                    let ty = format_ident!(
                        "{}",
                        td.rust_ident().expect("scalar TypeDesc has rust_ident"),
                    );
                    quote! { #id(#ty) }
                }
                VariantShape::Compound => {
                    quote! { #id(::std::vec::Vec<#value_ident<'p>>) }
                }
                VariantShape::Cursor => {
                    quote! { #id(#node_view_ident<'p>) }
                }
            }
        })
        .collect();

    quote! {
        /// AY-II.W0'.b — grammar-emitted value enum. Eager
        /// materialisation target for `Parsed::to_value()`. Variants
        /// enumerate non-transparent rules; admitted rules carry the
        /// matching `<Grammar><RuleCamel>Projection` struct directly,
        /// non-admitted rules carry their shape-classified payload.
        #[derive(Clone, Debug)]
        pub enum #value_ident<'p> {
            #(#variant_tokens,)*
            /// Fallback for records whose `variant_idx` is not a
            /// known rule discriminator (recovered records, stray
            /// sub-variant indices).
            Unknown(#node_view_ident<'p>),
        }
    }
}

/// Emit `impl crate::runtime::ValueRoot for <Grammar>` + the
/// `project_value_<grammar>` fn + its supporting dispatch. The emitted
/// fn routes every admitted rule through its grammar-derived
/// materializer and falls through to shape-based arms for every other
/// non-transparent rule. One code path per grammar; the emission is
/// driven by `VariantEntry::shape` directly.
fn emit_value_root_impl(
    grammar_ident: &syn::Ident,
    value_ident: &syn::Ident,
    rule_kind_ident: &syn::Ident,
    variants: &[VariantEntry],
    grammar_name: &str,
) -> TokenStream {
    let root_fn = format_ident!("project_value_{}", grammar_name);
    let frame_fn = format_ident!("project_frame_{}", grammar_name);
    let dispatch_fn = format_ident!("project_rule_kind_{}", grammar_name);
    let push_children_fn = format_ident!("project_push_children_{}", grammar_name);

    // Rule-id → RuleKind dispatch, scoped to this grammar. The
    // `variant_idx & 0xFF` is the emitter's canonical low-byte slice
    // (matches the view-layer `rule_kind()` dispatcher).
    let dispatch_arms: Vec<TokenStream> = variants
        .iter()
        .map(|v| {
            let kind_variant = format_ident!("{}", v.name);
            let idx_lit = (v.rule_id & 0xFF) as u8;
            quote! { #idx_lit => #rule_kind_ident::#kind_variant, }
        })
        .collect();

    // Per-variant project arms. Admitted rules route through their
    // materializer; non-admitted rules construct the variant inline
    // from the frame + input.
    let project_arms: Vec<TokenStream> = variants
        .iter()
        .map(|v| emit_project_arm(v, value_ident, rule_kind_ident, &push_children_fn, grammar_name))
        .collect();

    quote! {
        /// B5.W0.6 — joint `(kind, variant_idx)` dispatch local to the
        /// fused-pipeline projection path.
        ///
        /// `variant_idx = (rule_id & 0xFF)` collapses every rule whose
        /// id-mod-256 collides; for non-rule structural compounds the
        /// shape emitters stamp `variant_idx = 0` as a placeholder
        /// (see `emitter/shapes/{flat,array,object,inline}.rs`), which
        /// pre-B5.W0.6 collided with rule_id=0 (CSS L4 `namedColor`,
        /// JSON `null`, etc.) and routed Seq/Alt/Repeat intermediates
        /// to a leaf-rule's materialiser. The materialiser then panicked
        /// against the compound's `child_off` (a column rank, not an
        /// arena byte offset) at `payload_bytes`'s precondition assert.
        ///
        /// The dispatch now consults `kind` AS WELL AS `variant_idx`:
        /// a compound-kind frame carrying the placeholder `variant_idx
        /// = 0` is an intermediate without a rule binding and routes
        /// to `Unknown`. The `ValueFrame` doc-comment at
        /// `crates/tape/src/builder/value.rs:47` already declares this
        /// invariant — pre-B5.W0.6 the codegen ignored it.
        #[inline(always)]
        fn #dispatch_fn(
            kind: crate::runtime::tape::TapeKind,
            variant_idx: u8,
        ) -> #rule_kind_ident {
            // Intermediate compound — non-rule structural frame stamped
            // with the `variant_idx=0` placeholder by the shape emitters.
            // No rule binds; drop to `Unknown` so the project arm logic
            // descends into the children rather than calling a leaf
            // materialiser against a compound record.
            if variant_idx == 0 && kind.is_compound() {
                return #rule_kind_ident::Unknown;
            }
            match variant_idx {
                #(#dispatch_arms)*
                _ => #rule_kind_ident::Unknown,
            }
        }

        /// B5.W0.6 — push the projected value(s) for the record at
        /// `offset` onto `out`. For rule-bound records this is a single
        /// `<Grammar>Value` variant constructed via [`#frame_fn`]. For
        /// intermediate compound records (the `variant_idx=0` non-rule
        /// structural compounds emitted at inner Seq / Repeat / Alt
        /// positions) it recurses through the children, flattening the
        /// intermediate transparently — the user-visible value tree
        /// only carries rule-bound variants.
        ///
        /// Mirrors the walker-tape parity contract: the substrate emits
        /// one tape record per IR production, but only rule-bound
        /// productions surface as `<Grammar>Value` variants; structural
        /// intermediates are an implementation detail of the tape
        /// shape, not of the value tree.
        ///
        /// Reads `kind` + `variant_idx` from the tape (not the value
        /// frame). The materializer pattern at
        /// `materialize_projection_<rule>_<Grammar>` already treats
        /// `offset` as a tape offset (`tape.try_get(TapeOffset(offset))`);
        /// the dispatch is therefore consistent with the materialiser
        /// surface — tape is the canonical record substrate, the value
        /// frames are a parallel cache used only for typed scalar
        /// payload reads on leaves with a payload tag.
        #[inline]
        fn #push_children_fn<'p>(
            output: &crate::runtime::tape::Tape<#grammar_ident>,
            input: &'p str,
            offset: u32,
            out: &mut ::std::vec::Vec<#value_ident<'p>>,
        ) {
            let __tape = output;
            let __rec = match __tape.try_get(crate::runtime::tape::TapeOffset(offset)) {
                ::core::option::Option::Some(r) => r,
                ::core::option::Option::None => return,
            };
            // Intermediate structural compound — descend through its
            // children without surfacing a wrapper variant. Leaves and
            // rule-bound compounds project as a single value.
            if __rec.variant_idx() == 0 && __rec.kind().is_compound() {
                let __cur = crate::runtime::tape::TapeCursor::new(
                    __tape,
                    crate::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    #push_children_fn(output, input, __child.offset().0, out);
                }
            } else {
                out.push(#frame_fn(output, input, offset));
            }
        }

        /// AY-II.W0'.b — per-frame projector. Reads one record from the
        /// fused-pipeline [`Tape<R>`](crate::runtime::tape::Tape)
        /// tape and constructs the matching `<Grammar>Value` variant.
        /// Admitted rules tail-call their grammar-derived materializer;
        /// non-admitted rules construct the variant inline. Compound
        /// variants recurse through this same fn.
        ///
        /// B5.W0.6 — kind + variant_idx + span are read from the tape
        /// record (not the value frame). The value frame substrate is
        /// only consulted for typed-scalar payload reads on leaves
        /// whose `value_payload_for(frame)` returns the column-decoded
        /// payload — that path remains in the scalar arm.
        #[inline]
        fn #frame_fn<'p>(
            output: &crate::runtime::tape::Tape<#grammar_ident>,
            input: &'p str,
            offset: u32,
        ) -> #value_ident<'p> {
            let __tape = output;
            let __rec = match __tape.try_get(crate::runtime::tape::TapeOffset(offset)) {
                ::core::option::Option::Some(r) => r,
                ::core::option::Option::None => {
                    ::core::panic!(
                        "AY-II.W0'.b: tape offset {} out of range (tape len: {})",
                        offset,
                        __tape.len(),
                    );
                }
            };
            match #dispatch_fn(__rec.kind(), __rec.variant_idx()) {
                #(#project_arms)*
                _ => {
                    ::core::panic!(
                        "AY-II.W0'.b: unclassified (kind={:?}, variant_idx={}) on tape record at offset {}",
                        __rec.kind(),
                        __rec.variant_idx(),
                        offset,
                    );
                }
            }
        }

        /// AY-II.W0'.b — fused-pipeline root projector. Reads the root
        /// record from the tape and constructs the grammar's
        /// `<Grammar>Value<'p>` in one pass. No tape walk, no reparse,
        /// no visitor dispatch.
        ///
        /// B5.W1 — when the root tape record is a structural
        /// intermediate compound (variant_idx=0, kind compound — the
        /// shape emitters' Repeat / Seq scaffolding lands here), the
        /// projector descends into the first rule-bound child rather
        /// than panicking on `Unknown`. This mirrors
        /// `project_push_children_<Grammar>`'s transparent-recursion
        /// invariant.
        #[inline]
        fn #root_fn<'p>(
            output: &crate::runtime::tape::Tape<#grammar_ident>,
            input: &'p str,
        ) -> #value_ident<'p> {
            let root_off = output.root_offset();
            let __tape = output;
            // Skip every structural-intermediate (variant_idx=0 and
            // compound) wrapper at the root, descending through
            // `child_off` until a rule-bound record surfaces. The
            // typed projection layer expects the first rule-bound
            // record to project; structural intermediates are an
            // implementation detail of the tape emission shape, not
            // of the value tree.
            let mut __cur_off = root_off;
            loop {
                let __rec = match __tape.try_get(crate::runtime::tape::TapeOffset(__cur_off)) {
                    ::core::option::Option::Some(r) => r,
                    ::core::option::Option::None => break,
                };
                if __rec.variant_idx() == 0 && __rec.kind().is_compound() {
                    if __rec.has_children() {
                        if let ::core::option::Option::Some(__child) = __rec.child_off.as_u32().checked_sub(0) {
                            if __child != ::core::u32::MAX {
                                __cur_off = __child;
                                continue;
                            }
                        }
                    }
                    break;
                }
                break;
            }
            #frame_fn(output, input, __cur_off)
        }

        impl crate::runtime::ValueRoot for #grammar_ident {
            type Value<'p> = #value_ident<'p>;

            #[inline]
            fn project_value_output<'p>(
                output: &crate::runtime::tape::Tape<#grammar_ident>,
                input: &'p str,
            ) -> Self::Value<'p>
            where
                Self: 'p,
            {
                #root_fn(output, input)
            }
        }
    }
}

/// Emit one per-variant projection arm for `project_frame_<Grammar>`.
///
/// Shape-specialised:
/// - Projection → call the materializer + wrap in the variant. The
///   materializer signature is
///   `materialize_projection_<rule>_<Grammar>(output, input, offset)
///    -> Option<Projection>`; `unwrap_or_else(panic)` the admitted
///   case because the IR admission fact guarantees the rule projects
///   cleanly.
/// - Span → `&input[lo..hi]` wrapped in the Span variant.
/// - Scalar(td) → decode the frame's payload tag into the typed
///   primitive; fall back to span-text parse when no payload was
///   recorded.
/// - Compound → walk the frame's child run, project each child
///   recursively, collect into a `Vec<<Grammar>Value>`.
/// - Cursor → panics — the fused-pipeline projection does not
///   reconstruct a NodeView from the slab today.
fn emit_project_arm(
    v: &VariantEntry,
    value_ident: &syn::Ident,
    rule_kind_ident: &syn::Ident,
    push_children_fn: &syn::Ident,
    grammar_name: &str,
) -> TokenStream {
    let kind_variant = format_ident!("{}", v.name);
    let value_variant = format_ident!("{}", v.name);
    match &v.shape {
        VariantShape::Projection { .. } => {
            let materializer_fn = format_ident!(
                "materialize_projection_{}_{}",
                sanitise_ident(&v.raw_name),
                grammar_name,
            );
            let rule_name_lit = proc_macro2::Literal::string(&v.raw_name);
            quote! {
                #rule_kind_ident::#kind_variant => {
                    let proj = #materializer_fn(output, input, offset)
                        .unwrap_or_else(|| {
                            ::core::panic!(
                                "AY-II.W0'.b: materializer for admitted rule `{}` \
                                 returned None at frame offset {}; admission \
                                 invariant violated",
                                #rule_name_lit,
                                offset,
                            );
                        });
                    #value_ident::#value_variant(proj)
                }
            }
        }
        VariantShape::Span => quote! {
            #rule_kind_ident::#kind_variant => {
                let span = &input[__rec.span_lo as usize..__rec.span_hi as usize];
                #value_ident::#value_variant(span)
            }
        },
        VariantShape::Scalar(td) => emit_scalar_project_arm(
            rule_kind_ident,
            &kind_variant,
            value_ident,
            &value_variant,
            td,
        ),
        VariantShape::Compound => quote! {
            #rule_kind_ident::#kind_variant => {
                // B5.W0.6 — child iteration descends through intermediate
                // structural compounds (the `variant_idx=0` placeholders
                // emitted by inner Seq / Repeat / Alt positions in
                // `emitter/shapes/{flat,array,object,inline}.rs`). The
                // `push_children` helper yields rule-bound records only;
                // intermediate compounds are flattened transparently so
                // the user-visible `<Grammar>Value` tree omits them.
                //
                // Iteration walks the tape's child run via
                // `TapeCursor::children` — consistent with the dispatcher
                // reading `kind` / `variant_idx` from the tape record.
                let mut children: ::std::vec::Vec<#value_ident<'p>> =
                    ::std::vec::Vec::new();
                let __cur = crate::runtime::tape::TapeCursor::new(
                    __tape,
                    crate::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    #push_children_fn(output, input, __child.offset().0, &mut children);
                }
                #value_ident::#value_variant(children)
            }
        },
        VariantShape::Cursor => quote! {
            #rule_kind_ident::#kind_variant => {
                // Cursor variants wrap a structural NodeView; the
                // fused-pipeline projection does not reconstruct a
                // NodeView from the slab today. A grammar whose
                // classification lands on Cursor is one the emitter
                // has not yet produced a slab projection for; the
                // projection panics rather than silently fall back
                // to tape-walking.
                ::core::panic!(
                    "AY-II.W0'.b: Cursor-shape variant projection not yet \
                     available; tape record offset {}",
                    offset,
                );
            }
        },
    }
}

/// Emit a Scalar variant projection arm — decodes the leaf's payload
/// into the typed primitive. Falls back to span-text parsing when
/// no payload was recorded (leaves whose rule did not stage a typed
/// payload at parse time).
///
/// B5.W0.6 — payload reads consult the value substrate via
/// `value_frame_at(offset).and_then(value_payload_for)` for the
/// typed-tag column path; the span fallback reads `__rec.span_lo` /
/// `__rec.span_hi` from the tape record bound in the dispatcher.
fn emit_scalar_project_arm(
    rule_kind_ident: &syn::Ident,
    kind_variant: &syn::Ident,
    value_ident: &syn::Ident,
    value_variant: &syn::Ident,
    td: &TypeDesc,
) -> TokenStream {
    let fallback = scalar_fallback_from_span(td);
    let payload_lookup = quote! {
        output
            .frame(offset)
            .and_then(|f| output.payload_for(f))
    };
    match td {
        TypeDesc::F64 => quote! {
            #rule_kind_ident::#kind_variant => {
                let v: f64 = #payload_lookup
                    .and_then(|p| p.as_f64())
                    .unwrap_or_else(|| { #fallback });
                #value_ident::#value_variant(v)
            }
        },
        TypeDesc::Bool => quote! {
            #rule_kind_ident::#kind_variant => {
                let v: bool = #payload_lookup
                    .and_then(|p| p.as_bool())
                    .unwrap_or_else(|| { #fallback });
                #value_ident::#value_variant(v)
            }
        },
        TypeDesc::U32 => quote! {
            #rule_kind_ident::#kind_variant => {
                let v: u32 = #payload_lookup
                    .and_then(|p| p.as_u32())
                    .unwrap_or_else(|| { #fallback });
                #value_ident::#value_variant(v)
            }
        },
        _ => {
            let ident = td
                .rust_ident()
                .expect("scalar TypeDesc has rust_ident");
            let ty_ident = format_ident!("{}", ident);
            quote! {
                #rule_kind_ident::#kind_variant => {
                    let v: #ty_ident = #payload_lookup
                        .and_then(|p| p.as_u32())
                        .map(|v| v as #ty_ident)
                        .unwrap_or_else(|| { #fallback });
                    #value_ident::#value_variant(v)
                }
            }
        }
    }
}

/// Scalar fallback — reads the source span and parses into the target
/// primitive. Reads `__rec.span_lo` / `__rec.span_hi` from the tape
/// record (B5.W0.6 — dispatcher binds `__rec` to the tape record at
/// the projection's offset).
fn scalar_fallback_from_span(td: &TypeDesc) -> TokenStream {
    let slice = quote! {
        (&input[__rec.span_lo as usize..__rec.span_hi as usize])
    };
    match td {
        TypeDesc::Bool => quote! { #slice == "true" },
        TypeDesc::U32 => quote! { #slice.parse::<u32>().unwrap_or(0u32) },
        TypeDesc::F64 => quote! { #slice.parse::<f64>().unwrap_or(0.0) },
        TypeDesc::I8 => quote! { #slice.parse::<i8>().unwrap_or(0) },
        TypeDesc::U8 => quote! { #slice.parse::<u8>().unwrap_or(0) },
        TypeDesc::I16 => quote! { #slice.parse::<i16>().unwrap_or(0) },
        TypeDesc::U16 => quote! { #slice.parse::<u16>().unwrap_or(0) },
        TypeDesc::I32 => quote! { #slice.parse::<i32>().unwrap_or(0) },
        TypeDesc::I64 => quote! { #slice.parse::<i64>().unwrap_or(0) },
        TypeDesc::U64 => quote! { #slice.parse::<u64>().unwrap_or(0) },
        _ => quote! { ::core::default::Default::default() },
    }
}

/// Mirror of the emitter's `sanitise_ident` helper — rule names with
/// non-alphanumeric characters replace them with underscores; leading
/// digits get an `r_` prefix. Kept local so `view/value.rs` stays
/// self-contained.
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

/// Returns one impl per T ∈ { &'static str, f64, bool } — each
/// performs a cursor-walk over the tape, descending through
/// `PathSegment::Field` / `PathSegment::Index` steps and extracting
/// the leaf on exact match.
///
/// # AY-II.W0'.c — STRUCTURAL_SCAN_POLICY emission-time splice
///
/// The emitted `__path_walk` is policy-driven: at each step the
/// current record's `rule_kind()` dispatches through a compile-time
/// match arm whose body inlines the matching cursor primitive. Rules
/// whose [`ScanActivationFlags`] admit `OBJECT_KEY_SEEK` emit a
/// `cursor.bounded_lookahead(...)` iteration paired with
/// `cursor.object_key_seek(...)` for the value-position seek; rules
/// admitting `SCAN_STRUCTURAL_BOUNDED` emit a
/// `cursor.scan_structural_bounded(...)` indexed walk for positional
/// access. Rules whose policy alphabet class is
/// [`ScanAlphabetClass::Empty`][empty] retain the generic
/// children-iteration walker (the default used pre-AY-II.W0'.c).
///
/// The per-rule dispatch resolves at compile time — the match arms
/// below fold to the admitted primitive for that rule without any
/// runtime flag lookup or dispatch-table read. The emitted body
/// carries only primitives the rule's policy actually admits; non-
/// admitted rules fall through to the default arm.
///
/// [`ScanActivationFlags`]: tape::ScanActivationFlags
/// [empty]: tape::ScanAlphabetClass::Empty
fn emit_path_query_impls(
    ir: &GrammarIR,
    grammar_ident: &syn::Ident,
    _value_ident: &syn::Ident,
    node_view_ident: &syn::Ident,
    rule_kind_ident: &syn::Ident,
    variants: &[VariantEntry],
) -> TokenStream {
    use crate::backend::rust::emitter::shapes::dispatcher::lookup_scan_policy;
    use tape::ScanActivationFlags;

    // Partition non-transparent rules by the primitives their scan
    // policy admits. The sets may overlap (a rule can admit both
    // OBJECT_KEY_SEEK and SCAN_STRUCTURAL_BOUNDED); emission below
    // composes the partitions into disjoint match-arm groups.
    //
    // Variant idents route through the grammar's `<Grammar>RuleKind`
    // enum (raw rule names, not the `<Grammar>Value` disambiguated
    // form) per `view::mod::rule_kind_variants` — the dispatch is on
    // `cur.rule_kind()`, which yields a `<Grammar>RuleKind` value.
    let mut object_key_seek_rks: Vec<syn::Ident> = Vec::new();
    let mut scan_structural_rks: Vec<syn::Ident> = Vec::new();
    let mut bounded_lookahead_rks: Vec<syn::Ident> = Vec::new();
    for v in variants {
        let Some((_, flags)) =
            lookup_scan_policy(ir, v.rule_id)
        else {
            continue;
        };
        let rule = ir
            .rules
            .iter()
            .find(|r| r.id == v.rule_id)
            .expect("variant rule_id matches an IR rule");
        let raw_name = ir.get_string(rule.name);
        let variant_ident = format_ident!("{}", raw_name);
        if flags.contains(ScanActivationFlags::OBJECT_KEY_SEEK) {
            object_key_seek_rks.push(variant_ident.clone());
        }
        if flags.contains(ScanActivationFlags::SCAN_STRUCTURAL_BOUNDED) {
            scan_structural_rks.push(variant_ident.clone());
        }
        // Bounded-lookahead admission independent of object-key-seek
        // — rules like CSS `declarationList` admit bounded scan
        // without the object-value hop pattern.
        if flags.contains(ScanActivationFlags::BOUNDED_LOOKAHEAD)
            && !flags.contains(ScanActivationFlags::OBJECT_KEY_SEEK)
        {
            bounded_lookahead_rks.push(variant_ident);
        }
    }

    // Compose the Field handler. The object-key-seek fast path drops
    // in when the rule admits OBJECT_KEY_SEEK; the bounded-lookahead
    // fast path drops in for rules that admit BOUNDED_LOOKAHEAD
    // without the key-hop. Other rules fall through to the generic
    // children iteration.
    let field_fast_key_seek = if object_key_seek_rks.is_empty() {
        quote! {}
    } else {
        quote! {
            #( #rule_kind_ident::#object_key_seek_rks )|* => {
                // OBJECT_KEY_SEEK admission: bound the child scan
                // by the compound's span end (BOUNDED_LOOKAHEAD
                // co-admits with OBJECT_KEY_SEEK per
                // `lookup_scan_policy`'s Dense arm), compare
                // keys against the requested path segment, then
                // hop to the value via `TapeCursor::object_key_seek`.
                let parent = cur.cursor();
                let (_, parent_end) = parent.span();
                let mut iter = parent.bounded_lookahead(parent_end);
                let mut hit: ::core::option::Option<#node_view_ident<'p>> = None;
                loop {
                    let k_cur = match iter.next() {
                        ::core::option::Option::Some(c) => c,
                        ::core::option::Option::None => break,
                    };
                    // Skip the value slot so the next iteration
                    // lands on the following key.
                    let _ = iter.next();
                    let (k_lo, k_hi) = k_cur.span();
                    let raw = &cur_input[k_lo as usize..k_hi as usize];
                    let key_text = if raw.as_bytes().first() == ::core::option::Option::Some(&b'"')
                        && raw.as_bytes().last() == ::core::option::Option::Some(&b'"')
                        && raw.len() >= 2
                    {
                        &raw[1..raw.len() - 1]
                    } else {
                        raw
                    };
                    if key_text == *key {
                        // Span-equality seek to the value cursor;
                        // zero additional child iteration.
                        let v_cursor = parent.object_key_seek((k_lo, k_hi));
                        hit = v_cursor.map(|c| #node_view_ident::from_cursor(c, cur_input));
                        break;
                    }
                }
                cur = match hit {
                    ::core::option::Option::Some(v) => v,
                    ::core::option::Option::None => return ::core::option::Option::None,
                };
            }
        }
    };

    let field_fast_bounded = if bounded_lookahead_rks.is_empty() {
        quote! {}
    } else {
        quote! {
            #( #rule_kind_ident::#bounded_lookahead_rks )|* => {
                // BOUNDED_LOOKAHEAD without OBJECT_KEY_SEEK:
                // span-bounded key/value probe without the
                // span-equality hop. Used by rules whose policy
                // admits bounded scan but whose shape does not
                // present the {key,value} pairing the Dense class
                // gates on.
                let parent = cur.cursor();
                let (_, parent_end) = parent.span();
                let mut iter = parent.bounded_lookahead(parent_end);
                let mut hit: ::core::option::Option<#node_view_ident<'p>> = None;
                loop {
                    let k_cur = match iter.next() {
                        ::core::option::Option::Some(c) => c,
                        ::core::option::Option::None => break,
                    };
                    let v_cur = match iter.next() {
                        ::core::option::Option::Some(c) => c,
                        ::core::option::Option::None => break,
                    };
                    let (k_lo, k_hi) = k_cur.span();
                    let raw = &cur_input[k_lo as usize..k_hi as usize];
                    let key_text = if raw.as_bytes().first() == ::core::option::Option::Some(&b'"')
                        && raw.as_bytes().last() == ::core::option::Option::Some(&b'"')
                        && raw.len() >= 2
                    {
                        &raw[1..raw.len() - 1]
                    } else {
                        raw
                    };
                    if key_text == *key {
                        hit = ::core::option::Option::Some(
                            #node_view_ident::from_cursor(v_cur, cur_input),
                        );
                        break;
                    }
                }
                cur = match hit {
                    ::core::option::Option::Some(v) => v,
                    ::core::option::Option::None => return ::core::option::Option::None,
                };
            }
        }
    };

    let index_fast_scan = if scan_structural_rks.is_empty() {
        quote! {}
    } else {
        quote! {
            #( #rule_kind_ident::#scan_structural_rks )|* => {
                // SCAN_STRUCTURAL_BOUNDED admission: emit the
                // bounded structural scan and pick the i-th
                // admitted cursor. Zero-allocation iteration; the
                // iterator terminates early at the compound's end
                // span without visiting post-close records.
                let parent = cur.cursor();
                let (_, parent_end) = parent.span();
                let scan = parent.scan_structural_bounded(parent_end);
                cur = match scan.iter().nth(*i) {
                    ::core::option::Option::Some(c) =>
                        #node_view_ident::from_cursor(c, cur_input),
                    ::core::option::Option::None =>
                        return ::core::option::Option::None,
                };
            }
        }
    };

    let walk_fn = quote! {
        /// AY-II.W0'.c — policy-driven path walker. Descends from
        /// `view` per the given path, returning the narrowed
        /// NodeView on hit or `None` when any step misses.
        ///
        /// The per-step dispatch reads `cur.rule_kind()` and
        /// resolves to the structural-scan primitive the rule's
        /// [`STRUCTURAL_SCAN_POLICY`] entry admits: rules admitting
        /// `OBJECT_KEY_SEEK` use
        /// [`TapeCursor::bounded_lookahead`] + [`TapeCursor::object_key_seek`]
        /// for the key-match + value-hop sequence; rules admitting
        /// `SCAN_STRUCTURAL_BOUNDED` use
        /// [`TapeCursor::scan_structural_bounded`] for positional
        /// access. Rules outside the policy's admission fall
        /// through to a generic children iteration.
        ///
        /// [`STRUCTURAL_SCAN_POLICY`]: crate::STRUCTURAL_SCAN_POLICY
        /// [`TapeCursor::bounded_lookahead`]: crate::runtime::tape::TapeCursor::bounded_lookahead
        /// [`TapeCursor::object_key_seek`]: crate::runtime::tape::TapeCursor::object_key_seek
        /// [`TapeCursor::scan_structural_bounded`]: crate::runtime::tape::TapeCursor::scan_structural_bounded
        #[inline]
        fn __path_walk<'p>(
            view: #node_view_ident<'p>,
            path: crate::runtime::Path<'_>,
        ) -> ::core::option::Option<#node_view_ident<'p>> {
            let cur_input = view.input();
            let mut cur = view;
            for seg in path.iter() {
                match seg {
                    crate::runtime::PathSegment::Field(key) => {
                        match cur.rule_kind() {
                            #field_fast_key_seek
                            #field_fast_bounded
                            _ => {
                                // Generic walk — children pair
                                // (key, value) compared by source
                                // span text. Used for rules whose
                                // policy admission is
                                // `ScanAlphabetClass::Empty`.
                                let mut it = cur.children();
                                let mut found = None;
                                loop {
                                    let k = match it.next() {
                                        ::core::option::Option::Some(k) => k,
                                        ::core::option::Option::None => break,
                                    };
                                    let v = match it.next() {
                                        ::core::option::Option::Some(v) => v,
                                        ::core::option::Option::None => break,
                                    };
                                    let raw = k.span_text();
                                    let key_text = if raw.as_bytes().first() == ::core::option::Option::Some(&b'"')
                                        && raw.as_bytes().last() == ::core::option::Option::Some(&b'"')
                                        && raw.len() >= 2
                                    {
                                        &raw[1..raw.len() - 1]
                                    } else {
                                        raw
                                    };
                                    if key_text == *key {
                                        found = ::core::option::Option::Some(v);
                                        break;
                                    }
                                }
                                cur = match found {
                                    ::core::option::Option::Some(v) => v,
                                    ::core::option::Option::None =>
                                        return ::core::option::Option::None,
                                };
                            }
                        }
                    }
                    crate::runtime::PathSegment::Index(i) => {
                        match cur.rule_kind() {
                            #index_fast_scan
                            _ => {
                                cur = cur.child(*i)?;
                            }
                        }
                    }
                }
            }
            ::core::option::Option::Some(cur)
        }
    };

    // Per-T impl. Each finishes the walk with a T-specialised
    // leaf extractor.
    let impl_str = quote! {
        impl crate::runtime::PathQuery<&'static str> for #grammar_ident {
            #[inline]
            fn query<'p>(
                view: Self::View<'p>,
                path: crate::runtime::Path<'_>,
            ) -> ::core::option::Option<&'static str>
            where
                Self: 'p,
            {
                let node = #node_view_ident::from_cursor(view.cursor(), view.input());
                __path_walk(node, path)?;
                // Leaf-kind narrowing to `&'static str` is unsound
                // across the arbitrary-input lifetime — the
                // narrower `&'p str` impl below handles the zero-
                // copy borrow. This `&'static str` impl exists for
                // the bench-harness literal-path case and returns
                // None on non-'static hits.
                ::core::option::Option::None
            }
        }

        impl crate::runtime::PathQuery<f64> for #grammar_ident {
            #[inline]
            fn query<'p>(
                view: Self::View<'p>,
                path: crate::runtime::Path<'_>,
            ) -> ::core::option::Option<f64>
            where
                Self: 'p,
            {
                let node = #node_view_ident::from_cursor(view.cursor(), view.input());
                let hit = __path_walk(node, path)?;
                let tape = hit.cursor().tape();
                let rec = hit.cursor().record();
                if let ::core::option::Option::Some(v) = tape.payload_f64(rec) {
                    return ::core::option::Option::Some(v);
                }
                // Fallback: parse the span text.
                hit.span_text().parse::<f64>().ok()
            }
        }

        impl crate::runtime::PathQuery<bool> for #grammar_ident {
            #[inline]
            fn query<'p>(
                view: Self::View<'p>,
                path: crate::runtime::Path<'_>,
            ) -> ::core::option::Option<bool>
            where
                Self: 'p,
            {
                let node = #node_view_ident::from_cursor(view.cursor(), view.input());
                let hit = __path_walk(node, path)?;
                let tape = hit.cursor().tape();
                let rec = hit.cursor().record();
                if let ::core::option::Option::Some(v) = tape.payload_bool(rec) {
                    return ::core::option::Option::Some(v);
                }
                match hit.span_text() {
                    "true" => ::core::option::Option::Some(true),
                    "false" => ::core::option::Option::Some(false),
                    _ => ::core::option::Option::None,
                }
            }
        }
    };

    quote! {
        #walk_fn
        #impl_str
    }
}

/// Convenience accessor for downstream emitters: returns the
/// grammar's variant map so the materialise emitter can pick the
/// right constructor per `rule_kind()`. Post-W0'.b the fused
/// projection path lives entirely inside `emit_value_surface`; this
/// accessor is retained for any remaining consumer that still
/// inspects the variant classification surface (e.g. CSS-typed
/// accessor emission + the admitted-projection structural gates).
///
/// The emitted struct ident on `VariantInfoShape::Projection` is a
/// placeholder — the prefix-aware construction stays local to
/// `emit_value_surface`; downstream consumers inspect only
/// `has_cursor_fields`.
pub fn variant_entries_for(ir: &GrammarIR) -> Vec<VariantInfo> {
    let non_transparent: Vec<&IrRule> = ir
        .rules
        .iter()
        .filter(|r| !r.meta.is_transparent)
        .collect();
    let resolver = RustNamedTypes::from_ir(ir);
    let admissions = collect_projection_admissions(ir, &resolver);
    let mut out: Vec<VariantInfo> = Vec::with_capacity(non_transparent.len());
    let mut seen_names: std::collections::HashSet<String> =
        std::collections::HashSet::with_capacity(non_transparent.len());
    for rule in &non_transparent {
        let raw_name = ir.get_string(rule.name).to_string();
        let mut name = raw_name.clone();
        let mut idx = 0;
        while !seen_names.insert(name.clone()) {
            idx += 1;
            name = format!("{}_{}", raw_name, idx);
        }
        let shape = if let Some(admission) =
            admissions.iter().find(|a| a.rule_name() == raw_name)
        {
            VariantInfoShape::Projection {
                has_cursor_fields: admission.plan().has_cursor_fields,
            }
        } else {
            let type_desc = ir
                .types
                .iter()
                .find_map(|(id, ty)| (*id == rule.id).then_some(ty));
            match type_desc {
                Some(TypeDesc::Span) => VariantInfoShape::Span,
                Some(td) if td.is_scalar_payload() && !matches!(td, TypeDesc::Span) => {
                    VariantInfoShape::Scalar(td.clone())
                }
                _ => match peel::unwrap_structural_wrappers(&rule.body) {
                    IrNode::Seq(_) | IrNode::Alt(_, _) | IrNode::Repeat { .. } => {
                        VariantInfoShape::Compound
                    }
                    IrNode::Literal(_) | IrNode::Regex(_) => VariantInfoShape::Span,
                    _ => VariantInfoShape::Cursor,
                },
            }
        };
        out.push(VariantInfo {
            name,
            rule_id: rule.id,
            shape,
        });
    }
    out
}

/// Public mirror of [`VariantEntry`] for downstream emitters. The
/// private form carries module-private `VariantShape`; the public form
/// re-exports the same information so the materialise emitter can
/// match on the shape without the private type leaking.
pub struct VariantInfo {
    pub name: String,
    pub rule_id: u32,
    pub shape: VariantInfoShape,
}

/// Public mirror of [`VariantShape`].
pub enum VariantInfoShape {
    /// Admission — the emitter routes through the matching
    /// `materialize_projection_<rule>_<Grammar>` fn. `has_cursor_fields`
    /// mirrors the admission's plan so the downstream emitter can
    /// decide whether the projection struct carries a `'p` lifetime.
    Projection {
        has_cursor_fields: bool,
    },
    Span,
    Scalar(TypeDesc),
    Compound,
    Cursor,
}
