//! AY.W3b.2 — json-prototype per-shape inline fn pattern. The
//! BEAT-sonic lever per AY.md prop 3: emit five `#[inline(always)]`
//! per-shape fns per grammar, plus the root materialiser, each
//! monomorphised at the `parsed.to_value()` call site so LLVM
//! inlines the entire tree-build into a single flat function.
//! Matches `json-prototype`'s 0.89-0.94× sonic ceiling.
//!
//! # The five shapes
//!
//! Each grammar emits:
//!
//! - `materialize_object_<Grammar>` — compound whose children are
//!   `(key, value)` alternating pairs. Returns `Vec<<Grammar>Value>`
//!   of length `2 * entry_count`, ready for the root dispatcher to
//!   wrap in the grammar's Compound variant.
//! - `materialize_array_<Grammar>` — compound whose children are
//!   sequenced values. Returns `Vec<<Grammar>Value>` in declaration
//!   order.
//! - `materialize_string_<Grammar>` — Span-shaped leaves. Resolves
//!   through `payload_Span` with a span-text fallback; returns a
//!   borrowed `&'p str`.
//! - `materialize_number_<Grammar>` — f64-shaped leaves. Reads
//!   `payload_f64` with a span-parse fallback.
//! - `materialize_literal_<Grammar>` — Bool leaves. Reads
//!   `payload_bool` with a span-text fallback.
//!
//! Plus the root dispatcher:
//!
//! - `materialize_value_<Grammar>` — inspects `rule_kind()` and
//!   tail-calls the appropriate per-shape fn before wrapping the
//!   result in the grammar's `<Grammar>Value` variant.
//!
//! # Dispatch shape
//!
//! The root dispatcher is a single `match view.rule_kind() { … }`
//! with one arm per variant entry. The per-shape fns do the walk;
//! the root constructs the variant. Under LLVM with
//! `#[inline(always)]` on every per-shape fn, the root body
//! collapses into one flat function at the caller's site — the
//! same layout `json-prototype::parse_value` produces.
//!
//! # AY.W6.2 — grammar-derived direct-to-struct projection path
//!
//! In addition to the five-shape materialisers, every rule admitted
//! to the direct-to-struct surface by the layout pass
//! (`ir.payload_layouts` populated) emits a per-rule
//! `materialize_projection_<rule>_<Grammar>` helper that reads the
//! packed aggregate payload directly and constructs the matching
//! `<Grammar><RuleCamel>Projection` struct. The helper bypasses the
//! `Vec<<Grammar>Value>` intermediate — consumers that know the
//! admitted shape route straight through it without the Compound-
//! variant walker. The helper's presence in the expanded emitter
//! output is structural evidence the admission fact reaches the
//! materialiser crate (`cargo expand -p bbnf --test
//! named_type_preservation` surfaces one helper per admitted
//! rule).

use bbnf_ir::passes::{NamedTypeResolver, PayloadField, PayloadLayout};
use bbnf_ir::{GrammarIR, RuleId, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::backend::rust::view::{VariantInfo, VariantInfoShape, variant_entries_for};

/// Emit the per-shape + root materialise fns for `ir`.
///
/// Returns an empty [`TokenStream`] when the grammar has no
/// non-transparent rules.
pub fn emit_materialize_fns(ir: &GrammarIR, grammar_name: &str) -> TokenStream {
    let variants = variant_entries_for(ir);
    if variants.is_empty() {
        return quote! {};
    }

    let value_ident = format_ident!("{}Value", grammar_name);
    let node_view_ident = format_ident!("{}NodeView", grammar_name);
    let rule_kind_ident = format_ident!("{}RuleKind", grammar_name);

    let object_fn = emit_object_fn(&value_ident, &node_view_ident, grammar_name);
    let array_fn = emit_array_fn(&value_ident, &node_view_ident, grammar_name);
    let string_fn = emit_string_fn(&node_view_ident, grammar_name);
    let number_fn = emit_number_fn(&node_view_ident, grammar_name);
    let literal_fn = emit_literal_fn(&node_view_ident, grammar_name);
    let root_fn = emit_root_fn(
        &value_ident,
        &node_view_ident,
        &rule_kind_ident,
        &variants,
        grammar_name,
    );

    // === W0.c: fused pipeline read-side ===
    //
    // AY-II.W0.c — emit the fused-pipeline read-side projection entry
    // `project_value_<Grammar>`. Consumes a `ValueBuilderOutput<R>`
    // (the parallel substrate populated at parse time) and projects
    // the root frame into the grammar's `<Grammar>Value` enum via
    // the rule-id dispatch table. No tape access — the typed value
    // is reconstructed solely from the value substrate's frames +
    // payload columns + the caller's input slice.
    let grammar_ident = format_ident!("{}", grammar_name);
    let project_fn = emit_project_value_fn(
        &value_ident,
        &rule_kind_ident,
        &grammar_ident,
        &variants,
        grammar_name,
    );
    // === end W0.c fused pipeline read-side ===

    // === W0.d: projection totality emission ===
    //
    // AY.W6.2 — per-rule direct-to-struct projection helpers.
    // Consumes `ir.payload_layouts` directly: every admitted rule
    // produces one `materialize_projection_<rule>_<Grammar>` helper
    // that reads the packed aggregate payload and constructs the
    // `<Grammar><RuleCamel>Projection` struct emitted by
    // `emitter/grammar.rs::emit_direct_to_struct_projection`. W0.d
    // extends this to include resolver-backed admissions so the
    // totality invariant (`PROJECTION_DIRECT_TO_STRUCT.len() ==
    // count(materialize_projection_* fns) == count(consumer call
    // sites)`) holds per grammar.
    let projection_fns = emit_projection_fns(ir, &node_view_ident, grammar_name);
    // === end W0.d projection totality emission ===

    quote! {
        #object_fn
        #array_fn
        #string_fn
        #number_fn
        #literal_fn
        #root_fn
        #project_fn
        #projection_fns
    }
}

// ════════════════════════════════════════════════════════════════════
// === W0.c: fused pipeline read-side ===
// ════════════════════════════════════════════════════════════════════

/// AY-II.W0.c — emit `project_value_<Grammar>`.
///
/// Read-side companion of
/// [`ValueBuilder::finish`](crate::runtime::ValueBuilder::finish) +
/// [`Parsed::new_fused`](crate::runtime::Parsed::new_fused). The
/// emitted fn consumes a
/// [`ValueBuilderOutput`](crate::runtime::ValueBuilderOutput) and
/// an `&'p str` input slice, projects the root frame into the
/// grammar's `<Grammar>Value<'p>` enum, and returns without
/// touching the tape.
///
/// Emission shape:
///
/// 1. `project_frame_<Grammar>(output, input, offset) ->
///    <Grammar>Value<'p>` — recursive per-frame projector that
///    dispatches on the frame's `variant_idx` and constructs the
///    matching variant. Compound variants recurse into their child
///    frames through this same fn.
/// 2. `project_value_<Grammar>(output, input) ->
///    <Grammar>Value<'p>` — root entry; resolves the root frame
///    offset and tail-calls `project_frame_<Grammar>`.
///
/// Both fns are `#[inline]`; LLVM collapses the recursion at
/// monomorphisation when the projection tree is non-recursive
/// (scalar + Span variants).
fn emit_project_value_fn(
    value_ident: &syn::Ident,
    rule_kind_ident: &syn::Ident,
    grammar_ident: &syn::Ident,
    variants: &[VariantInfo],
    grammar_name: &str,
) -> TokenStream {
    let root_fn = format_ident!("project_value_{}", grammar_name);
    let frame_fn = format_ident!("project_frame_{}", grammar_name);
    let dispatch_fn = format_ident!("project_rule_kind_{}", grammar_name);

    // Rule-id → RuleKind dispatch, scoped to this grammar.
    let dispatch_arms: Vec<TokenStream> = variants
        .iter()
        .map(|v| {
            let kind_variant = format_ident!("{}", v.name);
            let idx_lit = (v.rule_id & 0xFF) as u8;
            quote! { #idx_lit => #rule_kind_ident::#kind_variant, }
        })
        .collect();

    // Per-variant projection arms for `project_frame_<Grammar>`.
    let project_arms: Vec<TokenStream> = variants
        .iter()
        .map(|v| emit_project_arm(v, value_ident, rule_kind_ident, &frame_fn))
        .collect();

    quote! {
        /// AY-II.W0.c — rule-id → RuleKind dispatch local to the
        /// fused-pipeline projection path. Mirrors the view layer's
        /// `rule_kind()` dispatch; scoped to the projection module
        /// to keep the two consumer paths coupled only through the
        /// `RuleKind` enum itself.
        #[inline(always)]
        fn #dispatch_fn(variant_idx: u8) -> #rule_kind_ident {
            match variant_idx {
                #(#dispatch_arms)*
                _ => #rule_kind_ident::Unknown,
            }
        }

        /// AY-II.W0.c — per-frame projector. Reads one frame from
        /// the value substrate and constructs the matching
        /// `<Grammar>Value` variant. Compound variants recurse into
        /// their child frames through this same fn. `#[inline]` so
        /// LLVM can fold the dispatch into the caller when the
        /// frame tree is bounded.
        #[inline]
        fn #frame_fn<'p>(
            output: &::bbnf::runtime::ValueBuilderOutput<#grammar_ident>,
            input: &'p str,
            offset: u32,
        ) -> #value_ident<'p> {
            let frame = match output.frame(offset) {
                ::core::option::Option::Some(f) => f,
                ::core::option::Option::None => {
                    // Substrate-inconsistency (offset out of range)
                    // — the fused-pipeline write side writes in
                    // push order, so every offset the emitter
                    // hands the projector is valid by
                    // construction; an out-of-range offset is an
                    // IR-invariant violation.
                    ::core::panic!(
                        "AY-II.W0.c: frame offset {} out of range (frames: {})",
                        offset,
                        output.frame_count(),
                    );
                }
            };
            match #dispatch_fn(frame.variant_idx) {
                #(#project_arms)*
                _ => {
                    // Recovery / unclassified records + sub-variant
                    // discriminators. AY-II.W0.c reserves these
                    // cases for a future recovery-aware projection
                    // landing. A frame whose rule-kind lands in
                    // Unknown (or in a sub-variant the main rule
                    // dispatch does not classify) is an IR-invariant
                    // violation for the fused-pipeline projection;
                    // panic rather than silently fall back to
                    // tape-walking.
                    let _ = frame;
                    ::core::panic!(
                        "AY-II.W0.c: unclassified variant_idx {} on frame at offset {}; \
                         fused-pipeline projection requires classified records",
                        frame.variant_idx,
                        offset,
                    );
                }
            }
        }

        /// AY-II.W0.c — fused-pipeline root projector. Reads the
        /// root frame from the value substrate and constructs the
        /// grammar's `<Grammar>Value<'p>` in one pass. No tape
        /// access, no reparse, no visitor dispatch.
        #[inline]
        fn #root_fn<'p>(
            output: &::bbnf::runtime::ValueBuilderOutput<#grammar_ident>,
            input: &'p str,
        ) -> #value_ident<'p> {
            if output.is_empty() {
                // Empty value substrate — only reachable via a
                // substrate-only `Parsed::new` that bypassed the
                // fused pipeline. AY-II.W0.c treats this as an IR
                // invariant violation: the fused parse entry
                // always populates the substrate. No silent
                // fallback to tape-walking.
                ::core::panic!(
                    "AY-II.W0.c: Parsed::to_value() called on an empty value substrate; \
                     fused parse entry was not invoked. See \
                     docs/tranches/AY-II/waves/W0.md §W0.c."
                );
            }
            #frame_fn(output, input, output.root_offset())
        }
    }
}

/// Emit one per-variant projection arm for `project_frame_<Grammar>`.
///
/// Shape-specialised:
/// - Span → `&input[lo..hi]` wrapped in the Span variant.
/// - Scalar(td) → decode the frame's payload tag into the typed
///   primitive; fall back to span-text parse when no payload was
///   recorded at parse time.
/// - Compound → walk the frame's child run, project each child
///   recursively via `project_frame_<Grammar>`, collect into a
///   `Vec<<Grammar>Value>`.
/// - Cursor → W0.c defers cursor-wrapping variants to the
///   recovery-projection follow-on; the arm panics on hit to
///   preserve the "no fallback" contract.
fn emit_project_arm(
    v: &VariantInfo,
    value_ident: &syn::Ident,
    rule_kind_ident: &syn::Ident,
    frame_fn: &syn::Ident,
) -> TokenStream {
    let kind_variant = format_ident!("{}", v.name);
    let value_variant = format_ident!("{}", v.name);
    match &v.shape {
        VariantInfoShape::Span => quote! {
            #rule_kind_ident::#kind_variant => {
                let span = &input[frame.span_lo as usize..frame.span_hi as usize];
                #value_ident::#value_variant(span)
            }
        },
        VariantInfoShape::Scalar(td) => emit_scalar_project_arm(
            rule_kind_ident,
            &kind_variant,
            value_ident,
            &value_variant,
            td,
        ),
        VariantInfoShape::Compound => quote! {
            #rule_kind_ident::#kind_variant => {
                // Compound variant — walk the frame's child run and
                // recurse via `project_frame_<Grammar>`. Child
                // offsets come from the value substrate's frame
                // arena; each child is projected in push order
                // (matches the emitter's declared field order at
                // parse time).
                let mut children: ::std::vec::Vec<#value_ident<'p>> =
                    ::std::vec::Vec::with_capacity(frame.child_count as usize);
                for (child_off, _child_frame) in output.children(offset) {
                    children.push(#frame_fn(output, input, child_off));
                }
                #value_ident::#value_variant(children)
            }
        },
        VariantInfoShape::Cursor => quote! {
            #rule_kind_ident::#kind_variant => {
                // Cursor variants wrap `<Grammar>NodeView` — those
                // are tape-backed and therefore outside the fused-
                // pipeline projection's contract. A grammar whose
                // classified rule lands on the Cursor shape is
                // one the emitter has not yet produced a value-
                // substrate projection for; the projection panics
                // rather than silently fall back to tape-walking.
                ::core::panic!(
                    "AY-II.W0.c: Cursor-shape variant projection not yet available; \
                     frame offset {}",
                    offset,
                );
            }
        },
    }
}

/// Emit a Scalar variant projection arm — decodes the frame's
/// payload tag into the typed primitive. Falls back to span-text
/// parsing when no payload was recorded (leaves whose rule did not
/// stage a typed payload at parse time).
fn emit_scalar_project_arm(
    rule_kind_ident: &syn::Ident,
    kind_variant: &syn::Ident,
    value_ident: &syn::Ident,
    value_variant: &syn::Ident,
    td: &TypeDesc,
) -> TokenStream {
    let fallback = scalar_fallback_from_span(td);
    match td {
        TypeDesc::F64 => quote! {
            #rule_kind_ident::#kind_variant => {
                let v: f64 = output.payload_for(frame)
                    .and_then(|p| p.as_f64())
                    .unwrap_or_else(|| { #fallback });
                #value_ident::#value_variant(v)
            }
        },
        TypeDesc::Bool => quote! {
            #rule_kind_ident::#kind_variant => {
                let v: bool = output.payload_for(frame)
                    .and_then(|p| p.as_bool())
                    .unwrap_or_else(|| { #fallback });
                #value_ident::#value_variant(v)
            }
        },
        TypeDesc::U32 => quote! {
            #rule_kind_ident::#kind_variant => {
                let v: u32 = output.payload_for(frame)
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
                    let v: #ty_ident = output.payload_for(frame)
                        .and_then(|p| p.as_u32())
                        .map(|v| v as #ty_ident)
                        .unwrap_or_else(|| { #fallback });
                    #value_ident::#value_variant(v)
                }
            }
        }
    }
}

/// Scalar fallback — reads the source span and parses into the
/// target primitive. Mirrors [`scalar_fallback`] but binds against
/// the projection context (`input` + `frame`) rather than the
/// view's `span_text()`.
///
/// The `&input[..]` slice is parenthesised to bind `&` to the full
/// slice expression before the `.parse()` method resolves — without
/// the parens the `&` captures the entire `Result` and yields a
/// `&i64` / `&f64` mismatch.
fn scalar_fallback_from_span(td: &TypeDesc) -> TokenStream {
    let slice = quote! {
        (&input[frame.span_lo as usize..frame.span_hi as usize])
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

// ════════════════════════════════════════════════════════════════════
// === end W0.c fused pipeline read-side ===
// ════════════════════════════════════════════════════════════════════

/// Object shape — interleaved (key, value) children. Walks the
/// compound's direct children and drives each through the root
/// materialiser, so keys and values are both recursively
/// materialised in declaration order.
fn emit_object_fn(
    value_ident: &syn::Ident,
    node_view_ident: &syn::Ident,
    grammar_name: &str,
) -> TokenStream {
    let fn_name = format_ident!("materialize_object_{}", grammar_name);
    let root_fn = format_ident!("materialize_value_{}", grammar_name);
    quote! {
        /// AY.W3b.2 — object shape walker. Returns the compound's
        /// children recursively materialised, ready for the root
        /// dispatcher to wrap in its grammar-specific Compound variant.
        #[inline(always)]
        fn #fn_name<'p>(view: #node_view_ident<'p>) -> ::std::vec::Vec<#value_ident<'p>> {
            let mut out: ::std::vec::Vec<#value_ident<'p>> =
                ::std::vec::Vec::with_capacity(view.cursor().child_count().max(1) * 2);
            for child in view.children() {
                out.push(#root_fn(child));
            }
            out
        }
    }
}

/// Array shape — sequenced child materialisation.
fn emit_array_fn(
    value_ident: &syn::Ident,
    node_view_ident: &syn::Ident,
    grammar_name: &str,
) -> TokenStream {
    let fn_name = format_ident!("materialize_array_{}", grammar_name);
    let root_fn = format_ident!("materialize_value_{}", grammar_name);
    quote! {
        /// AY.W3b.2 — array shape walker. Produces a
        /// `Vec<<Grammar>Value>` of the compound's children in
        /// declaration order.
        #[inline(always)]
        fn #fn_name<'p>(view: #node_view_ident<'p>) -> ::std::vec::Vec<#value_ident<'p>> {
            let mut out: ::std::vec::Vec<#value_ident<'p>> =
                ::std::vec::Vec::with_capacity(view.cursor().child_count());
            for child in view.children() {
                out.push(#root_fn(child));
            }
            out
        }
    }
}

/// String shape (Span leaf). Resolves through `payload_Span`
/// then falls back to the cursor's own span text. Zero-copy
/// borrow from the input slice.
fn emit_string_fn(node_view_ident: &syn::Ident, grammar_name: &str) -> TokenStream {
    let fn_name = format_ident!("materialize_string_{}", grammar_name);
    quote! {
        /// AY.W3b.2 — string (Span) materialiser. Zero-copy borrow
        /// from the input slice.
        #[inline(always)]
        fn #fn_name<'p>(view: #node_view_ident<'p>) -> &'p str {
            let tape = view.cursor().tape();
            let rec = view.cursor().record();
            if let Some((lo, hi)) = tape.payload_Span(rec) {
                return &view.input()[lo as usize..hi as usize];
            }
            view.span_text()
        }
    }
}

/// Number shape — f64 specialisation. The root dispatcher reads
/// per-TypeDesc payloads inline at its arm; this fn is the shared
/// f64 fallback for rules whose type classification is Number at
/// emit time.
fn emit_number_fn(node_view_ident: &syn::Ident, grammar_name: &str) -> TokenStream {
    let fn_name = format_ident!("materialize_number_{}", grammar_name);
    quote! {
        /// AY.W3b.2 — number (f64) materialiser. Payload-first
        /// read with a span-parse fallback.
        #[inline(always)]
        fn #fn_name<'p>(view: #node_view_ident<'p>) -> f64 {
            let tape = view.cursor().tape();
            let rec = view.cursor().record();
            if let Some(v) = tape.payload_f64(rec) {
                return v;
            }
            view.span_text().parse::<f64>().unwrap_or(0.0)
        }
    }
}

/// Literal shape (Bool). Payload-first read with a span-text
/// comparison fallback.
fn emit_literal_fn(node_view_ident: &syn::Ident, grammar_name: &str) -> TokenStream {
    let fn_name = format_ident!("materialize_literal_{}", grammar_name);
    quote! {
        /// AY.W3b.2 — literal / keyword materialiser.
        #[inline(always)]
        fn #fn_name<'p>(view: #node_view_ident<'p>) -> bool {
            let tape = view.cursor().tape();
            let rec = view.cursor().record();
            if let Some(v) = tape.payload_bool(rec) {
                return v;
            }
            view.span_text() == "true"
        }
    }
}

/// Root dispatcher — `materialize_value_<Grammar>`. Single `match
/// view.rule_kind() { … }` with one arm per variant:
///
/// - Span variants → read the span text via the string materialiser.
/// - Scalar variants → read the typed payload inline.
/// - Compound variants → drive children through the object walker
///   and wrap in the Compound variant.
/// - Cursor variants → wrap the `NodeView` directly.
/// - Unknown → fallback.
fn emit_root_fn(
    value_ident: &syn::Ident,
    node_view_ident: &syn::Ident,
    rule_kind_ident: &syn::Ident,
    variants: &[VariantInfo],
    grammar_name: &str,
) -> TokenStream {
    let fn_name = format_ident!("materialize_value_{}", grammar_name);
    let string_fn = format_ident!("materialize_string_{}", grammar_name);
    let number_fn = format_ident!("materialize_number_{}", grammar_name);
    let literal_fn = format_ident!("materialize_literal_{}", grammar_name);
    let object_fn = format_ident!("materialize_object_{}", grammar_name);

    let arms: Vec<TokenStream> = variants
        .iter()
        .map(|v| {
            let kind_variant = format_ident!("{}", v.name);
            let value_variant = format_ident!("{}", v.name);

            match &v.shape {
                VariantInfoShape::Span => {
                    quote! {
                        #rule_kind_ident::#kind_variant => {
                            #value_ident::#value_variant(#string_fn(view))
                        }
                    }
                }
                VariantInfoShape::Scalar(td) => emit_scalar_arm(
                    rule_kind_ident,
                    &kind_variant,
                    value_ident,
                    &value_variant,
                    td,
                    &number_fn,
                    &literal_fn,
                ),
                VariantInfoShape::Compound => {
                    // Route through the object walker — its capacity
                    // hint handles interleaved key-value compounds
                    // and degrades gracefully to `child_count()` on
                    // array-shaped rules. The Array walker is still
                    // emitted as a specialised alternative the
                    // future compound-classification pass can call.
                    quote! {
                        #rule_kind_ident::#kind_variant => {
                            let children = #object_fn(view);
                            #value_ident::#value_variant(children)
                        }
                    }
                }
                VariantInfoShape::Cursor => {
                    quote! {
                        #rule_kind_ident::#kind_variant => {
                            #value_ident::#value_variant(view)
                        }
                    }
                }
            }
        })
        .collect();

    quote! {
        /// AY.W3b.2 — root value materialiser. Dispatches on
        /// `rule_kind()` and constructs the grammar's
        /// `<Grammar>Value` variant directly. Every per-shape fn
        /// it calls is `#[inline(always)]`; this root is `#[inline]`
        /// so the whole tree collapses into a single flat function
        /// at the `parsed.to_value()` call site.
        #[inline]
        fn #fn_name<'p>(view: #node_view_ident<'p>) -> #value_ident<'p> {
            match view.rule_kind() {
                #(#arms)*
                _ => #value_ident::Unknown(view),
            }
        }
    }
}

/// Emit a Scalar variant arm — reads the typed payload inline.
/// F64 and Bool variants route through the dedicated per-shape
/// fns; other scalar types read directly via the grammar's typed
/// payload accessor.
fn emit_scalar_arm(
    rule_kind_ident: &syn::Ident,
    kind_variant: &syn::Ident,
    value_ident: &syn::Ident,
    value_variant: &syn::Ident,
    td: &TypeDesc,
    number_fn: &syn::Ident,
    literal_fn: &syn::Ident,
) -> TokenStream {
    match td {
        TypeDesc::F64 => {
            quote! {
                #rule_kind_ident::#kind_variant => {
                    #value_ident::#value_variant(#number_fn(view))
                }
            }
        }
        TypeDesc::Bool => {
            quote! {
                #rule_kind_ident::#kind_variant => {
                    #value_ident::#value_variant(#literal_fn(view))
                }
            }
        }
        _ => {
            let ident = td
                .rust_ident()
                .expect("scalar TypeDesc has rust_ident");
            let payload_fn = format_ident!("payload_{}", ident);
            let ty_ident = format_ident!("{}", ident);
            let fallback = scalar_fallback(td);
            quote! {
                #rule_kind_ident::#kind_variant => {
                    let tape = view.cursor().tape();
                    let rec = view.cursor().record();
                    let v: #ty_ident = if let Some(v) = tape.#payload_fn(rec) {
                        v
                    } else {
                        #fallback
                    };
                    #value_ident::#value_variant(v)
                }
            }
        }
    }
}

/// AY.W6.2 — emit per-rule direct-to-struct projection helpers.
///
/// Walks every admitted rule in `ir.payload_layouts` and produces one
/// `materialize_projection_<rule>_<Grammar>` helper per admission. The
/// helper:
///
/// 1. Reads the packed aggregate payload bytes via the tape's
///    `payload_bytes` accessor (one slice of `layout.total_bytes`).
/// 2. Decodes each scalar field from its admitted offset + type.
/// 3. Constructs the `<Grammar><RuleCamel>Projection` struct emitted
///    by `emitter/grammar.rs::emit_direct_to_struct_projection`.
///
/// The helper returns `Option<_>` — `None` when the aggregate buffer
/// is shorter than the admitted layout (a corrupted tape) or when the
/// record carries no payload (non-aggregate path). Consumers that
/// know the admitted shape call this helper directly; consumers that
/// do not continue through the `Vec<<Grammar>Value>` compound path
/// emitted by [`emit_object_fn`] / [`emit_array_fn`].
///
/// Iteration order mirrors `ir.rules` declaration order; deterministic
/// emission is what keeps the `cargo expand` output stable across
/// rebuilds.
fn emit_projection_fns(
    ir: &GrammarIR,
    node_view_ident: &syn::Ident,
    grammar_name: &str,
) -> TokenStream {
    let grammar_prefix = to_upper_camel(grammar_name);
    // Mirror the GrammarLayout arm of
    // `emitter/grammar.rs::collect_projection_admissions` so every
    // helper emitted here references a struct that
    // `emit_projection_structs` also emitted. The two arms are
    // mutually exclusive: a resolver-backed `Named(sid)` admission
    // takes the legacy `__named_type_shim_<name>` path — no synthesised
    // projection struct — while a layout-derived admission emits the
    // `<Grammar><RuleCamel>Projection` struct + marker + helper. Both
    // emitters consult the same `ir.payload_layouts` +
    // `RustNamedTypes` facts; the local re-implementation keeps the
    // value-materialise emitter free of cross-emitter coupling while
    // preserving the single admission source.
    let resolver = crate::backend::rust::view::named_types::RustNamedTypes::from_ir(ir);
    let mut fns: Vec<TokenStream> = Vec::new();
    for rule in &ir.rules {
        if rule.meta.is_transparent {
            continue;
        }
        // Resolver-backed arm dominates: if the rule's type is a
        // named projection the resolver knows about, skip — the
        // layout for this rule is represented by the resolver's
        // tuple shape, not by a synthesised projection struct.
        let type_desc = ir
            .types
            .iter()
            .find_map(|(id, ty)| (*id == rule.id).then_some(ty));
        if let Some(TypeDesc::Named(sid)) = type_desc {
            if resolver.resolve_named(*sid).is_some() {
                continue;
            }
        }
        let Some(layout) = ir.payload_layouts.get(&rule.id) else {
            continue;
        };
        if layout.fields.is_empty() {
            continue;
        }
        let rule_name = ir.get_string(rule.name).to_string();
        fns.push(emit_projection_fn(
            rule.id,
            &rule_name,
            layout,
            node_view_ident,
            grammar_name,
            &grammar_prefix,
        ));
    }
    quote! { #(#fns)* }
}

/// AY.W6.2 — emit a single direct-to-struct projection helper.
///
/// The emitted `#[inline]` fn consumes a `<Grammar>NodeView` and
/// returns `Option<<Grammar><RuleCamel>Projection>`; the body reads
/// `view.cursor().tape().payload_bytes(rec, TOTAL_BYTES)` and decodes
/// each admitted field. For `Span` fields the helper packs
/// `(lo, hi)` as `(u32, u32)` — the struct's field representation;
/// the input slice is not touched at projection time, so the
/// projection stays plain-data (`Copy`).
fn emit_projection_fn(
    _rule_id: RuleId,
    rule_name: &str,
    layout: &PayloadLayout,
    node_view_ident: &syn::Ident,
    grammar_name: &str,
    grammar_prefix: &str,
) -> TokenStream {
    let fn_ident = format_ident!(
        "materialize_projection_{}_{}",
        sanitise_ident(rule_name),
        grammar_name,
    );
    let struct_ident = format_ident!(
        "{}{}Projection",
        grammar_prefix,
        to_upper_camel(rule_name),
    );
    let total_bytes = layout.total_bytes as usize;
    let total_bytes_lit = proc_macro2::Literal::usize_unsuffixed(total_bytes);
    let field_inits: Vec<TokenStream> = layout
        .fields
        .iter()
        .enumerate()
        .map(|(idx, field)| emit_projection_field_read(idx, field))
        .collect();
    let field_names: Vec<_> = (0..layout.fields.len())
        .map(|idx| format_ident!("field_{}", idx))
        .collect();
    quote! {
        /// AY.W6.2 — grammar-derived direct-to-struct projection
        /// helper. Reads the packed aggregate payload for the
        /// admitted rule and constructs the matching projection
        /// struct; returns `None` when the tape's aggregate buffer is
        /// too short or the record carries no payload.
        ///
        /// Consumers that know the admitted shape call this helper
        /// directly, bypassing the `Vec<<Grammar>Value>` compound
        /// path. The helper is `#[inline]` so LLVM collapses it into
        /// the caller at monomorphisation time.
        #[inline]
        #[doc(hidden)]
        pub fn #fn_ident<'p>(view: #node_view_ident<'p>) -> ::core::option::Option<#struct_ident> {
            let tape = view.cursor().tape();
            let rec = view.cursor().record();
            let __bytes = tape.payload_bytes(rec, #total_bytes_lit)?;
            #(#field_inits)*
            ::core::option::Option::Some(#struct_ident {
                #(#field_names),*
            })
        }
    }
}

/// AY.W6.2 — emit one field-decode block for a projection helper.
///
/// Reads `<field_size>` bytes from the packed aggregate at the
/// admitted offset and converts to the field's Rust type. Span
/// fields decode the `(u32 lo, u32 hi)` pair; other scalars decode
/// via little-endian `from_le_bytes` matching the emission side's
/// `to_le_bytes` write.
fn emit_projection_field_read(idx: usize, field: &PayloadField) -> TokenStream {
    let offset = field.offset as usize;
    let offset_lit = proc_macro2::Literal::usize_unsuffixed(offset);
    let field_ident = format_ident!("field_{}", idx);
    match &field.ty {
        TypeDesc::Span => {
            let lo_end = offset + 4;
            let hi_end = offset + 8;
            let lo_end_lit = proc_macro2::Literal::usize_unsuffixed(lo_end);
            let hi_end_lit = proc_macro2::Literal::usize_unsuffixed(hi_end);
            quote! {
                let #field_ident: (u32, u32) = {
                    let __lo = u32::from_le_bytes(
                        <[u8; 4]>::try_from(&__bytes[#offset_lit..#lo_end_lit]).ok()?,
                    );
                    let __hi = u32::from_le_bytes(
                        <[u8; 4]>::try_from(&__bytes[#lo_end_lit..#hi_end_lit]).ok()?,
                    );
                    (__lo, __hi)
                };
            }
        }
        TypeDesc::Bool => {
            let end_lit =
                proc_macro2::Literal::usize_unsuffixed(offset + 1);
            quote! {
                let #field_ident: bool = {
                    let __b = *__bytes.get(#offset_lit)?;
                    let _ = #end_lit;
                    __b != 0
                };
            }
        }
        TypeDesc::I8 | TypeDesc::U8 => {
            let ty_tokens = projection_field_primitive(&field.ty);
            let end_lit =
                proc_macro2::Literal::usize_unsuffixed(offset + 1);
            quote! {
                let #field_ident: #ty_tokens = {
                    let __b = *__bytes.get(#offset_lit)?;
                    let _ = #end_lit;
                    __b as #ty_tokens
                };
            }
        }
        TypeDesc::I16 | TypeDesc::U16 => {
            let ty_tokens = projection_field_primitive(&field.ty);
            let end_lit =
                proc_macro2::Literal::usize_unsuffixed(offset + 2);
            quote! {
                let #field_ident: #ty_tokens = {
                    let __arr = <[u8; 2]>::try_from(
                        &__bytes[#offset_lit..#end_lit],
                    ).ok()?;
                    #ty_tokens::from_le_bytes(__arr)
                };
            }
        }
        TypeDesc::I32 | TypeDesc::U32 => {
            let ty_tokens = projection_field_primitive(&field.ty);
            let end_lit =
                proc_macro2::Literal::usize_unsuffixed(offset + 4);
            quote! {
                let #field_ident: #ty_tokens = {
                    let __arr = <[u8; 4]>::try_from(
                        &__bytes[#offset_lit..#end_lit],
                    ).ok()?;
                    #ty_tokens::from_le_bytes(__arr)
                };
            }
        }
        TypeDesc::I64 | TypeDesc::U64 => {
            let ty_tokens = projection_field_primitive(&field.ty);
            let end_lit =
                proc_macro2::Literal::usize_unsuffixed(offset + 8);
            quote! {
                let #field_ident: #ty_tokens = {
                    let __arr = <[u8; 8]>::try_from(
                        &__bytes[#offset_lit..#end_lit],
                    ).ok()?;
                    #ty_tokens::from_le_bytes(__arr)
                };
            }
        }
        TypeDesc::F64 => {
            let end_lit =
                proc_macro2::Literal::usize_unsuffixed(offset + 8);
            quote! {
                let #field_ident: f64 = {
                    let __arr = <[u8; 8]>::try_from(
                        &__bytes[#offset_lit..#end_lit],
                    ).ok()?;
                    f64::from_le_bytes(__arr)
                };
            }
        }
        other => {
            // `PayloadField::ty` is `is_scalar_payload` by layout-pass
            // invariant (crates/ir/src/passes/payload/layout.rs); any
            // non-scalar here is an IR invariant violation.
            panic!(
                "AY.W6.2: payload-layout field must be a scalar payload; \
                 found {other:?} at offset {offset}",
            );
        }
    }
}

/// Map a scalar `TypeDesc` to its primitive Rust type identifier for
/// field-read codegen. Span is handled separately in
/// [`emit_projection_field_read`] as a `(u32, u32)` pair.
fn projection_field_primitive(ty: &TypeDesc) -> TokenStream {
    let ident = ty
        .rust_ident()
        .expect(
            "AY.W6.2: projection field primitive type must map via \
             TypeDesc::rust_ident",
        );
    let ty_ident = format_ident!("{}", ident);
    quote! { #ty_ident }
}

/// AY.W6.2 — upper-camel-case a name. Mirrors the helper in
/// `emitter/grammar.rs`; duplicated locally so this emitter stays
/// self-contained without importing the sibling module's private
/// helpers.
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
/// Mirrors the helper in `emitter/grammar.rs` so the emitted
/// `materialize_projection_<rule>_<Grammar>` ident matches the
/// `__grammar_projection_<rule>` marker naming from the sibling
/// emitter.
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

/// Span-text parse fallback for scalars without a payload slot.
/// Mirrors `view/leaves.rs::scalar_value_fallback` — identical
/// logic, inlined here so this module stays self-contained.
fn scalar_fallback(td: &TypeDesc) -> TokenStream {
    match td {
        TypeDesc::Bool => quote! { view.span_text() == "true" },
        TypeDesc::U32 => quote! {
            view.span_text().parse::<u32>().unwrap_or(0u32)
        },
        TypeDesc::F64 => quote! {
            view.span_text().parse::<f64>().unwrap_or(0.0)
        },
        TypeDesc::I8 => quote! { view.span_text().parse::<i8>().unwrap_or(0) },
        TypeDesc::U8 => quote! { view.span_text().parse::<u8>().unwrap_or(0) },
        TypeDesc::I16 => quote! {
            view.span_text().parse::<i16>().unwrap_or(0)
        },
        TypeDesc::U16 => quote! {
            view.span_text().parse::<u16>().unwrap_or(0)
        },
        TypeDesc::I32 => quote! {
            view.span_text().parse::<i32>().unwrap_or(0)
        },
        TypeDesc::I64 => quote! {
            view.span_text().parse::<i64>().unwrap_or(0)
        },
        TypeDesc::U64 => quote! {
            view.span_text().parse::<u64>().unwrap_or(0)
        },
        _ => quote! { ::core::default::Default::default() },
    }
}
