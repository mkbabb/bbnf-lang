//! AY-II.W0'.b — projection materializer emission.
//!
//! AZ-II.cutover.O4: these materializers are no longer emitted.
//! StructDirect grammars return document-owned runtime values and
//! must not emit `materialize_projection_*` functions against
//! `tape::Tape`.
//!
//! Post-W0'.b the materialize family collapses to a single emission
//! path: one `materialize_projection_<rule>_<Grammar>` helper per
//! grammar-derived direct-to-struct admission, called from the
//! `project_value_<Grammar>` dispatcher emitted by `view/value.rs`.
//! The legacy `materialize_object/array/string/number/literal/value_*`
//! per-shape family retired when the fused-pipeline projection routed
//! every admission through the grammar-derived helpers — the per-shape
//! fns had no call sites outside the retired `materialize_value_*`
//! root, which is itself dead after document-owned value surfaces
//! became the production parse result.
//!
//! # Materializer signature
//!
//! ```ignore
//! fn materialize_projection_<rule>_<Grammar>(
//!     output: &Tape<R>,
//!     input: &'p str,
//!     offset: u32,
//! ) -> Option<<Grammar><RuleCamel>Projection>
//! ```
//!
//! The helper reads from the fused slab (W0'.a-published
//! [`Tape<R>`](::tape::Tape)) via:
//!
//! - `output.frame(offset)` — the admission's own frame,
//!   carrying its `span_lo`/`span_hi` + `variant_idx`.
//! - `output.payload_for(frame)` — scalar payloads stamped at
//!   parse time (`f64`, `bool`, `u32`).
//! - `output.children(offset)` — direct child frames for rich
//!   admissions with cursor-child fields.
//! - `output.payload_bytes(rec, N)` — the tape's aggregate
//!   payload buffer for multi-field packed admissions (the grammar-
//!   derived byte offsets the layout pass assigned). The access is
//!   through `output`, NOT through `view.cursor().tape()`; the
//!   W0'.b hard gate prohibits the latter pattern.
//!
//! Span-typed admissions decode directly from the frame's span slots
//! without touching the tape — every `Span` field surfaces as
//! `(frame.span_lo, frame.span_hi)` because the value builder stamps
//! span boundaries on every frame in lockstep with the tape record.
//!
//! # Runtime-call-count evidence
//!
//! The materializer body ends with an `unwrap_or_else(panic)` at the
//! dispatcher site — so a materializer failing to return `Some(_)`
//! aborts the fused projection. `tests/projection_totality.rs`
//! parses a grammar-derived fixture per admitted rule and calls
//! `to_value()`; the resulting `<Grammar>Value` tree contains the
//! projection struct iff the materializer ran. A passing test is the
//! runtime-call-count evidence the projection-consumer wiring fires
//! for every admission exercised by the fixture corpus.

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use bbnf_ir::{GrammarIR, TypeDesc};

use super::super::grammar::{
    ProjectionAdmission, ProjectionFieldKind, collect_projection_admissions,
};

/// Emit the per-admission projection materialise fns for `ir`.
///
/// Returns an empty [`TokenStream`] for StructDirect grammars or when
/// the grammar has no admissions.
pub fn emit_materialize_fns(ir: &GrammarIR, grammar_name: &str) -> TokenStream {
    if !crate::backend::rust::view::should_emit_legacy_tape_surface(ir, grammar_name) {
        return quote! {};
    }

    emit_projection_fns(ir, grammar_name)
}

// ════════════════════════════════════════════════════════════════════
// === AY-II.W0'.b: projection totality emission ===
// ════════════════════════════════════════════════════════════════════
//
// AY-II.W0'.b — emit per-admission direct-to-struct projection
// helpers. Single source of truth: the shared
// [`collect_projection_admissions`] helper in sibling
// `emitter/grammar.rs`. Whatever rules admit there, materialisers
// emit here — one `materialize_projection_<rule>_<Grammar>` fn per
// admission. Every admission carries a grammar-derived
// [`ProjectionFieldPlan`] whose fields emit 1:1 into the materializer
// body.
//
// Iteration order mirrors the admission walk's iteration order
// (declaration order); deterministic emission is what keeps the
// `cargo expand` output stable across rebuilds.
fn emit_projection_fns(ir: &GrammarIR, grammar_name: &str) -> TokenStream {
    let grammar_prefix = to_upper_camel(grammar_name);
    let resolver = crate::backend::rust::view::named_types::RustNamedTypes::from_ir(ir);
    let admissions = collect_projection_admissions(ir, &resolver);
    let fns: Vec<TokenStream> = admissions
        .iter()
        .map(|admission| emit_projection_fn(admission, grammar_name, &grammar_prefix))
        .collect();
    quote! { #(#fns)* }
}

/// AY-II.W0'.b — emit a single direct-to-struct projection helper.
///
/// The emitted `#[inline]` fn consumes
/// `(output: &Tape<R>, input: &'p str, offset: u32)` and returns
/// `Option<<Grammar><RuleCamel>Projection>` (owned projection struct
/// when the admission is packed-only; `<...Projection><'p>` when rich
/// fields are present).
///
/// Field decoding:
///
/// - **Span scalar** (`ProjectionFieldKind::Scalar { ty: TypeDesc::Span, .. }`):
///   decodes as `(frame.span_lo, frame.span_hi)`. The value builder
///   stamps span slots on every frame, so `Span` fields read directly
///   from the admission's own frame — no aggregate byte decoding.
/// - **Non-Span scalar** (other `ProjectionFieldKind::Scalar`): decodes
///   from `output.payload_bytes(rec, TOTAL_BYTES)` at the
///   admitted byte offset. `rec` comes from
///   `output.try_get(TapeOffset(offset))`; the frame offset
///   equals the tape record offset by construction (every
///   `begin_compound` / `push_leaf` pushes one row into each
///   substrate in lockstep).
/// - **Cursor child** (`ProjectionFieldKind::CursorChild`): not yet
///   reachable under the slab reader — W0'.b's scope covers the
///   fused-slab read path for packed admissions; resolver-backed rich
///   admissions (e.g. CSS L4 `colorFn` which has only scalar fields)
///   populate through the packed buffer. A cursor-child admission
///   falling through to this arm panics to preserve the "no fallback"
///   contract.
fn emit_projection_fn(
    admission: &ProjectionAdmission,
    grammar_name: &str,
    grammar_prefix: &str,
) -> TokenStream {
    let rule_name = admission.rule_name();
    let fn_ident = format_ident!(
        "materialize_projection_{}_{}",
        sanitise_ident(rule_name),
        grammar_name,
    );
    let struct_ident = admission.struct_ident(grammar_prefix);
    // The grammar marker struct ident (e.g. `BbnfBootstrap`) parameterises
    // `Tape<R><R>` so the slab read carries the same `R` the consumer
    // declared via `impl ValueRoot for #grammar_ident`. Without this `R`
    // the emitted fn loses type-grammar coupling and `syn::parse2` accepts
    // it as `Tape<R>` without a generic argument — which then fails
    // `cargo check` against the `Tape<R><R>` definition in
    // `crates/tape/src/builder/output.rs`.
    let grammar_ident = format_ident!("{}", grammar_prefix);
    let plan = admission.plan();
    let total_bytes = plan.packed_bytes as usize;
    let total_bytes_lit = proc_macro2::Literal::usize_unsuffixed(total_bytes);

    let field_inits: Vec<TokenStream> = plan
        .fields
        .iter()
        .enumerate()
        .map(|(idx, kind)| emit_projection_field_read(idx, kind))
        .collect();
    let field_names: Vec<_> = (0..plan.fields.len())
        .map(|idx| format_ident!("field_{}", idx))
        .collect();

    let return_ty: TokenStream = if plan.has_cursor_fields {
        quote! { #struct_ident<'p> }
    } else {
        quote! { #struct_ident }
    };

    // Detect the need for a tape bytes read. Any `Scalar` field whose
    // type is NOT `Span` drives a packed-byte read from the tape
    // arena (via `output`, not through a cursor). `Span`
    // fields + `CursorChild` fields never require the aggregate
    // buffer — Span reads from the frame's own span slots; cursor
    // reads walk the value children.
    let needs_tape_bytes = plan.fields.iter().any(|k| {
        matches!(
            k,
            ProjectionFieldKind::Scalar { ty, .. } if !matches!(ty, TypeDesc::Span)
        )
    });

    let bytes_read: TokenStream = if !needs_tape_bytes || total_bytes == 0 {
        quote! {
            let __bytes: &[u8] = &[];
            let _ = __bytes;
        }
    } else {
        quote! {
            let __tape = output;
            let __tape_rec = __tape
                .try_get(::tape::TapeOffset(offset))?;
            let __bytes = __tape.payload_bytes(__tape_rec, #total_bytes_lit)?;
        }
    };

    let children_read: TokenStream = if plan.has_cursor_fields {
        quote! {
            // Rich admission with cursor-child fields — walk the
            // value slab's direct-child iterator so per-field reads
            // can index into the collected slice by `child_idx`.
            let __children: ::std::vec::Vec<(u32, &::tape::ValueFrame)> =
                output.children(offset).collect();
        }
    } else {
        quote! {}
    };

    // Frame binding — every admission needs the compound's own frame
    // for Span field decoding + variant_idx sanity.
    let frame_read: TokenStream = quote! {
        let frame = output.frame(offset)?;
    };

    quote! {
        /// AY-II.W0'.b — grammar-derived direct-to-struct projection
        /// helper. Reads the admitted rule's frame from the
        /// fused-pipeline [`Tape<R>`](::tape::Tape)
        /// slab and constructs the matching projection struct;
        /// returns `None` when the slab's frame is absent or the
        /// tape's aggregate buffer is too short.
        ///
        /// Routed from `project_frame_<Grammar>` per admission.
        /// `#[inline]` so LLVM folds the body into the dispatcher at
        /// monomorphisation time. Emitted 1:1 per
        /// [`PROJECTION_DIRECT_TO_STRUCT`] entry — post-AY-II.W0'.b
        /// totality is admission : materialiser : consumer at
        /// 1:1:1 per grammar with runtime call-count truth.
        #[inline]
        #[doc(hidden)]
        pub fn #fn_ident<'p>(
            output: &::tape::Tape<#grammar_ident>,
            input: &'p str,
            offset: u32,
        ) -> ::core::option::Option<#return_ty> {
            let _ = input;
            #frame_read
            #bytes_read
            #children_read
            #(#field_inits)*
            ::core::option::Option::Some(#struct_ident {
                #(#field_names),*
            })
        }
    }
}

/// AY-II.W0'.b — emit one field-decode block for a projection helper.
///
/// - Span scalar → `(frame.span_lo, frame.span_hi)`.
/// - Non-Span scalar → `from_le_bytes` at the admitted byte offset of
///   the tape's aggregate buffer.
/// - Cursor child → not supported on the slab-read path yet; panics
///   at runtime (the current admission corpus does not exercise this
///   kind — resolver-backed rich admissions like CSS `colorFn` are
///   scalar-only tuples).
fn emit_projection_field_read(idx: usize, kind: &ProjectionFieldKind) -> TokenStream {
    let field_ident = format_ident!("field_{}", idx);
    let (ty, offset) = match kind {
        ProjectionFieldKind::Scalar { ty, offset } => (ty, *offset as usize),
        ProjectionFieldKind::CursorChild { child_idx, .. } => {
            // W0'.b does not surface a NodeView from the slab; the
            // admission's cursor-child path requires
            // `<Grammar>NodeView` reconstruction from the tape, which
            // is scope for a later wave. The panic preserves the "no
            // fallback" contract — the admission corpus currently does
            // not land any CursorChild-bearing admission (every
            // existing admission either packs scalars or reads a
            // Span from the frame), so this arm is unreachable under
            // the landed grammars.
            let child_idx_lit = proc_macro2::Literal::usize_unsuffixed(*child_idx);
            let idx_lit = proc_macro2::Literal::usize_unsuffixed(idx);
            return quote! {
                let #field_ident = ::core::panic!(
                    "AY-II.W0'.b: CursorChild fields not yet supported by \
                     slab-read materializer; field {} at child idx {}",
                    #idx_lit,
                    #child_idx_lit,
                );
            };
        }
    };
    let offset_lit = proc_macro2::Literal::usize_unsuffixed(offset);
    match ty {
        TypeDesc::Span => {
            // Span fields surface from the admission's OWN frame span
            // slots. No tape access; no aggregate byte decode. The
            // grammar-derived offset is irrelevant for Span — the
            // frame's `(span_lo, span_hi)` IS the value the layout
            // pass would have packed.
            let _ = offset_lit;
            quote! {
                let #field_ident: (u32, u32) = (frame.span_lo, frame.span_hi);
            }
        }
        TypeDesc::Bool => {
            let end_lit = proc_macro2::Literal::usize_unsuffixed(offset + 1);
            quote! {
                let #field_ident: bool = {
                    let __b = *__bytes.get(#offset_lit)?;
                    let _ = #end_lit;
                    __b != 0
                };
            }
        }
        TypeDesc::I8 | TypeDesc::U8 => {
            let ty_tokens = projection_field_primitive(ty);
            let end_lit = proc_macro2::Literal::usize_unsuffixed(offset + 1);
            quote! {
                let #field_ident: #ty_tokens = {
                    let __b = *__bytes.get(#offset_lit)?;
                    let _ = #end_lit;
                    __b as #ty_tokens
                };
            }
        }
        TypeDesc::I16 | TypeDesc::U16 => {
            let ty_tokens = projection_field_primitive(ty);
            let end_lit = proc_macro2::Literal::usize_unsuffixed(offset + 2);
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
            let ty_tokens = projection_field_primitive(ty);
            let end_lit = proc_macro2::Literal::usize_unsuffixed(offset + 4);
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
            let ty_tokens = projection_field_primitive(ty);
            let end_lit = proc_macro2::Literal::usize_unsuffixed(offset + 8);
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
            let end_lit = proc_macro2::Literal::usize_unsuffixed(offset + 8);
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
            panic!(
                "AY-II.W0'.b: payload-layout field must be a scalar payload; \
                 found {other:?} at offset {offset}",
            );
        }
    }
}

/// Map a scalar `TypeDesc` to its primitive Rust type identifier for
/// field-read codegen. Span is handled separately in
/// [`emit_projection_field_read`].
fn projection_field_primitive(ty: &TypeDesc) -> TokenStream {
    let ident = ty.rust_ident().expect(
        "AY-II.W0'.b: projection field primitive type must map via \
             TypeDesc::rust_ident",
    );
    let ty_ident = format_ident!("{}", ident);
    quote! { #ty_ident }
}

/// AY-II.W0'.b — upper-camel-case a name. Mirrors the helper in
/// `emitter/grammar.rs`; duplicated locally so this emitter stays
/// self-contained.
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

/// AY-II.W0'.b — sanitise a rule name into a lowercase Rust ident slug.
/// Mirrors the helper in `emitter/grammar.rs`.
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
