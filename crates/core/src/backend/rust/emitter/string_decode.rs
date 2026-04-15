//! String-decode payload emission for the Rust backend (AU.3.1).
//!
//! When a grammar rule carries
//! `-> decode_json_string_to_arena(input) : String`, this helper
//! emits a scan + tape push that lands the decoded UTF-8 into the
//! tape's unified arena via
//! [`bbnf_tape::TapeBuilder::push_leaf_with_arena_frame`] (AU.6.7).
//! The prior AU.3.1 `push_leaf_with_string` signature collapsed into
//! the unified-arena entry point; the decode kernel still streams
//! bytes into the arena in-place (zero intermediate allocation) and
//! commits the record once the length prefix has been back-stamped.
//!
//! # Placement in the emission pipeline
//!
//! The string-decode rule (e.g. JSON's `string`) is always resolved
//! by the pipeline to [`CallStrategy::InlineBody`] — no standalone
//! `__string` function is emitted, every call site inlines the
//! scan in place. That means the scan emitted here always runs
//! inside an enclosing rule's body (e.g. `__pair`, `__value`), not
//! as the body of a dedicated function.
//!
//! To make the decoded leaf observable via `.view().cursor()` the
//! scan MUST become a child of the enclosing rule's compound
//! record. That is what the existing `object` / `array` branches of
//! `__value` do: they `mark_children` before the sub-parse and the
//! rule epilogue pushes a `push_compound` wrapping whatever records
//! land in between. The string-decode scan here follows the same
//! protocol:
//!
//! 1. Call `mark_children` and write the returned `TapeOffset` into
//!    the outer rule's `__children` local, setting `__has_children
//!    = true` so the epilogue routes through the compound push path.
//! 2. Decode the JSON string into the arena.
//! 3. Push the arena-frame leaf via
//!    `push_leaf_with_arena_frame` — now a child of the
//!    about-to-be-pushed compound.
//! 4. Return `Some(())` so the enclosing `Seq` / `Alt` continues.
//!
//! The enclosing rule's epilogue then wraps the just-pushed string
//! leaf in a `Rule`/`Span` compound. `.view().cursor().children()`
//! returns an iterator yielding the string leaf, letting view-layer
//! code read the decoded UTF-8 via `payload_string`.
//!
//! When agent B's `TypeDesc::String` whitelist entry + aggregate-
//! prelude extension lands, this helper collapses into the standard
//! scalar prelude/epilogue path; the scan simply populates a
//! `__payload_string_{offset,len}` local and the epilogue owns the
//! push. Today's inline-push-with-mark-children form is the minimal
//! correct shape given that detour.
//!
//! The `mark_children` / `__has_children` write is gated on the
//! outer rule being an Alt-bodied MustTape rule (tape surgery
//! enabled — `ctx.tape_surgery.is_some()`). This is JSON's `value`
//! rule, where `string` is one of the Alt branches. For Seq-bodied
//! outer rules like `pair = string, colon >> value`, the prelude
//! already called `mark_children`, so the scan skips the redundant
//! mark and just pushes the leaf.
//!
//! [`CallStrategy::InlineBody`]: crate::backend::CallStrategy::InlineBody

use proc_macro2::TokenStream;
use quote::quote;

/// Emit the decode-kernel call site for a JSON quoted string.
///
/// Emits an expression of type `Option<()>` that, on success, also
/// appends a decoded-string leaf record via
/// `push_leaf_with_arena_frame` to the tape.
///
/// `variant_idx` is the owning rule's codegen discriminator
/// (`rule.id & 0xFF`), resolved at codegen time via the
/// `RustEmitCtx::string_decode_variant_idx` precomputed map.
///
/// `needs_mark_children` is true when the enclosing rule is an
/// Alt-bodied MustTape rule (AM.3 tape surgery — `__children` and
/// `__has_children` are mutable locals declared in the prelude and
/// branch arms selectively set them). In that case the scan prepends
/// a `mark_children` + `__has_children = true` write so the epilogue
/// picks the compound-push path and wraps the string leaf.
///
/// When `needs_mark_children` is false, the enclosing rule is
/// either Seq-bodied MustTape (already marked its children run in
/// the prelude) or a span-only rule that doesn't need a wrapping
/// compound; we skip the mark and just push the leaf.
pub fn emit_decode_call(variant_idx: u8, needs_mark_children: bool) -> TokenStream {
    let variant_lit = variant_idx;
    let mark_children = if needs_mark_children {
        quote! {
            __children =
                ::bbnf::runtime::tape::TapeBuilder::mark_children(tape);
            __has_children = true;
        }
    } else {
        quote! {}
    };
    quote! {
        {
            let __start = state.offset;
            #mark_children
            // Reserve the standard `(len: u32 LE, bytes)` arena frame
            // up front. The decode kernel's `Borrowed` fast path
            // (no `\` byte in the source span) wrote no bytes after
            // the prefix; we discard the prefix slot and push a
            // borrow-flagged leaf instead, so the reader slices
            // `source[span_lo + 1 .. span_hi - 1]` with zero arena
            // cost. `Owned` keeps the prefix and back-stamps the
            // length once the kernel returns.
            let __prefix_offset: u32 =
                ::bbnf::runtime::tape::TapeBuilder::arena_len(&*tape);
            {
                let __arena = ::bbnf::runtime::tape::TapeBuilder::arena_mut(tape);
                __arena.extend_from_slice(&0u32.to_le_bytes());
            }
            let __decode_result = {
                let __src = state.src_bytes;
                let __arena = ::bbnf::runtime::tape::TapeBuilder::arena_mut(tape);
                ::parse_that::parsers::scan::decode_json_string_to_arena(
                    __src, __start, __arena,
                )
            };
            match __decode_result {
                Some((__payload, __end)) => {
                    let __leaf_span_lo: u32 = __start as u32;
                    state.offset = __end;
                    match __payload {
                        ::parse_that::parsers::scan::StringPayload::Borrowed { .. } => {
                            // Decode kernel wrote nothing past the
                            // prefix slot — drop the prefix and push a
                            // borrow-flagged leaf. The reader slices
                            // source[span_lo + 1 .. span_hi - 1].
                            let __arena =
                                ::bbnf::runtime::tape::TapeBuilder::arena_mut(tape);
                            __arena.truncate(__prefix_offset as usize);
                            let _ = ::bbnf::runtime::tape::TapeBuilder::push_leaf_borrowed_string(
                                tape,
                                ::bbnf::runtime::tape::TapeKind::Span,
                                __leaf_span_lo,
                                state.offset as u32,
                                #variant_lit,
                                0u8,   // meta_idx
                            );
                        }
                        ::parse_that::parsers::scan::StringPayload::Owned {
                            len: __ol,
                            ..
                        } => {
                            // Owned bytes are appended right after our
                            // reserved prefix. Stamp the length and
                            // commit a standard arena-frame leaf.
                            ::bbnf::runtime::tape::TapeBuilder::stamp_arena_len_prefix(
                                tape,
                                __prefix_offset,
                                __ol,
                            );
                            let _ = ::bbnf::runtime::tape::TapeBuilder::push_leaf_with_arena_frame(
                                tape,
                                ::bbnf::runtime::tape::TapeKind::Span,
                                __leaf_span_lo,
                                state.offset as u32,
                                #variant_lit,
                                0u8,   // meta_idx
                                __prefix_offset,
                            );
                        }
                    }
                    Some(())
                }
                None => {
                    // Roll back the 4-byte prefix reservation so
                    // the arena stays compact on failure.
                    let __arena =
                        ::bbnf::runtime::tape::TapeBuilder::arena_mut(tape);
                    __arena.truncate(__prefix_offset as usize);
                    None
                }
            }
        }
    }
}
