//! Leaf-op emission for the Rust backend: literals, regex, epsilon.
//!
//! Under Tranche AC.2 tape-first, leaves emit side-effecting token
//! streams of type `Option<()>`. A successful match advances
//! `state.offset`; the tape is untouched — the enclosing rule's
//! prelude / epilogue owns the `push_leaf` / `push_compound` call.
//!
//! Each method is `pub(super)` so the trait impl in `mod.rs` can
//! delegate to it via `self.emit_xxx_impl(...)`.

use bbnf_ir::{GrammarIR, TypeDesc};
use proc_macro2::TokenStream;
use quote::quote;

use super::{RustEmitCtx, RustEmitter};

impl RustEmitter {
    pub(super) fn emit_literal_match_impl(
        &mut self,
        value: &str,
        guaranteed_byte: Option<u8>,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let bytes = value.as_bytes();

        // AV.0.2: bare-`Span` aggregate layout — pack (__span_lo,
        // state.offset) into the 8-byte layout field on literal match
        // success. Mirrors the F64 aggregate arm in
        // `emit_regex_match_impl`: probe the next aggregate field, and
        // if it is a `Span`, wrap the literal match with the pack.
        // `__span_lo` (prelude local) carries the rule's start offset;
        // `state.offset` at the pack site is the post-match end. For
        // single-leaf bodies (e.g. BBNF `identifier = /regex/ -> Span`)
        // this captures the full rule span; for multi-leaf bodies (e.g.
        // BBNF `literal = ( "\"" , /…/ , "\"" ) -> Span`) the per-leaf
        // hi is partial — the aggregate epilogue's Span fixup in
        // `emit_tape_span_only_aggregate_epilogue` /
        // `emit_must_tape_aggregate_epilogue` rewrites the buffer with
        // the rule-final `state.offset` before the `push_leaf_with`
        // call, so the on-tape Span is always correct.
        let span_pack = probe_span_aggregate_pack(ctx);

        if guaranteed_byte.is_some() {
            // Dispatch already proved this byte — just advance.
            if let Some(pack) = span_pack {
                return quote! {
                    {
                        state.offset += 1;
                        #pack
                        Some(())
                    }
                };
            }
            return quote! {
                {
                    state.offset += 1;
                    Some(())
                }
            };
        }

        if bytes.len() == 1 {
            let byte = bytes[0];
            if let Some(pack) = span_pack {
                return quote! {
                    {
                        if state.offset < state.src_bytes.len()
                            && state.src_bytes[state.offset] == #byte
                        {
                            state.offset += 1;
                            #pack
                            Some(())
                        } else {
                            None
                        }
                    }
                };
            }
            quote! {
                {
                    if state.offset < state.src_bytes.len()
                        && state.src_bytes[state.offset] == #byte
                    {
                        state.offset += 1;
                        Some(())
                    } else {
                        None
                    }
                }
            }
        } else {
            // Byte-array literal equality: load N bytes as `[u8; N]`
            // and compare with `*b"..."`. LLVM lowers this to a
            // single iN load + icmp for N in {2,4,8} and a
            // half-word + tail-byte combo for N in {3,5,6,7},
            // never invoking memcmp.
            let len = bytes.len();
            let lit = proc_macro2::Literal::byte_string(bytes);
            if let Some(pack) = span_pack {
                return quote! {
                    {
                        if state.offset + #len <= state.src_bytes.len()
                            && unsafe {
                                *(state.src_bytes.as_ptr().add(state.offset)
                                    as *const [u8; #len])
                            } == *#lit
                        {
                            state.offset += #len;
                            #pack
                            Some(())
                        } else {
                            None
                        }
                    }
                };
            }
            quote! {
                {
                    if state.offset + #len <= state.src_bytes.len()
                        && unsafe {
                            *(state.src_bytes.as_ptr().add(state.offset)
                                as *const [u8; #len])
                        } == *#lit
                    {
                        state.offset += #len;
                        Some(())
                    } else {
                        None
                    }
                }
            }
        }
    }

    pub(super) fn emit_regex_match_impl(
        &mut self,
        pattern: &str,
        _regex_id: usize,
        ir: &GrammarIR,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Build EmitOpts once up front so every classify_regex call
        // hits the ir.regex_info cache instead of re-parsing the HIR.
        let ws_pat = ir.ws_pattern.map(|sid| ir.get_string(sid));
        let opts =
            crate::generate::regex::EmitOpts::new(&crate::generate::regex::CostModel::DEFAULT)
                .with_fuse(!self.effective_prettify)
                .with_ir(ir)
                .with_ws_pattern(ws_pat);

        // AU.3.1: detection via the precomputed string-decode
        // pattern set on the ctx. This set is populated once at
        // ctx creation by walking the grammar IR for rules whose
        // body matches `Map { Regex(sid), FnDescriptor::Expr {
        // FnCall("decode_json_string_to_arena", [Input]), .. } }`.
        // Using a structural pattern-set beats chasing the
        // `ctx.current_rule_id` / `ctx.payload_types` signals:
        // those reflect the enclosing compile context, not the
        // rule that originally owned the regex, and break when
        // the string rule is inlined into `pair` / `value`.
        //
        // The decode check runs BEFORE the aggregate-layout / Span
        // probes below: a rule annotated with both `-> Span` and
        // `-> decode_json_string_to_arena(input)` (JSON `string`'s
        // historical shape) routes through the decode kernel — the
        // aggregate buffer is a dead stub that the enclosing rule's
        // epilogue ignores when `__has_payload` stays false after
        // the decode call's inline push. Probing aggregate fields
        // first would hijack the decode path and drop the UTF-8
        // payload.
        if let Some(sid) = ir.find_string_id(pattern) {
            if ctx.is_string_decode_pattern(sid) {
                let variant_idx = ctx.string_decode_variant_idx(sid);
                // `outer_tape_surgery_active` peeks at both
                // `ctx.tape_surgery` (set for the enclosing rule
                // itself when we're directly in a MustTape Alt
                // body) and the saved alt-context stack (set when
                // we're deeper, e.g. inside an Alt branch body whose
                // outer rule is MustTape). Either case requires the
                // decode scan to `mark_children` + set
                // `__has_children = true` so the rule's epilogue
                // pushes a compound wrapping the string leaf.
                let needs_mark_children = ctx.outer_tape_surgery_active();
                return crate::backend::rust::emitter::string_decode::emit_decode_call(
                    variant_idx,
                    needs_mark_children,
                );
            }
        }

        // AQ.6.B / AV.0.2: when an aggregate layout is active,
        // probe the next aggregate field and emit the field-specific
        // capture. The F64 arm drives the number scanner (JSON-strict
        // vs CSS-permissive per `allow_leading_dot`); the Span arm
        // packs `(__span_lo, state.offset)` into the 8-byte layout
        // slot on regex-match success without requiring a typed
        // scanner return value.
        if ctx.payload_layout.is_some() {
            use parse_that::regex::classify::RegexClass;
            let classified = opts.classify_regex(pattern);
            if let RegexClass::Numeric { allow_leading_dot, .. } = classified {
                if let Some(field) = ctx.next_aggregate_field() {
                    if matches!(field.ty, TypeDesc::F64) {
                        let offset = field.offset as usize;
                        let end = offset + 8;
                        let scan = number_scan_fn(allow_leading_dot);
                        return quote! {
                            match #scan(state) {
                                Some(__v) => {
                                    __aggregate_buf[#offset..#end]
                                        .copy_from_slice(&__v.to_le_bytes());
                                    __has_payload = true;
                                    Some(())
                                }
                                None => None,
                            }
                        };
                    }
                    // Non-F64 field at this cursor position — keep the
                    // field consumed but fall through to the non-
                    // aggregate emission path. Probing and advancing
                    // the cursor is idempotent across non-matching
                    // field types; emitters downstream see the
                    // advanced cursor and match the next field.
                }
            } else if let Some(pack) = probe_span_aggregate_pack(ctx) {
                // AV.0.2: bare-`Span` layout, non-numeric regex body —
                // wrap the regex match with the Span pack. The shared
                // `emit_regex` call below returns `Option<Span>`; we
                // discard the Span value (we already have the equivalent
                // `(lo, hi) = (__span_lo, state.offset)` in scope) and
                // fold the pack + `Some(())` into the match's success
                // arm.
                let regex_expr = crate::generate::regex::emit_regex(pattern, &opts);
                return quote! {
                    match ({ #regex_expr }) {
                        Some(_) => {
                            #pack
                            Some(())
                        }
                        None => None,
                    }
                };
            }
        }

        // AQ.6.A: for number patterns with F64 payload active, emit
        // the number scanner to capture the parsed value into the
        // typed payload variable. IR passes strip
        // `Map { NumberConvert }` down to bare `Regex`, so we detect
        // the number shape here via `RegexClass::Numeric` and route
        // through the scanner whose leading-dot policy matches the
        // regex classification: strict JSON rejects `.5`, CSS-
        // permissive accepts it.
        if ctx.has_payload_type(&TypeDesc::F64) {
            use parse_that::regex::classify::RegexClass;
            if let RegexClass::Numeric { allow_leading_dot, .. } = opts.classify_regex(pattern) {
                let tag_set = ctx.payload_tag(&TypeDesc::F64).map(|tag| {
                    quote! { __payload_tag = #tag; }
                });
                let scan = number_scan_fn(allow_leading_dot);
                return quote! {
                    match #scan(state) {
                        Some(__v) => { __payload_f64 = __v; #tag_set __has_payload = true; Some(()) }
                        None => None,
                    }
                };
            }
        }

        // Default: span-only scan. The shared regex emitter returns
        // `Option<Span>`; AU.6.5 no-value-discard drops the prior
        // `.map(|_| ())` wrap and returns `Option<Span>` directly —
        // the Span composes identically via `Some(_)` at every call
        // site, so one less Option epilogue per regex match in the
        // inner loop.
        let regex_expr = crate::generate::regex::emit_regex(pattern, &opts);
        quote! {
            { #regex_expr }
        }
    }

    pub(super) fn emit_epsilon_impl(&mut self, _ctx: &mut RustEmitCtx) -> TokenStream {
        // Epsilon: matches without advancing, no tape side effect.
        quote! { Some(()) }
    }

    pub(super) fn emit_seq_all_span_impl(
        &mut self,
        child_outputs: Vec<TokenStream>,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let span_blk = ctx.fresh_lifetime("span_blk");
        // All children are side-effecting `Option<()>`. Chain them
        // together under a labeled block for short-circuit failure.
        let child_checks: Vec<TokenStream> = child_outputs
            .into_iter()
            .map(|c| quote! {
                match (#c) {
                    Some(_) => (),
                    None => break #span_blk None,
                }
            })
            .collect();
        quote! {
            #span_blk: {
                #( #child_checks )*
                Some(())
            }
        }
    }
}

/// Map a numeric regex's `allow_leading_dot` flag onto the matching
/// parse-that scanner. JSON-strict (`allow_leading_dot: false`) routes
/// through `scan_number_strict_f64`; CSS-permissive and any numeric
/// class that admits bare `.5` routes through `scan_number_f64`
/// (generic `GENERIC_NUMBER_CONFIG`). The emitter chooses at codegen
/// time from the regex classification — no runtime branching.
pub(super) fn number_scan_fn(allow_leading_dot: bool) -> TokenStream {
    if allow_leading_dot {
        quote! { ::parse_that::scan_number_f64 }
    } else {
        quote! { ::parse_that::scan_number_strict_f64 }
    }
}

/// AV.0.2: probe `ctx` for a bare-`Span` aggregate field. When the
/// active payload layout's next field is a `TypeDesc::Span`, advance
/// the cursor and return the pack token stream that writes
/// `(__span_lo, state.offset)` into the layout's 8-byte slot.
///
/// The returned stream is inline — callers splice it into the
/// success arm of their literal / regex match (`#pack` between the
/// `state.offset` advance and the trailing `Some(())`). `__span_lo`
/// is the rule prelude's captured start offset; `state.offset` at
/// pack time is the post-match cursor, so for single-leaf bodies
/// (e.g. BBNF `identifier = /regex/ -> Span`) the full rule span
/// lands in the buffer. For multi-leaf bodies (e.g. BBNF `literal`
/// whose body is an Alt of Seq-of-leaves) the first leaf's pack
/// writes a premature `hi`; the rule's aggregate epilogue rewrites
/// the `hi` half of the buffer with the final `state.offset` before
/// the `push_leaf_with` call, so the on-tape Span is always correct.
///
/// Returns `None` when the cursor is past the layout's fields or
/// the current field is not `TypeDesc::Span` — callers then fall
/// through to their non-aggregate emission.
///
/// The cursor is only advanced when the field is actually a Span
/// (and the pack emits). Non-Span fields stay available to
/// downstream emitters (HexConvert, NumberConvert, map-constant
/// setters) that otherwise consume them in their own
/// `ctx.next_aggregate_field()` call. This preserves the kv-pair
/// U32 capture in hex colour rules and the F64 capture in numeric
/// aggregates even when a leading literal leaf emits first.
pub(super) fn probe_span_aggregate_pack(ctx: &mut RustEmitCtx) -> Option<TokenStream> {
    let layout = ctx.payload_layout.as_ref()?;
    let field = layout.fields.get(ctx.aggregate_field_cursor)?.clone();
    if !matches!(field.ty, TypeDesc::Span) {
        // Peek-only: leave the cursor for downstream emitters.
        return None;
    }
    // AW.0.3: bare-`Span` layouts (single Span field at offset 0,
    // total_bytes 8) route unconditionally through the epilogue's
    // `bare_span_epilogue_fixup`, which rewrites the buffer with
    // the rule-final `state.offset` and stamps `__has_payload =
    // true` itself. The leaf-level pack here would double-write
    // the first 8 bytes and reference a `__has_payload` local the
    // elided prelude no longer declares. Skip it.
    if layout.total_bytes == 8
        && layout.fields.len() == 1
        && field.offset == 0
    {
        // Cursor was never advanced — leave it so downstream
        // emitters see a consistent view (no fields consumed).
        return None;
    }
    // Commit the cursor advance now that we know the field is ours.
    ctx.aggregate_field_cursor += 1;
    let lo_off = field.offset as usize;
    let hi_off = lo_off + 4;
    Some(quote! {
        __aggregate_buf[#lo_off..#lo_off + 4]
            .copy_from_slice(&(__span_lo as u32).to_le_bytes());
        __aggregate_buf[#hi_off..#hi_off + 4]
            .copy_from_slice(&(state.offset as u32).to_le_bytes());
        __has_payload = true;
    })
}
