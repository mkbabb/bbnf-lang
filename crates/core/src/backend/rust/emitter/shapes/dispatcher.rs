//! Dispatcher emitter — top-level `parse_<grammar>_<root>` entry
//! point + the per-grammar shape support module (SIMD whitespace
//! cache, first-byte dispatch helpers).
//!
//! # Role — AW-V.W3.2
//!
//! The dispatcher mirrors the prototype's
//! `bbnf_json_prototype::parse_json` shape: skip initial whitespace,
//! dispatch on the first byte to the appropriate shape function,
//! verify trailing whitespace. Per-rule recursion threads through the
//! dispatcher (e.g. object's value-position reads dispatch back
//! through the shape dispatcher to land on number / string / bool /
//! null / nested object / array arms).
//!
//! The support module emits per-grammar SIMD scaffolding —
//! `ScanState` (64-byte whitespace bitmap cache, mirroring
//! `bbnf-json-prototype`'s `src/simd.rs::ScanState`), `skip_space`,
//! `first_quote_or_backslash`, and friends. Emitting these per-grammar
//! (rather than referencing a cross-crate symbol) keeps the hot path
//! free of function-call boundaries — every SIMD helper resolves at
//! link time into `parse_<grammar>_<root>`.

use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
use bbnf_ir::GrammarIR;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::{has_shape_dispatch, root_rule_name, sanitise_grammar};

/// Compose the dispatcher symbol for a (grammar, root-rule) pair —
/// `parse_<grammar>_<root>`. The emitted `parse()` in
/// `emit_grammar_impl` routes to this ident when the grammar has
/// full shape coverage.
pub fn dispatcher_fn_ident(grammar_ident_str: &str, root_rule: &str) -> proc_macro2::Ident {
    let grammar = sanitise_grammar(grammar_ident_str);
    let root = sanitise_grammar(root_rule);
    format_ident!("parse_{}_{}", grammar, root)
}

/// Compose the per-shape fn ident for a rule — `parse_<shape>_<grammar>_<rule>`.
pub fn shape_fn_ident(shape: &str, grammar_suffix: &str, rule_name: &str) -> proc_macro2::Ident {
    let rule = sanitise_grammar(rule_name);
    format_ident!("parse_{}_{}_{}", shape, grammar_suffix, rule)
}

/// Compose the per-shape visitor-path fn ident for a rule —
/// `parse_<shape>_visitor_<grammar>_<rule>`.
///
/// The visitor-path family mirrors the tape-path per-shape functions
/// but takes `&mut V: JsonVisitor` instead of `&mut TapeBuilder`,
/// emitting visitor method calls (`begin_object`, `key`, `string`,
/// `number_f64`, etc.) in place of tape record pushes. Monomorphised
/// per visitor at the call site; zero structural / PSI overhead on
/// the hot path. Matches `bbnf-json-prototype`'s perf shape.
pub fn visitor_shape_fn_ident(
    shape: &str,
    grammar_suffix: &str,
    rule_name: &str,
) -> proc_macro2::Ident {
    let rule = sanitise_grammar(rule_name);
    format_ident!("parse_{}_visitor_{}_{}", shape, grammar_suffix, rule)
}

/// Compose the visitor-path dispatcher symbol —
/// `parse_<grammar>_<root>_visitor`.
pub fn visitor_dispatcher_fn_ident(
    grammar_ident_str: &str,
    root_rule: &str,
) -> proc_macro2::Ident {
    let grammar = sanitise_grammar(grammar_ident_str);
    let root = sanitise_grammar(root_rule);
    format_ident!("parse_{}_{}_visitor", grammar, root)
}

/// Per-grammar SIMD support module. Emitted once per grammar with
/// shape dispatch. Contains the `ScanState`, `skip_space`, and SIMD
/// primitives every shape fn inlines.
///
/// The module is emitted at the same scope as the per-shape
/// functions so they can share `ScanState` by reference. Naming:
/// `__shape_support_<grammar>` — the grammar suffix prevents
/// collisions when multiple grammars coexist in one compilation.
pub fn emit_support_module(grammar_suffix: &str) -> TokenStream {
    let mod_ident = format_ident!("__shape_support_{}", grammar_suffix);
    quote! {
        /// AW-V.W3.2 — per-grammar shape-dispatch support.
        ///
        /// Inlined by every `parse_<shape>_<grammar>_<rule>` emitted
        /// sibling; carries the SIMD whitespace bitmap cache + the
        /// quoted-string scanner primitive. The module is private to
        /// the generated code — downstream consumers route through the
        /// top-level `parse_<grammar>_<root>` which inlines every
        /// helper under workspace LTO.
        #[allow(dead_code, non_snake_case)]
        pub(crate) mod #mod_ident {
            /// Per-parse SIMD scratch — 64-byte whitespace-bitmap
            /// cache mirroring `bbnf-json-prototype::simd::ScanState`.
            #[derive(Debug, Default)]
            pub struct ScanState {
                pub(crate) nospace_bits: u64,
                pub(crate) nospace_start: isize,
            }

            impl ScanState {
                #[inline]
                pub fn new() -> Self {
                    Self { nospace_bits: 0, nospace_start: -1 }
                }
            }

            /// Skip JSON whitespace at `*p`, returning the first
            /// non-whitespace byte (or `None` on EOF). Hot-path fast-
            /// exit when the next byte is non-whitespace.
            #[inline(always)]
            pub fn skip_space(
                input: &[u8],
                p: &mut usize,
                state: &mut ScanState,
            ) -> Option<u8> {
                // Direct boolean form rather than `matches!`: nightly's
                // `matches!` expansion decorates the inner `match` with
                // `#[allow(non_exhaustive_omitted_patterns)]` — an
                // attribute on an expression (unstable, E0658) —
                // surfaced by the bootstrap's `cargo expand` step.
                match input.get(*p) {
                    Some(&b) if b != b' ' && b != b'\t' && b != b'\n' && b != b'\r' => Some(b),
                    None => None,
                    _ => {
                        skip_space_slow(input, p, state);
                        input.get(*p).copied()
                    }
                }
            }

            #[inline(always)]
            pub(crate) fn skip_space_slow(
                input: &[u8],
                p: &mut usize,
                state: &mut ScanState,
            ) {
                loop {
                    let cache_base = state.nospace_start;
                    if cache_base >= 0 && (*p as isize) >= cache_base {
                        let rel = *p - cache_base as usize;
                        if rel < 64 {
                            let masked = state.nospace_bits & (!0u64 << rel);
                            if masked != 0 {
                                let bit = masked.trailing_zeros() as usize;
                                *p = cache_base as usize + bit;
                                return;
                            }
                            *p = cache_base as usize + 64;
                        }
                    }
                    if *p + 64 > input.len() {
                        while let Some(&b) = input.get(*p) {
                            // Direct boolean form (see `skip_space`).
                            if b != b' ' && b != b'\t' && b != b'\n' && b != b'\r' {
                                return;
                            }
                            *p += 1;
                        }
                        return;
                    }
                    let stripe = unsafe {
                        ::core::slice::from_raw_parts(input.as_ptr().add(*p), 64)
                    };
                    let mask = nospace_bitmap_64(stripe);
                    state.nospace_bits = mask;
                    state.nospace_start = *p as isize;
                    if mask != 0 {
                        let bit = mask.trailing_zeros() as usize;
                        *p += bit;
                        return;
                    }
                    *p += 64;
                }
            }

            /// Compute the 64-bit "non-whitespace" bitmap for a 64-byte
            /// stripe. Bit `i` is `1` iff `stripe[i]` is NOT in
            /// `{b' ', b'\t', b'\n', b'\r'}`.
            #[inline(always)]
            pub(crate) fn nospace_bitmap_64(stripe: &[u8]) -> u64 {
                #[cfg(target_arch = "aarch64")]
                unsafe { return nospace_bitmap_64_neon(stripe); }
                #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
                unsafe { return nospace_bitmap_64_avx2(stripe); }
                #[allow(unreachable_code)]
                nospace_bitmap_64_scalar(stripe)
            }

            #[cfg(target_arch = "aarch64")]
            #[inline(always)]
            unsafe fn nospace_bitmap_64_neon(stripe: &[u8]) -> u64 {
                use core::arch::aarch64::*;
                unsafe {
                    let ptr = stripe.as_ptr();
                    let space = vdupq_n_u8(b' ');
                    let tab = vdupq_n_u8(b'\t');
                    let nl = vdupq_n_u8(b'\n');
                    let cr = vdupq_n_u8(b'\r');
                    let bits_lo: [u8; 16] =
                        [1, 2, 4, 8, 16, 32, 64, 128, 1, 2, 4, 8, 16, 32, 64, 128];
                    let bit_vec = vld1q_u8(bits_lo.as_ptr());
                    let m0 = chunk_ns_mask16(ptr, 0, space, tab, nl, cr, bit_vec);
                    let m1 = chunk_ns_mask16(ptr, 16, space, tab, nl, cr, bit_vec);
                    let m2 = chunk_ns_mask16(ptr, 32, space, tab, nl, cr, bit_vec);
                    let m3 = chunk_ns_mask16(ptr, 48, space, tab, nl, cr, bit_vec);
                    (m0 as u64) | ((m1 as u64) << 16)
                        | ((m2 as u64) << 32) | ((m3 as u64) << 48)
                }
            }

            #[cfg(target_arch = "aarch64")]
            #[inline(always)]
            unsafe fn chunk_ns_mask16(
                ptr: *const u8,
                off: usize,
                space: core::arch::aarch64::uint8x16_t,
                tab: core::arch::aarch64::uint8x16_t,
                nl: core::arch::aarch64::uint8x16_t,
                cr: core::arch::aarch64::uint8x16_t,
                bit_vec: core::arch::aarch64::uint8x16_t,
            ) -> u16 {
                use core::arch::aarch64::*;
                unsafe {
                    let chunk = vld1q_u8(ptr.add(off));
                    let ws = vorrq_u8(
                        vorrq_u8(vceqq_u8(chunk, space), vceqq_u8(chunk, tab)),
                        vorrq_u8(vceqq_u8(chunk, nl), vceqq_u8(chunk, cr)),
                    );
                    let ns = vmvnq_u8(ws);
                    let weighted = vandq_u8(ns, bit_vec);
                    let low = vaddv_u8(vget_low_u8(weighted)) as u16;
                    let high = vaddv_u8(vget_high_u8(weighted)) as u16;
                    low | (high << 8)
                }
            }

            #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
            #[inline(always)]
            unsafe fn nospace_bitmap_64_avx2(stripe: &[u8]) -> u64 {
                use core::arch::x86_64::*;
                unsafe {
                    let ptr = stripe.as_ptr();
                    let space = _mm256_set1_epi8(b' ' as i8);
                    let tab = _mm256_set1_epi8(b'\t' as i8);
                    let nl = _mm256_set1_epi8(b'\n' as i8);
                    let cr = _mm256_set1_epi8(b'\r' as i8);
                    let mut out = 0u64;
                    for i in 0..2 {
                        let v = _mm256_loadu_si256(ptr.add(i * 32) as *const __m256i);
                        let ws = _mm256_or_si256(
                            _mm256_or_si256(
                                _mm256_cmpeq_epi8(v, space),
                                _mm256_cmpeq_epi8(v, tab),
                            ),
                            _mm256_or_si256(
                                _mm256_cmpeq_epi8(v, nl),
                                _mm256_cmpeq_epi8(v, cr),
                            ),
                        );
                        let ws_mask = _mm256_movemask_epi8(ws) as u32;
                        let ns_mask = !ws_mask as u64;
                        out |= (ns_mask & 0xFFFF_FFFF) << (i * 32);
                    }
                    out
                }
            }

            #[inline(always)]
            pub(crate) fn nospace_bitmap_64_scalar(stripe: &[u8]) -> u64 {
                let mut out = 0u64;
                for (i, &b) in stripe.iter().enumerate() {
                    // Direct boolean form (see `skip_space`).
                    if b != b' ' && b != b'\t' && b != b'\n' && b != b'\r' {
                        out |= 1u64 << i;
                    }
                }
                out
            }

            /// Find the first `b'"'` or `b'\\'` byte in `bytes`.
            /// Mirrors `bbnf-json-prototype::simd::first_quote_or_backslash`.
            #[inline(always)]
            pub fn first_quote_or_backslash(bytes: &[u8]) -> Option<(usize, u8)> {
                #[cfg(target_arch = "aarch64")]
                unsafe { return first_quote_or_backslash_neon(bytes); }
                #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
                unsafe { return first_quote_or_backslash_avx2(bytes); }
                #[allow(unreachable_code)]
                first_quote_or_backslash_scalar(bytes)
            }

            #[cfg(target_arch = "aarch64")]
            #[inline(always)]
            unsafe fn first_quote_or_backslash_neon(
                bytes: &[u8],
            ) -> Option<(usize, u8)> {
                use core::arch::aarch64::*;
                unsafe {
                    let quote = vdupq_n_u8(b'"');
                    let backslash = vdupq_n_u8(b'\\');
                    let ptr = bytes.as_ptr();
                    let len = bytes.len();
                    let mut i = 0usize;
                    while i + 16 <= len {
                        let v = vld1q_u8(ptr.add(i));
                        let hit = vorrq_u8(vceqq_u8(v, quote), vceqq_u8(v, backslash));
                        let packed = vshrn_n_u16::<4>(vreinterpretq_u16_u8(hit));
                        let bits = vget_lane_u64::<0>(vreinterpret_u64_u8(packed));
                        if bits != 0 {
                            let off = (bits.trailing_zeros() >> 2) as usize;
                            let byte = *ptr.add(i + off);
                            return Some((i + off, byte));
                        }
                        i += 16;
                    }
                    while i < len {
                        let b = *ptr.add(i);
                        if b == b'"' || b == b'\\' { return Some((i, b)); }
                        i += 1;
                    }
                    None
                }
            }

            #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
            #[inline(always)]
            unsafe fn first_quote_or_backslash_avx2(
                bytes: &[u8],
            ) -> Option<(usize, u8)> {
                use core::arch::x86_64::*;
                unsafe {
                    let quote = _mm256_set1_epi8(b'"' as i8);
                    let backslash = _mm256_set1_epi8(b'\\' as i8);
                    let ptr = bytes.as_ptr();
                    let len = bytes.len();
                    let mut i = 0usize;
                    while i + 32 <= len {
                        let v = _mm256_loadu_si256(ptr.add(i) as *const __m256i);
                        let hit = _mm256_or_si256(
                            _mm256_cmpeq_epi8(v, quote),
                            _mm256_cmpeq_epi8(v, backslash),
                        );
                        let mask = _mm256_movemask_epi8(hit) as u32;
                        if mask != 0 {
                            let off = mask.trailing_zeros() as usize;
                            return Some((i + off, *ptr.add(i + off)));
                        }
                        i += 32;
                    }
                    while i < len {
                        let b = *ptr.add(i);
                        if b == b'"' || b == b'\\' { return Some((i, b)); }
                        i += 1;
                    }
                    None
                }
            }

            #[inline(always)]
            pub(crate) fn first_quote_or_backslash_scalar(
                bytes: &[u8],
            ) -> Option<(usize, u8)> {
                for (i, &b) in bytes.iter().enumerate() {
                    if b == b'"' || b == b'\\' { return Some((i, b)); }
                }
                None
            }

            /// Map a byte into one of six arms: object `{` → 0,
            /// array `[` → 1, string `"` → 2, digit/`-` (number) → 3,
            /// keyword-led `t` / `f` / `n` → 4, else → 5.
            ///
            /// The emitter's dispatcher inlines this to compile-time
            /// byte matches; kept here as a reference helper for tests.
            #[inline(always)]
            pub(crate) fn shape_byte_arm(b: u8) -> u8 {
                match b {
                    b'{' => 0,
                    b'[' => 1,
                    b'"' => 2,
                    b'-' | b'0'..=b'9' => 3,
                    b't' | b'f' | b'n' => 4,
                    _ => 5,
                }
            }

            /// Expect an exact keyword sequence at `*p` and advance
            /// past it on match.
            #[inline(always)]
            pub fn expect_keyword(
                input: &[u8],
                p: &mut usize,
                word: &[u8],
            ) -> bool {
                let at = *p;
                let end = at + word.len();
                if input.len() < end || &input[at..end] != word {
                    return false;
                }
                *p = end;
                true
            }
        }
    }
}

/// Emit the dispatcher fn — `parse_<grammar>_<root>`.
///
/// Dispatches the next non-whitespace byte to the appropriate
/// shape-specific function. Routes through the rule's Alt structure
/// when the root rule is itself an Alt (JSON's `value =
/// object | array | string | number | bool | null` pattern); when the
/// root is a single-shape rule (e.g. a top-level Array), the
/// dispatcher emits a thin delegator.
pub fn emit_dispatcher(grammar_suffix: &str, ir: &GrammarIR) -> TokenStream {
    let Some(root_name) = root_rule_name(ir) else {
        return quote! {};
    };
    let entry = ir.entry;
    let Some(entry_rule) = ir.rules.iter().find(|r| r.id == entry) else {
        return quote! {};
    };

    let dispatcher_ident = dispatcher_fn_ident(grammar_suffix, &root_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    // Decide the dispatcher strategy based on the root rule's shape.
    // If the root is Alt-bodied (JSON's `value`) we emit a byte-
    // dispatch over the Alt branches. If the root is itself a shape
    // (e.g. a top-level Object), we emit a delegator to that shape fn.
    let root_tag = ir.shape_assignments.get(entry);

    let dispatch_body = if matches!(&entry_rule.body, bbnf_ir::IrNode::Alt(_, _))
        && has_shape_dispatch(ir)
    {
        // Root body is an Alt — enumerate branches and emit per-branch
        // byte-dispatch arms targeting each Ref's shape fn. Both an
        // unclassified root (pre-W4) and a Wrap-classified root (W4+)
        // take this path when the body is Alt-shaped.
        emit_alt_dispatch_body(grammar_suffix, entry_rule, ir)
    } else if root_tag.is_classified() {
        // AW-V.W4-activation — root is itself a W3 or W4 shape. The
        // dispatcher delegates directly to `parse_<shape>_<grammar>_<root>`.
        // Shape-fn arg shapes:
        //   - Number / Keyword take `first_byte` — the dispatcher peeks
        //     first non-ws byte, passes it in.
        //   - Object / Array / String / Scalar / Pratt / Unordered /
        //     ArgList / Flat / Wrap / HRegex take `(input, p, state,
        //     builder)` — the dispatcher skips leading ws and delegates.
        let shape_name = shape_tag_name(root_tag);
        let target_ident = shape_fn_ident(shape_name, grammar_suffix, &root_name);
        match root_tag {
            ShapeTag::Number | ShapeTag::Keyword => quote! {
                let first = #support_mod::skip_space(input, p, state)
                    .ok_or(::bbnf::runtime::tape::DtaError::UnexpectedEnd { offset: *p as u32 })?;
                #target_ident(input, p, first, builder)
            },
            _ => quote! {
                let _ = #support_mod::skip_space(input, p, state);
                #target_ident(input, p, state, builder)
            },
        }
    } else if matches!(root_tag, ShapeTag::None) && has_shape_dispatch(ir) {
        // Root unclassified but grammar has classified rules — use the
        // legacy Alt-dispatch body (pre-W4 pattern preserved for
        // transitional grammars where the root is a transparent alias).
        emit_alt_dispatch_body(grammar_suffix, entry_rule, ir)
    } else {
        // No shape coverage — shouldn't reach here (caller gates
        // dispatcher emission); emit a stub for safety.
        quote! {
            Err(::bbnf::runtime::tape::DtaError::InvalidState {
                state: ::bbnf::runtime::tape::DtaStateId::NONE,
            })
        }
    };

    // Dispatcher — for JSON's `value = object | array | string | number |
    // bool | null` Alt-dispatch pattern, this maps to a ByteDispatch
    // state at the DTA level. ByteDispatch pushes NO compound; it simply
    // transitions to the chosen branch rule's entry state. The
    // `pending_variant_idx` stamped by the Ref into `value` is then
    // overwritten by the target rule's own Ref-set stamp (e.g. array's
    // Ref sets variant=4, which lands on array's Seq compound push).
    //
    // Therefore the shape dispatcher emits NO outer wrap — it directly
    // delegates to the chosen shape fn. Both the root call site (from
    // `parse()`) and the non-root call site (from Object / Array value-
    // position recursion) share the same dispatch body; the walker
    // likewise does not differentiate between root and non-root value
    // positions (ByteDispatch's transition is the same either way).
    //
    // `#nonroot_ident` retained as an alias for backwards symbol
    // compatibility with the per-shape emitters that reference it —
    // both idents point at the same body.
    let nonroot_ident = format_ident!("{}__value", dispatcher_ident);
    let _ = entry;
    quote! {
        /// AW-V.W3.2 — top-level shape dispatcher.
        ///
        /// Mirrors the walker's `value` rule ByteDispatch: skip leading
        /// whitespace, dispatch on the first byte to the chosen branch
        /// shape fn, return its `TapeOffset` unchanged. No outer Rule /
        /// Alt compound is pushed — the DTA's ByteDispatch state for
        /// `value` emits no compound either, and the target rule's Ref
        /// overwrites any `pending_variant_idx` en route, so the chosen
        /// rule's own compound carries the final root variant.
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #dispatcher_ident(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut ::bbnf::runtime::tape::TapeBuilder,
        ) -> ::core::result::Result<
            ::bbnf::runtime::tape::TapeOffset,
            ::bbnf::runtime::tape::DtaError,
        > {
            #nonroot_ident(input, p, state, builder)
        }

        /// AW-V.W3.2 — value-position shape dispatcher. Called both at
        /// the grammar root and from Object / Array compound bodies.
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #nonroot_ident(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut ::bbnf::runtime::tape::TapeBuilder,
        ) -> ::core::result::Result<
            ::bbnf::runtime::tape::TapeOffset,
            ::bbnf::runtime::tape::DtaError,
        > {
            #dispatch_body
        }
    }
}

/// Convert a [`ShapeTag`] into the shape-fn prefix used by the
/// emitter (`object` / `array` / `string` / `number` / `keyword` /
/// `scalar` for W3; `pratt` / `unordered` / `arglist` / `flat` /
/// `wrap` / `hregex` for W4).
pub(super) fn shape_tag_name(tag: ShapeTag) -> &'static str {
    match tag {
        ShapeTag::Object => "object",
        ShapeTag::Array => "array",
        ShapeTag::String => "string",
        ShapeTag::Number => "number",
        ShapeTag::Keyword => "keyword",
        ShapeTag::Scalar => "scalar",
        ShapeTag::Pratt => "pratt",
        ShapeTag::Unordered => "unordered",
        ShapeTag::ArgList => "arglist",
        ShapeTag::Flat => "flat",
        ShapeTag::Wrap => "wrap",
        ShapeTag::HRegex => "hregex",
        ShapeTag::None => "unknown",
    }
}

/// Emit the Alt-dispatch body for the root rule — byte-matches the
/// next non-whitespace byte and calls the corresponding branch shape
/// fn. Mirrors `bbnf_json_prototype::parse_value`'s 6-arm match.
fn emit_alt_dispatch_body(
    grammar_suffix: &str,
    root_rule: &bbnf_ir::IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    use bbnf_ir::IrNode;

    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    // Walk the Alt branches (or single body) and collect per-branch
    // (first-byte(s), shape-fn-ident) pairs.
    let branches = match &root_rule.body {
        IrNode::Alt(bs, _) => bs.as_slice(),
        _ => {
            // Single body — unreachable via root_tag guard above.
            return quote! {
                Err(::bbnf::runtime::tape::DtaError::InvalidState {
                state: ::bbnf::runtime::tape::DtaStateId::NONE,
            })
            };
        }
    };

    let mut object_fn: Option<proc_macro2::Ident> = None;
    let mut array_fn: Option<proc_macro2::Ident> = None;
    let mut string_fn: Option<proc_macro2::Ident> = None;
    let mut number_fn: Option<proc_macro2::Ident> = None;
    let mut keyword_bool_fn: Option<proc_macro2::Ident> = None;
    let mut keyword_null_fn: Option<proc_macro2::Ident> = None;

    for branch in branches {
        let IrNode::Ref(rid) = &branch.node else { continue };
        let Some(rule) = ir.rules.iter().find(|r| r.id == *rid) else {
            continue;
        };
        let name = ir.get_string(rule.name);
        let tag = ir.shape_assignments.get(*rid);
        match tag {
            ShapeTag::Object => {
                object_fn = Some(shape_fn_ident("object", grammar_suffix, name));
            }
            ShapeTag::Array => {
                array_fn = Some(shape_fn_ident("array", grammar_suffix, name));
            }
            ShapeTag::String => {
                string_fn = Some(shape_fn_ident("string", grammar_suffix, name));
            }
            ShapeTag::Number => {
                number_fn = Some(shape_fn_ident("number", grammar_suffix, name));
            }
            ShapeTag::Keyword => {
                // Distinguish bool (two branches) from null (one
                // branch) via the rule's body shape.
                let is_null = rule_is_single_null_keyword(rule, ir);
                if is_null {
                    keyword_null_fn = Some(shape_fn_ident("keyword", grammar_suffix, name));
                } else {
                    keyword_bool_fn = Some(shape_fn_ident("keyword", grammar_suffix, name));
                }
            }
            _ => {}
        }
    }

    // Emit the arms, gating each on whether the branch shape fn
    // resolved. Missing branches fall into the default error arm.
    let object_arm = object_fn
        .as_ref()
        .map(|f| quote! { b'{' => { #f(input, p, state, builder) } })
        .unwrap_or_else(|| quote! {});
    let array_arm = array_fn
        .as_ref()
        .map(|f| quote! { b'[' => { #f(input, p, state, builder) } })
        .unwrap_or_else(|| quote! {});
    let string_arm = string_fn
        .as_ref()
        .map(|f| quote! { b'"' => { #f(input, p, state, builder) } })
        .unwrap_or_else(|| quote! {});
    let number_arm = number_fn
        .as_ref()
        .map(|f| quote! { b'-' | b'0'..=b'9' => { #f(input, p, first, builder) } })
        .unwrap_or_else(|| quote! {});
    let true_arm = keyword_bool_fn
        .as_ref()
        .map(|f| quote! { b't' | b'f' => { #f(input, p, first, builder) } })
        .unwrap_or_else(|| quote! {});
    let null_arm = keyword_null_fn
        .as_ref()
        .map(|f| quote! { b'n' => { #f(input, p, first, builder) } })
        .unwrap_or_else(|| quote! {});

    quote! {
        let first = #support_mod::skip_space(input, p, state)
            .ok_or(::bbnf::runtime::tape::DtaError::UnexpectedEnd { offset: *p as u32 })?;
        let __result = match first {
            #object_arm
            #array_arm
            #string_arm
            #number_arm
            #true_arm
            #null_arm
            c => {
                return ::core::result::Result::Err(
                    ::bbnf::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                        failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                    }
                );
            }
        };
        __result
    }
}

/// Predicate: is `rule`'s body a single literal matching `null`?
fn rule_is_single_null_keyword(rule: &bbnf_ir::IrRule, ir: &GrammarIR) -> bool {
    use bbnf_ir::IrNode;
    // Walk through Map / OptionalWhitespace wrappers.
    fn unwrap(node: &IrNode) -> &IrNode {
        match node {
            IrNode::Map { inner, .. } => unwrap(inner.as_ref()),
            IrNode::OptionalWhitespace(inner) => unwrap(inner.as_ref()),
            _ => node,
        }
    }
    matches!(unwrap(&rule.body), IrNode::Literal(sid)
        if ir.get_string(*sid) == "null")
}

// ─────────────────────────────────────────────────────────────────────
// AW-V.W3-bench-fix — visitor-path dispatcher.
//
// Mirrors the tape-path dispatcher (`parse_<grammar>_<root>`) but with
// a generic `V: JsonVisitor` parameter driving visitor method calls
// instead of tape records. Emitted alongside the tape-path so the
// per-shape visitor fns composing into the dispatcher each participate
// in the same monomorphisation at the call site.
//
// `parse_with_visitor::<V>` on the grammar struct routes here; the
// shape fns below call back into this dispatcher for value-position
// recursion.
// ─────────────────────────────────────────────────────────────────────

/// Emit the visitor-path dispatcher — `parse_<grammar>_<root>_visitor`.
///
/// The visitor-path dispatcher is isomorphic to the tape-path
/// [`emit_dispatcher`] but generic over a visitor type `V: JsonVisitor`.
/// It bypasses the tape entirely: visitor method calls (`begin_object`,
/// `key`, `string`, `number_f64`, etc.) replace the tape record pushes.
/// The prototype's `bbnf_json_prototype::parse_value::<V>` shape is the
/// reference — one monomorphic dispatcher per visitor type, all
/// per-shape bodies inlined.
pub fn emit_visitor_dispatcher(grammar_suffix: &str, ir: &GrammarIR) -> TokenStream {
    let Some(root_name) = root_rule_name(ir) else {
        return quote! {};
    };
    let entry = ir.entry;
    let Some(entry_rule) = ir.rules.iter().find(|r| r.id == entry) else {
        return quote! {};
    };

    let dispatcher_ident = visitor_dispatcher_fn_ident(grammar_suffix, &root_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    let root_tag = ir.shape_assignments.get(entry);

    let dispatch_body = if matches!(&entry_rule.body, bbnf_ir::IrNode::Alt(_, _))
        && has_shape_dispatch(ir)
    {
        emit_visitor_alt_dispatch_body(grammar_suffix, entry_rule, ir)
    } else if root_tag.is_classified() {
        // AW-V.W4-activation — root is itself a W3 or W4 shape.
        let shape_name = shape_tag_name(root_tag);
        let target_ident = visitor_shape_fn_ident(shape_name, grammar_suffix, &root_name);
        match root_tag {
            ShapeTag::Number | ShapeTag::Keyword => quote! {
                let first = #support_mod::skip_space(input, p, state)
                    .ok_or(::bbnf::runtime::ParseErr::Syntax {
                        offset: *p as u32, rule: None,
                    })?;
                #target_ident(input, p, first, visitor)
            },
            _ => quote! {
                let _ = #support_mod::skip_space(input, p, state);
                #target_ident(input, p, state, visitor)
            },
        }
    } else if matches!(root_tag, ShapeTag::None) && has_shape_dispatch(ir) {
        emit_visitor_alt_dispatch_body(grammar_suffix, entry_rule, ir)
    } else {
        quote! {
            Err(::bbnf::runtime::ParseErr::Syntax {
                offset: *p as u32, rule: None,
            })
        }
    };

    let nonroot_ident = format_ident!("{}__value", dispatcher_ident);
    quote! {
        /// AW-V.W3-bench-fix — top-level visitor-path dispatcher.
        ///
        /// Generic over the visitor type; `V: JsonVisitor` composes all
        /// per-shape sub-trait bounds (`ObjectVisitor`, `ArrayVisitor`,
        /// `StringVisitor`, `NumberVisitor`, `KeywordVisitor`) so every
        /// per-shape method invocation resolves statically at the
        /// monomorphisation site. Bypasses the tape entirely.
        ///
        /// The dispatcher's bounds are narrow by design: emitted only
        /// for grammars whose classified rules use W3-pure shapes
        /// (Object / Array / String / Number / Keyword / Scalar).
        /// Grammars carrying W4-classified rules (Pratt / Unordered /
        /// ArgList / Flat / Wrap / HRegex) skip visitor dispatcher
        /// emission entirely — the tape-path dispatcher still emits,
        /// the per-shape fns still compile, but the generic `V`
        /// visitor bound can't union W4 visitor sub-traits without
        /// rippling into callers that don't have those bounds. Visitor
        /// activation for W4-carrying grammars lands in a follow-on
        /// wave alongside the per-Ref `__value` dispatcher refactor.
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #dispatcher_ident<V>(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            visitor: &mut V,
        ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
        where
            V: ::bbnf::runtime::tape::ObjectVisitor
                + ::bbnf::runtime::tape::ArrayVisitor
                + ::bbnf::runtime::tape::StringVisitor
                + ::bbnf::runtime::tape::NumberVisitor
                + ::bbnf::runtime::tape::KeywordVisitor,
        {
            #nonroot_ident(input, p, state, visitor)
        }

        /// AW-V.W3-bench-fix — value-position visitor-path dispatcher.
        /// Called both at the grammar root and from the object / array
        /// shape fns' value-position recursion.
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #nonroot_ident<V>(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            visitor: &mut V,
        ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
        where
            V: ::bbnf::runtime::tape::ObjectVisitor
                + ::bbnf::runtime::tape::ArrayVisitor
                + ::bbnf::runtime::tape::StringVisitor
                + ::bbnf::runtime::tape::NumberVisitor
                + ::bbnf::runtime::tape::KeywordVisitor,
        {
            #dispatch_body
        }
    }
}

/// Returns `true` when `ir` has any `ShapeTag::Pratt` or
/// `ShapeTag::Unordered` rule — the shapes whose emitted bodies invoke
/// visitor methods (`PrattVisitor`, bespoke) *outside* the dispatcher's
/// W3 bound set (`ObjectVisitor + ArrayVisitor + StringVisitor +
/// NumberVisitor + KeywordVisitor`). Used by [`emit_shapes_for_grammar`]
/// + `grammar::emit_grammar_impl` to gate visitor-path emission:
/// Pratt/Unordered need trait bounds the dispatcher does not carry, so
/// grammars with those rules emit the tape path only.
///
/// Flat / Wrap / ArgList / HRegex are *not* W4-trait-bound — their
/// emitted bodies invoke only `.begin_*` / `.end_*` / `.string` /
/// `.number` / delegate-to-Ref, all of which are W3-bound visitor
/// methods. Those shapes do not trip the visitor-path trait-bound
/// mismatch, so they do not gate off the visitor path (AX.W0a.1 —
/// `docs/tranches/AW/audit/V-audit-overfit.md` §Gate-pathology).
pub fn has_w4_classified(ir: &GrammarIR) -> bool {
    ir.rules.iter().any(|r| {
        !r.meta.is_transparent
            && matches!(
                ir.shape_assignments.get(r.id),
                ShapeTag::Pratt | ShapeTag::Unordered,
            )
    })
}

// ─────────────────────────────────────────────────────────────────────
// AW-V.W5.2 — per-Ref value-position routing (Approach B).
//
// The dispatcher's `__value` body (above) byte-dispatches over Alt
// branches — it only works when the root rule is `Alt(Ref, Ref, …)` of
// classified branches (JSON's `value`). Non-Alt-rooted grammars (CSS
// `stylesheet` Array, Sheets `formula` Flat, BBNF `grammar` Array)
// cannot route per-Ref recursion through `__value` because the root
// shape fn IS the target of the delegating `__value` — recursing would
// loop back into the root.
//
// W5.2 resolves the target rule's shape at emission time and emits a
// direct call to that shape's per-rule fn, inlining through LLVM's
// per-site specialisation. Every shape emitter that previously called
// `#dispatcher_ident(input, p, state, builder)?` for a Ref-position
// recursion now receives the Ref's target `RuleId` at emission time
// and emits via [`emit_ref_call_tape`] / [`emit_ref_call_visitor`].
//
// The helpers return `None` when the target is unclassified
// (`ShapeTag::None`) — in that case the grammar is not admissible for
// `parse()` routing via the shape dispatcher entrypoint, and
// [`super::has_shape_dispatcher_entrypoint`] gates accordingly.
// ─────────────────────────────────────────────────────────────────────

/// Helper: derive the `__shape_support_<grammar>` module identifier.
/// Kept as a fn so `quote!` interpolation works cleanly.
fn support_mod_ident(grammar_suffix: &str) -> proc_macro2::Ident {
    format_ident!("__shape_support_{}", grammar_suffix)
}

/// Tape-path Ref-call emitter: resolves `target_rid`'s shape and emits
/// the direct call. Returns `None` when the target is unclassified.
///
/// Used by every shape emitter at value-position Ref sites. The emitted
/// stream is a single expression ending in a `Result`; the caller wraps
/// with `?` or `.map(|_| ...)` as appropriate.
pub fn emit_ref_call_tape(
    grammar_suffix: &str,
    target_rid: bbnf_ir::RuleId,
    ir: &GrammarIR,
) -> Option<TokenStream> {
    let target = ir.rules.iter().find(|r| r.id == target_rid)?;
    let tag = ir.shape_assignments.get(target_rid);
    let shape_name = match tag {
        ShapeTag::Object => "object",
        ShapeTag::Array => "array",
        ShapeTag::String => "string",
        ShapeTag::Number => "number",
        ShapeTag::Keyword => "keyword",
        ShapeTag::Scalar => "scalar",
        ShapeTag::Pratt => "pratt",
        ShapeTag::Unordered => "unordered",
        ShapeTag::ArgList => "arglist",
        ShapeTag::Flat => "flat",
        ShapeTag::Wrap => "wrap",
        ShapeTag::HRegex => "hregex",
        ShapeTag::None => return None,
    };
    let target_fn = shape_fn_ident(shape_name, grammar_suffix, ir.get_string(target.name));
    let support_mod = support_mod_ident(grammar_suffix);
    let expr = match tag {
        ShapeTag::Number | ShapeTag::Keyword => quote! {
            {
                let __first = #support_mod::skip_space(input, p, state)
                    .ok_or(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                        offset: *p as u32,
                    })?;
                #target_fn(input, p, __first, builder)
            }
        },
        _ => quote! {
            {
                let _ = #support_mod::skip_space(input, p, state);
                #target_fn(input, p, state, builder)
            }
        },
    };
    Some(expr)
}

/// Visitor-path Ref-call emitter: resolves `target_rid`'s shape and
/// emits the direct visitor-path call. Returns `None` when the target
/// is unclassified.
pub fn emit_ref_call_visitor(
    grammar_suffix: &str,
    target_rid: bbnf_ir::RuleId,
    ir: &GrammarIR,
) -> Option<TokenStream> {
    let target = ir.rules.iter().find(|r| r.id == target_rid)?;
    let tag = ir.shape_assignments.get(target_rid);
    let shape_name = match tag {
        ShapeTag::Object => "object",
        ShapeTag::Array => "array",
        ShapeTag::String => "string",
        ShapeTag::Number => "number",
        ShapeTag::Keyword => "keyword",
        ShapeTag::Scalar => "scalar",
        ShapeTag::Pratt => "pratt",
        ShapeTag::Unordered => "unordered",
        ShapeTag::ArgList => "arglist",
        ShapeTag::Flat => "flat",
        ShapeTag::Wrap => "wrap",
        ShapeTag::HRegex => "hregex",
        ShapeTag::None => return None,
    };
    let target_fn =
        visitor_shape_fn_ident(shape_name, grammar_suffix, ir.get_string(target.name));
    let support_mod = support_mod_ident(grammar_suffix);
    let expr = match tag {
        ShapeTag::Number | ShapeTag::Keyword => quote! {
            {
                let __first = #support_mod::skip_space(input, p, state)
                    .ok_or(::bbnf::runtime::ParseErr::Syntax {
                        offset: *p as u32, rule: None,
                    })?;
                #target_fn(input, p, __first, visitor)
            }
        },
        ShapeTag::String => quote! {
            {
                let _ = #support_mod::skip_space(input, p, state);
                #target_fn(input, p, state, visitor, /*is_key=*/ false)
            }
        },
        _ => quote! {
            {
                let _ = #support_mod::skip_space(input, p, state);
                #target_fn(input, p, state, visitor)
            }
        },
    };
    Some(expr)
}

/// Walk `node` and collect the target `RuleId` of every `Ref(rid)` at a
/// value position reachable from `node`. Used by detector-side
/// admission gates to check whether every value-position Ref in a
/// shape body routes to a classified rule.
///
/// Traverses through Map / OptionalWhitespace / Seq / Next / Skip /
/// Alt / Repeat / Minus / Negate / TokenDispatch — every syntactic
/// wrapper that holds a value-position child. Leaf nodes (Literal /
/// Regex / Epsilon) contribute no Refs.
pub fn collect_value_refs(node: &bbnf_ir::IrNode) -> Vec<bbnf_ir::RuleId> {
    use bbnf_ir::IrNode;
    let mut refs = Vec::new();
    fn walk(node: &IrNode, refs: &mut Vec<bbnf_ir::RuleId>) {
        match node {
            IrNode::Ref(rid) => refs.push(*rid),
            IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
                walk(inner, refs);
            }
            IrNode::Seq(children) => {
                for c in children {
                    walk(c, refs);
                }
            }
            IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
                walk(lhs, refs);
                walk(rhs, refs);
            }
            IrNode::Alt(branches, _) => {
                for b in branches {
                    walk(&b.node, refs);
                }
            }
            IrNode::Repeat { inner, .. } => walk(inner, refs),
            IrNode::Minus(lhs, rhs) => {
                walk(lhs, refs);
                walk(rhs, refs);
            }
            IrNode::Negate(inner) => walk(inner, refs),
            IrNode::TokenDispatch { .. }
            | IrNode::Literal(_)
            | IrNode::Regex(_)
            | IrNode::Epsilon => {}
        }
    }
    walk(node, &mut refs);
    refs
}


/// Visitor-path Alt-dispatch body — byte-matches the next non-
/// whitespace byte and invokes the matching visitor-path shape fn.
fn emit_visitor_alt_dispatch_body(
    grammar_suffix: &str,
    root_rule: &bbnf_ir::IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    use bbnf_ir::IrNode;

    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    let branches = match &root_rule.body {
        IrNode::Alt(bs, _) => bs.as_slice(),
        _ => {
            return quote! {
                Err(::bbnf::runtime::ParseErr::Syntax {
                    offset: *p as u32, rule: None,
                })
            };
        }
    };

    let mut object_fn: Option<proc_macro2::Ident> = None;
    let mut array_fn: Option<proc_macro2::Ident> = None;
    let mut string_fn: Option<proc_macro2::Ident> = None;
    let mut number_fn: Option<proc_macro2::Ident> = None;
    let mut keyword_bool_fn: Option<proc_macro2::Ident> = None;
    let mut keyword_null_fn: Option<proc_macro2::Ident> = None;

    for branch in branches {
        let IrNode::Ref(rid) = &branch.node else { continue };
        let Some(rule) = ir.rules.iter().find(|r| r.id == *rid) else {
            continue;
        };
        let name = ir.get_string(rule.name);
        let tag = ir.shape_assignments.get(*rid);
        match tag {
            ShapeTag::Object => {
                object_fn = Some(visitor_shape_fn_ident("object", grammar_suffix, name));
            }
            ShapeTag::Array => {
                array_fn = Some(visitor_shape_fn_ident("array", grammar_suffix, name));
            }
            ShapeTag::String => {
                string_fn = Some(visitor_shape_fn_ident("string", grammar_suffix, name));
            }
            ShapeTag::Number => {
                number_fn = Some(visitor_shape_fn_ident("number", grammar_suffix, name));
            }
            ShapeTag::Keyword => {
                let is_null = rule_is_single_null_keyword(rule, ir);
                if is_null {
                    keyword_null_fn =
                        Some(visitor_shape_fn_ident("keyword", grammar_suffix, name));
                } else {
                    keyword_bool_fn =
                        Some(visitor_shape_fn_ident("keyword", grammar_suffix, name));
                }
            }
            _ => {}
        }
    }

    let object_arm = object_fn
        .as_ref()
        .map(|f| quote! { b'{' => { #f(input, p, state, visitor) } })
        .unwrap_or_else(|| quote! {});
    let array_arm = array_fn
        .as_ref()
        .map(|f| quote! { b'[' => { #f(input, p, state, visitor) } })
        .unwrap_or_else(|| quote! {});
    let string_arm = string_fn
        .as_ref()
        .map(|f| quote! { b'"' => { #f(input, p, state, visitor, /*is_key=*/ false) } })
        .unwrap_or_else(|| quote! {});
    let number_arm = number_fn
        .as_ref()
        .map(|f| quote! { b'-' | b'0'..=b'9' => { #f(input, p, first, visitor) } })
        .unwrap_or_else(|| quote! {});
    let true_arm = keyword_bool_fn
        .as_ref()
        .map(|f| quote! { b't' | b'f' => { #f(input, p, first, visitor) } })
        .unwrap_or_else(|| quote! {});
    let null_arm = keyword_null_fn
        .as_ref()
        .map(|f| quote! { b'n' => { #f(input, p, first, visitor) } })
        .unwrap_or_else(|| quote! {});

    quote! {
        let first = #support_mod::skip_space(input, p, state)
            .ok_or(::bbnf::runtime::ParseErr::Syntax {
                offset: *p as u32, rule: None,
            })?;
        match first {
            #object_arm
            #array_arm
            #string_arm
            #number_arm
            #true_arm
            #null_arm
            _ => Err(::bbnf::runtime::ParseErr::Syntax {
                offset: *p as u32, rule: None,
            }),
        }
    }
}
