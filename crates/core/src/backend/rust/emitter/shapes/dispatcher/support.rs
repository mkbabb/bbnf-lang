//! Per-grammar SIMD/scan support module emission.
//!
//! Each grammar with shape dispatch carries a private
//! `__shape_support_<grammar>` module inlined alongside the
//! dispatcher. The module hosts:
//!
//! - `ScanState` — 64-byte whitespace-bitmap cache, lazy
//!   `OnceCell<StructuralIndex>` for grammars whose mined
//!   `structural_alphabet` is non-empty.
//! - `skip_space` / `skip_space_slow` — plain or comment-aware
//!   variant per the grammar's `@ws` regex classification, with
//!   the optional CTNS structural-index probe at the head of the
//!   slow path.
//! - `nospace_bitmap_64` — per-arch SIMD whitespace bitmap kernel
//!   (NEON / AVX2 / scalar fallback).
//! - `first_quote_or_backslash` — string-scanner primitive
//!   mirroring `json-prototype::simd::first_quote_or_backslash`.
//! - `shape_byte_arm`, `expect_keyword` — small dispatch helpers.
//!
//! Emitted once per grammar with shape dispatch; every per-shape
//! fn inlines the contents at link time under workspace LTO.

use bbnf_ir::GrammarIR;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

/// Classify the grammar's `@ws` directive. When the pattern matches
/// `RegexClass::WhitespaceWithBlockComment`, `skip_space` must also
/// skip `/* ... */` block comments (CSS contract); otherwise the
/// default JSON-shaped ASCII-whitespace-only skip applies.
fn ws_is_comment_aware(ir: &GrammarIR) -> bool {
    use parse_that::regex::classify::{RegexClass, classify_regex};
    let Some(ws_sid) = ir.ws_pattern else {
        return false;
    };
    let pattern = ir.get_string(ws_sid);
    matches!(classify_regex(pattern), RegexClass::WhitespaceWithBlockComment)
}

/// AY.W4.3 — whether the grammar's mined `structural_alphabet` is
/// non-empty AND suitable for substrate emission. Gates the
/// OnceCell field + ensure_structural_index helper — the probe
/// gating (`ctns_probe_admits` below) is strictly tighter.
fn has_structural_alphabet(ir: &GrammarIR) -> bool {
    !ir.profile().structural_alphabet.is_empty()
}

/// AY.W4.3 — whether the CTNS probe should be emitted at the head
/// of `skip_space_slow`. Conditions:
///
/// 1. Non-empty alphabet (probe needs something to skip toward).
/// 2. Comment-aware grammar (predictable long whitespace+comment
///    runs recoup the OnceCell scan_structural init cost); OR
///    sparse non-whitespace alphabet of moderate size (12..=24
///    bytes, excluding the near-empty JSON case which is faster
///    with pure bitmap).
/// 3. Alphabet excludes whitespace bytes — landing on whitespace
///    would break skip_space's "first non-whitespace byte"
///    contract.
///
/// In practice this admits Sheets (19 bytes, no whitespace, plain
/// @ws) and excludes JSON (6 bytes — too sparse to beat bitmap),
/// BBNF (28 bytes including whitespace), CSS L4 (53 bytes — over-
/// broad mining). The probe substrate (OnceCell + helper) emits
/// for any non-empty alphabet so future tranches can wire
/// additional consumers.
fn ctns_probe_admits(ir: &GrammarIR) -> bool {
    let alphabet = ir.profile().structural_alphabet;
    if alphabet.is_empty() {
        return false;
    }
    if alphabet.iter().any(|&b| b == b' ' || b == b'\t' || b == b'\n' || b == b'\r') {
        return false;
    }
    // Sparse-alphabet threshold: at least 12 bytes to beat the
    // bitmap loop's constant cost, at most 24 bytes to keep the
    // structural index density reasonable.
    alphabet.len() >= 12 && alphabet.len() <= 24
}

/// AY.W4.3 — emit a CTNS-style structural-index probe to inject at
/// the head of `skip_space_slow`. Only fires when the forward
/// whitespace+comment run is SUBSTANTIAL — the gap to the next
/// structural byte must exceed a SIMD-stripe boundary (64 bytes)
/// to recoup the probe's per-call overhead. On short runs the
/// bitmap loop handles them faster than the probe's validation
/// scan.
///
/// The probe advances `*p` on success but NEVER `return;`s —
/// letting the surrounding loop dispatch on the landed byte
/// (comment-skip for `/*`, bitmap loop for whitespace, return
/// for semantic byte).
fn ctns_probe_tokens() -> TokenStream {
    quote! {
        // AY.W4.3 — CTNS probe. Gated on gap > 64 B (one SIMD
        // stripe) to exceed the bitmap loop's crossover point.
        // On short whitespace runs the probe is skipped entirely;
        // the OnceCell lazy-init still runs once per parse but the
        // per-call cost is O(log N) binary search + a bounds test.
        let __ctns_idx = ensure_structural_index(state, input);
        if let ::core::option::Option::Some(__next_struct) =
            crate::runtime::tape::next_structural_at_or_after(
                __ctns_idx, *p as u32,
            )
        {
            let __next = __next_struct as usize;
            let __gap = __next.saturating_sub(*p);
            // Only probe when the gap exceeds one SIMD stripe; on
            // tight whitespace runs the bitmap loop wins. Cap the
            // validation window at 4096 B so pathological inputs
            // with hostile structural placement don't dominate.
            if __gap > 64 && __gap <= 4096 && __next <= input.len() {
                let __slice = unsafe {
                    input.get_unchecked(*p..__next)
                };
                let mut __all_ws = true;
                for &__b in __slice {
                    if __b != b' ' && __b != b'\t'
                        && __b != b'\n' && __b != b'\r'
                    {
                        __all_ws = false;
                        break;
                    }
                }
                if __all_ws {
                    *p = __next;
                    state.nospace_start = -1;
                    // The surrounding loop dispatches on the
                    // landed byte.
                }
            }
        }
    }
}

/// Emit the plain-ASCII `skip_space` pair (`skip_space` + `skip_space_slow`)
/// for grammars whose `@ws` pattern is the default JSON-shaped set
/// `{' ', '\t', '\n', '\r'}`. Uses the 64-byte SIMD bitmap cache
/// on the slow path. Takes `ctns_probe` (usually empty for plain-skip
/// grammars per the W4.3 gate — see `emit_support_module` for the
/// decision to keep the plain path probe-free).
fn emit_skip_space_plain_inner(ctns_probe: TokenStream) -> TokenStream {
    quote! {
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
            // AY.W1-fix retired the eager parse-entry scan_structural
            // call here. AY.W4.3 lands a lazy CTNS probe (gated on a
            // sparse non-whitespace alphabet — see
            // `has_structural_alphabet`) that pays for itself on
            // long whitespace runs.
            #ctns_probe
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
    }
}

/// Emit the comment-aware `skip_space` pair for grammars whose `@ws`
/// pattern matches `RegexClass::WhitespaceWithBlockComment`
/// (CSS's `(?s)(?:\s|\/\*...\*\/)*`).
///
/// The fast path treats `/` as a candidate separator (because `/*`
/// may open a block comment). The slow path alternates between
/// the SIMD whitespace bitmap (reused intact) and a per-iteration
/// check for `/*...*/`; on a comment opening, the body is skipped
/// via `memchr`-style scan for `*/` and the bitmap cache is
/// invalidated. A bare `/` (not followed by `*`) terminates the
/// skip — this mirrors the `@ws` regex's semantics (the comment
/// alternative starts with `/*`; a lone `/` is not whitespace).
///
/// AY.W4.3 — when `has_structural` is true, the slow path opens
/// with a CTNS-style probe via the lazy `structural_index`: on
/// long whitespace runs whose intervening bytes are all in the
/// `{' ', '\t', '\n', '\r'}` set AND the next structural byte is
/// not a comment opener (`/`), advance `*p` directly to the
/// structural position, bypassing the SIMD bitmap iteration.
fn emit_skip_space_comment_aware_inner(ctns_probe: TokenStream) -> TokenStream {
    quote! {
        /// Skip whitespace AND `/* ... */` block comments at `*p`,
        /// returning the first non-whitespace, non-comment byte
        /// (or `None` on EOF). Hot-path fast-exit when the next
        /// byte is neither whitespace nor `/`.
        ///
        /// When the next byte is `/`: if followed by `*`, enter
        /// the slow path to consume the comment body; otherwise
        /// return `Some(b'/')` — a bare `/` is a semantic byte.
        #[inline(always)]
        pub fn skip_space(
            input: &[u8],
            p: &mut usize,
            state: &mut ScanState,
        ) -> Option<u8> {
            match input.get(*p) {
                Some(&b)
                    if b != b' '
                        && b != b'\t'
                        && b != b'\n'
                        && b != b'\r'
                        && b != b'/' =>
                {
                    Some(b)
                }
                Some(&b'/') if input.get(*p + 1) != Some(&b'*') => {
                    // Bare `/` — not a comment opening; return it as
                    // the first non-whitespace byte.
                    Some(b'/')
                }
                None => None,
                _ => {
                    skip_space_slow(input, p, state);
                    input.get(*p).copied()
                }
            }
        }

        /// Advance `*p` past ASCII whitespace AND `/* ... */` block
        /// comments. The bitmap cache accelerates pure-whitespace
        /// runs; comment detection runs on every iteration where
        /// `*p` points at `/`.
        ///
        /// AY.W4.3 — opens with a CTNS-style structural-index probe
        /// when the grammar mines a non-empty alphabet. On long
        /// whitespace runs that don't intersect comment openers,
        /// the probe jumps directly to the next structural byte
        /// instead of iterating SIMD stripes.
        #[inline(always)]
        pub(crate) fn skip_space_slow(
            input: &[u8],
            p: &mut usize,
            state: &mut ScanState,
        ) {
            #ctns_probe
            loop {
                // Inline whitespace skip — reuses the bitmap cache
                // when valid.
                let cache_base = state.nospace_start;
                if cache_base >= 0 && (*p as isize) >= cache_base {
                    let rel = *p - cache_base as usize;
                    if rel < 64 {
                        let masked = state.nospace_bits & (!0u64 << rel);
                        if masked != 0 {
                            let bit = masked.trailing_zeros() as usize;
                            *p = cache_base as usize + bit;
                        } else {
                            *p = cache_base as usize + 64;
                            continue;
                        }
                    }
                }
                if *p >= input.len() {
                    return;
                }
                if *p + 64 > input.len() {
                    // Tail — direct byte loop.
                    while let Some(&b) = input.get(*p) {
                        if b != b' ' && b != b'\t' && b != b'\n' && b != b'\r' {
                            break;
                        }
                        *p += 1;
                    }
                } else {
                    // 64-byte SIMD stripe.
                    let stripe = unsafe {
                        ::core::slice::from_raw_parts(
                            input.as_ptr().add(*p),
                            64,
                        )
                    };
                    let mask = nospace_bitmap_64(stripe);
                    state.nospace_bits = mask;
                    state.nospace_start = *p as isize;
                    if mask != 0 {
                        let bit = mask.trailing_zeros() as usize;
                        *p += bit;
                    } else {
                        *p += 64;
                        continue;
                    }
                }
                // Whitespace run terminated. Check whether `*p`
                // opens a block comment.
                if input.get(*p) == Some(&b'/')
                    && input.get(*p + 1) == Some(&b'*')
                {
                    // Consume `/*` + body + `*/`. Body-scan iterates
                    // forward looking for `*/`; LLVM vectorises the
                    // byte search to SIMD under `-O3`.
                    *p += 2;
                    let len = input.len();
                    loop {
                        if *p + 1 >= len {
                            // Unterminated comment — eat to EOF.
                            *p = len;
                            state.nospace_start = -1;
                            return;
                        }
                        // Manual `*/` search. `iter().position` lowers
                        // to a vectorisable loop; inlined here so the
                        // emitted code has no external crate dep.
                        let slice = unsafe {
                            input.get_unchecked(*p..len)
                        };
                        match slice.iter().position(|&b| b == b'*') {
                            None => {
                                *p = len;
                                state.nospace_start = -1;
                                return;
                            }
                            Some(rel) => {
                                *p += rel + 1;
                                if input.get(*p) == Some(&b'/') {
                                    *p += 1;
                                    break;
                                }
                                // Lone `*` — keep scanning body.
                            }
                        }
                    }
                    // Advanced past comment; invalidate the cache
                    // (the stripe we just scanned may no longer be
                    // adjacent to `*p`).
                    state.nospace_start = -1;
                    continue;
                }
                // Not whitespace and not a comment opening — done.
                return;
            }
        }
    }
}

/// Per-grammar SIMD support module. Emitted once per grammar with
/// shape dispatch. Contains the `ScanState`, `skip_space`, and SIMD
/// primitives every shape fn inlines.
///
/// The module is emitted at the same scope as the per-shape
/// functions so they can share `ScanState` by reference. Naming:
/// `__shape_support_<grammar>` — the grammar suffix prevents
/// collisions when multiple grammars coexist in one compilation.
///
/// # Comment-aware whitespace (AX.W0a.2.s)
///
/// When the grammar's `@ws` pattern classifies as
/// [`RegexClass::WhitespaceWithBlockComment`], `skip_space` transparently
/// also skips `/* ... */` block comments. CSS L4's `@ws` regex
/// `(?s)(?:\s|\/\*[^*]*(?:\*+[^\/][^*]*)*\*+\/)*` activates this path;
/// JSON / Sheets / BBNF fall through to the plain ASCII-ws skip.
/// The dispatch is compile-time — callers never see two APIs.
///
/// # Structural-scan consumer (AY.W4.3 — W1 absorption)
///
/// When the grammar's mined `GRAMMAR_PROFILE.structural_alphabet`
/// has > 0 cardinality, ScanState carries a lazy
/// `OnceCell<StructuralIndex>` — populated on first per-parse
/// query via `scan_structural(input, alphabet)`. Consumer sites
/// (currently the comment-aware `skip_space_slow`'s post-comment
/// resume + `__regex_scan_<grammar>` adapter cold-path) probe via
/// `next_structural_at_or_after` to fast-skip to the next
/// structural delimiter when the in-stripe SIMD scan would
/// otherwise iterate byte-by-byte. Lazy-init keeps the cold-path
/// cost amortised — empty-alphabet grammars never pay the scan.
pub fn emit_support_module(grammar_suffix: &str, ir: &GrammarIR) -> TokenStream {
    let mod_ident = format_ident!("__shape_support_{}", grammar_suffix);
    let comment_aware = ws_is_comment_aware(ir);
    let has_structural = has_structural_alphabet(ir);
    // AY.W4.3 — CTNS probe gated on `ctns_probe_admits`: sparse
    // (<= 24 bytes) non-whitespace alphabet. Sheets (19 bytes,
    // no whitespace) qualifies; CSS L4 (53 bytes, over-broad
    // mining) does not.
    let ctns_probe = if ctns_probe_admits(ir) {
        ctns_probe_tokens()
    } else {
        quote! {}
    };
    let skip_space_body = if comment_aware {
        emit_skip_space_comment_aware_inner(ctns_probe)
    } else {
        emit_skip_space_plain_inner(ctns_probe)
    };

    // AY.W4.3 — lazy structural-scan field on ScanState.
    let structural_field = if has_structural {
        quote! {
            /// AY.W4.3 — lazy structural-byte index. Populated on
            /// first consumer query via `ensure_structural_index`;
            /// `OnceCell` discipline keeps the O(N) scan cost
            /// amortised across the parse rather than paid eagerly
            /// at parse-entry (AY.W1-fix demonstrated eager scans
            /// regress JSON twitter -64%).
            pub(crate) structural_index: ::core::cell::OnceCell<
                crate::runtime::tape::StructuralIndex,
            >,
        }
    } else {
        quote! {}
    };
    let structural_init = if has_structural {
        quote! { structural_index: ::core::cell::OnceCell::new(), }
    } else {
        quote! {}
    };
    let ensure_structural_fn = if has_structural {
        quote! {
            /// AY.W4.3 — lazy-init the per-parse structural index
            /// against the grammar's mined `structural_alphabet`.
            /// Idempotent; consumers may call freely.
            #[inline]
            pub(crate) fn ensure_structural_index<'a>(
                state: &'a mut ScanState,
                input: &[u8],
            ) -> &'a crate::runtime::tape::StructuralIndex {
                state.structural_index.get_or_init(|| {
                    crate::runtime::tape::scan_structural(
                        input,
                        super::GRAMMAR_PROFILE.structural_alphabet,
                    )
                })
            }
        }
    } else {
        quote! {}
    };

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
            /// cache mirroring `json-prototype::simd::ScanState`.
            ///
            /// AY.W4.3 — for grammars whose `structural_alphabet` is
            /// non-empty, ScanState additionally carries a lazy
            /// `OnceCell<StructuralIndex>` consumed by CTNS-style
            /// probes. Lazy-init keeps the O(N) scan cost amortised
            /// rather than paid eagerly at parse entry — see
            /// `AYW1-twitter-regression-diag` for the eager-init
            /// regression that motivates the OnceCell discipline.
            #[derive(Debug, Default)]
            pub struct ScanState {
                pub(crate) nospace_bits: u64,
                pub(crate) nospace_start: isize,
                #structural_field
            }

            impl ScanState {
                #[inline]
                pub fn new() -> Self {
                    Self {
                        nospace_bits: 0,
                        nospace_start: -1,
                        #structural_init
                    }
                }
            }

            #ensure_structural_fn

            #skip_space_body

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
            /// Mirrors `json-prototype::simd::first_quote_or_backslash`.
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
