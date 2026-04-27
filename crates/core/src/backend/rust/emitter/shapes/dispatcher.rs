//! Dispatcher emitter — top-level `parse_<grammar>_<root>` entry
//! point + the per-grammar shape support module (SIMD whitespace
//! cache, first-byte dispatch helpers).
//!
//! # Role — AW-V.W3.2
//!
//! The dispatcher mirrors the prototype's
//! `json_prototype::parse_json` shape: skip initial whitespace,
//! dispatch on the first byte to the appropriate shape function,
//! verify trailing whitespace. Per-rule recursion threads through the
//! dispatcher (e.g. object's value-position reads dispatch back
//! through the shape dispatcher to land on number / string / bool /
//! null / nested object / array arms).
//!
//! The support module emits per-grammar SIMD scaffolding —
//! `ScanState` (64-byte whitespace bitmap cache, mirroring
//! `json-prototype`'s `src/simd.rs::ScanState`), `skip_space`,
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
/// but takes `&mut V: JsonVisitor` instead of `&mut FusedBuilder`,
/// emitting visitor method calls (`begin_object`, `key`, `string`,
/// `number_f64`, etc.) in place of tape record pushes. Monomorphised
/// per visitor at the call site; zero structural / PSI overhead on
/// the hot path. Matches `json-prototype`'s perf shape.
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
///
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
            // AX.W0a.2.g — Keyword signature extended with `state`.
            // Number stays at `(input, p, first, builder)`; Keyword now
            // takes `(input, p, first, state, builder)`. The split
            // mirrors the Ref-call emitter's per-shape switch.
            ShapeTag::Number => quote! {
                let first = #support_mod::skip_space(input, p, state)
                    .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd { offset: *p as u32 })?;
                #target_ident(input, p, first, builder)
            },
            ShapeTag::Keyword => quote! {
                let first = #support_mod::skip_space(input, p, state)
                    .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd { offset: *p as u32 })?;
                #target_ident(input, p, first, state, builder)
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
            Err(crate::runtime::tape::DtaError::InvalidState {
                state: crate::runtime::tape::DtaStateId::NONE,
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

    // AY-II.W0.e — Grammar-activated structural-scan policy table.
    // Emitted at module scope alongside the dispatcher so the policy
    // is visible to every shape emitter's consumer site without
    // requiring cross-module path fix-up.
    let scan_policy = emit_structural_scan_policy(grammar_suffix, ir);

    quote! {
        #scan_policy

        /// AW-V.W3.2 — top-level shape dispatcher.
        ///
        /// Mirrors the walker's `value` rule ByteDispatch: skip leading
        /// whitespace, dispatch on the first byte to the chosen branch
        /// shape fn, return its `TapeOffset` unchanged. No outer Rule /
        /// Alt compound is pushed — the DTA's ByteDispatch state for
        /// `value` emits no compound either, and the target rule's Ref
        /// overwrites any `pending_variant_idx` en route, so the chosen
        /// rule's own compound carries the final root variant.
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]` per cross-shape
        /// recursion rationale.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #dispatcher_ident(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut crate::runtime::tape::Tape<()>,
        ) -> ::core::result::Result<
            crate::runtime::tape::TapeOffset,
            crate::runtime::tape::DtaError,
        > {
            #nonroot_ident(input, p, state, builder)
        }

        /// AW-V.W3.2 — value-position shape dispatcher. Called both at
        /// the grammar root and from Object / Array compound bodies.
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]`.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #nonroot_ident(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut crate::runtime::tape::Tape<()>,
        ) -> ::core::result::Result<
            crate::runtime::tape::TapeOffset,
            crate::runtime::tape::DtaError,
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
        ShapeTag::AltDispatch => "altdispatch",
        ShapeTag::None => "unknown",
    }
}

/// Emit the Alt-dispatch body for the root rule — byte-matches the
/// next non-whitespace byte and calls the corresponding branch shape
/// fn. Mirrors `json_prototype::parse_value`'s 6-arm match.
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
                Err(crate::runtime::tape::DtaError::InvalidState {
                state: crate::runtime::tape::DtaStateId::NONE,
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
    // AX.W0a.2.g — Keyword fn signature extended with `state: &mut
    // ScanState` so Ref-led Alt branches can delegate via
    // `emit_ref_call_tape`. Threading `state` here is a no-op for the
    // JSON true_arm / null_arm single-literal forms (they ignore the
    // argument via `_state`), and carries the Ref-branch delegation
    // path for grammars that admit Ref-led Keyword branches.
    let true_arm = keyword_bool_fn
        .as_ref()
        .map(|f| quote! { b't' | b'f' => { #f(input, p, first, state, builder) } })
        .unwrap_or_else(|| quote! {});
    let null_arm = keyword_null_fn
        .as_ref()
        .map(|f| quote! { b'n' => { #f(input, p, first, state, builder) } })
        .unwrap_or_else(|| quote! {});

    quote! {
        let first = #support_mod::skip_space(input, p, state)
            .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd { offset: *p as u32 })?;
        let __result = match first {
            #object_arm
            #array_arm
            #string_arm
            #number_arm
            #true_arm
            #null_arm
            c => {
                return ::core::result::Result::Err(
                    crate::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
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
/// The prototype's `json_prototype::parse_value::<V>` shape is the
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
            // AX.W0a.2.g — visitor-path Keyword signature extended with
            // `state` for Ref-branch delegation (see tape-path).
            ShapeTag::Number => quote! {
                let first = #support_mod::skip_space(input, p, state)
                    .ok_or(crate::runtime::ParseErr::Syntax {
                        offset: *p as u32, rule: None,
                    })?;
                #target_ident(input, p, first, visitor)
            },
            ShapeTag::Keyword => quote! {
                let first = #support_mod::skip_space(input, p, state)
                    .ok_or(crate::runtime::ParseErr::Syntax {
                        offset: *p as u32, rule: None,
                    })?;
                #target_ident(input, p, first, state, visitor)
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
            Err(crate::runtime::ParseErr::Syntax {
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
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]`.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #dispatcher_ident<V>(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            visitor: &mut V,
        ) -> ::core::result::Result<(), crate::runtime::ParseErr>
        where
            V: crate::runtime::tape::ObjectVisitor
                + crate::runtime::tape::ArrayVisitor
                + crate::runtime::tape::StringVisitor
                + crate::runtime::tape::NumberVisitor
                + crate::runtime::tape::KeywordVisitor,
        {
            #nonroot_ident(input, p, state, visitor)
        }

        /// AW-V.W3-bench-fix — value-position visitor-path dispatcher.
        /// Called both at the grammar root and from the object / array
        /// shape fns' value-position recursion.
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]`.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #nonroot_ident<V>(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            visitor: &mut V,
        ) -> ::core::result::Result<(), crate::runtime::ParseErr>
        where
            V: crate::runtime::tape::ObjectVisitor
                + crate::runtime::tape::ArrayVisitor
                + crate::runtime::tape::StringVisitor
                + crate::runtime::tape::NumberVisitor
                + crate::runtime::tape::KeywordVisitor,
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
        ShapeTag::AltDispatch => "altdispatch",
        ShapeTag::None => return None,
    };
    let target_fn = shape_fn_ident(shape_name, grammar_suffix, ir.get_string(target.name));
    let support_mod = support_mod_ident(grammar_suffix);
    // AX.W0a.2.s — Rules whose body admits whitespace as a leading
    // byte (CSS `combinator = /\s*>\s*/ | /\s*\+\s*/ | /\s*~\s*/ |
    // /\s+/`) handle whitespace internally. Pre-skipping at the Ref
    // call site erases the significant whitespace the rule needs —
    // `/\s+/` loses its match input and the descendant combinator
    // silently fails (bootstrap.css offset 8163, tailwind.css deep
    // selector sites). Suppress pre-skip when the target's leading
    // byte set includes any ASCII whitespace byte.
    let pre_skip_needed = !target_rule_accepts_leading_ws(&target.body, ir);
    // AX.W0a.2.g — Keyword's signature gained a `state` parameter so
    // Ref-led Alt branches can delegate via this helper. Number keeps
    // the legacy `(input, p, first, builder)` shape since its body
    // never recurses.
    let expr = match tag {
        ShapeTag::Number => quote! {
            {
                let __first = #support_mod::skip_space(input, p, state)
                    .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd {
                        offset: *p as u32,
                    })?;
                #target_fn(input, p, __first, builder)
            }
        },
        ShapeTag::Keyword => quote! {
            {
                let __first = #support_mod::skip_space(input, p, state)
                    .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd {
                        offset: *p as u32,
                    })?;
                #target_fn(input, p, __first, state, builder)
            }
        },
        _ if pre_skip_needed => quote! {
            {
                let _ = #support_mod::skip_space(input, p, state);
                #target_fn(input, p, state, builder)
            }
        },
        _ => quote! {
            {
                #target_fn(input, p, state, builder)
            }
        },
    };
    Some(expr)
}

/// Returns `true` if `body` can match an ASCII-whitespace byte as its
/// first input byte. Used by [`emit_ref_call_tape`] to suppress the
/// pre-skip ws step when the rule itself handles whitespace (CSS
/// `combinator`, where `/\s+/` IS a combinator branch).
///
/// Walks `Alt`, `OptionalWhitespace`, `Map`, and transparent `Seq` /
/// `Next` / `Skip` / `Repeat` structures to find the leading node in
/// each path. `Regex` nodes consult `ir.regex_info[sid].first_chars`;
/// `Literal` nodes check the first byte; `Ref` nodes recurse once
/// into the referenced rule.
fn target_rule_accepts_leading_ws(body: &bbnf_ir::IrNode, ir: &GrammarIR) -> bool {
    // Conservative recursion bound: `Ref` cycles are possible through
    // mutual recursion. One-level follow is sufficient for the CSS
    // `combinator`-shaped case; deeper chains fall back to the
    // existing pre-skip behaviour.
    target_rule_accepts_leading_ws_bounded(body, ir, 3)
}

fn target_rule_accepts_leading_ws_bounded(
    body: &bbnf_ir::IrNode,
    ir: &GrammarIR,
    budget: usize,
) -> bool {
    use bbnf_ir::IrNode;
    if budget == 0 {
        return false;
    }
    match body {
        IrNode::Literal(sid) => {
            let bytes = ir.strings[*sid as usize].as_bytes();
            bytes
                .first()
                .map(|&b| matches!(b, b' ' | b'\t' | b'\n' | b'\r' | 0x0C))
                .unwrap_or(false)
        }
        IrNode::Regex(sid) => {
            if let Some(info) = ir.regex_info.get(sid) {
                // Check whether any whitespace byte is in the
                // first-byte set of the pattern.
                info.first_chars.has(b' ')
                    || info.first_chars.has(b'\t')
                    || info.first_chars.has(b'\n')
                    || info.first_chars.has(b'\r')
            } else {
                false
            }
        }
        IrNode::Alt(branches, _) => branches
            .iter()
            .any(|b| target_rule_accepts_leading_ws_bounded(&b.node, ir, budget)),
        IrNode::Seq(children) => children
            .first()
            .map(|c| target_rule_accepts_leading_ws_bounded(c, ir, budget))
            .unwrap_or(false),
        IrNode::Next(a, _) | IrNode::Skip(a, _) => {
            target_rule_accepts_leading_ws_bounded(a, ir, budget)
        }
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            target_rule_accepts_leading_ws_bounded(inner, ir, budget)
        }
        IrNode::Repeat { inner, lo, .. } if *lo > 0 => {
            target_rule_accepts_leading_ws_bounded(inner, ir, budget)
        }
        IrNode::Ref(rid) => {
            let Some(target) = ir.rules.iter().find(|r| r.id == *rid) else {
                return false;
            };
            target_rule_accepts_leading_ws_bounded(&target.body, ir, budget - 1)
        }
        _ => false,
    }
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
        ShapeTag::AltDispatch => "altdispatch",
        ShapeTag::None => return None,
    };
    let target_fn =
        visitor_shape_fn_ident(shape_name, grammar_suffix, ir.get_string(target.name));
    let support_mod = support_mod_ident(grammar_suffix);
    // AX.W0a.2.g — visitor-path Keyword signature extended with
    // `state` (see tape-path emit_ref_call_tape).
    let expr = match tag {
        ShapeTag::Number => quote! {
            {
                let __first = #support_mod::skip_space(input, p, state)
                    .ok_or(crate::runtime::ParseErr::Syntax {
                        offset: *p as u32, rule: None,
                    })?;
                #target_fn(input, p, __first, visitor)
            }
        },
        ShapeTag::Keyword => quote! {
            {
                let __first = #support_mod::skip_space(input, p, state)
                    .ok_or(crate::runtime::ParseErr::Syntax {
                        offset: *p as u32, rule: None,
                    })?;
                #target_fn(input, p, __first, state, visitor)
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
                Err(crate::runtime::ParseErr::Syntax {
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
    // AX.W0a.2.g — visitor-path Keyword signature extended with
    // `state` (see tape-path emit_alt_dispatch_body).
    let true_arm = keyword_bool_fn
        .as_ref()
        .map(|f| quote! { b't' | b'f' => { #f(input, p, first, state, visitor) } })
        .unwrap_or_else(|| quote! {});
    let null_arm = keyword_null_fn
        .as_ref()
        .map(|f| quote! { b'n' => { #f(input, p, first, state, visitor) } })
        .unwrap_or_else(|| quote! {});

    quote! {
        let first = #support_mod::skip_space(input, p, state)
            .ok_or(crate::runtime::ParseErr::Syntax {
                offset: *p as u32, rule: None,
            })?;
        match first {
            #object_arm
            #array_arm
            #string_arm
            #number_arm
            #true_arm
            #null_arm
            _ => Err(crate::runtime::ParseErr::Syntax {
                offset: *p as u32, rule: None,
            }),
        }
    }
}

// === W0.e: structural-scan policy ===
//
// AY-II.W0.e — Grammar-activated structural-scan policy table.
//
// For each grammar the emitter produces a single module-scope const
// `STRUCTURAL_SCAN_POLICY: &[ScanPolicyEntry]` — one entry per non-
// transparent rule, populated from CSP-inferred FIRST-set facts
// intersected with the grammar's mined `structural_alphabet` +
// `structural_digraph_mask`.
//
// The const is consumed at emission time by the emitter's
// structural-scan-admitting shapes (`object_key_seek` inlining in
// `__path_walk`, `bounded_lookahead` in regex-scan adapters). There
// is no runtime flag and no hand-routed grammar specialisation —
// every decision resolves at codegen against grammar-derived facts.
//
// Schema (mirroring `crate::runtime::tape::ScanPolicyEntry`):
//
//   ScanPolicyEntry {
//       rule_id: u32,                       // IR RuleId
//       alphabet_class: ScanAlphabetClass,  // Empty / Sparse / Dense / Digraph
//       activation: ScanActivationFlags,    // bitmap of admitted primitives
//   }
//
// Sample entries (shape per grammar):
//
// - JSON `object`: class=Dense (FIRST ∩ alphabet = `{`, `:`, `,`, `}`
//   — 4 bytes), flags=OBJECT_KEY_SEEK | BOUNDED_LOOKAHEAD |
//   SCAN_STRUCTURAL_BOUNDED.
// - CSS L4 `declaration`: class=Dense (`:`, `;`, `/`), flags=
//   BOUNDED_LOOKAHEAD | SCAN_STRUCTURAL_BOUNDED | DIGRAPH_ADMIT
//   (comment digraph `/*`).
// - Sheets `cell_ref`: class=Sparse (`:` for range), flags=
//   BOUNDED_LOOKAHEAD.
// - BBNF `rule`: class=Digraph (digraph `->`, `(*`, `*)`), flags=
//   BOUNDED_LOOKAHEAD | DIGRAPH_ADMIT.

/// Emit the per-grammar `STRUCTURAL_SCAN_POLICY` const table — one
/// [`crate::runtime::tape::ScanPolicyEntry`] per non-transparent
/// rule, derived from FIRST-set facts + the grammar's mined
/// `structural_alphabet` + `structural_digraph_mask`.
///
/// The emitted stream lives at module scope alongside
/// `GRAMMAR_PROFILE`, immediately after the support module the
/// shape dispatcher wires in. Consumers (emitter shapes that admit
/// structural-scan primitives) index the slice by `rule_id` at
/// emission time — no runtime lookup.
///
/// Returns an empty stream when the grammar has no non-transparent
/// rules (i.e. every rule is a transparent alias).
pub fn emit_structural_scan_policy(
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    use crate::generate::regex::byte_class::classify_rule_alphabet;
    use tape::{ScanActivationFlags, ScanAlphabetClass};
    use bbnf_ir::IrNode;

    let profile = ir.profile();
    let structural_alphabet = profile.structural_alphabet.to_vec();
    let structural_digraph_mask = profile.structural_digraph_mask;

    // Build per-rule entries. We emit an entry for every non-
    // transparent rule so the consumer's `rule_id == x` probe is
    // uniform; rules with `Empty` class + no activation flags carry
    // `ScanPolicyEntry::EMPTY` semantics but distinct rule_ids.
    let mut entries: Vec<TokenStream> = Vec::new();
    for rule in &ir.rules {
        if rule.meta.is_transparent {
            continue;
        }

        // Materialise the rule's FIRST set as a byte slice.
        // `CharSet128::iter()` yields bytes in ascending order; the
        // classifier accepts an unsorted slice so the iteration
        // order is immaterial.
        let first_bytes: Vec<u8> = rule.meta.first_set.iter().collect();

        // A rule is "compound" for our purposes iff its body node
        // produces children the substrate emits as a compound record
        // (Seq / Alt / Repeat / top-level Rule / TokenDispatch).
        // Leaves (Literal / Regex / Epsilon / Ref-only / lookahead)
        // carry no children to scan.
        let is_compound = matches!(
            &rule.body,
            IrNode::Seq(_)
                | IrNode::Alt(_, _)
                | IrNode::Repeat { .. }
                | IrNode::TokenDispatch { .. }
        );

        let facts = classify_rule_alphabet(
            &first_bytes,
            &structural_alphabet,
            &structural_digraph_mask,
            is_compound,
        );

        // Derive the alphabet class from the intersection count +
        // digraph admission + compound-ness.
        let class = if facts.admits_digraph && !structural_alphabet.is_empty() {
            // Digraph-aware rules take precedence over dense — the
            // emitter needs the digraph-opener probe unconditionally
            // when the rule admits a multi-byte structural marker.
            ScanAlphabetClass::Digraph
        } else if facts.alphabet_intersection_count >= 4 {
            ScanAlphabetClass::Dense
        } else if facts.alphabet_intersection_count >= 1 {
            ScanAlphabetClass::Sparse
        } else {
            ScanAlphabetClass::Empty
        };

        // Derive activation flags from the class + is_compound.
        // Leaf rules never admit structural-scan primitives that
        // require children to walk.
        let mut flags: u8 = 0;
        if facts.is_compound {
            match class {
                ScanAlphabetClass::Dense => {
                    flags |= ScanActivationFlags::OBJECT_KEY_SEEK;
                    flags |= ScanActivationFlags::BOUNDED_LOOKAHEAD;
                    flags |= ScanActivationFlags::SCAN_STRUCTURAL_BOUNDED;
                }
                ScanAlphabetClass::Sparse => {
                    flags |= ScanActivationFlags::BOUNDED_LOOKAHEAD;
                }
                ScanAlphabetClass::Digraph => {
                    flags |= ScanActivationFlags::BOUNDED_LOOKAHEAD;
                    flags |= ScanActivationFlags::SCAN_STRUCTURAL_BOUNDED;
                    flags |= ScanActivationFlags::DIGRAPH_ADMIT;
                }
                ScanAlphabetClass::Empty => {}
            }
        }

        let class_tokens = match class {
            ScanAlphabetClass::Empty => {
                quote! { crate::runtime::tape::ScanAlphabetClass::Empty }
            }
            ScanAlphabetClass::Sparse => {
                quote! { crate::runtime::tape::ScanAlphabetClass::Sparse }
            }
            ScanAlphabetClass::Dense => {
                quote! { crate::runtime::tape::ScanAlphabetClass::Dense }
            }
            ScanAlphabetClass::Digraph => {
                quote! { crate::runtime::tape::ScanAlphabetClass::Digraph }
            }
        };

        let rule_id = rule.id;
        let flags_lit = proc_macro2::Literal::u8_unsuffixed(flags);
        entries.push(quote! {
            crate::runtime::tape::ScanPolicyEntry {
                rule_id: #rule_id,
                alphabet_class: #class_tokens,
                activation: crate::runtime::tape::ScanActivationFlags::from_bits(#flags_lit),
            }
        });
    }

    if entries.is_empty() {
        return quote! {};
    }

    let policy_ident = format_ident!("STRUCTURAL_SCAN_POLICY");
    let _ = grammar_suffix; // The const is module-scoped; name is grammar-agnostic.

    quote! {
        /// AY-II.W0.e — Grammar-activated structural-scan policy table.
        ///
        /// One entry per non-transparent rule, derived at codegen from
        /// CSP-inferred FIRST-set facts intersected with the grammar's
        /// mined `structural_alphabet` + `structural_digraph_mask`.
        /// Consumed at emission time by `emit_path_query_impls` in
        /// `backend::rust::view::value`, which inlines the matching
        /// cursor primitive in `__path_walk`'s per-`rule_kind()`
        /// dispatch:
        /// [`crate::runtime::tape::TapeCursor::object_key_seek`] /
        /// [`crate::runtime::tape::TapeCursor::bounded_lookahead`] /
        /// [`crate::runtime::tape::TapeCursor::scan_structural_bounded`]
        /// per the entry's `activation` bitmap.
        ///
        /// No runtime flag; no hand-routed grammar specialisation.
        /// AY-II.W0'.c retires the `#[allow(dead_code)]` that
        /// previously guarded this surface — the emitted grammar now
        /// carries a same-translation-unit consumer through
        /// `__path_walk`'s dispatch.
        pub const #policy_ident: &[crate::runtime::tape::ScanPolicyEntry] = &[
            #(#entries),*
        ];
    }
}

/// Look up a [`crate::runtime::tape::ScanPolicyEntry`] for a rule by
/// id within the `STRUCTURAL_SCAN_POLICY` slice — emission-time
/// helper that resolves during codegen so the generated call site
/// inlines the matching entry's class + activation bitmap without
/// a runtime search.
///
/// Returns `None` when the rule carries no structural-scan admission
/// (e.g. leaf rules, transparent aliases omitted from the policy
/// table).
pub fn lookup_scan_policy<'ir>(
    ir: &'ir GrammarIR,
    rule_id: bbnf_ir::RuleId,
) -> Option<(tape::ScanAlphabetClass, tape::ScanActivationFlags)> {
    use crate::generate::regex::byte_class::classify_rule_alphabet;
    use tape::{ScanActivationFlags, ScanAlphabetClass};
    use bbnf_ir::IrNode;

    let rule = ir.rules.iter().find(|r| r.id == rule_id)?;
    if rule.meta.is_transparent {
        return None;
    }

    let profile = ir.profile();
    let structural_alphabet = profile.structural_alphabet.to_vec();
    let structural_digraph_mask = profile.structural_digraph_mask;

    let first_bytes: Vec<u8> = rule.meta.first_set.iter().collect();
    let is_compound = matches!(
        &rule.body,
        IrNode::Seq(_)
            | IrNode::Alt(_, _)
            | IrNode::Repeat { .. }
            | IrNode::TokenDispatch { .. }
    );

    let facts = classify_rule_alphabet(
        &first_bytes,
        &structural_alphabet,
        &structural_digraph_mask,
        is_compound,
    );

    let class = if facts.admits_digraph && !structural_alphabet.is_empty() {
        ScanAlphabetClass::Digraph
    } else if facts.alphabet_intersection_count >= 4 {
        ScanAlphabetClass::Dense
    } else if facts.alphabet_intersection_count >= 1 {
        ScanAlphabetClass::Sparse
    } else {
        ScanAlphabetClass::Empty
    };

    let mut flags: u8 = 0;
    if facts.is_compound {
        match class {
            ScanAlphabetClass::Dense => {
                flags |= ScanActivationFlags::OBJECT_KEY_SEEK;
                flags |= ScanActivationFlags::BOUNDED_LOOKAHEAD;
                flags |= ScanActivationFlags::SCAN_STRUCTURAL_BOUNDED;
            }
            ScanAlphabetClass::Sparse => {
                flags |= ScanActivationFlags::BOUNDED_LOOKAHEAD;
            }
            ScanAlphabetClass::Digraph => {
                flags |= ScanActivationFlags::BOUNDED_LOOKAHEAD;
                flags |= ScanActivationFlags::SCAN_STRUCTURAL_BOUNDED;
                flags |= ScanActivationFlags::DIGRAPH_ADMIT;
            }
            ScanAlphabetClass::Empty => {}
        }
    }

    Some((class, ScanActivationFlags::from_bits(flags)))
}
