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
//! - parse-that `ASCII_WHITESPACE_CLASS` — the shared Apple ARM NEON /
//!   browser Wasm SIMD whitespace classifier.
//! - `first_quote_or_backslash` — direct parse-that scanner call.
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
    matches!(
        classify_regex(pattern),
        RegexClass::WhitespaceWithBlockComment
    )
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
    if alphabet
        .iter()
        .any(|&b| b == b' ' || b == b'\t' || b == b'\n' || b == b'\r')
    {
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
            __ctns_idx.next_structural_at_or_after(*p as u32)
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
                let mask = ::parse_that::ASCII_WHITESPACE_CLASS
                    .nonmember_bitmap_64(stripe);
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
                    let mask = ::parse_that::ASCII_WHITESPACE_CLASS
                        .nonmember_bitmap_64(stripe);
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
/// When the grammar has a mined structural alphabet, ScanState
/// carries a lazy `OnceCell<StructuralIndex>` populated on first
/// per-parse query via `scan_structural(input, alphabet)`. Consumer
/// sites probe via `next_structural_at_or_after` to fast-skip to the
/// next structural delimiter when the in-stripe SIMD scan would
/// otherwise iterate byte-by-byte. Lazy-init keeps the cold-path cost
/// amortised; empty-alphabet grammars never pay the scan.
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
                ::simd_scan::StructuralIndex,
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
            /// against the grammar's mined structural alphabet.
            /// Idempotent; consumers may call freely.
            #[inline]
            pub(crate) fn ensure_structural_index<'a>(
                state: &'a mut ScanState,
                input: &[u8],
            ) -> &'a ::simd_scan::StructuralIndex {
                state.structural_index.get_or_init(|| {
                    let alphabet = ::simd_scan::StructuralAlphabet {
                        singletons: super::GRAMMAR_STRUCTURAL_ALPHABET,
                        digraph_mask: super::GRAMMAR_STRUCTURAL_DIGRAPH_MASK,
                        digraph_pairs: super::GRAMMAR_STRUCTURAL_DIGRAPHS,
                        quote_classes: super::GRAMMAR_STRUCTURAL_QUOTE_CLASSES,
                    };
                    ::simd_scan::scan_structural(input, &alphabet)
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

            /// Find the first `b'"'` or `b'\\'` byte in `bytes`.
            /// Scanner ownership remains in the canonical parse-that package.
            #[inline(always)]
            pub fn first_quote_or_backslash(bytes: &[u8]) -> Option<(usize, u8)> {
                ::parse_that::find_first_of(bytes, &[b'"', b'\\'])
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

            /// AZ-IV.W3-DYNAMIC — byte-balanced value skip for the
            /// lazy bail-out parser's mismatched-key fast path.
            ///
            /// Advances `*p` past one structural value (object,
            /// array, string, number, true / false / null,
            /// identifier-shaped scalar) without producing any
            /// builder push. The scan is a forward state machine:
            ///
            /// - `{` / `[` — track open/close depth (treating bytes
            ///   inside `"…"` strings as opaque) and stop at depth
            ///   zero with the matching close.
            /// - `"` — scan to the next unescaped `"`.
            /// - everything else — read until the next structural
            ///   delimiter (`,` `}` `]` whitespace).
            ///
            /// Returns `Err` only on premature EOF inside an
            /// unterminated string or compound; the lazy-error-
            /// elision contract ensures the caller never propagates
            /// that error.
            #[inline]
            pub fn byte_skip_value(
                input: &[u8],
                p: &mut usize,
            ) -> ::core::result::Result<(), crate::runtime::DtaError> {
                let start = *p;
                let first = match input.get(start).copied() {
                    Some(b) => b,
                    None => {
                        return Err(crate::runtime::DtaError::UnexpectedEnd {
                            offset: start as u32,
                        });
                    }
                };
                match first {
                    b'{' | b'[' => byte_skip_balanced(input, p),
                    b'"' => byte_skip_string(input, p),
                    _ => byte_skip_scalar(input, p),
                }
            }

            /// AZ-IV.W3-DYNAMIC — balanced-compound skip. Honours
            /// `"` strings (with `\"` escapes) so `}` / `]` bytes
            /// inside string literals do not falsely close.
            #[inline]
            fn byte_skip_balanced(
                input: &[u8],
                p: &mut usize,
            ) -> ::core::result::Result<(), crate::runtime::DtaError> {
                let start = *p;
                let mut depth: u32 = 0;
                let mut i = start;
                while let Some(&b) = input.get(i) {
                    match b {
                        b'{' | b'[' => depth = depth.saturating_add(1),
                        b'}' | b']' => {
                            if depth <= 1 {
                                // depth==1 means this is the matching
                                // close; depth==0 means a stray close
                                // (malformed) — both terminate the
                                // skip with `*p` advanced past the
                                // bracket. The lazy contract elides
                                // any downstream error.
                                *p = i + 1;
                                return Ok(());
                            }
                            depth -= 1;
                        }
                        b'"' => {
                            // Scan past closing quote, honouring `\"`.
                            i += 1;
                            while let Some(&sb) = input.get(i) {
                                if sb == b'\\' {
                                    i += 2;
                                    continue;
                                }
                                if sb == b'"' {
                                    break;
                                }
                                i += 1;
                            }
                            if input.get(i).is_none() {
                                return Err(crate::runtime::DtaError::UnexpectedEnd {
                                    offset: start as u32,
                                });
                            }
                        }
                        _ => {}
                    }
                    i += 1;
                }
                Err(crate::runtime::DtaError::UnexpectedEnd {
                    offset: start as u32,
                })
            }

            /// AZ-IV.W3-DYNAMIC — quoted-string skip. Advances past
            /// the closing `"` honouring `\"` and `\\` escapes.
            #[inline]
            fn byte_skip_string(
                input: &[u8],
                p: &mut usize,
            ) -> ::core::result::Result<(), crate::runtime::DtaError> {
                let start = *p;
                let mut i = start + 1;
                while let Some(&b) = input.get(i) {
                    if b == b'\\' {
                        i += 2;
                        continue;
                    }
                    if b == b'"' {
                        *p = i + 1;
                        return Ok(());
                    }
                    i += 1;
                }
                Err(crate::runtime::DtaError::UnexpectedEnd {
                    offset: start as u32,
                })
            }

            /// AZ-IV.W3-DYNAMIC — scalar skip. Advances past
            /// non-structural bytes until a delimiter (`,` `}` `]`
            /// whitespace) or EOF.
            #[inline]
            fn byte_skip_scalar(
                input: &[u8],
                p: &mut usize,
            ) -> ::core::result::Result<(), crate::runtime::DtaError> {
                let mut i = *p;
                while let Some(&b) = input.get(i) {
                    match b {
                        b',' | b'}' | b']' | b' ' | b'\t' | b'\n' | b'\r' => break,
                        _ => i += 1,
                    }
                }
                *p = i;
                Ok(())
            }
        }
    }
}
