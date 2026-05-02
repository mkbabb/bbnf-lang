//! AUTO-GENERATED from `[workspace.metadata.bbnf.grammars]` — do not edit manually.
//! Regenerate: cargo xtask regen --grammar math

#![allow(
    dead_code,
    unused_variables,
    unused_mut,
    unused_parens,
    unused_assignments,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    clippy::all
)]

use ::parse_that::*;

pub struct MathParser;
mod __mathparser_emit_impl {
    #![allow(
        dead_code,
        unused_variables,
        unused_mut,
        unused_parens,
        unused_assignments,
        non_camel_case_types,
        non_snake_case,
        non_upper_case_globals,
        clippy::all,
    )]
    use super::*;
    use ::parse_that::*;
    pub const GRAMMAR_MathParser: [&'static str; 1usize] = [
        include_str!(
            concat!(env!("CARGO_MANIFEST_DIR"), "/../../grammar/misc/math.bbnf")
        ),
    ];
    /// Grammar-local Pratt operator metadata.
    ///
    /// The dense LUT carries precedence, associativity, arity, and
    /// the two-byte flag. This sparse slice only carries the data
    /// needed to resolve ambiguous first bytes and stamp the
    /// grammar's operator discriminant.
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub struct PrattEntry {
        pub byte: u8,
        pub second_byte: ::core::option::Option<u8>,
        pub op_discriminant: u8,
    }
    /// AW-III.W6.5 — aggregate dense Pratt precedence LUT.
    ///
    /// Union of every Pratt rule's packed LUT (last-write-wins
    /// per byte). See
    /// `bbnf::backend::rust::emitter::precedence` for the bit
    /// layout.
    pub const PRECEDENCE_LUT: [u8; 256] = [
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
    ];
    /// AW-III.W6.5 — aggregate sparse Pratt metadata slice.
    ///
    /// Flat union of every rule's mined operator entries.
    pub const PRECEDENCE_ENTRIES: &[PrattEntry] = &[];
    /// AW-III.W6.5 — total mined operator count for this
    /// grammar. Non-zero iff the lift admitted ≥ 1 chain OR the
    /// shape classifier admitted ≥ 1 single-rung Pratt rule.
    pub const PRECEDENCE_OPERATOR_COUNT: usize = 0usize;
    /// AZ-IV.W3.3 — codegen-emitted lazy-parse path plan.
    ///
    /// The static `PATH_PLAN` carries one row per `(rule, segment
    /// kind)` decision the executor consults. The runtime cursor
    /// linearly searches the static for a matching `(rule_id,
    /// segment_kind)` pair and applies the recorded decision; a
    /// missing match falls back to `ParseFully` at the executor
    /// surface.
    ///
    /// `SegmentKind` and `Decision` re-export from
    /// `crate::path::cursor` — the runtime executor's canonical
    /// alphabet — so the plan rows and the cursor's decision
    /// vocabulary stay byte-identical without duplication.
    #[allow(dead_code)]
    pub mod __path_plan {
        pub use crate::path::cursor::{Decision, SegmentKind};
        #[derive(Clone, Copy, Debug)]
        pub struct PathPlanEntry {
            pub rule_id: u32,
            pub segment_kind: SegmentKind,
            /// Branch / position index when the decision is
            /// `ParseUntil`; `u32::MAX` otherwise.
            pub field_index: u32,
            pub decision: Decision,
        }
        pub const PATH_PLAN_LEN: usize = 1;
        pub static PATH_PLAN: &[PathPlanEntry; 1] = &[
            PathPlanEntry {
                rule_id: 0,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
        ];
        /// Linear search the plan for the first `(rule_id,
        /// segment_kind)` match. The W3.1 executor consults this
        /// fn through its cursor; `None` = fall back to
        /// `ParseFully` at the executor surface.
        #[inline]
        pub fn lookup(
            rule_id: u32,
            segment_kind: SegmentKind,
        ) -> ::core::option::Option<&'static PathPlanEntry> {
            let mut i = 0usize;
            while i < PATH_PLAN.len() {
                let entry = &PATH_PLAN[i];
                if entry.rule_id == rule_id
                    && entry.segment_kind as u8 == segment_kind as u8
                {
                    return ::core::option::Option::Some(entry);
                }
                i += 1;
            }
            ::core::option::Option::None
        }
    }
    static __DTA_REGEX_0: &str = "(\\d+)?(\\.\\d+)?([eE][-+]?\\d+)?";
    #[inline]
    #[cold]
    fn __regex_scan_MathParser(
        pattern: &str,
        input: &[u8],
        pos: usize,
    ) -> ::core::option::Option<u32> {
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_0.as_ptr())
            || pattern == __DTA_REGEX_0
        {
            return '__dfa: {
                let mut __dfa_state: u32 = 0;
                let mut __dfa_p: usize = pos;
                let mut __dfa_last_match: ::core::option::Option<u32> = ::core::option::Option::Some(
                    pos as u32,
                );
                loop {
                    let b = match input.get(__dfa_p) {
                        ::core::option::Option::Some(&b) => b,
                        ::core::option::Option::None => break,
                    };
                    match __dfa_state {
                        0 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 0;
                                }
                                46 => __dfa_state = 1,
                                69 | 101 => __dfa_state = 3,
                                _ => break,
                            }
                        }
                        1 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 4;
                                }
                                _ => break,
                            }
                        }
                        2 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 2;
                                }
                                _ => break,
                            }
                        }
                        3 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 2;
                                }
                                43 | 45 => __dfa_state = 5,
                                _ => break,
                            }
                        }
                        4 => {
                            match b {
                                69 | 101 => __dfa_state = 3,
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 4;
                                }
                                _ => break,
                            }
                        }
                        5 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 2;
                                }
                                _ => break,
                            }
                        }
                        _ => unsafe { ::core::hint::unreachable_unchecked() }
                    }
                    __dfa_p += 1;
                    match __dfa_state {
                        0 | 2 | 4 => {
                            __dfa_last_match = ::core::option::Option::Some(
                                __dfa_p as u32,
                            );
                        }
                        _ => {}
                    }
                }
                break '__dfa __dfa_last_match.map(|end| end - pos as u32);
            };
        }
        ::core::option::Option::None
    }
    /// AW-V.W3.2 — per-grammar shape-dispatch support.
    ///
    /// Inlined by every `parse_<shape>_<grammar>_<rule>` emitted
    /// sibling; carries the SIMD whitespace bitmap cache + the
    /// quoted-string scanner primitive. The module is private to
    /// the generated code — downstream consumers route through the
    /// top-level `parse_<grammar>_<root>` which inlines every
    /// helper under workspace LTO.
    #[allow(dead_code, non_snake_case)]
    pub(crate) mod __shape_support_MathParser {
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
        }
        impl ScanState {
            #[inline]
            pub fn new() -> Self {
                Self {
                    nospace_bits: 0,
                    nospace_start: -1,
                }
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
            match input.get(*p) {
                Some(&b) if b != b' ' && b != b'\t' && b != b'\n' && b != b'\r' => {
                    Some(b)
                }
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
            unsafe {
                return nospace_bitmap_64_neon(stripe);
            }
            #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
            unsafe {
                return nospace_bitmap_64_avx2(stripe);
            }
            #[allow(unreachable_code)] nospace_bitmap_64_scalar(stripe)
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
                let bits_lo: [u8; 16] = [
                    1, 2, 4, 8, 16, 32, 64, 128, 1, 2, 4, 8, 16, 32, 64, 128,
                ];
                let bit_vec = vld1q_u8(bits_lo.as_ptr());
                let m0 = chunk_ns_mask16(ptr, 0, space, tab, nl, cr, bit_vec);
                let m1 = chunk_ns_mask16(ptr, 16, space, tab, nl, cr, bit_vec);
                let m2 = chunk_ns_mask16(ptr, 32, space, tab, nl, cr, bit_vec);
                let m3 = chunk_ns_mask16(ptr, 48, space, tab, nl, cr, bit_vec);
                (m0 as u64) | ((m1 as u64) << 16) | ((m2 as u64) << 32)
                    | ((m3 as u64) << 48)
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
            unsafe {
                return first_quote_or_backslash_neon(bytes);
            }
            #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
            unsafe {
                return first_quote_or_backslash_avx2(bytes);
            }
            #[allow(unreachable_code)] first_quote_or_backslash_scalar(bytes)
        }
        #[cfg(target_arch = "aarch64")]
        #[inline(always)]
        unsafe fn first_quote_or_backslash_neon(bytes: &[u8]) -> Option<(usize, u8)> {
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
                    if b == b'"' || b == b'\\' {
                        return Some((i, b));
                    }
                    i += 1;
                }
                None
            }
        }
        #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
        #[inline(always)]
        unsafe fn first_quote_or_backslash_avx2(bytes: &[u8]) -> Option<(usize, u8)> {
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
                    if b == b'"' || b == b'\\' {
                        return Some((i, b));
                    }
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
                if b == b'"' || b == b'\\' {
                    return Some((i, b));
                }
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
        pub fn expect_keyword(input: &[u8], p: &mut usize, word: &[u8]) -> bool {
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
                            *p = i + 1;
                            return Ok(());
                        }
                        depth -= 1;
                    }
                    b'"' => {
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
    /// AZ-I.W2-act.B3 — per-grammar HRegex-shape parse function,
    /// **struct-direct body**.
    ///
    /// Runs the per-grammar regex scan, decodes the matched bytes
    /// per the rule's host-fn descriptor (HexConvert, NumberConvert,
    /// or Expr { Input, return_type }), and routes the decoded
    /// value through the StructBuilder trait. Returns
    /// unit for StructDirect composition with sibling
    /// shape fns under struct-direct mode.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_hregex_MathParser_number<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_MathParser::ScanState,
        builder: &mut crate::runtime::math::MathStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        let _ = cursor;
        let span_lo = *p as u32;
        let Some(match_len) = __regex_scan_MathParser(
            "(\\d+)?(\\.\\d+)?([eE][-+]?\\d+)?",
            input,
            *p,
        ) else {
            return Err(crate::runtime::DtaError::Syntax {
                offset: span_lo,
            });
        };
        *p += match_len as usize;
        let span_hi = *p as u32;
        <crate::runtime::math::MathStructBuilder<
            'p,
        > as crate::runtime::StructBuilder>::push_leaf_with_str(
            builder,
            core::str::from_utf8(&input[span_lo as usize..span_hi as usize])
                .unwrap_or(""),
        );
        Ok(())
    }
    /// AW-V.W3.2 — top-level shape dispatcher.
    ///
    /// Mirrors the walker's `value` rule ByteDispatch: skip leading
    /// whitespace, dispatch on the first byte to the chosen branch
    /// shape fn, return unit after the chosen shape succeeds. No outer Rule /
    /// Alt compound is pushed — the DTA's ByteDispatch state for
    /// `value` emits no compound either, and the target rule's Ref
    /// overwrites any `pending_variant_idx` en route, so the chosen
    /// rule's own compound carries the final root variant.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]` per cross-shape
    /// recursion rationale.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_MathParser_number<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_MathParser::ScanState,
        builder: &mut crate::runtime::math::MathStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        parse_MathParser_number__value(input, p, state, builder, cursor)
    }
    /// AW-V.W3.2 — value-position shape dispatcher. Called both at
    /// the grammar root and from Object / Array compound bodies.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_MathParser_number__value<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_MathParser::ScanState,
        builder: &mut crate::runtime::math::MathStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        let _ = __shape_support_MathParser::skip_space(input, p, state);
        let _ = cursor.decide(0u32);
        parse_hregex_MathParser_number(input, p, state, builder, cursor)
    }
    impl MathParser {
        /// Parse an input string and return the grammar-specific
        /// document that owns the StructDirect runtime arena.
        pub fn parse(
            input: &str,
        ) -> ::core::result::Result<
            crate::runtime::math::MathDocument<'_>,
            crate::runtime::ParseErr,
        > {
            let __input_bytes = input.as_bytes();
            let mut state = __shape_support_MathParser::ScanState::new();
            let mut builder = crate::runtime::math::MathStructBuilder::new();
            static __EAGER_EMPTY_PATH: ::std::sync::LazyLock<
                crate::path::ir::TypedPath<crate::path::markers::Json, &'static str>,
            > = ::std::sync::LazyLock::new(|| {
                crate::path::ir::TypedPath::from_owned(::std::vec::Vec::new())
            });
            let mut __eager_cursor: crate::path::cursor::PathCursor<
                'static,
                crate::path::ir::TypedPath<crate::path::markers::Json, &'static str>,
            > = crate::path::cursor::PathCursor::new(
                &*__EAGER_EMPTY_PATH,
                |_rid, _kind, _idx| crate::path::cursor::Decision::ParseFully,
            );
            {
                let mut pos: usize = 0;
                parse_MathParser_number(
                        __input_bytes,
                        &mut pos,
                        &mut state,
                        &mut builder,
                        &mut __eager_cursor,
                    )
                    .map_err(|e| match e {
                        crate::runtime::DtaError::Syntax { offset } => {
                            crate::runtime::ParseErr::Syntax {
                                offset,
                                rule: None,
                            }
                        }
                        crate::runtime::DtaError::UnexpectedEnd { offset } => {
                            crate::runtime::ParseErr::Syntax {
                                offset,
                                rule: None,
                            }
                        }
                        crate::runtime::DtaError::InvalidState { .. } => {
                            crate::runtime::ParseErr::Syntax {
                                offset: 0,
                                rule: None,
                            }
                        }
                    })?;
                let _ = __shape_support_MathParser::skip_space(
                    __input_bytes,
                    &mut pos,
                    &mut state,
                );
                if pos != input.len() {
                    return Err(crate::runtime::ParseErr::Syntax {
                        offset: pos as u32,
                        rule: None,
                    });
                }
            }
            ::core::result::Result::Ok(builder.finalise(input))
        }
    }
}
pub use __mathparser_emit_impl::*;
