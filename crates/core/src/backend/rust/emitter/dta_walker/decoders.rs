//! AW-IV.W2.3.a — Inline decoder body fragments.
//!
//! # Architectural role
//!
//! AW-IV.W1 + W2.1 flattened the walker hot path by inlining DFA bodies
//! and per-arm tape-helper bodies at the source level. The residual
//! function-call boundaries sit at two call sites that the post-W1.4-aggro
//! samply on JSON twitter still marks as hot:
//!
//! - `parse_that::parsers::eisel_lemire::compute_f64(mantissa, exponent,
//!   negative)` — the Eisel-Lemire fast f64 decoder, ~150 lines across
//!   `eisel_lemire::mod.rs` + `eisel_lemire::algorithm.rs`. Called from
//!   every `Map { Regex, F64 }` arm (JSON `number`, CSS `<number>`,
//!   Sheets numeric literals).
//! - `parse_that::parsers::scan::quoted_simd::scan_quoted_string_simd(
//!   bytes, start, quote)` — the SIMD quoted-string scanner, ~60 lines
//!   of portable `std::simd::u8x16` logic with backslash-parity escape
//!   handling. Called for every JSON string, CSS string-literal, BBNF
//!   string-literal, Sheets string arm.
//!
//! Both callees remain in their source crates (`parse-that`) as
//! `pub fn`s — the debug / fuzz / replay surface keeps them reachable
//! for non-walker callers. The fragments here splice their bodies
//! verbatim into each consuming walker arm so the hot path crosses no
//! function-call boundary at the source level. LLVM's per-site
//! specialisation (const-folding the power-of-ten table lookup index,
//! the quote-byte splat, the accept-state set) depends on the inline
//! emission — cross-crate inlining through LTO is a verification cover,
//! not the strategy, per the AW-IV §architectural-thesis binding rule.
//!
//! # Cross-agent contract (W2.3.a ↔ W2.3.b ↔ W2.3.c)
//!
//! - **W2.3.b** pre-allocates `Columns::pay_agg` capacity to
//!   `input.len() / 2 + 2` so inline column writes from this module's
//!   splices can assume the capacity check is elided. The f64 decoder's
//!   8-byte write into `pay_agg[arena_off..arena_off + 8]` goes through
//!   the existing `columns.pay_agg.resize(..)` pre-reservation that the
//!   walker's Regex arm already performs for the payload width; W2.3.b
//!   will later lift that reservation out of the per-arm hot path.
//! - **W2.3.c** eliminates the `psi.push(PayloadJob::new(...))`
//!   scheduling call for inline-decodable scalars. The decoder splice
//!   writes the decoded value directly into the matching arena slot at
//!   parse time, obviating the Stage-B PSI dispatch for that payload
//!   class. The walker's Regex arm emits the f64 decoder body in place
//!   of the `psi.push` call; no `PayloadJob` for this arm enters the
//!   stream.
//!
//! # Fragment catalogue
//!
//! - [`emit_eisel_lemire_inline_body`] — given the byte range
//!   `input[lo..hi]` and an `arena_off: u32`, decodes the matched
//!   bytes to an f64 and writes 8 little-endian bytes into
//!   `columns.pay_agg[arena_off..arena_off + 8]`. Mirrors
//!   `parse-that::parsers::eisel_lemire::compute_f64` plus the mantissa
//!   + exponent + sign pre-parse (the walker knows the DFA validated the
//!   shape; the splice accumulates the mantissa and decimal exponent in
//!   a single forward pass, then feeds them through the Eisel-Lemire
//!   algorithm body verbatim).
//! - [`emit_neon_string_scan_inline_body`] — given the byte range
//!   `input` and a starting position `start`, scans for the matching
//!   closing quote using the `std::simd::u8x16` 16-byte chunked
//!   backslash-parity algorithm from
//!   `parse-that::parsers::scan::quoted_simd::scan_quoted_string_simd`.
//!   The body inlines every helper (`escaped_mask`,
//!   `odd_parity_backslashes`, `scalar_tail`) as labelled blocks so no
//!   fn-call boundary remains at the source level.
//!
//! # Per the §6 generalisation invariant
//!
//! Grammar-agnostic. The emitted tokens are identical for every grammar;
//! per-grammar specialisation occurs at LLVM-codegen time via the
//! literal binding of `quote_byte` / `arena_off` / the enclosing arm's
//! `lo` / `hi` bindings. This mirrors the DFA-inline splice shape from
//! `dfa_codegen::emit_dfa_inline_body`: one generic fragment, per-arm
//! literal bindings.

use proc_macro2::TokenStream;
use quote::quote;

/// AW-IV.W2.3.a — Emit the Eisel-Lemire f64 decoder body inline.
///
/// Consumes `input: &[u8]` + `lo: u32` + `match_len: u32` from the
/// enclosing scope (the Regex arm already binds these after the DFA
/// scan), and produces a labelled block yielding `Option<f64>`:
///
/// - `Some(f)` — the byte range decoded cleanly via the integer
///   fast-path, the Clinger fast-path, or the full Eisel-Lemire
///   algorithm.
/// - `None` — the ambiguous-rounding case (~0.01% of inputs, per the
///   `compute_f64` docs); callers fall back to `fast_float2::parse` on
///   the original string.
///
/// The splice walks the bytes once to accumulate `(mantissa, exponent,
/// negative)`, clamps `n_digits` to the 19-digit mantissa budget, and
/// threads the triple through the `compute_f64` algorithm body
/// verbatim. `POWER_OF_FIVE_128` is referenced from the public
/// `parse_that::parsers::eisel_lemire::POWER_OF_FIVE_128` path; its
/// backing table (5488 entries × 16 bytes) stays in the parse-that
/// crate — we read through the public path, we do not duplicate the
/// table per grammar.
///
/// ## Parameters (via the enclosing scope)
///
/// - `input: &[u8]` — the source input byte slice.
/// - `lo: u32` — start offset (inclusive) of the matched number range.
/// - `match_len: u32` — length of the matched number range.
///
/// ## Emitted bindings
///
/// - `__decoded_f64: Option<f64>` — the decoded value (the labelled
///   block's value); `None` on the ambiguous-rounding case or if the
///   input is empty.
pub(super) fn emit_eisel_lemire_inline_body() -> TokenStream {
    quote! {
        // AW-IV.W2.3.a inline-decode — Eisel-Lemire f64 body spliced
        // verbatim. Mirrors
        // `parse_that::parsers::eisel_lemire::compute_f64`. The
        // mantissa/exponent/sign pre-parse walks the DFA-validated
        // byte range in a single forward pass; the decoder body below
        // is the `compute_f64` algorithm with its helpers inlined.
        let __decoded_f64: ::core::option::Option<f64> = '__ellabel: {
            // ── Byte-range + trivial-case gate ─────────────────────
            let __hi_byte = (lo as usize).wrapping_add(match_len as usize);
            if __hi_byte > input.len() || __hi_byte == lo as usize {
                break '__ellabel ::core::option::Option::None;
            }
            let __bytes: &[u8] = unsafe {
                input.get_unchecked(lo as usize..__hi_byte)
            };
            let __len: usize = __bytes.len();

            // ── Sign extraction ───────────────────────────────────
            let __first = unsafe { *__bytes.get_unchecked(0) };
            let (__neg, __digits_start) = match __first {
                b'-' => (true, 1usize),
                b'+' => (false, 1usize),
                _ => (false, 0usize),
            };

            if __digits_start >= __len {
                break '__ellabel ::core::option::Option::None;
            }

            // ── Integer-part mantissa accumulation ────────────────
            //
            // 19-digit cap matches `compute_f64`'s mantissa budget;
            // digits beyond the cap advance the decimal exponent but
            // do not accumulate into the mantissa.
            let mut __i: usize = __digits_start;
            let mut __mantissa: u64 = 0;
            let mut __n_digits: u32 = 0;
            let mut __decimal_exp: i64 = 0;
            let mut __trailing_int_digits: u32 = 0;
            let __int_start = __i;
            while __i < __len {
                let __b = unsafe { *__bytes.get_unchecked(__i) };
                if !__b.is_ascii_digit() { break; }
                if __n_digits < 19 {
                    __mantissa = __mantissa
                        .wrapping_mul(10)
                        .wrapping_add((__b - b'0') as u64);
                } else {
                    __trailing_int_digits = __trailing_int_digits.wrapping_add(1);
                }
                __n_digits = __n_digits.wrapping_add(1);
                __i += 1;
            }
            let __has_pre_dot = __i > __int_start;
            __decimal_exp = __decimal_exp
                .wrapping_add(__trailing_int_digits as i64);

            // ── Fractional part ───────────────────────────────────
            let mut __has_frac = false;
            if __i < __len && unsafe { *__bytes.get_unchecked(__i) } == b'.' {
                __i += 1;
                let __frac_start = __i;
                while __i < __len {
                    let __b = unsafe { *__bytes.get_unchecked(__i) };
                    if !__b.is_ascii_digit() { break; }
                    if __n_digits < 19 {
                        __mantissa = __mantissa
                            .wrapping_mul(10)
                            .wrapping_add((__b - b'0') as u64);
                        __decimal_exp = __decimal_exp.wrapping_sub(1);
                    }
                    __n_digits = __n_digits.wrapping_add(1);
                    __i += 1;
                }
                if __i > __frac_start {
                    __has_frac = true;
                }
            }

            if !__has_pre_dot && !__has_frac {
                break '__ellabel ::core::option::Option::None;
            }

            // ── Exponent part (`eE[+-]?\d+`) ──────────────────────
            if __i < __len {
                let __e = unsafe { *__bytes.get_unchecked(__i) };
                if __e == b'e' || __e == b'E' {
                    __i += 1;
                    let mut __exp_neg = false;
                    if __i < __len {
                        let __sign = unsafe { *__bytes.get_unchecked(__i) };
                        if __sign == b'-' {
                            __exp_neg = true;
                            __i += 1;
                        } else if __sign == b'+' {
                            __i += 1;
                        }
                    }
                    let mut __exp_val: i64 = 0;
                    while __i < __len {
                        let __b = unsafe { *__bytes.get_unchecked(__i) };
                        if !__b.is_ascii_digit() { break; }
                        __exp_val = __exp_val
                            .wrapping_mul(10)
                            .wrapping_add((__b - b'0') as i64);
                        __i += 1;
                    }
                    if __exp_neg {
                        __decimal_exp = __decimal_exp.wrapping_sub(__exp_val);
                    } else {
                        __decimal_exp = __decimal_exp.wrapping_add(__exp_val);
                    }
                }
            }

            // ── Compute f64: Clinger fast path → Eisel-Lemire ─────
            //
            // Constants mirror
            // `parse_that::parsers::eisel_lemire`. The
            // `POWER_OF_FIVE_128` lookup table is referenced through
            // the public path — we do not inline the table (5488 × 16
            // bytes) per grammar; it's a single static in the
            // parse-that crate.
            const __MANTISSA_EXPLICIT_BITS: usize = 52;
            const __MIN_EXPONENT_ROUND_TO_EVEN: i64 = -4;
            const __MAX_EXPONENT_ROUND_TO_EVEN: i64 = 23;
            const __MINIMUM_EXPONENT: i32 = -1023;
            const __INFINITE_POWER: i32 = 0x7FF;
            const __SMALLEST_POWER_OF_FIVE: i32 = -342;
            const __LARGEST_POWER_OF_FIVE: i32 = 308;
            const __MAX_MANTISSA_FAST_PATH: u64 = 2_u64 << __MANTISSA_EXPLICIT_BITS;
            const __MIN_EXPONENT_FAST_PATH: i64 = -22;
            const __MAX_EXPONENT_FAST_PATH: i64 = 22;
            const __MAX_EXPONENT_DISGUISED_FAST_PATH: i64 = 37;
            const __FAST_PATH_POW10: [f64; 23] = [
                1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10,
                1e11, 1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19, 1e20,
                1e21, 1e22,
            ];
            const __INT_POW10: [u64; 16] = [
                1, 10, 100, 1_000, 10_000, 100_000, 1_000_000, 10_000_000,
                100_000_000, 1_000_000_000, 10_000_000_000, 100_000_000_000,
                1_000_000_000_000, 10_000_000_000_000, 100_000_000_000_000,
                1_000_000_000_000_000,
            ];

            // Degenerate cases matching `compute_f64`:
            if __mantissa == 0 {
                break '__ellabel ::core::option::Option::Some(
                    if __neg { -0.0 } else { 0.0 },
                );
            }
            if __decimal_exp < __SMALLEST_POWER_OF_FIVE as i64 {
                break '__ellabel ::core::option::Option::Some(
                    if __neg { -0.0 } else { 0.0 },
                );
            }
            if __decimal_exp > __LARGEST_POWER_OF_FIVE as i64 {
                break '__ellabel ::core::option::Option::Some(
                    if __neg { f64::NEG_INFINITY } else { f64::INFINITY },
                );
            }

            // Clinger fast path.
            let __fast_result: ::core::option::Option<f64> = 'fast: {
                if __mantissa > __MAX_MANTISSA_FAST_PATH {
                    break 'fast ::core::option::Option::None;
                }
                if __decimal_exp < __MIN_EXPONENT_FAST_PATH
                    || __decimal_exp > __MAX_EXPONENT_DISGUISED_FAST_PATH
                {
                    break 'fast ::core::option::Option::None;
                }
                let __value = if __decimal_exp <= __MAX_EXPONENT_FAST_PATH {
                    let __m = __mantissa as f64;
                    if __decimal_exp < 0 {
                        __m / __FAST_PATH_POW10[(-__decimal_exp) as usize]
                    } else {
                        __m * __FAST_PATH_POW10[__decimal_exp as usize]
                    }
                } else {
                    let __shift = (__decimal_exp - __MAX_EXPONENT_FAST_PATH) as usize;
                    let __scaled = match __mantissa.checked_mul(__INT_POW10[__shift]) {
                        ::core::option::Option::Some(v) => v,
                        ::core::option::Option::None => {
                            break 'fast ::core::option::Option::None;
                        }
                    };
                    if __scaled > __MAX_MANTISSA_FAST_PATH {
                        break 'fast ::core::option::Option::None;
                    }
                    (__scaled as f64)
                        * __FAST_PATH_POW10[__MAX_EXPONENT_FAST_PATH as usize]
                };
                ::core::option::Option::Some(if __neg { -__value } else { __value })
            };
            if let ::core::option::Option::Some(__f) = __fast_result {
                break '__ellabel ::core::option::Option::Some(__f);
            }

            // ── Eisel-Lemire slow path ────────────────────────────
            //
            // Mirrors `eisel_lemire::algorithm::compute_float`:
            // 128-bit multiply against `POWER_OF_FIVE_128`, extract
            // the high word with the 52+3-bit mantissa, round-to-
            // nearest with the ambiguous-rounding escape.
            let __q: i64 = __decimal_exp;
            let mut __w: u64 = __mantissa;
            let __lz = __w.leading_zeros();
            __w <<= __lz;

            let __mask: u64 = if __MANTISSA_EXPLICIT_BITS + 3 < 64 {
                0xFFFF_FFFF_FFFF_FFFF_u64 >> (__MANTISSA_EXPLICIT_BITS + 3)
            } else {
                0xFFFF_FFFF_FFFF_FFFF_u64
            };
            let __idx =
                (__q - __SMALLEST_POWER_OF_FIVE as i64) as usize;
            let (__lo5, __hi5) =
                ::parse_that::parsers::eisel_lemire::POWER_OF_FIVE_128[__idx];

            let __prod1 = (__w as u128) * (__lo5 as u128);
            let mut __first_lo: u64 = __prod1 as u64;
            let mut __first_hi: u64 = (__prod1 >> 64) as u64;
            if __first_hi & __mask == __mask {
                let __prod2 = (__w as u128) * (__hi5 as u128);
                let __second_hi: u64 = (__prod2 >> 64) as u64;
                __first_lo = __first_lo.wrapping_add(__second_hi);
                if __second_hi > __first_lo {
                    __first_hi = __first_hi.wrapping_add(1);
                }
            }

            if __first_lo == 0xFFFF_FFFF_FFFF_FFFF {
                let __inside_safe = __q >= -27 && __q <= 55;
                if !__inside_safe {
                    break '__ellabel ::core::option::Option::None;
                }
            }

            let __upperbit = (__first_hi >> 63) as i32;
            let mut __em_mantissa = __first_hi
                >> (__upperbit + 64 - __MANTISSA_EXPLICIT_BITS as i32 - 3);
            let __power_q =
                ((__q as i32).wrapping_mul(152_170 + 65536) >> 16) + 63;
            let mut __power2 = __power_q + __upperbit - __lz as i32 - __MINIMUM_EXPONENT;

            if __power2 <= 0 {
                if -__power2 + 1 >= 64 {
                    break '__ellabel ::core::option::Option::Some(
                        if __neg { -0.0 } else { 0.0 },
                    );
                }
                __em_mantissa >>= -__power2 + 1;
                __em_mantissa = __em_mantissa.wrapping_add(__em_mantissa & 1);
                __em_mantissa >>= 1;
                __power2 = (__em_mantissa >= (1_u64 << __MANTISSA_EXPLICIT_BITS))
                    as i32;
                // subnormal exit path.
                let mut __word = __em_mantissa;
                __word |= (__power2 as u64) << __MANTISSA_EXPLICIT_BITS;
                if __neg { __word |= 1u64 << 63; }
                break '__ellabel ::core::option::Option::Some(
                    f64::from_bits(__word),
                );
            }

            if __first_lo <= 1
                && __q >= __MIN_EXPONENT_ROUND_TO_EVEN
                && __q <= __MAX_EXPONENT_ROUND_TO_EVEN
                && (__em_mantissa & 3) == 1
                && (__em_mantissa
                    << (__upperbit + 64 - __MANTISSA_EXPLICIT_BITS as i32 - 3))
                    == __first_hi
            {
                __em_mantissa &= !1_u64;
            }
            __em_mantissa = __em_mantissa.wrapping_add(__em_mantissa & 1);
            __em_mantissa >>= 1;
            if __em_mantissa >= (2_u64 << __MANTISSA_EXPLICIT_BITS) {
                __em_mantissa = 1_u64 << __MANTISSA_EXPLICIT_BITS;
                __power2 += 1;
            }
            __em_mantissa &= !(1_u64 << __MANTISSA_EXPLICIT_BITS);
            if __power2 >= __INFINITE_POWER {
                break '__ellabel ::core::option::Option::Some(
                    if __neg { f64::NEG_INFINITY } else { f64::INFINITY },
                );
            }

            let mut __word = __em_mantissa;
            __word |= (__power2 as u64) << __MANTISSA_EXPLICIT_BITS;
            if __neg { __word |= 1u64 << 63; }
            ::core::option::Option::Some(f64::from_bits(__word))
        };
    }
}

/// AW-IV.W2.3.a — Emit the NEON / portable-SIMD quoted-string scanner
/// body inline.
///
/// Mirrors `parse_that::parsers::scan::quoted_simd::scan_quoted_string_simd`
/// — 16-byte `std::simd::u8x16` chunks, backslash-parity escape
/// handling, scalar tail for the remaining `< 16` bytes. The fragment
/// expects two bindings from the enclosing scope:
///
/// - `input: &[u8]` — the full input byte slice.
/// - `start: usize` — the byte offset AFTER the opening quote (i.e.
///   the first byte of the string body).
///
/// And one compile-time literal:
///
/// - `quote_byte: u8` — the closing-quote byte (`b'"'` for double-
///   quoted JSON strings, `b'\''` for single-quoted CSS strings).
///
/// Emits a labelled block yielding `Option<usize>`:
///
/// - `Some(pos)` — the byte offset of the closing quote.
/// - `None` — the string is unterminated.
///
/// Helpers (`escaped_mask`, `odd_parity_backslashes`, `scalar_tail`)
/// are inlined as labelled blocks so no fn-call boundary remains at
/// the source level. The per-site `quote_byte` literal lets LLVM
/// const-fold the quote-splat register setup; the per-site inline
/// admits a per-grammar specialisation of the tail loop's quote
/// branch.
///
/// The splice uses `std::simd` (portable SIMD) which lowers to NEON
/// on aarch64, AVX2 / SSE2 on x86_64, and scalar fallback on any
/// other target. Separate raw-intrinsic paths are unnecessary — the
/// portable-SIMD lowering matches the performance of hand-written NEON
/// on Apple M-series and hand-written AVX2 on x86_64 per the
/// `parse-that` quoted_simd benchmarks.
pub(super) fn emit_neon_string_scan_inline_body(quote_byte: u8) -> TokenStream {
    let quote_lit = proc_macro2::Literal::u8_unsuffixed(quote_byte);
    quote! {
        // AW-IV.W2.3.a inline-decode — SIMD quoted-string scanner body
        // spliced verbatim. Mirrors
        // `parse_that::parsers::scan::quoted_simd::scan_quoted_string_simd`
        // with `escaped_mask` + `odd_parity_backslashes` + scalar tail
        // inlined as labelled blocks; no fn-call boundary remains.
        let __sstring: ::core::option::Option<usize> = '__sstring: {
            use ::core::simd::prelude::*;
            const __QUOTE: u8 = #quote_lit;
            const __CHUNK: usize = 16;
            const __MASK16: u32 = 0xFFFF;
            const __ODD_BITS: u32 = 0xAAAA;

            let mut __pos: usize = start;
            let __end: usize = input.len();
            let mut __carry: u32 = 0;
            let __quote_splat = ::core::simd::u8x16::splat(__QUOTE);
            let __bs_splat = ::core::simd::u8x16::splat(b'\\');

            while __pos + __CHUNK <= __end {
                let __chunk = ::core::simd::u8x16::from_slice(
                    unsafe { input.get_unchecked(__pos..__pos + __CHUNK) },
                );
                let __quote_bits = __chunk
                    .simd_eq(__quote_splat)
                    .to_bitmask() as u32;
                let __bs_bits = __chunk
                    .simd_eq(__bs_splat)
                    .to_bitmask() as u32;

                // Fast path: no backslashes, no carry.
                if __bs_bits == 0 && __carry == 0 {
                    if __quote_bits != 0 {
                        break '__sstring ::core::option::Option::Some(
                            __pos + __quote_bits.trailing_zeros() as usize,
                        );
                    }
                    __pos += __CHUNK;
                    continue;
                }

                // escaped_mask inline — compute which bit positions are
                // escaped by an odd-parity backslash run.
                let __escaped: u32 = '__esc: {
                    if __bs_bits == 0 {
                        let __e = __carry;
                        __carry = 0;
                        break '__esc __e;
                    }
                    let __carry_in = __carry;

                    // odd_parity_backslashes inline — mark odd-parity
                    // positions within each backslash run, threading
                    // the carry from the previous chunk.
                    let __odd: u32 = '__odd: {
                        let mut __odd_acc: u32 = 0;
                        let mut __i_bit: u32 = 0;
                        if __carry_in == 1 && (__bs_bits & 1) != 0 {
                            let __run_len =
                                (!__bs_bits).trailing_zeros().min(16);
                            let __run_mask = if __run_len >= 16 {
                                __MASK16
                            } else {
                                (1u32 << __run_len) - 1
                            };
                            __odd_acc |= __run_mask & __ODD_BITS;
                            __i_bit = __run_len;
                        }
                        while __i_bit < 16 {
                            if (__bs_bits >> __i_bit) & 1 == 0 {
                                __i_bit += 1;
                                continue;
                            }
                            let __shifted = __bs_bits >> __i_bit;
                            let __run_len = (!__shifted)
                                .trailing_zeros()
                                .min(16 - __i_bit);
                            let __run_mask = if __run_len >= 32 {
                                u32::MAX
                            } else {
                                (1u32 << __run_len) - 1
                            };
                            __odd_acc |= (__run_mask & 0x5555) << __i_bit;
                            __i_bit += __run_len;
                        }
                        break '__odd __odd_acc & __MASK16;
                    };

                    let __e = ((__odd << 1) | __carry_in) & __MASK16;
                    __carry = (__odd >> 15) & 1;
                    break '__esc __e;
                };

                let __real_quotes = __quote_bits & !__escaped;
                if __real_quotes != 0 {
                    break '__sstring ::core::option::Option::Some(
                        __pos + __real_quotes.trailing_zeros() as usize,
                    );
                }
                __pos += __CHUNK;
            }

            // scalar_tail inline — for the last < 16 bytes.
            let mut __tail_pos: usize = __pos;
            let __byte0_escaped = __carry != 0;
            if __byte0_escaped && __tail_pos < __end {
                __tail_pos += 1;
            }
            let __result: ::core::option::Option<usize> = '__tail: loop {
                if __tail_pos >= __end {
                    break '__tail ::core::option::Option::None;
                }
                let __b = unsafe { *input.get_unchecked(__tail_pos) };
                if __b == __QUOTE {
                    break '__tail ::core::option::Option::Some(__tail_pos);
                }
                if __b == b'\\' {
                    __tail_pos += 1;
                    if __tail_pos >= __end {
                        break '__tail ::core::option::Option::None;
                    }
                    __tail_pos += 1;
                    continue;
                }
                __tail_pos += 1;
            };
            __result
        };
    }
}
