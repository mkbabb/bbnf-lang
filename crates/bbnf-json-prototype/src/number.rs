//! JSON number kernel — inline integer scan + Eisel-Lemire decode.
//!
//! # Approach
//!
//! Per the AW-IV P1 profile, canada.json's `<f64 as FromStr>::from_str`
//! path pays 6.10% of parse time as a concatenated cost of the
//! dispatcher + the libcore `dec2flt::lemire::compute_float<f64>`
//! kernel. sonic-rs inlines its number path; the prototype matches
//! that shape by calling
//! [`parse_that::parsers::eisel_lemire::compute_f64`] with
//! pre-accumulated (mantissa, exponent, sign) triples, bypassing the
//! string re-read that `f64::from_str` imposes.
//!
//! The outer shape is a tight per-digit loop; when the accumulated
//! mantissa overflows `u64` or the digit count exceeds 19, we fall
//! through to `str::parse::<f64>` on the lexed slice. Eisel-Lemire's
//! own `None` return (ambiguous rounding, ~0.01%) routes into the
//! same fallback.
//!
//! # Attribution
//!
//! `parse_that::parsers::eisel_lemire::compute_f64` is the underlying
//! inline body. It is `#[inline]`; workspace LTO + codegen-units=1
//! resolve it into the prototype's `parse_number_body` at link time.
//! `nm` on the prototype bench binary shows no `compute_float` symbol
//! (parity with sonic-rs which also inlines its Eisel-Lemire kernel).

use parse_that::parsers::eisel_lemire::compute_f64;

use crate::{JsonVisitor, ParseError};

/// Pre-computed 10^n for n ∈ [0, 16]. Used by the SIMD digit
/// accumulator to shift previously-accumulated digits left by the
/// count of fresh digits just absorbed.
#[allow(dead_code)]
const POW10_U64: [u64; 17] = [
    1,
    10,
    100,
    1_000,
    10_000,
    100_000,
    1_000_000,
    10_000_000,
    100_000_000,
    1_000_000_000,
    10_000_000_000,
    100_000_000_000,
    1_000_000_000_000,
    10_000_000_000_000,
    100_000_000_000_000,
    1_000_000_000_000_000,
    10_000_000_000_000_000,
];

/// Parse a JSON number literal at `*p`, advancing `*p` past it.
///
/// Accumulates the mantissa as a `u64` and the decimal exponent as
/// an `i64` during a single linear scan. On the common short-mantissa
/// path, the accumulated values feed directly into `compute_f64`
/// without a string re-read. On overflow (`mantissa > 2^63`) or on
/// `compute_f64`'s ambiguous-rounding return, the function falls back
/// to `<f64 as FromStr>::from_str` on the literal's byte slice.
///
/// # SIMD digit scan (aarch64)
///
/// When the input has ≥ 16 bytes remaining AND the first 16 bytes
/// are all digits, [`simd_str2int`] accumulates up to 16 digits in
/// parallel per sonic-rs's NEON unzip-multiply-add chain. Canada's
/// 90% numeric workload sees this path fire on every full-precision
/// float literal.
///
/// Errors on empty digit runs after `-`, `.`, or `e`.
#[inline(always)]
pub(crate) fn parse_number_body<V: JsonVisitor>(
    input: &[u8],
    p: &mut usize,
    first_byte: u8,
    visitor: &mut V,
) -> Result<(), ParseError> {
    let start = *p;
    let len = input.len();

    // Sign — the caller already inspected `first_byte` from
    // `input[*p]` during dispatch; use that directly to avoid the
    // re-read.
    let negative = first_byte == b'-';
    if negative {
        *p += 1;
    }

    // Integer digits.
    let int_start = *p;
    let mut mantissa: u64 = 0;
    let mut many_digits = false;
    // SIMD fast path — absorb up to 16 digits in one NEON stripe.
    #[cfg(target_arch = "aarch64")]
    {
        while *p + 16 <= len {
            let (sum, count) =
                unsafe { simd_str2int(&input[*p..*p + 16], 16) };
            if count == 0 {
                break;
            }
            mantissa = mantissa.wrapping_mul(POW10_U64[count]).wrapping_add(sum);
            *p += count;
            if count < 16 {
                break;
            }
        }
    }
    while *p < len {
        let b = input[*p];
        if b.is_ascii_digit() {
            mantissa = mantissa.wrapping_mul(10).wrapping_add((b - b'0') as u64);
            *p += 1;
        } else {
            break;
        }
    }
    if *p == int_start {
        return Err(ParseError::InvalidNumber(start));
    }
    let int_digit_count = *p - int_start;
    if int_digit_count > 19 {
        many_digits = true;
    }

    // Fraction.
    let mut fractional_digit_count: i64 = 0;
    if input.get(*p) == Some(&b'.') {
        *p += 1;
        let frac_start = *p;
        #[cfg(target_arch = "aarch64")]
        {
            while *p + 16 <= len {
                let (sum, count) =
                    unsafe { simd_str2int(&input[*p..*p + 16], 16) };
                if count == 0 {
                    break;
                }
                mantissa =
                    mantissa.wrapping_mul(POW10_U64[count]).wrapping_add(sum);
                *p += count;
                if count < 16 {
                    break;
                }
            }
        }
        while *p < len {
            let b = input[*p];
            if b.is_ascii_digit() {
                mantissa = mantissa.wrapping_mul(10).wrapping_add((b - b'0') as u64);
                *p += 1;
            } else {
                break;
            }
        }
        fractional_digit_count = (*p - frac_start) as i64;
        if fractional_digit_count == 0 {
            return Err(ParseError::InvalidNumber(start));
        }
        if int_digit_count as i64 + fractional_digit_count > 19 {
            many_digits = true;
        }
    }

    // Exponent.
    let mut exponent: i64 = -fractional_digit_count;
    if matches!(input.get(*p), Some(b'e') | Some(b'E')) {
        *p += 1;
        let exp_negative = match input.get(*p) {
            Some(b'+') => {
                *p += 1;
                false
            }
            Some(b'-') => {
                *p += 1;
                true
            }
            _ => false,
        };
        let exp_start = *p;
        let mut exp_val: i64 = 0;
        while *p < len {
            let b = input[*p];
            if b.is_ascii_digit() {
                // Cap at a value that saturates well past the f64
                // range to avoid overflow on pathological inputs.
                exp_val = exp_val.saturating_mul(10).saturating_add((b - b'0') as i64);
                *p += 1;
            } else {
                break;
            }
        }
        if *p == exp_start {
            return Err(ParseError::InvalidNumber(start));
        }
        exponent += if exp_negative { -exp_val } else { exp_val };
    }

    // Decode.
    let end = *p;
    let bytes = &input[start..end];
    let value = if many_digits {
        // Fall back to the std path for long mantissas.
        parse_fallback(bytes)
    } else {
        match compute_f64(exponent, mantissa, negative) {
            Some(v) => v,
            None => parse_fallback(bytes),
        }
    };

    visitor
        .number_f64(value)
        .map_err(|_| ParseError::VisitorReject(start))
}

/// Fallback path for numbers Eisel-Lemire cannot decode — long
/// mantissas + ambiguous-rounding cases. Uses `<f64 as FromStr>`
/// against the input slice unchecked.
///
/// The byte slice is valid ASCII by construction (only digits, `-`,
/// `+`, `.`, `e`, `E`), so `from_utf8_unchecked` is sound.
#[inline(never)]
#[cold]
fn parse_fallback(bytes: &[u8]) -> f64 {
    // SAFETY: the byte slice originates from the input, and the parse
    // loop only advances past bytes in `[0-9] ∪ {'-', '+', '.', 'e', 'E'}`.
    let s = unsafe { std::str::from_utf8_unchecked(bytes) };
    s.parse::<f64>().unwrap_or(f64::NAN)
}

// ── SIMD digit accumulator (aarch64) ────────────────────────────────
//
// Ported from `sonic-number-0.1.2/src/arch/aarch64.rs:83–137` (MIT)
// under the same attribution clause. The kernel takes a 16-byte
// window and a `need` digit cap (≤ 16); returns `(sum, count)` where
// `sum` = parsed integer value of the first `count` digits, and
// `count` = digits actually consumed. Non-digit bytes terminate the
// scan at their position.
//
// Algorithm: per-byte subtract `b'0'`, find first non-digit via a
// `vcgtq_u8 > 9` compare + `vshrn_n_u16<4>` bitmask, then dispatch
// on `count` to one of the packadd cascades which unzips even/odd
// lanes and multiplies them by successive powers of 10 in parallel.

#[cfg(target_arch = "aarch64")]
macro_rules! packadd_1 {
    ($v:expr) => {{
        use core::arch::aarch64::*;
        let even = vuzp1q_u8($v, $v);
        let odd = vuzp2q_u8($v, $v);
        vaddw_u8(vmull_u8(vget_low_u8(even), vdup_n_u8(10)), vget_low_u8(odd))
    }};
}

#[cfg(target_arch = "aarch64")]
macro_rules! packadd_2 {
    ($v:expr) => {{
        use core::arch::aarch64::*;
        let even = vuzp1q_u16($v, $v);
        let odd = vuzp2q_u16($v, $v);
        vaddw_u16(vmull_n_u16(vget_low_u16(even), 100), vget_low_u16(odd))
    }};
}

#[cfg(target_arch = "aarch64")]
macro_rules! packadd_4 {
    ($v:expr) => {{
        use core::arch::aarch64::*;
        let even = vuzp1q_u32($v, $v);
        let odd = vuzp2q_u32($v, $v);
        vaddw_u32(vmull_n_u32(vget_low_u32(even), 10000), vget_low_u32(odd))
    }};
}

#[cfg(target_arch = "aarch64")]
macro_rules! simd_add_5_8 {
    ($v:ident, $count:literal) => {{
        use core::arch::aarch64::*;
        let shifted = vextq_u8::<{ 16 - $count }>(vdupq_n_u8(0), $v);
        let p1 = packadd_1!(shifted);
        let p2 = packadd_2!(p1);
        (vgetq_lane_u32::<2>(p2) as u64) * 10000 + (vgetq_lane_u32::<3>(p2) as u64)
    }};
}

#[cfg(target_arch = "aarch64")]
macro_rules! simd_add_8 {
    ($v:ident) => {{
        let p1 = packadd_1!($v);
        let p2 = packadd_2!(p1);
        packadd_4!(p2)
    }};
}

#[cfg(target_arch = "aarch64")]
macro_rules! simd_add_9_15 {
    ($v:ident, $count:literal) => {{
        use core::arch::aarch64::*;
        let shifted = vextq_u8::<{ 16 - $count }>(vdupq_n_u8(0), $v);
        let p4 = simd_add_8!(shifted);
        vgetq_lane_u64::<0>(p4) * 100000000 + vgetq_lane_u64::<1>(p4)
    }};
}

/// Parse up to `need` (≤ 16) leading decimal digits from `c`; return
/// `(sum, count)` where `count` is the number of digits actually
/// consumed and `sum` is their integer value.
///
/// SAFETY: caller must guarantee `c.len() >= 16`.
#[cfg(target_arch = "aarch64")]
#[inline(always)]
unsafe fn simd_str2int(c: &[u8], need: usize) -> (u64, usize) {
    use core::arch::aarch64::*;
    debug_assert!(need <= 16);
    debug_assert!(c.len() >= 16);
    unsafe {
        let data = vld1q_u8(c.as_ptr());
        let zero_char = vdupq_n_u8(b'0');
        let digits = vsubq_u8(data, zero_char);
        let gt_nine = vcgtq_u8(digits, vdupq_n_u8(9));
        let mask16 = vreinterpretq_u16_u8(gt_nine);
        let mask8 = vshrn_n_u16::<4>(mask16);
        let mask64 = vget_lane_u64::<0>(vreinterpret_u64_u8(mask8));

        let mut count = need;
        if mask64 != 0 {
            let parsed = (mask64.trailing_zeros() >> 2) as usize;
            if parsed < need {
                count = parsed;
            }
        }

        let sum = match count {
            0 => 0,
            1 => vgetq_lane_u8::<0>(digits) as u64,
            2 => {
                (vgetq_lane_u8::<0>(digits) as u64) * 10
                    + (vgetq_lane_u8::<1>(digits) as u64)
            }
            3 => {
                let shifted = vextq_u8::<13>(vdupq_n_u8(0), digits);
                let p1 = packadd_1!(shifted);
                (vgetq_lane_u16::<6>(p1) as u64) * 100
                    + (vgetq_lane_u16::<7>(p1) as u64)
            }
            4 => {
                let shifted = vextq_u8::<12>(vdupq_n_u8(0), digits);
                let p1 = packadd_1!(shifted);
                (vgetq_lane_u16::<6>(p1) as u64) * 100
                    + (vgetq_lane_u16::<7>(p1) as u64)
            }
            5 => simd_add_5_8!(digits, 5),
            6 => simd_add_5_8!(digits, 6),
            7 => simd_add_5_8!(digits, 7),
            8 => simd_add_5_8!(digits, 8),
            9 => simd_add_9_15!(digits, 9),
            10 => simd_add_9_15!(digits, 10),
            11 => simd_add_9_15!(digits, 11),
            12 => simd_add_9_15!(digits, 12),
            13 => simd_add_9_15!(digits, 13),
            14 => simd_add_9_15!(digits, 14),
            15 => simd_add_9_15!(digits, 15),
            16 => {
                let p = simd_add_8!(digits);
                vgetq_lane_u64::<0>(p) * 100000000
                    + vgetq_lane_u64::<1>(p)
            }
            _ => core::hint::unreachable_unchecked(),
        };
        (sum, count)
    }
}
