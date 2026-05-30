//! Scalar reference for `BRACKET_DEPTH_MASK_64` — the executable specification
//! AND the default body (the scalar running-balance, per the L6 binding
//! condition; the CTZ-ranges path is consumer-only + parity-gated, REDRESS-89).
//!
//! Contract:
//!   Input  — 64 source bytes + the open/close byte sets (caller data) + the
//!            i32 inter-block `depth_carry` (the bracket nesting depth at the
//!            block's first byte; init 0 per parse, never retained across
//!            parses — Lock 1 transient-producer clause).
//!   Output — a 64-bit mask where bit `i` is set iff `src[i]` lies at bracket
//!            depth ≥ 1, i.e. STRICTLY INSIDE a balanced bracket group. The
//!            opening bracket is NOT marked (depth at the open is the outer
//!            depth); the matching close IS marked (it sits at the inner
//!            depth before decrementing). Plus the `depth_carry` for the next
//!            block.
//!
//! Interior-marking makes the mask the exact set of positions a top-level
//! structural-delimiter scan must AND-NOT away: a delimiter nested inside
//! `()`/`[]`/`{}` is not a top-level component boundary.
//!
//! The open/close byte sets are caller data (CSS uses `([{` / `)]}`); the
//! kernel names no grammar (Lock 14).
//!
//! Citations:
//!   * Lock 16 (SOTA-BEAT-DESIGN, dav1d primitive-lift row): this scalar
//!     running-balance is BOTH the parity anchor and the default body; the
//!     NEON body only accelerates the per-byte open/close classification and
//!     must agree bit-for-bit.
//!   * The running-balance (depth accumulator) is the standard bracket-matching
//!     scan; threading the i32 carry within one call keeps it transient.
//!
//! Body status: source-of-truth implementation AND the default consumer body.

/// The maximum bracket-open / bracket-close set size (CSS: 3 each).
const SET_CAP: usize = 4;

/// Returns true iff `byte` is a member of the first `len` entries of `set`.
#[inline(always)]
fn set_contains(set: &[u8; SET_CAP], len: usize, byte: u8) -> bool {
    let mut k = 0;
    while k < len {
        if set[k] == byte {
            return true;
        }
        k += 1;
    }
    false
}

/// Scalar reference / default body for `BRACKET_DEPTH_MASK_64`.
///
/// `opens`/`closes` carry the open and close bracket bytes (the first
/// `open_len`/`close_len` entries are live). Returns `(mask, depth_out)` where
/// `mask` marks every byte at nesting depth ≥ 1 and `depth_out` is the depth
/// at the byte AFTER the block.
#[inline]
pub fn bracket_depth_mask_64_scalar(
    src: &[u8; 64],
    opens: &[u8; SET_CAP],
    open_len: usize,
    closes: &[u8; SET_CAP],
    close_len: usize,
    depth_in: i32,
) -> (u64, i32) {
    let mut mask: u64 = 0;
    let mut depth = depth_in;

    for i in 0..64 {
        let byte = src[i];
        if set_contains(opens, open_len, byte) {
            // The open sits at the OUTER depth; mark it only if already nested.
            if depth > 0 {
                mask |= 1u64 << i;
            }
            depth += 1;
        } else if set_contains(closes, close_len, byte) {
            // The close sits at the INNER depth (before decrement); mark it if
            // depth ≥ 1.
            if depth > 0 {
                mask |= 1u64 << i;
                depth -= 1;
            }
            // An unbalanced close at depth 0 is left to the recursive scanner's
            // error path; it is not marked and does not go negative.
        } else if depth > 0 {
            mask |= 1u64 << i;
        }
    }

    (mask, depth)
}

/// The number of bracket bytes a CSS-style set carries.
pub const CSS_BRACKET_SET_LEN: usize = 3;
