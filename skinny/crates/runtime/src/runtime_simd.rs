//! Runtime-side bridge to the shared `bbnf-simd` structural primitives.
//!
//! Grammar-neutral by construction: every entry takes the alphabet / byte-set
//! as CALLER DATA (Lock 14 — the delimiter policy comes from the generated
//! grammar module, never hardcoded here). The shared aarch64 eq-set classifier
//! (`bbnf_simd::prim::byte_class_from_eq_set_64`) is the same kernel JSON's
//! `scan_structurals` rides; here it accelerates the CSS structural scan's
//! inner inert-byte skip, producing ONLY the next-significant index — the tape
//! and rich projection consume that index unchanged (speed from the scan,
//! never from dropping structure).

use bbnf_simd::prim::{
    bracket_depth_mask_64, byte_class_from_eq_set_64, comment_body_mask_64, CommentCarry,
};

/// CSS bracket open/close sets (caller data for the L6 primitive).
const BRACKET_OPENS: [u8; 4] = [b'(', b'[', b'{', 0];
const BRACKET_CLOSES: [u8; 4] = [b')', b']', b'}', 0];
const BRACKET_LEN: usize = 3;

/// Count top-level commas (commas at bracket depth 0, outside strings) in a CSS
/// selector prelude — the selector-list separator count. The L6
/// `bracket_depth_mask_64` primitive yields the bracket-interior mask 64 bytes
/// at a time; a top-level comma is a comma byte whose bit is NOT in that mask.
/// Blocks containing a quote fall back to the scalar walk (string interiors are
/// not bracket-tracked); the i32 depth carry threads across blocks within this
/// one call. Bit-identical to the scalar reference (`count_top_level_commas_ref`).
#[inline]
pub fn count_top_level_commas(bytes: &[u8]) -> usize {
    let end = bytes.len();
    let mut commas = 0usize;
    let mut depth: i32 = 0;
    let mut cursor = 0usize;

    const QUOTES: &[u8] = b"\"'";
    const COMMA: &[u8] = b",";

    while cursor + 64 <= end {
        let block: &[u8; 64] = bytes[cursor..cursor + 64]
            .try_into()
            .expect("slice length fixed by loop guard");
        // If a quote falls in this block, the scalar string-skip semantics are
        // required; defer the whole block to the scalar walk below.
        if byte_class_from_eq_set_64(block, QUOTES) != 0 {
            break;
        }
        let (interior, depth_out) = bracket_depth_mask_64(
            block,
            &BRACKET_OPENS,
            BRACKET_LEN,
            &BRACKET_CLOSES,
            BRACKET_LEN,
            depth,
        );
        // Top-level commas: comma bits not inside any bracket group.
        let comma_mask = byte_class_from_eq_set_64(block, COMMA);
        commas += (comma_mask & !interior).count_ones() as usize;
        depth = depth_out;
        cursor += 64;
    }

    // Scalar tail / quote-bearing remainder — exact reference semantics.
    let mut pos = cursor;
    while pos < end {
        match bytes[pos] {
            b'(' | b'[' | b'{' => depth += 1,
            b')' | b']' | b'}' => depth = depth.saturating_sub(1),
            b'"' | b'\'' => {
                pos = skip_string_runtime(bytes, pos);
                continue;
            }
            b',' if depth == 0 => commas += 1,
            _ => {}
        }
        pos += 1;
    }
    commas
}

/// Skip a CSS string literal (mirrors the generated `skip_string`): `pos` is at
/// the opening quote; returns the index just past the closing quote.
#[inline]
fn skip_string_runtime(bytes: &[u8], pos: usize) -> usize {
    let quote = bytes[pos];
    let mut cursor = pos + 1;
    while cursor < bytes.len() {
        match bytes[cursor] {
            b'\\' => cursor += 2,
            byte if byte == quote => return cursor + 1,
            _ => cursor += 1,
        }
    }
    bytes.len()
}

/// CSS block-comment digraphs.
const COMMENT_OPEN: [u8; 2] = [b'/', b'*'];
const COMMENT_CLOSE: [u8; 2] = [b'*', b'/'];

/// Find the end of a CSS block comment that opens at `start` (`bytes[start..]`
/// begins with `/*`). Returns the index just past the closing `*/`, or `None`
/// if the comment is unterminated — exactly matching the scalar recognizer's
/// `consume_comment_at` contract.
///
/// The hot inner scan (locating `*/`) runs 64 bytes at a time through the L5
/// `comment_body_mask_64` primitive (NEON on aarch64, scalar reference
/// elsewhere; checkasm-parity gated). We enter already inside the comment
/// (past the `/*` open) and walk blocks until the carry leaves the comment;
/// the close digraph's `*/` is the last interior pair, so the return index is
/// one past the set close bit. A scalar tail closes the final partial window.
#[inline]
pub fn find_comment_close(bytes: &[u8], start: usize) -> Option<usize> {
    let end = bytes.len();
    // Begin scanning just past the `/*` open, already inside the comment.
    let mut cursor = start + 2;
    let mut carry = CommentCarry { in_comment: true, pending_half: false };

    while cursor + 64 <= end {
        let block: &[u8; 64] = bytes[cursor..cursor + 64]
            .try_into()
            .expect("slice length fixed by loop guard");
        let (mask, next) = comment_body_mask_64(block, COMMENT_OPEN, COMMENT_CLOSE, carry);
        // We entered this block inside the comment, so the mask is a leading
        // contiguous run of interior bits (bit 0 set). The FIRST clear bit is
        // the byte immediately after the FIRST `*/` close — i.e. the return
        // value, relative to the block. `(!mask).trailing_zeros()` yields:
        //   * `fc < 64` → the comment closed at `fc-1`; return `cursor + fc`.
        //   * `fc == 64` → the whole block is interior. Either the comment is
        //     still open (carry stays in_comment), or it closed exactly on the
        //     block's last two bytes (carry leaves the comment) → return
        //     `cursor + 64`. The carry disambiguates.
        let fc = (!mask).trailing_zeros() as usize;
        if fc < 64 {
            return Some(cursor + fc);
        }
        if !next.in_comment && !next.pending_half {
            return Some(cursor + 64);
        }
        carry = next;
        cursor += 64;
    }

    // Scalar tail: < 64 bytes remaining. `carry.pending_half` means the last
    // SIMD block ended on `*` (close[0]); a leading `/` here closes the comment.
    if carry.pending_half && cursor < end && bytes[cursor] == COMMENT_CLOSE[1] {
        return Some(cursor + 1);
    }
    let mut pos = cursor;
    while pos + 1 < end {
        if bytes[pos] == COMMENT_CLOSE[0] && bytes[pos + 1] == COMMENT_CLOSE[1] {
            return Some(pos + 2);
        }
        pos += 1;
    }
    None
}


/// Return the first index `≥ from` in `bytes` whose byte is structurally
/// significant for the CSS component scan: a member of `delimiters` (caller
/// data, ≤4) OR a member of `fixed` (the recognizer's opener/closer/quote/
/// slash family, ≤8). Inert runs are skipped 64 bytes at a time through the
/// shared NEON eq-set classifier; a scalar tail closes the final window.
///
/// The combined significant set may exceed the 8-byte eq-set contract, so two
/// classifier fans are OR-reduced — both sets are caller data, the kernel names
/// no grammar.
#[inline]
pub fn find_css_significant(
    bytes: &[u8],
    from: usize,
    delimiters: &[u8],
    fixed: &[u8; 9],
) -> usize {
    let end = bytes.len();
    if from >= end {
        return from;
    }

    // Split the ≤13-byte significant set into two ≤8 eq-set fans. `delimiters`
    // is ≤4; `fixed` is 9 → fixed_a (8) + fixed_b (1). Delimiters often overlap
    // `fixed` (e.g. `{`), which is harmless (membership is idempotent).
    debug_assert!(delimiters.len() <= 4);
    let mut set_a = [0u8; 8];
    let mut set_b = [0u8; 8];
    // set_a := first 8 of `fixed`; set_b := last byte of `fixed` + delimiters.
    set_a.copy_from_slice(&fixed[..8]);
    set_b[0] = fixed[8];
    let dn = delimiters.len().min(4);
    set_b[1..1 + dn].copy_from_slice(&delimiters[..dn]);
    let set_a: &[u8] = &set_a;
    let set_b: &[u8] = &set_b[..1 + dn];

    let mut cursor = from;
    while cursor + 64 <= end {
        let block: &[u8; 64] = bytes[cursor..cursor + 64]
            .try_into()
            .expect("slice length fixed by loop guard");
        let mask = byte_class_from_eq_set_64(block, set_a) | byte_class_from_eq_set_64(block, set_b);
        if mask != 0 {
            return cursor + mask.trailing_zeros() as usize;
        }
        cursor += 64;
    }

    // Scalar tail (< 64 bytes remaining).
    while cursor < end {
        let byte = bytes[cursor];
        if fixed.contains(&byte) || delimiters.contains(&byte) {
            return cursor;
        }
        cursor += 1;
    }
    cursor
}
