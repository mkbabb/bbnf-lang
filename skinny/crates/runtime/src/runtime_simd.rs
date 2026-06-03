//! Runtime-side bridge to the shared `bbnf-simd` structural primitives.
//!
//! Grammar-neutral by construction: every entry takes the alphabet / byte-set
//! as CALLER DATA (Lock 14 — the delimiter policy comes from the generated
//! grammar module, never hardcoded here). The shared aarch64 eq-set / bracket-
//! depth classifiers (`bbnf_simd::prim`) are the same kernels JSON rides; here
//! `count_top_level_commas` accelerates the CSS selector-list separator count
//! via the bracket-depth-interior mask — the count is consumed unchanged (speed
//! from the scan, never from dropping structure).
//!
//! The CSS structural scan's per-byte dispatch was lifted to the G6
//! structural-classification kernel (`bbnf_simd::prim::structural_classify_64`),
//! consumed by `runtime_scan::css_balanced_component_scan`; the former
//! inner-inert-skip (`find_css_significant`) and the dead comment-close
//! consumer (`find_comment_close`) are SUBSUMED/RETIRED.

use bbnf_simd::prim::{bracket_depth_mask_64, byte_class_from_eq_set_64};

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
