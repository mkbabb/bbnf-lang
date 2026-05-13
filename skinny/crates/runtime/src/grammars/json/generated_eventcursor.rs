//! SK-V3 Wave 2 prototype: mask-driven dispatch + jump-table value-class LUT.
//!
//! Goal: measure whether replacing `parse_value_at`'s scalar 7-arm match
//! with a 256-byte class lookup + mask-skip whitespace recovery validates the
//! 0.18-0.22 c/B projection on M5 Max (random.json target 18-22 GiB/s).
//!
//! Layout differences vs. `generated.rs`:
//!
//! * `attach_structural_index` materialises per-chunk u64 bitmaps for
//!   whitespace and value-byte positions via 64-byte NEON sweeps (`vceqq` +
//!   `vshrn` movemask, mirroring `bbnf-simd::aarch64::classify_tbl4`).
//! * `parse_value_at` reads `byte = bytes[cursor]`, indexes
//!   `VALUE_CLASS_LUT[byte]` (dense u4), and dispatches via a small jump
//!   table.  When the byte at `cursor` is whitespace, we look up the
//!   precomputed mask, `tzcnt` the next set bit, and resume.
//! * Helpers (`parse_object`, `parse_array`, `parse_string`, `parse_number`,
//!   `parse_literal`, `parse_pair`, `consume_*`) are shared with the legacy
//!   path — only the dispatch hub is rewritten so the measurement isolates
//!   the LUT + mask change.

use super::parser::ParserState;
use super::value::{ParseError, ParseErrorKind};
use crate::tape::OffsetFlags;
use parse_that_regex::{
    match_json_number_from_first, match_json_string_at_quote, skip_json_whitespace,
    RegexErrorKind,
};

// ---------------------------------------------------------------------------
// Class LUT — dense u4 per byte; 255 = invalid.
// ---------------------------------------------------------------------------

const CLASS_INVALID: u8 = 255;
const CLASS_OBJECT_OPEN: u8 = 0; // '{'
const CLASS_ARRAY_OPEN: u8 = 1; // '['
const CLASS_STRING: u8 = 2; // '"'
const CLASS_NUMBER: u8 = 3; // '-' or '0'..='9'
const CLASS_TRUE: u8 = 4; // 't'
const CLASS_FALSE: u8 = 5; // 'f'
const CLASS_NULL: u8 = 6; // 'n'

const VALUE_CLASS_LUT: [u8; 256] = build_value_class_lut();

const fn build_value_class_lut() -> [u8; 256] {
    let mut lut = [CLASS_INVALID; 256];
    lut[b'{' as usize] = CLASS_OBJECT_OPEN;
    lut[b'[' as usize] = CLASS_ARRAY_OPEN;
    lut[b'"' as usize] = CLASS_STRING;
    lut[b'-' as usize] = CLASS_NUMBER;
    let mut digit = b'0';
    while digit <= b'9' {
        lut[digit as usize] = CLASS_NUMBER;
        digit += 1;
    }
    lut[b't' as usize] = CLASS_TRUE;
    lut[b'f' as usize] = CLASS_FALSE;
    lut[b'n' as usize] = CLASS_NULL;
    lut
}

// ---------------------------------------------------------------------------
// Whitespace bitmap.  One u64 per 64-byte chunk; bit i set ⇔ byte i is JSON
// whitespace (' ' | '\t' | '\r' | '\n').  Trailing partial chunk is zero-padded.
// ---------------------------------------------------------------------------

const _: () = assert!(core::mem::size_of::<u64>() == 8);

#[cfg(target_arch = "aarch64")]
#[inline(always)]
unsafe fn movemask16(value: core::arch::aarch64::uint8x16_t) -> u16 {
    // vshrn_n_u16<4> + per-nibble OR collapse — the canonical NEON movemask
    // recipe (see `bbnf-simd::aarch64::movemask`).
    use core::arch::aarch64::*;
    let pairs = unsafe { vshrn_n_u16::<4>(vreinterpretq_u16_u8(value)) };
    let nibble_bits = unsafe { vand_u8(pairs, vdup_n_u8(0x11)) };
    let lane_bits = unsafe {
        vorr_u8(
            vand_u8(nibble_bits, vdup_n_u8(0x01)),
            vsri_n_u8::<3>(vdup_n_u8(0), nibble_bits),
        )
    };
    let widened = unsafe { vcombine_u8(lane_bits, vdup_n_u8(0)) };
    let interleaved = unsafe { vzip1q_u8(widened, widened) };
    let mut packed = [0u8; 16];
    unsafe { vst1q_u8(packed.as_mut_ptr(), interleaved) };
    let mut mask = 0u16;
    for pair in 0..8 {
        let bits = packed[pair * 2];
        mask |= u16::from(bits & 0x03) << (pair * 2);
    }
    mask
}

#[cfg(target_arch = "aarch64")]
#[inline(always)]
unsafe fn lane_ws_mask(chunk: core::arch::aarch64::uint8x16_t) -> u16 {
    use core::arch::aarch64::*;
    let space = unsafe { vceqq_u8(chunk, vdupq_n_u8(b' ')) };
    let tab = unsafe { vceqq_u8(chunk, vdupq_n_u8(b'\t')) };
    let lf = unsafe { vceqq_u8(chunk, vdupq_n_u8(b'\n')) };
    let cr = unsafe { vceqq_u8(chunk, vdupq_n_u8(b'\r')) };
    let ws = unsafe { vorrq_u8(vorrq_u8(space, tab), vorrq_u8(lf, cr)) };
    unsafe { movemask16(ws) }
}

#[cfg(target_arch = "aarch64")]
#[inline(always)]
unsafe fn whitespace_mask_chunk(ptr: *const u8) -> u64 {
    use core::arch::aarch64::*;
    let lanes = unsafe { vld1q_u8_x4(ptr) };
    let m0 = u64::from(unsafe { lane_ws_mask(lanes.0) });
    let m1 = u64::from(unsafe { lane_ws_mask(lanes.1) });
    let m2 = u64::from(unsafe { lane_ws_mask(lanes.2) });
    let m3 = u64::from(unsafe { lane_ws_mask(lanes.3) });
    m0 | (m1 << 16) | (m2 << 32) | (m3 << 48)
}

#[cfg(not(target_arch = "aarch64"))]
#[inline(always)]
unsafe fn whitespace_mask_chunk(ptr: *const u8) -> u64 {
    let mut mask = 0u64;
    for i in 0..64 {
        let byte = unsafe { *ptr.add(i) };
        let is_ws = matches!(byte, b' ' | b'\t' | b'\n' | b'\r');
        mask |= (is_ws as u64) << i;
    }
    mask
}

/// Build the per-chunk whitespace bitmap.  One u64 per 64 source bytes; the
/// final partial chunk is loaded via a 64-byte scratch buffer so the NEON
/// path stays branch-free.
fn build_ws_bitmap(bytes: &[u8]) -> Box<[u64]> {
    let n = bytes.len();
    let chunks = n / 64;
    let trailing = n % 64;
    let total_chunks = chunks + if trailing > 0 { 1 } else { 0 };
    let mut bitmap = vec![0u64; total_chunks].into_boxed_slice();

    let ptr = bytes.as_ptr();
    for i in 0..chunks {
        unsafe {
            *bitmap.get_unchecked_mut(i) = whitespace_mask_chunk(ptr.add(i * 64));
        }
    }
    if trailing > 0 {
        let mut scratch = [0u8; 64];
        scratch[..trailing].copy_from_slice(&bytes[chunks * 64..]);
        unsafe {
            *bitmap.get_unchecked_mut(chunks) = whitespace_mask_chunk(scratch.as_ptr());
        }
        // Mask out positions beyond `trailing` so they don't appear as ws.
        let valid_bits = if trailing == 64 { !0u64 } else { (1u64 << trailing) - 1 };
        bitmap[chunks] &= valid_bits;
    }

    bitmap
}

/// Find the next non-whitespace byte at or after `cursor` using the
/// precomputed bitmap.  Returns `bytes.len()` if none.
#[inline(always)]
fn skip_ws_mask(bytes: &[u8], bitmap: &[u64], cursor: usize) -> usize {
    let n = bytes.len();
    if cursor >= n {
        return n;
    }
    let chunk_idx = cursor / 64;
    let bit_idx = cursor % 64;
    let total_chunks = bitmap.len();

    // First chunk: shift mask right so bit 0 is the byte at `cursor`.
    // Non-ws bytes correspond to *zero* bits in the ws bitmap; we want the
    // first zero in the relevant tail.  The tail length is bounded by both
    // the chunk boundary (64 - bit_idx) and the remaining source length.
    let chunk_base = chunk_idx * 64;
    let src_tail = n - chunk_base; // 1..=64
    let in_chunk_width = (64 - bit_idx).min(src_tail - bit_idx);
    let remaining = unsafe { *bitmap.get_unchecked(chunk_idx) } >> bit_idx;
    let in_chunk_mask = if in_chunk_width == 64 {
        !0u64
    } else {
        (1u64 << in_chunk_width) - 1
    };
    let non_ws = (!remaining) & in_chunk_mask;
    if non_ws != 0 {
        let off = non_ws.trailing_zeros() as usize;
        return cursor + off;
    }
    if chunk_base + bit_idx + in_chunk_width == n {
        return n;
    }

    // Walk subsequent chunks until we find a non-ws bit.
    let mut next_chunk = chunk_idx + 1;
    while next_chunk < total_chunks {
        let chunk_base = next_chunk * 64;
        let chunk_src_len = (n - chunk_base).min(64);
        let chunk_mask = if chunk_src_len == 64 {
            !0u64
        } else {
            (1u64 << chunk_src_len) - 1
        };
        let m = unsafe { *bitmap.get_unchecked(next_chunk) };
        let non_ws = (!m) & chunk_mask;
        if non_ws != 0 {
            let off = non_ws.trailing_zeros() as usize;
            return chunk_base + off;
        }
        next_chunk += 1;
    }
    n
}

// ---------------------------------------------------------------------------
// Parser entry-point (eventcursor variant).
// ---------------------------------------------------------------------------

pub(crate) fn attach_structural_index(state: &mut ParserState<'_>) {
    let bitmap = build_ws_bitmap(state.bytes);
    state.ws_bitmap = Some(bitmap);
}

#[inline(always)]
pub(crate) fn parse_json<'i>(state: &mut ParserState<'i>) -> Result<(), ParseError<'i>> {
    parse_value(state)?;
    skip_ws_ec(state);
    if state.cursor != state.bytes.len() {
        return Err(error(state, ParseErrorKind::TrailingCharacters));
    }
    Ok(())
}

#[inline(always)]
fn parse_value<'i>(state: &mut ParserState<'i>) -> Result<(), ParseError<'i>> {
    skip_ws_ec(state);
    parse_value_at(state)
}

#[inline(always)]
fn parse_value_at<'i>(state: &mut ParserState<'i>) -> Result<(), ParseError<'i>> {
    if state.cursor >= state.bytes.len() {
        return Err(error(state, ParseErrorKind::ExpectedValue));
    }
    let byte = unsafe { *state.bytes.get_unchecked(state.cursor) };
    // LUT dispatch — LLVM emits a jump table over the dense class id.
    let class = unsafe { *VALUE_CLASS_LUT.get_unchecked(byte as usize) };
    match class {
        0 => parse_object(state),                                    // '{'
        1 => parse_array(state),                                     // '['
        2 => parse_string(state),                                    // '"'
        3 => parse_number(state, byte),                              // '-' | '0'..'9'
        4 => parse_literal(state, b"true"),                          // 't'
        5 => parse_literal(state, b"false"),                         // 'f'
        6 => parse_literal(state, b"null"),                          // 'n'
        _ => Err(error(state, ParseErrorKind::ExpectedValue)),
    }
}

#[inline(always)]
fn parse_object<'i>(state: &mut ParserState<'i>) -> Result<(), ParseError<'i>> {
    if consume_structural_ec(state, b'{').is_none() {
        return Err(error(state, ParseErrorKind::ExpectedValue));
    }
    skip_ws_ec(state);
    if consume_ec(state, b'}') {
        return Ok(());
    }
    loop {
        parse_pair(state)?;
        if consume_container_next_ec(state, b'}', ParseErrorKind::ExpectedCommaOrObjectEnd)? {
            continue;
        }
        return Ok(());
    }
}

#[inline(always)]
fn parse_pair<'i>(state: &mut ParserState<'i>) -> Result<(), ParseError<'i>> {
    parse_key_colon(state)?;
    parse_value_at(state)
}

#[inline(always)]
fn parse_key_colon<'i>(state: &mut ParserState<'i>) -> Result<(), ParseError<'i>> {
    let start = state.cursor;
    let Some(open_cursor) = consume_quote_at_cursor(state) else {
        return Err(error(state, ParseErrorKind::ExpectedValue));
    };
    if let Some(raw_end) = match_tiny_plain_string(state.bytes, start) {
        state.cursor = raw_end;
    } else {
        let span = match_json_string_at_quote(state.bytes, start).map_err(|err| ParseError {
            input: state.input,
            offset: err.offset,
            kind: match err.kind {
                RegexErrorKind::ExpectedString => ParseErrorKind::ExpectedValue,
                _ => ParseErrorKind::InvalidString,
            },
        })?;
        if span.needs_unescape {
            state.patch_flags(open_cursor, OffsetFlags::NONE.with(OffsetFlags::HAS_ESC));
        }
        state.cursor = span.raw_end;
    }
    let colon = if state.cursor < state.bytes.len()
        && unsafe { *state.bytes.get_unchecked(state.cursor) } == b':'
    {
        state.cursor
    } else {
        skip_ws_mask_at(state, state.cursor)
    };
    if colon >= state.bytes.len() || unsafe { *state.bytes.get_unchecked(colon) } != b':' {
        return Err(error(state, ParseErrorKind::ExpectedColon));
    }
    state.cursor = skip_ws_mask_at(state, colon + 1);
    Ok(())
}

#[inline(always)]
fn parse_array<'i>(state: &mut ParserState<'i>) -> Result<(), ParseError<'i>> {
    if consume_structural_ec(state, b'[').is_none() {
        return Err(error(state, ParseErrorKind::ExpectedValue));
    }
    skip_ws_ec(state);
    if consume_ec(state, b']') {
        return Ok(());
    }
    loop {
        parse_value_at(state)?;
        if consume_container_next_ec(state, b']', ParseErrorKind::ExpectedCommaOrArrayEnd)? {
            continue;
        }
        return Ok(());
    }
}

#[inline(always)]
fn parse_string<'i>(state: &mut ParserState<'i>) -> Result<(), ParseError<'i>> {
    let start = state.cursor;
    let Some(open_cursor) = consume_quote_at_cursor(state) else {
        return Err(error(state, ParseErrorKind::ExpectedValue));
    };
    if let Some(raw_end) = match_tiny_plain_string(state.bytes, start) {
        state.cursor = raw_end;
        return Ok(());
    }
    let span = match_json_string_at_quote(state.bytes, start).map_err(|err| ParseError {
        input: state.input,
        offset: err.offset,
        kind: match err.kind {
            RegexErrorKind::ExpectedString => ParseErrorKind::ExpectedValue,
            _ => ParseErrorKind::InvalidString,
        },
    })?;
    if span.needs_unescape {
        state.patch_flags(open_cursor, OffsetFlags::NONE.with(OffsetFlags::HAS_ESC));
    }
    state.cursor = span.raw_end;
    Ok(())
}

#[inline(always)]
fn match_tiny_plain_string(input: &[u8], offset: usize) -> Option<usize> {
    let mut cursor = offset + 1;
    let limit = (cursor + 8).min(input.len());
    while cursor < limit {
        match input[cursor] {
            b'"' => return Some(cursor + 1),
            b'\\' | 0x00..=0x1f => return None,
            _ => cursor += 1,
        }
    }
    None
}

#[inline(always)]
fn parse_number<'i>(state: &mut ParserState<'i>, first: u8) -> Result<(), ParseError<'i>> {
    let number = match_json_number_from_first(state.bytes, state.cursor, first)
        .ok_or_else(|| error(state, ParseErrorKind::InvalidNumber))?;
    state.emit_plain_offset(number.start);
    state.cursor = number.end;
    Ok(())
}

#[inline(always)]
fn parse_literal<'i>(
    state: &mut ParserState<'i>,
    literal: &'static [u8],
) -> Result<(), ParseError<'i>> {
    let start = state.cursor;
    if state.bytes.get(start..start + literal.len()) != Some(literal) {
        return Err(error(
            state,
            ParseErrorKind::InvalidLiteral(
                std::str::from_utf8(literal).expect("literal is UTF-8"),
            ),
        ));
    }
    state.emit_plain_offset(start);
    state.cursor += literal.len();
    Ok(())
}

#[inline(always)]
fn skip_ws_ec(state: &mut ParserState<'_>) {
    state.cursor = skip_ws_mask_at(state, state.cursor);
}

#[inline(always)]
fn skip_ws_mask_at(state: &ParserState<'_>, cursor: usize) -> usize {
    if let Some(bitmap) = state.ws_bitmap.as_deref() {
        skip_ws_mask(state.bytes, bitmap, cursor)
    } else {
        skip_json_whitespace(state.bytes, cursor)
    }
}

#[inline(always)]
fn consume_quote_at_cursor(state: &mut ParserState<'_>) -> Option<u32> {
    let offset = state.cursor;
    if offset >= state.bytes.len() || unsafe { *state.bytes.get_unchecked(offset) } != b'"' {
        return None;
    }
    let cursor = state.emit_plain_offset(offset);
    state.cursor = offset + 1;
    Some(cursor)
}

#[inline(always)]
fn consume_ec(state: &mut ParserState<'_>, byte: u8) -> bool {
    if matches!(byte, b':' | b',') {
        return consume_delimiter_ec(state, byte);
    }
    if matches!(byte, b'{' | b'}' | b'[' | b']' | b'"') {
        return consume_structural_ec(state, byte).is_some();
    }
    if state.bytes.get(state.cursor).copied() == Some(byte) {
        state.cursor += 1;
        true
    } else {
        false
    }
}

#[inline(always)]
fn consume_delimiter_ec(state: &mut ParserState<'_>, byte: u8) -> bool {
    let offset = if state.cursor < state.bytes.len()
        && unsafe { *state.bytes.get_unchecked(state.cursor) } == byte
    {
        state.cursor
    } else {
        skip_ws_mask_at(state, state.cursor)
    };
    if offset >= state.bytes.len() || unsafe { *state.bytes.get_unchecked(offset) } != byte {
        return false;
    }
    state.cursor = offset + 1;
    true
}

#[inline(always)]
fn consume_structural_ec(state: &mut ParserState<'_>, byte: u8) -> Option<u32> {
    let offset = if state.cursor < state.bytes.len()
        && unsafe { *state.bytes.get_unchecked(state.cursor) } == byte
    {
        state.cursor
    } else {
        skip_ws_mask_at(state, state.cursor)
    };
    if offset >= state.bytes.len() || unsafe { *state.bytes.get_unchecked(offset) } != byte {
        return None;
    }
    let cursor = state.emit_plain_offset(offset);
    state.cursor = offset + 1;
    Some(cursor)
}

#[inline(always)]
fn consume_container_next_ec<'i>(
    state: &mut ParserState<'i>,
    close: u8,
    error_kind: ParseErrorKind,
) -> Result<bool, ParseError<'i>> {
    let current = if state.cursor < state.bytes.len() {
        Some(unsafe { *state.bytes.get_unchecked(state.cursor) })
    } else {
        None
    };
    let offset = if current == Some(b',') || current == Some(close) {
        state.cursor
    } else {
        skip_ws_mask_at(state, state.cursor)
    };
    if offset >= state.bytes.len() {
        return Err(error(state, error_kind));
    }
    let byte = unsafe { *state.bytes.get_unchecked(offset) };
    if byte == b',' {
        state.cursor = skip_ws_mask_at(state, offset + 1);
        return Ok(true);
    }
    if byte == close {
        state.emit_plain_offset(offset);
        state.cursor = offset + 1;
        return Ok(false);
    }
    Err(error(state, error_kind))
}

#[cold]
#[inline(never)]
fn error<'i>(state: &ParserState<'i>, kind: ParseErrorKind) -> ParseError<'i> {
    ParseError {
        input: state.input,
        offset: state.cursor,
        kind,
    }
}
