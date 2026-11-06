//! The `css_balanced_component_scan` named primitive (SK-V18 G2 §6, G6 redesign).
//!
//! This is the irreducible balanced-delimiter scan ALGORITHM that the CSS L4
//! recognizer is built from — the 94.1% scalar-scan hot leaf. It is
//! grammar-INVOKED (the generated CSS provider calls it by name) and
//! grammar-PARAMETERIZED: the STOP-set passed at each call site (`b";{}"`,
//! `b"{};"`, `b";}"`, `b":{};"`) is DERIVED by `codegen::lower::css_scan` from
//! the CSS grammar's `ruleBlock`/declaration/`atRuleBody` literal shapes; the
//! structural SKIP family (`( ) [ ] { } ' " /`) is derived from the selector
//! `()`/`[]`, the `ruleBlock` `{}`, the `cssString` quotes, and the `@ws`
//! comment. None of those byte-sets is a literal here — they arrive as caller
//! data, so a mutation of the invoking `.bbnf` rule changes the emitted ARG
//! byte-set without touching this kernel (the arg-mutation falsifier).
//!
//! NEUTRALITY (CH6/s6): the balanced-recognizer SHELL is CSS-EXERCISED-ONLY —
//! it returns offsets and EMITS NOTHING, structurally incompatible with the
//! parse-WITH-emit `{}`/`[]` descents of JSON/Sheets. The CSS-scoped name is
//! therefore FORCED, not contingent. The INNER structural-classification
//! sub-kernel (`bbnf_simd::prim::structural_classify_64`, the grammar-neutral
//! `vceqq` fan) stays neutral: it takes the significant byte-set + the kind
//! partition as CALLER DATA.
//!
//! G6 DISPATCH-TARGETING REDESIGN (simdjson stage-1). The per-significant-byte
//! dispatch (the byte LOAD + the `delimiters.contains` membership test + the
//! `match byte` kind dispatch — the attributed 79.5% self-time) is REPLACED by
//! ONE vectorized structural classification per 64-byte block:
//! `structural_classify_64` produces a `significant` bitmask plus per-kind masks
//! (`open`/`close`/`quote`/`slash`/`stop`). The shell CTZ-iterates `significant`
//! to the next structural position and reads its KIND from the masks by bit-test
//! — NO re-load of `bytes`, NO re-match. The shell still owns the structural
//! DECISIONS (recurse into a nested block, skip a string/comment, reject an
//! unexpected close); the redesign moves only the RECOGNIZE+DISPATCH-CLASSIFY
//! off the scalar critical path. The block classification is RE-DERIVED whenever
//! `pos` advances past the current block — at the block boundary or after a
//! recursion / string / comment jump (§3.3 of the redesign research).

use bbnf_simd::prim::{structural_classify_64, KindPartition};

/// A position-only scan error (the CSS provider maps it to its `CssFactError`).
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct ScanError {
    pub offset: usize,
    pub message: &'static str,
}

#[inline]
fn scan_error(offset: usize, message: &'static str) -> ScanError {
    ScanError { offset, message }
}

/// The structural SKIP alphabet for the balanced component scan: the fixed-9
/// significant-byte family the scan must stop on so it can dispatch on each.
/// These are the open/close pairs `( ) [ ] { }` (selector `()`/`[]` + `ruleBlock`
/// `{}`), the `cssString` quotes `' "`, and the comment-opening `/`. The kernel
/// treats this set as caller data; the membership is grammar-derived (see
/// `codegen::lower::css_scan::ScanAlphabet`).
pub type SkipAlphabet = [u8; 9];

/// The grammar-neutral kind buckets the structural classifier needs, derived
/// from the CSS SKIP family at the call boundary. The classifier names no
/// grammar byte; THESE are the caller data. (The `stop` bucket is the
/// per-call-site `delimiters`, threaded separately so the same partition shell
/// serves every STOP set.)
const OPEN_BYTES: &[u8] = b"([{";
const CLOSE_BYTES: &[u8] = b")]}";
const QUOTE_BYTES: &[u8] = b"'\"";
const SLASH_BYTES: &[u8] = b"/";

/// Build the caller-data kind partition for a given STOP set. `delimiters` is
/// the grammar-derived STOP family (≤4); the open/close/quote/slash buckets are
/// the SKIP family. (The `_skip` argument is the explicit provenance: the SKIP
/// alphabet is the source of the bracket/quote/slash buckets; it is named here
/// to keep the arg-mutation falsifier visible even though the buckets are the
/// stable structural family.)
#[inline]
fn kind_partition<'a>(delimiters: &'a [u8], _skip: &SkipAlphabet) -> KindPartition<'a> {
    KindPartition {
        open: OPEN_BYTES,
        close: CLOSE_BYTES,
        quote: QUOTE_BYTES,
        slash: SLASH_BYTES,
        stop: delimiters,
    }
}

/// The balanced-component scan kernel: advance from `start`, skipping balanced
/// `()`/`[]`/`{}`, strings, and comments, and returning the first `delimiters`
/// member found at the TOP level (or `None` at end of input). One vectorized
/// `structural_classify_64` per 64-byte block produces the significant bitmask +
/// per-kind masks; the shell CTZ-iterates the structurals and dispatches each
/// scalar-side. Behaviorally identical to the retired hand-written
/// `find_component_delim`.
#[inline]
pub fn css_balanced_component_scan(
    bytes: &[u8],
    pos: usize,
    delimiters: &[u8],
    skip: &SkipAlphabet,
) -> Result<Option<(u8, usize)>, ScanError> {
    let partition = kind_partition(delimiters, skip);
    scan_to_stop(bytes, pos, None, &partition, skip)
}

/// Consume a balanced component starting at the opener `start` (`bytes[start]`
/// is an open bracket), returning the index just past the matching `close`.
/// Recurses on nested `()`/`[]`/`{}`, skips strings/comments — the inner-recursive
/// half of the same scan machine. The inner scan stops on the `close` byte (plus
/// the structural family) using the SAME classify kernel; the close byte rides
/// the `stop` bucket as caller data.
#[inline]
pub fn css_consume_balanced(
    bytes: &[u8],
    start: usize,
    close: u8,
    skip: &SkipAlphabet,
) -> Result<usize, ScanError> {
    let close_set = [close];
    let partition = kind_partition(&close_set, skip);
    match scan_to_stop(bytes, start + 1, Some(close), &partition, skip)? {
        Some((_, stop_pos)) => Ok(stop_pos + 1),
        None => Err(scan_error(start, "unclosed component block")),
    }
}

/// The shared classify-then-iterate body for both the top-level scan and the
/// inner balanced consume.
///
/// `close_byte` distinguishes the two callers: `None` for the top-level scan
/// (an unexpected close `) ] }` is an ERROR; a returned STOP hands `(byte, pos)`
/// back); `Some(c)` for the inner consume (the `stop` bucket is exactly `{c}`,
/// and a STOP hit IS the matching close — the recursion target). In the inner
/// case a stray non-matching close still routes through the structural decision
/// (it recurses or errors per the scalar reference), preserving the original
/// `dispatch_structural` semantics exactly.
#[inline]
fn scan_to_stop(
    bytes: &[u8],
    mut pos: usize,
    close_byte: Option<u8>,
    partition: &KindPartition<'_>,
    skip: &SkipAlphabet,
) -> Result<Option<(u8, usize)>, ScanError> {
    let len = bytes.len();

    while pos < len {
        // Re-derive the block classification at the current `pos`. The cached
        // classification is valid only while `pos` stays inside this block; any
        // recursion / string / comment jump (or draining to the block end)
        // restarts the loop and re-classifies from the new `pos`.
        if pos + 64 <= len {
            let block_base = pos;
            let block: &[u8; 64] = bytes[block_base..block_base + 64]
                .try_into()
                .expect("64-byte block by loop guard");
            let c = structural_classify_64(block, partition);

            // Visit only the structurals at or after `pos` within this block:
            // mask off the bits below the entry offset (always 0 here, since we
            // classify FROM `pos`, but kept explicit for the boundary contract).
            let mut significant = c.significant;
            let mut jumped = false;
            while significant != 0 {
                let lane = significant.trailing_zeros() as usize;
                let bit = 1u64 << lane;
                let at = block_base + lane;

                if c.stop & bit != 0 {
                    // The membership test, now a bit-AND. Return the STOP hit.
                    return Ok(Some((bytes[at], at)));
                }
                if c.open & bit != 0 {
                    // Recurse into the nested balanced block; the close is the
                    // mate of THIS opener (read once, scalar-side).
                    let close = match bytes[at] {
                        b'(' => b')',
                        b'[' => b']',
                        _ => b'}', // b'{'
                    };
                    pos = css_consume_balanced(bytes, at, close, skip)?;
                    jumped = true;
                    break;
                }
                if c.quote & bit != 0 {
                    pos = consume_string(bytes, at)?;
                    jumped = true;
                    break;
                }
                if c.slash & bit != 0 {
                    if bytes.get(at + 1).copied() == Some(b'*') {
                        pos = consume_comment(bytes, at)?;
                        jumped = true;
                        break;
                    }
                    // A lone `/` not opening a comment: advance past it and keep
                    // draining the mask (the scalar `_ => pos + 1` arm).
                    significant &= significant - 1;
                    continue;
                }
                if c.close & bit != 0 {
                    // A close byte. In the inner consume the matching close is in
                    // the `stop` bucket and handled above; reaching here is an
                    // unexpected/non-matching close — an error at both callers
                    // (mirrors `dispatch_structural`'s close arm). The
                    // `close_byte` is the inner caller's mate; an unequal close
                    // is still rejected as unexpected.
                    let _ = close_byte;
                    return Err(scan_error(at, "unexpected closing delimiter"));
                }
                // Unreachable: significant ⊆ (stop|open|quote|slash|close).
                significant &= significant - 1;
            }

            if jumped {
                continue;
            }
            // No structural decision jumped `pos`: the whole block was inert or
            // every structural was a lone `/`. Advance past the block and
            // re-classify.
            pos = block_base + 64;
            continue;
        }

        // Scalar tail (< 64 bytes remaining): the exact per-byte reference.
        return scan_tail(bytes, pos, partition, skip);
    }
    Ok(None)
}

/// The scalar tail of `scan_to_stop`: the byte-exact per-byte reference for the
/// final < 64-byte window (where the vectorized block does not fit). Identical
/// decision logic to the block iterate, walked one byte at a time.
#[inline]
fn scan_tail(
    bytes: &[u8],
    mut pos: usize,
    partition: &KindPartition<'_>,
    skip: &SkipAlphabet,
) -> Result<Option<(u8, usize)>, ScanError> {
    let len = bytes.len();
    while pos < len {
        let byte = bytes[pos];
        if partition.stop.contains(&byte) {
            return Ok(Some((byte, pos)));
        }
        pos = dispatch_structural(bytes, pos, byte, skip)?;
    }
    Ok(None)
}

/// Dispatch on a structural byte (the scalar-tail decision, and the executable
/// reference the block-iterate mirrors): recurse into a nested balanced block,
/// skip a string/comment, or reject an unexpected close. A non-structural byte
/// (e.g. the comment-`/` that is not `/*`) advances by one.
#[inline]
fn dispatch_structural(
    bytes: &[u8],
    pos: usize,
    byte: u8,
    skip: &SkipAlphabet,
) -> Result<usize, ScanError> {
    let next = bytes.get(pos + 1).copied();
    let consumed = match byte {
        b'\'' | b'"' => consume_string(bytes, pos)?,
        b'/' if next == Some(b'*') => consume_comment(bytes, pos)?,
        b'(' => css_consume_balanced(bytes, pos, b')', skip)?,
        b'[' => css_consume_balanced(bytes, pos, b']', skip)?,
        b'{' => css_consume_balanced(bytes, pos, b'}', skip)?,
        b')' | b']' | b'}' => {
            return Err(scan_error(pos, "unexpected closing delimiter"));
        }
        _ => pos + 1,
    };
    Ok(consumed)
}

/// Skip a `/* ... */` comment beginning at `start`, returning the index past
/// `*/`.
#[inline]
fn consume_comment(bytes: &[u8], start: usize) -> Result<usize, ScanError> {
    let mut pos = start + 2;
    while pos + 1 < bytes.len() {
        if bytes[pos] == b'*' && bytes[pos + 1] == b'/' {
            return Ok(pos + 2);
        }
        pos += 1;
    }
    Err(scan_error(start, "unclosed comment"))
}

/// Skip a CSS string literal beginning at `start` (a quote byte), honouring
/// escapes and rejecting unescaped newlines. Returns the index past the close.
#[inline]
fn consume_string(bytes: &[u8], start: usize) -> Result<usize, ScanError> {
    let quote = bytes[start];
    let mut pos = start + 1;
    while pos < bytes.len() {
        match bytes[pos] {
            byte if byte == quote => return Ok(pos + 1),
            b'\\' => {
                pos += 1;
                if pos >= bytes.len() {
                    return Err(scan_error(start, "unclosed string"));
                }
                if bytes[pos] == b'\r' && bytes.get(pos + 1).copied() == Some(b'\n') {
                    pos += 2;
                } else {
                    pos += 1;
                }
            }
            b'\n' | b'\r' | 0x0c => {
                return Err(scan_error(pos, "unescaped newline in string"));
            }
            _ => pos += 1,
        }
    }
    Err(scan_error(start, "unclosed string"))
}
