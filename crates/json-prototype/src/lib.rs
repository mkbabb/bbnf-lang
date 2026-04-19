//! `bbnf-json-prototype` — JSON-only hand-tuned per-shape inline parser.
//!
//! # Role — AW-V.W2 (speed-ceiling validator)
//!
//! The AW-IV profile wave established that bbnf's JSON parse sits ~13×
//! behind sonic-rs not because the `bbnf-simd-scan` + `bbnf-tape`
//! substrate is deficient but because the consumer — the monolithic
//! `__dta_walker_inline::run` — is still an interpreter over a runtime
//! state table with cross-crate helper tails at every arm. This crate
//! inverts the consumer: five `#[inline(always)]` per-shape functions
//! (`parse_value` / `parse_object` / `parse_array` / `parse_string` /
//! `parse_number`) dispatched via the CPU stack, consuming the existing
//! SIMD + tape substrate directly.
//!
//! Per the AW-V invariant, the prototype lives in an isolated
//! worktree. If its 10%-of-sonic gate passes, the shape emerges as a
//! generalisation target for the forthcoming `shape_mining.rs` IR pass.
//! The *parse body* of each shape function is the codegen-regenerable
//! artefact — a hand prototype here, emitter-produced in AW-V.W3.
//!
//! # Design contract
//!
//! - Single entry: [`parse_json`] over a user-supplied visitor.
//! - Five shape functions, each `#[inline(always)]`, each monomorphised
//!   at the call site under [`parse_json::<V>`].
//! - Zero references to `dispatch_one` / `try_branch` /
//!   `advance_or_pop_with` / `DtaState` / `FrameStack`. Recursive
//!   descent over the CPU stack.
//! - Two shipped visitors — [`visitor::ValueVisitor`] (the sonic-parity
//!   validator, materialising into the crate-local [`value::Value`]
//!   enum) and [`visitor::TapeVisitor`] (the AW-IV substrate validator,
//!   emitting into `bbnf_tape::Columns` via `TapeBuilder`).
//! - Inline SIMD kernels consumed from `bbnf-simd-scan`'s `parity` +
//!   the crate-local [`simd`] module's `nospace64` bitmap cache +
//!   `first_quote_or_backslash` primitive.
//! - Inline Eisel-Lemire via
//!   `parse_that::parsers::eisel_lemire::compute_f64` for every number
//!   literal; fallback to `f64::from_str` only for the narrow
//!   ambiguous-rounding band the Eisel-Lemire algorithm returns `None`
//!   on.

#![warn(missing_docs)]

pub mod number;
pub mod simd;
pub mod string;
pub mod value;
pub mod visitor;

pub use value::{ArenaSpan, Document, NodeSpan, Number, StringSpan, Value, ARENA_TAG};
pub use visitor::{JsonVisitor, TapeVisitor, ValueVisitor};

use simd::ScanState;

/// Errors surfaced by [`parse_json`].
///
/// Every variant names a specific cycle-budget item per the prototype's
/// protocol: there is no `Other` / `Unknown` catch-all. The parser
/// early-exits on any one of these; the visitor never sees a call that
/// could produce a malformed sub-tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseError {
    /// Unexpected end-of-input at the named offset.
    Eof(usize),
    /// Trailing content after the root value at the named offset.
    Trailing(usize),
    /// Expected a string key at the named offset (opening `"`
    /// missing inside an object).
    ExpectKey(usize),
    /// Expected a `:` separator after a key at the named offset.
    ExpectColon(usize),
    /// Expected `,` or the matching close (`}` / `]`) at the named
    /// offset.
    ExpectCommaOrEnd(usize),
    /// Unexpected byte at the named offset during structural dispatch.
    UnexpectedByte(usize, u8),
    /// Expected a specific keyword (true / false / null) at the
    /// named offset; the byte sequence diverged.
    ExpectKeyword(usize),
    /// Unterminated string — opening `"` found but no matching close.
    UnterminatedString(usize),
    /// Invalid JSON-escape sequence within a string at the named
    /// offset.
    InvalidEscape(usize),
    /// Invalid number literal at the named offset — empty digit run
    /// after `-` or `.` / `e`.
    InvalidNumber(usize),
    /// Visitor rejected the event at the named offset (visitor-specific
    /// semantics; used by builder-style visitors for capacity limits).
    VisitorReject(usize),
}

impl core::fmt::Display for ParseError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            ParseError::Eof(p) => write!(f, "unexpected EOF at offset {}", p),
            ParseError::Trailing(p) => write!(f, "trailing content at offset {}", p),
            ParseError::ExpectKey(p) => write!(f, "expected string key at offset {}", p),
            ParseError::ExpectColon(p) => write!(f, "expected `:` at offset {}", p),
            ParseError::ExpectCommaOrEnd(p) => {
                write!(f, "expected `,` or close at offset {}", p)
            }
            ParseError::UnexpectedByte(p, b) => {
                write!(f, "unexpected byte {:#x} at offset {}", b, p)
            }
            ParseError::ExpectKeyword(p) => {
                write!(f, "expected keyword (true/false/null) at offset {}", p)
            }
            ParseError::UnterminatedString(p) => {
                write!(f, "unterminated string starting at offset {}", p)
            }
            ParseError::InvalidEscape(p) => {
                write!(f, "invalid escape sequence at offset {}", p)
            }
            ParseError::InvalidNumber(p) => {
                write!(f, "invalid number literal at offset {}", p)
            }
            ParseError::VisitorReject(p) => {
                write!(f, "visitor rejected event at offset {}", p)
            }
        }
    }
}

impl std::error::Error for ParseError {}

/// Parse a UTF-8 JSON document emitting events into `visitor`.
///
/// The visitor is monomorphised at this call site; the five shape
/// functions inline into the visitor's method bodies at compile time.
/// Per AW-V.W2, `nm` on the resulting bench binary shows zero
/// `dispatch_one` / `try_branch` / `advance_or_pop_with` /
/// `__dta_walker_inline` symbols; `cargo expand` shows each shape
/// function with its body inlined into `parse_json::<V>`.
///
/// # Contract
///
/// - `input` must be UTF-8. The parser does not re-validate it; the
///   SIMD kernels treat input as raw bytes and assume the content
///   between quotes is already valid (matches the `bbnf-tape`
///   decoder contract at `tape.rs:664` — the JSON-string body is
///   UTF-8-clean by construction).
/// - Whitespace handling matches RFC 8259: `{ \t, \n, \r, ' ' }`.
/// - Number parsing uses inline Eisel-Lemire; the narrow ambiguous-
///   rounding band (~0.01% of inputs per the algorithm's own docs)
///   falls back to `f64::from_str` internally.
/// - Visitors are monomorphised: no `dyn Visitor` dispatch. The two
///   shipped visitors ([`visitor::ValueVisitor`] +
///   [`visitor::TapeVisitor`]) cover the sonic-parity and
///   AW-IV-substrate bench lanes respectively.
#[inline]
pub fn parse_json<V: JsonVisitor>(
    input: &[u8],
    visitor: &mut V,
) -> Result<(), ParseError> {
    let mut p = 0usize;
    let mut state = ScanState::new();
    let first = skip_space(input, &mut p, &mut state)
        .ok_or(ParseError::Eof(0))?;
    parse_value(input, &mut p, &mut state, first, visitor)?;
    let _ = skip_space(input, &mut p, &mut state);
    if p != input.len() {
        return Err(ParseError::Trailing(p));
    }
    Ok(())
}

/// Skip JSON whitespace (`{space, \t, \n, \r}`) starting at `*p`
/// and return the first non-whitespace byte (or `None` on EOF).
///
/// Advances `*p` to the position of the returned byte (or to
/// `input.len()` on EOF). Mirrors sonic-rs's `skip_space` at
/// `parse.rs:1205–1240` — returning the first byte fuses the skip
/// with the caller's dispatch, sparing one redundant `input.get(*p)`
/// per call site.
///
/// Uses a 64-byte cached whitespace bitmap via [`ScanState`];
/// on inputs with short or zero whitespace runs the fast-exit path
/// skips SIMD entirely.
#[inline(always)]
pub(crate) fn skip_space(
    input: &[u8],
    p: &mut usize,
    state: &mut ScanState,
) -> Option<u8> {
    // Fast exit when the next byte is non-whitespace — covers 100%
    // of citm's structural runs and ~90% of twitter's.
    match input.get(*p) {
        Some(&b) if !matches!(b, b' ' | b'\t' | b'\n' | b'\r') => Some(b),
        None => None,
        _ => {
            simd::skip_space_slow(input, p, state);
            input.get(*p).copied()
        }
    }
}

/// Dispatch one JSON value at `*p` to the appropriate shape function.
///
/// `first_byte` is the dispatch byte the caller already consumed
/// from `input[*p]` (typically via [`skip_space`]'s return). Avoids
/// the redundant re-read inside parse_value.
///
/// The initial byte determines the shape:
///
/// - `{` → [`parse_object`]
/// - `[` → [`parse_array`]
/// - `"` → [`parse_string`]
/// - `-` | `0..=9` → [`parse_number`]
/// - `t` | `f` | `n` → keyword path (true/false/null)
#[inline(always)]
pub(crate) fn parse_value<V: JsonVisitor>(
    input: &[u8],
    p: &mut usize,
    state: &mut ScanState,
    first_byte: u8,
    visitor: &mut V,
) -> Result<(), ParseError> {
    let at = *p;
    match first_byte {
        b'{' => {
            *p += 1;
            parse_object(input, p, state, visitor)
        }
        b'[' => {
            *p += 1;
            parse_array(input, p, state, visitor)
        }
        b'"' => parse_string(input, p, state, visitor, /* is_key = */ false),
        b'-' | b'0'..=b'9' => parse_number(input, p, first_byte, visitor),
        b't' => {
            expect_keyword(input, p, b"true")?;
            visitor
                .bool(true)
                .map_err(|_| ParseError::VisitorReject(at))
        }
        b'f' => {
            expect_keyword(input, p, b"false")?;
            visitor
                .bool(false)
                .map_err(|_| ParseError::VisitorReject(at))
        }
        b'n' => {
            expect_keyword(input, p, b"null")?;
            visitor.null().map_err(|_| ParseError::VisitorReject(at))
        }
        c => Err(ParseError::UnexpectedByte(at, c)),
    }
}

/// Parse a JSON object — `{` already consumed by the dispatcher.
///
/// Mirrors `sonic-rs-0.3.17/src/parser.rs:417–446`. One inner loop:
/// expect key → `:` → value → `,` or `}`. Fast-empty check (`}` right
/// after `{`) avoids the single-key warmup on empty objects which are
/// common in twitter.json.
#[inline(always)]
pub(crate) fn parse_object<V: JsonVisitor>(
    input: &[u8],
    p: &mut usize,
    state: &mut ScanState,
    visitor: &mut V,
) -> Result<(), ParseError> {
    let begin_at = *p;
    visitor
        .begin_object()
        .map_err(|_| ParseError::VisitorReject(begin_at))?;
    let mut cur = skip_space(input, p, state);
    if cur == Some(b'}') {
        *p += 1;
        return visitor
            .end_object()
            .map_err(|_| ParseError::VisitorReject(*p));
    }
    loop {
        if cur != Some(b'"') {
            return Err(ParseError::ExpectKey(*p));
        }
        parse_string(input, p, state, visitor, /* is_key = */ true)?;
        if skip_space(input, p, state) != Some(b':') {
            return Err(ParseError::ExpectColon(*p));
        }
        *p += 1;
        let val_first = skip_space(input, p, state)
            .ok_or(ParseError::Eof(*p))?;
        parse_value(input, p, state, val_first, visitor)?;
        match skip_space(input, p, state) {
            Some(b'}') => {
                *p += 1;
                return visitor
                    .end_object()
                    .map_err(|_| ParseError::VisitorReject(*p));
            }
            Some(b',') => {
                *p += 1;
                cur = skip_space(input, p, state);
            }
            _ => return Err(ParseError::ExpectCommaOrEnd(*p)),
        }
    }
}

/// Parse a JSON array — `[` already consumed by the dispatcher.
///
/// Mirrors `sonic-rs-0.3.17/src/parser.rs:390–415`. Fast-empty check
/// (`]` right after `[`) avoids the single-item warmup on empty arrays.
#[inline(always)]
pub(crate) fn parse_array<V: JsonVisitor>(
    input: &[u8],
    p: &mut usize,
    state: &mut ScanState,
    visitor: &mut V,
) -> Result<(), ParseError> {
    let begin_at = *p;
    visitor
        .begin_array()
        .map_err(|_| ParseError::VisitorReject(begin_at))?;
    let mut first = skip_space(input, p, state)
        .ok_or(ParseError::Eof(*p))?;
    if first == b']' {
        *p += 1;
        return visitor
            .end_array()
            .map_err(|_| ParseError::VisitorReject(*p));
    }
    loop {
        parse_value(input, p, state, first, visitor)?;
        match skip_space(input, p, state) {
            Some(b']') => {
                *p += 1;
                return visitor
                    .end_array()
                    .map_err(|_| ParseError::VisitorReject(*p));
            }
            Some(b',') => {
                *p += 1;
                first = skip_space(input, p, state)
                    .ok_or(ParseError::Eof(*p))?;
            }
            _ => return Err(ParseError::ExpectCommaOrEnd(*p)),
        }
    }
}

/// Parse a JSON string — `"` NOT yet consumed.
///
/// Thin delegator to [`string::parse_string_body`]. Splitting the body
/// out allows LLVM to size-inline the common escape-free path without
/// pulling the cold escape path into every call site.
#[inline(always)]
pub(crate) fn parse_string<V: JsonVisitor>(
    input: &[u8],
    p: &mut usize,
    _state: &mut ScanState,
    visitor: &mut V,
    is_key: bool,
) -> Result<(), ParseError> {
    string::parse_string_body(input, p, visitor, is_key)
}

/// Parse a JSON number starting at `*p`.
///
/// `first_byte` is the dispatch byte the caller already consumed
/// from `input[*p]`; parse_number uses it to avoid a redundant
/// re-read for the sign check.
///
/// Thin delegator to [`number::parse_number_body`]. Eisel-Lemire +
/// fallback live in the `number` module.
#[inline(always)]
pub(crate) fn parse_number<V: JsonVisitor>(
    input: &[u8],
    p: &mut usize,
    first_byte: u8,
    visitor: &mut V,
) -> Result<(), ParseError> {
    number::parse_number_body(input, p, first_byte, visitor)
}

/// Expect an exact keyword sequence at `*p`. Advances `*p` past it on
/// match; returns [`ParseError::ExpectKeyword`] on miss.
#[inline(always)]
pub(crate) fn expect_keyword(
    input: &[u8],
    p: &mut usize,
    word: &[u8],
) -> Result<(), ParseError> {
    let at = *p;
    let end = at + word.len();
    if input.len() < end || &input[at..end] != word {
        return Err(ParseError::ExpectKeyword(at));
    }
    *p = end;
    Ok(())
}
