//! Tape emission helpers — AX.W0b.A retained surface.
//!
//! # Architectural role (post-W0b)
//!
//! The DTA walker interpreter retired with tranche AX.W0b: every
//! grammar routes through the shape dispatcher, so the historical
//! frame-stack machinery (`FrameStack`, `Frame`, `dispatch_one`,
//! `dta_run_cold`, `try_branch`, `advance_or_pop_with`, …) evaporates.
//! What survives is a thin leaf-emission API the shape emitters
//! still call for structural leaf + reducer-compound writes.
//!
//! The retained surface is strictly helper-shaped — no state machine,
//! no frame stack, no dispatch loop. Every function takes the columns
//! (plus a parallel `frame_depth` stream on inline-frame-depth
//! builders) and writes one record. Depth + variant_idx arrive as
//! explicit parameters from the caller; the per-grammar emitter knows
//! both at codegen time.
//!
//! # Retained helpers
//!
//! - [`emit_leaf`] / [`emit_leaf_with_payload`] — single-row leaf
//!   emission with / without a payload-in-arena child offset.
//! - [`emit_reducer_compound`] — Pratt / ShuntingYard reducer write.
//! - [`close_compound`] — seal a reserved compound row with the
//!   observed span and child offset.
//! - [`trim_with_pattern`] / [`trim_ascii_ws`] / [`first_ws_pattern`]
//!   — whitespace skip helpers (regex-driven and default-ASCII).
//! - [`saturating_u16`] — Repeat-bound saturation helper.
//! - [`lookup_precedence`] — Pratt operator-byte precedence lookup.
//! - [`DtaError`] — error surface shared with the emitted parse().

use crate::columns::Columns;
use crate::dta::{DtaPrecedenceEntry, DtaRuleId, DtaStateId};
use crate::kind::TapeKind;
use crate::tape::TapeOffset;

/// Error surface for emitted `parse()` dispatchers.
///
/// Kept flat — the generated `parse()` converts to its own
/// `ParseErr` shape at the crate boundary.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DtaError {
    /// The driver could not match at `offset`. `failing_state` /
    /// `failing_rule` carry provenance when the emitter can
    /// attribute it, or [`DtaStateId::NONE`] / [`DtaRuleId`] with
    /// `u32::MAX` otherwise.
    Syntax {
        /// Byte offset where the match attempt failed.
        offset: u32,
        /// The state id that dispatched to no successful child.
        failing_state: DtaStateId,
        /// The rule that was active when the failure occurred;
        /// `DtaRuleId(u32::MAX)` when the driver cannot attribute
        /// the failure to a specific rule.
        failing_rule: DtaRuleId,
    },
    /// Parse terminated with unconsumed trailing input, or the
    /// parser needed another byte but hit EOF.
    UnexpectedEnd {
        /// Byte offset where the driver terminated.
        offset: u32,
    },
    /// Invariant violation — the parser requested a state outside
    /// the valid range. Always a codegen bug.
    InvalidState {
        /// The out-of-range state id the driver encountered.
        state: DtaStateId,
    },
}

// ── Whitespace helpers ──────────────────────────────────────────────

/// AW-I.W4ε fallback: trim ASCII whitespace in-place at `*pos`.
///
/// Mirrors the `WsTrim { pattern: None }` semantics: advances `pos`
/// past every `' '` / `'\t'` / `'\n'` / `'\r'` byte at the current
/// offset, leaving `pos` at the first non-ws byte.
#[inline]
pub fn trim_ascii_ws(input: &[u8], pos: &mut u32) {
    let mut p = *pos as usize;
    while let Some(&b) = input.get(p) {
        match b {
            b' ' | b'\t' | b'\n' | b'\r' => p += 1,
            _ => break,
        }
    }
    *pos = p as u32;
}

/// AW-III.W2 — trim using the grammar's declared whitespace
/// semantics. When `pattern` carries a regex pattern, scan via the
/// supplied `regex_scan` fn pointer; otherwise fall back to
/// [`trim_ascii_ws`].
#[inline]
pub fn trim_with_pattern(
    regex_scan: fn(&str, &[u8], usize) -> Option<u32>,
    pattern: Option<&'static str>,
    input: &[u8],
    pos: &mut u32,
) {
    if let Some(pat) = pattern {
        if let Some(len) = regex_scan(pat, input, *pos as usize) {
            *pos += len;
        }
    } else {
        trim_ascii_ws(input, pos);
    }
}

/// Pratt LUT helpers the shape emitter consults when lowering a
/// `WsTrim` probe inside a per-grammar whitespace skip. Returns
/// the first regex pattern the emitted regex-scan adapter is
/// willing to match, or `None` when no whitespace regex was mined
/// for the grammar.
#[inline]
pub fn first_ws_pattern(_patterns: &[&'static str]) -> Option<&'static str> {
    None
}

// ── Repeat bound saturation ─────────────────────────────────────────

/// Saturating `u32 → u16` conversion.
///
/// Used by the emitted `parse()` bodies when packing grammar-fixed
/// Repeat `lo` / `hi` bounds into on-stack state. Values above
/// `u16::MAX` saturate (grammar-level `*` unbounds are represented
/// as `u16::MAX`, treated as "no upper bound" by the emitted
/// iteration driver).
#[inline]
pub fn saturating_u16(v: u32) -> u16 {
    if v >= u16::MAX as u32 {
        u16::MAX
    } else {
        v as u16
    }
}

// ── Column emission helpers ─────────────────────────────────────────

/// Emit a structural leaf record with no payload.
///
/// The `depth` and `variant` arguments come from the caller — the
/// per-grammar shape emitter tracks the enclosing rule's discriminant
/// at codegen time. `frame_depth` is stamped inline (the AW.1.4
/// Stage-C elision contract).
#[inline(always)]
pub fn emit_leaf(
    columns: &mut Columns,
    frame_depth: &mut Vec<u8>,
    depth: u8,
    variant: u8,
    kind: TapeKind,
    span_lo: u32,
    span_hi: u32,
) -> u32 {
    emit_leaf_with_payload(
        columns,
        frame_depth,
        depth,
        variant,
        kind,
        span_lo,
        span_hi,
        TapeOffset::NONE,
    )
}

/// Emit a leaf record with `child_off` stamped to a payload-bearing
/// arena offset.
///
/// Same column-write shape as [`emit_leaf`] except the polymorphic
/// `child_off` slot carries the arena byte offset where the typed
/// payload bytes live (written by [`stage_literal_payload_in_arena`]
/// or the shape emitter's typed-payload decoder). The record's
/// [`TapeRec::PAYLOAD_IN_ARENA_BIT`](crate::tape::TapeRec::PAYLOAD_IN_ARENA_BIT)
/// is set when `child_off != NONE`, signalling to scalar readers
/// (`payload_u8`, `payload_scalar::<T>`) that they should slice
/// `pay_agg` directly instead of indirecting through `pay_narrow` /
/// `pay_wide`.
#[inline(always)]
pub fn emit_leaf_with_payload(
    columns: &mut Columns,
    frame_depth: &mut Vec<u8>,
    depth: u8,
    variant: u8,
    kind: TapeKind,
    span_lo: u32,
    span_hi: u32,
    child_off: TapeOffset,
) -> u32 {
    let extra: u16 = if child_off.is_none() {
        0
    } else {
        crate::tape::TapeRec::PAYLOAD_IN_ARENA_BIT
    };
    let idx = columns.push_leaf_fused(kind, variant, extra, span_lo, span_hi, child_off);
    frame_depth.push(depth);
    idx
}

/// Emit a binary-operator compound for the ShuntingYard reducer.
///
/// The compound sits AFTER the RHS operand with `child_off` pointing
/// back at the LHS operand's tape row. Post-order layout for
/// reducer-produced compounds; the cursor's bounded backward-walk
/// fallback resolves the first-child lookup at read time.
#[inline]
pub fn emit_reducer_compound(
    columns: &mut Columns,
    frame_depth: &mut Vec<u8>,
    depth: u8,
    lhs_idx: u32,
    op_discriminant: u8,
    span_lo: u32,
    span_hi: u32,
) -> u32 {
    let idx = columns.push_leaf_fused(
        TapeKind::Rule,
        op_discriminant,
        crate::tape::TapeRec::HAS_CHILDREN_BIT,
        span_lo,
        span_hi,
        TapeOffset(lhs_idx),
    );
    frame_depth.push(depth);
    idx
}

/// Lookup the precedence entry for the operator starting at byte `b`
/// (with optional second byte `b2` for two-byte operators).
///
/// Two-byte operators (e.g. `<=`, `>=`) require both bytes to match;
/// a single-byte op entry matches regardless of the second byte.
/// Prefers two-byte matches when available; falls back to single-
/// byte.
#[cold]
#[inline(never)]
pub fn lookup_precedence(
    precedence: &'static [DtaPrecedenceEntry],
    b: u8,
    b2: Option<u8>,
) -> Option<&'static DtaPrecedenceEntry> {
    if let Some(b2v) = b2 {
        for entry in precedence {
            if entry.byte == b && entry.second_byte == Some(b2v) {
                return Some(entry);
            }
        }
    }
    for entry in precedence {
        if entry.byte == b && entry.second_byte.is_none() {
            return Some(entry);
        }
    }
    None
}

/// Close a reserved compound row — stamp `span_hi`, `child_off`,
/// and `has_children` on the parent.
///
/// The shape emitter passes the row index it reserved when opening
/// the compound, the byte-range end, and the index of the first
/// child row. `has_children` is stamped when any child row actually
/// landed (i.e. `first_child < columns.len()` at call time).
///
/// AY.W1.1 — flat-AoS substrate: the three back-patches land as field
/// updates on the AoS row directly via [`Columns::set_span_hi_at`],
/// [`Columns::set_child_off_at`], [`Columns::or_extra_at`].
#[inline(always)]
pub fn close_compound(
    columns: &mut Columns,
    parent_rec: u32,
    span_hi: u32,
    first_child: u32,
) {
    let has_children = (columns.len() as u32) > first_child;
    columns.set_span_hi_at(parent_rec, span_hi);
    if has_children {
        columns.set_child_off_at(parent_rec, TapeOffset(first_child));
        columns.or_extra_at(parent_rec, crate::tape::TapeRec::HAS_CHILDREN_BIT);
    }
}
