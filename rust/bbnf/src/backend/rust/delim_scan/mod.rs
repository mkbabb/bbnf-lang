//! Delimiter-driven flat scanner for Wrap(Repeat(Alt)) patterns.
//!
//! When a `Repeat` is inside a `Wrap` (e.g., `"{" >> items * << "}"`), and the
//! Repeat's body is an `Alt` whose branches can be distinguished by which
//! delimiter byte appears first in a forward `memchr` scan, this module emits
//! a flat scanner loop instead of the standard recursive-descent per-element loop.
//!
//! All delimiter bytes are extracted from the grammar's own `Literal` nodes —
//! no grammar-specific knowledge is hard-coded.
//!
//! The emitted scanner uses 2–3 `memchr` calls per item instead of ~20
//! recursive-descent operations, eliminating IIFE closures, checkpoint/restore,
//! and per-element Option wrapping.
//!
//! Sub-modules:
//! - `detect`: Pattern detection — `try_detect()` and structural helpers
//! - `emit`: Code emission — `emit_scan()`, `emit_scan_loop()`, `try_emit_alloc_wrap()`

mod detect;
mod emit;

use bbnf_ir::RuleId;

// ── Re-exports (public API for sibling codegen modules) ─────────────────────

pub(super) use emit::try_emit_alloc_wrap;
pub use emit::emit_scan;

// ── Configuration ───────────────────────────────────────────────────────────

/// Grammar-agnostic delimiter-scan configuration.
/// All bytes extracted from the IR's Literal nodes.
pub struct DelimScanConfig {
    /// Opening delimiter byte.
    pub open_byte: u8,
    /// Closing delimiter byte.
    pub close_byte: u8,
    /// Pivot byte that distinguishes branches.
    pub pivot_byte: u8,
    /// Optional trailing delimiter for the pivot branch.
    pub trail_byte: Option<u8>,
    /// RuleId of the block/fallback branch (the cyclic Ref in the Alt).
    pub block_fn: Option<RuleId>,
    /// RuleId of the pivot branch (the rule whose body contains the pivot Literal).
    pub pivot_fn: Option<RuleId>,
    /// RuleId of the content rule containing the Repeat(Alt) — used for Vec variant name.
    pub content_rule: Option<RuleId>,
}

