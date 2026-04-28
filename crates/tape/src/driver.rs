//! Tape emission error surface — AZ-II.cutover.A retained surface.
//!
//! # Architectural role (post-cutover.A)
//!
//! The DTA walker interpreter retired with tranche AX.W0b. The
//! pre-cutover.A `driver` module retained nine `pub fn` helpers
//! (`trim_ascii_ws`, `trim_with_pattern`, `first_ws_pattern`,
//! `saturating_u16`, `emit_leaf`, `emit_leaf_with_payload`,
//! `emit_reducer_compound`, `lookup_precedence`, `close_compound`)
//! the `audit/AUDIT-3-DECAY-INVENTORY.md` §6 sweep proved had zero
//! non-doc consumers across the workspace. AZ-II.cutover.A retires
//! all nine; the only surface this module still carries is
//! [`DtaError`] — the error enum the emitted `parse()` bodies still
//! return at the crate boundary.

use crate::dta::{DtaRuleId, DtaStateId};

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
