//! Dispatch Tape Automaton (DTA) live-substrate types.
//!
//! # Architectural role
//!
//! Post-AY.W0 the runtime DTA driver is retired; the per-grammar
//! emitter inlines its dispatch directly. What survives in this
//! module is the small, live substrate the Pratt / shunting-yard
//! lever still threads through emitted code:
//!
//! - [`DtaRuleId`] — the rule identifier the precedence-table
//!   entry pins, mirrored on the tape side so the emitter can
//!   build `const PRECEDENCE_ENTRIES_*` arrays without taking a
//!   crate dependency back on the IR.
//! - [`DtaAssociativity`] — left / right operator associativity;
//!   one byte per operator in the precedence table.
//! - [`DtaPrecedenceEntry`] — one row per operator, consumed by
//!   the emitted Pratt reducer at compile time.
//! - [`DtaStateId`] — retained solely as the `NONE` sentinel that
//!   the emitted ClassifyByte LUT and shape-emitter error paths
//!   stamp into `DtaError::Syntax`'s `failing_state` field.
//!
//! Everything else in the historical DTA module — the 13-variant
//! `DtaState` enum, the `DtaTable` struct + impls, `DtaDiagnostic`,
//! `LiteralPayload`, `DtaRuleEntry`, `DtaFrameKind`,
//! `DtaCounterOptional`, `SeqPromote` — was kernel-dead at AX
//! close (no consumer in the bench binaries, no `nm` symbol). The
//! AY.W0.3 carve retires the whole carry-forward block.

/// Opaque state identifier — retained for the `NONE` sentinel that
/// emitted ClassifyByte LUT entries and shape-emitter error paths
/// stamp into `DtaError::Syntax`'s `failing_state` field.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct DtaStateId(pub u16);

impl DtaStateId {
    /// Sentinel — no state.
    pub const NONE: DtaStateId = DtaStateId(u16::MAX);
}

/// IR rule id, mirrored on the tape side to avoid a crate dependency
/// edge from `tape` back into the IR for the precedence-table row.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct DtaRuleId(pub u32);

/// Operator associativity for the Pratt / shunting-yard reducer.
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum DtaAssociativity {
    /// Left-associative: `a op b op c` binds as `(a op b) op c`.
    Left = 0,
    /// Right-associative: `a op b op c` binds as `a op (b op c)`. In
    /// the Sheets precedence chain this is only `^`.
    Right = 1,
}

/// One operator's precedence-table row.
///
/// Emitted as one entry of a per-rule `const PRECEDENCE_ENTRIES_*:
/// &[DtaPrecedenceEntry] = &[…]` array; consumed by the Pratt
/// reducer arm in the per-grammar `parse()` body.
#[derive(Clone, Copy, Debug)]
pub struct DtaPrecedenceEntry {
    /// Operator's identifying first byte.
    pub byte: u8,
    /// Optional second byte for two-byte operators (`<<`, `>>`).
    pub second_byte: Option<u8>,
    /// Higher values bind tighter.
    pub precedence: u8,
    /// Left or right associative.
    pub associativity: DtaAssociativity,
    /// Rule whose variant_idx the runtime threads into the pushed
    /// compound (`+` / `-` both share `add_op`'s rule id with
    /// different discriminants).
    pub op_rule: DtaRuleId,
    /// Which Alt branch index within the op rule — stored as the
    /// typed payload's u8 discriminant.
    pub op_discriminant: u8,
}
