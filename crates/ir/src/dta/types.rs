//! Dispatch Tape Automaton (DTA) live-substrate types — IR-layer
//! owners.
//!
//! # Architectural role (post-AZ-II.cutover.A substrate hoist)
//!
//! These four types describe the small, codegen-time substrate the
//! Pratt / shunting-yard lever threads through emitted code:
//!
//! - [`DtaRuleId`] — the rule identifier the precedence-table entry
//!   pins. The IR is the natural owner of rule identity; pre-cutover.A
//!   the type lived under `tape::dta` to "avoid a crate dependency
//!   edge from tape back into the IR for the precedence-table row".
//!   That workaround inverts at the cutover: tape now consumes the
//!   IR's authoritative type via `bbnf_ir::dta::*`.
//! - [`DtaAssociativity`] — left / right operator associativity; one
//!   byte per operator in the precedence table.
//! - [`DtaPrecedenceEntry`] — one row per operator, consumed by the
//!   emitted Pratt reducer at compile time.
//! - [`DtaStateId`] — opaque state identifier; retained as the `NONE`
//!   sentinel that the emitted ClassifyByte LUT and shape-emitter
//!   error paths stamp into `DtaError::Syntax`'s `failing_state`
//!   field.

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

/// IR rule id — the canonical rule identifier the precedence-table
/// row pins.
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
