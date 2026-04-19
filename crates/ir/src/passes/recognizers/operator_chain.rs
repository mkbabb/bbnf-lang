//! AW-III.W6.5 — Operator-chain miner: extract per-grammar
//! operator entries from the `DtaTable`'s lifted
//! [`crate::passes::recognizers::dta::PrecedenceEntry`] lists for
//! downstream consumers (Pratt LUT emission).
//!
//! ## Architectural role
//!
//! [`crate::passes::recognizers::dta::collect_precedence_chain`]
//! already discovers operator chains at lift time — Sheets's six-
//! rung `__formula → … → __unary_expr`, BBNF's `value_or → … →
//! value_unary`, CSS's `calc`-style `mathExpr → mathProduct`. It
//! materialises the result as a
//! [`crate::passes::recognizers::dta::PrecedenceTable`] on each
//! emitted [`crate::passes::recognizers::dta::DtaState::ShuntingYard`].
//!
//! W6.5 needs the same precedence data in a shape the Rust-backend
//! emitter can consume when lowering the per-grammar
//! `PRECEDENCE_LUT` constant. The IR fact produced here —
//! [`OperatorChainEntry`] — is a flat, side-table projection of the
//! `DtaTable::shunting_yard_chains` map: one entry per operator
//! byte, with enough context (byte, second_byte, precedence,
//! associativity, arity, op_rule, op_discriminant) for the emitter
//! to pack a dense `[u8; 256]` LUT + a sparse metadata slice.
//!
//! The miner runs AFTER the DTA lift so it can read the already-
//! mined chain data; it does not re-implement chain detection.
//! Consumers (emitter) read [`collect_operator_chains`] and emit
//! per-grammar LUTs.

use crate::passes::recognizers::dta::{
    Associativity, DtaState, DtaTable, PrecedenceEntry,
};
use crate::RuleId;

// ── Public types ──────────────────────────────────────────────────────

/// Operator arity marker — indirectly pins the Pratt dispatch shape
/// for each LUT byte. Binary is the only arity `collect_precedence_
/// chain` currently admits; Prefix / Postfix are reserved for future
/// miners that extend the chain shape to unary operators.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum OperatorArity {
    /// Left / right operand, e.g. `+`, `*`, `^`.
    Binary,
    /// Prefix unary, e.g. `-x`. Reserved; not yet admitted.
    Prefix,
    /// Postfix unary, e.g. `x%`. Reserved; not yet admitted.
    Postfix,
}

impl OperatorArity {
    /// Pack arity into the 2 bits the LUT byte reserves for it
    /// (bits 5..7 of the packed LUT byte; see `precedence.rs`).
    #[inline]
    pub const fn to_bits(self) -> u8 {
        match self {
            OperatorArity::Binary => 0,
            OperatorArity::Prefix => 1,
            OperatorArity::Postfix => 2,
        }
    }
}

/// One operator row extracted from a mined precedence chain. Feeds
/// the per-grammar [`OperatorChainFacts`] collection; consumers
/// (Rust emitter's `precedence.rs`) produce a packed LUT + sparse
/// metadata slice.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OperatorChainEntry {
    /// First dispatch byte. When `second_byte.is_some()` the LUT
    /// marks this byte as a two-byte-op candidate; the sparse slice
    /// carries the second-byte + discriminant.
    pub byte: u8,
    /// Optional second byte for `<=`, `>=`, `<>` shape two-byte ops.
    pub second_byte: Option<u8>,
    /// Higher values bind tighter. Assigned innermost-first by
    /// `collect_precedence_chain` (deepest rung has the highest
    /// precedence). Range: 1..=15 (fits in 4 LUT bits).
    pub precedence: u8,
    /// Left-assoc (`next_min = prec + 1`) or right-assoc
    /// (`next_min = prec`).
    pub associativity: Associativity,
    /// Operator arity. Binary only for W6.5; reserved field.
    pub arity: OperatorArity,
    /// The rung rule that emits this operator. Used by the walker's
    /// `push_compound_binary` arm to thread variant_idx through.
    pub op_rule: RuleId,
    /// The u8 discriminant carried in the push_compound's payload
    /// column (e.g. Sheets `add_op "+" -> 0u8` emits `0` here).
    pub op_discriminant: u8,
}

/// All operator chains mined from a grammar's `DtaTable`.
///
/// One [`OperatorChainFacts`] per grammar; the `entries` slice lists
/// every operator row across every chain. The emitter ingests this
/// collection to produce the per-grammar `PRECEDENCE_LUT` +
/// `DtaPrecedenceEntry` metadata slice.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct OperatorChainFacts {
    /// Flat list of operator rows. Empty for grammars whose lift
    /// found no chains (i.e. [`DtaTable::shunting_yard_chains`] was
    /// empty).
    pub entries: Vec<OperatorChainEntry>,
    /// Rule ids for the heads of mined chains. Used by emitters
    /// that need to refer to the head state by rule name (e.g. for
    /// diagnostic messages).
    pub chain_heads: Vec<RuleId>,
}

impl OperatorChainFacts {
    /// Is the miner's output empty (no chains detected)?
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Number of distinct operator byte entries. Each emitted LUT
    /// row consumes one.
    #[inline]
    pub fn operator_count(&self) -> usize {
        self.entries.len()
    }
}

// ── Miner ────────────────────────────────────────────────────────────

/// Mine operator-chain facts from a lifted [`DtaTable`].
///
/// Walks `shunting_yard_chains` (the set of rules mapped to a
/// `ShuntingYard` state), collects the `PrecedenceEntry` list from
/// each `ShuntingYard` state's table, and projects each entry into
/// an [`OperatorChainEntry`] with arity `Binary` (the only arity
/// `collect_precedence_chain` admits today).
///
/// Idempotent: repeated runs over the same table produce identical
/// output.
///
/// ## §6 generalisation
///
/// The miner is grammar-name-blind. Sheets's six-rung arithmetic
/// tower, BBNF's `value_or` tower, and CSS's `calc()` math tower
/// all ride the same chain detector upstream — this miner ingests
/// their output uniformly and emits one `OperatorChainFacts` per
/// grammar without any per-grammar branching.
pub fn collect_operator_chains(table: &DtaTable) -> OperatorChainFacts {
    let mut entries: Vec<OperatorChainEntry> = Vec::new();
    let mut chain_heads: Vec<RuleId> = Vec::new();
    let mut seen_bytes: [bool; 256] = [false; 256];

    // `shunting_yard_chains` keys by the chain's outermost rule.
    // Walk in stable order so the emitted LUT byte layout is
    // deterministic across builds.
    let mut chain_rules: Vec<RuleId> = table.shunting_yard_chains.keys().copied().collect();
    chain_rules.sort_unstable();

    for rule_id in chain_rules {
        let state_id = match table.shunting_yard_chains.get(&rule_id) {
            Some(&sid) => sid,
            None => continue,
        };
        let state = match table.states.get(state_id.0 as usize) {
            Some(s) => s,
            None => continue,
        };
        let precedence = match state {
            DtaState::ShuntingYard { precedence, .. } => &precedence.entries,
            // Non-ShuntingYard state under the chain key — skip
            // defensively; lift invariant says this cannot happen.
            _ => continue,
        };
        chain_heads.push(rule_id);

        for pe in precedence {
            if seen_bytes[pe.byte as usize] {
                // Byte already claimed by an earlier chain; the
                // chain detector enforces pairwise disjointness
                // within a single chain, but not across chains.
                // Defensive: the emitter rejects duplicates, so
                // silently drop them here.
                continue;
            }
            seen_bytes[pe.byte as usize] = true;
            entries.push(to_chain_entry(pe));
        }
    }

    OperatorChainFacts {
        entries,
        chain_heads,
    }
}

/// Project a lifter-produced [`PrecedenceEntry`] into the flat
/// [`OperatorChainEntry`] shape consumers expect. Arity stays
/// `Binary` until the chain detector extends to unary operators.
fn to_chain_entry(pe: &PrecedenceEntry) -> OperatorChainEntry {
    OperatorChainEntry {
        byte: pe.byte,
        second_byte: pe.second_byte,
        precedence: pe.precedence,
        associativity: pe.associativity,
        arity: OperatorArity::Binary,
        op_rule: pe.op_rule,
        op_discriminant: pe.op_discriminant,
    }
}
