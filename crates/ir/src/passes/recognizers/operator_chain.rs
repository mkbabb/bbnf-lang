//! AW-III.W6.5 — Operator-chain miner: extract per-grammar
//! operator entries for downstream consumers (Pratt LUT emission).
//!
//! ## Architectural role
//!
//! The miner produces per-Pratt-rule operator-byte tables the Rust-
//! backend emitter consumes when lowering the per-rule
//! `PRECEDENCE_LUT_<rule>` constants. The IR fact produced here —
//! [`OperatorChainEntry`] — carries one entry per operator byte, with
//! enough context (byte, second_byte, precedence, associativity,
//! arity, op_rule, op_discriminant) for the emitter to pack a dense
//! `[u8; 256]` LUT plus a sparse metadata slice per rule.
//!
//! ## Pratt-classification authoritativeness (AX.W0a.2.k)
//!
//! The miner keys on [`crate::passes::recognizers::shape_dispatch::
//! ShapeTag::Pratt`] — every rule the shape detector admits as
//! Pratt-shape contributes its operator set. This decouples the LUT
//! from DTA lift semantics: the DTA's
//! [`crate::passes::recognizers::dta::collect_precedence_chain`]
//! requires ≥ 2 rungs to collapse a tower into one `ShuntingYard`
//! state (a walker-path optimisation). The Pratt shape emitter,
//! however, emits per-Pratt-rule `parse_pratt_*` bodies — including
//! single-rung Pratt rules (BBNF's `binary_factor = mapped_factor ,
//! (binary_operators ?w , mapped_factor) *`) whose operators must
//! populate the rule's LUT for the emitted
//! `parse_pratt_*_binary_factor` body's inner byte dispatch to
//! succeed.
//!
//! ## Per-rule scoping
//!
//! Each Pratt rule gets its OWN [`OperatorChainRule`] (and its own
//! emitted `PRECEDENCE_LUT_<rule>` constant). This avoids cross-rule
//! first-byte collisions that arise when multiple Pratt rules share
//! a first byte (BBNF: `||` in `value_or` vs `|` as grammar-level
//! alternation separator in `binary_factor`'s caller — the two
//! contexts must NOT share a LUT byte). Per-rule scoping ensures
//! each Pratt function consults only its own operator alphabet.
//!
//! Rule sources:
//!
//! 1. `DtaTable::shunting_yard_chains` — the multi-rung towers the
//!    DTA lift already mined (unchanged from W6.5). Each rule in a
//!    chain gets the chain's shared precedence table (the rung
//!    doesn't own its own slice).
//! 2. `ShapeTag::Pratt` rules outside those chains — structurally
//!    matched via [`crate::passes::recognizers::dta::
//!    match_operator_chain_rule`] to extract their operator entries
//!    with precedence = 1 (single-rung).

use crate::passes::recognizers::dta::{
    match_operator_chain_rule, Associativity, DtaState, DtaTable, PrecedenceEntry,
};
use crate::passes::recognizers::shape_dispatch::ShapeTag;
use crate::{GrammarIR, RuleId};

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

/// Per-rule operator-chain projection.
///
/// Each Pratt-classified rule owns one [`OperatorChainRule`] carrying
/// its operator entries. Per-rule scoping prevents cross-rule
/// first-byte collisions between Pratt contexts: BBNF's `value_or`
/// owns `||` without leaking into `binary_factor`'s byte-60-dispatch
/// (which carries `<<`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OperatorChainRule {
    /// The Pratt rule whose operator alphabet this entry owns.
    pub rule: RuleId,
    /// Rule name (resolved from `GrammarIR::get_string(rule.name)` at
    /// mining time). Carrying the name here avoids the emitter needing
    /// IR access to produce per-rule `PRECEDENCE_LUT_<name>` constants.
    pub rule_name: String,
    /// Operator entries for this rule's alphabet — one per operator
    /// first byte (multi-byte ops contribute the first byte here +
    /// the second in `second_byte`).
    pub entries: Vec<OperatorChainEntry>,
}

/// All operator chains mined from a grammar's IR + `DtaTable`.
///
/// One [`OperatorChainFacts`] per grammar. The `rules` slice lists
/// one [`OperatorChainRule`] per Pratt-classified rule — the emitter
/// ingests this collection to produce one
/// `PRECEDENCE_LUT_<rule>` + `PRECEDENCE_ENTRIES_<rule>` metadata
/// slice per rule, giving each `parse_pratt_*` function its own
/// operator alphabet.
///
/// `chain_heads` is the set of rules that carry operator entries;
/// consumers use it to diagnose mining hits / misses.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct OperatorChainFacts {
    /// One record per Pratt-classified rule whose operator chain was
    /// structurally matched. Sort-stable by [`RuleId`].
    pub rules: Vec<OperatorChainRule>,
    /// Rule ids for the heads of mined chains. Duplicates
    /// `rules[*].rule` — retained as a flat projection so consumers
    /// that only need the head set don't iterate the per-rule entries.
    pub chain_heads: Vec<RuleId>,
}

impl OperatorChainFacts {
    /// Is the miner's output empty (no chains detected)?
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.rules.iter().all(|r| r.entries.is_empty())
    }

    /// Total number of distinct operator byte entries across every
    /// Pratt rule. Each emitted LUT row consumes one.
    #[inline]
    pub fn operator_count(&self) -> usize {
        self.rules.iter().map(|r| r.entries.len()).sum()
    }

    /// Flat view of every operator entry across every rule. Preserves
    /// rule-iteration order for deterministic emission. Used by
    /// consumers that don't care about per-rule scoping (diagnostic
    /// tests, summary accumulators).
    pub fn entries_flat(&self) -> impl Iterator<Item = &OperatorChainEntry> {
        self.rules.iter().flat_map(|r| r.entries.iter())
    }
}

// ── Miner ────────────────────────────────────────────────────────────

/// Mine operator-chain facts from a grammar IR and its lifted
/// [`DtaTable`].
///
/// Two-source projection:
///
/// 1. Every multi-rung tower the DTA lift mined into
///    `shunting_yard_chains` contributes each operator-byte entry
///    from its [`DtaState::ShuntingYard`] state's precedence table
///    (pre-AX.W0a.2.k behaviour, unchanged).
/// 2. Every [`ShapeTag::Pratt`]-classified rule not covered by (1)
///    is structurally matched via [`match_operator_chain_rule`] and
///    contributes its operator entries with precedence = 1
///    (single-rung Pratt — BBNF's `binary_factor` is the canonical
///    case).
///
/// Bytes already claimed by higher-priority (multi-rung) chains are
/// not reclaimed — each operator byte maps to exactly one LUT row.
///
/// Idempotent: repeated runs over the same `(ir, table)` produce
/// identical output. Stable ordering: rule-id ascending within each
/// source, multi-rung chains first so their precedence values win
/// byte-collisions with single-rung Pratt rules.
///
/// ## §6 generalisation
///
/// The miner is grammar-name-blind. Sheets's six-rung arithmetic
/// tower, BBNF's `value_or` + `binary_factor` rules, and CSS's
/// `calc()` math tower all ride the same classification surface
/// (ShapeTag::Pratt + optional multi-rung DTA collapse) upstream —
/// this miner projects their operator sets uniformly into one
/// `OperatorChainFacts` per grammar without any per-grammar
/// branching.
pub fn collect_operator_chains(
    ir: &GrammarIR,
    table: &DtaTable,
) -> OperatorChainFacts {
    let mut rules_out: Vec<OperatorChainRule> = Vec::new();
    let mut chain_heads: Vec<RuleId> = Vec::new();

    // ── Source 1: multi-rung chains from the DTA lift ───────────────
    //
    // `shunting_yard_chains` keys by every chain rung (outermost AND
    // inner rungs all map to the same `ShuntingYard` state). Each
    // rule gets its OWN [`OperatorChainRule`] carrying the chain's
    // entries — per-rule LUT emission means each `parse_pratt_<rung>`
    // function has the full tower's operator alphabet (that's what
    // the walker's ShuntingYard state gave every chain rung).
    let mut chain_rules: Vec<RuleId> = table.shunting_yard_chains.keys().copied().collect();
    chain_rules.sort_unstable();

    for rule_id in &chain_rules {
        let state_id = match table.shunting_yard_chains.get(rule_id) {
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
        // Deduplicate within-rule byte collisions. The chain
        // detector enforces pairwise disjointness across rungs, so
        // this is defensive.
        let mut rule_entries: Vec<OperatorChainEntry> = Vec::new();
        let mut rule_seen: [bool; 256] = [false; 256];
        for pe in precedence {
            if rule_seen[pe.byte as usize] {
                continue;
            }
            rule_seen[pe.byte as usize] = true;
            rule_entries.push(to_chain_entry(pe));
        }
        let rule_name = ir
            .rules
            .iter()
            .find(|r| r.id == *rule_id)
            .map(|r| ir.get_string(r.name).to_string())
            .unwrap_or_default();
        chain_heads.push(*rule_id);
        rules_out.push(OperatorChainRule {
            rule: *rule_id,
            rule_name,
            entries: rule_entries,
        });
    }

    // ── Source 2: single-rung Pratt rules outside the multi-rung
    //             chains (AX.W0a.2.k). ────────────────────────────────
    //
    // The DTA lift's `collect_precedence_chain` enforces rungs ≥ 2
    // because the walker-path ShuntingYard collapse is only a win
    // for towers. The shape emitter, however, emits per-Pratt-rule
    // `parse_pratt_*` bodies — single-rung Pratt rules (BBNF's
    // `binary_factor`) need their operators in their per-rule LUT so
    // the emitted inner byte dispatch resolves `<<` / `>>` / `-` etc.
    //
    // Rule admission criterion: the rule's operator byte set must be
    // pairwise first-byte disjoint. Two-byte ops (`<<`, `>>`) are
    // distinguishable via the LUT's bit-7 two_byte flag + the
    // `PRECEDENCE_ENTRIES_<rule>` scan; ops that share first byte
    // without two-byte disambiguation (`<` + `<=` in the same rule)
    // would produce ambiguous runtime dispatch, so such rules are
    // dropped. Per-rule scoping means dropping value_cmp doesn't
    // affect binary_factor's alphabet.
    let multi_rung_rules: std::collections::HashSet<RuleId> =
        table.shunting_yard_chains.keys().copied().collect();

    for rule in &ir.rules {
        if !matches!(ir.shape_assignments.get(rule.id), ShapeTag::Pratt) {
            continue;
        }
        if multi_rung_rules.contains(&rule.id) {
            continue;
        }
        let rule_name = ir.get_string(rule.name).to_string();
        // Structural-match + within-rule disjointness. Rules that
        // fail either admission produce an EMPTY per-rule entry so
        // the emitter still has a record to project into an empty
        // `PRECEDENCE_LUT_<rule>` (walker-path parity — the rule
        // still emits a `parse_pratt_<rule>` function, the loop just
        // exits immediately because every LUT byte reads 0).
        let rule_entries: Vec<OperatorChainEntry> =
            match_operator_chain_rule(ir, rule)
                .and_then(|(_, operators, _)| {
                    // Within-rule first-byte disjointness: reject
                    // rules where two entries share a first byte
                    // without two-byte disambiguation (cmp_op's
                    // `<` + `<=` case). Ambiguous dispatch would
                    // silently corrupt runtime parsing.
                    let mut rule_seen: [bool; 256] = [false; 256];
                    for pe in &operators {
                        if rule_seen[pe.byte as usize] {
                            return None;
                        }
                        rule_seen[pe.byte as usize] = true;
                    }
                    Some(
                        operators
                            .into_iter()
                            .map(|mut pe| {
                                // Single-rung chain: precedence is 1
                                // (the innermost / only rung).
                                pe.precedence = 1;
                                to_chain_entry(&pe)
                            })
                            .collect::<Vec<_>>(),
                    )
                })
                .unwrap_or_default();
        if !rule_entries.is_empty() {
            chain_heads.push(rule.id);
        }
        rules_out.push(OperatorChainRule {
            rule: rule.id,
            rule_name,
            entries: rule_entries,
        });
    }

    OperatorChainFacts {
        rules: rules_out,
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
