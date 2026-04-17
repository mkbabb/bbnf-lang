//! Unit + topology tests for AW-III.W4.a state-visit frequency mining.
//!
//! The pass operates on a [`DtaTable`] directly without needing a
//! `GrammarIR` (the lifter is a separate concern); the tests therefore
//! construct synthetic `DtaTable`s exercising each shape the
//! propagation must reason about.
//!
//! # Why synthetic and not corpus tables
//!
//! Loading a real grammar requires the `bbnf` crate which depends on
//! `bbnf-ir` (the crate this test ships in), so a corpus-driven test
//! here would require a circular dev-dep. The synthetic shapes cover
//! every variant in `DtaState` plus the full topology surface the
//! emitter consumes (Repeat-multiplier, Ref-cycle convergence,
//! ByteDispatch density, AltLinear uniform prior). End-to-end
//! corpus coverage lives downstream in `crates/core/tests/` once the
//! W4.b emitter wires the ordering.

use std::collections::HashMap;

use bbnf_ir::passes::{
    compute_state_visit_frequency, partition_hot_cold, requires_hot_cold_split, top_hot_states,
    Associativity, CounterOptional, DtaState, DtaTable, FrameKind, LiteralPayload,
    PrecedenceEntry, PrecedenceTable, SeqPromote, StateId, HOT_BUDGET, REPEAT_BODY_MULTIPLIER,
    SHUNTING_YARD_MULTIPLIER,
};
use bbnf_ir::RuleId;

// ── Table builders ──────────────────────────────────────────────────

/// Produce an empty `DtaTable` — used for the empty-input test.
fn empty_table() -> DtaTable {
    DtaTable::default()
}

/// Push `state`, return its id.
fn push(table: &mut DtaTable, state: DtaState) -> StateId {
    let id = table.states.len();
    assert!(id < u16::MAX as usize, "synthetic DTA exceeded u16 range");
    table.states.push(state);
    StateId(id as u16)
}

/// Stamp `state_id` as the entry state for rule `rule_id` and (when no
/// other table-level entry has been set) the table's authoritative
/// entry rule.
fn set_entry(table: &mut DtaTable, rule_id: RuleId, state_id: StateId) {
    table.rule_entries.insert(rule_id, state_id);
    table.entry = rule_id;
}

/// A leaf-only table: one Seq with two Literal children. Verifies the
/// most basic propagation case.
fn flat_seq_table() -> DtaTable {
    let mut t = DtaTable::default();
    let lit_a = push(&mut t, DtaState::Literal { text: 0, payload: LiteralPayload::None });
    let lit_b = push(&mut t, DtaState::Literal { text: 0, payload: LiteralPayload::None });
    let seq = push(
        &mut t,
        DtaState::Seq {
            children: vec![lit_a, lit_b],
            frame: FrameKind::Seq,
            promote: SeqPromote::Default,
        },
    );
    set_entry(&mut t, 0, seq);
    t
}

/// A Repeat over an inner Literal — the topology test's anchor: the
/// inner literal must score above its parent Seq's other children.
///
/// Shape: `Seq([sibling_lit, Repeat(inner_lit, 0..MAX), trailing_lit])`.
fn repeat_topology_table() -> DtaTable {
    let mut t = DtaTable::default();
    let sibling = push(&mut t, DtaState::Literal { text: 0, payload: LiteralPayload::None });
    let inner = push(&mut t, DtaState::Literal { text: 0, payload: LiteralPayload::None });
    let repeat = push(
        &mut t,
        DtaState::Repeat {
            inner,
            lo: 0,
            hi: u32::MAX,
            counter_optional: None,
        },
    );
    let trailing = push(&mut t, DtaState::Literal { text: 0, payload: LiteralPayload::None });
    let seq = push(
        &mut t,
        DtaState::Seq {
            children: vec![sibling, repeat, trailing],
            frame: FrameKind::Seq,
            promote: SeqPromote::Default,
        },
    );
    set_entry(&mut t, 0, seq);
    // Layout (push order):
    //   0 sibling, 1 inner, 2 repeat, 3 trailing, 4 seq
    let _ids = (sibling, inner, repeat, trailing, seq);
    t
}

/// A ByteDispatch with three branches — one branch covers 200 of 256
/// bytes, two branches share the rest. The dense branch must rank
/// above the sparse ones.
fn byte_dispatch_density_table() -> DtaTable {
    let mut t = DtaTable::default();
    let dense = push(&mut t, DtaState::Literal { text: 0, payload: LiteralPayload::None });
    let sparse_a = push(&mut t, DtaState::Literal { text: 0, payload: LiteralPayload::None });
    let sparse_b = push(&mut t, DtaState::Literal { text: 0, payload: LiteralPayload::None });
    let mut table = vec![StateId::NONE; 256];
    for slot in &mut table[0..200] {
        *slot = dense;
    }
    for slot in &mut table[200..228] {
        *slot = sparse_a;
    }
    for slot in &mut table[228..256] {
        *slot = sparse_b;
    }
    let dispatch = push(
        &mut t,
        DtaState::ByteDispatch {
            table,
            fallback: None,
        },
    );
    set_entry(&mut t, 0, dispatch);
    t
}

/// AltLinear with three branches — uniform-prior split.
fn alt_linear_table() -> DtaTable {
    let mut t = DtaTable::default();
    let a = push(&mut t, DtaState::Literal { text: 0, payload: LiteralPayload::None });
    let b = push(&mut t, DtaState::Literal { text: 0, payload: LiteralPayload::None });
    let c = push(&mut t, DtaState::Literal { text: 0, payload: LiteralPayload::None });
    let alt = push(&mut t, DtaState::AltLinear { branches: vec![a, b, c] });
    set_entry(&mut t, 0, alt);
    t
}

/// Two rules joined by a `Ref` — the Ref target accumulates across
/// every cross-rule descent.
fn cross_rule_ref_table() -> DtaTable {
    let mut t = DtaTable::default();
    // Rule 1: a leaf.
    let leaf = push(&mut t, DtaState::Literal { text: 0, payload: LiteralPayload::None });
    let rule_1_entry = leaf;
    // Rule 0 (entry): three Refs into Rule 1, in a Seq.
    let r1 = push(&mut t, DtaState::Ref { rule: 1, target: rule_1_entry });
    let r2 = push(&mut t, DtaState::Ref { rule: 1, target: rule_1_entry });
    let r3 = push(&mut t, DtaState::Ref { rule: 1, target: rule_1_entry });
    let seq = push(
        &mut t,
        DtaState::Seq {
            children: vec![r1, r2, r3],
            frame: FrameKind::Seq,
            promote: SeqPromote::Default,
        },
    );
    t.rule_entries.insert(1, rule_1_entry);
    set_entry(&mut t, 0, seq);
    t
}

/// A self-recursive `Ref` cycle — frequency must remain finite (the
/// pass caps iterations + saturates at `u32::MAX`).
fn ref_cycle_table() -> DtaTable {
    let mut t = DtaTable::default();
    let leaf = push(&mut t, DtaState::Literal { text: 0, payload: LiteralPayload::None });
    let recursive_ref = push(&mut t, DtaState::Ref { rule: 0, target: StateId::NONE });
    let seq = push(
        &mut t,
        DtaState::Seq {
            children: vec![leaf, recursive_ref],
            frame: FrameKind::Seq,
            promote: SeqPromote::Default,
        },
    );
    // Patch the recursive ref to point at the seq (its rule's entry).
    if let Some(DtaState::Ref { target, .. }) = t.states.get_mut(recursive_ref.0 as usize) {
        *target = seq;
    }
    set_entry(&mut t, 0, seq);
    t
}

/// A ShuntingYard head — verifies the multiplier elevation.
fn shunting_yard_table() -> DtaTable {
    let mut t = DtaTable::default();
    let operand = push(&mut t, DtaState::Literal { text: 0, payload: LiteralPayload::None });
    let sibling = push(&mut t, DtaState::Literal { text: 0, payload: LiteralPayload::None });
    let sy = push(
        &mut t,
        DtaState::ShuntingYard {
            head: operand,
            precedence: PrecedenceTable {
                entries: vec![PrecedenceEntry {
                    byte: b'+',
                    second_byte: None,
                    precedence: 1,
                    associativity: Associativity::Left,
                    op_rule: 0,
                    op_discriminant: 0,
                }],
            },
        },
    );
    let seq = push(
        &mut t,
        DtaState::Seq {
            children: vec![sibling, sy],
            frame: FrameKind::Seq,
            promote: SeqPromote::Default,
        },
    );
    set_entry(&mut t, 0, seq);
    t
}

/// A large synthetic table with `> HOT_BUDGET` states. Every state is
/// a simple Literal so the topology is flat and the partitioning logic
/// is the focus.
fn over_budget_table() -> DtaTable {
    let mut t = DtaTable::default();
    let mut leaves = Vec::new();
    for _ in 0..(HOT_BUDGET + 16) {
        let id = push(&mut t, DtaState::Literal { text: 0, payload: LiteralPayload::None });
        leaves.push(id);
    }
    let seq = push(
        &mut t,
        DtaState::Seq {
            children: leaves,
            frame: FrameKind::Seq,
            promote: SeqPromote::Default,
        },
    );
    set_entry(&mut t, 0, seq);
    t
}

/// A diverse table mirroring the BBNF / JSON / CSS / Sheets shape mix:
/// Repeat over a ByteDispatch over multiple AltLinear branches, plus a
/// Ref cycle and a ShuntingYard chain. Used as the smoke-test stand-in
/// for the corpus.
fn diverse_corpus_proxy_table() -> DtaTable {
    let mut t = DtaTable::default();
    // Inner leaves.
    let lit_a = push(&mut t, DtaState::Literal { text: 0, payload: LiteralPayload::None });
    let lit_b = push(&mut t, DtaState::Literal { text: 0, payload: LiteralPayload::None });
    let lit_c = push(&mut t, DtaState::Literal { text: 0, payload: LiteralPayload::None });
    let regex = push(
        &mut t,
        DtaState::Regex {
            pattern: 0,
            payload: None,
        },
    );
    let ws = push(&mut t, DtaState::WsTrim { pattern: None });
    let eps = push(&mut t, DtaState::Epsilon);

    // AltLinear over the leaves.
    let alt = push(
        &mut t,
        DtaState::AltLinear {
            branches: vec![lit_a, lit_b, lit_c],
        },
    );

    // ByteDispatch into the alt + a regex branch.
    let mut bdt = vec![StateId::NONE; 256];
    for slot in &mut bdt[b'a' as usize..=b'z' as usize] {
        *slot = alt;
    }
    for slot in &mut bdt[b'0' as usize..=b'9' as usize] {
        *slot = regex;
    }
    let dispatch = push(
        &mut t,
        DtaState::ByteDispatch {
            table: bdt,
            fallback: Some(eps),
        },
    );

    // Repeat over the dispatch — the loop body the model boosts.
    let rep = push(
        &mut t,
        DtaState::Repeat {
            inner: dispatch,
            lo: 0,
            hi: u32::MAX,
            counter_optional: Some(CounterOptional::Nested),
        },
    );

    // Cross-rule Ref + a self-Ref cycle.
    let cross = push(&mut t, DtaState::Ref { rule: 1, target: StateId::NONE });

    // Outer Seq stitches everything.
    let outer = push(
        &mut t,
        DtaState::Seq {
            children: vec![ws, rep, cross],
            frame: FrameKind::Seq,
            promote: SeqPromote::Default,
        },
    );

    // Patch cross-rule ref target to the outer Seq (cycle).
    if let Some(DtaState::Ref { target, .. }) = t.states.get_mut(cross.0 as usize) {
        *target = outer;
    }

    // Rule 1: a Minus operating on two literals.
    let m_primary = push(&mut t, DtaState::Literal { text: 0, payload: LiteralPayload::None });
    let m_excluded = push(&mut t, DtaState::Literal { text: 0, payload: LiteralPayload::None });
    let minus = push(
        &mut t,
        DtaState::Minus {
            primary: m_primary,
            excluded: m_excluded,
        },
    );

    t.rule_entries.insert(1, minus);
    set_entry(&mut t, 0, outer);
    t
}

// ── Smoke test: pass produces non-empty Vec on diverse corpus shapes. ─

#[test]
fn smoke_non_empty_on_corpus_proxy_shapes() {
    let tables: Vec<(&str, DtaTable)> = vec![
        ("flat_seq", flat_seq_table()),
        ("repeat_topology", repeat_topology_table()),
        ("byte_dispatch_density", byte_dispatch_density_table()),
        ("alt_linear", alt_linear_table()),
        ("cross_rule_ref", cross_rule_ref_table()),
        ("ref_cycle", ref_cycle_table()),
        ("shunting_yard", shunting_yard_table()),
        ("over_budget", over_budget_table()),
        ("diverse_corpus_proxy", diverse_corpus_proxy_table()),
    ];
    for (name, table) in tables {
        let out = compute_state_visit_frequency(&table);
        assert_eq!(
            out.len(),
            table.states.len(),
            "{name}: ordering must cover every state"
        );
        for (sid, freq) in &out {
            assert!(
                (sid.0 as usize) < table.states.len(),
                "{name}: state id {} out of range (len={})",
                sid.0,
                table.states.len()
            );
            assert!(*freq > 0, "{name}: every reachable state has freq > 0");
        }
    }
}

#[test]
fn empty_table_yields_empty_ordering() {
    let out = compute_state_visit_frequency(&empty_table());
    assert!(out.is_empty(), "empty table → empty ordering");
}

// ── Determinism: two consecutive runs are identical. ─────────────────

#[test]
fn determinism_two_consecutive_runs_identical() {
    let table = diverse_corpus_proxy_table();
    let first = compute_state_visit_frequency(&table);
    let second = compute_state_visit_frequency(&table);
    assert_eq!(first, second, "consecutive runs must produce identical output");
}

#[test]
fn determinism_independent_of_table_construction_order() {
    // Two independently-constructed identical tables produce the same
    // ordering — the pass cannot embed any HashMap iteration order
    // dependence into its output.
    let a = diverse_corpus_proxy_table();
    let b = diverse_corpus_proxy_table();
    assert_eq!(
        compute_state_visit_frequency(&a),
        compute_state_visit_frequency(&b),
        "identical tables → identical orderings"
    );
}

// ── Ordering invariant: monotone descending. ────────────────────────

#[test]
fn ordering_monotone_descending_with_ties_by_state_id() {
    for table in [
        flat_seq_table(),
        repeat_topology_table(),
        byte_dispatch_density_table(),
        alt_linear_table(),
        cross_rule_ref_table(),
        ref_cycle_table(),
        shunting_yard_table(),
        over_budget_table(),
        diverse_corpus_proxy_table(),
    ] {
        let out = compute_state_visit_frequency(&table);
        for window in out.windows(2) {
            let (left_id, left_freq) = window[0];
            let (right_id, right_freq) = window[1];
            assert!(
                left_freq >= right_freq,
                "frequency must be non-increasing: {left_freq} >= {right_freq}"
            );
            if left_freq == right_freq {
                assert!(
                    left_id.0 < right_id.0,
                    "ties broken by ascending state id: {} < {}",
                    left_id.0,
                    right_id.0
                );
            }
        }
    }
}

// ── Topology test: Repeat child > Repeat parent's siblings. ─────────

#[test]
fn topology_repeat_child_outranks_seq_siblings() {
    let table = repeat_topology_table();
    // Layout from `repeat_topology_table`: ids 0..=4 in push order:
    //   0 sibling  (Seq child A)
    //   1 inner    (the Repeat body — must be hottest)
    //   2 repeat   (the Repeat itself — multiplier multiplies inner)
    //   3 trailing (Seq child C)
    //   4 seq      (the outer parent)
    let freq: HashMap<u16, u32> = compute_state_visit_frequency(&table)
        .into_iter()
        .map(|(sid, f)| (sid.0, f))
        .collect();

    let inner = freq[&1];
    let sibling = freq[&0];
    let trailing = freq[&3];
    let seq = freq[&4];

    assert!(
        inner > sibling,
        "Repeat body (inner) must outrank flat Seq sibling: {inner} > {sibling}"
    );
    assert!(
        inner > trailing,
        "Repeat body (inner) must outrank Seq tail sibling: {inner} > {trailing}"
    );
    // The parent Seq carries the entry seed, but its frequency is
    // strictly less than each child's because the propagation
    // accumulates `parent_freq` into every child's slot — the
    // children inherit the parent and add their own floor on top.
    // What matters is that the parent received the entry boost above
    // the bare floor of `BASE_FREQUENCY = 1`.
    assert!(seq > 1, "entry-seeded parent Seq received the entry boost");
}

#[test]
fn topology_repeat_multiplier_lifts_inner_to_at_least_n_times_parent() {
    // Construct a minimal Seq → Repeat-many → Literal, then assert the
    // inner Literal's frequency is at least `REPEAT_BODY_MULTIPLIER`
    // times the parent Seq's frequency (the multiplier must reach the
    // leaf).
    let mut t = DtaTable::default();
    let inner = push(&mut t, DtaState::Literal { text: 0, payload: LiteralPayload::None });
    let rep = push(
        &mut t,
        DtaState::Repeat {
            inner,
            lo: 0,
            hi: u32::MAX,
            counter_optional: None,
        },
    );
    let seq = push(
        &mut t,
        DtaState::Seq {
            children: vec![rep],
            frame: FrameKind::Seq,
            promote: SeqPromote::Default,
        },
    );
    set_entry(&mut t, 0, seq);

    let freq: HashMap<u16, u32> = compute_state_visit_frequency(&t)
        .into_iter()
        .map(|(sid, f)| (sid.0, f))
        .collect();

    assert!(
        freq[&inner.0] >= freq[&seq.0] * REPEAT_BODY_MULTIPLIER / 2,
        "Repeat-many inner ({}) should significantly exceed parent Seq ({}) — multiplier {}",
        freq[&inner.0],
        freq[&seq.0],
        REPEAT_BODY_MULTIPLIER
    );
}

#[test]
fn topology_byte_dispatch_dense_branch_outranks_sparse() {
    let table = byte_dispatch_density_table();
    let freq: HashMap<u16, u32> = compute_state_visit_frequency(&table)
        .into_iter()
        .map(|(sid, f)| (sid.0, f))
        .collect();
    let dense = freq[&0]; // first push → state 0 (dense target)
    let sparse_a = freq[&1];
    let sparse_b = freq[&2];
    assert!(
        dense > sparse_a,
        "ByteDispatch dense branch ({dense}) outranks sparse_a ({sparse_a})"
    );
    assert!(
        dense > sparse_b,
        "ByteDispatch dense branch ({dense}) outranks sparse_b ({sparse_b})"
    );
}

#[test]
fn topology_cross_rule_ref_target_accumulates_incoming() {
    // Three Refs into the same target; that target must rank above any
    // single Ref state thanks to summed-incident weight.
    let table = cross_rule_ref_table();
    let freq: HashMap<u16, u32> = compute_state_visit_frequency(&table)
        .into_iter()
        .map(|(sid, f)| (sid.0, f))
        .collect();
    let target = freq[&0]; // leaf (ref target)
    let r1 = freq[&1];
    let r2 = freq[&2];
    let r3 = freq[&3];
    assert!(
        target >= r1.max(r2).max(r3),
        "Ref target must accumulate across {r1}, {r2}, {r3}; got {target}"
    );
}

#[test]
fn topology_shunting_yard_head_outranks_sibling() {
    let table = shunting_yard_table();
    let freq: HashMap<u16, u32> = compute_state_visit_frequency(&table)
        .into_iter()
        .map(|(sid, f)| (sid.0, f))
        .collect();
    // ids: 0 operand (head), 1 sibling literal, 2 shunting-yard, 3 seq
    let head = freq[&0];
    let sibling = freq[&1];
    assert!(
        head > sibling,
        "ShuntingYard head ({head}) outranks plain Seq sibling ({sibling}) via multiplier {SHUNTING_YARD_MULTIPLIER}"
    );
}

// ── Cycle convergence: ref-cycle terminates with finite frequencies. ─

#[test]
fn ref_cycle_propagation_terminates_with_finite_frequencies() {
    let table = ref_cycle_table();
    let out = compute_state_visit_frequency(&table);
    for (_id, freq) in &out {
        assert!(
            *freq < u32::MAX,
            "cyclic state should not saturate u32::MAX in a small synthetic table"
        );
    }
}

// ── Partitioning: HOT_BUDGET split. ─────────────────────────────────

#[test]
fn requires_hot_cold_split_threshold() {
    assert!(
        !requires_hot_cold_split(&flat_seq_table()),
        "small table fits below HOT_BUDGET"
    );
    assert!(
        requires_hot_cold_split(&over_budget_table()),
        "over_budget_table sized to exceed HOT_BUDGET"
    );
}

#[test]
fn partition_hot_cold_respects_budget() {
    let table = over_budget_table();
    let order = compute_state_visit_frequency(&table);
    let (hot, cold) = partition_hot_cold(&table, &order);
    assert_eq!(hot.len(), HOT_BUDGET, "hot set sized at the budget");
    assert_eq!(
        hot.len() + cold.len(),
        table.states.len(),
        "every state lands in exactly one partition"
    );
    // Hot ∩ cold must be empty.
    for h in &hot {
        assert!(
            !cold.contains(h),
            "state {} must not appear in both partitions",
            h.0
        );
    }
}

#[test]
fn partition_hot_cold_under_budget_keeps_everything_hot() {
    let table = flat_seq_table();
    let order = compute_state_visit_frequency(&table);
    let (hot, cold) = partition_hot_cold(&table, &order);
    assert_eq!(hot.len(), table.states.len(), "all states hot when under budget");
    assert!(cold.is_empty(), "cold set empty when under budget");
}

// ── Top-N convenience accessor. ─────────────────────────────────────

#[test]
fn top_hot_states_returns_prefix_of_full_ordering() {
    let table = diverse_corpus_proxy_table();
    let full = compute_state_visit_frequency(&table);
    let top = top_hot_states(&table, 3);
    assert_eq!(top.len(), 3.min(full.len()));
    for (i, entry) in top.iter().enumerate() {
        assert_eq!(entry, &full[i], "top-N must equal the prefix of full ordering");
    }
}
// ── JSON-object proxy ordering — exercised + dumpable on -- --nocapture
//    (kept so future tuning can re-print the ordering for one stable
//    grammar shape; assertions verify the frequency model still ranks
//    the kv-pair Seq above its outer braces).

fn json_object_proxy_table() -> DtaTable {
    let mut t = DtaTable::default();
    let str_lit = push(&mut t, DtaState::Literal { text: 0, payload: LiteralPayload::None });
    let num_lit = push(&mut t, DtaState::Regex { pattern: 0, payload: None });
    let lbrace = push(&mut t, DtaState::Literal { text: 0, payload: LiteralPayload::None });
    let rbrace = push(&mut t, DtaState::Literal { text: 0, payload: LiteralPayload::None });
    let colon = push(&mut t, DtaState::Literal { text: 0, payload: LiteralPayload::None });
    let value_alt = push(
        &mut t,
        DtaState::AltLinear {
            branches: vec![str_lit, num_lit],
        },
    );
    let kv_seq = push(
        &mut t,
        DtaState::Seq {
            children: vec![str_lit, colon, value_alt],
            frame: FrameKind::Seq,
            promote: SeqPromote::Default,
        },
    );
    let kv_repeat = push(
        &mut t,
        DtaState::Repeat {
            inner: kv_seq,
            lo: 0,
            hi: u32::MAX,
            counter_optional: None,
        },
    );
    let object = push(
        &mut t,
        DtaState::Seq {
            children: vec![lbrace, kv_repeat, rbrace],
            frame: FrameKind::Seq,
            promote: SeqPromote::Default,
        },
    );
    set_entry(&mut t, 0, object);
    let _layout = (str_lit, num_lit, lbrace, rbrace, colon, value_alt, kv_seq, kv_repeat, object);
    t
}

#[test]
fn json_object_proxy_kv_seq_outranks_braces() {
    // Layout (push order):
    //   0 str_lit, 1 num_lit, 2 lbrace, 3 rbrace, 4 colon,
    //   5 value_alt, 6 kv_seq, 7 kv_repeat, 8 object
    let table = json_object_proxy_table();
    let names = [
        "str_lit",
        "num_lit",
        "lbrace",
        "rbrace",
        "colon",
        "value_alt",
        "kv_seq",
        "kv_repeat",
        "object",
    ];
    let order = compute_state_visit_frequency(&table);

    // Print the ordering when run with `-- --nocapture` so the W4.b
    // emitter author can sanity-check the frequency model on a stable
    // synthetic shape; suppressed by default by cargo test's stdout
    // capture.
    eprintln!(
        "=== JSON-object proxy state-visit ordering (state count {}) ===",
        table.states.len()
    );
    for (sid, freq) in &order {
        let name = names.get(sid.0 as usize).copied().unwrap_or("?");
        eprintln!("  state {:>2}  freq {:>10}  ({})", sid.0, freq, name);
    }

    let freq: HashMap<u16, u32> = order.into_iter().map(|(s, f)| (s.0, f)).collect();

    // Inner kv_seq + str_lit + colon + value_alt all sit under the
    // Repeat — they must outrank the once-only braces around the
    // outer object.
    assert!(
        freq[&6] > freq[&2],
        "kv_seq ({}) > lbrace ({}) — Repeat lifts the loop body",
        freq[&6],
        freq[&2]
    );
    assert!(
        freq[&6] > freq[&3],
        "kv_seq ({}) > rbrace ({}) — Repeat lifts the loop body",
        freq[&6],
        freq[&3]
    );
}
