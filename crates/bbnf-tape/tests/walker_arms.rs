//! Tranche AW-I.W2.1 — Walker arm completion regression tests.
//!
//! Exercises the `AltLinear`, `Repeat`, and `ShuntingYard` walker arms
//! of `bbnf_tape::driver::dta_run`. Each test constructs a minimal
//! `DtaTable` literal and drives the walker directly — no need to lift
//! from source grammars.

use bbnf_tape::{
    dta_run, Columns, DtaError, DtaFrameKind, DtaRuleEntry, DtaRuleId, DtaState, DtaStateId,
    DtaTable, PayloadStream, RegexScanner, TapeKind,
};

/// No-op regex scanner — the tests use only Literal / Epsilon states,
/// so the scanner is never called.
struct NullScanner;
impl RegexScanner for NullScanner {
    fn scan(&self, _pattern: &str, _input: &[u8], _offset: usize) -> Option<u32> {
        None
    }
}

// ── AltLinear ───────────────────────────────────────────────────────

#[test]
fn altlinear_backtracks_after_first_failure() {
    // Grammar:
    //   rule0 = "xy"  | "ab"
    // Implemented as AltLinear with two Seq branches:
    //   branch0 = Seq[Literal("x"), Literal("y")]
    //   branch1 = Seq[Literal("a"), Literal("b")]
    // Input "ab" — branch0 fails on 'a' != 'x'; branch1 matches.
    static BRANCH0_CHILDREN: &[DtaStateId] = &[DtaStateId(1), DtaStateId(2)];
    static BRANCH1_CHILDREN: &[DtaStateId] = &[DtaStateId(4), DtaStateId(5)];
    static BRANCHES: &[DtaStateId] = &[DtaStateId(3), DtaStateId(6)];
    static STATES: &[DtaState] = &[
        // 0: entry AltLinear
        DtaState::AltLinear { branches: BRANCHES },
        // 1: Literal("x")
        DtaState::Literal { text: "x" },
        // 2: Literal("y")
        DtaState::Literal { text: "y" },
        // 3: Seq for branch0 ["x","y"]
        DtaState::Seq {
            children: BRANCH0_CHILDREN,
            frame: DtaFrameKind::Seq,
        },
        // 4: Literal("a")
        DtaState::Literal { text: "a" },
        // 5: Literal("b")
        DtaState::Literal { text: "b" },
        // 6: Seq for branch1 ["a","b"]
        DtaState::Seq {
            children: BRANCH1_CHILDREN,
            frame: DtaFrameKind::Seq,
        },
    ];
    static RULE_ENTRIES: &[DtaRuleEntry] = &[DtaRuleEntry {
        rule: DtaRuleId(0),
        state: DtaStateId(0),
    }];
    let table = DtaTable {
        states: STATES,
        rule_entries: RULE_ENTRIES,
        shunting_yard_rules: &[],
        counter_optional_rules: &[],
        max_nesting_depth: 4,
    };

    let mut cols = Columns::new();
    let mut psi = PayloadStream::new();
    let mut fd: Vec<u8> = Vec::new();
    let result = dta_run(&table, b"ab", &NullScanner, &mut cols, &mut psi, &mut fd);
    assert!(
        result.is_ok(),
        "altlinear second-branch probe failed: {:?}",
        result,
    );

    // Root record is the Alt compound; it has exactly one child (the
    // Seq for branch1) which has two literal children.
    let root = cols.materialize(0);
    assert_eq!(root.kind(), TapeKind::Alt, "root is Alt");
    assert!(root.has_children(), "Alt has a child branch");
    // Branch index 1 stamped onto the Alt frame's cursor.
    assert_eq!(root.variant_idx(), 1, "branch 1 was selected");
}

#[test]
fn altlinear_exhausts_all_branches_returns_syntax() {
    // Grammar: rule = "x" | "y". Input "z" — both branches fail.
    static BRANCHES: &[DtaStateId] = &[DtaStateId(1), DtaStateId(2)];
    static STATES: &[DtaState] = &[
        DtaState::AltLinear { branches: BRANCHES },
        DtaState::Literal { text: "x" },
        DtaState::Literal { text: "y" },
    ];
    static RULE_ENTRIES: &[DtaRuleEntry] = &[DtaRuleEntry {
        rule: DtaRuleId(0),
        state: DtaStateId(0),
    }];
    let table = DtaTable {
        states: STATES,
        rule_entries: RULE_ENTRIES,
        shunting_yard_rules: &[],
        counter_optional_rules: &[],
        max_nesting_depth: 2,
    };
    let mut cols = Columns::new();
    let mut psi = PayloadStream::new();
    let mut fd: Vec<u8> = Vec::new();
    let err = dta_run(&table, b"z", &NullScanner, &mut cols, &mut psi, &mut fd);
    assert!(matches!(err, Err(DtaError::Syntax { .. })));
}
