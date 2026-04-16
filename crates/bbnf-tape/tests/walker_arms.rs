//! Tranche AW-I.W2.1 — Walker arm completion regression tests.
//!
//! Exercises the `AltLinear`, `Repeat`, and `ShuntingYard` walker arms
//! of `bbnf_tape::driver::dta_run`. Each test constructs a minimal
//! `DtaTable` literal and drives the walker directly — no need to lift
//! from source grammars.

use bbnf_tape::{
    dta_run, Columns, DtaAssociativity, DtaError, DtaFrameKind, DtaPrecedenceEntry, DtaRuleEntry,
    DtaRuleId, DtaState, DtaStateId, DtaTable, PayloadStream, RegexScanner, TapeKind,
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

// ── Repeat ──────────────────────────────────────────────────────────

#[test]
fn repeat_iterates_to_hi() {
    // Grammar: rule = "a"{0, 3}. Input "aaa" — 3 iterations.
    static STATES: &[DtaState] = &[
        DtaState::Repeat {
            inner: DtaStateId(1),
            lo: 0,
            hi: 3,
            counter_optional: None,
        },
        DtaState::Literal { text: "a" },
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
    let result = dta_run(&table, b"aaa", &NullScanner, &mut cols, &mut psi, &mut fd);
    assert!(result.is_ok(), "repeat {{0,3}} on aaa: {:?}", result);
    let root = cols.materialize(0);
    assert_eq!(root.kind(), TapeKind::Rule);
    assert!(root.has_children());
    for i in 1..=3 {
        let rec = cols.materialize(i);
        assert_eq!(rec.kind(), TapeKind::Literal, "child {} is literal", i);
    }
}

#[test]
fn repeat_many1_rejects_empty() {
    // Grammar: rule = "a"+ (lo=1, hi=u32::MAX). Empty input → Syntax.
    static STATES: &[DtaState] = &[
        DtaState::Repeat {
            inner: DtaStateId(1),
            lo: 1,
            hi: u32::MAX,
            counter_optional: None,
        },
        DtaState::Literal { text: "a" },
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
    let result = dta_run(&table, b"", &NullScanner, &mut cols, &mut psi, &mut fd);
    assert!(
        matches!(result, Err(DtaError::Syntax { .. })),
        "many1 on empty input must error: {:?}",
        result,
    );
}

#[test]
fn repeat_optional_admits_empty() {
    // Grammar: rule = "a"? (lo=0, hi=1). Empty input closes at count 0.
    static STATES: &[DtaState] = &[
        DtaState::Repeat {
            inner: DtaStateId(1),
            lo: 0,
            hi: 1,
            counter_optional: None,
        },
        DtaState::Literal { text: "a" },
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
    let result = dta_run(&table, b"", &NullScanner, &mut cols, &mut psi, &mut fd);
    assert!(result.is_ok(), "optional on empty input: {:?}", result);
    let root = cols.materialize(0);
    assert_eq!(root.kind(), TapeKind::Rule);
    assert!(!root.has_children());
}

// ── ShuntingYard ────────────────────────────────────────────────────

#[test]
fn shunting_yard_left_associative_add() {
    // Grammar: expr = primary (+|*) primary ...
    //   prec['+'] = 10, left
    //   prec['*'] = 20, left
    // Input: "1+2*3" — result (+ 1 (* 2 3)).
    static PRECEDENCE: &[DtaPrecedenceEntry] = &[
        DtaPrecedenceEntry {
            byte: b'+',
            second_byte: None,
            precedence: 10,
            associativity: DtaAssociativity::Left,
            op_rule: DtaRuleId(1),
            op_discriminant: 0,
        },
        DtaPrecedenceEntry {
            byte: b'*',
            second_byte: None,
            precedence: 20,
            associativity: DtaAssociativity::Left,
            op_rule: DtaRuleId(2),
            op_discriminant: 0,
        },
    ];
    static DIGITS_BRANCHES: &[DtaStateId] = &[
        DtaStateId(2),
        DtaStateId(3),
        DtaStateId(4),
        DtaStateId(5),
        DtaStateId(6),
        DtaStateId(7),
        DtaStateId(8),
        DtaStateId(9),
        DtaStateId(10),
        DtaStateId(11),
    ];
    static STATES: &[DtaState] = &[
        // 0: ShuntingYard(head=1, precedence=above)
        DtaState::ShuntingYard {
            head: DtaStateId(1),
            precedence: PRECEDENCE,
        },
        // 1: head = AltLinear over single-digit literals 0..9
        DtaState::AltLinear {
            branches: DIGITS_BRANCHES,
        },
        // 2..11: Literal("0") .. Literal("9")
        DtaState::Literal { text: "0" },
        DtaState::Literal { text: "1" },
        DtaState::Literal { text: "2" },
        DtaState::Literal { text: "3" },
        DtaState::Literal { text: "4" },
        DtaState::Literal { text: "5" },
        DtaState::Literal { text: "6" },
        DtaState::Literal { text: "7" },
        DtaState::Literal { text: "8" },
        DtaState::Literal { text: "9" },
    ];
    static RULE_ENTRIES: &[DtaRuleEntry] = &[DtaRuleEntry {
        rule: DtaRuleId(0),
        state: DtaStateId(0),
    }];
    let table = DtaTable {
        states: STATES,
        rule_entries: RULE_ENTRIES,
        shunting_yard_rules: &[DtaRuleId(0)],
        counter_optional_rules: &[],
        max_nesting_depth: 8,
    };
    let mut cols = Columns::new();
    let mut psi = PayloadStream::new();
    let mut fd: Vec<u8> = Vec::new();
    let result = dta_run(&table, b"1+2*3", &NullScanner, &mut cols, &mut psi, &mut fd);
    assert!(result.is_ok(), "1+2*3 parse: {:?}", result);

    // Outer SY root's child_off points at the final reduced operand.
    // For left-assoc `1+2*3`, the reducer emits mul(2,3) first then
    // add(1, mul(2,3)) — the latter is the final reduced root.
    let root = cols.materialize(0);
    assert_eq!(root.kind(), TapeKind::Rule, "outer SY root is Rule");
    assert!(root.has_children(), "outer SY root has the final reduced subtree");
    let final_idx = root.child_off.0;
    let final_op = cols.materialize(final_idx);
    assert_eq!(final_op.kind(), TapeKind::Rule, "final op is Rule compound");
    assert_eq!(
        final_op.variant_idx(),
        0,
        "top-level '+' has op_discriminant 0",
    );
    assert_eq!(final_op.span_lo, 0);
    assert_eq!(final_op.span_hi, 5);
}

#[test]
fn shunting_yard_right_associative_pow() {
    // Grammar: expr = primary ^ primary ...
    //   prec['^'] = 30, right
    // Input: "2^3^4" — result (^ 2 (^ 3 4)).
    static PRECEDENCE: &[DtaPrecedenceEntry] = &[DtaPrecedenceEntry {
        byte: b'^',
        second_byte: None,
        precedence: 30,
        associativity: DtaAssociativity::Right,
        op_rule: DtaRuleId(1),
        op_discriminant: 1,
    }];
    static DIGITS_BRANCHES: &[DtaStateId] = &[
        DtaStateId(2),
        DtaStateId(3),
        DtaStateId(4),
        DtaStateId(5),
        DtaStateId(6),
        DtaStateId(7),
        DtaStateId(8),
        DtaStateId(9),
        DtaStateId(10),
        DtaStateId(11),
    ];
    static STATES: &[DtaState] = &[
        DtaState::ShuntingYard {
            head: DtaStateId(1),
            precedence: PRECEDENCE,
        },
        DtaState::AltLinear {
            branches: DIGITS_BRANCHES,
        },
        DtaState::Literal { text: "0" },
        DtaState::Literal { text: "1" },
        DtaState::Literal { text: "2" },
        DtaState::Literal { text: "3" },
        DtaState::Literal { text: "4" },
        DtaState::Literal { text: "5" },
        DtaState::Literal { text: "6" },
        DtaState::Literal { text: "7" },
        DtaState::Literal { text: "8" },
        DtaState::Literal { text: "9" },
    ];
    static RULE_ENTRIES: &[DtaRuleEntry] = &[DtaRuleEntry {
        rule: DtaRuleId(0),
        state: DtaStateId(0),
    }];
    let table = DtaTable {
        states: STATES,
        rule_entries: RULE_ENTRIES,
        shunting_yard_rules: &[DtaRuleId(0)],
        counter_optional_rules: &[],
        max_nesting_depth: 8,
    };
    let mut cols = Columns::new();
    let mut psi = PayloadStream::new();
    let mut fd: Vec<u8> = Vec::new();
    let result = dta_run(&table, b"2^3^4", &NullScanner, &mut cols, &mut psi, &mut fd);
    assert!(result.is_ok(), "2^3^4 parse: {:?}", result);

    // Right-assoc: the reducer emits the inner ^(3,4) first then the
    // outer ^(2, ^(3,4)); the outer SY root's child_off points at
    // the outer ^.
    let root = cols.materialize(0);
    assert_eq!(root.kind(), TapeKind::Rule);
    assert!(root.has_children());
    let top_idx = root.child_off.0;
    let top_op = cols.materialize(top_idx);
    assert_eq!(top_op.kind(), TapeKind::Rule);
    assert_eq!(top_op.variant_idx(), 1, "^ discriminant = 1");
    assert_eq!(top_op.span_lo, 0);
    assert_eq!(top_op.span_hi, 5);
}
