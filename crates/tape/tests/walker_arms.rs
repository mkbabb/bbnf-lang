//! Tranche AW-I.W2.1 — Walker arm completion regression tests.
//!
//! Exercises the `AltLinear`, `Repeat`, and `ShuntingYard` walker arms
//! of `tape::driver::dta_run_cold`. Each test constructs a
//! minimal [`DtaTable`] literal and drives the cold-path walker
//! directly — no need to lift from source grammars.
//!
//! AW-III.W4.c — these tests target the cold-path replay surface
//! (`dta_run_cold`). The hot-path emitted walker (W4.b) preserves
//! the same arm semantics by mechanical lowering, so any deviation
//! caught here also catches a deviation in the emitted walker.

use tape::{
    dta_run_cold, Columns, DtaAssociativity, DtaError, DtaFrameKind, DtaPrecedenceEntry,
    DtaRuleEntry, DtaRuleId, DtaState, DtaStateId, DtaTable, PayloadStream, TapeKind,
};

/// AW-IV.W1.β — null regex-scan fn pointer. These tests drive
/// Literal / Epsilon / AltLinear / Repeat / ShuntingYard states only;
/// the cold path never consults the scan fn. Passing a never-matches
/// closure as `fn(&str, &[u8], usize) -> Option<u32>` satisfies the
/// signature without dragging in parse-that's Dfa machinery.
fn null_regex_scan(_pattern: &str, _input: &[u8], _pos: usize) -> Option<u32> {
    None
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
        DtaState::Literal { text: "x", payload: tape::LiteralPayload::None },
        // 2: Literal("y")
        DtaState::Literal { text: "y", payload: tape::LiteralPayload::None },
        // 3: Seq for branch0 ["x","y"]
        DtaState::Seq {
            children: BRANCH0_CHILDREN,
            frame: DtaFrameKind::Seq,
            promote: tape::SeqPromote::Default,
        },
        // 4: Literal("a")
        DtaState::Literal { text: "a", payload: tape::LiteralPayload::None },
        // 5: Literal("b")
        DtaState::Literal { text: "b", payload: tape::LiteralPayload::None },
        // 6: Seq for branch1 ["a","b"]
        DtaState::Seq {
            children: BRANCH1_CHILDREN,
            frame: DtaFrameKind::Seq,
            promote: tape::SeqPromote::Default,
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
        entry: DtaRuleId(0),
    };

    let mut cols = Columns::new();
    let mut psi = PayloadStream::new();
    let mut fd: Vec<u8> = Vec::new();
    let result = dta_run_cold(&table, b"ab", null_regex_scan, &mut cols, &mut psi, &mut fd);
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
        DtaState::Literal { text: "x", payload: tape::LiteralPayload::None },
        DtaState::Literal { text: "y", payload: tape::LiteralPayload::None },
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
        entry: DtaRuleId(0),
    };
    let mut cols = Columns::new();
    let mut psi = PayloadStream::new();
    let mut fd: Vec<u8> = Vec::new();
    let err = dta_run_cold(&table, b"z", null_regex_scan, &mut cols, &mut psi, &mut fd);
    assert!(matches!(err, Err(DtaError::Syntax { .. })));
}

#[test]
fn altlinear_nested_paren_group() {
    // Grammar:
    //   expr    = group | literal   (AltLinear)
    //   group   = "(" expr ")"      (Seq — self-recursive through expr)
    //   literal = "x"               (Literal)
    // Input "(x)".
    //
    // Reproduces the bbnf bootstrap failure on `a = ( "x" ) ;` — paren-
    // wrapped expressions fail when the enclosing AltLinear's branch is
    // the `group` Seq and the Seq's middle child dispatches back to
    // `expr` recursively. The bug surfaces because the inner AltLinear,
    // trying `group` first, fails on `x` (input doesn't start with "("),
    // which propagates a Syntax error up through `try_branch`. The
    // outer AltLinear's savepoint-restore truncated past the OUTER Alt
    // frame's parent_rec reservation — but when the INNER Alt surfaces
    // a branch failure, the inner's own body-failure-restore must not
    // rewind the OUTER Alt frame's state.
    //
    // Pre-fix expectation: this test fails because the inner AltLinear's
    // first-branch failure propagates through `try_branch`'s
    // stop_depth-bounded loop, but the bounded loop then re-enters
    // the main dispatcher which dispatches against the OUTER Alt's
    // state context, losing the inner recursion's frame correctness.
    static EXPR_BRANCHES: &[DtaStateId] = &[DtaStateId(1), DtaStateId(5)];
    // group = Seq[Literal("("), Ref(expr), Literal(")")]
    static GROUP_CHILDREN: &[DtaStateId] = &[DtaStateId(2), DtaStateId(3), DtaStateId(4)];
    static STATES: &[DtaState] = &[
        // 0: expr = AltLinear[group, literal]
        DtaState::AltLinear { branches: EXPR_BRANCHES },
        // 1: group = Seq["(", expr, ")"]
        DtaState::Seq {
            children: GROUP_CHILDREN,
            frame: DtaFrameKind::Seq,
            promote: tape::SeqPromote::Default,
        },
        // 2: "("
        DtaState::Literal { text: "(", payload: tape::LiteralPayload::None },
        // 3: expr self-ref → dispatch back to state 0.
        DtaState::Ref {
            rule: DtaRuleId(0),
            target: DtaStateId(0),
        },
        // 4: ")"
        DtaState::Literal { text: ")", payload: tape::LiteralPayload::None },
        // 5: literal = "x"
        DtaState::Literal { text: "x", payload: tape::LiteralPayload::None },
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
        max_nesting_depth: 8,
        entry: DtaRuleId(0),
    };

    let mut cols = Columns::new();
    let mut psi = PayloadStream::new();
    let mut fd: Vec<u8> = Vec::new();
    let result = dta_run_cold(&table, b"(x)", null_regex_scan, &mut cols, &mut psi, &mut fd);
    assert!(
        result.is_ok(),
        "nested paren-group parse of `(x)` failed: {:?}",
        result,
    );

    // Root: outer Alt selecting branch 0 (group). Contains a Seq child
    // with 3 descendants: "(", inner Alt, ")".
    let root = cols.materialize(0);
    assert_eq!(root.kind(), TapeKind::Alt, "root is Alt");
    assert!(root.has_children());
    assert_eq!(root.variant_idx(), 0, "outer Alt selected group branch (0)");
    assert_eq!(root.span_lo, 0);
    assert_eq!(root.span_hi, 3, "outer Alt covers the full `(x)`");
}

#[test]
fn altlinear_branch_fails_after_nested_ref_alt_partial() {
    // Reproduces bbnf bootstrap's paren-expression failure mode by
    // mimicking the exact shape: an outer AltLinear whose branches
    // dispatch through a Ref into ANOTHER AltLinear, and at least one
    // early branch partially succeeds (consumes bytes) before failing
    // deep inside — forcing the outer Alt to restore state and try the
    // next branch.
    //
    // Grammar shape:
    //   outer = inner_alt_b | paren_group
    //   inner_alt_b = "b" , "z"        (consumes "b" then fails on "z")
    //   paren_group = "(" , outer , ")"
    //   inner_alt_b is tried FIRST. Input "(b)" starts with "(" so
    //   inner_alt_b fails immediately. But input "(bz)" — inside the
    //   paren, outer recurses: inner_alt_b matches "b" then "z". OK.
    //   The TEST case is input "(b)": outer is tried, first branch
    //   inner_alt_b fails on "(", second paren_group succeeds.
    //
    //   Now make the fail DEEPER: make the first branch itself
    //   dispatch via a Ref into an Alt, so the failure happens two
    //   frames deep.
    //
    //   outer = a_or_b | paren_group
    //   a_or_b = a_literal | b_literal   (Ref → Alt)
    //   paren_group = "(" , outer , ")"
    //   Input "(a)": outer tries a_or_b → dispatches to AltLinear{a,b}
    //   → tries "a" → fails on "(". Tries "b" → fails. a_or_b errors.
    //   Outer retries with paren_group: matches "(", recurses to outer,
    //   tries a_or_b again → a matches. Matches ")". Done.
    static OUTER_BRANCHES: &[DtaStateId] = &[DtaStateId(1), DtaStateId(4)];
    static A_OR_B_BRANCHES: &[DtaStateId] = &[DtaStateId(2), DtaStateId(3)];
    static GROUP_CHILDREN: &[DtaStateId] = &[DtaStateId(5), DtaStateId(6), DtaStateId(7)];
    static STATES: &[DtaState] = &[
        // 0: outer = AltLinear[ref→a_or_b, paren_group]
        DtaState::AltLinear { branches: OUTER_BRANCHES },
        // 1: Ref to a_or_b — target UNRESOLVED (DtaStateId::NONE).
        // The walker MUST resolve via `rule_entry_for(rule)`. This
        // mirrors bbnf's generated.rs where 11 Ref entries have
        // `target: DtaStateId(65535)` and rely on the runtime lookup.
        DtaState::Ref {
            rule: DtaRuleId(1),
            target: DtaStateId::NONE,
        },
        // 2: Literal("a")
        DtaState::Literal { text: "a", payload: tape::LiteralPayload::None },
        // 3: Literal("b")
        DtaState::Literal { text: "b", payload: tape::LiteralPayload::None },
        // 4: paren_group = Seq["(", outer_ref, ")"]
        DtaState::Seq {
            children: GROUP_CHILDREN,
            frame: DtaFrameKind::Seq,
            promote: tape::SeqPromote::Default,
        },
        // 5: "("
        DtaState::Literal { text: "(", payload: tape::LiteralPayload::None },
        // 6: Ref to outer (self-recursive)
        DtaState::Ref {
            rule: DtaRuleId(0),
            target: DtaStateId(0),
        },
        // 7: ")"
        DtaState::Literal { text: ")", payload: tape::LiteralPayload::None },
        // 8: a_or_b = AltLinear[a, b]  (entry of rule 1)
        DtaState::AltLinear { branches: A_OR_B_BRANCHES },
    ];
    static RULE_ENTRIES: &[DtaRuleEntry] = &[
        DtaRuleEntry {
            rule: DtaRuleId(0),
            state: DtaStateId(0),
        },
        DtaRuleEntry {
            rule: DtaRuleId(1),
            state: DtaStateId(8),
        },
    ];
    let table = DtaTable {
        states: STATES,
        rule_entries: RULE_ENTRIES,
        shunting_yard_rules: &[],
        counter_optional_rules: &[],
        max_nesting_depth: 8,
        entry: DtaRuleId(0),
    };

    let mut cols = Columns::new();
    let mut psi = PayloadStream::new();
    let mut fd: Vec<u8> = Vec::new();
    let result = dta_run_cold(&table, b"(a)", null_regex_scan, &mut cols, &mut psi, &mut fd);
    assert!(
        result.is_ok(),
        "nested-ref Alt paren parse of `(a)` failed: {:?}",
        result,
    );
}

#[test]
fn altlinear_branch_partial_match_then_fails_restores_pos() {
    // Branch 0 matches ONE byte then fails on the second. Ensures
    // pos is correctly restored for branch 1.
    //
    // outer = ab_seq | a_then_end
    // ab_seq = "a" "b"          (tries; "a" matches, "b" fails)
    // a_then_end = "a"           (tries second; succeeds)
    static OUTER_BRANCHES: &[DtaStateId] = &[DtaStateId(1), DtaStateId(4)];
    static AB_CHILDREN: &[DtaStateId] = &[DtaStateId(2), DtaStateId(3)];
    static STATES: &[DtaState] = &[
        DtaState::AltLinear { branches: OUTER_BRANCHES },
        DtaState::Seq {
            children: AB_CHILDREN,
            frame: DtaFrameKind::Seq,
            promote: tape::SeqPromote::Default,
        },
        DtaState::Literal { text: "a", payload: tape::LiteralPayload::None },
        DtaState::Literal { text: "b", payload: tape::LiteralPayload::None },
        DtaState::Literal { text: "a", payload: tape::LiteralPayload::None },
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
        entry: DtaRuleId(0),
    };

    let mut cols = Columns::new();
    let mut psi = PayloadStream::new();
    let mut fd: Vec<u8> = Vec::new();
    let result = dta_run_cold(&table, b"a", null_regex_scan, &mut cols, &mut psi, &mut fd);
    assert!(result.is_ok(), "partial-match restore failed: {:?}", result);
    let root = cols.materialize(0);
    assert_eq!(root.variant_idx(), 1);
}

#[test]
fn altlinear_nested_paren_literal_only_works() {
    // Sanity counter-case: `a = "x"` works without paren wrapping.
    // If this ALSO fails the bug is wider than paren-specific.
    static EXPR_BRANCHES: &[DtaStateId] = &[DtaStateId(1), DtaStateId(5)];
    static GROUP_CHILDREN: &[DtaStateId] = &[DtaStateId(2), DtaStateId(3), DtaStateId(4)];
    static STATES: &[DtaState] = &[
        DtaState::AltLinear { branches: EXPR_BRANCHES },
        DtaState::Seq {
            children: GROUP_CHILDREN,
            frame: DtaFrameKind::Seq,
            promote: tape::SeqPromote::Default,
        },
        DtaState::Literal { text: "(", payload: tape::LiteralPayload::None },
        DtaState::Ref {
            rule: DtaRuleId(0),
            target: DtaStateId(0),
        },
        DtaState::Literal { text: ")", payload: tape::LiteralPayload::None },
        DtaState::Literal { text: "x", payload: tape::LiteralPayload::None },
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
        max_nesting_depth: 8,
        entry: DtaRuleId(0),
    };

    let mut cols = Columns::new();
    let mut psi = PayloadStream::new();
    let mut fd: Vec<u8> = Vec::new();
    let result = dta_run_cold(&table, b"x", null_regex_scan, &mut cols, &mut psi, &mut fd);
    assert!(result.is_ok(), "bare literal parse failed: {:?}", result);
    let root = cols.materialize(0);
    assert_eq!(root.kind(), TapeKind::Alt);
    assert_eq!(root.variant_idx(), 1, "selected literal branch (1)");
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
        DtaState::Literal { text: "a", payload: tape::LiteralPayload::None },
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
        entry: DtaRuleId(0),
    };
    let mut cols = Columns::new();
    let mut psi = PayloadStream::new();
    let mut fd: Vec<u8> = Vec::new();
    let result = dta_run_cold(&table, b"aaa", null_regex_scan, &mut cols, &mut psi, &mut fd);
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
        DtaState::Literal { text: "a", payload: tape::LiteralPayload::None },
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
        entry: DtaRuleId(0),
    };
    let mut cols = Columns::new();
    let mut psi = PayloadStream::new();
    let mut fd: Vec<u8> = Vec::new();
    let result = dta_run_cold(&table, b"", null_regex_scan, &mut cols, &mut psi, &mut fd);
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
        DtaState::Literal { text: "a", payload: tape::LiteralPayload::None },
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
        entry: DtaRuleId(0),
    };
    let mut cols = Columns::new();
    let mut psi = PayloadStream::new();
    let mut fd: Vec<u8> = Vec::new();
    let result = dta_run_cold(&table, b"", null_regex_scan, &mut cols, &mut psi, &mut fd);
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
        DtaState::Literal { text: "0", payload: tape::LiteralPayload::None },
        DtaState::Literal { text: "1", payload: tape::LiteralPayload::None },
        DtaState::Literal { text: "2", payload: tape::LiteralPayload::None },
        DtaState::Literal { text: "3", payload: tape::LiteralPayload::None },
        DtaState::Literal { text: "4", payload: tape::LiteralPayload::None },
        DtaState::Literal { text: "5", payload: tape::LiteralPayload::None },
        DtaState::Literal { text: "6", payload: tape::LiteralPayload::None },
        DtaState::Literal { text: "7", payload: tape::LiteralPayload::None },
        DtaState::Literal { text: "8", payload: tape::LiteralPayload::None },
        DtaState::Literal { text: "9", payload: tape::LiteralPayload::None },
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
        entry: DtaRuleId(0),
    };
    let mut cols = Columns::new();
    let mut psi = PayloadStream::new();
    let mut fd: Vec<u8> = Vec::new();
    let result = dta_run_cold(&table, b"1+2*3", null_regex_scan, &mut cols, &mut psi, &mut fd);
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
        DtaState::Literal { text: "0", payload: tape::LiteralPayload::None },
        DtaState::Literal { text: "1", payload: tape::LiteralPayload::None },
        DtaState::Literal { text: "2", payload: tape::LiteralPayload::None },
        DtaState::Literal { text: "3", payload: tape::LiteralPayload::None },
        DtaState::Literal { text: "4", payload: tape::LiteralPayload::None },
        DtaState::Literal { text: "5", payload: tape::LiteralPayload::None },
        DtaState::Literal { text: "6", payload: tape::LiteralPayload::None },
        DtaState::Literal { text: "7", payload: tape::LiteralPayload::None },
        DtaState::Literal { text: "8", payload: tape::LiteralPayload::None },
        DtaState::Literal { text: "9", payload: tape::LiteralPayload::None },
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
        entry: DtaRuleId(0),
    };
    let mut cols = Columns::new();
    let mut psi = PayloadStream::new();
    let mut fd: Vec<u8> = Vec::new();
    let result = dta_run_cold(&table, b"2^3^4", null_regex_scan, &mut cols, &mut psi, &mut fd);
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
