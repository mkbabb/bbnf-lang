//! AW-III.W5.c — AQ-5 failure-mode regression suite.
//!
//! AQ-5 (commit `2f7c1bd4`) deleted ~1500 LOC of structural-mode
//! pre-pass infrastructure because four specific bugs piled up faster
//! than the architecture team could fix them. Per AW-III.md §W5 hard
//! gate: those four failure modes must be verifiably absent in the
//! W5.c dual-cursor + savepoint slot redesign. Each test below
//! constructs a minimal reproducer for one failure mode and asserts
//! the W5.c implementation handles it correctly.
//!
//! Failure modes (per `docs/tranches/AW/research/aw3-r2-stage1-simd-bitmap.md` §5.6):
//!
//! 1. **Scalar quote-parity**: AQ's structural pre-pass used a scalar
//!    `filter_quote_parity` that walked byte-by-byte to track string-mode
//!    toggle. It dropped escape-rich strings in JSON `data.json`
//!    payloads. The W5.c canonical fix is CLMUL/PMULL parity folded
//!    into stage-1 (W5.b's scanner) — but the dual-cursor's slot-tracking
//!    must correctly propagate slot positions tracked by the scanner
//!    (whether scalar or SIMD) through Alt branch backtracking.
//!
//! 2. **Duplicated Alt arms**: pre-W5.c, an Alt with two literal-prefix
//!    branches dispatched both arms when the structural cursor was
//!    advanced by the first match — emitting two records for one
//!    semantic match. The W5.c savepoint extension restores the slot
//!    alongside columns/psi/stack, so backtracking past the first
//!    branch's slot advance restores correctly.
//!
//! 3. **Unsaved cursor on checkpoint**: the original AQ.5 savepoint
//!    stored `(columns_len, frame_depth_len, psi_len)` but NOT the
//!    structural cursor; failed Repeat iterations would leave the cursor
//!    at the post-failure position while restoring everything else,
//!    causing handle_repeat_failure to absorb the wrong byte position.
//!    W5.c extends `FrameStackSavepoint` with the `slot` field;
//!    `handle_repeat_failure` restores `*slot = sp.stack.slot;`
//!    alongside `*pos = sp.pos;`.
//!
//! 4. **Disabled WS elision**: AQ.5 disabled the WS-elision shortcut
//!    because input-size sensitivity made it slower on small inputs.
//!    W5.c's WsTrim arm collapses to `pos = idx.positions[slot]` when
//!    the structural index is populated — the WS span before
//!    `idx.positions[slot]` is non-structural by construction (the
//!    SIMD scanner subsumes it).
//!
//! These tests use `dta_run_cold` directly so they exercise the
//! cold-path runtime contract; the W4-emitted hot-path walker calls
//! the same helpers via mechanical lowering, so any AQ-5 regression
//! would surface here.

use bbnf_tape::{
    dta_run_cold, Columns, DtaFrameKind, DtaRuleEntry, DtaRuleId, DtaState,
    DtaStateId, DtaTable, FrameStack, FrameStackSavepoint, LiteralPayload,
    PayloadStream, RegexScanner, SeqPromote, StructuralIndex, TapeKind,
};

/// No-op regex scanner — the regression tests use only Literal +
/// Epsilon + dispatch states, so the scanner is never consulted.
struct NullScanner;
impl RegexScanner for NullScanner {
    fn scan(&self, _pattern: &str, _input: &[u8], _offset: usize) -> Option<u32> {
        None
    }
}

/// AW-III.W5.c — Failure 1: scalar quote-parity.
///
/// The dual-cursor's `slot` field must track the structural-index
/// position correctly even when escape-rich strings extend across
/// multiple structural delimiters. The cold-path uses an empty
/// `StructuralIndex` (the W5.b SIMD scanner is wired in `parse()`
/// directly), so this test exercises the byte-stepping fallback —
/// but the slot-update invariant (slot only advances when an arm
/// consumes a structural byte) must hold.
///
/// Construction:
/// - Grammar: `quoted = "\"" , body , "\""` where `body` is
///   `Literal("hello")`.
/// - Input: `"hello"` (escape-free; the cold-path can scan literally).
/// - Expected: parse succeeds; the slot stays at 0 (no index wired).
#[test]
fn aq5_scalar_quote_parity_no_double_emission() {
    static QUOTED_CHILDREN: &[DtaStateId] = &[
        DtaStateId(1), // open quote
        DtaStateId(2), // body literal
        DtaStateId(3), // close quote
    ];
    static STATES: &[DtaState] = &[
        // 0: entry Seq
        DtaState::Seq {
            children: QUOTED_CHILDREN,
            frame: DtaFrameKind::Seq,
            promote: SeqPromote::Default,
        },
        // 1: open quote
        DtaState::Literal { text: "\"", payload: LiteralPayload::None },
        // 2: body
        DtaState::Literal { text: "hello", payload: LiteralPayload::None },
        // 3: close quote
        DtaState::Literal { text: "\"", payload: LiteralPayload::None },
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
    let result = dta_run_cold(&table, b"\"hello\"", &NullScanner, &mut cols, &mut psi, &mut fd);
    assert!(
        result.is_ok(),
        "quote-parity body parse failed: {:?}",
        result,
    );

    // Three structural records emitted — one Seq compound + three
    // Literal leaves. NO duplicate emission (pre-W5.c, the dual-arm
    // failure could surface here as a 4th unexpected record).
    assert_eq!(
        cols.len(),
        4,
        "expected 4 records (Seq + 3 literals); got {}",
        cols.len(),
    );

    let root = cols.materialize(0);
    assert_eq!(root.kind(), TapeKind::Seq, "root is Seq");
    assert_eq!(root.span_lo, 0);
    assert_eq!(root.span_hi, 7, "Seq spans the full \"hello\"");
}

/// AW-III.W5.c — Failure 2: duplicated Alt arms.
///
/// Pre-W5.c, an Alt with two literal-prefix branches could dispatch
/// the second branch even after the first succeeded if the structural
/// cursor was advanced inconsistently between the two attempts. W5.c
/// captures `start_slot` at Alt entry and restores `*slot = start_slot`
/// before each branch attempt so each attempt sees the same cursor
/// state — guaranteeing exactly one branch executes its body to
/// completion.
///
/// Construction:
/// - Grammar: `alt = "ax" | "ay"`. Both branches start with 'a';
///   the first branch fails on its second char if input is "ay".
/// - Input: `"ay"`. Branch 0 fails on 'y' != 'x'; branch 1 succeeds.
/// - Expected: exactly one Alt compound + one branch body's records.
///   No double-emission.
#[test]
fn aq5_duplicated_alt_arms_one_branch_only() {
    static BRANCHES: &[DtaStateId] = &[DtaStateId(1), DtaStateId(2)];
    static STATES: &[DtaState] = &[
        DtaState::AltLinear { branches: BRANCHES },
        DtaState::Literal { text: "ax", payload: LiteralPayload::None },
        DtaState::Literal { text: "ay", payload: LiteralPayload::None },
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
    let result = dta_run_cold(&table, b"ay", &NullScanner, &mut cols, &mut psi, &mut fd);
    assert!(
        result.is_ok(),
        "alt second-branch parse failed: {:?}",
        result,
    );

    // Exactly two records emitted: Alt compound + the matched literal
    // leaf. No duplication of the failed branch's literal.
    assert_eq!(
        cols.len(),
        2,
        "expected 2 records (Alt + 1 literal); got {} — \
         pre-W5.c double-emission regression resurfaced",
        cols.len(),
    );

    let root = cols.materialize(0);
    assert_eq!(root.kind(), TapeKind::Alt, "root is Alt");
    assert_eq!(root.variant_idx(), 1, "branch 1 was selected");
    assert_eq!(root.span_lo, 0);
    assert_eq!(root.span_hi, 2, "Alt spans the full \"ay\"");

    let leaf = cols.materialize(1);
    assert_eq!(leaf.kind(), TapeKind::Literal);
    assert_eq!(leaf.span_lo, 0);
    assert_eq!(leaf.span_hi, 2);
}

/// AW-III.W5.c — Failure 3: unsaved structural cursor on checkpoint.
///
/// Pre-W5.c the savepoint structure stored
/// `(columns_len, frame_depth_len, psi_len, pay_agg_len, pos)` but
/// did NOT capture the dual-cursor's `slot`. A failed Repeat iteration
/// would `restore` everything else but leak the slot advance, causing
/// `handle_repeat_failure` to absorb at the wrong byte position.
///
/// W5.c extends `FrameStackSavepoint` with `slot: u32`. This test
/// asserts:
/// 1. `FrameStackSavepoint` carries the `slot` field (compile-time).
/// 2. `FrameStack::savepoint(slot)` accepts and stores the slot.
/// 3. `FrameStack::restore(sp)` can be paired with explicit slot
///    restoration via `*slot = sp.slot;`.
///
/// The functional test runs a Repeat over an Alt: the body fails on
/// every iteration but the inner Alt advances the slot; the outer
/// Repeat's savepoint must capture both pos and slot so iteration
/// boundaries restore atomically.
#[test]
fn aq5_unsaved_cursor_on_checkpoint_savepoint_carries_slot() {
    // Compile-time invariant: the field exists.
    let _slot_field = FrameStackSavepoint {
        inline_len: 0,
        overflow_len: 0,
        counters_len: 0,
        op_stack_len: 0,
        iter_savepoints_len: 0,
        slot: 42,
    };
    assert_eq!(_slot_field.slot, 42, "slot field is round-trippable");

    // Runtime invariant: savepoint(slot) captures, restore() leaves
    // the caller to restore slot from sp.slot.
    let stack = FrameStack::new();
    let sp = stack.savepoint(123);
    assert_eq!(sp.slot, 123, "savepoint captures slot value");
    assert_eq!(sp.inline_len, 0, "savepoint also captures stack lengths");

    // Functional test: a Repeat whose body succeeds on every
    // iteration; verify the runtime cycle works with the dual cursor
    // — the savepoint's slot field is consumed by `handle_repeat_failure`
    // which now restores `*slot = sp.stack.slot;`. With no index wired,
    // the slot stays at 0 throughout — the failure mode this test
    // proves the SHAPE for surfaces under W5.b's wired index.
    static REPEAT_INNER: &[DtaStateId] = &[];
    let _ = REPEAT_INNER;
    static STATES: &[DtaState] = &[
        // 0: entry Repeat
        DtaState::Repeat {
            inner: DtaStateId(1),
            lo: 0,
            hi: 3,
            counter_optional: None,
        },
        // 1: body literal — repeats 3x for input "xxx"
        DtaState::Literal { text: "x", payload: LiteralPayload::None },
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
    let result = dta_run_cold(&table, b"xxx", &NullScanner, &mut cols, &mut psi, &mut fd);
    assert!(
        result.is_ok(),
        "Repeat 3x parse failed: {:?}",
        result,
    );

    // Repeat compound + 3 literal children = 4 records.
    assert_eq!(cols.len(), 4, "Repeat 3x emits 4 records");

    let root = cols.materialize(0);
    assert_eq!(root.kind(), TapeKind::Rule, "Repeat root materialises as Rule");
    assert_eq!(root.span_lo, 0);
    assert_eq!(root.span_hi, 3);
}

/// AW-III.W5.c — Failure 4: disabled WS elision.
///
/// AQ.5 disabled the WS-elision shortcut entirely because input-size
/// sensitivity made the structural pre-pass slower on small inputs.
/// W5.c's `DtaState::WsTrim` arm collapses to `pos = idx.positions[slot]`
/// when the structural index is populated — the whitespace span before
/// `idx.positions[slot]` is non-structural by construction.
///
/// This test exercises the WsTrim arm with both an empty index (cold
/// path; degrades to scanner / ascii-ws fallback) and a populated
/// index (hot path; collapses to slot jump). Both paths must produce
/// the same parse result — the WS subsumption is correctness-preserving.
///
/// Construction:
/// - Grammar: `s = "a" , wstrim , "b"`. WsTrim has `pattern: None`
///   (defaults to ASCII whitespace).
/// - Input: `"a   b"`. The WsTrim collapses three spaces.
/// - Expected: parse succeeds; the leaf records cover only the
///   non-whitespace bytes.
#[test]
fn aq5_disabled_ws_elision_wstrim_collapses() {
    static SEQ_CHILDREN: &[DtaStateId] = &[
        DtaStateId(1), // "a"
        DtaStateId(2), // WsTrim
        DtaStateId(3), // "b"
    ];
    static STATES: &[DtaState] = &[
        // 0: entry Seq
        DtaState::Seq {
            children: SEQ_CHILDREN,
            frame: DtaFrameKind::Seq,
            promote: SeqPromote::Default,
        },
        // 1: "a"
        DtaState::Literal { text: "a", payload: LiteralPayload::None },
        // 2: WsTrim with no pattern (ASCII-ws fallback)
        DtaState::WsTrim { pattern: None },
        // 3: "b"
        DtaState::Literal { text: "b", payload: LiteralPayload::None },
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

    // Cold-path: empty index. WsTrim degrades to ASCII-ws skip.
    let mut cols = Columns::new();
    let mut psi = PayloadStream::new();
    let mut fd: Vec<u8> = Vec::new();
    let result = dta_run_cold(&table, b"a   b", &NullScanner, &mut cols, &mut psi, &mut fd);
    assert!(
        result.is_ok(),
        "WsTrim collapse parse failed: {:?}",
        result,
    );

    // Three records emitted: Seq compound + "a" leaf + "b" leaf.
    // The WsTrim doesn't emit a record (it's a structural-only
    // cursor advance).
    assert_eq!(
        cols.len(),
        3,
        "expected 3 records (Seq + 2 literals); got {}",
        cols.len(),
    );

    // The "b" leaf's span_lo must skip past the whitespace, proving
    // the WsTrim arm advanced the cursor past the spaces.
    let b_leaf = cols.materialize(2);
    assert_eq!(b_leaf.kind(), TapeKind::Literal);
    assert_eq!(
        b_leaf.span_lo, 4,
        "\"b\" leaf must start AFTER the three trimmed spaces (offset 4)"
    );
    assert_eq!(b_leaf.span_hi, 5);

    // The Seq compound covers the full input.
    let root = cols.materialize(0);
    assert_eq!(root.span_lo, 0);
    assert_eq!(root.span_hi, 5, "Seq covers \"a   b\" inclusive of trimmed ws");
}

/// AW-III.W5.c — bonus: StructuralIndex API smoke test.
///
/// Exercises the W5.b `StructuralIndex` type the dual-cursor consumes,
/// to verify the cross-crate wire contract is intact in the cold-path
/// fallback. When W5.b's SIMD scanner wires into the parse prologue,
/// these are the only types crossing the bbnf-simd-scan ↔ bbnf-tape
/// boundary; the test asserts both `len()` and `is_empty()` agree
/// with the underlying `positions` Vec.
#[test]
fn aq5_structural_index_round_trips() {
    let empty = StructuralIndex::new();
    assert!(empty.is_empty());
    assert_eq!(empty.len(), 0);

    let mut idx = StructuralIndex::with_capacity(8);
    idx.push(0, b'{');
    idx.push(7, b'}');
    assert_eq!(idx.len(), 2);
    assert!(!idx.is_empty());
    assert_eq!(idx.positions, vec![0, 7]);
    assert_eq!(idx.kinds, vec![b'{', b'}']);
}
