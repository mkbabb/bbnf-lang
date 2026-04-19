//! AW-III.W5.c — Driver dual-cursor regression suite.
//!
//! Exercises the `Cursor<'_>` carrier and the slot-aware dispatch
//! arms (`ByteDispatch` with index, `Regex` with bound, `WsTrim` with
//! collapse, `ConsumeToNextStructural`).
//!
//! The cold-path `dta_run_cold` runs with an internally-constructed
//! empty `StructuralIndex`; these tests instead use `dispatch_one`
//! directly with a populated index to verify the dual-cursor's
//! slot-aware shortcuts work end-to-end.

use tape::{
    dispatch_one, Columns, Cursor, DtaFrameKind, DtaRuleEntry, DtaRuleId,
    DtaState, DtaStateId, DtaTable, FrameStack, LiteralPayload, PayloadStream,
    SeqPromote, StepResult, StructuralIndex,
};

/// AW-IV.W1.β — null regex-scan fn pointer (see `walker_arms.rs`).
fn null_regex_scan(_pattern: &str, _input: &[u8], _pos: usize) -> Option<u32> {
    None
}

/// AW-III.W5.c — `Cursor::new` constructs at offset 0 with empty state.
#[test]
fn cursor_new_initialises_to_zero() {
    let src: &[u8] = b"hello";
    let idx = StructuralIndex::new();
    let cursor = Cursor::new(src, &idx);
    assert_eq!(cursor.pos, 0);
    assert_eq!(cursor.slot, 0);
    assert_eq!(cursor.src.len(), 5);
    assert!(cursor.idx.is_empty());
}

/// AW-III.W5.c — `Cursor::advance_slot` increments by exactly 1.
#[test]
fn cursor_advance_slot_increments() {
    let src: &[u8] = b"";
    let idx = StructuralIndex::new();
    let mut cursor = Cursor::new(src, &idx);
    cursor.advance_slot();
    cursor.advance_slot();
    assert_eq!(cursor.slot, 2);
}

/// AW-III.W5.c — `Cursor::jump_to_next_structural` jumps to the slot's
/// position and returns true; returns false when no further slot.
#[test]
fn cursor_jump_to_next_structural_walks_index() {
    let src: &[u8] = b"abcdefghij";
    let mut idx = StructuralIndex::new();
    idx.push(3, b'd');
    idx.push(6, b'g');

    let mut cursor = Cursor::new(src, &idx);
    assert!(cursor.jump_to_next_structural());
    assert_eq!(cursor.pos, 3);

    cursor.advance_slot();
    assert!(cursor.jump_to_next_structural());
    assert_eq!(cursor.pos, 6);

    cursor.advance_slot();
    assert!(!cursor.jump_to_next_structural());
    assert_eq!(cursor.pos, 6, "jump returns false; pos unchanged");
}

/// AW-III.W5.c — ByteDispatch reads `idx.kinds[slot]` when the index's
/// position matches `pos`. The fallback path (`input[pos]`) survives
/// when the index isn't aligned.
#[test]
fn byte_dispatch_consults_index_when_aligned() {
    static DISP_TABLE: [DtaStateId; 256] = {
        let mut t = [DtaStateId::NONE; 256];
        t[b'X' as usize] = DtaStateId(1);
        t
    };
    static STATES: &[DtaState] = &[
        DtaState::ByteDispatch {
            table: &DISP_TABLE,
            fallback: DtaStateId::NONE,
        },
        DtaState::Literal { text: "X", payload: LiteralPayload::None },
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

    // Populate index with the X position.
    let mut idx = StructuralIndex::new();
    idx.push(0, b'X');

    let input: &[u8] = b"X";
    let mut cols = Columns::new();
    let mut psi = PayloadStream::new();
    let mut fd: Vec<u8> = Vec::new();
    let mut stack = FrameStack::new();
    let mut pos: u32 = 0;
    let mut slot: u32 = 0;

    let result = dispatch_one(
        &table, input, null_regex_scan, &idx, &mut cols, &mut psi, &mut fd,
        &mut stack, DtaStateId(0), &mut pos, &mut slot,
    );
    assert!(result.is_ok(), "byte dispatch with index failed: {:?}", result);
    if let Ok(StepResult::Next(next)) = result {
        assert_eq!(next, DtaStateId(1), "dispatched to state 1 via X");
    }
}

/// AW-III.W5.c — `ConsumeToNextStructural` jumps cursor to next index
/// position when populated; falls back to ASCII-ws skip when empty.
#[test]
fn consume_to_next_structural_jumps_via_index() {
    static STATES: &[DtaState] = &[DtaState::ConsumeToNextStructural];
    static RULE_ENTRIES: &[DtaRuleEntry] = &[DtaRuleEntry {
        rule: DtaRuleId(0),
        state: DtaStateId(0),
    }];
    let table = DtaTable {
        states: STATES,
        rule_entries: RULE_ENTRIES,
        shunting_yard_rules: &[],
        counter_optional_rules: &[],
        max_nesting_depth: 1,
        entry: DtaRuleId(0),
    };

    // Index says next structural is at position 5.
    let mut idx = StructuralIndex::new();
    idx.push(5, b',');

    let input: &[u8] = b"hello,world";
    let mut cols = Columns::new();
    let mut psi = PayloadStream::new();
    let mut fd: Vec<u8> = Vec::new();
    let mut stack = FrameStack::new();
    let mut pos: u32 = 0;
    let mut slot: u32 = 0;

    let _ = dispatch_one(
        &table, input, null_regex_scan, &idx, &mut cols, &mut psi, &mut fd,
        &mut stack, DtaStateId(0), &mut pos, &mut slot,
    );

    assert_eq!(pos, 5, "cursor jumped to next structural delimiter");
    assert_eq!(slot, 1, "slot advanced past the consumed structural entry");
}

/// AW-III.W5.c — `ConsumeToNextStructural` falls back to ASCII-ws when
/// the index is empty (cold-path / pre-W5.b integration).
#[test]
fn consume_to_next_structural_ascii_fallback_no_index() {
    static STATES: &[DtaState] = &[DtaState::ConsumeToNextStructural];
    static RULE_ENTRIES: &[DtaRuleEntry] = &[DtaRuleEntry {
        rule: DtaRuleId(0),
        state: DtaStateId(0),
    }];
    let table = DtaTable {
        states: STATES,
        rule_entries: RULE_ENTRIES,
        shunting_yard_rules: &[],
        counter_optional_rules: &[],
        max_nesting_depth: 1,
        entry: DtaRuleId(0),
    };

    let idx = StructuralIndex::new();
    let input: &[u8] = b"   abc";
    let mut cols = Columns::new();
    let mut psi = PayloadStream::new();
    let mut fd: Vec<u8> = Vec::new();
    let mut stack = FrameStack::new();
    let mut pos: u32 = 0;
    let mut slot: u32 = 0;

    let _ = dispatch_one(
        &table, input, null_regex_scan, &idx, &mut cols, &mut psi, &mut fd,
        &mut stack, DtaStateId(0), &mut pos, &mut slot,
    );

    assert_eq!(pos, 3, "ASCII-ws fallback skipped 3 spaces");
    assert_eq!(slot, 0, "slot stays at 0 with empty index");
}

/// AW-III.W5.c — `WsTrim` collapses to `pos = idx.positions[slot]` when
/// the index is populated. The whitespace span is implicit.
#[test]
fn ws_trim_collapses_via_index() {
    static SEQ_CHILDREN: &[DtaStateId] = &[
        DtaStateId(1), // "a"
        DtaStateId(2), // WsTrim
        DtaStateId(3), // "b"
    ];
    static STATES: &[DtaState] = &[
        DtaState::Seq {
            children: SEQ_CHILDREN,
            frame: DtaFrameKind::Seq,
            promote: SeqPromote::Default,
        },
        DtaState::Literal { text: "a", payload: LiteralPayload::None },
        DtaState::WsTrim { pattern: None },
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

    // Populated index: 'a' at 0, 'b' at 4 (after 3 spaces).
    let mut idx = StructuralIndex::new();
    idx.push(0, b'a');
    idx.push(4, b'b');

    let input: &[u8] = b"a   b";
    let mut cols = Columns::new();
    let mut psi = PayloadStream::new();
    let mut fd: Vec<u8> = Vec::new();
    let mut stack = FrameStack::new();
    let mut pos: u32 = 0;
    let mut slot: u32 = 0;

    // Run through the full Seq dispatch.
    let mut state = DtaStateId(0);
    loop {
        let r = dispatch_one(
            &table, input, null_regex_scan, &idx, &mut cols, &mut psi, &mut fd,
            &mut stack, state, &mut pos, &mut slot,
        );
        match r {
            Ok(StepResult::Next(next)) => state = next,
            Ok(StepResult::Done) => break,
            Err(e) => panic!("dispatch failed: {:?}", e),
        }
    }

    // Verify the "b" leaf landed at position 4 — WsTrim must have
    // collapsed past the 3 spaces via the structural index.
    let b_leaf = cols.materialize(2);
    assert_eq!(
        b_leaf.span_lo, 4,
        "WsTrim must have advanced cursor past the trimmed whitespace"
    );
}
