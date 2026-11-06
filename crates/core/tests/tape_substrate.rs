//! Shared flat-tape substrate tests — record layout, builder round-trip,
//! cursor post-order child navigation, and O(depth) checkpoint/rollback.

use std::panic::{AssertUnwindSafe, catch_unwind};

use bbnf::runtime::builder::StructBuilder;
use bbnf::runtime::tape::TapeStructBuilder;
use bbnf::runtime::tape::record::{LeafType, TapeKind, TapeRec};
use bbnf_ir::registry::{AltBranchId, LayoutKind, StructLayout};
use bbnf_ir::types::TypeDesc;

fn layout(rule_id: u32) -> StructLayout {
    StructLayout {
        rule_id,
        rule_name: format!("rule_{rule_id}"),
        kind: LayoutKind::Struct,
        rule_type: TypeDesc::Span,
        fields: Vec::new(),
    }
}

#[test]
fn tape_rec_is_16_bytes_aligned_4() {
    assert_eq!(core::mem::size_of::<TapeRec>(), 16);
    assert_eq!(core::mem::align_of::<TapeRec>(), 4);
}

#[test]
fn rule_id_round_trips_full_u32_domain() {
    for rule_id in [0, 1, 31, 32, u32::MAX] {
        let mut builder = TapeStructBuilder::new();
        let handle = builder.begin_compound(&layout(rule_id));
        builder.end_compound(handle);
        assert_eq!(builder.root_cursor().unwrap().rule_id(), Some(rule_id));
    }
}

#[test]
fn branch_presence_and_full_u16_domain_are_exact() {
    let mut absent = TapeStructBuilder::new();
    let handle = absent.begin_compound(&layout(3));
    absent.end_compound(handle);
    assert_eq!(absent.root_cursor().unwrap().branch(), None);

    let last = AltBranchId::new(u16::MAX);
    let mut present = TapeStructBuilder::new();
    let handle = present.begin_compound(&layout(3));
    present.select_alt_branch(last);
    present.end_compound(handle);
    let root = present.root_cursor().unwrap();
    assert_eq!(root.branch(), Some(last));
    assert_eq!(root.rule_id(), Some(3), "branch must not clobber rule id");
}

#[test]
fn builder_preserves_structural_branch_and_repeated_u8_values() {
    let mut builder = TapeStructBuilder::new();
    let handle = builder.begin_compound(&layout(1));
    builder.push_leaf_with_u8(u8::MAX);
    builder.push_leaf_with_u8(7);
    builder.select_alt_branch(AltBranchId::new(u16::MAX));
    builder.end_compound(handle);

    let root = builder.root_cursor().expect("root");
    assert_eq!(root.branch(), Some(AltBranchId::new(u16::MAX)));
    let values = root
        .children_forward()
        .map(|child| child.as_u8())
        .collect::<Vec<_>>();
    assert_eq!(values, vec![Some(u8::MAX), Some(7)]);
}

// A single leaf inside one compound: builder appends Leaf then Open in
// post-order; the cursor reads the leaf back as the compound's child.
#[test]
fn single_leaf_round_trip() {
    let input = "42";
    let mut b = TapeStructBuilder::new();
    b.bind_input(input.as_bytes());
    let h = b.begin_compound(&layout(1));
    b.push_leaf_with_f64(42.0);
    b.end_compound(h);

    let root = b.root_cursor().expect("root");
    assert_eq!(root.kind(), Some(TapeKind::Open));
    assert_eq!(root.rule_id(), Some(1));
    assert_eq!(root.child_count(), 1);
    let child = root.child(0).expect("child 0");
    assert_eq!(child.kind(), Some(TapeKind::Leaf));
    assert_eq!(child.rule_id(), None);
    assert_eq!(child.leaf_type(), Some(LeafType::F64));
    assert_eq!(child.as_f64(), Some(42.0));
}

// Forward source order: three leaves pushed in order must read back in
// the same order through children_forward.
#[test]
fn children_forward_preserves_source_order() {
    let input = "abc";
    let mut b = TapeStructBuilder::new();
    b.bind_input(input.as_bytes());
    let h = b.begin_compound(&layout(2));
    b.push_leaf_with_i64(10);
    b.push_leaf_with_i64(20);
    b.push_leaf_with_i64(30);
    b.end_compound(h);

    let root = b.root_cursor().expect("root");
    assert_eq!(root.child_count(), 3);
    let vals: Vec<i64> = root
        .children_forward()
        .map(|c| c.as_i64().expect("i64 child"))
        .collect();
    assert_eq!(vals, vec![10, 20, 30]);
}

// Nested compounds: an outer compound holding a leaf and an inner
// compound (itself holding two leaves). The backward post-order walk
// must report exactly two immediate children of the outer, in order.
#[test]
fn nested_compounds_child_walk() {
    let input = "nested";
    let mut b = TapeStructBuilder::new();
    b.bind_input(input.as_bytes());
    let outer = b.begin_compound(&layout(3));
    b.push_leaf_with_bool(true);
    let inner = b.begin_compound(&layout(4));
    b.push_leaf_with_i64(7);
    b.push_leaf_with_i64(8);
    b.end_compound(inner);
    b.end_compound(outer);

    let root = b.root_cursor().expect("root");
    assert_eq!(root.child_count(), 2, "leaf + inner compound");
    let kids: Vec<_> = root.children_forward().collect();
    assert_eq!(root.rule_id(), Some(3));
    assert_eq!(kids[0].kind(), Some(TapeKind::Leaf));
    assert_eq!(kids[0].as_bool(), Some(true));
    assert_eq!(kids[1].kind(), Some(TapeKind::Open));
    assert_eq!(kids[1].rule_id(), Some(4));
    assert_eq!(kids[1].child_count(), 2);
    let inner_vals: Vec<i64> = kids[1]
        .children_forward()
        .map(|c| c.as_i64().expect("i64 child"))
        .collect();
    assert_eq!(inner_vals, vec![7, 8]);
}

#[test]
fn valid_nested_handles_and_bounds_preserve_exact_scope_identity() {
    let mut builder = TapeStructBuilder::new();
    builder.bind_input(b"abcd");

    let outer = builder.begin_compound(&layout(100));
    builder.record_compound_bounds_start(0);
    let inner = builder.begin_compound(&layout(200));
    builder.record_compound_bounds_start(1);
    builder.record_compound_bounds_end(3);
    builder.end_compound(inner);
    builder.record_compound_bounds_end(4);
    builder.end_compound(outer);

    let root = builder.root_cursor().expect("outer root");
    assert_eq!(root.rule_id(), Some(100));
    assert_eq!(root.span(), Some("abcd"));
    let child = root.child(0).expect("inner child");
    assert_eq!(child.rule_id(), Some(200));
    assert_eq!(child.span(), Some("bc"));
}

#[test]
fn out_of_order_handle_is_rejected_before_scope_mutation() {
    let mut builder = TapeStructBuilder::new();
    let outer = builder.begin_compound(&layout(300));
    let inner = builder.begin_compound(&layout(301));

    let rejected = catch_unwind(AssertUnwindSafe(|| builder.end_compound(outer)));
    assert!(
        rejected.is_err(),
        "outer handle must not close the inner scope"
    );
    assert!(
        builder.records().is_empty(),
        "rejection must precede writes"
    );
    assert!(builder.root_cursor().is_none());

    builder.end_compound(inner);
    assert_eq!(builder.records().len(), 1, "inner scope must remain open");
    assert!(
        builder.root_cursor().is_none(),
        "the unmatched outer scope must remain open"
    );
}

#[test]
fn cross_builder_handle_is_rejected_before_scope_mutation() {
    let mut left = TapeStructBuilder::new();
    let mut right = TapeStructBuilder::new();
    let left_handle = left.begin_compound(&layout(400));
    let right_handle = right.begin_compound(&layout(400));

    let rejected = catch_unwind(AssertUnwindSafe(|| left.end_compound(right_handle)));
    assert!(
        rejected.is_err(),
        "a handle from another builder must not alias the left scope"
    );
    assert!(left.records().is_empty(), "rejection must precede writes");

    left.end_compound(left_handle);
    assert_eq!(
        left.root_cursor().and_then(|root| root.rule_id()),
        Some(400)
    );
}

#[test]
fn rollback_never_reuses_a_discarded_scope_token() {
    let mut builder = TapeStructBuilder::new();
    let outer = builder.begin_compound(&layout(500));
    let checkpoint = builder.checkpoint();
    let stale = builder.begin_compound(&layout(501));
    builder.rollback(checkpoint);

    let fresh = builder.begin_compound(&layout(501));
    let rejected = catch_unwind(AssertUnwindSafe(|| builder.end_compound(stale)));
    assert!(
        rejected.is_err(),
        "a rolled-back handle must not alias the fresh scope"
    );
    assert!(
        builder.records().is_empty(),
        "rejection must precede writes"
    );

    builder.end_compound(fresh);
    builder.end_compound(outer);
    let root = builder.root_cursor().expect("outer root");
    assert_eq!(root.rule_id(), Some(500));
    assert_eq!(root.child(0).and_then(|child| child.rule_id()), Some(501));
}

#[test]
fn bounds_calls_are_balanced_ordered_and_scope_local() {
    let mut missing_start = TapeStructBuilder::new();
    let handle = missing_start.begin_compound(&layout(600));
    assert!(
        catch_unwind(AssertUnwindSafe(|| {
            missing_start.record_compound_bounds_end(1);
        }))
        .is_err()
    );
    missing_start.record_compound_bounds_start(0);
    missing_start.record_compound_bounds_end(1);
    missing_start.end_compound(handle);

    let mut duplicate_start = TapeStructBuilder::new();
    let handle = duplicate_start.begin_compound(&layout(601));
    duplicate_start.record_compound_bounds_start(0);
    assert!(
        catch_unwind(AssertUnwindSafe(|| {
            duplicate_start.record_compound_bounds_start(1);
        }))
        .is_err()
    );
    duplicate_start.record_compound_bounds_end(1);
    duplicate_start.end_compound(handle);

    let mut reversed = TapeStructBuilder::new();
    let handle = reversed.begin_compound(&layout(602));
    reversed.record_compound_bounds_start(2);
    assert!(
        catch_unwind(AssertUnwindSafe(|| {
            reversed.record_compound_bounds_end(1);
        }))
        .is_err()
    );
    reversed.record_compound_bounds_end(2);
    reversed.end_compound(handle);

    let mut cross_scope = TapeStructBuilder::new();
    let outer = cross_scope.begin_compound(&layout(603));
    cross_scope.record_compound_bounds_start(0);
    let inner = cross_scope.begin_compound(&layout(604));
    assert!(
        catch_unwind(AssertUnwindSafe(|| {
            cross_scope.record_compound_bounds_end(1);
        }))
        .is_err(),
        "outer bounds must not close through an inner scope"
    );
    cross_scope.end_compound(inner);
    cross_scope.record_compound_bounds_end(1);
    cross_scope.end_compound(outer);
}

#[test]
fn compound_close_rejects_an_unmatched_bounds_start_before_writing() {
    let mut builder = TapeStructBuilder::new();
    let handle = builder.begin_compound(&layout(700));
    builder.record_compound_bounds_start(0);

    let rejected = catch_unwind(AssertUnwindSafe(|| builder.end_compound(handle)));
    assert!(rejected.is_err());
    assert!(
        builder.records().is_empty(),
        "rejection must precede writes"
    );
    assert!(builder.root_cursor().is_none());
}

// Strings are copied into the arena, including slices of the bound input.
// The tape therefore never outlives an unsafely widened source borrow.
#[test]
fn input_string_is_owned_by_arena() {
    let input = "color: red;";
    let mut b = TapeStructBuilder::new();
    b.bind_input(input.as_bytes());
    let h = b.begin_compound(&layout(5));
    b.push_leaf_with_str(&input[7..10]); // "red"
    b.end_compound(h);
    assert!(!b.arena().is_empty());
    let root = b.root_cursor().expect("root");
    let child = root.child(0).expect("child");
    assert_eq!(child.as_str(), Some("red"));
    assert_eq!(child.span(), None, "arena length is not a source span");
}

// A temporary synthesized string can be dropped before cursor creation.
#[test]
fn temporary_string_outlives_its_source_owner() {
    let mut b = TapeStructBuilder::new();
    let h = b.begin_compound(&layout(6));
    {
        let owned = String::from("temporary");
        b.push_leaf_with_str(&owned);
    }
    b.end_compound(h);
    assert!(!b.arena().is_empty());
    let root = b.root_cursor().expect("root");
    assert_eq!(root.child(0).unwrap().as_str(), Some("temporary"));
}

#[test]
fn source_bytes_are_owned_and_unfinished_roots_are_hidden() {
    let mut builder = TapeStructBuilder::new();
    {
        let temporary_source = String::from("abcd");
        builder.bind_input(temporary_source.as_bytes());
    }
    let handle = builder.begin_compound(&layout(12));
    builder.record_compound_bounds_start(1);
    builder.record_compound_bounds_end(3);
    assert!(builder.root_cursor().is_none());
    builder.end_compound(handle);

    assert_eq!(
        builder.root_cursor().and_then(|root| root.span()),
        Some("bc")
    );
}

// Transactional checkpoint/rollback: a speculative compound is discarded and
// surviving scope state is restored exactly.
#[test]
fn checkpoint_rollback_truncates() {
    let input = "spec";
    let mut b = TapeStructBuilder::new();
    b.bind_input(input.as_bytes());
    let h = b.begin_compound(&layout(7));
    b.push_leaf_with_i64(1);

    let cp = b.checkpoint();
    // speculative work, then rollback.
    let inner = b.begin_compound(&layout(8));
    b.push_leaf_with_f64(3.14);
    b.push_leaf_with_f64(2.71);
    b.end_compound(inner);
    assert!(b.records().len() > 2);
    b.rollback(cp);

    b.push_leaf_with_i64(2);
    b.end_compound(h);

    let root = b.root_cursor().expect("root");
    assert_eq!(root.rule_id(), Some(7));
    let vals: Vec<i64> = root
        .children_forward()
        .map(|c| c.as_i64().expect("i64 child"))
        .collect();
    assert_eq!(vals, vec![1, 2], "speculative inner compound was discarded");
}

#[test]
fn rollback_restores_surviving_scope_branch_bounds_and_u8_leaves() {
    let input = "abcd";
    let mut builder = TapeStructBuilder::new();
    builder.bind_input(input.as_bytes());
    let handle = builder.begin_compound(&layout(10));
    builder.record_compound_bounds_start(1);

    let checkpoint = builder.checkpoint();
    builder.push_leaf_with_u8(u8::MAX);
    builder.select_alt_branch(AltBranchId::new(u16::MAX));
    builder.record_compound_bounds_end(4);
    builder.rollback(checkpoint);

    builder.record_compound_bounds_end(2);
    builder.end_compound(handle);

    let root = builder.root_cursor().expect("root");
    assert_eq!(root.branch(), None);
    assert_eq!(root.span(), Some("b"));
    assert_eq!(root.child_count(), 0);
}

#[test]
fn rollback_keeps_rule_id_lane_aligned() {
    let mut builder = TapeStructBuilder::new();
    let outer = builder.begin_compound(&layout(31));
    let checkpoint = builder.checkpoint();

    let discarded = builder.begin_compound(&layout(u32::MAX));
    builder.end_compound(discarded);
    builder.rollback(checkpoint);

    let retained = builder.begin_compound(&layout(32));
    builder.end_compound(retained);
    builder.end_compound(outer);

    let root = builder.root_cursor().expect("root");
    assert_eq!(root.rule_id(), Some(31));
    let child = root.child(0).expect("retained child");
    assert_eq!(child.rule_id(), Some(32));
}

// The sentinel boundary and all wider magnitudes ride the arena; smaller
// u64 values inline-pack without aliasing `TAPE_NONE`.
#[test]
fn u64_sentinel_boundaries_round_trip_exactly() {
    let input = "n";
    let mut b = TapeStructBuilder::new();
    b.bind_input(input.as_bytes());
    let h = b.begin_compound(&layout(9));
    let values = [
        u64::from(u32::MAX - 1),
        u64::from(u32::MAX),
        u64::from(u32::MAX) + 1,
        u64::MAX,
    ];
    for value in values {
        b.push_leaf_with_u64(value);
    }
    b.end_compound(h);
    let root = b.root_cursor().expect("root");
    let kids: Vec<_> = root.children_forward().collect();
    assert_eq!(
        kids.iter().map(|child| child.as_u64()).collect::<Vec<_>>(),
        values.map(Some)
    );
}

#[test]
fn typed_cursor_reads_reject_open_and_wrong_leaf_types() {
    let mut builder = TapeStructBuilder::new();
    let handle = builder.begin_compound(&layout(11));
    builder.push_leaf_with_f64(1.5);
    builder.push_leaf_with_i64(-2);
    builder.push_leaf_with_u64(3);
    builder.push_leaf_with_u8(4);
    builder.push_leaf_with_bool(true);
    builder.push_leaf_with_str("five");
    builder.end_compound(handle);

    let root = builder.root_cursor().expect("root");
    assert_eq!(root.leaf_type(), None);
    assert_eq!(root.as_f64(), None);
    assert_eq!(root.as_i64(), None);
    assert_eq!(root.as_u64(), None);
    assert_eq!(root.as_u8(), None);
    assert_eq!(root.as_bool(), None);
    assert_eq!(root.as_str(), None);

    let children = root.children_forward().collect::<Vec<_>>();
    assert_eq!(children[0].as_i64(), None);
    assert_eq!(children[1].as_u64(), None);
    assert_eq!(children[2].as_u8(), None);
    assert_eq!(children[3].as_bool(), None);
    assert_eq!(children[4].as_str(), None);
    assert_eq!(children[5].as_f64(), None);
}

#[test]
fn public_reads_are_checked_at_invalid_offsets() {
    let mut builder = TapeStructBuilder::new();
    builder.bind_input(b"x");
    let handle = builder.begin_compound(&layout(u32::MAX));
    builder.record_compound_bounds_start(0);
    builder.push_leaf_with_u64(u64::MAX);
    builder.record_compound_bounds_end(2);
    builder.end_compound(handle);

    assert_eq!(builder.root_cursor().and_then(|root| root.span()), None);
    assert!(builder.cursor_at(u32::MAX).is_none());
    assert_eq!(builder.arena().read_f64(u32::MAX), None);
    assert_eq!(builder.arena().read_i64(u32::MAX), None);
    assert_eq!(builder.arena().read_u64(u32::MAX), None);
    assert_eq!(builder.arena().read_str(u32::MAX, u32::MAX), None);
}
