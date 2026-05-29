//! Shared flat-tape substrate tests — record layout, builder round-trip,
//! cursor post-order child navigation, and O(1) checkpoint/rollback.

use bbnf::runtime::builder::StructBuilder;
use bbnf::runtime::tape::record::{LeafType, TapeKind, TapeRec};
use bbnf::runtime::tape::TapeStructBuilder;
use bbnf_ir::registry::{LayoutKind, StructLayout};
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
fn meta_idx_round_trips_full_5_bits() {
    for idx in 0u8..=0x1F {
        let mut rec = TapeRec::open(0, 0, idx);
        assert_eq!(rec.meta_idx(), idx, "open meta_idx {idx}");
        rec.set_meta_idx(idx);
        assert_eq!(rec.meta_idx(), idx, "reset meta_idx {idx}");
    }
}

#[test]
fn branch_round_trips_6_bits() {
    let mut rec = TapeRec::open(0, 0, 3);
    for b in 0u8..=0x3F {
        rec.set_branch(b);
        assert_eq!(rec.branch(), b);
        assert_eq!(rec.meta_idx(), 3, "branch must not clobber meta_idx");
    }
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

    let root = b.root_cursor(input).expect("root");
    assert_eq!(root.kind(), TapeKind::Open);
    assert_eq!(root.child_count(), 1);
    let child = root.child(0).expect("child 0");
    assert_eq!(child.kind(), TapeKind::Leaf);
    assert_eq!(child.leaf_type(), LeafType::F64);
    assert_eq!(child.as_f64(), 42.0);
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

    let root = b.root_cursor(input).expect("root");
    assert_eq!(root.child_count(), 3);
    let vals: Vec<i64> = root.children_forward().map(|c| c.as_i64()).collect();
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

    let root = b.root_cursor(input).expect("root");
    assert_eq!(root.child_count(), 2, "leaf + inner compound");
    let kids: Vec<_> = root.children_forward().collect();
    assert_eq!(kids[0].kind(), TapeKind::Leaf);
    assert!(kids[0].as_bool());
    assert_eq!(kids[1].kind(), TapeKind::Open);
    assert_eq!(kids[1].child_count(), 2);
    let inner_vals: Vec<i64> = kids[1].children_forward().map(|c| c.as_i64()).collect();
    assert_eq!(inner_vals, vec![7, 8]);
}

// Zero-copy string borrow: a slice of the bound input must round-trip
// as a span borrow (no arena bytes written).
#[test]
fn string_borrow_is_zero_copy() {
    let input = "color: red;";
    let mut b = TapeStructBuilder::new();
    b.bind_input(input.as_bytes());
    let h = b.begin_compound(&layout(5));
    b.push_leaf_with_str(&input[7..10]); // "red"
    b.end_compound(h);
    assert!(
        b.arena().is_empty(),
        "borrowed substring must not touch the payload arena"
    );
    let root = b.root_cursor(input).expect("root");
    let child = root.child(0).expect("child");
    assert_eq!(child.as_str(), "red");
}

// Non-borrowed (synthesised) string rides the arena.
#[test]
fn synthesised_string_uses_arena() {
    let input = "x";
    let owned = String::from("not-from-input");
    let mut b = TapeStructBuilder::new();
    b.bind_input(input.as_bytes());
    let h = b.begin_compound(&layout(6));
    b.push_leaf_with_str(&owned);
    b.end_compound(h);
    assert!(!b.arena().is_empty());
    let root = b.root_cursor(input).expect("root");
    assert_eq!(root.child(0).unwrap().as_str(), "not-from-input");
}

// O(1) checkpoint/rollback: a speculative compound is discarded by
// truncation; the tape returns to its prior length exactly.
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

    let root = b.root_cursor(input).expect("root");
    let vals: Vec<i64> = root.children_forward().map(|c| c.as_i64()).collect();
    assert_eq!(vals, vec![1, 2], "speculative inner compound was discarded");
}

// Wide u64 magnitudes ride the arena; small u64 inline-pack.
#[test]
fn u64_inline_vs_arena() {
    let input = "n";
    let mut b = TapeStructBuilder::new();
    b.bind_input(input.as_bytes());
    let h = b.begin_compound(&layout(9));
    b.push_leaf_with_u64(0xDEAD_BEEF); // <= u32::MAX -> inline
    b.push_leaf_with_u64(0x1_0000_0000); // > u32::MAX -> arena
    b.end_compound(h);
    let root = b.root_cursor(input).expect("root");
    let kids: Vec<_> = root.children_forward().collect();
    assert_eq!(kids[0].as_u64(), 0xDEAD_BEEF);
    assert_eq!(kids[1].as_u64(), 0x1_0000_0000);
}
