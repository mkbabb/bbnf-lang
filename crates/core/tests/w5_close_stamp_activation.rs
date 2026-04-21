//! AY.W5.c — Read-side activation probe for the write-time
//! close-stamped substrate.
//!
//! AY.W5.a landed the [`TapeBuilder::open_compound`] /
//! [`TapeBuilder::close_compound`] pair and the
//! [`TapeRec::SIB_SKIP_STAMPED_BIT`] observational flag. AY.W5.b
//! retargets the generated shape emitters to drive compounds via
//! that pair. This test binary lives on the AY.W5.c read-side and
//! proves the canonical cursor + `Parsed` view surface consumes the
//! new substrate without a fallback path.
//!
//! Two evidence modes coexist:
//!
//! 1. **Fixture-tape semantics proof** — builds a JSON-shaped fixture
//!    tape manually via the W5.a open/close API, asserts that direct
//!    children carry [`TapeRec::SIB_SKIP_STAMPED_BIT`], and walks the
//!    tape via [`TapeCursor`] (the same type every generated
//!    `#[derive(Parser)]` view wraps). The assertion passes in the
//!    W5.a + W5.c state — no emitter regen required.
//!
//! 2. **End-to-end emitter activation** — parses a real JSON fixture
//!    via the `#[derive(Parser)]` entry point and asserts that the
//!    resulting `Tape` contains at least one record with
//!    [`TapeRec::SIB_SKIP_STAMPED_BIT`] set. Pre-W5.b emitter regen
//!    the legacy `push_compound` path writes every compound, so no
//!    records carry the stamp bit — that assertion is scheduled to
//!    light up post-W5.b and is gated with `#[ignore]` until then.
//!
//! [`TapeBuilder::open_compound`]: tape::TapeBuilder::open_compound
//! [`TapeBuilder::close_compound`]: tape::TapeBuilder::close_compound
//! [`TapeRec::SIB_SKIP_STAMPED_BIT`]: tape::TapeRec::SIB_SKIP_STAMPED_BIT
//! [`TapeCursor`]: tape::TapeCursor

use bbnf_derive::Parser;
use tape::{PayloadData, TapeBuilder, TapeCursor, TapeKind, TapeOffset, TapeRec};

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf")]
struct W5ActivationJson;

// ── Fixture-tape semantics proof ────────────────────────────────────────────
//
// Build a tape for {"k1": 1, "k2": 2} using the W5.a open/close API
// directly. Compounds use `open_compound` / `close_compound`; string
// and number leaves use `push_leaf_with` and `push_leaf`. The resulting
// tape exercises the same structural shape an emitted JSON parser
// writes post-W5.b — nested compounds with stamped direct-children
// sib-skip links.
//
// Structural layout (rows 0..=8):
//
//   0: object (compound, Seq)
//     1: pair (compound, Seq)
//       2: string leaf       "k1"
//       3: literal leaf      `:`
//       4: number leaf       1     (wide-scalar payload)
//     5: pair (compound, Seq)
//       6: string leaf       "k2"
//       7: literal leaf      `:`
//       8: number leaf       2     (wide-scalar payload)

fn build_object_pair_fixture() -> tape::Tape {
    let mut b = TapeBuilder::new();

    let obj = b.open_compound(TapeKind::Seq, 0, 0, 0);
    assert_eq!(obj, TapeOffset(0));

    // pair 1: "k1": 1
    let pair1 = b.open_compound(TapeKind::Seq, 1, 0, 0);
    assert_eq!(pair1, TapeOffset(1));
    let _s1 = b.push_leaf(TapeKind::Literal, 1, 5, 0, 0);
    let _colon1 = b.push_leaf(TapeKind::Literal, 5, 6, 0, 0);
    let _n1 = b.push_leaf_with(
        TapeKind::Span,
        7,
        8,
        0,
        0,
        PayloadData::WideScalar(1u64),
    );
    b.close_compound(pair1, 8);

    // pair 2: "k2": 2
    let pair2 = b.open_compound(TapeKind::Seq, 10, 0, 0);
    assert_eq!(pair2, TapeOffset(5));
    let _s2 = b.push_leaf(TapeKind::Literal, 10, 14, 0, 0);
    let _colon2 = b.push_leaf(TapeKind::Literal, 14, 15, 0, 0);
    let _n2 = b.push_leaf_with(
        TapeKind::Span,
        16,
        17,
        0,
        0,
        PayloadData::WideScalar(2u64),
    );
    b.close_compound(pair2, 17);

    b.close_compound(obj, 18);

    b.finish().expect("fixture tape finalises cleanly")
}

/// Fixture-tape proof — every direct child of a write-time-closed
/// compound carries [`TapeRec::SIB_SKIP_STAMPED_BIT`], and the
/// [`TapeCursor`] walks the stamped structure in the correct forward
/// order.
#[test]
fn fixture_tape_marks_direct_children_as_stamped() {
    let tape = build_object_pair_fixture();

    // Count stamped records. Every direct child of a write-time-
    // closed compound is stamped exactly once (non-last children at
    // `note_push` time; last child at `close_compound` time). The
    // root compound itself has no outer frame and stays unstamped.
    let mut stamped = 0;
    for i in 0..tape.len() as u32 {
        if (tape.columns().extra_at(i) & TapeRec::SIB_SKIP_STAMPED_BIT) != 0 {
            stamped += 1;
        }
    }
    assert!(
        stamped > 0,
        "fixture built via open/close_compound should leave a non-empty stamp trail; got {stamped}"
    );
    // Every non-root record in the fixture is a direct child of some
    // closed compound, so every such slot carries the stamp.
    assert_eq!(
        stamped,
        tape.len() - 1,
        "every non-root record is a direct child of some closed compound and should be stamped"
    );

    // Root compound row itself is untouched: no outer open frame
    // ever saw it as a child.
    assert_eq!(
        tape.columns().extra_at(0) & TapeRec::SIB_SKIP_STAMPED_BIT,
        0,
        "root row has no outer frame; stamp bit must stay clear"
    );
}

/// Fixture-tape proof — [`TapeCursor`] iteration reads the stamped
/// substrate cleanly: two top-level `pair` children, each containing
/// three direct leaves (string / `:` / number).
#[test]
fn fixture_tape_cursor_traversal_matches_write_order() {
    let tape = build_object_pair_fixture();
    let root = TapeCursor::new(&tape, TapeOffset(0));

    // Top-level compound carries two direct children (the two pairs).
    assert_eq!(root.child_count(), 2, "object has two pairs");

    // Forward-iterate children via the ChildIter; offsets are the
    // pair rows, in emission order.
    let child_offsets: Vec<u32> =
        root.children().map(|c| c.offset().0).collect();
    assert_eq!(
        child_offsets,
        vec![1, 5],
        "child offsets match the fixture emission order (pair 1 at row 1, pair 2 at row 5)"
    );

    // Each pair carries three direct-leaf children (string / `:` /
    // number).
    let pair1 = root.child(0).expect("pair 1 present");
    assert_eq!(pair1.child_count(), 3, "pair carries string / : / number");

    let pair1_children: Vec<u32> =
        pair1.children().map(|c| c.offset().0).collect();
    assert_eq!(
        pair1_children,
        vec![2, 3, 4],
        "pair 1 children: string leaf + colon leaf + number leaf"
    );

    let pair2 = root.child(1).expect("pair 2 present");
    assert_eq!(pair2.child_count(), 3);

    let pair2_children: Vec<u32> =
        pair2.children().map(|c| c.offset().0).collect();
    assert_eq!(pair2_children, vec![6, 7, 8]);
}

/// Fixture-tape proof — write-time-closed compounds set `child_off`
/// to `parent + 1`, so `first_child_root` hits the pre-order O(1)
/// fast path rather than the post-order backward walk. Verified via
/// direct column inspection.
#[test]
fn fixture_tape_compound_child_off_is_pre_order() {
    let tape = build_object_pair_fixture();
    let cols = tape.columns();

    // Root object: row 0, first child at row 1.
    assert_eq!(cols.child_off_at(0), TapeOffset(1));
    // Pair 1: row 1, first child at row 2.
    assert_eq!(cols.child_off_at(1), TapeOffset(2));
    // Pair 2: row 5, first child at row 6.
    assert_eq!(cols.child_off_at(5), TapeOffset(6));
}

/// Fixture-tape proof — direct children of a write-time-closed
/// compound carry authoritative inter-sibling distances in
/// `sib_skip`, with the last sibling's slot at `0`. Matches the
/// cursor's forward-walk termination convention (step reads `0` ⇒
/// iteration ends).
#[test]
fn fixture_tape_sib_skip_is_authoritative() {
    let tape = build_object_pair_fixture();
    let cols = tape.columns();

    // Pair 1 (row 1) → Pair 2 (row 5): `sib_skip[1]` must be 4.
    assert_eq!(cols.sib_skip_at(1), 4);
    // Pair 2 is the last sibling; `sib_skip[5]` stays at 0.
    assert_eq!(cols.sib_skip_at(5), 0);

    // Pair 1's direct children: row 2 (string leaf), row 3 (colon
    // leaf), row 4 (number leaf). Inter-sibling distances: 1, 1, 0.
    assert_eq!(cols.sib_skip_at(2), 1, "string -> colon distance");
    assert_eq!(cols.sib_skip_at(3), 1, "colon -> number distance");
    assert_eq!(cols.sib_skip_at(4), 0, "number is last sibling of pair 1");
}

/// Legacy path smoke — a tape built exclusively via `push_compound`
/// stays on the finaliser-drives-sib_skip contract (every record has
/// [`TapeRec::SIB_SKIP_STAMPED_BIT`] clear) and the cursor reads it
/// identically. Apples-to-apples substrate-independence: the cursor
/// doesn't know which path wrote `sib_skip`.
#[test]
fn legacy_push_compound_tape_reads_identically() {
    let mut b = TapeBuilder::new();
    let mark = b.mark_children();
    let _k1 = b.push_leaf(TapeKind::Literal, 0, 4, 0, 0);
    let _v1 = b.push_leaf_with(
        TapeKind::Span,
        5,
        6,
        0,
        0,
        PayloadData::WideScalar(1u64),
    );
    let root = b.push_compound(TapeKind::Seq, mark, 0, 6, 0, 0);
    let tape = b.finish().unwrap();

    // No record carries the stamp bit — the finaliser wrote every
    // `sib_skip` slot.
    let cols = tape.columns();
    for i in 0..tape.len() as u32 {
        assert_eq!(
            cols.extra_at(i) & TapeRec::SIB_SKIP_STAMPED_BIT,
            0,
            "legacy push_compound tape row {i} must not carry SIB_SKIP_STAMPED_BIT"
        );
    }

    // Cursor reads the same 2-child shape whether the substrate was
    // stamped at write time (see fixture_tape_cursor_traversal_*)
    // or derived by the finaliser (this test).
    let cursor = TapeCursor::new(&tape, root);
    assert_eq!(cursor.child_count(), 2);
}

// ── Parser-side emitter activation ──────────────────────────────────────────
//
// Exercises the full pipeline: real JSON input → `#[derive(Parser)]`
// entry point → finished Tape → cursor consumption. Pre-W5.b the
// generated emitter still drives `push_compound`, so no record carries
// the stamp bit; the assertion is gated `#[ignore]` until W5.b
// retargets the shape emitters. The `fixture_tape_*` tests above
// carry the pre-regen read-side activation evidence.

#[test]
#[ignore = "activates post-W5.b emitter regen; fixture_tape_* tests carry the pre-regen semantics proof"]
fn parsed_tape_contains_write_time_close_stamps() {
    let input = r#"{"a": 1, "b": [2, 3], "c": {"d": true}}"#;
    let parsed = W5ActivationJson::parse(input).expect("parse succeeds");
    let tape = parsed.tape();

    let mut stamped = 0;
    for i in 0..tape.len() as u32 {
        if (tape.columns().extra_at(i) & TapeRec::SIB_SKIP_STAMPED_BIT) != 0 {
            stamped += 1;
        }
    }
    assert!(
        stamped > 0,
        "post-W5.b emitter regen: at least one record should carry SIB_SKIP_STAMPED_BIT \
         (observed {stamped}); pre-regen the emitter still drives push_compound"
    );

    // Independently, the cursor-level traversal on this tape must
    // yield a value-walk byte-identical to sonic-rs — the same
    // invariant the apples-to-apples harness enforces.
    let _value = parsed.to_value();
}
