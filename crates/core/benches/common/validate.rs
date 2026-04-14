//! Shared validation helpers for benchmark binaries.
//!
//! Each bench binary includes this via `#[path = "common/validate.rs"] mod validate;`
//! and calls the helpers before entering the benchmark loop to catch
//! structural regressions early.

#![allow(dead_code)]

use bbnf::runtime::tape::{TapeCursor, TapeKind};

/// Extract the span text for a cursor from the input.
fn cursor_text<'a>(cursor: &TapeCursor<'_>, input: &'a str) -> &'a str {
    let (lo, hi) = cursor.span();
    &input[lo as usize..hi as usize]
}

/// Recursively count all records reachable from a cursor.
fn count_reachable(cursor: TapeCursor<'_>) -> usize {
    let mut total = 1;
    for child in cursor.children() {
        total += count_reachable(child);
    }
    total
}

/// Assert that the tape record count falls in `[min, max]`.
/// Panics with `label` context if the assertion fails.
pub fn assert_record_count_range(
    tape: &bbnf::runtime::tape::Tape,
    min: usize,
    max: usize,
    label: &str,
) {
    let count = tape.len();
    assert!(
        count >= min && count <= max,
        "{}: tape record count {} outside [{}, {}]",
        label,
        count,
        min,
        max
    );
}

/// Assert that the root cursor's kind is one of the expected compound
/// kinds (Rule, Seq, Alt, Repeat). Panics with `label` context.
pub fn assert_root_kind_compound(
    cursor: &TapeCursor<'_>,
    label: &str,
) {
    assert!(
        matches!(
            cursor.kind(),
            TapeKind::Rule
                | TapeKind::Seq
                | TapeKind::Alt
                | TapeKind::Repeat
                | TapeKind::TokenDispatch
                | TapeKind::MapValue
        ),
        "{}: root kind {:?} is not a compound type",
        label,
        cursor.kind()
    );
}

/// Assert that the root cursor's kind matches one of the given expected kinds.
/// Panics with `label` context.
pub fn assert_root_kind(
    cursor: &TapeCursor<'_>,
    expected: &[TapeKind],
    label: &str,
) {
    assert!(
        expected.contains(&cursor.kind()),
        "{}: root kind {:?} not in expected set {:?}",
        label,
        cursor.kind(),
        expected
    );
}

/// Assert that the root span covers the full input.
pub fn assert_root_covers_input(
    cursor: &TapeCursor<'_>,
    input_len: usize,
    label: &str,
) {
    let (lo, hi) = cursor.span();
    assert_eq!(lo, 0, "{}: root span_lo should be 0", label);
    assert_eq!(
        hi as usize, input_len,
        "{}: root span_hi {} should equal input len {}",
        label, hi, input_len
    );
}
