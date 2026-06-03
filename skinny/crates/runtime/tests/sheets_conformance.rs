//! Permanent conformance characterization for the general recursive-descent
//! Google Sheets recognizer (`runtime::grammars::sheets`).
//!
//! TWO tiers:
//!  - TOWER CORE: formulas the operator/literal/func-call tower must recognize
//!    — these are hard `Ok` assertions; a failure here is a real emitter bug.
//!  - DEFERRED leaves: `cell_ref`/`range` are deferred to a raw `Span`
//!    delimiter-scan (the grammar's AU.6.7 TODO — typed leaves not yet precise).
//!    These cases record the ACTUAL behavior with a DEFERRED-IMPRECISE marker;
//!    they are NOT pinned to pass/fail, only characterized, so the test honestly
//!    documents the real state without pretending precision exists.

use runtime::grammars::sheets;
use runtime::grammars::sheets::GoogleSheetsKind;

/// The formulas the operator/literal/func-call tower must recognize. A failure
/// in any of these is a genuine recognizer bug, not a deferred imprecision.
const TOWER_CORE: &[&str] = &[
    "=1+2*3",     // additive over multiplicative precedence
    "=(1+2)*3",   // parenthesized grouping
    "=2^3",       // power operator
    "=1<>2",      // not-equal comparison
    "=-1+2",      // unary minus
    "=TRUE",      // boolean literal
    "=FALSE",     // boolean literal
    "=\"hello\"", // string literal
    "=1",         // bare number
    "=SUM(1,2)",  // func_call with SIMPLE numeric args
    "=1&2",       // concat operator
];

/// Deferred-leaf formulas: cell_ref / range. The grammar defers these to a raw
/// `Span` delimiter-scan (AU.6.7). Behavior is characterized, never pinned.
const DEFERRED_LEAVES: &[&str] = &[
    "=A1",          // single cell reference
    "=A1:B2",       // range
    "=SUM(A1:B2,3)", // func_call over a range arg
];

#[test]
fn tower_core_recognizes() {
    // Every TOWER-CORE formula MUST recognize (Ok). A failure is a real bug.
    let mut failures = Vec::new();
    for &formula in TOWER_CORE {
        match sheets::recognize(formula) {
            Ok(()) => {}
            Err(error) => failures.push(format!("  {formula:?} -> Err({error})")),
        }
    }
    assert!(
        failures.is_empty(),
        "TOWER-CORE formulas failed to recognize (real emitter bug):\n{}",
        failures.join("\n"),
    );
}

#[test]
fn tower_core_parses_to_document() {
    // The `parse` front-door must also yield a document for each tower-core
    // formula (the lazy tape-backed projection is reachable, not just recognize).
    for &formula in TOWER_CORE {
        let parsed = sheets::parse(formula);
        assert!(
            parsed.is_ok(),
            "TOWER-CORE parse failed for {formula:?}: {:?}",
            parsed.err().map(|error| error.to_string()),
        );
    }
}

/// The grammar-derived TYPED value API: the `->` projections in the Sheets
/// grammar (`number -> f64`, `boolean -> true|false`, `error_literal -> Nu8`)
/// surface as `as_f64`/`as_bool`/`as_tag` on the lazy node view. This test reads
/// real typed values through the public `runtime::grammars::sheets` API and
/// asserts the accessors dispatch on the node's decoded `Kind` (returning `None`
/// for the wrong kind, `Some(value)` for the right one).
#[test]
fn typed_leaves() {
    // Find the first recorded node of a given kind and run a reader over it.
    fn read_node<T>(
        formula: &str,
        kind: GoogleSheetsKind,
        read: impl Fn(&sheets::GoogleSheetsNode) -> T,
    ) -> Option<T> {
        let document = sheets::parse(formula).expect("formula must parse");
        // Collect the matching node's read result before the document is dropped —
        // the closure runs while the document (and its tape) are alive. Binding to
        // a local forces the iterator temporary to drop before `document`.
        let result = document
            .nodes()
            .find(|node| node.rule_kind() == Some(kind))
            .map(|node| read(&node));
        result
    }

    // `=1` — the number leaf projects `-> f64`; `as_f64` must decode it.
    let one = read_node("=1", GoogleSheetsKind::Number, |node| node.as_f64())
        .expect("=1 must contain a Number node");
    assert_eq!(one, Some(1.0), "as_f64 on `=1` number node");

    // `=3.5` — a fractional number, same projection.
    let frac = read_node("=3.5", GoogleSheetsKind::Number, |node| node.as_f64())
        .expect("=3.5 must contain a Number node");
    assert_eq!(frac, Some(3.5), "as_f64 on `=3.5` number node");

    // `=TRUE` / `=FALSE` — the boolean leaf projects `-> true | -> false`.
    let truth = read_node("=TRUE", GoogleSheetsKind::Boolean, |node| node.as_bool())
        .expect("=TRUE must contain a Boolean node");
    assert_eq!(truth, Some(true), "as_bool on `=TRUE` boolean node");
    let falsity = read_node("=FALSE", GoogleSheetsKind::Boolean, |node| node.as_bool())
        .expect("=FALSE must contain a Boolean node");
    assert_eq!(falsity, Some(false), "as_bool on `=FALSE` boolean node");

    // WRONG-KIND dispatch: `as_f64` on a Boolean node returns `None` — the
    // accessor guards on the node's decoded Kind, not just the span text.
    let wrong = read_node("=TRUE", GoogleSheetsKind::Boolean, |node| node.as_f64())
        .expect("=TRUE must contain a Boolean node");
    assert_eq!(wrong, None, "as_f64 on a non-number node must be None");
    let wrong_bool = read_node("=1", GoogleSheetsKind::Number, |node| node.as_bool())
        .expect("=1 must contain a Number node");
    assert_eq!(wrong_bool, None, "as_bool on a non-boolean node must be None");
}

#[test]
fn deferred_leaves_characterized() {
    // DEFERRED-IMPRECISE: cell_ref / range are deferred to a raw Span scan
    // (AU.6.7). We do NOT pin these to Ok or Err — we record whatever actually
    // happens so the test pins the real state honestly. The single assertion is
    // only that `recognize` and `parse` AGREE per input (no internal divergence).
    for &formula in DEFERRED_LEAVES {
        let recognized = sheets::recognize(formula).is_ok();
        let parsed = sheets::parse(formula).is_ok();
        // recognize == parse.map(|_| ()) by construction; this guards that the
        // two front-doors never disagree even while the leaf precision is deferred.
        assert_eq!(
            recognized, parsed,
            "DEFERRED-IMPRECISE: recognize/parse disagree for {formula:?}",
        );
    }
}
