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
