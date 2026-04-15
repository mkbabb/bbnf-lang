//! AV.3.4 — DTA diagnostic replay.
//!
//! The happy-path driver (AV.3.1–3.3) does not backtrack. Diagnostic
//! mode re-enters the same state table with an instrumentation hook
//! that tracks:
//!
//!   - `furthest_offset`: the deepest byte offset the driver reached
//!   - `failing_state`: the state that dispatched to no child at that
//!     offset
//!   - `failing_rule`: the rule active when the dead-end occurred
//!   - `states_visited`: cumulative state transitions (diagnostics-
//!     mode overhead budget)
//!
//! One automaton, two driver modes — the V4 PSI driver re-runs the
//! same `DtaTable` with the [`DtaDiagnostic`] hook live. These tests
//! exercise the diagnostic struct's invariants so the V4 driver has a
//! stable contract.

use bbnf::runtime::tape::{DtaDiagnostic, DtaRuleId, DtaStateId};

#[test]
fn diagnostic_empty_has_sentinel_ids() {
    let d = DtaDiagnostic::EMPTY;
    assert_eq!(d.furthest_offset, 0);
    assert_eq!(d.failing_state, DtaStateId::NONE);
    assert_eq!(d.failing_rule.0, u32::MAX);
    assert_eq!(d.states_visited, 0);
}

#[test]
fn observe_updates_only_on_advance() {
    let mut d = DtaDiagnostic::EMPTY;
    d.observe(10, DtaStateId(1), DtaRuleId(7));
    assert_eq!(d.furthest_offset, 10);
    assert_eq!(d.failing_state, DtaStateId(1));
    assert_eq!(d.failing_rule, DtaRuleId(7));

    // Shallower offset — no update.
    d.observe(5, DtaStateId(2), DtaRuleId(8));
    assert_eq!(d.furthest_offset, 10);
    assert_eq!(d.failing_state, DtaStateId(1));

    // Deeper offset — updates.
    d.observe(15, DtaStateId(3), DtaRuleId(9));
    assert_eq!(d.furthest_offset, 15);
    assert_eq!(d.failing_state, DtaStateId(3));
    assert_eq!(d.failing_rule, DtaRuleId(9));
}

#[test]
fn observe_ignores_equal_offset() {
    let mut d = DtaDiagnostic::EMPTY;
    d.observe(10, DtaStateId(1), DtaRuleId(7));
    d.observe(10, DtaStateId(2), DtaRuleId(8));
    // Equal offset must not overwrite — the first deepest wins.
    assert_eq!(d.failing_state, DtaStateId(1));
    assert_eq!(d.failing_rule, DtaRuleId(7));
}

#[test]
fn tick_increments_states_visited() {
    let mut d = DtaDiagnostic::EMPTY;
    for _ in 0..5 {
        d.tick();
    }
    assert_eq!(d.states_visited, 5);
}

#[test]
fn tick_saturates_at_u32_max() {
    let mut d = DtaDiagnostic::EMPTY;
    d.states_visited = u32::MAX - 1;
    d.tick();
    assert_eq!(d.states_visited, u32::MAX);
    d.tick();
    // Saturated — must not wrap.
    assert_eq!(d.states_visited, u32::MAX);
}
