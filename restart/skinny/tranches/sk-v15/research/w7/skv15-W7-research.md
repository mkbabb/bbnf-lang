# SK-V15 W7 Research: Decision Engine Spine

Date: 2026-05-28.
Scope: W7 authority, e-graph/CSP scaffold inventory, generated selection
consumer, staging guards.
Output: this file.

## 1 - Findings

W7 is unblocked because W6 closed with a routed typed CSS rejection. SPEC W7
accepts W6 admitted or routed and consumes `DEP-W7-DECISION-SPINE`.

The current decision spine is executable but scaffolded:

- `skinny/crates/passes/src/backend_egraph.rs` unions ranked candidates into
  one e-class, then runs an empty rewrite slice. `ActiveCostFacts` records
  e-graph node/class/iteration counts but no rewrite count.
- `skinny/crates/passes/src/decision_csp.rs` pins the active candidate and
  then adds mostly tautological predicates. The only real generic predicate
  already present is capacity cost, but it is not tested as a SAT/UNSAT
  selector.
- `DecisionCspFacts` still carries grammar-named status fields and a
  `JSON-CSS-*` block id. Those fields are metadata, not a generic selection
  basis, and W7 must remove them from the live decision record.
- `skinny/crates/codegen/src/lower/rust.rs` already fail-closes if
  active-cost or decision-CSP facts are missing or unsatisfied. That is the
  same-wave generated consumer for W7.

The least invasive real rewrite is direct-sink cost normalization. Once a
candidate is already eligible for `SinkOnly` with `DirectBuildNoConsumer`, it
does not retain an existing tape. The e-graph can assert an equivalent
normalized node with zero static-size and shape-rank penalties. This is
generic: it keys on `BackendShape::SinkOnly` plus `ShapeRationale`, not on a
grammar name or CSS/JSON row.

## 2 - Recommendations

Implement W7 in three source slices:

- Add a `NormalizeDirectSinkCost` e-graph rewrite in
  `backend_egraph.rs`, run it from production selection, and record
  `egraph_rewrite_count = report.total_applied`.
- Keep CSP grammar-neutral and make the capacity requirement explicitly
  falsifiable. A selected candidate with capacity cost above the permitted
  bound must make the CSP `unsat`; the corresponding admitted candidate must
  be `sat`.
- Add a codegen fixture that derives two valid backend-shape plans for the
  same grammar with different generic target facts and proves lowering output
  changes. Use `direct_build_consumer=true` versus `false`; do not patch
  generated output by hand.

## 3 - Risks

Primary risk: a rewrite that only increments metadata would be a W7
contrivance. The rewrite must create a lower-cost equivalent node that can
change extraction.

Secondary risk: renaming serialized fields in `DecisionCspFacts` can ripple
through snapshots. The current codebase has no committed snapshot consumer for
the grammar-named field names, so the field cleanup is local to `ir` and
`passes`.

Staging risk: pre-existing dirty files include `docs/precepts`,
prior-tranche CSS JSON reports, `skinny/crates/bbnf-bench/src/generated_real_typed.rs`,
and seven dirty skinny CSS generated runtime files. W7 must stage explicit
paths only.

## 4 - Sources

- `restart/skinny/tranches/sk-v15/SPEC.md` Section 10.
- `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md` W7.
- `skinny/crates/passes/src/backend_egraph.rs`.
- `skinny/crates/passes/src/decision_csp.rs`.
- `skinny/crates/passes/src/lib.rs`.
- `skinny/crates/codegen/src/lower/rust.rs`.
