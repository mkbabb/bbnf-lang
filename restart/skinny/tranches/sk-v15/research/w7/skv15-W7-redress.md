# SK-V15 Wave W7 Redress: Decision Engine Spine

Status: ADMIT-W7.

W7 consumes `DEP-W7-DECISION-SPINE`. The zero-rule e-graph, tautological CSP
proof, and grammar-named generic decision records are replaced by an executable
decision spine with a generated-selection consumer.

## Evidence

- Research/plan/challenge commit: `765779d98`.
- Implementation slice: this W7 source/redress commit.
- Required consumer commands:
  - `cargo test --manifest-path skinny/Cargo.toml -p passes decision_egraph_rewrite_changes_selected_shape -- --exact`
  - `cargo test --manifest-path skinny/Cargo.toml -p passes decision_csp_rejects_missing_required_fact -- --exact`
  - `cargo test --manifest-path skinny/Cargo.toml -p codegen decision_spine_changes_generated_selection_fixture -- --exact`
- Result: all three required exact commands passed with one test executed each.
- Broader passes verification:
  `cargo test --manifest-path skinny/Cargo.toml -p passes` passed `13` tests.

## Implementation

`backend_egraph.rs` now runs `NormalizeDirectSinkCost`, a real scheduled
rewrite that asserts an equivalent normalized direct-sink node after
`SinkOnly` with `DirectBuildNoConsumer` is already eligible. The selected
`ActiveCostFacts` records `egraph_rewrite_count = report.total_applied`, and
the exact e-graph test proves the rewrite changes selected shape rather than
only metadata.

`decision_csp.rs` removes the tautological parity constraint and records
grammar-neutral candidate scope. Capacity is a required generic fact: the exact
CSP test proves an admitted candidate is `sat`, while altering the same
candidate to `capacity_cost = 2` makes the CSP `unsat` with
`selected_rule_count = 0`.

`codegen` now has an exact generated-selection fixture. The same JSON grammar
lowers `object` as `SinkOnly` when `direct_build_consumer=true`, then as
`OffsetTape` when the generic target facts disable direct-build consumption.
The rule body changes through `lower_to_rust`; no generated output is
hand-patched.

## Grammar-Neutrality

The live decision spine no longer records `csp_named_grammars`,
`static_css_provider_status`, `json_sink_only_status`, or the
`JSON-CSS-W7-CSP-CASCADE-CONSUMED-BUT-NO-ROW-MOVEMENT` block id. The targeted
scan over `backend_egraph.rs`, `decision_csp.rs`, `ir/src/cost.rs`, and
`codegen/src/lower/rust.rs` has no `json_*` or `css_*` generic decision hits.

## Verification Notes

`cargo test --manifest-path skinny/Cargo.toml -p codegen` currently fails only
at `tests::css_l4_generated_runtimes_reproducible_from_request` with
`DifferentFile("generated.rs")`. That failure is caused by the pre-existing
dirty skinny CSS generated runtime files and is not staged as W7 work. The W7
codegen exact consumer passes.

Invariant checks:

- `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` returns `16`.
- `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l`
  returns `67`.

## Dependency Rows

- `DEP-W7-DECISION-SPINE`: consumed by the executable e-graph rewrite count,
  CSP SAT/UNSAT fact-removal proof, grammar-neutral decision record cleanup,
  and generated-selection fixture.

W8 is unblocked by W7 admission.
