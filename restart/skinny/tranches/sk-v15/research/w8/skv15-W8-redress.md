# SK-V15 Wave W8 Redress: EagerTape And OffsetTape Lowerers

Status: ADMIT-W8.

W8 consumes `DEP-W8-LOWERERS-A`. The EagerTape and OffsetTape label-string
lowerer scaffolds are replaced by expression-derived runtime plan output, and
the exact fixture tests fail the old `rule X -> eager_tape/offset_tape`
format-string path.

## Evidence

- Research/plan/challenge commit: `98cd63612`.
- Implementation slice: this W8 source/redress commit.
- Required consumer commands:
  - `cargo test --manifest-path skinny/Cargo.toml -p codegen backend_lowerer_fixture_rejects_label_string_scaffold -- --exact`
  - `cargo test --manifest-path skinny/Cargo.toml -p codegen lower_eager_tape_emits_runtime_relevant_diff -- --exact`
  - `cargo test --manifest-path skinny/Cargo.toml -p codegen lower_offset_tape_emits_runtime_relevant_diff -- --exact`
- Result: all three required exact commands passed with one test executed each.
- W7 regression guard:
  `cargo test --manifest-path skinny/Cargo.toml -p codegen decision_spine_changes_generated_selection_fixture -- --exact`
  passed.

## Implementation

`lower/tape_plan.rs` walks `BackendExpr` and emits deterministic operation
plans for entry, sequence, alternation, repeats, optional branches, byte
literals, regex spans, rule calls, span marks, tape emits, direct builds, value
projection, and return.

`EagerTape` and `OffsetTape` now call the shared renderer with separate flavor
semantics. The output names `ParserState` and `TapeBuilder` and uses
shape-specific operations such as `eager_match_literal_hex`,
`offset_match_literal_hex`, `capture_span_value`, `record_span_offsets`, and
`ParserState::emit_plain_offset`.

W8 does not claim EventTape, SinkOnly, CollapsedStage, or the all-five gate.
Those remain W9 scope.

## Verification Notes

Targeted scaffold scan over the W8 lowerer paths finds no live
`-> eager_tape` or `-> offset_tape` scaffold outside negative test assertions.

Full `cargo test --manifest-path skinny/Cargo.toml -p codegen` remains blocked
by pre-existing dirty skinny CSS generated runtime files, as recorded in W7.
The W8 exact codegen consumers pass and no generated CSS runtime file is
staged.

Invariant checks:

- `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` returns `16`.
- `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l`
  returns `67`.

## Dependency Rows

- `DEP-W8-LOWERERS-A`: consumed by scaffold-negative lowerer fixtures plus
  runtime-relevant EagerTape/OffsetTape operation-plan output.

W9 is unblocked by W8 admission.
