# SK-V15 Wave W9 Redress: Remaining Lowerers And All-Five Gate

Status: ADMIT-W9.

W9 consumes `DEP-W9-LOWERERS-B`. EventTape and CollapsedStage no longer emit
label-string lowerers, SinkOnly's per-rule plan names its compiled
`JsonSink+DirectBuild` path, and the all-five report gate consumes exactly the
canonical five BackendShape variants.

## Evidence

- Research/plan/challenge commit: `b3c50df2e`.
- Implementation slice: this W9 source/redress commit.
- Required consumer commands:
  - `cargo test --manifest-path skinny/Cargo.toml -p codegen lower_event_tape_emits_runtime_relevant_diff -- --exact`
  - `cargo test --manifest-path skinny/Cargo.toml -p codegen lower_sink_only_emits_runtime_relevant_diff -- --exact`
  - `cargo test --manifest-path skinny/Cargo.toml -p codegen lower_collapsed_stage_emits_runtime_relevant_diff -- --exact`
  - `cargo xtask gate-json --check-results --skv15-backend-lowerers-report restart/skinny/tranches/sk-v15/research/w9/skv15-W9-backend-lowerers-report.json`
- Result: all four required consumers passed. The broadened W8 scaffold guard
  `backend_lowerer_fixture_rejects_label_string_scaffold` also passed.

## Implementation

`lower/tape_plan.rs` now has four flavors: EagerTape, OffsetTape, EventTape,
and CollapsedStage. EventTape emits `runtime_plan::EventTapeRule` with
`ParserState+TapeBuilder+EventGrammar`, while CollapsedStage emits
`runtime_plan::CollapsedStageRule` with `ParserState+CollapsedStagePlan`.
Both outputs are derived from `BackendExpr` operations and include literal,
span, tape, direct-build, and return operations.

SinkOnly keeps its stronger `SinkOnlyProgram` and compiled runtime path. Its
per-rule plan now emits `runtime_plan::SinkOnlyRule
generated_runtime=JsonSink+DirectBuild`, and the exact test verifies the
compiled generated runtime contains `parse_direct` and `JsonSink`.

`gate-json` accepts `--skv15-backend-lowerers-report <path>` with
`--check-results`, validates W0 results, and consumes
`skv15-W9-backend-lowerers-report.json`. The report gate rejects missing or
extra shapes, non-implemented lowerers, absent command evidence, and EventTape
anti-sidecar fields that are not `absent`.

## Verification Notes

Product-code scaffold scan:

- `rg -n -- '-> event_tape|-> collapsed_stage|rule .* -> sink_only|todo!|unimplemented!|pass-through' skinny/crates/codegen/src/lower || true`
- Result: no hits.

Product-code EventTape anti-sidecar scan:

- `rg -n -g '*.rs' -- 'UnionTape|union_tape|EventCursor|EventStream|event_stream|event_vector|EventVector|Retained.*Stream|retained.*stream|sidecar.*event|event.*sidecar|alternate document projection|alternate.*projection|class column|class_column|second tape|second_tape|sixth BackendShape|new BackendShape' skinny/crates/ir/src skinny/crates/passes/src skinny/crates/codegen/src/lower skinny/crates/codegen/src/runtime_generator.rs skinny/crates/runtime/src || true`
- Result: no product-code hits.

The broader scan over `skinny/crates/codegen/src/lib.rs` finds only negative
test assertions for the forbidden EventTape terms.

The first all-five report gate attempt invoked the unrelated W2 frozen-root
lock gate before the W9 report consumer and failed while W9 source files were
still dirty. `xtask` was corrected so the W9 flag validates W0 results and the
W9 report, then the required W9 command passed.

Full `cargo test --manifest-path skinny/Cargo.toml -p codegen` remains blocked
by pre-existing dirty skinny CSS generated runtime files, as recorded in W7
and W8. Those generated files are not part of the W9 staged slice.

Invariant checks:

- `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` returns `16`.
- `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l`
  returns `67`.
- `all_backend_shapes()` still returns exactly
  `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`.

## Dependency Rows

- `DEP-W9-LOWERERS-B`: consumed by scaffold-negative lowerer fixtures,
  runtime-relevant EventTape/SinkOnly/CollapsedStage output, the all-five
  report gate, and product-code anti-sidecar scans.

W10 is unblocked by W9 admission.
