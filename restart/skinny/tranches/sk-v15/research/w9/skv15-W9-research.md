# SK-V15 W9 Research: Remaining BackendShape Lowerers

Date: 2026-05-28.
Scope: EventTape, SinkOnly, CollapsedStage, all-five BackendShape gate, and
EventTape anti-sidecar proof.
Output: this file.

## 1 - Findings

W9 is unblocked by W8 admission at `a913a1ffa`. SPEC W9 consumes
`DEP-W9-LOWERERS-B` and covers the three lowerers left out of W8:
EventTape, SinkOnly, and CollapsedStage. It also owns the all-five
BackendShape gate.

The live lowerer state is uneven:

- `skinny/crates/codegen/src/lower/event_tape.rs` still returns
  `format!("rule {} -> event_tape", rule.name)`.
- `skinny/crates/codegen/src/lower/collapsed_stage.rs` still returns
  `format!("rule {} -> collapsed_stage", rule.name)`.
- `skinny/crates/codegen/src/lower/sink_only.rs` already builds
  `SinkOnlyProgram` from `BackendIr` and feeds the compiled JSON runtime
  generator, but the per-rule plan string remains a summary shaped like
  `rule X -> sink_only(...)`.

The canonical shape set is already represented by `BackendShape` and
`all_backend_shapes()` as exactly:

```text
EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage
```

The missing executable piece is a gate that consumes a same-wave lowerer report
and rejects missing shapes, extra shapes, label-only lowerers, and EventTape
sidecar drift.

## 2 - Anti-Sidecar Boundary

EventTape may be a canonical lowerer output plan. It may not become a sidecar
event vector, retained parser stream, public substrate API, alternate document
projection, public `UnionTape`, or new/sixth `BackendShape`.

The existing runtime marker surface is `EventGrammar` plus `ValueRef` over the
existing tape. That is admissible as metadata over the same tape. W9 must not
add a second retained event stream or expose an EventTape document API.

## 3 - Same-Wave Consumer Route

W9 should extend the W8 shared `tape_plan` renderer with EventTape and
CollapsedStage flavors. The renderer already walks `BackendExpr`, so extending
it avoids a second label scaffold and keeps output runtime-relevant.

SinkOnly should keep its stronger `SinkOnlyProgram` path, while its per-rule
plan should explicitly name the runtime consumer surface:
`runtime_plan::SinkOnlyRule generated_runtime=JsonSink+DirectBuild`.

The all-five gate should be a new `gate-json` report consumer:

```text
cargo xtask gate-json --check-results --skv15-backend-lowerers-report <path>
```

It must verify the exact five-shape canon, require every lowerer to be
`implemented`, and consume EventTape anti-sidecar scan status.

## 4 - Risks

Primary risk: replacing `rule X -> event_tape` with a longer string that still
does not depend on the backend expression tree. The tests must assert
expression-derived operations and shape-specific runtime-plan markers.

Secondary risk: treating SinkOnly's summary string as sufficient while ignoring
the real compiled-runtime consumer. The proof should assert both the
`SinkOnlyProgram` path and the per-rule runtime-plan marker.

Hidden-coupling risk: EventTape terminology can accidentally become a new
public stream or sixth shape. The report gate and scans must prove this did
not happen.

Staging risk: pre-existing dirty files remain in docs, prior-tranche CSS JSON
reports, `skinny/crates/bbnf-bench/src/generated_real_typed.rs`, and seven
skinny CSS generated runtimes. W9 must stage explicit files only.

## 5 - Sources

- `restart/skinny/tranches/sk-v15/SPEC.md` Section 12.
- `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md` W9.
- `restart/skinny/tranches/sk-v15/research/w8/skv15-W8-redress.md`.
- `skinny/crates/codegen/src/lower/event_tape.rs`.
- `skinny/crates/codegen/src/lower/sink_only.rs`.
- `skinny/crates/codegen/src/lower/collapsed_stage.rs`.
- `skinny/crates/codegen/src/lower/tape_plan.rs`.
- `skinny/crates/codegen/src/lower/rust.rs`.
- `skinny/crates/ir/src/lib.rs`.
- `skinny/crates/ir/src/cost.rs`.
