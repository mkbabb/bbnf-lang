# SK-V15 Wave W9 Plan: Remaining Lowerers And All-Five Gate

Inputs: `skv15-W9-research.md`, W8 admission `a913a1ffa`, SPEC Section 12,
and DISPATCH-PROMPT W9.
Intervention: replace EventTape and CollapsedStage label-string lowerers,
promote SinkOnly's per-rule plan to a runtime-plan marker, and add a
gate-consumed all-five lowerer report.

Owner paths:

- `skinny/crates/codegen/src/lower/{event_tape.rs,sink_only.rs,collapsed_stage.rs,tape_plan.rs}`
- `skinny/crates/codegen/src/lib.rs`
- `skinny/xtask/src/main.rs`
- `restart/skinny/tranches/sk-v15/research/w9/*`
- `skinny/REDRESS.md`

Falsifiability gate:

- `cargo test --manifest-path skinny/Cargo.toml -p codegen lower_event_tape_emits_runtime_relevant_diff -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen lower_sink_only_emits_runtime_relevant_diff -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen lower_collapsed_stage_emits_runtime_relevant_diff -- --exact`
- `cargo xtask gate-json --check-results --skv15-backend-lowerers-report restart/skinny/tranches/sk-v15/research/w9/skv15-W9-backend-lowerers-report.json`

Same-wave consumer: `lower::rust::lower_to_rust` for lowerer plans,
`sink_only::lower_program` plus compiled-runtime rendering for SinkOnly, and
`gate-json` for the all-five report.

All-five report fields:

- schema and wave identity: `schema_version`, `wave_id`, `dep_row`.
- shape canon: exact ordered five-shape list, count, and no-extra status.
- lowerers: one entry per canonical shape with status `implemented`.
- EventTape anti-sidecar: all forbidden surfaces `absent`.
- commands: exact consumer commands.
- disposition: `ADMIT-W9`.

Pre-blocked routes: label strings, `todo!`, pass-through shells, generated file
hand patches, EventTape sidecar vector, retained parser stream, public substrate
API, alternate document projection, public `UnionTape`, new/sixth
`BackendShape`, W10 FNV work, and unrelated dirty files.

Expected result: W9 admits if all three exact tests pass, the all-five report
gate consumes the report, EventTape anti-sidecar scans are clean over product
code, and `DEP-W9-LOWERERS-B` is consumed.
