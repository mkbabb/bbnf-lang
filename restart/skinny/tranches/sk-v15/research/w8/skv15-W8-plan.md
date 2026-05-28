# SK-V15 Wave W8 Plan: EagerTape And OffsetTape Lowerers

Inputs: `skv15-W8-research.md`, W7 admission `9a0079cfb`, W8 sidecar inventory.
Intervention: replace EagerTape and OffsetTape label-string lowerers with a
shared operation-plan renderer over `BackendExpr`, then prove exact fixtures
fail the old scaffold.

Owner paths:

- `skinny/crates/codegen/src/lower/{eager_tape.rs,offset_tape.rs,mod.rs,tape_plan.rs}`
- `skinny/crates/codegen/src/lib.rs`
- `restart/skinny/tranches/sk-v15/research/w8/*`
- `skinny/REDRESS.md`

Falsifiability gate:

- `cargo test --manifest-path skinny/Cargo.toml -p codegen backend_lowerer_fixture_rejects_label_string_scaffold -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen lower_eager_tape_emits_runtime_relevant_diff -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p codegen lower_offset_tape_emits_runtime_relevant_diff -- --exact`

Hard cap: 75 minutes redress.

Same-wave consumer: `lower::rust::lower_to_rust` and its `RuleLoweringPlan`
fixtures. A full runtime file emitter for these shapes is W9+ unless W8 exposes
an intrinsic blocker.

Pre-blocked routes: label strings, `todo!`, pass-through shells, generated file
hand patches, EventTape sidecar work, sixth `BackendShape`, retained
class/structural/cursor streams, and unstaged unrelated dirty files.

Expected result: W8 admits if EagerTape and OffsetTape bodies contain
expression-derived runtime plan operations and the exact tests reject the old
`rule X -> eager_tape/offset_tape` scaffold.
