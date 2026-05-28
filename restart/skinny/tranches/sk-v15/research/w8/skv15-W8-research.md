# SK-V15 W8 Research: EagerTape And OffsetTape Lowerers

Date: 2026-05-28.
Scope: W8 authority, lowerer scaffold inventory, generated fixture route, staging
guards.
Output: this file.

## 1 - Findings

W8 is unblocked by W7 admission at `9a0079cfb`. SPEC W8 consumes
`DEP-W8-LOWERERS-A` and covers only EagerTape plus OffsetTape. W9 owns
EventTape, SinkOnly, CollapsedStage, and the all-five gate.

The scaffold is direct:

- `skinny/crates/codegen/src/lower/eager_tape.rs` returns
  `format!("rule {} -> eager_tape", rule.name)`.
- `skinny/crates/codegen/src/lower/offset_tape.rs` returns
  `format!("rule {} -> offset_tape", rule.name)`.
- `lower::rust::lower_to_rust` already consumes the W7 decision facts and
  stores each selected lowerer body in `RuleLoweringPlan`.

The same-wave consumer can be `lower_to_rust` fixture output. A full runtime
file emitter for EagerTape/OffsetTape does not exist yet, and W8 does not need
to turn into the W9 all-five/runtime-generator wave. The W8 obligation is to
replace label strings with runtime-relevant output paths and prove fixtures
would fail the old scaffold.

## 2 - Recommendations

Add a shared `tape_plan` renderer under `codegen::lower` and have EagerTape and
OffsetTape call it with different flavors. The renderer should walk
`BackendExpr` and emit deterministic operation lines for entry, sequence,
dispatch alternatives, repeats, optional branches, byte literals, regex spans,
rule calls, span marks, tape emits, direct builds, value projection, and
return. This is runtime-relevant because the output changes with the backend
expression tree; it is not a label or pass-through shell.

Add top-level exact tests for the DISPATCH commands:

- `backend_lowerer_fixture_rejects_label_string_scaffold`
- `lower_eager_tape_emits_runtime_relevant_diff`
- `lower_offset_tape_emits_runtime_relevant_diff`

Use small grammars that naturally select EagerTape and OffsetTape through the
decision spine. Do not hand-edit generated files.

## 3 - Risks

Primary risk: rendering a prettier label string would be a W8 contrivance. The
fixture must assert expression-derived operations, not only renamed shape text.

Secondary risk: W8 could sprawl into EventTape or all-five gate work. That is
W9 scope unless a W8 failure proves an intrinsic dependency.

Staging risk: pre-existing dirty files remain in docs, prior-tranche CSS JSON
reports, `skinny/crates/bbnf-bench/src/generated_real_typed.rs`, and seven
skinny CSS generated runtimes. W8 must stage explicit files only.

## 4 - Sources

- `restart/skinny/tranches/sk-v15/SPEC.md` Section 11.
- `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md` W8.
- `skinny/crates/codegen/src/lower/eager_tape.rs`.
- `skinny/crates/codegen/src/lower/offset_tape.rs`.
- `skinny/crates/codegen/src/lower/rust.rs`.
- `skinny/crates/ir/src/lib.rs`.
