# SK-V14 W6.0 A3: Generator Feasibility

Date: 2026-05-26.
Scope: identify the existing generator capabilities and the gap W6.0 must close.
Output: this file.

## §1 — Findings (concrete, file:line cited)

1. The root xtask currently exposes only `regen`. `xtask/src/main.rs:23-66` has no `regen-css` subcommand, so `SPEC.md:983-984` cannot execute at root HEAD.
2. Root `regen` writes grammar parser output and registry sidecars, not runtime support modules. The target path in `xtask/src/regen.rs:204-213` is `crates/core/src/grammar/generated/<ident>.rs`; no root runtime destination is present in the root xtask.
3. The skinny generator is source/request-driven, but it emits skinny fact-stream runtimes. `skinny/crates/codegen/src/grammar_provider.rs:31-69` validates source and frontend facts, while `skinny/crates/codegen/src/runtime_generator.rs:73-110` emits CSS fact-stream files (`config.rs`, `generated.rs`, `mod.rs`, `parser.rs`, `sink.rs`) under the skinny runtime roster.
4. That skinny output is not a substitute for the root CSS L4 API. The root CSS runtime has public typed values and arena/document APIs consumed by root tests and generated parser bindings; skinny fact streams do not expose `CssDocument`, `CssStructBuilder`, `CssTypedValue`, or `CssColor`.

## §2 — Recommendations (named falsifiability gates)

- W6.0 must add root runtime generation, not call skinny `regen-css` by accident.
- A valid W6.0 redress must prove `cargo xtask regen-css` at the repository root can recreate the CSS L4 runtime collapse product after deleting `crates/core/src/runtime/css_l4/`.
- The generated product must keep the existing public surface and pass the root CSS runtime tests; otherwise W6.0 is REJECT, not partially admitted.

## §3 — Risks (REDRESS entries to pre-block)

- A generator that copies committed CSS files into a generated file is a static-centralization recurrence, not a root runtime generator.
- A generator that emits a generic tree without CSS typed values violates `feedback_preserve-rich-ast` and will fail the CSS parity tests.
- A root `regen-css` alias that only delegates to `skinny/` leaves W6.0's destructive gate unsatisfied because the root runtime tree remains absent.

## §4 — Sources (every external citation)

- Local repository only; no external sources.
