# SK-V14 W2-A: Xtask Regen Architecture

Date: 2026-05-24.
Scope: Inspect the skinny xtask regen/check command surface and the existing codegen emission APIs.
Output: this file.

## §1 — Findings (concrete, file:line cited)

- `skinny/xtask/src/main.rs:8` exposes `regen-json`, `check-json`, `regen-real-typed`, and `check-real-typed`, but no `regen-css` or `check-css-l4-*` command. The dispatcher hard-codes JSON regen/check at `skinny/xtask/src/main.rs:18-21`, so W2 must add the first CSS facade rather than relying on an existing grammar-family command.
- JSON regen/check currently read one source file and write/check one runtime tree: `regen_json` reads `grammars/json.bbnf`, calls `codegen::emit_from_source("json", ...)`, and writes `crates/runtime/src/grammars/json` at `skinny/xtask/src/main.rs:121-125`; `check_json` mirrors the same path at `skinny/xtask/src/main.rs:128-134`.
- There is no shared `skinny/xtask/src/regen.rs` module yet; `skinny/xtask/src/main.rs:6` declares only `mod real_typed_schema;`. W2 is therefore the first opportunity to extract shared write/check mechanics instead of adding another one-off command body.
- The skinny workspace metadata currently declares only JSON source/output rows under `[workspace.metadata.bbnf.grammars.json]` at `skinny/Cargo.toml:54-70`. W2's required CSS source set and dual destinations cannot be inferred from existing skinny metadata without adding or interpreting new metadata.
- `codegen::emit_runtime_profile(grammar_name)` is the existing public hook for provider-backed runtime emission at `skinny/crates/codegen/src/lib.rs:117-120`. CSS providers are selected by `render_runtime_profile` through seven explicit `RuntimeProvider::CssL4*` branches at `skinny/crates/codegen/src/lib.rs:166-208`.
- `GrammarProfile` currently enumerates JSON plus seven CSS profiles at `skinny/crates/codegen/src/grammar_profile.rs:100-110`; bare `css_l4` is not one of them. A W2 `regen-css` implementation must aggregate the seven CSS profiles instead of calling `emit_runtime_profile("css_l4")`.

## §2 — Recommendations (named falsifiability gates)

- `G-W2-REGEN-FAMILY`: introduce shared `regen.rs` write/check plumbing parameterized by a roster entry (`profile`, `source_inputs`, `metadata_inputs`, `output_dir`, `check_command`) and keep `regen_css.rs` as the W2 facade that supplies CSS roster entries.
- `G-W2-CSS-AGGREGATE`: `cargo xtask regen-css` loops over the seven CSS runtime profiles currently present in `grammar_profile.rs:100-110` and writes their existing `skinny/crates/runtime/src/grammars/css_l4_*` trees.
- `G-W2-COMPANION-CHECK`: add one companion command per CSS profile (`check-css-l4-declaration-values`, etc.) plus an aggregate check path or aggregate `regen-css` check helper, all backed by the same roster.
- `G-W2-NO-BARE-CSS-L4-PROFILE`: tests or command validation must fail if `regen-css` attempts to use unsupported `css_l4` as a skinny runtime profile.

## §3 — Risks (REDRESS entries to pre-block)

- A one-off `regen-css` branch in `main.rs` would repeat the P-6 recurrence vector named in `restart/skinny/tranches/sk-v14/SPEC.md:502-506`; the plan must route through a parameterized `regen-{grammar}` family contract.
- Pulling W5's trait-dispatch refactor into W2 would exceed the W2 surface. W2 can register and check the existing provider outputs, but removal of the provider mesh is sequenced to W5 by `restart/skinny/tranches/sk-v14/SPEC.md:242`.
- Treating `regen-css` as row admission evidence is pre-blocked; W2 is an xtask correctness wave, not a CSS L4 SOTA wave, per `restart/skinny/tranches/sk-v14/SPEC.md:502-506`.

## §4 — Sources (every external citation)

No external citations. Local repository sources only.
