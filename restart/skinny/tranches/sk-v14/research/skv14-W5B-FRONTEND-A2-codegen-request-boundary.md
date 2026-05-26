# SK-V14 W5B-FRONTEND A2: Codegen Request Boundary

Date: 2026-05-26.
Scope: read-only inspection of codegen request/fact consumption and W5A boundary.
Output: this file.

## §1 — Findings (concrete, file:line cited)

W5A's request boundary is real but fact-only. `RuntimeGenerationRequest` carries
grammar/profile/source/metadata/output/expected-files fields, and
`emit_runtime_from_request` validates shape, maps request sources into
`grammar::RuntimeSource`, then calls `parse_runtime_source_facts` before
selecting the legacy profile (`skinny/crates/codegen/src/grammar_provider.rs:4`,
`skinny/crates/codegen/src/grammar_provider.rs:31`,
`skinny/crates/codegen/src/grammar_provider.rs:41`).

The current CSS path does not consume frontend IR yet. After facts are
validated, non-JSON requests still call `render_runtime_profile(profile, None)`,
so CSS source facts are materiality gates, not generator input. JSON is the
exception: it returns through `emit_from_source`
(`skinny/crates/codegen/src/grammar_provider.rs:69`,
`skinny/crates/codegen/src/grammar_provider.rs:77`).

The frontend plug-in point is before provider rendering: convert the W5A fact
scan into a canonical frontend/import/IR lowering step inside
`grammar_provider.rs` or a named neutral successor, then have `lib.rs` consume
that IR through the existing request-owned path. SPEC names this ownership at
`restart/skinny/tranches/sk-v14/SPEC.md:714` and the task at
`restart/skinny/tranches/sk-v14/SPEC.md:733`.

`RuntimeProvider`, `GrammarProfile`, and `render_runtime_profile` remain live
and must stay live in W5B-FRONTEND. The registry still has eight provider
profiles, and `render_runtime_profile` branches across all seven CSS providers
plus JSON (`skinny/crates/codegen/src/grammar_profile.rs:17`,
`skinny/crates/codegen/src/grammar_profile.rs:100`,
`skinny/crates/codegen/src/lib.rs:180`).

`xtask` now routes CSS through the W5A request boundary. `regen.rs` reads source
inputs, builds expected files from the profile roster, and calls
`emit_runtime_from_request`; `regen_css.rs` supplies all 15 CSS L4 sources to
each of the seven CSS targets (`skinny/xtask/src/regen.rs:48`,
`skinny/xtask/src/regen.rs:65`, `skinny/xtask/src/regen_css.rs:5`).

## §2 — Recommendations (named falsifiability gates)

- Add the Lock 14 owner-path/parent-diff gate first.
- Implement a neutral frontend lowering layer at the request boundary.
- Require executable coverage proving `@ws`, `@pretty`, `?w`, `>>`, `<<`, span
  capture, typed projections, and imports lower into canonical IR without
  changing provider/template counts.
- Keep `RuntimeProvider`, `GrammarProfile`, `render_runtime_profile`, and all
  provider/template files intact. W5C-GEN owns provider-free generation; W5D
  owns deletion.

## §3 — Risks (REDRESS entries to pre-block)

- Replacing `render_runtime_profile` or provider dispatch would be W5C-GEN work,
  not W5B-FRONTEND work.
- Extending `parse_runtime_source_facts` without an actual frontend/IR lowering
  artifact would paper-close W5B by repeating the W5A fact-only shape.

## §4 — Sources (every external citation)

Local repository files only; no external sources used.
