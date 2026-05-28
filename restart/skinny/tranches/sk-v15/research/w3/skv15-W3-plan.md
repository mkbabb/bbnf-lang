# SK-V15 Wave W3 Plan: Codegen Leak Abrogation

Inputs:

- `restart/skinny/tranches/sk-v15/research/w3/skv15-W3-A-profile-roster.md`
- `restart/skinny/tranches/sk-v15/research/w3/skv15-W3-B-runtime-generator.md`
- `restart/skinny/tranches/sk-v15/research/w3/skv15-W3-C-provider-request.md`
- `restart/skinny/tranches/sk-v15/research/w3/skv15-W3-D-xtask-consumers.md`
- `restart/skinny/tranches/sk-v15/research/w3/skv15-W3-E-passes-exclusion.md`
- `restart/skinny/tranches/sk-v15/research/w3/skv15-W3-F-authority-and-verification.md`
- `restart/skinny/tranches/sk-v15/SPEC.md` W3 row and
  `DEP-W3-W6-CSS-PROVIDER-TEMPLATE`
- `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md` W3 envelope

Intervention: remove the static runtime profile/mode/config leak family from
generic codegen by making runtime profile contracts request-carried metadata.
The profile contract will carry emitter kind, expected generated files,
frontend materiality requirements, and output labels. `grammar_provider.rs` and
`runtime_generator.rs` will consume that contract instead of selecting a
hardcoded JSON/CSS profile and instead of matching seven CSS profile IDs for
`fact_schema`, `row_id`, and `output_plane`.

## Owner Paths

- `skinny/crates/codegen/src/grammar_profile.rs`
- `skinny/crates/codegen/src/grammar_provider.rs`
- `skinny/crates/codegen/src/runtime_generator.rs`
- `skinny/crates/codegen/src/lib.rs`
- `skinny/xtask/src/regen.rs`
- `skinny/xtask/src/regen_css.rs`
- `skinny/xtask/src/main.rs`
- `restart/skinny/tranches/sk-v15/research/w3/skv15-W3-challenge-v1.md`
- `restart/skinny/tranches/sk-v15/research/w3/skv15-W3-redress.md`

No generated CSS runtime files, root Pattern H files, CSS typed provider,
Decision Engine, or old CSS parser/fact-stream/full-parse proof are owned by
W3. Existing dirty generated CSS and real-typed files remain preserved.

## Implementation Shape

1. Replace `RuntimeGenerationMode::{PassCompiled, FrontendFacts}` and the
   static `runtime_profiles()` roster with generic file-roster helpers and a
   `RuntimeProfileContract` carried by `RuntimeGenerationRequest`.
2. Add generic contract structs for emitter kind, frontend requirement booleans,
   and optional output labels. The CSS xtask target rows provide labels as
   data; `runtime_generator.rs` no longer has a CSS profile-name match.
3. Convert `validate_non_json_frontend_materiality()` into contract-driven
   frontend requirement validation. Requirement failures cite missing fact
   names, not JSON/CSS family status.
4. Keep compiled JSON emission available through the source-compiled path, but
   remove static profile lookup from `emit_from_source()` and typed direct
   generation. This avoids a generic branch on `json` while preserving current
   JSON output.
5. Update tests to build CSS requests from profile contracts and add a W3
   regression proving a non-CSS frontend profile can be validated by metadata
   without adding a `css_l4` or `json` match arm.

## Dependency Rows And Blocks

Consumed row: `DEP-W3-W6-CSS-PROVIDER-TEMPLATE`.

W3 neutralizes CSS provider/template/static profile roster fanout. It does not
delete `CSS_GENERATED_RS`, `CssFullParseSummary`, fact-stream-only `parse()`,
`parse_full`, generated CSS bodies, or CSS provider/template old proof. Those
remain W5/W6 work under `DEP-W6-CSS-GENERATED-RS` and
`DEP-W6-CSS-SUMMARY-FACT-STREAM`.

SPEC asks `runtime_generator.rs` for CSS plus Sheets, BBNF-self, CSV, or math
receiver proof. W3 will add an executable synthetic non-CSS frontend contract
test for provider/generator metadata generality. If that cannot prove through
real codegen without a paper receiver, redress records a row-level intrinsic
block for the missing named receiver instead of weakening the gate.

## Falsifiability Gate

- `rg -n "RuntimeGenerationMode|PassCompiled|FrontendFacts|css_profile_config|CssProfileConfig|runtime_profiles|CSS_L4_|JSON_PROFILE" skinny/crates/codegen/src` has no live static profile/mode/config hits.
- `runtime_generator.rs` has no match on `css_l4_*` profile IDs for output
  labels.
- The seven CSS request-generated codegen tests still reproduce current output
  or record pre-existing dirty generated-output blockers without editing those
  files.
- JSON generated runtime check passes if JSON-adjacent compiled generation
  changed.
- `gate-json --check-results` remains accepted on native Apple M5 Max/aarch64.
- Unrelated dirty root runtime/generated files remain unstaged and unmodified
  by W3.

## Required Commands

```sh
cargo fmt --manifest-path skinny/Cargo.toml --all --check
RUSTFLAGS="-C target-cpu=native" cargo test --profile ax-iter -p codegen
RUSTFLAGS="-C target-cpu=native" cargo run --profile ax-iter -p xtask -- check-json
RUSTFLAGS="-C target-cpu=native" cargo run --profile ax-iter -p xtask -- gate-json --check-results
rg -n "RuntimeGenerationMode|PassCompiled|FrontendFacts|css_profile_config|CssProfileConfig|runtime_profiles|CSS_L4_|JSON_PROFILE" skinny/crates/codegen/src
grep -cE "^[0-9]+\\. \\*\\*" restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
git diff --check
```

CSS `check-css-l4-*` and `check-real-typed` are expected to fail before W3
because their generated files are already dirty. They may be rerun as negative
evidence, but W3 must not patch those generated outputs.

## Budget And Challenge Trigger

SPEC W3 envelope: risk High, manual source/test LOC 150-320, generated
regen/check evidence, docs LOC 80-180, entry gate W2 admitted, exit gate one
coherent generic leak family removed with same-wave generator consumer.
Redress hard cap: 30 minutes.

W3 is a mandatory seven-lens CHALLENGE candidate because it changes generic
generation/provider behavior. If CHALLENGE finds that request-carried contracts
merely move CSS branches into xtask without generator consumption, or that the
non-CSS receiver is paper-only, redress rejects or records intrinsic block.

## Revert Protocol

Revert only the W3 owner paths. Do not restore, rewrite, or stage unrelated
dirty generated CSS, real-typed, root runtime, old research JSON, or root xtask
files. If the profile contract conversion exceeds the LOC budget or forces CSS
old-proof deletion, abort W3 redress and route through REDRESS/Omega rather than
widening scope.
