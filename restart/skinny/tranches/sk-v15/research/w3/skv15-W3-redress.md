# SK-V15 Wave W3 Redress: Codegen Leak Abrogation

Status: redress applied at HEAD.

## Scope

W3 neutralized the static runtime profile/mode/config leak family in skinny
codegen. It did not touch generated CSS runtime files, root Pattern H runtime
files, benchmark ledgers, Decision Engine, or CSS old proof bodies.

Owner paths changed:

- `skinny/crates/codegen/src/grammar_profile.rs`
- `skinny/crates/codegen/src/grammar_provider.rs`
- `skinny/crates/codegen/src/runtime_generator.rs`
- `skinny/crates/codegen/src/lib.rs`
- `skinny/xtask/src/main.rs`
- `skinny/xtask/src/regen.rs`
- `skinny/xtask/src/regen_css.rs`

Manual source/test diff across W3 owner paths: 354 inserted / 361 removed
lines. This stays inside the W3 150-320 net manual movement envelope.

## Implementation

- Removed `RuntimeGenerationMode::{PassCompiled, FrontendFacts}` and the
  static `runtime_profiles()` table from generic codegen.
- Replaced static profile lookup with `RuntimeProfileContract`, carried by
  `RuntimeGenerationRequest`.
- The contract now carries:
  - emitter kind (`CompiledLowering` or `RequestFacts`);
  - expected generated file roster;
  - frontend materiality requirements;
  - optional output labels (`fact_schema`, `row_id`, `output_plane`).
- `runtime_generator.rs` consumes output labels from the request contract
  instead of matching seven CSS profile IDs in `css_profile_config()`.
- `grammar_provider.rs` validates frontend materiality from contract flags
  instead of classifying requests as non-JSON by profile mode.
- `xtask` target rows now provide file roster, emitter kind, frontend
  requirements, and output labels as target data.

## Verification

Passed:

```sh
cargo fmt --manifest-path skinny/Cargo.toml --all --check
RUSTFLAGS="-C target-cpu=native" cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p codegen tests::css_l4_frontend_profiles_are_request_generated -- --exact
RUSTFLAGS="-C target-cpu=native" cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p codegen w5c_gen_css_runtime_output_depends_on_frontend_source_hash -- --exact
(cd skinny && RUSTFLAGS="-C target-cpu=native" cargo run --profile ax-iter -p xtask -- check-json)
(cd skinny && RUSTFLAGS="-C target-cpu=native" cargo run --profile ax-iter -p xtask -- gate-json --check-results)
rg -n "RuntimeGenerationMode|PassCompiled|FrontendFacts|css_profile_config|CssProfileConfig|runtime_profiles|CSS_L4_|JSON_PROFILE" skinny/crates/codegen/src
grep -cE "^[0-9]+\\. \\*\\*" restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
git diff --check
```

Invariant outputs:

- Lock count: `16`.
- Pattern H file count: `67`.
- Leak grep: no matches.

Blocked outside W3:

```sh
RUSTFLAGS="-C target-cpu=native" cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p codegen
(cd skinny && RUSTFLAGS="-C target-cpu=native" cargo run --profile ax-iter -p xtask -- check-css-l4-declaration-values)
```

The full codegen suite has one failure:
`tests::css_l4_generated_runtimes_reproducible_from_request` fails with
`DifferentFile("generated.rs")`. The CSS xtask check fails the same way. This
matches the pre-existing dirty `skinny/crates/runtime/src/grammars/css_l4_*/generated.rs`
state recorded by W3-D; W3 does not own generated CSS runtime repair.

Also blocked:

```sh
RUSTFLAGS="-C target-cpu=native" cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p codegen tests::w3_request_contract_consumes_bbnf_self_metadata_without_profile_branch -- --exact
RUSTFLAGS="-C target-cpu=native" cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p codegen tests::w3_request_contract_consumes_csv_metadata_without_profile_branch -- --exact
RUSTFLAGS="-C target-cpu=native" cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p codegen tests::w3_request_contract_consumes_math_metadata_without_profile_branch -- --exact
```

Each actual non-CSS receiver attempt moved into a hanging test path and had to
be killed. Per the W3 CHALLENGE guardrail, W3 records the named non-CSS
runtime-generator receiver proof as intrinsic-blocked rather than replacing it
with a toy source fixture.

## Disposition

W3 admits the codegen leak abrogation owner paths: generic codegen no longer
has the static runtime profile/mode/config branch family. `DEP-W3-W6-CSS-PROVIDER-TEMPLATE`
is neutralized but not deleted. CSS generated-output repair remains W5/W6
blocked, and the named non-CSS receiver proof remains an intrinsic block for
W11 dependency close accounting.
