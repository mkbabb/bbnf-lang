# SK-V12 W1b-1 Pre-CHALLENGE Adversarial Plan Review

Scope: plan-phase review only. No behavior/source edits. This review is bound
to SPEC Section 6 and the W1b-1 A1-A6 research cohort.

## Disposition

The smallest legal W1b-1 plan is admissible only as a scalar generated CSS L4
declaration-values scaffold. It can pass `G-W1b-1-CSS-L4-ORACLE` without
lightningcss, without SIMD, and without changing the main JSON result table,
but it must prove that Track 1 is generated, that the oracle is independent,
and that the companion gate consumes the provenance/size fields the SPEC
requires.

If the plan omits the owner-table fixes below, CHALLENGE should reject before
redress. The current SPEC owner list does not cover all files needed for a
legal scaffold.

## Smallest Legal Plan

1. Select exactly one row:
   `css_l4/declaration_values/direct_to_struct/main`.

2. Select exactly one output plane:
   `css_l4_declaration_value_fact_stream`.

3. Keep W1b-1 scalar-only. Even if W2 has passed, this wave does not need
   `bbnf-simd`, aarch64 helpers, ASM, or Lock 16 primitive admission. Any SIMD
   helper in W1b-1 expands the wave and should be rejected unless separately
   micro-proven, checkasm-covered, and same-wave consumed.

4. Create the owned fixture at
   `restart/skinny/tranches/sk-v12/research/w1b/css_l4_declaration_values.css`
   using the A5 three-rule declaration-values corpus. Do not use
   `normalize.css`, `bootstrap.css`, `tailwind.css`, `complex-errors.css`,
   Sheets, BBNF-self, JSON rows, or the stale REDRESS 111 fixture as the W1b-1
   row.

5. Amend the W1b-1 owner surface before redress. Required additions:

   - `skinny/crates/codegen/src/grammar_profile.rs`
   - a CSS-owned provider/template path such as
     `skinny/crates/codegen/src/css_l4_declaration_values_provider.rs`
   - a CSS-owned template directory if templates are split from the provider
   - `skinny/crates/runtime/src/lib.rs`
   - `skinny/xtask/src/main.rs` only if the W1b-1 report is routed through an
     xtask flag instead of the existing `bbnf-bench --bin gate` path

   The plan must also name any new oracle subdirectory under `bbnf-bench` if
   the oracle is not contained in the already-owned `nonjson_css_l4.rs` or
   `benches/nonjson_css_l4.rs`.

6. Add a data-driven codegen provider/profile, not a generic grammar-name
   branch. The generic selector may look up a provider by profile metadata; CSS
   policy must live in the CSS-owned provider/runtime files. A generic
   `if grammar_name == "css_l4"` or `match "css_l4"` branch carrying CSS
   behavior is a Lock 14 leak.

7. Prove generated Track 1 with a reproducibility test. The plan must require
   a codegen test that renders the CSS L4 declaration-values runtime to a temp
   directory and byte-compares it to the committed generated runtime, or an
   equivalent generation artifact consumed by the gate. A hand-written parser
   placed under `runtime/src/grammars/css_l4_declaration_values/` is not a
   generated Track 1.

8. Runtime shape should be minimal:

   - generated `mod.rs`, `config.rs`, and `generated.rs`
   - optional generated `parser.rs` only if the local split needs it
   - CSS-owned `sink.rs` / `host.rs` only for fact-stream emission and
     normalization

   Do not add CSS DOM/view/value traversal files for W1b-1. Do not reuse
   `JsonSink`, `JsonNodeKind`, generated JSON parser state, JSON scanner,
   JSON parse errors, or root-workspace CSS runtime types.

9. Track 1 and oracle must emit byte-identical canonical fact streams. The
   stream must include at least: schema version, input checksum/bytes,
   declaration ordinal/context, property, important flag, token facts, offsets,
   and final stream hash. Equality must retain both fact streams and a first
   diff artifact on failure; digest-only equality is not enough.

10. The oracle should be `cssparser`-backed inside `bbnf-bench`, independent
    from generated Track 1 and independent from the W1b-2 lightningcss
    comparator. The oracle may share nonsemantic utilities such as hex encoding
    or checksum helpers, but it must not share CSS token classification,
    declaration traversal, or generated runtime emitters with Track 1.

11. Extend only the companion non-JSON report row, not the main JSON
    `TelemetryRow` schema and not the outcome enum. Required consumed fields:

    - `strictness`
    - `grammar_checksum`
    - `input_checksum`
    - `input_bytes`
    - `measured_validation_path`
    - `profile_artifact`
    - `generated_loc`
    - `generated_module_bytes`
    - `grammar_size_guard`
    - `lock14_status`
    - `lock16_status`
    - `scalar_reference_status`
    - `checkasm_or_parity_status`

    For W1b-1, `outcome_id = C`, `verdict = GO`,
    `strict_output_equality = pass`,
    `track2_independence_status = independent_verified`,
    `same_wave_consumer_class = companion_gate_generated_baseline`,
    `lock16_status = not_applicable:scalar_only`, and
    `checkasm_or_parity_status = parity_pass`.

12. Record finite baseline Mbps for Track 1 and oracle with sample count
    `>= 30`. W1b-1 must not include `lightningcss_mbps`, a
    `> lightningcss_mbps + 1` verdict, or CSS ADMIT language. W1b-2 owns that
    gate.

13. Because report/gate/runtime exports move, require the JSON guard rerun from
    A6 unless the final diff proves no JSON-producing behavior and no
    report/gate path moved. If `report.rs`, `gate.rs`, runtime exports, generic
    codegen, generated JSON, or `RESULTS.md` changes, rerun the expanded JSON
    guard.

14. Rollback slice:
    revert CSS generated/runtime/bench/gate/report changes and save
    `/tmp/skv12-waveW1b-1-rejected.patch` if redress attempts a patch.

## Paper-Close / Rejection Routes

- **Generated-in-name-only:** committing a CSS parser under the generated
  runtime path without a codegen provider and regeneration proof.
- **Hand-only:** a `cssparser` oracle plus report, but no generated Track 1
  runtime compiled and benchmarked.
- **Parser-only:** parse success, declaration count equality, token count
  equality, pretty-printed CSS equality, or digest-only equality without the
  canonical fact stream artifacts.
- **Fixture substitution:** broad CSS corpora, error fixtures, JSON fixtures,
  Sheets, BBNF-self, or REDRESS 111 placeholders standing in for the selected
  CSS L4 row.
- **Wrong root:** full stylesheet admission or generic value admission instead
  of declaration-level `properties.bbnf::declaration` semantics.
- **Coupled oracle:** oracle calls generated Track 1, generated runtime
  internals, root `crates/core` CSS runtime, `runtime::generated_json`,
  `json_provider`, or lightningcss.
- **Grammar leak:** CSS policy in generic `codegen`, `runtime`, `tape`, `ir`,
  or `passes` roots instead of CSS-owned provider/runtime/bench files.
- **Owner escape:** editing `grammar_profile.rs`, `runtime/src/lib.rs`, a new
  CSS provider/template path, or `xtask/src/main.rs` without first amending the
  W1b-1 owner table and passing CHALLENGE.
- **Telemetry under-consumption:** emitting generated size, checksums, Lock 14,
  Lock 16, scalar/parity status, or validation paths without the companion gate
  rejecting bad values in the same wave.
- **Admission overclaim:** adding lightningcss placeholders, CSS ADMIT status,
  a new outcome variant, or main `RESULTS.md` CSS SOTA claims in W1b-1.
- **SIMD expansion:** touching `bbnf-simd`, aarch64 modules, or ASM helpers in
  a scaffold wave that is supposed to stay scalar-only.
- **JSON guard skip:** changing report/gate/runtime exports and recording
  `json_guard_state=not_refreshed:no_behavior_drift` without the A6 proof.

## Exact Fixes The Plan Must Include

The plan should be revised to contain these explicit clauses before CHALLENGE:

1. **Owner amendment clause:** list the owner additions from this review and
   state that redress cannot touch them until the plan/SPEC owner surface is
   amended.

2. **Generated proof clause:** require a codegen reproducibility test for the
   CSS runtime output and reject manually authored generated files.

3. **Fixture clause:** create the A5 fixture at the owned W1b path and record
   SHA-256, byte length, and grammar input checksums in the companion report.

4. **Fact-stream clause:** name the canonical fact schema and require retained
   Track 1/oracle fact artifacts plus first-diff output on failure.

5. **Oracle independence clause:** use `cssparser` only for the independent
   oracle; forbid root CSS runtime, lightningcss, generated runtime internals,
   and JSON runtime/codegen coupling.

6. **Gate-consumption clause:** extend `SkV12NonJsonRow` validation to reject
   missing or bad provenance, size, Lock 14/16, scalar reference, and parity
   fields.

7. **Outcome clause:** W1b-1 may record `C/GO` scaffold/equality, never CSS
   ADMIT. W1b-2 remains the first lightningcss comparator/admission wave.

8. **Guard clause:** run the expanded JSON guard if report/gate/runtime exports
   or generic codegen move; otherwise record the exact no-touch proof commands.

9. **Rollback clause:** name `/tmp/skv12-waveW1b-1-rejected.patch` and the
   files to revert on BLOCKED/FAIL.

## Minimum Redress Evidence The Accepted Plan Must Demand

```sh
cargo test -p codegen css_l4_declaration_values_profile_fields_are_consumed -- --nocapture
cargo test -p codegen css_l4_declaration_values_generated_runtime_reproducible -- --nocapture
cargo test -p runtime css_l4_declaration_values -- --nocapture
cargo test -p bbnf-bench nonjson_css_l4 -- --nocapture

CARGO_TARGET_DIR=/tmp/skv12-w1b1-target \
CRITERION_HOME=/tmp/skv12-w1b1-css-l4-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo bench -p bbnf-bench --bench nonjson_css_l4 -- css_l4/declaration_values/direct_to_struct/main

cargo run -p bbnf-bench --bin gate -- \
  --skv12-non-json-report ../restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-1-css-l4-oracle.json
```

If report/gate/runtime exports or generic codegen move, also require:

```sh
CARGO_TARGET_DIR=/tmp/skv12-w1b-1-json-guard-target \
CRITERION_HOME=/tmp/skv12-w1b-1-json-guard-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo run --manifest-path skinny/Cargo.toml -p xtask -- bench-json --advisory

CRITERION_HOME=/tmp/skv12-w1b-1-json-guard-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo run --manifest-path skinny/Cargo.toml -p xtask -- gate-json --advisory --check-results

CRITERION_HOME=/tmp/skv12-w1b-1-json-guard-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo run --manifest-path skinny/Cargo.toml -p xtask -- gate-json --with-cost-facts --advisory --check-results

awk -f restart/skinny/tranches/sk-v12/research/w1a/verify-skv12-json-floors.awk skinny/RESULTS.md
```

## CHALLENGE Questions

CHALLENGE should reject unless the plan answers these with concrete file paths
and commands:

1. What proves the CSS runtime was generated by codegen rather than hand-coded?
2. Where are CSS policy and fact-stream semantics isolated from generic roots?
3. What exact artifact preserves Track 1 facts, oracle facts, and first diff?
4. What gate code rejects bad generated LOC/module bytes, checksums, Lock
   status, and parity status?
5. What prevents W1b-1 from claiming lightningcss admission or mutating the
   main JSON result table?
6. What exact JSON guard path runs after report/gate/runtime export changes?
