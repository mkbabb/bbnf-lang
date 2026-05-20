# SK-V12 W1a A4: Regeneration And JSON Parity

Date: 2026-05-20.
Scope: generated output, parity, and regeneration mechanics for JSON modules under the W1a `GrammarConfig` / Lock 14 legality wave.
Output: `restart/skinny/tranches/sk-v12/research/w1a/A4-regen-json-parity.md`.

## §1 — Findings

1. W1a is a legality wave, not a CSS performance row.

   SPEC Section 4 says W1a's purpose is to make CSS L4 emission legal before any CSS generated parser is emitted, with owner paths in `skinny/crates/codegen/src/`, `skinny/crates/runtime/src/`, optional `skinny/crates/ir/src/` generated metadata types, generated JSON modules only as regen output, and `skinny/RESULTS.md` / `skinny/REDRESS.md` / gate-report scripts as needed (`restart/skinny/tranches/sk-v12/SPEC.md:314`, `:318`). Its exit gate requires generic-crate scan pass, JSON generated parity and guard floors, no CSS parser row claim, and no new directive/BIR/BackendShape/public substrate API (`restart/skinny/tranches/sk-v12/SPEC.md:341`). The pass contract keeps this phase read-only research with no source edits outside the research tree (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:11`, `:194`).

2. The current skinny regen path is JSON-only and emits a fixed runtime file set.

   `skinny/xtask/src/main.rs` reads `skinny/grammars/json.bbnf`, calls `codegen::emit_from_source("json", ...)`, and writes or checks `skinny/crates/runtime/src/grammars/json` (`skinny/xtask/src/main.rs:121`, `:128`). `codegen::emit_with_layout` gates through the JSON runtime profile, appends `sink_direct::render(...)` to the generated parser body, and inserts exactly these runtime files: `generated.rs`, `host.rs`, `mod.rs`, `parser.rs`, `scan.rs`, `sink.rs`, `value.rs`, `view.rs`, `visitor.rs` (`skinny/crates/codegen/src/lib.rs:102`, `:127`). Codegen tests assert the file set, deterministic emission, and the `parse_direct` sink-only body (`skinny/crates/codegen/src/lib.rs:252`, `:273`, `:285`).

3. `check-json` is byte-for-byte for expected files, but it is not a complete directory ownership check.

   `EmittedSource::check_dir` iterates expected files, reads each checked-in file, and fails on missing or different content (`skinny/crates/codegen/src/lib.rs:55`). It does not reject extra stale files in the output directory. If W1a adds a generated metadata file such as `config.rs`, the file set test and a stale-file deletion check should be updated, or old generated files can survive a rename/removal.

4. Two current "generated" JSON files are also used as template inputs.

   `json_provider::scan_rs()` and `json_provider::sink_rs()` use `include_str!` from `skinny/crates/runtime/src/grammars/json/scan.rs` and `skinny/crates/runtime/src/grammars/json/sink.rs`, not from `json_templates/` (`skinny/crates/codegen/src/json_provider.rs:56`, `:60`). Those files carry the generated header (`skinny/crates/runtime/src/grammars/json/scan.rs:1`, `skinny/crates/runtime/src/grammars/json/sink.rs:1`). This means a hand edit to scan/sink can become the expected regen output on the next build. W1a should either move them to template/source ownership or explicitly document them as source templates, because `check-json` alone cannot prove they are regenerated from an independent template.

5. Typed JSON output has a separate regen/check path and must be kept in the W1a guard set.

   `regen-real-typed` and `check-real-typed` read the same JSON grammar plus `xtask/src/real_typed_schema.rs`, then write/check `skinny/crates/bbnf-bench/src/generated_real_typed.rs` (`skinny/xtask/src/main.rs:136`, `:144`). The typed renderer emits an owned generated module header and schema hash (`skinny/crates/codegen/src/typed_direct.rs:13`), and codegen tests require typed direct output to avoid `JsonSink` and `serde_json::Value` (`skinny/crates/codegen/src/lib.rs:331`). W1a's JSON typed guard floors depend on this generated file even though it lives in the bench crate.

6. Current generated JSON output size is below the existing generated-runtime LOC ceiling.

   Current generated/runtime file line counts are: `generated.rs` 837, `host.rs` 3, `mod.rs` 20, `parser.rs` 69, `scan.rs` 276, `sink.rs` 119, `value.rs` 172, `view.rs` 459, `visitor.rs` 39; the runtime JSON directory totals 1,994 lines. `generated_real_typed.rs` is 1,846 lines. `lint-loc` reports the runtime JSON generated directory against a 4,000 LOC ceiling and the hand Track 2 JSON parser against 500 LOC (`skinny/xtask/src/main.rs:183`, `:194`). SPEC requires generated LOC, module bytes, grammar source size, and O(N) growth status for generated-size tracking (`restart/skinny/tranches/sk-v12/SPEC.md:259`, `:273`).

7. JSON parity is proven at three levels before benches run.

   The JSON parity bench calls parse/tape parity, direct digest parity, and typed parity before measuring each fixture (`skinny/crates/bbnf-bench/benches/json_parity.rs:17`). `parity::assert_parity` compares generated Track 1 against hand Track 2 offsets, flags, payload counters, and canonical serialization (`skinny/crates/bbnf-bench/src/parity.rs:23`). `assert_direct_struct_parity` compares generated `parse_direct`, hand Track 2, serde, and sonic-rs digest shape (`skinny/crates/bbnf-bench/src/direct_struct.rs:403`, `:420`). `assert_real_typed_parity` compares generated typed output, Track 2, serde, and sonic checksums (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:360`, `:449`).

8. `gate-json` consumes parity, Criterion metadata, report schema, and exact `RESULTS.md` rendering.

   The gate reads Criterion estimates and metadata for parse, direct, typed, sonic, serde, and SIMD rows (`skinny/crates/bbnf-bench/src/bin/gate.rs:107`). It validates schema and W0 telemetry before rendering (`skinny/crates/bbnf-bench/src/bin/gate.rs:370`), and if not run with `--update-results` / `--write-results`, it byte-compares rendered markdown to `skinny/RESULTS.md` and exits invalid on staleness (`skinny/crates/bbnf-bench/src/bin/gate.rs:384`). `bench-json` runs the full bench and then invokes `gate-json --update-results`, optionally advisory (`skinny/xtask/src/main.rs:206`).

9. The current executable guard-floor checks are not identical to the SK-V12 W1a guard table.

   SPEC Section 0.5 names four direct guard rows with Track 1 and Track 2 floors and seven typed guard rows with Track 1 and Track 2/oracle floors (`restart/skinny/tranches/sk-v12/SPEC.md:187`, `:196`). Current report validation enforces direct floors only when an `N-direct` row moves to `A / GO`, using `sk_v10_direct_floor` (`skinny/crates/bbnf-bench/src/report.rs:1162`, `:1378`), and enforces existing typed maintain floors only on Track 1 through `validate_existing_typed_maintain_floors` (`skinny/crates/bbnf-bench/src/report.rs:1342`, `:1366`). The SK-V12 floor numbers are present in SPEC/SYNTHESIS and alpha research, but not as the exact W1a table in code. W1a redress should either add an explicit SK-V12 guard-floor consumer or include a mechanical post-run floor check in the plan; otherwise `--update-results` can refresh a below-SPEC Mbps row without a dedicated floor failure.

10. `gate-json` currently runs a Lock 14 freeze before normal report work.

    The bench gate calls `lock14_baseline::validate(&workspace)` before companion reports or JSON result processing (`skinny/crates/bbnf-bench/src/bin/gate.rs:38`). That validator checks allowlist entries, frozen-root dirty state, parent diff authorization, and the five-variant BackendShape surface (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:342`). The frozen roots include grammar, runtime, IR, passes, codegen, SIMD, parse-that-regex, direct/typed/parity/track2 bench paths, and real typed schema (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:383`). Current parent-diff exceptions are older `sk-v8` / `sk-v10` scopes (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:480`). Since W1a intentionally edits generic/template/config paths, the final W1a plan must update or replace this W0 freeze with a W1a-aware Lock 14 scan/gate; otherwise `gate-json` can fail with "Lock 14 frozen roots are dirty" before JSON guard evidence is evaluated.

11. The SK-V12 non-JSON pass fixture is schema/gate-only and rejects producer-only shortcuts.

    `restart/skinny/tranches/sk-v12/research/skv12-W0-nonjson-pass.json` uses schema `sk-v12-nonjson-generated-v1`, wave `SK-V12-W0`, a non-JSON `css_l4` row, generated source/runtime provenance, an independent oracle, strict equality, and `json_guard_state = not_refreshed:no_behavior_drift`. `SkV12NonJsonReport` denies unknown fields (`skinny/crates/bbnf-bench/src/report.rs:161`, `:170`), rejects JSON grammar rows, mismatched generated provenance, coupled oracle paths, sample count below 30, `gate_only` consumers, and weak measurement context (`skinny/crates/bbnf-bench/src/report.rs:1849`, `:1930`, `:1948`, `:1971`, `:2007`). Unit tests cover the accepting fixture and required rejection classes (`skinny/crates/bbnf-bench/src/report.rs:2659`, `:2664`, `:2684`).

## §2 — Recommendations

Named gates for the W1a plan/redress:

1. `G-W1a-REGEN-JSON-BYTE-CLEAN`: run both JSON regen checks from the skinny workspace after redress:

   ```sh
   cd /Users/mkbabb/Programming/bbnf-lang/skinny
   cargo run -p xtask -- check-json
   cargo run -p xtask -- check-real-typed
   ```

   If either fails, regenerate the owned outputs:

   ```sh
   cd /Users/mkbabb/Programming/bbnf-lang/skinny
   cargo run -p xtask -- regen-json
   cargo run -p xtask -- regen-real-typed
   cargo run -p xtask -- check-json
   cargo run -p xtask -- check-real-typed
   ```

2. `G-W1a-CODEGEN-EMISSION-SHAPE`: run codegen unit coverage for deterministic file ownership and direct/typed renderer contracts:

   ```sh
   cd /Users/mkbabb/Programming/bbnf-lang/skinny
   cargo test -p codegen
   ```

3. `G-W1a-LOCK14-SCAN-CONSUMED`: after the W1a Lock 14 gate has been made W1a-aware, run the bench-gate tests that cover Lock 14 and SK-V12 report schema:

   ```sh
   cd /Users/mkbabb/Programming/bbnf-lang/skinny
   cargo test -p bbnf-bench lock14_baseline
   cargo test -p bbnf-bench skv12_non_json_report
   ```

4. `G-W1a-JSON-PARITY-UNIT`: run the unit-level JSON parity/guard contract tests:

   ```sh
   cd /Users/mkbabb/Programming/bbnf-lang/skinny
   cargo test -p bbnf-bench direct_contract
   cargo test -p bbnf-bench w6_typed_contract
   cargo test -p bbnf-bench generated_
   cargo test -p bbnf-bench parity
   ```

5. `G-W1a-JSON-GUARD-RUN`: if W1a touches any generic runtime, codegen, generated output, benchmark, report, gate, parser, scanner, SIMD, or JSON-producing path, refresh the native JSON guard run and then prove `RESULTS.md` exactness:

   ```sh
   cd /Users/mkbabb/Programming/bbnf-lang/skinny
   CARGO_TARGET_DIR=/tmp/skv12-w1a-json-guard RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- bench-json --advisory
   CARGO_TARGET_DIR=/tmp/skv12-w1a-json-guard RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --advisory --check-results
   ```

   If the W1a plan proves no JSON-producing path moved, record the no-touch proof and still run the cheap byte checks:

   ```sh
   cd /Users/mkbabb/Programming/bbnf-lang/skinny
   cargo run -p xtask -- check-json
   cargo run -p xtask -- check-real-typed
   cargo run -p xtask -- gate-json --skv12-non-json-report ../restart/skinny/tranches/sk-v12/research/skv12-W0-nonjson-pass.json
   ```

6. `G-W1a-SPEC-FLOOR-EXACT`: add or run an explicit check for the SPEC Section 0.5 floors:

   Direct rows:
   - `citm_catalog/direct_to_struct`: Track 1 >= 18191, Track 2 >= 17431.
   - `apache_builds/direct_to_struct`: Track 1 >= 11028, Track 2 >= 9996.
   - `marine_ik/direct_to_struct`: Track 1 >= 8759, Track 2 >= 9248.
   - `unicode_basic/direct_to_struct`: Track 1 >= 2253, Track 2 >= 2182.

   Typed rows:
   - `twitter/real_typed_struct`: Track 1 >= 17385, Track 2/oracle >= 15593.
   - `citm_catalog/real_typed_struct`: Track 1 >= 29928, Track 2/oracle >= 17321.
   - `apache_builds/real_typed_struct`: Track 1 >= 8308, Track 2/oracle >= 6754.
   - `github_events/real_typed_struct`: Track 1 >= 11633, Track 2/oracle >= 12029.
   - `update_center/real_typed_struct`: Track 1 >= 11613, Track 2/oracle >= 10150.
   - `mesh/real_typed_struct`: Track 1 >= 9214, Track 2/oracle >= 7739.
   - `marine_ik/real_typed_struct`: Track 1 >= 11552, Track 2/oracle >= 9894.

## §3 — Source / Generated Path Ownership

Current source inputs:

- `skinny/grammars/json.bbnf`: JSON grammar input for both runtime JSON and real typed regen.
- `skinny/crates/codegen/src/lib.rs`: emission orchestration and generated file list.
- `skinny/crates/codegen/src/json_provider.rs`: JSON provider and runtime/template source selection.
- `skinny/crates/codegen/src/json_templates/generated.rs`, `parser.rs`, `value.rs`, `view.rs`, `visitor.rs`: source templates.
- `skinny/crates/runtime/src/grammars/json/scan.rs` and `sink.rs`: currently marked generated but also used as provider template inputs; W1a should resolve this ownership ambiguity.
- `skinny/crates/codegen/src/sink_direct.rs`: source for the appended `parse_direct` body in runtime `generated.rs`.
- `skinny/crates/codegen/src/direct_schema.rs`, `typed_direct.rs`, `xtask/src/real_typed_schema.rs`: source for generated typed DirectBuild output.
- `skinny/crates/bbnf-bench/src/report.rs`, `gate.rs`, `src/bin/gate.rs`, `metadata.rs`, `lock14_baseline.rs`, and `benches/json_parity.rs`: telemetry/gate/parity source, not generated output.

Current generated outputs likely to change if W1a moves JSON policy into generated metadata/templates:

- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/runtime/src/grammars/json/host.rs`
- `skinny/crates/runtime/src/grammars/json/mod.rs`
- `skinny/crates/runtime/src/grammars/json/parser.rs`
- `skinny/crates/runtime/src/grammars/json/scan.rs`
- `skinny/crates/runtime/src/grammars/json/sink.rs`
- `skinny/crates/runtime/src/grammars/json/value.rs`
- `skinny/crates/runtime/src/grammars/json/view.rs`
- `skinny/crates/runtime/src/grammars/json/visitor.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- likely new JSON metadata/config output, if W1a implements `GrammarConfig`, for example `skinny/crates/runtime/src/grammars/json/config.rs` or an equivalent generated metadata module.

## §4 — Risks And Pitfalls

1. `check-json` can pass while extra stale generated files remain. Add a stale-file check if W1a changes the generated file roster.

2. `scan.rs` and `sink.rs` are both generated outputs and template inputs today. That weakens regen parity as a provenance proof for those files.

3. Current `gate-json` Lock 14 validation is a W0 freeze. W1a must land a W1a-aware Lock 14 scan/gate path before using `gate-json` as final evidence after generic/template/config edits.

4. The current executable guard-floor logic does not exactly enforce the SK-V12 Section 0.5 direct and typed floor table. Either add that table to gate/report validation or run a separate mechanical floor check after native guard refresh.

5. `gate-json --update-results` rewrites `skinny/RESULTS.md`; `gate-json --check-results` / no-update mode proves exactness against the checked-in report. Do not treat a fresh update as proof unless the same Criterion root is then checked without update.

6. Criterion metadata must be same-capture and native: `RUSTFLAGS="-C target-cpu=native"` and `target_cpu=native` are validated by the gate. Stale local `target/criterion` data can fail the gate independently of source correctness, so use an isolated `CARGO_TARGET_DIR` for W1a guard runs.

7. The installed repo pre-commit hook is root-workspace regen, not skinny regen. It runs `cargo xtask regen --check --staged` from the repository root (`.git/hooks/pre-commit:22`), while CI runs root `cargo xtask regen --check` (`.github/workflows/ci.yml:58`). This will not replace skinny `check-json` / `check-real-typed`; it can also fail on unrelated root grammar/generated staging. Keep the W1a slice isolated and run skinny commands explicitly from `/Users/mkbabb/Programming/bbnf-lang/skinny`.

8. W1a revert protocol includes generated output. If W1a fails, revert generic/template/config changes and generated JSON/typed output together, and save the rejected patch at `/tmp/skv12-waveW1a-rejected.patch` per SPEC (`restart/skinny/tranches/sk-v12/SPEC.md:348`).

## §5 — Sources

- `restart/skinny/tranches/sk-v12/SPEC.md`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/skinny/tranches/sk-v12/research/skv12-W0-nonjson-pass.json`
- `restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md`
- `skinny/xtask/src/main.rs`
- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/codegen/src/json_provider.rs`
- `skinny/crates/codegen/src/sink_direct.rs`
- `skinny/crates/codegen/src/typed_direct.rs`
- `skinny/crates/runtime/src/grammars/json/`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- `skinny/crates/bbnf-bench/src/parity.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `.git/hooks/pre-commit`
- `.github/workflows/ci.yml`
