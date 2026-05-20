# SK-V11 W1a R3: Fixtures And Tests Research

Date: 2026-05-20.
Scope: read-only research for `G-W1a-NONJSON-GATE` fixture and test surfaces.
Output: this file.
Source edit policy: no source edits; recommendations only.

## Findings

1. W1a is a gate/report schema lane, not a behavior lane. SPEC Section 4 names
   `skinny/crates/bbnf-bench/src/bin/gate.rs`,
   `skinny/crates/bbnf-bench/src/report.rs`,
   `skinny/crates/bbnf-bench/src/metadata.rs`,
   `skinny/crates/bbnf-bench/benches/`, and this `research/w1a/` directory as
   owner paths, with no parser row movement. The required fixture set is
   explicit: grammar id, domain, output plane, comparator/oracle, Track 2/oracle
   independence, run id, host, feature mask, same-wave consumer class, and
   producer-only telemetry rejection (`SPEC.md:284-318`).

2. The inherited schema surface is already two-layered. `Report` renders a
   26-column schema-v3 main table and a folded SK-V9 W0 telemetry manifest
   (`report.rs:8-91`, `report.rs:620-653`). `validate_schema_v3` consumes the
   main-table fields and required throughput/comparator cells (`report.rs:220-273`,
   `report.rs:499-507`). `validate_sk_v8_w0` consumes the manifest fields,
   duplicate/unknown row ids, row count, uniform run id, row identity, and typed
   guard maintenance (`report.rs:276-385`, `report.rs:509-575`).

3. The live gate is JSON-only at the manifest layer. `validate_sk_v8_w0` rejects
   any `grammar_id != json` or `domain != json_bench` (`report.rs:327-331`);
   `w0_telemetry` constructs row ids as `json/{corpus}/{workload}/main` and
   emits `grammar_id=json`, `domain=json_bench`, `same_wave_consumer_class=gate_only`,
   and `track2_independence_status=independent_verified` (`bin/gate.rs:444-529`).
   This is the right fail-closed starting point: W1a should add a separate
   non-JSON validator or companion-report validator, not relax W0 JSON rules in
   place.

4. Existing focused tests are builder-driven, not file-backed fixtures.
   `report.rs` has in-module test builders for schema-v3, W0 manifest,
   comparator evidence, row identity, outcome, strict/deferred validation,
   direct-contract, and W6 typed-contract cases (`report.rs:2016-2553`).
   `gate.rs` has strict-admission refusal tests for non-native comparators,
   unsupported outcomes, non-GO outcomes, deferred/view-boundary claims, plane
   mismatch, stale sidecars, and sidecar-same-run without a structured manifest
   (`gate.rs:136-182`, `gate.rs:459-543`). `bin/gate.rs` has metadata-capture
   tests for coherent required rows, fixture hash/byte mismatch, mixed capture,
   missing required bench, and SIMD metadata/hash mismatch (`bin/gate.rs:2192-2253`).

5. Existing bench fixture surfaces are JSON-only. `test-fixtures` owns 17
   canonical JSON perf fixtures plus embedded valid/invalid mini fixtures
   (`test-fixtures/src/lib.rs:7-44`, `manifest.toml:1-101`). `json_parity.rs`
   loads only `load_available_bench_fixtures`, writes Criterion metadata for
   JSON parse/direct/typed workloads, and writes per-bench `metadata.toml`
   files (`json_parity.rs:10-39`, `json_parity.rs:490-514`). `simd_scan.rs`
   writes SIMD scan metadata for the same JSON fixture set (`simd_scan.rs:9-36`,
   `simd_scan.rs:44-81`). There is no file-backed non-JSON benchmark fixture
   harness under `skinny/crates/bbnf-bench/benches/` today.

6. Existing RESULTS checks are partly split. The normal `gate-json` path shells
   to `bbnf-bench --bin gate` and compares rendered markdown to `RESULTS.md`
   unless `--update-results` is passed (`xtask/src/main.rs:242-275`,
   `bin/gate.rs:349-365`). The `--with-cost-facts` path diverts to a CostFacts
   JSON report, validates a `RESULTS.md` snapshot only when `--check-results`
   is present, and then validates the CostFacts manifest (`xtask/src/main.rs:298-386`,
   `xtask/src/main.rs:389-535`). Tests cover missing CostFacts manifest fields
   and illegal flags (`xtask/src/main.rs:646-696`).

7. Skinny has non-JSON grammar/proof hints but not a W1a-ready benchmark lane.
   The root workspace lists `css_l4`, `google_sheets`, and other non-JSON
   grammars, but the skinny workspace metadata names only `json`
   (`Cargo.toml:18-28`, `skinny/Cargo.toml:49-64`). Skinny runtime has a
   `sheets_witness` event-grammar proof test, but it is not a benchmark/report
   consumer (`runtime/src/tape/event_grammar_tests.rs:12-49`). P3-D allows
   non-JSON values such as `css_l4`, `sheets`, and `bbnf_self` only when the
   same wave updates the gate and fixtures; a companion report must also be
   gate-consumed and carry no hidden admission semantics (`p3d-telemetry-schema.md:154-170`).

## Existing Fixture And Test Surfaces

| Surface | Current coverage | W1a leverage |
|---|---|---|
| `skinny/crates/bbnf-bench/src/report.rs` unit tests | Synthetic `Report`, `TelemetryRow`, `SkV8Telemetry`, and comparator evidence builders; broad fail-closed manifest tests. | Best home for passing/failing non-JSON evidence fixtures. Add builders beside existing W0 helpers and keep JSON W0 tests unchanged. |
| `skinny/crates/bbnf-bench/src/gate.rs` unit tests | Outcome parsing, strict admission, strict comparator/native freshness/plane refusal. | Reuse for non-JSON comparator/oracle refusal if W1a introduces an oracle adapter or expands `StrictAdmissionEvidence`. |
| `skinny/crates/bbnf-bench/src/bin/gate.rs` unit tests | Criterion metadata TOML rows, run capture coherence, fixture hash/byte checks, SIMD hash checks. | Add only metadata-shape tests if W1a chooses a Criterion-style non-JSON report fixture; avoid requiring real non-JSON Criterion data in W1a. |
| `skinny/xtask/src/main.rs` unit tests | CostFacts `--with-cost-facts` flags, CostFacts report manifest fields, `RESULTS.md` run-id snapshot. | Add companion-gate CLI flag tests only if W1a creates `gate-non-json` or a `gate-json --non-json-report` entry point. Keep `--with-cost-facts --check-results` green. |
| `skinny/crates/test-fixtures` | JSON corpus manifest, embedded JSON valid/invalid, sha256/size validation. | Do not extend for W1a unless the wave chooses a real non-JSON corpus. For schema fixtures, builder tests are cheaper and avoid generated baseline authority. |
| `skinny/crates/bbnf-bench/benches/` | JSON parity and SIMD scan Criterion harnesses only. | W1a should not add measured non-JSON benchmarks; W1b owns generated non-JSON baseline. W1a can define report fixtures without running Criterion. |
| `skinny/RESULTS.md` | 41 JSON manifest rows with `json` grammar/domain and existing direct/typed SK-V10 row movement. | W1a must keep JSON rows byte-identical unless it explicitly updates every consumer, which SPEC forbids for JSON row movement in W1a. |

## Recommended W1a Fixture Set

Use synthetic report-level fixtures in `report.rs` first. They should exercise
the same structs the real renderer uses, and they should fail before any
behavior benchmark exists. If W1a chooses a companion report, mirror the same
cases in a small parser/validator test for that report's serialized shape.

### Passing fixtures

| Fixture | Shape | Required assertions |
|---|---|---|
| `non_json_css_l4_companion_minimal_passes` | One `css_l4/declaration_values/generated_direct/main` evidence row in a companion report, not `RESULTS.md`. | `grammar_id=css_l4`, `domain=css_l4_bench`, generated Track 1 present, independent oracle present, strict output equality proof present, same run id, host, build flags, sample count, feature mask, no-sidecar proof, and `same_wave_consumer_class=non_json_gate_schema_only`. No row-admission flag. |
| `non_json_css_l4_results_row_schema_passes_when_all_consumed` | One synthetic `TelemetryRow` with non-JSON row id and all existing required manifest/comparator fields consumed by the new validator. | Confirms allowed values for grammar/domain/workload/output plane/comparator-oracle without touching JSON W0 validator. This is useful only if W1a decides non-JSON rows may be rendered in `RESULTS.md`. |
| `json_w0_report_still_passes` | Existing exact opening-baseline builder. | Existing `w0_report_accepts_exact_opening_baseline` remains green and should be called by any composed W1a validator before non-JSON checks. |
| `gate_json_costfacts_check_results_still_passes` | CLI/integration command, not a unit fixture. | `gate-json --with-cost-facts --check-results` succeeds against current JSON results and CostFacts manifest. |

### Failing fixtures

| Fixture | Mutate from passing non-JSON fixture | Expected failure |
|---|---|---|
| `non_json_rejects_missing_grammar_id` | Empty or absent `grammar_id`. | Missing required non-JSON field. |
| `non_json_rejects_unknown_domain` | `domain=json_bench` with `grammar_id=css_l4`, or `domain=css_l4` without `_bench`. | Grammar/domain mismatch. |
| `non_json_rejects_json_only_row_id_shape` | `row_id=json/declaration_values/generated_direct/main` with `grammar_id=css_l4`. | Row identity mismatch. |
| `non_json_rejects_missing_output_plane` | Empty `output_plane` or comparator plane. | Required output/comparator plane absent. |
| `non_json_rejects_plane_mismatch` | Row output plane `css declaration digest`, oracle plane `DOM`. | Strict output equality/comparator plane mismatch. |
| `non_json_rejects_missing_oracle` | No comparator/oracle evidence entry. | Missing independent oracle evidence. |
| `non_json_rejects_oracle_without_source` | Oracle entry has empty `source_artifact`. | Comparator/oracle evidence source absent. |
| `non_json_rejects_track2_coupling` | `track2_independence_status=coupled_to_track1` or equivalent. | Track 2/oracle independence failure. |
| `non_json_rejects_missing_run_id` | Empty run id or malformed prefix. | Missing/invalid run id. |
| `non_json_rejects_mixed_run_id` | Companion report contains two rows with different valid-looking run ids. | Non-uniform run id. |
| `non_json_rejects_missing_host` | Empty host triple or no arch/cpu facts. | Missing host facts. |
| `non_json_rejects_missing_feature_mask` | Empty feature mask or no arch/os/simd/target_cpu facts. | Missing feature mask. |
| `non_json_rejects_gate_only_consumer` | `same_wave_consumer_class=gate_only` on a non-JSON admission-shaped row. | Producer-only/schema-only evidence cannot masquerade as a behavior consumer. |
| `non_json_rejects_producer_only_field` | Add a rendered/report field such as `pmu_cycles` that the validator ignores. | Producer-only telemetry rejection. |
| `non_json_rejects_generated_baseline_claim` | Add `baseline_authority=true`, `wave_id=SK-V11-W1b`, or a PASS/admission verdict. | W1a cannot claim generated non-JSON baseline authority or row admission. |
| `non_json_rejects_parse_only_sota_claim` | Workload `parse_only` with GO/admission fields. | Parse-only SOTA/non-JSON close claim rejected. |
| `non_json_rejects_direct_digest_as_typed` | Workload/consumer claims typed direct but output plane is digest. | Direct digest cannot prove typed product. |

## Recommended Validator Shape

The lowest-risk redress shape is additive:

1. Leave `Report::validate_sk_v8_w0` untouched for JSON.
2. Add a W1a validator for either:
   - `Report::validate_sk_v11_w1a_non_json_fixture(&NonJsonEvidenceReport)`, if
     W1a uses a companion report under `restart/skinny/tranches/sk-v11/research/w1a/`;
     or
   - `Report::validate_sk_v11_w1a_non_json_rows`, if W1a chooses to render
     non-JSON rows in `RESULTS.md`.
3. Make the W1a validator call the existing JSON validator first for JSON rows,
   then validate only the synthetic non-JSON fixture/report.
4. Add an unknown-field check for any serialized companion report so producer-only
   fields fail closed. For an in-memory `TelemetryRow` fixture, model this as a
   parsed report payload rather than a Rust struct mutation, because Rust structs
   cannot represent ignored unknown fields after deserialization unless
   `serde(deny_unknown_fields)` or a raw `Value` check is used.
5. Keep `same_wave_consumer_class=gate_only` valid only for existing JSON W0
   rows. W1a schema-only non-JSON fixtures should use a distinct value such as
   `non_json_gate_schema_only`; W1b/W2 can later require a generated parser
   consumer class.

## Commands To Run

Run from `/Users/mkbabb/Programming/bbnf-lang/skinny` unless noted.

Focused unit surfaces:

```sh
cargo test -p bbnf-bench report::tests -- --nocapture
cargo test -p bbnf-bench gate::tests -- --nocapture
cargo test -p bbnf-bench metadata -- --nocapture
cargo test -p bbnf-bench --bin gate w0_ -- --nocapture
cargo test -p xtask w1_costfacts -- --nocapture
```

Fixture loader and non-bench proof surfaces:

```sh
cargo test -p test-fixtures -- --nocapture
cargo test -p runtime event_grammar -- --nocapture
cargo test -p runtime --features proof event_grammar -- --nocapture
```

JSON gate preservation:

```sh
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --advisory --check-results
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
git diff --exit-code -- RESULTS.md
```

Optional full W0 rerun only if the redress owner intentionally refreshes native
Criterion data:

```sh
CARGO_TARGET_DIR=/tmp/skv11-w1a-target CRITERION_HOME=/tmp/skv11-w1a-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- bench-json --advisory
CRITERION_HOME=/tmp/skv11-w1a-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
```

## Pre-blocks For W1a

- Do not add or regenerate non-JSON parser output in W1a. W1b owns the first
  generated non-JSON baseline row.
- Do not edit `skinny/RESULTS.md` for JSON row movement. The W1a exit gate
  requires no JSON row moves.
- Do not relax `validate_sk_v8_w0` to accept non-JSON; add a separate W1a
  validator or companion report gate.
- Do not treat runtime `sheets_witness` proof tests as benchmark evidence.
- Do not let PMU, cycles, structural scan, masking probes, Criterion slope, or
  any extra report field become producer evidence unless the W1a validator reads
  and rejects/accepts it explicitly.

Self-verdict: research artifact only. I did not edit source and did not run the
test commands above.
