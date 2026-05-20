# SK-V11 W1a R1: Gate Validator

Date: 2026-05-20.
Scope: R1 gate-validator research for `G-W1a-NONJSON-GATE`; read-only analysis of current JSON gate/report behavior and the minimum W1a validator changes for non-JSON grammar evidence.
Output: this file.

## §1 — Findings (concrete, file:line cited)

1. W1a is a schema/gate lane, not a behavior lane. SPEC §4 says no parser row moves, and limits W1a work to teaching gate/report code to consume non-JSON benchmark evidence, adding pass/fail gate fixtures for grammar/domain/output-plane/comparator-oracle/Track2/run-id/host/feature-mask/consumer/producer-only cases, and proving JSON `gate-json --with-cost-facts --check-results` stays green (`restart/skinny/tranches/sk-v11/SPEC.md:284-315`). The dispatch protocol makes W1a CHALLENGE-mandatory and keeps research read-only (`restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:81-117`).

2. Current `gate-json` has two relevant validator paths. Without `--with-cost-facts`, `xtask` shells into `cargo run -p bbnf-bench --bin gate -- ...` and the bench gate builds a Markdown `Report`, validates `validate_schema_v3()` then `validate_sk_v8_w0()`, and compares the rendered output byte-for-byte to `skinny/RESULTS.md` unless updating (`skinny/xtask/src/main.rs:242-274`, `skinny/crates/bbnf-bench/src/bin/gate.rs:349-368`). With `--with-cost-facts`, `xtask` does not invoke the bench report validator; it text-checks `RESULTS.md` for W0 snapshot markers/run-id uniformity, then emits and validates a CostFacts JSON report (`skinny/xtask/src/main.rs:298-307`, `skinny/xtask/src/main.rs:328-386`, `skinny/xtask/src/main.rs:470-505`). Therefore W1a cannot claim non-JSON consumption from the existing `--with-cost-facts --check-results` command alone.

3. The bench gate currently discovers only JSON evidence. It iterates `test_fixtures::load_available_bench_fixtures()`, reads Criterion groups named `json_<fixture>`, and reads only known JSON bench names (`skinny/crates/bbnf-bench/src/bin/gate.rs:42-54`, `skinny/crates/bbnf-bench/src/bin/gate.rs:86-101`, `skinny/crates/bbnf-bench/src/bin/gate.rs:1298-1317`). The run fingerprint likewise accepts only `json_` groups tied to known SK-V8 baseline row ids, plus the JSON SIMD scan (`skinny/crates/bbnf-bench/src/bin/gate.rs:721-774`). Non-JSON Criterion files are currently unconsumed, not merely rejected.

4. The generated JSON report hardcodes JSON identity. `w0_telemetry()` emits row ids as `json/<corpus>/<workload>/main`, `grammar_id = "json"`, and `domain = "json_bench"` (`skinny/crates/bbnf-bench/src/bin/gate.rs:444-530`). The default `SkV8Telemetry::placeholder()` does the same (`skinny/crates/bbnf-bench/src/report.rs:389-418`). A non-JSON row must not rely on these constructors without an explicit non-JSON identity path.

5. If a non-JSON row is inserted into the current `Report`, it fails closed at several exact JSON-only checks:
   - `validate_sk_v8_w0()` rejects any `grammar_id/domain` other than `json/json_bench` (`skinny/crates/bbnf-bench/src/report.rs:327-331`).
   - `Report::validate_sk_v8_w0()` rejects any row id not in `SK_V8_OPEN_BASELINE`, except the one hardcoded SK-V10 W6 typed row, and also enforces the exact expected row count and all baseline rows (`skinny/crates/bbnf-bench/src/report.rs:509-574`).
   - `parse_row_id()` accepts only `json/<corpus>/<workload>/main` (`skinny/crates/bbnf-bench/src/report.rs:1675-1684`).
   - W0 profile validation requires `criterion-slope-profile:json_<corpus>/<json bench>/new/estimates.json` and only the three JSON workloads `parse_only`, `direct_to_struct`, and `real_typed_struct` (`skinny/crates/bbnf-bench/src/report.rs:1266-1303`).
   - W0 manifest semantics require `none:pre-W1` CostFacts sentinels, `redress_entry = none`, `track2_independence_status = independent_verified`, structured native build/host/feature fields, and one of the JSON workload substrate tuples (`skinny/crates/bbnf-bench/src/report.rs:1305-1391`).
   - W0 admission boundary requires `strictness = deferred`, `measured_validation_path = view-boundary`, and `parse_utf8 = view-boundary`, which rejects measured strict non-JSON admission evidence (`skinny/crates/bbnf-bench/src/report.rs:1394-1419`).
   - W0 rows require `same_wave_consumer_class = gate_only`, while admitted non-JSON behavior rows must not be gate-only (`skinny/crates/bbnf-bench/src/report.rs:369-374`; `restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md:164-166`).

6. Current comparator validation is JSON-specific and blocks a non-JSON oracle. Every row must have nonempty comparator evidence; comparator ids must be native JSON anchors, the JSON lossy flaw probe, or the fixed sidecar list, otherwise the validator returns `unsupported comparator id` (`skinny/crates/bbnf-bench/src/report.rs:1433-1525`). It also requires native `sonic_rs_strict` and `serde_json` comparators for every row (`skinny/crates/bbnf-bench/src/report.rs:1518-1519`), maps native comparator source paths to `criterion:json_<corpus>/...`, and supports only JSON workloads (`skinny/crates/bbnf-bench/src/report.rs:1611-1673`). The generic strict-admission helper also accepts only `sonic_rs_strict` or `serde_json` as strict comparators (`skinny/crates/bbnf-bench/src/gate.rs:136-180`).

7. Current schema-v3 checks are strict enough for JSON but too JSON-shaped for non-JSON. `TelemetryRow::validate_schema_v3()` requires nonempty rendered fields plus Track 1, Track 2, sonic strict, serde_json, and delta-vs-sonic values for every row (`skinny/crates/bbnf-bench/src/report.rs:220-273`). That preserves JSON gate health, but a non-JSON grammar without serde_json as a meaningful same-output comparator needs a sibling oracle rule rather than weakening this function globally.

8. Producer-only telemetry is currently not generally detectable for future non-JSON reports. The existing `Report` structs derive `Deserialize`, but the bench gate does not read a serialized report from disk; it constructs one internally (`skinny/crates/bbnf-bench/src/report.rs:11-17`, `skinny/crates/bbnf-bench/src/bin/gate.rs:48-54`). The CostFacts validator consumes JSON values and checks known required fields, but it does not reject unknown top-level or manifest keys (`skinny/xtask/src/main.rs:470-505`). W1a needs an explicit strict key-set or `serde(deny_unknown_fields)` path for non-JSON evidence, otherwise extra producer fields can be silently ignored.

## §2 — Recommendations (named falsifiability gates)

1. Keep `G-W1a-NONJSON-GATE` as a sibling evidence lane, not a relaxation of W0 JSON validation. The smallest safe shape is a companion non-JSON evidence report under `restart/skinny/tranches/sk-v11/research/w1a/` with a named gate command, while leaving the existing JSON `Report::validate_schema_v3()` and `Report::validate_sk_v8_w0()` invariants intact. This matches SPEC §0.3, which allows a companion gate-consumed report when non-JSON rows are not admitted to `skinny/RESULTS.md` (`restart/skinny/tranches/sk-v11/SPEC.md:104-115`).

2. Add a narrow `NonJsonEvidenceReport` validator in `report.rs` with strict deserialization or explicit key-set checking. Required fields should be the P3-D identifiers needed for W1a: `row_id`, `grammar_id`, `domain`, `corpus`, `workload`, `output_plane`, `track1_mbps`, `track2_mbps` or `oracle_mbps`, comparator/oracle id, comparator/oracle plane, strictness, freshness, value, source artifact, `measured_validation_path`, `profile_artifact`, `sample_cost`, `sample_count`, `build_flags`, `host_triple`, `feature_mask`, `wave_id`, `run_id`, `same_wave_consumer_class`, `track2_independence_status`, and `diagnostic_nonproducer_status` (`restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md:96-148`).

3. For W1a, validate non-JSON identity and evidence without admitting authority. The pass fixture should require `grammar_id` in the SK-V11 set (`css_l4`, `sheets`, `bbnf_self`), exact paired domain, row id `<grammar_id>/<corpus>/<workload>/main`, SPEC-named workload/output plane, finite positive Track 1 and independent Track 2/oracle Mbps, a nonempty oracle source artifact on the same output plane, structured host/build/feature fields, a valid uniform SK-V11 W1a run id, and an explicit non-admitting status such as `wave_id = SK-V11-W1a` plus `sk_v9_open_delta = nonjson-schema-probe-only`. It must reject `A / GO` row admission or any generated-baseline authority in W1a because SPEC §4 forbids row movement and baseline claims (`restart/skinny/tranches/sk-v11/SPEC.md:286-315`).

4. Add failing fixtures for every W1a exit condition: missing grammar id, wrong domain, malformed row id, missing/unsupported output plane, missing oracle/comparator id, missing oracle source artifact, missing or coupled Track 2/oracle status, invalid or mixed run id, missing host/build/feature mask, `same_wave_consumer_class = gate_only` on an admitted behavior-shaped row, unknown extra telemetry key, and any field present in the report but not consumed by the validator. P3-C names these as the W1a exit condition (`restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:75-80`).

5. Keep JSON green by testing both lanes separately. The JSON gate should still run the existing `gate-json --with-cost-facts --check-results` command and the existing `bbnf-bench` gate/report tests. The non-JSON W1a gate should be a separate command or a separate optional subpath whose failure cannot change JSON `RESULTS.md` row count, JSON row identity, or W0 baseline movement. If a later plan chooses to render non-JSON rows in `skinny/RESULTS.md`, it must update the `xtask` W0 snapshot checker too, because that checker currently counts only lines beginning `| json/` (`skinny/xtask/src/main.rs:334-351`).

## §3 — Risks (REDRESS entries to pre-block)

1. Weakening JSON validators to accept non-JSON is the main regression risk. Broadening `parse_row_id()`, `validate_comparator_evidence()`, or `validate_sk_v8_w0()` in place can accidentally admit JSON rows with stale sidecars, wrong strict planes, or non-uniform row identity. Prefer new non-JSON-specific helpers and keep JSON call sites unchanged.

2. A non-JSON row in `skinny/RESULTS.md` can become producer-only if only the current `--with-cost-facts --check-results` path is used, because that path text-scans JSON rows and ignores non-JSON row prefixes. This is a false green gate unless a sibling gate consumes the companion report or the snapshot checker is extended.

3. Unknown-field tolerance is a producer-only telemetry leak. If W1a uses `serde_json::Value` or derived `Deserialize` without `deny_unknown_fields`, extra comparator/oracle/profile fields can be emitted without validator consumption.

4. Reusing JSON comparators as generic proof is pre-blocked. P3-D blocks generic-crate JSON policy hidden in grammar/domain/workload/output-plane values and sidecar facts promoted to product evidence (`restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md:263-280`). Non-JSON evidence should name a same-output independent oracle rather than laundering a JSON strict anchor through the existing comparator rules.

5. W1a must not create W1b authority early. A passing W1a fixture proves the validator can consume non-JSON evidence; it must not establish the first generated non-JSON baseline row or behavior admission. W1b owns baseline plus oracle, and W2 owns the preferred admitted non-JSON intervention (`restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:67-70`, `restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:171-175`).

## §4 — Sources (every external citation)

No external citations. Internal sources read:

- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/skinny/tranches/sk-v11/SPEC.md`
- `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/gate.rs`
- `skinny/xtask/src/main.rs`

## Verification commands

Recommended verification for a W1a implementation:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo test -p bbnf-bench gate::tests report::tests
cargo test -p xtask costfacts
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
```

Additional W1a-only commands should be added with the implementation, for example:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo test -p bbnf-bench non_json_gate
cargo run -p xtask -- gate-non-json --manifest ../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-pass.json
cargo run -p xtask -- gate-non-json --manifest ../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-producer-only.json
```

The last command must fail. The JSON command must remain green before W1a can close `G-W1a-NONJSON-GATE`.
