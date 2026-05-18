# SK-V8 W0 Plan: Telemetry Gate Lock

Date: 2026-05-18.
Authority: `G-Alpha closed`; W0 only. W1-W6 remain blocked until W0 closes and each later entry gate is satisfied.

## Inputs

- `wave-0-telemetry-gate-research.md`
- `wave-0-results-baseline-research.md`
- `wave-0-sidecar-freshness-research.md`
- `wave-0-lock14-baseline-research.md`
- `wave-0-no-behavior-proof-research.md`
- `wave-0-verification-plan-research.md`

## Owner Paths

Allowed W0 redress paths:

- `skinny/crates/bbnf-bench/`
- `skinny/xtask/src/`
- `skinny/RESULTS.md`
- `restart/skinny/tranches/sk-v8/research/wave-0-*.md`
- `skinny/REDRESS.md` only if W0 rejects

Freeze paths:

- `skinny/grammars/json.bbnf`
- `skinny/crates/runtime/src/grammars/json/`
- `skinny/crates/runtime/src/tape/`
- `skinny/crates/bbnf-simd/`
- `skinny/crates/codegen/`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/src/track2/`
- `skinny/crates/bbnf-bench/src/parity.rs`
- `skinny/crates/bbnf-bench/src/scan.rs`
- `skinny/crates/bbnf-bench/src/materialization.rs`

## Implementation Shape

Keep the existing schema-v3 26-column markdown table stable. Add a generated `## SK-V8 W0 Telemetry Manifest` section to `skinny/RESULTS.md`. The manifest is part of the report model and is validated by `cargo xtask gate-json` before the report is written.

Add in `skinny/crates/bbnf-bench/src/report.rs`:

- `SkV8Telemetry` on every main `TelemetryRow`.
- `SkV8ComparatorEvidence` for native, sidecar, and absent comparator slots.
- `SkV8OpenBaseline` constants for the 38 current row ids and opening Track 1/Track 2 Mbps values.
- `Report::validate_sk_v8_w0()` for exact 38-row coverage, required fields, duplicate ids, opening-delta status, throughput drift within +/-1.0%, sidecar populated/absent consistency, non-placeholder hot leaves, and no producer-only telemetry.
- Markdown rendering for a compact manifest table that contains every Section 0.4 field or a semicolon-delimited structured value for repeated comparator evidence.

Add in `skinny/crates/bbnf-bench/src/gate.rs`:

- `parse_outcome_id` to reject unsupported outcome strings before admission.
- `StrictAdmissionEvidence` and `validate_strict_admission`.
- Reject `K` and reserved `S` as strict admissions, reject deferred row strictness, view-boundary validation, plane mismatch, stale/historical/absent sidecars, and sidecar-only strict claims without same-run manifest coverage.
- Focused tests for unsupported outcome, stale sidecar strict claim, parse-only strict GO claim, and plane mismatch.

Add in `skinny/crates/bbnf-bench/src/bin/gate.rs`:

- W0 run facts: run id, host triple, build flags, feature mask.
- Per-row telemetry construction from Criterion estimates and metadata rows.
- C++ sidecar evidence as structured values. W0 does not treat historical hard-coded sidecar values as same-run strict anchors. Missing sidecars render as `absent:<reason>`. Populated historical sidecar cells carry manifest-style planning evidence and `historical:<id>` freshness, so strict admission still rejects them.
- Call both `validate_schema_v3()` and `validate_sk_v8_w0()` before writing.

Add in `skinny/crates/bbnf-bench/src/lock14_baseline.rs`:

- Static Lock 14 allowlist classes for JSON grammar input, fixtures, generated JSON output, generated typed output, per-grammar templates/providers, bench telemetry schema, and host/API schema facts.
- A validator called by the gate binary. W0 treats this as an audit allowlist, not a write permit.

`skinny/xtask/src/main.rs` should stay unchanged unless the gate needs a pass-through flag. W0 does not use `gate-json --with-cost-facts`.

## SK-V8-Open Capture

The current `skinny/RESULTS.md` table is the `SK-V8-open` numeric baseline. W0 encodes the 38 current row ids and Track 1/Track 2 cells as constants in the gate/report model:

- 17 `parse_only` rows
- 17 `direct_to_struct` rows
- 4 `real_typed_struct` rows

For the first W0 report every row emits:

- `wave_id=SK-V8-open`
- `sk_v8_open_delta=baseline`
- `run_id=sk-v8-open:<git-sha>:<criterion-root>`

The W0 validator rejects any missing/extra baseline row and any Track 1 or Track 2 movement beyond +/-1.0% from the encoded `SK-V8-open` value.

## Field Rules

Every current main row must carry non-empty values for the SPEC Section 0.4 fields:

- identity: `row_id`, `grammar_id`, `domain`, `wave_id`, `run_id`
- comparator evidence: ids, planes, strictness, freshness, sidecar freshness
- validation: `measured_validation_path`, `substrate_surface`, `structural_projection_status`, `substrate_cardinality`, `same_wave_consumer_class`, `track2_independence_status`
- build/profile: `profile_artifact`, `sample_cost`, `sample_count`, `build_flags`, `host_triple`, `feature_mask`
- cost/redress: `costfacts_rule_id`, `costfacts_chosen_shape`, `costfacts_rejected_alternative_ids`, `redress_entry`
- baseline: `sk_v8_open_delta`

CostFacts fields are explicit W0 placeholders: `none:pre-W1`. W1 must replace them with real CostFacts evidence.

Hot leaf is populated as a Criterion profile artifact reference, not a symbol-profiler claim:

`criterion:<bench-path>/new/estimates.json;hot-leaf=criterion-slope;row=<row_id>`

This is W0 telemetry evidence only and cannot prescribe W3/W4 hot kernels.

## Verification

Required commands before W0 closure:

```sh
(cd skinny && cargo test -p bbnf-bench report::tests gate::tests lock14_baseline::tests)
(cd skinny && cargo xtask check-json)
(cd skinny && cargo xtask check-real-typed)
(cd skinny && cargo xtask check-conformance)
(cd skinny && cargo xtask gate-json --advisory)
git diff --exit-code -- skinny/grammars/json.bbnf skinny/crates/runtime/src/grammars/json skinny/crates/runtime/src/tape skinny/crates/bbnf-simd skinny/crates/codegen
git diff --exit-code -- skinny/crates/bbnf-bench/src/direct_struct.rs skinny/crates/bbnf-bench/src/real_typed_struct.rs skinny/crates/bbnf-bench/src/generated_real_typed.rs skinny/crates/bbnf-bench/src/track2 skinny/crates/bbnf-bench/src/parity.rs skinny/crates/bbnf-bench/src/scan.rs skinny/crates/bbnf-bench/src/materialization.rs
```

If local Criterion data is absent or incomplete, W0 rejects and routes to `skinny/REDRESS.md`; it does not synthesize throughput values by hand.

## Admit Gate

W0 closes only when:

- all 38 current rows have valid SK-V8 telemetry consumed by `gate-json`;
- `parse_only` rows remain substrate-guard non-admission (`K`);
- sidecar cells have manifest/freshness coverage or explicit `absent:<reason>`;
- one malformed sidecar/strict-claim fixture rejects in tests;
- schema-v3 header remains stable;
- no frozen parser/scanner/SIMD/asm/codegen/product/generated surface changes;
- refreshed `RESULTS.md` is generated by the gate path.

## Disposition

APPROVED for W0 redress. Later waves are not dispatched by this plan.
