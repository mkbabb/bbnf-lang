# SK-V8 W0 Telemetry And Gate-JSON Enforcement Research

Role: W0 research agent A.
Scope: telemetry schema and `gate-json` enforcement only. No implementation wave is dispatched by this artifact.
Date: 2026-05-18.

## Contract Summary

W0 is telemetry-only. SPEC Section 0.4 allows the live 26-column `skinny/RESULTS.md` table to remain unchanged, but every SK-V8 field added after W0 must be consumed by `gate-json` in the same wave. SPEC Section 3 requires all 38 current main rows to satisfy Section 0.4, sidecar freshness validation, malformed sidecar manifest rejection, unsupported-outcome rejection, strict-admission rejection for stale/deferred/mismatched evidence, and no parser/scanner/SIMD/asm/codegen/product/generated-output behavior change.

DISPATCH W0 confirms the redress phase may implement only W0 telemetry and gate validation, run focused `bbnf-bench`/`xtask` tests, run the updated `gate-json` path, prove every current main row has required telemetry, keep throughput within +/-1.0% of `SK-V8-open`, and prove no behavior change.

## Owner Paths

Allowed W0 implementation owner paths:

- `skinny/crates/bbnf-bench/`
- `skinny/xtask/src/`
- `skinny/RESULTS.md`
- `restart/skinny/tranches/sk-v8/research/` with `wave-0-<topic>.md` naming
- `skinny/REDRESS.md` only if W0 rejects

This research file writes only `restart/skinny/tranches/sk-v8/research/wave-0-telemetry-gate-research.md`.

## Current Code Surface

`skinny/crates/bbnf-bench/src/report.rs`

- `SCHEMA_V3_HEADER` and `SCHEMA_V3_ALIGN` define the existing 26-column rendered table.
- `Report` currently stores `title`, `rows`, `probe_rows`, and `notes`.
- `TelemetryRow` currently stores legacy row fields only: corpus/workload/outcome/verdict/strictness/parse UTF-8/escape/flaw/output plane/track Mbps/comparator Mbps/deltas/hot leaf/signal.
- `TelemetryRow::parse`, `TelemetryRow::workload`, and `TelemetryRow::new` construct rows with hard-coded `Strictness=deferred`, `parse_utf8=view-boundary`, and placeholder hot leaf text.
- `TelemetryRow::validate_schema_v3` checks legacy row non-emptiness, Track 1/Track 2 Mbps, `sonic-rs strict Mbps`, `serde_json Mbps`, and `Delta vs sonic-strict`; it does not check any SK-V8 Section 0.4 telemetry field.
- `Report::validate_schema_v3` only loops over legacy row validation.
- `Report::render_markdown` emits the legacy table plus probe rows and notes.
- Existing report tests cover legacy header rendering, missing comparator rejection, and probe rendering only.

`skinny/crates/bbnf-bench/src/gate.rs`

- `Outcome` includes `A`, `B`, `C`, `D`, `E`, `F-positive`, `F-noise`, `G`, `I`, `J`, `K`, `L`, `M`, and `N-direct`; it does not include reserved optional `S`, nor a parser for rejecting arbitrary outcome strings in telemetry.
- `validate_schema(rows: &[RowMetadata])` validates criterion metadata rows, not the rendered/report telemetry rows.
- `ThresholdInput.schema_ok` is a boolean combining metadata validity and estimate presence in the binary. It has no field-level diagnostics and no SK-V8 telemetry policy.
- `classify` treats `schema_ok=false` as `JSchemaFail`, then computes parse outcomes.
- Existing gate tests cover old threshold classification, metadata schema failure, SIMD hash failure, and direct-projection classification. There are no tests for Section 0.4 fields or strict-admission refusal.

`skinny/crates/bbnf-bench/src/bin/gate.rs`

- `main` reads criterion metadata and estimates, computes gate outcomes, pushes 17 `parse_only`, 17 `direct_to_struct`, and currently 4 `real_typed_struct` rows when available, validates legacy schema, writes `skinny/RESULTS.md`, and exits by verdict.
- `parse_comparators`, `direct_comparators`, and `real_typed_comparators` produce comparator Mbps cells.
- `sidecar_comparators` hard-codes C++ sidecar values. There is only a prose note saying C++ sidecar columns do not count as same-run strict anchors; no manifest is parsed or validated.
- `Estimates::required_present` checks timing coverage but does not feed row-level sample counts, profile artifacts, run ids, host triples, build flags, feature masks, comparator planes, or freshness into the rendered report.

`skinny/crates/bbnf-bench/src/metadata.rs`

- `RowMetadata` already carries several useful W0 sources: `cpu_model`, `cpu_arch`, `os_kernel`, `rustflags`, `target_cpu`, `profile`, `sample_size`, `measurement_time_s`, `feature_mask`, `sidecar_freshness`, `hot_leaf`, workload, strictness, parse mode, output plane, and comparator crate/version.
- `RowMetadata::required_fields_present` validates those metadata fields for criterion metadata, but its result is not the SK-V8 Section 0.4 report schema.
- W0 should reuse `RowMetadata` as evidence input where possible, but not treat it as sufficient because report rows aggregate multiple metadata rows and sidecar constants.

`skinny/xtask/src/main.rs`

- `gate_json` dispatches `cargo run -p bbnf-bench --bin gate -- ...`.
- `gate_json_cost_facts` is separate and currently only accepts `--with-cost-facts` plus optional `--advisory`.
- There is no W0 telemetry flag, manifest flag, malformed-manifest fixture path, or explicit "require SK-V8 telemetry" mode.

## Missing W0 Fields

Legacy report rows partially cover `Strictness`, `parse_utf8`, `escape_complete`, `flaw_probe`, `Output plane`, throughput, comparator Mbps, `Hot leaf`, and `Signal`. They do not provide the Section 0.4 gate-required fields below as structured, gate-consumed data:

- `row_id`
- `grammar_id`
- `domain`
- `comparator_id`
- `comparator_plane`
- `comparator_strictness`
- `comparator_freshness`
- `measured_validation_path`
- `Profile artifact`
- `Cycles per byte or equivalent sample cost`
- `Sample count`
- `Build flags`
- `Host triple`
- `Feature mask`
- `CostFacts rule id`
- `CostFacts chosen shape`
- `CostFacts rejected alternative ids`
- `Redress entry`
- `Wave id`
- `Run id`
- `Sidecar freshness`
- `SK-V8-open delta`
- `substrate_surface`
- `structural_projection_status`
- `substrate_cardinality`
- `same_wave_consumer_class`
- `track2_independence_status`

Current placeholder values also become W0 blockers after the telemetry lock: `Hot leaf=unprofiled in W0b...`, `Delta vs SK-V6=n/a...`, and prose-only sidecar provenance cannot satisfy Section 0.4.

## Proposed Data Shape

Keep the existing 26-column markdown table stable for W0 and add a structured gate-consumed telemetry payload behind it. The least disruptive shape is:

```rust
pub struct SkV8Telemetry {
    pub row_id: String,
    pub grammar_id: String,
    pub domain: String,
    pub comparator_id: Vec<String>,
    pub comparator_plane: Vec<String>,
    pub comparator_strictness: Vec<String>,
    pub comparator_freshness: Vec<String>,
    pub measured_validation_path: String,
    pub profile_artifact: String,
    pub sample_cost: String,
    pub sample_count: u64,
    pub build_flags: String,
    pub host_triple: String,
    pub feature_mask: String,
    pub costfacts_rule_id: String,
    pub costfacts_chosen_shape: String,
    pub costfacts_rejected_alternative_ids: Vec<String>,
    pub redress_entry: String,
    pub wave_id: String,
    pub run_id: String,
    pub sidecar_freshness: Vec<String>,
    pub sk_v8_open_delta: String,
    pub substrate_surface: String,
    pub structural_projection_status: String,
    pub substrate_cardinality: String,
    pub same_wave_consumer_class: String,
    pub track2_independence_status: String,
}
```

Recommended placement:

- Add `SkV8Telemetry` in `report.rs`.
- Add `sk_v8: SkV8Telemetry` to `TelemetryRow`, not `Option<SkV8Telemetry>`, so constructors must populate W0 telemetry and tests cannot accidentally omit it.
- Add `Report::validate_sk_v8_w0()` and `TelemetryRow::validate_sk_v8_w0()` in `report.rs` for field presence, row identity, and row-local semantics.
- Add strict-admission and comparator-policy helpers in `gate.rs`, using a small gate-facing evidence struct if importing `TelemetryRow` into `gate.rs` would make ownership unclear.
- Have `bin/gate.rs` populate telemetry from criterion metadata, estimates, sidecar manifest facts, host/build facts, and per-workload defaults.
- Have `bin/gate.rs` call both `report.validate_schema_v3()` and `report.validate_sk_v8_w0()` before writing `RESULTS.md`.
- Have `Report::render_markdown` append a machine-readable SK-V8 W0 telemetry manifest section, or write an adjacent gate artifact only if the W0 plan explicitly names that path. In either case, the binary must validate the in-memory manifest before emitting it.

For W0 CostFacts fields, use explicit pre-W1 non-empty values such as `none:pre-w1` / `[]` with a non-empty reason, then W1 tightens those to real CostFacts evidence. Empty strings must reject in W0.

## Gate-JSON Field Consumption

`gate-json` should consume every Section 0.4 field as follows:

| Field | W0 gate consumption |
|---|---|
| `row_id` | Required, unique, stable join key shaped like `json/<corpus>/<workload>/<track_set>`. Reject duplicates and unknown fixtures/workloads. |
| `grammar_id` | Required `json` for current W0 rows. Reject empty and unsupported generic behavior branching. |
| `domain` | Required `json_bench`. Reject empty. |
| `comparator_id` | Required per populated comparator/delta cell; reject unknown ids and populated Mbps cells without comparator ids. |
| `comparator_plane` | Required per comparator; strict admission requires equality with normalized row output plane. |
| `comparator_strictness` | Required per comparator; strict admission requires `strict`; lossy/permissive comparators are flaw probes only. |
| `comparator_freshness` | Required per comparator; strict admission cannot use stale, historical, absent, or prose-only evidence. |
| `measured_validation_path` | Required; strict admission requires `measured-row`. Current `parse_only` rows with `view-boundary` remain substrate-guard non-admission. |
| `Profile artifact` | Required existing artifact/path or explicit gate-approved profile artifact id. Reject placeholder `unprofiled`. |
| `Cycles per byte or equivalent sample cost` | Required positive numeric/equivalent tuple derived from timing and bytes. Reject zero, `n/a`, missing, and non-finite. |
| `Sample count` | Required positive integer. Prefer criterion `sample_size`; reject zero and mixed-run rows without split run ids. |
| `Build flags` | Required release/profile/target/env details. Reuse `RowMetadata.profile`, `rustflags`, `target_cpu`, and relevant env facts. |
| `Host triple` | Required Rust target triple plus host facts. `cpu_arch`/OS alone is not enough if target differs. |
| `Feature mask` | Required. Accept `n/a` only with an explicit non-SIMD reason; SIMD/ASM rows need actual feature/backend facts. |
| `CostFacts rule id` | Required non-empty W0 placeholder such as `none:pre-w1`; W1 later requires real ids. |
| `CostFacts chosen shape` | Required non-empty W0 placeholder; later limited to existing five shapes only. Reject `UnionTape`/new shapes. |
| `CostFacts rejected alternative ids` | Required explicit list or `none:pre-w1`; reject missing. |
| `Redress entry` | Required `none`, `REDRESS-<id>`, or `pending-rejection:<wave>`. |
| `Wave id` | Required `SK-V8-open`/`W0` for W0. Reject later-wave ids during W0. |
| `Run id` | Required, same for same-run evidence or explicitly split where facts are sidecar/historical. |
| `Sidecar freshness` | Required per sidecar comparator. Missing cells need `absent:<reason>`; populated C++ cells need manifest/freshness coverage. |
| `SK-V8-open delta` | Required `baseline` for opening capture; reject plain `n/a` on rows with an opening predecessor. |
| `substrate_surface` | Required W0 baseline value; no new substrate. Parse rows should describe retained offset tape or explicit non-admission baseline. |
| `structural_projection_status` | Required baseline such as `discarded_after_capacity`/`transient_only`/`n/a`; W0 must not claim `retained_as_tape`. |
| `substrate_cardinality` | Required; W0 must report `one`, `zero_or_inert`, or an explicit rejectable value. Reject `two_forbidden`/`unknown` for admission. |
| `same_wave_consumer_class` | Required; W0 telemetry can be `gate_only`, but W3 admission later requires a production consumer. Reject any W0 row pretending schema-only telemetry is production consumption. |
| `track2_independence_status` | Required. Existing Track 2 should be `independent_verified` or `independent_untouched`; reject `coupled_forbidden`. |

Strict-admission refusal should run before any row can be treated as strict SOTA evidence. It must reject if output plane and comparator plane differ, row strictness is not `strict`, comparator strictness is not `strict`, freshness is stale/historical/absent/sidecar-only without same-run manifest, measured validation is not `measured-row`, validation is `view-boundary`/post-parse, or outcome is `K`/reserved `S`.

## Exact Implementation Touchpoints To Plan

`skinny/crates/bbnf-bench/src/report.rs`

- Add `SkV8Telemetry` and, if useful, `SkV8ComparatorEvidence`.
- Extend `TelemetryRow`.
- Update `TelemetryRow::parse`, `TelemetryRow::workload`, and `TelemetryRow::new` to require or build telemetry.
- Add normalization helpers for row output plane and workload track set.
- Add `TelemetryRow::validate_sk_v8_w0`.
- Add `Report::validate_sk_v8_w0`.
- Update `Report::render_markdown` to emit a stable gate-consumed telemetry manifest if the W0 plan chooses manifest-in-RESULTS.
- Add tests for telemetry presence, manifest rendering, and legacy table preservation.

`skinny/crates/bbnf-bench/src/gate.rs`

- Add a strict-admission evidence validator, for example `validate_strict_admission(evidence: &StrictAdmissionEvidence) -> Result<(), GateReject>`.
- Add an outcome id parser/validator for rendered telemetry strings, rather than trusting enum construction paths only.
- Keep `classify` behavior unchanged except for feeding `schema_ok=false` when SK-V8 telemetry validation fails.
- Add failure diagnostics if the plan needs to prove which Section 0.4 field rejected the row.
- Add tests for unsupported outcome, strict plane mismatch, deferred validation admission, stale sidecar strict claim, `K`/`S` non-admission, and producer-only telemetry.

`skinny/crates/bbnf-bench/src/bin/gate.rs`

- Populate `SkV8Telemetry` for parse, direct, and real-typed rows before `report.push_*`.
- Build row ids from grammar/corpus/workload/track set.
- Create a `RunFacts` helper from host metadata and current process/build facts.
- Replace prose-only `sidecar_comparators` handling with values plus freshness evidence. If C++ sidecars remain hard-coded, they need matching manifest facts or must be `stale:<reason>`/`historical:<id>` and non-admission.
- Add malformed sidecar manifest support for tests, preferably by factoring parsing into pure helpers callable from unit tests.
- Call `report.validate_sk_v8_w0()` after `validate_schema_v3()` and before writing.

`skinny/crates/bbnf-bench/src/metadata.rs`

- Prefer reusing existing `RowMetadata` for `profile`, `sample_size`, `measurement_time_s`, host/build fields, feature mask, and sidecar freshness.
- Only extend this file if W0 needs a missing host triple/run id/feature fact that cannot be derived in `bin/gate.rs`.

`skinny/xtask/src/main.rs`

- If W0 introduces a manifest path or explicit telemetry mode, update `USAGE` and `gate_json` passthrough validation.
- Keep `gate-json --with-cost-facts` separate. W0 must not require W1 CostFacts output, but must require explicit pre-W1 CostFacts placeholders in row telemetry.

## Proposed Tests

Focused unit tests in `report.rs`:

- `sk_v8_w0_rejects_missing_telemetry`
- `sk_v8_w0_rejects_missing_profile_artifact`
- `sk_v8_w0_rejects_unprofiled_hot_leaf`
- `sk_v8_w0_rejects_missing_run_id`
- `sk_v8_w0_rejects_missing_sample_cost`
- `sk_v8_w0_rejects_duplicate_row_id`
- `sk_v8_w0_preserves_schema_v3_header`
- `sk_v8_w0_manifest_contains_every_section_0_4_field`

Focused unit tests in `gate.rs`:

- `rejects_unsupported_outcome_id`
- `rejects_k_as_strict_admission`
- `rejects_reserved_s_as_strict_admission`
- `rejects_strict_plane_mismatch`
- `rejects_deferred_validation_admission`
- `rejects_view_boundary_validation_admission`
- `rejects_stale_sidecar_strict_claim`
- `rejects_sidecar_only_cpp_without_same_run_manifest`
- `rejects_producer_only_telemetry`

Focused tests/helpers in `bin/gate.rs`:

- `w0_parse_row_telemetry_uses_non_admission_defaults`
- `w0_direct_row_telemetry_uses_digest_plane`
- `w0_real_typed_row_telemetry_uses_typed_direct_plane`
- `populated_sidecar_cell_requires_freshness_evidence`
- `malformed_sidecar_manifest_rejects`
- `absent_sidecar_cell_requires_absent_reason`

Focused `xtask` test or smoke check:

- `cargo xtask gate-json --advisory` should run the updated gate path and fail if any generated row lacks W0 telemetry.
- If a manifest flag is introduced, include a malformed-manifest invocation that exits non-zero.

## No-Behavior-Change Checks

W0 redress should prove the implementation touched telemetry/report/gate code only and did not alter parser behavior:

- `cargo test -p bbnf-bench`
- Focused test filters for the new report/gate/manifest tests.
- `cargo xtask gate-json --advisory` after current criterion data is available.
- `cargo xtask check-json`
- `cargo xtask check-real-typed`
- `cargo xtask check-conformance`
- Confirm no diffs under parser/scanner/SIMD/asm/codegen/generated-output paths.
- Confirm throughput cells in refreshed `RESULTS.md` move no more than +/-1.0% from `SK-V8-open`.

## Risks

- Profile artifacts and hot leaf attribution may exceed the 90-minute redress cap if W0 tries to deeply profile all 38 rows. The plan should define an acceptable lightweight profile artifact or split before implementation.
- Current C++ comparator sidecars are hard-coded constants. W0 needs manifest/freshness evidence or explicit stale/historical/non-admission treatment; prose notes are no longer enough.
- `RowMetadata` validates benchmark metadata, not report telemetry. Treating it as Section 0.4 compliance would be a paper close.
- Adding a manifest to `RESULTS.md` without validating it in `gate-json` would violate same-wave consumer requirements.
- Making `SkV8Telemetry` optional would preserve old tests while failing the W0 contract. Constructors should force telemetry.
- `Outcome` does not include reserved `S`. W0 can either continue using `K` for parse-only substrate-guard rows or deliberately add string-level acceptance for `S`, but any `S`/`K` strict admission must reject.
- `gate-json --with-cost-facts` is a W1 path. W0 should only require explicit pre-W1 CostFacts placeholders, not real CostFacts behavior.

## Recommendation

Implement W0 as a report/gate schema lock, not a behavior wave:

1. Add required SK-V8 telemetry structs to `report.rs`.
2. Populate every main row in `bin/gate.rs` from existing metadata, estimates, and explicit sidecar manifest facts.
3. Add `Report::validate_sk_v8_w0` and strict-admission validators.
4. Reject missing fields, unsupported outcomes, malformed sidecar manifests, stale/sidecar-only strict claims, and producer-only telemetry before writing `RESULTS.md`.
5. Preserve the rendered 26-column table unless the W0 plan chooses an explicitly gate-consumed manifest section.
6. Keep parser/scanner/SIMD/asm/codegen/product behavior untouched and prove it with the checks above.

Self-verdict: ACCEPT for W0 research readiness.
Confidence: 94%.
