# SK-V11 W1a R2 Report/Metadata Schema Research

Status: read-only research artifact. Source authority is unchanged. This file
records the current report and metadata schema surfaces, then names the W1a
allowed-value extensions needed to consume non-JSON evidence without adding
new rendered columns, manifest fields, or metadata keys.

## Read Set

- `restart/skinny/tranches/sk-v11/SPEC.md` section 0.3 and section 4.
- `restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md`.
- `skinny/crates/bbnf-bench/src/report.rs`.
- `skinny/crates/bbnf-bench/src/metadata.rs`.
- `skinny/RESULTS.md` main schema-v3 table and `SK-V9 W0 Telemetry Manifest`.
- `restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md`
  for the concrete non-JSON row-name candidates.

## Current Schema-v3 Surface

The live rendered main table has 26 columns:

```text
Corpus
Workload
Outcome
Verdict
Strictness
parse_utf8
escape_complete
flaw_probe
Output plane
Track 1 Mbps
Track 2 Mbps
sonic-rs strict Mbps
sonic-rs lossy Mbps
simdjson DOM Mbps
simdjson On Demand Mbps
yyjson default Mbps
asmjson SWAR Mbps
asmjson AVX-512 Mbps
RapidJSON default Mbps
serde_json Mbps
Delta vs SK-V6
Delta vs sonic-strict
Delta vs simdjson DOM
Delta vs yyjson
Hot leaf
Signal
```

The gate-consumed schema-v3 identifier set is larger than the 26 physical
columns because the manifest and comparator cell fold several identifiers into
one rendered cell. The current required identifiers are:

```text
row_id
grammar_id
domain
corpus
workload
outcome_id
verdict
strictness
parse_utf8
escape_complete
flaw_probe
output_plane
track1_mbps
track2_mbps
comparator_id
comparator_plane
comparator_strictness
comparator_freshness
sidecar_freshness
comparator_value_mbps
comparator_source_artifact
measured_validation_path
profile_artifact
sample_cost
sample_count
build_flags
host_triple
feature_mask
costfacts_rule_id
costfacts_chosen_shape
costfacts_rejected_alternative_ids
redress_entry
wave_id
run_id
sk_v9_open_delta
substrate_surface
structural_projection_status
substrate_cardinality
same_wave_consumer_class
track2_independence_status
diagnostic_nonproducer_status
```

W1a should not add a 27th main table column. Any new non-JSON fact must be
represented as an allowed value or structured payload inside these identifiers,
then consumed by the same-wave gate.

## Manifest Fields

The current manifest header is:

```text
Row id
Grammar
Domain
Wave
Run id
Validation
Profile artifact
Sample cost
Sample count
Build flags
Host triple
Feature mask
CostFacts
Redress
SK-V9-open delta
Substrate
Structural projection
Cardinality
Consumer
Track 2
Diagnostic nonproducer
Comparator evidence
```

The backing fields are:

| Manifest field | Backing identifier |
|---|---|
| `Row id` | `row_id` |
| `Grammar` | `grammar_id` |
| `Domain` | `domain` |
| `Wave` | `wave_id` |
| `Run id` | `run_id` |
| `Validation` | `measured_validation_path` |
| `Profile artifact` | `profile_artifact` |
| `Sample cost` | `sample_cost` |
| `Sample count` | `sample_count` |
| `Build flags` | `build_flags` |
| `Host triple` | `host_triple` |
| `Feature mask` | `feature_mask` |
| `CostFacts` | `costfacts_rule_id:costfacts_chosen_shape:costfacts_rejected_alternative_ids` |
| `Redress` | `redress_entry` |
| `SK-V9-open delta` | `sk_v9_open_delta` |
| `Substrate` | `substrate_surface` |
| `Structural projection` | `structural_projection_status` |
| `Cardinality` | `substrate_cardinality` |
| `Consumer` | `same_wave_consumer_class` |
| `Track 2` | `track2_independence_status` |
| `Diagnostic nonproducer` | `diagnostic_nonproducer_status` |
| `Comparator evidence` | `comparators: Vec<SkV8ComparatorEvidence>` |

Current `skinny/RESULTS.md` manifest values are JSON-only:

- `grammar_id`: `json`.
- `domain`: `json_bench`.
- `row_id`: `json/<corpus>/<workload>/main`.
- `wave_id`: `SK-V9-open`, `SK-V10-W2`, `SK-V10-W6`.
- `run_id`: `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`.
- `measured_validation_path`: `view-boundary`, `measured-row`.
- `sample_count`: `50`, `100`.
- `build_flags`: `profile=bench;rustflags=-C target-cpu=native;target_cpu=native`.
- `host_triple`: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
- `feature_mask`: `arch=aarch64;os=macos;simd=Scalar;target_cpu=native`.
- `CostFacts`: currently `none:pre-W1:none:pre-W1:none:pre-W1`.
- `redress_entry`: `none`.
- `sk_v9_open_delta`: `baseline`, `direct-reclaimed`, `typed-row-added`.
- `substrate_surface`: `borrowed_view_over_offset_tape`,
  `sink_only_digest`, `typed_direct_projection`.
- `structural_projection_status`: `discarded_after_capacity`, `n/a`.
- `substrate_cardinality`: `one`, `zero_or_inert`.
- `same_wave_consumer_class`: `gate_only`,
  `gate_json_direct_contract`, `gate_json_typed_contract`.
- `track2_independence_status`: `independent_verified`.
- `diagnostic_nonproducer_status`:
  `structural_scan+masking_probes+pmu+cycles:nonproducer`.

## Comparator Fields

Each comparator evidence entry renders as:

```text
<comparator_id>[plane=<comparator_plane>,strictness=<comparator_strictness>,freshness=<comparator_freshness>,sidecar=<sidecar_freshness>,mbps=<comparator_value_mbps>,source=<comparator_source_artifact>]
```

Backing fields:

| Comparator field | Current meaning |
|---|---|
| `comparator_id` | Identity of native comparator, flaw probe, sidecar comparator, or future oracle. |
| `comparator_plane` | Comparator/oracle output plane. Current values are `DOM`, `digest`, `typed direct`. |
| `comparator_strictness` | Current values are `strict` and `permissive`. |
| `comparator_freshness` | Current values are `same-run-native`, `historical:sk-v7-sidecar-profile`, and `absent:not-collected-for-<workload>`. |
| `sidecar_freshness` | Current values are `n/a`, `historical:sk-v7-sidecar-profile`, and `absent:not-collected-for-<workload>`. |
| `comparator_value_mbps` | Positive Mbps or `n/a` when absent. |
| `comparator_source_artifact` | Criterion path, historical sidecar path, absence proof, or future oracle source artifact. |

Current comparator identities:

- Native strict anchors: `sonic_rs_strict`, `serde_json`.
- Native flaw probe: `sonic_rs_lossy`.
- Historical/absent sidecar slots: `simdjson_dom`, `simdjson_ondemand`,
  `yyjson_default`, `asmjson_swar`, `asmjson_avx512`,
  `rapidjson_default`.

Current native strict source/plane mapping:

| Workload | `sonic_rs_strict` source suffix | `serde_json` source suffix | Plane |
|---|---|---|---|
| `parse_only` | `sonic_rs_anchor` | `serde_json` | `DOM` |
| `direct_to_struct` | `sonic_rs_direct_to_struct` | `serde_json_direct_to_struct` | `digest` |
| `real_typed_struct` | `sonic_rs_real_typed_struct` | `serde_json_real_typed_struct` | `typed direct` |

Current sidecar rules are intentionally not strict-admission rules: sidecars are
`DOM`, `strict`, and either historical or explicitly absent. The current
validator rejects `sidecar-same-run` unless a structured same-wave sidecar
manifest parser and gate are added.

## Current Grammar, Domain, Workload, and Output-plane Values

Current report/gate values:

- Grammar/domain pair: `json` / `json_bench`.
- Workloads: `parse_only`, `direct_to_struct`, `real_typed_struct`.
- Main-table output planes:
  - `borrowed view over offset tape vs DOM`.
  - `digest`.
  - `typed direct`.
- Manifest substrate surfaces:
  - `borrowed_view_over_offset_tape`.
  - `sink_only_digest`.
  - `typed_direct_projection`.
- Comparator planes:
  - `DOM`.
  - `digest`.
  - `typed direct`.

Current gate restrictions that W1a must account for:

- `validate_sk_v8_w0` rejects any `grammar_id` / `domain` except
  `json` / `json_bench`.
- `parse_row_id` rejects any row id whose first segment is not `json`.
- `validate_w0_manifest_semantics` only knows the three JSON workloads and
  their current substrate tuples.
- `validate_comparator_evidence` requires the JSON native comparators and all
  JSON sidecar slots.

## Metadata Schema

`metadata.rs` declares `SCHEMA_VERSION = "3"` and `RowMetadata` with these
fields:

```text
schema_version
cpu_model
cpu_arch
os_kernel
rustflags
target_cpu
profile
input_sha256
input_bytes
competitor_crate
competitor_version
bbnf_commit
warmup_samples
warmup_time_s
sample_size
measurement_time_s
confidence_interval
outlier_rejection
statistical_method
track
workload
strictness
parse_utf8
escape_complete
flaw_probe
output_plane
feature_mask
api_symbol
sidecar_freshness
primitive_status
hot_leaf
materialisation
parse_mode
source_ownership
allocator
plan_variant
host_call_mode
arena_writes
payload_allocations
scalar_parity_hash_twitter
scalar_parity_hash_citm
scalar_parity_hash_canada
peak_rss_bytes
cold_cache_mode
```

Current metadata track tags are `track1_generated`, `track2_handcoded`,
`competitor`, `simd_scan`, and `probe`. W1a does not need new metadata keys for
non-JSON report admission. It needs allowed-value extensions for existing
metadata fields such as `workload`, `output_plane`, `api_symbol`,
`materialisation`, `parse_mode`, `primitive_status`, and `hot_leaf`, plus the
existing report manifest identifiers for grammar/domain/run/provenance.

## W1a Allowed-value Extensions Needed

W1a should extend values and validators inside the current schema only:

| Existing field | Current live constraint | W1a extension needed |
|---|---|---|
| `row_id` | `json/<corpus>/<workload>/main` only. | Accept `<grammar_id>/<corpus>/<workload>/main`; keep the suffix `main` and require row id grammar/corpus/workload to match rendered fields. |
| `grammar_id` | `json`. | Add `css_l4`, `sheets`, `bbnf_self`. |
| `domain` | `json_bench`. | Add `css_l4_bench`, `sheets_bench`, `bbnf_self_bench`. |
| `corpus` | JSON fixture names such as `twitter`, `canada`, and `unicode_basic`. | Add selected non-JSON corpus names, for example `declaration_values` for CSS and `formula` for Sheets, only when the selected grammar/workload is gate-consumed. |
| `workload` | `parse_only`, `direct_to_struct`, `real_typed_struct`. | Add only SPEC-named generated direct/typed non-JSON workload tokens. Under the existing row-id shape, P3-A's preferred `css_l4/declaration_values/{direct,typed}` rows normalize to `css_l4/declaration_values/direct/main` or `css_l4/declaration_values/typed/main`, so `declaration_values` is the corpus and `direct` or `typed` is the workload. The fallback Sheets row must reconcile P3-A's `google_sheets/formula/{direct,typed}` spelling with P3-D's `sheets` grammar id before a gate value is accepted. |
| `output_plane` | `borrowed view over offset tape vs DOM`, `digest`, `typed direct`. | Keep `digest` and `typed direct`; add a SPEC-named non-JSON direct/typed output plane only if the selected grammar cannot honestly reuse those planes. |
| `comparator_plane` | `DOM`, `digest`, `typed direct`. | Match `output_plane` for strict admission or oracle equality. Non-JSON oracle planes must be exact, gate-consumed values. |
| `comparator_id` | JSON native/sidecar ids only. | Add SPEC-named same-run independent oracle ids, for example P3-D's `internal_oracle`, or a same-run native comparator if one exists for the selected non-JSON output plane. |
| `comparator_freshness` | `same-run-native`, historical sidecar, or absent sidecar. | Add a same-run oracle freshness token if the oracle is not a native comparator. Historical, absent, stale, and sidecar-only evidence must not admit a non-JSON row. |
| `sidecar_freshness` | `n/a`, historical, absent. | Keep `n/a` for non-sidecar same-run oracles. Do not use `sidecar-same-run` unless W1a also lands a structured sidecar manifest parser and gate consumer. |
| `comparator_source_artifact` | Criterion, historical sidecar, or absence proof paths. | Use the existing source field to name the non-JSON oracle or Track 2 path. Do not add a new oracle-path column. |
| `track2_independence_status` | `independent_verified`. | Keep `independent_verified` or add a SPEC-named equivalent proof token; coupled Track 2 remains forbidden. |
| `same_wave_consumer_class` | `gate_only`, `gate_json_direct_contract`, `gate_json_typed_contract`. | Add SPEC-named generated non-JSON direct/typed parser consumer classes. Admitted non-JSON behavior rows must not use `gate_only`. |
| `measured_validation_path` | `view-boundary`, `measured-row`. | Non-JSON strict row admission should use `measured-row`; W1a fixtures should reject deferred validation admission. |
| `strictness` | `deferred`, `strict`. | Non-JSON strict row admission should use `strict`; keep parse-only or deferred rows non-admitting. |
| `parse_utf8` | `view-boundary`, `measured-row`. | Non-JSON strict row admission needs an explicit measured validation token appropriate to the grammar; prefer `measured-row` when UTF-8 is part of the row contract. |
| `escape_complete` | `yes`. | Keep `yes` for strict output equality, or define a grammar-specific value only if the validator consumes it and does not weaken JSON. |
| `substrate_surface` | JSON substrate tuple only. | Reuse `sink_only_digest` or `typed_direct_projection` when the selected non-JSON output plane is a direct digest or typed direct product; otherwise add a SPEC-named non-JSON surface value. |
| `structural_projection_status` | `discarded_after_capacity`, `n/a`. | Keep `n/a` for direct/typed products unless the selected grammar needs a gate-consumed projection status. |
| `substrate_cardinality` | `one`, `zero_or_inert`. | Keep `zero_or_inert` for direct/typed product rows unless the selected output actually has nonzero substrate cardinality and the gate consumes the value. |
| `wave_id` | `SK-V9-open`, `SK-V10-W2`, `SK-V10-W6`. | Add W1a/W1b/W2 wave ids only in the same wave that updates the validator. |
| `run_id` | `sk-v9-open:criterion-fnv64-<16 lowercase hex>` and uniform within the report. | Define a uniform SK-V11 run-id prefix before accepting non-JSON rows or companion reports. Do not splice mixed run ids. |
| `sk_v9_open_delta` | `baseline`, `direct-reclaimed`, `typed-row-added`. | Use an existing field value or a SPEC-named SK-V11 delta token; do not add a new delta column. |
| `profile_artifact` / `hot_leaf` | Criterion path plus row id. | Point to the generated non-JSON Track 1 benchmark artifact and row id in the existing fields. |
| `sample_cost` / `sample_count` | `ns_per_byte=...;track1_ns=...;bytes=...`, count > 0. | Keep the same structured sample-cost shape and nonzero sample count for non-JSON rows. |
| `build_flags` / `host_triple` / `feature_mask` | Structured bench profile, host, and feature facts. | Reuse current structured fields; W1a fixtures should reject missing host or feature data. |
| `diagnostic_nonproducer_status` | PMU/cycles/scan/probe artifacts marked nonproducer. | Keep diagnostic artifacts non-producing unless a same-wave SPEC/gate revision promotes a measured field. |

## Non-column Implementation Boundary

The W1a gate/report lane can be implemented without new columns if it treats
non-JSON evidence as a new allowed-value branch over the same identifiers:

1. Main table keeps the 26-column `SCHEMA_V3_HEADER`.
2. Manifest keeps the 22 rendered fields and the folded `CostFacts` and
   `Comparator evidence` cells.
3. Comparator/oracle identity, output plane, freshness, Mbps, and source path
   stay in `SkV8ComparatorEvidence`.
4. Metadata TOML remains schema version `3`; non-JSON facts are existing field
   values, not new keys.
5. A companion non-JSON report is acceptable only if its gate command consumes
   the same semantic identifiers in the same wave and carries no admission
   semantics outside the validator.

Self-verdict: ACCEPT as read-only W1a R2 research. No source behavior, report
code, metadata code, results table, or gate code is edited by this artifact.
