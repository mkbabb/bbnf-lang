# SK-V10 P3-D: Telemetry Schema Binding

Pass: S-P3 Synthesis-Plan. Cycle: V1.
Date: 2026-05-19.
Scope: bind the SK-V10 telemetry schema, outcome enum, gate-json rejection
rules, run-id and comparator evidence, and new-column policy before any SK-V10
source wave dispatch.
Output: this file.

## Section 1 - Authority

This is a binding plan, not a source plan. It edits no runtime, benchmark, or
gate source. SK-V10 source implementation remains blocked until S-P3 writes the
measurable `SPEC.md` and `DISPATCH-PROMPT.md`.

Authorities read for this binding:

- `restart/audit/pass-3-runtime/PASS-3.md`
- `restart/skinny/tranches/sk-v9/SPEC.md`, especially the telemetry section
- `restart/skinny/tranches/sk-v9/research/p3/skv9-p3-D-telemetry-schema.md`
- `restart/skinny/tranches/sk-v10/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v10/HANDOFF.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2g-candidate-ledger.md`
- `skinny/RESULTS.md`
- `docs/benchmarks/SPEC.md`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/gate.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`

## Section 2 - Binding Verdict

SK-V10 inherits the SK-V9 telemetry schema unchanged. The bound schema is the
same 36-identifier, gate-consumed set from SK-V9 SPEC Section 0.y and
SK-V9 P3-D Section 2.2. It is realized as the existing schema-v3 rendered
report table plus the SK-V9 W0 telemetry manifest fields consumed by
`gate-json`.

No SK-V10 P3-D column is added. C12 telemetry refresh is gate-only: it may
refresh values, run ids, comparator evidence, sidecar freshness, REDRESS
anchors, and per-wave metadata inside the existing fields, but it cannot move a
behavior row by itself and cannot emit a producer-only field.

The current result authority is the W1-rendered `SK-V9-open` snapshot in
`skinny/RESULTS.md`: 17 `parse_only` rows, 17 `direct_to_struct` rows, and
6 `real_typed_struct` rows. The current uniform run id in that file is
`sk-v9-open:criterion-fnv64-a1e8a51ae806d386`.

## Section 3 - Required Schema

The required SK-V10 telemetry identifiers are exactly:

```text
row_id            grammar_id        domain            corpus
workload          outcome_id        verdict           strictness
output_plane      track1_mbps       track2_mbps       comparator_id
comparator_plane  comparator_strictness                comparator_freshness
measured_validation_path             profile_artifact  sample_cost
sample_count      build_flags       host_triple       feature_mask
costfacts_rule_id costfacts_chosen_shape               costfacts_rejected_alternative_ids
redress_entry     wave_id           run_id            sidecar_freshness
sk_v9_open_delta  substrate_surface structural_projection_status
substrate_cardinality                same_wave_consumer_class
track2_independence_status           diagnostic_nonproducer_status
```

Binding by field group:

| Field group | SK-V10 binding |
|---|---|
| Row identity | `row_id`, `grammar_id`, `domain`, `corpus`, and `workload` identify the fixture and output plane under measurement. Current rows remain `json` / `json_bench`; non-JSON proof belongs to totality unless a later SPEC names a gate. |
| Outcome and verdict | `outcome_id`, `verdict`, and `strictness` must be gate-derived or gate-validated. Prose cannot override the outcome enum. |
| Measured throughput | `track1_mbps` and `track2_mbps` are the only row-moving throughput fields. PMU, cycles, masking probes, structural scans, and Criterion slope artifacts remain diagnostic non-producers. |
| Comparator evidence | `comparator_id`, `comparator_plane`, `comparator_strictness`, `comparator_freshness`, and `sidecar_freshness` bind every comparator cell used for a delta or admission claim. |
| Validation path | `measured_validation_path` must become `measured-row` for strict admission. Current deferred/view-boundary rows are telemetry or guard evidence, not strict SOTA claims. |
| Profile and host evidence | `profile_artifact`, `sample_cost`, `sample_count`, `build_flags`, `host_triple`, and `feature_mask` bind rows to the same host and benchmark setup. |
| CostFacts | `costfacts_rule_id`, `costfacts_chosen_shape`, and `costfacts_rejected_alternative_ids` remain required-present. They stay `none:pre-W1` unless a wave proves a CostFacts-driven shape choice and `gate-json` consumes it in that wave. |
| Routing | `redress_entry`, `wave_id`, `run_id`, and `sk_v9_open_delta` bind every movement or rejection to a wave, a run, and a routed remainder. |
| Substrate and consumer evidence | `substrate_surface`, `structural_projection_status`, `substrate_cardinality`, and `same_wave_consumer_class` describe where the measured row consumed the change. REDRESS 98 retires W3; no SK-V10 wave may use these fields to relabel W3 as a live consumer. |
| Independence and non-producers | `track2_independence_status` must stay independent or explicitly untouched. `diagnostic_nonproducer_status` is fixed to `structural_scan+masking_probes+pmu+cycles:nonproducer`. |

The schema version remains `schema-v3 / SK-V9-open` for the inherited opening
snapshot. Behavior waves do not bump schema-v3 unless they add or remove a
column, and this P3-D authorizes no such bump.

## Section 4 - Outcome Enum

The SK-V10 telemetry outcome enum is the same 10-identifier W0-admissible set
that SK-V9 bound:

```text
A   C   G   I   J   K   L   M   N-direct   S
```

Meanings:

| Outcome | Meaning |
|---|---|
| `A` | beat-and-parity; eligible for `GO` only when strict-admission evidence also passes. |
| `C` | substrate-parity-codegen acceptable; GO-without-beat band. |
| `G` | substrate failure. |
| `I` | parity-oracle disagreement. |
| `J` | invalid-input schema rejection. |
| `K` | SIMD parity-hash fail; checkasm differential failure. |
| `L` | SIMD throughput fail. |
| `M` | memory-residency fail. |
| `N-direct` | direct-projection failure or direct digest guard miss. |
| `S` | substrate-guard non-admission; current parse-only SOTA demotion target. |

No new SK-V10 outcome variant is allowed by P3-D. `B`, `D`, `E`,
`F-positive`, and `F-noise` remain code-defined dormant variants, but they are
not SK-V10 telemetry-admissible unless a later SPEC and same-wave gate change
explicitly re-admit them.

## Section 5 - Gate-Json Rejection Rules

`gate-json` must fail closed for all of the following:

| Rejection | Rule |
|---|---|
| `missing_required_field` | Any empty or absent required schema-v3 or manifest field rejects the row. |
| `duplicate_or_unknown_row_id` | Duplicate row ids, unknown row ids, or row ids not matching the fixture/workload identity reject the manifest. |
| `unsupported_outcome` | Any `outcome_id` outside `A C G I J K L M N-direct S` rejects SK-V10 telemetry. |
| `non_uniform_run_id` | Rows in one refreshed report cannot carry mixed run ids. |
| `invalid_run_id` | A run id must match the gate-accepted grammar for the wave. The inherited opening grammar is `sk-v9-open:criterion-fnv64-<16 lowercase hex>`. |
| `missing_profile_or_sample` | Missing profile artifacts, missing `ns_per_byte` sample cost, or `sample_count=0` reject. |
| `producer_only_telemetry` | Any rendered field, manifest field, comparator field, sidecar field, PMU field, or profile field not consumed by `gate-json` in the same wave rejects the wave. |
| `diagnostic_nonproducer_mismatch` | `diagnostic_nonproducer_status` must equal `structural_scan+masking_probes+pmu+cycles:nonproducer`. Any other value rejects. |
| `track2_coupling` | Track 2 evidence cannot call or depend on generated Track 1 unless a SPEC explicitly owns that proof. |
| `stale_or_absent_strict_anchor` | Historical, stale, absent, sidecar-only, or comparator-only evidence cannot support strict admission. |
| `strict_plane_mismatch` | Strict admission rejects when normalized row output plane and comparator plane differ. |
| `deferred_validation_admission` | Strict admission rejects when row strictness is not `strict`, `parse_utf8` is not `measured-row`, `escape_complete` is not `yes`, or `measured_validation_path` is not `measured-row`. |
| `wrong_strict_comparator` | Strict admission accepts only same-run native `sonic_rs_strict` or `serde_json` anchors on the matching output plane. |
| `parse_only_sota_claim` | Current `parse_only` rows remain `S / NO-GO` planning evidence; they cannot close a SOTA target while validation remains deferred/view-boundary. |
| `direct_digest_as_typed` | Direct digest rows cannot be relabeled as typed product proof. |
| `w3_reopen` | W3 union/event substrate, class-lane-only, renamed union substrate, or W4-through-W3 consumer claims reject under REDRESS 98. |

The same-wave consumption rule is binding: every field emitted into
`skinny/RESULTS.md` must be read by `validate_schema_v3`,
`validate_sk_v8_w0`, `validate_strict_admission`, or the wave's same-commit
gate extension. There is no emit-now-consume-later route.

## Section 6 - Run-Id And Comparator Evidence

Run-id binding:

1. The inherited opening snapshot uses `sk-v9-open:criterion-fnv64-<16 hex>`;
   current RESULTS use `sk-v9-open:criterion-fnv64-a1e8a51ae806d386`.
2. Any SK-V10 row-moving wave must refresh the report to one fresh same-run
   run id and route the old/new delta through `sk_v9_open_delta`.
3. If a SK-V10 wave wants a new `sk-v10-*` run-id prefix, the same wave must
   update the gate validator and fixtures. Until that happens, the inherited
   `sk-v9-open:` grammar is the only accepted grammar.
4. Mixed valid-looking run ids in a single report reject. A row movement cannot
   splice old bbnf rows with new comparator rows by prose.

Comparator evidence binding:

| Comparator class | SK-V10 use |
|---|---|
| Same-run native strict | `sonic_rs_strict` and `serde_json` may support strict admission only when `comparator_freshness=same-run-native`, `sidecar_freshness=n/a`, `comparator_strictness=strict`, the source artifact is the expected Criterion path, and the comparator plane matches the row output plane. |
| Same-run flaw probe | `sonic_rs_lossy` is planning evidence only. It stays `permissive`, same-run native, and parse-only-scoped; it never anchors strict admission. |
| Sidecar planning signal | `simdjson_dom`, `simdjson_ondemand`, `yyjson_default`, `asmjson_swar`, `asmjson_avx512`, and `rapidjson_default` are planning signals unless a future same-run structured sidecar manifest is gate-consumed. Historical or absent sidecars never anchor strict admission. |
| Independent oracle | Track 2 is correctness/oracle evidence. It does not prove Track 1 took the same path and does not replace comparator evidence. |

New typed product rows require generated typed Track 1, independent Track 2 or
oracle, serde_json typed, sonic typed, checksum or full-fixture parity, same-run
Criterion rows, matching output plane, and a same-run run id. New direct rows
use direct-plane comparator evidence only; direct digest movement does not
admit typed rows.

## Section 7 - Producer-Only Telemetry Is Forbidden

Producer-only telemetry remains forbidden in SK-V10.

Disallowed examples:

- a new PMU or `cycles_per_byte` column;
- a structural-scan, masking-probe, or Criterion-slope value used as Track 1,
  Track 2, typed product, direct product, or strict-admission evidence;
- a sidecar manifest printed but not gate-consumed;
- a new CostFacts field printed without a same-wave validator;
- a new `same_wave_consumer_class` value whose producer and consumer do not
  both land and measure in the same wave;
- any SK-V10 telemetry refresh that changes values in `RESULTS.md` while
  `gate-json` ignores the changed evidence.

Diagnostic artifacts may inform S-P3 planning and falsifiability thresholds.
They do not become producers unless a later SPEC makes them measured-row
evidence and `gate-json` consumes them in the same wave. This P3-D authorizes
no such promotion.

## Section 8 - New Columns

No new columns are allowed for SK-V10 under this P3-D binding.

Allowed:

- add new rows only when a row-gated wave supplies every required field and the
  existing gate consumes them;
- refresh values inside the existing 36 identifiers;
- tighten allowed values or rejection rules if the same wave updates
  `gate-json` and fixtures;
- add a new run-id prefix only with a same-wave gate update.

Rejected:

- a 37th required identifier;
- a new rendered table column;
- a manifest column that `gate-json` does not read;
- a validator requirement for a field that the report does not emit;
- a new outcome variant.

If a later S-P3 `SPEC.md` proposes a schema change despite this binding, that
SPEC must explicitly supersede P3-D, update `gate-json` in the same wave, update
all fixture expectations, and explain why the existing 36 identifiers cannot
carry the evidence. Without that supersession, any new column is
producer-only telemetry and fails the exit gate.

## Section 9 - Self-Verdict

ACCEPT.

Reason: this binds SK-V10 to the SK-V9 gate-consumed 36-identifier schema,
keeps the 10-outcome enum, preserves same-run comparator and run-id evidence,
hard-rejects producer-only telemetry, and allows no new columns before a
deliberate same-wave `gate-json` schema change.
