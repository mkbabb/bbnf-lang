# SK-V11 P3-D: Telemetry-Schema Binding

Pass: S-P3 Synthesis-Plan. Cycle: V3.
Date: 2026-05-20.
Scope: bind the SK-V11 RESULTS schema, gate-json consumption, non-JSON row
policy, and producer-only rejection rules before any SK-V11 wave dispatch.
Output: this file.
Pass Alpha goalset: direct plane closure or measured direct fixpoint, existing
typed/direct GO guards maintained, parse-only closed as diagnostic, one
benchmarked non-JSON generated direct/typed intervention, and no producer-only
telemetry.
Candidate pool: research/p2/ post-CHALLENGE survivors.

## §1 — Synthesis (concrete; cites P1 row, P2 candidate, REDRESS entry, or goalset line)

P3-D is a schema binding, not source authority. S-P3 remains read-only against
`skinny/` source, and behavior work belongs only to later wave redress phases.

SK-V11 inherits the SK-V8 required-telemetry discipline from
`restart/skinny/tranches/sk-v8/SPEC.md` Section 0.4: the rendered
`skinny/RESULTS.md` schema-v3 table may stay as the live table shape, but every
required field must be emitted and consumed by `gate-json` in the same wave.
The P3 prompt still uses the older "24-column" shorthand; the live schema-v3
surface is the 26-column `SCHEMA_V3_HEADER` in
`skinny/crates/bbnf-bench/src/report.rs`, and SK-V8 P3-D already treats the
24-column wording as shorthand for that schema-v3 surface.

The current implementation is two-layered:

1. Schema-v3 main table: 26 rendered columns in `SCHEMA_V3_HEADER`, validated
   by `Report::validate_schema_v3`.
2. SK-V9 W0 telemetry manifest: the `## SK-V9 W0 Telemetry Manifest` block
   rendered by `Report::render_markdown`, backed by `SkV8Telemetry` and
   `SkV8ComparatorEvidence`, validated by `Report::validate_sk_v8_w0`.

The canonical SK-V11 telemetry set is the carried-forward schema-v3
gate-consumed identifier set from SK-V9/SK-V10 P3-D: the union of the rendered
schema fields, manifest fields, and comparator-evidence fields. SK-V11 adds no
required column at P3-D. It adds only allowed-value obligations for non-JSON
rows if P3-B/P3-F choose to put CSS L4, Sheets, or BBNF-self rows into
`skinny/RESULTS.md`; those obligations require a same-wave gate extension, not
a producer-only schema fork.

Load-bearing S-P1/S-P2 facts carried into this binding:

- Direct residual rows are the primary JSON closure surface; `instruments`,
  `numbers`, and `unicode_mixed` remain W0-clamped non-admissions until a
  behavior wave measures them.
- `parse_only` is diagnostic only and cannot close SK-V11.
- C1-C7 are parser primitive candidates; C8 is output oracle/host sink only;
  C9 is Lock-1/output-plane accounting only.
- W3 union/event/class-column/streaming-cursor repair remains REDRESS-closed.
- Non-JSON generality must be measured through a generated direct/typed parser;
  prose or JSON-only telemetry is insufficient.

## §2 — Deliverable (the shortlist / sequence / gate set / schema / ledger / SPEC section)

### §2.1 Rendered schema-v3 surface

SK-V11 preserves the live rendered main table shape:

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

No SK-V11 behavior wave may add, remove, or rename one of these columns unless
that same wave updates `gate-json`, fixture expectations, and this schema
binding through CHALLENGE. A new rendered column without a validator read is a
producer-only field and fails the wave.

### §2.2 Gate-consumed required identifiers

The required SK-V11 identifiers remain:

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

The list above names comparator `value_mbps` and `source_artifact` explicitly
because they are real `SkV8ComparatorEvidence` fields even though the rendered
manifest folds them into the single Comparator evidence cell. The gate may
render fewer physical columns than identifiers; the invariant is that each
identifier is reconstructable and validator-consumed.

### §2.3 SK-V11 additions

No new SK-V11 identifier is required for the JSON direct-plane gates.

If a non-JSON grammar row is rendered in `skinny/RESULTS.md`, the same wave
must extend the gate to accept these values inside existing fields:

| Existing field | SK-V11 allowed-value extension |
|---|---|
| `row_id` | `<grammar_id>/<corpus>/<workload>/main`; JSON remains `json/<corpus>/<workload>/main`. |
| `grammar_id` | `json`, `css_l4`, `sheets`, or `bbnf_self`. |
| `domain` | `json_bench`, `css_l4_bench`, `sheets_bench`, or `bbnf_self_bench`. |
| `workload` | JSON workloads plus a SPEC-named non-JSON generated direct or typed workload. |
| `output_plane` / `comparator_plane` | `digest`, `typed direct`, or a SPEC-named non-JSON direct/typed plane. |
| `comparator_id` | Same-run native comparator when one exists; otherwise a SPEC-named independent oracle such as `internal_oracle`, with source artifact and plane. |
| `track2_independence_status` | `independent_verified` or a SPEC-named equivalent oracle proof; coupled Track 2 stays forbidden. |
| `same_wave_consumer_class` | The generated non-JSON direct/typed parser consumer, not `gate_only`, for the admitted non-JSON behavior wave. |

If P3-F instead chooses a companion non-JSON report, it must name the gate
command and carry the same required identifiers or an explicitly smaller set
with no row-admission semantics hidden outside the validator. A companion report
cannot close SK-V11 unless its gate is run in the same wave and its admitted row
is reconciled into `SPEC.md`, `HANDOFF.md`, and `REDRESS.md`.

### §2.4 Outcome enum

The SK-V11 telemetry enum remains the W0-admissible set:

```text
A
C
G
I
J
K
L
M
N-direct
S
```

No SK-V11 P3-D outcome variant is added. `B`, `D`, `E`, `F-positive`, and
`F-noise` remain dormant code variants and are not SK-V11 telemetry-admissible
unless a later SPEC revision and same-wave gate change explicitly admit them.

### §2.5 Gate-json rejection rules

`gate-json` must fail closed for these SK-V11 cases:

| Rejection | Binding rule |
|---|---|
| `missing_required_column` | Any absent or empty schema-v3 main-table field required by `validate_schema_v3` rejects. |
| `missing_required_manifest_field` | Any absent or empty manifest field required by `validate_sk_v8_w0` or a same-wave SK-V11 extension rejects. |
| `producer_only_field` | Any rendered table column, manifest cell, comparator field, sidecar field, profile field, PMU/cycles field, or non-JSON field not read by `gate-json` in the same wave rejects. |
| `validator_only_field` | A validator requirement for a field the report does not emit rejects; schema and gate must move together. |
| `unsupported_outcome` | Any outcome outside `A C G I J K L M N-direct S` rejects. |
| `duplicate_or_unknown_row_id` | Duplicate row ids, unknown row ids, or a row id whose grammar/corpus/workload does not match the rendered row reject. |
| `invalid_or_mixed_run_id` | Opening rows use the accepted `sk-v9-open:criterion-fnv64-<16 lowercase hex>` grammar until a same-wave validator change admits a new prefix; one report cannot splice mixed run ids. |
| `missing_profile_or_sample` | Missing profile artifact, missing `ns_per_byte` sample cost, or `sample_count=0` rejects. |
| `stale_or_absent_strict_anchor` | Historical, stale, absent, sidecar-only, or comparator-only evidence cannot support strict admission. |
| `strict_plane_mismatch` | Strict admission rejects when normalized row output plane and comparator plane differ. |
| `deferred_validation_admission` | Strict admission rejects when row strictness is not `strict`, `parse_utf8` is not `measured-row`, `escape_complete` is not `yes`, or `measured_validation_path` is not `measured-row`. |
| `wrong_strict_comparator` | JSON strict admission accepts only same-run native `sonic_rs_strict` or `serde_json` anchors on the matching output plane. |
| `non_json_oracle_unconsumed` | Non-JSON admission rejects unless the comparator/oracle identity, source artifact, output plane, and independence proof are gate-consumed in that same wave. |
| `diagnostic_nonproducer_mismatch` | `diagnostic_nonproducer_status` must keep PMU, cycles, structural-scan, masking-probe, and Criterion-slope artifacts as non-producers unless a same-wave SPEC/gate revision promotes a measured field. |
| `parse_only_sota_claim` | `parse_only` rows remain diagnostic `S/L / NO-GO` and cannot close SK-V11. |
| `direct_digest_as_typed` | Direct digest evidence cannot admit or maintain a typed product row. |
| `w3_reopen` | Union/event/class-column/streaming-cursor/class-lane/sidecar substrate claims reject under REDRESS 96/97/98 and the SK-V11 goalset. |

The same-wave consumption rule is the central invariant: every field emitted
into `skinny/RESULTS.md` must be consumed by `validate_schema_v3`,
`validate_sk_v8_w0`, `validate_strict_admission`, or the wave's same-commit gate
extension. There is no emit-now-consume-later route.

## §3 — Falsifiability binding (named corpus rows + Mbps thresholds)

P3-D does not set row thresholds; P3-C owns that. P3-D binds the fields that
make P3-C's thresholds executable.

The direct residual rows require `track1_mbps`, `track2_mbps`, matching-plane
`sonic_rs_strict` comparator evidence, `measured_validation_path`, run id,
profile artifact, Track 2 independence, REDRESS entry, and same-wave consumer
class. The SK-V11-open seed floors are:

| Row | Seed floor Mbps |
|---|---:|
| `twitter/direct_to_struct` | 13740 |
| `canada/direct_to_struct` | 10637 |
| `github_events/direct_to_struct` | 13403 |
| `update_center/direct_to_struct` | 10059 |
| `mesh/direct_to_struct` | 8675 |
| `random/direct_to_struct` | 7878 |
| `gsoc-2018/direct_to_struct` | 3737 |
| `instruments/direct_to_struct` | 8969 |
| `numbers/direct_to_struct` | 2425 |
| `unicode_mixed/direct_to_struct` | 2588 |
| `unicode_escapes/direct_to_struct` | 3441 |
| `distinct_values/direct_to_struct` | 2658 |
| `y_string_unicode/direct_to_struct` | 3950 |

Existing guard rows remain schema-bound:

- 7 `real_typed_struct A / GO` rows: `twitter`, `citm_catalog`,
  `apache_builds`, `github_events`, `update_center`, `mesh`, `marine_ik`.
- 4 `direct_to_struct A / GO` rows: `citm_catalog`, `apache_builds`,
  `marine_ik`, `unicode_basic`.

The non-JSON close axis requires at least one generated direct/typed row whose
grammar id, workload, comparator/oracle, Track 1, Track 2 or independent
oracle, profile artifact, strict output proof, and same-wave consumer are
gate-consumed. A non-JSON row with only prose Lock 14 evidence is a schema
failure as well as a close failure.

## §4 — Pre-blocked routes (REDRESS entries each wave must NOT re-open)

P3-D blocks telemetry relabeling for these routes:

- W3 union/event/class-column/streaming-cursor/class-lane/sidecar substrate
  repair, including any retained structural-position vector hidden behind a
  schema value.
- Parse-only SOTA movement or parse-only rows counted as close evidence.
- PMU, cycles, structural scan, masking probes, Criterion slope, sidecars, or
  lazy-tape facts used as Track 1, Track 2, strict comparator, direct product,
  typed product, or non-JSON product evidence.
- Direct digest evidence as typed product proof.
- Sidecar same-run claims without a structured same-wave manifest parser and
  gate.
- Generic-crate JSON policy hidden in `grammar_id`, `domain`, workload, or
  output-plane values.
- New outcome variants, new report columns, or new manifest fields without a
  same-wave gate consumer.
- x86 implementation evidence as SK-V11 admission.

## §5 — Sources (every upstream artefact cited)

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` - P3-D scope,
  frontmatter, schema binding, and producer-only rule.
- `restart/skinny/tranches/sk-v8/SPEC.md` - Section 0.4 required telemetry and
  same-wave gate consumption rule.
- `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md` -
  treatment of the 24-column shorthand versus the live 26-column schema-v3
  header.
- `restart/skinny/tranches/sk-v9/research/p3/skv9-p3-D-telemetry-schema.md` -
  schema-v3 gate-consumed identifier set and outcome enum binding.
- `restart/skinny/tranches/sk-v10/research/p3/p3d-telemetry-schema.md` -
  SK-V10 carry-forward schema and producer-only rejection rules.
- `restart/skinny/tranches/sk-v11/SYNTHESIS.md` - Section 0 close condition,
  direct residual floors, guard rows, grammar generalization goal, and telemetry
  binding.
- `restart/skinny/tranches/sk-v11/HANDOFF.md` - ready-for-S-P3 status,
  residual direct rows, accepted S-P2 pool, and refusal conditions.
- `restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
  - accepted profile surface and diagnostic-only facts.
- `restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
  - accepted candidate pool and S-P3 load-bearing facts.
- `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md`,
  `p2d-substrate-tape.md`, and `p2f-grammar-neutral.md` - comparator,
  substrate, non-JSON, oracle, and accounting boundaries.
- `skinny/RESULTS.md` - live schema-v3 main table and SK-V9 W0 telemetry
  manifest.
- `skinny/crates/bbnf-bench/src/report.rs` - `SCHEMA_V3_HEADER`,
  `SkV8Telemetry`, `SkV8ComparatorEvidence`, `validate_schema_v3`,
  `validate_sk_v8_w0`, and markdown manifest rendering.
- `skinny/crates/bbnf-bench/src/gate.rs` - outcome enum and
  `validate_strict_admission`.
- `skinny/crates/bbnf-bench/src/bin/gate.rs` and `skinny/xtask/src/main.rs` -
  `gate-json`, `--check-results`, run-id, manifest, and CostFacts gate
  behavior.

Self-verdict: ACCEPT for V3 challenge. No source files edited; no telemetry
column added.
