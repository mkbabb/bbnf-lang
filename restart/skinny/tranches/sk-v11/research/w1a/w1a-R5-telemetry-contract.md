# SK-V11 W1a R5 Telemetry/Gate Contract

Pass: W1a Phase 1 Research.
Scope: read-only validation contract for `G-W1a-NONJSON-GATE`.
Owned path: `restart/skinny/tranches/sk-v11/research/w1a/w1a-R5-telemetry-contract.md`.
Source edits: none.

## Authorities

- `restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md`
- `restart/skinny/tranches/sk-v11/SPEC.md` sections 0.3, 1, 2.1, and 4
- `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md` non-negotiable
  falsifiability, pre-block, telemetry, and outcome discipline

## Contract Summary

W1a adds the gate/report lane that lets later waves carry non-JSON evidence
without weakening JSON `gate-json`. It does not move parser rows, create a
generated non-JSON baseline authority, or admit behavior. The implementable
rule is fail-closed validation: every emitted field must be reconstructable,
validator-consumed in the same wave, and tied to a row identity, comparator or
oracle, independent Track 2/oracle, run, host, feature mask, and same-wave
consumer class.

Existing JSON rows must continue to pass with the schema-v3 identifier set.
Non-JSON evidence may pass only as a gate-consumed report or future row shape
whose admission semantics are explicit in the validator. Prose-only Lock 14
evidence, producer-only fields, and gate-only non-JSON close claims reject.

## Gate Input Invariants

1. The validator has a strict schema for every accepted report shape.
2. Unknown rendered columns, manifest keys, comparator keys, sidecar keys,
   profile keys, or companion-report fields reject before admission checks.
3. Folded display cells are allowed only when each required identifier is
   reconstructable and consumed by a validator predicate.
4. A companion non-JSON report must name the gate command and carry either the
   full schema-v3 semantic set or an explicitly smaller non-admitting set. No
   row-admission fact may live outside the gate.
5. The outcome enum remains exactly `A C G I J K L M N-direct S`.
6. `skinny/RESULTS.md` remains unchanged by W1a; JSON `gate-json
   --with-cost-facts --check-results` remains green.

## Validation Rules

### R5-01 Grammar Identity

Required fields: `row_id`, `grammar_id`.

Accept when:

- `grammar_id` is exactly one of `json`, `css_l4`, `sheets`, or `bbnf_self`.
- `row_id` has the grammar prefix `<grammar_id>/.../main`.
- JSON rows keep the existing form `json/<corpus>/<workload>/main`.
- Non-JSON rows use `<grammar_id>/<corpus>/<workload>/main` and the grammar is
  explicitly registered by the same-wave validator or fixture set.

Reject when:

- `grammar_id` is missing, empty, unknown, mixed-case, or free text.
- `row_id` grammar and `grammar_id` disagree.
- A non-JSON row is disguised under the `json` grammar prefix.
- Duplicate or unknown row ids appear in one report.

Primary failure class: `duplicate_or_unknown_row_id` or
`missing_required_manifest_field`.

### R5-02 Domain Binding

Required fields: `grammar_id`, `domain`.

Accept only this mapping:

| `grammar_id` | `domain` |
|---|---|
| `json` | `json_bench` |
| `css_l4` | `css_l4_bench` |
| `sheets` | `sheets_bench` |
| `bbnf_self` | `bbnf_self_bench` |

Reject when:

- `domain` is missing, generic, or not the exact mapped value.
- A non-JSON grammar uses `json_bench`.
- The same report mixes domain policy for the same grammar.

Primary failure class: `missing_required_manifest_field`.

### R5-03 Output Plane

Required fields: `output_plane`, `comparator_plane`, `strictness`.

Accept when:

- The plane value is validator-known. JSON keeps the existing digest or typed
  direct planes. Non-JSON planes must be SPEC-named direct or typed product
  planes registered by the same-wave gate extension.
- Strict or row-admitting evidence has `output_plane == comparator_plane`.
- Digest evidence is used only for digest-plane direct rows.

Reject when:

- Either plane is missing, unknown, or present only in prose.
- Strict admission has a row/comparator plane mismatch.
- Digest evidence is relabeled as typed product proof.
- Parse-only evidence is treated as SK-V11 SOTA admission.

Primary failure classes: `strict_plane_mismatch`,
`direct_digest_as_typed`, or `parse_only_sota_claim`.

### R5-04 Comparator Or Oracle

Required fields: `comparator_id`, `comparator_plane`,
`comparator_strictness`, `comparator_freshness`, `comparator_value_mbps`,
`comparator_source_artifact`, `measured_validation_path`.

Accept when:

- JSON strict direct or typed admission uses a same-run native comparator on the
  matching plane: `sonic_rs_strict`, or `serde_json` only where the output
  plane and strictness match the validator's existing JSON rule.
- Non-JSON evidence uses either a same-run native comparator or a SPEC-named
  independent oracle such as `internal_oracle`.
- The comparator or oracle identity, source artifact, plane, strictness,
  freshness, and value or oracle status are read by the gate in the same wave.
- `measured_validation_path` is `measured-row` for strict row admission.

Reject when:

- Comparator evidence is absent, stale, historical, sidecar-only, or not tied
  to the same run.
- The comparator is wrong for the output plane.
- A non-JSON oracle exists only as prose, lacks a source artifact, or is not
  consumed by the gate.
- Validation is deferred beyond the admitting wave.

Primary failure classes: `wrong_strict_comparator`,
`stale_or_absent_strict_anchor`, `deferred_validation_admission`, or
`non_json_oracle_unconsumed`.

### R5-05 Track 2 Or Oracle Independence

Required fields: `track2_mbps`, `track2_independence_status`,
`comparator_source_artifact` or a same-wave consumed oracle source path.

Accept when:

- JSON direct/typed rows carry measured Track 2 throughput and
  `track2_independence_status = independent_verified`.
- Non-JSON row movement carries either measured Track 2 throughput or a
  same-output independent oracle that the validator treats as the Track 2
  substitute.
- The gate names the Track 2/oracle source path.
- The Track 2/oracle source does not call generated Track 1, generated
  SinkOnly helpers, generated typed helpers, or hidden benchmark-private parser
  code.

Reject when:

- `track2_independence_status` is missing, coupled, unknown, or gate-only.
- Track 2 calls or reuses generated Track 1.
- The oracle source path is hidden in prose or outside the validated report.
- A close or admission claim has Track 1 only.

Primary failure class: `track2_coupling` or `non_json_oracle_unconsumed`.

### R5-06 Run Id

Required field: `run_id`.

Accept when:

- Every row in one report has one uniform `run_id`.
- The SK-V11 opening JSON rows retain
  `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`.
- Any new same-wave non-JSON report uses a validator-known run-id grammar. Until
  such a grammar is admitted, the accepted opening pattern is
  `sk-v9-open:criterion-fnv64-<16 lowercase hex>`.
- Comparator freshness and source artifacts refer to the same run.

Reject when:

- `run_id` is missing, malformed, mixed across one report, or silently changed
  by a companion report.
- Comparator evidence comes from a different run while claiming same-run
  strict admission.

Primary failure class: `invalid_or_mixed_run_id`.

### R5-07 Host And Build Context

Required fields: `host_triple`, `build_flags`.

Accept when:

- `host_triple` is present, parseable, and uniform for the measured run.
- SK-V11 opening JSON rows retain
  `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
- `build_flags` is present and carries the measured compiler/runtime flags,
  including the W0 `RUSTFLAGS="-C target-cpu=native"` context for opening rows.
- Any implementation evidence remains on the SK-V11 aarch64 Apple Silicon
  target. x86 may appear only as comparator context, never implementation
  authority.

Reject when:

- Host is missing, malformed, or mixed inside one run.
- A row claims same-host measurement while comparator or Track 2 evidence comes
  from a different host.
- x86 implementation evidence is used as SK-V11 admission authority.

Primary failure class: `missing_required_manifest_field`.

### R5-08 Feature Mask

Required field: `feature_mask`.

Accept when:

- `feature_mask` is present and parsed as a validator-known token set.
- Scalar-only rows use an explicit token such as `scalar` or `none`, not an
  empty cell.
- Any SIMD/ASM or generic behavior evidence names the required feature gate and
  has a scalar or no-op fallback recorded by the same-wave gate.
- The mask is compatible with `build_flags` and `host_triple`.

Reject when:

- The mask is missing, empty, free text, or not consumed by the validator.
- The mask names x86 implementation features as SK-V11 production evidence.
- A feature-gated primitive lacks scalar/no-op fallback evidence.

Primary failure class: `missing_required_manifest_field`.

### R5-09 Same-Wave Consumer Class

Required field: `same_wave_consumer_class`.

Accept when:

- The value is validator-known and names the same-wave consumer of the emitted
  evidence.
- Existing JSON rows keep their schema-v3 consumer semantics.
- W1a non-JSON fixtures may name only non-admitting gate/report consumer
  classes.
- Any later admitted non-JSON behavior row names the generated non-JSON direct
  or typed parser consumer. `gate_only` is not sufficient for row movement.

Reject when:

- The class is missing, unknown, or not consumed by the validator.
- A non-JSON close/admission claim uses `gate_only`, documentation-only Lock 14,
  or a producer without a measured caller and row gate.
- A primitive, SIMD kernel, generated path, codegen shape, or host sink ships
  without scalar/oracle, parity when applicable, same-host microbench when
  required, same-wave consumer, and measured gate.

Primary failure class: `producer_only_field` or
`missing_required_manifest_field`.

### R5-10 Producer-Only Rejection

Required fields: all emitted fields, plus `diagnostic_nonproducer_status`.

Accept when:

- Every rendered field maps to a same-wave validator predicate.
- Diagnostic artifacts are explicitly classified by
  `diagnostic_nonproducer_status`.
- PMU, cycles, structural-scan, masking-probe, Criterion-slope, sidecar, and
  lazy-tape artifacts remain diagnostic unless a same-wave SPEC and gate change
  promotes a measured field.

Reject when:

- A rendered table column, manifest cell, comparator field, sidecar field,
  profile field, PMU/cycles field, or non-JSON field is not read by the same
  wave gate.
- The validator requires a field that the report does not emit.
- Diagnostic non-producers are used as Track 1, Track 2, comparator, direct
  product, typed product, or non-JSON product evidence.
- A report emits now and promises to consume later.

Primary failure classes: `producer_only_field`,
`validator_only_field`, or `diagnostic_nonproducer_mismatch`.

## Fixture Obligations For W1a

The W1a gate fixture set should include at minimum:

| Fixture | Expected result |
|---|---|
| Existing JSON schema-v3 report with W0 run id, host, build flags, feature mask, and current consumer fields | pass |
| Non-JSON companion report carrying registered `css_l4`, `sheets`, or `bbnf_self` grammar with no row-admission semantics | pass |
| Unknown or mismatched `grammar_id` and `row_id` prefix | fail |
| Domain mismatch, including non-JSON grammar under `json_bench` | fail |
| Missing or unknown `output_plane` | fail |
| `output_plane` and `comparator_plane` mismatch for strict admission | fail |
| Non-JSON oracle with missing source artifact or unconsumed oracle identity | fail |
| Coupled Track 2 or source path that calls generated Track 1 | fail |
| Missing, malformed, or mixed `run_id` | fail |
| Missing or mixed `host_triple` | fail |
| Missing, empty, or unparsed `feature_mask` | fail |
| `same_wave_consumer_class = gate_only` on a non-JSON close/admission claim | fail |
| Extra rendered field or companion-report key not read by the gate | fail |
| Diagnostic PMU/cycles/structural-scan field used as producer evidence | fail |

## Validation Order

1. Parse the report into a strict known-field structure.
2. Reject unknown fields and validator-only requirements.
3. Validate required identifier presence and outcome enum.
4. Validate row id, grammar id, domain, workload, and uniqueness.
5. Validate run id, host, build flags, feature mask, sample count, profile, and
   sample cost.
6. Validate output plane, comparator/oracle plane, strictness, freshness, and
   source artifact.
7. Validate Track 2/oracle independence and source path separation.
8. Validate same-wave consumer class and diagnostic non-producer status.
9. Apply admission-specific predicates. W1a itself has no parser row movement,
   no generated non-JSON baseline authority, and no non-JSON close.

## Self-Verdict

ACCEPT for Phase 1 research handoff. The contract translates the W1a telemetry
requirements into gate predicates and fixtures while preserving JSON behavior,
same-wave consumption, Track 2/oracle independence, and producer-only
rejection. No source files or generated outputs are edited.
