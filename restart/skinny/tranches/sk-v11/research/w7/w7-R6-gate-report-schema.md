# SK-V11 W7 R6 - Gate / Report / Schema / Results

Phase: W7 Research R6.
Date: 2026-05-20.
Scope: gate/report/schema/result authority for W7 Output Digest/Hash Host Sink.
Disposition: research only; no source edits.

## Authority Read

- `restart/skinny/tranches/sk-v11/SPEC.md` Section 11 gives W7 only C8
  authority: output digest/hash oracle or per-product host sink. The owner set
  is bench/product/report/gate/results only; generic parser crates are outside
  the lane.
- `restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md`
  names `G-W7-DIGEST-SINK`: fresh post-W6 profile must still name
  `output_digest_hash` as limiting on the selected residual subset, and every
  admitted direct row must clear its direct floor on Track 1 and independent
  Track 2/oracle.
- `restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md`
  preserves the schema-v3 table and the SK-V9 W0 telemetry manifest identifier
  set. W7 may not add a rendered column, manifest field, or outcome variant
  unless the same wave extends and tests the gate consumer.
- `skinny/RESULTS.md` is still the live rendered authority. It carries
  `N-direct / NO-GO` residual rows and current schema-v3 + telemetry manifest
  cells; W7 must update it only after the W7 gate packet passes.
- `skinny/crates/bbnf-bench/src/report.rs` already validates direct-row
  movement through `validate_direct_row_movement`: `A / GO`, digest output
  plane, strict measured-row validation, `escape_complete=yes`, Track 2
  independence, non-`gate_only` consumer, REDRESS provenance, non-open wave id,
  profile/hot-leaf coherence, and comparator evidence. However its
  `sk_v10_direct_floor` constants are not the SK-V11 §0.4 floor table, so any
  W7 row admission must update the same-wave floor consumer to the SK-V11
  values or fail closed.
- `skinny/crates/bbnf-bench/src/bin/gate.rs` renders the report from Criterion
  estimates, validates schema/W0 telemetry, and writes `RESULTS.md` only under
  `--update-results` / `--write-results`. W7 must add any W7-specific row
  provenance in this producer and consume it in `report.rs` in the same commit.

## W7 Floors

W7 can select only residual rows whose fresh post-W6 profile still names
`output_digest_hash` as a limiting hot leaf. If selected, the row admits only
when Track 1 and independent Track 2/oracle both meet the binding SK-V11 floor:

| Direct residual row | Binding floor Mbps |
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

For a selected non-JSON host-sink row, W7 needs generated Track 1, independent
oracle/Track 2, strict output equality, gate-consumed grammar/domain/workload,
and at least `>= 1.0%` same-run improvement over the accepted W1b baseline.
Without W1b non-JSON baseline authority, the non-JSON W7 route is unmeasurable.

## Guard Rows

Any W7 report refresh or host-sink edit must preserve the direct guard block on
both tracks:

| Direct guard row | Track 1 maintain | Track 2 maintain |
|---|---:|---:|
| `citm_catalog/direct_to_struct` | 18191 | 17431 |
| `apache_builds/direct_to_struct` | 11028 | 9996 |
| `marine_ik/direct_to_struct` | 8759 | 9248 |
| `unicode_basic/direct_to_struct` | 2253 | 2182 |

If W7 touches typed output, typed report/gate logic, or
`generated_real_typed.rs`, it also preserves:

| Typed guard row | Track 1 maintain | Track 2/oracle guard |
|---|---:|---:|
| `twitter/real_typed_struct` | 17385 | 15593 |
| `citm_catalog/real_typed_struct` | 29928 | 17321 |
| `apache_builds/real_typed_struct` | 8308 | 6754 |
| `github_events/real_typed_struct` | 11633 | 12029 |
| `update_center/real_typed_struct` | 11613 | 10150 |
| `mesh/real_typed_struct` | 9214 | 7739 |
| `marine_ik/real_typed_struct` | 11552 | 9894 |

## Required Telemetry Identifiers

W7 must keep the schema-v3 rendered columns and telemetry manifest identifiers
unchanged unless it adds a same-wave gate consumer. The gate-consumed identifier
set required for W7 row movement is:

```text
row_id grammar_id domain corpus workload outcome_id verdict strictness
parse_utf8 escape_complete flaw_probe output_plane track1_mbps track2_mbps
comparator_id comparator_plane comparator_strictness comparator_freshness
sidecar_freshness comparator_value_mbps comparator_source_artifact
measured_validation_path profile_artifact sample_cost sample_count build_flags
host_triple feature_mask costfacts_rule_id costfacts_chosen_shape
costfacts_rejected_alternative_ids redress_entry wave_id run_id
sk_v9_open_delta substrate_surface structural_projection_status
substrate_cardinality same_wave_consumer_class track2_independence_status
diagnostic_nonproducer_status
```

For W7 JSON direct admission, the expected values are:

- `grammar_id=json`, `domain=json_bench`, `workload=direct_to_struct`,
  `output_plane=digest`.
- `outcome_id=A`, `verdict=GO`, `strictness=strict`,
  `parse_utf8=measured-row`, `escape_complete=yes`,
  `measured_validation_path=measured-row`.
- `track2_independence_status=independent_verified`.
- `same_wave_consumer_class` names the product host sink consumer, not
  `gate_only`.
- `redress_entry` names the W7 REDRESS entry and `wave_id` is an SK-V11 W7
  value, not `SK-V9-open`.
- Comparator evidence includes same-run native strict digest-plane
  `sonic_rs_strict` and/or matching `serde_json`; absent/historical sidecars
  are planning only.
- `profile_artifact`, `hot_leaf`, and `sample_cost` identify the fresh post-W6
  Criterion/samply evidence, and the hot leaf must still support
  `output_digest_hash` as the limiting W7 target.

## Gate / Report Consumption

Existing consumption:

- `Report::validate_schema_v3` consumes the rendered schema-v3 fields.
- `Report::validate_sk_v8_w0` consumes uniqueness, run-id uniformity, baseline
  row identity, W6 typed special-case provenance, direct movement validation,
  and existing typed maintain floors.
- `validate_direct_row_movement` consumes direct row movement semantics,
  strictness, digest output plane, Track 2 independence, non-gate-only consumer,
  REDRESS/wave provenance, profile/hot-leaf coherence, and comparator evidence.
- `gate::validate_strict_admission` consumes native strict comparator,
  matching output plane, measured-row validation, and freshness.

Required W7 same-wave consumer updates:

- Replace or version the direct movement floor helper so W7 admitted rows use
  the SK-V11 §0.4 floor table above, not the stale SK-V10 constants currently
  present in `report.rs`.
- Add W7 provenance checks for `wave_id`, `redress_entry`,
  `sk_v9_open_delta`, and `same_wave_consumer_class`; a generic non-`gate_only`
  consumer is too weak for W7 if the row claims `G-W7-DIGEST-SINK`.
- Consume the post-W6 hot-leaf proof. A row whose `Hot leaf` / telemetry
  artifact does not show `output_digest_hash` as limiting is not a W7 row even
  if it clears Mbps floors.
- If a non-JSON host-sink row is selected, consume the companion report or
  `RESULTS.md` row through the gate in the same wave. Prose Lock 14 evidence
  and producer-only report fields do not count.

## Rejection Conditions

W7 must reject before or during redress on any of these:

- W3-W6 do not all have recorded dispositions, or W7 CHALLENGE does not accept
  the bounded output-digest subset.
- Fresh post-W6 profile does not name `output_digest_hash` as a limiting hot
  leaf for each selected row.
- No selected row admits, or any selected admitted row misses its Track 1 or
  Track 2/oracle floor.
- Direct or typed guard row falls below its maintain floor.
- Digest mismatch, decoded/raw segment boundary mismatch, or strict output
  equality failure.
- Track 2/oracle calls Track 1, reads a hidden sidecar, or shares a benchmark
  private parser path that invalidates independence.
- Digest/hash state enters generic parser crates, becomes parser semantics,
  or becomes a hidden semantic string/hash side table.
- Cache hint, prefetch, layout-only, PMU-only, samply-only, sidecar-only, or
  profile-only movement is used as PASS evidence without row movement.
- New column, manifest field, outcome variant, sidecar field, or non-JSON field
  is emitted without same-wave gate consumption.
- Parse-only movement is counted as W7 admission.

## Valid W7 Measurement Packet

Minimum commands for a valid W7 packet, with the Criterion and target roots
chosen by the W7 redress agent:

```sh
CARGO_TARGET_DIR=/tmp/skv11-w7-target \
CRITERION_HOME=/tmp/skv11-w7-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo run -p xtask -- bench-json --advisory

CRITERION_HOME=/tmp/skv11-w7-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo run -p xtask -- gate-json --with-cost-facts --check-results
```

Before a PASS commit, the packet must also include:

- direct digest parity tests for the selected sink paths, including raw string,
  escaped string, key, array string, object string, number, boolean, and null
  fold coverage as applicable;
- a same-host before/after probe or same-binary legacy-control proving the W7
  sink changed the selected row rather than only relabeling telemetry;
- `samply` or equivalent symbol evidence that the W7 host sink consumer is on
  the selected row's hot path;
- same-run measurements for all selected target rows and the full guard block;
- a gate update-results run only after all floors, parity, profile, consumer,
  guard, and comparator checks pass:

```sh
CRITERION_HOME=/tmp/skv11-w7-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo run -p xtask -- gate-json --with-cost-facts --update-results

CRITERION_HOME=/tmp/skv11-w7-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo run -p xtask -- gate-json --with-cost-facts --check-results
```

If any requirement fails, W7 records a measured REDRESS rejection, saves the
rejected patch at `/tmp/skv11-waveW7-rejected.patch`, reverts
host-sink/report/gate/RESULTS as one slice, and leaves `skinny/RESULTS.md`
unchanged.
