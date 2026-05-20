# SK-V11 W4-R4: Gate/Report Consumption

Date: 2026-05-20.
Scope: research-only review of `skinny/crates/bbnf-bench/src/report.rs` and
`skinny/crates/bbnf-bench/src/bin/gate.rs`, focused on JSON direct row
consumption for W4. No source, generated parser, `RESULTS.md`, or redress
ledger edits are made by this artifact.

## Findings

- `gate-json` already has a gate-consumed status/provenance schema for JSON
  direct row movement. W4 does not need new telemetry fields.
- The current JSON row producer is `bbnf-bench --bin gate`, invoked by
  `xtask gate-json`. It builds a `Report` from Criterion `estimates.json` and
  `metadata.toml`, validates the rendered schema and provenance, then either
  writes `skinny/RESULTS.md` or fails if the checked file is stale.
- Existing direct movement is intentionally narrow: a baseline
  `N-direct / NO-GO` `direct_to_struct` row may become `A / GO` only through
  the direct contract validator, with digest output, strict measured-row
  validation, independent Track 2, same-run native direct comparators, a
  non-`gate_only` consumer, REDRESS provenance, and a non-`SK-V9-open` wave id.
- `direct_row_decision` currently has only W2 and W10 direct admission paths.
  Every other baseline `N-direct` row that happens to classify as within sonic
  slack is forced back to `N-direct / NO-GO` by the W0 no-admission clamp.
- W4 therefore needs a W4-specific decision/marking path in `gate.rs`, plus
  matching validation/tests in `report.rs`, but it can store all W4 provenance
  in existing fields: `wave_id`, `redress_entry`, `sk_v9_open_delta`,
  `same_wave_consumer_class`, `strictness`, `parse_utf8`,
  `measured_validation_path`, comparator evidence, and the rendered `signal`.

## Existing Schema

`Report` contains `rows`, optional `probe_rows`, and notes. Each main
`TelemetryRow` carries rendered status columns plus a structured `sk_v8`
manifest. The schema structs use `serde(deny_unknown_fields)`, so JSON report
lanes reject producer-only fields rather than preserving them silently.

Main row status fields are:

- `corpus`, `workload`, `outcome_id`, `verdict`
- `strictness`, `parse_utf8`, `escape_complete`, `flaw_probe`, `output_plane`
- `track1_mbps`, `track2_mbps`
- native and sidecar comparator Mbps through `ComparatorSet`
- delta fields, `hot_leaf`, and `signal`

Provenance fields in `SkV8Telemetry` are:

- identity: `row_id`, `grammar_id`, `domain`
- validation/profile: `measured_validation_path`, `profile_artifact`,
  `sample_cost`, `sample_count`
- build/run: `build_flags`, `host_triple`, `feature_mask`, `wave_id`,
  `run_id`
- gate routing: `costfacts_rule_id`, `costfacts_chosen_shape`,
  `costfacts_rejected_alternative_ids`, `redress_entry`,
  `sk_v9_open_delta`, `same_wave_consumer_class`
- substrate/diagnostic: `substrate_surface`,
  `structural_projection_status`, `substrate_cardinality`,
  `track2_independence_status`, `diagnostic_nonproducer_status`
- comparator evidence: `comparator_id`, `comparator_plane`,
  `comparator_strictness`, `comparator_freshness`,
  `sidecar_freshness`, `value_mbps`, `source_artifact`

Status ids are defined in `gate.rs`: `A`, `B`, `C`, `D`, `E`, `F-positive`,
`F-noise`, `G`, `I`, `J`, `K`, `L`, `M`, `N-direct`, and `S`. For W4 direct
rows, the only admitting status is `A / GO`; `N-direct / NO-GO` remains the
strict direct residual state.

## W4 Consumption Shape

W4 should reuse the direct contract path instead of creating a companion report
or adding telemetry fields. The gate/report changes should be limited to W4
selection, floor checking, row marking, and validator tests.

Required producer behavior in `gate.rs`:

- Add a W4-specific branch before the W0 clamp in `direct_row_decision`.
- Admit only the CHALLENGE-selected W4 direct rows, at most three rows from
  `canada`, `mesh`, `random`, `update_center`, `github_events`, and `twitter`.
- Require direct correctness to pass. `I`, `J`, or other hard failures must not
  be converted to W4 admissions.
- Require both generated Track 1 and independent Track 2 Mbps to clear the W4
  floor for the selected row.
- Mark admitted rows through existing fields:
  - `outcome_id = "A"` and `verdict = "GO"` via `TelemetryRow::workload(...,
    None, ...)`
  - `strictness = "strict"`
  - `parse_utf8 = "measured-row"`
  - `measured_validation_path = "measured-row"`
  - `same_wave_consumer_class = "gate_json_direct_contract"`
  - `wave_id = "SK-V11-W4"`
  - `redress_entry = "<W4 REDRESS id>"`
  - `sk_v9_open_delta = "direct-dispatch-byteset"` or another W4-specific
    value consumed by tests
  - keep `output_plane = "digest"`
  - keep native comparator evidence sourced from
    `sonic_rs_direct_to_struct` and `serde_json_direct_to_struct`

Required consumer behavior in `report.rs`:

- Keep using `validate_direct_row_movement` for baseline `N-direct` rows, but
  make W4 provenance explicit enough that manual `RESULTS.md` edits cannot pass
  as generic direct movement.
- Consume W4 using existing fields. A W4 validator/test can require
  `wave_id = "SK-V11-W4"`, `same_wave_consumer_class =
  "gate_json_direct_contract"`, a concrete REDRESS id, strict measured-row
  validation, digest plane, `independent_verified` Track 2, and same-run native
  comparator sources.
- Align the producer floor table and consumer floor table. Current
  `validate_direct_row_movement` uses `sk_v10_direct_floor`, whose values do
  not exactly match SK-V11 Section 0.4 for every W4 candidate. W4 should either
  update the shared direct floor authority or introduce a selected W4 floor
  helper used by both producer and validator.
- Add guard consumption for Section 0.5 direct guard rows if W4 claims guard
  floors. The current validator enforces typed maintain floors, but it does not
  separately enforce direct guard floors for unchanged `A / GO` direct rows.

Do not add fields such as `w4_target`, `primitive_mbps`, `dispatch_shape`, or
`byteset_probe`. If those facts matter, place them in the W4 research/redress
artifact and consume the row-level consequence through existing fields and
Criterion row measurements.

## Selected Row Gates

SK-V11 Section 8 permits a maximum of three selected W4 target rows from the
six below. Section 0.4 floors and current `RESULTS.md` rows give this gate
surface:

| Row | Track 1 | Track 2 | Sonic direct | Section 0.4 floor | Track 1 gap | Track 2 gap |
|---|---:|---:|---:|---:|---:|---:|
| `canada/direct_to_struct` | 10316 | 9819 | 11700 | 10637 | 321 | 818 |
| `mesh/direct_to_struct` | 8561 | 8652 | 9542 | 8675 | 114 | 23 |
| `random/direct_to_struct` | 7693 | 6949 | 8665 | 7878 | 185 | 929 |
| `update_center/direct_to_struct` | 8187 | 7474 | 11064 | 10059 | 1872 | 2585 |
| `github_events/direct_to_struct` | 11918 | 10596 | 14743 | 13403 | 1485 | 2807 |
| `twitter/direct_to_struct` | 11613 | 10816 | 15113 | 13740 | 2127 | 2924 |

Near-floor candidates are `mesh`, `canada`, and `random`; they still require
both tracks to clear floor in the full Criterion row, not just primitive or
single-track probes. `update_center`, `github_events`, and `twitter` have much
larger Track 2 gaps, so selecting them needs stronger same-host microbench
evidence before redress.

Current code-level floor reality differs from Section 0.4 for these rows:

| Row | Section 0.4 floor | Current `sk_v10_direct_floor` |
|---|---:|---:|
| `canada/direct_to_struct` | 10637 | 10977 |
| `mesh/direct_to_struct` | 8675 | 8916 |
| `random/direct_to_struct` | 7878 | 7734 |
| `update_center/direct_to_struct` | 10059 | 10160 |
| `github_events/direct_to_struct` | 13403 | 14364 |
| `twitter/direct_to_struct` | 13740 | 13840 |

This mismatch is a gate-consumption risk. A W4 implementation should not let
the producer admit by one table while `report.rs` validates by another. The
least ambiguous shape is one shared W4 selected-row floor helper, with tests for
each selected row and a negative test proving unselected W4 candidates remain
clamped even if their current run clears floor.

## Required Tests

Focused `gate.rs` tests should prove:

- selected W4 rows admit only when both Track 1 and Track 2 clear the selected
  W4 floor;
- unselected rows from the six-corpus candidate set remain W0-clamped;
- hard correctness failures remain hard failures;
- W2 and W10 direct paths remain unchanged.

Focused `report.rs` tests should prove:

- a complete W4 direct row with strict measured-row provenance validates;
- W4 rejects `gate_only`, deferred validation, non-digest output, missing
  REDRESS, stale `SK-V9-open` wave id, bad Track 2 independence, wrong native
  comparator plane/source, and floor misses;
- direct guard floors are consumed if W4 claims Section 0.5 guard coverage;
- no unknown producer-only field is accepted by any JSON or non-JSON report
  parser.

## Verification Commands For A Later W4 Implementation

Run from `skinny` after a real W4 implementation and full native Criterion
capture:

```sh
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench w4_direct -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench direct_contract -- --nocapture
CRITERION_HOME=<w4-criterion> RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- bench-json --advisory
CRITERION_HOME=<w4-criterion> RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
```

## Sources

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/gate.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v11/SPEC.md`
- `restart/skinny/tranches/sk-v11/HANDOFF.md`
