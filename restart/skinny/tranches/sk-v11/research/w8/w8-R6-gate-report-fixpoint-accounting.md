# SK-V11 W8 R6: Gate/Report Fixpoint Accounting

Role: SK-V11 W8 research R6.
Date: 2026-05-20.
Scope: W8 gate/report/RESULTS/REDRESS accounting only.
Artifact: read-only research; no source, gate, report, or RESULTS edit is
authorized here.

## Read Surface

- `restart/skinny/tranches/sk-v11/SPEC.md` Section 12 defines W8 as direct
  residual fixpoint and row reclamation. Default W8 has no new primitive and no
  source work; source work must split to W8a after CHALLENGE.
- P3-D binds the live schema-v3 table plus SK-V9 W0 telemetry manifest. Required
  identifiers are the rendered row fields, comparator evidence fields, run/build
  fields, REDRESS provenance, same-wave consumer, Track 2 independence, and
  diagnostic-nonproducer status.
- `skinny/crates/bbnf-bench/src/report.rs` currently fails closed on missing
  schema-v3 fields, duplicate/unknown row ids, unsupported outcomes, mixed run
  ids, invalid direct movement, missing REDRESS provenance, gate-only consumers
  for moved direct rows, comparator evidence gaps, and maintain-floor failures.
- `skinny/crates/bbnf-bench/src/bin/gate.rs` renders the report, validates
  schema-v3 plus W0 telemetry before writing/checking `skinny/RESULTS.md`, and
  supports advisory checking without admitting global `N-direct / NoGo`.
- Current REDRESS through item 118 records W1a admission, W1b rejection, W2
  block, W3/W4 rejection, W5/W6 block, and W7 block. W8 inherits those
  dispositions.

## W8 Close Rule

W8 may close only when every SPEC §0.4 direct residual row has one of two
states:

1. `A / GO` strict direct admission in `skinny/RESULTS.md`, with generated Track
   1 and independent Track 2/oracle both clearing the binding floor under
   same-run strict direct comparator evidence.
2. A REDRESS uncloseable proof with measurement, tied to that exact row and
   candidate route, with enough fields for W9 to audit the remainder without
   moving the row.

Rows may not move because a prior wave produced planning evidence, a proof-only
artifact, a stale SK-V10 result, a parse-only win, a sidecar comparator, a direct
digest typed proof, or a future-phase promise.

## Required Per-Row REDRESS Proof Table

For every residual row that remains non-admitted at W8, REDRESS must record this
exact table. One row per SPEC §0.4 residual row; no grouped prose substitute.

| Field | Required value shape |
|---|---|
| `row_id` | `json/<corpus>/direct_to_struct/main` |
| `corpus/workload` | `<corpus>/direct_to_struct` |
| `opening_outcome` | SK-V11-open outcome, normally `N-direct / NO-GO` |
| `w8_disposition` | `A / GO`, `REDRESS-uncloseable`, or `BLOCKED-by-entry` |
| `attempted_candidate` | Exact W3-W8 candidate/route attempted, or `none: exhausted/no legal candidate` |
| `source_delta` | `none`, `reverted:<patch>`, or `W8a:<challenge artifact>` |
| `measurement_root` | Criterion/probe root or proof artifact path used for this row |
| `run_id` | Same-run run id; for RESULTS rows must match the report run id |
| `host_flags` | Host triple plus `RUSTFLAGS`/target CPU |
| `track1_mbps` | Generated Track 1 direct Mbps, measured or `n/a:not-run` only for entry-blocked rows |
| `track2_oracle_mbps` | Independent Track 2/oracle Mbps, measured or `n/a:not-run` only for entry-blocked rows |
| `sonic_direct_mbps` | Same-run strict sonic direct Mbps, or `n/a:not-run` with entry-block reason |
| `serde_direct_mbps` | Same-run serde direct Mbps when collected; otherwise explicit absence reason |
| `binding_floor_mbps` | SPEC §0.4 floor for the row |
| `track1_floor_result` | `PASS`, `FAIL`, or `NOT-RUN:<entry reason>` |
| `track2_floor_result` | `PASS`, `FAIL`, or `NOT-RUN:<entry reason>` |
| `strict_validation` | `measured-row` for admitted movement; otherwise exact failed/blocked validation state |
| `output_plane` | `digest`; anything else is not direct residual evidence |
| `track2_independence` | `independent_verified` or exact coupling/block reason |
| `same_wave_consumer` | Product/gate consumer identity; `gate_only` cannot admit a moved row |
| `guard_block_result` | Summary of all §0.5 direct and typed guards checked in the same accounting pass |
| `unsupported_movement_check` | Command/output reference proving no other row changed disposition |
| `routed_remainder` | `W9 close BLOCKED`, `Pass Omega`, `future SPEC+CHALLENGE`, or `none` |
| `redress_entry` | New REDRESS item id and section heading |

The table must include all thirteen SPEC §0.4 residual rows:

| Row | Binding floor Mbps | W8 proof row required unless admitted |
|---|---:|---|
| `json/twitter/direct_to_struct/main` | 13740 | yes |
| `json/canada/direct_to_struct/main` | 10637 | yes |
| `json/github_events/direct_to_struct/main` | 13403 | yes |
| `json/update_center/direct_to_struct/main` | 10059 | yes |
| `json/mesh/direct_to_struct/main` | 8675 | yes |
| `json/random/direct_to_struct/main` | 7878 | yes |
| `json/gsoc-2018/direct_to_struct/main` | 3737 | yes |
| `json/instruments/direct_to_struct/main` | 8969 | yes |
| `json/numbers/direct_to_struct/main` | 2425 | yes |
| `json/unicode_mixed/direct_to_struct/main` | 2588 | yes |
| `json/unicode_escapes/direct_to_struct/main` | 3441 | yes |
| `json/distinct_values/direct_to_struct/main` | 2658 | yes |
| `json/y_string_unicode/direct_to_struct/main` | 3950 | yes |

## Guard Rows To Check

W8 accounting must independently check every SPEC §0.5 guard row. These are not
optional just because W8 is documentation/gate/report accounting.

Direct guards:

| Row | Track 1 maintain | Track 2 maintain |
|---|---:|---:|
| `json/citm_catalog/direct_to_struct/main` | 18191 | 17431 |
| `json/apache_builds/direct_to_struct/main` | 11028 | 9996 |
| `json/marine_ik/direct_to_struct/main` | 8759 | 9248 |
| `json/unicode_basic/direct_to_struct/main` | 2253 | 2182 |

Typed guards:

| Row | Track 1 maintain | Track 2/oracle maintain |
|---|---:|---:|
| `json/twitter/real_typed_struct/main` | 17385 | 15593 |
| `json/citm_catalog/real_typed_struct/main` | 29928 | 17321 |
| `json/apache_builds/real_typed_struct/main` | 8308 | 6754 |
| `json/github_events/real_typed_struct/main` | 11633 | 12029 |
| `json/update_center/real_typed_struct/main` | 11613 | 10150 |
| `json/mesh/real_typed_struct/main` | 9214 | 7739 |
| `json/marine_ik/real_typed_struct/main` | 11552 | 9894 |

If any guard misses, W8 cannot close by accounting. The REDRESS entry must name
the guard miss separately from residual-row proof status.

## Unsupported Row Movement Checks

Use these commands from the repository root when W8 claims docs/gate/report-only
fixpoint accounting and no `skinny/RESULTS.md` movement.

```sh
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df \
RUSTFLAGS="-C target-cpu=native" \
cargo run --manifest-path skinny/Cargo.toml -p bbnf-bench --bin gate -- --advisory
```

```sh
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df \
RUSTFLAGS="-C target-cpu=native" \
cargo run --manifest-path skinny/Cargo.toml -p xtask -- gate-json --with-cost-facts --check-results
```

```sh
git diff --exit-code -- skinny/RESULTS.md
git diff --exit-code -- skinny/crates/bbnf-bench/src/report.rs skinny/crates/bbnf-bench/src/bin/gate.rs
git diff --check
```

For row-disposition auditing, compare only the rendered row identity and
disposition columns:

```sh
awk -F'|' '
  NR > 4 && /^\| [^|]+ \| [^|]+ \|/ {
    gsub(/^ +| +$/, "", $2);
    gsub(/^ +| +$/, "", $3);
    gsub(/^ +| +$/, "", $4);
    gsub(/^ +| +$/, "", $5);
    print $2 "/" $3 " " $4 " / " $5;
  }
' skinny/RESULTS.md
```

Expected W8 docs-only invariant:

- no parse-only row changes to admission;
- no direct residual row changes unless it has same-wave strict evidence or a
  matching REDRESS proof table row;
- no typed row changes from direct digest evidence;
- all four direct guards remain `A / GO`;
- all seven typed guards remain `A / GO`;
- no new row id, grammar id, domain, outcome id, comparator id, or run-id family
  appears unless the same wave updated the gate consumer.

## CHALLENGE Requirement

W8 docs/gate/report fixpoint accounting may skip W8 CHALLENGE when all of these
are true:

- no behavior source changes;
- no generated parser/runtime/SIMD/codegen changes;
- no gate schema or validator semantics change;
- no new row id, outcome, comparator, grammar/domain, or report field;
- no `skinny/RESULTS.md` row movement except already-admitted, already
  gate-consumed movement from earlier waves;
- W8 only records per-row proof accounting in REDRESS and verifies existing
  gate/report invariants.

W8 must not skip CHALLENGE if it proposes a final source route, changes
gate/report acceptance semantics, changes the telemetry schema, admits a row
without existing validator support, or treats a blocked/rejected W2-W7 route as
fresh behavior evidence. A final source route becomes W8a, consumes the spare
split, and must name exactly one candidate plus one row subset before redress.

## R6 Recommendation

Implement W8 as documentation/report accounting unless a separate W8a CHALLENGE
is explicitly opened. The next REDRESS item should use the proof table above for
all thirteen residual rows, carry REDRESS 113-118 forward, state that the
non-JSON axis remains blocked by REDRESS 112/113 unless superseded, and include
the unsupported-row-movement commands as evidence. If all rows are either
admitted by prior gate-consumed evidence or tabled as measured uncloseable
proofs and all guards hold, W8 can route to W9 close. Otherwise W8 must route
the missing row/proof set as `BLOCKED`.
