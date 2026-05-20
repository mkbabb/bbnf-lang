# SK-V11 W8 Plan - Direct Residual Fixpoint And Row Reclamation

Date: 2026-05-20.

Phase: W8 plan synthesis.

Disposition: docs/gate/result accounting only. No W8a source split.

## Selected Intervention

W8 selects no behavior source intervention. It executes SPEC Section 12 as
direct residual fixpoint accounting:

- re-evaluate every remaining direct residual under the strict direct contract;
- admit no rows by docs-only movement;
- record per-row uncloseable proofs for rows that still miss or are W0-clamped
  without measured behavior-wave provenance;
- carry the non-JSON generated-intervention axis as BLOCKED rather than
  paper-close it.

Because this plan touches no behavior source, no gate schema, no validator
semantics, no telemetry identifiers, and no `RESULTS.md` row status, W8 stays
inside the default docs/gate/result surface. SPEC permits W8 CHALLENGE to be
skipped for gate/report-only fixpoint accounting; this plan therefore routes
directly to redress after the plan commit.

## Entry Gate

The Section 12 entry gate is satisfied for docs-only fixpoint accounting:

- W3 rejected with measurement: REDRESS 114.
- W4 rejected with measurement: REDRESS 115.
- W5 blocked before source redress: REDRESS 116.
- W6 blocked before source redress: REDRESS 117.
- W7 blocked before source redress: REDRESS 118.
- W2 recorded the non-JSON BLOCKED route: REDRESS 113.
- W8 research provides a candidate-exhaustion proof plan for every remaining
  direct residual row.

No W8a source candidate remains on current evidence. A source split would need
exactly one row subset plus a CHALLENGE-accepted source route; the W8 research
cohort found none.

## Owner Paths

Redress may edit only:

- `restart/skinny/tranches/sk-v11/research/w8/redress/w8-redress-fixpoint.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v11/HANDOFF.md`

No W8 behavior source owner path is opened. No `skinny/RESULTS.md` movement is
allowed because no row admission is selected.

## Residual Row Proof Table

W8 redress records the following proof table in REDRESS. Floors are SPEC
Section 0.4 direct floors, `ceil(sonic-rs strict direct Mbps / 1.10)`.

| Row | Track 1 | Track 2 | sonic direct | floor | W8 proof |
|---|---:|---:|---:|---:|---|
| `twitter` | 11613 | 10816 | 15113 | 13740 | W5 string-span route blocked by REDRESS 116; W7 digest route blocked by REDRESS 118; no W8a source candidate remains. |
| `canada` | 10316 | 9819 | 11700 | 10637 | W3 numeric route measured-rejected on sibling `mesh`; `canada` has larger Track 2 floor gap; no W8a numeric candidate remains. |
| `github_events` | 11918 | 10596 | 14743 | 13403 | W5 string-span route blocked; W7 digest visible-bucket math cannot close both tracks; no W8a candidate remains. |
| `update_center` | 8187 | 7474 | 11064 | 10059 | W5 string-span route blocked; W7 digest route floor-insufficient; no W8a candidate remains. |
| `mesh` | 8561 | 8652 | 9542 | 8675 | W3 `number_span_emit_slot` measured 3835 / 3614 against 8675 and was reverted; row remains uncloseable in SK-V11. |
| `random` | 7693 | 6949 | 8665 | 7878 | W4 `container_tail_next` probe measured 3518 / 3498 against 7878 and was reverted; W5/W7 blocked; no W8a candidate remains. |
| `gsoc-2018` | 2665 | 2578 | 4110 | 3737 | Movemask/string-scan residual; W5 and W7 leave no accepted source authority; no W8a candidate remains. |
| `instruments` | 11569 | 10736 | 9865 | 8969 | Numerically above floor but W0-clamped; no W3-W8 measured behavior provenance, so docs-only admission is pre-blocked. |
| `numbers` | 4479 | 2366 | 2667 | 2425 | Track 2 misses floor and row is W0-clamped; W3 numeric route rejected; no W8a candidate remains. |
| `unicode_mixed` | 3753 | 2427 | 2846 | 2588 | Track 2 misses floor and row is W0-clamped; W6 decoded-source route blocked by REDRESS 117; no W8a candidate remains. |
| `unicode_escapes` | 1345 | 1341 | 3785 | 3441 | Unicode escape route blocked by W5/W6 and SK-V10 REDRESS 107/108 proof-only limits; no W8a candidate remains. |
| `distinct_values` | 1750 | 1625 | 2923 | 2658 | W5 string route blocked; W7 digest bucket insufficient; no W8a candidate remains. |
| `y_string_unicode` | 1983 | 1029 | 4344 | 3950 | Unicode escape/string route blocked by W5/W6 and prior proof-only limits; no W8a candidate remains. |

## Guard Checks

W8 redress must confirm no unsupported row movement:

- `git diff --exit-code -- skinny/RESULTS.md`
- skinny gate advisory with the SK-V11-open Criterion home:
  `CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run --manifest-path skinny/Cargo.toml -p bbnf-bench --bin gate -- --advisory`

Direct guards remain `citm_catalog`, `apache_builds`, `marine_ik`, and
`unicode_basic`. Typed guards remain `twitter`, `citm_catalog`,
`apache_builds`, `github_events`, `update_center`, `mesh`, and `marine_ik`.
Because W8 runs no source, gate, report, or RESULTS mutation, guard status is
preserved by unchanged inputs plus the advisory gate.

## Non-JSON Axis

W8 cannot admit the grammar-generalization axis. W1a admitted only a schema and
gate/report lane; W1b rejected the generated CSS L4 baseline; W2 blocked before
creating a first measurable non-JSON intervention; W7 found no non-JSON
host-sink route. W8 therefore carries this as a BLOCKED grammar-generalization
fixpoint into W9 and Pass Alpha.

## Falsifiability Gate

`G-W8-DIRECT-FIXPOINT` passes as a fixpoint only if redress records every
Section 0.4 direct row as either admitted or uncloseable with the table above,
confirms existing guards through the unchanged gate advisory, and makes no
unsupported `RESULTS.md` movement. If any row is moved without measured
provenance, W8 fails closed.

## Revert Protocol

No source patch is produced. If redress discovers unsupported `RESULTS.md`,
gate/report, or source movement, revert the W8 docs slice and record the
blocker instead of closing W8.
