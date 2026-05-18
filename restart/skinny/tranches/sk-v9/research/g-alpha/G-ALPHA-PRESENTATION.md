# SK-V9 G-Alpha Presentation

Date: 2026-05-18.

Status: ready to present. SK-V9 implementation is not dispatched.

## Decision

Recommendation: G-Alpha closed.

Rationale: SK-V9 Pass Alpha converged after two consecutive clean challenge
cycles:

- V3: 6/6 ACCEPT, minimum confidence 96%.
- V4 unchanged re-challenge: 6/6 ACCEPT, minimum confidence 96%.
- Open critical defects: none.
- Orphan REVISE dispositions: none.
- SK-V9 `SPEC.md` and `DISPATCH-PROMPT.md`: absent by design.

## Current SK-V8 Close State

Benchmark authority remains `skinny/RESULTS.md`:

| Family | State |
|---|---|
| `parse_only` | 16 `S / NO-GO`, 1 `L / NO-GO` |
| `direct_to_struct` | 3 `A / GO`, 14 `N-direct / NO-GO` |
| `real_typed_struct` | 4 `A / GO` |

All current main rows remain `Strictness=deferred`,
`parse_utf8=view-boundary`, and `escape_complete=yes`. Parse rows remain
non-admission guard telemetry under the current borrowed-view-vs-DOM plane.
Direct digest rows remain guard/control evidence unless a future direct output
contract or control-path tranche is accepted.

## Alpha Goalset

SK-V9 Alpha carries three behavior candidates:

| Candidate | Purpose | Admission boundary |
|---|---|---|
| Apache/CITM typed row-table admission | Convert W2 source/product parity into measured `real_typed_struct` rows. | Fresh run-id/metadata evidence, generated Track 1 DirectBuild, independent serde/oracle path, sonic parity lane, and `A / GO` row rendering. |
| Retained class/event grammar plus `ValueRef` proof | Prove the retained event grammar and cursor contract before structural parse implementation. | Proof-only at Alpha depth; no row movement without a later capped same-wave generated retained Track 1 consumer and challenge acceptance. |
| Direct output/control-path contract | Make direct rows product-contract-capable or keep them as guard rows. | Selected direct rows must meet Track 1 and Track 2 floors plus full-table maintain; digest alone is never product proof. |

Two Alpha-E entries are gate-only prerequisites, not behavior candidates:

| Prerequisite | Boundary |
|---|---|
| Comparator same-run evidence manifest | Evidence ingestion only; no parser data, row output, substrate, or strict admission by itself. |
| SK-V9-open telemetry/gate refresh | Report/gate run identity only; no behavior movement or measured row admission by itself. |

## Cost And Caps

| Candidate | LOC budget | Risk | Hard cap | Row effect |
|---|---:|---|---|---|
| Apache/CITM typed row-table admission | 300 | Medium | <=90 min implementation/redress | May add measured typed rows if all evidence gates pass |
| Retained class/event grammar plus `ValueRef` proof | 450 | High | <=90 min implementation/redress | Proof-only until later same-wave generated retained consumer |
| Direct output/control-path contract | 600 | High | <=90 min implementation/redress | May move selected direct guards only under future accepted gates |
| Comparator same-run evidence manifest | 500 | Medium-high | <=90 min implementation/redress | Freshness/plane fields only |
| SK-V9-open telemetry/gate refresh | 450 | Medium | <=90 min implementation/redress | Report/run identity only |

Any future S-P3 wave exceeding its LOC budget or <=90 minute implementation /
redress cap returns REVISE before dispatch.

## Binding Pre-Blocks

- REDRESS 91: Apache/CITM are not measured SK-V8 rows; Canada typed remains
  routed until full-fixture checksum parity exists.
- REDRESS 92: no structural parse implementation before retained class/event
  grammar plus `ValueRef` proof.
- REDRESS 93: scalar-parent folding and renamed parent-digest folds remain
  rejected without a V9-aware checked gate, full-table maintain proof, and
  independent Track 2 digest-arithmetic backstop.
- REDRESS 73: generated retained helper shape cannot be assumed to transfer to
  hand Track 2/control paths without direct hand-parser profiling.
- Alpha-C's full historical pre-block ledger is binding by reference.
- Pass Omega owns SC-6-L1-R1, broad lock amendments, canonical path cleanup, and
  top-level surface refresh unless separately ratified.

## After G-Alpha

If the user returns `G-Alpha closed`, the next move is the skinny pass sequence
for SK-V9. Downstream S-P3 must author the future SK-V9 wave plan from this
Alpha goalset after its own entry conditions are met. No SK-V9 implementation
wave may dispatch before that plan converges.
