# SK-V11 W1a Phase 1 Research - R6 REDRESS, Preblocks, and Wave Boundaries

Status: read-only research artifact.
Scope owner: W1a R6 REDRESS/preblocks and wave boundaries.
Owned path: `restart/skinny/tranches/sk-v11/research/w1a/w1a-R6-redress-boundaries.md`.
Source edits: none.

## Source Authority

- `skinny/REDRESS.md` entries 1-110, with W1a load-bearing entries 34, 35, 36, 37, 38, 48, 85, 86, 87, 100, 101, 109, and 110.
- `restart/skinny/tranches/sk-v11/SPEC.md` Section 4, plus Section 0.3, Section 1, Section 2, and Section 13 where they bind telemetry, no-row movement, CHALLENGE, and preblocks.
- `restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md`, especially W1a in the ownership map and W1a in the per-wave preblocked route ledger.
- S-P1/S-P2/S-P3 hardening convergence:
  - `research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
  - `research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
  - `research/p3/hardening/HARDENING-S-P3-V4-CONSOLIDATED.md`

## R6 Finding

W1a is a C9 accounting wave. It may create the non-JSON gate/report schema lane
and fail-closed fixture coverage, but it must not create parser behavior,
baseline authority, or row admission. The allowed movement is schema
consumption: grammar id, domain, output plane, comparator/oracle provenance,
Track 2/oracle independence, run id, host, feature mask, same-wave consumer
class, and producer-only telemetry rejection must become gate-checkable.

The wave boundary is one-way: W1a enables W1b to create the first generated
non-JSON baseline row, and W1b enables W2 to attempt the first generated
non-JSON intervention. If W1a starts proving non-JSON performance, emitting a
generated non-JSON parser, or changing a `RESULTS.md` row, it has crossed into
W1b/W2 and should return REVISE before redress.

## W1a Preblocked Routes

| Route pressure | W1a preblock | Binding source |
|---|---|---|
| JSON-provider emission as generic proof | A generated JSON provider, renamed JSON helper, or JSON-only emitted parser cannot prove Lock 14 generality. Non-JSON proof remains deferred to W1b/W2. | SPEC Section 4; P3-E W1a ledger; REDRESS 36-38, 85, 86 |
| Old hand non-JSON struct-direct runtimes as proof | Hand-only non-JSON output or legacy direct runtime shape cannot establish generated-parser generality, baseline authority, or row admission. | P3-E W1a ledger; S-P2 convergence |
| Behavior row movement | No direct, typed, parse-only, or non-JSON row may change verdict/outcome in W1a. No JSON `RESULTS.md` row moves. | SPEC Section 4 exit gate; REDRESS 100, 101, 109 |
| Generated baseline authority | W1a cannot create the first generated non-JSON baseline row. That is W1b. | SPEC Sections 4 and 5; P3-E ownership map |
| Documentation-only Lock 14 claims | Prose, schema presence, or a report field alone cannot close grammar generality. | SPEC Section 4; S-P2/S-P3 hardening |
| Producer-only telemetry | Any emitted field not consumed by the gate in the same wave is a blocker, not evidence. | SPEC Section 0.3 and Section 4; REDRESS 87 |
| Hidden directives or BIR variants | W1a must not add a directive, BIR variant, `BackendShape`, public substrate API, or hidden host schema fact. | SPEC Section 1 and Section 13; P3-E hard blocks |
| Track 1/Track 2 coupling | Track 2/oracle fields must reject shared generated Track 1 helpers, generated SinkOnly helpers, generated typed helpers, or benchmark-private parser code. | SPEC Section 2.3; REDRESS 34, 35, 48 |
| Gate-only row close | Gate/report changes may validate evidence shape; they cannot become non-gate-only consumers for row movement inside W1a. | REDRESS 100; P3-E W1a avoidance rule |
| Stale row-table reclamation | SK-V10 direct-contract precedents do not let W1a reclaim rows from stale sidecars, old comparators, or W0-clamped throughput. | REDRESS 100, 101, 109, 110; S-P1 hardening |
| W3 substrate family | Union/event substrate, class columns, structural-position vectors, streaming cursors, sidecars, retained class lanes, and W4-through-W3 cascade locks remain closed. | SPEC close condition; P3-E hard blocks; S-P1/S-P2 hardening |
| Diagnostic facts as behavior producers | PMU/cycles, structural scan speed, masking probes, lazy materialization, `tape_vs_tape`, parse-only rows, and CostFacts cannot admit rows. | S-P1 hardening; P3-E proof-only surfaces |

## Material Differentials

W1a has no parser/materialization route that can satisfy a P3-E Section 2.4
material-differential package. It has no behavior source delta, no generated
non-JSON baseline, and no same-wave product consumer. Any plan that tries to
reopen a string, numeric, dispatch, escape, W3 substrate, PMULL/CTZ, or output
sink route in W1a should be rejected as wrong wave.

The only material differentials W1a can claim are gate/report differentials:

| Differential boundary | Minimum W1a differential | Non-allowed claim |
|---|---|---|
| REDRESS 87 CostFacts evidence-only | The gate must consume non-JSON identifiers and reject producer-only fields; a report field by itself is insufficient. | CostFacts, schema presence, or diagnostics as performance evidence. |
| REDRESS 34/35/48 Track honesty | The schema must carry Track 2/oracle independence status and source identity, and fixtures must prove coupled/shared sources fail. | Generated Track 1 equals Track 2, hidden parser reuse, or hand-only proof. |
| REDRESS 36-38 and 85/86 Lock 14 | The schema must be grammar-neutral and must not encode JSON grammar policy in generic crates or runtime. | JSON provider emission, JSON-named generic helper, or renamed JSON path as generality proof. |
| REDRESS 100/101/109 direct movement contract | W1a may require fields needed by later movement contracts but cannot extend those predicates into row admission. | Any changed JSON row, generated non-JSON baseline, or direct residual admission. |
| REDRESS 110 close accounting | W1a starts from the closed SK-V10/SK-V11-open authority and cannot treat close docs as an open implementation route. | Stale SK-V10 row reclamation or close-by-documentation. |

If a W1a plan contains a REDRESS-adjacent implementation route, CHALLENGE should
require a material-differential paragraph before redress and then route it out
of W1a unless it is pure gate/report schema consumption.

## No-Row-Movement Constraints

- No parser row moves in W1a.
- No JSON `RESULTS.md` row moves.
- No direct, typed, parse-only, or non-JSON row admission.
- No generated non-JSON baseline authority.
- No W0-clamped row admission from opening throughput.
- No parse-only SOTA claim.
- No direct digest evidence may be relabeled as typed proof.
- Existing direct admits and typed admits remain guard rows if touched by
  report/gate output.
- Every new telemetry field must be consumed by `gate-json` or by a
  gate-consumed companion W1a report in the same wave.
- Missing non-JSON required fields, Track 2/oracle coupling, stale comparator
  provenance, wrong output plane, wrong strictness, and producer-only telemetry
  must fail closed.

## Revert Protocol

For the eventual W1a redress, the revert unit is one slice:
gate/report/metadata/fixture changes and any companion W1a report fixtures.
Generated output remains disallowed unless fixtures are explicitly named by the
accepted plan.

Revert is mandatory if any of these occur:

- JSON `gate-json --with-cost-facts --check-results` weakens or regresses.
- Missing non-JSON fields are accepted.
- Producer-only non-JSON telemetry is accepted.
- Track 2/oracle coupling is accepted.
- A JSON `RESULTS.md` row moves.
- Any direct, typed, parse-only, or non-JSON row is admitted.
- A generated non-JSON baseline is claimed.
- JSON policy leaks into a generic crate/runtime path.
- A directive, BIR variant, public substrate API, sidecar, or hidden schema fact
  is added.

The REDRESS record for a W1a miss should preserve the failed fixture or command,
the rejected field/provenance pattern, whether JSON gate behavior changed, and
the reason the gate/report schema could not consume non-JSON evidence without
weakening JSON.

## CHALLENGE Focus

| Lens | W1a focus | Reject if |
|---|---|---|
| CH1 correctness and measurable gates | Verify fail-closed fixtures for required non-JSON fields, output plane, strictness, comparator/oracle, run id, host, feature mask, Track 2/oracle independence, same-wave consumer class, and producer-only telemetry. | The plan relies on prose, optional display columns, or unconsumed fields. |
| CH2 generality and Lock 14 | Check grammar-neutral identifiers and confirm no JSON provider, JSON helper rename, generic JSON policy, directive, or BIR variant is used as proof. | JSON-only emission or a hand-only non-JSON runtime is treated as generality. |
| CH3 REDRESS regression/preblocks | Cross-check REDRESS 34, 35, 36, 37, 38, 48, 85, 86, 87, 100, 101, 109, and hard preblocks. | Any preblocked route is reopened without routing out of W1a. |
| CH4 cost and budget | Keep W1a to C9 gate/report scope, <=260 handwritten source/test/gate LOC in the later redress, and 0 generated LOC unless fixtures are named. | The plan hides behavior work inside schema plumbing or exceeds W1a's budget. |
| CH5 hidden coupling and Lock 1 | Verify Track 2/oracle independence fields are meaningful and consumed; no hidden sidecar, parser-owned projection, or shared parser path appears. | Gate fixtures cannot distinguish independent oracle from shared parser evidence. |
| CH6 anti-paper-close and next-wave impact | Confirm W1a only enables W1b and W2; it does not close non-JSON generality, baseline authority, or row movement. | The plan claims close from schema presence, gate-only consumers, or deferred evidence. |

## R6 Recommendation

W1a should proceed only as a gate/report schema research-to-plan handoff. The
plan should name the fixture matrix and the exact fail-closed predicates, then
send CHALLENGE directly at Lock 14 generality, Track 2/oracle independence,
producer-only telemetry, and wave-boundary leakage. Any row-moving or generated
non-JSON proof belongs to W1b or later.
