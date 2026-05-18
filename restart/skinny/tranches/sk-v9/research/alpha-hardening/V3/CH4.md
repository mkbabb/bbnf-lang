# SK-V9 Alpha Hardening V3 CH4 - Cost

Verdict: ACCEPT.
Confidence: 96%.

## Scope

CH4 reviewed the corrected SK-V9 Alpha packet at commit `32369fe8`
(`docs(sk-v9-alpha): fold V2 citation hardening`) against the V1 and V2
consolidated dispositions. The lane re-checked cost, LOC budgets, selected /
demoted status, same-wave consumers, `<=90 min` hard caps, proof-only retained
routing, no-deferral posture, and Alpha-depth scope.

## Findings

### F1 - ACCEPT - V2 correction does not reopen the cost lane

V2 consolidated records CH4 as ACCEPT at 96% confidence and says the only open
V2 defect was CH1's complete-table citation range in Alpha-B through Alpha-F
(`restart/skinny/tranches/sk-v9/research/alpha-hardening/V2/CONSOLIDATED.md:14-22`).
The V2 fold target required replacing `skinny/RESULTS.md:3-40` with
`skinny/RESULTS.md:3-42` in Alpha-B through Alpha-F
(`restart/skinny/tranches/sk-v9/research/alpha-hardening/V2/CONSOLIDATED.md:24-35`).
The corrected packet now carries the complete-table range in Alpha-B, Alpha-C,
Alpha-D, Alpha-E, and Alpha-F
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:22-39`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:25-30`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-D-validated-invalidated.md:24-43`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:21-33`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:29-40`).

No CH4 fold was requested by V2, and the V2 correction is citation-only, so the
cost acceptance survives.

### F2 - ACCEPT - Cost matrix, status, LOC budgets, and hard caps are G-Alpha-visible

PASS-ALPHA asks CH4 to verify LOC budget, risk classification, wave alignment,
and same-wave consumer per intervention
(`restart/prompts/pass-contracts/PASS-ALPHA.md:43`). The orchestrator CH4 lens
requires LOC budget, risk class, wave alignment, hard-cap realism, and
same-wave consumer (`restart/prompts/ORCHESTRATOR.md:86`).

The corrected `SYNTHESIS.md` carries a G-Alpha cost matrix for all five Alpha-E
entries with status, LOC budget, risk, downstream alignment, same-wave consumer,
hard cap, and expected row effect
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:106-120`). `HANDOFF.md` mirrors the
LOC budgets and `<=90 min implementation/redress` caps
(`restart/skinny/tranches/sk-v9/HANDOFF.md:57-65`). Alpha-F records that the
contract now carries LOC budget, hard cap, same-wave consumer, and expected row
effect (`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:48-62`).

### F3 - ACCEPT - Candidate status is reconciled without an unstated sixth route

PASS-ALPHA caps Alpha-E at `<=5` candidate interventions
(`restart/prompts/pass-contracts/PASS-ALPHA.md:26`). The packet identifies three
W6 behavior candidates and two gate-only prerequisites
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:37-52`;
`restart/skinny/tranches/sk-v9/HANDOFF.md:39-51`). The cost matrix classifies
them as `Behavior candidate`, `Proof precursor`, or `Gate prerequisite`
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:111-117`), and Alpha-F repeats the
same alignment while excluding Pass Omega residuals from skinny implementation
scope (`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:72-85`).

Alpha-E also rejects storage-only W3 production, W4 scalar-parent folding, PMULL,
CTZ, bulk bitmap, tiny-string, Unicode escape, REDRESS 73 transfer, cap-16
direct, object-pair value-byte, and Pass Omega residual routes as shortlist
entries (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:495-506`).
That leaves no ambiguous selected/demoted route for S-P3 to infer.

### F4 - ACCEPT - Same-wave consumer and no-deferral gates are explicit

The Alpha cost matrix names the same-wave consumer for each retained entry:
`gate-json` for typed row metadata, `ValueRef` cursor proof for retained grammar,
gate/report row classifier for direct contract/control, `gate-json` for sidecar
manifest validation, and `gate-json` for SK-V9-open manifest production and
consumption (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:111-117`). Alpha-E gives
the detailed consumer plans for those five entries
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:83-90`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:177-184`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:270-276`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:363-372`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:447-453`).

The hard cap is fail-closed: any future S-P3 plan exceeding either its LOC budget
or the `<=90 minute` implementation/redress cap returns REVISE before dispatch
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:119-120`). This satisfies the
orchestrator hard-cap discipline
(`restart/prompts/ORCHESTRATOR.md:214-227`) without pushing over-budget work into
an unreviewed future phase.

### F5 - ACCEPT - Retained class/event route is proof-only at Alpha depth

REDRESS 92 routed W3 because the scanner/tape event model is not isomorphic and
the owner surface exceeded the W3 LOC and 90-minute caps
(`skinny/REDRESS.md:2663-2690`). The corrected SK-V9 packet preserves that cost
boundary. `SYNTHESIS.md` classifies the retained route as a `Proof precursor`,
with no `RESULTS.md` row movement unless a later capped generated retained Track
1 consumer lands in the same accepted wave
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:111-115`). The parse-row goalset
repeats that `parse_only` rows remain non-admission at Alpha depth unless future
S-P3 first defines a capped implementation wave with same-wave generated retained
Track 1 consumption, output-plane validation, strict validation posture, and
challenge acceptance (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:151-156`).

`HANDOFF.md` repeats the proof-only row effect
(`restart/skinny/tranches/sk-v9/HANDOFF.md:61-63`), Alpha-F states the same
Alpha-depth limit
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:78-84`),
and Alpha-E says proof-only artifacts do not create GO/SOTA rows
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:201-217`).

### F6 - ACCEPT - Alpha-depth scope and G-Alpha boundary remain intact

PASS-ALPHA assigns the detailed Section 4.4 wave plan to downstream S-P3 and
requires G-Alpha sign-off before SK-V9 dispatch
(`restart/prompts/pass-contracts/PASS-ALPHA.md:53-54`;
`restart/prompts/pass-contracts/PASS-ALPHA.md:112-123`;
`restart/prompts/pass-contracts/PASS-ALPHA.md:167-178`). The corrected packet
states that SK-V9 implementation is not dispatched, no `SPEC.md` or
`DISPATCH-PROMPT.md` is created by Alpha, G-Alpha is required first, and
downstream S-P3 authors the future wave plan
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:5-9`;
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:63-75`;
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:330-335`;
`restart/skinny/tranches/sk-v9/HANDOFF.md:5-8`;
`restart/skinny/tranches/sk-v9/HANDOFF.md:67-77`;
`restart/skinny/tranches/sk-v9/HANDOFF.md:107-113`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:11-13`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:103-105`).

This is Alpha-depth scoping, not a deferral, because the packet withholds row
movement from proof-only and gate-only work and makes over-cap plans REVISE
before dispatch.

## Required Folds

None from CH4.

## Blockers To G-Alpha

No CH4 blocker remains. From the cost lane, G-Alpha may proceed after the full
V3 challenge consolidation satisfies Pass Alpha convergence: >=95% ACCEPT, zero
open critical defects, no orphan REVISE, and preserved user sign-off boundary
(`restart/prompts/pass-contracts/PASS-ALPHA.md:180-189`;
`restart/prompts/ORCHESTRATOR.md:118-123`;
`restart/prompts/ORCHESTRATOR.md:159-172`).
