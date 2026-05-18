# SK-V9 Alpha Hardening V2 CH4 - Cost

Verdict: ACCEPT.
Confidence: 96%.

## Scope

CH4 re-reviewed the V1-folded SK-V9 Alpha packet at commit `e3ebe0b4`.
The lane checked cost, LOC budgets, wave alignment, same-wave consumers,
`<=90 min` hard caps, candidate status, proof-only retained routing,
no-deferral posture, and Alpha-depth scope.

## Findings

### F1 - ACCEPT - V1 cost matrix fold is present on G-Alpha surfaces

V1 required an Alpha-level matrix with candidate status, LOC budget, risk,
downstream alignment, same-wave consumer, hard cap, split-before-dispatch rule,
and expected row effect
(`restart/skinny/tranches/sk-v9/research/alpha-hardening/V1/CONSOLIDATED.md:37-45`;
`restart/skinny/tranches/sk-v9/research/alpha-hardening/V1/CH4.md:141-153`).
The folded `SYNTHESIS.md` now carries that matrix for all five Alpha-E entries:
typed row-table admission, retained class/event proof, direct output/control,
same-run sidecar manifest, and SK-V9-open telemetry/gate refresh
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:106-120`). `HANDOFF.md` mirrors the
Alpha cost binding and the `<=90 min implementation/redress` cap for each entry
(`restart/skinny/tranches/sk-v9/HANDOFF.md:57-65`). Alpha-F also records that the
contract now carries LOC budget, hard cap, same-wave consumer, and expected row
effect (`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:57-60`).

This satisfies the CH4 contract: PASS-ALPHA asks this lane to verify LOC budget,
risk classification, wave alignment, and same-wave consumer
(`restart/prompts/pass-contracts/PASS-ALPHA.md:43`), and the orchestrator CH4
lens requires LOC budget, risk class, wave alignment, hard-cap realism, and
same-wave consumer (`restart/prompts/ORCHESTRATOR.md:86`).

### F2 - ACCEPT - Candidate status and count are no longer ambiguous

The folded packet reconciles Alpha-E's five sections with the three W6 behavior
residuals. `SYNTHESIS.md` now says W6 routes exactly three behavior candidates
and then separately names two gate-only prerequisites
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:37-52`). The cost matrix classifies
the entries as `Behavior candidate`, `Proof precursor`, or `Gate prerequisite`
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:111-117`). `HANDOFF.md` repeats that
only the three W6 behavior routes move into Alpha behavior scope, while the
sidecar manifest and SK-V9-open refresh are gate-only enablers
(`restart/skinny/tranches/sk-v9/HANDOFF.md:41-51`). Alpha-F matches the same
candidate alignment (`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:72-85`).

The candidate count remains within PASS-ALPHA's `<=5` Alpha-E cap
(`restart/prompts/pass-contracts/PASS-ALPHA.md:26`) without leaving S-P3 an
unstated sixth route or silently demoting a listed route.

### F3 - ACCEPT - Retained class/event route is proof-only at Alpha depth

V1's cost defect was that the retained route looked like both a proof and a
row-moving parse candidate. The folded packet resolves that. `SYNTHESIS.md`
states that parse rows remain non-admission unless a later S-P3 wave defines a
capped implementation with a same-wave generated retained Track 1 consumer,
output-plane validation, strict validation posture, and challenge acceptance
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:151-156`). Its cost matrix labels
the retained route a `Proof precursor` and says it has no `RESULTS.md` row
movement unless a later capped generated retained Track 1 consumer lands in the
same accepted wave (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:111-115`).
`HANDOFF.md` repeats the proof-only boundary
(`restart/skinny/tranches/sk-v9/HANDOFF.md:61-63`), and Alpha-F states the same
Alpha-depth limit (`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:80-82`).

Alpha-E's detailed retained candidate is now cost-coherent: the same-wave
consumer for the proof is the `ValueRef` cursor proof, no measured parse row can
admit without generated retained Track 1 consumption in the same wave, and the
proof tranche budget is 450 source/test LOC
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:177-184`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:213-217`).
That respects REDRESS 92's reason for routing W3: the production owner surface
spanned SIMD, scan, tape, generated parser, retained view/value, codegen, bench,
gate, and reporting, exceeding the prior LOC and 90-minute caps
(`skinny/REDRESS.md:2677-2681`).

### F4 - ACCEPT - Same-wave consumers and hard caps are explicit enough for Alpha

The folded matrix gives each entry a concrete consumer: `gate-json` for typed row
metadata, the `ValueRef` cursor proof for retained grammar, gate/report row
classifier for direct contract/control, `gate-json` for sidecar manifest
validation, and `gate-json` for SK-V9-open manifest production/consumption
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:111-117`). Alpha-E provides the
per-candidate detail behind those consumers
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:83-90`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:177-184`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:270-276`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:363-372`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:447-453`).

The hard cap is also fail-closed: any future S-P3 wave plan that exceeds the LOC
budget or `<=90 minute` implementation/redress cap returns REVISE before
dispatch (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:119-120`). This preserves
the orchestrator's hard-cap and no-deferral discipline
(`restart/prompts/ORCHESTRATOR.md:214-227`).

### F5 - ACCEPT - Alpha-depth scope is preserved without paper deferral

The absence of `SPEC.md` and `DISPATCH-PROMPT.md` remains contract-compliant.
PASS-ALPHA assigns the detailed Section 4.4 wave plan to downstream S-P3 after
Alpha (`restart/prompts/pass-contracts/PASS-ALPHA.md:53-54`;
`restart/prompts/pass-contracts/PASS-ALPHA.md:112-123`). The folded
`SYNTHESIS.md` and `HANDOFF.md` both state that SK-V9 implementation is not
dispatched, no `SPEC.md` or `DISPATCH-PROMPT.md` is created by Alpha, G-Alpha is
required first, and downstream S-P3 authors the future wave plan
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:5-9`;
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:63-75`;
`restart/skinny/tranches/sk-v9/HANDOFF.md:5-8`;
`restart/skinny/tranches/sk-v9/HANDOFF.md:67-77`).

This is not a deferral defect because the folded packet no longer claims row
movement from proof-only or gate-only work, and it makes over-cap downstream
plans REVISE before dispatch.

## Required Folds

None from CH4.

## Blockers To G-Alpha

No CH4 blocker remains. From the cost lane, G-Alpha may proceed after the full
V2 challenge consolidation satisfies the Alpha convergence rules and preserves
the user-controlled sign-off boundary
(`restart/prompts/pass-contracts/PASS-ALPHA.md:167-182`;
`restart/prompts/ORCHESTRATOR.md:118-123`).
