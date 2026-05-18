# SK-V9 Alpha Hardening V1 CH4 - Cost

Verdict: REVISE.
Confidence: 86%.

## Scope

Cost lane reviewed the SK-V9 Alpha packet for LOC budgets, risk class,
same-wave consumer proof, hard-cap binding, candidate count, wave alignment, and
whether the packet is implementable after G-Alpha without paper deferrals.

The absence of `SPEC.md` and `DISPATCH-PROMPT.md` is not itself a defect:
PASS-ALPHA says the detailed wave plan is downstream S-P3 work, and the SK-V9
packet repeats that boundary. Citations:
`restart/prompts/pass-contracts/PASS-ALPHA.md:3`,
`restart/prompts/pass-contracts/PASS-ALPHA.md:53`,
`restart/prompts/pass-contracts/PASS-ALPHA.md:114-122`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:5-9`, and
`restart/skinny/tranches/sk-v9/HANDOFF.md:86-90`.

## Findings

### F1 - REVISE - Alpha has LOC budgets but no candidate-level hard-cap / wave-alignment binding

PASS-ALPHA's CH4 lane asks whether each intervention has LOC budget, risk
classification, wave alignment, and same-wave consumer
(`restart/prompts/pass-contracts/PASS-ALPHA.md:43`). The G-Alpha presentation is
also expected to summarize rows, interventions, LOC budget, hard caps, and
pre-blocked routes (`restart/prompts/pass-contracts/PASS-ALPHA.md:167-176`).
The orchestrator's CH4 lens likewise requires LOC budget, risk class, wave
alignment, hard cap realism, and same-wave consumer
(`restart/prompts/ORCHESTRATOR.md:86`).

Alpha-E supplies per-candidate LOC and risk entries, for example 300 LOC for
typed row-table admission, 450 LOC for the retained grammar proof, 600 LOC for
direct output/control, 500 LOC for sidecar manifest, and 450 LOC for SK-V9-open
telemetry (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:96-105`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:188-198`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:273-282`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:355-362`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:427-435`).
But the Alpha packet does not bind those budgets to an implementation/redress
hard cap or a wave-alignment table before G-Alpha. SK-V8's accepted SPEC shape
made this binding explicit: every wave had a LOC budget plus `<=90 min`
implementation/redress cap, and plans exceeding either had to split before
dispatch or return REVISE (`restart/skinny/tranches/sk-v8/SPEC.md:216-245`).

This is a G-Alpha cost defect, not a request to pre-author the v9 SPEC. The
fold needed is a small Alpha-level cost matrix saying, for each Alpha-E
candidate, whether it is selected, enabling-only, or demoted; its LOC budget;
risk class; downstream wave alignment; same-wave consumer; and mandatory
`<=90 min` implementation/redress cap with a split-before-dispatch rule.

### F2 - REVISE - Candidate count is within cap, but the contract surface is inconsistent

PASS-ALPHA caps Alpha-E at `<=5` candidate interventions and requires each to
carry file paths, scalar/checkasm status, same-wave consumer plan, falsifiability
gate, LOC budget, and risk classification
(`restart/prompts/pass-contracts/PASS-ALPHA.md:26`). Alpha-E contains exactly
five candidate sections:
typed measured row-table admission, retained class/event grammar proof, direct
output/control-path contract, comparator/sidecar same-run manifest, and W0
telemetry/gate refresh
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:35`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:116`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:209`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:293`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:373`).
So the raw count passes.

The contract surface then narrows the set without disposition. SYNTHESIS says
W6 routes exactly three SK-V9 Alpha planning candidates
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:36-42`), and Alpha-F says the
candidate set is intentionally narrow with three skinny candidates plus Pass
Omega residuals (`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:66-75`).
Those surfaces omit the sidecar same-run manifest and SK-V9-open telemetry/gate
refresh candidates, even though Alpha-E gives each a LOC budget, risk, gates,
and same-wave consumer plan
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:327-357`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:400-429`).

Required fold: either carry all five through the G-Alpha packet with cost/cap
bindings, or demote candidates 4 and 5 explicitly as downstream enabling
signals outside the Alpha shortlist. Leaving five in Alpha-E and three in
SYNTHESIS/HANDOFF gives S-P3 an ambiguous candidate pool.

### F3 - REVISE - Structural proof is not yet implementable as a row-moving candidate under the stated caps

The retained class/event grammar route is correctly pre-blocked before source
work: SK-V8 REDRESS 92 says the W3 owner surface spanned SIMD, JSON scan, tape
layout, generated retained parser, retained view/value, codegen templates, bench
parity/materialization/gate code, and row reporting, exceeding the W3 LOC and
90-minute caps (`skinny/REDRESS.md:2677-2681`). The SK-V8 handoff repeats that
only a retained class/event grammar plus `ValueRef` cursor proof may precede a
renewed measured structural parse row wave
(`restart/skinny/tranches/sk-v8/HANDOFF.md:203-216`).

Alpha-E narrows the candidate to a proof tranche and says the same-wave consumer
is the `ValueRef` cursor proof, while no measured parse row can admit unless a
generated retained Track 1 consumer lands in the same wave
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:156-163`).
It budgets only 450 source/test LOC for that proof and says broader
parser/tape production rewrite should split later
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:188-192`).

SYNTHESIS, however, maps all parse-row targets to "retained class/event grammar
+ `ValueRef` cursor proof" as the candidate
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:107-125`) and states the precursor
needs an accepted proof artifact plus selected-row thresholds before any
implementation wave (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:90-95`).
That is cost-ambiguous: a proof-only wave can be implementable, but it cannot
also be the row-moving parse candidate unless the generated retained consumer is
named, same-wave, and fits the 90-minute/LOC cap.

Required fold: classify this route as proof-only with no row movement, or name
the generated retained Track 1 consumer and cap-fit plan. If the production
consumer cannot fit the Alpha cost envelope, the parse rows must remain
non-admission guard targets until S-P3 creates a separate challenged wave.

### F4 - ACCEPT WITH FOLD - Same-wave consumer language exists, but must be lifted into the contract

Alpha-E has same-wave consumer plans for all five candidates:
typed row-table admission consumes metadata in `gate-json`
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:66-73`);
retained grammar uses `ValueRef` proof and requires generated retained Track 1
consumer before row admission
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:156-163`);
direct output/control consumes the contract in gate/report rows
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:243-249`);
sidecar manifest is parsed and validated by `gate-json`
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:327-333`);
and SK-V9-open telemetry is produced and consumed by `gate-json`
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:400-406`).

The issue is placement: SYNTHESIS only makes "Same-wave consumer class" a
required telemetry field (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:233`)
instead of carrying a candidate-by-candidate consumer matrix. Lift Alpha-E's
consumer plans into the Alpha contract surface so G-Alpha can verify them without
implicitly deferring the proof to S-P3.

## Required Folds

1. Add an Alpha cost matrix to `SYNTHESIS.md` or Alpha-F, and mirror the summary
   in `HANDOFF.md`: candidate, selected/demoted status, owner-path family, LOC
   budget, risk class, downstream wave alignment, same-wave consumer, hard cap
   `<=90 min`, split-before-dispatch rule, and expected row effect.
2. Reconcile Alpha-E's five candidates with SYNTHESIS/Alpha-F's three-candidate
   contract. Carry candidates 4 and 5 with caps or explicitly demote them.
3. Fix the retained class/event grammar candidate so it is either proof-only
   with no row movement or a capped same-wave production-consumer plan. Do not
   let "proof now, production later" satisfy parse-row thresholds.
4. State that any future S-P3 wave plan exceeding either LOC budget or the
   `<=90 min` implementation/redress cap returns REVISE before dispatch.

## Blockers To G-Alpha

G-Alpha should not be presented as `closed` while F1-F3 are open. The packet can
move to G-Alpha after the folds above because the remaining structure is sound:
candidate count is not over the Alpha cap, the absence of a v9 SPEC is
contract-compliant, and the pre-blocked SK-V8 routes are preserved.
