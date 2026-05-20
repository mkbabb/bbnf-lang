# SK-V12 Pass Alpha CHALLENGE V2 - CH4 Cost / Scope

Date: 2026-05-20.
Lens: CH4 cost / scope.
Scope: Pass Alpha SK-V11 -> SK-V12 after commit `18f4b931`, V1 CH4 and
CONSOLIDATED, revised Alpha-E/F, SYNTHESIS, and HANDOFF.
Output: `restart/skinny/tranches/sk-v12/research/alpha-hardening/V2/CH4.md`.

## Overall Disposition

ACCEPT.

V1 CH4 returned REVISE because the Alpha packet lacked an Alpha-level hard-cap
surface, split triggers, executable baseline preflight, and a corrected E3 risk
class. The revised packet now carries those controls on the Alpha-E candidate
surface, mirrors the cap seed through Alpha-F/SYNTHESIS/HANDOFF, and preserves
same-wave consumer plus E5 reject discipline. No CH4 blocker remains.

## Disposition Matrix

| Area | Disposition | Evidence |
|---|---|---|
| Hard caps | ACCEPT | Alpha-E binds E1-E5 to 30 min plan and 75 min redress caps, and returns REVISE before behavior dispatch if a candidate exceeds LOC or redress cap (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:41`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:51`). Alpha-F, SYNTHESIS, and HANDOFF mirror the same cap seed (`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:193`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:203`, `restart/skinny/tranches/sk-v12/SYNTHESIS.md:186`-`restart/skinny/tranches/sk-v12/SYNTHESIS.md:196`, `restart/skinny/tranches/sk-v12/HANDOFF.md:92`-`restart/skinny/tranches/sk-v12/HANDOFF.md:103`). |
| Cost/cap matrix | ACCEPT | The Alpha-E matrix names every candidate, wave slot, LOC budget, risk, plan cap, redress cap, same-wave consumer, split rule, and revert protocol (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:35`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:47`). |
| Split triggers | ACCEPT | E1-E3 split when selected-grammar preflight cannot prove the generated seam/runtime/oracle/fixture/smoke/gate surface; E4 rejects before redress without an admitted baseline row id and Mbps; E5 rejects before non-JSON priority resolution or splits multi-row JSON work without microbench proof (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:43`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:47`). |
| Baseline preflight | ACCEPT | E1/E2/E3 require a proof-only preflight before behavior redress: named emission seam or runtime path, no JSON-only profile route as proof, runnable fixture plus independent oracle, compile/equality smoke, and REDRESS 111 gate consumption (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:53`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:68`). Alpha-F, SYNTHESIS, and HANDOFF carry the same pre-gate (`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:78`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:83`, `restart/skinny/tranches/sk-v12/SYNTHESIS.md:42`-`restart/skinny/tranches/sk-v12/SYNTHESIS.md:46`, `restart/skinny/tranches/sk-v12/HANDOFF.md:52`-`restart/skinny/tranches/sk-v12/HANDOFF.md:55`). |
| E3 risk class | ACCEPT | E3 is now medium-high in the Alpha matrix and candidate body, with the smaller-grammar exception conditioned on S-P1 proving a materially smaller runnable Track 1 plus independent oracle path (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:45`, `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:213`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:219`). |
| Same-wave consumers | ACCEPT | Each candidate names a same-wave row/gate consumer: E1 CSS gate row, E2 Sheets gate row, E3 BBNF-self gate row, E4 selected non-JSON row, and E5 selected JSON Track 1/Track 2 plus `gate-json` (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:99`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:101`, `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:151`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:153`, `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:199`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:200`, `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:255`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:257`, `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:323`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:325`). SYNTHESIS and HANDOFF make producer-only telemetry fail closed (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:70`-`restart/skinny/tranches/sk-v12/SYNTHESIS.md:72`, `restart/skinny/tranches/sk-v12/HANDOFF.md:81`-`restart/skinny/tranches/sk-v12/HANDOFF.md:88`). |
| E5 reject guard | ACCEPT | E5 remains lowest priority, requires fresh non-JSON-proven material evidence before dispatch, rejects JSON-only scheduling before the non-JSON priority resolves, and keeps REDRESS 114-119 routes pre-blocked unless materially different evidence exists (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:286`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:349`, `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:92`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:96`, `restart/skinny/tranches/sk-v12/SYNTHESIS.md:246`-`restart/skinny/tranches/sk-v12/SYNTHESIS.md:267`). |

## Findings

### CH4-1 - ACCEPT - Alpha hard caps are now auditable

The V1 finding asked for an Alpha-level cost/cap matrix that a G-Alpha reader
could audit before S-P3 authors SPEC. Alpha-E now provides the full matrix, and
Alpha-F/SYNTHESIS/HANDOFF seed the same maximum budget through the downstream
contract. The 75-minute redress cap matches the skinny redress cap shape of
60 minutes implementation plus 15 minutes measurement
(`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:65`-
`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:75`).

### CH4-2 - ACCEPT - Baseline work now has a preflight and split path

The V1 scope risk was that E1/E2/E3 could hide a first-of-class generator and
runtime unblock inside one baseline wave. The revised Alpha-E preflight prevents
that: if the selected grammar cannot prove the seam, runtime path, oracle,
fixture, compile/equality smoke, and gate consumer before behavior redress, S-P3
must split to generator/runtime unblock plus later baseline-report, or record a
measured `BLOCKED` route. This makes the REDRESS 112 cost explicit instead of
burying it in a 460-520 LOC baseline budget.

### CH4-3 - ACCEPT - E3 risk is corrected

E3 is no longer marked medium. The packet consistently marks BBNF-self
medium-high and keeps any downgrade contingent on S-P1 proving a materially
smaller runnable generated Track 1 plus independent oracle path. That resolves
the V1 risk-class correction without changing the fallback order.

### CH4-4 - ACCEPT - Same-wave consumer discipline holds

The candidate bodies name the hot row and gate/report consumer for every
candidate, and the synthesis/handoff surfaces fail closed on producer-only
telemetry. The HANDOFF cap table is intentionally compact, but the same-wave
consumer requirement is not lost because it is carried in the telemetry binding
and refusal conditions.

### CH4-5 - ACCEPT - E5 is guarded against JSON scope creep

E5 is still conditional, lowest priority, and not a standalone JSON retry. The
packet rejects scheduling it before the non-JSON priority resolves, requires
fresh material evidence beyond REDRESS 114-119, and requires same-wave gate-json
consumption. The "or explicitly blocked" branch does not reopen JSON first-wave
scope: it only applies after the non-JSON priority has been resolved as an
accepted success or measured block, and the fresh-material guard still applies.

## Required Folds

None under CH4.

## Blockers To G-Alpha

None under CH4. The revised packet is acceptable for cost/scope: hard caps are
stated, candidate costs are bounded, split triggers are explicit, the baseline
preflight is load-bearing, E3 risk is corrected, same-wave consumers are
present, and E5 remains reject-guarded.
