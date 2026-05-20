# SK-V12 Pass Alpha Hardening V3 - CH4 Cost / Tranche Budget

Date: 2026-05-20.
Lens: CH4 cost / tranche budget.
Scope: USER-PIN V1, Pass Alpha contract, skinny triumvirate caps, V2
consolidated hardening, current Alpha-E/F, SYNTHESIS, and HANDOFF after V2
folds.
Output: `restart/skinny/tranches/sk-v12/research/alpha-hardening/V3/CH4.md`.

## Verdict

PASS.

V3 remains CH4-clean. The V2 CH5 folds now present in Alpha-E qualify pre-pin
authority as context only after measured revalidation and add the local E2 JSON
guard refresh/demotion rule. Those folds do not merge waves, raise hand LOC
caps, remove generated LOC ceilings, or weaken rollback/failure actions. The
packet is still plausible for G-Alpha presentation under the cost/tranche lens.

## Checks

| Check | Disposition | Evidence |
|---|---|---|
| Pass/wave split | PASS | Alpha-F says it does not edit `SPEC.md`/`DISPATCH-PROMPT.md` and requires G-Alpha, then S-P1, S-P2, and S-P3 before wave authority (`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:11`-`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:16`, `:148`-`:171`). SYNTHESIS and HANDOFF mirror the same boundary (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:5`-`:11`, `:237`-`:254`; `restart/skinny/tranches/sk-v12/HANDOFF.md:168`-`:173`). |
| 20/15/30 caps | PASS | Alpha-E binds every wave seed to 20 min research, 15 min plan, and 30 min redress (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:73`-`:75`). Alpha-F and SYNTHESIS repeat 20/15/30 for W0-W5 (`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:215`-`:226`; `restart/skinny/tranches/sk-v12/SYNTHESIS.md:260`-`:271`). |
| Hand LOC caps | PASS | Alpha-E lists W1a/E2 <=360, W1b/E1 <=620, W2/E3 <=180, W3/E4 <=420, W4/E5 <=430, and W5 docs-only (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:58`-`:65`). Alpha-F and SYNTHESIS carry the same seed caps into G-Alpha (`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:215`-`:226`; `restart/skinny/tranches/sk-v12/SYNTHESIS.md:260`-`:271`). |
| Generated LOC ceilings | PASS | Alpha-E budgets generated output separately: W1a <=1200, W1b <=12000 unless S-P3 proves a tighter/full-stylesheet ceiling, W2 0, W3 <=4000 over E1, W4 <=2500 over current CSS runtime, and W5 0 (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:58`-`:65`). |
| O(N) discipline | PASS | Alpha-E requires S-P3 to report pre/post generated LOC for each generated output slice and return REVISE on unexplained O(N) growth (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:73`-`:75`). |
| W1a/W1b separation | PASS | Alpha-E states E2 and E1 are sequential, not one redress slice (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:67`-`:69`). Alpha-F requires W1a for `GrammarConfig` legality and W1b for CSS L4 row creation/comparator (`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:168`-`:171`). |
| V2 fold cost impact | PASS | The added E2 JSON guard rule is a gate/failure condition inside W1a, not a new implementation tranche: direct/typed guards refresh or record measured REDRESS demotion unless no JSON-producing path moved and `skinny/RESULTS.md` is proven unchanged (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:179`-`:184`). The pre-pin S-P artifacts are now context only after measured revalidation (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:23`-`:27`). |
| Rollback/failure actions | PASS | Alpha-E gives per-candidate rejected patch paths and dependency blockers for W1b, W1a, W2, W3, and W4 (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:135`-`:138`, `:188`-`:191`, `:227`-`:229`, `:282`-`:284`, `:341`-`:345`). Alpha-F/SYNTHESIS add the W0-W5 failure-action table (`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:215`-`:226`; `restart/skinny/tranches/sk-v12/SYNTHESIS.md:260`-`:271`). |

## Precise Folds

None required under CH4.

Non-blocking note: HANDOFF uses a compact seed table for W1a-W5 that lists hand
caps and failure paths, while Alpha-E/F and SYNTHESIS carry the explicit
20/15/30 campaign caps. That is acceptable for CH4 because the compact HANDOFF
table does not conflict with the folded cost authority.
