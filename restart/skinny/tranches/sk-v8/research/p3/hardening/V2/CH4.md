# SK-V8 S-P3 Hardening V2 CH4 COST

Verdict: ACCEPT.

Confidence: 96%.

## Scope

Reviewed `restart/prompts/ORCHESTRATOR.md` §3W/§3Z,
`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`,
`restart/skinny/tranches/sk-v8/SPEC.md`,
`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`,
`restart/skinny/tranches/sk-v8/HANDOFF.md`,
`restart/skinny/tranches/sk-v8/research/p3/p3-v2-hardening-fold.md`,
P3-A through P3-F, and
`restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md`.

## Blockers

None.

CH4 V1 was fully folded for cost/feasibility. The remaining W3 cost risk is
real, but the V2 packet now makes it a dispatch gate instead of an implicit
redress gamble.

## Evidence

Governing standard: CH4 requires stated realistic LOC budget, risk class, wave
alignment, hard cap, and same-wave consumer per kernel/primitive
(`restart/prompts/ORCHESTRATOR.md:74-88`). S-P3 repeats the CH4 checks: every
wave must carry a LOC budget, hard cap, phase breakdown, and same-wave-consumer
requirement; wave count must be <=12 and shortlist <=8
(`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:128-132`). V1 consolidation
required explicit W0-W6 source/edit budgets and a W3 pre-redress split gate
(`restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md:39-47`).

V2 fold disposition: the hardening fold states that SPEC, DISPATCH, and HANDOFF
now carry per-wave source/edit LOC budgets, conjunctive with the 90-minute cap,
and that W3 now requires a pre-redress fit estimate covering source/test LOC,
generated LOC, gate/report LOC, docs/RESULTS/REDRESS edits, and the revert
slice (`restart/skinny/tranches/sk-v8/research/p3/p3-v2-hardening-fold.md:18-22`,
`restart/skinny/tranches/sk-v8/research/p3/p3-v2-hardening-fold.md:27-40`).
P3-F mirrors the same V2 fold and preserves explicit per-wave source/edit LOC
budgets in SPEC, DISPATCH, and HANDOFF
(`restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:55-71`).

Per-wave budgets are explicit in the live SPEC. W0-W6 now have concrete
source/edit budget rows: W0 `0 production behavior LOC; <=350
report/gate/schema/test/doc LOC`, W1 `0 parser/generated behavior LOC; <=300
CostFacts/report/gate/test LOC`, W2 `<=650 source/test LOC`, W3 `<=450
source/test LOC default; <=650 only with accepted pre-redress fit proof`, W4
`<=300 source/test LOC and <=3 selected rows`, W5 `0 source LOC default; <=150
named Lock 14 cleanup LOC`, and W6 `0 source LOC; docs/RESULTS/REDRESS/HANDOFF/SPEC
reconciliation only` (`restart/skinny/tranches/sk-v8/SPEC.md:253-263`).

Budgets are mirrored in DISPATCH and HANDOFF. DISPATCH carries the same W0-W6
budget table (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:33-43`), and
HANDOFF carries the same W0-W6 source/edit LOC budget table
(`restart/skinny/tranches/sk-v8/HANDOFF.md:98-108`).

No generated/test/doc loophole remains for exceeding 90 minutes. SPEC makes LOC
budgets conjunctive with the 90-minute cap and rerun ceilings; it counts
hand-edited source, tests, gate/report/schema code, and hand-written doc/result
edits, while generated files must be named, diff-audited, and included in the
revert slice. A wave over either the LOC budget or 90-minute cap must split
before dispatch or return REVISE (`restart/skinny/tranches/sk-v8/SPEC.md:265-271`).
The phase cap separately states implementation/redress is 90 minutes maximum,
including source edits, generation, verification, RESULTS/REDRESS updates, and
rollback (`restart/skinny/tranches/sk-v8/SPEC.md:273-284`). DISPATCH repeats
the generation/verification/RESULTS/REDRESS/rollback inclusion and over-limit
split/REVISE rule (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:45-54`).
HANDOFF repeats that generated outputs are named, diff-audited, and included in
the revert slice, and that either LOC or time overflow forces split/REVISE
(`restart/skinny/tranches/sk-v8/HANDOFF.md:113-117`).

W3 now has an objective pre-redress LOC/time split gate. The SPEC W3 entry gate
requires the exact W3 plan to estimate touched source/test LOC, generated LOC,
gate/report LOC, docs/RESULTS/REDRESS edits, and the revert slice; if that
estimate exceeds the W3 budget or 90-minute implementation/redress cap, W3 must
split before dispatch or return REVISE
(`restart/skinny/tranches/sk-v8/SPEC.md:536-545`). DISPATCH repeats the same
W3 fit gate and requires split/REVISE before implementation
(`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:127-134`).

Rerun and wave-count governance remain cost-bounded. SPEC keeps W0-W6 rerun
ceilings and treats extra reruns as REDRESS cost evidence, not retry room
(`restart/skinny/tranches/sk-v8/SPEC.md:286-298`). The wave plan remains seven
waves, under the <=12 ceiling in ORCHESTRATOR §3Z
(`restart/skinny/tranches/sk-v8/SPEC.md:255-263`,
`restart/prompts/ORCHESTRATOR.md:118-128`). P3-A still holds the shortlist to
seven candidate waves, under the <=8 cap
(`restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:12`,
`restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:20-28`).

Same-wave consumer and revert discipline remain present. SPEC preserves the
global rule that no primitive, kernel, generated path, or substrate
representation may land without a same-wave hot-path consumer
(`restart/skinny/tranches/sk-v8/SPEC.md:245-247`). W3 specifically keeps
generated JSON retained Track 1 parsing as the consumer and includes generated
JSON output, gate, RESULTS, and REDRESS in the revert slice
(`restart/skinny/tranches/sk-v8/SPEC.md:584-598`).

## Residual Non-Blocking Risks

- W3 remains the main cost risk. The default `<=450 source/test LOC` budget is
  plausible only for a narrow Tier A slice; the `<=650` path is acceptable
  because it requires an accepted pre-redress fit proof and still cannot exceed
  90 minutes.
- Generated output can be large, but V2 blocks it from becoming hidden work by
  requiring naming, diff audit, inclusion in the revert slice, and time-cap
  accounting.
- P3-A/P3-B/P3-C still contain older prose that emphasizes time caps more than
  LOC caps, but P3-F, SPEC, DISPATCH, HANDOFF, and the V2 fold are now the live
  folded dispatch authorities. No contradiction relaxes the live cost gates.

## Required Fold If REVISE

None. No CH4 REVISE fold is required for V2.
