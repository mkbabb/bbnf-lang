# SK-V8 S-P3 Hardening V3 CH4 COST

Verdict: ACCEPT.

Confidence: 97%.

## Blockers

None.

The V3 citation fold preserved the V2 CH4 cost/feasibility controls. I found
no remaining LOC, W3 split-gate, 90-minute cap, generated-output, test, doc, or
revert-accounting loophole that would require REVISE.

## Evidence

The governing CH4 lens requires realistic LOC budget, risk class, wave
alignment, hard cap, and same-wave consumer per kernel or primitive
(`restart/prompts/ORCHESTRATOR.md:74-88`). S-P3 specifically requires every
wave to carry a LOC budget, hard cap, phase breakdown, and same-wave-consumer
requirement, with wave count <=12 and shortlist <=8
(`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:128-132`). ORCHESTRATOR
§3Z keeps the skinny wave bracket ceiling at 12 and requires unresolved REVISE
items to fold before convergence (`restart/prompts/ORCHESTRATOR.md:104-128`).

V1/V2 hardening history is resolved for CH4. V1 required explicit W0-W6
source/edit LOC budgets and a W3 pre-redress split gate estimating touched
source LOC, generated LOC, test LOC, gate/report LOC, and revert-slice size
(`restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md:39-47`).
V2 consolidated CH4 as ACCEPT at 96% and records that per-wave source/edit LOC
budgets plus the W3 pre-redress LOC/time split gate were fully folded into
SPEC, DISPATCH, and HANDOFF
(`restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md:23-31`).

V3 is a citation fold, not a cost-policy rewrite. It explicitly preserves
per-wave LOC/time budgets and the W3 pre-redress fit/split gate
(`restart/skinny/tranches/sk-v8/research/p3/p3-v3-citation-fold.md:37-49`).
P3-F V3 also states that CH4 remains preserved and that the citation fold does
not change LOC/time budgets, W3 split gates, dispatch lock, or same-wave
consumer requirements
(`restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:63-81`).

Per-wave source/edit LOC budgets remain explicit in the live SPEC. The wave
manifest carries W0 `0 production behavior LOC; <=350 report/gate/schema/test/doc
LOC`, W1 `0 parser/generated behavior LOC; <=300 CostFacts/report/gate/test
LOC`, W2 `<=650 source/test LOC`, W3 `<=450 source/test LOC default; <=650 only
with accepted pre-redress fit proof`, W4 `<=300 source/test LOC and <=3 selected
rows`, W5 `0 source LOC default; <=150 named Lock 14 cleanup LOC`, and W6 `0
source LOC; docs/RESULTS/REDRESS/HANDOFF/SPEC reconciliation only`
(`restart/skinny/tranches/sk-v8/SPEC.md:253-263`).

The same budget table is mirrored in DISPATCH and HANDOFF. DISPATCH carries the
W0-W6 source/edit LOC budget column with the same values
(`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:33-43`). HANDOFF carries
the matching dispatch-posture table
(`restart/skinny/tranches/sk-v8/HANDOFF.md:98-108`).

No generated-output, test, or doc loophole remains. SPEC makes LOC budgets
conjunctive with the 90-minute cap and rerun ceilings; counts hand-edited
source, tests, gate/report/schema code, and hand-written doc/result edits; and
requires every generated file to be named, diff-audited, and included in the
revert slice. Any wave exceeding either LOC budget or time cap must split before
dispatch or return REVISE (`restart/skinny/tranches/sk-v8/SPEC.md:265-271`).
SPEC separately keeps implementation/redress at 90 minutes maximum including
source edits, generation, verification, RESULTS/REDRESS updates, and rollback
(`restart/skinny/tranches/sk-v8/SPEC.md:273-284`). DISPATCH repeats that
generation, verification, RESULTS/REDRESS updates, rollback, generated-output
diff audit, and over-limit split/REVISE are mandatory
(`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:45-54`). HANDOFF repeats
the same generated-output diff/revert and LOC/time overflow rule
(`restart/skinny/tranches/sk-v8/HANDOFF.md:113-117`).

W3 retains an objective pre-redress fit/split gate. The live SPEC requires the
exact W3 plan to estimate touched source/test LOC, generated LOC, gate/report
LOC, docs/RESULTS/REDRESS edits, and the revert slice; if the estimate exceeds
the W3 LOC budget or the 90-minute implementation/redress cap, W3 must split
before dispatch or return REVISE
(`restart/skinny/tranches/sk-v8/SPEC.md:536-545`). DISPATCH repeats the same
W3 fit gate and requires split/REVISE before implementation
(`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:127-136`).

Rerun ceilings remain cost-bounded. SPEC lists focused verification and rerun
ceilings for W0-W6, including generated diff audit where relevant, and states
that extra reruns are REDRESS cost evidence rather than retry room
(`restart/skinny/tranches/sk-v8/SPEC.md:286-298`). P3-C preserves the same
90-minute implementation normalization and says candidates that cannot be
implemented, measured, and admitted or reverted inside 90 minutes must split or
return REVISE before redress
(`restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:22`,
`restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:312-314`).

Wave count and shortlist remain within caps. SPEC lists W0-W6 only
(`restart/skinny/tranches/sk-v8/SPEC.md:253-263`), and P3-A keeps the shortlist
at seven candidate waves, under the cap of eight
(`restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:12`,
`restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:20-28`).

Same-wave consumer and revert accounting remain intact. SPEC globally forbids
any primitive, kernel, generated path, or substrate representation without a
same-wave hot-path consumer (`restart/skinny/tranches/sk-v8/SPEC.md:245-247`).
For W3, generated JSON retained Track 1 parsing remains the same-wave consumer,
and runtime/tape, SIMD, codegen templates, generated JSON output, retained
view/value, gate, RESULTS, and REDRESS changes all revert as one slice
(`restart/skinny/tranches/sk-v8/SPEC.md:584-598`).

## Residual Non-Blocking Risks

- W3 remains cost-sensitive because Tier A spans runtime/tape, SIMD, generated
  parser, view/value, gate, RESULTS, and REDRESS surfaces. The V3 packet handles
  that risk by making LOC/time fit a pre-redress gate rather than accepting a
  broad implementation by default.
- Generated output volume could still be large, but it is explicitly named,
  diff-audited, included in revert accounting, and counted against the
  90-minute implementation/redress cap.
- P3-A through P3-E still carry some high-level time-cap language, but P3-F,
  SPEC, DISPATCH, HANDOFF, and the V3 citation fold are the live folded
  authorities and do not relax CH4.

## Required Fold If REVISE

None. No CH4 V3 fold is required.
