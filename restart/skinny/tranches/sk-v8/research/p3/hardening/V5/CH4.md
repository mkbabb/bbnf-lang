# SK-V8 S-P3 Hardening V5 CH4 COST

## Scope

CH4 V5 cost/feasibility challenge for the unchanged V4-folded S-P3 packet. Reviewed the live planning packet for cost realism, source/edit LOC ceilings, hard 90-minute implementation/redress cap, W3 split pressure, scalar/checkasm burden, same-wave consumers, generated-output accounting, and no-deferral governance. No SK-V8 implementation wave was reviewed as dispatchable.

## Verdict

ACCEPT.

Confidence: 97/100.

## Blockers

None.

## Evidence

- V4 consolidated hardening records a qualifying ACCEPT cycle with CH4 ACCEPT at 97 confidence, no REVISE/REJECT findings, and no open critical defect. It also states V4 is the first qualifying S-P3 ACCEPT after V3 REVISE and requires one more unchanged challenge cycle before S-P3 can converge; this V5 review finds no new CH4 defect in that unchanged packet.
- ORCHESTRATOR section 3W and PASS-3 keep CH4's assignment bounded to cost realism: wave count, LOC budget, hard cap, phase breakdown, and same-wave consumer pressure. The live packet remains within that frame: seven waves W0-W6, under the twelve-wave escalation threshold, and a seven-item shortlist under the eight-item limit.
- SPEC preserves explicit per-wave budgets: W0 has 0 production behavior LOC and <=350 report/gate/schema/test/doc LOC; W1 has 0 parser/generated behavior LOC and <=300 CostFacts/report/gate/test LOC; W2 has <=650 source/test LOC; W3 has <=450 source/test LOC by default and <=650 only with accepted pre-redress fit proof; W4 has <=300 source/test LOC and <=3 selected rows; W5 has 0 source LOC by default or <=150 named Lock 14 cleanup LOC; W6 has 0 source LOC. These budgets are conjunctive with the hard cap, not substitutes for it.
- SPEC, DISPATCH-PROMPT, and HANDOFF all mirror the <=90 minute implementation/redress cap as inclusive of source edits, generation, verification, RESULTS/REDRESS updates, and rollback. Generated outputs do not consume source LOC, but they must be named, diff-audited, reviewed, and included in the revert slice, closing the generated/test/doc loophole CH4 challenged in V1-V4.
- W3 remains objectively split-gated before redress: it must name the parse candidate, owner paths, rows, same-wave production consumer, revert slice, measured path, Lock 1 posture, scalar/checkasm proof, pre-block deltas, touched source/test LOC, generated LOC, gate/report LOC, docs/RESULTS/REDRESS edits, and time fit. If that exact W3 plan exceeds the W3 LOC budget or 90-minute cap, SPEC and DISPATCH require split before dispatch or REVISE.
- Scalar/checkasm burden is still explicit where primitive/kernel wiring is possible. W3 requires scalar oracle/checkasm before wiring and a same-wave generated retained parser consumer; W4 limits primitive work to selected rows and keeps SIMD/ASM expansion split-bound; W5 defaults to no primitive/source work except a named Lock 14 cleanup consumed by tests.
- Same-wave consumer and no-deferral rules remain enforced across SPEC, DISPATCH-PROMPT, HANDOFF, and P3-A through P3-F. No new directive, BIR, BackendShape, UnionTape, substrate, sidecar grammar dependency, parser-owned cursor, or tape_vs_tape production consumer is admitted. Tape_vs_tape remains telemetry/comparator evidence only unless a later wave supplies a named owner, LOC budget, tests, rerun ceiling, and production consumer.
- Implementation remains blocked. The packet still requires G-Alpha/W0 admission and per-wave research/plan/gate checks before any SK-V8 implementation wave. S-P3 alone does not dispatch W3 or any implementation wave.

## Residual Non-Blocking Risks

- W3 remains the highest cost-pressure wave because it combines strict union projection, retained parser consumption, generated-output review, scalar/checkasm validation, and strict benchmark gates. This is not a blocker because the packet now requires an objective pre-redress fit/split decision before W3 can consume implementation time.
- W4 row breadth and scalar/checkasm burden could force another split after W3 evidence lands. The current packet bounds W4 to <=3 selected rows and <=300 source/test LOC, so this is an execution risk, not an S-P3 planning defect.
- Generated-output review could still dominate elapsed time if W2/W3 owner paths are broader than estimated. The live packet accounts for that by making generated diffs named, reviewed, revert-bound, and counted against the 90-minute cap.

## Required Fold If REVISE

None. CH4 V5 finds no new critical cost or feasibility defect and therefore requires no fold.
