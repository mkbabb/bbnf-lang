# SK-V8 S-P3 Hardening V4 CH4 COST

Verdict: ACCEPT.

Confidence: 97%.

## Blockers

None.

V4 is an exact-traceability fold. It does not weaken the V2/V3 cost controls,
and the live packet still carries enforceable LOC/time gates, W3 split pressure,
scalar/checkasm burden, generated-output diff/revert accounting, and no-deferral
language. No open CH4 critical defect remains.

## Evidence

CH4's governing lens requires realistic LOC budget, risk class, wave alignment,
hard cap, and same-wave consumer per kernel/primitive
(`restart/prompts/ORCHESTRATOR.md:74-88`). S-P3 repeats the CH4 checks: every
wave needs a LOC budget, hard cap, phase breakdown, same-wave-consumer
requirement, wave count <=12, and shortlist <=8
(`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:128-132`). The S-P3 pass also
requires hardening folds before convergence and needs two consecutive qualifying
ACCEPT cycles with no open critical defects
(`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:153-169`;
`restart/prompts/ORCHESTRATOR.md:104-128`).

V4 preserved the cost semantics. The V4 fold says it changes traceability only
and preserves the G-Alpha/W0-only dispatch lock, W3 Tier A/Tier B split,
scalar/checkasm requirement, same-wave production consumer requirement,
per-wave 90-minute implementation/redress cap, source/edit LOC budgets, and
pre-blocked REDRESS coverage
(`restart/skinny/tranches/sk-v8/research/p3/p3-v4-exact-traceability-fold.md:28-39`).
The V3 consolidation required that V4 preserve all substantive V2/V3 gates,
including LOC/time gates
(`restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md:33-48`).

Per-wave LOC budgets and the 90-minute cap remain live in SPEC. Section 2 lists
W0-W6 only, with W0 `0 production behavior LOC; <=350
report/gate/schema/test/doc LOC`, W1 `0 parser/generated behavior LOC; <=300
CostFacts/report/gate/test LOC`, W2 `<=650 source/test LOC`, W3 `<=450
source/test LOC default; <=650 only with accepted pre-redress fit proof`, W4
`<=300 source/test LOC and <=3 selected rows`, W5 `0 source LOC default; <=150
named Lock 14 cleanup LOC`, and W6 `0 source LOC; docs/RESULTS/REDRESS/HANDOFF/SPEC
reconciliation only`, all capped at `<=90 min`
(`restart/skinny/tranches/sk-v8/SPEC.md:253-263`). SPEC makes those LOC budgets
conjunctive with the 90-minute cap and rerun ceilings; counts hand-edited
source, tests, gate/report/schema code, and hand-written doc/result edits; and
requires over-budget or over-time plans to split before dispatch or return
REVISE (`restart/skinny/tranches/sk-v8/SPEC.md:265-284`).

The live dispatch surfaces mirror the same controls. DISPATCH carries the same
W0-W6 source/edit LOC budget table and `<=90 min` implementation/redress cap
(`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:33-54`). HANDOFF mirrors the
source/edit LOC table, states that every implementation/redress slice is capped
at 90 minutes including source edits, generation, verification, RESULTS/REDRESS
updates, and rollback, and requires split/REVISE if either LOC or time is
exceeded (`restart/skinny/tranches/sk-v8/HANDOFF.md:98-117`).

W3 has an objective pre-redress fit/split gate. SPEC requires the exact W3 plan
to estimate touched source/test LOC, generated LOC, gate/report LOC,
docs/RESULTS/REDRESS edits, and the revert slice; if that estimate exceeds the
W3 LOC budget or the 90-minute cap, W3 must split before dispatch or return
REVISE (`restart/skinny/tranches/sk-v8/SPEC.md:536-545`). DISPATCH repeats the
same W3 pre-redress fit gate and requires split/REVISE before implementation
(`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:127-136`).

Generated-output and doc/test loopholes are closed. SPEC says generated outputs
do not consume source LOC, but every generated file must be named, diff-audited,
and included in the revert slice; the same paragraph keeps generated work under
the 90-minute cap and rerun ceilings
(`restart/skinny/tranches/sk-v8/SPEC.md:265-271`). DISPATCH repeats that
generated outputs must be named, diff-audited, and included in the revert slice,
and that generated output does not excuse source-budget overflow
(`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:45-54`). HANDOFF repeats the
same generated-output revert accounting
(`restart/skinny/tranches/sk-v8/HANDOFF.md:113-117`).

Scalar/checkasm and same-wave consumer burden remain explicit. SPEC forbids any
primitive, kernel, generated path, or substrate representation without a
same-wave hot-path consumer and requires scalar reference plus checkasm parity
before primitive wiring (`restart/skinny/tranches/sk-v8/SPEC.md:230-251`).
P3-C requires scalar reference, checkasm parity, same-wave hot-path consumer,
named bench rows, and symbol-path proof for any primitive or kernel
(`restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:24-32`).
P3-E repeats that SIMD/ASM work needs scalar reference, checkasm parity, asm
proof before production wiring, same-wave consumer, and REDRESS-backed revert on
failure (`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:103-116`).

W3's specific consumer and revert burden remain intact. P3-A says W3 needs a
scalar oracle for positions/classes and checkasm-style parity before wiring,
with generated JSON retained parsing as the same-wave production consumer
(`restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:27`).
SPEC Section 6 repeats that generated JSON retained Track 1 parsing is the
same-wave consumer, no telemetry-only row counts, and runtime/tape, SIMD,
codegen templates, generated JSON output, retained view/value, gate, RESULTS,
and REDRESS changes revert as one slice
(`restart/skinny/tranches/sk-v8/SPEC.md:584-598`).

No-deferral discipline remains explicit. SPEC forbids closure on "wired",
"advisory", "future consumer", "integrated", or "paper close" language without
measured evidence (`restart/skinny/tranches/sk-v8/SPEC.md:248-251`). P3-E says
a wave may not close by promising W0 profiles, comparator repair, CostFacts,
scalar/checkasm, a production consumer, REDRESS accounting, or non-regression
measurement in a later phase; missing evidence means the route is out of scope
or the failed implementation reverts and records REDRESS
(`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:18-20`).
HANDOFF keeps research, plan, and redress separate and forbids role merger
(`restart/skinny/tranches/sk-v8/HANDOFF.md:173-182`).

Wave split pressure is realistic. P3-A keeps the shortlist at seven candidate
waves under the cap of eight and excludes the old 900 LOC parser refactor as
violating the current 90-minute implementation cap unless split and
re-justified after W0
(`restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:14-18`).
P3-B keeps W0-W6 as seven waves and states every implementation/redress wave is
capped at 90 minutes inclusive of source implementation, focused verification,
generated-output review, gate refresh, RESULTS, REDRESS, and close documentation
(`restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md:20-30`).
P3-F preserves the seven-wave shortlist, explicit per-wave source/edit LOC
budgets, and notes W3 may still be too broad unless the LOC/time fit estimate
passes or the wave splits
(`restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:21-38`,
`restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:57-66`,
`restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:90-95`).

S-P2 V7 governance does not create a cost shortcut. V7 authorizes S-P3
Synthesis-Plan only, not SK-V8 implementation, W3 redress, or G-Alpha close
(`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md:13-20`).
It preserves cost, Tier A/Tier B, same-wave-consumer, scalar/checkasm, no new
substrate, and `tape_vs_tape` residual boundaries
(`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md:44-64`).

## Residual Non-Blocking Risks

- W3 remains the highest cost and schedule risk. The packet handles this by
  forcing exact owner files, selected rows, scalar/checkasm requirements,
  generated retained-parser consumption, revert-slice accounting, and an
  objective LOC/time split gate before redress.
- W2 and W3 may split after W0 because their thresholds and owner scopes depend
  on `SK-V8-open`; that is an intended cost control, not a blocker.
- Generated output volume could be large, but V4 keeps it named, diff-audited,
  reverted with the wave, and counted inside the 90-minute implementation cap.

## Required Fold If REVISE

None. No CH4 V4 fold is required.
