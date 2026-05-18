# SK-V8 S-P3 Hardening V1 CH4 COST

Title: SK-V8 S-P3 V1 cost-governance review.

Scope: `restart/prompts/ORCHESTRATOR.md`,
`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`,
`restart/prompts/skinny/PASS-ALPHA.md`,
`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`, P3-A through
P3-F, live `restart/skinny/tranches/sk-v8/SPEC.md`,
`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`,
`restart/skinny/tranches/sk-v8/HANDOFF.md`, S-P2 SC-1 through SC-6 plus
V7 consolidated, `skinny/RESULTS.md`, and `skinny/REDRESS.md`.

Verdict: REVISE.

Confidence: 91%.

## Blockers

### B1 - Per-wave LOC budgets are not carried into the folded packet

`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:128-132` makes CH4's
cost check conjunctive: every wave must carry a LOC budget, hard cap, phase
breakdown, and same-wave-consumer requirement. The current packet satisfies
the minute-cap, phase, consumer, revert, and rerun parts, but not the
per-wave LOC-budget part.

Exact file references:

- `restart/skinny/tranches/sk-v8/SPEC.md:230-238` lists W0-W6 with only an
  implementation/redress minute cap column. `restart/skinny/tranches/sk-v8/SPEC.md:240-250`
  adds phase caps and the 90-minute split/REVISE rule, but still no LOC
  budget column.
- `restart/skinny/tranches/sk-v8/SPEC.md:294-563` gives W0-W3 owner paths,
  tasks, gates, same-wave consumers, and revert protocols without source LOC
  ceilings. `restart/skinny/tranches/sk-v8/SPEC.md:565-724` does the same
  for W4-W6 except for W5.
- `restart/skinny/tranches/sk-v8/SPEC.md:648-649` is the only explicit source
  LOC budget found: W5's named Lock 14 cleanup has a 150 source LOC cleanup
  cap. That does not cover W0, W1, W2, W3, W4, or W6.
- `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:35-49` mirrors only the
  W0-W6 minute caps and the split/REVISE rule. Its shared entry gate at
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:91-101` requires owner
  paths, row gates, same-wave consumer, revert protocol, challenge, and
  <=90 minutes, but no LOC budget.
- `restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md:32-40`
  sequences W0-W6 and gives role/cap language, but only W5 carries an
  explicit source LOC ceiling.
- `restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:55-63`
  preserves same-wave consumers and the 90-minute implementation/redress hard
  cap, but does not fold LOC budgets into the SPEC or dispatch surface.

This is a cost-governance blocker because the 90-minute cap alone does not
give dispatchers an objective pre-redress split trigger for code volume,
generated-output churn, or revert-size risk.

### B2 - W3 has a split rule, but no LOC-based feasibility gate

W3 is the highest-cost candidate. The plan correctly narrows it to Tier A and
requires challenge acceptance, exact owners, scalar/checkasm where relevant,
generated retained-parser consumption, generated audit, full-table maintain,
and rollback:

- `restart/skinny/tranches/sk-v8/SPEC.md:470-489`
- `restart/skinny/tranches/sk-v8/SPEC.md:500-509`
- `restart/skinny/tranches/sk-v8/SPEC.md:525-557`

The packet also admits the cost risk. `restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:67-70`
says W3 Tier A may be too broad for one 90-minute slice once scalar oracle,
checkasm, generated audit, retained view parity, and gate refresh are counted.
`restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:89-90` requires
splitting W3 if it cannot be implemented, measured, reported, and reverted
inside 90 minutes. That is directionally correct, but without a W3 source LOC
budget the split decision remains subjective at dispatch time.

Required fold: W3 must get an explicit source/edit LOC budget, or a mandatory
pre-redress split rule keyed to the exact W3 plan's touched source LOC,
generated LOC, test LOC, and revert slice. The SPEC should state that W3 cannot
start redress until that estimate proves the selected Tier A slice fits both
the LOC budget and the <=90-minute implementation/redress cap.

## Accepted Cost Evidence

- Shortlist size passes: P3-A names seven candidate waves, under the requested
  cap of eight (`restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:12`,
  `restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:20-28`).
- Wave count passes: the live manifest is W0-W6, seven waves, under the
  skinny-bracket ceiling of twelve (`restart/skinny/tranches/sk-v8/SPEC.md:230-238`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:35-43`).
- Hard-cap governance passes apart from LOC: research is capped at 30 minutes
  per agent, plan at 30 minutes, challenge at 90 minutes when required, and
  implementation/redress at 90 minutes inclusive of source edits, generation,
  verification, RESULTS/REDRESS updates, and rollback
  (`restart/skinny/tranches/sk-v8/SPEC.md:240-250`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:45-49`).
- Phase separation passes: research, plan, challenge when required, and redress
  remain distinct (`restart/skinny/tranches/sk-v8/SPEC.md:220-224`,
  `restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md:50-62`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:167-176`).
- Same-wave consumers and revert protocols are present for W0-W6:
  `restart/skinny/tranches/sk-v8/SPEC.md:338-346`,
  `restart/skinny/tranches/sk-v8/SPEC.md:395-403`,
  `restart/skinny/tranches/sk-v8/SPEC.md:455-464`,
  `restart/skinny/tranches/sk-v8/SPEC.md:544-557`,
  `restart/skinny/tranches/sk-v8/SPEC.md:608-618`,
  `restart/skinny/tranches/sk-v8/SPEC.md:662-670`, and
  `restart/skinny/tranches/sk-v8/SPEC.md:716-724`.
- Rerun ceilings pass: W0-W6 have focused verification and rerun ceilings, and
  extra reruns become REDRESS cost evidence
  (`restart/skinny/tranches/sk-v8/SPEC.md:253-265`).
- The packet does force split or REVISE before dispatch when a slice cannot fit
  the 90-minute cap (`restart/skinny/tranches/sk-v8/SPEC.md:249-250`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:100-101`). CH4's revise
  is limited to the missing LOC-budget half of that governance.

## V6/V7 Governance Disposition

V6/V7 governance is accepted only as S-P2 convergence into S-P3 planning.
It does not authorize G-Alpha, W0 dispatch, W3 redress, or any implementation
wave by itself (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md:13-20`).
The folded S-P3 packet preserves the required boundaries: strict-vs-strict
comparison, Tier A/Tier B separation, Lock 14 grammar-neutrality, no new
directive/BIR/substrate surface, and no `tape_vs_tape` production-consumer
substitution.

## Residual Non-Blocking Risks

- W3 remains cost-fragile even after the revise fold. Tier A crosses runtime,
  SIMD, generated parser, view/value, gate, RESULTS, and REDRESS surfaces; it
  is likely to split unless the exact W3 plan is very narrow.
- W0/W1 telemetry and CostFacts work are non-behavioral, but they touch report,
  gate, schema, and generated-output audit surfaces. Their LOC budgets should
  distinguish production behavior LOC from report/gate/schema LOC.
- W4's one-to-three direct-row limit is a useful cost guard, but it should be
  paired with an explicit source/generated/test LOC ceiling before dispatch.

## Required Folds If REVISE

1. Add a `Source/edit LOC budget` column to
   `restart/skinny/tranches/sk-v8/SPEC.md` Section 2 and mirror it in
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`.
2. Give W0-W6 explicit budgets. Budgets may be `0 production source LOC` for
   telemetry/docs/close waves, but each wave must still name the allowed
   report/gate/schema/test/generated/RESULTS/REDRESS edit class and its LOC
   ceiling.
3. Preserve W5's existing 150 source LOC cleanup cap and make clear whether it
   is production source LOC only or includes tests/generated/report edits
   (`restart/skinny/tranches/sk-v8/SPEC.md:648-649`).
4. Add a W3-specific pre-redress split gate: the exact W3 plan must estimate
   touched source LOC, generated LOC, test LOC, gate/report LOC, and revert
   slice; if any estimate exceeds the W3 LOC budget or the 90-minute cap, W3
   splits before redress or returns REVISE.
5. Mirror the accepted LOC budgets into P3-F/HANDOFF so later dispatch agents
   do not rely on the older 120-300 minute pre-P3 caps or prose-only scope
   boundaries.

Self-verdict: REVISE.
