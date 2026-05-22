# SK-V13 S-P3 V3 CH4 Cost Challenge

Lens: CH4 cost, wave alignment, hard caps, subwave accounting, and same-wave consumer cost.
Commit under review: HEAD `b5f58b75589bc33223bed810a776da652bc5bde5`, with the folded S-P3 packet at `9f8bbfce5`.
Disposition vocabulary: ACCEPT / REVISE / REJECT.

## Verdict

ACCEPT.

V3 finds no CH4 regression after the V2 accepted cycle. The CH4 contract asks
for LOC budgets, hard caps, phase breakdown, wave alignment, bracket ceiling,
shortlist <=8, and same-wave consumers (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:128`-`:132`).
The orchestrator binds CH4 to realistic LOC/risk/wave/hard-cap checks and
same-wave consumers, then requires two accepted cycles or user pin for
convergence (`restart/prompts/ORCHESTRATOR.md:83`-`:88`,
`restart/prompts/ORCHESTRATOR.md:118`-`:128`). The current packet satisfies
that cost surface: W0-W15 is canonical, subwaves are accounted, W5-W8 cannot
close as support-only, W10.N/W11.N/W13/W14.N have explicit consumer minimums,
SIMD/ASM costs include same-wave zero-orphan closure, and hard caps are stated.

No V4 CH4 fold item is required.

## Evidence

- HEAD is `b5f58b75589bc33223bed810a776da652bc5bde5`. `git show --name-only b5f58b755` shows that commit added only the V2 S-P3 hardening files and consolidated verdict; the cost-bearing SPEC/DISPATCH fold remains the `9f8bbfce5` packet.
- The V2 consolidation recorded `G-S-P3-V2-CHALLENGE: ACCEPT`, 6/6 acceptance, zero critical defects, and zero open REVISE dispositions (`restart/skinny/tranches/sk-v13/research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md:10`-`:17`). It specifically accepted canonical waves, subwave accounting, no support-only rejection, consumer minimums, and SIMD zero-orphan requirements for CH4 (`restart/skinny/tranches/sk-v13/research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md:27`-`:34`).
- V2 also preserved the governance fact that V3 must run as the second accepted cycle and that no W0/source/generated/gate/RESULTS/REDRESS work is authorized until S-P3 convergence and G-Omega close (`restart/skinny/tranches/sk-v13/research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md:46`-`:54`).
- The SK-V13 addendum forbids support-only landings and requires each primitive to wire same-commit to a consumer that moves a row (`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:96`-`:102`), while raising W5-W9 and W12 source redress caps to 45 minutes and leaving research/plan caps unchanged (`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:129`-`:134`).
- P3-A stays within the shortlist ceiling by listing P3A-0 as W0 governance plus P3A-1 through P3A-7 as the seven S-P2 survivor intervention families, with consumers and gates attached (`restart/skinny/tranches/sk-v13/research/p3/p3a-candidate-shortlist.md:10`-`:15`, `:70`-`:79`).
- P3-D makes generated and resolver-heavy LOC a gate-consumed telemetry field, so generated size cannot hide outside the budget (`restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:240`-`:256`), and `gate-json` rejects generated LOC opacity (`restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:258`-`:296`).

## Fold Items

| Cost item | Verdict | Evidence |
|---|---|---|
| Canonical W0-W15 manifest | ACCEPT | SPEC declares W0-W15 the canonical V2 dispatch manifest and demotes P3-B W0-W11 to V1 packing aliases (`restart/skinny/tranches/sk-v13/SPEC.md:314`-`:320`). DISPATCH repeats that the W0-W15 table is authoritative and that P3-B labels are aliases only (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:188`-`:194`). P3-B itself carries the same V2 fold note and mapping (`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:10`-`:18`). |
| Subwave accounting and bracket caps | ACCEPT | SPEC states W10.N, W11.N, and W14.N are planning subwave series until an accepted plan declares a real triumvirate, and each real subwave counts against active skinny-bracket accounting (`restart/skinny/tranches/sk-v13/SPEC.md:314`-`:320`). P3-B says packed subwaves are not permission to exceed hard caps and become real waves if dispatched separately (`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:97`-`:101`). If packing overflows, P3-B and DISPATCH require `REJECT-BRACKET` / SK-V14 bracket-forward without dropping pinned rows (`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:140`-`:156`; `restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:188`-`:194`). |
| Hard caps and phase breakdown | ACCEPT | SPEC gives every Pre-W0/W0-W15 entry an edit LOC budget and redress cap (`restart/skinny/tranches/sk-v13/SPEC.md:322`-`:340`), then restates research, plan, challenge, and redress phase caps with W5-W9/W12 at 45+15 (`restart/skinny/tranches/sk-v13/SPEC.md:347`-`:354`). DISPATCH mirrors the phase caps and states research/plan are read-only while redress may edit only accepted owner paths (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:49`-`:63`). |
| W5-W8 no support-only close | ACCEPT | SPEC globally forbids primitives, generated paths, resolvers, union substrate, or telemetry producers without same-wave measured consumers, and forbids support-only behavior waves (`restart/skinny/tranches/sk-v13/SPEC.md:297`-`:306`). W5 rejects support-only extraction (`restart/skinny/tranches/sk-v13/SPEC.md:584`-`:598`), W6 rejects bounded e-graph/cost telemetry alone (`restart/skinny/tranches/sk-v13/SPEC.md:620`-`:635`), W7 treats cascade retirement without row consumption as measured reject (`restart/skinny/tranches/sk-v13/SPEC.md:658`-`:676`), and W8 requires a touched JSON/CSS row to consume the policy surface and move/admit/block (`restart/skinny/tranches/sk-v13/SPEC.md:699`-`:713`). DISPATCH summarizes the same W5-W8 anti-paper-close rule (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:196`-`:201`). |
| W10.N / W11.N / W13 / W14.N consumer minimums | ACCEPT | DISPATCH has a required consumer table for the four subwave/row families (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:203`-`:210`). SPEC mirrors the same consumer lines for W10.N CSS (`restart/skinny/tranches/sk-v13/SPEC.md:774`-`:784`), W11.N direct (`restart/skinny/tranches/sk-v13/SPEC.md:810`-`:820`), W13 typed (`restart/skinny/tranches/sk-v13/SPEC.md:875`-`:886`), and W14.N parse-only (`restart/skinny/tranches/sk-v13/SPEC.md:912`-`:922`). |
| SIMD zero-orphan cost and orphan prevention | ACCEPT | The SK-V13 synthesis requires zero aarch64 production orphans, same-wave measured consumers or deletion/demotion, and no second `a64_ascii_set_run_skip` deferral (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:84`-`:93`). SPEC requires scalar/checkasm/consumer/feature-mask proof for SIMD, adds `G-SIMD-GRAMMAR-POLICY`, and binds W9/C3 to same-wave `orphan_count_after = 0` without later W12 cleanup (`restart/skinny/tranches/sk-v13/SPEC.md:391`-`:398`, `:748`-`:751`). W12 then requires zero aarch64 orphans and rejects checkasm-only or microbench-only admissions (`restart/skinny/tranches/sk-v13/SPEC.md:846`-`:854`). DISPATCH repeats that any SIMD-touching wave, including W9/C3, must leave `orphan_count_after = 0` in the same wave (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:212`-`:217`). |
| Ledger and rerun cost containment | ACCEPT | SPEC sets rerun ceilings by wave family and makes extra reruns REDRESS cost evidence, not retry room (`restart/skinny/tranches/sk-v13/SPEC.md:356`-`:368`). P3-B serializes RESULTS, REDRESS, and rolling-delta writes through one finalizer per wave/subwave and keeps failed lanes out of partial ledger edits (`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:217`-`:227`). DISPATCH requires admit/reject REDRESS updates with thresholds, run ids, host, same-wave consumer evidence, and revert slices (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:253`-`:267`). |

## Verification

- Reviewed ORCHESTRATOR Section 3W/3Z, `PASS-3-SYNTHESIS-PLAN.md`, `SKINNY-TRIUMVIRATE.md`, SK-V13 SYNTHESIS/HANDOFF/SPEC/DISPATCH, P3-A through P3-F, V1 CH4/consolidated, V2 CH4/consolidated, and the commit metadata for `9f8bbfce5` and `b5f58b755`.
- Local check: `git diff --check --no-index -- /dev/null restart/skinny/tranches/sk-v13/research/p3/hardening/V3/CH4.md` produced no whitespace diagnostics; nonzero exit is expected for a new file diff.
