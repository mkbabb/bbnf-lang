# CH6 S-P3 V2 Anti-Paper-Close Challenge

## Scope

Challenge lens: CH6 anti-paper-close for SK-V8 S-P3 V2. Reviewed `restart/prompts/ORCHESTRATOR.md` Sections 3W/3Z, `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`, `restart/prompts/pass-contracts/PASS-ALPHA.md`, live `restart/skinny/tranches/sk-v8/SPEC.md`, `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`, `restart/skinny/tranches/sk-v8/HANDOFF.md`, `restart/skinny/tranches/sk-v8/research/p3/p3-v2-hardening-fold.md`, P3-A through P3-F, `restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md`, plus current `skinny/RESULTS.md` and `skinny/REDRESS.md` where needed to verify close status.

## Verdict

ACCEPT, 96% confidence.

The V2 packet is not paper-closing G-Alpha, W0, W3, SK-V8 close, or SK-V9 planning. The V1 REVISE folds that could have weakened CH6 discipline were folded into live measurable gates, LOC/time split gates, and dispatch locks. No blocking CH6 defect remains.

## Blockers

None.

## Evidence

1. CH6 standard is satisfied.
   - `restart/prompts/ORCHESTRATOR.md:74-88` defines CH6 as rejecting "complete", "wired", or "verified" claims without live evidence and forbids deferral.
   - `restart/prompts/ORCHESTRATOR.md:104-121` requires per-pass challenge/fold convergence; `restart/prompts/ORCHESTRATOR.md:116` says hardening without folding is paper-hardening.
   - `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:140-145` requires every S-P3 wave to close on measurement, not a future promise, and to carry a revert protocol plus named same-wave consumer.
   - `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:270-273` blocks wave dispatch without S-P3 convergence, falsifiability gates, named rows, thresholds, and no future-phase close.

2. V1 blockers were folded without creating a CH6 regression.
   - V1 consolidated was REVISE because CH1 and CH4 required folds; CH6 was ACCEPT but below the 95% qualifying floor: `restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md:7-27`.
   - Required V2 folds included W2 typed seed gates, per-wave LOC budgets, W3 pre-redress fit estimates, citation hygiene, and future-artifact naming discipline: `restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md:29-52`.
   - V2 folded those items into SPEC/DISPATCH/HANDOFF, including W3 fit estimates and over-budget split/REVISE before redress: `restart/skinny/tranches/sk-v8/research/p3/p3-v2-hardening-fold.md:16-25`.
   - V2 packet state preserves the dispatch lock and requires W1-W6 to have W0 close, exact plan, challenge acceptance, owners, same-wave consumer, row gates, revert protocol, LOC/time fit, and REDRESS routing: `restart/skinny/tranches/sk-v8/research/p3/p3-v2-hardening-fold.md:27-36`.

3. G-Alpha is not paper-closed.
   - SPEC says no implementation dispatches from S-P3, G-Alpha user signoff remains required, `G-Alpha closed` initially dispatches W0 only, and W1-W6 remain blocked until W0 plus further gates: `restart/skinny/tranches/sk-v8/SPEC.md:29-36`.
   - DISPATCH repeats that no SK-V8 implementation wave dispatches from S-P3 alone and that G-Alpha signoff is still required: `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:6-9`.
   - HANDOFF says G-Alpha is still required, only W0 is dispatchable if it closes, and W1-W6 need W0 closure and plan augmentation: `restart/skinny/tranches/sk-v8/HANDOFF.md:5-7`.
   - DISPATCH states current G-Alpha status is not closed and forbids implementation until the user signs off: `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:215-222`.

4. W0 is not paper-closed.
   - W0 entry requires G-Alpha closed, the SK-V7 close baseline, and a W0 plan naming `SK-V8-open` capture plus no-behavior proof: `restart/skinny/tranches/sk-v8/SPEC.md:343-348`.
   - W0 exit requires all 38 rows to satisfy required telemetry, throughput within +/-1.0%, malformed sidecar rejection, and no parser/scanner/SIMD/asm/codegen/product behavior change: `restart/skinny/tranches/sk-v8/SPEC.md:360-370`.
   - W0 same-wave consumer is `gate-json` consuming every emitted telemetry field; W0 revert restores report/gate/schema/RESULTS and records REDRESS: `restart/skinny/tranches/sk-v8/SPEC.md:372-383`.
   - DISPATCH requires W0 focused tests, updated `gate-json`, required telemetry on every row, throughput stability, no behavior change, and FAIL -> REDRESS: `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:56-89`.

5. W3 is not paper-closed.
   - W3 is explicitly not pre-authorized beyond its plan, and the plan must name exact files before implementation: `restart/skinny/tranches/sk-v8/SPEC.md:506-525`.
   - W3 entry requires W0/W1 admission, fresh plan, exact owners/rows, same-wave production consumer, revert protocol, measured-path proof, Lock 1 fork disposition, scalar/checkasm requirements, pre-block differences, LOC/time estimate, challenge acceptance, and Pass Omega/Lock 1 handling: `restart/skinny/tranches/sk-v8/SPEC.md:536-549`.
   - W3 exit requires selected rows to cross post-W0 thresholds, all 38 rows to maintain, measured-row proof, exactly one retained tape, no parser-owned cursor/fact slots, scalar/checkasm parity, view/ValueRef parity, Track 2 independence, Lock 14 proof, and parse-only non-admission unless separately proven: `restart/skinny/tranches/sk-v8/SPEC.md:565-582`.
   - W3 same-wave consumer is generated JSON retained Track 1 parsing plus retained view/ValueRef touched or proven untouched; telemetry-only row counts are rejected: `restart/skinny/tranches/sk-v8/SPEC.md:584-586`.
   - W3 revert protocol reverts runtime/tape, SIMD, codegen templates, generated JSON output, retained view/value, gates, RESULTS, and REDRESS as one slice, saves rejected patch, and adds REDRESS: `restart/skinny/tranches/sk-v8/SPEC.md:596-603`.

6. Every wave has measurable gates, same-wave consumers, revert/REDRESS/routing, and cap discipline.
   - Global close requires W0 telemetry, W1 CostFacts, behavior waves meeting named thresholds or rejecting with REDRESS, no pre-block reopen without fresh W0 evidence/same-wave consumer/REDRESS/no-regression/challenge, Lock 14/15, and docs agreement: `restart/skinny/tranches/sk-v8/SPEC.md:42-59`.
   - SPEC non-negotiables forbid source changes without same-wave hot-path consumers, route every miss to REDRESS or explicit residual, and reject "wired", "advisory", "future consumer", "integrated", or "paper close" close language without measured evidence: `restart/skinny/tranches/sk-v8/SPEC.md:230-251`.
   - Per-wave LOC budgets and 90-minute redress caps are explicit; over-budget plans must split before dispatch or return REVISE: `restart/skinny/tranches/sk-v8/SPEC.md:253-284`.
   - W1 gate/revert: `restart/skinny/tranches/sk-v8/SPEC.md:402-440`.
   - W2 gate/consumer/revert: `restart/skinny/tranches/sk-v8/SPEC.md:460-504`.
   - W4 gate/consumer/revert: `restart/skinny/tranches/sk-v8/SPEC.md:621-661`.
   - W5 gate/consumer/revert/block: `restart/skinny/tranches/sk-v8/SPEC.md:678-713`.
   - W6 gate/consumer/revert/block: `restart/skinny/tranches/sk-v8/SPEC.md:730-765`.
   - DISPATCH repeats the conditional-wave rule: W1-W6 cannot dispatch from the prompt alone and must have W0 admitted, fresh research/plan, exact owners, row gates, same-wave consumer, revert protocol, REDRESS/P3 references, challenge when high-risk, and <=90 minute fit: `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:90-106`.

7. SK-V8 close is not paper-closed.
   - Current RESULTS still show deferred/view-boundary rows and `N-direct / NoGo`, not a V8 close: `skinny/RESULTS.md:3-42` and `skinny/RESULTS.md:216-219`.
   - HANDOFF states the same current measured authority: `N-direct / NoGo`, placeholder hot leaves, and sidecar planning caveats: `restart/skinny/tranches/sk-v8/HANDOFF.md:24-43`.
   - W6 cannot close unless W0-W5 each have admitted/rejected/routed status and REDRESS/RESULTS/HANDOFF updates are present: `restart/skinny/tranches/sk-v8/SPEC.md:730-734`.
   - W6 exit blocks missing REDRESS/RESULTS, unresolved Lock 1/Omega, sidecar/permissive strict admission, architecture analogy without row data, and dropped falsifier rows: `restart/skinny/tranches/sk-v8/SPEC.md:743-765`.
   - HANDOFF requires behavior waves to admit on named row gates or reject with REDRESS, and says no wave closes without REDRESS or an explicit no-source telemetry close: `restart/skinny/tranches/sk-v8/HANDOFF.md:139-149` and `restart/skinny/tranches/sk-v8/HANDOFF.md:181-182`.

8. SK-V9 planning is not paper-closed.
   - PASS-ALPHA consumes a completed SK-V{N} cycle with measured RESULTS, complete REDRESS, no open implementation work, and Omega run before creating SK-V{N+1}: `restart/prompts/pass-contracts/PASS-ALPHA.md:3-16`.
   - PASS-ALPHA states the detailed SK-V{N+1} wave plan is downstream S-P3 output, not W6 output: `restart/prompts/pass-contracts/PASS-ALPHA.md:3-5` and `restart/prompts/pass-contracts/PASS-ALPHA.md:112-123`.
   - PASS-ALPHA requires user G-Alpha signoff and says no SK-V{N+1} dispatch without G-Alpha, no G-Alpha without challenge convergence, and no challenge convergence without measurable row gates: `restart/prompts/pass-contracts/PASS-ALPHA.md:167-182` and `restart/prompts/pass-contracts/PASS-ALPHA.md:201-205`.
   - SPEC W6 routes residuals to SK-V9 or Pass Omega, but only as close reconciliation after W0-W5 dispositions; it does not author an SK-V9 contract or dispatch: `restart/skinny/tranches/sk-v8/SPEC.md:730-741`.

## Residual Non-Blocking Risks

1. W6 uses SK-V9 as a residual destination label. That remains acceptable only because PASS-ALPHA owns actual SK-V9 synthesis and G-Alpha; W6 must not expand routing into a V9 plan.
2. W3 may still be too large for one 90-minute slice, but V2 now blocks redress unless the fit estimate passes or the work splits/returns REVISE: `restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:76-84`.
3. P3-A through P3-F still contain some planning-floor language and future-plan wording, but the live SPEC/DISPATCH/HANDOFF packet controls dispatch and turns those into conditional gates, not close credit.

## Required Fold If REVISE

N/A. Verdict is ACCEPT.
