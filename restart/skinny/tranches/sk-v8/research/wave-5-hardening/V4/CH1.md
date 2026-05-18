# SK-V8 W5 Hardening V4 CH1 - Correctness

Target: `d3398a68a82ace5087b8b87b6cb1235fa4a8bc22` (`docs(sk-v8-wave5-plan): route V4 after V3 revise`).

Verdict: ACCEPT

Confidence: 96%

## Findings

No material CH1 blocker remains.

1. The V3 CH1 REDRESS fold is complete for active W5 assertions. The plan anchors REDRESS 36-38, 85, and 86 at `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:77-79` and `:123-125`. The research note now anchors the audit-scope assertion at `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md:26-29`, plus the residue/test, findings, V2-fold, and V4 challenge assertions at `:97-99`, `:113-116`, `:185-186`, and `:207-209`.
2. The active process text routes to V4, not stale V3. The live plan says `ready for V4 challenge` and `V4 must challenge the folded documentation and unchanged source packet` at `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:5` and `:135-139`. The research recommendation says `Proceed to a W5 V4 challenge` at `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md:195-210`. Remaining V3 mentions are historical REVISE summaries.
3. Current `RESULTS.md` anchors resolve. The research note cites `skinny/RESULTS.md:46-85` and `skinny/RESULTS.md:138-141` at `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md:135-137`, and both target ranges exist in the current file.
4. Cwd-qualified verification remains intact. The plan separates `From skinny/` commands from repository-root commands at `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:87-114`; the V1 fold summary preserves the same cwd split at `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md:128-134`.
5. Post-commit zero-drift and Lock 14 posture are coherent. The target commit is doc-only over W5 plan/research files; no source, generated output, `skinny/RESULTS.md`, or W6 handoff file moved. The live plan still requires no performance claim, no row-table refresh, and no W6 close until two consecutive qualifying ACCEPT cycles.

## Verification/Evidence

- `git rev-parse HEAD` resolved to the target commit `d3398a68a82ace5087b8b87b6cb1235fa4a8bc22`.
- `git show --stat --patch d3398a68 -- .../skv8-W5-plan.md .../skv8-W5-lock14-audit-research.md` shows only the V3-to-V4 routing and REDRESS-anchor documentation fold.
- Anchor checks resolved `skinny/REDRESS.md:460-515`, `skinny/REDRESS.md:2399-2427`, `skinny/REDRESS.md:2431-2464`, `skinny/RESULTS.md:46-85`, and `skinny/RESULTS.md:138-141`.
- `git diff --exit-code HEAD -- skinny/RESULTS.md skinny/crates/runtime/src/grammars/json skinny/crates/codegen/src/json_templates skinny/crates/bbnf-bench/src/generated_real_typed.rs skinny/crates/bbnf-bench/src/direct_struct.rs skinny/crates/ir/src skinny/crates/codegen/src skinny/crates/passes/src skinny/crates/parse-that-regex/src skinny/crates/bbnf-simd/src skinny/crates/runtime/src skinny/crates/bbnf/src skinny/xtask/src` returned clean.
- The forbidden generic Lock 14 symbol scan returned no matches; the generic codegen branch scan returned no matches. Provider-residency scan returned only `skinny/xtask/src/main.rs:124`, `:132`, `:183`, and `skinny/crates/codegen/src/json_provider.rs:57`, `:61`, matching the allowed provider/tooling posture.
- I did not run cargo or xtask commands because the requested write constraint permits only `restart/skinny/tranches/sk-v8/research/wave-5-hardening/V4/CH1.md`; those commands can write build or generated artifacts.

## Required Folds

None for CH1. Do not dispatch W6; keep W5 on V4 hardening and require the documented unchanged-packet re-challenge after any qualifying V4 ACCEPT cycle.
