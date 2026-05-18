# SK-V8 W5 Hardening V5 CH1 - Correctness

Target: `42d5f034eee2a1931e46d13e7e20c62e49ca8c7a` (`docs(sk-v8-wave5-hardening): record V4 qualifying accept cycle`).

Verdict: ACCEPT

Confidence %: 95%

## Findings

No material CH1 blocker remains.

1. The V5 target is the required unchanged re-challenge of the accepted V4 packet. `42d5f034` has parent `d3398a68a82ace5087b8b87b6cb1235fa4a8bc22`, and the target diff adds only the V4 hardening reports plus `HARDENING-W5-V4-CONSOLIDATED.md`; no plan, research, source, generated output, `RESULTS.md`, or handoff surface moved.
2. Exact REDRESS anchors remain present wherever the active W5 packet asserts REDRESS 36-38/85/86 reconciliation. The plan anchors REDRESS 36-38, REDRESS 85, and REDRESS 86 at `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:77-79` and `:123-125`. The research note anchors the audit-scope assertion at `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md:26-29`, the residue-test assertion at `:97-99`, the findings assertion at `:113-116`, the V2 fold summary at `:185-186`, and the recommended challenge assertion at `:207-209`.
3. Current `RESULTS.md` anchors still resolve. The research note cites W0 manifest rows at `skinny/RESULTS.md:46-85` and report/Track 2 authority at `skinny/RESULTS.md:138-141` from `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md:135-137`; the current file has 141 lines and both ranges exist.
4. Cwd-qualified verification remains intact. The plan separates `skinny/` commands from repository-root commands at `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:92-109`, and the research V1 fold preserves the same cwd split at `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md:131-134`.
5. The remaining V4 process wording is stale only in chronology, not in close semantics. The unchanged plan/research still say `ready for V4 challenge` and `Proceed to a W5 V4 challenge` at `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:5` and `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md:197`, but the target HEAD adds the V4 consolidated result: 6/6 ACCEPT, minimum confidence 95%, first qualifying W5 acceptance cycle at `restart/skinny/tranches/sk-v8/research/wave-5-hardening/V4/HARDENING-W5-V4-CONSOLIDATED.md:20-21`, with exactly one unchanged qualifying re-challenge required before close at `:52-53`. That does not require a third cycle after V5 and does not dispatch W6.
6. Zero-drift and Lock 14 posture remain coherent. The zero-drift diff over `skinny/RESULTS.md`, generated JSON/typed output, direct guard source, IR, codegen, passes, parse-that-regex, SIMD, runtime, skinny bbnf, and xtask returned clean. The forbidden generic JSON policy scan and generic codegen branch scan returned no matches. Provider-residency output remains confined to `skinny/xtask/src/main.rs:124`, `:132`, `:183`, and `skinny/crates/codegen/src/json_provider.rs:57`, `:61`, matching the allowed tooling/provider posture.

## Verification/Evidence

- `git rev-parse HEAD` resolved to `42d5f034eee2a1931e46d13e7e20c62e49ca8c7a`.
- `git show --summary --format='%H%n%P%n%s' 42d5f034` shows parent `d3398a68a82ace5087b8b87b6cb1235fa4a8bc22` and only V4 hardening artifact create-mode entries.
- `git diff --name-status d3398a68 42d5f034` lists only `restart/skinny/tranches/sk-v8/research/wave-5-hardening/V4/CH1.md` through `CH6.md` and `HARDENING-W5-V4-CONSOLIDATED.md`.
- Anchor checks resolved `skinny/REDRESS.md:460-515`, `skinny/REDRESS.md:2399-2427`, `skinny/REDRESS.md:2431-2464`, `skinny/RESULTS.md:46-85`, and `skinny/RESULTS.md:138-141`.
- Repository-root `git diff --exit-code HEAD -- skinny/RESULTS.md skinny/crates/runtime/src/grammars/json skinny/crates/codegen/src/json_templates skinny/crates/bbnf-bench/src/generated_real_typed.rs skinny/crates/bbnf-bench/src/direct_struct.rs skinny/crates/ir/src skinny/crates/codegen/src skinny/crates/passes/src skinny/crates/parse-that-regex/src skinny/crates/bbnf-simd/src skinny/crates/runtime/src skinny/crates/bbnf/src skinny/xtask/src` returned clean.
- Repository-root forbidden generic Lock 14 symbol scan returned no matches; generic codegen JSON branch scan returned no matches; provider-residency scan returned only the allowed xtask generated-output tooling and `json_provider.rs` lines.
- I did not run cargo or xtask commands because the requested write constraint permits only `restart/skinny/tranches/sk-v8/research/wave-5-hardening/V5/CH1.md`; those commands can write build or generated artifacts. The V4 consolidated packet records the last live command evidence, including `cargo test -p bbnf-bench lock14_baseline -- --nocapture` passing 11/11 from `skinny/`.

## Required Folds

None for CH1. Do not dispatch W6 from this CH1 result alone. If the full V5 panel accepts the unchanged target, V5 can serve as the second qualifying W5 cycle required after V4 and may support W5 close processing without another challenge cycle.
