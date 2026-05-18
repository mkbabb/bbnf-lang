# SK-V8 W5 Hardening V3 CH1 - Correctness

Target: `b71a8aed2e4bc4ada47a517e93d52cc842551059` (`docs(sk-v8-wave5-hardening): fold V2 redress anchors and cleanup posture`).

Verdict: REVISE

Confidence: 88%

## Findings

1. Exact REDRESS anchoring is still incomplete. `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md:26-27` still asserts that REDRESS 36, 37, and 38 residue clusters remain neutralized by REDRESS 85 and 86 without the required current anchors. Later anchors at `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md:109-112` and `:182-183` are correct, but they do not satisfy V2 CH1's "wherever W5 asserts" requirement for the audit-scope assertion.
2. The stale current-posture language is otherwise folded. The live plan now challenges a named Lock 14 provider-boundary cleanup at `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:118` and CH4 now uses `<=150 source/test insertions` plus audit-gate/codegen/runtime evidence at `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:126-127`. The remaining `no-source/no-generic-edit` mentions at `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:133-134` and `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md:170-172` are historical V2-finding summaries, not active close criteria.
3. The current `RESULTS.md` and cwd-qualified verification folds are satisfied. The research cites `skinny/RESULTS.md:46-85` and `skinny/RESULTS.md:138-141` at `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md:132-134`, and the plan separates `skinny/` commands from repository-root commands at `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:92-109`.
4. Post-commit posture is not a W6 dispatch. `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:135-137` sends the unchanged packet to V3 and requires a second qualifying ACCEPT cycle before close.

## Verification/Evidence

- `git rev-parse HEAD` resolved to `b71a8aed2e4bc4ada47a517e93d52cc842551059`.
- `git show --name-only --format='%H%n%s' b71a8aed` shows a doc-only fold: W5 research/plan files plus V2 hardening artifacts; no source, generated output, `skinny/RESULTS.md`, or W6 handoff file changed.
- Current anchor targets resolve: `skinny/REDRESS.md:460-515`, `skinny/REDRESS.md:2399-2427`, `skinny/REDRESS.md:2431-2464`, `skinny/RESULTS.md:46-85`, and `skinny/RESULTS.md:138-141`.
- Repo-root zero-drift check against the target over `skinny/RESULTS.md`, generated JSON/typed output, direct guard source, generic crates, runtime, bbnf, and xtask returned clean.
- Generic codegen branch scan excluding `json_provider.rs` and `json_templates/**` returned no matches. Provider-residency scan returned only `skinny/xtask/src/main.rs:124`, `:132`, `:183`, and `skinny/crates/codegen/src/json_provider.rs:57`, `:61`.

## Required Folds

1. Add the exact REDRESS anchors directly to the audit-scope REDRESS assertion at `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md:26-27`: `skinny/REDRESS.md:460-515`, `skinny/REDRESS.md:2399-2427`, and `skinny/REDRESS.md:2431-2464`.
2. Preserve the already-correct named Lock 14 provider-boundary posture, cwd-qualified verification block, current `RESULTS.md` anchors, zero-drift checks, and V3-before-close posture. Do not dispatch W6.
