# SK-V8 W5 Hardening V2 CH1 - Correctness

Target: `6e159f5c70aa5b4560d874a0e446587beb8f857e` (`fix(sk-v8-wave5-lock14): isolate json provider boundary after V1 revise`).

Verdict: REVISE

Confidence: 91%

## Findings

1. The V1 CH1 cwd fold is satisfied in the plan, but stale no-source wording remains after the W5 source cleanup. The plan correctly splits commands between `skinny/` and repo root at `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:90-108`, yet the same file still says the challenge is against a "no-source audit close" at `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:116` and still describes CH4 as "0 source LOC" at `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:122-123`. That conflicts with the accepted named cleanup in `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:30-36` and the actual source/test diff of 148 insertions / 106 deletions across `skinny/crates/codegen/src/lib.rs`, `skinny/crates/codegen/src/json_provider.rs`, and `skinny/crates/bbnf-bench/src/lock14_baseline.rs`.

2. The research file still claims W5 has no generic edit. `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md:29-31` says CSS L4 / Sheets / BBNF-self proof is unchanged-output coverage because "W5 has no generic edit"; however `skinny/crates/bbnf-bench/src/lock14_baseline.rs:249-252` classifies `crates/codegen/src/lib.rs` as `generic_surface`, and the target commit edits that file. The non-JSON proof can remain unchanged-output/root-regen evidence, but the premise must be rewritten as "generic surface touched only to delegate provider material."

3. V1 CH1 required exact REDRESS anchors, but the folded W5 research/plan still cite only item numbers. `restart/skinny/tranches/sk-v8/research/wave-5-hardening/V1/CH1.md:31-35` and `:61-65` required current spans for REDRESS 36-38 and 85-86. The resolving anchors are `skinny/REDRESS.md:460-515`, `skinny/REDRESS.md:2399-2427`, and `skinny/REDRESS.md:2431-2464`; the folded files reference REDRESS 36-38/85/86 without those line anchors at `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md:25-26`, `:107-109`, and `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:77-78`.

4. The current `skinny/RESULTS.md` anchors are now correct. The folded research cites W0 manifest rows at `skinny/RESULTS.md:46-85` and Track 2/report authority at `skinny/RESULTS.md:138-141` in `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md:128-130`; those spans resolve in the current file.

## Verification/Evidence

- `git rev-parse HEAD` resolves to target `6e159f5c70aa5b4560d874a0e446587beb8f857e`.
- Repo-root `git diff --exit-code HEAD -- skinny/RESULTS.md skinny/crates/runtime/src/grammars/json skinny/crates/codegen/src/json_templates skinny/crates/bbnf-bench/src/generated_real_typed.rs skinny/crates/bbnf-bench/src/direct_struct.rs skinny/crates/ir/src skinny/crates/codegen/src skinny/crates/passes/src skinny/crates/parse-that-regex/src skinny/crates/bbnf-simd/src skinny/crates/runtime/src skinny/crates/bbnf/src skinny/xtask/src`: exit 0, no output.
- Repo-root forbidden JSON policy `rg` with the folded quoted globs: exit 1, no matches.
- Repo-root grammar-branch scan excluding `json_provider.rs` and `json_templates/**`: exit 1, no matches.
- Repo-root provider-residency scan returns only generated-output tooling and the provider: `skinny/xtask/src/main.rs:124`, `:132`, `:183`, plus `skinny/crates/codegen/src/json_provider.rs:57` and `:61`.
- Parent source diff under the W5 owner paths is exactly `skinny/crates/bbnf-bench/src/lock14_baseline.rs`, `skinny/crates/codegen/src/json_provider.rs`, and `skinny/crates/codegen/src/lib.rs`; shortstat is 148 insertions / 106 deletions.
- Provider-boundary posture is correct in code: `skinny/crates/codegen/src/lib.rs:108` and `:118-135` delegate JSON emission to `json_provider`; `skinny/crates/codegen/src/json_provider.rs:4-12` owns the JSON grammar-profile guard and `:48-61` owns JSON template/runtime includes; `skinny/crates/bbnf-bench/src/lock14_baseline.rs:189-192`, `:411-414`, `:477-485`, `:562-575`, and `:668-678` classify and test the W5-only provider parent diff.

## Required Folds

1. Replace the remaining no-source/no-generic-edit language with the accepted W5 named Lock 14 cleanup posture: <=150 source/test insertions, generic `codegen/src/lib.rs` touched only to delegate provider material, and same-wave consumer evidence from the audit gate plus existing codegen/runtime checks.
2. Add exact current REDRESS anchors wherever W5 asserts REDRESS 36-38/85/86 reconciliation: `skinny/REDRESS.md:460-515`, `skinny/REDRESS.md:2399-2427`, and `skinny/REDRESS.md:2431-2464`.
3. Preserve the now-correct cwd-qualified verification block and current `skinny/RESULTS.md:46-85` / `skinny/RESULTS.md:138-141` anchors; do not update `skinny/RESULTS.md`, generated outputs, or W6 status from this V2 cycle.
