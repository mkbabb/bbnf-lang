# SK-V8 W5 Hardening V3 CH4 - Cost, LOC, Timing, Same-Wave Consumer

Target: `b71a8aed2e4bc4ada47a517e93d52cc842551059`
(`docs(sk-v8-wave5-hardening): fold V2 redress anchors and cleanup posture`).

Verdict: ACCEPT

Confidence %: 95%

## Findings

1. The V2 fold fixes the stale cost posture. The live packet now states the
   named W5 cleanup as `<=150 source/test insertions`, not `0 source LOC`, in
   the plan CH4 line and in the V2 research fold.
2. The inherited W5 source/test slice is 148 insertions across only the named
   Rust owner paths: `skinny/crates/codegen/src/lib.rs` +12,
   `skinny/crates/codegen/src/json_provider.rs` +96, and
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs` +40.
3. The target commit itself is doc-only and touches only W5 research/plan files
   and V2 hardening artifacts under the named W5 hardening tree. It does not
   add source, generated output, `skinny/RESULTS.md`, or `HANDOFF.md` changes.
4. Same-wave consumer evidence remains inside W5: the audit gate plus existing
   codegen/runtime checks. `lock14_baseline` admits the W5 parent diff only for
   `codegen/src/lib.rs` plus `json_provider.rs`, and the provider split is
   consumed by current codegen/runtime emission paths.
5. Timing is credible for the 90-minute W5 cap. The target fold is documentation
   cleanup over an already bounded provider-boundary relocation; it does not
   request benchmark reruns, row-table refresh, generated-output churn, or W6
   dispatch.

## Verification/Evidence

- `git diff --numstat 6e159f5c^ 6e159f5c -- skinny/crates/codegen/src/lib.rs skinny/crates/codegen/src/json_provider.rs skinny/crates/bbnf-bench/src/lock14_baseline.rs`
  returned `40 + 96 + 12 = 148` Rust insertions.
- `git diff --name-only b71a8aed^ b71a8aed` listed only
  `skv8-W5-lock14-audit-research.md`, `skv8-W5-plan.md`, and V2 hardening
  artifacts.
- `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:30-36` names the
  provider-boundary cleanup and `<=150` source/test insertion cap; `:126-127`
  uses the same cap for CH4 instead of `0 source LOC`.
- `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md:174-183`
  records the V2 fold as 148 insertions, generic `lib.rs` delegation only, and
  audit plus existing codegen/runtime same-wave evidence.
- `restart/skinny/tranches/sk-v8/SPEC.md:677-692` permits only named Lock 14
  drift within the 90-minute and 150 source LOC cleanup cap, with the audit gate
  or existing codegen/runtime tests as the same-wave consumer.
- Current scans keep provider residency bounded: the generic codegen
  `grammar_name == "json"` / runtime-template scan returned no matches outside
  `json_provider.rs` and templates; the residency scan returned only xtask
  generated-output tooling and `json_provider.rs`.

## Required Folds

None for CH4. This ACCEPT does not dispatch W6.
