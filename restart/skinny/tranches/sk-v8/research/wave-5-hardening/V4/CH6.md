# SK-V8 W5 Hardening V4 CH6 - Anti-Paper-Close

Target: `d3398a68a82ace5087b8b87b6cb1235fa4a8bc22`
(`docs(sk-v8-wave5-plan): route V4 after V3 revise`).

Verdict: ACCEPT

Confidence %: 95%

## Findings

1. V1-V3 folds actually landed in the packet under review. V1's named Lock 14
   provider-boundary cleanup is present in source: `codegen/src/lib.rs`
   delegates JSON provider material to `json_provider`, `json_provider.rs` owns
   the JSON template/runtime includes, and `lock14_baseline.rs` carries the
   `per_grammar_provider` class. V2's cleanup posture is present in the plan and
   research, including the <=150 LOC cap, generic-surface delegation framing,
   current `skinny/RESULTS.md` anchors, and exact REDRESS 36-38/85/86 anchors.
   V3's fold is also present: active REDRESS reconciliation assertions now carry
   inline spans for `skinny/REDRESS.md:460-515`,
   `skinny/REDRESS.md:2399-2427`, and `skinny/REDRESS.md:2431-2464`.
2. Live command evidence remains cited. The research still records the clean
   repo state, zero-drift diff, passed Lock 14 baseline, passed `check-json`,
   `check-real-typed`, `check-conformance`, root regen, and package-test
   evidence. The plan still carries cwd-qualified commands split between
   `skinny/` and the repository root.
3. No source changed after the V1 provider-boundary source fold. From
   `6e159f5c` through the target, diffs are limited to W5 plan/research docs and
   V2/V3 hardening artifacts; `skinny/crates`, `skinny/RESULTS.md`, and
   `HANDOFF.md` are unchanged.
4. No generated output, `skinny/RESULTS.md`, benchmark-result, row-table, or
   performance movement is present in V4. The target commit modifies only the W5
   plan and W5 research, and the live zero-drift diff over generated/result and
   relevant owner paths returned clean before this CH6 file was written.
5. No technical cleanup is deferred. The packet preserves the hardening
   challenge gate and conditional revert/redress path for newly discovered live
   drift; it does not park a known blocker.
6. V4 does not dispatch W6 alone. The plan says a qualifying V4 ACCEPT requires
   one more unchanged-packet re-challenge, and W5 may close only after two
   consecutive qualifying ACCEPT cycles.

## Verification/Evidence

- `git rev-parse HEAD` returned
  `d3398a68a82ace5087b8b87b6cb1235fa4a8bc22`.
- `git show --stat --oneline --no-renames d3398a68` shows only two modified
  docs:
  `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md`
  and `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md`.
- `git diff --name-status d3398a68^ d3398a68` likewise shows only those two W5
  research docs modified.
- `git diff --name-status 6e159f5c d3398a68` shows only W5 plan/research docs
  plus V2/V3 review artifacts after the V1 source fold.
- `git diff --name-only 6e159f5c d3398a68 -- skinny/crates skinny/RESULTS.md restart/skinny/tranches/sk-v8/HANDOFF.md`
  returned no output.
- `git diff --exit-code HEAD -- skinny/RESULTS.md skinny/crates/runtime/src/grammars/json skinny/crates/codegen/src/json_templates skinny/crates/bbnf-bench/src/generated_real_typed.rs skinny/crates/bbnf-bench/src/direct_struct.rs skinny/crates/ir/src skinny/crates/codegen/src skinny/crates/passes/src skinny/crates/parse-that-regex/src skinny/crates/bbnf-simd/src skinny/crates/runtime/src skinny/crates/bbnf/src skinny/xtask/src`
  returned clean before this CH6 file was written.
- The forbidden renamed-policy scan returned no matches; the generic codegen
  grammar-name/provider scan excluding `json_provider.rs` and
  `json_templates/**` returned no matches.
- The provider-residency scan returned only generated-output tooling in
  `skinny/xtask/src/main.rs` plus allowed provider lines in
  `skinny/crates/codegen/src/json_provider.rs`.
- Source residency checks at HEAD show `skinny/crates/codegen/src/lib.rs`
  declares `mod json_provider;` and delegates JSON provider material through
  `json_provider::...`; `skinny/crates/codegen/src/json_provider.rs` owns the
  `include_str!` template/runtime material; and
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs` includes
  `per_grammar_provider`.
- Current plan anchors resolve at
  `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:77-85`,
  `:92-114`, and `:132-139`.
- Current research anchors resolve at
  `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md:26-35`,
  `:37-102`, `:124-193`, and `:197-210`.

## Required Folds

None for CH6 acceptance. Do not dispatch W6 from V4 alone; proceed only to the
required unchanged-packet re-challenge if the full V4 panel qualifies.
