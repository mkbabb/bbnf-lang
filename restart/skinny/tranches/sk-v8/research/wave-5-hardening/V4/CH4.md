# SK-V8 W5 Hardening V4 CH4 - Cost, LOC, Timing, Same-Wave Consumer

Target: `d3398a68a82ace5087b8b87b6cb1235fa4a8bc22`
(`docs(sk-v8-wave5-plan): route V4 after V3 revise`).

Verdict: ACCEPT

Confidence %: 95%

## Findings

1. No CH4 blocker remains. V3 failed on CH1 REDRESS anchoring, not on cost. The
   current target only routes the folded packet to V4 and preserves the already
   accepted cost posture.
2. The inherited W5 source/test cleanup remains within the `<=150` cap: 148
   insertions total across `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
   (+40), `skinny/crates/codegen/src/json_provider.rs` (+96), and
   `skinny/crates/codegen/src/lib.rs` (+12).
3. The target commit itself is documentation-only: 18 insertions and 9
   deletions across only `skv8-W5-lock14-audit-research.md` and
   `skv8-W5-plan.md`. No source/test file changed after the named cleanup
   commit.
4. Owner paths remain bounded. The live plan names the W5 research files, W5
   hardening artifacts, the two codegen paths, `lock14_baseline.rs`, and
   `HANDOFF.md` only after accepted close; generated output and
   `skinny/RESULTS.md` are explicitly out of scope.
5. No benchmark/report refresh is smuggled into the packet. The full W5 cleanup
   range has no diff in `skinny/RESULTS.md`, generated JSON output, JSON
   templates, generated typed output, direct guard source, or `HANDOFF.md`.
6. Same-wave consumer coverage is sufficient for CH4. The W5 plan and SPEC bind
   the cleanup to the audit gate plus existing codegen/runtime checks. The Lock
   14 baseline classifies `json_provider.rs` as `per_grammar_provider`,
   authorizes W5 parent diffs only for `codegen/src/lib.rs` plus
   `json_provider.rs`, and the current codegen path delegates JSON runtime
   material through that provider.
7. The 90-minute cap remains credible. V4 is a documentation challenge over an
   unchanged 148-line provider-boundary cleanup; it does not request benchmark
   reruns, row-table refresh, generated-output churn, or W6 dispatch.

## Verification/Evidence

- `git diff --numstat 6e159f5c^ 6e159f5c -- skinny/crates/codegen/src/lib.rs skinny/crates/codegen/src/json_provider.rs skinny/crates/bbnf-bench/src/lock14_baseline.rs`
  returned `40 + 96 + 12 = 148` Rust source/test insertions.
- `git diff --name-only d3398a68^ d3398a68` returned only
  `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md`
  and `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md`; `git diff
  --numstat d3398a68^ d3398a68` returned `10/3` and `8/6` for those docs.
- `git diff --name-only 6e159f5c d3398a68 -- skinny/crates/codegen/src/lib.rs skinny/crates/codegen/src/json_provider.rs skinny/crates/bbnf-bench/src/lock14_baseline.rs skinny/RESULTS.md restart/skinny/tranches/sk-v8/HANDOFF.md skinny/crates/runtime/src/grammars/json skinny/crates/codegen/src/json_templates skinny/crates/bbnf-bench/src/generated_real_typed.rs skinny/crates/bbnf-bench/src/direct_struct.rs`
  returned no paths, so source and report/generated surfaces did not move after
  the source cleanup.
- `git diff --name-only 6e159f5c^ d3398a68 -- skinny/RESULTS.md skinny/crates/runtime/src/grammars/json skinny/crates/codegen/src/json_templates skinny/crates/bbnf-bench/src/generated_real_typed.rs skinny/crates/bbnf-bench/src/direct_struct.rs restart/skinny/tranches/sk-v8/HANDOFF.md`
  returned no paths for the full cleanup range.
- `git diff --exit-code HEAD -- skinny/RESULTS.md skinny/crates/runtime/src/grammars/json skinny/crates/codegen/src/json_templates skinny/crates/bbnf-bench/src/generated_real_typed.rs skinny/crates/bbnf-bench/src/direct_struct.rs skinny/crates/ir/src skinny/crates/codegen/src skinny/crates/passes/src skinny/crates/parse-that-regex/src skinny/crates/bbnf-simd/src skinny/crates/runtime/src skinny/crates/bbnf/src skinny/xtask/src`
  returned clean before this review file was added.
- Read-only forbidden-policy and generic codegen scans returned no matches. The
  provider-residency scan returned only `skinny/xtask/src/main.rs` generated
  output tooling and `skinny/crates/codegen/src/json_provider.rs` runtime
  includes.
- `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:18-36` names owner
  paths and the `<=150` cap; `:126-127` names CH4's audit-gate plus
  codegen/runtime consumer; `:135-139` sends V4 to challenge and requires one
  more qualifying unchanged-packet cycle before W5 close.
- `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md:177-186`
  records the unchanged 148-line source packet, `lib.rs` delegation only, audit
  plus codegen/runtime same-wave evidence, and current REDRESS anchors.
- `restart/skinny/tranches/sk-v8/SPEC.md:223-257` gives W5 the default `0`
  source LOC budget, the `<=150` named Lock 14 cleanup allowance, the 90-minute
  cap, and no performance rerun unless source moved. `SPEC.md:677-692` binds
  W5 cleanup to named Lock 14 drift and the audit gate or existing
  codegen/runtime tests as same-wave consumer.
- I did not run cargo or regeneration commands in this CH4 pass because this
  assignment restricts writes to this markdown file and those commands may
  create build artifacts. The target is doc-only after the packet's recorded
  post-fold command evidence, so this review used read-only diffs and scans.

## Required Folds

None for CH4.

This ACCEPT does not dispatch W6 and does not close W5 by itself. The live plan
still requires a further qualifying unchanged-packet re-challenge after V4
before W5 may close.
