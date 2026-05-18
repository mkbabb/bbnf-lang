# SK-V8 W5 Hardening V2 CH4 - Cost, LOC, Timing, Same-Wave Consumer

Target: `6e159f5c70aa5b4560d874a0e446587beb8f857e`
(`fix(sk-v8-wave5-lock14): isolate json provider boundary after V1 revise`).

Verdict: ACCEPT

Confidence %: 94%

## Findings

1. The W5 cleanup stays inside the `<=150` source/test insertion cap. The only
   Rust source/test paths in the target add 148 lines total:
   `lock14_baseline.rs` +40, `json_provider.rs` +96, and `codegen/src/lib.rs`
   +12. The margin is narrow, but compliant.
2. The source/test owner surface is limited to the named V2 owner paths:
   `skinny/crates/codegen/src/lib.rs`,
   `skinny/crates/codegen/src/json_provider.rs`, and
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs`. The remaining changed
   paths are W5 research/plan docs and V1 hardening artifacts under the named
   W5 hardening tree.
3. The same-wave consumer is real enough for CH4. `lock14_baseline` now
   consumes the provider split by adding `per_grammar_provider`, validating the
   new provider path in the current allowlist, and admitting W5 parent diffs
   only for `crates/codegen/src/lib.rs` plus
   `crates/codegen/src/json_provider.rs`.
4. Timing remains within the W5 cost model. The fold is a provider-boundary
   relocation plus a Lock 14 authorization/test update, not a new generator,
   benchmark, or report cycle.
5. The target does not smuggle benchmark/report refresh work. It does not touch
   `skinny/RESULTS.md`, generated JSON output, generated typed output, direct
   benchmark structs, report files, or `HANDOFF.md`; the only bench-crate touch
   is the Lock 14 audit gate source.
6. CH4 does not authorize W6 dispatch. This ACCEPT can count as a V2 challenge
   result only; W5 still needs the planned next qualifying challenge cycle
   before close.

## Verification/Evidence

- `git diff --numstat 6e159f5c^ 6e159f5c -- skinny/crates/codegen/src/lib.rs skinny/crates/codegen/src/json_provider.rs skinny/crates/bbnf-bench/src/lock14_baseline.rs`
  returned 148 total source/test insertions and 106 deletions.
- `git diff --name-only 6e159f5c^ 6e159f5c` listed only W5 research/plan docs,
  V1 hardening artifacts, and the three named Rust owner paths.
- `git diff --name-only 6e159f5c^ 6e159f5c -- skinny/RESULTS.md skinny/crates/runtime/src/grammars/json skinny/crates/codegen/src/json_templates skinny/crates/bbnf-bench/src/generated_real_typed.rs skinny/crates/bbnf-bench/src/direct_struct.rs restart/skinny/tranches/sk-v8/HANDOFF.md`
  returned no paths.
- `rg -n 'grammar_name == "json"|backend\.grammar_name|include_str!\("json_templates|runtime/src/grammars/json' skinny/crates/codegen/src -g '*.rs' --glob '!json_provider.rs' --glob '!json_templates/**'`
  returned no matches.
- `rg -n 'emit_from_source\("json"\)|runtime/src/grammars/json' skinny/crates/codegen/src skinny/xtask/src -g '*.rs'`
  returned only xtask generated-output plumbing and `json_provider.rs`.
- From `skinny/`, `cargo test -p bbnf-bench lock14_baseline -- --nocapture`
  passed 11/11 Lock 14 tests, including
  `admits_w5_lock14_provider_parent_diff_only_under_w5_scope`.
- From `skinny/`, `cargo test -p parse-that-regex -p passes -p codegen -p ir`
  passed codegen 6/6, ir 3/3, parse-that-regex 22/22, passes 8/8, and
  doc-tests.
- From `skinny/`, `cargo xtask check-json`, `cargo xtask check-real-typed`,
  and `cargo xtask check-conformance` passed; conformance accepted 21 valid
  fixtures and rejected 7 invalid fixtures.
- From the repository root, `cargo xtask regen --check` passed with
  `clean (9 of 9 grammars matched)`.
- `git diff --exit-code HEAD -- skinny/RESULTS.md skinny/crates/runtime/src/grammars/json skinny/crates/codegen/src/json_templates skinny/crates/bbnf-bench/src/generated_real_typed.rs skinny/crates/bbnf-bench/src/direct_struct.rs skinny/crates/ir/src skinny/crates/codegen/src skinny/crates/passes/src skinny/crates/parse-that-regex/src skinny/crates/bbnf-simd/src skinny/crates/runtime/src skinny/crates/bbnf/src skinny/xtask/src`
  returned clean after verification.

## Required Folds

None.
