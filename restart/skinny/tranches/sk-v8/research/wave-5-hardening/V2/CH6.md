# SK-V8 W5 Hardening V2 CH6 - Anti-Paper-Close

Date: 2026-05-18.

Target: `6e159f5c70aa5b4560d874a0e446587beb8f857e`
(`fix(sk-v8-wave5-lock14): isolate json provider boundary after V1 revise`).

Verdict: ACCEPT

Confidence %: 95%

## Findings

1. W5 is backed by live command evidence at the target HEAD, not by paper
   assertion. I reran the V2 command gate after confirming HEAD resolves to
   `6e159f5c70aa5b4560d874a0e446587beb8f857e`: Lock 14 passed 11/11, the
   skinny xtask checks passed, W7/W8-era package tests passed, root regeneration
   check was clean, and the focused scans matched the W5 expectations.
2. The V1 anti-paper-close weakness is folded rather than ignored. The target
   no longer closes on the original no-source audit: it names the provider
   drift found by V1, moves the JSON profile guard and JSON template/runtime
   includes into `skinny/crates/codegen/src/json_provider.rs`, and leaves
   `skinny/crates/codegen/src/lib.rs` with provider delegation only.
3. Post-commit Lock 14 coverage is live and stricter than V1. The baseline now
   classifies `crates/codegen/src/json_provider.rs` as `per_grammar_provider`,
   admits that class, and authorizes W5 parent diffs only for
   `crates/codegen/src/lib.rs` plus `crates/codegen/src/json_provider.rs`.
   The new `admits_w5_lock14_provider_parent_diff_only_under_w5_scope` test is
   included in the 11/11 passing Lock 14 run.
4. Zero-drift holds for the CH6 close surface. The target has no parent diff in
   `skinny/RESULTS.md`, generated JSON output, JSON templates, generated typed
   output, direct guard source, or `HANDOFF.md`; the live HEAD-path zero-drift
   command also returned clean for the W5-listed generated and generic owner
   paths before this CH6 file was written.
5. There is no generated, `RESULTS.md`, benchmark-result, or performance
   movement hidden in the fold. The changed source/test insertion count is 148
   across the three named Rust owner paths, and the only bench-crate touch is
   the Lock 14 audit gate source, not a report or row-table refresh.
6. No technical cleanup is deferred to W6. The only remaining condition is the
   planned challenge discipline: W5 may close only after two consecutive
   qualifying ACCEPT cycles. That is a process gate, not a hidden W6 work item.
7. V2 alone does not dispatch W6. The plan explicitly requires a re-challenge
   after any qualifying V2 ACCEPT, `HANDOFF.md` is unchanged by the target, and
   this CH6 review does not mark W5 closed.

## Verification/Evidence

- Repo root `git rev-parse HEAD` returned
  `6e159f5c70aa5b4560d874a0e446587beb8f857e`.
- From `skinny/`, `CARGO_TARGET_DIR=/tmp/bbnf-lang-ch6-target cargo test -p bbnf-bench lock14_baseline -- --nocapture`
  passed: 11 tests passed, 0 failed.
- From `skinny/`, `CARGO_TARGET_DIR=/tmp/bbnf-lang-ch6-target cargo xtask check-json`,
  `cargo xtask check-real-typed`, and `cargo xtask check-conformance` passed;
  conformance accepted 21 valid fixtures and rejected 7 invalid fixtures.
- From `skinny/`, `CARGO_TARGET_DIR=/tmp/bbnf-lang-ch6-target cargo test -p parse-that-regex -p passes -p codegen -p ir`
  passed: codegen 6/6, ir 3/3, parse-that-regex 22/22, passes 8/8, and
  doc-tests green.
- From the repository root, `CARGO_TARGET_DIR=/tmp/bbnf-lang-ch6-target-root cargo xtask regen --check`
  passed with `regen --check: clean (9 of 9 grammars matched)`.
- Repo-root zero-drift command
  `git diff --exit-code HEAD -- skinny/RESULTS.md skinny/crates/runtime/src/grammars/json skinny/crates/codegen/src/json_templates skinny/crates/bbnf-bench/src/generated_real_typed.rs skinny/crates/bbnf-bench/src/direct_struct.rs skinny/crates/ir/src skinny/crates/codegen/src skinny/crates/passes/src skinny/crates/parse-that-regex/src skinny/crates/bbnf-simd/src skinny/crates/runtime/src skinny/crates/bbnf/src skinny/xtask/src`
  returned clean before this CH6 file was written.
- Parent-diff checks for `skinny/RESULTS.md`, generated JSON output,
  `json_templates`, generated typed output, `direct_struct.rs`, and
  `restart/skinny/tranches/sk-v8/HANDOFF.md` returned no output.
- The forbidden renamed-policy scan returned no matches for the W5 list:
  `StrictJson`, `StrictJsonTrustedUtf8`, `JsonStringMatch`, `JsonNumberMatch`,
  `skip_json`, `match_json`, `unescape_json`, `shapes_for_json`,
  `nominate_json`, `materialization_for_rule`, `descriptor_for_rule`,
  `rule_by_name("json")`, `MissingEntry("json")`, `StructuralAlphabet::json`,
  `UnionTape`, `union_tape`, `BackendShape::Union`, and `BackendShape::Json`.
- The generic codegen scan excluding `json_provider.rs` and
  `json_templates/**` returned no matches for `grammar_name == "json"`,
  `backend.grammar_name`, JSON template includes, or runtime JSON includes.
- The provider-residency scan returned only `skinny/crates/codegen/src/json_provider.rs`
  and existing `skinny/xtask/src/main.rs` generated-output tooling references.
- `git diff --numstat 6e159f5c^ 6e159f5c -- skinny/crates/codegen/src/lib.rs skinny/crates/codegen/src/json_provider.rs skinny/crates/bbnf-bench/src/lock14_baseline.rs`
  returned 148 source/test insertions and 106 deletions across the named Rust
  owner paths.
- `skinny/crates/codegen/src/lib.rs:102-146` delegates JSON provider material;
  `skinny/crates/codegen/src/json_provider.rs:4-73` owns the JSON guard and
  JSON template/runtime includes.
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs:188-193`,
  `:411-414`, `:477-485`, and `:562-575` establish the
  `per_grammar_provider` class and W5-only parent-diff allowance.
- `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:128-131` requires a
  re-challenge after a qualifying V2 ACCEPT and says W5 may close only after
  two consecutive qualifying ACCEPT cycles.

## Required Folds

None for CH6 acceptance.

Carry-forward constraints: do not mark W5 closed or dispatch W6 from V2 alone;
if V2 as a whole qualifies, re-challenge the unchanged packet once more.
