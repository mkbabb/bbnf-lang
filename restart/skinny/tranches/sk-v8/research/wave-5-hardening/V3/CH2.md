# SK-V8 W5 Hardening V3 CH2 - Generality

Target: `b71a8aed2e4bc4ada47a517e93d52cc842551059`.

Verdict: ACCEPT

Confidence %: 95

## Findings

1. The V2 fold does not reopen CH2. Target `b71a8aed` is a documentation-only
   fold over the earlier provider-boundary cleanup; it does not modify source,
   generated output, or `skinny/RESULTS.md`.
2. Provider-boundary classification is now explicit enough for W5. The JSON
   runtime-profile guard and JSON template/runtime includes reside in
   `skinny/crates/codegen/src/json_provider.rs`, which Lock 14 classifies as
   `per_grammar_provider`. `skinny/crates/codegen/src/lib.rs` remains a generic
   surface, but its live JSON contact is delegation to that provider boundary.
3. No generic JSON policy was found outside the allowed surfaces. The forbidden
   renamed-policy scan returned no matches in generic parse, pass, codegen, IR,
   SIMD, runtime, bbnf, or xtask roots after excluding generated JSON output
   and JSON templates.
4. Grammar-branch and provider-residency scans pass. The generic codegen scan
   excluding `json_provider.rs` and `json_templates/**` returned no matches for
   `grammar_name == "json"`, `backend.grammar_name`, JSON template includes, or
   `runtime/src/grammars/json` includes. The broader residency scan returned
   only `json_provider.rs` runtime includes and `xtask` generated-output
   plumbing.
5. REDRESS 36-38/85/86 closure is not reopened. The V2 fold now cites the
   append-only historical REDRESS 36-38 anchors and the later admitted REDRESS
   85/86 neutralization anchors. W5 is not claiming those old entries were
   edited in place; it is claiming no live generic route surface has reopened.
6. Non-JSON proof remains adequate for CH2 because the V2 packet records root
   `cargo xtask regen --check` clean for 9 of 9 grammars, including CSS L4,
   Google Sheets, and BBNF-self, and `b71a8aed` does not change code or
   generated files. This remains a provider-isolation proof, not a full
   multi-grammar provider registry.

## Verification/Evidence

- `git rev-parse HEAD` resolved to target
  `b71a8aed2e4bc4ada47a517e93d52cc842551059`.
- `git diff --name-status b71a8aed^ b71a8aed` shows only W5 research and V2
  hardening markdown files.
- `git diff --check b71a8aed^ b71a8aed` returned clean.
- `skinny/crates/codegen/src/json_provider.rs:4-13` owns the
  `backend.grammar_name == "json"` guard; `:48-73` owns JSON template/runtime
  includes.
- `skinny/crates/codegen/src/lib.rs:102-136` delegates runtime material to
  `json_provider`; it no longer owns the JSON guard or includes directly.
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs:188-193`, `:411-414`,
  `:477-485`, and `:562-575` classify `json_provider.rs`, admit the
  `per_grammar_provider` class, and constrain the W5 parent-diff allowance.
- Read-only forbidden-policy scan returned no matches for the W5 blocked JSON
  helper, renamed-policy, `UnionTape`, or `BackendShape::{Union,Json}` symbols
  in audited generic roots.
- Read-only grammar-branch scan returned no matches outside
  `json_provider.rs` and `json_templates/**`.
- Read-only residency scan returned only:
  `skinny/xtask/src/main.rs:124`, `:132`, `:183`, and
  `skinny/crates/codegen/src/json_provider.rs:57`, `:61`.
- `git diff --name-status b71a8aed^ b71a8aed -- skinny/RESULTS.md
  skinny/crates/runtime/src/grammars/json
  skinny/crates/codegen/src/json_templates
  skinny/crates/bbnf-bench/src/generated_real_typed.rs
  skinny/crates/bbnf-bench/src/direct_struct.rs
  skinny/crates/ir/src skinny/crates/codegen/src skinny/crates/passes/src
  skinny/crates/parse-that-regex/src skinny/crates/bbnf-simd/src
  skinny/crates/runtime/src skinny/crates/bbnf/src skinny/xtask/src`
  returned no paths.
- I did not run cargo tests or regen locally because this assignment restricts
  writes to this CH2 markdown file; cargo verification would create build
  artifacts. I used read-only scans/diffs and the target packet's recorded
  post-fold command evidence.

## Required Folds

None for CH2.

This ACCEPT is not W6 dispatch authority. W5 still needs the challenge process
required by `skv8-W5-plan.md`.
