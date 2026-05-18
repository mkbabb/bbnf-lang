# SK-V8 W5 Hardening V1 Consolidated

Date: 2026-05-18.

Verdict: REVISE.

Panel:

| Reviewer | Verdict | Confidence |
|---|---|---:|
| CH1 | REVISE | 88% |
| CH2 | REVISE | 92% |
| CH3 | ACCEPT | 93% |
| CH4 | ACCEPT | 95% |
| CH5 | ACCEPT | 96% |
| CH6 | ACCEPT | 96% |

Result: 4/6 ACCEPT. This is not a qualifying convergence cycle.

## Required Folds

1. Split W5 verification commands by working directory:
   - run `bbnf-bench`, skinny `xtask check-*`, and skinny package tests from
     `skinny/`;
   - run root `cargo xtask regen --check`, repo-path `git diff`, and repo-path
     `rg` from the repository root.
2. Replace stale or broad anchors with exact current anchors. In particular,
   W5 must not cite inherited `skinny/RESULTS.md:217-218`; current W0 manifest
   rows resolve at `skinny/RESULTS.md:46-85`, and current Track 2/report
   authority resolves at `skinny/RESULTS.md:138-141`.
3. Extend the W5 audit to cover grammar-name branches and provider residency,
   not only old helper names.
4. Move or explicitly classify the JSON profile guard and JSON template/runtime
   includes currently in `skinny/crates/codegen/src/lib.rs`. The accepted fold is
   a small W5 Lock 14 cleanup: isolate that surface into
   `skinny/crates/codegen/src/json_provider.rs`, classify it as
   `per_grammar_provider` in `lock14_baseline`, and authorize only the W5 parent
   diff for `crates/codegen/src/lib.rs` plus `crates/codegen/src/json_provider.rs`.
5. Re-run zero-drift, Lock 14, W7/W8 residue, check-json, check-real-typed,
   check-conformance, and root regen checks before V2.

## Non-Folds

- Do not claim W5 proves the absence of every JSON-named string. W5 proves that
  forbidden generic JSON policy and REDRESS 36-38/85/86 route surfaces do not
  reopen.
- Do not mark W5 closed from V1.
- Do not update `skinny/RESULTS.md` or claim throughput movement.
