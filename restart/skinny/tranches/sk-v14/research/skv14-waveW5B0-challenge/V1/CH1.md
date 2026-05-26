# SK-V14 W5B.0 CH1: Correctness

Date: 2026-05-26.
Scope: W5B.0 Lock 14 gate correctness review.
Disposition: REVISE.

## Findings

The measurable W5B.0 test set is correct. `SPEC.md` requires the eight exact
`w5b_lock14_frontend_*` tests, and V8 requires each test to write a dedicated
`/tmp/skv14-w5b-<test-name>.log` plus a dedicated nonzero `rg` proof.

The source gate is still absent before redress. `lock14_baseline.rs` stops at
`SK_V14_W5A_OWNER_PATHS`, aggregates only through W5A, and parent-diff routing
falls through after the W5A subject case.

## Required Folds

- Split W5B.0 redress owner paths from the aggregate W5B-FRONTEND Lock 14
  roster: W5B.0 may edit only `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
  plus proof logs, while `SK_V14_W5B_FRONTEND_OWNER_PATHS` must admit the exact
  future W5B owner paths named by `SPEC.md`.
- Treat the `SPEC.md` entry-gate wording as W5B.0 exit and W5B.1+ precondition,
  not as a self-cycle blocking W5B.0.
- Route W5B.0 through W5B.4 subject forms, or require every such commit subject
  to contain the aggregate `sk-v14-waveW5B-FRONTEND` token.
