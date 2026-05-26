# SK-V14 Wave W5B.0 Plan: Lock14 Frontend Gate

Inputs:

- `restart/skinny/tranches/sk-v14/SPEC.md` §8B requires W5B.0 to run before
  W5B.1..W5B.4 and to add the W5B-FRONTEND owner roster, parent-diff routing,
  and eight exact `w5b_lock14_frontend_*` tests.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B0-A-lock14-owner-routing.md`
  finds `lock14_baseline.rs` currently stops at W5A roster and routing.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B0-B-provider-template-topology.md`
  finds the provider/template topology guard still permits modified protected
  paths and counts only seven CSS template directories.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B0-C-authority-and-evidence.md`
  binds W5B.0 as the next step after V8 CRUD and requires dedicated per-test
  logs plus dedicated nonzero `rg` proof.
- `restart/skinny/tranches/sk-v14/research/skv14-waveW5B0-challenge/V1/CONSOLIDATED.md`
  records the seven-lens fold set.

Intervention: add the W5B-FRONTEND Lock 14 owner-path roster, subject routing,
provider/template mutation guard, all-template census, and exact tests in
`lock14_baseline.rs` before any W5B frontend source redress.

Owner paths:

- Redress may edit `skinny/crates/bbnf-bench/src/lock14_baseline.rs`.
- Redress may write dedicated proof logs under `/tmp/skv14-w5b-<test-name>.log`.
- Redress must not edit `skinny/crates/grammar/src/lib.rs`,
  `skinny/crates/codegen/src/lib.rs`, `skinny/crates/codegen/src/grammar_provider.rs`,
  `skinny/xtask/src/main.rs`, `skinny/xtask/src/regen.rs`, or
  `skinny/xtask/src/regen_css.rs` in W5B.0.
- The new `SK_V14_W5B_FRONTEND_OWNER_PATHS` roster admits the aggregate W5B
  source owner paths named by `SPEC.md` for W5B.1..W5B.4 after W5B.0 admits.

Falsifiability gate:

- `cargo test -p bbnf-bench w5b_lock14_frontend_owner_paths_admit -- --exact`
- `cargo test -p bbnf-bench w5b_lock14_frontend_rejects_w5c_subject -- --exact`
- `cargo test -p bbnf-bench w5b_lock14_frontend_rejects_w5d_subject -- --exact`
- `cargo test -p bbnf-bench w5b_lock14_frontend_rejects_modified_provider -- --exact`
- `cargo test -p bbnf-bench w5b_lock14_frontend_rejects_modified_template -- --exact`
- `cargo test -p bbnf-bench w5b_lock14_frontend_all_templates_guard_counts_8 -- --exact`
- `cargo test -p bbnf-bench w5b_lock14_frontend_allows_grammar_provider_exception -- --exact`
- `cargo test -p bbnf-bench w5b_lock14_frontend_generic_owner_leak_census -- --exact`

Each command tees to its matching `/tmp/skv14-w5b-<test-name>.log`, and each log
is paired with a dedicated
`rg "test result: ok\\. [1-9][0-9]* passed" /tmp/skv14-w5b-<test-name>.log`.
Wildcard aggregate greps do not satisfy the gate.

Hard cap: 30 minutes for redress. Commit-safe evidence at 27 minutes; halt at 30
minutes if the exact gate is not green.

Revert protocol: if the gate fails, revert the `lock14_baseline.rs` patch, save
the failed patch to `/tmp/skv14-waveW5B0-rejected.patch`, and add a reject entry
naming the failed owner route, topology guard, exact test, or proof command.

Same-wave consumer: `validate_git_freeze()` consumes the expanded owner roster
and parent-diff router in the Lock 14 gate; the eight exact unit tests consume
the new W5B-FRONTEND routing and provider/template guard in the same redress
commit.

Pre-blocked routes:

- No provider/template deletion.
- No provider-free generator-body replacement.
- No grammar/codegen/xtask frontend implementation edit in W5B.0.
- No public `@ws` revival.
- No grammar-name branch leakage in generic crates.
- No W5C-GEN or W5D-DELETE unblock from W5B.0 alone.
