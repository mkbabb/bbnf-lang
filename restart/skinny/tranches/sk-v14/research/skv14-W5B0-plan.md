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

The provider/template guard must reject modified, added, deleted, renamed, and
untracked protected paths for every `*_provider.rs` file except exactly
`crates/codegen/src/grammar_provider.rs`, and for every path containing
`_templates`. The all-template census must count exactly eight `*_templates`
directories: the seven CSS L4 template dirs plus `json_templates`.

The generic-owner leak census must prove that the W5B roster admits no
grammar-specific provider, template, or generated-runtime path. The sole
provider-shaped exception is `crates/codegen/src/grammar_provider.rs`, which
remains the neutral frontend request-boundary module rather than old provider
residue.

Owner paths:

- Redress may edit `skinny/crates/bbnf-bench/src/lock14_baseline.rs`.
- Redress may write dedicated proof logs under `/tmp/skv14-w5b-<test-name>.log`.
- Redress must not edit `skinny/crates/grammar/src/lib.rs`,
  `skinny/crates/codegen/src/lib.rs`, `skinny/crates/codegen/src/grammar_provider.rs`,
  `skinny/xtask/src/main.rs`, `skinny/xtask/src/regen.rs`, or
  `skinny/xtask/src/regen_css.rs` in W5B.0.
- The new `SK_V14_W5B_FRONTEND_OWNER_PATHS` roster admits the aggregate W5B
  source owner paths named by `SPEC.md` for W5B.1..W5B.4 after W5B.0 admits:
  `crates/grammar/src/lib.rs`, `crates/codegen/src/lib.rs`,
  `crates/codegen/src/grammar_provider.rs`, `xtask/src/main.rs`,
  `xtask/src/regen.rs`, `xtask/src/regen_css.rs`, and
  `crates/bbnf-bench/src/lock14_baseline.rs`. No neutral successor module is
  admitted in W5B.0.
- W5B-FRONTEND parent-diff routing admits subjects containing
  `sk-v14-waveW5B-FRONTEND`, `sk-v14-waveW5B-FRONTEND-redress`, or the explicit
  W5B.0..W5B.4 spellings `sk-v14-waveW5B0`/`sk-v14-waveW5B.0` through
  `sk-v14-waveW5B4`/`sk-v14-waveW5B.4`. W5C-GEN and W5D-DELETE subjects remain
  rejected until their own Lock 14 gates land.
- The provider/template mutation guard rejects `M`, `A`, `D`, `R`, and `??`
  statuses across `git status --porcelain`, unstaged `git diff --name-status`,
  cached `git diff --cached --name-status`, and parent diff for protected
  providers/templates. The sole provider exception is
  `crates/codegen/src/grammar_provider.rs`; template paths have no exception.

Falsifiability gate:

- `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench lock14_baseline::tests::w5b_lock14_frontend_owner_paths_admit --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench lock14_baseline::tests::w5b_lock14_frontend_rejects_w5c_subject --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench lock14_baseline::tests::w5b_lock14_frontend_rejects_w5d_subject --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench lock14_baseline::tests::w5b_lock14_frontend_rejects_modified_provider --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench lock14_baseline::tests::w5b_lock14_frontend_rejects_modified_template --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench lock14_baseline::tests::w5b_lock14_frontend_all_templates_guard_counts_8 --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench lock14_baseline::tests::w5b_lock14_frontend_allows_grammar_provider_exception --profile ax-iter -- --exact`
- `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench lock14_baseline::tests::w5b_lock14_frontend_generic_owner_leak_census --profile ax-iter -- --exact`

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
and parent-diff router in the Lock 14 gate. `validate()` consumes the
provider/template smuggling guard through `validate_w5a_provider_template_topology()`.
The eight exact unit tests consume the new W5B-FRONTEND routing and
provider/template guard in the same redress commit.

Pre-blocked routes:

- No provider/template deletion.
- No provider-free generator-body replacement.
- No grammar/codegen/xtask frontend implementation edit in W5B.0.
- No public `@ws` revival.
- No grammar-name branch leakage in generic crates.
- No W5C-GEN or W5D-DELETE unblock from W5B.0 alone.
