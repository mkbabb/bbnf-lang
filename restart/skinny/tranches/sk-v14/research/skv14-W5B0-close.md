# SK-V14 W5B.0 Close: Lock14 Frontend Gate

Date: 2026-05-26.
Status: ADMIT.
Implementation commit: `c52e624c6`.

## Scope

W5B.0 added the Lock 14 W5B-FRONTEND owner roster and parent-diff routing before
any W5B frontend source path moved. The source patch stayed confined to
`skinny/crates/bbnf-bench/src/lock14_baseline.rs`.

The admitted gate now:

- aggregates `SK_V14_W5B_FRONTEND_OWNER_PATHS`;
- routes `sk-v14-waveW5B-FRONTEND` and W5B.0..W5B.4 subject forms through the
  W5B roster;
- keeps W5C-GEN and W5D-DELETE subjects rejected;
- counts all eight `*_templates` dirs, including `json_templates`;
- rejects modified, added, deleted, renamed, and untracked protected providers
  and templates;
- preserves `crates/codegen/src/grammar_provider.rs` as the sole neutral
  provider-shaped exception.

## Evidence

Each exact test was run under `ax-iter`, teeing to its dedicated log, with a
dedicated nonzero `rg "test result: ok\\. [1-9][0-9]* passed"` proof:

- `/tmp/skv14-w5b-w5b_lock14_frontend_owner_paths_admit.log`
- `/tmp/skv14-w5b-w5b_lock14_frontend_rejects_w5c_subject.log`
- `/tmp/skv14-w5b-w5b_lock14_frontend_rejects_w5d_subject.log`
- `/tmp/skv14-w5b-w5b_lock14_frontend_rejects_modified_provider.log`
- `/tmp/skv14-w5b-w5b_lock14_frontend_rejects_modified_template.log`
- `/tmp/skv14-w5b-w5b_lock14_frontend_all_templates_guard_counts_8.log`
- `/tmp/skv14-w5b-w5b_lock14_frontend_allows_grammar_provider_exception.log`
- `/tmp/skv14-w5b-w5b_lock14_frontend_generic_owner_leak_census.log`

Additional executable check:

- `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench lock14_baseline::tests::accepts_current_allowlist --profile ax-iter -- --exact`

Diff hygiene:

- `rustfmt --check skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `git show --check --stat --oneline c52e624c6 -- skinny/crates/bbnf-bench/src/lock14_baseline.rs`

Package-wide `cargo fmt --manifest-path skinny/Cargo.toml -p bbnf-bench --check`
is not a W5B.0 proof because pre-existing formatting drift in
`generated_real_typed.rs` and `report.rs` fails that command outside the W5B.0
owner path.

## Downstream

W5B.0 does not close W5B-FRONTEND. W5B.1 IMPORT-CLOSURE is the next active
sub-wave. W5C-GEN and W5D-DELETE remain blocked until W5B.1..W5B.4 all admit.
