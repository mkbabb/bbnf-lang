# SK-V14 W5B-C: Lock 14 Baseline Close

Date: 2026-05-26.
Scope: W5B research agent C, Lock 14 forward invariant and baseline gate.
Output: `restart/skinny/tranches/sk-v14/research/skv14-W5B-C-lock14-baseline.md`.
HEAD: `286233fa2`.

## Findings

`lock14_baseline::validate` currently calls the temporary W5A topology guard.
That guard expects eight legacy providers and seven CSS template directories,
and rejects provider/template add, delete, or rename before W5B. W5B must
replace it with a post-W5 guard.

Required post-W5 checks:

- Zero `*_provider.rs` files under `skinny/crates/codegen/src`, excluding only
  `grammar_provider.rs`.
- Zero `css_l4_*_templates` directories.
- Status/diff/cached/parent-diff checks reject any reintroduced provider or
  template path.
- W5B parent diff is authorized only under a W5B subject.
- Generated-header baseline removes deleted provider/template paths, otherwise
  the companion lint fails after deletion.

Required source changes in `lock14_baseline.rs`:

- Replace `validate_w5a_provider_template_topology(root)?` with the post-W5
  topology guard.
- Add `SK_V14_W5B_OWNER_PATHS` and extend `current_lock14_owner_paths`.
- Add `sk-v14-w5b`, `sk-v14-W5B`, and `sk-v14-waveW5B` parent-diff
  authorization.
- Update or replace W5A topology unit tests with post-W5 tests.
- If `grammar_profile.rs` is deleted or renamed, remove it from
  `GENERIC_SCAN_ROOTS` or make missing roots intentionally accepted.

## Required Commands

```sh
test "$(find skinny/crates/codegen/src -name '*_provider.rs' \! -name 'grammar_provider.rs' | wc -l | tr -d ' ')" = "0"
test "$(find skinny/crates/codegen/src -type d -name 'css_l4_*_templates' | wc -l | tr -d ' ')" = "0"
cd skinny && cargo test -p bbnf-bench lock14_baseline -- --nocapture
cd skinny && cargo xtask gate-json --check-results --skv14-existing-results-capture
```

## Risk

The post-W5 guard is enforceable only after a provider-free runtime generator
exists. At HEAD, `grammar_provider.rs` still calls `render_runtime_profile`, so
closing the topology guard before replacing that dispatch would create a
compile-time failure, not an admit.
