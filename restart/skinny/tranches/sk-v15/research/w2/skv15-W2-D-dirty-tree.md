# SK-V15 W2-D - Dirty Tree And Owner Paths

Scope: read-only owner-path collision audit before W2 planning.

## W2 Owner Path Status

No W2 owner candidate was dirty or staged at dispatch:

- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/xtask/src/main.rs`
- `skinny/xtask/src/skv15_w0.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`
- `restart/skinny/tranches/sk-v15/research/w2/`

## Unrelated Dirty Work

The dirty tree is unrelated to W2 and must remain untouched. It includes root
runtime files under `crates/core/src/runtime/**`, `docs/precepts`, SK-V12/SK-V13
research JSONs, generated skinny CSS runtime files,
`skinny/crates/bbnf-bench/src/generated_real_typed.rs`, root `xtask/src/main.rs`,
and root `xtask/src/regen_simple_runtime.rs`.

W2 staging must therefore be path-explicit. Do not use broad `git add .`.
