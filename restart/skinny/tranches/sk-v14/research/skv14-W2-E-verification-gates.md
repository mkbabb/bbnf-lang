# SK-V14 W2-E: Verification Gates

Date: 2026-05-24.
Scope: Design W2 executable checks for `regen-css`, companion checks, and row-table non-movement.
Output: this file.

## §1 — Findings (concrete, file:line cited)

- Root `cargo xtask regen --check` already regenerates to a tempdir and byte-compares checked-in `.rs` files, with the tempdir loop at `xtask/src/regen.rs:570-614` and sidecar comparison at `xtask/src/regen.rs:615-635`. CI runs that root drift gate at `.github/workflows/ci.yml:55-58`.
- Skinny `codegen` already has reproducibility tests for all seven CSS runtime profiles: declaration values at `skinny/crates/codegen/src/lib.rs:504-510`, stylesheet/selectors at `skinny/crates/codegen/src/lib.rs:512-518`, declaration-values-extended at `skinny/crates/codegen/src/lib.rs:520-526`, visual functions at `skinny/crates/codegen/src/lib.rs:528-534`, at-rules/media at `skinny/crates/codegen/src/lib.rs:536-542`, vendor/custom at `skinny/crates/codegen/src/lib.rs:544-550`, and nested layout at `skinny/crates/codegen/src/lib.rs:552-558`.
- `skinny/xtask/src/main.rs:356-370` wires `gate-json --check-results` to W0/SK-V13 result snapshot validation. W2 should run this only as a no-regression/full-table check, not as CSS admission evidence.
- CSS report gates already require explicit `--check-results` for historical CSS reports; for example, the W2 stylesheet/selectors report path is rejected without `--check-results` at `skinny/crates/bbnf-bench/src/bin/gate.rs:117-124`.
- Historical CSS gate validation compares report values to Criterion lanes and checks close tolerance via `require_close` at `skinny/crates/bbnf-bench/src/bin/gate.rs:1062-1074` and `skinny/crates/bbnf-bench/src/bin/gate.rs:2576-2582`. These are useful guard patterns, but W2 is not a throughput admit wave.

## §2 — Recommendations (named falsifiability gates)

- `G-W2-ROOT-CSS-REGEN`: run `cargo xtask regen --grammar css_l4 --output /tmp/skv14-w2-root-css` and byte-compare the generated `css_l4.rs` and `css_l4.registry.json` against `crates/core/src/grammar/generated/`; also run `cargo xtask regen --check`.
- `G-W2-SKINNY-REGEN-CSS`: from `skinny/`, run `rm -rf crates/runtime/src/grammars/css_l4_*` followed by `cargo xtask regen-css`, then require `git diff --exit-code -- crates/runtime/src/grammars` to be empty.
- `G-W2-COMPANION-CHECKS`: run every `cargo xtask check-css-l4-*` companion command plus `cargo test -p codegen css_l4_ -- --nocapture` or the focused profile-specific tests.
- `G-W2-FULL-TABLE-MAINTAIN`: run `cargo xtask gate-json --check-results --skv14-existing-results-capture` and, if cost-facts are in scope, `cargo xtask gate-json --with-cost-facts --check-results`; no CSS rows may become admitted from W2 alone.

## §3 — Risks (REDRESS entries to pre-block)

- A `regen-css` command that writes to a tempdir only will not satisfy the SPEC destructive round-trip gate at `restart/skinny/tranches/sk-v14/SPEC.md:491`.
- A successful provider reproducibility unit test is not enough to satisfy W2 unless the same emission is exposed through actual xtask commands and companion checks.
- Full-table JSON maintain is a no-regression guard; using it to claim CSS movement would reopen gate-relabel-as-admit.

## §4 — Sources (every external citation)

No external citations. Local repository sources only.
