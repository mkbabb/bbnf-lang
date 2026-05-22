# SK-V13 W1 Plan - CSS Comparator/Oracle Harness Expansion

Wave: W1. Phase: Plan. Date: 2026-05-21.

## Selected Intervention

Land `G-W1-CSS-COMPARATOR-ORACLE`: a SK-V13 CSS coverage-matrix report that is
consumed by `cargo xtask gate-json --check-results` and that delegates the
existing declaration-values admission proof to the Criterion-backed
lightningcss/cssparser SOTA gate.

This is a harness/gate intervention only. It does not admit a new CSS feature
and it does not generate a new parser row.

## Owner Paths

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/xtask/src/main.rs`
- `restart/skinny/tranches/sk-v13/research/wave-1-*`
- `skinny/RESULTS.md` only if the W1 measurement refresh changes CSS manifest
  evidence
- `skinny/REDRESS.md` only on measured reject

## Redress Slice

1. Add `sk-v13-css-comparator-oracle-v1` report structs and validation.
   Validation requires:
   - wave id `SK-V13-W1` and run id prefix `sk-v13-w1:`;
   - all 24 SK-V13 CSS feature ids exactly once;
   - one measured declaration-values row and 23 explicit open-absent rows;
   - no `PARTIAL`, no admitted absent rows, and no harness-only admission;
   - coverage totals matching row sums.

2. Add `--skv13-css-comparator-oracle-report` to the bbnf-bench gate and xtask
   passthrough.
   - The flag is incompatible with JSON result update/probe flags.
   - The flag requires `--check-results`; W1 cannot skip JSON guard validation.
   - The gate loads the W1 report, validates the 24-feature matrix, then loads
     and validates the referenced SK-V12 CSS SOTA report against Criterion lanes
     and retained artifacts.

3. Harden CSS rolling threshold parsing so `threshold_mbps` is accepted only
   inside the `lightningcss_strict[...]` comparator segment and must equal
   `lightningcss_mbps + 1.0`.

4. Emit the W1 coverage report under the tranche research directory and record
   the redress evidence.

## Exit Gate

- `cargo test -p bbnf-bench --bin gate` passes.
- `cargo test -p xtask` passes.
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-css-comparator-oracle-report ../restart/skinny/tranches/sk-v13/research/wave-1-css-comparator-oracle.json` passes.
- The declaration-values row still satisfies the SK-V13 maintain floor through
  the existing Criterion-backed SK-V12 SOTA proof.
- `ROLLING-SOTA-DELTA.md` still contains exactly 51 JSON rows and 24 CSS rows,
  with only declaration-values numeric/admitted.

## Pre-Blocked Routes

W1 does not touch parser behavior, SIMD, union substrate, generated code, or
CSS row production. It therefore does not reopen REDRESS 28/33, 50-72, 82-84,
88/89, 96-98, 112/113, or 123-127 as behavior routes. REDRESS 123-127 remain
gate feed only; the W1 report cannot count the SK-V12 declaration-values row as
full CSS close.

## Revert Protocol

Revert the W1 report structs, companion flag, xtask passthrough, threshold
parser hardening, and W1 report artifact. Retain this plan and the research
artifact. If the comparator cannot be made same-plane and freshness-bound,
record REDRESS with the failing report and Criterion/artifact mismatch.
