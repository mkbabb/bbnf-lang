# SK-V13 W12 Plan - CSS Delimiter SIMD Production Split

Date: 2026-05-21.
Wave: W12.

## Selected Intervention

Promote the SK-V12 W4 test-local delimiter finder into production as
`bbnf_simd::find_ascii_set_member64(bytes, cursor, end, set)`, backed by
`prim::byte_class_from_eq_set_64` on 64-byte windows and a scalar tail. Wire the
API into the generated CSS L4 declaration-values `scan_block` loop for the
exact delimiter set `b"{};"`.

This completes the REDRESS-126 production split for the already micro-proven
`a64_ascii_set_run_skip` route. The wave does not attempt JSON PMULL/CSSC or
quote-aware CSS scanners.

## Owner Paths

- `skinny/crates/bbnf-simd/src/lib.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_ascii_set_member_find_64.rs`
- `skinny/xtask/src/main.rs`
- `skinny/crates/codegen/src/css_l4_declaration_values_templates/generated.rs`
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/REDRESS.md`
- `skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md` only if a row
  admits or an existing row status must be refreshed by the W12 gate.
- `restart/skinny/tranches/sk-v13/research/w12/`

Any JSON runtime/parser/codegen edit, x86 source edit, quote-aware CSS scanner
edit, or unrelated CSS feature expansion is REVISE.

## Falsifiability Gate

Gate id: `G-W12-SIMD-ASM-PRODUCTION`.

The gate requires:

- scalar reference status `pass`;
- strict caller checkasm for `find_ascii_set_member64`;
- production consumer status `wired` for
  `css_l4/declaration_values/direct_to_struct/main`;
- strict CSS fact-stream equality vs the existing cssparser/golden and
  lightningcss comparators;
- `orphan_count_after = 0`;
- REDRESS-126 cited as production-split history;
- no JSON guard or admitted CSS row demotion.

If Criterion shows production movement and the existing CSS declaration-values
row still clears lightningcss + 1, W12 may record `PASS-ADMIT`.

If the production-wired scanner is correct but does not move the row, W12
records `REJECTED-MEASURED`, saves the source patch at
`/tmp/skv13-waveW12-rejected.patch`, reverts the production consumer/API, and
keeps the zero-orphan evidence. Checkasm-only or microbench-only close is not
allowed.

## Measurement

1. `BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-simd --release --test checkasm_ascii_set_member_find_64 -- --nocapture`.
2. `BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo run -p xtask --release -- primitive-checkasm`.
3. `RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench lightningcss_sidecar_matches_generated_track1_and_cssparser -- --nocapture`.
4. `RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench writes_gate_consumed_css_l4_report -- --nocapture`.
5. `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench nonjson_css_l4 -- nonjson_css_l4/track1_generated_css_l4_decl_values`.
6. `cargo xtask gate-json --check-results --skv13-simd-asm-production-report ...` once the W12 report exists.

## Revert Protocol

On measured reject, save `/tmp/skv13-waveW12-rejected.patch`, revert
`bbnf-simd`, CSS runtime/template, gate/report, and xtask source edits, and
commit only the W12 REDRESS rejection and evidence artifacts.

## Pre-Blocked Routes

- REDRESS 126 blocks another production-split deferral.
- REDRESS 88/89/96/97/98 block local JSON structural SIMD body swaps and
  parser-local structural sidecars.
- REDRESS 122 remains the escape-mask prerequisite; this wave does not touch
  escape handling.
- No retained or new aarch64 orphan may remain after the wave.
