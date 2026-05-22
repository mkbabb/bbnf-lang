# SK-V13 W11.3 Plan - JsonDigestSink Stack Specialization

Date: 2026-05-21.
Wave: W11.3.

## Selected Intervention

Replace hot closure-based scalar folding in `JsonDigestSink` with direct stack
frame matches for object and array scalar methods. The parser, generated
runtime, `JsonSink` trait, number/string parsing, and comparator semantics stay
unchanged.

The intended consumer change is local to
`skinny/crates/bbnf-bench/src/direct_struct.rs`:

- `array_string`, `array_i64`, `array_u64`, `array_f64`, `array_bool`,
  `array_null`
- `object_string`, `object_i64`, `object_u64`, `object_f64`, `object_bool`,
  `object_null`

The change may delete or bypass `with_object_parent` / `with_array_parent` only
if all direct parity tests remain green.

## Owner Paths

- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/xtask/src/main.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `restart/skinny/tranches/sk-v13/research/w11.3/`

Any runtime, codegen, parser, SIMD, or generic-crate edit is REVISE.

## Falsifiability Gate

Gate id: `G-W11.3-JSON-DIRECT-SINK-STACK`.

Primary rows:

- `json/instruments/direct_to_struct/main`
- `json/mesh/direct_to_struct/main`
- `json/random/direct_to_struct/main`
- `json/canada/direct_to_struct/main`

Admission requires at least one primary row with Track 1 > same-run sonic
strict + 1 Mbps, strict equality, Track 2 independence, and no silent demotion
of existing admits.

If no primary row admits, save `/tmp/skv13-waveW11.3-rejected.patch`, revert
the sink patch, and commit measured rejection.

## Measurement

1. Run targeted direct sink parity tests.
2. Run Criterion direct lanes for the selected primary rows.
3. If a row admits, add a companion gate report and refresh `RESULTS.md` /
   `ROLLING-SOTA-DELTA.md`.
4. If no row admits, do not refresh results; record measurements in REDRESS.

## Pre-Blocked Routes

- No parser or generated runtime edit.
- No digest shortcut or comparator weakening.
- No `JsonSink` trait expansion.
- No row-private fixture branch.
- No SIMD or source hook.
