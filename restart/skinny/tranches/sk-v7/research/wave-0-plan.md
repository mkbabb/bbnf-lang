# SK-V7 Wave 0 Plan: sonic-rs strict comparator rebuild

Inputs: `restart/skinny/tranches/sk-v7/SPEC.md` §2,
`restart/skinny/tranches/sk-v7/SYNTHESIS.md` §3.1,
`restart/skinny/tranches/sk-v7/HANDOFF.md` §3, and
`restart/skinny/tranches/sk-v7/research/wave-0-r1-comparator-plane.md`.

Intervention: remove the `utf8_lossy` feature from the `sonic-rs` bench
dependency, rerun the comparator bench, regenerate `skinny/RESULTS.md`, and
archive the strict-baseline deltas.

## Owner Paths

- `skinny/crates/bbnf-bench/Cargo.toml`
- `skinny/crates/bbnf-bench/src/lib.rs` (verification only)
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v7/research/wave-0-strict-baseline.md`

## Falsifiability Gate

- `cargo tree -p bbnf-bench --edges=features | rg 'sonic-rs|utf8_lossy'`
  shows `sonic-rs` without `utf8_lossy`.
- `cargo bench -p bbnf-bench --bench json_parity` completes on the strict
  rebuild.
- `cargo run -p bbnf-bench --bin gate --release` refreshes `skinny/RESULTS.md`.
- sonic-rs Mbps drops 3-8% against the pre-W0 `skinny/RESULTS.md` rows, or the
  strict-baseline report names each exception.
- `instruments` parse reaches PASS against strict sonic, and `unicode_basic`
  reaches PASS or records the residual gap.
- Track 1 and Track 2 do not regress because no bbnf parser/runtime code is
  changed.

## Hard Cap

60 minutes for redress: 1 minute edit, 30 minutes bench, 15 minutes gate/report
rewrite, 14 minutes report and REDRESS close.

## Revert Protocol

If the feature-tree check fails, restore `utf8_lossy` in
`skinny/crates/bbnf-bench/Cargo.toml`, record a W0 rejection in
`skinny/REDRESS.md`, and do not advance to W1.

If the strict bench or gate fails for environmental reasons, keep the Cargo
patch unstaged, save the working diff to `/tmp/skv7-wave-0-rejected.patch`,
record the exact command failure in `skinny/REDRESS.md`, and halt W0 as
rejected.

If the strict bench completes but the falsifiability gate misses, keep the
strict feature change only if it makes the comparator plane honest; otherwise
revert via `/tmp/skv7-wave-0-rejected.patch`. In both cases, record a same-row
REDRESS entry with the measured miss and the next candidate shape.

## Same-Wave Consumer

The consumer is the existing `json_parity` bench and gate path:
`sonic_rs::from_slice` is exercised by `sonic_rs_anchor`,
`sonic_rs_direct_to_struct`, and `sonic_rs_real_typed_struct`; the refreshed
`skinny/RESULTS.md` is the runtime evidence.

## Pre-Blocked Routes

This wave must not reopen performance interventions from
`restart/skinny/tranches/sk-v7/HANDOFF.md` §3. In particular, it does not touch
the Class A tiny-string wiring rejected by REDRESS 28 and 33, the SK-V5 UTF-8
fusion family rejected by REDRESS 50-55, or the SK-V6 retained-parse and
direct-materialization families rejected by REDRESS 60-72.

## Schema Handling

The current report emitter does not yet emit every PASS-ALPHA §4.3 schema v3
column. W0 will preserve the gate-generated report, annotate strict-vs-lossy
deltas in `wave-0-strict-baseline.md`, and record any unpopulated schema v3
columns as W0 follow-up evidence rather than widening the comparator repair
into a report-harness refactor.
