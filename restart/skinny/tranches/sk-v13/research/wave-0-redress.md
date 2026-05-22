# SK-V13 W0 Redress

**Date:** 2026-05-21
**Wave:** W0 - Baseline, telemetry, rolling delta
**Disposition:** PASS

## What Landed

- Refreshed `skinny/RESULTS.md` with the native `SK-V13-open` JSON gate
  output while retaining the SK-V12 CSS L4 declaration-values admission row.
- Added `restart/skinny/ROLLING-SOTA-DELTA.md` with 75 targets:
  - 51 JSON rows: 17 corpora across `parse_only`, `direct_to_struct`, and
    `real_typed_struct`.
  - 24 CSS L4 parity feature rows, including the admitted
    `declaration_values` row and 23 open feature targets.
- Updated `xtask gate-json --check-results` to consume the rolling table before
  delegating to the bench gate.
- Updated the bench gate result writer so JSON refreshes preserve retained
  non-JSON CSS rows instead of silently deleting them.

## Gate Evidence

Commands run:

```text
cargo test -p xtask
cargo test -p bbnf-bench --bin gate
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory
```

Results:

- `cargo test -p xtask`: PASS, 3 tests.
- `cargo test -p bbnf-bench --bin gate`: PASS, 21 tests.
- Native `gate-json --check-results --advisory`: PASS. The rolling-delta
  precheck accepted exactly 51 JSON rows and 24 CSS feature rows, then the
  bench gate accepted `skinny/RESULTS.md`.

## Guardrails

- No parser, scanner, SIMD, codegen, generated runtime, product behavior, or
  generated real-typed bench file remains modified.
- The generated real-typed bench side effect produced during one gate run was
  reverted before admission because it is outside W0 owner paths.
- W0 admits no performance row. It only establishes the consumed telemetry and
  rolling-delta baseline for subsequent waves.
