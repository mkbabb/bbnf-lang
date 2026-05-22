# SK-V13 W0 Plan

**Date:** 2026-05-21
**Wave:** W0 - Baseline, telemetry, rolling delta
**Phase:** plan
**Entry gate:** PASS. G-Omega signoff and CRUD landed; SK-V13 S-P3 dispatch
authority promoted at `dad6094fc`; W0 research archived at `7fb7abfd3`.

## Owner Paths

Allowed by SPEC Section 3:

- `skinny/crates/bbnf-bench/`
- `skinny/xtask/src/`
- `skinny/RESULTS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `restart/skinny/tranches/sk-v13/research/wave-0-*`
- `skinny/REDRESS.md` only on reject

No parser, scanner, SIMD, codegen, generated runtime, or product behavior
source is in scope.

## Selected Intervention

Land the SK-V13-open telemetry lock as a consumed rolling-delta gate:

1. Create `restart/skinny/ROLLING-SOTA-DELTA.md`.
2. Render all 51 JSON target rows:
   - 17 `parse_only`
   - 17 `direct_to_struct`
   - 17 `real_typed_struct`
3. Render the 24 non-OUT_OF_SCOPE CSS parity feature targets, including the
   SK-V12 admitted declaration-values row and open rows for the remaining
   targets.
4. Add a gate consumer that rejects missing universes, duplicate rows, stale
   unsupported anchors, malformed margins, missing absent reasons, and missing
   G-Omega/SK-V13-open markers.
5. Wire the consumer through `cargo xtask gate-json --check-results` so W0
   does not produce telemetry that no same-wave gate consumes.

## Falsifiability Gate

`G-W0-BASELINE-ROLLING-DELTA`:

- `restart/skinny/ROLLING-SOTA-DELTA.md` exists.
- JSON target count is exactly 51 and covers every required corpus/plane pair.
- CSS target count is exactly 24 and includes no `PARTIAL` close status.
- Every numeric row has `margin = T1_current - T1_sota`.
- Missing rows use `absent:<reason>` for `T1_current`, `T1_sota`, and
  `margin`, and the reason is non-empty.
- `tranche_admitted` is one of `ADMITTED`, `OPEN`, `MISSING`,
  `ARCHITECTURAL-BLOCK`, or `OUT_OF_SCOPE`.
- The artifact declares `g_omega_status: signed` and
  `run_id: SK-V13-open`.
- `cargo xtask gate-json --check-results` consumes both `skinny/RESULTS.md`
  and the rolling-delta artifact.

## Consumer And Revert

Same-wave consumer: `xtask gate-json --check-results` invokes the SK-V13
rolling-delta validator before the JSON gate passthrough, so all W0 emitted
rolling telemetry is gate-consumed.

Revert protocol: revert `skinny/xtask/src/main.rs`,
`restart/skinny/ROLLING-SOTA-DELTA.md`, and any W0-only report/docs edits.
On reject, record the failure in `skinny/REDRESS.md` and save the rejected
patch under `/tmp/skv13-wave0-rejected.patch`.

## Measurement Plan

- `cargo test -p xtask`
- `cargo xtask gate-json --check-results --advisory`

The second command is allowed to preserve existing advisory semantics for
historical NO-GO rows, but any malformed rolling-delta table is a hard
pre-passthrough error.

## Routed Remainder

W0 does not admit CSS/JSON performance rows. It only makes subsequent rows
measurable and prevents silent omissions, stale run ids, permissive anchors,
or paper-close telemetry from entering SK-V13.
