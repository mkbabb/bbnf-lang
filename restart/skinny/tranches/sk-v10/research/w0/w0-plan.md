# SK-V10 W0 Plan - Telemetry Freeze

Pass: Wave Plan.
Cycle: W0.
Date: 2026-05-19.
Scope: gate-only W0 redress plan for `G-W0-TELEMETRY-FREEZE`.

## Entry Gate

PASS.

- S-P3 converged through V3 confirmation.
- The current RESULTS surface is reproducible from the frozen
  `target/skv9-w1/criterion` capture.
- The opening row surface matches SPEC Section 3: 17 parse `S / NO-GO`, 17
  direct rows with 3 `A / GO` and 14 `N-direct / NO-GO`, and 6 typed
  `A / GO`.

## Selected Intervention

Update the W0 cost-facts RESULTS snapshot guard to the SK-V10 opening surface.

Owner path:

- `skinny/xtask/src/main.rs`

No source parser, generated parser, benchmark body, `RESULTS.md`, or row
disposition edit is authorized.

## Exit Gate

`G-W0-TELEMETRY-FREEZE` from SPEC Section 3.

Required evidence:

```text
CRITERION_HOME=target/skv9-w1/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo xtask gate-json --advisory
```

```text
CRITERION_HOME=target/skv9-w1/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo xtask gate-json --with-cost-facts --check-results
```

The first command must confirm the report schema/run-id/metadata consumers and
the second must confirm the frozen RESULTS manifest markers, uniform run id,
and 40-row SK-V10 opening surface.

## LOC Budget And Risk

Budget: 120-240 gate/report LOC. Expected redress is below 20 LOC.

Risk: LOW-MEDIUM. The change narrows a stale gate invariant to the already
committed SK-V10 opening manifest size. It does not relax row-level evidence,
run-id grammar, marker checks, nonproducer status, or cost-facts validation.

## Revert Protocol

Revert the `skinny/xtask/src/main.rs` gate invariant change as one slice if
either evidence command fails. Leave `skinny/RESULTS.md` unchanged and record a
REDRESS rejection naming the malformed evidence consumer.

## Same-Wave Consumer

`cargo xtask gate-json --with-cost-facts --check-results` consumes the changed
row-count invariant in the same wave.

## Pre-Blocked Routes

W0/W0b telemetry from REDRESS 77/78 remains reporting authority only. This plan
does not use sidecar freshness, PMU, cycles, masking probes, or structural
scans as parser producers or strict admission shortcuts.
