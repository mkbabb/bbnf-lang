# SK-V12 W1 A6 - REDRESS And Pre-Blocks

Scope: read-only audit of W1 REDRESS semantics, selected-target discipline,
and pre-blocked routes.

## Dispatch State

W1 is dispatchable because W0 admitted. W1 still must select exactly one target
in ordered target priority:

1. CSS L4 declaration values.
2. Sheets formula.
3. BBNF-self grammar.

Plan-time fallback evaluation is allowed. Redress fallthrough is not allowed:
once the W1 plan selects one target, redress either admits that target or
records BLOCKED/REJECTED evidence for that target.

## Admit, Block, Reject

W1 admits only if exactly one selected non-JSON generated row clears
`G-W1-GENERATED-NONJSON-BASELINE` with generated Track 1, independent oracle
or Track 2, strict equality, sample count >= 30, Track 1 Mbps >= 1, oracle
Mbps >= 1, and companion gate consumption.

W1 records BLOCKED when no generated baseline can be created inside the
accepted owner surface or when the gate is unmeasurable. W1 records REJECTED
when an admissible selected attempt runs and fails measurement, equality, or
guard requirements.

## Pre-Blocked Routes

The following routes remain hard-blocked for W1:

- REDRESS 111 report fixture as the baseline.
- REDRESS 112/113 future-phase promise.
- Hand-only non-JSON parser.
- Stale `sheets_witness`.
- JSON provider cloning under a neutral name.
- Generic JSON policy.
- New directive, BIR, or backend-shape additions.
- REDRESS 70/71 typed-output shortcuts.
- Source-only baseline claims without measured Mbps.

## Revert Protocol

On failure, revert the selected codegen/runtime/bench/report/gate/RESULTS
changes and generated files as one slice, save
`/tmp/skv12-waveW1-rejected.patch`, and add REDRESS naming the failed
preflight or measurement.
