# SK-V11 W9 Close Plan

Pass: Wave Plan.
Cycle: W9 Close.
Date: 2026-05-20.
Gate: `G-W9-CLOSE-SK-V11`.

## Selected Close Shape

Close SK-V11 as a converged measured fixpoint under REDRESS 120.

This is not direct `GO` and not grammar-generalization success. The close
claim is narrower:

- W1a-W8 all have dispositions.
- Every Section 0.4 residual direct row is still `N-direct / NO-GO` but has a
  REDRESS 119 per-row fixpoint proof.
- The non-JSON generated direct/typed parser intervention axis is BLOCKED from
  REDRESS 113 and carried forward.
- Existing direct and typed guards remain unchanged.
- Parse-only remains diagnostic and the SK-V9 W3 union/substrate family remains
  pre-blocked.

## Owner Paths For Redress

The close redress may edit:

- `restart/skinny/tranches/sk-v11/research/close/close-redress.md`
- `restart/skinny/tranches/sk-v11/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v11/SPEC.md`
- `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v11/HANDOFF.md`
- `skinny/REDRESS.md`

It may not edit behavior source, generated parser output, benchmark bodies,
gate/report code, telemetry schema, or `skinny/RESULTS.md`.

## Required Close Edits

Redress must:

1. Add a close-redress artifact recording final wave dispositions, final result
   surface, verification commands, and routed SK-V12 remainder.
2. Append REDRESS 120 as the SK-V11 close record.
3. Reconcile packet documents so they agree that SK-V11 converged as measured
   fixpoint with overall `N-direct / NoGo`, direct plane not green, and
   grammar-generalization BLOCKED.
4. Present G-Alpha SK-V11 -> SK-V12 as downstream feedback authority only.
5. Preserve `skinny/RESULTS.md` unchanged.

## Verification

Before commit, run:

```text
git diff --exit-code -- skinny/RESULTS.md
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run --manifest-path skinny/Cargo.toml -p bbnf-bench --bin gate -- --advisory
git diff --check
```

## Revert Protocol

If any close document claims direct `GO`, non-JSON success, parse-only SOTA, or
source movement, revise before redress commit. If `RESULTS.md` changes, revert
the close slice and record the blocker instead.
