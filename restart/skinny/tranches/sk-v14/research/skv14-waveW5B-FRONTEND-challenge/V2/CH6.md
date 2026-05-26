# SK-V14 W5B-FRONTEND CHALLENGE V2 CH6 Anti-Paper-Close

Date: 2026-05-26.
Lens: CH6 Anti-Paper-Close.
Disposition: REVISE.

## Findings

1. The plan replaces SPEC full-table maintain with static no-diff proof.
   `skv14-W5B-FRONTEND-plan.md:116`-`121` describes exact no-diff, `:152` is
   existing-results capture only, and `:168`-`:169` are diff checks. This does
   not fold V1 CH6's exact-evidence blocker at
   `skv14-waveW5B-FRONTEND-challenge/V1/CH6.md:18`-`21` or the V1 consolidated
   fold at
   `skv14-waveW5B-FRONTEND-challenge/V1/HARDENING-SKV14-W5B-FRONTEND-V1-CONSOLIDATED.md:43`-`44`
   against `SPEC.md:750`.
2. The nonzero-pass proof is aggregate. `skv14-W5B-FRONTEND-plan.md:141`-`145`
   requires nonzero proof, but the shown glob can pass if only one log has
   `ok. N passed`. It does not prove each exact gate at
   `skv14-W5B-FRONTEND-plan.md:128`-`138`, including missing-import and cycle
   tests, ran nonzero.

## Accepted Folds

- Same-wave consumers are concrete at `skv14-W5B-FRONTEND-plan.md:215`-`230`.
- Revert protocol exists at `skv14-W5B-FRONTEND-plan.md:198`-`213`.
- Negative missing-import/cycle tests are named at `:91`, `:129`-`:130`, and
  `:221`.
- No paper frontend-IR: request-local only at `:85`-`:87`; no sidecar/runtime
  surface at `:244`-`:245`.
- W5B sub-slices are not deferrals at `:17`-`:20` and `:47`-`:49`.

## Required Fold

- Add fresh full-table maintain evidence against `SK-V14-open` within +/-1.0%
  on all rows, or amend SPEC before accept.
- Replace glob-only nonzero proof with per-test/per-log assertions.
