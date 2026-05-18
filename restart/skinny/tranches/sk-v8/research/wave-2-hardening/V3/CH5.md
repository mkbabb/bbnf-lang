# SK-V8 W2 Hardening V3 CH5 Review

Date: 2026-05-18.
Reviewer: CH5.
Target reviewed: current HEAD `8ce03af4`
(`fix(sk-v8-wave2-gate): fold typed hardening disposition`), unchanged from
the W2 V2-folded packet reviewed in
`restart/skinny/tranches/sk-v8/research/wave-2-hardening/V2/`.
Lens: governance, hidden coupling, and anti-deferral discipline.

## Verdict

ACCEPT.

Confidence: 95%.

## Findings

1. The V2-folded packet is unchanged for CH5 purposes. Current HEAD is still
   `8ce03af4`, there are no tracked dirty or staged file changes, and the W2
   disposition surfaces reviewed by V2 remain intact: `skinny/REDRESS.md`,
   `skinny/RESULTS.md`, `restart/skinny/tranches/sk-v8/HANDOFF.md`,
   `restart/skinny/tranches/sk-v8/SPEC.md`, and the W2 research/plan docs.

2. The anti-deferral challenge holds. W2 does not say "benchmarks later" as a
   hidden close condition. REDRESS 91 admits only
   `apache_builds/real_typed_struct` and `citm_catalog/real_typed_struct` as
   source/product rows, rejects `canada/real_typed_struct`, leaves
   `skinny/RESULTS.md` unchanged, and rejects benchmark row-table admission for
   this wave (`skinny/REDRESS.md:2622`, `skinny/REDRESS.md:2637`,
   `skinny/REDRESS.md:2648`). That is a disposition, not an implicit TODO.

3. W2 can be considered disposed for W3 entry without hiding row-table work.
   HANDOFF states the same split at the top of the file: W2 has source/product
   parity admitted and benchmark row-table admission rejected for this wave,
   with W3 as the next dispatchable wave only after its own research, plan,
   challenge, and redress gate
   (`restart/skinny/tranches/sk-v8/HANDOFF.md:5`). The later W2 disposition
   record repeats that the measured manifest remains the W0 four
   `real_typed_struct` rows and that W2 does not claim six measured
   `real_typed_struct A / GO` rows
   (`restart/skinny/tranches/sk-v8/HANDOFF.md:175`).

4. The row-table state is visible and bounded. `skinny/RESULTS.md` still
   contains exactly four measured `real_typed_struct` rows: `twitter`,
   `update_center`, `mesh`, and `marine_ik`
   (`skinny/RESULTS.md:7`, `skinny/RESULTS.md:18`,
   `skinny/RESULTS.md:21`, `skinny/RESULTS.md:28`). It has no measured
   `apache_builds/real_typed_struct`, `citm_catalog/real_typed_struct`, or
   `canada/real_typed_struct` rows. The missing W2 measured rows are not being
   smuggled into W3; they are explicitly rejected/routed for W2.

5. No CH5 hidden-coupling issue was found on re-challenge. W2 remains confined
   to the existing real typed schema/generator path and generated DirectBuild
   product-plane consumers. REDRESS says it adds no directive, BIR variant,
   `BackendShape`, substrate surface, sidecar, parser-owned cursor, runtime
   JSON behavior, or direct digest product claim (`skinny/REDRESS.md:2626`).
   The focused `lock14_baseline` suite passed, including the tests that admit
   only W2-scoped typed-owner parent diffs and reject W2-scoped parent diffs
   outside those paths. `cargo xtask check-real-typed` also passed.

6. V3 can serve as the second ACCEPT cycle for this CH5 governance lane if the
   V3 packet accepts it. V2 CH5 already ACCEPTed the V2-folded packet with no
   required folds. This V3 CH5 re-challenge reaches the same result on the
   unchanged packet, with the row-table rejection still explicit and W3 still
   gated by its own entry requirements. This does not create measured W2
   Apache/CITM rows; it only confirms that W2 is disposed for W3 entry.

## Required Folds

None.

Preserve the current wording: W2 admits source/product parity for Apache and
CITM, rejects Canada for W2, rejects benchmark row-table admission for this
wave, leaves `skinny/RESULTS.md` at the W0 four measured `real_typed_struct`
rows, and requires W3 to proceed through its own plan/challenge/redress gate.
