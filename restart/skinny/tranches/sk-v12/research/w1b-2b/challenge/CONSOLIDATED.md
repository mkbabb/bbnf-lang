# SK-V12 W1b-2b CHALLENGE Consolidated

Date: 2026-05-20.
Wave: W1b-2b - CSS L4 Lightningcss SOTA Report + Admission Gate.
Disposition: REVISE; route back to plan.

## Lens Results

- CH1 correctness: ACCEPT.
- CH2 generality / Lock 14: ACCEPT.
- CH3 regression / REDRESS: REVISE.
- CH4 cost: REVISE.
- CH5 hidden coupling: REVISE.
- CH6 anti-paper-close: ACCEPT.

## Blocking Revisions

1. Normalize W1b-2b REDRESS and gate labels. The revised plan must require
   `REDRESS-125` only for W1b-2b and the exact gate name
   `G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA`. Stale REDRESS 124 or W1b-2 labels in
   schema/test/outcome text are not implementation authority.
2. Keep the implementation inside the Section 7.2 cap by narrowing the redress
   surface. Either cut live Criterion parsing from W1b-2b or explicitly raise
   the LOC/time budget. Under the existing cap, redress should be a focused
   companion report/gate validator with a counted test set.
3. Make hidden-coupling checks executable. The revised plan must specify that
   report-provided Mbps, thresholds, margins, and sample counts are consistency
   checks only unless recomputed from a named authority; run-id and artifact
   binding must cover Track 1, cssparser, lightningcss, equality, benchmark,
   profile, and validation paths; CSS-only JSON guard roots must fail closed;
   direct cssparser use in the lightningcss comparator path must remain
   forbidden; companion flag collision handling must use the shared parser.
4. Preserve the RESULTS movement rule: no `skinny/RESULTS.md` movement for
   `PASS-MEASURED-BASELINE`; movement is allowed only for a real CSS
   `PASS-ADMIT-CANDIDATE` or an accepted measured JSON guard demotion.

The accepted surfaces are retained: a dedicated `sk-v12-css-l4-sota-v1`
companion schema, strict three-way equality, independent cssparser oracle,
lightningcss comparator evidence, exact CSS row identity, no new directive/BIR/
`BackendShape`/public substrate API, process-level Lock 14, explicit scalar
Lock 16 non-claim, and W5 owning final SK-V12 reconciliation.
