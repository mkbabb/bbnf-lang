# SK-V10 W10 CHALLENGE Consolidated

Pass: Wave CHALLENGE.
Cycle: W10.
Date: 2026-05-19.
Plan reviewed: `restart/skinny/tranches/sk-v10/research/w10/w10-plan.md`.
Disposition: ACCEPT WITH WATCHPOINTS.

## CH1 Correctness

ACCEPT. `instruments/direct_to_struct` is a direct digest row, has a Section
0.2 floor, and currently clears both Track 1 and Track 2 in `RESULTS.md`. The
plan keeps the generated Track 1 caller and independent hand Track 2 oracle in
the same output plane.

Watchpoint: fresh Criterion under `/tmp/skv10-w10-criterion` must still clear
both `instruments` direct floors before `RESULTS.md` moves.

## CH2 Generality / Lock 14

ACCEPT. The intervention is report/gate row-control behavior. It does not add
JSON policy to a generic crate, generated parser, runtime parser, or SIMD
primitive. The direct contract remains a product-plane gate, not a retained
substrate change.

## CH3 Regression / REDRESS

ACCEPT WITH WATCHPOINTS. Direct guard rows `citm_catalog`, `marine_ik`, and
`unicode_basic` must be measured in the same target capture and remain above
their maintain floors. Typed guard rows are not refreshed unless the report
renderer updates typed rows; if it does, typed floors must hold.

## CH4 Cost

ACCEPT. The expected code change is a narrow extension of the existing W2
direct reclamation table and provenance marker. It should be smaller and less
risky than a new direct parser mechanism. Do not refactor the direct contract
unless required by tests.

## CH5 Hidden Coupling

ACCEPT. The plan does not depend on W7, W8, W9, or W3. It must not silently
admit `mesh`, `random`, or other rows if they happen to pass in a local run;
that would exceed the planned target set and require a plan update.

## CH6 Anti-Paper-Close

ACCEPT. The row movement is not allowed from a manual `RESULTS.md` edit alone:
`gate-json` must render the W10 provenance and `Report::validate_sk_v8_w0`
must consume the same fields. A moved row without `REDRESS-109`, `SK-V10-W10`,
strict direct comparator evidence, and measured-row validation is a reject.

## Final Disposition

Proceed to redress. W10 admits only `instruments/direct_to_struct` if fresh
measurement, gate rendering, report validation, and guard floors pass. On any
miss, revert the W10 source/status slice and record a measured REDRESS reject.
