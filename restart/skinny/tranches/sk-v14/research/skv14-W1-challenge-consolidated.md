# SK-V14 Wave W1 Seven-Lens Challenge Consolidated

Date: 2026-05-24.
Wave: W1.
Phase: challenge.
Plan challenged: `skv14-W1-plan.md` at commit `1ec7d78a2`.

## Verdict

REVISE. The V1 plan chose the right fused intervention, but its gates were not
strong enough to falsify a false W1 close. The V2 addendum in
`skv14-W1-plan.md` resolves the REVISEs before redress begins.

## CH1 Correctness

REVISE. W1 cannot make equality conditional on future admits after pruning all
JSON rows. SPEC Section 4 and P3-C require W1 to consume comparator and
per-iteration equality columns for the JSON grid. V2 requires structured PASS
for every benchmarked JSON row and explicit intrinsic-block rows only where the
typed product surface is absent.

## CH2 Invariant And Lock Discipline

ACCEPT. W1 does not amend the 16-lock count, BackendShape canon, Pattern H
runtime count, or substrate-union elevation. The V2 plan keeps all parser,
runtime, codegen, generated output, and corpus content out of scope.

## CH3 Ledger And Prune Coverage

REVISE. PRUNE-1 had not yet landed in `RESULTS.md`,
`restart/skinny/ROLLING-SOTA-DELTA.md`, or `skinny/REDRESS.md`. V2 requires zero
JSON rolling `ADMITTED`, zero visible JSON `A | GO`, and exactly 22 new
row-keyed REDRESS entries continuing after `REDRESS-160`.

## CH4 Scope And Cost

REVISE. The V1 plan lacked a concrete LOC budget and rollback checkpoint. V2
adds per-slice caps, keeps the direct comparator decision to `JsonDirectDigest`
strict sink-shape equivalence, and makes source rollback mandatory before any
ledger edits if wrapper or manifest gates fail.

## CH5 Hidden Evidence Coupling

REVISE. Existing code still allowed DOM parse-only evidence, Track 2 comparator
function coupling, W0 equality placeholders, and historical sidecar strict
anchors. V2 requires 32-cell manifest parsing and gate rejection of stale
`comparator_evidence`, `sonic_rs_anchor`, Track2=comparator, and sidecar strict
anchor evidence.

## CH6 Dependency Ordering

ACCEPT WITH CONDITION. W1 closes before W2 dispatch. W2 through W6 remain
independent only after W1 records an executable W1 state; W7, W9, and W10 remain
blocked by any unresolved W1 comparator/equality failure.

## CH7 Overfit-Prune

REVISE. The raw stale-anchor grep was too broad for negative tests and too weak
for production evidence. V2 scopes grep to production comparator code while
requiring tests to construct stale strings if negative fixtures are needed.
V2 also forbids paper PASS equality without a timed-region call site.

## Resolution

All REVISEs are routed into `skv14-W1-plan.md` §3.1 and §4. No source or ledger
redress starts until those bindings are committed.
