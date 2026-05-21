# SK-V13 Alpha Hardening V2 Consolidated

Date: 2026-05-21.

Verdict: REVISE.

V2 corrected the V1 blockers for comparator semantics, 51-row accounting,
CSS close authority, REDRESS history, cost/cap realism, SPEC-local override
discipline, decision-engine fail-closed behavior, and anti-paper-close rules.
Four lenses accept the V2 packet. Two lenses require a narrow V3 fold before
Alpha can claim a clean convergence cycle.

## Lens Dispositions

| Lens | Verdict | Disposition |
|---|---|---|
| CH1 correctness | REVISE | Numeric and close-authority fixes accepted; remaining blocker is unanchored material citations in Alpha-A and Alpha-F. |
| CH2 generality / Lock | ACCEPT | SPEC-local override loophole closed; Lock 14/Lock 16, no-sidecar, single-tape, G-Omega, and decision-engine fail-closed constraints are carried. |
| CH3 regression / REDRESS | ACCEPT | REDRESS-119/120 are history only; 51 JSON rows including 10 absent typed rows are mandatory accounting; direct survey is priority/risk, not eligibility. |
| CH4 cost / concurrency | ACCEPT | CSS LOC arithmetic, hard caps, dependency/conflict matrix, and RESULTS/REDRESS serialization are sufficient for S-P1/S-P3 planning. |
| CH5 hidden coupling | REVISE | Remaining blocker is `bbnf-simd` alphabet-only dispatch: non-JSON consumers could inherit hardcoded JSON quote/escape/control constants without an explicit grammar-policy gate. |
| CH6 anti-paper-close | ACCEPT | Support-only, scaffold-only, deferred, ordinary measured-reject, weaker-scoping, old GO/slack, and missing rolling-delta closure routes are blocked. |

## Required V3 Fold

1. Replace the remaining file-level Alpha-A evidence bullets with line-resolving
   citations for profile truth, value/API union leaks, and SIMD/ASM/union scope.
2. Add line-level anchors to Alpha-F's source map for PASS-ALPHA, the addendum,
   SK-V12 close, CSS parity gap, profile truth, value/API union, and SIMD/ASM.
3. Add `G-SIMD-GRAMMAR-POLICY`: any wave that wires `bbnf-simd` into CSS,
   union, JSON parse_only, or shared generated code must prove the selected
   classifier uses the consuming grammar's quote/escape/control policy or a
   no-string policy. The gate requires scalar parity, checkasm/differential
   coverage for JSON and CSS policies, same-wave row consumer measurement, no
   public substrate API, and no retained sidecar classifier state.
4. Carry the CH4 accepted requirements into S-P3: bundled CSS rows need an exact
   parity matrix, E4 variant LOC must be selected per variant, W11/W14 fanout
   rows need row-local caps and route ownership, parallel redress may not
   parallel-write ledgers, and each wave must publish before/after rows plus
   ownership/conflict notes.

After the V3 fold, rerun the six-lens Alpha challenge. V2 is not an accepted
convergence cycle.
