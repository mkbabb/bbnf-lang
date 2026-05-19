# SK-V11 Pass Alpha Hardening V1 Consolidated

Date: 2026-05-19.
Pass: Pass Alpha CHALLENGE.
Cycle: V1.
Scope: Consolidate CH1-CH6 review of the SK-V11 Alpha packet.

## Disposition

ACCEPT.

The six-lens Alpha CHALLENGE returned one ACCEPT and five
ACCEPT-WITH-NITS, with no REVISE or REJECT. The packet can present G-Alpha
and dispatch S-P1 after the folds below.

| Lens | File | Disposition |
|---|---|---|
| CH1 correctness | `research/alpha-hardening/V1/CH1-correctness.md` | ACCEPT-WITH-NITS |
| CH2 generality / Lock 14 | `research/alpha-hardening/V1/CH2-generality-lock14.md` | ACCEPT |
| CH3 regression / REDRESS | `research/alpha-hardening/V1/CH3-regression-redress.md` | ACCEPT-WITH-NITS |
| CH4 cost / feasibility | `research/alpha-hardening/V1/CH4-cost.md` | ACCEPT-WITH-NITS |
| CH5 hidden coupling | `research/alpha-hardening/V1/CH5-hidden-coupling.md` | ACCEPT-WITH-NITS |
| CH6 next-tranche / anti-paper-close | `research/alpha-hardening/V1/CH6-next-tranche.md` | ACCEPT-WITH-NITS |

## Folded Changes

- CH1 corrected Alpha-F's typed-plane prose from five of seven raw sonic
  wins to six of seven; only `update_center` is below sonic throughput while
  still admitted under the slack gate.
- CH3 folded the PMULL-class prefix-XOR and CSSC/CTZ bulk-emission default
  pre-block into `SYNTHESIS.md` and Alpha-F. EOR3 or first-set extraction can
  only re-enter as caller-local, feature-gated, micro-proven direct/typed or
  non-JSON consumers with scalar fallback.
- CH3 clarified Alpha-E Candidate 1 from "measurement substrate" to
  "measurement harness" so it cannot be mistaken for a parser substrate.
- CH5 bound non-JSON telemetry choice to W0: S-P3 must freeze whether rows
  live in `skinny/RESULTS.md` or a companion report, and name the gate command
  before any non-JSON behavior wave dispatches.
- CH5 narrowed Candidate 5's consumer wording to generated direct/typed or
  non-JSON string/identifier callers, explicitly excluding parse-only
  structural producers.
- CH5 clarified that Alpha-E's `research/p1`, `research/p2`, and `research/p3`
  entries are pass artefact output roots, not behavior redress owner paths.
- CH4 clarified that pre-S-P3 micro-proofs are read-only research artifacts,
  throwaway `/tmp` benches, or existing bench invocations. Durable harness or
  production changes wait for the S-P3-authored wave packet.

## Hardening Verdict

G-Alpha can honestly present SK-V11 to S-P1. The Alpha packet remains
source-free, leaves `SPEC.md` and `DISPATCH-PROMPT.md` to S-P3, blocks
parse-only and W3 paper-close routes, requires direct row closure or measured
fixpoint, binds one non-JSON grammar intervention to an admitted benchmark,
and keeps aarch64 SIMD work behind micro-prove-first plus same-wave consumer.
