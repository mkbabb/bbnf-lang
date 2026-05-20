# SK-V11 S-P2 V3 Challenge Consolidation

Pass: S-P2 Research CHALLENGE consolidation.
Cycle: V3.
Date: 2026-05-20.
Scope: consolidate the six S-P2 V3 challenge lenses and convergence verdict.
Output: this file.

## Inputs

- Research cohort: `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md` through `p2f-grammar-neutral.md`, committed at `723bd14b`.
- Prior accepting cycle: `restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md`.
- Challenge lenses: `restart/skinny/tranches/sk-v11/research/p2/hardening/V3/CH1.md` through `CH6.md`.
- Convergence authority: `restart/prompts/ORCHESTRATOR.md` §3Z and `restart/prompts/skinny/PASS-2-RESEARCH.md` §4.

## Disposition Matrix

| Lens | Disposition | Load-bearing accepted facts |
|---|---|---|
| CH1 correctness | ACCEPT | V3 stayed within V2 accepted facts; candidate hot-leaf traceability, strict comparator boundaries, and AArch64-only ISA grounding hold. |
| CH2 generality / Lock 14 | ACCEPT | The C1-C7 parser pool, C8 oracle/host sink, and C9 accounting split is preserved; `json_provider` remains an S-P3 Lock 14 gate before any non-JSON generated-parser claim. |
| CH3 regression / REDRESS | ACCEPT | W3/class-column/streaming-cursor, parse-only movement, string/Unicode proof-to-production, PMULL/CTZ, and cache-hint reopenings remain blocked. |
| CH4 cost | ACCEPT | Retained candidates still carry scalar reference, parity/checkasm or product parity, micro-proof, same-wave consumer, feature/fallback, output-plane, and reject-boundary tuples. |
| CH5 hidden coupling / Lock 1 | ACCEPT | V3 preserves the existing offset tape plus direct/typed consumer union and rejects sidecars, retained positions, class lanes, Track 1/Track 2 coupling, and hidden directives. |
| CH6 anti-paper-close | ACCEPT | V3 is a faithful stability fold; proof-only, support-only, oracle-only, and accounting-only surfaces remain explicitly routed and cannot paper-close S-P3. |

## Cycle Verdict

ACCEPT-rate: 6/6 = 100%.

REJECT list: none.

REVISE list: none.

Open critical defects: none.

Cycle verdict: ACCEPT.

Convergence verdict: S-P2 converged. V2 and V3 are two consecutive ≥95%
ACCEPT cycles with zero open critical defects and no unresolved REVISE items,
satisfying `ORCHESTRATOR.md` §3Z and `PASS-2-RESEARCH.md` §4.

## Accepted Candidate Pool For S-P3

S-P3 may shortlist only candidates that preserve the V3 constraints:

1. C1-C7 are the parser primitive pool: byte-set/class-table masking, bounded
   special-byte string scan, escape/hex segment decode, digit span/accumulate,
   byte-set layout skip, generated FIRST/prefix/lookahead dispatch, and
   movemask/bitmap support only with a same-wave C1/C2/C6 consumer.
2. C8 is output digest/hash oracle or per-product host sink only. It is not
   parser vocabulary and cannot enter generic parser crates as semantics.
3. C9 is Lock-1/output-plane accounting only. It is not a row-moving parser
   primitive.
4. `HEX_QUARTET_X4_PROOF`, PRFM/STNP/cache hints, PMULL/CTZ, and EOR3/BCAX are
   proof/support/inventory only until a later wave names a new source delta,
   scalar oracle, strict parity/checkasm, feature/fallback, same-wave consumer,
   and measured row gate.
5. The W3 union/event/class-column/streaming-cursor family remains REDRESS
   closed and cannot be re-derived as a substrate intervention.
6. Non-JSON generality must be exercised by generated direct/typed parser
   benchmarking; the live JSON-provider codegen path is a blocker that S-P3
   must gate before claiming CSS L4 / Sheets / BBNF-self proof.

## Hand-On

Update `restart/skinny/tranches/sk-v11/HANDOFF.md` to `ready-for-S-P3` and
dispatch S-P3 per `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`.
