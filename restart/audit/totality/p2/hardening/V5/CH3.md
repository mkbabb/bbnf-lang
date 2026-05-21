# T-P2 V5 CH3 Regression / REDRESS Confirmation

Pass: T-P2 Totality Research.
Cycle: V5 unchanged-packet confirmation.
Lens: CH3 regression / REDRESS.
Agent: CH3.
Date: 2026-05-21.
Ownership: `restart/audit/totality/p2/hardening/V5/CH3.md` only.

## Verdict

ACCEPT.

The unchanged V4 packet still satisfies CH3. The V4 REDRESS-slice table keeps
material-differential requirements attached to each reopened family, does not
silently reopen prior REDRESS rejects, and does not demote prior guard floors or
admitted-row requirements.

## Findings

| ID | Disposition | Finding | Evidence |
|---|---|---|---|
| CH3-V5-001 | ACCEPT | The V5 cycle is correctly reviewing an unchanged V4 packet, and V4 explicitly carries the older REDRESS contracts forward. | The V4 consolidated file says V5 must challenge the V4 research packet as-is with no dossier or addendum edits expected (`restart/audit/totality/p2/hardening/HARDENING-T-P2-V4-CONSOLIDATED.md:32`-`37`). The V4 addendum states that it supplements V2/V3 and does not weaken Lock 14, Lock 1, REDRESS, material-differential, anti-paper-close, state-machine, or numeric abrogate contracts (`restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:11`-`14`). |
| CH3-V5-002 | ACCEPT | The V4 slice table preserves material-differential requirements instead of turning route labels into admissions. | V4 says a slice reaches S-P3 only if its blocker is cleared in the wave plan; otherwise it remains research evidence (`restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:60`-`64`). The table names expected row gates, rollback paths, abrogate thresholds, and blockers for each JSON direct, union, source-present, CSS, and parse-that slice (`restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:66`-`79`). 2B's checklist says `Union-C`, `PMULL+CSSC`, `SIMD-first union`, `UDOT`, `TBL/TBX`, or any other ASM-gen label is not shortlist-safe unless prior REDRESS rows, old scalar-cost treatment, row-local consumer, strict parity/checkasm, guard rows, rollback, and abort criteria are named (`restart/audit/totality/p2/2B-primitive-vocabulary.md:237`-`254`). |
| CH3-V5-003 | ACCEPT | Prior REDRESS rejections are not silently reopened. | REDRESS 88 rejected PMULL as a default `bitmap_prefix_xor_64` body after JSON row regressions despite correctness and visible asm (`skinny/REDRESS.md:2510`-`2540`); REDRESS 89 rejected the narrowed CSSC/CTZ bulk consumer after guard-row regressions despite passing correctness gates (`skinny/REDRESS.md:2544`-`2585`). REDRESS 96 and 97 were correctness-green retained-union attempts that missed every required row, and REDRESS 98 retired the union-substrate thesis on the host (`skinny/REDRESS.md:2823`-`2848`, `skinny/REDRESS.md:2881`-`2906`, `skinny/REDRESS.md:2910`-`2940`). V4 keeps `RS-UNION-PMULL-CSSC` non-shortlist until a combined matrix and exact first consumer exist, with replay of prior regression, no consumer, parity failure, or guard regression as abrogate thresholds (`restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:74`). |
| CH3-V5-004 | ACCEPT | REDRESS-119 direct rows remain controlled by row-specific differentials, not broad reopen language. | REDRESS 119 closed W8 as measured direct fixpoint with no behavior source intervention and no `RESULTS.md` row movement, and admits no direct row (`skinny/REDRESS.md:3497`-`3527`). REDRESS 120 keeps the 13 residual direct rows exhausted unless a future pass names a material differential beyond REDRESS 114-119 with fresh profile and micro-proof evidence (`skinny/REDRESS.md:3531`-`3553`). V4 maps selected direct slices to `semantic_digest_simd_mix` or `digit_run_accumulate_udot`, but each row keeps strict sonic row movement, prior guards, rollback, and blockers for byte-hash substitution, missing parity, or missing first consumer (`restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:68`-`73`). |
| CH3-V5-005 | ACCEPT | Proof-only and microbench-only rows remain non-admitting prerequisites, so there is no hidden production reopen. | REDRESS 122 admits `escape_mask_64` only as a correctness prerequisite and makes no production scanner, SIMD body, gate, `RESULTS.md`, or row admission change (`skinny/REDRESS.md:3603`-`3632`). REDRESS 126 records the ASCII delimiter route as `ROUTE-PRODUCTION-SPLIT`, not CSS ADMIT or production SIMD/ASM admission, and says production CSS wiring and same-wave consumer work are separate (`skinny/REDRESS.md:3766`-`3820`). V4 mirrors that classification: `escape_mask_64` is non-shortlist until consumed, decode/digit/context/cache rows remain non-shortlist where strict parity or exact callers are missing, and `RS-CSS-ASCII-RUN-SKIP` becomes dispatchable only after S-P3 names the exact generated scan-block row (`restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:49`-`58`, `restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:78`). |
| CH3-V5-006 | ACCEPT | Prior guard floors and admission states are not demoted. | V3's numeric abrogate caps fail closed on row regression, parity/checkasm/equality failure, stale-cost overflow, e-graph/CSP overflow, and generated LOC overrun (`restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md:103`-`115`). 2B repeats that primitive, union, and resolver routes fail closed on any scalar/checkasm/equality failure or any silently demoted admitted JSON/CSS row (`restart/audit/totality/p2/2B-primitive-vocabulary.md:256`-`268`). REDRESS 127 records CSS L4 `PASS-ADMIT` while preserving JSON guards with no demotion and leaving union and ASM-gen as separately routed, historically constrained work (`skinny/REDRESS.md:3824`-`3868`). V4 preserves normalized admission-state handling by reserving `admissibility_state` for the V3 enum and treating prose labels only as summary/blocker text (`restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:81`-`86`). |
| CH3-V5-007 | ACCEPT | The 2A-2F dossiers remain aligned with the CH3 posture. | 2A says REDRESS-119 reopens only row-by-row and union/PMULL/CSSC/SIMD labels are not material differentials without the consumer-level checklist (`restart/audit/totality/p2/2A-sota-landscape.md:60`-`63`). 2C says primitive parity alone is not admission and requires generated grammar policy plus same-wave consumer measurement (`restart/audit/totality/p2/2C-grammar-neutrality.md:79`, `restart/audit/totality/p2/2C-grammar-neutrality.md:145`). 2D says REDRESS 96/97/98 and 119 remain binding historical evidence and route-family names are not material differentials without row-local consumer and changed dataflow (`restart/audit/totality/p2/2D-cost-model.md:46`-`49`). 2E requires prior REDRESS, scalar-cost disposition, production consumer path, strict gate, rollback, and abort criteria before PMULL/CSSC/EOR3/UDOT/TBL routes become shortlist-safe (`restart/audit/totality/p2/2E-host-arch-esoterica.md:184`-`200`). 2F carries REDRESS 119 row-reopen discipline and keeps proof-only REDRESS 122/126 out of admission claims (`restart/audit/totality/p2/2F-parse-that-gaps.md:54`-`61`). |

## Required Repairs

None for CH3.

## Evidence Checked

- `restart/prompts/totality/PASS-2-RESEARCH.md`.
- `restart/audit/totality/p2/hardening/HARDENING-T-P2-V4-CONSOLIDATED.md`.
- `restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md`.
- `restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md`.
- `restart/audit/totality/p2/2A-sota-landscape.md` through
  `restart/audit/totality/p2/2F-parse-that-gaps.md`.
- `skinny/REDRESS.md`, especially REDRESS 88, 89, 96, 97, 98, 119, 120, 122,
  126, and 127.
