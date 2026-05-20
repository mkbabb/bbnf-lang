# SK-V11 S-P2 V3 CH6: Anti-Paper-Close / Evidence Discipline

Pass: S-P2 CHALLENGE.
Cycle: V3.
Date: 2026-05-20.
Lens: CH6 anti-paper-close / evidence discipline.
Disposition: ACCEPT.

## Read Basis

- `restart/prompts/skinny/PASS-2-RESEARCH.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md`
- `restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v11/research/p2/hardening/V2/CH6.md`

## Findings

1. **V3 is a valid stability fold, not a new paper admission.** PASS-2 says
   S-P2 selects and sequences nothing and requires grounded evidence rather than
   future-wave placeholders (`restart/prompts/skinny/PASS-2-RESEARCH.md:3`-`11`,
   `restart/prompts/skinny/PASS-2-RESEARCH.md:133`-`138`). The V2
   consolidation required only a stability fold that preserved the accepted
   candidate pool and carried forward five explicit facts
   (`restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md:41`-`56`).
   V3 does that: P2-A keeps C1-C5 as parser candidates and C8 as non-parser
   output-plane surface (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:20`-`30`);
   P2-B says proof-only SIMD rows and digest oracle stay outside parser row
   movers (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:9`-`12`);
   P2-C preserves AArch64-only inventory/candidate boundaries
   (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:9`);
   P2-D keeps the existing offset tape plus direct/typed consumer union only
   (`restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md:9`-`10`);
   P2-E preserves four parse-that gaps and keeps retained parse as guard only
   (`restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:13`-`17`);
   and P2-F preserves C1-C7 while routing C8/C9 out of parser primitives
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:10`-`17`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:37`-`56`).

2. **Proof-only and oracle-only surfaces remain clearly routed.** The x4 Unicode
   route is still proof-only: P2-A allows `hex_escape_quad_decode` only as support
   until a new source delta and same-wave product consumer exist
   (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:214`-`224`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:249`-`251`);
   P2-B's `HEX_QUARTET_X4_PROOF` row has no production `RESULTS.md` claim and
   rejects existing `unescape_string` reuse
   (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:247`-`264`,
   `restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:268`);
   P2-C marks x4 escape hex decode proof-gated only
   (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:54`-`62`);
   P2-E requires a new escaped-segment consumer and rejects the existing
   `unescape_string` path (`restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:45`-`50`,
   `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:78`-`84`);
   and P2-F carries the same surface as neutral escape/hex proof or new-segment
   work, not JSON production (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:45`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:62`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:78`).

3. **Digest/hash is not being paper-closed as parser speed.** P2-A labels C8 a
   non-parser output-plane surface and rejects it if digest logic enters generic
   parser crates or no direct/typed row moves
   (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:186`-`212`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:257`-`259`).
   P2-B makes `OUTPUT_DIGEST_HASH_ORACLE` process/oracle-only absent a concrete
   consumer packet (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:275`).
   P2-E excludes `output_digest_hash` from parse-that primitives
   (`restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:21`-`31`,
   `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:94`),
   and P2-F marks C8 as benchmark oracle only across CSS, Sheets, and BBNF-self
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:51`-`56`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:83`).

4. **Rows with missing production evidence are downgraded rather than deferred.**
   P2-B's common admission process requires scalar oracle, strict differential,
   feature gate, micro-proof, and same-wave consumer, and says rows without a
   consumer cannot be promoted by primitive parity alone
   (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:247`-`264`).
   P2-C keeps movemask, `EXT`, PMULL/CTZ, SHA3, PRFM/STNP, and cache hints as
   support or inventory when no V3 same-wave consumer exists
   (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:73`-`81`).
   P2-D gives D1-D5 output-plane declarations, micro-proof/reject boundaries, and
   no-op plans instead of treating tape or lazy-materialization facts as row
   admissions (`restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md:38`-`48`).
   P2-E requires exact parity commands, row gates, and `>= 1.0%` useful movement
   or proof-only status (`restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:35`-`43`).

5. **Grammar generalization remains evidence-gated.** P2-F identifies the current
   `json_provider` codegen path as an S-P3 Lock 14 blocker before any CSS,
   Sheets, or BBNF-self generated-parser claim
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:15`-`17`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:31`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:111`-`113`).
   It then requires a generated non-JSON direct/typed benchmark with Track 1,
   independent Track 2 or oracle, strict output equality, primitive self-time,
   PMU/cycles where available, strict checkasm for SIMD, fallback, no sidecar, no
   generic-crate grammar names, and same-wave generated consumer
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:86`-`91`).
   That satisfies the anti-paper-close requirement: non-JSON work is not accepted
   by prose, only routed to a measurable S-P3 gate.

6. **Evidence discipline is acceptable for convergence.** The pass contract says
   two consecutive >=95% ACCEPT cycles with zero open critical defects advance
   S-P2 (`restart/prompts/skinny/PASS-2-RESEARCH.md:155`-`158`). V2 was the first
   accepting cycle at 6/6 with no open critical defects
   (`restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md:26`-`39`).
   CH6 V2 already accepted the evidence routing for incomplete SIMD, digest/hash,
   and x4 escape surfaces (`restart/skinny/tranches/sk-v11/research/p2/hardening/V2/CH6.md:121`-`123`).
   V3 preserves those facts and adds no future-promise admission. Minor citation
   drift remains non-critical: for example, P2-F's sibling summary line cites
   compact ranges that do not cover every candidate row exactly
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:21`), but
   the proof-critical candidate rows and gates are present in the same artifacts
   and are line-cited elsewhere in this review.

## Required Redress

None.

## Residual Guards For S-P3

- Do not shortlist `HEX_QUARTET_X4_PROOF`, `MOVEMASK_EXHAUSTIVE_GATE`,
  PMULL/CTZ/SHA3/cache inventory, C8, or C9 as standalone row-moving parser
  primitives.
- Any S-P3 non-JSON wave must first resolve the `json_provider` codegen blocker
  or avoid claiming generated-parser generality.
- Any S-P3 kernel packet must carry scalar reference, strict parity/checkasm
  where applicable, same-host micro-proof, feature/fallback, and same-wave
  direct/typed/non-JSON consumer before production admission.

## Verdict

ACCEPT. V3 is a faithful stability fold of the V2 accepted research pool. It
does not close by future promise, and the proof-only, support-only,
inventory-only, oracle-only, and accounting-only surfaces remain clearly routed
away from parser row admission.
