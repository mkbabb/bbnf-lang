# SK-V11 S-P2 V2 CH6: Anti-Paper-Close / Evidence Discipline

Pass: S-P2 CHALLENGE.
Cycle: V2.
Date: 2026-05-19.
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
- `restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md`

## Findings

1. **Candidate admission is no longer self-certified.** PASS-2 requires every
   primitive to carry SOTA antecedent, scalar-reference shape, checkasm/parity
   discipline, and grammar-neutral generalisation before S-P3 may shortlist it
   (`restart/prompts/skinny/PASS-2-RESEARCH.md:1`-`8`,
   `restart/prompts/skinny/PASS-2-RESEARCH.md:95`-`123`). V2 now restates that
   rule inside the artifacts instead of relying on later plan prose: P2-A admits
   candidates only with a scalar reference, direct/typed consumer, same-output
   proof, and reject boundary (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:12`-`18`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:42`-`45`);
   P2-B makes scalar oracle, strict differential, micro-proof, feature gate, and
   same-wave consumer common to all SIMD/ASM rows (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:242`-`259`);
   P2-E gives explicit parity commands and product-row thresholds
   (`restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:33`-`41`).

2. **The V1 x4 escape paper-close is fixed.** V1 failed because the x4 Unicode
   route could be read as production by reusing an already-consuming caller. V2
   blocks that reading in multiple independent artifacts: P2-B marks
   `HEX_QUARTET_X4_PROOF` proof-only, requires a new scalar x4 oracle and strict
   valid/invalid/mixed/alignment/surrogate coverage, and states there is no V2
   production consumer (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:124`-`145`,
   `restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:263`);
   P2-A allows `hex_escape_quad_decode` only as support until a new source delta
   and same-wave product consumer exist (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:207`-`212`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:237`-`239`);
   P2-E defines the admissible shape as a new segment-stream product consumer,
   rejecting the existing `unescape_string` route (`restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:48`,
   `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:76`-`82`);
   P2-F carries C3 as neutral escape/hex proof or new-segment work, not JSON
   production (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:36`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:53`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:69`).

3. **Digest/hash is no longer a parser primitive or row mover by assertion.**
   P2-A reclassifies C8 as a non-parser output-plane surface with scalar digest
   source references, an output proof, and a reject boundary if no direct/typed
   row moves (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:174`-`200`).
   P2-B marks `OUTPUT_DIGEST_HASH_ORACLE` process/oracle-only unless a concrete
   consumer packet supplies scalar source, parity, feature/fallback, and cost
   gate (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:270`).
   P2-E excludes `output_digest_hash` from parse-that primitives
   (`restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:27`-`29`,
   `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:92`), and
   P2-F keeps C8 as benchmark oracle or host-sink only
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:42`-`47`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:74`).

4. **Rows with missing production evidence are explicitly downgraded, not
   paper-closed.** P2-B labels missing-consumer rows proof-only, support-only, or
   process/oracle-only (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:255`-`259`).
   P2-C demotes PMULL, CTZ, SHA3 ternary, PRFM/STNP, and cache hints to support
   or inventory because no same-wave consumer is present
   (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:71`-`79`).
   P2-D gives D1-D5 output-plane declarations, scalar-output parity plans,
   micro-proof boundaries, and no-op fallbacks instead of using tape/lazy facts
   as admissions (`restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md:37`-`47`).
   That is compatible with S-P2 being research-only: durable harness or
   production changes land only in later wave packets
   (`restart/skinny/tranches/sk-v11/HANDOFF.md:116`-`118`).

5. **Grammar generalization is grounded in concrete proof surfaces.** V2 does not
   claim Lock 14 by prose. P2-F maps C1-C7 across CSS L4, Sheets, and BBNF-self
   with line-cited grammar surfaces and records C8/C9 as non-parser surfaces
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:65`-`75`).
   It then requires a generated non-JSON direct/typed benchmark with Track 1,
   independent Track 2 or oracle, strict output equality, primitive self-time,
   checkasm/parity for SIMD, fallback, and no sidecar or generic-crate grammar
   names (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:77`-`82`).
   This satisfies the S-P1 authority that JSON-only profile telemetry may
   nominate primitive families but cannot prove CSS/Sheets/BBNF-self behavior
   (`restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:53`-`55`).

6. **Evidence hygiene is acceptable for CH6.** The retained candidates now cite
   local scalar sources, prior REDRESS blockers, strict test commands, and output
   comparators at the row where the claim is made. One non-blocking cleanup note:
   P2-F's source appendix cites `p2e-parse-that-gaps.md:31-127`
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:115`),
   while the V2 P2-E artifact ends earlier; the proof-critical P2-F findings
   cite the actual P2-E candidate and grammar sections inline
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:12`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:51`-`59`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:65`-`75`).
   This is not a blocking paper-close defect because it does not hide any
   candidate's scalar sketch, source reference, strict gate, or reject boundary.

## Residual Guards For S-P3

- Do not shortlist `HEX_QUARTET_X4_PROOF`, `MOVEMASK_EXHAUSTIVE_GATE`,
  PMULL/CTZ/SHA3/cache inventory, C8, or C9 as standalone row-moving parser
  primitives.
- Any widened string-special or UDOT/digit packet must carry the scalar oracle
  and strict parity it currently names as an admission prerequisite, not merely
  the existing primitive smoke coverage.
- Any non-JSON wave must materialize a generated parser and benchmark it; the
  current JSON-provider codegen limitation is acknowledged as a generalization
  risk, not accepted as closure (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:22`,
  `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:104`).

## Verdict

ACCEPT. V2 grounds the candidate pool at S-P2 depth and routes incomplete or
unsupported ideas to proof-only, support-only, inventory-only, oracle-only, or
accounting-only status. No candidate closes by future promise alone.
