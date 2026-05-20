# SK-V11 S-P2 CH3 Regression / REDRESS Firewall

Pass: S-P2 CHALLENGE. Cycle: V3.
Date: 2026-05-20.
Scope: regression and REDRESS-boundary review of the S-P2 V3 research cohort.
Output: this file.
Disposition: ACCEPT.
Accept rate contribution: 1/6.
Required redress: none.

## Inputs Read

- `restart/prompts/skinny/PASS-2-RESEARCH.md`.
- V3 research cohort:
  `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md`,
  `p2b-dav1d-process.md`, `p2c-arch-esoterica.md`,
  `p2d-substrate-tape.md`, `p2e-parse-that-gaps.md`, and
  `p2f-grammar-neutral.md`.
- V2 consolidated hardening:
  `restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md`.
- `skinny/REDRESS.md` through Item 110, with specific review of
  REDRESS 96/97/98/102.

## Findings

1. ACCEPT - V3 preserves the V2 accepted REDRESS boundary rather than expanding
   the candidate pool. The V2 consolidation accepted CH3 because x4 Unicode work
   was proof-only, string-block work was caller-gated, W3/class-column/sidecar
   routes stayed pre-blocked, and retained parse callers were guards only
   (`restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md:15`-`24`).
   Its required V3 fold asked only for a stability carry-forward, not substantive
   candidate redress (`restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md:41`-`56`).
   The V3 files do that: P2-A, P2-C, P2-D, P2-E, and P2-F all mark V3 as a
   stability fold and carry the same blocked-route facts
   (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:20`-`30`,
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:9`,
   `restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md:10`,
   `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:13`-`17`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:10`-`17`).

2. ACCEPT - The SK-V9 W3 falsification remains closed. REDRESS 96 rejected the
   class-column plus move-consumed structural-position vector after green
   correctness checks and failed every W3/W10b gate row
   (`skinny/REDRESS.md:2795`-`2848`). REDRESS 97 rejected the allocation-free
   streaming cursor after the same gate failure pattern
   (`skinny/REDRESS.md:2850`-`2906`). REDRESS 98 retires
   `G-W3-UNION-SUBSTRATE` and identifies the union-substrate thesis as falsified
   on this host (`skinny/REDRESS.md:2910`-`2949`). V3 does not reopen any of
   those routes: P2-A rejects retained class lanes, structural-position vectors,
   streaming cursors, parser-owned projections, hidden sidecars, and parse-only
   wins (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:16`-`18`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:82`-`84`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:242`-`245`);
   P2-D narrows substrate work to the existing source-offset tape plus
   direct/typed consumers (`restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md:9`-`24`,
   `restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md:66`-`68`);
   and P2-F keeps masks transient with no retained class/position sidecar
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:43`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:76`-`82`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:95`-`96`).

3. ACCEPT - The parse-only firewall is preserved. REDRESS 102 admits only a
   proof firewall: no behavior source, generated output, benchmark body, row
   movement, W3 substrate dependency, retained class column, structural cursor,
   streaming cursor, or parser-owned structural projection
   (`skinny/REDRESS.md:3040`-`3058`). V3 candidates require generated direct,
   typed, or non-JSON product-plane consumers for admission. P2-A states retained
   parse is diagnostic only (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:72`-`78`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:239`-`241`);
   P2-D makes retained parse a compatibility guard or micro-proof only
   (`restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md:38`-`48`);
   P2-E requires product-plane row gates and strict no-regression
   (`restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:35`-`43`);
   and P2-F requires a non-JSON generated parser benchmark before S-P3
   generality claims (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:86`-`91`).

4. ACCEPT - String and Unicode escape routes keep the V2 firewall. REDRESS 106
   rejected the full string primitive micro-proof at the caller level despite
   scalar/checkasm parity (`skinny/REDRESS.md:3150`-`3170`). REDRESS 107 admits
   `unescape_uxxxx_x4_neon` as proof-only with no row movement
   (`skinny/REDRESS.md:3172`-`3196`). REDRESS 108 rejects production reuse of the
   already-consuming `unescape_string` caller without a real source delta
   (`skinny/REDRESS.md:3198`-`3222`). V3 respects all three: P2-B keeps
   `HEX_QUARTET_X4_PROOF` proof-only and blocks `unescape_string` reuse
   (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:268`,
   `restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:330`-`356`);
   P2-C makes x4 production require a new caller and strict x4 parity, and keeps
   widened string blocks behind a new caller micro-proof
   (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:45`-`62`,
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:98`-`99`);
   P2-E blocks decoded scratch, materializer reuse, semantic string facts, and
   x4 proof-to-production promotion (`restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:48`-`50`,
   `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:82`,
   `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:90`);
   and P2-F keeps C3 as neutral escape/hex work or host-function work, not JSON
   Unicode policy in generic crates (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:45`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:78`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:101`).

5. ACCEPT - PMULL, CSSC CTZ, movemask, and cache-hint inventory does not reopen
   rejected SIMD production routes. REDRESS 88 rejects PMULL prefix-XOR as the
   default hot body after parse regressions (`skinny/REDRESS.md:2508`-`2540`),
   and REDRESS 89 rejects the CSSC CTZ next-bit bulk consumer despite green
   correctness and asm proof (`skinny/REDRESS.md:2542`-`2585`). P2-B restates
   both as risks (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:292`-`306`).
   P2-C leaves PMULL/CTZ, SHA3 ternary, PRFM/STNP, cache hints, and movemask-only
   work as support or inventory unless a future packet has scalar parity,
   same-wave consumer evidence, row gates, and no-regression fallback
   (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:21`-`23`,
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:77`-`81`,
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:94`-`96`).
   P2-F similarly restricts C7 to same-wave C1/C2/C6 support with no retained
   bitmap output (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:49`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:82`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:107`).

6. ACCEPT - The remaining direct/typed candidate boundaries do not reopen older
   REDRESS traps. Container dispatch stays local/current-pointer or generated
   FIRST-set dispatch; object next-key carry and value-byte compaction stay
   blocked (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:138`-`160`,
   `restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md:32`-`35`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:48`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:105`).
   Numeric work stays digit-span/accumulation only and rejects f64 fallback,
   mantissa widening, and parser-owned number policy
   (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:162`-`184`,
   `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:49`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:103`).
   Digest/hash is carried as benchmark oracle or per-product host sink only, not
   parser vocabulary or a generic-crate primitive
   (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:186`-`212`,
   `restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:275`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:55`-`56`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:83`).

## Disposition

ACCEPT. V3 preserves the V2 REDRESS firewall and does not reopen REDRESS
96/97/98/102, parse-only admission, W3 substrate repair, x4 production reuse,
full-string primitive paper admission, PMULL/CTZ default production rewires,
object/value-byte carry, numeric fallback widening, or digest-as-parser
vocabulary.

Accept rate contribution: `1/6`.

## Required Redress

None for S-P2 V3.

Carry-forward constraints for S-P3:

1. Any candidate with a mask, classifier, dispatch, string, escape, numeric, or
   tape shape must keep a same-wave generated direct, typed, or non-JSON product
   consumer; retained parse rows are guard evidence only.
2. Any W3-adjacent wording in S-P3 must remain refusal/pre-block language unless
   a new Alpha contract explicitly supersedes REDRESS 96/97/98/102.
3. Any x4 escape packet remains proof-only unless it names a new source delta,
   scalar x4 oracle, strict valid/invalid/mixed/alignment/surrogate parity, and a
   same-wave direct/typed/non-JSON product consumer beyond `unescape_string`.
4. Any string-block packet must be caller-level and row-gated; primitive parity
   alone does not admit production.
5. Any PMULL, CTZ, SHA3 ternary, movemask, or cache-hint route is support-only
   until a materially different product-plane packet clears scalar/checkasm,
   feature fallback, same-host micro-proof, and row no-regression gates.
