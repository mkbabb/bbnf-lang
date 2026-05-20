# SK-V11 S-P2 CH3 Regression
Pass: S-P2 CHALLENGE. Cycle: V2.
Date: 2026-05-19.
Scope: REDRESS regression review of S-P2 V2 candidates.
Output: this file.
Disposition: ACCEPT.
Accept rate contribution: 1.

## Findings

1. ACCEPT - The V1 x4 escape production reopening is closed. V2 no longer
   carries `ESCAPE_UXXXX_X4_PRODUCTION`; P2-B renames the work to
   `HEX_QUARTET_X4_PROOF`, states "proof-only harness evidence; no production
   `RESULTS.md` row and no caller speed claim," and explicitly refuses the
   already-consuming `unescape_string` path without a new escaped-segment source
   delta and direct/typed consumer
   (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:141`,
   `restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:145`,
   `restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:263`).
   P2-C repeats the same boundary for x4 escape hex decode
   (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:52`-
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:60`), and
   P2-E requires `pt_escaped_string_segments` to name a new direct, typed, or
   non-JSON source delta beyond `unescape_string`
   (`restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:48`,
   `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:80`).
   That is a material differential from REDRESS 107 proof-only admission and
   REDRESS 108 production rejection (`skinny/REDRESS.md:3172`-
   `skinny/REDRESS.md:3222`).

2. ACCEPT - The string-block family is no longer an unqualified widening route.
   P2-A limits C2 to direct/typed product consumers, marks retained parse guards
   as guards only, and rejects 64-byte retained trusted scans,
   generated-retained `StringBlock16`, NEON tiny-parser wiring, and
   primitive-parity-only production claims
   (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:87`-
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:100`).
   P2-B makes REDRESS 106 blocking explicit and requires caller rows plus
   no-regression gates before any production wiring
   (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:264`,
   `restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:325`-
   `restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:334`).
   P2-C's widened string-special block is admissible only as a new narrow
   micro-proof with its own scalar 64 oracle and a same-wave generated
   direct/typed caller; it rejects REDRESS 61/62/83/106 reopenings
   (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:43`-
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:51`).
   This is sufficient for S-P2 research. S-P3 must reject any packet that turns
   this back into retained wide scanning or primitive-parity-only production.

3. ACCEPT - W3, sidecar, class-column, and parse-only substrate routes are
   blocked across the V2 cohort. P2-A makes REDRESS 96/97/98/102 binding in the
   front matter and rejects retained class lanes, structural-position vectors,
   streaming cursors, parser-owned projections, hidden sidecars, and parse-only
   wins (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:12`-
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:18`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:70`-
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:72`).
   P2-D narrows "substrate union" to the existing source-offset tape plus
   direct/typed consumers, says W3 is not a candidate substrate repair, and
   makes retained parse guard-only
   (`restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md:9`,
   `restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md:23`,
   `restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md:39`).
   P2-F requires same-loop mask consumption and rejects retained
   class/position sidecars for the generated parser candidates
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:34`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:82`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:86`).
   No retained candidate reopens REDRESS 96/97/98/102 without a material
   differential.

4. ACCEPT - Container dispatch no longer carries object/key/value-byte state.
   P2-A scopes C4 to a local current-pointer pair probe and rejects object
   next-key carry, value-byte compaction, retained next-byte state, generic JSON
   pair policy, and parse-only evidence
   (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:126`-
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:148`).
   P2-D's D1 permits only configured separator/close classification with no
   retained cursor, class column, side table, or object-key/value-byte carry
   (`restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md:31`,
   `restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md:43`).
   P2-F keeps object/key/value-byte carry pre-blocked while allowing generated
   FIRST-set/prefix-trie/lookahead control as a grammar-template surface
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:39`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:56`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:96`).
   The remaining shape is materially different from REDRESS 65 and REDRESS 84.

5. ACCEPT - Numeric, bitmap, cache-hint, and digest surfaces preserve the
   REDRESS firewall. Digit candidates reject mantissa/table-only routes, f64
   fallback rewrites, and parser-owned number policy
   (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:150`-
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:172`,
   `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:47`,
   `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:90`).
   PMULL and CSSC CTZ stay inventory/support-only and cannot become default hot
   bodies without a future material-differential packet
   (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:19`,
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:77`,
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:92`-
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:97`).
   Digest/hash is explicitly a benchmark/product sink or oracle, not parser
   vocabulary, and cache hints remain inventory absent fresh row-moving evidence
   (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:174`-
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:200`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:46`-
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:47`).

6. ACCEPT - Retained parse callers are guard and micro-proof surfaces only.
   P2-E states this directly and requires S-P3 admission to come from generated
   direct, typed, or generated non-JSON product-plane consumers with strict
   output parity (`restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:15`,
   `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:33`,
   `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:41`).
   P2-F adds the non-JSON generated benchmark requirement before any parser
   candidate can reach S-P3 (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:63`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:77`-
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:82`).
   This satisfies the REDRESS 102 parse-only firewall for S-P2.

## Required Redress

None for S-P2 V2. The V1 CH3 REVISE items have been folded into the V2
candidate boundaries.

Carry-forward constraints for S-P3:

1. Any x4 escape packet is proof-only unless it names a new source delta and a
   same-wave direct/typed/non-JSON product consumer beyond `unescape_string`.
2. Any string-block packet must be a new caller-level proof with scalar oracle,
   representative row gates, and no retained wide-scan/StringBlock16/tiny-parser
   route.
3. Any mask, dispatch, or tape-shape packet must reject retained class columns,
   structural-position vectors, streaming cursors, parser-owned projections,
   hidden sidecars, and parse-only row movement.
4. Any container dispatch packet must stay local/current-pointer or generated
   FIRST-set dispatch; object next-key carry and value-byte compaction remain
   pre-blocked.
5. Any numeric, PMULL, CTZ, cache-hint, or digest packet must preserve the
   explicit V2 inventory/oracle/product-sink boundaries and show a same-wave
   product consumer before row movement can count.

## Accepted Facts

- V2 is REDRESS-clean for S-P2 research purposes. No retained V2 candidate
  currently reopens a blocked route without naming a material differential.
- CH3 contributes ACCEPT to the S-P2 V2 challenge matrix.
