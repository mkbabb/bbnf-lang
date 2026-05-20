# SK-V11 S-P2 Hardening V1 Consolidated

Pass: S-P2 Research.
Cycle: V1.
Date: 2026-05-19.
Status: REVISE.

## Disposition Matrix

| Lens | Disposition | Accept contribution | Blocking reason |
|---|---|---:|---|
| CH1 Correctness | REVISE | 0 | P2-F carried `lazy-tape/direct consumer shape` as a non-S-P1 antecedent; P2-C over-traced cache hints to `output_digest_hash`; citation hygiene needs tightening. |
| CH2 Generality | REVISE | 0 | Fixed `uXXXX` production, byte-set/FIRST-set dispatch, layout trivia, and output digest/hash need Lock 14 reframing. |
| CH3 Regression | REVISE | 0 | x4 escape production, widened string-block, object/key/value-byte carry, retained parse consumers, and W3-adjacent wording risk REDRESS reopenings. |
| CH4 Cost | REVISE | 0 | Candidate rows do not uniformly carry scalar-ref, strict parity, micro-proof, same-wave consumer, feature/fallback, and reject-boundary fields. |
| CH5 Hidden Coupling | REVISE | 0 | Retained structural-position/class-lane language, D5 tape-flag ambiguity, output-plane declarations, and generic-crate policy boundaries need hardening. |
| CH6 Anti-Paper-Close | REVISE | 0 | x4 escape production is paper-close without a new source delta; digest/hash and several P2-A/P2-C candidates are not S-P3-actionable. |

Accept rate: `0/6 = 0%`.

V1 does not satisfy the S-P2 convergence rule. No V1 candidate fact is accepted
for S-P3 shortlist use until the required folds land and a later CHALLENGE
cycle accepts them.

## Blocking Themes

1. **Candidate-pool hygiene.** V2 must distinguish candidate primitives from
   comparator pressure, support-only primitives, Lock 1 accounting, benchmark
   oracles, and ISA inventory. Every retained candidate must trace to one of
   the eight accepted S-P1 hot leaves:
   `bounded_plain_string_scan`, `string_escape_decode`,
   `unicode_escape_hex_decode`, `number_digit_span`,
   `ascii_whitespace_skip`, `container_dispatch`, `simd_movemask`, and
   `output_digest_hash`.
2. **Lock 14 reframing.** Fixed JSON `uXXXX` production becomes neutral
   hex-nibble/hex-run decode plus generated per-grammar escape policy. Byte-set
   masks stay separate from generated FIRST-set/prefix-trie dispatch. Byte-set
   whitespace skip stays separate from comment-aware layout trivia. Digest/hash
   is a benchmark/oracle or per-product host sink, not a generic parser
   primitive.
3. **REDRESS firewall.** V2 must hard-block x4 escape production that only
   reuses the already-consuming `unescape_string` caller; widened/string-block
   routes that inherit REDRESS 61/62/83/106; object/key/value-byte carry beyond
   the narrow REDRESS 63 array carry; retained structural-position/class-lane,
   streaming cursor, W3 union, parse-only, and sidecar routes.
4. **Cost and proof completeness.** Every retained candidate row needs the
   CH4 tuple: scalar-reference status, strict checkasm/parity expectation or
   N/A with product parity, micro-prove-first status, same-wave consumer,
   feature-gate/fallback plan, and reject boundary. For direct/typed/non-JSON
   rows, also name the generated Track 1 path, independent Track 2 or oracle,
   same-output proof, and no hidden shared sidecar.
5. **Citation and evidence hygiene.** P2-A's yyjson SIMD wording must be
   sourced or softened. P2-B/CH process citations must be pinned or marked as
   moving references. P2-C's non-official ISA mirrors must be replaced by
   official Arm references for admission claims, or demoted to inventory-only
   orientation. P2-F line ranges must include every sibling candidate it maps.

## Required V2 Fold Map

| Artefact | Required fold |
|---|---|
| P2-A | Reclassify comparator-derived ideas into candidate, support-only, or pressure. Add scalar-reference sketch, exact same-wave consumer/proof shape, and reject boundary to each retained candidate. Soften or source yyjson "no explicit SIMD". Delete retained class-column/structural-position allowances. |
| P2-B | Rename `ESCAPE_UXXXX_X4_PRODUCTION` to proof-only hex decode or to a new segment/caller candidate with a real source delta. Add concrete threshold/corpus/no-regression wording to string-block caller proof. Keep digest/hash as process/oracle unless a real consumer packet exists. |
| P2-C | Demote PRFM/STNP/cache hints to inventory unless fresh P1 behavior evidence ties them to a row-moving hot leaf. Fold P2-B's strict x4/checkasm caveat into the escape row. Add same-wave consumer, fallback, and reject-boundary fields for TBL, UDOT, string, whitespace, and support rows. |
| P2-D | Preserve "substrate union holds" only as the existing offset tape plus direct/typed consumer union. Add per-candidate output-plane declarations and scalar-output parity/micro-proof/reject-boundary fields. Constrain D5 to an internal sparse-flag encoding replacement with no new facts or direct-row claim. |
| P2-E | Keep only parse-that candidate gaps with concrete scalar references and product consumers. Remove retained-parse consumers as admission surfaces or mark them as guards/micro-proofs only. Add cost thresholds, row sets, strict-mode commands, and fallback boundaries. |
| P2-F | Remove C9 from the hot-leaf candidate pool or reclassify it as Lock 1 accounting. Split C1/C6 and C5 policy boundaries. Move C8 out of parser primitives. Add a proof-surface table for CSS L4, Sheets, and BBNF-self with line citations and cell verdicts (`grammar-neutral`, `per-grammar template`, `host function`, `benchmark oracle only`, or `reject`). |

## Advancement

S-P2 remains in progress. The orchestrator must fold V1 into a V2 research
cohort before dispatching CHALLENGE V2. A V2 fold that merely edits the
consolidation without changing the candidate artefacts is paper-hardening and
does not advance.
