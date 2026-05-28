# SK-V15 S-P2 V2 CH3 REGRESSION

Pass: S-P2 Research. Cycle: V2.
Agent: S-P2 CH3 REGRESSION.
Scope: current `p2a`-`p2f`, V1 CH3, V1 consolidation, `SYNTHESIS.md` §0.5, and `skinny/REDRESS.md`.
Disposition vocabulary: ACCEPT / REVISE / REJECT.

## Verdict

Overall CH3 verdict: ACCEPT.

No surviving V2 S-P2 candidate silently reopens the named REDRESS-blocked routes. The V2 packet keeps the surviving surfaces local, same-call, same-tape, grammar-owned, scalar-first, parity-gated, or explicitly REVISE-framed. The old active routes remain blocked: REDRESS 28/33 tiny-string/Class A wiring; REDRESS 50-55 UTF-8 fusion, aux side tables, event cursors, parser-local structural cursors, decoded stats, and quote-source materializers; REDRESS 60-72 retained string shortcuts and direct materializer families; REDRESS 80 mantissa-widen/f64 fallback; REDRESS 82-84 one-quartet unicode, StringBlock16, and object-pair compaction; REDRESS 88 PMULL; REDRESS 89 CSSC CTZ/bulk emit; retained union-substrate routes; CSS broadcast admits; and self-exempting gate-exclusion closes.

## Evidence

The governing lens says CH3 rejects any candidate that reopens REDRESS 28+33, 50-55, 60-72, 80, 82-84, 88, 89, or historical blocked routes without fresh P1 evidence and new framing (`restart/prompts/skinny/PASS-2-RESEARCH.md:109`). SK-V15 §0.5 adds broadcast-admission detection and gate-exclusion detection: N CSS admits need N distinct measurements unless explicitly aggregate, and Lock 14 / Lock 16 gates must report their own exclusion lists (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:107`, `:109`, `:121`-`:126`).

V2's effective candidate ledger is P2-F. It closes over P2-B/C/D/E, maps P2-A as comparator context only, and says no P2-A wording escapes without a verdict (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:12`, `:59`-`:69`). It also records CSS L4 as demoted/contrived and not proof of admission until prune/rebuild repairs the provider and comparator plane (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:14`, `:22`, `:117`, `:141`).

## Findings

| ID | Surface | Disposition | Finding |
|---|---|---|---|
| CH3-V2-1 | Tiny-string, string block, and local scanner replay | ACCEPT | REDRESS 28 and 33 reject active 16-byte tiny-string/Class A wiring after parity-green code regressed `twitter` and missed the real boundary (`skinny/REDRESS.md:324`, `:394`). V2 keeps `scan_string_event_64`, long-string scanner, and `bounded_plain_literal_span` as parameterized scalar-first, same-call helpers and explicitly rejects retained StringBlock/tiny-probe replay (`restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:50`, `:70`; `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:36`, `:54`, `:77`, `:112`, `:125`). |
| CH3-V2-2 | UTF-8 fusion and decoded-string materialization | ACCEPT | REDRESS 54/55 and 66-69 reject decoded stats, quote-source streaming materializers, parser-owned scratch, byte-output materializers, and semantic fact hashing (`skinny/REDRESS.md:815`, `:846`, `:1688`, `:1736`, `:1789`, `:1839`). V2 keeps `decode_escape_run`, `unescape_uxxxx_x4`, unicode batch, `escape_mask_64`, and `escaped_literal_segments` in BLOCKED/REVISE framing until a materially different scalar primitive plus same-wave consumer exists (`restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:51`, `:74`; `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:33`, `:35`, `:44`, `:57`, `:79`, `:129`). |
| CH3-V2-3 | Retained cursors, sidecars, second scanners, and union substrate | ACCEPT | REDRESS 50/51/53 and 96/97/98 block aux side tables, byte-class cursors, parser-local structural cursors, class columns, streaming structural cursors, and union-substrate replay (`skinny/REDRESS.md:715`, `:742`, `:784`, `:2800`, `:2852`, `:2910`). V2 candidates require transient masks or writes into the existing tape/sink, with retained sidecars, cross-call carry, public `UnionTape`, and second tapes rejected (`restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:14`, `:36`, `:76`; `restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:38`-`:41`, `:52`-`:58`; `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:18`, `:51`, `:80`, `:113`, `:139`). |
| CH3-V2-4 | PMULL and CSSC instruction availability | ACCEPT | REDRESS 88 and 89 prove correctness/checkasm is insufficient: PMULL prefix-XOR and CSSC CTZ/bulk emit regressed production parse rows (`skinny/REDRESS.md:2510`, `:2544`). V2 accepts only scalar/local mask algebra and rejects PMULL hot-body promotion and CSSC bulk-consumer promotion for SK-V15 shortlist (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:49`-`:50`; `restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:33`-`:34`, `:49`; `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:30`-`:31`, `:45`-`:46`, `:110`-`:111`, `:135`-`:137`). |
| CH3-V2-5 | Numeric rows and mantissa/fallback routes | ACCEPT | REDRESS 80 rejects the `canada` mantissa-widen/f64 fallback route after the measured fallback pool was zero (`skinny/REDRESS.md:2215`). V2 rejects `raw_number_span_classify`, A64 `UDOT`, and `digit_run_span_accumulate` as current S-P2 implementation candidates because `mesh` and decimal evidence is schema/comparator diagnostic, not a BBNF-side hot leaf (`restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:52`, `:64`; `restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:31`, `:45`; `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:154`, `:183`; `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:43`, `:56`, `:108`, `:131`). |
| CH3-V2-6 | Diagnostic/rejected rows | ACCEPT | V2 keeps rejected or diagnostic rows blocked: `EOB_PAD_CLAMP` is support inventory only (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:32`, `:109`); schema-shaped generated product builders and harness hashes are rejected (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:58`; `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:39`, `:114`); x86/AVX routes are diagnostic only and not SK-V15 admission anchors (`restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:35`, `:59`; `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:47`, `:115`). |
| CH3-V2-7 | CSS broadcast and stale CSS proof | ACCEPT | SK-V15 demotes CSS L4 because prior CSS admits included broadcast and parser-truth problems (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:39`-`:44`, `:62`-`:66`). V2 does not use CSS rows as admission proof: P2-D says CSS broadcast rows cannot prove tape candidates (`restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:55`), and P2-F uses CSS only as a future repaired generalisation witness after provider/comparator rebuild (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:22`, `:85`-`:91`, `:117`, `:141`). |
| CH3-V2-8 | Gate-exclusion reopen risk | ACCEPT | No V2 candidate relies on a Lock 14 / Lock 16 self-exempting gate report or hidden exclusion list to claim admission. The packet instead phrases future gates as requirements: generated grammar owns policy, generic crates cannot branch on grammar names, parity/equality gates are required before wiring, and same-wave consumers must move or reject rows (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:16`, `:71`-`:80`, `:83`-`:102`). This preserves §0.5's gate-exclusion rule rather than bypassing it. |

## Orphan V1 Disposition Check

V1 CH3 was ACCEPT, and V1 consolidation's non-CH3 REVISE/REJECT surfaces have no orphan CH3 regression left in V2.

| V1 fold item | V2 status |
|---|---|
| Reject numeric/digit rows until a current BBNF-side numeric hot leaf exists. | Folded: `raw_number_span_classify`, A64 `UDOT`, and `digit_run_span_accumulate` are rejected or diagnostic only. |
| Demote `EOB_PAD_CLAMP` from candidate to existing support inventory. | Folded in P2-B/P2-F; no S-P3 shortlist credit remains. |
| Add P2-A alias/disposition bridge. | Folded in P2-F §2.1; comparator-context names are ACCEPT-alias, REVISE, or REJECT, not silent candidates. |
| Add cost/orphan fields for non-REJECT survivors. | Folded in P2-F §2.2 with scalar reference, parity gate, same-wave consumer, LOC budget, risk class, wave alignment, and hard cap. |
| Tighten `offset_tape_capacity_policy_v2` against second scans. | Folded in P2-D: no second source scan, pre-scan capacity oracle, sidecar capacity plane, or parallel source pass is permitted. |
| Preserve CSS broadcast and gate-exclusion addenda. | Folded as a CH3-safe negative: CSS is not proof until repaired; no candidate claims admission through hidden gate exclusions. |

## Overall CH3 Verdict

ACCEPT. S-P2 V2 may advance from the CH3 regression lens. The handoff to S-P3 must preserve the current guardrails: local masks only, no retained sidecars or cross-call classifier state, no PMULL/CSSC hot-body promotion, no numeric/fallback reopen without fresh P1 evidence, no CSS broadcast proof, and no self-exempting Lock 14 / Lock 16 gate exclusions.
