# SK-V15 S-P2 CH2 GENERALITY - V3 CONFIRMATION

Pass: S-P2 Research. Cycle: V3.
Lens: CH2 GENERALITY.
Scope: confirmation audit of the folded P2-A through P2-F packet.
Output: this file.

## Verdict

ACCEPT.

The V2 CH2 result remains valid for V3. No surviving primitive contract is
JSON-only; every non-rejected survivor is covered by P2-F as a grammar-neutral
byte-set/classifier, validator, segment stream, or same-tape operation; the
CSS L4 / Sheets / BBNF-self witness language is bounded; and the rejected
numeric/digit, EOB, and x86 rows remain excluded from the implementation
shortlist.

This is a confirmation audit, not pass convergence by itself. The orchestrator
still requires the cycle protocol and convergence rule from ORCHESTRATOR
Section 3Z: challenge dispatch, consolidation, folding discipline, and two
consecutive >=95% ACCEPT cycles with zero open critical defects or orphan
REVISEs (`restart/prompts/ORCHESTRATOR.md:104-121`). V2 was the first clean
cycle and explicitly required V3 confirmation before S-P3 dispatch
(`restart/skinny/tranches/sk-v15/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md:10-12`,
`restart/skinny/tranches/sk-v15/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md:39-41`).

## Controlling Rules

- CH2 asks whether Lock 14 holds: no grammar-name leak, and interventions must
  work for CSS L4 / Sheets / BBNF-self rather than JSON alone
  (`restart/prompts/ORCHESTRATOR.md:81-88`).
- S-P2 specializes that rule by requiring every candidate to carry a P2-F
  grammar-neutral verdict or be reframed as a per-grammar template surface
  (`restart/prompts/skinny/PASS-2-RESEARCH.md:48-53`,
  `restart/prompts/skinny/PASS-2-RESEARCH.md:102-107`).
- SK-V15 adds gate-exclusion discipline and keeps CSS admission honest: JSON is
  the durable proof-of-concept, CSS is not, and Lock 14 / Lock 16 gates must
  report their own exclusions (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:98-117`).
- Lock 14 forbids grammar-specific branches, hardcoded punctuation/role policy,
  and grammar-named policy in generic crates; primitive policy must come from
  generated grammar config, caller data, or opaque metadata, and fleet-wide
  transfer requires proper non-JSON witnesses or scoped wording
  (`restart/locks/LOCKS.md:368-387`, `restart/locks/LOCKS.md:392-400`).

## Findings

| ID | Disposition | Finding |
|---|---|---|
| CH2-V3-F1 | ACCEPT | P2-F covers the full current candidate universe. It declares P2-B/C/D/E closed for implementation candidates and maps P2-A's seven comparator-context aliases so no P2-A row escapes without a verdict (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:5-12`, `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:59-69`). The current candidate tables are present in P2-A, P2-B, P2-C, P2-D, and P2-E (`restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:44-54`, `restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:43-58`, `restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:24-35`, `restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:34-41`, `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:27-29`). |
| CH2-V3-F2 | ACCEPT | No survivor is JSON-only as a primitive contract. P2-F accepts only generated-policy byte sets/classifiers, UTF-8/string validators, and same-tape operations; revised rows are safe only after per-grammar/template reframing with generated policy ownership (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:85-102`). P2-A and P2-E say JSON-specific control, escape, whitespace, number, and API policy must remain caller/generated policy rather than generic primitive behavior (`restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:58-66`, `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:223-236`). |
| CH2-V3-F3 | ACCEPT | P2-F grammar-neutral treatment is present for every survivor. Accepted rows cover P2-B byte-class/mask/UTF-8/string/tape support, P2-C TBL4/movemask classifier support, P2-D same-tape capacity/flags/facts/mask writing, and P2-E byte-set, local classifier, bounded literal, UTF-8, and escaped-segment surfaces (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:28-57`). The P2-F cost fold also scopes non-REJECT survivors to scalar reference, parity, same-wave consumer, risk, and cap discipline (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:71-82`). |
| CH2-V3-F4 | ACCEPT | CSS L4 / Sheets / BBNF-self witness language is bounded. P2-F says these grammars are required witnesses, not automatic proof; CSS is only a future repaired positive target, while Sheets and BBNF-self witness generated-policy generality without generic-crate branches (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:22`). The witness tables name CSS, Sheets, and BBNF-self targets while preserving generated alphabet, encoding, and same-tape boundaries (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:87-91`, `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:95-102`). The caveats then bound CSS, Sheets, and BBNF-self use explicitly (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:117-121`). |
| CH2-V3-F5 | ACCEPT | Rejected numeric and digit rows remain excluded. P2-A rejects `raw_number_span_classify` for this cycle because the cited evidence is schema/comparator diagnostic rather than a surviving BBNF-side numeric hot leaf (`restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:52-64`). P2-C rejects A64 UDOT digit work for the same reason (`restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:31`, `restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:45`). P2-E marks `digit_run_span_accumulate` rejected for S-P2 candidate status (`restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:154-183`, `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:233`). P2-F preserves those rejections in the final reject set (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:108`). |
| CH2-V3-F6 | ACCEPT | EOB and x86 remain excluded. P2-B keeps `EOB_PAD_CLAMP` as existing support inventory only, not an S-P2 implementation candidate (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:51`), and P2-F repeats the EOB rejection (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:32`, `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:109`). SK-V15 admission evidence is native Apple M5 Max / aarch64 only, while x86 and AVX-512 rows are diagnostic signals (`restart/skinny/tranches/sk-v15/HANDOFF.md:13-18`, `restart/skinny/tranches/sk-v15/SYNTHESIS.md:36-44`). P2-C and P2-F both reject x86 as an implementation/admission path (`restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:35`, `restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:51`, `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:20`, `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:115`). |

## V2 Carry-Forward Check

V2 CH2 already accepted the folded packet, specifically finding complete P2-F
coverage, no JSON-only generic primitive policy, bounded CSS/Sheets/BBNF-self
witness language, and numeric/digit/EOB rejection (`restart/skinny/tranches/sk-v15/research/p2/hardening/V2/CH2.md:8-20`,
`restart/skinny/tranches/sk-v15/research/p2/hardening/V2/CH2.md:47-50`).
The V2 consolidated packet recorded 7/7 ACCEPT, no open REVISE/REJECT list,
and closed the V1 CH2 P2-A alias orphan (`restart/skinny/tranches/sk-v15/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md:14-28`,
`restart/skinny/tranches/sk-v15/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md:30-37`).

No new CH2 blocker is found in the current P2-A through P2-F packet. No orphan
V2 CH2 REVISE remains.
