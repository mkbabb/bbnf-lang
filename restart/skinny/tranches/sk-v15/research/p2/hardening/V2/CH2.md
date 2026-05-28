# SK-V15 S-P2 CH2 GENERALITY - V2

Pass: S-P2 Research. Cycle: V2.
Lens: CH2 GENERALITY.
Scope: Lock 14 grammar-neutrality audit of current P2-A through P2-F.
Output: this file.

## Verdict

ACCEPT.

No CH2 blocker remains. The current packet gives every surviving S-P2
candidate a P2-F grammar-neutral verdict, folds P2-A's comparator-context
candidate wording into explicit aliases/dispositions, blocks JSON-only policy
from generic primitives, and keeps CSS L4 / Sheets / BBNF-self language scoped
to generated grammar policy or repaired future witnesses.

This is a CH2-only disposition. S-P2 still advances only under the orchestrator
cycle rule: two consecutive challenge cycles at >=95% ACCEPT, zero open critical
defects, and no orphan unresolved REVISE (`restart/prompts/ORCHESTRATOR.md:104`-`121`).

## Evidence

- The governing CH2 lens requires Lock 14 to hold with no grammar-name leak and
  no JSON-only intervention; S-P2 specializes that to requiring every candidate
  to carry a P2-F grammar-neutral verdict or be reframed as a per-grammar
  template surface (`restart/prompts/ORCHESTRATOR.md:81`-`88`;
  `restart/prompts/skinny/PASS-2-RESEARCH.md:102`-`107`).
- Current Lock 14 forbids grammar-specific branches, modules, public API types,
  feature flags, and hardcoded quote/escape/delimiter/number/string policy in
  generic crates; primitive policy must come from generated grammar config or
  caller data (`restart/locks/LOCKS.md:349`-`356`;
  `restart/locks/LOCKS.md:368`-`375`; `restart/locks/LOCKS.md:392`-`400`).
- SK-V15 keeps CSS admission reopened and requires Lock 14 / Lock 16 gates to
  scan their own exclusions; stale CSS rows cannot be used as current admission
  proof (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:34`-`50`;
  `restart/skinny/tranches/sk-v15/SYNTHESIS.md:98`-`110`).
- P2-F now states that P2-A is comparator context only and that its seven row
  names are explicitly mapped so no P2-A candidate wording escapes without a
  verdict (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:5`;
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:12`).

## Findings

| ID | Disposition | Finding |
|---|---|---|
| CH2-V2-F1 | ACCEPT | Every current candidate surface has a P2-F verdict. P2-B rows are covered from `BYTE_CLASS_FROM_TABLE_64` through schema/harness non-candidates, P2-C from A64 classifier inventory through x86 diagnostic exclusion, P2-D across same-tape capacity/flags/facts/mask writing, and P2-E across the six parse-that gaps (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:43`-`58`; `restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:24`-`35`; `restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:34`-`41`; `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:31`-`221`). P2-F assigns ACCEPT / REVISE / REJECT verdicts to those surfaces, and its P2-A alias table covers all seven P2-A row names (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:26`-`57`; `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:59`-`69`). |
| CH2-V2-F2 | ACCEPT | The surviving primitives do not smuggle JSON-only policy into generic APIs. Accepted forms are byte-set/classifier, encoding/string validator, and same-tape operations with generated grammar alphabets or opaque tape policy; revised forms are forced into generated string, escape, FIRST-set, or same-tape templates (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:85`-`102`). P2-E states the same rule for parse-that/bbnf-simd APIs and rejects JSON-only wording without a non-JSON consumer, negative-control witness, or scoped claim (`restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:223`-`236`). |
| CH2-V2-F3 | ACCEPT | CSS L4 / Sheets / BBNF-self generalisation language is scoped correctly. P2-F treats those grammars as required witnesses, not automatic proof, and keeps CSS positive proof behind provider/comparator repair (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:22`; `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:117`-`121`). The witness table names concrete CSS delimiter/string/fact targets, Sheets formula/string/tape targets, and BBNF-self punctuation/literal/fact targets while preserving the generated-policy boundary (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:87`-`91`). |
| CH2-V2-F4 | ACCEPT | Rejected numeric, digit, and EOB rows are not accidentally admitted. P2-F rejects `EOB_PAD_CLAMP` as support inventory, rejects A64 UDOT digit work and PTG digit-run work as current implementation candidates, rejects P2-A `raw_number_span_classify`, and repeats the numeric/digit plus EOB rejection in the REJECT set (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:32`; `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:43`; `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:56`; `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:67`; `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:104`-`109`). P2-E also marks `digit_run_span_accumulate` rejected for this cycle because no surviving BBNF-side numeric P1 hot leaf exists (`restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:154`-`183`). |
| CH2-V2-F5 | ACCEPT | Lock 14 policy ownership is preserved for tape/fact work. P2-D requires generic tape to store opaque bits/facts while generated grammar code interprets JSON, CSS, Sheets, or BBNF-self meanings, and rejects retained structural-position vectors, streaming cursors, class columns, whitespace bitmaps, density tables, decoded-byte sidecars, and public `UnionTape` shapes (`restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:43`-`58`). |

## Orphan V1 Disposition Check

| V1 issue | V2 disposition | Evidence |
|---|---|---|
| P2-A had a formal candidate table while P2-F originally scoped verdicts to P2-B/C/D/E. | Closed. P2-F now declares explicit alias disposition for P2-A comparator-context rows and maps all seven names. | `restart/skinny/tranches/sk-v15/research/p2/hardening/V1/CH2.md:19`; `restart/skinny/tranches/sk-v15/research/p2/hardening/V1/CH2.md:24`-`36`; `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:5`; `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:59`-`69`. |
| V1 CH2 suggested `raw_number_span_classify` could be revised into digit-run / bounded accumulation language. | Tightened. Current P2-F rejects the P2-A raw-number row and rejects numeric/digit surfaces as current S-P2 implementation candidates. | `restart/skinny/tranches/sk-v15/research/p2/hardening/V1/CH2.md:34`; `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:67`; `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:108`. |
| V1 consolidated hardening kept numeric/digit and EOB as open fold surfaces. | Closed for CH2. Numeric/digit rows and `EOB_PAD_CLAMP` are explicitly rejected or demoted to non-shortlist support inventory; no ACCEPT table row admits them as implementation candidates. | `restart/skinny/tranches/sk-v15/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md:21`-`24`; `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:32`; `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:43`; `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:56`; `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:104`-`109`. |

No orphan V1 CH2 REVISE remains.
