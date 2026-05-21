# T-P2 V3 CH6 Anti-Paper-Close

Pass: T-P2 Research.
Cycle: V3.
Lens: CH6 anti-paper-close.
Date: 2026-05-21.
Scope: anti-paper-close only: source pins/counts, microbench rows, ledger
entries, future equality/consumer/row gates, and G-Omega discipline.

## Verdict

ACCEPT.

V3 preserves the anti-paper-close boundary. The V3 fold repairs CH1/CH4
mechanics by pinning source authority, making `primary_sources_cited`
reproducible, centralizing executable ledger rows, normalizing admission states,
and adding numeric abrogate caps. Those repairs are not treated as admissions.
Across 2A-2F, source density, source presence, microbench wins, parity-only
proofs, and ledger presence remain below admission until strict equality or an
independent oracle, a same-wave production consumer, row movement or measured
rejection, and Lock/G-Omega discipline are gate-consumed.

## Findings

| disposition | target | finding |
|---|---|---|
| ACCEPT | `T-P2-V3-FOLD-ADDENDUM.md:12-20`, `:30-54`; `2A-sota-landscape.md:43-52`; `2D-cost-model.md:28-38` | Source pins and source counts are provenance machinery, not close evidence. The addendum repairs generic-root citations and counted IDs, while 2A and 2D explicitly state that citations ground candidates only and do not admit routes. |
| ACCEPT | `T-P2-V3-FOLD-ADDENDUM.md:56-101`; `2B-primitive-vocabulary.md:126-150`; `2E-host-arch-esoterica.md:109-148`; `2F-parse-that-gaps.md:188-227` | Ledger rows are not admissions by existence. V3 reserves `admissibility_state` for the state machine, moves `conditional`, `inventory`, `ADMITTED-EVIDENCE`, and `NOT-VALIDATED` into blocker/disposition vocabulary, and keeps owner tables subordinate to the shared executable ledger. |
| ACCEPT | `2A-sota-landscape.md:99-104`, `:110-128`; `2B-primitive-vocabulary.md:71-78`, `:134-154`; `2E-host-arch-esoterica.md:31-41`, `:87-90`, `:207-210`; `2F-parse-that-gaps.md:102-108`, `:155-158` | Microbench rows, parity repairs, local path presence, and ISA availability stay non-admitting. ASCII run-skip is still `micro_proven`, escape masks are prerequisite-only, parse-that import authority is conditional, and PMULL/CSSC/UDOT/TBL labels require row-local consumers and material differentials. |
| ACCEPT | `T-P2-V3-FOLD-ADDENDUM.md:89-115`; `2B-primitive-vocabulary.md:36-44`, `:150-154`; `2C-grammar-neutrality.md:84-87`, `:100-126`, `:161-166`; `2D-cost-model.md:73-90`, `:143-149`; `2F-parse-that-gaps.md:171-184`, `:221-227` | Future gates remain strict and measured. Candidate rows name scalar/parity prerequisites, strict equality or oracle gates, same-wave consumers, expected row movement or measured rejection, rollback, and fail-closed abrogate thresholds. No support-only landing is allowed. |
| ACCEPT | `PASS-2-RESEARCH.md:202-207`; `2D-cost-model.md:73`, `:103-108`, `:148-160`; `2F-parse-that-gaps.md:171-184`, `:232-239` | G-Omega discipline holds. V3 rejects new directives, BIR variants, `BackendShape` variants, second tapes, retained sidecars, public substrate APIs, and parse-that import expansion unless the lock/user amendment path explicitly authorizes the change. |
| ACCEPT | `2C-grammar-neutrality.md:113-126`; `T-P2-V3-FOLD-ADDENDUM.md:71-82` | The only apparent admission-looking label, `ADMITTED-EVIDENCE`, is correctly scoped as historical row evidence. 2C marks future CSS, Sheets, and BBNF-self witnesses `NOT-VALIDATED`, and the V3 addendum states those labels are not admission-state values. |

## Required Repairs

None for CH6.

Carry-forward requirement: future folds must preserve this boundary. Counted
source IDs remain audit metadata; `source_backed`, `checkasm_backed`, and
`micro_proven` remain non-admitting; ledger rows must name strict equality or an
independent oracle, the same-wave production consumer, and row movement,
measured rejection, or architectural block before any route can advance.
