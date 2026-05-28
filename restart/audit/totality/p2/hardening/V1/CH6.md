# CH6 - ANTI-PAPER-CLOSE

Disposition: REVISE

T-P2 V1 is mostly hostile to paper-close: I found no dossier relying on the words
"validated" or "proven" as a substitute for evidence, and the dossiers repeatedly
refute broadcast timing, source inventory, x86 diagnostic evidence, and scaffold
code as close evidence. The V1 gap is structural: several grounded rows still
present source density plus a bbnf-specific note, while the admission gate and
verification action are supplied elsewhere or not supplied per grounded item.
That is a REVISE because it can be folded without discarding the dossiers, but it
blocks T-P2 V1 convergence until folded.

## Critical Findings

| id | severity | finding | required disposition |
|---|---|---|---|
| CH6-V1-01 | REVISE | The V1 dossier schema lets grounded rows stop at `published source cited` + `bbnf-specific note`, while CH6 requires every grounded technique to state bbnf-specific transfer, admission gate, and verification action. This affects 2A's grounded SOTA rows (`restart/audit/totality/p2/2A-sota-landscape.md:46`-`55`), 2C's grounded transfer rows (`restart/audit/totality/p2/2C-grammar-neutrality.md:52`-`68`), 2D's grounded technique rows (`restart/audit/totality/p2/2D-cost-model.md:54`-`64`), and 2F's primitive-gap rows (`restart/audit/totality/p2/2F-parse-that-gaps.md:69`-`78`). | Fold a uniform per-grounded-row shape into V2: `transfer_reason`, `admission_gate`, `verification_action`, and `close_status`. |
| CH6-V1-02 | REVISE | 2A integrates sources well, but several `grounded` SOTA rows are not independently admissible as written. simdjson, sonic-rs, yyjson, cssparser, and lightningcss rows name transfer boundaries (`restart/audit/totality/p2/2A-sota-landscape.md:48`-`55`), while admission gates are deferred to later LAC/OQ rows (`restart/audit/totality/p2/2A-sota-landscape.md:94`-`108`). That invites citation-density close if a consumer cites only the grounding table. | Inline the gate on each grounded SOTA row: comparator plane, required row-local equality/timing, host route, and rejection if only architectural/literature evidence exists. |
| CH6-V1-03 | REVISE | 2C correctly says Lock 14 transfer must be operational (`restart/audit/totality/p2/2C-grammar-neutrality.md:44`-`48`), but some grounded rows still name a transfer target without a row-local admission gate: CSS syntax/token surface (`restart/audit/totality/p2/2C-grammar-neutrality.md:56`), custom properties/calc (`restart/audit/totality/p2/2C-grammar-neutrality.md:58`), selectors (`restart/audit/totality/p2/2C-grammar-neutrality.md:59`), Sheets (`restart/audit/totality/p2/2C-grammar-neutrality.md:65`), and BBNF-self (`restart/audit/totality/p2/2C-grammar-neutrality.md:66`). | Add per-row verification actions: generated-facts diff, generic-code no-change gate, same-workload comparator/equality where applicable, and non-JSON receiver proof. |
| CH6-V1-04 | REVISE | 2F uses strong anti-paper-close prose, but PTG-2F-03 and PTG-2F-04 mark primitives as grounded/partial while the row text mainly says to vendor/wire kernels and names current partial code (`restart/audit/totality/p2/2F-parse-that-gaps.md:73`-`74`). The actual verification pattern appears later in open questions and LAC rows (`restart/audit/totality/p2/2F-parse-that-gaps.md:107`-`120`). | Put owner, scalar oracle, checkasm/parity command, hardware gate, same-wave consumer, and row-local movement check directly on each grounded primitive-gap row. |
| CH6-V1-05 | ACCEPT | 2B and 2E are the strongest CH6-compliant dossiers. 2B states the five-cell admission manifest (`restart/audit/totality/p2/2B-primitive-vocabulary.md:90`-`100`) and rejects citation-only LD4 admission (`restart/audit/totality/p2/2B-primitive-vocabulary.md:64`-`67`). 2E's table carries abstract primitive, hardware gate, scalar/checkasm plan, and same-wave consumer columns (`restart/audit/totality/p2/2E-host-arch-esoterica.md:67`-`78`). | Preserve these patterns as the V2 row template for the other dossiers. |

## Evidence Inspected

- Required authority and CH6 criteria: `restart/audit/totality/p2/hardening/V1/CHALLENGE-CONTEXT.md:49`-`56`, `restart/prompts/totality/PASS-2-RESEARCH.md:127`-`131`, `restart/prompts/ORCHESTRATOR.md:81`-`88`, `restart/prompts/ORCHESTRATOR.md:110`-`123`.
- Dispatch obligations: citation density admits nothing; deep SIMD needs scalar reference, differential check, hardware gate, same-wave consumer, and row movement (`restart/audit/totality/p2/T-P2-DISPATCH-CONTEXT.md:78`-`103`).
- All six dossiers: `restart/audit/totality/p2/2A-sota-landscape.md`, `restart/audit/totality/p2/2B-primitive-vocabulary.md`, `restart/audit/totality/p2/2C-grammar-neutrality.md`, `restart/audit/totality/p2/2D-cost-model.md`, `restart/audit/totality/p2/2E-host-arch-esoterica.md`, `restart/audit/totality/p2/2F-parse-that-gaps.md`.

## Fold Requirements

1. In V2, every row whose state includes `grounded` must carry four explicit fields: bbnf-specific transfer reason, admission gate, verification action, and close status (`admissible`, `diagnostic`, `partial`, `blocked`, or `refuted`).
2. Do not let source count or source register length count as validation. If a row cites multiple papers/posts/source files but lacks a concrete bbnf gate, mark it `diagnostic`, `partial`, or `blocked`, not `grounded` alone.
3. Move verification requirements out of distant OQ/LAC sections when they are prerequisites for the grounded row itself. OQ/LAC may repeat or generalize the gate, but the row must stand alone.
4. Keep 2B/2E's primitive-manifest style as the template for SIMD/ASM rows: scalar oracle, strict checkasm/parity, hardware gate, same-wave consumer, and row-local movement.

## Convergence Block

Blocks T-P2 V1 convergence: yes. This is not a REJECT because the dossiers
mostly identify paper-close risks and contain the needed gates somewhere, but V1
cannot converge while grounded rows can be cited without their admission and
verification requirements attached.
