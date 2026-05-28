# CH2 Generality - T-P2 V2 SK-V15

Lens: CH2 GENERALITY.
Disposition: ACCEPT.
Target packet: `restart/audit/totality/p2/2A-sota-landscape.md` through
`restart/audit/totality/p2/2F-parse-that-gaps.md`.

## Critical Findings

| id | severity | finding | evidence | convergence impact |
|---|---|---|---|---|
| CH2-V2-CRIT-00 | none | No CH2-critical generality defect found. V2 preserves the V1 accept surface: no new directive, no new BIR, no sixth `BackendShape`, grammar-neutral route shape, and no single-grammar trap disguised as totality. | See checks below. | Does not block T-P2 V2 convergence. |

## Generality Checks

| check | result | evidence |
|---|---|---|
| No new directive / no new BIR / no sixth `BackendShape` | Pass. V2 states the non-regression fence directly and uses future grammar onboarding as a prohibition, not a new surface. | `2C-grammar-neutrality.md:52`-`55`; `2C-grammar-neutrality.md:149`; `2D-cost-model.md:47`-`53`; `2D-cost-model.md:62`; `2D-cost-model.md:68`. |
| Grammar-neutral route shape | Pass. The admissible route is grammar source plus workspace metadata plus generated per-grammar runtime surfaces, with CSS plus Sheets or BBNF-self required before generic claims. | `2C-grammar-neutrality.md:35`-`50`; `2C-grammar-neutrality.md:61`-`75`; `2C-grammar-neutrality.md:144`; `2C-grammar-neutrality.md:149`. |
| No JSON-only trap | Pass. JSON SOTA rows are kept on workload planes, not promoted fleet-wide; JSON string/number primitives remain scoped unless a grammar-specific receiver and oracle exist. | `2A-sota-landscape.md:51`-`55`; `2A-sota-landscape.md:63`; `2F-parse-that-gaps.md:76`-`79`. |
| No CSS-only trap | Pass. CSS metadata, typed-provider, CSSOM, and comparator rows are either partial, gated, or refuted when they are CSS-only. CSS must be paired with Sheets or BBNF-self for future grammar proof. | `2C-grammar-neutrality.md:62`-`70`; `2C-grammar-neutrality.md:72`-`75`; `2F-parse-that-gaps.md:78`-`80`. |
| Primitive vocabulary generality | Pass. Primitive rows require abstract semantics, scalar oracle, strict parity/checkasm, M5 Max/aarch64 gate, same-wave consumer, and row movement; non-JSON receivers are required where a primitive is claimed generic. | `2B-primitive-vocabulary.md:61`-`68`; `2B-primitive-vocabulary.md:72`-`76`; `2B-primitive-vocabulary.md:194`; `2B-primitive-vocabulary.md:201`-`204`. |
| Host-arch generality and close-route boundary | Pass. 2E keeps Apple M5 Max/aarch64 as close route, labels x86 diagnostic-only, and blocks ISA/checkasm-only promotion without consumer movement. | `2E-host-arch-esoterica.md:73`-`82`. |
| BackendShape generality | Pass. 2D keeps exactly five shapes and requires implementation or gate-consumed rejection inside that set. It does not add a sixth shape to escape lowerer debt. | `2D-cost-model.md:47`-`53`; `2D-cost-model.md:62`; `2D-cost-model.md:75`-`76`; `2D-cost-model.md:85`; `2D-cost-model.md:117`. |
| Sidecar and retained-route pressure | Pass for CH2 scope. 2A and 2C fence retained sidecars and generator-sidecars; 2B blocks retained frame/open-stack vocabulary absent a new contract or same-substrate proof. | `2A-sota-landscape.md:51`-`52`; `2A-sota-landscape.md:87`; `2A-sota-landscape.md:110`; `2C-grammar-neutrality.md:69`; `2B-primitive-vocabulary.md:185`. |

## Evidence Inspected

- `restart/audit/totality/p2/hardening/V2/CHALLENGE-CONTEXT.md`
- `restart/prompts/totality/PASS-2-RESEARCH.md`
- `restart/prompts/ORCHESTRATOR.md`
- `restart/audit/totality/p2/T-P2-DISPATCH-CONTEXT.md`
- `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md`
- `restart/audit/totality/p2/hardening/HARDENING-T-P2-V1-CONSOLIDATED.md`
- `restart/audit/totality/p2/2A-sota-landscape.md`
- `restart/audit/totality/p2/2B-primitive-vocabulary.md`
- `restart/audit/totality/p2/2C-grammar-neutrality.md`
- `restart/audit/totality/p2/2D-cost-model.md`
- `restart/audit/totality/p2/2E-host-arch-esoterica.md`
- `restart/audit/totality/p2/2F-parse-that-gaps.md`
- Targeted packet scans for `directive`, `BIR`, `BackendShape`, `sixth`,
  `grammar-neutral`, `future grammar`, `Sheets`, `BBNF-self`, `broadcast`,
  `fact-stream`, `CSSOM`, `sidecar`, and V2 row-shape fields.

## Fold Requirements

None. Disposition is ACCEPT.

## Convergence Impact

CH2 does not block T-P2 V2 convergence. This file contributes an ACCEPT for the
generality lens, preserving the V1 CH2 accept while the V2 packet folds other
lenses' obligations. Per the V2 challenge context, even a fully clean V2 can
only be the first clean T-P2 hardening cycle; a second consecutive clean cycle
is still required before ordinary §3Z convergence unless the user pins G2.
