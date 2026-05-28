# CH2 Generality - T-P2 V3 SK-V15

Lens: CH2 GENERALITY.
Disposition: ACCEPT.
Target packet: `restart/audit/totality/p2/2A-sota-landscape.md` through
`restart/audit/totality/p2/2F-parse-that-gaps.md`.

## Critical Findings

| id | severity | finding | evidence | convergence impact |
|---|---|---|---|---|
| CH2-V3-CRIT-00 | none | No CH2-critical generality defect found. The V3 packet preserves the V2 accept surface: no new directive, no new BIR, no sixth `BackendShape`, no admitted grammar switch, and no single-grammar route disguised as totality. | See checks below. | Does not block T-P2 V3 convergence. |

## Generality Checks

| check | result | evidence |
|---|---|---|
| No new directive / no new BIR / no sixth `BackendShape` | Pass. The live grammar-neutrality dossier carries the non-regression fence directly, and future grammar onboarding forbids generic branches, new directives, new BIR variants, and a sixth shape. The cost-model dossier keeps exactly five shapes and requires an all-five guard that fails on shape expansion. | `2C-grammar-neutrality.md:52`-`54`; `2C-grammar-neutrality.md:73`; `2C-grammar-neutrality.md:75`; `2C-grammar-neutrality.md:149`; `2D-cost-model.md:47`-`53`; `2D-cost-model.md:62`; `2D-cost-model.md:85`; `2D-cost-model.md:117`. |
| No grammar switch admitted | Pass. `RuntimeGenerationMode`, `runtime_profiles()`, and CSS profile match arms are named as the generic-crate grammar switch and are refuted, not admitted. The replacement route is a generated provider manifest plus Sheets or BBNF-self without hand-edited generic owner paths. | `2C-grammar-neutrality.md:70`; `2C-grammar-neutrality.md:126`; `2C-grammar-neutrality.md:134`; `2C-grammar-neutrality.md:144`; `2C-grammar-neutrality.md:149`. |
| No single-grammar route disguised as totality | Pass. 2C explicitly refutes JSON-only, CSS-only, generator-sidecar, and grammar-switch routes as fleet-wide generalization. CSS metadata remains partial until paired with Sheets or BBNF-self. | `2C-grammar-neutrality.md:44`-`51`; `2C-grammar-neutrality.md:62`; `2C-grammar-neutrality.md:72`; `2C-grammar-neutrality.md:73`; `2C-grammar-neutrality.md:75`; `2C-grammar-neutrality.md:121`-`122`. |
| JSON workload boundaries | Pass. JSON SOTA rows remain workload-plane bounded. JSON string/number primitives are partial for grammar-neutral transfer and cannot move CSS rows until CSS-specific scalar/provider semantics exist. | `2A-sota-landscape.md:51`-`55`; `2A-sota-landscape.md:63`; `2A-sota-landscape.md:67`; `2A-sota-landscape.md:91`; `2F-parse-that-gaps.md:76`; `2F-parse-that-gaps.md:121`. |
| CSS workload boundaries | Pass. CSS broadcast, fact-stream, four-counter, CSSOM/value substitution, and current Track 1 lightningcss admission are refuted or diagnostic. CSS typed provider work is scoped to generated CSS output plus same-workload comparator proof, not a generic proof by CSS alone. | `2A-sota-landscape.md:38`-`45`; `2A-sota-landscape.md:56`-`62`; `2A-sota-landscape.md:79`-`85`; `2C-grammar-neutrality.md:64`-`69`; `2C-grammar-neutrality.md:123`-`124`; `2F-parse-that-gaps.md:78`; `2F-parse-that-gaps.md:92`; `2F-parse-that-gaps.md:103`. |
| Primitive vocabulary generality | Pass. Primitive rows require scalar oracle, strict parity/checkasm, Apple M5 Max/aarch64 gate or scalar-delegate disclosure, same-wave consumer, and row movement; generic primitives require non-JSON receivers before admission. Source inventory and retained frame/open stack routes remain blocked. | `2B-primitive-vocabulary.md:62`-`70`; `2B-primitive-vocabulary.md:144`-`155`; `2B-primitive-vocabulary.md:180`-`185`; `2B-primitive-vocabulary.md:194`; `2E-host-arch-esoterica.md:73`-`82`; `2E-host-arch-esoterica.md:86`-`96`. |
| Host and BackendShape generality | Pass. 2E keeps aarch64 as the only close route and keeps x86/AVX-512 diagnostic. 2D keeps W7/W8/W9 bounded inside the five-shape canon and blocks CollapsedStage admission without aarch64 scalar/parity/hardware/consumer/movement evidence. | `2E-host-arch-esoterica.md:25`-`39`; `2E-host-arch-esoterica.md:86`-`89`; `2E-host-arch-esoterica.md:121`-`124`; `2D-cost-model.md:65`-`68`; `2D-cost-model.md:74`-`76`; `2D-cost-model.md:97`; `2D-cost-model.md:118`. |
| Runtime regex / substrate boundary | Pass for CH2 scope. 2F keeps runtime regex import blocked unless a generated-runtime consumer exists and CH3/CH5 review passes; regex/HIR facts remain local analysis/provider inputs, not a new runtime substrate or grammar switch. | `2F-parse-that-gaps.md:35`-`42`; `2F-parse-that-gaps.md:73`-`74`; `2F-parse-that-gaps.md:84`; `2F-parse-that-gaps.md:98`-`100`; `2F-parse-that-gaps.md:119`-`120`. |

## Evidence Inspected

- `restart/audit/totality/p2/hardening/V3/CHALLENGE-CONTEXT.md`
- `restart/audit/totality/p2/hardening/HARDENING-T-P2-V2-CONSOLIDATED.md`
- `restart/prompts/totality/PASS-2-RESEARCH.md`
- `restart/prompts/ORCHESTRATOR.md`
- `restart/audit/totality/p2/T-P2-DISPATCH-CONTEXT.md`
- `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md`
- `restart/audit/totality/p2/2A-sota-landscape.md`
- `restart/audit/totality/p2/2B-primitive-vocabulary.md`
- `restart/audit/totality/p2/2C-grammar-neutrality.md`
- `restart/audit/totality/p2/2D-cost-model.md`
- `restart/audit/totality/p2/2E-host-arch-esoterica.md`
- `restart/audit/totality/p2/2F-parse-that-gaps.md`
- Prior CH2 outputs: `restart/audit/totality/p2/hardening/V1/CH2.md` and
  `restart/audit/totality/p2/hardening/V2/CH2.md`.
- Targeted packet scans for `directive`, `BIR`, `BackendShape`, `sixth`,
  `grammar switch`, `RuntimeGenerationMode`, `runtime_profiles`,
  `generic-crate`, `single-grammar`, `JSON-only`, `CSS-only`,
  `grammar-neutral`, `future grammar`, `Sheets`, `BBNF-self`, `CSSOM`,
  `fact-stream`, `sidecar`, and `broadcast`.
- `git status --short --` on the six target dossiers and this CH output path
  before writing; the target dossiers had no dirty entries.

## Fold Requirements

None. Disposition is ACCEPT.

## Convergence Impact

CH2 does not block T-P2 V3 convergence. This is an ACCEPT for the generality
lens in the confirmation cycle. If CH1 and CH3-CH7 also remain ACCEPT with zero
orphan REVISE items and no target-packet edits, the V3 packet can satisfy the
two-consecutive-clean §3Z convergence rule described in the V3 challenge
context.
