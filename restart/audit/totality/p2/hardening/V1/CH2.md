# CH2 GENERALITY

Disposition: ACCEPT

## Critical Findings

| id | severity | finding | evidence |
|---|---:|---|---|
| CH2-V1-00 | none | No CH2-critical grammar-name leak, fleet-wide overclaim, or hidden directive/BIR/BackendShape expansion found. The dossiers bound JSON-only, CSS-only, x86-only, and scaffold-only claims instead of promoting them fleet-wide. | 2A keeps JSON claims on measured workload planes and marks current CSS evidence refuted until typed same-workload output exists (`restart/audit/totality/p2/2A-sota-landscape.md:25`, `restart/audit/totality/p2/2A-sota-landscape.md:35`, `restart/audit/totality/p2/2A-sota-landscape.md:56`, `restart/audit/totality/p2/2A-sota-landscape.md:104`); 2C explicitly refutes JSON-only/CSS-only/generic-crate grammar-switch generalisation (`restart/audit/totality/p2/2C-grammar-neutrality.md:41`, `restart/audit/totality/p2/2C-grammar-neutrality.md:114`, `restart/audit/totality/p2/2C-grammar-neutrality.md:119`); 2D preserves exactly five BackendShape variants and forbids a sixth (`restart/audit/totality/p2/2D-cost-model.md:44`, `restart/audit/totality/p2/2D-cost-model.md:59`, `restart/audit/totality/p2/2D-cost-model.md:105`). |

## Evidence Inspected

- Authority and lens contract: `restart/audit/totality/p2/hardening/V1/CHALLENGE-CONTEXT.md:1`, `restart/prompts/totality/PASS-2-RESEARCH.md:68`, `restart/prompts/totality/PASS-2-RESEARCH.md:99`, `restart/prompts/ORCHESTRATOR.md:74`, `restart/prompts/ORCHESTRATOR.md:92`, `restart/audit/totality/p2/T-P2-DISPATCH-CONTEXT.md:99`.
- 2A: JSON claims are same-plane only; CSS broadcast/fact-stream/full-parse evidence is refuted; SOTA comparator planes are proposed as explicit bench metadata, not grammar-neutral proof by citation (`restart/audit/totality/p2/2A-sota-landscape.md:25`, `restart/audit/totality/p2/2A-sota-landscape.md:56`, `restart/audit/totality/p2/2A-sota-landscape.md:58`, `restart/audit/totality/p2/2A-sota-landscape.md:88`, `restart/audit/totality/p2/2A-sota-landscape.md:104`).
- 2B: Layer 0 is x86 diagnostic, Layer 1 remains a grammar-neutral primitive vocabulary only under generated grammar facts, and generic primitives require JSON plus non-JSON consumers before admission (`restart/audit/totality/p2/2B-primitive-vocabulary.md:36`, `restart/audit/totality/p2/2B-primitive-vocabulary.md:51`, `restart/audit/totality/p2/2B-primitive-vocabulary.md:64`, `restart/audit/totality/p2/2B-primitive-vocabulary.md:141`, `restart/audit/totality/p2/2B-primitive-vocabulary.md:164`).
- 2C: Lock 14 transfer is operational: inputs are grammar source, workspace metadata, and generated per-grammar surfaces; CSS-only tables, CSS string-literal generation, runtime grammar switches, and omitted Lock 14 roots are all refuted (`restart/audit/totality/p2/2C-grammar-neutrality.md:32`, `restart/audit/totality/p2/2C-grammar-neutrality.md:46`, `restart/audit/totality/p2/2C-grammar-neutrality.md:55`, `restart/audit/totality/p2/2C-grammar-neutrality.md:62`, `restart/audit/totality/p2/2C-grammar-neutrality.md:67`, `restart/audit/totality/p2/2C-grammar-neutrality.md:68`).
- 2D: Decision Engine, CSP, and lowerer claims are bounded to grammar-neutral facts and existing five BackendShape variants; grammar-named CSP facts and AVX-512 CollapsedStage close claims are refuted (`restart/audit/totality/p2/2D-cost-model.md:48`, `restart/audit/totality/p2/2D-cost-model.md:58`, `restart/audit/totality/p2/2D-cost-model.md:59`, `restart/audit/totality/p2/2D-cost-model.md:82`, `restart/audit/totality/p2/2D-cost-model.md:85`, `restart/audit/totality/p2/2D-cost-model.md:104`).
- 2E: Host-arch claims are aarch64/M5-close bounded; x86 is secondary only; PMULL/CSSC/DotProd/I8MM/SHA3 rows require scalar oracle, gates, same-wave consumer, and row-local movement before admission (`restart/audit/totality/p2/2E-host-arch-esoterica.md:25`, `restart/audit/totality/p2/2E-host-arch-esoterica.md:71`, `restart/audit/totality/p2/2E-host-arch-esoterica.md:77`, `restart/audit/totality/p2/2E-host-arch-esoterica.md:118`, `restart/audit/totality/p2/2E-host-arch-esoterica.md:135`).
- 2F: parse-that gaps are scoped by owner and grammar semantics; JSON string/number primitives are not promoted to CSS parsing, runtime regex imports are blocked without generated-runtime consumer review, and CSS value parsing is routed to generated typed provider work (`restart/audit/totality/p2/2F-parse-that-gaps.md:33`, `restart/audit/totality/p2/2F-parse-that-gaps.md:74`, `restart/audit/totality/p2/2F-parse-that-gaps.md:76`, `restart/audit/totality/p2/2F-parse-that-gaps.md:98`, `restart/audit/totality/p2/2F-parse-that-gaps.md:119`).
- Targeted searches run: `rg -n "fleet|all grammars|arbitrary|future|grammar-neutral|JSON|CSS|Sheets|BBNF|BackendShape|directive|BIR|new directive|new BIR|Lock 14|general" restart/audit/totality/p2/2{A..F}*.md`; `rg -n "new directive|directive|directives|BIR|BackendShape|sixth|new/sixth|UnionTape|retained class|sidecar|grammar-neutral|all grammars|arbitrary grammar|future grammar|fleet-wide|generic crate|grammar switch|JSON-only|CSS-only|Sheets|BBNF-self|grammar-name|Json|Css|CSS_GENERATED_RS|RuntimeGenerationMode" restart/audit/totality/p2/2{A..F}*.md`.

## Fold Requirements

None. No REVISE or REJECT fold is required for CH2.

## Convergence Impact

This CH2 result does not block T-P2 V1 convergence.
