# CH5 HIDDEN COUPLING

Lens name: CH5 HIDDEN COUPLING - no hidden substrate, retained sidecar,
grammar switch, runtime regex substrate, self-excluding gate,
CSS fact-stream/CSSOM substitution, wrong-host close evidence, new BIR, new
directive, or sixth `BackendShape` admitted.

Disposition: ACCEPT

Target: SK-V15 T-P2 V3 confirmation packet at challenge target `d11a9eec0`.

## Critical Findings

| id | severity | finding | evidence | convergence impact |
|---|---:|---|---|---|
| CH5-V3-00 | none | No CH5-critical hidden-coupling defect found. The live V2 research packet still preserves the substrate, sidecar, grammar-switch, runtime-regex, self-excluding-gate, CSS-workload, wrong-host, directive/BIR, and five-shape fences required for V3 confirmation. | V3 explicitly asks CH5 to confirm no hidden substrate, grammar switch, runtime regex substrate, self-excluding gate, CSS fact-stream/CSSOM substitution, or wrong-host close evidence is admitted (`restart/audit/totality/p2/hardening/V3/CHALLENGE-CONTEXT.md:50`-`52`). V2 consolidation already recorded CH5 as ACCEPT with no retained sidecar, substrate expansion, hidden directive, new BIR, sixth shape, self-excluding gate, CSS substitution, runtime regex substrate, or wrong-host SIMD close (`restart/audit/totality/p2/hardening/HARDENING-T-P2-V2-CONSOLIDATED.md:27`). The six live dossiers retain those fences as detailed below. | Does not block T-P2 V3 convergence. CH5 contributes an ACCEPT toward the second clean cycle; full V3 convergence still depends on all seven lenses accepting with zero orphan REVISE items and no target-packet edits (`restart/audit/totality/p2/hardening/V3/CHALLENGE-CONTEXT.md:29`-`30`). |

## Guard Matrix

| guard | disposition | inspected evidence |
|---|---|---|
| Hidden substrate / retained sidecar | PASS | 2A permits only transient same-loop masks consumed into the existing substrate and explicitly bars retained cursor/list/class-column/sidecar and public substrate expansion (`restart/audit/totality/p2/2A-sota-landscape.md:51`-`52`, `:87`, `:110`). 2D lowerer gates require no sidecar expansion and EventTape output without retained sidecars (`restart/audit/totality/p2/2D-cost-model.md:67`-`68`, `:75`-`76`). |
| Layer 0 / Layer 1 coupling | PASS | 2B keeps Layer 0 as vendored x86 macro infrastructure, diagnostic only for SK-V15, and Layer 1 as one-way contract vocabulary with per-primitive gates; source names alone are not admission (`restart/audit/totality/p2/2B-primitive-vocabulary.md:40`-`42`, `:59`-`61`, `:90`-`97`). |
| Generic grammar switch | PASS | 2C names `RuntimeGenerationMode` plus `runtime_profiles()` as a generic-crate grammar switch and marks the route refuted/blocked; replacement must be generated manifest driven, not hand-edited generic routing (`restart/audit/totality/p2/2C-grammar-neutrality.md:70`, `:126`, `:144`). |
| Runtime regex substrate | PASS | 2F keeps regex work in local analysis facts and blocks runtime DFA/regex import unless a named generated-runtime consumer exists and passes CH3/CH5 review (`restart/audit/totality/p2/2F-parse-that-gaps.md:38`-`40`, `:73`-`74`, `:100`, `:120`). |
| Self-excluding gate | PASS | 2C refutes the current Lock 14 scan because it omits leak roots and requires inclusion/exclusion reporting that fails on same-change omissions (`restart/audit/totality/p2/2C-grammar-neutrality.md:74`, `:127`, `:148`). |
| CSS fact-stream / CSSOM substitution | PASS | 2A refutes `CssFullParseSummary`, fact-stream `parse()`, and current lightningcss close because the products are below CSS typed document/CSSOM value planes (`restart/audit/totality/p2/2A-sota-landscape.md:60`-`62`, `:81`-`85`). 2C and 2F keep fact streams diagnostic and require generated typed CSS provider plus same-workload `cssparser` equality before close (`restart/audit/totality/p2/2C-grammar-neutrality.md:64`, `:67`, `:111`-`115`; `restart/audit/totality/p2/2F-parse-that-gaps.md:78`, `:90`, `:102`). |
| CSS broadcast admission | PASS | 2A, 2C, 2D, 2E, and 2F each block the 24-row CSS broadcast tuple from admitting rows, primitive movement, shape-consumer evidence, or PMU validation (`restart/audit/totality/p2/2A-sota-landscape.md:59`, `:79`, `:108`; `restart/audit/totality/p2/2C-grammar-neutrality.md:68`, `:146`; `restart/audit/totality/p2/2D-cost-model.md:98`; `restart/audit/totality/p2/2E-host-arch-esoterica.md:80`, `:124`, `:140`; `restart/audit/totality/p2/2F-parse-that-gaps.md:80`, `:103`, `:122`). |
| Wrong-host close evidence | PASS | 2B demotes x86/AVX-512 to diagnostic contract evidence only (`restart/audit/totality/p2/2B-primitive-vocabulary.md:64`, `:157`-`164`, `:182`). 2D keeps AVX-512 CollapsedStage diagnostic until an aarch64 route exists (`restart/audit/totality/p2/2D-cost-model.md:65`, `:87`, `:97`, `:118`). 2E states Apple M5 Max/aarch64 is the only close route, x86 is diagnostic, and `svmatch_u8` is SVE2-not-NEON and refuted on this host (`restart/audit/totality/p2/2E-host-arch-esoterica.md:25`-`39`, `:68`-`82`, `:121`-`124`). |
| PMULL/CSSC and architecture-feature laundering | PASS | 2B and 2E both block PMULL/CSSC promotion from ISA bits or checkasm alone; current related aarch64 paths remain scalar-delegated until a same-wave consumer and row-local movement exist (`restart/audit/totality/p2/2B-primitive-vocabulary.md:72`, `:74`, `:148`-`149`, `:155`; `restart/audit/totality/p2/2E-host-arch-esoterica.md:75`-`76`, `:111`-`122`, `:139`). |
| New BIR / new directive / sixth `BackendShape` | PASS | 2C preserves no new directive, no new BIR, and no sixth `BackendShape`, and future grammar onboarding is source/metadata only with no new directive, BIR variant, sixth shape, or generic grammar branch (`restart/audit/totality/p2/2C-grammar-neutrality.md:52`-`54`, `:73`, `:149`). 2D preserves exactly the five-shape canon `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` and rejects solving lowerer debt by adding a new variant (`restart/audit/totality/p2/2D-cost-model.md:47`-`49`, `:62`, `:85`, `:117`). Current `skinny/crates/ir/src/lib.rs:340`-`345` and `skinny/crates/ir/src/cost.rs:333`-`339` still enumerate exactly those five variants. |

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
- Targeted searches over the six dossiers and relevant IR files for:
  `sidecar`, `retained`, `substrate`, `RuntimeGenerationMode`,
  `runtime_profiles`, `runtime regex`, `regex-automata`, `self-excluding`,
  `CSS_GENERATED_RS`, `CssFullParseSummary`, `fact-stream`, `CSSOM`,
  `broadcast`, `x86`, `AVX-512`, `svmatch`, `PMULL`, `CSSC`, `BIR`,
  `directive`, `BackendShape`, and `sixth`.
- `git status --short skinny/crates/ir/src grammar restart/skinny/tranches/sk-v15/SPEC.md`, which returned no dirty entries for the directive/BIR/BackendShape source surfaces checked by this lens.

## Fold Requirements

None. CH5 is ACCEPT and opens no V3 REVISE or REJECT fold item.

Preservation note for any later pass: keep retained sidecar-like routes
pre-blocked unless a new Alpha/P1/SPEC contract admits them; keep grammar
selection generated-manifest driven rather than a generic switch; keep runtime
regex import blocked absent named consumer plus CH3/CH5 review; keep CSS fact
streams and brace counters diagnostic; keep wrong-host evidence diagnostic;
and keep the five-variant `BackendShape` canon closed.

## Convergence Impact

CH5 does not block T-P2 V3 convergence. Because T-P2 V2 was already a clean
cycle and V3 is the confirmation challenge over the unchanged V2 research
packet, this ACCEPT can count as CH5's second consecutive clean cycle. If the
other six V3 lenses also return ACCEPT, with zero orphan REVISE items and no
target-packet edits, T-P2 satisfies the two-consecutive-clean Section 3Z
convergence rule (`restart/audit/totality/p2/hardening/V3/CHALLENGE-CONTEXT.md:27`-`30`).
