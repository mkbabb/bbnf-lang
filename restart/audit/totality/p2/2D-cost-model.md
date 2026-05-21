---
agent: 2D
pass: T-P2-research
cycle: V1
generated_at: 2026-05-21T08:37:56Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
primary_sources_cited: 11
techniques_grounded: 12
techniques_refuted: 6
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised: []
  first_cycle_additions: [T2D-EGRAPH-EXTRACTION, T2D-CSP-SCOPE, T2D-BACKENDSHAPE-FINITE-SET, T2D-COLLAPSEDSTAGE-X86-ONLY, T2D-P1P8-CASCADE-REFUTED]
locks_amendment_candidates: 5
---

## Executive Summary

The literature supports SK-V13's direction of replacing the P1-P8 priority
cascade with equality saturation plus cost extraction and feasibility
constraints. It does not support the current cascade as an optimizer, nor does
it support the V1 claim that the exact eight-step `derive_backend_shape`
algorithm is architecturally canonical. The defensible shape is narrower:
enumerate equivalent backend plans in an e-graph, guard rewrites with
grammar-derived facts, extract with an active cost model, then use CSP-style
constraints for parity, feature, consumer, and budget feasibility. The five
`BackendShape` variants are a useful implementation candidate set, not a
published theorem. `CollapsedStage` is grounded by AVX-512 JSON parsers only as
a hardware-gated transient FSM producer; it is not evidence for Apple M5 Max
or for a retained sidecar. Current skinny code already has the five-shape enum
and `CostFacts`, but the cost surface is metadata and the shape selector is a
hardcoded cascade. T-P3 should amend Lock 10 accordingly.

## Technique Grounding Table

| spec-claim or T-P1-divergence-id | published source cited | grounded / refuted / partial | bbnf-specific note |
|---|---|---|---|
| `T2D-EGRAPH-EXTRACTION`: equivalent backend plans can be represented together and extracted by cost. | Willsey et al., "egg: Fast and Extensible Equality Saturation", POPL/PLDI 2021, <https://arxiv.org/abs/2004.03082>; local `crates/egraph/src/language.rs:16-25`, `crates/egraph/src/extract.rs:83-94`. | grounded | This supports `BackendExpr` as an e-graph language and `BackendShape` choice as extraction, not as destructive rewrite. The local egraph crate already exposes the required `Language`, `CostModel`, and `Extractor` surfaces. |
| `T2D-EQSAT-ORIGIN`: equality saturation is a recognized optimizer architecture, not bbnf-invented. | Tate et al., "Equality Saturation: A New Approach to Optimization", POPL 2009, <https://dl.acm.org/doi/10.1145/1480881.1480915>. | grounded | T-P3 can cite this for the optimization pattern, while using Willsey/egg for the practical data structure and extraction discipline. |
| `P1-1B-D2`: `CostFacts` should be an active objective/frontier model. | `egg` extraction model above; local `crates/egraph/src/extract.rs:25-40` defines a lattice hook; `skinny/crates/ir/src/cost.rs:4-13` is only selected/rejected metadata. | partial | The architecture is defensible, but current skinny is not there. `CostFacts` must carry objective axes, evidence source, dominance/frontier status, extraction method, and stale-evidence handling before it can justify row movement. |
| `P1-1B-D3`: the exact P1-P8 cascade is the optimizer. | Local skinny `skinny/crates/passes/src/lib.rs:446-505` hardcodes the priority order; no primary optimizer source endorses this exact order. | refuted | Keep `PriorityStep` only as compatibility/diagnostic vocabulary. The actual resolver should be e-graph + cost + constraints, and any retained cascade path is a fallback, not admission evidence. |
| `P1-1B-D3`: a fixed eight-step `derive_backend_shape` is canonical. | BURG-style instruction selection grounds finite alternative selection by cost, not this fixed order: Fraser, Hanson, Proebsting, "Engineering a Simple, Efficient Code Generator Generator", ACM Letters on Programming Languages and Systems 1992, <https://dl.acm.org/doi/10.1145/176454.176487>. | partial/refuted | The finite-choice idea transfers; the named eight-step order does not. T-P3 should reword it as "derive candidates, prove feasibility, extract minimum-cost plan." |
| `T2D-CSP-SCOPE`: CSP solves global feasibility constraints after local extraction. | Google OR-Tools CP-SAT official docs, <https://developers.google.com/optimization/cp/cp_solver>; local `crates/egraph/src/csp_scheduler.rs:1-23` uses CSP only for dirty-frontier propagation. | partial | CSP is appropriate for constraints such as feature gates, same-wave consumer, and parity compatibility. Current local `CspScheduler` is not the promised multi-objective resolver; it schedules equality saturation. |
| `T2D-REGEX-NFA-DFA`: regex NFA/DFA plan selection belongs in costed alternatives. | Rust `regex-automata` hybrid DFA docs, <https://docs.rs/regex-automata/latest/regex_automata/hybrid/dfa/struct.DFA.html>; Russ Cox, "Regular Expression Matching Can Be Simple And Fast", <https://swtch.com/~rsc/regexp/regexp1.html>. | grounded | This supports an analyzer that chooses Pike/Thompson NFA, lazy DFA, or full DFA by state count/cache and grammar facts. It refutes "always DFA" or raw string-pattern switches in generic IR. |
| `T2D-SINKONLY-PROJECTION`: direct product/sink-only lowering can beat general materialization when the consumer is known. | Li et al., "Mison: A Fast JSON Parser for Data Analytics", VLDB 2017, <https://www.vldb.org/pvldb/vol10/p1118-li.pdf>. | grounded | Mison supports projection/filter-aware parsing as a materialization strategy. In bbnf terms, `SinkOnly` is admissible when generated direct/typed consumers are same-wave measured, not when it is only a digest shortcut. |
| `T2D-TAPE-MATERIALIZATION`: staged structural discovery plus later materialization is a proven JSON parser shape. | Langdale and Lemire, "Parsing Gigabytes of JSON per Second", VLDB 2019, <https://arxiv.org/abs/1902.08318>; simdjson source/docs, <https://github.com/simdjson/simdjson>. | grounded | This grounds `OffsetTape`/retained structural plans. REDRESS 96/97 show that bbnf's attempted retained SIMD union was too expensive on M5 Max; the source supports the general class, not the failed implementation. |
| `T2D-COLLAPSEDSTAGE-FSM`: AVX-512 branchless FSM/direct DOM writers are viable on x86. | `asmjson` README/source, <https://docs.rs/crate/asmjson/0.2.5/source/README.md>; Sneller AVX-512 branchless query-engine/string-processing posts and source, <https://sneller.ai/blog/branchless-code-avx-512/> and <https://github.com/SnellerInc/sneller>. | partial | These sources ground a `CollapsedStage`-like shape only under x86/AVX-512 feature gates and same-wave equality. They do not prove an aarch64 Apple M5 route, and they do not allow a retained sidecar. |
| `P1-1B-D4`: live `CollapsedStage` eligibility is enough. | Local skinny `skinny/crates/passes/src/lib.rs:804-806` checks only `avx512bw` plus entry node; local lowerer marker at `skinny/crates/codegen/src/lower/collapsed_stage.rs:15-17`. | refuted | Published AVX-512 parsers require concrete tables/FSM, feature gating, and an actual consumer. Current eligibility does not test byte-disjoint hub shape or emit a kernel. |
| `SKINNY-GEN-001`: replaying the W3 union-substrate route is viable. | `skinny/REDRESS.md` REDRESS 96/97/98: full class-column and streaming-cursor union attempts regressed every required row; REDRESS 98 retires that thesis. | refuted | New union work must cite material differential, such as e-graph-selected per-rule union shape or SIMD-first extraction, and measure row movement. Literature does not override local falsification. |
| `T2D-BACKENDSHAPE-FINITE-SET`: five backend shapes are a defensible candidate set. | BURG/egg sources above support finite alternatives plus cost extraction. Local canon: `skinny/crates/ir/src/lib.rs:401-408`, `skinny/crates/ir/src/cost.rs:127-135`. | partial | Exactly five is an implementation invariant, not a source-backed universal. Amend Lock 10 to say the five are the V1 candidate set unless T-P3 admits a generated/derived expansion with gates. |

## Architectural Assertions Defended

| assertion | defence | adoption rule |
|---|---|---|
| Backend-shape selection should be a search/extraction problem. | Equality saturation keeps alternative forms simultaneously and lets a cost model extract the winner. This is exactly the class of problem where destructive fixed ordering loses information. | Introduce `BackendExprLanguage` over BIR/shape candidates, bounded by node/iteration caps and per-rule guards. |
| Cost must become an executable contract. | Published and local egraph extraction take a `CostModel`; current skinny `CostFacts` only records the outcome after the fact. | `CostFacts` must include objective axes such as throughput, code size, materialization bytes, feature gate, branch/dispatch depth, evidence freshness, and scalarization/profile ID. |
| CSP belongs after candidate generation, not as a paper optimizer. | Constraint solvers are suitable for feasibility and objective constraints, but local `CspScheduler` is currently a saturation scheduler. | Use CSP for cross-rule constraints: feature compatibility, same-wave consumer, parity oracle availability, generated-size budget, and no hidden substrate. |
| `SinkOnly` is real when tied to a consumer. | Projection-aware parsers such as Mison show that known consumer shapes can avoid general materialization. | A `SinkOnly` row admits only with strict same-plane comparator, independent oracle, and generated consumer, not digest-only correctness. |
| `CollapsedStage` is a hardware-gated transient producer. | asmjson/Sneller-style AVX-512 parsing supports a collapsed FSM idea, but only as emitted code with tables, predicates, and a direct consumer. | On M5/aarch64, AVX-512 sources are conceptual only. Any SK-V13 use must be an ARMv9.2-specific candidate with scalar reference, checkasm, equality, and row movement. |
| The five-shape enum can remain a V1 fence. | Finite candidate sets are normal in code generation; local `BackendShape` already has the intended names. | Treat the five shapes as the allowed search domain. Do not add `BackendShape` variants in T-P3 unless G-Omega explicitly changes the lock. |

## Architectural Assertions Refuted

| refuted assertion | why it is refuted | consequence |
|---|---|---|
| The hardcoded P1-P8 cascade is a literature-grounded optimizer. | The sources ground equality saturation, dynamic-programming extraction, and constraint solving, not a bbnf-specific if/match chain. Local code at `skinny/crates/passes/src/lib.rs:446-505` is a heuristic. | T-P3 should amend Lock 10: the cascade is legacy compatibility until replaced, not close evidence. |
| The exact eight-step `derive_backend_shape` order is canonical. | Instruction-selection literature supports costed finite alternatives; parser literature supports workload-specific materialization. Neither supports the fixed order as universal. | Rewrite as a resolver pipeline: candidate generation -> rewrite saturation -> constraint filter -> cost extraction -> diagnostics. |
| Current `CostFacts` already implements the spec cost model. | `CostFacts` stores selected/rejected facts and measurements but no active objective, Pareto/frontier, scalarization, or extraction method. | Mark as partial; require schema expansion and a report consumer before SK-V13 W9 can claim decision-engine fold. |
| Existing `CollapsedStage` eligibility is admissible. | It only checks `avx512bw` and `Entry`; lowerer emits a marker string. The primary AVX-512 sources require real FSM/table code and equality. | Keep `CollapsedStage` disabled unless a generated kernel plus same-wave consumer lands. |
| Root `crates/egraph` already satisfies SK-V13 G2 for skinny. | It has reusable primitives, but skinny `passes` still uses the cascade and does not depend on that crate for backend-shape selection. | T-P3 should say "available substrate", not "landed decision engine." |
| Prior union REDRESS can be ignored because D3 unblocked the category. | USER PIN D3 unblocks the category, not the historical implementations. REDRESS 96/97/98 are still binding evidence against those shapes. | Any union candidate must name the material differential and avoid full-vector/cursor replay. |

## Open Research Questions

| UNKNOWN | verify_action |
|---|---|
| UNKNOWN-2D-01: Does `BackendExpr` have a stable, grammar-neutral node vocabulary suitable for `egraph::Language` without embedding JSON/CSS names? | Build a V1 `BackendExprLanguage` prototype from BIR nodes only; run a Lock 14 census over node constructors and rewrite guards. |
| UNKNOWN-2D-02: What cost axes are sufficient to prevent stale or overfit extraction? | Add a cost snapshot over JSON + CSS rows with throughput, cycles/byte, generated LOC, materialization bytes, feature gate, and evidence age; CH4 rejects if >30% of candidate exprs use stale/static fallback. |
| UNKNOWN-2D-03: Can bounded equality saturation avoid graph blowup on CSS L4 and BBNF-self grammars? | Run the local `BackoffScheduler` and `CspScheduler` with node/iteration caps over JSON, CSS declaration-values, and a synthetic selector grammar; publish node counts and saturation reason. |
| UNKNOWN-2D-04: Does CSP improve plan quality beyond extraction alone? | Compare egraph-only extraction with egraph+CSP on at least three grammars; record UNSAT causes and any selected plan deltas. |
| UNKNOWN-2D-05: Can `CollapsedStage` be restated for aarch64 without x86 leakage? | Require an aarch64-specific source-backed candidate from 2E, then micro-prove and wire it to a CSS or JSON row. Until then, treat x86 `CollapsedStage` as secondary/totality, not skinny M5 close. |
| UNKNOWN-2D-06: Do lowerers actually emit artefacts for all five shapes? | Add golden generated-source tests for EagerTape, OffsetTape, EventTape, SinkOnly, and CollapsedStage. Marker-string lowerers are non-admissible. |

## LOCKS-AMENDMENTS-CANDIDATE

| candidate | target | proposed amendment | supporting evidence |
|---|---|---|---|
| LAC-2D-01 | Lock 10 / cost model | Replace "derive backend shape by P1-P8 cascade" with "generate backend-plan candidates, saturate equivalent plans, filter infeasible plans, and extract by an active `CostModel`; legacy priority steps are diagnostics only." | `skinny/crates/passes/src/lib.rs:446-505`; egg/equality-saturation sources; T-P1 `P1-1B-D2/D3`. |
| LAC-2D-02 | Lock 10 / `CostFacts` schema | Require objective vectors, frontier/dominance status, scalarization profile, extraction method, evidence freshness, and source reference for every selected and rejected shape. | Current fields at `skinny/crates/ir/src/cost.rs:4-13`; local egraph `CostModel` at `crates/egraph/src/extract.rs:83-94`. |
| LAC-2D-03 | Lock 14 / grammar neutrality | Backend-shape rewrites and cost guards must consume generated grammar metadata, not grammar-name branches or JSON role mining. CSS/Sheets/BBNF-self proof fixtures are required before fleet-wide claims. | T-P1 1B/1D findings; `skinny/crates/passes/src/lib.rs:780-795` pattern switches; `restart/audit/totality/p1/1D-skinny-lessons.md` SKINNY-GEN-009/010. |
| LAC-2D-04 | Lock 16 / `CollapsedStage` | `CollapsedStage` is admissible only with a concrete emitted kernel/table/FSM, scalar reference, checkasm/parity, feature gate, and same-wave measured consumer. AVX-512 literature is x86-only and cannot close M5/aarch64 rows. | asmjson/Sneller sources; local marker lowerer; SK-V13 user pin x86-out-of-scope for skinny. |
| LAC-2D-05 | Lock 1 / union substrate history | Add a material-differential clause for union-shape search: e-graph-selected or grammar-configured union variants may be attempted, but REDRESS 96/97/98 block replay of full class-column vector and streaming cursor shapes. | REDRESS 96/97/98; SK-V13 scoping union candidates; equality-saturation route can express per-rule alternatives without a new public substrate. |

## Source Register

| id | primary source |
|---|---|
| SRC-01 | Willsey et al., "egg: Fast and Extensible Equality Saturation", POPL/PLDI 2021, <https://arxiv.org/abs/2004.03082>. |
| SRC-02 | Tate et al., "Equality Saturation: A New Approach to Optimization", POPL 2009, <https://dl.acm.org/doi/10.1145/1480881.1480915>. |
| SRC-03 | Fraser, Hanson, Proebsting, "Engineering a Simple, Efficient Code Generator Generator", ACM Letters on Programming Languages and Systems 1992, <https://dl.acm.org/doi/10.1145/176454.176487>. |
| SRC-04 | Google OR-Tools CP-SAT documentation, <https://developers.google.com/optimization/cp/cp_solver>. |
| SRC-05 | Rust `regex-automata` hybrid DFA documentation, <https://docs.rs/regex-automata/latest/regex_automata/hybrid/dfa/struct.DFA.html>. |
| SRC-06 | Russ Cox, "Regular Expression Matching Can Be Simple And Fast", <https://swtch.com/~rsc/regexp/regexp1.html>. |
| SRC-07 | Li et al., "Mison: A Fast JSON Parser for Data Analytics", VLDB 2017, <https://www.vldb.org/pvldb/vol10/p1118-li.pdf>. |
| SRC-08 | Langdale and Lemire, "Parsing Gigabytes of JSON per Second", VLDB 2019, <https://arxiv.org/abs/1902.08318>. |
| SRC-09 | simdjson source and implementation docs, <https://github.com/simdjson/simdjson>. |
| SRC-10 | asmjson crate source README, <https://docs.rs/crate/asmjson/0.2.5/source/README.md>. |
| SRC-11 | Sneller AVX-512 branchless-code post plus source repository, <https://sneller.ai/blog/branchless-code-avx-512/> and <https://github.com/SnellerInc/sneller>. |
