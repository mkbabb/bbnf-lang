---
agent: 2D
pass: T-P2-research
cycle: V1
generated_at: 2026-05-21T11:30:00Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
primary_sources_cited: 12
techniques_grounded: 7
techniques_refuted: 5
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised: []
  first_cycle_additions:
    - T2D-EGRAPH-EXTRACTION
    - T2D-EQSAT-ORIGIN
    - T2D-BURG-FINITE-ALTERNATIVES
    - T2D-CSP-FEASIBILITY-LAYER
    - T2D-REGEX-NFA-DFA-PLAN
    - T2D-TAPE-STAGED-MATERIALIZATION
    - T2D-SINKONLY-PROJECTION
    - T2D-COLLAPSEDSTAGE-X86-ONLY
    - T2D-P1P8-CASCADE-NOT-OPTIMIZER
    - T2D-EIGHT-STEP-ORDER-NOT-CANONICAL
    - T2D-FIVE-SHAPE-FINITE-SET
locks_amendment_candidates: 5
---

## Executive Summary

The literature supports a **search-and-extraction** decision-engine class
for backend-shape selection (equality saturation + cost-model extraction
+ constraint-feasibility filter), not a hardcoded `P1..P8` priority
cascade. The five-shape `BackendShape` enum is a *finite candidate set*
in the BURG / instruction-selection tradition — defensible as the search
domain, but not source-anointed as universal at exactly five members.
The eight-step `derive_backend_shape` ordering is not canonical in any
cited optimizer; it is bbnf-local heuristic vocabulary. `CollapsedStage`
in its current form (`avx512bw` + `Entry(_)` eligibility, marker-string
lowerer at four of five shapes per `P1-1B-D6`) is below the published
asmjson / Sneller branchless-AVX-512 admission bar; both cited sources
are x86 AVX-512 architecture-pressure only and do not admit M5/aarch64
rows. `CostFacts` as live (`rule_id`, `chosen`, `rationale`,
`priority_fired`) is metadata; published cost models (egg's `CostModel`,
BURG dynamic-programming extraction) require an objective vector,
frontier/dominance status, and an extraction method consumed by the
gate. Refutation is the load-bearing output: `P1-1B-D3`, `P1-1B-D4`,
`P1-1B-D6` are not defensible against the cited record.

## Technique Grounding Table

| spec-claim or T-P1-divergence-id | published source cited | grounded / refuted / partial | bbnf-specific note |
|---|---|---|---|
| `T2D-EGRAPH-EXTRACTION`: equivalent backend plans can be represented together and the winner extracted by a cost function. | Willsey, Nandi, Wang, Flatt, Tatlock, Panchekha, "egg: Fast and Extensible Equality Saturation", POPL 2021, <https://arxiv.org/abs/2004.03082>. | grounded | Supports lowering `BackendShape` selection from destructive cascade to extraction over an e-graph of candidate plans. Live `backend_egraph::select` invocation at `skinny/crates/passes/src/lib.rs:477` is the correct shape-class, but lacks a `CostModel` with objective/frontier semantics. |
| `T2D-EQSAT-ORIGIN`: equality saturation is a recognised optimizer architecture, not a one-off. | Tate, Stepp, Tatlock, Lerner, "Equality Saturation: A New Approach to Optimization", POPL 2009, <https://dl.acm.org/doi/10.1145/1480881.1480915>. | grounded | Establishes that destructive rewrites lose information; bbnf's `P1..P8` cascade at `skinny/crates/passes/src/lib.rs:446-505` is a destructive walk. Equality saturation is the published alternative. |
| `T2D-BURG-FINITE-ALTERNATIVES`: a finite set of code-generation alternatives selected by a cost criterion is the canonical instruction-selection shape. | Fraser, Hanson, Proebsting, "Engineering a Simple, Efficient Code Generator Generator", ACM Letters on Programming Languages and Systems, 1992, <https://dl.acm.org/doi/10.1145/176454.176487>. | grounded | Defends the *finite* nature of `BackendShape`. Refutes any claim that the *exact eight-step order* of `derive_backend_shape` is canonical — BURG uses dynamic-programming bottom-up cost minimisation, not a hand-ordered if/else chain. |
| `P1-1B-D3`: spec's eight-step priority order is canonical. | Same BURG source above + egg/eqsat sources. None of the cited literature endorses the exact bbnf eight-step name set (recovery → host decode → layout → first-set overlap → direct-only → collapsed-stage → event tape → offset tape). | refuted | The *finite-choice* idea transfers; the *fixed step order* does not. T-P3 should reword as "derive candidates, prove feasibility, extract minimum-cost plan." The `PriorityStep::ALL` table at `skinny/crates/ir/src/cost.rs:138-160` is diagnostic vocabulary, not the optimizer. |
| `P1-1B-D2`: `CostFacts` should be an active objective/frontier model. | `egg` defines `trait CostFunction` with cost vectors and Pareto/frontier extraction; BURG uses bottom-up DP over a cost lattice; OR-Tools CP-SAT defines multi-objective scalarisation, <https://developers.google.com/optimization/cp/cp_solver>. | partial | Live `CostFacts` at `skinny/crates/ir/src/cost.rs:4-17` carries `priority_fired` + `rationale` only; `ActiveCostFacts` at `:50-76` adds egraph telemetry but still names no objective/frontier/scalarisation fields. Schema must expand before a published cost-model claim is defensible. |
| `T2D-CSP-FEASIBILITY-LAYER`: CSP belongs after candidate generation for feasibility filtering, not as the primary optimizer. | Google OR-Tools CP-SAT documentation, <https://developers.google.com/optimization/cp/cp_solver>. | grounded | Supports CSP for feature-gate / consumer-availability / parity-availability / budget-feasibility constraints. Local `decision_csp::finalize_rule` invocation at `skinny/crates/passes/src/lib.rs:478` is correctly *post*-egraph; structure is sound. Schema completeness is a separate gate. |
| `T2D-REGEX-NFA-DFA-PLAN`: NFA/DFA plan choice belongs in costed alternatives, not hardcoded. | Russ Cox, "Regular Expression Matching Can Be Simple And Fast", <https://swtch.com/~rsc/regexp/regexp1.html>; Rust `regex-automata` hybrid DFA documentation, <https://docs.rs/regex-automata/latest/regex_automata/hybrid/dfa/struct.DFA.html>. | grounded | Defends an analyzer that chooses Pike/Thompson NFA, lazy DFA, or full DFA by state-count / cache pressure / grammar facts. Refutes any "always DFA" or "always NFA" admission in generic IR. The cost-model must consume these regex-engine facts. |
| `T2D-TAPE-STAGED-MATERIALIZATION`: staged structural-discovery then later materialisation is a proven JSON parser class. | Langdale, Lemire, "Parsing Gigabytes of JSON per Second", VLDB 2019, <https://arxiv.org/abs/1902.08318>; simdjson source at HEAD `168ef580757d75270475b379e83c2b39787a6765`, <https://github.com/simdjson/simdjson>. | grounded | Grounds the `EagerTape` / `OffsetTape` / `EventTape` class generally. Does NOT reopen retained union-tape or streaming-cursor designs, which `skinny/REDRESS.md` 96/97/98 measured as regressive on M5 Max. |
| `T2D-SINKONLY-PROJECTION`: direct projection / sink-only lowering can beat general materialisation when consumer is known. | Li, Katsipoulakis, Chandramouli, Goldstein, Kossmann, "Mison: A Fast JSON Parser for Data Analytics", VLDB 2017, <https://www.vldb.org/pvldb/vol10/p1118-li.pdf>. | grounded | Supports `SinkOnly` as projection-aware shape. Admission requires strict same-plane comparator + independent oracle + generated consumer (not digest-only correctness). Live `admits_sink_only` at `skinny/crates/passes/src/lib.rs:868-872` is structurally correct (requires `target.direct_only_output` + no retained consumer + `DirectBuild` in BIR). |
| `T2D-COLLAPSEDSTAGE-X86-ONLY`: branchless AVX-512 FSM is a published shape, but is x86-AVX-512-bound. | `asmjson` README/source (AVX-512 DOM kernel), <https://docs.rs/crate/asmjson/0.2.5/source/README.md>; Sneller branchless-AVX-512 blog post, <https://sneller.ai/blog/branchless-code-avx-512/>; Sneller SQL engine source, <https://github.com/SnellerInc/sneller>. | partial / architecture pressure | Both citations are x86-only. M5 Max / aarch64 has no admissibility path from these sources alone. Current `admits_collapsed_stage` at `skinny/crates/passes/src/lib.rs:874-876` (checks only `avx512bw` + `Entry(_)`) does not match the published bar: asmjson and Sneller carry concrete FSM tables, kernel emission, branchless dispatch. Marker-string lowerer at `skinny/crates/codegen/src/lower/collapsed_stage.rs:15-17` is not admissible (`P1-1B-D6`). |
| `P1-1B-D4`: `CollapsedStage` eligibility test is published-grade. | Same asmjson / Sneller sources. Both require concrete table-driven FSM emission, not a feature-flag check. | refuted | Eligibility must include the spec's ≥4 byte-disjoint arms hub condition (`restart/ARCHITECTURE.md:1096`) PLUS a kernel emitter PLUS scalar oracle + checkasm differential per Lock 16. Current check is below all three bars. |
| `P1-1B-D6`: four of five shape lowerers emit marker strings. | Same simdjson / asmjson / Mison sources. No published parser ships marker-string lowerers. | refuted | `EagerTape`, `OffsetTape`, `EventTape`, `CollapsedStage` lowerers at `skinny/crates/codegen/src/lower/{eager_tape,offset_tape,event_tape,collapsed_stage}.rs:15-17` emit `rule {name} -> <shape>` placeholders. The five-shape enum is a defensible search domain; four of its lowerers carry no real logic. |
| `T2D-FIVE-SHAPE-FINITE-SET`: five backend shapes is the defensible candidate set. | BURG/egg sources above support finite alternatives + cost extraction; local `ir::BackendShape` at `skinny/crates/ir/src/lib.rs:339-345`; `all_backend_shapes()` at `skinny/crates/ir/src/cost.rs:208-216`. | partial | Finite is defensible. Exactly five is bbnf-local convention, not source-mandated. Amend Lock 10 to say "the five are the V1 candidate set unless T-P3 admits a generated/derived expansion with G-Omega gate." |

## Architectural Assertions Defended

| assertion | defence | adoption rule |
|---|---|---|
| Backend-shape selection should be a search/extraction problem, not a fixed cascade. | Equality saturation (Tate POPL 2009, Willsey POPL 2021) keeps alternative plans simultaneously so a cost model extracts the winner. Destructive ordering loses information; bbnf's `P1..P8` cascade is destructive. | Introduce `BackendExprLanguage` over BIR/shape candidates, bounded by node/iteration caps and per-rule guards. Live invocation at `skinny/crates/passes/src/lib.rs:477` is correctly shaped; schema completion remains. |
| The five-shape enum is a defensible finite candidate set. | BURG (Fraser/Hanson/Proebsting LOPLAS 1992) grounds finite-alternative selection by cost. Five named shapes (`EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, `CollapsedStage`) form a closed enumeration suitable for bottom-up DP / extraction. | Treat the five as the allowed search domain. Do not add `BackendShape` variants without G-Omega; deriving expansion requires Lock 10 amendment. |
| Cost must become an executable contract, not after-the-fact metadata. | egg's `CostFunction` and BURG's dynamic-programming cost minimisation both require an objective. Live `CostFacts` at `skinny/crates/ir/src/cost.rs:4-17` stores only the *outcome* of selection. | `CostFacts` must include objective axes, evidence freshness, frontier/dominance status, extraction method, and a gate-consumed report before SK-V13 W9 can claim decision-engine fold. |
| CSP belongs after candidate generation, not as a paper optimizer. | OR-Tools CP-SAT is appropriate for feasibility / objective constraints — feature gates, same-wave consumer availability, parity-oracle availability, budget feasibility, generated-size budget. | Use CSP for feasibility filtering, not for shape choice. Live `decision_csp::finalize_rule` invocation at `skinny/crates/passes/src/lib.rs:478` is correctly *post*-egraph. |
| `SinkOnly` is real when tied to a known consumer. | Mison (VLDB 2017) shows projection/filter-aware parsing avoids general materialisation when consumer schema is known. | `SinkOnly` admits only with strict same-plane comparator, independent oracle, and generated consumer (`admits_sink_only` at `skinny/crates/passes/src/lib.rs:868-872` checks the structural pre-conditions). |
| Staged tape materialisation is a published JSON parser class. | simdjson (Langdale/Lemire VLDB 2019) grounds the stage1 (structural discovery) + stage2 (materialisation) shape. | `EagerTape`/`OffsetTape`/`EventTape` are admissible classes; concrete lowerers must exist (current marker strings are not admissions). |

## Architectural Assertions Refuted

| refuted assertion | why it is refuted | consequence |
|---|---|---|
| The hardcoded `P1..P8` cascade is a literature-grounded optimizer. | The sources ground equality saturation, dynamic-programming extraction, and constraint solving — none ground a bbnf-specific if/else chain. Live cascade at `skinny/crates/passes/src/lib.rs:446-505` is heuristic. | Amend Lock 10: the cascade is legacy compatibility until replaced; `PriorityStep::ALL` is diagnostic vocabulary only. |
| The exact eight-step order in `derive_backend_shape` is canonical. | BURG endorses costed finite alternatives, not a fixed bbnf order. simdjson / Mison / sonic-rs use workload-specific materialisation, not this universal step list. | Rewrite as a resolver pipeline: candidate generation → rewrite saturation → constraint filter → cost extraction → diagnostics. |
| Current `CostFacts` implements the spec cost model. | `CostFacts` stores selected/rejected facts + measurements (`skinny/crates/ir/src/cost.rs:4-17`) but no active objective, Pareto/frontier, scalarisation, or extraction method. `ActiveCostFacts` (`:50-76`) adds egraph telemetry but no published cost-model surface. | Mark as partial; require schema expansion + gate-consumed report before SK-V13 W9 can claim decision-engine fold. |
| Existing `CollapsedStage` eligibility is admissible. | `admits_collapsed_stage` at `skinny/crates/passes/src/lib.rs:874-876` checks `target.avx512bw` + `Entry(_)` only; the ≥4-byte-disjoint-arms hub condition (`restart/ARCHITECTURE.md:1096`) is missing. The published AVX-512 sources (asmjson, Sneller) require concrete FSM/table emission. The lowerer at `skinny/crates/codegen/src/lower/collapsed_stage.rs:15-17` emits a marker string. | Keep `CollapsedStage` disabled unless a generated aarch64 strategy (per 2E) plus same-wave consumer plus checkasm differential lands. |
| Four of five lowerers carrying real logic is acceptable. | `P1-1B-D6` records `EagerTape`, `OffsetTape`, `EventTape`, `CollapsedStage` lowerers as marker strings at `skinny/crates/codegen/src/lower/{eager,offset,event,collapsed}_tape.rs:15-17`. simdjson / Mison / asmjson all ship full kernels for every claimed shape; no published parser ships marker-string lowerers. | Either implement the four lowerers or amend Lock 10 to retire shapes whose lowerers cannot land. The current state — five-shape enum, one-shape implementation — is paper-architecture. |

## Open Research Questions

| UNKNOWN | verify_action |
|---|---|
| UNKNOWN-2D-01: Does `BackendExpr` have a stable grammar-neutral node vocabulary suitable for `egraph::Language`, without embedding JSON/CSS names? | Build a V1 `BackendExprLanguage` prototype from BIR nodes only; run a Lock 14 leak scan over node constructors and rewrite guards. |
| UNKNOWN-2D-02: What measured cost axes are sufficient to prevent stale or overfit extraction? | Add a cost snapshot over JSON + CSS rows with throughput, cycles/byte, IPC, generated LOC, materialisation bytes, feature gate, evidence age; reject if >30% of candidate exprs use stale/static fallback. |
| UNKNOWN-2D-03: Can bounded equality saturation avoid graph blow-up on CSS L4 + BBNF-self grammars? | Run `BackoffScheduler` / `CspScheduler` with node + iteration caps over JSON, CSS declaration-values, synthetic selector grammar; publish node counts + saturation reason. |
| UNKNOWN-2D-04: Does CSP improve plan quality beyond extraction alone? | Compare egraph-only extraction with egraph+CSP on at least three grammars; record UNSAT causes and any selected-plan deltas. |
| UNKNOWN-2D-05: Can `CollapsedStage` be restated for aarch64 without x86 leakage? | Require an aarch64-specific source-backed candidate from 2E, then micro-prove + wire to a CSS or JSON row. Until then, treat x86 `CollapsedStage` as totality background, not SK-V13 close-route. |
| UNKNOWN-2D-06: Do lowerers actually emit artefacts for all five shapes? | Add golden generated-source tests for `EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, `CollapsedStage`. Marker-string lowerers fail this gate. |

## LOCKS-AMENDMENTS-CANDIDATE

| candidate | target | proposed amendment | supporting evidence |
|---|---|---|---|
| LAC-2D-01 | Lock 10 / cost-model decision engine | Replace "derive backend shape by P1..P8 cascade" with "generate backend-plan candidates, saturate equivalent plans, filter infeasible plans, extract by an active `CostModel`; legacy priority steps are diagnostics only." | Cascade at `skinny/crates/passes/src/lib.rs:446-505`; egg/eqsat sources (SRC-01, SRC-02); BURG source (SRC-03); T-P1 `P1-1B-D2`/`P1-1B-D3`. |
| LAC-2D-02 | Lock 10 / `CostFacts` schema | Require objective vectors, frontier/dominance status, scalarisation profile, extraction method, evidence freshness, stale/static fallback marker, and source reference for every selected and rejected shape. | Current fields at `skinny/crates/ir/src/cost.rs:4-17`; `ActiveCostFacts` at `:50-76`; egg's `CostFunction`; BURG DP cost lattice. |
| LAC-2D-03 | Lock 14 / grammar neutrality | Backend-shape rewrites, CSP constraints, and cost guards must consume *generated* grammar metadata, not grammar-name branches or JSON-role mining. CSS plus Sheets or BBNF-self proof fixtures required before fleet-wide admission. | T-P1 1B/1D findings; `derive_recognizers` JSON whitelist at `skinny/crates/passes/src/lib.rs:331` (D8); `derive_materialization_roles` JSON-role mining at `:1300-1391` (D10); Lock 14 at `restart/locks/LOCKS.md:220`. |
| LAC-2D-04 | Lock 16 / `CollapsedStage` admissibility | `CollapsedStage` admits only as a concrete emitted transient strategy with scalar reference, checkasm/parity, feature gate, local temporary lifetime, and same-wave measured consumer. AVX-512 literature is x86-only and cannot close M5/aarch64 rows. | asmjson README (SRC-10); Sneller blog (SRC-11); Sneller source (SRC-12); local marker lowerer at `skinny/crates/codegen/src/lower/collapsed_stage.rs:15-17`; SK-V13 user pin x86-out-of-scope for skinny. |
| LAC-2D-05 | Lock 1 / union substrate history | Add a material-differential clause for union-shape search: e-graph-selected or grammar-configured union variants may be attempted, but `skinny/REDRESS.md` 96/97/98 block replay of full class-column vector and streaming-cursor shapes. | REDRESS 96/97/98; equality-saturation route can express per-rule alternatives without a new public substrate. |

## Source Register

| id | primary source |
|---|---|
| SRC-01 | Willsey, Nandi, Wang, Flatt, Tatlock, Panchekha, "egg: Fast and Extensible Equality Saturation", POPL 2021, <https://arxiv.org/abs/2004.03082>. |
| SRC-02 | Tate, Stepp, Tatlock, Lerner, "Equality Saturation: A New Approach to Optimization", POPL 2009, <https://dl.acm.org/doi/10.1145/1480881.1480915>. |
| SRC-03 | Fraser, Hanson, Proebsting, "Engineering a Simple, Efficient Code Generator Generator", ACM Letters on Programming Languages and Systems, 1992, <https://dl.acm.org/doi/10.1145/176454.176487>. |
| SRC-04 | Google OR-Tools CP-SAT documentation, <https://developers.google.com/optimization/cp/cp_solver>. |
| SRC-05 | Rust `regex-automata` hybrid DFA documentation, <https://docs.rs/regex-automata/latest/regex_automata/hybrid/dfa/struct.DFA.html>. |
| SRC-06 | Russ Cox, "Regular Expression Matching Can Be Simple And Fast", <https://swtch.com/~rsc/regexp/regexp1.html>. |
| SRC-07 | Li, Katsipoulakis, Chandramouli, Goldstein, Kossmann, "Mison: A Fast JSON Parser for Data Analytics", VLDB 2017, <https://www.vldb.org/pvldb/vol10/p1118-li.pdf>. |
| SRC-08 | Langdale, Lemire, "Parsing Gigabytes of JSON per Second", VLDB 2019, <https://arxiv.org/abs/1902.08318>. |
| SRC-09 | simdjson source and implementation docs, <https://github.com/simdjson/simdjson>. |
| SRC-10 | asmjson crate source README, <https://docs.rs/crate/asmjson/0.2.5/source/README.md>. |
| SRC-11 | Sneller branchless-code blog post, architecture pressure only, <https://sneller.ai/blog/branchless-code-avx-512/>. |
| SRC-12 | Sneller SQL engine source repository, <https://github.com/SnellerInc/sneller>. |
