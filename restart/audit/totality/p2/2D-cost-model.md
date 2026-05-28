---
agent: 2D
pass: T-P2-research
cycle: V1
generated_at: 2026-05-28T06:36:45Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
primary_sources_cited: 30
techniques_grounded: 7
techniques_refuted: 6
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised: []
  first_cycle_additions:
    - T2D-SKV15-EGRAPH-ACTIVATION
    - T2D-SKV15-COST-MODEL
    - T2D-SKV15-CSP-FEASIBILITY
    - T2D-SKV15-FIVE-SHAPE-CANON
    - T2D-SKV15-LOWERER-ADMISSION
    - T2D-SKV15-AARCH64-CLOSE-ROUTE
locks_amendment_candidates: 5
sk_cycle: SK-V15
t_p1_entry_state: CLEAN-FINAL-G1-AUTO-PINNED-NOT-NORMAL-3Z
implementation_floor: PASS-IMPL-V1-CSS-CONTRIVANCE-JSON-HONEST
host_close_route: Apple-M5-Max-aarch64
stale_sk_v14_material_reused: none
---

## Executive Summary

SK-V15 can defend an active Decision Engine only as a candidate-generation,
rewrite, feasibility, and extraction pipeline. The literature supports that
shape: equality saturation keeps alternatives non-destructively, cost
extraction chooses a cheapest representative, CP-SAT-style models express
constraints and objectives, BURG grounds finite alternative selection, Mison
grounds consumer-known projection, and simdjson grounds staged
materialization. The live SK-V15 implementation is not yet that system.
`backend_egraph.rs` constructs candidates and extracts a best node, but runs
the scheduler with zero rewrite rules. `decision_csp.rs` pins one selected
index, carries always-true or tautological constraints, records the grammar
name, and self-reports no generated-runtime row movement. Four of five
lowerers return marker strings. These are scaffold states, not evidence.

The five-shape BackendShape canon remains the only admissible candidate set:
`EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, `CollapsedStage`. Do not
add a sixth shape. SK-V15 should activate the existing five by executable
proof: at least one asserted e-graph rewrite, a non-tautological CSP whose
fact removal changes satisfiability or selection, grammar-neutral cost facts,
real lowerer output for all five shapes, and native Apple M5 Max / aarch64
close evidence. AVX-512 CollapsedStage sources are diagnostic only.

## Technique Grounding Table

| spec-claim or T-P1-divergence-id | primary source cited | grounded / refuted / partial | bbnf-specific note |
|---|---|---|---|
| `T2D-SKV15-EGRAPH-ACTIVATION`: a Decision Engine may use e-graphs to keep alternative backend plans before cost extraction. | Tate, Stepp, Tatlock, Lerner, "Equality Saturation: a New Approach To Optimization", POPL 2009, <https://www.cs.cornell.edu/~lerner/papers/popl09.html>; Willsey et al., "egg: Fast and Extensible Equality Saturation", POPL 2021, <https://popl21.sigplan.org/details/POPL-2021-research-papers/23/egg-Fast-and-Extensible-Equality-Saturation>; local e-graph rewrite/extract APIs at `crates/egraph/src/rewrite.rs:1-7`, `crates/egraph/src/extract.rs:1-11`, `crates/egraph/src/extract.rs:83-94`. | grounded as a technique; live implementation partial | The technique supports SK-V15 W7, but current `skinny/crates/passes/src/backend_egraph.rs:65-67` passes an empty rewrite slice. A zero-rule e-graph is candidate ranking through e-graph plumbing, not equality saturation evidence. |
| `T2D-SKV15-COST-MODEL`: backend selection needs an objective/frontier surface rather than a priority label. | `CostModel` and lattice extraction API at `crates/egraph/src/extract.rs:25-40`, `crates/egraph/src/extract.rs:83-94`; Fraser, Hanson, Proebsting, "Engineering a Simple, Efficient Code Generator Generator", LOPLAS 1992, DOI <https://doi.org/10.1145/151640.151642>. | grounded; live implementation partial | `skinny/crates/passes/src/backend_egraph.rs:176-203` has a real tuple cost ordering, and `skinny/crates/ir/src/cost.rs:173-199` records active-cost telemetry. But `skinny/crates/passes/src/lib.rs:584-587` gives `perf_cost: 0` to every candidate and ranks with static size/shape order, so the cost model is not yet measurement-bearing. |
| `T2D-SKV15-CSP-FEASIBILITY`: CSP is appropriate as a feasibility and objective layer after candidate generation. | Google OR-Tools CP-SAT official example shows integer variables, constraints, objective, solver, and feasible/optimal status, <https://developers.google.com/optimization/cp/cp_example>. Local CSP finalization at `skinny/crates/passes/src/decision_csp.rs:16-27`. | grounded as a technique; live implementation refuted as proof | SK-V15 should use CSP for hardware gates, parity availability, same-wave consumer availability, generated LOC budget, and no-sidecar constraints. Current `skinny/crates/passes/src/decision_csp.rs:53-83` forces the active candidate and adds constraints that are true for all candidate shapes except CollapsedStage/SIMD and capacity. It is not yet a non-tautological decision proof. |
| `T2D-SKV15-FIVE-SHAPE-CANON`: the candidate set is exactly five BackendShape variants. | Local enum at `skinny/crates/ir/src/lib.rs:339-345`; local all-five helper at `skinny/crates/ir/src/cost.rs:333-341`; SK-V15 non-negotiable rejects a new/sixth BackendShape at `restart/skinny/tranches/sk-v15/SPEC.md:120-129`. | grounded locally; finite-set literature supports the class | BURG grounds finite alternatives selected by cost; bbnf's exact five are a local contract. Preserve the five-shape canon and add all-five proof gates. Do not introduce `UnionTape`, retained class lanes, sidecar event vectors, or a sixth variant. |
| `T2D-SKV15-SINKONLY`: consumer-known direct projection can avoid general materialization. | Li et al., "Mison: A Fast JSON Parser for Data Analytics", VLDB 2017, <https://www.microsoft.com/en-us/research/publication/mison-fast-json-parser-data-analytics/>; SinkOnly lowerer source at `skinny/crates/codegen/src/lower/sink_only.rs:19-90`. | grounded; only lowerer with real current code | `SinkOnly` is the one non-marker lowerer. It remains admissible only for direct-output consumers with independent equality/oracle proof. It must not become a grammar-named `JsonSink` generic contract. |
| `T2D-SKV15-STAGED-TAPES`: EagerTape, OffsetTape, and EventTape are defensible materialization classes. | Langdale and Lemire, "Parsing Gigabytes of JSON per Second", VLDB Journal 2019, <https://arxiv.org/abs/1902.08318>; simdjson tape documentation, <https://simdjson.github.io/simdjson/md_doc_tape.html>. | grounded as parser architecture; current bbnf lowerers refuted as evidence | Staged structural discovery and tape materialization transfer as classes. Current lowerers at `skinny/crates/codegen/src/lower/eager_tape.rs:15-17`, `skinny/crates/codegen/src/lower/offset_tape.rs:15-17`, and `skinny/crates/codegen/src/lower/event_tape.rs:15-17` emit marker strings only, so they do not establish generated runtime behavior. |
| `T2D-SKV15-COLLAPSEDSTAGE`: branchless AVX-512 FSM work is useful pressure but not an M5 Max close route. | asmjson docs.rs source/docs at <https://docs.rs/asmjson/latest/asmjson/>; Sneller named-technique post "Branchless Code With AVX-512", <https://sneller.ai/blog/branchless-code-avx-512/>. | partial / diagnostic only | These sources are x86 AVX-512 pressure, not Apple M5 Max / aarch64 evidence. Current eligibility at `skinny/crates/passes/src/lib.rs:926-928` checks `target.avx512bw` plus entry shape only; current lowerer at `skinny/crates/codegen/src/lower/collapsed_stage.rs:15-17` emits a marker string. That cannot admit SK-V15. |
| `DEP-W7-DECISION-SPINE`: SK-V15 W7 needs at least one e-graph rewrite and a non-tautological CSP. | SK-V15 dependency row at `restart/skinny/tranches/sk-v15/SPEC.md:190-193`; W7 exit gate at `restart/skinny/tranches/sk-v15/SPEC.md:342-353`; PASS-IMPL audit at `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:65-73`. | grounded as SK-V15 requirement; current proof refuted | The required proof is executable: `egraph_rewrite_count >= 1`, fact deletion or alteration changes CSP satisfiability/selection, and generic facts carry no `json_*` or `css_*` names. |
| `DEP-W8-LOWERERS-A` and `DEP-W9-LOWERERS-B`: lowerers must emit real implementation paths. | SK-V15 W8/W9 rows at `restart/skinny/tranches/sk-v15/SPEC.md:195-199`, `restart/skinny/tranches/sk-v15/SPEC.md:200-204`; W8/W9 exit gates at `restart/skinny/tranches/sk-v15/SPEC.md:355-374`. | grounded as SK-V15 requirement; current four-shape scaffold refuted | EagerTape/OffsetTape are W8; EventTape/SinkOnly/CollapsedStage plus all-five gate are W9. Label strings, pass-through shells, no-op rules, and `todo!` fail. |

## Architectural Assertions Defended

| assertion | defence | SK-V15 adoption rule |
|---|---|---|
| Decision Engine activation is candidate generation plus non-destructive rewrites plus cost extraction plus feasibility filtering. | Equality saturation and egg ground non-destructive alternatives; local `Rewrite` only adds equivalences (`crates/egraph/src/rewrite.rs:1-7`), and local `Extractor` is explicitly cost-model driven (`crates/egraph/src/extract.rs:113-124`). | W7 must report at least one asserted rewrite with per-rule work, then show selected plan changes or a gate-consumed rejection. A zero-rule run is scaffold-only. |
| The cost model must be active and measurement-bearing. | Local `BackendCandidate` has `perf_cost`, `capacity_cost`, `static_size_cost`, and `shape_rank` (`skinny/crates/passes/src/backend_egraph.rs:13-25`); local `DecisionCost` orders those fields (`skinny/crates/passes/src/backend_egraph.rs:176-203`). | Candidate costs must consume row-local timing/equality/freshness. If `perf_cost` stays zero for every candidate, the report is not a cost model. |
| CSP is a feasibility guard, not a proof by naming. | OR-Tools examples model variables, constraints, objectives, and solver status; local `DecisionCspFacts` records variables, constraints, objective count, budget, selected shape, and status (`skinny/crates/ir/src/cost.rs:201-247`). | Removing parity, recognizer, substrate, SIMD, capacity, consumer, or budget facts must change SAT/UNSAT or selected candidate in at least one fixture. Otherwise CSP is tautological. |
| The five BackendShape canon is closed for SK-V15. | `BackendShape` has five variants and `all_backend_shapes()` returns those five (`skinny/crates/ir/src/lib.rs:339-345`, `skinny/crates/ir/src/cost.rs:333-341`). SK-V15 explicitly forbids a new/sixth shape (`restart/skinny/tranches/sk-v15/SPEC.md:120-129`). | Implement or gate-consume rejection for the existing five. Do not solve lowerer debt by adding a new variant. |
| `SinkOnly` is the only currently substantive shape lowerer. | `sink_only.rs` defines `SinkOnlyProgram`, rules, direct shapes, runtime policy summary, and expression lowering structures (`skinny/crates/codegen/src/lower/sink_only.rs:19-90`). | Keep SinkOnly direct-only and consumer-known. It cannot justify the other four shapes or an all-five claim. |
| CollapsedStage is a future/backend diagnostic unless re-derived for aarch64. | asmjson and Sneller ground x86 AVX-512 branchless/mask pressure; SK-V15 host close route is Apple M5 Max / aarch64 (`restart/audit/totality/p2/T-P2-DISPATCH-CONTEXT.md:99-101`). | CollapsedStage may remain in the five-shape canon, but admission on SK-V15 needs aarch64 source-backed strategy, scalar oracle, parity/checkasm or equivalent differential, and same-wave consumer. |

## Architectural Assertions Refuted

| refuted assertion | why it is refuted | consequence |
|---|---|---|
| A zero-rule e-graph proves Decision Engine activation. | `skinny/crates/passes/src/backend_egraph.rs:65-67` constructs `rules: [&dyn RewriteFn<DecisionNode, NoAnalysis>; 0] = []` and runs the scheduler over it. The e-graph crate's own `RunReport` counts per-rule work (`crates/egraph/src/scheduler.rs:13-39`), but zero rules mean no rewrite work can exist. | W7 must add at least one asserted rewrite and gate on rewrite count/work. |
| Current CSP is non-tautological. | `skinny/crates/passes/src/decision_csp.rs:53-83` first forces the selected active-cost candidate, then adds constraints that mostly accept all present shapes; it records `csp_named_grammars` from the grammar name at `skinny/crates/passes/src/decision_csp.rs:116-124`; it self-reports `block_id` no row movement at `skinny/crates/passes/src/decision_csp.rs:162-167`. | Replace grammar-named, self-selected CSP fixtures with grammar-neutral facts whose removal changes SAT/selection. |
| Marker-string lowerers are evidence for backend coverage. | Four files return only `format!("rule {} -> <shape>", rule.name)` at `skinny/crates/codegen/src/lower/eager_tape.rs:15-17`, `skinny/crates/codegen/src/lower/offset_tape.rs:15-17`, `skinny/crates/codegen/src/lower/event_tape.rs:15-17`, and `skinny/crates/codegen/src/lower/collapsed_stage.rs:15-17`. | W8/W9 lowerer tests must fail these old scaffolds and require runtime-relevant output or gate-consumed rejection. |
| The five-shape enum alone proves all-five BackendShape derivation. | The enum exists, but `backend_candidates()` still assigns every candidate `perf_cost: 0` (`skinny/crates/passes/src/lib.rs:571-587`) and four lowerers are markers. A candidate set is not a derivation proof. | Add an all-five gate that sees exactly the five variants and distinguishes implemented, rejected, and blocked per shape. |
| AVX-512 CollapsedStage can close SK-V15 on M5 Max. | asmjson requires AVX-512BW or SWAR and warns low-level AVX-512 calls fault on unsupported CPUs; Sneller's technique is AVX-512 mask-register branchlessness. Neither is aarch64 proof. Current predicate only checks `target.avx512bw` and entry shape (`skinny/crates/passes/src/lib.rs:926-928`). | Treat x86 CollapsedStage as secondary diagnostic. Aarch64 admission requires 2E-backed source evidence and hardware gating. |
| CSS broadcast/full-parse rows can serve as shape-consumer evidence. | PASS-IMPL records all 24 CSS rows as one measurement broadcast and a workload mismatch; `skinny/RESULTS.md` repeats identical `track1_mbps=2319.041`, `cssparser_mbps=2362.037`, and `lightningcss_mbps=929.281` across distinct CSS row IDs; `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:12-24` names this as dispositive contrivance. | No 2D shape may cite CSS W8R broadcast as row-local consumer proof. W5/W6 typed CSS provider and same-workload retime must precede CSS shape admission. |

## Open Research Questions

| UNKNOWN | verify_action |
|---|---|
| UNKNOWN-2D-01: What is the first SK-V15 e-graph rewrite that is grammar-neutral and load-bearing? | Add one rewrite over `DecisionNode` or a richer `BackendExprLanguage`, prove `RunReport.per_rule` has nonzero work, and show selected shape or generated rejection changes for CSS L4 plus Sheets or BBNF-self. |
| UNKNOWN-2D-02: Which cost axes are sufficient for extraction without overfit? | Populate `perf_cost`, freshness, capacity, static size, generated LOC, materialization bytes, and parity availability from row-local facts; fail if a candidate uses broadcast or stale evidence. |
| UNKNOWN-2D-03: Which CSP fact deletions must be negative fixtures? | For parity, recognizer, substrate, SIMD/hardware, same-wave consumer, budget, and generated-output facts, remove or alter the fact and require SAT/selection/report status to change. |
| UNKNOWN-2D-04: What is the aarch64 CollapsedStage analogue, if any? | 2E must supply a primary aarch64 source-backed technique. Until then CollapsedStage remains a five-shape candidate with diagnostic x86 evidence only and no M5 Max admission. |
| UNKNOWN-2D-05: Can all five lowerers emit runtime-relevant output without hidden sidecars? | W8/W9 should add golden generated-source fixtures for all five shapes, EventTape anti-sidecar scans, and all-five gate proof. Marker-string output must fail. |

## LOCKS-AMENDMENTS-CANDIDATE

| candidate | target | proposed amendment | supporting evidence |
|---|---|---|---|
| LAC-2D-01 | Lock 10 / Decision Engine | "Decision Engine active" means at least one asserted e-graph rewrite, nonzero rewrite work, cost extraction from measurement-bearing candidate facts, and a generated selection/rejection consumed by a gate. | Equality saturation sources; local zero-rule e-graph at `skinny/crates/passes/src/backend_egraph.rs:65-67`; W7 gate at `restart/skinny/tranches/sk-v15/SPEC.md:342-353`. |
| LAC-2D-02 | Lock 10 / cost model | `perf_cost: 0` for all candidates is forbidden as close evidence. Cost facts must carry row-local timing/equality freshness and objective axes. | Candidate construction at `skinny/crates/passes/src/lib.rs:571-587`; `ActiveCostFacts` telemetry at `skinny/crates/ir/src/cost.rs:173-199`. |
| LAC-2D-03 | Lock 10 / CSP | CSP proof requires at least one negative fixture where removing or altering a required fact changes SAT/UNSAT or selected candidate; grammar names are not allowed in generic CSP facts. | Current selected-index CSP at `skinny/crates/passes/src/decision_csp.rs:53-83`; `csp_named_grammars` at `skinny/crates/ir/src/cost.rs:201-247`. |
| LAC-2D-04 | Lock 10 / BackendShape | Preserve exactly five BackendShape variants and require an all-five gate: `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`. Adding a sixth shape is prohibited in SK-V15. | Enum and helper at `skinny/crates/ir/src/lib.rs:339-345`, `skinny/crates/ir/src/cost.rs:333-341`; SK-V15 non-negotiable at `restart/skinny/tranches/sk-v15/SPEC.md:120-129`. |
| LAC-2D-05 | Lock 16 / CollapsedStage | AVX-512 CollapsedStage evidence is x86 diagnostic only. SK-V15 admission requires an Apple M5 Max / aarch64 strategy with scalar oracle, parity/checkasm-equivalent proof, hardware gate, and same-wave consumer. | Dispatch host-close rule at `restart/audit/totality/p2/T-P2-DISPATCH-CONTEXT.md:99-101`; current CollapsedStage predicate at `skinny/crates/passes/src/lib.rs:926-928`; marker lowerer at `skinny/crates/codegen/src/lower/collapsed_stage.rs:15-17`. |

## Source Index

Primary external sources: Tate et al. POPL 2009 equality saturation; Willsey et
al. POPL 2021 egg; Fraser/Hanson/Proebsting LOPLAS 1992 BURG; Google OR-Tools
CP-SAT documentation; Li et al. VLDB 2017 Mison; Langdale/Lemire VLDB Journal
2019 simdjson; simdjson tape docs; asmjson docs.rs; Sneller branchless AVX-512
named-technique post.

Primary local sources: `restart/audit/totality/p2/T-P2-DISPATCH-CONTEXT.md`,
`restart/prompts/totality/PASS-2-RESEARCH.md`,
`restart/prompts/ORCHESTRATOR.md`,
`restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md`,
`restart/skinny/tranches/sk-v15/SPEC.md`,
`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md`,
`skinny/RESULTS.md`, `skinny/REDRESS.md`,
`skinny/crates/ir/src/lib.rs`, `skinny/crates/ir/src/cost.rs`,
`skinny/crates/passes/src/lib.rs`,
`skinny/crates/passes/src/backend_egraph.rs`,
`skinny/crates/passes/src/decision_csp.rs`,
`skinny/crates/codegen/src/lower/eager_tape.rs`,
`skinny/crates/codegen/src/lower/offset_tape.rs`,
`skinny/crates/codegen/src/lower/event_tape.rs`,
`skinny/crates/codegen/src/lower/sink_only.rs`,
`skinny/crates/codegen/src/lower/collapsed_stage.rs`,
`crates/egraph/src/extract.rs`, `crates/egraph/src/rewrite.rs`, and
`crates/egraph/src/scheduler.rs`.
