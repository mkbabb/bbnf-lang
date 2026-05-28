---
agent: 2D
pass: T-P2-research
cycle: V2
generated_at: 2026-05-28T06:36:45Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
primary_sources_cited: 30
techniques_grounded: 7
techniques_refuted: 6
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised:
    - "CH1-V1-01: replaced dead simdjson tape URL and recorded V2 URL check."
    - "CH4-V1-02: split Decision Engine and five-shape lowerer work into W7/W8/W9 costed units with LOC, risk, owner, hard-cap fit, gate, verification, and close status."
    - "CH6-V1-01: added row-local transfer reason, admission gate, verification action, close status, LOC estimate, risk class, wave owner, and hard-cap fit to grounded/partial rows."
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

| spec-claim or T-P1-divergence-id | primary source cited | state | transfer_reason | admission_gate | verification_action | close_status | loc_estimate | risk_class | wave_owner | hard_cap_fit |
|---|---|---|---|---|---|---|---:|---|---|---|
| `T2D-SKV15-EGRAPH-ACTIVATION`: a Decision Engine may use e-graphs to keep alternative backend plans before cost extraction. | Tate, Stepp, Tatlock, Lerner, "Equality Saturation: a New Approach To Optimization", POPL 2009, <https://www.cs.cornell.edu/~lerner/papers/popl09.html>; Willsey et al., "egg: Fast and Extensible Equality Saturation", POPL 2021, <https://popl21.sigplan.org/details/POPL-2021-research-papers/23/egg-Fast-and-Extensible-Equality-Saturation>; local e-graph rewrite/extract APIs at `crates/egraph/src/rewrite.rs:1-7`, `crates/egraph/src/extract.rs:1-11`, `crates/egraph/src/extract.rs:83-94`. | grounded as a technique; live implementation partial | W7 needs non-destructive backend alternatives before extraction. | Add at least one asserted grammar-neutral rewrite; require `RunReport.per_rule` nonzero work; reject zero-rule runs as scaffold. | Execute W7 fixtures showing rewrite work and selected-plan or gate-rejection movement. | partial-blocked | 350-650 | medium | W7 | fits if restricted to decision facts and fixtures. |
| `T2D-SKV15-COST-MODEL`: backend selection needs an objective/frontier surface rather than a priority label. | `CostModel` and lattice extraction API at `crates/egraph/src/extract.rs:25-40`, `crates/egraph/src/extract.rs:83-94`; Fraser, Hanson, Proebsting, "Engineering a Simple, Efficient Code Generator Generator", LOPLAS 1992, DOI <https://doi.org/10.1145/151640.151642>. | grounded; live implementation partial | W7 must turn candidate choice into measurement-bearing extraction rather than shape-priority sorting. | Populate nonzero row-local `perf_cost` or explicit diagnostic rejection facts; forbid all-candidate `perf_cost: 0` as close evidence. | Add negative fixture where stale/broadcast/missing timing facts alter extraction or report status. | partial-blocked | 250-450 | medium | W7 | fits with telemetry wiring only; broad benchmarking does not fit. |
| `T2D-SKV15-CSP-FEASIBILITY`: CSP is appropriate as a feasibility and objective layer after candidate generation. | Google OR-Tools CP-SAT official example shows integer variables, constraints, objective, solver, and feasible/optimal status, <https://developers.google.com/optimization/cp/cp_example>. Local CSP finalization at `skinny/crates/passes/src/decision_csp.rs:16-27`. | grounded as a technique; live implementation refuted as proof | W7 needs hardware, parity, consumer, substrate, and budget feasibility gates after candidate generation. | Add grammar-neutral facts; removing parity, recognizer, substrate, SIMD/hardware, consumer, budget, or generated-output facts must change SAT/UNSAT or selected candidate. | Run deletion/alteration fixtures and fail if generic facts include `json_*` or `css_*` names. | partial-blocked | 300-550 | medium-high | W7 | fits if scoped to CSP facts and fixtures. |
| `T2D-SKV15-FIVE-SHAPE-CANON`: the candidate set is exactly five BackendShape variants. | Local enum at `skinny/crates/ir/src/lib.rs:339-345`; local all-five helper at `skinny/crates/ir/src/cost.rs:333-341`; SK-V15 non-negotiable rejects a new/sixth BackendShape at `restart/skinny/tranches/sk-v15/SPEC.md:120-129`. | grounded locally; finite-set literature supports the class | W8/W9 must implement or reject the existing variants instead of adding a sixth shape. | All-five gate must see exactly `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` and classify each as implemented, gate-consumed rejected, scalar-delegated, or blocked. | Add enum/all-helper equality tests and generated report checks that fail on a sixth shape or missing existing shape. | admissible-after-gate | 120-220 | low-medium | W9 | fits as a guard; does not include lowerer implementation cost. |
| `T2D-SKV15-SINKONLY`: consumer-known direct projection can avoid general materialization. | Li et al., "Mison: A Fast JSON Parser for Data Analytics", VLDB 2017, <https://www.microsoft.com/en-us/research/publication/mison-fast-json-parser-data-analytics/>; SinkOnly lowerer source at `skinny/crates/codegen/src/lower/sink_only.rs:19-90`. | grounded; only lowerer with real current code | W9 may use direct projection only for consumer-known output paths. | Require independent equality/oracle proof and a same-wave consumer; reject grammar-named `JsonSink` generic contracts. | Add SinkOnly golden generated-source fixture plus direct-output equality test. | admissible-after-gate | 150-300 | medium | W9 | fits if consumer-known only. |
| `T2D-SKV15-STAGED-TAPES`: EagerTape, OffsetTape, and EventTape are defensible materialization classes. | Langdale and Lemire, "Parsing Gigabytes of JSON per Second", VLDB Journal 2019, <https://arxiv.org/abs/1902.08318>; simdjson tape documentation, <https://simdjson.org/api/0.8.0/md_doc_tape.html>. | grounded as parser architecture; current bbnf lowerers refuted as evidence | W8/W9 can transfer staged structural discovery and tape materialization as classes. | EagerTape/OffsetTape/EventTape must emit runtime-relevant output; marker strings, pass-through shells, no-op rules, and `todo!` fail. | Add golden generated-source fixtures and runtime equality checks for all staged tape shapes. | partial-blocked | 900-1600 | high | W8/W9 | fits only when split across W8 and W9; bulk tape rewrite exceeds cap. |
| `T2D-SKV15-COLLAPSEDSTAGE`: branchless AVX-512 FSM work is useful pressure but not an M5 Max close route. | asmjson docs.rs source/docs at <https://docs.rs/asmjson/latest/asmjson/>; Sneller named-technique post "Branchless Code With AVX-512", <https://sneller.ai/blog/branchless-code-avx-512/>. | partial / diagnostic only | W9 may keep CollapsedStage in the five-shape canon, but x86 AVX-512 evidence transfers only as pressure. | Apple M5 Max / aarch64 admission requires 2E-backed source evidence plus `scalar_reference`, `parity_or_checkasm`, `hardware_gate`, `same_wave_consumer`, and `row_movement_target`. | Keep x86 fixtures diagnostic; add aarch64-only admission fixture or report `diagnostic-only`. | diagnostic-only | 400-800 after 2E source; 80-140 diagnostic guard | high | W9 with 2E dependency | guard fits now; implementation blocked without 2E aarch64 route. |
| `DEP-W7-DECISION-SPINE`: SK-V15 W7 needs at least one e-graph rewrite and a non-tautological CSP. | SK-V15 dependency row at `restart/skinny/tranches/sk-v15/SPEC.md:190-193`; W7 exit gate at `restart/skinny/tranches/sk-v15/SPEC.md:342-353`; PASS-IMPL audit at `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:65-73`. | grounded as SK-V15 requirement; current proof refuted | W7 is the only admissible owner for Decision Engine activation and cost/CSP proof. | `egraph_rewrite_count >= 1`; fact deletion or alteration changes CSP satisfiability/selection; generic facts carry no `json_*` or `css_*` names. | Run W7 report fixture and negative CSP fixtures; fail zero-rule and self-selected CSP. | partial-blocked | 900-1400 | high | W7 | fits as one wave if lowerers are excluded. |
| `DEP-W8-LOWERERS-A`: W8 owns EagerTape and OffsetTape lowerer admission. | SK-V15 W8 row at `restart/skinny/tranches/sk-v15/SPEC.md:195-199`; W8 exit gate at `restart/skinny/tranches/sk-v15/SPEC.md:355-364`. | grounded as SK-V15 requirement; current scaffold refuted | W8 must replace marker-string EagerTape/OffsetTape outputs with runtime-relevant generated paths or gate-consumed rejection. | EagerTape and OffsetTape golden outputs must contain materialization behavior, not labels; no sidecar substrate expansion. | Add golden generated-source fixtures, equality checks, and marker-string regression tests for both shapes. | partial-blocked | 700-1100 | high | W8 | fits if limited to two lowerers and shared helpers. |
| `DEP-W9-LOWERERS-B`: W9 owns EventTape, SinkOnly, CollapsedStage, and all-five BackendShape gate. | SK-V15 W9 row at `restart/skinny/tranches/sk-v15/SPEC.md:200-204`; W9 exit gate at `restart/skinny/tranches/sk-v15/SPEC.md:365-374`. | grounded as SK-V15 requirement; current scaffold/diagnostic refuted | W9 must complete the remaining five-shape proof without adding a sixth shape. | EventTape output must not retain sidecars; SinkOnly must prove direct consumer equality; CollapsedStage is diagnostic-only unless aarch64 gate passes; all-five report must classify exactly five shapes. | Run EventTape/SinkOnly/CollapsedStage fixtures plus all-five report and sixth-shape regression test. | partial-blocked | 850-1300 | high | W9 | fits if CollapsedStage remains diagnostic until 2E supplies an aarch64 route. |

## W7/W8/W9 Costed Work Units

| wave | unit | LOC estimate | risk | wave owner | hard-cap fit | admission gate | verification action | close_status |
|---|---|---:|---|---|---|---|---|---|
| W7 | Decision Engine spine: e-graph rewrite activation, measurement-bearing cost extraction, and non-tautological CSP facts. | 900-1400 | high | W7 | Fits only if lowerer output is out of scope. | At least one asserted rewrite with nonzero per-rule work; cost extraction consumes row-local timing/equality/freshness or reports rejection; CSP fact deletion changes SAT/selection; no grammar-named generic facts. | Run W7 report fixture, negative CSP fact fixtures, and stale/broadcast timing rejection fixture. | partial-blocked |
| W8 | BackendShape lowerers A: `EagerTape` and `OffsetTape`. | 700-1100 | high | W8 | Fits if shared tape helpers are minimal and EventTape is deferred to W9. | Generated output must implement runtime-relevant tape materialization for both shapes; marker strings, pass-through shells, no-op rules, and sidecar expansion fail. | Golden generated-source fixtures plus runtime equality/parity checks for EagerTape and OffsetTape. | partial-blocked |
| W9 | BackendShape lowerers B and all-five guard: `EventTape`, `SinkOnly`, `CollapsedStage`, and exact five-shape report. | 850-1300 | high | W9 | Fits if CollapsedStage remains diagnostic-only unless 2E provides an aarch64 route. | EventTape emits runtime-relevant output without retained sidecars; SinkOnly proves consumer-known equality; CollapsedStage has aarch64 `scalar_reference`, `parity_or_checkasm`, `hardware_gate`, `same_wave_consumer`, and `row_movement_target` or reports diagnostic-only; all-five report sees exactly five shapes. | Golden fixtures, EventTape anti-sidecar scan, SinkOnly equality test, CollapsedStage diagnostic/aarch64 gate, and sixth-shape regression test. | partial-blocked |

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
2019 simdjson; simdjson tape docs at
<https://simdjson.org/api/0.8.0/md_doc_tape.html>; asmjson docs.rs; Sneller
branchless AVX-512 named-technique post.

V2 URL repair: the retired simdjson GitHub Pages tape-documentation URL
returned HTTP `404`; the replacement check for
`https://simdjson.org/api/0.8.0/md_doc_tape.html` returned HTTP `200`.

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
