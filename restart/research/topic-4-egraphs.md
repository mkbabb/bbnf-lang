# Topic 4 - E-graphs, Equality Saturation, And Bridge-Vs-Union Design

Research scope: e-graphs, equality saturation, CSP/egraph bridge design, shape mining,
and extraction/cost interaction.

Source count: 10 primary or official sources.

Adversarial finding count: 5.

## §1 — Settled position in the restart

1. Output shape authority: every research artefact must carry §1-§7, and §1
   must cite path:line for every settled claim engaged
   (`restart/research/INDEX.md:18-34`).
2. Topic authority: Topic 4 is "E-graphs + equality saturation +
   bridge-vs-union design" (`restart/research/INDEX.md:79`).
3. Topic lock authority: Topic 4 anchors on e-graph rewrites and seven
   rewrite categories for V1 (`restart/research/INDEX.md:81`).
4. Topic anchor authority: Topic 4 points at README §6, ARCHITECTURE §10,
   and MASTER-PLAN "D-tranche optimization rows"
   (`restart/research/INDEX.md:81-83`).
5. Topic question authority: the restart commits to "CSP + e-graph
   (bridged, not unioned) + shape mining + cost-model trait shared with
   regex" and asks why bridged, what the bridge does, and whether prior
   literature has done it (`restart/research/INDEX.md:83`).
6. Topic source authority: the named source set includes Tate et al. 2009,
   egg 2021, egglog, Small Proofs, egg docs/case studies, Cranelift, and
   Lean 4 `simp` (`restart/research/INDEX.md:84-91`).
7. Adversarial obligation: §6 must surface pressure even where the topic
   converges with SOTA (`restart/research/INDEX.md:149-153`).
8. Voice authority: research prose must stay calibrated, direct, and
   path-cited, with no placeholder wording or soft hedging
   (`restart/research/INDEX.md:155-157`).
9. Project voice authority: the restart README binds STYLE.md and requires
   path:line citations, receiver/blocker routing, no soft hedging, and no
   quick solutions (`restart/README.md:450-453`).
10. Style authority: project writing must be pragmatic, economical, clear,
    and calibrated (`docs/precepts/instructions/STYLE.md:3-16`).
11. Style guard: the prose must avoid vague attribution, promotional warmth,
    outline-shaped closers, and mechanical boldface
    (`docs/precepts/instructions/STYLE.md:58-73`).
12. Lessons authority: research waves need challenge before synthesis
    (`docs/precepts/instructions/LESSONS-LEARNED.md:31-37`).
13. Lessons authority: every substrate change needs a same-wave consumer or
    an explicit brittleness window and restoration wave
    (`docs/precepts/instructions/LESSONS-LEARNED.md:12-19`).
14. Lessons authority: close-honesty requires every claim to ground in cited
    artefacts and every residue to name a destination
    (`docs/precepts/instructions/LESSONS-LEARNED.md:243-256`).
15. V4 carry-baseline: the V4 hardening cohort returned READY with zero open
    punch items (`restart/audit/hardening/HARDENING-CONSOLIDATED-V4.md:104-112`).
16. V4 carry-baseline: the MASTER-PLAN trio carries the executable authority
    for tranches A through J after V4
    (`restart/audit/hardening/HARDENING-CONSOLIDATED-V4.md:120-124`).
17. V5 carry-baseline: V5 did not roll back V4 structural closure; it found
    formal fragment drift, stale citations, and example scarcity
    (`restart/audit/hardening/HARDENING-CONSOLIDATED-V5.md:22-29`).
18. V5 carry-baseline: V5 confirms tape/direct, Backend IR ownership, layout
    vocabulary, generic crate boundaries, path crate names, and yaml onboarding
    intact (`restart/audit/hardening/HARDENING-CONSOLIDATED-V5.md:74-78`).
19. V5.1 carry-baseline: synthesis bundle 1 closed the formal grammar route
    by binding Architecture §8.1 to README and PASS-1 §6
    (`restart/audit/hardening/HARDENING-SYNTHESIS-V5.1.md:47-53`).
20. V5.1 carry-baseline: synthesis bundle 3 closed stale-positive README and
    citation cleanup on the assigned surfaces
    (`restart/audit/hardening/HARDENING-SYNTHESIS-V5.1.md:54-59`).
21. V5.1 carry-baseline: synthesis bundle 8 closed the A->F->J yaml
    trajectory (`restart/audit/hardening/HARDENING-SYNTHESIS-V5.1.md:69-72`).
22. README pipeline claim: "The pipeline is fixed-point co-iteration with
    SSA-style discipline" and uses explicit input/output IRs, composable
    transformations, egraph rewrite substrate, CSP inference substrate, and a
    cost model that picks emission shapes (`restart/README.md:186-189`).
23. README pass order claim: parse, validate, type inference, shape mining,
    e-graph saturation, cost-model extraction, Backend IR, lowerers, and regen
    equality are the named sequence (`restart/README.md:190-207`).
24. README co-iteration claim: validation and type inference co-iterate to a
    fixed point, while shape mining through regen equality is single-forward
    (`restart/README.md:209`).
25. README cost claim: local costs feed e-graph extraction; global rule costs
    feed the strategy resolver; per-path costs handle Pratt LUT propagation
    (`restart/README.md:211-214`).
26. README trait claim: `Cost` has `score(&self, ctx: &Context) -> u64` and
    `branches(&self) -> impl Iterator<Item = (Choice, u64)>`
    (`restart/README.md:215-217`).
27. README shared-cost claim: parser and regex models implement the trait, and
    comparison logic lives in `cost-model` (`restart/README.md:217`).
28. README bridge claim: "CSP is the central inference substrate. E-graphs are
    the rewrite + extraction substrate" (`restart/README.md:219-222`).
29. README bridge location claim: the two compose through
    `passes/csp_egraph_bridge.rs` (`restart/README.md:221`).
30. README bridge operation claim: when CSP solves a constraint naming an
    e-class, the e-class is promoted to a CSP value and the e-graph's chosen
    representative becomes the CSP value (`restart/README.md:223`).
31. README bridge operation claim: when extraction references a CSP variable,
    the CSP solution is consulted (`restart/README.md:224`).
32. README bridge operation claim: the bridge maintains CSP variable IDs
    mapped bidirectionally to e-graph class IDs (`restart/README.md:225`).
33. README separation claim: "The bridge is real architecture, not a fused
    type" (`restart/README.md:227`).
34. README separation claim: CSP and e-graph stay separate substrates with
    explicit interface methods (`restart/README.md:227`).
35. README seven-category claim: algebraic, charclass merging, keyword-set
    detection, operator-chain detection, repeat-loop hoisting, tail-call
    elimination, and non-progressing-Alt removal all land in V1
    (`restart/README.md:229-239`).
36. README generic-crate claim: the `egraph` crate is generic and bbnf-specific
    rewrites live as plug-in passes (`restart/README.md:241`).
37. README closing claim: "the optimization is CSP + e-graph + shape-mining +
    cost-model with a union-system bridge" (`restart/README.md:471-473`).
38. Lock 4 claim: CSP type/layout inference, e-graph rewriting, pattern miners,
    shape analysis, and cost model compose by output-piping
    (`restart/locks/14-LOCKS.md:40`).
39. Lock 4 claim: "No unified hypergraph" and plans that fuse CSP and e-graph
    into one solver are faults (`restart/locks/14-LOCKS.md:40`).
40. Lock 6 claim: generated source artefacts are emitted by `xtask`; no
    proc-macro facade for codegen output (`restart/locks/14-LOCKS.md:44`).
41. Lock 10 claim: Pratt and SIMD are auto-detected, and the cost model decides
    when SIMD dispatch is worth it (`restart/locks/14-LOCKS.md:52`).
42. Lock 11 claim: `egraph`, `egraph-derive`, and `csp-solver` stay as path-deps
    until APIs stabilize (`restart/locks/14-LOCKS.md:54`).
43. Lock 14 claim: generic crates, including `egraph`, `csp-solver`,
    `parse-that`, and `simd-scan`, carry zero grammar-specific code
    (`restart/locks/14-LOCKS.md:60`).
44. Architecture boundary claim: optimization graph means CSP, egraph, miners,
    and cost model compose by output piping, against a fused global hypergraph
    (`restart/ARCHITECTURE.md:29-30`).
45. Architecture crate claim: `passes` owns type inference, shape mining,
    recognizer mining, normalization, egraph/CSP bridge, and extraction
    (`restart/ARCHITECTURE.md:49`).
46. Architecture cost crate claim: `cost-model` owns cost facts, SOTA profiles,
    extraction scoring, and generated LOC budgets (`restart/ARCHITECTURE.md:54`).
47. Architecture egraph crate claim: `egraph` owns equality saturation core and
    bridge APIs (`restart/ARCHITECTURE.md:58`).
48. Architecture csp crate claim: `csp-solver` is generic and serves type
    inference, layout choices, and extraction facts (`restart/ARCHITECTURE.md:60`).
49. Architecture privacy claim: `egraph` public surface is generic arena,
    rewrite, extraction, and explanation APIs; BBNF bridge terms stay hidden
    (`restart/ARCHITECTURE.md:319`).
50. Architecture privacy claim: `csp-solver` public surface is generic variables,
    domains, constraints, solver, and explanations; BBNF fact conversion stays
    hidden (`restart/ARCHITECTURE.md:321`).
51. Architecture pipeline claim: source load through regen equality includes
    egraph rewrite, CSP solve, cost extraction, and Backend IR
    (`restart/ARCHITECTURE.md:766-784`).
52. Architecture bridge invariant: egraph and CSP exchange facts through bridge
    tables owned by `passes::bridge` (`restart/ARCHITECTURE.md:791-800`).
53. Architecture extraction invariant: cost extraction selects from alternatives
    and does not introduce grammar semantics (`restart/ARCHITECTURE.md:799-800`).
54. Architecture codegen claim: generated source is committed and Lock 6 rejects
    a proc-macro facade (`restart/ARCHITECTURE.md:1236-1237`).
55. PASS-1 IR claim: `Seq` and `Alt` nodes are consumed by recognizers,
    e-graph, and cost model with no Rust/WASM lowering policy leakage
    (`restart/audit/pass-1-substrate/PASS-1.md:31`).
56. PASS-1 IR claim: `Repeat` and `Optional` are consumed by recognizers,
    error recovery, and cost model with no loop code-shape leakage
    (`restart/audit/pass-1-substrate/PASS-1.md:32`).
57. PASS-1 BIR claim: dispatch/speculation chooses branch shape from cost
    evidence (`restart/audit/pass-1-substrate/PASS-1.md:48`).
58. PASS-1 BIR claim: terminal/scanner lowering routes regex Unicode semantics
    to regex and scanner backend choice to PASS-2 (`restart/audit/pass-1-substrate/PASS-1.md:49`).
59. MASTER-PLAN ledger claim: CSP/egraph bridge is kept as bridged crates, and
    Tranche C owns bridge facts and extraction (`restart/MASTER-PLAN.md:51`).
60. MASTER-PLAN tranche claim: Tranche C is IR And Optimization Core, producing
    Grammar IR, side tables, CSP/egraph/cost bridge facts for Backend IR
    (`restart/MASTER-PLAN.md:160-165`).
61. MASTER-PLAN carry claim: Tranche C carries PASS-1 IR/type/bridge, current
    `ir`, and CSP/egraph crates into D, E, F, H, and I
    (`restart/MASTER-PLAN.md:180-185`).
62. MASTER-PLAN C goal: create Grammar IR, side tables, and the optimization
    bridge that feeds Backend IR extraction (`restart/MASTER-PLAN.md:294-297`).
63. MASTER-PLAN C inheritance: Lock 4 says bridge crates compose by output
    piping, not a fused hypergraph (`restart/MASTER-PLAN.md:301-306`).
64. MASTER-PLAN C.W2 claim: ShapeFacts and value-shape mining are produced
    before bridge tables (`restart/MASTER-PLAN.md:313-316`).
65. MASTER-PLAN C.W4 claim: CSP/egraph bridge tables exchange facts through
    bridge API (`restart/MASTER-PLAN.md:316`).
66. MASTER-PLAN C.W5 claim: CostFacts and extraction skeleton feed selected
    alternatives to the Backend IR builder (`restart/MASTER-PLAN.md:317`).
67. MASTER-PLAN D claim: Tranche D owns the extension surface, not the
    optimization bridge (`restart/MASTER-PLAN.md:329-359`).
68. MASTER-PLAN lock ownership: Lock 4 is owned by Tranche C and closes on
    bridge tests plus no fused hypergraph (`restart/MASTER-PLAN.md:691-699`).

## §2 — SOTA literature deep-dive

1. Primary source ledger:
2. S1 - Tate, Stepp, Tatlock, Lerner. 2009. "Equality Saturation: A New
   Approach to Optimization." POPL PDF:
   https://www.cs.cornell.edu/~lerner/papers/popl09.pdf
3. S2 - Willsey, Nandi, Wang, Flatt, Tatlock, Panchekha. 2021. "egg: Fast
   and Extensible Equality Saturation." POPL/PACMPL, DOI 10.1145/3434304,
   arXiv: https://arxiv.org/abs/2004.03082 and PDF
   https://arxiv.org/pdf/2004.03082
4. S3 - egg official crate docs, `Runner`, `Extractor`, and `Analysis`:
   https://docs.rs/egg/latest/src/egg/run.rs.html and
   https://docs.rs/egg/latest/egg/trait.Analysis.html
5. S4 - egg official repository and case-study tests:
   https://github.com/egraphs-good/egg
6. S5 - Zhang, Wang, Flatt, Cao, Zucker, Rosenthal, Tatlock, Willsey. 2023.
   "Better Together: Unifying Datalog and Equality Saturation." PLDI/PACMPL,
   DOI 10.1145/3591239, PDF:
   https://effect.systems/doc/pldi-2023-egglog/paper.pdf
7. S6 - egglog official repository and tutorial:
   https://github.com/egraphs-good/egglog and
   https://egraphs-good.github.io/egglog-tutorial/01-basics.html
8. S7 - Flatt, Coward, Willsey, Tatlock, Panchekha. 2022. "Small Proofs from
   Congruence Closure." FMCAD/arXiv:
   https://arxiv.org/abs/2209.03398
9. S8 - Cranelift official project page and Wasmtime API source for egraph
   mid-end:
   https://cranelift.dev/ and
   https://docs.wasmtime.dev/api/src/cranelift_codegen/egraph/mod.rs.html
10. S9 - Cranelift aegraph crate docs:
    https://docs.rs/cranelift-egraph/latest/cranelift_egraph/
11. S10 - Lean 4 `simp` official documentation and Lean source:
    https://lean4.dev/tactics/core/simp and
    https://github.com/leanprover/lean4/tree/master/src/Lean/Meta/Tactic/Simp
12. Provenance gap: the catalogue names "Yang et al. 2024, `egglog: Equality
    Saturation Meets Datalog`" (`restart/research/INDEX.md:87`).
13. I verified the canonical primary egglog paper as Zhang et al. 2023
    "Better Together: Unifying Datalog and Equality Saturation" (S5).
14. I did not verify a primary paper with the exact Yang-2024 title.
15. The gap is routed in §6 and §7 as a catalogue/source correction, not as a
    claim about egglog itself.

16. S1 load-bearing claim: equality saturation stores optimized versions in a
    common equivalence representation and selects a final program after
    saturation.
17. S1 evidence: the abstract states that analyses add equality information to
    a common IR, repeated analyses saturate it, and a profitability heuristic
    picks the final program from represented alternatives.
18. S1 tradeoff: equality saturation reduces phase-ordering pain, but saturation
    may not terminate and must be bounded.
19. S1 implementation detail: Peggy uses triggers and Rete-style matching to
    decide when equality analyses fire.
20. S1 operational bridge lesson: external analyses are not arbitrary side
    calls; they are trigger-indexed producers of equalities into the E-PEG.
21. S1 cost lesson: extraction can be formulated as a constrained global
    selection problem beyond greedy local extraction.
22. S1 evidence detail: Peggy's SelectBest uses a pseudo-Boolean solver to pick
    nodes satisfying well-formedness constraints and minimizing cost.
23. S1 pressure on restart: a CSP/egraph bridge is plausible because S1 already
    uses a solver-like extraction stage outside rewriting.
24. S1 pressure on restart: the bridge must keep equality growth monotone and
    put search/choice into extraction or CSP solution, instead of asking the
    e-graph representative to become truth too early.

25. S2 load-bearing claim: an e-graph represents a congruence relation over
    expressions, and equality saturation grows it by adding information.
26. S2 evidence: Willsey et al. define e-graphs with union-find, e-classes,
    e-nodes, hashconsing, and congruence invariants.
27. S2 operational claim: applying a rewrite means e-matching the left pattern,
    adding the instantiated right expression, and merging it with the matched
    e-class.
28. S2 tradeoff: e-graphs only add information, which avoids destructive
    rewrite ordering, but can grow without useful bounds.
29. S2 design claim: rebuilding defers invariant maintenance to phase
    boundaries and fits equality saturation's query-then-modify workload.
30. S2 bridge-relevant claim: e-class analyses attach semilattice facts to
    e-classes, propagate/join facts across growth, and let rewrites depend on
    facts.
31. S2 evidence detail: the paper names constant folding and free-variable
    analysis as examples previously requiring bespoke manipulation.
32. S2 tradeoff for bbnf: e-class analysis is the nearest prior art to an
    internal "bridge" between facts and rewrites.
33. S2 pressure on bbnf: e-class analysis lives inside the e-graph, while Lock 4
    wants CSP and egraph crates separated by output piping.
34. S2 conclusion: bbnf may use e-class analysis for egraph-local summaries,
    while keeping CSP's full domain/search/explanation outside the egraph.

35. S3 load-bearing claim: egg's `Runner` applies rewrites until saturation,
    iteration limit, node limit, or time limit, then extraction picks a best
    represented expression by cost.
36. S3 evidence: official `run.rs` docs say egraphs never forget state,
    compactly represent many equivalent expressions, and become ready for
    extraction by a cost function after saturation.
37. S3 pressure on bbnf: the restart's seven rewrite categories need explicit
    run limits and scheduling policy because official egg exposes those as
    first-class controls.
38. S3 load-bearing claim: egg's `Analysis` trait has `make` and `merge`, with
    optional `modify` and `pre_union`.
39. S3 evidence: docs.rs states arbitrary data associated with each `EClass`
    must behave across e-class merges.
40. S3 bridge lesson: e-class data is monotone/mergeable; CSP state with
    backtracking or unsat conflict explanation is a different discipline.
41. S3 conclusion: bridge facts that enter the egraph must be monotone
    summaries; non-monotone CSP search decisions should stay on the CSP side.

42. S4 load-bearing claim: egg is intended as a reusable egraph and equality
    saturation library for optimizers, synthesizers, and verifiers.
43. S4 evidence: the official repository links its paper, docs, tutorial, and
    test cases in propositional logic, math, and lambda calculus.
44. S4 tradeoff: the official repository points to egglog as an alternative
    approach based on Datalog, incremental execution, and composable analyses.
45. S4 bridge lesson: the egg ecosystem itself split into egg and egglog rather
    than treating one crate shape as universally sufficient.
46. S4 implication: bbnf's separate crates are defensible, but the plan should
    explicitly state why it chooses bridge tables over adopting egglog-like
    relational saturation in V1.

47. S5 load-bearing claim: egglog unifies Datalog and equality saturation in one
    fixpoint reasoning system.
48. S5 evidence: the PLDI paper says egglog supports Datalog-style incremental
    execution, cooperating analyses, and lattice reasoning, while also
    supporting EqSat term rewriting, congruence closure, and optimized-term
    extraction.
49. S5 evidence: the paper reimplements a unification-based pointer analysis
    and an EqSat floating-point rewriter; resulting systems are faster,
    simpler, and fix bugs found in originals.
50. S5 tradeoff: egglog closes the gap between relational analysis and EqSat by
    giving them a shared semantics.
51. S5 pressure on Lock 4: a unified Datalog/EqSat system is SOTA evidence
    against an unargued "no unified hypergraph" rule.
52. S5 convergence with bbnf: egglog also uses lattice-based analyses and
    functions, which resembles CSP/domain facts feeding rewrite legality.
53. S5 divergence from bbnf: egglog proves a union/fusion path can work, while
    bbnf locks bridge by output piping.
54. S5 operational lesson: if bbnf stays bridged, it should name the semantic
    line: egraph equivalence is monotone congruence; CSP solves domains,
    choices, and conflicts; the bridge passes facts and guards.

55. S6 load-bearing claim: egglog rules are defined separately from running
    them, and only terms present in the egraph are instantiated.
56. S6 evidence: the official tutorial says a rewrite rule asserts a new
    equivalent expression when the egraph contains a matching expression.
57. S6 evidence: the tutorial describes `run` as finding matches, applying
    rules, and rebuilding per iteration.
58. S6 bridge lesson: the operational unit is an iteration boundary; bridge
    calls should happen at similarly explicit boundaries.
59. S6 pressure on bbnf: a bridge that mutates CSP and egraph mid-rule without
    a boundary will be harder to test and explain.

60. S7 load-bearing claim: congruence-closure proof certificates matter for SMT
    solvers and equality saturation engines.
61. S7 evidence: Flatt et al. state that SMT solvers and equality saturation
    engines must generate proof certificates for verification and conflict
    clause generation.
62. S7 evidence: the paper gives an O(n^5) optimal algorithm under a relaxed
    proof-tree-size metric, plus a practical O(n log n) greedy algorithm.
63. S7 evidence: the implementation in egg yields the first certifying equality
    saturation engine and evaluates on 3,760 benchmarks.
64. S7 bridge lesson: once CSP facts gate rewrites, extraction and diagnostics
    need an explanation path from e-class equality, CSP constraint, and selected
    alternative to user-facing output.
65. S7 pressure on bbnf: the current bridge text says facts exchange, but it does
    not require proof/explanation material for bridge edges.

66. S8 load-bearing claim: Cranelift is an official production compiler using
    e-graphs to build a unified optimization framework.
67. S8 evidence: cranelift.dev states that Cranelift is, to its knowledge, the
    first production compiler to use e-graphs for a unified optimization
    framework.
68. S8 source evidence: `cranelift_codegen/egraph/mod.rs` describes a pass over
    a Function that removes non-skeleton nodes, performs GVN/rule application,
    creates Union nodes so values can have multiple representations, extracts
    the best values, then elaborates pure nodes back into layout.
69. S8 source evidence: the same source asserts that CLIF starts and ends with
    no Union nodes, while Union nodes may exist during the pass.
70. S8 source evidence: the post-egraph debug check panics if a Union value is
    still reachable after the pass.
71. S8 bridge lesson: Cranelift treats egraph material as pass-local and erases
    it back into ordinary IR.
72. S8 cost lesson: Cranelift's egraph cost source uses opcode costs, input
    costs, loop-depth scaling, saturating arithmetic, and infinity sentinel
    costs.
73. S8 pressure on bbnf: bridge products should be facts feeding Backend IR, not
    persistent egraph/CSP hybrid nodes in public IR.

74. S9 load-bearing claim: Cranelift's earlier `cranelift-egraph` crate docs
    define aegraph as a less powerful but highly optimized egraph variant.
75. S9 evidence: the docs state the main goal is memory efficiency and low
    allocation overhead in a production compiler.
76. S9 evidence: the docs name eclasses as semantic values and enodes as ways to
    compute those values, then explain equality saturation as cascading
    unioning through canonicalization.
77. S9 tradeoff: aegraph gives up some generality to fit compiler latency and
    memory constraints.
78. S9 pressure on bbnf: grammar optimizer V1 should pin node-growth, match,
    and extraction budgets before promising all seven rewrite categories.

79. S10 load-bearing claim: Lean 4 `simp` is a contrasting rewrite engine that
    repeatedly applies simplification lemmas to a fixed point.
80. S10 evidence: Lean docs say lemmas marked `@[simp]` feed a large database,
    and `simp only` exists for reproducible proof-library use.
81. S10 evidence: Lean docs warn that bad simplification lemmas such as bare
    commutativity can loop.
82. S10 source evidence: Lean's `Simp` implementation tracks simplifier context,
    theorem tables, congruence, caching, and maximum-step failure.
83. S10 convergence with bbnf: rewrite engines need orientation, guards, and
    budgets, even outside egraphs.
84. S10 divergence from bbnf: Lean `simp` chooses canonical simplification and
    proof stability instead of preserving all alternatives for cost extraction.

85. Operational answer: the bridge is justified when facts are useful across
    domains but the domains have different algebra.
86. Operational answer: egraph equality is congruence and monotone union.
87. Operational answer: CSP is domain narrowing, satisfiability, conflict
    reporting, and sometimes search.
88. Operational answer: cost extraction is choice among represented alternatives.
89. Operational answer: shape mining is recognition over Grammar IR, producing
    side tables before egraph/CSP exchange.
90. Operational answer: unioning all of those into one solver would erase
    ownership lines that the restart locks explicitly protect.
91. Operational answer: the bridge should therefore pass stable facts and
    identifiers, not raw solver internals.
92. Operational answer: the egraph side should publish `EClassId`, `NodeId`,
    rewrite provenance, e-class analysis summaries, and extraction candidates.
93. Operational answer: the CSP side should publish `CspVar`, domain summaries,
    solved assignments, unsat/conflict explanations, and legality predicates.
94. Operational answer: the name map should bind `NodeId -> EClassId`,
    `NodeId -> CspVar`, and where needed `EClassId -> CspVar`.
95. Operational answer: extraction should consult CSP solutions at choice time,
    then emit `CostFacts` and Backend IR facts.
96. Operational answer: rewrites should consult CSP facts only through guards or
    monotone summaries, never by mutating the CSP search state inside an
    e-matching loop.
97. Operational answer: a selected representative is an extraction result, not a
    durable e-class identity.
98. Operational answer: promotion from e-class to CSP value should mean
    "publish a constraint/fact about this e-class", not "commit to the current
    representative as the value."

## §3 — Convergence points

1. Convergence C1 - additive rewriting then extraction.
2. Restart evidence: egraph saturation precedes cost-model extraction
   (`restart/README.md:199-203`).
3. SOTA evidence: S1 and S2 both describe saturation/growth followed by
   profitability or cost-based extraction.
4. Match: bbnf's optimizer should preserve alternatives until extraction.
5. Consequence: rewrite categories can fire without selecting code shape early.

6. Convergence C2 - phase-ordering relief.
7. Restart evidence: transformations are composable and the egraph is the
   rewrite substrate (`restart/README.md:188`).
8. SOTA evidence: S1 and S2 both cite phase ordering as a major target of
   equality saturation.
9. Match: bbnf uses egraphs where ordered destructive rewrites would be brittle.
10. Consequence: Pratt detection, keyword detection, and repeat-loop hoisting
    can interact without hard-wiring a pass order among those rewrites.

11. Convergence C3 - cost extraction as a separate decision.
12. Restart evidence: local costs feed e-graph extraction and global costs feed
    strategy resolution (`restart/README.md:211-214`).
13. SOTA evidence: S1 uses a pseudo-Boolean global selection; S2/S3 expose
    extractor cost functions.
14. Match: bbnf's `cost-model` crate is correctly separated from the rewrite
    crate.
15. Consequence: regex scan cost and parser construct cost can share comparison
    logic without sharing syntax.

16. Convergence C4 - analyses can cooperate with rewrites.
17. Restart evidence: CSP and egraph exchange facts through bridge tables
    (`restart/ARCHITECTURE.md:791-800`).
18. SOTA evidence: S2 e-class analyses let rewrites depend on analysis facts.
19. Match: bbnf's bridge is a domain-specific version of facts cooperating with
    rewrites.
20. Refinement: bbnf should state that egraph-local facts must be monotone or
    merge-safe.

21. Convergence C5 - explicit boundaries are necessary.
22. Restart evidence: Lock 4 requires output piping, no fused hypergraph
    (`restart/locks/14-LOCKS.md:40`).
23. SOTA evidence: S3 `Runner` has phase limits; S6 `run` computes matches then
    applies updates and rebuilds.
24. Match: bridge exchange should be at named pass boundaries.
25. Consequence: `passes::bridge` should have a small API surface with
    deterministic inputs and outputs.

26. Convergence C6 - generic egraph crate is right.
27. Restart evidence: `egraph` public surface is generic and hides BBNF bridge
    terms (`restart/ARCHITECTURE.md:319`).
28. SOTA evidence: egg is a reusable library parameterized over user languages,
    rewrites, analyses, and cost functions.
29. Match: bbnf-specific rewrites belong in passes, not in the generic egraph
    crate.
30. Consequence: Lock 14's zero-overfitting rule is compatible with SOTA egraph
    practice.

31. Convergence C7 - egraph material should not leak into final IR.
32. Restart evidence: Backend IR is the input to lowerers and cost extraction
    selects alternatives without adding grammar semantics
    (`restart/ARCHITECTURE.md:799-802`).
33. SOTA evidence: Cranelift's egraph pass starts and ends with no Union nodes,
    with Union nodes allowed only during the pass.
34. Match: bbnf should emit Backend IR facts, not a permanent egraph/CSP hybrid
    IR.
35. Consequence: `CostFacts` and selected BIR alternatives are the fold product.

36. Convergence C8 - rewrite budgets matter.
37. Restart evidence: generated code budgets and SOTA gates are explicit
    authority surfaces (`restart/ARCHITECTURE.md:1239-1260`).
38. SOTA evidence: S1 bounds non-terminating saturation; S3 has iteration, node,
    and time limits; S10 has max-step failure.
39. Match: bbnf can make rewrite/run budgets a required acceptance gate.
40. Consequence: seven categories V1 needs "all categories under budget" rather
    than "all categories unbounded."

41. Convergence C9 - proof/explanation material is useful.
42. Restart evidence: `egraph` and `csp-solver` public surfaces include
    explanation APIs (`restart/ARCHITECTURE.md:319-321`).
43. SOTA evidence: S7 makes small congruence proofs a first-class need for
    certifying equality saturation.
44. Match: bridge explanations fit the restart's existing API vocabulary.
45. Consequence: bridge tests should include explanation round-trips beyond
    successful extraction.

46. Convergence C10 - Lean confirms orientation and guards matter.
47. Restart evidence: Pratt/SIMD are auto-detected and cost-governed, with no
    author-forced directives (`restart/locks/14-LOCKS.md:52`).
48. SOTA evidence: Lean `simp` needs curated simp lemmas, `simp only`, and
    looping-rule avoidance.
49. Match: rewrite systems need curated rule surfaces.
50. Consequence: bbnf rewrite registration should require category, guard,
    monotonicity note, and budget profile.

## §4 — Divergence points

1. Divergence D1 - bbnf rejects egglog-style fusion.
2. Restart evidence: Lock 4 says no unified hypergraph and forbids fusing CSP
   and egraph into one solver (`restart/locks/14-LOCKS.md:40`).
3. SOTA evidence: S5 egglog unifies Datalog and equality saturation and reports
   faster, simpler reimplementations for two applications.
4. Classification: principled divergence if bbnf records why CSP must remain a
   separate crate and diagnostic authority.
5. Current risk: the restart states the rule but does not yet give the SOTA
   counterargument.
6. Fold need: add a short rationale that egglog proves fusion is viable, then
   explain why bbnf V1 chooses bridge tables.

7. Divergence D2 - e-class representative language is too early.
8. Restart evidence: README says a solved CSP constraint can promote an e-class
   to a CSP value where the chosen representative becomes the CSP value
   (`restart/README.md:223`).
9. SOTA evidence: S2/S3 treat representatives as implementation details and
   extraction as the moment that chooses a best expression.
10. Classification: unconsidered or imprecise wording.
11. Risk: an e-class can grow and canonical IDs can change; choosing a
    representative before extraction can confuse equality with selection.
12. Fold need: replace "representative becomes value" with "bridge records a
    constraint/fact keyed by stable IDs; extraction later chooses an e-node."

13. Divergence D3 - seven rewrite categories V1 lacks run-control detail.
14. Restart evidence: README names all seven categories for V1
    (`restart/README.md:229-239`).
15. SOTA evidence: S1, S3, S6, S9, and S10 all stress termination, scheduling,
    cost, memory, or step limits.
16. Classification: settled commitment with under-specified acceptance gates.
17. Risk: commutativity, associativity, repeat hoisting, and loop/tail rewrites
    can multiply matches unless each category has a budget and guard profile.
18. Fold need: add a C.W4/C.W5 budget row for node limit, iteration limit,
    rewrite timeout, and per-category backoff.

19. Divergence D4 - bbnf's bridge tables have no explicit proof payload.
20. Restart evidence: Architecture exposes explanation APIs for egraph and CSP
    (`restart/ARCHITECTURE.md:319-321`), but README bridge bullets do not name
    explanations (`restart/README.md:221-227`).
21. SOTA evidence: S7 makes congruence proofs and proof size load-bearing for
    certifying equality saturation.
22. Classification: missing fold refinement.
23. Risk: diagnostics and translation checks cannot explain why a CSP fact
    allowed or denied a rewrite.
24. Fold need: make bridge rows carry provenance fields.

25. Divergence D5 - Cranelift integrates egraphs inside an existing IR.
26. Restart evidence: bbnf keeps egraph as a generic sister crate and bridge
    terms hidden (`restart/ARCHITECTURE.md:58-60`, `restart/ARCHITECTURE.md:319-321`).
27. SOTA evidence: S8 Cranelift represents egraphs in the DataFlowGraph during
    the pass, creates Union nodes, then erases them before the final CLIF state.
28. Classification: principled divergence.
29. Reason: bbnf is a grammar compiler with crate publication boundaries and
    zero grammar-specific generic crates.
30. Fold need: say that bbnf adopts Cranelift's pass-local erasure lesson,
    without adopting CLIF-style embedded Union nodes.

31. Divergence D6 - Lean `simp` is oriented canonicalization, not equality
    saturation.
32. Restart evidence: bbnf uses egraph saturation and cost extraction
    (`restart/README.md:199-203`).
33. SOTA evidence: S10 `simp` repeatedly applies simplification lemmas and
    warns against looping simplification rules.
34. Classification: useful contrast.
35. Reason: bbnf needs alternatives and cost extraction, while Lean needs stable
    proof-normalization behavior.
36. Fold need: use Lean as a rule-hygiene check, not as an architecture target.

37. Divergence D7 - Topic 4's D-tranche anchor mismatches MASTER-PLAN ownership.
38. Restart evidence: the topic catalogue names "MASTER-PLAN.md D-tranche
    optimization rows" (`restart/research/INDEX.md:82`).
39. Restart evidence: MASTER-PLAN puts optimization bridge ownership in Tranche
    C (`restart/MASTER-PLAN.md:294-317`) and D owns extension-surface work
    (`restart/MASTER-PLAN.md:329-359`).
40. Classification: provenance/routing drift in the research catalogue.
41. Risk: fold work could route bridge surgery to D instead of C.W4/C.W5.
42. Fold need: update the catalogue anchor in Phase 2 if research-index edits
    are permitted.

43. Divergence D8 - cost trait shape is too narrow for global extraction.
44. Restart evidence: README names a local-looking `score` and `branches` trait
    (`restart/README.md:215-217`).
45. SOTA evidence: S1 uses pseudo-Boolean global extraction; S2 notes other
    extraction procedures for complex cost functions; S8 uses loop-depth-scaled
    saturating costs.
46. Classification: likely under-specified.
47. Risk: the trait may fit local regex/parser comparisons while failing global
    well-formedness constraints.
48. Fold need: add a companion `ExtractorCost` or `CostModel::constraints`
    surface, or state that V1 extraction is local with a routed global-extract
    receiver.

## §5 — Refinements to fold

1. Refinement R1.
2. Target: `restart/README.md:221-227`.
3. Current text: bridge bullets promote an e-class to a CSP value by using the
   e-graph's chosen representative.
4. Proposed text: "When CSP solves a constraint that names an e-class, the
   bridge records a solved-domain fact keyed by stable `EClassId`/`NodeId`
   identifiers. It does not commit to an e-node representative. Extraction
   later chooses an e-node and consults the solved CSP fact before emitting
   `CostFacts`."
5. Rationale: S2/S3 place choice at extraction; S8 erases pass-local Union nodes
   before final IR; the restart already separates bridge tables and extraction
   (`restart/ARCHITECTURE.md:799-800`).

6. Refinement R2.
7. Target: `restart/README.md:225`.
8. Current text: "The bridge maintains a bidirectional name map: CSP variable
   IDs ↔ e-graph class IDs."
9. Proposed text: "The bridge maintains a three-way stable map:
   `GrammarNodeId -> EClassId`, `GrammarNodeId -> CspVar`, and
   `EClassId -> CspVar` only when a whole e-class has a proven CSP domain."
10. Rationale: S2 says e-class IDs and representatives are internal to the
    congruence structure; S7 pushes proof-bearing congruence; PASS-1 gives
    Grammar IR stable keys (`restart/audit/pass-1-substrate/PASS-1.md:28-37`).

11. Refinement R3.
12. Target: `restart/ARCHITECTURE.md:799`.
13. Current text: "Egraph and CSP exchange facts through bridge tables."
14. Proposed text: "Egraph and CSP exchange monotone facts through bridge
    tables; non-monotone CSP search state remains inside `csp-solver`, and
    extraction consults solved assignments through `passes::extract`."
15. Rationale: S2 e-class analyses require merge-safe data; S5 supports lattice
    reasoning; Lock 4 requires output piping (`restart/locks/14-LOCKS.md:40`).

16. Refinement R4.
17. Target: `restart/ARCHITECTURE.md:319-321`.
18. Current text: explanation APIs exist on generic egraph and CSP surfaces.
19. Proposed text: "Bridge outputs include `BridgeJustification { source_node,
    source_rule_or_constraint, eclass, csp_var, polarity, proof_ref }`, with
    proof refs supplied by egraph explanations and CSP explanations."
20. Rationale: S7 makes proof certificates a load-bearing equality-saturation
    concern; Architecture already names explanation APIs.

21. Refinement R5.
22. Target: `restart/MASTER-PLAN.md:316`.
23. Current text: "CSP/egraph bridge tables. | Egraph and CSP exchange facts
    through bridge API."
24. Proposed text: "CSP/egraph bridge tables: stable ID map, monotone fact
    exchange, rewrite guard API, and bridge-justification records. | Egraph and
    CSP exchange facts through bridge API; representative choice is tested only
    during C.W5 extraction."
25. Rationale: S2/S3/S6 show phase boundaries; S7 requires explanations; C.W5
    owns extraction (`restart/MASTER-PLAN.md:317`).

26. Refinement R6.
27. Target: `restart/MASTER-PLAN.md:317`.
28. Current text: "CostFacts and extraction skeleton. | Backend IR builder
    receives selected alternatives."
29. Proposed text: "CostFacts and extraction skeleton, including CSP-consulted
    legality and per-category rewrite budget evidence. | Backend IR builder
    receives selected alternatives with bridge justifications."
30. Rationale: S1/S8 use extraction constraints and cost heuristics; S3 exposes
    runner budgets; S7 asks for proof material.

31. Refinement R7.
32. Target: `restart/README.md:229-239`.
33. Current text: all seven e-graph rewrite categories are listed without run
    controls.
34. Proposed text: add a paragraph after the table: "Every rewrite category
    declares a guard set, monotonicity note, node-growth budget, scheduler
    profile, and extraction evidence fixture. C.W4 fails if any category only
    proves syntactic registration."
35. Rationale: S1, S3, S6, S9, and S10 all treat termination and rule discipline
    as operational requirements.

36. Refinement R8.
37. Target: `restart/ARCHITECTURE.md:54`.
38. Current text: `cost-model` owns cost facts, SOTA profiles, extraction
    scoring, and generated LOC budgets.
39. Proposed text: "`cost-model` owns local scores, branch choices, extraction
    constraints, SOTA profiles, generated LOC budgets, and profile metadata
    shared by parser and regex instances."
40. Rationale: S1's extraction can be constrained global selection; S8 uses
    loop-depth and infinity cost handling; README's trait needs room for that
    without fusing parser and regex internals.

41. Refinement R9.
42. Target: `restart/README.md:215-217`.
43. Current text: `Cost` has `score` and `branches`.
44. Proposed text: keep `score` and `branches`, then add "Global extraction
    constraints live beside the trait as `ExtractionConstraint` facts, so the
    shared trait does not force regex and parser to share internal syntax."
45. Rationale: S1 uses well-formedness constraints; S2 notes complex extraction
    procedures; Lock 4 preserves domain separation.

46. Refinement R10.
47. Target: `restart/research/INDEX.md:82`.
48. Current text: "`restart/MASTER-PLAN.md` D-tranche optimization rows."
49. Proposed text: "`restart/MASTER-PLAN.md` Tranche C optimization rows, plus
    D-tranche extension rows that consume generic-rule and rewrite-rejection
    outcomes."
50. Rationale: MASTER-PLAN puts bridge ownership in C.W4/C.W5
    (`restart/MASTER-PLAN.md:294-317`) and puts D on extension syntax
    (`restart/MASTER-PLAN.md:329-359`).

51. Refinement R11.
52. Target: `restart/MASTER-PLAN.md:698`.
53. Current text: "Bridge tests, no fused hypergraph."
54. Proposed text: "Bridge tests, no fused hypergraph, representative-stability
    test, and bridge-justification round-trip."
55. Rationale: S2/S3 warn against early representative choice; S7 requires proof
    and explanation surfaces.

56. Refinement R12.
57. Target: `restart/ARCHITECTURE.md:766-784`.
58. Current text: pipeline shows egraph rewrite before CSP solve.
59. Proposed text: keep order, but add a note: "CSP may have produced layout
    facts earlier inside `passes::layout`; the C.W4 bridge uses public solved
    or narrowed facts only, and does not expose layout-internal `TypeFacts`."
60. Rationale: V4 MASTER-PLAN hardening already reconciles layout-internal CSP
    with public cost-extraction CSP (`restart/audit/hardening/HARDENING-MASTER-PLAN-V4.md:111-113`).

## §6 — Adversarial findings

1. Finding A1 - representative promotion is unstable.
2. Contradicted or weakened lock: Lock 4 survives, but README bridge wording at
   `restart/README.md:223` is too strong.
3. SOTA evidence: S2/S3 make extraction the choice point; egraphs represent
   equivalent alternatives and IDs/canonicalization are internal.
4. SOTA evidence: S8 Cranelift allows Union nodes only during the egraph pass
   and checks none remain afterward.
5. Proposed amendment: replace "representative becomes CSP value" with
   stable-ID solved facts and extraction-time e-node selection.
6. Receiving phase: Phase 2 fold into README §6, ARCH §6, MASTER C.W4/C.W5.
7. Acceptance gate: a test where an e-class grows after a CSP fact is recorded
   still extracts the same legal Backend IR and never reads a stale
   representative.

8. Finding A2 - Lock 4 lacks the egglog counterargument.
9. Contradicted or weakened lock: Lock 4's "No unified hypergraph" is not
   contradicted, but it is under-argued against S5.
10. SOTA evidence: S5 egglog unifies Datalog and EqSat, then reports faster,
    simpler systems that fix bugs in the original systems.
11. Proposed amendment: add a short rationale: bbnf V1 rejects fusion because
    CSP, egraph, miners, and cost model have separate crate APIs, separate
    diagnostics, and separate stabilization gates; egglog-style fusion remains
    a post-V1 research comparison, not V1 architecture.
12. Receiving phase: Phase 2 fold into Lock 4 explication and ARCH §0/§6.
13. Acceptance gate: Lock 4 text names egglog as known SOTA and states the
    reason for bridge tables without dismissing the fusion design.

14. Finding A3 - seven rewrite categories need budget gates.
15. Contradicted or weakened lock: Topic 4's all-seven V1 claim is too weak as
    an implementation gate.
16. SOTA evidence: S1 bounds non-termination; S3 has iteration, node, and time
    limits; S10 warns that bad rewrite orientation can loop.
17. Proposed amendment: each rewrite category must carry guard, monotonicity,
    scheduler, node-growth, and fixture-budget evidence.
18. Receiving phase: Phase 2 fold into README §6 and MASTER C.W4 hard close.
19. Acceptance gate: `cargo test -p passes egraph_budget` includes at least one
    adversarial grammar per category and records node/iteration bounds.

20. Finding A4 - bridge facts need proof/explanation payloads.
21. Contradicted or weakened lock: Lock 4 and the explanation APIs are too weak
    unless bridge output preserves justification.
22. SOTA evidence: S7 says equality saturation engines need proof certificates
    for verification and conflict-clause generation, and implements certifying
    equality saturation in egg.
23. Proposed amendment: `BridgeJustification` is a first-class bridge output,
    and extraction emits a proof/explanation reference with each selected
    alternative.
24. Receiving phase: Phase 2 fold into ARCH public/private tables, MASTER C.W4,
    and PASS bridge rows.
25. Acceptance gate: a denied rewrite and an accepted rewrite both produce
    user-facing diagnostic provenance without exposing BBNF bridge terms from
    the generic egraph crate.

26. Finding A5 - source catalogue provenance gap.
27. Contradicted or weakened lock: none; this is research-source provenance.
28. SOTA evidence: the verified primary egglog source is Zhang et al. 2023 PLDI
    "Better Together"; no primary source was verified for exact title/authors
    "Yang et al. 2024, `egglog: Equality Saturation Meets Datalog`."
29. Proposed amendment: correct the research index source row to Zhang et al.
    2023, or add the precise Yang 2024 primary reference if a fold worker
    verifies it.
30. Receiving phase: Phase 2 research-index cleanup.
31. Acceptance gate: `rg -n "Yang et al. 2024|Better Together|egglog"`
    classifies the source row and records the canonical DOI or verified URL.

## §7 — Surgery proposals

1. Surgery S1 - README bridge wording.
2. Target: `restart/README.md:221-227`.
3. Directive: replace the three bridge bullets with stable-ID/fact wording:
4. "`passes/csp_egraph_bridge.rs` maintains stable maps among Grammar IR node
   IDs, e-class IDs, and CSP variables. CSP solutions publish solved-domain
   facts; egraph rewrites publish e-class facts and extraction candidates. The
   bridge does not commit to an e-node representative. `passes::extract`
   consults solved CSP facts when selecting an e-node and emits
   `BridgeJustification` with the selected `CostFacts`."
5. Acceptance gate: representative-stability test plus no stale
   "representative becomes the CSP's value" hit.
6. Dependency: §6 A1 and §5 R1/R2.

7. Surgery S2 - Architecture bridge invariant.
8. Target: `restart/ARCHITECTURE.md:799`.
9. Directive: change invariant to:
10. "Egraph and CSP exchange monotone facts through bridge tables; CSP search
    state remains in `csp-solver`; extraction consults solved assignments."
11. Acceptance gate: `rg -n "monotone facts|CSP search state|solved assignments"
    restart/ARCHITECTURE.md`.
12. Dependency: §5 R3.

13. Surgery S3 - Bridge justification API.
14. Target: `restart/ARCHITECTURE.md:319-321`.
15. Directive: add a short row or paragraph defining `BridgeJustification` and
    routing proof refs through egraph/CSP explanation APIs.
16. Acceptance gate: bridge test asserts an accepted rewrite and a denied guard
    both carry provenance.
17. Dependency: §6 A4 and §5 R4.

18. Surgery S4 - MASTER C.W4 scope.
19. Target: `restart/MASTER-PLAN.md:316`.
20. Directive: expand C.W4 from generic bridge tables to stable ID map,
    monotone fact exchange, rewrite guard API, budget policy, and
    justification records.
21. Acceptance gate: C.W4 hard-close command includes bridge facts, guard tests,
    and representative-stability test.
22. Dependency: §5 R5 and §6 A1/A4.

23. Surgery S5 - MASTER C.W5 extraction scope.
24. Target: `restart/MASTER-PLAN.md:317`.
25. Directive: add CSP-consulted legality, extraction constraints, and bridge
    justifications to the CostFacts/extraction skeleton row.
26. Acceptance gate: Backend IR builder receives selected alternatives plus
    proof refs and cost facts.
27. Dependency: §5 R6/R8/R9.

28. Surgery S6 - Rewrite budget gate.
29. Target: `restart/README.md:229-241`.
30. Directive: add a paragraph requiring every rewrite category to declare
    guards, monotonicity note, node-growth budget, scheduler profile, and
    extraction fixture.
31. Acceptance gate: `cargo test -p passes egraph_budget` or equivalent C.W4
    gate covers all seven categories.
32. Dependency: §6 A3 and §5 R7.

33. Surgery S7 - Cost model constraints.
34. Target: `restart/README.md:215-217` and `restart/ARCHITECTURE.md:54`.
35. Directive: preserve the shared `Cost` trait, then add
    `ExtractionConstraint` facts beside it for global well-formedness and
    legality constraints.
36. Acceptance gate: cost-model API docs distinguish local score/branch costs
    from global extraction constraints.
37. Dependency: §4 D8 and §5 R8/R9.

38. Surgery S8 - Lock 4 rationale.
39. Target: `restart/locks/14-LOCKS.md:40` or Architecture opening table at
    `restart/ARCHITECTURE.md:29-30`.
40. Directive: add one sentence: "egglog demonstrates that Datalog/EqSat fusion
    is viable; bbnf V1 keeps bridge tables because CSP, egraph, miners, and
    cost model have separate crate APIs, diagnostic ownership, and stabilization
    gates."
41. Acceptance gate: Lock 4 mentions egglog or the same rationale appears in
    ARCH §0 with path:line citation.
42. Dependency: §6 A2.

43. Surgery S9 - Topic catalogue correction.
44. Target: `restart/research/INDEX.md:82`.
45. Directive: replace "D-tranche optimization rows" with "Tranche C
    optimization rows, plus D-tranche extension rows that consume generic-rule
    and rewrite-rejection outcomes."
46. Acceptance gate: Topic 4 source row points to C.W4/C.W5 for bridge surgery.
47. Dependency: §4 D7 and §5 R10.

48. Surgery S10 - egglog source correction.
49. Target: `restart/research/INDEX.md:87`.
50. Directive: replace the unverified "Yang et al. 2024" wording with "Zhang et
    al. 2023, `Better Together: Unifying Datalog and Equality Saturation`,
    PLDI/PACMPL, DOI 10.1145/3591239", unless a later worker verifies the
    exact Yang 2024 source.
51. Acceptance gate: source row includes DOI or verified URL.
52. Dependency: §6 A5.

53. Surgery S11 - Lock ownership close proof.
54. Target: `restart/MASTER-PLAN.md:698`.
55. Directive: expand close proof to "Bridge tests, no fused hypergraph,
    representative-stability test, rewrite-budget test, and
    bridge-justification round-trip."
56. Acceptance gate: MASTER lock-ownership row names every C.W4/C.W5 bridge
    pressure point.
57. Dependency: §5 R11 and §6 A1/A3/A4.

58. Surgery S12 - PASS bridge fold.
59. Target: PASS-1 bridge/cost rows consumed by C.W4/C.W5, starting from
    `restart/audit/pass-1-substrate/PASS-1.md:31-52`.
60. Directive: add a PASS-side receiver note that recognizer/egraph/cost
    consumers read Grammar IR stable keys and must not leak lowerer policy into
    Grammar IR.
61. Acceptance gate: PASS bridge row names stable keys and rejects e-node
    representative leakage.
62. Dependency: §5 R2 and §6 A1.

63. Routed residue:
64. The Cranelift source is verified enough for production-shape evidence, but
    no line in the restart currently asks bbnf to adopt aegraph. Keep Cranelift
    as a pass-local erasure and budget model only.
65. The Lean `simp` source is useful for rewrite hygiene and loop avoidance. It
    is insufficient to justify replacing equality saturation with oriented
    simplification.
66. The egglog source row needs Phase 2 provenance cleanup because the verified
    primary paper differs from the catalogue label.
67. No source contradicts Lock 4 so strongly that bridge tables must be
    abandoned in V1. The lock survives if the fold adds the rationale and
    representative-stability surgery above.
