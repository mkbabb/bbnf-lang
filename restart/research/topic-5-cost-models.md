# Topic 5 - Cost models, Pareto extraction, and SMT-backed cost composition

Research scope: cost-model interfaces, e-graph extraction, Pareto-frontier
selection, symbolic and solver-backed cost composition, regex/parser cost
sharing, and downstream BIR/codegen/runtime consumers.

Source count: 9 primary or official sources, plus 2 provenance gaps.

Adversarial finding count: 6.

## §1 — Settled position in the restart

1. Output authority: each research artefact is `restart/research/<topic>.md`
   and is approximately 500-1000 lines (`restart/research/INDEX.md:5`).
2. Fold authority: Phase 2 absorbs §5 refinements and §7 surgery proposals
   into the existing trio and PASS surfaces (`restart/research/INDEX.md:5`).
3. Escalation authority: §6 adversarial findings may trigger Phase 2
   escalation when SOTA contradicts a settled lock
   (`restart/research/INDEX.md:5`).
4. Required reading authority: the research worker must read README, locks,
   Architecture, Master Plan, PASS surfaces, V4 hardening baseline, style
   precepts, and topic sources (`restart/research/INDEX.md:7-16`).
5. §1 contract: every settled claim engaged by the topic must be cited by
   path:line and rendered verbatim or near-verbatim
   (`restart/research/INDEX.md:18-24`).
6. §2 contract: the SOTA section uses primary sources, extracts load-bearing
   claims, experimental evidence, and tradeoffs, with 5-15 citations
   (`restart/research/INDEX.md:24`).
7. §3 contract: convergence cites both restart claim and SOTA evidence
   (`restart/research/INDEX.md:26`).
8. §4 contract: divergence names the departure and reason
   (`restart/research/INDEX.md:28`).
9. §5 contract: refinements give target file:line, current text, proposed
   text, and rationale with SOTA citation (`restart/research/INDEX.md:30`).
10. §6 contract: adversarial findings name contradicted lock, SOTA evidence,
    proposed amendment, and receiving phase (`restart/research/INDEX.md:32`).
11. §7 contract: surgery proposals name target file:line, directive,
    acceptance gate, and §5-vs-§6 dependency (`restart/research/INDEX.md:34`).
12. Topic authority: Topic 5 is "Cost models + Pareto extraction +
    SMT-backed cost composition" (`restart/research/INDEX.md:93`).
13. Topic lock pointer: Topic 5 says its anchors are "Lock 6 + Lock 7 (cost
    models; cost-model trait shared with regex)"
    (`restart/research/INDEX.md:95`).
14. Topic anchor authority: Topic 5 uses the same anchors as Topic 4 and
    specifically MASTER-PLAN D/E/F tranches that consume cost decisions
    (`restart/research/INDEX.md:95-97`).
15. Topic question authority: the restart commits to "cost-model trait shared
    with regex" and asks whether shared means same trait, different instances,
    one cost function, or another shape (`restart/research/INDEX.md:97`).
16. Topic source authority: the named source set includes SPORES, egg
    `Analysis`, SymPy, Almomany et al. if verifiable, LLVM CodeMetrics,
    Cranelift cost model, and Deb-style multi-objective optimization
    (`restart/research/INDEX.md:99-105`).
17. Adversarial obligation: every research agent must surface at least one
    §6 adversarial finding, even when the topic converges with SOTA
    (`restart/research/INDEX.md:149-153`).
18. Voice authority: research prose must stay calibrated, direct, path-cited,
    and free of placeholder wording (`restart/research/INDEX.md:155-157`).
19. README greenfield mandate: no quick solutions, no workarounds, no legacy
    code uncontested, no contrivance, and no overfitting
    (`restart/README.md:3-5`).
20. README internal identity: the internals are "CSP + e-graph + shape mining
    + cost model + bidirectional inference + grammar-derived everything"
    (`restart/README.md:5`).
21. README substrate non-derived list: e-graph, CSP, and cost-model substrates
    are explicitly not grammar-derived (`restart/README.md:15-23`).
22. README workspace crate claim: `cost-model` owns "Cost trait +
    per-construct/per-rule/per-path costs" (`restart/README.md:49`).
23. README regex location claim: the regex engine folds into `parse-that` and
    lives at `parse-that/regex/` until then (`restart/README.md:62`).
24. README dependency claim: `passes` depends on `egraph`, `csp-solver`,
    `cost-model`, and `ir` (`restart/README.md:66-84`).
25. README composition claim: optimization sister crates compose by
    output-piping per Lock 4 and are not fused into a unified hypergraph
    (`restart/README.md:92`).
26. README Grammar IR claim: Grammar IR is typed, cost-annotated, and
    shape-mined; it is the optimization domain (`restart/README.md:108-113`).
27. README side-table claim: optimized IR is Grammar IR with side tables,
    including cost annotations and shape-mining hints
    (`restart/README.md:113`).
28. README regex surface claim: Unicode class algebra is not a grammar-level
    BBNF surface; regex owns Unicode coverage (`restart/README.md:131-143`).
29. README auto-detection claim: Pratt operator chains, SIMD scanner
    opportunities, and PHF keyword sets emerge from grammar shape through
    cost-model decisions (`restart/README.md:180-182`).
30. README pipeline claim: each phase has explicit input and output IR; the
    egraph is the rewrite substrate, CSP the inference substrate, and the
    cost model picks emission shapes (`restart/README.md:186-189`).
31. README pass-order claim: cost-model extraction follows e-graph saturation
    and precedes lower-to-Backend-IR (`restart/README.md:190-207`).
32. README hybrid cost claim: local costs feed e-graph extraction, global rule
    costs feed the strategy resolver, and per-path costs handle Pratt LUT
    propagation (`restart/README.md:211-213`).
33. README trait claim: "`Cost` is a trait with `score(&self, ctx: &Context)
    -> u64` and `branches(&self) -> impl Iterator<Item = (Choice, u64)>`"
    (`restart/README.md:215-217`).
34. README shared-implementation claim: "The parser cost model implements;
    the regex cost model implements; the comparison logic lives in
    `cost-model`" (`restart/README.md:217`).
35. README regex bridge claim: bridging via `Cost` lets the parser know a
    regex scan is cheap or expensive without knowing regex internals
    (`restart/README.md:217`).
36. README CSP/e-graph claim: CSP and e-graph compose through an explicit
    bridge, with CSP variables mapped to e-graph class IDs
    (`restart/README.md:219-227`).
37. README bridge-status claim: the bridge is real architecture and the
    substrates stay separate (`restart/README.md:227`).
38. README shape-mining claim: hints carry weights, the cost model dampens by
    weight, and saturation is bounded (`restart/README.md:243-254`).
39. README Lock 4 carry claim: `passes` composes `egraph`, `csp-solver`, and
    `cost-model` by output-piping; the CSP/egraph union is bridged
    (`restart/README.md:386`).
40. README Lock 10 carry claim: shape miners honor automatic Pratt/SIMD
    selection, with no `@pratt` or `@simd` directives
    (`restart/README.md:392`).
41. README process claim: PASS-1 owns source, grammar, IR, passes, vm, host,
    cost-model, egraph, csp-solver, type system, BBNF extensions, and errors
    (`restart/README.md:406-410`).
42. README PASS-2 claim: PASS-2 owns codegen, runtime template, generated
    output, SIMD kernels, Pratt/SIMD auto-detection, and cost-model integration
    (`restart/README.md:408-410`).
43. README voice claim: concrete claims need path:line citations, no soft
    hedging, no placeholders, and no future without receiver
    (`restart/README.md:450-452`).
44. Lock 4 claim: CSP inference, e-graph rewriting, pattern miners, shape
    analysis, and cost model compose by output-piping with no unified
    hypergraph (`restart/locks/LOCKS.md:40`).
45. Lock 6 claim: xtask emits committed source artefacts; proc-macro codegen
    facades are faults (`restart/locks/LOCKS.md:44`).
46. Lock 7 claim: `crates/path/` is the consolidated path crate; the runtime
    cursor engine merges into it (`restart/locks/LOCKS.md:46`).
47. Lock 8 claim: every perf gate names a specific competitor number on a
    specific dataset on a specific platform (`restart/locks/LOCKS.md:48`).
48. Lock 10 claim: Pratt and SIMD are auto-detected; the cost model decides
    when SIMD overhead is worth dispatch cost (`restart/locks/LOCKS.md:52`).
49. Lock 14 claim: generic crates, including `egraph`, `csp-solver`,
    `bbnf-regex`, `parse-that`, and `simd-scan`, carry zero grammar-specific
    code (`restart/locks/LOCKS.md:60`).
50. Architecture conflict ledger claim: CSP, egraph, miners, and cost model
    compose by output piping; a fused global hypergraph is rejected
    (`restart/ARCHITECTURE.md:30`).
51. Architecture workspace claim: `cost-model` owns cost facts, SOTA profiles,
    extraction scoring, and generated LOC budgets
    (`restart/ARCHITECTURE.md:54`).
52. Architecture public API claim: `cost-model` exposes cost facts, profiles,
    SOTA gate schema, and generated LOC budget API while raw tuning internals
    stay private (`restart/ARCHITECTURE.md:315`).
53. Architecture pipeline claim: cost extraction follows CSP solve and
    precedes Backend IR (`restart/ARCHITECTURE.md:768-783`).
54. Architecture invariant claim: cost extraction selects from alternatives and
    introduces no new grammar semantics (`restart/ARCHITECTURE.md:791-802`).
55. Architecture Grammar IR claim: the `Regex` variant is owned by
    `parse-that/regex`, and `Annotation` can carry explicit cost/profile data
    (`restart/ARCHITECTURE.md:824-842`).
56. Architecture BIR claim: `RegexProgram`, `SimdScan`, and `PrattSpine` are
    Backend IR variants (`restart/ARCHITECTURE.md:879-903`).
57. Architecture BIR payload claim: `RegexProgram` calls the regex engine,
    `SimdScan` calls `simd-scan`, and `PrattSpine` is auto-detected only
    (`restart/ARCHITECTURE.md:919-922`).
58. Architecture BIR invariant claim: SIMD and Pratt are mined, not
    syntax-directed (`restart/ARCHITECTURE.md:965-973`).
59. Architecture side-table claim: `EGraphFacts`, `CspSolution`, and
    `CostFacts` are public side tables; CostFacts feed Backend IR extraction
    and benchmark reporting (`restart/ARCHITECTURE.md:985-993`).
60. Architecture diagnostic claim: `BBNF-PRATT-NOT-APPLIED` and
    `BBNF-SIMD-NOT-SELECTED` fire when cost model declines automatic choices
    (`restart/ARCHITECTURE.md:1021-1022`).
61. Architecture lowerer claim: generated code is Backend-IR-only and lowerers
    do not walk grammar directly (`restart/ARCHITECTURE.md:1220-1237`).
62. Architecture gate-owner claim: JSON perf gates involve `bbnf-bench`,
    `cost-model`, `runtime`, and `simd-scan`; generated LOC budget belongs to
    `cost-model::loc_budget` and `codegen::verify`
    (`restart/ARCHITECTURE.md:1249-1257`).
63. MASTER Tranche C claim: Grammar IR, side tables, and the optimization
    bridge feed Backend IR extraction (`restart/MASTER-PLAN.md:294-317`).
64. MASTER C.W5 claim: CostFacts and extraction skeleton close when Backend IR
    builder receives selected alternatives (`restart/MASTER-PLAN.md:310-318`).
65. MASTER Tranche D claim: regex Unicode stays in `parse-that/regex`
    (`restart/MASTER-PLAN.md:329-357`).
66. MASTER Tranche E claim: Backend IR and VM exist before production lowering
    (`restart/MASTER-PLAN.md:361-390`).
67. MASTER Tranche F claim: Rust lowerer emits committed runtime source and
    enforces a generated LOC budget (`restart/MASTER-PLAN.md:392-423`).
68. MASTER schema claim: recognizers feed recognizer mining and cost model;
    optimization profiles feed cost and SOTA gates (`restart/MASTER-PLAN.md:567-583`).
69. MASTER generated-LOC claim: F.W3-F.W5 and H/J carry concrete generated LOC
    budget gates (`restart/MASTER-PLAN.md:643-660`).
70. MASTER lock ownership claim: Lock 4 closes in C; Lock 8 closes in H/J;
    Lock 10 closes in C/H (`restart/MASTER-PLAN.md:691-708`).
71. PASS-1 cost verdict: trait-based scoring with SOTA gates and extraction
    evidence is KEEP (`restart/audit/pass-1-substrate/PASS-1.md:14`).
72. PASS-1 bridge claim: e-graph handles equivalence, CSP handles finite
    legality/choice, and cost scores legal alternatives
    (`restart/audit/pass-1-substrate/PASS-1.md:71-75`).
73. PASS-1 cost trait claim: the interface scores terminal, sequence,
    alternation, repetition, host call, layout, materialization, SIMD, Pratt,
    recovery, and generated-code pressure (`restart/audit/pass-1-substrate/PASS-1.md:75`).
74. PASS-1 crate layout claim: `cost-model` has `weights`, `score`,
    `evidence`, `profiles`, `sota`, and `tiebreak`
    (`restart/audit/pass-1-substrate/PASS-1.md:121-145`).
75. PASS-1 handoff claim: generated budget and SOTA scores need common
    evidence (`restart/audit/pass-1-substrate/PASS-1.md:157-164`).
76. PASS-2 BIR-boundary claim: PASS-1 produces BIR after cost extraction and
    lowerers consume BIR only (`restart/audit/pass-2-codegen/PASS-2.md:30-32`).
77. PASS-2 BIR variant claim: `AltDispatch`, `AltSpeculative`, `RegexDfa`,
    `PrattSpine`, and `SimdScan` are cost-relevant BIR nodes
    (`restart/audit/pass-2-codegen/PASS-2.md:50-79`).
78. PASS-2 refiner claim: PASS-2 may sharpen payload and add lower-time
    evidence but may not re-own Backend IR (`restart/audit/pass-2-codegen/PASS-2.md:83-96`).
79. PASS-2 threshold claim: SIMD selects only when structural alphabet and cost
    evidence beat scalar for expected input length
    (`restart/audit/pass-2-codegen/PASS-2.md:165-171`).
80. PASS-2 consumer gate claim: materialization cost table must be generated
    and documented through an executable gate
    (`restart/audit/pass-2-codegen/PASS-2.md:351-360`).
81. PASS-2 use claim: cost model trait and scores feed alt dispatch, PHF, SIMD,
    and Pratt choices (`restart/audit/pass-2-codegen/PASS-2.md:366-374`).
82. PASS-2 budget claim: generated Rust output starts from 168,750 LOC with a
    +2 percent ceiling (`restart/audit/pass-2-codegen/PASS-2.md:395-413`).
83. PASS-2 diagnostic claim: `BBNF-OPT001` and `BBNF-OPT002` are produced by
    cost-model decisions (`restart/audit/pass-2-codegen/PASS-2.md:535-546`).
84. PASS-3 consumer claim: materialization cost tables include field counts,
    payload arena bytes, and tape-token width per node kind
    (`restart/audit/pass-3-runtime/PASS-3.md:136-143`).
85. PASS-3 diagnostic claim: `BBNF-OPT001` and `BBNF-OPT002` explain automatic
    Pratt/SIMD fallback when the cost model declines
    (`restart/audit/pass-3-runtime/PASS-3.md:414-420`).
86. V4 carry baseline: every V4 hardening report returned READY, with zero open
    punch items and all V1/V3 conflicts closed
    (`restart/audit/hardening/HARDENING-CONSOLIDATED-V4.md:104-112`).
87. V4 voice baseline: path:line citations, receiver/blocker/gate triples, no
    quick solutions, no legacy code uncontested, and no overfitting held
    (`restart/audit/hardening/HARDENING-CONSOLIDATED-V4.md:114-118`).
88. V5 carry baseline: V5 is not a rollback of V4 structural closure; it found
    drift in formal fragments, diagnostics, citations, and examples
    (`restart/audit/hardening/HARDENING-CONSOLIDATED-V5.md:22-29`).
89. V5 intact-architecture claim: tape/direct, Backend IR ownership, layout
    vocabulary, generic crates, path names, and yaml onboarding remain intact
    (`restart/audit/hardening/HARDENING-CONSOLIDATED-V5.md:74-78`).
90. V5 diagnostic repair claim: recognizer diagnostics should explain automatic
    detection, cost-model rejection, grammar restructuring, and metadata
    disable-only language (`restart/audit/hardening/HARDENING-CONSOLIDATED-V5.md:131-159`).
91. V5.1 PASS claim: SIMD fallback now says cost evidence did not win and
    metadata cannot force SIMD (`restart/audit/hardening/HARDENING-PASS-1-PASS-2-V5.1.md:49-51`).
92. V5.1 PASS residue: cross-PASS line citations around diagnostics still carry
    actionable citation residue (`restart/audit/hardening/HARDENING-PASS-1-PASS-2-V5.1.md:55-56`).
93. V5.1 PASS-3 claim: recognizer directive drift is closed, and PASS-3
    optimizer diagnostic rows do not teach retired directives
    (`restart/audit/hardening/HARDENING-PASS-3-V5.1.md:45-52`).
94. V5.1 synthesis claim: README stale-positive cleanup is closed and rich
    Unicode is routed through `parse-that/regex`
    (`restart/audit/hardening/HARDENING-SYNTHESIS-V5.1.md:52-64`).
95. Style mandate: project writing is pragmatic, economical, clear, and
    calibrated (`docs/precepts/instructions/STYLE.md:3-16`).
96. Style guard: vague attribution, promotional warmth, outline-shaped closers,
    mechanical boldface, and title-case excess are forbidden
    (`docs/precepts/instructions/STYLE.md:58-80`).
97. Lessons research claim: research needs challenge before synthesis
    (`docs/precepts/instructions/LESSONS-LEARNED.md:38-45`).
98. Lessons contract claim: contracts need producer and consumer gates
    (`docs/precepts/instructions/LESSONS-LEARNED.md:74-80`).
99. Lessons commit claim: broad or history-relevant commits need bodies with
    why, what landed, evidence, and routed remainder
    (`docs/precepts/instructions/LESSONS-LEARNED.md:102-113`).
100. Lessons close-honesty claim: every close claim grounds in cited artefacts,
     and every residue names a destination
     (`docs/precepts/instructions/LESSONS-LEARNED.md:243-256`).

## §2 — SOTA literature deep-dive

### Source register

1. S1: Wang, Hutchison, Leang, Howe, and Suciu, "SPORES: Sum-Product
   Optimization via Relational Equality Saturation for Large Scale Linear
   Algebra," PVLDB 2020:
   <https://www.vldb.org/pvldb/vol13/p1919-wang.pdf>.
2. S2: egg official API documentation for `Analysis`, `CostFunction`,
   `Extractor`, `LpExtractor`, and `LpCostFunction`:
   <https://egraphs-good.github.io/egg/egg/trait.Analysis.html>,
   <https://egraphs-good.github.io/egg/egg/trait.CostFunction.html>,
   <https://egraphs-good.github.io/egg/egg/struct.Extractor.html>,
   <https://egraphs-good.github.io/egg/egg/struct.LpExtractor.html>,
   <https://egraphs-good.github.io/egg/egg/trait.LpCostFunction.html>.
3. S3: Meurer et al., "SymPy: symbolic computing in Python," PeerJ Computer
   Science 2017, DOI 10.7717/peerj-cs.103:
   <https://doi.org/10.7717/peerj-cs.103>.
4. S4: SymPy official rewriting and code-generation documentation:
   <https://docs.sympy.org/latest/modules/rewriting.html> and
   <https://docs.sympy.org/latest/modules/codegen.html>.
5. S5: LLVM official `CodeMetrics.cpp` source:
   <https://llvm.org/doxygen/CodeMetrics_8cpp_source.html>.
6. S6: Cranelift official e-graph cost source:
   <https://docs.wasmtime.dev/api/src/cranelift_codegen/egraph/cost.rs.html>.
7. S7: Bytecode Alliance official 2023 Cranelift article:
   <https://bytecodealliance.org/articles/wasmtime-and-cranelift-in-2023>.
8. S8: Z3 official guide, "Combining Objectives":
   <https://microsoft.github.io/z3guide/docs/optimization/combiningobjectives/>.
9. S9: Deb, "Multi-Objective Optimization Using Evolutionary Algorithms:
   An Introduction," KanGAL Report 2011003:
   <https://www.egr.msu.edu/~kdeb/papers/k2011003.pdf>.

Provenance gaps:

1. G1: The exact "Almomany et al. 2014, Cost-aware code motion in Java" source
   was not verified as a primary paper or canonical repository source. It is
   not used as evidence below.
2. G2: The exact "Deb 2014" source named in the topic was not verified. S9 is
   a Deb-authored canonical introduction from 2011 that cites Deb's 2001 Wiley
   book as the foundational text; it is used only for Pareto-front framing.

### S1 - SPORES

1. SPORES translates linear algebra into relational algebra, applies equality
   saturation, and extracts an optimized linear-algebra expression.
2. The relevant design point is cost-aware extraction after a broad equivalence
   search, not a hand-picked rewrite direction.
3. SPORES integrates with SystemML and uses SystemML metadata such as matrix
   dimensions and sparsity estimates.
4. Its evaluation asks whether relational equality saturation can derive
   hand-coded rewrites, find better optimizations, and keep compile overhead
   acceptable.
5. The paper reports derivation of all 84 SystemML sum-product rewrite rules.
6. It also reports new optimizations for ALS, MLR, and PNMF, including up to
   10x speedup beyond SystemML on ALS in its setup.
7. The load-bearing pressure is that the cheapest expression can be the
   opposite algebraic direction from another benchmark.
8. The optimizer therefore cannot bake a single local rewrite preference into
   its rule set.
9. The extraction cost must read facts such as sparsity, fusion availability,
   and common subexpressions.
10. SPORES also shows the cost of optimality: ILP extraction consumes most of
    the compile-time overhead in some configurations.
11. Its greedy extractor cut compile time without losing reported performance
    on the studied workloads.
12. Saturation convergence remained bounded by sampling and barriers in the
    workload, not by a universal guarantee.
13. The paper explicitly points to a future multi-objective optimizer for
    accuracy and runtime.
14. For bbnf, this supports the restart's e-graph plus cost extraction posture.
15. It also rejects any claim that a single `u64` local branch score is a
    complete extraction contract.

### S2 - egg official APIs

1. `Analysis<L>` attaches arbitrary `Data` to each e-class.
2. `Analysis::make` creates data for an e-node, and `Analysis::merge` joins
   data when e-classes merge.
3. The docs name constant folding as a common analysis use.
4. The example demonstrates analysis data driving new equality insertion
   through `modify`.
5. `CostFunction<L>` is the greedy-extraction interface.
6. It has an associated `Cost` type with `PartialOrd`, `Debug`, and `Clone`.
7. Its `cost` method receives an enode and a callback for child costs.
8. egg requires the cost function to be monotonic for proper extraction.
9. The docs warn that recursive occurrences can overflow size-like costs and
   recommend saturating arithmetic in that case.
10. `Extractor` finds a single cheapest `RecExpr` from an e-class under a
    `CostFunction`.
11. `LpExtractor` performs extraction through integer linear programming.
12. Its example contrasts tree extraction with DAG extraction: ILP can count
    common subexpressions once.
13. `LpCostFunction` exposes a per-node cost method that may inspect the
    e-graph.
14. For bbnf, the split is important: analysis facts, scalar cost functions,
    and solver-backed extraction are related interfaces with distinct duties.
15. A restart trait named only `score` and `branches` collapses those duties
    unless the fold names the missing layers.

### S3 - SymPy paper

1. Meurer et al. position SymPy as an extensible symbolic-computation system.
2. The paper's relevance is symbolic expression representation and
   transformation, rather than compiler extraction.
3. SymPy's evidence base is an open-source computer algebra system with broad
   submodules and a pure-Python architecture.
4. The paper supports a design in which costs and constraints can be expressed
   symbolically and then simplified or printed into executable forms.
5. For bbnf, SymPy is useful as a precedent for carrying cost expressions as
   symbolic objects before reducing them to a target-specific decision.
6. It supports symbolic cost expressions as an implementation device; grammar
   semantics stay outside that machinery.
7. The cost composition layer should therefore be a side-table/fact layer,
   rather than a new grammar syntax or a new optimized IR.

### S4 - SymPy rewriting and codegen docs

1. SymPy's `cse` function identifies common subexpressions before evaluation.
2. The docs expose replacements and reduced expressions as separate outputs.
3. The docs warn that some optional optimizations can be very slow on large
   expressions.
4. Code generation docs state that printers do not always print optimal code.
5. They also state that CSE is not automatically applied everywhere and should
   happen at codegen level or above.
6. `CodeBlock.cse()` returns a new code block with common subexpressions
   pulled out as assignments.
7. This matches bbnf's side-table and BIR approach: keep symbolic cost
   composition above lowerers, then emit concrete evidence.
8. It also supports an extraction record with replacements/reduced form, not a
   bare scalar score.

### S5 - LLVM CodeMetrics

1. LLVM's `CodeMetrics.cpp` implements code cost measurement utilities.
2. The implementation imports `TargetTransformInfo` and `InstructionCost`.
3. `analyzeBasicBlock` skips ephemeral values and handles calls specially.
4. Calls may be counted as inline candidates, recursion is marked, indirect
   branches affect duplicatability, vectors are counted, and instruction cost
   is obtained from target transform information.
5. The load-bearing claim is production cost is contextual.
6. It is not one global size constant per construct.
7. It consults target-specific hooks, control-flow facts, call properties, and
   duplication legality.
8. For bbnf, the cost context must include backend target, ISA/profile, and
   legality facts.
9. LLVM is a contrast source for Pareto extraction. It shows why a cost trait
   must not hide target context behind a unitless scalar.

### S6 - Cranelift e-graph cost source

1. Cranelift's e-graph cost source defines costs for e-graph representation.
2. It represents cost in a `u32` and states that the ordering should be
   meaningful while the single unit is arbitrary.
3. It begins with per-opcode costs and adds input costs.
4. It scales by loop nesting, with a cap because the representation has finite
   bits.
5. Arithmetic is saturating to preserve ordering.
6. A reserved maximum value acts as infinity.
7. The source is austere and practical: a heuristic order with saturation and
   sentinels.
8. For bbnf, Cranelift supports a scalar-extraction profile, but only as a
   particular target profile.
9. It does not support treating the restart's `u64` as the sole interface for
   all cost questions.

### S7 - Bytecode Alliance Cranelift article

1. The Bytecode Alliance article states that Cranelift introduced an acyclic
   e-graph mid-end and enabled it by default after parity and correctness work.
2. It describes e-graphs as representing equivalent expressions so the compiler
   can choose the least-cost version.
3. It says elaboration from e-graph back to CFG uses an explicit cost function
   and dynamic programming for minimum-cost extraction.
4. It also notes a second cost function encoded implicitly in lowering
   patterns.
5. The authors identify a future direction: combine these cost functions and
   lower directly from e-graph representation.
6. For bbnf, this is direct evidence that post-egraph cost composition can
   become multi-layered: e-graph extraction and lowerer selection can each have
   cost logic.
7. The restart's shared cost trait should therefore carry provenance and layer
   ownership, so extraction cost and lowering cost can compose without being
   accidentally unified.

### S8 - Z3 Optimize objectives

1. The Z3 guide says many optimization problems require multiple objectives.
2. Z3 defaults to lexicographic priority.
3. It exposes Pareto priority and independent "box" objectives.
4. This gives the restart an official SMT-backed vocabulary for composing
   parse throughput, generated size, compile time, allocation, and diagnostic
   quality costs.
5. It also gives the fold a caution: the objective combination mode is itself
   a policy decision.
6. A cost trait returning only `u64` has already chosen a scalarization.
7. The restart can still keep a scalar fast path if it records the profile and
   preserves enough vector data for Pareto or lexicographic re-evaluation.

### S9 - Deb multi-objective framing

1. Deb's report frames multi-objective optimization as simultaneous
   optimization of conflicting objectives.
2. It identifies trade-off optimal solutions as Pareto-optimal solutions.
3. It states that multi-objective problems naturally yield a set of
   Pareto-optimal solutions requiring later selection.
4. It gives a two-step principle: find non-dominated points near the front,
   then choose one using higher-level information.
5. It also warns that too many Pareto-optimal solutions can be impractical for
   decision making.
6. For bbnf, this supports a bounded Pareto frontier for extraction candidates,
   followed by profile-specific selection.
7. It does not require evolutionary algorithms in the compiler.
8. It only supplies the framing: keep a frontier when objectives conflict, then
   collapse with a policy and evidence.

### Operational answer to the shared-trait question

1. "Shared with regex" should mean one shared cost evidence interface and one
   comparison/extraction vocabulary.
2. It should not mean one monolithic cost function for parser BIR and regex
   internals.
3. It should not mean the parser reads regex NFA/DFA internals.
4. It should mean parser alternatives and regex alternatives both emit
   `CostCandidate` records into `cost-model`.
5. Each candidate carries a domain, stable id, legality state, objective vector,
   required facts, child links, and evidence.
6. The parser model implements the trait for Grammar IR/BIR alternatives.
7. The regex model implements the trait for regex HIR/automata alternatives.
8. Shared comparison logic lives in `cost-model` and can run scalar, lexicographic,
   Pareto, or solver-backed selection profiles.
9. Shared evidence means a parser decision can compare "scalar byte loop" with
   "regex prefilter plus DFA call" by reading a summarized regex candidate.
10. Domain-specific facts remain opaque across the boundary.
11. The single Rust trait should therefore have a shared output shape, not a
    shared implementation body.
12. A foldable sketch:

```rust
pub trait CostModel {
    type Candidate;
    type Facts;

    fn domain(&self) -> CostDomain;
    fn objectives(
        &self,
        candidate: &Self::Candidate,
        facts: &Self::Facts,
        ctx: &CostContext,
    ) -> CostVector;
    fn children<'a>(
        &'a self,
        candidate: &'a Self::Candidate,
    ) -> Box<dyn Iterator<Item = CostChild> + 'a>;
    fn legality(
        &self,
        candidate: &Self::Candidate,
        facts: &Self::Facts,
        ctx: &CostContext,
    ) -> CostLegality;
    fn evidence(
        &self,
        candidate: &Self::Candidate,
        score: &CostDecision,
    ) -> CostEvidence;
}
```

13. `CostVector` should include at least runtime estimate, compile-time cost,
    generated LOC/code size, allocation pressure, branch/setup overhead, and
    fallback risk.
14. `CostDecision` can include the scalarized score for fast extraction, but
    the vector and profile must survive into evidence.
15. `CostChild` must carry a stable node/e-class/BIR/regex-program identity so
    DAG extraction and common-subexpression accounting can work.
16. SMT-backed composition lowers legality and objective vectors into
    constraints and optimize goals, with objective mode recorded as weighted,
    lexicographic, Pareto, or box.

## §3 — Convergence points

1. Restart and SPORES converge on equality saturation followed by cost-aware
   extraction: README puts cost-model extraction after e-graph saturation
   (`restart/README.md:190-207`), and SPORES translates, saturates, and
   extracts through a cost model (S1).
2. Restart and SPORES converge on fact-sensitive cost: README says local,
   global, and per-path costs all feed decisions (`restart/README.md:211-213`);
   SPORES uses matrix dimensions, sparsity, fusion, and sharing facts (S1).
3. Restart and egg converge on analysis facts attached to e-classes:
   Architecture exposes `EGraphFacts` and `CostFacts`
   (`restart/ARCHITECTURE.md:985-993`), and egg's `Analysis` attaches data to
   e-classes with `make` and `merge` (S2).
4. Restart and egg converge on pluggable cost functions for extraction:
   PASS-1 names an `AnalysisCost`-style interface
   (`restart/audit/pass-1-substrate/PASS-1.md:75`), and egg's `CostFunction`
   supplies extraction cost (S2).
5. Restart and egg converge on the need for monotone/saturating costs:
   Cranelift-style saturated cost is compatible with PASS-2's SIMD threshold
   claims (`restart/audit/pass-2-codegen/PASS-2.md:165-171`), and egg warns
   about overflow in recursive cost functions (S2).
6. Restart and SymPy converge on symbolic composition before code emission:
   the restart keeps optimized data in side tables (`restart/README.md:113`),
   while SymPy exposes CSE/reduced expressions before code generation (S4).
7. Restart and LLVM converge on contextual cost: Architecture binds costs to
   target gates and owner crates (`restart/ARCHITECTURE.md:1249-1257`), and
   LLVM CodeMetrics consults target transform information and code properties
   (S5).
8. Restart and Cranelift converge on scalar heuristic extraction as a valid
   fast path: README's current `score -> u64` trait is a scalar path
   (`restart/README.md:215-217`), and Cranelift uses an ordered scalar cost
   with saturation and infinity (S6).
9. Restart and Bytecode Alliance converge on layered extraction/lowering cost:
   PASS-2 says cost scores feed alt dispatch, PHF, SIMD, and Pratt choices
   (`restart/audit/pass-2-codegen/PASS-2.md:366-374`), and Cranelift describes
   separate extraction and lowering cost logic (S7).
10. Restart and Z3 converge on the need for explicit objective policy:
    MASTER carries optimization profiles to cost and SOTA gates
    (`restart/MASTER-PLAN.md:567-583`), and Z3 exposes lexicographic, Pareto,
    and box objective modes (S8).
11. Restart and Deb converge on trade-off selection: generated LOC budgets and
    runtime gates are both first-class (`restart/MASTER-PLAN.md:643-660`),
    while Deb frames multi-objective optimization as a set of trade-off
    solutions followed by policy selection (S9).
12. Restart and V5.1 hardening converge on no force directives: Lock 10 says no
    `@pratt`/`@simd` (`restart/locks/LOCKS.md:52`), PASS-2 diagnostics now
    say cost evidence must win (`restart/audit/hardening/HARDENING-PASS-1-PASS-2-V5.1.md:49-51`),
    and production cost systems use measured/estimated evidence rather than
    author override (S5-S7).

## §4 — Divergence points

1. The README trait is too scalar for the topic title. It says `score(...) ->
   u64` (`restart/README.md:215-217`), while Z3 and Deb both treat
   multi-objective optimization as a policy-selected frontier (S8, S9).
2. The README branch iterator is too tree-shaped. It returns `(Choice, u64)`
   (`restart/README.md:215-217`), while egg's `LpExtractor` and SPORES both
   show DAG/shared-subexpression extraction where a local child sum can
   miscount sharing (S1, S2).
3. The restart currently conflates analysis, scoring, and extraction vocabulary.
   PASS-1 calls the interface `AnalysisCost`
   (`restart/audit/pass-1-substrate/PASS-1.md:75`), while egg keeps
   `Analysis`, `CostFunction`, and `LpCostFunction` separate (S2).
4. The restart's shared-regex claim lacks an operational shape. README says
   parser and regex both implement `Cost` (`restart/README.md:217`), but it
   does not specify the common evidence envelope, domain opacity, or stable ids.
5. The restart's cost context underspecifies target and profile. LLVM and
   Cranelift both show target/context-sensitive costs (S5, S6), while README's
   trait has an unnamed `Context` and a unitless score (`restart/README.md:215-217`).
6. The restart speaks of SMT-backed composition only in the topic title and CSP
   solver ownership. Architecture exposes `CspSolution` and `CostFacts`
   (`restart/ARCHITECTURE.md:985-993`), but it does not name an objective mode
   or Optimize lowering.
7. The restart's current cost trait cannot explain Pareto residue. Deb says a
   non-dominated set needs later choice (S9); restart evidence currently says
   selected/rejected alternatives are recorded
   (`restart/audit/pass-1-substrate/PASS-1.md:75`) but does not require a
   frontier or domination reason.
8. The source catalogue's lock pointer diverges from the active lock file.
   Topic 5 says Lock 6/7 are cost-related (`restart/research/INDEX.md:95`),
   while active Lock 6 is committed codegen and active Lock 7 is path crate
   consolidation (`restart/locks/LOCKS.md:44-46`).
9. The Almomany source remains a provenance gap. It must not support a dynamic
   cost-model claim until a primary citation is found.
10. The exact Deb 2014 pointer remains a provenance gap. The fold can cite S9
    for Pareto framing, but any "Deb 2014" wording should be replaced with the
    verified source or routed as bibliography cleanup.
11. Cranelift's official article describes future work to combine extraction
    and lowering cost functions (S7), while restart prose could be read as
    already having one comparison logic for all layers
    (`restart/README.md:217`).
12. SymPy docs show code printers are not automatically optimal and CSE belongs
    above or at codegen (S4); restart should keep symbolic cost composition in
    `passes::extract`/`cost-model`, not let runtime template code invent late
    cost rewrites.

## §5 — Refinements to fold

1. Target: `restart/README.md:215-218`.
   Current text: "`Cost` is a trait with `score(&self, ctx: &Context) -> u64`
   and `branches(&self) -> impl Iterator<Item = (Choice, u64)>`."
   Proposed text: "`CostModel` is a shared trait family whose implementations
   emit a `CostDecision` containing a scalar score, objective vector, legality,
   child links, and evidence. Parser and regex models implement the same
   evidence shape with domain-specific facts; comparison, scalarization,
   Pareto filtering, and SMT-backed objective composition live in
   `cost-model`."
   Rationale: egg separates analysis/scoring/LP extraction, Z3 exposes
   objective modes, and Deb frames Pareto trade-offs (S2, S8, S9).
2. Target: `restart/README.md:217`.
   Current text: "Bridging via `Cost` allows the parser to know 'this regex
   scan is X cheap' without knowing regex internals."
   Proposed text: "Regex contributes an opaque `RegexCostSummary` through the
   shared `CostDecision` envelope; parser extraction may compare that summary
   against scalar scanner and literal alternatives without importing regex HIR,
   NFA, DFA, or Unicode internals."
   Rationale: Lock 4/14 require output-piping and generic crates, while LLVM
   and Cranelift show contextual summaries are legitimate cost inputs (S5-S7).
3. Target: `restart/audit/pass-1-substrate/PASS-1.md:75`.
   Current text: "Cost model trait: an `AnalysisCost`-style interface scores
   terminal, sequence, alternation, repetition, host call, layout,
   materialization, SIMD, Pratt, recovery, and generated-code pressure."
   Proposed text: "Cost model API: `Analysis`-style e-class facts,
   `CostModel` objective scoring, and optional solver-backed extraction are
   separate layers. They share `CostDecision` evidence across terminal,
   sequence, alternation, repetition, host call, layout, materialization,
   SIMD, Pratt, recovery, regex, and generated-code pressure."
   Rationale: egg's API split is direct precedent (S2).
4. Target: `restart/audit/pass-1-substrate/PASS-1.md:143`.
   Current text: "`score/` the `AnalysisCost` trait and scorer; `evidence/`
   extraction logs of selected/rejected alternatives."
   Proposed text: "`score/` `CostModel` implementations and objective
   scalarizers; `frontier/` Pareto and lexicographic filtering; `solve/`
   SMT/ILP-backed composition adapters; `evidence/` selected, rejected, and
   dominated alternative logs."
   Rationale: SPORES, egg LP extraction, and Z3 objective modes all require a
   layer beyond one scalar scorer (S1, S2, S8).
5. Target: `restart/ARCHITECTURE.md:991-993`.
   Current text: "`CspSolution` ... Cost extraction, layout, host chain
   typing. ... `CostFacts` ... Backend IR extraction, benchmark report."
   Proposed text: "`CspSolution` feeds legality constraints into cost
   extraction; `CostFacts` stores `CostDecision` records, objective vectors,
   Pareto-front membership, scalarization profile, selected alternative, and
   dominated alternatives for Backend IR extraction and benchmark reporting."
   Rationale: SOTA cost composition needs legality plus objective evidence
   (S1, S8, S9).
6. Target: `restart/MASTER-PLAN.md:317`.
   Current text: "CostFacts and extraction skeleton. Backend IR builder
   receives selected alternatives."
   Proposed text: "CostFacts, objective profiles, Pareto-front extraction, and
   solver-backed composition skeleton. Backend IR builder receives selected
   alternatives plus evidence for rejected and dominated candidates."
   Rationale: selected-only handoff loses the evidence the research contract
   needs (S2, S9).
7. Target: `restart/audit/pass-2-codegen/PASS-2.md:56-57`.
   Current text: `AltDispatch` and `AltSpeculative` generation site is "cost
   model".
   Proposed text: generation site is "`passes::extract` using `CostDecision`
   evidence from `cost-model`."
   Rationale: codegen should consume BIR and evidence, not own extraction
   policy (S2, S7).
8. Target: `restart/audit/pass-2-codegen/PASS-2.md:170`.
   Current text: SIMD rejects when alphabet is Unicode-semantic, tiny, or
   scanner setup cost wins.
   Proposed text: SIMD rejects when the candidate is illegal for the target,
   the regex summary cannot expose a safe prefilter, the alphabet is too small,
   or the selected objective profile says setup/code-size cost dominates
   runtime gain.
   Rationale: LLVM/Cranelift target context and regex opacity both matter
   (S5-S7).
9. Target: `restart/audit/pass-3-runtime/PASS-3.md:141`.
   Current text: materialization cost table has field counts, payload arena
   bytes, and tape-token width.
   Proposed text: add selected objective profile, scalar score, objective
   vector, and domination reason for materialization alternatives.
   Rationale: consumer docs need enough evidence to explain why direct/tape
   alternatives were selected (S8, S9).
10. Target: `restart/research/INDEX.md:95`.
    Current text: "Lock 6 + Lock 7 (cost models; cost-model trait shared with
    regex)."
    Proposed text: "Lock 4 + Lock 8 + Lock 10 + Lock 14 pressure (cost
    models, SOTA gates, auto-detected recognizers, regex/parser genericity);
    active Locks 6 and 7 remain committed-codegen and path-crate locks."
    Rationale: active lock file lines 44-52 do not match the Topic 5 lock
    pointer.

## §6 — Adversarial findings

1. Finding A1 - scalar trait is too strong.
   Contradicted lock or settled claim: README's trait shape commits to
   `score -> u64` as the visible cost API (`restart/README.md:215-217`).
   SOTA evidence: Z3 supports lexicographic, Pareto, and box objective modes
   (S8), and Deb frames multi-objective problems as trade-off fronts requiring
   later selection (S9).
   Proposed amendment: keep a scalar fast path, but make `CostDecision`
   preserve objective vectors, profile, and Pareto status.
   Receiving phase: Phase 2 README/PASS-1 fold, then Tranche C `cost-model`
   API.
2. Finding A2 - branch iterator double-counts DAG sharing.
   Contradicted lock or settled claim: README's `branches() -> (Choice, u64)`
   shape is tree-local (`restart/README.md:215-217`), while PASS-1 promises
   extraction evidence (`restart/audit/pass-1-substrate/PASS-1.md:75`).
   SOTA evidence: egg `LpExtractor` counts common subexpressions once, and
   SPORES reports ILP/greedy extraction tradeoffs around shared expressions
   (S1, S2).
   Proposed amendment: add stable child identities and an optional
   solver-backed DAG extraction path to the shared cost shape.
   Receiving phase: Phase 2 PASS-1/PASS-2 fold, then C.W5.
3. Finding A3 - shared-with-regex can violate domain opacity.
   Contradicted lock or settled claim: README says parser can know regex scan
   cost without regex internals (`restart/README.md:217`), while Lock 4 and
   Lock 14 require output-piped, generic substrates
   (`restart/locks/LOCKS.md:40`, `restart/locks/LOCKS.md:60`).
   SOTA evidence: LLVM and Cranelift expose contextual costs through summaries
   and target hooks rather than one universal semantic body (S5-S7).
   Proposed amendment: define `RegexCostSummary` as an opaque contribution to
   `CostDecision`; parser code consumes only the summary.
   Receiving phase: Phase 2 README/Architecture fold, then D/H regex and SIMD
   gates.
4. Finding A4 - Topic 5 lock pointer is stale.
   Contradicted lock or settled claim: `restart/research/INDEX.md:95` maps
   Topic 5 to Lock 6/7 as cost locks, but active Lock 6 and Lock 7 are
   committed codegen and path-crate consolidation
   (`restart/locks/LOCKS.md:44-46`).
   SOTA evidence: none required; this is internal provenance pressure. It
   affects routing rather than cost theory.
   Proposed amendment: route Topic 5 to Lock 4, Lock 8, Lock 10, and Lock 14,
   with Lock 6 as downstream generated-evidence pressure only.
   Receiving phase: Phase 2 research-index cleanup.
5. Finding A5 - SMT-backed cost composition is under-specified.
   Contradicted lock or settled claim: Architecture exposes `CspSolution` and
   `CostFacts` (`restart/ARCHITECTURE.md:985-993`) but no objective mode,
   constraint lowering, or solver evidence.
   SOTA evidence: Z3 Optimize makes objective combination explicit (S8);
   SPORES shows solver extraction can dominate compile overhead (S1).
   Proposed amendment: add `ObjectiveMode::{Weighted, Lexicographic, Pareto,
   Box}` and record whether each decision used scalar, greedy, ILP, or SMT
   extraction.
   Receiving phase: Phase 2 Architecture/PASS-1 fold, then C.W5 hard gate.
6. Finding A6 - named source provenance gaps must not become evidence.
   Contradicted lock or settled claim: Topic 5 names Almomany et al. 2014 and
   Deb 2014-like material (`restart/research/INDEX.md:101-105`).
   SOTA evidence: the exact Almomany source and exact Deb 2014 source were not
   verified as primary/canonical sources in this pass.
   Proposed amendment: mark Almomany as provenance gap and replace exact Deb
   2014 wording with verified Deb source details or a bibliography TODO with a
   receiver.
   Receiving phase: Phase 2 research-index bibliography cleanup.

## §7 — Surgery proposals

1. Surgery S1.
   Target: `restart/README.md:215-218`.
   Directive: replace the scalar-only `Cost` paragraph with a shared
   `CostModel`/`CostDecision` paragraph that names scalar score, objective
   vector, legality, child links, evidence, parser implementation, regex
   implementation, and comparison logic in `cost-model`.
   Acceptance gate: `rg -n "CostDecision|objective vector|RegexCostSummary|Pareto" restart/README.md`
   returns the new terms, and the old `score(&self, ctx: &Context) -> u64`
   line is either gone or described as a fast-path method.
   Dependency: §6 A1/A3.
2. Surgery S2.
   Target: `restart/audit/pass-1-substrate/PASS-1.md:75`.
   Directive: split `AnalysisCost` into e-class analysis facts,
   `CostModel` scoring, and optional solver-backed extraction.
   Acceptance gate: `rg -n "Analysis.*CostModel|solver-backed|CostDecision" restart/audit/pass-1-substrate/PASS-1.md`.
   Dependency: §5 refinement 3 and §6 A2/A5.
3. Surgery S3.
   Target: `restart/audit/pass-1-substrate/PASS-1.md:143`.
   Directive: add `frontier/` and `solve/` to the `cost-model` child layout,
   and make `evidence/` include dominated alternatives.
   Acceptance gate: child-count remains within Lock 13's 4-10 range and
   `rg -n "frontier/|solve/|dominated" restart/audit/pass-1-substrate/PASS-1.md`.
   Dependency: §5 refinement 4.
4. Surgery S4.
   Target: `restart/ARCHITECTURE.md:985-993`.
   Directive: expand `CostFacts` and `CspSolution` rows with legality
   constraints, objective vectors, Pareto membership, profile, selected
   alternative, and domination reason.
   Acceptance gate: `rg -n "Pareto|objective vector|dominat|ObjectiveMode|CostDecision" restart/ARCHITECTURE.md`.
   Dependency: §6 A1/A5.
5. Surgery S5.
   Target: `restart/MASTER-PLAN.md:312-318`.
   Directive: change C.W5 from "CostFacts and extraction skeleton" to a
   concrete API/evidence wave for objective profiles, Pareto extraction, and
   solver-backed composition skeleton.
   Acceptance gate: C.W5 close gate names `cargo test -p cost-model facts
   frontier solve` or an equivalent split.
   Dependency: §5 refinement 6.
6. Surgery S6.
   Target: `restart/audit/pass-2-codegen/PASS-2.md:56-57`.
   Directive: replace generation site "cost model" with "`passes::extract`
   using `CostDecision` evidence from `cost-model`" for dispatch/speculation
   BIR nodes.
   Acceptance gate: `rg -n "passes::extract.*CostDecision|CostDecision.*AltDispatch" restart/audit/pass-2-codegen/PASS-2.md`.
   Dependency: §5 refinement 7.
7. Surgery S7.
   Target: `restart/audit/pass-2-codegen/PASS-2.md:165-171`.
   Directive: add target/profile-sensitive SIMD rejection language and regex
   prefilter opacity.
   Acceptance gate: PASS-2 SIMD row mentions selected objective profile,
   target legality, and opaque regex summary, with no force directive.
   Dependency: §5 refinement 8 and §6 A3.
8. Surgery S8.
   Target: `restart/audit/pass-3-runtime/PASS-3.md:136-143`.
   Directive: require materialization cost tables to include objective vector,
   scalarized score, selected profile, and domination reason.
   Acceptance gate: PASS-3 consumer gate still names
   `materialisation_cost.toml` or equivalent and now names objective evidence.
   Dependency: §5 refinement 9.
9. Surgery S9.
   Target: `restart/research/INDEX.md:95`.
   Directive: replace stale Lock 6/7 wording with active lock routing: Lock 4,
   Lock 8, Lock 10, Lock 14, plus downstream Lock 6 generated-evidence
   pressure.
   Acceptance gate: `rg -n "Topic 5|Lock 4|Lock 8|Lock 10|Lock 14" restart/research/INDEX.md`
   shows active lock mapping; `Lock 6 + Lock 7 (cost models` is gone.
   Dependency: §6 A4.
10. Surgery S10.
    Target: `restart/research/INDEX.md:101-105`.
    Directive: mark Almomany et al. as "provenance gap unless primary source
    is recovered" and replace exact Deb 2014 wording with the verified Deb
    source or a receiver-owned bibliography task.
    Acceptance gate: research index no longer asks workers to assert
    unverifiable sources as evidence.
    Dependency: §6 A6.
11. Surgery S11.
    Target: `restart/audit/pass-2-codegen/PASS-2.md:535-546` and
    `restart/audit/pass-3-runtime/PASS-3.md:414-420`.
    Directive: add objective-profile wording to `BBNF-OPT001` and
    `BBNF-OPT002`, so diagnostics can explain "runtime profile selected
    scalar" or "code-size profile rejected SIMD" without exposing directives.
    Acceptance gate: diagnostic rows mention cost profile or objective profile
    and still contain zero `@pratt` or `@simd` remediation text.
    Dependency: §5 refinements 8-9.
12. Surgery S12.
    Target: `restart/MASTER-PLAN.md:765-785`.
    Directive: add a carry row for cost evidence: receiver C/F/H/J, blocker
    "selected-only cost evidence loses dominated alternatives or profile
    provenance", gate "`cost-model` evidence report lists selected, rejected,
    dominated, objective mode, target, and profile."
    Acceptance gate: carry ledger names receiver, blocker, gate, and source.
    Dependency: §6 A1/A2/A5.

Closing position: the restart's architectural direction survives the SOTA pass
if "shared with regex" is folded as a shared cost evidence shape with
domain-specific implementations. The current scalar trait text is too narrow
and should be treated as a fast-path sketch, not the final API contract.
