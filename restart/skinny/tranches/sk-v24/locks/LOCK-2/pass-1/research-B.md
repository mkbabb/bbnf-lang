# LOCK-2 / Family 2B — IR-IS-EGRAPH (research, pass 1)

**Charter.** The middle IR *is* an e-graph, end to end. There is no `GrammarIr → BackendIr`
transposition tower: one `EGraph<BbnfNode>` begins holding grammar terms and ends holding
backend terms **in the same e-classes**. Passes are rewrite groups. Optimization is equality
saturation under a schedule. Extraction, cost-guided, projects the winning representatives out —
and the per-rule `BackendShape` falls out of *which* e-node won at each rule root. Lowering is
not a separate program; it is elaboration of the same graph.

This is the maximal reading of the lock: not islands at decision points, not a CSP that treats
the e-graph as a candidate generator — the e-graph is the compiler's mid-end IR itself.

---

## 1. The mechanism, made concrete

### 1.1 The unified node language `BbnfNode`

Today the middle is two flat towers. The grammar term layer is
`ir::ExprKind` (`ir/src/lib.rs:234`) — `Seq`, `Alt{branches,mode}`, `Repeat{body,min,max}`,
`Optional`, `Literal{bytes,case}`, `Regex{pattern}`, `Ref{name,target}`, `Annotation`,
`Project{body,target,ty}`, `Discard{left,right,keep_left}` — held as a `Vec<Expr>` indexed by
`ExprId` (an arena of *subtree indices*, not e-classes). The backend layer is
`ir::BackendExpr` (`ir/src/lib.rs:376`) — `Entry`, `Seq`, `Alt`, `RepeatLoop`, `OptionalBranch`,
`ByteLiteral`, `RegexProgram{pattern,span_kind}`, `CallRule`, `SpanMark`, `TapeEmit{kind}`,
`DirectBuild{shape,fields}`, `ValueProject`, `Return` — plus `Recognizer::SimdScan{mode,
alphabet,site}` (`ir/src/lib.rs:416`) held as parallel `Vec<BackendRule>` + facts.

Family 2B collapses both into **one** e-node enum whose child fields are `Id` (e-class
references), deriving `egraph::Language` mechanically:

```rust
#[derive(Clone, Eq, PartialEq, Hash, egraph_derive::Language)]
enum BbnfNode {
    // grammar-term layer (children are Ids, not ExprIds)
    Seq(Box<[Id]>), Alt { mode: AltMode, branches: Box<[Id]> },
    Repeat { body: Id, min: u32, max: Option<u32> }, Optional(Id),
    Literal(LitKey), Regex(RegexKey), Ref(RuleRef), Project { body: Id, tgt: SymId },
    Discard { left: Id, right: Id, keep_left: bool },
    // backend-term layer — lowered forms live in the SAME classes
    Entry(Id), CallRule(RuleRef), ByteLiteral(BytesKey),
    RegexProgram { body: Id, span: SpanKind }, SpanMark { kind: SpanMarkKind, body: Id },
    TapeEmit { kind: TapeKind, body: Id }, DirectBuild { shape: SymId, fields: Box<[Id]> },
    SimdScan { mode: SimdMode, alphabet: AlphaId, site: SimdSite, body: Id },
    MemberScan { set: AlphaId, body: Id }, MaskScan { mask: u128, body: Id }, // residual pair
    Return,
}
```

The derive projects every `Id`/`Box<[Id]>`/named-`Id` field into `children()` automatically
(the auto-detection path proven in `crates/egraph/tests/derive_language.rs`). Non-`Id` scalars
(`min`, `mask`, `AltMode`, interned keys) are the *operator payload* and participate in
`Hash`/`Eq`, so `MaskScan{mask=0x2800…}` and `MaskScan{mask=0x0800…}` are distinct operators —
exactly the discrimination PROVE-12 needs.

Interned keys (`LitKey`, `SymId`, `AlphaId`, `BytesKey`) are the crux of a subtle law:
**spans and rule names do not live in the e-node**. Structural sharing (hashcons) must merge
two identical `Literal b";"` occurrences into one class for free CSE, but a `SourceSpan` in the
node would defeat that merge, and diagnostics (constitution, LOCK-1C) still need per-occurrence
spans. Resolution: the node carries an interned *key*; spans live in a side-table keyed by the
originating occurrence, not by class. This is the span/CSE tension named in §5.

### 1.2 Passes become rewrite groups

Every extant pass is re-expressed as a `RewriteFn<BbnfNode, EClassFacts>` group
(`egraph::Rewrite`, imperative `search`/`should_apply`/`apply` — `crates/egraph/src/rewrite.rs`),
scheduled in phases:

- **Canonicalization ruleset** — algebraic normal forms that make later matching total:
  `Seq` associativity/flattening, `Alt` flatten + dedup, `Optional(x) ≡ Alt(ε, x)`, `Repeat`
  normal forms, `Discard` orientation. Pure equivalences (add, never remove), so the source
  form survives for cost comparison.
- **Lowering ruleset** — the grammar→backend elaboration, one rewrite per current lowering
  rule. `Ref → CallRule`; `Literal → ByteLiteral`; a dispatch `Alt` → dispatch-arm form; the
  first string/number terminal → `SpanMark`/`RegexProgram{span}`; a container rule →
  `TapeEmit`; a `Project` chain over a product → `DirectBuild`. Each rewrite **adds** the
  backend e-node into the *same class* as its grammar-term source, so the class now holds both
  "the grammar meaning" and "one legal backend realization." The realization is chosen by cost,
  not by the rule.
- **Recognizer ruleset** — `derive_recognizers` (`passes/src/lib.rs:338`) becomes a rewrite
  `DeriveSimdScan` that fires on self-loop scan classes and adds a `SimdScan`/`MemberScan`
  e-node. `SimdMode` (`ir/src/lib.rs:425`, today only `Exact`) gains `Prefilter` as a *rewrite
  target*, not an enum edit threaded by hand — the prefilter seam (GN-2.W1) becomes "a rewrite
  fired, extraction picked it."
- **Residualization ruleset** — PROVE-12 generalized. The const-fold that today mutates the CSS
  template (`codegen/src/runtime_generator.rs`, `find_component_delim` → const-generic `u128`
  mask) becomes the rewrite `ConstFoldDelimMask`: `MemberScan{set}` ⇒ add `MaskScan{mask =
  delim_mask(set)}` in the same class. Extraction prefers `MaskScan` by cost. This is precisely
  SPEC GN-5.W4 ("12's const-mask generalized to every grammar-derived stop-set") realized as
  saturation rather than a per-site template edit.

`should_apply` (`rewrite.rs`, the AA.5 guard) reads `EClassFacts` — the analysis lattice — so a
lowering rewrite can abort when the first-set already shows a tight dispatch, avoiding graph
bloat.

### 1.3 Saturation under a schedule

Drive to fixpoint with `CspScheduler` (`crates/egraph/src/csp_scheduler.rs`): each e-class is a
CSP variable with a monotone `DirtyDomain`; each parent edge a `ParentDirtyProp` constraint; one
`csp.propagate()` (AC-3 over the csp-solver crate) computes the transitive dirty closure, and
`run_on_dirty` re-probes rules only on the dirty frontier. Phase ordering is a *ruleset schedule*
(canon → lower → recognizer → residual), each phase saturated before the next opens — the
`Scheduler` trait already sequences rule slices, and phasing keeps grammar-layer and backend-layer
rewrites from oscillating across the layer seam. Budgets come from `CostConfig`
(`egraph::CostConfig`, `egraph_iter_limit`/`egraph_node_limit`).

### 1.4 Extraction emits BackendShape directly

`Extractor` (`crates/egraph/src/extract.rs`, greedy bottom-up over a `CostModel<BbnfNode>`)
computes the cheapest representative per class. A `BackendCost` model — the generalization of
`backend_egraph::DecisionCostModel` — assigns each backend e-node a lattice cost
(`Lattice`, `extract.rs`) folding perf, capacity, and static-size axes. Grammar-layer nodes are
assigned ⊤ cost so they are always dominated out at the root; a class that never lowered (no
backend rewrite fired) extracts to nothing → a hard diagnostic (fail-closed, mirroring
`codegen/src/lib.rs:114`).

`BackendShape` (`ir/src/lib.rs:361`: `EagerTape|OffsetTape|EventTape|SinkOnly|CollapsedStage`)
is then **read off the extracted rule-root e-node**: a `DirectBuild` spine with no surviving
`TapeEmit` ⇒ `SinkOnly`; surviving `TapeEmit` ⇒ `OffsetTape`/`EagerTape` by tape-kind; a
collapsed-stage marker ⇒ `CollapsedStage`. The hand-written candidate enumerator + argmin
(`passes/src/lib.rs:398` `derive_backend_shape`, `:504` `backend_egraph::select`, the whole 311-LOC
`backend_egraph.rs`) is *deleted*; shape becomes an extraction readout. The extracted `BbnfNode`
tree is what `codegen::lower::lower_to_rust` (`codegen/src/lib.rs:176`) consumes — the same-wave
consumer (Y.13 satisfied by construction).

---

## 2. The substrate

**Already in-tree and production-grade** (`crates/egraph/`, ~1490 LOC): `Language` + derive
(`egraph-derive`, auto-projects recursive fields to `Id`); `EGraph` with FxHashed hashcons,
union-find, deferred `rebuild`, `with_capacity` (`egraph.rs`); `Analysis` monotone lattice
(`analysis.rs`); imperative `Rewrite`/`RewriteFn` with dirty-frontier `run_on_dirty` and the
`should_apply` analysis guard (`rewrite.rs`); `BackoffScheduler` + `CspScheduler` (`scheduler.rs`,
`csp_scheduler.rs`) — the latter already wired to the `csp-solver` AC-3 `LatticeDomain`;
`Extractor` greedy bottom-up (`extract.rs`); `CostModel` + `Lattice` + `Scalar` multi-objective
seam; `CostConfig`/`CostWeights::CALIBRATED_WEIGHTS` (`cost_config.rs`, `cost_weights.rs`). The
csp-solver crate (`crates/csp-solver/`) supplies AC-3 (`solver/ac3.rs`), search, and lattice
domains.

**Net-new for 2B:** (a) `BbnfNode` in `ir/` deriving `Language`, subsuming `ExprKind` +
`BackendExpr` + `Recognizer`; (b) `intern_grammar(&GrammarIr) -> (EGraph, root_ids)` translating
the `ExprId` arena into interned e-classes (identical subterms hashcons → automatic CSE the flat
arena never had); (c) the four rewrite rulesets (canon, lower, recognizer, residual) as
`RewriteFn`s; (d) `EClassFacts: Analysis<BbnfNode>` carrying `first_set`/`nullable`/`span_kind`
(the `rewrite.rs` docs already anticipate `EClassFacts::first_set`); (e) `BackendCost: CostModel`
generalizing `DecisionCostModel`; (f) `read_shape(&BbnfNode) -> BackendShape`; (g) a
`ShapeReadout` that walks the extracted tree to the per-`RuleId` `BackendShape` map.

---

## 3. Integration seams (path:line where extant code serves)

- `ir/src/lib.rs:234` `ExprKind`, `:376` `BackendExpr`, `:416` `Recognizer`, `:425` `SimdMode` →
  fold into one `BbnfNode: Language`. `BackendShape` (`:361`) is retained as the extraction
  *readout* type, not a candidate enum.
- `passes/src/lib.rs:398` `derive_backend_shape` + `:504` `backend_egraph::select` + the whole
  `passes/src/backend_egraph.rs` (311 LOC) → one `saturate + extract + read_shape` call. The
  existing `DecisionCostModel`/`DecisionNode`/`NormalizeDirectSinkCost` in that file are the
  *seed* for `BackendCost` + the direct-sink normalization rewrite, promoted from a per-decision
  island to whole-IR scope.
- `passes/src/lib.rs:338` `derive_recognizers` → the `DeriveSimdScan` rewrite; the
  `SimdMode::Prefilter` extension (GN-2.W1) becomes a rewrite target + a cost coefficient.
- `codegen/src/runtime_generator.rs` (`find_component_delim` const-mask, PROVE-12) → the
  `ConstFoldDelimMask` rewrite; the `delim_mask` const fn moves in verbatim as the rewrite's
  payload computation.
- `ir/src/cost.rs` `ActiveCostFacts` (`:173`), `DecisionCspFacts` (`:203`), `PriorityStep`
  (`:264`) → the provenance record extraction emits; `CostFacts::for_backend_shape` (`:119`) →
  seeds `BackendCost` per-shape base costs.
- Extraction output → `codegen::lower::lower_to_rust` (`codegen/src/lib.rs:176`) consumes the
  extracted `BbnfNode` tree; `emit_from_source` (`lib.rs:101`) pipeline unchanged downstream of
  the middle.
- `CspScheduler` is documented isomorphic to the grammar tier's `csp-solver` usage in
  `passes` (`csp_scheduler.rs` header) — the wiring already rhymes.
- **Constitution ii bonus:** `egraph/src/lib.rs` states both grammar IR *and* regex HIR
  implement `Language` over this one infrastructure. With `BbnfNode` as the single middle
  language, a regex fragment is a `Regex`/`RegexProgram` sub-term *in the same e-graph* — the
  bbnf↔regex isomorphism (constitution ii, LOCK-5 ONE-HIR) becomes structural sharing, not a
  cross-crate functor.

---

## 4. Prior art (citations)

- **Equality saturation as the optimization model** — Tate, Stepp, Tatlock, Lerner, *"Equality
  Saturation: a New Approach to Optimization"* (POPL 2009). The literal shape of 2B: saturate,
  then extract the optimum. The mid-end that is an e-graph, not a sequence of destructive passes.
- **egg** — Willsey, Nandi, Wang, Flatt, Tatlock, Panchekha, *"egg: Fast and Extensible Equality
  Saturation"* (POPL 2021). E-class analysis, deferred rebuild, `Language`/`Analysis`/`Runner`/
  `Extractor` — the exact architecture `crates/egraph` mirrors.
- **egglog** — Zhang, Wang, Willsey, Tatlock et al., *"Better Together: Unifying Datalog and
  Equality Saturation"* (PLDI 2023). Rulesets + scheduling + the datalog view; the justification
  for phased rule groups and for expressing lowering as declarative rules.
- **Cranelift aegraph mid-end** — Chris Fallin, *"cranelift's egraph mid-end"* / the aegraph RFC.
  The closest *production* precedent for 2B's precise thesis: the compiler's mid-end IR *is* an
  (acyclic) e-graph; optimization = rewrite rules; lowering to backend = elaboration/extraction
  directly. Evidence that "IR-IS-EGRAPH" ships, and the source of the acyclicity discipline that
  bounds risk 2.
- **Denali** — Joshi, Nelson, Randall, *"Denali: A Goal-directed Superoptimizer"* (PLDI 2002).
  The ancestor of "extraction emits backend instructions" — e-graph + matching for optimal code
  generation.
- **Tensat / SPORES** — Yang et al. (MLSys 2021); Wang et al. (VLDB 2020). Equality saturation
  for graph rewriting with **ILP extraction** — the precedent *and* the warning that greedy
  bottom-up extraction is suboptimal when cost depends on sharing (see §5, risk 1).
- **MLIR** dialect-lowering-as-rewrite-patterns (PDL/PDLL). The analogy for cross-layer lowering
  rules living beside canonicalization in one framework.

---

## 5. Named risks & counterexamples

1. **BackendShape is caller-sensitive; greedy per-class extraction is not** (the make-or-break).
   `extract.rs` is greedy bottom-up and assumes a class has one best representative. But
   `admits_sink_only` (`passes/src/lib.rs:661`) shows shape legality depends on the *caller's
   target*. Counterexample: one shared sub-rule referenced by a `SinkOnly` caller and an
   `OffsetTape` caller — its class cannot simultaneously extract to "sink here, tape there."
   "Extraction emits BackendShape directly" presumes context-free per-class shape; the real
   decision is per-rule and context-dependent. Mitigations, all costly: context-split classes by
   caller shape (blowup), or retain a global shape CSP after extraction (concedes toward family
   2C), or ILP/beam extraction (Tensat). **This is the first thing a prototype must settle.**

2. **Cross-layer non-termination / node blowup.** One enum holding both grammar terms and backend
   terms invites rules firing across the layer seam; monotone-add means grammar nodes never leave,
   so the cost model must *strictly* dominate them at every root or saturation drifts toward
   `egraph_node_limit` (100k). Phasing mitigates but partly abandons "one saturation." Prototype
   must show grammar-layer nodes are always cost-dominated and the phase schedule reaches fixpoint
   inside the iteration budget.

3. **Cost coefficients must be measured, not asserted** (constitution vii; MEMORY actual-profiling).
   `DecisionCostModel` uses integer perf/capacity/size costs of unstated provenance. 2B is
   SPEC-ONLY unless `BackendCost` consumes a PMU-fitted fact table (the §0 cyc/B / ins/B tables,
   PROVE-12's 0.81–0.98× being one row). This is the 2D overlap 2B must at minimum *consume*.

4. **Determinism at regen** (constitution iii; clean-regen law). `compute_best` (`extract.rs`)
   updates on `cost < existing` with `FxHashMap` iteration order; float ties risk
   order-dependence at whole-IR scale. Must gate on regen bit-for-bit (the GN-1 gate) and break
   ties by a total order on interned keys, not on hash order.

5. **CSE merges spans and projections** (span/diagnostics tension). Hashcons merges identical
   subterms — desirable for CSE, hazardous for diagnostics: two structurally identical `Project`
   occurrences at different grammar spans must not collapse their `SourceSpan`s (LOCK-1C wants
   per-span failures). `Expr` carries `span` + `id` today (`ir/src/lib.rs:225`); putting span in
   the e-node Hash defeats CSE, omitting it loses attribution. Resolution (§1.1): interned keys
   in the node, spans in an occurrence-keyed side-table — must be prototyped end-to-end.

---

## 6. What a prototype must test to move this family

1. **Parity.** Define `BbnfNode`, `intern_grammar` the JSON `GrammarIr` into one
   `EGraph<BbnfNode>`, run canon→lower→recognizer→residual under `CspScheduler` to fixpoint,
   extract with `BackendCost`, `read_shape`, feed `lower_to_rust`. Gate: the emitted `generated.rs`
   is **byte-identical** to today's two-tower output (regen diff = 0). This proves the e-graph
   middle is behavior-preserving.

2. **The caller-sensitivity hazard (risk 1) head-on.** Construct/pick a grammar with a sub-rule
   shared by a sink caller and a tape caller; show whether greedy per-class extraction yields a
   legal per-rule `BackendShape` assignment or *requires* a post-extraction pass. This single
   experiment decides whether "extraction emits BackendShape directly" survives or must be
   amended to "extraction proposes; a bounded shape solve disposes."

3. **PROVE-12 as a rewrite.** Reproduce the CSS delimiter const-fold as `ConstFoldDelimMask`
   (rewrite adds `MaskScan{u128}`, extraction picks it), and show the same +18 generated LOC and
   0.81–0.98× css_canon cyc/B / −4.8% ins/B as the template mutation — residualization folded into
   saturation with zero perf loss and generalized to every grammar-derived stop-set.

4. **Compile-budget refutation test.** Measure whole-IR saturation on the largest grammar
   (CSS L4): e-node count, iterations, and compile-time delta vs today's linear passes, against
   `CostConfig` budgets. If whole-IR saturation blows the compile budget where an island
   (family-2A shape) would not, that is the measured refutation of 2B — report it as a named
   negative result, per the constitution.
