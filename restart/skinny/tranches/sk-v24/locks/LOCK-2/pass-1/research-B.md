# LOCK-2 · Family 2B — IR-IS-EGRAPH (pass 1, research B)

**Thesis.** The middle IR is not a tree that an e-graph occasionally consults at one decision
point; the middle IR *is* an e-graph. `GrammarIr` interns once into a single `EGraph<IrNode,
IrFacts>`; every compile pass (nullability, first-set, recognizer derivation, backend-shape
selection, projection canonicalization, residualization) is a rewrite group or an `Analysis`
lattice over that graph; congruence closure gives grammar-level GVN for free; a scheduled
saturation reaches a fixed point; and a single cost-guided extraction emits the `BackendExpr`
tree *and* its per-rule `BackendShape` directly to the lowerer. Passes stop being an ordered
pipeline with a phase-ordering problem and become an unordered rewrite set closed under
equality.

This is the maximal reading of the LOCK-2 charter. Its strongest form is *not* naive unbounded
equality saturation — that is refuted below by the IR's own shape (variadic `Seq`/`Alt`,
mutually-recursive rules, a non-local size budget). Its strongest form is the **aegraph
discipline applied to the grammar plane**: one e-graph as the IR container, congruence closure
+ bounded confluent normal-forming rewrites applied largely on-insert, exploratory saturation
reserved for the few passes that provably pay, and extraction-as-lowering with a cost model
that is monotone by construction where greedy extraction is used and delegated to the CSP
knapsack where it is not.

---

## 1. The mechanism, made concrete

### 1.1 The node — an Id-child IR

The current middle IR cannot be interned as-is. `ir/src/lib.rs:234` `ExprKind` names its children
as `ExprId(pub usize)` (`ir/src/lib.rs:19`) — arena indices into `GrammarIr.exprs`, not e-graph
`Id`s. `BackendExpr` (`ir/src/lib.rs:376`) names its children as `Box<BackendExpr>` /
`Vec<BackendExpr>` — owned subtrees. The derive macro (`egraph-derive/src/lib.rs`) projects only
literal `Id` / `Vec<Id>` / `Box<[Id]>` / `[Id;N]` fields into `children()`; per its own doc
(`egraph-derive/src/lib.rs:7-9`) **`Box<Self>` and `Vec<Self>` are classified as leaves**, so
`BackendExpr` derives an *empty* child slice today and `ExprKind` cannot derive at all. This is
the load-bearing seam of family 2B: the e-graph node is a distinct **`IrNode`** whose recursive
positions are `Id`:

```rust
#[derive(Clone, PartialEq, Eq, Hash, egraph_derive::Language)]
enum IrNode {
    Seq(Vec<Id>),                         // variadic
    Alt { mode: AltMode, branches: Vec<Id> },
    Repeat { body: Id, min: u32, max: Option<u32> },
    Optional(Id),
    Literal { bytes: Vec<u8>, case: CaseSensitivity },   // leaf (metadata)
    Regex { pattern: String },                            // leaf
    Ref { target: RuleId },                               // leaf — the recursion edge
    Project { body: Id, target: Sym, ty: Option<Sym> },
    Discard { children: [Id; 2], keep_left: bool },       // scattered Ids → [Id;N] per derive law
}
```

Two design consequences fall out immediately, both mandatory:

- **`Ref` is a leaf, not an `Id` child.** A grammar is a *call graph* of mutually-recursive
  rules; `json.value → array → value` is a cycle. If `Ref` held an `Id` to the callee class, the
  e-graph would be cyclic and greedy extraction (`egraph/src/extract.rs:134`) would fail to reach
  recursive classes (`best_cost` returns `None`, `extract.rs:175`). Keeping `Ref{target:RuleId}`
  a leaf makes each **rule body** an acyclic e-graph whose recursion is a symbolic edge resolved
  by a separate per-rule root table (`RuleId → Id`). This is precisely the aegraph move —
  eclasses reference only earlier eclasses within a body; the recursion lives outside the
  congruence relation. It is the single most important structural decision in 2B.
- **`Discard`'s two `ExprId`s** violate the derive's "no scattered `Id` fields" rule
  (`egraph-derive/src/lib.rs:18-22`); they must be a `[Id;2]`. Trivial, but it is a real edit the
  derive forces, and it is the kind of thing that makes "the AST *is* the e-node" false in the
  small.

The reconciliation `ExprId (arena) ↔ Id (e-graph)` is a one-time interning walk: post-order over
`GrammarIr.exprs`, `graph.add` each node, memo `ExprId → Id`. Because `add` hash-conses
(`egraph/src/egraph.rs:155`), structurally-identical sub-grammars collapse to one class on the
way in — GVN with zero extra code. The interning walk is the sole owner of the arena→graph map;
after it, the arena is dead and the graph is the IR.

### 1.2 Passes as rewrite groups + analyses

The extant pipeline (`passes/src/lib.rs`) is a fixed sequence: `derive_recognizers`
(`passes/src/lib.rs:338`) → `derive_backend_shape` (`passes/src/lib.rs:398`) → the degenerate
selector e-graph (`passes/src/backend_egraph.rs`). 2B dissolves this into two populations over the
one graph:

- **`Analysis<IrNode>` lattices** (`egraph/src/analysis.rs`) carry the *facts* that are pure
  functions of a class: `nullable: bool` (join = OR, the fixed-point already hand-rolled at
  `ir/src/lib.rs:320` becomes `merge`), `first_set: ByteSet` (join = union), and a per-class
  `CostSummary`. Congruence maintenance runs these to fixed point for free — the hand-written
  nullability worklist (`ir/src/lib.rs:322-348`) is deleted, replaced by `A::merge` returning
  "changed" (`analysis.rs:35`), which is already the LLVM-`Changed` convergence pattern the memory
  law mandates.
- **`Rewrite<IrNode,IrFacts>` groups** (`egraph/src/rewrite.rs:34`) carry the *equalities*:
  `Optional(x) ≡ Repeat{x,0,1}`; single-branch `Alt` ≡ its branch; `Seq([x])` ≡ `x`; adjacent
  `Literal` fusion; the PROVE-12 residualization (`prototypes/PROVE-12/`) recast — the delimiter
  set `contains` collapses to a const bitmask *as a rewrite* `MembershipScan(set) ≡
  MaskScan(delim_mask(set))` guarded by `should_apply` (`rewrite.rs:58`), instead of a hand
  mutation of the `CSS_GENERATED_RS` template. Recognizer derivation becomes a rewrite that adds a
  `SimdScan` alternative into the class of a self-loop/`Regex` leaf, leaving the scalar form
  present so extraction can choose per cost (this is exactly what `SimdMode`/`SimdSite` at
  `ir/src/lib.rs:425-430` are for).

The scheduler is `CspScheduler` (`egraph/src/csp_scheduler.rs:175`): its dirty-frontier AC-3
closure (`ParentDirtyProp`, `csp_scheduler.rs:120`) is already the incremental engine, and its
`node_limit`/`iter_limit` (`csp_scheduler.rs:177`) are the growth guards that keep a variadic
grammar from exploding. Saturation is *bounded and confluent by rule choice*, not open-ended.

### 1.3 Extraction emits BackendShape directly

Extraction (`egraph/src/extract.rs`) is the lowering. The cost model (`CostModel<IrNode>`,
`extract.rs:85`) is fed by `ir/src/cost.rs` — `CostFacts` (`cost.rs:5`), the fitted
`ActiveCostFacts` (`cost.rs:173`), `PriorityStep` (`cost.rs:264`) — and returns the cheapest
`IrNode` per class. **`BackendShape` (`ir/src/lib.rs:361`) is chosen at the same extraction, per
rule root**, because the shape is a function of the extracted body's cost summary (retention
lifetime, `SubstrateTarget` at `cost.rs:57`, capacity). The greedy bottom-up extractor
(`extract.rs:134`) is *sound only for monotone, local cost*; it suffices for the per-node kernel
choice (scalar vs `SimdScan`, `MembershipScan` vs `MaskScan`) but **not** for the global
generated-size budget (a knapsack: choosing `SinkOnly` here may bust the LOC budget there). That
non-monotone slice is handed to the `csp-solver` knapsack, exactly the seam the existing
`DecisionCspFacts` (`cost.rs:203`) and the vendored `csp-solver` were built for. The extracted
`BackendExpr` tree is rebuilt from `extract_tree` (`extract.rs:190`) into the `Box`/`Vec` owned
form the lowerer (`codegen::lower::rust::lower_to_rust`, cited as the same-wave consumer at
`backend_egraph.rs:11`) already consumes.

---

## 2. The substrate that exists, and what it lacks

| Piece | State | 2B need |
|---|---|---|
| `EGraph<N,A>` core (`egraph/src/egraph.rs`) | complete: hashcons, union, deferred rebuild | ✓ ready; `with_capacity` (`egraph.rs:78`) already sized for small grammars |
| `Analysis` lattice (`egraph/src/analysis.rs`) | complete, `Changed`-returning `merge` | host nullable/first-set/cost as `IrFacts` |
| `Rewrite`/`RewriteFn` (`egraph/src/rewrite.rs`) | complete, imperative search/apply | author the IR rewrite groups |
| `CspScheduler` (`egraph/src/csp_scheduler.rs`) | complete dirty-frontier AC-3 | reuse verbatim as the saturation schedule |
| `Extractor` (`egraph/src/extract.rs`) | greedy bottom-up, monotone only | **extend**: non-monotone/global extraction via `csp-solver`; cyclic guard |
| `egraph-derive` (`egraph-derive/src/lib.rs`) | projects literal `Id` fields only | **extend** to project `Box<Self>`/`Vec<Self>` recursive positions to `Id`, or author `IrNode` by hand |
| current consumer (`passes/src/backend_egraph.rs`) | **degenerate**: 5 childless `DecisionNode`s, one rewrite, `[Id;0]` | subsumed — this is a consumer-less substrate in gestation (Y.13 risk) that 2B redeems by making the IR the graph |

The honest reading of `backend_egraph.rs`: the e-graph today holds a flat set of pre-ranked
`BackendShape` candidates with **zero children** (`backend_egraph.rs:158,176`) and applies a
single cosmetic normalization rewrite (`NormalizeDirectSinkCost`, `backend_egraph.rs:193`). It
uses none of the e-graph's actual power — no congruence, no sharing, no recursive structure. The
`lib.rs` doc-claim that "both grammar IR nodes and regex HIR nodes implement `Language`"
(`egraph/src/lib.rs:9-10`) is **aspirational, not realized** — the sole `impl Language` in the tree
is `DecisionNode` (`backend_egraph.rs:181`). Family 2B is precisely the work of making that claim
true: the grammar IR node *becomes* the `Language`.

---

## 3. Prior art (with the load-bearing citations)

- **Cranelift ægraphs** (Fallin, EGRAPHS 2023; cfallin.org/blog/2026/04/09/aegraph;
  bytecodealliance/rfcs `cranelift-egraph.md`). The production proof that "the mid-end IR *is* an
  e-graph" is viable *only* under discipline: acyclic (each eclass refers to earlier eclasses),
  rules applied on-insert rather than to a fixed point, GVN/LICM/rematerialization folded into the
  graph in a few linear passes. This is the template for 2B's strongest form and the direct source
  of the `Ref`-as-leaf decision (§1.1). Its explicit non-goal — *no* full equality saturation,
  *no* fixpoint loop — is the guardrail against the variadic/cyclic blowup below.
- **egg** (Willsey et al, POPL 2021, `10.1145/3434304`). The deferred-rebuild + e-class-analysis
  model the in-tree crate mirrors (`rebuild` at `egraph.rs:251`, `Analysis` at `analysis.rs`).
  egg's own guidance: keep associative/commutative operators *out* of naive saturation; use
  analyses to gate rewrites — both directly constrain 2B.
- **egglog** (Zhang et al, PLDI 2023). Datalog ∪ equality-saturation: cooperating analyses,
  lattice reasoning, and *extraction of optimized terms* as first-class. The model for treating
  nullability/first-set (analyses) and normalization (rewrites) as one closed fixpoint rather than
  ordered passes.
- **MLIR `eqsat` dialect** (arXiv 2505.09363) and **DialEgg** (2025). Non-destructive rewriting as
  an IR dialect and the phase-ordering cure — evidence that "the IR carries its own equalities" is
  an active, validated direction, and a caution that MLIR pays for it in graph size.
- **RVSDG / optir**. Regionalized value-state dependence graphs are the eqsat-friendly IR shape;
  relevant because BBNF's grammar is a dataflow of rule outputs, not a CFG — the RVSDG region
  boundary maps to the per-rule root of §1.1.
- **Extraction-as-ILP**: Tensat (MLSys 2021), SPORES, Denali (Joshi et al). The canon that
  *optimal* extraction under sharing/global cost is NP-hard and is solved by ILP/CSP — the
  authority for handing the size-budget knapsack to `csp-solver` rather than the greedy extractor.
- **Guided equality saturation** (Kœhler et al, POPL 2024). Sketch-guided saturation to tame
  blowup — the fallback if any 2B rewrite group proves non-confluent.

---

## 4. Named risks and counterexamples

1. **Variadic associative/commutative blowup.** `Seq(Vec<Id>)` and `Alt{branches:Vec<Id>}`
   (`ir/src/lib.rs:235-239`) are the classic e-graph trap: naive re-association/commutation
   rewrites over an N-ary sequence generate exponentially many equivalent groupings. *Mitigation
   and law for 2B:* AC operators are **normal-formed one-directionally** (flatten nested `Seq`,
   sort/dedup `Alt` dispatch branches by first-set) and never given exploratory
   split/re-associate rewrites. Confluence of the normal-form set is a prototype obligation, not
   an assumption.
2. **Non-monotone global cost breaks greedy extraction.** The generated-size budget (GN-2/GN-5
   LOC budget, memory `generated-size-budget`) and capacity plans are knapsack-global; the greedy
   bottom-up extractor (`extract.rs:134`) is provably wrong for them. *Mitigation:* split the cost
   into a monotone-local part (greedy) and a global part (`csp-solver`); this is the CSP-central
   engine folded in, and it is where 2B and a CSP-central reading genuinely touch. A prototype must
   exhibit one grammar where local-greedy busts the budget and the CSP pass recovers it.
3. **Cyclic IR vs. finite extraction.** Recursive rules make the *rule call graph* cyclic; if
   recursion were an `Id` edge, extraction could loop or return `None` (`extract.rs:175`). The
   `Ref`-as-leaf + per-rule-root design (§1.1) is the fix, but it means the e-graph optimizes
   *within* a rule body, not across the fixpoint of the whole grammar — a real expressiveness
   ceiling that must be named, not hidden. Cross-rule inlining becomes an explicit, bounded rewrite
   (`Ref` → body copy, gated by a size analysis), never implicit congruence.
4. **Determinism / clean-regen.** The regen law demands byte-identical output across runs. The
   extractor's `best` map is an `FxHashMap` (`extract.rs:118`) and iteration over it, or any tie in
   cost, must resolve by a **total order** (class id, then a canonical node ordering) or regen
   drifts. `HashMap` iteration is not an admissible tie-breaker. This is a hard prototype gate:
   `regen ∘ regen = regen`, bit-for-bit, twice.
5. **Compile-time teardown on tiny grammars.** The `with_capacity` note (`egraph.rs:78-88`) warns
   that an oversized e-graph's *drop* can dominate the ~230µs compile profile of a ~300-node
   grammar. Promoting the whole IR to an e-graph multiplies node count; the budget must be
   measured, not assumed cheap. If the full-IR graph regresses compile time past budget, the
   aegraph on-insert discipline (fewer live alternatives) is the retreat, not abandonment.
6. **Consumer-in-wave (Y.13).** The extracted `BackendExpr`+`BackendShape` must reach
   `lower_to_rust` in the *same* wave the graph is stood up, or 2B recreates exactly the
   dead-substrate pathology the current `backend_egraph.rs` embodies. The consumer is named and
   extant (`backend_egraph.rs:11`); wiring it is non-optional.

---

## 5. What a prototype must test to move this family

1. **Interning + parity.** Derive/author `IrNode`, intern `json.bbnf` and
   `css_l4_declaration_values` (R1 §5-6), run the normal-form rewrite set + nullability/first-set
   analyses to fixed point, extract, and prove the extracted `BackendExpr`/`BackendShape` is
   **equivalent to the hand pipeline's output** (`passes::derive_recognizers`/`derive_backend_shape`)
   on json/css/sheets — and that `cargo xtask regen` is **bit-for-bit identical across two runs**
   (determinism gate #4).
2. **Blowup bound.** Measure e-node/e-class count and saturation iterations on the three live
   grammars; confirm the variadic normal-form set is confluent and terminates under
   `CspScheduler`'s `node_limit` (`csp_scheduler.rs:177`) with margin. Red line: any grammar whose
   graph exceeds a stated node budget under the chosen rewrite set refutes the *full* reading and
   forces the on-insert retreat.
3. **Extraction crossover.** Construct one grammar where a per-node-greedy shape choice busts the
   generated-size budget, show greedy extraction picks it, and show the `csp-solver` knapsack
   extraction recovers the legal optimum — proving the monotone/global split (risk #2) is real and
   necessary, not decorative.
4. **Compile-time budget.** PMU/`ax-iter`-profile the full-IR-as-e-graph compile against the
   current pipeline on the three grammars; the graph build + saturate + extract + **teardown** must
   stay within the compile budget (risk #5). Report the teardown fraction explicitly.
5. **Residualization-as-rewrite.** Reproduce PROVE-12's const-mask win (`prototypes/PROVE-12/`:
   css cyc/B 0.81-0.98×, +18 gen LOC) with the delimiter collapse expressed as an `IrNode` rewrite
   + extraction lowering rather than a template mutation, and show the emitted asm is the same
   straight-line residual — the test that "passes are rewrite groups" reaches the actual codegen
   win, not just the abstract graph.

**Verdict to carry forward.** Family 2B is coherent and its substrate is ~80% built, but its
*honest* strongest form is the aegraph-disciplined "IR is one e-graph, recursion is a symbolic
leaf, saturation is bounded/confluent, global cost is a CSP knapsack" — not textbook unbounded
equality saturation, which the IR's variadic+cyclic shape refutes on contact. The two hard
questions a prototype must answer before this family can be preferred: does the full-IR e-graph
stay within the compile-time budget, and does regen stay bit-for-bit deterministic through
cost-guided extraction.
