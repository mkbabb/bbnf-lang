# LOCK-2 pass-1 — Family 2B: IR-IS-EGRAPH

The thesis: the middle IR is not a tree that an e-graph *island* occasionally consults —
the middle IR **is** an e-graph, end to end. Every `BackendRule` body is interned as e-nodes
into one shared `EGraph`; every compile pass is a rewrite group over that graph; a scheduled
equality saturation reaches a fixed point (or an eager bound); and one cost-driven extraction
emits both the concrete lowered form **and** its `BackendShape` per rule. Codegen consumes the
extracted term. There is exactly one representation of the middle, one canonicalizer
(congruence closure), and one decision surface (extraction).

## 1. The extant substrate, diagnosed honestly

The e-graph engine is real and general. `crates/egraph/` is a complete equality-saturation
library: `EGraph<N, A>` with hash-cons + union-find + parent-edge rebuild
(`egraph/src/egraph.rs:155` `add`, `:202` `union`, `:251` `rebuild`), a `Language` trait
auto-derivable for any e-node enum (`egraph/src/language.rs:16`; derive at
`egraph-derive/src/lib.rs` classifies `Id`/`Box<[Id]>`/`Vec<Id>`/`[Id;N]` fields to children,
everything else to leaf metadata), a `Rewrite`/`RewriteFn` searcher-applier surface with an
`should_apply` analysis guard (`egraph/src/rewrite.rs:34,58`), a `CostModel`+`Extractor` doing
greedy bottom-up extraction over a `Lattice` cost (`egraph/src/extract.rs:85,121,134`), a
`BackoffScheduler` and a **CSP-backed** `CspScheduler` that models the dirty frontier as an
AC-3 lattice propagation (`egraph/src/csp_scheduler.rs:204` — `DirtyDomain` join, `ParentDirtyProp`
constraint, one `csp.propagate()` per iteration harvesting the transitive-dirty closure).

But today the e-graph touches the pipeline at exactly **one degenerate island**, and the middle
IR is a plain owned tree:

- The middle IR is `BackendIr { recognizers, rules: Vec<BackendRule{ name, expr: BackendExpr }> }`
  (`ir/src/lib.rs:352`), where `BackendExpr` (`ir/src/lib.rs:376`) is a recursive owned tree —
  `Seq(Vec<BackendExpr>)`, `Alt{mode, branches}`, `RepeatLoop{body: Box<..>}`, `ByteLiteral`,
  `RegexProgram{pattern, span_kind}`, `CallRule{callee}`, `SpanMark`, `TapeEmit`, `DirectBuild`,
  `ValueProject`, `Return`. It is built by tree recursion in `lower_to_backend`
  (`passes/src/lib.rs:1291` — `for rule in grammar.rules { lower_expr → materialize_rule → push }`),
  never interned.
- The **only** e-graph on the compile path is `passes/src/backend_egraph.rs:41` `select`: it
  builds `EGraph<DecisionNode, NoAnalysis>` where `DecisionNode { …, children: [Id; 0] }`
  (`backend_egraph.rs:159` — a **leaf-only** node, no structural children), unions all
  backend-shape candidates into one class, runs one `NormalizeDirectSinkCost` rewrite, and
  extracts the min-cost `BackendShape`. This is candidate-set selection, not IR rewriting; it
  exercises congruence over nothing. It is a shrunk-to-nothing instance of the sibling 2A
  "island" model, and the team kept the tree IR beside it. That is the state 2B supersedes.

So: the engine exists and is battle-shaped for grammar work; the *IR* has simply never been put
inside it. 2B's whole content is the transposition — and the honest reckoning with why it wasn't
done already (§4).

## 2. The mechanism made concrete

### 2.1 The transposition: `BackendExpr` (tree) → `BackendNode` (e-node)

Introduce `BackendNode`, the e-node isomorph of `BackendExpr`, deriving `Language`:
recursive positions become `Id`, `Box<BackendExpr>` → `Id`, `Vec<BackendExpr>` → `Vec<Id>`,
all scalar/byte/label metadata stays as leaf fields hashed into node identity.

```rust
#[derive(Clone, Eq, PartialEq, Hash, egraph_derive::Language)]
enum BackendNode {
    Entry(Id), Seq(Vec<Id>),
    Alt { mode: AltMode, branches: Vec<Id> },
    RepeatLoop { body: Id, min: u32 },
    OptionalBranch(Id),
    ByteLiteral(Vec<u8>),                       // leaf; congruence-merges equal literals
    RegexProgram { pattern: String, span_kind: SpanKind },
    CallRule { callee: RuleId },                // §4.1: an OPAQUE LEAF, not an Id edge
    SpanMark { kind: SpanMarkKind, label: String },
    TapeEmit { kind: TapeKind },
    DirectBuild { shape: String, fields: Vec<DirectBuildField> },
    ValueProject { body: Id, target: String, ty: Option<String> },
    Return,
}
```

`BackendIr` becomes `{ graph: EGraph<BackendNode, EClassFacts>, roots: HashMap<RuleId, Id>,
recognizers }`. The interning site is `passes/src/lib.rs:1291`: replace tree construction with
`intern(&mut graph, expr)` per rule, recording each rule's root e-class in `roots`. Interning is
mechanical — the same post-order walk `lower_expr` already does, `graph.add(node)` instead of
`Box::new`. **Cross-rule GVN falls out for free**: identical whitespace-skip loops, identical
`ByteLiteral(b"true")`, identical span kernels that appear in many rules hash-cons to one
e-class. This is the aegraph result — GVN "for free" during translation into the e-graph
([Fallin, aegraph](https://cfallin.org/blog/2026/04/09/aegraph/)).

### 2.2 The analysis lattice `EClassFacts`

The engine's `Analysis<N>` slot (`egraph/src/analysis.rs`, used through `should_apply` at
`rewrite.rs:58`) carries the grammar facts rewrites need to fire safely: **nullable** and
**first-set** per e-class (both already computed as tree analyses in `ir/src/lib.rs:326-338`
`nullable` and the first-set derivation). As `A::make`/`A::merge`, they become the per-class
lattice that (a) guards Alt-dispatch tightening (`should_apply` aborts a rewrite that would
bloat an already-disjoint dispatch — the AA.5 rationale in `rewrite.rs:45-56`), and (b) feeds
the recognizer-derivation rewrite (self-loop detection needs the body's character-class /
complement cardinality). One analysis, computed by congruence, replacing the standalone tree
passes.

### 2.3 Passes become rewrite groups

Each existing pass is re-expressed as a `Rewrite<BackendNode, EClassFacts>` (non-destructive:
`add` the equivalent form, `union` with the source — `rewrite.rs:70`), grouped and run under a
schedule:

- **Normalization group**: Seq-flatten (`Seq[.. Seq[a b] ..]` ≡ `Seq[.. a b ..]`), single-branch
  `Alt` collapse, `Optional(x)` ≡ `Alt{Empty, x}`, dead-branch pruning. These are the classic
  grammar canonicalizations; today they are scattered imperative tree edits.
- **Residualization group** (PROVE-12 generalized): the CSS delimiter-mask const-fold that
  PROVE-12 (`prototypes/PROVE-12/REPORT.md`, GO, CSS cyc/B 0.81-0.98×) hand-wrote into the
  `CSS_GENERATED_RS` template becomes a *rewrite rule*: a `find_component_delim`-shaped self-loop
  over a grammar-constant delimiter set rewrites to a `SpanMark`/scan node carrying the folded
  `u128` mask. PROVE-12 §7 explicitly flags this generalizes "to any grammar-derived
  delimiter/STOP-set scan … the same `const fn mask` + const-generic seam" — 2B is that
  generalization, homed as one rewrite instead of per-template hand-edits.
- **Recognizer-derivation group** (the GN-2.W1 seam): a Kleene/`RepeatLoop` over a character
  class whose complement cardinality ∈ [1,16] rewrites to a `SimdScan{ mode: Prefilter,
  site: SelfLoop{terminators} }` equivalent (`ir/src/lib.rs:425` `SimdMode`, `:430` `SimdSite` —
  today `Exact`/`PreEntry` only). Extraction then picks scan-vs-scalar by cost. This folds
  GN-2.W1's `derive_recognizers` detection (`passes/src/lib.rs:338`) into the same graph.
- **Materialization/tape group**: the `materialize_rule` pass (`passes/src/lib.rs:~1294`) and
  the tape-emit shaping become rewrites that add `TapeEmit`/`DirectBuild`/`ValueProject`
  alternatives, so that eager-tape vs sink-only vs collapsed-stage are *co-present e-nodes in one
  class* rather than a separately-computed `HashMap<RuleId, BackendShape>`.

### 2.4 Extraction emits BackendShape directly (the island's death)

This is the load-bearing unification. Today `BackendShape` is chosen by a **separate** path:
`derive_backend_shape` → `candidate` enumeration (`passes/src/lib.rs:504-580`) → the degenerate
`backend_egraph::select` island → `HashMap<RuleId, BackendShape>` (`passes/src/lib.rs:96`), while
the `BackendExpr` tree is built independently. In 2B they are the **same** extraction: the
alternatives added by the materialization/tape group mean each rule root's e-class contains one
e-node per admissible shape; the cost model ranks them; `Extractor::best_node(root)`
(`extract.rs:183`) yields *the* shape, and `extract_tree(root)` (`extract.rs:190`) yields the
concrete lowered term for codegen. The shape is an **attribute of the extracted form**, not a
parallel map. `ActiveCostFacts.selected_shape` (`ir/src/cost.rs:184`) is populated from the
extraction, and the entire `backend_egraph.rs` + the candidate machinery
(`passes/src/lib.rs:486-609`) collapses into the one extraction pass.

### 2.5 The schedule

Saturation runs under `CspScheduler` (`csp_scheduler.rs:204`) — already the isomorphic
CSP-as-scheduler the constitution's engine-sharing implies, reusing `csp-solver` for the
dirty-closure. The measured alternative 2B must weigh (§3) is aegraph-style **eager, acyclic,
single-pass** application (no fixpoint) — Cranelift's finding is that a levelized acyclic
e-graph + eager cascade rules attains the optimization level in a few linear passes at 1-5%
compile cost ([Fallin, aegraph](https://cfallin.org/blog/2026/04/09/aegraph/);
[Bytecode Alliance RFC #27](https://github.com/bytecodealliance/rfcs/blob/main/accepted/cranelift-egraph.md)).

### 2.6 Codegen consumes the extracted term

`codegen::lower::lower_to_rust` (invoked `codegen/src/lib.rs:176`, `LowerCtx` carries
`backend_shape`/`cost_facts` per `lib.rs:177-184`) reads `extract_tree(root)` per rule instead
of walking `BackendRule.expr`. This is the **same-wave consumer** that satisfies the Y.13
consumer-less-substrate law (constitution viii): the e-graph is not substrate-in-waiting, it is
read by the emitter in the same tranche it is built.

## 3. Prior art (SOTA, cited)

- **Cranelift ægraphs** — the decisive precedent and the honest template for 2B. Chris Fallin's
  production mid-end **is** an e-graph end to end: separate machine-independent passes deleted,
  GVN/LICM/rematerialization obtained "for free" during translation into the e-graph, better
  code than the classical pipeline at ~1-5% compile cost. Crucially it solves 2B's two blockers:
  **acyclicity** (a levelized "acyclic e-graph" so extraction terminates) and **eager rule
  application** (no unbounded fixpoint), plus **scoped elaboration** to turn the graph back into
  scheduled instructions. [blog](https://cfallin.org/blog/2026/04/09/aegraph/),
  [EGRAPHS'23](https://pldi23.sigplan.org/details/egraphs-2023-papers/2/-graphs-Acyclic-E-graphs-for-Efficient-Optimization-in-a-Production-Compiler),
  [RFC](https://github.com/bytecodealliance/rfcs/blob/main/accepted/cranelift-egraph.md).
- **Equality saturation / PEG** (Tate, Stepp, Tatlock, Lerner, POPL'09 / LMCS'11) — the origin
  of "the IR is an e-graph": Program Expression Graphs represent loops as expressions so the
  *whole* program lives in one e-graph; optimize by saturate-then-extract. The direct ancestor of
  2B's claim. [LMCS'11 PDF](https://www.cs.cornell.edu/~lerner/papers/lmcs11-eqsat.pdf).
- **egg** (Willsey et al., POPL'21) — deferred rebuilding + e-class analyses; our `EGraph.rebuild`
  (`egraph/src/egraph.rs:251`) and `Analysis` slot are the egg design. The `should_apply` guard
  is egg's conditional-rewrite pattern.
- **egglog** (Zhang et al., PLDI'23) — e-graphs unified with Datalog; the model for expressing
  the rewrite *groups* declaratively and for stratified/scheduled saturation.
- **Extraction is NP-hard in general**; ILP extraction (e.g. ROVER RTL datapath, up to 63% area)
  only scales to small graphs — the named risk for any sharing-sensitive cost model
  ([survey refs](https://www.cs.cornell.edu/courses/cs6120/2025sp/blog/superopt/)). 2B must keep
  extraction greedy-monotone or delegate the sharing-sensitive decision to the bounded CSP.
- **RVSDG / MLIR PDL** — regionalized dataflow IRs and pattern-rewrite infrastructure: precedent
  that a production compiler middle can be a rewrite-driven graph with region boundaries (the
  bbnf analog of per-rule regions, §4.1).

## 4. Named risks and counterexamples — the make-or-break

### 4.1 Recursion breaks greedy bottom-up extraction (THE blocker)

Grammar IR is cyclic: JSON `value → array → value`, `value → object → member → value`. If
`CallRule` is an `Id` edge into the callee's root class, the e-graph has a cycle, and the extant
extractor **deadlocks on it**: `Extractor::compute_best` (`extract.rs:143-149`) skips any node
whose children are not all already resolved — a class reachable only through itself never
resolves, so `best_node(value_root)` returns `None` and extraction fails. This is the exact
counterexample that would sink a naive "one flat e-graph over all rules with CallRule-as-Id."

The aegraph answer, adopted: **CallRule is an opaque leaf** (a `RuleId` metadata field, not a
child `Id`). The e-graph is then a forest of *acyclic per-rule-body regions*; cross-rule
structural sharing is recovered by GVN on the extracted-form hash (identical rule bodies still
merge), and the rule-call structure is a thin skeleton outside the graph — Cranelift's
"hybridize the CFG with the e-graph for pure operators + scoped elaboration." The honest
consequence: the maximalist slogan "one e-graph, no boundaries" is **false**; the lawful 2B is
"one e-graph engine, one acyclic region per rule body, a rule-call skeleton." A prototype MUST
decide whether the current greedy extractor tolerates the JSON recursion at all before any
"end-to-end" language is committed (§5 P2). This is the single most important gate for the
family, and where 2B either differentiates from or converges toward sibling 2A's explicit
islands.

### 4.2 Sharing-sensitive cost → NP-hard extraction

The SPEC imposes a `generated_loc_budget` (GN-2 §5; sk-v22 law). A cost that rewards *shared*
sub-parsers (one emitted function reused across rules) or bounds total generated size is
**non-monotone** — optimal extraction becomes NP-hard (§3). The extant `Extractor` is greedy
bottom-up and valid **only** for monotone cost (`extract.rs:9-11` says so). Disposition: keep the
extraction cost monotone (per-node, child-summed, as `AstSize`/`DecisionCostModel` are), and push
any global sharing/capacity/size decision to the **bounded CSP** post-solve
(`passes/src/decision_csp.rs`, `DecisionCspFacts` at `ir/src/cost.rs:203`). This is precisely the
sibling 2C boundary — 2B must name it as the seam it *delegates*, not one it dissolves, or it
overclaims.

### 4.3 Determinism / bit-for-bit regen

The SPEC's hardest gate is `cargo xtask regen` byte-identical (constitution, GN-1 gate). Two
hazards: saturation order and extraction tie-breaks. `compute_best` stores bests in an
`FxHashMap` (`extract.rs:118,128`) and iterates classes in `Vec` order (deterministic) but ties
resolve on first-seen — nondeterministic if rule application order varies. The extant island
already defends this with a `candidate_hash`/`stable_hash` total order on `DecisionNode`
(`backend_egraph.rs:170`, `selection_trace_hash`); 2B must give `BackendNode` extraction the same
stable tie-break key. Prototype gate P4.

### 4.4 Compile-time budget (the teardown tax)

The type plane compiles in ~230µs (SPEC §0/08). `egraph/src/egraph.rs:67-88` documents that on a
~300-node grammar an oversized hash-cons table's *teardown* can dominate the compile profile —
and `csp_scheduler.rs:44-49` notes SipHasher scratch cost was already ~10% of `compile_bbnf`.
Interning every rule body (vs the current thin tree) multiplies e-graph residency. Risk: 2B pays
a compile-time regression for an optimization win that the tiny grammars (json/css/sheets) may
not repay. Must be measured (§5 P5), with `with_capacity` sizing from grammar node count.

### 4.5 Metadata-heavy leaves bloat interning

`DirectBuild{shape, fields: Vec<DirectBuildField>}` and `RegexProgram{pattern}` carry large
non-`Id` payloads hashed into node identity. They congruence-merge correctly but inflate memo
keys; interning cost is real. Not fatal — they are leaves — but a named cost.

### 4.6 Y.13 — the island precedent is a warning

The team already built a narrow e-graph (`backend_egraph.rs`) and **kept the tree IR beside it**.
2B must prove the full transposition earns its keep — i.e., that codegen reading `extract_tree`
(§2.6) produces byte-identical output *and* that GVN/residualization-as-rewrite buy measurable
generated-size or perf, or 2B is consumer-less substrate dressed as architecture.

## 5. What a prototype must test to move this family

- **P2 (decisive, do first): does extraction survive JSON recursion?** Intern `json.bbnf`'s
  rules into one `EGraph<BackendNode>` with `CallRule` as (a) an `Id` edge, then (b) an opaque
  leaf. Run `Extractor::new(...).best_node(roots[value])`. Prove (a) deadlocks/`None` and (b)
  yields a valid term. Outcome dictates whether "IR-is-egraph" means one graph or acyclic regions
  + skeleton — the family's actual shape.
- **P1: measure GVN.** After interning json + css, report `total_nodes` vs sum of
  `BackendExpr` tree nodes → the structural-sharing ratio. If dedup is negligible on real
  grammars, the central 2B payoff (GVN "for free") is absent and the family weakens.
- **P3: regen parity.** Encode PROVE-12 residualization + Seq-flatten + one recognizer-derivation
  as rewrites; saturate under `CspScheduler`; extract; lower via `codegen::lower::lower_to_rust`
  reading `extract_tree`; assert `generated.rs` is **byte-identical** to the current template
  path (the SPEC hard gate). If it can't hit byte-parity, 2B cannot land.
- **P4: determinism.** Saturate+extract twice; assert identical extracted forms and a stable
  `selection_trace_hash`. Prove the tie-break is a total order, not `FxHashMap` order.
- **P5: compile budget.** `compile_bbnf` self-time, e-graph path vs current tree path, on
  json/css/sheets; must not regress the ~230µs envelope beyond a stated bound (aegraph's 1-5%
  is the reference ceiling).
- **P-sched: fixpoint vs eager.** Compare `CspScheduler` saturation against an aegraph-style
  eager single-pass over the acyclic regions on identical rewrite sets — measure iterations,
  compile time, and extracted-form quality; adopt the winner as 2B's schedule.

## 6. Integration seams (path:line)

- `ir/src/lib.rs:376` `enum BackendExpr` → the source of `BackendNode: Language`
  (`Box`→`Id`, `Vec`→`Vec<Id>`, metadata→leaf; derive at `egraph-derive/src/lib.rs`).
- `ir/src/lib.rs:352` `struct BackendIr` → `{ graph, roots, recognizers }`.
- `passes/src/lib.rs:1291` `lower_to_backend` → the interning site (replace tree build with
  `graph.add`).
- `passes/src/backend_egraph.rs` (whole) + `passes/src/lib.rs:398,486,504-609` (candidate
  enumeration, `derive_backend_shape`, `select`) → **subsumed** by §2.4 extraction.
- `egraph/src/extract.rs:134` `compute_best` → the cyclic-extraction blocker (§4.1); the tie-break
  determinism site (§4.3).
- `egraph/src/csp_scheduler.rs:204` → the saturation schedule (reused); the eager-aegraph
  alternative measured against it.
- `egraph/src/rewrite.rs:34,58` `Rewrite`/`should_apply` → where each pass-group rule lands, guarded
  by `EClassFacts`.
- `ir/src/lib.rs:326-338` (`nullable`, first-set) → the `Analysis<BackendNode>` lattice.
- `ir/src/cost.rs:5,173,184` `CostFacts`/`ActiveCostFacts`/`selected_shape` → the extraction cost
  model + provenance; `decision_csp.rs` / `cost.rs:203` `DecisionCspFacts` → the delegated
  sharing/capacity CSP (§4.2).
- `codegen/src/lib.rs:176-184` `lower_to_rust`/`LowerCtx` → the same-wave consumer reading
  `extract_tree` (§2.6, Y.13).
- `prototypes/PROVE-12/REPORT.md` §7 → residualization-as-rewrite, the generalization 2B homes.
- GN-2.W1 (`docs/tranches/sk-v24/GN-2.md`, `passes/src/lib.rs:338`) → recognizer-derivation
  rewrite group.
