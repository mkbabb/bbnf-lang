# LOCK-2 · Family 2B — IR-IS-EGRAPH — pass-2 research

*The middle IR is an e-graph end to end: `ExprKind`/`BackendExpr` nodes ARE e-nodes, the
tower's passes ARE rewrite groups keyed to named decision points, equality holds under a
schedule, and elaboration emits `BackendShape` + `BackendExpr` directly. Family developed to
its strongest form, alone.*

---

## 0. The thesis, sharpened against the binding frame

The cross-lock frame (XL-B) fixes that graph-opt engines operate WITHIN the middle-IR tower at
named decision points, not by redrawing the IR. 2B's strongest lawful reading is therefore NOT
"replace the tower with a monolithic saturating soup" — that reading dies on the ~230 us
per-parse compile budget and on XL-B itself. The strongest 2B is the **acyclic-e-graph (aegraph)
tower**: the tower keeps its levels and its decision points, but each level's term forest is
*represented as* an e-graph, congruence is maintained eagerly during construction, rewrite
groups fire at their decision points, and a single scoped **elaboration** pass (not a
saturate-then-extract fixpoint) lowers the graph to `BackendIr`. This is exactly the move
Cranelift made in 2022 for its production mid-end (Fallin, *The acyclic e-graph*,
cfallin.org/blog/2026/04/09/aegraph; Bytecode Alliance RFC-27 / `accepted/cranelift-egraph.md`;
ægraphs, EGRAPHS 2023). 2B's claim: what Cranelift proved for SSA/CFG holds for the grammar
tower, because the grammar tower is *more* amenable — its per-rule expression forest is already
acyclic (the only back-edges are `Ref`/`CallRule`, which are opaque e-node leaves, §4).

The distinction from the extant island approach is precise and measurable. Today
(`skinny/crates/passes/src/backend_egraph.rs:60`) an `EGraph<DecisionNode, NoAnalysis>` is built
*per rule* over <=5 childless candidate nodes, saturated with one rewrite, extracted for one
winner — an e-graph used as a ranked-choice ballot box, not as the IR. 2B makes the IR itself
the graph: `ExprKind` (`ir/src/lib.rs:234`) and `BackendExpr` (`ir/src/lib.rs:376`) become the
`Language`, their child `ExprId`/`Box<BackendExpr>` fields become `Id` children, and the tower's
transformations (epsilon elimination, dispatch-table synthesis, recognizer derivation, direct-
sink normalization, projection canon) become `Rewrite` groups over that one graph. Congruence
closure then delivers CSE/GVN across the whole grammar for free — identical sub-rules interned
once — which the current tree-of-`Expr` representation cannot express.

---

## 1. The mechanism, made concrete

**1.1 The node language.** Two `Language` impls, both auto-derivable via the existing
`egraph_derive::Language` macro (it already projects `Vec<Id>`/`Box<Id>`/`[Id;N]`/single `Id`
fields — see `crates/egraph/tests/derive_language.rs:5-37`, and the `LanguageChildren` impls at
`crates/egraph/src/language.rs:71-114`). The obstacle is that `ExprKind` today stores children
as `ExprId`/`RuleId` newtypes, not `egraph::Id`, and carries owned payloads (`Vec<u8>`, `String`)
inline. Two lawful shapes:

- **Flat e-node** (recommended): a `GExpr` enum isomorphic to `ExprKind` whose recursive fields
  are `Id` and whose *payloads are interned* — `Literal{ bytes: StringId }`, `Regex{ pattern:
  StringId }`, `Ref{ target: RuleId }`. Interning payloads is mandatory anyway for `Language:
  Eq + Hash` to give structural equality the congruence closure needs; it also collapses the
  `String` clone traffic the current `Expr` tree pays. `Seq(Box<[Id]>)`, `Alt{branches:
  Box<[Id]>, mode}`, `Repeat{body:Id, min, max}`, `Optional(Id)`, `Project{body:Id, target:
  StringId, ty}`, `Discard{left:Id, right:Id, keep_left}`. Note `AltMode`/`CaseSensitivity`
  (`ir/src/lib.rs:274,280`) are `Copy` scalars, so they ride as non-child leaf fields, exactly
  the `Pow{exp:u32}` pattern the derive already handles.
- **Backend e-node**: a `GShape` enum isomorphic to `BackendExpr` (`ir/src/lib.rs:376`) with the
  same Id-projection. Elaboration reads the extracted `GExpr` best-form and emits `GShape` — the
  BackendShape choice becomes an *elaboration decision per class*, not a separate CSP over
  candidate ballots.

**1.2 Passes to rewrite groups at named decision points.** XL-B's "named decision point" maps
one-to-one onto a `Rewrite` group name. The tower's existing passes re-express as:

| decision point | rewrite group | form |
|---|---|---|
| epsilon / nullable normalization | `elim-epsilon` | `Seq[eps, x] => x`, `Alt[x, dead] => x` (narrowing) |
| dispatch-table synthesis | `alt->dispatch` | `Alt{Speculative} => Alt{Dispatch}` when first-sets disjoint (`AltMode`, `:274`) |
| recognizer / scan-plan | `self-loop->prefilter` | `Repeat{class}` with complement <=16 => `SimdScan{Prefilter,..}` (see §5, GN-2.W1) |
| projection canon | `project-normalize` | `Project` fusion / hoist, the PROVE-12 residualization seam |
| direct-sink | `sink-normalize` | the extant `NormalizeDirectSinkCost` (`backend_egraph.rs:191`) generalized to `GExpr` |

Each group runs under the existing `CspScheduler` (`crates/egraph/src/csp_scheduler.rs:204`),
whose AC-3 dirty-frontier closure already restricts re-probing to classes that grew — the right
schedule primitive for "passes as rewrite groups" without re-scanning the whole grammar each
iteration.

**1.3 Elaboration, not extraction (the budget-critical refinement).** The extant `Extractor`
(`crates/egraph/src/extract.rs:115`) is egg's greedy bottom-up fixed-point over `classes()`.
2B's strongest form runs elaboration in the aegraph style: because the per-rule graph is acyclic
(§4), a *single* reverse-topological sweep assigns each class its best `GExpr` by monotone cost
and simultaneously emits the `GShape`, subsuming GVN/LICM-analogues (shared sub-rule hoisting) in
one pass — no `compute_best` fixpoint loop, no separate extract-tree recursion. This is the
aegraph's core efficiency claim (RFC-27: "acyclicity + eager rule application => efficient
single-pass optimization without fixpoints"). For the grammar fleet, whose multiplicity is
~1 (§2, Q1), the elaboration sweep is O(nodes) and its teardown is the dominant cost — met by the
existing `EGraph::with_capacity` right-sizing (`egraph.rs:78`, whose own doc-comment names the
"1024-bucket table teardown dominates the compile profile" pathology).

---

## 2. Q1 — saturated multiplicity, and what it implies for 2B

**Prediction, to be measured (SPEC-ONLY until PROVE-B runs):** on the three live grammars (json,
css_l4_declaration_values, sheets — `R1 §5`) the average e-class size after saturation is ~1 and
the max is small (2-3), concentrated at childless leaves (`Literal`, `Regex`, `Ref`) where a
narrowing rule offers an alternate form, essentially never at child-bearing `Seq`/`Alt`. Reason:
the tower's rewrite set is overwhelmingly *narrowing* (epsilon-elim, dispatch specialization,
sink-normalize all reduce or re-tag, they do not manufacture families of equivalents). The
extant DecisionNode graph makes this vivid — its nodes are childless and its one rewrite adds
exactly one alternative (`backend_egraph.rs:216`), so every class has <=2 nodes.

**Consequence for 2B:** where multiplicity ~1, "extraction-as-decision" collapses to
"cost-ranked canonicalization," and the aegraph single-sweep elaboration is not merely adequate
but *optimal* — the NP-hard DAG-extraction wall (SmoothE, ASPLOS 2025; e-boost, ICCAD 2025;
Tensat's ILP, MLSys 2021; extraction is NP-hard via min-set-cover when sharing makes tree-cost
non-compositional) is never approached, because there is no combinatorial choice frontier to
solve. **This is 2B's decisive evidence obligation:** PROVE-B must instrument, per grammar,
`max_eclass_size`, `count(classes with >=2 nodes)`, and *the node kind of each multi-node class*.
If the histogram confirms multiplicity ~1 with choices only at childless leaves, 2B ships greedy
elaboration and the CSP/ILP extraction machinery is provably unnecessary here (a named negative
result that *strengthens* 2B by deleting a subsystem). If a child-bearing class ever carries >=2
surviving forms with differing DAG cost, that single site is the counterexample that forces a
bounded local extraction there — and only there.

---

## 3. Q2 — widening rewrites coexisting with narrowing

A widening rewrite (one that manufactures an alternate/larger form — e.g. `alt->dispatch`
introducing a dispatch scaffold, or a hypothetical `Repeat{x} => Seq[x, Repeat{x}]` unrolling)
does **not** need confluence with the narrowing set, and this is the whole point of the e-graph
representation: rewrites *add equalities, never replace* (`crates/egraph/src/lib.rs:3-5`;
egg, Willsey et al., POPL 2021). Confluence is a property you need for *destructive* rewrite
systems (GHC's RULES pragma is the cautionary precedent — un-confluent RULES silently pick an
order-dependent normal form); an e-graph sidesteps it by keeping every form and deferring the
choice to elaboration. Termination is bought not by a decreasing measure but by the two
*operational* bounds already present: `egraph_iter_limit = 64` and `egraph_node_limit = 100_000`
(`crates/egraph/src/cost_config.rs:66-67`), enforced by the scheduler's growth guard
(`csp_scheduler.rs:280`). Semantics preservation is a *per-rule* obligation, discharged the way
PROVE-12 discharged its transform (§5 there: a byte-level argument that the residual is
extensionally equal) and asserted by a differential parity oracle at the tower's output —
byte-identical generated parser behavior, the constitution's clause vii artifact standard.

**Precedent for widening+narrowing coexisting in shipping engines:** egg's own rebuilding
tolerates rules in both directions (its exemplar `(* a 2) = (<< a 1)` mixes manufacture and,
with `a*2/2 = a`, cancellation); Cranelift's mid-end runs both algebraic simplifications
(narrowing) and forms that expand for later cancellation (RFC-27); Tensat deliberately widens the
tensor-op graph then extracts. The lawful rule for 2B: **a widening rule is admissible iff it is
(a) node-bounded — its RHS adds O(1) nodes per match — and (b) semantics-preserving per its own
oracle.** No global confluence proof is owed; the graph *is* the confluence.

---

## 4. Q3 — organ port budget, acyclicity, and `regen o regen` byte-identity

**Acyclicity — the load-bearing structural fact.** A grammar rule body is a finite expression
tree (`Expr` at `ir/src/lib.rs:227`, built via `add_expr` `:51` into a `Vec<Expr>` — an arena of
`ExprId`, no self-reference within a body). The only cycles in a grammar are *rule-level*
recursion, and those cross a `Ref{target: RuleId}` (`:253`) / `CallRule{callee}`
(`ir/src/lib.rs:393`) boundary — which in the e-node language is an **opaque childless leaf**
(the callee is a `RuleId` scalar, not an `Id` child). Therefore the per-rule e-graph is acyclic
by construction, and the aegraph elaboration's "no cycle-breaking needed" property (Fallin,
aegraph blog: "an enode cannot indirectly refer back to itself") holds. This is why the
Cranelift model ports cleanly: the grammar tower is a family of small acyclic e-graphs joined at
Ref leaves, precisely the aegraph's home turf.

**Budget.** Build+saturate+elaborate+teardown must hold the ~230 us order. The four costs:
(1) build/intern — one `add` per IR node, hashcons hit on shared sub-rules (a *saving* vs the
current tree); (2) saturate — with multiplicity ~1 and the AC-3 dirty frontier, <= a handful of
iterations over a small dirty set; (3) elaborate — single O(nodes) topo sweep; (4) teardown —
the named risk, mitigated by `with_capacity(node_count)` (`egraph.rs:78`) so the dropped `memo`
capacity is proportional to occupancy, not the next power of two. PROVE-B must bench the whole
organ cold-per-parse against the extant compile path and show it holds budget on all three
grammars; this is a *falsifiable* gate, not a projection.

**`regen o regen` byte-identity through cost-guided elaboration.** The determinism hazard the
directive names (HashMap iteration inadmissible) is real and locatable: `Extractor::best` is an
`FxHashMap` (`extract.rs:118`) but `compute_best` *iterates* `egraph.classes()`
(`extract.rs:140`), which is `self.classes.iter()` — **Vec order, insertion-ordered,
deterministic** (`egraph.rs:138`). So class *iteration* is already regen-stable provided IR
construction is deterministic (it is: arena push order). The remaining hazard is **cost ties**:
when two forms in a class tie on cost, the winner must break on a *total order*, not on map
probe order. The extant `DecisionCost` already does this correctly — its final field is a
`candidate_hash` (`backend_egraph.rs:246`, an FNV over the stable candidate id), making the
`Ord` total and the pick deterministic. **2B's rule:** the elaboration cost type is a lattice
whose `Ord` is total, with a final structural tie-break key = the interned-node canonical byte
key (StringId-resolved), so `regen o regen` is byte-identical by construction. The idempotent-regen
gate (precept 53; PROVE-12 §7 demonstrated it) is the artifact.

---

## 5. Q5 & the XL-F supersession — IR-modeled competing sites

2B is the family where a scan/self-loop site *lives in the IR as an e-node*, which is exactly
what makes competing-site modeling first-class. Today `Recognizer::SimdScan{mode,alphabet,site}`
(`ir/src/lib.rs:416`) with `SimdMode::Exact` only (`:425`) and `SimdSite::PreEntry` only (`:430`)
is a *sidecar* `Vec<Recognizer>` on `BackendIr` (`:355`), derived by
`passes::derive_recognizers` (`passes/src/lib.rs:338`) and stamped as a comment
(`R1 §8`). Under 2B this is not a sidecar: a self-loop scan is a `GShape` e-node
`SimdScan{mode, alphabet, site}` produced by the `self-loop->prefilter` rewrite, and its
*alternative* form (the scalar membership loop it replaces) coexists in the same class until
elaboration picks by cost. That is the IR-modeled competing site the question asks for.

**GN-2.W1's predicate (the XL-F supersession).** GN-2.W1 (`docs/tranches/sk-v24/GN-2.md:181-233`)
extends `SimdMode -> {Exact, Prefilter}` and `SimdSite -> {PreEntry, SelfLoop{terminators}}` and
adds the detection arm to `derive_recognizers` — a grammar-*structural* predicate (Kleene over a
char-class whose complement cardinality in [1,16]). **Relation to 2B (characterized with
evidence): 2B *wraps* GN-2.W1's predicate, it does not replace it.** The predicate is the
*guard* of the `self-loop->prefilter` rewrite's `search`; 2B changes *where the result lives*
(an e-node alternative resolved by cost-guided elaboration) not *how the site is detected*
(still the complement-cardinality test). Concretely: GN-2.W1 lands the detection + the
`SimdMode::Prefilter`/`SelfLoop` variants and the `lower/recognizer/` dispatch as a directed
pass; 2B, if it wins, re-homes that pass as a rewrite group whose match condition IS GN-2.W1's
predicate and whose alternative-vs-scalar choice IS an elaboration cost decision. The predicate
survives verbatim; only its consumer moves. This "waits-on" GN-2.W1 landing first (its dispatch
skeleton is the home PT-1.W5 also lands into, GN-2.md §2.4) — so 2B's recognizer-as-rewrite is a
*follow-on* to GN-2.W1, not a competitor. Binding static-size + cyc/B costs for the competing
site come from the same `CostFacts`/`perf_cost`/`static_size_cost` the DecisionNode already
carries (`backend_egraph.rs:170-173`); PROVE-01's certified 0.115 cyc/B skip floor
(`GN-2.md:190`) is the real, binding number that flips the elaboration choice at a `SelfLoop`
site.

---

## 6. Q8 — profile-fed cost into elaboration

2B's cost model is `CostModel<GExpr>` / `CostModel<GShape>` (`extract.rs:85`), and the
profile-fed coefficient enters as the *weights* it reads — `CostWeights`/`CALIBRATED_WEIGHTS`
(`crates/egraph/src/cost_weights.rs`, already grid-sweep-calibrated per `cost_config.rs:59-65`)
plus the per-shape `perf_cost` measured in cyc/B. For a coefficient to (a) flip a real per-site
decision it must change which `GShape` a class elaborates to (e.g. push a `SelfLoop` site from
scalar to `Prefilter` at PROVE-01's floor, or a rule from `EagerTape` to `SinkOnly`); the extant
test `decision_egraph_rewrite_changes_selected_shape` (`passes/src/lib.rs:2014`) is the seed
proving a cost change *can* flip the selected shape. Attribution when whole-grammar cyc/B
underdetermines per-feature coefficients is solved by A/B-differential capture — the paired
cold-per-parse protocol PROVE-12 §4 used (baseline vs modified binary interleaved back-to-back
per window), which isolates a single feature's delta. **Env-source fencing from regen** is the
determinism seam: `CostConfig::from_env` reads `BBNF_COST_*` (`cost_config.rs:83-118`), and any
env override MUST be walled out of the regen path — regen consumes only the committed
`CALIBRATED_WEIGHTS` fact table, never `from_env`, or `regen o regen` breaks. The freshness gate
going RED on mutation is the `selected_cost_freshness`/`candidate_cost_stale_rate_bps` machinery
already in `ActiveCostFacts` (`ir/src/cost.rs:187-189`, `backend_egraph.rs:127`) — 2B inherits
it. Which knobs a fit supersedes: the hand-set `perf_cost`/`shape_rank` heuristics in the
candidate builder (`passes/src/lib.rs:514-580`) become fitted coefficients; the `alt_per_branch`/
`dispatch_bonus`/`structural` weights (`cost_config.rs:101-115`) are the fit's output.

---

## 7. Q4 — context-sensitive extraction (audit owed, prediction stated)

The hazard: a sub-rule reached under two callers — one sink-only, one offset-tape — where one
per-class elaborated shape cannot serve both. This is the classic e-graph *context-sensitivity*
gap: greedy bottom-up extraction assigns one best form per class regardless of parent context.
**Audit owed:** grep `admits_sink_only` (named in the directive) across `passes/`/`codegen/` and
trace whether any grammar rule is `CallRule`'d from both a `SinkOnly` and an `OffsetTape`
BackendShape context. **Prediction:** the fleet is context-free at the rule seam because the
tower selects BackendShape *per grammar* (one shape per generated grammar — json is offset/lazy,
css is scan, `R1 §5`), not per call site, so a rule cannot be reached under two conflicting
whole-grammar shapes within one compile. If that holds, 2B's per-class elaboration is sound
without repair. If a genuine dual-context sub-rule exists, the identity-preserving repair is
**class duplication by context** (Cranelift handles this via scoped elaboration re-materializing
a value in each scope; the aegraph blog's LICM/GVN subsumption is exactly context-aware
re-placement) — cost is O(distinct contexts x class size), bounded and local. The prototype must
report the `admits_sink_only` census; a null result closes Q4 as "context-free, no repair," a
positive result scopes the repair to the enumerated sites.

---

## 8. Substrate inventory (what exists, what 2B must build)

**Exists, reusable verbatim:** `EGraph<N,A>` with hashcons + rebuild + `with_capacity`
(`crates/egraph/src/egraph.rs`); the `Language` trait + `egraph_derive` auto-projection
(`language.rs`, `tests/derive_language.rs`); `Extractor` greedy bottom-up (`extract.rs`, to be
narrowed to single-sweep elaboration for the acyclic case); `CspScheduler` dirty-frontier AC-3
(`csp_scheduler.rs`); `CostModel`/`Lattice`/`Scalar` (`extract.rs:36-111`); `Rewrite`/`RewriteFn`
(`rewrite.rs`); `CostConfig`/`CostWeights` (`cost_config.rs`, `cost_weights.rs`); the working
DecisionNode precedent end-to-end (`backend_egraph.rs`).

**2B must build:** (1) `GExpr`/`GShape` e-node enums isomorphic to `ExprKind`/`BackendExpr` with
Id-projected children and StringId-interned payloads — the derive does the trait, the interning
is new; (2) an intern pass `GrammarIr -> EGraph<GExpr>` (arena walk, one `add` per `Expr`); (3)
the five rewrite groups (§1.2), each a `Rewrite` with a semantics-preservation oracle; (4) an
`elaborate(&EGraph<GExpr>) -> BackendIr` single-topo-sweep that emits `GShape`/`BackendExpr` with a
total-ordered cost tie-break; (5) the differential parity oracle (byte-identical generated-parser
output vs the current tower) as the wave's born-RED gate.

**Prior art, cited:** aegraph as production mid-end IR — Fallin, *The acyclic e-graph*
(cfallin.org/blog/2026/04/09/aegraph), RFC-27 (`bytecodealliance/rfcs` `accepted/
cranelift-egraph.md`), ægraphs (EGRAPHS 2023). Equality saturation + rebuilding — egg (Willsey,
Nandi, Panchekha, et al., POPL 2021). Datalog/EqSat unification + seminaive schedule — egglog
(Zhang et al., PLDI 2023). Non-destructive rewriting as an MLIR dialect — *eqsat: An Equality
Saturation Dialect* (arXiv:2505.09363) — the direct precedent for "IR-level e-graph." Extraction
NP-hardness + ILP + differentiable/heuristic escapes — Tensat (MLSys 2021), SmoothE (ASPLOS
2025), e-boost (ICCAD 2025) — cited to justify *avoiding* general extraction here (multiplicity
~1, §2). Rewrite-confluence cautionary — GHC RULES.

---

## 9. Named risks & counterexamples

- **Multiplicity is NOT ~1.** If any live grammar produces child-bearing classes with >=2 costed
  forms, greedy elaboration is sub-optimal at those sites and 2B owes a bounded local extraction
  (the NP-hard wall in miniature). Mitigation: the PROVE-B histogram is the FIRST measurement;
  the finding scopes any extraction to enumerated sites, never whole-graph.
- **Teardown blows the budget.** The `memo`/`classes` drop can dominate on small grammars
  (the pathology `egraph.rs:70-77` itself documents). Counterexample: a 300-node grammar whose
  1024-bucket table teardown costs more than the whole current compile. Mitigation:
  `with_capacity(exact)`; PROVE-B benches teardown as a separate line item.
- **Payload interning changes hashing cost.** Moving `Vec<u8>`/`String` to `StringId` shifts work
  to an intern table; if the intern table's own teardown is unbounded the saving is illusory.
  Mitigation: shared StringId arena across the compile, measured.
- **Cycle assumption violated by a pathological grammar.** A grammar that inlines a rule into
  itself pre-tower (left-recursion elimination producing a self-referential body) would break
  acyclicity. Counterexample to hunt: any grammar where an `Expr` body transitively references
  its own `ExprId`. Mitigation: assert acyclicity per body at intern time; a violation routes to
  the general (fixpoint) `Extractor`, documented, not silently mis-elaborated.
- **XL-B overreach.** A 2B that dissolves the tower's levels violates XL-B and the ~230 us
  budget. The aegraph framing (levels kept, each represented as a graph) is the guard; a
  "one saturating soup" prototype is refused before it is benched.
- **Determinism regression via cost ties.** If the elaboration tie-break is ever map-probe order
  (not a total structural key), `regen o regen` diverges. The idempotent-regen gate catches it;
  the total-ordered lattice `Ord` prevents it.

---

## 10. What PROVE-B must test to move this family

1. **Multiplicity histogram** (decisive): per grammar, `max_eclass_size`, `#{classes >=2 nodes}`,
   and the node-kind of every multi-node class, after running the five rewrite groups to
   saturation. Confirms or refutes greedy-elaboration sufficiency (§2). Concrete artifact.
2. **Organ budget**: cold-per-parse build+saturate+elaborate+teardown on all three grammars vs
   the extant compile path; teardown as a separate line (§4). Falsifiable ~230 us gate.
3. **Byte-identity**: the elaborated `BackendIr` drives codegen to a generated parser that is
   *byte-identical* to today's on all three grammars (`cargo xtask check-*`), and `regen o regen`
   is idempotent (§4, precept 53).
4. **Cost-flip**: exhibit one real site where a fitted coefficient flips the elaborated `GShape`
   (a `SelfLoop` scalar<->`Prefilter` at PROVE-01's 0.115 cyc/B floor, or a per-grammar
   `EagerTape`<->`SinkOnly`), paired A/B-differential (§6). Seeds:
   `decision_egraph_rewrite_changes_selected_shape` (`passes/src/lib.rs:2014`).
5. **`admits_sink_only` census** (§7): report whether any sub-rule is dual-context; null => Q4
   closed context-free, positive => scoped repair enumerated.
6. **Widening coexistence**: land one widening rule (`alt->dispatch` or a bounded unroll) alongside
   the narrowing set, show the graph keeps both forms and elaboration picks correctly with a
   per-rule semantics oracle — no confluence proof owed (§3).
