# LOCK-2 · Family 2A — EGRAPH-ISLANDS · pass-2 research

*A directed pass pipeline with e-graph islands at named decision points (regex/HIR
canonicalization, scan-plan selection, projection canonicalization); every island carries a
same-wave consumer and a cost model; a CSP selects across extracted candidates. This report
makes the mechanism concrete against the as-built substrate, answers the pass-2 questions with
measured pointers, and states exactly what a prototype must decide.*

---

## 1. Thesis and the island contract

The family's claim is narrow and testable: graph optimization does **not** own the middle IR
(that is a sibling family's claim, and XL-B forbids redrawing the tower). It appears as
**bounded islands** — a build→saturate→extract→teardown organ dropped at one named decision
point, consuming a rich form and emitting a chosen form, then torn down. The pipeline between
islands is ordinary directed passes. An island is admissible only when four predicates hold, and
this quadruple **is** the family's governing law:

1. **Multiplicity ≥ 2 at a child-bearing node** after saturation — otherwise the island is
   `argmin` wearing an e-graph costume (see §3; the backend-shape island refutes itself here).
2. **A same-wave consumer** reads the extracted form in the same tranche (Y.13; constitution
   viii — consumer-less substrate forbidden).
3. **A cost model that is a *total order*** (regen determinism, §4.3).
4. **A CSP selects across sites** only where sites couple (the generated-size budget); where
   sites are independent the island's own cost-ranked extraction is the whole decision.

An island failing (1) is demoted to a plain cost-ranked pass; an island failing (2) is deleted.
This is the parsimony the constitution (vi) demands and the concrete discipline that separates
2A from a sibling's "IR is one e-graph end-to-end."

---

## 2. The substrate as built — two islands already exist

The engine is not hypothetical. `crates/egraph/` (1885 LOC hand) is a domain-agnostic
equality-saturation library: hash-cons `EGraph<N,A>` (`crates/egraph/src/egraph.rs:29` —
`classes: Vec<EClass>`, `memo: FxHashMap<N,Id>`), union-find (`unionfind.rs`), `Language` trait
(`language.rs`, auto-derivable via `crates/egraph-derive/`), additive `Rewrite`/`RewriteFn`
(`rewrite.rs:34,75` — apply never deletes, LLVM-style `Changed`-by-work-delta), greedy
bottom-up `Extractor` (`extract.rs:113`), and **two** schedulers: `BackoffScheduler`
(`scheduler.rs`) and the CSP-backed `CspScheduler` (`csp_scheduler.rs:174`). The two live
islands:

### Island A — grammar/regex HIR canonicalization (child-bearing, real)
`crates/ir/src/egraph/` builds a genuine e-graph over `GrammarENode` (Alt/Seq/Repeat/Regex/
Literal/Ref/Map), saturates with `CspScheduler::from_config` (`egraph/mod.rs:102`), extracts via
`write_back_optimized` under `GrammarCostModel` (`egraph/cost.rs`, embedding the shared
`CALIBRATED_WEIGHTS`). Rules are child-bearing and mostly **narrowing**:
`crates/ir/src/egraph/rules/universal.rs` — G1 `AltOfSingle`, G2 `RepeatOfSingle`, G3
`WrapOfEpsilonScalar`, G4 `ConcatLiterals`; `rules/suffix.rs:39` `CommonSuffixFactor`;
`rules/regex.rs` — `DeduplicateAltBranches`, `SupersetAbsorbAlt`, `UnionMergeAlt`, and the one
**representation-widening** rule, `FuseAltRegexBranches` (`Alt([Regex a, Lit b, Regex c])` →
`Regex("a|b|c")` — fewer nodes but a *larger* interned string). Regex-level work delegates to
`bbnf_regex::algebra`, so grammar-tier and HIR-tier rules share the same superset/union
primitives — the XL-C bridge is already physical.

### Island B — backend-shape selection (childless, degenerate — the counterexample)
`skinny/crates/passes/src/backend_egraph.rs:36` `select()` builds
`EGraph<DecisionNode, NoAnalysis>`, adds N candidate leaves (`children: [Id;0]`,
`backend_egraph.rs:158`), **unions them all into one class** (`:65-69`), runs one idempotent
rewrite `NormalizeDirectSinkCost` (`:191`, guarded by an `ends_with("#direct-sink-normalized")`
sentinel), and extracts the cheapest `DecisionCost` (`:239` — a 6-tuple ending in
`candidate_hash: u64`). This island has **no congruence sharing** — every node is childless, so
extraction is exactly `argmin` over 2–5 leaves. `decision_csp.rs:29` then re-solves the *same*
choice as a **1-variable** `Csp<CostFiniteDomain>` with 5 predicate constraints (recognizer/
substrate/simd/capacity/selected), which is an *attestation* layer (it fixes the variable to the
already-selected index, `:52`), not a search. **Island B is the family's own refutation of
predicate (1): where nodes are childless and multiplicity is a fixed candidate list, the e-graph
and the CSP are ceremony over a sort.** The pass-2 design must either give Island B child-bearing
structure (share sub-decisions across rules — Q4) or demote it to a cost-ranked pick.

---

## 3. The three named islands, made concrete

**I1 — regex/HIR canonicalization.** Already Island A. Child-bearing; multiplicity real at
`Alt`/`Seq` classes after fusion. Consumer: `write_back_optimized` → codegen dispatch. Cost:
`GrammarCostModel`. Ships as-is; pass-2 work is the total-order fix (§4.3) and measuring
multiplicity (Q1).

**I2 — scan-plan selection (the new island, XL-F territory).** Today the scan choice is a
*hand-written structural predicate* in `passes::derive_recognizers` (R1 §8; `ir/src/lib.rs:425`
carries `enum SimdMode {Exact}` / `SimdSite {PreEntry}`, extended by GN-2.W1 to `{Exact,
Prefilter}` / `{PreEntry, SelfLoop{terminators}}`): "a `Repetition` over a char-class whose
complement cardinality ∈ [1,16] is a self-loop scan site → emit `SimdMode::Prefilter`." **The 2A
question is whether this predicate becomes an island or stays a predicate**, and the evidence
says: the scan choice is a *canonicalization, not a decision* — when the complement ≤ 16 the
prefilter floor (0.115 cyc/B, SPEC §0 row 01) dominates the membership scan (~1.03 cyc/B)
unconditionally, and when it is > 16 or empty there is no terminator set and `Exact` is forced
(GN-2 §7 first counterexample). Multiplicity ≈ 1. **Therefore I2 is a lowering predicate, not an
e-graph island** — the XL-F supersession relation is **replaces** (predicate stands; no island
built) *unless* the prototype finds a fleet site with a genuine Exact-vs-Prefilter tie (Q5).

**I3 — projection/emit-spine canonicalization (residualization).** PROVE-12 (GO, CONFIRMED;
SPEC §0 row 12) proves this island *pays*: the CSS component-delimiter test const-folds to a
`u128` bitmask at generation time (`find_component_delim<const DELIMS:u128>`), disasm-verified as
an immediate `and` (PROVE-12/REPORT §3), every corpus/lane winning (cyc/B 0.80–0.91×, ins/B
−4.8/−6.8%, +18 generated LOC). This is a *staged* rewrite: a grammar-constant membership test
moves runtime→comptime. In e-graph terms it is a widening rewrite (manufacture the const-folded
alternative alongside the runtime `contains`) resolved by a cost model charging runtime
slice-dispatch. **But PROVE-12 achieved it with template const-generics + monomorphization, not
an e-graph** — the honest finding: for a *grammar-constant* fold the e-graph is unnecessary;
monomorphization is the residualizer. I3 earns an e-graph only where the fold choice is
non-obvious (competing residualization depths with a static-size trade-off — Q7).

---

## 4. The pass-2 questions, answered with evidence

### Q1 — Saturated multiplicity (the crux)
The two islands answer oppositely. **Island B**: multiplicity = candidate count (2–5), all
childless — extraction is argmin, the e-graph contributes nothing a `Vec::sort` would not.
**Island A**: multiplicity > 1 arises genuinely at `Alt`/`Seq`/`Repeat` classes after
`FuseAltRegexBranches`/`ConcatLiterals`/`CommonSuffixFactor` install alternatives — *this* is
where extraction is a real decision (keep the fused `Regex` vs the branch list). **What follows:
an island is justified only where post-saturation multiplicity ≥ 2 lands on child-bearing
nodes.** The load-bearing measurement owed: instrument `EGraph::classes()` after
`build_and_saturate` on the json/css-l4/sheets fleet and report, per node-kind, mean/max e-class
size and the fraction of *child-bearing* classes with size ≥ 2. Prediction from the rule set
(narrowing-dominant, one widening rule): multiplicity clusters near 1 at leaf classes, > 1 only
at `Alt` classes touched by fusion — the island earns its cost on Alt-heavy grammars (CSS L4,
sheets), near-degenerate on JSON.

### Q2 — Widening rewrites (coexistence with narrowing)
The substrate already coexists one widening rule (`FuseAltRegexBranches`) with eight narrowing
rules under one extraction. The three obligations:
- **(a) Confluence.** Trivially held: e-graph `apply` is *additive* (`rewrite.rs:70` — `add`
  then `union`, never delete). The e-graph represents *all* forms simultaneously; extraction
  chooses. Widening and narrowing cannot conflict because neither destroys the other's form —
  the precise structural advantage over destructive InstCombine passes (and over the two
  destructive passes `simplify_regex_algebra`/`merge_regex_alts`, 802 LOC, *deleted* once the
  additive rules reached parity — `rules/regex.rs:5-8`).
- **(b) Termination.** Bounded by `CspScheduler.node_limit`/`iter_limit`
  (`csp_scheduler.rs:178`, from `CostConfig`) plus per-rule *idempotency guards* — the
  `NormalizeDirectSinkCost` sentinel (`backend_egraph.rs:205`) and the fixed-point
  `Changed`-by-work-delta (`rewrite.rs:124`). A widening rule must carry such a guard or it
  re-fires forever; this is the concrete termination contract.
- **(c) Semantics.** The widened alternative must be observationally equal on all inputs (a
  prefilter skipping an inert run ≡ the exact scan; PROVE-12's `byte<128 ⇒ (DELIMS>>byte)&1 ≡
  contains`, REPORT §5). Verified by parity oracle, not asserted.
- **Precedent.** egg/egglog ship widening (`x → x*1`, AC that *grow* the graph) alongside
  narrowing extraction as standard practice (Willsey et al., *egg*, POPL 2021); the cost model is
  the sole arbiter. Cranelift's aegraph (Fallin, 2023) restricts to acyclic *elaboration* to
  bound blowup — the relevant lesson for teardown budget (Q3).

### Q3 — Organ port budget & regen byte-identity
**Budget.** The type-plane organ derives its schema in ~230 µs compile-time (SPEC §0 row 08).
The grammar e-graph organ's dominant cost is *teardown*, not build: the small-grammar profile
attributed 25.52% self-time to `drop_in_place` on the ~300-node BBNF case
(`egraph/mod.rs:73-76`), mitigated by hash-cons pre-sizing (`EGraph::with_capacity` from summed
`count_nodes`) and the Fx-hasher switch across scratch maps (`csp_scheduler.rs:44-49`,
`rewrite.rs:18-24`). The organ holds the per-parse-order compile budget only *with* pre-sizing;
a prototype must re-measure build+saturate+extract+teardown on the fleet and confirm
sub-millisecond per grammar.

**Determinism (the sharp risk).** `Extractor::compute_best` (`extract.rs:134`) uses strict
`cost < *existing` (`:156`), so *first-visited wins ties*, and visitation is `egraph.classes()`
= the `classes: Vec<EClass>` in **insertion order** (deterministic, *not* HashMap iteration — the
`best: FxHashMap` is point-queried only, never iterated for a decision). **But** tie-break
correctness then depends on Vec insertion order, and `GrammarCostModel` returns `f64` — a
`PartialOrd`, not a total order. **Any change to IR node count, interning order, or hasher
silently re-orders insertion and can flip an f64 tie.** `DecisionCost` (`backend_egraph.rs:239`)
already does this right — a 6-field `Ord` tuple ending in `candidate_hash: u64` (FNV of the id),
a total order with an explicit final discriminator. **Pass-2 law: every extraction cost must be a
total order whose final tie-break is a canonical structural hash of the e-node — never
Vec-insertion order and never HashMap iteration.** `regen∘regen` byte-identity holds iff this
holds; it is currently *fragile* for `GrammarCostModel`.

### Q4 — Context-sensitive extraction (the genuine open problem)
`admits_sink_only(rule_ir, target)` (`passes/src/lib.rs:954`) = `contains_direct_build(expr) &&
(target.direct_only_output || target.direct_build_consumer)`. The backend-shape decision is keyed
on **`RuleId`** (per-rule, `backend_egraph.rs:36`), and target features are **whole-compile**,
not per-caller. So a sub-rule R reached under *two* callers with different substrate demands (one
sink-only, one offset-tape) resolves to **one** global shape for R — the conflict is unmodeled.
This is exactly the context-sensitive extraction gap: e-graph extraction is context-**free** (one
best-node per class, globally), while the shape choice is context-**sensitive** (depends on the
caller). **What the prototype must audit:** walk the fleet `CallRule` graph and find any rule
invoked under ≥2 distinct substrate contexts. If **none** (likely JSON/sheets; CSS L4's shared
component sub-rules are the suspect), context-free per-class extraction is sound and 2A ships it
unmodified. If one exists, the identity-preserving repair is **sub-rule specialization keyed on
caller substrate** (clone R into R@sink / R@offset — bounded generated-size cost), the e-graph
analog of *colored e-graphs* (Singher & Itzhaky, 2023) or *guided/contextual equality
saturation* (Koehler et al., 2024); Cranelift's aegraph solves it by *elaboration* per use-site.
Complexity: linear in distinct (rule × context) pairs, which the generated-size budget (Q7)
bounds.

### Q5 — IR-modeled competing scan sites
The IR models scan sites via `SimdMode`/`SimdSite` (R1 §8). Today only `Exact`/`PreEntry` are
built; GN-2.W1 adds `Prefilter`/`SelfLoop{terminators}` (SPEC §0 line 111). Fleet candidate
sites: JSON whitespace-skip (complement = 6 structurals + quote ≤ 16 → *qualifies*, GN-2 §7),
JSON string-content scan, CSS component-delim scan (PROVE-12's residualized loop). **These are
*independent* sites (distinct rules), not coupled — a near-tree decision graph.** The binding
cost facts are real (0.115 cyc/B prefilter floor vs ~1.03 membership; 0.78–0.84 classify floor,
SPEC §0), but each site has **one** dominant plan given its complement cardinality — multiplicity
≈ 1 per site (Q1/I2). **The IR extension is `SimdSite::SelfLoop` (LOCK-3 owns building the node;
LOCK-5 owns any regex-leaf variant; LOCK-2 owns the cost model that ranks Exact-vs-Prefilter).**
2A's honest position: unless the prototype finds a site with a *binding* Exact-vs-Prefilter tie
(complement near the 16 boundary where fast-path fixed cost erases the skip win), scan selection
is a lowering predicate, not an island. The refutation laws pre-close the alternatives: no
multi-block schedule (PROVE-04), no digit-scan SIMD (PROVE-02).

### Q6 — Solver shape (near-tree: PBQP vs B&B vs e-graph)
The cross-site decision — independent per-site plan choice + a single global generated-size
budget + a few coherence edges (context-sensitivity, Q4) — is textbook **PBQP** (Partitioned
Boolean Quadratic Programming; Scholz & Eckstein, *Register Allocation for Irregular
Architectures*, 2002; LLVM RegAllocPBQP). Per-node discrete choice with pairwise cost matrices is
*exactly* the shape: each scan/projection site is a PBQP node with a small discrete domain
(Exact/Prefilter/residualization-depth), pairwise matrices near-empty (sites independent), the
budget one global `bounded_sum`. On this graph class PBQP's per-node argmin is **optimal in the
uncoupled case** and its heuristic reduction is optimal on trees; generic B&B over-generalizes
(exponential worst case for no benefit); full e-graph extraction is the wrong tool (it solves
candidate *generation*, not cross-site selection). **Division of labor 2A proposes: e-graph
islands *generate* per-site candidates (I1/I3); a PBQP/CSP *selects* across sites under the
budget.** The current `decision_csp.rs` 1-variable-per-rule CSP is PBQP degenerate (no edges);
pass-2 adds the budget edge and, if Q4 finds conflicts, the coherence edges. Solve-cost evidence
owed: PBQP-reduce vs the current CSP `nodes_explored`/`solve_us` (`decision_csp.rs:149,101`) on
the full-fleet decision graph.

### Q7 — Budget knapsack as constraint
The `generated_loc_budget` (GN-2 per-wave: W1 ≤ +50, W2 ≤ +40, W3 ≤ +25 generated LOC) is a
`bounded_sum` knapsack over per-site residualization/lowering depth. Folded as a **CSP
constraint** at the named sites it preserves **selection-equivalence** with the `PriorityStep`
P1–P8 cascade (`ir/src/cost.rs:264`) **iff the cascade is monotone** — which it is (each step a
strict refinement; `priority_data_role: "evidence-only"`, `decision_csp.rs:178`, confirms the
cascade is already demoted to evidence and the CSP is authoritative). **One solver home:** the
`csp-solver` crate (`crates/csp-solver/`, vendored from csc411), already home to `decision_csp.rs`
*and* the e-graph scheduler (`csp_scheduler.rs` uses `csp_solver::Csp`) — the same constraint
store a sibling type-CSP would share. The coordination point is a single `Csp<CostFiniteDomain>`
per compile with the budget as a `LambdaConstraint`/`bounded_sum` over all site variables.

### Q8 — Profile-fed cost (2A as consumer of a sibling's fit)
`CALIBRATED_WEIGHTS` (`cost_weights.rs:175`) are hand-set `f64`: `structural 1.0,
alt_per_branch 1.5, dispatch_bonus −2.0, call_overhead 4.0, dispatch_table 3.0, ...`. A
coefficient fitted from measured cyc/B **can** flip a real decision (e.g. `dispatch_table 3.0`
vs `call_overhead 4.0` governs inline-vs-dispatch at Alt sites). **Attribution is identifiable
via A/B-differential** — the PROVE benches are *already paired interleaved* (PROVE-12 "baseline
vs modified interleaved back-to-back"), so per-feature coefficients are recovered from the
interleaved Δ, not from whole-grammar cyc/B (which underdetermines them). The **freshness gate**
exists: `ActiveCostFacts.selected_cost_freshness` (`backend_egraph.rs:127`) goes "stale"→RED on
mutation. **Env-source fencing:** the fit must be *committed as a const fact table* (supersede
`CALIBRATED_WEIGHTS` in place), never read from env at regen — else `regen` is non-deterministic
(constitution iv/vii). A fit would supersede the hand-set structural/dispatch/table knobs; the
island cost models already read `CALIBRATED_WEIGHTS` through `GrammarCostModel`/`DecisionCostModel`,
so replacing the const is transparent to the islands (a one-line seam).

---

## 5. Prior art (citations)
- **egg** (Willsey, Nandi, Wang, Flatt, Tatlock, Panchekha; POPL 2021) — additive rewrites,
  deferred extraction, e-class analyses; direct ancestor of `crates/egraph/`.
- **egglog** (Zhang et al., PLDI 2023) — datalog + e-graph, staged/scheduled saturation; the
  reference for the `CspScheduler` dirty-frontier design.
- **Cranelift aegraph** (Fallin, 2023) — acyclic e-graph + *elaboration* for context-sensitive
  extraction and bounded teardown; the model for Q3 teardown and Q4 context repair.
- **Colored e-graphs** (Singher & Itzhaky, 2023) and **Guided Equality Saturation** (Koehler,
  Goens, Bhat, Grover, Steuwer, POPL 2024) — context-carrying extraction; the Q4 repair
  precedent.
- **PBQP register allocation** (Scholz & Eckstein 2002; Hames & Scholz 2006; LLVM RegAllocPBQP) —
  the near-tree per-node-choice + pairwise-matrix solver for Q6.
- **LLVM InstCombine / MLIR canonicalization** — destructive-narrowing baseline the additive
  e-graph supersedes (the deleted 802-LOC destructive passes, `rules/regex.rs:5`).
- **regex-automata** (BurntSushi) — the prefilter/DFA lineage feeding I2's scan candidates.

## 6. Named risks & counterexamples
- **R1 — Island B is ceremony.** Childless decision leaves + a 1-variable CSP add cost over a
  sort (§2). *Counterexample that would save it:* Q4 finds shared sub-rules under conflicting
  substrates, giving the decision graph real edges — then PBQP/CSP earns its place. If Q4 finds
  none, demote Island B to a cost-ranked pick and delete the CSP attestation.
- **R2 — f64 tie non-determinism.** `GrammarCostModel`'s f64 with Vec-insertion tie-break (§4.3)
  can flip `regen∘regen`. *Fix:* total-order cost with a structural-hash final field.
- **R3 — I2/I3 are predicates, not islands.** If fleet multiplicity ≈ 1 at scan/projection sites
  (Q1/Q5), building e-graph islands there is over-engineering; PROVE-12 already residualized I3
  with const-generics, no e-graph. *The family must not build islands the measurement refutes.*
- **R4 — teardown blows the budget.** The 25.52% `drop_in_place` finding (§4.3) recurs on
  Alt-heavy CSS L4 if pre-sizing is imperfect; a bounded-elaboration (aegraph) fallback is the
  named mitigation.

## 7. What a prototype must test to move family 2A
1. **Multiplicity census (decisive).** Instrument `EGraph::classes()` post-`build_and_saturate`
   on json/css-l4/sheets; report per-node-kind mean/max e-class size and the count of
   *child-bearing* classes with size ≥ 2. **Gate:** if child-bearing multiplicity ≥ 2 is
   effectively absent, I1 collapses to a canonicalization pass and the family's e-graph surface
   shrinks to I1-on-CSS only; if present, the island is justified and Q4 becomes live.
2. **Context-conflict audit.** Walk the `CallRule` graph for any rule under ≥2 substrate contexts
   (Q4). Produces the yes/no that decides context-free vs specialized extraction.
3. **Determinism harness.** Run `regen∘regen` after perturbing IR node count / hasher; prove
   byte-identity holds only with the total-order cost (§4.3). Must go RED before the fix, GREEN
   after.
4. **PBQP vs CSP solve-cost.** Build the full-fleet cross-site decision graph (independent sites
   + budget edge); compare PBQP-reduce vs `decision_csp.rs` `nodes_explored`/`solve_us`.
5. **Scan-tie probe (I2 falsifier).** Sweep a self-loop complement cardinality across the 16
   boundary and measure Exact-vs-Prefilter cyc/B to find (or refute) a *binding* tie — the single
   fact deciding whether I2 is an island or a predicate (XL-F relation).

## 8. Integration seams (path:line where extant code serves)
- Engine: `crates/egraph/src/{egraph.rs:29, extract.rs:113, csp_scheduler.rs:174, rewrite.rs:34,
  cost_weights.rs:175}`; derive `crates/egraph-derive/src/lib.rs`.
- Island A (HIR canon): `crates/ir/src/egraph/{mod.rs:60 build_and_saturate, write_back.rs:34,
  cost.rs GrammarCostModel, rules/{universal,suffix,regex}.rs}`.
- Island B (backend shape): `skinny/crates/passes/src/backend_egraph.rs:36`; CSP finalize
  `skinny/crates/passes/src/decision_csp.rs:29`; facts
  `skinny/crates/ir/src/cost.rs:{173 ActiveCostFacts, 203 DecisionCspFacts, 264 PriorityStep}`.
- Scan predicate (I2 / XL-F): `passes::derive_recognizers` (R1 §8; `ir/src/lib.rs:425`
  SimdMode/SimdSite); consumer `codegen/src/lower/recognizer/` (GN-2.W1 skeleton).
- Residualization (I3): PROVE-12 template `crates/codegen/src/runtime_generator.rs`
  (`CSS_GENERATED_RS`, `find_component_delim<const DELIMS:u128>`).
- Cost/shape seam: `passes::derive_backend_shape` + `admits_sink_only`
  (`skinny/crates/passes/src/lib.rs:954`); `BackendShape` (`ir/src/lib.rs:361`,
  `all_backend_shapes()` `cost.rs:334`).
- Solver home: `crates/csp-solver/` (`Csp`, `CostFiniteDomain`, `LambdaConstraint`,
  `OptimizationMode::MinimizeCost`).
