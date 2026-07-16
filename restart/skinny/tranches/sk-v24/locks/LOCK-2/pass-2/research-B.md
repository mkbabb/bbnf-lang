# LOCK-2 · PASS-2 · FAMILY 2B — IR-IS-EGRAPH

*The middle IR is an e-graph end to end; passes are rewrite groups; equality saturation under a
schedule; extraction emits `BackendShape` directly. This report develops that thesis to its
strongest form on the measurement substrate, names where the substrate refutes it, and states
exactly what a prototype must prove to advance it.*

---

## 1. The mechanism made concrete

2B collapses the current three-stage decision spine — `derive_backend_shape` (priority cascade)
→ `backend_egraph::select` (a degenerate per-rule e-graph) → `decision_csp` (a separate budget
CSP) — into **one whole-grammar e-graph** in which the tower's middle IR *is* the e-graph.

**The Language.** `BackendExpr` (`crates/ir/src/lib.rs:376`) is already a recursive sum with
`Box`/`Vec`/`Self` children — `Entry(inner)`, `Seq(Vec)`, `Alt{branches}`, `RepeatLoop{body}`,
`ValueProject{body}`, `OptionalBranch`, plus childless leaves (`ByteLiteral`, `RegexProgram`,
`CallRule`, `SpanMark`, `TapeEmit`, `Return`). This is *exactly* the shape `egraph_derive`
projects to `Id` (`crates/egraph/src/language.rs:16`, derive at `crates/egraph-derive/src/lib.rs`).
2B derives `Language for BackendExpr` mechanically: the whole grammar interns into ONE
`EGraph<BackendExpr, GrammarAnalysis>` where `CallRule` edges make rule references first-class
e-class children (a cyclic e-graph — see §5 risk R3).

**Passes as rewrite groups.** Each analysis/normalization pass becomes a `Rewrite`
(`crates/egraph/src/rewrite.rs:34`): dedup-alt-branches, superset-absorb-alt, union-merge-alt,
fuse-alt-regex, common-suffix-factor (the five today referenced at `cost_weights.rs:154-160`),
plus the widening rules 2B must add (§2): self-loop-recognize (`SimdMode::Prefilter` /
`SimdSite::SelfLoop`, GN-2.W1), residualize-const-fold (PROVE-12), and sink-vs-tape realization.
Nullability (`ir/src/lib.rs:320`) and first-set become e-class `Analysis` lattices
(`crates/egraph/src/analysis.rs`), computed by congruence during rebuild, not as standalone
walks.

**Saturation under a schedule.** `CspScheduler` (`crates/egraph/src/csp_scheduler.rs:175`)
already drives saturation with an AC-3 dirty-frontier: each e-class is a CSP variable carrying a
monotone `DirtyDomain`, each parent edge a `ParentDirtyProp` constraint, one `csp.propagate()`
computes the transitive dirty closure, `run_on_dirty` re-probes only dirtied classes. Iteration
caps and node caps come from `CostConfig` (`crates/egraph/src/cost_config.rs`,
`egraph_iter_limit`/`egraph_node_limit`). This is the schedule; it needs no invention, only the
richer rule set pointed at the whole IR.

**Extraction emits `BackendShape` directly.** After saturation, `Extractor`
(`crates/egraph/src/extract.rs:113`) computes per-class best-node bottom-up under a cost model
whose `Cost` is a `Lattice` (`extract.rs:36`, already Pareto-ready via `Scalar<T>` and the
multi-objective `join`/`dominated_by` surface). Each class's chosen e-node names its realization;
walking `extract_tree` (`extract.rs:190`) reconstructs the `BackendIr` — and where the winning
e-node is a shape-bearing variant, that IS the per-rule `BackendShape` (`ir/src/lib.rs:361`, the
five: `EagerTape|OffsetTape|EventTape|SinkOnly|CollapsedStage`). No separate cascade, no separate
select step.

**The rebuild discipline** is egg's deferred-invariant rebuild (`egraph.rs:251`): unions push to
a worklist, `rebuild()` re-canonicalizes parents and re-memos to a fixpoint — Willsey et al
(egg, POPL 2021). Work-done is measured LLVM-`Changed`-style (`rewrite.rs:66-70,91`): the sum of
`total_nodes` growth and `union_count` delta, not a structural hash — this is the correct
saturation fixpoint and already implemented.

---

## 2. Q2 — widening rewrites (the load-bearing question for 2B)

2B lives or dies on whether the IR-as-e-graph manufactures genuine alternatives. The current
rule set is *all narrowing* (dedup/absorb/merge/factor) — it can only shrink a class toward one
form. The **sole widening precedent already in-tree** is `NormalizeDirectSinkCost`
(`passes/src/backend_egraph.rs:191`): it *manufactures* a second e-node (`#direct-sink-normalized`
with zeroed `static_size_cost`/`shape_rank`) and unions it in. Its properties establish the
coexistence template 2B generalizes:

- **(b) termination.** The searcher skips any class already carrying the normalized variant
  (`backend_egraph.rs:203-207`, `ends_with("#direct-sink-normalized")`) — a one-shot widening
  guard. Generalized: every widening rule needs a *fuel* or *idempotence marker* so saturation
  cannot loop. This is the standard egg discipline for expansive rules (Willsey §5.2 "rule
  scheduling / bans").
- **(c) semantics preservation.** The manufactured node is a re-costing of the *same*
  `BackendShape::SinkOnly` — semantics-identical by construction. PROVE-12 residualization is the
  same shape: const-folding the delimiter test (`REPORT §5`, byte-identical checksum) is a widening
  rewrite whose two forms are provably equal for `byte < 128`. So widening-that-preserves-meaning
  is already demonstrated twice on the substrate.
- **(a) confluence with narrowing.** Confluence is NOT required for correctness in an e-graph —
  both forms coexist in the class and extraction picks; this is precisely egg's argument that
  equality saturation sidesteps phase-ordering (Tate et al, Peggy, POPL 2009). What IS required:
  the widening rule must reach a fixpoint (guard above) and the narrowing rules must not *delete*
  the widened alternative (they cannot — rewrites only `add`/`union`, never remove;
  `rewrite.rs:5-6`).

**Shipping precedent for widening+narrowing coexistence:** Cranelift's ægraph (Fallin, 2023) is
the strongest — it manufactures algebraic alternatives (strength-reduction *expansions*, GVN
*contractions*) in one saturating IR and lets scoped elaboration extract; egglog (Zhang et al,
PLDI 2023) formalizes expansive + contractive rules under seminaive scheduling; GHC's simplifier
runs INLINE (widening) beside case-of-case (narrowing) to a fixpoint. The precedent is settled;
the open question is entirely empirical multiplicity (§3).

---

## 3. Q1 — saturated multiplicity (the substrate's verdict, and it is adverse)

**The certified counterexample lives in-tree.** `CALIBRATED_WEIGHTS`
(`crates/egraph/src/cost_weights.rs:134-167`) records a grid sweep of 54 cost configurations over
the 4-grammar corpus (JSON, BBNF, CSS L4, Sheets). The result is a **documented null**: DTA state
counts were *identical* to baseline across every configuration —

| Grammar | states | sweep variance |
|---|---:|---|
| JSON | 51 | {51} |
| BBNF | 476 | {476} |
| CSS L4 | 2875 | {2875} |
| Sheets | 191 | {191} |

The doc's own diagnosis (`cost_weights.rs:154-161`): the current rewrite set "produces e-classes
with **at most one semantically-distinct canonical form** for these grammars, so the cost model
has no choice to exercise." **Multiplicity ≈ 1 across the fleet today.** Where multiplicity is 1,
saturation is pure canonicalization and extraction is a no-op — the entire e-graph apparatus
reduces to hash-cons + dedup, which the `memo` table (`egraph.rs:32`) already gives at a fraction
of the build/teardown cost. The current `backend_egraph::select` confirms this concretely: it
adds each candidate as a **childless** `DecisionNode` (`children: [Id; 0]`,
`backend_egraph.rs:158,181`), unions them into one class, runs the single widening rule, and
extracts the argmin — an `O(n log n)` sort dressed as equality saturation. There is no congruence
because there are no children; the e-graph earns nothing over `candidates.min_by_key`.

**What follows for 2B.** "IR IS an e-graph end to end" is only *architecturally* justified where
extraction faces a real choice among ≥2 survivors **at child-bearing nodes**. The null result
says: with today's narrowing-only rules, that never happens. 2B's burden is therefore precise —
it must inject widening rules (§2) that create ≥2 semantically-distinct survivors *and* show they
land at child-bearing IR nodes (`Alt`, `RepeatLoop`, `Seq`), not at costing leaves. The candidate
sites are exactly the tranche's live decisions: scan-plan (`SelfLoop`-recognize vs scalar body,
GN-2.W1), residualize-vs-interpret (PROVE-12), sink-vs-offset realization
(`admits_sink_only`). If saturation with these rules still collapses to multiplicity 1, 2B is
**refuted end-to-end** and the honest reduction is 2A-style islands (e-graph only at the proven
>1 sites) — a named negative result, not a silent pass.

---

## 4. Q4 — context-sensitive extraction (audited: no conflict in the current fleet)

`admits_sink_only` (`passes/src/lib.rs:954`) is `contains_direct_build(expr) && (target
.direct_only_output || target.direct_build_consumer)` — a function of the **rule body + the global
`TargetFeatures`**, with *no caller-context argument*. `BackendShape` is stored per-`RuleId`
(`passes/src/lib.rs:96`, `HashMap<RuleId, BackendShape>`): one rule, one shape, regardless of who
calls it. Sink-only admission is therefore **context-free by construction** on the substrate — a
sub-rule reached under both a sink-only caller and an offset-tape caller receives ONE shape; the
model cannot express two. I found **no caller-context conflict** in the fleet: no per-class shape
is asked to serve two incompatible callers, because the per-`RuleId` map forbids the question.

What follows for 2B: context-free per-class extraction is *sound* on the current fleet — extraction
may emit one `BackendShape` per rule-class and never faces the identity-preservation repair. This
is a genuine simplification 2B inherits. The caveat is forward-looking: GN-2.W1's `SelfLoop`
recognizer attaches to a scan site; a whitespace-skip rule shared across a sink-only and an
offset-tape caller would get one recognizer — safe *only because* the recognizer is
caller-invariant (a structural skip is identical to both consumers). Should any future decision
become genuinely caller-dependent, 2B would need Cranelift-style **scoped elaboration** (Fallin
2023: extraction parameterized by the enclosing region), whose identity-preserving cost is a class
duplication per distinct caller-context — bounded, but no longer a plain per-class extraction. The
prototype must confirm the recognizer-invariance assumption holds for every shared scan site.

---

## 5. Q3 — organ port budget, determinism, teardown

**Budget.** The e-graph engine already carries an explicit teardown-cost design note
(`egraph.rs:68-77`): without a capacity hint the `FxHashMap` memo settles at the next power of two,
so a ~300-node grammar can leave a 1024-bucket table "whose teardown dominates the compile
profile"; `with_capacity` (`egraph.rs:78`) sizes memo + classes to occupancy to keep dropped
capacity proportional. This is the direct lever on the ~230 µs per-parse compile budget. The build
is `add`-per-node (`egraph.rs:155`, one `A::make` + clones per child edge); saturation is
`CspScheduler` bounded by `egraph_iter_limit`/`egraph_node_limit`; extraction is `compute_best`
fixed-point (`extract.rs:134`). **Whether a WHOLE-grammar e-graph (CSS L4 = 2875 DTA states) holds
the budget is unmeasured** — the current per-rule select builds a tiny graph (≤5 candidate nodes),
so no fleet number bounds the end-to-end organ. This is the single most important open measurement
(§7).

**Determinism / `regen∘regen` byte-identity.** Extraction's total order must be content-derived,
never HashMap-iteration-derived. Today it IS: `DecisionCost` (`backend_egraph.rs:239`) is
`Ord`-derived with a trailing `candidate_hash: u64` (FNV-1a over the candidate id,
`backend_egraph.rs:304`) as the terminal tie-break, so equal-cost ties resolve on content, not on
`FxHashMap` order; `Extractor::compute_best` iterates `egraph.classes()` in `Vec` insertion order
(`egraph.rs:138`, deterministic) and `improved = cost < *existing` is strict, so first-insertion
wins only on a true tie the hash already broke. **The admissible tie-break for a richer 2B cost is
the same: a content hash as the last lattice axis.** The risk is any `Analysis` that iterates an
`FxHashMap` to fold data — that reintroduces HashMap-order nondeterminism and would break
`regen∘regen`; 2B must keep all order-sensitive folds over `Vec`-ordered `classes()`.

---

## 5b. Q5 — IR-modeled competing sites, and the leaf-vs-child-bearing distinction

There ARE competing sites modeled (or about to be) in the IR, not merely in grammar source, with
real binding costs — but they sit at **childless leaves**, which is precisely why they justify 2A
islands more than end-to-end 2B. The extension that models them is GN-2.W1's:
`SimdMode{Exact}→{Exact,Prefilter}` and `SimdSite{PreEntry}→{PreEntry,SelfLoop{terminators}}`
(`ir/src/lib.rs:425-433`). Every self-loop scan site with complement-alphabet cardinality in
`[1,16]` presents ≥2 lowerings in the IR — scalar loop vs `Recognizer::SimdScan{Prefilter,SelfLoop}`
— with binding static-size (+~35 generated LOC/site) and cyc/B costs (0.115 floor vs ~1.0
membership scan; certified §0 #01). Regex leaves add a third: `RegexProgram` scalar-descent vs
`dfa.find` (§0 #10, 8.8–29.5×). **Which lock owns building it:** GN-2 (kernel plane) owns the
`SimdMode`/`SimdSite` enum extension + `derive_recognizers` (`passes/src/lib.rs:338`); LOCK-2 owns
the *extraction* that chooses among the survivors — and GN-2.W1's `codegen/src/lower/recognizer/`
dispatch that "emits exactly one lowering per site (self-loop `find_member` XOR regex-leaf
`dfa.find`, never both)" IS a per-class argmin expressed imperatively. The XL-F supersession
relation is therefore **wraps**: 2B does not delete that dispatch, it re-expresses the per-site
choice as a cost-model extraction so it composes with the shape + budget solve in one pass. The
sharp consequence for §3: because these ≥2-survivor sites are *childless leaves*, they exhibit no
congruence/sharing that a whole-IR e-graph could exploit over a flat per-site argmin — they are 2A's
warrant, not 2B's. 2B's distinct warrant requires ≥2 survivors at CHILD-BEARING nodes; §3's null
says the current rule set produces none.

---

## 5c. Q6 — solver shape on this graph class (separable ⇒ greedy optimal; near-tree+coherence ⇒ PBQP)

The per-site decision graph is: per-node discrete choice (≤5 `BackendShape`s or ≤3 lowerings) +
pairwise coherence (caller/callee compatibility edges) + one global budget coupling. This is
textbook PBQP (Scholz & Eckstein 2002; LLVM `RegAllocPBQP`): per-node cost vectors + per-edge cost
matrices. The graph-class analysis Q6 asks for:

- **No coherence edges (the audited current fleet, §4).** The problem is *separable* — each node
  minimizes independently and **greedy bottom-up extraction is provably optimal** (it *is* the
  separable minimum). No solver needed; this is 2B's cheap common case and the reason the greedy
  `Extractor` is the right default where the budget is non-binding.
- **Coherence edges + near-tree call graph.** PBQP's reduction solver is **optimal** on trees and
  series-parallel graphs (R0 removes independent nodes, R1/R2 reductions are exact; only R_N is a
  heuristic on an irreducible core) and runs near-linear — a *structural optimality guarantee* on
  exactly this graph class that generic branch-and-bound (exponential worst-case, no structural
  guarantee) and sharing-aware e-graph extraction (NP-hard in general — the reason greedy is used)
  do not provide. Solve-cost on a ~300-node near-tree with a handful of edges is microseconds; the
  separator vs B&B is the guarantee, not the speed.

So the honest solver layering is: e-graph saturation *generates* the menu; greedy extraction
*decides* the separable case optimally; PBQP (hosted in the one csp-solver store, §6) handles only
the residual coherence-coupled + budget-coupled core. This is §6's boundary made precise: 2B's
final selection is a solver on the non-separable residual, converging with LOCK-2's CSP centre.

---

## 6. Q6/Q7/Q8 seams as they bear on 2B (the CSP hand-off is forced, not optional)

**The sharing-sensitive budget is 2B's hard boundary.** `extract.rs:9-11` states outright that the
greedy bottom-up extractor "is not optimal for cost models that depend on sharing." The
`generated_loc_budget` (GN-2 §5, per-wave `≤ +N` generated LOC) and the `bounded_sum` knapsack
(Q7) are **global, sharing-sensitive** costs — the size of the emitted parser depends on which
sites share a lowering, which greedy per-class extraction cannot see. Therefore "extraction emits
`BackendShape` directly" **cannot** honor a global budget through the current extractor. 2B has
exactly two lawful completions:

1. **DAG-cost / ILP extraction** — replace greedy `compute_best` with an ILP or PBQP extraction
   over the saturated graph (egg's `lp_extract`; Yang et al DAG-cost; PBQP per Scholz & Eckstein
   2002; Denali's goal-directed extraction, Joshi/Nelson/Randall 2002). This keeps 2B
   self-contained but concedes that extraction is a solver, converging with LOCK-2's CSP centre.
2. **Hand off the budget to the one constraint store** — saturation generates candidates; a single
   CSP (the `decision_csp` seam today at `passes/src/decision_csp.rs`, shareable with LOCK-1's
   store) selects under the budget constraint. This is selection-equivalent to the priority
   cascade only if the cascade's `PriorityStep` order (`ir/src/cost.rs:264`, 8-step, `SinkOnly` at
   P2) is expressible as constraint precedence — checkable, and the prototype must check it.

Either way, **2B end-to-end is not budget-complete alone**; the strongest honest 2B is
"IR-is-e-graph for canonicalization + candidate generation, with a solver extraction (ILP/PBQP or
CSP hand-off) for the budget-coupled final selection." Pretending the greedy extractor closes the
budget would violate `extract.rs`'s own stated limitation.

**Profile-fed cost (Q8).** The fitted-coefficient path exists but has **never flipped a decision**:
`CALIBRATED_WEIGHTS` is a null (§3), precisely because multiplicity ≈ 1. `Measurement`
(`ir/cost.rs:306`, `cycles_per_byte_x1000` at :311) and `EvidenceSource` (`:319`) are the fact-table
slots; `ActiveCostFacts.selected_cost_freshness` + `candidate.stale`
(`backend_egraph.rs:127,169`) are the freshness gate. PROVE-12's real cyc/B deltas (0.81–0.98×)
came from residualization (a rewrite), *not* from a cost-fit flipping an extraction — the fit is
inert until §3's multiplicity exists. For a coefficient to flip a real per-site decision (Q8a), the
widening rules of §2 must first create the alternative for it to choose between. Attribution under
whole-grammar cyc/B underdetermination (Q8) is an A/B-differential problem (measure two forms of
one site, hold the rest fixed) — the `json_canon_bench`/`css_canon_bench` paired instruments
(GN-2 gates) are the vehicle; env-source must be fenced from regen by committing the fit as a const
fact table (the `CALIBRATED_WEIGHTS` single-slot pattern, `cost_weights.rs:169-174`), never read
from the environment at generation time.

---

## 7. What a prototype must test to move Family 2B

One prototype, on CSS L4 (richest: 2875 DTA states), base-pinned, HEAD named, cold per-parse:

1. **Derive `Language for BackendExpr`** and intern one whole grammar into a single
   `EGraph<BackendExpr, GrammarAnalysis>` (cyclic via `CallRule`). Prove `map_children`/rebuild
   terminate on the recursive grammar (the R3 cyclic risk).
2. **Add ≥2 widening rules** — self-loop-recognize (`SimdMode::Prefilter`), residualize-const-fold
   (PROVE-12 as a rewrite), sink-vs-offset realization — with idempotence guards
   (`NormalizeDirectSinkCost` template).
3. **Measure saturated multiplicity** (Q1): average/max e-class size, and the COUNT of classes with
   ≥2 semantically-distinct survivors **at child-bearing nodes** (`Alt`/`RepeatLoop`/`Seq`), not at
   costing leaves. *This is the make-or-break number.* If it stays ≈1, 2B reduces to 2A islands —
   report as a named negative result.
4. **Show extraction flips ≥1 real `BackendShape`** vs the priority cascade on a site where the
   cascade and the cost lattice disagree (else the e-graph is inert).
5. **Time build+saturate+extract+teardown** against the ~230 µs/parse budget with and without
   `with_capacity`, isolating teardown (`egraph.rs:68-77`).
6. **Prove `regen∘regen` byte-identity** through cost-guided extraction — content-hash tie-break,
   no `FxHashMap`-order fold.
7. **Demonstrate the budget boundary** (Q6): run greedy extraction under the `generated_loc_budget`
   and show it violates the cap on a sharing case, then show ILP/PBQP or the CSP hand-off restores
   optimality — establishing that 2B's final selection is a solver, converging with LOCK-2's centre.

---

## 8. Prior art (citations)

- **egg** — Willsey, Nandi, Wang, Flatt, Tatlock, Panchekha, "egg: Fast and Extensible Equality
  Saturation," POPL 2021. The deferred-rebuild + hashcons + e-class-analysis design the in-tree
  engine implements (`egraph.rs:251` rebuild, `analysis.rs`, `extract.rs` greedy); rule scheduling
  / bans for expansive rules (§2 termination). `lp_extract` = the ILP extraction for
  sharing-sensitive cost (§6).
- **Cranelift ægraph** — Chris Fallin, "ægraphs: acyclic e-graphs for the Cranelift mid-end" (2023).
  The strongest *shipping* "IR IS the e-graph end-to-end" precedent: one saturating IR, elaboration
  = scoped extraction emitting CLIF. Two load-bearing lessons for 2B: **acyclicity** keeps
  extraction linear (contrast §5 R3 cyclic grammar IR), and **scoped elaboration** is the
  identity-preserving repair if any decision becomes caller-dependent (§4).
- **egglog** — Zhang, Wang, Willsey, Tatlock, "Better Together: Unifying Datalog and Equality
  Saturation," PLDI 2023. Seminaive scheduling of expansive + contractive rules; analyses as
  lattices — the model for passes-as-rewrite-groups + `Analysis` folds.
- **Peggy / Equality Saturation** — Tate, Stepp, Tatlock, Lerner, POPL 2009. The original
  "whole program as one e-graph, extraction via a global solver (ILP/PBQP)" — the direct ancestor
  of 2B's end-to-end thesis AND its solver-extraction requirement.
- **Extraction as a solver** — RegAllocPBQP (Scholz & Eckstein, 2002); Denali goal-directed
  superoptimizer (Joshi, Nelson, Randall, PLDI 2002); DAG-cost / ILP extraction is NP-hard in
  general — motivates §6's PBQP/CSP hand-off for the budget.
- **GHC simplifier** — inline (widening) beside case-of-case (narrowing) to a fixpoint; and
  **LLVM GVN/SCCP** as monotone rewrite — precedent that widening+narrowing coexist in production
  optimizers (§2).

---

## 9. Verdict (SPEC-honest)

2B's **mechanism is real and mostly built**: the engine (`crates/egraph`, 3105 LOC), the derivable
`Language` (`BackendExpr`), the AC-3 saturation scheduler, the lattice-ready extractor, and a
working widening-rewrite template all exist. 2B's **weakness is empirical and certified**: the
`CALIBRATED_WEIGHTS` null proves the current fleet has multiplicity ≈ 1, so "IR IS an e-graph
end-to-end" is today an over-claim — the e-graph does dedup a hash-cons already does. 2B's **hard
boundary is architectural**: the greedy extractor cannot honor the sharing-sensitive
generated-size budget (`extract.rs:9-11`), so end-to-end 2B necessarily terminates in a solver
extraction (ILP/PBQP or a CSP hand-off), converging toward LOCK-2's CSP centre rather than standing
alone. The prototype in §7 is decisive: if widening rules produce ≥2 survivors at child-bearing
nodes and extraction flips a real `BackendShape` within budget, 2B earns "end-to-end"; if
multiplicity stays 1, the lawful reduction is 2A islands, reported as a named negative result — the
substrate, not the author, deciding.
