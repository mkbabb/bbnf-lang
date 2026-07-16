# LOCK-2 · PASS-2 · FAMILY 2A — EGRAPH-ISLANDS

*A directed pass pipeline with e-graph islands at named decision points; every island holds a
same-wave consumer and a cost model; CSP selects over extracted candidates. This report makes
the mechanism concrete against the living substrate, names its one governing pathology, and
states the single prototype that moves the family.*

---

## 0. The mechanism, made concrete — the island already exists (and is degenerate)

Family 2A's archetype is not hypothetical. One e-graph island is already wired at the
backend-shape decision point and is the reference for every other:

`skinny/crates/passes/src/backend_egraph.rs::select` builds `EGraph::<DecisionNode,
NoAnalysis>::with_capacity(ranked.len())` (`:60`) from the ranked `BackendCandidate` set,
**unions every candidate into one e-class** (`:65-69`), runs the scheduler over one rewrite rule
(`:73-81`), and extracts the winner via `Extractor::new(&graph, &DecisionCostModel)` +
`best_node(root)` (`:84-87`). It emits full provenance (`ActiveCostFacts`, `:115-146`) naming its
generated-selection path (`derive_backend_shape_with_diagnostics`) and its **same-wave consumer**
(`codegen::lower::rust::lower_to_rust`, `:11`). The candidate ranking that feeds it is produced by
`passes/src/lib.rs::derive_backend_shape_with_diagnostics` (`:407`); the per-rule driver is
`select(rule.id, candidates)` at `lib.rs:504`. The five `BackendShape` values it ranges over —
`EagerTape|OffsetTape|EventTape|SinkOnly|CollapsedStage` — live at `ir/src/lib.rs:361`.

So the substrate exists, has a consumer, has a cost model, and is per-rule (rule-local islands).
This is exactly 2A's shape. The problem 2A must solve is **not** "build islands"; it is that the
one built island **has no choice to exercise**.

### The governing pathology: saturated multiplicity ≈ 1 (answers Q1)

The single most important fact for this family is a measured null result. `crates/egraph/src/
cost_weights.rs:134-167` records a 54-configuration grid sweep over {structural, alt_per_branch,
dispatch_bonus, ref_cost} across the four-grammar corpus. The DTA state count was **identical to
baseline in every configuration**: JSON 51 {51}, BBNF 476 {476}, CSS L4 2875 {2875}, Sheets 191
{191}. The stated cause (`cost_weights.rs:154-161`): the current rewrite set
(`DeduplicateAltBranches`, `SupersetAbsorbAlt`, `UnionMergeAlt`, `FuseAltRegexBranches`,
`CommonSuffixFactor` — **all narrowing**) "produces e-classes with at most one
semantically-distinct canonical form for these grammars, so the cost model has no choice to
exercise."

Consequence for 2A, stated plainly: **with multiplicity ≈ 1, extraction-as-decision is
vacuous** — it degenerates to plain cost-ranked canonicalization (pick the single survivor). The
saturation loop and the cost model are load-bearing only where saturation manufactures ≥2
survivors of genuinely distinct *emission shape*. Today the only fork is the child-bearing `Alt`
(branch factoring), and even there the narrowing rules collapse to a canonical form; the childless
leaf kinds (`Regex(StringId)`, terminals) never fork. **A grammar-tier e-graph island earns its
keep only when it hosts a widening rewrite that manufactures a real alternative** (§1). Absent
that, the honest design is a cost-ranked canonicalizer, not extraction-as-decision — and the cost
weights are inert (Q8 corollary).

---

## 1. Widening rewrites — the only thing that makes an island non-trivial (answers Q2)

The whole family turns on one question: what rule *manufactures* an alternative so extraction has
a decision to make? There is exactly one widening/normalizing rule shipping today, and it is
instructive. `backend_egraph.rs:191` `NormalizeDirectSinkCost` searches for a `SinkOnly` /
`DirectBuildNoConsumer` candidate with nonzero `static_size_cost`/`shape_rank`, **manufactures** a
sibling `#direct-sink-normalized` node with those fields zeroed (`:216-221`), and unions it into
the class (`:234-235`). It widens the class (adds a cheaper alternative) rather than narrowing.

Its three obligations, met concretely — the template for every 2A widening rule:

- **(a) Confluence with the narrowing set.** It is *idempotent by guard*: the search skips any
  class already containing a `#direct-sink-normalized` node (`:203-206`). A widening rule that is
  idempotent and monotone-lowering commutes with the narrowing rules because it only ever *adds*
  a node the narrowing rules are then free to absorb — the union lattice is join-semilattice
  confluent regardless of application order (egg's foundational result: the e-graph is the
  confluent normal form of the rewrite *relation*, not of a term rewriting system).
- **(b) Termination.** Guaranteed structurally here (the guard forbids a second application) and,
  in general, by the scheduler: `BackoffScheduler` (scheduler.rs) plus the hard `node_limit`
  (`csp_scheduler.rs:178-180`; `growth_limit_hit`, `:279-282`) and `iter_limit`. A widening rule
  with no shrink measure is admissible only under these caps — the standard egg discipline, already
  in-tree.
- **(c) Semantics preservation.** Trivial here (both nodes select the same `BackendShape`,
  differing only in cost annotation). For a widening rule that changes *shape* (GN-2.W1's
  SelfLoop→Prefilter manufacture), preservation is by construction of the rewrite: both members
  recognize the same language; the e-graph asserts their equivalence; extraction picks the cheaper
  emission.

**Precedent for widening + narrowing coexisting in shipping engines.** egg and egglog routinely
mix expansive rules (`x ⇒ x*1`, `a ⇒ a+0`, distributivity `a*(b+c) ⇒ a*b+a*c`) with contractive
ones (constant folding, `x*1 ⇒ x`) in one saturating set; termination is *not* proved from the
rules but delegated to node/iteration caps + BackoffScheduler ban-lists — precisely the in-tree
mechanism. Cranelift's ISLE/aegraph does the same: expansive algebraic rewrites bounded by a fuel
budget. The refutation-abiding position for 2A: **widening rules are admissible, but only under
the existing node/iter caps, and each must ship idempotent-or-measure-decreasing** as
`NormalizeDirectSinkCost` demonstrates.

**The XL-F supersession, characterized with evidence.** The GN-2.W1 scan-lowering predicate (an
`ExprKind::Repetition`/Kleene over a char-class/negated-set whose complement cardinality ∈ [1,16]
⇒ a `SelfLoop`/`Prefilter` recognizer) is a widening manufacture at the recognizer tier. Its
relation to the extant `derive_recognizers` is **WRAPS, not replaces**: GN-2.md §GN-2.W1
(`passes/src/lib.rs:338` plan, lines 203-208) specifies "add a *second* detection arm … **Keep
the existing `Exact`/`PreEntry` arm untouched**." It adds a competing candidate alongside the
existing one — the first structural place multiplicity ≥ 2 becomes real in the fleet. It
*waits-on* the IR extension (`SimdSite::SelfLoop{terminators}`, `ir/src/lib.rs:430`), which LOCK-3
owns building; LOCK-2 owns selecting over it.

---

## 2. Determinism and the port budget (answers Q3)

**Extraction is already deterministic without a total order in the cost — but fragilely so, and a
total-order tie-break exists.** `extract.rs::compute_best` (`:134-171`) iterates
`egraph.classes()` — `self.classes.iter().filter(|c| !c.nodes.is_empty())` (`egraph.rs:138`), a
`Vec` in insertion order, **not** a HashMap. Within a class, `class.iter()` walks the class's node
`Vec` (dedup'd by linear scan in `rebuild`, `egraph.rs:280-288` — deterministic). The `best` map
is `FxHashMap<Id,(Cost,N)>` used **only as a keyed lookup** (`:152`, `:177`); its iteration order
is never consulted for a decision. Improvement is gated `cost < *existing` (`:156`), so ties keep
the first node in `Vec` order. Therefore extraction determinism rests on `Vec` ordering —
**HashMap iteration is inadmissible and is in fact absent from the decision path.**

The admissible **total-order tie-break** is already built at the reference island: `DecisionCost`
(`backend_egraph.rs:239-247`) is a `PartialOrd/Ord` tuple `(freshness_rank, perf_cost,
capacity_cost, static_size_cost, shape_rank, candidate_hash)` whose final field
`candidate_hash = stable_hash(&candidate.id)` is FNV-1a over the candidate id
(`backend_egraph.rs:304-311`) — content-derived, HashMap-free, byte-stable across runs and hosts.
This makes the argmin **total** (no two candidates tie), so extraction is order-independent even
if `Vec` order changed. `selection_trace` sorts its rows (`:274-291`) before hashing to
`selection_trace_hash`, giving a replay-stable audit key. This tuple is the pattern every 2A cost
model must adopt: **a lexicographic cost whose last component is a content hash**, never a
`HashMap`-iteration-order-dependent value. The provenance already asserts
`determinism_replay_status: "pass"` and `rewrite_order_variance_pct: 0` (`:139-141`).

**Port budget: build+saturate+extract+teardown.** The ~230 µs order is a **compile-time** budget
(SPEC §0 row 08: the type plane "derives validated DirectSchemaSet in ~230µs compile-time, ZERO
runtime cost"), amortized over every parse — the island runs at regen/codegen, never per-parse.
The dominant cost at this grammar scale is **teardown**, and the substrate already addresses it:
`EGraph::with_capacity` (`egraph.rs:78-88`) exists precisely because "on a ~300-node grammar [the
default power-of-two table] can leave a 1024-bucket table whose teardown dominates the compile
profile." The reference island calls `with_capacity(ranked.len())` (`:60`). `regen∘regen`
byte-identity is preserved because (i) extraction is deterministic (above), (ii) the emitted trace
hash is stable, and (iii) generated files are fresh-regen output with an idempotent-on-disk proof
(PROVE-12 §6-7; GN-2 precept 53). Teardown is a compile-latency line item, not a per-parse runtime
cost.

---

## 3. Context-sensitive extraction — resolved by rule-local islands (answers Q4)

The audit target is `admits_sink_only` at `passes/src/lib.rs:954`; the caller-context guard is
`lib.rs:661`: `(Some(rule_ir), BackendShape::SinkOnly) if !admits_sink_only(rule_ir, target)` —
fail-closed, rejecting a `SinkOnly` shape for a rule whose caller context cannot serve it (a
sub-rule reached under an offset-tape caller cannot take the sink-only shape a sink-only caller
would give it). This is exactly the conflict the question names: one sub-rule, two caller
contexts, one per-class shape cannot serve both.

**But the island decomposition already dissolves it.** The reference island is **rule-local** —
`select(rule.id, candidates)` (`lib.rs:504`), one island per `RuleId`, keyed provenance
(`selected_rule_id`, `backend_egraph.rs:132`). Because the decision is taken per rule *in its
caller context* rather than once per shared e-class, **context IS the rule** and no single
per-class shape is asked to serve two contexts. The `admits_sink_only` guard (`:661`) is the
legality filter that prunes the context-illegal candidate *before* it enters the island's
candidate set (`derive_candidates`, `lib.rs:514`). Therefore: **context-free per-class extraction
holds, because the tower does not share one e-class across contexts** — it re-runs a small
rule-local island per context and prunes illegals up front. This is the identity-preserving
repair, O(rules × candidates), already paid. The alternative — one whole-grammar e-graph with
context-tagged e-classes (family 2B territory) — would reintroduce the conflict and demand
per-context extraction passes; 2A's rule-local islands avoid it by construction. The measured
*absence* of a cross-context per-class conflict in the fleet is what makes the rule-local
decomposition sound rather than lossy.

---

## 4. IR-modeled competing sites and the cost that binds them (answers Q5, Q8)

**Are there ≥2 scan/self-loop sites modeled IN the IR with real binding costs?** Today: no — the
IR models exactly one recognizer mode. `SimdMode` (`ir/src/lib.rs:425`) = `{Exact}`; `SimdSite`
(`:430`) = `{PreEntry}`. The `BackendCandidate` carries `perf_cost/capacity_cost/
static_size_cost/shape_rank` (`backend_egraph.rs:22-25`) but these are **ordinal ranks, not
measured cyc/B** — they cannot flip a decision because they were never fit to a measurement, and
multiplicity ≈ 1 leaves nothing to flip (§0).

**The IR extension that models the competing site is GN-2.W1's:** `SimdMode → {Exact, Prefilter}`
and `SimdSite → {PreEntry, SelfLoop{terminators: StructuralAlphabet}}` (`ir/src/lib.rs:425/430`,
GN-2.md §GN-2.W1). This is the first place the fleet will present two scan candidates per site —
`Exact` (scalar membership, ~1.03 cyc/B) vs `Prefilter` (`find_member`, floor 0.115 cyc/B; SPEC §0
row 01). **LOCK-3 owns building the node** (the IR tower's shape, XL-B); **LOCK-2 owns selecting
over it** at the named decision point (the existing `backend_egraph` island, extended to range
over the SelfLoop candidate). This is the concrete `SelfLoop`/`Prefilter` model the question asks
for, and its costs become *binding* precisely when fed the GN-2-measured cyc/B rather than ordinal
ranks.

**Profile-fed cost (Q8), evidence-grounded.** The CALIBRATED_WEIGHTS null (§0) is the direct
answer to "can a fitted coefficient flip a real per-site decision": **not while multiplicity ≈
1** — there is no alternative to re-rank. A fit bites only after a *widening* rule (§1) plants a
genuine second survivor at a real site (the SelfLoop/Prefilter pair, or the N*=3 kernel-crossover
the empirical spine names, R1 §8). Attribution when whole-grammar cyc/B underdetermines per-feature
coefficients: **A/B-differential**, and the substrate is built for it — GN-2's paired per-wave
benches vary exactly one feature per measurement (W1 prefilter, W2 string kernel, W4 classify), so
each coefficient is fit from a lane that moves only that feature. Env-source fencing from regen:
cost facts are **committed const tables**, not env reads — `CALIBRATED_WEIGHTS` is "exactly one
slot to update when a future calibration produces a non-trivial result" (`cost_weights.rs:169-174`
consumer contract); every consumer constructs weights via `CostWeights::default()` aliasing that
const. A profile fit supersedes the hand-set defaults at `cost_weights.rs:112-131` — nothing else —
and the freshness gate is `selected_cost_freshness` (`backend_egraph.rs:127-131`), which goes RED
on a stale (mutated) coefficient.

---

## 5. Solver shape and the budget knapsack — 2A's boundary (answers Q6, Q7)

2A's constitutional stance is **e-graphs GENERATE candidates, CSP SELECTS**. The living code
reveals the current arrangement is **double-homed**, and this is the family's cleanup: the
`backend_egraph` island *already extracts a winner* (`:84-87`), then `decision_csp.rs::solve_rule`
(`:29`) **re-solves the same decision** as a `Csp<CostFiniteDomain>` — 1 variable, 5 constraints
(`decision_csp.rs:142-143`), `MinimizeCost` — that by construction reproduces the extraction's
pick (costs are 0 for the selected index, 1 otherwise, `:44-47`). Two solvers, one decision,
multiplicity 1: the CSP is a tautology cross-check, not a solver.

**On the graph class (near-tree: independent per-rule sites + budget coupling + few coherence
edges), the right structure is PBQP** (Scholz & Eckstein 2002, RegAllocPBQP) — per-node discrete
choice with pairwise cost matrices — *when* the coupling edges are real. But the honest reading of
the substrate is: **today there are no coupling edges** (each rule's island is independent, the
per-rule CSP has one variable), so PBQP, generic B&B, and e-graph extraction all collapse to the
same trivial argmin, and the measured `csp_solve_us` (`decision_csp.rs:101`) is noise. The 2A
recommendation, evidence-first: **collapse the redundant per-rule CSP into the extraction argmin**
(one home — the island) and reintroduce a solver *only* when a genuine cross-site constraint makes
the problem >1 variable. That constraint is the budget knapsack.

**The budget knapsack as a constraint (Q7).** The `generated_loc_budget` (GN-2 §5; precept 53:
"W2 ≤ +40 generated LOC … overflow blocks the wave") is the one real cross-site coupling — a
`bounded_sum` over per-site emission sizes. Folded as a constraint, it couples the otherwise
independent per-rule islands: choosing `Prefilter` at site A (bigger emission) may force `Exact` at
site B to stay under budget. This is where PBQP/CSP earns its place — and it is the *only* place,
on this fleet, that a solver over the islands is non-vacuous. **Selection-equivalence with the
priority cascade** it replaces (`ir/src/cost.rs:264` `PriorityStep`, the 8-step `ALL` at `:276`):
the cascade is a lexicographic priority; a `MinimizeCost` CSP reproduces it iff the cost vector
encodes the priority order as its lexicographic key — which `DecisionCost` already does (§2, the
tuple ordering). So the knapsack folds in *without* breaking cascade equivalence, provided the
budget term is added as the *lowest*-priority (last) lexicographic component so it tie-breaks only
among equal-priority candidates. **One solver home:** the `csp-solver` crate (`crates/csp-solver/`),
whose constraint store LOCK-1 governs and LOCK-2 consumes — the same store already used both at the
grammar tier (`csp_domains.rs`, per `csp_scheduler.rs:22`) and *inside* the e-graph scheduler
itself (`CspScheduler`, `csp_scheduler.rs` — the saturation scheduler **is** a CSP propagation over
`DirtyDomain`, AC-3 transitive dirty closure through parent edges, `:141-169`). This isomorphism
(one constraint store, three consumers: grammar-tier domains, egraph scheduler, decision select) is
the coordination point GESTALT-B must unify.

---

## 6. Prior art (citations)

- **egg** (Willsey et al., POPL 2021, *"egg: Fast and Extensible Equality Saturation"*) — the
  greedy bottom-up extractor `extract.rs` implements (it cites egg by name, `:8-11`); the
  rebuild/hashcons/union-find core (`egraph.rs`); BackoffScheduler ban-lists for termination under
  widening. The multiplicity-1 finding is the negative image of egg's premise: egg pays off when
  the rewrite set is *expansive*.
- **egglog** (Zhang et al., PLDI 2023) — datalog+egraph; bears on whether the narrowing-only rule
  set should gain expansive rules to make extraction non-trivial (§0/§1).
- **Scholz & Eckstein, PBQP** (2002, *"Register allocation for irregular architectures"*) and
  LLVM's `RegAllocPBQP` — the per-node-choice + pairwise-cost model for budget-coupled site
  selection (§5); the correct target *only once the knapsack edge exists*.
- **Cranelift aegraph / ISLE** (Fallin, 2023) — acyclic-egraph one-pass extraction under a fuel
  budget: precedent for widening rules bounded by caps rather than a termination proof (§1), and
  for extraction-at-emit without a separate select pass.
- **Eisel–Lemire / fast_float** (Lemire 2021; Nigel Tao) — the profile-fed cost lane's measured
  feature (GN-2.W5), the A/B-differential fit target (§4).
- **simdjson two-stage classify** — the LUT-upgrade path the classify kernel documents
  (`aarch64/structural_classify_64.rs:1-31`, R1 §3), a future genuine-choice site.

---

## 7. Named risks and counterexamples

- **The island is a solution in search of a decision.** Risk: 2A ships e-graph islands whose
  multiplicity stays 1 (CALIBRATED_WEIGHTS null, §0), so the saturation/extraction machinery is
  pure overhead over a trivial argmin. Refuting counterexample: if *no* fleet grammar yields a
  qualifying SelfLoop (complement ∉ [1,16]) — GN-2.md §7 W1 risk names exactly this ("if NO grammar
  in the fleet has a qualifying self-loop, W1's consumer is a purpose-authored minimal skip
  grammar"). Mitigation: 2A islands are admissible **only** where a widening rule demonstrably
  manufactures ≥2 emission-distinct survivors; elsewhere the design down-declares to cost-ranked
  canonicalization (no extractor, no cost model). Consumer-less substrate is forbidden (Y.13) — an
  island whose choice is always singleton is a consumer-less e-graph.
- **Double-home mistaken for defense-in-depth.** Risk: keeping both the extraction pick and the
  per-rule CSP (§5) as a "cross-check" when they are the same argmin — two subsystems, one decision,
  violating system-cohesion. Counterexample: `decision_csp.rs:44-47` sets cost 0/1 to *force*
  agreement with the extractor, so the CSP can never disagree — it is not a check.
- **Widening breaks regen determinism.** Risk: a widening rule whose application order affects the
  saturated set, making `regen∘regen` non-identical. Mitigation: idempotent-guarded rules (§1) +
  total-order cost (§2); the reference rule (`:203-206`) shows the pattern.
- **Budget knapsack breaks cascade equivalence.** Risk: folding `bounded_sum` as a high-priority
  constraint reorders decisions the priority cascade fixed. Mitigation (§5): the budget term must
  be the *last* lexicographic component, tie-breaking only among equal-priority candidates.

---

## 8. What a prototype must test to move this family

**One prototype, minimal, decisive** — it converts the degenerate island into a genuine-choice
island and measures every 2A claim at once:

1. Extend `derive_recognizers` (`passes/src/lib.rs:338`) with the GN-2.W1 SelfLoop arm so a real
   grammar scan site (JSON whitespace-skip: complement = 6 structurals + quote ≤ 16) yields **two
   competing candidates** — `Exact` and `Prefilter` — entering the `backend_egraph` island's
   candidate set as ≥2 emission-distinct survivors (multiplicity 2, the first in the fleet).
2. Feed them **measured cyc/B** (Prefilter 0.115 floor vs Exact ~1.03; SPEC §0 row 01) as the
   `perf_cost`, replacing ordinal ranks — so the cost is *binding*.
3. Verify, against the four falsifiable questions the family rests on:
   - **(Q1/Q8)** extraction picks the measured-cheaper candidate; and a perturbed coefficient (a
     profile fit that raises Prefilter's fixed cost above the crossover) **flips** the pick — the
     first demonstration that a fitted coefficient flips a real per-site decision.
   - **(Q3)** `regen∘regen` is byte-identical and `selection_trace_hash` is stable across two runs
     and a reordered candidate `Vec` (proving the `candidate_hash` tie-break, not `Vec` order,
     carries determinism).
   - **(Q3, teardown)** build+saturate+extract+teardown for the extended island stays within the
     ~230 µs compile-order budget at JSON-grammar scale (`with_capacity` honored).
   - **(Q5/Q7)** add a second SelfLoop site and a `generated_loc_budget` `bounded_sum` constraint;
     confirm the coupling makes the per-site choices *interdependent* (choosing Prefilter at both
     exceeds budget, forcing one back to Exact) — the first non-vacuous solver instance, and the
     test that separates PBQP/CSP from trivial argmin by solve-cost.

Pass ⇒ 2A has one real island with a widening rule, a binding cost, deterministic extraction, and
a budget-coupled solver instance — the family's strongest concrete form. Fail (no coefficient flip,
or budget coupling never binds) ⇒ 2A down-declares to cost-ranked canonicalization and the e-graph
machinery is retired from the grammar tier as consumer-less substrate.
