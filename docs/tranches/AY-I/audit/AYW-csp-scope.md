# AYW — CSP solver scope + full-generality audit

Read-only audit of `crates/csp-solver/` and every workspace caller at
HEAD `a91633e3`. Charter: assess scope, evaluate generality,
enumerate under-utilization candidates, and recommend cost-model
unification moves.

Memory context per user auto-memory: `csp-always-optimize`
("foundational library, not gated by profile share"),
`csp-solver-crate` ("generalized CSP in own crate; Rust+Python
co-located"), `unified-propagate` ("one `propagate()` method, not
suffixed variants"), `pluggable-components` ("decision points must
be pluggable, not hardcoded branches"), `general-infra-crates`
("general-purpose constructs in own crate"),
`regex-crate-isomorphic` ("bbnf-regex must use the same
optimization architecture internally").

---

## § 1. Current CSP scope

### Public API surface

`crates/csp-solver/src/lib.rs` exposes `Csp<D: Domain>` with the
following capabilities:

- `add_variable(domain) -> VarId`, `add_variables(domain, count)`
- `add_constraint(impl Constraint<D>)`,
  `add_constraint_enum(ConstraintEnum<D>)` (devirtualised builtins),
  `add_soft_constraint(SoftLambdaConstraint<D>)`,
  `add_not_equal(x, y)`, `add_all_different(vars)`,
  `add_equals(var, val)`, `add_less_than(x, y)`,
  `add_greater_than(x, y)`
- `finalize()` — builds the `Adjacency` graph + per-variable
  constraint index used by the wdeg ordering
- `propagate()` / `propagate_with(PropagationStrategy::{Auto, Ac3,
  Sweep})` — single entry point per `unified-propagate` norm
- `solve(&SolveConfig)` — chooses
  Feasibility/MinimizeCost/MaximizeCost, Chronological or Backjump
  search
- `solve_with_given(config, given)` — assignment seed + one-hop
  domain reduction + AC-3 before search
- `solve_with_cost_eval(config, &dyn DomainCostEval<D>)` — arbitrary
  extrinsic cost surface (non-`CostDomain` variants)
- `solve_optimized(config)` where `D: CostDomain` — ergonomic
  wrapper for branch-and-bound

Configuration knobs: `Pruning::{None, ForwardChecking, Ac3, AcFc}`,
`Ordering::{Chronological, FailFirst, DomWdeg}`,
`OptimizationMode::{Feasibility, MinimizeCost, MaximizeCost}`,
`node_budget: Option<u64>` (default `1_000_000`).

Solver modules in `src/solver/`: `ac3.rs` (full + per-variable
variants), `backjump.rs` (conflict-directed), `backtrack.rs`
(chronological), `gac_alldiff.rs` (Régin 1994 Hopcroft-Karp +
iterative Tarjan), `local_search.rs` (min-conflicts
hill-climbing + LCG RNG), `monotonic.rs` (fixed-point sweep for
lattice domains), `nogoods.rs` (bounded LRU conflict store),
`optimize.rs` (branch-and-bound with incremental bound
maintenance).

Constraint families in `src/constraint/`: `AllDifferent`,
`CardinalityConstraint`, `LambdaConstraint`, `NotEqual`,
`ImplicationConstraint`, `SoftLambdaConstraint`, plus the generic
`Custom(Box<dyn Constraint<D>>)` escape hatch via `ConstraintEnum`.

Domain families in `src/domain/`: `BitsetDomain` (`u128`-backed,
zero-alloc iter), `FiniteDomain<T>`, `BitsetLatticeDomain`. The
`Domain` / `LatticeDomain` / `CostDomain` traits let consumers
plug bespoke lattices.

Total SLOC: 4,120 (`src/`), plus 8 test files in `tests/`
exercising sudoku, futoshiki, lattice, GAC, optimize, nogoods,
local-search, and standalone solver.

### Current callers (production + egraph substrate)

| Caller | File:line | Domain | Variables | Constraints | Mode | Scope | Timing |
|---|---|---:|---|---|---|---|---|
| Type projection | `crates/ir/src/passes/types/mod.rs:60,63` (+ `generate.rs`) | `TypeDomain` (singleton lattice over `TypeDesc`) | one per IR node (× normal + vec-context), one per rule | `Ground`, `Equal`, `Seq`, `Alt`, `AltInVec`, `Ref`, `Optional`, `Repeat`, `Project`, `Map` | Monotonic sweep (propagate) | Global (cross-rule via `RefConstraint`) | Compile-time |
| FIRST-sets | `crates/ir/src/passes/sets/first_sets.rs:53` | `CharSetDomain` (128-bit bitmap lattice) | one per rule + per subexpression | `Ground`, `Union`, `MultiUnion`, `SeqFirst` | Propagate (sweep) | Global | Compile-time |
| FOLLOW-sets | `crates/ir/src/passes/sets/follow.rs:41` | `CharSetDomain` | one per rule | `Ground`, `Union` | Propagate (sweep) | Global | Compile-time |
| Span eligibility | `crates/ir/src/passes/span.rs:34` | `BoolDomain` (top-down refinement lattice) | one per rule + per sub-node | `BoolGround`, `BoolEqual`, `BoolAnd` | Propagate (sweep) | Global | Compile-time |
| Dispatch eligibility | `crates/ir/src/passes/sets/dispatch/eligibility.rs:53` | `DispatchDomain` (`{Unknown, Dispatchable, NonDispatchable}`) | one per Alt node | `DisjointConstraint` | Propagate (AC-3) | Local (per Alt) | Compile-time |
| Inline plan | `crates/core/src/backend/rust/analysis/inline.rs:34` | `InlineDomain` (`{InlineBody, DirectCall}` lattice) | one per rule | `ForceDirectCall`, `ForceInlineBody`, `CostBudget`, `AltBranchLimit`, `ShapeGuard` | Propagate (AC-3) | Global (all rules) | Compile-time |
| Strategy synthesis | `crates/ir/src/passes/csp_strategy/mod.rs:460` + `constraints/engine.rs`, `shape_dict.rs` | `StrategyDomain` (cost-bearing finite) | Alt/Wrap/Engine/Materialization sites in every rule of a component | `ImplicationConstraint` (TokenDispatch → one-pass), `EnginePropagation` (pairwise equality) | `OptimizationMode::MinimizeCost` + `Pruning::ForwardChecking` | Per connected-component (call-graph) — approaches global via the component partition | Compile-time |
| Egraph scheduler | `crates/egraph/src/csp_scheduler.rs:192`, consumed by `crates/ir/src/egraph/mod.rs:103` and `parse-that/rust/regex/src/egraph/mod.rs:92` | `DirtyDomain` (boolean lattice) | one per e-class, rebuilt each saturation iteration | `ParentDirtyProp` (AC-3 upward closure) | Propagate (AC-3) | Global (e-graph-wide) | Compile-time (per saturation iter) |

Nine distinct production surfaces across three workspace crates +
one external path dependency, plus the internal
`puzzles/` demonstrations. Every surface uses
`csp_solver::Csp` as the single substrate; nothing escapes to a
parallel solver. The `unified-propagate` invariant holds.

Cross-rule constraint scaffolding lives in
`crates/ir/src/passes/csp_strategy/constraints/` with an
`install(ctx, csp, ir)` seam per constraint family (`engine`,
`shape_dict`) — pluggable per `pluggable-components`.

---

## § 2. Generality check

Feature coverage audit against the canonical CSP + COP toolchain:

| Feature | Status | Location |
|---|---|---|
| Chronological backtracking | ✔ | `solver/backtrack.rs` |
| Conflict-directed backjumping | ✔ | `solver/backjump.rs` |
| Forward checking | ✔ | `solver/propagate.rs::forward_check` |
| AC-3 (full + per-variable) | ✔ | `solver/ac3.rs` |
| AC-FC hybrid | ✔ | `solver/propagate.rs::ac_fc` |
| MRV (fail-first / min-dom) | ✔ | `ordering.rs::Ordering::FailFirst` |
| LCV (least-constraining-value) | ✖ | not implemented; value iter is insertion-order (backtrack) or cost-sorted (optimize) |
| dom/wdeg variable ordering | ✔ | `ordering.rs::Ordering::DomWdeg` |
| Path consistency (PC-2, etc.) | ✖ | AC-3 only |
| Global alldiff GAC (Régin) | ✔ | `solver/gac_alldiff.rs` (Hopcroft-Karp + Tarjan) |
| Cardinality / at-most-k | ✔ | `constraint/cardinality.rs` |
| Branch-and-bound COP | ✔ | `solver/optimize.rs` (incremental domain bound + soft-penalty pre-indexing) |
| IDA* / WBO / iterative MILP | ✖ | not implemented |
| Soft constraints (weighted penalty) | ✔ | `constraint/soft.rs` + `ConstraintEnum::Soft` |
| Symmetry breaking | ✖ | no `Lex`/`SBDS`/`SBDD`; user must hand-add ordering constraints |
| Conflict clause / nogood learning | ✔ (as store) | `solver/nogoods.rs` — bounded LRU `NogoodStore`; **not wired into backtrack/backjump solvers** — sits on the shelf |
| Restart strategies (Luby, etc.) | ✖ | single monolithic search |
| Monotonic lattice sweep | ✔ | `solver/monotonic.rs` |
| Local search (min-conflicts) | ✔ | `solver/local_search.rs` |
| Parallel search / portfolio | ✖ | single-threaded |
| Incremental re-solve after mutation | ✖ | solver carries no incremental invariants beyond `finalize()` rebuild |
| Node-budget termination + best-so-far | ✔ | `SolveConfig::node_budget` + `SolveStats::budget_exceeded` |
| Custom `DomainCostEval` trait | ✔ | `solver/optimize.rs::DomainCostEval` — extrinsic cost surface |

**Derivable-from-existing-architecture assessment:**

- **LCV** — trivial: `optimize.rs` already cost-sorts; a
  feasibility-mode value ordering hook that delegates to an
  `orderings::ValueOrdering` enum fills the gap. Existing
  adjacency + constraint scope infrastructure supplies the
  "how many neighbours does `val` eliminate" count.
- **Path consistency / PC-2** — derivable from the existing AC-3
  worklist substrate; the `Constraint::revise` trait carries
  enough information that a ternary PC-2 loop composes over
  existing binary revision. Low demand on current callers — every
  production caller is functionally a binary CSP or a lattice
  sweep, so PC-2 would be speculative machinery.
- **Symmetry breaking** — a pluggable `SymmetryBreaker` trait
  consulted after `finalize()` to auto-install lex-leader
  constraints fits the existing constraint-installer seam
  (`constraints/*/install` in csp_strategy).
- **Nogood learning (wire to search)** — `NogoodStore` already
  exists and is tested (`tests/nogoods.rs`); wiring into
  `backtrack_recurse` / `bb_recurse` is a local change
  restricted to the guard at value-selection time. Would
  accelerate the AF.3 component solver when the `EnginePropagation`
  constraint drives repeat failures.
- **Restart strategies** — derivable but low value today; every
  production solve converges or hits `node_budget`.
- **Parallel / portfolio** — orthogonal to the existing
  architecture; would require reworking `Variable` state
  management.

The solver is "good enough for every current caller" with three
real gaps: nogood wiring, LCV, and symmetry breaking. All are
pluggable additions, no architectural transposition required.

---

## § 3. Under-utilization — problems that look like CSP but aren't

Seven candidates, each a problem currently hand-coded, greedy, or
outside the CSP substrate.

### 3.1 Aggregate payload layout planning

**Current mechanism.** `crates/ir/src/passes/payload/layout.rs`
lines 465-500: `plan_layout_with_cap(fields, cap)` is a
single-pass greedy bump-allocator. For each scalar field in
declaration order, align to the field's natural alignment, check
against the cap, bump offset. Fields are never reordered; no
exploration of alternative packings. Returns `None` on any failure
(non-scalar, cap exceeded). 500 LOC total.

**Hidden optimization surface.** The declaration-order constraint
is semantic only for the emitter's field-label mapping; the
actual packing could reorder for padding minimization. A
`(u8, f64, u8, f64)` in declaration order wastes 7 bytes padding
per alignment gap (total 24 B); reorder to `(f64, f64, u8, u8)`
and it packs in 18 B. With the 16 B cap in the hot path, that
reorder is the difference between admission and rejection —
literally changes codegen shape.

**CSP framing.** Variables = field slot assignments; domain =
`0..MAX_OFFSET` (discrete slot positions at alignment granularity);
constraints = (a) pairwise no-overlap (`AllDifferent` over slot
ranges), (b) alignment (each field's offset divisible by its
alignment), (c) total cap. Objective = minimize total bytes. The
codegen already tracks a rule-to-field-name map; decoupling it
from declaration order lets the planner exploit reordering.

**Expected impact.** Enables admission of at least CSS L4
`Color` at the 16 B hot-path cap (currently needs 40 B +
`LargeAggregate`), which in turn lets the bench binary skip the
`PayloadData::LargeAggregate` branch on every colour record. A
pluggable `FieldOrderingPolicy` parameterises the planner so the
old declaration-order behaviour is preserved for rules that need
source-shape fidelity.

**Scope.** ~200 LOC — domain + constraints + integration shim
replacing `plan_layout_with_cap`. Fits in one wave.

### 3.2 E-graph extraction as global COP

**Current mechanism.** `crates/egraph/src/extract.rs` is a
greedy bottom-up fixed-point: each class picks its cheapest
e-node based on the current best-cost of its children. Converges
in a few iterations for monotone cost models but is **locally**
optimal only — doesn't account for shared sub-term cost.

**Hidden optimization surface.** A class that appears as a
descendant of two separate parents and has two e-node
alternatives A (cheap alone, forces expensive parent rewrite) and
B (expensive alone, unlocks cheap parent rewrites) cannot be
chosen optimally by greedy bottom-up. Proper extraction is an
integer program; the greedy algorithm is a heuristic that works
because current cost models are nearly separable and the rewrite
set produces at most one canonical form per class (the null
sweep result in `CALIBRATED_WEIGHTS` confirms this).

**CSP framing.** Variables = one per e-class (`VarId =
class_id`); domain = the e-nodes in that class; constraints =
(a) child-class membership (if this class picks node N, each
child class must pick a node whose canonical id matches N's child
slot), (b) ground root class must pick a node. Objective =
minimize total cost. Structure is a **DAG of choice variables**
with constraints between parent and child, i.e. exactly the CSP
shape the `csp_strategy` solver already solves for strategy
decisions. The `DomainCostEval` trait in `solver/optimize.rs`
lets the existing cost models plug in without modification.

**Expected impact.** Correctness improvement when the rewrite
set grows (the AW-IV.W5.3 null-calibration result is evidence
the current rewrite set is sparse — adding rules is the AY/BA
surface that unlocks this). Also gives the extractor a natural
wire for sharing-aware cost models (`CostModel::cost` could
receive a parent-multiplicity hint) without changing the
interface.

**Scope.** ~300 LOC — new `egraph::extract_csp` module that
parallels the existing `Extractor` with a `Csp<ClassDomain>`
backend. The existing greedy extractor stays as the fast path
for monotone scalar cost; the CSP path becomes the definitive
answer when the rewrite set admits multiple canonical forms per
class.

### 3.3 Shape-dict admission selection

**Current mechanism.**
`crates/ir/src/passes/csp_strategy/constraints/shape_dict.rs:81`
`solve_shape_dict_selection` is explicitly documented as greedy
top-N: score every template by `freq × savings - static_entry_cost`,
filter positive, dedup by `shape_hash`, truncate to
`MAX_SHAPE_DICT_ENTRIES = 32`. The file's own module doc admits
(line 18-24) "per-template scores are independent (no cross-
template interactions on the cost surface), so the budget-
cardinality optimization degenerates to greedy top-N."

**Why CSP framing matters anyway.** The independence assumption
is wrong the moment two templates share overlapping leaf
schemas — a shape-hash collision is already a cross-template
interaction, but the collision resolution is a secondary dedup
step rather than part of the selection. When AY-adjacent work
grows the template pool (per-site templates, cross-grammar
dedupe), the pairwise interactions start to matter:
"admitting templates A and B yields worse emitter output than
admitting A alone because the 5-bit `shape_dict_idx` byte is
exhausted." Today's cardinality constraint (`Σ include ≤ 32`) is
exactly a `CardinalityConstraint` — which csp-solver already has.

**CSP framing.** Variables = one boolean per template;
constraints = cardinality ≤ 32, pairwise shape-hash exclusion;
soft constraints = per-template `freq × savings` penalty on
exclusion. This is a knapsack / COP problem that becomes
non-trivial as soon as the interaction surface grows.

**Expected impact.** Low today (greedy is optimal under the
current pool), but the module doc's own wave-planning note about
"future cross-rule shape-dict interactions" makes the CSP
substrate the natural landing place when the cross-template
interactions actually exist. Moving now means every future
constraint sinks into `install(ctx, csp, ir)` rather than growing
a bespoke selector.

**Scope.** ~150 LOC — inflate the existing `shape_dict::install`
stub from its zero-constraint no-op into a real per-component
CardinalityConstraint + soft-penalty wire. The grammar-wide
selection already runs as a separate pass; folding it into the
component CSP is a modest reorganisation.

### 3.4 Byte-class dispatcher partitioning

**Current mechanism.**
`crates/core/src/generate/regex/byte_class.rs:70` — mines
per-pattern first-byte bitmaps, builds a 256-entry byte →
`Vec<pattern_idx>` mapping, and emits a `match input[pos]` ladder.
Declarative, non-heuristic, but purely local: each byte sees the
complete set of admissible patterns, no global optimization of
the arm shape. The 32-pattern cap (line 42) is a hard bound, not
a CSP-minimised one.

**Hidden optimization surface.** When multiple bytes route to the
same pattern set, the emitted code can merge them into a single
`match` arm (e.g., `b'0'..=b'9' => [pat_0, pat_3]`). This
happens today implicitly via the `byte_to_patterns` structure,
but the arm-count minimisation that would collapse e.g.
`b'a' | b'b' | b'c' => [0, 1]` into a range is not a constraint
the emitter optimises over.

**CSP framing.** Variables = one per byte; domain = pattern-set
equivalence classes; constraints = (a) each byte's assignment
must cover every pattern that accepts that byte, (b) minimise
the number of distinct equivalence classes (COP objective).
Falls out as a partition-minimisation CSP with the equivalence
classes as symmetry group — a classic `Lex`-breaking CSP
problem.

**Expected impact.** Smaller emitted DFA ladders → faster
compile + faster I$ hits at runtime. Modest but measurable on
wide-alphabet patterns (CSS L4, identifiers). Also gives a
natural home for future "merge rare bytes into a fallback arm"
optimisation — today hand-coded, tomorrow a soft constraint.

**Scope.** ~180 LOC — new module `byte_class_csp` paralleling
the existing `byte_class.rs`; swap the caller to the CSP version
once the output is verified identical on the test corpus.

### 3.5 Column / slot allocation for tape payloads

**Current mechanism.** `crates/tape/src/columns.rs` assigns
payload slots to `pay_narrow` (u32) vs `pay_wide` (u64) vs
typed-payload columns via hand-picked type-driven matches.
Explicit per-type constants, no cost model. The type-to-slot
mapping is stable but not searched — e.g., a rule-payload that
could fit either u32 or u64 always goes to u32 because the
`TypeDesc::payload_size_bytes` function says so, even when
neighbouring payloads share the u64 column and u32 is scarce.

**Hidden optimization surface.** The 16 B tape record has only
so many slot positions; grammars with many aggregate payloads
compete for them. Under high slot pressure, a CSP could trade
bytes for alignment: rule A takes the 4-byte slot so rule B can
fit its 8-byte slot, minimising total record count.

**CSP framing.** Variables = per-rule payload allocation;
domain = available column + offset pairs; constraints = alignment,
size, at-most-one-per-slot; objective = minimize total tape-row
growth. Shares constraint shape with §3.1 (payload layout
planning) — the two merge into a single "payload allocator CSP"
crossing record and field granularity.

**Expected impact.** Marginal on small grammars, tangible on
CSS L4's `Color` + related aggregates. Merges the code path
with §3.1 so the two layout decisions are one pass.

**Scope.** ~250 LOC if merged with §3.1; stands alone at ~150.

### 3.6 SCC-aware emit ordering (rule schedule)

**Current mechanism.** `crates/ir/src/passes/sets/scc.rs`:
Tarjan's algorithm returns SCCs in reverse-topological order.
This is the rule schedule for emit and memoisation — fixed by
the graph structure, not optimised.

**Reason it's CSP-shaped but probably not worth converting.**
The topological order is a constraint (edges), but the
objective is null — any valid topological order is correct. No
cost function to minimise, no pluggable ordering policy. A CSP
framing (variables = rule emit position, constraints = deps,
objective = minimise cross-rule coercion cost via
`CostWeights::cross_module_coercion`) would add a ranking
signal but none of the existing callers consume a rank.

**Recommendation.** **Leave alone**. The AF.3 component
partition (`crates/ir/src/passes/csp_strategy/components.rs`)
already uses a union-find over the call graph, which is the
appropriate data structure. Upgrading to CSP would add no
optimisation surface.

Included for completeness — not a recommended conversion.

### 3.7 Grammar shape classification

**Current mechanism.** `crates/ir/src/passes/recognizers/*.rs`
— ~20 miners, each testing a structural predicate and
classifying a rule into exactly one `RecognizerShape`. The
classification is order-sensitive: first miner to claim a rule
wins. Explicit per-miner predicates, no conflict resolution, no
priority ranking visible as a cost.

**Hidden optimization surface.** When two miners both match
(e.g., a rule is both "balanced wrap" shape and "arglist" shape),
the emitter picks based on the first-win ordering. A CSP framing
would make the priority explicit, backed by a cost model where
the cheaper-emitting shape wins.

**CSP framing.** Variables = one per rule; domain = eligible
shapes (set of classifier outputs); constraints = feasibility
from the miner predicates; objective = minimize per-shape
emission cost (tie-broken by precedence ladder when costs
equal). This **mirrors** the existing `StrategyDomain` in
`csp_strategy` — same shape of problem (per-rule categorical
decision with cost), different domain family.

**Expected impact.** The shape-dispatch tests
(`crates/ir/tests/shape_dispatch.rs` — 3,390 LOC cited in
`project_arch_consolidation`) already exercise the
classification across all shipped grammars; moving to CSP would
preserve current semantics while adding a pluggable cost model.
Synergy with §3.3 (shape-dict selection) — both decide "which
shape wins for this rule" and should share a constraint
surface.

**Scope.** ~400 LOC — a meaty refactor. Worth landing after the
simpler candidates (§3.1, §3.3) prove the cost-model plumbing.
Fits as a late-BA or post-BA landing rather than an AY wave.

---

## § 4. Cost model unification — CSP × e-graph × regex

### Current state

Three cost-model surfaces exist, and they are already partly
unified:

1. **`egraph::CostWeights`** (`crates/egraph/src/cost_weights.rs`,
   193 LOC). The authoritative cross-tier substrate. Carries nine
   dimensions: `structural`, `alt_per_branch`, `dispatch_bonus`,
   `call_overhead`, `inline_body_size_penalty`, `tape_push`,
   `dispatch_branch`, `dispatch_table`, `prettify_emission`,
   `cross_module_coercion`. Plus the grid-sweep-calibrated
   `CALIBRATED_WEIGHTS` const for production read paths.

2. **`bbnf_ir::egraph::GrammarCostModel`**
   (`crates/ir/src/egraph/cost.rs`). Embeds `CostWeights`, adds
   grammar-tier knobs (`literal_cost`, `regex_cost`, `ref_cost`,
   `seq_per_child`). Documents the invariant "Shared weights stay
   in sync across tiers — splitting them would let branch-factoring
   and dispatch incentives drift between regex and grammar
   extraction."

3. **`bbnf_regex::egraph::RegexExtractionCost`**
   (`parse-that/rust/regex/src/egraph/cost.rs`). Embeds
   `CostWeights`, adds regex HIR-specific knobs
   (`literal_per_byte`, `class_cost`, `repeat_cost`,
   `merged_bonus`). Per memory `regex-crate-isomorphic`:
   "bbnf-regex crate must also use the same optimization
   architecture internally" — this crate does.

The `CostWeights` substrate is the already-declared authoritative
surface; grammar tier and regex tier both embed it. Per memory
`feedback_pluggable_components` + the `egraph::CostModel` trait
with generic `Lattice` cost type, the surface is pluggable by
construction.

### Integration with CSP

The CSP layer is where the unification is weaker. `csp-solver`
carries no built-in cost surface — every CSP consumer that reads
`CostWeights` constructs its own `CostDomain` or extrinsic
`DomainCostEval`. Concretely:

- `csp_strategy::StrategyDomain` (`mod.rs:271-333`) implements
  `CostDomain` with a `Vec<f64>` cost vector populated from
  `CostConfig.strategy_*` fields (which themselves derive from
  `CostWeights`).
- `crates/core/src/backend/rust/analysis/inline.rs::CostBudgets`
  derives every threshold from `CostWeights` via the comment
  block at lines 80-106 ("CSP inline-vs-call thresholds" +
  "Shape-guard thresholds").
- `egraph::csp_scheduler` does not use costs at all — `DirtyDomain`
  is a boolean lattice.

So the cost surface exists (`CostWeights` is the single source of
truth), is cross-tier (both e-graph consumers embed it), and is
pluggable (via `CostDomain` + `DomainCostEval` + `CostModel::Cost:
Lattice`). **The split three-model concern is not about splits at
all — it's about consumer adoption.** Two things are not yet on
the unified surface:

- **The byte-class dispatcher** (`byte_class.rs`) has no cost
  model, just a 32-pattern cap.
- **The payload layout planner** (`payload/layout.rs`) has no
  cost model — it bump-allocates without exploring reorderings.

**Verdict: unified substrate exists; two consumers are not
wired. Cost unification = one-week task** (wire `byte_class.rs`
and `payload/layout.rs` to read `CostWeights`; zero new
infrastructure). The three-model concern named in the audit
prompt's spec was anticipatory — it does not describe today's
codebase, which has the unification landed at the `CostWeights`
level.

---

## § 5. Recommendations

Ordered by impact + simplicity. Each recommendation specifies
problem currently-hand-coded, CSP framing, expected impact, and
wave scope.

### R1 — Wire `NogoodStore` into backtrack + backjump search

**Problem.** `crates/csp-solver/src/solver/nogoods.rs` (167 LOC,
full test coverage) implements a bounded LRU nogood store, but
no solver consumes it. The AF.3 component solver's
`EnginePropagation` constraint produces repeat failures that
nogood learning would short-circuit.

**Move.** In `backtrack.rs::backtrack_recurse` and
`backjump.rs::backjump_recurse`, guard value selection against
`NogoodStore::is_nogood`, and on infeasible-completion record
the conflict in the store. Add `SolveConfig::nogood_store:
Option<NogoodStoreConfig>`.

**Impact.** Direct acceleration of the `csp_strategy` component
solver — the one production COP path. Lifts an on-shelf
infrastructure to consumer.

**Scope.** ~80 LOC (integration only, no new infrastructure). BA
or next CSP-adjacent tranche.

### R2 — Payload layout planner as CSP (candidate §3.1)

**Problem.** Greedy bump-allocator forbids reorderings that
admit more aggregates under the 16 B cap.

**CSP framing.** Per §3.1. Variables = per-field slot offsets;
constraints = alignment + no-overlap + cap; objective =
minimise total bytes (or, more aggressively, maximise admission
rate across rules).

**Impact.** Admits CSS L4 colour aggregates into the 16 B
hot-path cap, eliminating the `PayloadData::LargeAggregate`
branch. Samply attribution is the verification gate. Secondary
effect: provides a natural home for §3.5 (column allocation)
when the two pieces merge.

**Scope.** ~200 LOC. BA or a follow-on tranche.

### R3 — Wire cost surface into `byte_class.rs` + extend coverage

**Problem.** `crates/core/src/generate/regex/byte_class.rs`
carries no cost model — 32-pattern cap is arbitrary, arm-count
minimisation is implicit.

**Move.** Add a `CostWeights::byte_class_arm` dimension (or
reuse `dispatch_branch`); read it from `CostConfig` at emit
time; gate the `MAX_DISPATCHED = 32` constant on a
cost-threshold comparison rather than a hardcoded number. Per
`pluggable-components` memory: decision points must be
pluggable.

**Impact.** Opens the door to §3.4 (byte-class partition CSP)
as a follow-on; standalone benefit is an adjustable dispatch
cap that benchmarking can tune per-grammar.

**Scope.** ~50 LOC substrate wire + ~180 LOC if the partition
CSP follows. BA.

### R4 — Extend `OptimizationMode` into the egraph scheduler

**Problem.** The `CspScheduler` uses the CSP substrate only for
the dirty-class transitive closure (lattice propagation). It
could additionally provide a COP-backed extraction alternative
(§3.2) for the rare grammar where the e-class topology produces
multiple canonical forms.

**Move.** Implement `egraph::extract_csp` as a
`Csp<ClassChoiceDomain>` backend, sharing the `CostWeights`
substrate. Keep the greedy extractor as the default fast path;
the CSP extractor becomes the gate for multi-canonical-form
e-classes.

**Impact.** Unlocks the null sweep calibration result — once the
rewrite set grows to admit multiple canonical forms per class,
extraction becomes a real COP rather than a degenerate greedy
pick.

**Scope.** ~300 LOC. Post-BA — waits on a richer rewrite set.

### R5 — Install a `ValueOrdering` hook (LCV + per-caller value policies)

**Problem.** Feasibility-mode solvers iterate domain values in
insertion order; optimisation-mode sorts by cost. Neither
supports LCV (least-constraining-value), which would accelerate
the inline plan CSP (`inline.rs` — AC-3 with many unary
constraints would benefit from cost-of-refutation heuristics).

**Move.** Add `Ordering::LeastConstraining` alongside the
existing `Chronological / FailFirst / DomWdeg`, and a parallel
`ValueOrdering` enum for per-variable value iteration. Use the
existing adjacency + `DomainCostEval` surfaces as inputs.

**Impact.** Accelerates every production solve with ≥ 2
non-singleton variables. Aligns with `pluggable-components`
memory.

**Scope.** ~120 LOC. BA.

---

## Summary

- **9 production CSP callers** across three crates + external
  `parse-that/regex`. Every production caller uses the single
  `csp_solver::Csp` substrate; `unified-propagate` invariant
  holds.
- **Solver generality is substantive** — AC-3 + backjump +
  branch-and-bound + GAC alldiff + local search + nogood store +
  monotonic lattice sweep. Three gaps (LCV, path consistency,
  symmetry breaking) are derivable from existing architecture.
  **Nogood store exists but is not wired** — architecture debt
  to close.
- **Top under-utilisation candidates** in priority order:
  payload layout (§3.1), e-graph extraction (§3.2), shape-dict
  selection (§3.3), byte-class partitioning (§3.4).
- **Cost model unification**: `CostWeights` is already the
  single source of truth across grammar tier + regex tier +
  CSP-consumer configs. Remaining split = two non-wired
  consumers (byte class, payload layout).
- **Top 3 recommendations**: (R1) wire `NogoodStore`, (R2) CSP
  the payload layout planner, (R3) wire `CostWeights` into
  `byte_class.rs`.
