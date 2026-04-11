# Tranche AF — Three-Tier Emission, Universal Cost Model, Cross-Rule CSP

## Status: PROTOTYPE — execution gated on AE

This is a design document, not a current execution plan. AF is a
forward-looking sketch of the optimization work that becomes
tractable once AE's tape-first substrate completion lands. Nothing
in AF runs, nothing in AF compiles, nothing in AF lands on master
until AE is closed and its round-trip gate is green.

The architectural premise of AF is that AE makes morphology facts
trustworthy again. With shape-agnostic lowering producing correct
IR for every grammar regardless of optimizer state, the cost model
can finally read facts off `GrammarIR` without wondering whether
the bootstrap path silently zeroed them out. Every commitment in
AF leans on that premise.

## Context

After AE completes the tape-first substrate, every rule is a tape
record. That's a universal default, and it's the right default —
it's correct, it's load-bearing, it's the AB.0 lattice's `MustTape`
top. But it isn't the optimal choice for every rule.

A leaf rule with a `->` typed conversion body wants to compile
directly to `fn __rule_direct(state) -> Option<T>` without ever
pushing a tape record. The conversion is pure: the rule reads bytes,
runs the user's function, returns the typed scalar. The tape record
is unobservable overhead — nothing downstream walks it, the view
layer would just project it back through the same conversion, and
the per-record `push_compound` cost shows up at the top of every
parse profile for grammars whose hot loops are dominated by typed
leaves.

A rule with a static enum-variant lookup wants the same shape. A
rule whose body is `"px" -> 0u8 | "em" -> 1u8 | "rem" -> 2u8` is a
fused dispatch table that returns a `u8`; threading it through a
tape record costs cycles for no observable structure.

A rule with a delimited repetition of typed scalars wants the tape
for structural walk — because consumers may iterate it
declaratively — but should lazily project the scalar vector on
demand at the view layer, not at parse time. The tape record stays;
the projection moves.

AF makes this choice per-rule, automatically, from the IR
morphology facts the compiler already computes. Nothing about the
choice is a heuristic; everything reads from a lattice fact and
goes through a CSP variable.

## Three tiers

The emission lattice gains a new axis orthogonal to
`MaterializationClass`. Where materialization decides what record
shape a rule pushes, the emission tier decides what return type
the rule's parse function projects to.

- **Tier A — Tape only.** AC.2 / AE default. Every rule is a
  `fn __rule(state, tape) -> Option<TapeOffset>`. Universal,
  always legal, the safe top of the new lattice. Pinned by
  `@pretty`, `@debug`, multi-site consumers that walk the tape
  structurally, opaque rules, and any rule whose body is not a
  pure projection from bytes to a typed value.

- **Tier B — Direct-to-struct projection.** Leaf rules with typed
  `->` bodies skip the tape entirely and emit a second function
  alongside the tape shim:
  `fn __rule_direct(state) -> Option<T>`. The tape shim still
  exists for callers that need the structural record; the direct
  function exists for the hot path. Both share the same prelude
  parsing logic via a private helper. Eligibility requires
  `FixedShape` (no Vec / aggregates spilling beyond a 32-byte
  budget), pure conversion bodies (no `@pretty` / `@debug`
  pinning), and a single-site consumer model proven through the
  CSP's parent-compatibility constraint.

- **Tier C — Lazy typed AST at the view layer.** Universal, sits
  above Tier A. The view layer dispatches between tape walk
  (Tier A) and direct slot (Tier B) on demand. Every rule's
  `<Rule>View<'p>` gains a `direct: DirectSlot<'p>` field that
  holds a `Tier B`-projected value when the rule's emission tier
  is `Direct`, and a tape-cursor sentinel otherwise. Consumers
  pattern-match on `DirectSlot` to choose the right walk.

The three tiers form a join lattice with `Tape` at the top. The
CSP solves toward the lowest legal tier per rule, subject to the
parent-compatibility and pinning constraints that the tier
introduces.

## Activation phases

Each phase is a self-contained substrate-then-solver-then-emitter
landing. No phase consumes from a substrate that hasn't shipped.
Each phase deletes scaffolding from the previous phase that's no
longer load-bearing, with the explicit exception of the Y.5
`UnionFind` substrate, which AF.3 promotes from dormant to
production-active.

### AF.1 — Activate dormant infrastructure

Several pieces of infrastructure already exist in the tree but
don't have production consumers. AF.1 wakes them up before the
tier work begins, so the tier work has substrate to read from.

- **`EClassFacts` consumer.** Extend
  `classify_materialization`'s initial pass to read
  `is_fixed_shape` as a pre-seed for the Tier B `FixedShape`
  constraint. The fact is computed today and read by no one;
  AF.1 wires it into materialization domain construction so
  Tier B can reference it through a single sidecar lookup.

- **Dispatch FIRST-set caching.** Pre-compute
  `HashMap<NodeId, CharSet128>` in `generate_dispatch_tables` by
  piggybacking on the single tree walk in `annotate.rs`. Today
  the dispatch generator walks twice — once for FIRST sets, once
  for emission — and the FIRST-set walk is the longer of the two
  on CSS L4. Folding it into the annotate walk saves a
  measurable slice of compile time at zero behavioral cost.

- **Context facts fusion.** Move `compute_context_facts` into a
  new `ContextFactsMiner` implementing the Z.0 `RecognizerMiner`
  trait; delete the separate pass. `compute_context_facts` is
  the one outlier from the Z.0 single-walk recognizer mining
  consolidation — every other miner runs through `mine_recognizers`
  in one DAG walk. Folding context facts into the same walk
  removes a dedicated pass and a separate full-DAG traversal.

- **`TypeDesc` interning in codegen.** Route through
  `TypeDescId` in `backend/driver/{alt,seq,repeat,map}.rs`. The
  interner exists at `ir.type_desc_interner` and is populated end
  of `project_types`; the codegen still clones full `TypeDesc`
  values around. AF.1 threads the `TypeDescId` through the driver
  paths so the codegen reads from the interner instead of cloning.

These four activations are independent and mutually compatible.
Each ships as its own commit with a localized test. None changes
generated code.

### AF.2 — Universal cost model

Extend `egraph::CostWeights` with the strategy and emission knobs:
`ref_invocation`, `memo_overhead`, `dispatch_table_size`,
`rule_call_frequency_bonus`, `scc_cycle_penalty`, plus three new
materialization-tier weights for the Tier A / Tier B / Tier C cost
spread. Teach the CSP strategy solver to read `CostWeights`
directly instead of through `bbnf_ir::CostConfig`'s `strategy_*`
fields. Add `BBNF_COST_*` env var overrides for every new weight,
following the existing convention.

The result is one cost model — `egraph::CostWeights` — that the
grammar tier, the HIR tier, the CSP strategy solver, the
materialization classifier, and the e-graph extraction pass all
read from. The asymmetry that Z.6 closed for strategy knobs but
left open for emission knobs is closed in full. Any future tier
or any future cost-aware pass embeds the same struct.

The migration is mechanical: every `CostConfig::strategy_*` field
moves to `CostWeights`; every consumer updates its read site;
the old fields are deleted in the same commit. The
`StrategyConfig` sub-struct that Z.6 introduced is folded into
`CostWeights` directly — there's no second tier of nesting.

### AF.3 — Cross-rule CSP decomposition

Wake the Y.5 `UnionFind` substrate as its first real production
consumer. The substrate has been dormant since Y.5 — it solves
nothing, it observes nothing, it sits in
`crates/ir/src/passes/csp_strategy/components.rs` waiting for an
edge to traverse. AF.3 gives it edges and gives it work.

- **Per-Ref edge gathering** in
  `csp_strategy::components::build_components`. Walk every
  `IrNode::Ref` and add an edge between the calling rule's CSP
  variables and the target rule's CSP variables. The edges are
  cheap to enumerate — `ir.dag` already has every reference and
  the ref-target lookup is O(1) — and they partition the strategy
  variables into connected components by call topology.

- **Replace the per-rule solve loop** with
  `solve_grammar_components`. The current
  `solve_strategy_and_materialization` iterates rules in
  topological order and solves each rule's variables in
  isolation. The replacement walks components in topological
  order over the component graph and solves each component as a
  single CSP. Components with zero cross-rule edges degenerate
  to per-rule trivial picks, recovering the current fast path
  without a special case.

- **Install `EnginePropagation`** as the first cross-rule
  constraint. When a rule's regex engine is decided as
  `RegexEngine::Dfa`, propagate the decision to every rule that
  references it transitively — the engine choice is a property
  of the production grammar, not of any single rule, and per-rule
  solves have been making conflicting decisions for shared
  patterns. The propagation makes the engine choice consistent
  per-component.

The Y.-1 `node_budget` safety net from Tranche Y guards every
component solve. A blown component falls back to per-variable
trivial picks for that component without hanging the compile;
the per-component logging is preserved from the existing
`BBNF_CSP_REPORT=1` path.

### AF.4 — Three-tier emission substrate

The substrate that the rest of AF builds on. No solver, no
emitter — just the lattice and the sidecar.

- **`EmissionTier` enum** at
  `crates/ir/src/passes/materialization/lattice.rs`. Three
  variants: `Tape` (top), `Direct` (Tier B), `LazyView` (Tier C
  view-layer marker). Monotone join: disagreement widens toward
  `Tape`. The join is the same shape as `MaterializationClass`'s
  `mat_join`, just over a different axis.

- **Extend `Site` and `StrategyValue`** in
  `crates/ir/src/passes/csp_strategy/mod.rs` with the
  `Emission(NodeId)` variant. Domain is `{Tape, Direct, LazyView}`.
  No new constraint files yet; AF.5 lands the constraints.

- **Three new cost weights** on the universal `CostWeights` from
  AF.2: `emission_tape`, `emission_direct`, `emission_lazy_view`.
  Defaults bias toward `Direct` for `FixedShape` rules and
  `Tape` for everything else, but the CSP makes the actual call.

- **New `GrammarIR::emission_tier: HashMap<NodeId, EmissionTier>`
  sidecar.** Populated by the AF.5 decision pass; consumed by the
  AF.6 emitter. Read from `DriverState` via the same accessor
  pattern as `materialization_class`.

The lattice ships first; the consumers follow. Every existing
test passes because no consumer reads `ir.emission_tier` yet.

### AF.5 — Decision pass

Three unary clamps and one cross-rule binary implication
constraint, plus the decode pass that writes
`ir.emission_tier`.

- **`FixedShape` clamp** in `csp_strategy/domain_build.rs`. A
  rule whose `EClassFacts::is_fixed_shape` is false cannot be
  `Direct` — the projection would need to spill the aggregate to
  a side arena, which is out of scope for AF. Domain pre-filter,
  not a runtime constraint.

- **`TierFollowsMaterialization` clamp.** A rule pinned to
  `MustTape` by materialization (because of `@pretty`, `@debug`,
  multi-site consumer reuse, or any other materialization pin)
  cannot be `Direct`. Tier B requires the parser to skip the
  tape entirely; a `MustTape`-pinned rule cannot skip what it's
  required to push.

- **`NoPrettifyChain` clamp.** Any rule transitively reachable
  from a `@pretty` directive is clamped to `Tape`. Prettify
  walks parser state, not typed values, and a Tier B rule
  inside a prettify subtree would lose its observable structure.

- **`ParentCompatibility` binary implication constraint.** The
  second cross-rule edge type, after AF.3's `EnginePropagation`.
  When a parent Seq/Alt/Repeat is `Tape`, its `Direct` children
  must round-trip through a tape push at the parent boundary —
  the parent needs a `TapeOffset`, the child returns a typed
  value, and the boundary materializes a tape record from the
  typed value. The constraint penalizes that boundary by the
  cost of the round-trip; the CSP discovers when the round-trip
  is worth it and when `Direct` children should be `Tape` instead.

- **Decode pass.** Walks the solved CSP and writes
  `ir.emission_tier`. Same shape as the existing
  `decode_min_cost_per_variable` for `MaterializationClass`.
  Default fallback: `Tape`. Tape is always legal.

### AF.6 — Emitter + view codegen

The consumer side. Two emitters — one for the rule function
itself, one for the view layer — both reading from
`ir.emission_tier`.

- **`emit_rule_function_impl`** reads
  `ir.emission_tier[rule_root_id]`. On `Direct`, emits a second
  `__rule_direct` fn alongside the standard tape shim. The
  direct function returns `Option<T>` where `T` is resolved
  through the rule's `TypeDescId` (interned via AF.1's
  threading). The tape shim and the direct function share their
  prelude parsing logic via a private helper, generated once
  per `Direct` rule.

- **`view/mod.rs::generate_views`** emits a `DirectSlot<'p>`
  enum at the top of every grammar's view module. Variants:
  `Empty` (for `Tape`-tier rules), `Typed(T)` (one per
  `Direct`-tier rule's projected type). Adds a `direct:
  DirectSlot<'p>` field on every `<Rule>View<'p>`. View
  consumers pattern-match on `DirectSlot` to choose between the
  tape walk and the direct value.

- **Gated by `BBNF_AD_EMISSION=1`** for bench diffing during the
  AF.6 development cycle. The gate flips off in AF.7. The gate
  exists so the cost-model parameter sweep can run with the
  emitter on or off without recompiling the world.

### AF.7 — Default-on plus cleanup

Flip the `BBNF_AD_EMISSION` gate. Delete it. Delete every
`cfg!()` that the gate threaded through. Run a fresh post-AF
samply baseline: `compile_bbnf`, `compile_css_l4`,
`compile_json`, plus every parse-time bench from the post-AC.2
sweep. Capture the profiles to
`docs/benchmarks/profiles/post-AF/*.samply`. Write
`docs/benchmarks/post-AF.json` with per-phase attribution; every
"+X%" claim cites a samply profile delta.

Floor gates are relaxed from post-AC.2 because AF adds runtime
optimization at the cost of moderate compile-time degradation.
Tier B's two-function emission is roughly 1.2× the per-rule
codegen cost of Tier A; the parse-time wins on hot leaf rules
purchase the slowdown.

Concrete relaxations:
- `compile_bbnf` ≤ 1.3× pre-AF (was 1.5× pre-AC.2)
- `compile_css_l4` ≤ 2.5× pre-AF (was 2.0× pre-AC.2)
- Every parse-time floor from post-AC.2 holds, with the addition
  of two new gates for hot Tier B grammars: `json_canada` parse
  ≥ 1.7 GB/s, `css_tailwind` parse ≥ 0.40 GB/s.

## Architectural commitments (forward-looking)

1. **No legacy code.** Every phase deletes or activates. The
   Y.5 `UnionFind` scaffolding from AB.1 is the sole exception,
   and AF.3 retires it into active service. Nothing accumulates;
   AF closes with the same hygiene as AC and AE.

2. **One universal cost model.** Post-AF.2, the grammar tier,
   the HIR tier, the CSP strategy solver, the materialization
   classifier, and the e-graph extraction pass all read the
   same `egraph::CostWeights`. Any future tier, any future
   cost-aware pass, embeds the same struct. Asymmetry between
   strategy knobs and emission knobs is gone.

3. **Global optimization.** Post-AF.3, the CSP solves
   per-component, not per-rule. Cross-rule constraints —
   `EnginePropagation`, AF.5's `ParentCompatibility`, any future
   constraint that reaches across the call graph — propagate
   through the component graph and produce consistent decisions
   per-component. Per-rule solves are recovered as the
   degenerate case for components with no cross-rule edges.

4. **Three tiers decided automatically** from IR morphology
   facts. No heuristic threshold, no profile-guided override at
   the per-rule level. The CSP reads `EClassFacts`,
   `RuleMeta`, `ir.materialization`, and the universal cost
   model; the decision pass writes `ir.emission_tier`; the
   emitter consumes the sidecar. Every layer reads from the
   layer below; nothing skips the lattice.

5. **Compilation time can degrade moderately.** AF is a `-O3`
   pass: runtime wins purchase non-trivial compiler work. The
   floor gate budget is explicit and the post-AF baseline
   captures every degradation with a samply profile delta. If a
   phase produces compile-time degradation that the parse-time
   delta does not justify, the phase is reverted.

6. **Substrate before solve before emit.** Each AF phase lands
   substrate first (the lattice, the sidecar, the cost weights),
   then the decision pass (the CSP variables, the constraints,
   the decode), then the emitter consumer (the rule function,
   the view layer). No phase consumes from a substrate that
   hasn't shipped.

## Out of scope (even for AF)

- **TS / WASM backend migration.** Rust emitter only. The
  three-tier emission is a Rust-backend-local concern; the
  `Emitter` trait gains no new required methods. TS and WASM
  keep their existing eager AST surface, exactly as they did
  through AC.2 and AE.

- **VM bytecode path.** Unchanged. The VM consumes the same
  IR; the three-tier decision is invisible to it.

- **`DirectSlot` arena spill.** Aggregates larger than 32 bytes
  cannot project to `Direct` — the projection would require a
  side arena (`Box<dyn Any>` or similar) that AF deliberately
  excludes. Such rules are clamped to `Tape` via the
  `FixedShape` constraint. Side-arena spill is a candidate for
  a future tranche over the stable AF substrate.

- **Per-grammar profile-guided cost calibration.** AF reads the
  universal `CostWeights` with default values plus environment
  overrides. A feedback loop that tunes the weights from samply
  profiles per grammar is a follow-up tranche that builds on
  AF's universal substrate.

- **HIR-tier projection / direct scalar return from regex
  engines.** The bbnf-regex HIR e-graph stays `NoAnalysis`. The
  three-tier emission lattice lives in the grammar tier; the
  HIR tier's analysis-free regex extraction is unchanged.
  Cross-tier projection is not in AF's scope.

## The key insight

AC.2 collapsed every rule to a single ABI: `Option<TapeOffset>`.
That collapse was the right architectural close for the
substrate, and it remains the right default. AF observes that
the collapse is uniform where the grammar isn't: leaf rules with
typed conversions, rules with static enum lookups, rules with
fixed-shape projections — these don't benefit from the universal
ABI, they pay for it. The cost is small per call but compounds
across the parse loops that dominate every production profile.

The morphology facts that distinguish a `Direct`-eligible rule
from a `Tape`-pinned rule already exist. `EClassFacts` knows
whether a rule is fixed-shape; `RuleMeta` knows whether a rule
is `@pretty`-pinned; `ir.materialization` knows whether a rule
must push a record. What's missing is the lattice that joins
those facts into a per-rule emission decision and the CSP that
enforces parent-compatibility across the resulting choices. AF
lands the lattice and lets the CSP make the call.

The execution gate on AE is non-negotiable. Every fact AF reads
from the IR depends on the IR being correct in the first place.
AE makes the IR correct for the bootstrap path, for the
optimizer-gated path, for every shape-agnostic walk through the
tape boundary. AF is the consumer that finally gets to read it.
