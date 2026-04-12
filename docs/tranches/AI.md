# Tranche AI — Automatic Optimal Emission

## Context

AG closed the AF circle.  The three-tier emission lattice (Tape /
Direct / Lazy), the per-component CSP with cross-rule constraints,
and the shared CostWeights cost model are all live infrastructure.
But the substrate has no consumer: the Rust emitter dispatches solely
on `MaterializationClass`, ignoring `ir.emission_tier` entirely.
Zero production grammar rules classify as Direct because Tier B
eligibility is intentionally narrow (TransparentElide + bare-leaf
closure-free Map bodies only).  The five view-layer stub files carry
7-14 lines each with "Full implementation lands in AC.2" comments.

AI's thesis: **every rule's emission strategy is the output of a
single cost-minimization problem where the CSP, e-graph, and
materialization classifier cooperate to find the maximally efficient
emission form.**  The e-graph rewards body shapes that enable Direct
emission.  The CSP propagates tier constraints across the call graph.
The emitter reads the solved tier and emits the corresponding shim.
The view layer projects typed accessors over either tape cursors or
direct values.

Post-Z.7 baseline (the floor we must not regress below):
- JSON canada: 1,231 MB/s
- JSON citm: 1,896 MB/s
- CSS L4 tailwind: 256 MB/s
- CSS L4 bootstrap: 243 MB/s

## AI.1 — Wire emission tier into the Rust emitter

`ir.emission_tier` becomes a live consumer.

In `emit_rule_function_impl` (`backend/rust/emitter/grammar.rs`),
after resolving `MaterializationClass`, look up
`ir.emission_tier[rule.id]`:

- **Tape** -- unchanged AE behaviour (tape prelude + epilogue).
- **Direct** -- emit `__<rule>_inner(state) -> Option<T>` (parse
  logic), `__<rule>(state, tape)` (tape wrapper calling inner +
  push_compound), and `__<rule>_direct(state) -> Option<T>` (direct
  shim calling inner).  The inner-extraction refactor keeps the parse
  body in one place.
- **Lazy** -- emit tape function + annotate rule for view-layer
  `DirectSlot` generation (AI.5 consumes the annotation).

At call sites (`backend/driver/reference.rs`), when emitting
`Ref(target)` and the target is Direct, emit
`Self::__<target>_direct(state)` instead of
`Self::__<target>(state, tape)`.  When a Tape caller invokes a
Direct callee, wrap the return in a synthetic `push_leaf` coercion
priced at `cross_module_coercion`.

Critical files:
- `crates/core/src/backend/rust/emitter/grammar.rs`
- `crates/core/src/backend/rust/emitter/tape_prelude.rs`
- `crates/core/src/backend/driver/reference.rs`

Gate: `cargo test --workspace` green.  JSON grammar still compiles
(zero Direct rules -> no output change).  Test
`crates/core/tests/tier_b_emission.rs` with a synthetic
Direct-eligible grammar asserting the `__rule_direct` shim appears
in the generated TokenStream.

## AI.2 — Widen Tier B eligibility

Production grammars have >=3 Direct-eligible rules.

Relax `decide_tier_b_eligibility` (`decode_tier.rs`) to cover:

1. **Typed Seq projections** -- `Seq(children)` where every child
   is a leaf or a transparent Ref.
   `EClassFacts::all_descendants_elidable` already tracks this.
   Return `Direct`; TypeMap provides the tuple type.
2. **Single-branch Alt** -- `Alt([single])` collapses to the
   child's tier.  Degenerate case left by the normalizer.
3. **Map over transparent Ref** -- `Map { inner: Ref(target),
   fn_id }` where target is `TransparentElide` and fn_id is
   non-closure.  Remove the `Ref(_) -> Tape` guard; check the
   target's class.
4. **Bounded Repeat over transparent body** --
   `Repeat { inner, lo, hi }` where inner is a leaf/transparent Ref
   and hi != MAX.  `EClassFacts::is_fixed_shape` gates this.

Critical files:
- `crates/ir/src/passes/csp_strategy/decode_tier.rs`
- `crates/ir/src/egraph/analysis/facts.rs` (read-only)

Gate: JSON grammar has >=3 Direct-eligible rules (e.g. `string`,
`number`, `boolean` map variants).  Test
`crates/ir/tests/lattices/tier_b_eligibility.rs` with synthetic
rules.

## AI.3 — Emission-aware e-graph cost model

The e-graph prefers body forms that enable Direct emission.

Add `emission_tier_bonus: f64` (default -1.5, reward) to
`CostWeights`.  In `GrammarCostModel::cost`
(`crates/ir/src/egraph/cost.rs`), apply the bonus to e-nodes whose
structural shape passes the TransparentElide gate:

- Literal / Regex / Epsilon -- always apply.
- Map with non-closure FnDescriptor -- apply.
- Seq of <=3 leaves/Refs -- apply (discounted).
- Skip where `a` is a leaf -- apply.

This is a heuristic: the downstream classifier makes the final call.
The bonus biases extraction toward Direct-enabling forms without
overriding correctness.

Critical files:
- `crates/egraph/src/cost_weights.rs`
- `crates/ir/src/egraph/cost.rs`
- `crates/ir/src/cost_config.rs`

Gate: e-graph test with two equivalent forms in the same e-class;
extraction picks the Direct-enabling one.  `cargo test -p bbnf-ir`.

## AI.4 — Cross-component tier reconciliation

Global tier coherence across the call graph.

After `solve_grammar_components` returns per-component decisions,
walk every `Ref` edge via `compute_rule_deps`.  For each (parent,
child) pair where the parent's tier rank < child's tier rank (parent
is Direct but child is Tape), promote the parent to
`tier_join(parent, child)`.  This is a single monotone widening pass
over the DAG (the call graph is a DAG after SCC decomposition).

For -O3 mode: re-solve affected components after reconciliation,
then reconcile again.  Cap at 3 iterations (the 3-element tier
lattice guarantees fixpoint in <=3 rounds).

Wire into `compile.rs` between `solve_grammar_components` and
`decode_emission_tier`.

Critical files:
- `crates/ir/src/passes/csp_strategy/mod.rs`
  (new `reconcile_cross_component_tiers`)
- `crates/core/src/pipeline/compile.rs`

Gate: three-rule chain test A->B->C across components; verify
reconciliation promotes correctly.  `cargo test -p bbnf-ir`.

## AI.5 — View-layer typed accessors

The 5 stub files become real per-rule-kind codegen.

The universal accessors in `view/mod.rs` (348 lines) remain.  Each
stub becomes a per-kind accessor generator driven by `TypeDesc`:

- **`leaves.rs`** -- `.text() -> &'p str`, `.value() -> T` for
  F64/U32/Named rules.  For Lazy-tier rules, `DirectSlot` dispatch.
- **`seq.rs`** -- `.child_N() -> <ChildType>View<'p>` per Seq
  position.  Named children via Ref target name when available.
- **`alt.rs`** -- `.as_<variant>() -> Option<VariantView<'p>>` per
  Alt branch, discriminated on `variant_idx()`.
- **`repeat.rs`** -- `.iter() -> impl Iterator<Item = <Inner>View>`,
  `.len() -> usize`.
- **`grammar.rs`** -- top-level Root binding.

Each generator is called from `generate_views` based on the rule's
body kind and materialization class.

Critical files:
- `crates/core/src/backend/rust/view/{mod,alt,seq,leaves,repeat,grammar}.rs`
- `crates/core/tests/view_typed_accessors.rs` (new)

Gate: parse a JSON document through the generated parser, walk the
view tree using typed accessors, assert correctness.

## AI.6 — Ghost cleanup + bench baseline

Zero stubs, zero TODOs, zero ignored tests.  Samply baseline with
floor gates.

1. **Unignore remaining tests** -- the 4 `cost_weights_unified`
   ignores + `grammar_roundtrip` ignores.
2. **Delete all view stubs** -- the 5 files are real code after AI.5.
3. **Fix 17 TODO/FIXME markers** -- each gets the implementation or
   a deletion.
4. **Samply profiles** -- `docs/benchmarks/profiles/post-AI/` via
   interactive `samply record`.  JSON + CSS L4 + Google Sheets.
5. **Floor gates** -- post-AI throughput >= post-Z.7 baseline.
6. **CLAUDE.md update** -- architecture state section reflects the
   three-tier emission, emission-aware cost model, cross-component
   reconciliation.

Gate: `cargo test --workspace` zero ignored.
`cargo clippy --all-targets -- -D warnings` clean.  Bench JSON >=
post-Z.

## Dependency graph

```
AI.1 (wire emitter)  <-- FIRST
  |
  v
AI.2 (widen Tier B)     AI.3 (e-graph cost)
  |                        |
  v                        v
AI.5 (view accessors)   AI.4 (cross-component)
  |                        |
  v                        v
AI.6 (cleanup + bench) <--+
```

AI.1 must land first.  AI.2 and AI.3 are independent of each other.
AI.4 follows AI.3.  AI.5 follows AI.2.  AI.6 follows all.
