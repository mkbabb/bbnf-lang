# Tranche AB — Tape as the Only Runtime Substrate

## Context

Tranche AA (AA.0–AA.13) committed the substrate: `bbnf-tape` leaf
crate, `TypeDescInterner`, `GrammarAnalysis` / `EClassFacts`,
`csp-solver` vendored, `Lattice` trait, `Rewrite::should_apply`,
IIFE → labeled blocks emitter refactor. What AA did not commit is
a *consumer* for the tape. The parser still emits the eager AST
through `emit_alloc` → `BoxedEnum` → slab allocation. The tape sits
idle behind the public surface.

Tranche AB is the architectural close. It commits to **tape as the
only runtime output substrate**. Every emitted rule returns
`Option<TapeOffset>`. There is one parser ABI, one control-flow path,
one universal output format. Projection — direct scalar return, direct
aggregate builder, scalar side-arenas — is explicitly deferred to a
post-AB tranche over the stable tape substrate.

The earlier framing (tape-first hybrid with analysis-driven selective
direct projection) was withdrawn after design review: mixing parser
return types across rules (`Option<TapeOffset>` for tape rules,
`Option<f64>` for scalar rules, `Option<Rgb>` for aggregate rules)
forces per-call-site dispatch into the Seq emitter and the
`<Grammar>Enum` discriminator, multiplies the parity surface, and
couples the CSP solver to a two-dimensional decision space before a
single grammar runs end-to-end on the tape.

The commitment AB makes is narrower and more durable:

- **Tape is not just the law; tape is the only runtime output
  substrate in this tranche.** All optimization must preserve that.
- **One parser ABI**: every emitted rule returns
  `Option<TapeOffset>`. No mixed return types. No scalar-return
  special cases. No aggregate-return special cases.
- **Projection is a tape-layer optimization, not a parser-output
  strategy.** After the tape is stable, a follow-up tranche can
  introduce accessor fusion, view-layer scalar extraction, and
  internal builder shortcuts — all preserving the single ABI.
- **Public API is an owning parsed result type.** `Parsed<RootView<'_>>`
  holds the tape inline and lends out typed lazy views over it. No
  `(View, Tape)` tuple forcing callers to think about substrate
  lifetime.

The architectural benefit of direct projection still lands — but as
accessors fused over a stable tape in a follow-up tranche, not as a
parser-output strategy dragging AB into a mixed ABI.

## Inherited state from Tranche AA

The substrate needed for the single-ABI tape is in place at HEAD:

- **`bbnf-tape` crate** — leaf crate with 16-byte size-asserted
  `TapeRec`, `TapeKind` enum (13 variants), `TapeBuilder`
  (`mark_children` / `push_leaf` / `push_compound`),
  `TapeCursor<'tape>` (bounded by parent offset, pre-order child
  iterator), `ChunkedArena<T>` (64 KB chunks), 9 round-trip tests.
- **`TypeDescInterner`** on `GrammarIR::type_desc_interner` — hash-
  cons interning for structural type equality via `TypeDescId(u32)`,
  populated end of `project_types`, serde round-trip.
- **`GrammarAnalysis` / `EClassFacts`** — per-e-class lattice
  (first_set, nullable, width, literal_sid, regex_sid) running via
  `EGraph<GrammarENode, GrammarAnalysis>`. All grammar-tier rules
  genericized over `A: Analysis<GrammarENode>`.
- **`csp-solver`** vendored at `crates/csp-solver/` with
  `SoftLambdaConstraint`, `node_budget` safety net, `MinimizeCost` B&B.
- **`egraph::Lattice` trait** + `Scalar<T>` newtype. `CostModel::Cost:
  Lattice` bound.
- **`Rewrite::should_apply`** default-true predicate guard, threaded
  through `RewriteFn::run` and `run_on_dirty`.
- **Y.5 `UnionFind` substrate** — dormant cross-rule topology
  awaiting its first production consumer.
- **Labeled blocks** — every IIFE `(|| { ... })()` in the Rust emitter
  replaced with `'label: { ... break 'label #expr }`. Emitter clean
  for incremental per-kind changes.

**Load-bearing gap carried into AB**: `RecognizerDecision` carries
`alt_mode` / `wrap_mode` / `regex_engine` but no materialization
decision. There is no CSP variable, no sidecar, and no analysis
lattice fact encoding "does this node produce a full tape record,
a span-only record, or nothing at all." AB adds that substrate and
wires it end-to-end — scoped to the three tape-safe classes.

---

## The architecture — tape only, three classes

### Analysis lattice (`MaterializationClass`)

A `NodeId`-keyed sidecar on `GrammarIR::materialization` with **three**
variants, ordered by a monotone lattice where disagreement widens
toward the safe top:

```
               MustTape           ← top (safest default, always legal)
                  |
             TapeSpanOnly
                  |
            TransparentElide      ← bottom (most aggressive elision)
```

- **`MustTape`** — full tape record with `push_compound(kind,
  children, ...)`. The universal legal default. Pinned by `@pretty`,
  `@debug`, `preserve_identity`, multi-site consumer reuse, opaque
  rules, and any rule whose descendants aren't uniformly elidable.
- **`TapeSpanOnly`** — single `push_leaf(Span, ...)` record, no
  children, no compound header. Emitted for punctuation tokens,
  `Negate`, `OptionalWhitespace`, `Skip` left-sides, `@token`-annotated
  rules, and any leaf whose span is the only information a consumer
  needs. Still emits a tape record — still returns `Option<TapeOffset>`.
- **`TransparentElide`** — no record, no function. The rule is inlined
  at every call site during code generation. `is_alias` /
  `is_transparent` rules whose body is itself elision-compatible.
  The inlined body's own materialization class decides what records
  (if any) get pushed.

**Explicitly dropped** from AB: `DirectProjection` (scalar return,
aggregate return) and `TapeCompact` (bulk-encoded repeats with side
arenas). Both are projection-style optimizations that break the
one-ABI commitment and land in a post-AB tranche over the stable tape.

Classification is computed by a new pass `classify_materialization(ir)`
that runs **after** `project_types` inside `finalize_compile`. It
consumes:

- `TypeDescId` via `ir.type_desc_interner`,
- `EClassFacts` snapshotted into a NodeId-keyed `ir.eclass_facts`
  sidecar (new monotone fields: `elision_safe`, `closure_free`,
  `is_fixed_shape`, `all_descendants_elidable`),
- `RuleMeta.directives.pretty` / `.debug` / `.token`,
  `RuleMeta.preserve_identity`, `RuleMeta.is_transparent`,
  `RuleMeta.is_alias`,
- `ir.node_facts` (recognizer shapes) and `ir.recognizer_decisions`
  (strategy decisions).

Pass 1 is bottom-up over `ir.dag`; pass 2 is a consumer-pin fix-up
walking rules with `@pretty`/`@debug` and transitively pinning their
subtree to `MustTape`; pass 3 is a debug-only assertion sweep.

### CSP joint solve

`solve_strategy_decisions` becomes `solve_strategy_and_materialization`
and runs **grammar-wide** via the Y.5 `UnionFind` substrate — the first
production consumer of the dormant cross-rule topology. The `Site`
enum is extended with **one** new variable class:

```rust
enum Site {
    Alt(NodeId), Wrap(NodeId), Engine(NodeId),      // existing
    Materialization(NodeId),                          // domain: {MustTape, TapeSpanOnly, TransparentElide}
}
```

No `RecordShape`, no `ChildEncoding`, no `UnionDiscriminant`. The
reduced decision space keeps AC-3 propagation trivially tractable and
keeps the constraint surface small.

Cost weights (added to `CostConfig::strategy`):

| Weight | Default | Meaning |
|---|---|---|
| `materialization_must_tape` | `+6.0` | Baseline tape record cost |
| `materialization_tape_span_only` | `+3.0` | Cheaper: single leaf |
| `materialization_transparent_elide` | `-5.0` | Biggest reward: rule erased |
| `prettify_pin_penalty` | `1e9` | Soft-hard pin for `@pretty` subtrees |
| `debug_pin_penalty` | `1e9` | Soft-hard pin for `@debug` subtrees |

Cross-variable constraints:

- **`ParentPinsChild`** — if a Seq/Skip/Next parent is `MustTape`, its
  first structural child must push a record. If the child is
  `TransparentElide`, the transitively inlined body must itself push
  something the parent can collect.
- **`PrettifyPin`** — `SoftLambdaConstraint` with `1e9` penalty; every
  node inside a `@pretty` subtree is pinned to `MustTape`.
- **`DebugPin`** — same shape for `@debug`.
- **`ConsumerReuse`** — cross-rule domain pre-filter: if
  `ir.rule_uses[rule] > 1`, pre-filter `TransparentElide` out of every
  NodeId in the rule's body at domain-build time.
- **`TransparentElideChain`** — live cross-rule edge: a transparent
  rule with elision-compatible body elides at every call site.

Fallback semantics: budget-exceeded → domain-clamp every
`Materialization` to `{MustTape}`; run `decode_min_cost_per_variable`.
Unsatisfiable → emit `MustTape` unconditionally. The tape is always
a legal target.

Cross-rule decomposition via `UnionFind`: `TransparentElideChain` and
`PrettifyPin` are the only constraints that create cross-rule edges.
Per-component B&B under the existing `node_budget` safety net.

### Emitter / codegen integration

**One generated parser function signature for every rule:**

```rust
fn __rule<'a>(
    state: &mut ParserState<'a>,
    tape: &mut TapeBuilder,
) -> Option<TapeOffset>
```

Every rule, regardless of materialization class, returns
`Option<TapeOffset>`. This is the single parser ABI commitment.

**MustTape shape** (JSON `__pair`):

```rust
fn __pair<'a>(state: &mut ParserState<'a>, tape: &mut TapeBuilder) -> Option<TapeOffset> {
    'rule_blk: {
        let __span_lo = state.offset as u32;
        let __children = tape.mark_children();
        let _k = match __string(state, tape) { Some(v) => v, None => break 'rule_blk None };
        state.skip_ws();
        if state.eat_byte(b':').is_none() { break 'rule_blk None; }
        state.skip_ws();
        let _v = match __value(state, tape) { Some(v) => v, None => break 'rule_blk None };
        Some(tape.push_compound(TapeKind::Rule, __children, __span_lo, state.offset as u32, PAIR_VIDX))
    }
}
```

**TapeSpanOnly shape**:

```rust
fn __comma<'a>(state: &mut ParserState<'a>, tape: &mut TapeBuilder) -> Option<TapeOffset> {
    let __span_lo = state.offset as u32;
    state.eat_byte(b',')?;
    Some(tape.push_leaf(TapeKind::Span, __span_lo, state.offset as u32, 0))
}
```

**TransparentElide**: no function emitted. `compile_grammar` skips
the rule entirely. `compile_ref` inlines the body at the call site
when `materialization[ref_node] == TransparentElide`. The inlined
body pushes whatever its own class says to push — the caller sees
`Option<TapeOffset>` from the inlined expression.

**Mixed composition is trivial** because there is no mixing: every
child of every Seq/Alt/Repeat returns `Option<TapeOffset>`. The Seq
emitter collects offsets via `match ... Some(v) / None => break`,
then pushes a single compound at the end. No per-child dispatch on
return type. No `(Ident, MaterializationClass)` tuples.

**Driver threading**: `DriverState` gets
`pub materialization: HashMap<NodeId, MaterializationClass>`. The
emitter reads it at the rule/node boundary — no new parameters
threaded through every `emit_*` method.

**View type generation**: every rule gets a `<Rule>View<'tape>`
wrapper struct with accessor methods walking `TapeCursor`. The
top-level grammar enum `<Grammar>Enum<'tape>` branches per variant to
the view types. This is the only typed surface — there are no
scalar-return or struct-return rules bypassing the view layer.

### Public API — owning parse result

```rust
pub struct Parsed<View> {
    // Owns the tape inline; the view is a cursor into it.
    tape: bbnf_tape::Tape,
    root_offset: bbnf_tape::TapeOffset,
    _view_marker: std::marker::PhantomData<View>,
}

impl<View> Parsed<View> {
    pub fn view(&self) -> View { /* constructs View from (tape, root_offset) */ }
    pub fn tape(&self) -> &bbnf_tape::Tape { &self.tape }
    pub fn into_tape(self) -> bbnf_tape::Tape { self.tape }
}

impl Json {
    pub fn parse<'a>(input: &'a str) -> Result<Parsed<ValueView<'a>>, ParseErr> {
        let mut state = ParserState::new(input);
        let mut builder = TapeBuilder::with_capacity(1024);
        let root_off = Self::__value(&mut state, &mut builder).ok_or(ParseErr::Syntax)?;
        let tape = builder.finish().map_err(ParseErr::Tape)?;
        Ok(Parsed { tape, root_offset: root_off, _view_marker: PhantomData })
    }
}
```

The caller never deals with a `(View, Tape)` tuple. The tape is owned
by `Parsed<_>`, the view is lent out by `.view()`, and the lifetime
relationship is natural.

**Prettify**: the existing prettify emitter (`__<rule>_prettify(state,
builder)`) walks parser state directly and emits FmtBuilder ops — it
is already tape-agnostic. No changes. Prettify-mode parse calls reset
state between the parse pass and the prettify pass, exactly as today.

---

## Phase plan

### AB.0 — `MaterializationClass` + `classify_materialization`

- **New**: `crates/ir/src/passes/materialization/` directory module.
  Hosts the 3-variant `MaterializationClass` enum, the monotone
  lattice `mat_join`, and the `classify_materialization` pass with
  its three sub-passes (bottom-up initial + consumer-pin sweep + debug
  assertion sweep).
- **New fields on `EClassFacts`** (`crates/ir/src/egraph/analysis/facts.rs`):
  `elision_safe: bool`, `closure_free: bool`, `is_fixed_shape: bool`,
  `all_descendants_elidable: bool`. Extend `GrammarAnalysis::make` in
  `analysis/mod.rs` to compute them per-variant; extend `merge` with
  the four monotone joins.
- **New sidecar on `GrammarIR`**: `pub eclass_facts: HashMap<NodeId,
  EClassFacts>` (`#[serde(skip)]`) — snapshotted from
  `egraph.class(id).data` during `write_back_optimized`, then rekeyed
  to NodeId after the DAG build.
- **New sidecar on `GrammarIR`**: `pub materialization:
  HashMap<NodeId, MaterializationClass>` (`#[serde(skip)]`) —
  populated by `classify_materialization`.
- **Pipeline wiring**: `classify_materialization(&mut ir)` runs
  immediately after `project_types` in `finalize_compile`.
- **Tests**: `crates/ir/tests/materialization.rs` — lattice join
  (commutativity, idempotence, monotonicity), fixture grammars
  (transparent alias, span-only punctuation, prettify-pinned subtree),
  debug assertion sweep.

**Does not ship**: no CSP extension, no emitter changes. The
classification is computed and available via `ir.materialization`
but no consumer reads it yet.

### AB.1 — CSP joint strategy + materialization solve

- **Extend `Site` + `StrategyValue`** in
  `crates/ir/src/passes/csp_strategy/mod.rs` with the single
  `Materialization(NodeId)` variable class.
- **New constraints** in
  `crates/ir/src/passes/csp_strategy/constraints/`:
  `parent_pins_child.rs`, `prettify_pin.rs`, `debug_pin.rs`,
  `transparent_elide_chain.rs`.
- **Cost weights** on `CostConfig::strategy` — 5 new f64 fields
  with `BBNF_COST_*` env var overrides.
- **Domain builder**: `build_materialization_domain` consuming
  `EClassFacts::{elision_safe, all_descendants_elidable}` +
  `RuleMeta` directives.
- **Grammar-wide solve** via Y.5 `UnionFind`: rewrite `solve_rule` to
  `solve_component`; wake `components.rs::UnionFind` as first
  production consumer. Replace the per-rule loop with per-component.
- **Rename**: `solve_strategy_decisions` →
  `solve_strategy_and_materialization` (old name as deprecated alias
  for one phase).
- **Extraction bridge**: new `crates/ir/src/egraph/extraction_bridge/`
  module that walks extracted e-nodes and produces
  `SoftLambdaConstraint`s from `EClassFacts::elision_safe`.
- **Tests**: `crates/ir/tests/csp_materialization.rs` — fixture
  grammars with golden classification outcomes, per-constraint
  regression tests, multi-rule UnionFind component enumeration.

**Does not ship**: no emitter changes. `ir.materialization` is now
populated by the CSP solve; `classify_materialization` becomes the
initial estimate and the CSP refines it.

### AB.2 — Tape-first emitter baseline (the architectural close)

This is the tranche's real architectural close. Every production
grammar must compile and parse on the tape with every rule returning
`Option<TapeOffset>`. The parity gate is byte-identical structural
output against a pre-AB snapshot.

- **Add `materialization` to `DriverState`** at
  `crates/core/src/backend/driver/mod.rs`. Populated by the CSP
  solver (AB.1) or defaulted to `MustTape` for every NodeId.
- **`emit_rule_function_impl`** in
  `crates/core/src/backend/rust/emitter/grammar.rs`: emit the
  `(state, tape) -> Option<TapeOffset>` signature unconditionally. The
  body shape depends on `materialization[rule]`:
  - `MustTape` → prelude (`__span_lo`, `mark_children`) + body +
    epilogue (`push_compound`).
  - `TapeSpanOnly` → `__span_lo` + body + `push_leaf(Span, ...)`.
  - `TransparentElide` → no function emitted; handled in `compile_ref`.
- **`emit_rule_prelude` / `emit_rule_epilogue`** — new helpers on
  `RustEmitter` for the two record-emitting shapes.
- **Child call shape** in `compile_ref` /
  `crates/core/src/backend/rust/emitter/reference.rs`: every child
  call is `__rule(state, tape)` returning `Option<TapeOffset>`.
  `TransparentElide` refs inline the body at the call site.
- **Delete `emit_alloc` for all rules** at
  `crates/core/src/backend/rust/ir_types.rs:295`. The 14 indirect
  call sites in the emitter siblings become dead. Delete `BoxedEnum`
  in the same commit if grep is clean.
- **`<Grammar>Enum<'a>`** becomes `<Grammar>Enum<'tape>`. Every
  rule's variant becomes `<Rule>View<'tape>`. Generate view types in
  a new `crates/core/src/backend/rust/view/` directory module
  (per-kind siblings: `leaves.rs`, `seq.rs`, `alt.rs`, `repeat.rs`,
  `grammar.rs`) producing accessor methods over `TapeCursor`.
- **`Parsed<View>` type** in `crates/core/src/runtime/parsed.rs` (or
  equivalent) — the owning parse artifact.
- **`#[derive(Parser)]` surface** at `crates/derive/src/lib.rs`:
  generate `pub fn parse<'a>(input: &'a str) -> Result<Parsed<RootView<'a>>, ParseErr>`.
- **Prettify integration**: no changes to prettify emitter.
- **Parity gate**: `crates/core/tests/tape_parity.rs` compiles every
  production grammar (JSON, CSS L4, BBNF, Sheets, EBNF), runs 20+
  sample inputs through the tape-emitter output, and verifies
  structural equivalence + spans + prettify parity + invalid-input
  behavior against a golden snapshot captured pre-AB. Every grammar
  must match.

### AB.3 — View generation + consumer migration

- **Lazy typed views**: generate per-rule `<Rule>View<'tape>` accessor
  types in `crates/core/src/backend/rust/view/`. Each view holds a
  `TapeCursor<'tape>` and exposes child accessors (`.key()`,
  `.value()`, `.items()`) that re-wrap cursors.
- **`<Grammar>Enum<'tape>`**: variant discriminator keyed on
  `TapeKind` + `variant_idx` flags. Codegen assigns one
  `TapeKind::Rule` + a `variant_idx` per rule.
- **Derive surface** at `crates/derive/src/lib.rs`: `#[derive(Parser)]`
  emits the `parse` fn returning `Parsed<RootView<'_>>`.
- **Serializer migration**: `crates/ser/` consumers switch from
  walking the eager AST to walking view cursors. The `Serializer`
  trait surface is unchanged; what changes is the source of the walk.
- **Schema / debug helpers**: existing rule-meta helpers use
  `TapeCursor::kind()` + `variant_idx()` for dispatch.
- **Prettify parity**: the prettify emitter walks parser state
  during a second pass, unchanged. The parity gate confirms
  byte-identical formatter output against the pre-AB snapshot.
- **View type tests**: extend `crates/core/tests/tape_parity.rs` to
  walk the view types and compare against a golden structural
  snapshot.

### AB.4 — Tape elision optimizations

Only after AB.2/AB.3 are stable and the parity gate is green can
`TapeSpanOnly` and `TransparentElide` ship as actual optimizations
instead of cosmetic classification:

- **`TapeSpanOnly` emitter path**: single-leaf rules (`@token`,
  punctuation, `Negate`, `OptionalWhitespace`) emit
  `push_leaf(TapeKind::Span, ...)` instead of a compound wrapper
  around their children. View accessors read the single span directly.
- **`TransparentElide` emitter path**: transparent wrappers skip
  the `fn __rule` emission entirely. `compile_ref` inlines the body
  at every call site. The inlined body's own class decides what
  record (if any) gets pushed.
- **Record suppression sweep**: walk every Seq/Alt/Repeat whose
  children are all `TransparentElide` + at most one record-producing
  child — collapse the parent compound to the child's single record.
- **No parser-level direct projection ABI.** Projection remains
  strictly a view-layer concern. If a follow-up tranche wants to
  fuse scalar extraction into the parser body, it does so by
  specializing the view accessor path, not by changing the rule's
  return type.
- **Parity gate extension**: `tape_parity.rs` runs with elision
  enabled and confirms structural equivalence still holds. Parity
  is about view-level observable structure, not raw tape record
  counts — elision is allowed to produce fewer records as long as
  the view layer round-trips correctly.
- **Tests**: `crates/ir/tests/materialization_elision.rs` — fixture
  grammars where elision kicks in, assertions on tape record counts
  before/after, view round-trip checks.

### AB.5 — Cleanup + verification

- Delete the old eager emitter's `Option<Enum>` return shape and
  any residue (`BoxedEnum`, `emit_alloc`, IIFE fallbacks).
- Delete `compute_first_sets` / `compute_follow_sets` /
  `compute_nullable` / `refine_span_eligibility` imperative passes
  if `GrammarAnalysis::EClassFacts` fully covers them.
- Delete `TypeDesc::BoxedEnum` if grep is clean.
- Delete `has_family_recognizers` gate if the new CSP materialization
  decisions subsume it.
- Delete `BumpSlab` from parse-that if the tape's `ChunkedArena` is
  the only remaining arena consumer (commit in parse-that sibling
  repo).
- `OptimizableTier` trait in `crates/egraph/src/lib.rs` for
  cross-tier isomorphism.
- Y.13 consumer-invariant test extended for every new variant:
  `MaterializationClass`, new `EClassFacts` fields, new `TapeKind`
  variants.
- **Post-AB baseline**: fresh samply profiles for every production
  bench (`json_canada`, `json_twitter`, `json_citm`, `json_data_xl`,
  `css_tailwind`, `css_bootstrap`, `css_normalize`, `compile_bbnf`,
  `compile_css_l4`). `docs/benchmarks/post-AB.json` with every
  claimed delta cited from a samply symbol + self-time diff.
  `docs/benchmarks/expand/post-AB/*.rs` cargo-expand snapshots.
- **Floor gates** (relaxed for the tape-only commitment — projection
  optimizations are deferred, so tape-write throughput is the gating
  axis):
  - `json_canada` parse ≥ 1.5 GB/s
  - `json_twitter` parse ≥ 1.7 GB/s
  - `json_citm` parse ≥ 2.0 GB/s
  - `css_tailwind` parse ≥ 0.32 GB/s
  - `css_bootstrap` parse ≥ 0.30 GB/s
  - `compile_bbnf` ≤ 1.5× pre-AB
  - `compile_css_l4` ≤ 2.0× pre-AB
- Parity test green across every grammar.

---

## Critical files (load-bearing, in order of touch)

**AB.0 — analysis lattice:**

1. `crates/ir/src/egraph/analysis/facts.rs` — extend `EClassFacts`
   with 4 new monotone fields + merge logic.
2. `crates/ir/src/egraph/analysis/mod.rs` — extend
   `GrammarAnalysis::make` per-variant.
3. `crates/ir/src/egraph/write_back.rs` — snapshot e-class facts
   into pointer-keyed map during the extraction tree walk.
4. `crates/core/src/pipeline/compile.rs` — after `GrammarDag::from_ir`,
   rekey pointer-keyed snapshot to NodeId-keyed `ir.eclass_facts`.
   Call `classify_materialization(&mut ir)` after `project_types`
   in `finalize_compile`.
5. `crates/ir/src/types/grammar.rs` — add `eclass_facts` and
   `materialization` sidecars (both `#[serde(skip)]`).
6. **NEW**: `crates/ir/src/passes/materialization/{mod,lattice,
   classify,pin_sweep}.rs` — the 3-variant lattice, monotone join,
   `classify_materialization` pass, and consumer-pin fix-up.
7. `crates/ir/src/passes/mod.rs` — re-export `classify_materialization`
   + `MaterializationClass`.
8. **NEW**: `crates/ir/tests/materialization.rs` — lattice tests +
   fixture-grammar classification tests + debug assertion sweep.

**AB.1 — joint CSP solve:**

9. `crates/ir/src/passes/csp_strategy/mod.rs` — extend `Site` +
   `StrategyValue` with `Materialization`; rename
   `solve_strategy_decisions`; swap per-rule loop for per-component
   via UnionFind.
10. `crates/ir/src/passes/csp_strategy/components.rs` — wake the Y.5
    `UnionFind` substrate; add `components_with_variables` helper.
11. **NEW**: `crates/ir/src/passes/csp_strategy/constraints/` — four
    new constraint files (`parent_pins_child.rs`, `prettify_pin.rs`,
    `debug_pin.rs`, `transparent_elide_chain.rs`).
12. `crates/ir/src/cost_config.rs` — add `strategy` sub-config with
    5 new weights and `BBNF_COST_*` env var plumbing.
13. **NEW**: `crates/ir/src/egraph/extraction_bridge/{mod,
    facts_to_soft}.rs` — walks extracted e-nodes, produces soft
    constraints from `EClassFacts::elision_safe`.
14. **NEW**: `crates/ir/tests/csp_materialization.rs` — fixture
    grammars with golden classification outcomes, per-constraint
    regression tests, multi-rule UnionFind component enumeration.

**AB.2 — tape-first emitter baseline:**

15. `crates/core/src/backend/driver/mod.rs` — add `materialization`
    to `DriverState`.
16. `crates/core/src/backend/rust/emitter/grammar.rs` — single
    `(state, tape) -> Option<TapeOffset>` signature; dispatch on
    `materialization[rule]` for prelude/epilogue shape.
17. **NEW**: `crates/core/src/backend/rust/emitter/tape_prelude.rs`
    — `emit_rule_prelude` / `emit_rule_epilogue` helpers.
18. `crates/core/src/backend/rust/emitter/{seq,alt,repeat,reference,
    map_value}.rs` — uniform child call shape (every ref returns
    `Option<TapeOffset>`); `compile_ref` inlines `TransparentElide`
    bodies.
19. `crates/core/src/backend/rust/ir_types.rs` — delete `emit_alloc`
    family; delete `BoxedEnum` arm.
20. `crates/core/src/backend/types/decisions.rs` — delete
    `child_alloc` `BoxedEnum` arm.
21. **NEW**: `crates/core/src/backend/rust/view/{mod,leaves,seq,alt,
    repeat,grammar}.rs` — view-type codegen over `TapeCursor`.
22. `crates/core/src/backend/rust/ir_enums.rs` — every rule variant
    becomes a view type; top-level grammar enum becomes
    `<Grammar>Enum<'tape>`.
23. **NEW**: `crates/core/src/runtime/parsed.rs` (or equivalent) —
    `Parsed<View>` owning parse result type.
24. `crates/derive/src/lib.rs` — generate `parse` returning
    `Parsed<RootView<'_>>`.
25. **NEW**: `crates/core/tests/tape_parity.rs` — golden structural
    parity comparison across production grammars.

**AB.3 — view + consumer migration:**

26. Serializer / schema / debug consumers updated to walk views.
    Mostly localized to `crates/ser/` and `crates/core/src/backend/`.

**AB.4 — elision optimizations:**

27. Emitter paths for `TapeSpanOnly` and `TransparentElide` land as
    per-class branches in the AB.2 emitter helpers (no new files —
    incremental additions to `tape_prelude.rs` + `reference.rs`).
28. `crates/ir/tests/materialization_elision.rs` — elision regression
    fixtures.

**AB.5 — cleanup:**

29. `crates/ir/src/passes/sets/{first_sets,follow}.rs` — DELETE if
    `EClassFacts` covers.
30. `crates/ir/src/types/type_desc.rs` — remove `BoxedEnum` variant
    if grep is clean.
31. `crates/ir/src/types/grammar.rs` — delete `has_family_recognizers`
    if CSP materialization subsumes it.
32. `crates/core/src/backend/patterns/{cache,delim_scan,key_dispatch}.rs`
    — delete the detection halves.
33. `crates/egraph/src/lib.rs` — add `OptimizableTier` trait.

---

## Architectural commitments

1. **Tape is the only runtime output substrate.** Every rule returns
   `Option<TapeOffset>`. No mixed ABI. No scalar/aggregate parser
   return types. Projection is a post-baseline tape-layer optimization.
2. **One parser ABI, one control flow path.** Materialization class
   decides prelude/epilogue shape and transparent inlining at codegen
   time. The generated Rust token stream is single-path per rule.
3. **Owning parse result.** `Parsed<View>` owns the tape inline and
   lends out views. Callers never see a `(View, Tape)` tuple.
4. **No legacy code, no workarounds, no fallback shims.** The one
   transitional shim is AB.2's default-to-MustTape fallback when the
   CSP solver hasn't wired materialization in — deleted the moment
   AB.1 lands.
5. **Truth-based attribution.** Every "+X%" claim in AB.5 cites a
   samply profile symbol + self-time delta from a fresh post-AB
   profile.
6. **MustTape is always a legal target.** Every branch of
   `classify_materialization` that doesn't positively prove a lower
   class falls through to `MustTape`. CSP fallback semantics clamp
   every `Materialization` variable to `{MustTape}`.
7. **Substrate before solve before emit.** AB.0 lands the analysis
   lattice; AB.1 lands the CSP extension; AB.2 lands the emitter.
   No phase consumes from a substrate that hasn't shipped.
8. **Every new variant has a load-bearing consumer in the same
   commit.** Y.13's consumer-invariant test extends to every new
   `MaterializationClass` variant, new `EClassFacts` field, new
   `TapeKind` variant.
9. **DAG invariant preserved.** The durable DAG at
   `crates/core/src/pipeline/compile.rs:431` remains load-bearing.
   NodeId-keyed sidecars depend on it.
10. **Prettify emission is a hard constraint.** `PrettifyPin` in the
    CSP guarantees `@pretty` subtrees are `MustTape`. Prettify emitter
    is unchanged — it walks parser state, not typed AST.
11. **View layer is the only typed surface.** No rule bypasses the
    view layer with a scalar or struct return. Projection
    specialization, when it lands, happens inside the accessor path.

---

## Verification

**AB.0** — `cargo test -p bbnf-ir --test materialization` passes.
`classify_materialization` runs without panics on all 5 production
grammars. Debug assertion sweep passes. `ir.materialization` populated.

**AB.1** — `cargo test -p bbnf-ir --test csp_materialization` passes.
`BBNF_CSP_REPORT=1 cargo bench compile_pipeline` prints per-component
breakdown with zero budget exhaustions. UnionFind decomposition produces
≥2 components on CSS L4.

**AB.2** — `cargo build -p bbnf` clean. `cargo test -p bbnf` green.
`cargo expand -p bbnf --bench json_monolithic 2>&1 | grep
'push_compound'` finds ≥1 `TapeBuilder::push_compound` call in the
generated JSON parser. Every rule function signature returns
`Option<TapeOffset>`. Tape parity test
(`crates/core/tests/tape_parity.rs`) passes on JSON, CSS L4, BBNF,
Sheets, EBNF.

**AB.3** — View types generated for every rule. Serializer + schema
consumers migrated. Public API is `Parsed<RootView<'_>>`. Parity test
walks views and compares against golden structural snapshot.

**AB.4** — Elision enabled. Tape record counts drop on fixture
grammars with transparent wrappers and punctuation tokens. View
round-trip still matches. `materialization_elision.rs` regression
tests pass.

**AB.5** — `cargo clippy --all-targets -- -D warnings` clean.
`grep -rn "BoxedEnum"` returns zero hits. `grep -rn "emit_alloc"`
returns zero hits. Post-AB bench sweep committed. Floor gates met.

End-to-end smoke test:
```bash
cargo test --workspace
BBNF_EGRAPH_REPORT=1 BBNF_PIPELINE_REPORT=1 BBNF_CSP_REPORT=1 \
    cargo bench -p bbnf --bench compile_pipeline 2>&1 | head -200
cargo bench -p bbnf --bench json_monolithic
cargo bench -p bbnf --bench css_l4
```

---

## What this tranche does NOT include

1. **`DirectProjection` parser output** — deferred to post-AB. Parser
   rules never return scalar or struct types in this tranche.
2. **`TapeCompact` bulk encoding** — deferred to post-AB. No side
   arenas, no compact runs.
3. **`(View, Tape)` return tuples** — replaced with `Parsed<View>`
   owning result.
4. **Tape format freeze benchmark harness** — the `TapeRec` 16-byte
   format is the starting point. If post-AB measurement shows
   cache-miss pressure, revise in a follow-up tranche.
5. **Structural bitmap SIMD vectorization** — the tape write is
   scalar. NEON/AVX2 specialization lands in a follow-up.
6. **Perfect-hash dispatch (FCH)** — deferred. The tape-first
   emitter's Alt dispatch path is ready for it.
7. **Cross-tier TapeView export** — TS/WASM backends keep their
   existing shape. The `Emitter` trait gains no new required methods;
   the materialization lookup is Rust-backend-local.
8. **Profile-guided cost calibration feedback loop** — follow-up.
9. **HIR-tier materialization lattice** — the regex HIR e-graph
   keeps `NoAnalysis` (mirrored). HIR-tier projection is orthogonal.
10. **`OptimizableTier` trait** — lands in AB.5 if time permits.

---

## The key insight

> "Tape is not just the law; tape is the only runtime output
> substrate in this tranche. All optimization must preserve that."

The earlier "tape-first hybrid with projection ABI" framing was a
strictly larger surface than what we need to commit to tape working.
By collapsing to a single parser ABI — every rule returns
`Option<TapeOffset>`, every grammar produces a tape, every view reads
from that tape — we get the architectural close of AB.2 as a
genuinely load-bearing milestone rather than a waypoint in a five-class
lattice. Projection still wants to happen; it happens next tranche,
over a stable substrate, as accessor fusion and view-layer scalar
extraction.

The commitment is: **tape will work, and nothing in this tranche
competes with that goal.**
