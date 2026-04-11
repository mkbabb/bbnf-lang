# Tranche AF — Substrate Closure + Three-Tier Emission

## Context

Tranche AE landed the shape-agnostic lowering substrate, consolidated
the schema directive API to one surface, and made rule emission
identity-aware. The lowering pipeline now produces correct IR from any
well-formed tape regardless of which wrappers the optimizer has
elided. The substrate is what AC.2 promised it would be: shape-
agnostic from the tape boundary upward.

What AE deferred is one substrate-break and the optimization tranche
it gates. The lowering still hard-codes
`term_0 / term_1 / term_2 / value_atom_0 / value_unary_0 /
import_directive_0` sub-variant references inherited from the pre-AC.2
era; the new schema's `{rule_name}_{counter}` naming convention does
not produce those names under structural mode (deduplication by type
uniqueness collapses heterogeneous Alt branches when all branches
project to Span). Until those references are eliminated,
`crates/core/src/grammar/generated.rs` remains hand-patched and the
round-trip test gate cannot ship.

The downstream cost is the entire AF optimization tranche. AF's
universal cost model, cross-rule CSP component solve, and three-tier
emission lattice (Tape / Direct-to-struct / Lazy view) all read
morphology facts off `GrammarIR` and are blocked by the AE substrate-
break: variant identity ambiguity corrupts the CSP component graph's
edge topology, and the dormant infrastructure that AF.1 activates
(`EClassFacts::is_fixed_shape`, FIRST-set caching, `TypeDescId`
routing, `compute_context_facts` fusion into the Z.0 recognizer miner)
cannot be load-bearing while the lowering is still leaning on hand-
patched `generated.rs`.

This tranche closes both at once. AF.0 finishes AE's substrate-break
by replacing every hard-coded sub-variant reference with span-text
content dispatch (the same primitive the directive accessors already
use under structural mode), runs a clean end-to-end regen, ships the
round-trip gate, and deletes the working-tree hand patches. The
remaining six phases execute the AF-prototype design end-to-end:
dormant infra activation, universal cost model consolidation, cross-
rule CSP component solve via the dormant Y.5 `UnionFind` substrate,
the three-tier emission substrate, the per-rule decision pass, the
Tier B emitter + view-layer codegen, and the post-AF samply baseline.
AG (modules as first-class compilation units) is sketched as a
forward-looking prototype document, gated on AF landing cleanly.

The architectural premise is that substrate completion compounds.
AE made morphology facts trustworthy. AF makes them load-bearing.
AG makes them cacheable per-module. Each tranche removes one class of
"this can't be true yet" and lets the next layer be written without
conditionals.

## AF.0 — Substrate-break: span-text dispatch eliminates sub-variant refs

Every reference to `term_0 / term_1 / term_2 / value_atom_0 /
value_unary_0 / import_directive_0` is removed from
`crates/core/src/lower/`. A clean `scripts/bootstrap-bbnf.sh` produces
a `generated.rs` the unmodified lowering pipeline accepts. The hand
patches in `crates/core/src/grammar/generated.rs` are deleted. The
round-trip gate ships.

The lowering already has a working leaf classifier
(`lower_leaf_by_span_text`) that the directive extraction path uses
to handle `@debug` / `@ws` / `@token` / `@host` placeholder compounds.
Extend the same primitive to classify the leaves of `term`,
`value_atom`, and `value_unary` under the same content-dispatch
discipline. Grouped forms (`(`, `[`, `{`, `@{`) dispatch on leading
byte; bare leaves dispatch on the full span text via the existing
identifier / literal / regex classifier paths.

`lower_term_dispatch` walks `view.children()`, classifies the first
child by leading byte, and hands the body off to the right handler:
`(` → grouped expression, `[` → character class, `{` / `@{` → host
call, otherwise → leaf classifier. `lower_factor` uses by-elimination
child-finding via `find_child_by_kind`: known wrappers peel through
`peel_transparent`, the term core is identified by its `rule_kind`,
modifiers (`*`, `+`, `?`, `?w`) are picked off individually. No
positional reads. `lower_value_atom` extends the same leading-byte
dispatch into `dispatch_value_expr` so the `value_atom_0 /
value_unary_0` references collapse into the central matcher.
`lower_import_directive` in `host.rs` walks children by `rule_kind`,
finding `import_items` and `import_path` structurally; the
`absorb_import_by_text` span-text fallback is deleted.

A grep gate at `crates/core/tests/no_subvariant_refs.rs` fails the
build if any string of the form `BbnfBootstrapRuleKind::(term_[0-9]
| value_(atom|unary)_[0-9] | import_directive_[0-9])` appears
anywhere under `crates/core/src/lower/` or
`crates/core/src/grammar/host.rs`. The gate is the contract that says
the substrate-break is closed.

The grammar round-trip gate at `crates/core/tests/grammar_roundtrip.rs`
parses every production grammar (`bbnf.bbnf`, `json.bbnf`,
`css/pretty.bbnf`, `google_sheets.bbnf`, `ebnf.bbnf`) and asserts rule
counts match a frozen snapshot. Permanent regression gate against
silent shape-mismatch failure of the kind AE was built to fix.

### AF.0b — Deferred: pipeline codegen substrate-break

Wave 1's substrate-break closure lands as planned: every `term_0 /
term_1 / term_2 / value_atom_0 / value_unary_0 / import_directive_0`
reference is gone from `crates/core/src/lower/` and `crates/core/src/grammar/host.rs`.
The grep gate (`crates/core/tests/no_subvariant_refs.rs`) is the
contract that keeps it closed.

The clean regen itself — `scripts/bootstrap-bbnf.sh` producing a
`generated.rs` that replaces the hand-patched file — is deferred to a
follow-up sub-tranche **AF.0b**. A latent bug in the backend driver
codegen emits a degenerate `__grammar` body (three empty `'alt_blk:`
branches instead of the expected `'rpt_blk:` loop) when lowering the
`grammar = ( grammar_item ?w ) *` rule under structural mode. The
bug reproduces with every Wave 1 change reverted, so it pre-dates the
tranche; the hand-patched `generated.rs` has been silently masking it.
Investigating the codegen regression is unbounded work that would
block the rest of AF, so it is isolated into AF.0b.

Until AF.0b lands, `crates/core/src/grammar/generated.rs` remains
hand-patched and the grammar round-trip gate stays `#[ignore]`-gated.
The substrate-break closure (Wave 1) and all downstream AF phases
(AF.1–AF.7) run cleanly against the hand-patched parser: Wave 1's
span-text discipline in `lower/` is enum-drift-tolerant by design,
and an additional span-text fallback in `host.rs::absorb_import_structural`
(primary path: structural `find_descendant_by_kind(import_path)`;
fallback path: byte-offset extraction from the directive's source
slice) makes `load_module_graph` follow `@import` directives correctly
regardless of whether the runtime-stamped variant_idx matches the
compiled enum positions. With that fallback in place, the hand-
patched parser loads all 53 rules across `bbnf.bbnf`, `expressions.bbnf`,
and `types.bbnf`.

## AF.1 — Dormant infrastructure activation

Every piece of pre-built infrastructure that AC–AE landed without a
consumer is wired into the live pipeline. The compiler stops doing
work twice. The cost model and the IR passes start reading from the
same fact lattice.

**`EClassFacts::is_fixed_shape`** in
`crates/ir/src/egraph/analysis/facts.rs` is computed but never read.
AF.1 wires it into `classify_materialization` as the pre-seed for
Tier B eligibility: a rule with
`is_fixed_shape && elision_safe && closure_free &&
all_descendants_elidable` is a Tier B candidate before any cost solve
runs.

**FIRST-set double walk** in dispatch generation
(`crates/core/src/backend/driver/alt.rs` re-walks first sets the IR
`compute_first_sets` pass already produced) consolidates into the
existing `annotate.rs` walk. The dispatch generator reads the cached
values.

**`compute_context_facts`** (the separate pass at
`crates/ir/src/passes/context_facts.rs`) folds into Z.0's
`RecognizerMiner` trait substrate as a third miner impl. One IR walk,
three fact-producing miners, no separate full-DAG traversal.

**`TypeDescInterner` cloning** in codegen
(`backend/driver/{alt,seq,repeat,map}.rs` clones full `TypeDesc`
structs instead of routing through `TypeDescId`) becomes `TypeDescId`
lookups. The interner is the source of truth for type identity in
codegen hot loops.

**`NodeFacts` sidecar** consumers in the backend kernel layer
(`crates/core/src/backend/kernels/`) read directly from
`ir.node_facts` instead of recomputing per-emission.

## AF.2 — Universal cost model

One `CostWeights` struct, one source of truth, every cost computation
reads from it. The grammar tier, HIR tier, CSP solver, and backend
driver all share the same weights.

`crates/egraph/src/cost_weights.rs` already defines `CostWeights` as
the shared substrate; both `GrammarCostModel` and
`RegexExtractionCost` embed the struct. The gap is the consumers: the
CSP solver reads from a separate `StrategyCostKnobs` struct that AC.2
introduced as a stopgap, and the backend driver reads hardcoded
constants for inline-vs-call decisions. Both consume the same
dimensions — call overhead, allocation cost, dispatch cost, tape-push
cost — and should consult the same weights.

AF.2 extends `CostWeights` with the missing dimensions:
`call_overhead`, `inline_body_size_penalty`, `tape_push`, `slab_alloc`,
`dispatch_branch`, `dispatch_table`, `prettify_emission`,
`cross_module_coercion`. Defaults match current hardcoded values;
behavior is unchanged at the dimension level. `StrategyCostKnobs` is
deleted; every call site reads `ir.cost_weights` directly. The driver
reads `ir.cost_weights` for inline-vs-call cost decisions; hardcoded
constants are deleted. The `cross_module_coercion` weight (one
`push_compound` + one typed projection cost) is pre-seed for AG's
module substrate, so AG can consume it without needing a fresh
dimension addition.

A `cargo test -p bbnf-ir --test cost_weights_unified` gate patches the
weights to extreme values and asserts every consumer changes its
decision proportionally. The test is the contract that says the cost
model is the single source of truth.

## AF.3 — Cross-rule CSP via Y.5 UnionFind

The per-rule CSP solver becomes a per-component CSP solver. The Y.5
`UnionFind` substrate at
`crates/ir/src/passes/csp_strategy/components.rs`, dormant since
Tranche Y.5 landed it without a consumer, becomes the topology that
the strategy solve walks.

`solve_strategy_and_materialization` is renamed to
`solve_grammar_components`. The function walks the rule call graph,
partitions rules into connected components via `UnionFind`, and solves
each component as a single CSP unit. Three cross-rule constraints
become first-class. `EnginePropagation`: regex engine choice is global
per component, not per-rule; a component containing one DFA-eligible
rule promotes the whole component if the cost weights say it's worth
it. `ParentCompatibility`: a parent rule's tier choice is constrained
by its children's tier choices — a Tape parent calling a Direct child
pays a coercion cost; a Direct parent calling a Tape child cannot
exist without an upgrade pass. `TierFollowsMaterialization`: a rule's
emission tier is constrained by its materialization class
(`MustTape` → Tier A; `TransparentElide` → Tier B candidate;
`TapeSpanOnly` → either, cost-decided).

The CSP solver substrate itself is unchanged — `csp-solver` already
handles per-component solves cleanly. AF.3 is wiring, not substrate
work.

## AF.4 — Three-tier emission substrate

The emission lattice gains an axis orthogonal to
`MaterializationClass`. Where materialization decides what record
shape a rule pushes, the emission tier decides what return type the
rule's parse function projects to.

**Tier A — Tape only.** AE default. Universal, always legal.

**Tier B — Direct-to-struct projection.** Leaf rules with typed `->`
bodies emit a second function `__rule_direct(state) -> Option<T>`
alongside the tape shim. Both share the same prelude parsing logic via
a private `__rule_inner` helper. Eligibility: `FixedShape`, pure
conversion bodies, single-site consumer model.

**Tier C — Lazy view-layer projection.** Universal, sits above Tier A.
The view layer dispatches between tape walk and direct slot on demand.

`EmissionTier` is a CSP variable, constrained by the AF.3 constraints.
The solver writes the per-rule decision to `ir.emission_tier`. The
type lives in `crates/ir/src/types/grammar.rs`; the sidecar lives on
`GrammarIR`. `EmissionTier` is read-only after AF.5; AF.4 lands the
type and the sidecar but does not write to it.

## AF.5 — Per-rule tier decoder

A single pass reads the AF.3 component CSP output and writes
`ir.emission_tier` for every rule. After AF.5, the backend driver has
a complete per-rule tier decision and never makes its own.

`decode_emission_tier` at
`crates/ir/src/passes/csp_strategy/decode_tier.rs` walks the solved
CSP, picks the optimal tier per rule from the strategy assignment, and
writes `ir.emission_tier`. The pass runs in
`crates/core/src/pipeline/compile.rs` between
`solve_grammar_components` and `project_types`. Order is invariant:
`project_types` reads tier to decide whether to emit a `__rule_direct`
shim alongside the tape function.

Test gate asserts every rule in `bbnf.bbnf` gets a tier assignment;
`@pretty`-pinned rules are always Tape; typed leaves with
`FnDescriptor::NumberConvert / HexConvert / Constant` are always
Direct under cost model defaults; ambiguous rules decode
deterministically.

## AF.6 — Tier B emitter + view-layer codegen

Every Tier B rule emits a second function alongside the tape shim.
The view layer dispatches between tape walk and direct slot on demand.
Tier B becomes load-bearing for the hot path of every JSON / CSS /
Google Sheets parse.

`emit_rule_function_impl` reads `ir.emission_tier[rule.id]` and emits
Tier A unchanged; Tier B emits the existing tape function plus
`fn __rule_direct(state) -> Option<T>` that runs the same prelude
(extracted to a private `fn __rule_inner(state) -> Option<T>` helper)
and returns the typed value directly. Tier C adds the view layer
extension.

`DirectSlot<'p>` is a new enum in the view layer, parameterized by the
rule's projected type. The view's accessor methods dispatch on the
slot variant: `Tape(cursor)` walks the tape; `Direct(value)` returns
the cached value without walking. `<Rule>View<'p>` gains a
`direct: DirectSlot<'p>` field; the view constructor reads
`ir.emission_tier[rule.id]` to populate it.

`emit_call` consumes the per-call-site tier decision. A Tape parent
calling a Direct child wraps the direct call in a synthetic tape push
at the call site (per the `cross_module_coercion` cost weight from
AF.2). A Direct parent calling a Direct child calls directly.

TS and WASM backends are untouched. They are Tier A everywhere by
design — the view layer projection is Rust-specific. The `Emitter`
trait gains a `tier()` accessor that defaults to `EmissionTier::Tape`
for non-Rust backends.

## AF.7 — Default-on + post-AF baseline

Tier B is the default for typed leaves. The post-AF samply baseline
lands. Floor gates against the post-Z.7 baseline are enforced.

`cargo bench -p bbnf` runs the full JSON / CSS L4 / Google Sheets
bench suite in a single invocation. Profiles are captured to
`docs/benchmarks/profiles/post-AF/*.samply` via interactive
`samply record`. `docs/benchmarks/post-AF.json` captures the new
numbers.

Floor gates against post-Z.7 baseline: `json_canada` ≥ 1.7 GB/s;
`json_twitter` ≥ 1.6 GB/s; `json_citm` ≥ 2.0 GB/s;
`css_l4_tailwind` ≥ 280 MB/s (target: beat lightningcss);
`css_l4_bootstrap` ≥ 270 MB/s; `compile_bbnf` ≤ 1.05x post-Z.7
(compile-time non-regression budget).

## Architectural commitments

1. **Substrate-break is closed.** Zero hard-coded sub-variant
   references in `crates/core/src/lower/`. The grep gate at
   `crates/core/tests/no_subvariant_refs.rs` is the contract.
   `generated.rs` is reproducible from `scripts/bootstrap-bbnf.sh`
   with no hand patches.

2. **One cost model.** Every cost computation in the compiler reads
   from `ir.cost_weights`. `cost_weights_unified.rs` patches the
   weights to extremes and asserts every consumer responds
   proportionally. `StrategyCostKnobs` is deleted.

3. **CSP solves are component-scoped.** The Y.5 `UnionFind` substrate
   is the live topology of every cross-rule CSP. Cross-rule
   constraints (`EnginePropagation`, `ParentCompatibility`,
   `TierFollowsMaterialization`) are first-class.

4. **Three tiers, one decision pass.** `EmissionTier` is a CSP
   variable. `decode_emission_tier` is the only writer; every
   downstream consumer is a reader. No backend code re-decides the
   tier locally.

5. **Tier B is the default for typed leaves.** Rules with
   `FnDescriptor::NumberConvert / HexConvert / Constant` and
   `FixedShape` materialization decode to Tier B unless the cost
   model rejects them. The hot path of every production grammar's
   element loop is Tier B.

6. **Universal cost model is the join op.** The
   `cross_module_coercion` weight is pre-seeded for AG so the module
   substrate consumes the same lattice. AG adds zero new cost
   dimensions; it adds one constraint and one solver edge type.

7. **No legacy code.** Every workaround introduced during AE
   (the structural-mode strategy clamp, the directive walker stubs,
   the dead schema-emit modules, the inline planner's missing self-
   cycle guard, the hand patches in `generated.rs`, the absorb-by-
   text directive fallbacks) is gone. Every cost knob struct, every
   dispatch FIRST-set re-walk, every `TypeDesc` clone in codegen is
   gone. The tree contains only the AF substrate.

## What this tranche does NOT do

AG is the consumer of AF. AG treats `@import` as a first-class
compilation boundary, adds a `ModuleIR` substrate, caches per-module
IR on disk keyed by transitive fingerprint, and turns compilation of
a parent grammar into a link step. AG is gated on AF landing cleanly,
including the post-AF samply baseline. The AG design document lands
as `docs/tranches/AG-prototype.md` alongside the AF.7 post-landing
narrative.
