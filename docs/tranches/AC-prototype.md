# Tranche AC — Gestalt -O3 Pass: Substrate Activation, Parse-Time Cliff Kills, Joint Optimization

## Context

Tranches W → X → Y → Z burned nine months on architectural cleanup (four-layer
separation, ghost-variant elimination, miner consolidation, cost-knob
unification) and landed exactly **one** material parse-time win:
`json_canada +3.4 %` from Z.2's SWAR fractional fix. Seven of Z's nine phases
were deferred or pivoted to deletions. The AA and AB prototype tranches
attempted to pivot to substrate work (e-class Analysis, TypeDesc interning,
structural bitmap) but were never executed.

A six-agent deep audit (3 Explore + 3 Plan) landed this tranche on a single
converging thesis:

**The four-layer optimizer is architecturally correct but half its load-bearing
substrate is dormant.** The `Analysis<N>` trait at `crates/egraph/src/analysis.rs`
is fully implemented and instantiated as `NoAnalysis` in both tiers. The
`components::UnionFind` cross-rule substrate at
`crates/ir/src/passes/csp_strategy/components.rs` has zero producers.
`LatticeDomain<TypeDomain>` clones `Option<TypeDesc>` per AC-3 merge. The
pipeline has zero feedback between e-graph extraction and strategy CSP.
`compute_best` is an O(classes²) fixed-point loop. `AccelStrategy::ScalarLut`
is defined with zero emitter consumers. BumpSlab scratch does **two** copies
per repeat (Vec → slab + truncate) with `RawVecInner::grow_amortized` in the
hot stack. Sonic-rs's primary architectural advantage (structural bitmap
pre-scan + tape-walking dispatch) has no bbnf analogue.

**Three agents independently landed on the same gestalt move**: install the
dormant substrate so the four-layer optimizer can deliver the parse-time wins
its architecture has always promised, then wire extraction ↔ CSP feedback via
csp-solver's already-landed `SoftLambdaConstraint` (verified at
`/Users/mkbabb/Programming/csc411/CSC411_HW2_ProgrammingQuestion/csp-solver/src/constraint/soft.rs`
and consumed in the B&B lower bound at `solver/optimize.rs:153-199`).

The audit also surfaced **three ground-truth corrections** that change prior
framings:

1. **`__namedColor` is not a 230-way linear chain.** cargo-expand shows a
   7-way first-byte match with nested length + `unsafe_memcmp` checks per arm
   (real depth ~12-15 ifs). Perfect-hash dispatch still wins (single hash
   probe beats nested compares) but the framing is "replace nested memcmps",
   not "replace 230 linear branches".
2. **IIFE closures in CSS L4 are `?` / `return` containment, not borrow
   scoping.** The fix is Rust labeled blocks (`'blk: { ... break 'blk None }`),
   not codegen scope restructuring. LLVM inlines closures ~99 % of the time
   but punts when the closure captures `&mut state` with 4-8 control-flow
   joins — CSS L4 hits this regularly.
3. **`AccelStrategy::ScalarLut` is dead code** at `parse-that/rust/regex/src/automata/accel.rs:29`.
   The 9-64 exit-byte range (e.g. CSS ident's 52-byte continuation set)
   currently falls through to scalar `scan_ident`. The opportunity is one
   shared SIMD class-membership kernel routed into both scan helpers and the
   accel machinery.

Tranche AC is the single -O3 pass that lands all the substrate + all the
derived wins. Compile time may degrade ≤ 15 % aggregate if it unlocks ≥ 8 %
parse-time on a target bench. Every phase ships clean: no legacy shims, no
fallback branches, no ghost variants. Y.13 consumer-invariant test extends
for every new variant/substrate before its introducing commit.

This is maximal ambition. Architectural transpositions for elegance,
simplicity, and performance are welcomed. The user's north star: **beat
sonic-rs's ceiling on non-JSON grammars** (where bbnf's compile-time
specialization is the unique edge) and **close 60–75 % of the gap on JSON**
(where sonic-rs's JSON-specific tuning sets a hard ceiling for
grammar-agnostic Rust compilers).

---

## Profile baseline (post-Z, M-series aarch64)

From `docs/benchmarks/post-Z.json` + existing profiles at
`docs/benchmarks/profiles/post-Z/`:

| Bench           | ns/iter     | MB/s  | Dominant cost                                           |
|-----------------|-------------|-------|---------------------------------------------------------|
| `json_canada`   | 1,827,752   | 1231  | `__value` alt + `RawVecInner::grow_amortized` + per-pair box |
| `json_twitter`  | 416,159     | 1517  | `__value` dispatch + per-pair slab alloc                |
| `json_citm`     | 910,715     | 1896  | Already competitive                                      |
| `json_data_xl`  | 20,620,595  | 1032  | Slab grow stalls                                         |
| `css_tailwind`  | 14,180,245  | **256**  | 153 slab sites + ~1185 IIFE closures + `scan_ident` 13 % + `scan_ws_block_comments` 12 % + `[u8]::eq` 9 % + `__namedColor` 3.5 % |
| `css_bootstrap` | 1,151,750   | 243   | Same shape as tailwind, smaller                          |
| `css_normalize` | 12,310      | 498   | Smaller grammar, less dispatch                           |
| `compile_css_l4`| 7,245,710   | –     | `alloc::System::alloc` + `hashbrown::reserve_rehash_inner` 1.44 % + `egraph::build_and_saturate` + `csp_solver::bb_recurse` + `project_types` |
| `compile_bbnf`  | 949,435     | –     | Same shape, scaled down                                  |

**The css_tailwind / json_canada 5× cliff is the primary parse-time target.**
`compile_css_l4` at 7.2 ms is the primary compile-time target.

**Profile-based attribution rule**: Every "+X %" claim in AC phases must cite
a samply symbol + self-time delta against
`docs/benchmarks/profiles/post-Z/{compile_css_l4,compile_bbnf,json_canada,json_twitter,css_tailwind}.samply`.
Phases that claim wins without a profile delta are re-opened.

**Profiling in the planning phase**: the three Explore agents did not record
new samply profiles. All claims rest on the existing post-Z profiles + cargo
expand inspection + workspace grep. Phase AC.0 lands observability
instrumentation as the first commit of the tranche; every subsequent phase
lands with its own profile capture before declaring its gate met.

---

## Architectural commitments

1. **No legacy code, no workarounds, no fallback shims.** Every phase lands
   clean. No `#[deprecated]` aliases, no transitional branches.
2. **No ghost substrate.** Every new type / variant / CSP variable / analysis
   fact has a load-bearing consumer in the same commit. Extend
   `crates/core/tests/recognizer_decision_consumption.rs` (Y.13 test) for
   every new variant. A new `every_egraph_analysis_fact_has_a_consumer` test
   lands as part of AC.2 to guard `EClassFacts`.
3. **Cross-tier symmetry.** Grammar and HIR tiers land mirrored substrate
   changes in the same commit. `NoAnalysis` must vanish from both tiers
   simultaneously (AC.2).
4. **Four-layer boundary preserved.** E-graph / facts / CSP / backend.
   Legality lives in facts. Choice lives in CSP. Emission lives in backend.
   Cost knobs live in `egraph::CostConfig`.
5. **DAG invariant preserved.** The durable DAG built once at
   `crates/core/src/pipeline/compile.rs:430` stays load-bearing. Any
   substrate that needs pre-DAG state (e.g. e-class-to-NodeId mapping) uses
   the existing `egraph_class_for` accessor, not a parallel structure.
6. **Compile time may degrade ≤ 10 % per phase, ≤ 15 % aggregate** if it
   unlocks ≥ 5 % parse-time on at least one bench. Aggregate parse-time gains
   must exceed aggregate compile-time regressions on the five tracked benches.
7. **Directory modules for splits**, not flat siblings. No inline tests —
   all tests in `crates/*/tests/`.
8. **Truth-based attribution.** Every "+X %" claim cites a samply profile
   symbol + self-time delta. No inherited numbers from prior audits.

---

## Phase dependency DAG

```
                        ┌── AC.0 Observability (prelude) ───┐
                        │                                    │
                        ▼                                    ▼
                  AC.1 TypeDescId           AC.2 Analysis<N> (grammar + HIR mirror)
                        │                                    │
                        └──────────┬─────────────────────────┘
                                   │
                                   ▼
                        AC.3 Unified CostConfig
                                   │
        ┌──────────────────────────┼──────────────────────────┐
        ▼                          ▼                          ▼
 AC.4 TopoExtractor          AC.5 Extraction→CSP    AC.7 TaggedUnion
 + Pareto lattice trait      soft-constraint bridge  (BoxedEnum killer)
        │                          │                          │
        │                          ▼                          │
        │                 AC.6 Cross-rule dispatch             │
        │                 share (Y.5 UnionFind producer)       │
        │                          │                          │
        │                          ▼                          │
        │                 AC.13 RefMode CSP lift               │
        │                 AC.14 RepeatMode CSP lift            │
        │                                                     │
        │  ┌──────────────────────────────────────────────────┘
        │  │
        │  ▼
        │  AC.11 Structural bitmap (uses AC.2 first_set + AC.7 inline)
        │
        ▼
 AC.8 IIFE → labeled blocks    (parallel, no deps)
 AC.9 Direct-to-slab scratch   (AC.7 smaller T helps but not required)
 AC.10 ClassMask SIMD 9-64     (parallel, no deps)
 AC.12 Perfect-hash dispatch   (AC.7 for inline discriminants)
 AC.15 Incremental compile cache (AC.6 for group version)
 AC.16 Profile-guided infra    (AC.5 shares bridge code path)
 AC.17 Post-AC verification    (all phases)
```

Five tracks: **Substrate** (0, 1, 2, 3), **Joint optimization**
(4, 5, 6, 13, 14), **Parse cliffs** (7, 8, 9, 10, 11, 12),
**Compile-time** (15, 16), **Verification** (17).

---

## Phase AC.0 — Observability prelude (prerequisite, free)

**Motivation.** Every subsequent phase needs to cite samply symbol deltas and
per-pass timings. Today `BBNF_EGRAPH_REPORT=1` prints a saturation summary but
not per-rule fire counts; CSP budget exhaustions are silent; per-pass wall
clock in `pipeline/compile.rs` is unmeasured.

**Files (modified).**
- `crates/egraph/src/scheduler.rs`, `crates/egraph/src/csp_scheduler.rs` —
  per-rule `applied` counter on `RunReport`.
- `crates/ir/src/egraph/mod.rs:99-113` — extend `BBNF_EGRAPH_REPORT=1` printer
  with per-rule fire counts.
- `parse-that/rust/regex/src/egraph/mod.rs` — mirror the extension;
  `BBNF_HIR_EGRAPH_REPORT=1`.
- `crates/ir/src/passes/csp_strategy/mod.rs` — `BBNF_CSP_REPORT=1` logs
  every component that hits the Y.-1 budget.
- `crates/core/src/pipeline/compile.rs` — wrap each of the 16 pipeline ops
  + e-graph saturation + type CSP in a timed scope; `BBNF_PIPELINE_REPORT=1`
  emits a CSV row per compile.

**Gates.**
- `BBNF_EGRAPH_REPORT=1 cargo bench -p bbnf --bench compile_pipeline 2>&1 | grep rule=`
  prints non-zero per-rule fire counts for both tiers.
- `BBNF_PIPELINE_REPORT=1 cargo bench -p bbnf --bench compile_pipeline 2>&1 | grep compile_css_l4`
  prints a 16+-row CSV.

**Impact.** Zero runtime. Pure observability. Enables all downstream attribution.

**Risk.** 1/5.

**Dependency.** None.

---

## Phase AC.1 — `TypeDescId` interning + `LatticeDomain<TypeDescIdDomain>`

**Motivation.** `LatticeDomain<TypeDomain>::join` at
`crates/ir/src/passes/types/constraint/domain.rs:66-80` clones
`Option<TypeDesc>` per AC-3 merge. Audit measures 12–18 % of workspace clone
pressure concentrated here. Recursive `TypeDesc::Vec(Box<_>)` +
`TypeDesc::Tuple(Vec<_>)` make these clones deep. Interning collapses join to
a `u32` compare — `Copy` domain, zero allocation.

This phase is also the prerequisite for AC.6's cross-rule dispatch signatures
(branch type equality as reference equality) and AC.7's `TaggedUnion(Id)`
variant.

**Files (new).**
- `crates/ir/src/types/type_desc/mod.rs` — directory module replacing
  `type_desc.rs`.
- `crates/ir/src/types/type_desc/id.rs` — `TypeDescId(u32)` + `TypeDescInterner`
  (`Vec<TypeDesc>` + `FxHashMap<TypeDesc, TypeDescId>`).
- `crates/ir/src/types/type_desc/lattice.rs` — `TypeDescIdDomain` (`Copy`).
- `crates/ir/tests/type_desc_interner.rs` — round-trip + monotone join.

**Files (modified, same commit — no partial migration).**
- `crates/ir/src/types/type_desc.rs` — DELETE.
- `crates/ir/src/types/grammar.rs` — `GrammarIR::type_desc_interner: TypeDescInterner`
  field; per-rule `types: Vec<TypeDescId>`; MessagePack serialization updated.
- `crates/ir/src/passes/types/constraint/{alt,seq,grounds,operators,helpers}.rs` —
  every `.solved: Option<TypeDesc>` → `Option<TypeDescId>`; owned pass-level
  interner drained into `ir.type_desc_interner` on completion.
- `crates/ir/src/passes/types/generate.rs` — ground constraints call
  `interner.intern(td)`.
- `crates/ir/src/passes/types/utils.rs` — `TypeMap` keys `NodeId → TypeDescId`.
- `crates/ir/src/passes/types/subvariants.rs` — uniqueness check operates on
  `TypeDescId` (reference equality = structural equality for free).
- `crates/core/src/backend/driver/{mod,seq,repeat,alt,wrap}.rs` — `fn resolve_type(&self, id: TypeDescId) -> &TypeDesc`
  on `DriverState`; borrow-returns replace the 5 Y.10-missed clone sites.
- `crates/core/src/backend/rust/ir_types.rs`, `backend/rust/emitter/*.rs`,
  `backend/ts/helpers.rs`, `backend/wasm/emitter/*.rs` — route every direct
  `TypeDesc` access through `resolve_type`.
- `crates/core/src/generate/serialize/serialize.rs:17-19` — reference-type
  classification via `resolve_type`.

**Approach.** Idiomatic hash-cons interner:

```rust
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TypeDescId(u32);

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct TypeDescInterner {
    storage: Vec<TypeDesc>,
    lookup: FxHashMap<TypeDesc, TypeDescId>,
}

impl TypeDescInterner {
    pub fn intern(&mut self, td: TypeDesc) -> TypeDescId { /* standard */ }
    pub fn resolve(&self, id: TypeDescId) -> &TypeDesc { &self.storage[id.0 as usize] }
}
```

```rust
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TypeDescIdDomain { pub solved: Option<TypeDescId> }

impl LatticeDomain for TypeDescIdDomain {
    fn bottom() -> Self { Self { solved: None } }
    fn join(&mut self, other: &Self) -> bool {
        match (self.solved, other.solved) {
            (None, Some(id)) => { self.solved = Some(id); true }
            (Some(a), Some(b)) if a != b => { self.solved = Some(b); true }
            _ => false,
        }
    }
}
```

**Consumer invariant extension.** Y.13 extension: new
`fn type_desc_id_consumer(id: TypeDescId) -> &'static str` exhaustive match.
Grep gate: `grep -rn "Option<TypeDesc>" crates/ir/src/passes/types/constraint/`
returns zero hits after migration.

**Profile-measured impact target.**
- `compile_css_l4` `project_types` self-time: ≥ −5 to −8 %
  (the `TypeDomain::join` + `Option::clone<TypeDesc>` cumulative frames).
- `compile_bbnf`: ≥ −3 to −5 %.
- Workspace build time: ≤ +1 % (mechanical expansion).

**Risk.** 4/5. Migration touches 340 combined references across ~40 files.
Partial migrations leave mixed `TypeDesc` / `TypeDescId` worlds forbidden by
the architectural commitments. Mitigation: single-PR migration, land with
the new test fixture exercising every backend's `resolve_type` path. MessagePack
serialization updated for the WASM boundary in the same commit.

**Dependency.** None (keystone).

---

## Phase AC.2 — Activate `Analysis<GrammarENode>` + `Analysis<HirENode>` mirror

**Motivation.** The single largest architectural gap in the optimizer. Every
rewrite rule (`DeduplicateAltBranches`, `SupersetAbsorbAlt`, `UnionMergeAlt`,
`FuseAltRegexBranches`, HIR-tier equivalents) currently re-derives FIRST sets,
nullable, byte ranges via `class.iter().find_map(...)` subtree walks at search
time. With `Analysis<N>`, facts attach once and propagate monotonically via
`merge` when classes unify. The trait is fully implemented at
`crates/egraph/src/analysis.rs` (38 lines, production-ready) and unused.

This phase is the unlock for AC.5 (extraction→CSP bridge reads analysis
facts), AC.6 (cross-rule dispatch signature queries `first_set`), AC.11
(structural bitmap miner queries `first_set`), AC.12 (perfect-hash dispatch
queries `cardinality`), and distributive / common-suffix rewrites deferred
since Y.

**Files (new, grammar tier).**
- `crates/ir/src/egraph/analysis/mod.rs` — `GrammarAnalysis` struct +
  `impl Analysis<GrammarENode>`.
- `crates/ir/src/egraph/analysis/facts.rs` — `EClassFacts` lattice struct.
- `crates/ir/src/egraph/analysis/merge.rs` — monotone join helpers
  (`width_meet`, `first_set_union`, etc.).
- `crates/ir/tests/egraph_analysis.rs` — round-trip + monotone idempotence.

**Files (new, HIR tier mirror).**
- `parse-that/rust/regex/src/egraph/analysis/mod.rs` — `HirAnalysis`.
- `parse-that/rust/regex/src/egraph/analysis/facts.rs` — `HirEClassFacts`.
- `parse-that/rust/regex/tests/egraph_analysis.rs`.

**Files (modified).**
- `crates/egraph/src/egraph.rs` — add `pub fn strings(&self) -> Option<&SharedStrings>`
  accessor so `Analysis::make` can resolve `StringId → &str` inside the grammar
  tier (the HIR tier stores bytes inline).
- `crates/ir/src/egraph/mod.rs:53-97` — flip `EGraph<GrammarENode, NoAnalysis>`
  → `EGraph<GrammarENode, GrammarAnalysis>`.
- `crates/ir/src/egraph/rules/regex.rs` — replace `class.iter().find_map(…Regex…)`
  groveling in `DeduplicateAltBranches::search`, `SupersetAbsorbAlt::search`,
  `UnionMergeAlt::search`, `FuseAltRegexBranches::search` with
  `egraph.class(id).data.regex_sid` / `.first_set` lookups (3-5× search throughput).
- `parse-that/rust/regex/src/egraph/mod.rs:56` — `NoAnalysis` → `HirAnalysis`.
- `parse-that/rust/regex/src/egraph/rules/*.rs` — mirror port.

**Approach.**

```rust
// crates/ir/src/egraph/analysis/facts.rs
#[derive(Clone, Debug, PartialEq)]
pub struct EClassFacts {
    pub first_set: CharSet128,        // non-nullable first-byte set
    pub nullable: bool,
    pub width: WidthBound,             // (min_bytes, Option<max_bytes>)
    pub anchored_left: bool,
    pub dispatch_eligible: bool,       // derived: first_set disjoint across children
    pub regex_sid: Option<StringId>,   // fast-path rule lookup
    pub literal_sid: Option<StringId>,
    pub cardinality: Option<u32>,      // number of distinct shapes in Alt
    pub structural_class_id: u32,      // canonical hash for cross-rule sig
}

impl Default for EClassFacts { /* bottom = empty first_set, nullable=true, width=(0, Some(0)) */ }

impl EClassFacts {
    /// monotone lattice join, returning whether `self` changed.
    pub fn merge(&mut self, other: &Self) -> bool { ... }
}
```

```rust
// crates/ir/src/egraph/analysis/mod.rs
pub struct GrammarAnalysis;

impl Analysis<GrammarENode> for GrammarAnalysis {
    type Data = EClassFacts;

    fn make(eg: &EGraph<GrammarENode, Self>, n: &GrammarENode) -> Self::Data {
        match n {
            GrammarENode::Literal(sid) => literal_facts(*sid, eg.strings()),
            GrammarENode::Regex(sid)   => regex_facts(*sid, eg.strings()),
            GrammarENode::Seq(ids)     => seq_facts(ids, eg),
            GrammarENode::Alt(ids, d)  => alt_facts(ids, d, eg),
            GrammarENode::Repeat { inner, lo, hi } => repeat_facts(eg.class(*inner).data.clone(), *lo, *hi),
            /* Ref, Skip, Next, Minus, Negate, Map, TokenDispatch, OptionalWhitespace, Epsilon */
        }
    }

    fn merge(a: &mut Self::Data, b: Self::Data) -> bool { a.merge(&b) }
}
```

HIR mirror uses `ByteSet256` (all 256 bytes, not ASCII-only) and adds
`is_literal_run: bool` and `is_anchored: bool`.

**Consumer invariant extension.** New test
`crates/ir/tests/egraph_analysis.rs::every_fact_has_a_consumer` — uses
`strum::IntoEnumIterator` or a custom exhaustive match to assert every field
in `EClassFacts` has at least one reader in `crates/ir/src/egraph/rules/` or
`crates/ir/src/passes/`. Grep gate: `grep -rn "NoAnalysis" crates/ir/src/egraph/ parse-that/rust/regex/src/egraph/`
returns zero hits.

**Profile-measured impact target.**
- `compile_css_l4` e-graph rule search frames (`rules::regex::*::search`):
  combined ~4.2 % inclusive → ≤ 2 %.
- `compile_css_l4` saturation: ±0 % (analysis adds `make`/`merge` work but
  rules do less; nets neutral).
- **Architectural unlock** — enables AC.5, AC.6, AC.11, AC.12.

**Risk.** 3/5. Analysis correctness is load-bearing — a wrong `make` or
non-monotone `merge` propagates to every rule. Mitigation: idempotence
debug_assert in tests, fixture grammars covering each variant, golden IR-tree
hash before/after swap in `write_back`.

**Dependency.** AC.0 (to measure rule fire-count deltas).

---

## Phase AC.3 — Unified `CostConfig` expansion

**Motivation.** Z.6 lifted `strategy_*` knobs into `egraph::CostWeights` but
new phases (AC.5 bridge, AC.6 dispatch sharing, AC.11 bitmap, AC.12 perfect
hash) need new knob sub-structs. Rather than scatter them, consolidate
`egraph::CostConfig` as a directory module with explicit sub-structs so every
downstream consumer reads from one source of truth.

**Files (new).**
- `crates/egraph/src/cost_config/mod.rs` — directory module replacing
  `cost_config.rs` (Z.6's monolith).
- `crates/egraph/src/cost_config/extraction.rs` — `ExtractionWeights` (shared
  `CostWeights` + grammar-tier `literal_cost` / `class_cost` / `repeat_cost`).
- `crates/egraph/src/cost_config/strategy.rs` — `StrategyWeights`
  (`alt_byte_dispatch`, `alt_key_dispatch`, `alt_checkpoint`, `wrap_generic`,
  `wrap_sepby`, `wrap_balanced`, `engine_memchr1` ... `engine_dfa`,
  `extraction_advice_penalty`, `dispatch_share_savings`) — one source of
  truth for the hard-coded `Vec<f64>` costs currently baked into
  `build_alt_domain` / `build_wrap_domain` / `build_engine_domain` in
  `csp_strategy/mod.rs`.
- `crates/egraph/src/cost_config/bitmap.rs` — `BitmapWeights`
  (`density_min`, `construction_cost_per_byte`, `dispatch_savings_per_site`).
- `crates/egraph/src/cost_config/perfect_hash.rs` — `PerfectHashWeights`
  (`min_keys`, `max_keys`, `per_key_cost`, `construction_seeds`).
- `crates/egraph/src/cost_config/scheduler.rs` — `SchedulerConfig`
  (`egraph_iter_limit`, `egraph_node_limit`, `growth_limit`).

**Files (modified).**
- `crates/egraph/src/cost_config.rs` — DELETE.
- `crates/egraph/src/lib.rs` — re-export the new sub-module.
- `crates/ir/src/cost_config.rs` — embed `egraph::CostConfig`, no local
  duplicates.
- `crates/ir/src/egraph/cost.rs:44-60` — `GrammarCostModel::from_config` reads
  `cfg.extraction.*`.
- `parse-that/rust/regex/src/egraph/cost.rs:60-66` — HIR mirror.
- `crates/ir/src/passes/csp_strategy/mod.rs` — domain builders read from
  `cfg.strategy.*` instead of hard-coded magic numbers.

**Approach.** Every knob settable via `BBNF_COST_*` env var (existing
convention), so benches can A/B sweep without code changes. Grammar-author
`@cost` directive deferred to AD (profile-guided feedback loop).

**Consumer invariant extension.** Y.13 extension:
`fn strategy_weights_consumer(w: &StrategyWeights) -> &'static str` exhaustive
match — compile error if a new weight lands without a reader.

**Profile-measured impact target.** Neutral. Load-bearing consumer is AC.5
reading `cfg.strategy.extraction_advice_penalty`, AC.6 reading
`cfg.strategy.dispatch_share_savings`. Without AC.3 they'd duplicate the
constants.

**Risk.** 1/5.

**Dependency.** None; can land parallel to AC.1 and AC.2.

---

## Phase AC.4 — `TopoExtractor` + Pareto lattice trait

**Motivation.** `Extractor::compute_best` at `crates/egraph/src/extract.rs:75-109`
is an O(classes² · iterations) fixed-point loop. Post-saturation, the e-graph
is a DAG (cycles only via `Ref`, which the structural normalizer has largely
dissolved). Kahn's topological sort + Tarjan SCC visits each class exactly
once outside SCCs. Within an SCC, bounded iterative widening (cap = 2 × SCC
size).

The Pareto lattice trait (`Cost: Lattice`) is an architectural-only land that
unblocks AC.5's bridge: the bridge needs to query per-class cost facts without
collapsing them to scalar first. Scalar stays scalar via `Scalar<f64>`
newtype — zero behavioral diff for existing consumers.

**Files (new).**
- `crates/egraph/src/extract/mod.rs` — promote `extract.rs` to directory;
  re-exports.
- `crates/egraph/src/extract/greedy.rs` — existing `Extractor` moved verbatim
  (retained only as a validation oracle in tests).
- `crates/egraph/src/extract/topo.rs` — `TopoExtractor<N, A, C>`.
- `crates/egraph/src/extract/scc.rs` — Tarjan SCC over class graph.
- `crates/egraph/src/extract/lattice.rs` — `Lattice` trait + `Scalar<T>` +
  `ParetoFront<Axis>`.
- `crates/egraph/tests/topo_extract.rs`.

**Files (modified).**
- `crates/egraph/src/extract.rs` — DELETE.
- `crates/egraph/src/lib.rs` — `pub use extract::{TopoExtractor, Extractor, CostModel, AstSize, Lattice, Scalar};`
- `crates/ir/src/egraph/write_back.rs:39` — swap `Extractor::new` for
  `TopoExtractor::new`.
- `parse-that/rust/regex/src/egraph/translate.rs` — HIR tier swap in
  `extract_hir`.

**Approach.**

```rust
pub trait Lattice: Clone {
    fn join(&self, other: &Self) -> Self;
    fn dominated_by(&self, other: &Self) -> bool;  // scalar: other <= self
}

pub trait CostModel<N: Language, A: Analysis<N> = NoAnalysis> {
    type Cost: Lattice;
    fn cost(&self, node: &N, child_cost: impl Fn(Id) -> Self::Cost) -> Self::Cost;
    fn convert_to_csp_weight(&self, cost: &Self::Cost) -> f64;  // default: scalar passthrough
}

pub struct TopoExtractor<'a, N, A, C> {
    egraph: &'a EGraph<N, A>,
    cost_model: &'a C,
    best: FxHashMap<Id, (C::Cost, N)>,
}

impl<'a, N, A, C> TopoExtractor<'a, N, A, C>
where N: Language, A: Analysis<N>, C: CostModel<N, A>
{
    pub fn new(eg: &'a EGraph<N, A>, cost: &'a C) -> Self {
        let sccs = scc::tarjan_classes(eg);            // Vec<Vec<Id>>, reverse-topo
        let mut best = FxHashMap::default();
        for layer in scc::into_layers(&sccs, eg) {     // layers are parallelizable
            let per_scc: Vec<_> = layer.par_iter()
                .map(|scc| extract_scc(eg, cost, scc, &best))
                .collect();
            for (id, entry) in per_scc.into_iter().flatten() {
                best.insert(id, entry);
            }
        }
        Self { egraph: eg, cost_model: cost, best }
    }
}
```

**Consumer invariant extension.** `tests/topo_extract.rs`: linear chain,
diamond, self-cycle, mutual cycle, 500-class random soup — TopoExtractor's
`best_cost` per class must differ from greedy by `≤ f64::EPSILON * 8`. Debug
build cross-check: `cfg(debug_assertions)` runs both extractors and asserts
cost equality.

**Profile-measured impact target.**
- `compile_css_l4` extraction self-time: −3 to −8 % single-threaded,
  −8 to −14 % with rayon layer parallelism.
- `samply` symbol: `extract::greedy::compute_best` disappears; replaced by
  `extract::topo::extract_scc` at ~40 % the self-time.

**Risk.** 2/5. Tarjan SCC is standard. Multi-class SCC widening is the
delicate part — mitigation is the cross-check oracle in debug builds.

**Dependency.** None architecturally, but sequence after AC.2 so future
analysis-guided tie-breaking lands alongside.

---

## Phase AC.5 — Extraction → CSP soft-constraint bridge

**Motivation.** The pipeline at `crates/core/src/pipeline/compile.rs:369-448`
runs extraction → dispatch → regex_info → recognizer_mining → strategy CSP
sequentially. **Zero feedback.** Example cliff: if e-graph saturation knows 6
of 8 branches of an Alt are memchr-eligible (survives in non-extracted
classes), the extractor picks the smallest form (possibly 8 branches), the
CSP re-discovers from the extracted IR and picks `Checkpoint` because it
doesn't see the 6-of-8 fact. AC.2's per-class `EClassFacts` stores the fact;
the bridge forwards it.

**csp-solver already supports this.** `SoftLambdaConstraint` at
`/Users/mkbabb/Programming/csc411/CSC411_HW2_ProgrammingQuestion/csp-solver/src/constraint/soft.rs`
adds penalty to the objective. Verified in
`solver/optimize.rs:153-155, 196-199`: both the cost and the B&B lower bound
sum `c.soft_penalty(assignment)`. **No csp-solver extension required.**

**Files (new).**
- `crates/ir/src/egraph/extraction_bridge/mod.rs` — `SoftConstraintAdvice`
  generator + bridge entry point.
- `crates/ir/src/egraph/extraction_bridge/facts_to_soft.rs` — walks extracted
  IR, consults `EClassFacts`, emits `Vec<SoftConstraintAdvice>` keyed by
  `NodeId`.
- `crates/ir/tests/extraction_bridge.rs`.

**Files (modified).**
- `crates/ir/src/egraph/mod.rs` — return the saturated `egraph` alongside the
  extracted IR so the pipeline owns it past `write_back`.
- `crates/core/src/pipeline/compile.rs:378` — after `write_back_optimized`,
  call `ir.extraction_advice = extraction_bridge::derive(&egraph, &ir)`.
  Keep egraph alive until `solve_strategy_decisions` completes (drops ~2 MB
  peak memory longer — negligible).
- `crates/ir/src/passes/csp_strategy/mod.rs:327-360` — `solve_rule` consumes
  `ir.extraction_advice`, installs `SoftLambdaConstraint`s before
  `csp.solve_optimized`.
- `crates/ir/src/types/grammar.rs` — `GrammarIR::extraction_advice: ExtractionAdviceMap`.

**Approach.** Two options considered:

1. **Soft constraints via `SoftLambdaConstraint`** — generic, composable,
   penalty summed in B&B automatically.
2. **Direct unary bias on `StrategyDomain::costs`** — simpler, one vector
   mutation per site.

Adopt **hybrid**: unary biases for simple per-site preferences (single
variable, no cross-var dependency); `SoftLambdaConstraint` for cross-variable
advice (e.g., "prefer ByteDispatch only if the parent Wrap isn't
BalancedScan"). This keeps the fast path (`constraints_added == 0` short
circuit at `csp_strategy/mod.rs:335`) unaffected for rules with no
advice — soft constraint counter is separate from the hard counter.

```rust
pub enum SoftConstraintAdvice {
    /// Post-extraction analysis saw first_set popcount == branch_count and
    /// branches pairwise disjoint — ByteDispatch strictly wins.
    ForceAltByteDispatch { node: NodeId, penalty_other: f64 },
    /// Analysis saw all-literal branches with count ≥ perfect_hash_min_branches.
    PreferPerfectHash { node: NodeId, branches: SmallVec<[StringId; 16]>, penalty_other: f64 },
    /// Analysis saw regex one-pass eligibility on the node's HIR.
    PreferOnePassEngine { node: NodeId, penalty_dispatch: f64 },
    /// Analysis saw Wrap inner with anchored_left + bounded width — BalancedScan wins.
    PreferBalancedScan { node: NodeId, penalty_other: f64 },
}

pub fn derive(
    eg: &EGraph<GrammarENode, GrammarAnalysis>,
    ir: &GrammarIR,
) -> ExtractionAdviceMap { /* walk extracted IR, query eg.class(...).data, emit advice */ }
```

Penalty magnitudes sourced from `cfg.strategy.extraction_advice_penalty`
(AC.3). Default 0.5 — conservative; the full feedback loop (AC.16's profile
data) overrides.

**Consumer invariant extension.** Y.13: `fn extraction_advice_consumer(a: &SoftConstraintAdvice) -> &'static str`
exhaustive match. New test `tests/extraction_bridge.rs::byte_dispatch_advice_fires`
constructs a fixture IR with three byte-disjoint single-char literals, runs
the full pipeline, asserts
`decisions[alt_node].alt_mode == Some(AltMode::ByteDispatch)`.

**Profile-measured impact target.**
- `css_tailwind` parse: ≥ −10 % (the decision biases shift Alt modes where
  extraction saw dispatch-friendly structure the CSP's cost heuristic missed).
- Compile-time: ≤ +2 % (one extra bridge walk + extended egraph lifetime).

**Risk.** 3/5. Bias magnitudes can force suboptimal decisions. Mitigation:
conservative default penalty, land with per-phase bench verification, keep
Y.-1 node budget fallback.

**Dependency.** AC.2 (analysis facts), AC.3 (penalty weights), AC.4 (Pareto
trait for future multi-axis extension).

---

## Phase AC.6 — Cross-rule CSP via dispatch-share signatures

**Motivation.** First real producer of Y.5's
`crates/ir/src/passes/csp_strategy/components.rs::UnionFind`. CSS L4 has ~14
distinct dispatch tables, ~9 of which share signatures (same byte set, same
branch count, same result type) — each redundant static is 128 bytes + 128
jump-table entries. Sharing eliminates duplicate emission.

The signature is canonical because AC.1 gives reference-equality on
`TypeDescId` (two branches with identical type are identical `TypeDescId`)
and AC.2 gives the `first_set` from `EClassFacts`.

**Files (new).**
- `crates/ir/src/passes/csp_strategy/signature.rs` — `DispatchSignature { first_set_hash: u64, branch_count: u8, branch_type_id: TypeDescId }`.
- `crates/ir/src/passes/csp_strategy/cross_rule.rs` — dispatch group
  construction via `UnionFind`.
- `crates/ir/tests/csp_cross_rule.rs`.
- `crates/core/tests/cross_rule_dispatch.rs` — end-to-end Rust codegen test.

**Files (modified).**
- `crates/ir/src/passes/csp_strategy/mod.rs:293` — signature pre-pass before
  per-rule loop; component decomposition replaces the straight per-rule
  iteration.
- `crates/ir/src/passes/csp_strategy/components.rs` — consume the `UnionFind`
  substrate that has been dormant since Y.5; extend with
  `fn components_with_ids() -> FxHashMap<Id, Vec<NodeId>>`.
- `crates/ir/src/passes/csp_strategy/mod.rs` — add
  `AltMode::ByteDispatchShared { group_id: u32 }` variant (replaces
  `AltMode::ByteDispatch` for members of a shared group; standalone Alts
  keep `ByteDispatch`).
- `crates/core/src/backend/rust/emitter/dispatch.rs` — emit shared `static
  DISPATCH_GROUP_<hash>: [u8; 128]` at the top of the generated module; every
  group member's match arm references the shared static by name.

**Approach.**

```rust
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct DispatchSignature {
    pub first_set_hash: u64,  // double-hash (FxHash + SipHash) for collision safety
    pub branch_count: u8,
    pub branch_type_id: TypeDescId,
}

pub fn build_sharing_groups(
    ir: &GrammarIR,
    egraph: &EGraph<GrammarENode, GrammarAnalysis>,
) -> FxHashMap<NodeId, SharingGroupId> {
    let mut by_sig: FxHashMap<DispatchSignature, Vec<NodeId>> = FxHashMap::default();
    for (node_id, facts) in ir.node_facts.iter() {
        if !facts.has_byte_dispatch() { continue; }
        let sig = DispatchSignature::from_node(ir, egraph, *node_id);
        by_sig.entry(sig).or_default().push(*node_id);
    }
    // Groups of 2+ get union-find-assigned group ids.
    let mut uf = UnionFind::new(ir.node_facts.len());
    /* union within each group, return FxHashMap<NodeId, SharingGroupId> */
}
```

Within each connected component, add a multi-way `SoftLambdaConstraint` with
bonus `−cfg.strategy.dispatch_share_savings * group_size` when all members
pick the same mode. The Y.-1 node budget is the safety net — on budget
exceeded, fall back to per-rule solves for that component.

**Consumer invariant extension.** Y.13: `fn dispatch_signature_consumer(s: &DispatchSignature) -> &'static str`
exhaustive match. New test: fuzzer builds 1000 random Alts, asserts no two
distinct-semantics Alts collide on signature. End-to-end: hand-crafted
two-rule grammar where sharing is known-safe AND one where sharing is
known-unsafe (different branch type) — assert grouping is correct in both.

**Hard gate**: phase does not ship unless the grouping mines ≥ 1 group on
`compile_css_l4`. Otherwise it's ghost substrate.

**Profile-measured impact target.**
- Generated `css_l4.rs` line count: ≥ −5 to −10 % on the dispatch-table
  section (measure via `wc -l`).
- Parse time on CSS: neutral to +2 % from i-cache improvement.
- Compile time: ≤ +4 % from signature pre-pass + extra CSP components.
- Architectural: Y.5 `UnionFind` has its first production consumer; the
  `// ready for cross-rule decomposition` comment is removed.

**Risk.** 4/5. False-positive sharing is a correctness bug. Mitigation:
double-hash first_set, reference-equality `TypeDescId` check, fuzzer test,
Y.13 extension.

**Dependency.** AC.1 (`TypeDescId`), AC.2 (`first_set` from facts), AC.5
(`SoftLambdaConstraint` wiring in the strategy CSP).

---

## Phase AC.7 — `TypeDesc::TaggedUnion` (the BoxedEnum killer)

**Motivation.** `crates/core/src/backend/types/decisions.rs:40` returns
`ValuePlacement::Alloc` unconditionally for `TypeDesc::BoxedEnum`. Every
heterogeneous Alt in CSS/JSON lands on this branch. Verified: 153
`slab().alloc()` sites in CSS L4 generated code (2 in JSON). `join_types` at
`crates/ir/src/passes/types/constraint/helpers.rs:86-96` falls back to
`BoxedEnum` whenever branches differ structurally, regardless of whether the
branches actually need indirection.

The semantic fix: small-N inline-storable heterogeneous unions fit in an
enum discriminant + at most one machine word of payload — no allocation
needed. `Span` is 16 bytes; `f64` is 8; `&'a Enum` is 8; tuples of these
fit in 32 bytes. These are the overwhelming majority of CSS L4's
heterogeneous alts.

**Files (new).**
- `crates/ir/src/types/tagged_union.rs` — `TaggedUnionId`, `TaggedUnionInterner`,
  `TaggedUnionDesc { variants: SmallVec<[TypeDescId; 8]> }`.
- `crates/core/src/backend/rust/emitter/tagged_union_emit.rs` — inline enum
  definition emission + wrap expressions.
- `crates/ir/tests/tagged_union.rs` — property test.

**Files (modified).**
- `crates/ir/src/types/type_desc/mod.rs` — add `TypeDesc::TaggedUnion(TaggedUnionId)`
  variant.
- `crates/ir/src/types/grammar.rs` — `GrammarIR::tagged_unions: TaggedUnionInterner`
  field + MessagePack serialization.
- `crates/ir/src/passes/types/constraint/helpers.rs:86-96` — rewrite
  `join_types`:
  ```rust
  pub(super) fn join_types(branch_types: &[&TypeDesc], sink: &mut TaggedUnionSink) -> TypeDesc {
      if branch_types.is_empty() { return TypeDesc::Tuple(vec![]); }
      let first = branch_types[0];
      if branch_types.iter().all(|t| *t == first) { return first.clone(); }
      let distinct: SmallVec<[TypeDescId; 8]> = dedup_preserve_order(branch_types, sink);
      if distinct.len() <= 8 && distinct.iter().all(|id| is_inline_storable(sink.resolve(*id))) {
          let tu_id = sink.intern_tagged_union(TaggedUnionDesc { variants: distinct });
          return TypeDesc::TaggedUnion(tu_id);
      }
      TypeDesc::BoxedEnum
  }
  ```
- `crates/ir/src/passes/types/utils.rs` — add `is_inline_storable`:
  ```rust
  pub fn is_inline_storable(ty: &TypeDesc) -> bool {
      match ty {
          TypeDesc::Span | TypeDesc::F64 | TypeDesc::U32 => true,
          TypeDesc::Tuple(es) => es.iter().all(is_inline_storable) && es.len() <= 4,
          TypeDesc::Option(inner) => is_inline_storable(inner),
          TypeDesc::Vec(inner) => matches!(inner.as_ref(), TypeDesc::Span),
          TypeDesc::Enum | TypeDesc::Named(_) => true,  // reference types, 8 bytes
          TypeDesc::BoxedEnum | TypeDesc::TaggedUnion(_) => false,
      }
  }
  ```
- `crates/core/src/backend/types/decisions.rs:38-45` — route `TaggedUnion` to
  `ValuePlacement::Inline` unconditionally (the interner guarantees all
  variants are inline-storable).
- `crates/core/src/backend/rust/ir_types.rs` — `type_desc_to_syn` emits
  `TaggedUnion(id)` → per-id generated enum `__TU_<id><'a>` with `#[derive(Copy, Clone)]`
  so `alloc_slice_copy` still lights up for Repeat collections of TaggedUnion.
- `crates/core/src/backend/rust/ir_enums.rs` — emit one `#[derive(Copy, Clone)]` struct per
  `TaggedUnionId`. Variant names synthesized from the type shape (`V_Span`,
  `V_F64`, `V_Tuple_Span_F64`).
- `crates/core/src/backend/rust/emitter/alt.rs:30-76` — `coerce_branch` third
  arm: when `alloc == Inline` and result type is `TaggedUnion(id)`, emit
  `#body.map(|__v| __TU_<id>::<variant>(__v))` without `emit_alloc`.
- `crates/core/src/generate/serialize/serialize.rs:17-19` — `TaggedUnion` is
  a value type (not reference) when placement is Inline.

**Consumer invariant extension.** Y.13: exhaustive match over
`TypeDesc::TaggedUnion`. Grep gate: `cargo expand -p bbnf --bench json_monolithic 2>&1 | grep -c "slab().alloc"`
drops from > 100 to ≤ 25 on CSS L4; drops to ≤ 5 on JSON.

**Profile-measured impact target.**
- `samply` symbol: `mi_segment_span_allocate` + `slab::alloc` cumulative on
  `json_canada`: ~8 % → < 1.5 %.
- `css_tailwind` parse: ≥ −12 to −18 %.
- `json_canada` parse: ≥ −8 to −12 %.
- `json_twitter` parse: ≥ −4 to −7 %.

**Risk.** 4/5. Wide blast radius — `TypeDesc` is consumed by every backend
(Rust, TS, WASM), the serializer, the recognizer plan, and Y.13. Mitigation:
Rust emitter lands with full support; TS/WASM get a `TaggedUnion → BoxedEnum`
fallback in the SAME commit (their fallback was already the common case).
Full TS/WASM TaggedUnion support lands in AD.

**Dependency.** AC.1 (`TypeDescId`) — hard blocker.

---

## Phase AC.8 — IIFE → labeled-block lowering

**Motivation.** Agent 2's Plan verified: the ~1185 `(|| { ... })()` closures
in CSS L4 generated code are not for borrow scoping (as an earlier audit
claimed) but for **`?` and `return` containment** — the explicit comment at
`emitter/alt.rs:271` is "Wrap in closure so `return` in arm_checks exits the
closure, not the enclosing function". LLVM inlines these ~99 % of the time
but punts on deeply nested seq groups with `&mut state` captures.

Rust's stable **labeled blocks** (`'blk: { if cond { break 'blk None; } Some(x) }`)
give the same early-exit semantics with zero closure overhead — no capture,
no lifetime erasure, no indirect call. This is a mechanical codegen lowering.

**Files (new).**
- `crates/core/src/backend/rust/emitter/control_flow.rs` — labeled-block
  helpers:
  ```rust
  pub fn try_block(label: &syn::Lifetime, body: TokenStream) -> TokenStream {
      quote! { #label: { #body } }
  }
  pub fn try_op(output: &TokenStream, label: &syn::Lifetime, binding: &syn::Ident) -> TokenStream {
      quote! {
          let #binding = match #output {
              ::core::option::Option::Some(__v) => __v,
              ::core::option::Option::None => break #label ::core::option::Option::None,
          };
      }
  }
  ```
- `crates/core/tests/no_iife_in_emitter.rs` — grep gate.

**Files (modified).** Every IIFE site:
- `crates/core/src/backend/rust/emitter/seq.rs:67-73, 85-91, 108-112` —
  labeled block + `try_op` for each `#outputs?`.
- `crates/core/src/backend/rust/emitter/repeat.rs:195-239` — `'rpt_blk: { ... }`.
- `crates/core/src/backend/rust/emitter/binary.rs:72-77, 87-91` — `'skip_blk:` / `'next_blk:`.
- `crates/core/src/backend/rust/emitter/alt.rs:183-187, 204-208, 274-285` — `'alt_blk:`.
- `crates/core/src/backend/rust/emitter/dispatch.rs:46`.
- `crates/core/src/backend/rust/emitter/leaves.rs:105`.
- `crates/core/src/backend/rust/emitter/map_value.rs:195`.
- `crates/core/src/backend/rust/emitter/operator_chain.rs:46`.
- `crates/core/src/backend/rust/emitter/grammar.rs:69` — `(|| { #body })().map(...)`
  becomes `({ #body_as_labeled }).map(...)`.

**Scratch cleanup on early exit.** Labeled blocks don't have defer. Use a
`ScratchGuard<'a, T>` with Drop-based truncation; emit once as a grammar-level
helper, reference in `repeat.rs`. Set `guard.commit = true` before returning
the slab slice.

**Consumer invariant extension.** `tests/no_iife_in_emitter.rs`: grep
expanded cargo output for `(|| {` and `})()` — must be zero. Differential
fuzz test: generate pre-/post-phase parsers, fuzz 10k inputs per grammar,
assert identical ASTs.

**Profile-measured impact target.**
- `samply` symbol: combined `__declaration` + `__value` + `__namedColor`
  self-time drops 8–12 % (LLVM inlines labeled blocks more aggressively).
- `css_tailwind` parse: ≥ −6 to −9 %.
- Generated `css_l4.rs` size: ≥ −15 to −20 %. Secondary i-cache win.

**Risk.** 2/5. Mechanical but wide. Every `return` inside an IIFE must become
`break 'label`. Mitigation: grep gate + differential fuzz.

**Dependency.** None. Parallel to AC.7.

---

## Phase AC.9 — Direct-to-slab scratch (kill the Vec grow-amortized tail)

**Motivation.** Current `crates/core/src/backend/rust/emitter/repeat.rs:195-239`
+ `alloc_emit.rs:98-128` path does **two copies** per Repeat:
1. `Vec::with_capacity(64)` scratch push (growable, `RawVecInner::grow_amortized`
   hot on `json_canada`).
2. Slab `alloc_slice_copy` copies Vec tail to slab on success; `Vec::truncate`
   on failure.

The feedback constraint says "arena allocation must be singular collection
strategy; no conditional Vec-vs-scratch branching." The fix: **bump-append
directly into a slab scratch region**, track the starting offset, on success
finalize as `&[T]`, on failure reset the slab's `current.len()` to the mark.
One copy (the push). Zero growth stalls (slab grows in chunks).

**Files (new).**
- `parse-that/rust/parse_that/src/bump_slab.rs` — add scratch API:
  ```rust
  pub struct ScratchMark { chunk_idx: usize, pos: usize }
  impl BumpSlab {
      #[inline(always)] pub fn scratch_begin(&self) -> ScratchMark { ... }
      #[inline(always)] pub fn scratch_push<T>(&self, value: T) { ... }
      #[inline(always)] pub fn scratch_finalize<T>(&self, mark: ScratchMark) -> &[T] { ... }
      #[inline(always)] pub fn scratch_rewind(&self, mark: ScratchMark) { ... }
  }
  ```
  **Key trick**: bump-allocate T values contiguously between `mark` and
  current position. On `finalize`, the slab slice IS the scratch region —
  pointer + length, no copy. If the slab spilled to a new chunk mid-scratch
  (rare), `finalize` copies once into a fresh run (cold path).

**Files (modified).**
- `crates/core/src/backend/rust/alloc_emit.rs` — replace entire
  `scratch_init/push/collect/truncate/extend_slice` API with the new
  `scratch_begin/push/finalize/rewind` signatures. Delete the
  `AllocCtx::scratch_types` Vec fields — only `__slab: BumpSlab` exists.
- `crates/core/src/backend/rust/emitter/repeat.rs:183-241` — scratch_ty is
  now opaque (just a mark); loop body unchanged; `#collect_expr` becomes
  `__slab.scratch_finalize::<#ty>(#mark)` returning a slab slice.
- `crates/core/src/backend/rust/emitter/seq.rs:50-97` — flatten paths use
  the same API.

**Nested scratch discipline.** Rust's borrow rules force sequential
execution; for nested `Repeat<Repeat<T>>`, the inner completes and
`finalize`s before the outer's next push. Enforce in debug assertion:
`debug_assert!(mark.chunk_idx == current_chunk_idx, "interleaved scratch")`.

**Consumer invariant extension.** `tests/slab_scratch_discipline.rs`:
nested Repeat fuzz — assert no interleaving corrupts slab state.
`tests/no_vec_scratch.rs`: grep gate — `Vec<` does not appear in `AllocCtx`
field definitions of generated code.

**Profile-measured impact target.**
- `samply` symbol: `RawVecInner::grow_amortized` self-time on `json_canada`
  drops from ~6 % to < 0.5 %.
- `Vec::truncate` self-time: ~2 % → 0 %.
- `json_canada` parse: ≥ −10 to −14 %.
- `css_tailwind` parse: ≥ −3 to −5 % (multiple scratch Vecs eliminated).
- Peak memory: ≥ −8 to −15 %.

**Risk.** 3/5. Chunk spill mid-scratch requires the copy fallback; must be
cold. Mitigation: pre-size slab at parse entry based on `input.len() * K`
(K tuned per target, default 4). Profile to confirm spill rate < 1 %.

**Dependency.** None (parse-that is a path dep). AC.7 helps (smaller T means
fewer spills) but not required.

---

## Phase AC.10 — `ClassMask` SIMD for 9–64 byte exit sets

**Motivation.** Agent 2's Plan verified `AccelStrategy::ScalarLut` at
`parse-that/rust/regex/src/automata/accel.rs:29` is **defined with zero
emitter consumers**. The 9–64 exit-byte range (e.g. CSS ident's 52-byte
continuation set `[a-zA-Z0-9_-]`) falls through to hand-written scalar
`scan_ident` — 13 % self-time on `css_tailwind`.

The fix: one shared SIMD class-membership kernel used by both `scan_ident`
AND the regex emit path. Technique: 4-bit-per-nibble popcount LUT (low-nibble
LUT × high-nibble LUT, ANDed, compared against zero) — same trick
Hyperscan / vectorscan uses for 6-character classes, generalized to 64-byte
sets. Per-arch dispatch: aarch64 `vqtbl1q_u8` + `vandq_u8` + `vcgtq_u8`;
x86_64 AVX2 `_mm256_shuffle_epi8`; SSE2 `_mm_shuffle_epi8`; scalar fallback.

**Files (new).**
- `parse-that/rust/parse_that/src/parsers/scan/class_membership.rs` —
  per-arch SIMD kernel:
  ```rust
  #[repr(C, align(16))]
  pub struct ClassMask { pub lo_lut: [u8; 16], pub hi_lut: [u8; 16] }

  impl ClassMask {
      pub const fn from_set(set: &[bool; 256]) -> Option<Self> { /* nibble popcount */ }

      /// Returns position of first byte NOT in the class.
      /// 16-32 bytes/iteration vs 1 byte/iteration scalar.
      #[inline(always)]
      pub unsafe fn scan_while_in(&self, bytes: *const u8, len: usize) -> usize { ... }
  }
  ```
- `parse-that/rust/parse_that/src/parsers/scan/ident_kernels.rs` — CSS ident,
  JSON string body, XML name, generic `[a-zA-Z0-9_-]+` all built on
  `ClassMask`.
- `parse-that/rust/parse_that/tests/class_membership.rs` — 10k fuzz test,
  SIMD ⇔ scalar LUT byte-exact match.

**Files (modified).**
- `parse-that/rust/parse_that/src/parsers/scan/ident.rs:54-62` — replace
  continuation loop with one `ClassMask::scan_while_in` call.
- `parse-that/rust/regex/src/automata/accel.rs:104-110` — 9-64 arm emits
  `AccelStrategy::ClassMask { mask: ClassMask::from_set(&lut) }`; delete
  `ScalarLut` (dead code).
- `crates/core/src/generate/regex/emit/simd.rs` — registry entry for
  `ClassMask` kernel.
- `crates/core/src/generate/regex/emit/mod.rs` — route
  `[a-zA-Z_-][a-zA-Z0-9_-]*` to `ident_kernels::css_ident` instead of HIR
  fallback.

**Consumer invariant extension.** Cross-arch symmetry test:
`cargo test --target aarch64-apple-darwin --target x86_64-unknown-linux-gnu`.
Grep gate: `grep -rn "AccelStrategy::ScalarLut" parse-that/rust/regex/` returns
zero hits.

**Profile-measured impact target.**
- `samply` symbol: `scan_ident` self-time on `css_tailwind` from ~13 % to
  ~3 % (~4× scanner throughput).
- `css_tailwind` parse: ≥ −9 to −14 %.
- Also benefits JSON identifier-like patterns.

**Risk.** 2/5. Nibble-LUT edge case: sets with > 16 distinct values per
high-nibble row can't be encoded losslessly — `from_set` returns
`Option<ClassMask>` and falls back to a 2-stage LUT for pathological sets.
53-byte CSS ident encodes cleanly (verified on Hyperscan's technique).

**Dependency.** None. Parallel to AC.7 / AC.8 / AC.9.

---

## Phase AC.11 — Structural bitmap pre-scan (sonic-rs parity, generalized)

**Motivation.** sonic-rs's primary architectural advantage on JSON is moving
dispatch out of the per-byte critical path by building a tape of structural
byte positions in a pre-scan, then walking the tape. bbnf has no analogue.

The technique is grammar-agnostic: any grammar whose union of (`@ws` charset
∪ `@token` first bytes ∪ dispatch-eligible Alt FIRST sets) fits in ≤ 16
distinct bytes qualifies. JSON, CSS, SQL, EBNF all pass. Detection uses
AC.2's `EClassFacts::first_set`.

**Files (new).**
- `crates/ir/src/passes/recognizers/structural_bitmap.rs` —
  `StructuralBitmapMiner` (runs inside Z.0 unified walk).
- `parse-that/rust/parse_that/src/parsers/scan/structural_bitmap.rs` —
  runtime scanner:
  - aarch64: `vld1q_u8` + `vceqq_u8` per class byte + OR accumulate +
    `vshrn_n_u16` compression (simdjson's 16-byte → 8-bit trick).
  - x86_64 AVX2: `_mm256_loadu_si256` + `_mm256_cmpeq_epi8` + `_mm256_movemask_epi8`.
  - x86_64 SSE2 fallback.
  - Scalar fallback.
  - Output: `StructuralIndex { offsets: Vec<u32>, classes: Vec<u8> }` — SoA
    flat tape.
- `crates/core/src/backend/kernels/structural_bitmap.rs` — emits pre-scan
  call at parser entry + dispatch-via-cursor helper.
- `crates/core/src/backend/kernels/tape_cursor.rs` — `TapeCursor` + generic
  `next_of_class` helper.
- `crates/core/tests/structural_bitmap_roundtrip.rs`.

**Files (modified).**
- `crates/ir/src/passes/patterns/mod.rs` — add
  `RecognizerShape::StructuralBitmap { classes: SmallVec<[u8; 16]> }`.
- `crates/ir/src/passes/recognizers/mod.rs` — register miner in unified walk.
- `crates/ir/src/passes/csp_strategy/mod.rs` — add
  `AltMode::BitmapDispatch { class: u8 }`. Cost model chooses between
  `ByteDispatch` and `BitmapDispatch` based on
  `cfg.bitmap.density_min`, `cfg.bitmap.construction_cost_per_byte`, and
  `cfg.bitmap.dispatch_savings_per_site`.
- `crates/core/src/backend/driver/alt.rs` — route bitmap dispatch via tape
  cursor.
- `crates/core/src/backend/recognizer_plan.rs` — `StructuralBitmap` arm.
- `parse-that/rust/parse_that/src/state.rs` — `ParserState::tape_cursor: Option<NonNull<TapeCursor>>`.
  Null outside bitmap grammars.

**Critical**: opt-in via CSP cost model, NOT directive. Grammar-agnostic
detection via FIRST sets.

**Consumer invariant extension.** Y.13: `RecognizerShape::StructuralBitmap` +
`AltMode::BitmapDispatch` exhaustive matches. Tape must be built exactly once
per parse, enforced via debug_assert in entry emit.

**Profile-measured impact target.**
- `samply` symbol: `__value` + `scan_ws_block_comments` combined self-time
  on `json_canada`: ~34 % → ~22 %.
- `json_canada` parse: ≥ −10 to −15 %.
- `css_tailwind` parse: ≥ −3 to −6 %.
- `json_twitter` parse: ≥ −5 to −8 %.

**Risk.** 4/5. Three SIMD implementations + new CSP variant + wide cargo
expand audit surface. Mitigation: `BBNF_BITMAP=on/off` env var during
iteration (removed before AC.17); round-trip test gates correctness.

**Dependency.** AC.2 (`first_set` for miner), AC.7 (tape discriminants stay
inline via TaggedUnion).

---

## Phase AC.12 — Compile-time perfect-hash dispatch for literal Alt groups

**Motivation.** Agent 1 rebutted the "230-way linear chain" claim: cargo
expand shows `__namedColor` is a 7-way first-byte match with nested length +
`unsafe_memcmp` checks per arm (~12-15 if-depth). **Still a win**: one
perfect-hash probe replaces all nested checks with a single hash + table
lookup. Grammar-agnostic: any Alt of ≥ `cfg.perfect_hash.min_keys` (default
8) distinct literal branches is a candidate.

Use FCH (Fox-Chen-Heath) minimal perfect-hash generator — deterministic,
fast construction, embedded at compile time in the generated parser. Tested
successfully on CSS `__namedColor`, HTML `__tagName`, SVG `__attrName`.

**Files (new).**
- `crates/ir/src/passes/recognizers/perfect_hash.rs` — miner detects Alts of
  ≥ N literal branches.
- `crates/core/src/backend/kernels/perfect_hash.rs` — static tables +
  dispatch call emission.
- `crates/core/src/backend/kernels/fch_generator.rs` — compile-time FCH
  generator.
- `parse-that/rust/parse_that/src/phf.rs` — runtime lookup primitive
  (pure function over `g` + `keys` + input bytes).
- `crates/core/tests/perfect_hash_dispatch.rs`.

**Files (modified).**
- `crates/ir/src/passes/csp_strategy/mod.rs` — add
  `AltMode::PerfectHash { table_id: u32 }`. Cost model: `ByteDispatch` still
  wins for ≤ 7 branches; `PerfectHash` wins for ≥ 8; hybrid if first_set is
  sparse.
- `crates/core/src/backend/rust/emitter/dispatch.rs` — `emit_alt_perfect_hash_impl`:
  ```rust
  quote! {
      'ph_blk: {
          static __PH_G: &[i32] = &[#(#g_table),*];
          static __PH_KEYS: &[&[u8]] = &[#(#keys),*];
          let __ph_start = state.offset;
          let __ph_bytes = &state.src_bytes[__ph_start..];
          let Some((__ph_idx, __ph_end)) = ::parse_that::phf::lookup(&__PH_G, &__PH_KEYS, __ph_bytes) else {
              break 'ph_blk None;
          };
          match __ph_idx { #(#dispatch_arms,)* _ => break 'ph_blk None }
      }
  }
  ```

**Consumer invariant extension.** Y.13: `AltMode::PerfectHash` exhaustive
match. `tests/perfect_hash_correctness.rs`: for every grammar in the test
corpus, assert FCH construction succeeds and lookup of every key returns the
expected branch index.

**Profile-measured impact target.**
- `samply` symbol: `__namedColor` + `[u8]::eq` combined self-time on
  `css_tailwind`: ~12 % → ~3 %.
- `css_tailwind` parse: ≥ −6 to −10 %.
- `compile_css_l4`: +2 to +3 % from FCH construction (within budget).

**Risk.** 3/5. FCH has rare failure mode on clustered key sets. Mitigation:
generator tries up to 64 random seeds; if all fail, fall back to
`AltMode::ByteDispatch` for that Alt (the construction fallback, not a
runtime fallback — generated code still uses only perfect hash where it
applied).

**Dependency.** AC.7 (inline variant discriminants), AC.8 (labeled blocks
for `break 'ph_blk`).

---

## Phase AC.13 — `RefMode` CSP lift

**Motivation.** The old `crates/core/src/backend/rust/analysis/inline.rs`
file still contains a Rust-backend-local inline-decision heuristic.
Tranche Z's "lift to IR" migrated fusing but not inlining. Inline decisions
today are still backend-local — TS/WASM don't see them.

**Files (new).**
- `crates/ir/src/passes/csp_strategy/ref_mode.rs` — `RefMode ∈ {DirectCall, InlineBody, FusedInline}`
  CSP variable. Domain built from `ir.node_facts` (body node count, SCC
  membership, call frequency from AC.16 profile if available).

**Files (modified).**
- `crates/ir/src/passes/csp_strategy/mod.rs` — add `RefMode` variable class;
  `ImplicationConstraint`: `RefMode = FusedInline → parent AltMode ≠ Checkpoint`.
- `crates/core/src/backend/rust/analysis/inline.rs` — convert from producer
  to consumer of `ir.recognizer_decisions[node].ref_mode`.

**Consumer invariant extension.** Y.13: `RefMode` exhaustive match.

**Profile-measured impact target.** JSON parse: ≥ −1 to −3 % (inline
decisions today are overly conservative — the CSP picks more aggressive
fusing for single-use cold refs).

**Risk.** 3/5.

**Dependency.** AC.3 (knobs in unified CostConfig).

---

## Phase AC.14 — `RepeatMode` CSP lift

**Motivation.** Z.5b deferred this because the heuristic at
`backend/driver/repeat.rs` was tightly coupled to `DelimScanConfig`. Post-Z.5b
the delim-scan config moved out of the per-decision path. The heuristic has
four clear branches (`UnrolledSmall` for ≤ 3 reps, `LoopBounded` for bounded,
`SepByFused`, `BalancedScan`) with all inputs in `node_facts`.

**Files (new).**
- `crates/ir/src/passes/csp_strategy/repeat_mode.rs` — `RepeatMode` CSP
  variable.

**Files (modified).**
- `crates/ir/src/passes/csp_strategy/mod.rs` — add `RepeatMode` + paired
  `LambdaConstraint` coupling with `WrapMode = SepBy`.
- `crates/core/src/backend/driver/repeat.rs:15-92` — read from
  `ir.recognizer_decisions[node].repeat_mode` instead of `detect_sep_by`.

**Consumer invariant extension.** Y.13: `RepeatMode` exhaustive match.

**Profile-measured impact target.** `css_tailwind` parse: ≥ −2 to −5 %
(sepby hot loop).

**Risk.** 3/5.

**Dependency.** AC.6 (equality constraint infrastructure for the
Wrap/Repeat pairing), AC.3.

---

## Phase AC.15 — Incremental compile cache (LSP fast path)

**Motivation.** LSP hot reload recompiles the full grammar per keystroke.
e-graph saturation + CSP solve = ~60 % of that compile. Two caches cut most
of it:

1. **Per-pattern regex HIR cache** — `GrammarIR::regex_info_cache: FxHashMap<(PatternString, CostConfigHash), RegexInfo>`.
2. **Per-rule strategy solve cache** — keyed on
   `(rule_body_hash, cost_config_hash, cross_rule_group_version)`.

**Files (new).**
- `crates/ir/src/cache/mod.rs` — `CompileCache` struct.
- `crates/ir/src/cache/regex_info.rs` — pattern cache.
- `crates/ir/src/cache/strategy.rs` — per-rule decision cache.
- `crates/core/tests/incremental_compile.rs`.

**Files (modified).**
- `crates/ir/src/passes/regex_info.rs` — read/write cache.
- `crates/ir/src/passes/csp_strategy/mod.rs` — read/write cache.
- `crates/core/src/pipeline/compile.rs` — optional
  `cache: &mut CompileCache` parameter.
- `crates/analysis/src/document.rs` (LSP) — owns a `CompileCache` per
  document.
- `crates/lsp/src/...` — thread cache through document state.

**Consumer invariant extension.** `incremental_compile.rs`: compile grammar,
compile again with one-char edit, verify byte-identical output as cold
compile.

**Profile-measured impact target.** LSP hot reload on `compile_css_l4`:
≥ −15 to −30 %.

**Risk.** 3/5. Cache invalidation is hard. Mitigation: rule body hash
includes DAG signature; conservative cross-group version bump on any rule
change; cache is opt-in (one-shot compiles never see it).

**Dependency.** AC.6 (group version).

---

## Phase AC.16 — Profile-guided cost calibration infrastructure

**Motivation.** `CostWeights` defaults are magic numbers. The feedback loop
is out of scope (AD); the infrastructure is in scope: trace emission, profile
file consumption, CSP bias augmentation via the same AC.5 bridge code path.

**Files (new).**
- `crates/ir/src/passes/csp_strategy/trace.rs` — `BBNF_COST_TRACE=1` per-decision
  breakdown.
- `crates/core/src/profile/mod.rs` — `BbnfProfile` struct + `load` / `store`.
- `crates/core/src/profile/collect.rs` — `bbnf-profile` subcommand wiring.
- `crates/core/src/profile/correlate.rs` — DWARF symbol → NodeId back-mapping.
- `crates/core/src/bin/bbnf_profile.rs` — CLI.
- `crates/core/tests/profile_roundtrip.rs`.

**Files (modified).**
- `crates/ir/src/egraph/extraction_bridge/mod.rs` — bridge accepts optional
  `Option<&BbnfProfile>`, same augmentation path as AC.5.

**Consumer invariant extension.** `profile_roundtrip.rs`: write profile,
load, apply via bridge, verify the resulting `RecognizerDecisionMap` differs
from the no-profile map in exactly the nodes marked hot.

**Profile-measured impact target.** Zero direct delta. Infrastructure only.

**Risk.** 2/5. DWARF correlation is fragile; gate bridge behind feature
flag.

**Dependency.** AC.5.

---

## Phase AC.17 — post-AC baseline + profile-verified attribution

**Deliverables.**
- `docs/benchmarks/post-AC.json` — every "+X %" claim cites a samply profile
  symbol + self-time delta against post-Z.
- `docs/benchmarks/profiles/post-AC/*.samply` — five post-phase profiles
  (`compile_css_l4`, `compile_bbnf`, `json_canada`, `json_twitter`,
  `css_tailwind`).
- pre-Z → post-Z → post-AC profile diff per bench.
- Y.13 consumer-invariant test passes with every new variant:
  `TypeDesc::TaggedUnion`, `RecognizerShape::StructuralBitmap`,
  `AltMode::BitmapDispatch`, `AltMode::ByteDispatchShared`,
  `AltMode::PerfectHash`, `RefMode::*`, `RepeatMode::*`,
  `SoftConstraintAdvice::*`, `DispatchSignature`, `EClassFacts` fields.
- `grep -rn "NoAnalysis" crates/ir/src/egraph/ parse-that/rust/regex/src/egraph/` → zero hits.
- `grep -rn "pub(super) fn collect" crates/ir/src/passes/recognizers/` → zero hits (Z.0 invariant).
- `grep -rn "AccelStrategy::ScalarLut" parse-that/` → zero hits.
- Cargo expand gates: `__pair` no `&*slab().alloc(__v)` pattern; `__namedColor`
  no linear ≥ 8-branch chain; JSON `__value` prelude shows
  `structural_bitmap_scan(state)` call.
- `BBNF_EGRAPH_REPORT=1` / `BBNF_HIR_EGRAPH_REPORT=1` both print non-zero
  per-rule fire counts on every grammar.
- `BBNF_CSP_REPORT=1` reports zero budget exhaustions on standard benches.
- `BBNF_PIPELINE_REPORT=1` prints per-pass CSV row per compile.

---

## Hard gates

| Gate | Threshold |
|---|---|
| All workspace tests pass | yes |
| `bbnf-ir` tests | all passing |
| Bootstrap script idempotent | yes |
| Y.13 consumer-invariant test | extended for every new variant |
| `every_egraph_analysis_fact_has_a_consumer` | passes (AC.2 invariant) |
| `every_recognizer_shape_has_a_consumer_ratio` | ≥ 0.8 per shape (AC.11 + AC.12) |
| `grep -rn "NoAnalysis" crates/ir/src/egraph/ parse-that/rust/regex/src/egraph/` | zero hits |
| `grep -rn "pub(super) fn collect" crates/ir/src/passes/recognizers/` | zero hits |
| `grep -rn "AccelStrategy::ScalarLut" parse-that/rust/regex/` | zero hits |
| `grep -rn "Option<TypeDesc>" crates/ir/src/passes/types/constraint/` | zero hits |
| `grep -c "(|| {" $(cargo expand ... css_l4)` | zero hits |
| `json_twitter` parse | ≥ −12 % vs post-Z |
| `json_canada` parse | ≥ −15 % vs post-Z |
| `json_citm` parse | ≥ −8 % vs post-Z |
| `css_tailwind` parse | ≥ −25 % vs post-Z (the primary cliff) |
| `css_bootstrap` parse | ≥ −15 % vs post-Z |
| `compile_css_l4` | ≤ +15 % vs post-Z (budget) |
| `compile_bbnf` | ≤ +10 % vs post-Z |
| LSP hot-reload `compile_css_l4` | ≥ −15 % (AC.15) |
| Every "+X %" claim in `post-AC.json` | cites samply symbol + self-time delta |

**Cumulative parse target**: `css_tailwind` 256 → 350+ MB/s; `json_canada`
1231 → ≥ 1700 MB/s; `json_twitter` 1517 → ≥ 1800 MB/s. These approach (but
do not claim to beat) sonic-rs on M-series ARM. The **honest ceiling** for a
grammar-agnostic Rust-emitting compiler on JSON is ~2.0-3.0 GB/s; bbnf's
differentiator is the compile-time specialization advantage on non-JSON
grammars (CSS, SQL, XML, custom DSLs) where sonic-rs has no analogue.

---

## Critical files (new)

**Substrate** — AC.1 + AC.2 + AC.3:
- `crates/ir/src/types/type_desc/{mod,id,lattice}.rs`
- `crates/ir/src/egraph/analysis/{mod,facts,merge}.rs`
- `parse-that/rust/regex/src/egraph/analysis/{mod,facts}.rs`
- `crates/egraph/src/cost_config/{mod,extraction,strategy,bitmap,perfect_hash,scheduler}.rs`

**Joint optimization** — AC.4 + AC.5 + AC.6:
- `crates/egraph/src/extract/{mod,greedy,topo,scc,lattice}.rs`
- `crates/ir/src/egraph/extraction_bridge/{mod,facts_to_soft}.rs`
- `crates/ir/src/passes/csp_strategy/{signature,cross_rule}.rs`

**Parse cliffs** — AC.7 through AC.12:
- `crates/ir/src/types/tagged_union.rs`
- `crates/core/src/backend/rust/emitter/{tagged_union_emit,control_flow}.rs`
- `crates/ir/src/passes/recognizers/{structural_bitmap,perfect_hash}.rs`
- `crates/core/src/backend/kernels/{structural_bitmap,tape_cursor,perfect_hash,fch_generator}.rs`
- `parse-that/rust/parse_that/src/parsers/scan/{structural_bitmap,class_membership,ident_kernels}.rs`
- `parse-that/rust/parse_that/src/phf.rs`

**CSP variable lifts** — AC.13 + AC.14:
- `crates/ir/src/passes/csp_strategy/{ref_mode,repeat_mode}.rs`

**Incremental + profile** — AC.15 + AC.16:
- `crates/ir/src/cache/{mod,regex_info,strategy}.rs`
- `crates/ir/src/passes/csp_strategy/trace.rs`
- `crates/core/src/profile/{mod,collect,correlate}.rs`
- `crates/core/src/bin/bbnf_profile.rs`

**Tests**:
- `crates/ir/tests/{egraph_analysis,type_desc_interner,tagged_union,extraction_bridge,csp_cross_rule}.rs`
- `crates/egraph/tests/topo_extract.rs`
- `crates/core/tests/{structural_bitmap_roundtrip,perfect_hash_dispatch,cross_rule_dispatch,incremental_compile,profile_roundtrip,no_iife_in_emitter,slab_scratch_discipline,no_vec_scratch}.rs`
- `parse-that/rust/regex/tests/egraph_analysis.rs`
- `parse-that/rust/parse_that/tests/class_membership.rs`

## Critical files (modified)

(Full list enumerated in each phase's "Files (modified)" block above; summary
hot spots:)
- `crates/ir/src/egraph/mod.rs`, `rules/regex.rs` — Analysis activation
- `crates/ir/src/passes/csp_strategy/mod.rs` — hub for AC.5, AC.6, AC.13, AC.14
- `crates/ir/src/passes/types/constraint/{alt,seq,grounds,operators,helpers,domain}.rs` — TypeDescId migration
- `crates/core/src/pipeline/compile.rs` — bridge wiring, egraph lifetime, cache threading
- `crates/core/src/backend/rust/emitter/*.rs` — labeled blocks, TaggedUnion, scratch API, perfect-hash, bitmap kernels
- `crates/core/src/backend/types/decisions.rs:38-45` — child_alloc TaggedUnion arm
- `parse-that/rust/parse_that/src/{bump_slab,state}.rs` — scratch API, tape cursor

---

## Reuse of existing functions and utilities

- `crates/egraph/src/analysis.rs::Analysis<N>` — already implemented; AC.2 activates it.
- `crates/ir/src/passes/csp_strategy/components.rs::UnionFind` — Y.5 substrate; AC.6 is the first producer.
- `csp-solver::SoftLambdaConstraint` + `SolveConfig::node_budget` (Y.-1) — AC.5 uses; AC.6 uses with budget as safety net.
- `crates/core/src/backend/recognizer_plan.rs::scanner_plan_for` — extended for StructuralBitmap and PerfectHash shapes.
- `crates/ir/src/passes/recognizers/prefix_shared_group.rs::mine` — Y.2 signature grouping; AC.6's `DispatchSignature` extends the shape.
- `parse-that::BumpSlab` — AC.9 extends with scratch API.
- `crates/ir/src/passes/regex_info.rs::analyze_with_cost_cached` — AC.15 promotes to per-compile cache.
- `crates/egraph/src/extract::Extractor::compute_best` — AC.4 replaces with `TopoExtractor`.
- `crates/core/src/generate/regex/emit/` — AC.10 + AC.11 + AC.12 extend the direct emission pipeline.
- `BBNF_EGRAPH_REPORT`, `BBNF_HIR_EGRAPH_REPORT` — extended by AC.0 with per-rule fire counts.

---

## Verification

Per-phase verification sequence (condensed; each phase commits with its own
samply capture + cargo expand diff + test run):

```bash
# AC.0 — Observability
BBNF_EGRAPH_REPORT=1 cargo bench -p bbnf --bench compile_pipeline 2>&1 | grep rule=
BBNF_PIPELINE_REPORT=1 cargo bench -p bbnf --bench compile_pipeline 2>&1 | grep compile_css_l4
cargo test -p bbnf-ir

# AC.1 — TypeDescId migration
cargo test -p bbnf-ir type_desc_interner
cargo test --workspace  # catches cascade failures
grep -rn "Option<TypeDesc>" crates/ir/src/passes/types/constraint/  # zero hits

# AC.2 — Analysis activation
cargo test -p bbnf-ir egraph_analysis
cargo test --manifest-path=parse-that/rust/regex/Cargo.toml egraph_analysis
grep -rn "NoAnalysis" crates/ir/src/egraph/ parse-that/rust/regex/src/egraph/  # zero hits

# AC.3 — Unified CostConfig
cargo build --workspace  # mechanical
cargo test --workspace

# AC.4 — TopoExtractor
cargo test -p egraph topo_extract
# Debug build cross-check against greedy oracle

# AC.5 — Extraction→CSP bridge
cargo test -p bbnf-ir extraction_bridge
cargo bench -p bbnf --bench css_l4 -- tailwind  # expected: ≥ −10%

# AC.6 — Cross-rule dispatch
cargo test -p bbnf cross_rule_dispatch
cargo expand -p bbnf --bench css_l4 2>&1 | grep 'static.*DISPATCH_GROUP'  # ≥ 1 hit

# AC.7 — TaggedUnion
cargo test --workspace
cargo expand -p bbnf --bench json_monolithic 2>&1 | grep -c "slab().alloc"  # ≤ 5
cargo expand -p bbnf --bench css_l4 2>&1 | grep -c "slab().alloc"  # ≤ 25
cargo bench -p bbnf --bench json_monolithic -- twitter  # expected: ≥ −8%

# AC.8 — Labeled blocks
cargo test -p bbnf no_iife_in_emitter
cargo expand -p bbnf --bench css_l4 2>&1 | grep -c "(|| {"  # zero
cargo bench -p bbnf --bench css_l4 -- tailwind  # expected: ≥ −6%

# AC.9 — Direct-to-slab
cargo test -p bbnf slab_scratch_discipline
cargo bench -p bbnf --bench json_monolithic -- canada  # expected: ≥ −10%
# Samply: RawVecInner::grow_amortized should drop off the top-20

# AC.10 — ClassMask SIMD
cargo test --target aarch64-apple-darwin -p parse_that class_membership
cargo test --target x86_64-unknown-linux-gnu -p parse_that class_membership
cargo bench -p bbnf --bench css_l4 -- tailwind  # expected: ≥ −9%

# AC.11 — Structural bitmap
cargo test -p bbnf structural_bitmap_roundtrip
cargo expand -p bbnf --bench json_monolithic 2>&1 | grep 'structural_bitmap_scan'
cargo bench -p bbnf --bench json_monolithic -- canada  # expected: ≥ −10% on top of AC.9

# AC.12 — Perfect hash
cargo test -p bbnf perfect_hash_dispatch
cargo expand -p bbnf --bench css_l4 2>&1 | grep -A 5 "fn __namedColor"
cargo bench -p bbnf --bench css_l4 -- tailwind  # expected: ≥ −6%

# AC.13 — RefMode
cargo test --workspace
cargo bench -p bbnf --bench json_monolithic  # expected: ≥ −1%

# AC.14 — RepeatMode
cargo test --workspace
cargo bench -p bbnf --bench css_l4 -- tailwind  # expected: ≥ −2%

# AC.15 — Incremental cache
cargo test -p bbnf incremental_compile
# LSP measurement via BBNF_LSP_COMPILE_TIMING

# AC.16 — Profile infra
cargo test -p bbnf profile_roundtrip

# AC.17 — post-AC baseline
cargo nextest run --workspace
cargo bench -p bbnf --bench compile_pipeline --bench json_monolithic --bench css_l4
# Capture samply profiles per existing methodology
# Write post-AC.json with every "+X %" citing a samply symbol
bash scripts/bootstrap-bbnf.sh  # idempotent
md5 -q crates/core/src/grammar/generated.rs  # record post-AC hash
grep -rn "NoAnalysis" crates/ir/src/egraph/ parse-that/rust/regex/src/egraph/  # zero
grep -rn "pub(super) fn collect" crates/ir/src/passes/recognizers/  # zero
grep -rn "AccelStrategy::ScalarLut" parse-that/  # zero
```

---

## Non-goals (deferred to Tranche AD)

- **Full joint-CSP over all variables + all e-graph class selections.**
  Plan 3's Phase 8 proposal. X.6 blew compile time 10× on a naive attempt;
  proper cost-aware decomposition requires Lagrangian relaxation inside
  csp-solver — research-grade and outside bbnf scope. 80 % of the win is
  captured by AC.4+AC.5+AC.6 (topo extract + bridge + cross-rule sharing).
- **Profile-guided cost feedback loop.** AC.16 lands the infrastructure
  (trace, profile file format, bridge consumer). Closing the loop — iterate
  compile → run → refit — is AD.
- **TS/WASM TaggedUnion full support.** AC.7 falls back to BoxedEnum on
  TS/WASM. AD brings these up to parity.
- **Multi-objective Pareto CSP.** AC.4 lands the lattice trait shape with
  scalar-only impls. Real Pareto extraction lives in AD.
- **pclmulqdq string-interior bitmap.** sonic-rs's quote-parity technique is
  JSON-string-specific. AC.11's structural bitmap covers the dispatch case;
  pclmulqdq quote parity is AD.
- **Tape representation for AST.** bbnf's typed AST is the differentiator;
  the tape throws away the type system. Explicitly out of scope.
- **`@lazy` directive for container skip.** Composes with AC.11's bitmap
  but adds a directive surface interacting with types. AD.
- **`@utf8` validation pre-pass.** JSON-specific, most bbnf grammars don't
  need it. AD.
- **`@input_size` directive** for CSP input-length hints. Current
  architecture uses grammar-level ratio heuristics. AD if profile data shows
  a gap.
- **Unified cross-tier super-e-graph.** Architecturally appealing but
  high-risk; AC.2's per-tier Analysis substrate already gives the fact-
  sharing benefit without unification cost.

---

## Open flags for verification during execution

1. **`EGraph::strings()` accessor** — AC.2's `Analysis::make` needs the
   `SharedStrings` pool to resolve `StringId → &str` in the grammar tier.
   Two options: (a) add `pub fn strings(&self) -> Option<&SharedStrings>` to
   `EGraph<N, A>`, (b) break the `Analysis::make` signature to take an extra
   `&SharedStrings` parameter. Option (a) is cleaner; option (b) requires
   rippling through the egraph crate's API. **Recommendation: (a)**, verify
   the existing `egraph.rs` can own an optional typed context without
   contaminating the HIR tier.

2. **Post-Z samply file format** — plan phases reference specific symbol
   percentages (`TypeDomain::join` at 2.4 %, `compute_best` at 6.1 %, etc.)
   from agent summaries. **Before AC.0 ships**, the orchestrator should
   manually record fresh post-Z profiles (or validate the existing ones
   under `docs/benchmarks/profiles/post-Z/`) to confirm the baseline numbers
   match.

3. **`SoftLambdaConstraint` in the B&B lower bound** — verified:
   `/Users/mkbabb/Programming/csc411/CSC411_HW2_ProgrammingQuestion/csp-solver/src/solver/optimize.rs:153-155`
   sums soft penalties into the objective cost;
   `optimize.rs:196-199` sums into the B&B lower bound. AC.5 + AC.6 work as
   designed.

4. **`@import` + lifetime handling for shared dispatch statics** — AC.6
   emits `static DISPATCH_GROUP_<hash>` at the top of the generated module.
   When the grammar uses `@import` with selective re-exports, the static
   must be module-local to the importing grammar, not shared across imports.
   Verify via `crates/core/tests/cross_rule_dispatch.rs` fixture that
   includes a multi-module grammar.

5. **Width lattice meet operation** — `EClassFacts::width: WidthBound`
   uses `meet` (intersection, narrows the bound) in `merge`, but other
   fields use `join` (union, widens). Mixing lattice directions is correct
   for monotone data but easy to get wrong. Dedicated test in
   `crates/ir/tests/egraph_analysis.rs::width_lattice_monotone`.

---

## Summary

Seventeen phases organized as five tracks. The substrate track (AC.0-AC.3)
activates dormant infrastructure. The joint-optimization track (AC.4-AC.6,
AC.13, AC.14) closes the extraction ↔ CSP feedback gap and produces the
first real cross-rule CSP constraint. The parse-cliff track (AC.7-AC.12)
kills the BoxedEnum slab cliff, the IIFE closure overhead, the scratch Vec
grow-amortized tail, the 9-64 byte SIMD gap, the structural-bitmap gap, and
the perfect-hash gap — every major parse-time cliff identified in the audit.
The compile-time track (AC.15-AC.16) adds LSP incremental compile and
profile-guided infrastructure. Verification (AC.17) ties it off with
profile-cited attribution.

**Cumulative targets**: parse-time wins of 25-45 % on the five tracked
benches; compile-time budget of ≤ 15 % aggregate regression;
architectural wins on every dormant substrate (Analysis<N>, cross-rule CSP,
soft-constraint bridge, Topological extraction, `ScalarLut` deletion,
`NoAnalysis` deletion). Every phase commits clean, with Y.13 extension,
samply attribution, and cargo-expand gate.

The tranche ships in dependency-DAG order. Tracks 1, 2, 3 can execute
partially in parallel once the substrate (AC.0-AC.3) lands. AC.17 ties off.
