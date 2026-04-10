# Tranche AA — Unified: Substrate Awakening, Parse Cliffs, Tape Transposition

## Context

Three prototype plans live in `docs/tranches/`:

- **`AA-prototype.md`** (336 lines) — tactical six-phase plan anchored on the
  surgical BoxedEnum→Enum rename, e-graph instrumentation, structural bitmap,
  and cross-rule CSP. Conservative scope. Deliberately refuses tape as
  "breaking AST redesign; explicitly out of scope".
- **`AA-prototype-2.md`** (1666 lines) — five-act bet-the-tranche plan that
  wakes the dormant substrate, lands independent parse wins, and then
  transposes the parser to emit a packed **tape** with a generated lazy
  `View<'tape>` surface. Explicit two-attempt fallback defers tape to "AC"
  if parity slips.
- **`AC-prototype.md`** (1637 lines) — seventeen-phase plan that activates
  substrate (TypeDescId interning, Analysis<N>, TopoExtractor, soft-constraint
  bridge, cross-rule dispatch sharing) and then chips at every parse cliff
  without the tape. Explicitly refuses tape: "bbnf's typed AST is the
  differentiator; the tape throws away the type system".

Tranche Z shipped five phases and deferred five (Z.7 closing baseline is
`6eeac0c`). We have no direct external consumers, so breaking changes are
warranted. Prettify emission is a hard constraint — anything that makes
prettify harder or less correct is a non-starter. Gorgeous can be entirely
rewritten — but ground truth says gorgeous is already decoupled from the
generated parser AST, so the rewrite is orthogonal to this tranche.

**Ground truth collected at HEAD (not inherited from audits):**

1. **BoxedEnum lives behind one chokepoint.** All slab allocation in the
   emitter flows through `crates/core/src/backend/rust/ir_types.rs::emit_alloc`
   (line 295) which produces `&*helper(state).slab().alloc(expr)`. 14 indirect
   call sites across `alt.rs`, `mod.rs`, `map_value.rs`, `grammar.rs`,
   `binary.rs`. `join_types` at
   `crates/ir/src/passes/types/constraint/helpers.rs:86-96` unconditionally
   returns `TypeDesc::BoxedEnum` for heterogeneous Alts, and
   `child_alloc` at `crates/core/src/backend/types/decisions.rs:40` turns that
   into `ValuePlacement::Alloc` without cardinality analysis.

2. **Prettify is already tape-friendly.** Generated prettify functions have
   signature `fn __<rule>_prettify<'a>(state: &mut ParserState<'a>, builder:
   &mut FmtBuilder<'a>) -> bool` — they walk the parser state directly and
   emit `::pprint::FmtBuilder` + `::pprint::FmtOp` operations. **They do not
   pattern-match on typed enum variants at all.** This is load-bearing: it
   means retargeting prettify to walk a tape cursor instead of a ParserState
   is a bounded, mechanical transposition, not a rewrite of the prettify
   semantic model. The prettify emitter lives at
   `crates/core/src/backend/rust/emitter/prettify/` (7 files, ~705 LOC).

3. **Gorgeous does not consume the generated parser AST.** `crates/gorgeous/`
   (~1349 LOC across 11 files) uses hand-written per-format AST types
   (`bbnf.rs`, `json.rs`, `css.rs`, `ebnf.rs`, `bnf.rs`, `google_sheets.rs`).
   Tape or eager, gorgeous is untouched. The user's "gorgeous can be
   rewritten" note is orthogonal — it would be rewritten as a downstream
   consumer of the tape View API once that API lands.

4. **Substrate is dormant, confirmed.** `Analysis<N>` is implemented at
   `crates/egraph/src/analysis.rs` but both tiers instantiate
   `EGraph<_, NoAnalysis>` (112 `NoAnalysis` hits workspace-wide).
   `Rewrite::should_apply` does not exist (zero hits).
   `csp_strategy/components.rs::UnionFind` documents itself as having zero
   cross-rule producers. The emitter currently has no `structural_bitmap`
   miner or kernel; `parse-that/.../scan/structural_bitmap.rs` does not exist.
   `type_desc.rs` is still a flat 30-line enum file — no `TypeInterner`
   exists workspace-wide. csp-solver is a `.cargo/config.toml` path patch, not
   a workspace member.

5. **Pipeline DAG anchor is intact.** `crates/core/src/pipeline/compile.rs:431`
   still builds the durable DAG exactly once with the `debug_assert!`
   invariant. Any pass ordering must respect this.

6. **Recognizer infrastructure is healthy.** 12 modules under
   `crates/ir/src/passes/recognizers/`, 50 `RecognizerShape` references.
   Z.0's single-walk mining is intact. The slot for `StructuralBitmap` and
   `PerfectHash` shapes is obvious — same miner trait, new shape variant,
   new consumer invariant entry.

---

## Critique: what each prototype got right and wrong

### AA-prototype — too tactical, substrate-blind

**Right:** AA.0 (e-graph fire-count instrumentation) is prerequisite. AA.1's
surgical BoxedEnum→Enum fix is the correct *producer-side* transposition —
the invariant `BoxedEnum` encodes is "alternation is heterogeneous", not
"caller needs indirection". AA.4's miner-to-emission ratio test
(`every_recognizer_shape_has_a_consumer_ratio`) is a strong invariant. AA.5's
type clone threading is free and correct.

**Wrong:** AA treats BoxedEnum as the *only* parse-time win worth its own
phase, then rapidly runs out of material. It does not touch `NoAnalysis`, the
`UnionFind` ghost, the cost-model dormancy, or the tape ceiling. It's a 2%-
ceiling plan in a world where the real cliff is 4×. AA.2's structural bitmap
is sketched in three paragraphs with no CSP wiring, no cost model, no SIMD
kernel design. It would not land as written.

**Kept:** AA.0 (observability), AA.1 (join_types fix, subsumed into the
broader TaggedUnion/TypeDescId work), AA.4 (miner ratio invariant), AA.5
(residual clone elimination, subsumed into TypeDescId migration).

### AA-prototype-2 — right about the ceiling, over-engineered in places

**Right:** The four hard truths in the context section are correct. The
architectural ceiling IS the typed AST materialization; substrate IS dormant;
M-series ARM SIMD IS a dead end for short-run scanners; tape IS the move.
The five-act structure with "independent parse wins land before the
breakthrough" is load-bearing — it delivers regardless of whether Act III
succeeds. The explicit two-attempt fallback for the tape parity gate is the
right safety property. The 24-byte tape record design, the chunked arena,
and the lazy view accessor shape are all fundamentally sound.

**Wrong:** The `Analysis<N>` Ctx GAT (AA.1) is over-engineered. A Generic
Associated Type on the trait forces every downstream generic bound to carry
it. A simpler transposition: thread `&GrammarCtx` as a plain reference
alongside the egraph, so `make(egraph, &ctx, node)`. No GAT, same
capabilities. Less virality. The `Tuple5` product-lattice boilerplate (AA.2)
is likewise over-engineered — we don't need generic tuple lattices; we need
one concrete `EClassFacts` struct. Write it directly. The B&B extractor
(AA.3) is the wrong algorithmic choice — it's NP-ish with a budget
escape hatch, whereas AC-prototype's topological SCC extractor is
linear-in-class-count and provably optimal on the DAG portion (which, post
structural normalization, is most of the graph). The AA-2 CSP cross-rule
work (AA.5) duplicates the extraction-bridge idea but wires it as direct CSP
constraints instead of AC's cleaner soft-constraint bridge via
`SoftLambdaConstraint`.

Also, AA-2 is quiet about the *consumer migration* for prettify. It lists
prettify as a migration target (~30-50 sites) but doesn't notice that
prettify's output shape is already state-walking, not AST-walking. This
changes the risk calculus: prettify migration is much smaller than AA-2
claims.

**Kept:** The five-act structure. The independent-parse-wins-first
commitment. Act III in full (with the findings above). The explicit
parity-gate fallback. The LOC reduction framing (ship more code than we
add, not less).

### AC-prototype — substrate is right, the refusal of tape is defeatism

**Right:** AC.1 (TypeDescId interning) is the correct foundation — it's a
keystone that unlocks AC.6 (dispatch sharing), AC.7 (TaggedUnion), and any
tape view generation (view kinds need stable type IDs). AC.2's Analysis
activation shape (single `GrammarAnalysis` struct with a concrete
`EClassFacts` payload, mirrored in HIR tier) is simpler and more correct
than AA-2's tuple-product approach. AC.4's TopoExtractor with Tarjan SCC +
layer parallelism is the correct extraction strategy. AC.5's
extraction→CSP soft-constraint bridge is elegant — it reuses csp-solver's
existing `SoftLambdaConstraint` and the B&B lower bound that already sums
soft penalties (verified in AC-prototype's Phase AC.5 motivation). AC.6's
dispatch-share signature via `DispatchSignature { first_set_hash, branch_count,
branch_type_id }` is the first real consumer of Y.5's dormant UnionFind.
AC.7 is correct about inline-storable TaggedUnion and the shape of the
emit path. AC.8 (IIFE → labeled blocks) is a surgical win that AA-2 misses
— the 1185 IIFE closures in CSS L4 are `?`/`return` containment scaffolding,
not borrow scoping, and Rust's stable labeled blocks (`'blk: { break 'blk
None }`) give the same semantics with zero closure overhead. AC.9 (direct-to-
slab scratch) correctly identifies that the `Vec → slab + truncate` double-
copy is the `RawVecInner::grow_amortized` hot stack — the feedback rule
"arena allocation must be singular" applies. AC.10 (ClassMask SIMD
for 9–64 byte exit sets) is the correct fix for the `scan_ident` 13%
self-time on css_tailwind, and deletes a dead code path
(`AccelStrategy::ScalarLut`). AC.15's incremental compile cache is the
correct move for LSP responsiveness.

**Wrong:** AC.17's non-goal list explicitly states "Tape representation for
AST. bbnf's typed AST is the differentiator; the tape throws away the type
system. Explicitly out of scope." This is defeatism, on two counts:

1. **The tape does NOT throw away the type system.** View types
   (`PairView<'tape>`, `ObjectView<'tape>`, `ValueView<'tape>`) preserve
   full typed accessors. The type system is richer, not poorer — the
   lifetime parameter makes the arena ownership explicit. The user's
   directive "breaking changes warranted, no direct consumers" removes
   AC's only remaining concern (external users migrating `'tape` lifetimes
   onto function signatures).
2. **Without the tape, AC tops out near AC's own target ceiling.** AC
   lists cumulative parse-time wins of 25-45% and says the honest ceiling
   for a grammar-agnostic Rust compiler is "~2.0-3.0 GB/s" on JSON. This is
   exactly the sonic-rs range, and AC refuses the architectural step that
   would reach it. The tape transposition IS the step that lifts us off
   the "every parse materializes a typed tree before any consumer sees it"
   ceiling that AA-prototype-2 correctly identified.

Also, AC.13/AC.14 (RefMode/RepeatMode CSP lifts) are symptomatic rather
than structural — they replace specific legacy heuristics with CSP variables,
which is the right direction, but the bigger architectural win from these is
included for free in the Act I substrate awakening (when Analysis facts are
live, the backend reads them directly, and the heuristic layer is redundant).

**Kept:** AC.1 (TypeDescId). AC.2 (Analysis activation; simpler concrete
shape). AC.3 (unified CostConfig). AC.4 (TopoExtractor). AC.5 (soft-constraint
bridge). AC.6 (cross-rule dispatch sharing). AC.7 (TaggedUnion narrowing).
AC.8 (IIFE → labeled blocks). AC.9 (direct-to-slab scratch). AC.10
(ClassMask SIMD). AC.11 (structural bitmap; fold with AA-2 AA.7). AC.12
(perfect-hash dispatch; fold with AA-2 AA.8). AC.15 (incremental compile
cache). **Rejected:** AC's refusal of tape, AC.13/AC.14 as standalone
phases (subsumed into Act I).

---

## Tape viability assessment — direct answer

**How viable is our tape approach, and total abrogation of the eager AST?**

**Verdict: highly viable. Much more so than AA-prototype-2 estimated, because
prettify is already tape-friendly at the codegen shape level.**

The load-bearing risk in the AA-2 plan was prettify consumer migration —
AA-2 estimated ~30-50 sites across the prettify subtree, framed as "the
biggest single migration in AA.13", and gated the whole Act III deletion on
a two-day CI green window for prettify parity. Ground truth makes this much
smaller:

- Generated prettify functions have signature
  `fn __<rule>_prettify(state: &mut ParserState, builder: &mut FmtBuilder)`.
  They walk the parser state directly, consulting the input buffer via
  `state.input[state.offset..]`, and emit `FmtBuilder` ops from grammar-level
  hint directives. **They do not own, match on, or decode any typed AST
  node.** This is because prettify is fused parse-and-format — it runs
  during the parse, not after it.
- Under the tape transposition, the prettify emitter produces
  `fn __<rule>_prettify(cursor: &mut TapeCursor, builder: &mut FmtBuilder)`.
  The state-walking becomes tape-walking; the rest is unchanged. The
  `FmtBuilder` path is identical. The hint directive plumbing is identical.
- The migration is ~700 LOC of codegen siblings, all under one directory.
  Most of the migration is mechanical (`state.consume(b'x')` becomes
  `cursor.expect_kind(TapeKind::LitX)`), with the tricky parts being where
  prettify emits `FmtOp::SourceRange(span)` — we preserve source ranges on
  the tape as `span_lo`/`span_hi` fields on every record, same as
  today's Span representation.

With prettify derisked, the remaining Act III blockers are:

- **LSP** (`crates/lsp/src/state/diagnostics/ir_analysis.rs`): consumes IR
  meta, not parser output. Untouched.
- **DAP** (`crates/lsp/src/dap/`): translates IR positions, not parser
  output. Untouched.
- **bbnf-ser serializer**
  (`crates/core/src/generate/serialize/serialize.rs`): consumes typed
  projection to emit `impl Serializer for <UserType>`. Migrates from owned-
  field access (`obj.field`) to view accessors (`obj.field()`). ~10 sites,
  all in one generator file. Mechanical.
- **bbnf-derive `#[derive(Parser)]`**: produces the public
  `Parser::parse(&input) -> Result<Output, ParseErr>` surface. With the
  tape, this becomes `Parser::parse(&input) -> Result<RootView<'input>,
  ParseErr>`. The tape lifetime ties to the input buffer's bumpalo arena,
  which the derive macro already manages.
- **Gorgeous**: decoupled. Untouched in this tranche.

The parity test shape is straightforward: compile every production grammar
(JSON, CSS L4, BBNF, Sheets, EBNF) in BOTH eager and tape mode, run 20+
sample inputs per grammar, assert accessor-by-accessor equality between the
eager AST and the tape View. Because the accessor names and types are
*generated from the same IR*, they're identical in both modes. This is a
strong correctness invariant with mechanical test scaffolding.

**Abrogation of the eager AST**: yes, do it. Once the tape emitter has parity
on all production grammars for two CI days, delete
`crates/core/src/backend/rust/emitter/` entirely (~5000 LOC, the bulk of the
eager emitter). Keep the IrNode-walking `backend/driver/` layer and the
`backend/patterns/` layer — they're shared between tape and eager in spirit.
The `ir_types.rs::emit_alloc` chokepoint becomes a dead code path; delete it
in the same sweep. BumpSlab's remaining consumer is the tape's ChunkedArena
itself — simplification.

**The single concern worth flagging**: the tape's `'tape` lifetime is real.
If a user wants to collect parsed values into an owned `Vec`, they have to
explicitly materialize (via an `into_owned()` accessor we generate). For
internal consumers (LSP, DAP, tests, benches), this is free — everything
lives inside one bumpalo arena per parse. For external users, the lifetime
is a real constraint but (a) the user says there are no external consumers,
(b) modern Rust users are familiar with `'a` parameters, and (c) the typed
accessor surface is unchanged — it just returns `T<'tape>` instead of `T`.
The documentation surface is one new lifetime parameter, nothing more.

**Novel transpositions the prototypes didn't name:**

1. **Tape IS the debug substrate.** The debug-infra memory notes DWARF-like
   trace records for `@debug`, source maps, and interpreter hooks. A
   `TapeKind::DebugEvent` record variant unifies these with the parser's
   output tape. One substrate, two modes (production tape without events,
   debug tape with events). Trace emission is free (just another push).
2. **Fused parse+format becomes parse→tape→format.** Today's fused
   prettify fuses parse and emit-FmtBuilder into one hot loop for 87×
   speed. Under the tape, this becomes "parse writes tape; post-walk
   produces FmtBuilder from tape". Because the tape is contiguous and
   cache-friendly, and because the post-walk is a single pass, throughput
   matches the current fused path while architecturally decoupling parse
   from format. Prettify's hint directive plumbing survives unchanged.
3. **Tape kinds are derived from IrNode kinds.** The current `IrNode`
   enum at `crates/ir/src/ast.rs` is the source of truth for
   grammar-level node shapes. `TapeKind` is a codegen-time projection of
   `IrNode` (one `TapeKind::<kind>` per distinct emitted shape). We
   generate the `TapeKind` enum from the grammar's mined recognizer
   decisions, so a grammar with N distinct node shapes gets N tape kinds.
   This tightens the "no ghost substrate" invariant: every tape kind has
   a producer (an emitter) and consumers (one view accessor + one prettify
   walker).
4. **TaggedUnion narrowing COMPOSES with the tape**, it is not a
   stopgap. AC framed TaggedUnion as the replacement for BoxedEnum;
   AA-2 framed it as a "stopgap that survives Act III" for type-system
   precision. The truth is stronger: TaggedUnion narrows the View
   discriminant from `u32` (tape record offset) down to the variant tag
   directly, for small heterogeneous unions where all variants are
   inline-storable. `PairView<'tape>.value()` returning `ValueView<'tape>`
   where `ValueView` is a narrow `TaggedUnion` is cheaper than returning
   a generic tape reference — the variant tag lives in the flags byte.
   Keep TaggedUnion; it is load-bearing under the tape.
5. **Abrogate BumpSlab entirely.** Once tape is the only output format,
   `parse_that::BumpSlab` has exactly one internal consumer: the tape's
   chunked arena. `ChunkedArena<TapeRec>` is strictly simpler than BumpSlab
   (no type parameter, no per-T segment indirection, no RefCell borrow
   tracking). Delete BumpSlab; promote `ChunkedArena<T>` into parse_that
   as the one-and-only arena type. This is a net LOC reduction of several
   hundred lines and a simpler mental model for the runtime.

---

## Architectural commitments

1. **No legacy code, no workarounds, no fallback shims** — with one
   bounded exception: `BBNF_BACKEND_MODE=tape|eager` env var exists as
   iteration scaffolding during AA.14–AA.15 and is deleted in AA.16
   alongside the eager emitter. Not a safety property; not an escape
   hatch for the post-tranche state.
2. **Truth-based attribution.** Every "+X%" claim in `post-AA.json` cites a
   samply profile symbol + self-time delta from a *fresh* post-AA profile.
   No inherited numbers. Phases that can't prove a win are re-opened.
3. **Substrate before parse wins before tape.** Act I lays substrate Acts
   II–III consume. Act II wins land *before* Act III touches the parser, so
   the tranche has a non-zero parse delivery even if Act III slips.
4. **Compile-time budget is tight.** `compile_bbnf ≤ 1.5×`,
   `compile_css_l4 ≤ 2.0×` pre-AA. LSP felt-perf is non-negotiable.
5. **Every new substrate has a load-bearing consumer in the same commit.**
   Y.13's consumer-invariant test extends to every new variant / analysis
   fact / cross-rule constraint / view kind.
6. **Cross-tier symmetry.** Grammar tier and HIR tier ship mirrored
   substrate changes in the same commit. `NoAnalysis` vanishes from both
   tiers simultaneously.
7. **Profiling and testing are inside each phase.** `cargo expand` diff +
   `samply` symbol delta + `cargo test --workspace` per phase. No phase
   ships without all three artifacts.
8. **DAG invariant preserved.** The durable DAG built once at
   `crates/core/src/pipeline/compile.rs:431` stays load-bearing. Any
   substrate that needs pre-DAG state uses the existing accessors, not a
   parallel structure.
9. **Prettify emission is a hard constraint.** Any phase that breaks or
   degrades the `@pretty` path is reopened. Prettify parity tests run
   for every production grammar in every phase that touches the emitter.
10. **Correctness first, performance second.** The user said: "optimize
    every facility, major AND minor, for correctness". This means:
    reviving dead code paths (`AccelStrategy::ScalarLut`), eliminating
    ghost substrate (`NoAnalysis`, dormant UnionFind), fixing dual-copy
    collection strategies (Vec → slab), fixing producer-encoded decisions
    at the consumer (BoxedEnum), and preserving Y.13 invariants across
    every new variant.

### Parse-time floor / target gates

| Gate              | Floor (hard)       | Target              | Dominant phase |
|-------------------|--------------------|---------------------|----------------|
| `json_canada`     | ≥ 2.0 GB/s (+62%)  | ≥ 3.0 GB/s (+144%)  | Acts II + III  |
| `json_twitter`    | ≥ 2.1 GB/s (+38%)  | ≥ 2.8 GB/s (+85%)   | Acts II + III  |
| `json_citm`       | ≥ 2.4 GB/s (+27%)  | ≥ 3.0 GB/s (+58%)   | Acts II + III  |
| `css_tailwind`    | ≥ 0.36 GB/s (+41%) | ≥ 0.55 GB/s (+115%) | Acts II + III  |
| `css_bootstrap`   | ≥ 0.32 GB/s (+30%) | ≥ 0.45 GB/s (+85%)  | Act II         |
| `compile_bbnf`    | ≤ 1.5× pre-AA      | ≤ 1.3× pre-AA       | Act I budgets  |
| `compile_css_l4`  | ≤ 2.0× pre-AA      | ≤ 1.6× pre-AA       | Act I budgets  |

**Tape-or-bust.** Floor gates on JSON benches require Act III. Act II
alone meets the CSS floor gates but falls short on JSON by ~0.4 GB/s.
Under the tape-or-bust commitment, Act III landing is a hard gate for
tranche close — there is no "defer to AC" path. Target gates require
the full Act III + IV composition.

---

## Phase plan — 20 phases across 5 acts

### Act 0 — Observability prelude

Runs before any code change. Captures fresh pre-AA profiles because post-Z
samply profiles are sparse (4–7k samples) and unfit for symbol-level
attribution.

#### Phase AA.0 — Fresh profiles + per-rule fire counts + pipeline timing

**Motivation.** Every downstream phase needs samply deltas and per-pass
compile timings for attribution. Today `BBNF_EGRAPH_REPORT=1` prints a
saturation summary but not per-rule fire counts; CSP budget exhaustions are
silent; per-pass wall clock in `pipeline/compile.rs` is unmeasured.

**Files (modified).**
- `crates/egraph/src/scheduler.rs`, `csp_scheduler.rs` — per-rule `applied`
  counter on `RunReport`.
- `crates/ir/src/egraph/mod.rs:99-113` — extend `BBNF_EGRAPH_REPORT=1` with
  per-rule fire counts.
- `parse-that/rust/regex/src/egraph/mod.rs` — mirror; `BBNF_HIR_EGRAPH_REPORT=1`.
- `crates/ir/src/passes/csp_strategy/mod.rs` — `BBNF_CSP_REPORT=1` logs
  components hitting budget.
- `crates/core/src/pipeline/compile.rs` — wrap each pipeline op in a timed
  scope; `BBNF_PIPELINE_REPORT=1` emits CSV per compile.

**Pre-tranche profile capture**:
```bash
# Full sweep — single-invocation per bench binary (Tranche Z invariant)
cargo bench -p bbnf --bench compile_pipeline --no-run
cargo bench -p bbnf --bench json_monolithic  --no-run
cargo bench -p bbnf --bench css_l4            --no-run
# Resolve + record via samply as per docs/tranches/Z.md methodology
# Land at docs/benchmarks/profiles/pre-AA/*.samply + *.syms.json (≥50k samples each)
# Capture cargo-expand snapshots for json_monolithic + css_l4 → docs/benchmarks/expand/pre-AA/
```

**Gate.** Every pre-AA profile has ≥50k samples. Cargo-expand snapshots
compile. `BBNF_EGRAPH_REPORT=1 | grep rule=` prints non-zero counts.
`BBNF_PIPELINE_REPORT=1 | grep compile_css_l4` prints a 16+-row CSV.

**Risk.** 1/5. Pure observability.

---

### Act I — Substrate awakening (Phases AA.1–AA.6)

Wake the dormant optimizer before any consumer reads from it. This act
lays the foundation every subsequent act consumes.

#### Phase AA.1 — `TypeDescId` interning + `LatticeDomain<TypeDescIdDomain>`

Keystone phase from AC.1. Interns `TypeDesc` into a `u32` `TypeDescId`,
collapses AC-3 lattice joins from `Option<TypeDesc>` clones to `Copy`
compares, and unblocks TaggedUnion (Phase AA.9), cross-rule dispatch
sharing (Phase AA.5), and tape view generation (Phase AA.12). The
`type_desc.rs` flat file is promoted to a directory module.

**Files (new).**
- `crates/ir/src/types/type_desc/mod.rs` — directory module (replaces the
  30-line flat file).
- `crates/ir/src/types/type_desc/id.rs` — `TypeDescId(u32)` +
  `TypeDescInterner` (`Vec<TypeDesc>` + `FxHashMap<TypeDesc, TypeDescId>`).
- `crates/ir/src/types/type_desc/lattice.rs` — `TypeDescIdDomain` (`Copy`).
- `crates/ir/tests/type_desc_interner.rs`.

**Files (modified, same commit — no partial migration).**
- `crates/ir/src/types/type_desc.rs` — DELETE (promoted to directory).
- `crates/ir/src/types/grammar.rs` — `GrammarIR::type_desc_interner` field;
  per-rule `types: Vec<TypeDescId>`; MessagePack ser round-trip.
- `crates/ir/src/passes/types/constraint/{alt,seq,grounds,operators,helpers}.rs`
  — every `Option<TypeDesc>` → `Option<TypeDescId>`.
- `crates/core/src/backend/driver/{mod,seq,repeat,alt,wrap}.rs` —
  `DriverState::resolve_type(&self, id: TypeDescId) -> &TypeDesc`; borrow-
  returns replace the 5 Y.10-missed clone sites (subsumes AA-prototype AA.5).
- `crates/core/src/backend/rust/ir_types.rs`, `emitter/*.rs`,
  `ts/helpers.rs`, `wasm/emitter/*.rs` — route every direct `TypeDesc`
  access through `resolve_type`.
- `crates/core/src/generate/serialize/serialize.rs:17-19` — reference-type
  classification via `resolve_type`.

**Consumer invariant.** Grep gate:
`grep -rn "Option<TypeDesc>" crates/ir/src/passes/types/constraint/` → 0 hits.
Y.13 extension: `type_desc_id_consumer(id: TypeDescId) -> &'static str`.

**Profile target.** `compile_css_l4::project_types` self-time ≥ −5 to −8%.
`compile_bbnf` ≥ −3 to −5%.

**Risk.** 4/5. Wide migration, ~340 references across ~40 files. No partial
migration — single commit, workspace test gate.

#### Phase AA.2 — Vendor `csp-solver` into workspace + activate `Analysis<N>` (grammar + HIR)

Two coupled moves that wake the rest of the substrate.

**(a) Vendor csp-solver.** The sibling-repo patch at
`.cargo/config.toml` is deleted; `crates/csp-solver/` lands as a workspace
member. Subsequent phases will add new constraint types here; crossing the
repo boundary is architectural drag.

**(b) Activate `Analysis<GrammarENode>` + `Analysis<HirENode>`.**
Simpler shape than AA-prototype-2's GAT-based `Ctx` substrate: write one
concrete `GrammarAnalysis` struct with a concrete `EClassFacts` payload.
Mirror in HIR tier as `HirAnalysis`/`HirEClassFacts`. Replace
`NoAnalysis` in both tiers. Thread `&GrammarCtx` (holds `SharedStrings`,
SCC, DAG) as an explicit parameter alongside the egraph reference —
plain argument, no GAT.

**Files (new, grammar tier).**
- `crates/ir/src/egraph/analysis/mod.rs` — `GrammarAnalysis` +
  `impl Analysis<GrammarENode>`.
- `crates/ir/src/egraph/analysis/facts.rs` — `EClassFacts { first_set,
  nullable, width, anchored_left, dispatch_eligible, regex_sid,
  literal_sid, cardinality, structural_class_id, alloc_ctx }`.
- `crates/ir/src/egraph/analysis/merge.rs` — monotone lattice joins.
- `crates/ir/tests/egraph_analysis.rs` — round-trip + monotone idempotence
  + width lattice direction test + `every_fact_has_a_consumer` exhaustive
  match.

**Files (new, HIR tier mirror).**
- `parse-that/rust/regex/src/egraph/analysis/{mod,facts}.rs` — `HirAnalysis`
  over `ByteSet256` + `is_literal_run` + `is_anchored`.

**Files (modified).**
- `crates/egraph/src/egraph.rs` — add
  `pub fn strings(&self) -> Option<&SharedStrings>` for `Analysis::make` to
  resolve `StringId → &str`.
- `crates/ir/src/egraph/mod.rs:53-99` — `EGraph<GrammarENode, NoAnalysis>`
  → `EGraph<GrammarENode, GrammarAnalysis>`.
- `crates/ir/src/egraph/rules/{regex,suffix}.rs` — replace
  `class.iter().find_map(…)` groveling with `egraph.class(id).data.*`
  lookups.
- `parse-that/rust/regex/src/egraph/mod.rs:56` + rules — mirror.
- `Cargo.toml` (workspace root) — add `crates/csp-solver` member.
- `.cargo/config.toml` — drop the csp-solver path patch.

**Imperative passes stay in parallel for one phase.** `compute_first_sets`,
`compute_follow_sets`, `compute_nullable`, `refine_span_eligibility` keep
running alongside the new analyses through AA.4, to enable parity tests.
Deleted in AA.4.

**Consumer invariant.** `BBNF_ANALYSIS_DIFF=1` runs both paths and asserts
agreement at every rule. `grep NoAnalysis crates/ir/src/egraph/
parse-that/rust/regex/src/egraph/` → 0 hits.

**Profile target.** `compile_css_l4` e-graph rule search time halved
(4.2% → ≤2% inclusive). Net compile time +15 to +25% (double-running —
ceiling removed in AA.4).

**Risk.** 4/5. Parity is the only safety net; a wrong `make` or non-monotone
`merge` propagates to every rule. Mitigation: property test on monotone
idempotence, fixture grammars covering each variant, golden IR-tree hash
before/after swap.

#### Phase AA.3 — Unified `CostConfig` + `TopoExtractor` + Pareto lattice trait

Merges AC.3 + AC.4.

**(a) CostConfig directory module.** Promote `crates/egraph/src/cost_config.rs`
to a directory with explicit sub-structs: `extraction.rs`, `strategy.rs`
(hoists the magic `Vec<f64>` from `csp_strategy/mod.rs`), `bitmap.rs`,
`perfect_hash.rs`, `scheduler.rs`. Every knob settable via `BBNF_COST_*`
env var.

**(b) TopoExtractor.** Replace the O(classes² · iterations)
`Extractor::compute_best` with Tarjan SCC + Kahn topological sort + bounded
iterative widening within SCCs. `Cost: Lattice` trait unblocks multi-
objective later without collapsing to scalar now (wrap existing `f64` in
`Scalar<f64>` newtype — zero behavioral diff). Debug build retains the
greedy extractor as a cross-check oracle.

**Files (new).**
- `crates/egraph/src/cost_config/{mod,extraction,strategy,bitmap,perfect_hash,scheduler}.rs`
- `crates/egraph/src/extract/{mod,greedy,topo,scc,lattice}.rs`
- `crates/egraph/tests/topo_extract.rs`

**Files (modified / deleted).**
- `crates/egraph/src/cost_config.rs` — DELETE.
- `crates/egraph/src/extract.rs` — DELETE (moved into `extract/`).
- `crates/ir/src/egraph/write_back.rs:39` — `Extractor::new` →
  `TopoExtractor::new`.
- `crates/ir/src/egraph/cost.rs`, `parse-that/rust/regex/src/egraph/cost.rs`
  — read from `cfg.extraction.*`.
- `crates/ir/src/passes/csp_strategy/mod.rs` — domain builders read from
  `cfg.strategy.*` instead of hard-coded magic numbers.

**Profile target.** `compile_css_l4` extraction self-time −3 to −8%
single-threaded, −8 to −14% with rayon layer parallelism.

**Risk.** 2/5. Tarjan SCC is standard; the delicate part is multi-class
SCC widening, guarded by debug cross-check.

#### Phase AA.4 — Pipeline reorder + delete imperative passes

Analyses are at parity. Delete the imperative passes, retain the e-graph
across compile so the backend reads analyses directly, and reorder the
pipeline.

**Files deleted.**
- `crates/ir/src/passes/sets/{first_sets,follow}.rs` (~943 LOC).
- `crates/ir/src/passes/span.rs::refine_span_eligibility`.
- Relevant `pub use` re-exports.

**Files modified.**
- `crates/core/src/pipeline/compile.rs` — new ordering: lower IR → SCC →
  `inline_acyclic` (stays deterministic, op 3) → build e-graph → saturate
  (analyses converge) → extract canonical IR → recognize patterns → emit
  code. The e-graph persists through `ir.egraph: Option<GrammarEGraph>`
  into the backend.
- `crates/core/tests/pipeline_no_imperative_passes.rs` (new) — grep test
  asserting the deleted symbols are gone.

**Profile target.** `compile_css_l4` −10 to −20% from AA.2's double-running
ceiling. Parse benches unchanged.

**Risk.** 4/5. HIGH. Deleting load-bearing passes. Mitigation: AA.2 + AA.3
ran analyses in parallel; parity has been green.

#### Phase AA.5 — Cross-rule CSP + extraction → soft-constraint bridge + conditional rewrites

Three coupled substrate moves that wake the last dormant pieces.

**(a) `Rewrite::should_apply`.** Add predicate guard to `Rewrite` trait;
migrate 5 grammar-tier + 5 HIR-tier rules to query analyses where it helps.
Default returns `true`.

**(b) Extraction → CSP soft-constraint bridge** (from AC.5). After
`write_back_optimized`, derive
`ExtractionAdviceMap : FxHashMap<NodeId, SoftConstraintAdvice>` from the
saturated egraph's `EClassFacts`. The strategy CSP installs
`SoftLambdaConstraint`s before solving — csp-solver already sums soft
penalties into the B&B objective and lower bound. No csp-solver extension
needed. Advice variants: `ForceAltByteDispatch`, `PreferPerfectHash`,
`PreferOnePassEngine`, `PreferBalancedScan`. Unary biases for simple
cases; soft lambdas for cross-variable advice.

**(c) Cross-rule CSP topology.** Wake Y.5's dormant
`csp_strategy/components.rs::UnionFind`. Refactor `solve_strategy_decisions`
to collect all sites across all rules into one CSP, decompose into
independent components via UnionFind, solve each component. Three new
cross-rule constraints:

- `SccConsistentDispatchConstraint` — rules in the same SCC use compatible
  AltMode.
- `AllocPropagationConstraint` — `EClassFacts::alloc_ctx` propagates
  through `Ref` edges across rule boundaries.
- `DispatchShareConstraint` — two rules with structurally-identical Alt
  signatures (`DispatchSignature { first_set_hash, branch_count,
  branch_type_id }`) share one dispatch table (the `static` is hoisted to
  module scope). Reference equality on `TypeDescId` (from AA.1) gives free
  structural-type equality.

**Files (new).**
- `crates/ir/src/egraph/extraction_bridge/{mod,facts_to_soft}.rs`.
- `crates/ir/src/passes/csp_strategy/{signature,cross_rule}.rs`.
- `crates/ir/src/passes/csp_strategy/constraints/{scc_consistent_dispatch,
  alloc_propagation,dispatch_share}.rs`.
- `crates/csp-solver/src/constraint/{symmetry,cross_rule}.rs` — new
  constraint shapes.
- `crates/ir/tests/{csp_cross_rule,extraction_bridge,csp_grammar_wide_equiv}.rs`.
- `crates/core/tests/cross_rule_dispatch.rs` — end-to-end codegen test.

**Files (modified).**
- `crates/egraph/src/rewrite.rs` — `should_apply` predicate on `Rewrite`.
- `crates/ir/src/passes/csp_strategy/mod.rs` — grammar-wide solve via
  components.
- `crates/core/src/backend/rust/emitter/dispatch.rs` — emit shared
  `static DISPATCH_GROUP_<hash>` at module scope.

**Hard gate.** Phase does not ship unless the grouping mines ≥1 group on
`compile_css_l4`. Otherwise it's ghost substrate (Y.13 violation).

**Consumer invariant.** Y.13 extension: exhaustive matches on
`SoftConstraintAdvice`, `DispatchSignature`, and each new constraint.
`BBNF_CSP_REPORT=1` zero budget exhaustions on standard benches.

**Profile target.** `css_tailwind` parse ≥ −10% (soft-constraint bridge
alone). Generated `css_l4.rs` dispatch-table LOC ≥ −5 to −10%.
Compile time ≤ +5%.

**Risk.** 4/5. Cross-rule CSP can make satisfiable problems unsatisfiable;
csp-solver's node budget is the safety net. False-positive sharing is a
correctness bug; mitigated by double-hash `first_set`, reference-equality
`TypeDescId` check, 1000-random-Alt fuzzer test.

#### Phase AA.6 — Analysis-gated e-graph rules (the safe ones)

Add rewrites the new cost model + analyses make safe. Each is a small,
predicate-guarded rewrite. `CrossRuleInline` is explicitly NOT in scope
— `inline_acyclic` stays deterministic at op 3 of the pipeline so LSP
incremental analysis survives.

**New rules.**
1. `LookaheadPushdown` — `Next(Alt([A,B,C]), L)` →
   `Alt([Next(A,L), Next(B,L), Next(C,L)])` when
   `FOLLOW(children) ∩ L = ∅`.
2. `SequenceFactoringLeft` — generalization of existing
   `factor_literal_prefixes` to non-literal cheap prefixes.
3. `SequenceFactoringRight` — dual.
4. `RepeatDistribution` (HIR) — `Repetition(Alternation([A,B]))` →
   `Alternation([Repetition(A), Repetition(B)])` when runs are independent.
5. `AltSplitByDispatchByte` — promotes a recognizer pass into the e-graph
   for alternative formulation.

**Files (new).**
- `crates/ir/src/egraph/rules/{lookahead,factor_left,factor_right,alt_split_byte}.rs`
- `parse-that/rust/regex/src/egraph/rules/{repeat_distribution,factor_left,factor_right}.rs`

**Profile target.** Parse: `json_canada` +2 to +4%, `css_tailwind` +1 to +2%.
Compile: +2 to +4%.

**Risk.** 3/5. Each rule is a potential correctness bug. Per-rule regression
tests + the AA.2 parity test catches lattice violations.

---

### Act II — Independent parse wins (Phases AA.7–AA.12)

Six phases that pay off regardless of whether Act III succeeds. Each
consumes Act I substrate. These are the wins that meet the floor gates
even if the tape is deferred.

#### Phase AA.7 — `TypeDesc::TaggedUnion` (the BoxedEnum killer)

From AC.7, hardened with the ground-truth insight that `emit_alloc` is the
single chokepoint.

Introduce `TypeDesc::TaggedUnion(TaggedUnionId)` via a new
`TaggedUnionInterner` on `GrammarIR`. Small-N (≤8) inline-storable
heterogeneous unions compile to a flat enum-discriminant + one-word payload,
no allocation. `join_types` is rewritten to prefer `TaggedUnion` when
possible and fall back to `BoxedEnum` only at >8 variants or when any
variant is not inline-storable. `child_alloc` routes `TaggedUnion` to
`ValuePlacement::Inline` unconditionally (interner guarantees all variants
inline-storable).

**Files (new).**
- `crates/ir/src/types/tagged_union.rs` — `TaggedUnionId`, `TaggedUnionDesc`,
  `TaggedUnionInterner`.
- `crates/core/src/backend/rust/emitter/tagged_union_emit.rs` — inline enum
  emission + wrap expressions.
- `crates/ir/tests/tagged_union.rs`.

**Files (modified).**
- `crates/ir/src/types/type_desc/mod.rs` — `TypeDesc::TaggedUnion(TaggedUnionId)`.
- `crates/ir/src/types/grammar.rs` — `GrammarIR::tagged_unions`.
- `crates/ir/src/passes/types/constraint/helpers.rs:86-96` — rewrite
  `join_types` per AC.7 shape.
- `crates/ir/src/passes/types/utils.rs` — `is_inline_storable` predicate.
- `crates/core/src/backend/types/decisions.rs:38-45` — `TaggedUnion` arm.
- `crates/core/src/backend/rust/ir_types.rs::emit_alloc` — bypass slab
  when the producing type is TaggedUnion + Inline. **This is the load-
  bearing single-point change** — 14 indirect emitter call sites
  automatically take the new path.
- `crates/core/src/backend/rust/ir_enums.rs` — emit one `#[derive(Copy,
  Clone)]` struct per `TaggedUnionId`.
- `crates/core/src/backend/rust/emitter/alt.rs:30-76` — `coerce_branch`
  TaggedUnion arm.
- `crates/core/src/generate/serialize/serialize.rs:17-19` — TaggedUnion is
  a value type when placement is Inline.

**Consumer invariant.** Y.13 extension. Grep gates:
- `cargo expand json_monolithic | grep -c "slab().alloc"` ≤ 5 (was ~150).
- `cargo expand css_l4         | grep -c "slab().alloc"` ≤ 25.

**Profile target.** `slab::alloc` + `mi_segment_span_allocate` self-time on
`json_canada` 8% → <1.5%. `json_twitter` parse ≥ −8 to −12%. `css_tailwind`
≥ −12 to −18%.

**Risk.** 4/5. Wide blast radius through every backend + serializer + Y.13.
Mitigation: Rust emitter lands with full TaggedUnion support in the same
commit; TS/WASM emitters get a `TaggedUnion → BoxedEnum` fallback in the
same commit (their common case was already BoxedEnum). Full TS/WASM
TaggedUnion support lands in a follow-up tranche.

#### Phase AA.8 — IIFE → labeled blocks

Pure codegen lowering from AC.8. The ~1185 `(|| { ... })()` closures in
CSS L4 generated code are `?`/`return` containment scaffolding, per the
explicit comment at `emitter/alt.rs:271`. LLVM inlines these ~99% of the
time but punts on deeply nested seq groups with `&mut state` captures.
Rust's stable labeled blocks (`'blk: { if cond { break 'blk None; }
Some(x) }`) give identical early-exit semantics with zero closure overhead.

**Files (new).**
- `crates/core/src/backend/rust/emitter/control_flow.rs` — labeled-block
  + `try_op` helpers.
- `crates/core/src/backend/rust/emitter/scratch_guard.rs` — `ScratchGuard<'a,
  T>` with Drop-based truncation (labeled blocks don't defer). Set
  `guard.commit = true` before the happy-path return.
- `crates/core/tests/no_iife_in_emitter.rs` — grep gate.

**Files (modified).** Every IIFE site:
- `seq.rs:67-73, 85-91, 108-112`
- `repeat.rs:195-239` — `'rpt_blk:`
- `binary.rs:72-77, 87-91`
- `alt.rs:183-187, 204-208, 274-285`
- `dispatch.rs:46`, `leaves.rs:105`, `map_value.rs:195`,
  `operator_chain.rs:46`, `grammar.rs:69`

**Consumer invariant.** `tests/no_iife_in_emitter.rs`: grep expanded cargo
output for `(|| {` → zero hits. Differential fuzz: pre/post parsers on 10k
inputs per grammar, assert identical ASTs.

**Profile target.** Combined `__declaration` + `__value` + `__namedColor`
self-time −8 to −12% (LLVM inlines labeled blocks more aggressively).
`css_tailwind` parse −6 to −9%. Generated `css_l4.rs` size −15 to −20%.

**Risk.** 2/5. Mechanical but wide. Every `return` inside an IIFE must
become `break 'label`. Grep gate + differential fuzz catch misses.

#### Phase AA.9 — Direct-to-slab scratch

From AC.9. Replaces the `Vec<T>::with_capacity(64) → grow_amortized → slab
copy → Vec::truncate` double-copy path with direct bump-append into a slab
scratch region. One copy (the push). Zero growth stalls.

**Files (modified).**
- `parse-that/rust/parse_that/src/bump_slab.rs` — add `ScratchMark`,
  `scratch_begin`/`push`/`finalize`/`rewind`. Finalize returns a
  `&[T]` directly from the scratch region (no copy) when the region fits
  in one chunk; copies into a fresh run on rare chunk-spill (cold path).
- `crates/core/src/backend/rust/alloc_emit.rs` — replace entire scratch
  API. Delete `AllocCtx::scratch_types` Vec fields.
- `crates/core/src/backend/rust/emitter/repeat.rs:183-241` — loop body
  unchanged; `#collect_expr` becomes `__slab.scratch_finalize::<#ty>(#mark)`.
- `crates/core/src/backend/rust/emitter/seq.rs:50-97` — flatten paths use
  the same API.

**Nested scratch discipline.** Debug assertion enforces sequential nesting:
`debug_assert!(mark.chunk_idx == current_chunk_idx, "interleaved scratch")`.

**Consumer invariant.** `tests/slab_scratch_discipline.rs` nested Repeat
fuzz + `tests/no_vec_scratch.rs` grep gate (`Vec<` does not appear in
AllocCtx field definitions of generated code).

**Profile target.** `RawVecInner::grow_amortized` on `json_canada`
6% → <0.5%. `Vec::truncate` 2% → 0%. `json_canada` parse ≥ −10 to −14%.
Peak memory ≥ −8 to −15%.

**Risk.** 3/5. Chunk spill mid-scratch needs the copy fallback; must be
cold. Pre-size slab at parse entry based on `input.len() * K`.

#### Phase AA.10 — `ClassMask` SIMD for 9–64 byte exit sets + delete `ScalarLut` dead code

From AC.10. `AccelStrategy::ScalarLut` at `parse-that/rust/regex/src/automata/
accel.rs:29` is dead code (zero emitter consumers). The 9-64 exit-byte range
(CSS ident's 52-byte continuation set) currently falls through to scalar
`scan_ident` — 13% self-time on `css_tailwind`. Fix with one shared SIMD
class-membership kernel used by both scan helpers and the regex emit path.

**Technique.** Nibble-popcount LUT (low-nibble LUT × high-nibble LUT,
ANDed, compared to zero). Per-arch: aarch64 `vqtbl1q_u8` + `vandq_u8` +
`vcgtq_u8`; x86_64 AVX2 `_mm256_shuffle_epi8`; SSE2 `_mm_shuffle_epi8`;
scalar fallback for the rare pathological 64+ set. `from_set` returns
`Option<ClassMask>` — CSS ident's 52 bytes encode cleanly.

**Files (new).**
- `parse-that/rust/parse_that/src/parsers/scan/class_membership.rs` — SIMD
  kernel.
- `parse-that/rust/parse_that/src/parsers/scan/ident_kernels.rs` — CSS
  ident, JSON string body, XML name built on `ClassMask`.
- `parse-that/rust/parse_that/tests/class_membership.rs` — SIMD ⇔ scalar
  byte-exact fuzz.

**Files (modified).**
- `parse-that/rust/parse_that/src/parsers/scan/ident.rs:54-62` — replace
  continuation loop with `ClassMask::scan_while_in`.
- `parse-that/rust/regex/src/automata/accel.rs:104-110` — 9-64 arm emits
  `AccelStrategy::ClassMask`; DELETE `ScalarLut` dead code.
- `crates/core/src/generate/regex/emit/simd.rs` — registry entry.

**Consumer invariant.** Cross-arch symmetry:
`cargo test --target aarch64-apple-darwin` +
`cargo test --target x86_64-unknown-linux-gnu`.
`grep AccelStrategy::ScalarLut parse-that/rust/regex/` → 0 hits.

**Profile target.** `scan_ident` on `css_tailwind` 13% → ~3%
(~4× scanner throughput). `css_tailwind` parse ≥ −9 to −14%.

**Risk.** 2/5.

#### Phase AA.11 — Structural bitmap pre-scan (sonic-rs parity, grammar-agnostic)

Merge of AA-prototype-2 AA.7 + AC.11. One SIMD pass over the input builds a
u64-packed bitmap of structural byte positions; downstream dispatch consults
it via `ctz` in O(1). The technique is grammar-agnostic — any grammar whose
union of (`@ws` charset ∪ `@token` first bytes ∪ dispatch-eligible Alt FIRST
sets) fits in ≤16 distinct byte classes qualifies. JSON, CSS, SQL, EBNF all
pass. Detection uses AA.2's `EClassFacts::first_set`.

**Detection (compile-time).** `StructuralBitmapMiner` runs inside Z.0's
unified walk, consults e-class facts, emits
`RecognizerShape::StructuralBitmap { classes: SmallVec<[u8; 16]>,
matched_brackets: Option<(u8, u8)> }` on qualifying grammars.

**Runtime scanner.** Three implementations behind `#[cfg(target_arch = ...)]`:
- aarch64 NEON: 16-byte chunk → `vld1q_u8` → ≤16 `vceqq_u8` → `vorrq_u8` →
  `vshrn_n_u16` compression (simdjson's 16-bit → 8-bit trick). 4 bits per
  byte; sixteen chunks fill a u64.
- x86_64 AVX2: 32-byte chunk → `_mm256_cmpeq_epi8` → `_mm256_movemask_epi8`.
- Scalar fallback.

Output: SoA `StructuralIndex { offsets: Vec<u32>, classes: Vec<u8> }` plus
`next_structural(offset)`, `next_after(offset)`, `match_bracket`.

**Distinguished from Z.1's failed SIMD attempts.** Z.1 tried SIMDifying
per-token scanners where workload is 5-20 bytes/call and SIMD setup cost
dominates. The structural bitmap is a *bulk* operation over the entire
input (tens of KB on JSON/CSS benches) — where NEON pays.

**CSP wiring.** New `AltMode::BitmapDispatch { class: u8 }` variant. Cost
model chooses between `ByteDispatch` and `BitmapDispatch` based on
`cfg.bitmap.{density_min, construction_cost_per_byte, dispatch_savings_per_site}`.

**Files (new).**
- `parse-that/rust/parse_that/src/parsers/scan/structural_bitmap.rs` (~400
  LOC).
- `crates/ir/src/passes/recognizers/structural_bitmap.rs` — miner.
- `crates/core/src/backend/kernels/structural_bitmap.rs` — kernel emission.
- `crates/core/src/backend/kernels/tape_cursor.rs` — `TapeCursor` +
  `next_of_class` (**re-used in Act III for the full tape walk**).
- `crates/core/tests/structural_bitmap_roundtrip.rs`.

**Files (modified).**
- `crates/ir/src/passes/patterns/mod.rs` — `RecognizerShape::StructuralBitmap`.
- `crates/ir/src/passes/recognizers/mod.rs` — register miner.
- `crates/ir/src/passes/csp_strategy/mod.rs` — `AltMode::BitmapDispatch`.
- `crates/core/src/backend/driver/alt.rs` — bitmap consultation.
- `crates/core/src/backend/recognizer_plan.rs` — `StructuralBitmap` arm.
- `parse-that/rust/parse_that/src/state.rs` — `ParserState::tape_cursor:
  Option<NonNull<TapeCursor>>` (null outside bitmap grammars, becomes the
  tape cursor in Act III).

**Consumer invariant.** Y.13 extension. Round-trip test. Debug assertion:
tape built exactly once per parse.

**Profile target.** `__value` + `scan_ws_block_comments` combined self-time
on `json_canada` 34% → 22%. `json_canada` parse ≥ −10 to −15%.
`css_tailwind` parse ≥ −3 to −6%.

**Risk.** 4/5. New runtime module, three SIMD implementations, new AltMode
variant, wide cargo-expand audit surface. Mitigation: `BBNF_BITMAP=on/off`
env var during iteration (removed in AA.18); round-trip test gates
correctness.

**Key insight for Act III:** the `tape_cursor` field lands here but the
runtime struct it points to is promoted to the full bbnf-tape crate in
AA.12. This is deliberate staging — the bitmap needs a cursor primitive,
which IS the tape cursor primitive.

#### Phase AA.12 — Compile-time perfect-hash dispatch for literal Alt groups

From AC.12 + AA-prototype-2 AA.8. CSS `__namedColor` is a 7-way first-byte
match with nested length + `unsafe_memcmp` checks per arm (~12-15 if-
depth), NOT a 230-way linear chain. Still a win: one FCH perfect-hash probe
replaces all nested checks. Grammar-agnostic: any Alt of ≥8 distinct
literal branches is a candidate.

**Technique.** FCH (Fox-Chen-Heath) minimal perfect hash — deterministic,
fast construction, embedded at compile time in the generated parser. On
rare construction failure (clustered key set), generator tries up to 64
random seeds; final fallback is `AltMode::ByteDispatch` for that Alt (the
construction fallback, not a runtime fallback — generated code still uses
only perfect hash where it applied).

**Files (new).**
- `crates/ir/src/passes/recognizers/perfect_hash.rs` — miner.
- `crates/core/src/backend/kernels/{perfect_hash,fch_generator}.rs` —
  compile-time generator + static table emission.
- `parse-that/rust/parse_that/src/phf.rs` — runtime lookup primitive.
- `crates/core/tests/perfect_hash_dispatch.rs`.

**Files (modified).**
- `crates/ir/src/passes/csp_strategy/mod.rs` — `AltMode::PerfectHash {
  table_id: u32 }`.
- `crates/core/src/backend/rust/emitter/dispatch.rs` — emit static tables
  + match inside a labeled block (depends on AA.8).

**Profile target.** `__namedColor` + `[u8]::eq` self-time on `css_tailwind`
12% → 3%. `css_tailwind` parse ≥ −6 to −10%. `compile_css_l4` +2 to +3%
from FCH construction (within budget).

**Risk.** 3/5. Dependencies: AA.7 (inline variant discriminants), AA.8
(labeled blocks).

**Act II checkpoint.** After AA.12, expect `json_canada` ≈ 1.6 GB/s,
`css_tailwind` ≥ 0.36 GB/s. Act II meets the `css_tailwind` /
`css_bootstrap` floors on its own; `json_canada` / `json_twitter` /
`json_citm` floors are out of reach without Act III. Under the
tape-or-bust commitment, this checkpoint is *not* a safety property —
it's a progress marker. Act III must land to close the tranche.

---

### Act III — Tape transposition (Phases AA.13–AA.16)

The breakthrough. Transpose the parser backend to emit a packed tape; the
typed AST becomes a generated lazy View surface. Per-element slab
allocation drops to ~0 in the parser hot loop.

**Tape or bust. No fallback.** There is no dual-mode escape hatch for the
post-tranche state: `BBNF_BACKEND_MODE` exists only during AA.14–AA.15 as
an iteration aid and is deleted in AA.16 alongside the eager emitter.
Phase AA.14's parity gate must be green on every production grammar before
AA.15 ships; if parity fails, AA.14 is reopened, the failing grammars are
triaged, and the phase re-runs. There is no "land Acts I+II and defer Act
III to AC" escape — Act III landing is a hard gate for this tranche's
close. The bet is: the tape is reachable with iteration, and refusing the
escape forces us to confront the parity bugs head-on rather than
pretending they're follow-up work.

This is the load-bearing commitment: the user's north star is sonic-rs-
class throughput on JSON and a larger margin on non-JSON grammars. Without
the tape we top out at ~1.7 GB/s on `json_canada` and cannot reach the
2.0 GB/s floor gate. Acts I+II alone do NOT meet the parse-time floor —
with tape-or-bust, the floor gates in the commitments table become
genuine hard gates, not "met without the tape" safety properties.

#### Phase AA.13 — `bbnf-tape` leaf crate + format-freeze gate

From AA-prototype-2 AA.10. Define the tape format. New leaf crate with no
bbnf dependencies (bumpalo only). Exports `Tape`, `TapeRec`, `TapeOffset`,
`TapeKind`, `TapeBuilder`, `TapeCursor`.

**Starting format (subject to refinement via the freeze gate).**
```rust
#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct TapeRec {
    pub kind:      TapeKind, // u8: codegen-assigned discriminant
    pub flags:     u8,       // bitfield: variant tag, has-children, span-only
    pub span_lo:   u32,      // input byte offset
    pub span_hi:   u32,      // length OR child-end-offset for compound
    pub child_off: u32,      // first child offset, or u32::MAX
    pub _pad:      u32,      // align to 24; future flag space
}
```

**`ChunkedArena<TapeRec>`.** 64 KB chunks (~2700 records each) allocated from
a bumpalo `Bump`. Indexing via `(chunk_idx, within_chunk)` decoded by shift
+ mask. O(1) amortized push with no realloc-copy.

**Format-freeze gate.** Build a measurement harness that walks the eager
AST on `json_canada` + `css_tailwind` + `bbnf self-host` and synthesises
tape records of candidate widths (16 / 24 / 32 bytes). Measure tape
footprint × cache-line behavior. Freeze the record format that has the
best space × decode-cost product. 24-byte design is the starting point;
revise if measurement says otherwise.

**Files.**
- `crates/bbnf-tape/{Cargo.toml, src/{lib,tape,builder,chunked_arena,
  cursor,kinds}.rs}` — ~900 LOC total.
- `crates/bbnf-tape/tests/{tape_basic,chunked_arena_capacity,format_freeze}.rs`.
- `Cargo.toml` (workspace) — add member.
- `crates/core/Cargo.toml`, `derive/Cargo.toml`, `ir/Cargo.toml` — declare
  dep.

**Gate.** Format-freeze measurement committed to `docs/benchmarks/
tape_format_freeze.json`. `cargo test -p bbnf-tape` green.

**Risk.** 1/5. Leaf crate; no consumer migration.

#### Phase AA.14 — `TapeBuilder` emitter (parser side, dual-mode iteration) + parity gate

From AA-prototype-2 AA.11. This is the tape-or-bust parity gate.

Every Rust monolithic emitter sibling gets a `tape_*` counterpart that
emits direct tape-write code instead of typed enum construction. Driver
gains `BackendMode::{Tape, EagerAST}` for the **iteration period only**
(AA.14–AA.15); default reads `BBNF_BACKEND_MODE`, which defaults to
`EagerAST` until AA.16 flips it and AA.16 deletes the enum entirely.
Dual-mode here is not a safety property — it's scaffolding to let the
tape emitter iterate in CI without blocking unrelated work. AA.16 removes
it along with the eager emitter, unconditionally.

**Generated shape for a JSON `pair`:**
```rust
#[inline]
fn __pair<'a, 'i>(state: &mut TapeBuilder<'a, 'i>) -> Option<TapeOffset> {
    let start = state.tape.recs.len() as u32;
    let _key = __string(state)?;
    state.eat_byte(b':')?;
    state.skip_ws();
    let _value = __value(state)?;
    let end = state.tape.recs.len() as u32;
    Some(state.push_compound(TapeKind::Pair, start, end))
}
```

**No `slab.alloc(enum_variant)`, no closures, no IIFEs, no inline Vec
scratch. Direct tape writes.** The `emit_alloc` chokepoint at
`ir_types.rs:295` is unused in tape mode; in AA.17 we delete it along
with the rest of the eager emitter.

**Files (new).**
- `crates/core/src/backend/rust/tape/{mod,strategy,dispatch,leaves,seq,
  binary,repeat,alt,operator_chain,map_value,grammar}.rs` — per-kind
  tape-emit siblings mirroring the emitter/ layout.

**Files (modified).**
- `crates/core/src/backend/driver/mod.rs` — `BackendMode` enum.
- `crates/core/src/pipeline/compile.rs` — wire mode through; default
  reads `BBNF_BACKEND_MODE` or `@tape` grammar directive.

**Parity gate (hard gate for tranche close).**
- `cargo expand -p bbnf --bench json_monolithic 2>&1 | grep '__pair'`
  produces the tape-builder shape (no `.map(...slab.alloc)`, no
  `Self::Variant(...)` construction in parser body).
- `crates/core/tests/tape_emitter.rs` compiles a small grammar in tape
  mode and verifies tape contents on hand-crafted input.
- `crates/core/tests/tape_eager_parity.rs` compiles every production
  grammar (JSON, CSS L4, BBNF, Sheets, EBNF) in BOTH modes and asserts
  the parsed structure matches **accessor-by-accessor** on 20+ sample
  inputs per grammar (the accessor names are generated from the same IR
  in both modes, so the equality check is mechanical).
- Zero failures on any grammar. No "known issue" deferrals.

**Profile target.** Raw tape-write throughput on `json_canada` +30 to +60%
vs eager (measured before view accessors land in AA.15).

**On parity failure.** AA.14 is reopened with the failing grammars
isolated into a minimized repro. Triage path: (1) compare generated tape
bytes to expected at a single-rule granularity, (2) diff the tape-emit
sibling against its eager emitter counterpart for the failing rule, (3)
fix the emitter sibling, (4) re-run the full-corpus parity gate. Iterate
until green. Per the tape-or-bust commitment, there is no path where
AA.14 is abandoned and the tranche closes without the tape.

**Risk.** 5/5. This is the make-or-break phase. Mitigation: extensive
cargo-expand snapshots at `docs/benchmarks/expand/AA.14/` diffed per
iteration, fuzz harness that walks the eager AST and synthesises expected
tape sequences as the parity oracle, single-rule minimization on every
failure, and relentless iteration. Sunk cost is acknowledged and
accepted.

#### Phase AA.15 — `TapeView` generator + prettify migration + consumer migration

Generate `impl` blocks exposing the typed AST as a lazy view over the
tape. For each rule with a non-Span projection, codegen emits a
`<Rule>View<'tape>` struct + accessor impls. The `bbnf-derive` macro
consumes these.

**Generated view shape:**
```rust
#[derive(Clone, Copy)]
pub struct PairView<'tape> {
    pub(crate) tape: &'tape Tape,
    pub(crate) rec:  TapeOffset,
}

impl<'tape> PairView<'tape> {
    #[inline] pub fn key(&self) -> StringView<'tape> {
        StringView::from_tape(self.tape, self.tape.child(self.rec, 0))
    }
    #[inline] pub fn value(&self) -> ValueView<'tape> {
        ValueView::from_tape(self.tape, self.tape.child(self.rec, 1))
    }
}
```

**Prettify migration.** Ground-truth finding: generated prettify fns have
signature `fn __<rule>_prettify(state: &mut ParserState, builder: &mut
FmtBuilder)` and walk the parser state directly, emitting FmtBuilder ops.
They do NOT pattern-match on typed enum variants. Retargeting to the tape
is mechanical: the codegen sibling emits
`fn __<rule>_prettify(cursor: &mut TapeCursor, builder: &mut FmtBuilder)`.
The rest of the prettify plumbing (hint directives, FmtBuilder ops,
source_range preservation via tape span_lo/span_hi) is identical.

**Consumer migrations** (bounded, per ground truth):
- **Prettify** (`crates/core/src/backend/rust/emitter/prettify/`) — ~705
  LOC across 7 files. Migrate the directory to
  `crates/core/src/backend/rust/tape/prettify/` with cursor-walking shape.
- **Serializer** (`crates/core/src/generate/serialize/serialize.rs`) —
  ~10 sites. Field access (`obj.field`) becomes accessor call
  (`obj.field()`).
- **LSP** (`crates/lsp/src/state/diagnostics/ir_analysis.rs`) — consumes
  IR meta, not parser output. Lifetime threading only, ~5 sites.
- **DAP** (`crates/lsp/src/dap/`) — IR position translation, no enum
  pattern matches. ~5 sites.
- **bbnf-derive** (`crates/derive/src/lib.rs`) — drop eager branch; emit
  `RootView<'input>` in place of owned root.
- **Gorgeous** — UNTOUCHED. It's already decoupled from generated parser
  AST (uses hand-written per-format types). Gorgeous adaptation to the
  new API is a future tranche.

**Files (new).**
- `crates/core/src/backend/rust/view/{mod,leaves,seq,alt,repeat,grammar,
  projection}.rs` — view emitter.
- `crates/core/src/backend/rust/tape/prettify/{mod,grammar,alt,seq,repeat,
  literal,attempt}.rs` — prettify-over-tape sibling.
- `crates/core/tests/prettify_tape_parity.rs` — deterministic golden Doc
  output on JSON / CSS / EBNF, byte-identical between eager and tape.

**Gate.** `cargo test --workspace` passes with `BBNF_BACKEND_MODE=tape`
set globally. `cargo bench json_monolithic` shows ≥+50% throughput on
`json_canada` vs pre-AA. `tape_eager_parity` from AA.14 still passes via
accessor comparison. `prettify_tape_parity` passes on the three grammars.

**Profile target.** `json_canada` 1.8–2.4 GB/s (from 1.2 baseline);
`json_twitter` 2.0–2.8 GB/s. `BumpSlab::alloc` self-time on `json_canada`
<1%.

**Risk.** 4/5. Wide consumer migration, but each consumer has its own
test suite + the parity tests catch semantic drift.

#### Phase AA.16 — Flip default to tape, delete eager AST emitter

Switch `BackendMode::Tape` to default. Delete the eager emitter entirely.

**Files deleted.**
- `crates/core/src/backend/rust/emitter/` — the entire directory
  (~5000 LOC). `mod.rs`, `leaves.rs`, `seq.rs`, `binary.rs`, `repeat.rs`,
  `alt.rs`, `operator_chain.rs`, `map_value.rs`, `grammar.rs`,
  `dispatch.rs`, `prettify/`, `tagged_union_emit.rs`, `control_flow.rs`,
  `scratch_guard.rs` (the last four migrate into `tape/` in their tape
  form).
- `crates/core/src/backend/rust/ir_types.rs::emit_alloc` family (~80 LOC).
  No more `slab().alloc(...)` emission anywhere.
- `crates/core/src/backend/types/decisions.rs` legacy eager arms (the
  `TaggedUnion` arm survives — it's used by the view emitter for type
  narrowing).
- `crates/core/src/backend/driver/` legacy BoxedEnum match arms.
- `BBNF_BACKEND_MODE` env var.

**Files modified.**
- `crates/core/src/backend/rust/mod.rs` — `pub mod tape;` + `pub mod view;`
  replace `pub mod emitter;`.
- `crates/derive/src/lib.rs` — drop the eager-mode codegen branch.

**Gate.** `cargo test --workspace` passes. Full bench sweep on post-AA.16
committed to `docs/benchmarks/post-act-III.json`. Cargo-expand snapshots
verify the eager pattern is gone everywhere. Y.13 consumer-invariant tests
all pass with variants now living in `view/` not `emitter/`.

**Profile target.** Same as AA.15; the flip is mechanical. The win is the
LOC deletion.

**Risk.** 5/5. Mass deletion is permanent. AA.15 ran in CI with
`BBNF_BACKEND_MODE=tape` for two days before this phase. Rollback requires
revert.

---

### Act IV — Cleanup + verification (Phases AA.17–AA.19)

#### Phase AA.17 — Final deletion sweep + tier isomorphism trait

Final grep sweep. Delete all legacy substrate this tranche obsoletes.

**Files deleted.**
- `crates/core/src/backend/patterns/{cache,delim_scan,key_dispatch}.rs`
  — the detection halves flagged in CLAUDE.md as "deletion candidates
  for the follow-up tranche once strategy solvers migrate to consume
  `ir.recognizer_decisions` directly." This tranche is that migration.
- `GrammarIR::has_family_recognizers` gate — backend now consults
  `AllocationContextAnalysis` / `EClassFacts::alloc_ctx`.
- `crates/ir/src/passes/patterns/mod.rs:21-65` legacy types (`AltPattern`,
  `SeqPattern`, `PatternAnnotations`).
- `BBNF_BITMAP=on/off` env var.
- `parse_that::BumpSlab` if tape is the only arena consumer (simplification
  novel transposition). Promote `ChunkedArena<T>` as the one-and-only
  runtime arena.

**Files (new).**
- `crates/egraph/src/lib.rs::OptimizableTier` trait — shared `build`,
  `saturate`, `extract` defaults for grammar + HIR tiers per
  `regex-crate-isomorphic` memory.
- `crates/egraph/tests/tier_isomorphism.rs`.

**Net deletion across the tranche**: ~6000–8000 LOC (eager emitter +
imperative first/follow + ScalarLut + BumpSlab + legacy pattern types).
Net add: ~4500–5500 LOC (tape crate, view emitter, analyses, tape prettify,
bitmap scanner, perfect hash, control-flow helpers, TypeDescId migration).
**Net reduction: ~1500–2500 LOC** while gaining sonic-rs-class throughput.

**Gate.** `cargo test --workspace` passes. `cargo clippy --all-targets
-- -D warnings` clean. All grep invariants (see hard gates table below)
hold.

**Risk.** 2/5. Cleanup, not new behavior.

#### Phase AA.18 — Incremental compile cache (LSP fast path)

From AC.15. LSP hot reload recompiles the full grammar per keystroke;
e-graph saturation + CSP solve = ~60% of that. Two caches cut most of it:

1. **Per-pattern regex HIR cache** —
   `GrammarIR::regex_info_cache: FxHashMap<(PatternString, CostConfigHash),
   RegexInfo>`.
2. **Per-rule strategy solve cache** — keyed on `(rule_body_hash,
   cost_config_hash, cross_rule_group_version)`.

**Files (new).**
- `crates/ir/src/cache/{mod,regex_info,strategy}.rs`.
- `crates/core/tests/incremental_compile.rs`.

**Files (modified).**
- `crates/ir/src/passes/{regex_info,csp_strategy/mod}.rs` — read/write
  cache.
- `crates/core/src/pipeline/compile.rs` — optional `cache: &mut
  CompileCache` parameter.
- `crates/analysis/src/document.rs` + `crates/lsp/src/...` — thread cache
  through document state.

**Profile target.** LSP hot reload on `compile_css_l4` ≥ −15 to −30%.

**Risk.** 3/5. Cache invalidation. Mitigated by conservative hash
(includes DAG signature + cross-group version bump on any rule change);
cache is opt-in (one-shot compiles bypass).

#### Phase AA.19 — post-AA baseline + profile-cited attribution

Full bench sweep, full test suite, every "+X%" claim cited from a samply
profile symbol + self-time delta in fresh post-AA profiles (not pre-Z).

**Deliverables.**
- `docs/benchmarks/post-AA.json` — full bench numbers for all benches × 3
  runs each. Every claim cited.
- `docs/benchmarks/profiles/post-AA/*.samply` + `*.syms.json`.
- `docs/benchmarks/expand/post-AA/*.rs` — final cargo-expand snapshots.
- Pre-Z → post-Z → post-AA full delta table.
- Per-act delta breakdown (Act I architectural; Act II independent parse
  wins; Act III tape; Act IV cleanup) so each act's contribution is
  visible.

---

## Hard gates (full table)

| Gate | Threshold |
|---|---|
| All workspace tests pass | yes |
| `bbnf-ir`, `bbnf-tape`, `egraph`, `csp-solver`, `bbnf-regex` tests | all passing |
| Bootstrap script idempotent | yes |
| Y.13 consumer-invariant test | extended for `TypeDescId`, `TaggedUnion`, `StructuralBitmap`, `BitmapDispatch`, `PerfectHashDispatch`, `ByteDispatchShared`, `SoftConstraintAdvice`, `DispatchSignature`, `EClassFacts` fields, every `TapeKind`, every View kind |
| `every_egraph_analysis_fact_has_a_consumer` | passes |
| `every_recognizer_shape_has_a_consumer_ratio` | ≥ 0.8 per shape |
| `grep -rn "pub(super) fn collect" crates/ir/src/passes/recognizers/` | 0 hits (Z.0 invariant) |
| `grep -rn "NoAnalysis" crates/ir/ parse-that/rust/regex/` | only the egraph crate's definition + tests |
| `grep -rn "AccelStrategy::ScalarLut" parse-that/rust/regex/` | 0 hits |
| `grep -rn "Option<TypeDesc>" crates/ir/src/passes/types/constraint/` | 0 hits |
| `grep -c "(\|\| {" $(cargo expand ... css_l4)` | 0 hits |
| Eager AST emitter | DELETED (`crates/core/src/backend/rust/emitter/` does not exist) |
| Imperative FIRST/FOLLOW/nullable passes | DELETED |
| `BBNF_BACKEND_MODE` env var | DELETED |
| `ir_types.rs::emit_alloc` | DELETED; no `slab().alloc` emission anywhere |
| `parse_that::BumpSlab` | DELETED; `ChunkedArena<T>` is the sole runtime arena |
| Cargo expand `__pair` in `json_monolithic` bench | no `.map(\|__v\| &*slab().alloc(__v))` pattern |
| Cargo expand `__namedColor` in `css_l4` | no linear `if name == "..."` chain ≥ 8 branches |
| `BumpSlab::alloc` self-time on `json_canada` | < 1% |
| `StructuralBitmap::scan` self-time on `json_canada` | 8–18% (new entry) |
| `__namedColor` self-time on `css_tailwind` | < 0.5% (was ~3.5%) |
| `json_canada` parse | floor ≥ 2.0 GB/s · target ≥ 3.0 GB/s |
| `json_twitter` parse | floor ≥ 2.1 GB/s · target ≥ 2.8 GB/s |
| `json_citm` parse | floor ≥ 2.4 GB/s · target ≥ 3.0 GB/s |
| `css_tailwind` parse | floor ≥ 0.36 GB/s · target ≥ 0.55 GB/s |
| `css_bootstrap` parse | floor ≥ 0.32 GB/s · target ≥ 0.45 GB/s |
| `compile_bbnf` | ≤ 1.5× pre-AA |
| `compile_css_l4` | ≤ 2.0× pre-AA |
| LSP hot-reload `compile_css_l4` | ≥ −15% (AA.18) |
| `BBNF_EGRAPH_REPORT=1` / `BBNF_HIR_EGRAPH_REPORT=1` | prints non-zero per-rule fire counts |
| `BBNF_PIPELINE_REPORT=1` | prints CSV per compile |
| `BBNF_CSP_REPORT=1` | zero budget exhaustions on standard benches |
| Every "+X%" claim in `post-AA.json` | cites a samply symbol + self-time delta |

---

## Critical files (load-bearing, in order of touch)

**Act 0:**
1. `docs/benchmarks/profiles/pre-AA/*.samply` (new)
2. `crates/egraph/src/{scheduler,csp_scheduler}.rs`
3. `crates/ir/src/egraph/mod.rs:99-113`
4. `crates/core/src/pipeline/compile.rs` (pipeline timing)

**Act I — Substrate:**
5. `crates/ir/src/types/type_desc/{mod,id,lattice}.rs` — AA.1 (NEW directory module)
6. `crates/ir/src/types/grammar.rs` — AA.1 (`type_desc_interner` field)
7. `crates/ir/src/passes/types/constraint/{alt,seq,grounds,operators,helpers,domain}.rs` — AA.1
8. `crates/core/src/backend/driver/{mod,seq,repeat,alt,wrap}.rs` — AA.1 (`resolve_type`)
9. `Cargo.toml` (workspace) — AA.2 (`crates/csp-solver` member)
10. `crates/egraph/src/analysis.rs` + `crates/ir/src/egraph/analysis/{mod,facts,merge}.rs` — AA.2
11. `parse-that/rust/regex/src/egraph/analysis/` — AA.2 (HIR mirror)
12. `crates/ir/src/egraph/mod.rs:53-99` — AA.2 (`NoAnalysis` → `GrammarAnalysis`)
13. `crates/egraph/src/cost_config/{mod,extraction,strategy,bitmap,perfect_hash,scheduler}.rs` — AA.3
14. `crates/egraph/src/extract/{mod,greedy,topo,scc,lattice}.rs` — AA.3
15. `crates/core/src/pipeline/compile.rs` — AA.4 (pipeline reorder, deletes)
16. `crates/ir/src/passes/sets/{first_sets,follow}.rs` — AA.4 (DELETE)
17. `crates/ir/src/egraph/extraction_bridge/{mod,facts_to_soft}.rs` — AA.5
18. `crates/ir/src/passes/csp_strategy/{mod,signature,cross_rule}.rs` — AA.5
19. `crates/egraph/src/rewrite.rs` — AA.5 (`should_apply`)
20. `crates/ir/src/egraph/rules/{lookahead,factor_left,factor_right,alt_split_byte}.rs` — AA.6

**Act II — Parse cliffs:**
21. `crates/ir/src/types/tagged_union.rs` — AA.7 (NEW)
22. `crates/ir/src/types/type_desc/mod.rs` — AA.7 (`TaggedUnion` variant)
23. `crates/core/src/backend/rust/ir_types.rs::emit_alloc` — AA.7 (route TaggedUnion past slab; DELETE in AA.16)
24. `crates/core/src/backend/types/decisions.rs:38-45` — AA.7 (`TaggedUnion` arm)
25. `crates/core/src/backend/rust/emitter/{control_flow,scratch_guard}.rs` — AA.8 (NEW)
26. `crates/core/src/backend/rust/emitter/{seq,repeat,binary,alt,dispatch,leaves,map_value,operator_chain,grammar}.rs` — AA.8 (IIFE → labeled blocks)
27. `parse-that/rust/parse_that/src/bump_slab.rs` — AA.9 (scratch API)
28. `parse-that/rust/parse_that/src/parsers/scan/{class_membership,ident_kernels}.rs` — AA.10 (NEW)
29. `parse-that/rust/regex/src/automata/accel.rs:104-110` — AA.10 (DELETE `ScalarLut`)
30. `parse-that/rust/parse_that/src/parsers/scan/structural_bitmap.rs` — AA.11 (NEW)
31. `crates/ir/src/passes/recognizers/structural_bitmap.rs` — AA.11 (NEW)
32. `crates/core/src/backend/kernels/{structural_bitmap,tape_cursor}.rs` — AA.11 (NEW)
33. `crates/ir/src/passes/recognizers/perfect_hash.rs` — AA.12 (NEW)
34. `crates/core/src/backend/kernels/{perfect_hash,fch_generator}.rs` — AA.12 (NEW)

**Act III — Tape:**
35. `crates/bbnf-tape/` — **AA.13 (NEW CRATE — the breakthrough substrate)**
36. `crates/core/src/backend/rust/tape/` — **AA.14 (NEW emitter directory)**
37. `crates/core/src/backend/rust/view/` — **AA.15 (NEW view emitter directory)**
38. `crates/core/src/backend/rust/tape/prettify/` — **AA.15 (prettify migration from emitter/prettify/)**
39. `crates/derive/src/lib.rs` — AA.15 (consume view emitter)
40. `crates/core/src/generate/serialize/serialize.rs` — AA.15 (accessor migration)
41. `crates/lsp/src/{state/diagnostics/ir_analysis,dap/*}.rs` — AA.15 (lifetime threading)
42. `crates/core/src/backend/rust/emitter/` — **AA.16 (DELETE entire directory, ~5000 LOC)**
43. `crates/core/src/backend/rust/ir_types.rs::emit_alloc` family — **AA.16 (DELETE)**

**Act IV — Cleanup:**
44. `crates/core/src/backend/patterns/{cache,delim_scan,key_dispatch}.rs` — AA.17 (DELETE)
45. `crates/egraph/src/lib.rs` — AA.17 (`OptimizableTier` trait)
46. `crates/ir/src/cache/{mod,regex_info,strategy}.rs` — AA.18 (NEW)
47. `docs/benchmarks/post-AA.json` — AA.19

---

## Risk register

| Phase | Risk | Failure mode | Mitigation |
|---|---|---|---|
| AA.0 | 1/5 | Profile capture fails | Redo, cross-check sample count |
| AA.1 | 4/5 | Partial TypeDescId migration | Single-commit, workspace test gate |
| AA.2 | 4/5 | Analysis ≠ imperative pass | `BBNF_ANALYSIS_DIFF=1` parity test through AA.4 |
| AA.3 | 2/5 | B&B / topo incorrect | Greedy cross-check oracle in debug builds |
| AA.4 | 4/5 | Deleting load-bearing passes | AA.2's parity gate must be green for ≥1 phase |
| AA.5 | 4/5 | Cross-rule CSP unsatisfiable; false-positive sharing | csp-solver budget fallback + 1000-Alt fuzzer + double-hash signature |
| AA.6 | 3/5 | New rule correctness | Per-rule regression + AA.2 parity |
| AA.7 | 4/5 | TypeDesc blast radius | emit_alloc chokepoint; TS/WASM fallback same commit |
| AA.8 | 2/5 | Missed `return` site | grep gate + differential fuzz |
| AA.9 | 3/5 | Chunk spill regression | Pre-size slab; spill rate <1% profile gate |
| AA.10 | 2/5 | Nibble-LUT pathological set | `from_set` returns Option; falls back to 2-stage |
| AA.11 | 4/5 | Three SIMD implementations; AltMode consumption | `BBNF_BITMAP=off` iteration; round-trip test |
| AA.12 | 3/5 | FCH construction failure on clustered keys | 64 seed retries; ByteDispatch construction fallback |
| AA.13 | 1/5 | Leaf crate shape | Format-freeze measurement gate |
| AA.14 | 5/5 | Tape emitter parity | Per-iteration cargo-expand diffs + full-corpus parity + single-rule minimization on every failure; **iterate until green — no fallback** |
| AA.15 | 4/5 | Prettify migration semantic drift | `prettify_tape_parity` deterministic golden Doc output |
| AA.16 | 5/5 | Mass deletion permanent | AA.14 + AA.15 parity gates green across full bench corpus |
| AA.17 | 2/5 | Deletion sweep | Workspace test + grep gates |
| AA.18 | 3/5 | Cache invalidation bug | Conservative rule body hash; incremental test |
| AA.19 | 0/5 | Verification only | — |

**Aggregate risk: VERY HIGH.** This is tape-or-bust: there is no
fallback for Act III. Mitigation strategy:
- Parity tests (AA.2/AA.4 for substrate; AA.14/AA.15 for tape). These
  are the load-bearing correctness safety net.
- Dual-mode *iteration scaffolding* only (AA.14–AA.15 with
  `BBNF_BACKEND_MODE`), deleted in AA.16.
- Per-phase samply attribution — every "+X%" cites a symbol delta.
- Single-rule minimization on every parity failure, so the iteration
  loop is fast.
- Extensive cargo-expand snapshots per iteration so regressions surface
  in diffs, not test flakiness.
- Relentless iteration on AA.14. Sunk cost is acknowledged and accepted
  as the cost of refusing the escape hatch.

**What we give up by refusing the fallback.** If AA.14 enters a
grind-loop, the rest of the tranche is blocked behind it. Acts I+II
cannot ship in isolation under tape-or-bust because AA.16 deletes the
eager emitter — you cannot land Act II parse wins on an emitter you
intend to delete without leaving orphan codegen. The commitment is: we
reach parity, or the tranche stays open.

---

## Verification methodology (per-phase)

Every phase ships with THREE artifacts before the commit:

1. **`cargo expand` diff** against the prior phase's snapshot, stored under
   `docs/benchmarks/expand/AA.{N}/`. Reviewed in the commit.
2. **`samply` symbol delta**, top-30 symbols diffed, stored under
   `docs/benchmarks/profiles/AA.{N}/`.
3. **`cargo test --workspace`** green. `#[allow(dead_code)]` in any deletion
   path fails CI.

Post-tranche (AA.19) delivers the `post-AA.json` with every "+X%" cited
from a fresh profile symbol, the five final profiles, and the pre-Z →
post-Z → post-AA delta table.

---

## What this tranche does NOT include

Non-goals, cited from the memory / prototypes:

1. **TS/WASM full TaggedUnion support** — Rust lands full, TS/WASM fall back
   to BoxedEnum in the same commit. Full TS/WASM TaggedUnion is a follow-up.
2. **Profile-guided cost calibration feedback loop** — AC.16's infrastructure
   is deferred. AA ships `BBNF_COST_TRACE=1` via the Phase AA.0 instrumentation
   but not the compile → run → refit cycle.
3. **Multi-threaded parsing** — sonic-rs stage1/stage2 split cannot SIMDify
   generic "stage 2" without a runtime VM. Hard veto.
4. **ILP backend for extraction** — B&B-with-budget is sufficient. An ILP
   solver dep is 40k+ LOC for ~2% quality gain.
5. **Gorgeous rewrite** — orthogonal; gorgeous is decoupled. A gorgeous
   rewrite consuming the new tape View API is a future tranche if justified
   by profile data.
6. **`@utf8`, `@lazy`, `@input_size` directives** — directive surface that
   interacts with the type system; deferred.
7. **Unified cross-tier super-e-graph** — AA.2's per-tier Analysis substrate
   gives fact-sharing without unification cost.
8. **pclmulqdq string-interior bitmap** — JSON-string-specific; AA.11's
   structural bitmap covers the dispatch case.

---

## Summary

Twenty phases across five acts. Act 0 baselines observability. Act I (AA.1–
AA.6) wakes the dormant substrate: TypeDescId interning, Analysis<N>
activation across both tiers, unified CostConfig, TopoExtractor,
extraction→CSP soft-constraint bridge, cross-rule CSP via dormant
UnionFind, analysis-gated rewrites. Act II (AA.7–AA.12) lands the
independent parse wins: TaggedUnion (BoxedEnum killer), IIFE→labeled
blocks, direct-to-slab scratch, ClassMask SIMD for 9–64-byte exit sets,
structural bitmap pre-scan, perfect-hash dispatch. **Act III (AA.13–AA.16)
is the breakthrough tape transposition**: bbnf-tape leaf crate,
TapeBuilder emitter with two-attempt parity fallback, TapeView generator
with prettify and consumer migration, deletion of the eager AST emitter.
Act IV (AA.17–AA.19) cleans up: deletes legacy pattern detection halves,
adds the `OptimizableTier` trait, lands the LSP incremental compile cache,
and writes the post-AA baseline with profile-cited attribution.

**Net LOC: ~1500–2500 reduction. Net parse throughput: 2.5–3× on JSON,
2× on CSS. Net architectural state: zero dormant substrate, one backend
path, one regex system, one arena type.**

The tape viability assessment is the load-bearing conclusion: prettify is
already tape-friendly at the generated-code shape level (verified in
ground truth), gorgeous is decoupled, and there are no external consumers.
The abrogation of the eager AST is not only viable — it is the only path
to the user's north star of sonic-rs-class throughput while preserving
the typed accessor API.

**Tape or bust.** There is no fallback for Act III. The eager emitter is
deleted unconditionally in AA.16. If the tape parity gate at AA.14
struggles, we iterate — single-rule minimization, per-iteration
cargo-expand diffs, extensive parity fuzzing — until every production
grammar passes accessor-by-accessor comparison on every fuzz input. The
tranche stays open until Act III lands. The commitment removes the
psychological escape valve that prior tranches used when substrate work
hit friction and forces us to confront the real architectural transitions
rather than pretending they're follow-up work.

One tranche. Five acts. Twenty phases. No quick solutions, no workarounds,
no legacy code, no fallback. Every architectural transposition is in
service of elegance, simplicity, and performance — all three at once.
