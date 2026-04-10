# Tranche AA — Substrate Awakening, Independent Parse Wins, and the Tape Transposition

## Context

Tranches W → Z burned through nine months of architectural cleanup and landed
exactly **one** material parse-time win: `json_canada +3.4 %` from Z.2's SWAR
fractional fix. Everything else fell within ±1.5 % noise. Of 18 parse-time
gates across four tranches, one met the bar. Compile-time wins came from
non-CSP architectural fixes (`factor_literal_prefixes`, e-graph lifecycle,
AC-3 clone elimination) — none from the cost model the tranches were ostensibly
building. Half of every recent tranche has been deleting prior tranches'
speculative substrate.

Post-Z numbers (M3 ARM): `json_canada` 1231 MB/s, `json_citm` 1896 MB/s,
`json_twitter` 1517 MB/s, `css_tailwind` 256 MB/s. sonic-rs is 3–6 GB/s.
lightning-css is 3–5× faster than us on CSS. We are at 25–50 % of sonic-rs
throughput on JSON, ~20 % on CSS.

A six-agent audit (profile, CSP, e-graph, regex, sonic-rs SOTA, clones,
codegen) plus a follow-up architectural reframe converged on **four hard
truths**:

1. **The optimizer substrate is dormant.** The `egraph` crate is
   architecturally complete (`Analysis`, `Rewrite`, `Extractor`, `CspScheduler`,
   `CostWeights`) but every load-bearing parameter is inert. `NoAnalysis` is
   the only `Analysis` impl in the workspace. `Rewrite::should_apply` doesn't
   exist — rules fire unconditionally. Extraction is greedy bottom-up
   (provably non-optimal under e-class sharing). `GrammarCostModel::Cost = f64`
   (no multi-objective). The strategy CSP is per-rule with unary constraints —
   the Y.5 cross-rule `UnionFind` substrate has zero consumers. We have
   egg/egglog's machinery and use none of it.

2. **The recurring failure mode is misframed audits.** Tranche Z corrected
   three of its own phases mid-flight after re-grepping the source.
   `CallStrategy::InlineFusion`, `WrapMode::DelimScan`, four `strategy_*` cost
   knobs were ghosts that drifted from W/V. Three of four recognizer families
   matched zero times on production grammars. Half of every recent tranche
   has been deleting prior tranches' speculative substrate.

3. **Explicit SIMD on M-series ARM is a dead end for short-run scan helpers.**
   Three SIMD prototypes in Z.1 regressed against the LLVM-autovectorized Y.7
   byte-class LUT. The autovectorizer reaches ~3–4 cycles/byte; explicit NEON
   setup cost is net-negative on minified-CSS short-run dominance. This is
   durable. Drop the theme. SIMD only pays where the workload is *long-run*
   bulk (≥ 64 bytes per call) — which is exactly the structural-bitmap
   pre-scan, not per-token scanners.

4. **The architectural ceiling is the typed AST construction itself, not
   parse_that combinators.** The Rust monolithic backend already generates
   direct recursive functions; combinators are gone in the hot path. What
   remains is the cost of materialising typed enum variants into a slab,
   one alloc per heterogeneous-Alt element, all the way down. The agent
   audits couldn't see this because they were looking at hot symbols *within*
   the existing model. Fixing the BoxedEnum producer (the AA-prototype's
   surgical scope) removes ~95 % of per-pair allocations on JSON and lifts
   us to maybe 1.5–1.8 GB/s — still half of sonic-rs because **every parse
   run constructs a complete typed tree before any consumer sees it**. The
   tree IS the parser output; that contract is what's expensive.

The user's directive on this turn:

> We should have NO combinators. If this needs an architecture change to
> reach the sonic-rs upperbound, what is it? What would it take? Do not be
> a defeatist.

The answer is the **tape transposition**: emit a packed binary tape during
parsing; expose the typed AST as a generated lazy `impl` over the tape that
materialises nodes on accessor calls. The user-facing API is preserved
(modulo a `'tape` lifetime parameter on view types). Per-element slab
allocation drops to ~0 in the parser hot loop. The sonic-rs upperbound
becomes reachable.

But the tape transposition is the most architecturally aggressive change
bbnf has ever attempted, and concentrating three HIGH-risk emitter phases
in a row with no independent parse-time win in between is exactly the
failure mode the prior tranches kept hitting. So Tranche AA is **structured
in five acts** with the parse-time wins distributed:

- **Act I — Substrate awakening** (Phases AA.1–AA.6): wake the dormant
  egraph machinery (Analysis with `Ctx` GAT, multi-objective `CostVector`,
  branch-and-bound extraction, conditional rewrites, cross-rule CSP) so
  every subsequent act has a real cost model and a real fact substrate to
  query. Imperative FIRST/FOLLOW/nullable passes are deleted via parity
  tests. csp-solver is vendored into the workspace.

- **Act II — Independent parse wins** (Phases AA.7–AA.9): the parse-time
  improvements that pay off **without** the tape. Structural bitmap
  pre-scan (sonic-rs's main parse-loop trick, generalised). Compile-time
  perfect-hash dispatch for keyword Alts (closes the css_tailwind
  `__namedColor` cliff). The surgical BoxedEnum → TaggedUnion narrowing
  (the AA-prototype's intent, expressed correctly via type narrowing). All
  three land regardless of whether Act III succeeds. Together they should
  deliver +25–40 % on the JSON benches and +30–50 % on css_tailwind even
  without the tape.

- **Act III — Tape transposition** (Phases AA.10–AA.14): the breakthrough.
  New `bbnf-tape` leaf crate, `TapeBuilder` emitter, `TapeView` generator,
  explicit consumer migration phase (gorgeous, lsp, debugger, bbnf-ser,
  prettify), and only then deletion of the eager AST emitter. Each phase
  is dual-mode via `BBNF_BACKEND_MODE=tape` for two full phases of CI
  before the deletion. **Explicit fall-back**: if Phase AA.11's parity gate
  fails after two attempts, Acts I + II + IV + V land without Act III.
  The tape becomes Tranche AC.

- **Act IV — SIMD + dispatch refinements** (Phases AA.15–AA.16): the
  dispatch-shape and slab-sizing improvements that compose with the
  bitmap (Act II) and the tape (Act III) but add only modest risk on
  their own.

- **Act V — Cleanup + verification** (Phases AA.17–AA.19): tier
  isomorphism, final deletion sweep, post-AA bench sweep with profile
  attribution.

The five-act structure has three load-bearing properties:

- **Parse-time wins land independently of the tape.** If Act III is a
  total wash, Act II still delivers ≥+25 % on `json_canada` and ≥+30 %
  on `css_tailwind` — twice the cumulative parse-time wins of W+X+Y+Z
  combined.
- **Each act builds substrate the next consumes**, so there is no
  parallel "old optimizer / new optimizer" path bleeding into the tranche.
- **The compile-time budget is tight enough to keep LSP responsiveness
  intact**: `compile_bbnf ≤ 1.5×`, `compile_css_l4 ≤ 2×` pre-AA. Tighter
  than the user's draft (3×) because LSP felt-perf at 30 ms per keystroke
  is a regression we shouldn't ship.

This is one tranche. Five acts. Twenty phases. No quick solutions, no
workarounds, no legacy code. The tape view preserves the public contract
(`Parser::parse(input) -> Result<RootView<'tape>, ParseErr>`) and the
typed-accessor API. Every architectural transposition is in service of
elegance, simplicity, and performance — all three at once.

---

## The architectural reframe — why the tape transposition is the answer

**Today**: the generated parser allocates each typed AST node into a
`BumpSlab` and references children via `&'arena Node`. The slab IS the AST.
Every accessor reads the typed enum field directly. Per-element cost on
heterogeneous Alt nodes is `slab.alloc(EnumVariant(payload))` → one allocation,
one tag write, one variant payload write, one indirection. JSON `__pair` runs
this on every value. At ~1.2 GB/s on `json_canada`, slab + variant
construction is the dominant cost.

**After Act III**: the generated parser appends fixed-size records to a
`Tape` (a chunked-arena `Vec<TapeRec>`). Records are 24 bytes:
`(kind: u8, flags: u8, span_lo: u32, span_hi: u32, child_off: u32, _pad: u32)`.
Compound nodes (Seq, Alt-with-content, Repeat-element) reference children
via tape offsets, not pointers. The parser hot loop becomes a sequence of
tape pushes — no closures, no `Result` `?` propagation through every call
(errors live in a sticky `error: Option<ParseErr>` on the builder, checked
at sub-tree exits), no per-element enum construction.

The typed AST is a **generated `impl` over the tape**:

```rust
// generated for `pair = string ":" value`
#[derive(Clone, Copy)]
pub struct PairView<'tape> {
    tape: &'tape Tape,
    rec: TapeOffset,
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

Each accessor is `unsafe { *self.tape.recs.get_unchecked(rec.0 as usize) }`
plus an offset add — 2–3 instructions. The eager path costs 5+ for the slab
indirection alone.

**Why this is not simdjson's tape**: simdjson's tape is the *interface*.
Users walk it directly with offsets. We keep typed accessors with full IDE
autocomplete and type checking. The tape is the *implementation*; the lazy
view is the user-visible API.

**Why this is not sonic-rs's direct deserialization**: sonic-rs writes into
a user-supplied target type and so requires the user to pre-declare the
schema. Our grammar IS the schema; codegen produces both the tape parser
and the view types from one source.

**The honest scope of consumer migration** (the part the user's draft
under-stated): a `Pair` consumer will see `Pair<'tape>` (the lifetime is
real, not hidden). Internal callers (the codegen siblings, gorgeous, lsp,
debugger, bbnf-ser) need lifetime-parameter migration. External
`#[derive(Parser)]` users that only chain accessors via `let r = parse(...)`
are unaffected; users that name types in function signatures need a
`'tape` parameter. Migration is mostly mechanical but not invisible. The
work is bounded (~1500–2500 LOC across 5–8 modules) and lives in its own
phase (AA.13).

---

## Profile-grounded ground truth (post-Z, six-agent audit)

| Bench | Time/iter | Top hot symbols | Dominant cost |
|---|---:|---|---|
| `json_twitter` (1517 MB/s) | 416 µs | `__value` dispatch + `slab().alloc(__v)` per pair value + `scan_number_f64` + `quoted_string_scan_full` | Per-pair value boxing (BoxedEnum cascade) + scalar dispatch chain |
| `json_canada` (1231 MB/s) | 1828 µs | `__value` (11.5 KB) + `RawVecInner::grow_amortized` + `mi_segment_span_allocate` + `scan_number_f64` (post-SWAR) | Slab grow stalls + per-element alloc + branch-mispredicted dispatch |
| `css_tailwind` (256 MB/s) | 14180 µs | `__declaration` (24.3 KB) + `__namedColor` (16.8 KB, 230 linear branches) + `__value` (14.2 KB) + 153 × `slab().alloc()` + `scan_ident` (~13 %) + `scan_ws_block_comments` (~12 %) + `[u8]::eq` (~9 %) | Polymorphic alloc + 230-way keyword search + scalar ws/ident loops |
| `compile_css_l4` | 7246 µs | `<std::alloc::System>::alloc` + `hashbrown::reserve_rehash_inner` + `egraph::build_and_saturate` + `csp_solver::bb_recurse` + `project_types` | Hash table churn + e-graph saturation + CSP lattice clones |
| `compile_bbnf` | 949 µs | Same shape as `compile_css_l4`, scaled down | Same |

**Cargo-expand corroboration:**

- JSON `__pair` expansion at the value call:
  `.map(|__v| &*__JsonParserEnum_alloc(state).slab().alloc(__v))`
  — every parse, every pair, one slab alloc.
- CSS L4 generated parser: 153 `slab().alloc()` call sites versus JSON's 2;
  the heterogeneous `Value` union forces per-variant boxing.
- CSS L4 `__namedColor`: ~230 sequential `if name == "<lit>"` branches
  inside one closure, no hash table.
- JSON `__value` dispatch: 6 hand-coded byte branches via unsafe
  `*(ptr.add(offset) as *const [u8; 4])` literal compares; not a dispatch
  table.
- CSS L4 IIFE checkpoint stacking: ~953 `(|| { ... })()` blocks for
  scope-local checkpoint isolation.

**Confirmed source-grounded truths cited from current HEAD (`6eeac0c`):**

- `crates/core/src/backend/types/decisions.rs:38-45` — `child_alloc`
  produces `Alloc` for `BoxedEnum` unconditionally (line 40).
- `crates/ir/src/passes/types/constraint/helpers.rs:86-96` — `join_types`
  returns `BoxedEnum` for any heterogeneous Alt with no cardinality check.
- `crates/ir/src/passes/csp_strategy/components.rs` — Y.5 `UnionFind`
  shipped with the comment *"As of Tranche Y.5 the only cross-variable
  constraints ... are all within the same rule body. There are zero
  cross-rule constraints in production."*
- `crates/egraph/src/analysis.rs` — `Analysis<N>` trait is fully
  implemented; both grammar tier and HIR tier instantiate with `NoAnalysis`.
- The 16-step pipeline at `crates/core/src/pipeline/compile.rs` builds the
  durable DAG once at line 430. `project_types` runs as op 16.

---

## Architectural commitments

1. **No legacy code, no workarounds, no fallback shims**, with one
   bounded exception: dual-mode `BBNF_BACKEND_MODE=tape|eager` env var
   exists for the duration of Phases AA.11–AA.13 only and is deleted in
   AA.14.
2. **Truth-based attribution.** Every "+X %" claim in `post-AA.json` cites
   a samply profile symbol + self-time delta from a fresh post-AA profile
   (not a sparse pre-Z profile). Phases that claim wins they cannot prove
   are re-opened or marked "architectural-only".
3. **Substrate before parse wins before tape.** Act I lays down the
   substrate Acts II–IV consume. Act II's parse wins land *before* Act III
   touches the parser, so the tranche has a non-zero parse delivery even if
   Act III slips. Act III is the breakthrough; Acts IV/V are derived.
4. **Compile-time budget is tight.** `compile_bbnf ≤ 1.5×`,
   `compile_css_l4 ≤ 2×` pre-AA. LSP responsiveness is non-negotiable.
   Per-component CSP / extraction budgets absorb overruns by falling back
   to greedy / per-variable trivial picks.
5. **Every new substrate has a load-bearing consumer in the same commit.**
   Y.13's consumer-invariant test extends to every new variant /
   analysis fact / cross-rule constraint / view kind. No phase ships
   without the test.
6. **Cross-tier symmetry.** Grammar tier and HIR tier remain isomorphic on
   the substrate they share (Analysis impl, cost model, scheduler, B&B
   extractor). New rewrites land as mirrored pairs unless their domain is
   exclusive to one tier.
7. **Profiling and testing happen inside each phase, not deferred.** Each
   phase ships with: a samply profile diff naming the moved symbol, a
   cargo-expand audit of the touched generated code, and a bench delta
   above the noise floor (±1.5 % parse, ±5 % compile).
8. **Determinism over cleverness.** `inline_acyclic` stays a deterministic
   pre-pass at op 3 of the 16-op pipeline. Inlining decisions do NOT
   migrate into the e-graph (cost gate fine, decision migration not). LSP
   incremental analysis depends on stable codegen output across compiles
   of the same grammar.

### Bench gates — two tier (floor / target)

| Gate | Floor (hard) | Target | Dominant phase |
|---|---|---|---|
| `json_canada` parse | ≥ 2.0 GB/s (+62 %) | ≥ 3.0 GB/s (+144 %) | AA.7 bitmap + AA.9 narrowing + AA.11–14 tape |
| `json_twitter` parse | ≥ 2.1 GB/s (+38 %) | ≥ 2.8 GB/s (+85 %) | AA.7 + AA.9 + tape |
| `json_citm` parse | ≥ 2.4 GB/s (+27 %) | ≥ 3.0 GB/s (+58 %) | AA.7 + AA.9 + tape |
| `css_tailwind` parse | ≥ 0.36 GB/s (+41 %) | ≥ 0.50 GB/s (+95 %) | AA.7 bitmap + AA.8 perfect hash + tape |
| `css_bootstrap` parse | ≥ 0.32 GB/s (+30 %) | ≥ 0.45 GB/s (+85 %) | AA.7 + AA.8 |
| `compile_bbnf` | ≤ 1.5× pre-AA | ≤ 1.3× | AA.2 / AA.3 / AA.5 |
| `compile_css_l4` | ≤ 2.0× pre-AA | ≤ 1.6× | AA.2 / AA.3 / AA.5 |

**Floor**: hard gate. Missing the floor on any parse bench triggers a
phase-by-phase rollback audit. Hitting the floor counts as a tranche win
even if the target is missed. **Target**: aspirational. Phases that reach
the target are noted in `post-AA.json` with the responsible symbol delta.

The structural commitment is that **the floor is reachable with Acts I + II
alone** (without the tape). The target requires Act III. This makes Act
III's fall-back option safe: if the tape doesn't land, the tranche still
ships meaningful parse-time wins.

---

## Phase 0 — Profiling baseline (non-deferrable, runs first)

Per the user's instruction "Profiling and testing should NOT be deferred to
be WITHIN the tranche": this runs **before any code changes**. The post-Z
`.syms.json` profiles are sparse (4–7k samples per the Phase 1 audit) and
unfit for symbol-level attribution.

**Steps** (executed when plan is approved and we exit plan mode):

1. Capture fresh `samply record --save-only --unstable-presymbolicate`
   profiles for: `compile_bbnf`, `compile_css_l4`, `compile_json`,
   `compile_sheets`, `compile_ebnf`, `json_canada`, `json_twitter`,
   `json_citm`, `json_data_xl`, `css_tailwind`, `css_bootstrap`. Each at
   HEAD (`6eeac0c`). Profiles land at
   `docs/benchmarks/profiles/pre-AA/*.samply` + `*.syms.json`.
2. Capture per-bench wall-clock numbers via single-invocation `cargo bench`
   sweeps (per Tranche Z's invariant that single-invocation produces
   non-contaminated numbers). Land at `docs/benchmarks/pre-AA.json`.
3. Run `cargo expand -p bbnf --bench json_monolithic >
   docs/benchmarks/expand/pre-AA.json_monolithic.rs` and the same for
   `css_l4`. These are the substrate cargo-expand snapshots that every
   code-emitting phase diffs against.
4. Sample the top-30 hot symbols per profile via `samply --csv` (or jq the
   syms.json). Build a prioritisation table: which symbols are > 2 %
   self-time on which bench. This is the truth source for "did the phase
   land what it claimed."

**Gate**: every profile has ≥ 50k samples (vs the post-Z 4–7k); the expand
snapshots compile cleanly; the wall-clock numbers reproduce within ±2 % on
three runs.

**No source files touched.**

---

## Act I — Substrate awakening (Phases AA.1–AA.6)

The egraph machinery is built and untested in production. Wake it before
any optimisation rule consumes it. The pieces are: the `Analysis` trait
needs context-passing (`Ctx<'a>` GAT); analyses need a product-lattice
substrate so they can be composed; extraction needs to be optimal under
sharing (B&B with budget); rewrites need to be conditional
(`should_apply`); CSP needs cross-rule constraints; csp-solver should be
in the workspace, not patched from a sibling repo.

### Phase AA.1 — `egraph-derive` Analysis macro + `Ctx` GAT in `Analysis` trait

Wake the substrate by giving the existing `Analysis` trait a way to read
context (a `Ctx<'a>` GAT for `make`/`merge`/`modify`) and by extending the
proc-macro to derive `Analysis` impls for product lattices.

**Files:**
- `crates/egraph/src/analysis.rs` — add `type Ctx<'a>;` GAT, change `make`
  signature to `(egraph, ctx, node) -> Data`.
- `crates/egraph/src/egraph.rs` — `add` becomes `add_with_ctx`;
  `add(node) = add_with_ctx(&(), node)` for `NoAnalysis::Ctx = ()`.
- `crates/egraph/src/{rewrite,extract,scheduler,csp_scheduler}.rs` — thread
  `Ctx` through.
- `crates/egraph-derive/src/lib.rs` — extend with `#[derive(Analysis)]` for
  tuple structs of analysis fields; emit per-field `make`/`merge` plumbing.
- All `bbnf-ir` and `bbnf-regex` consumer call sites pass `&()` until their
  own analyses land (Phase AA.2).

**Gate**: `cargo test -p egraph` + `cargo test -p egraph-derive` pass; new
derive-test verifies a hand-written 2-field product lattice merges
correctly under union.

**Profile expectation**: zero measurable delta. Pure signature change.

**Risk**: LOW. Mechanical.

### Phase AA.2 — Foundational analyses (parity-tested against imperative passes)

Replace `NoAnalysis` with a real `GrammarAnalysis` product lattice in the
grammar tier. Five sub-analyses cover everything subsequent phases need:

- `NullableAnalysis` — boolean lattice; `merge` is `||`.
- `FirstSetAnalysis` — `CharSet128` lattice; `merge` is union.
- `FollowSetAnalysis` — `CharSet128` lattice; populated via fixed-point
  during saturation, not as a separate pass.
- `AllocationContextAnalysis` — `{Inline, Alloc, VecOf(_)}` lattice;
  propagates from rule body downward. The substrate that AA.7 (bitmap),
  AA.9 (TaggedUnion narrowing), and Act III (tape format decisions) all
  consult.
- `InlineOkAnalysis` — boolean lattice; ⊤ means "this rule's projected
  type can be inlined into any parent enum variant slot without
  indirection." This is the analysis the BoxedEnum producer fix actually
  wants.

**Critically**: the imperative `compute_first_sets`, `compute_follow_sets`,
`compute_nullable`, `refine_span_eligibility` passes keep running in
parallel for **exactly one phase** (this one) to enable the parity test.
They are deleted in Phase AA.4.

**Files:**
- `crates/egraph/src/analyses/{mod.rs, tuple.rs}` — generic product-lattice
  substrate.
- `crates/ir/src/egraph/analyses/{mod.rs, nullable.rs, first_set.rs,
  follow_set.rs, alloc_ctx.rs, inline_ok.rs}` — five new analysis files.
- `crates/ir/src/egraph/mod.rs` — typedef
  `GrammarAnalysis = Tuple5<Nullable, FirstSet, FollowSet, AllocCtx, InlineOk>`;
  switch `EGraph<GrammarENode, GrammarAnalysis>`; thread `&GrammarCtx`
  (containing `SharedRuleMap` + the SCC + the literal interner) through
  `add` calls in `build_egraph.rs`.
- `crates/ir/tests/analysis_parity.rs` — for each production grammar, after
  saturation, walk every rule's root e-class and assert
  `analysis.first_set == rule.meta.first_set` (and same for nullable,
  follow, span eligibility, alloc ctx). The parity gate.

**HIR-tier mirror** (per `regex-crate-isomorphic`):
`parse-that/rust/regex/src/egraph/analyses/{nullable,width_range,first_byte,is_recognizable,follow_byte}.rs`
plus the `HirAnalysis` typedef in
`parse-that/rust/regex/src/egraph/mod.rs`. Same parity-test pattern in
`parse-that/rust/regex/tests/analysis_parity.rs`.

**Gate**: parity tests pass on all 5 production grammars (JSON, CSS L4,
BBNF, Sheets, EBNF). `cargo bench compile_*` shows ≤ 25 % compile-time
regression in the worst case (analysis runs alongside the imperative
passes — this is the temporary ceiling, removed in Phase AA.4).

**Profile expectation**: `compile_css_l4` +15 to +25 % (worst case, double-running). `compile_bbnf` +10 %.

**Risk**: MEDIUM-HIGH. Parity is the only safety net. Mitigation: a
`BBNF_ANALYSIS_DIFF=1` env var that runs both paths and asserts agreement
at every rule, used in CI for the duration of Phase AA.2.

### Phase AA.3 — Multi-objective `CostVector` + budgeted optimal extraction

The current `Extractor` is greedy bottom-up
(`crates/egraph/src/extract.rs:75–109`) and provably non-optimal under
e-class sharing. Replace with a branch-and-bound `OptimalExtractor` that
operates on a multi-objective `CostVector` and falls back to greedy when a
per-grammar budget is exhausted.

**Files:**
- `crates/egraph/src/cost_weights.rs` — add
  `CostVector { code_size: f32, parse_time: f32, alloc_count: u32, compile_budget: u32 }`
  and a `Scalarize` weighted-sum reducer.
- `crates/egraph/src/cost_config.rs` — add
  `extraction_budget_nodes: usize` (default 60_000),
  `extraction_mode: ExtractionMode::{Greedy, OptimalOrBudgeted}` (default
  `OptimalOrBudgeted` post-AA.6; `Greedy` until then).
- `crates/egraph/src/extract.rs` — `CostModel::Cost` becomes a real
  associated type (not hard-coded `f64`); add default `scalarize` and
  `lower_bound` methods.
- `crates/egraph/src/extract_optimal.rs` (new) — branch-and-bound DP with
  the budget cap; on exhaustion, return `(greedy_result, BudgetExhausted)`.
- `crates/ir/src/egraph/cost.rs` — `GrammarCostModel::Cost = CostVector`;
  rewrite `cost(...)` to compute all four fields, with parse-time weighted
  from `AllocationContextAnalysis` (Inline → 0, Alloc → 5, VecOf → 10).
- `parse-that/rust/regex/src/egraph/cost.rs` — same migration for
  `RegexExtractionCost`.
- `crates/egraph/tests/extract_optimal.rs` — hand-crafted small e-graph
  with known optimum; verify B&B finds it; verify budget fallback kicks in
  on a synthetic 1000-class e-graph.

**Gate**: parity test from Phase AA.2 still passes (analyses are
independent of cost model). New extraction-optimality unit test passes.
Bench `compile_bbnf` ≤ +15 % (B&B is bounded by budget).

**Profile expectation**: With `extraction_mode = Greedy` (default this
phase), zero delta. With `OptimalOrBudgeted` (manually toggled in test),
`compile_bbnf` +5 to +8 %, `compile_css_l4` +8 to +12 %.

**Risk**: MEDIUM. Mitigation: dual-mode (`BBNF_EXTRACTION_MODE=greedy|optimal`)
until the default flip in AA.6.

### Phase AA.4 — Pipeline reorder + imperative pass deletion

Now that analyses are at parity, delete the imperative passes, retain the
e-graph across the rest of compile (so the backend can read analyses),
and reorder the pipeline.

**Files:**
- `crates/core/src/pipeline/compile.rs` — delete `compute_first_sets`,
  `compute_follow_sets`, `refine_span_eligibility`, `compute_nullable`. The
  new ordering: lower IR → SCC → build e-graph → saturate (analyses
  converge during saturation) → extract canonical IR → recognise patterns
  → emit code. The e-graph + analyses persist into the backend through
  `ir.egraph: Option<GrammarEGraph>`. **`inline_acyclic` stays as
  op 3 of the (now 13-op) pipeline — deterministic, not in the e-graph.**
- `crates/ir/src/passes/sets/{first_sets.rs, follow.rs}` — **delete**
  (~943 LOC). Replaced by the analyses.
- `crates/ir/src/passes/span.rs::refine_span_eligibility` — **delete**.
  Span eligibility now flows from `InlineOkAnalysis ∧
  SpanProjectableAnalysis` (the latter added in AA.5).
- `crates/ir/src/passes/mod.rs` — drop `pub use` re-exports of deleted
  symbols.
- `crates/core/tests/pipeline_no_imperative_passes.rs` (new) — grep test
  asserting `compute_first_sets`, `compute_follow_sets`,
  `refine_span_eligibility` no longer exist as items in `bbnf-ir`.

**Gate**: workspace tests pass. `compile_css_l4` post-AA.4 returns to
within ±10 % of pre-AA (the parallel-running burden is gone; analyses run
alone).

**Profile expectation**: `compile_css_l4` -10 to -20 % from AA.2 (back near
baseline). Parse benches unchanged.

**Risk**: HIGH. Deleting load-bearing passes. Mitigation: AA.2 + AA.3 ran
analyses in parallel for two full phases; AA.4 deletes only after the
parity tests have been green for both. Any consumer that imports the
deleted symbols fails the build immediately.

### Phase AA.5 — Conditional rewrites + cross-rule CSP topology + csp-solver vendoring

Three coupled changes that wake the rest of the substrate.

**(a) `Rewrite::should_apply`**: Add
`should_apply(egraph, ctx, class_id, match) -> bool` and migrate the
existing 5 grammar-tier + 5 HIR-tier rules to query analyses where it
benefits them. Default returns `true`.

- `SupersetAbsorbAlt::should_apply` queries `AllocationContextAnalysis`
  (don't absorb a branch whose alloc ctx differs from siblings).
- `FuseAltRegexBranches::should_apply` queries `InlineOkAnalysis` (don't
  fuse if downstream consumers need typed children).
- `CommonSuffixFactor::should_apply` queries `NullableAnalysis`.

**(b) Cross-rule CSP topology**: Wake the dormant Y.5 substrate. Refactor
`solve_strategy_decisions` to operate grammar-wide (collect all sites
across all rules into one CSP), use Y.5's `UnionFind` to decompose into
independent components, solve each component with B&B per the csp-solver
capability. Three new cross-rule constraints land:

- `SccConsistentDispatchConstraint` — all rules in the same SCC must use
  compatible AltMode (so mutual recursion doesn't see inconsistent
  dispatch shapes).
- `AllocPropagationConstraint` — `AllocationContextAnalysis` propagates
  through `Ref` edges across rule boundaries.
- `DispatchShareConstraint` — two rules with structurally-identical Alt
  signatures share one dispatch table (the `static` is hoisted to module
  scope).

The X.6 freeze cautionary tale: Y.-1's CSP node budget is the safety net.
A blown component falls back to per-variable trivial picks for that
component without hanging the compile. Diagnostic via `BBNF_CSP_REPORT=1`.

**(c) Vendor `csp-solver` into the workspace**: Move
`/Users/mkbabb/Programming/csc411/CSC411_HW2_ProgrammingQuestion/csp-solver/`
→ `crates/csp-solver/`. The Phase AA.5 work adds new constraint types
(`AllEqualConstraint`, `GroupImplicationConstraint`); there's no point
continuing to cross repo boundaries when this tranche owns the new files.
`.cargo/config.toml` patch is dropped; workspace member is added.

**Files:**
- `crates/egraph/src/rewrite.rs` — add `should_apply` to `Rewrite`; modify
  `RewriteFn::run` to consult.
- `crates/ir/src/egraph/rules/regex.rs`, `suffix.rs` — port rules to
  consult analyses.
- `crates/ir/src/passes/csp_strategy/mod.rs` — grammar-wide solve via
  components.
- `crates/ir/src/passes/csp_strategy/components.rs` — wake the Y.5
  `UnionFind` substrate as a real producer.
- `crates/ir/src/passes/csp_strategy/constraints/{scc_consistent_dispatch.rs, alloc_propagation.rs, dispatch_share.rs}`
  (new).
- `crates/csp-solver/` (vendored) + `crates/csp-solver/src/constraint/{symmetry.rs, cross_rule.rs}`
  (new constraint types).
- `Cargo.toml` (workspace) — add `crates/csp-solver` member.
- `.cargo/config.toml` — drop the `csp-solver = { path = ... }` patch.
- `crates/ir/tests/csp_grammar_wide_equiv.rs` (new) — for grammars with no
  cross-rule constraints, grammar-wide solve must produce the same
  `RecognizerDecisionMap` as the old per-rule solve.
- `crates/ir/tests/cross_rule_dispatch.rs` (new) — two rules with
  structurally-identical Alts share one dispatch table.

**Gate**: parity tests + new cross-rule tests pass. `compile_bbnf` ≤ +5 %
(predicates skip no-op applies, cross-rule decomposition offsets cost).

**Risk**: MEDIUM. Cross-rule constraints can make a satisfiable CSP
unsatisfiable. Mitigation: csp-solver's existing fallback
(`decode_min_cost_per_variable`) is the safety net; emit a
`BBNF_CSP_REPORT=1` diagnostic on any fallback.

### Phase AA.6 — Analysis-gated e-graph rules (the safe ones)

Add the rewrites that the new cost model + analyses make safe. Each is a
small, predicate-guarded rewrite. **`CrossRuleInline` is explicitly NOT
in scope** — `inline_acyclic` stays as a deterministic op 3 of the
pipeline so LSP incremental analysis caching survives.

The rules:

1. **`LookaheadPushdown`** — `Next(Alt([A,B,C]), L)` →
   `Alt([Next(A,L), Next(B,L), Next(C,L)])` when `FollowSetAnalysis`
   confirms `FOLLOW(children) ∩ L = ∅`.
2. **`SequenceFactoringLeft`** — `Alt([Seq([A,X]), Seq([A,Y])])` →
   `Seq([A, Alt([X,Y])])`. Predicate: `A` is cheap (single literal/regex).
   Generalisation of the existing `factor_literal_prefixes` IR pass which
   is restricted to literals.
3. **`SequenceFactoringRight`** — symmetric. `CommonSuffixFactor` already
   exists in `crates/ir/src/egraph/rules/suffix.rs`; this is its dual for
   non-Seq-uniform Alts.
4. **`RepeatDistribution`** (HIR) — `Repetition(Alternation([A,B]))` →
   `Alternation([Repetition(A), Repetition(B)])` when a new
   `RunIndependence` HIR analysis confirms the runs are independent.
5. **`AltSplitByDispatchByte`** — given an Alt with mixed-FIRST branches,
   factor by leading byte to enable dispatch. Currently a recogniser pass;
   promotion lets the e-graph try alternative formulations.

After AA.6, flip `extraction_mode` default to `OptimalOrBudgeted`.

**Files:**
- `crates/ir/src/egraph/rules/{lookahead.rs, factor_left.rs,
  factor_right.rs, alt_split_byte.rs}` (new). Each ~80–150 LOC.
- `crates/ir/src/egraph/rules/mod.rs` — register in `default_rules`.
- `parse-that/rust/regex/src/egraph/rules/{repeat_distribution.rs,
  factor_left.rs, factor_right.rs}` (new HIR-tier mirrors per
  `regex-crate-isomorphic`).
- `crates/egraph/src/cost_config.rs` — flip default
  `extraction_mode = OptimalOrBudgeted`.
- Per-rule unit tests in `crates/ir/tests/rules_*.rs`.

**Gate**: each rule's unit test passes; end-to-end test verifies the JSON
`value` rule extracts with lookahead-pushed dispatch shape; CSS L4
`declaration` rule extracts with sequence-factored common prefix.

**Profile expectation**: parse benches `json_canada` +2 to +4 %,
`json_twitter` +2 to +3 %, `css_tailwind` +1 to +2 %. Compile benches +2
to +4 % (rules iterate).

**Risk**: MEDIUM. Each rule is a potential correctness bug. Mitigation:
each rule has hand-crafted before/after regression test + the parity test
from AA.2 catches lattice violations.

---

## Act II — Independent parse wins (Phases AA.7–AA.9)

These three phases land **before** the tape and pay off **without** it.
They use the substrate from Act I and they hedge Act III: if Act III
slips or rolls back, Act II's wins are still in the tree. Each phase is
independent of the next, so they can land in parallel review tracks.

### Phase AA.7 — Structural bitmap pre-scan (sonic-rs's main parse-loop trick, generalised)

This is the load-bearing parse-time win that doesn't require the tape.
One SIMD pass over the input identifies every structural byte and stores
them as a u64-packed bitmap. Downstream dispatch consults the bitmap in
O(1) instead of per-byte lookahead. **The technique is grammar-agnostic**
— the only requirement is that the union of `@token` first bytes + `@ws`
first bytes + dispatch-Alt FIRST sets fits in ≤ 16 distinct byte classes.
JSON, CSS, SQL, EBNF all qualify.

**Detection (compile-time):**

New miner `StructuralBitmapMiner` at
`crates/ir/src/passes/recognizers/structural_bitmap.rs` runs inside
Z.0's unified walk. It queries the AA.2 e-class
`FirstSetAnalysis` to compute the union over all dispatch-eligible Alt
branches in the grammar plus the `@ws` charset plus the `@token` first
bytes. If the union has ≤ 16 distinct bytes AND the input density
(estimated from a representative sample bench input) is ≥
`bitmap_scan_density_floor` (default 0.06 = 1 in ~16 bytes), the miner
emits `RecognizerShape::StructuralBitmap { classes: SmallVec<[u8; 16]>,
matched_brackets: Option<(u8, u8)> }` on the grammar's entry point.

**Runtime scanner:**

New module at
`parse-that/rust/parse_that/src/parsers/scan/structural_bitmap.rs`. Three
implementations behind `#[cfg(target_arch = ...)]`:

1. **aarch64 NEON** (the M-series target): 16-byte chunk → `vld1q_u8` →
   up to 16 `vceqq_u8` compares ORed via `vorrq_u8` → mask extraction
   via `vshrn_n_u16` (the simdjson trick for compressing a 16-bit mask
   to 4 bits per byte). Each chunk produces 4 bits into a u64; sixteen
   chunks fill the u64.
2. **x86_64 AVX2**: 32-byte chunk → `_mm256_loadu_si256` →
   `_mm256_cmpeq_epi8` compares ORed → `_mm256_movemask_epi8` → 32 bits
   per chunk.
3. **Scalar fallback** for other arches (uses the existing
   `find_first_of_nibble_lut` from
   `parse-that/.../scanners.rs:195`).

Output: `StructuralBitmap { words: bumpalo::Vec<u64>, input_len: usize }`
plus `next_structural(offset) -> Option<usize>` (`ctz`-driven), `next_after(offset)`,
`match_bracket(open_pos, open, close, bytes) -> Option<usize>`.
Construction is a single forward sweep.

**Why this is different from Z.1's failed SIMD attempts**: Z.1 tried to
SIMDify per-token scanners (`scan_ws_block_comments`) where the workload
is 5–20 bytes per call and SIMD setup cost dominates. The structural
bitmap is a *bulk* operation over the entire input — typically tens of
KB on a JSON / CSS bench — where the per-byte SIMD cost amortises. This
is the workload where NEON pays.

**CSP wiring:**

New `AltMode::BitmapDispatch` variant in `csp_strategy/mod.rs`. The CSP
picks it when the rule has a `RecognizerShape::StructuralBitmap` and the
parent context permits. Cost model: bitmap construction cost is amortised
(O(input_len) once per parse), so for grammars with many dispatch sites
the bitmap dominates per-site lookahead.

**Backend wiring:**

New kernel at `crates/core/src/backend/kernels/structural_bitmap.rs`
emits the pre-scan call once at the parser entry. The dispatch path at
`crates/core/src/backend/driver/alt.rs` consults the bitmap via
`next_structural` to jump to the next structural position when an Alt
is dispatch-eligible AND its FIRST set is a subset of the bitmap classes.

**Files (net-new):**
- `parse-that/rust/parse_that/src/parsers/scan/structural_bitmap.rs` (~400 LOC).
- `crates/ir/src/passes/recognizers/structural_bitmap.rs` (miner).
- `crates/core/src/backend/kernels/structural_bitmap.rs` (kernel emission).
- `crates/core/tests/structural_bitmap_roundtrip.rs` (round-trip test).

**Files modified:**
- `parse-that/rust/parse_that/src/parsers/scan/mod.rs` — re-export.
- `crates/ir/src/passes/patterns/mod.rs` — `RecognizerShape::StructuralBitmap` variant.
- `crates/ir/src/passes/recognizers/mod.rs` — register the miner.
- `crates/ir/src/passes/csp_strategy/mod.rs` — `AltMode::BitmapDispatch`.
- `crates/core/src/backend/strategy/alt_strategy.rs` — bitmap-vs-byte cost.
- `crates/core/src/backend/driver/alt.rs` — bitmap consultation path.
- `crates/core/src/backend/recognizer_plan.rs` — `StructuralBitmap` arm.
- `crates/egraph/src/cost_weights.rs` — `bitmap_scan_density_floor`,
  `bitmap_construction_cost`.
- `crates/core/tests/recognizer_decision_consumption.rs` — Y.13
  extension.

**Profile-measured impact target:**
- `json_canada` parse: −15 to −22 % (large array dispatch dominates;
  bitmap eliminates per-element lookahead).
- `json_twitter` parse: −8 to −12 % (smaller dispatch sites; gain from
  tighter inner loops).
- `css_tailwind` parse: −5 to −10 % (selector + declaration dispatch).
- Samply on `json_canada`: `structural_bitmap::scan` symbol present on
  the hot stack at 8–18 % self-time; per-element dispatch chain symbols
  drop out of the top-10.
- Cargo expand: the JSON entry rule prelude shows the
  `structural_bitmap_scan(state)` call.

**Risk**: MEDIUM-HIGH. New runtime module, three SIMD implementations, new
AltMode variant, wide cargo-expand audit surface. Mitigation: ship behind
a runtime-on/runtime-off `BBNF_BITMAP=on/off` env var during the iteration
phase; flip on by default once the round-trip test plus all bench gates
are green for two days; remove the env var before AA.19.

### Phase AA.8 — Compile-time perfect-hash dispatch for keyword Alts

`__namedColor` in CSS L4 is a 230-way linear if-else over string literals,
~3.5 % of `css_tailwind` parse time. Any Alt of N ≥ 8 string literals is a
candidate. Today the codegen falls through to checkpoint chains. Use a
compile-time perfect hash (Lemire-style pthash, FCH, or BBHash — start
with the simplest correct one; benchmark; upgrade if needed).

The grammar e-graph's analysis tells us the variant count without
re-walking. The CSP from AA.5's `DispatchShareConstraint` machinery
becomes the natural producer: an Alt with all-Literal branches and
`branch_count ≥ perfect_hash_min_branches` (default 8) becomes a candidate
for `AltMode::PerfectHashDispatch(group_id)`. The CSP weighs it against
`ByteDispatch` (which loses for N ≥ 8 because the byte branches blow up
the i-cache) and `Checkpoint` (which always loses for keyword sets).

**Compile-time emission:**

New module `crates/core/src/backend/patterns/perfect_hash.rs` builds the
perfect hash table at compile time. Emits a `static [u32; N]` table + a
`static [&'static str; N]` keys array + a `match` over the index.

**Generated code shape:**

```rust
fn parse_named_color(state: &mut ParserState) -> Option<NamedColorVariant> {
    let key = scan_ident_or_keyword(state)?;
    let hash = perfect_hash_named_color(key);
    let idx = (hash % N) as usize;
    if NAMED_COLOR_KEYS[idx] != key { return None; }
    Some(NAMED_COLOR_VARIANTS[idx])
}
```

**Files (net-new):**
- `crates/core/src/backend/patterns/perfect_hash.rs` — generator + emission helpers.
- `crates/core/tests/perfect_hash_dispatch.rs` — round-trip test.

**Files modified:**
- `crates/ir/src/passes/csp_strategy/mod.rs` — `AltMode::PerfectHashDispatch`.
- `crates/core/src/backend/strategy/alt_strategy.rs` — wire the new mode.
- `crates/core/src/backend/rust/emitter/dispatch.rs` — emit static tables + match.
- `crates/egraph/src/cost_weights.rs` — `perfect_hash_min_branches`,
  `perfect_hash_lookup_cost`.
- `crates/core/tests/recognizer_decision_consumption.rs` — Y.13 extension.

**Profile-measured impact target:**
- `css_tailwind` parse: −3 to −5 % (the namedColor cliff).
- `css_bootstrap` parse: −2 to −4 % (CSS property keyword dispatch).
- Cargo expand `__namedColor`: ~230 if-else branches → static tables +
  single match.
- Samply: `__namedColor` self-time on `css_tailwind` drops from ~3.5 % to
  < 0.5 %.

**Risk**: LOW-MEDIUM. Perfect-hash construction is well-understood; the
risk is in the construction-vs-runtime trade-off (perfect hashes can have
expensive build steps). Mitigation: use a simple FCH or BBHash variant
that's fast at build time and accept slightly larger tables in exchange.

### Phase AA.9 — Surgical BoxedEnum → TaggedUnion narrowing (the stopgap that survives Act III)

The AA-prototype's surgical patch (return `Enum` instead of `BoxedEnum`)
mostly works but leaves the heterogeneous case under-typed. The right fix
is to track the actual variant set and let `child_alloc` decide based on
cardinality + size.

This phase lands AS A STOPGAP. After Act III deletes the eager AST
emitter, `TaggedUnion` is no longer load-bearing for parse-time (the tape
removes the alloc entirely). But the type narrowing remains useful for
type-system precision (tighter generated `enum` definitions, smaller
enum discriminants in the View types). It survives Act III as a
type-system improvement.

Introduce `TypeDesc::TaggedUnion(SmallVec<[TypeDescId; 8]>)`. Semantics:
an ordered set of distinct types. Constraints:
- Cardinality ≤ 8 (above which we fall back to `BoxedEnum` per current
  behaviour).
- All variants must be inline-storable (Span, scalar, ε) for the Inline
  placement to be chosen by `child_alloc`. A union containing a `Vec` or
  another `TaggedUnion` gets `Alloc`.

Modify `join_types` at
`crates/ir/src/passes/types/constraint/helpers.rs:86-96`:

```rust
pub(super) fn join_types(branch_types: &[&TypeDesc], interner: &mut TypeInterner) -> TypeDesc {
    if branch_types.is_empty() { return TypeDesc::Tuple(vec![]); }
    let first = branch_types[0];
    if branch_types.iter().all(|t| *t == first) { return first.clone(); }
    let distinct: SmallVec<[TypeDescId; 8]> = unique_intern(branch_types, interner);
    if distinct.len() <= 8 { TypeDesc::TaggedUnion(distinct) } else { TypeDesc::BoxedEnum }
}
```

Modify `child_alloc` at `crates/core/src/backend/types/decisions.rs`:

```rust
pub fn child_alloc(ty: &TypeDesc, parent: ValuePlacement, ir: &GrammarIR) -> ValuePlacement {
    match ty {
        TypeDesc::TaggedUnion(variants) => {
            if variants.iter().all(|v| ir.resolve(*v).is_inline_storable()) {
                ValuePlacement::Inline
            } else {
                ValuePlacement::Alloc
            }
        }
        TypeDesc::BoxedEnum => ValuePlacement::Alloc,
        TypeDesc::Enum if parent == ValuePlacement::Alloc => ValuePlacement::Alloc,
        TypeDesc::Vec(inner) if **inner != TypeDesc::Span => ValuePlacement::Alloc,
        _ => ValuePlacement::Inline,
    }
}
```

Add `TypeDesc::is_inline_storable() -> bool` returning `true` for `Span`,
scalar types (`f64`, `bool`, `u8`...), `Option<inline>`, and
`TaggedUnion(all_inline)`.

`TypeInterner` lives on `GrammarIR` and is consulted for the
`TypeDescId → &TypeDesc` resolution. This is the prerequisite that the
the user's draft Phase 4 (imperative pass deletion) didn't quite cover —
without interning, the `TaggedUnion(SmallVec<TypeDescId; 8>)` payload has
no anchor.

**Files modified:**
- `crates/ir/src/types/{type_desc.rs, grammar.rs}` — `TaggedUnion` variant,
  `TypeInterner`, `is_inline_storable()`.
- `crates/ir/src/passes/types/constraint/{helpers,alt,seq}.rs` — narrow
  `join_types` + propagate.
- `crates/core/src/backend/types/decisions.rs` — `TaggedUnion` arm.
- `crates/core/src/backend/rust/emitter/alt.rs` — recognise `TaggedUnion`
  + `Inline` and emit the inline path.
- `crates/core/src/backend/rust/ir_types.rs` — `TaggedUnion → Rust type`
  mapping.
- `crates/core/src/generate/serialize/serialize.rs` — `TaggedUnion` is a
  value type when all variants are inline-storable.
- `crates/ir/tests/types.rs` — assertions.
- `crates/core/tests/recognizer_decision_consumption.rs` — Y.13
  extension.

**Profile-measured impact target:**
- `json_twitter` parse: −8 to −12 % (the per-pair value box becomes inline).
- `json_citm` parse: −5 to −8 %.
- `json_canada` parse: −2 to −4 % (compounds with AA.7).
- Cargo expand `__pair` in `json_monolithic`: zero `&*slab().alloc(__v)` patterns.
- Samply: `slab::alloc` self-time on `json_twitter` drops from ~12 % to
  < 2 % even before the tape.

**Risk**: MEDIUM-HIGH. The blast radius is wide — `TypeDesc` is touched by
every backend, the serialiser, the recogniser plan, and the Y.13
invariant. Mitigation: the analysis fact `cardinality` from AA.2 — the
join no longer needs to recompute the variant set, it queries the
e-class analysis. Plus the type-interner from this phase prevents the
clone explosion the constraint pass would otherwise pay.

**Act II checkpoint**: After AA.9, expect `json_canada` ≥ 1.6 GB/s,
`css_tailwind` ≥ 0.36 GB/s. The floor gates are met *without* the tape.
This is the safety property that makes Act III's fall-back option real.

---

## Act III — Tape transposition (Phases AA.10–AA.14)

This is the act that lifts us off the typed-AST ceiling. By the end of
Act III, the Rust monolithic backend emits a tape-builder + a lazy view,
the eager AST is gone, per-element slab allocation drops to ~0 in the
parser hot loop. The contract `Parser::parse(input) -> Result<RootView<'tape>, ParseErr>`
is preserved.

**Explicit fall-back**: If Phase AA.11's parity gate fails after **two**
attempts, the tranche removes Acts III from the active scope, the
half-built `bbnf-tape` crate from Phase AA.10 stays in-tree as
groundwork, and the tape becomes Tranche AC. Acts I + II + IV (minus
Phase AA.16, which depends on the tape) + V land regardless.

### Phase AA.10 — `bbnf-tape` leaf crate + format spec + chunked arena strategy

Define the tape format. New leaf crate `crates/bbnf-tape/` (per
`general-infra-crates`) with no bbnf dependencies. The crate exports the
`Tape`, `TapeRec`, `TapeOffset`, `TapeKind` types, plus a `TapeBuilder`
API for the runtime.

**Format** (the actual record sizes are subject to refinement during AA.10
based on measured distributions across the JSON/CSS/BBNF benches —
measure first, freeze second; the freeze is a gate for AA.10 closing):

```rust
// crates/bbnf-tape/src/lib.rs
#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct TapeRec {
    pub kind: TapeKind,    // u8: rule kind discriminant (codegen assigns)
    pub flags: u8,         // bitfield: variant index, has-children, span-only
    pub span_lo: u32,      // input byte offset (little-endian)
    pub span_hi: u32,      // length OR child-end-offset for compound nodes
    pub child_off: u32,    // offset of first child record in `tape.recs`, or u32::MAX
    pub _pad: u32,         // align to 24 bytes; future flag space
}

pub struct Tape {
    pub recs: ChunkedArena<TapeRec>,  // chunked: 64KB chunks, no realloc-copy
    pub strings: bumpalo::collections::Vec<u8>,  // span content (zero-copy slices into input)
}

pub struct TapeBuilder<'arena, 'input> {
    pub tape: Tape,
    pub input: &'input [u8],
    pub arena: &'arena bumpalo::Bump,
    pub error: Option<ParseErr>,  // sticky error; checked at sub-tree exits
}
```

**`ChunkedArena<T>`**: The slab strategy is the key Phase AA.10
deliverable. A simple `Vec<TapeRec>` would force `Vec::reserve` /
realloc-copy on every push beyond capacity, and json_canada needs ~24 MB
of tape (a 1M-element JSON array → ~1M+ records × 24 bytes). Realloc-copy
on every doubling is a big cost.

`ChunkedArena<T>` allocates 64 KB chunks (~2700 records each). On push,
it appends to the current chunk; when full, it allocates a new chunk
from the same `bumpalo::Bump`. Indexing by `TapeOffset` is `(chunk_idx,
within_chunk)` decoded via right-shift + mask. The view layer always
consults via the offset → chunk decoder, which is a single shift + mask
+ pointer load.

**Files:**
- `crates/bbnf-tape/Cargo.toml` — leaf crate, deps: `bumpalo` only.
- `crates/bbnf-tape/src/{lib.rs, tape.rs, builder.rs, chunked_arena.rs}` — full crate (~700 LOC).
- `crates/bbnf-tape/tests/tape_basic.rs` — round-trip test for hand-crafted small tape.
- `crates/bbnf-tape/tests/chunked_arena_capacity.rs` — assert push performance is O(1) amortised, no realloc-copy regressions.
- `Cargo.toml` (workspace) — add `bbnf-tape` member.
- `crates/core/Cargo.toml`, `crates/derive/Cargo.toml`, `crates/ir/Cargo.toml` — declare `bbnf-tape` dep.

**Format-freeze gate (this phase only)**: Build a small benchmark using the
`json_canada` and `css_tailwind` inputs that walks the eager AST and
synthesises tape records of various candidate widths (16 / 24 / 32 bytes).
Measure the tape footprint and the cache-line behaviour. Freeze the
record format that has the best space × decode-cost product. The current
24-byte design is the starting point; revise if measurement says
otherwise.

**Gate**: `cargo test -p bbnf-tape` passes; round-trip test verifies a
hand-built tape decodes to the expected accessor results;
`chunked_arena_capacity` test asserts O(1) amortised push.

**No backend code touched yet.** The crate exists, ready for AA.11.

**Risk**: LOW. New leaf crate; no consumer migration yet.

### Phase AA.11 — `TapeBuilder` emitter (parser side, dual-mode)

Add the tape-builder emitter mode alongside the existing eager AST
emitter (which still runs as the default — flipped in AA.14). Every Rust
monolithic emitter file gets a `tape_*` sibling that emits direct
tape-write code instead of typed enum construction.

**Files** (per `directory-module-structure`):
- `crates/core/src/backend/rust/tape/mod.rs` — `TapeEmitter` trait + dispatch.
- `crates/core/src/backend/rust/tape/{leaves.rs, seq.rs, binary.rs,
  repeat.rs, alt.rs, operator_chain.rs, map_value.rs, grammar.rs}` —
  per-kind tape-emit siblings.
- `crates/core/src/backend/rust/tape/dispatch.rs` — tape-aware Alt dispatch
  (consults `AltMode::BitmapDispatch` from AA.7 if the bitmap is
  available).
- `crates/core/src/backend/rust/tape/strategy.rs` — `MonoCtx` analogue
  carrying tape-builder local state.
- `crates/core/src/backend/driver/mod.rs` — driver gets a
  `BackendMode::Tape | BackendMode::EagerAST` enum; codegen pipeline
  picks per-grammar (default `EagerAST` until AA.14).
- `crates/core/src/pipeline/compile.rs` — wire the mode through; default
  reads `BBNF_BACKEND_MODE` env var or grammar `@tape` directive.

**Generated code shape** (for a JSON `pair`):

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

**No `slab.alloc(enum_variant)`. No closures. No IIFEs. Direct tape writes.**

**Gate (the parity gate that decides Act III's fate):**
- `cargo expand -p bbnf --bench json_monolithic 2>&1 | grep '__pair'`
  produces the tape-builder shape above (no `.map(...slab.alloc)` pattern,
  no `Self::Variant(...)` construction in the parser body).
- `cargo test -p bbnf` passes (the emitter is opt-in via
  `BBNF_BACKEND_MODE=tape`; default still `EagerAST`).
- A new `crates/core/tests/tape_emitter.rs` compiles a small grammar in
  tape mode and verifies the parser produces the expected tape contents
  on hand-crafted input.
- A `tape_eager_parity` test compiles every production grammar in BOTH
  modes and asserts the parsed structure matches accessor-by-accessor on
  20+ sample inputs per grammar.

**Profile expectation**: with `BBNF_BACKEND_MODE=tape`, `json_canada`
parse +30 to +60 % throughput in early measurement against synthetic
full-tape walkers. (The view side isn't generated yet, so this measures
the raw tape-write speed. This is not the final number; AA.12 measures
end-to-end after the View generator lands.)

**Fall-back trigger**: If two attempts to land AA.11 fail to pass the
`tape_eager_parity` test (i.e. the tape parser produces incorrect output
on ≥ 1 grammar), the Act III scope is removed. Acts I + II + IV (minus
AA.16) + V land. Tranche AA closes with the Act II floor gates and the
substrate work. The half-built `bbnf-tape` crate stays in tree; Tranche
AC picks it up.

**Risk**: HIGH. New emitter; parallel to old. Mitigation: dual-mode +
extensive cargo-expand snapshots in `docs/benchmarks/expand/AA.11/` +
the parity test running on the full bench corpus + the explicit
fall-back option.

### Phase AA.12 — `TapeView` generator (AST side, lazy view types)

Generate the `impl` blocks that expose the typed AST as a lazy view over
the tape. For each rule with a non-Span projection, the codegen emits a
`MyRuleView<'tape>` struct + accessor `impl`s. The `bbnf-derive` macro is
updated to consume these and produce the same public surface as before
(modulo the `'tape` lifetime parameter on view types).

**Files:**
- `crates/core/src/backend/rust/view/mod.rs` — `ViewEmitter` trait.
- `crates/core/src/backend/rust/view/{leaves.rs, seq.rs, alt.rs,
  repeat.rs, grammar.rs, projection.rs}` — per-kind view-emit siblings +
  the projection logic.
- `crates/derive/src/lib.rs` — `#[derive(Parser)]` consumes the view
  emitter; macro output references `RootView<'tape>` instead of an owned
  typed tree. Public API: `Parser::parse<'i>(input: &'i [u8]) ->
  Result<RootView<'i>, ParseErr>` (the lifetime is tied to the input
  slice via the bumpalo arena).

**Generated view shape:**

```rust
// for `pair = string ":" value`
#[derive(Clone, Copy)]
pub struct PairView<'tape> { pub(crate) tape: &'tape Tape, pub(crate) rec: TapeOffset }

impl<'tape> PairView<'tape> {
    #[inline] pub fn key(&self) -> StringView<'tape> {
        StringView::from_tape(self.tape, self.tape.child(self.rec, 0))
    }
    #[inline] pub fn value(&self) -> ValueView<'tape> {
        ValueView::from_tape(self.tape, self.tape.child(self.rec, 1))
    }
}

#[derive(Clone, Copy)]
pub struct ObjectView<'tape> { pub(crate) tape: &'tape Tape, pub(crate) rec: TapeOffset }

impl<'tape> ObjectView<'tape> {
    pub fn pairs(&self) -> impl Iterator<Item = PairView<'tape>> + 'tape {
        let start = self.tape.child_offset(self.rec, 0);
        let end = self.tape.child_offset(self.rec, 1);
        (start..end).map(move |off| PairView { tape: self.tape, rec: TapeOffset(off as u32) })
    }
}
```

**Gate:**
- `cargo test --workspace` passes with `BBNF_BACKEND_MODE=tape` set globally.
- `cargo bench json_monolithic` shows ≥ +50 % throughput on `json_canada`
  over pre-AA baseline (this is the first measurement of end-to-end tape
  parser + view).
- The `tape_eager_parity` test from AA.11 still passes via accessor
  comparison instead of structural comparison.

**Profile expectation**: `json_canada` 1.8–2.4 GB/s (vs 1.2 GB/s
baseline); `json_twitter` 2.0–2.8 GB/s. Samply shows `BumpSlab::alloc` at
< 1 % self-time on json_canada (vs 10–15 % baseline).

**Risk**: HIGH. The contract preservation is the bet. Mitigation: the
parity test from AA.11 still runs, plus the explicit consumer migration
in AA.13 catches everything the parity test misses (anything that
pattern-matches on enum variants instead of using accessors).

### Phase AA.13 — Consumer migration (gorgeous, lsp, debugger, bbnf-ser, prettify)

This phase exists because the tape view's lifetime parameter is real, not
hidden. Internal consumers that name the typed AST in function signatures
need a `'tape` parameter. Some consumers pattern-match on enum variants
in ways that don't migrate mechanically. This phase audits and migrates
each consumer in this commit, with all tests green under
`BBNF_BACKEND_MODE=tape`.

The five consumers and their migration shapes:

**(a) `gorgeous`**: grammar-driven formatter. Pattern-matches on enum
variants in the bbnf-grammar formatters. Migration: convert
`match v { Value::String(s) => use s }` to
`match v.kind() { ValueKind::String => { let s = v.as_string().unwrap(); use s } }`.
Where the inner binding chains accessors, the conversion is mechanical.
Where it holds the bound value across complex control flow, the
migration may need a `let s = v.as_string()?;` early-bind. Audit count:
estimated ~20–30 sites across the gorgeous codebase. Each is local.

**(b) `lsp`** (`crates/lsp/src/`): the IR-backed analysis layer at
`crates/lsp/src/state/diagnostics/ir_analysis.rs` consumes the type
projection and the IR meta — none of this pattern-matches on parser
output, so the migration is just lifetime-parameter threading on the
public types it returns. Audit count: ~5–10 sites.

**(c) `bbnf-debugger` / DAP**
(`crates/lsp/src/dap/{adapter.rs, mapping.rs}`): translates between IR
nodes and DAP positions. Lifetime parameter threading; no enum pattern
matches. Audit count: ~5 sites.

**(d) `bbnf-ser` consumers**
(`crates/core/src/generate/serialize/serialize.rs`): writes serialiser
impls for grammar-projected types. The serialiser body migrates from
owned-types to view-types: `obj.field` becomes `obj.field()` (an accessor
call). The trait surface is unchanged. Audit count: ~10 sites in the
generator + however many the `bbnf-ser` trait test fixtures use.

**(e) `prettify`** (`crates/core/src/backend/rust/emitter/prettify/`):
fuses parse + format. The current eager-AST prettify path is the
deepest consumer of the typed tree — it generates `to_doc()` impls per
rule that walk the tree structure. Migration: the prettify codegen path
becomes a sibling of the view emitter
(`crates/core/src/backend/rust/view/prettify/`) that generates `to_doc`
impls over the view types. Lifetime parameter threading + accessor
calls. Audit count: ~30–50 sites across the prettify subtree. **This is
the biggest single migration in AA.13.**

**Two-day green-CI requirement**: AA.13 closes only when
`BBNF_BACKEND_MODE=tape cargo test --workspace` runs green for two
consecutive days in CI. AA.14's deletion is gated on this.

**Files modified** (estimated bounded scope, per the audit):
- `crates/gorgeous/src/**` — ~20–30 sites.
- `crates/lsp/src/{analysis,state,dap}/**` — ~10–15 sites.
- `crates/core/src/generate/serialize/serialize.rs` — ~10 sites.
- `crates/core/src/backend/rust/emitter/prettify/**` — ~30–50 sites
  (migrated to a new sibling at `view/prettify/`).
- All consumer test fixtures that name view types in their public surface.

**Gate**: full workspace test pass under `BBNF_BACKEND_MODE=tape`;
prettify-mode end-to-end smoke test on JSON / CSS / EBNF generates
identical output to the eager mode (the prettify Doc output is
deterministic, so this is a strong correctness check).

**Risk**: MEDIUM-HIGH. Wide blast radius across consumer crates. Mitigation:
each consumer crate has its own test suite + the parity test from AA.11/AA.12
runs every consumer. Two-day CI window catches anything the local tests
miss. The `BBNF_BACKEND_MODE` env var means rollback is one config flip.

### Phase AA.14 — Flip default to tape, delete eager AST emitter

Switch `BackendMode::Tape` to default. Delete the eager AST emitter
(`crates/core/src/backend/rust/emitter/`) — all of it, ~5000 LOC. Per
`no-legacy-code`, no parallel path survives.

**Files deleted:**
- `crates/core/src/backend/rust/emitter/{mod.rs, leaves.rs, seq.rs,
  binary.rs, repeat.rs, alt.rs, operator_chain.rs, map_value.rs,
  grammar.rs, dispatch.rs}` — entire eager emitter (~5000 LOC).
- `crates/core/src/backend/rust/emitter/prettify/` — eager-AST prettify
  path. The migration to `view/prettify/` happened in AA.13.
- `crates/core/src/backend/types/decisions.rs` legacy `child_alloc` arms
  for the eager case (the AA.9 `TaggedUnion` arm survives — it's still
  used by the View emitter for type narrowing).
- `crates/core/src/backend/driver/{alt,seq,repeat,wrap}.rs` legacy
  `BoxedEnum` match arms (the AA.9 `TaggedUnion` arms survive).

**Files modified:**
- `crates/core/src/backend/rust/mod.rs` — `pub mod tape;` + `pub mod view;`
  replace `pub mod emitter;`.
- `crates/core/src/backend/driver/mod.rs` — drop `BackendMode` enum (only
  one mode now); driver always emits tape.
- `crates/derive/src/lib.rs` — drop the eager-mode codegen branch.
- `crates/core/src/pipeline/compile.rs` — drop the `BBNF_BACKEND_MODE`
  env var; tape is the only mode.

**Gate:**
- `cargo test --workspace` passes.
- `cargo bench` post-AA.14 numbers committed to
  `docs/benchmarks/post-act-III.json`.
- `cargo expand` snapshots verify the eager pattern is GONE everywhere.
- Every consumer-invariant test from prior tranches still passes (with
  the variants they reference now living in `view/` not `emitter/`).

**Profile expectation**: same as AA.12 (the default flip is mechanical).
The win is the LOC deletion + the `BBNF_BACKEND_MODE` removal.

**Risk**: HIGH. The mass deletion is permanent. Mitigation: AA.13 ran in
CI with `BBNF_BACKEND_MODE=tape` set for two days; AA.14 just removes the
toggle. If any test fails post-flip, AA.14 rolls back, AA.13 is reopened
to find what the consumer migration missed, and the flip retries after
fix.

---

## Act IV — SIMD + dispatch refinements (Phases AA.15–AA.16)

These two phases compose with the bitmap (AA.7) and the tape (AA.14) but
add only modest risk on their own. They land after the tape so they can
exploit it.

### Phase AA.15 — Subtree skipping + branch-free dispatch ladder

Two related wins on top of the bitmap.

**Subtree skipping**: when a `BalancedWrap` rule's open/close are in the
bitmap, the runtime can skip directly from open to matching close via
match-bracket counting on the bitmap
(`StructuralBitmap::match_bracket`). For now this eliminates the
inner-Alt loop in any wrap-then-content pattern. (Lazy parsing for
sub-tree skip-without-decode is out of scope this tranche; the
infrastructure for it is present.)

**Branch-free dispatch ladder**: today `ByteDispatch` emits
`match byte { ... }` which LLVM compiles to a jump table for ≥ 8 arms but
a branch chain for 2–7. For 4–7 arms, emit a precomputed
`static [u8; 256]` lookup table indexed by byte, with
`unsafe { *TABLE.get_unchecked(byte as usize) }` followed by a single
dispatch on the discriminant. Faster on M3, equivalent on x86.

**New CSP axis**: `DispatchKernel::{Match, LookupTable, Bitmap}` decides
per dispatch site.

**Files:**
- `crates/core/src/backend/rust/tape/dispatch.rs` — emit lookup-table form
  when `DispatchKernel::LookupTable`; emit bitmap-skip when
  `RecognizerShape::StructuralBitmap` has `matched_brackets: Some(_)`.
- `crates/ir/src/passes/csp_strategy/mod.rs` — add `DispatchKernel`
  variable to the strategy CSP; cost-model picks the kernel.
- `crates/egraph/src/cost_weights.rs` —
  `dispatch_kernel_lookup_table_threshold: usize = 4` (Alt arity at which
  lookup table is preferred).

**Gate**: `cargo expand` shows the new lookup-table dispatch in JSON's
`__value`. `samply` shows reduced branch misses on `json_twitter` (the
most pair-heavy bench).

**Profile expectation**: `json_twitter` 2.5–3.0 GB/s (composes with
AA.7 + AA.12).

**Risk**: LOW. Pure dispatch shape change.

### Phase AA.16 — Slab pre-sizing via `SlabSizingAnalysis`

The tape `ChunkedArena<TapeRec>` allocates 64 KB chunks on demand. A new
`SlabSizingAnalysis` propagates a per-rule "expected record count
multiplier" from leaves up to the entry rule; the parser entry uses the
result to call `Tape::with_capacity(input_len * multiplier)`. This
eliminates the small overhead of the first few chunk allocations on long
inputs.

**Files:**
- `crates/ir/src/egraph/analyses/slab_sizing.rs` (new) — lattice element
  is `(min_recs_per_input_byte: f32, max_recs_per_input_byte: f32)`;
  merge takes (min, max) bounds.
- `crates/ir/src/egraph/analyses/mod.rs` — register the analysis (now
  Tuple6).
- `crates/core/src/backend/rust/tape/grammar.rs` — entry-rule emission
  consults the analysis result and emits `Tape::with_capacity(input.len()
  * EST_RECS_PER_BYTE)`.
- `crates/bbnf-tape/src/lib.rs` — add `Tape::with_capacity`.

**Gate**: `samply` shows `Vec::reserve` self-time drops to < 0.5 % on
`json_data_xl` (the largest bench).

**Profile expectation**: marginal but free. `json_data_xl` +1 to +2 %.

**Risk**: LOW.

---

## Act V — Cleanup + verification (Phases AA.17–AA.19)

### Phase AA.17 — Tier isomorphism enforcement (`OptimizableTier` shared trait)

Per `regex-crate-isomorphic`, the bbnf-regex HIR e-graph and the grammar
e-graph should expose identical APIs. Currently
`parse-that/rust/regex/src/egraph/mod.rs` has `build_hir_egraph` +
`saturate_hir_egraph` as separate calls; `crates/ir/src/egraph/mod.rs` has
`build_and_saturate`. Phase AA.17 unifies them behind a shared trait.

**Files:**
- `crates/egraph/src/lib.rs` — add `OptimizableTier` trait with `build`,
  `saturate`, `extract` defaults.
- `crates/ir/src/egraph/mod.rs` — implement; delete duplicated build/saturate body.
- `parse-that/rust/regex/src/egraph/mod.rs` — implement; delete duplicated body.
- `crates/egraph/tests/tier_isomorphism.rs` — assert both tiers expose the same trait.

**Gate**: tier-isomorphism test passes; `compile_json` (most regex-heavy)
within ±1 % of AA.16.

**Risk**: LOW.

### Phase AA.18 — Final deletion sweep

Final grep sweep for `#[deprecated]`, `// Tranche * removed`, `// Kept for
backward compat` introduced by W/X/Y/Z that this tranche removes. Plus
the legacy substrate that this tranche obsoletes.

**Files deleted:**
- `crates/core/src/backend/patterns/{cache,delim_scan,key_dispatch}.rs` —
  the detection halves flagged in CLAUDE.md as "deletion candidates for
  the follow-up tranche once strategy solvers migrate to consume
  `ir.recognizer_decisions` directly." Tranche AA is that migration.
- `GrammarIR::has_family_recognizers` — the gate for the three deleted
  family recognizers; the driver now consults `AllocationContextAnalysis`
  directly.
- `crates/ir/src/passes/patterns/mod.rs:21-65` legacy types
  (`AltPattern`, `SeqPattern`, `PatternAnnotations`).
- `crates/ir/src/recognizer/` directory if it still exists (the pre-V.3
  substrate).
- `BBNF_BACKEND_MODE` env var (gone in AA.14, but verify no test still
  references it).
- `BBNF_BITMAP=on/off` env var (the AA.7 iteration toggle, gone).

**Net deletion target across the tranche**: ~5500–7000 LOC across all
phases (the eager emitter is the bulk). Net add: ~4000–5000 LOC (tape
crate, view emitter, analyses, new rules, bitmap scanner). **Net
reduction: ~1500–2000 LOC** while gaining sonic-rs-class throughput.

**Gate**: `cargo test --workspace` passes; `cargo clippy --all-targets
-- -D warnings` clean.

**Risk**: LOW (cleanup, not new behaviour).

### Phase AA.19 — post-AA bench sweep + verification + post-AA.json

Per Tranche Z's `tranche_length_notes` discipline. Every "+X %" claim
cited from a samply symbol delta in the post-AA profiles, not pre-Z
(which are sparse).

**Deliverables:**
- `docs/benchmarks/post-AA.json` — full bench numbers for all 11 benches
  × 3 runs each.
- `docs/benchmarks/profiles/post-AA/*.samply` + `*.syms.json`.
- `docs/benchmarks/expand/post-AA/*.rs` — final cargo-expand snapshots
  of all hot paths.
- Pre-Z → post-Z → post-AA full delta table in `post-AA.json`.
- Per-act delta breakdown (Act I architectural; Act II independent
  parse wins; Act III tape; Act IV refinements; Act V cleanup) so the
  contribution of each act is visible.

---

## Hard gates (full table)

| Gate | Threshold |
|---|---|
| All workspace tests pass | yes |
| `bbnf-ir`, `bbnf-tape`, `egraph`, `csp-solver`, `bbnf-regex` tests | all passing |
| Bootstrap script idempotent | yes |
| Y.13 consumer-invariant test | passes with `TaggedUnion`, `StructuralBitmap`, `BitmapDispatch`, `PerfectHashDispatch`, `LookupTableDispatch`, `BitmapSubtree`, `InlineOk`, `SpanProjectable`, `AllocationContext`, `SlabSizing` added |
| `every_recognizer_shape_has_a_consumer_ratio` | passes (≥ 80 % per shape) |
| `grep -rn "pub(super) fn collect" crates/ir/src/passes/recognizers/` | zero hits (Z.0 invariant preserved) |
| `grep -rn "NoAnalysis" crates/ ../parse-that/` | only the egraph crate's definition + tests; no production consumer |
| Eager AST emitter | DELETED (no `crates/core/src/backend/rust/emitter/` directory) |
| Imperative FIRST/FOLLOW/nullable passes | DELETED |
| `BBNF_BACKEND_MODE` env var | DELETED |
| `BumpSlab::alloc` self-time on `json_canada` | < 1 % |
| `StructuralBitmap::scan` self-time on `json_canada` | 8–18 % (new entry) |
| `__namedColor` self-time on `css_tailwind` | < 0.5 % (was ~3.5 %) |
| Cargo expand `__pair` in `json_monolithic` bench | no `.map(\|__v\| &*slab().alloc(__v))` pattern |
| Cargo expand `__namedColor` in `css_l4` bench | no linear `if name == "..."` chain ≥ 8 branches |
| `json_canada` parse | floor: ≥ 2.0 GB/s · target: ≥ 3.0 GB/s |
| `json_twitter` parse | floor: ≥ 2.1 GB/s · target: ≥ 2.8 GB/s |
| `json_citm` parse | floor: ≥ 2.4 GB/s · target: ≥ 3.0 GB/s |
| `css_tailwind` parse | floor: ≥ 0.36 GB/s · target: ≥ 0.50 GB/s |
| `css_bootstrap` parse | floor: ≥ 0.32 GB/s · target: ≥ 0.45 GB/s |
| `compile_bbnf` | ≤ 1.5× pre-AA |
| `compile_css_l4` | ≤ 2.0× pre-AA |
| `BBNF_EGRAPH_REPORT=1` | prints non-zero per-rule fire counts |
| `BBNF_PIPELINE_REPORT=1` | prints CSV per compile |
| `BBNF_CSP_REPORT=1` | zero budget exhaustions on standard benches |
| Every "+X %" claim in `post-AA.json` | cites a samply symbol + self-time delta |

---

## Compile-time budget

The user accepts compile-time degradation. The plan respects a tighter
ceiling than the draft (1.5× / 2× instead of 2× / 3×) because LSP
felt-perf is a non-negotiable user-facing surface.

| Component | Estimated compile-time delta | Bounding mechanism |
|---|---|---|
| Live `GrammarAnalysis` (5 sub-analyses) | +20–35 ms on `compile_css_l4` | `analysis_iter_budget: usize = 24` cap |
| B&B `OptimalExtractor` | +5–15 ms on `compile_css_l4` | `extraction_budget_nodes: usize = 60_000` cap; falls back to greedy on exhaustion |
| Cross-rule CSP B&B | +3–10 ms on `compile_css_l4` | `csp_budget_nodes: u64 = 250_000` cap; per-component decomposition keeps each B&B small |
| New e-graph rules (5) | +3–8 ms on `compile_css_l4` | `egraph_iter_limit` cap |
| Tape codegen (replaces eager) | net 0 to -10 ms | view emitter is simpler than eager (less type cascading) |
| Bitmap miner | +1 ms | one-shot, post-Z.0 unified walk |
| Perfect-hash table generation | +2–5 ms on `compile_css_l4` (CSS has the most keyword Alts) | only fires on N ≥ 8 keyword Alts |
| **Total worst case on `compile_css_l4`** | +33–73 ms (currently ~9 ms) | ~4–8× — exceeds the 2× target |

**Budget mitigation strategy**: every new analysis / rule / extraction
mode is gated behind a `CostConfig::*_budget` knob. Default budgets keep
`compile_bbnf ≤ 1.5×` and `compile_css_l4 ≤ 2×` pre-AA. Tighter budgets
via `BBNF_COST_*` env vars trade optimisation for speed. If the worst-case
exceeds the budget, AA.18 includes a tightening pass: reduce
`extraction_budget_nodes` to 30_000, reduce `csp_budget_nodes` to
100_000, accept the resulting 5–10 % parse-time loss in exchange for
hitting the compile-time budget.

**Hard gates**: `compile_bbnf ≤ 1.5×`, `compile_css_l4 ≤ 2×`. If a phase
blows the budget, the phase is reworked, not just accepted.

---

## Verification methodology (per the user's "do it now, not later" directive)

Every phase has THREE verification steps run BEFORE the phase commits:

1. **`cargo expand` diff** — diff against the prior phase's expand
   snapshot. Reviewed in the commit. Lands in
   `docs/benchmarks/expand/AA.{N}/`.
2. **`samply` symbol delta** — fresh profile, top-30 symbols diffed.
   Lands in `docs/benchmarks/profiles/AA.{N}/`.
3. **`cargo test --workspace`** — every phase keeps the test suite
   green. Per `no-legacy-code`, an `#[allow(dead_code)]` anywhere in a
   deletion path is a test-failing regression.

**Pre-tranche** (Phase 0): fresh profiles for ALL 11 benches captured
before any code change. The user explicitly said "Profiling and testing
should NOT be deferred to be WITHIN the tranche."

**Per-phase** (Phases AA.1–AA.18): the three steps above. No phase ships
without all three artifacts.

**Post-tranche** (Phase AA.19): full bench sweep, full test suite, every
"+X %" claim cited from a profile symbol delta.

**Parity tests** are the load-bearing safety net for the substrate
awakening (AA.2, AA.4) and the tape transposition (AA.11, AA.12). They
run in CI for the duration of those phases. They are deleted in AA.18
once the legacy paths are gone.

---

## Critical files (load-bearing, in order of touch)

**Act I:**
1. `crates/egraph/src/analysis.rs` — AA.1 (`Ctx` GAT)
2. `crates/egraph-derive/src/lib.rs` — AA.1 (`#[derive(Analysis)]`)
3. `crates/ir/src/egraph/analyses/` — AA.2 (new directory, 5 files)
4. `crates/ir/src/egraph/mod.rs` — AA.2 (`NoAnalysis` → `GrammarAnalysis`)
5. `parse-that/rust/regex/src/egraph/analyses/` — AA.2 (HIR mirror)
6. `crates/egraph/src/extract_optimal.rs` — AA.3 (B&B extractor, new)
7. `crates/core/src/pipeline/compile.rs` — AA.4 (pipeline reorder, deletes)
8. `crates/ir/src/passes/sets/{first_sets,follow}.rs` — AA.4 (DELETE)
9. `crates/ir/src/passes/csp_strategy/mod.rs` — AA.5 (grammar-wide solve)
10. `crates/csp-solver/` — AA.5 (vendored from sibling repo)
11. `crates/ir/src/egraph/rules/{lookahead,factor_left,factor_right,alt_split_byte}.rs` — AA.6 (new rules)

**Act II:**
12. `parse-that/rust/parse_that/src/parsers/scan/structural_bitmap.rs` — AA.7 (NEW SIMD scanner)
13. `crates/ir/src/passes/recognizers/structural_bitmap.rs` — AA.7 (new miner)
14. `crates/core/src/backend/kernels/structural_bitmap.rs` — AA.7 (kernel)
15. `crates/core/src/backend/patterns/perfect_hash.rs` — AA.8 (perfect-hash generator)
16. `crates/ir/src/passes/types/constraint/helpers.rs` — AA.9 (`join_types` narrowing)
17. `crates/ir/src/types/{type_desc.rs, grammar.rs}` — AA.9 (`TaggedUnion` + `TypeInterner`)
18. `crates/core/src/backend/types/decisions.rs` — AA.9 (`TaggedUnion` arm)

**Act III:**
19. `crates/bbnf-tape/` — **AA.10 (NEW CRATE — the breakthrough substrate)**
20. `crates/core/src/backend/rust/tape/` — **AA.11 (new emitter directory)**
21. `crates/core/src/backend/rust/view/` — **AA.12 (new view emitter directory)**
22. `crates/derive/src/lib.rs` — AA.12 (`#[derive(Parser)]` consumes view emitter)
23. `crates/gorgeous/src/**`, `crates/lsp/src/**`, `crates/core/src/generate/serialize/serialize.rs` — AA.13 (consumer migration)
24. `crates/core/src/backend/rust/view/prettify/` — AA.13 (prettify path migrated from emitter/prettify/)
25. `crates/core/src/backend/rust/emitter/` — **AA.14 (DELETE entire directory, ~5000 LOC)**

**Act IV:**
26. `crates/core/src/backend/rust/tape/dispatch.rs` — AA.15 (lookup-table dispatch)
27. `crates/ir/src/egraph/analyses/slab_sizing.rs` — AA.16 (new analysis)

**Act V:**
28. `crates/egraph/src/lib.rs` — AA.17 (`OptimizableTier` trait)
29. `docs/benchmarks/post-AA.json` — AA.19

---

## Risk register

| Phase | Risk | Failure mode | Mitigation |
|---|---|---|---|
| 0 | None | — | — |
| AA.1 | LOW | Mechanical signature errors | Workspace builds |
| AA.2 | MED-HIGH | Analysis ≠ imperative pass | `BBNF_ANALYSIS_DIFF=1` parity test in CI for one phase |
| AA.3 | MED | B&B incorrect | Dual-mode env var + unit tests |
| AA.4 | HIGH | Deleting load-bearing passes | Parity test must be green for two phases first |
| AA.5 | MED | Cross-rule CSP unsatisfiable | csp-solver fallback + diagnostic env var |
| AA.6 | MED | New rule correctness bug | Per-rule regression tests |
| AA.7 | MED-HIGH | Bitmap consultation wrong | `BBNF_BITMAP=off` env var iteration period |
| AA.8 | LOW-MED | Perfect-hash construction quality | Standard FCH/BBHash; benchmark vs trie |
| AA.9 | MED-HIGH | TypeDesc blast radius | `TypeInterner` contains the surface |
| AA.10 | LOW | New leaf crate | None — isolated |
| AA.11 | HIGH | New emitter wrong | Dual-mode + parity test on every grammar; **two-attempt fall-back** |
| AA.12 | HIGH | View accessors miscompile | Parity test extended to accessor comparison |
| AA.13 | MED-HIGH | Consumer migration drops a case | Two-day green CI gate before AA.14 |
| AA.14 | HIGH | Mass deletion permanent | AA.13's two-day CI is the prerequisite |
| AA.15 | LOW | Lookup-table dispatch shape | cargo-expand verification |
| AA.16 | LOW | Slab pre-sizing wrong | Default capacity stays as fallback |
| AA.17 | LOW | Tier isomorphism trait | Mechanical |
| AA.18 | LOW | Cleanup sweep | Workspace build + clippy clean |
| AA.19 | None | — | Verification only |

**Aggregate risk**: HIGH. The most architecturally aggressive tranche
bbnf has attempted. The mitigation strategy is:
- The parity tests (AA.2/AA.4 and AA.11/AA.12).
- The dual-mode period (AA.11–AA.14 with `BBNF_BACKEND_MODE`).
- The per-phase samply attribution.
- **The two-attempt fall-back at AA.11 — Act III becomes Tranche AC if
  the tape parser can't be made correct in this tranche.**
- The Act II floor gates being met *without* the tape (AA.7 + AA.8 +
  AA.9 alone deliver the floor gates).

No phase ships without all the mitigations. The fall-back is the
load-bearing structural property: the tranche delivers parse-time wins
even in the worst case.

---

## What this tranche deliberately does NOT include

Per `no-overfitting`. Each non-goal cites a tranche or memory that
established its dead-end status.

1. **Explicit NEON/SSE2 intrinsics for the byte-class LUT.** Z.1
   prototyped three variants; all regressed against Y.7's
   auto-vectorised LUT due to setup cost on M-series ARM. Memory
   `feedback_perf_breakthrough_accuracy` codifies this. The bitmap
   scanner in AA.7 is a NEW workload (long-run pre-scan over input),
   not a retry of Z.1.
2. **JSON-specific lazy parsing.** sonic-rs's lazy mode skips
   sub-objects until accessed; the tape view in AA.12 already provides
   on-demand decoding — that's the closest grammar-agnostic analogue.
3. **Per-element-type slabs.** Typed `Slab<T>` allocators are tempting
   but destroy locality and balloon `MonoCtx` size. The tape format has
   one record kind, one chunked arena. Done.
4. **Multi-threaded parsing.** simdjson's stage 1/2 split overlaps
   structural scan with value extraction. In bbnf-lang, "stage 2" is
   the user's accessor calls — they cannot be SIMDified generically
   without a runtime VM. Hard veto.
5. **ILP backend for extraction.** B&B with budget is sufficient for the
   e-graph sizes we see (100–10k nodes). An ILP solver dependency would
   be ~40k LOC for at most a 2 % quality gain. No.
6. **Pattern DSL with proc-macro frontend.** The imperative
   `Rewrite::search` style is verbose but gives line-level samply
   attribution. A DSL that generates anonymous closures defeats per-rule
   profiling. No.
7. **Memoisation cache upgrades.** `MemoStore` is bypassed by codegen
   except for left-recursive rules; the codegen path is what we're
   optimising.
8. **Direct-to-user-struct deserialisation (sonic-rs deserialise style).**
   Requires the user to pre-declare the schema. Our grammar IS the
   schema; the tape view IS the user-facing API.
9. **`CrossRuleInline` as an e-graph rewrite rule.** `inline_acyclic`
   stays a deterministic op 3 of the pipeline. LSP incremental analysis
   depends on stable codegen output across compiles of the same
   grammar.
10. **Profile-guided cost calibration loop.** Wiring runtime parse
    measurements back into compile-time cost weights is a tranche unto
    itself; AA ships the observability instrumentation that enables it
    (AA.0 + AA.5's `BBNF_*_REPORT` env vars).
11. **`@utf8` validation directive / `@lazy` directive.** Both are
    composable with the structural bitmap from AA.7 but add directive
    surface that interacts with the type system; deferred to AC.
12. **pclmulqdq string-interior bitmap.** The structural bitmap from
    AA.7 covers the dispatch case; pclmulqdq for quote-parity scanning
    is a JSON-string specialisation that pays only on JSON benches.

---

## The single-plan commitment + Act III fall-back

Per `single-plan-execution`: this is ONE tranche, not a sequence. Phase 0
is the prerequisite; AA.1–AA.6 are Act I; AA.7–AA.9 are Act II;
AA.10–AA.14 are Act III; AA.15–AA.16 are Act IV; AA.17–AA.19 are Act V.
The compile-time gates (`compile_bbnf ≤ 1.5×`, `compile_css_l4 ≤ 2×`)
apply to the WHOLE tranche, not per-phase. The parse-time gates apply
post-AA.19.

**Act III fall-back protocol**: If Phase AA.11's `tape_eager_parity` test
fails twice, the tranche removes Acts III from active scope and proceeds
directly from AA.9 to Phase AA.15 (still applicable to the eager
emitter), AA.17–AA.19. The half-built `bbnf-tape` crate from AA.10
stays in the tree as foundation. Tranche AC picks up the tape. The
tranche closes with the Act II floor gates met.

There is no "Tranche AB" planned to follow this. If the post-AA hot-path
numbers suggest a follow-up, that's a new audit and a new tranche. AA is
self-contained.

---

## Verification — how we know the plan worked

End state, post-AA.19:

1. `cargo bench json_canada` ≥ 2.0 GB/s (the floor; expected 2.5–3.0 if
   Act III lands).
2. `samply` profile of `json_canada` shows: `BumpSlab::alloc` < 1 %,
   `StructuralBitmap::scan` 8–18 %, recursive `__*` rule functions
   ≤ 32 % combined (vs current ~50 %+).
3. `crates/core/src/backend/rust/emitter/` does not exist (assuming
   Act III lands).
4. `crates/ir/src/passes/sets/{first_sets,follow}.rs` does not exist.
5. `grep -rn "NoAnalysis" crates/ ../parse-that/` returns only the
   egraph crate's definition + tests.
6. Y.13 consumer-invariant test extended to: `BitmapDispatch`,
   `LookupTableDispatch`, `BitmapSubtree`, `PerfectHashDispatch`,
   `TaggedUnion`, `InlineOk`, `SpanProjectable`, `AllocationContext`,
   `SlabSizing` — every new variant has a load-bearing consumer.
7. `compile_bbnf` ≤ 1.5× pre-AA; `compile_css_l4` ≤ 2× pre-AA.
8. `docs/benchmarks/post-AA.json` cites every "+X %" from a samply symbol
   + self-time delta (per `accurate-perf-narrative` +
   `truth-based-attribution`).
9. The plan rolled back zero phases. (If any phase rolled back —
   including the Act III fall-back — it's documented in `post-AA.json`
   with the rollback reason.)
