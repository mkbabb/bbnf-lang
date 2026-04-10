# Tranche AA — BoxedEnum Unboxing, Structural Bitmap, Cross-Rule CSP, E-graph Truth

## Context

Tranche Z closed with five phases delivered (Z.0 single-walk recognizer mining, Z.2 SWAR fractional digit-loop consistency, Z.5/Z.5b/Z.6 cumulative ghost-variant deletion across `CallStrategy`/`WrapMode`/four `strategy_*` cost knobs) and five deferred to AA (Z.1 SIMD ws, Z.3 single-variant inlining, Z.3b structural bitmap, Z.4 dispatch-fallback consolidation, deeper RefMode CSP work). The only landed parse-time win was json_canada −3.4% from Z.2; everything else is within noise. The Z audit revealed its original framings were based on misreading the JSON codegen and the cost-knob substrate, and the "real" bottlenecks are:

1. **JSON per-pair value boxing** — `.map(|__v| &*slab().alloc(__v))` at `__pair`'s expansion, forced by `child_alloc(BoxedEnum, _) = ValuePlacement::Alloc` **unconditionally** at `crates/core/src/backend/types/decisions.rs:40`. Meanwhile `child_alloc(Enum, parent)` inherits from parent. If `join_types` returns `Enum` instead of `BoxedEnum`, the cascade (Object Repeat → Pair Seq → Value child) collapses to inline storage because `pair` is a Tuple type with `body_alloc = Inline`. The fix is surgical at the producer, not at the consumers.

2. **CSP is using ~15-20% of its potential.** Constraint count per rule averages 1-3; all edges are intra-rule; the solver's fast-path (`decode_min_cost_per_variable`) fires whenever `constraints_added == 0`, which is most of the time. The Y.5 connected-components substrate is dormant.

3. **E-graph fire counts are opaque.** No instrumentation in `BackoffScheduler`; the 5 grammar-tier rules + HIR-tier rules may or may not be firing. We cannot prune or strengthen without measurement.

4. **No structural pre-scan infrastructure.** sonic-rs's grammar-agnostic bitmap pre-scan has no analogue — grep for `structural_bitmap` / `pre_scan` in the backend returns zero hits. The JSON `__value` hot loop + CSS `__value`/`__declaration` deep checkpoint chains both benefit from a fixed-set structural-byte bitmap over the input buffer.

5. **PunctWsRegion mines 56× on CSS L4 but most hits are filtered** by the `!ir.has_family_recognizers` gate plus Inline-alloc context checks. The miner identifies matches the backend never emits — dormant recognition with real compile-time cost.

6. **Residual type clones in `backend/driver/mod.rs:68-69`** — the `get_rule_type` lookup clones `TypeDesc` per rule during emit. Post-Y.10 cleanup missed this hot loop.

Tranche AA's job is to land the Z.3 BoxedEnum refactor (surgically, via the producer + decision layer only), introduce the grammar-agnostic structural bitmap pre-scan, add cross-rule CSP constraints that exercise the dormant solver substrate, instrument the e-graph to make fire counts legible, prune the PunctWsRegion miner's dead matches, and strip the residual type clones — all grounded in direct source inspection + cargo expand + samply profiles, not inherited audit claims.

---

## Profiling methodology

Unchanged from Tranche Z. Pre-AA baselines already exist at `docs/benchmarks/profiles/post-Z/*.samply` and serve as AA's pre-baseline. Post-AA profiles land at `docs/benchmarks/profiles/post-AA/*.samply`. Every "+X%" claim in `post-AA.json` must cite a symbol name + self-time delta from the profile diff.

### Build + resolve + record

```bash
cargo bench -p bbnf --bench compile_pipeline --no-run
cargo bench -p bbnf --bench json_monolithic --no-run
cargo bench -p bbnf --bench css_l4 --no-run

BIN=$(find target/release/deps -maxdepth 1 -type f -perm -111 -name 'compile_pipeline-*' ! -name '*.d' ! -name '*.dSYM' | xargs ls -t | head -1)
JSON_BIN=$(find target/release/deps -maxdepth 1 -type f -perm -111 -name 'json_monolithic-*' ! -name '*.d' ! -name '*.dSYM' | xargs ls -t | head -1)
CSS_BIN=$(find target/release/deps -maxdepth 1 -type f -perm -111 -name 'css_l4-*' ! -name '*.d' ! -name '*.dSYM' | xargs ls -t | head -1)

xcrun dwarfdump --uuid "$BIN" "$BIN.dSYM/Contents/Resources/DWARF/$(basename "$BIN")"  # UUIDs must match

samply record --save-only --unstable-presymbolicate -o docs/benchmarks/profiles/post-AA/compile_css_l4.samply -- "$BIN" compile_css_l4 --bench
samply record --save-only --unstable-presymbolicate -o docs/benchmarks/profiles/post-AA/compile_bbnf.samply -- "$BIN" compile_bbnf --bench
(cd crates/core && samply record --save-only --unstable-presymbolicate -o ../../docs/benchmarks/profiles/post-AA/json_canada.samply  -- "$JSON_BIN" canada  --bench)
(cd crates/core && samply record --save-only --unstable-presymbolicate -o ../../docs/benchmarks/profiles/post-AA/json_twitter.samply -- "$JSON_BIN" twitter --bench)
(cd crates/core && samply record --save-only --unstable-presymbolicate -o ../../docs/benchmarks/profiles/post-AA/css_tailwind.samply  -- "$CSS_BIN" tailwind  --bench)
```

### Bench sweep invariant

Parse benches MUST run in a single `"$BIN" --bench` invocation (no per-bench subprocess) because the bencher crate's cold-start overhead contaminates per-invocation timings. Post-Z discovered that individual `"$BIN" citm --bench` runs produce ~5-12% slower numbers than the single-invocation sweep — this is a process-startup artifact, not a real regression.

### Pre-commit profile check

Every phase that claims a parse-time or compile-time delta must land with:
1. A pre/post bench sweep showing the delta above the noise floor (±1.5% for parse, ±5% for compile on this machine).
2. A cargo-expand diff showing the generated code change (for codegen phases).
3. A samply symbol delta showing the expected hot-path movement (e.g., `slab::alloc` self-time drops).

---

## Architectural commitments

1. **No legacy code, no workarounds.** Every phase lands clean; no `#[deprecated]` aliases, no transitional shims.

2. **Truth-based attribution.** Every "+X%" claim in post-AA.json cites a samply profile line. Phases that claim improvements but can't cite a profile delta are re-opened or marked "architectural win only".

3. **Surgical over rewrite.** AA.1 (the BoxedEnum refactor) is deliberately surgical: change the producer + one decision-layer function, delete dead conversion branches downstream, update test assertions. The `TypeDesc::BoxedEnum` variant itself is NOT deleted in AA — that's a follow-up after the producer change proves stable across the full bench + test suite. This prevents the multi-file dispatch-table cascade that blocked Z.4.

4. **Grammar-agnostic, not JSON-specific.** The structural bitmap pre-scan (AA.2) detects ANY grammar with a small fixed delimiter set — JSON, CSS selectors, SQL keywords, EBNF tokens all opt in automatically.

5. **Zero ghost substrate — still.** Every new variant/enum/CSP variable has a load-bearing consumer before the commit that introduces it. Y.13 consumer-invariant test extends to any new decision surface. A new `every_recognizer_shape_has_a_consumer` test lands as part of AA.4 to catch the PunctWsRegion-style mined-but-not-emitted case at compile time.

6. **Cross-rule CSP is the real universal cost model.** AA.3's goal isn't new variable classes — it's new constraint topology. The Y.5 `components::UnionFind` substrate was built for cross-rule coupling but the current constraint set never crosses rule boundaries. AA.3 lands the first cross-rule constraint.

### Bench gates vs post-Z

| Gate | Threshold | Dominant phase |
|---|---|---|
| `json_twitter` parse | ≥ −8% | AA.1 unconditional pair-value unboxing |
| `json_citm` parse | ≥ −5% | AA.1 pair-value unboxing |
| `json_canada` parse | ≥ −3% | AA.1 + possibly AA.2 bitmap pre-scan |
| `css_tailwind` parse | ≥ −3% | AA.2 structural bitmap on selector FIRST sets |
| `compile_css_l4` | ≥ −2% | AA.5 PunctWsRegion gating + AA.6 clone elim |
| `compile_bbnf` | ≥ −2% | AA.6 clone elim |

---

## Phases

### Phase AA.0 — E-graph fire-count instrumentation (prerequisite)

**Motivation**: We cannot prune or strengthen e-graph rules without measurement. The 5 grammar-tier rules + HIR-tier rules run at every compile, but neither `BackoffScheduler` nor `CspScheduler` reports per-rule fire counts on a production compile. The `BBNF_EGRAPH_REPORT=1` env var prints saturation summary but not per-rule breakdown.

**Approach**: Extend the existing `BBNF_EGRAPH_REPORT` path at `crates/ir/src/egraph/mod.rs:99-113` to include per-rule `applied` counts. The `RewriteFn::run` blanket impl already returns the work delta (`total_nodes` + `union_count`); we add a parallel per-rule counter consumed by the report. Mirror the same instrumentation in `parse-that/rust/regex/src/egraph/` so both tiers report together.

**Files**:
- `crates/egraph/src/scheduler.rs` — per-rule fire count on `RunReport`
- `crates/egraph/src/csp_scheduler.rs` — same counter plumbing
- `crates/ir/src/egraph/mod.rs:99-113` — extend the report output
- `parse-that/rust/regex/src/egraph/mod.rs` — mirror the extension

**Impact**: Zero runtime. Pure observability. Enables AA.4 rule pruning decisions.

**Gate**: `BBNF_EGRAPH_REPORT=1 cargo bench -p bbnf --bench compile_pipeline compile_css_l4 2>&1 | grep 'rule='` prints non-zero fire counts for at least one rule.

### Phase AA.1 — BoxedEnum → Enum producer refactor (surgical)

**Motivation**: `.map(|__v| &*__JsonParserEnum_alloc(state).slab().alloc(__v))` at `__pair`'s expansion is the residual JSON parse-time bottleneck. The cause: `child_alloc(BoxedEnum, _) = ValuePlacement::Alloc` at `crates/core/src/backend/types/decisions.rs:40` is **unconditional**, while `child_alloc(Enum, parent_alloc)` inherits from the parent. A Tuple rule like `pair` compiles with `body_alloc = Inline` (Tuple is not BoxedEnum at the rule level, per `driver/mod.rs:269`), so if the value's type were `Enum` instead of `BoxedEnum`, the value child would propagate Inline through `child_alloc` and skip the slab call.

The invariant `BoxedEnum` encodes at the producer (`join_types` at `crates/ir/src/passes/types/constraint/helpers.rs:94`) is **"this alternation is heterogeneous"**, NOT **"the caller needs indirection"**. The indirection is a secondary consequence enforced at the consumer (`decisions::child_alloc`). Moving the "needs ref" decision from the producer to the consumer is the correct architectural transposition.

**Approach (phased across two commits)**:

**Commit 1**: Change the producer.
1. `crates/ir/src/passes/types/constraint/helpers.rs:86-96` — `join_types` returns `TypeDesc::Enum` instead of `TypeDesc::BoxedEnum` for heterogeneous branches.
2. Update the two test assertions in `crates/ir/tests/types.rs:165-170` and `crates/ir/tests/csp_types.rs` to assert `Enum` where they previously asserted `BoxedEnum` for Alt-projection results.
3. Run the full workspace test suite + cargo expand json_monolithic and diff `__pair` — expected: `.map(|__v| &*slab().alloc(__v))` becomes just `Self::__value(state)`.
4. Run the parse bench sweep and confirm `json_twitter` / `json_citm` improvements (≥5% expected).

**Commit 2**: Prune dead conversion branches downstream.
1. `crates/core/src/backend/driver/alt.rs:39-41` — the `BoxedEnum → Enum` conversion in Inline contexts becomes dead (producer never emits BoxedEnum). Delete the branch.
2. `crates/core/src/backend/driver/mod.rs:208, 269` — the `BoxedEnum`-specific rule-body alloc decision is dead for Alt-resolved rules. Adjust: rule bodies whose type is `Enum` (heterogeneous Alt) still need `Alloc` if the rule is non-transparent and called from a context that requires `&Enum`, so the `BoxedEnum | None` arm simplifies to `None` for the alloc fallback.
3. `crates/core/src/backend/driver/repeat.rs:31, 54, 71, 76, 86`, `crates/core/src/backend/driver/wrap.rs:53`, `crates/core/src/backend/driver/seq.rs:90` — similar dead-branch cleanup.
4. `crates/core/src/backend/types/decisions.rs:40` — reduce to just `TypeDesc::Vec(inner) if **inner != TypeDesc::Span => Alloc` + `_ => Inline` (or keep the Enum+parent_alloc branch).

**Commit 3**: (optional, deferred to AA-late) Rename or delete `TypeDesc::BoxedEnum`. Only after the full bench sweep + tests + profile verify the producer change landed cleanly. This final step is NOT blocked by AA.1 — it's a cosmetic cleanup that can be deferred to AB.

**Files**:
- `crates/ir/src/passes/types/constraint/helpers.rs:86-96` (producer)
- `crates/ir/tests/types.rs:165-170`, `crates/ir/tests/csp_types.rs` (test assertions)
- `crates/core/src/backend/driver/{alt,mod,seq,repeat,wrap}.rs` (dead-branch cleanup, commit 2)
- `crates/core/src/backend/types/decisions.rs:38-45` (decision-layer simplification, commit 2)

**Profile-measured impact target**:
- `json_twitter` parse: ≥ −8% (the pair-heavy bench) — confirmed by samply showing `slab::alloc` self-time dropped to <1% on the hot stack
- `json_citm` parse: ≥ −5%
- `json_canada` parse: ≥ −2% (marginal — canada is array-heavy, fewer pairs)
- cargo expand `__pair` delta: `.map(|__v| &*slab().alloc(__v))` → `Self::__value(state)` verified

**Risk**: MEDIUM (3/5). The ~25 pattern-match sites all currently treat `BoxedEnum | Enum` together, so changing the producer doesn't break the consumers — they see `Enum` and proceed. The RISK is in the Vec-context branches that explicitly convert `BoxedEnum → Enum` and the rule-body-alloc branches that treat `BoxedEnum` as "needs Alloc". Both are dead-code candidates after the producer change but need explicit verification via cargo expand + full test run. The **serializer** at `crates/core/src/generate/serialize/serialize.rs:17-19` treats `BoxedEnum | Vec(_)` as reference types — this is the one call site that may need positive logic (not just dead-branch deletion).

### Phase AA.2 — Structural bitmap pre-scan (grammar-agnostic)

**Motivation**: sonic-rs's primary architectural advantage is the SIMD structural-byte bitmap: pre-scan the input once to record every structural position (`{ } [ ] , :` for JSON, `{ } , ;` for CSS declarations, etc.), producing a u64-packed bitmap. Subsequent structural dispatch consults the bitmap in O(1) rather than per-byte lookahead. The bbnf backend has NO analogue today — grep for `structural_bitmap` / `pre_scan` returns zero hits.

The bitmap is **grammar-agnostic**: any grammar whose entry-rule Alt's branches FIRST sets are all subsets of a fixed ≤8-byte set qualifies. JSON, CSS selectors, SQL keywords, EBNF tokens all match the template.

**Approach**:

1. Add `RecognizerShape::StructuralBitmap { bytes: SmallVec<[u8; 8]> }` variant at `crates/ir/src/passes/patterns/mod.rs` (or wherever `RecognizerShape` lives). The miner — new file `crates/ir/src/passes/recognizers/structural_bitmap.rs` — runs inside Z.0's unified walk via `impl RecognizerMiner`. It detects rules whose entry is an Alt whose branches' FIRST sets are subsets of ≤8 distinct single-byte set AND the byte density in a representative input sample exceeds `structural_bitmap_density_min` (new `egraph::CostWeights` field, default 0.08 = 1 in 12 bytes).

2. New kernel at `crates/core/src/backend/kernels/structural_bitmap.rs` emits a pre-scan call at the parser entry point — runs once per parse, producing a `u64` per 64-byte chunk + a fallback for tail. The dispatch layer consults the bitmap via `ctz` / `clz` to jump to the next structural position in O(1).

3. New runtime scanner at `parse-that/rust/parse_that/src/parsers/scan/structural_bitmap.rs` — NEON `vceqq_u8` for aarch64 (baseline), SSE2 `_mm_cmpeq_epi8` for x86_64 (baseline). Each 16-byte chunk produces a u16 mask; four chunks combine into a u64 per 64-byte window.

4. The backend driver at `crates/core/src/backend/driver/alt.rs` conditionally consults the bitmap when the resolved AltMode is `ByteDispatch` and the Alt's FIRST set matches the grammar's detected bitmap set. When the bitmap covers the dispatch, the generated `match` reduces to a bitmap probe + jump instead of a byte compare + branch.

5. **Consumer invariant**: `RecognizerShape::StructuralBitmap` added to the Y.13 consumer-invariant test in the same commit that introduces the variant.

**Files (net-new)**:
- `crates/ir/src/passes/recognizers/structural_bitmap.rs` — miner (`impl RecognizerMiner`)
- `crates/core/src/backend/kernels/structural_bitmap.rs` — kernel emission
- `parse-that/rust/parse_that/src/parsers/scan/structural_bitmap.rs` — runtime SIMD scanner

**Files (modified)**:
- `crates/ir/src/passes/patterns/mod.rs` — add `RecognizerShape::StructuralBitmap` variant
- `crates/ir/src/passes/recognizers/mod.rs` — add `StructuralBitmapMiner` to the unified-walk miner list
- `crates/core/src/backend/driver/alt.rs` — bitmap consultation path
- `crates/core/src/backend/recognizer_plan.rs` — `StructuralBitmap` match arm
- `crates/egraph/src/cost_weights.rs` — `structural_bitmap_density_min` knob
- `crates/core/tests/recognizer_decision_consumption.rs` — Y.13 extension

**Profile-measured impact target**:
- `json_canada` parse: ≥ −3% additional (on top of AA.1) — SIMD pre-scan amortized across the ~1M-element coordinate arrays
- `json_twitter` parse: ≥ −2% additional
- `css_tailwind` parse: ≥ −2% — CSS selector FIRST sets are narrow single-byte sets (`. # [ >`)

**Risk**: MEDIUM (3/5). Net-new infrastructure, but every touch point is a new file or a well-scoped variant addition to an existing file. The riskiest part is the bitmap consultation in `driver/alt.rs` — if it isn't correctly bypassed when the grammar doesn't match the template, it could produce wrong code. The Y.13 invariant test + a new unit test in `crates/core/tests/structural_bitmap_roundtrip.rs` guard the invariant.

### Phase AA.3 — Cross-rule CSP constraint (first real use of Y.5 substrate)

**Motivation**: Today's strategy CSP fires an `ImplicationConstraint` per Alt-child-Engine pair within a rule, but constraints never cross rule boundaries. The Y.5 `components::UnionFind` substrate was built for cross-rule coupling but has no production consumer — it's a ghost substrate.

The natural first cross-rule constraint: **dispatch-table sharing**. If rule A's Alt is `ByteDispatch` and rule B's Alt has a structurally-identical FIRST set + branch set, the two dispatch tables can be hoisted to a single `const` and shared. This is a compile-time win (less generated code, better i-cache) but only if the CSP knows both Alts should choose the same mode.

**Approach**:
1. Add a new constraint type in `crates/ir/src/passes/csp_strategy/mod.rs`: `DispatchShareConstraint` that couples two `AltMode` variables across rules when their signatures (byte-set hash + branch count) match.
2. Extend `solve_strategy_decisions` to compute a pre-pass signature index: `FxHashMap<AltSignature, Vec<NodeId>>` of every candidate Alt. Alts with the same signature are added as a `DispatchShareConstraint` group.
3. The constraint's `revise` method propagates: if any Alt in the group chose `ByteDispatch`, all Alts in the group must also choose `ByteDispatch` (or the CSP falls back to per-rule).
4. The backend at `crates/core/src/backend/rust/emitter/dispatch.rs` emits the dispatch table as a `static` referenced by both rules' match arms.

**Files**:
- `crates/ir/src/passes/csp_strategy/mod.rs` — add `DispatchShareConstraint` + signature pre-pass
- `crates/ir/src/passes/csp_strategy/components.rs` — wire the Y.5 UnionFind substrate to group signature-equivalent Alts
- `crates/core/src/backend/rust/emitter/dispatch.rs` — emit shared dispatch-table `static`
- `crates/core/tests/cross_rule_dispatch.rs` (new) — unit test: two rules with structurally-identical Alts share one dispatch table in the generated code

**Profile-measured impact target**:
- `compile_css_l4` code size: −N% lines of generated code on grammars with signature-identical Alts (CSS L4 has ~3-4 such patterns per `values.bbnf`)
- Parse time: neutral (dispatch table is already O(1); sharing is a code-size + i-cache win)
- The architectural win is the point: cross-rule constraints are no longer ghost infrastructure.

**Risk**: LOW-MEDIUM (2/5). The constraint is purely additive; if no two rules match, the pre-pass is a no-op. The main risk is signature-equivalence false positives (two Alts with the same byte-set but different branch semantics) — guarded by signature = `(ByteSet, Vec<(StringId, NodeId)>)` hash rather than just the byte set.

### Phase AA.4 — PunctWsRegion miner gating (mined-but-not-emitted cleanup)

**Motivation**: PunctWsRegion mines 56 matches on CSS L4 but the backend only emits kernels for hits that satisfy BOTH `!ir.has_family_recognizers` being false AND the current node's `parent_alloc == Inline`. Most of the 56 mined hits fall outside this gate, representing compile-time cost with no runtime payoff — a "ghost recognition" in the sense that the miner fires but the emission path skips.

**Approach**:
1. Move the `parent_alloc == Inline` check into the `PunctWsRegionMiner::inspect` method as an early return. The miner already receives `&RecognizerMineCtx` — extend the ctx with a per-NodeId "expected alloc" lookup (or compute it from `ir.types` during the unified walk).
2. Add `every_recognizer_shape_has_a_consumer_ratio` test in `crates/core/tests/recognizer_decision_consumption.rs` that compiles each of the 5 production grammars (JSON, CSS L4, BBNF, Sheets, EBNF), runs `mine_recognizers`, and asserts that for every `RecognizerShape` variant, the ratio of mined records to emitted kernels is ≥ 0.8 (80% of mined records reach a real emission path).
3. If any shape fails the ratio, the test fails and the miner is either strengthened (to match only emittable shapes) or the backend is strengthened (to emit the missing path).

**Files**:
- `crates/ir/src/passes/recognizers/punct_ws_region.rs` — gate the inspect method
- `crates/ir/src/passes/recognizers/mod.rs` — thread expected-alloc info via `RecognizerMineCtx`
- `crates/core/tests/recognizer_decision_consumption.rs` — new ratio-invariant test

**Profile-measured impact target**: `compile_css_l4` −1% (less miner work); parse-time neutral. The real win is preventing the drift where miners over-fire.

**Risk**: LOW (1/5). Gating the miner only REMOVES records; the backend was already ignoring them.

### Phase AA.5 — Residual type-clone elimination in backend/driver

**Motivation**: Agent 1's audit identified 5 `TypeDesc::clone()` calls in `crates/core/src/backend/driver/`:
- `mod.rs:68-69` — `get_rule_type` lookup clones per rule during emit (hot loop)
- `seq.rs:159` — per-group clone
- `repeat.rs:70, 72` — cardinality type clones

Tranche Y.10 cleaned most type cloning in the IR passes but missed the backend driver. Each of these is a per-compile hot-path clone.

**Approach**: Thread borrows through the driver by lifetime annotation. The `DriverState` already holds a reference to `&GrammarIR`; the type lookups should return `Option<&TypeDesc>` instead of `Option<TypeDesc>`. Callers that need ownership clone once at the call site explicitly; callers that only need to pattern-match borrow for free.

**Files**:
- `crates/core/src/backend/driver/mod.rs:60-90` — add `fn get_rule_type_ref(&self, rule_id) -> Option<&TypeDesc>` and migrate callers to borrow
- `crates/core/src/backend/driver/seq.rs:145-165` — borrow instead of clone
- `crates/core/src/backend/driver/repeat.rs:65-80` — same

**Profile-measured impact target**: `compile_bbnf` −1%, `compile_css_l4` −1%. Small but free.

**Risk**: LOW (1/5). Pure borrow-threading; if it compiles, it's correct.

### Phase AA.6 — post-AA baseline + verification

Same shape as Z.7.

**Deliverables**:
- `docs/benchmarks/post-AA.json` with per-phase attribution, every "+X%" claim citing a samply profile symbol + delta
- `docs/benchmarks/profiles/post-AA/*.samply` — the five post-phase profiles
- Pre-Z → post-Z → post-AA profile diff per bench
- Y.13 consumer-invariant test passes with `RecognizerShape::StructuralBitmap` added + the new `every_recognizer_shape_has_a_consumer_ratio` ratio invariant
- E-graph fire-count report (AA.0 instrumentation) captured for each grammar

---

## Hard gates

| Gate | Threshold |
|---|---|
| All workspace tests pass | yes |
| bbnf-ir tests | all passing |
| Bootstrap script idempotent | yes |
| Y.13 consumer-invariant test | passes with `RecognizerShape::StructuralBitmap` added |
| `every_recognizer_shape_has_a_consumer_ratio` | passes (≥80% mined-to-emitted ratio per shape) |
| `grep -rn "pub(super) fn collect" crates/ir/src/passes/recognizers/` | zero hits (Z.0 invariant preserved) |
| cargo expand `__pair` in `json_monolithic` bench | no `.map(\|__v\| &*slab().alloc(__v))` pattern |
| `json_twitter` parse | ≥ −8% vs post-Z |
| `json_citm` parse | ≥ −5% vs post-Z |
| `json_canada` parse | ≥ −3% vs post-Z |
| `css_tailwind` parse | ≥ −3% vs post-Z |
| `compile_css_l4` | ≥ −2% vs post-Z |
| `compile_bbnf` | ≥ −2% vs post-Z |
| `BBNF_EGRAPH_REPORT=1` | prints non-zero per-rule fire counts (AA.0) |
| Every "+X%" claim in post-AA.json | cites a samply symbol + self-time delta |

---

## Files added

- `crates/ir/src/passes/recognizers/structural_bitmap.rs` (AA.2)
- `crates/core/src/backend/kernels/structural_bitmap.rs` (AA.2)
- `parse-that/rust/parse_that/src/parsers/scan/structural_bitmap.rs` (AA.2)
- `crates/core/tests/cross_rule_dispatch.rs` (AA.3)
- `docs/benchmarks/post-AA.json` (AA.6)
- `docs/benchmarks/profiles/post-AA/*.samply` (AA.6)

## Files modified

- `crates/egraph/src/scheduler.rs`, `crates/egraph/src/csp_scheduler.rs` — per-rule fire-count instrumentation (AA.0)
- `crates/ir/src/egraph/mod.rs` — extended `BBNF_EGRAPH_REPORT` output (AA.0)
- `parse-that/rust/regex/src/egraph/mod.rs` — mirrored fire-count extension (AA.0)
- `crates/ir/src/passes/types/constraint/helpers.rs` — `join_types` returns `Enum` (AA.1)
- `crates/ir/tests/types.rs`, `crates/ir/tests/csp_types.rs` — test assertion updates (AA.1)
- `crates/core/src/backend/driver/{alt,mod,seq,repeat,wrap}.rs` — dead-branch cleanup (AA.1)
- `crates/core/src/backend/types/decisions.rs` — `child_alloc` simplification (AA.1)
- `crates/core/src/generate/serialize/serialize.rs` — ref-type check update if needed (AA.1)
- `crates/ir/src/passes/patterns/mod.rs` — `RecognizerShape::StructuralBitmap` variant (AA.2)
- `crates/ir/src/passes/recognizers/mod.rs` — wire `StructuralBitmapMiner` into unified walk (AA.2)
- `crates/core/src/backend/driver/alt.rs` — bitmap consultation path (AA.2)
- `crates/core/src/backend/recognizer_plan.rs` — `StructuralBitmap` arm (AA.2)
- `crates/egraph/src/cost_weights.rs` — `structural_bitmap_density_min` knob (AA.2)
- `crates/ir/src/passes/csp_strategy/mod.rs` — `DispatchShareConstraint` + cross-rule pre-pass (AA.3)
- `crates/ir/src/passes/csp_strategy/components.rs` — consume Y.5 UnionFind for grouping (AA.3)
- `crates/core/src/backend/rust/emitter/dispatch.rs` — shared dispatch-table `static` emission (AA.3)
- `crates/ir/src/passes/recognizers/punct_ws_region.rs` — gate the inspect method (AA.4)
- `crates/core/tests/recognizer_decision_consumption.rs` — Y.13 extension + new ratio-invariant test (AA.2, AA.4)
- `crates/core/src/backend/driver/mod.rs` — `get_rule_type_ref` borrow threading (AA.5)
- `crates/core/src/backend/driver/seq.rs`, `repeat.rs` — borrow instead of clone (AA.5)

## Verification

End-to-end verification per phase:

1. **AA.0** — `BBNF_EGRAPH_REPORT=1 cargo bench -p bbnf --bench compile_pipeline 2>&1 | grep rule=` prints non-zero counts. `cargo test -p bbnf-ir` passes. Commit.

2. **AA.1 (commit 1)** — `cargo test -p bbnf && cargo test -p bbnf-ir`, then `cargo expand -p bbnf --bench json_monolithic 2>&1 | grep -A 2 "fn __pair"` should show `Self::__value(state)` WITHOUT the `.map(|__v| &*slab().alloc(__v))` wrapper. Run the full parse bench sweep (single invocation) and confirm `json_twitter`/`json_citm` improvements. Commit.

3. **AA.1 (commit 2)** — Delete dead conversion branches. Workspace tests + cargo expand unchanged. Commit.

4. **AA.2** — `cargo test -p bbnf && cargo test -p bbnf-ir`, then `cargo expand -p bbnf --bench json_monolithic 2>&1 | grep 'structural_bitmap'` should find the pre-scan call at the entry point. Samply profile `json_canada` should show `structural_bitmap_scan` in the hot stack. Full parse bench sweep. Commit.

5. **AA.3** — `cargo test -p bbnf cross_rule_dispatch`, then `cargo expand -p bbnf --bench css_l4 2>&1 | grep 'static.*DISPATCH_TABLE'` should find shared dispatch-table constants. Commit.

6. **AA.4** — `cargo test -p bbnf --test recognizer_decision_consumption` — the new ratio test must pass. Commit.

7. **AA.5** — `cargo build --workspace` + `cargo test --workspace`. Profile `compile_bbnf` before/after; expect clone self-time to drop. Commit.

8. **AA.6** — Full bench sweep (single invocation per bench binary); capture post-AA profiles; write `post-AA.json` with every "+X%" citing a samply symbol. Commit.

## Non-goals (Tranche AB)

- **Full `TypeDesc::BoxedEnum` variant deletion** — AA.1 changes the producer; the variant remains as a never-produced deprecated path. AB renames or removes it after stability is confirmed.
- **SIMD ws_block_comments / number mantissa 16-byte detection** — Z profiled these as net-negative on the M-series ARM machine; the Y.7/Y.8 LUT+SWAR paths are already at LLVM autovectorization optima. Only revisit if a future profile shows them in the top-3 hot path.
- **Dispatch-fallback consolidation** (Z.4 reframed) — still requires multi-file `generate_dispatch_tables` refactor; AB.
- **Full RefMode CSP cross-ref coupling** — Z discovered the Rust inline.rs CSP has no TS/WASM consumer; AA.3 delivers the cross-rule edge via a different route. AB revisits if profile evidence shows Ref decisions are hot.
- **Tape-style output format** — sonic-rs technique requiring breaking AST redesign; explicitly out of scope.
- **Lazy vs eager parse modes** — JSON-specific; too narrow for grammar-agnostic infrastructure.
