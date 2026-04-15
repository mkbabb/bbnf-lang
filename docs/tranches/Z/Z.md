# Tranche Z — Sonic-rs Parity, Universal Cost Model, Gestalt Compile Wins

## Context

Tranche Y closed with the ghost substrate eliminated (~830 lines of dead infrastructure deleted, AltMode 5→3, WrapMode 5→4, every surviving variant verified by the Y.13 consumer-invariant test) and the freezing guards in place (csp-solver node budget, bench wall-clock guards, nextest slow-timeout). Compile-time gates missed by 2.3–9.6%; parse-time gates by 0.6–10.6%. Y recovered every post-X regression but didn't fully reach the post-W targets.

A second-pass audit over the initial Z draft revealed that several of the draft's numbers were inherited from earlier conversations and no longer matched source. This version is rebased on ground truth via direct source inspection at:
- `crates/ir/src/passes/recognizers/mod.rs:55` (the recognizer miner orchestrator)
- `crates/core/src/backend/rust/analysis/inline.rs:29–96` (the existing CallMode CSP)
- `crates/core/src/backend/strategy/ref_strategy.rs` (a resolver shim, not a solver)
- `crates/core/src/backend/driver/repeat.rs:15–92` (pure heuristic, no cost model)
- `crates/core/src/backend/rust/emitter/alt.rs:30–76` (the slab-alloc call sites)

### Corrected findings

1. **Recognizer mining is nine independent DAG walks — and two of them weren't in the draft.** `mine_recognizers` runs one `recognize_tree` base walk (Phase 2), then nine miner calls in Phase 3: seven recognizer miners (`quoted_string`, `balanced_wrap`, `comment_ws`, `identifier`, `separator_list`, `token_led_branches`, `punct_ws_region`) plus two config miners the draft missed (`delim_scan::collect`, `key_dispatch::collect`). Every walk reads the same `GrammarDag`, calls the same `visit_children_alt` recursor, writes into shared output collections. They differ only in per-node match logic. `token_led_branches` additionally requires `context_facts` (computed once via `compute_context_facts`, a worklist propagation — not a tree walk).

2. **JSON per-element boxing is ~1 alloc per element, not ~4.** `collapse_simple_spans` already handles the all-Span-child case; the remaining allocation is **heterogeneous-alt sub-variant wrapping** at `emitter/alt.rs:50,63,70`. Sonic-rs parity therefore requires extending `collapse_simple_spans` to single-variant enum inlining, not rebuilding the scalar emission path.

3. **Ref strategy is already CSP-driven — but Rust-backend-local and over a narrower domain than the backend emits.** `inline.rs:29–96` defines `InlineDomain` as an AC-3 `LatticeDomain` with `CallMode ∈ {DirectCall, InlineBody}`. The backend's `CallStrategy` enum has three variants including `InlineFusion` — decided separately via `@token` + `fuse_token_dispatch` and coalesced post-hoc. That's a ghost decision point exactly of the kind Y.13 was built to prevent. Constraints are all unary (`CostBudgetConstraint::propagate` treats each rule in isolation). The draft's "lift from heuristic to CSP" framing is wrong. The real work is: add `InlineFusion` to the CSP domain; add cross-ref coupling constraints; move the solver out of `backend/rust/analysis/` into `bbnf_ir::passes::csp_strategy` so TS/WASM backends see the same decision.

4. **Repeat and Seq are pure heuristics today — no CSP at all.** `driver/repeat.rs:27` calls `decisions::detect_sep_by(inner)` — a structural match with zero cost model — and hard-codes the tri-branch `{sep_by, optional, many}` choice. `driver/seq.rs` uses `NodeFacts.operator_chain` as a binary gate. Repeat has a non-trivial cost spread (unroll vs. loop vs. scan) that's invisible to the current CSP. Signal rank from the audit: **3.5/5**, close to Ref's 4.5.

5. **Scan helpers have byte-class LUTs from Y.7/Y.8/Y.9, but no explicit SIMD.** The Z.1/Z.2 intrinsics story is correct: `parse-that/.../scan/{ws_comment,number}.rs` compile to competitive scalar code via the autovectorizer, but explicit `_mm_cmpeq_epi8` / `vceqq_u8` would close the parse-time gap on `tailwind` / `canada`. **Confirmation requires an actual profile** — see the Profiling Methodology section.

6. **The cross-tier cost model is asymmetric.** `bbnf_ir::CostConfig` owns the `strategy_*` knobs grammar-tier only. `egraph::CostWeights` is shared across grammar + HIR tiers but the strategy knobs haven't been lifted there. Universal cost model means `StrategyConfig` moves to `egraph` as a neutral sub-struct, both tiers embed it.

---

## Profiling methodology (macOS)

All performance claims in this tranche must be verified against an actual samply profile. The profile workflow is load-bearing — the draft inherited several wrong numbers ("4 allocs" and "9x walks") from earlier audits, and only direct source inspection caught them. Every phase that quotes a "+X%" number must be profile-confirmed before its gate is declared met.

### 1. Build the exact bench binaries with debuginfo

```bash
cargo bench -p bbnf --bench compile_pipeline --no-run
cargo bench -p bbnf --bench json_monolithic --no-run
cargo bench -p bbnf --bench css_l4 --no-run
```

Ensure `[profile.release] debug = true` is set in the bench crate's `Cargo.toml` so symbols survive into the `.dSYM`.

### 2. Resolve the newest executable per bench

```bash
BIN=$(find target/release/deps -maxdepth 1 -type f -perm -111 -name 'compile_pipeline-*' | xargs ls -t | head -1)
JSON_BIN=$(find target/release/deps -maxdepth 1 -type f -perm -111 -name 'json_monolithic-*' | xargs ls -t | head -1)
CSS_BIN=$(find target/release/deps -maxdepth 1 -type f -perm -111 -name 'css_l4-*' | xargs ls -t | head -1)
```

Never glob `target/release/deps/foo-*` directly — it matches stale binaries, `.d` files, `.dSYM` bundles, and intermediate objects. `ls -t | head -1` guarantees the newest.

### 3. Verify symbol UUIDs match before recording

```bash
xcrun dwarfdump --uuid "$BIN" "$BIN.dSYM/Contents/Resources/DWARF/$(basename "$BIN")"
```

The two UUIDs must match. If they don't, samply symbolication silently falls back to stripped stacks and the profile is useless.

### 4. Record compile-phase profiles

```bash
samply record --save-only --unstable-presymbolicate -o /tmp/compile_bbnf.samply    -- "$BIN" compile_bbnf
samply record --save-only --unstable-presymbolicate -o /tmp/compile_json.samply    -- "$BIN" compile_json
samply record --save-only --unstable-presymbolicate -o /tmp/compile_css_l4.samply  -- "$BIN" compile_css_l4
```

### 5. Record parse-phase profiles from `crates/core`

Parse benches are cwd-sensitive (relative data paths). Run them from `crates/core`:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/crates/core

samply record --save-only --unstable-presymbolicate \
    -o /tmp/json_canada.samply \
    -- ../../target/release/deps/$(basename "$JSON_BIN") canada

samply record --save-only --unstable-presymbolicate \
    -o /tmp/json_twitter.samply \
    -- ../../target/release/deps/$(basename "$JSON_BIN") twitter

samply record --save-only --unstable-presymbolicate \
    -o /tmp/css_tailwind.samply \
    -- ../../target/release/deps/$(basename "$CSS_BIN") tailwind
```

### Pitfalls to avoid

- Don't glob stale binaries via `target/release/deps/foo-*`.
- Don't run parse benches from repo root (cwd breaks relative data paths).
- Don't assume symbols are usable just because the binary exists — verify UUID first.
- `samply` without `--unstable-presymbolicate` re-resolves symbols at load time; if the binary moves, symbols are lost. `--save-only` + presymbolicate bakes them in.

### Pre-Z baselines to archive

Before starting Z.0, capture these five profiles as `docs/benchmarks/profiles/pre-Z/*.samply` so Z.7 has a delta reference:

1. `compile_css_l4.samply` — for Z.0 + Z.5 attribution
2. `compile_bbnf.samply` — for Z.0 attribution
3. `json_canada.samply` — for Z.2 + Z.3 attribution
4. `json_twitter.samply` — for Z.3 + Z.3b attribution
5. `css_tailwind.samply` — for Z.1 + Z.4 attribution

---

## Architectural commitments

1. **No legacy code, no workarounds.** Every phase lands clean. Scaffolding is idiomatic and lives in its right place.

2. **Compile-time may degrade slightly for parse-time wins.** Maximal -O3 pass. Up to ~5% compile-time degradation is acceptable if it unblocks ≥10% parse-time improvement — measured against an actual profile, not assumed.

3. **Universal cost model.** `StrategyConfig` lifts to the shared `egraph::cost_config` substrate. Any strategy CSP (grammar-tier, HIR-tier, future tiers) reads from the same struct. Ref/Repeat strategy decisions move into `bbnf_ir::passes::csp_strategy` so all backends consume one decision map.

4. **Gestalt over local.** The recognizer walk consolidation, the cross-ref Ref coupling, the Repeat CSP, the single-variant inline scalars, the explicit SIMD intrinsics, and the structural bitmap pre-pass — each is a whole-system change, not a per-site peephole.

5. **Zero ghost substrate — still.** Every new variant / enum / CSP variable has a load-bearing consumer before the commit that introduces it. The Y.13 consumer-invariant test extends to every new decision variant (`RefMode::InlineFusion`, `RepeatMode::*`, `RecognizerShape::StructuralBitmap`).

6. **Truth-based phase attribution.** Every compile-time or parse-time claim cites a specific samply profile line (symbol name + self-time delta). No more "expected +X%" without a pre/post delta on the actual profile.

### Bench gates vs post-Y (revised to reflect the corrected Z.3 boxing audit and expanded Z.5 + Z.5b scope)

| Gate | Threshold | Dominant phase |
|---|---|---|
| `compile_css_l4` | ≥ −5% | Z.0 single-walk + Z.5 / Z.5b cleanup |
| `compile_bbnf` | ≥ −3% | Z.0 single-walk + Z.5 / Z.5b cleanup |
| `json_canada` parse | ≥ +12% | Z.2 SIMD mantissa + Z.3 single-variant inline |
| `json_citm` parse | ≥ +8% | Z.3 single-variant inline |
| `json_twitter` parse | ≥ +10% | Z.3 single-variant inline (element-heavy) + Z.3b bitmap |
| `css_tailwind` parse | ≥ +8% | Z.1 SIMD ws + Z.4 checkpoint reduction |

---

## Phases

### Phase Z.0 — Recognizer miner consolidation (single-walk dispatch)

`mine_recognizers` at `crates/ir/src/passes/recognizers/mod.rs:55` currently runs nine independent DAG descents after the Phase 2 `recognize_tree` base walk: seven recognizer miners plus two config miners (`delim_scan::collect`, `key_dispatch::collect`) the draft missed. Every walk reads the same `GrammarDag`, calls the same `visit_children_alt` recursor, writes into shared output collections. They differ only in per-node match logic.

**Approach**: Introduce a trait + shared context in `mod.rs`:

```rust
pub struct RecognizerMineCtx<'a> {
    pub ir: &'a GrammarIR,
    pub dag: &'a GrammarDag,
    pub context_facts: &'a ContextFactsMap,
}

pub trait RecognizerMiner {
    /// Called once per DAG node during a single orchestrator walk.
    /// Miners matching the node shape push into `out`. All miners
    /// see every node; the per-miner match predicate distinguishes.
    fn inspect(
        &self,
        node: &IrNode,
        node_id: NodeId,
        ctx: &RecognizerMineCtx,
        out: &mut Vec<(NodeId, Recognizer)>,
    );
}
```

Each current `fn collect` + `fn walk` pair collapses to a single `impl RecognizerMiner for XxxMiner { fn inspect(&self, ...) }`. `mine_recognizers` runs **one** DAG walk, invoking every miner's `inspect` at every node. `delim_scan::collect` and `key_dispatch::collect` emit onto different IR sidecars (not `node_facts`); they get a sibling `ConfigMiner` trait or fold into the orchestrator via parallel inspect hooks in the same walk.

**Scope deliberately excluded**: `csp_strategy` mining (two further walks downstream) is a **different computation model** (CSP constraint generation, not fact accumulation). Folding it into this walk would violate the four-layer architecture's facts→CSP boundary.

**Files**:
- `crates/ir/src/passes/recognizers/mod.rs` — orchestrator refactor (single walk + trait dispatch)
- `crates/ir/src/passes/recognizers/{quoted_string,balanced_wrap,comment_ws,identifier,separator_list,token_led_branches,punct_ws_region}.rs` — `impl RecognizerMiner` conversion (delete `collect` + `walk`, keep match logic as `inspect`)
- `crates/ir/src/passes/recognizers/{delim_scan,key_dispatch}.rs` — same trait or sibling `ConfigMiner`

**Profile-measured impact target**: `compile_css_l4` −3% / `compile_bbnf` −2% vs. pre-Z baselines. If post-phase profile shows the recognizer phase is not in the top-3 self-time costs, re-evaluate the gate against the actual hot path.

### Phase Z.1 — Explicit SIMD intrinsics for `scan_ws_block_comments`

Y.7's LUT compiles to one load + one branch per byte. Explicit SIMD compares 16 bytes per instruction sequence. The +15% post-W gate on `css_tailwind` assumed this path.

**File**: `parse-that/rust/parse_that/src/parsers/scan/ws_comment.rs`

**Approach**:
- `#[cfg(target_arch = "x86_64")]`: `_mm_loadu_si128` → five `_mm_cmpeq_epi8` compares (space, tab, `\n`, `\r`, `\x0C`) → `_mm_or_si128` chain → `_mm_movemask_epi8` → `trailing_zeros` on the non-ws mask
- `#[cfg(target_arch = "aarch64")]`: `vld1q_u8` → five `vceqq_u8` → `vorrq_u8` chain → mask extraction via `vmaxvq_u8`
- Scalar fallback (Y.7 LUT) for other architectures
- Comment-aware slow-path fallback preserved unchanged

**Profile-measured impact target**: `css_tailwind` parse +6–8% additional on top of Y.7's ~4%. Pre-commit sanity: post-phase `samply` profile of `css_tailwind` must show `_mm_cmpeq_epi8` / `vceqq_u8` symbols on the hot stack.

### Phase Z.2 — Explicit SIMD intrinsics for `scan_number_mantissa` detection

Y.8's SWAR digit validation processes 8 bytes per arithmetic chunk. Explicit SIMD processes 16 via vector compares.

**File**: `parse-that/rust/parse_that/src/parsers/scan/number.rs`

**Approach**:
- x86_64: load 16 bytes, `_mm_cmpgt_epi8`(`<'0'`) + `_mm_cmplt_epi8`(`>'9'`) + `_mm_or_si128`, `_mm_movemask_epi8` → `trailing_zeros` gives leading-digit count
- aarch64: `vld1q_u8` + `vcgeq_u8`/`vcleq_u8` + `vandq_u8` + `vmaxvq_u8`-style extraction
- Scalar fallback: Y.8's `all_eight_are_ascii_digits`
- `parse_eight_digits` (mantissa accumulation) unchanged — SIMD-ify detection only

**Profile-measured impact target**: `json_canada` parse +8–12% additional. Pre-commit: `json_canada` samply profile must show SSE2/NEON symbols on the mantissa hot path.

### Phase Z.3 — Single-variant enum inlining (heterogeneous Alt scalar path)

**Corrected scope**: The draft claimed JSON boxes ~4 allocations per element. Direct inspection of `alt.rs:30–76` shows `collapse_simple_spans` already handles all-Span-child collapse. The remaining allocation is **heterogeneous-alt sub-variant wrapping** — ~1 alloc per element, not 4. The sonic-rs parity gap is this one residual boxing.

**Approach**:
1. Add `TypeDesc::InlineSpan` variant to `crates/ir/src/types/type_desc.rs` — semantically a `Span` that cannot be enum-wrapped; stays inline.
2. `coerce_branch` at `emitter/alt.rs:30–76` detects when all branches of a heterogeneous Alt resolve to a single scalar-producing variant (Span, bool, `Option<Span>`). In that case, the Alt collapses to the inlined scalar: no slab allocation, no enum discriminant.
3. Extend `SeqConstraint` at `passes/types/constraint/seq.rs:96–108` to propagate `InlineSpan` through Seq collapse the same way it propagates `Span`.
4. The boxed path stays for genuinely heterogeneous branches (`Object` with nested values, `Array` with nested items) — unchanged.

**Files**:
- `crates/ir/src/types/type_desc.rs` — `InlineSpan` variant
- `crates/core/src/backend/rust/emitter/alt.rs:30–76` — single-variant detection + inline path
- `crates/core/src/backend/rust/emitter/value.rs` — inlined scalar emission
- `crates/ir/src/passes/types/constraint/seq.rs:96–108` — `InlineSpan` propagation
- `crates/core/src/backend/rust/ir_types.rs` — `InlineSpan` → Rust type mapping

**Profile-measured impact target**: `json_canada` +4–6%; `json_twitter` +8–12% (element-heavy); `json_citm` +5–8%. The draft's +15–20% claim for twitter was unsupportable — the per-element boxing count was already ~1, not 4. Pre-commit: `json_twitter` samply profile must show slab-alloc self-time dropped to <2% on the hot stack.

### Phase Z.3b — Structural bitmap pre-pass (grammar-agnostic Recognizer)

sonic-rs' second big technique is a structural-char bitmap: pre-scan the input once to mark every structural byte (`{ } [ ] , :` for JSON), then dispatch decisions against the bitmap instead of per-byte lookahead. This generalizes to any grammar whose delimiters are a small fixed set of single-byte literals.

**Approach**: Add `RecognizerShape::StructuralBitmap { bytes: SmallVec<[u8; 8]> }`. The miner (runs inside Z.0's unified walk) detects grammars where:
- A hot Alt's branches' FIRST sets are all subsets of a fixed single-byte set of ≤ 8 distinct bytes
- Structural-byte density in representative inputs exceeds a threshold (`structural_bitmap_density_min`, new `cost_config` knob)

When detected, the backend emits a 1-time SIMD pre-scan: x86_64 uses `memchr` multi-byte search (internally PCMPESTRI / packed compares); aarch64 uses NEON lane-mask extraction. The scan produces a `u64`-packed bitmap (bit per structural position) the dispatcher consults in O(1) during parse.

**Files (net-new)**:
- `crates/ir/src/passes/recognizers/structural_bitmap.rs` — miner (`impl RecognizerMiner`, part of Z.0 walk)
- `crates/core/src/backend/kernels/structural_bitmap.rs` — kernel emission
- `parse-that/rust/parse_that/src/parsers/scan/structural_bitmap.rs` — runtime SIMD scanner

**Consumer-invariant extension**: `RecognizerShape::StructuralBitmap` added to `crates/core/tests/recognizer_decision_consumption.rs` before the commit that introduces it.

**Profile-measured impact target**: `json_canada` +3–5% additional (on top of Z.3); `json_twitter` +3–5% (delimiter-heavy). **Zero overfit**: the miner's match predicate is grammar-agnostic. CSS selectors, SQL keywords, EBNF token grammars would opt in automatically.

### Phase Z.4 — Checkpoint reduction via dispatch-fallback consolidation

CSS L4's `value` alternation has a deep checkpoint chain — each branch saves `state.offset` before trying, restores on failure. `cargo expand` on the tailwind hot path (post-Y) shows ~50 checkpoint sites. When `AltMode::ByteDispatch` is resolved and the dispatch table covers ≥80% of branches, the remaining ≤20% can emit under the dispatch's fallback arm — no per-branch save/restore.

**Approach**: In `crates/core/src/backend/strategy/alt_strategy.rs`, when `AltMode::ByteDispatch` solves and the covered ratio exceeds `strategy_dispatch_fallback_threshold` (new `cost_config` knob, default 0.8), the dispatch emission path consolidates the uncovered branches into a single fallback arm. The emitter at `emitter/dispatch.rs` already supports a fallback arm; the change routes uncovered branches through it.

**Files**:
- `crates/core/src/backend/strategy/alt_strategy.rs` — consolidation rule
- `crates/core/src/backend/rust/emitter/dispatch.rs` — consolidated-fallback emission
- `crates/core/src/backend/driver/alt.rs` — dispatcher wire-up
- `crates/ir/src/cost_config.rs` — `strategy_dispatch_fallback_threshold` knob

**Profile-measured impact target**: `css_tailwind` parse +3–5%; `css_bootstrap` parse +2–4%. Pre-commit: `cargo expand` on the tailwind path must show `value` alternation's checkpoint count reduced by ≥40%.

### Phase Z.5 — Lift CallMode CSP to bbnf-ir + extend with InlineFusion + cross-ref coupling

**Corrected framing**: The draft claimed Ref is a heuristic that needs to become a CSP. Direct inspection of `crates/core/src/backend/rust/analysis/inline.rs:29–96` shows Ref **is** already CSP-solved via `csp_solver::Csp<InlineDomain>` with `CallMode ∈ {DirectCall, InlineBody}` and AC-3 propagation. Three gaps remain:

1. **Domain is too narrow.** Backend `CallStrategy` has three variants — `DirectCall`, `InlineBody`, `InlineFusion` — but CSP `CallMode` has only two. `InlineFusion` is decided via `@token` + `fuse_token_dispatch` and coalesced into `CallStrategy` post-hoc. This is a ghost decision point — precisely what Y.13 was built to prevent. Unify by adding `InlineFusion` to the CSP domain.

2. **Constraints are all unary.** `CostBudgetConstraint::propagate` treats every rule in isolation. Cross-ref coupling — inlining rule A is cheaper when its callers inline it — is invisible. Add a `CrossRefCouplingConstraint` that edges between caller-rule variables and callee-rule variables, with cost computed from the projected code-size delta.

3. **Solver is Rust-backend-local.** `inline.rs` lives under `crates/core/src/backend/rust/analysis/`; TS and WASM backends don't see the decision. Move the solver to `crates/ir/src/passes/csp_strategy/ref_mode.rs` alongside the existing AltMode / WrapMode / RegexEngine solvers. The result lands in `ir.recognizer_decisions` as a `RefModeMap` that every backend consumes via the existing `backend/strategy/ref_strategy.rs` resolver.

**Files**:
- `crates/ir/src/passes/csp_strategy/ref_mode.rs` (new) — lifted solver, extended domain, cross-ref constraints
- `crates/ir/src/passes/csp_strategy/mod.rs` — add `RefMode` variable class; wire into strategy solve entry
- `crates/core/src/backend/rust/analysis/inline.rs` — reduce to feasibility-precomputation shim (body size, recursion check); no longer owns the decision
- `crates/core/src/backend/strategy/ref_strategy.rs` — already a resolver; point at the new `ir.recognizer_decisions.ref_mode` source
- `crates/core/tests/recognizer_decision_consumption.rs` — Y.13 extension: `RefMode` exhaustive match
- `crates/ir/src/cost_config.rs` — `ref_inline_body_cost_per_node`, `ref_inline_fusion_bonus`, `ref_direct_call_overhead`, `ref_cross_coupling_weight`

**Profile-measured impact target**: `compile_css_l4` −1–2% (cleaner codegen); parse-time neutral on single-grammar baseline. The architectural win is the point — the decision unifies across backends and the `InlineFusion` ghost variant is eliminated.

### Phase Z.5b — Repeat strategy CSP (net-new variable class)

`crates/core/src/backend/driver/repeat.rs:15–92` is pure heuristic: `decisions::detect_sep_by(inner)` (structural match, no cost model) → tri-branch hard-code between `{sep_by, optional, many}`. Audit signal rank: **3.5/5**, close to Ref's 4.5. Affects every `Repeat` node in every grammar. Cost spread between variants (unrolled small-count vs. loop vs. sep-by fused vs. balanced scan) is large on CSS grammars where `Repeat` dominates.

**Approach**: New CSP variable class `RepeatMode ∈ {SepByFused, OptionalInline, UnrolledSmall, LoopBounded, BalancedScan}` in `crates/ir/src/passes/csp_strategy/repeat_mode.rs`. Domain constraints:
- `SepByFused` feasible iff `detect_sep_by(inner)` structurally matches
- `OptionalInline` feasible iff `lo == 0 && hi == 1`
- `UnrolledSmall` feasible iff `hi < unroll_threshold` (knob) and `inner` cost below threshold
- `LoopBounded` is the default
- `BalancedScan` feasible iff `inner` matches a balanced-delimiter shape (hands off to existing `WrapMode::BalancedScan`)

Cost function reads new `repeat_*` knobs from `cost_config`. Cross-decision coupling: `RepeatMode` interacts with the `WrapMode` of its parent (`Wrap(open, Repeat(...), close)` has a different optimum than bare `Repeat`) and with the `AltMode` of the inner node when relevant. Both edges are `ImplicationConstraint`s in the existing CSP substrate.

**Files**:
- `crates/ir/src/passes/csp_strategy/repeat_mode.rs` (new)
- `crates/ir/src/passes/csp_strategy/mod.rs` — add `RepeatMode` variable class + cross-decision constraint edges to `WrapMode` and `AltMode`
- `crates/core/src/backend/driver/repeat.rs` — read from `ir.recognizer_decisions.repeat_mode` instead of calling `decisions::detect_sep_by` directly
- `crates/core/src/backend/strategy/repeat_strategy.rs` — resolver shim
- `crates/core/tests/recognizer_decision_consumption.rs` — Y.13 extension: `RepeatMode` exhaustive match
- `crates/ir/src/cost_config.rs` — `repeat_unroll_threshold`, `repeat_sep_by_bonus`, `repeat_scan_savings`, `repeat_unroll_cost_per_node` knobs

**Profile-measured impact target**: `compile_css_l4` −1–2% (cleaner codegen, fewer heuristic branches); `css_tailwind` parse +2–4% (better sep_by + unroll decisions on the `Repeat` hot path).

### Phase Z.6 — Universal strategy cost knobs (cross-tier lift)

The `strategy_*` fields in `bbnf_ir::CostConfig` are grammar-only. Lift to a shared `egraph::StrategyConfig` that both grammar and HIR tiers read. Z.5 and Z.5b consume from the lifted struct from day one.

**Files**:
- `crates/egraph/src/cost_config.rs` — `StrategyConfig` sub-struct field on `CostConfig`
- `crates/ir/src/cost_config.rs` — `bbnf_ir::CostConfig` embeds `egraph::StrategyConfig` via the `egraph` field; grammar-tier strategy CSP reads `ir.cost_config.egraph.strategy` instead of `ir.cost_config.strategy_*`
- `parse-that/rust/regex/src/egraph/cost.rs` — `RegexExtractionCost` gains access to `StrategyConfig` for future HIR-tier strategy CSP work

**Impact**: zero runtime. Architectural symmetry — the universal cost model is no longer asymmetric.

### Phase Z.7 — post-Z baseline + profile-verified attribution

Full bench sweep + samply profile captures + consumer-invariant test + cargo-expand audit for inline-scalar verification + gate analysis vs. post-Y.

**Deliverables**:
- `docs/benchmarks/post-Z.json` with per-phase attribution, each "+X%" claim citing the corresponding samply profile delta (not just the bench delta — the profile should show which symbol moved)
- `docs/benchmarks/profiles/post-Z/*.samply` — the five post-phase profiles (compile_css_l4, compile_bbnf, json_canada, json_twitter, css_tailwind), captured via the Profiling Methodology commands above
- Pre-Z vs. post-Z profile diff per bench; any phase whose claim isn't backed by a measurable delta in the profile must be re-opened or explicitly marked as "architectural win only" in post-Z.json
- Y.13 consumer-invariant test passes with `RefMode`, `RepeatMode`, and `RecognizerShape::StructuralBitmap` exhaustive matches
- `grep -rn "pub(super) fn collect" crates/ir/src/passes/recognizers/` returns zero hits (Z.0 invariant)

---

## Hard gates

| Gate | Threshold |
|---|---|
| All workspace tests pass | yes |
| bbnf-ir tests | all passing |
| Bootstrap script idempotent | yes |
| Y.13 consumer-invariant test | passes with `RefMode`, `RepeatMode`, `RecognizerShape::StructuralBitmap` added |
| `grep -rn backend::patterns crates/` | zero hits (Y.1 invariant preserved) |
| `grep -rn "pub(super) fn collect" crates/ir/src/passes/recognizers/` | zero hits (Z.0 invariant) |
| `compile_css_l4` | ≥ −5% vs post-Y |
| `compile_bbnf` | ≥ −3% vs post-Y |
| `json_canada` parse | ≥ +12% vs post-Y |
| `json_citm` parse | ≥ +8% vs post-Y |
| `json_twitter` parse | ≥ +10% vs post-Y |
| `css_tailwind` parse | ≥ +8% vs post-Y |
| Every `RefMode::InlineFusion`, `RepeatMode::*`, and `RecognizerShape::StructuralBitmap` variant | ≥1 production consumer verified by Y.13 extension |
| `strategy_*` reachable from both tiers | Cross-tier symmetry verified via compile-time embed |
| Every "+X%" claim in post-Z.json | Cites a samply profile line (symbol name + self-time delta) |

---

## Files added

- `crates/ir/src/passes/recognizers/structural_bitmap.rs` (Z.3b)
- `crates/ir/src/passes/csp_strategy/ref_mode.rs` (Z.5)
- `crates/ir/src/passes/csp_strategy/repeat_mode.rs` (Z.5b)
- `crates/core/src/backend/kernels/structural_bitmap.rs` (Z.3b)
- `parse-that/rust/parse_that/src/parsers/scan/structural_bitmap.rs` (Z.3b)
- `docs/benchmarks/post-Z.json` (Z.7)
- `docs/benchmarks/profiles/{pre,post}-Z/*.samply` (Z.7)

## Files modified

- `crates/ir/src/passes/recognizers/mod.rs` — `RecognizerMiner` trait + single-walk orchestrator (Z.0)
- `crates/ir/src/passes/recognizers/{quoted_string,balanced_wrap,comment_ws,identifier,separator_list,token_led_branches,punct_ws_region,delim_scan,key_dispatch}.rs` — `impl RecognizerMiner` (Z.0)
- `parse-that/rust/parse_that/src/parsers/scan/ws_comment.rs` — SIMD intrinsics (Z.1)
- `parse-that/rust/parse_that/src/parsers/scan/number.rs` — SIMD mantissa detection (Z.2)
- `crates/core/src/backend/rust/emitter/alt.rs` — single-variant inlining (Z.3)
- `crates/core/src/backend/rust/emitter/value.rs` — inlined scalar emission (Z.3)
- `crates/ir/src/types/type_desc.rs` — `InlineSpan` variant (Z.3)
- `crates/ir/src/passes/types/constraint/seq.rs` — `InlineSpan` propagation (Z.3)
- `crates/core/src/backend/rust/ir_types.rs` — `InlineSpan` → Rust type (Z.3)
- `crates/core/src/backend/strategy/alt_strategy.rs` — dispatch-fallback consolidation (Z.4)
- `crates/core/src/backend/rust/emitter/dispatch.rs` — consolidated fallback emission (Z.4)
- `crates/core/src/backend/driver/alt.rs` — dispatcher wire-up (Z.4)
- `crates/core/src/backend/rust/analysis/inline.rs` — reduce to feasibility-helper shim (Z.5)
- `crates/core/src/backend/strategy/ref_strategy.rs` — resolver wire-up to `ir.recognizer_decisions.ref_mode` (Z.5)
- `crates/core/src/backend/driver/repeat.rs` — read from `ir.recognizer_decisions.repeat_mode` (Z.5b)
- `crates/core/src/backend/strategy/repeat_strategy.rs` — resolver (Z.5b)
- `crates/ir/src/passes/csp_strategy/mod.rs` — add `RefMode` + `RepeatMode` variable classes + cross-decision constraint edges (Z.5, Z.5b)
- `crates/ir/src/cost_config.rs` — embed `egraph::StrategyConfig`, add `ref_*` and `repeat_*` knobs (Z.5, Z.5b, Z.6)
- `crates/egraph/src/cost_config.rs` — `StrategyConfig` sub-struct (Z.6)
- `parse-that/rust/regex/src/egraph/cost.rs` — read `StrategyConfig` from neutral substrate (Z.6)
- `crates/core/tests/recognizer_decision_consumption.rs` — `RefMode`, `RepeatMode`, `StructuralBitmap` exhaustive match (Z.3b, Z.5, Z.5b)

## Non-goals (Tranche AA)

- **Full simdjson-style tier-based structural indexing with iterate-skipping**: beyond the grammar-agnostic delimiter bitmap of Z.3b. Z.3b is the minimum generalizable version; tier-based indexing is dedicated infrastructure out of Z's scope.
- **Lattice improvements to AC-3 convergence** in type projection: lower priority than CSP expansion; Tranche AA.
- **Memoization strategy CSP**: requires call-site frequency analysis; signal rank 1/5; Tranche AA.
- **Seq CSP and Sp-method CSP**: audit signal rank 2.5/5 and 2/5 respectively — below the Z bar. Tranche AA.
- **Prefix-factoring and dispatch-eligibility threshold as explicit CSP variables**: the three implicit decisions the audit surfaced. Tranche AA, once the Z-core CSPs are landed and validated.

