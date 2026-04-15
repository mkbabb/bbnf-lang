# Tranche W — Activation, Hot-Path Demolition, and Three Distinct Cliffs

## Original edict (carried forward)

> Devise a path forward: audit the hitherto made changes and the remaining plan. **NO quick solutions, NO workarounds: idiomatic, gestalt approaches.** This is a development product. Architectural transpositions in the sake of elegance, simplicity, and performance above all are both necessary and desirable. **NO legacy code.**
>
> Are all of our optimizations (CSP, egraph, structural pattern matching) properly wired in and activating? What of the shared cost analysis structure between regex and the graph? Where are we duplicating effort, cloning, and not taking advantage of rich structure analysis afforded by the IR and DAG? What's our current CSP optimizing for? We should be optimizing both locally and globally and do so in full generality.

This plan is the audit-driven response to that edict. It reads from a four-agent audit (CSP wiring, e-graph cost sharing, cargo-expand generated code, workspace cloning) plus two profiling agents covering all three benches in both directions (parse time + compile time). Every claim is grounded in measured profile data or grep-confirmed source.

The previous plan in this file (Tranche V — Recognizer-Family Transposition) has shipped (post-V baseline at `docs/benchmarks/post-V.json`). It built the substrate. The audits show the substrate is **architectural theatre at the load-bearing points**:

- `crates/ir/src/passes/csp_recognizers.rs` has **zero references to `csp_solver::`**. It's a pattern-match cascade misnamed "csp".
- `crates/core/src/backend/kernels/` has **zero production consumers**. Eight family modules with `quote! { compile_error!("V.8 wires this") }` bodies.
- `CostWeights` is shared by **type only**. Both tiers call `::default()` independently — no runtime coordination, no per-grammar tuning.
- Backend strategy solvers (`alt_strategy.rs` etc.) are **priority cascades** that ignore `ir.recognizer_decisions`. The `// Future: pluggable cost model via CSP constraints` comment is unfulfilled.
- **`OptimizationMode::MinimizeCost` is invoked zero times anywhere in the workspace.** Every real CSP is feasibility-only.

Tranche V was a substrate; Tranche W is its **activation, demolition, and hot-path elimination**.

---

## 1. The three distinct cliffs

The profiling agent confirmed that `compile_bbnf`, `compile_json`, and `compile_css_l4` hit **three structurally different bottlenecks** at meaningful scale. Each one is a separate architectural fix; none of them is solvable by the same change.

| Bench | Time/iter | Top inclusive bottleneck | % of total | Source |
|---|---:|---|---:|---|
| `compile_css_l4` | 126 ms | `bbnf_ir::passes::prefix::factor_literal_prefixes` rebuilding `FxHashMap<String,u32>` per recursive call | **91.73%** | `crates/ir/src/passes/prefix.rs:275-280, 350` |
| `compile_bbnf` | 1.83 ms | `drop_in_place<EGraph<GrammarENode>>` + rayon `LockLatch::wait_and_reset` (grammar-tier e-graph lifecycle) | **25.52% self + 25.24% incl** | `crates/ir/src/egraph/{node,build_egraph}.rs` |
| `compile_json` | 103 µs | `bbnf_regex::info::RegexInfo::analyze_from_hir` → `bbnf_regex::egraph::simplify_hir` (per-pattern HIR e-graph) | **18.25% incl** | `parse-that/rust/regex/src/info/mod.rs:131`, `egraph/mod.rs:118` |

These are not the same problem. CSS L4's cliff is driven by a 148-branch single-literal alternation in `color.bbnf::namedColor` triggering pathological trie recursion. compile_bbnf's cliff is the **fixed cost of constructing-then-tearing-down the grammar-tier e-graph regardless of grammar size** — it dominates a small grammar because it's a per-compile constant overhead. compile_json's cliff is the **per-pattern HIR e-graph saturation** running M times for M unique regex patterns with no caching.

Tranche W must eliminate all three.

The parse benches add a fourth and fifth set of findings:

| Bench | Throughput | Top self | Allocator overhead | Source |
|---|---:|---|---:|---|
| `json_monolithic::canada` | 1106 MB/s (1085 measured) | `parse_that::scan_number_mantissa` 28.58%, `__value` dispatch 18%, eisel-lemire 5.5%, slab clone 3.4% | 12.7% slab construction (`ptr::write` + `Clone::clone` + `Vec::push_mut` + `alloc`) | `parse_that/src/parsers/scan/number.rs`, generated bench code |
| `css_l4::tailwind` | 216 MB/s (208 measured) | `scan_ident` 13.11%, `scan_ws_block_comments` 11.26%, `ptr::write` 6.79%, `[u8]::eq` 4.26%, slab clone 4.01%, `str::is_char_boundary` 3.14% | **17% slab construction**, **10% UTF-8 bounds checks**, **9% literal byte-compare** | generated CSS bench, `parse_that::scan_ident`, `parsers/scan/ws_comment.rs` |

Plus the static-analysis findings from the cargo-expand audit:

- **86 duplicate `is_ascii_digit()` while-loops** in CSS L4 generated parser (~55,900 bytes of byte-equivalent code)
- 24 inline `trim_leading_whitespace_mut` calls in JSON
- 310 `scan_ws_block_comments` calls scattered in CSS
- 37 dispatch tables, ~15-20% structurally identical
- `backend/kernels/*::emit_call` placeholder bodies have **zero production consumers**

---

## 2. The optimizer layers — what is wired and what is theatre

Audit agent A (CSP wiring) and audit agent B (e-graph cost sharing) produced the following ledger.

### 2.1 Real CSPs — feasibility only, never optimization

| File | Real CSP? | Mode | Cost-aware? |
|---|---|---|---|
| `passes/sets/dispatch/eligibility.rs:67` | Yes | `propagate()` only | No |
| `passes/types/mod.rs:61` | Yes | `propagate()` only | No |
| `egraph/csp_scheduler.rs::run` | Yes (dirty propagation) | `propagate()` only | No |
| **`passes/csp_recognizers.rs`** | **No.** Zero `csp_solver::` references | Pattern walk | No |
| `backend/strategy/alt_strategy.rs::decide_alt_strategy` | No. Priority cascade | — | No |
| `backend/strategy/seq_strategy.rs::classify_seq` | No. Pattern match | — | No |
| `backend/strategy/wrap_strategy.rs` | **19 lines, no solver function at all** | — | No |
| `backend/strategy/repeat_strategy.rs` | No. `lo/hi` switch | — | No |
| `backend/strategy/ref_strategy.rs` | No. Pre-computed lookup | — | No |

**Verdict**: There is no cost-driven optimization in the entire workspace. Three real CSPs do feasibility checks; one named "CSP" is a pattern walker; five strategy solvers are priority cascades. `OptimizationMode::MinimizeCost`, `solve_optimized()`, `CostDomain` — the entire optimization side of csp-solver — is **dead API**. The unifying gestalt rule the original edict asks for ("optimizing both locally and globally and do so in full generality") is **structurally impossible today** because no pass minimizes any cost function.

### 2.2 E-graph cost sharing — type-only, no value coordination

`crates/egraph/src/cost_weights.rs::CostWeights` is `pub use`-d by both tiers. Both `crates/ir/src/egraph/cost.rs::GrammarCostModel` and `parse-that/rust/regex/src/egraph/cost.rs::RegexExtractionCost` embed it as `weights: CostWeights`. Both call `CostWeights::default()` independently. **There is no runtime configuration**, no per-grammar tunability, no environment-variable override, no `@cost` directive, no cross-tier feedback. The "shared substrate" rhetoric in CLAUDE.md is structural fiction.

The two extraction tiers also don't talk to each other. `simplify_hir` runs once per `StringId`. `write_back_optimized` runs once per `RuleId`. They produce independent solutions to independent cost problems. There is no global "what's the minimum total emitted code given these constraints" objective anywhere.

### 2.3 The kernel layer — placeholder files

`crates/core/src/backend/kernels/` was created in V.7. Eight modules. Imports from `backend/rust/`: zero. `backend/kernels/balanced_wrap.rs::emit_call` returns `quote! { compile_error!("V.8 wires this") }`. The `quoted_string`, `number`, `identifier`, `comment_ws` modules return `quote! { ::parse_that::scan_*(state) }` but **nothing in the codegen path calls them** — every regex emit site still routes through `generate/regex/emit/scanner_plan.rs` (5 narrow shapes) → `generate/regex/emit/generalized/` (inline loops). The cargo-expand audit confirms: no `kernels::` references in either JSON or CSS generated code.

### 2.4 Hot-path clones — measured per-compile cost

| File:Line | What's cloned | Per-compile cost |
|---|---|---|
| `recognizers/mod.rs:85` | Entire `GrammarDag` (NodeId→DagNode HashMap, 1k–10k entries) | **40k–200k bytes** |
| `passes/sets/sort.rs:23` | `ir.strings: Vec<String>` | 1k–5k bytes |
| `passes/sets/factor_lookahead.rs:19` | `ir.strings: Vec<String>` | 1k–5k bytes |
| `passes/sets/dispatch/build.rs:40,46,116` | `CharSet128` in O(branches²) loop | 200–3000 × 16 bytes |
| **`passes/prefix.rs:275-280`** | **`FxHashMap<String,u32>` per recursive call** | **~115 ms inclusive on CSS L4 (the cliff itself)** |
| `context/propagate.rs:37` | `ContextFacts` per worklist iteration | 80k–320k bytes |
| `passes/types/mod.rs:71,76,...` | `TypeDesc` per node | 1k–3k bytes |
| `passes/types/constraint/*.rs:40-89` | `TypeDesc` in AC-3 | 5k–10k bytes |
| `egraph/extract.rs:92,100`, `language.rs:51` | E-node best-form per fixed-point iteration | ~10k clones × ~32 bytes |

The **worst clone in the codebase is the one in `factor_literal_prefixes`** because it scales with grammar size and recursive depth and gets re-built per call. Profile-confirmed: it's the entire 91.73% of compile_css_l4.

---

## 3. Architectural commitments (what makes Tranche W gestalt)

These rules govern what lands and what does not. They are not aspirational; they are the gates.

1. **Every file with "csp" in its name uses `csp_solver::Csp`.** `csp_recognizers.rs` is renamed `csp_strategy.rs` and rewritten to call `csp.add_constraint(...)`, `csp.finalize()`, `csp.solve(&config)`. If it doesn't actually solve constraints, the file doesn't exist.
2. **Every file under `backend/kernels/` is consumed by at least one production codegen call site.** Placeholder files with `compile_error!` bodies are deleted.
3. **`OptimizationMode::MinimizeCost` is invoked at least once in production code.** The strategy CSP from rule #1 is the first.
4. **`CostWeights` is read from `GrammarIR.cost_config` everywhere.** No more `::default()` calls in cost-model constructors. Per-grammar tunability is real (env var or `@cost` directive).
5. **No data structure > 1 KB is cloned in a hot pass unless audit-justified inline.** The seven hot-path clones in §2.4 are eliminated.
6. **The strategy solvers consume `ir.recognizer_decisions`.** No more priority cascades. Each `decide_*` is a thin lookup.
7. **`factor_literal_prefixes` runs in O(grammar.size)`, not O(rules × strings_len × depth).** The dedup map is built once and threaded through, keyed by `&[u8]` against `&[String]`, not cloned per call.
8. **The grammar-tier e-graph builds and tears down at most once per compile, with hash-cons tables sized appropriately.** The `drop_in_place<EGraph<GrammarENode>>` cost is measured down to <5% of compile_bbnf or the e-graph is structurally restructured to be lazier.
9. **`simplify_hir` is invoked at most once per canonical pattern shape per compile.** The HIR e-graph caches across `StringId` lookups.
10. **Tranche W is one commit series.** No co-existence of legacy and new at any commit boundary. Deletions ship with the additions that replace them.
11. **Bench gates are non-regression vs post-V on every row, with at least one row improving by ≥1% parse time and `compile_pipeline::compile_css_l4` improving by ≥50%.** That last number is conservative — the profile says 7-10× is achievable.

---

## 4. What lands — six demolition phases, one tranche

Each phase is a logical commit (some are multiple commits). The tranche is the union. Implementation dependency order; later phases depend on earlier substrates.

### Phase 0 — `factor_literal_prefixes` rewrite (single largest win)

**File**: `crates/ir/src/passes/prefix.rs:252-381`

The audit found two structural mistakes in `factor_literal_prefixes`:

1. **Per-call dedup map rebuild** (lines 275-280):
   ```rust
   let mut dedup: FxHashMap<String, u32> = FxHashMap::default();
   for (i, s) in strings.iter().enumerate() {
       dedup.entry(s.clone()).or_insert(i as u32);
   }
   ```
   On CSS L4, `strings` has thousands of entries. Every recursive call clones every string. This produces every sample of `RawTable::reserve_rehash`, every `String::clone`, every `drop_in_place::<(String, u32)>` you see in the profile. **44.5% of compile_css_l4 is libsystem_malloc.** That's this loop.

2. **Recursive self-call at line 350** with the same pattern: every level of the trie rebuilds the full dedup. For CSS's `namedColor` (148 single-literal branches, longest ~20 chars), this recurses ~20 levels deep, rebuilding a thousand-entry hashmap at each level.

3. **Three recursive descents into Alt children** (lines 40, 59, 75) with `IrNode` clones at lines 158, 168, 174, 184, 200, 370 — each clone walks an `IrNode` subtree.

**Fix**:

- Build the dedup map **once per pass invocation** in `factor_common_prefixes`. Pass it through as `&FxHashMap<&[u8], u32>` keyed against the `&[String]` reference, so no `String::clone` ever runs.
- Convert `factor_literal_prefixes` from a free function to a method on a `FactorCtx` struct that owns:
  - The shared dedup map (`&FxHashMap<&[u8], u32>`)
  - Scratch buffers for `Vec<Option<LiteralBranchInfo>>`, `Vec<Option<u8>>`, `Vec<AltBranch>` (cleared per call, not allocated)
  - The `&[String]` strings table reference
- Eliminate the `IrNode` clones in lines 158/168/174/184/200/370 via `std::mem::replace` and move semantics. Use `strip_leading_take(&mut node)` instead of `strip_leading(node.clone())`.
- The trie recursion structure stays — it's correct — only the dedup map and the clone behavior change.

**Estimated impact** (from the profiling agent's recommendation #1): **7-10× speedup on compile_css_l4**, dropping it from 126 ms to ~13-18 ms. The hashbrown 23% + libsystem_malloc 44.5% + hash_bytes 11% sums together collapse. This single change recovers the V.11 regression and delivers an additional 5-7× improvement on top.

### Phase 1 — Grammar-tier e-graph lifecycle (compile_bbnf cliff)

**Files**: `crates/ir/src/egraph/{mod,build_egraph,write_back,node}.rs`, `crates/egraph/src/egraph.rs`

The compile_bbnf cliff is **`drop_in_place<EGraph<GrammarENode>>` at 25.52% self time** plus **rayon `LockLatch::wait_and_reset` at 25.24% inclusive**. For a 80-line grammar with no large literal alternations, this is a per-compile fixed cost dominating because everything else is small.

The drop is expensive because the e-graph carries hash-cons tables (`HashMap<GrammarENode, Id>`), e-class member lists, parent edges, analysis data — all of which need to be deallocated. For BBNF (~150 e-classes, ~300 e-nodes), the absolute cost is small, but the **profile says it's 25% of total compile time**, meaning the e-graph constructor + saturator + drop together cost ~460 µs out of 1.83 ms. Half of that is the drop alone.

Two diagnostic angles must run before the fix:

1. **Why is drop so expensive?** Hash-table teardown is O(capacity), not O(size). If the hash-cons table was overprovisioned (e.g., default initial capacity 1024 for a 300-node graph), the drop walks empty buckets. **Fix**: size hint the hash-cons table from the IR node count via `EGraph::with_capacity_for_ir(ir)`.

2. **Why does rayon show up?** `egraph::csp_scheduler` runs the saturator with rayon parallelism. For a small graph the parallelism overhead exceeds the work. **Fix**: gate `CspScheduler` parallelism on `e-graph node count > threshold` (e.g., 1000); fall back to serial scheduling for small graphs. The threshold is empirical and lives in `CostConfig`.

3. **Cross-cutting fix**: extend `EGraph::clear()` (instead of dropping) so subsequent compiles in the same process reuse the allocations. The bench harness creates a fresh `GrammarIR` per iter; each iter pays a fresh e-graph construction. Pooling the e-graph across iters via a thread-local `RefCell<EGraph<GrammarENode>>` would eliminate the drop cost entirely from steady-state benches. (Real compile invocations also see this win for daemon-style workloads like LSP.)

**Estimated impact**: compile_bbnf drops from 1.83 ms to ~1.2-1.4 ms (25-35% improvement). compile_json sees a smaller benefit since regex-tier dominates it. compile_css_l4 sees no benefit (the `factor_literal_prefixes` cliff dwarfs everything).

### Phase 2 — HIR e-graph caching (compile_json cliff)

**Files**: `crates/ir/src/passes/regex_info.rs`, `parse-that/rust/regex/src/info/mod.rs`, `parse-that/rust/regex/src/egraph/mod.rs`

JSON's 18.25% inclusive in `compute_regex_info` decomposes into `RegexInfo::analyze_from_hir` → `simplify_hir`. For each unique `StringId`, the HIR e-graph is built, saturated, extracted, and dropped — once per pattern. JSON has 4 patterns; CSS L4 has dozens. There is no caching.

Many patterns canonicalize to the same form (regex algebra dedups overlapping classes, common prefixes). Two patterns that look different in source can simplify to the same canonical Hir. Today they re-saturate independently.

**Fix**:

- Add a per-compile cache `SaturationCache: HashMap<StringId, Hir>` populated by `compute_regex_info` and consumed by every downstream consumer that needs canonical HIR.
- `RegexInfo::analyze_from_hir` checks the cache before invoking `simplify_hir`.
- For multi-pattern dedup: hash the canonical form and dedupe across `StringId`s.
- Cross-grammar caching (between compile invocations) is out of scope for this tranche; per-compile dedup only.

**Estimated impact**: compile_json drops from 103 µs to ~80-90 µs. Larger grammars with regex-heavy bodies (CSS L4 has many regexes too, but they're invisible behind the prefix.rs cliff) see proportional improvement once Phase 0 lands.

### Phase 3 — Activate the optimizer layers (turn theatre into solving)

**Files**: `crates/ir/src/passes/csp_recognizers.rs` (deleted), `crates/ir/src/passes/csp_strategy.rs` (new), `crates/ir/src/cost_config.rs` (new), `crates/ir/src/types/grammar.rs` (extends `GrammarIR`), backend strategy solvers, backend/kernels/

This is the substrate-activation work. It's split into four substeps that share a commit series:

#### 3a — `CostConfig` shared substrate

```rust
// crates/ir/src/cost_config.rs (new)
pub struct CostConfig {
    pub egraph: egraph::CostWeights,         // shared with both tiers
    pub literal_per_byte: f64,
    pub class_cost: f64,
    pub repeat_cost: f64,
    pub merged_bonus: f64,
    pub token_dispatch_bonus: f64,
    pub lookahead_penalty: f64,
    pub hoist_size_savings: f64,
    pub unroll_bound: u32,
    pub egraph_parallelism_threshold: usize,
    // ... per-grammar tunables ...
}
```

`GrammarIR` gains a `cost_config: CostConfig` field. `GrammarCostModel::from_config(&CostConfig)` and `RegexExtractionCost::from_config(&CostConfig)` constructors replace the `::default()` calls. Both tiers read from the same struct. Per-grammar tunability via `BBNF_COST_*` environment variables for benchmarking; the `@cost` directive lands in a future tranche.

#### 3b — Real strategy CSP

Delete `crates/ir/src/passes/csp_recognizers.rs`. Create `crates/ir/src/passes/csp_strategy.rs` that **actually uses csp-solver**:

- Variables: per-Alt `AltMode`, per-Wrap `WrapMode`, per-Seq `SeqMode`, per-Repeat `RepeatMode`, per-Ref `CallStrategy`, per-(node, sid) `RegexEngine`, per-rule `MemoMode`, per-group `HoistPlan`. Each as `csp_solver::Variable<BitsetDomain>` (the existing fast bitset domain).
- Constraints: domain restriction from `NodeFacts.recognizer` (mining produces feasibility); `ImplicationConstraint` (V.1) for parent-child compatibility ("if `AltMode = TokenDispatch` then all child engines must be one-pass"); `CardinalityConstraint` (V.1) for hoisting threshold.
- Objective: weighted sum from `CostConfig` minus hoist sharing bonus. Solved via `csp.solve(&SolveConfig { optimization_mode: OptimizationMode::MinimizeCost, .. })`. **The first production use of the cost-optimization API.**
- Two-stage solve: per-rule local AC-3 first, then global pass over `HoistPlan` only. Keeps the global problem ~200 vars on CSS L4.

The output is `RecognizerDecisionMap` with the same shape as V.6, but produced by an actual CSP solve. The pipeline call site renames from `solve_recognizer_decisions` to `solve_strategy_decisions`.

#### 3c — Strategy solver migration

Every `decide_*` function under `crates/core/src/backend/strategy/{alt,seq,wrap,repeat,ref}_strategy.rs` becomes a thin lookup against `ir.recognizer_decisions`. Priority cascades deleted. The detection halves of `backend/patterns/{delim_scan,key_dispatch}.rs` move into the strategy CSP's domain feasibility phase. `backend/patterns/cache.rs` is replaced by direct `recognizer_decisions` reads.

#### 3d — Wire backend/kernels/

Replace every placeholder body in `backend/kernels/*.rs`:

- `quoted_string` → `parse_that::scan_string_quoted` / `quoted_string_scan_full`
- `number` → `number_fused_scan_convert` / `number_span_scan_strict`
- `identifier` → `parse_that::scan_ident`
- `comment_ws` → `parse_that::scan_ws_block_comments`
- `charclass` → `find_first_of_4` / `nibble_lut` / inline byte-range with helper hoisting
- `prefix_class` → `memcmp` + tail dispatch
- `balanced_wrap` → `parse_that::scan_balanced` (replaces `delim_scan` config consumption)
- `sep_list` → `memchr2` + element callback

Add a `KernelRegistry` struct on `DriverState` that hashes `RecognizerSignature`, emits each helper exactly once per generated module, and returns call-site `TokenStream`s for subsequent uses. The 86 duplicate CSS digit-scan loops collapse to one helper definition + 86 call sites.

`crates/core/src/backend/rust/emitter/leaves.rs` (and TS / WASM equivalents) are wired to dispatch through the registry for family-classified patterns. Opaque patterns fall through to the existing HIR walker or DFA.

### Phase 4 — Hot-path clone elimination

The audit found seven specific clones to fix. Five are independent of Phases 0-3 and should land first. Two depend on the strategy migration:

**Independent**:

1. `recognizers/mod.rs:85` — borrow `&ir.dag` instead of cloning. Single largest non-prefix.rs clone (40k-200k bytes).
2. `passes/sets/sort.rs:23` and `factor_lookahead.rs:19` — borrow `&[String]`.
3. `passes/sets/dispatch/build.rs:40,46,116` — borrow `&CharSet128` in the O(branches²) pairwise check.
4. `context/propagate.rs:37` — match on `Option<&ContextFacts>` instead of `.cloned().unwrap_or_default()`.
5. `egraph/extract.rs:92,100` and `language.rs:51` — investigate whether the `.clone()` of best-form e-nodes can be avoided via `Cow<'_, N>` or by storing `Box<N>` in the table.

**Dependent on §3c (strategy migration)**:

6. `passes/types/mod.rs:71,76,99,...` and `types/constraint/*.rs:40-89` — once strategy CSP consumes type info, the per-node `TypeDesc` clones become redundant. Use `Rc<TypeDesc>` or reference wrapper.

### Phase 5 — Parser hot-path improvements (parse-time win)

**Files**: `parse-that/rust/parse_that/src/state.rs`, `parse-that/rust/parse_that/src/bump_slab.rs`, `crates/core/src/backend/rust/emitter/{leaves,alloc_emit}.rs`, generated parser code

Five parse-time profile-driven optimizations from the agent's ROI ranking:

#### 5a — `&[u8]` threading (eliminates UTF-8 bounds checks)

**Profile evidence**: CSS tailwind 3.14% `str::is_char_boundary` + 5.21% `RangeFrom::index` + 1.89% `get_unchecked` = **~10% of parse time** = 2 ms/iter. JSON canada 1.42%.

`ParserState::remaining()` returns `&str`. Every `&input[state.offset..]` re-validates the offset is a char boundary. The slab-allocated parser doesn't actually need str-level slicing — it works on bytes. UTF-8 validation should happen **once at parse entry** via `std::str::from_utf8(input)`, then the parser operates on `&[u8]`.

**Fix**: convert `ParserState::remaining()` to return `&[u8]`. Span extraction at the end of a successful match converts back via `from_utf8_unchecked` on the validated range. Literal compares become byte-wise. Regex is already byte-oriented.

**Impact**: ~10% on CSS tailwind (2 ms drop, 216 → 240 MB/s). ~1.5% on JSON canada.

#### 5b — `CssL4ParserEnum::clone` → `Copy` fast-path

**Profile evidence**: CSS tailwind 4.01% self in `clone` + 6.79% in `ptr::write` + 2.57% in `alloc_slice_clone` = **~13%** = 2.6 ms.

`BumpSlab::alloc_slice_clone` walks `Clone::clone` on each element. For POD enum variants (most BBNF-generated enums carry `NonNull<T>`, `Span`, `u32` — all `Copy`), this is a memcpy that should be a single `copy_nonoverlapping` for the whole slice.

**Fix**: in `derive(Parser)` codegen, detect when all variants of `<Grammar>ParserEnum` are POD and derive `Copy`. Switch `alloc_slice_clone` → `alloc_slice_copy` (a new `BumpSlab` method that uses `ptr::copy_nonoverlapping`). Alternatively, `#[repr(transparent)]` over `[u8; 48]` with a single `copy_nonoverlapping` per element.

**Impact**: ~6-8% on CSS tailwind, ~2% on JSON canada.

#### 5c — Perfect-hash literal dispatch

**Profile evidence**: CSS tailwind `<[u8] as PartialEq>::eq` + `equal_same_length` + `<&str>::starts_with` + `<[u8]>::starts_with` = **~12%** of parse time = 2.4 ms.

For factored Alts whose remainders are all ≤4-byte literals, generate a perfect-hash dispatch: read the next 1-4 bytes as a u32 (masked for length), `match` against constants. This collapses N × `[u8]::eq` to one u32 load + match.

**Fix**: extend `crates/core/src/backend/rust/emitter/dispatch.rs` to detect this case and emit u32-keyed match arms when the factored Alt's remainders fit. The `__namedColor` dispatch table (from CSS L4) is the prototypical case.

**Impact**: ~8-10% on CSS tailwind. Minor on JSON.

#### 5d — Hoisted digit / alphanumeric scanners

**Profile evidence (cargo expand audit)**: 86 inline `is_ascii_digit()` while-loops in CSS L4 generated code = ~55,900 bytes of duplicated code. Each loop is a generated `while __pos < __end { let __b = ...; if __b.is_ascii_digit() { __pos += 1; } else { break; } }` body.

**Fix**: when `RegexClass::CharClassQuantified { class: DIGITS, .. }` (or similar) is detected, route through `kernels::charclass::emit_call` which produces a single `parse_that::scan_digits_mut(state)` helper call. Add the `scan_digits_mut` and `scan_alnum_mut` helpers to `parse_that::scanners` (using `find_first_of_4` / `nibble_lut` internally).

**Impact**: 55,900 bytes of generated code → ~1 helper definition + 86 call sites. Cache locality + I-cache pressure improvements (~3-5% CSS parse). This is also Phase 3d's `KernelRegistry` doing real work.

#### 5e — JSON parser slab construction reduction

**Profile evidence**: JSON canada 12.7% in slab construction (`ptr::write` + `Clone::clone` + `Vec::push_mut` + `alloc`).

The `JsonParserEnum::clone` 3.4% comes from the array hot path: every value gets cloned somewhere in the array element vec management. Investigation needed (likely `alloc_slice_clone` path same as 5b). The fix from 5b applies if `JsonParserEnum` is POD; verify.

---

## 5. Hard gates

| Gate | Threshold |
|---|---|
| All workspace tests pass | yes |
| `bbnf-regex` tests | 25/25 |
| Bootstrap | succeeds; MD5 unchanged or new MD5 committed |
| Consumer-invariant test | passes |
| Grep invariant — deleted files unreferenced | zero hits |
| `csp_solver::Csp::solve` invocation count in workspace | **≥ 1** (currently 0) |
| `OptimizationMode::MinimizeCost` invocation count | **≥ 1** (currently 0) |
| `backend/kernels/` `compile_error!` stub count | **0** |
| `backend/kernels/` consumer count | ≥ 1 production caller per family module |
| `cargo bench compile_pipeline::compile_css_l4` | **≥ 50% improvement** vs post-V (126ms → ≤63ms; profile predicts ~13-18ms) |
| `cargo bench compile_pipeline::compile_bbnf` | **≥ 25% improvement** (1.83ms → ≤1.37ms) |
| `cargo bench compile_pipeline::compile_json` | **≥ 10% improvement** (103µs → ≤93µs) |
| `cargo bench json_monolithic::canada` parse | **non-regression**; ideally +1-2% (number scan path is already optimal) |
| `cargo bench css_l4::tailwind` parse | **≥ +5% improvement** (slab Copy + literal dispatch + ws hoisting cumulative) |

Failure on any hard gate holds the tranche open. No workarounds. No deferrals.

---

## 6. Files added

- `crates/ir/src/cost_config.rs` (new) — shared `CostConfig` substrate
- `crates/ir/src/passes/csp_strategy.rs` (new) — replaces `csp_recognizers.rs`, actually uses csp-solver
- `crates/ir/src/passes/prefix.rs` — substantial rewrite of `factor_literal_prefixes` (introduces `FactorCtx`)
- Real bodies in `crates/core/src/backend/kernels/*.rs` — replaces V.7 placeholders
- `crates/core/src/backend/driver/kernel_registry.rs` (new) — signature dedup + helper emission
- `crates/core/src/backend/types/decisions.rs` (rename target for `backend/patterns/decisions.rs`)
- `parse-that/rust/parse_that/src/scanners.rs` — adds `scan_digits_mut`, `scan_alnum_mut`
- `parse-that/rust/parse_that/src/bump_slab.rs` — adds `alloc_slice_copy<T: Copy>`
- `docs/benchmarks/post-W.json`

## 7. Files modified

- `crates/ir/src/types/grammar.rs` — adds `cost_config: CostConfig` field
- `crates/ir/src/egraph/cost.rs`, `parse-that/rust/regex/src/egraph/cost.rs` — read from `CostConfig`
- `crates/ir/src/egraph/{mod,build_egraph,write_back}.rs` — capacity hints for hash-cons; optional pooling via `EGraph::clear()` instead of drop
- `crates/egraph/src/csp_scheduler.rs` — gate parallelism on node count (read from `CostConfig.egraph_parallelism_threshold`)
- `crates/ir/src/passes/recognizers/mod.rs` — borrow `&ir.dag` instead of cloning
- `crates/ir/src/passes/sets/sort.rs`, `factor_lookahead.rs` — borrow `&[String]`
- `crates/ir/src/passes/sets/dispatch/build.rs` — borrow `CharSet128`
- `crates/ir/src/passes/context/propagate.rs` — borrow `ContextFacts` in worklist
- `crates/ir/src/passes/regex_info.rs` — populate `SaturationCache`
- `parse-that/rust/regex/src/info/mod.rs` — `analyze_from_hir` reads cache
- `parse-that/rust/regex/src/egraph/mod.rs` — exposes cache-aware `simplify_hir`
- `crates/ir/src/passes/mod.rs` — `csp_recognizers` → `csp_strategy`
- `crates/core/src/pipeline/compile.rs` — call `solve_strategy_decisions` (renamed)
- `crates/core/src/backend/strategy/{alt,seq,wrap,repeat,ref}_strategy.rs` — read from `ir.recognizer_decisions`; delete priority cascades
- `crates/core/src/backend/driver/{alt,seq,repeat,wrap,node,map,reference}.rs` — pure dispatchers
- `crates/core/src/backend/rust/emitter/leaves.rs` (+ TS / WASM equivalents) — wire `KernelRegistry`
- `crates/core/src/backend/rust/emitter/dispatch.rs` — perfect-hash u32-keyed dispatch
- `crates/core/src/backend/rust/alloc_emit.rs` — emit `alloc_slice_copy` for POD enums
- `crates/core/src/generate/regex/emit/mod.rs` — delete hard-coded special cases
- `parse-that/rust/parse_that/src/state.rs` — `remaining()` returns `&[u8]`
- `crates/{ir,core}/CLAUDE.md`, repo-root `CLAUDE.md` — documentation refresh

## 8. Files deleted

- `crates/ir/src/passes/csp_recognizers.rs` (replaced by `csp_strategy.rs`)
- `crates/core/src/generate/regex/emit/scanner_plan.rs`
- `crates/core/src/generate/regex/emit/generalized/mod.rs`
- `crates/core/src/generate/regex/emit/generalized/class_segments.rs`
- `crates/core/src/generate/regex/emit/generalized/` (the directory)
- `crates/core/src/backend/patterns/delim_scan.rs`
- `crates/core/src/backend/patterns/key_dispatch.rs`
- `crates/core/src/backend/patterns/cache.rs`
- `crates/core/src/backend/patterns/mod.rs`
- `crates/core/src/backend/patterns/` (the directory)

`crates/core/src/backend/patterns/decisions.rs` is **renamed**, not deleted.

---

## 9. Verification

```bash
# Tests
cargo test --workspace --exclude bbnf-lsp --exclude bbnf-analysis
cd /Users/mkbabb/Programming/parse-that/rust/regex && cargo test
cd /Users/mkbabb/Programming/bbnf-lang

# Bootstrap
bash scripts/bootstrap-bbnf.sh
md5 -q crates/core/src/grammar/generated.rs

# Bench sweep
cargo bench -p bbnf --bench json_monolithic --bench css_l4 --bench compile_pipeline 2>&1 | tee /tmp/post-W-benches.txt

# Architectural assertions
grep -rn "csp_solver::Csp" crates/ir/src/passes/csp_strategy.rs   # must be non-empty
grep -rn "OptimizationMode::MinimizeCost" crates/                  # must be non-empty
grep -rn "compile_error" crates/core/src/backend/kernels/          # must be empty
grep -rn "csp_recognizers" crates/                                 # must be empty
grep -rn "scanner_plan\|emit::generalized" crates/                 # must be empty
grep -rn "backend::patterns::" crates/                             # must be empty (decisions.rs renamed)

# Consumer-invariant
cargo test -p bbnf --test recognizer_consumer_invariant

# Profile-confirmed cliffs
samply record --save-only -o /tmp/post-W-css.json -- target/release/deps/compile_pipeline-* --bench compile_css_l4
# Top inclusive in factor_literal_prefixes must be < 30% (currently 91.73%)

samply record --save-only -o /tmp/post-W-bbnf.json -- target/release/deps/compile_pipeline-* --bench compile_bbnf
# drop_in_place<EGraph<GrammarENode>> self time must be < 5% (currently 25.52%)

samply record --save-only -o /tmp/post-W-json.json -- target/release/deps/compile_pipeline-* --bench compile_json
# simplify_hir inclusive must be < 4% (currently 8.34%)
```

If any cliff measurement still exceeds the post-W threshold, the tranche is incomplete.

---

## 10. Non-goals

- No new SIMD intrinsics beyond `scan_digits_mut` / `scan_alnum_mut`. The substrate is mature; the wins are wiring + clone removal + cliff demolition.
- No e-graph rule additions in Tranche W. The five grammar-tier and five HIR-tier shape-exposure rules from the original Tranche V plan are still deferred (Tranche X, post-W validation).
- No global cross-grammar HIR e-graph caching across compile invocations; per-compile dedup only (`SaturationCache`).
- No staged half-states. V.4/V.6 introduced the recognizer field + decision map; Tranche W activates them with a real CSP and a real kernel registry. There is no third intermediate.
- No "follow-up tranche W2 to delete legacy". The deletions in §8 happen in this tranche.
- No `@cost` directive yet — `CostConfig` is read from defaults plus `BBNF_COST_*` environment variables for benchmarking.
- No regex DFA compile profiling pursued in this tranche — the profiles confirmed `bbnf_regex::*` is 0.07% max-inclusive of grammar compile, so DFA compile is not a systemic blocker. It's a candidate for a separate tranche if and when DFA-compiled patterns become a real workload concern.

---

## Reference: previous plan in this file

The tranche immediately preceding this one was **Tranche V — Recognizer-Family Transposition**. It built the substrate Tranche W activates: csp-solver `ImplicationConstraint` + `CardinalityConstraint`; `RegexClass::{CharClassQuantified, PrefixThenClass, AccelDriven}`; `EngineSet`; `RecognizerInfo` trait extensions with `Token`/`DispatchGroup`/`DelimScan` wrappers; `NodeFacts.recognizer` field with `Recognizer`/`RecognizerShape`/`RecognizerSignature`; `mine_recognizers` pass with eight miners; `csp_recognizers.rs` (which Tranche W replaces); `backend/kernels/` directory (which Tranche W wires); `DriverState::recognizer_decision` accessor; consumer-invariant grep test; `docs/benchmarks/post-V.json`.

V.11 noted honestly in its baseline JSON: *"spent without gain"* — the substrate exists but no emission consumer reads `ir.recognizer_decisions`. **Tranche W is what makes that statement obsolete.**

