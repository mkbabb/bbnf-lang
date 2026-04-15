# Tranche Y — Activation Truth II: Load-Bearing Emission, Cross-Rule Optimization, and Parse-Time SIMD

## Context

Tranche X shipped through commit `8274e15` (X.8e). Of the bench gates it set, only one landed: `compile_css_l4 −30.1%` (Phase X.0's `find_node_id_for_var` O(1) reverse map). Every parse-time gate missed, most as regressions (`json_canada −3.9%`, `css_tailwind −5.6%`, `css_bootstrap −7.4%`), and every compile-time gate except CSS L4 also missed.

A three-agent audit (ghost substrate, SIMD state, bench infrastructure) converged on three findings:

1. **The parse-time regressions are a single cold probe.** `backend/driver/node.rs::try_emit_family_kernel` calls `dag.node_for + node_facts.get` on every node. CSS L4 and JSON grammars match zero of the four new recognizer families (FunctionHead, HashPrefix, UnitTail, PunctWsRegion), so the probe is pure overhead on every parse-time bench.

2. **Half of X's activation substrate is ghost code.** `AltMode::SharedHelper` and `WrapMode::SharedHelper` are emitted by `csp_strategy.rs` but fall through to `Checkpoint` in `alt_strategy.rs`. `AltMode::TokenDispatch` has elevation logic but still falls through in the hard case. `backend/patterns/` still exists as 4 re-export shims with 11 import sites. `RegexInfo::decisions` is populated but JSON emit sites don't consume it.

3. **The CSP solver is unbounded.** `csp-solver` has no iteration cap, no depth bound, no wall-clock timeout. The X.6 global-CSP attempt hung `compile_css_l4` at 94ms (10× blowup) because branch-and-bound explored the cross-product with no guard. If Y's connected-components decomposition misjudges a component or Y.3's token-dispatch broadening hits a pathological grammar, the same freeze recurs. **Benches and tests have no timeouts either** — a hung compile hangs the whole run.

Tranche Y's job is to make every X decision load-bearing, fix the measured parse-time cliffs with SIMD, and **install the freezing guards that X.6 should have had**. No ghost substrate. No legacy shims. No unbounded search.

---

## Safety prelude — Phase Y.-1: freezing guards

**This phase lands before any Y work that could trip unbounded search.** The X.6 freeze is documented in source; Y.3 (token-dispatch broadening) and Y.5 (connected-components CSP) both touch the same failure surface. The guards must be in place before the surface is touched again.

### Y.-1.a — CSP solver budget

**Files**: `/Users/mkbabb/Programming/csc411/CSC411_HW2_ProgrammingQuestion/csp-solver/src/lib.rs`, `.../src/solver/backtrack.rs`, `.../src/solver/optimize.rs`

Add `SolveConfig::node_budget: Option<u64>` (default `Some(1_000_000)`). The backtracking and branch-and-bound recursions increment a shared counter; when the budget is exceeded, the solver returns `Err(SolveError::BudgetExceeded)` carrying the best-so-far solution for `MinimizeCost`.

Call sites in `crates/ir/src/passes/csp_strategy.rs` catch `BudgetExceeded`, log one line per blown component, and fall back to the per-variable trivial pick. This is a **structured failure mode**, not a panic: a budget hit produces a valid (if suboptimal) decision map and lets the compile finish.

### Y.-1.b — Bench wall-clock guards

**Files**: `crates/core/benches/compile_pipeline.rs`, `crates/core/benches/json_monolithic.rs`, `crates/core/benches/css_l4.rs`, new `crates/core/benches/common/timeout.rs`

`bencher` has no built-in timeout. Install a minimal wall-clock guard: a helper `fn bench_with_timeout<F>(b: &mut Bencher, limit: Duration, f: F)` that wraps the iteration. Each bench iteration checks `Instant::now() - start > limit` at the top of the closure; exceeding it panics with a clear message (`"bench exceeded N seconds — likely a performance regression"`).

Per-bench limits (generous but finite):
- `compile_pipeline::compile_json`: 5 ms × sample ceiling → 50 ms per iteration
- `compile_pipeline::compile_css_l4`: 100 ms per iteration (current ~7 ms; 14× headroom)
- `compile_pipeline::compile_bbnf`: 50 ms per iteration
- `json_monolithic::*`: 1 s per iteration
- `css_l4::tailwind`: 5 s per iteration (tailwind CSS is 3.6 MB)
- All others: 500 ms per iteration default

When a bench trips the guard, criterion/bencher aborts with a clear diagnostic instead of hanging CI.

### Y.-1.c — Test timeouts via cargo-nextest

**Files**: `.config/nextest.toml` (new) OR workspace `Cargo.toml` + documentation

Add a `.config/nextest.toml` with:

```toml
[profile.default]
slow-timeout = { period = "30s", terminate-after = 3 }
leak-timeout = "100ms"

[profile.ci]
slow-timeout = { period = "60s", terminate-after = 2 }
fail-fast = false
```

Update `Makefile` targets (`test`, `bench`) to use `cargo nextest run` / `cargo nextest run --profile ci` where possible, retaining `cargo test` as a fallback. The `terminate-after = 3` flag kills a test that stays "slow" for three 30-second windows — 90 seconds max before a runaway test is killed, instead of the current unbounded default.

### Y.-1 gate

- `csp-solver` has a non-`None` default node budget AND budget-exceeded is handled at all Y call sites
- Running `cargo bench -p bbnf --bench compile_pipeline` with a deliberate `std::thread::sleep(Duration::from_secs(10))` injected into a pass is killed by the guard within the per-bench limit (manual test, not committed)
- `cargo nextest run` completes a full workspace sweep with no unbounded-wait failures

**Estimated impact**: zero runtime cost in the happy path (the budget counter is a single `u64` increment). Infinite value when a regression would otherwise freeze CI.

---

## Architectural commitments (hard rules)

1. **Zero ghost substrate.** Every `AltMode`, `WrapMode`, `RegexEngine` variant has ≥1 production consumer whose output is observable in cargo-expand or bench numbers. Variants that cannot satisfy this are **deleted from the enum and CSP domain**, not kept as "Tranche Z substrate".

2. **Driver probe elimination.** `backend/driver/node.rs::compile_node` does not pay the per-node `try_emit_family_kernel` probe when no family shape matches. Elided via `ir.has_family_recognizers: bool`, OR the zero-match families are deleted in Y.4.

3. **Cross-rule CSP via connected components.** One `Csp::<StrategyDomain>` per compile; union-find partitions variables by `ImplicationConstraint` edges; per-component solve. `compile_css_l4` regresses ≤5% versus per-rule post-X.

4. **Cross-tier cost feedback.** `RegexExtractionCost::from_config(&CostConfig)` mirrors `GrammarCostModel::from_config`. Grep invariant: no `RegexExtractionCost::default()` in production.

5. **Authoritative `RegexEngine` consumption at emit sites.** Per the user's split of Y.6 into Y.6a/Y.6b: JSON and CSS kernel emit sites (`kernels::quoted_string::emit_json_call`, `kernels::number::emit_call_*`) read the chosen `RegexEngine` from `ir.regex_info.decisions` directly. This closes X.11a as a real deliverable, not a side effect of cost feedback.

6. **`backend/patterns/` fully deleted.** Directory gone. `decisions.rs` moves to `backend/types/decisions.rs`. Grep: `grep -rn "backend::patterns\|crate::backend::patterns\|super::patterns" crates/` returns zero hits.

7. **SharedHelper load-bearing.** For each of `quoted_string`, `identifier`, `comment_ws`, `balanced_wrap`: ≥1 production hoisting site in expanded CSS L4 AND a `parse_*_shared` function definition. For JSON: `parse_json_string_shared`, `parse_json_number_shared` present.

8. **`AltMode::TokenDispatch` authoritative or gone.** Either `fuse_token_dispatch` converts every CSP-flagged Alt upstream (Option A), OR the backend emits TokenDispatch from the Alt at codegen (Option B). `alt_strategy.rs` fallthrough-to-Checkpoint is deleted either way.

9. **Family recognizer match-or-delete — staged order.** Per user feedback: (1) profile match counts, (2) relax detection for Ref-wrapped / inlined / normalized shapes, (3) re-measure, (4) delete only then. Deletion is the last resort, not the first response.

10. **TypeDesc clone elimination in AC-3.** Four clone sites in `crates/ir/src/passes/types/constraint/domain.rs` (lines 40, 57, 69, 75) reduced to ≤1 via `&TypeDesc` propagation through the join lattice.

11. **Parse-time SIMD hits the gates.** `json_canada` +5% and `css_tailwind` +15% versus post-W. Contingent on Y.7/Y.8/Y.9 landing cleanly.

12. **E-graph rule additions are de-prioritizable.** Y.11 (common-suffix) and Y.12 (distributive) are in scope but **do not gate tranche closure** if parse/activation phases run long. Per user feedback: parse-time and activation gates close before schedule is spent on the e-graph pair. If pressure arrives, Y.11/Y.12 move to Tranche Z.

13. **Legacy code: zero tolerance.** `backend/patterns/` deleted, probe elided or families deleted, `alt_strategy` Checkpoint fallthrough for non-Checkpoint variants deleted.

14. **Bench gates (vs post-W, from post-X baseline)**:

| Bench | post-W | post-X | target |
|---|---:|---:|---:|
| `compile_css_l4` | 10.21 ms | 7.14 ms | **≤6.13 ms (−40%)** |
| `compile_bbnf` | 987 µs | 933 µs | **≤888 µs (−10%)** |
| `compile_json` | 108 µs | 98.4 µs | **≤97 µs (−10%)** |
| `json_canada` parse | 1188 MB/s | 1142 MB/s | **≥1247 MB/s (+5%)** |
| `json_citm` parse | 1897 MB/s | 1881 MB/s | **≥1954 MB/s (+3%)** |
| `css_tailwind` parse | 249 MB/s | 235 MB/s | **≥286 MB/s (+15%)** |
| `css_bootstrap` parse | 244 MB/s | 226 MB/s | **≥268 MB/s (+10%)** |
| `css_normalize` parse | 481 MB/s | 472 MB/s | **≥505 MB/s (+5%)** |

15. **Y.0 profile gate.** Per user feedback: after Y.0, the family probe path is absent from the top 20 symbols in samply profiles of `css_tailwind` and `json_canada`. Specifically, `try_emit_family_kernel` and `node_for`/`node_facts::get` do not appear in the top 20 self-time entries.

16. **Kernel consumer count gate — explicit denominator.** The baseline 8 kernels that must have ≥1 production caller regardless of Y.4 outcome: `charclass`, `quoted_string`, `number`, `comment_ws`, `identifier`, `balanced_wrap`, `prefix_class`, `sep_list` (8/8). Family kernels (`function_head`, `hash_prefix`, `unit_tail`, `punct_ws_region`) count toward the gate **only if they survive Y.4**. After Y.4 the denominator is `8 + (number of surviving families)` — any deleted family is removed from both numerator and denominator. Gate text in post-Y.json states the final numerator/denominator explicitly.

17. **Consumer-invariant test.** `crates/core/tests/recognizer_decision_consumption.rs` walks every `AltMode`/`WrapMode`/`RegexEngine` variant via exhaustive match and asserts grep-level or runtime evidence of ≥1 consumer. Runs in CI.

---

## Phases — execution order

Revised ordering per user feedback: parse-time and activation gates close before the de-prioritizable e-graph pair.

### Phase Y.-1 — Freezing guards (safety prelude)

See "Safety prelude" above. Lands first. Nothing else proceeds until the CSP budget, bench timeouts, and nextest config are in place.

### Phase Y.0 — Driver probe elimination

**Files**: `crates/ir/src/types/grammar.rs`, `crates/ir/src/passes/recognizers/mod.rs`, `crates/core/src/backend/driver/node.rs`

Add `GrammarIR::has_family_recognizers: bool`. Set at end of `mine_recognizers` if any `NodeFacts::recognizer` has a family shape. `try_emit_family_kernel` (at `node.rs:20-45`) early-returns when the flag is false.

**Gate check**: Y.0 profile gate — after landing, rerun samply on `css_tailwind` + `json_canada` and confirm `try_emit_family_kernel` is absent from the top 20 self-time symbols.

**Estimated impact**: `json_canada` +4%, `json_citm` +3%, `css_tailwind` +5%, `css_bootstrap` +7%. Recovers most post-X parse regressions.

### Phase Y.1 — `backend/patterns/` full deletion

**Files**: `crates/core/src/backend/patterns/` (deleted); 11 import sites across `backend/rust/`, `backend/ts/`, `backend/wasm/`, `backend/driver/`, `backend/emitter.rs`, `generate/serialize/serialize.rs`.

1. Move `backend/patterns/decisions.rs` → `crates/core/src/backend/types/decisions.rs` (new directory for the multi-file types layer; existing `backend/types.rs` absorbs its siblings or is promoted to `backend/types/mod.rs`).
2. Rewire every `use crate::backend::patterns::key_dispatch::*` → `use bbnf_ir::{KeyClass, KeyDispatchConfig, DetectedBranch, KeyDispatchMatch, key_class_regex_pattern}`.
3. Rewire every `use crate::backend::patterns::decisions` → `use crate::backend::types::decisions`.
4. Delete `crates/core/src/backend/patterns/` entirely (5 files).

**Gate**: `grep -rn "backend::patterns" crates/` returns zero hits.

### Phase Y.2 — SharedHelper load-bearing emission

**Files**: `crates/core/src/backend/rust/emitter/grammar.rs`, `crates/core/src/backend/rust/emitter/alt.rs`, `crates/core/src/backend/rust/emitter/wrap.rs`, `crates/core/src/backend/driver/alt.rs`, `crates/core/src/backend/driver/wrap.rs`, `crates/ir/src/passes/recognizers/mod.rs`, `crates/ir/src/passes/recognizers/prefix_shared_group.rs` (already exists, extend)

Substrate already in place: `Recognizer.peer_group` field exists (`passes/patterns/mod.rs:247`), mining populates groups ≥3 members in `prefix_shared_group.rs` (55 lines, complete), `AltMode::SharedHelper`/`WrapMode::SharedHelper` CSP variants exist, `strategy_hoist_savings` cost weight exists.

Missing: **backend emission.** This is the phase that adds it.

1. `RustEmitter` gains a per-compile `shared_helper_pool: FxHashMap<(Family, GroupId), SharedHelperDef>`.
2. When `compile_alt`/`compile_wrap` sees `AltMode::SharedHelper(group)` / `WrapMode::SharedHelper(group)`:
   - Synthesize signature (e.g. `parse_quoted_string_shared(state) -> Option<Span<'a>>`).
   - Emit body once per `(family, group)` into `emit_grammar`'s top-level definition list.
   - Emit call at the peer site instead of an inline recognizer body.
3. Four canonical families each get a helper in expanded CSS L4: `parse_quoted_string_shared`, `parse_identifier_shared`, `parse_comment_ws_shared`, `parse_balanced_wrap_shared`.
4. JSON beneficiaries: `parse_json_string_shared`, `parse_json_number_shared` for object-key / object-value / array-element positions.

**Verify**: `cargo expand -p bbnf --bench css_l4 | grep -c 'parse_.*_shared'` ≥ 4; `cargo expand -p bbnf --bench json_monolithic | grep -c 'parse_json.*_shared'` ≥ 2.

**Estimated impact**: `css_tailwind` +5–8%, `css_bootstrap` +4–6%.

### Phase Y.3 — `AltMode::TokenDispatch` true activation

**Files**: `crates/ir/src/passes/transform/fuse_token/factor.rs`, `crates/ir/src/passes/transform/fuse_token/detect.rs`, `crates/core/src/backend/strategy/alt_strategy.rs`

Current acceptance criteria in `factor.rs:25-122` are narrower than the CSP's `TokenLedBranches` shape — the CSP flags Alts that `fuse_token_dispatch` refuses, and those fall through to Checkpoint.

**Option A (preferred)**: extend `try_factor_alt` to match every shape the CSP flags. Broaden the ≥4-branch threshold if the CSP marked it, broaden the ≥3-continuation threshold where the CSP confirms disjointness, relax the `strip_leading_keyword` guard where the CSP has already verified the invariant. After Y.3, `alt_strategy.rs:169` `AltMode::TokenDispatch =>` branch is unreachable.

**Option B (fallback)**: add an `AltStrategy::TokenDispatchFromAlt` variant that converts the Alt at codegen time, emitting the same `TokenDispatchArmCompiled<O>` the existing path uses.

Either way, the fallthrough-to-Checkpoint for `AltMode::TokenDispatch` at `alt_strategy.rs` is deleted.

**Estimated impact**: `css_tailwind` +2–4%.

### Phase Y.4 — Family recognizer match-or-delete (staged)

**Files**: `crates/ir/src/passes/recognizers/{function_head,hash_prefix,unit_tail,punct_ws_region}.rs` (relax or delete), `crates/ir/src/passes/recognizers/mod.rs` (counters).

Per user feedback — staged order, deletion is the last step:

**Step 1 — Profile.** Instrument `mine_recognizers` with per-family match counters gated on `BBNF_RECOGNIZER_REPORT=1`. Run against CSS L4 grammars (tailwind, bootstrap, normalize) + JSON grammars (canada, citm, twitter). Commit `docs/profiles/family-match-counts.txt`.

**Step 2 — Relax detection.** For each zero-match family, inspect how the grammar actually expresses the shape and broaden the matcher:
- **FunctionHead**: CSS L4 uses `rgb = "rgb" >> _` as its own rule. Relax to match `Ref(rule_id)` where the rule body is `Seq(Literal, Literal("("), ...)`.
- **HashPrefix**: CSS L4 uses `hash_color = "#" >> hex`. Relax to match the Ref form.
- **UnitTail**: CSS L4 uses `length = number >> unit_literal`. Relax to match `Seq(Ref(number_rule), Ref(unit_rule))` where `number_rule` body is a numeric regex.
- **PunctWsRegion**: JSON uses `"," >> ws` inside sep_by rules. Inspect whether the sep_by rewrite already handles it — relax to match the sep_by-generated shape if relevant.

**Step 3 — Re-measure.** Re-run the counter pass. A family that now matches ≥1 proceeds to kernel verification: cargo-expand confirms the kernel emits and bench delta is non-negative.

**Step 4 — Delete.** Only families that remain at zero matches after relaxation are deleted. Full deletion includes recognizer module, kernel module, enum variant, backend routing, emitter trait method, and the kernel consumer gate denominator.

**Estimated impact**: match-relaxing unlocks the kernel emission path: +2–5% on the target grammar per successful family.

### Phase Y.5 — Cross-rule CSP via connected components

**Files**: `crates/ir/src/passes/csp_strategy.rs`, `crates/ir/src/passes/csp_strategy/components.rs` (new)

X.6 blew up because the union-CSP explored the cross-product of every rule's decision space. Connected-components decomposition is the right shape:

1. Walk every rule body, collecting variables + constraints into a shared store.
2. Union-find over variable ids, unioning pairs connected by `ImplicationConstraint` (or any future cross-variable constraint).
3. Partition variables into components.
4. For each component: fresh `Csp::<StrategyDomain>::new()`, add component's variables + constraints, solve with `MinimizeCost`. **The Y.-1 node budget applies here** — a blown component falls back to per-variable trivial pick for that component without hanging the compile.
5. Merge per-component results into global decision map.

Components with zero cross-variable constraints degenerate to trivial per-variable picks (the current fast-path at `csp_strategy.rs:283-286`). Components spanning rule boundaries via `peer_group` get their own sub-solve — the architectural substrate for Y.2's cross-rule `SharedHelper` hoisting.

**Gate**: `compile_css_l4` regresses ≤5% vs post-X per-rule solve. The Y.-1 budget logs zero blown components on the standard bench grammars.

**Estimated impact**: ~2% on `compile_bbnf` from amortized CSP construction. Primary value is architectural — enables Y.2's cross-rule hoisting cost-weighting.

### Phase Y.6a — Cross-tier cost feedback (`RegexExtractionCost::from_config`)

**Files**: `parse-that/rust/regex/src/egraph/cost.rs`, `crates/ir/src/passes/regex_info.rs`

`RegexExtractionCost::from_config(&CostConfig) -> Self` mirrors `GrammarCostModel::from_config`. `compute_regex_info` constructs its cost from `ir.cost_config` and passes it through `analyze_with_cost_cached`.

**Gate**: `grep -rn "RegexExtractionCost::default\|RegexExtractionCost::new()" crates/ parse-that/` returns hits only in tests.

### Phase Y.6b — Authoritative `RegexEngine` consumption at JSON/CSS emit sites (the X.11a that must not dissolve)

**Files**: `crates/core/src/backend/kernels/quoted_string.rs`, `crates/core/src/backend/kernels/number.rs`, `crates/core/src/generate/regex/emit/scanner_plan.rs`, `crates/ir/src/passes/regex_info.rs`

Per user feedback: cross-tier cost feedback + SIMD number scanning are valuable, but **neither guarantees the architectural requirement that regex-engine decisions are authoritative at emit sites**. Closing Y with better numbers and the same authority gap is unacceptable.

Concrete wiring:

1. `RegexInfo::decisions: FxHashMap<PatternId, RegexEngineKind>` field — populated by the strategy CSP in Y.5 (write path already exists via `extract_regex_engine_decisions`).
2. `kernels::quoted_string::emit_json_call` takes the authoritative engine variant as a parameter, looked up from `ir.regex_info.decisions[pattern_id]` at the call site in `scanner_plan::plan_regex_scanner`. Zero local re-classification.
3. `kernels::number::emit_call_*` same pattern — reads the authoritative engine.
4. `scanner_plan::plan_regex_scanner` primary path consults `ir.regex_info.decisions[pattern_id]` BEFORE calling `classify_regex`. `classify_regex` survives only as the fall-through when no authoritative decision exists (e.g. dynamically-constructed patterns in tests).

**Gate**: `grep -rn "classify_regex" crates/core/src/generate/regex/emit/scanner_plan.rs` shows the call appears only in the fall-through branch. JSON emit sites consume `ir.regex_info.decisions` directly.

**Verify**: `cargo expand -p bbnf --bench json_monolithic` shows that the JSON string/number paths emit code matching the CSP-chosen engine variant (observable via the function name pattern in the expansion).

### Phase Y.10 — TypeDesc clone elimination in AC-3 propagation [**moved earlier per user feedback**]

**Files**: `crates/ir/src/passes/types/constraint/domain.rs`, `crates/ir/src/passes/types/constraint/helpers.rs`, `crates/ir/src/passes/types/constraint/seq.rs`, `crates/ir/src/passes/types/constraint/operators.rs`

Moved ahead of SIMD phases per user feedback: compile-time gate closer, low risk, lower blast radius than the upstream SIMD changes.

Four `self.solved.clone()` sites in `domain.rs`:
- Line 40: `singleton_value()`
- Line 57: `values()`
- Line 69: `join()` first assignment
- Line 75: `join()` second assignment

Change contract: `TypeDomain` keeps `solved: Option<TypeDesc>` owned, but the join primitive accepts `Option<&TypeDesc>` on both sides and clones into `solved` only when the lattice value actually changes. `join_types` in `helpers.rs` and constraint `revise` methods thread references through. Extraction (`project_types` phase 3) takes ownership via one final `.clone()` per solved variable.

**Estimated impact**: `compile_bbnf` ≥5% (closes the −10% gate). `compile_css_l4` ~3% (compounding with Y.0 to close the −40% gate).

### Phase Y.7 — Parse-time SIMD: `scan_ws_block_comments`

**Files**: `parse-that/rust/parse_that/src/parsers/scan/ws_comment.rs`

Inner loop is scalar 5-way compare. Replace with portable-SIMD byte-class skip:

1. 16-byte SIMD chunks via `core::arch::x86_64::*` / `core::arch::aarch64::*` with scalar fallback (`#[cfg(target_arch = "x86_64")]` / `#[cfg(target_arch = "aarch64")]` + `else`).
2. Compare against `b' '`, `b'\t'`, `b'\n'`, `b'\r'`, `b'\x0C'` via vector compare.
3. OR compare results into an "is ws" mask, invert, `trailing_zeros` for first non-ws.
4. Scalar tail for the last <16 bytes.
5. The existing `#[cold]` comment-aware slow path from X.7a is preserved as the fallback when `memchr(b'/', input)` finds a `/`.

**Estimated impact**: `css_tailwind` +8–12%, `css_bootstrap` +5–8%.

### Phase Y.8 — Parse-time SIMD: `scan_number_mantissa`

**Files**: `parse-that/rust/parse_that/src/parsers/scan/number.rs`

30.56% of `json_canada` self-time. Existing 8-byte chunking via `parse_eight_digits` uses multiply-shift arithmetic (simdjson trick) but the digit *detection* is still scalar.

Rewrite detection:

1. 16-byte SIMD chunk load.
2. Subtract `b'0'` (saturating).
3. Compare `<= 9` via unsigned saturating compare.
4. `tzcnt(mask | (1 << 16))` gives leading digit count.
5. Advance by that count; continue if 16; else exit.
6. Preserve `parse_eight_digits` for the mantissa accumulation itself — only the detection loop becomes SIMD.

**Estimated impact**: `json_canada` +10–15%, `json_citm` +5–8%. Closes the `json_canada +5%` gate with headroom.

### Phase Y.9 — Parse-time SIMD: `scan_digits/alnum/hex`

**Files**: `parse-that/rust/parse_that/src/parsers/scan/digits.rs`

Three functions, same scalar-loop shape, different byte-class masks:
- digits: `[b'0', b'9']` range
- alnum: `[b'0', b'9'] | [b'A', b'Z'] | [b'a', b'z']`
- hex: `[b'0', b'9'] | [b'A', b'F'] | [b'a', b'f']`

Range check compiles branchlessly: `(b - lo) <= (hi - lo)` via unsigned saturating subtract + compare. Same SIMD framing as Y.8.

**Estimated impact**: compounding with Y.0's kernel routing: `css_tailwind` +3–5% additional, `json_canada` +2–3% additional.

### Phase Y.11 — E-graph common-suffix factoring rule [**de-prioritizable**]

**Files**: `crates/ir/src/egraph/rules/suffix.rs` (new), `crates/ir/src/egraph/rules/mod.rs` (register), `crates/ir/tests/egraph_suffix.rs` (new)

`Alt([Seq([A, x]), Seq([B, x])]) → Seq([Alt([A, B]), x])` when `x` is structurally identical across branches. Dual of `passes/prefix.rs` prefix factoring. Detect via e-class canonical equality on the shared suffix node.

**Per user feedback**: if Y.0–Y.10 run long, Y.11 moves to Tranche Z.

### Phase Y.12 — E-graph dispatch-unlocking distributive rule [**de-prioritizable**]

**Files**: `crates/ir/src/egraph/rules/distribute.rs` (new), `crates/ir/src/egraph/rules/mod.rs` (register)

Narrow: `Seq([Alt([A, B]), c])` where `A.first_set ∩ B.first_set = ∅` and `c` is a leaf/small node → `Alt([Seq([A, c]), Seq([B, c])])`. Unlocks dispatch tables for patterns like `(keyword | identifier) ws`.

**Per user feedback**: same de-prioritization as Y.11.

### Phase Y.13 — Consumer-invariant enforcement test

**Files**: `crates/core/tests/recognizer_decision_consumption.rs` (new)

1. Walks every `AltMode`, `WrapMode`, `RegexEngine` variant via exhaustive match (compile-time enforced — adding a new variant fails the test until the consumer exists).
2. For each: (a) grep production source for a consumer pattern, OR (b) construct a minimal grammar, compile via the Rust backend, assert expected emission via cargo-expand-style token comparison.
3. Fails if any variant has no observable consumer.

### Phase Y.14 — Residual contingent phases (from X.7c / X.7d)

Land **only if** Y.0 + Y.2 + Y.7 + Y.8 + Y.9 together have not closed the +15% `css_tailwind` gate.

**Y.14a — Packed u32 dispatch for hot CSS literal families** (was X.7c):

Re-profile `css_tailwind` after Y.7–Y.9. Commit `docs/profiles/post-Y9-tailwind.txt`. For keyword families where the profile shows >2% residual (likely: CSS property names, color names, short function-name remainders), collapse `N × [u8]::eq` into `u32::from_le_bytes` + match in `backend/rust/emitter/dispatch.rs`. Family-scoped by reprofile.

**Y.14b — `&[u8]` threading in ParserState** (was X.7d):

Highest-blast-radius parse-time change. Default defer. Cargo-expand evidence is good (376 `state.src[..]` sites, zero `is_char_boundary` calls) but structural surface area is largest of any parse-that change. Only lands if Y.14a + gates still short.

### Phase Y.15 — Import surface audit

**Files**: `crates/core/src/backend/driver/{repeat,seq,wrap}.rs`, `crates/core/src/backend/rust/emitter/*.rs`, `crates/core/src/backend/{ts,wasm}/emitter/*.rs`, `crates/core/src/backend/emitter.rs`

After Y.1 deletion, audit that every backend uses consistent import paths for `KeyDispatchConfig`, `DelimScanConfig`, `DetectedBranch`, and `decisions` helpers. No backend-local re-exports, no duplication.

### Phase Y.16 — post-Y baseline and final verification

**Files**: `docs/benchmarks/post-Y.json` (new), `docs/profiles/post-Y-*.samply.txt` (new)

Full bench sweep + samply profile sweep + cargo expand audit:

- `cargo bench -p bbnf --bench compile_pipeline --bench css_l4 --bench json_monolithic`
- Samply dSYM-symbolicated profile each of `compile_css_l4`, `compile_bbnf`, `compile_json`, `css_tailwind`, `json_canada`.
- `cargo expand` audits for Y.2 helpers, Y.6b engine routing, Y.9 SIMD scanners.
- Consumer-invariant test green.
- `grep -rn "backend::patterns" crates/` zero hits.
- `post-Y.json` has explicit `win_categories` (compile-time / parse-time / activation) + per-phase attribution + explicit kernel consumer count with denominator reflecting Y.4 outcome.

---

## Critical files to modify (quick reference)

**Safety prelude**:
- `/Users/mkbabb/Programming/csc411/CSC411_HW2_ProgrammingQuestion/csp-solver/src/lib.rs` (SolveConfig)
- `/Users/mkbabb/Programming/csc411/CSC411_HW2_ProgrammingQuestion/csp-solver/src/solver/{backtrack,optimize}.rs` (budget counter)
- `/Users/mkbabb/Programming/bbnf-lang/crates/core/benches/common/timeout.rs` (new)
- `/Users/mkbabb/Programming/bbnf-lang/.config/nextest.toml` (new)
- `/Users/mkbabb/Programming/bbnf-lang/Makefile` (nextest invocation)

**Ghost substrate elimination**:
- `crates/ir/src/types/grammar.rs` (`has_family_recognizers`)
- `crates/core/src/backend/driver/node.rs:20-45` (`try_emit_family_kernel` guard)
- `crates/core/src/backend/strategy/alt_strategy.rs:145-196` (fallthrough deletion)
- `crates/core/src/backend/patterns/` (delete entire directory, 5 files)
- `crates/core/src/backend/types/decisions.rs` (new, moved from `patterns/decisions.rs`)

**SharedHelper emission**:
- `crates/core/src/backend/rust/emitter/grammar.rs` (shared helper pool)
- `crates/core/src/backend/rust/emitter/{alt,wrap}.rs` (peer site routing)
- `crates/ir/src/passes/recognizers/prefix_shared_group.rs` (existing, may need threshold tuning)

**Token dispatch activation**:
- `crates/ir/src/passes/transform/fuse_token/{factor,detect}.rs` (broaden acceptance)
- `crates/core/src/backend/strategy/alt_strategy.rs` (delete Checkpoint fallthrough)

**Cross-rule CSP**:
- `crates/ir/src/passes/csp_strategy.rs` (connected-components)
- `crates/ir/src/passes/csp_strategy/components.rs` (new, union-find helper)

**Cross-tier cost / authoritative engine**:
- `parse-that/rust/regex/src/egraph/cost.rs` (`from_config`)
- `crates/ir/src/passes/regex_info.rs` (`decisions` field wiring)
- `crates/core/src/generate/regex/emit/scanner_plan.rs` (authoritative read path)
- `crates/core/src/backend/kernels/{quoted_string,number}.rs` (Y.6b emit site consumption)

**AC-3 clones**:
- `crates/ir/src/passes/types/constraint/{domain,helpers,seq,operators}.rs`

**Parse-time SIMD** (parse-that):
- `parse-that/rust/parse_that/src/parsers/scan/ws_comment.rs`
- `parse-that/rust/parse_that/src/parsers/scan/number.rs`
- `parse-that/rust/parse_that/src/parsers/scan/digits.rs`

**E-graph rules** (de-prioritizable):
- `crates/ir/src/egraph/rules/suffix.rs` (new, Y.11)
- `crates/ir/src/egraph/rules/distribute.rs` (new, Y.12)
- `crates/ir/src/egraph/rules/mod.rs` (registration)

**Tests**:
- `crates/core/tests/recognizer_decision_consumption.rs` (new, Y.13)
- `crates/ir/tests/egraph_suffix.rs` (new, Y.11)

---

## Existing functions and utilities to reuse

- `crates/core/src/backend/recognizer_plan.rs::scanner_plan_for()` (X.8f) — single-view lookup over `recognizer_decisions + node_facts + sidecars`. Y.6b uses it for the authoritative engine read.
- `crates/ir/src/passes/recognizers/prefix_shared_group.rs::mine()` — already groups by `signature.shape_hash` with `HOIST_THRESHOLD = 3`. Y.2 extends the emitter to consume its output.
- `crates/egraph/src/cost_config.rs::CostConfig::from_env()` — shared between grammar-tier and regex-tier cost models. Y.6a taps the same config struct.
- `csp_solver::constraint::ImplicationConstraint` — already wired for `TokenDispatch → RegexEngine` parent-child compatibility. Y.5 reuses as the edge type for connected components.
- `bbnf_ir::{KeyClass, KeyDispatchConfig, DetectedBranch, KeyDispatchMatch, key_class_regex_pattern}` — authoritative types, imported directly after Y.1 replaces the `backend/patterns/` re-exports.
- `parse_that::{scan_digits_mut, scan_alnum_mut, scan_hex_mut}` — the targets of Y.9's SIMD rewrite; already called from `kernels::charclass`.
- `memchr::memchr*` — already used in `scan_ws_block_comments`'s `#[cold]` slow path; Y.7 keeps it as the `/` detection before entering the comment state machine.

---

## Verification

```bash
# Tests (with nextest for timeouts)
cargo nextest run --workspace --exclude bbnf-lsp --exclude bbnf-analysis
cd /Users/mkbabb/Programming/parse-that/rust/regex && cargo test
cd /Users/mkbabb/Programming/bbnf-lang

# Bootstrap
bash scripts/bootstrap-bbnf.sh
md5 -q crates/core/src/grammar/generated.rs  # record post-Y hash

# Bench sweep (with Y.-1 wall-clock guards)
cargo bench -p bbnf --bench compile_pipeline --bench css_l4 --bench json_monolithic 2>&1 | tee /tmp/post-Y-benches.txt

# Architectural grep assertions
grep -rn "backend::patterns\|crate::backend::patterns\|super::patterns" crates/     # empty
grep -rn "RegexExtractionCost::default\b" crates/ parse-that/                       # test-only
grep -rn "classify_regex" crates/core/src/generate/regex/emit/scanner_plan.rs       # only in fall-through branch
grep -rn "try_emit_family_kernel" crates/core/src/backend/driver/node.rs            # early-returns on has_family_recognizers
grep -c "\.clone()" crates/ir/src/passes/types/constraint/domain.rs                 # ≤1
grep -rn "AltMode::SharedHelper\|WrapMode::SharedHelper" crates/core/src/backend/   # consumed, not fallthrough
grep -rn "peer_group:" crates/                                                      # populated and read
grep -rn "node_budget" /Users/mkbabb/Programming/csc411/CSC411_HW2_ProgrammingQuestion/csp-solver/src/  # non-None default

# Cargo expand re-measure (Y.2 + Y.6b)
cargo expand -p bbnf --bench css_l4 > /tmp/expand_css_postY.rs 2>&1
cargo expand -p bbnf --bench json_monolithic > /tmp/expand_json_postY.rs 2>&1
grep -c 'parse_quoted_string_shared\|parse_identifier_shared\|parse_comment_ws_shared\|parse_balanced_wrap_shared' /tmp/expand_css_postY.rs  # ≥4
grep -c 'parse_json_string_shared\|parse_json_number_shared' /tmp/expand_json_postY.rs                                                       # ≥2
grep -c 'is_ascii_digit\|is_ascii_whitespace\|is_ascii_alphanumeric' /tmp/expand_css_postY.rs                                                # 0 outside cold paths

# Consumer-invariant test
cargo nextest run -p bbnf --test recognizer_decision_consumption

# Profile-confirmed cliffs (samply, dSYM-symbolicated)
cargo bench -p bbnf --bench compile_pipeline --bench css_l4 --bench json_monolithic --no-run
COMP_BIN=$(find target/release/deps -maxdepth 1 -type f -perm -111 -name 'compile_pipeline-*' ! -name '*.d' ! -name '*.dSYM' -exec ls -t {} + | head -1)
CSS_BIN=$(find target/release/deps -maxdepth 1 -type f -perm -111 -name 'css_l4-*' ! -name '*.d' ! -name '*.dSYM' -exec ls -t {} + | head -1)
JSON_BIN=$(find target/release/deps -maxdepth 1 -type f -perm -111 -name 'json_monolithic-*' ! -name '*.d' ! -name '*.dSYM' -exec ls -t {} + | head -1)
xcrun dwarfdump --uuid "$CSS_BIN" && xcrun dwarfdump --uuid "$CSS_BIN.dSYM"  # UUIDs match

samply record --save-only --unstable-presymbolicate -o /tmp/postY_compile_pipeline.samply -- "$COMP_BIN" --bench
(cd crates/core && samply record --save-only --unstable-presymbolicate -o /tmp/postY_css_tailwind.samply -- "$CSS_BIN" --bench tailwind)
(cd crates/core && samply record --save-only --unstable-presymbolicate -o /tmp/postY_json_canada.samply -- "$JSON_BIN" --bench canada)

# Profile assertions:
# Y.0: try_emit_family_kernel absent from top 20 symbols in css_tailwind, json_canada
# Y.10: TypeDesc::clone ≤0.5% in compile_css_l4 (was 2.4% post-X)
# Y.7: scan_ws_block_comments ≤3% in css_tailwind (was 12%)
# Y.8: scan_number_mantissa ≤10% in json_canada (was 30%)
# SIMD: vpcmpeqb / tzcnt present in disassembly of scan_number_mantissa
```

If any assertion fails, the tranche does not close.

---

## Non-goals (Tranche Z)

- **Full DAG-based cross-rule hoisting beyond signature-based slicing.** Y.2 hoists by `signature.shape_hash` via `prefix_shared_group.rs`. True sub-tree hash-cons analysis across the DAG is Z.
- **Broad new e-graph rule programs.** Y adds exactly two (common-suffix, distributive) and both are de-prioritizable. Broader algebraic rewrites are Z.
- **Rewriting bbnf-regex HIR engine for SIMD matching.** Y adds SIMD to three scan helpers. HIR-driven match loops stay scalar.
- **Upstreaming SIMD to `core::simd`.** Y uses `core::arch::x86_64::*` + `core::arch::aarch64::*`. Portable std::simd migration is Z.
- **Global CSP with cross-component coupling.** Y.5 gives per-component optimality; true global joint objective is Z.
- **Cost-weight upgrade for non-alt strategy solvers.** `seq_strategy` / `repeat_strategy` / `ref_strategy` / `wrap_strategy` have small decision spaces; cost-weight tuning is speculative until a profile shows the CSP search is the bottleneck for any of them.
- **Upgrading dispatch-eligibility / type-projection / egraph-scheduler CSPs to `MinimizeCost`.** Each models a real CSP but the domain has no meaningful cost function today.
- **No legacy retention.** `backend/patterns/` deleted; probe elided or families deleted; fallthrough branches deleted. "Reserved for future use" substrate is by definition ghost substrate.

---

## Tranche length

Per user feedback: gates are ambitious relative to post-X miss. The tranche may stay open until it is truly done, and that is the intended behavior. No half-states across tranche boundaries. Y.14 contingent items and Y.11/Y.12 de-prioritization exist precisely so the critical-path phases (Y.-1 through Y.10) can close even if schedule pressure arrives.

---

## Reference: Tranche X inheritance (landed at `8274e15` + partial out-of-order commits)

**Landed and load-bearing**:
- X.0: `find_node_id_for_var` O(1) reverse map
- X.1: charclass kernel routing
- X.2: FxHash for egraph per-iteration scratch
- X.3: HIR e-graph per-compile `SaturationCache`
- X.4: `format!()` audit in `ir_enums.rs` + `types/mod.rs`
- X.5: TypeDesc clone elision in `driver/{repeat,seq,wrap}.rs`
- X.7a: `scan_ws_block_comments` zero-ws fast-return + `#[cold]` path
- X.7b: byte-array pointer-cast literal compares
- X.8a/b: upstream `DelimScan`/`KeyDispatch` detection in `bbnf-ir::passes::recognizers::*`
- X.8c: `WrapMode` gate at `wrap.rs` before emit_delim_scan
- X.8d: `extract_regex_engine_decisions` in `csp_strategy.rs` (partial — populated but not consumed at emit sites)
- X.8e: `AltMode::TokenDispatch` elevation path (partial — falls through in hard case)
- X.8f: `ScannerPlanRecord` unification bridge at `backend/recognizer_plan.rs`
- X.9a: `prefix_class` kernel end-to-end (planner + emitter + body)
- X.9b: `sep_list` Span-case kernel routing
- X.10a/b/c: CSS family recognizer + kernel pairs — **zero production matches** (Y.4 target)
- X.11b: JSON PunctWsRegion recognizer + kernel — **zero production matches** (Y.4 target)
- X.12a: FxHash FIRST/FOLLOW per-pass scratch

**Attempted and reverted**:
- X.6: global CSP batching (9ms → 94ms CSS L4 blowup) — replaced by Y.5 connected-components

**Deferred / not load-bearing, addressed in Y**:
- X.8h: `backend/patterns/` deletion → **Y.1**
- X.9c: signature-based SharedHelper activation → **Y.2** (substrate exists, backend emission missing)
- X.9d: four-family helper slice → **Y.2**
- X.11a: JSON authoritative `RegexEngine` end-to-end → **Y.6b** (explicit per user feedback, not dissolved into Y.6a/Y.8)
- X.11c: JSON shared helper hoisting → **Y.2**
- X.11d: surgical `scan_number_mantissa` → **Y.8** (now SIMD detection loop)
- X.12b: consumer-invariant verification sweep → **Y.13**
- Cross-tier cost feedback → **Y.6a**
- TypeDesc clone elimination in AC-3 → **Y.10** (moved earlier per user feedback)
- Parse-time SIMD → **Y.7, Y.8, Y.9**
- Driver probe elimination → **Y.0** (post-X regression culprit)
- E-graph rule expansion (common-suffix, distributive) → **Y.11, Y.12** (de-prioritizable)
- Bench/test/CSP timeouts → **Y.-1** (new, from freezing-session feedback)

