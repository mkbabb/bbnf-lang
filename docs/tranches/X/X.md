# Tranche X — Activation Truth, Type-Pass Demolition, and the Cost-Sharing Gestalt

## Original edict (carried forward)

> Devise a path forward: audit the hitherto made changes and the remaining plan. **NO quick solutions, NO workarounds: idiomatic, gestalt approaches.** This is a development product. Architectural transpositions in the sake of elegance, simplicity, and performance above all are both necessary and desirable. **NO legacy code.**
>
> Are all of our optimizations (CSP, egraph, structural pattern matching) properly wired in and activating? What of the shared cost analysis structure between regex and the graph? Where are we duplicating effort, cloning, and not taking advantage of rich structure analysis afforded by the IR and DAG? What's our current CSP optimizing for? We should be optimizing both locally and globally and do so in full generality.

This plan is the audit-driven response to that edict, **post-Tranche W**. It reads from a three-agent audit: a structural code audit (43 findings), a samply profile of all five benches symbolicated against the dSYM bundles, and a cargo-expand audit of the JSON / CSS L4 / compile-pipeline expansions. Every claim is grounded in measured profile data, line-counted expansion output, or grep-confirmed source.

The prior plan in this file (Tranche W — Activation, Hot-Path Demolition, and Three Distinct Cliffs) has shipped (post-W baseline at `docs/benchmarks/post-W.json`). It built the substrate AND the activation layer on top of Tranche V. The post-W audit reveals that the activation lands the gestalt commitments — `csp_strategy.rs` uses `csp_solver::Csp::solve_optimized(MinimizeCost)`, six of eight `backend/kernels/` family modules have real production callers, the per-compile `CostConfig` substrate threads through every consumer — but **two new cliffs emerged** that the previous profile couldn't see (because they were hidden behind the `factor_literal_prefixes` 91.73% cliff that Tranche W demolished), AND **one Tranche W activation is silently bypassed**:

- `bbnf_ir::passes::types::project_types::find_node_id_for_var` is a linear `iter().chain().find_map()` scan over `system.node_vars` + `system.vec_context_vars` called inside a Seq-children loop. **23.13% self-time on `compile_css_l4`**, dominating the new ~10 ms total. Now that the prefix factoring cliff is gone, the type pass is the 51.03% module-share giant.
- `bbnf_regex::egraph::saturate_hir_egraph` re-saturates the same 6-10 JSON patterns on every compile of the same grammar. The Tranche W phase 2 `needs_saturation` skip works for trivial HIRs but the JSON string and number patterns still pay the full saturate-extract-drop cycle every time. **9.17% inclusive on `compile_json`** = ~10 µs of the 5 µs gap.
- The 86 `is_ascii_digit()` inline while-loops in CSS L4 generated parser code that the cargo-expand audit flagged in pre-Tranche-W are **STILL there**, unchanged. Tranche W's `kernels::charclass::emit_call_opt` is wired into `scanner_plan::plan_regex_scanner` for `RegexClass::CharClassQuantified`, but CSS L4's numeric patterns (`[0-9]+(\.[0-9]+)?(e[+-]?[0-9]+)?`) classify as `RegexClass::Numeric` at the top level. The generalized emitter then handles each sub-class (`[0-9]+`) **inline** via `__b.is_ascii_digit()` predicate loops without consulting `scanner_plan`. **Three emit sites bypass the kernel: `generalized/mod.rs:238`, `generalized/class_segments.rs:184`, `hir/leaf.rs:135`.** The substrate is correct; the routing has a hole.

Tranche X is the demolition of these new cliffs, the closure of the kernel-routing hole, and the next architectural transpositions that turn Tranche W's activation substrate into a data path where IR decisions are authoritative in emission.

**Scope**: Tranche X is the **full activation tranche** for the Tranche W substrate, plus the compile-cliff demolition surfaced by post-W profiling. In one sentence: X delivers full AOT activation of `WrapMode`, `RegexEngine`, and `AltMode::TokenDispatch`; a narrow signature-based slice of `SharedHelper` for the four highest-duplication families (quoted_string, identifier, comment_ws, balanced_wrap); end-to-end completion of the `prefix_class` and `sep_list` kernels; expansion of the CSS recognizer family set (function-heads, hash-prefix tails, unit-tail families); and one real JSON parse-side improvement (authoritative `RegexEngine` end-to-end + JSON structural punctuation+ws recognition + signature-based JSON helper hoisting). The legacy `backend/patterns/` directory is replaced upstream and then deleted within the tranche. The compile cliffs surfaced by post-W profiling (type-pass linear scan, HIR re-saturation per compile, `format!()` on hot paths, TypeDesc clone chains, egraph SipHasher cost) are demolished alongside.

**Tranche Y** retains the deepest items: full DAG-based cross-rule sub-expression hoisting beyond signature-based slicing, common-suffix factoring, type-driven dispatch fusion, broad new e-graph rule programs (the V-era ten rules), and a real global cross-rule cost objective with joint optimization. Phase 6 in X is CSP *batching*; Y is true global CSP. The substrate that X activates becomes the platform Y builds on. Narrow new e-graph rules are allowed in X **only** when they expose an already-planned X dispatch/scanner form (per §3 rule 23) — broad rule programs remain Y.

---

## 1. The new top-of-profile cliffs (post-Tranche W)

The samply profile of `compile_css_l4` (10.21 ms total) shows how dramatically Tranche W reshaped the cost surface. Before W, `factor_literal_prefixes` was 91.73% inclusive. After W, it's 5.84% inclusive. The space it vacated is now occupied by:

| Bench | Time/iter | New top cost | % | Source |
|---|---:|---|---:|---|
| `compile_css_l4` | 10.21 ms | `passes::types::project_types::find_node_id_for_var` linear scan inside Seq loops | **23.13% self / 51.03% module-share** | `crates/ir/src/passes/types/mod.rs:336-345` |
| `compile_bbnf` | 987 µs | egraph `Id`/`String`/`u32` hashing via `RandomState` SipHasher + `TypeDesc::clone` in driver `or_else` chains | **~10% combined hashing + 7.42% TypeDesc** | `crates/egraph/src/egraph.rs` (default hasher); `crates/core/src/backend/driver/{repeat,seq,wrap}.rs` |
| `compile_json` | 108 µs | `bbnf_regex::egraph::simplify_hir` re-saturating identical patterns + `alloc::fmt::format_inner` 4.32% self | **9.17% inclusive (HIR) + 4.32% format** | `parse-that/rust/regex/src/egraph/mod.rs:117`; suspicious `format!()` in hot pipeline paths |
| `json_canada` parse | 1.93 ms | `scan_number_mantissa` 30.56% — upstream parse_that, slice `Iterator::all` 8.20% | dominant | `parse-that/rust/parse_that/src/parsers/scan/number.rs::scan_number_mantissa` |
| `css_tailwind` parse | 15.23 ms | `scan_ws_block_comments` 12.03% on a tailwind input that has zero comments + `equal_same_length` 9.27% literal memcmp | dominant | `parse-that/rust/parse_that/src/parsers/scan/ws_comment.rs`; CSS dispatch fallthrough |

These are not the Tranche W cliffs. The Tranche W cliffs (`factor_literal_prefixes` at 91.73%, `drop_in_place<EGraph<GrammarENode>>` at 25.52%, `simplify_hir` at 18.25%) are gone or reduced to the low single digits. The new top costs are surfaced cleanly because the noise floor dropped.

The cargo-expand audit confirms two more structural facts:

- **Zero `alloc_slice_clone` calls** in either expanded grammar — Tranche W phase 5b's Copy + memcpy fast path took effect uniformly.
- **Zero structural dispatch-table duplicates** — the previous "37 dispatch tables" count was a misread; dispatch is in `match … as_bytes()` ladder form, not literal `[u8; 128]` arrays, and there are no isomorphic ladders to dedupe.

But also:

- **86 `is_ascii_digit()` inline while-loops in CSS L4 generated parser** — unchanged from pre-Tranche-W. The Tranche W kernel routing exists but isn't consulted on the path that emits them.
- **512 `starts_with("…")` UTF-8 string literal compares in CSS L4 generated parser** — these are CSS keyword dispatch (color names, property names). Each one is a UTF-8 length check + memcmp. A perfect-hash u32 dispatch (the deferred Tranche W phase 5c) would collapse them.
- **376 `state.src[..]` UTF-8 slice sites in CSS L4** — Phase 5a (the deferred `&[u8]` threading) blast radius. **Zero `is_char_boundary` calls in the generated parser** — meaning the LLVM optimizer already elides the bounds checks on every call site, so the blast radius is purely **borrow-form migration**, not a behavioral change. Less invasive than the Tranche W plan estimated.
- **955 `(|| { … })()` Option-rollback IIFEs in CSS L4** — alt-checkpoint and optional-commit emission patterns. LLVM inlines them; the cost is purely cosmetic (~6k lines of expanded output) but it suggests a cleaner alternative emission shape.

---

## 2. What's still theatre vs what activated post-Tranche W

The structural code audit confirms Tranche W's commitments held:

| Layer | Pre-W state | Post-W state |
|---|---|---|
| Strategy CSP | `csp_recognizers.rs`: zero `csp_solver::` references, pattern-walk only | `csp_strategy.rs`: 7 `csp_solver::Csp` references, `OptimizationMode::MinimizeCost` invoked at line 294, fast-path elides solve when no `ImplicationConstraint` fires |
| Cost weights | Both tiers called `::default()` independently | Both tiers read from `ir.cost_config`; `GrammarCostModel::from_config(&ir.cost_config)` at `pipeline/compile.rs:370`, `RegexExtractionCost { weights: ir.cost_config.egraph.weights, hir_*: ir.cost_config.hir_* }` at `regex_info.rs:35` |
| Backend kernels | 8 placeholder `compile_error!` stubs, zero consumers | 6/8 modules wired through real production callers (`scanner_plan.rs::SharedScanner::into_tokens` + `dispatch.rs::emit_delim_scan_impl`) |
| Strategy solvers | All five (alt/seq/repeat/ref/wrap) priority cascades | `alt_strategy.rs` reads from `ir.recognizer_decisions`; the other four still standalone |
| Hot-path clones | 7 audit-flagged clones (DAG, strings ×2, ContextFacts, CharSet128 ×3) | All eliminated; `mine_recognizers` borrows `&ir.dag` via NLL; CharSet128 became `Copy + Hash` |

What's still theatre or incomplete after Tranche W:

1. **`backend/patterns/{delim_scan,key_dispatch,cache,mod}.rs` (4 files, 251+334+~150+~30 lines) is still in the workspace.** The Tranche W plan §8 called for deletion. The deletion was deferred because `alt_strategy.rs::decide_alt_strategy` still falls back to `patterns::key_dispatch::try_detect` when `ir.recognizer_decisions` doesn't cover an Alt, and `dispatch.rs::emit_delim_scan_impl` consumes `DelimScanConfig` from a `dstate.delim_scan_configs` map populated by `patterns::cache::solve_delim_scan_configs`. Both consumers are real today; deletion requires migrating their inputs upstream into the recognizer mining pass + the strategy CSP.

2. **`prefix_class.rs` is a 17-line stub** that hardcodes `quote! { ::parse_that::scan_ident(state) }` regardless of the prefix bytes — wrong for any non-identifier prefix. Zero production callers. **`sep_list.rs` is a 23-line lookup-stub** that emits a single `memchr(separator_byte, ...)` call; the full sep-by element loop still lives in `backend/driver/repeat.rs::emit_sep_by`.

3. **`seq_strategy.rs`, `repeat_strategy.rs`, `ref_strategy.rs`, `wrap_strategy.rs` are still standalone classifiers**, not consumers of `ir.recognizer_decisions`. Their decision spaces are smaller than Alt's (e.g., Repeat is just `Optional` vs `Many` based on `lo`/`hi` bounds), so the cost-weight upgrade is lower priority. But the gestalt asymmetry — one solver reads the CSP, four don't — is a smell.

4. **The CSP layer beyond the strategy CSP is still feasibility-only.** Three other CSPs in the workspace use `propagate()` without cost minimization:
   - `passes/sets/dispatch/eligibility.rs:67` — dispatch eligibility (boolean lattice)
   - `passes/types/mod.rs:61` — type projection (the type lattice)
   - `egraph/csp_scheduler.rs::run` — dirty propagation
   
   Each one solves a real CSP but does NOT minimize a cost function. The plan's gestalt rule ("optimizing both locally and globally and do so in full generality") points at these as upgrade candidates. Each one models a domain where a cost weight COULD distinguish between feasible solutions if we had a good cost function.

5. **Cross-tier cost coordination is non-existent.** `compute_regex_info` builds `RegexExtractionCost` from `ir.cost_config` and runs HIR extraction independently. `pipeline/compile.rs:370` builds `GrammarCostModel::from_config(&ir.cost_config)` and runs grammar-tier extraction independently. **There is no feedback loop:** the grammar-tier doesn't see which HIR-tier engine the strategy CSP picked, and the HIR-tier doesn't see whether the regex pattern is going to be used inside a `TokenDispatch` parent (which would constrain it to one-pass-eligible variants). The Tranche W `ImplicationConstraint` enforces compatibility post-hoc; it doesn't optimize jointly.

6. **No cross-rule sub-expression sharing analysis.** The durable DAG (`ir.dag`) has a `HashMap<*const IrNode, NodeId>` reverse map and hash-cons identity for every distinct sub-expression. **No analysis walks this to find rules that share sub-trees.** The infrastructure exists (`Recognizer.peer_group` field, `csp_strategy::AltMode::SharedHelper(group)`, the cost weight `strategy_hoist_savings`) but the analysis that would set `peer_group` and the emission that would hoist a shared helper are both absent from production. The `prefix_shared_group::mine` pass exists but only groups by recognizer signature, not by structural sub-tree identity.

7. **`compute_context_facts` is computed once per compile in `mine_recognizers` and discarded.** The `ContextFactsMap` is local to one pass; downstream passes (`compute_sp_method_rules`, `project_types`, the strategy CSP itself) could read from it but don't. The cost is small (~500 µs per CSS L4 compile) but the structure-sharing is missed.

---

## 3. Architectural commitments (Tranche X)

These rules govern what lands and what does not. They are not aspirational; they are the gates.

1. **`find_node_id_for_var` is O(1).** The linear scan at `passes/types/mod.rs:336-345` is replaced with a `HashMap<VarId, NodeId>` reverse map built once at Phase 3 entry and cached on the type-projection state struct. Profile re-measure: `project_types` self-time on `compile_css_l4` drops from 23.13% to <5%.

2. **The egraph hashtables use `FxHashMap`, not `RandomState`.** The grammar-tier `EGraph<GrammarENode>` and HIR-tier `EGraph<HirENode>` both use the default `std::collections::HashMap` with `RandomState`. Profile shows ~10% combined SipHasher cost on `compile_bbnf`. `crates/egraph/src/egraph.rs::EGraph.memo` is already `FxHashMap` per the imports — but the `eclass` parent edges and `unionfind` are using `Vec`s with no hashing. The 10% must come from rule-application HashMap usage. **Audit and switch every per-compile HashMap in the egraph crate to `FxHashMap`.** Profile re-measure: hashing self-time on `compile_bbnf` drops from ~10% combined to <2%.

3. **The 86 `is_ascii_digit()` inline loops in CSS L4 generated parser are zero.** The three emit sites that bypass `kernels::charclass::emit_call_opt` (`generalized/mod.rs:238`, `generalized/class_segments.rs:184`, `hir/leaf.rs:135`) call the kernel as a short-circuit at the top of each emit path. The kernel returns a real `scan_digits_mut` / `scan_alnum_mut` / `scan_hex_mut` invocation. Cargo-expand re-measure: `is_ascii_digit` count in `/tmp/expand_css.rs` drops from 86 to <10 (residue is the bounded `[0-9]{1,3}` shapes the kernel doesn't yet handle).

4. **The HIR e-graph has a per-compile saturation cache.** `simplify_hir` hashes the input HIR (the parsed regex pattern's AST is already `Eq + Hash` after Tranche W's Hash derive on `CharSet128`) and caches the canonical output keyed on the input hash. JSON's 4 patterns saturate once per compile, not per-call. Profile re-measure: `simplify_hir` inclusive on `compile_json` drops from 9.17% to <2%.

5. **The format!() audit is complete.** Every `format!()` call in `crates/ir/src/passes/`, `crates/core/src/pipeline/`, and `crates/core/src/backend/driver/` is either deleted (if it's a debug/error message that fires once) or replaced with a `&'static str` (if the format string has no arguments) or hoisted to a `#[cold]` error path. Profile re-measure: `alloc::fmt::format_inner` self-time on `compile_json` drops from 4.32% to <0.5%.

6. **The TypeDesc clone chain in driver is reference-borrowed.** The `ir.vec_elem_type(n).cloned().or_else(|| ir.node_type(n).cloned())` pattern at `backend/driver/{repeat,seq,wrap}.rs` becomes `ir.vec_elem_type(n).or_else(|| ir.node_type(n))` returning `Option<&TypeDesc>`. Callers that need `TypeDesc` by value clone explicitly only when storing. Profile re-measure: `TypeDesc::clone` self-time on `compile_bbnf` drops from 2.41% to <0.5%.

7. **`scan_ws_block_comments` has a no-comments fast path.** The Tailwind input has zero comments but pays 12.03% of parse time on a state machine that's checking for `/*`. The fast path is `memchr3(b' ', b'\t', b'\n')` until the loop exits or `input[pos] == b'/'`, then enter the existing state machine. Lives in `parse-that/rust/parse_that/src/parsers/scan/ws_comment.rs`. Profile re-measure: `scan_ws_block_comments` self-time on `css_tailwind` drops from 12.03% to <4%.

8. **The CSS keyword ladder uses byte-literal comparisons.** The 512 `starts_with("…")` UTF-8 calls become `starts_with(b"…")` byte-literal calls. `backend/rust/emitter/leaves.rs::emit_literal_match_impl` emits `state.src.as_bytes()[state.offset..].starts_with(#byte_literal)` instead of `state.src[state.offset..].starts_with(#str_literal)`. This is a one-line emitter change that touches 512 generated sites in CSS L4 alone. Profile re-measure: `equal_same_length` self-time on `css_tailwind` drops from 9.27% to <3%.

9. **`backend/patterns/{delim_scan,key_dispatch,cache,mod}.rs` is deleted.** The detection halves move into the recognizer mining pass (or directly into `csp_strategy::build_alt_domain` / `build_wrap_domain`). The configs (`DelimScanConfig`, `KeyDispatchConfig`) move into `backend/types.rs` (or stay; they're already there per the CLAUDE.md). The `dstate.delim_scan_configs` and `dstate.key_dispatch_configs` lookup maps are built from `ir.recognizer_decisions` directly. The `alt_strategy::decide_alt_strategy` structural fallback is deleted. The legacy patterns directory is gone.

10. **The strategy CSP is global, not per-rule.** A single `Csp::<StrategyDomain>::new()` is allocated once per compile, populated with variables from every rule body (per-rule sub-passes accumulate into the same CSP), then `solve_optimized()` runs once over the union. The per-rule allocation overhead that's currently ~2 µs × 4 rules = 8 µs on JSON drops to ~2 µs total. The `compile_json` -10% gate that Tranche W missed at -4.8% is met.

11. **Tranche X is one commit series.** No co-existence of legacy and new at any commit boundary. Deletions ship with the additions that replace them.

12. **Bench gates are non-regression vs post-W on every row.** Specific improvement targets:
    - `compile_pipeline::compile_css_l4`: ≥40% improvement vs post-W (10.2 ms → ≤6 ms; `find_node_id_for_var` fix predicts ~6 ms)
    - `compile_pipeline::compile_bbnf`: ≥10% improvement vs post-W (987 µs → ≤888 µs; FxHash + TypeDesc fixes predict ~860 µs)
    - `compile_pipeline::compile_json`: ≥10% improvement vs post-W AND meets the original Tranche W -10%-vs-post-V gate (108 µs → ≤97 µs vs post-W, ≤93 µs vs post-V; HIR cache + format! fix + global CSP)
    - `json_monolithic::canada` parse: **≥+5% improvement vs post-W** (1188 → ≥1247 MB/s) via authoritative `RegexEngine` end-to-end (11a) + structural punctuation+ws recognition (11b) + signature-based JSON helper hoisting (11c); optional surgical mantissa SWAR (11d) if 11a-c fall short
    - `json_monolithic::citm` parse: ≥+3% improvement vs post-W (1897 → ≥1954 MB/s) — secondary JSON gate, same activation phases
    - `css_l4::tailwind` parse: ≥+15% improvement vs post-W (249 → ≥286 MB/s; cumulative from 7a + 7b + 7c-for-hot-CSS-families + Phase 10 family expansion, with 7d still contingent)
    - `css_l4::bootstrap` parse: ≥+10% improvement vs post-W (244 → ≥268 MB/s) — secondary CSS gate, same Phase 10 families

13. **Regex engine decisions are authoritative in AOT emission.** `scanner_plan::plan_regex_scanner` currently classifies via `opts.classify_regex(pattern)` at every emit site, independent of whatever the IR CSP already decided. Tranche X makes emission read from the authoritative regex-engine decision in `ir.regex_info` (introducing the decision field if not yet present). `classify_regex` survives only as the fall-through path when the authoritative decision is absent; the primary path consults IR. Grep invariant: `plan_regex_scanner`'s primary path has zero direct calls to `classify_regex`.

14. **WrapMode decisions are authoritative in AOT emission.** The same principle for wrap emission. Backend delim-scan, balanced-wrap, and paired-delim emit paths must read from `ir.recognizer_decisions` for the wrap node, not independently rediscover the shape via `patterns::cache::solve_delim_scan_configs`. This is the data-path prerequisite for §8a's legacy deletion — the backend cannot stop consuming `patterns::cache::*` until there is an authoritative replacement in `ir.recognizer_decisions`.

15. **Every `RecognizerDecision` variant has ≥1 production consumer.** A new test (`crates/core/tests/recognizer_decision_consumption.rs`, extending the existing consumer-invariant harness) walks `ir.recognizer_decisions` by variant and asserts that every `AltMode`, `WrapMode`, `SeqMode`, and regex-engine variant reaches at least one emission site under AOT or VM. Any variant with zero consumers is either activated in this tranche or explicitly renamed into a `Reserved::*` holding area and removed from production decision emission. This is the activation theme made testable, and the companion gate to rules 13, 14, and 17.

16. **Charclass kernel routing is structural, not per-site.** Phase 1 targets three profiled emit sites (`generalized/mod.rs:238`, `generalized/class_segments.rs:184`, `hir/leaf.rs:135`), but the gate is structural: **every** leaf/charclass emission path in `crates/core/src/generate/regex/emit/` consults `kernels::charclass::emit_call_opt` as a short-circuit at the top. Grep invariant: no leaf/charclass emitter contains a `while … is_ascii_digit()` / `is_ascii_hexdigit()` / `is_ascii_alphanumeric()` loop without a preceding kernel check. Fixing CSS L4's hole without this structural gate would leave sibling holes elsewhere.

17. **`AltMode::TokenDispatch` is either activated in this tranche or explicitly deferred by name.** If §8e lands, `dispatch.rs::emit_token_dispatch` reads `ir.recognizer_decisions[alt_id]::AltMode::TokenDispatch(table)` and has zero calls to `patterns::key_dispatch::try_detect`. If §8e is held (blocked by §8a Step 1), §10 non-goals explicitly names it as Tranche Y. Never silently in the substrate.

18. **The `backend/patterns/` deletion is sequenced, not atomic.** §8a is a three-step commit sequence: (1) upstream replacement lands in recognizer mining and populates `ir.recognizer_decisions`; (2) backend lookup maps rebuild from `ir.recognizer_decisions`; (3) legacy files deleted. No single commit has the deletion landing before the upstream producer. The intermediate state (legacy present but unused) passes every gate.

19. **Limited `SharedHelper` activation by recognizer signature lands in X.** A narrow slice of cross-rule helper hoisting lands, gated by recognizer signature equality (NOT full DAG-based structural sharing — that is Tranche Y). At least one production hoisting site exists in the generated parser for each of: `quoted_string`, `identifier`, `comment_ws`, `balanced_wrap`. The hoisting reads from `Recognizer.peer_group`, populated via signature canonicalization in recognizer mining. This rule is the precondition for Y; X earns the right to claim `SharedHelper` is "activated" without overreaching into full DAG-based hoisting.

20. **`prefix_class` and `sep_list` are activated end-to-end, not stubbed.** Three edits per kernel: kernel body, planner routing, emitter / driver routing. After Tranche X, `RegexClass::PrefixThenClass` and `RegexClass::AccelDriven` (where applicable) flow through `kernels::prefix_class::emit_call_opt`, and the sep-by element loop flows through `kernels::sep_list::emit_call`. Both with zero stub bodies and ≥1 production caller. The kernel consumer-count gate moves from 6/8 → 8/8 (and to 11/11 after Phase 10's family kernels).

21. **Scanner-planning unification bridge.** A small backend-facing record (`crates/core/src/backend/recognizer_plan.rs::ScannerPlanRecord`) exposes the authoritative scanner plan computed in IR — `(family, regex_engine, emit_hint, peer_group)` — so that `scanner_plan.rs`, recognizer mining, and AOT emission stop acting like loosely-coupled subsystems. This is one struct, not a new abstraction layer; the existing three sites read from it. Without this, every "decision authority" rule (13, 14, 17, 19) is enforced by grep gates instead of by data flow.

22. **Cached `ContextFacts` is consumed by ≥2 production passes.** §8g caches `compute_context_facts` on `ir.context_facts`, but the cache is only worth landing if downstream passes actually read from it. At least two of `compute_sp_method_rules`, `csp_strategy::build_alt_domain`, `dispatch::generate_dispatch_tables`, `project_types` consume `ir.context_facts.*` directly. The post-X grep gate confirms ≥2 production reads. A single-consumer cache is partial value and is held until a second consumer lands.

23. **Narrow e-graph rule additions are allowed where they expose already-planned dispatch / scanner forms.** The "no new e-graph rules" non-goal is softened: broad new rule programs (the V-era ten rules) remain Tranche Y, but rule additions narrowly required to expose a recognizer family the tranche is already activating — e.g., a normalization rule that turns `Concat([Literal("rgb"), Literal("(")])` into a single `RecognizerFamily::FunctionHead { "rgb" }` shape — are in-scope. The discipline: each new rule must be tied to a Tranche-X activation deliverable (Phase 9, 10, or 11), not to a speculative future use.

24. **No duplicated planner logic across IR and backend.** If IR / CSP decides alt / wrap / regex / token-dispatch strategy, the backend must consume that decision rather than rediscovering it. Remaining exceptions (where backend-side structural fallback is justified) must be named explicitly in this rule and listed in §10. The grep gate is structural: zero independent structural detection in `backend/` for any decision family declared activated in §3 rules 13, 14, 17, 19, or 20. This is the rule that makes the §8 deletion of `backend/patterns/` non-reversible.

25. **JSON has a parse-time deliverable, not just a compile-time one.** `json_monolithic::canada` parse target is **≥+5% improvement vs post-W**, earned via authoritative `RegexEngine` end-to-end (Phase 11a) + JSON structural punctuation+ws recognition (Phase 11b) + JSON shared helper hoisting (Phase 11c). If 11a-c together fall short of +5%, Phase 11d (surgical `scan_number_mantissa` improvement upstream in parse-that) is reclassified from non-goal to optional in-scope subtask. Excluding the parse-that change purely by repository boundary is artificial when the change is surgical and the target requires it.

26. **CSS recognizer family expansion is a tranche deliverable.** Three new family classes land in X (Phase 10): function-heads (`rgb(`, `rgba(`, `hsl(`, `hsla(`, `calc(`, `var(`, `url(`, `attr(`), hash-prefix tails (`#abcdef` color literals), and unit-tail families (`12px`, `1.5em`, `100%`, etc.). Each lives as a recognizer family module under `crates/ir/src/passes/recognizers/` with a corresponding kernel under `crates/core/src/backend/kernels/`. These exercise the §3 rule 21 unification bridge and earn part of the `css_l4::tailwind` parse improvement. The kernel consumer count rises from 8/8 (post-9b) to 11/11 after Phase 10.

---

## 4. What lands — thirteen phases, one tranche

Each phase is a logical commit (some are multiple). The tranche is the union. Implementation dependency order; later phases compose with earlier substrates.

### Phase 0 — `find_node_id_for_var` reverse map (single largest win)

**File**: `crates/ir/src/passes/types/mod.rs:336-345` plus the constraint generation site (`crates/ir/src/passes/types/generate.rs` or wherever VarId allocation happens)

The current code:

```rust
fn find_node_id_for_var(system: &TypeSystem, var: VarId) -> Option<NodeId> {
    system.node_vars
        .iter()
        .chain(system.vec_context_vars.iter())
        .find_map(|(nid, v)| if *v == var { Some(*nid) } else { None })
}
```

This is called inside a Seq-children loop in `project_types`. On CSS L4 with hundreds of Seq rules and thousands of variables, the search becomes O(rules × children × vars) ≈ quadratic. The samply profile shows it as **23.13% self-time on `compile_css_l4`**, the single largest hot spot in the post-Tranche-W workspace.

**Fix**: build a `var_to_node: FxHashMap<VarId, NodeId>` reverse-map once at Phase 3 entry of `project_types`, before the constraint propagation loop. The map is populated from `system.node_vars` and `system.vec_context_vars` simultaneously. The lookup becomes O(1).

The fix is structural, not a micro-optimization: the existing `node_vars`/`vec_context_vars` are forward maps (NodeId → VarId), and the reverse direction is what the constraint solver actually needs. Storing both directions costs ~32 bytes per variable; on CSS L4 that's ~50 KB, negligible.

**Estimated impact** (from the profiling agent's recommendation #1): **`compile_css_l4` drops from 10.21 ms to ~6 ms (-40%)**. Other compile benches see smaller wins because their type systems have fewer variables.

### Phase 1 — Generalized regex emitter routes through `kernels::charclass`

**Files**: `crates/core/src/generate/regex/emit/generalized/mod.rs:238`, `crates/core/src/generate/regex/emit/generalized/class_segments.rs:184`, `crates/core/src/generate/regex/emit/hir/leaf.rs:135`

The cargo-expand audit found the 86 `is_ascii_digit()` while-loops in CSS L4 generated parser are unchanged from pre-Tranche-W. The Tranche W substrate routes `RegexClass::CharClassQuantified` through `kernels::charclass::emit_call_opt`, but CSS L4 numeric patterns (`[0-9]+(\.[0-9]+)?(e[+-]?[0-9]+)?`) classify as `RegexClass::Numeric` at the top level. The generalized emitter then handles each sub-class (`[0-9]+`, `[a-zA-Z_]`, etc.) **inline** via predicate-loop emission without consulting `scanner_plan`.

Three concrete emit sites bypass the kernel:

- `generalized/mod.rs:238` — the `r"\d"` branch
- `generalized/class_segments.rs:184` — the `'d'` escape branch
- `hir/leaf.rs:135` — the HIR leaf fallback

Each produces the 16-line `while __pos < __end { let __b = ...; if __b.is_ascii_digit() { __pos += 1; } else { break; } }` loop verbatim. **All 86 inline loops** in `/tmp/expand_css.rs` come from these three sites.

**Fix**: at the top of each of the three emit functions, call `kernels::charclass::emit_call_opt(&class.chars, class.negated, lo, hi)` as a short-circuit. If the kernel returns `Some(ts)`, return `ts` instead of falling through to the inline loop emission. The kernel already handles `[0-9]+` / `[0-9]*` / `[a-zA-Z0-9]+` / `[0-9a-fA-F]+` — these are exactly the shapes the inline emitter is producing.

**Estimated impact**:
- Cargo-expand: `is_ascii_digit` count drops from 86 to <10 (residue is bounded shapes the kernel doesn't yet handle).
- Generated CSS L4 parser size: drops by ~5,000 lines.
- I-cache pressure on the parse path: meaningful improvement (each digit-class match is now one helper call, not 16 lines of inline code).
- `css_tailwind` parse-time: ~3-5% improvement from cache locality alone.

### Phase 2 — FxHash for the egraph

**File**: `crates/egraph/src/egraph.rs` and friends

The samply profile of `compile_bbnf` shows three SipHasher hot spots in the egraph hashtables:

- `Id` hashing via `RandomState`: 3.51% self
- `String` hashing via `RandomState`: 3.13% self  
- `u32` hashing via `RandomState`: 3.08% self

Total: ~10% of `compile_bbnf` time spent in `RandomState` hashing. The `EGraph::memo` field is already `FxHashMap` per the imports. The other hashtable usages must be in:
- `csp_scheduler::CspScheduler::run` (`HashMap<Id, usize>` for node count snapshots)
- `csp_scheduler::build_csp` (`HashMap<Id, VarId>`)
- Per-rule rewrite-rule auxiliary `HashMap`s

**Fix**: audit every `HashMap` in the egraph crate and the IR egraph wrapper. Switch all per-compile (non-API-boundary) ones to `rustc_hash::FxHashMap`. The egraph crate already has `rustc-hash` as a dep (the `memo` field uses it).

**Estimated impact**: `compile_bbnf` drops by ~8% (~80 µs out of 987 µs). Other compile benches see proportional drops (egraph hashing is per-node, not per-grammar-feature).

### Phase 3 — HIR e-graph saturation cache (closes the compile_json gate)

**Files**: `parse-that/rust/regex/src/egraph/mod.rs::simplify_hir`, `parse-that/rust/regex/src/info/mod.rs::analyze_with_cost`, `crates/ir/src/passes/regex_info.rs`

The samply profile of `compile_json` shows `bbnf_regex::egraph::saturate_hir_egraph` 1.86% self + `simplify_hir` 9.17% inclusive. JSON has 4 unique regex patterns; each one re-saturates from scratch on every compile. The Tranche W phase 2 `needs_saturation` skip works for trivial HIRs (single Class, single Literal) but JSON's string and number patterns have nested Alternation and Repetition, so they take the full saturate-extract-drop path.

**Fix**: a per-compile `SaturationCache: FxHashMap<u64, Hir>` keyed on the canonical hash of the input HIR. `simplify_hir` checks the cache before building the e-graph; on hit, returns the cached canonical form. On miss, runs the full saturation, stores the result, returns it. The cache lives on the `RegexExtractionCost` struct (or a sibling per-compile container) so the entire saturation work for a grammar happens at most once per pattern shape.

For the cache to work, `Hir` must implement `Hash + Eq`. The Tranche W `derive(Hash)` on `CharSet128` is the missing piece — let me verify the rest of the HIR types derive Hash. `Hir` derives `Clone, Debug, PartialEq` per the audit; `Hash` is missing on the recursive types (`Repetition`, `Hir::Concat`, `Hir::Alternation`). Adding Hash is straightforward — Box<Hir> is hashable if Hir is, Vec<Hir> is hashable if Hir is, the leaf types are already Copy + Hash (post-W).

**Estimated impact**: `compile_json` drops from 108 µs to ~95 µs (~12% improvement vs post-W, ~9.5% vs post-V). Combined with phases 4 and 6, the original Tranche W `compile_json` -10% gate is met.

### Phase 4 — `format!()` audit in hot pipeline paths

**Files**: every file under `crates/ir/src/passes/`, `crates/core/src/pipeline/`, `crates/core/src/backend/driver/`, and the constraint diagnostics in `csp_strategy.rs`

The samply profile of `compile_json` shows `alloc::fmt::format_inner` at **4.32% self time** — ~5 µs of the 108 µs total. JSON should not be doing any formatting during a successful compile. The cost is from `format!()` calls on hot paths that build error or debug strings even when never logged.

**Fix**: grep every file under the listed directories for `format!`. For each call site:
- If the result is an error message that fires once at most: add `#[cold]` or move to a sub-function tagged `#[cold]`.
- If the result is a debug message that fires per-rule or per-node: delete the `format!()` call and use `&'static str` if possible, or guard behind `#[cfg(feature = "debug-trace")]`.
- If the format string has zero arguments: replace `format!("...")` with `"..."` (allocates nothing).
- If the format string is genuinely needed at runtime: leave it alone but document why.

**Estimated impact**: `compile_json` drops by ~5 µs (~4-5% improvement). `compile_bbnf` and `compile_css_l4` see proportional drops (the format! calls are per-pass, not per-grammar-feature).

### Phase 5 — TypeDesc clone elision in driver `or_else` chains

**Files**: `crates/core/src/backend/driver/repeat.rs:31,79`, `crates/core/src/backend/driver/seq.rs:72`, `crates/core/src/backend/driver/wrap.rs:52`

The samply profile of `compile_bbnf` shows `Option::or_else` for `TypeDesc` lookups at **7.42% self-time across hex-distinct call sites** plus `TypeDesc::clone` at 2.41% self. The pattern is:

```rust
let elem_ty = ir.vec_elem_type(n).cloned().or_else(|| ir.node_type(n).cloned()).unwrap_or(TypeDesc::Span);
```

The `.cloned()` calls force a `TypeDesc::clone` for both arms, even though only one is used.

**Fix**: change the call sites to:

```rust
let elem_ty: TypeDesc = ir.vec_elem_type(n).or_else(|| ir.node_type(n)).cloned().unwrap_or(TypeDesc::Span);
```

The `.cloned()` moves to the end, after the `or_else` short-circuits. Now we clone at most once. For sites that only need `&TypeDesc`, the clone disappears entirely.

The deeper architectural fix: change `vec_elem_type` and `node_type` to return `Option<&TypeDesc>` (already the case), and have the consumers borrow throughout the dispatch chain. The `unwrap_or(TypeDesc::Span)` becomes `.unwrap_or(&TypeDesc::Span)` — but `TypeDesc::Span` isn't a const yet. Make it one (it's a unit variant).

**Estimated impact**: `compile_bbnf` drops by ~5% from clone elision. Smaller wins on other compile benches.

### Phase 6 — Compile-scoped CSP batching (closes the compile_json gap)

**Files**: `crates/ir/src/passes/csp_strategy.rs`

**This phase is batching, not globalization.** Tranche X does not introduce new cross-rule coupling constraints or a global cost objective beyond what the per-rule CSP already expresses. `ImplicationConstraint` (Alt parent ↔ child Engine) is still within-rule. A real global CSP — with cross-rule `SharedHelper` hoisting variables and a joint objective — belongs to Tranche Y alongside `Recognizer.peer_group` activation. The honest gain here is O(N) per-rule allocations collapsing into O(1) per compile, plus a single `solve_optimized` call over the union. The section name reflects the substance.

The Tranche W strategy CSP runs per-rule: one `Csp::<StrategyDomain>::new()` per rule body, with per-rule `sites` Vec and `by_node` HashMap. On JSON's 4 rules, that's 4 × ~2 µs = 8 µs of construction overhead. The fast-path saves only ~0.3 µs per rule when no `ImplicationConstraint` fires.

**Fix**: lift the CSP construction to `solve_strategy_decisions`:

```rust
pub fn solve_strategy_decisions(ir: &GrammarIR) -> RecognizerDecisionMap {
    let mut csp = Csp::<StrategyDomain>::new();
    let mut all_sites: Vec<(VarId, Site, RuleId)> = Vec::new();
    let mut all_by_node: HashMap<NodeId, (Option<VarId>, Option<VarId>, Option<VarId>)> = HashMap::new();
    
    for rule in &ir.rules {
        collect_sites_into(&rule.body, ir, dag, cfg, &mut csp, &mut all_sites, &mut all_by_node, rule.id);
    }
    
    if all_sites.is_empty() {
        return HashMap::new();
    }
    
    let constraints_added = add_token_dispatch_constraints_global(ir, dag, &all_by_node, &mut csp);
    
    if constraints_added == 0 {
        // Fast path: no cross-variable constraints anywhere in the IR.
        return decode_min_cost_globally(&csp, &all_sites);
    }
    
    csp.finalize();
    let solution = csp.solve_optimized(&config);
    decode_solution_globally(&solution, &all_sites)
}
```

The single `Csp::new()` allocation replaces N per-rule allocations. The single `add_token_dispatch_constraints_global` walk replaces N per-rule walks. The single `solve_optimized` call replaces N per-rule solves.

For the cross-rule constraints — the `ImplicationConstraint` between an Alt's TokenDispatch mode and its child Engine modes — the global CSP can express them naturally because all the variables exist in the same problem instance.

**Estimated impact**: `compile_json` drops by ~5-7 µs (closes the original Tranche W -10% gate vs post-V baseline). Larger compile benches see smaller relative wins because the per-rule overhead amortizes over more work.

### Phase 7 — Parser hot-path improvements (priority-sequenced, evidence-driven)

Two profile-backed wins land first (7a + 7b), then the tranche **reprofiles** `css_tailwind`, and 7c / 7d are gated on the reprofile and on the remaining gap to the +15% `css_tailwind` gate. This sequencing is deliberate: 7a/7b are measured demolition with clean profiles, 7c is speculative until the post-7a/7b profile data arrives, and 7d is the highest-blast-radius parse-that change in the plan and defaults to defer.

#### 7a — `scan_ws_block_comments` no-comments fast path [lands first]

**File**: `parse-that/rust/parse_that/src/parsers/scan/ws_comment.rs`

12.03% self-time on `css_tailwind` for a state machine that's checking for `/*` on input that has zero comments. The Tailwind CSS file is large (3.6 MB) and the existing scanner walks every byte through the comment-aware state machine.

**Fix**: at the top of `scan_ws_block_comments`, check if the input contains any `/` byte via `memchr(b'/', input)`. If there's no `/` in the next N bytes (or in the entire remaining input), skip the state machine and use a tight `memchr3(b' ', b'\t', b'\n')` loop until a non-whitespace byte is found. If `/` IS present, enter the existing state machine.

**Estimated impact**: `scan_ws_block_comments` self-time on `css_tailwind` drops from 12.03% to <4%. Total parse-time improvement: ~8%.

#### 7b — Byte-literal CSS keyword dispatch [lands second]

**File**: `crates/core/src/backend/rust/emitter/leaves.rs::emit_literal_match_impl`

The cargo-expand audit found **512 `starts_with("…")` UTF-8 string-literal compares** in CSS L4 generated parser. Each one is a UTF-8 length check + memcmp. The samply profile shows `equal_same_length` at 9.27% self on `css_tailwind`.

**Fix**: change the emitter to emit `state.src.as_bytes()[state.offset..].starts_with(#byte_literal)` instead of `state.src[state.offset..].starts_with(#str_literal)`. The byte-literal version skips the UTF-8 length validation.

**Estimated impact**: `equal_same_length` drops from 9.27% to <3%. Total parse-time improvement: ~6%.

#### 7a/7b reprofile checkpoint [mandatory; scoping data for 7c]

After 7a + 7b land, **re-run samply on `css_tailwind`** (same dSYM-symbolicated procedure as the initial profile) and capture which keyword families still dominate:

- `scan_ws_block_comments` self-time (target <4%)
- `equal_same_length` self-time (target <3%)
- per-family residual keyword-dispatch cost (used to scope 7c precisely)
- `css_l4::tailwind` parse MB/s (the +15% gate)

The reprofile is **scoping data for 7c**, not a deferral gate. 7c lands by default for the clearly hot CSS families per §3 rule 26 and the user-feedback expansion — the question is *which* families, not *whether* to ship 7c. 7d remains contingent (see below). Commit `docs/profiles/post-7b-tailwind.txt` as the input to the family scoping.

#### 7c — Packed u32 dispatch for hot CSS literal families [in-scope, family-scoped by reprofile]

**File**: `crates/core/src/backend/rust/emitter/dispatch.rs`

CSS keyword groups (color names, property names, function names) are alternations of literals. After Tranche W's prefix factoring + 7b's byte-literal compares, the inner dispatch is still N × `[u8]::eq` for each remainder. 7c collapses this to a single `u32::from_le_bytes` + match for the families the reprofile flagged as hot.

**Default targets** (lands for these unconditionally if the family appears in the post-7b profile above ~2% residual):

- **CSS property names** (`color`, `width`, `margin`, `padding`, etc.) — the largest single family
- **CSS color names** (the post-W `namedColor` ladder, even after prefix factoring)
- **Short function-name remainders** (`gb`, `gba`, `sl`, `lc` after prefix factoring of `r`/`h`/`ca`)

Other families fall through to the 7b byte-literal path; the packed-dispatch shape only lands where the reprofile shows residual cost. The plan §5c proposed reading the next 1-4 bytes as a u32 and matching against constants:

**Fix**: in `dispatch.rs`, detect when a factored Alt's remainders are all ≤4 bytes. Emit:

```rust
let key = if state.offset + 4 <= state.src.len() {
    u32::from_le_bytes([
        state.src.as_bytes()[state.offset],
        state.src.as_bytes()[state.offset + 1],
        state.src.as_bytes()[state.offset + 2],
        state.src.as_bytes()[state.offset + 3],
    ]) & ((1u32 << (8 * len)) - 1)
} else { 0 };
match key {
    0x... => /* "ed", advance 2 */,
    0x... => /* "et", advance 2 */,
    ...
    _ => /* fall through */,
}
```

**Estimated impact**: ~5-8% on `css_tailwind`. Smaller on JSON (which has fewer keyword groups). Contingent on the reprofile showing residual cost; otherwise deferred to Tranche Y.

#### 7d — `&[u8]` threading in ParserState [highest-priority deferral; default: defer]

**File**: `parse-that/rust/parse_that/src/state.rs`

**This is the highest-blast-radius parse-time change in the plan and the default is to defer.** 7d only lands if 7a + 7b + 7c together have not closed the +15% `css_tailwind` gate. The cargo-expand evidence is good — 376 `state.src[..]` UTF-8 slice sites, zero `is_char_boundary` calls, meaning LLVM already elides bounds checks on the generated parser — but "less invasive than estimated" is not "low-risk". The structural surface area of parse-that state threading is still the largest parse-that change in X's scope, and it touches every parse-that consumer.

**Fix (if needed)**: convert `ParserState::remaining()` and friends to return `&[u8]` instead of `&str`. Span extraction at the end of a successful match converts back via `from_utf8_unchecked` on the validated range. Literal compares become byte-wise (which 7b already did at the codegen level). Regex is already byte-oriented.

This was deferred from Tranche W as the highest-invasiveness phase 5a. The cargo-expand audit shows it's less invasive than the W estimate, but the default is still defer-until-needed, not land-eagerly.

**Estimated impact (if it lands)**: ~5-8% on `css_tailwind` parse-time. If 7a + 7b + 7c already meet the +15% gate, this estimated 5-8% becomes Tranche Y headroom rather than required X work.

### Phase 8 — AOT decision activation (the activation core)

This phase is the heart of the tranche. Tranche W built the activation substrate; Tranche X makes IR/CSP decisions authoritative in AOT emission. Every routing gap where the backend rediscovers what the IR already decided is closed here. The legacy `backend/patterns/` directory is replaced upstream, then deleted — both within this tranche.

#### 8a — Upstream `DelimScanConfig` / `KeyDispatchConfig` migration (recognizer mining)

Structural detection moves into recognizer mining as an explicit deliverable, not an implied side effect of the deletion in 8h:

- `crates/ir/src/passes/recognizers/balanced_wrap.rs::collect` is extended to populate the open/close/pivot bytes that `DelimScanConfig` carries.
- `crates/ir/src/passes/recognizers/key_dispatch.rs` (new) owns keyword-prefix detection.
- Both populate `ir.recognizer_decisions` with authoritative `DelimScanConfig` / `KeyDispatchConfig` values.
- The configs (`DelimScanConfig`, `KeyDispatchConfig`) move to `crates/core/src/backend/types.rs` if they are not already there.

After 8a, `ir.recognizer_decisions` is the authoritative source for delim-scan and key-dispatch decisions. The legacy `patterns::cache::solve_*` code still exists and is still called by the backend at this commit boundary.

#### 8b — Backend reads from authoritative decisions (Step 2 of the patterns replacement)

`BackendPreparation::from_ir` builds `dstate.delim_scan_configs` and `dstate.key_dispatch_configs` lookup maps from `ir.recognizer_decisions` instead of from `patterns::cache::solve_*`. The `alt_strategy::decide_alt_strategy` structural fallback (lines 151-169) is deleted — by this point CSP coverage is complete.

After 8b, the data path flows IR → backend with no independent detection. The legacy `patterns/` files are present but unused. All gates pass. This is the precondition for 8h (the deletion).

#### 8c — `WrapMode` AOT consumption

The backend wrap emission paths read from `ir.recognizer_decisions` for the wrap node:

- `dispatch.rs::emit_delim_scan_impl` consumes the authoritative `WrapMode::DelimScan { config }` from IR.
- `kernels::balanced_wrap::emit_call` consumes the authoritative `WrapMode::BalancedWrap { open, close }` from IR.
- The paired-delim emit path consumes `WrapMode::PairedDelim { open, close }` from IR.
- `crates/core/src/backend/strategy/wrap_strategy.rs` is migrated to read from `ir.recognizer_decisions` for the WrapMode decision (mirroring the Tranche W `alt_strategy.rs` migration).

Closes §3 rule 14. Without 8c, the backend continues to re-derive wrap shape from `patterns::cache::*` and `WrapMode` decisions stay half-activated. This is the data-path deliverable that makes 8h (the deletion) coherent.

#### 8d — `RegexEngine` AOT consumption

The strategy CSP already chooses a regex engine per pattern in `csp_strategy.rs`. Tranche X surfaces this on `RegexInfo`:

- `crates/ir/src/passes/regex_info.rs::RegexInfo` gains a `decisions: FxHashMap<PatternId, RegexEngineKind>` field, populated by the strategy CSP after Phase 6's compile-scoped CSP runs.
- `scanner_plan::plan_regex_scanner` reads `ir.regex_info.decisions[pattern_id]` on the primary path and routes accordingly. The existing `classify_regex` call survives only as the fall-through path when an authoritative decision is absent.
- Generated parsers for JSON / CSS / etc. now use the engine the CSP picked, not the engine `classify_regex` would re-derive at emit time.

Closes §3 rule 13. This is the substrate Phase 11a builds on for the JSON parse-side win.

#### 8e — `AltMode::TokenDispatch` AOT activation [hard deliverable]

The Alt-level dispatch table currently comes from structural detection in `backend/rust/emitter/dispatch.rs::emit_token_dispatch` via `patterns::key_dispatch::try_detect` (the same code 8h is deleting). The strategy CSP already decides `AltMode::TokenDispatch(table)` for the same alts in `csp_strategy.rs`. This phase wires the decision through:

1. `dispatch.rs::emit_token_dispatch` signature takes `&KeyDispatchConfig` from `ir.recognizer_decisions[alt_id]` (populated in 8a) instead of calling `patterns::key_dispatch::try_detect` at emit time.
2. Grep invariant: `dispatch.rs` contains zero calls to `patterns::key_dispatch::try_detect`.
3. The §5 recognizer-decision-consumption gate confirms `AltMode::TokenDispatch` has ≥1 production consumer under AOT.

Closes §3 rule 17. **8e is now a hard deliverable, not "land or defer"** — per the user feedback expansion: token-led branches are mined and chosen by the CSP today, but the AOT emission still rediscovers them via structural detection. Leaving that gap unclosed wastes the largest CSS-side activation win still on the table.

#### 8f — Scanner-planning unification bridge

A small backend-facing record exposes the authoritative scanner plan computed in IR so that `scanner_plan.rs`, recognizer mining, and AOT emission stop acting like loosely-coupled subsystems:

- `crates/core/src/backend/recognizer_plan.rs` (new) defines `ScannerPlanRecord { family: RecognizerFamily, regex_engine: Option<RegexEngineKind>, emit_hint: EmitHint, peer_group: Option<GroupId> }`.
- Populated during recognizer mining; exposed via `ir.recognizer_decisions` per node.
- Read by `scanner_plan::plan_regex_scanner`, `dispatch::emit_delim_scan_impl`, `kernels::*::emit_call`, and the new family kernels (Phase 10).

This is one struct, not a new abstraction layer — the existing three sites read from it. Without this bridge, every "decision authority" rule (13, 14, 17, 19) is enforced by grep gates instead of by data flow. Closes §3 rule 21.

#### 8g — `ContextFacts` multi-pass consumption

`compute_context_facts` is computed once in `mine_recognizers` and currently discarded. Tranche X caches it on `ir.context_facts` AND ensures ≥2 downstream passes consume it:

- `compute_sp_method_rules` reads `ir.context_facts.{is_unique_first, has_no_collision}` for sp-method eligibility decisions.
- `csp_strategy::build_alt_domain` reads `ir.context_facts.{prefix_dispersion}` for cost weighting.
- Optionally `dispatch::generate_dispatch_tables` reads `ir.context_facts.first_byte_distribution`.

The §5 gate confirms ≥2 production reads on `ir.context_facts.*`. Closes §3 rule 22. A single-consumer cache is partial value and is held until a second consumer lands.

#### 8h — Legacy `backend/patterns/` deletion (Step 3 of the patterns replacement)

After 8a-8g land, the backend has zero non-test consumers of `backend/patterns/*`. Tranche X then deletes:

- `crates/core/src/backend/patterns/delim_scan.rs`
- `crates/core/src/backend/patterns/key_dispatch.rs`
- `crates/core/src/backend/patterns/cache.rs`
- `crates/core/src/backend/patterns/mod.rs`
- `crates/core/src/backend/patterns/` (the directory)

Grep invariant: zero `backend::patterns::` references anywhere in the workspace. The commitment is upstream-first (8a, 8b, 8c, 8d, 8e all land before 8h), then deletion — sequenced commits within X, not a tranche boundary. If 8a-8g regress a gate, 8h is held within this tranche cycle (no Tranche X.5).

### Phase 9 — Kernel completion + limited `SharedHelper` activation

This phase finishes the kernel story end-to-end and earns the right to claim `SharedHelper` is "activated" — without overreaching into full DAG-based cross-rule sharing (which is Tranche Y).

#### 9a — `kernels::prefix_class` end-to-end (kernel body + planner + emitter)

Today, `prefix_class.rs` is a 17-line stub hardcoded to `scan_ident` regardless of prefix bytes. **Zero production callers.** Three edits:

1. **Kernel body** — memcmp prefix check + tail-class scan, dispatched by tail_class shape (alnum / digits / hex / unrecognized). Returns `Option<TokenStream>` so unrecognized tail shapes short-circuit to the generalized emitter.

   ```rust
   pub fn emit_call_opt(prefix: &[u8], tail_class: &CharSet128) -> Option<TokenStream> {
       let prefix_lit = proc_macro2::Literal::byte_string(prefix);
       let len = prefix.len();
       let tail_scan = match tail_class_shape(tail_class) {
           TailShape::Alnum  => quote! { ::parse_that::scan_alnum_mut(state) },
           TailShape::Digits => quote! { ::parse_that::scan_digits_mut(state) },
           TailShape::Hex    => quote! { ::parse_that::scan_hex_mut(state) },
           TailShape::Unrecognized => return None,
       };
       Some(quote! {
           if state.src.as_bytes()[state.offset..].starts_with(#prefix_lit) {
               state.offset += #len;
               #tail_scan
           } else { None }
       })
   }
   ```

2. **Planner routing** — `scanner_plan::plan_regex_scanner` routes both `RegexClass::PrefixThenClass { prefix, tail_class }` AND `RegexClass::AccelDriven(...)` (where the accel driver reduces to a prefix + tail-class shape) to `kernels::prefix_class::emit_call_opt(prefix, tail_class).map(ScannerPlan::Kernel)` instead of returning `None`. Without this edit, the kernel has zero callers regardless of how correct its body is.

3. **Emitter fallback** — the generalized emitter's `PrefixThenClass` fallback path defers to the kernel at the top of the emit function — the same short-circuit pattern as Phase 1's charclass routing.

Closes §3 rule 20 for `prefix_class`.

#### 9b — `kernels::sep_list` end-to-end (kernel body + driver routing)

1. **Kernel body** — the full sep-by element loop migrates from `backend/driver/repeat.rs::emit_sep_by`. The body handles the `element (SEP element)*` shape plus optional trailing separator.

2. **Driver routing** — `emit_sep_by` becomes a thin dispatcher that calls `kernels::sep_list::emit_call(separator, element_emit_fn)`. The driver retains the element-emission closure for `in_vec` / `current_rule_name` / slab context.

3. **Repeat / wrap driver consumption** — the `(open, sep, element, close)` pattern in `wrap.rs` and `repeat.rs` routes through `kernels::sep_list::emit_call_with_wrap` for true separator-list recognizers, instead of leaving the loop shape split across legacy driver logic.

Closes §3 rule 20 for `sep_list`. The kernel consumer count gate moves from **6/8 → 8/8**.

#### 9c — Limited `SharedHelper` activation by recognizer signature

A narrow slice of cross-rule helper hoisting lands in X, gated by recognizer signature equality (NOT full DAG-based structural sharing — that is Tranche Y):

- `crates/ir/src/passes/recognizers/mod.rs` canonicalizes recognizer signatures (regex pattern + body shape + family) and populates `Recognizer.peer_group` for sites with matching signatures.
- `crates/ir/src/passes/csp_strategy.rs` reads `peer_group` and emits `AltMode::SharedHelper(group_id)` when the cost weight `strategy_hoist_savings` favors hoisting.
- The backend emits `SharedHelper` as a single shared function called from each peer site, deduplicating inline emission across rules.
- `crates/ir/src/passes/csp_strategy/shared_helper.rs` (new) carries the signature canonicalization helper.

Closes §3 rule 19. This is the precondition for 9d's family-specific hoisting.

#### 9d — Quoted string / identifier / comment_ws / balanced_wrap shared helper slice

The four families above are the ones with the most cross-rule duplication in CSS L4 per the cargo-expand audit. Each gets at least one production hoisting site under 9c's machinery:

- `parse_quoted_string_shared(state) -> Option<Span>` — replaces inline quoted-string emission across all CSS L4 quoted-string sites
- `parse_identifier_shared(state) -> Option<Span>` — replaces inline identifier emission across CSS L4 identifier sites
- `parse_comment_ws_shared(state)` — replaces inline ws-with-comments emission across CSS L4 ws sites
- `parse_balanced_wrap_shared(state, open: u8, close: u8) -> Option<Span>` — replaces inline balanced-wrap emission across CSS L4 brace/paren/bracket sites

Cargo-expand re-measure confirms at least one site per family is hoisted out of inline duplication. Generated parser size on CSS L4 drops by an estimated ~3,000 lines from this phase alone. Closes §3 rule 19's "≥1 production hoisting per family" requirement.

### Phase 10 — CSS recognizer family expansion

Three new recognizer families land in X. Each lives as a recognizer family module under `crates/ir/src/passes/recognizers/` with a corresponding kernel under `crates/core/src/backend/kernels/`. These earn part of the `css_l4::tailwind` parse improvement and exercise the §8f scanner-planning unification bridge.

#### 10a — Function-head families (`rgb(`, `rgba(`, `hsl(`, `hsla(`, `calc(`, `var(`, `url(`, `attr(`)

`crates/ir/src/passes/recognizers/function_head.rs` (new) detects `Concat([Literal(name), Literal("("), …])` for the eight function names above and emits a `RecognizerFamily::FunctionHead { name, paren_byte }` decision.

`crates/core/src/backend/kernels/function_head.rs` (new) emits a single combined memcmp + paren check, replacing 8 separate inline `Literal("rgb").seq(Literal("("))` emissions in CSS L4. Per §3 rule 23, this phase may add a small e-graph normalization rule that turns `Concat([Literal(name), Literal("(")])` into a single `FunctionHead` shape so the recognizer pass sees a uniform structure.

#### 10b — Hash-prefix tails (`#abcdef`, `#abc` color literals)

`crates/ir/src/passes/recognizers/hash_prefix.rs` (new) detects `Seq(Literal("#"), CharClassQuantified(hex))` and emits `RecognizerFamily::HashPrefix { tail_class }`.

`crates/core/src/backend/kernels/hash_prefix.rs` (new) emits memcmp `#` + `scan_hex_mut`. Routes through 8f's `ScannerPlanRecord` so the planner sees a single family rather than two unrelated leaves.

#### 10c — Unit-tail families (`12px`, `1.5em`, `100%`, etc.)

`crates/ir/src/passes/recognizers/unit_tail.rs` (new) detects `Seq(Number, Literal(unit))` for `unit ∈ {"px", "em", "rem", "%", "vh", "vw", "ms", "s", "deg", "rad", "fr", "ch", "ex", "vmin", "vmax"}` and emits `RecognizerFamily::UnitTail { unit }`.

`crates/core/src/backend/kernels/unit_tail.rs` (new) emits the fused number scanner (`css_number_scan_f64`) + unit memcmp.

These three families are the next-step CSS wins from the audit feedback. The kernel consumer count rises from **8/8 (post-9b) to 11/11** after Phase 10.

### Phase 11 — JSON parse-side activation

JSON parse-time becomes a real tranche deliverable, not non-regression. Three JSON-specific phases plus an optional contingent fallback if the target falls short.

#### 11a — JSON string/number authoritative `RegexEngine` end-to-end

Build on Phase 8d. The JSON grammar's string and number patterns route through `kernels::quoted_string::emit_json_call` and `kernels::number::emit_call_*` via `SharedScanner::into_tokens`, but the routing is via `RegexClass` classification at emit time. With Phase 8d's authoritative `RegexInfo::decisions`, the JSON string/number patterns read their engine choice from `ir.regex_info.decisions` directly:

- `kernels::quoted_string::emit_json_call` reads the authoritative engine variant for the JSON string pattern.
- `kernels::number::emit_call_*` reads the authoritative engine variant for the JSON number pattern.
- No local re-classification at the JSON emit sites.

This ensures the CSP-decided engine is honored end-to-end. The kernel fast paths become load-bearing instead of incidental. Closes the JSON-specific half of §3 rule 13.

#### 11b — JSON structural punctuation+ws region recognition

A new recognizer family `RecognizerFamily::PunctWsRegion { puncts: SmallVec<[u8; 8]> }` detects clusters of `, : { } [ ]` with surrounding whitespace and emits a single combined scanner that walks the cluster in one pass:

- `crates/ir/src/passes/recognizers/punct_ws_region.rs` (new) detects `Seq(Ws*, Literal(p), Ws*)` for `p ∈ {",", ":", "{", "}", "[", "]"}` and clusters of those.
- `crates/core/src/backend/kernels/punct_ws_region.rs` (new) emits a SIMD-friendly scanner that consumes the punctuation + surrounding ws in a single pass instead of N separate `parse_ws_then_lit_then_ws` calls.

JSON object/array parsing is the immediate beneficiary; the family is grammar-generic so other dictionary-shaped grammars (CSS property declarations, TOML tables) get the win for free.

#### 11c — JSON shared helper hoisting by signature

JSON's string and number recognizers appear in multiple positions (object key, object value, array element). With Phase 9c's signature-based `SharedHelper` activation, recognizer mining sees these as signature-equivalent and hoists them to a single shared helper called from each position:

- `parse_json_string_shared(state) -> Option<Span>`
- `parse_json_number_shared(state) -> Option<(Span, f64)>` (fused) or `Option<Span>` (non-fused)

Cargo-expand re-measure confirms the JSON generated parser has a single shared function per family instead of N inline copies. This is JSON's immediate beneficiary of Phase 9c — and the validation that limited `SharedHelper` activation actually moves bench numbers.

#### 11d — Optional surgical `scan_number_mantissa` improvement [contingent]

**Reclassified from non-goal to optional in-scope per §3 rule 25.** If 11a + 11b + 11c together fall short of the +5% `json_canada` parse target, the surgical fix in `parse-that/rust/parse_that/src/parsers/scan/number.rs::scan_number_mantissa` lands. The current 30.56% self-time is dominated by a per-byte loop; a SWAR pass or a `memchr_iter` exit over `[0-9]` would close the gap.

This is the only parse-that change that would land via X. Default is "land only if 11a-c are insufficient". Excluding it purely by repository boundary is artificial when the change is surgical and the JSON parse target requires it.

### Phase 12 — Architectural completeness

Lower-priority items the audit flagged but did not measure as load-bearing on the bench gates. Parked at the end of the tranche so they do not gate higher-priority phases.

#### 12a — FxHash workspace audit

The egraph crate is the priority (Phase 2). The IR pass crates also use `std::collections::HashMap` widely. Profile-guided audit: any HashMap that's >100 entries on CSS L4 switches to `FxHashMap`. Incremental cleanup that unblocks future tranches from random per-compile RandomState costs.

#### 12b — Final consumer-invariant verification sweep

The `recognizer_decision_consumption.rs` test (§5 gate) runs as part of the tranche-closing CI sweep. Any decision variant flagged with zero consumers is either fixed within the tranche or held with explicit annotation in §10 — never silent.

---

## 5. Hard gates

| Gate | Threshold |
|---|---|
| All workspace tests pass | yes |
| `bbnf-regex` tests | 25/25 |
| Bootstrap | succeeds; new MD5 committed |
| Consumer-invariant test | passes |
| Grep invariant — deleted files unreferenced | zero hits for `backend::patterns::` |
| `find_node_id_for_var` linear scan in `passes/types/mod.rs` | replaced with O(1) lookup |
| `is_ascii_digit` count in cargo expand of CSS L4 generated parser | **< 10** (currently 86) |
| `format!()` calls in `passes/`, `pipeline/`, `backend/driver/` | **0** in production code paths (only in `#[cold]` error sites) |
| `RandomState` HashMap usage in egraph crate | **0** (all switched to FxHashMap) |
| `cargo bench compile_pipeline::compile_css_l4` | **≥ 40% improvement vs post-W** (10.21 ms → ≤6.13 ms; profile predicts ~6 ms) |
| `cargo bench compile_pipeline::compile_bbnf` | **≥ 10% improvement vs post-W** (987 µs → ≤888 µs) |
| `cargo bench compile_pipeline::compile_json` | **≥ 10% improvement vs post-W AND ≥ 10% vs post-V** (108 µs → ≤97 µs vs W, ≤93 µs vs V) — closes the Tranche W gate miss |
| `cargo bench json_monolithic::canada` parse | **≥ +5% improvement vs post-W** (1188 → ≥1247 MB/s) via Phase 11a + 11b + 11c; if short, 11d (surgical mantissa SWAR) lands |
| `cargo bench json_monolithic::citm` parse | **≥ +3% improvement vs post-W** (1897 → ≥1954 MB/s) — secondary JSON gate, same activation phases |
| `cargo bench css_l4::tailwind` parse | **≥ +15% improvement vs post-W** (249 → ≥286 MB/s; cumulative from 7a + 7b + 7c-for-hot-CSS-families + Phase 10 family expansion; 7d still contingent) |
| `cargo bench css_l4::bootstrap` parse | **≥ +10% improvement vs post-W** (244 → ≥268 MB/s) — secondary CSS gate, same Phase 10 families |
| Recognizer decision consumption audit | every `RecognizerDecision` variant (all `AltMode`, `WrapMode`, `SeqMode`, regex-engine variants) has ≥1 production consumer in AOT or VM — new invariant test `crates/core/tests/recognizer_decision_consumption.rs` |
| No duplicated planner logic | structural grep over `crates/core/src/backend/` finds zero independent detection of any decision family declared activated in §3 rules 13, 14, 17, 19, 20; remaining exceptions are named explicitly in §3 rule 24 |
| Charclass kernel routing is structural | grep over `crates/core/src/generate/regex/emit/` finds zero inline `is_ascii_digit` / `is_ascii_hexdigit` / `is_ascii_alphanumeric` loops without a preceding `kernels::charclass::emit_call_opt` short-circuit |
| `RegexEngine` decision authority | `scanner_plan::plan_regex_scanner` reads `ir.regex_info.decisions` on the primary path; `classify_regex` appears only in the fall-through path for missing decisions |
| `WrapMode` decision authority | backend wrap emission (`emit_delim_scan_impl`, `kernels::balanced_wrap::emit_call`, `wrap_strategy.rs`) reads from `ir.recognizer_decisions` for delim / balanced / paired wraps; zero independent structural detection in `backend/` |
| `AltMode::TokenDispatch` AOT activation | `dispatch.rs::emit_token_dispatch` has zero calls to `patterns::key_dispatch::try_detect`; the dispatch table comes from `ir.recognizer_decisions[alt_id]`; this is a hard deliverable, not a "land or defer" |
| Limited `SharedHelper` activation | each of {quoted_string, identifier, comment_ws, balanced_wrap} has ≥1 production hoisting site in the generated CSS L4 parser; `Recognizer.peer_group` is populated by signature canonicalization in recognizer mining; the generated parser contains `parse_quoted_string_shared` / `parse_identifier_shared` / `parse_comment_ws_shared` / `parse_balanced_wrap_shared` (or equivalents) |
| Scanner-planning unification bridge | `crates/core/src/backend/recognizer_plan.rs::ScannerPlanRecord` exists and is read by ≥3 sites (`scanner_plan.rs`, `dispatch.rs`, at least one kernel family) |
| `ContextFacts` multi-pass consumption | grep over `crates/ir/src/passes/` and `crates/core/src/backend/` finds ≥2 production reads on `ir.context_facts.*` |
| `backend/patterns/` deletion is sequenced | 8a + 8b + 8c + 8d + 8e commits land strictly before the 8h deletion commit; the intermediate state with legacy present but unused passes every gate |
| Kernel consumer count | **8/8** (post-9b): every `backend/kernels/` family module has ≥1 production caller; **11/11** after Phase 10's three new family kernels (function_head, hash_prefix, unit_tail) |
| CSS recognizer family expansion | each new family kernel (`kernels::function_head`, `kernels::hash_prefix`, `kernels::unit_tail`) has ≥1 production caller via the recognizer pass; cargo-expand on CSS L4 confirms ≥1 collapse site per family |
| `docs/benchmarks/post-X.json` categorizes wins | the report has a `win_categories` section with explicit compile-time, parse-time, and activation buckets, plus per-phase attribution linking each bench delta to the phase that earned it |

Failure on any hard gate holds the tranche open. No workarounds. No deferrals.

---

## 6. Files added

**IR / recognizer mining**:

- `crates/ir/src/passes/recognizers/key_dispatch.rs` (new) — keyword-prefix detection (was in `backend/patterns/key_dispatch.rs`); populates `ir.recognizer_decisions` with `KeyDispatchConfig` per Phase 8a
- `crates/ir/src/passes/recognizers/function_head.rs` (new, Phase 10a) — detects `Concat([Literal(name), Literal("("), …])` for the eight CSS function names
- `crates/ir/src/passes/recognizers/hash_prefix.rs` (new, Phase 10b) — detects `Seq(Literal("#"), CharClassQuantified(hex))` for CSS color literals
- `crates/ir/src/passes/recognizers/unit_tail.rs` (new, Phase 10c) — detects `Seq(Number, Literal(unit))` for CSS unit families
- `crates/ir/src/passes/recognizers/punct_ws_region.rs` (new, Phase 11b) — detects punctuation+ws clusters for JSON object/array parsing

**Backend kernels** (one per new recognizer family):

- `crates/core/src/backend/kernels/function_head.rs` (new, Phase 10a)
- `crates/core/src/backend/kernels/hash_prefix.rs` (new, Phase 10b)
- `crates/core/src/backend/kernels/unit_tail.rs` (new, Phase 10c)
- `crates/core/src/backend/kernels/punct_ws_region.rs` (new, Phase 11b)

**Activation infrastructure**:

- `crates/core/src/backend/recognizer_plan.rs` (new, Phase 8f) — `ScannerPlanRecord` scanner-planning unification bridge
- `crates/ir/src/passes/csp_strategy/shared_helper.rs` (new, Phase 9c) — recognizer signature canonicalization for limited `SharedHelper` activation

**HIR / cache**:

- `parse-that/rust/regex/src/egraph/saturation_cache.rs` (new, Phase 3) — per-compile HIR canonicalization cache

**Tests**:

- `crates/core/tests/recognizer_decision_consumption.rs` (new) — invariant test asserting every `RecognizerDecision` variant has ≥1 production consumer in AOT or VM; satisfies the §5 consumption audit gate

**Benchmarks / docs**:

- `docs/benchmarks/post-X.json` — with explicit `win_categories` section (compile-time / parse-time / activation buckets) and per-phase attribution per §5 categorization gate
- `docs/profiles/post-7b-tailwind.txt` (new) — samply re-profile output after 7a + 7b land, used to scope which CSS families get 7c packed dispatch

## 7. Files modified

- `crates/ir/src/passes/types/mod.rs` — `find_node_id_for_var` becomes a `FxHashMap<VarId, NodeId>` lookup; reverse map built once at Phase 3 entry
- `crates/ir/src/passes/types/generate.rs` — populates the reverse map alongside `node_vars`
- `crates/core/src/generate/regex/emit/generalized/mod.rs` — `kernels::charclass::emit_call_opt` short-circuit
- `crates/core/src/generate/regex/emit/generalized/class_segments.rs` — same
- `crates/core/src/generate/regex/emit/hir/leaf.rs` — same
- `crates/egraph/src/egraph.rs` — every per-compile HashMap → FxHashMap
- `crates/egraph/src/csp_scheduler.rs` — `HashMap<Id, ...>` → FxHashMap
- `parse-that/rust/regex/src/egraph/mod.rs` — `simplify_hir` reads/writes the saturation cache
- `parse-that/rust/regex/src/egraph/cost.rs` — `RegexExtractionCost` carries the cache (or it's a sibling)
- `parse-that/rust/regex/src/hir/mod.rs` — `Hir`, `Repetition`, `CharClass::Bytes`, etc. derive `Hash` (`CharSet128` already does after Tranche W)
- `crates/ir/src/passes/regex_info.rs` — passes a per-compile cache through `analyze_with_cost`
- `crates/ir/src/passes/csp_strategy.rs` — global CSP, single `Csp::new()`, single `solve_optimized` call
- `crates/core/src/backend/driver/{repeat,seq,wrap}.rs` — `or_else` chain returns `Option<&TypeDesc>`, clones once at the end
- `crates/ir/src/types/type_desc.rs` — `TypeDesc::Span` exposed as a `const` for `unwrap_or(&TypeDesc::Span)`
- `parse-that/rust/parse_that/src/parsers/scan/ws_comment.rs` — `memchr3(b' ', b'\t', b'\n')` no-comments fast path
- `crates/core/src/backend/rust/emitter/leaves.rs` — emit byte-literal `starts_with` instead of `&str`
- `crates/core/src/backend/rust/emitter/dispatch.rs` — packed u32 dispatch for hot CSS literal families (Phase 7c, in-scope, family-scoped by reprofile); `emit_token_dispatch` reads `KeyDispatchConfig` from `ir.recognizer_decisions` instead of calling `patterns::key_dispatch::try_detect` (Phase 8e); `emit_delim_scan_impl` reads `WrapMode::DelimScan { config }` from `ir.recognizer_decisions` (Phase 8c)
- `crates/core/src/generate/regex/emit/scanner_plan.rs` — primary path reads regex engine decisions from `ir.regex_info.decisions` (Phase 8d); routes `RegexClass::PrefixThenClass` and `RegexClass::AccelDriven` to `kernels::prefix_class::emit_call_opt` (Phase 9a); reads from `ScannerPlanRecord` per Phase 8f
- `crates/ir/src/passes/regex_info.rs` — `RegexInfo` gains `decisions: FxHashMap<PatternId, RegexEngineKind>` populated by the strategy CSP (Phase 8d); passes a per-compile saturation cache through `analyze_with_cost` (Phase 3)
- `crates/core/src/backend/strategy/wrap_strategy.rs` — migrated to read `WrapMode` from `ir.recognizer_decisions` (Phase 8c), mirroring the Tranche W `alt_strategy.rs` migration; structural detection deleted
- `crates/core/src/backend/kernels/balanced_wrap.rs` — `emit_call` consumes `WrapMode::BalancedWrap { open, close }` from `ir.recognizer_decisions` instead of the structural balanced-wrap detection (Phase 8c)
- `crates/core/src/backend/kernels/{quoted_string,identifier,comment_ws,balanced_wrap}.rs` — gain `emit_shared_helper(...)` variants for the Phase 9d slice; emit `parse_*_shared` functions hoisted via `Recognizer.peer_group`
- `crates/ir/src/passes/recognizers/mod.rs` — populates `Recognizer.peer_group` via signature canonicalization (Phase 9c); populates `ir.context_facts` instead of discarding (Phase 8g); populates `ScannerPlanRecord` per node (Phase 8f)
- `crates/ir/src/passes/csp_strategy.rs` — emits `AltMode::SharedHelper(group_id)` when `peer_group` and cost favors hoisting (Phase 9c); also the global compile-scoped batching from Phase 6
- `crates/core/src/backend/driver/repeat.rs` — `emit_sep_by` becomes a thin dispatcher to `kernels::sep_list::emit_call` / `emit_call_with_wrap` (Phase 9b)
- `crates/core/src/backend/driver/wrap.rs` — wrap drivers route through `kernels::sep_list::emit_call_with_wrap` for true separator-list shapes (Phase 9b)
- `crates/core/src/backend/kernels/prefix_class.rs` — real memcmp + tail-class body, `emit_call_opt` returns `Option<TokenStream>` (Phase 9a)
- `crates/core/src/backend/kernels/sep_list.rs` — full sep-by element loop migrated from `repeat.rs` (Phase 9b)
- `crates/ir/src/passes/sets/mod.rs`, `crates/ir/src/passes/types/mod.rs`, `crates/core/src/backend/strategy/*.rs` — at least two of these gain reads on `ir.context_facts.*` for §3 rule 22 / Phase 8g
- `parse-that/rust/parse_that/src/parsers/scan/number.rs` — optional surgical `scan_number_mantissa` improvement (Phase 11d, contingent)
- `parse-that/rust/parse_that/src/state.rs` — `ParserState::remaining()` returns `&[u8]` (Phase 7d, contingent on +15% gate miss)
- `crates/ir/src/types/grammar.rs` — `context_facts: ContextFactsMap` field added (Phase 8g)
- `crates/ir/src/passes/regex/`, dispatching, etc. — every `format!()` audited and demoted to `#[cold]` or `&'static str` (Phase 4)
- `crates/core/src/backend/strategy/alt_strategy.rs` — structural fallback deleted (Phase 8b)
- `crates/core/src/pipeline/compile.rs` — `BackendPreparation::from_ir` reads `delim_scan_configs` / `key_dispatch_configs` from `ir.recognizer_decisions` (Phase 8b); strategy CSP runs once per compile (Phase 6)
- `crates/{ir,core}/CLAUDE.md`, repo-root `CLAUDE.md` — documentation refresh covering thirteen-phase activation

## 8. Files deleted

- `crates/core/src/backend/patterns/delim_scan.rs`
- `crates/core/src/backend/patterns/key_dispatch.rs`
- `crates/core/src/backend/patterns/cache.rs`
- `crates/core/src/backend/patterns/mod.rs`
- `crates/core/src/backend/patterns/` (the directory)

`crates/core/src/backend/patterns/decisions.rs` is **renamed** to `crates/core/src/backend/types/decisions.rs`, not deleted (it carries shared decision functions, not detection).

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
cargo bench -p bbnf --bench json_monolithic --bench css_l4 --bench compile_pipeline 2>&1 | tee /tmp/post-X-benches.txt

# Architectural assertions
grep -rn "find_node_id_for_var\b" crates/                          # must be the new HashMap-backed function
grep -rn "RandomState" crates/egraph/                              # must be empty or only in tests
grep -rn "format!\b" crates/ir/src/passes/ crates/core/src/pipeline/ crates/core/src/backend/driver/ # must be empty or #[cold]
grep -rn "backend::patterns::" crates/                             # must be empty (directory deleted)
grep -rn "compile_error" crates/core/src/backend/kernels/          # must be empty (still)
grep -rn "csp_solver::Csp\|OptimizationMode::MinimizeCost" crates/  # must be non-empty (still)
grep -rn "patterns::key_dispatch::try_detect" crates/core/src/backend/  # must be empty (Phase 8e — emit_token_dispatch reads from ir.recognizer_decisions)
grep -rn "patterns::cache::solve_" crates/core/src/backend/  # must be empty (Phase 8b — backend reads authoritative decisions)
grep -rn "classify_regex" crates/core/src/generate/regex/emit/scanner_plan.rs  # must appear only in fall-through path (§3 rule 13 / Phase 8d)
grep -rn "is_ascii_digit\|is_ascii_hexdigit\|is_ascii_alphanumeric" crates/core/src/generate/regex/emit/  # every hit must have a preceding kernels::charclass::emit_call_opt short-circuit (§3 rule 16)
grep -rn "ScannerPlanRecord" crates/  # must appear in ≥3 sites (Phase 8f / §3 rule 21)
grep -rn "ir\.context_facts\." crates/  # must appear in ≥2 production reads (Phase 8g / §3 rule 22)
grep -rn "Recognizer\.peer_group\|peer_group:" crates/  # must be populated and read (Phase 9c / §3 rule 19)
grep -rn "AltMode::SharedHelper" crates/  # must be emitted by csp_strategy and consumed by backend (Phase 9c / §3 rule 19)
grep -rn "parse_quoted_string_shared\|parse_identifier_shared\|parse_comment_ws_shared\|parse_balanced_wrap_shared" /tmp/expand_css_postX.rs  # ≥1 production hoisting site per family (Phase 9d)
grep -rn "RecognizerFamily::FunctionHead\|RecognizerFamily::HashPrefix\|RecognizerFamily::UnitTail\|RecognizerFamily::PunctWsRegion" crates/  # all four new families must be populated and consumed (Phase 10 + 11b)
grep -rn "ir\.regex_info\.decisions" crates/  # must appear in scanner_plan + JSON kernels (Phase 8d + 11a)

# Cargo expand for JSON: confirm shared helpers landed
cargo expand -p bbnf --bench json_monolithic > /tmp/expand_json_postX.rs 2>&1
grep -c "parse_json_string_shared\|parse_json_number_shared" /tmp/expand_json_postX.rs  # must be ≥2 (Phase 11c)

# Cargo expand re-measure
cargo expand -p bbnf --bench css_l4 > /tmp/expand_css_postX.rs 2>&1
grep -c "is_ascii_digit" /tmp/expand_css_postX.rs                  # must be < 10 (currently 86)
grep -c "scan_digits_mut\|scan_alnum_mut\|scan_hex_mut" /tmp/expand_css_postX.rs # must be > 60 (currently 0)

# Consumer-invariant
cargo test -p bbnf --test recognizer_consumer_invariant

# Profile-confirmed cliffs (samply)
cargo bench -p bbnf --bench compile_pipeline --no-run
COMP_BIN=$(find target/release/deps -maxdepth 1 -type f -perm -111 -name 'compile_pipeline-*' ! -name '*.d' ! -name '*.dSYM' -exec ls -t {} + | head -1)
samply record --save-only --unstable-presymbolicate -o /tmp/postX_compile_css_l4.samply -- "$COMP_BIN" --bench compile_css_l4
# Top inclusive in passes::types::project_types must be < 10% (currently 51.03%)
# find_node_id_for_var must NOT appear in the top 20

samply record --save-only --unstable-presymbolicate -o /tmp/postX_compile_bbnf.samply -- "$COMP_BIN" --bench compile_bbnf
# RandomState SipHasher self-time must be < 2% (currently ~10% combined)
# TypeDesc::clone self-time must be < 0.5% (currently 2.41%)

samply record --save-only --unstable-presymbolicate -o /tmp/postX_compile_json.samply -- "$COMP_BIN" --bench compile_json
# simplify_hir inclusive must be < 2% (currently 9.17%)
# format_inner self-time must be < 0.5% (currently 4.32%)
```

If any cliff measurement still exceeds the post-X threshold, the tranche is incomplete.

---

## 10. Non-goals

- **No broad new e-graph rule program.** The V-era ten shape-exposure rules (five grammar-tier, five HIR-tier) remain deferred to Tranche Y. **Narrow rule additions are allowed in X** when they expose an already-planned dispatch / scanner form, per §3 rule 23 — e.g., a Phase 10a normalization rule that turns `Concat([Literal("rgb"), Literal("(")])` into a `RecognizerFamily::FunctionHead { "rgb" }` shape. The discipline: each new rule must be tied to a Tranche-X activation deliverable (Phase 9, 10, or 11), not to a speculative future use.
- **Full DAG-based cross-rule sub-expression hoisting is deferred to Tranche Y by name.** Phase 9c activates `SharedHelper` only via recognizer signature equality — a narrow signature-based slice for the four families enumerated in 9d. The full DAG-based analysis (where the pass walks `ir.dag` for shared sub-trees regardless of recognizer family) and the corresponding emission path are both Tranche Y. The substrate is in place — `Recognizer.peer_group`, `csp_strategy::AltMode::SharedHelper`, the `strategy_hoist_savings` cost weight, and the DAG's hash-cons identity — but X earns the right to claim "limited `SharedHelper` activated" without overreaching into full DAG-based hoisting. Phase 6's compile-scoped CSP batching is the precondition (one CSP per compile, not N) so that Tranche Y can add cross-rule `SharedHelper` variables to the same problem instance.
- **No common-suffix factoring.** Dual of common-prefix factoring; deferred to Tranche Y.
- **No type-driven dispatch fusion.** Deferred to Tranche Y.
- **`scan_number_mantissa` SWAR is reclassified from non-goal to optional in-scope contingent.** Per §3 rule 25, if Phase 11a + 11b + 11c together fall short of the +5% `json_canada` parse target, Phase 11d (the surgical mantissa improvement upstream in parse-that) lands. Default is "land only if 11a-c are insufficient". This is the only parse-that change that may land via X; broader parse-that work stays out of scope.
- **No cost-weight upgrade for `seq_strategy` / `repeat_strategy` / `ref_strategy` / `wrap_strategy`.** Their decision spaces are small (Repeat is `Optional` vs `Many` based on `lo`/`hi`, Ref is `DirectCall` vs `InlineBody` vs `InlineFusion`, etc.). The cost-weight gestalt upgrade is consistent with the architecture but doesn't move bench numbers. Deferred to Tranche Y. (Note: `wrap_strategy` IS modified in Phase 8c — but only to consume `ir.recognizer_decisions`, not to gain a new cost objective.)
- **No upgrade of the dispatch-eligibility / type-projection / egraph-scheduler CSPs to `MinimizeCost`.** Each one models a real CSP but its domain doesn't have a meaningful cost function today. Adding cost weights is consistent with the architecture but speculative; deferred until a profile shows the CSP search is the bottleneck.
- **No real global cross-rule cost objective in the strategy CSP.** Phase 6 is *batching* — one `Csp::new()` per compile instead of N — not globalization. A real global CSP with cross-rule `SharedHelper` hoisting variables and a joint objective is Tranche Y. Phase 6's batched CSP becomes the substrate Y builds on.
- **No staged tranches between W and X.** Tranche W introduced the substrate; Tranche X delivers the full activation. There is no Tranche W.5. Phase 8's eight-step internal sequencing (8a → 8h, replace then delete) is *within* this tranche, not a separate tranche — sequenced commits inside X, not a half-state across tranche boundaries.
- **No "follow-up tranche to delete legacy".** The deletions in §8 happen in this tranche, even if Phase 8h lands in a later commit than Phase 8a.

---

## Reference: previous plan in this file

The tranche immediately preceding this one was **Tranche W — Activation, Hot-Path Demolition, and Three Distinct Cliffs**. It built the activation layer that Tranche X now inherits and extends. Tranche W landed (post-W baseline at `docs/benchmarks/post-W.json`):

- Phase 0: `factor_literal_prefixes` rewrite around a per-pass `FactorCtx` (10.6× speedup on `compile_css_l4`'s 148-branch `namedColor` cliff)
- Phase 1: `EGraph::with_capacity` hash-cons sizing for both grammar and HIR tiers; rayon parallelism threshold bumped from 16 to 128 via `CostConfig.parallelism_threshold`
- Phase 2: HIR e-graph capacity hints + `needs_saturation` skip for trivial HIRs
- Phase 3a: `egraph::CostConfig` + `bbnf_ir::CostConfig` substrate; `GrammarCostModel::from_config`, `RegexExtractionCost::from_egraph_config`; `BBNF_COST_*` env-var overrides
- Phase 3b: Real `csp_solver::Csp` with `OptimizationMode::MinimizeCost` in `csp_strategy.rs`; `csp_recognizers.rs` deleted; per-rule fast-path elides solve when no `ImplicationConstraint` fires
- Phase 3c: `alt_strategy.rs` migrated to read from `ir.recognizer_decisions` with structural fallback
- Phase 3d / 5d: 6/8 `backend/kernels/` family modules wired through `scanner_plan::SharedScanner::into_tokens` and `dispatch.rs::emit_delim_scan_impl`; `kernels::balanced_wrap::emit_call` owns the full delim-scan body; `parse_that::scan_digits_mut` / `scan_alnum_mut` / `scan_hex_mut` hoisted helpers added
- Phase 4: 4 hot-path clones eliminated (recognizers/mod.rs DAG clone, sort.rs/factor_lookahead.rs strings clone, propagate.rs ContextFacts clone); `CharSet128` derives `Copy + Hash`
- Phase 5b: parser enums emit manual `Clone + Copy` impls (avoiding nightly `derive_clone_copy_internals`); `BumpSlab::alloc_slice_copy` fast path used at every codegen call site

Tranche W left two new top-of-profile cliffs visible (the type-pass linear scan, the egraph hashing) and one silent activation hole (the generalized regex emitter bypassing `kernels::charclass`). **Tranche X is what makes the post-W profile cliffs disappear.**
