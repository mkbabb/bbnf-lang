**Ground Truth**

I did this as a planning-only audit. I did not edit tranche docs or code. I had to use a clean `HEAD` worktree for measurement because the current worktree does not build: [`crates/core/src/grammar/generated.rs`](/Users/mkbabb/Programming/bbnf-lang/crates/core/src/grammar/generated.rs) no longer matches consumers in [`crates/core/src/graph/deps.rs`](/Users/mkbabb/Programming/bbnf-lang/crates/core/src/graph/deps.rs) / [`metadata.rs`](/Users/mkbabb/Programming/bbnf-lang/crates/core/src/graph/metadata.rs) / lowering, with missing `BbnfBootstrapRuleKind::{rhs,value_expr}`.

On clean `HEAD`, current measured state is:

- `compile_pipeline --bench`: `compile_json 141 µs`, `compile_css_l4 10.19 ms`, `compile_bbnf 2.01 ms`, `compile_sheets 2.04 ms`, `compile_css_mono 311 µs`, `compile_ebnf 475 µs`.
- `json_monolithic --bench`: `canada 1764 MB/s`, `citm 2016 MB/s`, `data 1558 MB/s`, `data_xl 1004 MB/s`, `twitter 1635 MB/s`.
- `css_l4 --bench`: `bootstrap 349 MB/s`, `normalize 627 MB/s`, `tailwind` fails at byte `387594` in `tailwind.css`.
- There is no current local regex bench target. Regex truth comes from compile-time profiles and regex code paths, not a live bench binary.

`cargo expand` truth:

- JSON expanded parser has `24` `trim_leading_whitespace_mut` calls and `0` `scan_structural` / `filter_quote_parity` / `__rule_direct` / `DirectSlot` hits in [/tmp/json_monolithic.expand.rs](/tmp/json_monolithic.expand.rs).
- CSS L4 expanded parser also has `0` `scan_structural` / `filter_quote_parity` / `__rule_direct` / `DirectSlot` hits, but `222` `scan_ws_block_comments` calls and `4` `scan_number_f64` hits in [/tmp/css_l4.expand.rs](/tmp/css_l4.expand.rs).
- JSON’s f64 payload path is effectively dead in expansion: it emits one `push_leaf_with_f64`, but there is no `__has_payload = true` assignment at all. CSS L4 does set `__has_payload = true` for numeric leaves.
- Structural dispatch substrate exists in source at [`crates/core/src/backend/rust/emitter/alt.rs`](/Users/mkbabb/Programming/bbnf-lang/crates/core/src/backend/rust/emitter/alt.rs:115) and parse-entry setup exists in [`grammar.rs`](/Users/mkbabb/Programming/bbnf-lang/crates/core/src/backend/rust/emitter/grammar.rs:464), but it is not present in emitted JSON/CSS L4 code today.

`samply` truth:

- `json_twitter`: top self-time is `parse_that::scanners::trim_leading_whitespace_scan_and_cache` at `38.9%`, then `JsonParser::__value` at `30.7%`, then `JsonParser::__pair` at `23.9%`, then `memchr` at `4.4%`.
- `css_bootstrap`: top self-time is `CssL4Parser::__declaration` at `30.3%`, `__compoundSelector` at `23.7%`, `__value` at `6.7%`, and `scan_ws_block_comments_slow` at `5.6%`.
- `compile_css_l4`: top self-time includes `quicksort<u32>` `5.3%`, `SipHasher::write` `4.6%`, `compute_facts_for_node` `2.2%`, `solve_component` `2.1%`, `bb_recurse` `1.9%`, `TapeCursor::children` `1.4%`, `FactorCtx::factor` `1.3%`, `TypeDesc::clone` `1.1%`.
- `compile_bbnf`: top self-time includes `quicksort<u32>` `6.8%`, `bb_recurse` `3.9%`, `SipHasher::write` `3.6%`, `solve_component` `3.3%`, `TypeDesc::clone` `2.0%`, `TapeCursor::children` `1.8%`, `compute_facts_for_node` `1.1%`. Regex-related inclusive time is real but secondary: `compute_regex_info 3.3%`, `RegexInfo::analyze_with_cost_cached 3.1%`, `simplify_hir_cached 2.4%`.

**Tranche Recap**

What actually landed across `AA` through `AO`, distilled:

- `AA`: big reset plan; promised `post-AA` artifacts never materialized.
- `AB`: tape-only runtime substrate landed; direct projection explicitly deferred.
- `AC`: tape-first rewrite landed; direct projection, typed direct views, and compact encodings stayed deferred.
- `AE`: lowering became tape/shape-agnostic; clean bootstrap regen remained deferred.
- `AF`: three-tier substrate work advanced, but Tier B / direct-consumer wiring was deferred.
- `AG`: tier variables were activated in IR, but Tier B still had no live emitter/view consumer.
- `AH`: audit only, no landed code.
- `AI`: planned to wire emission tiers and typed accessors; repo state says this never closed as documented.
- `AJ`: zero-copy `Parsed<'p, R>`, child access fixes, and canada correctness recovery landed.
- `AK`: flat tape + per-branch variant discriminator landed.
- `AL`: prototypes only; no landed tranche doc.
- `AM`: payload buffer, per-branch tape surgery, and structural-scan substrate landed; structural dispatch integration did not.
- `AN`: correctness fixes, CSS `@ws` SIMD routing, whitespace bitmap caching landed; scanner generalization, one-pass string scanning, wider SIMD, and instrumentation remain open.
- `AO`: still a plan, not a closed tranche. Its central thesis is correct: the remaining gap is composition, especially structural dispatch and reduced transient work.

Net: the repo has a strong tape substrate, partial payload projection, and lazy views. It does not have a live tripartite emission system. It does not have live direct-to-struct parser ABI. It does not have structural dispatch active on the working JSON/CSS L4 benches. Tailwind CSS L4 still does not parse.

**What Works / What Doesn’t**

Works:

- Tape-first zero-copy runtime.
- Per-branch tape surgery.
- CSS whitespace/comment scanner routing.
- Some numeric leaf payload projection, mainly CSS-side.
- Compile pipeline is fast enough to iterate.

Doesn’t work:

- Current dirty worktree build.
- Tailwind CSS L4 parse.
- Direct-to-struct as a real parser output mode.
- Lazy AST beyond tape views plus recomputing accessors.
- Structural prescan/dispatch on emitted JSON/CSS parsers.
- Full benchmark/profile bookkeeping after `Z`; `post-AA` is missing.
- Regex benchmarking as a live local target.

**AP Plan**

`P0: Closure and truth`
- Fix the bootstrap/generated mismatch first so the main worktree builds again.
- Record AO as still open, with explicit facts: Tailwind failure, direct projection not live, structural dispatch not emitted, `post-AA` missing, regex bench missing.
- Delete `css_monolithic` and `compile_css_mono` after replacing any remaining dependency on them; keep only CSS L4.

`P1: Correctness and activation`
- Make CSS L4 Tailwind parse green. The failure region starts at the multi-selector / custom-property block around `var(--tw-empty,/*!*/ /*!*/)` in `tailwind.css`.
- Activate structural dispatch on JSON first, then CSS L4 where legal. The infrastructure exists; emitted code proves it is currently inert.
- Validate activation with new `cargo expand` gates: require emitted `scan_structural` / `filter_quote_parity` for grammars that qualify.

`P2: Real direct projection`
- Stop calling the current state “direct-to-struct”. It is tape + f64 payloads.
- Implement real direct projection for deterministic seq/object cases while preserving tape fallback for ambiguous/inspection-heavy paths.
- Generalize payload-bearing leaves beyond `f64` only where measured recomputation cost exists.
- Add caching or slot reuse for expensive lazy leaf projections so repeated `.value()` calls do not keep reparsing spans.

`P3: Hot-path demolition`
- JSON parse: kill repeated whitespace trimming by structural prescan; current `trim_leading_whitespace_scan_and_cache` is the top self hotspot.
- CSS L4 parse: reduce declaration/selector control-flow bloat and move `scan_ws_block_comments_slow` off the hot path where possible.
- Compile path: attack clone/hash/sort churn before new fancy optimizations. Highest-value general fixes are:
  - eliminate repeated `TapeCursor::children()` allocation in compile-time graph/lowering walks such as [`crates/core/src/graph/deps.rs`](/Users/mkbabb/Programming/bbnf-lang/crates/core/src/graph/deps.rs),
  - replace clone-heavy `TypeDesc` flows with IDs/borrows,
  - reduce e-graph parent/node cloning in [`crates/egraph/src/egraph.rs`](/Users/mkbabb/Programming/bbnf-lang/crates/egraph/src/egraph.rs),
  - cache reverse regex string lookup and DFA/HIR emission products in regex codegen.

`P4: Sonic-rs gap, without overfitting`
- Adopt their general lessons, not JSON-only tricks: borrow-first APIs, fewer transient representations, scratch reuse in parser state, fused scan stages, optional direct materialization, and flatter hot code.
- Do not overfit to one dataset. Every optimization must be validated on `json_monolithic`, `css_l4`, and compile pipeline, with regex compile-time signals tracked until a real regex bench is restored.

If you want the next step, it should be an implementation tranche that starts with `P0 + P1`: restore buildability, make Tailwind parse, then force structural dispatch to become visible in emitted JSON/CSS code. That is the shortest path to turning AO from prose into load-bearing behavior.