# Tranche AG — Substrate Closure

AG closed the AF circle: every deferred AF deliverable became a live
AG deliverable.

## AG.0 — Build / test parallelization

Cut typical dev iteration time from 2–4 minutes to under 30 seconds.

- **Proc-macro cache key tier separation.** `BBNF_DERIVE_BUILD_ID`
  no longer folds the derive/core/ir source tree hash into the cache
  key. A manually-bumped `BBNF_SCHEMA_VERSION` in
  `crates/derive/src/lib.rs` owns invalidation; `build.rs` keeps its
  `rerun-if-changed` directives for cargo reload. Unrelated refactors
  no longer bust `target/.bbnf-cache/`.
- **33 → 4 test binaries.** `crates/ir/tests/` collapsed to four
  `main.rs`-rooted binary roots: `lattices/`, `passes/`, `egraph/`,
  `vm/`. Same 262 test count, ~75% fewer rustc/linker invocations.
- **Dev profile overrides.** `opt-level = 1` for `bbnf-ir`,
  `csp-solver`, `parse_that`; `codegen-units = 256` workspace-wide.
- **Bisect script.** `scripts/bisect-fastpath.sh` wraps `git bisect
  run` with per-step log capture under `/tmp/bisect-<short-hash>.log`.
- **Developer doc.** `docs/dev/build.md` codifies cache-bump policy,
  output-to-file discipline, iteration-time budgets.

Warm `cargo build -p bbnf --lib`: 0.19 s (target ≤ 20 s).
`cargo test -p bbnf-ir`: 22.2 s (target ≤ 60 s).

## AG.1 — FastPath regression resolution

The FastPath panic at `generate/regex/emit/mod.rs:38` fired whenever
the CSP had decided `RegexEngine::Dfa` for a pattern that would
classify as a fast path under default `EmitOpts`.

Root cause: `solve_regex_strategy` probed via
`emit_regex_direct_call(pattern)` (which hard-codes default opts)
while `emit_regex` passed the caller's real opts to
`emit_regex_fast_path`. The opts divergence surfaced after AF.3's
per-component CSP solve populated `ir.regex_engine_decisions`.

Fix: `emit_regex` now walks the tier ladder directly (fast path →
HIR → DFA → compile_error) instead of dispatching through the strategy
enum. `solve_regex_strategy` remains as a pure classifier running the
same predicates with the caller's real opts.

## AG.4 — Lowering + clean-regen fix + multi-hint @pretty

Three structural-mode lowering bugs and the CSS/Google Sheets parser
gate.

1. **Alt separator placeholders mistaken for alt branches.** The
   iteration filter now strips empty-span compounds and single-char
   `|`/`,` separators, so `grammar_item` lowers to `Alt[4]` instead
   of `Alt[9]`.
2. **`lower_mapped_factor` hard-coding `child(0)` as the factor.**
   Under the clean regen, `factor` is inlined into `mapped_factor` so
   children are `[big_comment?, term, modifier?, big_comment?,
   mapping?]`. Content-based classification replaces positional reads.
3. **Tape-elided leaf tokens.** When the parser consumes bytes for an
   identifier or literal without pushing a tape record, the new
   `lower_leaf_by_span_text_str` fallback recovers the leaf from the
   compound's span text after stripping the modifier and mapping.

The `dispatch_expression` leaf fast-path is gated by
`is_single_token_span` to prevent compound rule bodies from matching
as single literals (the literal rule's body starts and ends with `"`
across 3 alternation branches).

The hand-patched `generated.rs` receives a surgical
`trim_leading_whitespace_mut` injection inside the pretty_hint+ loop
so `@pretty blockContent block indent ;` parses. The clean regen
produces a correct `__grammar` body (`Repeat(OptionalWhitespace(
Ref(grammar_item)))`) but cannot replace the hand-patched file yet
because `host::extract_grammar` reads the pre-inline tape shape.
Deferred to AG.4b.

## AG.5 — CSP tier variable activation

Activated per-rule `EmissionTier` variables in the CSP solver and
wired the solved tiers into `ir.emission_tier`.

- **Per-rule tier variables.** `solve_component` now populates tier
  variables for every rule in the component, clamped by the rule's
  `MaterializationClass`: `MustTape` forces `Tape`, `TapeSpanOnly`
  permits `{Tape, Lazy}`, `TransparentElide` permits all three tiers.
- **`build_alt_domain` cost formula migration.** Dispatch cost
  formula unified: `arm_count * dispatch_branch + dispatch_table`.
  Legacy two-path cost model collapsed.
- **3-tuple return from `solve_grammar_components`.** Returns
  `(RecognizerDecisionMap, HashMap<NodeId, MaterializationClass>,
  HashMap<RuleId, EmissionTier>)`. `compile.rs` merges tier decisions
  into `ir.emission_tier`; `decode_emission_tier` runs as fallback for
  CSP-elided rules only.
- **10 tests un-ignored.** `cost_weights_unified` ignores that
  depended on tier variable infrastructure restored to active.

## AG.2, AG.3 — Deferred

Tier B emitter and view-layer DirectSlot have zero live consumers:
`decode_emission_tier`'s Tier B eligibility check
(TransparentElide + pure-conversion leaf) never fires on any
production grammar under the current constraint set. Infrastructure
is live; emitter activation deferred to Tranche AI.
