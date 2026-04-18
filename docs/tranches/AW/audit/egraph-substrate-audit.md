# E-graph Substrate Audit — AX.W6 vs Today

## Angle headline

"What of the e-graph forward approach — don't we have this?" **Yes, the substrate is largely in place.** The general-purpose e-graph crate plus the grammar-IR consumer expose every trait AX.W6 needs. Extraction is consumer-activated today: `write_back_optimized` mutates `ir.rules[].body`, so whatever the cost model picks after saturation feeds every downstream pass. What W6 proposes that is **genuinely new** is (a) five new `GrammarENode` leaf-ish variants, (b) eight additional `Rewrite` impls as sibling files of `rules/regex.rs`, and (c) a `classify_shape(egraph, root) -> ShapeTag` reader that replaces the hand-coded detectors in `crates/ir/src/passes/recognizers/shape_dispatch/`. Crucially, W6 is **not** a re-enactment of the 2026-04-08 deletion; that deletion retired a cross-rule cascading pipeline, not algebraic grammar rewrites, and the leaf-predicate guardrail on G5 is new.

## §1 — Today's `crates/egraph/` API surface

Files: `analysis.rs`, `cost_config.rs`, `cost_weights.rs`, `csp_scheduler.rs`, `eclass.rs`, `egraph.rs`, `extract.rs`, `id.rs`, `language.rs`, `lib.rs`, `rewrite.rs`, `scheduler.rs`, `unionfind.rs` plus the `egraph-derive` proc-macro crate.

Traits and public surface, all re-exported from `lib.rs`:

- `Language` (`language.rs:16`) — `children()`/`children_mut()`/`matches()`/`num_children()`/`is_leaf()`/`map_children()`; derivable via `egraph_derive::Language` with `#[language(skip)]`.
- `Analysis<N>` (`analysis.rs:18`) — `Data: Clone`, `make()`, `merge() -> bool`, `modify()`. `NoAnalysis` is the trivial impl.
- `Rewrite<N, A>` (`rewrite.rs:34`) — `name()`, associated `type Match`, `search() -> Vec<(Id, Match)>`, `should_apply()` (AA.5 analysis-guided gate), `apply()`. Blanket `RewriteFn` erasure for heterogeneous scheduler dispatch.
- `CostModel<N>` (`extract.rs:73`) — `type Cost: Lattice`, `cost(node, child_cost)`. `AstSize` scalar default; `Lattice for f64/usize` and `Scalar<T>` blanket impls.
- `Scheduler` (`scheduler.rs:43`) — `run(egraph, rules) -> RunReport`. Two concrete schedulers: `BackoffScheduler` (simple iter/growth/node cap) and `CspScheduler` (CSP-backed dirty-frontier via `csp-solver::LatticeDomain` + `ParentDirtyProp`).

Concrete types: `EGraph<N, A>` with `add`, `union`, `rebuild`, `classes()`, `class()`, `find()`, `total_nodes()`, `union_count()`, `with_capacity()`; `EClass { id, nodes, data, parents }`; `Extractor` with `best_cost()`/`best_node()`/`extract_tree()` (fixed-point greedy bottom-up); `DirtyDomain`, `ParentDirtyProp`, `CspScheduler`; shared `CostWeights` struct (structural, `alt_per_branch`, `dispatch_bonus`, `call_overhead`, `inline_body_size_penalty`, `tape_push`, `dispatch_branch`, `dispatch_table`, `prettify_emission`, `cross_module_coercion`) + `CALIBRATED_WEIGHTS` const; `CostConfig` with `egraph_iter_limit=64` and `egraph_node_limit=100_000`.

Capabilities: union, incremental rebuild, analysis-aware saturation with growth caps, per-class analysis facts, CSP-coupled dirty propagation, greedy extraction, lattice-typed cost for future Pareto. Multi-objective extraction is scaffolded (`Lattice::dominated_by`) but not wired.

## §2 — Today's `crates/ir/src/egraph/` surface

Module tree: `analysis/{mod,facts}.rs`, `rules/{mod,regex,suffix}.rs`, `build_egraph.rs`, `cost.rs`, `interner.rs`, `node.rs`, `write_back.rs`.

`GrammarENode` variants (`node.rs:23`): `Literal(StringId)`, `Regex(StringId)`, `Epsilon`, `Ref(RuleId)`, `Seq(Box<[Id]>)`, `Alt(Box<[Id]>, Option<AltDispatch>)`, `Repeat { inner, lo, hi }`, `Skip([Id; 2])`, `Next([Id; 2])`, `Minus([Id; 2])`, `Negate(Id)`, `OptionalWhitespace(Id)`, `Map { inner, fn_id }`, `TokenDispatch { token, arms (skip), fallback (skip) }`.

`EClassFacts` (`analysis/facts.rs:84`): `first_set: CharSet128`, `nullable: bool`, `width: WidthBound{min,max}`, `literal_sid: Option<StringId>`, `regex_sid: Option<StringId>`, `elision_safe: bool`, `closure_free: bool`, `is_fixed_shape: bool`, `all_descendants_elidable: bool` — all monotone, all populated bottom-up by `GrammarAnalysis::make` for every variant.

`GrammarCostModel` (`cost.rs`): embeds `CostWeights`, adds `literal_cost`, `regex_cost`, `ref_cost`, `seq_per_child`, plus a raw `FnDescriptor` table for Map-cost precision; `from_config(CostConfig)` is the gestalt entry.

Rewrite rules live today (`rules/`):
- `rules/regex.rs`: `DeduplicateAltBranches`, `SupersetAbsorbAlt` (via `bbnf_regex::algebra::pattern_is_superset`), `UnionMergeAlt` (via `try_union_patterns`), `FuseAltRegexBranches` (literal+regex → fused pattern).
- `rules/suffix.rs`: `CommonSuffixFactor` (Tranche Y.11 — dual of prefix-factoring).

Saturation entry: `build_and_saturate(&ir) -> (EGraph, SharedStrings, FxHashMap<RuleId, Id>)` at `mod.rs:60`. Pre-sizes by `count_nodes` sum. Calls `default_rules()`, runs `CspScheduler::from_config`, emits a `BBNF_EGRAPH_REPORT=1` per-rule attribution line. Extraction consumer: `write_back_optimized` (`write_back.rs:34`) uses `Extractor::new(egraph, cost)`, walks each rule root, rebuilds `IrNode` respecting rule boundaries (emits `Ref(rule_id)` instead of inlining across rules), mutates `ir.rules[].body` in place. Wired from `crates/core/src/pipeline/compile.rs:591-603` inside the `egraph_build_saturate_writeback` timer span, immediately after `hoist_recurring_patterns`.

## §3 — Deletion archaeology (commit bfa50f25, 2026-04-08)

Deleted files and what they held:
- `rules/inline.rs` (223 LOC) — `InlineEligibleRef` union-of-criteria rewrite: `Ref(id) ≡ <body>` when acyclic+small (≤ threshold) or single-use, non-entry, non-`preserve_identity`.
- `rules/normalize.rs` (455 LOC) — `EliminateEpsilon`, `UnwrapSingletonSeq`, `EliminateEpsilonInAlt`, `EliminateEpsilonInRepeat`, `EliminateEpsilonInSkipNext`, `MergeLiterals`, `UnwrapSingletonAlt`.
- `rules/prefix.rs` (430 LOC) — `FactorSharedSeqPrefix`, `FactorLiteralByteTrie` (byte-level trie splitting for dispatch).
- `rules/structural.rs` (122 LOC) — `CanonicalizeAlias` + `build_alias_map`.

Failure mode named in the commit: **"the normalizer's cross-rule cascading (inline→merge→factor→inline) architecturally cannot be expressed in one-pass saturation."** The rules were deleted because they enacted a pipeline whose value depended on an iterated fixed-point over mutated tree shapes. The imperative normalizer (`passes/transform/*`, `passes/regex/*`, `passes/prefix.rs`) continues to do that cascading; the e-graph was demoted to "permanent secondary" running once on normalized IR. No live code references the deleted types — `default_rules()` was reduced in the same commit.

## §4 — Feasibility table for AX.W6 rewrites

| Rule | Implementable today on `GrammarENode` + `Rewrite`? | Needs new e-node variant? | Precondition mineable from `EClassFacts` today? | Re-enacts a 2026-04-08-deleted pattern? |
|---|---|---|---|---|
| **G1 Alt-flatten** `Alt([Alt(…), x]) ≡ Alt([…, x])` | Yes — pure shape match on `Alt(Box<[Id]>)` | No | No preconds | No (deleted set had no Alt-assoc) |
| **G2 Seq-flatten** `Seq([Seq(…), x]) ≡ Seq([…, x])` | Yes — mirror of G1 on `Seq` | No | No preconds | No |
| **G3 KwWs fusion** `Seq([Literal, OW(Epsilon)]) ≡ KwWs` | Yes, once variant added | **Yes — `KwWs(StringId)`** | `facts.literal_sid` + width bound | No (deleted never fused OW) |
| **G4 PHF-dispatch inference** `Alt([Lit,…,Lit]) ≡ PhfDispatch(…)` | Yes | **Yes — `PhfDispatch(Box<[StringId]>)`** | `literal_sid` per child + width bound | Partial overlap with `FactorLiteralByteTrie` but PhfDispatch is a *leaf*, not a trie rewrite — different shape |
| **G5 Ref-to-leaf inline** `Ref(id) ≡ body` when `facts.all_descendants_elidable ∧ body is leaf ∧ ref_count>1` | Yes | No | `facts.all_descendants_elidable`, `literal_sid`, `regex_sid` cover the leaf-ness; `ref_count` is grammar meta, not in facts — must be passed as rule constructor data (same pattern as pre-deletion `InlineEligibleRef`) | **Yes — same fault line as deleted `InlineEligibleRef`.** The guardrail that's new: the precondition is `body extracts to a leaf`, not "body ≤ N nodes". Bounded by construction — inlining a leaf adds ≤ 1 e-node. |
| **G6 PhfLoop** `Repeat { Alt([Lit,…,Lit]) } ≡ PhfLoop` | Yes | **Yes — `PhfLoop { sids: Box<[StringId]>, lo, hi }`** | per-child `literal_sid` | No |
| **G7 ClassifyByteLoop** `Repeat { Alt_disjoint_first } ≡ ClassifyByteLoop` | Yes | **Yes — `ClassifyByteLoop { ... }`** | per-child `first_set` + `is_disjoint` check | No |
| **G8 OperatorChain** nested `Seq/Alt` chain ≡ `OperatorChain` | Yes but the match is deepest and stateful | **Yes — `OperatorChain { ranges, ops }`** | `width`, `operator_chain::collect_operator_chains` miner exists | No |
| **G9 HeadwordFolding** `Alt([Seq(L, a), Seq(L, b), …]) ≡ Seq(L, Alt([a, b]))` | Yes — mirror of `CommonSuffixFactor` | No | None | **Conceptual overlap with `FactorSharedSeqPrefix`** (deleted prefix.rs). The shape is almost identical. Guardrail: like `CommonSuffixFactor`, fire only when every branch is Seq with matching lead class; the deleted prefix version was paired with cascading inlining. Standalone it should be safe (CommonSuffixFactor is live). |

## §5 — Consumer activation audit

Extraction **is** consumer-activated today via `write_back_optimized`. The `Extractor` runs; the best e-node per class walks back into `ir.rules[].body`; every downstream pass (`shape_dispatch`, `recognizers`, `classify_materialization`, emission) reads that IR. Concretely: after `egraph_build_saturate_writeback` returns, the next pipeline span rebuilds the DAG (`timer.span("build_dag", …)`) and facts are recomputed on the extracted IR. So the rewrites' output already drives runtime code today — just not in a way that decides shape classification.

What W6 proposes to add to the consumer: a new function `classify_shape(egraph, root_id) -> ShapeTag` (named `extract_shape_tag` in the research doc) living in `shape_dispatch/mod.rs`. It reads the class's extracted best node, checks which new variant it is (`KwWs`, `PhfDispatch`, `OperatorChain`, `PhfLoop`, `ClassifyByteLoop`), maps to `ShapeTag`. This replaces the 779 LOC of per-shape detectors in `shape_dispatch/{object,array,string,number,keyword,scalar,pratt,arglist,flat,hregex,wrap,unordered}.rs` (actual total by `wc -l`: 1676 LOC across 13 files) with roughly 150 LOC of tag dispatch.

## §6 — Verdict

**Largely reinvention at the substrate layer, genuine at the rule layer.** The user's instinct is right: the e-graph *forward* approach is already built. Traits (`Language`, `Rewrite`, `Analysis`, `CostModel`, `Scheduler`), concrete types (`EGraph`, `EClass`, `Extractor`, `CspScheduler`, `BackoffScheduler`, `CostWeights`), per-class facts (`EClassFacts`), cost model (`GrammarCostModel`), CSP coupling (`csp_scheduler.rs` → `csp-solver::LatticeDomain`), and consumer activation (`write_back_optimized` → `ir.rules[].body`) all exist and all fire every compile. AX.W6 does **not** need to build scaffolding; every W6 rewrite is a sibling file of `rules/regex.rs` / `rules/suffix.rs` at roughly 50 LOC each plus a `GrammarCostModel::cost` arm.

Categorisation of W6 rewrites against the substrate:

- **Already implementable with ≤ 50 LOC, zero substrate change**: G1 Alt-flatten, G2 Seq-flatten, G9 HeadwordFolding. These match on existing variants only.
- **Need one new `GrammarENode` variant each, no substrate change**: G3 (`KwWs`), G4 (`PhfDispatch`), G6 (`PhfLoop`), G7 (`ClassifyByteLoop`), G8 (`OperatorChain`). Each adds one variant (derive auto-generates `Language`), one `GrammarCostModel` arm, one `GrammarAnalysis::make` arm for facts, and — critically — **one lowering route in every backend emitter**. The last item is the actual work; the e-graph plumbing is trivial.
- **G5 Ref-to-leaf inline is the risk**: structurally identical to the deleted `InlineEligibleRef`. The new guardrail (`extracted body is a leaf`) is tighter than the deleted `body ≤ INLINE_SIZE_THRESHOLD` bound, and it composes with `egraph_node_limit=100_000`. Edge cases the guardrail may not fully cover: a `Ref` whose body extracts to `KwWs` (newly a "leaf") could cascade through G1/G2 flattening if the inlinee sits inside an `Alt` or `Seq`. Combined with a rewrite rule like G9 that also fires under Seq rearrangement, there's a confluence risk. The research doc acknowledges "G5 is non-confluent with G1" and relies on cost-model determinism to pick a unique representative per class. This is sound *if* saturation terminates — and the `iter_limit=64` cap guarantees that. So the failure mode is *not* re-introduced; it's *bounded*.

**The architectural advance W6 delivers is not the e-graph — it's the rewrite set plus extraction-as-classifier.** The forward approach exists; what was missing was the grammar-level algebraic rule set and the reader that turns extraction into `ShapeTag`. W6 ships both. The user is right to sense reinvention at the substrate level: there is effectively none. The real work is the eight rewrite files, the variant additions, the emitter routes, and the detector retirement. Substrate-with-consumer closes in one wave because `write_back_optimized` is already the consumer. Net recommendation: accept W6α + W6β as planned; flag G5 for explicit commit-time test of the "inlinee inside Alt/Seq under G9" confluence case; verify the five new variants have emitter routes before W6β hard gate (else the rewrites land but extraction has nothing to lower to).
