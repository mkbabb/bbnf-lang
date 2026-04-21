# AYW-egraph-cost-firing — E-graph rule firing + cost model audit

**Scope.** AY-audit-3: inventory every registered e-graph rule (grammar tier + HIR/regex tier), measure its firing rate on every production grammar, verify the cost model is pluggable + shared across tiers, and render a unification proposal for the regex-tier and grammar-tier e-graphs.

**Method.** Compiled all six production grammars (JSON, EBNF, BNF, BBNF, Google Sheets, CSS L4) through `bbnf::pipeline::compile_grammar_request` / `compile_paths_request` with `BBNF_EGRAPH_REPORT=1` and `BBNF_HIR_EGRAPH_REPORT=1`, capturing the scheduler's per-rule `total_work` (node-adds + union-calls). Raw report: `/tmp/fire-stderr-clean.txt` (post-cache-clear). Reproducer: `crates/core/examples/egraph_fire_probe.rs`.

---

## § 1. E-graph rule inventory

### Grammar-tier — `crates/ir/src/egraph/rules/`

Registered in `default_rules()` at `crates/ir/src/egraph/rules/mod.rs:76-100`. Nine rules.

| # | Rule | File:line | Pattern detected | Rewritten form | Precondition |
|---|---|---|---|---|---|
| 1 | `DeduplicateAltBranches` | `rules/regex.rs:49` | `Alt` with two children sharing same canonical e-class id | `Alt` with duplicates removed | none |
| 2 | `SupersetAbsorbAlt` | `rules/regex.rs:114` | `Alt` with one branch regex being a strict pattern-superset of another | `Alt` with subsumed branches dropped | both branches must be `Regex(sid)`; `bbnf_regex::algebra::pattern_is_superset` must return true |
| 3 | `UnionMergeAlt` | `rules/regex.rs:204` (impl at `:377`) | `Alt` whose two branches have mergeable regex char classes | `Alt` with merged regex in place of both | both branches `Regex(sid)`; `bbnf_regex::algebra::try_union_patterns` returns `Some(merged)` strictly shorter than the sum |
| 4 | `FuseAltRegexBranches` | `rules/regex.rs:239` | `Alt` where all branches are `Regex` or `Literal`, no `AltDispatch`, ≥1 `Regex` | `Regex` node carrying the combined `a|b|c` pattern | `dispatch.is_none()`, all branches resolve to regex/literal, at least one regex |
| 5 | `CommonSuffixFactor` | `rules/suffix.rs:39` | `Alt` whose every branch is a `Seq` of length ≥2 sharing the same canonical last child | `Seq([Alt(heads), shared_tail])` | every branch must be `Seq` with ≥2 children; all branches' last canonical e-class equal |
| 6 | `AltOfSingle` (G1) | `rules/universal.rs:32` | `Alt([x])` with exactly one child | `x` (union with sole child) | `children.len() == 1` |
| 7 | `RepeatOfSingle` (G2) | `rules/universal.rs:79` | `Repeat { lo:1, hi:1, inner }` | `inner` | `lo == 1 && hi == 1` |
| 8 | `WrapOfEpsilonScalar` (G3) | `rules/universal.rs:144` | `Alt([leaf, Epsilon])` where leaf's class projects to a scalar `TypeDesc` | `leaf` (union with scalar branch) | `Alt` has exactly two branches; one contains `Epsilon`; the other is `Literal`/`Regex` or a `Ref` whose `TypeDesc` is scalar |
| 9 | `ConcatLiterals` (G4) | `rules/universal.rs:278` | `Seq` containing ≥1 adjacent run of ≥2 children whose canonical classes each carry a `Literal` | `Seq` with each run fused into a single `Literal` (interned via `SharedStrings`) | rebuilt child list must differ from original |

### HIR/regex-tier — `parse-that/rust/regex/src/egraph/rules/`

Registered in `default_hir_rules()` at `parse-that/rust/regex/src/egraph/rules/mod.rs:64-73`. Five rules.

| # | Rule | File:line | Pattern detected | Rewritten form | Precondition |
|---|---|---|---|---|---|
| 1 | `FlattenAltConcat` | `rules/flatten.rs:19` | `Alternation` child-class contains another `Alternation` node; or `Concat` child-class contains another `Concat` | Flat `Alternation` / `Concat` with inner children hoisted | at least one nested child |
| 2 | `DeduplicateAlternation` | `rules/redundant.rs:17` | `Alternation` with two children sharing canonical id | deduped children | at least one duplicate |
| 3 | `SupersetAbsorbClass` | `rules/superset.rs:21` | `Alternation` with two `Class` branches where one byteset is a strict superset of another | `Alternation` with subsumed branches dropped | both branches must project to non-negated `ByteSet`; `algebra::is_superset` returns true |
| 4 | `UnionMergeClass` | `rules/union.rs:20` | `Alternation` with two `Class` branches whose bytesets can be merged into a strictly smaller class | `Alternation` with merged `Class` in place of both | bytesets ≠, `try_union` returns a non-trivial merge |
| 5 | `AbsorbRepetition` | `rules/repetition.rs:25` | `Concat` with two adjacent `Repetition` children sharing canonical `sub` e-class + matching greedy flag | `Concat` with a single merged `Repetition { min: m1+m2, max: add(max1,max2), ... }` | canonical equality of `sub`; greedy flags match |

**Substrate totals:** 14 registered rules across 1,837 LOC of rule files (9 grammar-tier rules in 1,126 LOC; 5 HIR-tier rules in 711 LOC including shared `util.rs`).

---

## § 2. Firing evidence per production grammar

Each grammar's compile emits one grammar-tier saturation report plus one HIR-tier report per unique regex pattern. The matrix below aggregates the `total_work` counter from the scheduler's `RunReport.per_rule` field (defined at `crates/egraph/src/scheduler.rs:34-39`, incremented inside both `BackoffScheduler::run` and `CspScheduler::run`).

### Matrix — grammar-tier e-graph (work per grammar)

| Grammar | dedupAlt | supersetAlt | unionMerge | fuseAltRegex | suffixFactor | altOfSingle | repOfSingle | wrapOfEps | concatLit |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| JSON | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| EBNF | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| BNF | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| BBNF | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| Sheets | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| **CSS L4** | 0 | 0 | 0 | **4** | 0 | 0 | 0 | 0 | 0 |

### Matrix — HIR-tier e-graph (sum over all `simplify_hir` calls per grammar)

The HIR tier runs once per unique regex pattern; each grammar triggers 1-13 HIR saturations (JSON: 2, CSS L4: 13, Sheets: 9, BBNF: 11). Per-grammar totals across **all** HIR saturations:

| Grammar | flatten | dedupAlt | supersetClass | unionMerge | absorbRep |
|---|---:|---:|---:|---:|---:|
| JSON | 0 | 0 | 0 | 0 | 0 |
| EBNF | 0 | 0 | 0 | 0 | 0 |
| BNF | 0 | 0 | 0 | 0 | 0 |
| BBNF | 0 | 0 | 0 | 0 | 0 |
| Sheets | 0 | 0 | 0 | 0 | 0 |
| CSS L4 | 0 | 0 | 0 | 0 | 0 |

### Aggregate firing

| Tier | Registered | Non-zero on any grammar | Zero everywhere |
|---|---:|---:|---:|
| Grammar | 9 | 1 (`fuse-alt-regex-branches`, CSS L4 only, 4 units) | **8** |
| HIR | 5 | 0 | **5** |
| **Combined** | **14** | **1** | **13** |

### Dead-rule flags (invariant-2 "substrate-without-consumer" violations)

Thirteen of 14 retained rules have **zero `check()` matches** across every production grammar. They are carried in the saturation loop — each `search()` walks the full class list every iteration — yet produce no equivalences anywhere the grammars cross. The HIR tier is entirely dead; every `simplify_hir` saturation runs to its `saturated: true` fixed point on iteration 1 with `applied: 0`.

Flagged as substrate-without-consumer:

- Grammar-tier: `DeduplicateAltBranches`, `SupersetAbsorbAlt`, `UnionMergeAlt`, `CommonSuffixFactor`, `AltOfSingle`, `RepeatOfSingle`, `WrapOfEpsilonScalar`, `ConcatLiterals`.
- HIR-tier: `FlattenAltConcat`, `DeduplicateAlternation`, `SupersetAbsorbClass`, `UnionMergeClass`, `AbsorbRepetition`.

Sole survivor: `FuseAltRegexBranches`, 4 fires on CSS L4.

### Why this happens — diagnosed

The structural normalizer loop (`crates/core/src/pipeline/compile.rs:510-569`) runs **before** the e-graph and executes `canonicalize_aliases`, `inline_acyclic`, `fuse_single_use`, `eliminate_epsilon`, `merge_literals`, and `factor_common_prefixes` to fixed point. By the time the egraph sees the IR, the normalizer has already:

- Collapsed single-branch `Alt`s → eliminates G1 `AltOfSingle` residue.
- Eliminated epsilon-only branches → eliminates G3 `WrapOfEpsilonScalar` matches.
- Merged adjacent literals → eliminates G4 `ConcatLiterals` matches.
- Factored common prefixes → eliminates most prefix-style factoring candidates; symmetrically, `CommonSuffixFactor` almost never matches because `Alt` branches that would share suffixes have already been hoisted into common prefix form (and the Alt structures that remain don't have uniform `Seq` shape).
- Deduped alt branches by structural hash → eliminates `DeduplicateAltBranches`.

The HIR tier never fires because every regex pattern in the production grammars parses to a simple enough HIR that the `needs_saturation` fast-path (`parse-that/rust/regex/src/egraph/mod.rs:197-204`) short-circuits most, and the ones that do run contain no `Alternation` with duplicate / supersetting children, no nested `Alt`/`Concat` that the parser doesn't already flatten, and no adjacent `Repetition` pairs (the HIR parser lifts repetition into the `Repetition` wrapper directly; back-to-back repetition comes only from codegen-generated synthetic patterns that the grammar pipeline never emits).

`FuseAltRegexBranches` is the sole survivor because it matches a grammar-tier shape (sibling `Regex` branches inside an `Alt` with no `AltDispatch`) that the normalizer does NOT fuse. CSS L4's `value-unit` family emits this shape in four locations.

---

## § 3. Cost model analysis

### Pluggable — yes, partially

The cost model is architecturally pluggable:

- `egraph::CostModel<N>` trait (`crates/egraph/src/extract.rs:73`) — type-parameterized over `Cost: Lattice`, per-node cost function with `child_cost` closure. Any consumer can implement it.
- `egraph::CostWeights` (`crates/egraph/src/cost_weights.rs:40`) — shared substrate embedded by every domain-specific model. Holds ten knobs (`structural`, `alt_per_branch`, `dispatch_bonus`, `call_overhead`, `inline_body_size_penalty`, `tape_push`, `dispatch_branch`, `dispatch_table`, `prettify_emission`, `cross_module_coercion`).
- Grammar-tier: `GrammarCostModel` (`crates/ir/src/egraph/cost.rs:25`) embeds `CostWeights`, layers `literal_cost` + `regex_cost` + `ref_cost` + `seq_per_child`.
- HIR-tier: `RegexExtractionCost` (`parse-that/rust/regex/src/egraph/cost.rs:18`) embeds `CostWeights`, layers `literal_per_byte` + `class_cost` + `repeat_cost` + `merged_bonus`.
- `CostConfig::from_env` reads `BBNF_COST_LITERAL` / `BBNF_COST_REGEX` / `BBNF_COST_REF` / `BBNF_COST_SEQ_PER_CHILD` / `BBNF_COST_HIR_*` / `BBNF_COST_MAT_*` + (from `CostConfig`) `BBNF_COST_DISPATCH_BONUS`, so every knob is tunable.

Not fully pluggable:

- `Pattern registry` is not modelled as a pluggable decision point — rules are hand-coded structs with `Rewrite<N, A>` impls; there is no rule-list DSL or external `default_rules()` override.
- `Rewrite rules` themselves are pluggable (the `RewriteFn` trait is dyn-compatible, consumers pass arbitrary `&[&dyn RewriteFn]` to the scheduler), but the `default_rules()` factory is the only production call site, and no downstream consumer rebinds it.

### What does it cost?

Both models cost `CostWeights::structural` per node (default `1.0`) plus domain knobs. Representative formulas:

- Grammar `Alt([a, b, c], Some(dispatch))` = `structural + 1.5 * 3 + c(a)+c(b)+c(c) - 2.0` (dispatch bonus).
- Grammar `Seq([a, b])` = `structural + 1.0*2 + c(a)+c(b)`.
- Grammar `Literal` = `1.0`, `Regex` = `2.0`, `Ref` = `0.5`, `Epsilon` = `0.5`.
- HIR `Alternation([...])` = `structural + 1.5*n + Σ c(ci)`.
- HIR `Literal(bytes)` = `structural + 0.25 * bytes.len()`.
- HIR `Class(_)` = `structural + 1.5`.
- HIR `Repetition { sub }` = `structural + 1.0 + c(sub)`.

The cost unit is **abstract**, not microseconds. Both tiers combine (a) node count (via `structural`), (b) structural-shape penalty (alt_per_branch), (c) shape-dispatch bonus (dispatch_bonus), (d) domain-specific per-leaf weights. None of the weights are calibrated against wall-clock parse time — the `CALIBRATED_WEIGHTS` const at `crates/egraph/src/cost_weights.rs:177` is explicitly labelled a **null-result** from the AW-IV.W5.3 sweep because "the cost model has no choice to exercise" given the current rewrite set.

### Does cost guide extraction toward a better result?

In principle yes (demonstrated by `parse-that/rust/regex/tests/egraph_simplify.rs::simplify_hir_preserves_structure`, which expects `Alt([[a-c], [d-f]])` → `[a-f]` exclusively via cost-guided extraction after `UnionMergeClass` installs the merged form). In practice on production grammars: **no measurable effect**, because 13 of 14 rules never add a second node to any class. Extraction is effectively a best-node-per-class identity over the initial insertion (the sole non-identity case is CSS L4's 4 fused-regex classes, where the fused form is strictly cheaper than the 2-3-branch `Alt` it replaces).

### Regression test for cost-guided extraction vs greedy?

No. `crates/ir/tests/egraph/egraph_grammar.rs` exercises only per-rule search/apply assertions on synthetic IR; `egraph_universal.rs`, `egraph_suffix.rs` do the same; `egraph_roundtrip.rs` checks write-back preserves semantics; `egraph_analysis.rs` exercises `EClassFacts`. None compares cost-guided vs AstSize extraction on any grammar. No test gates the null-result finding against regression. If the retained rules ever did fire, their cost-model bias would be verified only by end-to-end parse correctness, not by cost-ordered extraction invariants.

---

## § 4. Regex × e-graph cost unification

### Does regex have its own cost model?

Yes. `RegexExtractionCost` at `parse-that/rust/regex/src/egraph/cost.rs:18`. It embeds the same `CostWeights` substrate, so `structural`, `alt_per_branch`, and `dispatch_bonus` are already unified. Domain knobs (`literal_per_byte`, `class_cost`, `repeat_cost`, `merged_bonus`) are regex-specific.

The two models are therefore **partially unified**: the cross-cutting weights are one source of truth; the per-leaf multipliers are per-tier. This split is correct — "how expensive is one byte in a literal" is a regex concept and has no grammar-tier analogue. The unification-in-substrate approach in `cost_weights.rs:12-25` (the ASCII-art diagram) is architecturally sound.

### Are regex optimizations expressible as e-graph rewrites?

Partially:

- ✓ **Superset absorption** (`SupersetAbsorbClass`) — already an e-graph rewrite.
- ✓ **Char-class union** (`UnionMergeClass`) — already an e-graph rewrite.
- ✓ **Alt flattening** (`FlattenAltConcat`) — already an e-graph rewrite.
- ✓ **Alt dedup** (`DeduplicateAlternation`) — already an e-graph rewrite.
- ✓ **Repetition absorption** (`AbsorbRepetition`) — already an e-graph rewrite (and the canonical equality-saturation win the user's AA research prototype singled out).
- ✗ **NFA→DFA subset construction** (`parse-that/rust/regex/src/automata/dfa.rs:218`) — a fundamentally different substrate. Subset construction maps HIR states → DFA state-sets over alphabet classes, operating on an automaton, not on a term tree. E-graph rewrites operate on term equality; powerset is a graph transformation that doesn't have a "left-hand-side = right-hand-side" shape.
- ✗ **Hopcroft DFA minimization** (`parse-that/rust/regex/src/automata/dfa.rs:291`) — partition-refinement over DFA states. Not expressible as a pattern-match-and-rewrite on term trees.
- ✗ **FIRST-set / FOLLOW-set** (`parse-that/rust/regex/src/first.rs`, `crates/ir/src/passes/follow_sets.rs`) — dataflow over the grammar/NFA, not a term rewrite. The grammar-tier `GrammarAnalysis::EClassFacts` already embeds `first_set: CharSet128` as an abstract-interpretation value; that IS the e-graph analogue of FIRST-set, and it's already unified (Tranche AA.2).
- ✗ **Dead-state elimination** — partition-refinement over DFA reachability; not a term rewrite.
- △ **Literal prefix / suffix extraction** (`parse-that/rust/regex/src/info/literal_prefix.rs`) — expressible as a term rewrite (`Concat([Literal(l), rest])` → "pattern with literal prefix `l`") but the consumer is the DFA codegen, not an IR-level rewrite; the extraction is a read-only query, not an equivalence.

**Verdict:** The e-graph is the right substrate for algebraic rewrites (distributivity, commutativity on `Alt`, idempotence on `Repetition`). It is the wrong substrate for automaton-level passes (powerset, minimization, reachability). The current split — e-graph owns HIR algebra, bespoke passes own DFA construction — is correct.

### Could regex patterns + grammar IR nodes coexist in ONE e-graph?

Technically yes — a sum-type e-node `enum UniNode { Grammar(GrammarENode), Regex(HirENode) }` plus bridging rewrites (`Grammar::Regex(sid) ↔ Regex::*`) would give the framework a single e-graph. But the payoff is near-zero and the cost is nontrivial:

- Current call order: grammar-tier `FuseAltRegexBranches` uses `SharedStrings` to build a regex **as a string**, re-parsed later by the regex HIR parser. A unified e-graph would let the grammar rule produce a `Regex::Alternation([...])` node directly, skipping the round-trip.
- Counter: `FuseAltRegexBranches` fires 4 times total across all 6 production grammars. The amortized cost of re-parsing 4 strings is negligible.
- The grammar tier runs at compile-time per grammar; the HIR tier runs at compile-time per regex pattern. A unified e-graph would force both scans over the combined substrate at every saturation step, paying `O(grammar_nodes + regex_nodes)` per rule per iteration. Current decomposition pays `O(grammar_nodes)` for grammar rules and `O(regex_nodes)` for HIR rules separately.
- The two tiers already share: `egraph` crate (substrate, scheduler, extractor), `CostWeights` (shared knobs), `egraph_derive::Language` (derivation).

**Recommendation:** Keep the tiers separate. The `isomorphic-api` / `regex-crate-isomorphic` invariants are met: both tiers use the same substrate + the same shared weights. Further unification would collapse a correct separation of concerns (intra-regex rewrites vs cross-regex-inside-grammar rewrites) into one monolithic e-graph with no measurable gain.

---

## § 5. Extraction path audit

### Algorithm — greedy bottom-up fixed-point

`crates/egraph/src/extract.rs:122-158` — `Extractor::compute_best`:

```text
loop:
  changed = false
  for each class c in egraph.classes():
    for each node n in class.iter():
      if !all children of n have a resolved best-cost: skip
      cost = cost_model.cost(n, |cid| self.best[find(cid)].0)
      if cost < self.best[canonical_of(c)].0:
        self.best[canonical_of(c)] = (cost, n.clone())
        changed = true
  if !changed: break
```

This is the **Egg greedy bottom-up** algorithm (cited explicitly in the module doc). It picks the cheapest e-node per class, using current children's best costs. Iterates until no class's best improves.

### Determinism

Deterministic on a fixed iteration order: `egraph.classes()` (backed by a union-find vector walk; ordering is insertion-then-union-driven). Once the union-find settles and rule scheduling stops, the extraction is a pure function of `(egraph, cost_model)`. Tests run green across reruns.

### Confluence

Not globally confluent. The greedy algorithm is **monotone** for monotone cost models (where `cost(n, c1..cn)` is monotone non-decreasing in every child cost), which both `GrammarCostModel` and `RegexExtractionCost` are: every child contributes additively or with a fixed non-negative multiplier. Under monotonicity, greedy bottom-up converges to the single best cost per class; there is no "different iteration order gives different answer" ambiguity.

For cost models that depend on sharing (same subtree appearing twice — cheaper if counted once), greedy is **not** optimal. The extractor's module doc explicitly calls this out (`extract.rs:9-11`): "It's not optimal for all cost models (e.g., cost models that depend on sharing require a more sophisticated extractor)." Neither `GrammarCostModel` nor `RegexExtractionCost` exploits sharing; both are strictly additive over children. So greedy is locally optimal for the current cost models.

### Invariant broken when cost goes up during rewrite

Cost monotonicity requires that no rewrite INCREASES the extractable cost of its class. With non-destructive rewrites this is automatic: the original form stays in the class; `min(original, rewritten)` dominates, so the class's best-cost is monotonically non-increasing as more equivalences land.

If a rewrite's `apply()` unions two classes whose pre-union best-costs differ, the post-union class inherits `min(a, b)`. No invariant is broken.

**Potential failure modes** (none observed in this audit):

1. A non-monotone cost model (e.g., a cost that rewards a specific child combination) — `WrapOfEpsilonScalar::is_scalar` almost trips this because it rewards scalar projections, but the reward is delivered via **removing** the outer `Alt` (union with the leaf), and leaf nodes are always cheaper than their wrapping `Alt`, so the union is always cost-reducing.
2. A cost depending on non-canonicalized child ids — the extractor's `find_ref(c)` normalization in the cost closure eliminates this. Current models cannot observe pre-union vs post-union ids.
3. Child-cost resolution order — the fixed-point loop retries until no class improves; as long as the DAG has no cost-cycle, it terminates with every reachable class at its min.

No cost regression is assert-gated. A dedicated regression test (§3) would close this surface.

---

## § 6. G5-G9 per-shape rewrites — deferred?

AY.W2.4 scheduled five per-shape rewrites (Pratt, Wrap, Flat, HRegex, AltDispatch) to subsume the hand-coded shape detectors in `crates/ir/src/passes/recognizers/shape_dispatch/` (~644 LOC across five detector files). **None landed.** `default_rules()` contains no such rules; no `G5`/`G6`/`G7`/`G8`/`G9` construct exists anywhere in the codebase.

Per-shape firing prognosis (based on the detector pattern + observed grammar IR shapes):

| Rule | Detector subsumed | Pattern | Would fire on | Verdict |
|---|---|---|---|---|
| **G5** Pratt simplification: single-rung Pratt → Flat | `recognizers/shape_dispatch/pratt.rs` (128 LOC) | Pratt operator-precedence body with exactly one binding tier | No production grammar today (JSON/BNF/EBNF/BBNF have no Pratt; Sheets has multi-rung Pratt; CSS has no Pratt). Future grammars may. | **Retire now; re-add if/when a single-rung Pratt grammar lands.** |
| **G6** Wrap + epsilon-only-tail → leaf | `recognizers/shape_dispatch/wrap.rs` (72 LOC) | Same shape as G3 (`WrapOfEpsilonScalar`) but with a non-scalar inner body — the detector's scope is broader | G3 is the scalar case; G6 would match `Wrap { Alt([body, ε]) }` where `body`'s projection is non-scalar (e.g. a `Vec<_>`). This shape DOES appear in grammar IR (JSON array-body with optional trailing comma pattern uses exactly this shape post-normalizer). | **Defer; bundle with G3 as `WrapOfEpsilon` with scalar/composite branches.** |
| **G7** Flat + single-Ref → Scalar | `recognizers/shape_dispatch/flat.rs` (240 LOC) | Flat shape whose sole non-epsilon child is a `Ref` | After structural normalizer, this form collapses via `inline_acyclic` already. Post-normalizer IR has no `Flat-of-single-Ref` shape. | **Retire as obsolete** — structural normalizer subsumes the rewrite. |
| **G8** HRegex + context-insensitive → regex primitive | `recognizers/shape_dispatch/hregex.rs` (68 LOC) | Heterogeneous-regex shape whose outer context carries no dispatch predicate | This shape exists (CSS L4 keyword dispatchers mix regex + literal branches). But `FuseAltRegexBranches` already covers the same substrate when `AltDispatch::is_none()`. | **Retire as duplicate of `FuseAltRegexBranches`.** |
| **G9** AltDispatch + first-byte-disjoint → byte-dispatch table | `recognizers/shape_dispatch/alt_dispatch.rs` (136 LOC) | `Alt` whose branches have disjoint first-byte sets | **Would fire on every `Alt` in every grammar.** The `disjoint_first` pass at `crates/ir/src/passes/recognizers/disjoint_first.rs` already emits this decision; as an e-graph rewrite, it'd re-compute it. | **Redundant as a rewrite; keep as a detector.** The dispatch-table decision is an attribute of a node, not an equivalence of nodes. |

**Summary for G5-G9:**

- G5 (Pratt): insurance for future grammars — land only when a single-rung Pratt grammar exists.
- G6 (Wrap-of-ε composite): valuable — bundle as G3 extension.
- G7 (Flat-single-Ref): obsolete — normalizer subsumes.
- G8 (HRegex context-insensitive): obsolete — `FuseAltRegexBranches` already covers.
- G9 (AltDispatch): not a rewrite — attribute decision stays in the detector.

**Net:** of the five planned G5-G9 rewrites, only G6 (as a G3 extension) has non-trivial consumer coverage. The other four are either insurance or obsolete. AY.W2's planned "detector retirement via G5-G9 subsumption" is not achievable — the detectors' logic is not rewrite-shaped, and the structural normalizer already subsumes most of the rewrite-shaped subset.

---

## § 7. Recommendations

Ordered by impact × simplicity. All are read-only-audit findings; none of this is a plan commitment.

### R1. Move the G1/G2/G3/G4/CommonSuffixFactor rules inside the structural normalizer loop — or retire them.

**Evidence:** Zero fires across six production grammars. The structural normalizer already:

- Single-branch alt collapse (G1 equivalent inside `canonicalize_aliases` / `fuse_single_use`).
- Epsilon elimination (subsumes G3 scalar-branch cases).
- Literal merging (G4 equivalent).
- Prefix factoring (structural-normalizer primary; G-suffix is the dual).

**Action:** Either delete G1/G2/G3/G4 + `CommonSuffixFactor` from `default_rules()` (`crates/ir/src/egraph/rules/mod.rs:89-99`) and rely on the normalizer; or move them into the normalizer's fixed-point loop so they fire **before** the normalizer's passes get a chance to subsume them. Post-normalizer saturation is architecturally designed to catch equivalences the normalizer misses due to pass-order blindness; in practice the normalizer converges with nothing for the egraph to catch.

The user's own docstring (`rules/mod.rs:8-26`) argues the rules justify themselves via "ordering-independent equivalence" — the empirical matrix says the normalizer is ordering-sufficient on the current grammar corpus.

**Impact:** -400 LOC. Saves the `iter=1, applied=0` saturation overhead on every compile (dominated by the class-iterator walk × 9 rules). Not free, not huge.

### R2. Retire the HIR-tier e-graph entirely, OR verify it ever fires.

**Evidence:** Zero fires across 48 HIR saturations (sum across six grammars). The `needs_saturation` fast-path (`regex/src/egraph/mod.rs:197-204`) short-circuits simple patterns; the ones that fall through to full saturation produce no work.

**Action:** Either (a) delete `default_hir_rules` + the saturation path in `RegexInfo::analyze_from_hir`, keeping only `build_hir_egraph` as a testing substrate; or (b) find a real pattern where any of the five HIR rules produces a cheaper canonical form, commit it as a regression test, and keep the code on the production path. Option (b) needs a grammar with back-to-back quantifiers, duplicate alt branches, or char-class overlap — none of the current six grammars exercise this, so the HIR tier is dead code for the current production surface.

**Impact:** -700 LOC. Removes the entire HIR saturation cache path (`saturation_cache.rs`) which is a full e-graph per-pattern cost with zero payoff on current grammars.

### R3. Promote a single non-null cost-model regression test.

**Evidence:** Section 3 + section 5 — the cost model is theoretically pluggable but has zero consumer test that asserts cost-guided extraction picks a form the greedy AstSize model would miss on a production IR. The `CALIBRATED_WEIGHTS` const is explicitly a null-result; the surface below it is untested.

**Action:** In `crates/ir/tests/egraph/`, add `cost_guides_extraction.rs` with a test: build a two-form class (one via `FuseAltRegexBranches`, one original), extract under `GrammarCostModel` vs `AstSize`, assert the two disagree. CSS L4's four fused-regex classes are the seed. This gives the cost model a regression floor so a future tranche that calibrates weights sees movement, not silence.

**Impact:** One test, ~50 LOC. Closes the null-result surface.

---

## Probe artifact

`crates/core/examples/egraph_fire_probe.rs` — run via `cargo run --example egraph_fire_probe --release` with `BBNF_EGRAPH_REPORT=1 BBNF_HIR_EGRAPH_REPORT=1`. Emits the per-grammar saturation report for every production grammar. Read-only on IR; no modifications to any crate. Kept in-tree as the firing-audit reproducer; retire when the retained rules change.

## References

- Rule definitions: `crates/ir/src/egraph/rules/{mod,regex,suffix,universal}.rs`; `parse-that/rust/regex/src/egraph/rules/{mod,flatten,redundant,repetition,superset,union}.rs`.
- Cost models: `crates/ir/src/egraph/cost.rs`; `parse-that/rust/regex/src/egraph/cost.rs`; shared weights `crates/egraph/src/cost_weights.rs`.
- Extractor: `crates/egraph/src/extract.rs`.
- Scheduler: `crates/egraph/src/{scheduler.rs,csp_scheduler.rs}`.
- Firing reports: `BBNF_EGRAPH_REPORT` at `crates/ir/src/egraph/mod.rs:106-122`; `BBNF_HIR_EGRAPH_REPORT` at `parse-that/rust/regex/src/egraph/mod.rs:95-112`.
- Pipeline entry: `crates/core/src/pipeline/compile.rs:591-603`.
- Raw data: `/tmp/fire-stderr-clean.txt` (in-session probe output).
