# DEPARTURE THESIS B — E-graph-native grammar compilation via algebraic grammar rewrites

## 1. Angle headline

Place the full grammar IR into an e-graph; apply algebraic rewrite rules that transform `GrammarENode` trees into equivalent `GrammarENode` trees; extract the cheapest form by a cost model that scores each form by how well it feeds the emitter; emit fn-per-rule (or per-shape) from the extracted grammar. The classifier (W3.1) and the emitter (W3.2) both consume the e-graph's extracted output. The authored `grammar/json.bbnf` and the extracted IR may be structurally different, but observationally equivalent on every accepted input.

This is a **grammar-to-grammar** transform, not a post-classification Shape-Emit IR transform like `aw5-n1-egraph-rewrite-codegen.md` proposes. That proposal operates after shape classification, rewriting *how we compile* each shape; this proposal operates *before* classification, rewriting *what we compile*.

## 2. Motivation — why grammar rewrite dominates grammar classification

The shape classifier (`crates/ir/src/passes/recognizers/shape_dispatch/{object,array,string,number,keyword,scalar}.rs`, 779 LOC) is a **6-way hard-coded taxonomy** that inspects rule bodies via bespoke predicates (`unwrap_wrap`, `unwrap_map_ow`, "is this a `Wrap(single-byte, Repeat(Seq(key, sep, value)), single-byte)`?"). Each detector is a pile of pattern-matching against one canonical shape. The H1 audit (`aw5-h1-shape-taxonomy-audit.md:29-30`) showed this covers 58-72% of hot-path visits under strict accounting; W4 then hand-codes four additional shapes (`ArgList`, `Flat`, `Wrap`, `HRegex`) to lift coverage to 91-93%. Every new grammar feature risks requiring a new shape.

The archaeology is revealing. Commit `bfa50f25` (2026-04-08) deleted ~1200 LOC of grammar-rewrite e-graph rules (`inline.rs` 223 LOC, `normalize.rs` 455 LOC, `prefix.rs` 430 LOC, `structural.rs` 122 LOC) because "the normalizer's cross-rule cascading (inline→merge→factor→inline) architecturally cannot be expressed in one-pass saturation". The four-layer optimizer doc demoted the e-graph to "permanent secondary" running once after the imperative normalizer converges. Retained rules are all regex-algebra peephole (`DeduplicateAltBranches`, `SupersetAbsorbAlt`, `UnionMergeAlt`, `FuseAltRegexBranches`, `CommonSuffixFactor`). None of them bridges to shape classification.

What this thesis proposes is the opposite direction: instead of burying more shapes into a hand-classified taxonomy, teach the e-graph more **algebraic** rules that make the grammar look the same when the compiler cares (same shape after extraction) and different when the grammar author cares (authored shape preserved for round-trip and debugging). Shape classification then becomes e-graph extraction — the shape tag is the **extracted canonical form**, not a bolt-on pass.

The distinction from agent 5's thesis A: A keeps the authored grammar, emits directly without an interpreter. This thesis keeps whatever emitter. Both departures can stack — emit fn-per-rule from the extracted grammar.

## 3. Concrete rewrite set with before/after IR sketches

All rules operate on `GrammarENode` (`crates/ir/src/egraph/node.rs:23-68`). Preconditions query `EClassFacts` (`crates/ir/src/egraph/analysis/facts.rs:84-117` — `first_set`, `nullable`, `width`, `literal_sid`, `regex_sid`) already populated by `GrammarAnalysis`; no new miner needed.

**G1 — Alt associativity flatten.**
```
Alt([Alt([a, b]), c])  ≡  Alt([a, b, c])
```
Precondition: always fires. Enables N-way byte dispatch over the flattened set rather than 2-way tree. Today's `AltDispatch` computation (`crates/ir/src/passes/recognizers/disjoint_first.rs`) only fires when ALL branches have disjoint FIRST sets; after flattening, a 3-way tree where 2 branches merge to disjoint FIRST and 1 doesn't can lift the 2 to a byte-dispatch, leaving the third as a linear try. Currently impossible.

**G2 — Seq associativity right-canonicalise.**
```
Seq([Seq([a, b]), c, d])  ≡  Seq([a, b, c, d])
```
Enables the tail-inline the shape emitter currently depends on (per-shape `emitter/shapes/object.rs` hand-walks `Wrap(open, middle, close)` then `unwrap_map_ow(middle)` — both operate by peeling Seq/Map wrappers). Fold peeling into extraction.

**G3 — `Kw+WS` fusion.**
```
Seq([Literal(sid), OptionalWhitespace(Epsilon)])
  where facts.literal(sid).width.max ≤ 16  ≡  KwWs(sid)
```
`KwWs` is a new leaf constructor that the emitter renders as a fused `cmp_literal + skip_ws` scan. The authored grammar writes `"important" ?w` everywhere; after rewriting, the emitter sees one terminal. Eliminates the per-call `SkipWsInline` dispatch `aw5-n1` proposes as its R9 (but at IR level, not SEIR level).

**G4 — PHF dispatch inference.**
```
Alt([Literal(s_1), ..., Literal(s_N)])
  where N ≥ 3 ∧ all widths ≤ 16  ≡  PhfDispatch(s_1, ..., s_N)
```
The `KeywordStatsMiner` writes per-Alt stats today; `PhfDispatch` hoists its decision into the IR. Keyword shape detector (H1's Shape 5, `shape_dispatch/keyword.rs`) degenerates to "does this class contain a `PhfDispatch` node after extraction?". Cost model decides via `CostWeights::dispatch_branch × N` vs `dispatch_table + phf_cost`.

**G5 — Ref-to-leaf inlining (reintroduces the deleted `inline_acyclic` with a shape-aware predicate).**
```
Ref(id)  where ir.rules[id].body extracted to leaf ∧ ref_count(id) > 1 ∧ !preserve_identity(id)  
  ≡  <inlined body>
```
The original `inline_acyclic` (commit `1bac615e`) was deleted because unbounded inlining without cross-rule cascading blew up. A shape-aware predicate ("body is a leaf" — `Literal`, `Regex`, `Epsilon`, `KwWs`) bounds the inline radius: no recursion explosion, no fixed-point cascade. **This is the specific guardrail that distinguishes this thesis from the failed 2026-04-08 attempt.**

**G6 — Repeat-over-Alt-of-literals → PHF loop (JSON-universal).**
```
Repeat { inner: Alt([Literal, ..]), lo, hi }
  where every branch is Literal  ≡  PhfLoop(s_1, ..., s_N, lo, hi)
```
Enables the `Keyword` shape emitter to drop its current linear-trial body. Fires on JSON `array` body (`value | value | value...` with Alt over 6 branches), on CSS `compoundSelector` (5 Alt branches in a Repeat — Unordered shape).

**G7 (CSS-specific) — Disjoint-FIRST Repeat-over-Alt ≡ byte-class-dispatched loop.**
```
Repeat { inner: Alt_disjoint_first, lo, hi }  ≡  ClassifyByteLoop(disjoint_table, lo, hi)
```
Extends `DtaState::ClassifyByte` reach into Repeat contexts that today need the Unordered shape's hand-coded emitter.

**G8 (Sheets-specific) — Operator-chain associativity collapse.**
```
Seq([left, Op_higher_prec, right_expr, Op_lower_prec, ..])
  ≡  OperatorChain([left, right_expr, ..], [Op_higher, Op_lower])
```
The existing `operator_chain::collect_operator_chains` does this today as a miner producing `OperatorChainFacts`. Lift into IR as a canonical form; the Pratt shape detector (H1's Shape 6) becomes "does this class extract to `OperatorChain`?". The Pratt tower in `grammar/google-sheets/google-sheets.bbnf:94-118` (6 rungs) extracts to one `OperatorChain` node with a precedence LUT.

## 4. Cost model integration

Cost composes through `GrammarCostModel` (embedded in `egraph::CostWeights`, `crates/egraph/src/cost_weights.rs:40-112`). Existing scalar cost is summed `structural + alt_per_branch × N + dispatch_bonus × (dispatch present) + tape_push × tape_writes + ...`. For grammar rewrites:

- `PhfDispatch(N)` costs `dispatch_table + dispatch_branch × log2(N)` vs `alt_per_branch × N` for the alt.
- `KwWs` costs `literal_scan + ws_skip_fused` = negative delta vs `Seq([Literal, OptionalWhitespace])`.
- `OperatorChain(k)` costs `pratt_base + k × pratt_per_rung` vs the nested Seq chain's `k × (alt_per_branch + seq_per_child)`.
- `ClassifyByteLoop` costs `256 × 1 + loop_body` vs `Repeat × alt_per_branch × N`.

Lattice (`crates/egraph/src/extract.rs:36-58`) is already in place for multi-objective extraction: `(cycles, bytes)` Pareto to avoid the CSS 154 KB walker icache pathology (AW-V.md:42). The cost model reads directly from `CostConfig` — single per-compile source of truth; no new knob system.

CSP integration: `crates/csp-solver/src/solver/` already models per-rule decisions; the grammar e-graph's extraction is a CSP where variables = e-classes, domains = extracted e-nodes, constraints = cost-dominance. The `CspScheduler` (`crates/egraph/src/csp_scheduler.rs`) already propagates dirty-flag through parent edges. **This is exactly the unification the orchestrator seeds**: egraph extraction IS a CSP. The constraint "pick the shape-classifiable form when available" is a per-class preference constraint; "bound total instruction bytes per-fn ≤ 100 KB" is a whole-grammar sum constraint. Both fit csp-solver's existing `LatticeDomain` abstraction.

## 5. E-graph readiness — API surface available today

From `crates/egraph/src/lib.rs:46-57`, today:
- `Language` trait (`language.rs:16-57`) — auto-derived via `#[derive(egraph_derive::Language)]`. `GrammarENode` (`crates/ir/src/egraph/node.rs:23`) already implements it. ✓
- `EGraph<N, A>` with `add`, `union`, `rebuild`, `classes`, `class` (`egraph.rs:155-301`). ✓
- `Rewrite<N, A>` trait (`rewrite.rs:34-71`) with `search`/`should_apply`/`apply`. Existing rules (`DeduplicateAltBranches`, `CommonSuffixFactor`) are templates. ✓
- `GrammarAnalysis` with per-class `EClassFacts` (`crates/ir/src/egraph/analysis/facts.rs:84`) already populated — preconditions evaluable cheaply. ✓
- `Extractor` with greedy bottom-up, `CostModel` trait (`extract.rs:73-99`), `Lattice` for Pareto (`extract.rs:36-58`). ✓
- `BackoffScheduler` + `CspScheduler` with node/iter/growth caps (`scheduler.rs:50-113`). ✓
- `CostConfig::egraph_iter_limit = 64` and `egraph_node_limit = 100_000` (`cost_config.rs:42-46`) — saturation already bounded. ✓

**What's needed beyond today**:
- Three new e-node variants on `GrammarENode`: `KwWs(StringId)`, `PhfDispatch(Box<[StringId]>)`, `OperatorChain(...)`, `ClassifyByteLoop(...)`, `PhfLoop(...)`. These are leaf-ish — minor additions to `Language` auto-derive.
- `GrammarCostModel` extensions for the new variants. Already has the `CostWeights` slots; extend `cost()` match arms.
- The rewrite rules themselves — one file each per rule, following `rules/suffix.rs` precedent (~50 LOC per rule).

Saturation termination: every rewrite above either produces a more-compact canonical form (`G1`–`G3`, `G4`, `G6`, `G7`, `G8` are node-count-decreasing) or is bounded by a leaf-predicate (`G5`). `G1`/`G2` reach a normal form in O(depth) iterations per class. The 64-iter/100K-node cap already absorbs pathology.

## 6. Relationship to shape-emitter W3.1 — rewrites subsume classification

Every detector in `shape_dispatch/*.rs` is a hand-coded pattern match that answers "is this rule shape-X?". Under this thesis, the question becomes: **does this rule's e-class, after saturation, extract to a canonical shape-X form?** The detector per shape is one predicate (`extracted_node.is_phf_dispatch()`, `extracted_node.is_operator_chain()`, etc.) — 1 LOC per shape, not 40–200 LOC per shape.

The W4 shapes (`ArgList`, `Flat`, `Wrap`, `HRegex`) each admit a canonical IR rewrite that the e-graph can discover:
- `ArgList` ≡ `Seq([name, "(", Repeat(Seq(arg, sep?)), ")"])` after `G2` flattening — one structural predicate.
- `Flat` ≡ `Seq([Literal, (Literal|Ref|Regex)+])` — becomes `FlatSeq(head, body)` under a new rewrite `G9`.
- `Wrap` ≡ `Alt([Ref_1, ..., Ref_n])` with no Literal branches — already canonical after dedup; shape tag reads `first_set.is_disjoint_over_refs()`.
- `HRegex` ≡ `Map { Regex, host_fn }` — already canonical.

The `ShapeAssignments` data structure (`shape_dispatch/mod.rs:73-109`) persists, but its producer is `extract_shape_tag(egraph, root_class_id)` rather than a hand-coded tree walker. W3.1's 779 LOC of bespoke detectors collapses to ~150 LOC of tag-reading plus the rewrite rules themselves.

## 7. Cross-grammar applicability

| Rewrite | JSON | CSS L4 | Sheets | BBNF | Universal? |
|---|---|---|---|---|---|
| G1 Alt flatten | ✓ value | ✓ many | ✓ primary | ✓ directive | yes |
| G2 Seq flatten | ✓ pair | ✓ *Decl family | ✓ cell | ✓ rule body | yes |
| G3 Kw+WS fusion | (minor) | ✓ heavy — every `"!important"?w` | ✓ formula spaces | ✓ directive bodies | yes |
| G4 PHF dispatch | ✓ value dispatcher | ✓ 45+ Kw Alts | ✓ error_literal | ✓ directive 7-way | yes |
| G5 Ref-to-leaf inline | (minor) | ✓ token rules | ✓ cell_ref | ✓ many | yes |
| G6 PhfLoop | ✓ array-of-values | ✓ compoundSelector | (rare) | (rare) | mostly JSON/CSS |
| G7 ClassifyByteLoop | (rare) | ✓ compoundSelector | ✓ operator token | ✓ term | CSS-heavy |
| G8 OperatorChain | (no) | ✓ mathProduct/mathExpr | ✓ 6-rung tower | ✓ value_expr 6-rung | Sheets/CSS/BBNF |

All 8 rules fire across multiple grammars. None is grammar-specific at the pattern level — each queries only `EClassFacts` and `GrammarENode` shape, never `ir.name`. This passes the AW-IV §6/§7 governance rule. The per-grammar output differs because per-grammar IR differs; the rewrite set is universal.

## 8. Risks

**Saturation blow-up.** The `inline_acyclic` failure mode (2026-04-08) is the canonical precedent: unbounded inlining cross-references cascade. `G5`'s leaf-predicate (`extracted_node.is_leaf() ∧ ref_count > 1`) is the new guardrail — inlining only terminal bodies, not recursive rules. Combined with `egraph_iter_limit = 64`, `egraph_node_limit = 100_000`, the worst case is bounded.

**Non-confluence.** `G1` and `G2` are confluent (associative, idempotent). `G3` and `G4` commute with `G1`/`G2`. `G5` is non-confluent with `G1` (inlining a leaf that's in an Alt changes the flattening pattern) but the cost model picks a single representative — determinism rescued by extraction. Empirical: CSS L4 has 2875 DTA states (per `AW-IV.W5.3` sweep); the e-graph pre-sized to ~10K nodes (4× `count_nodes` sum) fits comfortably in the 100K cap even after 64 saturation rounds.

**Compile-time budget.** bbnf-regex HIR saturation already runs; measure the current e-graph pass cost: `BBNF_EGRAPH_REPORT=1` (`crates/ir/src/egraph/mod.rs:105-121`) shows current rule-set cost. Estimate: CSS L4 bootstrap regen is ~10s per cold run; the current e-graph pass is <300ms (a fraction of one percent of bootstrap). Adding 8 grammar rewrites doubles the rule set; projected CSS L4 e-graph pass at <1s. Apple M4 Max compile-time target (bootstrap < 15s): fits. Measurement plan: instrument `build_and_saturate` with a per-rule timer (the `RunReport.per_rule` field already tracks work), compare before/after on CSS L4 + Sheets + BBNF.

## 9. Phase-in plan

Incrementally: this lands as sub-modules of `crates/ir/src/egraph/rules/` (one file per rewrite, `grammar_*.rs`). No new crate. The existing `default_rules()` function extends to include them. W3.1's detectors stay live as ground-truth oracle for a regression harness: for every rule classified by the detector, the extractor must produce a node the detector's predicate matches. When the harness is green across all 4 grammars, delete the detectors; shape classification becomes the extraction function. This is **substrate + consumer in one wave** per the README.md §Substrate-with-consumer rule: the pass doesn't "land" until W3.1's detectors are deleted and shape tags flow from extraction.

The 8 rewrites partition cleanly into two waves:
- **Wave α**: `G1`, `G2`, `G3`, `G4` — grammar-universal, strictly simplifying. Regression gate: bootstrap regen idempotency + all grammar roundtrips.
- **Wave β**: `G5`, `G6`, `G7`, `G8` — shape-specific. Each deletes one `shape_dispatch/*.rs` detector upon landing.

Bound: this is an engineering problem, not a research problem. egg (the e-graph library this crate imitates) has shown grammar-to-grammar rewriting works at scale; the novelty here is integration with per-grammar IR-fact preconditions + the CSP cost-extraction unification. Prior art: egg's own papers (Willsey et al., 2021), Souffle (for the Datalog-over-e-graph pattern), Rewriter-discovery via Ruler (Nandi et al., 2021) — but Ruler is not needed; the rule set above is hand-selected with clear cost semantics. The closest grammar-specific prior art is the egglog combinator fusion work and LuaJIT's trace synthesis — but neither uses an EBNF-level substrate with typed shape categories. This is a novel composition of known substrates.
