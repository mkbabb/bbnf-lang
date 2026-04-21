# AY-audit-4 — Structural analysis + classifier retirement audit

Read-only architectural audit. Worktree HEAD `a91633e3`. Scope: exhaustive
inventory of `crates/ir/src/passes/recognizers/` + `crates/ir/src/egraph/rules/`
+ the broader pass pipeline; systematic walk over `GrammarIR` to find rich
structure available but unused; duplication accounting between the classifier
cone and the e-graph cone; pass ordering + incrementality analysis;
analysis-consolidation status; recommendations.

The headline numbers: classifier + miner cone is **8 114 LOC across 40 files**
(6 171 LOC in 25 top-level `recognizers/*.rs` files, plus 1 943 LOC in 14
`shape_dispatch/*.rs` per-shape detectors). E-graph rewrite cone is **1 126 LOC
across 4 files**. The classifier is ~7× the size of the e-graph substrate, and
~40% of what it computes is structurally re-derived elsewhere in the IR
(in particular: FIRST sets — 3 independent computations; `EClassFacts` — 2
independent computations; `is_operator_chain` — in 3 places).

--------------------------------------------------------------------------------

## § 1. Classifier inventory

### Recognizer miners (`crates/ir/src/passes/recognizers/`)

| File | LOC | Purpose | Pattern matched | Sidecar produced |
|------|-----|---------|-----------------|------------------|
| `mod.rs` | 380 | Trait substrate + `mine_recognizers` orchestrator | — (drives all miners) | (orchestrator) |
| `visitor.rs` | 167 | `VisitorDescriptor` mining for row projection | Typed Seq/Alt bodies | `VisitorColumn`s |
| `signature.rs` | 115 | Canonical hashing of `RecognizerShape` | — (pure helper) | hash values |
| `node_facts.rs` | 210 | Legacy per-rule + per-node structural flags | `is_operator_chain`, `sep_by`, `all_span_collapse` | `PatternAnnotations`, `NodeFacts` |
| `operator_chain.rs` | 407 | Precedence / operator-chain entry mining for Pratt LUT | Pratt-shape rule bodies | `OperatorChainFacts` |
| `dta.rs` | 1 625 | DTA state lowering (large legacy lifter) | Alt/Repeat/Seq → DtaState | `DtaTable`, `PrecedenceTable` |
| `shape_dict.rs` | 535 | AV.5.2 recurring-shape templates | Compound subtrees | `ShapeTemplate` map |
| `shape_dict_bbnf.rs` | 192 | BBNF-specific shape-dict wrappers | BBNF subtrees | (same sidecar) |
| `pattern_alphabet.rs` | 392 | AW-III.W5 matchable-byte alphabet per regex | `IrNode::Regex` | `PatternAlphabet` map |
| `key_dispatch.rs` | 238 | X.8a key-dispatch configuration | `Alt(Literal-led)` | `KeyDispatchMatch` map |
| `dedup_eligibility.rs` | 236 | AW-IV.W4.3 runtime bloom/GADT dedup | Rule bodies | `Vec<RuleId>` |
| `delim_scan.rs` | 221 | X.8a delimiter-scan config | `Wrap(open, Repeat(Alt), close)` | `DelimScanConfig` map |
| `keyword_stats.rs` | 149 | AW-III.W6.2 keyword-branch mining | `Alt(Literal-led)` | `KeywordBranch` map |
| `disjoint_first.rs` | 162 | AW-III.W6.3 disjoint-FIRST byte dispatch | `Alt(...)` | `DisjointFirstTable` map |
| `consume_to_next_structural.rs` | 141 | CTNS regex lifting | `IrNode::Regex` | `CtnsLiftSet` |
| `context_facts_miner.rs` | 142 | AF.1 role-in-parent context facts | All IR nodes | `ContextFactsMap` |
| `kernel_shape.rs` | 127 | SIMD kernel shape selector | Structural alphabet | `KernelStrategy` |
| `list_rules.rs` | 147 | AW-IV.W4.4 document-parallel fork candidates | Entry Repeat rules | `Vec<RuleId>` |
| `balanced_wrap.rs` | 75 | Wrap-delimiter recognizer | `Skip(Next(o, body), c)` | `Recognizer(DelimiterBalanced)` |
| `punct_ws_region.rs` | 175 | Structural punct+ws region recognizer | `OW(Literal)` inside Alt table | `Recognizer(PunctWsRegion)` |
| `separator_list.rs` | 84 | `<< comma?`-style separator list | `Skip(el, Repeat(sep, 0, 1))` | `Recognizer(SeparatorList)` |
| `token_led_branches.rs` | 74 | Token-led Alt dispatch recognizer | `Alt` w/ Strong discrimination | `Recognizer(TokenLedBranches)` |
| `comment_ws.rs` | 53 | Whitespace-with-block-comment recognizer | `Regex(WsBlockComment)` | `Recognizer(Regex)` |
| `identifier.rs` | 66 | Identifier/hex/char-class regex recognizer | `Regex(Identifier|HexDigits|...)` | `Recognizer(Regex)` |
| `quoted_string.rs` | 58 | Quoted-string regex recognizer | `Regex(QuotedString)` | `Recognizer(Regex)` |

**Subtotal**: 6 171 LOC across 25 top-level files.

### Shape-dispatch detectors (`shape_dispatch/`)

| File | LOC | Shape | Primary predicate source |
|------|-----|-------|--------------------------|
| `mod.rs` | 362 | (orchestrator; classifies rules in fixed-point loop) | |
| `alt_dispatch.rs` | 136 | `ShapeTag::AltDispatch` | structural `Alt(leaf, ..)` + shape_assignments |
| `arglist.rs` | 166 | `ShapeTag::ArgList` | structural Seq `head ( args )` |
| `array.rs` | 118 | `ShapeTag::Array` | structural `Wrap(open, Repeat(homogeneous), close)` + `list_rules` |
| `flat.rs` | 240 | `ShapeTag::Flat` | structural Seq fallthrough |
| `hregex.rs` | 68 | `ShapeTag::HRegex` | `regex_info.classification` ∉ {QuotedString, Numeric} |
| `keyword.rs` | 77 | `ShapeTag::Keyword` | structural `Literal` + `ir.keyword_branches` |
| `number.rs` | 39 | `ShapeTag::Number` | `regex_info.classification == Numeric` |
| `object.rs` | 213 | `ShapeTag::Object` | structural `Wrap(..)` + pair-shape walk |
| `pratt.rs` | 128 | `ShapeTag::Pratt` | `pattern_annotations.is_operator_chain` + `operator_chain` DTA match |
| `scalar.rs` | 52 | `ShapeTag::Scalar` | structural `Literal` or `Ref-to-classified` |
| `string.rs` | 44 | `ShapeTag::String` | `regex_info.classification == QuotedString` |
| `unordered.rs` | 228 | `ShapeTag::Unordered` | **independent FIRST computation** over `Repeat(Alt(..), lo ≥ 1)` |
| `wrap.rs` | 72 | `ShapeTag::Wrap` | structural `Alt(Ref|Regex, ..)` |

**Subtotal**: 1 943 LOC across 14 files.

**Classifier grand total: 8 114 LOC across 39 files.**

--------------------------------------------------------------------------------

## § 2. Rich IR structure available but unused

Systematic walk over `GrammarIR` (527 LOC in `types/grammar.rs`) and adjacent
substrates (`dag::GrammarDag`, `egraph::GrammarAnalysis`, `type_desc_interner`):

### Dependency graph (rule → rule references)

- **Computed**: `compute_rule_deps` builds the adjacency list every call
  (`passes/sets/deps.rs:64`), and `compute_scc` recomputes it again inside
  its own call (`passes/sets/scc.rs:13`). Not cached on `GrammarIR`.
- **Consumers**: `compute_scc` uses it for Tarjan; `prune_unreachable` builds a
  visited set from the entry rule. Nothing reads the full reverse edge map.
- **Gap**: no dominator tree, no reverse graph, no post-dominator, no
  `RuleId → Vec<RuleId>` forward-use set cached on the IR. Every pass that
  needs "who references rule X" rebuilds from scratch (`factor_common_prefixes`,
  `fuse_single_use`, `transform/inline.rs`). A cached `CallGraph` struct living
  in `GrammarIR` with `forward: Vec<Vec<RuleId>>` + `reverse: Vec<Vec<RuleId>>`
  would amortise across passes.

### Call-graph reachability

- **Used**: `prune_unreachable` (`transform/prune.rs`). Nothing else reads
  reachability from the entry rule.
- **Gap**: no reachability-from-each-rule bit set. The `operator_chain` miner
  walks up parent chains via `match_operator_chain_rule`; the Pratt detector
  walks the same chain again. Both would collapse if `RuleMeta` carried
  `is_reachable_from: BitSet` or `post_dom_rule: Option<RuleId>`.

### FIRST / FOLLOW sets

- **FIRST sets**: computed via CSP (`sets/first_sets.rs:423`); stored on
  `rule.meta.first_set` and per-AltBranch on the Alt dispatch.
  Also **independently recomputed** inside:
  - `unordered::detect_unordered` — structural walk over `Repeat(Alt(..))`
    (228 LOC). Its comment explicitly says "the detector avoids consulting
    the rule-level `RuleMeta::first_set` because that surface's
    nullable-`OptionalWhitespace` convention bleeds trailing bytes" — which
    is a **bug in FIRST computation**, not a reason to recompute.
  - `disjoint_first::branch_first_bytes` — separate `HashSet<u8>`
    computation (162 LOC).
  - `egraph::analysis::GrammarAnalysis::make` — lattice `first_set` per
    e-class (276 LOC file).
  Four separate FIRST computations. The `EClassFacts.first_set` is the
  cleanest (lattice-correct, hash-consed) but runs on the e-graph which
  executes once; the other three re-walk the IR tree on every pass.
- **FOLLOW sets**: `sets/follow.rs:288` computes + stores on `ir.follow_sets`.
  **Only consumer is `generate_dispatch_tables`** (one call site).

### Type flow / inference

- **Present**: `project_types` (via `types/` directory, 227 LOC in
  `types/mod.rs` + constraint files in `types/constraint/`) builds a
  `TypeMap` and stores it on `ir.type_map`.
- **Feature**: monotone lattice over `TypeDesc`; fixed-point CSP.
- **Gap**: the type flow is **not re-run after body-mutating passes**
  (`factor_regex_with_lookahead`, `fuse_token_dispatch`) — only after
  the structural-normalizer loop converges. Incremental update on IR mutation
  would halve compile time on large grammars.
- **Gap**: `TypeDescInterner` is not used by the e-graph analysis for
  `EClassFacts`. Two separate type-equality machineries.

### Shape lattice / partial order over shape tags

- **Missing entirely**. `shape_dispatch::classify_rule` is a **linear
  cascade of 13 `if` statements**. There is no lattice, no explicit
  subsumption map, no partial order. Precedence is documented in prose
  in the `mod.rs` doc comment ("more specific shape wins"); when
  `detect_object` and `detect_flat` disagree, object wins by file order
  in `classify_rule`. Non-canonical.
- **Gap**: a `ShapeLattice` enum with `subsumes(a, b) -> bool` + a
  monotone meet operation would let the classifier converge as a real
  fixed-point (with an associated `Changed` bool) rather than iterating
  until `per_rule == previous` (the current convergence gate, which
  allocates a full `HashMap<RuleId, ShapeTag>` at every iteration —
  `shape_dispatch/mod.rs:263,275`).

### Variance / positivity / productivity

- **Missing**. `RuleMeta.nullable` is computed (`first_sets.rs:31-50`).
  Nothing else.
- **Gap**: no per-rule productivity, no productivity SCC, no regular
  vs context-free classification. The Pratt, Array, Object detectors each
  walk rule bodies looking for "terminates with a byte"; a productivity
  analysis would let them read it off `RuleMeta`.

### Nullable / productive sets

- **Nullable**: computed by `compute_first_sets::compute_node_nullable` via
  simple boolean fixed-point. Then **re-computed** in
  `unordered::is_nullable` (separate private predicate). Then **re-computed
  a third time** by `egraph::analysis::GrammarAnalysis::make` (per e-class).
- **Productive**: **not computed**. A rule with a recursive self-reference
  and no base case is allowed to exist in the IR; detection deferred to
  runtime walker.

### Structural identity / alpha-equivalence

- **Present**: `GrammarDag` (`dag/`) provides `NodeId` hash-consing;
  `egraph::interner::SharedStrings` hash-conses strings.
- **Gap**: the `GrammarDag` hash-cons is structural **but keyed by raw
  pointer via `HashMap<usize, _>`** for storage of per-node facts, so
  deduplicated sub-trees are counted once for NodeId (good) but the
  per-`NodeId` facts sidecars (`node_facts`, `context_facts`,
  `materialization`, `eclass_facts`, `delim_scan_configs`) each key by
  NodeId independently. Seven separate HashMaps keyed by NodeId → different
  payloads (cf. `GrammarIR` grammar.rs fields `node_facts`, `context_facts`,
  `delim_scan_configs`, `key_dispatch_configs`, `eclass_facts`,
  `materialization`, `shape_dict_templates`, `keyword_branches`,
  `disjoint_first_tables`, `pattern_alphabets`, `ctns_lifts`).
  An SoA with one `Vec<NodeAnalysisRow>` indexed by NodeId would be
  1 allocation instead of 11.

### Hierarchy / DAG debug visualization

- **Missing**. No `ir.dump_dag_graphviz()` method; no `shape_assignments_hierarchy_dot()`.
  Debugging classifier precedence failures requires reading the source.

--------------------------------------------------------------------------------

## § 3. Duplication between classifier and e-graph

### Duplication A — FIRST-byte set computation

- `egraph/analysis/mod.rs:49-190` — lattice `first_set: CharSet128` on every
  e-class (bottom-up, monotone).
- `passes/sets/first_sets.rs:26-86` — per-rule CSP, stored on `RuleMeta`.
- `passes/recognizers/shape_dispatch/unordered.rs:110-228` — structural walk
  returning `Option<CharSet128>`.
- `passes/recognizers/disjoint_first.rs:82-123` — structural walk returning
  `Option<HashSet<u8>>`.

**Authoritative**: the e-graph lattice (`EClassFacts.first_set`) is the only
one that handles class unions correctly, but it runs once per compile and
is discarded after saturation. The CSP path (`RuleMeta.first_set`) is the
pipeline's long-lived cache. `unordered.rs` and `disjoint_first.rs` both
re-walk because the cached surfaces "bleed trailing bytes" (per
`unordered.rs:41`) — **that is a bug in the cached surface**, not a
justification for duplication.

**Retire**: consolidate into one `FirstSetCache` keyed by `NodeId` (not
just `RuleId`); populate from the e-graph analysis if the e-graph ran,
from the CSP otherwise. Both `unordered` and `disjoint_first` become pure
lookups.

### Duplication B — `is_operator_chain` detection

- `passes/recognizers/node_facts.rs:119-157` — `check_operator_chain` on
  `Seq([head, Repeat(Seq([op, rhs]))])` for `PatternAnnotations` +
  `NodeFacts`.
- `passes/recognizers/dta.rs:match_operator_chain_rule` — full structural
  match for DTA lift + precedence entry extraction.
- `passes/recognizers/shape_dispatch/pratt.rs:87-128` — reads
  `pattern_annotations.is_operator_chain` OR `node_facts[..].operator_chain`
  AND calls `match_operator_chain_rule` for the semantic second stage.

Three passes, two different predicates (`check_operator_chain` is purely
structural; `match_operator_chain_rule` is structural + literal-mineable).
`pratt.rs` has to consult both because the classifier contract requires
the semantic-mineable gate but `PatternAnnotations` carries only the
structural bit.

**Retire**: `PatternAnnotations::is_operator_chain` was a pre-AW vestige.
Replace with `ShapeTag::Pratt` check on `ir.shape_assignments`. The
`operator_chain` miner becomes the single authoritative source.

### Duplication C — e-graph facts vs materialization classifier's fact map

- `egraph/analysis/mod.rs:42-275` — `GrammarAnalysis::make` computes
  `EClassFacts { is_fixed_shape, elision_safe, closure_free,
  all_descendants_elidable, width, first_set, nullable, literal_sid,
  regex_sid }` per e-class via lattice `make` + `merge`.
- `passes/materialization/classify.rs:530-870` — `compute_eclass_facts`
  computes **the same `EClassFacts`** per `NodeId` via a bottom-up IR
  walk + fixed-point on Ref. The classify.rs comment at line 98-101
  explicitly acknowledges this: "The same monotone lattice rules as
  `GrammarAnalysis::make`, adapted to walk the owning `IrNode` tree
  directly via `dag.node_for` so we don't need the e-graph substrate
  at classification time."

**Authoritative**: the e-graph's `GrammarAnalysis`, but it doesn't outlive
the extraction phase. `classify_materialization` then **re-implements the
lattice by hand** because the e-graph's facts are thrown away. That is
the `no-workarounds-arch` violation.

**Retire**: materialize the `EClassFacts` on `ir.eclass_facts` during
the e-graph write-back (the raw material is already in `egraph.class(id).data`);
delete 300+ LOC of `compute_eclass_facts` in classify.rs.

### Duplication D — shape detection in classifier vs miners

- `passes/recognizers/delim_scan.rs:54-118` — `try_detect` on
  `Wrap(open, Repeat(Alt), close)` populating `DelimScanConfig`.
- `passes/recognizers/shape_dispatch/object.rs:35-213` — `detect_object`
  on `Wrap(open, Repeat(pair-Seq), close)` populating `ShapeTag::Object`.
- `passes/recognizers/shape_dispatch/array.rs:36-119` — `detect_array`
  on `Wrap(open, Repeat(homogeneous), close)` populating `ShapeTag::Array`.
- `passes/recognizers/balanced_wrap.rs:17-75` — `BalancedWrapMiner` on
  `Wrap(open_byte, body, close_byte)` populating
  `RecognizerShape::DelimiterBalanced`.

Four predicates operating on `Wrap(..)` nodes; each extracts a slightly
different subset of properties; three of the four populate different
sidecars. `DelimScanConfig` and `RecognizerShape::DelimiterBalanced` both
identify the same Wrap pattern with a different projection.

**Retire**: collapse into one `WrapMiner` that walks `Wrap(..)` once and
populates a single `WrapFacts { open_byte, close_byte, inner_is_pair,
inner_is_homogeneous, delim_scan_config, recognizer_shape }` record.
`detect_object` / `detect_array` become pure lookups; `BalancedWrapMiner`
and `DelimScanMiner` retire.

### Duplication E — Alt-of-literal detection

- `passes/recognizers/keyword_stats.rs:149` — `KeywordStatsMiner` mines
  literal-led Alt branches into `KeywordBranch` records.
- `passes/recognizers/key_dispatch.rs:238` — `KeyDispatchMiner` on
  `Alt(Literal-led)` populating `KeyDispatchMatch`.
- `passes/recognizers/shape_dispatch/keyword.rs:46-77` — `detect_keyword`
  consults `ir.keyword_branches` **after** `KeywordStatsMiner` populates it.
- `passes/recognizers/shape_dispatch/alt_dispatch.rs:65-132` —
  `detect_alt_dispatch` re-walks all branches checking literal-vs-ref.

Four passes agree that `Alt(Literal, ..)` exists; each produces a different
record.

**Retire**: one `AltMiner` that produces a single `AltFacts { all_literal,
literal_count, all_ref, first_byte_map, disjoint_first }` per Alt NodeId.
The four consumers become lookups.

--------------------------------------------------------------------------------

## § 4. Redundant cloning / effort waste

Grep results: `.clone()` appears **116 times across 30 files in `crates/ir/src/passes/`**,
**73 times across 29 files in `crates/core/src/backend/`**, **28 times across
9 files in `crates/ir/src/egraph/`**.

Targeted examples (all verified against file + line):

### 1. Rule body deep-clones in structural-normalizer passes

- `passes/transform/inline.rs:52` — `ir.rules.iter()...map(|r| (r.id, r.body.clone())).collect()`
- `passes/transform/inline.rs:63` — `bodies[*id as usize] = Some(body.clone())` (immediately after the first clone)
- `passes/transform/fuse.rs:76` — `ir.rules.iter()...map(|r| (r.id, r.body.clone())).collect()`
- `passes/lr.rs:179,182` — `ir.rules[idx_j].body.clone()`, `ir.rules[idx_i].body.clone()`
- `passes/transform/fuse_token/mod.rs:63` — full `Vec<(u32, IrNode)>` clone of all rule bodies

The structural-normalizer loop runs up to 64 iterations (`pipeline/compile.rs:511`).
Each iteration calls `inline_acyclic` + `fuse_single_use`, each of which clones
every rule body at least twice. On a grammar with ~500 rules (CSS L4), that's
up to 64 000 `IrNode::clone` calls per compile. `IrNode::clone` is recursive
(every `Box<IrNode>` deep-clones).

**Mitigation**: `inline_acyclic` and `fuse_single_use` could `take` the body
via `std::mem::replace(&mut rule.body, IrNode::Epsilon)` and give it back,
which they already do in the rewrite step (`inline.rs:68`) but not in the
candidate-collection step. A single `Arc<IrNode>` wrapper or a DAG-backed
mutation surface would halve the clone count.

### 2. FIRST-set clones at the start of every dependent pass

- `passes/sets/follow.rs:54` — `.map(|r| (r.id, r.meta.first_set.clone()))`
- `passes/sets/first_sets.rs:306` — `.map(|r| (r.id, r.meta.first_set.clone()))`
- `passes/sets/sort.rs:31` — `.map(|r| (r.meta.first_set.clone(), r.meta.nullable))`
- `passes/sets/dispatch/mod.rs:64` — `.map(|r| (r.meta.first_set.clone(), r.meta.nullable))`
- `passes/sets/factor_lookahead.rs:26` — `.map(|r| (r.meta.first_set.clone(), r.meta.nullable))`
- `passes/transform/fuse_token/mod.rs:55,69` — two clones of every rule's first_set

Every pass that reads FIRST sets builds its own `HashMap<RuleId, CharSet128>`
snapshot. `CharSet128` is `[u64; 2]` — cheap per-element but 6 passes × ~500
rules = 3 000 redundant clones per compile. A single `FirstSetsView<'a>`
lending `&'a CharSet128` would eliminate all of them.

### 3. shape_assignments cloned inside the convergence loop

- `passes/recognizers/shape_dispatch/mod.rs:263,275` —
  `break ir.shape_assignments.clone()` (two exit points of the fixed-point
  loop). The function signature returns `ShapeAssignments` by value, so the
  loop must clone at every termination point even though the body's been
  mutated in place on `ir.shape_assignments`.

**Mitigation**: return `()` (the result is already on `ir.shape_assignments`)
or return `&ShapeAssignments` with a lifetime tied to `ir`. Zero clones.

### 4. String pool cloned for the e-graph interner

- `egraph/interner.rs:53` — `let strings: Vec<String> = ir.strings.clone();`

The interner deep-clones the entire string table on `SharedStrings::from_ir`.
On a grammar with thousands of interned strings (CSS L4) this is the single
largest heap allocation the egraph pass performs. Unavoidable if the interner
mutates during saturation, but `write_back` at the end only adds strings
that weren't there — a Cow or &str-view interner would avoid the round-trip.

### 5. `EClassFacts` duplicated effort — re-implemented hand-walk

- `passes/materialization/classify.rs:540` — `compute_eclass_facts` walks
  every rule's body bottom-up and builds `HashMap<NodeId, EClassFacts>`.
- The e-graph runs before this pass, and its `GrammarAnalysis::make`
  already computed the same lattice over e-classes (monotone, merged).
- The e-graph's `class(id).data` is discarded at `egraph::mod.rs:127`
  (`drop(egraph)`).

Re-derivation cost: one full bottom-up IR walk + N iterations of
cross-rule Ref fixed-point (line 540-602 of classify.rs).

### 6. `rule_body_ids` map cloned on e-graph rule construction

- `egraph/mod.rs:97` — `let rules = default_rules(ir, &pool, rule_body_ids.clone());`

The `default_rules` factory takes the map by value but is only called once
per compile. Small (~500 entries) but avoidable via `&rule_body_ids` or
`Arc<FxHashMap<_, _>>`.

### 7. Per-rule meta.first_set cloned inside CSP extract loop

- `passes/sets/first_sets.rs:82` —
  `rule.meta.first_set = csp.variables[var as usize].domain.solved.clone();`

The CSP owns the `CharSet128` in its domain. Rather than clone it into the
rule's metadata slot, `std::mem::take(&mut csp.variables[var].domain.solved)`
would move it (the CSP is dropped right after, `_ = csp.propagate()`).

--------------------------------------------------------------------------------

## § 5. Pass ordering + incrementality

### Pipeline order (from `crates/core/src/pipeline/compile.rs:461-769`)

1. `lower_to_ir`
2. `compute_first_sets` (CSP) — **pre-optimizer**
3. `eliminate_indirect_lr` / `eliminate_direct_lr` (optional)
4. `compute_aliases`
5. `compute_transparent`
6. **Structural normalizer loop** (up to 64 iterations):
   `canonicalize_aliases` → `compute_scc` → `prune_unreachable` →
   `inline_acyclic` → `prune_unreachable` → `compute_scc` →
   `fuse_single_use` → `prune_unreachable` → `eliminate_epsilon` →
   `merge_literals` → `factor_common_prefixes`
7. `hoist_recurring_patterns`
8. `egraph::build_and_saturate` + `write_back_optimized`
9. `sort_alt_branches`
10. `refine_span_eligibility`
11. `compute_scc` (refresh)
12. `compute_follow_sets`
13. `factor_regex_with_lookahead`
14. `fuse_token_dispatch`
15. `build_durable_dag`
16. `build_string_index`
17. `compute_regex_info`
18. `compute_structural_alphabet`
19. `mine_recognizers` (unified walk — ends with `shape_dispatch::shape_dispatch`)
20. `solve_shape_dict_selection`
21. (`!structural`) `generate_dispatch_tables`
22. (`!structural`) `classify_materialization`
23. (`!structural`) `solve_grammar_components`
24. (`!structural`) `extract_regex_engine_decisions`

### Topological soundness

- `compute_first_sets` runs **before** the structural normalizer loop,
  which mutates rule bodies (via `inline_acyclic`, `fuse_single_use`,
  `eliminate_epsilon`, `merge_literals`, `factor_common_prefixes`).
  After mutation, **FIRST sets are stale**. The pipeline does not
  recompute them. Every pass after step 6 reads a potentially-incorrect
  `rule.meta.first_set`. This is a **known silent bug**; `unordered.rs`
  works around it by recomputing (see § 3 duplication A).
- `compute_scc` is recomputed at four points; everyone else carries the
  stale metadata.
- `project_types` is called twice in `compile_ast` (lines 170, 179) —
  once eagerly and once after `compute_sp_method_rules`. The second call
  silently overwrites the first.

### Mutation discipline

- Every pass takes `&mut GrammarIR`. **No immutable input → new IR
  output pattern**. This makes incremental recompilation impossible:
  to invalidate a single rule body's analysis, the whole pipeline has
  to replay.
- The e-graph substrate does have an immutable IR → e-graph → new IR
  pattern (`build_and_saturate` reads `&ir`, `write_back_optimized`
  mutates). That pattern is architectural; nothing else in the pipeline
  adopts it.

### Pass fusion opportunity

- **`compute_rule_deps` + `compute_scc`** are called twice inside the
  normalizer loop. Each `compute_scc` internally calls
  `compute_rule_deps`. Two dep-rebuilds per iteration × 64 iterations
  = 128 adjacency-list constructions per compile.
- **`mine_recognizers` already unified 9 walks into 1** (Tranche Z.0).
  But the `shape_dispatch` classifier that runs at the tail re-walks
  every rule body **N times** (fixed-point convergence, N up to
  `ir.rules.len() + 2`). On CSS L4 (~500 rules), up to 500 full
  rule-body walks. This is a prime target for fusion into the
  unified walk.
- **`compute_eclass_facts`** (in `classify.rs`) could fuse into
  `project_types` (same walk shape, same fixed-point) but runs
  separately.
- **`recognize_tree` and `mine_recognizers`** both walk every rule body
  bottom-up. `mod.rs:246-250` explicitly does a Phase 2 walk separate
  from the Z.0 unified walk because the two producers' output types
  differ. Would fuse trivially given a unified `Sidecar` enum.

--------------------------------------------------------------------------------

## § 6. Analysis consolidation (AY → BA seeds)

### Memory note `analysis-consolidation` says

> Eliminate AST analysis/; all analysis moves to IR passes as single source
> of truth.

### Current status

- `crates/analysis/` is **LSP-feature code**, not grammar analysis (2 424
  LOC across features/, directives/, state/ — all for `lsp-server`
  integration: completion, rename, inlay hints, formatting-for-LSP).
- There is **no `crates/core/src/graph/` AST-analysis layer** duplicating
  IR analysis — `crates/core/src/graph/` contains `calculate_ast_deps`
  and `tarjan_scc` used only at lower-to-IR time (before the IR exists).
  The IR-level `compute_rule_deps` + `compute_scc` redo the same work
  post-lower.
- **Real analysis consolidation debt**:
  1. `graph/tarjan_scc` (AST-level) + `passes/sets/scc.rs::compute_scc`
     (IR-level) implement Tarjan twice. The AST-level call is used only
     for topological sort before IR lowering (`pipeline/compile.rs:455`).
     Could lower first, then `compute_scc`, deleting ~50 LOC of AST Tarjan.
  2. `graph/calculate_ast_deps` + `passes/sets/deps.rs::compute_rule_deps`
     are the same analysis at different IR levels. Retire the former.
  3. The **five separate sidecars keyed by NodeId** on `GrammarIR`
     (`node_facts`, `context_facts`, `eclass_facts`, `materialization`,
     `delim_scan_configs`, `key_dispatch_configs`, `shape_dict_templates`,
     `keyword_branches`, `disjoint_first_tables`, `pattern_alphabets`,
     `ctns_lifts` — **eleven sidecars**) are an SoA organization masquerading
     as AoS. A single `NodeAnalysis { ... }` with Option fields, indexed
     by NodeId, would collapse 11 HashMap allocations into one Vec.

### LOC estimate for BA consolidation

| Target | LOC saved (approx) | Touchpoints |
|--------|-------------------:|-------------|
| Retire AST Tarjan + `calculate_ast_deps` | ~120 | `graph/mod.rs`, `graph/deps.rs` |
| Collapse 11 NodeId sidecars → 1 `NodeAnalysis` Vec | ~600 (metadata + lookup stubs) | `types/grammar.rs`, every miner |
| Retire duplicate FIRST-byte computations | ~300 | `unordered.rs`, `disjoint_first.rs` (partial), merge into single `FirstCache` |
| Retire `compute_eclass_facts` | ~350 | `materialization/classify.rs:530-870` |
| Retire `PatternAnnotations.is_operator_chain` | ~80 | `node_facts.rs`, `pratt.rs` |

**Total: ~1 450 LOC** of pure consolidation (no semantic change).

### Three candidates for BA consolidation

1. **Retire `classify_materialization::compute_eclass_facts`** — the e-graph
   analysis already computes the same bits. Plumb `egraph.class(id).data`
   into `ir.eclass_facts` during `write_back_optimized`; the materialization
   classifier becomes a pure read from the sidecar.

2. **Retire `PatternAnnotations` legacy annotations** — `mine_recognizers`
   Phase 1 (line 232-240 in `recognizers/mod.rs`) runs a separate walk to
   populate `pattern_annotations`. The only real consumer is
   `pratt::detect_pratt`, which can read `ir.shape_assignments` directly
   (the operator-chain detection is now redundant with `ShapeTag::Pratt`).

3. **Collapse the 11 per-NodeId sidecars into one `NodeAnalysisRow`** —
   SoA → AoS. Every miner writes to `row.delim_scan = Some(..)` or
   `row.recognizer = Some(..)`; every consumer reads the same row by
   NodeId. Halves the HashMap allocation + hash traffic on
   `mine_recognizers`.

--------------------------------------------------------------------------------

## § 7. Recommendations

### R1. Consolidate `EClassFacts` — single source of truth

**Impact**: high (deletes ~350 LOC, closes a noted arch debt).
**Simplicity**: high (mechanical change: plumb egraph analysis out).

The e-graph's `GrammarAnalysis::make` already computes the monotone
lattice. Extract the per-NodeId facts at e-graph write-back time and
populate `ir.eclass_facts` from the authoritative source. Retire
`classify.rs::compute_eclass_facts` (lines 530-870). The materialization
classifier reads a finished map.

Concrete edits:
- Add `extract_eclass_facts(egraph, rule_body_ids) -> EClassFactsMap`
  to `egraph/write_back.rs` (mirror of `extract_ir_node` but carrying
  the fact).
- Wire into `pipeline/compile.rs:591` (after write-back).
- Replace `classify_materialization` pre-seed with a sidecar read.

### R2. Replace classifier linear-cascade with shape lattice

**Impact**: medium-high (retires ~1 400 LOC of overlapping detectors if
combined with R4).
**Simplicity**: medium (requires designing the lattice; the code is
mechanical afterwards).

Define `ShapeLattice` as a partial order over `ShapeTag`. Each detector
produces `ShapeCandidate { tag, rank }`; the classifier picks the
maximal rank. Convergence is a real `Changed` bool (no full
HashMap compare at each iteration — the current approach, shape_dispatch/mod.rs:272).

This unblocks:
- Deleting the prose precedence comments (mod.rs:51-55, mod.rs:287-316).
- Running detectors in parallel (they become pure functions
  `(RuleId, &GrammarIR, &ShapeAssignments) → Option<ShapeCandidate>`).
- Retiring `is_w3_classified` / `is_w4_classified` / `is_classified`
  helpers — those are a flat-boolean approximation of the lattice.

### R3. Fuse shape-dispatch into the unified `mine_recognizers` walk

**Impact**: high (eliminates N rule-body walks per compile, where N is
rule count; CSS L4 pays for ~500 extra walks today per
`shape_dispatch/mod.rs:257-277`).
**Simplicity**: medium (must reorganize the classifier to operate on
per-rule state as it's encountered in the walk rather than in a
post-walk fixed-point).

Today the fixed-point only exists because `AltDispatch` depends on its
branch targets being classified first. A simple topological-order
iteration (SCCs first, then by rule depth) would eliminate the
fixed-point entirely — and the pipeline **already has** the SCC
output cached on `RuleMeta.scc_id` (computed in the normalizer loop at
`compile.rs:555`).

### R4. Retire duplicate miners (DelimScan + BalancedWrap; KeywordStats +
KeyDispatch + Keyword detector; QuotedString + String detector; Identifier
+ HRegex detector)

**Impact**: high (retires ~500 LOC of overlap, consolidates four sidecar
types into one).
**Simplicity**: medium (one migration per miner).

Each triplet / pair above identifies the same structural pattern. Merge
into a single miner that writes a richer record; detectors read the
record.

Example:
```rust
pub struct WrapFacts {
    pub open_byte: Option<u8>,
    pub close_byte: Option<u8>,
    pub inner_is_pair_seq: bool,
    pub inner_is_homogeneous_repeat: bool,
    pub delim_scan_config: Option<DelimScanConfig>,
}
```
The Object / Array / delim-scan emitter code all read from `WrapFacts`
via NodeId. `BalancedWrapMiner` + `DelimScanMiner` + `object::detect_object`
+ `array::detect_array` retire.

### R5. Single `FirstCache` keyed by NodeId

**Impact**: medium-high (closes the "stale FIRST after mutation" bug
noted in § 5 and cited by `unordered.rs:41`).
**Simplicity**: medium (wiring change; each consumer switches to a
lookup).

Populate `ir.first_cache: HashMap<NodeId, CharSet128>` from the e-graph
analysis during write-back; recompute on structural-normalizer mutation
via the existing dispatch-table pass. `disjoint_first.rs`,
`unordered.rs`, `factor_lookahead.rs`, `sort.rs`, and the four other
consumers become pure lookups. Retire the four parallel implementations.

--------------------------------------------------------------------------------

**Summary**: the classifier cone is 8 114 LOC; the e-graph cone is 1 126 LOC;
a conservative estimate is that **~30-40% of the classifier cone is
mechanically retirable** via consolidation with the e-graph substrate
already present in the IR. The primary barriers are (a) the e-graph's
output is discarded post-extraction rather than materialized onto the IR,
and (b) the 11 per-NodeId sidecar HashMaps are an SoA expressing an AoS
analysis row. Both can land in BA.
