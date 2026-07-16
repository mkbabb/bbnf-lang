# LOCK-3 · Family 3B — SINGLE-IR-MULTIPASS (the LLVM shape)

**Thesis.** One rich IR — the `GrammarIr` arena — refined in place by annotating passes.
BackendShape, materialization, recognizers, cost, and types are all *attributes* keyed by
`ExprId`/`RuleId`, not a second tree. `BackendIr`/`BackendExpr` are deleted. Emission is a
single read-only fold over `(GrammarIr, Attributes)`. The fewest transpositions that suffice
is exactly one: source text → IR at parse time. Everything after is in-place refinement plus a
read-only emit.

---

## 1. The mechanism made concrete

### 1.1 What the tower is today (verified, path:line)

There are **three** IR encodings and two whole-tree transpositions between them:

1. **Parse transposition** — `.bbnf` → `grammar::parse_grammar` → `GrammarIr { name, source_hash,
   rules: Vec<Rule>, exprs: Vec<Expr> }` (`crates/ir/src/lib.rs:33-39`). The arena is SoA:
   `Expr { id, kind: ExprKind, span }` indexed by `ExprId(usize)` (`ir/lib.rs:18,226-231`);
   `ExprKind` is a 10-arm enum (`ir/lib.rs:234-271`: Seq, Alt, Repeat, Optional, Literal, Regex,
   Ref, Annotation, Project, Discard). This is the ONE lawful transposition.

2. **Backend transposition** — `passes::compile` (`crates/passes/src/lib.rs:32-68`) builds
   `BackendIr { grammar_name, entry_rule, recognizers, rules: Vec<BackendRule>, shape_facts }`
   (`ir/lib.rs:351-358`) via `extract::single_plan` (`passes/lib.rs:1283`). Its core,
   `lower_expr` (`passes/lib.rs:1318-1362`), is a **1:1 recursive copy** of `ExprKind` into
   `BackendExpr` (`ir/lib.rs:376-413`): Seq↔Seq, Alt↔Alt, Repeat↔RepeatLoop, Optional↔
   OptionalBranch, Literal↔ByteLiteral, Regex↔RegexProgram (adds a *derived* `span_kind`,
   `passes/lib.rs:1345`), Ref↔CallRule, Project↔ValueProject; Discard and Annotation are lowered
   away (`passes/lib.rs:1352-1361`). `materialize_rule` (`passes/lib.rs:1364-1391`) then splices
   `SpanMark(Start) · body · SpanMark(End) · TapeEmit{kind} · DirectBuild{shape,fields} · Return`
   around a rule body **from a per-rule descriptor** (`MaterializationPlan.descriptors:
   HashMap<RuleId, MaterializationDescriptor>`, R6 §5c).

3. **Emit transposition** — `codegen::lower_to_rust(backend: &BackendIr, ctx: &LowerCtx)`
   (`crates/codegen/src/lower/rust.rs:32`) walks `BackendExpr`, reading side tables through
   `LowerCtx { backend_shape: &HashMap<RuleId,BackendShape>, cost_facts: &HashMap<RuleId,CostFacts>
   }` (`lower/rust.rs:24-32`), then `runtime_generator::emit_sink_only` splices templates.

Everything the two derived passes need is **already a side table keyed by ExprId/RuleId**:
`LayoutFacts` is seven such maps (`passes/lib.rs:91-100`: rule_types, node_types, hot_call_graph,
backend_shape, cost_facts, per_grammar_policy, same_substrate_union), `TypeFacts.node_types:
HashMap<ExprId,Type>` (`passes/lib.rs:127-128`), and `nullability` returns a dense
`Vec<bool>` indexed by `ExprId.0` (`ir/lib.rs:320-349`). **The attribute store already exists,
unformalized.**

### 1.2 The 3B diagnosis: the second IR is pure redundancy

`BackendExpr` renames `ExprKind` node-for-node and computes one derived field inline (`span_kind`);
`materialize_rule` splices a wrapper that is fully determined by a **per-rule attribute**. Neither
is a transposition into a different *shape* — it is a copy plus attribute-injection. The proof of
the redundancy is structural, and it is visible in the maintenance tax: adding `->` to the language
required editing `ExprKind::Project` (`ir/lib.rs:261`) **and** `BackendExpr::ValueProject`
(`ir/lib.rs:407`) **and** `lower_expr`'s Project arm (`passes/lib.rs:1352`) **and** `nullability`
(`ir/lib.rs:337`) **and** `pretty_expr` (`ir/lib.rs:185`) — five edit sites in lockstep, two of
which exist only to keep the second enum synchronized. The `debug_assert` at `passes/lib.rs:58-61`
(cost_facts.chosen == backend_shape) is a second symptom: two side tables forced to agree because
the model lets them disagree.

### 1.3 The 3B substrate (LLVM/GHC/MLIR shape)

**One IR** = the `GrammarIr` arena, treated as an LLVM `Module`. Two pass kinds:

- **Analysis pass**: reads `(arena, attrs)`, writes attributes. Nullability, first-set, types,
  `span_kind`, recognizers, materialization descriptors, backend-shape selection
  (`PriorityStep::ALL`, `ir/cost.rs:276`), cost. Output lands in a single formalized
  `Attributes` store (§1.4). These already exist as `derive_*` functions producing HashMaps.

- **Transform pass**: mutates the arena **append-only** and declares what it invalidates
  (LLVM `PreservedAnalyses`; the memory `changed-flag-convergence` law's `Changed` bool). The
  only mutations the current fleet needs are (a) desugar `Discard`/`Annotation` once in the arena
  (today done at copy-time inside `lower_expr`) and (b) the materialization wrapper — which 3B
  keeps as an *attribute the emitter reads*, not a spliced subtree, unless a grammar needs
  sub-expression materialization (§4 R3).

**BackendShape as attribute.** `BackendShape` (`ir/lib.rs:360-367`: EagerTape|OffsetTape|EventTape
|SinkOnly|CollapsedStage) stops being carried on a `BackendIr` and lives in
`attrs.backend_shape[RuleId]` — where it *already is* (`LayoutFacts.backend_shape`,
`passes/lib.rs:96`). `LowerCtx` already reads it as `&HashMap<RuleId,BackendShape>`; 3B makes the
*whole* emitter do this and drops the `BackendIr` argument entirely.

**Emission** = one read-only fold `emit(grammar: &GrammarIr, attrs: &Attributes) -> String`:
for each `Rule`, read `attrs.backend_shape[rule.id]`, `attrs.materialization[rule.id]`,
`attrs.is_entry[rule.id]`, and fold `rule.body` over the arena reading per-node attributes
(`attrs.span_kind[expr]`, `attrs.node_type[expr]`, `attrs.recognizer[expr]`). The
`BackendExpr::Entry` wrapper (`passes/lib.rs:1296`) becomes an `is_entry` bit; `SpanMark/TapeEmit/
DirectBuild/Return` become the emitter's materialization prologue/epilogue read from the descriptor.

Net: three IR types (`GrammarIr`, `BackendIr`, `LoweredRust`) → two (`GrammarIr`, output). Two
transpositions → one. Two node enums → one.

### 1.4 The `Attributes` store — the formalization

`crates/ir/src/attr.rs` (new): `Attributes` is `LayoutFacts` promoted to a first-class type with
typed slots and a density discipline — **dense `Vec<T>` indexed by `ExprId.0`** where the analysis
is total (the `nullability` `Vec<bool>` is the archetype, `ir/lib.rs:321`), **sparse `HashMap`**
where partial (materialization descriptors are per-*some*-rules). A slot carries a validity token;
`PreservedAnalyses`-style invalidation lives here. This is MLIR's attribute dictionary and rustc's
query cache, keyed the way skinny already keys — by arena index.

### 1.5 Two strata (the parsimony cut — Y.13 honest)

The current pipeline has **zero in-place mutation**: `normalize` clones (`passes/lib.rs:27-30`),
every `derive_*` is pure analysis, and `lower_expr` is the only "mutation" — which is really the
transposition 3B deletes. Therefore LLVM-style invalidation is **consumer-less today** (Y.13
forbids consumer-less substrate). 3B ships in two strata:

- **Stratum 1 — THE COLLAPSE (lands now).** Delete `BackendIr`/`BackendExpr`/`BackendRule`
  (`ir/lib.rs:351-413`), `lower_expr`, `materialize_rule`, `single_plan`. Move `span_kind` and the
  materialization descriptor into `Attributes`. Rewrite the emitter as the `(GrammarIr, Attributes)`
  fold. Consumer = the emitter, same wave. No invalidation machinery (no mutating pass to consume
  it). Gate: byte-identical regen on all three live grammars.

- **Stratum 2 — THE GROWTH (lands with its first Transform).** The `Pass`/`Analysis`/`Transform`
  trait split + `PassManager` + `PreservedAnalyses` land **with** their first mutating consumer —
  Discard-desugar as an in-place append-only rewrite, or (interlock) LOCK-2's e-graph rewrite
  island as a Transform over the one IR. Never before; invalidation without a mutator is the
  consumer-less defect the constitution (Y.13/viii) forbids.

---

## 2. Prior art (citations)

- **LLVM** — one mutable IR (`Module`/`Function`/`Instruction`); `PassManager` pipeline;
  analysis results in `AnalysisManager` with explicit `PreservedAnalyses` invalidation
  (llvm.org/docs/NewPassManager.html, WritingAnLLVMNewPMPass); instruction metadata/attributes
  (`!range`, `nonnull`, `nsw` — LangRef §Metadata/§ParameterAttributes). Target lowering info is
  attached late (SelectionDAG/MIR), not a new IR level until the last responsible moment — the
  exact "BackendShape as attribute, keep one IR" posture. **Directly supports 3B.**
- **GHC Core** — the canonical single-IR-multipass: Core is refined by a long chain of
  Core-to-Core passes (the Simplifier runs itself→itself repeatedly); STG/Cmm are *late* lowerings
  (GHC Commentary, "Compiler/Core-to-Core pipeline"; Peyton Jones & Santos, "A transformation-based
  optimiser for Haskell"). Strongest exemplar that many passes over one IR beat a deep tower.
- **MLIR** — operations carry a `DictionaryAttr`; `-canonicalize` rewrites in place; progressive
  lowering keeps the op set and refines by attributes/passes (mlir.llvm.org, "Understanding the IR
  Structure", "Operation Canonicalization"). Best prior art for "shape/backend facts as first-class
  attributes on the one IR." Note the honest counterpoint: MLIR *does* use dialect lowering — but
  its dialects share one op/attr substrate, which is 3B's "one IR + attributes," not 3A's distinct
  typed levels.
- **rustc** — the query system memoizes analyses keyed by `DefId` (isomorphic to skinny's ExprId/
  RuleId keying); MIR is refined **in place** by `mir_transform` passes (rustc-dev-guide,
  "The MIR (Mid-level IR)", "Query evaluation"). Counterpoint to name: rustc chose a *tower*
  (HIR→THIR→MIR) for the front half — evidence the tower is defensible; 3B's rebuttal is that
  skinny's "levels" are 1:1 renames, not the genuine representation changes that justify rustc's
  towers (HIR is untyped-ish, THIR is typed, MIR is CFG — real shape changes; `ExprKind`→
  `BackendExpr` is neither).
- **egg / egglog** — e-class analysis is an attribute attached to e-classes, refined to a fixpoint
  under a lattice (egraphs-good.github.io; Willsey et al., "egg: Fast and Extensible Equality
  Saturation"). 3B's `Attributes` store is the non-saturating monotone cousin; the LOCK-2 seam is
  exact: if a slot forms a lattice, the same store feeds an e-graph island as a Transform pass.
- **The `changed-flag-convergence` and `no-orthogonal-codepaths` memory laws** are the internal
  prior art: fixpoint via `Changed` bool (matches LLVM `PreservedAnalyses`), and one arena/one
  substrate (matches the single-IR thesis).

---

## 3. Integration seams (path:line where extant code serves)

| Seam | Location | 3B action |
|---|---|---|
| The redundant copy | `passes/src/lib.rs:1318` `lower_expr` | DELETE; emit folds `ExprKind` directly |
| The materialization splice | `passes/src/lib.rs:1364` `materialize_rule` | move descriptor to `attrs.materialization[RuleId]`; emitter reads it |
| `single_plan` (builds BackendIr) | `passes/src/lib.rs:1283` | DELETE; `compile` returns `(GrammarIr, Attributes)` |
| Second node enum | `ir/src/lib.rs:376-413` `BackendExpr` | DELETE |
| Second IR container | `ir/src/lib.rs:351-358` `BackendIr`/`BackendRule` | DELETE |
| BackendShape enum | `ir/src/lib.rs:360-367` | KEEP as attribute value type |
| The attribute store (unformalized) | `passes/src/lib.rs:91-100` `LayoutFacts` | promote to `ir::Attributes` (`ir/src/attr.rs`) |
| Dense-attribute archetype | `ir/src/lib.rs:320-349` `nullability` | template for `Vec<T>`-indexed slots |
| Emitter reads attrs already | `codegen/src/lower/rust.rs:24-32` `LowerCtx` | generalize: drop `&BackendIr`, take `(&GrammarIr,&Attributes)` |
| span_kind derived inline | `passes/src/lib.rs:1345` `backend_span_kind` | move to a `span_kind` analysis attribute on Regex nodes |
| Shape-selection pass | `ir/src/cost.rs:276` `PriorityStep::ALL` + `passes/lib.rs:407` `derive_backend_shape_with_diagnostics` | becomes an Analysis pass writing `attrs.backend_shape` |
| Entry wrapping | `passes/src/lib.rs:1296` `BackendExpr::Entry` | becomes `attrs.is_entry[RuleId]` |
| Redundancy assert (symptom) | `passes/src/lib.rs:58-61` | deleted when the two tables become one slot |
| Pass-order driver | `passes/lib.rs:32-68` `compile` | becomes the Stratum-2 `PassManager` schedule |

Interlocks: GN-3 (type plane) threads `Project.ty` into `attrs.node_type`/`DirectBuildTarget`
(`passes/lib.rs:204`, R6 §8b) — an Analysis pass under 3B, no BackendExpr to touch. GN-2.W1's
recognizer lowering (`passes/lib.rs:338`) writes `attrs.recognizer[expr]`. LOCK-2's e-graph
islands are Stratum-2 Transform passes. LOCK-1's type e-classes are a lattice-valued attribute
slot.

---

## 4. Named risks and counterexamples

- **R1 — attribute invalidation is the LLVM tar-pit (the hardest thing).** A Transform that
  mutates the arena silently invalidates analyses keyed against the old node set. *Counterexample:*
  desugaring `a << b` (`ExprKind::Discard`, `ir/lib.rs:266`) into `Seq[a,b]` produces new ExprIds;
  a first-set attribute keyed by the old id now mis-reads. *Guard:* `PreservedAnalyses` — a
  Transform declares its invalidation set; the manager recomputes. This is precisely why Stratum 2
  lands only WITH a mutating consumer that exercises it. Stratum 1 has no mutator and is immune.

- **R2 — arena renumbering breaks every keyed attribute.** *Counterexample:* replacing a node
  in place shifts `Vec<Expr>` indices. *Guard:* **append-only** arena rewrites (never renumber);
  a rewritten node's slot is repointed (`rule.body` reassigned), the dead node left unreachable and
  either GC'd by a final prune pass or simply not walked. Keys stay stable. `add_expr`
  (`ir/lib.rs:51`) is already append-only.

- **R3 — materialization is per-rule, but might need to be per-subexpression.** 3B keeps the
  wrapper as a per-`RuleId` attribute; the current descriptor map is per-rule
  (`HashMap<RuleId,...>`, R6 §5c) and `TapeEmit` wraps whole rule bodies, so this holds for the
  live fleet. *Counterexample:* a grammar wanting a container materialized around a nested
  sub-expression (not the rule body) needs a per-`ExprId` materialization slot. *Guard:* the store
  admits both keyings; the prototype must confirm no fleet grammar (json/css/sheets/csv) needs the
  finer key before committing to the coarse one.

- **R4 — same-substrate-union must stay uniform.** The `no-orthogonal-codepaths` law forbids
  per-node backend shapes drifting apart. An attribute model *could* express per-node shapes.
  *Guard:* `backend_shape` is keyed by `RuleId` only, and the `same_substrate_union` validator
  (`passes/lib.rs:57`, `SameSubstrateUnionFacts`) asserts union-consistency — it survives as a
  verifier pass over the store.

- **R5 — losing the tower's per-level contract (the 3A rebuttal to answer).** A sibling family
  argues three checkable levels catch lowering bugs a single mutable IR hides. *3B answer:* the
  byte-identical regen gate (§5.1) is a stronger end-to-end oracle than per-level round-trips for
  the current fleet, and a `verify(grammar, attrs)` pass (assert every emitted-node's attributes
  are live and consistent) recovers the intra-pipeline check without a second tree. The honest
  residual: if the pipeline grows many genuinely shape-changing lowerings (à la rustc HIR→MIR), the
  tower's cost may re-earn itself — 3B's boundary is "1:1 renames don't justify a level."

---

## 5. What a prototype must test to move 3B

1. **The killer test — collapse to byte-identity.** Delete `BackendIr`/`BackendExpr`/`lower_expr`/
   `materialize_rule`/`single_plan`; re-express `span_kind`, `is_entry`, and the materialization
   descriptor as `Attributes` slots; rewrite the emitter as the `(GrammarIr, Attributes)` fold.
   GATE: `cargo xtask regen-json && check-json && regen-css && check-css-l4-declaration-values &&
   regen-sheets && check-sheets` — **all byte-identical**. If the attribute-reading emitter
   reproduces every live grammar's `generated.rs` exactly, the second IR was pure redundancy and
   Stratum 1 is proven. This is falsifiable: a missed attribute or an emit-order divergence breaks
   the diff.

2. **LOC-delta measurement.** Count deleted (`BackendExpr`+`BackendIr`+`BackendRule`+`lower_expr`+
   `materialize_rule`+`single_plan`+the `LowerCtx` `&BackendIr` plumbing ≈ 250-350 LOC) vs added
   (`attr.rs` store + emitter fold). 3B wins iff net LOC drops **and** byte-identity holds.

3. **Extensibility count (the maintenance-tax proof).** Reproduce the `Project` addition under 3B
   and count edit sites: tower = 5 (`ExprKind`, `BackendExpr`, `lower_expr`, `nullability`,
   `pretty`); 3B target = 2 (`ExprKind`, emitter + any analysis that reads it). Measured, not
   asserted.

4. **Stratum-2 invalidation smoke test (only if a mutator exists).** Land Discard-desugar as an
   append-only Transform with a declared invalidation set; run an analysis before it, mutate,
   and assert every subsequent attribute read hits a live, current node (a `debug_assert`
   over the store), and that regen stays byte-identical. Do NOT build the invalidation machinery
   without this consumer.

**Bottom line for 3B:** the strongest form is minimal — the second IR is deletable *today* with a
byte-identical gate, because every "lowering" is either an analysis already producing a keyed side
table or a 1:1 rename. The LLVM apparatus (Pass trait, PreservedAnalyses) is the growth path that
lands with its first real Transform, never speculatively.
