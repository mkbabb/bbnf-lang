# LOCK-3 · Family 3B — SINGLE-IR-MULTIPASS (the LLVM shape)

**Thesis.** One rich node arena, refined in place by annotating passes; `BackendShape`,
tape/emit intent, recognizers, cost, and projected types are *attributes keyed by node id*,
never a second tree. The generator's structural crossings collapse from three (source→AST→
`GrammarIr`→`BackendIr`→Rust) to **two** — `source → arena` at parse, `arena → Rust` at emit —
with every analysis and lowering decision living *between*, as side-tables, not as a rebuild.
This is the fewest transpositions that suffice, and it is the only LOCK-3 family under which the
three standing laws (no value-discard, no silent epsilon, preserve rich AST) hold *by
construction* rather than by vigilance.

---

## 1. The tower at HEAD, and where it multiplies representations

The generator today runs three distinct node universes with hard transpositions between them:

- **AST → `GrammarIr`** (arena): `GrammarIr { rules: Vec<Rule>, exprs: Vec<Expr> }`,
  `ir/src/lib.rs:33-39`; nodes addressed by `ExprId(usize)` (`:19`), 10-variant `ExprKind`
  (`:233-271`). This is already a flat SoA arena — the right substrate. Crucially, analyses over
  it *already use the attribute pattern*: `nullability(&GrammarIr) -> Vec<bool>` is a side-table
  indexed by `ExprId` computed by a `Changed`-style fixpoint (`ir/src/lib.rs:320-349`). That
  function is the existing proof-of-shape for the whole family.

- **`GrammarIr` → `BackendIr`** (a *second* recursive tree): `BackendIr`/`BackendExpr`/
  `BackendRule`, `ir/src/lib.rs:351-413`. This enum is `Box`-recursive (not arena-indexed), so it
  **loses `ExprId` identity and the `SourceSpan`** carried on every `Expr` (`:227-231`). The
  transposition is `Passes::lower_expr` (`passes/src/lib.rs:1318-1360`) — a full structural
  rebuild — plus `materialize_rule` (`:1362-1389`), which *splices* new nodes (`SpanMark(Start) ·
  body · SpanMark(End) · TapeEmit · DirectBuild · Return`) around each materializable body. The
  `BackendIr` assembly site is `passes/src/lib.rs:1300-1316`.

- **`BackendIr` → Rust**: `codegen::lower::lower_to_rust` consumes `backend_shape`, `cost_facts`,
  `per_grammar_policy` (`codegen/src/lib.rs:176-184`), walking `BackendExpr`.

Beside these sit the analysis products, *all already attribute-shaped but scattered*:
`ShapeFacts` (`ir/src/lib.rs:537-587`), `Recognizer`s (`:415-422`, produced by
`derive_recognizers`, `passes/src/lib.rs:338`), per-rule `CostFacts`/`ActiveCostFacts`
(`ir/src/cost.rs:4-200`), the `MaterializationRoles`/`MaterializationPlan`
(`passes/src/lib.rs:1391-1490`), the discarded projection `ty` (`ExprKind::Project.ty`, dropped
at `infer_expr`, `passes/src/lib.rs:203`).

**The observation that founds 3B:** `BackendExpr`'s *grammatical* half (`Seq`/`Alt`/`RepeatLoop`/
`OptionalBranch`/`ByteLiteral`/`RegexProgram`/`CallRule`) is a near 1:1 re-encoding of `ExprKind`
— a rename that discards `ExprId` and span. Its *backend* half (`SpanMark`/`TapeEmit`/
`DirectBuild`/`ValueProject`/`Return`) is not new *structure* at all: it is *emit intent attached
to specific grammar nodes*. `materialize_rule` proves this — every backend node it inserts is
keyed to one `RuleId`'s descriptor. So `BackendIr` is an *attribute bundle wearing a tree's
clothing*. 3B strips the clothing.

---

## 2. The mechanism made concrete

### 2a. One arena, keyed identity preserved

Keep exactly the arena that exists: `GrammarIr.exprs: Vec<Expr>`, `ExprId`, 10-variant
`ExprKind`. Do **not** grow `ExprKind` with backend variants (that path is the god-enum
counterexample — §6). `ExprKind` stays purely grammatical; `SourceSpan` (`ir/src/lib.rs:227`)
therefore survives untouched all the way to emission, which is what makes grammar-sourced
diagnostics free.

### 2b. The attribute store (the "analysis manager")

Replace `BackendIr` with a `Lowering` bundle of `ExprId`/`RuleId`-keyed side tables — the LLVM
`AnalysisManager` / MLIR op-attribute model, but Rust-typed and dense:

```
struct Lowering<'g> {
    grammar:  &'g GrammarIr,
    // dense IndexVec<ExprId, T> analyses (nullability is the template)
    nullable:   Box<[bool]>,                    // = ir::nullability today
    first_set:  IndexVec<ExprId, FirstSet>,
    ty:         IndexVec<ExprId, InferredTy>,   // Project.ty threads HERE, never dropped
    // sparse ExprId-keyed emit intent (replaces materialize_rule's spliced nodes)
    emit:       SecondaryMap<ExprId, EmitAction>,   // Span(Start/End) | TapeEmit(kind) | Return
    build:      SecondaryMap<RuleId, DirectBuild>,  // shape + Vec<DirectBuildField>
    project:    SecondaryMap<ExprId, ProjectedLeaf>,
    // whole-grammar / per-rule decisions
    shape:      SecondaryMap<RuleId, BackendShape>, // BackendShape AS ATTRIBUTE
    recognizer: SecondaryMap<RuleId, Recognizer>,
    cost:       SecondaryMap<RuleId, CostFacts>,
}
```

Each field is written by exactly one annotating pass and read by named consumers (Y.13):
`nullability` (`ir/src/lib.rs:320`) → `first_set` → `infer` (`passes/src/lib.rs`, the `Project`
arm at `:203` now *writes* `project`/`ty` instead of discarding) → `derive_recognizers`
(`:338`, writes `recognizer`) → `derive_backend_shape` (`:398`, writes `shape` — where the
LOCK-2 e-graph/CSP decision lands, §4) → `derive_materialization_*` (`:1136-1490`, writes
`emit`/`build`). No pass rebuilds the arena; each returns `&mut Lowering`.

### 2c. Emission is one arena walk that consults attributes

The emitter walks `grammar.exprs` from the entry rule (recursing `ExprId`, not a `Box` tree). At
each `ExprId` it emits the grammatical code for the `ExprKind` and *interleaves* the `emit`
attribute where present — the exact `SpanMark…TapeEmit…Return` sequence `materialize_rule`
splices today (`passes/src/lib.rs:1370-1388`) becomes "if `emit[id]` is `Some`, wrap here." The
`build[rule]` attribute drives `DirectBuild`. `shape[rule]` selects the emit strategy per rule
(the `lower/{offset_tape,sink_only,event_tape,collapsed_stage}.rs` split, R1 §2). This is a
straight port of the existing `codegen/lower/*` walk from `BackendExpr` to `(&Expr, &Lowering)`.

**Net transposition count: two.** Everything else is in-place annotation.

---

## 3. Why 3B *is* the three standing laws

- **No value-discard.** The canonical discard site is `ExprKind::Project { body, .. } =>
  self.infer_expr(*body)` (`passes/src/lib.rs:203`), throwing away `target`/`ty`. Under 3B there
  is no lossy rebuild to discard *into*: the `Project` arm's only job is to *write* `project[id]`
  / `ty[id]`. A dropped value is a *missing attribute write*, caught deterministically by a
  debug-assert over `SecondaryMap` coverage. The `typed-materialization-invariant` ("every `->`
  reaches the tape emitter") becomes a coverage check on one table, not an audit of a rebuild.

- **No silent epsilon.** The silent-epsilon hazard lives in lowering catch-alls — e.g.
  `ExprKind::Annotation { .. } => BackendExpr::Seq(Vec::new())` (`passes/src/lib.rs:1348`), an
  empty node synthesized in a rebuild. With no rebuild, there is no arm that can *fabricate* an
  empty node; an annotation is a `Some(EmitAction)` or absent, and the emitter's match over
  attribute+kind is exhaustive and fail-explicit.

- **Preserve rich AST.** `BackendExpr` *is* the flattening the law forbids: it drops `ExprId`,
  `SourceSpan`, and re-Boxes. 3B never leaves the arena, so parity with the rich typed AST is not
  "preserved through a translation" — there is no translation.

---

## 4. LOCK-2 interop: attributes are exactly where graph decisions land

3B does not own the e-graph/CSP engine (LOCK-2 does); it defines the *seam*. The engine already
produces a decision written as an attribute: `backend_egraph::select(rule_id, candidates) ->
ActiveSelection { shape, .. }` (`passes/src/backend_egraph.rs:36-149`) picks a `BackendShape` via
`EGraph`/`Extractor` and returns it as a per-rule fact. Under 3B this is unchanged in spirit —
`derive_backend_shape` writes `shape[rule_id]`. Two refinements the family enables:

- **Rewrites that *do* mutate the arena** (regex-HIR canon, projection canon) run as passes that
  read the arena, saturate a *scoped* e-graph island, and extract *back into the arena in place* —
  the Cranelift aegraph model (a streaming append-and-union over an acyclic node table,
  single-pass), not a persistent parallel graph. Grammar rule bodies form a DAG once recursion is
  cut at `Ref` (`ExprKind::Ref.target`, `ir/src/lib.rs:253-256`), so the *acyclic* e-graph
  precondition holds for intra-rule rewriting; inter-rule recursion is handled by keying rewrites
  to `RuleId` boundaries — the same boundary `materialize_rule` already respects.
- **Per-site cost** feeds the CSP as it does now (`DecisionCspFacts`, `ir/src/cost.rs:202-248`);
  the CSP writes one attribute per rule. `ActiveCostFacts` (`cost.rs:173-200`) is the provenance
  record for that write.

The e-graph's own *analysis* discipline — data attached to each e-class, merged on union (egg's
`Analysis` trait) — is the same pattern as 3B's attribute store one level down, the deepest
argument that "annotate in place" is the SOTA idiom, not a shortcut.

---

## 5. Prior art (citations)

- **LLVM new PassManager + AnalysisManager** — one `Module`/`Function`/`Instruction` IR mutated
  in place; analyses (dominators, LoopInfo, AA) are *cached results keyed by IR unit* with explicit
  invalidation, never a parallel IR. This is `Lowering`'s direct ancestor: side-table analyses over
  one mutable IR. (llvm.org "Writing an LLVM Pass" / new PM docs.)
- **MLIR — one IR, many dialects, attributes on operations, progressive lowering.** "Every
  operation at every level is first-class IR… dialects can be mixed within a single module";
  lowering is *pattern-rewrite of ops in place* (DialectConversion), and scheduling/semantic facts
  ride as *attributes on ops*. This is the exact model for "BackendShape as attributes" and "fewest
  transpositions": bbnf's grammar-level nodes and their lowered emit-intent coexist in one module,
  attributes carrying the backend decision. (Lattner et al., MLIR, CGO 2021; https://mlir.llvm.org.)
- **Cranelift aegraph** — an *acyclic* e-graph that is a "streaming journal of nodes": build a
  node, add equivalents, union in place, refer forward by latest id; the node language is a *union
  of CLIF and machine instructions* so lowering rules are rewrites within one IR. Grounds §4's
  in-place, single-IR e-graph rewriting. (cfallin.org/blog/2026/04/09/aegraph;
  bytecodealliance/rfcs cranelift-egraph.md; EGRAPHS 2023.)
- **GHC Core** — a small typed IR with dozens of Core-to-Core passes, each transforming the same
  representation in place; the canonical "one IR, many passes" success story at compiler scale.
- **egg `Analysis`** — computed data attached per e-class and merged on `union`
  (https://docs.rs/egg): the annotation-lattice idiom 3B mirrors.
- **Counter-example, named honestly: rustc HIR → THIR → MIR.** rustc *does* use multiple IRs. But
  each rustc IR serves a *distinct* client (name resolution/type-check vs pattern exhaustiveness vs
  borrow-check/codegen) over a *whole language*. bbnf's surface is one small grammar algebra; the
  `BackendExpr` split buys no new client — its grammatical half duplicates `ExprKind` and its
  backend half is attributes. The multi-IR tower (family 3A) pays a round-trip/parity-oracle tax
  justified when levels diverge semantically and unjustified here. 3B's claim: bbnf is on the
  GHC-Core/Cranelift side of that line, not the rustc side.

---

## 6. Named risks and counterexamples

1. **The god-enum.** If backend node kinds are added to `ExprKind` (one enum for both levels),
   every front-end pass must match backend variants — a kitchen-sink that violates the
   no-god-modules edict. **Resolution:** attributes-not-nodes (§2a). `ExprKind` never grows;
   `EmitAction`/`DirectBuild` live in `Lowering`, not the arena. The prototype must *prove the
   emitter needs no backend node kind in `ExprKind`* to walk correctly.
2. **The MLIR verifier gap.** MLIR/LLVM tolerate one mutable IR because a runtime *verifier*
   re-checks invariants after every pass; Rust's exhaustive `match` is the compile-time analogue,
   but attribute *coverage* (did every `Project` write `project[id]`?) is not enforced by the type
   system. **Resolution:** a `Lowering::verify()` pass asserting per-table coverage against the set
   of nodes that require it (every `ExprId` whose kind is `Project` has `project[id]`; every
   materializable `RuleId` has `build` and matching `emit` marks) — fail-explicit, run as a
   debug-assert at each pass boundary. This is the price of one IR and is a first-class deliverable,
   not an afterthought.
3. **Attribute-map dtype / dense-vs-sparse.** `nullable`/`first_set`/`ty` are total (dense
   `IndexVec`); `emit`/`build`/`recognizer`/`shape` are sparse (`SecondaryMap`). Choosing wrong
   wastes memory or forces `Option` noise. **Resolution:** dense for whole-arena analyses, sparse
   for per-decision attributes — the split is mechanical from "is this defined at every node?".
4. **Pass ordering / stale attributes.** In-place mutation risks a pass reading an attribute a
   later rewrite invalidated (LLVM's invalidation problem). **Resolution:** the grammar pipeline is
   a fixed DAG of passes with no fixpoint *across* passes (only within, à la `nullability`), so
   ordering is static and declarable; an e-graph rewrite that mutates the arena invalidates
   downstream analysis attributes, which the pipeline recomputes rather than patches (the
   `Changed`-flag convergence law applies *within* a pass, not across the tower).
5. **E-graph cyclicity.** Recursive grammars are cyclic; the aegraph model needs acyclicity.
   **Resolution (§4):** cut at `RuleId` boundaries — intra-rule bodies are acyclic DAGs; recursion
   is a `CallRule`/`Ref` edge the e-graph treats as an opaque leaf. The prototype must confirm a
   left-recursive or mutually-recursive grammar (sheets, CSS) survives this cut.
6. **Regression risk against a working tree.** `BackendIr` + `lower_expr` + `materialize_rule` are
   live and passing on json/css/sheets. **Resolution:** the prototype is a *shadow* — build
   `Lowering` alongside `BackendIr`, emit from both, diff byte-for-byte on all three live grammars
   before deleting `BackendIr`. Byte-identity is the migration gate (mirrors GN-3's derived≡hand
   discipline).

---

## 7. Integration seams (where extant code serves)

- **Delete:** `BackendIr`/`BackendExpr`/`BackendRule` (`ir/src/lib.rs:351-413`); `lower_expr`
  (`passes/src/lib.rs:1318-1360`); `materialize_rule`'s node-splice (`:1362-1389`, becomes an
  `emit`-attribute writer); the `BackendIr` assembly (`:1300-1316`).
- **Keep as the template:** `nullability` (`ir/src/lib.rs:320-349`) — the exemplar dense attribute
  pass; its `Changed`-fixpoint shape is copied for `first_set`.
- **Rehome as attribute writers (minimal change):** `derive_recognizers` (`passes/src/lib.rs:338`)
  → writes `recognizer[rule]`; `derive_backend_shape` (`:398`) → writes `shape[rule]` (the
  `backend_egraph::select` call, `backend_egraph.rs:36`, is unchanged, only its sink is);
  `derive_materialization_roles`/`_plan` (`:1391-1490`, `:1136-1391`) → write `emit`/`build`;
  `infer`'s `Project` arm (`:203`) → writes `project`/`ty` (this is *also* the exact wire GN-3.W2
  needs — 3B and the type plane converge here).
- **Retarget the emitter:** `codegen::lower::lower_to_rust` and the `lower/{offset_tape,sink_only,
  event_tape,collapsed_stage,css_scan}.rs` walkers (R1 §2) change input from `&BackendExpr` to
  `(&Expr, &Lowering)` — same emit logic, different node source.
- **Cost/provenance unchanged:** `CostFacts`/`ActiveCostFacts`/`DecisionCspFacts`
  (`ir/src/cost.rs`) are already attribute records; they become `Lowering.cost` entries verbatim.
- **PROVE-12 residualization is a corollary, not a special case.** The const-fold delimiter-mask
  win (`prototypes/PROVE-12/patch.diff`, +18 LOC, cyc/B 0.81-0.98×) is emit-time specialization
  from a *grammar-constant attribute* (the delimiter STOP-set). Under 3B that STOP-set is an
  attribute (`emit`/recognizer alphabet) the emitter reads and bakes as a const generic — exactly
  what PROVE-12 does by hand, generalized to "any grammar-derived stop-set" (its own §7
  generalization note). No new mechanism; residualization is "the emitter reads an attribute and
  emits a constant."

---

## 8. What a prototype must test to move this family

1. **Emitter needs no backend node in `ExprKind`.** Re-emit the live JSON grammar walking
   `(&Expr, &Lowering)` where `emit`/`build`/`shape` reproduce `materialize_rule`'s spliced
   sequence, and diff the generated `grammars/json/generated.rs` **byte-for-byte** against HEAD.
   Success = byte-identical from attributes alone (proves §2a, refutes risk 1).
2. **Coverage verifier catches a dropped `->`.** Wire `infer`'s `Project` arm to write `project`,
   delete one write, and confirm `Lowering::verify()` fails explicitly (proves no-value-discard is
   mechanically enforced, risk 2).
3. **Span survives to emission.** Emit a deliberate error and confirm the `SourceSpan` on the
   offending `ExprId` reaches the diagnostic — impossible under `BackendExpr` (which drops span),
   trivial under 3B. Proves the rich-AST-preservation dividend.
4. **Acyclic e-graph rewrite in place on a recursive grammar.** Run a single intra-rule
   projection-canon rewrite over the sheets grammar (mutually recursive), extracting back into the
   arena, and confirm `derive_backend_shape` reads correct post-rewrite attributes (proves risk 5
   cut works, grounds §4).
5. **Transposition count is two.** Instrument the pipeline; assert exactly two structural node
   materializations occur (parse-build, emit-walk) and every intermediate pass is `&mut Lowering`
   (the family's defining measurement).
6. **Shadow-diff on all three live grammars** (json/css/sheets) before `BackendIr` deletion —
   byte-identity as the migration gate (risk 6).

The move-the-family artifact is test 1: a byte-identical JSON re-emit from the attribute store
with `ExprKind` ungrown proves the entire thesis — that `BackendIr` was attributes wearing a
tree's clothing, and one IR suffices.

Sources: https://mlir.llvm.org · https://cfallin.org/blog/2026/04/09/aegraph/ ·
https://github.com/bytecodealliance/rfcs/blob/main/accepted/cranelift-egraph.md · https://docs.rs/egg
