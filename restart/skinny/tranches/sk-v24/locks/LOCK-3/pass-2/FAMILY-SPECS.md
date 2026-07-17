# LOCK-3 — THE IR TRANSPOSITION SPINE · PASS-2 FAMILY SPECS (synthesis)

*One cogent specification per family while the routes remain incompatible — architecture, mechanism,
integration seams (path:line), the gates each carries (born-RED, named owner), and its exact open
questions. Then the incompatibility map: which families cannot coexist and precisely why. Distilled by
SYNTHESIS (Fable-tier) over the pass-2 research corpus (A/B/C/D/priorart, all pinned `06e70053f`), the
four pass-2 prototypes (PROTO-1, PROTO-2, PROTO-1-t1, PROTO-2-3A), both critics, the LOCK-3 REGISTRY,
and CROSS-LOCK. Elegance earns nothing on its own; a family stalled at a missing primitive of
difficulty equal to the original problem is marked **BLOCKED-candidate**.*

---

## 0. VERIFIED BASELINE (re-greped by SYNTHESIS on live this pass)

**Live HEAD = `06e70053f`** (`git rev-parse HEAD` = `06e70053f67f4ea1…`). This is **exactly the pin all
five research reports used**. Base-drift is a **non-issue**: the prior-registry fold pin `c43126422`
and the lock-mint `b3cb8c16b` are both ancestors of HEAD, and `git diff --stat {c43126422,b3cb8c16b}..HEAD
-- skinny/crates` is **empty** — the intervening commits touched only `restart/.../tranches/` (research
corpus + fold ledgers), never `skinny/crates`. The code tree every LOCK-3 prototype measured is
**byte-identical to HEAD**; the PROTO-1 / PROTO-1-t1 / PROTO-2-3A byte-identity floors transfer verbatim.
XL-H wrong-artifact-risk is **discharged** for every LOCK-3 prototype this pass.

The source tree lives at top-level `skinny/crates/` (NOT `restart/skinny/`, which holds docs only).
Anchors re-verified by hand on `06e70053f` (all CONFIRMED; each research report independently re-found them):

| Fact | Live truth (`06e70053f`, verified this pass) |
|---|---|
| `->` ty-drop | `passes/src/lib.rs:204` `ExprKind::Project { body, .. } => self.infer_expr(*body)?` — both `target` + `ty` bound by `..`, discarded |
| `:203` non-defect | `ExprKind::Annotation { .. } => Type::Builtin(BuiltinTy::Unit)` — legit untyped `@ws`/`@cap` |
| name-gate survives | `has_rule("value")` @ `codegen/src/runtime_generator.rs:98` (comment claims "shape-based"; the predicate is still a rule-name query) |
| schema is Struct-only | `DirectTypeKind` @ `codegen/src/direct_schema.rs:52` has **one** variant (`Struct{fields,ignored,unknown}`); **no `Sum`** |
| `.target` is a DEAD field | `DirectBuildField.target: Option<DirectBuildTarget>` is `None` at every offset-tape descriptor (`passes/src/lib.rs:1157,1177,1206,1228,…`), `Some(..)` nowhere, never read; the sole `DirectSchemaSet` producer is the hand file `xtask/src/real_typed_schema.rs` |
| `from_ir`/`TypeAnalysis` absent from live skinny | **zero hits** in `skinny/crates`; exist ONLY in legacy `crates/core/src/backend/driver/analysis.rs` over the legacy `GrammarIR` (a reference-resolved grammar, no `node_types`) |
| `CollapsedStage` gate is DEAD | `admits_collapsed_stage` @ `passes/src/lib.rs:959` = `target.avx512bw && matches!(…Entry(_))`; `avx512bw` is `cfg!(target_arch="x86_64")`-derived — **permanently false on aarch64** (SPEC §0: x86 dead) |

**The spine as an edge-DAG (research-A §1, the sharpest cartography of the pass).** `passes::compile`
(`passes/src/lib.rs:32-68`) drives a chain that research-A labels as **≥5 edges over ~4 distinct value
levels**:

```
.bbnf ─E1 parse─▶ GrammarIr ─E2 infer─▶ node_types:HashMap<ExprId,Type> ─E3 lower─▶ BackendExpr(13-arm)
      ─E4 materialize─▶ BackendExpr' ─E5 sink_only─▶ SinkOnlyProgram ─E6 emit─▶ .rs
```

- **E4 is a rewrite *within* the BackendExpr sum** — `materialize_rule` (`passes:1362`) splices
  `Seq[SpanMark(Start), body, SpanMark(End), TapeEmit{kind}, DirectBuild{shape,fields}, Return]`, all
  arms of the one 13-arm `BackendExpr` (`ir/src/lib.rs:376-413`), keyed `descriptors.get(&rule_id)`. Not
  a level. Its output carries **no datum absent from `GrammarIr` + `MaterializationPlan`**
  (`derive_materialization_plan` @ `passes:1139` takes **only** `grammar`, verified at call site) — so
  E4 is a **pure derivation recomputable inside the fold** (PROTO-2, by trace).
- **L2 `SinkOnlyProgram`** (E5) flattens the materialized L1 into rosters (`direct_shapes`, `span_kinds`,
  `literals`) — a re-encoding, not a new value level.

**Two facts bind every family.** (1) The tower is **3B-shaped today** — one attribute-annotated
`LayoutFacts` bundle (`passes:93` `node_types: HashMap<ExprId, Type>`, populated `:212`, carried in
`PipelineOutput.layout_facts`), a materialized `BackendExpr`/`SinkOnlyProgram` plane, then a **fractured**
emission stage (`json_sink_direct/`, `css_scan_direct/`, `general_descent/`, `json_typed_direct.rs`, gated
by `has_rule("value")`@98). LOCK-3 either **hardens** (3A/3B) or **re-founds** (3C/3D); every divergence
from the GN codex is a **named supersession** (Constitution ix). (2) The **`Project.ty`@204 thread** is
single-owner and **DISCHARGED for the fact plane** (PROTO-1 RAN: RED [4 spans] → GREEN, zero emit
footprint, four regen checks byte-identical) — but the schema stays **starved** (`.target` dead; sole
producer is the hand file), and both the CONSUME half and the consistency CONTRACT over the threaded
value remain open (cross-lock directive 2).

---

## 1. FAMILY 3A — CONTRACT-TOWER · ADVANCED (as discipline; standalone tower BANKED) · 60%

**Architecture.** A small fixed tower whose **transposition edges each carry a decidable contract**
`C_k(in_k, out_k)`, run at regen and in CI — the MLIR `--verify-each` / rustc `-Zvalidate-mir` /
GHC `-dcore-lint` discipline, **strengthened from well-formedness to refinement/parity** (translation
validation: Pnueli/Siegel TACAS'98; Alive2 PLDI'21; the corpus-bounded analogue of CompCert per-pass
simulation and Nanopass conformance). The standalone typed-multi-level-tower is **BANKED** (the "three
distinct levels" count is a design-collapse claim, not a live fact — the live spine has ≥5 edges over
~4 levels); 3A advances **as a discipline layered on the 3B single-IR substrate** — the two **compose**
(§5), with running evidence on both sides.

**Mechanism — three artifacts, oracles already resident.**
1. **Threaded node-type at the infer edge** (the shared `:204` obligation). `LayoutFacts.node_types`
   *already stores* `HashMap<ExprId, Type>`; `:204` is not a missing store but a **replacement** (body's
   structural type inserted for the `Project` node, declared `ty` dropped). Fix: when `ty=Some(name)`,
   resolve `name` to a `Type` (new `Type::Named(String)` arm, `passes:234`; backend-agnostic per the
   `TypeDesc::Named` law) and insert *that* as a **parallel** fact, leaving `classify_projection` reading
   `Project.ty` directly (zero emit footprint).
2. **Per-edge `verify` predicate** — `verify_project_types(g, f)`: for every `Project{ty:Some(_)}`,
   assert `f.node_types[id]` is the resolved declared type. **RAN** (PROTO-1): RED (4 spans) → GREEN,
   zero footprint.
3. **`differential(edge, corpus)` runner** generalizing the two shipping parity digests
   (`bbnf-bench/src/parity.rs:23-62` `assert_parity`; `json_parity.rs:198-266` `strict_product`+`digest`).
   Per-edge digest `d_E`; parity = every `d_E` matches a committed golden. **RAN on the free E3 edge**
   (PROTO-2-3A): 4/4, injected single-arm fault named to its arm with zero false positives across a
   5-fault matrix; E2↔E3 separation proven (E2 fault trips E2 while E3 stays GREEN → localizes to the
   edge, never merely "a failure").

**Integration seams.** E1 `layout::types::infer`/`infer_expr` (`passes:183-213`); E3 `materialize_rule`
(`passes:1362`); E5 `runtime_generator.rs:98` gate; goldens are regen output (clean-regen law binds them).

**Gates it carries (born-RED, named owner).**
- **G-3A-a — name→`Type` resolution** (owner **LOCK-1**, born-RED). Until it lands, `Type::Named(String)`
  is a **nominal** wrapper never structurally equal to the **structural** `infer_expr(body)`, so the
  `≠ body` clause of the verify predicate is **vacuously true whenever `ty=Some`** — the organ verifies
  *threading-presence*, not *declared-vs-inferred consistency*. **The contract is a gate-that-cannot-fail
  until LOCK-1.** Do not assert non-vacuity absent it.
- **G-3A-b — per-ExprId carriage** (owner LOCK-3, latent). `materialize_rule` keys by `RuleId`;
  `node_types`/`Project` are per-`ExprId`. A grammar where two rules project the same sub-rule to
  different `ty` forces ExprId carriage — latent (JSON/CSS have no `->`; Sheets is single-projection).

**Open questions.**
- (i) Does **≥1 Sheets projection exist where declared `ty` constrains BEYOND `infer_expr(body)`**? If
  yes, the contract is non-vacuous *in principle* (LOCK-1 need only resolve the name); if no, report the
  contract **`unverified`**. **This is the sharpest 3A uncertainty and runnable today** (Prototype Brief β).
- (ii) Is `render` (`runtime_generator.rs:78`) a pure projection of L2 (so E4 carries no oracle)?
- (iii) The CSS `Err⇒SinkOnly / Ok⇒Scan` fork (`runtime_generator.rs:40-71`) — first-class edge oracle
  or a declared two-root tower?
- (iv) E3 is a **lossy** correspondence (the sink map erases `SpanMark.kind`/`TapeEmit.kind`), so
  `relift(lower_expr(x))==x` is false on those fields — "free round-trip" over-claims; the honest oracle
  excludes the erased arms (caught at E2). A named property to fold in, not a defect.

**Status: ADVANCED (discipline) · 60%.** The distinctive organ RAN and localizes; but the load-bearing
*contract meaning* (E1-infer non-vacuity) is UNRUN and gate-cannot-fail until LOCK-1. Not
convergence-eligible.

---

## 2. FAMILY 3B — SINGLE-IR-MULTIPASS · ADVANCED (the convergent substrate) · 55%

**Architecture.** One rich IR (`GrammarIr`) refined in place by annotating passes; `BackendShape` and the
materialization decisions carried as **attributes** on the same IR; the fewest transpositions that suffice
(the LLVM pass-manager shape — prior-art §4). The `BackendExpr`/`SinkOnlyProgram` materialized plane is
**deleted** and re-expressed as one `emit(&GrammarIr, &Attributes)` fold; the three emitter *bodies*
(`json_sink_direct::render`, `emit_general_descent`, `css_scan_direct::render`) are genuinely distinct
backends and **stay** — "one fold" unifies emitter *selection*, not the backend bodies.

**Mechanism.** (a) A `classify_materialization`-shaped annotating pass writes `GrammarIr.materialization`
(relocated `derive_materialization_roles` @ `passes:1403`, its role assignment threaded onto the attribute
bundle, **single-homed**). (b) A `project_types`-shaped pass writes `GrammarIr.types` from the `:204`
node-type. (c) `single_plan` (`passes:1283`, today building `BackendIr` via `lower_expr`+`materialize_rule`)
is re-expressed as a fold over `grammar.rules` consulting `Attributes{materialization, types, recognizers,
shape_facts}`, inlining the emit-order template. **Net-LOC:** delete `BackendExpr::{SpanMark,TapeEmit,
DirectBuild,Return}` (~37-line enum) + `materialize_rule` (~30) + `lower_expr` (~45) + the `single_plan`
build; add the fold + `classify_materialization` + `project_types`. Emission-extensibility 5→2
(`BackendShape`'s five variants collapse toward attribute-driven {tape-emit, sink-emit}).

**What is earned (RAN this pass).** PROTO-2 cleared the **fact-layer purity precondition by trace** — the
four `SinkOnlyFacts` accumulations read only `GrammarIr + backend_shape + descriptor`, so `materialize_rule`
is a pure derivation recomputable inside one fold. PROTO-1-t1 then **executed** the emit-dispatch seam
retirement (replaced `has_rule("value")` with a structural ≥4-of-6 value-category census) — **byte-identical
GREEN 3/3** (`check-{json,sheets,real-typed}`), and **refuted by execution** PROTO-2's `dispatch_alt_count`
discriminator (setting it to 0 turns `check-sheets` RED: Sheets carries dispatch `Alt`s too — the honest
discriminator is the specific value-category census, not any dispatch count).

**Integration seams.** `derive_materialization_plan` (`passes:1139`); `single_plan` (`:1283`); the two
**structural tree-walkers** the fold must re-base: `json_sink_direct/dispatch.rs` (**318 LOC**) +
`general_descent/view.rs` (**431 LOC**) — verified line counts this pass.

**Gates it carries (born-RED, named owner).**
- **G-3B-1 — THE killer gate, UNRUN after two passes.** *Execute* the whole `emit(&GrammarIr,&Attributes)`
  fold; **regen×check × {json,css,sheets,real-typed} byte-identical** (`cargo xtask check-json &&
  check-sheets && check-css-l4-declaration-values && check-real-typed` all exit 0). Two passes in: trace
  (PROTO-2) + one slice (PROTO-1-t1), **no fold**. "Executable" ≠ "executed." **The demotion trigger is
  partly met**: run it next pass (Prototype Brief α) or demote 3B to "structurally-motivated, unproven."
- **G-3B-1a — the re-walk re-expression IS the unconverged hard part.** Deleting `SinkOnlyExpr` **adds
  back** a comparable re-walk of `GrammarIr` in `dispatch.rs` (318) + `view.rs` (431) — structural
  tree-walk consumers, not emission-only intermediates. **The −100 net-LOC is the reward IFF the walkers
  re-base byte-identically — an unrun assumption; report CONTINGENT, not banked** (the classic
  elegant-reduction tell).
- **G-3B-2 — relocation ≠ retirement (a DRY smell, carried).** PROTO-1-t1's slice **duplicated**
  `roles.value` into codegen (a `system-cohesion`/DRY violation the probe flagged). The proven artifact is
  a byte-identically *relocatable* slice with a DRY home, NOT the single-home fold. True **RETIREMENT**
  (vs relocation) stays born-RED on `DirectTypeKind::Sum` (GN-3.W1) + a value-role attribute.
- **G-3B-3 — materialization key** (owner LOCK-3, unrun). per-`RuleId` suffices for JSON/Sheets; the outer
  tree keys by `NodeId`. Does any single rule need two distinct descriptors → per-ExprId forced (CSS L4
  inline alternatives the live candidate)? A partial migration is a `no-orthogonal-codepaths` defect.

**Open questions.** (i) On the *executed* fold, are header comments / `BTreeMap` key order / roster order
incidental — every non-identical byte attributed? (ii) Does `css_scan` hide a fourth value level
(grammar-name prefixes / entry-rule threading not in `GrammarIr`)? (iii) The CONSUME half — `.target` is a
**dead field**; true CONSUME (delete the schema param, synthesize `DirectSchemaSet` from `sink_only`,
resolve JSON's zero-arrow corpus) sits **inside G-3B-1's fold**, not a separable move.

**Status: ADVANCED (substrate) · 55%.** The cleanest structural argument in the lock; one byte-identical
slice ran; the killer whole-fold gate is UNRUN and the net-LOC rests on an unrun re-walk. Not
convergence-eligible.

---

## 3. FAMILY 3C — GRAMMAR-HOMOIOUSIA · BLOCKED (C1) / BLOCKED-candidate (C2) · 32%

**Architecture.** Every IR level is a grammar-shaped sum-of-products node algebra with a `.bbnf` face;
every transposition between adjacent levels is a total grammar-to-grammar map carrying a round-trip oracle;
the generator, applied to the grammar specifying a level, emits that level's Rust types (bootstrap
alignment with LOCK-7 — Nanopass `define-language`, Spoofax/Stratego, MLIR ODS; prior-art §2/§6). **The
family cleaves into two claims of very different strength (research-C's decisive move) — keep them costed
apart; it is the family's honesty.**

- **C1 — the IR *data model* is grammar-specified** (enum/struct shells emitted from a bbnf grammar). The
  **front IR already realizes this**: `GrammarIr`/`ExprKind` (10-arm sum) has a total bidirectional grammar
  face — forward `parse_grammar` (`grammar/src/lib.rs:29`), backward `GrammarIr::pretty` (`ir/src/lib.rs:124-204`,
  every arm handled) — so `parse_grammar(g.pretty()) ≅ g` is a buildable, zero-runtime functor witness. The
  self-hosting `.bbnf` corpus is authored (`grammar/bbnf/bbnf.bbnf`, `expressions.bbnf`, `types.bbnf`,
  `misc/regex.bbnf`). **BLOCKED** on two external gates below.
- **C2 — the IR *transpositions* are grammar-to-grammar maps.** **REFUTED on live** (the sharpest
  counterexample): bbnf maps **bytes → tape**; `lower_expr`/`materialize_rule` are **tree → tree** folds
  with no byte stream, so **no bbnf grammar *is* this map**. Realizing it demands a
  term-rewriting/attribute-eval engine bbnf lacks → importing one is an orthogonal-subsystem breach OR a
  collapse into LOCK-2's engine. **BLOCKED-candidate** — a missing primitive of difficulty *equal to* the
  original problem — **ceded to LOCK-2** (provisional on XL-B, WORKING not final).

**Mechanism (C1, the only live-adjacent surface).** Replace the **fabricated** product masquerading as a
derived sum — `derive_shape_facts` (`passes:285-294`) emits a hardcoded 6-variant `JsonValue` enum without
consuming the grammar's `Alt` — with an `ExprKind::Alt → DirectTypeKind::Sum` grammar-to-grammar map, and
prove the derived sum is **byte-identical** to today's fabricated list (the equality *is* the proof the map
recovers what is invented). Retires the two live re-derivations: `ProjectionKind`+`classify_projection`
(the lossy 4-way fold, `general_descent/view.rs:20-46`) and `derive_materialization_roles` (`passes:1403`).

**Gates it carries (all externally owned, born-RED).**
- **G-3C-1 — `DirectTypeKind::Sum`** (owner **GN-3.W1**; seam `direct_schema.rs:52`, Struct-only verified;
  +420 LOC). The product-only subset excludes every `Alt` = excludes `ExprKind`/`BackendExpr` themselves —
  **must not stand for the formulation** (elegant-reduction trap by construction).
- **G-3C-2 — a named consuming wave** (owner **LOCK-7·7B**, TOTAL-SELF-HOST literally enumerates "IR
  grammar"). On live there is **no `ir.bbnf`, no meta-grammar**; C1's sole reader is an unlanded LOCK-7
  wave (Y.13 consumer-less-substrate — admissible only as a born-RED edge under GESTALT-B).
- **G-3C-3 — fixpoint** (owner LOCK-7·7A): `regen∘regen = regen` byte-identical over a grammar-generated
  `ir/src/lib.rs` carrying derives/doc-comments/impls — likely forces per-derive/per-impl annotations.
- **G-3C-4 — C2 cession** to LOCK-2 (provisional on XL-B).

**Open questions.** R-2: `pretty()` is **information-lossy** — drops `AltMode` (Dispatch vs Speculative),
erases `SourceSpan`, renumbers `ExprId`; the functor holds only as *structural-eq modulo renumber +
span-erasure + AltMode-recovery*. A homoiousia oracle needs a face preserving AltMode+spans. R-5: capacity
counts + closed enum tables in the hand schema are **document facts, not grammar facts** — a
grammar-to-grammar map cannot invent them; they enter only if the typed grammar *declares* them
(`-> Vec<T> @cap(875)`). This bounds where byte-identity is honestly reachable.

**Status: BLOCKED (C1) / BLOCKED-candidate (C2) · 32%.** Distinctive content **0% convergence-eligible**;
SPEC-ONLY this pass (concrete or SPEC-ONLY, Constitution vii). The one win that stands (the `->`-thread) is
the same single-owner `:204` thread PROTO-1 ran — 3C **consumes**, it does not author.

---

## 4. FAMILY 3D — STAGED-RESIDUAL · BANKED (into 3B's attribute store) · 38%

**Architecture.** The IR is an explicitly two-stage program: every node carries a binding time — `Stage ∈
{S0 (generator-time, erased), S1 (runtime, emitted)}`, `S0 ⊑ S1`, join=S1. Residualization = evaluate the
S0 sub-graph at generation time, delete it, emit only the S1 frontier specialized w.r.t. the S0 values it
consumes (Nielson BTA over a two-level λ-calculus; LMS types-distinguish-stages; PROVE-12 = first-Futamura
specialization — prior-art §5). "Type-plane erasure = stage-0 erasure on the type sub-plane" is the
unification thesis. **The stage mechanism is architecturally a 3B move** — `Stage` is one attribute slot on
the L1→L2 edge, not a level; the separate-staging-spine formulation stays **BANKED into 3B's `Attributes`
store**.

**Mechanism.** A pass `binding_times(&GrammarIr) -> StageMap` keyed by `ExprId` (a node is S0 iff every
data input is a grammar constant reachable without consuming bytes; Nielson's congruence: an operator is S0
only when all operands are S0). At every S0→S1 frontier, one general emit rule specializes the S1 node
w.r.t. the S0 value (the const-generic monomorphization PROVE-12 applied by hand to one delimiter mask).

**Live substrate (declared, not re-authored).** Two S0 islands already ship: `derive_alphabet` →
`render_alphabet_consts` (`css_scan.rs:487` → `css_scan_direct/mod.rs:112`, the CSS stop-set baked
`const … : &[u8]`); `derive_recognizers` (`passes:338`) folding single-byte `Literal`s into
`StructuralAlphabet`. The one unnamed stage fact that flows today is the type sub-plane (Constitution v ⟺
"the type plane is entirely S0").

**Gates it carries.**
- **G-3D-i — the unification is unproven (the distinctive, 0% run).** Attach a binding-time fact to live
  IR and prove "type-plane = stage-0" beyond restatement. The thesis (ii) **spec-cites-itself** (its
  warrant is Constitution v, a definitional restatement, not an independent result).
- **G-3D-ii — `binding_times(CssScanProgram)` == PROVE-12 partition** (runnable via checksum parity;
  unrun).
- **G-3D-iii — `CollapsedStage` is a DEAD gate** (`admits_collapsed_stage`@959 reads `avx512bw`,
  permanently false on aarch64). A genuine Y.13 consumer-less-substrate **on the living tree** — a problem
  3D must fix (re-home admission off `avx512bw` onto a grammar-derived binding-time predicate, no x86
  smuggled back), **not** a bank.
- **G-3D-iv — JSON type-plane PMU delta unmeasured.** JSON has no scan-side S0 (hot rules dispatch to hand
  kernels) → the JSON claim is *entirely* the type sub-plane; absent a measured cold paired-PMU delta it
  collapses to "typed façade, same speed" (acceptable only if declared; PROVE-09 law).

**Named risks (binding).** R-A: **S0-ness is necessary, not sufficient, for a win** — PROVE-12 won because
it *deleted interpretive dispatch* (slice loads + length guards), not merely because the set was constant;
the JSON first-byte `match` is S0-keyed but LLVM may already lower it to a jump table (residualizing it may
be neutral). **Which frontier to specialize is a cost decision owned by LOCK-2** (`ir/cost.rs`); LOCK-3
owns only the stage *fact contract* at the L1→L2 edge. R-C: `ty` is a bare `Option<String>`; BTA cannot
unify strings — threading *consumes* LOCK-1's resolved type, born-RED on LOCK-1.

**Status: BANKED · 38%.** Nothing 3D-distinctive ran; leverage rests on a retired-target PROVE-12
(`find_component_delim` retired from `codegen/src`; PMU not re-run on `06e70053f`), an already-shipped
`ScanAlphabet`, the shared `:204` thread, and a proven-DEAD gate. Reactivates to ADVANCED **only if** the
unification is proven concretely (binding_times on real IR + `CollapsedStage` re-home + a measured
type-plane delta). The **PROVE-12-`u128`-byte-identity flagship stays RETIRED** — do not re-pose it.

---

## 5. THE INCOMPATIBILITY MAP (which families cannot coexist, and precisely why)

The lock has **one hard architectural incompatibility** and **three boundary incompatibilities**. All
resolve toward a **single convergent spine** (§6); the losers are BANKED or ceded, never funded twice
(funding two homes for one seam is a `no-orthogonal-codepaths` / double-home breach).

**I-1 (HARD) · 3A-standalone-typed-multi-level-tower ⊥ 3B-single-IR.** These are the two poles of the
same axis (prior-art §9, the "IR proliferation" trap). 3A-standalone posits **distinct typed levels**,
each a total function with its own language predicate (Nanopass many-IL); 3B posits **one rich IR refined
in place**, BackendShape as attributes (LLVM/Trees-That-Grow). They cannot both be the tower's shape. **The
live spine is already 3B-shaped** (one `LayoutFacts` bundle, attribute-annotated). **Resolution
(REGISTRY XL-3, decisive by execution):** 3B is the substrate; 3A **demotes to a per-edge contract
discipline over the single IR** (a `verify(grammar,attrs)` predicate + per-edge differential digest,
recovering the nanopass discipline **without a second tree**). Standalone 3A tower **BANKED**. Nanopass's
own measured cost (compile time ~2× for 5→50 passes) is the binding reason to reject the many-IL pole under
extreme parsimony.

**I-2 (BLOCKED-candidate) · 3C-C2-grammar-to-grammar-transpositions ⊥ the live spine AND ⊥ grammar-totality-
without-a-new-engine.** bbnf maps **bytes→tape**; the live transpositions (`lower_expr`, `materialize_rule`)
are **tree→tree** folds with no byte stream — **no bbnf grammar *is* the map**. To make transpositions
grammar-to-grammar you must import a term-rewriting/attribute-evaluation engine (Stratego-shaped) that bbnf
lacks — **a missing primitive of difficulty equal to the original problem** (prior-art §3/§6: attribute
circularity is undecidable-adjacent; unconstrained `innermost` non-terminates). Importing it is an
orthogonal-subsystem breach (Constitution: no orthogonal subsystems) OR it collapses into **LOCK-2's
rewrite/schedule engine**. Therefore 3C-C2 is **incompatible with 3B's single-fold** (which is a
hand-written total fold, not a grammar-scheduled rewrite) and is **ceded to LOCK-2** (provisional on XL-B).
This is the map's genuine BLOCKED-candidate marking, not an elegance discount.

**I-3 (BOUNDARY) · 3D-separate-staging-spine ⊥ 3B-single-IR.** A two-stage tower as **distinct levels**
duplicates 3B's IR with a stage dimension. **Resolution:** `Stage` is **one attribute slot** in 3B's
`Attributes` store (LMS's "types distinguish stages" realized as an attribute, not a level). 3D-distinctive
spine **BANKED into 3B**. They compose; they do not compete.

**I-4 (BOUNDARY) · 3D-hardcoded-per-site-staging-emitter-branch ⊥ XL-B decision-driver boundary.** A
LOCK-3 hardcoded emitter branch that *chooses which frontier to residualize* usurps LOCK-2's seat (per-site
staging-form scheduling is a cost decision, R-A). LOCK-3 owns the stage **fact contract** at the L1→L2 edge;
LOCK-2 owns **what engine** selects over it. A staging-form emitter branch in LOCK-3 is incompatible with
that boundary — **ceded to LOCK-2** (provisional on XL-B).

**I-5 (BOUNDARY) · 3C-materialized-back-IR-`.bbnf` ⊥ no-ts-ir + Y.13.** Giving `BackendExpr`/`DirectSchemaSet`
a `.bbnf` face with **no regen consumer** is consumer-less substrate (forbidden). Compatible **only** as a
born-RED LOCK-7 edge (the bootstrap fixpoint is the consumer), never landed on spec. The backend-agnostic
middle is a **contract** (tape + schema, PROVE-14), not a materialized shared instruction IR — `no-ts-ir`
holds across 3C/3D.

**What is NOT incompatible (composes cleanly):** 3B substrate + 3A per-edge contract discipline + 3D
stage-attribute. This is the convergent spine (§6). 3C-C1's `Alt→Sum` map and 3D's stage fact both **write
attributes onto the same single IR** and both **consume** the shared `:204` thread — they are the same
architectural shape, distinguished only by which attribute they author and whether their consumer has
landed.

---

## 6. THE CONVERGENT SPINE (hypothesis; architecturally sound; rests on two UNRUN load-bearing halves)

**One IR (3B substrate) + per-pass contracts (3A discipline) + a stage attribute (3D erasure); 3C banked**
until GN-3.W1 (`DirectTypeKind::Sum`) + a named LOCK-7·7B wave unblock it. The families **compose**, not
compete (prior-art §10's convergent shape: a 3B single rich IR whose levels carry 3A-nanopass contracts
sharpened to AG-attribute granularity, with 3D-LMS type-staging as the erasure mechanism, 3C admitted as
contract-level homoiousia bounded by LOCK-7/no-ts-ir). The AG frame is the unifying insight: the type level
is a **schedulable attribute plane**, and its schedule — like the graph-IR schedule and the per-site
staging choice — is the **same LOCK-2 solver seam**.

| Component | State after pass-2 |
|---|---|
| `:204` thread + verify predicate (RED→GREEN, zero footprint) | **PROVEN** (PROTO-1) |
| Fold fact-layer purity (`materialize_rule` recomputable) | **PROVEN by trace** (PROTO-2) |
| 3A refinement oracle — localizes an injected fault to its edge | **RAN** (PROTO-2-3A) — on the **free E3 edge**; E2↔E3 separation proven |
| **3A *contract* non-vacuity (E1-infer; declared-vs-inferred)** | **UNRUN + gate-cannot-fail until LOCK-1** — the organ that earns 3A's *contract* |
| Seam relocation (`has_rule("value")` → value-role census) | **RAN byte-identical** (PROTO-1-t1) — relocatable slice, DRY home; single-home deferred |
| PROTO-2's `dispatch_alt_count` discriminator | **REFUTED by execution** — working discriminator = ≥4-of-6 value-category census |
| **3B-G1: execute the WHOLE fold, regen×check byte-identical** | **UNRUN — the spine's load-bearing gate** (−100 net-LOC contingent on an unrun re-walk of dispatch.rs 318 + view.rs 431) |
| CONSUME (`.target` → synthesize `DirectSchemaSet`) | **REFUTED-as-stated** (`.target` dead); behind the 3B fold |
| 3D binding_times on real IR + `CollapsedStage` re-home | UNRUN (BANKED) |
| 3C C1 (grammar-specified data model) | BLOCKED (Sum node + LOCK-7 consumer) |
| 3C C2 (grammar-to-grammar transposition) | **BLOCKED-candidate — refuted, ceded to LOCK-2 (provisional on XL-B)** |

**The two UNRUN load-bearing halves (both critics converge, and these are this synthesis's two prototype
briefs):** 3B rests on **G-3B-1, the whole-fold regen×check** (one slice ≠ the fold; the slice used a
Constitution-forbidden duplicated home); 3A rests on the **E1-infer non-vacuity** (the oracle is proven on
the 1:1 free edge, not on the edge that could be vacuous). No family is convergence-eligible (pass 2 of ≥3;
each carries ≥1 UNRUN load-bearing gate; two consecutive clean passes + fresh non-author audit outstanding
for all four).

**Cross-pollination (one, named, partially executed):** 3A's refinement-oracle organ ⟶ 3B's
`(GrammarIr, Attributes)` fold — PROTO-2-3A ran the E3 correspondence oracle on the **live `json`
BackendIr** (15 rules / 100 nodes), the per-edge contract over the single IR the 3B fold would fold. Each
analysis/emit pass over the single IR carries a checkable output contract; the nanopass discipline is
recovered without a second tree. *Deferred graft:* 3D's residual-count oracle is a 3A-style refinement
oracle, re-evaluate only after the 3D re-cut lands on real IR.

---

## 7. CROSS-LOCK DIRECTIVES CARRIED (conveyed as facts; challenges route to GESTALT-B with evidence)

1. **XL-1 — the `TypeAnalysis::from_ir` consumer contradiction** routes to GESTALT-B as a named-supersession
   challenge with evidence. XL-F(1) says "TypeAnalysis::from_ir … is the real type consumer"; SYNTHESIS
   re-verified `TypeAnalysis`/`from_ir`/`analysis.rs` are **absent from live `skinny/crates`** (legacy
   `crates/core` only, over `GrammarIR` with no `node_types`). Two routable readings: **(a)** XL-F(1)
   describes the *greenfield tree-of-record* consumer (reconcilable under XL-A) — then it must be **labeled**
   as such, not a live-spine pointer; **(b)** a stale pointer needing amendment. The live consuming edge on
   the measurement substrate is the `DirectSchemaSet` seam (`derive_materialization_plan` →
   `json_typed_direct::render`). Owed: an explicit named-supersession line, not a quiet substitution.
2. **`Project.ty`@204 is single-owner; the thread is DISCHARGED, the CONSUME and the CONTRACT are not.**
   Assign the thread to **exactly one owner** (a GN-3 wave, `no-value-discard`); 3A verifies, 3B threads as
   `attrs.node_type`, 3C declares, 3D erases as S0 — the other three CONSUME. Two entangled obligations
   remain unfunded by funding the thread: **(i)** the CONSUME half sits inside G-3B-1's fold; **(ii)** the
   consistency CONTRACT (3A) is a separate born-RED obligation on LOCK-1 — thread ≠ contract.
3. **Keep the decision-driver seat unmerged from LOCK-2 (XL-B, WORKING → provisional).** LOCK-3 owns
   *where* the driver sits (the fact contract at the L1→L2 edge); LOCK-2 owns *what engine*. The cessions
   (3C-C2, 3D staging-form) carry "(provisional on XL-B)" — not final until GESTALT-B.
4. **`no-ts-ir` holds** across 3C/3D — the meta-grammar emits TS *type declarations*, never a runtime TS
   IR; the backend-agnostic middle is **tape + schema** (PROVE-14), not a shared instruction IR.

---

## 8. THE TWO SHARPEST UNCERTAINTIES → PROTOTYPE BRIEFS (returned via StructuredOutput)

**Brief α — G-3B-1, the whole-fold regen×check** (`bbnf-worktree`). The spine's single load-bearing gate,
UNRUN after two passes; its resolution decides whether the convergent substrate (on which the 3A discipline
and 3D attribute both hang) stands or 3B demotes.

**Brief β — 3A E1-infer non-vacuity witness** (`bbnf-worktree`). Decides whether 3A's contract has *meaning*
or is presence-only-until-LOCK-1 — the edge where the contract's meaning lives, runnable today without
waiting on LOCK-1's name→Type resolution.

*(Full self-contained briefs are in the StructuredOutput payload.)*
