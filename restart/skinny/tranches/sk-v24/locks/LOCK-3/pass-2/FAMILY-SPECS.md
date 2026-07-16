# LOCK-3 — THE IR TRANSPOSITION SPINE · pass-2 FAMILY-SPECS

*Synthesis (Fable-tier), pass 2. One cogent specification per family while routes remain
incompatible, then the incompatibility map. Elegance earns nothing on its own; a family stalled on
a missing primitive of equal difficulty to the original problem is marked **BLOCKED-candidate**.
Built on the pass-1 registry dispositions (3A ADVANCED-discipline / 3B ADVANCED-substrate / 3C
BLOCKED / 3D BANKED), re-cut against the fresh pass-2 research-{A,B,C,D,priorart} sweep **and the
two pass-2 prototypes that RAN** (PROTO-1 RAN_MEASURED, PROTO-2 GATE-PASSES-by-trace), bound by the
CROSS-LOCK ledger (XL-A…H) as settled fact.*

---

## 0. The settled baseline (pass-2, re-anchored to live)

**Base-pin (XL-H).** Live master HEAD is **`skinny@c43126422`** (`git rev-parse HEAD` =
`c43126422423…`, confirmed this pass). The directive token `b3cb8c16b` is a stale ancestor
(**10 commits behind**, ancestor-of-HEAD ✓); the prior synthesis draft's `c93fc43143` and the
PROTOs' `c93fc4314` are also ancestors of HEAD — the cited anchors are byte-stable across them and
were **re-greped by hand on `c43126422` this pass**. The source tree lives at top-level
`skinny/crates/` (NOT `restart/skinny/`, which holds docs/tranches only).

Confirmed verbatim on `c43126422`:

- `skinny/crates/passes/src/lib.rs` is **2096 LOC**. `:203` = `ExprKind::Annotation { .. } =>
  Type::Builtin(BuiltinTy::Unit)` (a **non-defect** — `@ws`/`@cap` legitimately untyped). `:204` =
  `ExprKind::Project { body, .. } => self.infer_expr(*body)?` — the `->` ty-drop (`ty`/`target` via `..`).
- `DirectTypeKind` @ `codegen/src/direct_schema.rs:52` is **Struct-only** (a `StringEnum` *ref*
  surface at `:115` exists, but no recursive `Sum` type-constructor) — the sum-node wall stands.
- `has_rule("value")` @ `codegen/src/runtime_generator.rs:98` **survives** (clause-vi name-gate).
- CSS emit is **IR-driven** (`css_scan_direct` + `ScanAlphabet::derive_alphabet` → `const … : &[u8]`);
  `find_component_delim` retired from `codegen/src` (survives only in `runtime/src`).
- `binding_times`/`Stage` **absent** from live `crates/ir/`; `DirectBuildField.target` is `None` at
  every offset-tape descriptor site.

**Two facts bind every family (re-affirmed).** (1) The tower is **3B-shaped today** — one
attribute-annotated `LayoutFacts` bundle (`passes/src/lib.rs:32-62`, mutated in place at `:54-57`),
`BackendExpr`/`SinkOnlyProgram` a materialized plane, then a **fractured** emission stage. (2) The
`Project.ty@204` thread and the starved schema (`DirectBuildTarget` real, `.target = None`
everywhere; sole schema producer the hand file `xtask/src/real_typed_schema.rs`) is the obligation
shared by 3A/3B/3D and the product of 3C.

**What pass-2 EXECUTED changed (the two prototypes that ran):**

- **PROTO-1 (RAN_MEASURED).** The single-owner `:204` thread + `verify_project_types` per-pass
  predicate: reads **RED (4 spans) on HEAD, GREEN after the thread**, with `check-json` /
  `check-sheets` / `check-real-typed` **byte-identical predicate-in vs predicate-out (zero emit
  footprint)** and **0 `classify_projection` on the derive path**. → *The 3A gate + the shared
  `:204` thread is DISCHARGED and EXECUTED on live.* The **CONSUME** half — populate
  `DirectBuildField.target` and reproduce the hand `real_typed_schema.rs` byte-identically — is
  **REFUTED as stated**: `.target` is a **dead field** (never populated, never read; the typed
  render clones the hand `DirectSchemaSet` wholesale). True CONSUME sits behind three walls
  (type / wiring / corpus) and the 3B fold — carried to pass-3.

- **PROTO-2 (GATE-PASSES by verified trace; fold UNRUN).** Data-flow trace: the four
  `SinkOnlyFacts` accumulations read **only `GrammarIr` + `backend_shape` + descriptor** — so the
  emit-order framing is a **pure derivation** recomputable inside one fold (Q-3B-1 precondition =
  YES). Seam disposition **corrected**: `has_rule("value")` **relocates to a value-role attribute,
  NOT `backend_shape`** (JSON and Sheets are both uniformly `BackendShape::SinkOnly` →
  backend-shape-indistinguishable); `derive_materialization_roles` **RELOCATES** (JSON carries 0
  `->`), true retirement is a born-RED GN-3/LOCK-7 edge. net-LOC ≈ **−100 LOCK-3-local / −220
  gestalt-conditional**; extensibility **5→2 confirmed**. **The killer gate — *execute* the fold,
  regen×check byte-identical — remains UNRUN.**

---

## FAMILY 3A — CONTRACT-TOWER · ADVANCED (discipline) · convergence 62% (↑ from 55)

**Architecture.** One IR + **per-pass output contracts** — a `verify(grammar, attrs)` predicate at
each analysis/emit edge, plus a per-edge **differential digest** (translation-validation at edge
granularity). NOT a standalone tower of distinct typed value levels (BANKED pass-1, reaffirmed).
The nanopass "small fixed tower of total functions, each a checkable contract" is recovered
**without a second tree** — the named cross-pollination onto the 3B substrate.

**Mechanism (two organs).**
1. **The per-pass verify predicate — RAN.** `verify_project_types` over `Project@204`: for every
   `Project` with `ty == Some(d)`, `Err` unless `node_types[project] == Type::Declared(d)` **and**
   `≠ node_types[body]`. RED (4 spans) → GREEN, **zero emit footprint** (PROTO-1). This organ is
   **PROVEN**. It installs the missing contract on the opaque `ty:Option<String>` — a raw string
   forwarded to codegen with nothing asserting body/declared consistency.
2. **The per-edge refinement oracle — SPEC'd, UNRUN (the distinctive organ).** One
   `differential(edge, corpus)` runner, `edge ∈ {E1 infer, E2 materialize, E3 sink}`, generalizing
   the two specialized instances that already ship: `bbnf-bench/benches/json_parity.rs`
   (`track1/track2` digests) and `runtime/tests/css_scan_differential.rs` (clean-room scalar oracle
   swept over start-pos × STOP-set × corpus). **E3 is a *free* oracle**: `lower_expr`
   (`codegen/src/lower/sink_only.rs`) is a **1:1 arm map** `BackendExpr ↦ SinkOnlyExpr` (`ValueProject.ty`
   copied through), so `relift(lower_expr(x)) == x` needs no reference implementation. The runner
   must (a) reproduce the existing digests and (b) **localize an INJECTED single-arm mis-lowering to
   its edge** (E3 round-trip breaks + terminal digest breaks, E1/E2 stay GREEN ⇒ name E3).

**Integration seams (live path:line).** verify site `passes/src/lib.rs:204`; sole `ty` consumer
`single_plan` `passes/src/lib.rs:1349`; structural type `passes::Type` `:234-243`; pipeline driver
`compile` `:32-62`; `BackendExpr` sum `ir/src/lib.rs:376-413`; E3 1:1 map
`codegen/src/lower/sink_only.rs`; runner templates `bbnf-bench/benches/json_parity.rs` +
`runtime/tests/css_scan_differential.rs`; CSS second-root probe `runtime_generator.rs:40-71`.

**Gates it carries.**
- **G-3A-a (born-RED, LOCK-1).** `ty:Option<String>` vs `passes::Type` (structural, 7-arm) — the
  contract needs a name→Type resolution absent on live. Without it the check degrades to canonical
  string-equality. Owner: LOCK-1 (the type language). 3A **consumes**, never authors.
- **G-3A-b (corpus-coverage / witness census).** Per-arm reachability over the fleet; an arm with
  **zero witnesses reports `unverified`, never `sound`** (Y.13 for oracles). `ValueProject` has
  witnesses only on Sheets; `Return`/`EventTape`/`OffsetTape`/`CollapsedStage` may be zero-witness.

**Exact open questions.** (i) Does the `differential(edge,corpus)` runner localize an injected
E2/E3 fault to the injected edge (never merely "a failure")? (ii) Is `render`
(`runtime_generator.rs:78`) a **pure projection** of L2 (so "three levels" holds and E4 carries no
value)? (iii) Is the `ty`-contract non-vacuous — ≥1 non-trivial Sheets projection where declared
`ty` constrains beyond `infer_expr(body)`, else report `unverified`? (iv) Does the CSS `Err⇒SinkOnly
/ Ok⇒Scan` fork force a **two-rooted** tower or a first-class edge oracle?

**Disposition.** ADVANCED-as-discipline. The verify organ is proven; the refinement oracle — the
one thing distinguishing 3A from a tautological `verify()` — is the sharpest UNRUN uncertainty in
the lock (**PROTO BRIEF 2**). Not convergence-eligible (organ unrun; pass 2 of ≥3).

---

## FAMILY 3B — SINGLE-IR-MULTIPASS · ADVANCED (the convergent substrate) · convergence 60% (↑ from 50)

**Architecture.** One rich IR refined in place by annotating passes (the LLVM/rustc-query shape);
`BackendShape` and the type/materialization planes are **attributes keyed by stable node identity**;
the fewest transpositions that suffice. The family's entire surface is **emission**, not the tower —
skinny already runs the annotate-in-place pipeline for shape/cost/policy (`:54-57`). 3B collapses
the three name-gated emitters into **one `emit(&SinkOnlyProgram, &Attributes)` fold** dispatching on
structural attributes only, never a rule name or file-family branch (MLIR's "one conversion
framework, pattern-matched on structure").

**Mechanism.**
- **The fold.** Replace the `has_rule("value")` gate (`runtime_generator.rs:99`) + `emit_general_descent`
  + CSS route with one fold whose arms are data-driven by `attrs.backend_shape.get(rule_id)` and
  `attrs.materialization.descriptors.get(rule_id)`. `EmitAttributes` is **not new IR** — it surfaces
  the existing `LayoutFacts` sub-planes (`backend_shape`, `MaterializationPlan.descriptors`,
  `ScanAlphabet`, `node_types`).
- **`materialize_rule` recompute — PROVEN PURE (PROTO-2).** The splice `Seq[SpanMark(Start), body,
  SpanMark(End), TapeEmit{kind}, DirectBuild{shape,fields}, Return]` derives every field from
  `MaterializationDescriptor` ⊆ `GrammarIr`; the four `SinkOnlyFacts` accumulations read nothing
  outside `GrammarIr + backend_shape + descriptor`. Recomputable inside the fold byte-identically.
- **The `:204` thread (shared, DISCHARGED for the fact plane).** Skinny already has a per-ExprId
  type plane (`node_types` insert `:212`, consumed at `:179`); only the `Project` arm writes the
  wrong value. Add `Type::Named/Declared(StringId)` (backend-agnostic-types law), write it at `:204`
  when `ty=Some`, fill `DirectBuildField.target` in `derive_materialization_plan`. **The fold
  CONSUMES the annotation by map lookup** (exactly as the outer tree's `TypeAnalysis::from_ir`
  consumes `ir.type_map` without re-deriving). PROTO-1 proved the RED→GREEN + zero-footprint; the
  **CONSUME reproduction of `real_typed_schema.rs` is REFUTED-as-stated** and carried here.

**Integration seams.** name-gate `runtime_generator.rs:98-99`; three emitters `:106` (JSON),
`general_descent::render` `:159`, `css_scan::derive_program` `:57`; role classifier
`derive_materialization_roles` `passes/src/lib.rs:1403`; descriptor sites `:1146-1269` (`.target`
all `None`); type write `:204`/`:212`; downstream typed consumer `json_typed_direct::render` +
`schema_direct::lower_program`; hand schema `xtask/src/real_typed_schema.rs`.

**Gates it carries.**
- **G-3B-1 (THE killer gate, UNRUN).** *Execute* the fold; **regen×check × {json,css,sheets}
  byte-identical**. PROTO-2 cleared the precondition by trace + pinned the 4/4 baseline floor but
  **did not implement the fold within cap**. "Executable" ≠ "executed." **This is the one falsifiable
  that earns the family** (**PROTO BRIEF 1**).
- **G-3B-2 (born-RED, GN-3.W1 + LOCK-7).** The byte-literal role classifier and `has_rule("value")`
  **RELOCATE** now (name-gate → value-role attribute; not `backend_shape`, per PROTO-2's correction).
  True **RETIREMENT** is gated on `DirectTypeKind::Sum` (declares `value = object|array|…` so the
  heuristic is superseded by inference) — carried, never asserted retired absent the node.
- **G-3B-3 (materialization key).** per-`RuleId` (`descriptors: HashMap<RuleId,_>`) suffices for
  JSON/Sheets; the outer tree keys by **`NodeId`** because one rule holds many distinct Seq/Alt
  nodes (CSS L4 inline alternatives the live candidate). One key fleet-wide or the migration is the
  `no-orthogonal-codepaths` defect.

**Exact open questions.** (i) Do the six regen outputs come back byte-identical, or does incidental
text (header comments, `BTreeMap` key order `:110`, roster order) break identity without semantic
change — and is every non-identical byte attributable incidental-vs-semantic? (ii) Does `css_scan`
hide a **fourth value level** (grammar-name prefixes / entry-rule threading not in `GrammarIr`)?
(iii) Does any fleet rule (json/css/sheets/csv) need two distinct descriptors → per-ExprId forced?

**Disposition.** ADVANCED-substrate; the convergent spine hangs on it. Not convergence-eligible
until G-3B-1 runs green. **Demote to "structurally-motivated, unproven" if the fold is not executed
next pass.**

---

## FAMILY 3C — GRAMMAR-HOMOIOUSIA · BLOCKED · convergence 34% (↑ from 30)

**Architecture — split into two claims of very different strength (research-C's decisive move).**
- **C1 — the IR *data model* is grammar-specified.** The enum/struct shells (`ExprKind`,
  `BackendExpr`, `DirectBuild*`) emitted from a bbnf grammar describing them (schema-generation).
  Real prior art: tree-sitter `node-types.json`, MLIR ODS, attribute grammars.
- **C2 — the IR *transpositions* are grammar-to-grammar maps.** `lower_expr`/`materialize_rule`
  as grammar/rewrite artifacts (term-transduction). **REFUTED on live.**

**Mechanism / why C2 is refuted (the sharpest counterexample).** bbnf grammars map **bytes → tape**
(recognizers over input). `lower_expr` (`passes/src/lib.rs:1329`) and `materialize_rule` (`:1362`)
are **tree → tree** folds — no byte stream, so no bbnf grammar *is* this map. Realizing
"grammar-to-grammar maps" demands a term-rewriting / attribute-evaluation engine bbnf lacks
(e-graph / DRR-PDL). Importing one is either an **orthogonal-subsystem violation**
(`no-orthogonal-codepaths`/`system-cohesion`) or a **collapse of 3C into LOCK-2**. **C2 has no honest
home inside 3C's charter** — it is ceded to LOCK-2.

**Gates it carries (all born-RED, externally owned).**
- **`DirectTypeKind::Sum`** @ `direct_schema.rs:52`, owner **GN-3.W1** (+420 LOC). `ExprKind`
  (10-arm) and `BackendExpr` (13-arm) are irreducibly sums; the product-only subset excludes every
  `Alt` = excludes the family's own IR. **Do not treat the product-only subset as standing for the
  formulation** (directive).
- **The consumer** @ regen lane, owner **LOCK-7 · 7B** (TOTAL-SELF-HOST literally enumerates "IR
  grammar"). On live there is **no `ir.bbnf`, no meta-grammar, one grammar (`json.bbnf`)** — C1 is a
  substrate whose sole reader is an **unlanded LOCK-7 wave** (Y.13). Admissible only as a born-RED
  edge under GESTALT-B, never a self-standing 3C deliverable.
- **Fixpoint** `regen∘regen = regen` byte-identical (LOCK-7 · 7A) over a grammar-generated
  `ir/src/lib.rs` carrying derives/doc-comments/impls — the sharpest engineering gate; likely forces
  per-derive/per-impl grammar annotations.

**The one win that stands alone.** C1/C2 aside, 3C's `->`-thread (P4) is the **same single-owner
`:204` thread** PROTO-1 ran — resolve `Project.ty` as `DirectBuildTypeRef::Named`, retire the
byte-literal classifier + shape fallback + name-gate. This is not distinctively 3C; it is the shared
gate assigned to one owner (a GN-3 wave). 3C **consumes** the threaded value.

**BLOCKED-candidate analysis.** C1's missing primitive (`DirectTypeKind::Sum`) is **bounded** (a
+420 LOC GN-3 wave), **not** of equal difficulty to the original problem → **BLOCKED, not
BLOCKED-candidate**. C2's missing primitive (a tree-rewrite/attribute-eval engine) **IS** of equal
difficulty (it is LOCK-2's entire engine) → **C2 is a BLOCKED-candidate, refuted-on-live, ceded to
LOCK-2**.

**Disposition.** Hold **BLOCKED**. Do not author `ir.bbnf` until (a) GN-3.W1 lands `Sum` and (b) a
consuming GN/LOCK-7 wave is named. The family is honest only when C1 and C2 are costed apart and the
consumer is named as LOCK-7, not asserted present. SPEC-ONLY this pass (Constitution vii).

---

## FAMILY 3D — STAGED-RESIDUAL · BANKED · convergence 42% (↑ from 38)

**Architecture.** The IR as an explicitly two-stage program — a `BindingTime ∈ {Stage0, Stage1}`
**fact over the existing `BackendExpr` nodes** (an annotation on the 3B single IR, per binding-time
analysis / offline PE; Jones-Gomard-Sestoft 1993). A node is Stage0 iff all inputs are
grammar-constant; residualization = erasure of a Stage0 node from the emitted recognizer. PROVE-12
is the corollary (one Stage0 delimiter set → const mask, disasm-verified zero witness). **3D is 3B's
type-directed refinement, not a rival tower** — it shares `node_types`, adds only the binding-time
dimension.

**Mechanism — the substrate already ships (three un-unified organs).**
- **`BackendShape::CollapsedStage`** (`ir/src/cost.rs:131-136`, policy triad `LocalTempOnly` +
  `LocalLoop`) — the stage-plane's home; a first-class CSP candidate (`decision_csp.rs:68-78`).
- **`ScanAlphabet::derive_alphabet`** (`codegen/src/lower/css_scan.rs:38-102,487-565`) — the
  grammar-harvested Stage0 alphabet → `const … : &[u8]` feeding `css_scan_direct`. **Declared
  dependency; not re-authored** (directive).
- **PROVE-12** — the residualization transform, CONFIRMED (ins/B −4.8…−6.8%, cyc/B 0.80–0.91×,
  byte-identical, +18 LOC) on base `d2b32f6e3`.

**The distinctive thesis (UNPROVEN).** "Type-plane erasure = stage-0 erasure on the type sub-plane"
— the type plane costs zero at runtime (Constitution v) ⟺ the type plane is entirely Stage0. So the
`:204` `ty`-thread IS a Stage0 residualization to a Rust *type*, symmetric to PROVE-12's delimiter
erasure. **This is the same single-owner `:204` thread** (PROTO-1) — 3D's distinctive content over
"3B + a const table" is (i) `Stage` as a typed IR fact and (ii) the unification statement, both
still SPEC-only.

**Gates it carries.**
- **G-3D-1 (the live RED gate — proven consumer-less substrate).** `admits_collapsed_stage`
  (`passes/src/lib.rs:959-961`) reads `target.avx512bw && Entry(_)`; **avx512bw is permanently false
  on aarch64** (SPEC §0: x86 dead) → `CollapsedStage` is **never admitted**, `CollapsedStageNotViable`
  fires whenever declared (`diagnostics.rs:13`). The stage plane is a **dead shape** — the strongest
  live evidence of a Y.13 violation. First obligation: re-home admission onto a grammar-derived
  binding-time predicate ("this Entry rule has a Stage0 residual island ≥ k") **without smuggling x86
  back**.
- **G-3D-2 (JSON has no scan-side Stage0).** JSON hot rules already dispatch to hand kernels — the
  JSON win is **entirely** in the type sub-plane; if the type-plane PMU delta is zero, JSON's claim
  collapses to "typed façade, same speed" (acceptable only if declared as such).

**Exact open questions.** (i) Does `binding_times(CssScanProgram)` on the **real** CSS IR compute a
Stage0/Stage1 partition that **equals the set PROVE-12 hand-residualized** (oracle: parity
byte-identical iff correct)? (ii) Does re-homing `admits_collapsed_stage` make `CollapsedStage`
admissible on aarch64 via `decision_csp` without a stub (RED→GREEN on `CollapsedStageNotViable`)?
(iii) New-site cold **paired-PMU** JSON first-byte-dispatch delta — real ins/B or a named negative
(PROVE-09 law)?

**RETIRED sub-verdict (carried).** The PROVE-12-`u128`-const-generic byte-identity flagship stays
**RETIRED** (target `find_component_delim` gone from `codegen/src`) — do not re-pose. The surviving
live residual is already shipped as `ScanAlphabet` (`const &[u8]`, a different residual).

**Disposition.** BANKED — the stage-attribute subsumes into 3B's `Attributes` store; reactivate to
ADVANCED **only if** the unification is proven concretely (binding_times on real IR + the CollapsedStage
re-home + a measured type-plane delta). Most-prone-to-unmeasured-plausibility family.

---

## THE CONVERGENT SPINE (hypothesis, architecturally sound; rests on one UNRUN gate)

**One IR (3B substrate) + per-pass contracts (3A discipline) + a stage attribute (3D erasure); 3C
banked** until GN-3.W1 (sums) + a named LOCK-7 wave unblock it. The families **compose**, they do
not compete — once 3A is read as per-pass contracts over one IR, 3D as one attribute slot, and 3C's
only live win is the shared `:204` thread. What is now proven vs pending:

| Component | State after pass-2 |
|---|---|
| `:204` thread + verify predicate (RED→GREEN, zero footprint) | **PROVEN** (PROTO-1) |
| Fold fact-layer purity (`materialize_rule` recomputable) | **PROVEN by trace** (PROTO-2) |
| **3B-G1: execute the fold, regen×check byte-identical** | **UNRUN — the spine's load-bearing gate** |
| **3A refinement oracle (`differential(edge,corpus)`, injected-fault localization)** | **UNRUN — the organ that earns 3A** |
| CONSUME (`.target` → reproduce `real_typed_schema.rs`) | **REFUTED-as-stated**; behind the 3B fold |
| 3D binding_times on real IR + CollapsedStage re-home | UNRUN (BANKED) |
| 3C C1 (grammar-specified data model) | BLOCKED (sum-node + LOCK-7 consumer) |
| 3C C2 (grammar-to-grammar transposition) | **BLOCKED-candidate — refuted, ceded to LOCK-2** |

---

## INCOMPATIBILITY MAP (which families cannot coexist, and precisely why)

1. **3A-as-standalone-typed-multi-level-tower ⊥ 3B-single-IR.** The lock's one hard architectural
   incompatibility: distinct typed value levels (a real second tree) vs one IR with attributes. PROTO-1
   resolved it toward **one IR + per-pass contracts**; the *standalone tower* formulation is **BANKED**.
   3A-as-discipline and 3B-as-substrate **compose** — not incompatible once 3A is re-read. The
   incompatibility is real but retired; only the banked formulation re-opens it.

2. **3C-C2 ⊥ the whole lock (and the constitution).** Grammar-to-grammar transposition requires a
   tree-rewrite engine bbnf lacks → importing one breaches `no-orthogonal-codepaths`/`system-cohesion`,
   OR it collapses 3C's transposition into **LOCK-2's engine** — which violates **XL-B** (LOCK-3 owns
   the tower's shape and the transposition; LOCK-2's engines operate *within* named decision points,
   they do not *become* the transposition). C2 cannot coexist with LOCK-3 owning its own spine.
   **BLOCKED-candidate:** stalled on a missing primitive (a term-transduction facility) of difficulty
   **equal to** the original problem.

3. **The seam-dissolution is single-fundable only.** `has_rule("value")`@98 +
   `derive_materialization_roles`@1403 are the **same live hardcodes** that 3B would "retire via the
   fold" and 3C would "dissolve via grammar." Two routes at one seam; the gestalt funds **one**. 3B
   (ADVANCED) owns it; 3C's grammar-dissolution is the banked alternative that reactivates only if 3C
   unblocks. Funding both is a double-home breach.

4. **`Project.ty@204` is single-owner, not four-way.** 3A verifies it, 3B threads it as
   `attrs.node_type`, 3C's product declares it, 3D erases it as S0 — all four claim the **same two
   lines**. PROTO-1 discharged the thread once (RAN). The thread has **exactly one owner** (a GN-3
   wave, `no-value-discard`); the other three **CONSUME** the threaded value. Funding it four ways is
   forbidden. (Not an architectural incompatibility — a single-funding discipline.)

5. **CONSUME (`.target` population) ⊥ pass-2 parsimony; belongs to the 3B fold.** PROTO-1 refuted
   `target = Some(ty)` as a one-liner: `.target` is dead on the typed path (render clones the hand
   `DirectSchemaSet`). Reaching true CONSUME requires deleting `emit_typed_from_source`'s schema param
   + synthesizing `DirectSchemaSet` from `sink_only` + resolving JSON's zero-arrow corpus — i.e. it is
   **inside G-3B-1's fold**, not a separable 3A/3D move. The three separate CONSUME formulations
   cannot each land independently.

6. **Driver seat ⊥ LOCK-2 merge (XL-B / directive).** LOCK-3 owns *where* the driver sits (the fact
   contract at the L1→L2 edge); LOCK-2 owns *what engine*. 3A/3B respect this; 3D's per-site
   staging-form scheduling **must stay LOCK-2's seat, gated on a real consumer** — a LOCK-3 hardcoded
   emitter branch for staging-form is incompatible with the boundary.

---

## PASS-3 DIRECTIVES (the two sharpest, returned as prototype briefs below)

The convergent spine now rests on exactly two UNRUN gates. **PROTO BRIEF 1** executes G-3B-1 (the
fold, byte-identical) — earns the substrate the whole spine hangs on. **PROTO BRIEF 2** executes the
3A refinement oracle — earns 3A beyond a tautological verifier and delivers the CI contract every
byte-identity gate is an instance of. 3D stays BANKED (its distinctive is the shared thread, already
run; its remaining organ is confirmatory). 3C stays BLOCKED (born-RED on GN-3.W1 + LOCK-7).
