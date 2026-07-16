# LOCK-3 · Family 3A — CONTRACT-TOWER (pass-2 research)

A small fixed tower, each level a total function with a checkable contract:
round-trip / parity oracles per level, CI-enforced. This report develops that
family alone, pinned to the sole living tree `skinny@c43126422` (the directive's
`b3cb8c16b` and R6's `7f3be27c2` are stale ancestors; every `path:line` below was
opened on live).

---

## 0. Base reconciliation — two trees, one living

`skinny/` is the sole living implementation (Constitution; R6 §0 restates it).
The shared directive names the "downstream type consumer" as
`TypeAnalysis::from_ir` at `backend/driver/analysis.rs` — that symbol exists ONLY
in the LEGACY tree (`crates/core/src/backend/driver/analysis.rs:16` `struct
TypeAnalysis`, `:22 pub fn from_ir(ir:&GrammarIR)`), over the legacy `GrammarIR`,
a different type from skinny's `ir::GrammarIr`. **There is no `TypeAnalysis` in
skinny.** So the shared question, read literally, targets a cross-tree consumer
absent from the living spine. The 3A disposition re-asks it against live: *who,
on skinny, consumes the projection type, and what contract governs that
consumption?* — answered decisively in §2.

Likewise `derive_materialization_roles@runtime_generator.rs:1403` is a
legacy-tree name (skinny `runtime_generator.rs` is 522 LOC; its living analogue
is `extract::derive_materialization_plan`@`passes/src/lib.rs:1139`). Only
`has_rule("value")`@`runtime_generator.rs:98` is genuinely live. Directive seam
citations must be re-pinned before use.

---

## 1. The enumerated edge-DAG of the live spine (answers directive Q3)

`passes::compile`@`passes/src/lib.rs:32-68` is the whole transposition tower. The
distinct **value levels** and the total functions between them:

```
 .bbnf ──parse──▶ L0  ir::GrammarIr           (ExprKind, 10 arms; Project{body,target,ty})
                    │
      normalize ─── E0 ──▶ L0'  normalized GrammarIr        (lib.rs:33)
                    │
   ┌────────────────┼───────────────────────────────────────────┐
   │ E1 infer       │ E2 materialize (extract::single_plan:1283) │
   ▼ (lib.rs:35,149)▼                                            │
 T  layout::types::TypeFacts        L1  ir::BackendIr / BackendExpr (13 arms;
   (node_types,rule_types)              SpanMark/TapeEmit/DirectBuild/ValueProject/Return)
   │  consumed by                       │
   │  layout::run(:36) ─▶ LayoutFacts   │  E3 sink lowering
   │  (cost/shape/policy/CSP facts)     ▼  (lower::sink_only::lower_program, sink_only.rs:126)
   │                                 L2  SinkOnlyProgram / SinkOnlyExpr (12 arms)
   └────────────┐                       │  ── OR ── lower::rust::lower_to_rust (rust.rs:32,
                │                        │            CostFacts W7 fail-closed gate)
     LayoutFacts + BackendIr ──▶ PipelineOutput (lib.rs:62)
                                         │  E4 emit (runtime_generator::render, :78)
                                         ▼
                            EmittedSource {BTreeMap<file, rust src>}
```

Two facts the framing must reconcile:

- **`materialize_rule`'s SpanMark/TapeEmit/DirectBuild/Return framing** IS the
  `BackendExpr` sum at `ir/lib.rs:376-413` (13 arms) — reproduced in
  `SinkOnlyExpr` (`sink_only.rs:78-100`, 12 arms; `Entry` folded). E3
  (`lower_expr`@`sink_only.rs:174-235`) is a **structure-preserving 1:1 arm map**:
  every `BackendExpr` arm has exactly one `SinkOnlyExpr` image, `ValueProject.ty`
  copied through unchanged (`:228-232`). This is the cleanest candidate for a
  round-trip oracle — it is already a total renaming.
- **`SinkOnlyProgram`** and the CSS **`CssScanProgram`** are the two arms of
  `LoweredArtifact` (`runtime_generator.rs:19-24`). CSS **bypasses L1/L2
  entirely** — `lower_for_runtime`@`:40-71` probes `css_scan::derive_program`;
  `Ok ⇒ Scan` (raw-AST → SIMD scan), `Err ⇒ SinkOnly`. So the DAG has **a second
  root-to-emit path** that never touches BackendIr. Any contract tower must cover
  BOTH or declare the CSS island out of scope with a named reason.

**Count: three transposition edges carry value** (E1 type, E2 materialize, E3
sink) + one emit codegen (E4) + one bypass branch (CSS). "Distinct value levels"
= four typed IRs (GrammarIr, {TypeFacts | BackendIr}, SinkOnlyProgram,
EmittedSource) plus the CSS side-IR. This is the "small fixed tower (three
levels)" of the 3A charter made concrete and total — provided E4 is proven a pure
projection of L2 (test 5, §7).

---

## 2. The `->` type at the infer edge (shared Q + verify Q), on live

**The decisive live fact.** `ExprKind::Project.ty` has **exactly one consumer on
the entire tree**: the materializer `extract::single_plan`@`passes/src/lib.rs:1349`

```
ExprKind::Project { body, target, ty } => BackendExpr::ValueProject { body:…, target:…, ty: ty.clone() }
```

Every OTHER site that matches `Project` drops `ty` via `..` and recurses into
`body`: infer `:204`, nullability `:337`, and eight analyses (`:786 :809 :831
:923 :1096 :1507 :1543 :1591 :1630`). The `:204` drop is therefore **not a leak
into emission** — the declared type already reaches emission through the disjoint
materialize edge (E2): `Project.ty → ValueProject.ty (BackendExpr) → ValueProject.ty
(SinkOnlyExpr, sink_only.rs:228) → emit`, an **opaque `Option<String>` forwarded
UNCHECKED**.

`infer` (`:149-156`) at `:204` re-derives the projection type **structurally**
(`self.infer_expr(*body)`), never reading the declared `ty`; its `TypeFacts`
feed only `layout::run`@`:36`. R6 §0 confirms the consequence: `grammars/json.bbnf`
has ZERO `->`, so the type machinery is exercised **only by Sheets** on the
general-descent path; the `ty` string on JSON is vacuous.

**Consequence for 3A.** Threading `:204` `ty` does not *feed* a starved consumer
— the consumer (`single_plan:1349`) already holds it. Its value is to install the
**missing contract**: today the declared `ty` is a raw String forwarded to codegen
with nothing asserting it is *consistent with the body's structural type*. The
contract is exactly:

> `verify_project(node):  infer_expr(body)  ⊨  declared_ty`

wired as a per-pass predicate over `Project`@`:204`. It reads **RED = unverified**
on HEAD (the fact is dropped, no oracle exists) and **GREEN** once `ty` threads
into `node_types` and is checked against `infer_expr(body)` on the real Sheets
corpus. **Zero emit footprint is structural, not hoped-for**: the predicate adds
no output — `node_types` feeds `layout::run`, which does not branch on projection
node-types, and the declared `ty` already flows to emission via E2 unchanged.
Regen is byte-identical with the predicate on/off *by construction*, because the
predicate writes nothing to any edge's output. That is the RED→GREEN transition
the prototype must execute: add the fact + the check; regen four fleet grammars;
diff `EmittedSource` byte-for-byte (expect identity); then inject a contradictory
`-> Number` on a string-bodied projection and watch the predicate flip RED.

This also **retires the opaque-String hazard**: `ValueProject.ty:Option<String>`
is unparsed at every level. The contract turns it into a *checked* fact by
reconciling it against `passes::Type` (`lib.rs:234-243`) at the one site that
already reads it — the "consume one threaded node-type here rather than
re-derive it" the directive demands.

---

## 3. `differential(edge, corpus)` — one runner generalizing what already ships

The directive's runner is **already present in two specialized instances**; 3A's
job is to prove one parametric runner reproduces both and localizes an injected
fault to its edge.

- **Edge = E4/CSS scan.** `crates/runtime/tests/css_scan_differential.rs` — the
  production `css_balanced_component_scan` vs an independent scalar
  `scan_reference`, asserting **byte-identical** `(byte,pos)` STOP, `None`-at-EOF,
  and error-offset, swept over **every start position × 4 grammar-derived STOP
  sets × 4 real corpora** (71 KB–495 KB). This is a per-edge differential with a
  clean-room oracle — the template.
- **Edge = whole-pipeline JSON.** `crates/bbnf-bench/benches/json_parity.rs` —
  `track1_generated` vs `track2_handcoded` via `track2_tape_fingerprint`,
  `track1_digest/track2_digest`, `track1_strict_product/track2_strict_product`
  (`:52,202-218`). These are the XOR-checksum / json_parity digests the directive
  names; they compare the two *implementations*, not the intermediate edges.
- **SIMD parity** (`bbnf-simd/tests/checkasm_parity.rs`) is the same idea at the
  kernel edge.

**The generalization** the prototype must execute: `differential(edge, corpus)`
with `edge ∈ {E1,E2,E3}` and a per-edge oracle:
(a) **E3** — round-trip: `SinkOnlyExpr ↦ BackendExpr` is a total inverse renaming
(the arm map is 1:1), so `lower_expr` is checkable by structural equality of a
re-lifted tree — a *free* oracle, no reference implementation.
(b) **E2** — the declared-`ty` contract of §2 IS the edge oracle (materialize is
where `ty` is consumed).
(c) **Terminal** — reproduce the `track1/track2` digests. **Localization**: inject
a mis-lowering at exactly one arm (e.g. `TapeEmit`→`SpanMark` in `lower_expr`) and
require the runner to name **E3** because the E3 round-trip breaks while E1/E2 stay
GREEN and the terminal digest also breaks — the edge whose per-level oracle *and*
the terminal oracle flip, minus edges downstream, isolates the site. **Coverage
metric**: per-arm reachability over the fleet — count `BackendExpr` arms actually
constructed by `single_plan` across {json, css, sheets, +1}; an arm with **zero
witnesses** is reported **`unverified`, never `sound`** (Y.13 applied to oracles).
Today `ValueProject` has witnesses only on Sheets; `Return`, and the
`OffsetTape`/`EventTape`/`CollapsedStage` shapes (`ir/lib.rs:361-367`) may be
zero-witness — the runner must surface that census, not hide it.

---

## 4. Prior art (cited, mapped to each 3A mechanism)

- **LLVM `verifyModule`/`verifyFunction`** and **MLIR op `verify()` +
  `-verify-diagnostics`** — the canonical "each IR level carries an invariant
  predicate, CI-enforced" pattern; MLIR's per-op verifier + trait interfaces is
  the direct model for a per-`ExprKind`/`BackendExpr`-arm verify hook.
  (mlir.llvm.org/docs/DefiningDialects/Operations, "Verifiers".)
- **Nanopass framework** (Sarkar/Waddell/Dybvig, ICFP'04; Keep & Dybvig, ICFP'13)
  — the "small fixed tower of total functions, each defined by the language it
  consumes/produces" is literally the 3A thesis; each pass has a language
  definition the framework checks structurally. E3's 1:1 arm map is a nanopass
  "identity on all but one form" transformer.
- **Translation validation / CompCert** (Pnueli-Siegel-Singerman 1998; Leroy,
  CACM 2009) — validate each *run* by comparing input/output rather than proving
  the compiler. `differential(edge,corpus)` is translation validation at edge
  granularity; injected-fault localization is its refutation-completeness test.
- **rustc `-Zvalidate-mir`/`-Zverify-llvm-ir`, GHC `-dcore-lint`** — staged
  MIR/Core invariants between passes; the "zero-emit-footprint" discipline (a
  checker that never changes codegen) mirrors these validation-only passes.
- **MiniZinc/Z3 solver-checkers** — a model's solution independently re-checked
  against constraints; the E2 `ty`-contract has the same shape (declared `ty`
  re-checked against inferred structure), connecting to LOCK-1 without owning it.
- **tree-sitter corpus tests** and **egg/egglog `check`** — corpus-driven
  differential + rewrite-invariant checking give the per-node witness-count idiom.

---

## 5. Integration seams (live `path:line`)

| Seam | Location | Role in 3A |
|---|---|---|
| infer `Project` drop | `passes/src/lib.rs:204` | install verify predicate here |
| sole `ty` consumer | `passes/src/lib.rs:1349` (`single_plan`) | E2 edge; `ValueProject.ty` build |
| `passes::Type` | `passes/src/lib.rs:234-243` | structural type to check `ty` against |
| pipeline driver | `passes/src/lib.rs:32-68` (`compile`) | where per-edge oracles hang |
| `BackendExpr` sum | `ir/lib.rs:376-413` | L1 arms (SpanMark/TapeEmit/DirectBuild/ValueProject/Return) |
| E3 1:1 arm map | `sink_only.rs:174-235` (`lower_expr`) | free round-trip oracle |
| existing edge differential | `runtime/tests/css_scan_differential.rs` | the runner template |
| existing digest oracles | `bbnf-bench/benches/json_parity.rs:52,202-218` | terminal parity oracle |
| CSS bypass probe | `runtime_generator.rs:40-71` | the second root→emit path to cover |
| name-gate | `runtime_generator.rs:98` (`has_rule("value")`) | shape fact, not name — 3A models as an edge fact |
| roster validator | `grammar_profile.rs:3,35` | E4 output contract (file roster) |

---

## 6. Named risks & counterexamples

1. **The `ty` contract may be vacuously GREEN.** JSON has no `->`; if the Sheets
   projections all declare types trivially equal to `infer_expr(body)`, the RED
   branch is never exercised on real corpora — *sound but untested*. Mitigation:
   fault injection is mandatory (§2), and the census (§3) must show ≥1 non-trivial
   Sheets projection where declared `ty` constrains beyond the body's structural
   type, else 3A reports the contract `unverified`.
2. **Opaque `ty:Option<String>` has no grammar-side type language to check
   against.** `passes::Type` (7 arms) is structural, not nominal; the declared
   `ty` is free-form (`"Number"`, `"String"`). Reconciling them needs a
   name→Type resolution absent on live — a **born-RED dependency on LOCK-1** (the
   type language, owner: LOCK-1). 3A carries it as a named edge, not a homegrown
   parser; without it the contract degrades to canonical string-equality.
3. **CSS bypass double-root.** A tower proven total on GrammarIr→…→emit does not
   cover the `Err ⇒ SinkOnly` / `Ok ⇒ Scan` fork; a contract ignoring it
   over-claims. Either the fork is a first-class edge with its own oracle
   (`css_scan_differential` already is one) or the tower is declared two-rooted.
4. **Zero-witness arms falsely "sound".** `Return`, `CollapsedStage`,
   `EventTape`/`OffsetTape` may never be constructed by the fleet; a differential
   reporting them GREEN is lying. The `unverified` label (§3) is the guard; risk
   is CI treating absence as pass.
5. **Zero-footprint is contingent.** The "adds no output" guarantee holds only
   while `layout::run` never reads projection node-types; must be re-verified
   after any LOCK-1 change that makes layout consume richer type facts.

---

## 7. What a prototype MUST test to move 3A

1. **RED→GREEN + zero-footprint**: add `node_types[Project]=declared_ty` fact +
   `verify_project` predicate at `passes:204`; regen {json, css, sheets, +1};
   assert `EmittedSource` byte-identical (footprint=0); then inject a
   contradictory `->` and assert the predicate flips RED. (Executes the shared Q.)
2. **E3 free round-trip oracle**: implement `SinkOnlyExpr→BackendExpr` re-lift;
   assert `relift(lower_expr(x)) == x` over the fleet; inject an arm swap; assert
   equality breaks *at E3* while E1/E2 stay GREEN.
3. **`differential(edge,corpus)` reproduction**: one runner reproduces the
   `track1/track2` json_parity digests AND the css_scan differential from a single
   `(edge,corpus)` parameterization; report the per-arm witness census with
   `sound|unverified` labels.
4. **Localization**: single-arm fault injection at E2 and E3; the runner must name
   the injected edge, not merely "a failure".
5. **E4-as-projection + net-LOC**: prove `render` (`runtime_generator.rs:78`) is a
   pure projection of L2 so "three levels" holds; measure LOC delta of the tower
   vs today's ad-hoc checks, and whether the E4 emit surface (JSON 8-file roster,
   general-descent 5-file, CSS scan — `grammar_profile.rs:3,17`) collapses under
   one contract or stays three.

Anything short of an executed regen byte-diff and an executed fault-injection is
SPEC-ONLY (Constitution vii); this family advances only on the running artifact.
