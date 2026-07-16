# LOCK-3 — THE IR TRANSPOSITION SPINE · PASS-2 FAMILY SPECS (synthesis)

*One cogent specification per family while the routes remain incompatible — architecture, mechanism,
integration seams (path:line), the gates each would carry (born-RED with named owners), and its exact
open questions. Then the incompatibility map. Distilled by SYNTHESIS (Fable-tier) over the pass-2
research corpus (A/B/C/D/priorart), the prior pass-2 prototypes (PROTO-1, PROTO-2, PROTO-1-t1,
PROTO-2-3A), both critics, the REGISTRY ledger, and CROSS-LOCK. Elegance earns nothing on its own; a
family stalled at a missing primitive of difficulty equal to the original problem is marked
BLOCKED-candidate.*

---

## 0. VERIFIED BASELINE (re-greped by SYNTHESIS on live this pass)

**Live HEAD = `0bb33a77b`** (`git rev-parse HEAD`). Base-drift **discharged under XL-H**: both the
fresh-research pin `061c79fa7` and the prior-registry pin `c43126422` are ancestors of HEAD, and
`git diff --stat {061c79fa7,c43126422}..HEAD -- skinny/crates` is **empty for both** — the intervening
commits touched only `restart/.../tranches/` (research corpus + fold ledgers), never `skinny/crates`.
The code tree every LOCK-3 prototype measured is **byte-identical to HEAD**; PROTO-1 / PROTO-1-t1 /
PROTO-2-3A byte-identity floors transfer verbatim. Wrong-artifact-risk is nil.

Anchors re-verified by hand on `0bb33a77b` (all CONFIRMED; the fresh research independently re-found
each at `061c79fa7`):

| Fact | Live truth |
|---|---|
| `->` ty-drop | `passes/src/lib.rs:204` `ExprKind::Project { body, .. } => self.infer_expr(*body)?` (both `target` + `ty` bound by `..`, dropped) |
| `:203` non-defect | `ExprKind::Annotation { .. } => Type::Builtin(BuiltinTy::Unit)` (legit untyped `@ws`/`@cap`) |
| name-gate survives | `has_rule("value")` @ `codegen/src/runtime_generator.rs:98` |
| schema is Struct-only | `DirectTypeKind` @ `codegen/src/direct_schema.rs:52` has **one** variant (`Struct{fields,ignored,unknown}`); no `Sum` |
| `.target` is a DEAD field | `DirectBuildTarget { … }` constructed at **zero** sites in `skinny/crates`; the 4 `target: Some` in passes are all `ExprKind::Ref{target:Some}` (rule-ref), never `DirectBuildField.target`; the field is `None` everywhere and read nowhere |
| `from_ir`/`TypeAnalysis` absent | **zero hits** in `skinny/crates`; they exist ONLY in legacy `crates/core/src/backend/driver/analysis.rs` over the legacy `GrammarIR` |
| `CollapsedStage` gate is DEAD | `admits_collapsed_stage` @ `passes/src/lib.rs:959` = `target.avx512bw && matches!(…Entry(_))`; `avx512bw` permanently false on aarch64 (SPEC §0: x86 dead) |

**The spine as an edge-DAG (research-A §0, the sharpest new cartography of the pass).** Walking
`codegen::emit_from_source`, the live spine is **six labeled edges over FOUR distinct value levels** —
not three:

```
.bbnf ─E1 parse─▶ GrammarIr(+node_types) ─E2 infer─▶ Type/ExprId ─E3 lower─▶ BackendExpr
      ─E4 materialize─▶ BackendExpr' ─E5 sink_only─▶ SinkOnlyProgram ─E6 emit─▶ .rs
```

- **L0** `GrammarIr` + `facts.node_types` · **L1** `BackendExpr` (13-arm) · **L2** `SinkOnlyProgram`
  (`SinkOnlyExpr` mirrors E4's `SpanMark/TapeEmit/DirectBuild` near-isomorphically) · **L3** emitted Rust.
- **E4 is a rewrite *within* L1** (`materialize_rule` returns `BackendExpr`, keyed
  `descriptors.get(&rule_id)`), not a level. **L2 is a redundant re-encoding of the materialized L1** —
  it carries no value L1 lacks; it strips `ValueProject`/`Ref`-target detail the emitter never reads.
- **Two facts bind every family.** (1) The tower is **3B-shaped today** — one attribute-annotated
  `LayoutFacts` bundle (`passes/src/lib.rs`), `BackendExpr`/`SinkOnlyProgram` a materialized plane, a
  **fractured** emission stage (`json_sink_direct/`, `css_scan_direct/`, `general_descent/`,
  `json_typed_direct.rs`); LOCK-3 either hardens (3A/3B) or re-founds (3C/3D), every divergence from
  the GN codex a **named supersession** (Constitution ix). (2) The **`Project.ty`@204 thread** is
  single-owner and **DISCHARGED for the fact plane** (PROTO-1: RED→GREEN, zero emit footprint) — but
  the schema stays **starved** (`.target` dead; sole schema producer is the hand file
  `xtask/src/real_typed_schema.rs`), and both the CONSUME half and the consistency CONTRACT over the
  threaded value remain open.

---

## 1. FAMILY 3A — CONTRACT-TOWER · ADVANCED (as discipline) · standalone-tower BANKED

**Architecture.** A small fixed tower whose **transposition edges each carry a decidable contract**
`C_k(in_k, out_k)`, run at regen and in CI (the MLIR `--verify-each` / GHC `-dcore-lint` /
rustc `-Zvalidate-mir` discipline; nanopass's contract-per-level without nanopass's many-IL cost —
prior-art §2, §7 "IR proliferation" trap). The standalone typed-multi-level-tower is **BANKED**; 3A
advances **as a discipline layered on the 3B single-IR substrate** — the two compose (§6), and this
composition already has running evidence on both sides.

**Mechanism — three contracts, oracles already resident.**
- `C_{L0→L1}` **structural + type totality.** Every visited `ExprId` has a `node_types` entry; every
  admitted rule maps to exactly one `MaterializationDescriptor`; every `->` node's threaded type equals
  the `DirectBuildTarget.type_ref` its rule emits (the producer≡consumer identity — read once, never
  re-derived).
- `C_{L1→L2}` **serialize faithfulness.** `SinkOnlyExpr` preserves the `SpanMark/TapeEmit/DirectBuild`
  sequence + labels/kinds. This is the contract that **justifies or condemns L2 as a level** (§0, §5).
- `C_{L2→L3}` **emission determinism/parity — already shipped:** `xtask::regen::check_target`
  (`xtask/src/regen.rs:37`) byte-identity per grammar, plus the semantic digests in
  `bbnf-bench/src/direct_struct.rs:421-482` (`parity_digests`/`typed_checksum`/`track2_strict_product`).
  3A's strongest existing leg.

**The distinctive organ — RAN (PROTO-2-3A).** `differential(edge, corpus)`: the per-edge refinement
oracle localizing an injected mis-lowering **to its edge**, not merely to "a failure." E3 correspondence
oracle GREEN on live `json` BackendIr (15 rules/100 nodes) + 13/13 synthetic; a single-arm E3 fault
named to its arm with **zero false-positive witnesses** across a 5-fault matrix (incl. a
same-discriminant payload corruption a tag-only check would miss); an E2 fault trips E2 while E3 stays
GREEN (E2↔E3 separation proven). Y.13 witness-census discipline proven (zero-witness arm ⇒ `unverified`,
never `sound`). This is the "sharpest UNRUN uncertainty" the prior frozen spec mislabeled — now RAN.

**Integration seams (path:line, live).**
`passes/src/lib.rs:204` (produce) · `:1149-1287` (`derive_materialization_plan`, carry into `.target`)
· `:1362-1391` (`materialize_rule` — the E4 splice + the partial `descriptors.get` else-body) ·
`:1403-1491` (`derive_materialization_roles` — the heuristic classifier) · `xtask/src/regen.rs:37`
(`check_target`, the shipping `C_{L2→L3}`) · `bbnf-bench/src/direct_struct.rs:421-482` (the differential
runner substrate) · `runtime_generator.rs:98` (`has_rule("value")` name-gate, to become a contract
violation not routing).

**Gates it carries (born-RED, named owners).**
- **G-3A-a — name→`Type` resolution (born-RED on LOCK-1).** The proven predicate checks
  `node_types[project]==Type::Declared(d)` **and** `≠ node_types[body]`. `Type::Declared(String)` is a
  **nominal** wrapper never structurally equal to the structural `infer_expr(body)` → the `≠ body` clause
  is **vacuously true whenever `ty=Some`**. The organ verifies **threading-presence**, not
  **declared-vs-inferred consistency**. True non-vacuity is born-RED on LOCK-1; until it lands the
  contract degrades to string-equality. **The organ is PROVEN; the *contract* is unearned.**
- **G-3A-b — the type conjunct of `C_{L0→L1}` (born-RED on GN-3.W2).** On live the `->` type dies at
  `:204`, `.target` is dead → the type contract is born-RED with a named owner (GN-3.W2 threads it). 3A
  supplies the checkable contract that thread must satisfy; it does not itself land the thread.
- **G-3A-c — the E4 partial function (LIVE `no-silent-epsilon` breach, ownable NOW — research-A §R3/§6.4,
  the pass's freshest sharp gap).** `materialize_rule`'s `descriptors.get(&rule_id)` **else-return-`body`**
  and the byte-literal `>= 4`-value-ref heuristic are a **silently-partial** function: a rule the
  classifier misses lowers with **no materialization and no error**. A "total function with a checkable
  contract" cannot rest on it. The contract must make role assignment **total** (every rule → a role or
  explicit `NoRole`), turning the name-gate + byte-literal gates into contract *violations*. → **Brief #2.**

**Exact open questions.**
- (i) Is the fixed tower **three levels or four**? Requires measuring the **L1→L2 information delta**
  (does `SinkOnlyExpr` drop any datum `BackendExpr` carries the emitter needs?). If L2 is a pure
  re-encoding the collapse to three is legal; if lossy it is illegal (§5 incompat #5). **UNRUN.**
- (ii) Is `render` (`runtime_generator.rs:78`) a pure projection of L2 (so E4-emit carries no oracle)?
  **UNRUN.**
- (iii) The CSS `Err⇒SinkOnly / Ok⇒Scan` fork (`runtime_generator.rs:40-71`): first-class edge oracle,
  or a declared two-root tower? **UNDECIDED.**
- (iv) Exhibit ≥1 Sheets projection where declared `ty` constrains **beyond** `infer_expr(body)` — else
  report the contract `unverified`. Gated by G-3A-a.
- (v) The runner is a `+607`-LOC **probe**, not the shipping `differential(edge,corpus)` CI contract
  that is 3A's stated deliverable (every byte-identity gate in the lock = one instance of one edge oracle).

**Withheld** (settled): do not re-litigate the standalone typed-multi-level-tower (BANKED); do not
re-argue the oracle localizes an injected fault (PROVEN); do not re-run E3 on the shipping map (GREEN).

---

## 2. FAMILY 3B — SINGLE-IR-MULTIPASS · ADVANCED (the convergent substrate)

**Architecture.** One rich IR refined in place by annotating passes (the **LLVM shape**, prior-art §3);
`BackendShape` and the materialization plan become **attributes hung on that one IR** (`HashMap<RuleId,
MaterializationDescriptor>` + `HashMap<ExprId, TypeDesc>`), not fresh tiers; the emitter is **one fold**
`emit(&GrammarIr, &Attributes)` that *reads* those attributes. The **existence proof is in-tree**: the
mature legacy `crates/core` runs exactly this — `TypeAnalysis::from_ir` clones `ir.types`;
`BackendPreparation` hangs `alt_strategies`/`delim_scan_configs`/`key_dispatch_configs` as
`HashMap<NodeId,_>` side-tables. Skinny's `GrammarIr` (`ir/src/lib.rs:34-39`) has **no attribute slot**;
the 3B move promotes the transient `MaterializationPlan` (`passes/src/lib.rs:1127-1130`, already exactly
this table) from a threaded argument to a durable IR annotation, and **deletes the second tier**.

**Mechanism.** `materialize_rule`'s emit-order splice `Seq[SpanMark,body,SpanMark,TapeEmit,DirectBuild,
Return]` is a **pure derivation** of `(rule_id, body, descriptor)` — **PROVEN by trace** (PROTO-2:
the four `SinkOnlyFacts` accumulations read only `GrammarIr + backend_shape + descriptor`), so it is
recomputable inside one fold; byte-identity reduces to "does the fold build the same descriptor table?"
Two of the three dispatch decisions are **already fold-clean** (`lower_for_runtime` selects Scan(CSS) vs
SinkOnly purely by shape — "no grammar NAME, no config field"). The one surviving fork is
`emit_sink_only`'s `has_rule("value")`, which 3B replaces with a branch on the descriptor's `TapeKind`.

**Running evidence (PROTO-1-t1, byte-identical 3/3).** The emit-dispatch seam retirement — replacing
the `has_rule("value")` name-gate with a **structural value-role census** — is **RAN byte-identical**
(`check-{json,sheets,real-typed}`), the first *run* code on the fold path. It also **refuted
PROTO-2's `dispatch_alt_count` discriminator by execution**: `== 0` turns `check-sheets` RED (Sheets
carries dispatch Alts too); the honest discriminator is the specific **≥4-of-6 value-category census**.

**Integration seams (path:line, live).**
`ir/src/lib.rs:34-39` (`GrammarIr`, no attr slot — the substrate edit) · `passes/src/lib.rs:1127-1130`
(`MaterializationPlan`, the table to promote) · `:1362-1391` (`materialize_rule`, folds into the emitter)
· `:1403-1491` (`derive_materialization_roles`, retire or single-home) · `runtime_generator.rs:40-73`
(`lower_for_runtime`, the already-clean firewall) · `:98` (`has_rule("value")`, to delete) ·
`json_sink_direct/dispatch.rs` (**318 LOC structural walker** to re-base) · `emit/general_descent/view.rs`
(**431 LOC structural walker** to re-base) · the 3 `json_templates/*` files (fixed runtime library —
stay `include_str!` verbatim, byte-identical by construction).

**Gates it carries.**
- **G-3B-1 — THE killer gate, UNRUN after two passes (the elegant-reduction trap, live).** *Execute*
  the whole `emit(&GrammarIr,&Attributes)` fold; **regen×check × {json,css,sheets,real-typed}
  byte-identical**. Two passes in: trace (PROTO-2) + one slice (PROTO-1-t1), **no fold**. "Executable"
  ≠ "executed." **Per the demotion trigger: run it next pass or demote 3B to "structurally-motivated,
  unproven."** → **Brief #1.**
- **G-3B-1a — the re-walk IS the unconverged hard part.** Deleting `SinkOnlyExpr` **adds back** a
  comparable re-walk of `GrammarIr` in `dispatch.rs` (318) + `view.rs` (431) — structural tree-walk
  consumers, not emission-only intermediates. Byte-identity risk lives precisely in re-basing those two
  walkers (a reordered/differently-keyed walk breaks identity). **The −100 net-LOC is the reward IFF
  the walkers re-base byte-identically — an unrun assumption, reported CONTINGENT, not a measured
  deletion.**
- **G-3B-2 — relocation ≠ retirement (a DRY smell, born-RED).** PROTO-1-t1's slice ports the value-role
  logic **into codegen**, duplicating passes-side logic (a `system-cohesion`/DRY breach the probe itself
  flags). The *name* is gone on that fork; the *clean single-home retirement* is not. Production home =
  thread passes-computed `roles.value` onto the attribute bundle (`LayoutFacts → Attributes`). True
  RETIREMENT stays born-RED on `DirectTypeKind::Sum` (GN-3.W1) + a value-role attribute.
- **G-3B-3 — materialization key unconfirmed.** per-`RuleId` suffices for JSON/Sheets; does any single
  rule need two distinct descriptors → per-`ExprId` forced (CSS L4 inline alternatives the candidate)?
  A partial migration is a `no-orthogonal-codepaths` defect. **Node-types are already per-`ExprId`;
  materialization is per-`RuleId`** — matching the legacy split. UNRUN fleet-check.
- **G-3B-4 — CONSUME correctly REFUTED-as-stated.** `.target` is a **dead field** (verified §0);
  `target=Some(ty)` one-liner refuted; true CONSUME (delete the schema param, synthesize `DirectSchemaSet`
  from `sink_only`, resolve JSON's zero-arrow corpus) sits **inside G-3B-1's fold**, not a separable move.
- **G-3B-5 — byte-identity incidental-vs-semantic.** header comments, `BTreeMap` key order, roster order
  must be shown incidental on the executed fold, every non-identical byte attributed; trace `css_scan`
  for a hidden fourth value level. UNRUN.

**Exact open questions.** (a) Does the whole fold regen byte-identical (G-3B-1)? (b) Do the two 750-LOC
walkers re-base without breaking identity (G-3B-1a)? (c) Is the −100 net-LOC real or does the re-walk
eat it? (d) Can `derive_materialization_roles` be **retired** (not relocated) without json.bbnf gaining
`->` (a LOCK-7 grammar-totality dependency)?

**Withheld:** do not build `PassManager`/`PreservedAnalyses` invalidation machinery (consumer-less until
a mutating Transform exists — the linear `compile` pipeline makes invalidation trivial today); do not
call the name-gate "retired" (PROTO-1-t1 proved it byte-identically *relocatable*, not retired).

---

## 3. FAMILY 3C — GRAMMAR-HOMOIOUSIA · BLOCKED (C2 is BLOCKED-candidate) · bank the gift

**Architecture.** The IR's node **algebra** is itself a bbnf grammar (`ir.bbnf`, ~40 lines: each node
kind a rule, each alternative a `->` naming its Rust variant); transpositions are grammar-to-grammar
maps; the generator eats its own type declarations (bootstrap alignment with LOCK-7). The family cleaves
into **two claims of very different strength** — the C1/C2 split is the real analysis progress.

- **C1 — the IR *data model* is grammar-specified** (enum/struct shells emitted from `ir.bbnf`).
  Research-C PROVE-A: author `ir.bbnf` for the 10 `ExprKind` arms, run through `emit_typed_from_source`
  with a Sum-capable schema, assert the emitted `enum` is variant-set-equal + byte-identical to the hand
  `ir/src/lib.rs:234-271`. **Bounded born-RED**, not this family's to invent.
- **C2 — the IR *transpositions* are grammar-to-grammar maps. REFUTED on live (the sharpest
  counterexample).** bbnf maps **bytes → tape**; `lower_expr`/`materialize_rule` are **tree → tree**
  folds with no byte stream, so **no bbnf grammar *is* this map**. Research-C §R-2 sharpens it: the
  transposition is **grammar + facts → grammar** (an *attributed* map — `label/shape/fields/kind` are
  synthesized by role classification, absent from source), not a pure rewrite. Realizing it demands a
  **term-rewriting / attribute-evaluation engine bbnf lacks** → importing one is an orthogonal-subsystem
  breach OR a collapse into **LOCK-2's engine**.

**BLOCKED-candidate ruling (C2).** The missing primitive — a term-rewriting/attribute-grammar engine —
is of **difficulty equal to the original problem** (a general transposition engine *is* the compiler).
Per the charter, elegance earns nothing here: **C2 is BLOCKED-candidate, ceded to LOCK-2** (provisional
on XL-B). Two named exits, both external: (a) fold facts into the grammar as `->` chains (**GN-3.W3**
"The Authored Shape"), making the map pure again; (b) model as an attribute grammar (Nanopass's actual
shape) — machinery bbnf does not have today. **C1** is *not* BLOCKED-candidate — it is blocked on a
**bounded** born-RED owner (Sum node, +420 LOC, scheduled).

**Integration seams (path:line, live).**
`ir/src/lib.rs:234-271` (frontend `ExprKind`, the sum to express + oracle diff target) · `:376-413`
(backend `BackendExpr`, `lower.bbnf` targets) · `:250` (`Regex{pattern:String}` — opaque leaf,
**dissolves the ONE-HIR precondition** per XL-C; regex is NOT a 3C dependency) · `passes/src/lib.rs:1349-1353`
(pure 7-arm variant→variant map) · `:1362-1391` (fact-dependent tail — the C2 refutation) · `direct_schema.rs:52`
(the born-RED Sum gate) · `json_typed_direct.rs:435` (`render_sum_arm`, to be added by GN-3.W1) ·
`codegen/src/lib.rs:130` (`emit_typed_from_source`, the generation driver already wired) ·
`grammar/bnf/bnf.bbnf` + `@pretty` (self-host precedent — grammar-describes-grammar + structured emit).

**Gates it carries (all externally owned).**
- **G-3C-1 — `DirectTypeKind::Sum` (born-RED, GN-3.W1, +420 LOC).** The product-only subset **excludes
  every `Alt`** = excludes `ExprKind` (10-arm) / `BackendExpr` (13-arm) themselves — must NOT be treated
  as standing for the formulation (elegant-reduction trap by construction). The IR *is* a sum of node
  kinds; without the sum node there is no family.
- **G-3C-2 — a named consumer (born-RED, LOCK-7·7B/7D).** On live there is **no `ir.bbnf`, no
  meta-grammar, one grammar (`json.bbnf`)**; C1's sole reader is an **unlanded LOCK-7 bootstrap/oracle
  wave** (Y.13). Admissible only as a born-RED edge under GESTALT-B.
- **G-3C-3 — the fixpoint (LOCK-7·7A).** `regen∘regen = regen` byte-identical over a grammar-generated
  `ir/src/lib.rs` carrying derives/doc-comments/impls — the sharpest engineering gate; likely forces
  per-derive/per-impl annotations. A recurring bootstrap cost, not a one-time bring-up.

**Note — one pass-1 blocker DISCHARGED by XL-C.** The LOCK-5 HIR-functor verdict is settled: "LOCK-3 3C
no longer hang[s] on ONE-HIR" (regex is an opaque `Regex(StringId)` leaf). Two external gates remain
(Sum + LOCK-7 consumer); the family stays BLOCKED. Distinctive content is **0% convergence-eligible**;
**SPEC-ONLY this pass.**

**Exact open questions (all gated on external owners; do NOT author `ir.bbnf` until they land).**
(i) Does the declaration face round-trip byte-identical once GN-3.W1's `render_sum_arm` exists (PROVE-A)?
(ii) What is the **pure-arm fraction** of `lower_expr` — do GN-3.W3's authored-shape arrows close the
fact gap so the map is pure again (PROVE-B; expected 7/10 pure, 3 fact-dependent)? (iii) Does the
generator eat itself without staging skew (PROVE-C fixpoint)?

**Withheld:** do not re-derive the blockers (settled); do not propose authoring the IR meta-grammar as
pass-3 mechanism (consumer-less-forbidden); `ir/cost.rs` stays hand-written primitives.

---

## 4. FAMILY 3D — STAGED-RESIDUAL · BANKED (subsumed into 3B's attribute store)

**Architecture.** The IR is an explicitly two-stage program: a **binding-time (BT) fact per node**
(`Static` = value fixed by the grammar at generation; `Dynamic` = depends on runtime input).
Residualization is **erasure of every Static node into a generator-time constant, emitting only the
Dynamic residual** — offline partial evaluation over `GrammarIr`, the two-level λ-calculus (Nielson &
Nielson) with the generator as `mix`. The stage lives **in the type** (LMS `Rep[T]`; prior-art §4), so
"type-plane erasure = stage-0 erasure on the type sub-plane" makes the zero-runtime-cost invariant
(Constitution v) mechanized rather than asserted. **PROVE-12 is the first Futamura projection** (the
grammar-constant delimiter set specialized to a `const`-generic `u128` mask). The stage mechanism **is
architecturally a 3B move** (`Stage`/`BindingTime` is one attribute slot on the same `Attributes` store);
the separate-staging-spine formulation stays **BANKED into 3B**.

**Why BANKED, not ADVANCED (Constitution vii — nothing 3D-distinctive ran).** 3D's content over "3B +
a shipped `ScanAlphabet` const" is exactly two claims, **both SPEC-only**: (i) `BindingTime ∈
{Static,Dynamic}` as a typed IR fact; (ii) the unification "type-plane erasure = stage-0 erasure" —
which **spec-cites-itself** (its warrant is Constitution v ⟺ "the type plane is entirely Stage0," a
definitional restatement). Four soft anchors, none 3D-convergence: **PROVE-12** measured a
**retired-target** site (`find_component_delim` gone from `codegen/src`) at stale base `d2b32f6e3`, PMU
not re-run; **`ScanAlphabet`** is **already shipped** (declared dependency, not 3D-authored);
**`admits_collapsed_stage`** reads dead `avx512bw` → `CollapsedStage` is a **dead shape** (a genuine
Y.13 consumer-less-substrate on the living tree — evidence of a PROBLEM, not a win).

**Integration seams (path:line, live).**
`ir/src/lib.rs:357` (`ShapeFacts`, the BT-fact home) · new `binding_times(&BackendIr)->StageFacts` pass
beside `passes/src/lib.rs:398` (`derive_backend_shape`) + `:959` (`admits_collapsed_stage`) ·
`css_scan.rs:487` (`derive_alphabet` — the Static island, declared dependency) · `css_scan_direct/mod.rs:112-149`
(the `const …_STOP: &[u8]` residuals) · `runtime/src/grammars/json/scan.rs:19` (`STRUCTURAL_ALPHABET`) ·
`json_sink_direct/dispatch.rs` (`derive_arms` first-byte keys — the NEW residual measurement site) ·
`ir/src/lib.rs:366` + `cost.rs:131,258,267` + `lower/collapsed_stage.rs` + `lower/tape_plan.rs:9-53`
(the DORMANT stage shape) · `:204`/`:1349`/`:1136-1391` (the shared `ty` thread as an S0 residual).

**Gates it carries (all under the banked banner).**
- **G-3D-i — the unification unproven (the distinctive, 0% run).** Attach a BT fact to live IR and
  prove "type-plane = stage-0" beyond restatement — the same `:204` thread as an S0 residual to a Rust
  *type* (3D **consumes**, does not re-own).
- **G-3D-ii — `binding_times(CssScanProgram)` == PROVE-12 partition (unrun).** Runnable via the PROVE-12
  checksum (parity byte-identical iff the Static set equals the hand-residualized set).
- **G-3D-iii — CollapsedStage re-home (unrun).** Re-home admission off `avx512bw` onto a grammar-derived
  BT predicate → admissible on aarch64 via `decision_csp`, no stub, **no x86 smuggled back**; RED→GREEN
  on `CollapsedStageNotViable`.
- **G-3D-iv — JSON type-plane PMU delta (unmeasured).** JSON has no scan-side Stage0 → the JSON claim is
  *entirely* the type sub-plane; absent a measured cold paired-PMU delta it collapses to "typed façade,
  same speed" (acceptable only if declared; PROVE-09 law).

**Named risks that bound the family (research-D).** **R-A degenerate BTA** — a naive BTA marks
*everything except input bytes* Static; the useful **frontier** (Static nodes with **Dynamic
consumers** — precisely the delimiter set PROVE-12 hit) is narrow and must be shown non-empty *and* that
residualizing exactly it (not more) wins. **R-E capacity hints are corpus facts, not grammar facts**
(`875`/`184`/… come from *counting documents*) → a grammar-sourced BTA **cannot** mark them Static —
counterexample to "all Static facts derive from the grammar."

**Exact open questions.** (i) Is the BTA frontier non-empty and *narrow* on real IR, or does it collapse
to the trivial "all-but-input Static" split (R-A)? (ii) Does the type-plane erasure buy a measured ins/B
delta at the JSON first-byte dispatch, or only a typed façade (G-3D-iv)? (iii) Can CollapsedStage be
re-homed onto a live aarch64 predicate without smuggling x86 back (G-3D-iii)?

**Withheld:** the PROVE-12-`u128`-byte-identity flagship is **RETIRED** — do not re-pose it; do not
re-author `css_scan_direct` (landed) — declare the dependency; per-site staging-form scheduling is
**LOCK-2's** seat (gated on a real consumer, provisional on XL-B), not a LOCK-3 hardcoded emitter branch.

---

## 5. THE INCOMPATIBILITY MAP (which families cannot coexist, and precisely why)

1. **3A-standalone-typed-tower ⊥ 3B-single-IR — the lock's one hard architectural incompatibility.**
   Distinct typed levels each a total function (3A's charter) vs one rich IR refined in place with
   BackendShape-as-attributes (3B's charter). These cannot BOTH be independent load-bearing towers —
   two IR homes for the same facts is a double-home breach (`no-orthogonal-codepaths`). **RESOLVED
   toward one-IR + per-pass contracts** (§6): 3A-as-standalone-tower **BANKED**; 3A's *discipline*
   composes onto 3B's *substrate*. Running evidence both sides (PROTO-2-3A's E3 oracle ran on the same
   live BackendIr the 3B fold would fold; PROTO-1-t1's slice byte-identical). The incompatibility is
   real but adjudicated — they coexist ONLY as substrate(3B) + discipline(3A), never as rival towers.

2. **3C grammar-dissolution ⊥ 3B fold-retirement — same seam, cannot be double-funded.** Both target the
   identical live hardcodes `has_rule("value")`@98 + `derive_materialization_roles`@1403: 3C would
   "dissolve via grammar-declared `->`", 3B would "retire via attribute-reading fold." The seam is
   dissolved **once**. **Single-funded to 3B decisively by execution** (XL-3): PROTO-1-t1 RAN 3B's
   byte-identical relocation of this exact seam. 3C's grammar-dissolution alternative reactivates **only
   if 3C unblocks** (Sum node + LOCK-7 consumer). Funding both is a double-home breach.

3. **3D per-site staging-form scheduling ⊥ LOCK-2's engine seat (XL-B boundary).** LOCK-3 owns *where*
   the fact contract sits at the L1→L2 edge; LOCK-2 owns *what engine* decides per-site residualization
   (cost/CSP over monomorph-count vs ins/B under the generated-size budget). A LOCK-3 **hardcoded
   emitter branch** for staging-form is incompatible with the boundary. 3D's stage-**attribute** is
   compatible (banked into 3B's store); 3D's per-site **decision** is not (ceded to LOCK-2, provisional
   on XL-B). NOT final until GESTALT-B.

4. **3C-C2 (transposition-as-grammar) ⊥ the live tree-to-tree fold reality — BLOCKED-candidate.** bbnf
   maps bytes→tape; the transposition is tree→tree with synthesized attributes (grammar+facts→grammar).
   No bbnf grammar *is* this map without a term-rewriting/attribute-eval engine of difficulty equal to
   the original problem. This is not a coexistence question but a **missing-primitive block**: C2 is
   ceded to LOCK-2 (provisional on XL-B) or awaits GN-3.W3's authored arrows to make the map pure.

5. **3A's four-level observation ⊥ 3A/3B's shared "three fixed levels" premise (internal, gating).** The
   live spine has FOUR value levels (§0); L2 (`SinkOnlyProgram`) is a redundant re-encoding of the
   materialized L1. Collapsing to three (which both 3A's "small fixed tower" and 3B's "single IR"
   assume) is **legal ONLY if `C_{L1→L2}` is a proven identity** — i.e. L2 drops no datum L1 carries the
   emitter needs. If L2 is lossy, the collapse is illegal and the tower is genuinely four levels. This is
   the **open question gating whether 3A and 3B agree on the level count** — measurable as the L1→L2
   information delta (research-A §6.1), UNRUN.

**No pure standalone incompatibility remains between the two advancing families** — 3A and 3B *compose*
(they do not converge to one mechanism; the critics confirmed this). The residual tensions are (2) the
single-funded seam (3B wins by execution), (3)/(4) the LOCK-2 boundary cessions, and (5) the internal
level-count question that both advancing families share.

---

## 6. THE CONVERGENT SPINE (hypothesis; architecturally sound; rests on two UNRUN load-bearing halves)

**One IR (3B substrate) + per-pass contracts (3A discipline) + a stage attribute (3D erasure); 3C
banked** until GN-3.W1 (sums) + a named LOCK-7 wave unblock it. The families **compose**, not compete
(prior-art §8 scores the external record the same way: a 3B-shaped single rich IR carrying 3A-nanopass
contracts, with 3D-LMS type-staging as the erasure mechanism, 3C admitted only as contract-level
homoiousia bounded by LOCK-7/no-ts-ir).

| Component | State after the four-prototype fold |
|---|---|
| `:204` thread + verify predicate (RED→GREEN, zero footprint) | **PROVEN** (PROTO-1) |
| Fold fact-layer purity (`materialize_rule` recomputable) | **PROVEN by trace** (PROTO-2) |
| 3A refinement oracle localizes an injected fault to its edge | **RAN** (PROTO-2-3A) — on the **free E3 edge**; E2↔E3 separation proven |
| 3A *contract* non-vacuity (E1-infer; declared-vs-inferred) | **UNRUN + gate-cannot-fail until LOCK-1** (name→`Type`) |
| Seam relocation (`has_rule("value")` → value-role census) | **RAN byte-identical** (PROTO-1-t1) — relocatable slice, DRY home; single-home + retirement deferred |
| PROTO-2's `dispatch_alt_count` discriminator | **REFUTED by execution** — working discriminator = ≥4-of-6 value-category census |
| **3B-G1: execute the WHOLE fold, regen×check byte-identical** | **UNRUN — the spine's load-bearing gate** (−100 net-LOC contingent on an unrun re-walk) |
| E4 role assignment total-vs-silently-partial (`no-silent-epsilon`) | **UNRUN — live breach candidate, ownable now** (research-A §R3) |
| CONSUME (`.target` → synthesize `DirectSchemaSet`) | **REFUTED-as-stated** (`.target` dead); behind the 3B fold |
| 3D binding_times on real IR + CollapsedStage re-home | UNRUN (BANKED) |
| 3C C1 (grammar-specified data model) | BLOCKED (Sum node + LOCK-7 consumer) |
| 3C C2 (grammar-to-grammar transposition) | **BLOCKED-candidate — refuted, ceded to LOCK-2 (provisional on XL-B)** |

**The two UNRUN load-bearing halves:** 3B rests on **G-3B-1, the whole-fold regen×check** (one slice ≠
the fold; the slice used a Constitution-forbidden duplicated home); 3A rests on the **E1-infer
non-vacuity** (proven on the 1:1 free edge, not on the edge that could be vacuous), which is
gate-cannot-fail until LOCK-1 — so the **runnable-today** sharpest 3A-side gap is instead the **E4
partial-function totality** (G-3A-c), which advances the contract's *totality* independent of LOCK-1 and
is the precondition for retiring the classifier into the fold (it entangles 3A and 3B). No family is
convergence-eligible (pass 2 of ≥3; each carries ≥1 UNRUN load-bearing gate; two consecutive clean
passes + a fresh non-author audit outstanding for all four).

---

## 7. CROSS-LOCK / GESTALT DIRECTIVES

1. **XL-1 — the `TypeAnalysis::from_ir` consumer contradiction (route to GESTALT-B as a
   named-supersession challenge with evidence).** CROSS-LOCK XL-F(1) is binding fact:
   *"TypeAnalysis::from_ir at backend/driver/analysis.rs is the real type consumer."* **All four fresh
   research reports (A §2, C §5, D §R-C) + SYNTHESIS re-verify:** `TypeAnalysis`/`from_ir` are **absent
   from live `skinny/crates`** (zero hits) — they exist only in **legacy `crates/core`** over the legacy
   `GrammarIR` (which has no `node_types`/`TypeDesc`). Two readings, both routable: **(a)** XL-F(1)
   describes the *greenfield tree-of-record* consumer (reconcilable under XL-A: skinny is the measurement
   substrate, designs target greenfield) — then it must be **labeled** as such, not a live-spine pointer;
   **(b)** it is a **stale pointer for the live spine** and XL-F(1) needs amendment. The live consuming
   edge on the measurement substrate is the `DirectSchemaSet` seam (`derive_materialization_plan` →
   `json_typed_direct::render`). Owed: an explicit named-supersession line (Constitution ix), not a quiet
   substitution.

2. **`Project.ty`@204 is single-owner; the thread is DISCHARGED, the CONSUME and the CONTRACT are not.**
   Assign the thread to **exactly one owner** (a GN-3 wave, `no-value-discard`); 3A verifies, 3B threads
   as `attrs.node_type`, 3C declares, 3D erases as S0 — the other three **CONSUME**. Two entangled
   obligations remain, **not** funded by funding the thread: **(i)** the CONSUME half — `.target` is a
   dead field; true CONSUME sits **inside G-3B-1's fold**; **(ii)** the consistency CONTRACT (3A) is a
   **separate born-RED obligation on LOCK-1** (name→`Type`). Thread = *presence*; contract =
   *consistency*. Entangled with XL-1: *what* consumes the value is the XL-1 dispute.

3. **Keep the decision-driver seat unmerged from LOCK-2 (XL-B, WORKING → provisional).** LOCK-3 owns
   *where* the driver sits (the fact contract at the L1→L2 edge); LOCK-2 owns *what engine*. 3A/3B
   respect this; 3D's per-site staging-form scheduling **stays LOCK-2's seat, gated on a real consumer**.
   The cessions to LOCK-2 (3C-C2, 3D staging-form) carry **"(provisional on XL-B)"** — not final until
   GESTALT-B.

4. **`no-ts-ir` holds across 3C/3D** — the meta-grammar emits TS *type declarations*, never a runtime TS
   IR; the backend-agnostic middle is **tape + schema** (PROVE-14), not a shared instruction IR.

5. **XL-B strengthened for GESTALT-B (evidence, non-blocking).** The 3A oracle on live IR + the 3B fold
   slice byte-identical strengthen LOCK-3's claim to the middle-IR tower's shape and the decision-driver
   seat; carry to GESTALT-B when XL-B finalizes.

---

## 8. THE TWO SHARPEST UNCERTAINTIES → PROTOTYPE BRIEFS (pass-3)

The two whose resolution most advances the next pass, both in the advancing 3A/3B cluster (3C is blocked
on external owners; 3D is banked and its runnable movers re-confirm known ground). They are **entangled
and ordered**: Brief #2 (E4 totality) is the precondition for Brief #1 (the fold) — a partial classifier
cannot be folded byte-identically *and* made total simultaneously without first knowing the
partial-function boundary. Full self-contained text is carried in the StructuredOutput return.

### Brief #1 — 3B G-3B-1: the whole-fold byte-identity gate (bbnf-worktree)
Promote `MaterializationPlan` to a durable `Attributes` bundle, write the single
`emit(&GrammarIr,&Attributes)` fold replacing `materialize_rule` + `emit_sink_only`'s `has_rule("value")`
gate, re-base `dispatch.rs` (318) + `view.rs` (431) onto `GrammarIr`; **PASS iff `regen×check ×
{json,css,sheets,real-typed}` is `git diff`-empty across the four generated dirs, and the classifier
deletion nets ≤ the substrate addition on `lint-loc`.** Run it or demote 3B to "structurally-motivated,
unproven."

### Brief #2 — 3A/3B G-3A-c: the E4 silent-partial-function / total-role contract (bbnf-worktree)
Exhibit ≥1 real fleet grammar rule the role heuristic misses → `materialize_rule` returns `body` with
**no tape-op and no error** (a live `no-silent-epsilon` breach); then a `NoRole`-total predicate flips
RED on exactly that rule, GREEN on all currently-classified rules. Advances both 3A (the totality
contract deliverable) and 3B (you cannot retire a partial classifier into a fold until it is total), and
runs fully on live TODAY — independent of the LOCK-1 born-RED edge that gates the *other* 3A half.
