# LOCK-3 — THE IR TRANSPOSITION SPINE · pass-1 FAMILY-SPECS

*Synthesis (Fable-tier), pass 1. One cogent specification per family while routes remain
incompatible, then the incompatibility map. Elegance earns nothing; a family stalled on a
missing primitive of equal difficulty to the original problem is marked BLOCKED-candidate.*

---

## 0. The settled baseline (all four researchers agree, path:line verified this pass)

The spine is **not hypothetical**. At HEAD it is a four-edge transposition chain:

```
.bbnf ──parse──▶ GrammarIr ──compile/lower_expr──▶ BackendIr ──sink_only──▶ SinkOnlyProgram ──lower_to_rust/emit──▶ .rs text
        (ir:33)   (arena)      (passes:1318)  (ir:352)  (lower/sink_only)              (lower/rust:32 + codegen)
```

Verified anchors: `GrammarIr` `ir/src/lib.rs:33`; `nullability` Changed-flag fixpoint `ir/src/lib.rs:320`;
`BackendIr` `ir/src/lib.rs:352`, `BackendShape` (5 variants) `:361`, `BackendExpr` `:376`;
`single_plan` `passes/src/lib.rs:1283`, `lower_expr` (10-arm `ExprKind→BackendExpr` copy) `:1318`,
`materialize_rule` `:1362`; the load-bearing defect **`ExprKind::Project` drops its type** —
`infer_expr(Project) => self.infer_expr(*body)` at `passes/src/lib.rs:203` (confirmed verbatim this pass).

Two facts bind every family below:
1. **The tower is 3B-shaped today** — one rich `BackendIr`, `BackendShape` an attribute, fewest
   transpositions. Whatever LOCK-3 chooses either *hardens* this shape (3A/3B) or *re-founds* it
   (3C/3D), and every divergence from the GN codex must be a **named supersession**, never silent
   (Constitution ix).
2. **The `->` thread is broken and the schema is starved** — `DirectBuildTarget` (`ir/src/lib.rs:483`)
   is a real type lattice, but `.target` is `None` on every offset-tape descriptor and the sole schema
   producer is the 1014-LOC hand file `xtask/src/real_typed_schema.rs`. Closing this seam (the
   typed-materialization-invariant) is the one obligation shared by 3A, 3B, and 3D; it is 3C's *product*.

---

## FAMILY 3A — CONTRACT-TOWER

**Architecture.** A small fixed tower whose *number of levels is measured, not decreed*, where each
level owns a triad of organs: (a) a total well-formedness verifier `verify(&self) -> Result<(),
Vec<ContractViolation>>` run after every pass (the GHC `-dcore-lint` / MLIR `--verify-each` /
rustc `MirPhase`-ladder discipline); (b) a round-trip/idempotence oracle (L0 `parse∘pretty=id` at
`ir/src/lib.rs:124`; the tower's tail is regen byte-identity, `xtask/src/regen.rs:37`); (c) a
**per-run refinement oracle** — translation validation (Pnueli/Necula) over the corpus, a `Digest`
fold per edge that attributes a mis-lowering to the edge that introduced it, generalizing the
PROVE-12 XOR-checksum and the `json_parity` track digests.

**Mechanism.** `ContractViolation` is a typed enum (never `Err(String)` — R4), colocated per level:
`crates/ir/src/verify/{grammar_hir,backend_ir}.rs`, `crates/codegen/src/verify/{lowered_rust,refine}.rs`,
consumed by `crates/xtask/src/gate/verify_each.rs` (the Y.13 consumer). The **flagship contract** is
the typed-materialization-invariant expressed as an L1 predicate: *for every `ExprKind::Project` in
L0, the L1 value carries a populated `type_ref`*. It turns **RED on HEAD** (because `passes:203`
drops the type) while `cargo build` stays green — the non-vacuity proof. Totality is enforced two
ways: the shape maps are exhaustive `match` (compiler-checked, no-silent-epsilon), and the four
`extract` `panic!`s (`passes/src/lib.rs:1783,1817,1862,1864`) become either upstream-discharged or
typed violations, so no `panic!` is reachable from a well-formed input.

**Integration seams.** Fold existing ad-hoc checks: `resolve_refs`/`validate`/`nullability`
(`ir/src/lib.rs:84,110,300,320`) → L0 verifier; the W7 fail-closed block (`lower/rust.rs:32-170`,
already checks shape==cost.chosen, CSP sat, budget pass) → L2 verifier, **retyped from String**;
`regen.rs:37` → L2 idempotence; PROVE-12 checksum + `json_parity.rs:19,202` → refinement instances.
Residualization is positioned as a *validated rewrite at the emission edge*, **not** a tower level
(3A's explicit refusal of 3D's staging plane — the parity oracle already discharges it).

**Gates it carries.** (G1) flagship L1 contract RED on HEAD, GREEN after the `->` thread lands;
(G2) emitted `.rs` byte-identical with verifiers on/off (zero runtime footprint, Constitution v);
(G3) an injected mis-lowering (corrupted `delim_mask`) is caught **and localized to its edge** by the
refinement oracle; (G4) verifier LOC + regen-time delta bounded.

**Open questions.** (Q1, sharpest) **Is the level count three or four?** R3 flags the honest chain
has four edges; `SinkOnlyProgram` is a real level with no verifier while `BackendIr`↔`LoweredRust`
may be ceremony. The count must be *earned by an edge enumeration*, not asserted. (Q2) Does the
contract discipline *require distinct typed value levels*, or can a per-pass `verify(arena, attrs)`
predicate on a **single** IR (the 3B substrate) localize the same defects? — this is the live
incompatibility with 3B (§Incompat). (Q3) The refinement oracle is corpus-bounded (R2): constructs
with zero corpus witnesses are *unverified*, and coverage must be a first-class metric.

**Disposition: ADVANCED (as a discipline), CONTESTED (as a standalone tower).** Nothing is
missing — every organ has an extant fragment; the flagship contract is buildable now and provably
non-vacuous. But 3A's *value* is likely as a contract layer over whatever substrate wins, not as an
independent multi-level tower; its standalone form directly contests 3B and the codex's 3B-lean
commitment. Not blocked.

---

## FAMILY 3B — SINGLE-IR-MULTIPASS

**Architecture.** One rich IR — the `GrammarIr` arena treated as an LLVM `Module`. `BackendIr`,
`BackendExpr`, `BackendRule` are **deleted**. BackendShape, materialization, recognizers, cost, types
become **attributes** keyed by `ExprId`/`RuleId` in one formalized `Attributes` store
(`crates/ir/src/attr.rs`, = `LayoutFacts` at `passes/src/lib.rs:91-100` promoted to a first-class
type: dense `Vec<T>` where analysis is total — the `nullability` `Vec<bool>` archetype — sparse
`HashMap` where partial). Emission becomes one read-only fold `emit(&GrammarIr, &Attributes) ->
String`. Three IR types → two; two whole-tree transpositions → one (parse); two node enums → one.

**Mechanism (two strata, Y.13-honest).** **Stratum 1 (lands now):** delete
`single_plan`/`lower_expr`/`materialize_rule` (`passes:1283,1318,1362`) and the second enums
(`ir:352-413`); move `span_kind`, `is_entry`, and the materialization descriptor into `Attributes`;
rewrite the emitter to read the store (generalize `LowerCtx` at `codegen/src/lower/rust.rs:24`, which
*already* reads `backend_shape`/`cost_facts` as `HashMap<RuleId,_>`). The pipeline today has **zero
in-place mutation**, so LLVM `PreservedAnalyses` machinery would be consumer-less — **forbidden now**.
**Stratum 2 (lands with its first Transform):** the `Analysis`/`Transform` trait split + `PassManager`
+ invalidation arrive *with* their first mutating consumer (Discard-desugar as append-only rewrite, or
— interlock — LOCK-2's e-graph island as a Transform over the one IR). Never speculatively.

**Integration seams.** Every "lowering" today is either an analysis already producing a keyed side
table or a 1:1 rename — the redundancy is structural and visible in the maintenance tax (adding `->`
touched 5 lockstep sites). `derive_backend_shape` (`passes:398`) → Analysis writing
`attrs.backend_shape`; GN-2 recognizer lowering → `attrs.recognizer[expr]`; GN-3 `Project.ty` →
`attrs.node_type`/`DirectBuildTarget` (an Analysis, **no BackendExpr to touch** — this is where 3B
*closes* the coarse-`TapeEmit{kind}` collapse MLIR warns of).

**Gates it carries.** (G1, killer) delete the second IR, re-express as the `(GrammarIr, Attributes)`
fold, and `cargo xtask regen-json && check-json && regen-css && check-css-l4-declaration-values`
**all byte-identical** — if the attribute-reading emitter reproduces every live `generated.rs`
exactly, the second IR was pure redundancy; a missed attribute or emit-order divergence breaks the
diff (falsifiable). (G2) net LOC drops (~250-350 deleted vs `attr.rs`+fold added). (G3) the `->`
extensibility count falls 5→2. (G4) Stratum-2 invalidation smoke test **only if a mutator exists**.

**Open questions.** (Q1) Does byte-identity actually hold, or does the second IR carry a real
emit-order/derived fact the fold cannot reproduce? (Q2) Is materialization ever needed per-`ExprId`
rather than per-`RuleId`? (no fleet grammar needs it today — must confirm before committing the coarse
key). (Q3) 3A's rebuttal: does deleting the typed level forfeit defect-localization? 3B's answer is a
`verify(grammar, attrs)` pass, which is *exactly* 3A-discipline-on-3B-substrate — the hybrid.

**Disposition: ADVANCED, strongest structural claim.** Nothing is missing; the collapse is
executable today behind a byte-identity gate. No cross-lock primitive required for Stratum 1. Not
blocked.

---

## FAMILY 3C — GRAMMAR-HOMOIOUSIA

**Architecture.** The IR is of *like substance* to the grammars it lowers: its node algebra is
bbnf-specified in a meta-grammar (`grammar/ir/*.bbnf`), its structural transpositions are
grammar-to-grammar maps (the nanopass `define-language`/`define-pass` shape), and the same
direct-to-struct emitter that produces JSON/CSS parsers regenerates `ir/src/lib.rs`. **Orthogonal to
level count** (it can be a tower or a single graph) and to staging; its axis is the IR's *substance*
— grammar-derived vs hand-written Rust. Payoffs: bootstrap alignment (LOCK-7), rust/ts skeleton
isomorphism by construction (LOCK-6 6B), dogfooding the type plane on the compiler's own middle.

**Mechanism.** Three artifacts, each extending extant machinery: (1a) `grammar/ir/grammar-ir.bbnf`
declaring nodes as sums-of-products via `->` projections (bbnf is already self-hosted:
`grammar/bbnf/bbnf.bbnf`, 89 lines, sums-via-alt + `-> Span` + `@import`); (1b) point the working
direct-to-struct emitter (`codegen/src/json_typed_direct.rs`, `render`, 1245 LOC, consuming
`DirectSchemaSet` from `direct_schema.rs`) at the IR grammar's derived schema, so `ir/src/lib.rs`
gains `// @generated`; (1c) transpositions as declarative rewrite clauses `src_ctor => tgt_ctor` with
the catamorphism fringe *derived* (SYB/uniplate on the generated enums), only the semantic clauses
(`Regex→RegexProgram{span_kind}`, `Project→ValueProject`) hand-authored. The heuristic role
classifier `derive_materialization_roles` (`passes:1403`, byte-literal pattern-matching that invents
the `JsonValue` taxonomy) and the `sink_only.has_rule("value")` name-gate are the anti-patterns 3C
dissolves — the object grammar *declares* its shapes.

**Integration seams.** `direct_schema.rs` (schema IR), `json_typed_direct.rs:10` (emitter),
`schema_direct.rs:11` (bridge), the `ShapeLowering` trait + `select_lowering` in `lower/mod.rs`
(lowering is *already* pluggable dispatch — the pass-registry seam), `regen.rs:19` (bootstrap host,
gains an `ir.bbnf` target + fixpoint gate).

**Gates it carries.** (G1) `ir.bbnf` + emitter regenerates the **product-only** nodes (`Rule`, `Expr`,
`SourceSpan`, `DirectBuild*` records) byte-identical (modulo ordering) to the hand-written structs.
(G2) one declarative transposition clause (`Literal→ByteLiteral`+`Ref→CallRule`) + derived fold
reproduces `lower_expr`'s output byte-identically on the JSON grammar. (G3) `regen∘regen` byte-
identical (7A fixpoint). (G4) generated-LOC on `ir/src/lib.rs` ≤ hand LOC (generated-size-budget).

**Open questions / the walls.** (W1, **hard**) The node algebra is fundamentally a **sum** —
`ExprKind` (10 variants), `BackendExpr` (13). The schema IR is **Struct-only today**
(`DirectTypeKind` at `direct_schema.rs:52`); emitting the sum requires `DirectTypeKind::Enum`, a
**GN-3 W1 / LOCK-1 deliverable 3C cannot itself supply**. Its own prototype is scoped to product-only
nodes precisely to dodge this. (W2) The IR grammar must be **functor-compatible with LOCK-5's ONE-HIR
/ LAWFUL-FUNCTOR verdict**, which is *undecided* — 3C must consume that verdict, not define it
(Constitution ii; 3C-here and 5A-there are the same claim from two seats). (W3) **No GN-wave consumes
an IR grammar today** — absent a named same-wave consumer it is consumer-less substrate (Y.13/viii).
(W4) cost/analysis facts (`ir/cost.rs`, HashMaps + CSP/e-graph state) are *not* clean sums-of-products
and must stay hand-written primitives — 3C's lawful scope is the node algebra + structural maps, not
the cost substrate (over-reach = god-modularizing the meta-grammar). (W5) `no-ts-ir`: the meta-grammar
emits TS *type declarations*, never a TS runtime IR.

**Disposition: BLOCKED-candidate.** 3C's flagship (emit the node *algebra*) is stalled on three
externally-owned primitives it cannot resolve within LOCK-3: `DirectTypeKind::Enum` (LOCK-1/GN-3 W1),
LOCK-5's undecided HIR-isomorphism verdict, and the absence of any GN-wave that consumes an IR
grammar. The product-only demonstration (G1) is real and worth running as a *scoping probe*, but it
deliberately excludes the sum — i.e. the part that *is* the family. None of the three blockers is
intractable (each is a bounded, named deliverable), so 3C is BLOCKED-candidate, not RETIRED: it
converts to ADVANCED the moment LOCK-1 lands sums and LOCK-5 rules on the functor. Its unique gift —
the sternest possible consumer for the type plane and the bootstrap gate — is why it should be banked,
not dropped.

---

## FAMILY 3D — STAGED-RESIDUAL

**Architecture.** The IR is an explicitly two-stage program. Every node carries a binding time —
`Stage = { Comptime(S0), Runtime(S1) }`, `S0 ⊑ S1`. The generator IS a partial evaluator (the first
Futamura projection made concrete); **residualization is the erasure of stage 0**. This is *not* a new
tower level (the sharpest intra-family discipline, R5): `Stage` is an LLVM-shape **attribute** on the
existing IR and erasure happens inside the existing `lower_to_rust` — an annotating pass + a split
leaf emitter, nothing structural. The thesis: *the comptime/runtime split lives in the types, and
stage-0-costs-zero subsumes type-plane-costs-zero* (LMS's "only types distinguish the two stages",
Kovács's 2LTT erasure metatheorem).

**Mechanism.** `fn binding_times(grammar) -> Vec<Stage>` beside `nullability` — the *identical*
monotone Changed-flag fixpoint (`ir/src/lib.rs:320`), one `Vec<Stage>` parallel to the `Vec<bool>`.
Seeds: `Literal`/`Regex`/`Project.ty`/`Annotation` = S0 (grammar constants); the input cursor = S1
(the only genuine runtime seed). Everything joins. Result: S1 recognizer spines with **S0 islands
hanging off them** (delimiter sets, alphabets, first-sets, DFA tables, capacity hints, field types).
The emitter splits: `emit_const(node) -> Residual` (evaluates an S0 subtree to a `u128` mask / `[u8;N]`
alphabet / DFA table / `usize` cap / `DirectTypeRef` and splices it as `const`/`const fn`/const-generic
arg); `emit_runtime(node)` emits the S1 spine reading the S0 constants by name. `Residual` is the
generation-time analogue of the runtime tape value plane, *consumed in the same pass that produces it*
(Y.13-clean).

**The load-bearing claim (PROVE-12 as corollary).** PROVE-12 hand-wrote exactly one `emit_const`: the
delimiter set `b";{}"` folded to a `u128`, `find_component_delim<const DELIMS: u128>` reading it as a
const generic, the per-byte `delimiters.contains` interpretive dispatch (an S0 value stuck at S1 —
*precisely the binding-time error BTA forbids*) erased. 3D claims a stage-annotated lowering emits
this **automatically** from the annotated IR, no hand template edit. The reference implementation to
reproduce byte-identically is `restart/skinny/tranches/sk-v24/prototypes/PROVE-12/patch.diff`; its
parity checksum (`track1_full=1`, `track1_rich=17839`, mask-fold `MATCH`) is the pass/fail line.
**GN-5.W4's explicit ask** ("12's const-mask generalized to every grammar-derived stop-set") *is* this
family.

**The strong unification.** GN-3's ZERO-RUNTIME-COST type invariant is *not a separate law* — a
projected type is an S0 value; `DirectBuildTarget.type_ref` residualizes to a Rust *type*, which has
no runtime representation by construction. **Type-plane erasure is the special case of stage-0 erasure
applied to the type sub-plane.** LOCK-1 types the S1 values; LOCK-3 erases the S0 program that computed
the S1 program — one erasure from two angles. This is the strongest cross-family synthesis in the
portfolio (prior-art §6.3): 3D subsumes GN-3 and PROVE-12 as two erasures of one stage.

**Integration seams.** `binding_times` beside `nullability` (`ir:320`); `Residual` enum colocated with
`DirectBuildTypeRef`; `LowerCtx` gains `stage_map: Vec<Stage>` (`codegen/src/lib.rs:177`); the BTA
pass slot in `passes::compile` after infer/shape (types resolved → `Project.ty` known S0) and before
`lower_to_rust`, *reusing* the alphabets `derive_recognizers` (`passes:338`) already computes;
`StructuralAlphabet` (`ir:435`) is already an S0 residual, merely *named* as such; richest new targets
beyond PROVE-12 are `codegen/src/lower/css_scan.rs` and the JSON first-byte dispatch. Per-site staging
form (monomorph vs shared `const` table) is a `DecisionCspFacts`/`PriorityStep` decision handed to
**LOCK-2** (`ir/cost.rs:203,264`) — 3D owns the staging *semantics*, LOCK-2 owns the *schedule*.

**Gates it carries.** (P1, load-bearing) `binding_times` + residualizer emit
`find_component_delim<const DELIMS>` from the annotated CSS IR alone; `regen-css` output **byte-
identical to PROVE-12's patch**, checksum MATCH — until this is green, 3D is SPEC-only. (P2)
generalize to a NEW site (JSON dispatch), measured **cold-paired PMU** (PROVE-09's two-arm-window
self-deception is fatal here — 3D is the family most prone to unmeasured plausibility), floor
−3.7…−6.4% ins/B. (P3) per-site staging is a `DecisionCspFacts` output, not a hardcoded emitter
branch. (P4) a **residual-count oracle** emitted at regen (guards the silent BTA-over-approximation
failure: a static node marked S1 loses the win with no error) + `Vec<Stage>` content-hash stable.

**Open questions.** (Q1) Does the delimiter set cleanly present as an S0 island the BTA marks without
binding-time-improvement hand-help, and can the const-generic seam be emitted *structurally*? — P1
answers this. (Q2) Monomorph explosion vs the generated-size budget: the law must *offer*
monomorphization as a LOCK-2 candidate, never force it. (Q3) Capacity hints (`@cap(875)`) are *corpus*
facts not grammar facts — staging them S0 is a `StageError::Leak` unless the grammar declares them
(no-silent-epsilon). (Q4) LOCK-2 owns per-site scheduling — but 3D's *kernel* (blanket residualize one
island) runs without LOCK-2, so this is a refinement dependency, not a blocker.

**Disposition: ADVANCED, most-evidenced synthesis vein.** PROVE-12 is a landed, born-verified
instance; the kernel is implementable within LOCK-3 without LOCK-2 (only the monomorph-vs-table
*choice* defers). Not blocked. The one risk is measurement discipline, not a missing primitive.

---

## THE INCOMPATIBILITY MAP

The four families are **not four mutually-exclusive spines**. Exactly *one* axis carries a true
incompatibility; everything else composes. Precisely:

### The ONE hard incompatibility — the level-count axis

**3A-as-tower ⊗ 3B-as-single-IR.** They make opposite claims on the primary axis LOCK-3 owns: *how
many IR representations exist.* 3A (as chartered) wants a small fixed tower of **distinct typed value
levels**, each with a verifier at its value boundary — which requires each level to *exist as a
distinct type*. 3B wants **one** IR, deleting `BackendIr`/`BackendExpr`, with facts as attributes.
A verifier that reads a distinct typed level cannot read a level that has been deleted; they cannot
both be the spine's shape. This is the genuine, load-bearing incompatibility of LOCK-3.

**But it dissolves under a named hybrid.** If 3A's contract is re-read as a **per-pass predicate
`verify(arena, attrs)` over the single IR** (which research-B R5 offers and research-A Q2 concedes),
the discipline survives without distinct typed levels. The prior-art synthesis names this hybrid —
*3A-discipline layered on the 3B-substrate* — as the strongest available: keep one IR, give each pass
a nanopass-style checkable output contract. So the incompatibility is **real as-chartered, resolved by
a hybrid**, and the resolving question — *does per-pass contract-on-one-IR localize the defects a
typed-level tower would?* — is the sharpest uncertainty in the lock (Prototype 1).

### The compatible layerings (these coexist; do NOT treat as competitors)

- **3B ⊕ 3D** — fully compatible. 3D is *architecturally a 3B move*: `Stage` is one more attribute
  slot on the single IR; the residualizer is a split leaf emitter, not a transposition. 3D cites the
  single-IR annotating-pass shape as its own discipline (R5). The winning substrate is one IR whose
  attribute store includes `stage`.
- **3B ⊕ 3C** — compatible, orthogonal axes (count vs substance). 3C emits whatever node algebra
  survives 3B's collapse; if 3B deletes `BackendExpr`, 3C simply has fewer enums to generate.
- **3A ⊕ 3C** — deeply compatible: **nanopass is 3A+3C fused** (grammar-specified IR *with*
  auto-generated well-formedness contracts). They share the same prior art (Keep/Dybvig).
- **3A ⊕ 3D** — compatible: 3D's residual-count oracle *is* a 3A-style refinement oracle; the stage
  attribute can carry a contract.
- **3C ⊕ 3D** — compatible in principle (a grammar-specified, stage-annotated IR), though both defer
  on different externals.

### The tensions (compatible, but must agree at a seam)

- **3D vs 3C on who owns the type erasure.** Both claim GN-3's type plane and the same seam
  (`passes/src/lib.rs:203`). 3D *erases* the projected type as an S0 value; 3C *emits its declaration*
  as a grammar-derived schema. Different operations at different points — compatible, but they must
  agree that `Project.ty` is threaded (not dropped) exactly once. Not an incompatibility; a
  coordination obligation for the gestalt.
- **3A refuses 3D's staging-as-level, and 3D agrees.** 3A explicitly refuses to make residualization a
  tower level (the parity oracle discharges it); 3D explicitly refuses to make `Stage` a tower level
  (it is an attribute). They *converge* on "residualization is not a level" — a shared verdict, not a
  conflict.

### The net shape for the gestalt

The likely convergent spine is **one IR (3B substrate) + per-pass contracts (3A discipline) + a stage
attribute with residualization (3D erasure)** — three compatible layers on one axis-resolved base —
with **3C banked as BLOCKED-candidate** until LOCK-1 (sums), LOCK-5 (HIR functor), and a named
consuming wave unblock it. The single decision that gates this whole picture is the level-count
adjudication (3A-tower vs 3B-single), because it determines whether the contracts hang on typed levels
or on passes over one IR.

---

## DISPOSITIONS (for the registry)

| Family | Disposition | Missing primitive? | One-line reason |
|---|---|---|---|
| **3A** | ADVANCED (as discipline) / CONTESTED (as tower) | none | Every organ has an extant fragment; flagship contract buildable + provably non-vacuous; but its value is as a layer, and its tower form contests 3B. |
| **3B** | ADVANCED (strongest structural) | none | The second IR is deletable today behind a byte-identity gate; no cross-lock primitive for Stratum 1. |
| **3C** | **BLOCKED-candidate** | `DirectTypeKind::Enum` (LOCK-1/GN-3 W1) + LOCK-5 HIR-functor verdict + a consuming GN-wave | Its flagship (sum node algebra) needs three externally-owned primitives; product-only demo excludes the very part that is the family. Bounded, not intractable → bank, don't retire. |
| **3D** | ADVANCED (most-evidenced synthesis) | none for the kernel (LOCK-2 only for per-site scheduling) | PROVE-12 is landed born-verified; kernel runs without LOCK-2; subsumes type + constant erasure under one law. Risk is measurement discipline, not a primitive. |

---

## THE TWO SHARPEST UNCERTAINTIES (next-pass prototype briefs)

Returned in full via StructuredOutput. In brief:

1. **THE LEVEL-COUNT ADJUDICATION (3A ⊗ 3B).** Collapse to one IR behind byte-identity, *and* wire the
   flagship contract as a per-pass predicate on that one IR — does it localize the defect without a
   second typed level? Resolves the lock's only hard incompatibility.
2. **PROVE-12 AS COROLLARY (3D).** Can `binding_times` + the split emitter reproduce PROVE-12's
   const-generic residual byte-identically from the annotated IR alone? Resolves whether 3D graduates
   from SPEC-only and whether the type plane is subsumed by stage-0 erasure.
