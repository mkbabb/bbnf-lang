# LOCK-3 — THE IR TRANSPOSITION SPINE · pass-1 FAMILY-SPECS

*Synthesis (Fable-tier), pass 1. One cogent specification per family while routes remain
incompatible, then the incompatibility map. Elegance earns nothing on its own; a family stalled on
a missing primitive of equal difficulty to the original problem is marked BLOCKED-candidate. This
supersedes the 03:48 draft, re-cut against the current research-{A,B,C,D,priorart} sweep.*

---

## 0. The settled baseline (all five reports agree, path:line verified this pass)

The spine is **not hypothetical**. At HEAD it is a four-edge transposition chain:

```
.bbnf ──parse──▶ GrammarIr ──compile/lower_expr──▶ BackendIr ──sink_only──▶ SinkOnlyProgram ──lower_to_rust+emit──▶ .rs text
        (ir:33)   (arena)      (passes:1318)  (ir:352)  (lower/sink_only)                (lower/rust + codegen)
```

Verified anchors: `GrammarIr` `ir/src/lib.rs:33`; `nullability` Changed-flag fixpoint `ir/src/lib.rs:320`;
`BackendIr` `ir/src/lib.rs:352`, `BackendShape` (5 variants: EagerTape/OffsetTape/EventTape/SinkOnly/
CollapsedStage) `:361`, `BackendExpr` `:376`; `lower_expr` (10-arm `ExprKind→BackendExpr` copy)
`passes/src/lib.rs:1318`, `materialize_rule` `:1362`; the load-bearing defect **`ExprKind::Project`
drops its type** — `ExprKind::Project { body, .. } => self.infer_expr(*body)?` at
`passes/src/lib.rs:204` (confirmed verbatim this pass). xtask surface: `regen-json`/`check-json`,
`regen-css`/`check-css-l4-declaration-values`, `regen-sheets`/`check-sheets`, `lint-loc`
(`xtask/src/main.rs:40-49`).

**Three facts bind every family below.**
1. **The tower is 3B-shaped today** — one rich `BackendIr`, `BackendShape` an attribute, fewest
   transpositions. LOCK-3 either *hardens* this shape (3A/3B) or *re-founds* it (3C/3D); every
   divergence from the GN codex must be a **named supersession**, never silent (Constitution ix).
2. **The `->` thread is broken and the schema is starved** — `DirectBuildTarget` (`ir/src/lib.rs:483`)
   is a real type lattice, but `.target` is `None` on every offset-tape descriptor and the sole schema
   producer is the 1014-LOC hand file `xtask/src/real_typed_schema.rs`. Closing this seam
   (typed-materialization-invariant) is the obligation shared by 3A/3B/3D; it is 3C's *product*.
3. **PROVE-12 is a landed, born-verified residualizer** — the CSS `find_component_delim` delimiter
   set const-folded to a `u128`, byte-identical output, cyc/B 0.81-0.98×, +18 LOC
   (`prototypes/PROVE-12/`). It is the concrete over-baseline win 3C and 3D both claim to generalize.

**Three axes organize the family space and pre-determine the incompatibility map:**
- **Axis M — representation multiplicity.** 3A = distinct typed IR *values* per level; 3B = one arena
  + side-tables; 3C-as-authored = distinct IR *grammars* per level; 3D = agnostic (a stage attribute).
- **Axis S — IR substance.** Hand-written Rust enums (3A/3B/3D) vs bbnf-specified-and-generated node
  languages + grammar-to-grammar transpositions (3C).
- **Axis R — residualization ownership.** Who generalizes PROVE-12: a validated rewrite at the emit
  edge (3A), the emitter reads a grammar-constant attribute (3B), BTA *forces* the const as erasure of
  Stage-0 (3D), or one directed rewrite rule with a `@const` marker fires N grammars (3C).

M is where 3A and 3B collide; S isolates 3C; R is where 3C and 3D contest one seat.

---

## FAMILY 3A — CONTRACT-TOWER

**Architecture.** A small fixed tower — L1 HIR (`GrammarIr`), L2 MIR (`BackendIr` + `LayoutFacts`
side-table), L3 LIR (`SinkOnlyProgram` → text) — whose *level count is earned by an edge enumeration,
not decreed*. Each level owns a triad: (a) a total well-formedness verifier `verify(&self) ->
Result<(), Vec<ContractViolation>>` run after every pass (GHC `-dcore-lint` / MLIR `--verify-each` /
rustc `-Zvalidate-mir` / Cranelift CLIF verifier discipline); (b) a round-trip/idempotence oracle
(L1 `parse∘pretty=id` `ir/src/lib.rs:124`; the tail is regen byte-identity); (c) a **per-run
refinement oracle** — translation validation (Pnueli/Necula) over the corpus via a naive HIR
tree-walking reference interpreter whose value/event stream must equal the compiled parser's,
generalizing the PROVE-12 XOR-checksum and the json track digests.

**Mechanism.** `ContractViolation` is a typed enum (never `Err(String)`), colocated per level:
`crates/ir/src/verify/` + `crates/codegen/src/verify/`, consumed by `xtask verify-tower` (the Y.13
consumer). The **flagship contract** is the typed-materialization-invariant as an L2 legalization
predicate: *MIR contains no `Project`; every projected field's `DirectBuildTarget.target` is `Some`.*
It turns **RED on HEAD** (because `passes:204` drops the type) while `cargo build` stays green — the
non-vacuity proof. The W7 fail-closed checks now living **inside** lowering
(`codegen/src/lib.rs:114,684,708`) are L2 predicates checked one level too late; 3A hoists them into
`verify(MIR)` so **T3 becomes total** (no fail-closed at emit; no-silent-epsilon is the runtime
backstop, made *unreachable* by the L2 contract). Multi-backend cut is **T3**: L2 is the
backend-agnostic middle; each backend supplies its own `T3` + L3 contract + parity oracle, the MIR
verifier the shared contract "add a backend" must satisfy.

**Integration seams.** Fold ad-hoc checks: `resolve_refs`/`validate`/`nullability`
(`ir:84,110,300,320`) → L1; the W7 block (`codegen/src/lib.rs:114,684,708`, retyped from String) → L2;
regen byte-identity → L2/L3 idempotence; PROVE-12 checksum + json parity → refinement instances. New
substrate: `crates/bbnf-interp/` (the reference interpreter — general-infra home, not stuffed into
`passes`). Residualization is a *validated rewrite at the emit edge*, **not** a tower level (3A's
explicit refusal of 3D's staging plane — the parity oracle discharges it).

**Gates it carries (born-RED).** (G1) flagship L2 contract RED on HEAD, GREEN after the `->` thread
lands. (G2) emitted `.rs` byte-identical with verifiers on/off (Constitution v). (G3) an injected
mis-lowering (corrupted `delim_mask`) is caught **and localized to its edge** by the refinement
oracle. (G4) verifier LOC + regen-time delta bounded; verifier/oracle output sorted by `RuleId` (regen
byte-identity must survive `HashMap` iteration nondeterminism).

**Open questions.** (Q1, sharpest) **Is the level count three or four?** The honest chain has four
edges; `SinkOnlyProgram` is a real level with no verifier while `BackendIr`↔`LoweredRust` may be
ceremony. The count must be earned, not asserted. (Q2) Does the contract discipline *require distinct
typed value levels*, or can a per-pass `verify(arena, attrs)` predicate on a **single** IR (the 3B
substrate) localize the same defects? — this is the live incompatibility with 3B. (Q3) The refinement
oracle is corpus-bounded: constructs with zero corpus witnesses are unverified; coverage must be a
first-class metric. (Q4) Who validates the validator? — a shared interpreter bug passes the oracle
falsely; mitigated by divergent-algorithm + three third-oracles, residual common-mode risk unquantified.

**Disposition: ADVANCED (as a discipline), CONTESTED (as a standalone tower).** Nothing is missing —
every organ has an extant fragment; the flagship contract is buildable now and provably non-vacuous.
But 3A's *value* is likely as a contract layer over whatever substrate wins, not as an independent
multi-level tower; its standalone form contests 3B and the codex's 3B-lean commitment. Not blocked.

---

## FAMILY 3B — SINGLE-IR-MULTIPASS

**Architecture.** One rich IR — the `GrammarIr` arena as an LLVM `Module`. `BackendIr`/`BackendExpr`/
`BackendRule` are **deleted**. BackendShape, materialization intent, recognizers, cost, and projected
types become **attributes** keyed by `ExprId`/`RuleId` in one `Lowering`/`Attributes` store
(`crates/ir/src/attr.rs`, = `LayoutFacts` at `passes/src/lib.rs:91` promoted to a first-class type:
dense `IndexVec<ExprId,T>` where analysis is total — the `nullability` `Vec<bool>` archetype — sparse
`SecondaryMap` where partial). `SourceSpan` survives untouched to emission (grammar-sourced diagnostics
free). Emission becomes one read-only fold `emit(&GrammarIr, &Attributes) -> String`. Three IR types →
two; two whole-tree transpositions → **one** (parse); two node enums → one. The founding observation:
`BackendExpr`'s grammatical half is a 1:1 rename of `ExprKind` that discards `ExprId`+span; its backend
half is emit-intent keyed to `RuleId` — `BackendIr` is *attributes wearing a tree's clothing*. This is
the only family under which the three standing laws hold **by construction** (no lossy rebuild to
discard into; no arm that can fabricate an empty node; no translation for parity to be lost through).

**Mechanism (two strata, Y.13-honest).** **Stratum 1 (lands now):** delete
`lower_expr`/`materialize_rule` (`passes:1318,1362`) and the second enums (`ir:352-413`); move
`span_kind`, `is_entry`, and the materialization descriptor into `Attributes`; rewrite the emitter to
read the store (generalize `LowerCtx` at `codegen/src/lower/rust.rs`, which *already* reads
`backend_shape`/`cost_facts` as `HashMap<RuleId,_>`). The pipeline today has **zero in-place
mutation**, so LLVM `PreservedAnalyses` machinery would be consumer-less — **forbidden now**.
**Stratum 2 (lands with its first Transform):** the `Analysis`/`Transform` split + `PassManager` +
invalidation arrive *with* their first mutating consumer (Discard-desugar as append-only rewrite, or —
interlock — LOCK-2's e-graph island as a Transform over the one IR; acyclicity holds by cutting
recursion at `RuleId` boundaries, the Cranelift aegraph model). Never speculatively. `ExprKind` **never
grows** (the god-enum is the named counterexample).

**Integration seams.** Every "lowering" is either an analysis already producing a keyed side table or a
1:1 rename — the redundancy is visible in the maintenance tax (adding `->` touched 5 lockstep sites).
`derive_backend_shape` (`passes:398`) → Analysis writing `attrs.backend_shape`; recognizer lowering →
`attrs.recognizer[expr]`; **`infer`'s `Project` arm (`:204`) → writes `attrs.node_type`/
`DirectBuildTarget`** (an Analysis, *no BackendExpr to touch* — this is where 3B *closes* the
coarse-`TapeEmit{kind}` collapse MLIR warns of, and the exact wire GN-3 needs).

**Gates it carries (born-RED).** (G1, killer) delete the second IR, re-express as the
`(GrammarIr, Attributes)` fold, and `cargo xtask regen-json && check-json && regen-css &&
check-css-l4-declaration-values && regen-sheets && check-sheets` **all byte-identical** — if the
attribute-reading emitter reproduces every live `generated.rs` exactly, the second IR was pure
redundancy; a missed attribute or emit-order divergence breaks the diff (falsifiable). (G2) net LOC
drops (~250-350 deleted vs `attr.rs`+fold added). (G3) `->` extensibility count falls 5→2. (G4) a
`Lowering::verify()` coverage assert (every `Project` `ExprId` has `node_type`; every materializable
`RuleId` has `build`) at each pass boundary — the price of one IR, a first-class deliverable.

**Open questions.** (Q1) Does byte-identity actually hold, or does the second IR carry a real
emit-order/derived fact the fold cannot reproduce? (Q2) Is materialization ever needed per-`ExprId`
rather than per-`RuleId`? (no fleet grammar needs it today — confirm before committing the coarse key).
(Q3) Attribute coverage has no *type-system* enforcement (Rust's exhaustive `match` covers node kinds,
not "did every `Project` write its attribute"); the coverage verifier is a runtime debug-assert — does
that re-import the vigilance 3B claims to abolish? (Q4) 3A's rebuttal: does deleting the typed level
forfeit defect-localization? — 3B's answer is `verify(grammar, attrs)`, which *is*
3A-discipline-on-3B-substrate, the hybrid.

**Disposition: ADVANCED, strongest structural claim.** Nothing is missing; the collapse is executable
today behind a byte-identity gate; no cross-lock primitive for Stratum 1. Not blocked.

---

## FAMILY 3C — GRAMMAR-HOMOIOUSIA

**Architecture.** The IR is of *like substance* to the grammars it lowers: its node algebra is
bbnf-specified in meta-grammars (`crates/ir/grammars/{hir,mid,backend}.bbnf`), its structural
transpositions are grammar-to-grammar rewrite maps (the nanopass `define-language`/`define-pass`
shape; ISLE proves such maps compile to efficient single-pass Rust, so *efficiency is not the
blocker*), and the same direct-to-struct emitter that produces JSON/CSS parsers regenerates
`ir/src/lib.rs`. **Orthogonal to level count in principle** (a tower or a single graph) and to staging;
its axis is the IR's *substance*. Payoffs: bootstrap alignment (LOCK-7), rust/ts skeleton isomorphism
by construction (LOCK-6 6B), dogfooding the type plane on the compiler's own middle.

**Mechanism.** (A) IR node grammars declaring nodes as sums-of-products via `->` projections (bbnf is
already self-hosted: `grammar/bbnf/bbnf.bbnf`, 89 lines, sums-via-alt + `-> Span` + `@import`), plus an
**attribute vocabulary** (`@derive`, `@box`, `@newtype`, `@serde`) a bare grammar cannot express. (B)
point the working direct-to-struct emitter (`codegen/src/json_typed_direct.rs`, `render`, 1245 LOC,
consuming `DirectSchemaSet` from `direct_schema.rs`) at the IR grammar's derived schema, so
`ir/src/lib.rs` gains `// @generated`. (C) transpositions as declarative rewrite clauses
`src_ctor => tgt_ctor` with the catamorphism fringe *derived* (nanopass identity map), only the
semantic clauses (`Regex→RegexProgram{span_kind}`, `Project→ValueProject`) hand-authored, non-structural
kernels (HM `unify`, cost search, first-set fixpoint) escaping to typed `@host` functions.
Residualization (Axis R): PROVE-12 becomes ONE rewrite rule over `backend.bbnf`
(`residualize: find_delim{set:@const s} -> find_delim_masked{mask: delim_mask(s)}`) firing N grammars.

**Integration seams.** `direct_schema.rs`, `json_typed_direct.rs:10`, `schema_direct.rs:11`, the
`ShapeLowering`/`select_lowering` seam in `lower/mod.rs` (lowering is *already* pluggable dispatch),
`regen.rs` (bootstrap host, gains an `ir.bbnf` target + `regen∘regen` fixpoint gate). **Named
supersessions**: GN-1's raw-string template home dissolves into a `backend.bbnf` unparse grammar;
GN-3's `type_plane::derive` generalizes from user-document to universal schema producer. Both
subtractive.

**Open questions / the walls.** (W1, **the make-or-break, UNMEASURED**) The **structural-fraction** —
the proportion of each pass expressible as identity+structural-rewrite vs escaped to `@host`. Target
≥70%; if analyses dominate, "grammar-specified" is a cosmetic wrapper over hand-written passes and 3C
collapses to a sibling. *This number is the single fact pass 1 must hand pass 2*, and it is measurable
cheaply as a spec-probe (classify the arms of `lower_expr`/`materialize_rule`/`infer`/`derive_*`
without building the interpreter). (W2, **missing primitive**) The node algebra is a **sum** —
`ExprKind` (10 variants), `BackendExpr` (13). The schema IR is **Struct-only today**
(`DirectTypeKind` at `direct_schema.rs:52`); emitting the sum requires `DirectTypeKind::Enum`, a
**GN-3 W1 / LOCK-1 deliverable 3C cannot supply**. (W3, **missing primitive**) the grammar-to-grammar
**rewrite DSL + comptime interpreter** does not exist — and building it *is the transposition spine one
meta-level up*, arguably equal-or-greater difficulty than the original problem. (W4, **coupling**) the
IR grammar must be **functor-compatible with LOCK-5's ONE-HIR/LAWFUL-FUNCTOR verdict**, undecided
(Constitution ii; 3C-here and 5A-there are one claim from two seats); and the wired self-host is
LOCK-7's (`bbnf.bbnf` is dormant, source parsed by a 2142-LOC hand parser). (W5) **no GN-wave consumes
an IR grammar today** — absent a named same-wave consumer it is consumer-less substrate (Y.13/viii).
(W6) cost/CSP/e-graph state is *not* clean sums-of-products and must stay hand-written — 3C's lawful
scope is the node algebra + structural maps, not the cost substrate. (W7) `no-ts-ir`: the meta-grammar
emits TS *type declarations*, never a TS runtime IR.

**Disposition: BLOCKED-candidate.** Two of its three walls are externally-owned primitives it cannot
resolve within LOCK-3 (`DirectTypeKind::Enum` from LOCK-1/GN-3 W1; the rewrite-DSL/interpreter, of
equal difficulty to the spine itself), plus an undecided LOCK-5 functor and no consuming wave — and its
make-or-break number (structural-fraction) is unmeasured. The product-only, structural-fraction
demonstrations are real and worth running as *scoping probes*, but they deliberately exclude the sum
and the DSL — the parts that *are* the family. None of the blockers is intractable (each a bounded,
named deliverable), so 3C is BLOCKED-candidate, not RETIRED: it converts to ADVANCED the moment LOCK-1
lands sums, LOCK-5 rules on the functor, and the structural-fraction clears 70%. Its unique gift — the
sternest consumer for the type plane and the bootstrap gate — is why it should be **banked, not
dropped**.

---

## FAMILY 3D — STAGED-RESIDUAL

**Architecture.** The IR is an explicitly two-stage program. Every node carries a binding time —
`Stage = { Static(S0), Dynamic(S1) }`, `S0 ⊑ S1`. The generator IS a partial evaluator (the first
Futamura projection made concrete); **residualization is the erasure of stage 0**. This is *not* a new
tower level (the sharpest intra-family discipline): `Stage` is an LLVM-shape **attribute** on the
existing IR (a `Vec<Stage>` parallel to `nullability`'s `Vec<bool>`), erasure happens inside the
existing `lower_to_rust` — an annotating pass + a split leaf emitter, nothing structural. Thesis: *the
comptime/runtime split lives in the types, and stage-0-costs-zero subsumes type-plane-costs-zero* (LMS
"only types distinguish the two stages"; Kovács 2LTT erasure metatheorem makes it a theorem, not an
analogy).

**Mechanism.** `fn binding_times(grammar) -> Vec<Stage>` beside `nullability` — the *identical*
monotone Changed-flag fixpoint (`ir/src/lib.rs:320`). Seeds: `Literal`/`Regex`/`Project.ty`/
`Annotation` = S0 (grammar constants); the input cursor = S1 (the only genuine runtime seed).
Everything joins (op is Dynamic iff any operand is Dynamic). Result: S1 recognizer spines with **S0
islands** (delimiter sets, alphabets, first-sets, DFA tables, capacity hints, field types). The emitter
splits: `emit_const(node) -> Residual` (evaluates an S0 subtree to a `u128` mask / `[u8;N]` alphabet /
DFA table / `usize` cap / `DirectTypeRef`, splices it as `const`/const-generic); `emit_runtime(node)`
emits the S1 spine reading the S0 constants by name. `Residual` is consumed in the same pass that
produces it (Y.13-clean). **`CollapsedStage` reclaimed**: the dormant consumer-less shape
(`ir/src/lib.rs:361`) gets its literal meaning — the rule whose entire body residualizes to a
straight-line tape-free recognizer (the lattice endpoint), retiring the dead surface by giving it
semantics rather than deleting it.

**The load-bearing claim (PROVE-12 as corollary).** PROVE-12 hand-wrote exactly one `emit_const`: the
delimiter set `b";{}"` folded to a `u128`, `find_component_delim<const DELIMS: u128>` reading it as a
const generic, the per-byte `delimiters.contains` interpretive dispatch (an S0 value stuck at S1 —
*precisely the binding-time error BTA forbids*) erased. 3D claims a stage-annotated lowering emits this
**automatically** from the annotated IR, no hand template edit. Reference to reproduce byte-identically:
`prototypes/PROVE-12/patch.diff`; `css_canon_bench` parity checksums `track1_full_parse=1`,
`track1_rich=17839`, both MATCH — the pass/fail line. GN-5.W4's ask ("12's const-mask generalized to
every grammar-derived stop-set") *is* this family.

**The strong unification.** GN-3's ZERO-RUNTIME-COST type invariant is *not a separate law* — a
projected type is an S0 value; `DirectBuildTarget.type_ref` residualizes to a Rust *type*, which has no
runtime representation by construction. **Type-plane erasure is the special case of stage-0 erasure
applied to the type sub-plane.** LOCK-1 types the S1 values; LOCK-3 erases the S0 program that computed
the S1 program — one erasure from two angles. This is the strongest cross-family synthesis in the
portfolio (prior-art §6.3): 3D subsumes GN-3 and PROVE-12 as two erasures of one stage.

**Integration seams.** `binding_times` beside `nullability` (`ir:320`); `Residual` colocated with
`DirectBuildTypeRef`; `LowerCtx` gains `stage_map: Vec<Stage>`; the BTA pass slot in `passes::compile`
after infer/shape (types resolved → `Project.ty` known S0) and before `lower_to_rust`, *reusing* the
alphabets `derive_recognizers` (`passes:338`) already computes; `StructuralAlphabet` (`ir:435`) is
already an S0 residual, merely *named* as such. Richest new targets beyond PROVE-12: `lower/css_scan.rs`
and JSON first-byte dispatch. Per-site staging form (monomorph vs shared `const` table) is a
`DecisionCspFacts`/`PriorityStep` decision handed to **LOCK-2** (`ir/cost.rs:203,264`) — 3D owns the
staging *semantics*, LOCK-2 owns the *schedule*.

**Gates it carries (born-RED).** (P1, load-bearing) `binding_times` + residualizer emit
`find_component_delim<const DELIMS>` from the annotated CSS IR alone; `regen-css` output **byte-
identical to PROVE-12's patch**, checksum MATCH — until this is green, 3D is SPEC-only. (P2) generalize
to a NEW site (JSON dispatch), measured **cold-paired PMU** (PROVE-09's two-arm-window self-deception
is fatal here — 3D is the family most prone to unmeasured plausibility), floor −3.7…−6.4% ins/B. (P3)
per-site staging is a `DecisionCspFacts` output, not a hardcoded emitter branch. (P4) a residual-count
oracle at regen (guards silent BTA over-approximation: a Static node marked S1 loses the win with no
error) + `Vec<Stage>` content-hash stable.

**Open questions.** (Q1) Does the delimiter set cleanly present as an S0 island BTA marks *without
binding-time-improvement hand-help* ("The Trick"), and can the const-generic seam be emitted
structurally? — P1 answers this. (Q2) Monomorph explosion vs the generated-size budget: the law must
*offer* monomorphization as a LOCK-2 candidate, never force it. (Q3) **Backend-dependent division (the
first cross-backend risk this family owns, unresolved)** — the optimal division for Rust (lean on the
monomorphizer: `const DELIMS` → 1 surviving symbol + 2 inlined) is *wrong* for TS (V8 has no
monomorphization; Static values must be fully residualized at build time — 5 specialized functions,
not 1 generic). The CSP cost model must be **backend-parameterized**; a single division is not
isomorphic. Directly implicates `no-ts-ir`/LOCK-6 (the backend-agnostic middle is the tape + schema,
not a shared instruction IR). (Q4) Capacity hints (`@cap(875)`) are *corpus* facts not grammar facts —
staging them S0 is a `StageError::Leak` unless the grammar declares them (no-silent-epsilon).

**Disposition: ADVANCED, most-evidenced synthesis vein.** PROVE-12 is a landed, born-verified
instance; the kernel is implementable within LOCK-3 without LOCK-2 (only the monomorph-vs-table *choice*
defers, and the backend-parameterized division is a refinement). Not blocked. The one acute risk is
measurement discipline + the unresolved cross-backend tension, not a missing primitive.

---

## THE INCOMPATIBILITY MAP

The four families are **not four mutually-exclusive spines**. Exactly one axis carries a *hard*
incompatibility; one axis carries a *contested seat*; everything else composes.

### The ONE hard incompatibility — Axis M, the level-count axis
**3A-as-tower ⊗ 3B-as-single-IR.** Opposite claims on the primary axis LOCK-3 owns — *how many IR
representations exist*. 3A (as chartered) wants a small fixed tower of **distinct typed value levels**,
each with a verifier at its value boundary — which requires each level to *exist as a distinct type*.
3B wants **one** IR, deleting `BackendIr`/`BackendExpr`, facts as attributes. A verifier that reads a
distinct typed level cannot read a level that has been deleted; they cannot both be the spine's shape.
This is the genuine, load-bearing incompatibility of LOCK-3.

**But it dissolves under a named hybrid.** If 3A's contract is re-read as a **per-pass predicate
`verify(arena, attrs)` over the single IR** (which research-B offers and research-A concedes), the
discipline survives without distinct typed levels. Prior-art §6.2 names this hybrid — *3A-discipline
layered on the 3B-substrate* — as the strongest available: keep one IR, give each pass a nanopass-style
checkable output contract. So the incompatibility is **real as-chartered, resolved by a hybrid**, and
the resolving question — *does per-pass contract-on-one-IR localize the defects a typed-level tower
would?* — is the sharpest uncertainty in the lock (Brief 1).

### The contested seat — Axis R, residualization ownership
**3D ⊗ 3C on who generalizes PROVE-12.** Both are the *right* generalization of the same landed win,
by *incompatible theories*. 3D: residualization is the **forced** erasure of a binding-time lattice
(*derived by BTA, not authored*). 3C: residualization is **one directed rewrite rule** with a `@const`
marker (*authored, grammar-to-grammar*). They are not disjoint operations — 3C's `@const` *is* a coarse
Static tag, and 3D could be the semantics behind a 3C rewrite — but as **owning theories they cannot
both hold**: you cannot simultaneously claim residualization is "forced by BTA, not authored" and "one
authored rewrite rule the pass writer places." This is the derived-vs-authored division. Brief 2
adjudicates it: if BTA *forces* the byte-identical residual with no hand-authoring, 3D's theory
dominates the seat; if the residual needs hand-help to fire, 3C's authored-rewrite has standing.

### The compatible layerings (these coexist; do NOT treat as competitors)
- **3B ⊕ 3D** — fully compatible. 3D is *architecturally a 3B move*: `Stage` is one more attribute slot
  on the single IR; the residualizer is a split leaf emitter, not a transposition.
- **3B ⊕ 3C** — compatible, orthogonal axes (count vs substance). 3C emits whatever node algebra
  survives 3B's collapse (fewer enums to generate).
- **3A ⊕ 3C** — deeply compatible: **nanopass is 3A+3C fused** (grammar-specified IR *with*
  auto-generated well-formedness contracts). Shared prior art (Keep/Dybvig). Note: this is *also* why
  3C leans 3A-shaped on Axis M — grammar-to-grammar maps presuppose distinct source/target grammars, so
  3C-as-authored is a 3A ally and a 3B antagonist unless the ISLE single-grammar reading is taken.
- **3A ⊕ 3D** — compatible: 3D's residual-count oracle *is* a 3A-style refinement oracle; the stage
  attribute can carry a contract. Both explicitly refuse to make residualization a tower level — a
  shared verdict, not a conflict.

### The coordination tension (compatible, but must agree at a seam)
- **3D vs 3C on the type-erasure seam.** Both touch `passes/src/lib.rs:204`. 3D *erases* the projected
  type as an S0 value; 3C *emits its declaration* as a grammar-derived schema. Compatible, but they must
  agree that `Project.ty` is threaded (not dropped) exactly once — a coordination obligation for the
  gestalt, not an incompatibility.

### The net shape for the gestalt
The likely convergent spine is **one IR (3B substrate) + per-pass contracts (3A discipline) + a stage
attribute with residualization (3D erasure)** — three compatible layers on one axis-resolved base —
with **3C banked as BLOCKED-candidate** until LOCK-1 (sums), LOCK-5 (HIR functor), and a named consuming
wave unblock it, and its structural-fraction clears 70%. The two decisions that gate the whole picture
are the **level-count adjudication** (Brief 1 — does per-pass contract on one IR suffice?) and the
**residualization-ownership adjudication** (Brief 2 — does BTA force PROVE-12?).

---

## DISPOSITIONS (for the registry)

| Family | Disposition | Missing primitive? | One-line reason |
|---|---|---|---|
| **3A** | ADVANCED (discipline) / CONTESTED (tower) | none | Every organ has an extant fragment; flagship L2 contract buildable + provably non-vacuous (RED on HEAD); but its value is as a layer, and its tower form contests 3B on Axis M. |
| **3B** | ADVANCED (strongest structural) | none | The second IR is deletable today behind a byte-identity gate; standing laws hold by construction; no cross-lock primitive for Stratum 1. |
| **3C** | **BLOCKED-candidate** | `DirectTypeKind::Enum` (LOCK-1/GN-3 W1) + the rewrite-DSL/interpreter (≈ the spine one meta-level up) + LOCK-5 HIR-functor verdict + a consuming GN-wave; structural-fraction unmeasured | Its flagship (sum node algebra + grammar-to-grammar maps) needs externally-owned primitives, one of equal difficulty to the original problem. Bounded, not intractable → bank, don't retire. |
| **3D** | ADVANCED (most-evidenced synthesis) | none for the kernel (LOCK-2 for per-site scheduling; backend-parameterized division for TS) | PROVE-12 is landed born-verified; kernel runs without LOCK-2; subsumes type + constant erasure under one law. Acute risks are measurement discipline + the cross-backend division, not a primitive. |

---

## THE TWO SHARPEST UNCERTAINTIES (next-pass prototype briefs)

Returned in full via StructuredOutput. In brief:
1. **THE LEVEL-COUNT ADJUDICATION (3A ⊗ 3B → hybrid).** Collapse to one IR behind byte-identity, *and*
   wire the flagship no-value-discard contract as a per-pass predicate on that one IR — does it localize
   the defect (RED on HEAD, GREEN after the `->` thread) without a second typed level? Resolves the
   lock's only hard incompatibility.
2. **PROVE-12 AS FORCED BTA COROLLARY (3D vs 3C on Axis R).** Can `binding_times` + the split emitter
   reproduce PROVE-12's const-generic residual byte-identically from the annotated IR alone, no
   hand-authoring? Resolves whether 3D graduates from SPEC-only and whether residualization is derived
   (3D) or authored (3C).
