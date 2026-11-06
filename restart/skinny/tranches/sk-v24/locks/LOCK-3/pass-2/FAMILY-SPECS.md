# LOCK-3 — THE IR TRANSPOSITION SPINE · FAMILY-SPECS (pass-2 synthesis, RAN-prototype-aware)

*Synthesis distillation, one cogent spec per family, written after the four pass-2 prototypes
RAN (PROTO-1 `:204`-thread, PROTO-2 fold-fact-purity trace, PROTO-1-t1 seam-slice, PROTO-2-3A
refinement oracle). This supersedes the frozen 15:10 FAMILY-SPECS that named only the two
early prototypes; it folds all four and re-cuts every gap against executed evidence, per the
REGISTRY process-correction.*

## §0 — Verified baseline (re-anchored to live)

`git rev-parse HEAD = 3f2350f15af1cc886e2968be70e7bee610ae0e2a` — **exactly the pin all five
research reports used**; the source tree lives at top-level `skinny/crates/` (NOT
`restart/skinny/`, docs only). XL-H hygiene: the REGISTRY's `c43126422` re-anchor is byte-
identical to `3f2350f15` over `skinny/crates` (`git diff --stat` empty), so every prototype
number transfers verbatim; the live tree measured here **is** HEAD. Anchors re-greped this
pass, all CONFIRMED:

| Fact | Live truth (`3f2350f15`) |
|---|---|
| `->` ty-drop | `crates/passes/src/lib.rs:204` `ExprKind::Project { body, .. } => self.infer_expr(*body)?` |
| `:203` non-defect | `Annotation { .. } => Type::Builtin(BuiltinTy::Unit)` (legit `@ws`/`@cap` untyped) |
| projection re-derive | `crates/codegen/src/general_descent/view.rs:35` `classify_projection(target,ty)` → 4-way `ProjectionKind{Float,Bool,Tag,Span}` (`:20`), `Span` fallthrough `:45` |
| schema sum-wall | `crates/codegen/src/direct_schema.rs:52` `DirectTypeKind` **Struct-only** (no `Sum` variant) |
| dead stage gate | `crates/passes/src/lib.rs:959` `admits_collapsed_stage` reads `target.avx512bw` (`:380` = `cfg!(all(target_arch="x86_64", target_feature="avx512bw"))`) — **permanently false on aarch64** |
| re-walk consumers | `json_sink_direct/dispatch.rs` = 318 LOC, `general_descent/view.rs` = 431 LOC — **structural tree-walk consumers**, not emission-only intermediates |
| XL-1 (verified) | `TypeAnalysis`/`from_ir`/`analysis.rs` **absent from live `skinny/crates/`**; exist only in legacy `crates/core/...` over legacy `GrammarIR` |

**The spine as it stands (four-edge transposition chain):**
```
.bbnf ─parse→ GrammarIr ─lower_expr/materialize_rule→ BackendIr(BackendExpr) ─sink_only→ SinkOnlyProgram ─render/emit→ .rs
```
Two facts bind every family. (1) The tower is **3B-shaped today** — a flat `ExprId`/`RuleId`
arena (`ir/src/lib.rs:16,19`) with attribute side-tables (`node_types`
`passes/src/lib.rs:93,212`; `nullability` dense column `ir/src/lib.rs:320`), then **three tree
rebuilds** (`lower_expr`, `materialize_rule` splice, `SinkOnlyExpr`) before a **fractured**
emission stage. LOCK-3 either hardens this (3A/3B) or re-founds it (3C/3D), every divergence a
named supersession. (2) The **`Project.ty`@204 thread** is single-owner and DISCHARGED for the
*fact* plane (PROTO-1 RED→GREEN, zero emit footprint) — but the CONSUME half (`.target` is a
dead field, `None` at every construction site) and the consistency CONTRACT over the threaded
value remain open, and are the axes the families split on.

---

## §1 — FAMILY 3A · CONTRACT-TOWER · ADVANCED (as discipline on the 3B substrate) · 60%

**Architecture.** A small **fixed** tower (target three level-languages: `TypedHir` = GrammarIr
+ resolved `node_types`; `MidIr` = BackendExpr/SinkOnlyProgram; `Emit` = source), each edge a
*total function carrying a checkable contract* — a per-edge `verify(pre,post)` predicate that is
pure, emit-silent (regen byte-identical with verifiers on/off), and CI-wired. This is the
nanopass "output checked against declared target language" discipline + rustc's MIR `Validator`,
expressed as **data over the live IRs**, not a macro-generated IL zoo (priorart §2 TRAP: nanopass
IL proliferation ≈ 2× build — rejected; adopt contract-per-level, reject many-tiny-ILs). The
standalone typed-multi-level-tower is **BANKED**; 3A advances only as a discipline layered on 3B.

**Mechanism (what RAN).** PROTO-2-3A executed the distinctive organ — the per-edge refinement
oracle `differential(edge,corpus)` — 4/4 tests EXIT 0. The **E3 correspondence oracle** (sink map
= 1:1 arm renaming) is reference-free and GREEN on live `json` BackendIr (15 rules/100 nodes); an
injected single-arm E3 fault localizes to its arm with **zero false-positive witnesses** across a
5-fault matrix (incl. a same-discriminant payload corruption a tag-only check misses); an E2 input
fault trips E2 while E3 stays GREEN — so the runner **localizes to the injected edge, never merely
"a failure."** PROTO-1 separately proved `verify_project_types` over `Project@204`: RED (4 spans) →
GREEN, four regen checks byte-identical.

**Integration seams.** Thread: `passes/src/lib.rs:204`→`:212`. Consumer flip:
`general_descent/view.rs:35` `classify_projection`. Verifier host: `passes/src/lib.rs:126`
`TypeFacts`. Differential runner: `bbnf-bench/src/direct_struct.rs` (`mix`,`typed_checksum`),
parity `benches/json_parity.rs:200`.

**Gates it would carry (born-RED).**
- G-3A-α **the contract is a GATE-THAT-CANNOT-FAIL until LOCK-1.** `Type::Declared(String)` is a
  *nominal* wrapper never structurally equal to the *structural* `infer_expr(body)`, so the proven
  predicate's `≠ body` clause is **vacuously true whenever `ty=Some`**. The organ verifies
  threading-*presence*, not declared-vs-inferred *consistency*. True non-vacuity is **born-RED on
  LOCK-1** (`G-3A-a` name→`Type` resolution); until it lands the contract degrades to string-equality.
- G-3A-β **E1-infer injected-fault matrix UNRUN** — the oracle ran on the *free* E3 edge (1:1
  renaming); the E1(infer)/E2(materialize) matrix, where the `ty`-contract's meaning lives, is unrun.
- G-3A-γ E3 is a lossy correspondence (erases `SpanMark.kind`/`TapeEmit.kind`), not a round-trip;
  a fault flipping only an erased `kind` is invisible to E3 (must be caught at E2).
- G-3A-δ E4/`render` (`runtime_generator.rs:78`) as pure projection of L2 unproven; CSS
  `Err⇒SinkOnly / Ok⇒Scan` two-root fork (`:40-71`) undecided.
- G-3A-ε the runner is a `+607`-LOC probe, not the shipping `differential(edge,corpus)` CI contract.

**Exact open questions.** (i) Does ≥1 Sheets projection exist where declared `ty` constrains
*beyond* `infer_expr(body)` — i.e. can the contract EVER be non-vacuous, or is it structurally
doomed until LOCK-1? (ii) Is `render` a pure projection (three-levels holds, E4 carries no oracle)?
(iii) Is the CSS fork a first-class edge oracle or a declared two-root tower?

---

## §2 — FAMILY 3B · SINGLE-IR-MULTIPASS · ADVANCED (the convergent substrate) · 55%

**Architecture.** One rich arena refined in place; backend shape, tape kind, residual mask, and
projected type are attribute **columns keyed by the arena's own `ExprId`/`RuleId`**, not a second
tree. Emission is one fold `emit(&GrammarIr, &Attributes)`. Precedent: LLVM new-PM (analyses in
side maps keyed by IR unit), MLIR attribute dictionaries, Cranelift `SecondaryMap`, GHC Trees-That-
Grow. The mechanism deletes `BackendExpr`/`BackendIr`/`SinkOnlyExpr` as trees (3 rebuilds → 0) and
lets the emit fold walk the arena joined against columns; it *repairs* a standing no-value-discard
violation (`Discard{keep_left}` flattened to `Seq` at `lower_expr:1354-1358`, "metadata for a later
phase") for free.

**Mechanism (what RAN).** PROTO-2 cleared the fact-layer purity precondition **by trace**: the four
`SinkOnlyFacts` accumulations read only `GrammarIr + backend_shape + descriptor`, so
`materialize_rule`'s emit-order splice is a **pure derivation recomputable inside one fold** (its
sole external input is the per-`RuleId` descriptor from `derive_materialization_plan(&GrammarIr)`).
PROTO-1-t1 then **executed** the emit-dispatch seam retirement — replacing the `has_rule("value")`
name-gate with a structural value-role census — **byte-identical GREEN 3/3** (`check-{json,sheets,
real-typed}`), the first *run* code on the fold path. It also **refuted PROTO-2's discriminator by
execution**: `dispatch_alt_count == 0` turns `check-sheets` RED (Sheets carries dispatch Alts too);
the honest discriminator is the **≥4-of-6 value-category census** (`roles.value`).

**Integration seams.** Rebuilds to delete: `lower_expr` `passes/src/lib.rs:1318-1360`;
`materialize_rule` `:1362-1389`; `single_plan` `:1283-1316`; `SinkOnlyExpr`
`lower/sink_only.rs:175-238`. Emit dispatch: `render` `runtime_generator.rs:78`; `has_rule("value")`
`:98`; JSON static templates `:101-131`; general-descent `:147-188`. The **two structural re-walk
consumers** that must re-base onto `GrammarIr`: `json_sink_direct/dispatch.rs` (318 LOC) +
`general_descent/view.rs` (431 LOC).

**Gates it would carry.**
- G-3B-1 **THE killer gate — still UNRUN after two passes.** *Execute* the whole
  `emit(&GrammarIr,&Attributes)` fold; **regen×check × {json,css,sheets,real-typed} byte-identical.**
  Trace (PROTO-2) + one slice (PROTO-1-t1) ≠ the fold. Per the spec's own condition the demotion
  trigger is partly met: run it next pass or demote 3B to "structurally-motivated, unproven."
- G-3B-1a **the re-walk re-expression IS the unconverged hard part** (the elegant-reduction tell).
  Deleting `SinkOnlyExpr` adds back a comparable re-walk of `GrammarIr` in dispatch.rs (318) +
  view.rs (431) — a reordered/differently-keyed walk breaks byte-identity. The **−100 net-LOC is the
  reward IFF the walkers re-base byte-identically — an unrun assumption, CONTINGENT, not a measured
  deletion.**
- G-3B-2 **relocation ≠ retirement** (a DRY smell, born-RED). PROTO-1-t1's slice duplicated
  `roles.value` into codegen (a `system-cohesion`/DRY breach the probe flags). The clean single-home
  is threading passes-computed `roles.value` onto the attribute bundle; true RETIREMENT stays born-RED
  on `DirectTypeKind::Sum` + a value-role attribute.
- G-3B-3 materialization key (per-`RuleId` vs per-`ExprId`) unconfirmed for the typed fleet (CSS L4
  inline alternatives the live candidate); a partial migration is a `no-orthogonal-codepaths` defect.
- G-3B-4 CONSUME REFUTED-as-stated: `.target` is a **dead field** (`None` at all ~9 sites, never read);
  true CONSUME sits behind three walls inside G-3B-1's fold, not a separable move.
- G-3B-5 byte-identity incidental-vs-semantic (header comments, `BTreeMap` order) must be attributed
  on the *executed* fold; does `css_scan` hide a fourth value level?

**Exact open question.** Does the whole fold hold byte-identically once dispatch.rs+view.rs re-base
onto `GrammarIr`, or does the reordered walk break identity and evaporate the −100 LOC?

---

## §3 — FAMILY 3C · GRAMMAR-HOMOIOUSIA · BLOCKED (bank the gift) · 32%

**Architecture.** The IR is itself grammar-specified (`ir.bbnf`); transpositions are grammar-to-
grammar maps; the generator eats itself. Nanopass `define-language` + MLIR-PDL + Stratego/Spoofax +
attribute grammars are the mature precedents (priorart §2,§3,§6). Research-C's decisive move is the
**C1/C2 split**, which is the family's honesty and its real analysis progress:

- **C1 — the IR *data model* is grammar-specified.** Bounded born-RED: `DirectTypeKind::Sum`
  @`direct_schema.rs:52` (verified Struct-only) + a consuming **LOCK-7·7B** wave. On live there is
  **no `ir.bbnf`, no meta-grammar, one grammar (`json.bbnf`)**; C1's sole reader is an unlanded LOCK-7
  wave — admissible only as a born-RED edge under GESTALT-B (Y.13 consumer-less-substrate).
- **C2 — the IR *transpositions* are grammar-to-grammar maps.** **REFUTED on live** (the sharpest
  counterexample): bbnf maps bytes → tape, but `lower_expr`/`materialize_rule` are **tree → tree**
  folds with no byte stream; furthermore `derive_materialization_roles` ("the rule referencing ≥4
  value targets") and `BackendShape` selection are **whole-grammar analyses / cost-directed
  decisions**, expressible in *no* rewrite-rule language. Realizing C2 demands a term-rewriting/
  attribute-eval engine bbnf lacks → importing one is an orthogonal-subsystem breach OR a collapse
  into LOCK-2's engine.

**Gates (all externally owned).** G-3C-1 `DirectTypeKind::Sum` (born-RED, GN-3.W1) — the product-only
subset excludes every `Alt` = excludes `ExprKind`/`BackendExpr` themselves, and **must not be treated
as standing for the formulation** (elegant-reduction trap by construction). G-3C-2 named LOCK-7·7B
consumer (born-RED, Y.13). G-3C-3 `regen∘regen = regen` fixpoint over grammar-generated `ir/src/lib.rs`
carrying derives/docs/impls (LOCK-7·7A). G-3C-4 C2 cession to LOCK-2 (provisional on XL-B WORKING).

**BLOCKED-candidate verdict (C2).** C2 stalls at a missing primitive — a term-rewriting/attribute-
evaluation engine — **of difficulty equal to the original transposition problem**. Elegance earns
nothing here: importing the engine is a constitution breach, ceding it is LOCK-2's seat. C2 is a
BLOCKED-candidate, ceded to LOCK-2 (provisional on XL-B). C1 is BLOCKED on two born-RED external
primitives. The **distinctive 3C content is 0% convergence-eligible this pass; SPEC-ONLY**. What
stands alone is *not* distinctively 3C: the `->`-thread is the same single-owner `:204` thread PROTO-1
ran (3C consumes it, does not author it).

**Exact open question.** Is any LOCK-7·7B wave named that would *consume* `ir.bbnf` in-wave — the only
route from BLOCKED to ADVANCED? (Not resolvable inside LOCK-3.)

---

## §4 — FAMILY 3D · STAGED-RESIDUAL · BANKED (stage-attribute subsumed into 3B) · 38%

**Architecture.** The IR is an explicitly two-stage program: a comptime (stage-0) sub-program the
generator evaluates away and a runtime (stage-1) sub-program over bytes; residualization is *erasure
of stage-0*; PROVE-12 is a corollary. `BindingTime ∈ {Static,Dynamic}` is an offline BTA (Jones–
Gomard–Sestoft; LMS carries the stage in the *types*, priorart §5) attached as an `Attributes` side-
table over `GrammarIr` — a monotone fold isomorphic to `infer_expr`. This is **architecturally a 3B
move** (`Stage` is one attribute slot); the separate-staging-spine formulation stays BANKED into 3B's
`Attributes` store.

**What did NOT run (Constitution vii).** Nothing 3D-distinctive ran this pass. The two distinctive
claims — (i) `BindingTime` as a typed IR fact, (ii) "type-plane erasure = stage-0 erasure" — are both
SPEC-only, and (ii) **spec-cites-itself** (its warrant is Constitution v restated). The leverage rests
on four soft anchors: **PROVE-12** is a retired-target measurement (its target `find_component_delim`
is retired from `codegen/src`; PMU not re-run on the moved base); **`ScanAlphabet`** is already
shipped, not 3D-authored; **G-3D-1 the DEAD gate** — `admits_collapsed_stage`@`passes:959` reads
`avx512bw` (`:380`, x86-only), so `CollapsedStage` is a **dead shape** that fires
`CollapsedStageNotViable` whenever declared — a genuine Y.13 consumer-less-substrate *problem*, not a
bank; the **PROVE-12-`u128`-byte-identity flagship stays RETIRED**.

**Gates (reactivates to ADVANCED only if all three run concretely).** G-3D-i attach binding-time to
real IR and prove "type-plane = stage-0" beyond restatement (consumes the `:204` thread). G-3D-ii
`binding_times(CssScanProgram)` == PROVE-12 partition (checksum parity oracle). G-3D-iii re-home
`admits_collapsed_stage` off `avx512bw` onto a grammar-derived binding-time predicate → admissible on
aarch64 via `decision_csp`, no x86 smuggled back; RED→GREEN on `CollapsedStageNotViable`. G-3D-iv JSON
type-plane cold paired-PMU delta (likely NEUTRAL — the `match byte` is already LLVM-residualized; owned
as a named negative if so).

**Exact open question.** Does a BTA over the real CSS IR reproduce `derive_alphabet`'s hand division
byte-identically AND subsume a *second* live consumer (the CollapsedStage re-home), or is it decoration
over an already-shipped const?

---

## §5 — THE CONVERGENT SPINE (hypothesis; sound; rests on two UNRUN load-bearing halves)

**One IR (3B substrate) + per-pass contracts (3A discipline) + a stage attribute (3D erasure); 3C
banked** until GN-3.W1 (sums) + a named LOCK-7 wave unblock it. The families **compose, not compete**:
3A's E3 refinement oracle ran on the *same live BackendIr* the 3B fold would fold — the graft is
partially executed. State after the four-prototype fold:

| Component | State |
|---|---|
| `:204` thread + verify predicate (RED→GREEN, zero footprint) | **PROVEN** (PROTO-1) |
| Fold fact-layer purity (`materialize_rule` recomputable) | **PROVEN by trace** (PROTO-2) |
| 3A oracle localizes an injected fault to its edge | **RAN** on the free E3 edge; E2↔E3 separation proven (PROTO-2-3A) |
| 3A *contract* non-vacuity (E1-infer; declared-vs-inferred) | **UNRUN + gate-cannot-fail until LOCK-1** |
| Seam relocation (`has_rule("value")` → value-role census) | **RAN byte-identical** (PROTO-1-t1); single-home + retirement deferred |
| `dispatch_alt_count` discriminator | **REFUTED by execution**; working = ≥4-of-6 census |
| **3B-G1: whole fold, regen×check byte-identical** | **UNRUN — the spine's load-bearing gate** (−100 LOC contingent on unrun re-walk) |
| CONSUME (`.target` → schema) | **REFUTED-as-stated** (`.target` dead); behind the fold |
| 3D binding_times on real IR + CollapsedStage re-home | UNRUN (BANKED) |
| 3C C1 / C2 | BLOCKED / **BLOCKED-candidate** (refuted, ceded to LOCK-2 prov. XL-B) |

**The two UNRUN load-bearing halves** both critics converge on, and the two sharpest uncertainties
for pass 3: (1) **3B G-3B-1** — the whole-fold regen×check byte-identity (one slice ≠ the fold; the
slice used a Constitution-forbidden DRY home); (2) **3A E1-infer non-vacuity** — the oracle is proven
on the 1:1 free edge, not on the edge that could be vacuous. No family is convergence-eligible (pass 2
of ≥3; each carries ≥1 UNRUN load-bearing gate).

---

## §6 — THE INCOMPATIBILITY MAP (which families cannot coexist, and precisely why)

**Resolved incompatibility (the lock's one hard fork).** *Distinct typed level-languages (3A-tower,
nanopass many-IL) ⊥ one rich IR refined in place (3B).* These cannot both be the substrate: 3A-tower
mints a fresh IL per edge (≈2× build, per-level boilerplate — priorart §2 TRAP); 3B keeps one arena.
**Resolved toward 3B**, with running evidence both sides (3A oracle on live IR; 3B slice byte-
identical). 3A survives *only* as a discipline over the 3B IR (per-edge `verify` predicates on one
tree, not a tree per edge); the standalone typed-multi-level-tower is BANKED. Funding both substrates
is a double-home breach.

**3C ⊥ 3B on the seam-dissolution, single-funded to 3B by execution (XL-3).** `has_rule("value")`@98
+ `derive_materialization_roles`@1403 are the **same live hardcodes** 3C would "dissolve via grammar"
and 3B would "retire via fold." PROTO-1-t1 executed 3B's byte-identical relocation of this exact seam.
The gestalt funds **one** (3B); 3C's grammar-dissolution reactivates only if 3C unblocks. They cannot
both own the seam.

**3C-C2 ⊥ LOCK-3's own charter (BLOCKED-candidate).** C2 (transpositions-as-grammar-maps) requires a
term-rewriting engine of difficulty equal to the original problem; importing it breaches
orthogonal-subsystem/parsimony, so C2 is *incompatible with landing inside LOCK-3* and is ceded to
LOCK-2 (provisional on XL-B). This is not a rivalry with a sibling family — it is a family stalled at a
missing primitive, hence BLOCKED-candidate.

**3D ⊥ a separate staging spine, compatible-as-attribute with 3B.** 3D's `BindingTime` is *not* a new
IR level; a fourth level between HIR and BackendShape would contradict "fewest transpositions." 3D is
**compatible with 3B** as an orthogonal attribute axis (Stage column) and **incompatible with itself
as a standalone tower**. Its per-site polyvariant staging *decision* ⊥ a LOCK-3 hardcoded emitter
branch — that decision is LOCK-2's seat (XL-B), gated on a real consumer.

**No cross-family mechanism collision remains unadjudicated.** 3A-discipline, 3B-substrate,
3D-attribute compose on one IR; 3C is banked. The only live tension inside the composed spine is
*ownership of the threaded `:204` value*: 3A verifies it (contract), 3B threads it (column), 3D erases
it (S0 residual), 3C declares it — the thread has **one author** (a GN-3 wave, `no-value-discard`) and
three consumers; this is composition, not incompatibility, provided the CONSUME half lands inside
G-3B-1's fold and the CONTRACT half lands born-RED on LOCK-1.

**Cross-lock (carried, not re-litigated here — binding per CROSS-LOCK).** XL-1: the
`TypeAnalysis::from_ir` "real type consumer" (XL-F(1)) is **absent from live skinny** (verified);
the live consuming edge is the `DirectSchemaSet` seam — routed to GESTALT-B as a named-supersession
challenge with evidence (either XL-F(1) describes the greenfield tree-of-record and must be *labeled*
so, or it is a stale live-spine pointer needing amendment). XL-B: LOCK-3 owns the middle-IR tower
shape + decision-driver seat; LOCK-2's engines operate *within* (WORKING → final at GESTALT-B). The
cessions (3C-C2, 3D staging-form) carry "(provisional on XL-B)".
