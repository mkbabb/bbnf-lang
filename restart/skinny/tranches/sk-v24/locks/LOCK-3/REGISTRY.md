# LOCK-3 — THE IR TRANSPOSITION SPINE · REGISTRY

*Living adjudication ledger across the ≥3-pass convergence loop. The AGGLOMERATOR folds each
pass (research + FAMILY-SPECS + PROTOs + CRIT-1 + CRIT-2) into this file. Convergence % is the
**critics' floor** unless the agglomerator can name why a critic erred; no family approaches 100%
until it has zero open gaps + a fresh audit + two consecutive clean passes. Status is one of
ADVANCED / BANKED / BLOCKED / RETIRED.*

Charter: the IR tower and its transpositions (grammar AST → typed HIR → mid-IR → backend shapes →
emission); lowering contracts and invariants; residualization (PROVE-12) generalized; the decision
driver's seat; multi-backend (Rust now, TS later) over a backend-agnostic middle. Standing laws:
`no-value-discard`, `no-silent-epsilon`, `preserve-rich-ast`.

---

## VERIFIED BASELINE (re-anchored by the agglomerator against live this pass — supersedes FAMILY-SPECS §0)

Live tree is **`skinny@b3cb8c16b`**. The spine at HEAD is a four-edge transposition chain:

```
.bbnf ──parse──▶ GrammarIr ──compile/lower_expr──▶ BackendIr ──sink_only──▶ SinkOnlyProgram ──lower_to_rust/emit──▶ .rs
```

Anchors corrected and confirmed **verbatim this pass** (agglomerator re-ran the greps):

| Fact | Live truth (`b3cb8c16b`) | Pass-1 synthesis had it as |
|---|---|---|
| The `->` ty-drop | `passes/src/lib.rs:**204**` `ExprKind::Project { body, .. } => self.infer_expr(*body)?` (`ty` destructured via `..`) | mis-cited as `:203` |
| `passes/src/lib.rs:203` | `ExprKind::Annotation { .. } => Type::Builtin(BuiltinTy::Unit)` — a **non-defect** (`@ws`/`@cap` directives are legitimately untyped) | mis-cited as the `Project` defect |
| CSS emit path | **IR-driven**: `css_scan_direct/{mod,projection}.rs` + `lower/css_scan.rs::ScanAlphabet::derive_alphabet` emit grammar-derived `const … : &[u8]`; `CSS_GENERATED_RS` **gone** | (prototypes saw the retired raw-string template) |
| `find_component_delim` (PROVE-12 target) | **retired** from `codegen/src`; survives only in `runtime/src/runtime_scan.rs` + a bench doc-comment | (assumed present) |
| `binding_times` / `Stage` | **absent** from live `crates/ir/src/` — 3D is entirely unlanded | (assumed landed via worktree) |
| `has_rule("value")` name-gate | **survives** at `runtime_generator.rs:98` (a masked fallback, clause-vi-forbidden) | (targeted for dissolution, not retired) |
| `DirectTypeKind` | **Struct-only** (no `Enum` variant) — sum node algebra un-emittable | (named blocker — correct) |
| Prototype baseline | both PROTO ran `d2b32f6e3`, **51 commits** behind live (`git rev-list --count d2b32f6e3..HEAD` = 51; ancestor confirmed) | described as "HEAD, verified this pass" |

**Two facts bind every family.** (1) The tower is **3B-shaped today** — one rich `BackendIr`,
`BackendShape` an attribute, fewest transpositions; LOCK-3 either hardens (3A/3B) or re-founds (3C/3D),
and every divergence from the GN codex is a **named supersession** (Constitution ix). (2) The
**`Project.ty`@204 thread is broken and the schema is starved** — `DirectBuildTarget` is a real type
lattice but `.target` is `None` on every offset-tape descriptor; the sole schema producer is the
hand file `xtask/src/real_typed_schema.rs`. Closing this seam is the obligation shared by 3A/3B/3D and
the product of 3C — **and pass-1 discharged it in no family** (see the quadruple-home directive).

---

## CURRENT DISPOSITIONS (as of pass-1)

| Family | Status | Convergence | One-line |
|---|---|---|---|
| **3A · CONTRACT-TOWER** | **ADVANCED** (as discipline-on-3B-substrate; the standalone typed-multi-level-tower **formulation is BANKED**) | **55%** | Per-pass contract-on-one-IR RAN/PROVEN (localization + zero footprint), but on the *wrong node*; the distinctive refinement-oracle organ is entirely unrun. |
| **3B · SINGLE-IR-MULTIPASS** | **ADVANCED** (the convergent substrate) | **50%** | The killer byte-identity collapse gate is UNRUN; the literal "pure redundancy" thesis is refuted (second IR carries emit-order framing) → "tree-redundant, computation-essential"; a name-gate + role-classifier survive that the collapse must **retire, not relocate**. |
| **3C · GRAMMAR-HOMOIOUSIA** | **BLOCKED** (bank the gift; converts to ADVANCED on unblock) | **30%** | Stalled on three externally-owned primitives (`DirectTypeKind::Enum`, LOCK-5 HIR-functor verdict, a consuming GN-wave); zero prototype ran; its runnable part definitionally excludes the sum that *is* the family. |
| **3D · STAGED-RESIDUAL** | **BANKED** (stage-attribute subsumed into the 3B substrate; the PROVE-12-`u128`-byte-identity flagship is **RETIRED** as a named negative) | **38%** | Flagship P1 REFUTED and its target retired on live; machinery ran only on a synthetic mirror; unlanded; the surviving live residual is already shipped as `ScanAlphabet`; the distinctive unification is unproven. |

Floor rule applied: 3A floor(58,55)=55, 3B floor(55,50)=50, 3C floor(45,30)=30, 3D floor(40,38)=38.
The agglomerator re-verified every crux fact both critics rest on (line identity, staleness, CSS
IR-drive, 3D non-landing, name-gate survival, Struct-only schema) — **the critics did not err**, so
the floor stands unadjusted.

---

## FAMILY 3A — CONTRACT-TOWER · ADVANCED (discipline) · 55%

**What is earned (RAN/PROVEN, baseline-independent).** PROTO-1 probe (b): a per-pass
`verify(arena, attrs)` predicate on a **single** IR localizes a dropped type to the exact L0→L1
(infer) edge while `cargo build` stays green — **3A-as-a-typed-tower is not required for defect
localization**. Probe (c): the verifier has **zero emit footprint** (four regen checks byte-identical
on/off; `runtime/src/grammars/` git-clean). The prior-art spine (GHC Core-Lint, MLIR `--verify-each`,
rustc `MirPhase` ladder, translation validation) is sound; the `ContractViolation`-not-`Err(String)`
retyping is real prerequisite work.

**Redirect (crowding).** 3A and 3B were charted to collide on the level-count axis. PROTO-1 resolves
that toward the hybrid: **the redundant *formulation* is "3A as a standalone tower of distinct typed
value levels" — BANKED.** 3A the family **advances as a discipline layered on the 3B single-IR
substrate**. The two do not compete once 3A is re-read as per-pass contracts over one IR; they
**compose** (this is the named cross-pollination below).

**Open gaps.**
- **G-3A-1 (non-vacuity anchor invalid as run).** PROTO-1 wired its flagship contract over
  `Annotation`@203 (a non-defect returning `Unit`), not `Project`@204 (the real `ty`-drop). "RED on
  HEAD" is currently RED for a non-defect; the real defect went unexercised. **Re-wire over
  `ExprKind::Project`@204; re-prove RED-on-HEAD / GREEN-after-thread.**
- **G-3A-2 (the distinctive organ is entirely unrun).** The **per-edge refinement / translation-
  validation oracle** — the one thing that distinguishes 3A-discipline from a trivial `verify()` —
  never ran. Probe (c) is near-tautological (an assert-and-return cannot change emitted `.rs`); the
  zero-cost law is only tested non-trivially by an oracle that folds over emitted output.
- **G-3A-3 (level count decreed, not measured).** PROTO-1 *surfaced counter-evidence to "three"*:
  `materialize_rule` synthesizes `SpanMark/TapeEmit/DirectBuild/Return` framing with no `ExprKind`
  counterpart, and `SinkOnlyProgram` is a real level with no verifier. The labeled edge-DAG
  enumeration (3 vs 4) was not performed.
- **G-3A-4 (panic-discharge unproven; corpus coverage undefined).** "well-formedness ⇒ the four
  `extract` panics unreachable" is asserted, not shown; the refinement oracle is corpus-bounded and
  its coverage is not yet a first-class metric.

**Pass-2 directives.**
1. Re-run the flagship contract over `Project`@204 on **live `b3cb8c16b`**; RED-on-HEAD then
   GREEN-after-thread is the falsifiable.
2. **Open the underexplored formulation:** prototype the **per-edge refinement oracle** —
   generalize the PROVE-12 XOR-checksum + `json_parity` `track1/track2` digests into one
   `differential(edge, corpus)` runner; prove it (a) reproduces the existing digests and (b) detects
   an **injected mis-lowering localized to its edge**. This is the organ that earns 3A beyond a
   tautological verifier.
3. Perform the edge-DAG enumeration; reconcile the `materialize_rule` fourth-edge fact.
4. Define + threshold the refinement oracle's corpus-coverage metric (zero-witness constructs report
   as *unverified*, not sound).

**Withheld from researchers:** do **not** re-litigate the standalone typed-multi-level-tower form
(BANKED); do **not** re-argue whether per-pass contracts localize without a typed level (PROVEN).

---

## FAMILY 3B — SINGLE-IR-MULTIPASS · ADVANCED (substrate) · 50%

**What is earned.** The cleanest structural argument in the lock: the deletion targets all exist on
live (`single_plan`@1283, `lower_expr`@1318, `materialize_rule`@1362, `BackendIr/Expr/Rule`,
`BackendShape`); the attribute store already exists unformalized (`LayoutFacts`, `nullability`
`Vec<bool>` archetype). The two-stratum Y.13 discipline — collapse now; `PassManager`/invalidation
land **only with** a first mutating consumer — is exactly right and forbids the consumer-less-substrate
trap by construction. 3B is the substrate the convergent spine hangs on.

**Open gaps.**
- **G-3B-1 (the killer gate is UNRUN — elegant-reduction trap).** G1 — delete the second IR,
  re-express emission as one `emit(&GrammarIr,&Attributes)` fold, and prove **all six regen checks
  byte-identical** (regen/check × json/css/sheets) — was **not executed** ("beyond the cap"). The
  entire proof of the family is deferred. "Executable" ≠ "executed."
- **G-3B-2 (literal thesis refuted as stated).** PROTO-1's data-flow verdict: `materialize_rule`
  synthesizes emit-order framing absent from `ExprKind` → the second IR is **"tree-redundant but
  computation-essential."** The G1 premise ("if the fold reproduces every generated.rs, the second IR
  was pure redundancy") is false as worded; the correct, weaker, and *unrun* claim is "the framing is
  a pure derivation recomputable inside one fold, byte-identically."
- **G-3B-3 (the masked fallbacks survive).** `has_rule("value")`@`runtime_generator.rs:98` (a
  clause-vi-forbidden name-gate) and `derive_materialization_roles`@1403 (byte-literal role
  classifier) persist on live. A naive collapse **relocates** them inside the fold. 3B must show the
  collapse **retires** them.
- **G-3B-4 (coarse key unconfirmed).** The commit to a per-`RuleId` materialization key was not
  checked against "no fleet grammar needs per-`ExprId`" — risking the exact `TapeEmit{kind}` collapse
  MLIR warns of that 3B claims to close.
- **G-3B-5 (Stratum-2 headline honestly deferred).** The `Analysis/Transform/PassManager` "LLVM shape"
  that names the family does not land in Stratum 1 at all; Stratum 1 is "delete a redundant tree."
  Scope-truth to carry, not a defect.

**Pass-2 directives.**
1. **RUN G1** on live: full collapse + all six regen/check byte-identical. This is the one falsifiable
   gate that earns the family; run it or demote 3B to "structurally-motivated, unproven."
2. Prove the `materialize_rule` emit-order framing is faithfully recomputable inside the
   `(GrammarIr, Attributes)` fold (the specific obstacle PROTO-1 named).
3. **Assign the seam-dissolution here (not to 3C):** show the collapse **retires** `has_rule("value")`
   + the role classifier. (See redirect below — the gestalt must not fund both routes.)
4. Confirm per-`RuleId` suffices before committing the coarse key.
5. Execute the net-LOC-drop measurement and the 5→2 extensibility count (both *identified, not
   executed* in PROTO-1).

**Withheld from researchers:** do **not** build any `PassManager`/`PreservedAnalyses` invalidation
machinery — it is consumer-less until a mutating Transform exists (Stratum 2 only).

---

## FAMILY 3C — GRAMMAR-HOMOIOUSIA · BLOCKED · 30%

**Correctly the honest disposition.** Three externally-owned blockers, all confirmed on live:
(1) `DirectTypeKind::Enum` — the schema IR is **Struct-only** at `direct_schema.rs:52`, so the sum
node algebra (`ExprKind` 10-arm, `BackendExpr` 13-arm) is **un-emittable** (LOCK-1/GN-3 W1 deliverable);
(2) LOCK-5's **undecided** ONE-HIR/LAWFUL-FUNCTOR verdict, which the `ir.bbnf` functor must be
compatible with (3C-here = 5A-there, one claim from two seats — Constitution ii, *consume the verdict,
never define it*); (3) **no GN-wave consumes an IR grammar today** (Y.13 consumer-less-substrate).
Each blocker is bounded and named → **BLOCKED, not RETIRED.** Bank the gift: 3C is the sternest possible
consumer for the type plane and the bootstrap gate.

**Open gaps.**
- **G-3C-1 (zero pass-1 artifact).** Even the product-only scoping probe (emit `Rule`/`Expr`/
  `SourceSpan`/`DirectBuild*` byte-identical) was not run. The family is fully SPEC-ONLY this pass and
  must wear the label (Constitution vii).
- **G-3C-2 (the runnable part ≠ the family).** The product-only demo **deliberately excludes the sum**
  — the part that *is* the family — to dodge blocker (1). Elegant-reduction trap by construction.
- **G-3C-3 (consumer-less authoring risk).** Proposing to author `grammar/ir/*.bbnf` + point the
  emitter at it, **before a consuming wave is named**, is itself the Y.13 violation. Artifact-creation
  must be gated on a named consumer, not merely acknowledged.

**Pass-2 directives.**
1. Hold BLOCKED. **Do not author `ir.bbnf`** until (a) LOCK-1/GN-3 W1 lands `DirectTypeKind::Enum` and
   (b) a consuming GN-wave is named. Track both as born-RED dependency edges.
2. Consume LOCK-5's HIR-functor verdict when it lands; do not pre-empt it.
3. *Only if* a consumer is named: run the product-only G1 emit-byte-identity + `regen∘regen` fixpoint
   probe as a **scoping probe** (proves the emitter is expressive enough for compiler-internal ADTs),
   explicitly labeling that it excludes the sum.

**Withheld from researchers:** do **not** re-derive the three blockers (settled); do **not** propose
authoring the IR meta-grammar as pass-2 mechanism (consumer-less-forbidden); the cost/analysis
substrate (`ir/cost.rs`) stays hand-written primitives — do not god-modularize it into the meta-grammar.

---

## FAMILY 3D — STAGED-RESIDUAL · BANKED · 38%

**Why BANKED, not ADVANCED (FAMILY-SPECS graded this pre-prototype at 03:48; PROTO-2 at 04:04 and the
live tree refute it).** The stage mechanism **is architecturally a 3B move** — `Stage` is one more
attribute slot on the single IR; the residualizer is a split leaf emitter, not a transposition. It
therefore **converges onto the 3B substrate** (the convergent spine already reads "…+ a stage
attribute with residualization"). Per the redirect rule, the separate-staging-spine formulation is
**BANKED** and its subsumed stage-attribute lands as part of 3B's `Attributes` store. Its distinctive
content is unproven and its flagship refuted, so it is not funded as an independent spine.

**RETIRED sub-verdict (named negative).** The **PROVE-12-`u128`-const-generic byte-identity flagship
(P1)** is retired: byte-identity FAILS structurally on the stale baseline, **and** its target
`find_component_delim` is **retired from the live emit path** — the gate cannot pass on either tree. A
gate whose concrete artifact no longer exists is a Constitution-vii violation.

**Open gaps.**
- **G-3D-1 (unlanded on the real tree; tests vacuous).** `binding_times`/`Stage` do not exist in live
  `crates/ir/`. The 4/4 tests + `residual_islands=4` + `stage_leaks=0` ran on a **synthetic GrammarIr
  mirror** shaped to succeed — a gate-that-cannot-fail; vacuous w.r.t. the real spine.
- **G-3D-2 (consumer-less on the real tree).** The only would-be artifact (`binding_times`) has no
  consumer on live — STEP 2 (the residualizer) was structurally refuted and not run (Y.13).
- **G-3D-3 (the distinctive contribution is unproven).** The real differentiator over "3B + a const
  table" is (i) `Stage` as a typed IR fact and (ii) the strong unification "type-plane-erasure =
  stage-0-erasure applied to the type sub-plane." Nobody threaded `Project.ty`@204 as an S0 residual
  this pass; the unification is SPEC-only.
- **G-3D-4 (surviving claim is partly already-done-and-unnamed).** The weaker live claim
  ("grammar-derived stop-set is a BTA-liftable S0 island") is real — but the live tree **already
  ships it** as `ScanAlphabet::derive_alphabet` → `const … : &[u8]` feeding a SIMD scan, under a
  different name and a different residual (`const &[u8]`, not `u128`).

**Pass-2 directives (all under the banked banner; reactivate to ADVANCED only if the unification is
proven concretely).**
1. **Re-cut the thesis to the live residual.** Re-specify the residual-lift against
   `ScanAlphabet`/`css_scan_direct` (`const &[u8]`), **not** the retired `find_component_delim`
   `u128`. State the concrete byte-target on live.
2. **Open the underexplored formulation:** prove the **strong unification concretely** by threading
   `Project.ty`@204 as an S0 residual to a Rust *type* — the un-done half that would make "type-plane
   = stage-0" more than an assertion. This is the one 3D contribution not already shipped as
   `ScanAlphabet`.
3. Run `binding_times` on the **real** CSS/JSON IR (not a synthetic mirror); prove the delimiter
   island is an S0 node the live emit path actually exposes.
4. If (1)–(3) land: measure a NEW-site cold-**paired-PMU** win (JSON first-byte dispatch), floor
   −3.7…−6.4% ins/B (3D is the family most prone to unmeasured plausibility — PROVE-09 law).

**Withheld from researchers:** the PROVE-12-`u128`-byte-identity claim is **RETIRED** — do not re-pose
it; do **not** re-fund the CSS template→IR emitter under a 3D banner (**it is already landed on live as
`css_scan_direct`** — declare the dependency, do not re-author it); per-site staging-form scheduling is
**LOCK-2's** seat (gated on a real consumer), not a LOCK-3 hardcoded emitter branch.

---

## REDIRECT / CROWDING DECISIONS (this pass)

1. **3A-tower ⊗ 3B-single dissolves into the hybrid.** The lock's one hard incompatibility (distinct
   typed levels vs one IR) is resolved by PROTO-1 toward **one IR + per-pass contracts**. Redundant
   formulation banked: 3A-as-standalone-typed-tower. Advanced: 3A-as-discipline + 3B-as-substrate,
   composing.
2. **3D's separate-staging-spine → BANKED into 3B's attribute store.** `Stage` is an attribute, not a
   level; both critics + FAMILY-SPECS agree residualization is "not a level." The subsumed stage
   attribute lands with 3B; 3D's unique unification thesis is re-opened scoped to live.
3. **The seam-dissolution is single-funded to 3B.** `derive_materialization_roles` +
   `has_rule("value")`@98 are the **same live hardcodes** 3C would "dissolve via grammar" and 3B would
   "retire via fold." Two routes at one seam; the gestalt funds **one**. 3B (ADVANCED) owns it; 3C's
   grammar-dissolution is the banked alternative that reactivates only if 3C unblocks.

## CROSS-POLLINATION (the one named, justified graft this pass)

**3A's refinement-oracle organ ⟶ 3B's `(GrammarIr, Attributes)` fold.** Both families are developed
enough to name the graft: 3B has the collapse mapped (deletion targets + attribute store), 3A ran the
per-pass predicate (probe b). The graft: each analysis/emit pass over the single IR carries a
**checkable output contract** — a `verify(grammar, attrs)` predicate + a per-edge differential digest —
so the discipline of a nanopass tower is recovered **without a second tree**. This *is* the convergent
spine's "one IR (3B) + per-pass contracts (3A discipline)."
*Deferred graft (not this pass):* 3D's residual-count oracle is a 3A-style refinement oracle, but 3D is
BANKED and its oracle ran only on a synthetic mirror — re-evaluate the graft only after the 3D re-cut
(directive 3D-3) lands on real IR.

## CROSS-LOCK / GESTALT DIRECTIVES

1. **`Project.ty`@204 is QUADRUPLE-homed and undischarged → make it a single-owner born-RED gate.**
   3A verifies it, 3B threads it as `attrs.node_type`, 3C's product, 3D erases it as S0 — all four
   claim the same two lines, and **pass-1 discharged it in none** (PROTO-1 mis-probed it; the others
   didn't touch it). Assign the thread to **exactly one owner** (it is a GN-3 wave, `no-value-discard`);
   the other three **consume** the threaded value. Do not fund it four ways.
2. **Re-pin every pass-2 prototype to live `skinny@b3cb8c16b`** (or a named, justified frozen tree).
   Non-negotiable: pass-1's two "sharpest uncertainty" prototypes tested a 51-commit-stale tree and a
   mis-identified node — the convergence numbers stand on the wrong artifact until re-run.
3. **Keep the decision-driver seat unmerged from LOCK-2.** LOCK-3 owns *where* the driver sits (fact
   contract at the L1→L2 edge); LOCK-2 owns *what engine*. 3A respects this; 3D blurred it (P3 handed
   staging-form to LOCK-2 with no live consumer) — keep the staging-form CSP LOCK-2's, gated on a real
   consumer.
4. **`no-ts-ir` holds** across 3C/3D — the meta-grammar emits TS *type declarations*, never a runtime
   TS IR; the backend-agnostic middle is tape + schema (PROVE-14), not a shared instruction IR.

---

## PASS HISTORY

### Pass 1 (folded by AGGLOMERATOR, Fable-tier)

- **Inputs:** research-{A,B,C,D,priorart}, FAMILY-SPECS, PROTO-1 (level-count adjudication, RAN_MEASURED),
  PROTO-2 (PROVE-12 corollary, RAN_UNMEASURED/REFUTED), CRIT-1 (within-lock), CRIT-2 (cross-lock).
- **Agglomerator verification (live `b3cb8c16b`):** confirmed both critics' crux facts — 203=`Annotation`
  (non-defect) / 204=`Project` ty-drop; 51-commit staleness of the PROTO baseline; CSS IR-driven via
  `css_scan_direct`/`ScanAlphabet` (`CSS_GENERATED_RS` gone); `find_component_delim` retired from
  `codegen/src`; `binding_times`/`Stage` absent from live `ir/`; `has_rule("value")`@98 survives;
  `DirectTypeKind` Struct-only. **The critics did not err; FAMILY-SPECS §0 over-graded on stale/mis-probed
  evidence.** Floor taken unadjusted.
- **Dispositions set:** 3A ADVANCED-as-discipline (tower formulation BANKED) 55%; 3B ADVANCED-substrate 50%;
  3C BLOCKED 30%; 3D BANKED (PROVE-12-`u128` gate RETIRED) 38%.
- **Redirects:** 3A-tower⊗3B dissolved to hybrid; 3D-staging-spine banked into 3B attribute store;
  seam-dissolution single-funded to 3B. Cross-pollination: 3A refinement-oracle ⟶ 3B attributes fold.
- **Convergent-spine hypothesis (architecturally sound, rests on three unrun/refuted gates):** one IR
  (3B) + per-pass contracts (3A discipline) + a stage attribute (3D erasure), **3C banked** until
  LOCK-1 (sums) + LOCK-5 (functor) + a named wave unblock it. The gating gates: 3B-G1 byte-identity
  collapse (UNRUN), the 3A refinement oracle + `Project`@204 re-anchor (UNRUN/mis-run), the 3D live
  re-cut (REFUTED-as-posed).
- **Lock-wide:** no family convergence-eligible (pass 1 of ≥3; the baseline-staleness defect alone bars
  all four this pass).

<!-- Pass 2 appends below this line -->
