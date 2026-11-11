# LOCK-3 — pass-1 · CRIT-1 (Adversarial Critic 1 of 2 — the WITHIN-LOCK view)

*Weighting: internal soundness of each family spec, and whether the two prototypes actually
tested the sharpest uncertainty. Every claim below is path:line-verified against the live tree
this pass. Convergence %s are pass-1 figures on a ≥3-pass loop; none can approach 100% (that
needs zero gaps + fresh audit + two consecutive clean passes).*

---

## THE HEADLINE DEFECT — both prototypes ran against a STALE baseline (systematic)

FAMILY-SPECS §0 declares its anchors "path:line verified **this pass**" and describes the main
tree: `ExprKind` a 10-arm enum with `Project { body, target, ty }` (`skinny/crates/ir/src/lib.rs:261`)
and the `->` ty-drop at "passes/src/lib.rs:203". **Both PROTO-1 and PROTO-2 ran against
`d2b32f6e3` — an OLDER ANCESTOR of main HEAD `b3cb8c16b`** (`git merge-base --is-ancestor` =
YES). That ancestor materially diverges from the spec baseline on the *exact two points under
test*:

1. **PROTO-1's node does not exist in the spec baseline.** In `d2b32f6e` there is NO
   `ExprKind::Project` (grep: only `ValueProject` at ir:508); the `->` node there is
   `ExprKind::Annotation`. So PROTO-1 wired its flagship contract over **`Annotation { value:
   Some(_) }`** nodes. But in the main tree the two are DISTINCT arms:
   - `passes/src/lib.rs:203` = `ExprKind::Annotation { .. } => Type::Builtin(BuiltinTy::Unit)` —
     **correct behaviour** (annotations are `@cap`/`@ws` directives, legitimately untyped).
   - `passes/src/lib.rs:204` = `ExprKind::Project { body, .. } => self.infer_expr(*body)?` —
     **the real ty-drop** (the `ty` field is destructured away as `..`).

   FAMILY-SPECS §0 mis-cites: it calls line **203** the `Project` defect; 203 is the `Annotation`
   arm, the `Project` defect is at **204**. So the "flagship L1 contract turns RED on HEAD because
   passes:203 drops the type" non-vacuity proof rests on a conflation — the contract PROTO-1 wired
   (over `Annotation`) would **not fire on the real `Project` defect** in the main tree, and
   `Annotation→Unit` is not a defect at all. The probe's *methodological* verdict (a per-pass
   predicate on one IR localizes a dropped type to its edge) survives baseline-independently, but
   the *specific* flagship-contract / non-vacuity anchor (Gate G1) was never exercised against the
   node it names.

2. **PROTO-2 refuted PROVE-12 on a tree that no longer reflects the spine.** PROTO-2 found the CSS
   recognizer is a raw-string template in `d2b32f6e`, so the delimiter set is not an IR node the
   BTA can lift — hence "byte-identity fails structurally." But the **main tree already replaced
   that template with the IR-driven `css_scan` path** (`skinny/crates/codegen/src/lower/css_scan.rs`,
   `css_scan_direct/`), which PROTO-2's own "constructive nuance" concedes. `find_component_delim`
   is retired from the emit path (main tree: it survives only in `runtime/src/runtime_scan.rs` +
   a bench). So PROTO-2 refuted PROVE-12-byte-identity against a baseline the campaign has already
   superseded — a refutation of a fossil.

**Consequence for the lock:** the two "sharpest uncertainty" prototypes did not test the sharpest
uncertainty *on the baseline the spec verifies*. This is prototype-evidence-misuse of the
SPEC_ONLY-vs-concrete kind — running numbers exist, but not against the artifact under design.
Every pass-2 re-run must pin `skinny@b3cb8c16b` (main HEAD) and re-key its anchors, or the
convergence claims stand on the wrong tree. This defect caps every family's pass-1 convergence.

---

## FAMILY 3A — CONTRACT-TOWER

**Internal soundness: mostly sound as a discipline; the standalone-tower disposition is correct
(CONTESTED); non-vacuity anchor is compromised by the baseline defect.**

- **What holds.** The incompatibility map's resolution is sound: PROTO-1 probe (b) demonstrated a
  per-pass `verify(arena, attrs)` predicate localizes a dropped type to its edge with `cargo build`
  green, and probe (c) proved zero emit footprint (four regen checks byte-identical on/off,
  `runtime/src/grammars/` git-clean). The "3A-as-persisted-typed-tower is not required for
  localization" verdict is earned. Constitution v (type plane costs zero) is affirmatively tested
  for the contract organ.
- **GAP-3A-1 (non-vacuity anchor on wrong node).** The flagship contract was validated over
  `Annotation` nodes on `d2b32f6e`, not `Project` nodes on main HEAD. In the main tree the
  contract as written is green-on-a-non-defect. Re-instantiate over `ExprKind::Project` (ty
  destructured at passes:204) and re-prove RED-on-HEAD / GREEN-after-thread. Until then G1 is
  unverified against its named node.
- **GAP-3A-2 (Q1 — the level count was never enumerated).** The charter says "number of levels
  measured, not decreed." PROTO-1 *confirmed* the honest chain has a 4th edge (`materialize_rule`
  synthesizes `SpanMark/TapeEmit/DirectBuild/Return` framing with no `ExprKind` counterpart;
  `SinkOnlyProgram` is a real level with no verifier), yet the promised **edge enumeration** that
  earns "three or four" was not performed. The count remains decreed, not measured — the family's
  own sharpest Q is open.
- **GAP-3A-3 (refinement oracle is corpus-bounded, coverage metric undefined).** Q3 concedes
  constructs with zero corpus witnesses are unverified, and the spec makes "coverage a first-class
  metric" — but no metric is defined, thresholded, or gated. The refinement/translation-validation
  organ is spec-only (no PROVE ran it; PROTO-1 tested only the well-formedness organ (b)).
- **GAP-3A-4 (panic-discharge unproven).** "The four `extract` panics become upstream-discharged or
  typed violations, so no panic is reachable from a well-formed input" is asserted, not shown; no
  proof that well-formedness ⇒ panic-unreachable.

**Convergence: 58%.** Discipline earned on one organ; tower-vs-single resolved toward hybrid; but
the non-vacuity anchor is on the wrong node, the level count is unmeasured, and two of three organs
(refinement, panic-discharge) are spec-only.

---

## FAMILY 3B — SINGLE-IR-MULTIPASS

**Internal soundness: the strongest structural argument, but its KILLER gate is UNRUN and its
literal thesis is refuted by its own probe.**

- **GAP-3B-1 (G1, the killer gate, NOT RUN — the elegant-reduction trap).** The load-bearing claim
  is "delete `BackendIr`/`BackendExpr`, re-express emission as one `emit(&GrammarIr,&Attributes)`
  fold, and all four regen checks byte-identical." PROTO-1 probe (a) **did not run** it ("beyond the
  cap"). The elegant collapse is asserted; the hard byte-identity proof is deferred. This is exactly
  the elegant-reduction trap: the load-bearing step reads "collapse the IR *and then* prove
  byte-identity," and the second half is unconverged.
- **GAP-3B-2 ("second IR is pure redundancy" refuted as literally stated).** PROTO-1's data-flow
  verdict: `materialize_rule` synthesizes emit-order framing (`SpanMark`/`TapeEmit`/`DirectBuild`/
  `Return`) that **has no counterpart in `ExprKind`** — so the second IR is "not vacuous as a data
  structure; it carries a real emit-order fact." The spec's G1 premise ("if the fold reproduces
  every live generated.rs, the second IR was pure redundancy") is therefore false as stated; the
  correct, weaker claim is "tree-redundant but computation-essential — recomputable inside one
  fold," and *that* recomputation's byte-identity is the unrun proof. The disposition
  "ADVANCED, strongest structural claim" over-reads a probe that returned PARTIAL/refuted-as-stated.
- **GAP-3B-3 (Q2 — per-`ExprId` vs per-`RuleId` key unresolved).** The spec commits to a coarse
  per-`RuleId` materialization key "before confirming no fleet grammar needs per-`ExprId`." That
  confirmation was not run; committing the coarse key without it risks the exact `TapeEmit{kind}`
  collapse MLIR warns of that 3B claims to *close*.
- **GAP-3B-4 (Stratum-2 is honestly consumer-gated — carry it explicitly).** The `Analysis/Transform/
  PassManager` split correctly waits for a mutating consumer (Y.13-clean), but that means the
  "LLVM-shape" that names the family does not land in Stratum 1 at all; Stratum 1 is "delete a
  redundant tree." The family's headline (in-place refinement by annotating passes) is deferred to
  a wave that does not yet exist. Not a defect — a scope-truth the registry must carry.

**Convergence: 55%.** The structural intuition is compelling and the incompatibility resolution is
sound, but the single falsifiable gate that would earn it is unrun, and the literal "pure
redundancy" thesis is already refuted by the probe meant to support it.

---

## FAMILY 3C — GRAMMAR-HOMOIOUSIA

**Internal soundness: honestly BLOCKED-candidate; the blockers are correctly named; but even the
product-only scoping probe was not run, and one blocker is under-stated.**

- **What holds.** The three externally-owned blockers are real and correctly attributed:
  `DirectTypeKind::Enum` (sum emission — LOCK-1/GN-3 W1), LOCK-5's undecided HIR-functor verdict
  (Constitution ii — 3C-here and 5A-there are one claim), and the absent consuming GN-wave
  (Y.13/viii). W4 (cost substrate stays hand-written primitives) correctly refuses meta-grammar
  over-reach. The disposition BLOCKED-candidate (bank, don't retire) is right.
- **GAP-3C-1 (the scoping probe G1 was not executed).** The one thing 3C could have run this pass —
  "`ir.bbnf` + emitter regenerates the product-only nodes byte-identical" — was not run. So even the
  product-only claim is spec-only; the family has *zero* concrete artifact this pass. Per
  Constitution vii that is the SPEC-ONLY label, and it must be worn.
- **GAP-3C-2 (the demo excludes the family — acknowledged, unresolved).** The spec concedes "the
  product-only demo deliberately excludes the sum — i.e. the part that *is* the family." An
  elegant-reduction trap by construction: the runnable part is not the load-bearing part. Nothing
  advances 3C within the lock; it is a pure dependency-wait.
- **GAP-3C-3 (consumer-less-substrate risk under-stated).** W3 names "no GN-wave consumes an IR
  grammar today," but the spec still proposes authoring `grammar/ir/*.bbnf` + pointing the emitter
  at it as pass-1 mechanism. Building the substrate before the consumer wave is named is precisely
  the Y.13 violation; the spec must gate artifact-creation on a named consumer, not merely
  acknowledge the gap.

**Convergence: 45%.** Well-characterized, correctly banked, blockers sound — but every claim is
spec-only, the runnable probe was skipped, and the runnable part is definitionally not the family.

---

## FAMILY 3D — STAGED-RESIDUAL

**Internal soundness: the pass-1 disposition ("ADVANCED, most-evidenced synthesis vein") is
CONTRADICTED by its own prototype. This is the largest claim/evidence gap in the lock.**

- **GAP-3D-1 (P1, the load-bearing gate, REFUTED — disposition must be reconciled).** FAMILY-SPECS
  makes P1 the SPEC-only/graduation line: "until PROVE-12 byte-identity is green, 3D is SPEC-only."
  PROTO-2's verdict is **REFUTED (named negative)**: `binding_times` + the split emitter cannot
  reproduce PROVE-12's const-generic residual — byte-identity fails structurally, checksum N/A
  (never reached). By the family's *own* gate, 3D is **SPEC-only**, not "most-evidenced ADVANCED."
  The disposition (written 03:48, before PROTO-2 at 04:04) is now falsified and must be downgraded.
  The "PROVE-12 as corollary / strongest cross-family synthesis in the portfolio" framing is refuted
  as posed.
- **GAP-3D-2 (vacuous convergence — the 4/4 tests pass on a SYNTHETIC mirror).** `binding_times`'
  "4 passed" and `residual_islands = 4` were measured on a **hand-built GrammarIr mirroring the CSS
  scan**, not on the real CSS grammar (which — on `d2b32f6e` — never enters `GrammarIr`). The
  passing tests are vacuous w.r.t. the real spine: they prove the fixpoint runs on a toy shaped to
  succeed, an unverified-gestalt / gate-that-cannot-fail. P4's "residual-count oracle" is likewise a
  synthetic-mirror count.
- **GAP-3D-3 (consumer-less substrate on the real tree).** The only landed artifact is
  `binding_times` (+ `residual_islands`/`stage_leaks`). STEP 2 — the residualizer that would consume
  it — was structurally refuted and not run. So on the real spine `binding_times` has **no
  consumer** (Y.13/viii). It is exactly the consumer-less substrate the constitution forbids, until
  a residualizer that reads the real IR exists.
- **GAP-3D-4 (the weaker, live claim is real but the family must be re-scoped to it).** PROTO-2's
  constructive nuance is the salvageable core: the main tree's `ScanAlphabet::derive_alphabet` DOES
  lift the delimiter set as a grammar-derived S0 island — but to a `const &[u8]`, feeding a SIMD
  scan, **not** PROVE-12's `u128` const-generic mask. So 3D's residual-lift is real in a weaker
  form; the family's thesis must be re-cut to "grammar-derived stop-set → `const &[u8]` island" and
  the PROVE-12-`u128`-byte-identity claim retired as a named negative. Re-run against main HEAD
  (`b3cb8c16b`) where `css_scan` is IR-driven, not against the `d2b32f6e` template fossil.
- **GAP-3D-5 (the strong unification is unproven).** "Stage-0 erasure subsumes type-plane-cost-zero;
  3D subsumes GN-3 and PROVE-12 as two erasures of one stage" is the portfolio's strongest synthesis
  claim — and it rides entirely on P1, which is refuted. Until a residualizer erases a real S0 type
  island on the real IR, the unification is SPEC-only.

**Convergence: 40%.** The BTA machinery is landed and monotone-correct, and the weaker live claim is
genuinely corroborated — but the disposition is contradicted by the prototype, the passing tests are
vacuous (synthetic mirror), the artifact is consumer-less on the real tree, and the load-bearing
unification is unproven.

---

## THE INCOMPATIBILITY MAP — assessment

**Sound, and the strongest part of the synthesis.** The single hard incompatibility (3A-tower ⊗
3B-single on the level-count axis) is correctly identified, and PROTO-1 probe (b) genuinely
resolves it toward the hybrid (one IR + per-pass contracts). The compatible-layering claims
(3B⊕3D, 3B⊕3C, 3A⊕3C=nanopass, 3A⊕3D) are architecturally correct. Two residual gaps:

- **GAP-MAP-1 (the level count itself is still unadjudicated).** The map says the whole picture is
  gated on "3A-tower vs 3B-single," and PROTO-1 resolved *localization* (contracts need not hang on
  typed levels) — but NOT the *count* (3 vs 4 edges). PROTO-1 confirmed a real 4th edge exists
  (`SinkOnlyProgram`, `materialize_rule` framing). So "one IR" (3B) is not yet proven to be the
  achievable count; the byte-identity collapse (GAP-3B-1) is what would prove it, and it is unrun.
- **GAP-MAP-2 (the 3C↔3D type-ownership coordination is named but the seam is the dropped node).**
  The map notes 3C and 3D "must agree that `Project.ty` is threaded exactly once." In the main tree
  `Project.ty` is *dropped* at passes:204 — the coordination seam is currently a hole in both
  families' foundation, and neither prototype touched the real `Project` node.

---

## CONSTITUTION AUDIT (binding clauses)

- **(v) type plane costs zero** — affirmatively verified for the 3A contract organ (PROTO-1 (c)).
  For 3D the stronger "stage-0 erasure subsumes it" is unproven (GAP-3D-5).
- **(vii) concrete artifacts or SPEC-ONLY** — 3C is fully SPEC-only (probe skipped) and must wear
  the label; 3D's disposition claims "most-evidenced" while its load-bearing gate is REFUTED — a
  vii violation of the "no vague optimism" clause that CRIT flags for correction.
- **(viii) consumer-less substrate forbidden** — 3D's `binding_times` is consumer-less on the real
  tree (GAP-3D-3); 3C's proposed `ir.bbnf` authoring is consumer-less until a wave is named
  (GAP-3C-3); 3B correctly gates Stratum-2 on a consumer.
- **(ix) extant-tranche consciousness** — respected; divergences from the GN codex are framed as
  supersessions.
- **Anchor accuracy (vii-adjacent)** — FAMILY-SPECS §0's "verified this pass" is inaccurate: the
  Project defect is at passes:**204** not 203, and both prototypes verified a *different* tree
  (`d2b32f6e`) than §0 describes (`b3cb8c16b`). The "confirmed verbatim this pass" claim does not
  hold for the cited line.

---

## NEXT-PASS DIRECTIVES (exact, ordered)

1. **Re-pin every prototype to `skinny@b3cb8c16b` (main HEAD).** Re-run PROTO-1's contract over
   `ExprKind::Project` (ty at passes:204), and PROTO-2's residual-lift against the IR-driven
   `css_scan` path, not the `d2b32f6e` template. Non-negotiable — pass-1 evidence is on the wrong
   tree.
2. **Run 3B's G1 byte-identity collapse** (the one falsifiable claim that earns the family) or
   demote 3B from "strongest structural" to "structurally-motivated, unproven."
3. **Downgrade 3D's disposition** to reconcile with PROTO-2 REFUTED; re-scope its thesis to the
   `const &[u8]` island (GAP-3D-4) and retire the PROVE-12-`u128`-byte-identity claim as a named
   negative.
4. **Perform the level-count edge enumeration** (3A Q1 / GAP-MAP-1) — measure 3 vs 4, do not decree.
5. **Run 3C's product-only scoping probe** or mark 3C explicitly SPEC-only with zero pass-1
   artifact.

---

## CONVERGENCE SUMMARY

| Family | Convergence | Blocking gaps |
|---|---|---|
| **3A** | **58%** | wrong-node non-vacuity anchor; unmeasured level count; refinement + panic-discharge organs spec-only |
| **3B** | **55%** | killer byte-identity gate UNRUN (elegant-reduction trap); "pure redundancy" refuted as stated; ExprId-vs-RuleId key unconfirmed; Stratum-2 headline deferred |
| **3C** | **45%** | three externals (Enum/functor/wave) + scoping probe not run + runnable part ≠ the family + consumer-less-authoring risk |
| **3D** | **40%** | disposition CONTRADICTED by PROTO-2 REFUTED; tests vacuous (synthetic mirror); consumer-less on real tree; unification unproven; stale baseline |

*Lock-wide: no family is convergence-eligible (pass 1 of ≥3; the baseline-staleness defect alone
bars all four). The convergent-spine hypothesis — one IR (3B) + per-pass contracts (3A) + stage
attribute (3D), 3C banked — is architecturally sound but rests on three unrun/refuted gates
(3B-G1 collapse, 3D-P1 residual, the level-count enumeration).*
