# LOCK-3 — THE IR TRANSPOSITION SPINE · pass-1 · CRIT-2 (adversarial, cross-lock weight)

*Critic 2 of 2. I ground every verdict in the live tree (`skinny@b3cb8c16b`), not the
synthesis's paraphrase or the prototypes' checkout. Two of the pass's load-bearing artifacts —
FAMILY-SPECS §0 and both PROTO runs — are anchored to code that does not match live, and the
mismatch changes three of the four dispositions. Convergence numbers are earned against that,
weighted toward cross-lock double-homing per the critic-2 charter.*

---

## 0. TWO CROSS-CUTTING DEFECTS THAT MOVE THE WHOLE LOCK

### D1 — THE PROTOTYPES RAN 51 COMMITS STALE (measured, not asserted)

Both PROTO-1 and PROTO-2 checked out worktree HEAD `d2b32f6e3` ("exactly the PROVE-12
baseline, sk-v22 graduation runtime"). Live HEAD is `b3cb8c16b`. Verified:
`git rev-list --count d2b32f6e3..HEAD` = **51**. `d2b32f6e` *is* an ancestor of live, so the
prototypes tested a tree fifty-one commits behind the tree the research reports and FAMILY-SPECS
§0 cite as "HEAD, path:line verified this pass." This is not a nit — it inverts a verdict:

- **PROTO-2's headline REFUTED rests on a premise that is FALSE on live.** Its finding #1 —
  "the CSS recognizer is a raw-string template (`CSS_GENERATED_RS`), not IR-driven, so the
  delimiter set is not an IR node the BTA can lift" — is stale. On live, `grep CSS_GENERATED_RS`
  returns only `css_scan_direct/mod.rs:13` *"`CSS_GENERATED_RS` is gone"*. The CSS path is now
  IR-driven through `css_scan_direct/` + `lower/css_scan.rs::ScanAlphabet::derive_alphabet`,
  emitting grammar-derived `const AT_RULE_STOP: &[u8]`. The "whole-subsystem rewrite, out of
  scope" that PROTO-2 says blocks 3D **is already landed on live.** PROTO-2's own "constructive
  nuance" admits this, then keeps the REFUTED verdict line anyway — a reader consuming the
  verdict gets the wrong forward picture.
- **PROVE-12's patch target is RETIRED on live.** `find_component_delim` survives on live only
  in a bench doc-comment (`css_track1_profile.rs:4`) and a runtime doc-comment
  (`runtime_scan.rs:92`); the function itself is gone from the emit/runtime path. So 3D's
  flagship gate P1 ("`regen-css` byte-identical to `PROVE-12/patch.diff`, `find_component_delim
  <const DELIMS>` emitted not authored") **cannot pass on either tree**: absent on the stale
  baseline (template), absent on live (retired). A gate whose concrete artifact no longer exists
  is a constitution-vii violation (SPEC-ONLY masquerading as a runnable gate).

Consequence: every pass-2 prototype MUST run against live `b3cb8c16b` (or a named, justified
frozen tree), and 3D's gate must be re-specified against `ScanAlphabet`/`css_scan_direct`, not
the retired `find_component_delim`.

### D2 — THE FLAGSHIP CONTRACT WAS PROBED ON THE WRONG NODE

The single most load-bearing claim in the lock is the *non-vacuity* of the
typed-materialization-invariant contract — the proof that a per-pass check catches the dropped
`->` type while `cargo build` stays green. FAMILY-SPECS §0 states it "confirmed verbatim this
pass": *"`infer_expr(Project) => self.infer_expr(*body)` at `passes/src/lib.rs:203`."* Live code
(verified):

```
203:  ExprKind::Annotation { .. } => Type::Builtin(BuiltinTy::Unit),
204:  ExprKind::Project { body, .. } => self.infer_expr(*body)?,
```

The `Project` arm — the actual `->` carrier (`grammar/src/lib.rs:1482` `parse_projection` builds
`ExprKind::Project { ty: scan.type_text }`) — is at **204**, not 203. Line 203 is `Annotation`
(the `@name value` directive node), which returns `Unit`. These are *different nodes*.

**PROTO-1 wired its contract against the wrong one.** Its `verify()` "asserts every
`ExprKind::Annotation` with a declared type carries a populated, non-`Unit` `node_type`," and its
mapping table declares `Annotation` to be "the `->` projection/directive node." It is not — `->`
is `Project`. `Annotation`→`Unit` is arguably *correct* (a `@ws`/`@cap` directive has no
projected value type). So PROTO-1's "RED on HEAD" — the pass's entire non-vacuity proof — may be
RED for a non-defect, while the *real* defect (`Project.ty` dropped at 204) went unexercised by
the wired contract. The mechanism (per-pass predicate localizes to the infer edge, zero runtime
footprint) is sound and genuinely RAN/MEASURED; but the *demonstration that the contract catches
a real load-bearing defect* is not yet valid and must be re-run against `Project`@204.

This is an unverified-gestalt failure: FAMILY-SPECS, PROTO-1, and the research reports each name
"the `passes:203` `Project` drop" and none of them noticed that at 203 sits `Annotation`, at 204
sits `Project`, and the prototype asserted over the former.

---

## FAMILY 3A — CONTRACT-TOWER · convergence 55%

**What is earned.** PROTO-1 genuinely RAN two of the three organs: (b) a per-pass contract on one
IR localizes a dropped type to the exact infer edge with `cargo build` green; (c) zero emit
footprint (regen byte-identical, verifier on/off). The prior-art spine (GHC Core-Lint, MLIR
`--verify-each`, rustc `MirPhase` ladder, translation validation) is correctly cited and the
`ContractViolation`-not-`Err(String)` (R4) retyping is real prerequisite work. As a *discipline
layered on one IR*, 3A converges.

**What is not.**
- **The non-vacuity proof is invalid as run (D2).** Re-run against `Project`@204 before any
  "flagship RED on HEAD" claim stands. Until then the family's central falsifiable is unmet.
- **Probe (c) is close to a gate-that-cannot-fail.** A `verify()` that only asserts and returns,
  by construction, cannot change emitted `.rs`; "byte-identical on/off" is near-tautological. The
  zero-runtime-footprint law is real, but this probe does not test it non-trivially — a
  refinement oracle that folds over emitted output (P2) would.
- **The level-count question (Q1: 3 or 4) is unresolved and PROTO-1 supplies counter-evidence
  against "three."** PROTO-1 found `materialize_rule` synthesizes emit-order framing
  (`SpanMark/TapeEmit/DirectBuild/Return`) with *no `ExprKind` counterpart* — a real fourth-edge
  fact. The edge-DAG enumeration (P3) was not run; "three levels" remains decreed, not measured
  (the family's own R3).
- **As a standalone tower, 3A is refuted for the fleet** (PROTO-1 one-line adjudication) — its
  value survives only as the hybrid. FAMILY-SPECS concedes this ("CONTESTED as tower").

**Enumerated open gaps (3A):**
1. Re-wire the flagship contract against `ExprKind::Project`@204 (the true `->` drop), not
   `Annotation`@203, and re-demonstrate RED-on-HEAD / GREEN-after-thread.
2. Run the per-edge refinement oracle (P2): reproduce PROVE-12 `MATCH` + JSON `track1/track2`
   digests AND detect an injected mis-lowering localized to its edge. Unrun this pass.
3. Settle the edge count empirically (P3 labeled edge-DAG); reconcile the `materialize_rule`
   fourth-edge fact PROTO-1 surfaced.
4. Quantify the refinement oracle's corpus coverage as a first-class metric (R2: zero-witness
   constructs are unverified, not sound).

---

## FAMILY 3B — SINGLE-IR-MULTIPASS · convergence 50%

**What is earned.** The structural argument is the lock's cleanest: the deletion targets exist on
live (`single_plan`@1283, `lower_expr`@1318, `materialize_rule`@1362, `BackendIr/Expr/Rule`,
`BackendShape`) and the attribute store already exists unformalized (`LayoutFacts`, `nullability`
`Vec<bool>` archetype). The two-stratum Y.13 discipline (collapse now; PassManager/invalidation
only *with* a mutating consumer) is exactly right and forbids the consumer-less-substrate trap by
construction.

**What is not — the killer gate is UNRUN and the literal thesis is REFUTED.**
- **G1 (byte-identity after full collapse) was explicitly NOT executed** ("beyond the cap" —
  PROTO-1 probe (a)). This is the *entire* proof of the family. FAMILY-SPECS grades 3B "ADVANCED,
  strongest structural claim … executable today behind a byte-identity gate" — but "executable"
  is not "executed," and the disposition overstates unrun work as done. Classic
  **elegant-reduction trap**: "delete the second IR" reads clean; the load-bearing
  and-then-the-hard-part (rewrite 2 producers + 2 consumers as one fold, *prove* byte-identity) is
  the unconverged step.
- **"The second IR is pure redundancy" is refuted as literally stated.** PROTO-1 found
  `materialize_rule` synthesizes emit-order framing absent from `ExprKind`; the second IR is
  "tree-redundant but computation-essential." The fold must *recompute* that framing
  deterministically and reproduce emit order exactly — plausible, unproven. Until G1 runs, 3B's
  killer claim is a conjecture with a named obstacle.
- **A masked fallback survives untouched: the `has_rule("value")` name-gate**
  (`runtime_generator.rs:98`, live) — a name-gate the constitution's clause vi forbids. No pass-1
  evidence removes it; under a naive 3B collapse it persists inside the fold. 3B must show the
  collapse *retires* the name-gate and `derive_materialization_roles`@1403 heuristic classifier,
  not merely relocate them.

**Enumerated open gaps (3B):**
1. RUN G1 on live: full collapse + `regen-json/check-json/regen-css/check-css/regen-sheets/
   check-sheets` all byte-identical. Unrun.
2. Prove the `materialize_rule` emit-order framing is faithfully recomputable inside the
   `(GrammarIr, Attributes)` fold (the specific obstacle PROTO-1 named).
3. Confirm per-`RuleId` materialization suffices (no fleet grammar needs per-`ExprId`) before
   committing the coarse key (R3).
4. Show the collapse retires the `has_rule("value")` name-gate + byte-literal role classifier,
   not relocates them.
5. Net-LOC-drop and 5→2 extensibility-count *identified but not executed* in PROTO-1; execute.

---

## FAMILY 3C — GRAMMAR-HOMOIOUSIA · convergence 30%

**Correctly BLOCKED-candidate; disposition is the honest one in the pass.** Three externally-owned
blockers are precisely named: `DirectTypeKind::Enum` (LOCK-1/GN-3 W1; live `direct_schema.rs`
Struct-only), LOCK-5's undecided HIR-functor verdict (Constitution ii), and — the Y.13 one — *no
GN-wave consumes an IR grammar today*. Each is bounded, so BLOCKED-candidate over RETIRED is right.

**What CRIT-2 adds.**
- **Zero running-prototype evidence this pass.** Neither PROTO tested 3C; its product-only G1
  (emit the struct nodes byte-identical) was not run. The disposition rests entirely on SPEC
  reasoning + prior art (nanopass, Sysmel 3-cycle fixpoint). Legitimate for a blocked family, but
  the convergence number must reflect that the family's own falsifiable is unexecuted.
- **3C's flagship deliberately excludes the part that IS the family.** Its scoping probe emits
  *product-only* nodes to dodge the sum (`ExprKind`/`BackendExpr` are the sums that matter). A
  demo that excludes the sum node algebra proves the emitter, not the family. FAMILY-SPECS says
  this plainly — I confirm it and hold the convergence low accordingly.
- **Cross-lock note (double-home):** 3C's anti-pattern targets (`derive_materialization_roles`,
  `has_rule("value")`) are the *same* live hardcodes 3B must retire. 3C "dissolves via grammar";
  3B "collapses via fold." They are two routes at one seam — the gestalt must not fund both.

**Enumerated open gaps (3C):**
1. LOCK-1/GN-3 W1 must land `DirectTypeKind::Enum` (hard prerequisite; sum node algebra
   un-emittable without it).
2. LOCK-5 must rule on the ONE-HIR/LAWFUL-FUNCTOR form 3C's `ir.bbnf` must be compatible with
   (3C-here = 5A-there, one claim two seats).
3. Name the consuming GN-wave, or 3C is consumer-less-substrate-forbidden (Y.13). None exists in
   the codex today.
4. Run the product-only G1 emit-byte-identity + `regen∘regen` fixpoint probe (unrun) to prove the
   emitter is expressive enough for compiler-internal ADTs before the grammar move.

---

## FAMILY 3D — STAGED-RESIDUAL · convergence 38%  (FAMILY-SPECS overstates this materially)

**FAMILY-SPECS disposition (03:48, pre-prototype): "ADVANCED, most-evidenced synthesis vein …
none [missing] for the kernel … risk is measurement discipline, not a primitive."** PROTO-2 (04:04)
and the live tree refute the core of that grade.

- **Flagship P1 is REFUTED, and the gate is unsatisfiable (D1).** "PROVE-12 as corollary" —
  reproduce the const-generic `u128` residual byte-identically *from the annotated IR alone* — is
  the claim that IS the family (per research-D §1.3, "the load-bearing claim"). PROTO-2: byte-
  identity FAILS structurally on the baseline, and the target function is retired on live. The
  disposition's "no missing primitive" is wrong: on the stale baseline the missing primitive is
  an IR-driven CSS emitter; on live that primitive *exists* (`css_scan_direct`) but chose a
  different residual (`const &[u8]` → SIMD), so PROVE-12's specific patch is unreproducible for a
  second, independent reason. Either way the flagship does not go green.
- **Prototype-evidence misuse (constitution vii).** The disposition anchors "most-evidenced" on
  "PROVE-12 landed born-verified." But PROVE-12's landed status evidences the *hand patch*, not
  the *emergent-from-annotated-IR* claim that is the family's whole thesis. Using a landed hand
  patch to anchor an ADVANCED grade for the emergence claim is precisely the SPEC_ONLY-anchoring-
  a-claim fallacy the brief names. PROTO-2 makes the distinction concrete and negative.
- **The machinery ran only on a SYNTHETIC mirror.** `binding_times` + `residual_islands` (4) +
  `stage_leaks` (0) passed 4/4 — but on "a synthetic GrammarIr mirroring the CSS component scan,"
  never on the real CSS IR (which, stale-baseline, wasn't in the IR at all). `binding_times`/
  `Stage` do not exist in live `crates/ir/` (verified) — 3D is entirely un-landed.
- **The distinctive contribution is unproven.** 3D's real differentiator over "3B + a const
  table" is (i) the `Stage` attribute making comptime/runtime a typed IR fact and (ii) the
  strong unification "type-plane-erasure = stage-0-erasure applied to the type sub-plane." Neither
  was demonstrated: nobody threaded `Project.ty`@204 as an S0 residual this pass. Without that,
  3D's landed content reduces to "grammar-derived delimiter const," which the live tree
  *already ships* under a different name (`ScanAlphabet`) — i.e. 3D's surviving claim is
  partly already-done-and-unnamed, not a new vein.
- **Double-home with LOCK-2 is aspirational, not seamed.** "3D owns staging semantics, LOCK-2
  owns the schedule" — but the live residual form (SIMD const-slice) is a hardcoded design choice,
  not a `DecisionCspFacts` output. The staging-form seam has no live consumer (Y.13 risk): P3 was
  not run.

3D is not RETIRED — the weaker "grammar-derived S0 island is BTA-liftable" is real and corroborated
by live `ScanAlphabet`. But it is nowhere near "ADVANCED, most-evidenced." Its flagship is refuted,
its gate stale/unsatisfiable, its distinctive contribution unproven, its LOCK-2 seam consumer-less.

**Enumerated open gaps (3D):**
1. Re-specify P1 against the LIVE `ScanAlphabet`/`css_scan_direct` residual (not retired
   `find_component_delim`); state the concrete byte-target on live.
2. Run `binding_times` on the REAL CSS/JSON IR, not a synthetic mirror; prove the delimiter
   island is an S0 node the live emit path actually exposes.
3. Demonstrate the strong unification concretely: thread `Project.ty`@204 as an S0 residual to a
   Rust type (the un-done half that would make "type-plane = stage-0" more than an assertion).
4. Measure a NEW-site cold-paired-PMU win (P2, JSON first-byte dispatch), floor −3.7…−6.4% ins/B —
   unrun; 3D is the family most prone to unmeasured plausibility (its own risk 2 + PROVE-09 law).
5. Prove the per-site staging-form decision (P3) is a `DecisionCspFacts` output with a live
   consumer, not a hardcoded emitter branch.

---

## CROSS-LOCK / DOUBLE-HOMING (critic-2 weight)

1. **The `Project.ty`@204 seam is QUADRUPLE-homed and un-discharged.** 3A (verifies it), 3B
   (threads it as `attrs.node_type`), 3C (its product), 3D (erases it as S0) all claim the same
   two lines. FAMILY-SPECS §0 calls it "the one obligation shared by 3A/3B/3D; 3C's product," and
   the incompat-map notes the coordination duty. But pass-1 evidence neither threaded nor
   coordinated it — PROTO-1 mis-probed it (D2), the others didn't touch it. The gestalt must
   assign this seam to exactly ONE owner (it is a GN-3 wave, `no-value-discard`), and the other
   three consume the threaded value. Funding it four ways is the double-home to collapse.
2. **3D's enabling rewrite is double-homed with 3B/3C — and already partly landed.** The
   template→IR-emitter step PROTO-2 says 3D needs is 3B's collapse / 3C's homoiousia, and on live
   it exists as `css_scan_direct`. FAMILY-SPECS calls "3B ⊕ 3D fully compatible" but hides that
   3D's residual-lift is *dependent on*, not merely compatible with, 3B/3C's template dissolution.
   Declare the dependency; do not re-fund the CSS emitter under a 3D banner.
3. **3C ⋈ LOCK-5 is one claim from two seats** (3C's `ir.bbnf` functor = 5A ONE-HIR). LOCK-3 must
   consume LOCK-5's verdict, never define it (Constitution ii). Correctly flagged; keep it a
   born-RED dependency edge, not a LOCK-3 deliverable.
4. **The decision-driver seat vs LOCK-2.** research-priorart §5 draws the line ("LOCK-3 owns
   *where* the driver sits; LOCK-2 owns *what engine*") — respected by 3A (fact-contract at the
   L1→L2 edge) but blurred by 3D (P3 hands staging-form to LOCK-2 with no live consumer). Keep the
   seat unmerged; the staging-form CSP is LOCK-2's, gated on a real consumer.
5. **`no-ts-ir` guard holds** across 3C/3D (meta-grammar emits TS *type decls*, never a runtime TS
   IR) — correctly stated; no violation found. The backend-agnostic middle is tape + schema
   (PROVE-14), not a shared instruction IR — no family contradicts this.

---

## THE INCOMPATIBILITY MAP — verdict

The map's core claim is SOUND and PROTO-1 corroborates it: the one hard incompatibility is
3A-tower ⊗ 3B-single-IR, dissolved by the hybrid (per-pass contract on one IR). The convergent
spine "one IR (3B) + per-pass contracts (3A discipline) + a stage attribute (3D erasure), 3C
banked" is the right shape.

Two understatements to correct: (a) the "3B ⊕ 3D fully compatible" layering is actually a
*dependency* (3D needs 3B/3C's template dissolution — §CROSS-LOCK 2); (b) the map treats the
Project.ty seam as a "coordination obligation for the gestalt" when pass-1 shows it is
*undischarged and mis-probed* — it should be a born-RED, single-owner gate, not a footnote.

---

## SUMMARY TABLE

| Family | CRIT-2 % | FAMILY-SPECS said | Divergence & why |
|---|---|---|---|
| **3A** | **55%** | ADVANCED (discipline) / CONTESTED (tower) | Agree on direction; but non-vacuity proof invalid as run (probed `Annotation`@203 not `Project`@204); edge count unsettled; refinement oracle unrun. |
| **3B** | **50%** | ADVANCED, strongest structural | Killer G1 byte-identity UNRUN; literal "pure redundancy" thesis refuted (emit-order framing); name-gate survives. Elegant-reduction trap. |
| **3C** | **30%** | BLOCKED-candidate | Agree; add: zero prototype evidence, product-only demo excludes the family's sum, double-homed hardcodes with 3B. |
| **3D** | **38%** | ADVANCED, most-evidenced | Overstated. Flagship P1 REFUTED + gate targets retired code; machinery ran on synthetic mirror only; unification unproven; LOCK-2 seam consumer-less; "no missing primitive" is wrong. |

**One line:** the hybrid spine is correctly identified, but pass-1's evidence is anchored to a
51-commit-stale tree and a mis-identified node; 3B owes its killer gate a run on live, 3D's
"most-evidenced" grade is refuted by its own prototype and must fall to a contingent core, and the
Project.ty seam must become a single-owner born-RED gate before any family claims to close it.
