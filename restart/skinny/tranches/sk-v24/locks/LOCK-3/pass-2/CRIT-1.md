# LOCK-3 — THE IR TRANSPOSITION SPINE · pass-2 · CRIT-1 (Adversarial Critic 1 of 2)

*WITHIN-LOCK-weighted adversarial critique — internal soundness of each family spec, and whether
the pass-2 prototypes actually tested the sharpest uncertainty. Every family run against the
failure-mode checklist (vacuous convergence, spec-cites-self, gates-that-cannot-fail, the
elegant-reduction trap, legacy aliases, masked fallbacks, unverified gestalt, consumer-less
substrate) + the Constitution (grammar totality, isomorphism, refutation laws, zero-runtime types,
parsimony, Y.13) + prototype-evidence misuse. All live claims re-verified by hand on the actual
working tree this pass. Cross-lock ledger entries treated as binding facts; challenges named for
routing to GESTALT-B with evidence.*

---

## 0. BASELINE — verified live this pass (every anchor re-greped by hand)

- **HEAD = `06e70053f`** (`git rev-parse HEAD` confirmed). FAMILY-SPECS §0's pin is CORRECT.
  `c93fc4314` (PROTO-1.md, PROTO-1-t1.md) and `ce51c36fc` (PROTO-2.md, PROTO-2-3A.md) are **both
  ancestors of HEAD**, and `git diff --stat {c93fc4314,ce51c36fc}..HEAD -- skinny/crates` is
  **empty** — every LOCK-3 prototype measured a `skinny/crates` tree **byte-identical to HEAD**.
  XL-H wrong-artifact-risk is **discharged** for all four. (The session-start git log showed
  `c93fc4314` as tip; HEAD has since advanced to `06e70053f`, a `restart/`-only resume commit — no
  code drift.)
- **Anchors re-verified live:** `:204` `Project{body,..} => infer_expr(*body)` (ty dropped) ✓;
  `:203` `Annotation => Builtin(Unit)` (legit) ✓; `direct_schema.rs:52` `DirectTypeKind` Struct-only,
  no `Sum` ✓; `admits_collapsed_stage`@`passes:959` reads `avx512bw`@`:380` =
  `cfg!(target_arch="x86_64" … avx512bw)` — **permanently false on aarch64** (DEAD gate) ✓;
  `runtime_generator.rs:98` **`if !sink_only.has_rule("value")` is STILL LIVE on the tree** — the
  comment reads "Shape-based, never a grammar name" but the predicate is a **rule-name query**
  (PROTO-1-t1's retirement lived only in a worktree, never landed) ✓.
- **`.target` dead-field claim — VERIFIED (I disambiguated a false hit).** `grep "target: Some"`
  in passes/codegen/ir returns 4 hits, **all four are `ExprKind::Ref { target: Some(rule) }`**
  (`passes:913,1104,1552,1599`) — a *different* `target` (the resolved-Ref rule id), **not**
  `DirectBuildField.target: Option<DirectBuildTarget>` (`ir:470`), which is `Some` **nowhere**. The
  field is genuinely dead; the "sole `DirectSchemaSet` producer is the hand file" claim holds.
- **PROTO-1's byte-coupling site — VERIFIED.** `general_descent/emit.rs:300-305`: `Seq(children)`
  emits `*cursor = skip_ws(bytes, *cursor);` for **every child at index>0**, unconditional on
  whether the child emits code. This is the real reason deleting no-op marker `Seq`-slots perturbs
  sheets bytes. **The finding is sound, not an artifact of the probe.**
- **Walker line counts — VERIFIED live:** `json_sink_direct/dispatch.rs` **318**,
  `general_descent/view.rs` **431**. FAMILY-SPECS's G-3B-1a figures are exact.

---

## 1. THE HEADLINE WITHIN-LOCK FINDING — THE SYNTHESIS IS STALE FOR A SECOND TIME, AND ITS PROTO CITATION CHAIN IS BROKEN

This is the single most important internal-soundness defect of the lock, and it must be folded
before pass-3 is briefed. **File mtimes (verified `ls`):** `FAMILY-SPECS.md` **19:32**;
`PROTO-1.md` **19:49**; `PROTO-2.md` **19:52**. The synthesis was cut **17–20 minutes before its
own two sharpest prototypes finished** — and those prototypes were written by **overwriting** the
files `PROTO-1.md` / `PROTO-2.md` that the synthesis cites for *entirely different* results.

**1a — The two "UNRUN load-bearing halves" (§6/§8) both RAN, with material verdicts the synthesis
does not reflect.** FAMILY-SPECS §8 issues Brief α (whole-fold G-3B-1) and Brief β (3A E1-infer
non-vacuity) as the two sharpest uncertainties "returned via StructuredOutput" for pass-3. **Both
are already discharged in this directory:**

- **Brief α → PROTO-1.md (19:49): `FAIL / DEMOTE 3B`.** Executing the minimal deletion (drop the two
  zero-consumer `SpanMark` markers + `Return`) perturbs `sheets/generated.rs` by **−3 `skip_ws`
  lines** (via the `emit.rs:300-305` per-Seq-child rule I verified above). The gate as the synthesis
  wrote it — *"delete the plane + four byte-identical regens"* — is **UNSATISFIABLE for sheets
  without a prior deliberate sheets re-baseline commit**; "byte-identical to committed" and "delete
  the plane" are **mutually exclusive** for sheets on this baseline. **This is worse than UNRUN: the
  killer gate is now shown unsatisfiable-as-specified.** The whole fold + the two 318/431 walker
  re-bases (G-3B-1a) were **not carried** — the net-LOC PASS bar is explicitly *not met*.

- **Brief β → PROTO-2.md (19:52): `PASS` — but it REFUTES the synthesis's own verify predicate.**
  24/28 Sheets projections carry a declared target NOT recoverable from `infer_expr(body)` (three
  regex bodies all infer `Span` yet declare Float/Span/Bool) — so the contract is **non-vacuous in
  principle**, answering §1 open-q (i) YES. **BUT the load-bearing caveat inverts the mechanism
  spec:** scoped to `Project{ty:Some}` — *exactly* how FAMILY-SPECS §1 mechanism-2 and G-3A-a write
  `verify_project_types` — only 4 projections carry `:ty`, all four are `-> input : Span` over regex
  bodies, so `infer_expr(body)==Span==resolved(ty)` for every one → **presence-only, the vacuity
  feared**. Non-vacuity is earned solely by the target-only `Project{ty:None}` cases (24/28), so the
  predicate MUST assert over the declared **value category** (`classify_projection`'s
  `ProjectionKind`), NOT the `:ty` nominal wrapper. **Consequence: G-3A-a's disposition — "gate-that-
  cannot-fail until LOCK-1" — is REFUTED for the value-category scoping: the contract FIRES TODAY**
  (number's Float, boolean's Bool, every literal's Tag all differ from the structural body type),
  with no name→Type resolution needed.

**1b — The PROTO citation chain is broken (an artifact-hygiene defect on internal soundness).**
FAMILY-SPECS §0 enumerates "the four pass-2 prototypes (PROTO-1, PROTO-2, …)" and cites **PROTO-1**
for *"the `:204` thread … RED [4 spans] → GREEN, zero emit footprint, four regen checks byte-
identical"* and **PROTO-2** for *"cleared the fact-layer purity precondition by trace."* Neither file
now contains that content — `PROTO-1.md` is the whole-fold FAIL, `PROTO-2.md` is the 3A non-vacuity
PASS. (PROTO-1-t1.md and PROTO-2-3A.md both self-document dodging *"the existing PROTO-1.md (the
:204-thread probe) and PROTO-2.md (the fold fact-purity trace)"* — proving those were the *earlier*
occupants of those filenames, since overwritten.) So the §6 convergence rows *"`:204` thread … PROVEN
(PROTO-1)"* and *"Fold fact-layer purity … PROVEN by trace (PROTO-2)"* are **dangling citations** — a
reader who follows them lands on different probes. Two of the lock's four claimed proofs no longer
have a resident artifact (they survive only in git history / the prose). **The agglomerator owes a
reconciliation:** re-home the overwritten :204-thread and fold-purity-trace results under stable
names, and re-point every §0/§6 citation, before pass-3 consumes this map.

**1c — This is the SAME error CRIT-2 caught one recurrence earlier.** The prior CRIT-2 (§0.2) flagged
the 15:10 synthesis as stale vs the 15:34/15:38 prototypes. The 19:32 re-cut folded *those*
(PROTO-1-t1, PROTO-2-3A are cited correctly) — and then immediately went stale again against the
19:49/19:52 pair. **The pattern is structural, not incidental: prototypes are landing inside the
synthesis's own authoring window.** Pass-3 must gate the synthesis cut on a frozen prototype set, or
this recurs a third time.

---

## 2. PER-FAMILY HARDENING (convergence % + enumerated open gaps)

### FAMILY 3A — CONTRACT-TOWER · **62%** (FAMILY-SPECS said 60%; net-flat: a real β-PASS uplift, offset by a mechanism-spec defect the PASS exposes)

**Genuinely earned.** (a) The refinement oracle RAN (PROTO-2-3A): `differential(edge,corpus)`
localizes a 5-fault matrix to its arm with zero false positives, catches a *same-discriminant*
payload corruption a tag-only walk misses, and separates an E2 input fault (O2 RED) from E3 (GREEN)
— "names the edge, never merely a failure," with the G-3A-b zero-witness⇒`unverified` discipline
proven. (b) The contract's **non-vacuity is now PROVEN in principle** (PROTO-2.md/Brief β): §1
open-q (i) resolves YES. This is the sharpest 3A uncertainty and it is discharged — a real uplift the
synthesis's own text (which still lists it "UNRUN + gate-cannot-fail until LOCK-1") does not credit.

**But two internal-soundness defects cap the uplift:**
- **The elegant-reduction trap is intact on the load-bearing edge.** The oracle ran on E3, a
  *definitionally 1:1 rename* (that is *why* it is a "free oracle"); the injected-fault localization
  on **E1-infer** — where the contract's meaning lives — is **only digest-grounded, never fault-
  injected** (PROTO-2-3A scope-limit 1). "3A's organ runs and localizes" is proven where it is easy.
- **The spec's own `verify` predicate is written in the vacuous form** (mechanism-2, G-3A-a: scoped
  to `ty:Some`). Brief β shows that scoping is presence-only on the live fleet. Until §1 is re-cut to
  the value-category surface, the *specified* organ verifies threading-presence, not consistency —
  a gate-that-cannot-fail **written into the spec**, even though the *correct* predicate fires today.

**Enumerated open gaps (3A):**
1. **Re-cut mechanism-2 / G-3A-a to the value-category (`ProjectionKind`) surface** — the `:ty`-only
   predicate is refuted vacuous by Brief β; the spec must adopt the full-`->`-target form the
   prototype proved fires. (New gap, surfaced by the newest prototype.)
2. **E1-infer injected-fault localization** — mutate `infer`, show E3 stays GREEN while E1 localizes.
   PROTO-2-3A only digest-grounded E1; this is the load-bearing widening. Unrun.
3. **G-3A-a name→Type resolution** (born-RED, owner LOCK-1) — required only for the *nominal* `:ty`
   consistency arm; the value-category arm is independent of it. Carry, do not gate the whole family.
4. **Open-q (ii)** — prove `render`@`runtime_generator.rs:78` is a pure projection of L2, else an E4
   oracle is owed. Unrun.
5. **Open-q (iii/iv)** — the CSS `Err⇒SinkOnly / Ok⇒Scan` fork (`runtime_generator.rs:40-71`):
   first-class edge oracle or declared two-root tower. Undecided.
6. **Promote the +607-LOC PROTO-2-3A test to a shipping `differential(edge,corpus)` CI contract** —
   the stated 3A deliverable is a contract, not a probe test. Unrun.

### FAMILY 3B — SINGLE-IR-MULTIPASS · **54%** (FAMILY-SPECS said 55%; hold-to-slightly-down: the killer gate ran and FAILED-as-written)

**Genuinely earned.** Fact-layer purity PROVEN by trace (the old PROTO-2, per §1b now a dangling
citation); the emit-dispatch fork retirement EXECUTED byte-identical (PROTO-1-t1, 3/3), and PROTO-2's
named discriminator (`dispatch_alt_count`) **refuted by execution** — the honest discriminator is the
≥4-of-6 value-category census (`roles.value`). One real slice of the fold path is green.

**But the substrate's convergence did NOT rise this pass — it took a hit:**
- **The killer gate G-3B-1 RAN and returned FAIL (PROTO-1.md/Brief α).** Per FAMILY-SPECS §2's own
  demotion rule — *"run it next pass (Brief α) or demote 3B to structurally-motivated, unproven"* —
  the trigger is now **met and fired**. The byte-identity gate is not merely unrun; it is
  **unsatisfiable-as-specified for sheets** (the `emit.rs:300-305` incidental-separator coupling,
  verified live). The architecture survives (the plane is a denormalized copy of `descriptors`,
  semantically deletable, `strict_product` invariant), but the **byte-identity proof is refuted** and
  the gate needs re-specification (a sheets re-baseline commit *before* deletion). That is a NEW,
  unenumerated gap the pass added — convergence should not rise on a FAIL.
- **G-3B-1a — the elegant-reduction tell — is STILL UNRUN.** PROTO-1.md ran only the minimal marker
  deletion; the −100 net-LOC reward hangs on re-basing `dispatch.rs` (318) + `view.rs` (431) onto
  `GrammarIr` byte-identically. `view.rs`/`general_descent` is shown **byte-fragile**; the walker
  re-base is exactly the "and-then-the-hard-part." Report CONTINGENT, not banked.
- **G-3B-2's green slice is a masked-fallback smell.** PROTO-1-t1 obtained its byte-identical GREEN by
  **porting `derive_materialization_roles.value` into codegen** — a DRY / system-cohesion /
  no-orthogonal-codepaths violation its own text flags. It proves *executability*, not the
  *converged architecture*; the single-home wiring (thread `roles.value` onto the attribute bundle)
  is unrun. Registry must carry it "executable, single-home deferred," not "sub-gate discharged."

**Enumerated open gaps (3B):**
1. **Re-specify G-3B-1** as a sheets-re-baseline-then-delete gate (the as-written byte-identity gate
   is refuted by PROTO-1.md); then execute the *whole* `emit(&GrammarIr,&Attributes)` fold. THE
   killer gate, now shown unsatisfiable in its old form. Unrun.
2. **G-3B-1a re-walk re-base** — dispatch.rs (318) + view.rs (431) onto `GrammarIr`, byte-identical.
   view.rs shown fragile. The net-LOC reward rests entirely here. Unrun.
3. **Single-home wiring** of the value-role discriminator — retire the codegen-side duplicate
   PROTO-1-t1 introduced. Unrun (Constitution-forbidden as production).
4. **css_scan fourth-level** (open-q ii) — confirm no emission state (grammar prefixes, entry-rule
   threading) hides outside `GrammarIr` in `lower/css_scan.rs`. Untraced.
5. **Per-`RuleId` vs per-`ExprId` key** (G-3B-3) — fleet check; a partial migration is the
   no-orthogonal-codepaths defect. Undecided.
6. **True RETIREMENT (vs relocation)** of the byte-literal classifier — born-RED on `DirectTypeKind::Sum`
   (GN-3.W1); never asserted retired absent the node.

### FAMILY 3C — GRAMMAR-HOMOIOUSIA · **32%** (concur with FAMILY-SPECS and CRIT-2)

Correctly held BLOCKED (C1) / BLOCKED-candidate (C2). The C1/C2 split is the honest cut. No new
prototype this pass — 3C is the one family whose sharpest uncertainty was **not** tested. Its cheapest
concrete proof (research-C §7.2: the `ExprKind::Alt → DirectTypeKind::Sum` E6 map emitting byte-
identical to `derive_shape_facts`'s hardcoded 6-variant list, `passes:285-294`) remains **SPEC-ONLY**
— and it is the *only* run that could move C1 off 0%-convergence-eligible before its external gates
land. Distinctive content is 0% convergence-eligible; the one live win (the `:204` thread) is the
shared single-owner thread, not distinctively 3C. Constitution vii: **SPEC-ONLY this pass.** No
convergence uplift is available without the Sum node (GN-3.W1) or a LOCK-7·7B consumer.

**Enumerated open gaps (3C):**
1. `DirectTypeKind::Sum` — born-RED, GN-3.W1 (+420 LOC, Struct-only verified). Product-only subset
   excludes every `Alt` = excludes `ExprKind`/`BackendExpr`; must not stand for the formulation.
2. Named in-wave consumer for C1 — LOCK-7·7B; no live reader (Y.13). Admissible only as a born-RED
   GESTALT-B edge.
3. Fixpoint `regen∘regen = regen` byte-identical over a grammar-generated `ir/src/lib.rs`
   (derives/doc-comments/impls) — likely forces per-derive annotations. LOCK-7·7A.
4. C2 cession to LOCK-2 — provisional on XL-B (WORKING, not final).
5. The E6 sum-edge byte-identity prototype (research-C §7.2) — the cheapest concrete C1 proof, UNRUN.

### FAMILY 3D — STAGED-RESIDUAL · **37%** (FAMILY-SPECS said 38%; hold, no new evidence)

BANKED into 3B's attribute store, correctly. Nothing 3D-distinctive ran this pass. The distinctive
thesis "type-plane erasure = stage-0 erasure" **spec-cites-itself** — its warrant is Constitution v
(a definitional restatement, not an independent result). `binding_times` on real IR is UNRUN;
`CollapsedStage` is a **verified-DEAD gate** (`admits_collapsed_stage`@959 → `avx512bw` → false on
aarch64) — a genuine Y.13 consumer-less-substrate *on the living tree*, evidence of the problem, not
the solution. The PROVE-12 perf anchor was **not re-run on `06e70053f`** (base still `d2b32f6e3`), and
the type-plane JSON PMU delta the directive asks for is **UNMEASURED** (PROVE-09 law: absent a cold-
paired number the JSON claim collapses to "typed façade, same speed," acceptable only if so declared).

**Enumerated open gaps (3D):**
1. `binding_times(CssScanProgram)` on REAL CSS IR — prove the S0/S1 partition *equals* PROVE-12's
   hand-residualized set (parity byte-identical iff correct). Unrun.
2. Re-home `admits_collapsed_stage` off `avx512bw` onto a grammar-derived binding-time predicate;
   RED→GREEN with no x86 smuggled back. Unrun.
3. Type-plane JSON first-byte cold paired-PMU — real ins/B or a named negative. UNMEASURED.
4. Re-confirm PROVE-12 on `06e70053f` (XL-H hygiene; base moved). Unrun.
5. The unification thesis proven concretely, not restated from Constitution v.

---

## 3. SPINE VERDICT

The convergent spine (3B substrate + 3A discipline + 3D attribute; 3C banked) is **architecturally
sound and the family composition holds**. But this pass's two newest prototypes — the ones FAMILY-SPECS
declared the two sharpest uncertainties — landed *after* the synthesis cut and **change the map**:

- **3A's contract now has PROVEN meaning (β-PASS) — but the spec's own predicate is written in the
  vacuous form the PASS refutes.** Net: a real uplift and a spec-correction obligation, roughly
  cancelling on the number; the load-bearing E1 *injected-fault* arm and the shipping-contract
  promotion remain unrun. **62%.**
- **3B's killer gate RAN and FAILED-as-written (α-FAIL) — the byte-identity gate is unsatisfiable for
  sheets without a re-baseline, and the walker re-base (the true hard part) is still unrun.** Per the
  synthesis's own rule, the demotion trigger fired. Architecture survives; the byte-proof does not.
  **54%.**
- **3C stays BLOCKED / SPEC-ONLY (32%); 3D stays BANKED (37%)** — neither had its sharpest uncertainty
  tested (3C's cheapest E6-sum proof and 3D's `binding_times`-on-real-IR both UNRUN).

**No family is convergence-eligible** (pass 2 of ≥3; each carries ≥1 unrun/failed load-bearing gate;
two consecutive clean passes + fresh non-author audit outstanding for all four).

**Three corrections the registry MUST fold before pass-3 (in priority order):**
1. **Re-fold PROTO-1.md (α, FAIL) and PROTO-2.md (β, PASS-with-correction) into the family
   dispositions and the §6 table** — the synthesis is stale for a second time against its own
   sharpest prototypes; the §8 briefs are already discharged, not pass-3 directives.
2. **Reconcile the broken PROTO citation chain (§1b)** — re-home the overwritten :204-thread and
   fold-purity-trace results under stable names; re-point every §0/§6 citation. Two claimed proofs
   currently have no resident artifact.
3. **Freeze the prototype set before the next synthesis cut** — the stale-synthesis error has now
   recurred twice at the same structural cause (prototypes landing inside the authoring window).

*(Well-handled this pass, credit noted: FAMILY-SPECS §7.1 now surfaces the XL-F(1) `TypeAnalysis::from_ir`
consumer contradiction as an explicit named-supersession challenge routed to GESTALT-B — the silent
re-home the prior CRIT-2 flagged is corrected. That cross-lock finding is properly disposed.)*
