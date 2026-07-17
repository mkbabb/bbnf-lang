# LOCK-2 — THE GRAPH ENGINE — pass-2 · CRIT-2 (Adversarial critic 2 of 2, CROSS-LOCK weight)

*Charter: harden/challenge/refine every family against the failure-mode checklist — vacuous
convergence, spec-cites-itself circularity, gates-that-cannot-fail, the elegant-reduction trap,
legacy aliases, masked fallbacks, unverified gestalt, consumer-less substrate — plus constitution
violations (grammar totality, isomorphism, refutation laws, zero-runtime-cost types, parsimony) and
prototype-evidence misuse (SPEC_ONLY anchoring a perf claim). As critic 2 I weight the CROSS-LOCK
view: where this lock contradicts or double-homes work against the extant GN/PT codex and the other
locks. Read this pass: PORTFOLIO + CONSTITUTION, FAMILY-SPECS (19:25), PROTO-1 (19:46) + patch,
PROTO-2 (19:47) + patch, research-{A,B,priorart}, CROSS-LOCK (XL-A..H), the on-disk CRIT-1, and — the
load-bearing step — the actual on-disk layout of the top-level `crates/` workspace and the
`skinny/crates/` workspace, verified at working HEAD `06e70053f` (base-pin `ce51c36fc` is an
ancestor).*

---

## 0. THREE GOVERNING CROSS-LOCK FINDINGS — all decision-grade, none folded into the spec

### 0.1 The staleness defect CRIT-1 named "the document's load-bearing joint" RECURS one iteration later

CRIT-1 §0.1 flagged that FAMILY-SPECS (13:10) predated PROTO-1 (13:25) and PROTO-2 (13:40). **The
identical defect is present again, one iteration on.** On-disk mtimes: `FAMILY-SPECS.md` = **19:25**;
`PROTO-1.md` = **19:46**; `PROTO-2.md` = **19:47**. The synthesis was written BEFORE the two
prototypes now on disk ran, and the two prototypes on disk are **the pass-3 briefs the synthesis
itself defines** — PROTO-1 = Brief 1 (PROVE-2-ALTCOST-CROSSOVER), PROTO-2 = Brief 2
(PROVE-2-SUBSTRATE-RECONCILE). So the FAMILY-SPECS speaks throughout in the pre-resolution
conditional about experiments that have since RUN:

- §0.2 line 58/69: the branch-count crossover fix is *"cheap, named by the probe itself, and
  **untested**."* — PROTO-1 has since **run it** (structural crossover green, N\*=3.0).
- §0.3 / §V: *"the adopt-and-prove port … is the actual unbuilt core, and NO family scopes it,"*
  routed as a *"binding blocker"* to GESTALT-B. — PROTO-2 has since **run the port** (engine crate
  compiles in skinny, conflict walk = 0).

The synthesis' family verdicts (2A ADVANCED, 2B "converging into 2A," 2C EARNED-solver, 2D BLOCKED)
were assigned **before the two probes that turn on them ran**. This is not a nuance — it is a
process-integrity defect at the document's load-bearing joint, recurring across two consecutive
synthesis iterations. **The FAMILY-SPECS must be re-cut against PROTO-1/PROTO-2 before any family
verdict is decision-grade; the numbers below are floored by this staleness.**

### 0.2 The substrate contradiction re-diagnosed on disk — SHARPER than §0.3 and NOT dissolved by PROTO-2

The FAMILY-SPECS §0.3 frames a "three-tree split" (engine top-level, seams skinny, `bounded_sum` a
bank tree) and routes it to GESTALT-B as the "binding blocker." PROTO-2 (newer) claims to **dissolve**
it ("the engine crate is already in skinny; no GESTALT-B escalation warranted"). **Disk shows both
are partly wrong, and the true shape is more damaging to the lock's distinctive thesis than either
admits.** Verified at HEAD `06e70053f`:

| fact | disk truth (verified) |
|---|---|
| `skinny/Cargo.toml` deps | `ir = {path="crates/ir"}` (skinny-local); **`egraph = {path="../crates/egraph"}`**, **`csp-solver = {path="../crates/csp-solver"}`** — the GENERIC engine crates are the TOP-LEVEL ones, path-referenced UP |
| `skinny/crates/egraph/` | **does not exist** — skinny has no local engine crate; it consumes top-level `crates/egraph` |
| `GrammarENode` / `GrammarCostModel` | live ONLY in top-level `crates/ir/src/egraph/{node,cost}.rs`; **`grep -rln GrammarCostModel\|GrammarENode skinny/ = EMPTY`** — never referenced in skinny |
| skinny decision seam cost model | `passes/src/backend_egraph.rs:261` `impl CostModel for DecisionCostModel { fn cost(&self, node, **_child_cost**) { node.score } }` — **ignores child cost; returns a precomputed scalar** = the childless argmin |
| `bounded_sum.rs` | **`find crates -name bounded_sum.rs` = EMPTY; `find skinny … = EMPTY`** — absent from HEAD in BOTH trees; exists only in PROTO worktrees (the divergent bank) |
| crossover Alt arm | top-level `cost.rs:114` at HEAD is STILL `base + self.weights.dispatch_bonus` — **the flat form; the crossover rewire is worktree-only, not landed** |

**The precise contradiction is not "three trees" — it is two DIFFERENT decision cost models in two
trees, and the two prototypes proved things in opposite ones:**

- **PROTO-1 proved the N-crossover in `GrammarCostModel`** (top-level `crates/ir/src/egraph/cost.rs`)
  — a child-bearing, tree-profile cost model that **skinny's decision path never invokes** (grep
  empty).
- **PROTO-2 accepted `DecisionNode`/`DecisionCostModel`** (skinny `backend_egraph.rs`, **childless**,
  `_child_cost` ignored, returns `node.score`) as "skinny's substrate-native GrammarENode-analogue"
  and declared the port "not needed."

So the two probes, taken together, **do NOT close the gap they appear to.** PROTO-2's "resolution"
*concedes* the tree-profile crux rather than resolving it: by accepting the childless `DecisionNode`
as skinny's e-node, it **entrenches** that skinny's production decision seam is exactly the `argmin`
over childless candidates that Family 2A's OWN gate (i) demotes ("not a genuine island"). **The
honest floor CRIT-1 named — "0 genuine child-bearing islands with a live consumer in the substrate" —
is UNCHANGED by both probes**, because PROTO-1's child-bearing cost model is in a tree the substrate
does not call and PROTO-2 blessed the childless one as canonical. The "adopt-and-prove port"
(wire `GrammarENode`/`GrammarCostModel`'s tree profile INTO skinny's decision seam, replacing the
childless `DecisionNode`) is **still the unbuilt core, and still unscoped** — PROTO-2 did not build
it; it renamed the gap "done."

> **Challenge routed to GESTALT-B (CROSS-LOCK protocol, with evidence, REFINING CRIT-1's and the
> FAMILY-SPECS' §0.3 version).** The lock carries TWO grammar-decision e-node types doing overlapping
> work — `GrammarENode` (top-level, tree-profile, over the interned `GrammarIR`/`DagNode` DAG) and
> `DecisionNode` (skinny, childless, over `BackendCandidate`). The generic `egraph` PRIMITIVES crate
> is cleanly shared by path-dep (parsimony OK there); the DUPLICATION is at the grammar-e-node layer,
> and it is the very layer the lock's distinctive extraction thesis lives in. XL-A cannot be honored
> ("skinny is the only living implementation") while skinny's decision seam runs a childless argmin
> and the tree-profile engine sits unreferenced in the "legacy" tree. GESTALT-B must adjudicate: is
> the substrate's e-node `DecisionNode` (then the tree-profile thesis is dead and I2/I4 are
> canonicalizations forever) or `GrammarENode` (then the port is real, unbuilt, and owed a scoping
> family)? PROTO-2's "no escalation warranted" is over-read.

### 0.3 PROTO-1's PMU half is UNMEASURED — the master gate is half-open, and the crossover is N-sensitivity, not proven perf-tracking

Brief 1's pass/fail (FAMILY-SPECS line 493) requires BOTH clauses: *"a single fixed calibrated vector
yields small-N SEQUENTIAL and large-N DISPATCH AND the two lowerings' cyc/B 95% CI excludes 1.00×
same-direction."* PROTO-1 delivered the **first** clause (green, deterministic, N\*=3.0) and reports
the **second UNMEASURED** — verdict line `RAN_UNMEASURED`, blocked by a nonexistent bench target
(`css_canon_bench` does not exist), no scriptable PMU on darwin-ARM, loadavg 6.98–17.37, and "no
direct synthetic-EGraph → runnable-parser lowering." **Per the family's own contract the gate is not
PASS — it is half-open.** Three adversarial consequences:

1. **The crossover proves the model is N-SENSITIVE, not that its N-sensitivity tracks real cost.** The
   whole point of extraction-as-decider is to pick the *faster* lowering; the structural crossover
   proves only that the model picks *differently* by N. Whether DISPATCH is actually faster at N=8 in
   this pipeline is exactly the unmeasured clause. PROTO-1 is honest ("a well-established
   microarchitectural fact, but not the novel claim under test and not asserted here as measured") —
   but any downstream reading of "extraction-as-decider EARNED / 2A/2B/2D unblock" **over-reads**,
   because the unblock is contingent on the unmeasured half. Constitution vii: SPEC-ONLY must not
   anchor a perf claim.
2. **The crossover N\*=3.0 falls out of HAND-AUTHORED coefficients** (`dispatch_table=3.0`,
   `alt_per_branch=1.5`, `dispatch_branch=0.5`), and **2D has zero executed fit**. So the model
   discriminates because it was *given* discriminating coefficients — a subtle circularity: the
   discrimination is not grounded in measurement, and the clause that would ground it (PMU + a 2D fit)
   is precisely what is unmeasured. The elegant-reduction trap in its purest form — "the model now
   discriminates, **and-then-the-hard-part** (does it discriminate toward the faster lowering, under
   fitted coefficients) is unbuilt."
3. **Neither refuted nor converged remains the honest label** — CRIT-1 §0.3's verdict survives the
   new evidence intact. The structural precondition advanced from "named/untested" to "green in a
   worktree"; the deciding clause did not move.

---

## Family 2A — EGRAPH-ISLANDS — convergence 46%

The directed-spine + local-islands architecture is coherent and evidence-aligned; gate (i) is a real
RED-capable predicate; PROTO-1's crossover limb is a genuine advance on the master gate's *structural
precondition*. Credit all three. But §0.2 shows the crossover was proven in a cost model
(`GrammarCostModel`) the substrate's decision seam never invokes, §0.3 shows the perf half unmeasured,
and by the family's own gate (i) the honest floor of genuine islands in the substrate remains **0**.

**Enumerated open gaps:**
1. **[MASTER — structural half green, deciding half UNMEASURED] I2/I4 tree-profile extraction is
   SPEC-ONLY straddling, and the green half is in the WRONG ENGINE for the substrate (§0.2).**
   PROTO-1's crossover lives in top-level `GrammarCostModel`; skinny's decision seam runs childless
   `DecisionCostModel` (`node.score`, `_child_cost` ignored). Even a full PROTO-1 PASS would prove a
   property of an engine the substrate does not call on its decision path. The tree profile is dead
   code in the substrate until the port lands.
2. **[§0.2, unscoped] The port — wire the tree-profile `GrammarENode`/`GrammarCostModel` into skinny's
   decision seam, replacing the childless `DecisionNode` — is the unbuilt core, still unscoped by any
   family.** PROTO-2 renamed it "done" by blessing `DecisionNode` as substrate-native; that concedes
   the tree-profile thesis rather than realizing it.
3. **[§0.3] The perf clause of gate (i)/(iv) is UNMEASURED.** "Changes the pick under a fixed
   calibrated vector" is green; "moves cyc/B at value-starts" is not. No bench target, no PMU host.
   The born-RED GN-2.W1 clause (≥8× skip, ≥0.95× dense, ≥3 cold PMU windows) gates an unbuilt consumer
   — cannot fail this wave.
4. **I1 fails the family's OWN gate (i) — confirmed on disk.** `DecisionCostModel::cost` ignores
   `_child_cost` → `argmin` over 2–5 childless candidates; the sibling `decision_csp` re-solves a
   1-variable CSP pinning the already-chosen index. Two subsystems, one decision, zero congruence
   sharing. By gate (i) demote to a plain cost-ranked pass; genuine child-bearing islands with a live
   substrate consumer = **0**.
5. **[XL-C] I2 (regex-HIR canon) is WITHHELD to LOCK-5.** The lawful `Hir` is LOCK-5's; the two
   domain-fit types are RATIFIED (grammar-tier `Regex(StringId)` opaque; engine-tier rich `Hir`); the
   `rules/regex.rs` keep/abrogate is LOCK-5's verdict. Off this lock, untested here.
6. **[XL-D] I3 (scan-plan) sites are IR-latent, consumer BLOCKED two locks deep (LOCK-9 9C / LOCK-5).**
   Skinny IR models a single `SimdMode`/`SimdSite`; the `Prefilter`/`SelfLoop` node shape is LOCK-3's;
   research-A's own verdict is scan selection is a lowering predicate (multiplicity ≈ 1), which gate
   (i) demotes absent a binding Exact-vs-Prefilter tie the fleet has not exhibited.
7. **[XL-F] I4 (projection canon) is probably an always-apply canonicalization double-homed with
   LOCK-1's `Project`.** PROVE-12 residualized it by monomorphization, not an e-graph; a name-gate
   unless LOCK-1 1D ships an e-graph — see the cross-lock double-home note below.
8. **Ceremonial-deletion gate (v) unexercised** — the ~68-field provenance apparatus deletion is
   asserted, dead until a plan-reading consumer exists; masked-fallback watch item.

## Family 2B — IR-IS-EGRAPH — convergence 38%

Context-freedom is the pass's real 2B advance: PROTO-2's `admits_sink_conflict_walk` returned **0
conflicts non-vacuously** (`pair`, `string` are live multi-caller sink-only rules with uniform
verdicts), and the on-disk root cause is sound — `admits_sink_only(rule_ir, target)` takes no caller
argument and `target` is threaded globally from one `TargetFeatures::host()`. Credit the lift from
CRIT-1's "UNSETTLED / wrong-tree." But the maximal reading stays XL-B-refuted, the distinctive thesis
stays master-gate-straddled in the wrong engine, and 2B's real deliverable was conceded, not built.

**Enumerated open gaps:**
1. **[MASTER, §0.2] "extraction emits `BackendShape` directly" remains unearned in the substrate.**
   The child-bearing crossover is in `GrammarCostModel` (top-level, unreferenced by skinny); skinny's
   e-node is childless `DecisionNode`. Multiplicity ≈ 1 in the substrate stands. XL-B forecloses the
   maximal reading (LOCK-3 owns the driver seat); "converging into 2A" is the sound disposition.
2. **[XL-B, binding] The maximal reading seizes LOCK-3's decision-driver seat.** FAMILY-SPECS concedes
   this correctly. The lawful residual (acyclic in-place e-node substrate, on-insert rewrite, B&B
   extraction for the budget slice) IS the 2A island posture — no distinct existence over 2A.
3. **The hard architectural boundary is real (research-B §6, priorart).** Greedy bottom-up extraction
   is optimal only for MONOTONE cost; the generated-size budget is sharing-sensitive / non-monotone,
   so "extraction emits `BackendShape` directly" cannot honor the budget through the current
   extractor. Both lawful completions dissolve 2B's independence into a solver — converging with 2C.
4. **Whole-grammar organ within ~230 µs incl. teardown — UNTESTED** (the spec's own "single most
   important open measurement"). No proto stood the full-IR e-graph over CSS-L4 (2875 states) and
   timed build+saturate+extract+**teardown**; `CspScheduler` rebuilds a fresh `Csp` per iteration —
   the opposite of the aegraph no-rebuild lesson (priorart §1, Cranelift avg class 1.13). Pure
   assertion.
5. **regen byte-identity through cost-guided extraction — UNTESTED at grammar scale.**
   `GrammarCostModel` returns `f64` with Vec tie-break; the total-order/`candidate_hash` fix is owed,
   zero grammar-scale evidence. (PROTO-2's `check-json ×2` byte-identity proves regen neutrality of a
   TEST-ONLY patch — its own patch touches only `mod recognizers` in `lib.rs`, no production path — so
   it does NOT exercise cost-guided extraction determinism.)
6. **[§0.2] 2B's real deliverable — the port — was CONCEDED, not built.** PROTO-2 declared
   `DecisionNode` substrate-native and the port unneeded. That entrenches the childless argmin as
   skinny's decision e-node; it does not give 2B a distinct existence. The two grammar-e-node types
   (`GrammarENode`/`DecisionNode`) are a parsimony duplication the gestalt must collapse.

## Family 2C — CSP-CENTRAL — convergence 50%

Still the strongest-earned family: the solver-shape question is closed (B&B-native-knapsack over the
near-tree scan-plan graph; PBQP refuted as ceremony — RN=0 but Lagrangian budget-dual carries an
irremovable integrality gap, css-l4 obj 39.003 vs true 21.011, ~10⁴× wall-clock). Credit the genuine
positive. But every tooth remains SPEC-ONLY, the propagator is not landed, and the verdict carries an
untested escape hatch.

**Enumerated open gaps:**
1. **`bounded_sum` is NOT at HEAD — worktree-only (verified: `find` empty in both trees).** The "merged
   and tested 6/6" is a concrete prototype artifact (constitution vii satisfied for the CLAIM), but the
   substrate does not carry it; the 6/6 ran against a **third divergent bank tree** carrying
   `soft.rs`/`cardinality.rs`/a `Soft` variant the base lacks. Landing + provenance reconciliation is
   owed.
2. **[XL-D, consumer-blocked] Budget-binds-on-REAL-costs is unproven.** PROTO evidence used SYNTHETIC
   per-site costs (admissible for the solver-*shape* question). Whether a real static-size vector makes
   the budget bind over ≥2 IR-modeled sites is untested; the sites don't exist in skinny IR
   (`SimdMode`/`SimdSite` single-valued); exposing them is GN-2.W1, unbuilt.
3. **Consumer-less (viii/Y.13).** No codegen consumer reads the scan plan; the decision layer has no
   downstream. Parity/determinism is checked where nothing is wired.
4. **The B&B-wins verdict is contingent on HARD coherence; the soft/finite-preference penalty is
   unrepresentable in the base B&B** (needs the `Soft` variant the bank prototypes) and untested — the
   one residual point in PBQP's favor, deferred not closed.
5. **[XL-G, cross-lock] One-solver-home with LOCK-1 1B is a GESTALT-B unification point, unresolved.**
   `bounded_sum` extends the same `csp-solver` LOCK-1 1B centralizes as its constraint store. Two locks
   now claim `csp-solver`; the three-tree/bank provenance must reconcile before the home is coherent.
6. **Degenerate without 2D.** Absent a fitted cost table the objective is `static_size_cost` — "a
   CSP-shaped priority cascade with extra steps." 2C non-degeneracy leans on an unrun family.

## Family 2D — PROFILE-CLOSED-LOOP — convergence 30%

Sound discipline (AutoFDO/BOLT/uops.info precedent), correct determinism invariant (loop closes at
calibrate-time, so `regen∘regen` survives, load noise off the regen path). But zero executed fit, and
the master-gate dependency resolved AT BEST partially adverse this pass.

**Enumerated open gaps:**
1. **[MASTER] The ≥2-survivor front exists only in a probe, in the wrong engine, from unfitted
   coefficients (§0.2/§0.3).** PROTO-1 manufactured a discriminable front IN `GrammarCostModel`, but
   (a) skinny's decision seam does not use it, (b) the crossover falls out of hand-authored
   coefficients — the very thing 2D exists to FIT — so 2D has a *path* and still no *fit*.
2. **Zero executed fit (gate 1).** "A fitted coefficient FLIPS a decision AND beats the hardcoded lane
   cold-paired" — unrun. PROTO-1's crossover is a manual coefficient rewire, not an OLS/ridge fit.
3. **Falsifiable freshness (gate 4) unbuilt.** "Mutate a coefficient → gate goes RED,
   `check-calibration` fails" — no prototype showed the gate can go RED; the current `"pass"`/`0`
   stamps are the lie 2D promises to remove, so the anti-provenance-theater claim is itself presently
   provenance theater.
4. **[XL-F, named supersession owed] The only unblocked plane touches LOCK-9/GN-2 knobs**
   (`CSS_SHUFTI_MIN_SPAN`, `GATE=16`, `select_backend` crossover). Fitting these supersedes hand-tuned
   GN literals — the spec now flags this as a named supersession (partial credit over CRIT-1), but it
   remains a cross-lock claim on another lock's knobs, not a free attachment.
5. **PMU half of the closed loop is unmeasurable in this environment** (§0.3) — the ins/B-leads /
   cyc/B-certifies discipline (PROVE-02 law) has no host here; the loop cannot be exercised end-to-end.

---

## Cross-cutting adversarial notes (checklist + constitution + CROSS-LOCK)

- **CROSS-LOCK double-homing — one NOT surfaced by the spec (egraph crate ⊗ LOCK-1 1D).** The
  FAMILY-SPECS acknowledges the `csp-solver` co-home with LOCK-1 1B (2C gap) and the `Project`
  double-home with LOCK-1 (I4). It does NOT address that **LOCK-1 1D (TYPES-AS-ECLASSES) also claims
  the e-graph engine** ("shares the engine with LOCK-2"). So the generic `egraph` crate is claimed by
  LOCK-1 (1D type lattice), LOCK-2 (2A/2B islands, 2C candidate generation), and — via `../crates/`
  path-dep — the skinny `passes` seam. A GESTALT-B one-e-graph-home coordination point the spec omits.
- **CROSS-LOCK duplication (parsimony vi).** Two grammar-decision e-node types (`GrammarENode`
  top-level tree-profile; `DecisionNode` skinny childless) do overlapping work in two trees; PROTO-2
  entrenches rather than collapses it. The generic `egraph` primitives crate IS cleanly shared — the
  duplication is precisely at the thesis-bearing e-node layer.
- **XL-B honored** — 2B's maximal driver-seat reading correctly conceded to LOCK-3.
- **XL-C honored** — I2 correctly WITHHELD to LOCK-5; two domain-fit types treated as binding fact.
- **XL-D honored** — I3/2C teeth correctly marked consumer-BLOCKED two locks deep.
- **XL-A CONTRADICTED by disk (challenge to GESTALT-B, §0.2)** — "skinny is the only living
  implementation" is false of the DECISION engine: skinny runs a childless argmin and the tree-profile
  engine sits unreferenced in the "legacy" tree. PROTO-2's dissolution is over-read.
- **XL-H (base law) — a wrinkle.** PROTO-1/PROTO-2 base-pin `ce51c36fc`; working HEAD is `06e70053f`
  (ancestor-clean, so not wrong-artifact by the letter). But my disk verification is at `06e70053f`,
  and the crossover/`bounded_sum` are absent there (worktree-only) — so the landed substrate is
  *behind* the prototype claims. Every "EARNED" that rests on a worktree artifact (crossover green,
  `bounded_sum` 6/6) must carry a "not-landed" rider until committed.
- **Vacuous convergence:** the "net pass-2 shape" (2A spine, 2D beneath, 2C Island B) is the
  FAIL-branch of the master gate realized — a real negative result, but presented as a converged
  architecture. Grade the lock "canonicalization + BackendShape driver EARNED; extraction/CSP-
  extraction SPEC-ONLY," not "2A ADVANCED."
- **Gates-that-cannot-fail:** PROTO-2's conflict walk is honest and non-vacuous (0 with live
  witnesses). But PROTO-1's PMU clause "was not measurable" — a clause that cannot be run this wave is
  a clause that cannot fail this wave; the master gate's deciding half is unfalsifiable in this
  environment.
- **Elegant-reduction trap — two live instances.** (1) §0.3 — the crossover proves N-sensitivity;
  "and-then-the-hard-part" (perf-tracking under fitted coefficients) is unbuilt. (2) §0.2 — the port
  from GrammarCostModel-in-legacy to skinny's decision seam is the hard, unscoped middle of every
  tree-profile claim; PROTO-2 stepped over it by renaming the gap.
- **Spec-cites-itself circularity:** the family logic rests on independent measurements (Cranelift
  1.13, the 54-config null, PBQP integrality gap) — not present in the argument. The residual cycle
  (2C non-degenerate ⇐ 2D fitted ⇐ ≥2-survivor front ⇐ the master gate 2C's objective would decide) is
  broken by the master gate's half-open state removing a grounded front.
- **Consumer-less substrate (viii/Y.13):** 2C's scan plan has no reader; the ~68-field provenance
  apparatus is dead. Both correctly slated, neither executed.
- **Concrete-or-SPEC-ONLY (vii):** 2D SPEC-ONLY (zero fit); the crossover perf-win SPEC-ONLY (PMU
  unmeasured); `bounded_sum` binding-on-real-costs SPEC-ONLY (synthetic costs). PROTO-2's context-
  freedom = 0 is genuinely concrete and non-vacuous — the pass's cleanest earned result alongside the
  earlier B&B-vs-PBQP resolution.
- **Refutation laws (iv):** honored in form — ratios not absolutes, no warm benches, aarch64-only,
  loadavg recorded and used to caveat.

## Lock-level disposition (critic-2 recommendation)

The honest LOCK-2 after pass-2 is **2A's directed spine + aggressive canonicalization + the existing
`BackendShape` driver (childless argmin, wired in skinny)**, plus **B&B + `bounded_sum` as the
scan-plan solver home (earned as a prototype, NOT landed at HEAD)**, with everything
extraction-distinctive SPEC-ONLY by the lock's own half-open master gate:

- *extraction/CSP-extraction as a DECIDER* — SPEC-ONLY straddling, and its green half (PROTO-1
  crossover) is in a cost model (`GrammarCostModel`) skinny's decision seam does not invoke; the perf
  half is UNMEASURED; the port that would reach the substrate is unscoped. Neither refuted nor
  converged, exactly as CRIT-1 held — the new probe did not move the deciding clause.
- *scan-plan CSP/budget* — solver-shape closed (B&B), propagator built but worktree-only; teeth
  (binding-on-real-costs, IR-modeled sites, live consumer) SPEC-ONLY / two-locks-blocked; soft-
  coherence escape hatch untested.
- *2B* — no distinct existence over 2A (XL-B forecloses the driver seat; master gate straddled;
  context-freedom RATIFIED at 0 — the one real lift); its port deliverable was conceded, not built.
- *2D* — sound discipline, zero executed fit, front exists only in a probe from unfitted coefficients.

**The single highest-value pass-3 directive (cross-lock):** GESTALT-B must first adjudicate the
grammar-e-node duplication (§0.2) — declare whether skinny's decision e-node is the childless
`DecisionNode` (tree-profile thesis dead; I2/I4 canonicalizations forever) or the tree-profile
`GrammarENode` (the port is real, unbuilt, and owed a scoping family). ONLY THEN, in one tree that
holds engine + seams + a landed `bounded_sum`: (1) land the crossover Alt arm into the seam skinny
actually calls and re-fire the master gate on a PMU host so the deciding clause can pass or fail; (2)
build a codegen consumer that reads the scan plan so 2C is not consumer-less; (3) run ONE executed 2D
fit against whatever front (1) grounds. No family may speak of convergence before those land — and
none did this pass. The convergence numbers above are floored by §0.1 (verdicts predate the probes),
§0.2 (the probes proved things in opposite engines; the substrate floor of genuine islands is still
0), and §0.3 (the master gate's deciding half is unmeasured).
