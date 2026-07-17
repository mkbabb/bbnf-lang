# LOCK-2 — THE GRAPH ENGINE — pass-2 CRIT-1 (adversarial critic 1 of 2, within-lock weight)

*Charter: harden/challenge/refine every family against the failure-mode checklist (vacuous
convergence, spec-cites-itself circularity, gates-that-cannot-fail, the elegant-reduction trap,
legacy aliases, masked fallbacks, unverified gestalt, consumer-less substrate, SPEC_ONLY anchoring a
perf claim) + constitution violations + prototype-evidence misuse. Weight: internal soundness of each
family spec and whether the prototypes tested the sharpest uncertainty. All disk claims re-verified
this pass at HEAD `06e70053f` (base-pin `ce51c36fc` confirmed ancestor). This CRIT-1 supersedes the
14:04 CRIT-1, which — like the FAMILY-SPECS — predates the two briefs that actually ran.*

---

## 0. THE HEADLINE DEFECT — the FAMILY-SPECS is stale against its own two decisive prototypes (self-supersession)

The FAMILY-SPECS opens by mocking the 13:10 draft as "written BEFORE its own two decisive
prototypes ran." **The current document (mtime 19:25) commits the identical sin.** Its §0 verdicts
synthesize the *earlier* pass-2 prototype pair — the widening-rewrite master gate (`§0.2`,
`LOCK-2-widen-confluent.patch` @13:25) and the B&B-vs-PBQP solver shape (`§0.1`, `PROTO-2.patch`
@13:40) — and then emit Brief 1 + Brief 2 as "the two sharpest uncertainties for pass 3." **Those two
briefs then RAN, at 19:46–19:47, and now occupy `PROTO-1.md` and `PROTO-2.md` on disk — and the
synthesis was never updated to their results.** Verified by mtime and by content:

| file on disk | mtime | what it actually is | FAMILY-SPECS §0 calls it | stale? |
|---|---|---|---|---|
| `PROTO-1.md` | 19:46 | **PROVE-2-ALTCOST-CROSSOVER** (Brief 1) — RAN | "widening master gate FAIL / SPEC-ONLY straddling" | **YES** |
| `PROTO-2.md` | 19:47 | **PROVE-2-SUBSTRATE-RECONCILE** (Brief 2) — RAN | "solver shape CLOSED (B&B v PBQP)" | **YES** |

Both of the document's declared blocking uncertainties **have results sitting in the same directory
that move the verdicts**, and the synthesis reads as if they are still open. Every per-family verdict
below the fold ("SPEC-ONLY straddling," "context-freedom UNSETTLED," "the binding substrate
contradiction," "challenge routed to GESTALT-B") is written against a knowledge state its own briefs
have overtaken. **This is not a nit — it is the document failing the exact standard it sets.** A
pass cannot be scored on a synthesis that predates its decisive evidence. The numbers below reflect
disk truth as of 19:47, not the 19:25 text.

---

## 1. WHAT THE TWO BRIEFS ACTUALLY PROVED — and where each is softer than its own verdict line

### 1.1 PROTO-1 (altcost crossover) — the structural half is EARNED; the load-bearing half is the elegant-reduction trap
Green, deterministic, honest: one fixed unmodified `CALIBRATED_WEIGHTS` vector yields
2-branch→SEQUENTIAL, 8-branch→DISPATCH, crossing at `N*=3.0`, driven by the shipping coefficients and
**not** the forbidden `dispatch_bonus` sign. The scalar-constant refutation is itself refuted. **Credit
is real: the cost arm is now N-sensitive, which is exactly the structural precondition 2A/2B/2D were
blocked on.** But three defects keep this from earning "extraction-as-decider":

- **(a) The crossover is arithmetic, not evidence (gate-that-cannot-fail).** Two lines with slopes
  `dispatch_branch=0.5 < alt_per_branch=1.5` and a positive intercept `dispatch_table=3.0` cross —
  *necessarily*, at `N* = 3.0/(1.5−0.5) = 3.0`. The prototype proves the arm now *reads* N; it proves
  nothing about whether `N*=3` is the **correct** boundary. The coefficients are hand-authored and
  **never fitted** (2D never ran). So the model discriminates — possibly **wrongly**. "Extraction
  discriminates" ≠ "extraction discriminates truthfully."
- **(b) The PMU half is UNMEASURED and unmeasurable here (elegant-reduction trap).** Brief 1's PASS
  condition bundles "the two lowerings' cyc/B 95% CI excludes 1.00× same-direction at value-starts."
  PROTO-1 delivered only the arithmetic half; the runtime half is blocked by no `css_canon_bench`
  target, no `perf`/`instruments` on darwin-ARM, loadavg 7–17, and **no synthetic-EGraph→runnable-parser
  lowering path**. The document's own §454 gate — "moving a bench at value-starts" — is untouched. The
  load-bearing step reads *"the cost model now discriminates on N (easy) — and then the hard part
  (does that match reality?) is unmeasured."* Classic elegant-reduction.
- **(c) Consumer-less in production (viii/Y.13).** PROTO-1 itself records that `ManufactureDispatchAlt`
  "appears nowhere in `crates/*/src` outside its own definition file," so "production extraction has no
  `None`/`Some` choice point." The rewire is **regen-neutral precisely because it has no live
  consumer.** A cost term that discriminates a synthetic-only twin is a consumer-less substrate until a
  production rewrite manufactures the choice. The synthesis must not read "structural crossover proven"
  as "the lock has a live discriminating decision."

**Honest label: structural precondition EARNED (green test); extraction-as-decider still SPEC-ONLY —
its truth (crossover location) unfitted, its runtime (PMU) unmeasured, its consumer synthetic.** The
FAMILY-SPECS' "SPEC-ONLY straddling" survives *upgraded*: no longer "the arm can't read N" but "the arm
reads N, at an unvalidated point, with no live consumer, and the runtime claim is unmeasured."

### 1.2 PROTO-2 (substrate reconcile) — context-freedom RATIFIED on structure; the "binding blocker" was a false premise; but the reconcile hides the real gap
PROTO-2 is the stronger prototype and it corrects a genuine FAMILY-SPECS error. **Re-verified on disk
this pass:**
- `admits_sink_only(rule_ir, target)` (skinny `passes/src/lib.rs:954`) takes **no caller argument**;
  `target` is one global `TargetFeatures::host()` threaded through the per-rule loop. Context-freedom
  is **structurally** sound and does not depend on grammar count. The walk (0 conflicts) is
  corroboration, not the proof. **2B context-freedom is legitimately RATIFIED on the structural
  argument.** The FAMILY-SPECS "context-freedom UNSETTLED — the real walk was never run" is **stale**.
- **The §0.3 "binding substrate contradiction / three-tree split / challenge routed to GESTALT-B" was
  built on a false premise.** §0.3 asserts "the engine … exists **only** under top-level `crates/` …
  ABSENT (skinny `ir` = `cost.rs`+`lib.rs`) … skinny has no engine at all." **Disk refutes this:**
  `skinny/crates/passes/Cargo.toml` declares `egraph.workspace = true` + `csp-solver.workspace = true`;
  `skinny/crates/passes/src/backend_egraph.rs` builds `EGraph::<DecisionNode>`, runs a rewrite via
  `BackoffScheduler`, and **extracts** via `Extractor::best_node` at a wired seam `select()` called from
  `lib.rs:505`. The generic engine crate **is in skinny and already extracts.** §0.3's "load-bearing
  blocker" conflated "`GrammarENode` not in skinny `ir`" (true) with "no engine in skinny" (**false**).
  PROTO-2's "no GESTALT-B escalation warranted" is correct on this narrow point.

**But PROTO-2's reconcile buries the sharpest within-lock finding, which its own facts + PROTO-1's
together expose (I verified all four disk facts):**

- Skinny's sole `impl Language` is `DecisionNode`, and `DecisionNode` has `children: [Id; 0]` — a
  **zero-length array, childless by construction** (`backend_egraph.rs:157`; `children()` returns the
  empty slice). This is exactly I1, the leaf-min the family's own gate (i) demotes to "not a genuine
  island."
- `GrammarENode` / `GrammarCostModel` — the **child-bearing** node (`Alt(children, dispatch)`) that
  PROTO-1's crossover operates on — exist **only** in top-level `crates/ir/src/egraph/{node,cost}.rs`.
  `grep -rn GrammarENode skinny/crates/` returns **nothing**.
- PROTO-2 itself concludes "`GrammarENode` is welded to the top-level `ir` DAG and is **not** the
  portable organ."
- PROTO-1 ran its crossover on **top-level** `crates/ir/src/egraph/cost.rs` (it names the tree), i.e.
  on the tree XL-A archives at GN-0 — **not** the skinny substrate its brief required.

**Synthesis of the two prototypes (which neither states):** the lock's *distinctive* mechanism —
child-bearing extraction that discriminates on branch count — is demonstrable **only in the tree about
to be archived**, and is **architecturally absent from the surviving substrate**, which carries only a
childless leaf-min. PROTO-2's "DecisionNode suffices, no escalation" is a **masked fallback**: true that
no engine *port* is needed, false that the substrate question is resolved — skinny cannot host the
child-bearing island the lock's thesis requires without authoring a child-bearing node it does not have.
**The binding blocker did not dissolve; it mutated** from "port the engine" (a false framing) to
"skinny's only node is childless, so the master-gate island has no substrate home." No family scopes
*that* either.

- **Non-vacuity caveat on the walk:** the "fleet" is **one grammar**. `find skinny -name '*.bbnf'` = 1
  (`json.bbnf`). PROTO-2's `conflicts=0` covers json alone (15 rules, 2 live witnesses). The structural
  argument carries ratification; the *empirical* "over the fleet" is 1/N. The entire "fleet" vocabulary
  in the FAMILY-SPECS and in the solver-shape prototype (JSON 3 sites, **CSS-L4 12 sites**) references
  grammars that **do not exist as `.bbnf` in skinny** — a standing wrong-tree/provenance flag on every
  "fleet" number in the lock.

---

## Family 2A — EGRAPH-ISLANDS — convergence **52%** (prior CRIT-1: 48%)

Credit: PROTO-1 proved the structural crossover (green, deterministic, single fixed vector) — the
master gate's easy half is now EARNED, up from "the discriminating cost model was never built." The
canonicalization spine + the extant `BackendShape` driver (I1) remain the family's real floor.

**Enumerated open gaps:**
1. **[MASTER, elegant-reduction] Extraction-as-decider is still SPEC-ONLY.** The crossover is arithmetic
   (guaranteed by coefficient signs, §1.1a); `N*=3` is unfitted (may discriminate wrongly); the PMU
   clause (cyc/B CI excludes 1.00× at value-starts) is UNMEASURED and unmeasurable in this environment
   (§1.1b). The green test proves the arm reads N, **not** that the pick is truthful or moves a bench.
2. **[SUBSTRATE, load-bearing, NEW] The child-bearing island has no home in the surviving tree.** The
   only skinny `impl Language` is childless `DecisionNode` (`children:[Id;0]`); the child-bearing
   `GrammarENode` lives only in legacy top-level `crates/ir`, is non-portable (PROTO-2), and is where
   XL-A sends the axe (GN-0). **The count of child-bearing islands with a live consumer in the substrate
   is 0, and cannot become ≥1 without authoring a child-bearing skinny node no family scopes.** I1
   remains the leaf-min the family's own gate (i) demotes.
3. **[consumer-less, viii] The altcost rewire has no production consumer.** `ManufactureDispatchAlt` is
   absent from all production `src`; the discrimination is synthetic-only (§1.1c).
4. **[XL-C] I2 (regex-HIR canon) WITHHELD** pending LOCK-5 keep/abrogate of `rules/regex.rs`.
5. **[XL-D, two-locks-deep] I3 (scan-plan) sites are IR-latent, consumer-BLOCKED** (`SimdMode`/`SimdSite`
   single-valued; `SelfLoop`/`Prefilter` node shape is LOCK-3's; derivation consumer BLOCKED at LOCK-9
   9C / LOCK-5).
6. **[XL-F] I4 (projection canon)** is probably an always-apply canonicalization (PROVE-12 monomorphized,
   no e-graph) double-homed with LOCK-1's `Project` — unresolved whether it is an island at all.

Verdict: **ADVANCED (spine + driver EARNED), master value still SPEC-ONLY** — upgraded in kind, not
closed. The pass proved the arm can read N; it did not prove the reading is right, has a consumer, or
has a substrate home.

---

## Family 2B — IR-IS-EGRAPH — convergence **46%** (prior CRIT-1: 32%)

Credit: the largest move of the pass. **Context-freedom RATIFIED** on the sound structural argument
(re-verified: `admits_sink_only` takes no caller arg; global `target`); the pass-1 NP-hard
BLOCKED-candidate objection stays contingently lifted (repair = bounded sub-rule specialization, not
optimal context-sensitive extraction). The §0.3 "engine absent from skinny" premise is **refuted by
disk** — the engine crate is wired and extracting in skinny.

**Enumerated open gaps:**
1. **[distinct existence] 2B still earns nothing over 2A.** Multiplicity ≈ 1 across the fleet (triple-
   confirmed); "IR IS an e-graph end-to-end" buys only hash-cons dedup today; the maximal reading stays
   XL-B-refuted (LOCK-3's driver seat). Its lawful residual **is** the 2A island posture.
2. **[NEW, load-bearing] The e-graph that would carry 2B's thesis is childless in the substrate.** 2B's
   whole claim is a child-bearing node substrate across the tower; skinny's only `impl Language` is
   `DecisionNode` (`children:[Id;0]`), and the child-bearing `GrammarENode` is non-portable (PROTO-2's
   own finding). 2B-in-skinny is presently a childless bag — the thesis has no substrate.
3. **[empirical thinness] Context-freedom's *walk* is 1 grammar (json).** Ratification rests on structure;
   the "over the fleet" empirical is 1/N — fine as corroboration, mislabeled as fleet coverage.
4. **[gate 4, unmeasured] Whole-grammar organ (build+saturate+extract+teardown) within ~230 µs on CSS-L4
   (2875 states)** — the single most important 2B measurement — **never ran**; `CspScheduler` rebuilds a
   fresh `Csp` per iteration (opposite of the aegraph no-rebuild lesson): a pure-assertion scaling risk.
5. **[total-order] `regen∘regen` through cost-guided extraction at grammar scale** with the f64 +
   content-hash tie-break is owed and untested at that scale (per-json byte-identity ≠ CSS-L4-scale).

Verdict: **context-freedom RATIFIED, distinct existence still unearned, substrate childless.** The
BLOCKED-candidate stays lifted; 2B converges into 2A but cannot yet stand in the surviving tree.

---

## Family 2C — CSP-CENTRAL — convergence **54%** (prior CRIT-1: 52%)

The lock's strongest family and its genuine positive result: the solver-shape question is CLOSED
(B&B + `bounded_sum` native-knapsack optimal on every instance 3–35 nodes/<100 µs; PBQP's Lagrangian
budget-dual carries an irremovable integrality gap at ~10⁴× wall-clock). `bounded_sum` is
MERGED-and-tested (`6/6`). Unaffected by the two new briefs; it stands.

**Enumerated open gaps:**
1. **[XL-D, consumer-blocked] The budget never binds on real IR-modeled sites.** Source says ≥2 competing
   scan sites; the IR says single-valued (`SimdMode`/`SimdSite`); the `SelfLoop`/`Prefilter` bridge is
   LOCK-3's, consumer BLOCKED two locks deep. **The teeth remain SPEC-ONLY.**
2. **[provenance, NEW] The solver-shape prototype ran on graphs whose grammars aren't in the substrate.**
   "CSS-L4 12 sites" — but css_l4 is not among skinny's `.bbnf` (json is the only one). The near-tree /
   treewidth-≤2 evidence and the B&B-optimality numbers were taken on top-level or synthetic instances,
   not the measurement substrate (XL-H wrong-artifact flag). The *verdict* (B&B over PBQP) is robust;
   the *instance realism* is not substrate-anchored.
3. **[escape-hatch] `bounded_sum` entered via `ConstraintEnum::Custom`, merged from a divergent bank tree**
   carrying a `Soft` variant the base lacks. The soft/finite coherence-penalty residual (the one point
   for PBQP) is unrepresentable in base B&B and **untested**.
4. **[2D-degenerate] Without a fitted cost table the objective is `static_size_cost` — "a CSP-shaped
   priority cascade with extra steps."** Non-degeneracy leans entirely on 2D, which has run zero fits.
5. **[XL-G, one-solver-home] Unification with LOCK-1's csp-solver** is a GESTALT-B point; the divergent-
   bank provenance of `bounded_sum` must reconcile first.

Verdict: **EARNED-solver / BANKED-teeth** — solver home is the pass's real convergence; every tooth is
consumer-blocked or 2D-dependent.

---

## Family 2D — PROFILE-CLOSED-LOOP — convergence **34%** (prior CRIT-1: 30%)

Credit: Brief 1 reversed 2D's "resolved adversely" — a discriminable front now provably exists (the
N-crossover). The determinism invariant (loop closes at calibrate-time, regen a pure function of
`(grammar, committed-β)`) is correct; the substrate is ~70% present.

**Enumerated open gaps:**
1. **[concrete-or-SPEC-ONLY, vii] Zero executed fit.** No PROVE-2D ran; the fit-harness, freshness-check
   bodies, and a committed `CALIBRATED_WEIGHTS` derived from PMU are all absent. 2D may not speak of
   convergence until one fit lands (its own gate).
2. **[NEW, sharp] The very crossover 2D exists to produce is currently hand-authored and unfitted.**
   PROTO-1's `N*=3` comes from hand-set coefficients — exactly the authored magic 2D must replace. So 2D
   is now *more* motivated and *no less* unbuilt: the front exists but at an unvalidated point that only
   a fit can justify. "Front exists" is not "front is correctly placed."
3. **[falsifiable freshness] The gate has never been shown to go RED.** Today's `"pass"`/`0` stamps are
   the theater 2D removes; until a proto mutates a coefficient and the gate reddens, the anti-theater
   claim is itself provenance theater.
4. **[unblocked plane is cross-lock-owned] `select_backend` kernel crossover (LOCK-9), `CSS_SHUFTI_MIN_SPAN`/
   `GATE=16` (GN-2)** — fitting these supersedes hand-tuned GN literals: a **named supersession (ix)** the
   spec must declare, not attach freely.

Verdict: **BLOCKED (SPEC-ONLY), composable not exclusive** — front un-blocked, fit still un-run.

---

## Failure-mode checklist — hits found

- **Elegant-reduction trap — HIT (2A/2B/2D crux).** PROTO-1 delivered the arithmetic half of the master
  gate and left "does the pick match reality? (PMU)" unmeasured. The load-bearing step reads
  *and-then-the-hard-part*.
- **Gate-that-cannot-fail — HIT.** The structural crossover is arithmetically guaranteed by coefficient
  signs; the PMU clause cannot be run in this environment. The master gate as-executed neither can fail
  (structural half) nor can complete (PMU half).
- **Consumer-less substrate (viii/Y.13) — HIT ×3.** (a) PROTO-1's altcost rewire has no production
  consumer (`ManufactureDispatchAlt` synthetic-only); (b) 2C's teeth consumer BLOCKED two locks deep
  (XL-D); (c) 2D's fitted coefficients have no live decision to flip.
- **SPEC_ONLY anchoring a perf claim — WATCH/near-HIT.** PROTO-1 correctly labels the runtime
  superiority "well-established microarchitectural fact … not asserted here as measured." The synthesis
  must not upgrade "structural crossover proven" into "extraction-as-decider EARNED" — that would anchor
  the master perf claim on an unmeasured half.
- **Masked fallback — HIT.** PROTO-2's "DecisionNode suffices, no GESTALT-B escalation" masks that
  DecisionNode is childless and structurally cannot carry the master-gate island; the substrate blocker
  mutated rather than dissolved.
- **Spec-cites-itself circularity — SOFT HIT.** `N*=3` is "documented at `cost_weights.rs:87-88`"; the
  prototype confirms the code matches the comment, but the comment/coefficients are hand-authored — the
  crossover validates against itself, not against measured runtime.
- **Stale synthesis (document-integrity) — HIT.** §0 (the load-bearing verdicts) predates the two briefs
  it prescribed and that then ran (§0 above). The FAMILY-SPECS must be re-cut to disk-as-of-19:47 before
  any convergence is scored.
- **Unverified gestalt — N/A this pass** (XL-B WORKING, correctly deferred to GESTALT-B).

---

## Routing / what pass 3 must carry (within-lock)

1. **Re-cut the FAMILY-SPECS §0 to the two briefs' actual results** (PROTO-1 = altcost crossover
   structural-EARNED/PMU-UNMEASURED; PROTO-2 = substrate-reconcile PASS, context-freedom RATIFIED,
   §0.3 "engine absent from skinny" retracted as disk-false). No score before this.
2. **Scope the real substrate gap** (supersedes §0.3's false "port the engine"): author a **child-bearing
   skinny node** or accept that the master-gate island lives only in the archived legacy tree. This is
   the lock's true unscoped core and belongs to GESTALT-B **with the corrected premise** (engine crate
   is in skinny; only the child-bearing node is not).
3. **The PMU half of the master gate needs a real host** (perf/instruments, quiet machine, a
   synthetic→runnable lowering) — until then extraction-as-decider stays SPEC-ONLY regardless of the
   green structural test.
4. **2C's teeth and 2D's first fit** remain the two consumer/evidence gates; neither moved this pass.

**Net (disk-truth, 19:47):** the pass earned two genuine results — the B&B/`bounded_sum` solver home
(2C, unchanged) and 2B context-freedom (structural, RATIFIED) — plus one honest structural
half-result (PROTO-1's N-crossover). It did **not** earn extraction-as-decider (arithmetic + unfitted +
unmeasured + consumer-less), and it **surfaced a harder substrate finding than the one it retired**:
the child-bearing island the lock's thesis needs exists only in the tree XL-A archives, and the
surviving substrate carries a childless leaf-min. The lock is a converging core with **four exact
gaps** (master PMU, substrate child-node home, XL-D teeth, 2D first fit) — not a convergence.
