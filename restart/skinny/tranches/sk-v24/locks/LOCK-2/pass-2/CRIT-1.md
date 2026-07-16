# LOCK-2 — THE GRAPH ENGINE — pass-2 · CRIT-1 (Adversarial critic 1 of 2, WITHIN-LOCK weight)

*Charter: harden/challenge/refine every family against the failure-mode checklist — vacuous
convergence, spec-cites-itself circularity, gates-that-cannot-fail, the elegant-reduction trap,
legacy aliases, masked fallbacks, unverified gestalt, consumer-less substrate — plus constitution
violations (grammar totality, isomorphism, refutation laws, zero-runtime-cost types, parsimony) and
prototype-evidence misuse (SPEC_ONLY anchoring a perf claim). Critic 1 weights the WITHIN-LOCK view:
internal soundness of each family spec and whether the two prototypes actually tested the sharpest
uncertainty. Read this pass: PORTFOLIO (LOCK-2 + CONSTITUTION), FAMILY-SPECS (13:10), PROTO-1
(13:25) + its patch, PROTO-2 (13:40) + its patch, research-{A,B}, pass-1 CRIT-1/CRIT-2, CROSS-LOCK,
and — the load-bearing step — the actual on-disk layout of BOTH the top-level `crates/` workspace and
the `skinny/crates/` workspace at base-pin `ce51c36fc`.*

---

## 0. TWO GOVERNING WITHIN-LOCK FINDINGS — both decision-grade, neither folded into the spec

### 0.1 The FAMILY-SPECS predates its own decisive prototypes and still speaks in the pre-resolution conditional

Timestamps on disk: `FAMILY-SPECS.md` = 13:10; `PROTO-1.md` = 13:25; `PROTO-2.md` = 13:40. **The
synthesis was written BEFORE the two experiments it turns on ran.** Its §0 states the governing
conditional verbatim: *"until it fires, the honest LOCK-2 is 'canonicalization + the existing
BackendShape driver,' with all extraction/CSP-extraction machinery recorded SPEC-ONLY."* Brief 1 has
now fired — as PROTO-1 — and **returned the FAIL branch** (`picks identical` across the calibrated
weight space). Per the FAMILY-SPECS' own pass/fail contract and PROTO-1's own verdict line: *"LOCK-2
collapses to canonicalization + the `BackendShape` driver; extraction / CSP-extraction is
SPEC_ONLY."*

The consequence is not a nuance — it is the lock's shape. **The conditional has resolved to the
collapse branch, and the spec's family verdicts are all written pre-resolution.** 2A is graded
"ADVANCED — the lock's spine"; 2B "converging into 2A"; 2C "BANKED"; 2D "BLOCKED". Those grades were
assigned before the master gate ran. They must be re-read as: *the spine (directed pipeline +
canonicalization) survives; every distinctive extraction/CSP-extraction claim (2A-I2/I4 tree
profiles, 2B-extraction-is-the-driver, 2C's non-degenerate objective, 2D's shape-lattice) is now
SPEC-ONLY by the lock's own gate, not merely "gated pending an unrun brief."* The synthesis has not
been updated to say this. That is a staleness defect at the document's load-bearing joint.

### 0.2 The cross-tree provenance error CRIT-1 pass-1 flagged as "the pass's single most load-bearing defect" REPEATS in pass-2 — PROTO-1 ran the master gate on the wrong tree, and its context-rider grepped the wrong tree

Verified on disk this pass. **Two separate cargo workspaces coexist at base-pin `ce51c36fc`:**

| | top-level `crates/` | `skinny/crates/` |
|---|---|---|
| own `Cargo.toml`? | yes (root workspace) | yes (`skinny/Cargo.toml`, **not referenced by root** — count 0) |
| `bbnf-ir` package | **here** (`crates/ir`, name `bbnf-ir`) | package name is `ir`, not `bbnf-ir` |
| `egraph/` module (GrammarENode, `GrammarCostModel`, rules/) | **EXISTS** (`crates/ir/src/egraph/`) | **ABSENT** (skinny `ir` = cost.rs + lib.rs only) |
| `passes/` crate (`backend_egraph.rs`, `decision_csp.rs`, `admits_sink_only`, `derive_recognizers`, `SimdMode`/`SimdSite`) | **ABSENT** (no `crates/passes` member) | **EXISTS** (`skinny/crates/passes/`) |
| production-wired | yes (`crates/core/.../pipeline.rs`) | separate workspace |
| XL-A label | **legacy** ("archives at GN-0") | **measurement substrate** ("only living implementation") |

The engine LOCK-2 optimizes (`GrammarENode`/`GrammarCostModel`/the rewrite rules) lives **only in the
top-level tree** — the tree XL-A labels *legacy*. The decision seams the FAMILY-SPECS wires (`passes/
backend_egraph.rs`, `SimdMode`/`SimdSite`, `admits_sink_only`) live **only in skinny** — the tree
XL-A labels *the substrate* — which has **no engine at all**. **No single tree has both.** Three
concrete consequences, each decision-grade:

1. **PROTO-1 ran the master gate against the top-level (XL-A-legacy) engine.** Its command is
   `cargo test -p bbnf-ir --test egraph widen_confluent`; it added `crates/ir/src/egraph/rules/
   widen.rs`; the `GrammarCostModel` whose Alt-arm it indicts exists **only** in top-level `crates/
   ir`. Brief 1's method text explicitly required *"Skinny worktree … IN the skinny measurement
   substrate (XL-A)."* The skinny `ir` package has no egraph module to extend, so PROTO-1 could not
   have satisfied that clause — and did not disclose the substitution. **The FAMILY-SPECS §0 itself
   listed pass-1 PROTO-1's defect (a) as "ran on the legacy tree" and named the fix as "in the skinny
   substrate"; PROTO-1 pass-2 fixed (b) hand-union and (c) confluence/termination but left (a)
   unfixed.** The sharpest uncertainty was tested, for the second consecutive pass, on the tree that
   is missing half the seams the family cites.

2. **PROTO-1's context-conflict rider is a wrong-tree artifact.** It reports *"`admits_sink_only` has
   no implementation in the tree (`grep -rn admits_sink_only crates/` → 0 hits) … Rider result: 0
   conflicts → context-free extraction confirmed."* But `admits_sink_only` **does** exist — in
   `skinny/crates/passes/src/lib.rs`, the tree PROTO-1 was not grepping. So the rider that Brief 1
   introduced *precisely to settle 2B's context-freedom* declared the predicate nonexistent because it
   looked in the top-level tree, then substituted a "structural confirmation" from the top-level
   `GrammarCostModel`. **2B's context-freedom is therefore UNSETTLED, not "confirmed"** — the caller-
   conflict walk over the real `admits_sink_only` was never run.

3. **The two-tree reconciliation — "adopt-and-prove / port the engine into skinny" — remains unscoped
   by any family, exactly as CRIT-1 pass-1 named it.** To build Island B (or any of the lock's live
   decision sites) end-to-end you need the engine (top-level) AND the decision seams (skinny) in one
   tree. No family scopes that port. The FAMILY-SPECS §0 XL-A bullet asserts the reconciliation is
   trivial ("prove the organ's behavior on the skinny substrate; design its home in greenfield
   layout"), but the organ (the engine) does not exist in the substrate, so "prove it on skinny" is
   not presently executable.

**Challenge to route to GESTALT-B (per CROSS-LOCK protocol, with evidence):** XL-A declares "skinny@
HEAD is the only living implementation; legacy top-level `crates/` archives at GN-0," yet on disk the
top-level `crates/` workspace is the compiling, production-wired tree that owns the e-graph engine,
while `skinny/crates/` is a separate workspace unreferenced by root. Either (a) PROTO-1/PROTO-2 ran on
XL-A-legacy and their verdicts inherit the wrong-tree defect, or (b) XL-A's legacy/substrate labels
are inverted relative to disk and the FAMILY-SPECS §0 framing rests on a mislabeled foundation. Both
readings are damaging; both must be resolved before either prototype's verdict is "decision-grade."
(Pass-1 CRIT-2 already recorded the portfolio-wide tree-of-record contradiction — LOCK-1 "skinny
retires" vs LOCK-3/5/7 "skinny is the live tree of record"; this is that contradiction biting the
prototypes.)

### 0.3 What the master gate actually showed — subtler and LESS decisive than §0's "inert substrate" prediction

Credit where due, because it sharpens the critique rather than blunting it. **PROTO-1's egraph limb
PASSED** — the piece pass-1 left unbuilt is now built and green: a *registered* `ManufactureDispatchAlt`
`Rewrite`, fired through `CspScheduler` (not hand-unioned), manufactures **≥2 semantically-distinct
survivors at a child-bearing `Alt`** (`none_forms==1, some_forms==1`), stays **confluent** with the
shipping all-narrowing set (`DeduplicateAltBranches` ran every iteration, did 0 work, did not collapse
the dispatch twin — it keys on the branch set, blind to the dispatch table), and **terminates** with
wide margin (`iters=2/64, nodes=20/100000`). This refutes §0's implicit claim that the substrate is
*inert*. It is not inert; it manufactures confluent alternatives.

The FAIL is located entirely at **extraction under calibration**, and its cause is **named and
fixable**: `GrammarCostModel::cost`'s `Alt` arm is `base [+ dispatch_bonus if Some]`, where `base` is
identical across the twin (same children, same N), so `cost(Some) − cost(None) ≡ dispatch_bonus` — a
scalar **constant in N**. The model never reads `dispatch_table`/`dispatch_branch` for an `Alt`; there
is no setup-vs-savings amortization, no branch-count crossover. Only the *sign* of one knob flips the
pick; no interior of the calibrated space does. PROTO-1 §4 itself states the fix — *"extend the `Alt`
arm to net `dispatch_table` against `N·dispatch_branch` vs `N·alt_per_branch` — a branch-count-
dependent term … deliberately out of scope for a parsimonious probe."*

**This makes the collapse verdict OVER-READ its evidence, in the elegant-reduction-trap sense.** The
honest statement is *"today's cost model cannot discriminate the twin under calibration,"* not
*"extraction is architecturally inert."* The load-bearing next step — a branch-count-dependent cost
term, then re-measure — is cheap, named, and **untested**. The lock cannot claim the extraction thesis
is *refuted* (it is not; the discriminating cost model was never built) nor *converged* (the
discriminating cost model was never built). It is genuinely SPEC-ONLY, straddling. The confluence
result is also narrow: it holds because `DeduplicateAltBranches` is blind to the dispatch dimension —
a different narrowing rule that normalized on dispatch presence could collapse the twin, so
"confluent with the current set" is proven, "confluent in general" is not.

---

## Family 2A — EGRAPH-ISLANDS — convergence 48%

The directed-spine + local-islands architecture is coherent, evidence-aligned, and — post-XL-B — the
lawful home of 2B's surviving mechanism. Gate (i) ("produce child-bearing nodes OR be genuinely
constrained, else write it as a plain cost-ranked choice, do NOT island it") is a real gate that can
go RED, and PROTO-1's egraph limb is a genuine advance over pass-1's hand-union. Credit all three. But
the master gate that the family's *distinctive* value rides on has now FAILED, its provenance is
cross-tree-broken (§0.2), and by the family's own predicate (1) its honest floor of genuine islands
is **0, not 1**.

**Enumerated open gaps:**
1. **[MASTER — now RESOLVED to the FAIL branch] I2/I4 tree-profile extraction is SPEC-ONLY, not
   "gated."** PROTO-1 shows extraction does not discriminate a confluent child-bearing twin under any
   calibrated vector. The tree profile is dead code until a branch-count-dependent cost term exists
   (§0.3) — which is named and untested. The family's *distinctive* extraction value is presently
   unearned.
2. **[§0.2] The master gate ran on the top-level engine; Island B's seams are in skinny; the port is
   unscoped.** `backend_egraph.rs`/`decision_csp.rs`/`SimdMode` (skinny) and `GrammarCostModel`/rules
   (top-level) are in different workspaces. "Same-wave consumer that changes emitted code" cannot be
   demonstrated until they are one tree, and no family scopes the unification.
3. **I1 — the "honest floor" island — fails the family's OWN predicate (1).** `backend_egraph::select`
   builds `EGraph<DecisionNode>` over 2–5 **childless** leaves, unions them, extracts `min` — which is
   `argmin` over a candidate list with no congruence sharing (research-A §2 calls it "the family's own
   refutation of predicate (1)"; the `decision_csp` step re-solves a 1-variable CSP that pins the
   already-selected index). By gate (i) I1 should be **demoted to a plain cost-ranked pass**, not
   counted as an island. The count of genuine child-bearing extraction islands with a live consumer in
   the substrate is **0**.
4. **[XL-C] I2 (regex-HIR canon) is a WITHHELD cross-lock contribution.** Homed in parse-that/LOCK-5;
   the lawful `Hir` is LOCK-5's to render; the shared `rules/regex.rs` exemplar's keep/abrogate is
   LOCK-5's verdict. Untested here and off this lock.
5. **[XL-D, Brief 2] I3 (scan-plan) sites are IR-latent and consumer-BLOCKED two locks deep.** Skinny
   IR models a single `SimdMode::Exact`/`SimdSite::PreEntry` (confirmed on skinny@HEAD by pass-1
   CRIT-2); `Prefilter`/`SelfLoop` node shape is LOCK-3's to build; the derivation consumer is BLOCKED
   through LOCK-9 9C / LOCK-5. Research-A's own §3/Q5 verdict is that scan selection is *a lowering
   predicate, not an island* (multiplicity ≈ 1 per site) — i.e. gate (i) demotes I3 too, absent a
   binding Exact-vs-Prefilter tie the fleet has not exhibited.
6. **I4 (projection canon) is probably a canonicalization, not an island, and double-homes LOCK-1.**
   PROVE-12 residualized it with monomorphization, not an e-graph (research-A §3, I3-of-that-report);
   `map∘map`/dead-projection are always-apply → plain rewrites by gate (i); the LOCK-1 `Project`
   double-home is a name-gate unless LOCK-1 ships an e-graph, which its disposition says it will not.
7. **GN-2.W1 supersession (XL-F "wraps") is stated but its consumer is unbuilt** — a born-RED gate on
   an unbuilt consumer cannot fail this wave; it must be marked GN/LOCK-9-owned, not LOCK-2-earned.
8. **Ceremonial-deletion gate (v) unexercised** — the ~68-field provenance apparatus deletion is
   asserted, not demonstrated; masked-fallback watch item until it lands.

## Family 2B — IR-IS-EGRAPH — convergence 32%

The maximal reading is cross-lock-refuted by XL-B (extraction cannot be the decision driver — that
seat is LOCK-3's), and the pass-2 spec honestly concedes this, retreating to "the 2A island posture."
The `Ref`-as-leaf / per-rule root-table insight (acyclic body e-graph, terminating greedy extraction)
is genuinely sound — credit. But the family's distinctive thesis is now refuted by the master gate,
and its key structural-soundness claim was mis-tested on the wrong tree.

**Enumerated open gaps:**
1. **[MASTER — RESOLVED to FAIL] "extraction emits `BackendShape` directly" is refuted for the
   shipping cost model.** PROTO-1's extraction limb is precisely this claim's test, and it fails: the
   e-graph is inert as a *decider* under calibration (though not as a *representer* — §0.3). Combined
   with XL-B foreclosing the driver seat and the budget-boundary forcing termination in a solver
   (research-B §6), 2B has no distinctive existence over 2A. "Converging into 2A" is the sound
   disposition.
2. **[§0.2] Context-freedom — the load-bearing structural-soundness claim — is UNSETTLED, not
   "confirmed."** PROTO-1's rider grepped `crates/` (top-level), found `admits_sink_only` absent
   (because it lives in skinny), and substituted a top-level structural confirmation. The caller-
   conflict walk over the real predicate was never run. The spec's claim that context-freedom is
   "structurally sound, confirmed by Brief 1's cheap CallRule rider" is not backed by what PROTO-1
   ran. The NP-hard objection is *contingently* moot only if the walk actually returns 0 conflicts —
   which is untested.
3. **Whole-grammar organ within ~230 µs incl. teardown — UNTESTED (the spec's own "single most
   important open measurement").** No proto stood the full-IR e-graph up over CSS-L4 (2875 states) and
   timed build+saturate+extract+**teardown**; the 25.52% `drop_in_place` finding recurs on Alt-heavy
   grammars; `CspScheduler` rebuilds a fresh `Csp` per iteration — the opposite of aegraph's no-rebuild
   lesson. Pure assertion.
4. **regen bit-identity through cost-guided extraction — UNTESTED at grammar scale.** `GrammarCostModel`
   returns `f64` (PartialOrd) with Vec-insertion tie-break (research-A §4.3); the total-order/structural-
   hash fix is a stated obligation with zero grammar-scale evidence. `DecisionCost`'s 6-tuple does it
   right; `GrammarCostModel` does not — currently fragile.
5. **AC-normal-form confluence + the extraction-crossover exhibit do not exist in-IR.** One-directional
   `Seq`/`Alt` normal-forming is asserted confluent, untested; the "greedy busts the budget, solver
   recovers" exhibit was built by PROTO-2 over *synthetic scan sites in csp-solver*, not the IR e-graph
   2B posits.
6. **Mis-scoped by the two-tree split (§0.2):** in skinny the grammar IR is not a `Language` (net-new);
   in top-level it already is and is wired. 2B's real deliverable is *adopt-and-prove the port*, which
   no family scopes and no proto exercised.

## Family 2C — CSP-CENTRAL — convergence 52%

The pass's genuine positive result lives here. **PROTO-2 resolved Brief 2 / Q6 decisively**, and the
`bounded_sum` propagator is now merged and tested (6/6). This is the strongest-earned family. But its
entire *teeth* — a binding cross-site budget on real costs, over IR-modeled sites, read by a real
consumer — remain SPEC-ONLY, and the solver-home verdict carries an untested escape hatch.

**What is now EARNED (credit, folded into the grade):**
- **Solver home resolved:** on the fleet's *actual* near-tree scan-plan graphs (JSON 3 sites; CSS-L4
  12 sites, coherence a forest), PBQP fully reduces (**RN=0, zero fallbacks, ~1–5 µs**) but its
  Lagrangian budget-dual carries an **irremovable integrality gap** (css-l4 slack: obj 39.003 vs the
  true 21.011) and runs ~10⁴× slower; **B&B + `bounded_sum` is optimal on every instance in 3–35
  nodes / <100 µs.** The near-tree premise is CONFIRMED (the "irreducibly dense" FAIL branch is
  refuted). **Verdict: B&B-native-knapsack is the home; PBQP is ceremony.** This closes 2C's Q6 and
  the GESTALT-B "one solver" coordination point.
- `bounded_sum` merged into `crates/csp-solver/src/constraint/` via the `Custom` escape hatch; the
  regression floor (`cargo test -p csp-solver --test bounded_sum` = 6/6) held.

**Enumerated open gaps:**
1. **[XL-D, Brief 2] Budget-binds-on-REAL-costs is unproven.** PROTO-2 used *synthetic* per-site costs
   (admissible for the solver-*shape* question, and PROTO-2 flags this) — the budget-binding realism is
   XL-D-blocked two locks deep. Whether a *real* static-size cost vector makes the budget bind over ≥2
   real sites is untested.
2. **The ≥2 IR-modeled competing scan sites do not exist in skinny IR** (`SimdMode`/`SimdSite` single-
   valued — pass-1 CRIT-2 on skinny@HEAD). The `.bbnf` grammars with multiple shufti loops are not
   under the skinny budget. Exposing the sites is GN-2.W1, unbuilt.
3. **Consumer-less (viii/Y.13).** No codegen consumer reads the scan plan; `regen∘regen` byte-identity
   holds only because the plan perturbs nothing wired (PROTO-2 did not even run `check-json` — "zero
   grammar/generated files touched"). The parity/determinism gate is checked at a decision layer with
   no downstream.
4. **The B&B-wins verdict is contingent on HARD coherence — the soft-penalty escape hatch is untested
   and unrepresentable in the base tree.** PROTO-2 §5: if coherence is a *finite preference* penalty,
   that term is native to PBQP's pairwise matrix but has **no representation in the base B&B** (the
   `ConstraintEnum` lacks the `Soft` variant the divergent bank carries). The one residual point in
   PBQP's favor is deferred, not closed. So "B&B is the home" is proven *for hard coherence*, open for
   soft.
5. **Provenance sprawl — a THIRD divergent tree.** The `bounded_sum` bank was "cut against a divergent
   tree carrying `soft.rs`/`cardinality.rs`/a `Soft` variant the base-pin tree does NOT." Alongside the
   two workspaces of §0.2, this is a third tree state feeding the lock's evidence; the adaptation
   (drop `backjumping`, add `cancel`) is undisclosed drift the gestalt must reconcile.
6. **Degenerate without 2D.** Absent a fitted cost table the objective is `static_size_cost` — "a
   CSP-shaped priority cascade with extra steps." 2D is SPEC-ONLY (below), so 2C's non-degeneracy leans
   on an unrun family.

## Family 2D — PROFILE-CLOSED-LOOP — convergence 30%

SPEC-ONLY (vii): **no PROVE-2D prototype ran** — only Briefs 1 and 2 have PROTO files. The loop
discipline is sound, the AutoFDO/BOLT precedent correct, and the load-bearing determinism invariant
(the loop closes at *calibrate-time, not regen-time*, so `regen∘regen` survives and all load noise
lives in `xtask fit-cost`) is right and non-negotiable. Credit both. But under vii a family carrying
perf claims with zero running artifact is "status report," and its master-gate dependency has now
resolved *adversely*.

**Enumerated open gaps:**
1. **[MASTER — BINDING, now confirmed adverse] There is no ≥2-survivor decision to fit.** PROTO-1
   confirms that even with a widening rewrite manufacturing confluent survivors, the shipping cost
   model cannot discriminate them under calibration → multiplicity-that-a-coefficient-can-flip ≈ 1.
   2D is consumer-less until the cost-model extension (§0.3) manufactures a real front — doubly unbuilt
   (cost-model change + fit). PROTO-1 hands 2D a *path* (fit `dispatch_table`/`dispatch_branch` at a
   branch-count crossover) but does not walk it.
2. **Zero executed fit (gate 1).** "A fitted coefficient FLIPS a decision AND beats the hardcoded lane
   cold-paired" — unrun. PROTO-1's W1/W2 is a manual weight swap over a cost model that *cannot* flip,
   not an OLS/ridge fit; it establishes weight-*insensitivity* at the interior, the opposite of what
   2D needs.
3. **Attribution underdetermination is named, not solved.** Whole-grammar cyc/B underdetermines per-
   decision β; the A/B-differential (one lever per paired arm) is the proposed primitive but the fit
   harness body does not exist; the identifiability ceiling (count of independently-toggleable levers
   with a paired bench) is undisclosed.
4. **The only *unblocked* plane touches knobs another lock owns** (`CSS_SHUFTI_MIN_SPAN`, `GATE=16`,
   `select_backend` crossover — LOCK-9/GN-2). Fitting these supersedes hand-tuned GN literals — a
   named supersession (ix) the spec must declare, not attach freely.
5. **Falsifiable freshness (gate 4) unbuilt.** "Mutate a coefficient → gate goes RED,
   `check-calibration` fails" is the anti-provenance-theater guard 2D promises; the current
   `"pass"`/`0` stamps are the lie it removes. No prototype showed the gate can go RED — so the
   anti-provenance-theater claim is *itself, presently, provenance theater*.
6. **`from_env` grep-gate asserted, not demonstrated** — the two-cost-source masked-fallback fence
   (`BBNF_COST_*` out of the regen path) is stated; no CI gate exists yet.

---

## Cross-cutting adversarial notes (checklist + constitution)

- **Vacuous convergence:** the FAMILY-SPECS' "net pass-2 shape" (one frame — 2A spine — 2D beneath,
  2C's Island B the scan seam) is actually the FAIL-branch of its own master gate, now realized. That
  is not vacuous — the collapse is a real, decision-grade outcome — but the spec presents it as a
  *converged architecture* rather than as *the negative result the gate returned*. Grade the lock as
  "canonicalization + BackendShape driver EARNED; extraction/CSP-extraction SPEC-ONLY," not "2A
  ADVANCED."
- **Spec-cites-itself circularity:** NOT present in the family logic — §0 rests on independent
  measurements (in-tree 54-config null + Cranelift 1.13/2-of-4M + the shipping all-narrowing set) and
  PROTO-1/PROTO-2 are external-to-the-argument experiments. This discipline is the pass's genuine
  strength; I do not fault it. The circularity risk is elsewhere: 2C non-degenerate ⇐ 2D fitted ⇐ a
  ≥2-survivor front ⇐ the master gate 2C's own objective would have decided — a dependency cycle that
  the master-gate FAIL breaks by removing the front entirely.
- **Gates-that-cannot-fail:** PROTO-1's egraph-limb gates are honest and one (extraction) genuinely
  failed. But (a) PROTO-1's "context-free confirmed" rider *could not have failed* — it grepped a tree
  without the predicate (§0.2); a rider that verdicts on the absence of a file it looked for in the
  wrong directory is a gate that cannot fail. (b) The GN-2.W1 born-RED gates (≥8× skip, ≥0.95× dense)
  gate an *unbuilt* consumer — cannot fail this wave; mark GN/LOCK-9-owned.
- **Elegant-reduction trap:** two live instances. (1) §0.3 — "extend `GrammarCostModel`'s Alt arm with
  a branch-count term, then re-measure" is the load-bearing step between the FAIL and any PASS, and it
  is unbuilt; the collapse verdict reads-and-then-the-hard-part. (2) 2B's "adopt-and-prove the port" —
  the port between the two workspaces is the hard, unscoped middle of every 2B/Island-B claim.
- **Consumer-less substrate (viii/Y.13):** PROTO-2's scan plan has no reader (2C-gap 3); the ~68-field
  provenance apparatus (2A-gap 8) is dead until a plan-reading consumer exists. Both correctly slated
  for deletion/wiring, neither executed.
- **Concrete-or-SPEC-ONLY (vii):** 2D must carry SPEC-ONLY (no artifact). PROTO-2's synthetic-cost
  budget-binding must carry SPEC-ONLY for the *binding* claim (PROTO-2 does flag it). PROTO-1's PMU
  limb was correctly *not run* (moot after the extraction FAIL) — honest. But PROTO-1's confluence
  result is real and should be credited as the pass's second earned artifact (after PROTO-2's solver
  resolution).
- **Prototype-evidence misuse:** PROTO-1 is anchored in the FAMILY-SPECS §0 as the pass's master
  experiment "de-risks or refutes 2A-I2/I4, 2B-extraction, and 2D-lattice at once." It does — but (i)
  on the top-level engine, not the skinny substrate Brief 1 required (§0.2); (ii) its context rider is
  a wrong-tree artifact; (iii) its extraction FAIL is a cost-model-expressiveness gap the probe itself
  names a fix for and declines to test (§0.3), so "refutes" over-reaches — "leaves SPEC-ONLY straddling"
  is the honest label.
- **Extant-codex supersession (ix):** GN-2.W1 "wraps" (XL-F) is named — good. The SHUFTI/GATE fits
  (2D) are still "attach," not named supersessions.
- **Parsimony/colocation (vi):** `bounded_sum` correctly homed in `csp-solver` — clean. The two-
  workspace split and the third (bank) tree are the parsimony violation the gestalt must adjudicate:
  the lock cannot claim a coherent substrate while its engine, its decision seams, and its banked
  constraint live in three different tree states.
- **Refutation laws (iv):** honored in form (ratios not absolutes, no warm benches, aarch64-only).
  PROTO-1/PROTO-2 ran at loadavg ~5–10; PROTO-2's fine-λ-grid correction (the coarse pass's obj-120
  was a grid artifact) is a model of honest self-correction — credit.
- **Zero-runtime-cost types (v):** not stressed this pass; the provenance-apparatus deletion is the
  only clause-v item and it is unexercised.

## Lock-level disposition (critic-1 recommendation)

The honest LOCK-2 after pass-2 is **2A's directed spine + aggressive canonicalization + the existing
BackendShape driver**, plus **B&B + `bounded_sum` as the scan-plan solver home** (PROTO-2's earned
result), with everything extraction-distinctive recorded SPEC-ONLY by the lock's own master gate:

- *extraction/CSP-extraction as a DECIDER* — SPEC-ONLY, straddling: PROTO-1 built the confluent
  widening rewrite (egraph limb green) but showed the shipping `GrammarCostModel` cannot discriminate
  the twin under calibration; the fix (branch-count-dependent Alt cost) is named and untested. Neither
  refuted nor converged.
- *scan-plan CSP/budget* — mechanism proven (`bounded_sum` merged, 6/6; solver home resolved to B&B),
  but binding-on-real-costs, IR-modeled sites, and a live consumer are SPEC-ONLY / two-locks-blocked;
  the soft-coherence escape hatch is untested.
- *2B* — does not survive as a distinct family (distinctive thesis master-gate-refuted, maximal reading
  XL-B-foreclosed, context-freedom UNSETTLED on the wrong tree); "converging into 2A" is the sound
  disposition, but the port that would realize it is unscoped.
- *2D* — sound discipline, right determinism invariant, zero executed fit, master-gate dependency
  confirmed adverse (no front to fit).

**The single highest-value pass-3 directive (within-lock):** reconcile the two workspaces FIRST —
port/adopt the top-level `crates/ir/egraph` engine into the skinny substrate (or authoritatively
declare which tree IS the substrate and delete the other's duplicate) so that engine + decision seams
live in one tree — then, in that tree: (1) extend `GrammarCostModel`'s `Alt` arm with the branch-count
term PROTO-1 named and re-fire the master gate (does a *single fixed calibrated vector* pick DISPATCH
at large N and SEQUENTIAL at small N?); (2) run the real `admits_sink_only` caller-conflict walk (does
the fleet have a context conflict, settling 2B's context-freedom for real); (3) build a codegen
consumer that reads PROTO-2's scan plan so 2C is not consumer-less; (4) run ONE executed 2D fit
against whatever front (1) manufactures. No family may speak of convergence before those four land —
and none was tested this pass. Convergence percentages above are floored by §0.1 (verdicts predate the
prototypes) and §0.2 (both prototypes carry wrong-tree provenance).
