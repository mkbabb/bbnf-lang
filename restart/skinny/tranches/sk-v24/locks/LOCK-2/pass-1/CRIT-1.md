# LOCK-2 — THE GRAPH ENGINE — pass-1 · CRIT-1 (Adversarial critic 1 of 2, WITHIN-LOCK weight)

*Charter: harden/challenge/refine every family against the failure-mode checklist — vacuous
convergence, spec-cites-itself circularity, gates that cannot fail, the elegant-reduction trap,
legacy aliases, masked fallbacks, unverified gestalt, consumer-less substrate — plus constitution
violations and prototype-evidence misuse (SPEC_ONLY anchoring a perf claim). Critic 1 weights
internal soundness of each family spec and whether the prototypes tested the sharpest uncertainty.*

Code claims independently re-verified this pass (skinny/crates, non-worktree HEAD):
`ir/src/lib.rs:423-432` — `SimdMode{ Exact }`, `SimdSite{ PreEntry }` are **single-valued**;
`SimdSite::SelfLoop`/`SimdMode::Prefilter` appear **nowhere** in `passes/`, `ir/`, `codegen/`.
`passes/src/lib.rs:590` — `perf_cost: 0` (hardwired, ×5 sites). `passes/src/decision_csp.rs:207` —
constraint returns `value == selected_index` (re-selects its input). `passes/src/backend_egraph.rs:58-64`
— every candidate `union`ed into one class (bag, not congruence). `branch_cycbench.rs:172,185` —
iterates `for &b in &data` over **every corpus byte**, not value-start positions. All §0 substrate
claims stand. My critique is not that §0 is wrong; it is that the **two prototypes do not lift the
families as far as their PASS/PASS headline reads**, and the FAMILY-SPECS verdicts (authored 04:46,
before the protos ran at 04:57/05:01) are stale w.r.t. the evidence that now exists.

---

## THE GOVERNING FINDING — the master gate was satisfied by construction, not by the fleet

The whole lock is gated on §0: *does a rewrite manufacture a genuine e-class alternative such that
the Extractor picks differently under two weight vectors AND the picks produce a measured cyc/B
delta?* PROTO-1 returns PASS. Three within-lock defects make that PASS **weaker than the
canonicalization/extraction distinction the lock actually turns on** — and, read honestly, PROTO-1's
own data lands on the *canonicalization-suffices* side (the FAIL branch's architectural conclusion),
not the extraction side it claims to anchor.

1. **The rewrite was never fired — the alternative was hand-`union`ed.** PROTO-1's own candor:
   "Part A builds the e-class by direct `union` rather than firing a registered `Rewrite` impl … the
   *construction* is hand-authored, the *extraction* is production." The gate's literal subject — *a
   rewrite* manufactures the alternative — is not exercised. The `ManufactureDispatchAlt` rewrite
   (search: `Alt(_,None)` with disjoint FIRST; apply: union the `Some(AltDispatch)` twin) is asserted
   "trivial next step" and left UNBUILT/UNRUN. **Elegant-reduction trap**: the load-bearing step reads
   "author the rewrite (trivial) — and then the payoff is proven." A rewrite that must *detect
   disjoint FIRST sets* and *prove the two forms semantically equal* is not self-evidently trivial;
   it is precisely the analysis 2A-Island-A/C would have to carry, and it is the unconverged part.

2. **The divergence-forcing weight vector is a synthetic sign-flip, not a cost any calibrated model
   emits.** W2 sets `dispatch_bonus = +2.0` — the sign-flip of the calibrated `−2.0`. Part A therefore
   proves the extractor is *weight-sensitive*, NOT that any real cost lattice presents a genuine
   Pareto front where sequential is ever the right pick. **This is a gate that cannot fail in the
   direction it needs**: given any two priced forms, a hand-chosen sign-flip *always* flips the pick,
   independent of whether the fleet faces a real decision. Worse, PROTO-1's *own Part B* shows the
   dispatch table is cheaper by **2.28×–3.34× on every corpus** — i.e. the table dominates
   unconditionally. When one form dominates everywhere, the correct architecture is
   **canonicalization** (always rewrite Alt→table), and **extraction is inert** — exactly the aegraph
   verdict (avg class 1.13, choice layer used 2-of-4M nodes) the lock cites as its external twin.
   PROTO-1 thus corroborates the aegraph warning while being reported as its refutation.

3. **Part B's cyc/B contrast is measured on a non-representative workload.** The bench classifies
   every corpus byte (twitter: 514 197 / 631 514 = 81% NOMATCH). The sequential form pays up to 7
   first-byte compares on every NOMATCH byte; a real parser never runs the `value` Alt on
   interior/whitespace bytes — it fires only at value-start positions after ws-skip. The 2–3× ratio is
   therefore inflated by the synthetic byte mix. The report's "direction and CI-exclusion are not
   sensitive to this" is **asserted, not measured** (unverified gestalt) — and it is the load-bearing
   quantity: restricted to value-starts the table's single-load edge shrinks and, for small
   alternations, could cross 1.00×. The ratio is certified (paired, PMU, CI-tight) but **for the wrong
   contrast**.

**Net.** PROTO-1 earns exactly one thing: the machinery is *not structurally broken* — extraction and
the cost model respond to weights, and two lowerings of an Alt differ in silicon. That refutes "the
egraph is a dead costume." It does **not** earn the stronger claim §0 needs to de-risk 2B-extraction
and 2D-lattice "simultaneously": that a *real calibrated cost model*, fed a *real fired rewrite*,
faces a *live multi-candidate extraction decision* on a workload the fleet actually runs. On the
canonicalization-vs-extraction axis, the honest reading of PROTO-1 is that the fleet's Alt decision is
a canonicalization (pick the always-cheaper table), which needs no Extractor/CSP. **§0's "PASS
de-risks 2A/2B/2D" is overclaimed; the evidence is consistent with the collapse form (canon + the
existing BackendShape driver, extraction/CSP SPEC-ONLY) that §0 assigns to FAIL.**

## THE SECOND GATE — PROTO-2's PASS is consumer-less and synthetic-cost (clauses vii + viii)

PROTO-2 returns PASS for "the generated-size budget binds over ≥2 competing scan sites." Two defects
that the report itself surfaces but the verdict does not discount:

4. **The ≥2 competing sites do not exist in the IR — only in grammar source text.** Verified: the IR
   models exactly ONE `SimdScan{ Exact, PreEntry }` per grammar; `SimdSite` has no `SelfLoop`,
   `SimdMode` has no `Prefilter`, and neither symbol is referenced anywhere in the tree. PROTO-2's
   Step-0 audit ("CSS-L4 ≥10, BBNF ≥3") counted `[^D]*` **regex patterns in `.bbnf` source**, not
   modeled decision sites. Its own honest asterisk: "the sites above are *latent in grammar source but
   unmodeled by the IR* … the per-loop `SelfLoop` recognizer-derivation that would expose them is
   exactly the GN-2.W1 consumer … unbuilt." The CSP was built over **hypothetical** sites.

5. **The costs are invented and no consumer reads the plan.** Verbatim: "the cyc/B are synthetic cost
   *inputs*, not measurements" (`Prefilter=40 LOC/fast, Exact=8 LOC/slow`). Whether a tight budget
   "forces Exact on 3 sites" is a pure function of author-chosen constants; different constants → the
   binding vanishes. And: "full `cargo xtask regen` is therefore not exercised (**no consumer reads
   the plan yet**)." The parity-floor and determinism gates are checked at the CSP decision layer — a
   layer with no downstream. **This is textbook consumer-less substrate (viii / Y.13)** — the exact
   violation the brief was meant to guard against — and a **SPEC_ONLY claim (synthetic cost) anchoring
   a "teeth exist on the corpus" verdict (clause vii)**. The `bounded_sum` propagator is real and
   tested (7 cases), but its consumer is the unbuilt GN-2.W1 recognizer-derivation subsystem, which is
   LOCK-2-external (GN-2 / LOCK-5 territory).

**Net.** PROTO-2 earns: the knapsack propagator works, and *if* the fleet had ≥2 IR-modeled sites with
binding real costs, a global CSP would strictly dominate greedy. Both antecedents are unbuilt/
synthetic. The honest verdict is **SPEC-ONLY-pending-the-SelfLoop/Prefilter recognizer-derivation**,
not "2C's teeth exist on the corpus."

---

## Family 2A — EGRAPH-ISLANDS — convergence 55%

Strongest-aligned family; architecture (LLVM directed spine + local islands at named chokepoints) is
coherent, evidence-aligned, and its gate (i) — "produce child-bearing nodes OR be genuinely
constrained, else write it as a plain cost-ranked choice, do NOT island it" — is a **gate that can
genuinely go RED**. Credit. But every one of its three candidate islands is, in this pass, either
cross-lock-owned, built on unmodeled IR, or pushed toward collapse by PROTO-1's own data.

**Enumerated open gaps:**
1. **Island A (regex-HIR canon) is untested AND cross-lock-owned.** PROTO-1 tested a JSON *Alt*
   dispatch, not a regex-leaf prefilter/DFA canonicalization; the spec's own open-Q1 ("does the fleet
   have a real regex leaf whose prefilter/DFA verdict changes under canon?") is unanswered. The spec
   defers the lawful HIR form to LOCK-5 — legitimate, but it means 2A's only child-bearing island
   depends on a verdict not yet rendered by another lock.
2. **Island B (scan-plan) has no IR representation and no live consumer today.** Its justification is
   PROTO-2, which is consumer-less (sites unmodeled, costs synthetic). The "live consumer
   (`find_member`)" is aspirational until `SimdSite::SelfLoop`/`SimdMode::Prefilter` and the per-loop
   recognizer-derivation are built (GN-2.W1). Not BLOCKED — the path is clear — but "same-wave
   consumer that changes emitted code" is not yet present.
3. **Island C (projection canon) is untested and likely a canonicalization, not an island.** PROTO-1's
   table-always-wins result implies `map∘map` and dead-projection elimination are always-apply
   rewrites (one canonical form), i.e. by gate (i) they should be written as plain rewrites, NOT
   islanded. No proto exercised Island C's cost source (`derive_materialization_plan`).
4. **The island count is unresolved and the honest floor may be zero fully-instantiated islands.**
   Open-Q3 ("three, two, or one?") is open; given #1 (cross-lock), #2 (unmodeled), #3
   (canonicalization), the present count of islands standing on in-tree child-bearing extraction with
   a live consumer is **0**. The family survives as a *design* whose first instantiation is entirely
   downstream work.
5. **Ceremonial-deletion gate unexercised.** Gate (v) (delete the ~68-field provenance apparatus,
   relocate inline `#[cfg(test)]`) is stated, not demonstrated — a masked-fallback watch item until
   the deletion actually lands (the provenance strings are the clause-vii "vague optimism as code").

## Family 2B — IR-IS-EGRAPH — convergence 38%

The maximal reading (unbounded saturation) is refuted on contact by the IR's own shape (AC blowup,
cyclic rules, non-local budget) — the spec concedes this and retreats to the aegraph discipline. The
`Ref{target}` = LEAF / per-rule root-table insight (acyclic e-graph, terminating greedy extraction)
is genuinely sound and well-reasoned — credit. But its distinctive thesis is unmet.

**Enumerated open gaps:**
1. **The master gate did not flip the way 2B needs.** By the spec's own open-Q3, if §0 does not show
   extraction buys something, 2B reduces to "GVN/hash-consing for free," which 2A/2C inherit from the
   shared engine. Per the governing finding above, PROTO-1's data is consistent with
   canonicalization-suffices (table dominates), so 2B's distinctive value ("the IR *becomes* the
   e-graph; extraction *is* the lowering") remains gated on an **unmet** condition, and the aegraph
   twin predicts the residual is ~0.1%.
2. **Compile-time budget UNTESTED.** No proto built the full-IR e-graph over CSS-L4 (2875 nodes) or
   measured build+saturate+extract+**teardown** against ~230µs. The `CspScheduler` rebuilds a fresh
   `Csp` every iteration (`csp_scheduler.rs:229`) — the exact opposite of aegraph's no-rebuild lesson;
   this scaling risk is named in prior-art trap #2 and remains a pure assertion.
3. **regen bit-identity through cost-guided extraction UNTESTED.** Gate (3) (total-order tie-break;
   `HashMap` iteration inadmissible) is a stated obligation with zero evidence.
4. **The extraction-crossover exhibit (gate 5) does not exist.** "One grammar where per-node greedy
   busts the budget and the CSP knapsack recovers the legal optimum" was neither constructed nor run
   in-IR; PROTO-2 built the analogue over synthetic scan sites, not over the IR e-graph 2B posits.
5. **AC confluence is a bare prototype obligation.** One-directional normal-forming of `Seq`/`Alt`
   (flatten/sort/dedup) with no exploratory re-associate is asserted confluent; untested.

## Family 2C — CSP-CENTRAL — convergence 32%

BLOCKED-candidate whose sole teeth (a binding cross-site budget) rest entirely on PROTO-2 — which is
consumer-less and synthetic-cost (defects 4–5 above). The Unison prior art additionally refutes the
family's maximal reading by construction.

**Enumerated open gaps:**
1. **The teeth exist only on a hypothetical corpus.** The ≥2 competing sites are unmodeled in the IR
   (single `SimdScan`); the binding demonstration uses invented LOC/cyc/B constants; no consumer reads
   the plan. Until `SelfLoop`/`Prefilter` are IR-real and costs are 2D-fitted, "the budget binds on
   the fleet" (open-Q1) is **unproven**, and the counterexample (JSON/Sheets = 1 site each) is only
   partially answered (CSS/BBNF source *patterns* ≥2, but IR sites = 1).
2. **The maximal reading is refuted by its own prior art.** CSS-L4's 2875 DTA states exceed Unison's
   proven-optimal ~946–1000 instr/function ceiling; a monolithic global COP is off the cliff by
   construction. The spec concedes this and retreats to "bounded post-solve over a small coupled
   sub-CSP" — but that surviving form is **operationally indistinguishable from 2A's Island B**,
   which the incompatibility map (§III) admits. 2C-as-primacy does not survive; only 2C-as-Island-B
   does, which is not a distinct family.
3. **Degenerate without 2D (named dependency).** Open-Q3: without the PMU cost table the objective
   collapses to `static_size_cost` — "a CSP-shaped priority cascade with extra steps." 2C cannot be
   non-degenerate on its own.
4. **The P1–P8 PriorityStep retirement is an unproven masked-fallback risk.** "RETIRED into
   legality/soft-preference constraints" preserves rather than subsumes the cascade unless it is shown
   the constraint form produces identical selections; no such equivalence check exists.
5. **regen-through-consumer determinism unproven** (same consumer-less hole as PROTO-2 gate).

## Family 2D — PROFILE-CLOSED-LOOP — convergence 40%

The loop *discipline* is sound and correctly precedented (AutoFDO/BOLT/Ansor), and the load-bearing
determinism invariant — the loop closes at **calibrate-time, not regen-time**, so
`regen∘regen=regen` survives and all load noise lives in `xtask calibrate` — is correct and
non-negotiably right. Credit. But 2D is a *dependent* layer, and every empirical claim it makes is
untested in this pass.

**Enumerated open gaps:**
1. **No fit was ever run.** Open-Q2 ("does any knob's fitted optimum differ from its current value
   with a measured win?") is unanswered. PROTO-1's sign-flip is a *manual weight swap*, NOT a fit from
   measured cyc/B; it establishes weight-sensitivity, not that OLS/ridge over a structural feature
   vector recovers a coefficient that flips a real decision. Gate (1) (a fitted value flips a decision
   AND the fitted lane beats the hardcoded lane cold-paired) has zero evidence.
2. **Attribution underdetermination is named and deferred, not solved.** The bench emits
   whole-grammar cyc/B; fitting per-decision β from whole-parse cyc/B over ~15 equations with dozens
   of features is unidentifiable. The spec offers two primitives (A/B differential vs sub-parse
   probes) and **chooses neither** — "the prototype must choose." **Elegant-reduction trap**: the hard
   part (making per-decision attribution identifiable) is the unconverged core, punted to a future
   prototype.
3. **The "demonstrated spread" knobs are cited, not re-established.** `CSS_SHUFTI_MIN_SPAN`,
   `GATE=16`, `N*≈3 build_lo6_table`, `CapacityPolicy` — the spread is asserted from prior GN work,
   not shown in this pass. Whether *these specific* knobs move the *LOCK-2 decision* (as opposed to a
   GN-local threshold) is unverified.
4. **The backend-shape-lattice plane reproduces the null (self-conceded BLOCKED).** 2D at the shape
   lattice is refuted by §0; it is only unblocked at threshold/kernel/capacity knobs — i.e. 2D's
   applicability is bounded to exactly the plane where 2A/2C do NOT need it, so its composition value
   is narrower than "the coefficient plane under any generator" advertises.
5. **The freshness-gate teeth (gate 4) are unbuilt.** "Mutate one coefficient → `report.rs:1750` goes
   RED, `check-calibration` fails" is the provenance-theater guard 2D promises to install; the current
   `"pass"`/`0` stamps are the lie. No prototype demonstrated the gate can actually go RED — so the
   anti-provenance-theater claim is itself, presently, provenance theater.

---

## Cross-cutting adversarial notes (constitution + method)

- **Spec/evidence ordering.** FAMILY-SPECS (04:46) pre-dates both protos (04:57/05:01). The verdicts
  ("ADVANCED-candidate", "BLOCKED-candidate pending Brief 2") are pre-evidence; when updated with the
  proto results *as actually run* (hand-union + synthetic-weight; consumer-less + synthetic-cost),
  none of the four families crosses the "prototype-answerable, now answered YES" threshold the spec
  language implies. The specs should be re-cut to record PROTO-1 as *machinery-not-broken* (not
  *extraction-decision-real*) and PROTO-2 as *SPEC-ONLY-pending-consumer*.
- **No circularity.** §0 rests on two *independent* measurements (in-tree R-1 null + external aegraph
  1.13/2-of-4M) — not spec-cites-itself. This part is sound and is the pass's genuine strength.
- **Clause vii (concrete-or-SPEC-ONLY):** PROTO-2's synthetic-cost budget-binding must carry the
  SPEC-ONLY label; it currently reads PASS. PROTO-1's cyc/B is real PMU but on a synthetic (every-byte)
  workload — the ratio is certified for a contrast the parser does not run.
- **Clause viii (consumer-less substrate):** PROTO-2's `bounded_sum` + scan-plan CSP has no reader;
  2A-Island-B and 2C both inherit this hole until GN-2.W1's recognizer-derivation exists.
- **Refutation laws (iv):** honored in form — ratios not absolutes, no warm benches, aarch64-only. But
  PROTO-1 ran at loadavg ~9.3–9.7; the paired design mitigates drift, yet the every-byte workload is a
  separate, unmitigated representativeness defect distinct from load noise.
- **Parsimony (vi):** `bounded_sum` correctly homed in csp-solver (general-infra edict) — clean.

## Lock-level disposition (critic-1 recommendation)

The honest LOCK-2 after pass-1 is **2A's directed spine + aggressive canonicalization + the existing
BackendShape driver**, with the *extraction/CSP-extraction* machinery recorded **SPEC-ONLY pending a
FIRED rewrite that faces a real (non-sign-flipped) cost decision on a representative workload**, and
the *scan-plan CSP/budget* machinery recorded **SPEC-ONLY pending the `SelfLoop`/`Prefilter` IR
extension + a live codegen consumer (GN-2.W1)**. 2B and 2C do not survive as *primacy* families:
2B's distinctive extraction thesis is unmet and its maximal form IR-refuted; 2C's maximal form is
Unison-refuted and its surviving form is 2A-Island-B under another name. 2D is a sound *discipline*
whose every empirical claim is untested and whose applicable plane is exactly where the generators do
not need it. Pass-2 must fire the real rewrite, build the IR sites + consumer, and run one actual fit
before any family may speak of convergence.
