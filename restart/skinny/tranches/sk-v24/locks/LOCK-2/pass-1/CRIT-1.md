# LOCK-2 — THE GRAPH ENGINE — pass-1 · CRIT-1 (Adversarial critic 1 of 2, WITHIN-LOCK weight)

*Charter: harden/challenge/refine every family against the failure-mode checklist — vacuous
convergence, spec-cites-itself circularity, gates-that-cannot-fail, the elegant-reduction trap,
legacy aliases, masked fallbacks, unverified gestalt, consumer-less substrate — plus constitution
violations and prototype-evidence misuse (SPEC_ONLY anchoring a perf claim). Critic 1 weights the
WITHIN-LOCK view: internal soundness of each family spec and whether the two prototypes actually
tested the sharpest uncertainty. This supersedes the 05:07 CRIT-1 draft (written before PROTO-1
09:53 / PROTO-2-wf30 09:58 and the 09:45 FAMILY-SPECS re-cut) and folds a decisive on-disk
reconciliation neither prior critic performed.*

Read this pass: PORTFOLIO (LOCK-2 + CONSTITUTION), FAMILY-SPECS (09:45), PROTO-1, PROTO-2 (wf-28)
and PROTO-2-wf30, both patches, partA/partB raw, research-{A,B,C,D,priorart}, the prior CRIT-1/CRIT-2,
and — the load-bearing step — the actual code on disk in **both** the `skinny/crates/` tree and the
top-level `crates/` tree.

---

## 0. THE GOVERNING WITHIN-LOCK FINDING — the two prototypes and the §0 census describe THREE DIFFERENT TREES

This is the pass's single most load-bearing defect, and it invalidates the way both prior critics
read the evidence. I verified it on disk this pass; it is decision-grade.

**There are two grammar-IR codebases, and the lock's artifacts are silently spread across both.**

1. **The skinny tree (`skinny/crates/`) — LOCK-2's actual redesign target.**
   `skinny/crates/ir/src/` has **NO `egraph/` module at all** — no `GrammarENode`, no
   `build_and_saturate`, no `write_back`, no `GrammarCostModel`. Its *only* e-graph use is
   `skinny/crates/passes/src/backend_egraph.rs`, which imports `EGraph/Extractor/NoAnalysis` from the
   shared `egraph` engine crate and builds the **childless-bag** union + `best_node` (line 84–86), plus
   the rigged 1-var `decision_csp.rs` and `perf_cost: 0` (`lib.rs:590`, and ×4 more at 2023/2035/2062/2083).
   **§0's "inert decision substrate" census is ACCURATE — but only for this tree.** Here the grammar IR
   genuinely is *not* a `Language`; the FAMILY-SPECS 2B claim "making it true is 2B's work" is correct
   *for skinny*.

2. **The top-level legacy tree (`crates/`) — where `bbnf-ir` actually lives.**
   `grep -l 'name = "bbnf-ir"'` matches **only `crates/ir/Cargo.toml`.** This tree has the full
   `crates/ir/src/egraph/` module (`node.rs` — `#[derive(…, Language)] pub enum GrammarENode`;
   `build_egraph.rs`; `write_back.rs` 14.5 KB; `interner.rs`; `rules/regex.rs`; `analysis/`; `cost.rs`),
   **and it is WIRED INTO PRODUCTION COMPILE** at
   `crates/core/src/pipeline/compile/pipeline.rs:218–222`:
   `timer.span("egraph_build_saturate_writeback", || { let (egraph, pool, rule_body_ids) =
   bbnf_ir::egraph::build_and_saturate(&ir); bbnf_ir::egraph::write_back_optimized(…); … })`.
   It is exercised by production tests (`crates/ir/tests/egraph/egraph_analysis.rs:106` —
   "grammar_analysis_activates_in_production_build_and_saturate").

3. **PROTO-1 ran against the LEGACY tree, not skinny.** PROTO-1's commands are
   `cargo build … -p bbnf-ir --example prove2_branch` and it imports `crates/ir/src/egraph/cost.rs`,
   `crates/ir/src/types/node.rs` (`AltDispatch`), and builds `EGraph<GrammarENode, NoAnalysis>`.
   `bbnf-ir` = `crates/ir` = legacy. `GrammarENode`/`GrammarCostModel` exist **nowhere in skinny.**

**The three-way consequence, which corrects BOTH prior critics:**

- **CRIT-2's headline is doubly wrong.** CRIT-2 §0 declared "the entire unused grammar e-graph …
  fully built and never called on the production path … THE constitution-viii / Y.13 dead substrate."
  On disk the legacy `crates/ir/src/egraph/` is **not** unconsumed — it is called at
  `crates/core/pipeline.rs:218`. CRIT-2 conflated the two trees and inverted the fact. There is no
  "order-of-magnitude-larger uncatalogued dead substrate"; there is a *wired legacy optimizer in a
  different tree* and a *genuinely-absent optimizer in skinny*.

- **The synthesis §0 and 2B are right about skinny but the prototype that "anchors" them is not
  skinny's.** PROTO-1 proves that a cost-guided `Extractor` picks divergently between two `Alt`
  lowerings **on the legacy `GrammarCostModel`/`GrammarENode` engine** — machinery that must be
  *ported into skinny before any of it is true there*. The porting (legacy `crates/ir/egraph` →
  skinny) is the actual unbuilt core, and **no family scopes it**: 2B thinks it must *build* the
  Language (it already exists in legacy), CRIT-2 thinks it must *delete* dead code (it's live in
  legacy), and the synthesis presents PROTO-1 as the lock's own production machinery (it's another
  tree's). **This is a cross-tree provenance error: the sharpest uncertainty was tested on the wrong
  codebase relative to the redesign target.**

- **The honest reframing 2B is owed:** 2B's real deliverable in skinny is neither "make the IR a
  Language" nor "delete the dead e-graph" — it is *port/adopt the legacy `crates/ir/egraph` engine
  into skinny's pipeline in place of the childless-bag `backend_egraph`, and prove adoption stays in
  budget, is regen-byte-identical, and changes ≥1 output for the measurable better.* All three are
  UNTESTED, and the port itself is unscoped. The "80% substrate-built" intuition is true only if
  skinny is allowed to vendor legacy `crates/ir/egraph` wholesale — a cross-tree dependency the lock
  never states and the constitution's parsimony/colocation edict (vi) must adjudicate.

No spec-cites-itself circularity in the §0 logic (it rests on the R-1 in-tree null + the external
aegraph 1.13/2-of-4M twin — two independent measurements). That discipline is real and I do not
fault it. The fault is the **tree census**, which is load-bearing for every family's convergence
distance.

---

## 1. THE TWO PROTOTYPES, RE-AUDITED (within-lock: did they test the sharpest uncertainty?)

### PROTO-1 (§0 master gate) — RAN, but the rewrite is unfired, the weight vector is a gate that cannot fail, and the workload is unrepresentative

Three within-lock defects survive PROTO-1's PASS, and together they land PROTO-1's *own data* on the
canonicalization-suffices (FAIL-branch architectural) side, not the extraction side it claims.

1. **The rewrite was never fired — the alternative was hand-`union`ed (elegant-reduction trap).**
   PROTO-1's own candor (limits §2): "Part A builds the e-class by direct `union` rather than firing
   a registered `Rewrite` … the *construction* is hand-authored, the *extraction* is production." The
   gate's literal subject — *a rewrite manufactures the alternative* — is not exercised. R-1's null
   was about the *rewrite set* being all-narrowing (`DeduplicateAltBranches`, `SupersetAbsorbAlt`,
   `UnionMergeAlt`, `FuseAltRegexBranches`, `CommonSuffixFactor`); a `ManufactureDispatchAlt` widening
   rewrite must be shown (a) to *detect disjoint FIRST sets* and *prove the two Alt forms semantically
   equal*, (b) **confluent with the narrowing set** (does `DeduplicateAltBranches` collapse the twin
   right back?), and (c) **terminating under `CspScheduler`**. "Trivial next step" is asserted; it is
   precisely the analysis Islands A/C must carry, and it is the unconverged core.

2. **W2 is a synthetic sign-flip — a gate that cannot fail in the direction it needs.** W2 sets
   `dispatch_bonus = +2.0`, the sign-flip of the calibrated `−2.0`. Given *any* two priced forms, a
   hand-chosen sign-flip *always* flips `best_node`; Part A therefore proves the extractor is
   weight-sensitive, NOT that any *calibrated* cost model presents a real Pareto front where sequential
   is ever the right pick. Worse: PROTO-1's *own Part B* shows the dispatch table dominates by
   **2.27×–3.34× on every corpus**. When one form dominates everywhere, the correct architecture is
   **canonicalization** (always rewrite Alt→table) and **extraction is inert** — exactly the aegraph
   verdict (avg class 1.13, choice layer used 2-of-4M) the lock cites as its own external twin.
   **PROTO-1 corroborates the aegraph warning while being reported as its refutation.**

3. **Part B's cyc/B is measured on a non-representative byte stream (unverified gestalt).** The bench
   classifies *every corpus byte*; the branch histograms confirm twitter = 514,198 NOMATCH / 631,515
   = **81% of measured bytes are not value-starts**. A real parser fires the `value` Alt only at
   value-start positions after ws-skip; the sequential form's 7-compare fall-through is charged on
   4-of-5 bytes it would never see. The report's "direction and CI-exclusion are not sensitive to
   this" is **asserted, never measured at real positions.** The ratio is CI-tight and PMU-real — but
   *for the wrong contrast*; the magnitude must not anchor any downstream cost claim (yet §0/2D lean
   on it as "a real front to reason about").

**Net (PROTO-1).** Earns: skinny's *shared engine crate* is not structurally broken — a cost model
and Extractor respond to weights (on the legacy `GrammarENode`, per §0). Does **not** earn: that a
*fired* rewrite faces a *calibrated* multi-candidate extraction decision on a *representative*
workload *in skinny*. Read honestly, PROTO-1's data is consistent with the collapse form (canon + the
existing BackendShape driver, extraction/CSP SPEC-ONLY) that §0 assigns to FAIL — with the added
provenance caveat that even that reading is on the wrong tree.

### PROTO-2 (budget gate) — bounded_sum is real; the teeth are consumer-less and synthetic-cost

PROTO-2-wf30 is the more honest of the two artifacts and it explicitly downgrades its own verdict to
**"Conditional — 2A Island B mechanism PROVEN; 2C teeth LATENT (source-real, IR-unmodeled,
un-budgeted)."** I concur with wf30 over wf28's flatter "PASS," and I add:

4. **The ≥2 competing sites do not exist in the IR — only in grammar source text.** wf30 Scope A,
   verified: `skinny` models exactly **1** grammar (`json`) under the budget, with **1** `SimdScan{
   Exact, PreEntry }`; `SimdSite` has only `PreEntry`, `SimdMode` only `Exact`; **0** `SelfLoop`. The
   "CSS-L4 ≥10, BBNF ≥3" audit counted `[^D]*` **regex patterns in `.bbnf` source**, not IR-modeled
   decision sites — and those grammars are **not even under the skinny `generated_loc_budget`** (only
   `json` is registered). The CSP ran over **latent, unmodeled, un-budgeted** sites. Exposing them is
   the unbuilt GN-2.W1 SelfLoop derivation.

5. **The costs are invented and no consumer reads the plan (consumer-less substrate, viii/Y.13).**
   Verbatim: "the cyc/B are synthetic cost *inputs*, not measurements"; `Prefilter=40/300 LOC,
   Exact=8/100 LOC`. Whether a tight budget "forces `Exact` on N sites" is a pure function of
   author-chosen constants (wf30 itself: "If real Prefilter lowering is ~12 LOC, the budget may never
   bind"). And end-to-end regen is *not* exercised through a plan-reader: wf28 says "no consumer reads
   the plan yet"; wf30 shows `regen∘regen` byte-identity only because the additive propagator
   *perturbs nothing that is wired* (`check-json` exit 0 proves the plan changes *no* output — the
   plan has no reader). **The parity floor and determinism gates are checked at a decision layer with
   no downstream.** This is textbook consumer-less substrate — the exact pathology the brief exists to
   guard against.

**Net (PROTO-2).** Earns, cleanly: the `bounded_sum` knapsack propagator is real, tested (6–7 cases,
111 no-regression), correctly homed in `csp-solver` (general-infra edict, parsimony vi — clean), and
*mechanically* forces `Exact` under a tight budget where a 1-var CSP cannot. Does **not** earn: that
the budget binds on *real* costs over *IR-modeled* sites read by a *real consumer*. Honest label:
**2A-Island-B mechanism proven; 2C teeth SPEC-ONLY-pending-the-SelfLoop/Prefilter derivation +
2D-fitted costs + a codegen consumer (GN-2.W1).** wf30's own verdict line already says this; the
FAMILY-SPECS should adopt it verbatim and drop wf28's "2C's teeth exist on the corpus."

---

## Family 2A — EGRAPH-ISLANDS — convergence 50%

Strongest-aligned family; the LLVM directed-spine + local-islands architecture is coherent and
evidence-aligned, and gate (i) — "produce child-bearing nodes OR be genuinely constrained, else write
it as a plain cost-ranked choice, do NOT island it" — is a **gate that can genuinely go RED**. That
gate is the family's real strength and I credit it. But every candidate island is presently
cross-lock-owned, built on unmodeled skinny IR, pushed toward canonicalization-collapse by PROTO-1's
own data, or anchored by a prototype that ran on the wrong tree.

**Enumerated open gaps:**
1. **Island A (regex-HIR canon) is untested AND cross-lock-owned.** PROTO-1 tested a JSON *Alt*
   dispatch, not a regex-leaf prefilter/DFA canonicalization; open-Q(1) ("does the fleet have a real
   regex leaf whose prefilter/DFA verdict changes under canon?") is asked by the family and left
   unanswered by its own probe. Island A's lawful HIR form is deferred to LOCK-5 (clause ii), and the
   regex rewrite set (`crates/ir/src/egraph/rules/regex.rs`, legacy) is contested between LOCK-2-2A-A
   and LOCK-5-5A/5B with no cession. 2A's only child-bearing island depends on a verdict another lock
   has not rendered.
2. **Island B (scan-plan) has no skinny-IR representation and no live consumer.** Verified: skinny IR
   models one `SimdScan{Exact,PreEntry}`; `SimdSite::SelfLoop`/`SimdMode::Prefilter` are absent. Its
   justification is PROTO-2, which is consumer-less and synthetic-cost. The "live consumer
   (`find_member`)" is aspirational until the per-loop recognizer-derivation lands (GN-2.W1). Not
   BLOCKED — the mechanism (bounded_sum) is proven — but "same-wave consumer that changes emitted
   code" is not present this wave.
3. **Island C (projection canon) is untested and is probably a canonicalization, not an island.**
   PROTO-1's table-always-wins result implies `map∘map⇄map` and dead-projection elimination are
   always-apply rewrites (one canonical form) — by gate (i) they should be written as plain rewrites,
   NOT islanded. No proto exercised `derive_materialization_plan`. Island C also double-homes with
   LOCK-1's `Project` store (whose e-graph family 1D is BANKED-vacuous) — a name-gate unless LOCK-1
   actually ships an e-graph, which its own disposition says it will not.
4. **The island count is unresolved and the honest floor is ZERO fully-instantiated islands.**
   Open-Q(3) ("three, two, or one?") is open; given #1 (cross-lock, untested), #2 (unmodeled,
   consumer-less), #3 (canonicalization), the count of islands standing on *skinny*-in-tree
   child-bearing extraction with a live consumer is **0**. The family survives as a *design* whose
   first instantiation is entirely downstream/cross-tree work.
5. **GN-2.W1 supersession undeclared (clause ix).** "Island B subsumes and generalizes GN-2.W1's
   hand-written detection arm" is not a disposition. Per clause ix, is Island B a replacement for
   GN-2.W1, a wrapper landing after it, or gated behind it? (wf30 says "gated *behind* it.") LOCK-9
   line 176 already places its four-tier lowering on Island B's plan predicate, so Island B is on
   LOCK-9's critical path — the supersession must be named, not "subsumes/generalizes."
6. **Ceremonial-deletion gate (v) unexercised.** Deleting the ~68-field provenance apparatus + relocating
   inline `#[cfg(test)]` (no-inline-tests law) is stated, not demonstrated — a masked-fallback watch
   item until the deletion lands.

## Family 2B — IR-IS-EGRAPH — convergence 34%

The maximal reading (unbounded saturation) is refuted on contact by the IR's own shape (variadic
`Seq`/`Alt` AC-blowup, mutually-recursive rules, non-local budget); the spec concedes this and retreats
to the aegraph discipline. The `Ref{target}`-as-LEAF / per-rule root-table insight (acyclic body
e-graph, terminating greedy extraction) is genuinely sound — credit. But its distinctive thesis is
unmet and its scope is mis-measured by §0's tree census.

**Enumerated open gaps:**
1. **Mis-scoped by the tree census (the §0 finding).** In *skinny* the grammar IR is not a Language,
   so 2B looks like net-new construction; in *legacy* `crates/ir` the Language + `Ref`-as-leaf +
   per-rule root table + write-back cycle guard **already exist and are wired**. 2B's real skinny
   deliverable is **port/adopt legacy `crates/ir/egraph` (or re-derive it), then prove budget +
   regen-identity + a measurable output change** — a cross-tree dependency the family never states.
   The convergence distance is neither "build it" (understated by the tree census in one direction)
   nor "delete it" (CRIT-2's inversion) but "adopt-and-prove," which is unprototyped.
2. **The signature claim — extraction emits `BackendShape` directly — rests on a context-freedom the
   IR does not have (research-B risk 1, uncovered nowhere else this pass).** `admits_sink_only`
   (`passes/src/lib.rs:954`) keys shape legality on the *caller's* target: a sub-rule referenced by a
   `SinkOnly` caller and an `OffsetTape` caller cannot extract to one shape for both. Greedy per-class
   extraction is context-free; `BackendShape` is caller-sensitive. The only identity-preserving repair
   — optimal context-sensitive DAG extraction — is **NP-hard and inapproximable within any constant
   ratio** (prior-art [6][7]): a missing primitive of *equal difficulty to the original problem*, the
   BLOCKED-candidate condition. Whether the fleet has any caller-conflict is prototype-answerable (a
   caller-map audit over `admits_sink_only`/`requires_eager_tape`) and was **not run** this pass.
3. **"Does extraction buy anything" is unmet.** By open-Q(3), if §0 does not show extraction buys
   something, 2B reduces to "GVN/hash-consing for free," which 2A/2C inherit from the shared engine.
   PROTO-1's data is consistent with canonicalization-suffices (§1), and the aegraph twin predicts the
   residual is ~0.1%. 2B's *distinctive* value beyond free GVN is unshown.
4. **Compile-time budget incl. teardown UNTESTED.** No proto stood the full-IR e-graph up over CSS-L4
   (2875 states) and timed build+saturate+extract+**teardown** against ~230µs. `CspScheduler` rebuilds
   a fresh `Csp` every iteration (`csp_scheduler.rs`) — the opposite of aegraph's no-rebuild lesson;
   this scaling risk is a pure assertion.
5. **regen bit-identity through cost-guided extraction UNTESTED.** Gate (3) (total-order tie-break;
   `FxHashMap`/`HashMap` iteration inadmissible) is a stated obligation with zero grammar-scale evidence.
6. **AC confluence + the extraction-crossover exhibit (gates 1, 5) do not exist in-IR.** One-directional
   `Seq`/`Alt` normal-forming is asserted confluent, untested; the "greedy busts the budget, knapsack
   recovers the optimum" exhibit was built by PROTO-2 over *synthetic scan sites*, not the IR e-graph
   2B posits.

## Family 2C — CSP-CENTRAL — convergence 42%

PROTO-2's `bounded_sum` propagator raises this above the synthesis's original "BLOCKED-candidate" —
the mechanism is real, tested, and correctly homed. But the family's *entire teeth* (a binding
cross-site budget on real costs, read by a real consumer) remain open, and its maximal reading is
Unison-refuted by construction.

**Enumerated open gaps:**
1. **Budget-binds-on-REAL-costs is unproven.** The binding was shown only under author-chosen
   synthetic constants (§1 defect 5). The one fact that justifies the whole family — that a *real*
   static-size cost vector makes the budget bind over ≥2 *IR-modeled* sites — is untested. wf30: "If
   real Prefilter lowering is ~12 LOC, the budget may never bind."
2. **The decision sites are unmodeled in skinny IR** (`SimdSite::SelfLoop`/`SimdMode::Prefilter`
   absent; one `SimdScan` per grammar); modeling them is GN-2.W1, unbuilt. The `.bbnf` grammars with
   ≥2 shufti loops are not under the skinny budget at all.
3. **Consumer-less as it stands (viii/Y.13).** "No consumer reads the plan yet." 2C's consumer is
   GN-2.W1's `lower_to_rust` reading the plan; until it lands, 2C is the exact pathology the lock
   condemns, one level up. End-to-end `regen∘regen` through a plan-reader is therefore untested (the
   `check-json` exit-0 proves only that the plan changes nothing wired).
4. **The maximal reading is refuted by its own prior art.** CSS-L4's 2875 DTA states exceed Unison's
   proven-optimal ~946–1000 instr/function ceiling; a monolithic global COP is off the cliff by
   construction. The spec concedes this and retreats to "bounded post-solve over a small coupled
   sub-CSP" — but that surviving form is **operationally indistinguishable from 2A's Island B** (the
   incompatibility map §III admits it). 2C-as-primacy does not survive; only 2C-as-Island-B does,
   which is not a distinct family.
5. **Degenerate without 2D (named self-dependency).** Without a fitted PMU cost table the objective
   collapses to `static_size_cost` — "a CSP-shaped priority cascade with extra steps." And 2D is
   SPEC-ONLY (below). A family whose non-degeneracy depends on an unrun family cannot itself converge.
6. **The P1–P8 `PriorityStep` retirement is an unproven masked-fallback risk.** "RETIRED into
   legality/soft-preference constraints" preserves rather than subsumes the cascade unless it is shown
   the constraint form produces *identical* selections; no such equivalence check exists.

## Family 2D — PROFILE-CLOSED-LOOP — convergence 30%

SPEC-ONLY: **no PROVE-2D prototype ran** (only Briefs 1 and 2 have PROTO files). The loop *discipline*
is sound and correctly precedented (AutoFDO/BOLT/Ansor), and the load-bearing determinism invariant —
the loop closes at **calibrate-time, not regen-time**, so `regen∘regen` survives and all load noise
lives in `xtask calibrate` — is correct and non-negotiably right. Credit for both. But under clause
vii a family carrying perf claims with zero running artifact and zero measured numbers is "status
report / vague optimism," and every empirical claim 2D makes is untested.

**Enumerated open gaps:**
1. **Zero executed prototype.** PROVE-2D-A ("a fitted coefficient flips a decision AND beats the
   hardcoded lane cold-paired") and PROVE-2D-B ("freshness goes RED on a mutated coefficient") are
   unrun. PROTO-1's sign-flip is a *manual weight swap*, NOT a fit from measured cyc/B; it establishes
   weight-sensitivity, not that OLS/ridge over a structural feature vector recovers a coefficient that
   flips a real decision. Gate (1) has zero evidence.
2. **Attribution underdetermination is named and deferred, not solved (elegant-reduction trap).** The
   bench emits *whole-grammar* cyc/B; fitting per-decision β over ~15 equations with dozens of features
   is unidentifiable. The spec offers two primitives (A/B differential vs sub-parse probes) and
   **chooses neither** — "the prototype must choose." The hard part (making per-decision attribution
   identifiable) is the unconverged core, punted to a future prototype.
3. **The "demonstrated spread" knobs are cited, not re-established.** `CSS_SHUFTI_MIN_SPAN`, `GATE=16`,
   `N*≈3 build_lo6_table`, `CapacityPolicy` — the spread is asserted from prior GN work, not shown this
   pass; whether *these specific* knobs move a *LOCK-2 decision* (vs a GN-local threshold) is unverified.
   Fitting `CSS_SHUFTI_MIN_SPAN`/`GATE=16` supersedes hand-tuned GN-2.W1/W3 literals — a named
   supersession (clause ix) the spec treats as free attachment.
4. **The backend-shape-lattice plane reproduces the null (self-conceded BLOCKED).** 2D over the shape
   lattice is refuted by §0 (no firing rewrite manufactures a branch in skinny); it is only unblocked at
   threshold/kernel/capacity knobs — i.e. 2D's applicable plane is exactly where 2A/2C do NOT need it,
   so its composition value is narrower than "the coefficient plane under any generator" advertises.
5. **`from_env` masked-fallback not fenced off the regen path.** Keeping `from_env` (`BBNF_COST_*`)
   alongside `from_fact_table` is two cost sources; if regen ever reads env when the table is absent,
   `regen∘regen` breaks. The spec does not fence env out of the regen path.
6. **The freshness-gate teeth (gate 4) are unbuilt.** "Mutate one coefficient → `report.rs:1750` goes
   RED, `check-calibration` fails" is the anti-provenance-theater guard 2D promises; the current
   `"pass"`/`0` stamps are the lie. No prototype demonstrated the gate can go RED — so the
   anti-provenance-theater claim is *itself, presently, provenance theater*.

---

## Cross-cutting adversarial notes (checklist + constitution)

- **Spec-cites-itself circularity:** NOT present in the family logic — §0 rests on two independent
  measurements (R-1 in-tree null + aegraph 1.13/2-of-4M external twin), and 2C/2D cite Unison and
  AutoFDO/BOLT externally. This discipline is the pass's genuine strength; I do not fault it.
- **Gates-that-cannot-fail:** PROTO-1's W2 sign-flip (§1 defect 2) is one; also, the born-RED GN-2.W1
  gates (≥8.0× skip, ≥0.95× dense) are good gates but gate an *unbuilt* consumer — a gate on an unbuilt
  consumer cannot fail this wave, so it must be marked GN-owned, not LOCK-2-owned, or the consumer
  landed in-wave.
- **Consumer-less substrate (viii/Y.13):** PROTO-2's plan has no reader (2C-gap 3). Correcting CRIT-2:
  the *legacy* `crates/ir/egraph` is NOT the uncatalogued dead substrate — it is wired at
  `crates/core/pipeline.rs:218`. The real Y.13 exposure is (a) PROTO-2's reader-less plan, and (b) the
  ~68 provenance fields §0 already fingers; both are correctly slated for deletion/consumer-wiring.
- **Concrete-or-SPEC-ONLY (vii):** 2D must carry the SPEC-ONLY label (no artifact ran). PROTO-2's
  synthetic-cost budget-binding must carry SPEC-ONLY (wf30 already does; wf28 does not). PROTO-1's cyc/B
  magnitude is real PMU but on an unrepresentative every-byte workload — certified for a contrast the
  parser does not run; it must not anchor 2D's "real front."
- **Prototype-evidence misuse:** PROTO-1 is presented as anchoring 2A-Islands-A/C, 2B-extraction, and
  2D-lattice "simultaneously," but (i) it ran on the *legacy* tree not skinny (§0), (ii) it exercised a
  grammar-`Alt` dispatch — Island-B-flavored — not Island A (regex canon) or Island C (projection
  canon), which received zero evidence, and (iii) the divergence came from a hand-`union` + sign-flip,
  not a fired rewrite + calibrated weights. The de-risking claim over-reaches on all three axes.
- **Extant-codex supersession (ix):** the GN-2.W1 subsumption and the SHUFTI/GATE fits are supersessions
  stated as "subsumes/attaches," not *named* supersessions.
- **Parsimony (vi):** `bounded_sum` correctly homed in `csp-solver` — clean. But whether skinny may
  vendor legacy `crates/ir/egraph` wholesale (2B's real path) is an unaddressed colocation/parsimony
  question the gestalt must adjudicate.
- **Refutation laws (iv):** honored in form (ratios not absolutes, no warm benches, aarch64-only).
  PROTO-1 ran at loadavg ~9.3–9.9; paired interleaving mitigates drift, but the every-byte workload is
  a separate, unmitigated representativeness defect distinct from load noise.

## Lock-level disposition (critic-1 recommendation)

The honest LOCK-2 after pass-1 is **2A's directed spine + aggressive canonicalization + the existing
BackendShape driver**, with two machinery classes recorded SPEC-ONLY:
(a) *extraction/CSP-extraction* — SPEC-ONLY pending a **fired** rewrite (confluent with the narrowing
set, terminating) that faces a **calibrated** (non-sign-flipped) cost decision on a **representative**
workload **in the skinny tree** (PROTO-1 ran on legacy `crates/ir`, not skinny);
(b) *scan-plan CSP/budget* — the `bounded_sum` propagator is proven, but binding-on-real-costs is
SPEC-ONLY pending the `SelfLoop`/`Prefilter` IR extension + real 2D-fitted costs + a live codegen
consumer (GN-2.W1).
2B and 2C do not survive as *primacy* families: 2B's distinctive extraction thesis is unmet, its
maximal form IR-refuted, its signature caller-free-extraction claim NP-hard-refuted (research-B risk 1),
and its scope mis-measured by the tree census; 2C's maximal form is Unison-refuted and its surviving
form is 2A-Island-B under another name. 2D is a sound *discipline* with zero running artifact whose
applicable plane is exactly where the generators do not need it.

**The single highest-value pass-2 directive (within-lock):** reconcile the two trees and scope the
*actual* deliverable — port/adopt (or re-derive) the legacy `crates/ir/egraph` engine into skinny,
then, in skinny, (1) fire a real `ManufactureDispatchAlt` rewrite and show it confluent + terminating,
(2) run the `admits_sink_only` caller-conflict audit (does the fleet even have a caller-conflict — if
not, 2B's NP-hard objection is moot and extraction is sound), (3) build the `SelfLoop`/`Prefilter` IR
sites + a codegen consumer so PROTO-2's plan is read, and (4) run one actual 2D fit. No family may
speak of convergence before those four land — and none of the four was tested this pass.
