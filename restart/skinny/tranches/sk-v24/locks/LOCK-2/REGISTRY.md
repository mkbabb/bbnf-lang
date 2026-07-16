# LOCK-2 — THE GRAPH ENGINE — REGISTRY

*e-graph + CSP optimization: HIR canon, IR rewriting, kernel/backend selection, capacity & schedule
decisions, profile-fed cost, deterministic regen. Families — 2A EGRAPH-ISLANDS · 2B IR-IS-EGRAPH ·
2C CSP-CENTRAL · 2D PROFILE-CLOSED-LOOP. This registry is the agglomerator's running ledger across
passes; the newest pass governs, prior passes retained below as history.*

Constitution binds: consumer-less substrate forbidden (viii/Y.13); type plane costs zero at runtime
(v); concrete artifacts or SPEC-ONLY (vii); refutation laws — no warm benches, ratios certified /
absolutes directional (iv); deterministic regen (`regen∘regen = regen` byte-identical); extant
GN/PT codex is the baseline (ix).

---

## Current disposition (as of pass 1)

| Family | Status | Conv. | One-line |
|---|---|--:|---|
| **2A EGRAPH-ISLANDS** | **ADVANCED** | **55%** | The defensible core: LLVM directed spine + local islands. Carried forward as the lock's spine, but its 3 candidate islands are cross-lock-owned (A), unmodeled+consumer-unbuilt (B), or a plain canonicalization miswritten as an island (C). |
| **2B IR-IS-EGRAPH** | **BLOCKED** | **40%** | Net-new in the skinny tree (census confirmed), but a *working, wired* exemplar exists in the legacy `crates/` tree — feasibility de-risked, distinctive extraction value triple-refuted as inert. Blocked on the 2A⊗2B middle-IR fork + untested skinny compile-budget/regen. |
| **2C CSP-CENTRAL** | **BANKED** | **35%** | As-primacy refuted (Unison ceiling; collapses to 2A Island B — both critics concur). The `bounded_sum` knapsack propagator is a solid, tested **banked asset** folded into Island B; real-cost binding + a live consumer remain unbuilt. |
| **2D PROFILE-CLOSED-LOOP** | **BLOCKED** | **33%** | Sound discipline (calibrate-time closure preserves the regen fixpoint), but SPEC-ONLY: zero PROVE-2D artifact ran. Blocked at the shape lattice (§0 null recurs); unblocked-in-principle at threshold knobs, unmeasured. |

**The lock returns as a converged CORE: 2A's directed spine + aggressive canonicalization + the
existing BackendShape driver, anchored by running code (the legacy grammar e-graph proves the
canonicalization architecture ships and is deterministic enough for a compile path), with the
extraction / CSP-extraction machinery recorded SPEC-ONLY pending a FIRED, confluent manufacturing
rewrite, and the scan-plan CSP/budget machinery recorded SPEC-ONLY pending the `SelfLoop`/`Prefilter`
IR extension + a live GN-2.W1 codegen consumer.**

---

## 0. The pass-1 governing correction — TWO TREES, and the census dispute is settled

The two critics disagreed on the substrate census, and the disagreement is **not a judgment call —
it is a tree-identity error**, resolved on disk this pass:

- **The skinny tree (`skinny/crates/`) is LOCK-2's subject** (sk-v24 is the skinny restart;
  FAMILY-SPECS cites `skinny/Cargo.toml:67`, `skinny/crates/passes/...`). On disk:
  `skinny/crates/ir/src/` is **only `cost.rs` + `lib.rs`** — no e-graph module, no `GrammarENode`,
  no `build_and_saturate`. The **sole `impl Language` is `DecisionNode`**
  (`skinny/crates/passes/src/backend_egraph.rs:181`). **research-A, research-B, research-priorart,
  CRIT-1, and the FAMILY-SPECS §0 census are all CORRECT for the skinny tree.** 2B *is* net-new work
  there; the backend_egraph *is* the degenerate childless bag; SimdMode/SimdSite *are* single-valued.

- **CRIT-2's §0 "the census is false on disk" audited the WRONG tree.** Its cites
  (`crates/ir/src/egraph/node.rs`, `write_back.rs`) are the **legacy pre-restart `crates/` tree**,
  which is a *different* codebase. There, a full grammar e-graph **does** exist and **is wired on the
  legacy production compile path**: `crates/core/src/pipeline/compile/pipeline.rs:219-221` calls
  `bbnf_ir::egraph::build_and_saturate` → `GrammarCostModel::from_config` → `write_back_optimized`
  (2643 LOC across `node.rs`/`write_back.rs`/`cost.rs`/`interner.rs`/`analysis/`/`rules/`, with
  `GrammarENode: Language`, `Ref`-as-leaf, per-rule root table, cycle guard). CRIT-2's convergence
  drops, dead-substrate catalogue demand, and triple-home cross-lock claims are **all premised on
  the wrong tree and do not bind the skinny locks as stated.**

**Both underlying facts stand; the reconciliation is now settled and WITHHELD from re-litigation:**
1. The skinny tree has no grammar e-graph — 2B is genuinely net-new there.
2. The legacy tree has a *working, wired, shipping* grammar e-graph — an in-project reference
   implementation of exactly the 2A/2B architecture. This is a **de-risking asset**, not a dead
   substrate to catalogue: it proves the aegraph-discipline grammar e-graph (recursion-as-leaf, cost
   -guided extraction, JSON `value↔object↔array` cycle handling) *works on a real compile path* in
   this project.

**The decisive, non-synthetic §0 answer the legacy tree hands us for free.** The legacy wired rule
set (`default_rules()`, `crates/ir/src/egraph/rules/mod.rs:87-96`) is **all-narrowing** —
`DeduplicateAltBranches`, `SupersetAbsorbAlt`, `UnionMergeAlt`, `FuseAltRegexBranches`,
`CommonSuffixFactor`, `AltOfSingle`, `RepeatOfSingle`, `WrapOfEpsilonScalar`, `ConcatLiterals`.
**No widening/manufacturing rule exists even in the project's own shipping grammar e-graph.** So the
§0 finding is now **triple-confirmed** (in-tree R-1 54-config null + external Cranelift aegraph
avg-class-1.13/2-of-4M + the project's own wired all-narrowing rule set): *canonicalization is the
value; extraction is inert absent a manufacturing rewrite that no one has built or shown confluent.*
This corroborates the critics' skeptical read and **downgrades PROTO-1's PASS headline** — PROTO-1
had to hand-`union` a second candidate precisely because no rule manufactures one, in either tree.

---

## 1. The two master-gate prototypes — how much they actually lifted

Both critics independently converge (and this pass verifies) that the two PASS verdicts are **softer
than the binary label**. The agglomerator adopts the critics' floor, with named reasons:

### PROTO-1 (PROVE-2-BRANCH, §0 master gate) — earns *machinery-not-broken*, NOT *extraction-decision-real*
- **The rewrite was never fired.** The 2-candidate e-class was built by direct `union`, not by a
  registered `Rewrite` through the scheduler. The load-bearing step — a *widening* rewrite that
  fires under `CspScheduler` **and is confluent with the all-narrowing rule set** and terminating —
  is declared "trivial next step" and left unbuilt. Both critics flag the elegant-reduction trap;
  the legacy all-narrowing exemplar (§0) proves it is *not* trivial (nothing in-project manufactures
  a branch).
- **W2 is a synthetic sign-flip** (`dispatch_bonus = +2.0`, the negation of the calibrated `−2.0`).
  This proves weight-*sensitivity*, not a real Pareto front. And PROTO-1's own Part B shows the
  dispatch table dominates **2.28×–3.34× on every corpus** — one form dominating everywhere is a
  **canonicalization** signal (always rewrite Alt→table), which needs no extractor.
- **Part B's cyc/B is measured on the wrong byte distribution** (twitter: 81% NOMATCH; a real parser
  never runs the value Alt on interior/ws bytes). The ratio is certified for a contrast the parser
  does not run; the "direction not sensitive" claim is asserted, not measured.
- **De-risking over-reach:** the artifact is a grammar-`Alt` dispatch decision (Island-B/backend-shape
  flavored). Islands A (regex-HIR canon) and C (projection canon) — the two islands whose entire
  justification is "child-bearing nodes → real extraction" — received **zero evidence**.

### PROTO-2 (PROVE-2-BUDGET, budget gate) — earns *the propagator works*, NOT *teeth exist on the corpus*
- **Real, tested net-new asset:** `bounded_sum` knapsack propagator (7 tests, 111 no-regression,
  +467 LOC additive, `Custom`-seam wired). This is genuine and BANKED.
- **The ≥2 competing sites are latent in grammar SOURCE, unmodeled by the IR.** `ir::SimdSite` has
  only `PreEntry`, `ir::SimdMode` only `Exact`; `derive_recognizers` collapses to ONE
  `SimdScan{Exact,PreEntry}` per grammar. The CSP ran over source-audited sites, not IR sites.
- **The costs are invented** (Prefilter=40 LOC, Exact=8 LOC, synthetic cyc/B). Whether the budget
  *actually* binds is a pure function of author-chosen constants; different constants → binding
  vanishes. If real Prefilter lowering is ~12 LOC, it may never bind.
- **No consumer reads the plan; `regen∘regen` is untested end-to-end.** Determinism shown at the CSP
  decision layer only. The consumer is GN-2.W1's unbuilt `SelfLoop`/`find_member` lowering. A plan
  nothing reads is the Y.13 pathology one level up (clause viii).

---

## 2. Per-family ledger

### 2A — EGRAPH-ISLANDS · ADVANCED · 55%
*Convergence: critics' floor (CRIT-1 55, CRIT-2 58); held at 55 — the legacy-tree canon exemplar
de-risks the canonicalization core but closes none of the enumerated island gaps.*

Strongest-aligned; the directed-shell architecture (LLVM spine + local islands at named chokepoints)
is coherent and evidence-aligned, and its gate (i) — "child-bearing nodes OR genuinely constrained,
else write it as a plain cost-ranked choice, do NOT island it" — genuinely can go RED. The legacy
tree's wired grammar e-graph is a running proof the *canonicalization spine* ships.

**Open gaps:**
1. **Island A (regex-HIR canon) — un-evidenced AND cross-lock-owned.** No proto touched a regex leaf;
   open-Q(1) ("a real fleet regex leaf whose prefilter/DFA verdict changes under canon?") unanswered.
   Ownership contested with LOCK-5 (constitutional owner of the lawful HIR form); the legacy
   `rules/regex.rs` (434 LOC) is the shared exemplar both locks claim.
2. **Island B (scan-plan) — no IR representation, consumer unbuilt.** Justified only by PROTO-2
   (consumer-less, synthetic-cost). `SimdSite::SelfLoop`/`SimdMode::Prefilter` + the per-loop
   recognizer-derivation (GN-2.W1) do not exist. "Live consumer (`find_member`)" is aspirational.
3. **Island C (projection canon) — likely a canonicalization, not an island; contests LOCK-1's
   `Project`.** PROTO-1's table-always-wins implies `map∘map` / dead-projection are always-apply
   rewrites (one canonical form) → by gate (i) they are plain rewrites, not islands. LOCK-1 owns the
   `Project` discard-site and its e-graph family (1D) is BANKED-vacuous → no LOCK-1 e-graph to share.
4. **Island count unresolved; honest floor may be 0 fully-instantiated islands** (A cross-lock, B
   unmodeled, C canonicalization). The family currently survives as a *design* whose first
   instantiation is downstream/cross-lock work.
5. **The manufacturing rewrite's confluence with the all-narrowing set is untested** — the actual
   R-1-null-breaking artifact, now shown (§0) to be non-trivial in the project's own code.
6. **GN-2.W1 supersession undeclared (clause ix)** — "subsumes and generalizes" is not a disposition;
   is Island B a replacement / wrapper / waits-on GN-2.W1?
7. **`regen∘regen` byte-identity untested end-to-end; ceremonial ~68-field provenance deletion
   unexercised.**

**Pass-2 directives:** advance the canonicalization spine (anchored by the legacy exemplar); resolve
the island count to a concrete N with a live consumer; carry Islands A/C as WITHHELD pending
cross-lock cession (below). Do not island a decision that is a plain always-apply canonicalization.

### 2B — IR-IS-EGRAPH · BLOCKED · 40%
*Convergence: between critic floors (CRIT-1 38, CRIT-2 42). CRIT-2's 42 reasoning (`already a
Language`) is wrong-tree; corrected, feasibility rises (a proven wired exemplar exists) but
distinctive value falls (extraction triple-refuted inert) — net 40, held low by the unresolved fork
and untested skinny gates.*

Blocked on the primary architectural fork (2A⊗2B middle-IR) and on genuinely untested skinny gates.
The `Ref`-as-leaf / per-rule root-table insight is sound — and *already realized* in the legacy
`write_back.rs`, which raises this from "novel structural risk" to "port a proven design."

**Open gaps:**
1. **The distinctive thesis is triple-refuted as inert.** "The IR *becomes* the e-graph; extraction
   *is* the lowering" buys value over 2A's local canonicalization only if extraction chooses among
   >1 candidates. The legacy tree's own wired rule set is all-narrowing (§0); avg class ≈ aegraph's
   1.13 is the predicted reality. Absent a manufacturing rewrite, 2B reduces to "GVN/hash-consing for
   free," which 2A inherits from the shared engine.
2. **Skinny compile-budget incl. teardown — untested** against ~230 µs. (The legacy tree runs it on
   *its* compile path but is not held to the skinny latency target.)
3. **Skinny regen byte-identity through cost-guided extraction — untested** (total-order tie-break
   obligation; `HashMap` iteration inadmissible). The legacy path presumably satisfies a determinism
   invariant — measure whether the port does under the skinny regen law.
4. **The extraction-crossover exhibit (one grammar where greedy busts the budget and the knapsack
   recovers) does not exist in-IR.**
5. **AC confluence** of the one-directional Seq/Alt normal-forming is asserted, untested.

**Pass-2 directives:** the port-feasibility prototype (below) is 2B's mover — measure the legacy
`crates/ir/src/egraph/` ported into skinny against budget + regen. Feed the result into the
2A⊗2B fork decision (a GESTALT-level call). Do NOT reopen "make the IR a Language" as if greenfield —
mine the legacy exemplar.

### 2C — CSP-CENTRAL · BANKED · 35%
*Convergence: CRIT-1 32, CRIT-2 50. Banked, not scored on the 50 (that credited the propagator, now
recorded as the surviving asset); the family-as-primacy takes the 32 floor, rounded to 35 to reflect
the tested propagator.*

**Redirect / crowding resolution:** both critics converge that 2C-as-primacy collapses to 2A Island
B — the Unison ceiling (CSS-L4 2875 DTA states ≫ ~946-1000 proven-optimal/function) forbids the
monolithic "one global COP per grammar," and the surviving "bounded post-solve over a small coupled
sub-CSP" is operationally 2A Island B. **2C is therefore BANKED: its distinctive primacy claim is
retired; its `bounded_sum` propagator (tested, real) is a banked asset folded into Island B.**

**Open gaps (inherited by Island B):**
1. **Budget-binds-on-REAL-costs unproven** — shown only under synthetic Prefilter=40/Exact=8.
2. **Decision sites unmodeled in the IR** (`SelfLoop`/`Prefilter` absent; GN-2.W1 unbuilt).
3. **Consumer-less as it stands** — no reader of the plan.
4. **End-to-end `regen∘regen` untested** (CSP-layer only).
5. **Degenerate without 2D** — without a fitted cost table the objective is `static_size_cost`.

**Underexplored formulation to OPEN (redirect):** **PBQP** (research-C's own prior art — LLVM
`RegAllocPBQP`, "Optimal DNN Primitive Selection with PBQP"). The scan-plan/backend-shape decision
graph is *near-tree* (sites independent save the budget + a few coherence edges); PBQP is
optimal-on-trees, linear on sparse graphs, and may be the correct solver shape for the coupled
sub-CSP — sidestepping both the generic-B&B cost and the Unison monolith. Nobody prototyped it;
everyone reached for e-graph extraction or generic CSP. Open PBQP as the underexplored middle.

### 2D — PROFILE-CLOSED-LOOP · BLOCKED · 33%
*Convergence: critics' floor (CRIT-1 40, CRIT-2 33); take 33 — SPEC-ONLY, zero executed artifact,
clause vii.*

The loop discipline is sound and correctly precedented (AutoFDO/BOLT/Ansor); the load-bearing
determinism invariant (loop closes at calibrate-time, not regen-time → `regen∘regen` survives) is
non-negotiably right. But every empirical claim is untested.

**Open gaps:**
1. **Zero executed prototype** — PROVE-2D-A (fitted coefficient flips a decision AND wins cold-paired)
   and PROVE-2D-B (freshness goes RED on mutation) never ran. Under clause vii, 2D cannot anchor a
   perf claim until one runs.
2. **Attribution underdetermination unresolved** — whole-grammar cyc/B, ~15 equations, dozens of
   features → β unidentifiable. The A/B-differential primitive is *proposed*, not built (chooses
   neither — elegant-reduction trap).
3. **No knob shown to have fitted-optimum ≠ current-value with a measured win** — the null-trap escape
   (kernel crossover, `CSS_SHUFTI_MIN_SPAN`, GATE=16, capacity) is asserted from spread *elsewhere*.
4. **`from_env` masked-fallback not fenced off the regen path** — dual cost source; if regen reads env
   when the table is absent, `regen∘regen` breaks. Fence env out of regen.
5. **Lattice plane stays BLOCKED** (§0 null recurs); unblocked in principle only at thresholds,
   unmeasured. 2D's applicable plane is exactly where the generators do not need it.

**Pass-2 directives:** 2D is a **composable layer under whichever of 2A wins** (not exclusive with
any) — but it must ship ONE executed fit before it may speak of convergence. Its exact prerequisite:
a candidate generator with real per-site alternatives (Island B's real costs).

---

## 3. Crowding, cross-pollination, and the incompatibility fork

**BANKED for crowding:** 2C-as-primacy → folded into 2A Island B (both critics concur; Unison-refuted
monolith). The `bounded_sum` propagator survives as the banked mechanism.

**OPENED to de-crowd:** PBQP as the near-tree solver formulation for the coupled scan-plan sub-CSP
(§2C) — the underexplored middle between e-graph extraction and generic B&B.

**Cross-pollination grafts (both families developed enough; named + justified):**
1. **2C `bounded_sum` → 2A Island B solver.** PROTO-2's tested propagator *is* Island B's budget
   knapsack. The primary graft; the seam both families always touched.
2. **2D fitted per-site costs → Island B objective.** Replaces PROTO-2's synthetic Prefilter=40/
   Exact=8 with fitted cyc/B. Gated on 2D running one fit.
3. **Legacy `crates/ir/src/egraph/` → 2A/2B skinny port.** The single highest-value graft this pass
   surfaces: a *working, wired, in-project* exemplar of `GrammarENode: Language` / `write_back` /
   `rules/regex.rs` / `analysis/`. 2B's skinny instantiation is a port of proven code, not a
   greenfield build. Its all-narrowing rule set is also the decisive §0 datum (canon-suffices).

**Primary fork (unresolved — GESTALT-level):** 2A (directed spine, IR is an annotated tree between
islands) ⊗ 2B (IR *is* one e-graph end-to-end). The port-feasibility prototype (§4) supplies the
input; the fork itself is decided at gestalt, not by a LOCK-2 pass.

---

## 4. Pass-2 briefs (what research must answer · what to prototype · what stays withheld)

**PROTOTYPE (the movers):**
- **PROVE-2-BRANCH-v2 — the real, non-synthetic §0 test.** (a) MEASURE the legacy wired grammar
  e-graph's *actual* average e-class size after saturation on the fleet grammars (it runs at
  `crates/core/.../pipeline.rs:219`) — if ≈1.13, canon-suffices is the honest §0 answer; if
  meaningfully >1, extraction is live. (b) Author a REAL registered `ManufactureDispatchAlt` rewrite
  fired through `CspScheduler` and PROVE it confluent with the all-narrowing set + terminating — the
  step both PROTOs and both critics flag as the unbuilt load-bearing core. (c) Re-measure Part B
  cyc/B at REAL value-start positions only (post ws-skip), not every corpus byte.
- **PROVE-2B-PORT — 2B's mover and the fork's input.** Port the legacy `crates/ir/src/egraph/` into
  skinny; measure build+saturate+extract+**teardown** against ~230 µs and `regen∘regen` byte-identity
  through cost-guided extraction. Resolves 2B gaps 2/3 with the legacy design as the substrate.
- **PROVE-2C-REALCOST — Island B / 2C teeth on real costs.** Build the `SimdSite::SelfLoop`/
  `SimdMode::Prefilter` IR extension so sites are IR-modeled; feed the `bounded_sum` CSP GN-2.W1-
  lowered (or 2D-fitted) real static-size + cyc/B; does the budget STILL bind? End-to-end
  `regen∘regen` through a real consumer. Compare the B&B CSP to a **PBQP** formulation on solve cost.
- **PROVE-2D-A/B — 2D's first executed artifact.** Pick `select_backend` kernel crossover
  (`dispatch.rs:89`) or `CSS_SHUFTI_MIN_SPAN`; A/B-differential fit; prove a fitted value flips a
  decision AND wins cold-paired AND drives `report.rs:1750` freshness RED on mutation.

**RESEARCH MUST ANSWER:**
- The 2A⊗2B fork: does the legacy-exemplar port stay in the skinny compile budget + regen fixpoint?
- Does any fleet grammar present ≥2 *IR-modeled* competing scan sites with *real* binding costs?
- Is PBQP the right solver shape for the near-tree coupled sub-CSP (vs generic B&B / e-graph
  extraction)?

**CROSS-LOCK ADJUDICATION (blocks 2A Islands A/C until GESTALT rules):**
- Island A regex-canon rewrite set: LOCK-2 vs LOCK-5 ownership (legacy `rules/regex.rs` is the shared
  exemplar; LOCK-5 is the constitutional owner of the lawful HIR form — needs explicit cession).
- Island C projection canon: LOCK-1 owns `Project`; LOCK-1's e-graph family is BANKED-vacuous → no
  shared engine. Withhold Island C pending LOCK-1's store disposition.
- GN-2.W1 supersession must be NAMED (clause ix) before Island B lands.

**WITHHELD FROM RESEARCHERS (do not re-open):**
- 2C-as-primacy (banked; Unison-refuted). Researchers receive only the `bounded_sum`-as-Island-B-
  solver framing + the PBQP alternative.
- The census: settled (§0). Skinny has no grammar e-graph (research correct); legacy `crates/` has a
  wired one (CRIT-2 correct, wrong tree). Do not re-audit; do not re-litigate CRIT-2's convergence
  drops or dead-substrate catalogue — they are wrong-tree artifacts.
- Islands A/C active prototyping — withheld pending cross-lock cession above.

---

## Pass history

### Pass 1 (agglomerated from research-{A,B,C,D,priorart}, FAMILY-SPECS, PROTO-1/2, CRIT-1/2)
- **Governing correction:** the census dispute between the critics is a two-tree identity error, not
  a judgment call — resolved on disk. Skinny tree (LOCK-2's subject) has no grammar e-graph; the
  legacy `crates/` tree has a working, wired one. Both facts stand; reconciliation settled.
- **Decisive §0 datum found on disk:** the legacy tree's own wired rule set is all-narrowing → §0
  triple-confirmed (R-1 null + aegraph 1.13 + in-project all-narrowing set). Canonicalization is the
  value; extraction is inert absent an unbuilt, unproven-confluent manufacturing rewrite.
- **Dispositions:** 2A ADVANCED 55 (core spine); 2B BLOCKED 40 (net-new in skinny but de-risked by
  legacy exemplar, distinctive value refuted); 2C BANKED 35 (primacy → Island B; propagator banked);
  2D BLOCKED 33 (SPEC-ONLY, zero fits).
- **Crowding:** banked 2C-as-primacy into Island B; opened PBQP as the underexplored solver
  formulation.
- **Prototypes lifted:** PROTO-1 → machinery-not-broken (NOT extraction-decision-real);
  PROTO-2 → propagator-works (NOT teeth-on-corpus). Both PASS verdicts softened to the critics' floor.
