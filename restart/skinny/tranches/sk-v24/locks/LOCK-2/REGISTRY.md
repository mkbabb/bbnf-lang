# LOCK-2 — THE GRAPH ENGINE — REGISTRY

*e-graph + CSP optimization: HIR canon, IR rewriting, kernel/backend selection, capacity & schedule
decisions, profile-fed cost, deterministic regen. Families — 2A EGRAPH-ISLANDS · 2B IR-IS-EGRAPH ·
2C CSP-CENTRAL · 2D PROFILE-CLOSED-LOOP. This registry is the agglomerator's running ledger across
passes; the newest pass governs, prior passes retained below as history.*

Constitution binds: consumer-less substrate forbidden (viii/Y.13); type plane costs zero at runtime
(v); concrete artifacts or SPEC-ONLY (vii); refutation laws — no warm benches, ratios certified /
absolutes directional (iv); deterministic regen (`regen∘regen = regen` byte-identical); extant
GN/PT codex is the baseline, divergences are NAMED supersessions (ix).

**All XL-letters in this pass-2 body are the ORCHESTRATOR ledger's (`CROSS-LOCK.md`, A..H), which is
the binding authority. Pass-1 used a private A..G lettering that collided with it; §3 carries the
reconciliation and every pass-1 citation is re-anchored here.**

---

## Current disposition (as of pass 2)

| Family | Status | Conv. | One-line |
|---|---|--:|---|
| **2A EGRAPH-ISLANDS** | **ADVANCED** | **56%** | The surviving frame. Two decision-grade probe advances this pass: the pick's DIRECTION tracks measured cyc/B (dispatch faster at N=8, sequential at N=2, 7/7 CIs both directions) and a child-bearing tree-profile e-node is authorable *directly over skinny's flat `GrammarIr`* in-budget (19.7 µs). But the binding hole is UNCHANGED: **0 child-bearing islands whose extraction pick MOVES emitted code** — both probes discard their pick (`let _organ`, `let _ = best_node`). Gate (i) can go RED and its structural half is exercised; the live-consumer half is not. |
| **2B IR-IS-EGRAPH** | **BANKED** | **44%** | BLOCKED-candidate LIFTED (the pass-1 NP-hard context-sensitive-DAG objection is refuted — context-freedom RATIFIED on the structural argument: `admits_sink_only` takes no caller arg, one global `target`, real walk 0 conflicts) — but it **earns no distinct existence over 2A** (both critics concur). The ≥2 survivors the child-node probe showed are MANUFACTURED by the widening rewrite, at a discarded consumer; the surviving node IS 2A's island node. Its three assets — context-freedom, child-node-over-flat-arena buildability, `Ref`-as-leaf/per-rule-root-table — bank into 2A. |
| **2C CSP-CENTRAL** | **BANKED** | **50%** | As-primacy stays RETIRED (pass-1, Unison ceiling). NEW this pass: the solver-shape DESIGN question is CLOSED — **B&B + `bounded_sum` over the near-tree scan-plan graph is the home; PBQP is ceremony** (it cannot encode Σcost≤B as a pairwise cost; its Lagrangian budget-dual carries an irremovable integrality gap, css-l4 slack obj 39.003 vs true 21.011, at ~10⁴× wall-clock). But **`bounded_sum` is VERIFIED ABSENT from the tracked tree at HEAD** (agglomerator re-checked): the propagator + 6/6 live in a divergent bank worktree. EARNED-in-a-worktree, not on the substrate. Teeth BANKED (source-real, IR-latent, consumer-blocked). |
| **2D PROFILE-CLOSED-LOOP** | **BLOCKED** | **32%** | SPEC-ONLY; **zero executed fit** (clause vii). Sound discipline, correct precedent (AutoFDO/BOLT/PGO), right determinism invariant (loop closes at calibrate-time, not regen-time). The dispatch-cycb probe hands it its first MEASURED front (the N=2/8 direction reversal) — a real target to fit — but no coefficient has been fitted from PMU and no decision flipped by a fitted value. And what must be fitted is DISTRIBUTION, not N (§0.4). Composable, not exclusive. |

**One surviving frame — 2A's directed-spine-with-islands** — into which 2B's lawful mechanism banks,
of which 2C's Island B (solver home = B&B + `bounded_sum`, landing owed) is the scan-plan seam, and
beneath which 2D is the composable coefficient layer. The lock earned **four genuine results** this
pass — the deciding DIRECTION (synthetic, clean paired PMU), substrate BUILDABILITY (observational),
2B context-freedom (structural), and the solver-shape DESIGN verdict (worktree-only primitive). It did
**not** earn extraction-as-decider *in production*: both probes' consumers are synthetic/observational
and discard their picks, so the honest floor — **0 genuine child-bearing island whose pick changes
emitted code** — is exactly where pass-1 left it. The single binding pass-3 obligation is the
**REPLACING-not-observing consumer** neither probe supplied.

---

## 0. The pass-2 governing findings

### 0.1 The synthesis was STALE against its own two decisive probes — for the SECOND time (both critics, headline)
`FAMILY-SPECS.md` (22:27) predates `PROTO-2-CHILD-NODE-IN-SEAM.md` (22:44) and
`PROTO-2-DISPATCH-CYCB.md` (22:48). Its §0.2b verdict ("the deciding PMU half UNMEASURED") and its
§0.3/§V verdict ("the substrate gap NOT dissolved … the unbuilt core, unscoped by any family, routed
to GESTALT-B as the binding blocker") were both written **before** the two probes that answer exactly
those questions and both PASS on disk. This is the **identical defect the synthesis charges its own
19:25 predecessor with, recurring within the same pass.** The agglomerator therefore scores against
disk truth as of 22:48 (the probes' actual results), not the 22:27 text — and the pass-3 directive
"re-cut the synthesis to the probes' results" is a REPEAT, not a first-time, obligation. This is a
process defect, not a family-soundness fault; it does not move a family number by itself, but no
number may be premised on a §0 verdict the synthesis had not reconciled with the probes.

### 0.2 The two probes RAN_MEASURED — what each PROVED, and where each PASS is softer than its verdict line
- **PROVE-2-DISPATCH-CYCB (the deciding half, §0.2b) — the strongest result of the pass.** On a
  runnable hand-authored lowering (`dispatch_canon_bench.rs`, `libc`-only, no e-graph), cold paired
  `ri_cycles`/`proc_pid_rusage`: at **N=8** dispatch/sequential cyc/B `hi95 < 1.0` in **7/7 CIs**
  (across 5 runs, both match + 256-entry-LUT shapes); at **N=2** the direction **reverses**,
  `lo95 > 1.0` in **7/7**; load-immune ins/B corroborates the direction at both N (dispatch does FEWER
  ins at N=8: 4.67 vs 6.32; MORE at N=2: 4.67 vs 4.25). **The N-crossover tracks real runtime cost;
  the direction question genuinely CLOSES; PROTO-1's "no PMU on darwin-ARM" excuse is refuted** (the
  counter was always `proc_pid_rusage`). *Residual softnesses (both critics):* (a) the crossover is
  **bracketed 2 < N* ≤ 8, not fitted** — the model fires at N*=3, but the true integer boundary was
  not swept, so the model may discriminate wrongly in the N=3..5 band real grammars populate; (b) the
  corpus is **round-robin uniform over `0..N`**, giving sequential its arithmetic best case `(N+1)/2`
  — the shipping cost arm reads **N alone, blind to hit distribution**; a grammar whose first alt
  matches 95% of value-starts favors SEQUENTIAL even at N=20 (CRIT-1 F2, new sharp uncertainty); (c)
  the consumer is a hand-written microbench, **not a pipeline-generated parser** (§0.2c conceded open);
  the closest real anchor (json `value`) is N=6, past any crossover.
- **PROVE-2-CHILD-NODE-IN-SEAM (the substrate gap, §0.3/§V) — buildability PROVEN; "0→1 live consumer"
  overstated.** A child-bearing `BbnfNode` (`Seq(Vec<Id>)`, `Alt{branches,dispatch}`) authored **purely
  over skinny's flat `GrammarIr`** — dragging **none** of the top-level DAG (`DagNode`/`StringId`/
  `FnId`/`AltDispatch`) — compiles in the production crate, saturates to **≥2 survivors at the real
  6-branch json `value` Alt** (pairwise-disjoint FIRST bytes), runs in median **19.7 µs** cold, picks
  dispatch (N=6 > N*=3), check-json byte-identical ×2. **This resolves the organ-SEPARABILITY horn:
  the flat arena CAN host a child-bearing tree-profile e-node in-budget — no port from legacy, no XL-A
  inversion, no GESTALT-B escalation on buildability.** *But its headline "the honest floor moves 0→1,
  a live consumer" is a masked fallback (both critics, agglomerator concurs on the patch):* the seam
  wiring is `let _organ = probe_child_bearing(…)` (discarded) and inside, `let _ = extractor.best_node(root)`
  (the pick itself is discarded) — **regen-neutral *because* consumer-less** (viii/Y.13). The node runs
  ALONGSIDE `DecisionNode` observationally; it never changes emitted code. And the ≥2 survivors are
  **MANUFACTURED** by `ManufactureDispatchAlt` unioning in the twin — without it multiplicity is 1, the
  triple-confirmed finding intact. The honest floor is **0→1 for "a child-bearing node that runs
  observationally in the seam" and 0→0 for "a child-bearing island whose pick changes emitted code"** —
  and gate (i) demands the latter.

**Net:** DIRECTION (synthetic) and BUILDABILITY (observational) advanced from SPEC-ONLY to green. What
did NOT close: the crossover FIT (§0.2a), the PRODUCTION consumer (§0.2c), gate (i)'s live-consumer
clause (pick discarded), and the CSS-L4-scale organ (2B gate 4). The master gate is **substantially
de-risked but not PASSED**; "extraction-as-decider EARNED" (the dispatch probe's verdict line)
**overreads** by conflating "the direction is real" with "extraction in the pipeline emits the faster
shape." **Base-pin note (XL-H):** the two probes are NOT co-based — dispatch on `ce51c36fc`, child-node
on `c93fc4314` (a descendant, agglomerator-verified); "both PASS ⇒ gate closed" composes two verdicts
across a base boundary. Each descent is individually clean; the composition is the XL-H nuance.

### 0.3 `bounded_sum` is VERIFIED ABSENT from HEAD (agglomerator re-check — 2C's load-bearing primitive is worktree-only)
At HEAD `90db093bf`, `crates/csp-solver/src/constraint/` = `{all_different, all_different_except,
dispatch, implication, lambda, not_equal, scratch, traits, mod}` — **no `bounded_sum.rs`, no `Soft`**;
`git grep bounded_sum|BoundedSum` over the tracked tree returns **empty**. Both critics flagged this
(CRIT-1 F4, CRIT-2 CL-4); the agglomerator confirmed it on disk. The pass-1 registry recorded the
propagator as "tested, RAN, +345 additive, 6/6" — that measurement was on a **divergent bank
worktree** carrying `soft.rs`/`cardinality.rs`/a `Soft` variant the base lacks, entered via
`ConstraintEnum::Custom`. **Landing + provenance reconciliation is a 2C convergence PREREQUISITE, not a
footnote.** The solver-shape DESIGN verdict (B&B beats PBQP) stands — it is a sound *design* result —
but the mechanism it selects does not exist on the measurement substrate (XL-A: skinny@HEAD is the only
living implementation).

### 0.4 What must be FITTED is DISTRIBUTION, not N (the pass's new sharp uncertainty, CRIT-1 F2)
The shipping cost arm is `dispatch_table + N·dispatch_branch` vs `N·alt_per_branch` — a function of **N
alone**. Real cyc/B is **distribution-dependent**: the dispatch probe's win at N=8 is *arithmetically
guaranteed* by its uniform corpus (sequential's mean trial depth `(N+1)/2`). A fit that only recovers
N*=3.0 from a uniform corpus fits the arithmetic, not the cost. This is the genuinely underexplored
middle no probe touched, and it is precisely 2D's real content — the hit-distribution dependence, not
the branch count. Opened as the pass-3 de-crowding question (§4), replacing the now-refuted PBQP
formulation.

---

## 1. Per-family ledger

### 2A — EGRAPH-ISLANDS · ADVANCED · 56%
*Convergence: both critics' floor (CRIT-1 56, CRIT-2 56), up from pass-1's 50. The +6 is two
decision-grade probe advances (DIRECTION measured, child-node BUILDABLE), both metabolized by the
critics' 22:48 scoring. No named reason to exceed or drop below the floor: the binding live-consumer
clause did not move, and distribution-blindness is priced additional, not into, the number.*

The lock's spine. The directed-shell architecture (LLVM canonicalization spine + local islands at named
decision points *within* LOCK-3's tower, XL-B) is coherent, and gate (i) — "child-bearing survivors that
CHANGE THE PICK under a fixed vector AND move cyc/B, else demote to a plain cost-ranked pass" — can
genuinely go RED (both critics credit this). The dispatch probe measured the second conjunct (cyc/B);
the child-node probe proved a child-bearing organ fits the flat arena. Both real.

**Open gaps:**
1. **[MASTER — gate (i) live consumer, NOT closed] 0 child-bearing islands whose pick MOVES emitted
   code.** Both probes left the floor at 0: I1 is a leaf-min; the child node discards `let _organ` /
   `let _ = best_node`; regen-neutral *because* consumer-less (viii/Y.13). Needed: a production rewrite
   that manufactures a real ≥2-survivor choice at the seam skinny calls, USES the pick, changes emitted
   code, and proves parity to `DecisionNode` on every shipping rule. Until then the distinctive
   extraction value (I2/I4 tree profiles) is SPEC-ONLY.
2. **[MASTER — §0.2a, DISTRIBUTION not N] The crossover LOCATION is unfitted and the model is
   distribution-blind (§0.4).** `dispatch_table + N·dispatch_branch` reads N alone; the deciding probe
   used dispatch's best-case uniform corpus; N*=3.0 is an arithmetic artifact of two slopes, never
   validated against a skewed corpus. The gate that fires on uniform hits cannot fail on its own test
   corpus.
3. **[SUBSTRATE — reframed] Buildability ANSWERED; the surviving question is PROMOTION** (CRIT-2 CL-2):
   does skinny promote the child-bearing node to the shape decider — with a parity proof that its pick
   equals `DecisionNode`'s on every shipping rule — or does it stay observational forever? The child
   node has a substrate home but no throne. The pass-1 "adopt-and-prove port from legacy" is
   **SUPERSEDED**: the node is authored fresh over the flat arena (`backend_egraph_bbnf.rs`), no legacy
   DAG dragged.
4. **[double-home, XL-F item 2 — pass-3 obligation the ledger SET and pass-2 SKIPPED] The Alt-lowering
   decision double-homes with GN-2.W1's lowering predicate.** BOTH probes exercise an Alt-lowering pick;
   the relation (**replaces / wraps / waits-on**) is still unnamed. research-B argues WRAPS with
   evidence (the predicate's structural test survives as the rewrite's `search`; its verdict survives as
   a candidate; the graph+cost owns the decision). Name it pass-3.
5. **[XL-C] I2 regex-HIR canon WITHHELD** — lives at the ENGINE tier (parse-that), which XL-C ratifies
   as the canonical home; awaits LOCK-5's lawful `Hir` rendering + keep/abrogate of `rules/regex.rs`.
6. **[scan sites — see §3] I3 scan-plan: ≥2 IR-modeled competing sites** absent from the IR
   (`SimdMode`/`SimdSite` single-valued, disk-confirmed); the derivation consumer is blocked in LOCK-9
   9C / LOCK-5 territory; none of the source-real self-loop grammars (css/pretty=15, css/l4≥10) exist as
   skinny `.bbnf` (json is the sole one). Consumer-blocked; NOT an authoritative XL-D matter (§3).
7. **[XL-F, LOCK-1] I4 projection canon — probably an always-apply canonicalization, not an island.**
   PROVE-12 monomorphized (const-fold, no e-graph) implies `map∘map`/dead-projection are always-apply →
   by gate (i) a plain rewrite; double-homed with LOCK-1's `Project`@`passes/src/lib.rs:204`.
8. **Confluence is per-rewrite × per-narrowing-set, not general.** `ManufactureDispatchAlt` is confluent
   with the shipping all-narrowing set (`DeduplicateAltBranches` did 0 work — keys on branch-set, blind
   to the dispatch dimension); a future narrowing rewrite could break it. No general guarantee.
9. **`regen∘regen` byte-identity for a *plan-reading* consumer untested; ceremonial ~68-field
   provenance deletion (gate v) unexercised** — no reader exists.

**Pass-3 directives:** build the REPLACING-not-observing consumer (gap 1 — the top mover); fit the
crossover against a SKEWED corpus, not just sweep the integer boundary (gap 2/§0.4); name the GN-2.W1
relation (gap 4, XL-F item 2); carry I2/I3/I4 WITHHELD pending the cross-lock cessions.

### 2B — IR-IS-EGRAPH · BANKED · 44%
*Convergence: both critics' floor (CRIT-1 44, CRIT-2 44), up from pass-1's 32 — the jump is the
BLOCKED-candidate LIFT (NP-hard objection refuted) + substrate buildability. Status changed BLOCKED →
**BANKED**: both critics show it "converging into 2A"; per the crowding rule the redundant family banks,
its assets folded into 2A, its distinctive thesis retired.*

The BLOCKED-candidate condition is LIFTED. Context-freedom is RATIFIED on the STRUCTURAL argument —
`admits_sink_only` (`passes/src/lib.rs:958`) takes **no caller argument**, `target` is one global
`TargetFeatures::host()`, and the real conflict walk over the skinny fleet returned **0 conflicts,
non-vacuously** (15 rules, 28 CallRule edges, 5 multi-caller rules, `pair`/`string` live sink-only
witnesses). Any repair, if ever needed, is bounded sub-rule specialization — not optimal
context-sensitive DAG extraction — and there is no conflict to repair. This is 2B's one genuine lift and
it survives the fleet-thinness caveat (structure, not empirics: json is the sole skinny `.bbnf`).

**Why BANKED, not a standalone family:**
1. **No distinct existence over 2A.** The ≥2 survivors the child-node probe showed are **manufactured**
   by the widening rewrite (multiplicity 1 without it — triple-confirmed); the surviving node **is** 2A's
   island node (same `BbnfNode`, same crossover arm); the maximal "extraction IS the decision driver"
   reading is XL-B-refuted (LOCK-3's seat, CRIT-2 CL-6 confirms LOCK-2 kept this posture clean). The
   lawful residual IS the 2A island posture. 2B is a **mechanism-span choice inside 2A** —
   persistent-in-place substrate vs discrete build-teardown islands — not a family.
2. **The mechanism-span question is UNMEASURED at scale (gate 4).** The whole-grammar organ
   (build+saturate+extract+**teardown**) at CSS-L4 scale (2875 DTA states) within ~230 µs — "the single
   most important open 2B measurement" — never ran, and CANNOT run on skinny (json only). The 19.7 µs is
   one 6-branch json Alt; `CspScheduler` still rebuilds a fresh `Csp` per iteration (opposite the aegraph
   no-rebuild lesson) — a pure-assertion scaling risk at the exact scale the substrate cannot exercise.
3. **`regen∘regen` through COST-GUIDED extraction unproven at grammar scale.** `GrammarCostModel` returns
   `f64` with a Vec tie-break; the total-order fix is owed. The child-node probe's check-json ×2 proves
   regen neutrality of a *discarded* organ (byte-identical *because* it changes nothing), NOT determinism
   of a cost-guided extraction that changes output.

**Banked assets folded into 2A:** context-freedom ratification; the child-node-over-flat-arena
buildability proof (`backend_egraph_bbnf.rs`); the `Ref`-as-leaf / per-rule-root-table acyclicity insight.

**Pass-3 directives:** do NOT reopen 2B as a standalone architecture or "make the IR a Language"
greenfield. Its surviving question — persistent substrate vs build-teardown — is answered by the
CSS-L4-scale organ measurement (2A gap; requires the fleet grammars as skinny `.bbnf` or a scaled
synthetic). The buildability question is CLOSED; do not re-open it.

### 2C — CSP-CENTRAL · BANKED · 50%
*Convergence: CRIT-2 floor 50 (CRIT-1 52). Banked. The solver-shape DESIGN verdict is genuinely closed
and decision-grade; the 50 (not higher) is the VERIFIED landing debt (§0.3) + the teeth remaining
consumer-blocked/2D-dependent. No new 2C evidence ran this pass — both probes concern extraction/
substrate, not the solver — so the number rides a prior design probe whose primitive I confirmed absent.*

As-primacy stays RETIRED (pass-1, Unison ceiling: CP proven-optimal only to ~946–1000 instr/function;
CSS-L4 = 2875 DTA states is off the cliff). What survives:

**The solver-shape DESIGN question is CLOSED (a real, earned design convergence).** On the fleet's
scan-plan decision graphs (near-tree, treewidth ≤ 2), **B&B + `bounded_sum` wins outright**: it carries
the global size budget natively as knapsack bound-consistency, optimal on every instance (3–35 nodes,
<100 µs, matches brute force). **PBQP is ceremony**: it fully reduces (R0/R1/R2) but **cannot encode
Σcost ≤ B as a pairwise cost**; its Lagrangian budget-dual carries an irremovable integrality gap
(css-l4 slack obj 39.003 vs true 21.011) at ~10⁴× wall-clock; the only alternative — a degree-n
budget-pressure node — destroys R0/R1/R2 reducibility. **This closes the pass-1 "open PBQP" question in
the negative:** PBQP is REFUSED as the decider, surviving only as (a) the recognition that coherence +
budget ARE the edges, (b) a possible home for a *soft/finite* coherence penalty IF one turns real (needs
the absent `Soft` extension — untested, load-bearing only if finite coherence preferences exist).

**Open gaps:**
1. **[LANDING — §0.3, VERIFIED] `bounded_sum` absent from `crates/csp-solver/src/constraint/` at HEAD.**
   The propagator + 6/6 live in a divergent bank worktree (`soft.rs`/`cardinality.rs`/`Soft`), via
   `ConstraintEnum::Custom`. Landing + provenance reconciliation is a convergence PREREQUISITE.
2. **[teeth, consumer-blocked — see §3] ≥2 IR-modeled competing scan sites with REAL binding costs.**
   Source says yes (css/pretty=15, css/l4≥10, bbnf=2, csv=2); the IR says no (`SimdMode`/`SimdSite`
   single-valued); the `SelfLoop`/`Prefilter` node shape is LOCK-3's; **and none of those grammars are
   skinny `.bbnf`.** Teeth are SPEC-ONLY on the substrate.
3. **Degenerate without 2D.** Absent a fitted table the objective is `static_size_cost` — "a CSP-shaped
   priority cascade with extra steps." Non-degeneracy leans entirely on 2D (zero fits run).
4. **Soft/finite coherence penalty untested** — the one residual point PBQP's pairwise matrix could win;
   unrepresentable in base B&B without the absent `Soft` extension; load-bearing only if finite coherence
   preferences are real (unknown).
5. **[one solver home — see §3] `bounded_sum` extends the same csp-solver LOCK-1 1B centralizes** — a
   GESTALT-B unification point, contingent on gap 1's provenance reconciliation. This is **NOT an
   authoritative XL-G matter** (XL-G = owner confirmations); it is a portfolio GESTALT-B point (§3).

**Pass-3 directives:** LAND `bounded_sum` from the bank tree and reconcile provenance BEFORE any
"one-solver-home with LOCK-1" claim; keep PBQP RETIRED as decider (do not re-open); the teeth stay
SPEC-ONLY pending the IR-modeled sites + a real consumer (a joint LOCK-3/LOCK-5/LOCK-9 obligation, §3).

### 2D — PROFILE-CLOSED-LOOP · BLOCKED · 32%
*Convergence: CRIT-1 floor 32 (CRIT-2 33). SPEC-ONLY, zero executed artifact (clause vii). The
dispatch-cycb front is a hair of forward motion; the −2 from pass-1's 30 baseline is net of the F1
evidence-staleness and the confirmation that the consumer 2D must co-land against does not yet exist.*

The loop discipline is sound and correctly precedented (AutoFDO/BOLT/PGO; LLVM `getFunctionHash` →
drop-on-mismatch is the direct freshness-gate precedent); the load-bearing determinism invariant — the
loop closes at *calibrate-time, not regen-time*, so `regen∘regen` survives — is non-negotiably right.
~70% of the substrate is present-but-inert. But every empirical claim is untested.

**Open gaps:**
1. **[MASTER, BINDING] Zero executed fit.** No coefficient fitted from PMU; no decision flipped by a
   fitted value. The dispatch probe hands 2D a MEASURED front (the N=2/8 direction) but on hand-authored
   lowerings, not an extraction pick; the child-node probe's real-seam front DISCARDS its pick. 2D still
   has no live ≥2-survivor decision with a consuming reader to fit against — it must co-land with the
   REPLACING-not-observing consumer (2A gap 1).
2. **[§0.4] What must be fitted is DISTRIBUTION, not N.** A fit that recovers N*=3.0 from a uniform
   corpus fits the arithmetic, not the cost. The fit's real content is the hit-distribution dependence no
   probe has surfaced. This is the sharper form of the master-gate dependency after the dispatch probe.
3. **Falsifiable freshness unexercised** — today's `check-calibration` `"pass"`/`0` stamps are theater
   until a proto reddens the gate (mutate a coefficient → RED). The anti-theater claim is itself
   provenance theater until then.
4. **The only UNBLOCKED plane is cross-lock-owned** — `select_backend` kernel crossover (LOCK-9),
   `CSS_SHUFTI_MIN_SPAN`/`GATE=16` (GN-2 scanner knobs). Fitting these supersedes hand-tuned GN literals
   — a **named supersession (ix)** the spec must DECLARE, not attach freely. The plane LOCK-2 owns (shape
   lattice) is master-gate-BLOCKED.
5. **`from_env` masked-fallback** must stay fenced off the regen path (a CI grep-gate); attribution
   ceiling = count of independently-toggleable levers with a paired bench — undisclosed, must be stated.

**Pass-3 directives:** ship ONE executed fit — a fitted value FLIPS a real decision AND wins cold-paired
AND drives freshness RED on mutation — before 2D may speak of convergence. Prerequisite: the live
≥2-survivor decision at the real seam (2A gap 1). Fit DISTRIBUTION, not N. Name the LOCK-9/GN knob
supersession.

---

## 2. The two probes — RAN_MEASURED ledger

| Probe | HEAD | Verdict | Earns | Does NOT earn |
|---|---|---|---|---|
| **PROVE-2-DISPATCH-CYCB** | `ce51c36fc` | PASS | The pick's DIRECTION tracks measured cyc/B (N=8 dispatch faster, N=2 sequential, 7/7 CIs both ways, ins/B corroborates) — §0.2b closed | The crossover FIT (§0.2a bracketed 2<N*≤8), the DISTRIBUTION dependence (uniform corpus = dispatch's best case), the PRODUCTION consumer (synthetic microbench) |
| **PROVE-2-CHILD-NODE-IN-SEAM** | `c93fc4314` (descendant of the above) | PASS | A child-bearing tree-profile e-node is authorable over skinny's FLAT `GrammarIr` in-budget (19.7 µs), ≥2 survivors at real json `value`, regen-neutral — organ-SEPARABILITY, no XL-A inversion | The LIVE consumer (pick discarded, `let _organ`/`let _ = best_node`), the intrinsic multiplicity (≥2 is MANUFACTURED), the CSS-L4-scale organ (one small rule) |

Both individually clean base descents; the composition crosses a base boundary (XL-H). Neither closes
the binding hole: **0 child-bearing island whose pick MOVES emitted code.**

---

## 3. Cross-lock reconciliation (CRIT-2's distinctive finding — the pass-1 private lettering is SUPERSEDED)

The pass-1 registry and the pass-2 FAMILY-SPECS cite a private XL-lettering (A..G) that **collides with
the orchestrator's binding `CROSS-LOCK.md` (A..H)**. The ledger is the authority ("treat them as
binding"); a citation by a letter that now names a *different* adjudication is an unverified cross-lock
anchor. Verified against `CROSS-LOCK.md` this pass:

| Substance (what LOCK-2 means) | pass-1/FAMILY-SPECS said | AUTHORITATIVE ledger letter | note |
|---|---|---|---|
| Tree of record | XL-A | **XL-A — TREE OF RECORD** ✓ | correct. skinny@HEAD = MEASUREMENT SUBSTRATE; ports scope organ-level. |
| Middle-IR / decision-driver ownership | XL-B | **XL-B — TOWER OWNERSHIP** ✓ | correct (WORKING, final at GESTALT-B). CRIT-2 CL-6: LOCK-2 kept this clean. |
| Regex-canon exemplar (I2) | "XL-E" (private) | **XL-C — THE REGEX AXIS** | re-anchor. XL-C ratifies engine-tier `Hir` at parse-that; I2 WITHHELD there. |
| Scan-derivation / `SelfLoop`/`Prefilter` consumer | "XL-D" (private) | **XL-F — NAMED SUPERSESSIONS, item (2)** + LOCK-9 9C / LOCK-5 registries | re-anchor. Authoritative XL-D = TS EXECUTION FACE (unrelated). The GN-2.W1 predicate supersession lives under XL-F(2); the consumer-blocked substance is a portfolio fact, not an XL-lettered adjudication. |
| GN-2.W1 lowering-predicate relation | (unnamed) | **XL-F item (2)** — "replaces/wraps/waits-on to be confirmed pass-3" | the ledger SET this obligation; pass-2 SKIPPED it (CRIT-2 CL-3). Name it pass-3 (research-B: WRAPS). |
| Projection canon (I4) double-home | "XL-F" (private) | LOCK-1 1D + LOCK-1-owned `Project`@204 | re-anchor. Authoritative XL-F = named supersessions (unrelated to projection). A portfolio double-home, not an XL letter. |
| One solver home with LOCK-1 1B | "XL-G" (private) | (no ledger letter) — GESTALT-B portfolio point | re-anchor. Authoritative XL-G = OWNER CONFIRMATIONS (unrelated). "one solver home" is a GESTALT-B unification point, contingent on `bounded_sum` landing (§0.3). |
| Base law (measurements name HEAD) | XL-H | **XL-H — BASE LAW** ✓ | correct. The two probes' cross-base composition is the live XL-H nuance (§2). |

**Consequence:** every load-bearing "consumer BLOCKED (XL-D)" and "one-solver-home (XL-G)" claim in the
prior body inherited the mis-anchor. The SUBSTANCE holds (scan sites genuinely un-IR-modeled; the solver
genuinely should unify with LOCK-1), but the letters are corrected here and the pass-3 synthesis must
carry only authoritative letters. This is a cross-lock discipline defect the within-lock critic could not
see — binding, and folded.

---

## 4. Crowding, cross-pollination, and the fork

**RETIRED / BANKED for crowding:**
- **2C-as-primacy** — RETIRED pass-1 (Unison). Stays retired.
- **2B-as-a-family** — BANKED into 2A this pass (both critics: "converges into 2A"). Its distinctive
  "extraction IS the decision driver" thesis is XL-B-refuted; its multiplicity>1 claim is
  manufactured-not-intrinsic; its lawful residual IS 2A's island posture. Assets folded (§1/2B).
- **PBQP-as-decider** — CLOSED (refused) this pass by the solver-shape verdict. The pass-1 "opened PBQP
  as the underexplored near-tree solver" question is ANSWERED in the negative (§1/2C).

**OPENED to de-crowd (replacing the closed PBQP question):** the **distribution-aware cost model**
(§0.4). The cost arm reads N alone; real cyc/B depends on alt-hit distribution; the uniform corpus is
dispatch's arithmetic best case. Does a first-set-frequency-weighted / distribution-aware Alt cost arm
change the pick on a skewed corpus, and can 2D fit the hit-distribution dependence? This is the genuinely
underexplored middle no model or probe addressed, and it is 2D's real content.

**Cross-pollination grafts (both families developed enough; named + justified):**
1. **2C `bounded_sum` → 2A Island B solver** — the primary graft, the seam both families always touched.
   **Now gated on the VERIFIED landing debt (§0.3):** the propagator must LAND from the divergent bank
   before the graft is real. Coordinate with LOCK-1 1B (one solver home, a GESTALT-B point — NOT XL-G).
2. **2D fitted per-site costs → Island B objective** — replaces the synthetic Prefilter=300/Exact=100
   with fitted cyc/B. Sharpened by the dispatch probe (the direction reversal is 2D's first real
   cost-fact) AND by §0.4 (the fit's real content is distribution, not N). Gated on 2D running one fit.
3. **Child-node organ (`BbnfNode` over flat arena) + measured direction → the PROMOTION mover.** The two
   probes together de-risk the master gate; the graft is to PROMOTE `BbnfNode` to the shape decider (or a
   co-decider at the Alt-lowering point), fed by 2D's distribution-aware fit, selected via 2C's B&B +
   `bounded_sum` where the budget couples. **This SUPERSEDES pass-1's "port the legacy `crates/ir/egraph`
   into skinny" graft** — the child node is authored fresh over the flat arena, no legacy DAG dragged, no
   XL-A inversion (a named supersession of pass-1 graft 3, driven by PROVE-2-CHILD-NODE).

**The primary fork — RE-STATED, NARROWED.** It remains **LOCK-2 ⊗ LOCK-3** (XL-B tower ownership,
WORKING, final at GESTALT-B), and CRIT-2 CL-6 confirms LOCK-2 held the posture clean (no encroachment on
LOCK-3's seat). But the child-node probe NARROWS it: the buildability horn is answered (a child-bearing
node IS authorable in skinny), so the surviving GESTALT-B question is **PROMOTION** — does skinny promote
the child-bearing node to the shape decider (with a parity proof vs `DecisionNode`), or does it stay
observational forever? The intra-lock 2A⊗2B fork is DISSOLVED (2B BANKED into 2A).

---

## 5. Pass-3 briefs (what to prototype · what research must answer · what stays withheld)

**PROTOTYPE (the movers):**
- **PROVE-2-PROMOTE — the REPLACING-not-observing consumer (THE top mover, both critics' bottom line).**
  A production rewrite that manufactures a real ≥2-survivor choice at the seam skinny calls, USES the
  extraction pick (not `let _ = best_node`), CHANGES emitted code, and proves parity to `DecisionNode` on
  every shipping rule (so regen stays byte-identical where the shape is unchanged, and DIVERGES exactly
  and only where the measured pick is genuinely better). This is the one artifact both probes stepped
  around; it converts gate (i)'s live-consumer clause from 0 to 1 and is the sole route to
  "extraction-as-decider EARNED in production."
- **PROVE-2-DIST — the distribution-aware crossover FIT (§0.2a/§0.4).** Re-run the cyc/B measurement on a
  SKEWED corpus (first alt matches 95% of value-starts) and sweep the integer boundary; show whether the
  N-only model mis-decides in the N=3..5 band, and whether a first-set-frequency-weighted cost arm fixes
  it. This is simultaneously 2D's first fit target.
- **PROVE-2-ORGAN-SCALE — the CSS-L4-scale organ (2B gate 4, never run).** build+saturate+extract+
  **teardown** at ~2875 DTA states within ~230 µs, on a fleet grammar authored as skinny `.bbnf` or a
  scaled synthetic; settles persistent-substrate vs build-teardown and the `CspScheduler`-rebuild scaling
  risk.
- **PROVE-2D-FIT — 2D's first executed fit.** Fit a coefficient from A/B-differential PMU; prove it FLIPS
  a real decision, wins cold-paired, drives `check-calibration` RED on mutation. Prerequisite: PROVE-2-
  PROMOTE's live ≥2-survivor decision. Name the LOCK-9/GN knob supersession.
- **LAND `bounded_sum`** from the divergent bank into `crates/csp-solver/src/constraint/` and reconcile
  provenance; re-run the 6/6 on the substrate. Prerequisite to any "one solver home" claim.

**RESEARCH MUST ANSWER:**
- Does a distribution-aware (first-set-frequency-weighted) cost arm change the pick where the N-only model
  does not — and does the N-only model mis-decide on any real fleet grammar in the N=3..5 band?
- What is the CSS-L4-scale organ's build+saturate+extract+teardown cost against ~230 µs, and does the
  per-iteration `Csp` rebuild scale?
- The GN-2.W1 relation (XL-F item 2): replaces / wraps / waits-on — with evidence (research-B: WRAPS).
- Does the fleet ever present ≥2 *IR-modeled* competing scan sites with *real binding* costs — who owns
  building the `SelfLoop`/`Prefilter` node shape (LOCK-3) and the derivation (LOCK-5/LOCK-9)?

**WITHHELD FROM RESEARCHERS (do not re-open):**
- **PBQP-as-decider** — CLOSED/refused this pass (budget not encodable as pairwise cost; Lagrangian gap;
  degree-n node destroys reducibility). Researchers receive only B&B + `bounded_sum` as the solver home,
  and the SOFT-coherence-penalty residual as the *only* surviving PBQP-shaped question (IF finite
  coherence preferences turn real).
- **2C-as-primacy** — RETIRED (Unison). Stays retired.
- **Buildability of a child-bearing node in skinny** — ANSWERED (child-node probe). The surviving
  question is PROMOTION; do not re-open "is it buildable" or "port from legacy" (superseded).
- **The census** — settled: skinny has no grammar e-graph; the child-bearing node is authored fresh over
  the flat arena, NOT ported from legacy. Do not re-audit; do not frame as a legacy port.
- **2A⊗2B as a "primary fork"** — dissolved (2B BANKED into 2A). The fork is LOCK-2⊗LOCK-3 (XL-B),
  narrowed to PROMOTION.
- **The private XL-lettering** — superseded by the authoritative ledger (§3). Cite only `CROSS-LOCK.md`
  letters A..H going forward.

---

## Pass history

### Pass 2 (agglomerated from research-{A,B,C,D,priorart}, FAMILY-SPECS [22:27, stale ×2], PROTO-2-CHILD-NODE-IN-SEAM + PROTO-2-DISPATCH-CYCB [both RAN_MEASURED PASS], the on-disk CRIT-1/CRIT-2, CROSS-LOCK.md A..H)
- **Two probes RAN_MEASURED, both PASS, both softer than their verdict lines.** Dispatch-cycb: the
  pick's DIRECTION tracks measured cyc/B (7/7 CIs both ways) — §0.2b closed; residual = the crossover
  FIT unfitted, corpus uniform (distribution-blind), consumer synthetic. Child-node: a child-bearing
  e-node is BUILDABLE over skinny's flat arena in-budget — but its pick is DISCARDED (consumer-less,
  regen-neutral *because* consumer-less) and its ≥2 is MANUFACTURED. **The binding hole — 0
  child-bearing island whose pick MOVES emitted code — is UNCHANGED.**
- **`bounded_sum` VERIFIED ABSENT from HEAD** (`90db093bf`, agglomerator re-check): 2C's solver-shape
  DESIGN verdict (B&B beats PBQP) stands, but its load-bearing primitive is worktree-only — EARNED-in-a-
  worktree, not on the substrate. Landing is a convergence prerequisite.
- **Cross-lock lettering RECONCILED** (CRIT-2's distinctive finding): the pass-1 private A..G collides
  with the authoritative `CROSS-LOCK.md` A..H; §3 re-anchors every citation (regex→XL-C, GN-2.W1→XL-F(2),
  one-solver-home→GESTALT-B-not-XL-G, scan-derivation→XL-F(2)+LOCK-9/5 not XL-D).
- **Dispositions:** 2A ADVANCED 56 (spine; two probe advances; binding live-consumer hole unmoved);
  2B **BANKED** 44 (BLOCKED-candidate LIFTED via context-freedom RATIFIED, converges into 2A, assets
  folded); 2C **BANKED** 50 (solver-shape DESIGN closed, `bounded_sum` verified-absent, teeth latent);
  2D BLOCKED 32 (SPEC-ONLY, zero fit; dispatch front is a target but distribution, not N, is the content).
- **Crowding:** 2B banked into 2A; PBQP CLOSED (refused, not just opened); OPENED the distribution-aware
  cost model as the new underexplored middle. Primary fork re-stated LOCK-2⊗LOCK-3 (XL-B), NARROWED to
  PROMOTION (buildability answered). Pass-1 "legacy port" graft SUPERSEDED by the flat-arena child node.
- **Top pass-3 mover:** the REPLACING-not-observing consumer (PROVE-2-PROMOTE) — the one artifact both
  probes stepped around.

### Pass 1 (agglomerated from research-{A,B,C,D,priorart}, FAMILY-SPECS, PROTO-1/2 [RAN_MEASURED], the FINAL on-disk CRIT-1/CRIT-2)
- **Correction of the prior draft:** the earlier registry draft cited superseded critic numbers (2A
  55/58, 2B 38/42, 2C 32/50, 2D 40/33) and framed §0 as a two-tree "census dispute" the FINAL critiques
  do not have (CRIT-2 re-anchored on disk, concurs). Corrected to the on-disk floor: 2A 50, 2B 32, 2C 30,
  2D 30.
- **Two NEW governing findings folded in:** (1) PROTO-1 ran on the LEGACY tree (`bbnf-ir = crates/ir`) —
  the sharpest uncertainty tested on the wrong codebase; the legacy→skinny port was the unbuilt core no
  family scoped ("adopt-and-prove", not "build"/"delete"). *[Pass-2 SUPERSEDES: the child-node probe
  authored the node fresh over the flat arena — no legacy port needed.]* (2) LOCK-2 mislocated its primary
  fork: the real fork is LOCK-2⊗LOCK-3 (middle-IR / decision-driver ownership), not intra-lock 2A⊗2B;
  plus the cross-lock ledger.
- **Decisive §0 datum retained:** the legacy tree's own wired rule set is all-narrowing → §0
  triple-confirmed (R-1 null + aegraph 1.13 + in-project all-narrowing set). Canonicalization is the
  value; extraction is inert absent a manufacturing rewrite. *[Pass-2: the manufacturing rewrite
  (`ManufactureDispatchAlt`) fires confluent + terminating, so extraction is ALIVE — but its pick is
  discarded, so the consumer floor holds.]*
- **Prototypes RAN_MEASURED:** PROTO-1 → machinery-not-broken (on legacy), not extraction-decision-real
  (in skinny). PROTO-2 → propagator works + regen byte-identical (of a plan nothing reads), teeth
  source-real but IR-latent. *[Pass-2: PROTO-2's `bounded_sum` verified NOT at HEAD — worktree-only.]*
- **Dispositions:** 2A ADVANCED 50; 2B BLOCKED 32; 2C BANKED 30 (as-primacy RETIRED → Island B);
  2D BLOCKED 30. **Crowding:** retired 2C-as-primacy into Island B; opened PBQP as the underexplored
  near-tree solver. Primary fork re-located to LOCK-2⊗LOCK-3 (XL-B). Tree-of-record gated the port
  direction. *[Pass-2: PBQP CLOSED/refused; port direction MOOT (flat-arena node).]*
</content>
</invoke>
