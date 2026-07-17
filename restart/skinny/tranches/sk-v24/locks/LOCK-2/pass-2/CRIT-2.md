# LOCK-2 — THE GRAPH ENGINE — pass-2 CRIT-2 (adversarial critic 2 of 2, CROSS-LOCK weight)

*Charter: harden/challenge/refine every family against the failure-mode checklist + constitution
violations + prototype-evidence misuse, weighting the CROSS-LOCK view — where this lock contradicts or
double-homes work against the extant GN/PT codex and the other locks (LOCK interactions live in the
portfolio; the ledger's adjudications are binding, challenges route to the gestalts with evidence).
This CRIT-2 supersedes the 20:12 CRIT-2, which — like the 20:12 CRIT-1 — predates the two probes that
actually ran (22:44, 22:48). Disk re-verified this pass. Base-pin note: the two decisive probes are NOT
co-based — PROTO-2-DISPATCH-CYCB measured HEAD `ce51c36fc`, PROTO-2-CHILD-NODE-IN-SEAM measured HEAD
`c93fc4314` (a descendant) — so "both PASS ⇒ master gate closed" composes two verdicts across a base
boundary (XL-H).*

---

## 0. TWO HEADLINE DEFECTS — one a RECURRENCE, one a cross-lock anchoring error

### 0.1 The FAMILY-SPECS is STALE against its own two decisive prototypes — AGAIN (self-supersession, second occurrence)
The 22:27 FAMILY-SPECS opens by declaring it "RE-CUTS the 19:25 draft, which both critics flagged as
STALE — it emitted Brief 1 + Brief 2 and then those briefs RAN … without the synthesis being updated."
**The 22:27 document commits the identical sin a second time.** Its §0.2 verdict is "the deciding PMU
half UNMEASURED"; its §0.3 verdict is "the substrate gap … NOT dissolved … still the unbuilt core …
still unscoped by any family"; its §V routes the substrate contradiction to GESTALT-B as "the pass's
binding blocker." It then names Brief A (child-bearing node at the seam skinny calls) and Brief B (the
runnable-lowering cyc/B measurement) as "the two sharpest uncertainties for pass 3" (lines 481-485).

**Both briefs then RAN — 17 and 21 minutes after the synthesis was written — and BOTH PASS:**

| file | mtime | what it is | FAMILY-SPECS §0 calls it | stale? |
|---|---|---|---|---|
| `FAMILY-SPECS.md` | **22:27** | the scored synthesis | — | — |
| `PROTO-2-CHILD-NODE-IN-SEAM.md` | **22:44** | Brief A — RAN, **PASS** (child-bearing node in skinny seam) | "§0.3 the unbuilt core, unscoped by any family; binding blocker → GESTALT-B" | **YES** |
| `PROTO-2-DISPATCH-CYCB.md` | **22:48** | Brief B — RAN, **PASS** (dispatch cyc/B, direction reversal) | "§0.2b the deciding PMU half UNMEASURED" | **YES** |

The two verdicts the FAMILY-SPECS marks as the lock's binding blockers (§0.2b, §0.3/§V) both have
disk-resident PASS results the synthesis never absorbed. This is not a nit — it is the document failing
the exact standard it invokes against its predecessor, and it recurs *within the same pass*. **No
convergence may be scored on the 22:27 text; the numbers below are scored against disk truth as of
22:48, and the pass-3 obligation "re-cut §0 to the probes' actual results" is now a REPEAT directive,
not a first one.**

### 0.2 CROSS-LOCK ANCHORING ERROR — XL-D and XL-G are mis-cited against the binding ledger
The FAMILY-SPECS declares itself bound to "the CROSS-LOCK ledger (XL-A..H)" and cites XL-letters as
settled facts. Two of those citations do not correspond to the ledger's actual content:

| FAMILY-SPECS cite | what the doc uses it for | what CROSS-LOCK.md ACTUALLY adjudicates under that letter |
|---|---|---|
| **XL-D** (lines 140,166,211,342,468,476) | "≥2 IR-modeled competing scan sites; consumer BLOCKED two locks deep (LOCK-9 9C / LOCK-5)" | **XL-D = THE TS EXECUTION FACE** — runtime decoders refuted as fast TS face; the build-time emitter is the fast face. **Nothing about scan sites.** |
| **XL-G** (lines 35,349) | "the 'one solver' coordination; one-solver-home with LOCK-1 1B" | **XL-G = OWNER CONFIRMATIONS PENDING** — value.js 4.0.0 divorce; LOCK-8 symmetric confirmation. **Nothing about solvers.** |

XL-B (tower ownership) and XL-C (regex axis) are cited correctly; XL-D and XL-G are not. The most likely
cause is that the ledger was re-lettered between passes and the synthesis anchors to a superseded
numbering — but the ledger is the authority ("treat them as binding"), and a spec that cites binding
adjudications by a letter that now names a *different* adjudication has an **unverified cross-lock
anchor**. Every load-bearing "consumer BLOCKED two locks deep (XL-D)" and "one-solver-home (XL-G)" claim
inherits that defect. The *substance* under those claims may still hold (scan sites genuinely are not
IR-modeled; the solver genuinely should unify with LOCK-1) — but the citations must be reconciled to the
current ledger before the adjudications can be conveyed as facts. **This is a cross-lock discipline
violation the within-lock critic could not see; it is the distinctive CRIT-2 finding of the pass.**

---

## 1. WHAT THE TWO NEW PROBES ACTUALLY PROVE — and where each PASS is softer than its verdict line

### 1.1 PROTO-2-DISPATCH-CYCB (Brief B, the deciding half §0.2b) — the strongest result of the pass; three residual softnesses
Green and honest: at N=8, dispatch/sequential cyc/B `hi95 < 1.0` in **7/7 CIs** across 5 runs and both
dispatch shapes (match + LUT); at N=2 the direction **reverses** (`lo95 > 1.0`) in 7/7; load-immune ins/B
corroborates the direction at both N (dispatch does FEWER ins at N=8, MORE at N=2). **The N-crossover
tracks real runtime cost — §0.2b (does the pick track cost / does the direction reverse) genuinely
PASSES.** Credit is real and it upgrades every family gated on the deciding half. But:

- **(a) §0.2a is NOT closed — the crossover is bracketed, not fitted.** The probe measured only N=2 and
  N=8; it "did not sweep the exact integer crossover." It establishes `2 < N* ≤ 8`, "consistent with"
  the cost model's `N*=3.0` — but consistency across a factor-of-4 bracket is not confirmation that
  `N*=3` is the *correct* boundary. The true runtime crossover could sit at N=4 or N=5 while the
  hand-authored model fires at N=3, so the model could still discriminate WRONGLY in the N=3..5 band —
  which is exactly the band real grammars populate. The gate-that-cannot-fail concern survives:
  direction at the extremes was never in doubt (PROTO-1 itself called dispatch-beats-sequential-at-8 "a
  well-established microarchitectural fact"); the unresolved question was always the *boundary*, and the
  boundary was not measured.
- **(b) The consumer is still synthetic (§0.2c OPEN, conceded).** The "runnable lowering" is a
  hand-authored microbench (`dispatch_canon_bench.rs`, synthetic N-keyword corpus `apple berry cocoa…`),
  hand-written "to the exact shapes the two codegen paths emit" — NOT a parser the skinny pipeline
  actually generated from a grammar through extraction. The probe's own verdict concedes "§0.2c
  synthetic-consumer remain[s] separate open." The measured fact is "a hand-written dispatch shape beats
  a hand-written sequential shape"; the unmeasured fact is "extraction driving the pipeline picks the
  faster shape and the pipeline emits it." The decision under test is one step removed from the
  production decision.
- **(c) Substrate provenance (CL-5).** The corpus is synthetic; the closest real anchor (json `value`)
  has N=6, unambiguously past any crossover in 2..8 and so not near-boundary evidence. The "fleet"
  (CSS-L4, etc.) numbers the lock keeps invoking still reference grammars that are not `.bbnf` in skinny.

### 1.2 PROTO-2-CHILD-NODE-IN-SEAM (Brief A, the substrate gap §0.3/§V) — buildability PROVEN; the "0→1 live consumer" claim is a masked fallback
Green: a child-bearing `BbnfNode` (`Seq(Vec<Id>)`, `Alt{branches,dispatch}`) authored **purely over
skinny's flat `GrammarIr`** — dragging none of the top-level DAG (`DagNode`/`StringId`/`FnId`/
`AltDispatch`) — compiles in the production crate, saturates to ≥2 survivors at json `value`, runs in
median 19.7 µs, regen-neutral ×2. **This genuinely resolves the organ-separability half of §0.3: a
child-bearing tree-profile node CAN live over skinny's flat arena.** That is the real, decision-grade
lift, and it largely MOOTS the §V challenge routed to GESTALT-B ("is a child-bearing node buildable in
skinny?" — yes). But the report's headline "the honest floor moves **0 → 1**: a genuine child-bearing
island **with a live consumer**" overstates, and the patch (verified on disk) shows why:

- **Consumer-less by construction (viii/Y.13) — the regen-neutrality IS the tell.** The seam wiring is
  `let _organ = probe_child_bearing(grammar, rule.body);` — the report **discards** it (source comment:
  "never feeds `ActiveSelection`, so … regen-neutral"; `let _ = extractor.best_node(root)` — the
  extraction result is thrown away). The node is wired **alongside** the childless `DecisionNode`,
  "observational … rather than swapped in as the shape decider." **A pick that is computed and discarded
  is the definition of consumer-less substrate.** The FAMILY-SPECS's own §0.2c flagged exactly this
  pattern for PROTO-1's synthetic twin ("regen-neutral precisely because it has no live consumer");
  PROTO-2-CHILD-NODE reproduces it and relabels it a live consumer. The honest floor is **0 → 1 for "a
  child-bearing node that compiles and runs observationally in the seam," and 0 → 0 for "a child-bearing
  island whose pick changes emitted code"** — and gate (i) of Family 2A demands the latter.
- **The ≥2 survivors are MANUFACTURED, not intrinsic (2B's refuted thesis unrefuted).** The patch's
  `ManufactureDispatchAlt` "manufacture[s] the missing dispatch/sequential twin … and union[s] it in"
  (`egraph.union(class_id, id)`). The multiplicity is created by the widening rewrite; without it,
  multiplicity is 1 — precisely the triple-confirmed finding the FAMILY-SPECS credits to 2B's refutation.
  "The extraction thesis is ALIVE for skinny" is true only in the sense that *we can always manufacture a
  second candidate to extract over* — never in doubt. It does not restore 2B's distinctive claim (that
  real grammars carry genuine multiplicity).
- **Scale.** The 19.7 µs organ is one 6-branch Alt on json `value`. It is NOT the whole-grammar
  build+saturate+extract+teardown at CSS-L4 scale (2875 DTA states) that 2B gate 4 names as "the single
  most important open measurement" — still never run. "Comfortably inside the ~230 µs budget"
  extrapolates from a single small rule.

**Net of the two probes:** the deciding-direction question and the substrate-buildability question both
advanced from SPEC-ONLY to a genuine green result. What did NOT close: the crossover FIT (§0.2a), the
PRODUCTION consumer (§0.2c — both probes remain synthetic/observational), gate (i)'s live-consumer clause
(the child node's pick is discarded), and the CSS-L4-scale organ. The master gate is **substantially
de-risked but not PASSED**; "extraction-as-decider EARNED" (PROTO-2-DISPATCH-CYCB's verdict line)
overreads by conflating "the direction is real" with "extraction in the pipeline emits the faster shape."

---

## 2. CROSS-LOCK FINDINGS (this critic's weight)

**CL-1 — XL-D / XL-G mis-citation (§0.2).** Binding ledger anchors wrong for two of four cited letters.
Reconcile before any adjudication in this lock is conveyed as fact.

**CL-2 — XL-A over-escalation, now largely mooted by Brief A.** The FAMILY-SPECS §0.3/§V routes to
GESTALT-B, as "the pass's binding blocker," the question "is the substrate's grammar-decision e-node
`DecisionNode` (tree-profile thesis dead) or a child-bearing node (port real, unbuilt)?" PROTO-2-CHILD-
NODE answers the buildability horn empirically: a child-bearing node IS authorable over skinny's flat
arena, no top-level DAG dragged, no XL-A inversion. The GESTALT-B challenge as framed is **stale** — it
asks whether the port is possible; the probe built it. The **surviving** cross-lock question is narrower
and should be re-framed for GESTALT-B: *does skinny PROMOTE the child-bearing node to the shape decider
(with a parity proof that its pick equals `DecisionNode`'s on every shipping rule), or does it stay
observational forever?* Until promoted, XL-A "skinny is the only living implementation" is honored while
skinny's PRODUCTION shape decision remains the childless argmin — the tree-profile extraction thesis has
a substrate home but no throne.

**CL-3 — XL-F item 2 double-homing UNRECONCILED (the pass-3 obligation the FAMILY-SPECS skipped).** The
ledger's XL-F registry item (2): "GN-2.W1's lowering predicate superseded-in-part by LOCK-2 Island-B —
relation (replaces / wraps / waits-on) to be confirmed **pass-3**." The dispatch-vs-sequential Alt
decision BOTH new probes exercise IS a lowering-shape decision — the exact territory GN-2.W1's predicate
governs. Pass-2 closes without confirming the relation, and the probes DEEPEN the overlap (a
child-bearing node now *makes* an Alt-lowering pick in the seam) without naming whether it replaces,
wraps, or waits on GN-2.W1. A live double-homing against the GN codex (constitution ix) the FAMILY-SPECS
should have surfaced and did not. **Owed to GESTALT-B with the relation named.**

**CL-4 — 2C one-solver-home + `bounded_sum` divergent-bank provenance.** The solver-home EARNED result
(6/6) is worktree-only, entered via `ConstraintEnum::Custom`, merged from a divergent bank tree carrying
a `Soft` variant the base lacks. Unification with LOCK-1's csp-solver (LOCK-1 1B centralizes the same
crate) is a real GESTALT-B point — but it is mis-anchored to "XL-G" (CL-1), and the divergent-bank
provenance reconciliation (landing + attribution) is owed BEFORE the two locks can be said to share one
solver home. A cross-lock "one engine" claim resting on an unlanded worktree patch from an unreconciled
bank tree is a consumer-less-substrate risk at the seam.

**CL-5 — the "fleet" is 1 grammar (wrong-tree provenance, XL-H).** json.bbnf is the sole `.bbnf` in
skinny. Every "CSS-L4 12 sites / 2875 states / fleet" number references grammars absent from the
measurement substrate; the two new probes are BOTH on synthetic constructs (a synthetic N-keyword corpus;
the json `value` rule). The solver-shape near-tree / B&B-optimality numbers were taken on top-level or
synthetic instances. The verdicts (B&B over PBQP; direction reversal) are robust; the instance realism
and every "over the fleet" phrase are not substrate-anchored.

**CL-6 — XL-B holds; no encroachment on LOCK-3's seat this pass (the one clean cross-lock posture).** The
FAMILY-SPECS keeps 2B's maximal reading XL-B-refuted and folds extraction into "within LOCK-3's tower at
named points." No unverified gestalt. Preserve it.

---

## Family 2A — EGRAPH-ISLANDS — convergence **56%** (prior CRIT-2 46; disk-truth-as-of-22:48)

Credit: Brief B proves the deciding direction is real (§1.1); Brief A proves a child-bearing node is
buildable over skinny's flat arena (§1.2) — the organ-separability half of the substrate gap the family's
own gate (i) was blocked on. Real, decision-grade lifts.

**Enumerated open gaps:**
1. **[MASTER — gate (i) live consumer, NOT closed] The child-bearing island's pick is discarded.** Seam
   wiring `let _organ = …` (disk-verified); the node runs alongside `DecisionNode` observationally and
   never changes emitted code — regen-neutral *because* consumer-less (viii/Y.13). Gate (i) demands
   "rewrites produce survivors that CHANGE THE PICK under a fixed vector AND that pick moves cyc/B" — the
   second conjunct is now measured (Brief B) but the FIRST is unmet in production: no shipping rule's
   emitted code changes. Honest floor: **1 observational child-bearing node, 0 live-consumer islands.**
2. **[MASTER — §0.2a] The crossover is bracketed (2<N*≤8), not fitted.** N*=3 hand-authored; the runtime
   boundary not swept; the model may discriminate wrongly in the N=3..5 band real grammars populate.
3. **[MASTER — §0.2c] Both probes' consumers are synthetic/observational.** Dispatch bench is a
   hand-written microbench, not a pipeline-generated parser; the child node is discarded. Extraction has
   not yet driven a production emission.
4. **[SUBSTRATE — CL-2] The GESTALT-B challenge is stale; the surviving question is PROMOTION** (child-
   bearing node → shape decider, with a parity proof vs `DecisionNode`). Un-promoted, I1 remains the
   leaf-min gate (i) demotes.
5. **[CL-3, XL-F] The Alt-lowering decision double-homes with GN-2.W1's lowering predicate**; relation
   (replaces/wraps/waits-on) unconfirmed — a pass-3 obligation the ledger set and pass-2 skipped.
6. **[XL-C] I2 (regex-HIR canon) WITHHELD** pending LOCK-5 keep/abrogate of `rules/regex.rs`.
7. **[scan-sites, mis-cited "XL-D"] I3 sites IR-latent, consumer-blocked two locks deep** (LOCK-9 9C /
   LOCK-5); the "XL-D" citation is wrong (CL-1).
8. **[I4] projection canon** probably an always-apply canonicalization (PROVE-12 monomorphized, no
   e-graph) double-homed with LOCK-1's `Project` — unresolved whether an island at all.

Verdict: **ADVANCED — spine + driver EARNED; substrate buildability EARNED; deciding direction EARNED;
the LIVE-CONSUMER island still unearned.** The pass proved the arm reads N, the direction is real, a
child-bearing node fits the arena; it did not prove any shipping rule's emitted code changes.

---

## Family 2B — IR-IS-EGRAPH — convergence **44%** (prior CRIT-2 38)

Credit: context-freedom RATIFIED stands (structural: `admits_sink_only` takes no caller arg, global
`target`); Brief A eases the "substrate childless" gap — a child-bearing node IS buildable in the
surviving tree.

**Enumerated open gaps:**
1. **[distinct existence — UNEARNED] The ≥2 survivors are manufactured (§1.2).** `ManufactureDispatchAlt`
   unions in the twin; multiplicity is 1 without it. 2B's distinctive thesis stays triple-refuted; the
   maximal reading stays XL-B-refuted; the lawful residual IS the 2A island posture. 2B earns nothing
   over 2A.
2. **[gate 4, UNMEASURED] Whole-grammar organ at CSS-L4 scale (2875 states) never ran.** The 19.7 µs is
   one 6-branch json Alt; `CspScheduler` still rebuilds a fresh `Csp` per iteration — a pure-assertion
   scaling risk unaddressed by a single small-rule timing.
3. **[empirical thinness / CL-5] Context-freedom's walk is 1 grammar (json).** Structure carries the
   ratification; "over the fleet" is 1/N.
4. **[total-order] `regen∘regen` through cost-guided extraction at grammar scale** with f64 + content-
   hash tie-break — owed, untested at CSS-L4 scale (per-json byte-identity ≠ grammar-scale determinism).
5. **[consumer, CL-2] The buildable node's consumer is observational.** 2B-in-skinny is a child-bearing
   bag whose extraction is discarded; the thesis has a substrate but no throne.

Verdict: **context-freedom RATIFIED; substrate now buildable; distinct existence still unearned, organ
untested at scale, consumer observational.** BLOCKED-candidate stays lifted; 2B converges into 2A.

---

## Family 2C — CSP-CENTRAL — convergence **50%** (prior CRIT-2 50, unchanged by the two probes)

The lock's strongest positive: solver-shape CLOSED (B&B + `bounded_sum` optimal; PBQP's Lagrangian dual
carries an irremovable integrality gap at ~10⁴× wall-clock). Unaffected by Brief A/B (they concern
extraction/substrate, not the solver). It stands.

**Enumerated open gaps:**
1. **[consumer-blocked, mis-cited "XL-D"] The budget never binds on real IR-modeled sites.** Source says
   ≥2 competing scan sites; the IR says single-valued (`SimdMode`/`SimdSite`); the `SelfLoop`/`Prefilter`
   node shape is LOCK-3's, consumer blocked two locks deep — AND none of those grammars exist as `.bbnf`
   in skinny (CL-5). Teeth remain SPEC-ONLY. The "XL-D" anchor is wrong (CL-1).
2. **[provenance / one-solver-home, CL-4, mis-cited "XL-G"] `bounded_sum` is worktree-only, from a
   divergent bank tree** (`Soft`/`cardinality.rs`), via `ConstraintEnum::Custom`; landing + attribution
   owed. Unification with LOCK-1's csp-solver mis-anchored to XL-G (owner confirmations), not any solver
   adjudication.
3. **[soft/finite coherence penalty] The one residual point for PBQP** (finite coherence preference,
   needs the `Soft` extension) is untested; load-bearing only if finite coherence preferences are real.
4. **[2D-degenerate] Without a fitted table the objective is `static_size_cost`** — "a CSP-shaped
   priority cascade with extra steps." Non-degeneracy leans entirely on 2D (zero fits run).

Verdict: **EARNED-solver / BANKED-teeth.** Solver home is the pass's cleanest earned mechanism; every
tooth is consumer-blocked, 2D-dependent, or resting on unlanded divergent-bank provenance.

---

## Family 2D — PROFILE-CLOSED-LOOP — convergence **33%** (prior CRIT-2 30)

Credit: Brief B hands 2D a REAL, MEASURED front — the direction reversal (dispatch faster at N=8,
sequential at N=2) is now empirical, so a fit finally has a target to reproduce. Slight lift.

**Enumerated open gaps:**
1. **[concrete-or-SPEC-ONLY, vii] Zero executed fit.** No PROVE-2D ran; the fit-harness, freshness-check
   bodies, and a PMU-derived committed `CALIBRATED_WEIGHTS` are all absent. 2D may not speak of
   convergence until one fit lands.
2. **[§0.2a, SHARPER after Brief B] The measured direction reverses in 2<N*≤8, but the integer crossover
   was NOT swept.** 2D's actual job is to FIT the coefficient to the TRUE runtime crossover; Brief B
   proves the reversal EXISTS without locating it, so 2D is now *more* motivated and still un-run —
   `N*=3` remains hand-authored magic sitting inside a factor-of-4 bracket.
3. **[falsifiable freshness] The gate has never gone RED.** Today's `"pass"`/`0` stamps are the theater
   2D removes; until a proto mutates a coefficient and the gate reddens, the anti-theater claim is itself
   provenance theater.
4. **[unblocked plane is cross-lock-owned, ix] `select_backend` kernel crossover (LOCK-9),
   `CSS_SHUFTI_MIN_SPAN`/`GATE=16` (GN-2)** — fitting these supersedes hand-tuned GN literals: a NAMED
   supersession the spec must declare, not attach freely.

Verdict: **BLOCKED (SPEC-ONLY), composable not exclusive** — the front is now MEASURED (Brief B) but
un-fitted; zero executed fit; must ship one fit before it may speak of convergence.

---

## Failure-mode checklist — hits found

- **Stale synthesis / self-supersession — HIT (headline, RECURRENCE).** The 22:27 FAMILY-SPECS's two
  binding-blocker verdicts (§0.2b, §0.3/§V) are both overtaken by disk-resident PASS probes at 22:44 and
  22:48 — the exact sin it invokes against its own predecessor, a second time within the pass.
- **Cross-lock anchoring error — HIT (distinctive CRIT-2 finding).** XL-D and XL-G are cited for
  adjudications the ledger assigns to different letters (TS execution face; owner confirmations). Binding
  anchors unverified against the current ledger.
- **Consumer-less substrate (viii/Y.13) — HIT ×3.** (a) the child-bearing node's extraction is discarded
  (`let _organ = …`, regen-neutral *because* consumer-less); (b) 2C's teeth consumer-blocked two locks
  deep; (c) 2D's fitted coefficients have no live decision to flip.
- **Masked fallback — HIT.** PROTO-2-CHILD-NODE's "honest floor moves 0→1, a live consumer" masks that
  the consumer is observational and the ≥2 is manufactured; buildability was proven, live-consumer was
  not — the claim relabels the former as the latter.
- **Gate-that-cannot-fail — SOFT HIT.** Brief B measured direction at N=2 and N=8, the extremes where the
  outcome was "a well-established microarchitectural fact" (PROTO-1's own words); the boundary — the only
  thing genuinely in doubt (§0.2a) — was not swept.
- **SPEC_ONLY anchoring a perf claim — WATCH/near-HIT.** "Extraction-as-decider EARNED" (Brief B verdict
  line) anchors on a synthetic microbench + a discarded child node; the synthesis must not upgrade
  "direction is real" into "the pipeline emits the faster shape."
- **Double-homing against the GN codex (ix) — HIT.** The Alt-lowering decision double-homes with GN-2.W1's
  lowering predicate; XL-F item 2's pass-3 relation (replaces/wraps/waits-on) unconfirmed.
- **Unverified gestalt — N/A** (XL-B correctly kept WORKING, deferred to GESTALT-B — the one clean posture).

---

## Routing / what pass 3 must carry (cross-lock)

1. **Re-cut §0 to the two 22:44/22:48 probes' actual results — a REPEAT directive.** No score on the
   22:27 text. §0.2b: deciding direction EARNED (synthetic); §0.3/§V: substrate buildability EARNED,
   live-consumer NOT.
2. **Reconcile the XL-D / XL-G citations to the current ledger** (CL-1). Every "consumer BLOCKED (XL-D)"
   and "one-solver-home (XL-G)" claim inherits the mis-anchor until fixed.
3. **Re-frame the GESTALT-B substrate challenge** (CL-2): buildability is answered; the surviving question
   is PROMOTION (child-bearing node → shape decider, with a parity proof vs `DecisionNode`), not port.
4. **Confirm XL-F item 2** (CL-3): name whether Island-B's Alt-lowering decision replaces / wraps /
   waits-on GN-2.W1's lowering predicate — the pass-3 obligation the ledger set.
5. **Land + reconcile `bounded_sum`'s divergent-bank provenance** (CL-4) before any "one solver home"
   claim with LOCK-1.
6. **The remaining hard measurements:** the crossover FIT (§0.2a — sweep the integer boundary), the
   PRODUCTION consumer (§0.2c — extraction driving a pipeline emission), the CSS-L4-scale organ (2B gate
   4), and 2D's first executed fit. None moved to production this pass.

**Net (disk-truth, 22:48):** the pass earned four genuine results — the B&B/`bounded_sum` solver home
(2C), 2B context-freedom (structural), the deciding-DIRECTION (Brief B, synthetic), and substrate
BUILDABILITY (Brief A, observational). It did NOT earn extraction-as-decider in production (synthetic
consumer + discarded child pick + unfitted boundary), and it carries a document **stale against its own
two decisive probes a second time** and **mis-anchored to the cross-lock ledger on two of four cited
letters.** The lock is a converging core with the exact gaps enumerated per family — most sharply the
live-consumer clause of gate (i), the crossover fit, and the GN-2.W1 double-homing — not a convergence.
