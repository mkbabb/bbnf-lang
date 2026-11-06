# LOCK-2 — THE GRAPH ENGINE — pass-2 CRIT-1 (ADVERSARIAL, WITHIN-LOCK)

*Critic 1 of 2. Weight: internal soundness of each family spec + whether the prototypes tested the
sharpest uncertainty. Method: FAMILY-SPECS.md (22:27) + PROTO-1 + both PROTO-2 files + research-{A..D,priorart}
+ PORTFOLIO + CROSS-LOCK read; every load-bearing disk claim re-verified at HEAD this pass. Fable-tier, pass 2.*

Disk re-verified this pass (all CONFIRMED): `skinny/crates/passes/src/backend_egraph.rs:264-265`
`fn cost(&self, node, _child_cost) { node.score }` — childless argmin; skinny's **sole** `impl Language`
is `DecisionNode` (`:181`), childless; `GrammarCostModel`/`GrammarENode` live only in top-level
`crates/{ir,egraph,core}` (grep), absent from skinny; **`bounded_sum` is NOT in
`crates/csp-solver/src/constraint/` at HEAD** (dir holds all_different/dispatch/implication/lambda/
not_equal/… — no `bounded_sum.rs`, no `Soft`); **`skinny/grammars/` holds exactly one file: `json.bbnf`.**

---

## 0. THE PASS-LEVEL FINDINGS (these condition every family score below)

### F1 — THE SYNTHESIS IS STALE AGAINST ITS OWN DECIDING EVIDENCE (process defect, load-bearing)
FAMILY-SPECS.md mtime **22:27**. `PROTO-2-CHILD-NODE-IN-SEAM.md` **22:44** and
`PROTO-2-DISPATCH-CYCB.md` **22:48** BOTH post-date it. The synthesis's two central verdicts —
§0.2b "the deciding PMU half is UNMEASURED … went unrun" and §0.3 "the port … was renamed 'done,' it
did not build it" — were written **before** the two probes that claim to answer exactly those two
questions. **This is the identical defect the synthesis charges the 19:25 draft with in its own
header** ("it synthesized the earlier prototype pair … then those briefs RAN … without the synthesis
being updated to their results"). The document must be RE-CUT a third time against the 22:44/22:48
probes before any convergence talk. Every §0 verdict citing "unmeasured/unbuilt" is now
evidence-overtaken and must be re-adjudicated, not carried forward verbatim. **Ceiling effect: no
family may claim a number premised on a §0 verdict the synthesis itself has not reconciled with the
newest disk truth.**

### F2 — THE DECIDING HALF (§0.2b) GENUINELY ADVANCED — but only for a UNIFORM hit distribution (elegant-reduction residue)
`PROTO-2-DISPATCH-CYCB` is the pass's most honest measurement: runnable hand-authored lowerings, paired
cold `ri_cycles`, DISPATCH cyc/B 95% CI `hi95<1.0` at N=8 in **7/7** CIs and SEQUENTIAL faster at N=2
(`lo95>1.0`) in **7/7**, load-immune ins/B corroborating direction at both N (4.67 vs 6.32 @ N=8;
4.67 vs 4.25 @ N=2). The direction reversal is real; PROTO-1's "no PMU on darwin-ARM" excuse is refuted
(the counter was always `proc_pid_rusage`). **This closes the DIRECTION question.** But the sharpest
uncertainty it did NOT test is the one the cost model actually rides on: the corpus is **round-robin
uniform over `0..N`**, giving SEQUENTIAL mean trial depth `(N+1)/2` — the arithmetic best case for
dispatch at large N. The shipping cost arm is `dispatch_table + N·dispatch_branch` vs `N·alt_per_branch`
— **a function of N ALONE, blind to hit distribution.** Real cost is distribution-dependent: a grammar
whose first alt matches 95 % of value-starts favours SEQUENTIAL even at N=20. **"The pick tracks real
cost" is proven only for uniform hits;** for skewed hits the N-only model can mis-decide and the probe's
uniform corpus makes the win *arithmetically guaranteed* (gate-that-cannot-fail flavour). §0.2a
(crossover *location* fitted, not just bracketed 2<N*≤8) therefore remains fully open, and it is
distribution — not N — that the fit must capture.

### F3 — THE SUBSTRATE PROBE PROVED CAPABILITY, NOT CONSUMPTION — the consumer-less floor is UNCHANGED (viii/Y.13)
`PROTO-2-CHILD-NODE` earns a genuine result: a child-bearing `BbnfNode` CAN be authored purely over the
flat `GrammarIr` arena (no top-level DAG dragged), saturates to ≥2 survivors at the real json `value`
6-branch Alt, runs the organ in median 19.7 µs, regen-neutral. Organ-separability resolves for
separability — a real lift, and it retires the "flat arena can't host a tree-profile node" fear. **But
its own wiring refutes its top-line "the honest floor moves 0 → 1 … a live consumer wired into the
production decision seam."** Verified on the patch: the seam call is `let _organ =
…probe_child_bearing(grammar, rule.body);` (**discarded**), and inside the organ the extraction is
`let _ = extractor.best_node(root);` (**the pick itself is discarded**). The organ "never feeds
`ActiveSelection`," which is *why* it is regen-neutral. By the constitution's own definition
(consumer-less substrate forbidden, viii/Y.13) a discarded `let _` is not a live consumer — it is the
canonical consumer-less substrate. The probe's honest "Scope honesty" paragraph concedes exactly this:
the node "reasons over grammar structure" while `DecisionNode` "decides which backend shape" — **they
answer different questions, so the organ runs ALONGSIDE, not in place of, `DecisionNode`.** §0.3 asked
for a child-bearing node **replacing** the childless one at the seam skinny calls, **changing emitted
code**; the probe delivered a parallel observer that changes nothing. **The honest floor "0 genuine
child-bearing islands with a live consumer that moves emitted code" is UNCHANGED.** The residual — a
production rewrite that manufactures a real ≥2-survivor choice, USES the pick, and proves parity to
`DecisionNode` on every shipping rule — is the same unbuilt core, now with its substrate obstruction
cleared. Warning for the re-cut: adopting the probe's "0→1" verbatim would launder a consumer-less
substrate into an EARNED — the exact laundering the synthesis (predating) correctly refused.

### F4 — 2C's "EARNED-solver" RESTS ON A PRIMITIVE ABSENT FROM THE SUBSTRATE (unverified-at-HEAD)
`bounded_sum` is **not in `crates/csp-solver/src/constraint/` at HEAD** (verified). The 6/6 green and
the whole B&B-native-knapsack verdict live in a divergent bank worktree carrying a `Soft`
variant/`soft.rs`/`cardinality.rs` the base lacks, entered via `ConstraintEnum::Custom`. The synthesis
is honest that landing is "owed" — but it still stamps 2C **EARNED-solver**. An EARNED label on a
solver whose load-bearing propagator cannot be built or re-run on the measurement substrate (XL-A: skinny
is the only living implementation) is generous. "Solver-shape CLOSED" is a *design* verdict from a
worktree; on the substrate the constraint does not exist. Provenance reconciliation + landing is a
convergence PREREQUISITE, not a footnote.

### F5 — "OVER THE FLEET" IS 1/1 (XL-A wrong-tree flag, standing)
`skinny/grammars/` = `{json.bbnf}`, sole. Every css-l4 (2875 DTA states), google_sheets, css/pretty,
bbnf, csv, math, ebnf number in the specs is a **top-level (legacy, GN-0-archival) artifact**, not on
the living substrate. Consequences the specs already flag but which cap scores: 2B context-freedom
"corroborated over the fleet" is **1 grammar**; 2C's "css/pretty=15, css/l4≥10 self-loop sites" are all
in a tree skinny does not compile; the whole-grammar-organ-at-CSS-L4-scale gate (2B gate 4, "the single
most important open 2B measurement") **cannot be run on the substrate at all** until those grammars exist
as skinny `.bbnf`. Ratification-by-structure is sound; ratification-by-fleet-empirics is 1/N and must
say so at every use.

---

## Family 2A — EGRAPH-ISLANDS · ADVANCED · **56%** (was CRIT-1 52; +4 net)

The lock's surviving spine, and after F2/F3 the two of three master-gate sub-questions that advanced this
pass both live here: the deciding half MEASURED (F2, direction only), the substrate capability PROVEN
(F3, consumer-less). The lift is real but bounded by the consumer floor and the distribution-blindness.

**What is now sound (credit):** I1 ships wired + consumer-attached (leaf-min, correctly demoted by gate
(i)); the widening/narrowing template is confluent-and-terminating for the shipping set (PROTO-1);
`BbnfNode` proves a child-bearing organ is authorable over the flat arena in-budget (PROTO-2-CHILD-NODE);
the pick's DIRECTION tracks cyc/B for uniform hits (PROTO-2-DISPATCH-CYCB). Gate (i) can genuinely go
RED. NOT BLOCKED-candidate.

**Enumerated open gaps:**
1. **[CONSUMER, viii/Y.13 — the crux] 0 genuine child-bearing islands whose pick MOVES emitted code.**
   Both probes left the honest floor at 0 (F3). I1 is a leaf-min; PROTO-2-CHILD-NODE discards
   (`let _organ`, `let _ = best_node`). Needed: a production rewrite that manufactures a real ≥2-survivor
   choice, USES the extraction pick, and proves parity to `DecisionNode` on every shipping rule. Until
   then the distinctive extraction value (I2/I4 tree profiles) is SPEC-ONLY.
2. **[§0.2a — DISTRIBUTION, not N] The crossover LOCATION is unfitted and the model is distribution-blind
   (F2).** `dispatch_table + N·dispatch_branch` reads N alone; real cost depends on alt-hit distribution;
   the deciding probe used a uniform corpus (dispatch's best case). N*=3.0 is an arithmetic artifact of
   the two slopes, never validated against a skewed corpus. A gate that fires only on uniform hits is a
   gate that cannot fail on its own test corpus.
3. **[F1] The 2A verdict text cites §0.2b "unmeasured" and §0.3 "not built" — both evidence-overtaken.**
   The score cannot be re-affirmed until the family text is re-cut against the 22:44/22:48 probes.
4. **[XL-C] I2 regex-HIR canon WITHHELD** — LOCK-5 keep/abrogate of `rules/regex.rs` unresolved.
5. **[XL-D] I3 scan-plan: ≥2 IR-modeled competing sites** — consumer BLOCKED two locks deep; none exist
   as skinny `.bbnf` (F5).
6. **[XL-F] I4 projection canon: island or always-apply canonicalization** double-homed with LOCK-1's
   `Project` — unresolved; PROVE-12 monomorphized suggests plain rewrite.
7. **Confluence is per-rewrite × per-narrowing-set, not general** — proven only against the current
   shipping set (`DeduplicateAltBranches` did 0 work because it keys on branch-set, blind to the dispatch
   dimension). A future narrowing rewrite could break confluence; no general guarantee.

## Family 2B — IR-IS-EGRAPH · BLOCKED-candidate LIFTED → folds into 2A · **44%** (was CRIT-1 46; −2)

Down two, not up: PROTO-2-CHILD-NODE's "multiplicity ≥2 at json value" is a genuine corroboration that
the extraction thesis is *alive for skinny* (2A-I2/I4 are not canonicalizations-forever) — a point in
2B's favour. But it simultaneously confirms 2B's terminal ceiling: the surviving node **is** 2A's island
node (same `BbnfNode`, same crossover arm), so 2B still **earns no distinct existence over 2A**, and the
"multiplicity ≥2" it showed is at a *discarded* consumer (F3). The strongest honest 2B — "IR-as-e-graph
for canon + candidate generation, B&B extraction for the budget-coupled slice" — is verbatim 2A's
island+CSP spine (research-B §6 boundary: greedy extraction can't honour the sharing-sensitive budget).

**Sound this pass:** context-freedom RATIFIED on the STRUCTURAL argument (`admits_sink_only` takes no
caller arg; global `target`; real walk 0 conflicts) — this is 2B's one genuine lift and it survives F5
(structure, not fleet-empirics). The pass-1 NP-hard BLOCKED-candidate objection stays lifted.

**Enumerated open gaps:**
1. **No distinct existence over 2A** — the maximal "extraction IS the decision driver" is XL-B-refuted
   (LOCK-3's seat); the lawful residual is the 2A posture. 2B is a mechanism-span choice
   (persistent-in-place vs build-teardown) inside 2A, not a family.
2. **[F1/F3] Its distinctive deliverable — a child-bearing e-node with a LIVE consumer in the surviving
   tree — is still unbuilt.** PROTO-2-CHILD-NODE authored the node but discarded its pick; the specs'
   §0.3 "renamed 'done,' not built" is *more* right than the probe's "0→1", not less.
3. **[gate 4, unrunnable on substrate] Whole-grammar organ (build+saturate+extract+teardown) within
   ~230 µs at CSS-L4 scale (2875 states)** — never run, and CANNOT be run on skinny (json only, F5).
   `CspScheduler` rebuilds a fresh `Csp` per iteration (opposite the aegraph no-rebuild lesson) — a
   pure-assertion scaling risk at the exact scale the substrate cannot exercise.
4. **`regen∘regen` through COST-GUIDED extraction is unproven.** `GrammarCostModel` returns `f64` with a
   Vec tie-break; the total-order fix is owed and untested at grammar scale. PROTO-2-CHILD-NODE's
   `check-json ×2` proves regen neutrality of a *discarded* organ (byte-identical *because* it changes
   nothing), NOT determinism of a cost-guided extraction that changes output.
5. **Multiplicity ≈1 with the shipping rule set** (triple-confirmed) — so "IR IS an e-graph end-to-end"
   buys only the dedup hash-consing already gives, absent the manufactured dispatch twin.

## Family 2C — CSP-CENTRAL · EARNED-solver(qualified) / BANKED-teeth · **52%** (was CRIT-1 54; −2)

Down two: the solver-shape design verdict (B&B + `bounded_sum` beats PBQP; PBQP's Lagrangian dual carries
an irremovable integrality gap at ~10⁴× wall-clock) is the pass's cleanest *design* result and I do not
contest its logic. But F4 forces the qualification: **the propagator does not exist at HEAD.** An
"EARNED-solver" whose primitive cannot be built on the measurement substrate is EARNED-in-a-worktree, not
EARNED-on-the-substrate. The 2-point downgrade is the provenance/landing debt, not a logic fault.

**Enumerated open gaps:**
1. **[F4 — landing] `bounded_sum` absent from `crates/csp-solver/src/constraint/` at HEAD.** The 6/6 and
   the whole knapsack-native verdict live in a divergent bank tree (`Soft`/`soft.rs`/`cardinality.rs`).
   Landing + provenance reconciliation is a convergence PREREQUISITE, not a footnote.
2. **[XL-D — teeth, consumer-blocked, F5] ≥2 IR-modeled competing scan sites with REAL binding costs.**
   Source says yes (css/pretty=15, css/l4≥10, bbnf=2, csv=2); the IR says no (`SimdMode`/`SimdSite`
   single-valued); the `SelfLoop`/`Prefilter` node shape is LOCK-3's, derivation consumer BLOCKED two
   locks deep — **and none of those grammars are skinny `.bbnf`.** Teeth are SPEC-ONLY on the substrate.
3. **Degenerate without 2D** — absent a fitted table the objective is `static_size_cost`, i.e. "a
   CSP-shaped priority cascade with extra steps." The non-degeneracy of the objective is a 2D dependency
   that 2D has not discharged (zero executed fit).
4. **Soft/finite coherence penalty untested** — the one residual point where PBQP's pairwise matrix could
   still win; unrepresentable in base B&B without the (absent) `Soft` extension; load-bearing only if
   finite coherence preferences are real (unknown).
5. **[XL-G] One-solver-home with LOCK-1 1B** — `bounded_sum` extends the same csp-solver LOCK-1
   centralizes; a GESTALT-B unification point contingent on F4's provenance reconciliation landing first.

## Family 2D — PROFILE-CLOSED-LOOP · BLOCKED (SPEC-ONLY), composable · **32%** (was CRIT-1 34; −2)

Sound discipline (AutoFDO/BOLT precedent, calibrate-time-not-regen-time determinism invariant, ~70%
substrate present-but-inert). PROTO-2-DISPATCH-CYCB hands 2D its first real cyc/B cost-fact (the N=2/8
direction) — a hair of forward motion. But **still zero executed FIT**: no coefficient fitted from PMU,
no decision flipped by a fitted value. The master-gate dependency the synthesis names BINDING is *worse*
than it reads: PROTO-1's manufactured front is in the wrong engine from unfitted coefficients, and
PROTO-2-CHILD-NODE's real-seam front DISCARDS its pick (F3) — so 2D still has no live ≥2-survivor
decision with a consuming reader to fit against. The −2 is the F1 evidence-staleness plus the confirmation
that the consumer 2D must co-land against does not yet exist.

**Enumerated open gaps:**
1. **[MASTER, BINDING] Zero executed fit.** 2D must ship ONE fit that FLIPS a real decision AND beats the
   hardcoded lane cold-paired on a real grammar. The decision to fit against does not exist (F3); the
   only cyc/B evidence (PROTO-2-DISPATCH-CYCB) is on hand-authored lowerings, not an extraction pick.
2. **[F2] What must be FITTED is distribution, not N.** The cost model is distribution-blind; a fit that
   only recovers N*=3.0 from a uniform corpus fits the arithmetic, not the cost. The fit's real content
   is the hit-distribution dependence no probe has surfaced.
3. **Falsifiable freshness unexercised** — today's `check-calibration` `"pass"`/`0` stamps are theater
   until a proto reddens the gate (mutate a coefficient → RED). The anti-theater claim is itself
   provenance theater until then (the specs concede this; it stands).
4. **The only UNBLOCKED plane is cross-lock-owned** — `select_backend` kernel crossover (LOCK-9),
   `CSS_SHUFTI_MIN_SPAN`/`GATE=16` (GN-2). Fitting these supersedes hand-tuned GN literals — a **named
   supersession (ix)** the spec must DECLARE, not attach freely.
5. **Attribution ceiling undisclosed** — A/B-differential is the sound method, but the ceiling = count of
   independently-toggleable levers with a paired bench; that count is never stated. Disclose it.

---

## BOTTOM LINE (within-lock)

The pass produced two real advances the synthesis has not yet metabolized (F1): the deciding half of the
master gate is MEASURED for uniform hits (PROTO-2-DISPATCH-CYCB, methodologically clean), and a
child-bearing organ is provably authorable over skinny's flat arena in-budget (PROTO-2-CHILD-NODE). Both
matter. Neither closes the lock's binding hole: **there is still zero genuine child-bearing island whose
extraction pick MOVES emitted code** — I1 is a leaf-min, PROTO-2-CHILD-NODE discards its own pick
(`let _organ` / `let _ = best_node`), and the honest consumer-less floor is UNCHANGED by both probes.
Two new sharp uncertainties surfaced that no probe tested: the cost model is **distribution-blind** while
its one runtime validation used dispatch's best-case uniform corpus (F2), and 2C's EARNED-solver rests on
a **`bounded_sum` primitive absent from HEAD** (F4). The single most consequential action for pass 3 is
NOT another gate — it is the **REPLACING-not-observing** consumer: a production rewrite that manufactures
a real multi-survivor choice at the seam skinny calls, USES the pick, changes emitted code, and proves
parity — the one artifact both probes stepped around. Convergence: 2A 56 · 2B 44 · 2C 52 · 2D 32. One
surviving frame (2A's directed-spine-with-islands, 2C-Island-B folded in, 2D beneath), gated on a live
consumer that neither probe supplied and a distribution-aware fit no probe attempted.
