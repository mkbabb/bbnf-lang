# LOCK-2 — THE GRAPH ENGINE — pass-2 FAMILY SPECS (synthesis, RE-CUT against PROTO-1/PROTO-2 + both on-disk critiques)

*e-graph + CSP optimization: HIR canon, IR rewriting, kernel/backend selection, capacity &
schedule decisions, profile-fed cost, deterministic regen. Four families — 2A EGRAPH-ISLANDS ·
2B IR-IS-EGRAPH · 2C CSP-CENTRAL · 2D PROFILE-CLOSED-LOOP — one cogent spec each while routes
remain incompatible, then the incompatibility map. Synthesis over research-{A,B,C,D,priorart}
(pass-2), the solver-shape probe (B&B-vs-PBQP, RAN), PROTO-1 (PROVE-2-ALTCOST-CROSSOVER, RAN),
PROTO-2 (PROVE-2-SUBSTRATE-RECONCILE, RAN), the two on-disk critiques CRIT-1 (within-lock) +
CRIT-2 (cross-lock), the pass-1 registry, and the CROSS-LOCK ledger (XL-A..H). Fable-tier, pass 2.*

**This document RE-CUTS the 19:25 draft, which both critics flagged as STALE — it synthesized the
earlier prototype pair, emitted Brief 1 + Brief 2, and then those briefs RAN as `PROTO-1.md`
(19:46) and `PROTO-2.md` (19:47) without the synthesis being updated to their results.** Every §0
verdict below reflects disk truth verified this synthesis pass (skinny path-deps `../crates/egraph`;
`skinny/crates/ir/src = {cost.rs, lib.rs}`; `DecisionCostModel::cost` returns `node.score`, ignores
`_child_cost` — childless argmin; `GrammarENode`/`GrammarCostModel` live only top-level).

Constitution binds: consumer-less substrate forbidden (viii/Y.13); type plane costs zero at
runtime (v); concrete artifacts or SPEC-ONLY (vii); refutation laws — no warm benches, ratios
certified / absolutes directional (iv); deterministic regen (`regen∘regen = regen`); extant GN/PT
codex is the baseline, divergences are NAMED supersessions (ix).

---

## 0. What pass-2 actually resolved — three probes ran; one closed, one is half-open, one entrenched the substrate gap

### 0.1 SOLVER SHAPE — CLOSED (a genuine, earned convergence; unaffected by the two later probes)
On the fleet's scan-plan decision graphs, the near-tree premise is CONFIRMED: every real instance is
a forest (treewidth ≤ 2); PBQP's R0/R1/R2 reductions collapse it fully (RN=0, ~1–5 µs). But **B&B +
`bounded_sum` wins outright**: it carries the global size budget natively and exactly as knapsack
bound-consistency, optimal on every instance (3–35 nodes, <100 µs, matches brute force), while PBQP
**cannot encode `Σcost ≤ B` as a pairwise cost** and its Lagrangian budget-dual carries an
irremovable integrality gap (css-l4 slack obj 39.003 vs true 21.011) at ~10⁴× wall-clock. **Verdict:
the scan-plan / Island-B solver home = the existing B&B + `bounded_sum`; PBQP is ceremony.** This
closes Q6 and the XL-G "one solver" coordination in favour of B&B-native-knapsack. The `bounded_sum`
propagator is built and tested (`cargo test -p csp-solver --test bounded_sum` = 6/6). Two riders:
(a) it is **worktree-only, not at HEAD** — merged from a divergent bank tree carrying
`soft.rs`/`cardinality.rs`/a `Soft` variant the base lacks, entered via the base
`ConstraintEnum::Custom` seam; landing + provenance reconciliation is owed. (b) The one residual
point for PBQP — a *soft/finite* coherence-preference penalty (native to PBQP's pairwise matrix,
unrepresentable in base B&B) — is untested; only load-bearing if finite coherence preferences turn
out real.

### 0.2 THE MASTER GATE (extraction-as-decider) — HALF-OPEN: structural precondition EARNED, deciding PMU half UNMEASURED
PROTO-1 built the piece pass-1 left unbuilt and its **structural half is green and deterministic**: a
single fixed, unmodified `CALIBRATED_WEIGHTS` vector picks 2-branch→SEQUENTIAL and 8-branch→DISPATCH,
crossing at `N* = dispatch_table/(alt_per_branch−dispatch_branch) = 3.0`, driven by the shipping
calibrated coefficients and **not** the forbidden `dispatch_bonus` sign. PROTO-1's own sign-only
refutation is thereby refuted; the fix was the documented two-line rewire of the `Alt` cost arm to
coefficients that already ship (`cost_weights.rs`). **Credit is real: the cost arm now discriminates
the two `Alt` forms on branch-count N — exactly the structural precondition 2A/2B/2D were blocked
on.** But the gate does not PASS, and three defects keep "structural crossover" from becoming
"extraction-as-decider EARNED":

- **(a) The crossover is arithmetic, not evidence (gate-that-cannot-fail).** Two lines with slopes
  `dispatch_branch=0.5 < alt_per_branch=1.5` and intercept `dispatch_table=3.0` cross *necessarily*
  at `N*=3.0`. The probe proves the arm *reads* N; it proves nothing about whether `N*=3` is the
  **correct** boundary. The coefficients are hand-authored and never fitted (2D never ran). The model
  discriminates — possibly wrongly. "Discriminates" ≠ "discriminates truthfully."
- **(b) The deciding PMU half is UNMEASURED.** The gate's second clause — the two lowerings' cyc/B
  95% CI excludes 1.00× same-direction at value-starts — went unrun. PROTO-1 cited a nonexistent
  `css_canon_bench` target, "no scriptable PMU on darwin-ARM," loadavg 7–17, and "no synthetic
  EGraph→runnable-parser lowering." *The whole point of extraction-as-decider is to pick the faster
  lowering; whether DISPATCH is actually faster at N=8 in this pipeline is exactly the unmeasured
  clause.* Classic elegant-reduction: the easy half (model reads N) is done; the hard half (does the
  reading track real cost?) is untouched. (Note for pass 3: the "no PMU" claim is over-stated — the
  canon benches obtain cyc/B via per-process `ri_cycles` / `proc_pid_rusage(RUSAGE_INFO_V5)`, which
  needs no `perf`/`instruments`; `json_canon_bench` exists. The real blocker is the runnable
  lowering, not the counter. See Brief B.)
- **(c) Consumer-less in production (viii/Y.13).** PROTO-1 records that `ManufactureDispatchAlt`
  "appears nowhere in `crates/*/src` outside its own definition file"; production extraction has no
  `None`/`Some` choice point, which is *why* the rewire is regen-neutral. A cost term that
  discriminates a synthetic-only twin is a consumer-less substrate until a production rewrite
  manufactures the choice.

**Honest label: structural precondition EARNED (green test); extraction-as-decider still SPEC-ONLY —
its crossover location unfitted, its runtime unmeasured, its consumer synthetic.** Neither refuted
nor converged.

### 0.3 THE SUBSTRATE GAP — re-diagnosed on disk, SHARPER than "three trees" and NOT dissolved by PROTO-2
The stale draft framed a "three-tree split" and its §0.3 asserted skinny "has no engine at all."
**Disk refutes that framing and reveals a sharper, more damaging shape** (CRIT-2, re-verified this
pass at HEAD):

| fact | disk truth (verified) |
|---|---|
| skinny engine dep | `skinny/Cargo.toml:36-37`: `egraph = {path="../crates/egraph"}`, `csp-solver = {path="../crates/csp-solver"}` — the **generic PRIMITIVES crates are the TOP-LEVEL ones, path-referenced UP**; `skinny/crates/egraph/` does **not** exist. The engine primitives *are* shared cleanly (parsimony OK there). |
| skinny grammar e-node | sole `impl Language` = `DecisionNode`; `DecisionCostModel::cost(&self, node, _child_cost) { node.score }` (`backend_egraph.rs:264-265`) — **ignores child cost, returns a precomputed scalar = a childless argmin over 2–5 candidates.** |
| child-bearing e-node | `GrammarENode`/`GrammarCostModel` (the tree-profile, child-bearing node PROTO-1's crossover operates on) live **only** in top-level `crates/ir/src/egraph/{node,cost}.rs`; **absent from skinny** (grep empty). |
| the two probes | **PROTO-1 proved the N-crossover in `GrammarCostModel`** (top-level) — a cost model **skinny's decision seam never invokes.** **PROTO-2 accepted childless `DecisionNode`** as "skinny's substrate-native GrammarENode-analogue" and declared the port "not needed." |

**The precise contradiction is not "three trees" — it is TWO DIFFERENT decision cost models in two
trees, and the two probes proved things in OPPOSITE ones.** The generic `egraph` primitives crate is
cleanly shared; the DUPLICATION is at the **grammar-e-node layer — the very layer the lock's
distinctive extraction thesis lives in.** PROTO-2's "no escalation warranted" is over-read: by
blessing childless `DecisionNode` as canonical it **entrenches** that skinny's production decision
seam is exactly the `argmin` over childless candidates that Family 2A's own gate (i) demotes to "not
a genuine island." **The honest floor — "0 genuine child-bearing islands with a live consumer in the
substrate" — is UNCHANGED by both probes**, because PROTO-1's child-bearing crossover is in a tree
skinny does not call and PROTO-2 blessed the childless one. The "adopt-and-prove port" (wire a
tree-profile child-bearing node into the seam skinny *actually calls*, replacing childless
`DecisionNode`) is **still the unbuilt core, and still unscoped by any family** — PROTO-2 renamed the
gap "done," it did not build it.

> **Challenge routed to GESTALT-B (CROSS-LOCK, with evidence).** The lock carries TWO
> grammar-decision e-node types doing overlapping work — `GrammarENode` (top-level, tree-profile,
> child-bearing) and `DecisionNode` (skinny, childless argmin). XL-A ("skinny is the only living
> implementation") cannot be honored while skinny's decision seam runs a childless argmin and the
> tree-profile engine sits unreferenced in the "legacy" tree. GESTALT-B must adjudicate: **is the
> substrate's grammar-decision e-node `DecisionNode`** (then the tree-profile extraction thesis is
> dead and I2/I4 are canonicalizations forever) **or a child-bearing tree-profile node** (then the
> port is real, unbuilt, and owed a scoping family)? The generic primitives crate is shared and needs
> no escalation; only the grammar-e-node duplication does.

### 0.4 CONTEXT-FREEDOM — RATIFIED (the pass's second clean earned result)
PROTO-2's real conflict walk over the skinny fleet returned **0 conflicts, non-vacuously**
(`admits_sink_conflict_walk`: 15 rules, 28 CallRule edges, 5 multi-caller rules, `pair`/`string` live
sink-only witnesses, `conflicts=0`). The structural root is sound and disk-verified:
`admits_sink_only(rule_ir, target)` (`passes/src/lib.rs:958`) takes **no caller argument**, and
`target` is one global `TargetFeatures::host()` threaded through the per-rule loop — so a rule's
sink-only admissibility is provably caller-invariant. **2B context-freedom is RATIFIED on the
structural argument, corroborated empirically.** The pass-1 NP-hard context-sensitive-DAG-extraction
BLOCKED-candidate objection stays **lifted** (any conflict's repair would be bounded sub-rule
specialization, not optimal context-sensitive extraction — and there is no conflict). Caveat: the
empirical walk is 1 grammar (`json.bbnf` is the sole skinny `.bbnf`); ratification rests on structure,
"over the fleet" is 1/N.

### 0.5 The net — EARNED / SPEC-ONLY / BLOCKED
- **EARNED (concrete artifacts):** (i) the canonicalization spine + the wired `BackendShape` driver
  (I1 ships in skinny, per-rule, consumer-attached — but a childless leaf-min); (ii) the
  confluent-terminating **widening-rewrite template** (`ManufactureDispatchAlt`, green); (iii) the
  **N-crossover structural precondition** (PROTO-1, green, single fixed vector); (iv) the **scan-plan
  solver home** = B&B + `bounded_sum` (solver-shape probe; `6/6`, worktree-only); (v) **2B
  context-freedom** (PROTO-2, RATIFIED).
- **SPEC-ONLY / half-open:** extraction/CSP-extraction **as a decider** — the deciding PMU half is
  unmeasured (§0.2b), the crossover unfitted (§0.2a), the production consumer synthetic (§0.2c), and
  the green half lives in a cost model the substrate seam does not invoke (§0.3).
- **BLOCKED / consumer-less:** 2C's *teeth* (a binding budget on real costs over IR-modeled sites
  read by a real emitter) — `SimdSite::SelfLoop`/`Prefilter` node shape is LOCK-3's, its derivation
  consumer BLOCKED two locks deep (LOCK-9 9C / LOCK-5, XL-D); 2D's *fit* — no live ≥2-survivor
  decision exists to fit until the master gate lands one at the real seam.
- **The substrate gap (§0.3) gates the in-substrate re-fire of the master gate** and is the
  highest-value pass-3 prerequisite (both critics concur).

---

## Family 2A — EGRAPH-ISLANDS · ADVANCED · ~46% (critic floor; CRIT-1 52 / CRIT-2 46)

**Architecture.** An LLVM-shaped **directed pass pipeline** with a small named set of e-graph
**islands** at decision points *inside* LOCK-3's tower (XL-B: within, at named points). Each island
is a canonicalizing organ — `build → saturate(budgeted) → extract(total-order) → teardown` — reading
a committed cost table and feeding **exactly one** same-wave consumer that changes emitted code. Above
the islands sits a thin **selection layer**: a CSP over extracted candidates resolving the few
cross-site couplings a local island cannot see (the shared generated-size budget; pairwise coherence
between adjacent scan sites). **Islands GENERATE; the CSP SELECTS**, and post-solver-probe that CSP is
**B&B + `bounded_sum`**. Post-XL-B this is also the lawful home of 2B's surviving mechanism (§I).

**Mechanism — two island profiles, four candidate sites.** A **leaf profile** (no scheduler loop;
direct total-order min) for childless decision nodes; a **tree profile** (saturate + bottom-up
extract) for child-bearing nodes where a subterm rewrites many ways.

| island | node kind | consumer (same-wave) | home | status (post-proto) |
|---|---|---|---|---|
| **I1 backend-shape** (extant) | childless leaf | `codegen::lower::rust::lower_to_rust` | LOCK-2 skinny seam | **wired, but a leaf-min — by gate (i) demote to a plain cost-ranked pass; NOT a genuine island** |
| **I2 regex-HIR canon** | child-bearing | engine-tier NFA/prefilter (LOCK-5) | **parse-that / LOCK-5 (XL-C)** | WITHHELD; awaits LOCK-5 lawful `Hir` + keep/abrogate of `rules/regex.rs` |
| **I3 scan-plan** | leaf-per-site + budget edge | `lower/recognizer/` → `find_member`/`dfa.find` | LOCK-2 selection / **LOCK-3 node shape (XL-D)** | solver home EARNED; sites IR-latent, consumer BLOCKED two locks deep |
| **I4 projection canon** | child-bearing | `schema_direct`/tape emitter | LOCK-2, **double-homed LOCK-1 (XL-F)** | probably always-apply canonicalization (PROVE-12 monomorphized, no e-graph) → plain rewrite by gate (i) |

I1 is real (`backend_egraph.rs:60`, `EGraph<DecisionNode>` over 2–5 childless candidates, one
narrowing rewrite, `Extractor::best_node`, emits `ActiveCostFacts` for the named consumer) — but
`DecisionCostModel::cost` returns `node.score` ignoring child cost (disk-confirmed `:264`), and the
sibling `decision_csp.rs` re-solves a 1-var CSP pinning the already-chosen index. **By gate (i), I1 is
not a genuine island; the count of child-bearing extraction islands with a live consumer in the
substrate is 0, and BOTH probes left it at 0** (§0.3).

**Widening + narrowing coexist without a correctness confluence proof** (rewrites are unions, never
replacements; saturation keeps both forms; termination bounded by `node_limit`/`iter_limit`;
semantics a per-rule oracle). PROTO-1 confirmed this operationally for `ManufactureDispatchAlt`
(confluent with the shipping all-narrowing set — `DeduplicateAltBranches` did 0 work, keys on the
branch set, blind to the dispatch dimension; terminates `iters=2/64, nodes=20/100000`). But
"confluent with the current set" is proven; "confluent in general" is not — the guarantee is
per-rewrite × per-narrowing-set.

**Integration seams (path:line).** Shared engine primitives (top-level, path-dep'd UP from skinny) =
`crates/egraph/src/{egraph,extract,scheduler,csp_scheduler,cost_weights}.rs`; the tree-profile
grammar e-node = `crates/ir/src/egraph/{node,cost,rules,write_back,analysis}.rs` (**top-level only**);
skinny decision seams = `skinny/crates/passes/src/backend_egraph.rs` (I1, childless `DecisionNode`),
`decision_csp.rs` (selection layer; needs `bounded_sum` merged into
`crates/csp-solver/src/constraint/`), `skinny/crates/ir/src/lib.rs`
(`Recognizer`/`SimdMode`/`SimdSite`), produced at `passes/src/lib.rs:338` (`derive_recognizers`),
lowering home `codegen/src/lower/recognizer/`. Cost tables `egraph/src/cost_weights.rs`
(`CALIBRATED_WEIGHTS`); per-decision facts `ir/src/cost.rs`. **The child-bearing node home and the
skinny decision seam are in different trees (§0.3) — the unscoped port.**

**Gates it carries.** (i) **island-earns-the-e-graph** (RED-capable, and PROTO-1 exercised its
structural half): an island's rewrites produce child-bearing survivors that *change the pick under a
fixed calibrated vector* AND that pick moves cyc/B at value-starts — else demote to a plain
cost-ranked pass. (ii) **total-order tie-break**: last cost coordinate a stable content hash (extant
`DecisionCost` ends `candidate_hash: FNV-1a(id)`); negative control (drop the hash) must go
non-deterministic. (iii) **teardown**: `with_capacity(occupancy)`. (iv) GN-2.W1 born-RED: ≥8× cyc/B on
the wired skip site, ≥0.95× dense, ≥3 cold paired PMU windows, ins/B load-invariant, structural parity
before timing. (v) deletion of the ceremonial ~68-field provenance apparatus once a plan-reading
consumer exists (unexercised — no reader).

**Exact open questions.** (1) **[MASTER — deciding half]** Does the N-crossover pick track *real*
cost — is DISPATCH actually faster at large N in a runnable lowering, cyc/B CI excluding 1.00× at
value-starts (Brief B)? Until measured, extraction-as-decider is SPEC-ONLY however green the
structural half. (2) **[SUBSTRATE, §0.3]** Does a child-bearing tree-profile node have a home in the
seam skinny *actually calls*, replacing childless `DecisionNode`, within budget + regen (Brief A)?
Until it does, the crossover is proven in a cost model the substrate never invokes. (3) **[XL-C]**
LOCK-5 keep-or-abrogate of `rules/regex.rs` (I2 WITHHELD). (4) **[XL-D]** ≥2 IR-modeled competing scan
sites (LOCK-3 owes the node shape; consumer BLOCKED two locks deep). (5) **[XL-F]** Is I4 an island or
an always-apply canonicalization double-homed with LOCK-1's `Project`@`passes/src/lib.rs:204`?

**Verdict. ADVANCED — the lock's spine and the lawful home of 2B's surviving mechanism after XL-B.**
Gate (i) can genuinely go RED and PROTO-1 exercised its structural half honestly. But the
*distinctive* extraction value (I2/I4 tree profiles) is **SPEC-ONLY straddling**: the widening rewrite
fires and is confluent, the cost arm now reads N, but (a) that reading is unvalidated/unfitted, (b)
the deciding cyc/B clause is unmeasured, and (c) the green half is in `GrammarCostModel`, which
skinny's decision seam does not call. **Honest floor: 0 genuine child-bearing islands with a live
consumer in the substrate; I1 is a leaf-min gate (i) demotes.** NOT BLOCKED-candidate — the missing
pieces (a runnable-lowering PMU measurement; a child-bearing skinny node) are named and buildable, not
intractable primitives.

---

## Family 2B — IR-IS-EGRAPH · BLOCKED-candidate LIFTED → converging into 2A · ~38% (critic floor; CRIT-1 46 / CRIT-2 38)

**Architecture (maximal reading — CROSS-LOCK-REFUTED by XL-B).** The middle IR *is* an e-graph end to
end; passes *are* rewrite groups; a single cost-guided extraction emits per-rule `BackendShape`
directly with `derive_backend_shape` deleted. **This makes extraction the decision driver — the seat
XL-B assigns to LOCK-3.** An undeclared supersession of LOCK-3's charter; not LOCK-2's to instantiate.

**Architecture (lawful residual under XL-B).** The e-graph is the **in-place child-bearing node
substrate at named decision points within LOCK-3's tower** — acyclic (recursion a symbolic
`Ref`/`CallRule` leaf resolved by a per-rule root table, so greedy elaboration terminates in one
reverse-topo sweep — the aegraph move), rules on-insert, exploratory saturation only where it provably
pays, global cost handed to the one constraint store. **This residual is operationally the 2A island
posture** (Cranelift's own convergence: levels-as-graphs, avg class 1.13, no saturate-then-extract
fixpoint). The remaining difference is *span* — persistent in-place substrate vs discrete
build-teardown islands — a mechanism choice inside 2A's frame, decided by the organ-budget/teardown
measurement, not an ownership war.

**Mechanism — real / refuted / ratified.** REAL and mostly built: `BackendExpr` (`ir/src/lib.rs:376`)
is the recursive sum `egraph_derive` projects to `Id`; the AC-3 scheduler, the `Lattice` extractor,
and the widening-rewrite template all exist; PROTO-1 added a confluent, terminating
`ManufactureDispatchAlt`. REFUTED-in-part: the distinctive thesis — multiplicity ≈ 1 across the fleet
with today's rules (triple-confirmed), so "IR IS an e-graph end-to-end" buys only the dedup a hash-cons
already gives; and the shipping cost model could not discriminate even a manufactured child-bearing
twin until PROTO-1's rewire, which lives in the wrong tree (§0.3). RATIFIED: **context-freedom**
(§0.4) — `admits_sink_only` takes no caller arg; global `target`; real walk = 0 conflicts,
non-vacuous. **The pass-1 NP-hard BLOCKED-candidate condition is LIFTED** (repair, if ever needed, is
bounded sub-rule specialization; no conflict exists).

**The hard architectural boundary (research-B §6, load-bearing).** Greedy bottom-up extraction "is not
optimal for cost models that depend on sharing" (`extract.rs:9-11`); the generated-size budget is a
global, sharing-sensitive cost. So "extraction emits `BackendShape` directly" **cannot honour the
budget through the current extractor.** 2B's two lawful completions both dissolve its independence:
(1) replace greedy extraction with a solver extraction for the budget slice — *extraction becomes a
solver, converging with 2C*; (2) hand the budget to the one constraint store — *the CSP decides the
coupled slice*. The solver-shape probe settled which solver: **B&B + `bounded_sum`**. The strongest
honest 2B is "IR-as-e-graph for canonicalization + candidate generation, with a B&B extraction for the
budget-coupled final selection" — which *is* 2A's island+CSP spine.

**Integration seams.** `egraph-derive/src/lib.rs` (project recursive positions of a child-bearing
skinny node, or hand-author `Language for BackendExpr`); `crates/egraph/src/analysis.rs`
(nullable/first-set as `Analysis` lattices — skinny's `ir/src/lib.rs` nullability fixpoint is already
this shape); `crates/egraph/src/extract.rs` (cyclic guard + the solver-extraction seam);
`skinny/crates/passes/src/backend_egraph.rs` (the extant `lower_to_rust` consumer). The doc-claim
"grammar IR and regex HIR implement `Language`" is aspirational — skinny's sole `impl Language` is
childless `DecisionNode`.

**Gates it carries.** (1) AC operators normal-formed **one-directionally** (flatten `Seq`, sort/dedup
`Alt` by first-set), never exploratory re-associate; confluence a prototype obligation. (2)
e-node/e-class count + iterations bounded on all live grammars. (3) `regen∘regen` byte-identical
through cost-guided extraction (content-hash tie-break; `GrammarCostModel` returns `f64` with Vec
tie-break — the total-order fix is owed and untested at grammar scale; PROTO-2's `check-json ×2` proves
regen neutrality only of a TEST-ONLY patch, not cost-guided-extraction determinism). (4)
**whole-grammar organ (build+saturate+extract+teardown) within ~230 µs** on CSS-L4 (2875 DTA states) —
*the single most important open 2B measurement, never run*; `CspScheduler` rebuilds a fresh `Csp` per
iteration (opposite of aegraph's no-rebuild lesson) — a pure-assertion scaling risk.

**Exact open questions.** (1) **[MASTER, Brief B]** does extraction pick the *faster* lowering, not
merely a *different* one? (2) whole-grammar organ within budget incl. teardown at CSS-L4 scale? (3)
**[SUBSTRATE, Brief A]** the port — a child-bearing skinny node at the seam skinny calls — was
*conceded* by PROTO-2 (childless `DecisionNode` blessed), not built; who scopes it?

**Verdict. BLOCKED-candidate LIFTED → converging into 2A.** The maximal reading is XL-B-refuted; the
lawful residual is the 2A island posture; the distinctive value is triple-refuted (multiplicity ≈ 1)
and further SPEC-ONLY-straddled by the master gate; the budget boundary forces a B&B extraction,
dissolving independence into 2A's spine. Context-freedom RATIFIED is the pass's genuine 2B lift. But
2B still **earns no distinct existence over 2A**, and its real deliverable — a child-bearing e-node in
the surviving tree — was renamed "done," not built (§0.3).

---

## Family 2C — CSP-CENTRAL · EARNED-solver / BANKED-teeth · ~50% (critic floor; CRIT-1 54 / CRIT-2 50)

**Architecture (as-primacy RETIRED; as-Island-B EARNED-solver).** The maximal reading — "the decision
driver IS one global COP over the whole grammar" — is RETIRED (Unison ceiling: CP proven-optimal only
to ~946–1000 instr/function; CSS-L4 = 2875 DTA states is off the cliff). What survives and is LIVE:
**the scan-plan CSP** — per-site lowering choices as variables, legality + the generated-size budget
as constraints, a cost lattice as objective, e-graphs demoted to pure candidate generators. **This is
the same seam as 2A-I3, and the solver-shape probe earned its home.**

**Mechanism (the coupled sub-CSP).** Variables: one per lowering site (backend shape per rule;
scan/prefilter tier per self-loop site; capacity plan per container; residualization depth per
delimiter scan), domain = the island's extracted candidates (≤5). Constraints: legality (the four
`decision_csp.rs` lambdas become genuine domain filters, not tautologies over a pinned variable);
**coherence** (binary equality edges from `SameSubstrateUnionFacts` — the "few edges" keeping the graph
near-tree, probe-confirmed treewidth ≤ 2); **the generated-size budget as a global `bounded_sum`
knapsack** (`Σ static_size_cost ≤ loc_budget`). Objective: `MinimizeCost` via the existing B&B, per-value
cost fed by `Measurement.cycles_per_byte_x1000` — replacing the HEAD hardcoded `perf_cost` (2D's
territory).

**Solver shape (Q6 — CLOSED).** B&B + `bounded_sum` optimal on every near-tree instance (3–35 nodes,
<100 µs); PBQP fully reduces but its Lagrangian budget-dual carries an irremovable integrality gap at
~10⁴× wall-clock. **Route B (B&B-native-knapsack) is the home; Route A (PBQP) is ceremony.** The
propagator is `6/6` — but **worktree-only, from a divergent bank tree** (`soft.rs`/`cardinality.rs`/a
`Soft` variant the base lacks), dropped in via `ConstraintEnum::Custom`; **landing + provenance
reconciliation is owed** (§0.1). Residual PBQP point: soft/finite coherence penalty unrepresentable in
base B&B (needs the `Soft` extension) — untested; load-bearing only if finite coherence preferences
are real.

**Selection-equivalence with the cascade** (conservative extension): the priority cascade never
optimized the budget (a post-hoc lint that "blocks the wave"), so folding it in as a constraint cannot
lose a decision the cascade made — it reproduces the per-site optimum exactly in the budget-slack
regime and *corrects* the cascade only in the budget-tight regime the cascade is blind to.

**Integration seams.** `passes/src/lib.rs:407` (`derive_backend_shape_with_diagnostics`),
`passes/src/backend_egraph.rs`, `passes/src/decision_csp.rs` (replace the rubber-stamp 1-var CSP);
`crates/csp-solver/src/{lib.rs, constraint/ (bounded_sum to LAND), solver/optimize.rs,
solver/search.rs, domain/cost_finite.rs}`; `ir/src/cost.rs` (`DecisionCspFacts`, `PriorityStep`,
`Measurement`, `SameSubstrateUnionFacts`); budget = `Cargo.toml` `loc_budget`.

**Gates it carries.** (1) a budget-binding grammar: the CSP forces `Exact` on ≥1
legal-but-unaffordable site under a tight budget AND emits a byte-identical plan under slack. (2)
`regen∘regen` on the global solve: variable order by stable `RuleId`, value order by the
`candidate_hash` total order. (3) solve cost within budget (probe: <100 µs, comfortable). (4) the
objective consumes ≥1 real cost fact (2D dependency).

**Exact open questions.** (1) **[XL-D, consumer-blocked]** ≥2 *IR-modeled* competing scan sites with
*real binding* costs so the budget binds? Source says yes (css/pretty=15, css/l4≥10, bbnf=2, csv=2
self-loops); the IR says no (`SimdMode`/`SimdSite` single-valued); the `SelfLoop`/`Prefilter` node
shape is LOCK-3's, derivation consumer BLOCKED two locks deep, **and none of those grammars exist as
`.bbnf` in skinny (json is the only one)** — a standing wrong-tree flag on every "fleet" number. (2)
Is soft/finite coherence penalty load-bearing? (3) Degenerate without 2D: absent a fitted table the
objective is `static_size_cost`, "a CSP-shaped priority cascade with extra steps." (4) one-solver-home
with LOCK-1 1B (XL-G): `bounded_sum` extends the same csp-solver LOCK-1 centralizes — a GESTALT-B
unification point; the divergent-bank provenance must reconcile first.

**Verdict. EARNED-solver / BANKED-teeth.** The solver-shape question is CLOSED (B&B-native-knapsack) —
the pass's cleanest earned mechanism. NOT BLOCKED-candidate — the missing primitive is *built*
(worktree). It is blocked on a **consumer** (IR-modeled sites read by a real emitter) no single lock
owns end-to-end, on 2D for a non-degenerate objective, and on landing/reconciling `bounded_sum`. Teeth
remain SPEC-ONLY.

---

## Family 2D — PROFILE-CLOSED-LOOP · BLOCKED (SPEC-ONLY), composable · ~30% (critic floor; CRIT-1 34 / CRIT-2 30)

**Architecture.** The cost model is **measured, not authored**: coefficients fitted offline from cold
paired PMU captures, committed as a content-addressed const fact table, read (never re-measured) by the
decision solver at regen. Three phases — **FIT** (offline `xtask fit-cost`, A/B-differential, one lever
per paired arm — the only sound attribution when whole-grammar cyc/B underdetermines per-feature
coefficients), **COMMIT** (human-gated; write `CALIBRATED_WEIGHTS`, bump `cost_formula_version`),
**FRESHNESS GATE** (CI, born-RED; drift → RED). **The load-bearing determinism invariant:** the loop
closes at *calibrate-time, not regen-time* — regen is a pure function of `(grammar, committed-β)`, so
`regen∘regen = regen` survives; all load noise lives in `xtask fit-cost`, off the regen path. Exactly
the AutoFDO/BOLT/PGO discipline with a stale-profile gate (LLVM `getFunctionHash` → drop-on-mismatch is
the direct precedent).

**Mechanism.** ~70% substrate-present, all inert. `ActiveCostFacts` carries `cost_formula_version`,
`selected_cost_freshness`, `determinism_replay_status`, `rewrite_order_variance_pct`,
`selection_trace_hash`; `Measurement` stores cyc/B as **u64 fixed-point** (×1000 — the float-tie
determinism fence); the total-order tie-break is already correct (`candidate_hash` FNV-1a suffix);
`decision_csp.rs:183 static_profile_provider_status: "static-template-blocker"` is the field 2D
replaces; `passes/src/lib.rs:590 perf_cost: 0` + `:589 stale: false` are literally the two hardcoded
fields the fit fills. The env-fence is nearly clean (`from_env` is bench-only; regen builds
`CostConfig::default()`); a CI grep-gate forbidding `from_env` on any regen path closes it hard. The
residual consumer is proven (PROVE-12: const-fold at generation time, +18 gen LOC, cyc/B 0.81–0.98×,
regen bit-for-bit). Missing 30%: the fit-harness body, the freshness-check body, and — critically — **a
live decision for a coefficient to flip.**

**Integration seams.** `ir/src/cost.rs` (`ActiveCostFacts`, `Measurement`, `EvidenceSource`);
`egraph/src/cost_weights.rs` (`CALIBRATED_WEIGHTS` slot; the 54-config null guard `:134-167`);
`passes/src/backend_egraph.rs`/`lib.rs:589-590` (`perf_cost`/`stale`); `decision_csp.rs:183`; `xtask`
(`fit-cost`/`check-calibration`); the freshness gate in `report.rs`.

**Gates it carries.** (1) a fitted value FLIPS a decision AND the fitted lane beats the hardcoded lane
cold-paired on a real grammar. (2) **ins/B is the load-invariant parity check, cyc/B is the certified
objective** (PROVE-02/09 law: an ins/B win that does not carry to cyc/B is rejected — the digit-kernel
refutation is why cyc/B, not ins/B, is the objective). (3) `regen∘regen` byte-identical; `from_env`
grep-gate GREEN. (4) **falsifiable freshness**: mutate a coefficient → the gate goes RED,
`check-calibration` fails (today's `"pass"`/`0` stamps are the theater 2D removes — until a proto
reddens the gate, the anti-theater claim is itself provenance theater). (5) aarch64-only; a flip must
survive on a held-out grammar; on `source_hash` mismatch reset `perf_cost` to the neutral default so a
stale table degrades to the static cascade rather than driving on rotten coefficients (R4).

**Exact open questions.** (1) **[MASTER, BINDING]** the multiplicity-1 wall: a fitted coefficient
cannot flip a decision that has no discriminable alternatives. PROTO-1 hands 2D the *path* (fit
`dispatch_table`/`dispatch_branch` at a branch-count crossover) but does not walk it — and the front it
manufactured is in the wrong engine (§0.3) from unfitted coefficients (the very thing 2D exists to
replace). 2D must **co-land with the first decision that has ≥2 real survivors at the real seam**
(Brief A) and the first runnable cyc/B measurement (Brief B). (2) Attribution underdetermination —
resolved structurally by A/B-differential; ceiling = count of independently-toggleable levers with a
paired bench (disclose it). (3) The only *unblocked* plane is cross-lock-owned: `select_backend` kernel
crossover (LOCK-9), `CSS_SHUFTI_MIN_SPAN`/`GATE=16` (GN-2 scanner knobs) — fitting these supersedes
hand-tuned GN literals, a **named supersession (ix)** the spec must declare, not attach freely.

**Verdict. BLOCKED (SPEC-ONLY) — composable, not exclusive.** Sound discipline, correct precedent,
right determinism invariant; **zero executed fit**. Its master-gate dependency resolved *at best
partially adverse* this pass (a front exists only in a probe, in the wrong engine, from unfitted
coefficients). A composable coefficient layer UNDER whichever generator wins. NOT strictly
BLOCKED-candidate — the blocker (a live ≥2-survivor decision) is a named, buildable change, not an
intractable primitive — but 2D must ship ONE executed fit before it may speak of convergence.

---

## The incompatibility map — which families cannot coexist, and precisely why (pass-2, post-proto)

Pass 1 drew a primary fork {2A,2B}, a primacy conflict {2B,2C}, a scope conflict {2A,2C}, one
composable layer {2D}. The cross-lock adjudications collapsed the hard forks into sub-questions of one
frame; the probes resolve the internal ones and leave two empirical gates + one substrate contradiction.

### I. 2A ⊗ 2B — DOWNGRADED to mechanism-difference-inside-2A (XL-B), residual now empirical
XL-B forecloses 2B's maximal reading (extraction cannot be the decision driver — LOCK-3's seat). 2B's
lawful residual (acyclic in-place e-node substrate, on-insert rewrite, B&B extraction for the budget
slice) is operationally the 2A island posture — Cranelift's own convergence. What remains is a
**mechanism choice inside 2A's frame** — persistent-in-place substrate vs rebuilt-and-torn-down at
chokepoints — decided by the organ-budget/teardown measurement (2B gate 4), an empirical question.
**No longer the lock's primary fork.**

### II. 2B ⊗ 2C — DISSOLVED; the solver-shape probe named the meeting point's solver
Both routes reach the same seam: 2B's budget boundary forces extraction to terminate in a solver for
the sharing-sensitive slice; 2C demotes extraction to candidate generation and makes the CSP decide.
**They meet at the `bounded_sum` knapsack over the near-tree scan-plan graph — and the probe settled
the solver: B&B, not PBQP.** No live primacy conflict remains.

### III. 2A ~ 2C — CONVERGED to identity at Island B
2C-as-primacy is RETIRED (Unison); its surviving form *is* 2A's Island B — same variables, same
`bounded_sum`, same (now-resolved) B&B solver. The same route under two names; the coupled core is tiny
(3–35 B&B nodes).

### IV. 2D — composable with the winner (NOT exclusive), prerequisite + blocked plane
2D is the coefficient plane *underneath* the candidate generator, orthogonal to *who decides*.
Constraints are dependency, not exclusion: BLOCKED at the shape lattice (the master gate recurs),
unblocked only at knobs another lock owns (named supersession), requiring the winning generator to have
manufactured ≥2 discriminable survivors first at the real seam.

### V. The substrate gap — a cross-lock incompatibility, not intra-family (NEW, load-bearing)
The generic `egraph` primitives crate is cleanly shared (skinny path-deps `../crates/egraph`), but the
**grammar-decision e-node is DUPLICATED and divergent**: `GrammarENode` (top-level, child-bearing,
tree-profile) vs `DecisionNode` (skinny, childless argmin). The two probes proved the master gate and
the port **in opposite engines** (§0.3); the honest floor of genuine child-bearing islands with a live
substrate consumer stayed 0 under both. Every family's distinctive value needs a child-bearing node in
the seam skinny *actually calls*; no family scopes authoring it. **This is the pass's binding blocker
and a challenge routed to GESTALT-B** — which grammar-e-node is the substrate's, `DecisionNode`
(tree-profile thesis dead) or a child-bearing node (port real, unbuilt, owed a scoping family)?

### The surviving shared gates (bind all four)
Every family's distinctive value — 2A's I2/I4 tree profiles, 2B's extraction, 2C's non-degenerate
objective, 2D's lattice attachment — is gated on **the master gate's TWO unresolved halves**: (a) a
child-bearing node in the seam skinny actually calls (§0.3 / Brief A), and (b) the pick tracking real
cost — cyc/B CI excluding 1.00× at value-starts in a runnable lowering (§0.2b / Brief B). PROTO-1
proved only that the arm *reads* N, in a tree skinny does not call. The scan-plan/2C axis carries a
second, orthogonal gate — ≥2 IR-modeled competing sites with binding costs (consumer BLOCKED two locks
deep, XL-D). All negative results are decision-grade.

**Net pass-2 shape.** One surviving frame — **2A's directed-spine-with-islands**, into which 2B's
lawful mechanism folds and 2C's Island B (solver home EARNED = B&B + `bounded_sum`) is the scan-plan
seam — with **2D the composable coefficient layer beneath it**. The lock is one architecture with **two
earned results (the B&B/`bounded_sum` solver home; 2B context-freedom)**, **one green structural
half-result (the N-crossover, in the wrong engine)**, **one half-open crux (extraction-as-decider,
pending a child-bearing skinny node + a runnable-lowering cyc/B measurement)**, **one consumer-blocked
axis (IR-modeled scan sites, XL-D)**, and **one binding substrate contradiction (the duplicated
grammar-e-node layer, §0.3/§V)** that GESTALT-B must adjudicate.

---

## Prototype briefs (the two sharpest uncertainties for pass 3) — see StructuredOutput
Brief A resolves the substrate gap (§0.3/§V) — a child-bearing node at the seam skinny actually calls;
Brief B resolves the deciding half (§0.2b) — does the pick track real cost, measured via the existing
`ri_cycles` harness that PROTO-1 wrongly declared unavailable. Together they close the master gate that
gates every family, and neither depends on a cross-lock cession.
