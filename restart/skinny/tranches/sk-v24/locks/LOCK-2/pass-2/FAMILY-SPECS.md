# LOCK-2 — THE GRAPH ENGINE — pass-2 FAMILY SPECS (synthesis, post-prototype + post-CRIT-1)

*e-graph + CSP optimization: HIR canon, IR rewriting, kernel/backend selection, capacity &
schedule decisions, profile-fed cost, deterministic regen. Four families — 2A EGRAPH-ISLANDS ·
2B IR-IS-EGRAPH · 2C CSP-CENTRAL · 2D PROFILE-CLOSED-LOOP — one cogent spec each while routes
remain incompatible, then the incompatibility map. Synthesis over research-{A,B,C,D,priorart}
(pass-2), PROTO-1 (RAN, master gate), PROTO-2 (RAN, MEASURED, decisive), the on-disk CRIT-1, the
pass-1 registry, and the CROSS-LOCK adjudications (XL-A..H). Fable-tier, pass 2. This document
supersedes the 13:10 draft, which was written BEFORE its own two decisive prototypes ran.*

Constitution binds: consumer-less substrate forbidden (viii/Y.13); type plane costs zero at
runtime (v); concrete artifacts or SPEC-ONLY (vii); refutation laws — no warm benches, ratios
certified / absolutes directional (iv); deterministic regen (`regen∘regen = regen`); extant GN/PT
codex is the baseline, divergences are NAMED supersessions (ix).

---

## 0. What pass-2 actually resolved — two prototypes ran, and they split the lock cleanly

Pass 1 left the lock with two master gates and a re-located primary fork (LOCK-2⊗LOCK-3). Two
prototypes ran this pass. **One closed decisively; one returned a straddle that CRIT-1 shows is
softer than a refutation; and both exposed a substrate contradiction that no family scopes.**

### 0.1 PROTO-2 — the solver-shape question is CLOSED (a genuine, earned convergence)
On the fleet's **actual** scan-plan graphs (JSON 3 sites; CSS-L4 12 sites, coherence a forest),
the near-tree premise is **CONFIRMED**: every real instance is a forest (treewidth ≤ 2), R0/R1/R2
reductions collapse it fully (**RN=0, zero fallbacks, ~1–5 µs**); only a synthetic K4 clique
(treewidth 3) forces `RN=1`. But **B&B + `bounded_sum` wins outright**: it carries the global size
budget *natively and exactly* as knapsack bound-consistency, is optimal on all five instances
(matches brute force in 3–35 nodes / <100 µs), while **PBQP must dualize the budget** — a
`Σcost ≤ B` knapsack has no pairwise encoding — and the Lagrangian dual carries an **irremovable
integrality gap** (css-l4 slack: obj 39.003 vs true 21.011; the knapsack optimum is off the
Lagrangian path) at **~10⁴× the wall-clock**. **Verdict: 2C/Island-B solver home = the existing
B&B + `bounded_sum`; PBQP is ceremony.** This closes 2C's Q6 and the GESTALT-B "one solver"
coordination (XL-G) in favour of **B&B-native-knapsack over PBQP-plug-in-propagator**. The one
residual point for PBQP — a *soft/finite* coherence preference penalty (native to PBQP's pairwise
matrix, unrepresentable in the base B&B whose `ConstraintEnum` lacks the `Soft` variant) — is
untested and is the only follow-up in PBQP's favour; it does not move the verdict because the
*budget*, not coherence, is the coupling that stresses the solver.

### 0.2 PROTO-1 — the master gate returned FAIL, but the honest label is SPEC-ONLY STRADDLING, not "refuted"
PROTO-1 built the piece pass-1 left unbuilt and it is **green**: a *registered*
`ManufactureDispatchAlt` `Rewrite`, fired through `CspScheduler` (not hand-`union`ed), manufactures
**≥2 semantically-distinct survivors at a child-bearing `Alt`**, stays **confluent** with the
shipping all-narrowing set (`DeduplicateAltBranches` ran every iteration, did 0 work, did not
collapse the dispatch twin — it keys on the branch set, blind to the dispatch table), and
**terminates** with wide margin (`iters=2/64, nodes=20/100000`). **This refutes §0's pass-1 claim
that the substrate is *inert*: it is not — it manufactures confluent alternatives.**

The FAIL is located *entirely* at **extraction under calibration**, and its cause is **named and
cheap**: `GrammarCostModel::cost`'s `Alt` arm is `base [+ dispatch_bonus if Some]`, where `base` is
identical across the twin (same children, same N) → `cost(Some) − cost(None) ≡ dispatch_bonus`, a
scalar **constant in N**. Only the *sign* of one knob flips the pick; no interior of the calibrated
space does. **The honest statement is "today's cost model cannot discriminate the twin," NOT
"extraction is architecturally inert."** The load-bearing fix — net `dispatch_table` (fixed setup)
against `N·dispatch_branch` vs `N·alt_per_branch` (a **branch-count-dependent** term) so a *single
fixed* calibrated vector picks DISPATCH at large N and SEQUENTIAL at small N — is cheap, named by
the probe itself, and **untested**. So extraction-as-decider is neither refuted nor converged: it
is **SPEC-ONLY straddling**. The confluence result is also narrow: it holds because
`DeduplicateAltBranches` is blind to the dispatch dimension; "confluent with the current set" is
proven, "confluent in general" is not.

### 0.3 The substrate contradiction — the two-tree/three-tree split that no family scopes (the pass's load-bearing blocker)
Verified on disk at base-pin `ce51c36fc` (CRIT-1 §0.2): **no single tree holds both the engine and
the decision seams.**

| | top-level `crates/` (XL-A: *legacy*) | `skinny/crates/` (XL-A: *substrate*) |
|---|---|---|
| `egraph/` engine — `GrammarENode:Language`, `GrammarCostModel`, `rules/`, `write_back.rs` | **EXISTS**, production-wired (`core/pipeline.rs:218`) | **ABSENT** (skinny `ir` = `cost.rs`+`lib.rs`) |
| `passes/` seams — `backend_egraph.rs`, `decision_csp.rs`, `admits_sink_only`, `SimdMode`/`SimdSite` | **ABSENT** | **EXISTS** |
| `bounded_sum` propagator | absent from base | absent from base — lives only in a **third divergent bank tree** carrying `soft.rs`/`cardinality.rs`/a `Soft` variant the base lacks |

Three decision-grade consequences: (1) **PROTO-1 ran the master gate on the top-level engine**
(`cargo test -p bbnf-ir`; `bbnf-ir = crates/ir = legacy`), not the skinny substrate its own brief
required — the sharpest uncertainty was tested, for the second consecutive pass, on the tree
missing half the seams. (2) **PROTO-1's context-conflict rider is a wrong-tree artifact**: it
grepped `crates/` for `admits_sink_only`, found 0, and declared "context-free confirmed" — but the
predicate lives in `skinny/crates/passes`. **2B's context-freedom is UNSETTLED, not confirmed** —
the real caller-conflict walk was never run. (3) **"Adopt-and-prove / port the engine into skinny"
is the actual unbuilt core, and NO family scopes it.** The 13:10 draft asserted XL-A "dissolves the
port-direction question… there is no port between trees" — **disk refutes that**: the engine is in
one tree, the seams in another, so "prove the organ on skinny" is *not presently executable*.

> **Challenge routed to GESTALT-B (CROSS-LOCK protocol, with evidence).** XL-A declares
> "skinny@HEAD is the only living implementation; legacy `crates/` archives at GN-0," yet on disk
> the top-level workspace is the compiling, production-wired tree that *owns the e-graph engine*,
> while `skinny/crates/` is a separate workspace (unreferenced by root) with *no engine at all*.
> Either (a) both prototypes inherit a wrong-tree defect, or (b) XL-A's legacy/substrate labels are
> inverted relative to disk. Both are damaging; both must resolve before either verdict is
> decision-grade. Until then LOCK-2's substrate is three tree-states (engine, seams, `bounded_sum`)
> and the lock cannot claim a coherent home (parsimony vi).

### 0.4 The net — what is EARNED, what is SPEC-ONLY, what is BLOCKED
- **EARNED (concrete artifacts):** (i) the canonicalization spine + the existing `BackendShape`
  driver (I1 ships, per-rule, consumer-attached); (ii) the confluent-terminating **widening rewrite
  template** (PROTO-1 egraph limb, green); (iii) the **scan-plan solver home** = B&B +
  `bounded_sum`, PBQP refuted as ceremony (PROTO-2, `6/6` regression floor held).
- **SPEC-ONLY straddling:** extraction/CSP-extraction **as a decider** — the widening rewrite fires
  and is confluent, but the shipping cost model cannot discriminate its survivors under calibration;
  the branch-count-dependent fix is named and untested (Brief 1).
- **BLOCKED / consumer-less:** 2C's *teeth* (a binding budget on real costs over IR-modeled sites
  read by a real consumer) — the `SimdSite::SelfLoop`/`Prefilter` node shape is LOCK-3's, its
  derivation consumer BLOCKED two locks deep (LOCK-9 9C / LOCK-5, XL-D); 2D's *fit* — no ≥2-survivor
  decision exists to fit until Brief 1 manufactures one.
- **The substrate contradiction (§0.3) gates the in-substrate re-fire of the master gate** and is
  the highest-value pass-3 prerequisite.

---

## Family 2A — EGRAPH-ISLANDS

**Architecture.** An LLVM-shaped **directed pass pipeline** with a small named set of e-graph
**islands** planted at decision points *inside* LOCK-3's tower (XL-B: within, at named points).
Each island is a canonicalizing organ — `build → saturate(budgeted) → extract(total-order) →
teardown` — reading a committed cost fact-table and feeding **exactly one** same-wave consumer that
changes emitted code. Above the islands sits a thin **selection layer**: a CSP over the extracted
candidates resolving the few cross-site couplings a local island cannot see (the shared
generated-size budget; pairwise coherence between adjacent scan sites). **Islands GENERATE; the CSP
SELECTS.** Post-XL-B this is also the lawful home of 2B's surviving mechanism (§I). Post-PROTO-2 the
selection layer's solver is settled: **B&B + `bounded_sum`**.

**Mechanism — two island profiles, four candidate sites.** A **leaf profile** (no scheduler loop;
direct total-order min) for childless decision nodes; a **tree profile** (saturate + bottom-up
extract) for child-bearing nodes where a subterm rewrites many ways.

| island | node kind | profile | consumer (same-wave) | home | status (post-proto) |
|---|---|---|---|---|---|
| **I1 backend-shape** (extant) | childless leaf | leaf | `codegen::lower::rust::lower_to_rust` | LOCK-2 (skinny seams) | **EARNED, but a leaf-min — by gate (i) demote to a plain cost-ranked pass, not counted an island** |
| **I2 regex-HIR canon** | child-bearing | tree | engine-tier NFA/prefilter build | **parse-that / LOCK-5 (XL-C)** | WITHHELD cross-lock; awaits LOCK-5 lawful `Hir` + keep/abrogate of `rules/regex.rs` |
| **I3 scan-plan** | leaf-per-site + budget edge | leaf + B&B/`bounded_sum` | `lower/recognizer/` → `find_member`/`dfa.find` | LOCK-2 selection / **LOCK-3 node shape (XL-D)** | solver home EARNED; sites IR-latent, consumer BLOCKED two locks deep |
| **I4 projection canon** | child-bearing | tree | `schema_direct` / tape emitter | LOCK-2, **double-homed LOCK-1 (XL-F)** | probably an always-apply canonicalization (PROVE-12 monomorphized, no e-graph) → plain rewrite by gate (i) |

I1 is real today (`passes/src/backend_egraph.rs:60`, `EGraph<DecisionNode>` over 2–5 **childless**
candidates, `with_capacity(ranked.len())`, one narrowing rewrite, extract cheapest `:84-88`, emits
`ActiveCostFacts` for the named consumer `:11`) — but it is `argmin` over a candidate list with no
congruence sharing, and the sibling `decision_csp.rs` re-solves a 1-variable CSP pinning the
already-selected index (`:44-47`) — two subsystems, one decision. **By gate (i), I1 is not a genuine
island; the count of child-bearing extraction islands with a live consumer in the substrate is 0.**

**Widening + narrowing coexist without a confluence proof** *for correctness* (rewrites are unions,
never replacements; saturation keeps both forms; termination bounded operationally by
`node_limit`/`iter_limit`; semantics preservation a per-rule oracle — PROVE-12 discharges the
delimiter-mask equivalence). **PROTO-1 confirmed this operationally** for `ManufactureDispatchAlt`.
But the master gate needs more than coexistence-for-correctness: it needs the widening rewrite's
survivor to *change the pick*, and PROTO-1 showed the current `GrammarCostModel` cannot make it do
so (§0.2). Confluence is per-rewrite and per-narrowing-set, not free.

**Integration seams (path:line).** Engine host (top-level, XL-A-contested) = `crates/egraph/src/
{egraph,extract,scheduler,csp_scheduler,cost_weights}.rs` + `crates/ir/src/egraph/{rules,
write_back,analysis}.rs`; decision seams (skinny) = `passes/src/backend_egraph.rs` (I1),
`passes/src/decision_csp.rs` (selection layer — needs `bounded_sum` merged into
`csp-solver/src/constraint/`), `ir/src/lib.rs` (`Recognizer`/`SimdMode`/`SimdSite`), produced at
`passes/src/lib.rs:338` (`derive_recognizers`), lowering home `codegen/src/lower/recognizer/`
(GN-2.W1). Cost tables `egraph/src/cost_weights.rs` (`CALIBRATED_WEIGHTS`); per-decision facts
`ir/src/cost.rs`. **The two homes are two workspaces (§0.3) — the unscoped port.**

**Gates it carries.** (i) **island-earns-the-e-graph** (RED-capable, and PROTO-1 exercised it): an
island's rewrites produce child-bearing survivors that *change the pick under a fixed calibrated
vector* OR its selection is genuinely constrained (Island B's budget) — else demote to a plain
cost-ranked choice. (ii) **total-order tie-break**: last cost coordinate a stable content hash
(extant `DecisionCost` ends `candidate_hash: FNV-1a(id)`) → extraction a total min independent of
container iteration; the negative control (drop the hash) must go non-deterministic. (iii)
**teardown**: `with_capacity(occupancy)`. (iv) GN-2.W1 born-RED: ≥8× cyc/B on the wired skip site,
≥0.95× dense, ≥3 cold paired PMU windows, ins/B load-invariant, structural parity before timing.
(v) deletion of the ceremonial ~68-field provenance apparatus once a plan-reading consumer exists.

**Exact open questions.** (1) **[MASTER, Brief 1]** Does a branch-count-dependent `Alt` cost term
make a *single fixed* calibrated vector pick DISPATCH at large N and SEQUENTIAL at small N —
converting PROTO-1's sign-flip into a structural crossover — such that I2/I4's tree profile faces a
real ≥2-survivor decision? Until it does, I2/I4 are leaf-collapse canonicalizations and the tree
profile is dead code. (2) **[§0.3]** In which tree does the engine+seams+`bounded_sum` co-reside,
and who scopes the port? (3) **[XL-C]** LOCK-5 keep-or-abrogate of `rules/regex.rs` (I2 WITHHELD).
(4) **[XL-D]** ≥2 IR-modeled competing scan sites (LOCK-3 owes the node shape; consumer BLOCKED two
locks deep). (5) **[XL-F]** Is I4 a real island or an always-apply canonicalization double-homed
with LOCK-1's `Project`@`passes/src/lib.rs:204`?

**Verdict.** **ADVANCED — the lock's spine, and the lawful home of 2B's surviving mechanism after
XL-B.** Its gate can genuinely go RED and PROTO-1 exercised it honestly. But its *distinctive*
extraction value (I2/I4 tree profiles) is **SPEC-ONLY straddling** on the master gate: the widening
rewrite fires and is confluent, but no calibrated vector discriminates its survivors until the
branch-count cost term exists (Brief 1). Honest floor: **0 genuine child-bearing islands with a live
consumer in one tree**; I1 is a leaf-min the family's own gate (i) demotes. Not BLOCKED-candidate —
the missing piece is a named, cheap cost-model term, not an intractable primitive.

---

## Family 2B — IR-IS-EGRAPH

**Architecture (maximal reading — CROSS-LOCK-REFUTED by XL-B).** The middle IR *is* an e-graph end
to end; passes *are* rewrite groups; a single cost-guided extraction emits per-rule `BackendShape`
directly with `derive_backend_shape` deleted. **This makes extraction the decision driver — the seat
XL-B assigns to LOCK-3.** An undeclared supersession of LOCK-3's charter; not LOCK-2's to
instantiate.

**Architecture (lawful residual under XL-B).** The e-graph is the **in-place node substrate at named
decision points within LOCK-3's tower** — acyclic (recursion is a symbolic `Ref`/`CallRule` leaf
resolved by a per-rule root table, so greedy elaboration terminates in one reverse-topo sweep, the
aegraph move), rules applied largely on-insert, exploratory saturation only where it provably pays,
global cost handed to the one constraint store. **This residual is operationally the 2A island
posture** — Cranelift's own convergence (ægraph: levels kept, each represented as a graph; avg class
1.13; no saturate-then-extract fixpoint). The difference that remains is *span* (in-place e-node
substrate across the tower vs discrete build-teardown islands) — a mechanism choice inside 2A's
frame, decided by the organ-budget/teardown measurement, not an ownership war.

**Mechanism — what is real, what is refuted, what is unsettled.** REAL and mostly built:
`BackendExpr` (`ir/src/lib.rs:376`) is the recursive sum `egraph_derive` projects to `Id`; the AC-3
saturation scheduler, the `Lattice`-valued extractor, and the widening-rewrite template
(`NormalizeDirectSinkCost`, one-shot idempotence guard) all exist; PROTO-1's `ManufactureDispatchAlt`
adds a second, confluent, terminating one. REFUTED-in-part: the distinctive thesis. Multiplicity ≈ 1
across the fleet with today's rules (triple-confirmed), so "IR IS an e-graph end-to-end" today buys
only the dedup a hash-cons already gives; and the shipping cost model cannot discriminate even a
manufactured child-bearing twin (§0.2). **UNSETTLED (not "confirmed"): context-freedom.** The
structural argument is sound — `admits_sink_only(rule_ir, target)` (`passes/src/lib.rs:954`) takes
**no caller argument**, and `BackendShape` is stored per-`RuleId`, so the model *cannot represent* a
caller-context conflict — **but PROTO-1's rider that was supposed to confirm this grepped the wrong
tree (§0.3) and the real fleet walk was never run.** The NP-hard context-sensitive-DAG-extraction
objection is therefore **contingently moot pending the real walk**, and even the adverse outcome does
not reinstate the NP-hard primitive: a conflict's identity-preserving repair is bounded sub-rule
specialization (distinct `RuleId`s), not optimal context-sensitive extraction. **Pass-1's
BLOCKED-candidate condition (an NP-hard primitive of equal difficulty) is contingently LIFTED.**

**The hard architectural boundary (research-B §6, load-bearing).** Greedy bottom-up extraction "is
not optimal for cost models that depend on sharing" (`extract.rs:9-11`); the generated-size budget
is a global, sharing-sensitive cost. So "extraction emits `BackendShape` directly" **cannot honour
the budget through the current extractor.** 2B's two lawful completions both dissolve its
independence: (1) replace greedy extraction with a solver extraction for the budget slice —
*extraction becomes a solver, converging with 2C*; (2) hand the budget to the one constraint store —
*the CSP decides the budget-coupled slice*. **PROTO-2 settled which solver: B&B + `bounded_sum`.**
The strongest honest 2B is "IR-as-e-graph for canonicalization + candidate generation, with a B&B
extraction for the budget-coupled final selection" — which is 2A's island+CSP spine.

**Integration seams.** `egraph-derive/src/lib.rs` (project recursive positions, or hand-author
`Language for BackendExpr`); `egraph/src/analysis.rs` (nullable/first-set as `Analysis` lattices);
`egraph/src/extract.rs` (cyclic guard + the solver-extraction seam); `passes/src/backend_egraph.rs`
(the extant `lower_to_rust` consumer). The `lib.rs:9-10` doc-claim "grammar IR and regex HIR
implement `Language`" is aspirational — the sole `impl Language` is `DecisionNode` (childless bag).

**Gates it carries.** (1) AC operators normal-formed **one-directionally** (flatten `Seq`,
sort/dedup `Alt` by first-set), never exploratory re-associate; confluence a prototype obligation.
(2) e-node/e-class count + iterations bounded on all live grammars. (3) `regen∘regen` byte-identical
through cost-guided extraction (content-hash tie-break; `GrammarCostModel` returns `f64` today with
Vec tie-break — the total-order fix is owed and untested at grammar scale). (4) **Whole-grammar
organ (build+saturate+extract+teardown) within ~230 µs** on CSS-L4 (2875 DTA states) — *the single
most important open measurement*; `CspScheduler` rebuilds a fresh `Csp` per iteration (opposite of
aegraph's no-rebuild lesson) — a pure-assertion scaling risk. (5) the real `admits_sink_only`
caller-conflict walk (never run — §0.3).

**Exact open questions.** (1) **[MASTER, Brief 1]** ≥2 semantically-distinct survivors *at
child-bearing nodes* under a discriminating cost model? (2) whole-grammar organ within budget incl.
teardown? (3) does extraction flip ≥1 real `BackendShape` vs the cascade where they disagree? (4)
**[§0.3, Brief 2]** the real caller-conflict walk over skinny `admits_sink_only` — 0 or ≥1?

**Verdict.** **BLOCKED-candidate LIFTED → converging into 2A.** The maximal reading is XL-B-refuted
(seizes LOCK-3's driver seat); the lawful residual is the 2A island posture; the distinctive value
is triple-refuted (multiplicity ≈ 1) and now further SPEC-ONLY-straddled by PROTO-1; the budget
boundary forces termination in a B&B extraction (PROTO-2), dissolving independence into 2A's spine.
The pass-1 NP-hard BLOCKED-candidate objection is **contingently removed** (context-freedom
structurally sound, repair is bounded specialization) — but **context-freedom is UNSETTLED**, not
confirmed (the real walk was never run). 2B is BLOCKED on *earning distinct existence over 2A*,
which only the master gate can grant and has not. Its real deliverable — adopt-and-prove the port
between the two workspaces — is unscoped by any family (§0.3).

---

## Family 2C — CSP-CENTRAL

**Architecture (as-primacy RETIRED; as-Island-B EARNED-solver).** The maximal reading — "the
decision driver IS one global COP over the whole grammar" — is RETIRED (Unison ceiling: CP
proven-optimal only to ~946–1000 instr/function; CSS-L4 = 2875 DTA states is off the cliff). What
survives and is LIVE: **the scan-plan CSP** — per-site lowering choices as variables, legality + the
generated-size budget as constraints, a cost lattice as objective, e-graphs demoted to pure
candidate generators. **This is the same seam as 2A-I3, and PROTO-2 earned its solver home.**

**Mechanism (the coupled sub-CSP).** Variables: one per lowering decision site (backend shape per
rule; scan/prefilter tier per self-loop site; capacity plan per container; residualization depth per
delimiter scan), domain = the candidates the island's e-class extracts (≤5). Constraints: legality
(the four `decision_csp.rs` lambdas become genuine domain filters, not tautologies over a pinned
variable); **coherence** (binary equality edges from `SameSubstrateUnionFacts` where a shared
sub-rule under two callers must agree — the "few coherence edges" keeping the graph near-tree,
PROTO-2 confirmed treewidth ≤ 2); **the generated-size budget as a global `bounded_sum` knapsack**
(`Σ static_size_cost ≤ loc_budget`). Objective: `MinimizeCost` via the existing B&B
(`solver/optimize.rs` + `solver/search.rs`), per-value cost fed by `Measurement.
cycles_per_byte_x1000` — replacing the HEAD hardcoded integer `perf_cost` (2D's territory).

**Solver shape (Q6 — RESOLVED by PROTO-2, no longer SPEC-ONLY).** On the fleet's actual near-tree
graphs, **B&B + `bounded_sum` is optimal on every instance in 3–35 nodes / <100 µs**; PBQP fully
reduces (RN=0, ~1–5 µs) but its Lagrangian budget-dual carries an irremovable integrality gap
(css-l4 slack 39.003 vs true 21.011) at ~10⁴× wall-clock. **Route B (B&B-native-knapsack) is the
home; Route A (PBQP) is ceremony.** The `bounded_sum` propagator is MERGED-and-tested
(`cargo test -p csp-solver --test bounded_sum` = 6/6) — but merged from a **divergent bank tree**
carrying `soft.rs`/`cardinality.rs`/a `Soft` variant the base lacks (§0.3 provenance caveat); it
dropped in via the base `ConstraintEnum::Custom` escape hatch. **Residual (the one point for PBQP):**
soft/finite coherence penalty is unrepresentable in base B&B (needs the `Soft` extension the bank
prototypes) — untested; only load-bearing if finite coherence preferences turn out real.

**Selection-equivalence with the cascade it replaces** (conservative extension): the priority
cascade never optimized the budget (a post-hoc lint that "blocks the wave"), so folding it in as a
constraint cannot lose a decision the cascade made — it reproduces the per-site optimum exactly in
the budget-slack regime and *corrects* the cascade only in the budget-tight regime the cascade is
blind to.

**Integration seams.** `passes/src/lib.rs:407` (`derive_backend_shape_with_diagnostics`),
`passes/src/backend_egraph.rs`, `passes/src/decision_csp.rs` (replace the rubber-stamp 1-var CSP);
`csp-solver/src/{lib.rs, constraint/ (bounded_sum merged), solver/optimize.rs, solver/search.rs,
domain/cost_finite.rs, solver/gac/}`; `ir/src/cost.rs` (`DecisionCspFacts`, `PriorityStep`,
`Measurement`, `SameSubstrateUnionFacts`); budget = `Cargo.toml` `loc_budget`.

**Gates it carries.** (1) a budget-binding grammar: the CSP forces `Exact` on ≥1
legal-but-unaffordable site under a tight budget AND emits a byte-identical plan under slack (parity
floor). (2) `regen∘regen` byte-identity on the **global** solve: variable order by stable `RuleId`,
value order by the `candidate_hash` total order the extractor uses. (3) solve cost within budget,
reported (PROTO-2: <100 µs, comfortable). (4) the objective consumes ≥1 real cost fact (2D
dependency).

**Exact open questions.** (1) **[XL-D, consumer-blocked]** Does the fleet present ≥2 *IR-modeled*
competing scan sites with *real binding* costs, so the budget actually binds? Source says yes
(css/pretty=15, css/l4≥10, bbnf=2, csv=2 self-loops); the IR says no (`SimdMode`/`SimdSite`
single-valued); the bridge (`SelfLoop`/`Prefilter` node shape) is LOCK-3's to build, derivation
consumer BLOCKED two locks deep. (2) Is soft/finite coherence penalty load-bearing (the one residual
PBQP point)? (3) Degenerate without 2D: absent a fitted cost table the objective is
`static_size_cost`, "a CSP-shaped priority cascade with extra steps." (4) one-solver-home with
LOCK-1 1B (XL-G): `bounded_sum` extends the same csp-solver LOCK-1 centralizes — a GESTALT-B
unification point, and the three-tree provenance (§0.3) must reconcile first.

**Verdict.** **EARNED-solver / BANKED-teeth.** The solver-shape question is CLOSED (B&B-native-
knapsack, PROTO-2) — the pass's genuine positive result and the strongest-earned family mechanism.
The `bounded_sum` propagator is built and tested. Not BLOCKED-candidate — the missing primitive is
*built*. It is blocked on a **consumer** (IR-modeled sites read by a real emitter) no single lock
currently owns end-to-end, and on the three-tree reconciliation (§0.3). Teeth remain SPEC-ONLY.

---

## Family 2D — PROFILE-CLOSED-LOOP

**Architecture.** The cost model is **measured, not authored**: coefficients fitted offline from cold
paired PMU captures, committed as a content-addressed const fact table, read (never re-measured) by
the decision solver at regen. Three phases — **FIT** (offline `xtask fit-cost`, A/B-differential, one
lever per paired arm — the only sound attribution when whole-grammar cyc/B underdetermines per-feature
coefficients), **COMMIT** (human-gated; write `CALIBRATED_WEIGHTS`, bump `cost_formula_version`),
**FRESHNESS GATE** (CI, born-RED; drift → RED). **The load-bearing determinism invariant:** the loop
closes at *calibrate-time, not regen-time* — regen is a pure function of `(grammar, committed-β)`, so
`regen∘regen = regen` survives; all load noise lives in `xtask fit-cost`, off the regen path. Exactly
the AutoFDO/BOLT discipline with a stale-profile staleness gate.

**Mechanism.** ~70% substrate-present, all inert. `ActiveCostFacts` carries `cost_formula_version`,
`selected_cost_freshness`, `determinism_replay_status`, `rewrite_order_variance_pct`,
`selection_trace_hash`; `Measurement` stores cyc/B as **u64 fixed-point** (×1000 — the float-tie
determinism fence); the total-order tie-break is already correct (`candidate_hash` FNV-1a suffix);
`decision_csp.rs:183 static_profile_provider_status: "static-template-blocker"` is the field 2D
replaces. The env-fence is nearly clean (`from_env` reads only a runtime probe, not the regen path);
a CI grep-gate forbidding `from_env` on any regen path closes it hard. The residual consumer is
proven (PROVE-12: const-fold at generation time, +18 generated LOC, cyc/B 0.81–0.98×, regen
bit-for-bit). The missing 30%: the fit-harness body, the freshness-check body, and — critically —
**a live decision for a coefficient to flip.**

**Integration seams.** `ir/src/cost.rs` (`ActiveCostFacts`, `Measurement`, `EvidenceSource`);
`egraph/src/cost_weights.rs` (`CALIBRATED_WEIGHTS` slot; the 54-config null guard `:134-167`);
`passes/src/backend_egraph.rs` (`perf_cost`, freshness fields today hardcoded `FORMULA=
"sk-v13-w6-integer-v1"`); `decision_csp.rs:183` (`"static-template-blocker"` →
`"fitted-committed-v1"`); `xtask` (`fit-cost`/`check-calibration`); the freshness gate in `report.rs`.

**Gates it carries.** (1) a fitted value FLIPS a decision AND the fitted lane beats the hardcoded
lane cold-paired on a real grammar. (2) **ins/B leads, cyc/B is the certified check** (PROVE-02 law:
an ins/B win that does not carry to cyc/B is rejected, not committed). (3) `regen∘regen`
byte-identical; `from_env` grep-gate GREEN. (4) **falsifiable freshness**: mutate a coefficient → the
gate goes RED, `check-calibration` fails (today's `"pass"`/`0` stamps are the lie 2D removes — until
a proto shows the gate can go RED, the anti-theater claim is itself provenance theater). (5)
aarch64-only; a flip must survive on a held-out grammar.

**Exact open questions.** (1) **[MASTER, Brief 1 — BINDING]** the multiplicity-1 wall: a fitted
coefficient cannot flip a decision that has no discriminable alternatives; PROTO-1 confirms even a
manufactured confluent twin is not discriminated under calibration. 2D must **co-land with the first
decision that has ≥2 real survivors** — which Brief 1's branch-count cost term either manufactures or
refutes. PROTO-1 hands 2D the *path* (fit `dispatch_table`/`dispatch_branch` at a branch-count
crossover) but does not walk it. (2) Attribution underdetermination — resolved structurally by
A/B-differential; ceiling = count of independently-toggleable levers with a paired bench (disclose
it). (3) The only *unblocked* plane is cross-lock-owned: `select_backend` kernel crossover (LOCK-9),
`CSS_SHUFTI_MIN_SPAN`/`GATE=16` (GN-2 scanner knobs) — fitting these supersedes hand-tuned GN
literals, a **named supersession (ix)** the spec must declare, not treat as free attachment.

**Verdict.** **BLOCKED (SPEC-ONLY) — composable, not exclusive.** Sound discipline, correct
precedent (AutoFDO/BOLT/uops.info), right determinism invariant; **zero executed fit** (no PROVE-2D
ran). Its master-gate dependency resolved *adversely* this pass (PROTO-1: no discriminable front to
fit). It is a composable coefficient layer UNDER whichever generator wins, not architecturally
exclusive with any family. Not strictly BLOCKED-candidate — the blocker (a live ≥2-survivor decision)
is a named, cheap cost-model change (Brief 1), not an intractable primitive — but it must ship ONE
executed fit before it may speak of convergence.

---

## The incompatibility map — which families cannot coexist, and precisely why (pass-2, post-proto)

Pass 1 drew a primary fork {2A,2B}, a primacy conflict {2B,2C}, a scope conflict {2A,2C}, one
composable layer {2D}. The cross-lock adjudications collapsed the hard forks into sub-questions of a
single frame; the two prototypes now *resolve* the internal ones and leave two empirical gates + one
substrate contradiction.

### I. 2A ⊗ 2B — DOWNGRADED to mechanism-difference-inside-2A (XL-B), and the residual is now empirical
XL-B forecloses 2B's maximal reading (extraction cannot be the decision driver — LOCK-3's seat).
2B's lawful residual (acyclic in-place e-node substrate, on-insert rewrite, B&B extraction for the
budget slice) is operationally the 2A island posture — Cranelift's own convergence. What remains is
a **mechanism choice inside 2A's frame**: e-graph substrate *persists in place across the tower*
(2B-residual) or is *rebuilt-and-torn-down at chokepoints* (2A islands) — decided by the
organ-budget/teardown measurement (2B gate 4), an empirical question. **No longer the lock's primary
fork.**

### II. 2B ⊗ 2C — DISSOLVED, and PROTO-2 named the meeting point's solver
Both routes reach the same seam: 2B's budget boundary forces extraction to terminate in a solver for
the sharing-sensitive slice; 2C demotes extraction to candidate generation and makes the CSP decide.
**They meet at the `bounded_sum` knapsack over the near-tree scan-plan graph — and PROTO-2 settled
the solver: B&B, not PBQP.** No live primacy conflict remains.

### III. 2A ~ 2C — CONVERGED to identity at Island B
2C-as-primacy is RETIRED (Unison); its surviving form *is* 2A's Island B — same variables, same
`bounded_sum`, same (now-resolved) B&B solver. Not a conflict; the same route under two names. The
only residual axis (scope vocabulary) both agree is a *bounded, small* sub-CSP (peel independent
sites, solve only the coupled core) — PROTO-2 confirmed the coupled core is tiny (3–35 B&B nodes).

### IV. 2D — composable with the winner (NOT exclusive), prerequisite + blocked plane
2D is the coefficient plane *underneath* the candidate generator, orthogonal to *who decides*.
Constraints are dependency, not exclusion: BLOCKED at the shape lattice (the master gate recurs),
unblocked only at knobs another lock owns (named supersession), requiring the winning generator to
have manufactured ≥2 discriminable survivors first (Brief 1).

### V. The substrate contradiction — a cross-lock incompatibility, not an intra-family one (NEW, load-bearing)
The engine (top-level `crates/`), the decision seams (`skinny/crates/`), and the `bounded_sum`
propagator (a third divergent bank) live in **three different tree-states**; no single tree holds
engine+seams. Every family's live deliverable requires them co-resident, and no family scopes the
port. XL-A's "no port between trees" framing is refuted by disk. **This is the pass's binding
blocker and a challenge routed to GESTALT-B (§0.3).** It is incompatible with the 13:10 draft's
premise that the organ can be "proven on skinny" — skinny has no organ.

### The surviving shared gates (bind all four)
Every family's distinctive value — 2A's I2/I4 tree profiles, 2B's extraction, 2C's non-degenerate
objective, 2D's lattice attachment — is gated on **whether a discriminating cost model makes a
confluent widening rewrite's ≥2 survivors change the pick under a fixed calibrated vector at a
child-bearing node, moving a bench at value-starts (Brief 1).** PROTO-1 built the rewrite (green) and
showed the *current* model cannot; the named fix is untested. The scan-plan/2C axis carries a second,
orthogonal gate — ≥2 IR-modeled competing sites with binding costs so the budget binds (consumer
BLOCKED two locks deep, XL-D). Both negative results are decision-grade.

**Net pass-2 shape.** One surviving frame — **2A's directed-spine-with-islands**, into which 2B's
lawful mechanism folds and 2C's Island B (solver home now EARNED = B&B + `bounded_sum`) is the
scan-plan seam — with **2D the composable coefficient layer beneath it**. The lock is one
architecture with **one earned mechanism (the B&B/`bounded_sum` scan-plan solver)**, **one
SPEC-ONLY-straddling crux (extraction-as-decider, pending the branch-count cost term)**, **one
consumer-blocked axis (IR-modeled scan sites, XL-D)**, and **one binding substrate contradiction (the
three-tree split, §0.3/§V)** that GESTALT-B must adjudicate.

---

## Prototype briefs (the two sharpest uncertainties for pass 3)

### Brief 1 — PROVE-2-ALTCOST-CROSSOVER (kind: bbnf-worktree)
**Question.** Extend `GrammarCostModel::cost`'s `Alt` arm to net `dispatch_table` (fixed setup)
against `N·dispatch_branch` vs `N·alt_per_branch` (a branch-count-dependent term). Does a **single
fixed calibrated weight vector** then pick DISPATCH at large N and SEQUENTIAL at small N — the
structural crossover PROTO-1 named and declined to build — such that `Extractor` picks *differently*
under two calibrated vectors from *structure, not the forbidden `dispatch_bonus` sign-flip*, and the
two lowerings move cyc/B at real value-start positions? This is the exact FAIL-cause PROTO-1 located;
it resolves extraction-as-decider (refuted vs real) for 2A-I2/I4, 2B, and 2D at once — the whole
lock's crux.
**Method.** Worktree base-pinned (`baseCommit`), HEAD named, `--profile ax-iter`. In the tree that
holds `GrammarENode`/`GrammarCostModel` (top-level `crates/ir/src/egraph/`; note the §0.3
substrate caveat and, if Brief 2 lands first, run in the reconciled skinny tree). Edit the `Alt` arm
so setup-vs-per-branch amortization is expressed; reuse PROTO-1's `ManufactureDispatchAlt` +
all-narrowing set through `CspScheduler`. Build two child-bearing `Alt`s: small-N (2 branches) and
large-N (8 branches). Extract both under ONE `CALIBRATED_WEIGHTS` vector; assert small-N→SEQUENTIAL,
large-N→DISPATCH (crossover from a fixed vector). Then lower each pick; cold-paired PMU (N≥200, ≥3
windows, ins/B load-invariant, structural parity before timing) at value-start positions only.
**Commands.** `cargo test -p bbnf-ir --test egraph alt_cost_crossover`; `cargo bench --bench
css_canon_bench`; regen twice for `regen∘regen`.
**Pass/fail.** PASS = a single fixed calibrated vector yields small-N SEQUENTIAL and large-N DISPATCH
(structural crossover), AND the two lowerings' cyc/B 95% CI excludes 1.00× same-direction at
value-starts → extraction-as-decider EARNED; 2A/2B/2D unblock. FAIL = the crossover never appears for
any single fixed vector (still sign-only), OR CI includes 1.00× → extraction-as-decider REFUTED; LOCK-2
collapses to canonicalization + the `BackendShape` driver. Decision-grade either way.

### Brief 2 — PROVE-2-SUBSTRATE-RECONCILE (kind: bbnf-worktree)
**Question.** Can the e-graph engine (top-level `crates/ir/src/egraph/`: `GrammarENode`,
`GrammarCostModel`, `rules/`, `write_back.rs`) be ported into the skinny substrate and wired to
skinny's decision seams (`passes/src/backend_egraph.rs`) in ONE tree — compiling, with `regen∘regen`
byte-identical — and does the REAL `admits_sink_only` caller-conflict walk over the skinny fleet
return 0 or ≥1 conflicts? This scopes the adopt-and-prove port no family scopes, settles 2B's
context-freedom for real (PROTO-1's rider grepped the wrong tree), and tests whether XL-A's
"skinny is the substrate" survives contact with the engine's actual home (§0.3 challenge to
GESTALT-B).
**Method.** Skinny worktree, base-pinned. Vendor/port the top-level `egraph/` engine modules into
`skinny/crates/ir/src/egraph/` (organ-level, per XL-A); wire one skinny decision seam
(`backend_egraph::select`) to consume an extracted `GrammarENode` candidate. Merge `bounded_sum.rs`
from the divergent bank into `skinny` `csp-solver/src/constraint/` via the `Custom` seam. Then walk
the skinny `CallRule` graph: for each `RuleId`, collect callers' substrate contexts
(`TargetFeatures`-derived) and flag any rule where `admits_sink_only` (`passes/src/lib.rs:954`, the
REAL predicate) would verdict differently across callers. Report conflict count + smallest witness.
**Commands.** `env -C skinny cargo build -p ir -p passes`; `env -C skinny cargo run -p xtask --
check-json` twice (`regen∘regen`); `cargo test -p passes admits_sink_conflict_walk`.
**Pass/fail.** PASS = engine compiles in skinny + ≥1 skinny seam consumes an extracted candidate +
`check-json` byte-identical across two runs + the real conflict walk returns a concrete count (0 →
2B context-freedom RATIFIED; ≥1 → repair is bounded sub-rule specialization, name the silently-wrong
site). FAIL = the port cannot compile in skinny without pulling the whole top-level tree (the
workspaces are not organ-separable → XL-A inversion, escalate to GESTALT-B), OR regen diverges.
Decision-grade either way — it either scopes the port or proves the tree-of-record contradiction is
load-bearing.
