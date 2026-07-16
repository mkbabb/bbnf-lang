# LOCK-2 — THE GRAPH ENGINE — REGISTRY

*e-graph + CSP optimization: HIR canon, IR rewriting, kernel/backend selection, capacity & schedule
decisions, profile-fed cost, deterministic regen. Families — 2A EGRAPH-ISLANDS · 2B IR-IS-EGRAPH ·
2C CSP-CENTRAL · 2D PROFILE-CLOSED-LOOP. This registry is the agglomerator's running ledger across
passes; the newest pass governs, prior passes retained below as history.*

Constitution binds: consumer-less substrate forbidden (viii/Y.13); type plane costs zero at runtime
(v); concrete artifacts or SPEC-ONLY (vii); refutation laws — no warm benches, ratios certified /
absolutes directional (iv); deterministic regen (`regen∘regen = regen` byte-identical); extant
GN/PT codex is the baseline, divergences are NAMED supersessions (ix).

---

## Current disposition (as of pass 1)

| Family | Status | Conv. | One-line |
|---|---|--:|---|
| **2A EGRAPH-ISLANDS** | **ADVANCED** | **50%** | The defensible core: LLVM directed canonicalization spine + local islands. The lock's spine, but all three candidate islands are cross-lock-contested (A owned+possibly-abrogated by LOCK-5, B's derivation BLOCKED two locks deep, C double-homed with LOCK-1), and the middle-IR it operates on is LOCK-3's. Honest floor: 0 fully-instantiated in-tree islands this wave. |
| **2B IR-IS-EGRAPH** | **BLOCKED** | **32%** | Net-new in skinny (both critics concur), de-risked by a working, wired legacy exemplar — but its distinctive extraction value is triple-refuted inert, its signature "extraction emits BackendShape" claim rests on an NP-hard context-sensitive repair, and its real fork is LOCK-2⊗LOCK-3 (tower ownership), not intra-lock 2A⊗2B. |
| **2C CSP-CENTRAL** | **BANKED** | **30%** | As-primacy RETIRED (Unison ceiling; collapses to 2A Island B — both critics concur). The `bounded_sum` knapsack propagator is a tested, RAN, purely-additive **banked asset** folded into Island B. Teeth are source-real (≥2 shufti sites in CSS/BBNF/CSV source) but IR-latent, un-budgeted, consumer-less; the consumer is BLOCKED two locks deep. |
| **2D PROFILE-CLOSED-LOOP** | **BLOCKED** | **30%** | Sound discipline (calibrate-time closure preserves the regen fixpoint), correct precedent (AutoFDO/BOLT/Ansor), but SPEC-ONLY: zero PROVE-2D artifact ran. Its only unblocked plane (threshold/kernel/capacity knobs) sits inside LOCK-9/GN territory; the plane LOCK-2 owns (shape lattice) is §0-BLOCKED. |

**The lock returns as a converged CORE — 2A's directed canonicalization spine + the existing
BackendShape driver, anchored by the running legacy grammar e-graph — but it is a core LOCK-2 cannot
ratify alone.** Every one of its four families reaches across a lock boundary the intra-lock
incompatibility map does not draw: the middle IR is LOCK-3's (XL-B), the scan sites are
LOCK-5/LOCK-9's (XL-D/E), the projections are LOCK-1's (XL-F), PROVE-12 is LOCK-3's (XL-C), the
solver home is shared with LOCK-1 (XL-G), and the tree it ports into is portfolio-contested (XL-A).
Extraction / CSP-extraction is recorded SPEC-ONLY pending a **fired**, confluent, non-sign-flipped
manufacturing rewrite on a **representative** workload **in the skinny tree**; the scan-plan
CSP/budget machinery is recorded SPEC-ONLY pending an **IR-modeled** `SelfLoop`/`Prefilter` site +
a **live consumer no single lock currently owns**.

---

## 0. The pass-1 governing corrections — census AGREED; two NEW findings replace the prior "dispute"

The prior draft of this registry treated the substrate census as a *disputed judgment call between
the two critics* and spent its §0 "settling" it. **That framing is stale.** The two on-disk critiques
(the FINAL CRIT-1, which supersedes its 05:07 draft, and the FINAL CRIT-2, re-anchored to skinny HEAD
`fe160a91a`) **AGREE on the census** — CRIT-2 explicitly re-anchored on disk "to avoid the wrong-tree
trap the prior CRIT-2 fell into" and states "the within-lock substrate census (§0, CRIT-1,
research-A/B) is correct **for the skinny tree**, and I do not re-litigate it." There is no census
dispute to adjudicate. What the FINAL critiques deliver instead are **two new governing findings**,
neither of which the prior draft carried, plus one decisive datum that survives.

### The census (settled, not disputed — both critics + on-disk verification)
- **The skinny tree (`skinny/crates/`) is LOCK-2's subject.** `skinny/crates/ir/src/` is **only
  `cost.rs` + `lib.rs`** — no `egraph/`, no `GrammarENode`, no `build_and_saturate`. The **sole
  `impl Language` is `DecisionNode`** (`backend_egraph.rs`), a childless-bag union + `best_node`;
  the 1-var `decision_csp.rs` re-selects its input; `perf_cost: 0` (×5). `SimdSite = {PreEntry}`,
  `SimdMode = {Exact}` — single-valued (`ir/src/lib.rs:401-409`, verified on disk this pass). **2B
  is genuinely net-new here; the substrate is ceremonial here.** research-A/B/priorart, CRIT-1,
  CRIT-2, and FAMILY-SPECS §0 are all correct for this tree.
- **The legacy tree (`crates/`, where `bbnf-ir` actually lives) has a working, WIRED grammar
  e-graph** (`crates/ir/src/egraph/`: `GrammarENode: Language`, `write_back.rs`, `rules/regex.rs`,
  `analysis/`, per-rule root table, cycle guard), called on the production compile path at
  `crates/core/.../pipeline.rs:218-222`. This is a **de-risking asset** — an in-project reference
  implementation of the 2A/2B architecture that ships and is deterministic on a real compile path.

### NEW FINDING 1 (CRIT-1's governing within-lock finding): the sharpest uncertainty was tested on the WRONG tree
PROTO-1 (`prove2_branch`) is built `-p bbnf-ir` and imports `crates/ir/src/egraph/cost.rs` /
`types/node.rs` — **`bbnf-ir = crates/ir = the LEGACY tree.**` `GrammarENode`/`GrammarCostModel`
exist **nowhere in skinny.** So PROTO-1 proves a divergent cost-guided pick **on the legacy engine —
machinery that must be *ported into skinny* before any of it is true there.** The port (legacy
`crates/ir/egraph` → skinny, replacing the childless-bag `backend_egraph`) is **the actual unbuilt
core, and NO family scopes it**: 2B thinks it must *build* the Language (it already exists in legacy),
the prior CRIT-2 thought it must *delete* dead code (it's live in legacy), and the prior registry
presented PROTO-1 as the lock's own production machinery (it's another tree's). 2B's honest skinny
deliverable is neither "build" nor "delete" but **adopt-and-prove** — port/re-derive the legacy
engine, prove it stays in budget, regen-byte-identical, and changes ≥1 output for the measurable
better. All three are UNTESTED and the port is unscoped.

### NEW FINDING 2 (CRIT-2's governing cross-lock finding): LOCK-2 mislocates its own primary fork
The FAMILY-SPECS names **2A⊗2B** as the "primary architectural fork." That is the **wrong fork**,
because *the middle IR is not LOCK-2's to shape.* **LOCK-3's charter owns "the IR tower and its
transpositions … the decision driver's seat"**; its REGISTRY says "the tower is 3B-shaped today …
LOCK-3 either hardens (3A/3B) or re-founds (3C/3D)." Both 2A ("the middle IR is an annotated tree,
e-graphs are local tactics") and 2B ("the middle IR *is* one e-graph container; `derive_backend_shape`
is deleted; extraction *is* the lowering") are **verdicts on what LOCK-3's tower IS** — 2B is a fifth
option imposed on LOCK-3's spine that LOCK-3's own family set does not enumerate. **The genuine
primary fork is LOCK-2 ⊗ LOCK-3: does graph-optimization govern the middle IR's SHAPE, or operate as
a tactic WITHIN a tower LOCK-3 owns?** The intra-lock 2A⊗2B fork is a *sub-question* of a boundary
LOCK-2 has not earned the right to answer alone — an undeclared supersession of LOCK-3 (clause ix)
that lowers every family touching the middle IR.

### The decisive §0 datum that survives both findings (canon-suffices, triple-confirmed)
The legacy wired rule set (`default_rules()`, `rules/mod.rs:87-96`) is **all-narrowing** —
`DeduplicateAltBranches`, `SupersetAbsorbAlt`, `UnionMergeAlt`, `FuseAltRegexBranches`,
`CommonSuffixFactor`, `AltOfSingle`, `RepeatOfSingle`, `WrapOfEpsilonScalar`, `ConcatLiterals`. **No
widening/manufacturing rule exists even in the project's own shipping grammar e-graph.** So §0 is
**triple-confirmed** (in-tree R-1 54-config null + external Cranelift aegraph avg-class-1.13 /
2-of-4M + the project's own wired all-narrowing set): *canonicalization is the value; extraction is
inert absent a manufacturing rewrite that no one has built or shown confluent.* This survives the
PROTO-1 re-run — which confirmed the payoff exists but built the alternative by hand-`union`, never a
fired rewrite (§1).

---

## 1. The two master-gate prototypes — RAN_MEASURED, and how much they actually lifted

Both prototypes RAN this pass and reproduced within noise. Both critics independently converge (and
this pass verifies) that the two verdicts are **softer than the binary label**; the agglomerator
adopts the critics' floor with named reasons.

### PROTO-1 (PROVE-2-BRANCH, §0 master gate) — RAN_MEASURED · earns *machinery-not-broken (on legacy)*, NOT *extraction-decision-real (in skinny)*
- **RAN_MEASURED, reproduced.** Part A: e-class held **2 distinct Alt forms (no collapse)**; W1
  (`dispatch_bonus=−2.0`) picked dispatch-table (best_cost 13.0); W2 (`+2.0`) picked sequential-trial
  (15.0); picks diverged. Part B (cold-paired, PMU cyc/B, N=200-240, 3 windows): dispatch table
  strictly cheaper — twitter 3.344× [3.320, 3.368], citm 2.910× [2.897, 2.922], canada 2.268×
  [2.255, 2.281]; all three 95% CIs exclude 1.00× same-direction; prior-artifact agreement within
  noise (3.342/2.868/2.281× prior). The **machinery is not broken**: a cost model + Extractor respond
  to weights and the pick moves silicon.
- **But it ran on the LEGACY tree (NEW FINDING 1).** `-p bbnf-ir` = `crates/ir` = legacy. The result
  is on the legacy `GrammarENode`/`GrammarCostModel` — un-ported to skinny.
- **The rewrite was never fired** (PROTO-1's own candor, limits §2): the 2-candidate e-class was
  built by direct `union`, not a registered `Rewrite` through `CspScheduler`. The load-bearing step —
  a *widening* `ManufactureDispatchAlt` that (a) detects disjoint FIRST sets + proves the two forms
  equal, (b) is **confluent with the all-narrowing set** (does `DeduplicateAltBranches` collapse the
  twin right back?), (c) **terminates** — is declared "trivial next step" and left unbuilt. Both
  critics flag the elegant-reduction trap; the legacy all-narrowing exemplar (§0) proves it is *not*
  trivial (nothing in-project manufactures a branch).
- **W2 is a synthetic sign-flip** (`+2.0` = negation of calibrated `−2.0`) — proves weight-sensitivity,
  not a real Pareto front where *calibrated* weights ever pick sequential. And the dispatch table
  dominating **2.27×-3.34× on every corpus** is a **canonicalization** signal (always rewrite
  Alt→table), which needs no extractor.
- **Part B's cyc/B is the wrong byte distribution** (twitter 81% NOMATCH; a real parser never fires
  the value Alt on interior/ws bytes). Certified for a contrast the parser does not run; "direction
  not sensitive" is asserted, not measured at value-starts.
- **De-risking over-reach:** the artifact is a grammar-`Alt` dispatch decision (Island-B-flavored).
  Islands A (regex-HIR canon) and C (projection canon) — the two islands whose entire justification
  is "child-bearing nodes → real extraction" — received **zero evidence**.

### PROTO-2 (PROVE-2-BUDGET, budget gate) — RAN_MEASURED · earns *the propagator works + regen holds*, NOT *teeth on the corpus*
- **Real, tested, RAN net-new asset:** `bounded_sum` knapsack propagator (`crates/csp-solver/src/
  constraint/bounded_sum.rs`, +345 LOC purely additive / 0 deletions, wired via the existing `Custom`
  seam with no dispatch edit; `cargo test -p csp-solver --test bounded_sum` = **6 passed / 0 failed**).
  BANKED, correctly homed in csp-solver (general-infra edict, parsimony vi — clean).
- **The MECHANISM binds.** Slack budget 1000/700 → plan `{Prefilter,Prefilter}`, 0 forced Exact
  (parity floor with the per-rule greedy pick). Tight 550/500 → `{Prefilter,Exact}`, exactly 1 site
  forced to Exact (**budget BINDS, plan changes from greedy**). Budget 150 (< 2×Exact) → domain
  wipeout → `Unsatisfiable`. The rigged 1-var CSP (`value == selected_index`) structurally cannot make
  this cross-site trade.
- **Regen byte-identity RAN** (new vs prior draft): `env -C skinny cargo run -p xtask -- check-json` =
  **exit 0** — skinny `passes` recompiled against the modified csp-solver still regens byte-identical
  (`check_dir` fails closed on any diff). BUT CRIT-1 correctly discounts: byte-identity holds *because
  the additive propagator perturbs nothing that is wired* (the plan has no reader) — determinism shown
  at the CSP-decision layer only.
- **The ≥2 competing sites are source-real but IR-latent** (the whole finding). Skinny IR models **1**
  grammar (json), **1** `SimdScan{Exact,PreEntry}`, **0** `SelfLoop`; `SimdSite`/`SimdMode` are
  single-valued. The whole-repo grammar SOURCE audit found abundant shufti self-loops
  (`css/pretty.bbnf=15`, `css/l4/*≥10`, `bbnf.bbnf=2`, `csv.bbnf=2`, `json` string-interior=1) — so
  **the Brief-2 cheap falsifier did NOT fire: the fleet has the structure in source.** 2C is refuted
  **only for the skinny corpus as modeled today**, not in-source. The blocker is precisely
  "IR-unmodeled + un-budgeted + consumer-less," not "the fleet lacks it."
- **The costs are invented** (Prefilter=300/40 LOC, Exact=100/8 LOC, synthetic cyc/B). Whether the
  budget binds is a pure function of author-chosen constants; wf30's own candor: "if real Prefilter
  lowering is ~12 LOC, the budget may never bind." Real costs are 2D's territory (itself SPEC-ONLY) —
  a binding demonstration SPEC-ONLY resting on SPEC-ONLY.
- **No consumer reads the plan** (viii/Y.13). The consumer is GN-2.W1's unbuilt SelfLoop/`find_member`
  lowering — and cross-lock, it is BLOCKED **two locks deep** (XL-D). The honest verdict (adopted
  verbatim from wf30, over wf28's flatter PASS): *2A Island B mechanism PROVEN; 2C teeth LATENT —
  source-real, IR-unmodeled, un-budgeted, consumer-less.*

---

## 2. Per-family ledger

### 2A — EGRAPH-ISLANDS · ADVANCED · 50%
*Convergence: on-disk critics' floor (CRIT-1 50, CRIT-2 50). The prior draft's 55 cited stale critic
numbers (55/58) from superseded drafts; corrected to the final on-disk 50/50. No named reason to
exceed the floor — the PROTO-1 re-run confirmed machinery-not-broken but touched none of the island
gaps, and CRIT-2's cross-lock island contests are additional, not priced-in.*

Strongest-aligned; the directed-shell architecture (LLVM canonicalization spine + local islands at
named chokepoints) is coherent, and its gate (i) — "child-bearing nodes OR genuinely constrained,
else write it as a plain cost-ranked choice, do NOT island it" — genuinely can go RED (both critics
credit this; it is not a gate-that-cannot-fail). The legacy tree's wired grammar e-graph is running
proof the *canonicalization spine* ships.

**Open gaps:**
1. **Middle-IR ownership vs LOCK-3 unadjudicated (XL-B, NEW).** 2A's "directed spine, IR is an
   annotated tree between islands" is itself a verdict on LOCK-3's tower. Speculative annexation of
   LOCK-3 territory until GESTALT-B rules. Lowers the family.
2. **Island A (regex-HIR canon) — un-evidenced AND doubly cross-lock-blocked (XL-E).** No proto
   touched a regex leaf; open-Q(1) unanswered. LOCK-5 owns the lawful HIR (unrendered) AND its P1 lists
   "fold into it **or abrogate**" the shared `rules/regex.rs` exemplar 2A wants to port; LOCK-5 P2:
   grammar-driven regex SPEC-ONLY lock-wide. 2A's only child-bearing island depends on a verdict + a
   substrate another lock has neither rendered nor decided to keep.
3. **Island B (scan-plan) — sites absent from the IR; deriving substrate BLOCKED two locks deep
   (XL-D).** `SimdSite::SelfLoop`/`SimdMode::Prefilter` do not exist (disk-confirmed). The
   recognizer-derivation is not one dependency ("GN-2.W1 unbuilt") but blocked through LOCK-9 9C
   (BLOCKED — "scanner-as-DFA") and LOCK-5 (grammar→DFA source-of-truth, SPEC-ONLY). "Same-wave
   consumer that changes emitted code" is not present and not near-term.
4. **Island C (projection canon) — double-homed with LOCK-1, and probably not an island (XL-F).**
   `Project`@`passes/src/lib.rs:204` is LOCK-1's; PROTO-1's table-always-wins implies `map∘map` /
   dead-projection are always-apply canonicalizations → by gate (i) they are plain rewrites, NOT
   islands. The prior "LOCK-1's 1D is BANKED-vacuous → nothing to share" is a wrong-tree read: LOCK-1's
   REGISTRY says its live 1B/1D substrate is "the shipped `crates/egraph`" running on root. Contested,
   not vacated.
5. **Island count unresolved; honest floor is 0 fully-instantiated in-tree islands this wave** (A
   cross-lock+possibly-abrogated, B substrate-BLOCKED, C canonicalization). The family survives as a
   *design* whose first instantiation is entirely downstream/cross-lock work.
6. **The manufacturing rewrite's confluence with the all-narrowing set is untested** — the actual
   R-1-null-breaking artifact, shown (§0) non-trivial in the project's own code; PROTO-1 hand-`union`ed.
7. **GN-2.W1 supersession undeclared (clause ix)** — wf30 says Island B is "gated *behind*" GN-2.W1;
   LOCK-9:176 places its four-tier lowering on Island B's plan predicate, so Island B is on LOCK-9's
   critical path. The supersession must be NAMED (replacement / wrapper / waits-on), not "subsumes."
8. **Clause-vi name-gate live in the file 2A restructures.** `has_rule("value")` at
   `codegen/src/runtime_generator.rs:98` is a forbidden name-gate LOCK-3 flags; LOCK-2 is silent.
9. **`regen∘regen` byte-identity for a *plan-reading* consumer untested; ceremonial ~68-field
   provenance deletion (gate v) unexercised.**

**Pass-2 directives:** advance the canonicalization spine (anchored by the legacy exemplar); resolve
the island count to a concrete N with a live consumer *only after* the XL-B tower fork is adjudicated;
carry Islands A/C/B as WITHHELD pending cross-lock cession (§5). Do not island a decision that is a
plain always-apply canonicalization.

### 2B — IR-IS-EGRAPH · BLOCKED · 32%
*Convergence: on-disk critics' floor (CRIT-1 34, CRIT-2 32) → 32. The prior draft's 40 cited stale
numbers (38/42) and reasoned upward from correcting a wrong-tree CRIT-2 that the FINAL CRIT-2 already
corrected itself. No named reason to exceed the floor: feasibility is de-risked by the legacy exemplar,
but distinctive value is triple-refuted and the primacy conflict is with LOCK-3, not 2A.*

Blocked on the real fork (LOCK-2⊗LOCK-3 tower ownership, XL-B) and on genuinely untested skinny gates.
The `Ref`-as-leaf / per-rule root-table insight is sound — and *already realized* in the legacy
`write_back.rs` — raising this from "novel structural risk" to "adopt-and-prove a proven design."

**Open gaps:**
1. **The real fork is LOCK-2 ⊗ LOCK-3, not 2A ⊗ 2B (XL-B, NEW governing finding).** 2B deletes
   `derive_backend_shape` and makes extraction the driver — seizing the seat LOCK-3's charter owns. A
   GESTALT boundary call, mis-scoped as intra-lock.
2. **The distinctive thesis is triple-refuted as inert.** "The IR *becomes* the e-graph; extraction
   *is* the lowering" buys value over 2A's local canonicalization only if extraction chooses among >1
   candidates. The legacy wired rule set is all-narrowing (§0); avg class ≈ aegraph's 1.13 is the
   predicted reality. Absent a manufacturing rewrite, 2B reduces to "GVN/hash-consing for free," which
   2A inherits from the shared engine. aegraph twin predicts residual ~0.1%.
3. **Signature claim rests on an NP-hard repair (research-B risk 1).** "Extraction emits `BackendShape`
   directly" presumes context-free per-class shape, but `admits_sink_only` (`passes/src/lib.rs:954`)
   keys shape legality on the *caller's* target — a sub-rule under a `SinkOnly` caller and an
   `OffsetTape` caller cannot extract one shape for both. Greedy per-class extraction is context-free.
   The only identity-preserving repair — optimal context-sensitive DAG extraction — is **NP-hard,
   inapproximable within any constant ratio** (prior-art [6][7]): a missing primitive of *equal
   difficulty to the original problem*, the BLOCKED-candidate condition. LOCK-3's `DirectTypeKind` is
   Struct-only / sum-node un-emittable — the algebra 2B depends on is itself incomplete. Whether the
   fleet has any caller-conflict is a prototype-answerable `admits_sink_only` audit — **not run**.
4. **Adopt-and-prove is unscoped, and the port DIRECTION is portfolio-contested (XL-A).** The legacy
   exemplar sits in the tree LOCK-1 calls the *retiring base* and LOCK-5 may *abrogate* — so
   PROVE-2B-PORT cannot even name its source of truth until the tree-of-record contradiction is
   resolved.
5. **Skinny compile-budget incl. teardown — untested** against ~230 µs; `CspScheduler` rebuilds a
   fresh `Csp` each iteration (opposite of aegraph's no-rebuild lesson) — a pure-assertion scaling risk.
6. **Skinny regen byte-identity through cost-guided extraction — untested** (total-order tie-break
   obligation; `HashMap` iteration inadmissible). The extraction-crossover exhibit does not exist
   in-IR. AC confluence of one-directional Seq/Alt normal-forming asserted, untested.
7. **PROVE-12-as-rewrite anchored on a RETIRED target + double-homed (XL-C).** `find_component_delim`
   is gone from skinny `codegen/` (disk-confirmed); PROVE-12 generalization is LOCK-3's charter + 3D's
   family. Claimed without naming LOCK-3.

**Pass-2 directives:** the fork is LOCK-2⊗LOCK-3 (GESTALT-B), and the port direction is gated on XL-A
— **resolve both before scoping PROVE-2B-PORT.** Then measure the legacy engine ported into skinny
against budget + regen. Do NOT reopen "make the IR a Language" as greenfield — mine the legacy exemplar.

### 2C — CSP-CENTRAL · BANKED · 30%
*Convergence: on-disk critics' floor (CRIT-1 42, CRIT-2 30) → 30. Banked, not scored on the 42 (which
credited the propagator, now the surviving banked asset); the family-as-primacy takes CRIT-2's 30
floor — as-primacy is Unison-refuted and the honest CONDITIONAL proto + the two-locks-deep consumer
pull it there.*

**Redirect / crowding resolution (both critics concur):** 2C-as-primacy collapses to 2A Island B —
the Unison ceiling (CSS-L4 2875 DTA states ≫ ~946-1000 proven-optimal/function) forbids the monolithic
"one global COP per grammar," and the surviving "bounded post-solve over a small coupled sub-CSP" is
operationally indistinguishable from Island B. **2C-as-primacy is RETIRED; its `bounded_sum`
propagator (tested, RAN, +345 additive) survives as a banked asset folded into Island B.**

**Open gaps (inherited by Island B):**
1. **Budget-binds-on-REAL-costs unproven** — shown only under synthetic Prefilter=300/Exact=100; the
   sites' *real* static-size cost is 2D's territory (SPEC-ONLY). SPEC-ONLY resting on SPEC-ONLY.
2. **Sites source-real but IR-latent + un-budgeted.** Abundant in source (css/pretty=15, css/l4≥10,
   bbnf=2, csv=2) but `SelfLoop`/`Prefilter` absent from the IR and those grammars are not under the
   skinny `generated_loc_budget` (only json is). Modeling them = GN-2.W1.
3. **Consumer BLOCKED two locks deep (XL-D)** — not "GN-2.W1 unbuilt." 2C's teeth await a grammar→DFA
   scan-derivation LOCK-5 owns unbuilt and LOCK-9 9C is BLOCKED on. End-to-end `regen∘regen` through a
   plan-reader is therefore untested (check-json exit-0 proves only the plan changes nothing wired).
4. **Degenerate without 2D** — without a fitted cost table the objective is `static_size_cost`, "a
   CSP-shaped priority cascade with extra steps"; and 2D is SPEC-ONLY.
5. **P1–P8 `PriorityStep` retirement is an unproven masked-fallback** — no selection-equivalence check
   that the constraint form subsumes rather than preserves the cascade.
6. **One-solver-home coordination with LOCK-1 1B unnamed (XL-G).** `bounded_sum` extends the same
   csp-solver LOCK-1's constraint store centralizes; a GESTALT unification point neither map names.

**Underexplored formulation to OPEN (de-crowd):** **PBQP** (research-C prior art — LLVM `RegAllocPBQP`,
Scholz & Eckstein 2002; per-node discrete choice with pairwise coupling cost matrices; "Instruction
Selection Based on SSA-Graphs" companion). The scan-plan/backend-shape decision graph is *near-tree*
(sites independent save the budget + a few coherence edges); PBQP is optimal-on-trees, linear on
sparse graphs, and may be the correct solver shape for the coupled sub-CSP — sidestepping both
generic-B&B cost and the Unison monolith. Nobody prototyped it; everyone reached for e-graph
extraction or generic CSP. Opened as the underexplored middle.

### 2D — PROFILE-CLOSED-LOOP · BLOCKED · 30%
*Convergence: on-disk critics' floor (CRIT-1 30, CRIT-2 33) → 30. SPEC-ONLY, zero executed artifact,
clause vii.*

The loop discipline is sound and correctly precedented (AutoFDO/BOLT/Ansor); the load-bearing
determinism invariant (loop closes at calibrate-time, not regen-time → `regen∘regen` survives) is
non-negotiably right. But every empirical claim is untested.

**Open gaps:**
1. **Zero executed prototype** — PROVE-2D-A (fitted coefficient flips a decision AND wins cold-paired)
   and PROVE-2D-B (freshness goes RED on mutation) never ran. PROTO-1's sign-flip is a *manual weight
   swap*, NOT a fit from measured cyc/B. Under clause vii, 2D cannot anchor a perf claim until one runs.
2. **Attribution underdetermination unresolved** — whole-grammar cyc/B, ~15 equations, dozens of
   features → β unidentifiable. The A/B-differential primitive is *proposed*, chooses neither (the
   elegant-reduction trap: the hard part is punted to "the prototype must choose").
3. **The unblocked plane is cross-lock-owned (XL, NEW sharpening).** 2D's *entire* unblocked plane —
   `select_backend`/`build_lo6_table` kernel crossover (LOCK-9 kernel territory), `CSS_SHUFTI_MIN_SPAN`
   / `GATE=16` (GN-2 scanner knobs) — sits inside LOCK-9/GN land; the plane LOCK-2 owns (shape lattice)
   is §0-BLOCKED. Applicability is exactly where the LOCK-2 generators don't need it AND another lock
   owns the knob. Fitting these knobs supersedes hand-tuned GN literals — a named supersession (ix) the
   spec treats as free attachment.
4. **`from_env` masked-fallback not fenced off the regen path** — dual cost source; if regen reads env
   when the table is absent, `regen∘regen` breaks. Fence env out of regen.
5. **Freshness-gate teeth (gate 4) unbuilt** — "mutate a coefficient → `report.rs:1750` RED,
   `check-calibration` fails" is the anti-provenance-theater guard 2D promises; no proto showed the
   gate can go RED, so the anti-theater claim is itself presently provenance theater.

**Pass-2 directives:** 2D is a **composable layer under whichever generator wins** (not exclusive with
any) — but it must ship ONE executed fit before it may speak of convergence, and it must name the
LOCK-9/GN knob supersessions its only applicable plane touches. Prerequisite: a candidate generator
with real per-site alternatives (Island B's real costs).

---

## 3. Cross-lock contradiction / double-home ledger (CRIT-2's core — the prior draft carried none of this)

The intra-lock incompatibility map (2A⊗2B, 2B⊗2C, 2A~2C) is entirely within LOCK-2. The load-bearing
forks are **cross-lock**, and each lowers the family that touches it. These are the pass's
highest-value new material and become GESTALT-B obligations (§5).

| # | Seam | Contradiction / double-home | Status |
|---|---|---|---|
| **XL-A** | **Tree-of-record** | **LOCK-1** REGISTRY: "Root is the greenfield base; **skinny retires**." **LOCK-3/5/7**: skinny is the live tree of record; legacy `crates/` is **archived by GN-0**. Contradictory across the portfolio. LOCK-2's whole port-INTO-skinny plan (§4 graft 3, PROVE-2B-PORT) assumes skinny is the destination; if LOCK-1's reading holds, the port direction **inverts** and legacy is the base. | **Gates PROVE-2B-PORT's very direction.** Unnamed by LOCK-2 until this pass. |
| **XL-B** | **Middle-IR / decision-driver ownership** | LOCK-3 charter owns "the IR tower … the decision driver's seat." 2A/2B both re-shape it; 2B deletes `derive_backend_shape` and makes extraction the driver. | **The genuine primary fork** (NEW FINDING 2). Framed intra-lock; never adjudicated. |
| **XL-C** | **PROVE-12 residualization** | Double/triple-homed: LOCK-2 (2B residual ruleset / 2A Island C), LOCK-3 charter ("PROVE-12 generalized"), LOCK-3 3D STAGED-RESIDUAL. research-B's `ConstFoldDelimMask` anchors on `find_component_delim` — **RETIRED from skinny codegen** (disk-confirmed). | LOCK-2 claims it without naming LOCK-3 co-owner; anchors on a dead site. |
| **XL-D** | **Scan-derivation substrate** | Triple-homed: LOCK-2 Island B / 2C (`SelfLoop`/`Prefilter` derivation), LOCK-9 9C SCANNER-AS-DFA (**BLOCKED** — enabling substrate owned by LOCK-5, unbuilt), LOCK-5 continuum (grammar→DFA, SPEC-ONLY). | LOCK-2 treats the consumer as one near-term dependency; it is **BLOCKED two locks deep**. Unnamed. |
| **XL-E** | **Regex-canon exemplar** | LOCK-2 Island A wants to *port* legacy `rules/regex.rs` (434 LOC). LOCK-5 P1: skinny "needs no grammar-tier regex e-graph … fold into it **or abrogate**"; LOCK-5 P2 SPEC-ONLY lock-wide. | LOCK-2 treats it as a port asset, not a possible LOCK-5 abrogation target. Half-named. |
| **XL-F** | **Projection canon** | LOCK-2 Island C ("shared engine with LOCK-1") vs LOCK-1 1D + LOCK-1-owned `Project`@204. LOCK-2's "1D vacuous → nothing to share" is a wrong-tree read: LOCK-1's live 1B/1D substrate = "the shipped `crates/egraph`" on root. | Double-homed; resolved only by an unverified tree-read. |
| **XL-G** | **One solver home** | LOCK-2 2C + LOCK-1 1B ONE-CONSTRAINT-STORE ("the csp-solver crate is the engine"). Portfolio GESTALT asks "one solver home?". | `bounded_sum` correctly homed (parsimony credit), but the net-new extension is uncoordinated with LOCK-1's store. Lesser, positive-adjacent. |

---

## 4. Crowding, cross-pollination, and the forks

**RETIRED for crowding:** 2C-as-primacy → folded into 2A Island B (both critics concur; Unison-refuted
monolith). The `bounded_sum` propagator survives as the banked mechanism.

**OPENED to de-crowd:** PBQP as the near-tree solver formulation for the coupled scan-plan sub-CSP
(§2C) — the underexplored middle between e-graph extraction and generic B&B.

**Cross-pollination grafts (both families developed enough; named + justified):**
1. **2C `bounded_sum` → 2A Island B solver.** PROTO-2's tested, RAN propagator *is* Island B's budget
   knapsack. The primary graft; the seam both families always touched. **Coordinate with LOCK-1 1B
   (XL-G) — one solver home.**
2. **2D fitted per-site costs → Island B objective.** Replaces PROTO-2's synthetic Prefilter=300/
   Exact=100 with fitted cyc/B. Gated on 2D running one fit; the fit touches LOCK-9/GN knobs (name the
   supersession).
3. **Legacy `crates/ir/src/egraph/` → 2A/2B skinny port.** A *working, wired, in-project* exemplar of
   `GrammarENode: Language` / `write_back` / `rules/regex.rs` / `analysis/`. **CONDITIONAL on XL-A** —
   the port direction is portfolio-contested; if LOCK-1's tree-of-record reading holds, this graft
   inverts. Its all-narrowing rule set is also the decisive §0 datum (canon-suffices).

**Primary fork — RE-LOCATED (NEW FINDING 2):** the fork is **LOCK-2 ⊗ LOCK-3** (does graph-opt govern
the middle IR's SHAPE, or operate as a tactic within a LOCK-3-owned tower?), **not** the intra-lock
2A⊗2B the prior draft named. 2A⊗2B is a sub-question decided only after the tower's owner is set. This
is a GESTALT-B call, not a LOCK-2 pass.

---

## 5. Pass-2 briefs (what to prototype · what research must answer · what stays withheld)

**CROSS-LOCK ADJUDICATION FIRST (blocks the movers below until GESTALT rules):**
- **XL-A tree-of-record — the top gate.** Resolve LOCK-1 ("skinny retires") vs LOCK-3/5/7 ("skinny is
  the tree of record; legacy archived by GN-0"). PROVE-2B-PORT's direction and graft 3 both invert on
  the answer. Cannot scope the port until this lands.
- **XL-B LOCK-2/LOCK-3 middle-IR ownership** — the genuine primary fork. Does graph-opt own the tower's
  shape or operate within it? Promote to GESTALT-B.
- **XL-D LOCK-2/LOCK-5/LOCK-9 scan-derivation home** — Island B's consumer is BLOCKED two locks deep;
  name the owner.
- **XL-E Island A** — LOCK-5 cession vs abrogation of `rules/regex.rs`. **XL-F Island C** — LOCK-1's
  `Project` + live root egraph. **XL-G** — one solver home with LOCK-1 1B.
- **GN-2.W1 supersession must be NAMED (clause ix)** before Island B lands (replacement/wrapper/waits-on).

**PROTOTYPE (the movers, gated on the adjudications above):**
- **PROVE-2-BRANCH-v2 — the real, non-synthetic §0 test, IN SKINNY.** (a) MEASURE the legacy wired
  grammar e-graph's *actual* average e-class size after saturation on the fleet grammars (it runs at
  `crates/core/.../pipeline.rs:218`) — if ≈1.13, canon-suffices is the honest §0 answer; if
  meaningfully >1, extraction is live. (b) Author a REAL registered `ManufactureDispatchAlt` rewrite
  fired through `CspScheduler` and PROVE it **confluent with the all-narrowing set + terminating** —
  the step both PROTOs and both critics flag as the unbuilt load-bearing core. (c) Re-measure Part B
  cyc/B at REAL value-start positions only (post ws-skip). (d) Do it **in skinny**, not legacy — the
  prior PROTO-1 ran on `bbnf-ir = crates/ir = legacy`.
- **PROVE-2B-PORT — 2B's mover, gated on XL-A.** Port the legacy `crates/ir/src/egraph/` into skinny;
  measure build+saturate+extract+**teardown** against ~230 µs and `regen∘regen` byte-identity through
  cost-guided extraction. Also run the `admits_sink_only` caller-conflict audit (does the fleet even
  have one — if not, the NP-hard objection is moot).
- **PROVE-2C-REALCOST — Island B / 2C teeth on real costs.** Build the `SelfLoop`/`Prefilter` IR
  extension (XL-D-owner-permitting) so sites are IR-modeled; feed the `bounded_sum` CSP GN-2.W1-lowered
  (or 2D-fitted) real static-size + cyc/B; does the budget STILL bind? End-to-end `regen∘regen` through
  a real consumer. Compare B&B vs a **PBQP** formulation on solve cost.
- **PROVE-2D-A/B — 2D's first executed artifact.** Pick `select_backend` kernel crossover
  (`dispatch.rs:89`) or `CSS_SHUFTI_MIN_SPAN`; A/B-differential fit; prove a fitted value flips a
  decision AND wins cold-paired AND drives `report.rs:1750` freshness RED on mutation. Name the LOCK-9/
  GN knob supersession.

**RESEARCH MUST ANSWER:**
- Does the legacy-exemplar port stay in the skinny compile budget + regen fixpoint — and in which
  direction (XL-A)?
- Does any fleet grammar present ≥2 *IR-modeled* competing scan sites with *real* binding costs (source
  says yes; IR says no; who builds the bridge, XL-D)?
- Is PBQP the right solver shape for the near-tree coupled sub-CSP (vs generic B&B / e-graph extraction)?
- Does the fleet have any `admits_sink_only` caller-conflict (moots or activates 2B's NP-hard objection)?

**WITHHELD FROM RESEARCHERS (do not re-open):**
- 2C-as-primacy (RETIRED; Unison-refuted). Researchers receive only the `bounded_sum`-as-Island-B
  solver framing + the PBQP alternative.
- The census: settled (§0) — **both final critics AGREE** skinny has no grammar e-graph; legacy
  `crates/` has a wired one. Do NOT re-audit; do NOT re-litigate as a critic dispute (the prior draft's
  framing was against superseded critic drafts; the on-disk critiques concur).
- Islands A/B/C active prototyping — withheld pending cross-lock cession (§5 adjudications).
- 2A⊗2B as the "primary fork" — it is a sub-question of XL-B; do not treat as the top fork.

---

## Pass history

### Pass 1 (agglomerated from research-{A,B,C,D,priorart}, FAMILY-SPECS, PROTO-1/2 [RAN_MEASURED], the FINAL on-disk CRIT-1/CRIT-2)
- **Correction of the prior draft:** the earlier registry draft was agglomerated from *superseded*
  critic drafts — it cited convergence numbers (2A 55/58, 2B 38/42, 2C 32/50, 2D 40/33) that do NOT
  match the on-disk final critiques (2A 50/50, 2B 34/32, 2C 42/30, 2D 30/33), and framed §0 as a
  two-tree "census dispute" that the FINAL critiques do not have (CRIT-2 re-anchored on disk and
  concurs with CRIT-1). Numbers corrected to the on-disk floor: **2A 50, 2B 32, 2C 30, 2D 30.**
- **Two NEW governing findings folded in** (absent from the prior draft): (1) CRIT-1 — PROTO-1 ran on
  the LEGACY tree (`bbnf-ir = crates/ir`), so the sharpest uncertainty was tested on the wrong
  codebase; the legacy→skinny port is the unbuilt core no family scopes ("adopt-and-prove," not
  "build"/"delete"). (2) CRIT-2 — LOCK-2 mislocates its primary fork: the real fork is **LOCK-2⊗LOCK-3**
  (middle-IR / decision-driver ownership), not intra-lock 2A⊗2B; plus a full cross-lock ledger
  (XL-A..XL-G) the prior draft carried none of.
- **Decisive §0 datum retained:** the legacy tree's own wired rule set is all-narrowing → §0
  triple-confirmed (R-1 null + aegraph 1.13 + in-project all-narrowing set). Canonicalization is the
  value; extraction is inert absent an unbuilt, unproven-confluent manufacturing rewrite.
- **Prototypes RAN_MEASURED:** PROTO-1 → machinery-not-broken (on legacy), NOT extraction-decision-real
  (in skinny); confirmed within noise; the fired/confluent rewrite is still unbuilt. PROTO-2 →
  propagator works + regen byte-identical (of a plan nothing reads), teeth source-real but IR-latent /
  un-budgeted / consumer-less; budget binds only on synthetic costs. Both verdicts softened to the
  critics' floor.
- **Dispositions:** 2A ADVANCED 50 (core spine, cross-lock-contested islands); 2B BLOCKED 32 (net-new
  in skinny, de-risked by legacy exemplar, distinctive value refuted, real fork is XL-B); 2C BANKED 30
  (as-primacy RETIRED → Island B; propagator banked; teeth source-real IR-latent); 2D BLOCKED 30
  (SPEC-ONLY, zero fits, unblocked plane is LOCK-9/GN-owned).
- **Crowding:** retired 2C-as-primacy into Island B; opened PBQP as the underexplored near-tree solver
  formulation. **Primary fork re-located to LOCK-2⊗LOCK-3 (XL-B).** Tree-of-record (XL-A) gates the
  port direction — promoted to the top GESTALT-B obligation.
