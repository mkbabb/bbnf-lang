# LOCK-2 — THE GRAPH ENGINE — pass-1 · CRIT-2 (Adversarial critic 2 of 2, CROSS-LOCK weight)

*Charter: harden/challenge/refine every family against the failure-mode checklist — vacuous
convergence, spec-cites-itself circularity, gates that cannot fail, the elegant-reduction trap,
legacy aliases, masked fallbacks, unverified gestalt, consumer-less substrate — plus constitution
violations and prototype-evidence misuse. Critic 2 weights the CROSS-LOCK view: where LOCK-2
contradicts or double-homes work against the extant GN/PT codex and the other locks. Every
divergence must be a NAMED supersession (Constitution ix).*

**Disk re-anchored this pass (skinny HEAD `fe160a91a`, non-worktree), to avoid the wrong-tree trap
the prior CRIT-2 fell into.** Verified verbatim: `skinny/crates/ir/src/` = **`cost.rs` + `lib.rs`
only** — no egraph module, no `GrammarENode` (2B genuinely net-new in the subject tree);
`ir/src/lib.rs:425-431` `SimdMode{Exact}` / `SimdSite{PreEntry}` **single-valued** (Island B / 2C
sites absent from the IR); `find_component_delim` **gone from `codegen/`** — survives only in a
bench bin + `runtime/src/runtime_scan.rs` (PROVE-12's cited codegen target is RETIRED);
`has_rule("value")` name-gate **live** at `codegen/src/runtime_generator.rs:98`. The within-lock
substrate census (§0, CRIT-1, research-A/B) is correct **for the skinny tree**, and I do not
re-litigate it. My weight is the cross-lock seam, which neither the FAMILY-SPECS incompatibility map
nor the LOCK-2 REGISTRY adjudicates: **the map is entirely intra-LOCK-2 (2A⊗2B, 2B⊗2C, 2A~2C), and
the load-bearing forks are actually LOCK-2 ⊗ LOCK-3, LOCK-2 ⊗ LOCK-5, LOCK-2 ⊗ LOCK-9, and a
lock-portfolio tree-of-record contradiction.**

I concur with CRIT-1's within-lock reasoning and adopt its floor as my starting point; the
cross-lock gaps below are **additional** open gaps, so my convergence numbers sit at or below CRIT-1's.

---

## THE GOVERNING CROSS-LOCK FINDING — LOCK-2 mislocates its own primary fork

The FAMILY-SPECS names its "primary architectural fork" as **2A ⊗ 2B** (§I of the incompatibility
map): "the IR cannot simultaneously be a tree between islands and the e-graph container." That is a
real tension, but it is the **wrong fork**, because *the middle IR is not LOCK-2's to shape*.

- **LOCK-3's charter** (portfolio §LOCK-3): "the IR tower and its transpositions … lowering
  contracts and invariants; residualization (PROVE-12) generalized; **the decision driver's seat**."
- **LOCK-3's REGISTRY** (re-verified at `skinny@b3cb8c16b`): "The tower is **3B-shaped today** — one
  rich `BackendIr`, `BackendShape` an attribute, fewest transpositions; LOCK-3 either **hardens
  (3A/3B) or re-founds (3C/3D)**."

2A ("the middle IR stays an ordinary annotated tree, e-graphs are local island tactics") and 2B
("the middle IR *is* one e-graph container end-to-end; `derive_backend_shape` is deleted; extraction
*is* the lowering") are **both** verdicts on what LOCK-3's tower IS. 2B is, precisely, a **fifth
option imposed on LOCK-3's spine** that LOCK-3's own family set (3A hardens / 3B annotates / 3C
grammar-homoiousia / 3D staged-residual) does not enumerate. The genuine primary fork of this lock
is therefore **LOCK-2 ⊗ LOCK-3: does graph-optimization govern the middle IR's SHAPE (LOCK-2 owns
the tower), or does it operate as a tactic WITHIN a tower LOCK-3 owns?** The FAMILY-SPECS resolves
this silently in LOCK-2's favor by talking about "the middle IR" as if LOCK-2 defines it. That is an
**undeclared supersession** of LOCK-3 (clause ix). Until GESTALT-B adjudicates the tower's owner,
2A's "directed spine" and 2B's "IR-is-egraph" are both **speculative annexations of LOCK-3
territory**, and the intra-lock 2A⊗2B fork is a sub-question of a boundary LOCK-2 has not earned the
right to answer alone. This is the single most load-bearing cross-lock gap and it lowers every
family that touches the middle IR.

---

## Cross-lock contradiction / double-home ledger (the critic-2 core)

| # | Seam | The contradiction / double-home | LOCK-2's handling |
|---|---|---|---|
| **XL-A** | **Tree-of-record** | **LOCK-1** REGISTRY: *"Root is the greenfield base; **skinny retires**"* (quoting `project_skv24_handoff.md`). **LOCK-3/5/7** REGISTRIES: **skinny is the live tree of record**; the legacy `crates/` tree is **archived by GN-0** (LOCK-7 XL-4). These are contradictory across the portfolio. | LOCK-2's whole pass-1 governing correction + its highest-value graft ("legacy `crates/ir/src/egraph/` → **port INTO skinny**", REGISTRY §3.3) + the PROVE-2B-PORT mover **assume skinny is the destination**. If LOCK-1's reading holds, the port direction **inverts** and the legacy egraph is the base, not an asset. **Unnamed by LOCK-2.** |
| **XL-B** | **Middle-IR / decision-driver ownership** | LOCK-3 charter owns "the IR tower … the decision driver's seat." 2A/2B both re-shape it; 2B deletes `derive_backend_shape` and makes extraction the driver. | Framed as intra-lock 2A⊗2B; the LOCK-2/LOCK-3 boundary is **never adjudicated**. Governing finding above. |
| **XL-C** | **PROVE-12 residualization** | Double/triple-homed: LOCK-2 (2B "residual ruleset" / 2A Island C), **LOCK-3 charter** ("residualization (PROVE-12) generalized"), **LOCK-3 3D STAGED-RESIDUAL**. And research-B's `ConstFoldDelimMask` rewrite anchors on `codegen/src/runtime_generator.rs::find_component_delim` — **RETIRED from skinny codegen on disk** (confirmed; LOCK-3 REGISTRY concurs: "retired from `codegen/src`"). | LOCK-2 cites PROVE-12 as its own rewrite target; **does not name LOCK-3 as co-owner**, and anchors on a dead codegen site. |
| **XL-D** | **Scan-derivation substrate** | Triple-homed: LOCK-2 **Island B / 2C** (`SimdSite::SelfLoop`/`SimdMode::Prefilter` recognizer-derivation), **LOCK-9 9C SCANNER-AS-DFA** (BLOCKED — "quote/number/ws scanning become compiled DFA products; hand-written scanners cease to exist"), **LOCK-5 continuum** (owns grammar→DFA; SPEC-ONLY lock-wide per its P2). LOCK-9 9C is BLOCKED *precisely because* "the enabling substrate (grammar→DFA source-of-truth) is **owned by LOCK-5 and unbuilt**." | LOCK-2 treats the consumer as a single near-term dependency ("GN-2.W1 unbuilt"). It is actually **BLOCKED two locks deep** and its *provenance* (hand-written arm vs DFA-derived) is contested. Unnamed. |
| **XL-E** | **Regex-canon exemplar** | LOCK-2's Island A + REGISTRY graft want to **port** legacy `egraph/rules/regex.rs` (434 LOC). LOCK-5 P1: skinny "needs no grammar-tier regex e-graph" is a live option — *"fold INTO it, **or to abrogate**."* LOCK-5 P2: the grammar-driven regex premise is **SPEC-ONLY lock-wide** (no artifact exercised it). | LOCK-2 marks Island A WITHHELD pending LOCK-5 cession (good) but **treats the exemplar as a port asset**, not as a possible LOCK-5 abrogation target. Half-named. |
| **XL-F** | **Projection canon** | LOCK-2 Island C ("shared engine with LOCK-1") vs **LOCK-1 1D TYPES-AS-ECLASSES** + LOCK-1-owned `Project`@`passes/src/lib.rs:204`. LOCK-2 asserts "LOCK-1's 1D is BANKED-vacuous → no shared engine." But LOCK-1 REGISTRY: the **live 1B/1D substrate = "the shipped `crates/egraph`"** running on root. | LOCK-2's "1D vacuous, nothing to share" is a **wrong-tree assertion** (skinny has no egraph; LOCK-1's runs on root). Projection/type canon is **double-homed**, resolved only by an unverified tree-read. |
| **XL-G** | **One solver home** | LOCK-2 2C ("csp-solver is the engine") + LOCK-1 1B ONE-CONSTRAINT-STORE ("every judgment is one constraint system; the csp-solver crate is the engine"). The portfolio GESTALT explicitly asks "one solver home?". | Constitution-aligned (general-infra edict), and `bounded_sum` is correctly homed in csp-solver (parsimony credit). But the **net-new `bounded_sum` extension is uncoordinated** with LOCK-1's constraint-store; neither lock's map names the shared home. Lesser, positive-adjacent. |

---

## Prototype-evidence misuse (cross-lock + method)

- **Two protos for Brief 2 reach different verdicts; the honest one must govern.** `PROTO-2.md`
  (worktree `…-28`) returns **PASS** ("2C's teeth exist on the corpus"). `PROTO-2-wf30.md`
  (worktree `…-30`) returns the **CONDITIONAL** verdict — *"2C teeth LATENT (source-real,
  IR-unmodeled, **un-budgeted**)"* — and its own reconciliation note explains why the PASS
  overclaims: the ≥2 sites are audited in **grammar source**, not in the IR, which models exactly
  one `PreEntry Exact` scan (I confirmed on disk). The REGISTRY correctly took the conditional
  floor; the **PASS headline is prototype-evidence misuse** and must not anchor 2C.
- **Consumer-less substrate reproduced BY the probe (clause viii / Y.13).** Both PROTO-2 variants
  build `bounded_sum` + a scan-plan CSP whose plan **nothing reads** — the consumer
  (SelfLoop-derivation) is unbuilt. Cross-lock sharpening (XL-D): it is not a near-term LOCK-2
  deliverable but a substrate **LOCK-9 9C blocks on and LOCK-5 owns unbuilt**. The brief was meant
  to *test for* the Y.13 pathology; the prototype *instantiates* it two locks deep.
- **Synthetic cost anchoring a corpus claim (clause vii).** `Prefilter=40/Exact=8 LOC`,
  `Exact=10/Prefilter=1` cyc/B are author-chosen; whether the budget "binds" is a pure function of
  those constants. Real costs are 2D's territory (itself SPEC-ONLY, zero fits) — so the binding
  demonstration is SPEC_ONLY resting on SPEC_ONLY.
- **PROTO-1: PASS de-risks "2A/2B/2D simultaneously" is overclaimed.** I concur with CRIT-1 fully
  (hand-`union` not a fired rewrite; W2 is the sign-flip of the calibrated `−2.0`, a gate that
  cannot fail in the direction needed; Part B measures **every corpus byte** — twitter 81% NOMATCH —
  a contrast the parser never runs). Cross-lock addition: PROTO-1's Part B cyc/B is a *real PMU
  number on a synthetic workload*; using it to anchor "**2D-lattice anchored**" is a **directional
  measurement dressed as a decision-grade perf claim** (clause vii). Its own Part B shows the table
  dominates 2.27×–3.34× everywhere → the honest architecture is **canonicalization** (always
  rewrite Alt→table), the FAIL-branch conclusion, not the extraction one.
- **Baseline staleness (cross-lock method inconsistency).** Both protos ran at `d2b32f6e3` —
  **~51 commits behind live** (LOCK-3 verified `git rev-list --count d2b32f6e3..b3cb8c16b = 51`);
  skinny is now at `fe160a91a`, further still. **LOCK-3 and LOCK-7 re-ran their load-bearing greps
  at live HEAD**; LOCK-2's REGISTRY does **not** re-anchor its cost-path cites (`cost_weights.rs`,
  `backend_egraph.rs`, `decision_csp.rs`) against the moved tree. The §0 substrate claims I
  spot-checked still hold at `fe160a91a`, but the un-re-anchored line cites are a method gap the
  sibling locks did not tolerate.

**Non-circularity (credit).** §0 rests on three *independent* legs — the in-tree R-1 54-config null,
the external Cranelift aegraph (avg class 1.13, choice layer 2-of-4M), and the project's own
all-narrowing legacy rule set. Not spec-cites-itself. This is the pass's genuine strength and I do
not discount it. The `bounded_sum` propagator (7 tests, additive, correctly homed) and 2D's
calibrate-time-closure determinism invariant are likewise sound and earn their credit.

---

## Family 2A — EGRAPH-ISLANDS — convergence **50%**

Strongest-aligned; the LLVM directed-spine + local-island architecture is coherent, the legacy
canonicalization exemplar proves the spine ships, and gate (i) genuinely can go RED (credit — not a
gate-that-cannot-fail). But each of its three islands is cross-lock-contested, and the boundary with
LOCK-3 (XL-B) is unadjudicated. CRIT-1's floor (55) does not price the cross-lock island gaps; I
take **50**.

**Enumerated open gaps:**
1. **Middle-IR ownership vs LOCK-3 unadjudicated (XL-B).** 2A's "directed spine, IR is an annotated
   tree between islands" is a verdict on LOCK-3's tower; the incompatibility map never names the
   LOCK-2/LOCK-3 seam. Speculative annexation until GESTALT-B rules.
2. **Island A doubly-blocked cross-lock (XL-E).** LOCK-5 owns the lawful HIR (unrendered) AND may
   **abrogate** the shared `rules/regex.rs` exemplar 2A/2B want to port; LOCK-5 P2: grammar-driven
   regex SPEC-ONLY lock-wide. Island A's only child-bearing extraction depends on a verdict + a
   substrate that another lock has neither rendered nor decided to keep.
3. **Island B sites absent from IR; deriving substrate cross-lock-BLOCKED (XL-D).** `SimdSite={PreEntry}`,
   `SimdMode={Exact}` confirmed on disk at `fe160a91a`. The recognizer-derivation is not "GN-2.W1
   unbuilt" (one dependency) but blocked through LOCK-9 9C (BLOCKED) and LOCK-5 (grammar→DFA
   SPEC-ONLY). "Same-wave consumer that changes emitted code" is not present and not near-term.
4. **Island C double-homed with LOCK-1, and probably not an island (XL-F).** `Project`@204 is LOCK-1's;
   LOCK-2's "1D vacuous → no shared engine" is an unverified wrong-tree read (LOCK-1's egraph runs on
   root). And PROTO-1's table-always-wins implies `map∘map`/dead-projection are always-apply
   canonicalizations → by gate (i) they are plain rewrites, **not** islands.
5. **Manufacturing rewrite still hand-`union`'d, un-fired, confluence-unproven** (PROTO-1 candor);
   island count honest floor is plausibly **0 fully-instantiated islands** (A cross-lock, B
   substrate-blocked, C canonicalization).
6. **Deletion gate unexercised + adjacent name-gate unaddressed.** The 68-field provenance deletion
   (gate v) is stated, not shown; and `has_rule("value")` — the **clause-vi-forbidden name-gate**
   LOCK-3 flags — is **live at `runtime_generator.rs:98`**, the very codegen file 2A restructures,
   yet LOCK-2 is silent on it.

## Family 2B — IR-IS-EGRAPH — convergence **32%**

The `Ref`-as-leaf / per-rule-root-table insight is sound and legacy-realized (credit), but 2B's
signature thesis collides head-on with LOCK-3 and rests on a LOCK-3-owned algebra LOCK-3 says is
incomplete. CRIT-1's 38 is within-lock; the cross-lock primacy conflict and the NP-hard repair pull
it to **32**.

**Enumerated open gaps:**
1. **The real fork is LOCK-2 ⊗ LOCK-3, not 2A ⊗ 2B (XL-B, governing finding).** 2B deletes
   `derive_backend_shape` and makes extraction the driver — seizing the seat LOCK-3's charter owns.
   Mis-scoped as intra-lock; a GESTALT boundary call.
2. **"Extraction emits BackendShape directly" depends on a starved, caller-sensitive LOCK-3 algebra.**
   LOCK-3 REGISTRY: `DirectTypeKind` **Struct-only, sum node algebra un-emittable**. research-B risk
   1 (caller-sensitive shape via `admits_sink_only`) is LOCK-3's domain; its only identity-preserving
   repair is **NP-hard optimal context-sensitive extraction** (open-Q4) — a missing primitive of
   *equal difficulty to the original problem* (the BLOCKED-candidate condition).
3. **PROVE-12-as-rewrite anchored on a retired target + double-homed (XL-C).** `find_component_delim`
   gone from skinny `codegen/` (disk-confirmed); PROVE-12 generalization is LOCK-3's charter item and
   3D's family. LOCK-2 claims it without naming LOCK-3.
4. **Distinctive extraction value triple-refuted inert (§0);** reduces to GVN/hash-consing that 2A
   inherits from the shared engine. aegraph twin predicts residual ~0.1%.
5. **Compile-budget incl. teardown / regen bit-identity / AC-confluence all untested;** and the
   legacy exemplar it would port sits in the tree LOCK-5 may abrogate and LOCK-1 calls the retiring
   base (XL-A) — the port's **direction is cross-lock-undecided**, so PROVE-2B-PORT cannot even name
   its source of truth yet.
6. **Substrate cites un-re-anchored** to `fe160a91a` (~51+ commits past the proto baseline), unlike
   sibling locks.

## Family 2C — CSP-CENTRAL — convergence **30%** (as-primacy BANKED; propagator BANKED)

As-primacy is retired (both critics + Unison ceiling); the surviving form is Island B under another
name. The `bounded_sum` propagator is a genuine banked asset. CRIT-1's 32 stands; the cross-lock
depth of the missing consumer (XL-D) and the honest CONDITIONAL proto pull it to **30**.

**Enumerated open gaps:**
1. **As-primacy Unison-refuted** (CSS-L4 2875 DTA states ≫ ~946-1000 proven-optimal/function); the
   monolithic global COP is off the cliff by construction. Surviving "bounded post-solve over a small
   coupled sub-CSP" = 2A Island B — **not a distinct family**.
2. **Teeth consumer-less (viii) + synthetic-cost (vii); the honest proto governs.** PROTO-2-wf30's
   CONDITIONAL verdict (source-real, IR-latent, un-budgeted) is the truth; PROTO-2's PASS overclaims.
   On disk: one `PreEntry Exact` scan per grammar, no `SelfLoop`/`Prefilter`.
3. **The consumer is cross-lock BLOCKED two locks deep (XL-D)**, not "GN-2.W1 unbuilt." 2C's teeth
   await a grammar→DFA scan-derivation LOCK-5 owns unbuilt and LOCK-9 9C is BLOCKED on.
4. **Degenerate without 2D's fitted costs** (named dependency, itself SPEC-ONLY); the **P1–P8
   PriorityStep retirement is an unproven masked-fallback** (no selection-equivalence check that the
   constraint form subsumes rather than preserves the cascade).
5. **One-solver-home coordination with LOCK-1 1B unnamed (XL-G).** `bounded_sum` extends the same
   csp-solver LOCK-1's constraint store centralizes; a GESTALT unification point neither map names.

## Family 2D — PROFILE-CLOSED-LOOP — convergence **33%**

The loop discipline and the calibrate-time-closure determinism invariant are correct and
non-negotiable (credit). But zero fit ran (clause vii), and — the cross-lock sharpening — its
only applicable knobs live in LOCK-9 / GN scanner-kernel territory. REGISTRY's 33 stands.

**Enumerated open gaps:**
1. **Zero executed fit (clause vii, SPEC-ONLY).** Gate (1) — a fitted value flips a decision AND
   beats the hardcoded lane cold-paired — has no evidence; PROTO-1's sign-flip is a manual weight
   swap, not a fit.
2. **Attribution underdetermination named, neither primitive built** (A/B-differential vs sub-parse
   probes) — the elegant-reduction trap: the hard, unconverged core is punted to "the prototype must
   choose."
3. **The null-trap-escape knobs are cross-lock-owned.** `select_backend` / `build_lo6_table` kernel
   crossover (`bbnf-simd/src/dispatch.rs`) is **LOCK-9 kernel territory**; `CSS_SHUFTI_MIN_SPAN` /
   `GATE=16` are **GN-2 scanner knobs**. 2D's *entire* unblocked plane sits inside LOCK-9/GN land —
   so "the coefficient plane under any LOCK-2 generator" is advertised over a surface LOCK-2 does not
   own, and the shape-lattice plane (the one LOCK-2 owns) is BLOCKED by §0. Applicability is
   **exactly where the LOCK-2 generators don't need it AND another lock owns the knob**.
4. **Freshness-gate teeth (gate 4) unbuilt** — the anti-provenance-theater claim (`report.rs:1750`
   RED-on-mutation, `check-calibration` fails) is itself, presently, provenance theater.
5. **`from_env` dual cost-source not fenced off the regen path** — a masked fallback that breaks
   `regen∘regen` if the committed table is absent.

---

## Constitution & failure-mode audit (summary)

- **ix Extant-tranche consciousness — VIOLATED in multiple places.** GN-2.W1 supersession undeclared
  (Island B: replacement / wrapper / waits-on?); LOCK-3's ownership of the tower + PROVE-12 not named
  as co-home; the tree-of-record contradiction (XL-A) unnamed. Divergences must be **named
  supersessions**, not silent annexations.
- **viii Consumer-less substrate — TRIPPED by both PROTO-2 variants** (plan nothing reads), and the
  consumer is blocked two locks deep (XL-D).
- **vii Concrete-or-SPEC-ONLY — PROTO-2 synthetic-cost + PROTO-1 synthetic-workload cyc/B** are used
  to anchor corpus/decision claims; both need the SPEC-ONLY label they currently lack.
- **iv Refutation laws — honored in form** (ratios not absolutes, aarch64-only, no warm benches) but
  PROTO-1 ran at loadavg ~10 on an unrepresentative every-byte workload — a representativeness defect
  distinct from load noise.
- **vi Parsimony / general-infra — CLEAN:** `bounded_sum` correctly homed in csp-solver.
- **Gate-that-cannot-fail:** PROTO-1's W2 sign-flip. **Elegant-reduction trap:** the fired,
  confluent `ManufactureDispatchAlt` rewrite (2A/2B) and 2D's attribution primitive — both the
  unconverged hard part, declared "trivial/next-step."

## Lock-level disposition (critic-2 recommendation)

LOCK-2's honest converged core is **2A's directed canonicalization spine + the existing BackendShape
driver**, anchored by the legacy exemplar — *but it is a core LOCK-2 cannot ratify alone*, because
the middle IR it operates on is LOCK-3's (XL-B), the scan sites it optimizes are LOCK-9/LOCK-5's
(XL-D/E), the projections it canonicalizes are LOCK-1's (XL-F), PROVE-12 is LOCK-3's (XL-C), and the
tree it ports into is portfolio-contested (XL-A). Every one of LOCK-2's four families reaches across
a lock boundary the incompatibility map does not draw. The lock should return as a **rigorously
converged CORE with its cross-lock seams promoted to explicit GESTALT-B obligations**: (1) the
LOCK-2/LOCK-3 middle-IR ownership fork; (2) the LOCK-2/LOCK-5/LOCK-9 scan-derivation home; (3) the
LOCK-1/LOCK-2 projection + one-solver-home unification; (4) the portfolio tree-of-record
contradiction, which gates the PROVE-2B-PORT mover's very direction. Extraction/CSP-extraction stays
**SPEC-ONLY pending a fired, confluent, non-sign-flipped rewrite on a representative workload**;
scan-plan CSP/budget stays **SPEC-ONLY pending an IR-modeled site + a live consumer no single lock
currently owns**. No family may speak of convergence before those seams are named supersessions and
one real fit / one fired rewrite runs.
