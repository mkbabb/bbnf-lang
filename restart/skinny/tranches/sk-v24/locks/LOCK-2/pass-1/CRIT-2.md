# LOCK-2 — THE GRAPH ENGINE — pass-1 CRIT-2 (adversarial, cross-lock weighted)

*Critic 2 of 2. Charter: harden every family against the failure-mode checklist (vacuous
convergence, spec-cites-itself, gates-that-cannot-fail, elegant-reduction trap, legacy aliases,
masked fallbacks, unverified gestalt, consumer-less substrate) + constitution violations +
prototype-evidence misuse; weight the CROSS-LOCK view. No CRIT-1 exists in this lock's pass-1 dir —
this is the first adversarial audit. Read: PORTFOLIO, FAMILY-SPECS, PROTO-1/2 (+ patch/bench),
research-{A,B,C,D,priorart}, and on-disk substrate + adjacent-lock FAMILY-SPECS (1,3,5,9).*

---

## 0. THE FINDING THAT RE-CUTS THE WHOLE LOCK — the synthesis (and both researchers) audited the WRONG dead substrate

Every report in this lock — research-A, research-B, priorart, and the synthesis §0 — grounds the
lock on one census claim: *"the sole `impl Language` today is `DecisionNode`; the grammar IR is not
yet a Language; making it one is family 2B's work; `Ref`-as-leaf is the single most important
structural decision in 2B; the inert decision substrate is `backend_egraph.rs`'s childless bag."*
**This census is false on disk, and I verified it three ways this pass:**

1. `crates/ir/src/egraph/node.rs:23-24` — `#[derive(..., Language)] pub enum GrammarENode`. The
   grammar IR **already implements `Language`.** research-A's "sole `impl Language` is
   `DecisionNode`" (research-A §0) and research-B's "the sole `impl Language` in the tree is
   `DecisionNode` (`backend_egraph.rs:181`)" and "`ExprKind` cannot derive at all … 2B is precisely
   the work of making that claim true" (research-B §2) are **all wrong**.
2. `crates/ir/src/egraph/` is a full module tree: `build_egraph.rs`, `write_back.rs` (14.5 KB),
   `interner.rs`, `cost.rs`, `rules/` (incl. `rules/regex.rs`), `analysis/`. `write_back.rs`'s own
   header: *"After saturation, the e-graph contains every equivalent form of each rule's body. This
   module walks each rule's root e-class via `Extractor` (cost-model-guided), rebuilds a concrete
   `IrNode` tree … Rule boundaries are preserved: … emits `Ref(Y)` instead of inlining … essential
   for preserving mutual recursion (JSON: value↔object↔array↔value). An additional per-walk
   `visiting` set guards against non-rule-rooted cycles."* **This is, verbatim, research-B §1.1's
   "load-bearing seam of family 2B" — `Ref`-as-leaf, per-rule root table, acyclic body extraction,
   cycle guard — declared net-new, already built.**
3. LOCK-2's OWN PROTO-1 imports `crates/ir/src/egraph/cost.rs:109-118` and builds
   `EGraph<GrammarENode, NoAnalysis>` — the prototype used the very Language impl the synthesis says
   does not exist. The lock's evidence contradicts the lock's census.

**Two consequences, both decision-grade:**

- **(A) The real Y.13 consumer-less substrate is uncatalogued.** `crates/ir/src/lib.rs:17` exposes
  `pub mod egraph`, but a tree-wide grep finds **zero consumers outside the `ir` crate** — `passes/`
  drives the hand-rolled `derive_recognizers`/`derive_backend_shape`, never `write_back`/
  `build_egraph`. So a complete grammar e-graph (intern → saturate → extract → write-back, with
  recursion and cycle handling) sits **fully built and never called on the production path.** THAT
  is the constitution-viii / Y.13 dead substrate the lock exists to redeem — an order of magnitude
  larger than the ~68 provenance fields §0 fingered. The synthesis's headline "inert decision
  substrate" (the `backend_egraph.rs` childless bag) is a real but *secondary* symptom; the primary
  dead substrate is an entire unused optimizer. No family names it. **This omission must be closed
  before convergence talk: the lock's own diagnosis is incomplete.**

- **(B) 2B is mis-scoped by a stale census; its convergence must drop, not because it is hard but
  because the synthesis measured the wrong distance.** 2B is not "make the IR a Language" (done) or
  "the `Ref`-as-leaf decision" (done) — it is "wire the extant unused grammar e-graph into the
  pipeline in place of the hand-rolled passes, and prove adoption (i) stays in the ~230 µs budget
  incl. teardown, (ii) is `regen∘regen` byte-identical through cost-guided extraction, (iii) changes
  at least one output for the measurable better." All three are UNTESTED. The "80% substrate-built"
  claim is right by accident; the residual is exactly the three hard questions, none prototyped.

This finding is cross-lock (§5 below): the same unused `crates/ir/src/egraph/` tree is what LOCK-5
5A/5B calls the grammar-side of its coproduct Language, and what LOCK-1 1D calls "the template."
Three locks describe the same built-but-unused module; none states who owns turning it on.

---

## 1. THE TWO MASTER-GATE PROTOTYPES, RE-AUDITED — both PASS verdicts are softer than the binary label

### PROTO-1 (Brief 1, §0 master gate) — the elegant-reduction trap, and a mis-distributed measurement

**The gate asks (verbatim): does *any rewrite* create an e-class holding >1 candidate.** PROTO-1
built the two-form e-class by **direct `union`, not by firing a rewrite** — its own candor: *"the
construction is hand-authored, the extraction is production … a productionized
`ManufactureDispatchAlt` rewrite … is the trivial next step."* This is the elegant-reduction trap:
the load-bearing step — a *widening* rewrite that fires under the scheduler — is declared trivial
and left unbuilt. It is not trivial, because the extant rewrite set is **all-narrowing**
(`DeduplicateAltBranches`, `UnionMergeAlt`, `FuseAltRegexBranches`, `CommonSuffixFactor`,
`SupersetAbsorbAlt`); a manufacturing rewrite that adds the dispatch twin must be shown **confluent
with the narrowing set** (does `DeduplicateAltBranches` immediately collapse the twin back? does the
extraction tie-break stay total?) and **terminating under `CspScheduler`**. PROTO-1 bypasses the
rewrite set entirely, so the one thing R-1's null was actually about — that the *set* only narrows —
is untouched. The gate proves the *payoff exists given a branch*; it does **not** prove *a rewrite
manufactures the branch*. Weak PASS.

**The cyc/B magnitude is measured on the wrong byte distribution.** Part B classifies *every corpus
byte* (twitter: 514,197 NOMATCH of 631,514 — **81 % of measured bytes are not value-starts**), so
the sequential classifier eats its full 7-compare fall-through on 4-of-5 bytes. At real value-start
positions the mix is mostly `"`/digit (early in grammar order), where dispatch's one-load advantage
over 2–3 early compares is *far* thinner. The report waves this off ("direction … not sensitive")
with **no measurement at real positions.** The 2.28–3.34× is therefore a synthetic-stream artifact;
the *direction* is plausible (enough for a directional §0 pass), but the **magnitude must not anchor
any downstream cost claim** — yet the synthesis cites it as 2D's "real front to reason about" and
"a scalar weight sign-flip flips the pick." Prototype-evidence misuse if 2D leans on the numbers.

**The de-risking claim over-reaches.** PROTO-1 verdict: "anchored — 2A-Islands-A/C, 2B-extraction,
2D-lattice simultaneously." The artifact is a **grammar `Alt` dispatch-table** decision — that is
Island-B / backend-shape flavored, **not** Island A (regex-HIR canon) or Island C (projection
canon), which are the two islands whose entire justification is "nodes with children → real
extraction." Islands A and C received **zero evidence.** FAMILY-SPECS 2A open-question (1) — "does
the fleet have a real regex leaf whose prefilter/DFA verdict changes under canon?" — is asked and
left **unanswered by its own probe**, yet the verdict marks 2A-Islands-A/C "anchored." The claim and
the artifact are mismatched.

### PROTO-2 (Brief 2, budget) — a *conditional* proof over unmodeled sites with invented costs

PROTO-2 is the stronger of the two: real fleet audit (CSS-L4 ≥10, BBNF ≥3 competing self-loop
sites — refutes §0's "JSON's one whitespace loop" counterexample for the corpus), a genuinely
net-new `bounded_sum` propagator (7 tests, 111 no-regression), forces `Exact` on 3 sites under a
tight budget, byte-identical slack-budget parity. But its own asterisks gut the "teeth exist on the
corpus" claim to a **conditional**:

- **The sites are latent-in-source but UNMODELED by the IR.** `ir::SimdSite` has only `PreEntry`,
  `ir::SimdMode` only `Exact`; `derive_recognizers` collapses every structural byte into ONE
  `SimdScan{Exact,PreEntry}` per grammar. The ≥10 CSS sites the CSP ran over **do not exist in the
  IR** — the per-loop `SelfLoop` recognizer-derivation is GN-2.W1, unbuilt. The CSP was built over
  *grammar-source-audited* sites, not IR sites.
- **The costs are invented.** Prefilter=40 LOC, Exact=8 LOC, and all per-site cyc/B are *synthetic
  inputs, not measurements* (the report says so). Whether the budget **actually** binds depends
  entirely on the *real* static-size costs — unknown until GN-2.W1 lowering exists and 2D fits them.
  If real Prefilter lowering is ~12 LOC, the budget may never bind. So "budget binds on the corpus"
  is proven only up to a fabricated cost vector. This is the family's actual teeth, and it is open.
- **`regen∘regen` byte-identity — the constitution's hard gate — is NOT tested.** "full `cargo xtask
  regen` is not exercised (no consumer reads the plan yet)." Determinism is shown at the CSP
  decision layer only. And "no consumer reads the plan yet" is itself the Y.13 flag: **2C/Island B's
  consumer does not exist** — it is GN-2.W1's unbuilt lowering. A plan nothing reads is dead
  substrate; PROTO-2 built a plan with no reader.

Net: PROTO-2 unblocks the **machinery** (propagator works, forces Exact, parity holds); it leaves
open **binding-on-real-costs**, **IR site modeling (GN-2.W1)**, **the consumer**, and **end-to-end
regen determinism**. The synthesis's post-PROTO verdict language ("2C's teeth exist on the corpus")
is a category jump from what ran.

---

## 2. FAMILY 2A — EGRAPH-ISLANDS · convergence 58%

Strongest-aligned, and the directed-shell architecture is coherent. But three of its own open
questions are open, its two child-bearing islands (A, C) have zero evidence, and it double-homes.

**Enumerated open gaps:**
1. **Island A is un-evidenced and double-homed with LOCK-5.** No prototype touched a regex leaf;
   FAMILY-SPECS open-Q(1) is unanswered. Worse, "2A owns the rewrite set + cost model; LOCK-5 owns
   the lawful HIR form" collides with LOCK-5 5A ("one `derive(Language)` on `ExprKind` gives one
   saturation engine for regex + grammar canonicalization") and 5B (joint coproduct Language +
   regex algebra rewrites). `crates/ir/src/egraph/rules/regex.rs` (434 LOC) — the regex rewrite set
   over the grammar e-graph — **already exists** and is claimed by both LOCK-2-2A-IslandA and
   LOCK-5-5A/5B. Who owns the regex-canon rewrite set is unadjudicated across the two locks.
2. **Island C shares an engine with a BANKED LOCK-1 family, and contests `Project` ownership.**
   Island C = "projection canonicalization (shared engine with LOCK-1)." LOCK-1's 1D
   (types-as-eclasses) is **BANK — cost-extraction VACUOUS**, and LOCK-1 §5.1 insists **one store
   owns the `Project` discard-site**. If LOCK-1 lands 1A (union-find, not an e-graph), there is *no
   LOCK-1 e-graph to share*, and `Project` lives in a union-find store while Island C wants to
   rewrite `map∘map⇄map` over the same nodes in an e-graph — a direct two-owner conflict on
   `Project`. The "shared engine, distinct island (cost objective is the split)" boundary is a
   **name-gate** unless LOCK-1's e-graph actually ships, which its own disposition says it will not.
3. **GN-2.W1 supersession is undeclared (constitution ix).** Island B "subsumes and generalizes
   GN-2.W1's hand-written detection arm," and PROTO-2 calls the unbuilt SelfLoop derivation "exactly
   the GN-2.W1 consumer." Is Island B a **replacement** for GN-2.W1, a **wrapper** that lands after
   it, or does it **wait on** it? The extant codex already homes the prefilter recognizer +
   `find_member` lowering in GN-2.W1. Per clause ix every divergence is a *named* supersession;
   "subsumes and generalizes" is not a disposition.
4. **The manufacturing rewrite's confluence with the narrowing set is untested** (§1) — the actual
   R-1-null-breaking artifact.
5. **`regen∘regen` byte-identity untested end-to-end** for any island (PROTO-2 shows CSP-layer
   determinism only).
6. **Island count (3/2/1) unresolved** — open-Q(3), by design deferred, but a converged family must
   answer it.

## FAMILY 2B — IR-IS-EGRAPH · convergence 42%

Re-scoped by §0: the structural work is **built and unused**, not net-new. VALUE-AT-RISK stands, but
for a different reason than the synthesis gives.

**Enumerated open gaps:**
1. **Stale substrate premise (unverified gestalt).** "2B is the work of making the IR a Language"
   and "`Ref`-as-leaf is the single most important structural decision in 2B" are already in
   `crates/ir/src/egraph/{node.rs,write_back.rs}` (§0). 2B must be re-authored as "adopt the extant
   unused grammar e-graph into the pipeline," not "build it."
2. **The unused engine is itself the dead substrate (Y.13)** — uncatalogued (§0-A). 2B's real
   deliverable is turning it on with a consumer, or deleting it; the family names neither.
3. **Compile budget incl. teardown — untested.** research-B risk #5 (the `with_capacity` teardown
   warning on ~300-node grammars) is real and unmeasured; no proto stood the full IR up as one
   graph and timed build+saturate+extract+teardown against ~230 µs.
4. **regen determinism through cost-guided extraction — untested.** The `FxHashMap best` + cost-tie
   total-order obligation (research-B risk #4) is asserted, never demonstrated at grammar scale.
5. **"Does extraction buy anything" — only weakly evidenced.** PROTO-1's hand-union (not a firing
   rewrite) on a backend-dispatch `Alt`, with a mis-distributed cyc/B, is the sole datum; the
   aegraph twin (1.13 avg class, 2-of-4M, ~0.1 %) predicts ~nothing. 2B's *distinctive* value beyond
   the GVN/hash-consing 2A/2C also inherit is unshown.
6. **Double-home with LOCK-5 5A on `ExprKind: Language`** — both claim the grammar-IR-as-Language
   work (which is in fact done); the coproduct-vs-grammar-only shape is unadjudicated.

## FAMILY 2C — CSP-CENTRAL · convergence 50%

PROTO-2 raised this from the synthesis's "BLOCKED-candidate" by building the propagator, but the
*teeth* (budget binds on real costs, with a real consumer) remain open.

**Enumerated open gaps:**
1. **Budget-binds-on-REAL-costs unproven** — the binding was shown only under synthetic
   Prefilter=40/Exact=8 (§1, PROTO-2). The one fact that justifies the whole family is untested
   against real static-size costs.
2. **The decision sites are unmodeled in the IR** (`SimdSite::SelfLoop`/`SimdMode::Prefilter`
   absent) — the CSP ran over grammar-source sites, not IR sites; modeling them is GN-2.W1, unbuilt.
3. **Consumer-less as it stands** — "no consumer reads the plan yet." 2C's consumer is GN-2.W1's
   `lower_to_rust` reading the plan; until that lands, 2C is the exact Y.13 pathology the lock
   condemns, one level up.
4. **End-to-end `regen∘regen` determinism untested** (CSP-layer only).
5. **Objective degenerate without 2D** — named self-dependency; without a fitted cost table the
   objective is `static_size_cost`, "a CSP-shaped priority cascade with extra steps."
6. **Monolithic-vs-bounded-post-solve scope unresolved** — the Unison ceiling (~946–1000 instr/fn;
   CSS-L4 has 2875 DTA states) forbids the maximal "one global COP per grammar" reading; 2C is
   lawful only as a bounded post-solve over a small coupled sub-CSP, which is **not** the family's
   stated architecture. The spec concedes this in its verdict but keeps the maximal framing in §2.

## FAMILY 2D — PROFILE-CLOSED-LOOP · convergence 33%

**SPEC-ONLY.** No prototype ran (only Briefs 1 and 2 have PROTO files; PROVE-2D-A/B were never
executed). Under clause vii, a family carrying perf claims with zero running artifact and zero
measured numbers is exactly "status report / vague optimism." The loop is elegantly specified and
every seam is located, but nothing was demonstrated.

**Enumerated open gaps:**
1. **Zero executed prototype** — PROVE-2D-A ("a fitted coefficient flips a decision AND wins
   cold-paired") and PROVE-2D-B ("freshness goes RED on a mutated coefficient") are unrun. 2D is
   SPEC-ONLY; per clause vii it cannot anchor any perf claim until one runs.
2. **Attribution underdetermination unresolved** — whole-grammar cyc/B, ~15 equations, dozens of
   features → β unidentifiable (research-D risk #2). The proposed A/B-differential primitive is
   *proposed*, not built; without it the fit is ill-posed.
3. **No knob shown to have a fitted-optimum ≠ current-value with a measured win** — the entire
   "null-trap escape" (kernel crossover, `CSS_SHUFTI_MIN_SPAN`, GATE=16, capacity) is asserted from
   demonstrated *spread elsewhere*, never demonstrated to *flip* here.
4. **`from_env` masked-fallback not gated off the regen path** — "keep `from_env` (`BBNF_COST_*`)
   for sweeps" alongside `from_fact_table` is two cost sources; if regen ever reads env when the
   table is absent, `regen∘regen` breaks. The spec does not fence env out of the regen path.
5. **The lattice plane stays BLOCKED** — 2D over the backend-shape lattice reproduces the R-1 null
   (PROTO-1's weak, hand-unioned pass does not lift it, since no firing rewrite exists); 2D is
   unblocked *in principle* only at thresholds, unmeasured.

---

## 3. GATES-THAT-CANNOT-FAIL / VACUOUS-CONVERGENCE audit

- **The two master facts are labeled resolved by binary PASS, but both PASSes are conditional**
  (§1). §0 says "de-risks or refutes simultaneously"; the prototypes de-risked the *payoff-given-a-
  branch* (weakly) and the *machinery-given-synthetic-costs* (conditionally), not the two facts as
  stated (a *rewrite* manufactures a branch; the budget binds on *real* costs). Treating them as
  closed is premature-convergence.
- **The born-RED gates are inherited from GN-2.W1** (≥8.0× skip, ≥0.95× dense). Good gates — but
  they gate *GN-2.W1's* consumer, which is unbuilt; a gate on an unbuilt consumer cannot fail this
  wave. LOCK-2 must either land the consumer in-wave or mark the gate as GN-owned, not LOCK-2-owned.
- **No spec-cites-itself circularity detected** in the family logic — the reports cite external
  twins (aegraph 1.13/2-of-4M, Unison 946-1000) and in-tree measurements (R-1 sweep). This is a
  strength; the discipline is real.

## 4. LEGACY-ALIAS / MASKED-FALLBACK audit

- **2D `from_env` + `from_fact_table`** — dual cost source, fallback not fenced (2D-gap 4).
- **The ~68 provenance fields** are correctly slated for deletion by 2A/2B — good, no legacy kept.
  But the far larger dead code — `crates/ir/src/egraph/` unused — has no deletion-or-adoption
  disposition (§0-A). Silence on built-but-unused code is the masked-fallback failure at module
  scale: it will read as "we have an e-graph" while nothing calls it.

## 5. CROSS-LOCK LEDGER (critic-2 weighting) — where LOCK-2 double-homes or contradicts

1. **The grammar e-graph (`crates/ir/src/egraph/`) is TRIPLE-HOMED and no lock owns turning it on.**
   LOCK-2-2B ("make the IR a Language" — already done), LOCK-5-5A/5B (grammar side of the coproduct
   Language; `rules/regex.rs`), LOCK-1-1D ("the grammar tier is the template"). All three describe
   the same built-but-unused tree; none states the owner of wiring it into `passes/`. **GESTALT-B
   must assign single ownership** or it stays dead in all three.
2. **`Project` / projection canonicalization double-home: LOCK-2-2A-Island-C ⊗ LOCK-1.** LOCK-1 §5.1
   demands one store owns `Project`; its e-graph family (1D) is BANKED-vacuous, so the likely
   LOCK-1 winner (1A) is union-find. Island C's "shared engine with LOCK-1" premise then has no
   engine to share — Island C either becomes a solo LOCK-2 e-graph over `Project` (contradicting
   LOCK-1's single-store rule) or does not exist. **Unadjudicated contradiction.**
3. **Regex-canon rewrite set double-home: LOCK-2-2A-Island-A ⊗ LOCK-5-5A/5B.** `rules/regex.rs`
   already exists; both locks claim the regex canonicalization rewrite set. LOCK-5 is the
   constitutionally-designated owner of the lawful HIR form (clause ii); LOCK-2 claiming "the
   rewrite set + cost model" needs LOCK-5's explicit cession, absent today.
4. **Kernel-crossover cost knob TRIPLE-touched: 2D ⊗ LOCK-9 ⊗ LOCK-3-3D.** `select_backend`
   (`bbnf-simd/dispatch.rs:89`, the N*≈3 crossover) — 2D fits it, LOCK-9 owns the kernels, LOCK-3-3D
   owns the monomorph-vs-table staging ("LOCK-2 owns the *schedule*, 3D owns the *staging
   semantics*"). LOCK-3 cleanly cedes the *choice* to LOCK-2 — the one well-drawn seam — but the
   *fitting* of the crossover (2D) vs the *kernel* (LOCK-9) vs the *cost-model backend-
   parameterization* (3D-Q3, "a single division is not backend-portable") is a three-way seam the
   gestalt must sequence.
5. **`CSS_SHUFTI_MIN_SPAN` / GATE=16 are extant-GN-codex knobs (GN-2.W1/W3).** 2D fitting them
   supersedes hand-tuned literals — a named supersession is required per clause ix; the spec treats
   them as free attachment points.
6. **LOCK-9 → LOCK-2 handoff is CLEAN** (the one to keep): LOCK-9 line 176 explicitly selects its
   four-tier lowering "by a plan predicate (the LOCK-2 scan-plan-selection e-graph island)." LOCK-9
   defers scan-plan selection to LOCK-2 Island B. This is the model the other seams should follow —
   but it also means LOCK-2 Island B is now on LOCK-9's critical path, raising the cost of Island B
   collapsing to "GN-2.W1's hand arm."

## 6. CONSTITUTION check

- **viii / Y.13 (consumer-less substrate forbidden):** VIOLATED in the *diagnosis* — the largest
  dead substrate (`crates/ir/src/egraph/`, zero consumers) is uncatalogued (§0-A); and PROTO-2 built
  a plan with no reader (2C-gap 3). The lock condemns the pathology while sitting on its biggest
  instance.
- **vii (concrete artifacts or SPEC-ONLY):** 2D is SPEC-ONLY and must be labeled so; PROTO-1's cyc/B
  magnitude must not anchor perf claims (§1).
- **ix (extant-codex consciousness):** GN-2.W1 subsumption and the `SHUFTI_MIN_SPAN`/GATE=16 fits are
  supersessions stated as "subsumes/attaches," not named supersessions (§2-gap 3, §5-5).
- **iv (refutation laws), v (zero-cost types), regen-fixpoint:** no violation in principle; the
  fixpoint gate is simply *untested end-to-end* everywhere (2A-5, 2B-4, 2C-4).

---

## 7. VERDICT SUMMARY

The lock's architecture is sound and its two prototypes are honest and well-built — but the pass
converges on a **false substrate census** that mislocates the dead code (missing the entire unused
grammar e-graph), reads two *conditional* prototype PASSes as *closed* master facts, and leaves four
cross-lock double-homes (the grammar e-graph, `Project`, the regex-canon rule set, the kernel knob)
unadjudicated. 2A is the defensible core once Islands A/C get evidence and the GN-2.W1 supersession
is named; 2C's machinery is real but its teeth (real-cost binding + a live consumer) are open; 2B is
mis-scoped and must be re-cut around the built-but-unused engine; 2D is SPEC-ONLY and cannot converge
until one PROVE-2D artifact runs. **No family is at 100%; the lock returns as a converged CORE (2A
directed-shell + 2C bounded_sum propagator, both anchored by running code) with the exact gaps
enumerated above.** The single highest-value pass-2 directive: **catalogue and dispose of
`crates/ir/src/egraph/` — adopt it (that IS 2B/2A's real deliverable) or delete it — before any
further extraction/CSP design.**
