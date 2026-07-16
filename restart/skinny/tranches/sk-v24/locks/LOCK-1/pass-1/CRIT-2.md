# LOCK-1 — THE SOLVER SPINE · pass-1 · ADVERSARIAL CRITIC 2 of 2 (cross-lock weight)

*Critic 2 verifies FAMILY-SPECS + the two PROTO probes against source, runs the
failure-mode checklist, and weights the cross-lock view: where LOCK-1 contradicts or
double-homes work against the extant GN/PT codex and the other locks. Verdicts are
grounded in the working tree as it stands 2026-07-16, not in the reports' word.*

---

## 0. THE FINDING THAT RE-FRAMES THE LOCK A SECOND TIME — TWO LIVE WORKSPACES, an undecided greenfield base, and only two of four families prototyped

FAMILY-SPECS §0's decisive move is that the four charters reference **two different code
lineages**. Verified this pass — and both are **real, git-tracked, separately-buildable cargo
workspaces**, not one live and one deleted:

- **`skinny/` is its own workspace** (`skinny/Cargo.toml`: members `crates/bbnf, grammar, ir,
  passes, codegen, runtime, parse-that-regex, …`). The 1A/1C substrate is **present exactly as
  cited**: `skinny/crates/passes/src/lib.rs:204` is verbatim
  `ExprKind::Project { body, .. } => self.infer_expr(*body)?` (the discard); `TypeFacts` at
  `:126` with `subst` `:129` and `obligations` `:130`; `infer_expr` at `:178`. The "dormant
  HM(X) scaffold, populated by nothing" is a **true description of code that exists** — so 1A's
  claim that activating it is *simultaneously the mechanism AND a Y.13 repair* is **correct**,
  and I retract the earlier draft's "archived/void" framing entirely.
- **The root workspace** (`/Cargo.toml`: members `crates/core, ir, egraph, csp-solver, …`; the
  active `cargo xtask regen` tree) carries the **live 1B/1D substrate**: root
  `crates/ir/src/passes/types/constraint/*` (the running CSP) and `crates/egraph` (the shipped
  e-graph). These run and ship, verified.

So the honest §0 is not "one tree is dead" — it is **two live workspaces, and the greenfield
(SK-V24 GESTALT-B) must be cut from ONE of them.** That is **THE LINEAGE GAP (X1)** and it is
the lock's dominant unresolved question. It is not decided anywhere in the pass artifacts, and
it inverts the cost accounting:

- If the greenfield is cut **from root** (the active regen workspace), 1A/1C's "activate the
  dormant `skinny` scaffold" targets a workspace that **is not the base** — they must port
  PROVE-08 into root, where the 1B CSP already runs (the HARD-INCOMPATIBLE 1A⊥1B collision at
  full displacement cost).
- If cut **from skinny** (and note project memory SK-V22: *"skinny graduates to tree of record,
  legacy archives"* — a direct tension with FAMILY-SPECS §0 calling skinny the *retired*
  lineage), then 1B/1D's "already runs, don't rebuild" argues from a workspace being archived,
  and 1B/1D must be **ported** into the graduating skinny tree — so "it already runs" stops
  being a free win.

FAMILY-SPECS §0 names both lineages but **decides neither**, and the SK-V22 graduation memo
points the opposite way from its "retired/live" labels. Until LOCK-7/GESTALT-B fix the base,
half the lock's families are scored against a tree that may not survive.

**The second structural fact (this one stands unqualified): only two of four families were
prototyped, and neither is 1A or 1C.** PROTO-1 instruments the live root CSP (1B's engine);
PROTO-2 instruments the shipped `crates/egraph` (1D's engine). **1A and 1C have zero
running-prototype evidence** — under Constitution vii they are **SPEC_ONLY**, and FAMILY-SPECS
never labels them so. Worse for the verdict: the one probe on the **recursion axis** (PROTO-2)
vindicated **1D's** e-graph cycle, not 1A's nominal fold — yet FAMILY-SPECS concludes the lock
"converges to **1A-trunk (+1C layer)**," handing victory to the two un-prototyped families and
retiring the two that ran. That is not disqualifying for 1A/1C on the merits, but it means the
convergence claim rests on unmeasured families and must be scored down accordingly.

---

## FAMILY 1A — UNIFICATION-FIRST · convergence 52%

**What is genuine (verified).** The HM(X)/OutsideIn(X) stratification is the field's settled
answer (research-priorart §2A/§5); the union-find leaf core is proven (PROVE-08
CONFIRMED_DELTA); the dormant `TypeFacts` scaffold really exists in `skinny/crates/passes`
(`:126` `subst`/`obligations`, `:204` the discard) so the "activation = mechanism + Y.13
repair" claim is **sound**. On paper it is the endorsed spine, and its substrate is present.

**Enumerated open gaps:**
- **G1A-1 [lineage-contingent: activates the *retired* workspace, cost inverts if greenfield ≠
  skinny].** 1A's seam catalog is entirely in the **`skinny/` workspace** (a separate cargo
  tree, `skinny/Cargo.toml`). If GESTALT-B cuts the greenfield from **root** (the active regen
  workspace), the "activate the dormant scaffold" mechanism targets a non-base tree and becomes
  "port PROVE-08 into root and **displace the running 1B CSP**" — the full HARD-INCOMPATIBLE
  1A⊥1B collision, not a scaffold flip. 1A's apparent cheapness is **contingent on the
  unresolved X1 lineage decision** and is not free if the base is root. **Unresolved (cross-lock).**
- **G1A-2 [no prototype for 1A's OWN recursion story — the recursion probe tested a rival].**
  FAMILY-SPECS gate (i) — "recursive `value` infers one 6-variant `Sum`, terminating, via the
  nominal `Rule(id)` fold" — is the make-or-break both 1A and 1D name first. **PROTO-2 proved
  it for 1D's e-graph cycle, and its finding (3) explicitly says native cyclicity *beats* 1A's
  nominal fold ("1A would need an explicit nominal binder + fold"). 1A's fold guard is asserted,
  never run**, and the only recursion evidence in the pass argues *against* it. SPEC_ONLY.
- **G1A-3 [row discipline still unpicked].** Leijen scoped-labels vs Gaster–Jones lacks —
  FAMILY-SPECS marks "Unresolved"; still unresolved. "records, informally" re-derives Wand's
  principal-type bug (priorart §2C trap 2).
- **G1A-4 [generalization criterion "named, not stated"].** The value-restriction criterion
  (priorart trap 4) is still not written down; low-risk but a real open item.
- **G1A-5 [the width-defaulting justification rests on a manipulated census].** 1A's "width
  residue is a defaulting lattice, not a CSP" leans entirely on PROTO-1's THIN verdict —
  which is gate-manipulated and measures the wrong engine (X3/X4). 1A inherits that
  weakness: its cheapest-residue argument is not soundly established.

**Why not higher:** the endorsed spine has real dormant substrate but **no prototype**, the one
recursion probe favored the rival engine, and its whole cost model is contingent on an
undecided cross-lock lineage call. Sound on paper, unproven in tree.

---

## FAMILY 1B — ONE-CONSTRAINT-STORE · convergence 58%

**What is genuine (and verified at source).** 1B is the **only family that runs in the
live tree** — `TypeDomain: LatticeDomain`, per-former propagators, `ObligationSink`,
`HeterogeneousAltJoin`, both backend consumers, end-to-end tests. That is not speculative
substrate; it ships. And the pass's **one confirmed defect is real** — I read it verbatim:

```
crates/ir/src/passes/types/constraint/domain.rs  (TypeDomain::join)
  (Some(existing), Some(ty)) if existing != ty => {
      // Re-assignment to a different type: allow it (the solver
      // may refine types during convergence).
      self.solved = Some(ty.clone());  true
  }
```

**R-B2 CONFIRMED** — last-writer-wins on a genuine unification clash: non-monotone (not a
lattice join), order-dependent, a silent overwrite where `no-silent-epsilon` demands a raised
obligation. FAMILY-SPECS's sharpest concrete finding, and it holds.

**Enumerated open gaps:**
- **G1B-1 [R-B1 store-shape STILL open, and NO probe touched it].** Federation vs single
  sum-domain — FAMILY-SPECS: "needs a prototype, not paper." **No prototype was run.**
  PROTO-1 counted residue; it never built the decode-class-depends-on-sum-variant
  counterexample either way. The decision the family "cannot make on paper" is exactly as
  unmade after pass-1 as before.
- **G1B-2 [the "1B collapses to 1A" retirement is NOT soundly established].** FAMILY-SPECS
  leans on PROTO-1 to fold 1B toward 1A. But (X3) PROTO-1's verdict is gate-manipulated —
  **both** pre-registered gates fail on all four corpora — and (X4) it measures 1B's own
  impoverished pass, which has **zero overload/legality code** (grep-verified: no
  `overload`/`escape`/`legality` in `crates/ir/src/passes/types/`), so the rich-residue
  thesis is tested against an instrument that cannot exhibit it. **Retiring the running
  type-system-of-record on that census is unjustified.** This is the inverse elegant-reduction
  trap: the "and-then-collapse-to-1A" step is unconverged.
- **G1B-3 [R-B4 residue-richness — the whole distinctness axis — remains contingent].**
  1B's distinctness from 1A rides entirely on residue richness. That axis is the one PROTO-1
  claims to close and does not (X3/X4). Genuinely **open**.
- **G1B-4 [no consumer-side verification the R-B2 fix is backward-safe].** Fixing `join` to
  fail-explicit changes live shipping code read by both backends and asserted by
  `types_heterogeneous_alt`. No probe confirmed the corpus still solves once refinement is
  reserved for bottom→ground. Named, untested.

**Why the highest of the four:** strongest evidence base (runs + one verified bug + a real
census), but its *disposition* (collapse to 1A) is the least soundly reached.

---

## FAMILY 1C — BIDIRECTIONAL-LOCAL · convergence 38%

**What is genuine.** Bidirectional check/synthesize improves error locality (priorart §2D);
`classify_arrow` is a two-line bidirectional seed; the `Infer`/`infer_expr` substrate it splits
exists in `skinny/crates/passes` (verified). As a *diagnostics discipline layered on the winning
engine* the idea is sound.

**Enumerated open gaps:**
- **G1C-1 [lineage-contingent substrate + no prototype].** Same as 1A: every seam
  (`synth`/`check` split on `Infer`, `RuleSig`, `PassDiagnostic` span-add, the `Project`
  mode-switch at `:204`) lives in the separate **`skinny/` workspace** — real, but the *retired*
  lineage (X1). No prototype ran against it; if the greenfield is cut from root, the seams move.
- **G1C-2 [the make-or-break experiment was NOT run].** FAMILY-SPECS names R-C2 the family's
  make-or-break and calls it "measurable (Brief candidate)": *does 1C localize a diagnostic
  where unify-first does not?* **No diagnostic head-to-head was prototyped in pass-1.** The
  single experiment that justifies 1C's separate identity is unexecuted — so 1C's distinctness
  is **asserted, not measured**, a direct Constitution-vii breach if advanced as more than a
  footnote on 1A.
- **G1C-3 [R-C2 self-admitted degeneracy].** On generic `json.bbnf` (zero `->`) 1C ≡ 1A. Its
  value exists only on typed grammars — and PROTO-1's own numbers show heterogeneous residue
  ≤2% even on CSS L4, i.e. very few sites where checking-against-a-declared-type differs from
  synthesis. The distinctness surface is small *and* unmeasured.
- **G1C-4 [R-C4 subtype-join-not-equality at the seam / R-C3 sub-span granularity].** Both
  design-fixed, neither built; in root the diagnostics type differs from the archived
  `PassDiagnostic` R-C5 targets, so even the substrate gap is mis-addressed.

**Why lowest:** real but retired-lineage substrate, no prototype, and the one experiment that
would earn it a name (R-C2 diagnostic head-to-head) was skipped. It is currently a footnote on
1A wearing a family charter until that measurement runs.

---

## FAMILY 1D — TYPES-AS-ECLASSES · convergence 48%

**What is genuine (and it is the best-evidenced family on prototype).** PROTO-2 RAN and
MEASURED: injectivity = unification holds (a–d), the width+injectivity fixpoint converges in
2 passes ≤ bound with no oscillation (e), and native cyclicity yields a finite μ-representative
for free via the shipped visited-set. On the **recursion axis** 1D is genuinely ahead of 1A's
nominal fold — that is a real, measured result. Credit where due.

**But the headline claim is FALSIFIED, and the cross-lock cost is masked.**

- **G1D-1 [SEVERE — "zero new engine" is FALSE, verified in the patch].** FAMILY-SPECS line
  254 and research-D §0 both claim 1D reaches all three walls with **"zero new engine — only a
  third `Language`."** PROTO-2's own patch **modifies the shared engine**
  `crates/egraph/src/egraph.rs::union`: it deletes the `if changed { analysis_pending.push }`
  guard and pushes to `analysis_pending` **unconditionally on every merge** (+30/−4, plus a
  `rebuild_pass_count` field). I confirmed both the current guarded form and the patch's
  unconditional form. **1D requires a core-engine semantic change; the "third Language only"
  claim does not survive its own existence proof.** FAMILY-SPECS's 1D disposition (VIABLE-AT-
  RISK, "~30 LOC classical primitive on a shipped engine") **predates and does not reflect
  this** (FAMILY-SPECS 03:49 < PROTO-2 04:10) — the registry must be corrected.
- **G1D-2 [CROSS-LOCK BLAST RADIUS — "free" is asserted, never measured].** The amended
  `union()` is on **LOCK-2's `GrammarENode` and LOCK-5's `RegexENode` hot saturation path**.
  Every union in those tiers now unconditionally enqueues `analysis_pending`, re-canonicalizes,
  and calls `modify` (a no-op for `GrammarAnalysis`/`NoAnalysis`) — extra worklist churn per
  union across two other locks. PROTO-2 verified **correctness** ("37 passed") and **explicitly
  makes no perf claim**. The "free for no-op analyses" line is a claim, not a measurement.
  **This is a masked cross-lock cost** (Constitution vi parsimony; refutation-law posture on
  unmeasured perf). 1D's disposition **cannot be settled inside LOCK-1** — it needs LOCK-2's
  sign-off on the engine change.
- **G1D-3 [AC-soundness carried as an unbuilt design obligation].** PROTO-2 finding (4):
  positional injectivity is **unsound for associative-commutative constructors** (`Sum`); the
  probe stayed sound only by making the sum-rewrites confluent so injectivity "lies dormant on
  `Sum`." A production 1D **must tag constructors injective-vs-AC and gate the hook** — unbuilt.
  The existence proof's soundness rests on the test never holding two rival same-arity sums, a
  coincidence of the fixture, not a mechanism. The retirement condition wasn't hit, but the
  soundness carrier isn't built either.
- **G1D-4 [the idle-machinery raison-d'être is STILL unmet — PROTO-2 defeats the wrong
  retirement].** 1D's sharpest caveat (priorart §2F): equality saturation's value is
  extraction-by-cost over multi-valued forms, and pure HM has a unique principal type — nothing
  to extract. PROTO-1 confirms **overload/cost residue = 0** in all four corpora → the cost
  extractor has nothing to rank. **PROTO-2 retired only the *recursion* condition (native
  cyclicity beats the nominal fold); it did NOT retire the *residue/idle-machinery* condition.**
  FAMILY-SPECS itself says two convergent negatives retire 1D — a thin residue **and** a
  sufficient fold — and PROTO-1 delivers the thin residue. So the correct reading is: **1D's
  distinctive engine is idle; its only proven edge (free sum+recursion) is precisely what
  FAMILY-SPECS says 1A reaches more cheaply.** PROTO-2 makes 1D "sound and elegant," not
  "necessary." Elegance earns nothing.

**Why not lower:** it is the only family with a real, ran-and-measured prototype, and the
recursion win is genuine. Why not higher: its charter claim is falsified, its cross-lock cost
is unmeasured, and its reason to exist over 1A is unmet by the very probe that "advanced" it.

---

## CROSS-LOCK & CONSTITUTION FINDINGS (critic-2 weight)

- **X1 — THE LINEAGE GAP (the lock's foundational hole; cross-lock LOCK-7/GESTALT-B).** Which
  tree is the greenfield cut from? **Both are live, separately-buildable cargo workspaces**
  (verified): `skinny/Cargo.toml` (1A/1C's `passes`/`codegen`, the dormant scaffold — present)
  and the root `/Cargo.toml` (1B/1D's `ir`/`egraph`, the active regen tree). FAMILY-SPECS §0
  labels skinny "retired" and root "live", **but project memory SK-V22 says the opposite —
  "skinny graduates to tree of record, legacy archives."** So the direction is not merely
  undecided, the two authorities *conflict*. The decision (LOCK-7 self-hosting depth /
  GESTALT-B's port list) determines whether 1A/1C activate the base or a side tree, and whether
  1B/1D "already runs" is in-scope or must be ported. If the greenfield is skinny, 1B/1D must be
  **ported** and 1B's "resurrecting a second core is waste" collapses (you port either way); if
  root, 1A/1C's mechanism becomes a PROVE-08 port that must **displace** the running 1B CSP.
  **This must be resolved before LOCK-1 convergence and it is not a LOCK-1-local question.**

- **X2 — C4 voided without a NAMED SUPERSESSION (Constitution ix breach).** FAMILY-SPECS §0
  declares prior-art C4 / GN-3 §1.5 ("no CSP or e-graph consumer exists in this tranche") "void /
  factually false." GN-3 §1.5 is a **committed adjudication in the extant codex**. Constitution
  ix: every divergence from the GN codex is a *named supersession*, never a silent contradiction.
  Calling a committed verdict "factually false" inline is a correction, not a supersession. The
  GN-3 §1.5 override must be recorded as a formal supersession (with the GN-3 doc amended), or the
  lock violates its own constitution clause. **Procedural, unresolved.**

- **X3 — PROTO-1 is a gate that cannot fail (failure-mode: manipulated gate).** Both
  pre-registered gates FAIL on all four corpora: `eq+sum ≥95%` lands 82–89%; `structural-non-eq
  ≤2` lands 18 / 1020 / 53 / 87. The census reaches "THIN — decisive" by **discarding both and
  substituting a third gate** (`genuine-multi-valued ≥15% ⇒ RICH`) chosen post-hoc *because* it
  yields THIN — and even prints the re-cut instructions for a future probe. The conclusion may
  well be right, but the **honest pass-1 output is INCONCLUSIVE on the registered gates + a
  proposed re-cut to confirm in pass-2**, not a decisive retirement of 1B and 1D. As written it
  is the elegant-reduction trap: "measure the residue" converged, "and therefore retire two
  engines" did not.

- **X4 — PROTO-1 measures the wrong engine for the residue question (circularity).**
  `overload = 0` and `decode-class = one-fixed-per-site` are **artifacts of the live pass not
  modeling overload/legality** — grep-verified: `crates/ir/src/passes/types/` contains no
  `overload`, `escape`, or `legality` code. The families' rich-residue thesis (decode-class as an
  escape-conditioned *overload* — research-A §1.7.2, research-B §4.4) is tested against an
  implementation that **structurally cannot produce that residue**. A census cannot refute a
  thesis about judgments its instrument does not implement. The residue axis is **not closed.**

- **X5 — 1D↔LOCK-2 is DOUBLE-HOMED, not "substrate-shared."** The incompatibility map scores
  1B⊥1D as "engine-exclusive, substrate-shared, resolvable by subordination under LOCK-2."
  PROTO-2 shows 1D **modifies** LOCK-2's engine (X5=G1D-1/G1D-2), it does not merely consume it.
  Subordination is the wrong frame: 1D's engine amendment lands in LOCK-2's core and affects
  LOCK-5's regex tier. The disposition of 1D's substrate cannot be "banked under LOCK-2" quietly;
  the union() change is a LOCK-2/GESTALT-B adjudication with a required perf measurement.

- **X6 — protocol: convergence-talk is premature.** One pass, no fresh non-author audit yet
  (this critique is the first), zero families with two consecutive clean passes. Per the pass
  protocol, no family may be called converged; the FAMILY-SPECS "SYNTHESIS VERDICT" reads as a
  near-closure and should be down-graded to "pass-1 disposition, all four open."

---

## VERDICT (critic 2)

The lock is **less converged than FAMILY-SPECS presents**, and the gap is not in the physics
but in the bookkeeping and the evidence-fit:

1. **The lineage is undecided AND the two authorities conflict (X1)** — 1A/1C target the skinny
   workspace, 1B/1D the root workspace; both are live, and FAMILY-SPECS ("skinny retired") and
   SK-V22 ("skinny graduates to tree of record") point opposite ways. This is the gating
   cross-lock unknown; nothing converges until GESTALT-B/LOCK-7 fix the base.
2. **The verdict rests on un-prototyped families:** it crowns 1A/1C (real substrate, but zero
   prototype, and the sole recursion probe favored their rival 1D) and retires 1B (runs) and 1D
   (measured) — on a census (PROTO-1) that is gate-manipulated (X3) and measures the wrong
   engine (X4).
3. **1D's charter claim is falsified** (zero-new-engine is false; G1D-1) and its cross-lock cost
   is unmeasured (G1D-2) — its registry disposition predates its own probe and must be corrected.
4. **1B carries the one verified bug** (R-B2, confirmed at source) but its retirement is unsound.
5. **1C's make-or-break was never run** (G1C-2).

**Pass-2 must:** (a) get LOCK-7/GESTALT-B to fix the greenfield lineage before any family is
scored; (b) re-run the residue census on an instrument that *models* overload/legality, with the
registered gate honored or formally re-cut in advance, not post-hoc; (c) measure PROTO-2's
`union()` amendment against LOCK-2/LOCK-5 saturation (the "free" claim), and route the engine
change through LOCK-2; (d) run the 1C diagnostic head-to-head that pass-1 skipped; (e) record the
GN-3 §1.5 override as a named supersession. **No family is BLOCKED; none is converged.**

### Convergence table
| Family | conv % | disposition | the one gap that dominates |
|---|---:|---|---|
| 1A UNIFICATION-FIRST | 52% | VIABLE-ON-PAPER | no prototype; recursion probe favored rival 1D; cost inverts on the undecided lineage (G1A-1/G1A-2/X1) |
| 1B ONE-CONSTRAINT-STORE | 58% | VIABLE, runs | retirement-to-1A rests on a manipulated, wrong-engine census (G1B-2/X3/X4) |
| 1C BIDIRECTIONAL-LOCAL | 38% | OVERLAY-UNPROVEN | make-or-break diagnostic head-to-head never run; retired-lineage substrate (G1C-1/G1C-2) |
| 1D TYPES-AS-ECLASSES | 48% | VIABLE-AT-RISK | "zero new engine" falsified; cross-lock cost unmeasured; idle-machinery unmet (G1D-1/G1D-2/G1D-4) |
