# LOCK-1 — THE SOLVER SPINE · pass-1 · ADVERSARIAL CRITIC 2 of 2 (cross-lock weight)

*Critic 2 re-verifies FAMILY-SPECS + all four PROTO probes against the working tree
(2026-07-16), runs the failure-mode checklist, and weights the cross-lock view: where
LOCK-1 contradicts or double-homes work against the extant GN/PT codex and the other locks.
This pass supersedes the earlier CRIT-2 draft (04:22), which — like CRIT-1 and FAMILY-SPECS
itself — predates the two decisive probes that ran afterward (PROTO-1B-clash 04:48,
PROTO-2-recursion-parity 05:01) and therefore repeated a factual error those probes correct.*

---

## 0. THE FINDING THAT DOMINATES THIS PASS — the specs are STALE, and their sharpest
## concrete claim is DEAD CODE

FAMILY-SPECS §7 names **two** decisive open questions and declares them **"both un-run this
pass"**: Brief 1 (CSP-equality soundness — does `TypeDomain::join` swallow a clash?) and
Brief 2 (1A-nominal-fold vs running-CSP recursion parity). **Both have since run**
(`PROTO-1B-clash.md` 04:48; `PROTO-2-recursion-parity.md` 05:01), and their results are **not
folded into FAMILY-SPECS (04:41)** — which is therefore overtaken by its own briefs. Worse,
each brief **falsifies a load-bearing claim the specs assert as settled**:

**(a) The "confirmed shipping defect" is dead code — verified at source.** FAMILY-SPECS
elevates `TypeDomain::join` (`constraint/domain.rs:72-77`) to the crux of the whole lock: it
is *1B.O1 "the confirmed shipping defect"*, the *§5.1 incompatibility-map* cell ("1B CSP
lattice … overwrite-unsound as shipped"), *1A.O1* ("Wand's overwrite bug … already ships in
the root join"), and *Brief 1's* named artefact. CRIT-1 §0.2 and the prior CRIT-2 both
"CONFIRMED" it. **I grepped it: the only `.join(` call in `crates/ir/src` is
`FactWidth::join` (`egraph/analysis/facts.rs:223`). `TypeDomain::join` (domain.rs:66-80) has
ZERO call sites in the type pass.** The monotonic solver
(`csp-solver/src/solver/monotonic.rs:29-44`) drives propagation purely through
`constraint.revise(...)` and never calls `domain.join`. **The single most-cited concrete
finding in the lock is a defect in unreachable code.** PROTO-1B caught this; the specs, both
prior critics, and Brief 1 did not.

**(b) The REAL carrier is `assign`, and its true hazard is worse but unreachable.** The live
equality carrier is `assign` (`revise.rs:27-35`, verified: `*slot = Some(ty)` with no clash
check), *also* last-writer-wins — under a monotonic loop with **no iteration cap** (verified:
`loop { … if !changed { break } }`). Two incompatible grounds on one var would **flip-flop
forever** (non-termination), strictly worse than "silently pick a leaf." **But** the tree IR
generator never places two incompatible *grounds* on one scalar var; the ONLY reachable clash
site — the `Alt` join — routes through `join_types` → `HeterogeneousAltJoin` + a named
`TypeObligation`, i.e. **raise-on-disagreement**. PROTO-1B ran the live pass on `Bool` vs
`F64` arrows and got `ir.types[0] = HeterogeneousAltJoin([Bool,F64])` **PASS**. Net: the live
1B pass is **sound on the reachable surface**; the unsoundness FAMILY-SPECS trumpets is (i)
mis-attributed to dead code and (ii) a latent-only hazard in `assign` that clashes never
reach. This *softens* 1B's "unsound as shipped" indictment to "latent, unreachable" — and
removes the specs' main lever for "1B must be repaired, ceding to 1A."

**(c) The 1D disposition is contradicted by the fresher of two conflicting probes.**
`PROTO-2.md` (strawman: 1D-vs-unbuilt-1A-nominal-fold) → **"keep 1D."** FAMILY-SPECS's 1D
disposition ("BANK — narrow recursion-axis keep") rests on it. `PROTO-2-recursion-parity.md`
(1D-vs-the-**running** CSP) → **"RETIRE 1D."** The two reach **opposite verdicts on the same
retire-or-keep question**, the parity probe **explicitly flags owner-adjudication**, and
FAMILY-SPECS reflects only the strawman one. So even 1D's "recursion-axis watching brief" is
already contested by the newest evidence.

**Consequence.** FAMILY-SPECS reads as a near-closed synthesis ("the surviving spine is 1A +
defaulting lattice + 1C folded in"), but it is a **snapshot taken one brief-cycle too early**:
its confirmed defect is void, its 1D keep is contradicted, and its two decisive briefs have
answers it never absorbed. No family may be scored on the specs' own framing; each is scored
below on the *tree as it stands* plus all four probes.

---

## FAMILY 1A — UNIFICATION-FIRST · convergence 55%

**What is genuine (verified).** HM(X)/OutsideIn(X) stratification is the field's settled
answer (research-priorart §2A/§5); the union-find leaf core is proven (PROVE-08); the dormant
`TypeFacts` scaffold really exists in `skinny/crates/passes` (`:126` `subst`/`obligations`,
`:204` the `Project` discard — verified verbatim), so "activation = mechanism + Y.13 repair"
is **sound**. **New, favorable:** PROTO-2-recursion-parity shows the *running* type pass
closes recursion **nominally** (`BoxedEnum`/`Enum` knot on sight) and **terminates + stays
finite** on JSON, Sheets (mutual), and CSS L4 — i.e. a nominal fold, which is 1A's own
mechanism, is *empirically proven to work in tree*. 1A's make-or-break recursion gate is now
**indirectly evidenced** (the incumbent already is a nominal fold that terminates), where
before it was pure assertion.

**Enumerated open gaps:**
- **G1A-1 [lineage framing in the specs may be INVERTED against the governing memo —
  cross-lock].** FAMILY-SPECS §0 calls root "the live lineage the greenfield is cut from" and
  skinny the "older lineage." **Project memory SK-V22 says the opposite and governs:
  "skinny graduates to tree of record … the greenfield totality replaces it"** (verified in
  `project_skv22_graduation.md`). If SK-V22 holds, 1A's substrate is in the **graduating**
  tree (its cost accounting *improves*, not "activates a retired side-tree" as prior CRIT-2
  feared) — **but the specs' entire cost model is then built on a reversed premise.** The
  direction is not merely undecided; **two authorities conflict**, and LOCK-1 cannot be scored
  until GESTALT-B/LOCK-7 fix the base. Unresolved, and the specs are on the wrong side of the
  memo as written.
- **G1A-2 [no *direct* 1A prototype; recursion evidence is second-hand].** The nominal
  `unify(Rule i, Rule j)=i==j` fold was never built as 1A code. Its recursion story is now
  *supported* by the running incumbent (G1A above) and *attacked* by PROTO-2-strawman ("native
  cyclicity beats the nominal fold"). 1A rides on other families' probes; SPEC_ONLY on its own.
- **G1A-3 [row discipline still unpicked].** Leijen scoped-labels vs Gaster–Jones lacks —
  FAMILY-SPECS marks "Unresolved (currently unmeasurable, no typed grammars)"; still open.
  Note the specs' own justification ("Wand's bug already ships in the root join") is now void
  (§0a) — so the row-discipline urgency is *lower* than the specs imply, not higher.
- **G1A-4 [generalization/value-restriction criterion "named, not stated"]** (priorart trap 4).
  Low-risk (rules are pure schema producers) but unwritten.
- **G1A-5 [the width-defaulting justification still leans on a mis-gated census].** 1A's "the
  residue is a defaulting lattice, not a CSP" rests on PROTO-1's THIN verdict, which is
  gate-manipulated (X3) and measures an engine with no overload/legality code (X4). 1A's
  cheapest-residue argument is *plausible and prior-art-consonant* (research §2G) but not
  soundly established on the pass's own instrument.

**Why 55%:** the endorsed spine, substrate present, recursion now indirectly evidenced by the
running nominal fold — but no direct prototype, row discipline open, and a cost model whose
lineage premise contradicts the governing memo.

---

## FAMILY 1B — ONE-CONSTRAINT-STORE · convergence 55%

**What is genuine (verified at source).** 1B is the **only family that runs in the live
tree** — `TypeDomain: LatticeDomain`, per-former propagators, `HeterogeneousAltJoin`, both
backend consumers, end-to-end tests. That ships. **But the pass's "one confirmed defect" is at
dead code (§0a), and PROTO-1B shows the live pass is actually SOUND on every reachable clash.**
This *removes* the defect FAMILY-SPECS leaned on and simultaneously *weakens* the "1B must be
hardened, ceding to 1A" argument.

**Enumerated open gaps:**
- **G1B-1 [the "confirmed defect" is mis-cited — spec factual error].** FAMILY-SPECS 1B.O1,
  §5.1, and Brief 1 all name `TypeDomain::join` as the shipping unsoundness. It is **dead
  code** (verified). The real carrier `assign` is last-writer-wins under an uncapped loop — a
  **latent hazard unreachable for clashes under well-formed tree IR** (the only clash site,
  `Alt` join, correctly raises). The specs must be corrected: as shipped, on real grammars, 1B
  raises on disagreement. The hardening PROTO-1B recommends (record `TypeClash`, refuse the
  overwrite in `assign`) is still worth doing, but as latent-hazard defense, not "Wand's bug in
  production."
- **G1B-2 [R-B1 store-shape STILL open, and NO probe touched it].** Federation vs single
  sum-domain — FAMILY-SPECS: "needs a prototype, not paper." No prototype was run; PROTO-1
  counted residue, PROTO-1B tested the clash arm — neither built the
  decode-class-depends-on-sum-variant counterexample either way. The decision the family
  "cannot make on paper" is exactly as unmade after pass-1.
- **G1B-3 [the "1B collapses to 1A" retirement is NOT soundly earned — and its main prop just
  fell].** The specs fold 1B into 1A via (i) PROTO-1's THIN census and (ii) "its equality is
  unsound as shipped, so fixing it = becoming 1A." Prop (ii) is now void (§0b: sound as
  reachable). Prop (i) is gate-manipulated (X3) on an instrument with no overload/legality code
  (X4). **Retiring the running type-system-of-record on a mis-gated census + a void soundness
  claim is not earned.** Genuinely open.
- **G1B-4 [no consumer-side verification the `assign` hardening is backward-safe].** Adding
  clash-raising to `assign` changes live code read by both backends and asserted by
  `types_heterogeneous_alt`. No probe confirmed the corpus still solves once overwrite is
  refused. Named, untested.

**Why 55% (tied with 1A, down from prior 58%):** strongest evidence base (runs + a real
census + a ran clash probe), and its live soundness is *better* than the specs claimed — but
its disposition (collapse-to-1A) lost its principal justification this cycle and its
store-shape decision is still on paper.

---

## FAMILY 1C — BIDIRECTIONAL-LOCAL · convergence 40%

**What is genuine.** Bidirectional check/synthesize improves error locality (priorart §2D);
`classify_arrow` is a two-line bidirectional seed; the `infer_expr` substrate it splits exists
in `skinny/crates/passes` (verified). As a *diagnostics discipline layered on the winning
engine* the idea is sound, and FAMILY-SPECS correctly demotes it to FOLD (not an engine
family).

**Enumerated open gaps:**
- **G1C-1 [substrate now in the GRADUATING tree, but still no prototype].** Every seam
  (`synth`/`check` split, `RuleSig`, `PassDiagnostic` span-add, the `Project` mode-switch at
  `:204`) lives in `skinny/` — which, per SK-V22, is the graduating tree, not a retired one
  (a correction to prior CRIT-2's G1C-1). So the lineage exposure is *smaller* than earlier
  thought — but **no diagnostic prototype ran against it**.
- **G1C-2 [the make-or-break experiment was NOT run — Constitution vii].** FAMILY-SPECS G2/
  R-C2 is the family's make-or-break: does `check_expr` localize `TypeError.span` at the
  `Project` span where unify-first reports at a synthesized interior? **No diagnostic
  head-to-head was prototyped in pass-1** (PROTO-1/1B/2/2-parity all test other axes). 1C's
  distinct value is **entirely unmeasured** — the pass's most conspicuous omission, unchanged
  across both brief-cycles.
- **G1C-3 [self-admitted degeneracy, now sharpened by numbers].** On generic `json.bbnf`
  (zero `->`) 1C ≡ 1A. PROTO-1 shows heterogeneous residue ≤2% even on CSS L4 — very few sites
  where checking-against-a-declared-type differs from synthesis. The addressable surface is
  small *and* the typed grammars that would exercise it (twitter/citm/canada) **do not yet
  exist** (FAMILY-SPECS §0, verified premise). G2 cannot even run until GN-3.W3 authors them.
- **G1C-4 [R-C4 subtype-join / R-C3 sub-span granularity — design-fixed, unbuilt].** MLsub
  subtype-join reserved for numerics; `Project.ty` is one `String` with whole-expr span only
  (the quality ceiling on 1C's entire thesis). Neither built.

**Why lowest (40%, up marginally from prior 38%):** real substrate on the *graduating* tree,
but no prototype and the one experiment that would earn it a name (R-C2) skipped twice. A
footnote on 1A wearing a family charter until that measurement runs.

---

## FAMILY 1D — TYPES-AS-ECLASSES · convergence 45%

**What is genuine (best prototype evidence, and it advanced twice).** PROTO-2 RAN: injectivity
= unification (a–d), width+injectivity fixpoint converges in 2 passes with no oscillation (e),
native cyclicity yields a finite μ-representative for free. PROTO-2-recursion-parity RAN: it
measured 1D's μ-sum against the *running* CSP's actual `ir.types` output and confirmed 1D is
**strictly more faithful** — `Sum[6]` with `List(#0) ≠ Row(#0)` vs the running solver's
`Sum[4]` with **`array == object == List(Enum)`** (the running TypeDesc cannot even distinguish
a list-of-value from a row-of-kv-pair). That structural richness is *real and measured*, agreed
by both probes.

**But the headline is falsified, the two probes conflict, and the retire-verdict rests on a
masked hand-code fallback.**

- **G1D-1 [SEVERE — "zero new engine" is FALSE; verified].** FAMILY-SPECS 1D and research-D §0
  claim "zero new engine — only a third `Language`." PROTO-2's patch **modifies the shared
  engine** `crates/egraph/src/egraph.rs::union` (delete the `if changed` guard; push
  `analysis_pending` unconditionally; add `rebuild_pass_count`; +30/−4). I confirmed the guarded
  form is still at HEAD and the patch's unconditional form in the diff. 1D **requires a
  core-engine semantic change**; the charter claim does not survive its own existence proof.
  (FAMILY-SPECS's revised text now *does* acknowledge this — "a change to the shared/vendored
  engine, not a consumer-side addition" — good; the prior critique was folded. The *claim* in
  the charter line and research-D still needs correcting in the registry.)
- **G1D-2 [CROSS-LOCK BLAST RADIUS — "free" is asserted, never measured — X5].** The amended
  `union()` is on **LOCK-2's `GrammarENode` and LOCK-5's `RegexENode` hot saturation path**.
  Every union in those tiers now unconditionally enqueues, re-canonicalizes, and calls `modify`
  (a no-op for `GrammarAnalysis`/`NoAnalysis`) — extra worklist churn per union across two other
  locks. PROTO-2 verified *correctness* (37 passed) and **explicitly makes no perf claim**. The
  "free for no-op analyses" line is a claim, not a measurement. **1D's disposition cannot be
  settled inside LOCK-1** — it needs LOCK-2's sign-off + a saturation perf measurement.
- **G1D-3 [AC-soundness carried as an unbuilt design obligation].** PROTO-2 finding (4):
  positional injectivity is **unsound for AC constructors** (`Sum`); the probe stayed sound only
  by keeping AC-sum rewrites confluent so injectivity "lies dormant on `Sum`." A production 1D
  must **tag constructors injective-vs-AC and gate the hook** — unbuilt; the soundness rests on
  the fixture never holding two rival same-arity sums, a coincidence, not a mechanism.
- **G1D-4 [TWO PROBES, OPPOSITE VERDICTS, UNRECONCILED — and the RETIRE rests on a masked
  fallback].** PROTO-2-strawman → keep; PROTO-2-recursion-parity → retire. FAMILY-SPECS carries
  only the keep. The RETIRE verdict's own reasoning is contestable: it concedes the running CSP's
  TypeDesc is **lossy** (`array == object`, `Sum[4]` not `Sum[6]`) and declares the loss "not
  load-bearing" **because hand-written runtime types (`crates/core/src/runtime/json/value.rs` +
  `JsonStructBuilder`) supply the 6-variant `JsonValue` downstream.** That is exactly the
  hand-code-covers-for-weak-inference pattern the standing laws forbid
  (**typed-materialization-invariant**: "every `->` reaches the tape emitter; inference composes
  types, never loses them"; **no-value-discard**; **preserve-rich-ast**). If the greenfield is
  grammar-authoritative-typed (per project memory `grammar-authoritative-status`), a TypeDesc
  that collapses array and object is a **wall**, and 1D's structural μ-type is precisely what
  closes it — so the "unused capacity" verdict presupposes the *codegen-distributed-nominal*
  architecture and quietly rejects the *grammar-authoritative* one. **That architectural choice
  is unadjudicated (X1-adjacent), so neither PROTO-2 cleanly decides 1D.**

**Why 45%:** only family with ran-and-measured prototypes, and the recursion *structure* win is
genuine and measured against the real incumbent. But its charter claim is falsified, its two
probes disagree, its cross-lock cost is unmeasured, and its raison d'être (cost-extraction) is
idle where PROTO-1 found the mass (X4). Elegant and possibly-necessary — the "necessary" turns
on an unresolved architecture call.

---

## CROSS-LOCK & CONSTITUTION FINDINGS (critic-2 weight)

- **X1 — THE LINEAGE GAP, now with a DIRECT MEMO CONTRADICTION (cross-lock LOCK-7/GESTALT-B;
  the lock's foundational hole).** Both trees are live, separately-buildable cargo workspaces
  (verified: `skinny/Cargo.toml`; root `/Cargo.toml`). FAMILY-SPECS §0 frames **root** as "the
  tree the greenfield is cut from" and skinny as the "older lineage." **Project memory SK-V22
  governs and says the reverse: "skinny graduates to tree of record … the greenfield totality
  replaces it"** (verified). This is not "undecided" — the specs are **on the wrong side of the
  governing memo as written.** If SK-V22 holds, the cost accounting **inverts**: 1A/1C target
  the *graduating* tree (cheaper than the specs credit), and 1B/1D's "already runs in root" is
  the *archiving* tree (their free-win evaporates — they must be ported). LOCK-1 cannot converge
  until GESTALT-B/LOCK-7 reconcile FAMILY-SPECS §0 with SK-V22. **This alone caps every family
  below full convergence.**

- **X2 — C4/GN-3 §1.5 voided without a NAMED SUPERSESSION (Constitution ix breach).**
  FAMILY-SPECS §0 declares prior-art C4 / GN-3 §1.5 ("no CSP or e-graph consumer exists … so no
  such substrate lands") "factually superseded." GN-3 §1.5 is a **committed adjudication in the
  extant codex.** Constitution ix requires every divergence to be a *named supersession* with the
  GN-3 doc amended, not an inline "factually false." Procedural, unresolved — carry to GESTALT-B.

- **X3 — PROTO-1 is a gate that cannot fail (manipulated gate).** Both pre-registered gates FAIL
  on all four corpora (`eq+sum ≥95%` lands 82–89%; `structural-non-eq ≤2` lands 18/1020/53/87).
  The census reaches "THIN — decisive" by **substituting a third gate post-hoc** (`multi-valued
  ≥15% ⇒ RICH`) chosen *because* it yields THIN, and even prints re-cut instructions for a future
  probe. Honest pass-1 output is **INCONCLUSIVE on the registered gates + a proposed re-cut**, not
  a decisive retirement of 1B and 1D. The elegant-reduction trap in evidence form.

- **X4 — PROTO-1 measures the wrong engine for the residue thesis (circularity).** `overload = 0`
  and `decode-class = one-fixed-per-site` are artifacts of the live pass **not modeling
  overload/legality** (grep-verified earlier by CRIT-1/prior-CRIT-2: no `overload`/`escape`/
  `legality` in `crates/ir/src/passes/types/`). The families' rich-residue thesis (decode-class as
  an escape-conditioned *overload*, research-A §1.7.2 / research-B §4.4) is tested against an
  instrument that **structurally cannot exhibit it.** The residue axis is **not closed.**

- **X5 — 1D↔LOCK-2 is DOUBLE-HOMED, not "substrate-shared."** PROTO-2 shows 1D **modifies**
  LOCK-2's engine (`egraph.rs::union`), affecting LOCK-5's regex tier. The incompatibility map's
  "resolvable by subordination under LOCK-2" is the wrong frame: an engine amendment in LOCK-2's
  core with an unmeasured cross-lock perf cost cannot be "banked under LOCK-2" quietly. It is a
  GESTALT-B adjudication with a **required saturation measurement** (the "free" claim).

- **X6 — the two PROTO-2 probes conflict and only one is in the specs (cross-family evidence
  hygiene).** PROTO-2 (keep 1D) and PROTO-2-recursion-parity (retire 1D) reach opposite verdicts
  on the same question; the parity probe **explicitly flags owner-adjudication**; FAMILY-SPECS
  carries only the keep. The registry must record both and adjudicate — the pass cannot present a
  single 1D disposition while holding two contradictory measured verdicts.

- **X7 — protocol: convergence-talk is premature.** One pass; this is the first fresh non-author
  audit; zero families with two consecutive clean passes. FAMILY-SPECS's "synthesis floor / the
  surviving spine" reads as near-closure and must be down-graded to "pass-1 disposition, all four
  open" — especially now that its confirmed defect is void and its 1D keep is contested.

---

## VERDICT (critic 2)

The lock is **materially less converged than FAMILY-SPECS presents**, and the gap widened this
cycle rather than closing:

1. **The lineage is undecided AND the specs contradict the governing memo (X1).** FAMILY-SPECS
   §0 ("root is the base, skinny older") is opposite to SK-V22 ("skinny graduates to tree of
   record, the greenfield replaces it"). This inverts the whole cost model and gates the lock.
2. **The specs are stale: the two "un-run" briefs ran and undercut the specs.** The "confirmed
   shipping defect" (`TypeDomain::join`) is **dead code (§0a, verified)**; the live 1B pass is
   **sound on every reachable clash (§0b)**; and the 1D "recursion-axis keep" is **contradicted
   by PROTO-2-recursion-parity's RETIRE** — which itself rests on a masked hand-code fallback
   (`json/value.rs` supplying structure the lossy TypeDesc drops), a **typed-materialization**
   tension the specs never adjudicate.
3. **The synthesis crowns un-prototyped families** (1A/1C: real substrate, zero direct
   prototype) and retires the ran/measured ones (1B runs; 1D measured) on a **gate-manipulated
   (X3), wrong-engine (X4) census.**
4. **1D's charter claim is falsified** (zero-new-engine false, G1D-1) with an **unmeasured
   cross-lock cost** (G1D-2/X5).
5. **1C's make-or-break was never run** across two brief-cycles (G1C-2).

**Pass-2 must:** (a) get LOCK-7/GESTALT-B to reconcile FAMILY-SPECS §0 with SK-V22 and fix the
greenfield base **before any family is scored**; (b) correct the specs/registry — the confirmed
defect is dead code; the live carrier is `assign`; the live pass is reachable-sound; (c) re-run
the residue census on an instrument that *models* overload/legality with the gate honored or
formally re-cut in advance; (d) reconcile the two contradictory PROTO-2 verdicts, and adjudicate
the lossy-TypeDesc-vs-hand-code question (grammar-authoritative or codegen-distributed?) since it
decides 1D; (e) route PROTO-2's `union()` amendment through LOCK-2 with a saturation measurement;
(f) run the 1C diagnostic head-to-head; (g) record the GN-3 §1.5 override as a named supersession.
**No family is BLOCKED; none is converged.**

### Convergence table
| Family | conv % | disposition | the one gap that dominates |
|---|---:|---|---|
| 1A UNIFICATION-FIRST | 55% | VIABLE-ON-PAPER | no direct prototype; cost model built on a lineage premise that contradicts SK-V22 (G1A-1/G1A-2/X1) |
| 1B ONE-CONSTRAINT-STORE | 55% | VIABLE, runs, reachable-sound | its "confirmed defect" is dead code and its retirement-to-1A lost its main prop; store-shape unbuilt (G1B-1/G1B-3/X3/X4) |
| 1C BIDIRECTIONAL-LOCAL | 40% | OVERLAY-UNPROVEN | make-or-break diagnostic head-to-head never run across two brief-cycles (G1C-2) |
| 1D TYPES-AS-ECLASSES | 45% | VIABLE-AT-RISK, verdict CONTESTED | zero-new-engine falsified; two probes give opposite verdicts; RETIRE rests on a masked hand-code fallback (G1D-1/G1D-4/X5/X6) |
