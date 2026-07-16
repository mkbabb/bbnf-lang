# LOCK-1 — THE SOLVER SPINE · pass-1 · CRIT-1 (adversarial critic 1 of 2 — WITHIN-LOCK view)

*Charge: harden/challenge/refine every family against the failure-mode checklist (vacuous
convergence, spec-cites-itself circularity, gates that cannot fail, the elegant-reduction trap,
legacy aliases, masked fallbacks, unverified gestalt, consumer-less substrate) + constitution
violations + prototype-evidence misuse. Critic 1 weights internal soundness of each family spec
and whether the prototypes actually tested the sharpest uncertainty. Verified against source
(§0), not taken on the reports' word.*

---

## 0. WHAT I VERIFIED AGAINST SOURCE (and what it settles)

Every load-bearing structural claim in FAMILY-SPECS §0 I re-checked at HEAD:

1. **§0's reframing is FACTUALLY GROUNDED — prior-art C4's "consumer-less" veto is void for
   the live lineage.** Confirmed: `crates/ir/src/passes/types/mod.rs` runs `project_types()` →
   `generate_constraints` → `system.csp.propagate()` (verified in tree); `TypeDomain:
   LatticeDomain` exists; `GrammarIR::type_obligations` is a live field
   (`crates/ir/src/types/grammar.rs:428`); **both** backends consume it
   (`backend/rust/ir_types.rs:342` and `backend/ts/projection.rs:37` both match on
   `TypeDesc::HeterogeneousAltJoin(_)`); end-to-end tests exist
   (`core/tests/{types_heterogeneous_alt,project_types_json}.rs`). **The critic accepts §0:**
   1B and 1D are NOT consumer-less substrate; the honest adjudicator is residue richness, not a
   metatheory prohibition. This is the strongest, cleanest part of the pass.

2. **R-B2 is a CONFIRMED defect, quoted accurately.** `constraint/domain.rs:66-81`,
   `TypeDomain::join`, arm `(Some(existing), Some(ty)) if existing != ty => { self.solved =
   Some(ty.clone()); true }` with comment *"the solver may refine types during convergence."*
   This is last-writer-wins on a genuine clash — non-monotone, order-dependent, a **masked
   fallback** where `no-silent-epsilon` demands a raised obligation. FAMILY-SPECS 1B names it
   correctly. **But the critic finds a consequence the spec missed — see §5.1.**

3. **PROTO-2's "zero new engine" is FALSIFIED by its own method — and the engine patch is not
   in the tree.** `crates/egraph/src/egraph.rs:235-237` at HEAD still guards
   `let changed = A::merge(...); if changed { self.analysis_pending.push(dst); }`. This is the
   *old* behavior PROTO-2 says it amended (+30/−4) so that `modify` fires on **every** structural
   merge, not only on lattice change. The amendment was reverted after the probe. **On the
   shipped engine, injectivity never fires on a structural `Sum`/`Arrow`/`List` merge** (they do
   not move the width lattice). See §5.4.

4. **The census's `width` tag is Seq/tuple-arity, NOT numeric bit-width.** Confirmed in
   `PROTO-1.patch.diff`: `IrNode::Seq(children) => … c.bump_p("width")` (SeqConstraint);
   `NumberConvert/HexConvert` → `"decode-class"`; and the tagging comment (patch line 325-326)
   *asserts* "width/capacity/decode are deterministic monotone functions … not [residue]." This
   is decisive for the residue verdict — see §5.2.

Neither probe's code is currently in the tree (`crates/core/tests/lock1_residue_census.rs`
absent; egraph amendment reverted). Both survive only as `.patch.diff`. The measurements are
credible but **not reproducible from HEAD** — a pass-2 reproducibility obligation for both.

---

## 1. THE CROSS-CUTTING FINDING THAT RE-FRAMES THE WHOLE LOCK

**The fork's two adjudicating axes are each under-measured, in the same direction, so the
synthesis verdict ("collapse to 1A-trunk + optional 1C, or keep 1B") is NOT yet earned by the
evidence — it is earned by the *framing* of the evidence.** FAMILY-SPECS §"two axes" asserts
both are "measurable now." They were only half-measured:

- **Residue axis (Brief 1 / PROTO-1):** measured *from ground truth* for exactly ONE category
  (homogeneous vs heterogeneous `Alt`, read off `HeterogeneousAltJoin`). Every other category
  — width, capacity, decode-class — is **pre-tagged single-valued by the taxonomy itself**
  (patch line 132, 325-326), not measured. And the numeric bit-width judgment that the *entire*
  fork discussion centers on (prior-art §2G, 1A §1.7, 1A-gate-iv, 1C-R-C4, 1D-test-3, PROVE-08's
  admitted U8Tag→u32 loss) **is not a census category at all** — the live pass maps numbers to
  one decode (`NumberConvert`), so the census cannot see a width residue the live pass never
  produces. The RICH gate could only ever fail.

- **Recursion axis (Brief 2 / PROTO-2):** the 1D side was *built*; the 1A side was *asserted*.
  PROTO-2 finding 3 ("native cyclicity **beats** a nominal fold — 1A would need an explicit
  nominal binder + fold") is a one-sided comparison: **no 1A nominal-fold prototype was
  constructed to race against.** The "head-to-head vs 1A" that both 1A-R1 and 1D-test-7 name as
  make-or-break was not a head-to-head.

Consequence: the lock's convergence story ("1A + defaulting lattice is the surviving spine")
is *plausible and prior-art-consonant*, but pass-1 proved it for **document-shape residue
only**, on a pass that already assumes numeric determinism, with the counter-family's recursion
prototype uncontested by an actual 1A build. This is the **elegant-reduction trap in evidence
form**: "the residue is thin AND-then the hard part [numeric width residue; 1A's own recursion
cost] is unmeasured." No family is retired on this evidence; none is vindicated either.

---

## 2. FAMILY 1A — UNIFICATION-FIRST · convergence 70%

The prior-art-endorsed spine (research-priorart §5), proven seed (PROVE-08 CONFIRMED_DELTA),
dormant scaffold real (`TypeFacts.subst/.obligations` confirmed present in skinny lineage per
the spec's cites). Internally the soundest architecture. **But no 1A prototype was built this
pass — every 1A gate is asserted — and the numeric-width question it defers is the exact
question the census could not measure.**

Enumerated open gaps:
1. **Row discipline unresolved (spec-admitted).** Leijen scoped-labels vs Gaster–Jones
   lacks-predicates — the spec explicitly leaves this open ("1A must pick one and state it").
   Wand's principal-type trap (prior-art §2C trap 2) bites if it ships "records, informally."
   **Unresolved, and load-bearing for the document-shape half.**
2. **Numeric-width residue: CSP vs defaulting lattice — UNMEASURED, not merely unresolved.**
   1A §1.7 defers width/overload/capacity to a `csp-solver` post-solve "or an ordered defaulting
   lattice, which Brief 1 may show suffices." PROTO-1 did **not** measure numeric width (its
   `width` tag is tuple-arity; numeric goes to `decode-class`, pre-tagged single). So 1A's own
   central sub-question is open on the evidence, contra the spec's implication that Brief 1 fixed
   it. (Note: PROTO-2 test-e independently shows numeric width *is* a monotone join with
   minimal-cover extraction — i.e. a defaulting lattice — which supports 1A's cheap-fallback,
   but that is 1D's probe, not 1A's, and it proves the lattice suffices, not that a CSP is
   needed.)
3. **Generalization criterion (value restriction) named, not stated.** Prior-art trap 4. Rules
   are pure schema producers so risk is low, but the criterion is unwritten.
4. **1A's make-or-break gate (R1 recursive-sum via nominal fold) is UNBUILT and its comparison
   is one-sided against it.** The nominal `unify(Rule i, Rule j) = i==j` fold that guards
   recursion was not prototyped; PROTO-2 asserts (without building 1A) that 1D's native
   cyclicity *beats* it. 1A's termination/finiteness claim on `value = obj|arr|…` is unproven on
   1A's own machinery.
5. **Two-systems merge (R6) unproven.** The claim that `codegen::type_plane::hm::Ty` is deleted
   and `passes` becomes the single home is architectural intent; no prototype demonstrates the
   merge leaves `git diff crates/runtime/` empty and derive wall-time within GN-3's ~230µs.
6. **Constitution v (zero-runtime) asserted, not exercised.** No pass-1 artifact drove the
   runtime-diff gate for 1A.

---

## 3. FAMILY 1B — ONE-CONSTRAINT-STORE · convergence 68%

Best-*evidenced* family — it demonstrably runs (§0 verified). Its distinctness, however, is
contingent (R-B4) and its two genuinely-open design questions were **not** prototyped this pass;
one confirmed defect stands; and the census that adjudicates its distinctness ran *on 1B's own
buggy output*.

Enumerated open gaps:
1. **R-B2 confirmed defect — unfixed, and it biases the very census used to judge 1B (NEW,
   critic-found).** The last-writer-wins arm (`domain.rs:70-80`) *silently reconciles* a genuine
   clash instead of raising an obligation. PROTO-1's residue census reads homo/hetero from the
   *solved* `TypeDesc` — but a clash that this arm swallowed into a single type is counted as
   `equality`, never as residue. **The THIN verdict and the R-B2 defect are therefore not
   independent: the defect manufactures thinness.** PROTO-1 did not instrument how many times the
   `existing != ty` arm fired, so the bias is unquantified. (Mitigating: propagate converged in
   0.26s with no oscillation, suggesting the arm rarely/never fired on these corpora — but "rare"
   is asserted, not counted.) **Gap: fix the arm to fail-explicit AND re-run the census counting
   clash-arm hits, before the THIN verdict is load-bearing.**
2. **R-B1 store-shape (federation vs single sum-domain) — UNBUILT.** The spec says this "needs a
   prototype, not paper," and research-B §6.2 demanded the decode-class-depends-on-sum-variant
   counterexample be implemented *both* ways and measured. **Neither way was built.** PROTO-1 is
   a census, not the store-shape probe. The family's own sharpest design decision remains on
   paper.
3. **R-B4 distinctness verdict is directionally supported but methodologically entangled.** The
   census shows document-shape residue ≤2% ⇒ 1B collapses toward 1A on the equality axis. But
   (a) it is 1B's *own* output (mild circularity: "does running-1B produce rich residue?" →
   "no"), (b) the thinness is partly a taxonomy pre-tag (§5.2), and (c) it is biased by gap 1.
   The verdict "1B collapses toward 1A" is *probably right* but not *cleanly earned*.
4. **"One store" is honestly "one engine + one obligation channel + one discipline," not one
   variable arena — the spec concedes this but never resolves whether that dilution retires the
   family's thesis.** If 1B is federated `Csp<TypeDomain>` + `Csp<CostFiniteDomain>` sequenced by
   `solve_with_given` (which is what the tree runs), then 1B *is* HM(X) with X = the solver — i.e.
   1A's architecture with a CSP residue solver, not a distinct center. The incompatibility map
   calls 1A⊥1B "HARD-INCOMPATIBLE (two equality engines)"; but if the census retires the CSP
   equality engine toward union-find, 1B's *surviving* content is exactly 1A's §1.7 post-solve.
   **Unresolved: is 1B, post-census, a distinct family or 1A's residue-solver?**
5. **Constitution v (zero-runtime) asserted, not exercised** (R-B5) — same as 1A.

---

## 4. FAMILY 1C — BIDIRECTIONAL-LOCAL · convergence 55%

Least-converged family. Its entire raison d'être — diagnostic locality at the annotation site —
is *falsifiable and was not tested*, despite the spec (and research-C §6.2) naming the test as
"the measurement that justifies the family."

Enumerated open gaps:
1. **The make-or-break diagnostic head-to-head (R-C2) was NOT run.** No PROTO exercised
   "feed `string -> decode_…(input) : Bool` to unify-first and to `check_expr`; assert 1C's
   `TypeError.span == Project.SourceSpan` while unify-first reports at a synthesized interior."
   The family's distinct value is **entirely unmeasured**. The spec's own words: "if unify-first
   reports the same span … family 1C carries no distinct value and should fold into 1A." That
   experiment is the pass's most conspicuous omission.
2. **On generic `json.bbnf` (zero `->`), 1C ≡ 1A — spec-admitted degeneracy.** 1C's value exists
   *only* on typed grammars (twitter/citm/canada `->` chains). PROTO-1 confirms the corpora are
   sum/equality-dominated with sparse genuine residue — which is precisely the regime where
   checking has little to check against. **1C's addressable surface may be near-empty on the
   actual corpus** — unquantified.
3. **R-C4 subtyping-not-equality at the seam — design-fixed, unbuilt.** MLsub subtype-join over
   `u8≤u16≤u32≤u64` reserved for numerics, equality-unify for the rest. Getting it wrong collapses
   1C into spurious-clash or lost-width. No prototype.
4. **R-C5 substrate gap real, unbuilt.** `PassDiagnostic` (skinny) is `RuleId`-only, no span; 1C's
   headline requires adding `span: Option<SourceSpan>`. Additive but untouched this pass.
5. **R-C3 span granularity ceiling.** `Project.ty` is one `String`; a wrong ascription should
   underline the `T`, but `Project` carries whole-expr span only. The quality ceiling on 1C's
   entire thesis is a sub-span the IR does not carry.
6. **Incompatibility-map disposition unresolved.** The map places 1C as a *layer on the winner
   (naturally 1A)*, "not a fourth engine." If so, 1C is not a family but a diagnostics discipline;
   its 55% is a ceiling until the head-to-head (gap 1) proves the layer earns naming.

---

## 5. FAMILY 1D — TYPES-AS-ECLASSES · convergence 60%

PROTO-2 advanced 1D factually (injectivity-as-unification demonstrated (a)-(d); combined
width+injectivity fixpoint converged in 2 passes (e)). But its headline claim is falsified by its
own method, its load-bearing soundness obligation is unbuilt, and its distinctive engine is idle
on the axis the census found dominant.

Enumerated open gaps:
1. **"Zero new engine" is FALSIFIED (critic-verified against HEAD).** FAMILY-SPECS 1D:
   "reaches all three walls with zero new engine — only a third Language"; "rides `csp_scheduler`
   unchanged"; "the one net-new primitive is the backward direction (~30 LOC)." PROTO-2 §"one
   core-engine amendment" required **+30/−4 in `crates/egraph/src/egraph.rs`** to schedule
   `modify` on every real merge — because structural `Sum`/`Arrow` merges do not move the width
   lattice, so on the shipped engine (verified: `egraph.rs:236` still `if changed`) **injectivity
   would never fire.** This is a semantic change to the *shared* engine consumed by LOCK-2's
   grammar tier and the regex tier. So the net-new primitive is (~22 LOC modify hook) **+ an
   engine-core scheduling change**, and the latter is a LOCK-2/isomorphism-plane coupling the spec
   does not count. The amendment is also **reverted** — the proof rests on code not in the tree.
2. **The load-bearing soundness obligation (AC-vs-injective coexistence) is UNBUILT — the elegant
   -reduction trap.** PROTO-2 finding (4): positional injectivity is **unsound for AC
   constructors**; applied to `Sum`, two differently-ordered same-arity sums falsely unify
   (`null`≡`str`). The probe kept it sound by testing the two mechanisms in **separate e-graphs**:
   test (a) exercises `Sum`-as-tuple injectivity *with no AC rewrites running*; tests (b/c/d) run
   AC-sum rewrites *with injectivity dormant on `Sum`*. **The hard interaction — AC canonicalization
   and injectivity both live on `Sum`, gated by the constructor-injective-vs-AC flag that finding
   (4) calls "the one non-negotiable design obligation" — was never built or run.** The confluence
   argument ("the class never holds two rival same-arity sums") is asserted, not demonstrated under
   saturation with injectivity active. This is the sharpest uncertainty in 1D, and the prototype
   tested *around* it, not *through* it.
3. **The idle-machinery caveat stands — 1D's engine is idle where the census found the mass.**
   Prior-art §2F: equality saturation earns its keep only where the type is multi-valued (residue).
   PROTO-1 (however qualified) shows document-shape residue is thin. PROTO-2 test-e *manufactured*
   a width residue and showed 1D handles it — but that residue is **exactly a defaulting lattice**
   (monotone join + minimal-cover extraction), which 1A's §1.7 reaches without an e-graph. So
   PROTO-2 proved 1D *can* do the residue, not that 1D is *needed* for it. **1D's raison d'être
   (extraction-by-cost over many equal forms) remains unproven: `overload`/cost residue is exactly
   0 in all four grammars — there is nothing to extract over.**
4. **The recursion-axis "win" is one-sided (shared with §1).** Finding (3) "native cyclicity beats
   a nominal fold" was decided without building the nominal fold. 1D's advantage on recursion is
   *plausible* (free μ-extraction via the existing visited set is genuinely elegant) but the
   "beats" is a comparison against an unbuilt opponent.
5. **R4 determinism gate passed only on the toy graph.** Byte-identical extraction across two
   orderings held for the 6-arm `value` sum (10 classes). The GN-3 W3 gate (byte-identical
   `DirectSchemaSet` over the *real* typed grammars, FxHashMap iteration order) was not exercised
   at corpus scale.
6. **TS-backend isomorphism unaddressed.** 1D adds a third `Language` on the Rust side; 1B's
   verified consumers include `backend/ts/projection.rs`. How a TS backend consumes e-graph-derived
   types (constitution iii) is unspecified.

---

## 6. CONSTITUTION & PROTOTYPE-EVIDENCE AUDIT (lock-wide)

- **iv/v (refutation laws / zero-runtime, no perf claim):** RESPECTED. Both probes explicitly
  disclaim perf ("makes no perf claim, only counts"; "no cold timing claim"). No family anchors a
  perf claim on SPEC_ONLY. The zero-runtime gate is *asserted by every family, exercised by
  neither probe* — acceptable for pass-1, a named gap for all four in pass-2.
- **vi (parsimony / no god modules):** RESPECTED. 1A `types/{row,scheme,solve_annotations}`, 1D
  `egraph/{type_node,type_analysis,type_rules}` + `codegen/schema/type_plane/derive.rs` — clean
  splits, no kitchen-sink.
- **viii (consumer-less substrate):** the pass's cleanest result — §0's reframing is verified;
  1B/1D ship with live backend consumers. Prior-art C4's veto is correctly retired.
- **Masked fallbacks / legacy aliases:** ONE confirmed (R-B2, the "solver may refine" arm — a
  silent-overwrite masquerading as refinement). Correctly named by 1B; its census-biasing
  consequence (§5.1 gap 1) is the critic's addition.
- **Prototype-evidence misuse:** PROTO-1 is **MEASURED for document-shape residue, assumption-
  driven (SPEC-adjacent) for the width/overload axis** — its RICH gate is a *gate that cannot
  fail* because the only ground-truth-measured category is hetero-alt, and its strict-THIN gate is
  conceded to be mis-calibrated (the report itself says re-cut it). The verdict is honest about
  the miss but then *reinterprets past its own gates* — a self-serving re-cut that happens to be
  prior-art-consonant, but is a re-cut nonetheless. PROTO-2 is **MEASURED for (a)-(e) on a
  reverted engine patch, SPEC-ONLY for its load-bearing soundness obligation** (AC-vs-injective
  coexistence).

---

## 7. PASS-2 DIRECTIVES (what must be built, not argued)

1. **Fix R-B2 to fail-explicit, then re-run the census counting `existing != ty` arm hits** —
   sever the defect↔thinness entanglement (§5.1). Until then the THIN verdict is not load-bearing.
2. **Build the numeric-width probe the census skipped** — a corpus field genuinely
   under-determined between u8/u16/u32, measured for whether the choice is defaulting (ordered
   lattice) or search. This is 1A's §1.7 open question and the fork's true residue axis.
3. **Build 1C's diagnostic head-to-head (R-C2)** — the single experiment that decides whether 1C
   is a family or a footnote.
4. **Build the 1A nominal-fold recursion prototype** and race it against PROTO-2's cyclic
   extraction — make the recursion axis an actual head-to-head, not a one-sided assertion.
5. **Build 1D's AC-sum + injectivity coexistence** with the constructor-injective flag active on
   `Sum` under saturation — the untested interaction that is 1D's real soundness question; and
   **carry the `union`-scheduling engine amendment as a named LOCK-2/isomorphism cost**, not a
   free ~30-LOC hook.
6. **Build R-B1 both ways** (federated-`solve_with_given` vs single sum-domain) on the
   decode-class-depends-on-sum-variant counterexample — the store-shape decision the family cannot
   make on paper.
7. **Land both probes in the tree** (census test + engine amendment) or state them retired —
   pass-1 evidence currently lives only in `.patch.diff`, non-reproducible from HEAD.

---

## 8. CONVERGENCE SUMMARY

| Family | Disposition (spec) | Critic-1 convergence | Dominant open gap |
|---|---|---|---:|
| **1A** UNIFICATION-FIRST | VIABLE | **70%** | its own gates unbuilt; numeric-width residue unmeasured; recursion comparison one-sided against it |
| **1B** ONE-CONSTRAINT-STORE | VIABLE (1 defect) | **68%** | R-B2 unfixed *and* biases the census that judges it; R-B1 store-shape unbuilt; distinctness entangled with methodology |
| **1C** BIDIRECTIONAL-LOCAL | VIABLE (falsifiable) | **55%** | make-or-break diagnostic head-to-head NOT run; raison d'être entirely unmeasured |
| **1D** TYPES-AS-ECLASSES | VIABLE-AT-RISK | **60%** | "zero new engine" falsified (reverted core patch); AC-vs-injective coexistence unbuilt; engine idle on the census-dominant axis |

**Lock-wide:** the pass produced one clean structural verification (§0), one confirmed defect
(R-B2), and two probes that each advanced their family while testing *around* the sharpest
uncertainty rather than through it. The synthesis verdict ("1A-trunk + optional 1C layer, 1B
survives only on residue-richness, 1D survives as recursion-elegance or LOCK-2 substrate") is
prior-art-consonant and probably correct — but on pass-1 evidence it is **framed, not earned**:
the residue axis is measured for document shapes only (numeric width unmeasured, and the census
biased by the unfixed R-B2 arm), and the recursion axis is a one-sided build. No family converges;
none is retired. Pass-2 directives §7 are the exact experiments that would earn the verdict.
