# LOCK-1 — THE SOLVER SPINE · pass-1 · CRIT-1 (adversarial critic 1 of 2 — WITHIN-LOCK view)

*Charge: harden/challenge/refine every family against the failure-mode checklist (vacuous
convergence, spec-cites-itself circularity, gates that cannot fail, the elegant-reduction trap,
legacy aliases, masked fallbacks, unverified gestalt, consumer-less substrate) + constitution
violations + prototype-evidence misuse. Critic 1 weights internal soundness of each family spec
and whether the prototypes actually tested the sharpest uncertainty. Verified against HEAD (§0),
not taken on the reports' word.*

---

## 0. WHAT I VERIFIED AGAINST SOURCE THIS PASS (and the timeline that reframes everything)

**The decisive structural finding: the FAMILY-SPECS I am critiquing is STALE relative to its own
adjudicating evidence.** File mtimes, on disk:

| artifact | time | relation |
|---|---|---|
| research-{A,B,C,D,priorart} | 03:38–03:41 | inputs |
| PROTO-1 (census), PROTO-2 (1D existence) | 04:02–04:10 | inputs the spec cites |
| **FAMILY-SPECS.md (this critique's target)** | **04:41** | frozen HERE |
| PROTO-1B-clash | 04:48 | **ran AFTER — the spec calls it "un-run"** |
| PROTO-2-recursion-parity | 05:01 | **ran AFTER — the spec calls it "un-run"** |

FAMILY-SPECS §7 presents Brief 1 (CSP-equality soundness) and Brief 2 (1A-fold vs running-CSP
recursion parity) as *"both un-run this pass … the next two adjudicate the surviving fork."* Both
DID run — as PROTO-1B-clash and PROTO-2-recursion-parity — and **both partially refute the spec's
own body.** A reader of FAMILY-SPECS alone gets a materially wrong picture on two load-bearing
points (§0.2, §0.3 below). This is **unverified-gestalt at the document level: the synthesis was
sealed before its decisive measurements landed, and was never re-cut to consume them.** The single
highest-value pass-2 action is to re-fold both post-spec probes into the spec — the verdict shifts.

I re-checked every load-bearing structural claim the spec leans on. Four verified facts, three of
which cut against the spec as written:

### 0.1 §0's consumer-less reframing is TRUE — the cleanest result in the pass
Confirmed at HEAD: the live root pass runs `project_types()` → `generate_constraints` →
`csp.propagate()`; `TypeDomain: LatticeDomain` exists (`constraint/domain.rs:61`);
`GrammarIR::type_obligations` is a live field consumed by **both** backends
(`backend/rust/ir_types.rs`, `backend/ts/projection.rs` both match `HeterogeneousAltJoin`).
**1B and 1D are NOT consumer-less substrate; prior-art C4's veto is correctly retired.** Accept.

### 0.2 The spec's "CONFIRMED SHIPPING DEFECT" (O1) is MIS-ATTRIBUTED TO DEAD CODE — verified
FAMILY-SPECS calls the last-writer-wins arm of `TypeDomain::join` (`constraint/domain.rs:72-77`)
**"the confirmed shipping defect,"** *"Wand's principal-type-losing bug (priorart trap 2) in
production,"* *"unsound as shipped"* — and makes it the load-bearing reason to prefer 1A's
union-find carrier over the running CSP (1A O1, 1B O1, §5.1 table cell "overwrite-unsound as
shipped," synthesis floor §6). **I grepped `crates/ir/src/passes/types/` for `.join(`: ZERO call
sites.** `TypeDomain::join` (domain.rs:66) is **dead code** — the only references are the impl and
a doc comment; the monotonic solver drives propagation through `constraint.revise(...)` and never
calls `domain.join`. PROTO-1B-clash confirms this independently and goes further: the genuine live
clash site (an `Alt` join of two incompatible `->` arrows) routes through `join_types`
(`revise.rs:116`) which **raises** `TypeObligation::HeterogeneousAltJoin` naming the rule — i.e.
**raise-on-disagreement, the OPPOSITE of last-writer-wins**, verified by a live-pass test yielding
`ir.types[0] = HeterogeneousAltJoin([Bool, F64])` plus the obligation.

The real last-writer-wins carrier is `assign` (`revise.rs:27`: `*slot = Some(ty); true`, no clash
detection — I read it). But the generator never places two incompatible *ground* constraints on
one scalar var, so `assign` is **unreachable for clashes under well-formed tree IR** — a latent
hazard, not a shipping defect. **Consequence for the critique:** the spec's single strongest
argument for the carrier fork — "the running CSP's equality is unsound as shipped, so seed 1A's
union-find instead" — rests on a mischaracterized defect. With the live path proven sound, the
carrier choice is **more open than the spec claims**, not less (see §3, §5.1).

### 0.3 The egraph `union` amendment is STILL REVERTED at HEAD — 1D's proof is off-tree
`crates/egraph/src/egraph.rs:235-237` at HEAD: `let changed = A::merge(...); if changed {
self.analysis_pending.push(dst); }` — the *old* guard PROTO-2 says it amended (+30/−4) so `modify`
fires on every structural merge. **On the shipped engine, 1D's injectivity hook never fires on a
`Sum`/`Arrow`/`List` merge** (they do not move the width lattice). The amendment is a change to the
*shared* engine LOCK-2 and the regex tier consume, and it is not in the tree. The spec's 1D
integration-seams paragraph claims 1D "reuses `egraph.rs`" as if untouched; PROTO-2 §"one
core-engine amendment" contradicts that, and HEAD confirms the amendment was reverted.

### 0.4 The census cannot see the residue the fork is about; the corpus has zero `->`
From `PROTO-1.patch.diff`: the census `width` tag is `IrNode::Seq(children) => bump_p("width")`
(tuple *arity*); numeric bit-width goes to `NumberConvert/HexConvert => "decode-class"`, **one
fixed decode per site, pre-tagged single-valued.** So numeric width — the exact judgment the whole
fork centers on (prior-art §2G, 1A O2, PROVE-08's admitted U8Tag→u32 loss) — **is not a census
category and cannot register as residue.** And I confirmed the corpus sparsity is worse than the
spec's "essentially zero": `grep -c '\->'` over every in-tree grammar (json, css/pretty, sheets,
bbnf, ebnf, bnf) returns **0 arrows in every file**. The census measured *structural* inference
over grammars that contain **literally none of the `->` projections the type plane exists to
type**. The THIN verdict is a property of a corpus that lacks the phenomenon (§1).

---

## 1. THE CROSS-CUTTING FINDING — THE THIN VERDICT IS MEASURED ON THE WRONG CORPUS

The spine verdict ("1A + ordered defaulting lattice; retire 1B one-store and 1D cost-extraction on
the evidence") rests almost entirely on PROTO-1's THIN census. That census has two structural
defects that the spec concedes in fragments but never lets bite:

1. **The instrument cannot measure the quantity (gate that cannot fail).** "Genuine multi-valued
   residue" is read only from `HeterogeneousAltJoin` (Alt homo/hetero). Every other category —
   width, capacity, decode-class — is **pre-tagged single-valued by the taxonomy** (§0.4), and the
   numeric-width residue that dominates the fork discussion **is not a category at all**. So
   "overload/cost residue is exactly 0" is true *of the live pass*, but the live pass is
   architecturally incapable of emitting width residue (it maps numbers to one fixed decode). A
   null reading from an instrument that cannot register the quantity is **not evidence that the
   quantity is zero** — it is evidence the instrument is blind. The spec treats "0" as decisive for
   retiring 1D's cost-extraction and 1B's one-store; it is not.

2. **The corpus lacks the feature under test (unverified gestalt).** Zero `->` in every in-tree
   grammar (§0.4); the typed per-document grammars GN-3.W3 names *do not exist*. The spec states
   this (§0 second fact) and correctly marks 1A's row-poly and 1C's diagnostics "unfalsifiable / no
   corpus" — **but then still treats PROTO-1's THIN as load-bearing for retiring 1B and 1D.** You
   cannot have it both ways: if the corpus is too `->`-poor to measure 1A/1C, it is too `->`-poor to
   *retire* 1B/1D on residue thinness. Thinness here is a property of grammars that barely exercise
   the plane, not of the plane's essential residue. **The retire-1B/1D verdict is framed on evidence
   the same document declares insufficient for the sibling families.**

This is the **elegant-reduction trap in evidence form**: "the residue is thin AND-THEN the hard part
— numeric-width residue on real typed grammars — is unmeasured and unmeasurable on this corpus." The
1A-spine conclusion is prior-art-consonant (research-priorart §5 independently reaches it via
HM(X)/defaulting) and *probably right* — but pass-1 earned it by framing, not by measurement.

---

## 2. FAMILY 1A — UNIFICATION-FIRST · convergence 68%

The prior-art-endorsed architecture (research-priorart §5), proven seed (PROVE-08), now *further*
supported by the post-spec probes (PROTO-2-recursion-parity retires 1D toward the running
incumbent; PROTO-1B proves the live clash path sound). Internally the soundest *architecture*. **But
the spec conflates two different 1A's, and the load-bearing one is unproven and got weaker this
pass.**

The conflation: "1A ADVANCE (the spine)" fuses (i) *1A-the-stratified-architecture* — union-find
equality trunk + bounded residue solver + let-generalization — which prior art and both post-spec
probes support; with (ii) *1A-the-union-find-carrier* that **deletes the running root CSP and
re-seeds equality from PROVE-08's `hm.rs`**. The running pass is a CSP, not union-find. (i) is
well-evidenced; (ii) is an in-scope-migration that **the spec's own §5.1 admits is 1A's "quietest
liability" (O3 lineage displacement)** — and its justification just collapsed (§0.2: the CSP carrier
it would replace is not unsound on the live path).

Enumerated open gaps:
1. **The carrier-displacement case (O3) LOST its load-bearing argument this pass.** 1A's win
   "requires deleting running root code"; the spec justified paying that cost by "the running CSP is
   overwrite-unsound as shipped." §0.2 refutes that — dead code + sound live path. So the reason to
   prefer union-find *as the carrier* over the running CSP is now: prior-art familiarity + "raises
   clashes correctly" (which the running Alt-join *also* does). **The carrier fork is unresolved and
   the spec's tilt is unearned.**
2. **Row discipline (O1) unresolved AND unmeasurable.** Leijen scoped-labels vs Gaster–Jones
   lacks-predicates — spec-admitted open, principal-type-critical (Wand's trap), zero `->` corpus to
   decide it on. Sequences behind GN-3.W3.
3. **Numeric-width residue (O2): CSP vs defaulting — UNMEASURED, not resolved.** The census cannot
   see it (§0.4). PROTO-2 test-(e) independently shows numeric width *is* a monotone join with
   minimal-cover extraction — i.e. a defaulting lattice suffices — which *supports* 1A's cheap
   fallback, but that is 1D's probe on a manufactured residue, not 1A's, and it proves the lattice
   suffices, not that the spec measured it.
4. **No 1A prototype was built — every 1A gate (G1–G6) is asserted.** In particular the nominal-fold
   recursion termination (G1) was never raced; PROTO-2-recursion-parity shows the *running CSP*
   terminates on all real recursion, but that is the incumbent's proof, not 1A-union-find's.
5. **Generalization criterion (value restriction) named, not stated** (prior-art trap 4). Rules are
   pure schema producers so risk is low, but the criterion is unwritten.
6. **Zero-runtime (Constitution v) asserted, not exercised** — no probe drove `git diff
   crates/runtime/` empty for 1A.

---

## 3. FAMILY 1B — ONE-CONSTRAINT-STORE · convergence 65%

The best-*evidenced* family (§0.1 verified it runs), and the follow-on probe **strengthened it in a
way the spec never absorbed**: PROTO-1B proves the live equality carrier (Alt join) is
raise-on-disagreement — sound. Yet the spec, frozen before that probe, brands 1B's equality
"unsound as shipped" (O1) and leans on that to tilt toward 1A. The spec is *pessimistic about the
wrong thing*.

Enumerated open gaps:
1. **O1 "the confirmed shipping defect" is REFUTED for the live path (§0.2).** `TypeDomain::join` is
   dead code; the live clash site raises correctly. The genuine latent hazard is `assign`
   last-writer-wins under an uncapped monotonic loop (PROTO-1B finding 3: two incompatible grounds
   would *oscillate forever*, strictly worse than last-writer-wins) — but it is unreachable under
   well-formed tree IR. **Gap: harden `assign`/`join` to record `TypeObligation::TypeClash` +
   refuse the overwrite (PROTO-1B's minimal repair), so the hazard becomes an explicit obligation.
   This is additive and cheap; do it regardless of the carrier choice.**
2. **The single-store thesis is genuinely BLOCKED-candidate (R-B1) — and R-B1 was never built.** The
   spec says the literal one heterogeneous store needs per-revise tag dispatch (a wall of the same
   character as the problem) and reduces to a *federation* that "is 1A wearing a CSP coat." That
   collapse is **asserted, not demonstrated** — the decode-class-depends-on-sum-variant
   counterexample research-B demanded be built both ways was built neither way. The family's
   sharpest design decision remains on paper.
3. **The census that retires 1B's distinctness is 1B's OWN output (spec-cites-itself circularity).**
   PROTO-1 reads richness from 1B's solved `HeterogeneousAltJoin`: "does running-1B produce rich
   residue? No." Mild circularity, compounded by §1's corpus defect. Mitigating: the live carrier is
   sound (§0.2), so a swallowed-clash-counted-as-equality bias (the old worry) does not fire on the
   reachable surface — but the distinctness verdict is still measured on 1B's own thin corpus.
4. **Post-refutation, is 1B distinct or IS it the surviving spine?** With §0.2 removing the "seed
   1A's union-find instead" argument, 1B-as-running-CSP-hardened is a *live candidate for the carrier
   seat itself* — it has both backend consumers, a sound live clash path, and needs only the additive
   `assign` hardening. The spec's framing ("federation IS 1A") may have it backwards: the cheapest
   sound spine may be *the running CSP hardened*, not a union-find re-seed that deletes it. **This
   inversion is the pass's most important unadjudicated fork.**
5. **Zero-runtime (v) asserted, not exercised** — same as 1A.

---

## 4. FAMILY 1C — BIDIRECTIONAL-LOCAL · convergence 55%

Least converged, and structurally **untestable on the current corpus**. The FOLD disposition (1C is
a checking-front + SCC-seam + span-field discipline over the winning engine, not a fourth engine) is
correct and prior-art-consonant (Dunfield–Krishnaswami: bidirectional improves error locality). But
its entire raison d'être is unmeasured, and the spec concedes it cannot be measured until GN-3.W3.

Enumerated open gaps:
1. **The make-or-break diagnostic head-to-head (G2) CANNOT RUN — zero `->` corpus (§0.4).** The
   experiment — "feed an erroneous typed grammar to check-first vs unify-first; assert check-first's
   `TypeError.span == Project span`" — has no input to run on. 1C's distinct value is **structurally
   unfalsifiable today**, not merely un-run. The spec states this (O1) but still assigns a
   disposition; the disposition is a *bet*, not a finding.
2. **On zero-`->` grammars, 1C ≡ its host engine (spec-admitted degeneracy).** 1C's addressable
   surface on the actual corpus is near-empty.
3. **Substrate gap real, unbuilt.** `PassDiagnostic` (skinny) is `RuleId`-only; 1C's headline needs
   `span: Option<SourceSpan>`. Additive, untouched. Bank it regardless of engine winner.
4. **Sub-span granularity ceiling (O3).** `Project.ty` is one `String`; a wrong ascription should
   underline the `T`, but `Project` carries whole-expr span only — the quality ceiling on 1C's
   entire thesis is a sub-span the IR does not carry.
5. **Is 1C a family at all?** Per the Constitution's shared-mechanism rule it reuses PROVE-08's
   `Unifier` verbatim → closest kin to 1A → a discipline layer, not an engine center. The 55% is a
   ceiling until a typed corpus lets G2 prove the layer earns naming.

---

## 5. FAMILY 1D — TYPES-AS-ECLASSES · convergence 52%

**The most confused disposition in the pass, because the spec's "BANK — narrow recursion keep"
predates the probe that answers its own gate.** The spec gates 1D's keep on Brief 2 (G6: does native
cyclicity change real output schema?). Brief 2 ran (PROTO-2-recursion-parity, 05:01) and returned
**RETIRE** — the running CSP is finite/terminating on every real recursive grammar, and 1D's finer
μ-sum (`List(#0) ≠ Row(#0)`, all arms) is *"unused capacity relative to the shipped system, not a
capability it lacks."* Meanwhile the sibling PROTO-2.md (measured against a *strawman* unbuilt
nominal fold) says KEEP. **The two probes contradict; the spec is frozen between them; the owner
adjudication PROTO-2-recursion-parity flags is unresolved.** The evidence pushes past BANK to RETIRE
(with a narrow keep-hook only if a future consumer ever needs a *packed* rule-level structural
μ-type — none does today).

Enumerated open gaps:
1. **"Zero new engine" FALSIFIED, and the amendment is off-tree (§0.3, verified at HEAD).** The
   injectivity hook needs the `union`-scheduling change to the *shared* engine; without it,
   injectivity never fires on structural merges. The spec's integration-seams paragraph counts this
   as free reuse. It is a LOCK-2/isomorphism-plane coupling and it is reverted — the (a)–(e) proof
   rests on code not in the tree. **Reproducibility obligation.**
2. **The recursion "beats a nominal fold" (PROTO-2 finding 3) is a STRAWMAN VICTORY.**
   PROTO-2-recursion-parity names it exactly: 1D was raced against 1A's *unbuilt* nominal fold, and
   when raced against the *running* incumbent it wins nothing load-bearing. The spec's sole surviving
   1D advantage (native cyclicity) is thus **contested by the very brief the spec said would decide
   it** — and loses.
3. **The AC-vs-injective coexistence (the "one non-negotiable design obligation") is UNBUILT — the
   elegant-reduction trap.** PROTO-2 finding 4: positional injectivity is *unsound* for AC
   constructors (`Sum`); the probe kept it sound by running the two mechanisms in **separate
   e-graphs** — injectivity dormant on `Sum` in the cyclic test, AC-rewrites absent in the
   injectivity test. The hard interaction — AC canonicalization and injectivity both live on `Sum`
   under saturation, gated by the injective-vs-AC flag — **was tested around, never through.** The
   confluence argument is asserted.
4. **The engine is idle where the mass is and empty where its raison d'être lives.** Prior-art §2F:
   equality saturation earns its keep only over genuinely multi-valued types; the census (however
   qualified) shows document-shape residue thin and cost/overload residue 0. 1D's cost-extraction has
   nothing to extract over. Its recursion win is now non-load-bearing (gap 2). **What is 1D for?**
5. **Determinism gate passed only on the toy graph.** Byte-identical extraction held on 10 classes;
   the GN-3.W3 gate (byte-identical `DirectSchemaSet` over real typed grammars under FxHashMap
   iteration at 191-rule CSS) is unexercised.
6. **TS-backend isomorphism unaddressed** (Constitution iii). 1B's verified consumers include
   `backend/ts/projection.rs`; how a TS backend consumes e-graph-derived types is unspecified.

---

## 6. CONSTITUTION & PROTOTYPE-EVIDENCE AUDIT (lock-wide)

- **iv / v (refutation laws / zero-runtime, no perf claim):** RESPECTED. Every probe disclaims perf
  ("makes no perf claim, only counts"; "no cold timing claim"). No family anchors a perf claim on
  SPEC_ONLY. The zero-runtime gate is *asserted by all four, exercised by none* — a named pass-2 gap
  for every family, acceptable at pass-1.
- **vi (parsimony / no god modules):** RESPECTED. 1A `types/{row,scheme}`, 1D
  `egraph/{type_node,type_analysis,type_rules}` — clean splits.
- **vii (concrete artifacts or SPEC-ONLY):** MIXED. PROTO-1/-2/-1B/-2-parity all RAN and are
  labeled MEASURED. But the spec **fails to consume its two post-04:41 measured probes** (§0), so the
  *spec's* conclusions on the carrier fork (O1) and the 1D disposition are SPEC-adjacent assertions
  contradicted by measured evidence sitting in the same directory. This is the sharpest vii breach:
  not a fabricated number, but a synthesis that ignores its own later measurements.
- **viii (consumer-less substrate):** the pass's cleanest result — §0.1 verified; 1B/1D ship with
  live backend consumers; prior-art C4's veto correctly retired.
- **ix (extant-tranche consciousness):** GN-3 §1.5 adjudicated CSP/e-graph OUT of the type plane;
  §0.1's live-lineage discovery is a *named supersession* of that verdict (the live tree already
  runs a CSP with consumers) — correctly handled, not silently contradicted.
- **Masked fallbacks / legacy aliases:** ONE latent hazard confirmed (`assign` last-writer-wins,
  §0.2 / 3.1), unreachable for clashes but un-guarded in principle. The spec's naming of it is
  *mis-attributed to dead code* — a documentation defect that inflates the case against 1B.
- **Gates that cannot fail:** PROTO-1's RICH gate on the numeric axis (§1.1) — the only
  ground-truth-measured category is hetero-Alt; width/decode/capacity are pre-tagged single, and the
  report itself concedes the strict-THIN gate is mis-calibrated and "re-cut" it post-hoc. Honest
  about the miss, but the re-cut is self-serving (it happens to be prior-art-consonant).
- **Prototype-evidence misuse:** PROTO-2 (strawman) vs PROTO-2-recursion-parity (incumbent) reach
  **opposite verdicts on the same retire-or-keep question** and the spec cites only the strawman's
  keep. The spec's 1D BANK is anchored on the probe that measured against an unbuilt opponent.

---

## 7. PASS-2 DIRECTIVES (what must be built, not argued)

1. **Re-cut FAMILY-SPECS to consume PROTO-1B-clash and PROTO-2-recursion-parity.** The spec is stale
   (§0). Fixing this alone flips 1D BANK→RETIRE and reopens the carrier fork. Highest-value action.
2. **Reopen and adjudicate the carrier fork on the corrected facts (§3.4):** running-CSP-hardened vs
   union-find-reseed. The spec's tilt to union-find rested on a dead-code defect (§0.2). Weigh:
   1B keeps both backend consumers + sound live path + only additive hardening; 1A deletes running
   code (O3). Measure derive wall-time and `git diff crates/runtime/` for *both* seeds.
3. **Land the `assign`/`join` raise-on-disagreement hardening** (PROTO-1B minimal repair) — additive,
   converts the latent oscillation hazard into an explicit `TypeClash` obligation, ships regardless
   of carrier winner.
4. **Build a numeric-width probe the census structurally cannot run (§0.4, §1.1)** — a corpus field
   genuinely under-determined between u8/u16/u32, measured for defaulting-lattice vs search. This is
   1A O2 and the fork's true residue axis, and it needs a *typed grammar* to exist (blocks on
   GN-3.W3, or a hand-authored micro-fixture).
5. **Reconcile the PROTO-2 contradiction into the record** (owner-adjudication flag): the strawman
   keep is falsified by the incumbent-parity retire. Record RETIRE-with-keep-hook, or state the
   narrow future consumer that would revive 1D.
6. **If 1D is not retired, build the AC-sum + injectivity coexistence under saturation** with the
   injective-vs-AC flag active on `Sum` (§5.3), and **carry the `union`-scheduling amendment as a
   named LOCK-2/isomorphism cost**, not a free hook — and land it in the tree (§0.3).
7. **Author at least one typed `->` grammar** (GN-3.W3) so 1C's diagnostic head-to-head (G2) and
   1A's row discipline (O1) become *falsifiable at all*. Until then 1C's 55% and 1A's row clause are
   ceilings no pass can raise.
8. **Land both census probes in the tree or mark them retired** — pass-1 residue evidence lives only
   in `.patch.diff`; the egraph amendment is reverted (§0.3). Non-reproducible from HEAD.

---

## 8. CONVERGENCE SUMMARY

| Family | Disposition (spec) | Critic-1 | Dominant open gap |
|---|---|---:|---|
| **1A** UNIFICATION-FIRST | ADVANCE (spine) | **68%** | conflates architecture (well-evidenced) with union-find carrier (unproven; its justifying defect is dead code, §0.2); every 1A gate unbuilt |
| **1B** ONE-CONSTRAINT-STORE | ADVANCE fed. / BANK thesis | **65%** | spec brands it "unsound as shipped" — REFUTED (live path sound, §0.2); R-B1 store-shape unbuilt; may be the cheaper spine, not 1A's coat (§3.4) |
| **1C** BIDIRECTIONAL-LOCAL | FOLD | **55%** | make-or-break diagnostic head-to-head structurally un-runnable — zero `->` corpus; raison d'être unfalsifiable today |
| **1D** TYPES-AS-ECLASSES | BANK (recursion keep) | **52%** | disposition frozen before Brief 2, which returned RETIRE; strawman keep; "zero new engine" falsified (reverted core patch); AC-vs-injective untested |

**Lock-wide.** Pass-1 produced one clean verification (§0.1 — consumer-less veto retired), two
measured post-spec probes (PROTO-1B, PROTO-2-recursion-parity), and **a synthesis that was frozen
before both landed and never re-cut to consume them.** The 1A-spine + defaulting-lattice conclusion
is prior-art-consonant and probably right, but pass-1 earned it by *framing on the wrong corpus*
(zero `->`, numeric residue unmeasurable — §1) and on a *mis-attributed defect* (dead-code
`TypeDomain::join` cast as a production bug — §0.2). The two post-spec probes, correctly folded in,
**strengthen the incumbent running CSP** (its live clash path is sound) and **retire 1D harder than
the spec allows** (BANK→RETIRE), which together reopen the carrier fork the spec prematurely closed.
No family converges; none is cleanly retired; the sharpest unadjudicated question is now the
inverted carrier fork of §3.4 — *harden the running CSP, or delete it for a union-find re-seed?* —
which the spec answered on a defect that does not exist.
