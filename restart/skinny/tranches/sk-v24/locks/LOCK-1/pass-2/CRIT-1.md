# LOCK-1 — THE SOLVER SPINE · pass-2 · CRIT-1 (Adversarial Critic 1 of 2 — WITHIN-LOCK view)

*Charter: harden/challenge/refine every family against the failure-mode checklist + the
constitution + prototype-evidence discipline; weight the internal soundness of each family spec
and whether the prototypes tested the sharpest uncertainty. Convergence % + exact enumerated gaps
per family. Base pin re-verified at HEAD `06e70053f` (matches FAMILY-SPECS; `ce51c36fc` — the probe
pin — confirmed ancestor). All path:line and grep facts below independently re-run, not inherited.
This report supersedes the earlier CRIT-1 pinned at `c43126422` (pre-dates the second-round probes).*

---

## §CRIT-0 — THE HEADLINE FINDING: FAMILY-SPECS IS STALE; THE SYNTHESIS'S CENTRAL TENSION IS DISSOLVED BY ITS OWN COMMISSIONED PROBES

**The synthesis (FAMILY-SPECS.md, mtime 18:46) predates the two second-round probes it set in
motion (PROTO-1.md 19:27, PROTO-2.md 19:37).** It presents Brief α (let-gen additivity) and Brief β
(X8 clash arm) as *the two open uncertainties* and hangs its entire organizing thesis — §0 X10, "the
pass's deepest finding," the "void the floor hides," the claim that the 43–55% floors are "measured
against an architecture no family ships" — on Brief α being unresolved. **Both briefs are now RUN,
and Brief α's resolution does not merely fill the void; it proves the void was a category error.**
A critic cannot ratify convergence numbers whose load-bearing premise the pass's own evidence has
since overturned. This is the report's spine; every per-family number below is re-struck against it.

**What PROTO-1 (Brief α, RAN, ce51c36fc) actually found — verified independently:**
- `TypeDesc` has **no type-variable variant** (I re-read `crates/ir/src/types/type_desc.rs`: the
  arms are `Span, F64/Bool/I8..U64, Option, Vec, Tuple, BoxedEnum, Enum, Named, HeterogeneousAltJoin`
  — **no `Var`, no `Row`, no `Sum`**). There is nothing to generalize and nothing to fresh-instantiate.
- One `RuleId` → one type var (`generate.rs:89`), and the Ref bridge is **unidirectional** (use
  sites never write the rule binding). False fusion — two uses competing for one binding's monotype
  — is **structurally unreachable**, not merely untested.
- The only polymorphic surface (grammar closures) is beta-reduced pre-IR **and is non-functional**:
  I confirmed `crates/core/src/lower/expression/mod.rs:168` is `node.child(4).expect("closure:
  missing body child")` and the closure tests at `crates/core/tests/pipeline.rs:704` are `#[ignore]`d
  against exactly that panic.

**Consequence the synthesis has not absorbed.** §0's X10 demanded HM(X) let-generalization "at
exactly the layer let-generalization exists to make sound (false-fusion soundness)." **That layer
does not exist in this IR.** Let-generalization is not "unbuilt" (a redirect-inward); it is
**inapplicable** to a monomorphic-by-construction IR. The correct LOCK-1 move is to **strike
let-generalization from the settled floor** and replace it with a *named, enforced invariant gate* —
which the pass has NOT done. This reclassifies several §0 findings:
- The "void" (X10) is **retired**, not carried forward. Prior-art §2 already flagged the value
  restriction as "vacuous for LOCK-1"; PROTO-1 extends that to the whole let-gen axis.
- The convergence floors were **measured against a phantom requirement** — a mild vacuous-convergence
  fault in the synthesis (the floor cited a feature the problem class lacks).

**The new gap the resolution opens (and the synthesis has no gate for) — the elegant-reduction
trap.** PROTO-1's verdict "1B is monomorphic *by construction*" is load-bearing, and its
"construction" is *beta-reduction + inline expansion of closures before types*. But **that
construction does not run** — closure lowering panics (verified above). So the soundness rests on a
step ("and-then-beta-reduction-monomorphizes-the-polymorphism") that is itself unconverged and
currently broken. PROTO-1's own caveat concedes it: *"If a future tranche wires a live parametric
rule that survives into the IR as an un-reduced call … THEN the false-fusion floor becomes
reachable."* This is precisely the reading-and-then-the-hard-part pattern the checklist names. **The
floor is delivered zero-diff only because the polymorphic surface is simultaneously absent AND
broken.** The pass owes a NAMED GATE — e.g. an assertion that no parametric/`Call`/`Closure` node
survives lowering into the type pass, CI-enforced — so that "monomorphic by construction" is a
*checked invariant*, not an incidental property of a broken feature. No family carries this gate
today; it is a new open gap for the lock (**G-CRIT-α**).

---

## §CRIT-1 — WHAT THE SECOND-ROUND PROBES SETTLED, AND WHAT THEY LEFT UNTOUCHED (probe-coverage audit)

The pass spent its *entire* second-round probe budget on α and β — **both 1B-favorable**. The
decisive experiments for 1A (port-cost race), 1C (R-C2 localization), and 1D (Row-derivation /
scheduler-tier) were **not run**. This is a coverage skew the convergence table must reflect: 1B's
number moves hard, the other three are stale-in-place.

| probe | family served | verdict | independently confirmed? |
|---|---|---|---|
| PROTO-1 (false-fusion, Brief α) | 1B (also moots 1A's distinctive) | floor unreachable → delivered zero-diff | YES — patch adds ONLY `crates/core/tests/proto1_false_fusion.rs`; zero solver-spine lines |
| PROTO-2 (X8 clash arm, Brief β) | carrier-independent (1B + 1C) | LANDED in worktree, 4/4 conjuncts | YES — patch touches csp-solver accessor + `types/{domain,revise,mod,obligation}` + test; **zero runtime, zero codegen** |
| PROTO-2-iso (carrier iso floor) | 1B (categorically excludes 1A/1D) | 1B is the only carrier that crosses to TS | Consistent — sole `TypeDesc→TS` sink `type_desc_to_ts` at `ts/projection.rs:11` dispatches on `TypeDesc` variants; no monotype/`TypeENode` path exists |
| **NOT RUN** | 1A port-cost (G-1A-1) | SPEC_ONLY | Option A never built — parity race remains a claim |
| **NOT RUN** | 1C R-C2 (make-or-break) | SPEC_ONLY | no mismatch fixture authored; distinct identity unfalsifiable |
| **NOT RUN** | 1D D1 Row-derivation / D2 scheduler | SPEC_ONLY | re-homed to GN-1; `TypeDesc::Row` still absent at HEAD (verified) |

**A critical caveat on β's "LANDED."** X8 is landed **in the probe worktree**, not in the tree of
record. I re-read `crates/ir/src/passes/types/constraint/revise.rs` at HEAD `06e70053f`: `assign` is
**still the raw 3-line LWW** (`if slot.as_ref()==Some(&ty){false}else{*slot=Some(ty);true}`). Under
XL-H, an unmerged worktree patch is proven-*landable*, not shipped. The registry's "one deliverable
owed for two passes" is de-risked to "proven-landable, awaiting merge" — a real advance, but not
"shipped." Do not let the convergence bookkeeping record X8 as closed until the patch is in HEAD.

---

## §CRIT-2 — FAMILY 1A · UNIFICATION-FIRST · ~57% (was ~55%; distinctive advantage MOOTED)

**Endorsed:** the architecture is HM(X) restated (prior-art §2), proven at leaf scale (PROVE-08), and
correctly BANKED-as-architecture. No dispute on the theory.

**The pass's net effect on 1A is a DEMOTION of its carrier case, lightly offset by gap-closure.**
- PROTO-1 **moots 1A's one distinctive rightness.** The synthesis itself said 1A "alone would carry
  polymorphism … its one distinctive rightness against the void of §0," and gate G3 (let-gen prevents
  false fusion) was "the gate §0's void makes mandatory and 1A alone carries." **G3 is now MOOT** —
  there is no false fusion to prevent. 1A's sole advantage over 1B evaporates; the fork collapses
  onto build-cost + iso, both LEAN-1B.
- G-1A-2 (**dead displacement lever**) — I confirmed it hard: `TypeDomain::join` is *defined*
  (`domain.rs:66`) but has **zero call sites** in `crates/ir/src/passes/types/` (grep `.join(` over
  the pass = empty). The live carrier is `assign`. 1A's justification "the running CSP is
  overwrite-unsound as shipped" targets dead code. **1A's tilt is unearned — reconfirmed.**
- PROTO-2-iso hardens the iso elimination: a fused monotype has **no `type_desc_to_ts` path**; 1A
  ranks last on iso categorically. If LOCK-6 makes iso dispositive, 1A is pre-eliminated.

**Enumerated open gaps (1A):**
- **G-1A-1 [port cost, STILL SPEC_ONLY].** "Activate the scaffold" inverts under XL-A to "port
  PROVE-08 into root + displace the running CSP." I measured the rewire surface: **62 files match
  `TypeDesc::`** (the synthesis's "~91 sites" is in the right order of magnitude). Option A is never
  built; the parity/wall-time race is a claim. This is the ONE probe that would move 1A, and it was
  not run.
- **G-1A-2 [displacement lever dead].** Confirmed (above). Not a gap that *closes* 1A — a gap that
  *sinks* its carrier rationale.
- **G-1A-3 [distinctive advantage MOOTED — NEW].** G3/let-gen was 1A's differentiator; PROTO-1
  retires it. 1A's carrier case now has **no** distinctive win, only distinctive *costs* (delete
  ~101 LOC, rewire ~91 sites, re-invent the `BoxedEnum→structural` refinement lattice inside
  union-find, author a net-new `*_to_ts` sink).
- **G-1A-4 [`HeterogeneousAltJoin` retirement unproven].** No probe shows the Join variant *retiring*
  under unification vs *coexisting*. Unchanged.
- **G-1A-5/6 [width + row unmeasured].** Verified zero row-shaped `->` in the real corpus (json's
  arrows are scalar leaf projections); Leijen-vs-Gaster-Jones row discipline is unfalsifiable on this
  instrument. Width residue unrun.

**Verdict.** Architecture endorsed; as a carrier, LEAN-against is now *strengthened to
near-eliminated* — its distinctive advantage mooted, its displacement lever dead, its iso path
categorically absent, and its one moving probe unrun. ~57%: the design is well-understood and its
disposition (bank-as-architecture, do-not-seat-as-carrier) is clarifying, but the port-cost race that
would formally close the fork is still SPEC_ONLY.

---

## §CRIT-3 — FAMILY 1B · ONE-CONSTRAINT-STORE · ~70% (was ~54%; the three headline gaps resolved, four remain)

**1B is the pass's big mover, and the movement is earned by RAN probes — not asserted.** Three of its
four largest gaps are resolved:
- **G-1B-1 (X8, was "FATAL unmet ×2") → proven-landable.** PROTO-2 lands the refinement-aware clash
  arm: `types_heterogeneous_alt` green, four `project_types_*` suites green, exactly one
  `TypeClash` on the synthetic fixture, `git diff crates/core/src/runtime/` = 0. I confirmed the
  patch's blast radius is carrier-independent (csp-solver accessor + `types/` pass + obligation enum
  + test; **zero runtime, zero codegen**). This ALSO closes, *for 1B*, the §0 zero-runtime gap that
  "all four assert, none exercise" — X8 exercises it on a landed change.
- **G-1B-2 (was "SHARPEST, §0 void: polymorphic soundness untested") → resolved as N-A.** PROTO-1:
  false fusion is unreachable; 1B ships the (now-struck) floor for free. The leading carrier's
  polymorphic soundness is not untested — the question is moot.
- **iso (G-XL-2) → MEASURED.** PROTO-2-iso: 1B's `TypeDesc` is the only carrier that crosses to a
  well-formed parallel TS type. The record vocabulary needs no LOCK-6 amendment.

**But the adversarial reading finds four unclosed gaps and two NEW faults the synthesis's number does
not price:**
- **G-1B-3 [consumer-less substrate, viii — OPEN].** PROTO-2 *confirms* `TypeClash` is parity-inert:
  codegen reads `ir.types` + `struct_registry`, never `type_obligations`. Its only consumer is 1C's
  **unbuilt** LSP bridge. The joint wave is now *co-named* (X8 ⊕ 1C's `type_obligations →
  lsp_types::Diagnostic{range}`) but **naming is not building**. Until the bridge lands, X8's
  producer feeds a void — Constitution viii stands open.
- **G-1B-4 [gate-that-cannot-fail — NEW].** PROTO-2's byte-identity proof is: refinements still land
  *because* "the refuse arm **never fires on real grammars** … it fires only on the synthetic clash
  fixture." That is a double edge the synthesis reads as pure win. If the refuse arm never fires on
  any real corpus, then the *actual deliverable* (clash detection) has **zero real-corpus exercise** —
  validated only on a hand-built fixture. Combined with G-1B-3, X8 is substrate whose **producer
  fires only synthetically AND whose consumer does not exist**. Defensible as prospective
  soundness-hardening (§56 no-silent-epsilon), but the convergence ledger must not score X8 as an
  *exercised, shipping* deliverable — it is a guard against a clash no shipping grammar produces.
- **G-1B-5 [X8 is a PARTIAL hardening — NEW scope gap].** PROTO-2's `is_fully_ground` **deliberately
  treats `Enum` and `Named` as non-ground** (to avoid misreading legal vec-context/map-result
  bridges as clashes). Sound for those bridges — but it means the clash arm **cannot catch a
  disagreement at the `Enum`/`Named` level** (two disagreeing `Named` map-results still LWW-overwrite
  silently). The registry's X8 was "raise-on-disagreement hardening of `assign`," full stop; the
  probe delivers "raise-on-*fully-ground*-disagreement." Whether the ground-only arm is the *complete*
  X8 or a first slice is unresolved.
- **G-1B-6 [residue census + B4 STILL UNRUN — the distinguishing thesis untested].** Neither
  second-round probe touched B3 (product-lattice residue census on a modeling instrument with
  overload/legality/decode-class present) or **B4** (per-variant `Sum` store shape vs unsound flat
  cell — research-B §3's "sharpest constraint on store shape"). The "residue ≈ 0" claim **still reads
  its own coarse output over a corpus with zero overload code** — spec-cites-itself, unchanged across
  two passes. 1B's *distinguishing* claim ("one store carries all five judgment axes") is unexercised;
  what is proven is the *structural-equality* store it already was.
- **G-1B-7 [single-store thesis = GESTALT-B, unchanged].** The literal one-heterogeneous-store is a
  LOCK-1⊕LOCK-2 merge over the shared vendored `csp-solver` — correctly BLOCKED-candidate, routed to
  GESTALT-B. This alone caps 1B's *within-LOCK-1* convergence below 100%: the family's namesake
  cannot be seated here.

**Verdict.** LEADING carrier, and the lead is now measured, not asserted — a genuine jump. But the
jump is on the *equality-carrier + iso + X8* axes; the *product-store distinguishing thesis* (B3/B4)
remains unrun, the obligation remains consumer-less, X8 is unmerged and ground-only, and the namesake
single-store is a GESTALT-B call. ~70%: real convergence on "1B is the carrier," not-yet on "1B is
the one heterogeneous store."

---

## §CRIT-4 — FAMILY 1C · BIDIRECTIONAL-LOCAL · ~42% (essentially unmoved; make-or-break not run)

**Correctly BANKED as a compatible overlay on the winning trunk.** The mechanism (check arm at the
`->` node + span field on the obligation + shared reconciler) is clearly specified and TS-lowerable by
construction (payload is `TypeDesc` + span). No dispute on the design.

**The pass did not run 1C's decisive experiment.** R-C2 (the side-by-side localization proof on a
hand-authored declared/synthesized *mismatch* fixture) is 1C's self-declared make-or-break, and it was
**not** the probe target. So 1C's distinct identity — "check moves the span to the `->`" — remains
**unfalsified and unfalsifiable on the real corpus** (json's 4 arrows are all scalar leaf projections,
none a mismatch). This is unchanged from the prior pass.

**Enumerated open gaps (1C):**
- **G-1C-1 [make-or-break has NO INPUT, STANDS].** No typed-mismatch `->` corpus exists; blocks on
  GN-3.W3 or a hand fixture. The one experiment that reactivates 1C to ADVANCED was not run.
- **G-1C-2 [GN-3.W3 wall].** Inline `(string -> key)` trips the typed-materialization invariant
  (`lower/expression/wrap.rs`); only the rule-level form clears it. Caps 1C's addressable surface.
  Unchanged.
- **G-1C-3 [consumer-less, viii — now CO-NAMED, still UNBUILT].** PROTO-2 firms the plan: 1C's
  span-bearing obligation and X8's `TypeClash` share the *same* unbuilt LSP bridge, and PROTO-2
  populates the `node: Option<NodeId>` range anchor. Progress on *substrate readiness*; zero progress
  on the *consumer*. The joint wave is named but unscheduled — it still risks falling between
  carrier-selection and GESTALT-B.
- **G-1C-4 [sub-span ceiling, a LOCK-3 field].** `Project.ty` is one whole-expr `String` span; a
  wrong ascription cannot underline the offending sub-term. The thesis's quality ceiling is a sub-span
  the IR does not carry. Unchanged.

**Verdict.** Disposition correct (BANKED, reactivation-gated on R-C2). Convergence essentially
static; the small +2% over the prior number is only the joint-wave substrate firming (node anchor
populated by PROTO-2), not any test of the localization thesis itself. A pass that resolves 1B's
briefs while leaving 1C's make-or-break untouched has not converged 1C — it has deferred it.

---

## §CRIT-5 — FAMILY 1D · TYPES-AS-ECLASSES · ~44% (unmoved; engine dead, hook re-homed, residue = vocabulary)

**Correctly BANKED; three legs RETIRED as named negatives** (recursion-keep beats only a strawman;
"zero new engine" FALSIFIED — the `union` amendment is reverted at HEAD and is a 3-lock blast radius;
cost-extraction IDLE with overload residue = 0). No dispute.

**The one surviving hook (structural `Row`-from-body derivation) is UNPROVEN and its home is not
LOCK-1 — I concur, and the tree confirms it.** `TypeDesc` at HEAD has **no `Row` and no `Sum`**
(verified: only `HeterogeneousAltJoin`), so the Row vocabulary (X4) is itself still unbuilt at the
tree of record. The D1 make-or-break (derive `Row` from an un-annotated `object` body without a
brace hint) remains UNBUILT — the first-round probe measured `Unknown` on the real body; deriving
`Row` needs a `->`-declared `key` role, which is engine-agnostic (any carrier reads it once declared).
**Extraction ≠ inference:** every probe proved 1D can *extract* `List ≠ Row` given a `Row` node; none
proved it *derives* the classification. That missing primitive is of the same difficulty as the
original problem, and it is not e-graph-specific.

**Enumerated open gaps (1D):**
- **G-1D-1 [D1 derivation UNBUILT, re-homed to GN-1].** The true make-or-break; not run this pass.
  Correctly re-homed to the GN-1 loom (X9) as a LOCK-7⊕LOCK-3 codepath-unification question, not a
  LOCK-1 recursion race.
- **G-1D-2 [name-collision, vi].** A net-new `TypeAnalysis` collides with the live
  `driver/analysis.rs` struct (I confirmed `TypeAnalysis::from_ir` at `analysis.rs:22`). 1D-as-engine
  is a Constitution-vi violation.
- **G-1D-3 [no TS path, iii].** A `TypeENode` is not a `TypeDesc` → no `type_desc_to_ts` mirror
  (ranks last on iso with 1A, per PROTO-2-iso).
- **G-1D-4 [3-lock blast radius].** The `union` amendment fires on LOCK-2 grammar-tier and LOCK-5
  regex-tier — a GESTALT-B/LOCK-2/LOCK-5 saturation-perf call, never a free LOCK-1 hook.

**Verdict.** BANKED; engine thesis dead; LOCK-1's residue from 1D is exactly the engine-agnostic
vocabulary `TypeDesc::Row`/`LayoutKind::Row` (X4), which *any* carrier populates — dissolving 1D's
sole advantage into an IR-expressiveness fix. Within LOCK-1 the *disposition* is fully clear (bank,
keep vocabulary, re-home the hook), but the *family-as-engine* is refuted; ~44% reflects a clear
disposition over a dead thesis, not a converging design.

---

## §CRIT-6 — INCOMPATIBILITY MAP: soundness check

The §2 map is internally sound and I ratify its structure with two amendments:
- **INC-1 (the carrier fork).** The synthesis kept the fork "open" on the contingency that let-gen
  might force the union-find re-seed (re-inflating 1B's cost to 1A's). **PROTO-1 closes that
  contingency:** let-gen is inapplicable, imported by neither carrier, zero-diff on 1B. The fork no
  longer hinges on build-cost-via-let-gen. It now rests solely on (i) the *base* build-cost (1B
  additive net-0 / X8 proven-landable vs 1A delete-101-LOC-+-rewire-62-files-+-re-invent-refinement-
  lattice-+-author-new-`*_to_ts`) and (ii) iso (1B crosses, 1A categorically cannot). **Both
  LEAN-1B, and the contingency that kept INC-1 from CLOSING is gone.** The synthesis should record
  INC-1 as CLOSED-for-1B-as-carrier (the iso seat still routes to LOCK-6 to *render*, but the
  within-lock carrier adjudication is no longer contingent). This is the single largest bookkeeping
  correction the pass owes.
- **INC-4 / single-store.** Correct — GESTALT-B. No change.

---

## §CRIT-7 — CHECKLIST SWEEP (explicit)

- **Vacuous convergence:** FOUND (§CRIT-0) — the 43–55% floors were measured against HM(X)-with-let-
  gen, a requirement the problem class lacks; PROTO-1 retires it. Corrected by striking let-gen from
  the floor + adding G-CRIT-α.
- **Spec-cites-itself:** FOUND (G-1B-6) — "residue ≈ 0" still reads its own coarse output over a
  zero-overload corpus, two passes running.
- **Gate-that-cannot-fail:** FOUND (G-1B-4) — X8's byte-identity holds *because* the refuse arm never
  fires on real grammars; the clash-detection deliverable has zero real-corpus exercise.
- **Elegant-reduction trap:** FOUND (§CRIT-0 / G-CRIT-α) — "monomorphic by construction" rests on a
  beta-reduction path whose closure lowering panics (`lower/expression/mod.rs:168`). Load-bearing
  "and-then-beta-reduction" step is broken; the floor is delivered only because polymorphism is absent
  AND non-functional. Needs a CI-enforced monomorphization-invariant gate.
- **Legacy aliases:** none new; XL-F name-collision risk (1D `TypeAnalysis`) already flagged G-1D-2.
- **Masked fallbacks:** the `json_templates` / `has_rule("value")` mask is correctly re-homed to GN-1
  (X9); LOCK-1's residue is the `TypeDesc::Row` vocabulary — verified still absent at HEAD, so X4 is a
  real open IR fix, not a done deal.
- **Unverified gestalt:** the two GESTALT-B routings (single-store merge, Open-X residue double-home)
  are correctly deferred, not asserted.
- **Consumer-less substrate:** FOUND (G-1B-3, G-1C-3) — `TypeClash` + span-obligation both feed an
  unbuilt LSP bridge; co-named but unscheduled.
- **Constitution v (zero runtime):** now *exercised* on a landed change for the first time (X8 patch,
  `git diff runtime/` = 0) — a genuine close for 1B; still asserted-not-exercised for 1A/1C/1D.
- **Prototype-evidence misuse:** X8 must not be logged as "shipped" — it is unmerged (raw LWW at
  HEAD, verified) and its consumer is absent. "Proven-landable," not "landed in the tree of record."

---

## §CRIT-8 — NEXT-PASS DIRECTIVES (what would actually converge the lock)

1. **Re-cut §0.** Strike let-generalization from the settled floor; record X10/the "void" as
   RETIRED-by-PROTO-1 (inapplicable, not unbuilt); add **G-CRIT-α** (a CI-enforced invariant: no
   parametric/`Call`/`Closure` IR node survives lowering into the type pass — the checkable form of
   "monomorphic by construction"). Fix the closure-lowering panic or explicitly ledger the surface as
   frozen-monomorphic.
2. **Close INC-1 for-1B** in the bookkeeping (the let-gen contingency that kept it open is gone).
3. **Merge X8 into the tree of record** (raw LWW → refinement-aware arm) and **schedule the joint LSP
   wave** so `TypeClash` + the span-obligation cease to be consumer-less. Decide whether the
   ground-only arm (G-1B-5) is the complete X8 or a first slice (author an `Enum`/`Named`-level
   disagreement fixture).
4. **Run the unrun distinguishing probes** — 1B's B3/B4 (product-lattice residue census + per-variant
   `Sum` store shape) on a *modeling* instrument, and 1C's R-C2 on a hand-authored mismatch fixture.
   These are the three experiments that would move 1B past 70% and 1C past 42%; the pass ran none of
   them.
5. **1A port-cost (G-1A-1)** stays SPEC_ONLY; either build Option A to formally close the fork or
   ratify 1A as banked-architecture / not-carrier and stop scoring its unbuilt parity race.
