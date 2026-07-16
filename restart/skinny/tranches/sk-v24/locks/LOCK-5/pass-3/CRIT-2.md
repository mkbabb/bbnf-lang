# LOCK-5 — THE REGEX CONTINUUM — pass-3 CRIT-2 (adversarial critic 2 of 2, cross-lock weight)

Critic 1 hardens intra-lock; my remit (constitution ix) is the **cross-lock view** plus the
failure-mode checklist and the constitution. I read PORTFOLIO (LOCK-5 + constitution),
CROSS-LOCK (XL-A…H, conveyed as binding fact), the pass-3 FAMILY-SPECS, all three prototypes
that RAN this pass (PROTO-1, PROTO-2 fire-census, **PROTO-2-5C**), research-{A,B,C,D,priorart},
the pass-2 registry + both pass-2 critics, and spot-verified the two load-bearing claims my
headline rests on.

**Verified in-tree / on-disk this pass (run/grepped, not read from the reports):**
- `FAMILY-SPECS.md` mtime = **15:27:43**. `PROTO-2-5C.md` mtime = **16:07:57**. `grep -c PROTO-2-5C
  FAMILY-SPECS.md` = **0**. The synthesis predates the 5C reference-control run by 40 minutes and
  references it **nowhere**. (§0 — the headline.)
- `follow_sound_fraction.rs:114` — `match analyze(pattern).first { … }`; line 112 comment: *"`Unknown`
  ⇒ … full alphabet (sound over-approximation)"* — but the **non-Unknown** path takes `analyze().first`
  verbatim, which PROTO-1's own Caveat 1 proves UNDER-approximates alternations (`sheet_prefix` returns
  `Exact{'\''}`, dropping the letter branch). The over-approx source (DFA start-state class set) is
  **not** wired. 5D's soundness gate is unbuilt exactly as the caveat says. (§5D.)
- Base-pin split: PROTO-1 + PROTO-2 pin `HEAD = ce51c36fc` (bbnf); PROTO-2-5C pins
  `HEAD = ef10d5b78` (parse-that, READ-ONLY). Both legitimate under XL-C (one-regex-home = parse-that
  for the engine). But FAMILY-SPECS §headline asserts *"Two prototypes ran, both base-pinned to
  `ce51c36fc`"* — it does not know the third exists. (§0.)

None of the three prototypes fabricate. PROTO-2 and PROTO-2-5C are genuinely good science —
model-independent structural results (proj `total_applied=0`; cong passes≈n allocation-independent),
honest about their own confounds (the fire-census does not count real-grammar bare-vs-projected
frequency; the 5C Valmari lane is allocation-confounded and explicitly not load-bearing). **The
defect is not the code. It is, a SECOND consecutive pass, the synthesis that sits on top of it.**

---

## 0. THE HEADLINE — THE STALE-SYNTHESIS DEFECT RECURS: FAMILY-SPECS CONTRADICTS ITS OWN pass-3 5C PROTOTYPE

Pass-2 CRIT-2's headline was: *"the pass-2 FAMILY-SPECS is STALE against its own pass-2 prototypes"*
— it declared "no prototype ran" while PROTO-1 (5B RETIRE) and PROTO-2 (5A REFUTED) had run the very
briefs it nominated for pass-3. **That exact failure mode has recurred, un-fixed, in pass-3, on 5C.**

The pass-3 FAMILY-SPECS declares, in four places:
- §headline: *"What did NOT run (a third pass in a row): **5C's reference-Hopcroft control** (the
  automata-as-extraction centerpiece) … 5C's centerpiece is the lock's one genuinely-unrun decisive
  number and is **Brief 2**."*
- §5C: *"centerpiece **unrun a THIRD pass** + prior-art-predicted loss"*, convergence **held at 45%**,
  *"Brief 2 forces the number that ends it."*
- §Brief 2: a full pass-4 method to run the reference control.

But the same directory holds **PROTO-2-5C.md (16:07)** = Brief 2, **RAN** on parse-that HEAD
`ef10d5b78`, verdict **`RAN_MEASURED — FAIL (the congruence centerpiece RETIRES)`**:
- oracle 0/56 divergences (all four minimizers + incumbent byte-identical; ρ=1 across the grown
  corpus);
- the `DfaSig` congruence rebuild degenerates to **O(n²·k)** on exactly the acyclic chain-DFAs it
  exists to accelerate (`[01]{8}`: n=9 → **8 passes-to-fixpoint**, allocation-independent, unambiguous);
- `incumbent ≈ null` (<1%): the `:358` strawman was **never** the source of any "acyclic-beats-Hopcroft"
  gap at regex scale — the e-graph centerpiece had no cost-model foe to defeat;
- the e-graph lane is, on top, the **slowest correct lane** on acyclic (cong 37664 > null 24376 >
  revuz 17168).

**So Brief 2 was run inside pass-3, after the synthesis was written, and the decisive verdict landed
— and the synthesis was never reconciled to it.** The FAMILY-SPECS 5C convergence (45%, ADVANCED,
"centerpiece unrun") is the **pre-run floor**; the run supersedes it. A registry cut from FAMILY-SPECS
alone would record 5C's centerpiece as "still to be tested" when it has been tested and **refuted**.
This is the checklist's **unverified gestalt** in its purest form, and it is the **same defect the
pass-2 critic already named** — a process failure, not a one-off. **Every 5C line below is re-cut from
PROTO-2-5C, not the stale synthesis.**

The reconciled pass-3 truth for 5C: the automata-as-extraction centerpiece is a **run-earned negative
result** (disposition-certain, exactly the shape of 5B's RETIRED). What survives — 2C-as-table
(settled) + delete-`:358` (mandatory bug-fix) + a single reverse-topological **Revuz** pass — needs
**no e-graph-over-automata mechanism at all**. The family qua "e-graph automata beats the classical
pipeline" is refuted; it dissolved into three non-family-specific deliverables. **Prior art
(research-priorart TRAP A, Remy Wang) PREDICTED this exact loss**, and the run confirmed the
prediction — the constitution's own disposition (vii: named negative results are science).

---

## FAMILY 5A — ONE-HIR — CRIT-2 floor **45%** (maximal REFUTED pass-2; residual is refactor-shaped; NO 5A prototype ran a THIRD pass; the gate pass-2 flagged as unmovable is unmoved again)

FAMILY-SPECS carries 45% and I concur on the number — but the **reasons** are worse than the prose
admits, and one is a verbatim repeat of a pass-2 CRIT-2 finding.

- **G5A-a (the `/foo$/` Look-consumer gate is STILL unbuilt — THIRD pass; gate-that-cannot-fail,
  unmoved twice).** Pass-2 CRIT-2 §G5A-d struck this exact gate: *"the test was not built … Y.13 on
  `Look::Start` stands exactly where pass-1 left it. Gate-that-cannot-fail: unmoved."* Pass-3
  FAMILY-SPECS still lists G-5A.2 as *"UNBUILT"* and calls the test *"buildable."* Three passes, a
  named live correctness defect (`RegexKind` has no anchor variant → `/foo$/` and `/foo/`
  indistinguishable, both over-accept), a test the synthesis itself says is buildable — and it has
  never been built. A gate that is "buildable" three passes running and never built is a gate that
  cannot fail. **Unmoved.**
- **G5A-b (elegant-reduction, honestly downgraded — CREDIT).** research-A's concession that the
  1148-LOC classify **RELOCATES, not shrinks** (its `AccelDriven`/`PrefixThenClass`/`CharClassQuantified`
  tier content is real information an IR pass keeps) directly answers pass-2 CRIT-2 §G5A-e. This is
  genuine honesty and I credit it — but it also confirms the maximal "1148-LOC collapse" was
  overclaim, and what remains (delete the ~200-LOC name-gate + the 322-LOC shadow crate) is a
  **refactor**, not an architecture. The family's distinguishing mechanism is dead; FAMILY-SPECS
  half-concedes this (*"the residual is refactor-shaped, not architecture-shaped"*).
- **G5A-c (the shrink is contingent on paying the parser import — net-shrink UNVERIFIED).** G-5A.parser-cost:
  deleting the heuristic forces skinny to consume a REAL parser (parse-that `hir/parser/` ~787 LOC).
  Whether the shadow-deletion **net-shrinks** after the import is paid is an open question the
  synthesis names but does not answer. A "shrink" that imports 787 LOC to delete 522 may be a growth.
- **G5A-d (grammar-driven SPEC-ONLY — constitution i, THIRD pass, ZERO artifacts).** Re-scoped to
  "promote + de-dup + break-the-discard" (correct framing) but un-executed; `parse_generated` still
  discards its AST; two `regex.bbnf` still diverge. Deferred to LOCK-7/7A a third time. (§3.)
- **G5A-e (automata parity from the narrowed type — G-5A.3, UNRUN THREE PASSES; SPEC_ONLY anchoring
  a perf claim).** research-B argues `from_expr` is moot under the survivor (the bridge routes to the
  engine's single `from_hir`), so there is no second NFA builder to keep in parity — a reasonable
  narrowing, and if it holds G-5A.3 retires as moot. But PROVE-10's 8.8–29.5× band has **never** been
  reproduced from any narrowed/merged type in three passes; constitution v (zero-runtime-cost of the
  narrowed automata path) is unverified. The claim rides SPEC-ONLY.

**Assessment. 45% (held — stalled).** Held at 45% for a THIRD pass with a refactor-shaped residual,
no prototype, and the same Look-consumer gate unmoved. This is not convergence-toward-shipping; it is
a stable diagnosis. I do not lower it (the analysis genuinely sharpened — classify-relocates conceded),
but I flag that a family parked at 45% three passes with all gates unbuilt is a candidate for
**re-labeling from a family to "the survivor's bridge-hardening refactor residual"** — which
FAMILY-SPECS itself gestures at but does not commit to.

---

## FAMILY 5B — LAWFUL-FUNCTOR — CRIT-2 floor **40%** (RETIRED; PROTO-2 strengthened the retire, honestly; conditional on an unratified LOCK-1 fact)

I concur with RETIRED at 40%. PROTO-2 (fire-census) strengthened the pass-2 retire from *estimated*
to *measured* fork-independence: on projected branches the three regex-specific rules apply
**exactly 0** work (`proj total_applied=0`, one iteration, no unions) — the 250-LOC de-dup the functor
claimed dies **for free under Fork-B**, attributable to no family. That is clean.

- **G5B-a (retire conditional on the unratified `->` representation — carried, correct, cross-lock).**
  The retire rests on `->` staying a whole-leaf wrapper (`Project` skinny / `Map` top-level) that
  never binds a sub-leaf capture. LOCK-1 owns the real `->` typing and has **not** ratified it.
  Sub-leaf-capture representation un-retires 5B AND breaks 5D's `Project` byte-identity oracle. The
  synthesis records this correctly as the sole re-open trigger routed to LOCK-1. Good.
- **G5B-b (research-B's "total reverse functor" must not launder credit — correctly firewalled).**
  FAMILY-SPECS explicitly refuses to credit the shadow-deletion refactor (net −322, fixes the
  anchor-blind nullability bug) to 5B — *"crediting it to 5B would launder a retired family's
  disposition."* This directly discharges a laundering risk; I credit the discipline.

**Assessment. 40% (held).** Disposition-certainty of a conditional retire, raised in confidence by
PROTO-2. No dispute. The one open coupling (LOCK-1 `->` representation) is a cross-lock fact, not an
intra-lock gap.

---

## FAMILY 5C — AUTOMATA-AS-EXTRACTION — CRIT-2 floor **55%** (RE-CUT FROM PROTO-2-5C: centerpiece RUN-EARNED RETIRED, disposition-certain; the surviving deliverable needs NO e-graph; FAMILY-SPECS 45%/unrun is STALE)

**The FAMILY-SPECS number and status are stale (§0). Re-cut from the run:**

- **The centerpiece is RETIRED by a run, not "unrun."** PROTO-2-5C discharged the "decisive number unrun
  three passes." The `DfaSig` congruence rebuild is O(n²·k) on the target acyclic class (passes ≈ diameter
  ≈ n; `[01]{8}` n=9 → 8 passes; `log n` held only for shallow bushy alternations, refuted for every
  bounded chain — precisely the class the fast-path exists to accelerate). The e-graph lane is the slowest
  correct lane on acyclic. **The maximal claim ("e-graph automata beats the classical pipeline") is
  run-refuted.** This is a real pass-3 advance — the family's decisive question is ANSWERED — and the
  synthesis missed it because it was written 40 minutes early.
- **`incumbent ≈ null` erases the family's premise.** Deleting the `:358` full-restart moves the sum
  <1% on both tags. The "strawman" the whole family was built to beat **was never slow at regex scale**
  — so the centerpiece had no foe. This is the deepest refutation: not "the e-graph loses the race" but
  "there was no race to win."
- **Prior art PREDICTED this (research-priorart TRAP A) and the run confirmed it.** *E-graphs ARE tree
  automata; the direction that works is automata→e-graph, not eqsat→automata; DAG-extraction is NP-hard
  and dominates eqsat time.* The centerpiece ran the isomorphism backwards; the measured O(n²·k) is
  exactly the predicted loss. Constitution vii is satisfied by a **named negative result**.
- **What survives needs no e-graph:** 2C-as-table (research-C's zero-witness grep settled it — no
  `lut_budget`/`codegen_budget`/`shared_lut`/`table_budget` coupling anywhere in `crates/ir/src/` → tier
  selection is LOCAL → the monotone `EngineSet` table wins, CSP over it is Y.13 substrate); the
  delete-`:358` bug-fix (mandatory, correctness-neutral, 0 divergences); and a single reverse-topological
  **Revuz** pass (the only lane whose passes stay at 1 as n grows — a structural/table optimization).
- **2B-as-minimizer BLOCKED (Kozen) — cleanest result in the lock, unchanged.** No finite ground rewrite
  set is complete for star-equivalence. 2B-as-determinizer survives (25/26 ρ=1); 2A/Hopcroft closes.

**Open gaps (the survivor's, not the centerpiece's):**
- **G5C-INT (survivor integration UNBUILT).** The minimize-swap (`dfa.rs:89-91`), the Valmari-or-Revuz
  native partial-DFA handling of `DEAD` (`:330/:340/:423`), and the tier-table home
  (`info/mod.rs:344`) are specified but not landed. The FAIL disposed the centerpiece; it did not ship
  the survivor.
- **G5C-VALMARI (the "correct control" head-to-head is confounded).** PROTO-2-5C's Valmari lane is
  allocation-confounded (`Vec<Vec<Vec<u32>>>` + per-splitter HashMap) and explicitly NOT load-bearing;
  the FAIL rests soundly on the allocation-independent passes-count + `incumbent≈null`. But this means
  the clean "a correct O(n log n) Hopcroft beats congruence on timing" comparison was **not** produced
  — only the passes-count proxy. Sufficient to dispose the centerpiece; insufficient to size the Revuz
  win against a de-confounded control (the report says `null/revuz=1.42×`, modest).
- **G5C-2C ⊗ LOCK-2 — single-home unratified (cross-lock; §3).** research-C discharged the witness
  search (zero global-budget coupling) → LOCK-5 proposes single-homing the tier table here. LOCK-2 has
  not deferred it. Still LOCK-5's unilateral proposal.

**Assessment. 55% (RE-CUT UP from the stale 45%).** The number RISES because the decisive control RAN
and disposed the centerpiece cleanly — disposition-certainty is now high, mirroring 5B's 40%
RETIRED-with-evidence but higher because a concrete survivor ships (2C-table + Revuz + `:358` fix). It
is a **convergence-of-disposition** number, not a shipping number; the survivor's integration is
unbuilt. **Pass-4 must NOT re-budget Brief 2 — it is discharged.** The remaining 5C work is
integration + the LOCK-2 ratification, not another race.

---

## FAMILY 5D — GRAMMAR-FRAGMENT — CRIT-2 floor **43%** (PROTO-1 RAN the fraction, but under an under-approx source its own author flags as unsound-for-the-verdict; substrate + cold-pair unbuilt)

FAMILY-SPECS raised 35→45 on PROTO-1. The raise is partly earned and partly over-credits a single
directional number whose soundness the prototype **itself disclaims**.

- **G5D-a (the decisive number ran under an UNSOUND source — prototype-evidence over-reach).** PROTO-1
  Caveat 1 (verified: `follow_sound_fraction.rs:114` uses `analyze().first`): the fraction ran on an
  **under-approximating** first-set, so the *"truly SOUND (3)"* verdicts *"are not reliably sound."*
  The direct-scan-default direction is robust for the **structural** partition (6/16 bounded-by-construction
  is first-set-independent; the counterexample lands non-disjoint regardless) — but the exact
  genuine-give-back count and the "sound" trio are **contingent on an over-approx re-run that does not
  exist** (Brief 1, UNBUILT). Presenting "direct-scan-default RATIFIED" on this basis leans on a number
  more confident than its source. The load-bearing step — *"draw first(C) from the DFA start-state class
  set"* — is the elegant-reduction's unconverged hard part.
- **G5D-b (the `ByteClass` substrate does not fire — masked-fallback, source-confirmed, UNBUILT).**
  `derive_recognizers` (`passes/src/lib.rs:338`) emits `SimdScan` only for structural bytes or a
  `QuotedString`-keyed `Regex`; a general `[a-z]` falls to `_ => {}` — the masked wildcard. No
  `Recognizer::ByteClass`, no `BackendExpr::ByteClass` anywhere (grep: zero). 5D's entire direct-scan
  chooser is not firing. The fix (add the variant, gate on the FOLLOW fixed point, route to the tape) is
  unbuilt — same as pass-1 and pass-2. (§G5D-tape.)
- **G5D-c (the entire cold-pair UNBUILT — G-5D.3).** Class-scan vs prefiltered `Dfa::find`, cold-paired,
  rare-class AND dense-class: never run. The ratio that decides whether the DFA leaves the generator path
  or stays reachable is unmeasured after three passes. The "DFA→narrow fallback" verdict is therefore
  **not yet measured** — it is inferred from the (unsound-source) fraction alone.
- **G5D-d (PROTO-2 fire-census is genuine evidence FOR 5D — CREDIT).** 5D's direct lowering does not
  route through the grammar-tier regex e-graph (proj 0/0/0), so keeping one on the generator path is
  un-consumed substrate. This is a clean, correct use of PROTO-2's result.

**Assessment. 43% (essentially held; I discount the 45→ raise by 2 points).** PROTO-1 is real
momentum and the structural partition is robust — but the raise credits a *soundness-caveated*
directional number as if the verdict were settled, and the substrate + the entire cold-pair remain
unbuilt three passes running. The verdict is directionally right and evidentially soft; 43% registers
both.

---

## 2. CROSS-LOCK VIEW (constitution ix — my primary remit)

**Credit:** the pass-3 FAMILY-SPECS carries the seven-item DOUBLE-HOMES section and the
INCOMPATIBILITY MAP forward, and correctly folds XL-C (two domain-fit types + bridge) as settled
fact. That is the right posture. But every resolution is still a **one-sided LOCK-5 proposal**; none
is ratified by the counterpart lock, and two cross-lock consequences are now **sharper** because of
the pass-3 runs:

1. **5C's refutation BINDS LOCK-2 (new, load-bearing).** LOCK-2's charter offers `2B-IS-EGRAPH` (the
   middle IR is an e-graph end to end) and `2C-CSP-CENTRAL` (the decision driver IS a CSP). PROTO-2-5C
   + research-priorart TRAP A jointly establish that **automata minimization must NOT be an e-graph
   island and tier-selection must NOT be a CSP** — the e-graph-over-automata direction is run-refuted
   (O(n²·k)) and the tier has zero shared-resource coupling (local table wins; CSP is Y.13 substrate).
   This is a binding input LOCK-2 must consume at GESTALT: two of LOCK-2's candidate island/CSP sites
   are refuted at the regex layer. LOCK-5 names the tier single-home but does **not** name this
   sharper constraint on 2B/2C; GESTALT-Ω owes the reconciliation.
2. **Axis-1 verdict ⊗ LOCK-4-4D / LOCK-9-9C / LOCK-3-3C — RE-CUT still owed, still nobody's charter
   (carried from pass-2, UNMOVED).** XL-C ratified two-types+bridge, but 4D's premise ("one HIR, not a
   second engine") is weakened, and **9C depends on a `ByteClass` substrate that PROTO-1 confirms does
   NOT fire** (masked wildcard, G5D-b). So LOCK-9-9C is a **consumer waiting on an un-delivered
   producer** — the reverse of Y.13, and a real DAG hazard: 9C cannot converge until 5D lands the
   `ByteClass` recognizer, which is unbuilt three passes. FAMILY-SPECS names this ("the substrate
   LOCK-5 offers 9C is itself UNBUILT") but leaves it as a pass-4 Brief-1 dependency — meaning 9C's
   convergence is gated on a number LOCK-5 has not produced.
3. **5B ⊗ LOCK-1 re-open trigger — correct, unratified.** Carried faithfully; routed to LOCK-1 with
   PROTO-2 evidence. No dispute.
4. **rust/ts ⊗ LOCK-6 — the serde-ownership boundary surfaced in BOTH passes, unresolved.** Pass-2's
   PROTO-2 hit the `CharClass`/`Look` serde E0277 break; pass-3's PROTO-2-5C had to run in a READ-ONLY
   parse-that scratch (7 external patch prefixes repointed). The "who adds `CharClass: Serialize`"
   question lands squarely on the parse-that READ-ONLY / LOCK-6 boundary and is unresolved two passes.
   The mirror/skeleton must carry BOTH types + the automata crossing (PROVE-14 tape) — larger than
   "carry the merged enum." Still nobody's charter.
5. **Grammar-driven SPEC-ONLY — constitution i, THIRD pass, ZERO artifacts (the deepest gap).** The
   lock is literally "the grammar-driven regex engine." After three passes it has prototyped an
   IR-level fork census, an IR-level lowering fraction, and a parse-that automata race — **not one
   grammar-driven regex path**. `parse_generated` discards its AST; two `regex.bbnf` diverge. Every
   pass defers the namesake clause to LOCK-7/7A. A lock that never once exercises its own defining
   thesis across three passes has not tested whether the thesis holds. Every verdict rides
   SPEC-ONLY(grammar-driven); GESTALT-P must not read the SPEC-ONLY label as "converged pending
   LOCK-7" — it is "un-attempted three passes."

---

## 3. FAILURE-MODE CHECKLIST — HITS

- **Unverified gestalt (THE headline, RECURRING):** FAMILY-SPECS (15:27) predates and contradicts
  PROTO-2-5C (16:07) — declares 5C's centerpiece "unrun a THIRD pass / Brief 2" while Brief 2 ran and
  the centerpiece RETIRED. **Same defect pass-2 CRIT-2 named; recurred un-fixed.**
- **Gate that cannot fail:** G-5A.2 `/foo$/` Look-consumer test "buildable" three passes, never built
  — unmoved since pass-1; consumer-less `Look::Start` (Y.13) unmoved.
- **Elegant-reduction trap:** 5C "minimization as congruence closure" — the load-bearing
  "and-then-saturate-the-automaton" step RAN and degenerated to O(n²·k) (the trap made explicit and
  measured); 5D "draw first(C) from an over-approximation" is the unconverged hard part; 5A "automata
  at PROVE-10 numbers" (G-5A.3) unrun three passes.
- **Masked fallback:** `derive_recognizers` `_ => {}` swallows the desugared `ByteClass`; the 5D
  chooser is not firing — source-confirmed, unbuilt three passes.
- **Prototype-evidence misuse:** 5D's "direct-scan-default RATIFIED" leans on PROTO-1's fraction under
  an under-approx source its own Caveat 1 disclaims as unsound-for-the-verdict; PROVE-10's perf band
  un-reproduced from any narrowed type (SPEC_ONLY anchoring a perf claim).
- **Legacy aliases / divergent homes:** two `regex.bbnf` (one discards its AST); top-level
  `crates/bbnf-regex` legacy duplicate; the skinny shadow crate (anchor-blind classify).
- **Spec-cites-itself / circularity:** grammar-driven regex (the regex grammar defines regex; grammars
  contain regex) SPEC-ONLY and contradicted by two extant grammars; deferred to 7A a third time.
- **Consumer-less substrate (Y.13):** grammar-tier regex e-graph proven consumer-less under projection
  (PROTO-2 — a POSITIVE finding, correctly used FOR Fork-B); `Look::Start` inert; LOCK-9-9C is a
  consumer waiting on the un-delivered `ByteClass` producer (reverse-Y.13 DAG hazard).
- **Constitution:** (i) grammar-totality SPEC-ONLY, three passes, zero artifacts; (v) zero-runtime-cost
  of the narrowed automata path unverified (G-5A.3 unrun); (viii) `Look::Start`, 9C's un-delivered
  producer; (ix) double-homes named but unratified, and the 5C→LOCK-2 binding not yet named.
- **NOT a vacuous-convergence hit — credit:** 5C's centerpiece disposition is now RUN-earned (a genuine
  pass-3 advance the synthesis failed to ingest, not a failure of the prototype); research-A's
  classify-relocates concession and the 5B laundering firewall are honest discipline.

---

## 4. CONVERGENCE (CRIT-2 floor — re-cut from the PROTOTYPES, not the stale synthesis)

| Family | Status | % | One line |
|---|---|---|---|
| 5A ONE-HIR | ADVANCED (maximal REFUTED; refactor residual) | **45** | Held/stalled a THIRD pass; classify-relocates honestly conceded (credit); `/foo$/` gate buildable-but-unbuilt three passes (unmoved); net-shrink contingent on 787-LOC parser import; grammar-driven SPEC-ONLY |
| 5B LAWFUL-FUNCTOR | **RETIRED (earned-conditional)** | **40** | PROTO-2 measured fork-independence (proj `total_applied=0`); conditional on LOCK-1 keeping the whole-leaf `->` repr; laundering firewall correctly held |
| 5C AUTOMATA-AS-EXTRACTION | **centerpiece RETIRED (run-earned); survivor ADVANCED** | **55** | RE-CUT from PROTO-2-5C (which FAMILY-SPECS never ingested): cong O(n²·k), incumbent≈null, prior-art-predicted loss confirmed → 2C-table + `:358` fix + no-egraph Revuz; Brief 2 DISCHARGED, not pending |
| 5D GRAMMAR-FRAGMENT | ADVANCING (survivor's lowering policy) | **43** | PROTO-1 RAN the fraction but under an under-approx source it disclaims as unsound-for-verdict; `ByteClass` substrate un-firing + entire cold-pair unbuilt three passes; direction robust, verdict soft |

**Lock-wide.** No family converged. The single deepest defect is procedural and RECURRING: **the
FAMILY-SPECS is stale against its own pass-3 prototype (PROTO-2-5C) — the exact failure mode pass-2
CRIT-2 flagged, un-fixed.** The 5C centerpiece is disposed by a run and the synthesis does not know
it. The grammar-driven premise (constitution i) is SPEC-ONLY after THREE passes with zero
grammar-driven artifacts — the lock has never once exercised its own defining thesis. Two of LOCK-2's
candidate island/CSP sites (automata-min-as-e-graph, tier-as-CSP) are now run-refuted at the regex
layer, a binding input LOCK-2 must consume. **The FAMILY-SPECS 5C section must be rebuilt from
PROTO-2-5C before any registry is cut from it.**

**CRIT-2 pass-4 directives (ordered by what gates the most):**
1. **Reconcile the synthesis to PROTO-2-5C.** Rewrite 5C from the run: centerpiece RETIRED
   (run-earned, disposition-certain), survivor = 2C-table + `:358` fix + no-egraph Revuz. **Do not
   re-budget Brief 2 — it is discharged.** Record the same institutional guard the pass-2 critic asked
   for: reconcile FAMILY-SPECS to every prototype in-folder before cutting the registry.
2. **Name the 5C→LOCK-2 binding.** Automata minimization is NOT an e-graph island; tier-selection is
   NOT a CSP (run-refuted + prior-art). Route to LOCK-2 as a constraint on 2B/2C, not a proposal.
3. **Build the ONE gate that has been "buildable" three passes:** the `/foo$/` Look-consumer test that
   FAILS on the shadow path and PASSES through the bridge. A gate un-built three passes cannot fail.
4. **Close 5D's soundness gate (Brief 1 over-approx re-run) BEFORE claiming direct-scan-default
   ratified;** land the `ByteClass` recognizer so 5D's substrate fires and LOCK-9-9C stops waiting on
   an un-delivered producer; run the cold-pair.
5. **Exercise ONE grammar-driven regex path** (promote parse-that `regex.bbnf`, break the AST discard)
   — the lock's namesake, un-attempted three passes; SPEC-ONLY is not "pending," it is "never tried."
6. **Ratify (not declare)** the LOCK-2 tier single-home, the LOCK-1 `->` re-open trigger, the LOCK-6
   serde-ownership boundary, and the 4D/9C/3C re-cut with those locks at GESTALT — every double-home
   resolution is still LOCK-5's unilateral word.
