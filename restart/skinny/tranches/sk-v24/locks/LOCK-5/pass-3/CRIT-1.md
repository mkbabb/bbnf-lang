# LOCK-5 — THE REGEX CONTINUUM — pass-3 · CRIT-1 (adversarial, WITHIN-LOCK weight) — RE-CUT

Critic 1 of 2. I weight internal soundness of each family spec and whether the prototypes tested the
*sharpest* uncertainty. Settled ground (XL-C two-types + bridge; parse-that one-regex-home; base-pin
XL-H) is binding fact, not re-litigated. All timestamps, run outputs, and source claims below were
re-verified on disk this pass. This CRIT-1 supersedes the 16:17 CRIT-1 (which critiqued the earlier
15:27 FAMILY-SPECS and correctly forced the 5C re-cut); the synthesis was re-cut at **22:40** and DID
integrate PROTO-2-5C — 5C now reads RETIRED-centerpiece @ 55%, exactly the prior CRIT-1 correction.
That defect is fixed. **A new one, of the same family, has taken its place.**

---

## THE DOMINANT FINDING — the stale-synthesis defect RECURS A THIRD TIME, now on the lock's TITULAR deliverable, and this time against a prototype that PREDATES the synthesis by 2.5 hours

**FAMILY-SPECS.md mtime = 22:40. `PROTO-2-P2.md` mtime = 20:05 — 2h35m EARLIER.** The synthesis had
PROTO-2-P2 in its own directory and did not agglomerate it. Verified on disk (`ls -la`), verified by
content.

The FAMILY-SPECS asserts, as the lock's TITULAR-DELIVERABLE FLOOR, in **four** load-bearing places:

- Headline (line 70–71): "The lock's TITULAR thesis — grammar-driven — remains at **ZERO artifacts
  after three passes** and is the one genuinely **un-attempted** formulation."
- PRECONDITION P2 (line 89–96): "grammar-driven is SPEC-ONLY (re-confirmed; **ZERO grammar-driven
  artifacts after THREE passes**) … `parse_generated(&str) -> bool` still **discards its AST**
  (source-verified this pass)."
- Brief 2 (line 419–420): "Brief 2 forces the **lock's TITULAR deliverable** — the one formulation
  **ZERO passes have touched** — **producing the first grammar-driven artifact**."
- Brief 2 pass/fail (line 466): "the grammar-driven path has its **FIRST artifact**; SPEC-ONLY
  advances from **un-attempted** to measured-feasible."

**Every one of these is FALSE against the same directory.** PROTO-2-P2 (20:05) is the first
grammar-driven artifact, RAN_MEASURED: `regex::parse_generated` at **`mod.rs:34 -> Result<Hir,
ParseError>`** (NOT `bool`) is the real grammar-driven path — generated-parser → typed AST →
`host::lower` fold → bespoke `Hir` — **already wired end-to-end**, DFA-matching the hand parser on
**43/45** both-parsed patterns, raw AND through the XL-C `simplify_hir` bridge. PROTO-2-P2's FINDING 0
is explicit: "**Break-the-discard is NOT a code change to author — the Hir-returning path exists
today.**" The FAMILY-SPECS's P2 fixates on the WRONG symbol (`generated.rs:2288 parse_generated ->
bool`, a *diagnostic shim* that builds then discards) and misses the sibling `mod.rs:34` that returns
the Hir. Its "source-verified this pass" verified the shim and mislabelled the deliverable
un-attempted.

**Then it recurs a second layer.** The FAMILY-SPECS issues Brief 1 (5D ByteClass end-to-end + the
over-consumption correctness fix) and Brief 2 (grammar-driven break-the-discard + de-dup) as **the
two sharpest uncertainties for pass-4**. Both had already been executed by the time convergence talk
began: **PROTO-1-5D (22:51) IS Brief 1**, PASS on both clauses; **PROTO-2-P2-CLOSED (22:59) IS Brief
2**, closed to **56/56 both oracles, strict full-parity gate green**. Offering two already-executed
probes as "the two sharpest uncertainties for pass-4" is the strongest possible evidence the
synthesis did not agglomerate its own lock's run evidence — the identical failure pass-2 CRIT-2 named,
that pass-3's 15:27 synthesis committed on 5C, now committed a THIRD time on the titular thread.

This does **not** impeach the prototypes — PROTO-2-P2, PROTO-2-P2-CLOSED, and PROTO-1-5D are good
science (renumbering-invariant canonical-minimal-DFA oracle strictly stronger than match-equivalence;
host-fold attribution by raw-AST dump; the correctness counterexample machine-asserted). It impeaches
the synthesis's claim to be a converged agglomeration. **The FAMILY-SPECS must be re-cut against
PROTO-2-P2 / PROTO-2-P2-CLOSED / PROTO-1-5D before any lock-level convergence talk.**

---

## THE CORRECT GRAMMAR-DRIVEN DISPOSITION (what the re-cut owes) — advanced, NOT discharged

Being adversarial in both directions: PROTO-2-P2-CLOSED is strong but does **not** retire
SPEC-ONLY(grammar-driven). The honest disposition is **PARITY-DEMONSTRATED-IN-SCRATCH, gated on
LOCK-7A promotion** — a large move from the FALSE "un-attempted," but not "converged":

1. **It is a differential-oracle result (LOCK-7D shape), not the LOCK-7A bootstrap.** It proves the
   grammar-driven parser DFA-matches the *hand* parser 56/56. It does NOT exercise the
   committed-stage-0 + fixpoint-regen self-hosting that clause i / LOCK-7A actually names; the
   self-referential circularity (`regex.bbnf` embeds regex leaves to parse itself, priorart §1) is
   untouched. "Promote to production parser" remains LOCK-7A-gated.
2. **It ran in a READ-ONLY parse-that scratch, on parse-that's `bootstrap/regex.bbnf` + `host.rs`** —
   nothing landed in the tree of record. The fix is a **5-edit host-side re-sync** (`+50/−6` in
   `host.rs`), not a grammar/regen change; P2-CLOSED itself flags (caveat 1) that "grammar-carries-
   captures + regen" is the cleaner architecture and "remains the GESTALT-P call."
3. **56/56 is 54 oracle-verified + 2 reasoned.** P2-CLOSED caveat 2: bare `^`/`$` anchors and `\p{}`
   are "**not** in the 56-corpus … verified by reasoning, not by a divergence flipping to pass."
   Bare-anchor lowering is precisely the construct 5A's `/foo$/` Look-defect (below) says is
   mishandled — a corpus that would surface it is exactly the one absent.

**Net:** the titular-floor language ("ZERO artifacts / 0% evidenced / un-attempted") is wrong and
must be replaced with "grammar-driven parser demonstrated at full DFA-parity to the hand parser in
scratch (56/56 raw+bridge), host-side re-sync, de-dup surface quantified (retire
`grammar/misc/regex.bbnf`: 1 promote / 21 delete / 3 relocate); promotion + self-hosting-bootstrap
LOCK-7A-gated." That is a genuine converged *core with an exact gap*, which is what GESTALT-P should
receive — not the false floor.

---

## SECONDARY LOCK-LEVEL FINDINGS

**F-2 — the PROTO-2-5C run's own "NET-POSITIVE 62715 ns" headline is confounded; the synthesis
correctly avoided it, but the gap it papers over is real.** `PROTO-2-5C-run.txt` reports dispatch
net-positivity as `Σ(valmari−revuz) = 69501 ns` savings − `6786 ns` dispatch = **+62715 ns**. That
savings figure is measured against the **allocation-confounded Valmari lane** the report *elsewhere
disclaims as non-load-bearing*. Against the **fair `null` control** (the `:358`-fixed Moore), the same
run's acyclic slice gives `null−revuz = 24376−17168 = 7208 ns` — barely above the 6786 ns dispatch
cost, i.e. **+422 ns net, ~150× smaller than the headline**. FAMILY-SPECS §5C gap (3) correctly cites
the fair `7208 > 6786` number, not the run's inflated headline — **credit the synthesis for catching
its own prototype's confound.** But the consequence stands as an open gap: the `is_acyclic`→Revuz
fast-path is **marginally** net-positive on this corpus (n≤20), so it may not earn its dispatch and
the deliverable may reduce to research-C's conditional — "just replace restart-Moore with correct
Valmari-Hopcroft, **no dispatch**." Un-decided; needs the de-confounded Valmari control.

**F-3 — PROTO-1-5D discharges two of 5D's four named gaps; the sharpest (the selector wiring) it
honestly does NOT.** The FAMILY-SPECS 5D entry (55%) predates PROTO-1-5D (22:51). PROTO-1-5D moves
5D: (a) the over-consumption counterexample is now a **PROVEN LIVE bug** (current `match_leaf` WRONG
on 12/20 delimiter-free inputs, `aandb` over-consumes `[0,5)` and rejects a valid sentence) —
FAMILY-SPECS gap (3) "UNBUILT" is discharged; (b) the `SimdMode/Recognizer/BackendExpr::ByteClass`
substrate + `match_byteclass_guarded` **land in-tree and fresh-compile (EXIT 0)**, tape byte-identical
to `([a-z]-"and")+` on 20/20 — FAMILY-SPECS gap (1) "DESIGNED not built" is discharged. **But** the
test drives three **verbatim-port** scanners, not the emitted `generated.rs` (runtime-coupled,
uncompiled), because `derive_recognizers` (`passes/src/lib.rs:338`) is **still NOT rewired onto the
FOLLOW fixed point** — "the one designed-not-wired seam." So the datatypes + emitter primitives land
and the semantics are port-verified, but the **selector that routes real leaves to ByteClass vs the
masked `RegexProgram` is unbuilt** — the end-to-end chain is not exercised, and 9C's producer is not
delivered. PROTO-1-5D's verdict line "**LOCK-9-9C unblocked**" over-reaches the FAMILY-SPECS's own
more-honest double-home #4 ("de-risked, still **UNDELIVERED**"). Take the FAMILY-SPECS wording.

**F-4 — SPEC_ONLY-anchors-a-perf-claim: NOT triggered (discipline held).** proto-1-overapprox's
direct-scan speedups (3.246× on `hex6[rare]`, 2.812× on `lower[dense]`, `probe-run.txt`) are RUN
cold-paired (reps=240, inputs=256), tape byte-identical 256/256, first(C) a SOUND over-approximation
off the DFA start-state class set — not SPEC_ONLY. research-A line 138 explicitly refuses to transfer
the PROVE-10 8.8–29.5× prefilter band to a construction/scan-parity claim. No perf claim rests on
SPEC_ONLY. Credit.

**F-5 — the de-dup debt is now QUANTIFIED with a plan, not just "diverge."** The two `regex.bbnf` are
no longer an open hand-wave: P2 + P2-CLOSED independently recount **1 promote (`octal`) / 21 delete /
3 relocate ⇒ retire `grammar/misc/regex.bbnf`**, the bootstrap grammar the strict superset for the
regular fragment. FAMILY-SPECS §5A note (line 404) and Brief 2 still frame this as an open
"promote + de-dup gap"; it is measured. Credit the prototypes; the synthesis undersells them.

---

## FAMILY 5A — ONE-HIR — **CRIT-1 convergence: 42%** (concur with synthesis; parked refactor residual, re-label candidate confirmed)

The narrowed survivor-residual is internally sound and honestly bounded; the "classify RELOCATES not
shrinks" concession (research-A line 88–90, "≈0 net shrink") is a genuine anti-overclaim. But the
distinguishing mechanism is dead, the residual is refactor-shaped, and **zero 5A prototypes ran across
three passes.**

**Enumerated open gaps.**
1. **G-5A.shrink is the elegant-reduction trap, sign still UNKNOWN.** Delete the ~200-LOC name-gate +
   ~322-LOC shadow crate — *and then the hard part*: skinny must import a REAL parser (parse-that
   `hir/parser/` ~787 LOC). "delete 522, import 787" may be net GROWTH. FAMILY-SPECS flags this as the
   "CRIT-1 floor"; it remains UNBUILT and its sign is the family's sole distinguishing claim.
2. **G-5A.look is a LIVE correctness defect, buildable-but-unbuilt a THIRD pass.** `build_look`
   (`nfa.rs:254`) collapses all six `Look` to one zero-width state; anchoring is enforced OUTSIDE the
   automaton by `detect_anchored` reading the *Hir*, not the NFA — so `/foo$/` wrongly accepts
   `"foobar"` unless the search layer consults it. The failing-then-passing test
   (`search(compile("foo$"),"foobar").is_none()`) is the Y.13 consumer guard for any shared type
   carrying anchor semantics; it has never run. (This is the exact construct P2-CLOSED verified
   "by reasoning, not oracle" — the two open items point at the same untested surface.)
3. **G-5A.leak UNBUILT:** `[a-c]≡[a-z]` identical `Span<width>` graded against LOCK-1's real HM, not
   skinny's stub `BuiltinTy` — LOCK-1-gated, un-exercised.
4. **The one measurable win is credited to the survivor, not 5A.** The enum→`SpanFacts` collapse
   (first-sets triplicated 3×, already drifting — `bbnf-regex class_byte` returns `None` on `\d`-in-
   class, `regex_first` does not) is the only place lines disappear, and the FAMILY-SPECS itself
   (line 123–124) calls it "**a SURVIVOR win, not a 5A win**." A family whose non-dead content is
   homed elsewhere is at disposition-certainty, not convergence.

**Not BLOCKED** (no missing primitive). 42% holds — nothing moved it this pass; re-label to "the
survivor's bridge-hardening refactor residual" if parked a fourth.

## FAMILY 5B — LAWFUL-FUNCTOR — **CRIT-1 convergence: 40%** (concur; cleanest disposition)

The retire is sound; PROTO-2 legitimately strengthened it (projected-branch `total_applied=0` →
the ~215-LOC de-dup dies FREE under Fork-B). The win-laundering guard is correctly applied: the
de-dup and the reverse `Hir→String` unparse are credited to the bridge/5A-residual, NOT to the
retired functor (research-B firewalls its own reverse-functor away from 5B — discipline held).

**Enumerated open gaps.**
1. **The sole re-open condition is genuinely open.** The retire rests on `->` staying a whole-leaf
   `Project`/`Map` wrapper that never binds a sub-leaf capture — a LOCK-1 fact LOCK-1 has NOT
   ratified. Disposition-certainty of a *conditional* retire; 40% correctly refuses a shipping read.
2. **Consumer-assignment residual:** the total `Hir→String` unparse (bridge round-trip oracle) is
   nobody's gate today; FAMILY-SPECS homes it in 5A-residual/bridge-hardening — confirm it is
   actually carried there, or it is a consumer-less obligation.

## FAMILY 5C — AUTOMATA-AS-EXTRACTION — **CRIT-1 convergence: 55%** (concur; the re-cut correctly integrated PROTO-2-5C)

The 22:40 re-cut fixed the prior stale defect: centerpiece run-earned RETIRED (`[01]{8}` n=9 → 8
passes-to-fixpoint ⇒ O(n²·k) on the acyclic chains it exists to accelerate; `incumbent≈null` <1% ⇒ no
cost-model foe existed). 2B-as-minimizer cleanly BLOCKED (Kozen; greatest-vs-least fixpoint; `(ab)*`
loop states bisimilar but never bottom-up-congruent) — the lock's sharpest impossibility. Survivor
(2C-as-table + delete-`dfa.rs:358` + single-pass Revuz-or-plain-Hopcroft) is concrete. The FAIL rests
soundly on the allocation-**independent** passes measure, not the Valmari-confounded timing.

**Enumerated open gaps (the ACTUAL pass-4 residual).**
1. **The byte-rarity oracle is a NAMED MISSING INPUT** for 2C-as-table accel-byte quality
   (regex-automata's static ASCII-rank table, absent in-tree; "without it accel-byte selection is
   guesswork," research-C). The table is settled as the mechanism; one of its inputs is absent. **The
   real pass-4 5C uncertainty** — the synthesis lists it (line 260) but carries no gate for it.
2. **The de-confounded Valmari control is unbuilt** (CSR inverse transitions, three parallel
   partition vectors, absent-transition partial-DFA rule). The FAIL disposition holds, but the Revuz
   win's SIZE is not cleanly measured — see F-2: the acyclic dispatch is only marginally net-positive
   against the fair control (+422 ns, not the run's confounded +62715), so Revuz-with-dispatch may
   collapse to "correct Valmari-Hopcroft, no dispatch."
3. **All survivor integration UNBUILT:** minimize-swap (`dfa.rs:89-91`), Valmari/Revuz partial-DFA
   `DEAD` handling (`:330/:340/:423`), tier-table home (`info/mod.rs:344`), and the mandatory
   correctness-neutral delete-`:358` (0 divergences) — none landed.
4. **The CSP-over-tier escalation witness was never constructed.** The synthetic budget-cap crossover
   research-C promised (a K-leaf grammar where local argmax overflows the generated-size budget and
   the CSP demotes a leaf to fit) did NOT run in PROTO-2-5C. "Table wins, CSP consumer-less" is proven
   for *today's* codegen, not shown robust against the one condition that would flip it.

55% is convergence-of-disposition, high because the decisive control RAN and a concrete non-e-graph
survivor ships — NOT a shipping number (integration unbuilt).

## FAMILY 5D — GRAMMAR-FRAGMENT — **CRIT-1 convergence: 58%** (synthesis 55%; +3 earned by PROTO-1-5D discharging the substrate + correctness gaps, capped by the unbuilt selector wiring + the un-run wired fraction)

The distinguishing thesis (direct-scan-default, DFA off the generator path) is RUN-EARNED on a SOUND
over-approximating first(C) (proto-1-overapprox — this discharges the pass-2 CRIT-1 concern that the
verdict rode the unsound `analyze().first` predicate; the genuine trio holds at 3/16, machine-asserted
`flips == ["sheet_prefix"]`). PROTO-1-5D adds the proven correctness bug + the compiling substrate.
Two real advances; but the family's actual hinge is untouched.

**Enumerated open gaps.**
1. **`derive_recognizers` is NOT rewired onto the FOLLOW fixed point — the selector is unbuilt.** The
   substrate + both emitters exist to *receive* the verdict; the producer that decides which leaves
   get `ByteClass` vs the masked `RegexProgram` is the one designed-not-wired seam. Until it lands,
   no real grammar leaf is routed through the new path — the end-to-end chain is unexercised (the
   PROTO-1-5D test uses verbatim-port primitives, not the emitted parser). This is the elegant-
   reduction trap's residue: "direct-scan-default … *and then* wire the FOLLOW-fixed-point selector."
2. **The wired-fraction Q7 re-run over JSON/Sheets/CSS is UN-RUN — "the single most load-bearing open
   number" (research-D §10).** The synthetic 16-leaf sample gives a NON-EMPTY residue (7/16), which
   *reverses* the pass-2 "DFA has no codegen consumer" and would ADMIT the DFA to codegen; but if the
   WIRED fraction is ~100% FOLLOW-disjoint, the DFA has no generator-path consumer and Y.13 FORBIDS
   wiring it. This binary — DFA-in-codegen vs standalone-API-only — is undecided until the wired
   fraction runs.
3. **The residue-tier cold-pair (`match_leaf` vs `Dfa::find` on the 7/16 give-back leaves) is UN-RUN.**
   proto-1-overapprox timed `hex6[rare]`/`lower[dense]`, not the actual give-back residue; the
   per-leaf cost gate that decides which residue leaves earn the DFA is open (Q8.3).
4. **Capture-leaf → `ValueProject` byte-identity is UNBUILT, LOCK-1-gated** (structurally in place;
   the assert is the open follow-on; judge against RE2C TDFA(1), not re-invented — priorart).
5. **Corpus breadth:** 16 leaves, approximate sheets FOLLOW-wiring (39-rule subset, host-heavy rules
   approximate). Affects FOLLOW breadth, never the counterexample gate or the genuine trio — but the
   fraction's corpus-representativeness is untested until the wired grammars replace the synthetic
   sample.

**Not BLOCKED.** 58%: the substrate compiles and the correctness bug is proven (real, run-earned), but
the selector wiring + the wired-fraction that actually decides "does the DFA leave the generator path"
are both unbuilt — integration, and the decisive number, remain open.

---

## INCOMPATIBILITY MAP + CROSS-LOCK — soundness check (within-lock lens)

- Axis-1 CLOSED and 5A-residual ∧ 5D COMPATIBLE are internally sound; the shared `Look`/`ByteClass`
  chooser answering the SAME `/foo$/` Y.13 anchor question for both is correct — and it makes 5A's
  unbuilt `LookMatcher` a **joint** gate, not a 5A-only one (both families ride the same unanswered
  anchor consumer).
- 5C ∥ {5A,5D} orthogonality survives the centerpiece retirement (GRAFT-1/2 engine-side). Sound.
- The LOCK-1 re-open trigger correctly binds 5B-un-retire AND 5D-capture-oracle to the same
  unratified `->` sub-leaf-capture fact. Sound.
- **GRAFT-3 (5D direct-scan-vs-DFA ⊗ 5C-2C tier table) is a real coherence obligation, now sharper:**
  with PROTO-1-5D landing a `ByteClass` chooser AND 2C owning a per-pattern `EngineSet`, the
  "class-scan vs DFA" decision genuinely exists in TWO homes; the pass-4 substrate build must wire
  them as ONE dispatch point (the double-emit trap, priorart §4.10). Flag it as born-RED for GESTALT.

---

## CHECKLIST SUMMARY (failure modes)

- **Unverified / stale gestalt — HIT (dominant):** FAMILY-SPECS 22:40 declares grammar-driven "ZERO
  artifacts / un-attempted" while PROTO-2-P2 (20:05, 2.5h earlier) is the first artifact at 43/45, and
  issues Briefs 1 & 2 already executed by PROTO-1-5D (22:51) / PROTO-2-P2-CLOSED (22:59). Third
  consecutive recurrence of the same defect (pass-2, pass-3-15:27, pass-3-22:40).
- **Vacuous convergence — PARTIAL:** 5A 42 (no run, parked — defensible), 5B 40 (retired —
  defensible); the vacuity is the STALE titular-floor number ("0% evidenced") contradicted by 56/56.
- **Elegant-reduction trap — HIT ×2:** 5A ("delete name-gate SHRINK" … then import ~787-LOC parser,
  net sign unknown); 5D ("direct-scan-default" … then wire `derive_recognizers` onto the FOLLOW fixed
  point — the selector, unbuilt).
- **Gate that cannot fail — PARTIAL:** PROTO-1-5D's gate is real but scoped to verbatim-port
  primitives, not the emitted parser (end-to-end unexercised); P2-CLOSED's 56/56 has 2 constructs
  (bare anchors, `\p{}`) "verified by reasoning, not oracle."
- **Prototype tested the sharpest uncertainty — MIXED:** grammar-driven YES (P2/P2-CLOSED hit the
  titular thread, synthesis under-consumed); 5D NO (tested the compiling substrate + the proven bug;
  left the selector wiring + wired fraction + residue cold-pair unbuilt); 5C YES (integrated); 5A no
  proto.
- **Consumer-less substrate — used correctly** (Fork-B; CSP-over-tier → table). The live Y.13 question
  is DFA-on-codegen-path, which the un-run wired fraction settles (5D gap 2).
- **SPEC_ONLY anchoring a perf claim — NOT triggered:** discipline held (cold-paired runs; PROVE-10
  band explicitly not transferred).
- **Legacy aliases / masked fallbacks — improved:** the ByteClass masked-wildcard is being *replaced*
  with a positive-class gate (negated bodies stay masked, disclosed); the two divergent `regex.bbnf`
  now have a quantified retirement plan (1/21/3). Both were open debts; the prototypes closed the
  disposition, the synthesis undersells them.
- **Titular-deliverable floor — HIT (must re-cut):** the "0 artifacts / un-attempted" language is
  FALSE; the correct floor is "PARITY-DEMONSTRATED-in-scratch (56/56 raw+bridge), host-side, LOCK-7A
  promotion + self-hosting-bootstrap gated."

**Bottom line for the agglomerator.** The single action that most changes this lock's honesty is
**re-cutting the grammar-driven titular thread against PROTO-2-P2 / PROTO-2-P2-CLOSED**: the
deliverable is not un-attempted — it is at full DFA-parity to the hand parser in scratch (56/56), with
the exact remaining gap being LOCK-7A promotion + the self-hosting bootstrap (not "0% evidenced").
Briefs 1 and 2 are spent — reissue the GENUINELY-open pass-4 uncertainties: 5D's `derive_recognizers`
wiring + the WIRED Q7 fraction (does the DFA leave the generator path), 5C's byte-rarity oracle + the
de-confounded Valmari control, and 5A's `/foo$/`+`LookMatcher` (the joint 5A/5D anchor consumer, still
unbuilt a third pass). Family numbers: 5A 42% (parked refactor, shrink-sign unknown), 5B 40%
(cleanest, conditional retire), 5C 55% (disposition-decided, integration unbuilt, acyclic dispatch
marginal), 5D 58% (substrate + bug earned, selector + decisive fraction open). The lock is a converged
CORE of its own grammar-driven charter — but only once the floor language is corrected from "0
artifacts" to "parity-demonstrated, LOCK-7A-gated."
