# LOCK-5 — THE REGEX CONTINUUM — pass-3 · CRIT-1 (adversarial, WITHIN-LOCK weight)

Critic 1 of 2. I weight internal soundness of each family spec and whether the prototypes tested the
*sharpest* uncertainty. Settled ground (XL-C two-types + bridge; parse-that one-regex-home; base-pin
XL-H) is treated as binding fact, not re-litigated. All source anchors below were re-verified this
pass; the dominant finding was confirmed at the filesystem level.

---

## THE DOMINANT FINDING — the synthesis is STALE against its own lock's latest, sound, decisive prototype (UNVERIFIED GESTALT)

**FAMILY-SPECS.md was frozen at 15:27; PROTO-2-5C.md landed at 16:07 — 40 minutes later — and the
synthesis never integrated it.** Confirmed by mtime and by grep: the only `RETIRES` token in
FAMILY-SPECS (line 411) sits *inside Brief 2's own hypothetical pass/fail disjunction*, not as a
citation of any result. The synthesis therefore asserts, in five places, a claim its own directory
refutes:

- Line 22 (headline): "its automata centerpiece is **unrun a THIRD pass**."
- Line 51: "**What did NOT run (a third pass in a row):** 5C's reference-Hopcroft control."
- Line 55: "5C's centerpiece is the lock's one genuinely-**unrun** decisive number and is Brief 2."
- Line 176 / 219 / 233–234: 5C headline "centerpiece unrun a THIRD pass"; "(G-5C.1) reference control
  … UNRUN"; "Corpus still 26 … must grow to ≥50 tagged."
- Lines 55, 362, 390–412: **Brief 2 is issued as the pass-4 obligation.**

**PROTO-2-5C IS Brief 2, executed.** It base-pins parse-that HEAD `ef10d5b`, grows `COMPILE_PATTERNS`
26→**56** (12 acyclic / 44 cyclic), implements the exact four lanes the brief demands (L1 delete-`:358`
null, L2 Valmari partial-DFA, L3 Revuz acyclic, L4 `DfaSig` congruence), runs a BFS-canonical
correctness oracle FIRST (**0/56 divergences**), and measures passes-to-fixpoint. It returns
**RAN_MEASURED — FAIL: the congruence centerpiece RETIRES** (`[01]{8}` n=9 → 8 passes ⇒ O(n²·k), not
O(n·k·log n); `incumbent ≈ null` <1% ⇒ the `:358` strawman never justified the centerpiece).

Consequences the synthesis owes and does not pay:

1. **Three factual claims in §5C are false against the same directory:** centerpiece is RUN not unrun;
   G-5C.1 reference control is RUN (Valmari + Revuz + congruence, 0-divergence oracle) not UNRUN;
   corpus is 56 not 26. These are not stale-by-a-pass — they are stale by 40 minutes, same author-cycle.
2. **5C's convergence number is set for a refuted reason.** "Held at 45% … the centerpiece staying
   unrun a third pass is the ceiling" (line 233). The stated ceiling no longer exists. The number is
   simultaneously *too low* (a run-earned retirement of the distinguishing mechanism is a MORE certain
   disposition than an unrun-parked one — cf. 5B's RETIRED being a legitimate disposition) and set on a
   defunct rationale. This is textbook unverified-gestalt (Constitution vii; failure-mode "unverified
   gestalt").
3. **Brief 2 is a wasted re-issue.** Offering an already-executed probe as one of "the two sharpest
   uncertainties for pass-4" is the strongest possible evidence the synthesis did not agglomerate its
   own lock's evidence. The genuinely-open pass-4 uncertainty in 5C is NOT the control (decided) but the
   byte-rarity oracle for 2C-as-table's accel-byte quality (still absent in-tree).

This finding does not impeach PROTO-2-5C, whose FAIL rests on the allocation-*independent* passes
measure (scale-free, not the Valmari-confounded timing) — it is sound. It impeaches the synthesis's
claim to be a converged agglomeration. **The FAMILY-SPECS must be re-cut against PROTO-2-5C before any
convergence talk on 5C.**

---

## SECONDARY LOCK-LEVEL FINDINGS

**F-2 — vacuous-convergence signature across three of four families.** 5A 45→45 (its own text: "no 5A
prototype ran pass-3"), 5B 40→40, 5C 45→45 (proto ran, ignored). The one family that moved (5D 35→45)
is the only one with a fresh run. A static number is defensible for a RETIRED disposition (5B) and even
for a parked refactor (5A) — but 5C holding static *while its decisive prototype ran and inverted its
rationale* is a provable mis-set, and that contaminates the "held at 45%" language used for the others.
Convergence must be earned per the changed-flag discipline (MEMORY changed-flag-convergence); three
identical hold-numbers with new evidence in the directory read as placeholders.

**F-3 — the lock's TITULAR deliverable has zero prototype evidence after three passes.** "THE REGEX
CONTINUUM — the grammar-driven regex engine." P2 concedes: `parse_generated` still discards its AST
(`generated.rs:2288 → bool`), two `regex.bbnf` still diverge, "ZERO grammar-driven artifacts after
THREE passes." Every family verdict rides `SPEC-ONLY(grammar-driven)`. What is actually converging is
the **two-types-plus-bridge survivor + automata internals** — neither of which is grammar-first. This is
correctly gated per-family (LOCK-7/7A), so it is not a per-family violation; but the *lock* cannot be
returned as a converged core of its own charter while its defining property (Constitution i) is 0%
evidenced. GESTALT-P must receive this as an explicit named floor, not buried under family %s.

**F-4 — SPEC_ONLY-anchors-a-perf-claim: NOT triggered (discipline held).** Checked. PROTO-1 is a
structural fraction (no perf claim). PROTO-2 is work-counts. PROTO-2-5C explicitly disclaims large-n
throughput and rests FAIL on the passes measure. research-A line 98 explicitly refuses to transfer the
8.8–29.5× PROVE-10 prefilter band to a construction-parity claim. No perf claim rests on SPEC_ONLY.
Credit where due.

---

## FAMILY 5A — ONE-HIR — **CRIT-1 convergence: 42%** (synthesis 45%; markdown for the parser-import trap + three passes of zero prototype)

Internally the narrowed survivor-residual is sound and honestly bounded (the "classify RELOCATES not
shrinks" correction is a genuine anti-overclaim). But the family is *parked*, not converging, and its
one net-positive claim is an elegant-reduction trap.

**Enumerated open gaps.**
1. **The "net shrink" is the elegant-reduction trap.** "Delete the ~200-LOC name-gate block + 322-LOC
   shadow crate (net negative)" — *and then the hard part*: deleting the heuristic forces skinny to
   import a REAL parser (parse-that `hir/parser/` ~787 LOC). G-5A.parser-cost is UNBUILT; the sign of
   the net LOC delta is therefore **unknown**, and the family's sole distinguishing win ("SHRINK") could
   be a net GROWTH. The synthesis names this but still carries "genuine skinny-side SHRINK" in the
   architecture line (spec §5A) without the measured delta.
2. **Every gate UNBUILT, three passes running:** G-5A.2 (`/foo$/` Look test), G-5A.shrink (ΔLOC),
   G-5A.leak (`[a-c]≡[a-z]` identical `Span`). No 5A prototype ran pass-3 (conceded). A refactor-shaped
   residual with four unbuilt gates and zero runs is not a 45%-converged core.
3. **The `/foo$/` Look-consumer defect is asserted but unexercised.** The claim that re-pointing
   `analyze` through `RegexInfo` makes `/foo$/` reject `"foobar"` is a *correctness improvement in the
   substrate* — but it is buildable-and-unbuilt. Until the failing-then-passing test runs, "the bridge
   fixes a live correctness defect" is SPEC-ONLY.
4. **Under XL-C the distinguishing mechanism is dead** (the maximal single-home claim). What survives is
   "engine-tier hardening, largely already-existing" (spec §5A) — i.e. 5A's residual is a subset of the
   survivor's work, homed elsewhere. A family whose non-dead content is credited to the survivor is at
   disposition-certainty, not convergence; 45% overstates it.

**Not blocked.** No missing primitive. But 42%, not 45%: the parser-import cost surfacing this pass is a
*downgrade* to the shrink claim, not an upgrade.

## FAMILY 5B — LAWFUL-FUNCTOR — **CRIT-1 convergence: 40%** (concur with synthesis; least-problematic)

The retire is sound and PROTO-2 legitimately strengthened it (fork-independence now measured 0/0/0, not
estimated). The win-laundering guard is correctly applied: the −322 shadow-crate refactor is credited to
5A's residual / the banked refactor, NOT to the functor. The re-open trigger (LOCK-1 sub-leaf capture)
is correctly routed cross-lock.

**Enumerated open gaps.**
1. **The sole re-open condition is genuinely open, not closed.** The retire *rests on* `->` staying a
   whole-leaf wrapper (`Project`/`Map`) that never binds sub-leaf captures — a LOCK-1 fact LOCK-1 has
   NOT ratified. This is disposition-certainty of a *conditional* retire; 40% correctly refuses to read
   it as a shipping number. No markdown, no markup.
2. **Minor:** research-B's "total reverse functor" (`Hir→String` structural unparse) is correctly
   demoted to serving the bridge's round-trip oracle, not an adjunction — but the synthesis should note
   this makes the unparse a **5A/bridge residual obligation**, currently unhomed (nobody's gate carries
   "implement the total `Hir→String`"). Small consumer-assignment gap.

## FAMILY 5C — AUTOMATA-AS-EXTRACTION — **CRIT-1 convergence: 55%** (synthesis 45% is set for a REFUTED reason; corrected number is HIGHER, see dominant finding)

Once PROTO-2-5C is admitted, 5C is *more* converged than the synthesis claims, because the disposition
is now decided: the e-graph-over-automata centerpiece RETIRES (run-earned), and the surviving deliverable
is concrete — **2C-as-table (settled) + mandatory delete-`:358` + a single reverse-topological Revuz
pass as a no-e-graph acyclic optimization**. 2B-as-minimizer is cleanly BLOCKED (Kozen; the lock's
sharpest impossibility). The internal soundness of PROTO-2-5C's FAIL is good: it rests on the
allocation-independent passes measure, not the Valmari-confounded timing (which the report itself
brackets as non-load-bearing).

**Enumerated open gaps (the ACTUAL pass-4 residual, post-PROTO-2-5C).**
1. **The byte-rarity oracle is a named MISSING INPUT for 2C-as-table.** research-C: no frequency oracle
   exists in-tree; without regex-automata's static ASCII-rank table, accel-byte selection "is
   guesswork." The table is settled as the *mechanism*, but one of its inputs is absent — the table's
   accel-byte *decisions* are underdetermined until it lands. The synthesis mentions this (line 210) but
   does not carry it as a gate. It is the real pass-4 uncertainty, not Brief 2.
2. **The `is_acyclic`-dispatch net-positivity is the weakest measured conjunct.** PROTO-2-5C: against the
   *fair* `null` control the acyclic saving is Σ(null−revuz)=7208 ns vs 6786 ns dispatch — "marginally"
   net-positive, "the weakest of the three even at its best," and inheriting the Valmari confound in its
   headline figure. So even the surviving Revuz fast-path's *worth* over just-deleting-`:358` is
   marginal at regex scale (n≤20). Whether Revuz's 1.42× single-pass edge justifies the acyclicity DFS
   on the 79% cyclic remainder is unsettled.
3. **CSP-over-tier escalation witness never exhibited (correctly — that absence IS the finding).** Zero
   global-budget coupling grep-confirmed → single-home the table in 5C; escalate to LOCK-2 only on an
   exhibited shared-LUT/Teddy-bucket cap. Sound; but the synthetic-cap crossover research-C promised
   ("the prototype constructs the synthetic cap to exhibit the crossover") did NOT run in PROTO-2-5C —
   so "the table wins, CSP is consumer-less" is proven for *today's* codegen, not shown robust against
   the one condition that would flip it.
4. **SPEC-ONLY(grammar-driven) rides here too**, and the `:358` fix, though "MANDATORY regardless" and
   free (0 divergences), is un-landed.

**Prototype-tested-the-sharpest-uncertainty: YES for 5C** — PROTO-2-5C hit exactly the decisive number.
The failure is entirely in the synthesis not consuming it. Convergence corrected UP to 55%, with the
explicit flag that the *document's* 45% is stale-and-wrong-reasoned.

## FAMILY 5D — GRAMMAR-FRAGMENT — **CRIT-1 convergence: 43%** (synthesis 45%; the +10 is earned by a real run, but the run tested the LEAST-uncertain part)

PROTO-1 is a genuine advance — a real Changed-flag FOLLOW fixed point that converges and is sound on the
canonical `word "and" word` trap, defeating the confirmed-unsound `one_pass.rs` Repetition arm
(`Hir::Repetition(rep) => check_one_pass_eligible(&rep.sub)` — verified at source, recurses without a
FOLLOW check). But three within-lock soundness problems keep this below the synthesis number.

**Enumerated open gaps.**
1. **The architecture verdict rests on an ADMITTEDLY-UNSOUND predicate.** PROTO-1's fraction ran on
   `analyze().first`, which UNDER-approximates alternations (`sheet_prefix` → `Exact{'\''}`, drops the
   letter branch) and returns UNKNOWN for `sheets::number`. The synthesis concedes the "3 SOUND"
   verdicts are "not yet reliably sound." So "direct-scan-default, DFA-narrow-fallback" — the verdict
   that gates LOCK-9-9C and LOCK-4-4D — is directionally set on a predicate that is *currently unsound*.
   The sound over-approx version (Brief 1a) is UNBUILT. If over-approx flips any leaf sound→unsound the
   verdict weakens. This is the elegant-reduction trap: "direct-scan-default … *and then* draw first(C)
   from the DFA start-state class set (the hard, unbuilt part)."
2. **PROTO-1 tested the DEMONSTRABLE part, not the sharpest uncertainty.** What ran: a FOLLOW fixed
   point exists and catches one injected counterexample — a build-and-it-holds property. What did NOT
   run: (a) the sound over-approx fraction (whether the partition survives), (b) whether the direct scan
   actually ties-or-beats `Dfa::find` (whether the DFA truly leaves the generator path). The two
   sharpest 5D uncertainties are both UNBUILT (Brief 1b/1c). The prototype advanced the family without
   testing what the family's verdict actually hinges on.
3. **The load-bearing "3/16 genuine" is a PROSE hand-count, not a test output.** The test asserts only
   the single counterexample (`!cx_leaf.disjoint`, `overlap ∋ 'a'`) and `total>0`; it emits
   `disjoint/bounded/prone_sound` but NOT a machine "genuine=3" partition. The mapping of the 13
   non-disjoint leaves into {3 genuine, 3 conservative-FP, 6 bounded, 1 opaque} is researcher
   adjudication in the markdown table, unverified by assertion. The fraction that carries the verdict is
   ungated (gates-that-cannot-fail, partial): the counterexample IS a real gate, but the
   architecture-deciding number is reported, not checked. Brief 1's pass/fail ("still exactly {word,
   string, identifier}") would require machine-encoding that set — currently manual.
4. **The corpus is 16 leaves with APPROXIMATE sheets FOLLOW-wiring.** Sheets loaded "under the skinny
   parser subset (39 rules; … rule wiring for a few host-heavy constructs is approximate"). The
   architecture-gating fraction is derived from a 16-leaf corpus whose follow-sets are admittedly
   approximate for the non-trivial half. Brief 1 does NOT grow this corpus — it only swaps first(C) on
   the same 16 leaves — so the fraction's corpus-representativeness is never tested. Enumerated gap the
   synthesis does not list.
5. **The `ByteClass` substrate is confirmed UNBUILT (masked wildcard).** `derive_recognizers` falls to
   `_ => {}` for non-structural classes; no `Recognizer::ByteClass`, no `BackendExpr::ByteClass` (grep
   zero). This is honestly disclosed as unbuilt (not a masked fallback smuggled into the design), but it
   means the *entire tape-reaching path* of 5D — G-5D.tape, G-5D.capture — is speculative. Whether
   direct-scan even reaches the tape non-inert is untested.

**Not blocked; the +10 is real** (first decisive-ish number landed). But 43%: the verdict rides an
unsound predicate, the partition is hand-counted, the corpus is small-and-approximate, and the two gates
that would actually confirm "the DFA leaves the generator path" are unbuilt.

---

## INCOMPATIBILITY MAP + CROSS-LOCK — soundness check

- The Axis-1 CLOSED status and the 5A-residual∧5D COMPATIBLE finding are internally sound.
- **5C ∥ {5A,5D} orthogonality survives PROTO-2-5C** — the retirement of the centerpiece does not touch
  the orthogonality claim (GRAFT-1/2 hold engine-side regardless), so this section is not invalidated by
  the dominant finding. But CROSS-LOCK double-home #4 (5A/5D ⊗ 9C SUPERSESSION) correctly flags that the
  substrate LOCK-5 offers 9C "is itself UNBUILT (the `ByteClass` recognizer does not fire)." That is the
  honest statement; the synthesis should not let 9C's re-cut proceed on an undelivered substrate.
- **Double-home #3 (5C-2C ⊗ LOCK-2)** is now *more* settled than the synthesis knew: PROTO-2-5C's
  0-divergence corpus + no-coupling grep jointly discharge the witness search for today's codegen. Fine.
- The LOCK-1 re-open trigger (double-home #2) correctly binds 5B-un-retire AND 5D-capture-oracle to the
  same unratified `->` fact. Sound.

---

## CHECKLIST SUMMARY (failure modes)

- **Unverified / stale gestalt — HIT (dominant):** FAMILY-SPECS 15:27 predates PROTO-2-5C 16:07;
  §5C's run-status, gate-status, corpus-size, convergence rationale, and Brief 2 are all refuted by the
  same directory.
- **Vacuous convergence — HIT:** 5A/5B/5C held identical numbers; 5C's static 45% is provably mis-set
  (its decisive proto ran and inverted the rationale).
- **Elegant-reduction trap — HIT ×2:** 5A ("delete name-gate SHRINK" … then import ~787-LOC parser,
  net sign unknown); 5D ("direct-scan-default" … then build the sound over-approx first-set + ByteClass
  substrate + cold-pair, all unbuilt).
- **Gate that cannot fail — PARTIAL HIT:** PROTO-1's architecture-deciding fraction is ungated (only the
  one counterexample is asserted; the 3/16-genuine partition is prose).
- **Prototype tested the sharpest uncertainty — MIXED:** 5C YES (ignored); 5D NO (tested the
  demonstrable FOLLOW-fixed-point, left over-approx soundness + cold-pair unbuilt); 5A no proto.
- **Consumer-less substrate — used correctly** by the families (grammar-tier e-graph inert under
  projection → Fork-B; CSP-over-tier consumer-less → table). One residual: 2C-as-table's byte-rarity
  input is absent (underdetermined accel-byte decisions).
- **Spec-cites-itself circularity — NOT a violation:** XL-C self-citation is orchestrator-ratified
  protocol, conveyed as fact by design.
- **SPEC_ONLY anchoring a perf claim — NOT triggered:** discipline held (no perf claim rests on
  SPEC_ONLY; the PROVE-10 band is explicitly not transferred).
- **Legacy aliases / masked fallbacks — no design-level violation;** two divergent `regex.bbnf` persist
  as an honest de-dup debt (gated LOCK-7/7A), and the ByteClass masked wildcard is disclosed-as-unbuilt,
  not smuggled.
- **Titular-deliverable floor — HIT:** grammar-driven regex = 0 artifacts / 3 passes; the whole lock
  rides SPEC-ONLY on its own charter.

**Bottom line for the agglomerator.** The single action that most changes this lock's honesty is
**re-cutting §5C against PROTO-2-5C**: the centerpiece is run-earned RETIRED, 5C's number rises (~55%),
Brief 2 is spent, and the real pass-4 5C uncertainty is the byte-rarity oracle. 5D's +10 is earned but
its verdict rests on an unsound predicate and two unbuilt gates (43%). 5A is a parked refactor whose
shrink claim now has a surfaced cost that could invert its sign (42%). 5B's conditional retire is the
only clean disposition (40%). The lock is NOT a converged core of its own grammar-driven charter (0
artifacts, 3 passes) and must be returned to GESTALT-P with that floor stated explicitly.
