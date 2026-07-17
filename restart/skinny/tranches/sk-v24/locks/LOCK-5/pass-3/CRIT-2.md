# LOCK-5 — THE REGEX CONTINUUM — pass-3 · CRIT-2 (adversarial, CROSS-LOCK weight)

Critic 2 of 2. I weight the CROSS-LOCK view (Constitution ix): where LOCK-5 contradicts or double-homes
against the GN/PT codex and the other locks, and whether its proposals to LOCK-1/2/4/6/7/9 are earned or
laundered. XL-A/XL-C/XL-H treated as binding fact. All anchors below re-verified this pass at the
filesystem level; the dominant finding was confirmed by mtime + source + the probes' own run logs.

**This CRIT-2 reviews the RE-CUT FAMILY-SPECS (mtime 22:40), not the 15:27 draft CRIT-1 saw.** The
re-cut correctly discharged CRIT-1's dominant finding — §5C now integrates PROTO-2-5C, the centerpiece
is RUN-EARNED RETIRED, 5C rose to 55%, and the stale "unrun a third pass" language is gone. Credit that.
The re-cut ALSO adopted CRIT-1's 5A=42% and 5D's earned +run. My charter is what the re-cut STILL gets
wrong — and it repeats CRIT-1's exact structural failure on a MORE consequential prototype.

---

## THE DOMINANT FINDING — the re-cut is STALE against its own directory's grammar-driven prototypes; the lock's TITULAR floor is FALSE (UNVERIFIED GESTALT, on the namesake)

**The re-cut synthesis (22:40) fixed the 5C staleness CRIT-1 named and simultaneously left a WORSE one
uncorrected: it is stale against `PROTO-2-P2.md` (mtime 20:03 — TWO HOURS THIRTY-SEVEN MINUTES before
the re-cut) and `PROTO-2-P2-CLOSED.md` (22:59).** Both are grammar-driven — the lock's TITULAR
deliverable, Constitution i. The 20:03 probe was available for the entire re-cut window and was ignored.

The synthesis asserts, in FOUR load-bearing places, a claim its own directory refutes:

- **Line 70–71 (the lock HEADLINE):** "grammar-driven — remains at **ZERO artifacts after three
  passes** and is the one genuinely **un-attempted** formulation." — FALSE. PROTO-2-P2 (20:03) is a
  RAN_MEASURED grammar-driven artifact.
- **Line 89–97 (Precondition P2):** "grammar-driven is SPEC-ONLY (re-confirmed; **ZERO grammar-driven
  artifacts after THREE passes**) … `parse_generated(&str) -> bool` still **discards its AST**
  (source-verified this pass)." — FALSE, and the source-verification hit the WRONG symbol. P2's
  FINDING 0 (verified from source this pass) establishes TWO `parse_generated`: the `generated.rs:2288
  -> bool` **diagnostic shim** the synthesis cites, and `regex::parse_generated` (`mod.rs:34`)
  `-> Result<Hir, ParseError>` — **the real grammar-driven path** (generated parser → typed
  `RegexParserEnum` AST → `host::lower` fold → bespoke `Hir`). The discard is **already broken at the
  outer layer**; "break-the-discard" is not a change to author. The synthesis "source-verified" the
  shim and missed the Hir path its own probe documented 2h37m earlier.
- **Line 32 ("Four prototypes ran"):** an UNDERCOUNT. By 22:40 the directory held ≥5 RAN probes
  (proto-1-overapprox, PROTO-2 fork-census, PROTO-2-5C, PROTO-2-P2; and the substrate-building
  PROTO-1-5D lands 22:51). Counting four while two grammar-driven runs sit un-agglomerated is the
  tell of incomplete agglomeration (Constitution vii).
- **Brief 2 (lines 447–468), re-issued as one of "the two sharpest uncertainties for pass-4":** it was
  EXECUTED (PROTO-2-P2, measured-feasible 43/45) and then CLOSED (PROTO-2-P2-CLOSED, 22:59): **RAW
  56/56 and BRIDGE 56/56 byte-DFA-identical to the hand parser, 0 rejects / 0 divergences, strict
  full-parity gate GREEN**, de-dup surface fully quantified (**1 promote / 21 delete / 3 relocate ⇒
  retire `grammar/misc/regex.bbnf`**). Offering an already-CLOSED probe as a pass-4 obligation is the
  precise error CRIT-1 caught for 5C's Brief 2 — the re-cut moved the error from 5C to the titular
  family instead of curing it.

**Why this is WORSE than CRIT-1's finding, and why it is a CROSS-LOCK defect (my charter).** The
synthesis does not merely mis-state a family number — it hands GESTALT-P an explicit, FALSE floor:
line 95 instructs the gestalt to read `SPEC-ONLY(grammar-driven)` as **"un-attempted three passes," NOT
"converged pending LOCK-7."** The directory proves the opposite. The true floor to convey to GESTALT-P /
LOCK-7A is **PARITY-DEMONSTRATED**: the grammar-driven path is wired end-to-end, DFA-byte-identical to
the hand parser on 56/56 (raw AND through the XL-C `simplify_hir` bridge), with **one open ARCHITECTURAL
choice** (host-fold re-sync `host.rs +50/−6` vs grammar-carries-typed-captures + regen — "either closes
it," GESTALT-P's call) and the de-dup surface CLOSED. That is a converged-pending-LOCK-7A disposition.
Directing the gestalt to treat it as un-attempted would cause GESTALT-P to re-budget a discharged probe
and to under-state Constitution-i compliance for the whole parse-that weave. This is unverified-gestalt
propagating UP the DAG — the most damaging place it can occur.

**Second, minor, same signature.** PROTO-1-5D (22:51) BUILT the `ByteClass` substrate
(`SimdMode::ByteClass`/`Recognizer::ByteClass`/`BackendExpr::ByteClass` + `match_byteclass_guarded`) and
**fresh-compiled it green in-tree** (`cargo build -p ir -p passes -p codegen`, EXIT 0). The 22:40
synthesis still calls that substrate "**DESIGNED, not built**" (line 324) / "**UNBUILT**" (lines 308,
324). Eleven minutes' staleness — forgivable in isolation, but it is the SAME freeze-before-directory
pattern, and it under-states 5D's true build state (substrate landed; only the `derive_recognizers`
FOLLOW-gate rewiring is unwired). CROSS-LOCK double-home #4's "the producer is UNBUILT (masked wildcard)"
is now half-false: the producer NODES exist and compile; only their SELECTION gate is unwired.

**Verdict on the finding.** It does NOT impeach any probe — PROTO-2-P2-CLOSED's 56/56 rests on a strict
byte-canonical DFA oracle, and proto-1-overapprox's partition is machine-asserted (below). It impeaches
the synthesis's claim to be a converged agglomeration of its OWN directory. **§ headline, Precondition
P2, cross-lock #5, and Brief 2 must ALL be re-cut against PROTO-2-P2 / PROTO-2-P2-CLOSED before this lock
is returned to GESTALT-P.**

---

## CROSS-LOCK LEDGER — where LOCK-5 contradicts / double-homes / launders (my primary charter)

**XL5-1 — the 5C→LOCK-2 "BINDING INPUT" over-reaches on ONE of its two halves (a laundered grep).**
FAMILY-SPECS line 393 routes to LOCK-2 as "a **BINDING** constraint (not a proposal)": (a) automata-min
is NOT an e-graph island, AND (b) tier-selection is NOT a CSP. **Half (a) is run-earned binding** —
PROTO-2-5C measured O(n²·k) on the acyclic target class + `incumbent ≈ null`; that legitimately refutes
LOCK-2's implicit automata-island candidate. **Half (b) is NOT run-earned** — it rests on research-C's
zero-witness grep (no shared-budget coupling in *today's* `crates/ir/`) plus reasoning; the synthetic
budget-cap crossover that research-C promised "to exhibit the flip" NEVER ran (the synthesis itself
concedes this, lines 251–252: "ABSENT today, CONSTRUCTIBLE … gated on a budget-binding corpus that does
not yet exist"; CRIT-1 flag #3 concurs). LOCK-2's **2C charter (CSP-CENTRAL) explicitly names "prefilter
tier" as a per-site CSP variable** — so half (b) is a REAL cross-lock contradiction, but its refutation
is grep-for-today, not run-for-always. Routing both halves as "binding, not a proposal" launders a grep
into a run and would let GESTALT retire a live LOCK-2 candidate on absent evidence. **Correct routing:
half (a) BINDING (run-backed); half (b) STRONG PROPOSAL — GESTALT/LOCK-2 must construct the
budget-binding witness before retiring 2C's tier variable.** This is CRIT-2's sharpest cross-lock ding.

**XL5-2 — Axis-1 re-cut (double-home #1) is a consumer-less adjudication.** The two-types+bridge verdict
(XL-C) re-cuts LOCK-4-4D, LOCK-9-9C, LOCK-3-3C — all three were written against ONE-HIR — yet the
synthesis says the re-cut is "**still nobody's charter**" (line 392). An adjudication that re-cuts three
downstream families but is owned by no family/charter is a Y.13-adjacent defect at the cross-lock layer:
a verdict with no consumer. It must become an explicit named GESTALT-P/Ω work item ("re-cut 4D/9C/3C
against two-types+bridge"), not a floating note. Named, but under-actioned.

**XL5-3 — cross-lock #5 (5A ⊗ LOCK-7) is stale in the SAME way as the headline.** Line 403–404 tells
LOCK-7: "Two divergent `regex.bbnf` (one discards its AST) … promote + de-dup + break-the-discard …
zero artifacts three passes — the substance of Brief 2." PROTO-2-P2-CLOSED settles the de-dup surface
NUMERICALLY (25 misc rules = 1 promote `octal` / 21 delete [17 dominated + 4 non-regular out-of-scope] /
3 relocate [flags/end/regex wrapper] ⇒ retire `grammar/misc/regex.bbnf`) AND demonstrates parity. The
cross-lock note to LOCK-7 must convey a QUANTIFIED de-dup + a PARITY-DEMONSTRATED artifact + the ONE open
architectural choice — not "zero artifacts / discards its AST."

**XL5-4 — double-home #7 (serde-ownership ⊗ LOCK-6) is real and recurring — correctly named.** PROTO-2-P2,
PROTO-2-P2-CLOSED, and PROTO-2-5C all had to run in a READ-ONLY rsync scratch with repointed patch
prefixes; the `CharClass: Serialize` ownership question (who lands it, on the parse-that READ-ONLY / LOCK-6
boundary) persists two passes. PROTO-2-P2-CLOSED confirms parse-that stayed byte-identical
(`diff … IDENTICAL` pre-edit; the sole change is scratch `host.rs +50/−6`). Legitimate GESTALT-Ω gap;
the synthesis carries it correctly (cross-lock #7). No ding — logged so GESTALT does not lose it.

**XL5-5 — double-home #2 (5C ∥ LOCK-2) discharge is sound; #3 (LOCK-1 re-open) is sound.** The
0-divergence corpus + no-coupling grep jointly discharge the CSP-witness search for today's codegen
(consistent with XL5-1's caveat that "today" ≠ "always"). The LOCK-1 sub-leaf-capture re-open trigger
correctly binds 5B-un-retire AND 5D's `Project` byte-identity oracle to the same unratified `->` fact,
routed with PROTO-2 evidence. Both cross-lock claims hold.

---

## FAMILY 5A — ONE-HIR — **CRIT-2 convergence: 42%** (concur with re-cut / CRIT-1)

The narrowed survivor-residual is honestly bounded and the "classify RELOCATES not shrinks" anti-overclaim
is genuine. Concur with 42%. Enumerated open gaps:

1. **The net-shrink sign remains UNKNOWN — the elegant-reduction trap stands.** Delete ~200-LOC name-gate
   + ~322-LOC shadow crate "and then the hard part": skinny must import a REAL parser (parse-that
   `hir/parser/` ~787 LOC). G-5A.shrink is UNBUILT; the family's sole distinguishing win ("SHRINK") could
   be net GROWTH. PROTO-2-P2-CLOSED's de-dup count de-risks the LOCK-7 grammar seam but NOT the
   skinny-side import cost — those are different edges.
2. **Every gate UNBUILT, three passes; zero 5A run.** G-5A.look (`/foo$/` + `LookMatcher`), G-5A.shrink
   (ΔLOC), G-5A.leak (`[a-c]≡[a-z]` identical `Span` graded against LOCK-1's real HM, not skinny's stub
   `BuiltinTy`), G-5A.fork-collapse. A refactor-shaped residual with four unbuilt gates.
3. **The `/foo$/` `Look`-consumer defect is a LIVE correctness claim, still unexercised.** `build_look`
   (`nfa.rs:254`) collapses six `Look` variants to one zero-width state; anchoring lives OUTSIDE the NFA
   in `detect_anchored`. The failing-then-passing test (`/foo$/` must reject `"foobar"`) is
   buildable-and-unbuilt a THIRD pass. Until it runs, "the bridge fixes a live defect" is SPEC-ONLY.
4. Under XL-C the distinguishing mechanism is dead; the residual is a subset of the survivor's work homed
   elsewhere → disposition-certainty, not convergence. NOT BLOCKED (no missing primitive). 42%.

## FAMILY 5B — LAWFUL-FUNCTOR — **CRIT-2 convergence: 40%** (concur; cleanest disposition)

Sound conditional retire, legitimately strengthened by PROTO-2 (fork-independence measured 0/0/0). The
win-laundering guard is correctly applied (the −322 shadow-crate refactor credited to 5A-residual, NOT
the functor; the reverse `Hir→String` unparse homed in the bridge round-trip oracle, not 5B). Gaps:

1. **The sole re-open condition is genuinely OPEN, not closed** — the retire rests on `->` staying a
   whole-leaf wrapper (`Map`/`Project`) that never binds a sub-leaf capture, a LOCK-1 fact LOCK-1 has
   NOT ratified. 40% correctly refuses to read a conditional retire as a shipping number.
2. **Consumer-assignment residual:** the total `Hir→String` unparse (fixing `canonical_pattern`'s
   partiality) is currently nobody's GATE — home it in the survivor's bridge-hardening, explicitly.
   (Note: PROTO-2-P2-CLOSED's `escape_body`/brace-trim re-sync is precisely the reverse-fold direction;
   the bridge round-trip oracle should absorb it.) Minor. 40%.

## FAMILY 5C — AUTOMATA-AS-EXTRACTION — **CRIT-2 convergence: 52%** (re-cut 55%; markdown −3 for the laundered LOCK-2 half + unbuilt integration)

The re-cut correctly integrated PROTO-2-5C: centerpiece RUN-EARNED RETIRED, 2B-as-minimizer cleanly
BLOCKED (Kozen — least/greatest-fixpoint mismatch; `(ab)*` bisimilar loop states never bottom-up
congruent; the lock's sharpest impossibility). The disposition is well-earned and run-backed. I mark
DOWN from 55% for two reasons the re-cut under-weights:

1. **The LOCK-2 "binding input" over-reaches (XL5-1).** Half of the flagship cross-lock claim (tier NOT a
   CSP) is grep-for-today, not run-for-always; the flipping witness never ran. A 55% that includes a
   laundered binding-input over-states the cross-lock soundness.
2. **The byte-rarity oracle is a NAMED MISSING INPUT for 2C-as-table** (no in-tree frequency oracle;
   without regex-automata's ASCII-rank table, accel-byte selection "is guesswork"). The mechanism is
   settled; one of its inputs is absent → accel-byte DECISIONS are underdetermined. This — not the
   discharged Brief 2 — is the real pass-4 5C uncertainty.
3. **`is_acyclic`-dispatch net-positivity is marginal** — Σ(null−revuz)=7208 ns vs 6786 ns dispatch at
   regex scale (n≤20); the acyclic fast-path's worth over just-deleting-`:358` is unsettled, and its
   headline still inherits the Valmari allocation-confound (a de-confounded CSR control is UNBUILT).
4. **Survivor integration ENTIRELY UNBUILT** — minimize-swap (`dfa.rs:89-91`), Valmari/Revuz partial-DFA
   `DEAD` handling, delete-`:358`, tier-table home (`info/mod.rs:344`). 55% reads as convergence-of-
   disposition; the shipping distance is larger. FAMILY RETIRED (centerpiece); 2B BLOCKED. **52%.**

## FAMILY 5D — GRAMMAR-FRAGMENT — **CRIT-2 convergence: 55%** (concur with re-cut; the +run is earned, machine-asserted)

The re-cut's jump to 55% is EARNED and answers CRIT-1's three strongest 5D objections. I verified
proto-1-overapprox's run log directly: the partition is **machine-asserted, not prose** —
`sound→unsound flips: ["sheet_prefix"]`, `unsound→sound flips: []`, `[genuine-give-back trio still
NON-disjoint under over-approx]: ["word","string","identifier"] (3/3)`, `test result: ok. 1 passed`; the
single `sheet_prefix` flip is a `!`-delimiter conservative false-positive in BOTH branches (0 genuine
sound→unsound flips); cold-pair `256/256` byte-identical on both leaves, direct scan 3.25× (rare) /
2.81–3.00× (dense) vs `Dfa::find`. CRIT-1 gap-1 (unsound predicate), gap-2 (scan-beats-DFA untested),
gap-3 (prose hand-count) are ALL closed by the sound over-approx run. Genuine advance. Remaining gaps:

1. **Corpus breadth** — 16 leaves, approximate sheets FOLLOW-wiring (39-rule subset, host-heavy rules
   approximate). The architecture-gating fraction rides a small corpus; the WIRED-fraction re-run on the
   real JSON/Sheets/CSS corpus (does the give-back residue stay non-empty in production?) is UNBUILT.
2. **`derive_recognizers` FOLLOW-gate rewiring is the one unwired seam** — PROTO-1-5D built the substrate
   (compiles green) but did NOT rewire `derive_recognizers` (`passes/src/lib.rs:338`) onto the FOLLOW
   fixed point under the cap. The closure exists (`proto-1-overapprox/follow_sound_fraction.rs`); wiring
   is the pass-4 delivery (Brief 1 — the ONE brief that is genuinely open and correctly targeted).
3. **GRAFT-3 coherence obligation (cross-lock, non-optional)** — 5D's generator-path direct-scan-vs-DFA
   FOLLOW-guard and 5C-2C's engine-tier prefilter table decide "class-scan vs DFA" for the SAME leaves
   from two homes; they MUST be ONE dispatch surface (the double-emit trap). Un-built; a real pass-4
   integration risk that GRAFT-3 names but no family yet owns.
4. **Capture-leaf `->` (LOCK-1-gated)** — regex-internal `->` forbidden at the byte plane; judge against
   RE2C TDFA(1). Same unratified LOCK-1 fact as 5B's re-open. NOT BLOCKED. 55%.

## FAMILY P2 — GRAMMAR-DRIVEN (the TITULAR deliverable) — **CRIT-2 convergence: 60%; the floor is PARITY-DEMONSTRATED, NOT "un-attempted"**

Not one of the four charter families, but the lock's namesake (Constitution i) and the subject of my
dominant finding. It must carry an explicit convergence line so GESTALT-P does not receive the false
"0 artifacts / un-attempted" floor. State of evidence (verified this pass):

- The grammar-driven path is **wired end-to-end today** (`regex::parse_generated`, `mod.rs:34
  -> Result<Hir>`; discard already broken; 525-LOC `host.rs` fold). PROTO-2-P2 → measured-feasible
  (43/45); PROTO-2-P2-CLOSED → **RAW 56/56 & BRIDGE 56/56 byte-DFA-identical to the hand parser, 0
  reject / 0 diverge, strict full-parity gate GREEN**, closed by a 5-edit host-only re-sync (`+50/−6`,
  no grammar change, no regen).
- De-dup surface CLOSED: **1 promote / 21 delete / 3 relocate ⇒ retire `grammar/misc/regex.bbnf`**.

Enumerated OPEN gaps (why 60%, not higher):
1. **The architectural CHOICE is open, GESTALT-P's call** — host-fold re-sync (verified, smaller
   surface) vs grammar-carries-typed-captures + regen (cleaner, unbuilt). "Either closes it."
2. **Two constructs closed by construction, not by the DFA oracle** — bare `^`/`$` anchors and `\p{}`
   are absent from the 56-corpus; their arms are verified by reasoning, not by a divergence flipping to
   pass. Bare dot and `\x` WERE in-corpus and ARE oracle-verified.
3. `lower_class_atom_node_byte` (in-class `\x` range endpoints) left reading `bytes[0]`; no corpus range
   exercises it (one-line GESTALT-P follow-on).
4. **SPEC-ONLY → PARITY-DEMONSTRATED, still gated LOCK-7A** for promotion to production parser (the
   host-fold-vs-grammar-captures choice is the LOCK-7A seam). This is a converged-pending-gate
   disposition — the OPPOSITE of the synthesis's "un-attempted." 60%.

---

## CHECKLIST SUMMARY (failure modes)

- **Unverified / stale gestalt — HIT (dominant):** re-cut (22:40) ignores PROTO-2-P2 (20:03, 2h37m
  prior); headline / Precondition-P2 / cross-lock-#5 / Brief-2 all refuted by the same directory. The
  re-cut CURED CRIT-1's 5C staleness and REPRODUCED the identical error on the titular family.
- **Titular-deliverable floor — INVERTED (was CRIT-1's HIT; now FALSE):** grammar-driven is NOT 0
  artifacts / un-attempted — it is PARITY-DEMONSTRATED (56/56 raw+bridge), de-dup CLOSED, one
  architectural choice open. GESTALT-P must receive PARITY-DEMONSTRATED, not "un-attempted."
- **Vacuous convergence — NOT a hit this re-cut:** numbers moved with evidence (5C 45→55 on PROTO-2-5C,
  5D +run). Improvement over CRIT-1's finding. But P2's number was never surfaced at all (0-of-a-family).
- **Elegant-reduction trap — HIT (5A):** "delete name-gate SHRINK" … then import ~787-LOC parser, net
  sign unknown. (5D's trap is now DISCHARGED — the sound over-approx first(C) ran and machine-asserted.)
- **Gate that cannot fail — NOT a hit this pass:** proto-1-overapprox machine-asserts the partition
  (flips/genuine-trio/256-256), PROTO-2-P2-CLOSED uses a strict byte-canonical DFA gate. CRIT-1's
  partial-hit is CURED.
- **Laundered binding-input — HIT (cross-lock XL5-1):** 5C→LOCK-2 "binding, not a proposal" over-reaches
  on the tier-NOT-a-CSP half (grep-for-today, flipping witness never ran). Half (a) binding, half (b)
  proposal.
- **Consumer-less adjudication — HIT (cross-lock XL5-2):** the Axis-1 re-cut of 4D/9C/3C is "nobody's
  charter"; a verdict re-cutting three downstream families with no owner.
- **Consumer-less substrate — used correctly by the families;** one residual (2C byte-rarity input
  absent). 5D's ByteClass producer is now BUILT (PROTO-1-5D), not masked — cross-lock #4 half-cured.
- **SPEC_ONLY anchoring a perf claim — NOT triggered:** discipline held (PROTO-1/PROTO-2 structural or
  work-counts; PROTO-2-P2* explicitly no-perf; PROVE-10 band not transferred).
- **Legacy aliases / spec-cites-itself — no violation:** XL-C self-citation is orchestrator-ratified;
  the two `regex.bbnf` are honest de-dup debt now QUANTIFIED (retire misc, carry `octal`).

**Bottom line for the agglomerator.** The single action that most changes this lock's honesty is
**re-cutting the headline + Precondition-P2 + cross-lock-#5 + Brief-2 against PROTO-2-P2 / PROTO-2-P2-CLOSED**:
the grammar-driven titular deliverable is PARITY-DEMONSTRATED (56/56 raw+bridge), de-dup CLOSED, Brief 2
SPENT — GESTALT-P must NOT be told "un-attempted." Second, **split the 5C→LOCK-2 binding input**: automata-
min-not-an-island is run-binding; tier-not-a-CSP is a proposal until the budget-cap witness runs. Family
numbers: 5A 42 (parked refactor, shrink-sign unknown), 5B 40 (clean conditional retire), 5C 52 (disposition
earned, one cross-lock half laundered + integration unbuilt), 5D 55 (run-earned, machine-asserted, wiring
+ corpus + GRAFT-3 open), P2 60 (PARITY-DEMONSTRATED, one architectural choice + LOCK-7A gate open). The
lock is a rigorously converged CORE of its own charter — but only once its titular floor is stated as
PARITY-DEMONSTRATED, not "0 artifacts / un-attempted."
