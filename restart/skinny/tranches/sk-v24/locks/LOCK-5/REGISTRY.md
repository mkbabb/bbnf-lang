# LOCK-5 — THE REGEX CONTINUUM — FAMILY REGISTRY

Grammar-driven regex engine: the regex HIR truly isomorphic to the bbnf HIR/IR, with
NFA→DFA→minimize→prefilter recast as solver problems where measured to win.

**Status legend.** ADVANCED = active line, earns prototype budget. BANKED = sound but
redundant-under-squeeze or absorbed into a sibling; held behind a named un-bank/retire gate; not
killed. BLOCKED = the family's *maximal claim* rests on a missing primitive as-hard-or-harder than
the original problem (the family may survive narrowed). RETIRED = the family's distinguishing
mechanism is removed from the portfolio (a re-homed survivor may persist and advance separately).

**Convergence scoring.** Conservative floor of the two within-lock critics, raised only with a
named reason a critic erred. **Two such reasons were found this pass, and they are symmetric — the
pass's central lesson.** (i) On **5C**, both critics correctly re-cut the synthesis's stale 45%
UP to 55%: the FAMILY-SPECS (15:27) was frozen 40 min before PROTO-2-5C (16:07) ran Brief 2 and
retired the centerpiece — the critics ingested the run the synthesis missed. (ii) On **5D**, *both
critics themselves* went stale against `proto-1-overapprox/PROTO-1.md` (16:09), which ran Brief 1
(the over-approx sound re-run + the cold-pair) and PASSED every clause — yet CRIT-1 (16:17) and
CRIT-2 (16:23), both authored *after* it, repeatedly assert those exact probes "UNBUILT" / "does
not exist." Their 43% floor is set for a discharged reason; the named-erred correction raises 5D to
55%. **The same stale-gestalt failure mode recurred at two document levels in one pass — the
synthesis missed the 5C run, the critics missed the 5D run.**

---

## THE PASS-3 HEADLINE — TWO decisive runs landed AFTER the documents that judge them; both verdicts INVERT their document's floor

Both are the checklist's *unverified/stale gestalt* in its purest form, and the second is the more
instructive because it caught the critics doing exactly what they impeached the synthesis for.

- **5C — the automata-as-extraction CENTERPIECE is RUN-EARNED RETIRED (PROTO-2-5C, HEAD `ef10d5b78`,
  parse-that READ-ONLY).** Brief 2 ran. Corpus grown 26→56 (12 acyclic / 44 cyclic, runtime
  back-edge DFS); oracle **0/56 divergences** (all four minimizers + incumbent byte-identical, ρ=1).
  The `DfaSig` congruence rebuild degenerates to **O(n²·k)** on exactly the acyclic chain-DFAs it
  exists to accelerate (`[01]{8}` n=9 → **8 passes-to-fixpoint**; `log n` held only for shallow bushy
  alternations, refuted for every bounded chain); it is the **slowest correct lane** on acyclic
  (cong 37664 > null 24376 > revuz 17168). And `incumbent ≈ null` (<1% on both tags) shows the
  `:358` strawman **was never slow at regex scale** — the centerpiece had no cost-model foe to beat.
  Prior art (TRAP A, Remy Wang: e-graphs ARE tree automata; the working direction is automata→e-graph;
  DAG-extraction is NP-hard) **predicted this loss**; the run confirmed it. **FAMILY-SPECS declares this
  "unrun a THIRD pass" and re-issues it as Brief 2 — stale by 40 min, same author-cycle. Both critics
  caught it and re-cut 5C UP to 55%.**

- **5D — Brief 1 is RUN-EARNED PASSED (proto-1-overapprox, HEAD `ce51c36fc`, ff-only from ancestor
  `d2b32f6e3`, not wrong-artifact-risk).** The survivor's generator-path lowering policy is now
  ratified on a **sound** basis: (a) first(C) drawn from the **DFA start-state class set**
  (over-approx, measured off the parse-that anchored `Dfa` state-0 row) recovers the dropped
  alternation branches the under-approx `analyze().first` lost (`sheet_prefix` 1→54 bytes; `\$?`
  leading-optionals recovered; opaque `sheets::number` *tightened* 256→11); genuine-give-back holds at
  **3/16 = {word, sheets::string, sheets::identifier}**, **machine-asserted** in-test; **exactly one**
  disjointness flip (`sheet_prefix` true→false) is a `!`-delimiter-terminated **conservative
  false-positive in BOTH branches**, so **0 GENUINE flips** — no leaf goes genuinely sound→unsound.
  (c) Cold-pair (N=240×256, paired, black_box-sunk, correctness asserted at setup): tape
  **byte-identical 256/256** on both leaves; direct scan **3.25×** faster than `Dfa::find` on rare
  `[0-9a-fA-F]{6}` and **2.81–3.00×** on dense `[a-z]+` — both clear ≥0.98× and ≥0.95×. **Verdict:
  the DFA leaves the generator path; direct-scan-default RATIFIED, DFA→narrow fallback for the
  genuine trio. Both critics call this "UNBUILT" three passes running — they went stale against a run
  in a subdirectory of the very folder they reviewed.**

**The reconciliation this registry renders (written in neither the synthesis nor the critiques):**

> **PASS-3 VERDICT (rendered within LOCK-5). The two-types+bridge survivor (XL-C, RATIFIED) now has
> BOTH of its decisive run-gates closed: (1) its GENERATOR-PATH lowering is direct-scan-default with
> the DFA measured OFF the path (5D Brief 1 PASSED, sound source); (2) its AUTOMATA-INTERNALS
> deliverable is 2C-as-table + delete-`:358` + a single-pass Revuz — the e-graph-over-automata
> centerpiece is RUN-REFUTED and RETIRED (5C Brief 2 FAILED). The survivor's remaining work is
> INTEGRATION (build the `ByteClass` substrate, ship the tier table), not another type-plane or
> automata-race. The lock's TITULAR thesis — grammar-driven — remains at ZERO artifacts after THREE
> passes and is the one genuinely un-attempted formulation.**

---

## PRECONDITIONS (lock-wide, gate every family's earned convergence)

**P1 — the two-tree fork, now decisively favoring Fork-B (abrogate the grammar-tier regex e-graph).**
The `->` representation differs by tree (re-verified this pass): skinny lowers `->` to
`ExprKind::Project{body,target,ty}` (`skinny/crates/ir/src/lib.rs:261`) — a typed wrapping edge, with
**no grammar-tier regex e-graph at all**; the top-level tree lowers it to `GrammarENode::Map{inner,fn_id}`
(`crates/ir/src/egraph/node.rs:52`). PROTO-2 (fire-census, pass-3) measured the fork-gating number:
over 33 Alt-of-regex patterns, on **bare** `Alt([Regex,…])` the three regex-specific rules fire
(11/19/30 work) but on **projected** `Alt([Map(Regex,Tag),…])` every rule applies **exactly 0**
(`total_applied=0`, one iteration, no unions) — `class_regex` finds no bare `Regex` inside a `Map`
e-class. Real BBNF alternations almost always carry per-branch tags (`a->A | b->B`), so the `Map`
barrier is the common case → the grammar-tier regex e-graph is **consumer-less under
projection-domination (Y.13)** → **Fork-B is free, and 5B's 250-LOC de-dup is definitively a fork
artifact, not a family win.** GESTALT still picks the fork, subject only to counting how often real
grammars present a **bare** `Alt`-of-regex (PROTO-2 did NOT count that frequency). The number leans
abrogate.

**P2 — grammar-driven is SPEC-ONLY (re-confirmed; ZERO grammar-driven artifacts after THREE passes) —
the lock's TITULAR-DELIVERABLE FLOOR (both critics name it explicitly).** The lock is literally "the
grammar-driven regex engine." After three passes it has prototyped an IR-level fork census, an
IR-level lowering fraction+cold-pair, and a parse-that automata race — **not one grammar-driven regex
path**. `parse-that/.../regex/generated.rs:2288` `parse_generated(&str)->bool` still **discards its
AST**; two `regex.bbnf` still diverge (`bootstrap/regex.bbnf` 72 LOC L4-spec vs `grammar/misc/regex.bbnf`
27 LOC legacy EBNF). Every verdict rides `SPEC-ONLY(grammar-driven)`; **GESTALT-P must read that label
as "un-attempted three passes," NOT "converged pending LOCK-7."** The namesake work is
promote + de-dup + break-the-discard, gated LOCK-7/7A.

---

## FAMILY 5A — ONE-HIR — **ADVANCED — 42%** (maximal REFUTED pass-2; residual is a refactor, not architecture; NO 5A prototype ran pass-3; re-label candidate)

Regex as the fragment of the bbnf HIR carrying no rule reference / no projection. **Maximal
identity-merge REFUTED (PROTO-2, pass-2): REGEX-LEAK ≫ 0, cache Hir-keyed not Id-view, three
hand-mirrored enums.** Under the settled two-types verdict (XL-C) what survives is engine-tier
leaf-growth (`Class`/`Look`, already in parse-that `hir/mod.rs:117`) + the deletion of the skinny
name-gate heuristic + the shadow-crate deletion + the bridge re-point — **a refactor residual, largely
already-existing, homed under the survivor.**

**What sharpened this pass (credited, honest).** research-A conceded the 1148-LOC engine `classify.rs`
**RELOCATES, not shrinks** — its tier content (`AccelDriven`/`PrefixThenClass`/`CharClassQuantified`)
is real information an IR pass keeps; only the internal re-lex step deletes. The maximal "1148-LOC
collapse" was overclaim. The genuine skinny-side shrink is the ~200-LOC name-gate block
(`classify`/`regex_is_nullable`/`first_from_prefix`/`parse_leading_class`, `bbnf-regex/src/lib.rs:92`,
a Constitution-vi violation reconstructing first-sets the `Class`/`Repeat` leaves carry by
construction) + the 322-LOC shadow crate.

**Exact open gaps (all UNBUILT, several a THIRD pass).**
1. **The net-shrink SIGN IS UNKNOWN — the elegant-reduction trap.** Deleting the heuristic forces
   skinny to import a REAL parser (parse-that `hir/parser/` ~787 LOC). "Delete 522, import 787" may be
   a net GROWTH. G-5A.parser-cost UNBUILT; the family's sole distinguishing win ("SHRINK") is unsigned.
2. **The `/foo$/` Look-consumer gate is buildable-but-unbuilt a THIRD pass — a gate-that-cannot-fail.**
   `RegexKind` (`bbnf-regex/src/lib.rs:16`) has no anchor variant, so `/foo$/` and `/foo/` are
   indistinguishable (both over-accept) — a live correctness defect. Re-pointing `analyze` through the
   engine's `RegexInfo` (which parses `$`→`Look::End`) makes `/foo$/` reject `"foobar"`. Named
   buildable pass-1, pass-2, pass-3; never built.
3. G-5A.leak (`[a-c]≡[a-z]` identical `Span<width>`) UNBUILT; `Class ⊢ Span` graded only against
   skinny's stub `BuiltinTy`, not LOCK-1's real HM.
4. G-5A.3 automata parity UNRUN three passes — but research-B argues it is **moot** under the survivor
   (the bridge routes to the engine's single `from_hir`; there is no second NFA builder to keep in
   parity). If that holds, G-5A.3 retires as moot rather than unrun. PROVE-10's 8.8–29.5× band has
   never been reproduced from any narrowed type — SPEC-ONLY, but no perf CLAIM rests on it (discipline
   held).

**BLOCKED assessment: NOT BLOCKED** (no missing primitive), but the distinguishing mechanism is dead
and the residual is a refactor with every gate unbuilt and zero runs three passes. Convergence
**42%** (CRIT-1 floor taken: the surfaced parser-import cost is a *downgrade* to the shrink claim, and
"parked-refactor with four unbuilt gates" is not a 45%-converged core). **Re-label candidate:** if 5A
stays parked a fourth pass, it should be re-labeled from a family to "the survivor's bridge-hardening
refactor residual" (both critics gesture at this; neither committed it).

**Pass-4 directives.** *Build* the `/foo$/` failing-then-passing test (the gate buildable three
passes). *Measure* the shadow-deletion net ΔLOC AFTER paying the ~787-LOC parser import — sign, not
estimate. *Route* `Class ⊢ Span` / `Look ⊢ Unit` + width-var through LOCK-1's real HM. *Withhold:* the
"genuine SHRINK" prior — the sign is unknown until the parser import is priced.

---

## FAMILY 5B — LAWFUL-FUNCTOR — **RETIRED — 40%** (retire STRENGTHENED by PROTO-2; conditional on an unratified LOCK-1 fact)

Two domain-fit HIRs joined by a total functor through the shared e-graph; the win was
write-the-regex-algebra-once + delete the duplicate grammar-tier rules.

**RETIRED (earned-conditional, pass-2), STRENGTHENED this pass.** PROTO-2 (fire-census) moved the
fork-independence from *estimated* to *measured*: on projected branches the three regex-specific rules
apply **exactly 0** work (`proj total_applied=0`, one iteration) — the 250-LOC de-dup the functor
claimed dies **for free under Fork-B**, attributable to no family. The functor as an architecture with
independent value is dead; its soundness win (the projection-dropping-union bug) was already a non-bug
under the whole-leaf wrapper (pass-2, C2 not met). research-B's "total reverse functor" (`Hir→String`
structural unparse fixing `canonical_pattern`'s partiality) is real engineering but serves the string
BRIDGE (the survivor's `simplify_hir` round-trip oracle), not a two-way adjunction — **correctly
firewalled away from 5B so it does not launder a retired family's disposition** (both critics credit
this discipline).

**Sole re-open trigger (LOCK-1-gated, cross-lock).** The retire rests on `->` staying a whole-leaf
wrapper (`Project` skinny / `Map` top-level) that never binds a **sub-leaf** capture. LOCK-1 owns the
real `->` typing and has NOT ratified it. If LOCK-1 ratifies a sub-leaf-capture representation (5D's
capture-leaf ambition, `(/[0-9]+/->Digits)` reaching inside the class), the projection-dropping-union
bug reappears and un-retires 5B **AND** breaks 5D's `Project` byte-identity oracle — both hang on the
same unratified fact. Routed to LOCK-1 with the PROTO-2 evidence. The joint-coproduct variant stays
un-opened (dies undesigned; acceptable, the functor's core value is a proven non-bug).

**BLOCKED assessment: N/A (RETIRED).** Convergence **40%** (both critics concur) — disposition-certainty
of a conditional retire, raised in confidence by the measured fork-independence, not a shipping number.

**Pass-4 directives.** Nothing for the functor (retired). One residual consumer-assignment gap: the
total `Hir→String` unparse (bridge round-trip oracle) is currently nobody's gate — home it in the
survivor's bridge-hardening (5A-residual), not 5B. *Withhold:* the coproduct — leave un-opened unless
LOCK-1 reopens the projection-soundness question.

---

## FAMILY 5C — AUTOMATA-AS-EXTRACTION — **RETIRED — 55%** (centerpiece RUN-EARNED RETIRED; survivor re-homed & advancing; internal 2B-as-minimizer BLOCKED)

Push the shared e-graph one level below the HIR, to the automaton. **This distinguishing mechanism —
e-graph-over-automata as the thing that beats the classical pipeline — is RUN-EARNED RETIRED
(PROTO-2-5C, Brief 2, the decisive number unrun three passes now RUN).** The status is RETIRED per the
legend (distinguishing mechanism removed) at a *high* disposition-certainty (55% > 5B's 40%) because a
**concrete non-e-graph survivor ships** and the negative result is run-earned + prior-art-predicted.

**The run (PROTO-2-5C, `RAN_MEASURED — FAIL`).** Corpus 26→56 (12 acyclic / 44 cyclic); oracle 0/56
divergences (five lanes byte-identical, ρ=1). The `DfaSig` congruence rebuild is **O(n²·k)** on the
target acyclic class (`[01]{8}` n=9 → 8 passes; passes track DFA diameter, not log n — `log n` held
only for shallow bushy alternations, refuted for every bounded chain); it is the **slowest correct
lane** on acyclic. `incumbent ≈ null` (<1% both tags): the `:358` full-restart strawman was never slow
at regex scale — no cost-model foe existed. Prior art TRAP A predicted the loss (eqsat→automata runs
the isomorphism backwards; DAG-extraction NP-hard); the run confirmed it. **Brief 2 is DISCHARGED —
pass-4 must NOT re-budget it.**

**The three sub-verdicts.**
- **2A minimization-as-congruence — RETIRED as centerpiece** (O(n²·k) on its own target class; the
  slowest correct lane). What *survives* it: a single **reverse-topological Revuz pass** — the only
  lane whose passes stay at **1** as n grows, a genuinely fastest-on-acyclic **structural/table**
  optimization needing **no e-graph** (1.42× over bug-fixed Moore; modest, growing with n).
- **2B determinization-as-derivative-saturation — BLOCKED (as minimizer), Kozen** — regular-language
  star-equivalence needs the star-induction rule, not a finite ground rewrite → no finite e-graph rule
  set is complete (the lock's cleanest impossibility). 2B-as-*determinizer* survives (25/26 ρ=1);
  2A/Hopcroft closes.
- **2C prefilter/tier-selection — SURVIVES as a LOCAL TABLE, not a CSP.** research-C's zero-witness
  grep (no `lut_budget`/`codegen_budget`/`shared_lut`/`table_budget` anywhere in `crates/ir/src/`) →
  no shared resource → tier is local → the monotone `EngineSet` table wins; a CSP over it is Y.13
  substrate. This is the family's shipping survivor.

**SURVIVOR (re-homed, ADVANCING, needs no e-graph): 2C-as-table + delete-`:358` (mandatory, latent
~O(n³) bug, correctness-neutral, 0 divergences) + single-pass Revuz.** Integration is UNBUILT:
minimize-swap (`dfa.rs:89-91`), Valmari/Revuz native partial-DFA `DEAD` handling (`:330/:340/:423`),
tier-table home (`info/mod.rs:344`).

**Exact open gaps (the survivor's, not the centerpiece's).** (1) The **byte-rarity oracle** — a NAMED
MISSING INPUT for 2C-as-table's accel-byte quality (regex-automata's static ASCII-rank table, absent
in-tree; without it accel-byte selection "is guesswork"). This, not Brief 2, is the real pass-4 5C
uncertainty. (2) The de-confounded "correct Hopcroft beats congruence on TIMING" comparison was never
produced — PROTO-2-5C's Valmari lane is allocation-confounded (explicitly not load-bearing); the FAIL
rests soundly on the allocation-independent passes-count + `incumbent≈null`, sufficient to dispose the
centerpiece but insufficient to size the Revuz win against a de-confounded control. (3) `is_acyclic`
dispatch net-positivity is the weakest measured conjunct (against the fair `null` control:
Σ(null−revuz)=7208 ns > 6786 ns dispatch — marginal). (4) survivor integration unbuilt (above).

**BLOCKED assessment: FAMILY status RETIRED (centerpiece); internal 2B-as-minimizer BLOCKED (Kozen).**
Convergence **55%** (both critics; re-cut UP from the stale 45%) — convergence-of-disposition, high
because the decisive control RAN and a concrete survivor ships; NOT a shipping number (integration
unbuilt).

**Pass-4 directives.** *Ship* the survivor (2C-table + delete-`:358` + single-pass Revuz). *Land* the
byte-rarity oracle. *Do NOT* re-run the centerpiece race (Brief 2 discharged). *Route to LOCK-2 as a
BINDING CONSTRAINT (not a proposal):* automata-minimization is NOT an e-graph island and tier-selection
is NOT a CSP (run-refuted + prior-art) — two of LOCK-2's candidate island/CSP sites are refuted at the
regex layer. *Withhold:* any "e-graph automata could still win at large-n" re-opening — the passes
measure is scale-free.

---

## FAMILY 5D — GRAMMAR-FRAGMENT — **ADVANCED — 55%** (the survivor's generator-path lowering policy; Brief 1 RUN-EARNED PASSED; both critics stale against the run)

A `/pat/` lowers DIRECTLY into native grammar IR — a straight-line class-scan
(`Recognizer::ByteClass` membership over the tape, no automaton) when the leaf is FOLLOW-sound; a
`Seq[Repeat, FOLLOW-guard]` or the DFA fallback for genuine give-back. The Glushkov/Berry-Sethi
position automaton (FIRST/LAST/FOLLOW over a marked expression) done as an IR lowering. Under XL-C, 5D
is **the generator-path lowering policy of the two-types survivor.**

**Brief 1 RAN and PASSED every clause (proto-1-overapprox, HEAD `ce51c36fc`) — the decisive advance
both critics missed.** The under-approx PROTO-1 (pass-3 base) established the 3/16 partition on
`analyze().first`, flagged (its own Caveat 1) as unsound-for-the-verdict. proto-1-overapprox supplied
the SOUND source and re-ran:
- **Over-approx first(C) = the DFA start-state class set** (measured off the parse-that anchored `Dfa`
  state-0 row). It recovers the dropped alternation branches (`sheet_prefix` 1→54 bytes {`,A-Z,_,a-z};
  cell_ref/range_end `\$?` leading-optionals 1→53/53/11) and *tightens* opaque `sheets::number`
  256→11 {.,0-9}. A leaf absent from the table falls back to the full alphabet (maximal sound
  over-approx) — the verdict can never silently go unsound.
- **genuine-give-back holds at 3/16 = {word, sheets::string, sheets::identifier}, MACHINE-ASSERTED
  in-test** (`flips == ["sheet_prefix"]`, genuine trio 3/3 non-disjoint, counterexample `word` overlap
  ∋ `'a'`). The single disjointness flip (`sheet_prefix` true→false) is a `!`-delimiter-terminated
  **conservative false-positive in BOTH branches** — one-pass-safe by construction, exactly the kind
  the raw predicate over-flags — so **0 GENUINE flips**; no leaf goes genuinely sound→unsound; no new
  give-back revealed. (The raw brief-literal count moves 3/16→2/16 only because `sheet_prefix` leaves
  the raw-disjoint bucket; the architecture-gating count holds at 3/16.)
- **Cold-pair (N=240×256, paired, black_box-sunk, correctness asserted at setup):** tape
  **byte-identical 256/256** on both leaves; direct scan **3.25×** faster than `Dfa::find` on rare
  `[0-9a-fA-F]{6}` (1.37 vs 4.24–4.40 ns) and **2.81–3.00×** on dense `[a-z]+` (4.16 vs 11.79–12.67
  ns) — both clear ≥0.98× and ≥0.95×.

⇒ **the DFA leaves the generator path; direct-scan-default is RATIFIED, DFA→narrow fallback for the
genuine trio.** This gates LOCK-9-9C and LOCK-4-4D. The FOLLOW predicate is a genuine Changed-flag
fixed point (nullable→first→follow, per-`ExprId`, `Ref` inter-rule bridge) that DEFEATS the unsound
`check_one_pass_eligible` (`one_pass.rs:64`, whose `Repetition` arm recurses without a FOLLOW check
and greenlights a straight-line lowering that rejects `"a and b"`).

**Why both critics erred (named).** CRIT-1 (§5D gaps 1–3) and CRIT-2 (§5D G5D-a/c) both assert the
over-approx re-run and the entire cold-pair "UNBUILT" / "does not exist" — but
`proto-1-overapprox/PROTO-1.md` (16:09) predates CRIT-1 (16:17) and CRIT-2 (16:23) and contains both,
RAN and PASSING. The critics descended into `PROTO-2-5C.md` (16:07) and correctly caught the synthesis
stale on 5C, then did NOT descend into the `proto-1-overapprox/` subdirectory (16:08) and went stale on
5D themselves. Their 43% floor is set for a discharged reason → the named-erred correction raises 5D to
**55%** (the two "sharpest uncertainties" they flagged UNBUILT are the two that RAN and PASSED).

**Exact open gaps (what genuinely remains — integration + breadth, not thesis).**
1. **The `ByteClass` substrate is DESIGNED, not built** (a LOCK-3-governed multi-crate edit beyond a
   parsimonious probe): add `Recognizer::ByteClass{set,site}` (`ir/src/lib.rs:416`), gate
   `derive_recognizers` (`passes/src/lib.rs:338`) on the FOLLOW fixed point in place of the
   `QuotedString`/`_ => {}` masked wildcard, route `BackendExpr::ByteClass` to the tape
   (`codegen/src/lower/tape_plan.rs`). **Its one correctness obligation — a direct class-scan emits the
   same tape a DFA match would — is EMPIRICALLY DISCHARGED (256/256 byte-identical), so the tape-risk is
   retired; only the wiring is unbuilt.** This is the piece LOCK-9-9C waits on (see cross-lock #4).
2. **Corpus breadth.** The fraction is over **16 leaves** with **approximate** sheets FOLLOW-wiring
   (39-rule skinny subset; host-heavy rules approximate — affects FOLLOW breadth only, never the
   counterexample gate or the genuine trio). The over-approx re-run swapped first(C) on the same 16
   leaves; corpus-representativeness is untested. Grow with exact sheets wiring.
3. **Capture-leaf → `Project`** byte-identical to a hand-authored grammar capture, under LOCK-1's
   ratified `->` — UNBUILT; hangs on the same unratified sub-leaf-capture fact as the 5B re-open trigger.

**BLOCKED assessment: NOT BLOCKED.** Convergence raised to **55%** — the family's DISTINGUISHING thesis
(direct-scan-default, DFA off the generator path) is now RUN-EARNED on a sound source with cold-pair
evidence and a machine-asserted partition; the tape-risk is empirically retired. What remains is
substrate wiring + corpus breadth + the LOCK-1-gated capture-leaf — integration, not architecture.

**Pass-4 directives.** *Build* the `ByteClass` substrate (the tape-obligation is already discharged;
this unblocks LOCK-9-9C). *Grow* the corpus beyond 16 leaves with exact sheets FOLLOW-wiring. *Withhold:*
nothing to re-litigate on the verdict — hand researchers the RATIFIED direct-scan-default policy to
IMPLEMENT, plus the capture-leaf oracle pending LOCK-1.

---

## REDIRECT-CROWDING DECISIONS

- **Axis-1 stays CLOSED (pass-2); the survivor's TWO run-gates both closed this pass.** 5A-identity
  refuted, 5B-functor retired (both pass-2). This pass closed the survivor's remaining decisive
  gates: 5D's generator-path lowering (Brief 1 PASSED) and 5C's automata-internals disposition (Brief
  2 FAILED → survivor is 2C-table + `:358` + Revuz). No new crowding among the surviving pieces —
  **5A-residual (bridge/shadow/typing), 5D (lowering policy), 5C-survivor (tier table + bug fix) are
  COMPLEMENTARY, not redundant**; none is banked-as-redundant.
- **The 250-LOC de-dup stays BANKED as a standalone refactor residual** (neither 5A nor 5B) — free
  under Fork-B, a bare string-bridge reuse of `simplify_hir` under Fork-A. Crediting it to any live
  family launders a retired family's win.
- **The underexplored formulation to OPEN remains the GRAMMAR-DRIVEN BOOTSTRAP path** (constitution
  i's namesake). After three passes with ZERO grammar-driven artifacts, the genuinely un-attempted
  thing is not another type-plane or automata variant but the **break-the-discard** work: promote
  `bootstrap/regex.bbnf`, make `parse_generated()` return its AST not `bool`, de-dup
  `grammar/misc/regex.bbnf`. Gated LOCK-7/7A; the ONE formulation no pass has touched.
- **5A is parked but NOT redundant-with-5D** (complementary hardening) — so it is not banked-as-crowded;
  instead it carries a **re-label directive** (family → refactor-residual) if it stays parked a 4th pass.

---

## CROSS-POLLINATION (named grafts only, both sides developed)

- **GRAFT-1 (WITHDRAWN for the minimizer; dormant for the determinizer).** Pass-2's "engine `Hir`'s
  derived `Language` hosts 5C-2B's saturation" survives only for 2B-as-**determinizer** (25/26 ρ=1,
  un-exercised this pass, low priority) — the 2B/2A **minimizer** graft is RETIRED with the centerpiece
  (PROTO-2-5C). Do not carry it forward as an active graft.
- **GRAFT-2 (mechanical, holds): the byte-class partition (`sets/equiv.rs`, num_classes≤8) consumes
  the engine's `Class` byte-sets** — now feeding Revuz + the 2C table (not the retired congruence
  minimizer). A consequence of riding the survivor's engine `Hir`, not a separate build.
- **GRAFT-3 (NEWLY NAMEABLE — the survivor's prefilter/lowering SEAM): 5D's direct-scan-vs-DFA
  FOLLOW-guard ⊗ 5C-2C's tier table.** Both decide "class-scan vs DFA" for the same leaves from two
  homes — 5D on the generator path (FOLLOW-sound → direct scan), 2C at the engine tier (per-pattern
  `EngineSet`). They must be **ONE consistent policy, not two divergent decisions.** Both sides are now
  developed enough to name the graft: 5D's verdict is RATIFIED (Brief 1) and 2C-as-table is settled
  (zero-witness). Coherence requirement, not a deferred coupling — the pass-4 substrate build must wire
  them as a single decision surface (feeding the PROVE-10 prefilter seam).

---

## CROSS-LOCK DOUBLE-HOMES (constitution ix — LOCK-5 is the hub; resolutions are LOCK-5 proposals
GESTALT RATIFIES, except where a RUN makes them BINDING inputs)

1. **Axis-1 verdict ⊗ LOCK-4-4D / LOCK-9-9C / LOCK-3-3C — RE-CUT (carried, still owed).** XL-C
   ratified two-types+bridge; 4D's premise ("one HIR, not a second engine") is weakened; 4D/9C/3C
   consume the two-types landing or converge on a refuted verdict. Still nobody's charter.
2. **5C REFUTATION ⊗ LOCK-2 — now a BINDING INPUT, not a proposal (new, load-bearing this pass).**
   LOCK-2's charter offers `2B-IS-EGRAPH` (middle IR is an e-graph end-to-end) and `2C-CSP-CENTRAL`
   (the decision driver IS a CSP). PROTO-2-5C + prior-art TRAP A jointly establish that **automata
   minimization must NOT be an e-graph island** (run-refuted O(n²·k), incumbent≈null) **and
   tier-selection must NOT be a CSP** (zero shared-resource coupling; local table wins). Two of LOCK-2's
   candidate island/CSP sites are refuted at the regex layer — GESTALT-Ω owes the reconciliation.
   Single-home the tier table in 5C; escalate to LOCK-2's CSP only on an exhibited shared-LUT/Teddy
   budget witness (none exists).
3. **5B retirement ⊗ LOCK-1 (RE-OPEN TRIGGER).** The retire rests on `->` staying a whole-leaf wrapper
   that never binds sub-leaf captures. Sub-leaf-capture representation un-retires 5B AND breaks 5D's
   `Project` byte-identity oracle — both hang on the same unratified LOCK-1 fact. Routed with PROTO-2
   evidence.
4. **5A/5D ⊗ LOCK-9-9C (SCANNER-AS-DFA) — dependency DE-RISKED but still UNDELIVERED.** LOCK-5 owns the
   `Hir`→DFA→classify substrate + the `ByteClass` chooser; 9C consumes it or retires. The `ByteClass`
   producer is UNBUILT (masked wildcard) — 9C is a consumer waiting on an un-delivered producer
   (reverse-Y.13 DAG hazard). **BUT Brief 1 empirically discharged the producer's one correctness
   obligation (direct-scan tape ≡ DFA tape, 256/256), so the design is validated — 9C's dependency is
   de-risked, only the wiring remains** (pass-4 5D directive #1 delivers it).
5. **5A ⊗ LOCK-7 bootstrap.** Two divergent `regex.bbnf` (one discards its AST). Re-scoped "promote +
   de-dup + break-the-discard"; SPEC-ONLY(bootstrap) until 7A. Zero artifacts three passes.
6. **Type-plane ⊗ LOCK-1.** `Class ⊢ Span` / `Look ⊢ Unit` + width-var graded only against skinny's
   stub `BuiltinTy`. Route through LOCK-1's real HM + row-poly + widths.
7. **rust/ts ⊗ LOCK-6.** The mirror/skeleton carries BOTH types (`Regex(StringId)` grammar-side + rich
   `Hir`/`Class`/`Look`/`ByteClass` engine-side + automata crossing to TS, PROVE-14 tape) — larger than
   "carry the merged enum." The serde-derive-ownership question (who adds `CharClass: Serialize`,
   pass-2 E0277) lands on the parse-that READ-ONLY / LOCK-6 boundary — unresolved two passes (pass-3
   PROTO-2-5C had to run in a READ-ONLY scratch with 7 repointed patch prefixes). Named gap for GESTALT-Ω.

---

## PASS-3 DIRECTIVES (consolidated, ordered by what gates the most downstream)

1. **Record BOTH stale-gestalt corrections as the pass's central process finding** (institutional
   guard): reconcile the registry to EVERY prototype in-folder before cutting — the synthesis missed
   PROTO-2-5C (5C), the critics missed proto-1-overapprox (5D). 5C centerpiece RUN-EARNED RETIRED
   (Brief 2 DISCHARGED — do NOT re-budget); 5D Brief 1 RUN-EARNED PASSED (direct-scan-default RATIFIED,
   DFA off the generator path).
2. **5D: BUILD the `ByteClass` substrate** (`Recognizer::ByteClass` + `BackendExpr::ByteClass` → tape;
   gate `derive_recognizers` on the FOLLOW fixed point) — the tape-obligation is empirically discharged;
   this unblocks LOCK-9-9C. GROW the corpus beyond 16 leaves with exact sheets FOLLOW-wiring.
3. **5C: SHIP the survivor** (2C-as-table + delete-`:358` + single-pass Revuz, no e-graph); land the
   byte-rarity oracle (the named missing input). Do NOT re-run the centerpiece.
4. **Route the 5C→LOCK-2 BINDING** (not proposal): automata-min NOT an e-graph island, tier NOT a CSP.
   Single-home the tier table in 5C.
5. **5A: BUILD the `/foo$/` Look-consumer test** (buildable-but-unbuilt three passes); MEASURE the
   shadow-deletion net ΔLOC after paying the ~787-LOC parser import (sign unknown); route Class/Look
   typing through LOCK-1's HM. RE-LABEL 5A as a refactor residual if it stays parked a 4th pass.
6. **EXERCISE ONE grammar-driven regex path** (the lock's namesake, 0 artifacts / 3 passes): promote
   `bootstrap/regex.bbnf`, make `parse_generated` return its AST, de-dup `grammar/misc/regex.bbnf`.
   SPEC-ONLY = "never attempted," not "pending."
7. **RATIFY (not declare)** the cross-lock resolutions with the counterpart locks at GESTALT: the
   4D/9C/3C two-types re-cut; the LOCK-1 `->` sub-leaf-capture re-open trigger; the LOCK-2 tier
   single-home + the 5C 2B/2C constraint; the LOCK-6 serde-ownership + both-types mirror obligation.
8. **Carry the SPEC-ONLY(grammar-driven) label** on every verdict line until 7A (P2); GESTALT-P reads
   it as "un-attempted three passes," not "converged pending LOCK-7."

---

## PER-PASS HISTORY

### Pass 3 (research-{A,B,C,D,priorart} → FAMILY-SPECS → PROTO-1[5D under-approx], PROTO-2[5B/fork census], PROTO-2-5C[5C Brief 2], proto-1-overapprox[5D Brief 1] → CRIT-1, CRIT-2 → this)

| Family | Status | % | One-line |
|---|---|---|---|
| 5A ONE-HIR | ADVANCED (refactor residual) | 42 | No 5A proto pass-3; classify-relocates honestly conceded (credit); `/foo$/` gate buildable-but-unbuilt a THIRD pass; net-shrink SIGN unknown (contingent on ~787-LOC parser import); re-label candidate if parked a 4th pass; CRIT-1 floor taken |
| 5B LAWFUL-FUNCTOR | **RETIRED (earned-conditional)** | 40 | PROTO-2 measured fork-independence (proj `total_applied=0`) — retire STRENGTHENED; conditional only on LOCK-1 keeping the whole-leaf `->` repr; laundering firewall held (reverse-`Hir→String` unparse homed in the bridge, not 5B) |
| 5C AUTOMATA-AS-EXTRACTION | **RETIRED (centerpiece; survivor advancing)** | 55 | RE-CUT UP from the stale 45%: PROTO-2-5C ran Brief 2 — cong O(n²·k) ([01]{8} n=9→8 passes), incumbent≈null, slowest correct lane, prior-art-predicted loss confirmed → centerpiece RETIRED; survivor = 2C-table + delete-`:358` + no-egraph Revuz; 2B-as-minimizer BLOCKED (Kozen); Brief 2 DISCHARGED |
| 5D GRAMMAR-FRAGMENT | **ADVANCED** | 55 | RAISED above the critics' 43% (NAMED erred: both stale vs proto-1-overapprox 16:09): Brief 1 RAN & PASSED — over-approx sound first(C), genuine-give-back 3/16 machine-asserted, 0 GENUINE flips, cold-pair tape 256/256 identical, direct-scan 3.25×/2.81-3.00× ≥0.98× → DFA OFF the generator path, direct-scan-default RATIFIED; substrate wiring + corpus breadth remain (integration, not thesis) |

**Prototypes run this pass (FOUR — the two decisive ones landed AFTER the documents judging them).**
PROTO-1 (5D under-approx, HEAD `ce51c36fc`): 3/16 partition on `analyze().first`, self-flagged unsound
source. PROTO-2 (5B/fork fire-census, `ce51c36fc`): projected-branch rule work `total_applied=0` →
Fork-B free, 5B retire strengthened. **PROTO-2-5C (5C Brief 2, HEAD `ef10d5b78`, parse-that READ-ONLY):
FAIL — centerpiece RETIRES; 56-pattern corpus, 0/56 oracle, cong O(n²·k), incumbent≈null. The
FAMILY-SPECS (15:27) predated it (16:07) and declared it unrun.** **proto-1-overapprox (5D Brief 1,
`ce51c36fc`): PASS — over-approx sound re-run + cold-pair; direct-scan-default RATIFIED. BOTH CRITICS
(16:17, 16:23) postdated it (16:09) and declared it UNBUILT.** The registry is re-cut from the
prototypes, not the stale synthesis or the stale critiques.

### Pass 2 (research-{A,B,C,D,priorart} → FAMILY-SPECS → PROTO-2[5A brief], PROTO-1[5B brief] → CRIT-1, CRIT-2 → registry)

| Family | Status | % | One-line |
|---|---|---|---|
| 5A ONE-HIR | ADVANCED (maximal REFUTED) | 45 | PROTO-2 RAN: clean-merge refuted — REGEX-LEAK ≈29 ≫ 0, cache Hir-keyed not Id-view, serde break (artifact-removable), 3 mirrored enums; narrowed Class/Look survivor feeds the Axis-1 winner |
| 5B LAWFUL-FUNCTOR | **RETIRED (earned-conditional)** | 40 | PROTO-1 RAN 5B's own gate: copy dies 250 LOC + 59/59 DFA-equiv, but C2 (projection-drop soundness bug) NOT MET — a non-bug under the Map-wrapper → dies to a bare merge; conditional on LOCK-1 keeping Map-barrier |
| 5C AUTOMATA-AS-EXTRACTION | ADVANCED (2B-as-minimizer BLOCKED) | 45 | NO 5C prototype ran; reference-Hopcroft control + delete-`:358` null + grown corpus + 2C all unrun; centerpiece effectively refuted, survivor a 15%-contingent strawman-measured sliver; 2B BLOCKED (Kozen) |
| 5D GRAMMAR-FRAGMENT | BANKED (survivor's lowering policy) | 35 | 3 in-tree facts source-CONFIRMED (unsound one-pass predicate, masked `ByteClass` wildcard, opaque `RegexProgram` emitter); diagnosis sharpened, resolution unbuilt; re-homed off the refuted 5A-maximal |

**Axis-1 resolved by the runs: two domain-fit types + cheap string bridge (owned by no family's
charter), rendered as the lock's verdict; later RATIFIED as XL-C.**

### Pass 1 (research-{A,B,C,D,priorart} → FAMILY-SPECS → PROTO-1, PROTO-2 → CRIT-1, CRIT-2 → registry)

| Family | Status | % | One-line |
|---|---|---|---|
| 5A ONE-HIR | ADVANCED | 55 | Tax+nullability RAN & favourable but pre-favouring tree; bootstrap, automata parity, `Look`-consumer, classify re-home, god-type all SPEC-ONLY |
| 5D GRAMMAR-FRAGMENT | BANKED (into 5A) | 35 | Maximal "no engine" reading RETIRED by PROVE-10; folded as 5A's lowering policy; one-pass fraction + throughput UNMEASURED; masked-wildcard contradiction |
| 5B LAWFUL-FUNCTOR | BANKED | 45 | Sound but converges with 5A + value-target (434-LOC copy) absent from the graduating tree; held behind the un-bank/retire gate |
| 5C AUTOMATA-AS-EXTRACTION | ADVANCED | 60 | 2B-as-minimizer BLOCKED (ρ_max=1.667); 2A EXACT but 3.53× slower, 15% acyclic fast path vs a strawman baseline; 2C unprototyped + double-homed |

**Trajectory.** 5A 55→45→42 (maximal RAN-refuted pass-2, then parked-refactor with unsigned shrink
pass-3). 5B 45→40→40 (BANKED→RETIRED pass-2, strengthened pass-3). 5C 60→45→55 (no run pass-2; Brief 2
RAN pass-3 → centerpiece RUN-EARNED RETIRED, survivor concrete → disposition-certainty rises). 5D
35→35→55 (diagnosis pass-2; Brief 1 RUN-EARNED PASSED pass-3 → direct-scan-default RATIFIED, DFA off the
generator path — the critics' 43% floor corrected for stale-against-the-run).
