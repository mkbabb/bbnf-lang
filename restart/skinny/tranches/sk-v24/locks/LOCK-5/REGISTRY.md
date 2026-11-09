# LOCK-5 — THE REGEX CONTINUUM — FAMILY REGISTRY

Grammar-driven regex engine: the regex HIR truly isomorphic to the bbnf HIR/IR, with
NFA→DFA→minimize→prefilter recast as solver problems where measured to win.

**Status legend.** ADVANCED = active line, earns prototype budget. BANKED = sound but
redundant-under-squeeze or absorbed into a sibling; held behind a named un-bank/retire gate; not
killed. BLOCKED = the family's *maximal claim* rests on a missing primitive as-hard-or-harder than
the original problem (the family may survive narrowed). RETIRED = the family's distinguishing
mechanism is removed from the portfolio (a re-homed survivor may persist and advance separately).

**Convergence scoring.** Conservative floor of the two within-lock critics, raised only with a
named reason a critic erred. This registry is the pass-3 **FINAL re-cut**: it supersedes the 16:32
afternoon agglomeration (which integrated the afternoon prototypes but predated the EVENING
grammar-driven runs and the two FINAL critiques 23:31/23:37). The floors below are taken from the
final CRIT-1 (23:31) and CRIT-2 (23:37).

---

## THE PASS-3 FINAL HEADLINE — the STALE-GESTALT defect recurred a THIRD consecutive time, now on the lock's TITULAR deliverable; both final critics HIT it against the same directory; the titular floor is CORRECTED from "0 artifacts / un-attempted" to PARITY-DEMONSTRATED

This is the pass's dominant process finding and its dominant substantive correction, and they are the
same event. The `stale-synthesis` failure mode has now fired at THREE document levels across two
passes, each caught by the critics one layer down:

- **Pass-2:** CRIT-2 named it.
- **Pass-3 (15:27 FAMILY-SPECS):** committed on 5C — the synthesis froze 40 min before PROTO-2-5C ran
  Brief 2 and retired the centerpiece. Both critics caught it; the 22:40 re-cut fixed 5C (→ RETIRED,
  55%).
- **Pass-3 (22:40 FAMILY-SPECS re-cut):** cured 5C and simultaneously committed the SAME defect on the
  lock's TITULAR thread — it declares grammar-driven "**ZERO artifacts after three passes … the one
  genuinely un-attempted formulation**" while `PROTO-2-P2.md` (20:03, **2h37m earlier**, in its own
  directory) is the FIRST grammar-driven artifact RAN_MEASURED, and re-issues Briefs 1 & 2 as "the two
  sharpest pass-4 uncertainties" when BOTH had already been executed (PROTO-1-5D 22:51 IS Brief 1;
  PROTO-2-P2-CLOSED 22:59 IS Brief 2, CLOSED to 56/56). **Both final critics (CRIT-1 23:31, CRIT-2
  23:37) HIT this as their dominant finding.** The 16:32 registry likewise predates the evening runs —
  THIS re-cut is the first agglomeration to integrate all of them.

**THE TITULAR RE-CUT (the single correction that most changes this lock's honesty).** The
grammar-driven path is NOT un-attempted. Verified from source + three run logs this pass:

> **PASS-3 FINAL VERDICT (rendered within LOCK-5). The grammar-driven titular deliverable is
> PARITY-DEMONSTRATED, not "0 artifacts."** `regex::parse_generated` (`mod.rs:34`) already returns
> `Result<Hir, ParseError>` — the discard is broken at the outer layer; the AST-discarding
> `generated.rs:2288 -> bool` the prior synthesis "source-verified" is a diagnostic SHIM, the wrong
> symbol. On the real path (generated parser → typed `RegexParserEnum` AST → 525-LOC `host::lower`
> fold → bespoke `Hir`): PROTO-2-P2 measured-feasible 43/45; **PROTO-2-P2-CLOSED closed the gap to RAW
> 56/56 AND BRIDGE 56/56 byte-DFA-identical to the hand parser** (renumbering-invariant canonical
> minimal-DFA oracle, strictly stronger than match-equivalence; both raw and through the XL-C
> `simplify_hir` bridge), **0 rejects / 0 divergences, strict full-parity gate GREEN**, closed by a
> **5-edit host-only re-sync (`host.rs +50/−6`, no grammar change, no regen)**. The de-dup surface is
> CLOSED: **1 promote (`octal`) / 21 delete (17 dominated + 4 non-regular out-of-scope) / 3 relocate ⇒
> retire `grammar/misc/regex.bbnf`**. The remaining gap is NOT feasibility — it is (a) ONE architectural
> choice, GESTALT-P's call: host-fold re-sync (verified, smaller surface) vs grammar-carries-typed-
> captures + regen (cleaner, unbuilt); (b) LOCK-7A promotion to production parser + the self-hosting
> bootstrap (regex.bbnf embeds regex leaves to parse itself — untouched); (c) two constructs (bare
> `^`/`$`, `\p{}`) verified by REASONING not oracle (absent from the 56-corpus — exactly the construct
> 5A's `/foo$/` Look-defect predicts is mishandled). **The correct floor for GESTALT-P is
> PARITY-DEMONSTRATED-in-scratch, LOCK-7A-gated — NOT "un-attempted."**

The two decisive 5C/5D runs of the afternoon (both PASSED/FAILED their document's floor) stand as
already integrated in this registry; the evening runs are the new integration. All three families the
survivor rests on now have their decisive gates closed AND the titular thread has its first artifacts.

---

## PRECONDITIONS (lock-wide, gate every family's earned convergence)

**P1 — the two-tree fork, now decisively favoring Fork-B (abrogate the grammar-tier regex e-graph).**
The `->` representation differs by tree (re-verified): skinny lowers `->` to
`ExprKind::Project{body,target,ty}` (`skinny/crates/ir/src/lib.rs:261`) — a typed wrapping edge, with
**no grammar-tier regex e-graph at all**; the top-level tree lowers it to `GrammarENode::Map{inner,fn_id}`
(`crates/ir/src/egraph/node.rs:52`). PROTO-2 (fire-census, pass-3) measured the fork-gating number:
over 33 Alt-of-regex patterns, on **bare** `Alt([Regex,…])` the three regex-specific rules fire
(11/19/30 work) but on **projected** `Alt([Map(Regex,Tag),…])` every rule applies **exactly 0**
(`proj total_applied=0`, one iteration, no unions). Real BBNF alternations almost always carry
per-branch tags (`a->A | b->B`), so the `Map` barrier is the common case → the grammar-tier regex
e-graph is **consumer-less under projection-domination (Y.13)** → **Fork-B is free, and 5B's 250-LOC
de-dup is a fork artifact, not a family win.** GESTALT still picks the fork, subject only to counting
how often real grammars present a **bare** `Alt`-of-regex (PROTO-2 did NOT count that frequency). The
number leans abrogate.

**P2 — grammar-driven is PARITY-DEMONSTRATED (CORRECTED this pass from the prior FALSE "SPEC-ONLY /
ZERO artifacts" floor).** The lock is literally "the grammar-driven regex engine." After three passes
it now has THREE grammar-driven-thread artifacts, all RAN_MEASURED: PROTO-2-P2 (first artifact, 43/45),
PROTO-2-P2-CLOSED (56/56 full parity, gap closed), and PROTO-1-5D (the `ByteClass` generator-path
substrate landing + fresh-compiling). `parse_generated` already returns `Hir` at `mod.rs:34`;
"break-the-discard" was never a change to author. **GESTALT-P must read the titular label as
"PARITY-DEMONSTRATED-in-scratch, LOCK-7A-gated for promotion + self-hosting bootstrap," NOT "un-attempted
three passes."** The namesake work remaining is: the host-fold-vs-grammar-captures architectural choice
(GESTALT-P), the LOCK-7A promotion + self-hosting bootstrap, and the bare-anchor/`\p{}` oracle corpus.
This is a converged CORE with an exact gap — see FAMILY P2 below.

---

## FAMILY 5A — ONE-HIR — **ADVANCED — 42%** (parked refactor residual; maximal REFUTED pass-2; NO 5A prototype ran pass-3; re-label candidate)

Regex as the fragment of the bbnf HIR carrying no rule reference / no projection. **Maximal
identity-merge REFUTED (PROTO-2, pass-2): REGEX-LEAK ≫ 0, cache Hir-keyed not Id-view, three
hand-mirrored enums.** Under the settled two-types verdict (XL-C) what survives is engine-tier
leaf-growth (`Class`/`Look`, already in parse-that `hir/mod.rs:117`) + the deletion of the skinny
name-gate heuristic + the shadow-crate deletion + the bridge re-point — a refactor residual, largely
already-existing, homed under the survivor. **Both final critics concur at 42%; nothing moved it this
pass; zero 5A prototypes ran across three passes.**

**What sharpened this pass (credited, honest).** research-A conceded the 1148-LOC engine `classify.rs`
**RELOCATES, not shrinks** (research-A line 88–90, "≈0 net shrink") — a genuine anti-overclaim; its
tier content (`AccelDriven`/`PrefixThenClass`/`CharClassQuantified`) is real information an IR pass
keeps; only the internal re-lex step deletes. The genuine skinny-side shrink is the ~200-LOC name-gate
block (`bbnf-regex/src/lib.rs:92`) + the 322-LOC shadow crate — but see gap 1.

**Exact open gaps (all UNBUILT; four gates, zero runs three passes).**
1. **G-5A.shrink — net-shrink SIGN UNKNOWN, the elegant-reduction trap.** Deleting the ~200-LOC
   name-gate + ~322-LOC shadow crate forces skinny to import a REAL parser (parse-that `hir/parser/`
   ~787 LOC). "Delete 522, import 787" may be net GROWTH; the family's sole distinguishing win
   ("SHRINK") is unsigned. **PROTO-2-P2-CLOSED's de-dup count de-risks the LOCK-7 GRAMMAR seam but NOT
   the skinny-side PARSER-IMPORT edge — different edges** (CRIT-2 gap 1). UNBUILT.
2. **G-5A.look — a LIVE correctness defect, buildable-but-unbuilt a THIRD pass, now a JOINT 5A/5D gate.**
   `build_look` (`nfa.rs:254`) collapses all six `Look` to one zero-width state; anchoring is enforced
   OUTSIDE the automaton by `detect_anchored` reading the *Hir*, so `/foo$/` wrongly accepts `"foobar"`
   unless the search layer consults it. The failing-then-passing test
   (`search(compile("foo$"),"foobar").is_none()`) has never run. **This is the exact construct
   PROTO-2-P2-CLOSED verified "by reasoning, not oracle" — 5A's Look-defect and P2's un-oracled
   bare-anchor arm point at the SAME untested surface** (both critics: the `Look`/`ByteClass` chooser is
   a JOINT anchor consumer, not 5A-only).
3. **G-5A.leak** (`[a-c]≡[a-z]` identical `Span<width>`) UNBUILT; `Class ⊢ Span` graded only against
   skinny's stub `BuiltinTy`, not LOCK-1's real HM. LOCK-1-gated.
4. **The one measurable win is credited to the SURVIVOR, not 5A** — the enum→`SpanFacts` collapse
   (first-sets already triplicated + drifting: `bbnf-regex class_byte` returns `None` on `\d`-in-class,
   `regex_first` does not). A family whose non-dead content is homed elsewhere is at
   disposition-certainty, not convergence.

**BLOCKED assessment: NOT BLOCKED** (no missing primitive), but the distinguishing mechanism is dead and
the residual is a refactor with four unbuilt gates and zero runs three passes. Convergence **42%** (both
critics' floor). **Re-label directive:** if 5A stays parked a fourth pass, re-label it from a family to
"the survivor's bridge-hardening refactor residual."

**Pass-4 directives.** *Build* the `/foo$/` failing-then-passing test (the JOINT 5A/5D anchor consumer,
buildable three passes). *Measure* the shadow-deletion net ΔLOC AFTER paying the ~787-LOC parser import —
sign, not estimate. *Route* `Class ⊢ Span` / `Look ⊢ Unit` + width-var through LOCK-1's real HM.
*Withhold:* the "genuine SHRINK" prior — the sign is unknown until the parser import is priced.

---

## FAMILY 5B — LAWFUL-FUNCTOR — **RETIRED — 40%** (retire STRENGTHENED by PROTO-2; conditional on an unratified LOCK-1 fact; cleanest disposition)

Two domain-fit HIRs joined by a total functor through the shared e-graph; the win was
write-the-regex-algebra-once + delete the duplicate grammar-tier rules.

**RETIRED (earned-conditional, pass-2), STRENGTHENED this pass.** PROTO-2 (fire-census) moved the
fork-independence from *estimated* to *measured*: on projected branches the three regex-specific rules
apply **exactly 0** work (`proj total_applied=0`, one iteration) — the ~215-LOC de-dup the functor
claimed dies **FREE under Fork-B**, attributable to no family. The functor as an architecture with
independent value is dead; its soundness win (the projection-dropping-union bug) was already a non-bug
under the whole-leaf wrapper (pass-2, C2 not met). research-B's "total reverse functor" (`Hir→String`
structural unparse fixing `canonical_pattern`'s partiality) is real engineering but serves the string
BRIDGE (the survivor's `simplify_hir` round-trip oracle), **correctly firewalled away from 5B so it does
not launder a retired family's disposition** (both critics credit this). **Note:** PROTO-2-P2-CLOSED's
`escape_body`/brace-trim host re-sync is precisely the reverse-fold direction — the bridge round-trip
oracle should absorb it (CRIT-2 5B gap 2).

**Sole re-open trigger (LOCK-1-gated, cross-lock).** The retire rests on `->` staying a whole-leaf
wrapper (`Project` skinny / `Map` top-level) that never binds a **sub-leaf** capture. LOCK-1 owns the
real `->` typing and has NOT ratified it. If LOCK-1 ratifies a sub-leaf-capture representation (5D's
capture-leaf ambition, `(/[0-9]+/->Digits)` reaching inside the class), the projection-dropping-union
bug reappears and un-retires 5B **AND** breaks 5D's `Project` byte-identity oracle — both hang on the
same unratified fact. Routed to LOCK-1 with the PROTO-2 evidence. The joint-coproduct variant stays
un-opened (dies undesigned; acceptable — the functor's core value is a proven non-bug).

**BLOCKED assessment: N/A (RETIRED).** Convergence **40%** (both critics concur) — disposition-certainty
of a conditional retire, raised in confidence by the measured fork-independence, not a shipping number.

**Pass-4 directives.** Nothing for the functor (retired). One residual consumer-assignment gap: the
total `Hir→String` unparse (bridge round-trip oracle) is currently nobody's gate — home it explicitly in
the survivor's bridge-hardening (5A-residual), and confirm it absorbs P2-CLOSED's reverse-fold
(`escape_body`). *Withhold:* the coproduct — leave un-opened unless LOCK-1 reopens the
projection-soundness question.

---

## FAMILY 5C — AUTOMATA-AS-EXTRACTION — **RETIRED — 52%** (centerpiece RUN-EARNED RETIRED; survivor re-homed & advancing; internal 2B-as-minimizer BLOCKED; −3 from the stale 55 for the laundered LOCK-2 half)

Push the shared e-graph one level below the HIR, to the automaton. **This distinguishing mechanism —
e-graph-over-automata as the thing that beats the classical pipeline — is RUN-EARNED RETIRED
(PROTO-2-5C, Brief 2).** RETIRED per the legend at a *high* disposition-certainty because a concrete
non-e-graph survivor ships and the negative result is run-earned + prior-art-predicted.

**The run (PROTO-2-5C, `RAN_MEASURED — FAIL`, HEAD `ef10d5b78`, parse-that READ-ONLY).** Corpus 26→56
(12 acyclic / 44 cyclic, runtime back-edge DFS); oracle **0/56 divergences** (all four minimizers +
incumbent byte-identical, ρ=1). The `DfaSig` congruence rebuild is **O(n²·k)** on the target acyclic
class (`[01]{8}` n=9 → **8 passes-to-fixpoint**; passes track DFA diameter, not log n — `log n` held only
for shallow bushy alternations, refuted for every bounded chain); it is the **slowest correct lane** on
acyclic (cong 37664 > null 24376 > revuz 17168). `incumbent ≈ null` (<1% both tags): the `:358`
full-restart strawman was never slow at regex scale — no cost-model foe existed. Prior art TRAP A
predicted the loss (eqsat→automata runs the isomorphism backwards; DAG-extraction NP-hard); the run
confirmed it. **Brief 2 is DISCHARGED — pass-4 must NOT re-budget it.**

**The three sub-verdicts.**
- **2A minimization-as-congruence — RETIRED as centerpiece** (O(n²·k) on its own target class). What
  *survives*: a single **reverse-topological Revuz pass** — the only lane whose passes stay at **1** as n
  grows, a genuinely fastest-on-acyclic **structural/table** optimization needing **no e-graph** (1.42×
  over bug-fixed Moore; modest, growing with n).
- **2B determinization-as-derivative-saturation — BLOCKED (as minimizer), Kozen** — regular-language
  star-equivalence needs the star-induction rule (least/greatest-fixpoint mismatch; `(ab)*` loop states
  bisimilar but never bottom-up-congruent), so no finite ground rewrite is complete — the lock's cleanest
  impossibility. 2B-as-*determinizer* survives (25/26 ρ=1); 2A/Hopcroft closes.
- **2C prefilter/tier-selection — SURVIVES as a LOCAL TABLE, not a CSP** — research-C's zero-witness grep
  (no `lut_budget`/`codegen_budget`/`shared_lut`/`table_budget` in `crates/ir/src/`) → no shared resource
  → tier is local → monotone `EngineSet` table wins. The family's shipping survivor.

**SURVIVOR (re-homed, ADVANCING, needs no e-graph): 2C-as-table + delete-`:358` (mandatory, latent
~O(n³) bug, correctness-neutral, 0 divergences) + single-pass Revuz.** Integration is UNBUILT:
minimize-swap (`dfa.rs:89-91`), Valmari/Revuz native partial-DFA `DEAD` handling (`:330/:340/:423`),
tier-table home (`info/mod.rs:344`).

**Exact open gaps (the survivor's, not the centerpiece's).**
1. **The byte-rarity oracle — a NAMED MISSING INPUT** for 2C-as-table's accel-byte quality
   (regex-automata's static ASCII-rank table, absent in-tree; without it accel-byte selection "is
   guesswork," research-C). This — not Brief 2 — is the real pass-4 5C uncertainty; both critics name it.
2. **The de-confounded Valmari control is UNBUILT.** PROTO-2-5C's Valmari lane is allocation-confounded
   (explicitly not load-bearing); the FAIL rests soundly on the allocation-independent passes-count +
   `incumbent≈null`, but the Revuz win's SIZE is not cleanly measured. **The `is_acyclic`-dispatch
   net-positivity is MARGINAL against the FAIR control: Σ(null−revuz) = 7208 ns > 6786 ns dispatch =
   +422 ns net** (NOT the run's confounded +62715 ns headline, which is measured against the disclaimed
   Valmari lane — ~150× inflated; the synthesis correctly avoided that headline, credited). So
   Revuz-with-dispatch may collapse to research-C's conditional: "just replace restart-Moore with correct
   Valmari-Hopcroft, **no dispatch**." Needs a de-confounded CSR control.
3. **The CSP-over-tier escalation WITNESS was never constructed** — the synthetic budget-cap crossover
   research-C promised (a K-leaf grammar where local argmax overflows the generated-size budget and the
   CSP demotes a leaf to fit) did NOT run. "Table wins, CSP consumer-less" is proven for *today's*
   codegen, not shown robust against the one condition that would flip it. **This is why the 5C→LOCK-2
   binding SPLITS — see cross-lock #2.**
4. **Survivor integration UNBUILT** (above).

**BLOCKED assessment: FAMILY RETIRED (centerpiece); internal 2B-as-minimizer BLOCKED (Kozen).**
Convergence **52%** — CRIT-2's floor taken (−3 from the stale 55). **Named reason CRIT-1's 55 was not
taken:** the 55 folds in the 5C→LOCK-2 "binding, not proposal" routing on BOTH halves; CRIT-2 correctly
shows half (b) (tier-NOT-a-CSP) is grep-for-today, not run-for-always, and the flipping witness never ran
— a laundered binding legitimately lowers the family's cross-lock convergence. 52% is
convergence-of-disposition, high because the decisive control RAN and a concrete survivor ships — NOT a
shipping number (integration unbuilt).

**Pass-4 directives.** *Ship* the survivor (2C-table + delete-`:358` + single-pass Revuz). *Land* the
byte-rarity oracle. *Build* the de-confounded Valmari CSR control (size the Revuz win; decide dispatch vs
no-dispatch). *Construct* the budget-cap CSP witness BEFORE any retire of 2C's tier-CSP variable. *Do
NOT* re-run the centerpiece (Brief 2 discharged). *Withhold:* any "e-graph automata could still win at
large-n" re-opening — the passes measure is scale-free.

---

## FAMILY 5D — GRAMMAR-FRAGMENT — **ADVANCED — 55%** (the survivor's generator-path lowering policy; Brief 1 RUN-EARNED PASSED; substrate LANDED & compiles; selector wiring + wired-fraction still open)

A `/pat/` lowers DIRECTLY into native grammar IR — a straight-line class-scan
(`Recognizer::ByteClass` membership over the tape, no automaton) when the leaf is FOLLOW-sound; a
`Seq[Repeat, FOLLOW-guard]` or the DFA fallback for genuine give-back. The Glushkov/Berry-Sethi
position automaton done as an IR lowering. Under XL-C, 5D is **the generator-path lowering policy of the
two-types survivor.**

**Brief 1 RAN and PASSED every clause — the distinguishing thesis is RUN-EARNED on a SOUND source.**
- **proto-1-overapprox (HEAD `ce51c36fc`):** over-approx first(C) = the DFA start-state class set
  (measured off the parse-that anchored `Dfa` state-0 row) recovers the dropped alternation branches
  (`sheet_prefix` 1→54 bytes; `\$?` leading-optionals recovered) and *tightens* opaque `sheets::number`
  256→11; genuine-give-back holds at **3/16 = {word, sheets::string, sheets::identifier}, MACHINE-ASSERTED
  in-test** (`flips == ["sheet_prefix"]`, genuine trio 3/3 non-disjoint); the single disjointness flip is
  a `!`-delimiter-terminated conservative false-positive in BOTH branches → **0 GENUINE flips**, no leaf
  goes genuinely sound→unsound. Cold-pair (N=240×256, paired, black_box-sunk, correctness asserted at
  setup): tape **byte-identical 256/256** on both leaves; direct scan **3.25×** faster than `Dfa::find`
  on rare `[0-9a-fA-F]{6}` and **2.81–3.00×** on dense `[a-z]+`.
- **PROTO-1-5D (HEAD `ce51c36fc`, FRESH COMPILE) — the substrate LANDED this pass:** `ir::SimdMode::ByteClass`
  / `ir::Recognizer::ByteClass{set,follow,site}` / `ir::BackendExpr::ByteClass` + `byteset_from_ranges`/
  `byteset_contains` land in-tree, route through **both** emitters (`tape_plan.rs` `scan_byteclass_guarded`
  + `general_descent/emit.rs` `match_byteclass_guarded`), and **fresh-compile green** (`cargo build -p ir
  -p passes -p codegen`, genuine `Compiling …`, EXIT 0). The **over-consumption bug is now PROVEN LIVE:**
  current masked-wildcard `match_leaf` is WRONG on **12/20 delimiter-free inputs** (`aandb`: over-consumes
  span `[0,5)` and REJECTS a valid sentence); `ByteClass`+FOLLOW-guard parses correctly with tape
  **byte-identical to `([a-z]-"and")+` on 20/20** — the FOLLOW-guard is a **correctness fix, not a perf
  nicety.**

⇒ **the DFA leaves the generator path; direct-scan-default RATIFIED, DFA→narrow fallback for the genuine
trio.** The FOLLOW predicate is a genuine Changed-flag fixed point that DEFEATS the unsound
`check_one_pass_eligible` (`one_pass.rs:64`). This gates LOCK-9-9C and LOCK-4-4D.

**Exact open gaps (integration + the decisive number, not thesis).**
1. **`derive_recognizers` is NOT rewired onto the FOLLOW fixed point — the selector is the ONE
   designed-not-wired seam.** The substrate + both emitters exist to *receive* the verdict; the producer
   that decides which leaves get `ByteClass` vs the masked `RegexProgram` is unbuilt. The closure exists
   (`proto-1-overapprox/follow_sound_fraction.rs`), but `derive_recognizers` (`passes/src/lib.rs:338`) was
   NOT rewired under the cap. Until it lands, no REAL grammar leaf is routed through the new path — the
   PROTO-1-5D test drives verbatim-port primitives, not the emitted `generated.rs` (end-to-end
   unexercised). This is the elegant-reduction trap's residue: "direct-scan-default … *and then* wire the
   FOLLOW-fixed-point selector." (Both critics: PROTO-1-5D's "LOCK-9-9C unblocked" over-reaches the more
   honest "de-risked, still UNDELIVERED" — take the honest wording.)
2. **The WIRED-fraction Q7 re-run over real JSON/Sheets/CSS is UN-RUN — "the single most load-bearing open
   number" (research-D §10).** The synthetic 16-leaf sample gives a NON-EMPTY residue (7/16) that
   *reverses* the pass-2 "DFA has no codegen consumer" and would ADMIT the DFA to codegen; but if the
   WIRED fraction is ~100% FOLLOW-disjoint, the DFA has no generator-path consumer and Y.13 FORBIDS wiring
   it. This binary — DFA-in-codegen vs standalone-API-only — is undecided until the wired fraction runs.
3. **The residue-tier cold-pair** (`match_leaf` vs `Dfa::find` on the 7/16 give-back leaves) is UN-RUN;
   proto-1-overapprox timed `hex6[rare]`/`lower[dense]`, not the actual give-back residue (Q8.3).
4. **Corpus breadth** — 16 leaves, approximate sheets FOLLOW-wiring (39-rule subset, host-heavy rules
   approximate). Affects FOLLOW breadth only, never the counterexample gate or the genuine trio.
5. **Capture-leaf → `Project`** byte-identity — UNBUILT, LOCK-1-gated (same unratified sub-leaf-capture
   fact as the 5B re-open; judge against RE2C TDFA(1), not re-invented).

**BLOCKED assessment: NOT BLOCKED.** Convergence **55%** — CRIT-2's floor taken. **Named reason CRIT-1's
58 was not taken:** CRIT-1's +3 credits the substrate-build + proven bug; CRIT-2 correctly caps at 55
because the two gates that actually DECIDE "does the DFA leave the generator path" (the selector wiring +
the wired fraction) are both unbuilt — the substrate landing is genuine progress but the decisive number
remains open. The family's distinguishing thesis is RUN-EARNED; what remains is integration + the wired
fraction, not architecture.

**Pass-4 directives.** *Wire* `derive_recognizers` onto the FOLLOW fixed point (the closure exists; the
tape-obligation is empirically discharged — this unblocks LOCK-9-9C). *Run* the WIRED-fraction Q7 over
real JSON/Sheets/CSS (the decisive DFA-in-codegen-vs-API number). *Run* the residue-tier cold-pair.
*Grow* the corpus beyond 16 leaves with exact sheets FOLLOW-wiring. *Withhold:* nothing to re-litigate on
the verdict — hand researchers the RATIFIED direct-scan-default policy to WIRE, plus the capture-leaf
oracle pending LOCK-1.

---

## FAMILY P2 — GRAMMAR-DRIVEN (the TITULAR deliverable) — **ADVANCED — 60%** (NEWLY OPENED & PARITY-DEMONSTRATED; the formulation every prior pass named "un-attempted" is now the pass's strongest artifact)

Not one of the four charter families — the lock's namesake (Constitution i), tracked here because the
prior synthesis's FALSE "0 artifacts / un-attempted" floor would otherwise propagate UP the DAG to
GESTALT-P. This is the redirect-crowding "underexplored formulation to OPEN" every prior pass named; it
is now OPENED and PARITY-DEMONSTRATED.

**State of evidence (verified from source + three run logs this pass).**
- The grammar-driven path is **wired end-to-end today**: `regex::parse_generated` (`mod.rs:34
  -> Result<Hir, ParseError>`) — generated parser → typed `RegexParserEnum` AST → 525-LOC `host::lower`
  fold → bespoke `Hir`. The AST-discarding `generated.rs:2288 -> bool` is a diagnostic SHIM; the discard
  is **already broken at the outer layer**. "Break-the-discard" was never a change to author.
- **PROTO-2-P2 → measured-feasible (43/45).** **PROTO-2-P2-CLOSED → RAW 56/56 & BRIDGE 56/56
  byte-DFA-identical to the hand parser** (renumbering-invariant canonical minimal-DFA oracle, strictly
  stronger than match-equivalence; raw AND through the XL-C `simplify_hir` bridge), **0 rejects / 0
  divergences, strict full-parity gate GREEN**, closed by a **5-edit host-only re-sync (`host.rs +50/−6`,
  no grammar change, no regen)**. parse-that stayed byte-identical (`diff … IDENTICAL` pre-edit).
- **De-dup surface CLOSED: 1 promote (`octal`) / 21 delete (17 dominated + 4 non-regular out-of-scope:
  lookahead/lookbehind/back_reference/anchor) / 3 relocate (flags/end/regex literal-delimiters) ⇒ retire
  `grammar/misc/regex.bbnf`.** The bootstrap grammar (13 rules / 72 LOC, engine-wired) is the strict
  superset for the regular fragment.

**Exact open gaps (why 60%, not higher).**
1. **The architectural CHOICE is open, GESTALT-P's call** — host-fold re-sync (verified, smaller surface)
   vs grammar-carries-typed-captures + regen (cleaner, unbuilt). "Either closes it." This is the LOCK-7A
   seam.
2. **It is a differential-oracle result (LOCK-7D shape), NOT the LOCK-7A bootstrap.** It proves the
   grammar-driven parser DFA-matches the *hand* parser 56/56; it does NOT exercise the committed-stage-0 +
   fixpoint-regen self-hosting clause i / LOCK-7A names — the self-referential circularity (`regex.bbnf`
   embeds regex leaves to parse itself, priorart §1) is UNTOUCHED. "Promote to production parser" remains
   LOCK-7A-gated.
3. **Two constructs closed by CONSTRUCTION, not by the DFA oracle** — bare `^`/`$` anchors and `\p{}` are
   absent from the 56-corpus, their arms verified by reasoning. **These are exactly the construct 5A's
   `/foo$/` Look-defect (G-5A.look) predicts is mishandled — the un-oracled P2 arm and the unbuilt 5A
   anchor test are the same untested surface.** Bare dot and `\x` WERE in-corpus and ARE oracle-verified.
4. `lower_class_atom_node_byte` (in-class `\x` range endpoints) left reading `bytes[0]`; no corpus range
   exercises it (one-line GESTALT-P follow-on).
5. Ran in a READ-ONLY parse-that scratch (`bootstrap/regex.bbnf` + `host.rs`) — nothing landed in the tree
   of record.

**BLOCKED assessment: NOT BLOCKED.** Convergence **60%** (CRIT-2's explicit figure; CRIT-1's prose
disposition "PARITY-DEMONSTRATED-in-scratch, advanced NOT discharged, LOCK-7A-gated" concurs — no named
reason to lower below the sole explicit critic figure). A converged CORE with an exact gap — the OPPOSITE
of the synthesis's "un-attempted."

**Pass-4 directives.** *Do NOT re-budget* Briefs 1 & 2 — both SPENT (executed + closed). *Surface* to
GESTALT-P the ONE architectural choice (host-fold vs grammar-captures+regen). *Build* the bare-anchor /
`\p{}` oracle corpus (joint with 5A's `/foo$/` test — same surface). *Convey to LOCK-7A* the QUANTIFIED
de-dup (1/21/3 ⇒ retire misc) + the PARITY-DEMONSTRATED artifact + the untouched self-hosting bootstrap
circularity — NOT "zero artifacts / discards its AST." *Withhold:* the FALSE "un-attempted" floor entirely.

---

## REDIRECT-CROWDING DECISIONS

- **Axis-1 stays CLOSED (pass-2); the survivor's decisive gates AND the titular thread's first artifacts
  all landed this pass.** 5A-identity refuted, 5B-functor retired (both pass-2). Pass-3 closed 5D's
  generator-path lowering (Brief 1 PASSED + substrate landed), 5C's automata-internals disposition (Brief
  2 FAILED → survivor is 2C-table + `:358` + Revuz), AND opened the titular P2 thread to
  PARITY-DEMONSTRATED. No crowding among the surviving pieces — **5A-residual (bridge/shadow/typing), 5D
  (lowering policy), 5C-survivor (tier table + bug fix), P2 (grammar-driven parser) are COMPLEMENTARY,
  not redundant.**
- **The underexplored formulation to OPEN — the GRAMMAR-DRIVEN BOOTSTRAP path — is now OPENED (family
  P2).** The thing every prior pass named as the ONE un-attempted formulation is now the pass's strongest
  artifact (56/56 parity). What remains genuinely un-attempted is NARROWER: the self-hosting bootstrap
  circularity (committed-stage-0 + fixpoint-regen), LOCK-7A-gated. Do NOT re-issue "grammar-driven is
  un-attempted"; issue "self-hosting bootstrap is LOCK-7A-gated."
- **The 250-LOC de-dup stays BANKED as a standalone refactor residual** (neither 5A nor 5B) — free under
  Fork-B, a bare string-bridge reuse of `simplify_hir` under Fork-A. Crediting it to any live family
  launders a retired family's win.
- **5A is parked but NOT redundant-with-5D** (complementary hardening; its `/foo$/` Look gate is now a
  JOINT 5A/5D/P2 anchor consumer) — so it is not banked-as-crowded; it carries a **re-label directive**
  (family → refactor-residual) if it stays parked a 4th pass.

---

## CROSS-POLLINATION (named grafts only, both sides developed)

- **GRAFT-1 (WITHDRAWN for the minimizer; dormant for the determinizer).** The 2B/2A **minimizer** graft
  is RETIRED with the centerpiece (PROTO-2-5C). Survives only for 2B-as-**determinizer** (25/26 ρ=1,
  low priority). Do not carry it forward as an active minimizer graft.
- **GRAFT-2 (mechanical, holds): the byte-class partition (`sets/equiv.rs`, num_classes≤8) consumes the
  engine's `Class` byte-sets** — feeding Revuz + the 2C table (not the retired congruence minimizer). A
  consequence of riding the survivor's engine `Hir`.
- **GRAFT-3 (BORN-RED, the survivor's prefilter/lowering SEAM — flag for GESTALT): 5D's
  direct-scan-vs-DFA FOLLOW-guard ⊗ 5C-2C's tier table.** With PROTO-1-5D landing a `ByteClass` chooser
  AND 2C owning a per-pattern `EngineSet`, the "class-scan vs DFA" decision genuinely exists in TWO homes
  from two homes — 5D on the generator path (FOLLOW-sound → direct scan), 2C at the engine tier. They must
  be **ONE dispatch surface, not two divergent decisions** (the double-emit trap, priorart §4.10). Both
  sides are developed enough to name the graft (5D's verdict RATIFIED, 2C-as-table settled). **Coherence
  requirement, no family yet owns it — pass-4 substrate build must wire them as a single decision surface**
  (feeding the PROVE-10 prefilter seam). Both critics flag it born-RED.

---

## CROSS-LOCK DOUBLE-HOMES (constitution ix — LOCK-5 is the hub; resolutions are LOCK-5 proposals
GESTALT RATIFIES, except where a RUN makes them BINDING inputs)

1. **Axis-1 verdict ⊗ LOCK-4-4D / LOCK-9-9C / LOCK-3-3C — RE-CUT, and it is a CONSUMER-LESS adjudication
   (must become an OWNED work item, not a floating note — CRIT-2 XL5-2).** XL-C ratified two-types+bridge;
   4D's premise ("one HIR, not a second engine") is weakened; 4D/9C/3C consume the two-types landing or
   converge on a refuted verdict. This adjudication re-cuts THREE downstream families but is "still
   nobody's charter" — a Y.13-adjacent defect at the cross-lock layer. **GESTALT-P/Ω owes an explicit
   named work item: "re-cut 4D/9C/3C against two-types+bridge."**
2. **5C REFUTATION ⊗ LOCK-2 — SPLIT (half BINDING, half PROPOSAL — CRIT-2 XL5-1, the sharpest cross-lock
   ding, ADOPTED).** LOCK-2's charter offers `2B-IS-EGRAPH` and `2C-CSP-CENTRAL`. **Half (a) — automata
   minimization is NOT an e-graph island — is RUN-EARNED BINDING** (PROTO-2-5C measured O(n²·k) on the
   acyclic target + `incumbent≈null`; legitimately refutes LOCK-2's automata-island candidate). **Half (b)
   — tier-selection is NOT a CSP — is a STRONG PROPOSAL, not binding:** it rests on research-C's
   zero-witness grep of *today's* `crates/ir/` plus reasoning; the synthetic budget-cap crossover that
   would exhibit the flip NEVER ran, and LOCK-2's 2C charter explicitly names "prefilter tier" as a
   per-site CSP variable. Routing both as "binding, not a proposal" would launder a grep into a run.
   **GESTALT/LOCK-2 must construct the budget-binding witness before retiring 2C's tier-CSP variable.**
   Single-home the tier table in 5C; escalate to LOCK-2's CSP only on an exhibited shared-LUT/Teddy budget
   witness (none exists).
3. **5B retirement ⊗ LOCK-1 (RE-OPEN TRIGGER).** The retire rests on `->` staying a whole-leaf wrapper
   that never binds sub-leaf captures. Sub-leaf-capture representation un-retires 5B AND breaks 5D's
   `Project` byte-identity oracle — both hang on the same unratified LOCK-1 fact. Routed with PROTO-2
   evidence. (Sound per both critics.)
4. **5A/5D ⊗ LOCK-9-9C (SCANNER-AS-DFA) — dependency HALF-CURED (producer NODES now BUILT; SELECTION gate
   unwired).** LOCK-5 owns the `Hir`→DFA→classify substrate + the `ByteClass` chooser; 9C consumes it or
   retires. **PROTO-1-5D BUILT the `ByteClass` producer nodes and fresh-compiled them green — the masked
   wildcard is being replaced with a positive-class gate** (negated bodies stay masked, disclosed). The
   producer's one correctness obligation (direct-scan tape ≡ DFA tape, 256/256 + 20/20 byte-identical) is
   EMPIRICALLY DISCHARGED. **Only the SELECTION gate (`derive_recognizers` FOLLOW-rewire) is unwired** —
   cross-lock #4's old "producer is UNBUILT (masked wildcard)" is now half-false. 9C's dependency is
   de-risked; pass-4 5D directive #1 (wire the selector) delivers it.
5. **P2 ⊗ LOCK-7 bootstrap — RE-CUT from "zero artifacts" to PARITY-DEMONSTRATED + QUANTIFIED de-dup.**
   The prior "two divergent `regex.bbnf` (one discards its AST) … zero artifacts three passes" is FALSE.
   Convey to LOCK-7A: (a) the grammar-driven parser is PARITY-DEMONSTRATED (56/56 raw+bridge, host-fold
   re-sync); (b) de-dup surface CLOSED (1 promote / 21 delete / 3 relocate ⇒ retire
   `grammar/misc/regex.bbnf`); (c) ONE open architectural choice (host-fold vs grammar-captures+regen);
   (d) the self-hosting bootstrap circularity is UNTOUCHED and LOCK-7A-gated. NOT "un-attempted."
6. **Type-plane ⊗ LOCK-1.** `Class ⊢ Span` / `Look ⊢ Unit` + width-var graded only against skinny's stub
   `BuiltinTy`. Route through LOCK-1's real HM + row-poly + widths.
7. **rust/ts ⊗ LOCK-6 (serde-ownership, RECURRING — correctly carried).** The mirror/skeleton carries BOTH
   types (`Regex(StringId)` grammar-side + rich `Hir`/`Class`/`Look`/`ByteClass` engine-side). The
   `CharClass: Serialize` ownership question (who lands it, on the parse-that READ-ONLY / LOCK-6 boundary)
   persists — PROTO-2-P2, PROTO-2-P2-CLOSED, and PROTO-2-5C all had to run in a READ-ONLY rsync scratch
   with repointed patch prefixes (parse-that byte-identical, sole change scratch `host.rs`). Named gap for
   GESTALT-Ω.

---

## PASS-4 DIRECTIVES (consolidated, ordered by what gates the most downstream)

1. **Record the THIRD stale-gestalt recurrence as the pass's central process finding** (institutional
   guard): reconcile the registry to EVERY prototype in-folder before cutting — the 22:40 synthesis
   missed the EVENING grammar-driven runs (PROTO-2-P2 20:03, PROTO-1-5D 22:51, PROTO-2-P2-CLOSED 22:59)
   exactly as the 15:27 synthesis missed PROTO-2-5C. This re-cut integrates all of them.
2. **P2 (TITULAR): convey PARITY-DEMONSTRATED, not "un-attempted."** Briefs 1 & 2 SPENT — do NOT
   re-budget. Genuinely-open P2 work = the host-fold-vs-grammar-captures architectural choice (GESTALT-P),
   the LOCK-7A promotion + self-hosting bootstrap circularity (untouched), the bare-anchor/`\p{}` oracle
   corpus (joint with 5A's `/foo$/`).
3. **5D: WIRE `derive_recognizers` onto the FOLLOW fixed point** (the ONE designed-not-wired seam;
   substrate + both emitters land + compile green; tape-obligation discharged — unblocks LOCK-9-9C). **RUN
   the WIRED-fraction Q7** over real JSON/Sheets/CSS (the single most load-bearing open number:
   DFA-in-codegen vs standalone-API). Run the residue-tier cold-pair; grow the corpus with exact sheets
   FOLLOW-wiring.
4. **5C: SHIP the survivor** (2C-table + delete-`:358` + single-pass Revuz, no e-graph); land the
   byte-rarity oracle; build the de-confounded Valmari CSR control (Revuz-with-dispatch vs no-dispatch, the
   +422 ns margin); construct the budget-cap CSP witness. Do NOT re-run the centerpiece.
5. **Route the 5C→LOCK-2 SPLIT** (not a monolithic binding): half (a) automata-min-NOT-an-island BINDING;
   half (b) tier-NOT-a-CSP STRONG PROPOSAL until the budget-cap witness runs. Single-home the tier table
   in 5C.
6. **5A: BUILD the `/foo$/` Look-consumer test** (the JOINT 5A/5D/P2 anchor consumer, buildable-but-unbuilt
   three passes); MEASURE the shadow-deletion net ΔLOC after paying the ~787-LOC parser import (sign
   unknown); route Class/Look typing through LOCK-1's HM. RE-LABEL 5A as a refactor residual if parked a
   4th pass.
7. **GRAFT-3: wire 5D's FOLLOW-guard direct-scan and 2C's engine tier as ONE dispatch surface** (born-RED,
   no owner — the double-emit trap).
8. **RATIFY (not declare) the cross-lock resolutions** with the counterpart locks at GESTALT: the
   4D/9C/3C two-types re-cut (make it an OWNED work item); the LOCK-1 `->` sub-leaf-capture re-open
   trigger; the LOCK-2 tier single-home + the SPLIT binding; the LOCK-6 serde-ownership; the LOCK-7A
   PARITY-DEMONSTRATED + de-dup conveyance.

---

## PER-PASS HISTORY

### Pass 3 — FINAL re-cut (integrates the EVENING grammar-driven prototypes + the FINAL critiques; supersedes the 16:32 afternoon cut)

Corpus: research-{A,B,C,D,priorart} → FAMILY-SPECS (15:27 → re-cut 22:40) → PROTO-1[5D under-approx],
PROTO-2[5B/fork census], PROTO-2-5C[5C Brief 2], proto-1-overapprox[5D Brief 1], **PROTO-2-P2[titular
first artifact], PROTO-1-5D[5D substrate + bug], PROTO-2-P2-CLOSED[titular gap closed]** → CRIT-1 (23:31),
CRIT-2 (23:37) → this.

| Family | Status | % | One-line |
|---|---|---|---|
| 5A ONE-HIR | ADVANCED (refactor residual) | 42 | No 5A proto three passes; classify-relocates honestly conceded; net-shrink SIGN unknown (contingent on ~787-LOC parser import, an edge P2-CLOSED's de-dup does NOT price); `/foo$/` Look gate now a JOINT 5A/5D/P2 anchor consumer, buildable-but-unbuilt a THIRD pass; re-label candidate; both critics 42% |
| 5B LAWFUL-FUNCTOR | **RETIRED (earned-conditional)** | 40 | PROTO-2 measured fork-independence (proj `total_applied=0`) — retire STRENGTHENED; conditional only on LOCK-1 keeping whole-leaf `->`; laundering firewall held (reverse `Hir→String` unparse homed in the bridge; P2-CLOSED's `escape_body` is that reverse-fold — absorb it there) |
| 5C AUTOMATA-AS-EXTRACTION | **RETIRED (centerpiece; survivor advancing)** | 52 | Centerpiece RUN-EARNED RETIRED (cong O(n²·k), `[01]{8}` n=9→8 passes, incumbent≈null, prior-art-predicted); survivor = 2C-table + delete-`:358` + no-egraph Revuz; 2B-as-minimizer BLOCKED (Kozen); CRIT-2 floor (−3 from 55): the 5C→LOCK-2 tier-NOT-a-CSP half is grep-for-today not run-for-always (witness never ran) → SPLIT the binding; acyclic dispatch marginal (+422 ns fair) |
| 5D GRAMMAR-FRAGMENT | **ADVANCED** | 55 | Brief 1 RAN & PASSED (over-approx sound first(C), 3/16 machine-asserted, 0 GENUINE flips, cold-pair 256/256, 3.25×/2.81-3.00×) → direct-scan-default RATIFIED; PROTO-1-5D landed the ByteClass substrate (fresh-compile EXIT 0) + PROVED the over-consumption bug (aandb 12/20 wrong); CRIT-2 floor 55 (not CRIT-1's 58): the selector wiring + the WIRED fraction that DECIDE "DFA off the generator path" are both unbuilt |
| P2 GRAMMAR-DRIVEN (titular) | **ADVANCED (NEWLY OPENED)** | 60 | The FALSE "0 artifacts / un-attempted" floor CORRECTED: `parse_generated -> Result<Hir>` at `mod.rs:34` (discard already broken); PROTO-2-P2 43/45 → PROTO-2-P2-CLOSED **56/56 raw+bridge byte-DFA-identical, strict gate GREEN**, 5-edit host-only re-sync; de-dup CLOSED (1/21/3 ⇒ retire misc); open = ONE architectural choice + LOCK-7A promotion/self-hosting-bootstrap + bare-anchor/`\p{}` oracle; both critics HIT the synthesis as stale against this |

**Prototypes run this pass (SEVEN — the three decisive EVENING runs landed AFTER the 22:40 synthesis that
judged them).** PROTO-1 (5D under-approx, `ce51c36fc`): 3/16 partition, self-flagged unsound source.
PROTO-2 (5B/fork census, `ce51c36fc`): projected `total_applied=0` → Fork-B free. PROTO-2-5C (5C Brief 2,
`ef10d5b78`, READ-ONLY): FAIL — centerpiece RETIRES. proto-1-overapprox (5D Brief 1, `ce51c36fc`): PASS —
over-approx sound + cold-pair; direct-scan-default RATIFIED. **PROTO-2-P2 (titular, `ef10d5b78`,
READ-ONLY): FIRST ARTIFACT 43/45. PROTO-1-5D (5D substrate, `ce51c36fc`): substrate lands + fresh-compiles
+ over-consumption bug PROVEN. PROTO-2-P2-CLOSED (titular, `ef10d5b78`, READ-ONLY): 56/56 raw+bridge full
parity, gap CLOSED.** The registry is re-cut from the prototypes, not the stale synthesis or the (partly
stale) critiques.

### Pass 3 — afternoon cut (16:32, SUPERSEDED by the FINAL re-cut above)

Integrated the afternoon prototypes (PROTO-2-5C, proto-1-overapprox) and correctly re-cut 5C→55 / 5D→55,
but PREDATED the evening grammar-driven runs and the final critiques; its titular-floor language ("ZERO
artifacts / un-attempted") was FALSE against runs that had not yet landed. Retained here as the record of
the mid-pass state that the final re-cut corrected.

### Pass 2 (research-{A,B,C,D,priorart} → FAMILY-SPECS → PROTO-2[5A brief], PROTO-1[5B brief] → CRIT-1, CRIT-2 → registry)

| Family | Status | % | One-line |
|---|---|---|---|
| 5A ONE-HIR | ADVANCED (maximal REFUTED) | 45 | PROTO-2 RAN: clean-merge refuted — REGEX-LEAK ≈29 ≫ 0, cache Hir-keyed not Id-view, serde break (artifact-removable), 3 mirrored enums; narrowed Class/Look survivor feeds the Axis-1 winner |
| 5B LAWFUL-FUNCTOR | **RETIRED (earned-conditional)** | 40 | PROTO-1 RAN 5B's own gate: copy dies 250 LOC + 59/59 DFA-equiv, but C2 (projection-drop soundness bug) NOT MET — a non-bug under the Map-wrapper → dies to a bare merge; conditional on LOCK-1 keeping Map-barrier |
| 5C AUTOMATA-AS-EXTRACTION | ADVANCED (2B-as-minimizer BLOCKED) | 45 | NO 5C prototype ran; reference-Hopcroft control + delete-`:358` null + grown corpus + 2C all unrun; centerpiece effectively refuted, survivor a 15%-contingent strawman-measured sliver; 2B BLOCKED (Kozen) |
| 5D GRAMMAR-FRAGMENT | BANKED (survivor's lowering policy) | 35 | 3 in-tree facts source-CONFIRMED (unsound one-pass predicate, masked `ByteClass` wildcard, opaque `RegexProgram` emitter); diagnosis sharpened, resolution unbuilt; re-homed off the refuted 5A-maximal |

**Axis-1 resolved by the runs: two domain-fit types + cheap string bridge (owned by no family's charter),
rendered as the lock's verdict; later RATIFIED as XL-C.**

### Pass 1 (research-{A,B,C,D,priorart} → FAMILY-SPECS → PROTO-1, PROTO-2 → CRIT-1, CRIT-2 → registry)

| Family | Status | % | One-line |
|---|---|---|---|
| 5A ONE-HIR | ADVANCED | 55 | Tax+nullability RAN & favourable but pre-favouring tree; bootstrap, automata parity, `Look`-consumer, classify re-home, god-type all SPEC-ONLY |
| 5D GRAMMAR-FRAGMENT | BANKED (into 5A) | 35 | Maximal "no engine" reading RETIRED by PROVE-10; folded as 5A's lowering policy; one-pass fraction + throughput UNMEASURED; masked-wildcard contradiction |
| 5B LAWFUL-FUNCTOR | BANKED | 45 | Sound but converges with 5A + value-target (434-LOC copy) absent from the graduating tree; held behind the un-bank/retire gate |
| 5C AUTOMATA-AS-EXTRACTION | ADVANCED | 60 | 2B-as-minimizer BLOCKED (ρ_max=1.667); 2A EXACT but 3.53× slower, 15% acyclic fast path vs a strawman baseline; 2C unprototyped + double-homed |

**Trajectory.** 5A 55→45→42 (maximal RAN-refuted pass-2, then parked-refactor with unsigned shrink
pass-3). 5B 45→40→40 (BANKED→RETIRED pass-2, strengthened pass-3). 5C 60→45→52 (no run pass-2; Brief 2
RAN pass-3 → centerpiece RUN-EARNED RETIRED; final floor 52 after the LOCK-2 binding SPLIT). 5D 35→35→55
(diagnosis pass-2; Brief 1 PASSED + substrate landed pass-3; selector wiring + wired fraction cap it). P2
(titular) —→—→60 (NEWLY OPENED pass-3: the "un-attempted" formulation is now PARITY-DEMONSTRATED at
56/56 — the pass's dominant correction).
