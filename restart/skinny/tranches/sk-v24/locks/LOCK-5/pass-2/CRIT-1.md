# LOCK-5 — THE REGEX CONTINUUM — pass-2 CRIT-1 (adversarial, within-lock weight)

Critic 1 of 2. Weighting the **within-lock** view: internal soundness of each family spec, and
whether the prototypes actually tested the sharpest uncertainty. Read against PORTFOLIO
(CONSTITUTION + LOCK-5), FAMILY-SPECS.md, research-{A,B,C,D,priorart}, PROTO-2.md (5A brief),
proto-1/PROTO-1.md (5B brief), the pass-1 REGISTRY floors.

---

## THE HEADLINE — the FAMILY-SPECS convergence numbers are STALE; both briefs RAN to NEGATIVE

**Timeline (file mtimes, verified): research 09:34–09:37 → FAMILY-SPECS synthesis 09:45 →
PROTO-2 (5A Brief 2) 09:59 → PROTO-1 (5B Brief 1) 10:01.** The synthesis SET the two briefs and
labeled every convergence % *"un-earned until prototype."* The prototypes then EXECUTED those
exact briefs — **after** the synthesis was written — and both returned **negative** verdicts.
FAMILY-SPECS still presents them as open ("Exact open questions… un-run"). The convergence
numbers in FAMILY-SPECS (5A ~62%, 5B 45%, 5C 60%, 5D 35%) therefore do **not** integrate the
evidence that now exists. This is a **vacuous-convergence hazard at the document level**: the
synthesis reads as if the sharpest uncertainties are unresolved when its own budgeted prototypes
resolved them, both against the favored reading.

**The combined prototype verdict — which FAMILY-SPECS nowhere states.** Run in the ONE top-level
`crates/ir` tree (both base-pinned to `7cdbff4ef`, credit: this discharges P1's "measure in one
tree" demand cleanly, no wrong-tree fraud):

- **PROTO-2 → 5A's ONE-HIR clean-merge horn REFUTED.** Merging `Class`/`Look` into `IrNode`:
  `E0004×53` non-exhaustive + `E0277×4` serde breaks; **REGEX-LEAK ≈ 29 ≫ 0**; the named
  `SaturationCache` is `Hir`-keyed, not the Id-view. Both of 5A's own fail conditions fired.
- **PROTO-1 → 5B's LAWFUL-FUNCTOR RETIRED per its own gate.** Copy dies 250 LOC (434→184, exceeds
  the ~215 estimate — credit), round-trip 59/59 DFA-equiv — **but criterion 2 (projection bug
  fails-under-string-bridge) NOT MET**: the projection-dropping-union bug is a **non-bug** under
  the Map-wrapper representation (`projection_union_merge_is_already_safe_under_string_bridge`
  passes). The 250-LOC win reduces to a bare string-bridge merge → the family's own RETIRE
  trigger "dies trivially under a bare 5A merge."

**The load-bearing consequence FAMILY-SPECS refuses to draw.** Constitution (ii): *"LOCK-5
decides the lawful form [of HIR isomorphism]; every other lock consumes its verdict."*
FAMILY-SPECS instead concludes *"The whole Axis-1 verdict is un-earnable until GESTALT picks the
P1 fork"* — punting the lock's **defining decision** to GESTALT. But the two prototypes together
**already earn a verdict**: neither 5A's ONE-HIR identity-merge nor 5B's functor survives; what
survives is **domain-fit two types (grammar keeps `Regex(StringId)` opaque; the engine keeps rich
`Hir`) + a string-bridge 250-LOC de-dup that belongs to NEITHER family's architecture**. That is
a renderable Axis-1 verdict *within the lock*, and the deferral-to-GESTALT is an **elegant-
reduction / unverified-gestalt trap**: the hard part ("which type-plane architecture wins") is
declared out of scope precisely when the evidence to decide it landed 15 minutes after the
synthesis was written.

---

## FAMILY 5A — ONE-HIR — CRIT-1 convergence **42%** (down from 55% floor; the 62% provisional is REFUTED)

The core thesis — *"one authored type, two derived views; there is nothing to map; identity not
isomorphism"* — is **refuted on its two load-bearing axes** by the very brief the synthesis set.

**Internal-soundness failures found:**

1. **research-A's per-arm leak census is a CATEGORY ERROR.** research-A censused the *combinator
   arms* (`Seq/Alt/Repeat/…`), found them clean, and concluded "no combinator arm carries a
   regex-specific concern… the side-table facts be read off the structure by the shared analysis
   fabric." PROTO-2 censused the **passes that must dispatch on the new `Class`/`Look` leaves**
   and found **≈29 that need regex-specific arms** — `first_sets.rs` needs
   `cc.to_positive_byte_ranges()`, `regex_info.rs::collect_regex_ids` keys on a `StringId` a
   `Class` does not have, `types/generate.rs` must map the leaf to a `TypeDesc`. The
   "shared-fabric-absorbs-it" claim is **empirically false**. G-5A.1's "pass-2 read it 0" is not
   just un-earned, it is **contradicted (≈29)**.
2. **The "two derived views / cache keys on the Id-view" mechanism is TYPE-LEVEL FALSE
   (PROTO-2c).** The named `SaturationCache` is `FxHashMap<Hir, Hir>` — `Hir`-keyed. The collapse
   that PROTO-2 got to pass is the **egraph memo**, a *different* cache than the one 5A named. So
   5A's headline cache-consumer is **consumer-less as specified** — Y.13 fires on 5A's own
   mechanism, now confirmed by prototype (not merely the Q5 hypothetical the synthesis banked).
3. **`Serialize` breaks unpatchably.** `IrNode` derives `Serialize/Deserialize`; `bbnf_regex::
   CharClass`/`Look` carry no serde and parse-that is READ-ONLY. This is a **structural wall**,
   not a tuning gap — the derive cannot be made to hold from bbnf-lang.
4. **The "ONE authored type" parsimony claim is already false at HEAD.** PROTO-2 notes
   `IrNode → DagNode → GrammarENode` are **three hand-mirrored enums** today; the three "trivial"
   lowering arms force the merge to be mirrored into two further enums. 5A's constitution-(vi)
   parsimony boast ("not a god-type, one type") is contradicted by the existing three-enum mirror
   before any regex work.

**What genuinely survives (credited):** `egraph-derive`'s `Language` projection *is* total over
the merged leaves (`classify_field` sends non-`Id` payloads to `Leaf`); the egraph memo *does*
collapse span-distinct duplicates without forcing `Eq/Hash` onto the spanned type; G-5A.2
(`/foo$/` rejects `"foobar"`, `/foo/` accepts) is a genuinely **falsifiable** consumer gate — a
gate that *can* fail, correctly designed. And the sub-fragment `{Class,Look}` bbnf-growth is
still needed by whatever type-plane wins. So 5A is not zero — but its **maximal claim is dead**,
and the narrowed survivor ("grow `Class`/`Look` on the engine side, grammar leaf stays opaque")
**is the domain-fit outcome, not 5A**.

**Enumerated open gaps (5A):**
- **G-5A.1 REFUTED, exact count SOFT.** ≈29 leaks, but PROTO-2 graded only 6/53 arms by direct
  read; the rest "from pass purpose." The exact REGEX-LEAK count is unearned until the arms are
  *written*; the pass/fail (≫1) is robust, the magnitude is not.
- **Serde wall unresolved.** No path in-tree makes `IrNode: Serialize` hold with `CharClass`
  variants; either parse-that adds serde (crosses the READ-ONLY boundary → LOCK-6/ownership
  question) or the leaf carries a serde-safe projection (`Vec<(u8,u8)>`) — which is *itself* the
  domain-fit separation 5A denies.
- **G-5A.3 automata parity UNRUN.** No `from_expr` NFA byte-identical to `from_hir`; Group-elision
  and `Optional↔Repeat{0,1}` identity un-asserted; a silent NFA divergence is possible.
- **G-5A.6 classify shrink UNRUN.** No LOC-before/after; "relocation masquerading as reduction"
  (priorart trap #6) untested.
- **Grammar-totality SPEC-ONLY (P2).** `bootstrap/regex.bbnf::parse_generated()` returns `bool`
  — discards its AST; break-the-discard un-attempted; gated on LOCK-7.

---

## FAMILY 5B — LAWFUL-FUNCTOR — CRIT-1 convergence **30% — RETIRED (as architecture)**

PROTO-1 ran 5B's own un-bank/retire gate and settled it: **RETIRE**. The retire *verdict* is
earned (high disposition-certainty); the family's distinguishing **mechanism (the cross-domain
functor) carries no value the cheaper string-bridge merge does not already carry**. Credit the
rigor — 59/59 DFA-equivalence, product-BFS symmetric-difference oracle, 250-LOC measured.

**The subtle within-lock contradiction the synthesis predates.** PROTO-1 retires the *functor*;
PROTO-2 concludes *"5B's domain-fit separation is justified."* These are **not** in conflict once
split: the **functor mechanism is dead**, the **domain-fit two-type premise is vindicated**. The
FAMILY-SPECS conflates them under "5B BANKED 45%." The correct reading: 5B-the-functor → RETIRED;
5B-the-domain-fit-premise → promoted into the **lock verdict** (it is what beats 5A's merge).

**Enumerated open gaps (5B — salvage routing, not functor viability):**
- **The 250-LOC de-dup is UNHOMED.** It is a real, lawful consolidation available to *any*
  approach (string-bridge reuse of the regex-side rules). It must be BANKED as a **standalone
  refactor residual**, attributed to neither 5A nor 5B — otherwise a retired family's win is
  laundered into a live family's convergence.
- **Consumer-density of the surviving 184 LOC is UNMEASURED — a Y.13 risk on the whole
  grammar-tier regex e-graph.** PROTO-1 found the fusion rules are **structurally inert on
  projected (Map-wrapped) branches** (they locate a regex via `class_regex(child)`, which returns
  `None` on a `Map` child). Real grammar regex leaves usually carry `->`. So the surviving 184 LOC
  (and even the bare-branch rules) may fire on almost nothing. The **leaf-richness / projected-
  fraction census (5B's own Q3) was NOT run** — it decides whether the grammar-tier regex e-graph
  has enough consumers to exist at all, and it is the exact number Fork-B (abrogate) needs.
- **Fork-A vs Fork-B undecided.** 5B's home only exists under Fork-A; the census above informs the
  fork, but FAMILY-SPECS defers the fork to GESTALT with no census run.

---

## FAMILY 5C — AUTOMATA-AS-EXTRACTION — CRIT-1 convergence **45% (over-graded at 60%)**

No new 5C prototype ran in pass-2 (pass-2 PROTO-2 is the 5A brief; pass-1's PROTO-2 covered
2A/2B). So 5C's number rests entirely on pass-1 measurement, and FAMILY-SPECS's own prose already
concedes the ceiling: *"the maximal claim… is effectively refuted."* A family whose **centerpiece
is refuted** and whose **surviving deliverable is a 15%-contingent, unmeasured sliver** is not 60%
converged.

**Internal-soundness assessment (sound, but the grade doesn't follow the prose):**
- **2A minimization-as-congruence: NO-GO wholesale** (3.53× slower, N=500). The 15% acyclic path
  (0.80×) is measured **only against the restart strawman** (`dfa.rs:358` global restart), needs
  an **unbuilt alloc-light rewrite** to matter, and its net value **after `is_acyclic` dispatch on
  the other 85% is unmeasured, possibly negative**. This is the **elegant-reduction trap** in pure
  form: "ship the acyclic fast path" — and-then-the-hard-part (beat *correct* Hopcroft; prove the
  congruence rebuild hits O(n·k·log n) not degenerate Moore; prove the 15% slice is net-positive)
  is **entirely unrun**. The decisive question — *does deleting `:358` alone erase 2A's reason to
  exist?* — is the sharpest 5C uncertainty and **no prototype tested it**.
- **2B-as-minimizer: BLOCKED (Kozen 1994), correctly.** Star-induction is not a finite ground
  rewrite → no finite e-graph rule set is complete. This is a **provable** missing primitive, the
  cleanest BLOCKED in the lock. 2B-as-determinizer survives — sound.
- **2C tier: table-suffices — the ONE genuinely converged sub-part.** Local function of
  `(cardinality, prefix, byte-rarity)`, no cross-site coupling, CSP only on an exhibited global
  budget witness (else Y.13). Correctly single-homed in 5C over LOCK-2. This part earns its grade.

**Enumerated open gaps (5C):**
- **G-5C.1 reference-Hopcroft control (Null-B) UNRUN** — every 5C perf number is still measured
  against the restart strawman; a crossover vs HEAD is disinformation (priorart trap #2).
- **G-5C.2 congruence rebuild O(n·k·log n) UNPROVEN** — named asymptotic risk it degenerates to
  Moore-minus-restart (ties Null-A, loses Null-B).
- **The corpus is still 26 (`COMPILE_PATTERNS`), gate cites ≥50 — un-grown, un-stratified.** The
  acyclic/cyclic crossover cannot be located on it; the 15% fraction is unqualified.
- **G-5C.ρ quality debt `minimal(egraph)−minimal(Hopcroft)` unmeasured** across a grown corpus.
- **2C global-budget witness neither exhibited nor refuted** — the table-vs-CSP fork is argued,
  not demonstrated.

---

## FAMILY 5D — GRAMMAR-FRAGMENT — CRIT-1 convergence **35% (unchanged; parent-coupling now UNSOUND)**

Three source-CONFIRMED facts this pass are real and valuable (credit): the one-pass predicate is
**unsound for its own use** (`[a-z]+z` on `"abz"`, verified at `one_pass.rs:38`); the chooser
**does not fire** on `ByteClass` (`derive_recognizers` `_ => {}` swallow, `passes/lib.rs:338`);
the structured class **never reaches the emitter** (`BackendExpr::RegexProgram{pattern:String}`,
opaque). Each names its fix; none is a blocker. These are genuine bug-exposures, not design
convergence.

**Internal-soundness problem — the parent coupling is now broken.** FAMILY-SPECS folds 5D "into
5A" as "5A's generator-path lowering corollary." But **5A's merge is refuted (PROTO-2)**, so
"BANKED into 5A" now means BANKED-into-a-refuted-parent. 5D's actual substance —
`regex_desugar` to native `ExprKind` + a `ByteClass` leaf + FOLLOW-sound routing — is
**independent of whether the grammar leaf is merged into one type**; it needs a desugar pass, not
5A's ONE-HIR identity. So 5D **survives the 5A refutation** but its FAMILY-SPECS framing ("5D =
5A's lowering policy") must be **re-homed** onto the surviving domain-fit verdict, not the dead
merge. The synthesis's Axis-2 "5A ∧ 5D COMPATIBLE, one family" collapses when 5A does.

**The elegant resolution 5D leans on is unrun where it is hardest.** "A capturing regex desugars
its captures into grammar `Seq`/`Project` nodes — the grammar's OWN native span capture — so the
Laurikari tagged-DFA is never built." Elegant — but *and-then-the-hard-part* (prove the projected
sub-span reaches the tape byte-identically to a hand-authored grammar capture; prove the DFA path
carries only capture-free leaves) is **unexhibited**. Same trap shape as 5C.

**Enumerated open gaps (5D):**
- **G-5D.4 FOLLOW-sound one-pass fraction UNMEASURED** — the single number deciding delete-vs-rehome
  the DFA. Hand-evaluated ~92% reported / sound-fraction-lower; neither *run*.
- **G-5D.3 class-scan vs prefiltered `Dfa::find` UNMEASURED** — the honest prior is a **tie** on
  class-led; a tie must be reported as a tie (no inherited PROVE-10 band).
- **G-5D.chooser / G-5D.tape UNBUILT** — no `ByteClassRecognizer`, no `BackendExpr::ByteClass`
  reaching the tape, no capturing-leaf → `Project` exhibit. All three confirmed-faults have
  *named* fixes but **zero built**.
- **Parent re-homing** — 5D's charter coupling to 5A must be rewritten against the domain-fit
  verdict.

---

## CROSS-CUTTING CHECKLIST HITS (constitution + failure-mode)

- **Vacuous convergence (document-level).** FAMILY-SPECS % predate the two prototypes that refute
  them; the numbers must be re-cut post-evidence. [primary]
- **Elegant-reduction trap ×3.** 5C's 15% acyclic path, 5D's capture-to-`Project`, and the whole
  Axis-1 deferral-to-GESTALT each read *"and-then-the-hard-part is unconverged."*
- **Unverified gestalt / charter abdication.** Constitution (ii) makes LOCK-5 decide the lawful
  HIR form; FAMILY-SPECS defers it to GESTALT when the prototypes already earn the domain-fit
  verdict. The lock can and should render it.
- **Consumer-less substrate (Y.13) ×2, both prototype-confirmed.** 5A's named `SaturationCache`
  keys on `Hir`, not the Id-view it posits (dead cache-consumer). The grammar-tier regex e-graph
  is structurally inert on projected branches; its consumer density is unmeasured.
- **Spec-cites-itself / SPEC-ONLY raison d'être.** The lock's headline ("grammar-driven regex
  engine," constitution i) is SPEC-ONLY lock-wide: no artifact exercised a grammar-driven path;
  `bootstrap/regex.bbnf` discards its AST. Correctly labeled, but it means the constitutional
  premise is entirely unexercised — gated on LOCK-7.
- **Parsimony / god-type (vi).** 5A's "one type" is already false at HEAD (`IrNode→DagNode→
  GrammarENode` three-enum mirror).
- **Zero-runtime-cost types (v).** Not exercised — no type-plane erasure demonstrated for
  `Class`/`Look` through LOCK-1's real HM; `Class ⊢ Span` graded only against skinny's stub.
- **Win-laundering guard.** The 250-LOC de-dup (real) must be BANKED as a standalone refactor, not
  credited to any live family; PROTO-1 explicitly shows it is achievable without the functor.
- **No SPEC-ONLY-anchoring-a-perf-claim violation found (credit).** PROVE-10's band is measured;
  5A/5D both explicitly refuse to inherit it (G-5A.3, G-5D.3 re-derive from the merged type).
- **No wrong-tree fraud (credit).** Both prototypes base-pinned `7cdbff4ef`, ran in top-level
  `crates/ir`; PROTO-1 consumed parse-that READ-ONLY via rsync scratch.

## The single sharpest within-lock demand for pass-3

The two briefs tested the right uncertainties and **resolved them** — but the resolution
(neither 5A-merge nor 5B-functor; domain-fit-two-types + string-bridge de-dup wins) is **not
written into any verdict line**. Pass-3 must (1) re-cut all four convergence numbers against the
prototype evidence, (2) render the Axis-1 verdict *within the lock* rather than defer it, (3) run
the one unrun number that gates the P1 fork — the **projected-fraction / leaf-richness census**
that decides whether the surviving grammar-tier regex e-graph has any consumers (Fork-A) or should
be abrogated (Fork-B), and (4) run G-5C.1 (reference-Hopcroft Null-B) so 5C's sliver is measured
against a correct baseline, not the restart strawman.
