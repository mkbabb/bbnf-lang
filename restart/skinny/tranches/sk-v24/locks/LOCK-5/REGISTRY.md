# LOCK-5 — THE REGEX CONTINUUM — FAMILY REGISTRY

Grammar-driven regex engine: the regex HIR truly isomorphic to the bbnf HIR/IR, with
NFA→DFA→minimize→prefilter recast as solver problems where measured to win.

**Status legend.** ADVANCED = active line, earns prototype budget. BANKED = sound but
redundant-under-squeeze or absorbed into a sibling; held in reserve behind a named
un-bank/retire gate; not killed. BLOCKED = the family's *maximal claim* rests on a missing
primitive that is as-hard-or-harder than the original problem (the family may survive in a
narrowed form). RETIRED = the family's distinguishing mechanism is removed from the portfolio.

**Convergence scoring.** Conservative floor of the two within-lock critics, raised only with a
named reason a critic erred. Two such reasons were found this pass and are named at their families
(5A serde over-count; 5B RAN-earned-retire under-credit). Pass-2's decisive numbers ran in **one**
top-level `crates/ir` tree (both prototypes base-pinned `7cdbff4ef`) — P1's "measure in one tree"
demand is discharged under the Fork-A premise; the numbers are earned *in that tree*, still
tree-conditional on GESTALT's fork.

---

## THE PASS-2 HEADLINE — BOTH AXIS-1 MAXIMAL HORNS RAN, BOTH FAILED; THE SURVIVOR IS UN-HOMED

The pass-2 FAMILY-SPECS (synthesis, mtime 09:45) declared *"no new prototype ran … these are the
pass-3 budget"* and carried the pre-run floors. **Its own two budgeted prototypes then ran inside
pass-2 (PROTO-2 09:59, PROTO-1 10:01) and both returned decisive negatives.** Both critics flag this
as an unverified-gestalt at the document level: the synthesis convergence table is *stale against
evidence in its own folder*. **Every number below is re-cut from the prototypes, not the synthesis.**

- **PROTO-2 (5A clean-merge horn) — REFUTED, RAN on HEAD, top-level `crates/ir`.** Merging
  `Class(CharClass)`/`Look(Look)` into `IrNode`: `E0004×53` non-exhaustive + `E0277×4` serde breaks;
  **REGEX-LEAK ≈ 29 ≫ 0**; the named `SaturationCache` is `FxHashMap<Hir,Hir>` (Hir-keyed), **not**
  the auto-derived Id-view — the two-views cache claim is type-level false (the collapse that works is
  the egraph *memo*, a different cache). "There is nothing to map / the god-type dissolves" is falsified.
- **PROTO-1 (5B functor un-bank gate) — RETIRED per its own gate, RAN on the same HEAD.** Copy dies
  **250 LOC** (434→184, C1 MET), round-trip **59/59 DFA-equivalent** (C3 MET), but **C2 (the
  projection-dropping-union bug fails-under-string-bridge) NOT MET** — the bug is a *non-bug* under the
  `Map`-wrapper representation of `->` (`projection_union_merge_is_already_safe_under_string_bridge`
  passes). The un-bank conjunction requires all three; C2 is false → the 250-LOC win reduces to a bare
  string-bridge merge → the family's own RETIRE trigger *"dies trivially under a bare 5A merge."*

**The reconciliation both prototypes require, written nowhere in the synthesis (now the lock's named
Axis-1 verdict).** PROTO-2 justifies keeping **two domain-fit TYPES** (refutes folding them into one);
PROTO-1 retires the **FUNCTOR as an architecture with independent value** (its soundness win is a
non-bug, its LOC win is bare-merge-achievable). These are not contradictory once split:

> **AXIS-1 VERDICT (rendered within LOCK-5, per constitution ii — the lock decides the lawful HIR form):
> keep TWO DOMAIN-FIT TYPES (grammar keeps `Regex(StringId)` opaque; the engine keeps the rich `Hir`,
> grown with `Class`/`Look`), joined by a CHEAP string/`format!` bridge that reuses the engine's own
> `simplify_hir`; the 250-LOC grammar-tier consolidation is a standalone refactor residual owned by
> neither family. 5A-identity is refuted; 5B-functor is retired; the survivor is the third position.**

This is a *renderable* verdict — CRIT-1 and CRIT-2 both demand it be rendered here rather than deferred
to GESTALT, precisely because the evidence to decide it landed 15 minutes after the synthesis was written.
The deferral-to-GESTALT was the checklist's elegant-reduction / charter-abdication trap.

---

## PRECONDITIONS (lock-wide, gate every family's earned convergence)

**P1 — the two-tree reconciliation, now a named GESTALT fork (not a footnote).** Two live snapshots
hold incompatible substrate:

| | skinny `crates/ir` (graduating, skv22) | top-level `crates/ir` (mature egraph) |
|---|---|---|
| regex leaf | `ExprKind::Regex{pattern:String}` — opaque (`lib.rs:250`) | `IrNode::Regex(StringId)` — interned (`node.rs:36`) |
| grammar-tier regex e-graph | **absent** (seam only) | `egraph/rules/regex.rs` — **434 LOC, real** |
| shape | already 5A-shaped (nothing to bridge) | already 5B-shaped (the 434-LOC copy 5B kills) |

Both pass-2 prototypes ran in the **top-level** tree (the only one where 5A's `egraph-derive` enabler
*and* 5B's 434-LOC value-target physically exist) — discharging P1's "measure in one tree" under the
**Fork-A** premise. GESTALT must pick the fork:
- **Fork-A (fold top-level INTO skinny):** the copy survives reconciliation → the numbers above are the
  tree of record; 5B's LOC-death is real-but-bare-merge-achievable.
- **Fork-B (abrogate the grammar-tier regex e-graph):** skinny ships without one today → the 434-LOC
  copy is **deleted for free, by neither family** → 5B's 250-LOC death is a fork artifact, not a family
  win; and 5A's cache-view loses its only consumer (Y.13 on 5A's own mechanism). **The projected-fraction
  / leaf-richness census (the number Fork-B needs) is the ONE unrun number gating the fork** — PROTO-1
  found the surviving 184 LOC are structurally inert on `Map`-wrapped (projected) branches, so the
  grammar-tier e-graph's consumer density is unmeasured and may be near zero.

**P2 — grammar-driven is SPEC-ONLY lock-wide (re-confirmed; ZERO grammar-driven artifacts after two
passes).** Neither pass-2 prototype exercised a grammar-driven regex path (both are IR-level). Two
`regex.bbnf` diverge — `parse-that/rust/bootstrap/regex.bbnf` (`parse_generated()` returns `bool`,
**discards its AST**) and `grammar/misc/regex.bbnf` (divergent) — and skinny hand-scans regex to an
opaque `String` (`grammar/src/lib.rs:1561`). The lock named "grammar-driven regex engine" has exercised
no such path in two passes. Constitution (i) is SPEC-ONLY; 5A's task is **promote + de-dup +
break-the-discard**, gated on LOCK-7/7A. **Every verdict line carries the SPEC-ONLY(grammar-driven) label.**

---

## FAMILY 5A — ONE-HIR — **ADVANCED — 45%** (maximal horn REFUTED; narrowed survivor feeds the Axis-1 winner)

Regex as the fragment of the bbnf HIR carrying no rule reference and no projection —
`{Seq,Alt,Repeat,Optional,Literal,Class,Look}` — with `Class`/`Look` added to the grammar node so the
regex HIR *is* a subset ("nothing to map," identity not isomorphism).

**Maximal claim REFUTED by run (PROTO-2, RAN, HEAD `7cdbff4ef`, top-level tree).** Three fail-axes:
1. **Tax axis — REGEX-LEAK ≈ 29 ≫ 0.** research-A's pass-2 "0-leak" was a **category error** — it
   censused the combinator arms (`Seq/Alt/Repeat`, clean) not the passes that must *dispatch* on the new
   leaves. Named leaking passes: `sets/first_sets.rs` (FIRST of `Class` is `cc.to_positive_byte_ranges()`;
   a `_ => empty` is a silent-epsilon bug), `passes/regex_info.rs::collect_regex_ids` (keys on `StringId`
   a `Class` lacks — the `SaturationCache` feeder), `passes/types/generate.rs` (leaf→`TypeDesc`),
   `passes/payload/scalar_routing.rs`, `passes/materialization/classify.rs` (now *evidence against* the
   "classify re-homes trivially" claim), `vm/compiler/node.rs` (byte-range codegen).
2. **Cache axis — the two-views mechanism is type-level FALSE.** `SaturationCache` is `Hir`-keyed; the
   span-erased collapse that PROTO-2 got to pass is the **egraph memo**, a *different* cache than 5A
   named. 5A's headline cache-consumer is consumer-less as specified (Y.13 on 5A's own mechanism,
   prototype-confirmed, not the Q5 hypothetical it was banked as).
3. **Serialization axis — the derive breaks** (`E0277×4`): `IrNode` derives `Serialize/Deserialize`;
   `bbnf_regex::CharClass`/`Look` carry no serde. *(Named critic correction: CRIT-2 verified the crate's
   `serde` feature is already on — this is a one-line parse-that `#[cfg_attr]` edit blocked only by the
   probe's READ-ONLY policy, NOT a structural wall. CRIT-1 over-counted it as an independent structural
   refutation; the robust refutation is the leak census alone. This correction raises the floor 42→45.)*

Plus a run-surfaced structural refutation of the "ONE authored type" premise: `IrNode → DagNode →
GrammarENode` are **three hand-mirrored enums today**; the merge is mirrored into two of them — the
god-type is not dissolved, it is triplicated.

**What genuinely survives (credited).** `egraph-derive`'s `Language` projection **is total** over the
merged leaves (`classify_field` sends non-`Id` payloads to `Leaf`); the egraph memo **does** collapse
span-distinct duplicates to one e-class without forcing `Eq/Hash` onto the spanned type (two-views
geometry sound *on the egraph axis*). G-5A.2 (`/foo$/` rejects `"foobar"`, `/foo/` accepts) is a
genuinely **falsifiable** consumer gate — correctly designed. And growing `Class`/`Look` **on the engine
side** is still needed by the Axis-1 winner. So 5A is not zero — but its maximal claim is dead, and the
**narrowed survivor (Class/Look leaves, grammar leaf stays opaque) IS the domain-fit landing**, not the
identity-merge.

**Exact open gaps.** (1) Leak *magnitude* soft — only 6/53 arms directly read; pass/fail (≫1) robust,
the ≈29 count is not load-bearing. (2) Serde wall is artifact-removable but crosses the parse-that
READ-ONLY / LOCK-6 ownership boundary — unresolved *who* adds the derive. (3) **G-5A.3 automata parity
UNRUN** (`from_expr` NFA byte-identical to vendored `from_hir`; Group-elision + `Optional↔Repeat{0,1}`
un-asserted; PROVE-10 band not re-derived from the type — SPEC-ONLY after two passes). (4) **G-5A.2
Look-consumer test UNBUILT** — "closed *conceptually*" is the tell; `Look::Start` inert stands where
pass-1 left it. (5) classify re-home now shown **non-trivial by the run** (needs regex dispatch), not the
elegant one-liner. (6) grammar-totality SPEC-ONLY (P2, `parse_generated` discards its AST). (7)
`Class ⊢ Span` graded only against skinny's stub `BuiltinTy`, not LOCK-1's real HM.

**Pass-3 directives.**
- *Must answer:* re-cut the leak census by *writing* the arms (magnitude, not estimate); route
  `Class`/`Look` typing through LOCK-1's real type system; where does `[α-ω]*` byte-expansion live
  (confined to automata per the `to_positive_byte_ranges` panic, or leaks into the shared type).
- *Must prototype:* G-5A.3 `from_expr`≡`from_hir` NFA byte-identity + freshly-run throughput; the
  `/foo$/` Look-consumer test that can *fail*; classify re-home with LOC before/after (shrink, not relocate).
- *Withhold from researchers (anti-anchoring):* PROTO-2's ≈29 leak *magnitude* (hand-graded) and any
  inherited PROVE-10 band — force the count to be built and the automata numbers re-run from the type.

---

## FAMILY 5B — LAWFUL-FUNCTOR — **RETIRED — 40%** (earned-conditional; the retire is RAN-earned)

Two domain-fit HIRs joined by a total functor `F ⊣ G` through the shared e-graph; the win was
*write-the-regex-algebra-once* and delete the duplicate grammar-tier rules.

**RETIRED by measurement (PROTO-1 RAN 5B's own un-bank gate — a genuine advance over pass-1's vacuous
elimination; both critics credit this as the pass's clearest process improvement).** The functor is
lawful (59/59 DFA-equivalence via product-BFS symmetric-difference over 256 bytes; structural fuse ≡
string fuse) and would delete 250 LOC — but the projection-dropping-union soundness bug it was banked to
fix **does not exist** under the current `Map`-wrapper representation of `->`. Root cause: `[a-c]->A`
lowers to `Map{Regex([a-c]),fn_id}`; the fuse/union/superset rules locate a branch's regex via
`class_regex(child)` over the child's e-class, so a projection-wrapped branch resolves to `None` and the
rules are **structurally inert on projected branches** — a property of the representation, tier-agnostic.
The 250-LOC win is the *shared* payoff of reusing the engine-side rules, achievable by a bare
string-bridge merge with identical correctness → the RETIRE trigger fires.

**Rule-death table (PROTO-1 measured):** `SupersetAbsorbAlt`(~80) + `UnionMergeAlt`(~90) +
`FuseAltRegexBranches`-string-parts(~85) + `regex_escape_literal`/`pattern_has_top_level_pipe`(~45) +
`class_regex` = **250 LOC deleted** (434→184); `DeduplicateAltBranches` survives (grammar-general).

**Two conditions keep the retire EARNED-CONDITIONAL (re-open triggers, not closed facts).**
- **G5B-a — the retire rests on the `Map`-barrier representation, which LOCK-1 has NOT ratified.** If
  LOCK-1's real `->` typing represents projection as anything other than an inner-class `Map` barrier
  (e.g. a typed edge annotation), the projection-dropping-union bug **reappears and un-retires 5B**.
  Recorded as a LOCK-1-gated re-open trigger.
- **G5B-b — the 250-LOC death is a FORK artifact.** Under Fork-B the copy is deleted for free; under
  Fork-A it dies to a bare merge. Either way the LOC death is **not evidence for any family** — it is
  BANKED as a standalone refactor residual (win-laundering guard).

**Named reconciliation (the flat contradiction resolved).** PROTO-2 text "5B's domain-fit separation is
justified" and PROTO-1 text "5B → RETIRED" are not opposite: PROTO-2 vindicates two domain-fit **TYPES**
(the Axis-1 winner's premise), PROTO-1 retires the **FUNCTOR** as independent-value architecture. Both
hold. *(Named floor correction: CRIT-1's 30% under-weights the RAN-earned retirement; CRIT-2's ~55%
over-weights given the two open conditions — 40% sits between, disposition-certainty of a conditional retire.)*

**Un-retire gate (if pass-3 re-opens it).** Only LOCK-1 abandoning the `Map`-barrier representation
reopens 5B. The **joint coproduct** variant (`Left(HirENode)|Right(GrammarENode)`, bridge folded
pre-emission) was never built either pass — it dies undesigned, acceptable given the functor's core value
axis is now a proven non-bug.

**Pass-3 directives.**
- *Must answer:* nothing new for the functor — it is retired. The one live question is LOCK-1's `->`
  representation (owned by LOCK-1), which is the sole re-open trigger.
- *Withhold:* leave the coproduct un-opened unless LOCK-1 reopens the projection-soundness question.

---

## FAMILY 5C — AUTOMATA-AS-EXTRACTION — **ADVANCED — 45%** (internal 2B-as-minimizer BLOCKED; NO 5C prototype ran in pass-2)

Push the shared e-graph one level below the HIR, to the automaton. Orthogonal to Axis-1 — rides *below*
whichever type-plane wins. **No 5C prototype ran in pass-2** (PROTO-2 was the 5A brief, PROTO-1 the 5B
gate); every decisive 5C number is exactly as unrun as pass-1 left it. Pass-2 sharpened the *analysis*
(named the O(n²) asymptotic risk; re-confirmed the `:358` restart present; re-stated Kozen) — sharper
prose is not a run; the family must **not** rise. *(Named floor correction: CRIT-2 held the inherited
pass-1 60% without re-grading against its own "centerpiece effectively refuted" prose; CRIT-1 re-cut to
45% with reasons — centerpiece refuted, survivor a 15%-contingent sliver, all decisive numbers unrun
after two passes. Take 45%.)*

- **2A minimization-as-congruence — EXACT (26/26 byte-identical), perf NO-GO wholesale (3.53× slower,
  N=500), 15% acyclic sliver contingent.** The win (0.80×, 4/26) is measured **only against the
  `hopcroft_minimize` strawman** (naive Moore + a global restart at `dfa.rs:358`, O(n²·k·B) — *not*
  Hopcroft), needs an **unbuilt** alloc-light rewrite to matter, and its net value **after `is_acyclic`
  dispatch on the other 85% is unmeasured, possibly negative.** The decisive question — *does deleting
  `:358` alone erase 2A's reason to exist?* — is unrun after two passes.
- **2B determinization-as-derivative-saturation — internal BLOCKED (as minimizer), cleanly confirmed.**
  ρ_max = 1.667, organic miss `\w+(?:[-_]\w+)*`; `default_hir_rules` is exactly the ACUI +
  charclass-lattice + bound-arithmetic fragment — sound, incomplete for regular-language equivalence.
  Kozen 1994: star-equivalence needs the star-induction rule, not a finite ground rewrite → **no finite
  e-graph rule set is complete** — a provably-impossible missing primitive (the lock's cleanest BLOCKED).
  2B-as-*determinizer* survives (25/26 ρ=1); 2A/Hopcroft closes.
- **2C prefilter/tier-selection — UNPROTOTYPED, the honest boundary is a TABLE not a CSP.** Local
  function of `(cardinality, literal-prefix, byte-rarity)`, no cross-site coupling; a CSP only on an
  exhibited global codegen/LUT-budget witness (else Y.13). **Double-homed with LOCK-2's scan-plan CSP —
  single-home the table in 5C; escalate only on a witness.** The one part that could positively *ship*
  was never built either pass.

**Exact open gaps.** (1) G-5C.1 reference-Hopcroft control (Null-B) UNRUN — every perf number is vs the
strawman. (2) delete-`:358` null unpriced. (3) congruence rebuild O(n·k·log n) UNPROVEN (named risk it
degenerates to Moore-minus-restart). (4) corpus still **26** (`COMPILE_PATTERNS`), gate cites ≥50 —
un-grown, un-stratified; the acyclic/cyclic crossover cannot be located. (5) G-5C.ρ quality-debt
`minimal(egraph)−minimal(Hopcroft)` unmeasured on a grown corpus. (6) 2C global-budget witness neither
exhibited nor refuted.

**Pass-3 directives.**
- *Must prototype:* the reference O(n·k·log n) Hopcroft control + the delete-`:358` null, on a GROWN
  (≥50, tagged acyclic/cyclic) corpus, with `is_acyclic` dispatch folded across ALL patterns; 2C's
  table-vs-CSP with a global-budget witness or its downgrade.
- *Must answer:* does the acyclic fast path beat a *correct* Hopcroft, or does fixing `:358` erase its
  reason to exist; does the congruence rebuild hit O(n·k·log n).
- *Withhold:* the incumbent restart-Moore as "the" baseline (force the reference-Hopcroft control) and
  2C's "clean CSP GO" framing (make table-vs-CSP be earned).

---

## FAMILY 5D — GRAMMAR-FRAGMENT — **BANKED — 35%** (re-homed onto the domain-fit survivor, NOT the refuted 5A-maximal)

A `/pat/` desugars once into native grammar nodes; the classical NFA→DFA survives as one opt-in lowering
target the scan-plan cost model selects. Pass-2 turned three pass-1 *suspicions* into three
source-CONFIRMED *diagnoses* (honest work) — but a confirmed bug is a problem statement, not a resolution,
and the two decisive numbers are unrun after two passes.

**Parent re-homing (critical).** FAMILY-SPECS folded 5D "into 5A as 5A's lowering policy" — but 5A's
maximal merge is REFUTED, so that framing banks-into-a-refuted-parent. 5D's actual substance
(`regex_desugar` to native nodes + a `ByteClass` leaf + FOLLOW-sound routing) is **independent of the
ONE-HIR identity**; it needs a desugar pass, not 5A's merge. **5D survives the 5A refutation and re-homes
onto the surviving domain-fit-two-types verdict** — it is the *generator-path lowering policy* of the
Axis-1 winner, not of the dead identity-merge.

**Three source-CONFIRMED facts (both critics independently grepped):**
1. **`check_one_pass_eligible` is UNSOUND for its own use** (`one_pass.rs:64`): its `Repetition` arm
   recurses into the sub, never checks the class against its own FOLLOW; `[a-z]+z` on `"abz"` returns
   `true` but is not one-pass (greedy give-back) → a straight-line lowering would **reject a matching
   string**. Both trees route on this flag (`RegexRecognizer::scanable()`). *Fix (elegant):* desugar to
   `Seq[Repeat, suffix]`, let grammar FOLLOW (`AltMode::Dispatch`) decide; priorart: regex-automata uses
   build-and-bail, not a pre-deciding predicate. UNBUILT.
2. **`derive_recognizers` does NOT fire on `ByteClass`** (`passes/lib.rs:353`): matches single-byte
   `Literal` + `analyze()`-keyed `QuotedString` `Regex`, then `_ => {}` silently swallows a desugared
   `ByteClass` → the SIMD-recognizer chooser 5D rests on **is not firing** (pass-1 masked-wildcard
   contradiction now source-confirmed). *Fix:* structural `ByteClassRecognizer`. UNBUILT.
3. **The structured class never reaches the emitter** (`BackendExpr::RegexProgram{pattern:String}`,
   opaque). Typed-materialization invariant obliges `BackendExpr::ByteClass` → tape. UNBUILT.

**Exact open gaps.** (G-5D.4) FOLLOW-sound one-pass fraction — THE number deciding delete-vs-rehome the
DFA — UNMEASURED two passes (hand-evaluated ~92% under the *unsound* predicate; sound fraction lower,
neither run). (G-5D.3) class-scan vs prefiltered `Dfa::find` cold-paired UNMEASURED (honest prior: **tie**
on class-led, DFA wins on literal-prefix-led — a tie is a tie). (G-5D.chooser/tape) all three
confirmed-faults have *named* fixes, **zero built**. Capture-to-`Project` (the Laurikari-tagged-DFA
avoidance) unexhibited (elegant-reduction trap — and-then-the-hard-part unrun).

**Pass-3 directives.**
- *Must answer:* G-5D.4 the FOLLOW-sound one-pass fraction over the engine corpus with real leaves.
- *Must prototype:* G-5D.3 cold-paired class-scan vs prefiltered `Dfa::find`; the `ByteClassRecognizer`
  that actually fires (retire the masked wildcard); `BackendExpr::ByteClass` reaching the tape;
  capture-leaf → `Project` byte-identical to a hand-authored grammar capture.
- *Withhold:* the "one-pass fraction is high / straight-line beats the DFA" prior — measure both fresh.

---

## REDIRECT-CROWDING DECISIONS

- **Axis-1 did NOT converge to one mechanism — both mechanisms FAILED, collapsing to a shared
  SURVIVOR.** 5A-identity refuted (PROTO-2), 5B-functor retired (PROTO-1). The survivor — **two
  domain-fit types + a cheap string bridge** — is owned by neither family's original charter and is
  **re-homed as the lock's Axis-1 verdict** (see headline). 5A stays ADVANCED as the *narrowed* Class/Look
  leaf-growth that feeds the survivor; 5B is RETIRED (functor), its domain-fit-two-types *premise* folded
  into the winner.
- **The 250-LOC de-dup is BANKED as a standalone refactor residual** attributed to neither 5A nor 5B —
  a lawful string-bridge reuse of the engine's `simplify_hir`, achievable without the functor and deleted
  for free under Fork-B. Crediting it to any live family would launder a retired family's win.
- **Underexplored formulation OPENED in place of the collapsed Axis-1 horns: the GRAMMAR-DRIVEN
  BOOTSTRAP path** (constitution i's namesake). After two passes with ZERO grammar-driven artifacts, the
  genuinely underexplored thing is not another type-plane variant but the **break-the-discard** work:
  promote `parse-that/bootstrap/regex.bbnf`, make `parse_generated()` return its AST instead of `bool`,
  de-dup `grammar/misc/regex.bbnf`. This is the only formulation neither pass touched. Gated on LOCK-7/7A.
- **Within 5C: 2B-as-minimizer BLOCKED (Kozen) — the redundant reading;** the underexplored survivor is
  **2C** (tier table/CSP), the one part that could positively ship and was never prototyped either pass.

---

## CROSS-POLLINATION (named grafts only, both sides developed)

- **GRAFT-1 (holds, re-homed): the engine-side rich `Hir`'s derived `Language` hosts 5C-2B-determinizer's
  saturation theory.** With 5A-maximal refuted, the graft lives in the *engine* tree (not a merged
  grammar type): `HirENode` already derives `Language`; 2B saturates over it, 2A/Hopcroft closes. Both
  sides developed (PROTO-1's `simplify_hir` round-trip RAN 59/59; PROTO-2 5C RAN with a clean bound). The
  constitution's "automata construction consumes the IR e-graph" realized end-to-end, engine-side.
- **GRAFT-2 (mechanical): 5C-2A's byte-class partition consumes the engine's `Class` byte-sets.**
  `sets/equiv.rs` reads the `ByteSet` the `Class` leaf carries — a consequence of 5C riding on the
  Axis-1 winner's HIR, not a separate build.
- **BANKED de-dup graft (refactor residual): the grammar-tier 250-LOC copy reuses the engine's
  `simplify_hir` via the cheap string bridge** — the concrete, RAN cross-tier reuse PROTO-1 exhibited.
  Not a family win; a standalone consolidation.
- **WITHHELD (not developed enough): 5D's `ByteClassRecognizer` chooser × 5C-2C tier table.** Both feed
  the PROVE-10 prefilter seam, but 5D is 35% (resolution unbuilt) and 2C unprototyped — the graft cannot
  yet be named. Deferred coupling, not an active graft.

---

## CROSS-LOCK DOUBLE-HOMES (constitution ix — LOCK-5 is the hub; named back; resolutions are LOCK-5
proposals GESTALT must RATIFY, not settled facts)

1. **Axis-1 verdict ⊗ LOCK-4-4D / LOCK-9-9C / LOCK-3-3C.** All three hang on "ONE-HIR." The verdict is
   now **"two domain-fit types + cheap bridge"** — which is arguably *not* one HIR. **Re-cut 4D/9C/3C to
   consume the two-types landing, or they converge on a verdict the prototypes refuted.** 4D's premise
   ("one HIR, not a second engine") is specifically weakened.
2. **5B retirement ⊗ LOCK-1 (unratified representation dependency — RE-OPEN TRIGGER).** The retire rests
   on `->` staying a `Map`-wrapper barrier. LOCK-1 owns the real `->` typing and has not ratified it. If
   LOCK-1 represents projection otherwise, the projection-dropping-union bug reappears and un-retires 5B.
3. **5C-2C ⊗ LOCK-2 scan-plan/tier CSP.** Same tier-selection, two homes, no witness sought. **Single-home
   the table in 5C** (a property of the compiled DFA); escalate to the CSP home only on an exhibited
   global codegen/LUT-budget witness. LOCK-2 has not deferred it — unadjudicated.
4. **5A/5D ⊗ LOCK-9-9C (SCANNER-AS-DFA).** 9C sources `bootstrap/regex.bbnf` and extends the same classify
   `RegexClass`. **Supersession: LOCK-5 owns the HIR→DFA→classify substrate + the `ByteClass` chooser; 9C
   consumes it or retires.** But PROTO-2 shows the classify re-home is **non-trivial** (needs regex
   dispatch) — the substrate LOCK-5 offers 9C is itself unbuilt.
5. **5A ⊗ LOCK-7 bootstrap.** Two divergent `regex.bbnf` (one discards its AST). **Re-scope 5A from
   "author regex.bbnf" to "promote + de-dup + break-the-discard"; SPEC-ONLY(bootstrap) until 7A.**
6. **Type-plane ⊗ LOCK-1.** `Class ⊢ Span` graded only against skinny's stub `BuiltinTy`. **Route the
   `Class`/`Look` typing verdict through LOCK-1's real HM + row-poly + widths.**
7. **rust/ts ⊗ LOCK-6.** With the winner now **two types**, LOCK-6's mirror/skeleton must carry BOTH
   (`Regex(StringId)` grammar-side + rich `Hir`/`Class`/`Look`/`ByteClass` engine-side + automata crossing
   to TS, PROVE-14 tape) — a larger obligation than "carry the merged enum." Named gap for GESTALT-Ω;
   still nobody's charter. The serde-derive-ownership question (who adds `CharClass: Serialize`) also lands
   on the parse-that READ-ONLY / LOCK-6 boundary.

---

## PASS-3 DIRECTIVES (consolidated, ordered by what gates the most downstream)

1. **Render the Axis-1 verdict WITHIN the lock** (done above) and **re-cut LOCK-4-4D / LOCK-9-9C /
   LOCK-3-3C** from "ONE-HIR" to "two-domain-fit-types + cheap bridge" — three downstream locks currently
   hang on a verdict the prototypes refuted.
2. **BANK the 250-LOC de-dup as a standalone refactor residual** (neither family's win); record 5B's
   RETIRE as conditional on LOCK-1 keeping the `Map`-barrier + fork-independence (Fork-B deletes the copy
   for free).
3. **Run the Fork-gating number: the projected-fraction / leaf-richness census** of the grammar-tier
   regex e-graph (does the surviving 184 LOC fire on anything, given it is inert on `Map`-wrapped
   branches?) — the number that tells GESTALT Fork-A (keep) vs Fork-B (abrogate).
4. **Run the still-decisive unrun numbers:** 5D G-5D.4 (FOLLOW-sound one-pass fraction) + G-5D.3
   (class-scan vs prefiltered `Dfa::find`, cold-paired); 5C reference-Hopcroft control + delete-`:358`
   null on a GROWN (≥50, tagged) corpus + 2C table-vs-CSP witness; 5A G-5A.3 automata parity from the
   narrowed type + a `/foo$/` Look-consumer test that can FAIL.
5. **Exercise ONE grammar-driven regex path** (open the underexplored bootstrap formulation: promote
   `bootstrap/regex.bbnf`, break-the-discard, de-dup `grammar/misc/regex.bbnf`) — the lock's namesake,
   SPEC-ONLY with zero artifacts after two passes.
6. **Single-home the tier CSP** against LOCK-2; **RATIFY** (not merely declare) the 9C / LOCK-1 / LOCK-6
   supersessions with those locks.
7. **Withhold from researchers:** PROTO-2's leak *magnitude* (≈29 is hand-graded, 6/53 arms read — build
   it) and any inherited PROVE-10 band; PROTO-1's functor conclusion (retired — do not re-litigate); hand
   researchers the **un-homed two-types-cheap-bridge landing as the thing to HARDEN**, not a horn to
   re-open.
8. **Carry the SPEC-ONLY(grammar-driven) label** on every verdict line lock-wide until 7A lands (P2).

---

## PER-PASS HISTORY

### Pass 2 (research-{A,B,C,D,priorart} → FAMILY-SPECS → PROTO-2 [5A brief], PROTO-1 [5B brief] → CRIT-1, CRIT-2 → this)

| Family | Status | % | One-line |
|---|---|---|---|
| 5A ONE-HIR | ADVANCED (maximal REFUTED) | 45 | PROTO-2 RAN on HEAD: clean-merge refuted — REGEX-LEAK ≈29 ≫ 0, cache Hir-keyed not Id-view (two-views claim type-level false), serde break (artifact-removable), 3 mirrored enums; egraph Language totality + memo-collapse hold; narrowed Class/Look survivor feeds the Axis-1 winner; automata parity / Look-consumer / classify-shrink / bootstrap still unrun |
| 5B LAWFUL-FUNCTOR | **RETIRED (earned-conditional)** | 40 | PROTO-1 RAN 5B's own gate: copy dies 250 LOC (C1 MET) + 59/59 DFA-equiv (C3 MET), but C2 (projection-drop soundness bug fails-under-string-bridge) NOT MET — the bug is a non-bug under the Map-wrapper repr → dies to a bare merge; retire RAN-earned but conditional on LOCK-1 keeping the Map-barrier + fork-independence |
| 5C AUTOMATA-AS-EXTRACTION | ADVANCED (2B-as-minimizer BLOCKED) | 45 | NO 5C prototype ran this pass; reference-Hopcroft control + delete-`:358` null + grown/tagged corpus + 2C all still unrun after two passes; centerpiece (2A beats classical) effectively refuted, survivor a 15%-contingent strawman-measured sliver; 2B BLOCKED (Kozen) cleanly confirmed |
| 5D GRAMMAR-FRAGMENT | BANKED (survivor's lowering policy) | 35 | 3 in-tree facts source-CONFIRMED (unsound one-pass predicate `one_pass.rs:64`, masked `ByteClass` wildcard `lib.rs:353`, opaque `RegexProgram` emitter) — diagnosis sharpened, resolution unbuilt; re-homed off the refuted 5A-maximal onto the domain-fit survivor; G-5D.4/.3 unmeasured two passes |

**Prototypes run this pass.** PROTO-2 (5A merge census, RAN_MEASURED on `7cdbff4ef`, top-level `crates/ir`:
57 errors = 53 E0004 + 4 E0277; ≈29 REGEX-LEAK; egraph-derive Language TOTAL; two-views probe 7/7 PASS;
SaturationCache Hir-keyed). PROTO-1 (5B un-bank gate, RAN_MEASURED on the same HEAD: 250 LOC death, 59/59
DFA-equiv, C2 not met — projection bug is a non-bug under the string bridge). Both base-pinned the ONE
top-level tree (no wrong-artifact-risk); parse-that consumed READ-ONLY. **Axis-1 was resolved by the runs
and the resolution — two domain-fit types + cheap bridge, owned by no family's charter — is now rendered
as the lock's verdict** (the pass-2 synthesis's stale deferral-to-GESTALT is corrected here).

### Pass 1 (research-{A,B,C,D,priorart} → FAMILY-SPECS → PROTO-1, PROTO-2 → CRIT-1, CRIT-2 → registry)

| Family | Status | % | One-line |
|---|---|---|---|
| 5A ONE-HIR | ADVANCED | 55 | Tax+nullability RAN & favourable, but stale/pre-favouring tree; bootstrap, automata parity, `Look`-consumer, classify re-home, UTF-8, god-type all SPEC-ONLY |
| 5D GRAMMAR-FRAGMENT | BANKED (into 5A) | 35 | Maximal "no engine" reading RETIRED by PROVE-10; folded as 5A's lowering policy; decisive one-pass fraction + throughput UNMEASURED; masked-wildcard contradiction |
| 5B LAWFUL-FUNCTOR | BANKED | 45 | Sound but converges with 5A + value-target (434-LOC copy) absent from the graduating tree; eliminated-without-testing (0/5 gates); held behind the G-5B.2 un-bank/retire gate |
| 5C AUTOMATA-AS-EXTRACTION | ADVANCED | 60 | 2B-as-minimizer BLOCKED (ρ_max=1.667); 2A EXACT but 3.53× slower, 15% acyclic fast path vs a strawman baseline; 2C unprototyped + double-homed |

**Pass-1→Pass-2 deltas.** 5A 55→45 (maximal horn moved from SPEC-ONLY to RAN-refuted). 5B 45→40 and
BANKED→RETIRED (un-bank gate RAN, C2 non-bug). 5C 60→45 (no run; CRIT-1 re-cut against the "centerpiece
refuted" prose CRIT-2 wrote but did not price). 5D 35→35 (three suspicions confirmed as diagnoses,
resolution still unbuilt; parent re-homed off the refuted 5A-maximal).
