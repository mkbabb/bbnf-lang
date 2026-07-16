# LOCK-5 — THE REGEX CONTINUUM — FAMILY REGISTRY

Grammar-driven regex engine: the regex HIR truly isomorphic to the bbnf HIR/IR, with
NFA→DFA→minimize→prefilter recast as solver problems where measured to win.

**Status legend.** ADVANCED = active line, earns pass-2 prototype budget. BANKED = sound
but redundant-under-squeeze or absorbed into a sibling; held in reserve behind a named
un-bank/retire gate; not killed. BLOCKED = the family's *maximal claim* rests on a missing
primitive that is as-hard-or-harder than the original problem (the family may survive in a
narrowed form). RETIRED = removed from the portfolio.

**Convergence scoring.** Conservative floor of the two within-lock critics, raised only with
a named reason the critic erred. No such reason was found this pass; every number below is the
critic floor. All pass-1 numbers are **tree-conditional** (see Precondition 1) and must not be
read as earned until one reconciled tree exists to measure in.

---

## PRECONDITIONS (lock-wide, gate every family's earned convergence)

**P1 — the two-tree reconciliation is a precondition, not a footnote.** Two live snapshots
hold incompatible substrate and every pass-1 number silently assumes one of them:

| | skinny `crates/ir` (graduating, skv22) | top-level `crates/ir` (mature egraph) |
|---|---|---|
| regex leaf | `ExprKind::Regex{pattern:String}` — opaque | `IrNode::Regex(StringId)` — interned |
| grammar-tier regex e-graph | **absent** (`backend_egraph.rs` seam only) | `egraph/rules/regex.rs` — **434 LOC, real** |
| classify | 3-pattern stub (`bbnf-regex`, 322 LOC) | re-homed into `passes/materialization/classify.rs` |
| shape | **already 5A-shaped** (nothing for 5B to bridge) | **already 5B-shaped** (the duplicate 5B kills) |

PROTO-1 ran in skinny (the tree that pre-favours 5A); PROTO-2 ran in the parse-that engine.
**No prototype ran in top-level `crates/ir`** — the one tree where 5B's substrate physically
exists. The Axis-1 verdict cannot be earned until GESTALT reconciles the two into one tree of
record. Recommended reconciliation (GESTALT obligation, recorded not resolved here): skinny is
the graduating tree of record; the top-level regex-egraph is the more mature substrate to fold
INTO it, or to abrogate if the graduating tree needs no grammar-tier regex e-graph.

**P2 — the grammar-driven premise (constitution i) is SPEC-ONLY lock-wide.** The lock is named
"grammar-driven regex engine," yet **not one artifact this pass exercised a grammar-driven regex
path.** PROTO-1 hand-translated through the READ-ONLY HIR parser, not a bbnf grammar. `skinny/`
has no `regex.bbnf`; regex is hand-scanned to an opaque `String` (`grammar/src/lib.rs:1561
parse_regex`). LOCK-7 found **two** extant `regex.bbnf` grammars — `parse-that/rust/bootstrap/
regex.bbnf` (generates `generated.rs` whose `parse_generated()` **discards** its AST) and
`grammar/misc/regex.bbnf` (a divergent third home). Constitution clause (i) is SPEC-ONLY, and
5A's task is not greenfield authoring but **promote + de-dup + break the discard**. Every family's
verdict line must carry the SPEC-ONLY(grammar-driven) label until LOCK-7/7A lands.

---

## FAMILY 5A — ONE-HIR — **ADVANCED — 55%**

Regex is the fragment of the bbnf HIR carrying no rule reference and no projection:
`{Seq,Alt,Repeat,Optional,Literal,Class,Look}`. Add exactly two leaves to `ExprKind`
(`Class{set,negated}`, `Look{kind}`) and the regex HIR *is* a subset — one type, one crate,
"nothing to map" (constitution ii maximal reading). Leading Axis-1 candidate.

**What genuinely converged (RAN, favourable).** The tax census is empirical: **13 grammar-plane
`ExprKind` arms, 0 graded REGEX-LEAK**; the type plane (the brief's nominated leak site) reuses
`Literal`'s own `Builtin(Bytes)`, no scalar-byte type needed. Nullability parity oracle **3/3**;
the extant string-regex oracle stays green (no refutation). `cargo build --workspace` GREEN.
These collapse the *"functor-to-keep-passes-clean"* motive for 5B — real, and not disputed.

**Exact open gaps.**
1. **Stale-tree census.** PROTO-1 ran on `d2b32f6e3` (**8 variants**, no `Project`/`Discard`,
   validator still hard-rejects nullable repeats). Graduating HEAD has **10 variants** incl.
   `Project` — precisely where LOCK-1's real type system (widths, decode classes, `->`) lands.
   The 0-leak count is not load-bearing until re-run on HEAD and on the LOCK-1/LOCK-3 target enum.
2. **`Look` consumer-less inversion (Y.13, gate-that-cannot-fail).** Every `Look` arm is trivial
   *precisely because nothing consumes it* (`Look→true`, `→Unit`, `→empty Seq`, `→{}`). The
   "0 leaks" metric structurally **cannot distinguish** carried-and-read (`Class`, whose byte-set
   `first_bytes` reads) from never-read (`Look`). G-5A.1 passes most cleanly exactly when `Look`
   is consumer-less substrate. Must be re-specified so it can *fail*: name a grammar-side `Look`
   consumer (the PT-1.W5 anchored/unanchored distinction) or demote `Look` to `Annotation`.
3. **Clause-(i) grammar totality bypassed.** The parity oracle **hand-built** the three subtrees;
   it never parsed regex text through a bootstrapped `regex.bbnf`. The production half (regex text
   → `ExprKind` via self-hosted grammar) — 5A's whole grammar-totality claim and its bootstrap
   circularity — is untested. Premise now *contradicted* by two extant divergent grammars (P2).
   SPEC-ONLY-until-7A; re-scope from "author" to "promote+de-dup+break the discard."
4. **Automata parity (G-5A.3) UNRUN.** No `Dfa::find` from an `ExprKind`-built NFA vs the vendored
   `Hir`→NFA path; `from_hir → from_expr` not built; PROVE-10's 8.8–29.5× band not reproduced from
   the merged type. "The ONE type hosts the whole engine at frozen numbers" is SPEC-ONLY.
5. **Classify re-home is the elegant-reduction trap.** "classify becomes an IR analysis pass"
   re-expresses a **1148-LOC** classifier structurally; no prototype touched it.
6. **Eq/Hash-vs-span god-type (G-5A.5) untested** (saturation cache needs `Eq+Hash`; IR needs
   arena/`ExprId`/span/`Serialize` — one type must serve both without a god-type). **UTF-8
   placement (Q2) untested** (`[α-ω]*` byte-expansion confined to automata, or Unicode leaks into
   the shared enum).
7. **Not validated against the top-level `IrNode`/`GrammarENode` tree** — the merge collision with
   the existing `derive(Language)` + 434-LOC copy (P1).

**Pass-2 directives.**
- *Must answer:* re-run the tax census on graduating HEAD (10 variants) **and** the LOCK-1/LOCK-3
  target enum, re-grading `infer_expr(Class)` under real `Project` typing; route the `Class`/`Look`
  typing verdict through LOCK-1 (not skinny's stub `BuiltinTy`).
- *Must prototype:* G-5A.3 automata-from-`ExprKind` byte-identical to the vendored path, then
  reproduce PROVE-10's band from the merged type; a `Look`-consumer test that can *fail*.
- *Withhold from researchers (anti-anchoring):* the "5A dominates / functor unneeded" verdict and
  the inherited PROVE-10 numbers — force G-5A.3 to be *re-run* from the merged type, not cited.

---

## FAMILY 5D — GRAMMAR-FRAGMENT — **BANKED (into 5A) — 35%**

A `/pat/` desugars once into native `ExprKind`; no separate engine on the generator path; the DFA
survives as one opt-in lowering target chosen by a one-pass check. **Folded into 5A** under the
portfolio "share-a-mechanism, share-a-family" rule: both add a byte-class leaf and dissolve the
opaque string; they differ *only* on lowering policy. BANKED, not RETIRED — the fold absorbs the
mechanism but 5D retains a live decisive number that fixes 5A's lowering policy.

**Maximal reading RETIRED.** "No engine on the generator path" is refuted by PROVE-10 (DFA +
start-state memchr prefilter, 8.8–29.5×, 44/44) and the scannerless-slowdown literature. The
surviving lawful 5D is a **byte-recognizer that STILL compiles to the same DFA+prefilter, carrying
`->` projections on its transitions (Ragel transition-action precedent), one lowering per site
(GN-2.W1 interlock)** — the underexplored formulation opened in 5D's place (see Redirect-crowding).

**Exact open gaps (none measured — the fold must not launder 5A's earned tax result here).**
1. **G-5D.4 one-pass fraction — THE decisive number, UNMEASURED.** Neither prototype ran
   `check_one_pass_eligible` over the corpus. It alone decides whether "no engine" holds vs "the
   DFA stays load-bearing." The synthesis's "fold, settle later" is a deferral, not a resolution.
2. **G-5D.3 throughput head-to-head UNMEASURED.** Straight-line class scan vs the PROVE-10
   prefiltered `Dfa::find`, cold-paired, `count_a==count_b`. research-D R6 itself concedes "if it
   merely ties, report a tie" — even the tie is unmeasured.
3. **FOLLOW-overlap predicate adequacy self-flagged.** `check_one_pass_eligible` tests FIRST-set
   disjointness across *alternations*; research-D R2 asks whether it also rejects a `Repetition`
   whose class overlaps its own FOLLOW (`[a-z]+z` on `"abz"`, greedy give-back). Named, never
   closed. 5D routes on a predicate not yet verified sound for its own use.
4. **Masked-fallback contradiction.** PROTO-1 celebrates that `derive_recognizers` "absorbed
   `Class`/`Look` via its existing `_ => {}` wildcard, no edit." But `derive_recognizers` is the
   *recognizer-selection* pass — the very site 5D needs to choose `Recognizer::SimdScan` over
   `ByteClass` shapes. A wildcard silently swallowing `ByteClass` means 5D's SIMD-fast-path chooser
   **is not firing**. PROTO-1's "0-edit win" and 5D's "recognizer chosen by IR analysis" are
   mutually contradictory on the same line of code.
5. **`BackendExpr::ByteClass` emit unverified** (consumer-less risk); `regex_desugar` never built,
   so G-5D.1 (zero `ExprKind::Regex` survive) is asserted from the 5A merge, not run.

**Pass-2 directives.**
- *Must answer:* measure G-5D.4 (one-pass fraction over the engine corpus + real leaves) — the
  single number that draws 5A's lowering-policy line.
- *Must prototype:* G-5D.3 cold-paired class-scan vs prefiltered `Dfa::find`; close the R2
  FOLLOW-overlap gap; prove `derive_recognizers` actually *selects* on `ByteClass` (retire the
  masked wildcard).
- *Withhold:* the "one-pass fraction is high / straight-line beats the DFA" prior — measure both
  fresh; a tie is a tie.

---

## FAMILY 5B — LAWFUL-FUNCTOR — **BANKED — 45%**

Two domain-fit HIRs joined by a total functor `F ⊣ G` through the shared e-graph; the win is
*write-the-regex-algebra-once* and delete the duplicate grammar-tier rules. Architecturally sound,
every primitive cited-working (MLIR dialect conversion, egg/egglog, Galois connection). BANKED for
**convergence-crowding + value-target-absent-from-the-graduating-tree**, held behind a decisive
un-bank/retire gate — NOT retired, because eliminating it by running 5A's test is vacuous.

**Why BANKED, not ADVANCED.**
1. **Converges with 5A toward one mechanism.** Once bbnf grows `Class`/`Look` (PROTO-1 showed this
   is trivial), the 5A/5B difference collapses to "one type" vs "two types + a near-trivial functor."
   5B's own risk register concedes `Class`/`Look` cross only as *atomic* nodes, so the shared-
   optimization payoff reaches only `Concat/Alt/Repeat/Literal/Empty` — **precisely the fragment 5A
   unifies for free.** If coverage is high, 5A is simpler; if low, the functor buys little. Squeezed
   from both sides.
2. **The value target is absent from the graduating tree (P1).** 5B's raison d'être is deleting the
   434-LOC duplicate rules that live in **top-level**. The skinny (graduating) tree has **no
   grammar-tier regex e-graph** — nothing to bridge, no duplicate to kill. Its entire mechanism
   presupposes substrate skv22's tree of record lacks.

**Why NOT RETIRED (vacuous-convergence guard).** PROTO-1 declared 5B "unneeded" having run **0 of
5B's own gates**. G-5B.2 (does the 434-LOC copy actually die when the algebra is written once?) —
the load-bearing value question — was never probed; the named counterexample (`FuseAltRegexBranches`
mixing `Regex`+`Literal` grammar leaves) was never built. The flagship "already-ships" precedent is
a category slip: `translate.rs` is a **same-domain** `Hir↔HirENode` translator; 5B needs a
**cross-domain** `GrammarENode↔HirENode` functor that does not exist. "5A has 0 leaks" and "5B's
write-once value materializes" are different questions.

**Exact open gaps.** (a) cross-domain `insert_bbnf`/`extract_bbnf` never built + round-trip oracle
never run; (b) G-5B.2 rule-death untested — the number deciding 5B-vs-5A; (c) span/projection
survival under `F∘G` unmeasured (the true "reasonable point" is still asserted, not numeric); (d)
generate-axis cost vs `analyze()` unmeasured (clause-v neutrality unbacked); (e) value-target
absent from graduating tree unresolved (P1); (f) LOCK-2 Island A asks whether the fleet even *has*
a rich regex leaf — if not, the shared-rewrite value is a LOCK-5-owned negative.

**Un-bank / retire gate (MANDATORY pass-2).** Run G-5B.2 in a **named tree**: build
`FuseAltRegexBranches` + the cross-domain functor in the tree that actually holds the 434-LOC copy.
- If the copy dies when algebra is written once **and** the reconciled tree keeps a grammar-tier
  regex e-graph → 5B **un-banks to ADVANCED**.
- If the copy is absent or dies trivially under 5A's merge in the graduating tree → 5B → **RETIRED**.
Until this runs, "5B unneeded" is struck from every verdict line.

**Underexplored formulation opened in 5B's place (redirect-crowding).** The **joint coproduct
e-graph** — `RegexOrGrammar = Left(HirENode) | Right(GrammarENode)`, `F`/`G` as bridge rewrites
folded away before emission (the literal MLIR `unrealized_conversion_cast` transcription). This is
5B's strongest form, distinct from the weak same-domain `translate.rs` precedent, and the actual
vehicle a reconciled two-tree world would need *if* it turns out to require cross-domain rewriting
(rules that fuse regex+literal leaves). Opening it gives 5B a real, distinct thing to prove rather
than re-running the pairwise-functor question 5A already dominates.

**Pass-2 directives.**
- *Must answer:* does the grammar-side rule copy die (G-5B.2), in the tree that holds it?
- *Must prototype:* `FuseAltRegexBranches` + cross-domain functor round-trip oracle (strict off,
  canonical on, DFA-equivalence) on the top-level tree; OR the joint-coproduct-egraph if the
  reconciled tree keeps a regex e-graph.
- *Withhold:* PROTO-1's "5B unneeded" conclusion and 5A's 0-leak framing — hand researchers ONLY
  the G-5B.2 value task on a named tree, so value is measured without the elimination prior.

---

## FAMILY 5C — AUTOMATA-AS-EXTRACTION — **ADVANCED — 60%**

Push the shared e-graph one level below the HIR, to the automaton. Orthogonal to Axis-1 — rides
*below* whichever type-plane family wins, consuming its HIR and producing its DFA. Best-evidenced
family this pass (PROTO-2 RAN & MEASURED, both halves). Three operations, three sub-verdicts:

**2A minimization-as-congruence — EXACT, but the "GO" is narrower than the synthesis stated.**
- Correctness: **26/26 byte-identical** minimized state count + language-equivalent, cyclic
  included. EXACT.
- Perf: worklist-2A **3.53× SLOWER** whole-corpus — **NO-GO as a wholesale replacement.** Only the
  acyclic-congruence fast path ships (**0.80×, 15% of patterns**), and even that "needs the
  alloc-light rewrite to matter" (unbuilt, out of probe scope). FAMILY-SPECS' "2A GO (a landing
  minimizer)" **rounds up**; the honest reading is "2A-general NO-GO; a 15%-coverage acyclic
  dispatch fast path, contingent on an unbuilt rewrite."
- **The win is against a strawman.** The incumbent `hopcroft_minimize` is naive Moore + global
  restart (`dfa.rs:358`, O(n²·k)) — **not** Hopcroft. Research-C P1 required comparison vs "(b) a
  reference O(n·k·log n) Hopcroft"; PROTO-2 compared only vs incumbent (a). The crossover vs a
  *correct* Hopcroft — the number deciding whether any of this beats just *fixing the restart* — is
  UNMEASURED. The null alternative (delete `:358`) was never priced.

**2B determinization-as-derivative-saturation — BLOCKED (as minimizer), cleanly confirmed.** The
lock's cleanest result. ρ_max = **1.667**, with an **organic corpus miss** `\w+(?:[-_]\w+)*`
(ρ=1.5) and a direct witness (`simplify_hir((ab)*a) ≠ simplify_hir(a(ba)*)`). Regex equivalence is
PSPACE-complete (Meyer–Stockmeyer); no finite rewrite set is complete (Kozen schematic) — a
**provably-impossible missing primitive**, not merely "as hard as the original." **2B-as-minimizer
is BLOCKED.** 2B-as-*determinizer* survives (produce a possibly-non-minimal DFA, close with 2A);
25/26 corpus patterns ρ=1 corroborates engine correctness.

**2C prefilter/tier-selection CSP — UNPROTOTYPED, "clean CSP GO" unearned + double-homed.** The one
part of 5C that could positively *ship* was never built (PROTO-2 covered 2A/2B only). The family's
own honest boundary: absent a demonstrated *global* constraint, the verdict is the **pluggable
cost-sorted table**, not the CSP. Double-homed with LOCK-2's scan-plan CSP (must be single-homed).

**Exact open gaps.** (1) reference-Hopcroft control (research-C P1(b)) UNRUN — G-5C.1 unmet; (2)
"just delete the `:358` restart" null unpriced — if fixing the incumbent erases 2A's win, the
shippable sliver has no consumer; (3) FAMILY-SPECS §5C overstates "2A GO"; (4) 2C unprototyped —
run the global-constraint witness or downgrade to "pluggable table"; (5) corpus is **26**, gate
cites **50** — G-5C.1 unsatisfiable as written (re-cut to 26 or grow the corpus); (6) the
15%-slice 0.80× excludes the `is_acyclic` dispatch cost on the other 85% — net benefit after
dispatch unmeasured, possibly negative; (7) G-5C.5 determinism/tie-break for regen bit-identity
unexercised (2C not built).

**Pass-2 directives.**
- *Must answer:* does the acyclic fast path beat a *correct* O(n·k·log n) Hopcroft (not the restart
  strawman)? Does deleting `dfa.rs:358` erase 2A's entire reason to exist?
- *Must prototype:* the reference-Hopcroft control + the null "delete-the-restart"; 2C tier CSP with
  a global-constraint witness (shared SIMD-LUT/codegen budget) or its downgrade to a table; fold the
  `is_acyclic` dispatch cost into the head-to-head.
- *Withhold:* the incumbent restart-Moore as "the" baseline (force the reference-Hopcroft control),
  and 2C's "clean CSP GO" framing (make table-vs-CSP be earned, not assumed).

**Family status.** ADVANCED. The maximal claim (e-graph automata beats the classical pipeline at
engine scale) is effectively refuted — the honest deliverable is a 15%-applicable contingent fast
path + 2B's named theory bound + 2C's to-run adjudication. 2B-as-minimizer carries an internal
BLOCKED marker; the family is not blocked (2A/2C ship independently of 2B).

---

## REDIRECT-CROWDING DECISIONS

- **5A ⊕ 5D → one spine (ADVANCED 5A + BANKED 5D).** They share the mechanism (byte-class leaf,
  dissolve the opaque string); they differ only on lowering policy. 5D banked into 5A; its maximal
  "no engine" reading RETIRED (PROVE-10); its decisive one-pass number becomes 5A's lowering-policy
  sub-question. Underexplored formulation opened in its place: the **Ragel transition-action
  byte-recognizer** that carries `->` projections and still compiles to DFA+prefilter.
- **5B BANKED for convergence with 5A** (win reaches only the fragment 5A unifies for free) **+
  value-target absent from the graduating tree.** Held behind the mandatory G-5B.2 un-bank/retire
  gate. Underexplored formulation opened in its place: the **joint coproduct e-graph** (bridge
  nodes folded pre-emission) — 5B's strongest, distinct-from-`translate.rs` form.
- **5C not crowded** — orthogonal (rides below the Axis-1 winner), not competing with any Axis-1
  family. Stays ADVANCED on its own evidence.

---

## CROSS-POLLINATION (named grafts only, both sides developed)

- **GRAFT-1 (justified): 5A's single derived `Language` hosts 5C-2B-determinizer's saturation
  theory.** 2B reuses the HIR-level five rewrite rules as its equivalence theory; if 5A ships one
  `derive(Language)` on `ExprKind`, 2B's Brzozowski-derivative determinization saturates over that
  same Language and 2A closes the minimization. Both sides developed (5A tax census RAN; 5C-2B RAN
  with a clean measured bound). The graft: **5A's merged Language IS the substrate 2B determinizes
  over; 2A is the closer.** This is the constitution's "automata construction consumes the IR
  e-graph" realized end-to-end.
- **GRAFT-2 (consequential, mechanical): 5C-2A's byte-class partition consumes 5A's `Class`
  byte-sets.** `sets/equiv.rs` partition for `succ` width reads the `ByteSet256` the `Class` leaf
  already carries — a natural consequence of 5C riding on 5A's HIR, not a separate build.
- **WITHHELD (not yet developed enough to graft): 5D's structural recognizer chooser × 5C-2C tier
  CSP.** Both feed PROVE-10's prefilter seam, but 5D is BANKED-35% and 2C is unprototyped — neither
  is developed enough for the graft to be named. Recorded as a deferred coupling, not an active graft.

---

## CROSS-LOCK DOUBLE-HOMES (constitution ix — LOCK-5 is the hub; it must name these back)

1. **5A/5D ⊗ LOCK-9 9C (SCANNER-AS-DFA).** 9C is "BLOCKED-candidate (cross-lock-coupled to
   LOCK-5)"; it sources `bootstrap/regex.bbnf` and extends the same `classify.rs RegexClass` 5A/5D
   re-home. LOCK-9 defers; LOCK-5 must reciprocate. **Supersession: LOCK-5 owns the
   HIR→DFA→classify substrate; 9C consumes it or retires.**
2. **5C-2C ⊗ LOCK-2 scan-plan/tier CSP.** LOCK-2 defers Island A (regex HIR canonicalization) to
   LOCK-5 but *also* claims the tier CSP (`SimdMode×SimdSite×kernel` + prefilter cost model). Same
   tier-selection CSP, two homes. **Single-home the prefilter/tier CSP before either converges.**
3. **5A/5D ⊗ LOCK-3 IR tower.** 5A/5D mutate `ExprKind`/`BackendExpr` — LOCK-3's tower. LOCK-3
   makes 3C BLOCKED-candidate until LOCK-5 rules on the functor. **LOCK-5's enum edits are
   LOCK-3-governed; the typed-materialization invariant applies to the new leaves — `ByteClass`
   must reach the tape emitter.**
4. **5A ⊗ LOCK-7 bootstrap.** Premise contradicted: two extant `regex.bbnf` (one discards output,
   one divergent). **Re-scope 5A from "author regex.bbnf" to "promote + de-dup + break the discard";
   SPEC-ONLY(bootstrap) until 7A.**
5. **5A/5B ⊗ LOCK-4 4D (GRAMMAR-SUGAR).** 4D is contingent on LOCK-5 choosing ONE-HIR; reopening
   5B weakens 4D. **LOCK-5's Axis-1 choice is load-bearing for LOCK-4-4D, LOCK-9-9C, LOCK-3-3C —
   three downstream locks blocked on a verdict currently asserted, not earned.**
6. **Type-plane ⊗ LOCK-1.** `Class→Builtin(Bytes)` was graded against skinny's stub `BuiltinTy`.
   **Route the `Class`/`Look` typing verdict through LOCK-1's real type system (HM + row-poly +
   widths), not the skinny stub.**
7. **rust/ts isomorphism ⊗ LOCK-6.** The rust/ts face of the merged/functored regex HIR (how
   `Class`/`Look`/`ByteClass` + automata cross to TS, PROVE-14 tape) is unaddressed. **Named gap for
   GESTALT-Ω: if 5A merges regex into `ExprKind`, LOCK-6's mirror/skeleton must carry the merged
   enum.**

---

## PASS-2 DIRECTIVES (consolidated, ordered by what gates the most downstream)

1. **Resolve P1 first** — GESTALT must produce one reconciled tree; every family re-measures there.
   No earned convergence until it exists.
2. **Run 5B's G-5B.2** (`FuseAltRegexBranches` + cross-domain functor) in the tree holding the
   434-LOC copy — un-bank or retire 5B. Until then "5B unneeded" is struck.
3. **Measure 5D's one-pass fraction (G-5D.4)** and the class-scan-vs-prefiltered-`Dfa::find`
   throughput (G-5D.3); close the R2 FOLLOW-overlap gap; retire the `derive_recognizers` masked
   wildcard by proving it selects on `ByteClass`.
4. **Re-run 5A's tax census on graduating HEAD (10 variants) and the LOCK-1/LOCK-3 target enum**;
   route `Class`/`Look` typing through LOCK-1; re-specify the `Look`-consumer gate so it can *fail*
   (name a consumer or demote to `Annotation`).
5. **Build 5A's automata parity (G-5A.3)** — `from_expr` NFA byte-identical to the vendored path;
   reproduce PROVE-10's band from the merged type (do not inherit the number).
6. **Run 5C's reference-Hopcroft control (P1(b)) and the "delete-the-`:358`-restart" null**; correct
   FAMILY-SPECS §5C to "2A-general NO-GO"; build 2C (tier CSP) with a global-constraint witness or
   downgrade to a table; single-home the tier CSP against LOCK-2.
7. **Name every cross-lock double-home** (9C, LOCK-2 tier CSP, LOCK-3 enum, LOCK-7 bootstrap, LOCK-4
   4D, LOCK-6 TS face, LOCK-1 typing) as an explicit supersession/interlock in the pass-2 registry.
8. **Carry the SPEC-ONLY(grammar-driven) label** on every verdict line lock-wide until 7A lands (P2).

---

## PER-PASS HISTORY

### Pass 1 (research-{A,B,C,D,priorart} → FAMILY-SPECS → PROTO-1, PROTO-2 → CRIT-1, CRIT-2 → this)

| Family | Status | % | One-line |
|---|---|---|---|
| 5A ONE-HIR | ADVANCED | 55 | Tax+nullability RAN & favourable, but stale/pre-favouring tree; bootstrap, automata parity, `Look`-consumer, classify re-home, UTF-8, god-type all SPEC-ONLY |
| 5D GRAMMAR-FRAGMENT | BANKED (into 5A) | 35 | Maximal "no engine" reading RETIRED by PROVE-10; folded as 5A's lowering policy; decisive one-pass fraction + throughput UNMEASURED; masked-wildcard contradiction |
| 5B LAWFUL-FUNCTOR | BANKED | 45 | Sound but converges with 5A + value-target (434-LOC copy) absent from the graduating tree; eliminated-without-testing (0/5 gates); held behind the G-5B.2 un-bank/retire gate |
| 5C AUTOMATA-AS-EXTRACTION | ADVANCED | 60 | 2B-as-minimizer BLOCKED (ρ_max=1.667, cleanly confirmed); 2A EXACT but 3.53× slower, 15% acyclic fast path vs a strawman baseline; 2C unprototyped + double-homed |

**Prototypes run this pass.** PROTO-1 (5A tax census + nullability parity, RAN, GREEN, on stale
`d2b32f6e3`). PROTO-2 (5C 2A worklist minimizer + 2B derivative saturator, RAN & MEASURED,
cold-paired N=500). Refutation-law compliance clean where measured (no warm benches, no shim perf
claims, READ-ONLY scratch-copy). The gap is what did NOT run: 5B (whole), 5C-2C, 5D's decisive
numbers, 5A's automata/bootstrap/classify — all SPEC-ONLY and now labelled as such.

**Axis-1 status: NOT resolved.** The synthesis's "0 leaks ⇒ 5A dominates, functor unneeded" is a
one-sided cost census converted into a two-sided architecture verdict (vacuous convergence). 5A
leads on the tax axis; the write-once axis (5B's value) is untested; the two-tree reconciliation is
the precondition that fixes which tree the decision is even made in.
