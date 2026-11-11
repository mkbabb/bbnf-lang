# LOCK-5 — THE REGEX CONTINUUM — pass-1 · CRIT-1 (adversarial, WITHIN-LOCK weight)

Critic 1 of 2. Charge: harden every family against the failure-mode checklist (vacuous
convergence, spec-cites-itself, gates-that-cannot-fail, the elegant-reduction trap,
legacy aliases, masked fallbacks, unverified gestalt, consumer-less substrate) + the
constitution clauses (grammar totality, isomorphism, refutation laws, zero-runtime-cost,
parsimony, SPEC-ONLY discipline) + prototype-evidence misuse. Critic-1 lens: internal
soundness of each family spec, and whether the two prototypes actually tested the sharpest
uncertainty each family lives or dies by. Per-family convergence % + exact enumerated gaps
below; verdict rows carry a WITHIN-LOCK reading, not a GESTALT one.

---

## 0. THE STRUCTURAL FLAW THAT DISCOUNTS EVERY FAMILY: two prototypes, two trees, and NEITHER is the tree where the contested family is real

The synthesis's own §0 "load-bearing terrain fact" is more damaging than it admits. There
are two live repo snapshots:

- **skinny tree** (`skinny/crates/ir`): regex is `ExprKind::Regex { pattern: String }`
  (opaque string), classify is a 3-pattern stub. This tree is **already 5A-shaped** — it
  has no functor, no GrammarENode regex rules, nothing for 5B to bridge.
- **top-level tree** (`crates/ir`): regex is `IrNode::Regex(StringId)` (interned),
  `GrammarENode` derives `Language`, and `crates/ir/src/egraph/rules/regex.rs` is a **real
  434-LOC grammar-tier regex rule set** (verified this pass). This tree is **already
  5B-shaped** — it holds the duplicate rules whose *death* is 5B's entire value prop.

**PROTO-1 ran in skinny. PROTO-2 ran in the parse-that engine. NO prototype ran in
`crates/ir`** — the one tree where 5B's substrate physically exists. So:

1. **5A was measured in the tree where its answer was pre-ordained.** "Extend an
   opaque-string enum, 0 leaks" is trivially favourable in a tree that already lacks the
   competing substrate. The census did not test 5A against the mature regex-egraph tree
   (top-level `IrNode`/`GrammarENode`), where the merge collides with an *existing*
   `derive(Language)` on the grammar node + a 434-LOC rule copy — exactly the collision
   G-5A.5 (Eq/Hash-vs-span) and 5B's Q1 (does the copy die) are about.
2. **5B could not have been tested and was not.** Its raison d'être (write-algebra-once,
   the 434-LOC copy is deletable — G-5B.2) is *untestable in skinny* (the copy isn't
   there) and *untested in top-level* (no prototype ran there). Zero of 5B's five
   "prototype-must-test" items executed.

Consequence for adjudication: the Axis-1 verdict the synthesis declares "collapsed by a
single census+oracle" (FAMILY-SPECS line 337, and Brief-1's pass/fail) is **not actually
resolved** — see §2. Every convergence % below is *tree-conditional*; the §0
reconciliation deferred to GESTALT is not a bookkeeping note, it is a precondition the
pass-1 numbers silently assume away.

---

## FAMILY 5A — ONE-HIR — convergence 55%

**What genuinely converged (credit where due).** The tax census RAN and is empirical: 13
grammar-plane `ExprKind` arms, **0 graded REGEX-LEAK**, plus 6 mechanical backend arms 5A
and 5B incur identically. The nullability parity oracle RAN: 3/3, extant string-regex
oracle stays green (no refutation). These are two real, favourable, non-SPEC results — the
family's strongest evidence and the reason it leads Axis 1.

**But the prototype tested the EASY half and hand-waved the hard half (elegant-reduction
trap).** Research-A itself lists five prototype-must-test items; PROTO-1 ran **two** (tax +
nullability). The three unrun are the ones that prove the architecture end-to-end:

- **G-5A.1 gap — the census ran on a STALE, STRIPPED enum.** PROTO-1 (self-disclosed)
  built against commit `d2b32f6e3`, where `ExprKind` "has 8 variants; no Project/Discard;
  validator still hard-rejects nullable repeats." Verified: current HEAD `ExprKind` has
  **10 variants** including `Project` (:261) and `Discard` (:266). The type-plane arm
  (`infer_expr`, the brief's nominated leak site) was therefore tested with the
  **projection machinery absent** — and `Project` is precisely where LOCK-1's real type
  system (widths, decode classes, `->` projections) lands. "Class → Builtin(Bytes),
  trivial" is a result about a type plane that does not yet have its type system. The
  0-leak count is not load-bearing until re-run on HEAD, and again on the LOCK-1/LOCK-3
  target enum. As it stands the decisive number was measured against a floor version of
  the very plane whose leakage is the question.

- **Clause-(i) grammar totality is SPEC-ONLY — the parity oracle bypassed the grammar.**
  PROTO-1 **hand-built** the three `ExprKind` subtrees ("mirroring the real HIR"). It did
  NOT parse them through a bootstrapped `regex.bbnf` (which research-A confirms *does not
  exist* — "a to-BUILD deliverable"). So G-5A.2's clause — "the three JSON leaves *parsed
  through regex.bbnf*" — was never exercised; only "if you already have the right tree,
  nullability is correct" was shown. The clause-(i) half (regex text → `ExprKind` via the
  bootstrapped, self-hosted grammar) — 5A's whole grammar-totality claim and its hard
  bootstrap circularity — is untested. G-5A.4 (regen∘regen fixpoint) is self-declared
  **SPEC-ONLY-until-7A**. So 5A's "identity, nothing to map" (clause ii maximal reading)
  is proven for the *analysis* of a hand-built tree, not for the *production* of that tree.

- **G-5A.3 automata parity UNRUN — the engine was never re-homed.** No `Dfa::find` from an
  `ExprKind`-built NFA vs the vendored `Hir`→NFA path; parse-that's `Hir` still exists;
  `from_hir → from_expr` was not built; the PROVE-10 band (research-A item 3, "the
  strongest positive result available") was not reproduced. The claim "the ONE type hosts
  the whole engine at frozen numbers" is SPEC-ONLY.

**Consumer-less-`Look` (Y.13) is UNRESOLVED and the census data points the WRONG way — a
gate that rewards the violation.** Every `Look` arm PROTO-1 reports is pure pass-through:
`nullability Look→true`, `lower_expr Look→empty Seq`, `first_bytes Look→transparent`,
`mark_transitive Look→{}`. That is the *signature of a consumer-less leaf*: it is trivial
in every pass precisely because nothing reads it. The metric "0 regex-leak arms" **cannot
distinguish** "carried structurally and read" (`Class` — `first_bytes` reads its
byte-set, a real consumer) from "never read" (`Look`). So G-5A.1 **passes most cleanly
exactly when `Look` is consumer-less substrate** (constitution viii). The gate is
mis-aimed at the wrong risk: it certifies "grammar passes stay clean" while being blind to
"the new leaf has no grammar consumer." Q3 is not answered; the evidence gathered
*corroborates the concern* the synthesis filed as open.

**Q2 UTF-8/codepoint placement (`[α-ω]*` byte-expansion blowup) UNRUN** (research-A item 4).

**Convergence rationale.** Two strong empirical results, but on a stale tree, in the tree
that pre-favours 5A, with clause-(i) bootstrap + automata parity + Look-consumer + UTF-8
all SPEC-ONLY. The load-bearing merge (retire parse-that `Hir`, run automata from
`ExprKind`) has not been demonstrated once. 55%.

**Exact open gaps (5A).**
1. Re-run the tax census on HEAD (10-variant `ExprKind`) and on the LOCK-1/LOCK-3 target
   enum with `Project`/`Discard`/full type system present; re-grade `infer_expr` for
   `Class` under real projection typing. Until then the 0-leak count is stale.
2. Build `regex.bbnf`, parse the three leaves *through it* into `ExprKind`, re-run the
   parity oracle — close the clause-(i) grammar-totality half PROTO-1 bypassed.
3. Run G-5A.3: `Dfa::find` from `ExprKind`-built NFA byte-identical to the vendored
   `Hir`→NFA path; reproduce PROVE-10's 8.8–29.5× band from the merged type. Currently
   SPEC-ONLY.
4. Resolve Q3 with a metric that can *fail*: name a grammar-side consumer of `Look` (the
   PT-1.W5 anchored/unanchored distinction) or demote to `Annotation`. The current
   "0 leaks" gate structurally cannot detect consumer-less `Look`.
5. G-5A.4 bootstrap fixpoint + Q2 UTF-8 placement — both unrun; label SPEC-ONLY-until-7A
   explicitly in the registry.
6. Re-validate 5A against the top-level `IrNode`/`GrammarENode` tree (the merge collision
   with the existing `derive(Language)` + 434-LOC copy), not only skinny.

---

## FAMILY 5B — LAWFUL-FUNCTOR — convergence 45%

**5B is entirely SPEC-ONLY and the synthesis does not label it so (constitution vii
violation in the synthesis itself).** Zero of research-B's five prototype-must-test items
ran (round-trip oracle, nullability parity through `Analysis<HirENode>`, span-survival
fraction, generate-axis cost, sublattice-membership decidability). Every 5B gate (G-5B.1–4)
is presented in FAMILY-SPECS as measurement-ready without the SPEC-ONLY tag that clause
vii demands for a family with no running prototype.

**The cited "already ships" precedent is a category slip (spec-cites-a-weaker-thing).**
5B's headline evidence is that `parse-that/rust/regex/src/egraph/translate.rs` "already
ships the functor, proven at engine scale." Verified: `translate.rs` is a **within-domain**
`Hir ↔ HirENode` translator (owning-tree ↔ e-node, same regex domain). 5B's *actual*
deliverable is a **cross-domain** functor `GrammarENode ↔ HirENode` (`insert_bbnf` /
`extract_bbnf`) that does NOT exist and is NOT what `translate.rs` does. The top-level
`crates/ir/src/egraph/` has `node.rs`/`rules`/`cost.rs`/`build_egraph.rs`/`write_back.rs`
but **no `translate.rs`** (verified). So the "precedent already ships" claim conflates the
easy same-domain translator with the unbuilt cross-domain functor — the elegant-reduction
trap in citation form.

**5B's own value prop was never on the test.** Brief-1 measured 5A's *cost* (tax census);
it did not measure 5B's *value* (G-5B.2: does the 434-LOC grammar-side copy actually die
when the algebra is written once?). The counterexample research-B/synthesis both name —
`FuseAltRegexBranches` mixing `Regex` and `Literal` grammar leaves, i.e. a rule that needs
the grammar context and so might force `GrammarENode` to keep its own impls — was never
built. The 434-LOC copy that would die lives in top-level `crates/ir` (verified present),
a tree no prototype touched. So 5B's central claim is neither confirmed nor refuted.

**Therefore Axis 1 is NOT resolved (vacuous-convergence flag on the synthesis).** The
synthesis asserts "Brief-1 collapses Axis 1: 0 leaks ⇒ 5A/5D dominate, functor unneeded."
This is an asymmetric ledger fallacy: showing 5A is *cheap* does not show 5B is
*valueless*. 5B's justification was never "keep grammar passes clean" (5A already does
that); it was "write the regex algebra once across the repo boundary and delete the
duplicate." That question sits entirely untested. Axis 1 remains genuinely open on the
write-once axis, and the Brief-1 pass/fail is a gate that reads decisive while testing
only half the decision.

**Convergence rationale.** The architecture is standard and sound per priorart (MLIR
dialect conversion, egg/egglog, Galois-connection framing are real, cited, working
precedents), and the shared e-graph crate genuinely is cross-repo. But nothing ran, the
flagship precedent doesn't do what 5B needs, and the one experiment that supposedly retired
5B tested 5A's cost instead of 5B's value. 45% — a coherent unbuilt spec whose decisive
uncertainty is untouched.

**Exact open gaps (5B).**
1. Build `insert_bbnf`/`extract_bbnf` (the *cross-domain* functor, not `translate.rs`) in
   the top-level `crates/ir` tree and run the round-trip oracle (saturation-off strict,
   saturation-on canonical, DFA-equivalence) over the 44-test corpus. Nothing here ran.
2. Build the `FuseAltRegexBranches` counterexample (G-5B.2 / Q1): does the 434-LOC
   grammar-side copy die, or does `GrammarENode` still need its own impls? This is the
   number that decides 5B-vs-5A, and it is unmeasured.
3. Label 5B **SPEC-ONLY** in the registry until (1) runs — clause vii.
4. Span/projection survival fraction under F∘G (side-table vs erasure) — unmeasured;
   "reasonable point" is still asserted, not numeric.
5. Generate-axis cost (functor-through-e-graph vs `analyze()` string re-parse), cold
   paired, cache warm/cold — unmeasured; the clause-v neutrality claim is unbacked.
6. Correct the synthesis's Axis-1 claim: Brief-1 measured 5A cost, not 5B value; Axis 1 is
   not collapsed.

---

## FAMILY 5C — AUTOMATA-AS-EXTRACTION — convergence 60%

**Best-prototyped family: PROTO-2 ran both halves, measured, gated. Genuine credit.** The
2B verdict is the cleanest result in the lock.

**2B: BLOCKED CONFIRMED — sound and well-evidenced.** ρ_max = 1.667, with an *organic
corpus* miss at `\w+(?:[-_]\w+)*` (ρ=1.5), and a direct witness (`simplify_hir((ab)*a)` ≠
`simplify_hir(a(ba)*)`). The PSPACE-completeness / Kozen-schematic argument is correctly a
*provably-impossible missing primitive*, not merely "as hard as the original." This is a
model negative result — a real BLOCKED-candidate confirmed by measurement, not asserted. No
gap here beyond corpus breadth.

**2A: the synthesis verdict "GO (a landing minimizer)" is OVERSTATED vs PROTO-2's own
numbers.** PROTO-2: worklist-2A general is **3.53× SLOWER — NO-GO** as a replacement. Only
the acyclic-congruence fast path ships, at 0.80×, covering **15% of patterns** — and even
that "needs the alloc-light rewrite to matter" (unproven, out of probe scope). FAMILY-SPECS
§5C row calls this "2A GO (a landing minimizer)"; the honest reading is "2A-general NO-GO;
a 15%-coverage acyclic dispatch fast path, contingent on an unbuilt alloc-light rewrite."
The verdict line in PROTO-2 is honest; the FAMILY-SPECS summary rounds it up.

**2A's win is against a STRAWMAN, and the research-demanded control was not run.** The
incumbent 2A beats is `hopcroft_minimize` which is **not Hopcroft** — it is naive
Moore-refinement with a global restart (`dfa.rs:358`, O(n²·k)). Research-C P1 explicitly
required comparison vs "**(b) a reference O(n·k·log n) Hopcroft**." PROTO-2 compared only
vs incumbent (a). So the acyclic-congruence 0.80× is 0.80× *of a broken baseline*; the
crossover vs a correct Hopcroft — the number that says whether any of this is worth
building over just *fixing the restart* — is UNMEASURED. G-5C.1 ("compile-time ratio + the
small/large crossover vs real-Hopcroft") is unmet. Note the cheapest alternative the
family never priced: delete the `:358` restart (turn the incumbent into real Moore/Hopcroft)
— that may erase 2A's entire reason to exist and was not measured.

**2C: entirely UNPROTOTYPED — the "clean CSP GO" is unearned.** PROTO-2 covered 2A and 2B
only. Research-C's P3 (tier-selection CSP vs cost-sorted table, the global-constraint
witness) did not run. FAMILY-SPECS lists G-5C.3 and calls 2C "the clean CSP GO (no fixpoint
hazard)" — but the family's own honest boundary is that *absent a demonstrated global
constraint, the verdict is the pluggable table, not the CSP*. With no prototype, 2C's GO is
SPEC-ONLY and its natural resolution ("it's a table") is the more likely one the synthesis
soft-pedals.

**Corpus caveat (minor).** 26 patterns, not the 50 the brief cites. Adequate to *refute*
2B (one counterexample suffices), thinner to *certify* 2A's correctness gate (26/26 vs
50/50) and its perf crossover.

**Convergence rationale.** 2B converged (BLOCKED, evidenced) — the lock's best result. 2A
converged to a *narrower and weaker* result than the synthesis states, with the
research-mandated reference-Hopcroft control missing and the shippable sliver contingent on
unbuilt work. 2C unprototyped. Net 60%.

**Exact open gaps (5C).**
1. Run the research-C P1 (b) control: 2A (and the acyclic-congruence path) vs a **correct
   O(n·k·log n) Hopcroft**, not only vs the restart-Moore strawman. Report the real
   crossover. Currently unmeasured — G-5C.1 unmet.
2. Price the null alternative: delete the `dfa.rs:358` global restart and re-measure. If
   fixing the incumbent erases 2A's win, 2A's shippable sliver has no consumer (Y.13-ish).
3. Correct FAMILY-SPECS §5C: 2A-general is **NO-GO** per PROTO-2; the deliverable is a
   15%-coverage acyclic fast path *conditional on the unproven alloc-light rewrite* — say
   so, don't summarize as "2A GO."
4. Prototype 2C (research-C P3): exhibit ≥1 corpus with a global (shared SIMD-LUT/codegen)
   constraint where the CSP beats the sorted table, or downgrade 2C's verdict to "pluggable
   table" and label the CSP GO SPEC-ONLY.
5. Re-run 2A correctness on the full 50-pattern corpus (or reconcile the "50" the brief
   cites with the 26 the file holds).
6. Confirm the acyclic-congruence path shares the real `crates/egraph` union-find as
   claimed (G-5C.5 determinism/tie-break for regen bit-identity) — unrun.

---

## FAMILY 5D — GRAMMAR-FRAGMENT — convergence 42%

**5D was folded into 5A as "the lowering-policy sub-question" — and the fold conveniently
buries 5D's single load-bearing number, which NEITHER prototype measured.**

**G-5D.4 (the one-pass fraction) is THE number 5D lives or dies by, and it is unmeasured.**
Research-D states it plainly: the one-pass fraction is "the single number that decides
whether 'no engine on the generator path' holds." PROTO-1 tested 5A's tax; PROTO-2 tested
5C's automata. Neither ran `check_one_pass_eligible` over the corpus. So 5D's central
uncertainty sits entirely untested, and the synthesis's "fold into 5A, settle by the
one-pass fraction later" is a deferral dressed as a resolution.

**5D's one genuine perf claim is SPEC-ONLY.** G-5D.3 (straight-line class scan cold-paired
throughput ≥ the PROVE-10 prefiltered `Dfa::find`) — research-D item 4, "THE unmeasured
question the prototype must settle head-to-head" — did not run. The maximal reading ("no
engine on the generator path") is correctly *refuted* by PROVE-10 (a proper named negative
result — real credit). But the *surviving* 5D claim (the straight-line scan at least ties
the prefiltered DFA) is unbacked; research-D's own R6 concedes "if it merely ties, report a
tie" — and even the tie was not measured.

**Spec-cites-its-own-existence (mild circularity).** 5D leans on "the combinator-vs-DFA
decision is ALREADY BUILT (`check_one_pass_eligible`)" and "the SIMD-recognizer chooser is
ALREADY BUILT." But its own R2 flags that `check_one_pass_eligible` tests FIRST-set
disjointness across alternations and may NOT reject a `Repetition` whose class overlaps its
own FOLLOW (`[a-z]+z` on `"abz"`, greedy give-back) — "a named gap the desugar must close
before trusting the flag." So 5D cites a predicate's existence as evidence of convergence
while simultaneously flagging that the predicate may be insufficient for the exact case
that matters — and no prototype checked which. Existence ≠ adequacy.

**Y.13 consumer-less anchor (shared with 5A's Look).** 5D's zero-width boundary node for
`^ $ \b` inherits the same consumer-less question (Q3); unresolved, same as 5A.

**Convergence rationale.** As a distinct architectural sub-center, 5D is essentially
unprototyped: its decisive number (one-pass fraction) unmeasured, its perf claim SPEC-ONLY,
its predicate-adequacy self-flagged as open. The maximal reading was cleanly refuted (which
is worth something). 42%. (If scored only as "5A's lowering policy" it rides 5A's 55%, but
the fold should not launder its unmeasured hard number into 5A's earned tax result.)

**Exact open gaps (5D).**
1. Measure G-5D.4: run `check_one_pass_eligible` over the 50+-pattern engine corpus + the
   real leaves; report the one-pass fraction. Unmeasured — the single decision number.
2. Run G-5D.3: cold-paired straight-line class scan vs PROVE-10 prefiltered `Dfa::find`,
   `count_a==count_b` gate. SPEC-ONLY; even the tie is unmeasured.
3. Close R2: verify `check_one_pass_eligible` rejects a `Repetition` whose class overlaps
   its FOLLOW (greedy give-back), or name the desugar fix. Predicate adequacy untested.
4. Resolve the shared Q3 (consumer-less anchor/`Look`) — same gate-blindness as 5A.
5. State in the registry that the 5A+5D fold does not transfer 5A's measured tax result to
   5D's unmeasured lowering claim — they earned different amounts of evidence.

---

## CROSS-CUTTING (constitution + synthesis integrity)

- **Clause vii (concrete-or-SPEC-ONLY) is under-applied by the synthesis.** 5B (whole),
  5D's G-5D.3/G-5D.4, and 5C-2C ran no prototype yet their gates read as measurement-ready.
  The registry must carry an explicit SPEC-ONLY label on each. Only 5A's tax+nullability,
  and 5C's 2A-correctness/2A-perf/2B, are RAN.

- **§0 two-tree reconciliation is a precondition, not a footnote.** Every pass-1 number is
  tree-conditional; 5A tested in the tree that pre-favours it, 5B untestable in that tree
  and untested in its own. The lock cannot report earned convergence until one reconciled
  tree exists — this is a LOCK-5 obligation the synthesis correctly records but the
  convergence framing quietly discounts.

- **The Axis-1 "collapse" is the synthesis's own vacuous-convergence risk.** "0 leaks ⇒ 5A
  dominates, functor unneeded" (line 337 + Brief-1 pass/fail) converts a one-sided cost
  census into a two-sided architecture verdict. Axis 1 is not collapsed; the write-once
  question (5B's value) is untouched.

- **Refutation-law compliance is good where measured.** PROTO-2 used cold-paired N=500,
  interleaved/order-cancelled, no warm benches, no shim perf claims, READ-ONLY scratch-copy
  discipline — clause iv honoured. PROTO-1 made no perf claim and said so. No violations
  found in what ran; the problem is what did *not* run, not disinformation in what did.

- **Legacy-alias / masked-fallback scan: clean.** No family smuggles a legacy path; 5A/5D
  delete the classify stub and string re-parsers outright; 5C keeps the classical pipeline
  as an *explicitly named* fallback, not a masked one. No findings.

## CONVERGENCE SUMMARY (Critic-1, within-lock)

| Family | % | One-line why |
|---|---|---|
| 5A ONE-HIR | 55% | Tax+nullability RAN & favourable, but on a stale/pre-favouring tree; clause-(i) bootstrap, automata parity, Look-consumer, UTF-8 all SPEC-ONLY |
| 5B LAWFUL-FUNCTOR | 45% | Sound spec, zero prototypes; flagship precedent (`translate.rs`) is same-domain not the cross-domain functor needed; its value prop (rule-death) untested and Axis-1 not actually resolved |
| 5C AUTOMATA-AS-EXTRACTION | 60% | 2B BLOCKED cleanly confirmed (best result); 2A-general NO-GO (synthesis overstates "GO"), reference-Hopcroft control missing; 2C unprototyped |
| 5D GRAMMAR-FRAGMENT | 42% | Maximal reading cleanly refuted; but its decisive one-pass-fraction number and its one perf claim are both unmeasured; predicate-adequacy self-flagged open |

Registry directive for pass-2: 5B and 5C-2C and 5D-perf must either RUN or carry SPEC-ONLY;
5A's tax census must re-run on HEAD/target enum; 5C must add the reference-Hopcroft control
and price the "just delete the restart" null; the Look/anchor consumer gate must be
re-specified so it can *fail*; and no family reports earned convergence until §0's
two-tree reconciliation produces one tree to measure in.
