# LOCK-5 — THE REGEX CONTINUUM — pass-2 CRIT-2 (adversarial critic 2 of 2, cross-lock weight)

Critic 1 hardens intra-lock; my remit (constitution ix) is the **cross-lock view** plus the
failure-mode checklist and the constitution. I read PORTFOLIO (LOCK-5 + constitution), pass-2
FAMILY-SPECS, PROTO-2 (5A merge census on HEAD), proto-1/PROTO-1 (5B un-bank gate),
research-{A,B,C,D,priorart}, the pass-1 registry + both pass-1 critics, and spot-verified every
load-bearing code claim the findings rest on.

**Verified in-tree this pass (not read from the reports — run/grepped):**
- top-level `crates/ir/src/egraph/rules/regex.rs` = **434 LOC** (PROTO-1's death target is real).
- `IrNode` at `node.rs:30` derives `#[derive(Serialize, Deserialize, …)]` (`node.rs:29`) — PROTO-2's
  E0277 serde break is a genuine consequence, **not** a probe artifact of the enum.
- `crates/ir/Cargo.toml:14` — `bbnf-regex = { … features = ["serde"] }` is **already on**, yet
  `CharClass`/`Look` still fail `Serialize` ⇒ the feature wires serde into `charset.rs` only, not
  `hir/mod.rs`. The break is real **today** but is a **one-derive parse-that edit** away (blocked
  only by the probe's READ-ONLY policy) — bears directly on how much weight the serde strike carries.
- `one_pass.rs:64` — `Repetition(rep) => check_one_pass_eligible(&rep.sub)`: recurses into the sub,
  **never checks the repetition class against its own FOLLOW**. 5D fact-1 (unsound predicate) confirmed.
- skinny `passes/src/lib.rs:342/349/353` — `derive_recognizers` matches single-byte `Literal`, an
  `analyze()`-keyed `QuotedString` `Regex`, then `_ => {}`. 5D fact-2 (masked wildcard swallows a
  desugared `ByteClass`; the `QuotedString` arm keys on the 3-pattern oracle) confirmed.
- parse-that `translate.rs:19/60` — `insert_hir`/`extract_hir` are **same-domain** `Hir↔HirENode`.
  5B's "the cross-domain functor does not exist; `translate.rs` is only the same-domain model" confirmed.

None of the prototypes fabricate. The problem is not the code — it is the **synthesis that sits on
top of it.**

---

## 0. THE HEADLINE — THE PASS-2 FAMILY-SPECS IS STALE AGAINST ITS OWN PASS-2 PROTOTYPES

This is the single deepest defect and it invalidates the synthesis's convergence table wholesale.

FAMILY-SPECS.md (mtime **09:45**) declares in §top: *"What pass-2 was. A **research + synthesis**
pass: **no new prototype ran.** … the numbers the registry demanded run … remain **unrun**. They are
the **pass-3** prototype budget; the two briefs below select the two that gate most."* It then states
**Brief 1 (5B un-bank/retire gate)** and **Brief 2 (5A merged-type leak census)** as *future* work,
and carries **5B BANKED 45%** and **5A ~62% (un-earned until prototype)**.

But the same directory holds:
- **PROTO-2.md (09:59)** = Brief 2, **RAN** on HEAD `7cdbff4ef`, top-level `crates/ir`. Verdict:
  **"5A's clean-merge horn is REFUTED."** REGEX-LEAK ≈ 29 ≫ 0; the `IrNode` derive breaks (E0277 ×4).
- **proto-1/PROTO-1.md (10:01)** = Brief 1, **RAN** on the same HEAD, top-level `crates/ir`. Verdict:
  **"5B → RETIRED."** Copy dies 250 LOC (met), 59/59 DFA-equiv (met), Criterion 2 (projection-drop
  soundness bug) **NOT MET — the bug is a non-bug under the Map-wrapper representation.**

So the two briefs the synthesis nominates for pass-3 **were both run inside pass-2, after the synthesis
was written, and both decisive verdicts landed** — and the synthesis was **never reconciled to them.**
The FAMILY-SPECS convergence numbers (5A ~62% BANKED-pending, 5B BANKED 45%) are the **pre-run floor**;
the runs supersede them. A registry built from FAMILY-SPECS alone would record the exact opposite of
what the prototypes proved. **This is the checklist's "unverified gestalt" in its purest form: the
synthesis is out of date relative to evidence sitting in the same folder.** Every number below is
re-cut from the **prototypes**, not the stale synthesis.

The reconciled post-pass-2 truth: **both Axis-1 maximal horns ran, and BOTH failed.** 5A's "identity /
nothing to map" is refuted (a real ~29-leak + serde tax). 5B's "the functor buys unique soundness
value" is refuted (the soundness bug does not exist). What actually survives is a **third position no
family owns as its charter**: *two domain-fit types kept, joined by a cheap (string/`format!`) bridge,
with the 250-LOC consolidation harvested by a bare merge.* That un-homed middle is the real Axis-1
landing — and the synthesis, still framing Axis-1 as "un-earnable until GESTALT runs both horns,"
hands GESTALT-P a question that has **already been answered against both stated options.**

---

## 1. THE TWO PASS-2 PROTOTYPES CONTRADICT EACH OTHER ON 5B — AND NOBODY RECONCILED IT

PROTO-2's verdict text: *"5B's domain-fit separation is justified."* PROTO-1's verdict text:
*"5B → RETIRED."* Read flat, these are opposite. The reconciliation exists but is written **nowhere**:

- PROTO-2 justifies keeping **two domain-fit TYPES** (it refutes 5A folding them into ONE).
- PROTO-1 retires 5B's **FUNCTOR as an architecture with independent value** (the soundness win is a
  non-bug; the LOC win reduces to a bare merge).

Both can hold simultaneously — but only if the registry states the synthesis explicitly: *keep two
types (5A-merge refuted), bridge them cheaply (5B-functor retired).* Left unreconciled, a reader takes
"5B justified" (PROTO-2) and "5B retired" (PROTO-1) as a coin-flip. **CRIT-2 requires the registry to
carry this reconciliation as a named line, not leave two prototype verdicts in flat contradiction.**
It is also load-bearing cross-lock: LOCK-4-4D, LOCK-3-3C, LOCK-9-9C all hang on "ONE-HIR," and the
answer is now "**neither one type nor a functor — two types + a trivial bridge**," which none of those
three downstream locks were told to consume.

---

## FAMILY 5A — ONE-HIR — convergence **58%** (maximal horn REFUTED; narrowed landing survives)

**What pass-2 genuinely earned (PROTO-2, RAN, top-level HEAD — this is real science):** the
clean-merge maximal reading ("regex HIR *is* a subset, there is nothing to map, the god-type
dissolves") is **falsified by a run**, on the mature tree the synthesis itself named the tree of
record. That is a legitimate certified negative and it retires 5A's strongest rhetorical claim.

**But the refutation is itself over-weighted, and the narrowed landing carries the same unrun gaps:**

- **G5A-a (serde strike is artifact-removable — PROTO-2 over-counts its own fail conditions).**
  PROTO-2 lists **two independent sufficient** fail conditions: (1) REGEX-LEAK ≈29, (2) the derive
  breaks. I verified (2) is `CharClass`/`Look` lacking serde impls in `hir/mod.rs` while the crate's
  `serde` feature is *already enabled* — a **one-line `#[cfg_attr(...derive)]` in parse-that**, blocked
  only by the probe's READ-ONLY policy, not by anything architectural. FAMILY-SPECS 5A itself concedes
  *"CodepointRange is POD + Serialize-clean."* So (2) is **not** an independent architectural refutation
  of 5A — it is a removable artifact. The **robust** refutation is (1) alone, and even (1) is soft
  (below).

- **G5A-b (the ≈29 leak count is hand-graded, only 6 arms read).** PROTO-2 states plainly: *"the rest
  are graded from pass purpose; the ~29/~24 split is approximate."* Six arms were directly inspected
  (`first_sets`, `regex_info`, `types/generate`, `scalar_routing`, `span`, `inspect/walk`). The
  pass/fail fact (REGEX-LEAK ≥ 1 ⇒ "nothing to map" is false) is **robust** — I do not dispute it. But
  the **magnitude** (29 vs 24) that a cost comparison against 5B would need is an estimate, not a count.
  The synthesis must not convert "≈29" into a load-bearing number.

- **G5A-c (the "ONE authored type" premise is shown FALSE by PROTO-2 itself).** PROTO-2's own note:
  the three lowering sites (`dag/build.rs:59`, `egraph/build_egraph.rs:25`, `dag/mod.rs:160`) force the
  merge to be mirrored into **`DagNode` and `GrammarENode`** — *"IrNode → DagNode → GrammarENode are
  three hand-mirrored enums."* 5A's headline is "one authored type, two **derived** views." The tree
  already has **three hand-mirrored** enums today. The "two-views-are-derived" geometry is true only for
  the `egraph-derive` view; the `DagNode` view is hand-mirrored. **The god-type is not dissolved; it is
  triplicated.** This is a new, run-surfaced refutation of 5A's central structural claim, and the
  narrowed 5A inherits it.

- **G5A-d (the `Look` consumer gate STILL cannot fail — pass-1 G5A-a un-closed).** FAMILY-SPECS claims
  research-A *"named a reader and a failing test"* (the `/foo$/` vs `/foo/` automaton accept condition)
  and *"closed the Y.13 gate **conceptually**."* The word "conceptually" is the tell: **the test was
  not built** — PROTO-2 was the merge census, not the Look-consumer test; G-5A.2 remains SPEC-ONLY. The
  admissible-set narrowing to `{End, EndLF}` and the demotion of `Look::Start` to "no-op unless a reader
  is wired" are asserted. Consumer-less-substrate (Y.13) on `Look::Start` stands exactly where pass-1
  left it. **Gate-that-cannot-fail: unmoved.**

- **G5A-e (classify re-home — elegant-reduction trap, now shown non-trivial by the run).** Pass-1 flagged
  "classify becomes an IR analysis pass" as re-expressing a 1148-LOC classifier structurally, unbuilt.
  Pass-2 did not build it — and worse, PROTO-2's leak census lands `classify.rs:181,587` and
  `pin_sweep.rs:94` in the **REGEX-LEAK** column (the materialization lattice must know what a `Class`
  produces). So the run now provides **evidence against** "classify re-homes trivially": the re-home is
  not just unbuilt, it is shown to require regex-specific dispatch. The load-bearing "…and then
  re-implement the hard classifier" step is unconverged and now empirically harder than the sentence.

- **G5A-f (automata parity G-5A.3 STILL unrun — SPEC_ONLY anchoring a perf claim).** No `from_expr` NFA
  vs vendored `from_hir` byte-identity; PROVE-10's 8.8–29.5× band **not** reproduced from any merged or
  narrowed type. The claim "the ONE type hosts the whole engine at frozen numbers" is SPEC-ONLY after
  two passes. Constitution v (zero-runtime-cost of the merged-type automata path) unverified.

- **G5A-g (grammar-driven premise SPEC-ONLY — P2, unmoved).** Neither pass-2 prototype exercised a
  grammar-driven regex path; both are IR-level. The lock named "grammar-driven regex engine" still has
  **zero grammar-driven artifacts** after two passes. FAMILY-SPECS' re-scope of 5A to "promote + de-dup
  + break-the-discard" (parse-that `regex.bbnf` discards its AST; `grammar/misc/regex.bbnf` divergent) is
  the correct framing (credit vs pass-1) but is still SPEC-ONLY-until-7A.

**Assessment.** 58%. The maximal horn is now a *run* negative (progress: the ceiling is known). The
narrowed landing (two types + Class/Look leaves + a real bounded tax) is the de-facto Axis-1 winner, but
it carries the identical unrun gates — automata parity, classify re-home (now shown non-trivial), Look
consumer, bootstrap, grammar-totality — plus a **new** run-surfaced refutation of the "one authored
type" premise (three mirrored enums). Not raised above my pass-1 58%: the resolved-ceiling gain is offset
by the triplication finding and the still-unrun decisive gates.

---

## FAMILY 5B — LAWFUL-FUNCTOR — **RETIRED (earned-conditional)** — convergence-of-disposition **~55%**

Pass-1 CRIT-2 struck "5B unneeded" as vacuous (0/4 gates run). Pass-2 fixed that: **PROTO-1 RAN the
un-bank gate**, and the retirement is now *earned by measurement*, not asserted — a real advance. But it
is **earned-conditional**, and two conditions are open:

- **G5B-a (the retire rests on the Map-wrapper `->` representation, which LOCK-1 has NOT ratified).**
  PROTO-1's Criterion-2 non-bug is: `[a-c]->A` lowers to `Map{Regex([a-c]), fn_id}`, and the fuse/union
  rules locate a branch's regex via `class_regex(child)` over the child's e-class — a Map-wrapped branch
  resolves to `None`, so the merge is structurally inert on projected branches. **This is a property of
  the current `Map`-wrapper lowering of `->`.** LOCK-1 owns the *real* `->` / projection type system and
  has not ratified that `->` stays a `Map` wrapper. If LOCK-1's real projection typing represents `->`
  any other way (e.g. a typed edge annotation, not an inner-class Map barrier), the
  projection-dropping-union bug **can reappear and un-retire 5B.** The retirement is contingent on
  LOCK-1's verdict — an open cross-lock condition, not a closed fact.

- **G5B-b (single-tree; the 250-LOC death is a FORK artifact, not a family win).** PROTO-1 ran only in
  **top-level** `crates/ir`. The 250 LOC deleted are rules that exist **only** in top-level. Under P1's
  **Fork-B** (abrogate the grammar-tier regex e-graph — skinny, the graduating tree of record, ships
  without one today), that 250 LOC **is deleted for free, by neither 5A nor 5B.** PROTO-1 correctly folds
  the win down to "achievable by a bare 5A merge," but does not fold in Fork-B, under which the win is
  **zero** (the copy never survives reconciliation). So 5B's LOC value is doubly non-load-bearing:
  bare-merge-achievable *and* fork-dependent. The retirement is correct; the reasoning must name that the
  copy's death is not evidence *for* any family.

**Assessment.** RETIRED, and the retire is now RAN-earned (a genuine pass-2 advance over the vacuous
pass-1 elimination — credit). Convergence-of-disposition ~55%: the decision is evidenced but conditional
on (a) LOCK-1 keeping the Map-barrier representation of `->`, and (b) fork-independence. The **joint
coproduct** variant (5B's strongest form) was **never built** either pass — it dies undesigned, which is
acceptable given the functor's core value axis is now a proven non-bug, but the registry should say so
rather than leave the coproduct as a phantom un-run option.

---

## FAMILY 5D — GRAMMAR-FRAGMENT (5A's lowering policy) — convergence **35%** (diagnosis sharpened, resolution unbuilt)

Pass-2's three CONFIRMED in-tree facts (all of which I independently grepped and confirmed) are real
convergence **on the problem statement**, not on the fix:

- **Fact 1 confirmed (`one_pass.rs:64`): the committed one-pass predicate is UNSOUND for its own use.**
  A straight-line lowering routed on this flag would **reject a matching string** (`[a-z]+z` on `"abz"`).
  Both trees route on it (`RegexRecognizer::scanable()`). This is a genuine, source-confirmed
  **correctness fault** — the sharpest 5D result. But the **fix** (desugar to `Seq[Repeat, suffix]` and
  let grammar FOLLOW / `AltMode::Dispatch` decide, or adopt regex-automata's build-and-bail) is
  **unbuilt**.

- **Fact 2 confirmed (`passes/lib.rs:353`): `derive_recognizers` `_ => {}` swallows a desugared
  `ByteClass`.** Pass-1's masked-fallback contradiction is now **source-confirmed** (I verified the
  three arms + wildcard). 5D's entire SIMD fast-path chooser **is not firing.** The fix (structural
  `ByteClassRecognizer`) is **unbuilt**.

- **Fact 3 confirmed: the structured class never reaches the emitter** (`BackendExpr::RegexProgram
  {pattern:String}` is opaque). The typed-materialization invariant obliges `BackendExpr::ByteClass` →
  tape. **Unbuilt.**

- **G5D-a (THE decisive number G-5D.4 STILL UNMEASURED — two passes).** The FOLLOW-sound one-pass
  fraction — the single figure deciding delete-vs-rehome the DFA — was not run in pass-1 or pass-2.
  FAMILY-SPECS asks for *both* the reported (~92% under the unsound predicate) and the sound fraction;
  neither exists. The narrowing line remains undrawn.

- **G5D-b (throughput head-to-head G-5D.3 UNMEASURED).** Class-scan vs prefiltered `Dfa::find`,
  cold-paired: never run. Even the "tie is a tie" honest prior is unmeasured.

**Assessment.** 35% (held, not raised). Pass-2 turned three pass-1 *suspicions* into three *confirmed
diagnoses* — good, honest work — but a confirmed bug is a problem statement, not a resolution, and the
two decisive numbers that fix 5A's lowering policy are unrun after two passes. The masked-wildcard
contradiction (pass-1 G5D-d) is now proven real; the fix is not built.

---

## FAMILY 5C — AUTOMATA-AS-EXTRACTION — convergence **60%** (NO 5C prototype ran in pass-2)

Pass-2 ran **no** 5C prototype — PROTO-2 was the 5A merge, PROTO-1 the 5B gate. Every decisive 5C number
the pass-1 registry demanded remains exactly as unrun as pass-1 left it. The pass-2 FAMILY-SPECS sharpened
the *analysis* (named the O(n²) asymptotic risk of naive congruence iteration; re-confirmed the `:358`
restart is present; re-stated the Kozen bound), which is worth acknowledging — but sharper prose is not a
run, and 5C's convergence must **not** rise this pass.

- **G5C-a (reference-Hopcroft control STILL unrun — G-5C.1 unmet, two passes).** The decisive question —
  does the 15% acyclic fast path beat a *correct* O(n·k·log n) Hopcroft, or does **deleting `dfa.rs:358`
  alone erase 2A's entire reason to exist?** — is unmeasured. 2A's only measured win (0.80×) is against
  a **strawman** (naive Moore + global restart), on a 4-pattern slice, in isolation, with the
  `is_acyclic` dispatch cost on the other 85% **not folded in**. The shippable sliver may be net-negative
  and has no consumer if `:358` is simply fixed.

- **G5C-b (2B-as-minimizer BLOCKED — Kozen — cleanly confirmed, no dispute).** ρ_max 1.667, organic miss
  `\w+(?:[-_]\w+)*`. No finite ground rewrite set is complete for star-equivalence. This is the lock's
  cleanest, most valuable result. 2B-as-*determinizer* survives; 2A/Hopcroft closes.

- **G5C-c (corpus still 26; gate cites ≥50 — born-RED unsatisfiable-as-written).** FAMILY-SPECS says
  "GROW `COMPILE_PATTERNS` to ≥50, stratified" — but it is **not grown**; the corpus is 26. G-5C.1 as
  written cannot be met. Pass-3 must grow-and-tag before any minimizer number is trustworthy.

- **G5C-d (2C UNPROTOTYPED — the one part that could positively ship — and double-homed with LOCK-2).**
  The tier-selection table/CSP was not built in either pass. FAMILY-SPECS' resolution ("home the *table*
  in 5C; escalate to the CSP only on an exhibited global-budget witness") is asserted; **no witness was
  sought.** The double-home with LOCK-2's scan-plan/tier CSP is named but unadjudicated.

**Assessment.** 60% (held — no run this pass; the honest deliverable is a 15%-applicable contingent fast
path + 2B's named theory bound + 2C's still-to-adjudicate table-vs-CSP). The pass-3 budget owes
reference-Hopcroft + delete-`:358`-null + a grown corpus + 2C.

---

## 2. CROSS-LOCK VIEW (constitution ix — my primary remit)

**Credit where due (vs pass-1):** pass-2 FAMILY-SPECS now carries a **CROSS-LOCK DOUBLE-HOMES section
naming all seven** interlocks — directly answering pass-1 CRIT-2's largest structural complaint (the hub
named none of them). That is real progress. But every resolution in that section is a **one-sided
declaration LOCK-5 makes**; none is ratified by the other lock. The named supersessions are *proposals*
GESTALT must confirm, not settled facts. The cross-lock risks that remain:

1. **The Axis-1 verdict three downstream locks hang on is now MORE ambiguous, not less.** LOCK-4-4D,
   LOCK-9-9C, LOCK-3-3C are all contingent on "ONE-HIR." Pass-2 ran both Axis-1 horns and **both maximal
   readings failed**: 5A-merge refuted (PROTO-2), 5B-functor retired (PROTO-1). The resolved position —
   *two domain-fit types + a cheap bridge* — is **owned by no family's charter**. So the three downstream
   locks inherit a spine whose top is now **empty**, worse than pass-1's "5A leads." LOCK-4-4D's premise
   ("one HIR, not a second engine") is specifically weakened: the answer is "two types," which is
   arguably *not* one HIR. GESTALT-P must re-cut 4D/9C/3C to consume "two-types-cheap-bridge," or the
   three locks converge on a verdict the prototypes refuted.

2. **5B retirement ⊗ LOCK-1 (unratified representation dependency).** 5B's retire rests on `->` staying a
   `Map`-wrapper (§G5B-a). LOCK-1 owns the real `->` type system and has not ratified that lowering. The
   retirement is conditional on LOCK-1's verdict — a live cross-lock coupling that must be recorded as a
   re-open trigger, not buried.

3. **5C-2C ⊗ LOCK-2 tier CSP — still double-homed, no witness sought.** Unchanged from pass-1. The tier
   selection is claimed by both locks; LOCK-5's "single-home the table here" is asserted; LOCK-2 has not
   deferred it. Must be adjudicated before either lock converges.

4. **5A/5D ⊗ LOCK-9-9C.** The classify re-home + SIMD-recognizer selection are claimed by both. LOCK-5
   now *names* the supersession ("LOCK-5 owns the HIR→DFA→classify substrate + the ByteClass chooser; 9C
   consumes or retires"). But PROTO-2's leak census shows the classify re-home is **non-trivial** (needs
   regex dispatch) — so the substrate LOCK-5 offers 9C is itself unbuilt. 9C is deferred to a deliverable
   that does not yet exist.

5. **5A ⊗ LOCK-7 bootstrap — SPEC-ONLY, unmoved.** Two divergent `regex.bbnf` (one discards its AST, one
   divergent); grammar-driven regex is SPEC-ONLY lock-wide after two passes. Re-scope to "promote + de-dup
   + break-the-discard" is correct but un-executed. Every verdict line must carry SPEC-ONLY(grammar-driven).

6. **rust/ts ⊗ LOCK-6 — entirely unaddressed, two passes.** The rust/ts face of the merged/narrowed regex
   HIR (`Class`/`Look`/`ByteClass` + automata crossing to TS, PROVE-14 tape) is named as a "gap for
   GESTALT-Ω" and left there. With 5A now landing as "two types," LOCK-6's mirror/skeleton must carry
   **both** — a larger obligation than "carry the merged enum." Still nobody's charter.

---

## 3. FAILURE-MODE CHECKLIST — HITS

- **Unverified gestalt (the headline):** the pass-2 FAMILY-SPECS predates and **contradicts its own
  pass-2 prototypes** — declares "no prototype ran / these are pass-3 briefs" while PROTO-1 (5B RETIRE)
  and PROTO-2 (5A REFUTED) ran the very briefs; convergence table never reconciled to the runs.
- **Elegant-reduction trap:** 5A "classify becomes an IR analysis pass" — unbuilt, and PROTO-2's leak
  census now shows it needs regex dispatch (empirically the hard part, not an elegant one-liner); 5A
  "automata at PROVE-10 numbers" (G-5A.3 unrun, two passes); 5C-2A "acyclic fast path + [the alloc-light
  rewrite, unbuilt]".
- **Gate that cannot fail / consumer-less substrate (Y.13):** `Look::Start` still inert, its consumer
  test (`/foo$/`) "closed **conceptually**" but unbuilt; `BackendExpr::ByteClass` emit unbuilt.
- **Masked fallback:** `derive_recognizers` `_ => {}` swallows a desugared `ByteClass` — now
  **source-confirmed**; the SIMD chooser 5D rests on is not firing.
- **Legacy aliases / divergent homes:** two `regex.bbnf` (discard + divergent) persist; the opaque
  `RegexProgram{String}` still carries the class to the emitter (5D fact 3).
- **Prototype-evidence misuse (both directions):** the stale FAMILY-SPECS *under-claims* (numbers
  pre-run); PROTO-2 *over-refutes* 5A on the artifact-removable serde strike (counts it a second
  independent sufficient fail condition when it is a one-derive parse-that edit) and on an approximate
  hand-grade (only 6 of ≈29 arms read).
- **Spec-cites-itself / circularity:** grammar-driven regex (regex grammar defines regex; grammars
  contain regex) SPEC-ONLY *and* contradicted by two extant grammars; deferred to LOCK-7/7A.
- **Constitution:** (i) grammar-driven SPEC-ONLY lock-wide, two passes, zero grammar-driven artifacts;
  (v) zero-runtime-cost of the merged/narrowed automata path unverified (G-5A.3 unrun); (vii) verdict
  lines must carry SPEC-ONLY(bootstrap/automata); (viii) `Look::Start`; (ix) double-homes now *named*
  but resolutions unratified.
- **Not a vacuous-convergence hit this pass (credit):** 5B's retirement is now RAN-earned, reversing
  pass-1's vacuous elimination — the single clearest process improvement of the pass.

---

## 4. CONVERGENCE (CRIT-2 floor — re-cut from the PROTOTYPES, not the stale synthesis)

| Family | Status | % | One line |
|---|---|---|---|
| 5A ONE-HIR | ADVANCED (maximal REFUTED; narrowed survives) | **58** | PROTO-2 RAN: clean-merge refuted (≈29 leak, serde break, 3 mirrored enums); narrowed two-types landing carries the same unrun automata/classify/Look/bootstrap gates |
| 5D GRAMMAR-FRAGMENT | BANKED (5A's lowering policy) | **35** | 3 in-tree facts source-confirmed (unsound one-pass predicate, masked `ByteClass` wildcard, opaque emitter) — diagnosis sharpened, resolution unbuilt; G-5D.4/.3 unmeasured two passes |
| 5B LAWFUL-FUNCTOR | **RETIRED (earned-conditional)** | **~55** | PROTO-1 RAN: copy dies 250 LOC + 59/59 DFA-equiv, but Criterion-2 soundness bug is a non-bug → dies to a bare merge; retire conditional on LOCK-1 keeping the Map-barrier + fork-independence |
| 5C AUTOMATA-AS-EXTRACTION | ADVANCED (2B-as-minimizer BLOCKED) | **60** | NO 5C prototype ran this pass; reference-Hopcroft control + delete-`:358` null + grown corpus + 2C all still unrun; 2B BLOCKED (Kozen) confirmed |

**Lock-wide.** No family converged. The **Axis-1 spine is now empty at the top**: both maximal horns ran
and failed, and the surviving two-types-cheap-bridge landing is un-homed by any charter — the three
downstream locks (4D/9C/3C) hang on a verdict the prototypes refuted rather than confirmed. The
grammar-driven premise (constitution i) is SPEC-ONLY after two passes with zero grammar-driven artifacts.
**The pass-2 FAMILY-SPECS must be rebuilt from PROTO-1/PROTO-2 before any registry is cut from it.**

**CRIT-2 pass-3 directives (ordered by what gates the most):**
1. **Reconcile the synthesis to its own protos.** Rewrite the FAMILY-SPECS convergence table from
   PROTO-1 (5B RETIRE) and PROTO-2 (5A merge REFUTED); record the two-types-cheap-bridge landing as the
   named Axis-1 winner and re-home it (no family currently owns it).
2. **Re-cut 4D/9C/3C's dependency** from "ONE-HIR" to "two-domain-fit-types + cheap bridge" — or accept
   they converge on a refuted verdict.
3. **Record 5B's retirement as conditional** on LOCK-1 keeping the `Map`-barrier representation of `->`
   and on fork-independence (Fork-B deletes the copy for free); name the re-open trigger.
4. **Run the still-decisive numbers:** 5D's FOLLOW-sound one-pass fraction (G-5D.4) + class-scan-vs-DFA
   throughput (G-5D.3); 5C's reference-Hopcroft control + delete-`:358` null on a grown (≥50, tagged)
   corpus + 2C table-vs-CSP witness; 5A's automata parity (G-5A.3) from the narrowed type + a Look
   consumer test that can *fail*.
5. **Exercise ONE grammar-driven regex path** (promote parse-that `regex.bbnf`, break the discard) — the
   lock's namesake, SPEC-ONLY after two passes.
6. **Single-home the tier CSP** against LOCK-2; **ratify** (not merely declare) the 9C/LOCK-1/LOCK-6
   supersessions with those locks.
