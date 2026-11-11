# LOCK-5 — THE REGEX CONTINUUM — pass-1 CRIT-2 (adversarial critic 2 of 2, cross-lock weight)

Critic 1's remit is intra-lock hardening; mine carries the **cross-lock view** (constitution
ix): where this lock double-homes work against LOCK-2/3/4/7/9 or the GN/PT codex, and where a
family's convergence is asserted rather than earned. I read PORTFOLIO (LOCK-5 + constitution),
FAMILY-SPECS, research-{A,B,C,D,priorart}, PROTO-1 (+patch), PROTO-2 (+proto2.rs/patch/run),
and spot-verified the load-bearing code claims in-tree.

**Verified this pass (in-tree, skinny HEAD + cross-lock specs):**
- `skinny/grammars/` holds **only `json.bbnf`** — no `regex.bbnf`. `parse_regex` returns a raw
  `String` (`grammar/src/lib.rs:1561`); `skip_regex` scans to the delimiter (`:1166`). Regex is
  hand-scanned to an opaque string TODAY. Confirmed.
- `bbnf-regex` classify is the 3-pattern table (`==r"[ \t\n\r]*"`, `starts_with('"')`,
  `starts_with("-?(0|[1-9]")`, else `Unknown`) — `src/lib.rs:92-103`. Confirmed.
- `nullability` escapes to `analyze(pattern).nullable` at `ir/src/lib.rs:331`. Confirmed.
- **Current skinny `ExprKind` has 10 variants** — `Seq/Alt/Repeat/Optional/Literal/Regex/Ref/
  Annotation/Project/Discard` (`ir/src/lib.rs:235-266`). **PROTO-1 ran on `d2b32f6e3`, an
  8-variant snapshot with NO `Project`/`Discard`.** The tax census is against a staler tree than
  the graduating HEAD. This matters (5A §2 below).
- **LOCK-7 found regex.bbnf EXISTS — twice**: `parse-that/rust/bootstrap/regex.bbnf` generates
  `generated.rs` (2293 LOC) whose `parse_generated()` **discards** its AST (production regex is
  still the hand engine); and `grammar/misc/regex.bbnf` (1006 B) is "a THIRD, divergent regex
  grammar… the one-regex-home breach LOCK-5 owns" (LOCK-7 FAMILY-SPECS:30-36). This directly
  contradicts research-A's / 5A's premise that "regex.bbnf is to-BUILD, not extant."

---

## 0. THE VERDICT LINE, RE-STATED HONESTLY

The pass produced two genuine results and one over-reach:
- **Earned (PROTO-2, RAN):** 5C-2A is EXACT but **3.53× slower** whole-corpus; 5C-2B is
  **BLOCKED-confirmed** (ρ=1.667 organic). These are legitimate certified negatives.
- **Earned (PROTO-1, RAN):** merging `Class`/`Look` into `ExprKind` taxes the grammar passes
  trivially on the tested tree; nullability parity 3/3.
- **Over-reach:** the FAMILY-SPECS + PROTO-1 verdict "**5A/5D dominate; 5B unneeded**" is drawn
  from the *5A* 0-leak count alone. **5B's own gates were never run.** Eliminating a family by
  running its rival's test is vacuous convergence (checklist hit #1). And "dominate" is asserted
  while **3 of 5 5A gates and the decisive 5D number are unmeasured**, and the lock's foundational
  premise (grammar-driven regex, constitution i) is SPEC-ONLY across every family.

The single deepest fact CRIT-2 surfaces: **the lock is named "THE REGEX CONTINUUM — grammar-driven
regex engine," yet not one artifact this pass exercised a grammar-driven regex path.** PROTO-1
built `ExprKind::Class` by hand-translating through the *read-only* HIR parser, not through a bbnf
grammar. `regex.bbnf` in skinny does not exist; the two that exist elsewhere either discard their
output or are a divergent breach. Constitution clause (i) is therefore **SPEC-ONLY, lock-wide**,
and no family's verdict line carries that label (constitution vii violation).

---

## FAMILY 5A — ONE-HIR — convergence **58%**

**What is earned.** The pass's headline question — does folding regex atoms into `ExprKind` leak
regex into grammar passes? — is genuinely answered: **0 grammar-plane leaks, 13 arms all trivial,
nullability 3/3** (PROTO-1 RAN, GREEN). That collapses Axis-1's "functor to keep passes clean"
motive for 5B. Real, and I do not dispute it.

**Where it is NOT converged (enumerated open gaps):**

- **G5A-a (the Look consumer-less inversion — Y.13, constitution viii).** PROTO-1's "0 leaks" is
  *partly manufactured by making `Look` inert*: `Look→true` (nullability), `Look→Builtin(Unit)`
  (type), `Look→empty Seq` (lower), `Look→false/{}` everywhere else. Every arm is trivial
  **precisely because nothing consumes `Look`.** A consumer-less variant *always* yields
  trivial-total arms — so gate G-5A.1 ("0 arms need regex-specific dispatch") and Y.13
  (consumer-less `Look`) are in direct tension, and PROTO-1 resolved the first by *creating* the
  second. research-A Q3 and research-D R5 both flag this; neither prototype demonstrated a
  grammar-side `Look` consumer (the PT-1.W5 anchored/unanchored distinction is cited, never
  built). Until a real consumer is shown or `Look` demotes to `Annotation`, 5A ships a
  consumer-less variant into the shared enum. **This is the checklist "gate that cannot fail"
  crossed with "consumer-less substrate."**

- **G5A-b (stale-tree census — prototype-evidence misuse).** The 13-arm/0-leak count is against
  an **8-variant tree** (`d2b32f6e3`, no `Project`/`Discard`, "validator still hard-rejects
  nullable repeats"). Current graduating HEAD has 10 variants and a richer pass set; the mature
  top-level `crates/ir` (FAMILY-SPECS §0) is richer still. The census under-counts arm sites and,
  more importantly, tests the type-plane leak (research-A Q1) against skinny's **degenerate
  `BuiltinTy`** — where `Class→Builtin(Bytes)` is trivial only because the type plane is a stub.
  **LOCK-1 owns the real type system** (ADTs, row-poly, widths, decode classes); whether `Class`
  there acquires "a scalar-byte type unlike any grammar leaf" is unretired. The 0-leak claim
  generalizes from the thinnest available tree to the whole architecture.

- **G5A-c (classify re-homing is the elegant-reduction trap).** "classify becomes an IR analysis
  pass" re-expresses a **1148-LOC `classify.rs`** as "structural passes deriving
  {Whitespace,QuotedString,Numeric}." **No prototype touched it.** The load-bearing step reads
  "…and then re-implement the hard 1148-LOC classifier structurally" — the unconverged hard part
  behind an elegant sentence.

- **G5A-d (automata parity unrun — SPEC_ONLY anchoring a perf claim).** research-A calls
  "automata from `ExprKind` at PROVE-10's 8.8–29.5×" the "strongest positive result available."
  G-5A.3 (from_expr NFA == vendored path, byte-identical) **was not run.** The claim that the
  merged type hosts the whole engine at frozen numbers cites PROVE-10 but produced no artifact —
  constitution vii: SPEC-ONLY masquerading as a perf inheritance.

- **G5A-e (bootstrap fixpoint SPEC-ONLY; premise now contradicted).** G-5A.4 (regen∘regen
  byte-identical on regex.bbnf) unrun. FAMILY-SPECS itself concedes "5A is SPEC-ONLY until 7A."
  But LOCK-7 shows the situation is *worse than to-BUILD*: two regex.bbnf grammars already exist
  and neither is production (one discards output, one is a divergent breach). 5A's task is not
  greenfield authoring but **promotion + de-duplication of extant divergent grammars** — a
  problem 5A's §0 substrate table never enumerated. Its verdict line must carry the SPEC-ONLY
  label and be re-cut against the real (multi-home) starting state.

- **G5A-f (Eq/Hash-vs-span god-type, G-5A.5) untested.** The invariant collision (saturation
  cache needs `Eq+Hash`; IR needs arena/`ExprId`/span/`Serialize`) — 5A must satisfy both in one
  type without a god-type. No prototype exercised the merged type under the saturation cache.

- **G5A-g (UTF-8/codepoint, Q2) untested.** `[α-ω]*` byte-expansion placement (keep `Class`
  byte-clean vs Unicode leaks into the shared enum) — unresolved.

**Assessment:** NOT BLOCKED, but its "dominate" is one axis of six. Axis-1-clean is real; the
lowering, the classifier, the automata, the bootstrap, and the `Look` consumer are open.

---

## FAMILY 5D — GRAMMAR-FRAGMENT (folded as 5A's lowering policy) — convergence **35%**

FAMILY-SPECS folds 5D into 5A under the portfolio "share-a-mechanism, share-a-family" rule — a
defensible call. But the fold **inherits none of 5D's evidence**, because 5D's decisive gates were
never in either brief:

- **G5D-a (the one-pass fraction, G-5D.4 — THE number, UNMEASURED).** The single figure that
  decides whether "no engine on the generator path" holds vs "the DFA stays load-bearing" was not
  measured. research-D's own maximal reading is **refuted by PROVE-10**; the surviving 5D is "the
  grammar IR gains a byte-recognizer that STILL compiles to DFA+prefilter." Without G-5D.4 the
  narrowing line is undrawn.

- **G5D-b (throughput head-to-head, G-5D.3 — UNMEASURED).** Straight-line class-scan vs the
  PROVE-10 prefiltered `Dfa::find`, cold-paired. research-D itself warns (R6): "if it merely ties,
  report the tie as a tie." No measurement exists; the "faster than a DFA (no indirection)" thesis
  is unbacked.

- **G5D-c (the FOLLOW-overlap gap research-D flags against itself).** `check_one_pass_eligible`
  currently tests FIRST-set disjointness across *alternations*; research-D R2 explicitly asks
  whether it also rejects a `Repetition` whose class overlaps its own FOLLOW (`[a-z]+z` on `abz`)
  and states: "if not, that is a named gap the desugar must close before trusting the flag." The
  gap was named, never closed. **5D routes on a predicate not yet verified sound for its own use.**

- **G5D-d (masked-fallback contradiction — the `_ => {}` wildcard).** PROTO-1 celebrates that
  `derive_recognizers` "already carried a `_ => {}` wildcard, so it absorbed `Class`/`Look` with
  no edit at all." But `derive_recognizers` is *the recognizer-selection pass* — the very site
  5D needs to choose `Recognizer::SimdScan` over `ByteClass` shapes. A wildcard **silently
  swallowing `ByteClass`** means the structural recognizer chooser 5D's whole SIMD-fast-path
  argument rests on **is not firing**. PROTO-1's "0 edit" win and 5D's "recognizer chosen by IR
  analysis over ByteClass" claim are mutually contradictory on the same line of code. Checklist:
  masked fallback.

- **G5D-e (BackendExpr::ByteClass emit unverified — consumer-less risk).** PROTO-1 adds the
  primitive and cascades 6 arms, but no test emits correct scan code from it; it may be as inert
  as `Look`. G-5D.1 ("zero `ExprKind::Regex` survive") is asserted from the 5A merge, but
  `regex_desugar` itself was never built.

**Assessment:** as a lowering policy it is architecturally sound and PROVE-10-floored, but every
decisive figure is unmeasured and one load-bearing predicate is self-flagged-unsound. 35%.

---

## FAMILY 5B — LAWFUL-FUNCTOR — convergence **45%**

**The elimination is unearned (checklist: vacuous convergence).** PROTO-1 declared 5B "unneeded"
having run **zero of 5B's four gates**. G-5B.1 (prism coverage number), **G-5B.2
(single-rule-drives-both — the central value: does the 434-LOC grammar-side copy actually die?)**,
G-5B.3 (round-trip DFA-equivalence oracle), G-5B.4 (zero-bridge-residue) are all unrun. "5A has
0 leaks" and "5B's write-algebra-once value materializes" are **different questions**; the first
does not answer the second. FAMILY-SPECS Q1 ("does the grammar-side copy actually die, or does
5B collapse toward 5A?") is the load-bearing uncertainty and it was never probed.

**Enumerated open gaps:**

- **G5B-a (the value-target does not exist in the graduating tree — cross-tree defect).** 5B's
  raison d'être is deleting the duplicated regex rules (`crates/ir/egraph/rules/regex.rs`, 434
  LOC) that live in the **top-level** tree. FAMILY-SPECS §0 confirms the **skinny (graduating)
  tree has NO grammar-tier regex e-graph** ("none inline; `backend_egraph.rs` seam"). So in the
  tree of record, 5B has **nothing to bridge and no duplicate to kill** — its entire mechanism
  presupposes substrate that skv22's graduating tree lacks. This is the FAMILY-SPECS §0 two-tree
  contradiction biting 5B specifically, and it is unresolved.

- **G5B-b (the win reaches only the shared sub-signature).** 5B's own risk register concedes
  `Class`/`Look` cross only as *atomic* nodes; cross-repo rewriting *inside* an atom is rejected.
  So the shared-optimization payoff is confined to `Concat/Alt/Repeat/Literal/Empty` — precisely
  the fragment 5A already unifies for free. If the coverage number (G-5B.1) is high, 5A is
  simpler; if low, the functor buys little. Untested either way.

- **G5B-c (span/projection loss — the true "reasonable point," unmeasured).** `F∘G` erases spans
  unless a side-table is threaded (MLIR source-materialization). research-B step 3 asks to
  "quantify what survives, decide by measured loss." No number.

- **G5B-d (LOCK-2 already claims the value 5B claims — double-home, see §Cross-lock).** LOCK-2
  Island A ("regex HIR canonicalization") explicitly names itself "LOCK-5 territory" but ALSO
  asks whether the 5-grammar fleet even *has* a rich regex leaf; if not, the regex-canon e-graph
  (the shared rewrite library 5B is built on) is "a LOCK-5-owned negative." 5B's whole edifice
  may be canonicalizing over three JSON leaves.

**Assessment:** coherently specified, NOT BLOCKED, every primitive cited-working — but its
convergence *as an adjudicated decision* is thin: provisionally eliminated without running its own
gates, and its substrate target is absent from the graduating tree. It cannot be retired on
current evidence; pass-2 must run G-5B.2 (build `FuseAltRegexBranches`, the named counterexample)
before "5B unneeded" is more than an assertion.

---

## FAMILY 5C — AUTOMATA-AS-EXTRACTION — convergence **62%**

**The best-evidenced family this pass** (PROTO-2 RAN & MEASURED, both halves). The negatives are
real science:
- **2A: EXACT** (26/26 byte-identical + language-equiv, cyclic included) but **3.53× slower**
  whole-corpus; congruence-acyclic **0.80×** on the 15% acyclic slice. Correctly reported NO-GO
  as a wholesale replacement.
- **2B: BLOCKED-confirmed** — ρ_max 1.667, **organic** miss `\w+(?:[-_]\w+)*` ρ=1.5; direct
  witness `simplify_hir((ab)*a) ≠ simplify_hir(a(ba)*)`. Theory-bounded, honestly filed.

These are genuine convergence — a family that *names its negative results* per its own charter.
But the family is not whole:

- **G5C-a (2C never prototyped — G-5C.3 UNRUN).** The tier-selection CSP — the family's "clean
  CSP GO," the one part with no fixpoint hazard — was **not built**. PROTO-2 covered only 2A/2B.
  The CSP-vs-table adjudication (research-C P3) is open, and it is **double-homed with LOCK-2**
  (§Cross-lock). The one part of 5C that could positively *ship* is the one part unmeasured and
  contested.

- **G5C-b (2A's shippable core is the elegant-reduction trap).** The surviving deliverable —
  "dispatch acyclic→congruence (0.80×), else incumbent" — covers **15% of patterns** and PROTO-2
  states the win needs "the alloc-light rewrite to matter beyond the 15% slice… **out of this
  probe's parsimony scope and unproven here.**" The load-bearing perf step is explicitly deferred:
  "…and then the hard part." Worse, the 0.80× is measured on the 4-pattern acyclic subset in
  isolation; **the cost of the `is_acyclic` dispatch check on the other 85% is not folded into any
  head-to-head.** Net benefit after dispatch is unmeasured and could be negative.

- **G5C-c (the premise "beat classical" fails at corpus scale).** The incumbent restart-Moore is
  O(n²·k) yet *wins* because the corpus is tiny DFAs (n≤16) where its alloc-light path beats the
  worklist's predecessor-lists/HashMaps. 5C's value proposition lives at large-n DFAs **that do
  not occur in the corpus**. The honest lock-level finding is: *automata-as-extraction does not
  beat the classical pipeline at the scales the engine actually sees.* That is a converged
  negative for the family's maximal claim — good — but it means 5C's positive contribution shrinks
  to a 15%-applicable fast path + two named bounds. The family's charter ("wins stay") yielded
  ~no wins.

- **G5C-d (corpus/gate mismatch).** G-5C.1 as written demands "every one of the **50**
  `regex_dfa_compile` patterns"; the file holds **26** (PROTO-2 discloses this honestly). The gate
  cannot be met as specified; either the gate is re-cut to 26 or the corpus is grown. Small but a
  born-RED gate must be satisfiable.

- **G5C-e (G-5C.5 determinism unaddressed).** Extraction/CSP tie-breaks for regen bit-identity —
  not exercised (2C wasn't built).

**Assessment:** 2A/2B are converged as bounded/negative results (the honest, valuable core). 2C —
the only would-be GO — is unrun and double-homed. 62% reflects strong evidence on two of three
operations and a contested void on the third.

---

## CROSS-LOCK VIEW (constitution ix — the double-homes LOCK-5 does not name back)

LOCK-5 is the **hub** the other locks defer to, yet its FAMILY-SPECS names none of them. Every
one of these was found by reading the *other* locks' specs; LOCK-5's own text is silent, which is
itself the gap.

1. **5A/5D ⊗ LOCK-9 9C (SCANNER-AS-DFA) — DOUBLE-HOMED, one-sided acknowledgment.** LOCK-9 9C is
   marked "**BLOCKED-candidate (cross-lock-coupled to LOCK-5)**" and states outright (LOCK-9
   FAMILY-SPECS:211-213): "9C's central pipeline — bootstrapped regex grammar as the DFA source of
   truth… — **is LOCK-5's deliverable, not LOCK-9's.**" 9C sources from `bootstrap/regex.bbnf` and
   **extends `regex/classify.rs:55 RegexClass`** — the *same* 1148-LOC classifier 5A/5D re-home,
   and the *same* quote/number/ws recognizers 5D's structural chooser selects. **The classify
   re-home and the SIMD-recognizer selection are claimed by both locks.** LOCK-9 defers; LOCK-5
   never reciprocates. GESTALT-P will inherit an unadjudicated double-home unless LOCK-5 names the
   supersession (recommend: LOCK-5 owns the HIR→DFA→classify substrate; LOCK-9 9C consumes it or
   retires).

2. **5C-2C ⊗ LOCK-2 scan-plan/tier CSP — DOUBLE-HOMED, contested.** LOCK-2 defers "regex HIR
   canonicalization" (Island A) to LOCK-5 ("Island A home = LOCK-5 territory", LOCK-2:97) — good.
   But LOCK-2 **also claims the scan-plan CSP**: "`SimdMode∈{Exact,Prefilter}` × `SimdSite∈
   {PreEntry,SelfLoop}` × kernel" (LOCK-2:78) and a prefilter cost model (LOCK-2:253, regex-automata
   `rare.rs`). That is **precisely 5C-2C** (per-scan-site `tier∈{Memchr,Shufti,Teddy,None}` by
   `Extractor`/`CostModel`). Two locks, one tier-selection CSP. Neither adjudicates. Worse, LOCK-2
   asks (Island A, LOCK-2:107) whether the grammar fleet has "a *real* regex leaf… If none, Island
   A is a **LOCK-5-owned negative**" — which would also gut 5B's shared-rewrite value (G5B-d).
   **The prefilter/tier CSP must be single-homed before either lock converges.**

3. **5A/5D ⊗ LOCK-3 IR tower — enum ownership uncoordinated.** LOCK-3 owns `ExprKind→BackendExpr`
   lowering (LOCK-3:20) and flags "**`ExprKind::Project` drops its type**" as its load-bearing
   defect. 5A/5D **mutate `ExprKind` (add `Class`/`Look`/`ByteClass`) and `BackendExpr` (add
   `ByteClass`)** — LOCK-3's tower. LOCK-3 explicitly makes 3C "BLOCKED-candidate until… LOCK-5
   rules on the functor" (LOCK-3:313) and requires "the IR grammar must be functor-compatible with
   LOCK-5's ONE-HIR" (LOCK-3:168). The dependency is one-directional in LOCK-3's text; LOCK-5 must
   name that its enum edits are LOCK-3-governed (typed-materialization invariant applies to the new
   leaves too — does `ByteClass` reach the tape emitter?).

4. **5A ⊗ LOCK-7 bootstrap — premise contradicted, substrate un-enumerated.** LOCK-7 (FAMILY-SPECS:
   30-36) found **two extant regex.bbnf grammars** (parse-that generating-then-discarding; a
   divergent `grammar/misc/regex.bbnf` breach). 5A/research-A assert "regex.bbnf is to-BUILD."
   **Both cannot be true.** LOCK-5's §0 two-tree table lists ~6 regex homes but omits these two
   grammar files — the substrate census is incomplete, and the actual 5A task (promote + de-dup
   divergent grammars, break the discard) is harder and differently-shaped than "author regex.bbnf."
   The "one-regex-home breach LOCK-5 owns" (LOCK-7:36) is LOCK-7 assigning LOCK-5 a cleanup 5A never
   scoped.

5. **5A/5B ⊗ LOCK-4 4D (GRAMMAR-SUGAR) — downstream coupling to the Axis-1 verdict.** LOCK-4 4D
   ("Regex meets 5D at `IrNode::Regex` — one HIR, not a second engine", LOCK-4:237) and its
   advancement note ("LOCK-5 pull toward 4D's one-IR", LOCK-4:296) make 4D **contingent on LOCK-5
   choosing ONE-HIR**. If LOCK-5 reopened 5B, 4D weakens. LOCK-5's Axis-1 choice is load-bearing
   for at least LOCK-4-4D, LOCK-9-9C, and LOCK-3-3C — three downstream locks blocked on a verdict
   that is currently *asserted* (5A dominates) rather than *earned* across its gates. That fragility
   is a cross-lock convergence risk: three locks would inherit a verdict resting on one axis.

6. **Type-plane cross-lock (LOCK-1).** PROTO-1 graded `Class→Builtin(Bytes)` trivial against
   skinny's stub `BuiltinTy`. **LOCK-1 owns the real type system.** Whether `Class`/`Look` type
   cleanly under HM+row-poly+widths is LOCK-1's ratification, not settled by the skinny stub
   (G5A-b). LOCK-5 must route the `Class`/`Look` typing verdict through LOCK-1.

7. **rust/ts isomorphism of the regex HIR (LOCK-6) — entirely unaddressed.** Constitution ii
   (regex HIR isomorphic to bbnf HIR/IR) is treated as a *rust-side* claim throughout LOCK-5. The
   **rust/ts** face (constitution iii, LOCK-6) of the merged/functored regex HIR — how `Class`/
   `Look`/`ByteClass` and the automata cross to TS (PROVE-14 tape) — is never mentioned. If 5A
   merges regex into `ExprKind`, LOCK-6's mirror/skeleton must carry the merged enum; no family
   scoped this. Named gap for GESTALT-Ω.

---

## FAILURE-MODE CHECKLIST — HITS

- **Vacuous convergence:** 5B declared "unneeded" with 0/4 of its own gates run (G5B-a..d).
- **Spec-cites-itself / circularity:** regex.bbnf bootstrap (regex grammar defines regex; grammars
  contain regex) deferred to LOCK-7; premise SPEC-ONLY *and* contradicted by two extant grammars.
- **Gate that cannot fail:** G-5A.1 "0 arms need regex-specific dispatch" passes partly because
  `Look` is inert — the gate rewards consumer-lessness (G5A-a).
- **Elegant-reduction trap:** 5A "classify becomes an IR analysis pass" (1148 LOC re-home, unbuilt,
  G5A-c); 5A "automata at PROVE-10 numbers" (G-5A.3 unrun, G5A-d); 5C-2A "congruence fast path +
  [the alloc-light rewrite, unproven, out of scope]" (G5C-b).
- **Legacy aliases / masked fallbacks:** `derive_recognizers` `_=>{}` wildcard silently swallows
  `ByteClass`, contradicting 5D's recognizer chooser (G5D-d); 3-to-5 divergent regex homes persist.
- **Unverified gestalt:** two-tree (in fact three-snapshot: top-level, skinny HEAD, proto's
  `d2b32f6e3`) reconciliation deferred; tax census run on the stalest tree (G5A-b).
- **Consumer-less substrate (Y.13):** `Look` (G5A-a); `BackendExpr::ByteClass` emit unproven
  (G5D-e).
- **Constitution violations:** (i) grammar-driven regex SPEC-ONLY lock-wide; (v) zero-runtime-cost
  of the merged-type automata path unverified; (vii) verdict lines omit the SPEC-ONLY label on
  bootstrap/automata; (viii) `Look`; (ix) LOCK-5 names none of its five downstream double-homes.
- **Prototype-evidence misuse:** 8-variant-tree tax census generalized to a whole-architecture
  "dominate"; PROVE-10 numbers cited to anchor 5A's automata claim without running G-5A.3.

---

## PASS-2 DIRECTIVES (what must run before any convergence talk)

1. **Run 5B's G-5B.2 counterexample** (`FuseAltRegexBranches` mixing `Regex`+`Literal` grammar
   leaves): does the grammar-side rule copy actually die, or does 5B collapse to 5A? Until this
   runs, "5B unneeded" is struck from the verdict.
2. **Measure the one-pass fraction (G-5D.4) and the class-scan-vs-prefiltered-`Dfa::find`
   throughput (G-5D.3)** on the real leaves + engine corpus, and **close research-D R2's
   FOLLOW-overlap gap** before trusting `check_one_pass_eligible`.
3. **Build 5C-2C** (tier CSP) and adjudicate it against a cost-sorted table **and against LOCK-2's
   scan-plan CSP** — single-home the tier selection.
4. **Prove a grammar-side `Look` consumer** or demote `Look` to `Annotation` (retire the Y.13
   candidate); prove `derive_recognizers` actually selects on `ByteClass` (retire the masked
   wildcard).
5. **Re-cut 5A's §0 substrate census** to include `parse-that/rust/bootstrap/regex.bbnf` and
   `grammar/misc/regex.bbnf`; re-scope 5A from "author regex.bbnf" to "promote+de-dup+break the
   discard," and carry the SPEC-ONLY(bootstrap) label until 7A lands.
6. **Route the `Class`/`Look` typing verdict through LOCK-1** (not skinny's `BuiltinTy`); re-run
   the arm census on current graduating HEAD (10 variants), not `d2b32f6e3`.
7. **Name, in LOCK-5's registry, every cross-lock double-home** (9C, LOCK-2 tier CSP, LOCK-3 enum,
   LOCK-7 bootstrap, LOCK-4 4D, LOCK-6 TS face) as an explicit supersession/interlock, per
   constitution ix.

**CRIT-2 verdict:** 5A 58% (Axis-1 earned, five gaps open), 5D 35% (folded; decisive numbers
unmeasured), 5B 45% (coherent but eliminated-without-testing; substrate absent from graduating
tree), 5C 62% (2A/2B converged as honest negatives; 2C unrun + double-homed). Lock-wide: the
grammar-driven premise is SPEC-ONLY, and the hub does not map its five downstream double-homes.
No family may be called converged this pass.
