# 30 — Completeness Critic: Coverage Map, Gaps, Contradictions

Audit of the 28-report research forest (01–28) plus three adversarial
verification reports (29-verify-0/1/2) against the twelve dimensions the driving
directive demands. The question is not whether the reports are good — most are
sharp and adversarial — but whether, taken together, they answer everything the
synthesis must answer, agree where they overlap, and avoid certifying prose in
place of finding.

The headline: **two of the twelve demanded dimensions have no owning report**,
**one cross-report contradiction on Lock-14 status is unresolved**, and **the
single most important synthesis question — go/no-go on beginning totality
implementation — is owned by nobody.**

---

## 1. Dimension-to-report coverage map

| # | Demanded dimension | Owning report(s) | Depth |
|---|---|---|---|
| 1 | Full history (pre-restart, pivot, skinny) | 01, 03 | DEEP |
| 2 | **Cutover decision (when to begin totality)** | **none** | **ABSENT** |
| 3 | Overfitting audit | 07, 08, 09, 10, 12, 13 | DEEP (saturated) |
| 4 | Meta-process / local-maxima | 04 | DEEP |
| 5 | parse-that fold + parsing primitives | 17 | DEEP |
| 6 | Totality crate/module structure | 23 | DEEP |
| 7 | Rust+ASM emission (dav1d-inspired, >SOTA) | 18, 19, 20 | DEEP |
| 8 | Full type resolution (CSP/egraph/HM) | 21 | DEEP |
| 9 | Robust Value API | 22, 26 | DEEP |
| 10 | Recap of ALL precepts with status | 06 | DEEP |
| 11 | Chronically-deferred fold-in | 05 | DEEP |
| 12 | **What is NOVEL / best-in-class** | **none (incidental only)** | **ABSENT** |

Ten dimensions are owned and deep. Two — **#2 (cutover) and #12 (novelty)** —
are structurally absent. These are the gaps the synthesis must fill or
explicitly own, because no sibling report did.

---

## 2. GAP-1 (RANK 1): the cutover decision has no owning report

The directive lists "the cutover decision — when to begin totality
implementation" as dimension (2). A keyword sweep of all 28 reports for
`cutover|cut-over|when to begin|begin totality|kickoff|first wave|sequencing`
returns **zero substantive hits**. The closest any report comes is report 05,
which treats the D-G pre-block reconcile as a *hidden gate* blocking substrate
waves (`05-deferred-census.md:184,217-218`) — but as a deferred-item entry, not
as the affirmative question "is the campaign ready to start building totality,
and on what evidence?"

Every report answers "what is wrong NOW." None answers "what must be TRUE before
W0 of the MASTER-PLAN A-tranche may begin, and is it true?" This is the single
most consequential synthesis question, and it is orphaned. The synthesis must
construct the cutover criterion from scratch out of the other reports' evidence:

- The blocking preconditions exist as scattered facts: BackendShape absent from
  totality (verify-2 Claim 05, report 24), no type inference (report 21), no
  unified Value API (report 22), the substrate cluster un-folded (report 28),
  the decision-engine cost model is theatre (report 24), nine of sixteen Locks
  violated (report 02).
- The "ready" signals also exist: parse-that already folded (report 17), the
  recognizer layer largely clean post-W6 (report 11), the skinny SoA tape proven
  and portable (report 28), the crate target-graph drawn (report 23).
- But **no report weighs these into a go/no-go**, nor proposes the gating
  predicate (e.g., "W0-W2 substrate fold + Value-API unification must land in
  crates/ before any A-tranche wave"). The synthesis owes this construction.

Recommendation: the synthesis must author the cutover predicate as a
first-class deliverable, not infer it implicitly. The idiomatic form is a hard
runtime gate (a bench/oracle that must be green in crates/), mirroring the
no-deferral and AX-invariant-13 precepts — not a documentation milestone.

---

## 3. GAP-2 (RANK 2): "what is NOVEL / best-in-class" has no owning report

Dimension (12) demands a verdict on what is genuinely novel about the approach
and whether it is best-in-class. A sweep for `novel|best-in-class|differentiat`
finds the word "novel" used three times, all incidental: a "structurally novel
third grammar" (`03-sk-arc-ledger.md:260`), a "novel ident stem"
(`11-ir-recognizer-residual.md:26`), and "genuinely novel" applied to one dav1d
refill pattern (`19-dav1d-emitter-design.md:212`). **No report sets out the
prior-art landscape** (Yacc, ANTLR4, tree-sitter, pest, lalrpop, the simdjson /
sonic-rs / yyjson hand-written codegen lineage, the Parol/peg-generator family)
and asks whether "a fully generalized parser generator that emits grammar-derived
NEON-accelerated parsers from a .bbnf file" is actually new, or a reframing of
known techniques.

This is a real blind spot because the campaign's entire justification is the >SOTA
claim. Reports 14 and 20 audit whether the *benchmark numbers* are honest, but
**no report audits whether the THESIS is novel** — i.e., whether grammar-derived
SIMD emission is a genuine advance over (a) tree-sitter's table-driven generated
scanners, (b) simdjson's hand-written-but-not-generated stage-1/stage-2, or
(c) the "generate a specialized parser per grammar" idea that pest and ANTLR
already embody without SIMD. The adversarial framing the synthesis must supply:
**the novelty is narrow** — it is specifically "*generated* SIMD structural
classification driven by grammar facts, with a backend-agnostic IR and a unified
lazy/eager value plane" — and even that is only novel if it ships, which per
reports 21/22/24/28 it has not in crates/. Best-in-class is currently a claim
about two grammars under a disclosed thermal asymmetry (report 14), not a proven
general property.

Recommendation: the synthesis must add the prior-art teardown and state the
novelty claim at its true (narrow) scope, with the honest caveat that an
unshipped generalization is not best-in-class at anything.

---

## 4. Unresolved contradiction (RANK 3): Lock-14 status — "clean" vs "RED in substance"

Three reports touch the same fact and do not agree:

- **Report 11** frames the post-W6 recognizer layer as "largely clean: Lock-14
  passes, shape keying is structurally grounded... rather than rule names"
  (verdict line), with the W6e gate green.
- **verify-1 Claim 02** independently re-derives that the gate is "green by
  exclusion": `GrammarAuditTag` (bare `Json`/`CssL4`/`Sheets`/`Bbnf` variants at
  `payload_coverage.rs:83-86`) and the grammar-NAMED module `shape_dict_bbnf.rs`
  both escape the roster and remain grammar-specific code in a generic crate —
  "Lock 14 is RED in substance despite the W6e gate being green."
- **verify-0 Claim 06** confirms the gate scans only `crates/ir/src` +
  `crates/analysis/src` and provably excludes the xtask emitter that still
  hardcodes `"css_l4"`/`"CssL4Parser"` — the D-7 green-by-exclusion anti-pattern.

Report 11 does mention the roster narrowness and lists `shape_dict_bbnf` as a
naming residual, so it is not blind — but its top-line "largely clean / Lock-14
passes" reads more favorably than the two independent verifications support. The
synthesis must reconcile to the harsher, evidence-backed reading: **the W6e gate
is not proof of Lock-14 closure; it proves only that full parser-struct idents
are absent from non-comment src lines.** Report 02 already counts Lock-14
VIOLATED in totality; report 11's "nominally satisfied" framing should be
subordinated to that.

A second, smaller contradiction: **verify-1 Claim 04 REFUTES** a sibling claim
that `grammar_facts.rs` + `shape_dispatch/scalar.rs` carry live residuals —
those are doc-comment-only at HEAD. If any synthesis input cites those two files
as live violations, it is stale and must be dropped in favor of the
`shape_dict_bbnf` + `GrammarAuditTag` residual.

---

## 5. Citation errors the verifiers caught that must not propagate

The three verification reports flag four materially-wrong attributions in the
source forest. The synthesis must use the corrected values:

1. **"0/19" bench gate is wrong** — the primary FINAL docs record **0/17** parse
   entries (verify-0 Claim 01; `FINAL-III.md:57`, `FINAL-IV.md:80-100`). The
   "0/19" exists only in a secondary archaeology table that contradicts the
   close records. Report 01 rides the 0/19 figure; the synthesis must say 0/17.
2. **"entirely fake CSS admissions" originate in SK-V12, not SK-V13** (verify-0
   Claim 03). The source report itself grades SK-V13 as zero-code. Mis-attributing
   the fraud origin to SK-V13 repeats an owner the evidence refutes.
3. **The ~78,500 LOC DTA/PSI reclaim is a bundled projection** (verify-2 Claim
   01) — interpreter source is ~12k LOC; the dominant 57,481 is generated walker
   output. Report 01 / the 01-divergence verdict attribute the whole figure to
   the interpreter; the synthesis must not say "78,500 LOC interpreter."
4. **The "§13.1 x86 vs LOCKS:635 direct contradiction" is overstated** (verify-2
   Claim 02) — both operate on different objects (literature retention vs source
   deletion); it is a precision-gap, not a contradiction or a staleness
   invalidation.

These are not nitpicks: each is a number or owner a downstream planner would
repeat. The synthesis is the propagation chokepoint.

---

## 6. Coverage imbalance: overfitting is 6×-saturated; novelty/cutover starved

Six reports (07, 08, 09, 10, 12, 13) attack dimension (3), the JSON/CSS
overfitting audit, with heavy overlap — every one re-derives the same family of
grammar-name leaks, hardcoded literal bytes, and Lock-14 firewall residuals from
a slightly different file vantage. This is thorough but redundant: the synthesis
will fold six reports into one finding ("grammar names leak into generic crates
at N sites across emitter, IR, xtask, runtime, and the firewall ledger itself").
Meanwhile dimensions (2) and (12) got zero reports each. The forest over-indexed
on the easy, evidence-dense overfit hunt and under-indexed on the two
forward-looking judgment dimensions. The synthesis should compress 07–13 into a
single overfit ledger and spend the reclaimed attention on cutover and novelty.

---

## 7. Questions the synthesis must answer that NO report answers

1. **Go/no-go on totality cutover, with the gating predicate** (GAP-1). Nobody
   owns it.
2. **Is the core thesis novel vs. tree-sitter / pest / simdjson-codegen, and at
   what scope?** (GAP-2). Nobody owns it.
3. **What is the minimum shippable totality slice that proves the generator
   thesis in crates/?** Reports 22/24/28 each describe one un-folded piece
   (value plane, decision engine, substrate) but no report sequences them into a
   single first-landing wave. The cutover answer depends on this and it is
   un-synthesized.
4. **Reconciled Lock-14 status** (Section 4) — "passes" vs "RED in substance"
   must collapse to one verdict.
5. **Does beating two grammars under a disclosed thermal asymmetry (report 14)
   constitute "best-in-class" per the directive's precept (3)?** The honest
   answer is no — but no report states it, because report 14 audits number
   honesty and report 20 audits the SOTA gap; neither adjudicates the
   *generalized* best-in-class claim across the nine-grammar corpus where five
   grammars have zero correctness oracle (report 15).

---

## 8. Reports whose verdict-tone trends toward "clean bill" — re-audit flags

The directive warns against verdicts that smell of praise. Two are borderline:

- **Report 11 (ir-recognizer-residual)** — verdict "largely clean." It DOES
  carry four concrete defects (`infer_associativity` hardcodes `"^"` as
  right-assoc at `grammar_facts.rs:1517`; `bbnf_shape_templates` mined-but-unused;
  dead `CounterOptional::Lookahead`; wrong-file panic message), so it is not
  hollow. But its top-line favorability is contradicted by verify-1 (Section 4).
  Subordinate the "clean" framing to the RED-in-substance reading.
- **Report 12 (json-courier-audit)** — verdict says "The full-tree (crates/) has
  zero violations." That is a clean bill for one half of the tree. It is
  evidence-backed (the @generated headers there are genuine xtask outputs), so it
  survives — but the synthesis should pair it with report 13's finding that the
  xtask pipeline producing those "clean" files IS the per-grammar fork, so the
  "zero violations" is true at the file level and false at the system level.

No report reads as pure praise; the forest is adversarial. The risk is the
*aggregate* tone — when the overfit cluster keeps finding the same residuals and
the recognizer layer is called "clean," a reader could conclude the tree is
closer to ready than reports 21/22/24/28 (all "does not exist / theatre /
un-folded") establish. The synthesis must lead with the four absent-architecture
findings, not the residual-hunt.

---

## 9. Process notes (out-of-band)

- **Reports 02 (526 lines) and 03 (351 lines) exceed the 350-line cap** the
  dispatch imposed. This is itself an instance of the report-04 finding that the
  pass produces documentation faster than the cap allows — the completeness pass
  reproduces the meta-process pathology it audits.
- The forest has **three verification reports against six load-bearing claims**
  (18 claims total) but **28 audits** — coverage of ~64% of one claim per audit.
  Several load-bearing claims (the value-API three-substrate count, the
  type-system vaporware claim, the decision-engine cost-theatre claim) were NOT
  independently verified. The synthesis should treat reports 21/22/24 as
  high-confidence (concrete `rg`-able absence claims) but flag that they are
  un-cross-checked.

---

## 10. Ranked missing-work list

1. **CUTOVER DECISION (GAP-1)** — author the go/no-go predicate and the minimum
   shippable totality slice. No owning report. Highest synthesis priority.
2. **NOVELTY / BEST-IN-CLASS (GAP-2)** — prior-art teardown + narrow-scope
   novelty claim + honest "not best-in-class until it ships generally." No
   owning report.
3. **RECONCILE LOCK-14 STATUS** — collapse report 11 "clean" vs verify-1 "RED in
   substance" to the harsher evidenced verdict; drop the refuted
   grammar_facts/scalar.rs file attribution (verify-1 Claim 04).
4. **CORRECT FOUR PROPAGATING CITATIONS** — 0/17 not 0/19; SK-V12 not SK-V13;
   78,500 is bundled not interpreter; §13.1 is a precision-gap not a
   contradiction.
5. **SEQUENCE THE UN-FOLDED ARCHITECTURE** — reports 22/24/28 each describe one
   absent piece; no report orders them into a first-landing wave. The cutover
   answer depends on this sequence.
6. **COMPRESS THE 6× OVERFIT CLUSTER (07–13)** into one ledger; reclaim attention
   for the two starved dimensions.
7. **ADJUDICATE BEST-IN-CLASS AGAINST THE FULL CORPUS** — two grammars beaten
   under disclosed asymmetry, five grammars with zero oracle (report 15); state
   plainly this is not corpus-wide best-in-class.
8. **FLAG UN-CROSS-CHECKED LOAD-BEARING CLAIMS** — the type-system, value-API,
   and decision-engine verdicts were not independently verified; mark confidence.
