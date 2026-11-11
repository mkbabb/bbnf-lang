# LOCK-8 — THE TS BACKBONE PLANE · pass-1 · ADVERSARIAL CRITIC 2 of 2

*Critic 2, cross-lock-weighted. Read: PORTFOLIO.md (LOCK-8 charter + CONSTITUTION), FAMILY-SPECS.md,
research-{A,B,C,D,priorart}, PROTO-1 (+patch, +evidence traces), PROTO-2 (+patch), and the sibling
lock artifacts (LOCK-4 FAMILY-SPECS + CRIT-2, LOCK-6 FAMILY-SPECS) for the double-home read. Charter
applied verbatim; convergence is EARNED; this is pass 1 — no family is near converged. Two prototypes
ran and both are genuine, refutable, concrete artifacts (constitution vii satisfied). Neither settles
the family's terminal deliverable, and both leave the charter's own named items (packrat; PT-E1
scoping MECHANISM) undesigned.*

---

## 0. THE HEADLINE FINDING (cross-lock weighted)

**Two of LOCK-8's four families terminate on a mechanism that is homed in another lock and is
UNCONVERGED there.** 8A center B's terminal move — "make constant-capture a construction-time /
build-time specializer (**the V8 face of LOCK-4B**)" (FAMILY-SPECS:41) — and 8C-ambitious's only
lawful door — "a **build-time-generated specialized dispatcher (LOCK-4B)**" (FAMILY-SPECS:204,294) —
are the *same* mechanism: the TS build-time source fuser that generalizes `leaf.ts:154-273` to any
arity + nesting. LOCK-4B FAMILY-SPECS:115-118 names that fuser as **4B's home**; LOCK-4 CRIT-2:25,110
records that the TS-execution number for that fuser is **unrun and the deciding axis unmeasured this
pass**. So LOCK-8's two hardest families reduce to "the easy part is proven [hand arities inline,
PROTO-1] **and then** the build-time specializer that generalizes it [LOCK-4B, unbuilt, unconverged,
double-homed]." That is the **elegant-reduction trap** operating *across a lock boundary*: PROTO-1
retired the precondition (hand-unrolled arms inline), the synthesis reads it as retiring the pivot,
but the load-bearing generalization — and its code-size budget, and the fact it need only fire for
SFI-heterogeneous sites — is neither built nor owned here.

**Second cross-lock finding: 8C is very nearly a re-homing of LOCK-6, not an orthogonal center.**
8C's three strata are Stratum A (one spec → Rust dumper + TS decoder) = **LOCK-6B SPEC-GENERATED-
SKELETON**; Stratum C (behavioral differential over a shared corpus) = **LOCK-6D ORACLE-ISOMORPHISM**;
and "the isomorphism LINTER" (name-map + CI tree correspondence + EXEMPT rows) = **LOCK-6A MIRROR-TREE**
verbatim. LOCK-6A FAMILY-SPECS:68 *explicitly claims the parse-that combinator core as its named case*;
LOCK-6B:124 *explicitly recasts GN-4.W4's tape port as its generated deliverable*. The portfolio's
intent is that 8C **consumes** LOCK-6's verdict (PORTFOLIO:200-202) — but FAMILY-SPECS writes 8C as
**owning** the strata, the generator, and the linter. As written, GESTALT-P finds the linter and the
spec-generator double-homed between LOCK-6 and LOCK-8C. This is nameable now, not at gestalt.

The honest pass-1 state: **8A and 8B produced real measurements that de-risk their preconditions; 8C
and 8D produced strong recon but no prototype; and every family carries at least one load-bearing step
that reads *and-then-the-hard-part*.** The pass-2 registry must not read the two green protos as a
mandate — the terminal deliverables are still open, and two of them are owned by other, unconverged
locks.

---

## 1. FAMILY 8A — THE SHAPE-STABLE CORE (V8 runtime plane)

**Credit earned.** PROTO-1 is a genuine, refutable, cold-deterministic VM-facts probe (constitution
vii, iv-honesty). It confirmed (a) the `Object.create` view is a distinct hidden class and the
fixed-shape record collapses all views (`%HaveSameMap` false→true), (b) the arity-3 child parser
inlines while the general arm's does **not** for SFI-heterogeneous leaves, (c) zero steady-path
deopts. Gate 5 honors the ~+5% honest-number law rather than a ≥10% gate (SPEC §0). This is the
family that did the most real work this pass.

**Gaps (adversarial):**

1. **The terminal deliverable is unbuilt and double-homed (the headline).** PROTO-1 validated
   *hand-written* arity-3 named replicas. 8A center B's actual deliverable is a **build-time
   specializer** generalizing this to arbitrary arity + nesting across value.js's 61 `all()` sites —
   which is LOCK-4B's TS fuser, unrun on the TS side (§0). PROTO-1 proves the easy half; the load-
   bearing half is neither prototyped here nor converged in its home lock. 8A's disposition ("pivot
   retired, ADVANCE") overstates this.

2. **The `StateView` fix is asserted behaviorally safe and CONTRADICTS 8D's field contract —
   unverified.** The patch replaces `Object.create(state)` (which inherited **all** ParserState fields
   + the `getLineAndColumn` **method** via prototype) with `new StateView(src, value, offset, isError,
   furthest)` — **five data fields, no methods**. It **omits `expected` and `getLineAndColumn`**, which
   8D declares are part of the consumer-read field triple (FAMILY-SPECS:244,264). Any `.mapState` fn
   that reads `view.expected`, or calls `view.getLineAndColumn()` (now a `TypeError` — the method is on
   `ParserState.prototype`, absent on `StateView`), silently breaks. The patch comment "Carries exactly
   the fields a mapState fn reads" is an **unaudited correctness claim** — PROTO-1 measured only
   `%HaveSameMap`, never ran value.js/latex-paper against the patched `mapState`. This is a masked-
   correctness gap and a **missed entry in the incompatibility map** (8A ⟂ 8D on the StateView field
   set, absent from the map at FAMILY-SPECS:291-297).

3. **The `StateView` fix introduces a double force-cast leak that 8B must then fight.** The patch ends
   `fn(state, oldView as unknown as ParserState<T>)` — an `as unknown as` double cast is exactly the
   unsound-cast class 8B exists to eliminate. 8A's "one genuinely new source change" manufactures a new
   `Parser<any>`-grade hole at the seam. 8A and 8B are declared "non-conflict, orthogonal"
   (FAMILY-SPECS:297) — this cast refutes that: 8A's own fix violates 8B's soundness thesis.

4. **Gate 4 is mis-specified against its own proto.** Gate 4 reads "general arm's `parsers[i].parser`
   does **not** inline" (FAMILY-SPECS:66) — but PROTO-1 proved the general arm **does** inline for
   SFI-**homogeneous** leaves. On a homogeneous corpus gate 4 falsely fails. The gate needs the
   "SFI-heterogeneous leaf list" qualifier PROTO-1 discovered; as written it gives wrong verdicts, and
   the specializer's scope (only heterogeneous sites need unrolling) is not folded back into center B.

5. **The temporal leg for the inlining win is unmeasured; 8A's own "two legs" doctrine is half-run.**
   Center D declares "structural + temporal are the two legs; neither alone suffices" (FAMILY-SPECS:50).
   PROTO-1 ran only the structural leg (inlining fires). PROVE-11's temporal number is for the *lazy-
   field* patch, not the constant-capture/inlining change. So the throughput attributable to inlining —
   bounded to ~+5% by gate 5 — is still unmeasured. "8A PASS / thesis HOLDS" is a structural verdict
   dressed as a whole one.

6. **Code-size budget unaddressed.** Unrolling every heterogeneous `all()` site to N monomorphic call
   sites (× 61 sites, some high-arity) is exactly the generated-size regression the constitution's
   parsimony clause (vi) and the generated-size-budget law guard. 8A names no ceiling on the emitted
   specialization.

**Convergence: 55%.**

---

## 2. FAMILY 8B — TYPE-LEVEL TOTALITY (type plane)

**Credit earned.** PROTO-2 is a real, refutable measurement: 0 new TS2589, error set byte-identical
across baseline↔candidate on value.js's real 259-file parsing graph, 1.042× instantiations, `.test-d.ts`
GREEN-on-candidate / RED-on-1.0.0 (a genuine positive control, not a vacuous gate), and **empty dist
via SHA-identical emit** — constitution v (zero runtime cost) *actually proven*, not asserted. The
`Parser<T,E>` frontier is correctly pre-BANKED as a named negative-result (Y.13 discipline).

**Gaps (adversarial):**

1. **The headline — "no `Parser<any>` anywhere" — is untouched by the proto.** PROTO-2 added *output*
   precision to exactly three combinators (`string`, `all`, `any`) via `const S`. It removed **zero**
   of PT-E2's enumerated `Parser<any>` **input** leaks — the actual charter problem. PROTO-2's own
   finding concedes "the precision uplift is carried **entirely** by `string`'s `const S`"
   (PROTO-2:54). The full typed algebra (§2b: `then: Parser<[T,S]>` dropping the unsound `S|T`, `or`,
   `chain`, `opt`) is **unprototyped**, and each carries its own instantiation / TS2589 risk. The
   budget PASS is for a thin slice, not the totality the family is named for.

2. **The spec and the proto DISAGREE on `all`, and the disagreement is the hardest correctness case.**
   FAMILY-SPECS:109 specifies `all(...Ps): Parser<DropUndefined<Values<Ps>>>`. The PROTO-2 patch ships
   `all → Parser<Values<T>>` — **no `DropUndefined`**. `DropUndefined` is defined in `types.ts` and
   **never used** — consumer-less substrate (constitution viii / Y.13) inside the prototype. So the
   proto's "PASS" does not cover the signature the spec actually mandates.

3. **The `opt()`-in-`all()` type is currently WRONG and was never exercised.** With `Values<T>`,
   `all(a, opt(b), c)` types as `[A, B|undefined, C]` while runtime returns `[A,C]` **or** `[A,B,C]` —
   a length-variable tuple the fixed type lies about. `DropUndefined` unconditionally drops → `[A,C]`,
   equally wrong for the b-present case. The synthesis admits this (open Q i, R1) as "the hardest
   correctness problem" — yet PROTO-2 declared 8B PASS **without testing a single `opt`-in-`all` site**,
   and it is unconfirmed whether value.js even *has* such a typed site to expose it. The "strictly more
   precise" claim is true; "correct" is false for R1, and R1 is unmeasured.

4. **latex-paper native budget unmeasured.** The budget gate names "real value.js **+ latex-paper**
   grammars" (FAMILY-SPECS:129). PROTO-2 ran value.js natively but latex-paper only as a bounded
   1.0.0-family overlay (pin is `^0.7.1`) — "not a native compile" by its own caveat (PROTO-2:50). One
   of the two named budget corpora is not actually measured.

5. **The diagnostics-typing MECHANISM is unresolved and demoted by 8D.** 8B "ADOPTs the result-union
   as *the* typed outcome" (FAMILY-SPECS:116). The incompatibility map's own resolution demotes it to a
   *secondary* `.withDiagnostics` surface behind 8D's field-triple (FAMILY-SPECS:293), and defers to
   pass-2 whether both live. So 8B's stated mechanism for "diagnostics typed by position" is not
   actually adopted — it is a pending adjudication, and two coexisting surfaces strain parsimony (vi).

**Convergence: 50%.**

---

## 3. FAMILY 8C — THE ISOMORPHIC MIRROR (correspondence plane)

**Credit earned.** The data plane rests on **CONFIRMED** substrate (SPEC §0 row 14 / PROVE-14: byte-
exact tape decode, stop-early 4.4–4.6×, full-walk 0.91× native-won). 8C correctly refuses the wasm-
core-everything overreach the same measurement refutes, and correctly BANKS the ambitious runtime-
dispatch arm as refuted-on-arrival by C1 (−14%). Good measured discipline.

**Gaps (adversarial):**

1. **Double-homed with LOCK-6 across all three strata + the linter (the headline §0).** Stratum A =
   LOCK-6B; Stratum C = LOCK-6D; "the isomorphism LINTER" = LOCK-6A — and LOCK-6A/6B *explicitly* claim
   the parse-that surface and the GN-4.W4 tape port. 8C is written as **owner** of the generator and
   linter; the portfolio intends it as **consumer** of LOCK-6's verdict. Until re-scoped to "the parse-
   that FACE contributing the three named divergences as EXEMPT rows," GESTALT-P inherits a genuine
   double-home. This is not a gestalt-only concern; it is a pass-1 mis-scope.

2. **Tape-crossing home ambiguity.** 8C homes the crossing at `crates/bbnf-bench/…/ts-tape/`
   (FAMILY-SPECS:191). LOCK-6B/GN-4.W4 home it at `ts/tape/{tape,load,pointer}.ts` (LOCK-6
   FAMILY-SPECS:123-124,191). Two directories for one artifact — the physical home is unadjudicated
   between the locks.

3. **No prototype this pass; gate 3 is SPEC_ONLY leaning on PROTO-1 by analogy.** Gate 3 ("generated
   node-vocabulary dispatch holds the arity-unroll floor **or is refuted**") asserts a *generated*
   dispatcher inlines like the hand arms. PROTO-1 tested **hand replicas**, not generated code. The
   generated-dispatcher inlining is the same unbuilt/unmeasured mechanism as 8A gap 1 — a perf-adjacent
   claim with no artifact of its own. Under constitution vii this is SPEC_ONLY and must be labeled so,
   not carried as an ADVANCE-grade gate.

4. **The three "structural divergences it must classify" have no verification that classification is
   sufficient.** Rust's `SpanParser` has no TS twin (R-B); regex is oracle-only (R-C); diagnostics are
   behavioral-mirror-only (R-D). Each is asserted as an EXEMPT row, but the linter that would *enforce*
   the exemptions is itself the double-homed 6A deliverable — so the mechanism that makes these
   "classified not silently divergent" is not yet owned anywhere concretely.

**Convergence: 45%.**

---

## 4. FAMILY 8D — CONSUMER-CONTRACT-FIRST (API plane)

**Credit earned.** The strongest recon in the lock: value.js is really 4.0.0 and **bundles** parse-that
(the semver inversion, constitution ix named supersession), the nine-symbol contract with two below-
combinator seams is concrete and cited, and the gates are deliberately anti-vacuous — the mandatory
**rebuild-value.js-dist** step (R-8D4) and the three-step latex matrix defeat the stale-bundle false
green. This family understood the failure-mode checklist.

**Gaps (adversarial):**

1. **Open question (i) blocks the family's own make-or-break gate.** "How exactly does value.js resolve
   parse-that at build time … must be pinned before any suite runs" (FAMILY-SPECS:275). The crater
   harness (gate 4) — the family's central falsifier — **cannot be constructed** until (i) is answered,
   and (i) is unanswered. The family is "pure construction," but the construction is currently blocked
   on an unresolved mechanical fact.

2. **The hardest consumer (latex-paper's real 0.7→1.1 jump) is entirely unmeasured, and the one probe
   that touched it flashed warning.** Gate 4's three-step matrix (0.7→1.0, then 1.0→1.1) is born-RED and
   unrun. PROTO-2's latex overlay was bounded 1.0.0-family only and *already surfaced* "4 errors …
   0.7.1→1.0.0 API-drift artifacts" (PROTO-2:50) — i.e., the real jump has breaks, and they are
   **unenumerated**. The MIGRATION.md symbol-delta the family promises has no input yet.

3. **PT-E1 per-parse-scoping — the charter's named diagnostics design problem — is a CONTRACT without a
   MECHANISM.** 8D specifies *what* is readable (the field triple, on the returned state) and *what must
   not happen* (no 0.8.2 console leak). It does **not** design *how* the field triple is per-parse-
   scoped without a hot-path tax (context object? reset-on-entry? null-scope read?). The charter names
   "the diagnostics plane (PT-E1 per-parse scoping)" as a design problem; 8D delivers the consumer
   contract but leaves the scoping mechanism asserted.

4. **The 8B ⟂ 8D reconciliation is deferred, and 8A's StateView aggravates it.** The additive
   resolution (field-triple + separate result-union) is pushed to pass-2. Meanwhile 8A's StateView
   drops two of the three contracted fields (§1 gap 2) — so the field-triple 8D is protecting is
   *already* being narrowed by a sibling family's proto, unnoticed.

**Convergence: 50%.**

---

## 5. PLANE-WIDE GAPS (charter items no family homes)

- **Packrat semantics — a named charter item — is undesigned across all four families.** FAMILY-SPECS
  mentions "packrat" exactly **once** (a harness disclosure flag, line 49); priorart carries 21
  mentions the synthesis dropped. The LOCK-8 task names "packrat semantics" explicitly (PORTFOLIO:189).
  No family owns packrat placement, memo-key shape stability under 8A, or the type of a memoized parser
  under 8B. This is a charter-coverage hole, not a deferral.
- **The two protos measured the two *easiest* uncertainties.** Brief 1 (does inlining fire) and Brief 2
  (does the thin type slice compile) both cleared. The *hard* uncertainties — does the build-time
  specializer generalize within a code-size budget (8A/4B), does the full typed algebra hold budget
  (8B), does the real latex jump survive (8D), does the mirror not double-home LOCK-6 (8C) — are all
  unmeasured. Pass-2's briefs must target these, not re-run the green ones.

---

## 6. CROSS-LOCK LEDGER (critic-2 weight)

| item | LOCK-8 claim | collides / double-homes with | disposition |
|---|---|---|---|
| Build-time TS specializer / fuser | 8A center B "the V8 face of LOCK-4B"; 8C-ambitious's only door | **LOCK-4B** owns the TS fuser (`leaf.ts:154-273`→any arity); LOCK-4 CRIT-2: TS number **unrun** | Not LOCK-8's to converge; 8A/8C must be marked *dependent on unconverged LOCK-4B*, not ADVANCE-clean |
| Isomorphism linter (name-map + CI tree correspondence) | 8C "the isomorphism LINTER" | **LOCK-6A** MIRROR-TREE (claims parse-that core as its case) | Re-scope 8C to consumer; linter homes in 6A |
| One spec → Rust dumper + TS decoder | 8C Stratum A | **LOCK-6B** SPEC-GENERATED-SKELETON (recasts GN-4.W4 tape port) | Double-home; 6B owns the generator |
| Behavioral differential oracle | 8C Stratum C | **LOCK-6D** ORACLE-ISOMORPHISM | Double-home; 6D owns the null-hypothesis test |
| Tape crossing directory | 8C `crates/bbnf-bench/…/ts-tape/` | **GN-4.W4 / LOCK-6B** `ts/tape/` | Home ambiguity; adjudicate the physical path |
| StateView field set | 8A `{src,value,offset,isError,furthest}` | **8D** field triple `{furthest,expected,getLineAndColumn}` | Missed incompatibility-map entry (8A ⟂ 8D) |
| Result-union soundness cast | 8A `as unknown as ParserState<T>` | **8B** "no unsound casts" | 8A ⟂ 8B is NOT the declared non-conflict |

**Net cross-lock read:** LOCK-8 as written **borrows two terminal mechanisms from LOCK-4B (unconverged)
and re-homes three from LOCK-6 (the weave lock it is meant to consume)**. The families that measured
(8A structural, 8B thin-slice) are honest about what they ran but overstate what that buys; the
families that did not prototype (8C, 8D) carry the double-homes and the blocked-harness. No family is
BLOCKED-candidate — the synthesis's BLOCKED adjudication is correct — but "ADVANCE" across the board
understates that the lock's terminal deliverables are, in two of four cases, owned elsewhere and
unconverged. Pass-2 must (a) re-scope 8C as LOCK-6 consumer, (b) mark 8A/8C's specializer dependency on
LOCK-4B explicitly, (c) fold PROTO-1's SFI-heterogeneity finding into center B + gate 4, (d) reconcile
StateView's field set with 8D before the snapshot lands, (e) home packrat, and (f) run the *hard*
briefs (specializer code-size, full type algebra, real latex jump).
