# LOCK-8 — pass-3 — ADVERSARIAL CRITIC 1 of 2 (WITHIN-LOCK view)

*Weighting: internal soundness of each family spec + whether the prototypes tested the
sharpest uncertainty. Read: PORTFOLIO/CONSTITUTION, CROSS-LOCK, FAMILY-SPECS (22:47), research-
{A,B,C,D,priorart}, PROTO-1/2/2-compaction/8BD + every evidence-*/trace/log in the dir. Every
numeric claim I could reach was cross-checked against its raw artifact.*

---

## PLANE-WIDE FINDINGS (bind all four families)

### PW-1 — THE SYNTHESIS OF RECORD IS STALE VS ITS OWN DIRECTORY (documentation-integrity failure)
FAMILY-SPECS.md is stamped **22:47**. Two decisive prototypes in the SAME directory postdate it:
`PROTO-8BD.md` (**23:11**) and `PROTO-2-compaction.md` (**23:24**). Consequence: the synthesis's
entire "residue" framing is contradicted by run evidence it never folded.
- 8B "residue is three un-run gates (emit-identity, native budget, cross-language mirror)" —
  **all three RAN post-synthesis.** Emit-identity byte-diff = 0 (PROTO-8BD, md5-confirmed,
  logs present); cross-language mirror = DIVERGE (PROTO-2-compaction, evidence-compaction-*.out
  verified: `RUST_TUPLE_ARITY=3`, `TS_VALUE_LENGTH=2`).
- 8D "residue is the one UN-RUN make-or-break crater" (8D-Q1) — **RAN GREEN post-synthesis**
  (PROTO-8BD: force-scope `vue-tsc` TS2322=2=baseline across baseline/pristine/tightened, logs
  confirm; document.ts:81/text.ts:145,154 zero new red; vitest 118✓/9-skip unchanged).
- The "PLANE-WIDE STATE" section ("three remaining UN-RUN gates all ride the same missing
  artifact… Brief 1 constructs the tightened scratch once and fires all three") is **entirely
  obsolete** — Brief 1 IS PROTO-8BD and Brief 2 IS PROTO-2-compaction; both fired. A critic or
  agglomerator reading the synthesis-of-record would materially mis-assess the lock. **This must
  be re-synthesized before GESTALT-P; the frozen doc understates convergence AND is internally
  self-contradictory with sibling artifacts.** (The prototypes are sound; the record is stale.)

### PW-2 — F5 KEYSTONE GATE `proof:no-dead-combinator` IS AN UNVERIFIED-EXTANT-GATE, MISATTRIBUTED (constitution vii)
FAMILY-SPECS cites `proof:no-dead-combinator` **six times** (F5, 8D Tier B, 8D-Q3, integration
seams, incompatibility map) as an **already-shipping mechanical gate** — *"parse-that already
ships `proof:no-dead-combinator` in its `proof:all` chain… already operationalized 'the first-
party suite is the consumer of the algebra'"* — and attributes it to **research-D §1**.
**It does not exist in research-D, nor anywhere in the research corpus** (grep across all
research-*.md = 0 hits; the string lives ONLY in FAMILY-SPECS.md). research-D §1 actually argues
the Y.13 consumer is *"the RAN `test/*.test.ts` (13 files, 124 tests) + `pt-b3-fusion.bench.ts`…
Y.13 satisfied by an executable oracle"* and names a **different** gate for a **different**
subsystem (`proof:packrat-*`), explicitly telling the tranche it must *"declare that the
discharge, or mark packrat SPEC-ONLY-until-armed."* The synthesis has **upgraded a
research-proposed 'declare the RAN suite as consumer' argument into an asserted extant npm
script**, then rested the family it calls "strongest" (8D Tier B, the whole combinator-algebra
Y.13 discharge) on it. This is exactly the spec-cites-itself / unverified-substrate failure
mode: a load-bearing gate presented as measured repo fact, unverifiable this pass (parse-that
read-only), neither RAN-confirmed nor SPEC-ONLY-labelled. **Remedy: either verify the script in
parse-that's `package.json` and cite the run, or restate the discharge in research-D's actual
terms (the RAN corpus is the disclosed first-party consumer). Until then every "Y.13 ADJUDICATED
mechanically (F5)" claim is over-stated.** The *substance* (first-party suite = lawful consumer)
survives; the *cited mechanism* does not.

---

## 8A — SHAPE-STABLE CORE — **80%**

**Sound.** PROTO-1 is honest and decisive: structural leg PASS is verified in
evidence-structural.out (`HaveSameMap(diagOff,diagPopulated)=true`, `…realFailedParseState=true`),
and the temporal refutation is verified in the raw traces — `cannot inline … regexParser: big
function, size (248) >= max-size (100)` appears in BOTH temporal-A and temporal-B; site C shows
0 leaf-inline lines. A zero-measured claim was struck at the shipping tier and reported as a
refutation, not buried. No vacuous gate here.

**Open gaps (enumerated):**
- **8A-Q1 — the sharpest LIVE uncertainty went UN-PROTOTYPED.** The pass measured the arity-2/3
  unroll, which the census proves has **zero live callers** (F1/F2). The synthesis itself locates
  the real headroom elsewhere: latex's only live `any` traffic is arity-≥15 `inlineNode`
  (`document.ts:81`) on the *general* arm, and whether routing it through the FIRST-set
  `dispatch()` LUT beats the sequential general arm is **"Un-measured — this is where the live
  corpus's headroom sits."** PROTO-1 tested the dead path; the live path is the un-run one. The
  prototype tested the sharpest *theoretical* claim, not the sharpest *live-value* one.
- **8A-Q2** — arity-2/3 unroll disposition (keep-as-first-party-bench-micro-opt vs prune) is
  unresolved; both its defenses (temporal F3, consumer F1) fell, so it survives on first-party
  bench alone. Disposition-pending, coupled to 8D Tier B / PW-2.
- **−60.1% B/op lever is SPEC_ONLY at this pin.** evidence-structural.out confirms 248 B/op still
  pays two eager diag arrays; the certified −60.1% is *not applied at ef10d5b*. Honestly
  disclosed, but it is carried in the "durable substrate KEEP" list with a headline number that
  is **not measured on this substrate** — a perf figure anchored off-pin. Fine as disclosed;
  flagged so the gestalt does not bank it as measured-here.

## 8B — TYPE-LEVEL TOTALITY — **84%**

**Sound and stronger than the stale doc says** (PW-1). PROTO-8BD closed emit-identity (byte-diff
0, zero-cost law upheld) and PROTO-2-compaction resolved the cross-language question as a
concrete DIVERGE (Rust keeps fixed-arity 3; TS compacts to 2) — a clean lawful-non-mirror row,
verified in the evidence files. 8B-G-ASUNKNOWN correctly reclassified from "flagship leak" to
lawful variance-bridge.

**Open gaps (enumerated):**
- **8B-Q3 — "no `Parser<any>` anywhere" (PORTFOLIO 8B charter) is silently re-scoped to "no
  `Parser<any>` in the PUBLIC surface."** ParserFunction's phantom `T` and the `as unknown as`
  bridges are LEAVE-AS-IS; the goal is *definitionally* unreachable and has been narrowed. The
  narrowing is defensible but is a **charter-word supersession that is not named as one**
  (constitution ix). State it as a named supersession with the residual `Parser<any>` inventory.
- **Route-1 keeps a NOMINALLY UNSOUND type** (undefined/opt elements mis-report arity), labelled
  LAWFUL-NON-MIRROR. Accepted, but it means 8B does **not** reach full type totality — an
  admitted permanent residual, not a closable gap. Correct call; flagged so "totality" is not
  read as literal.
- **Gate 4 (`tsc --generateTrace` instantiation BUDGET) fired only as its TS2589-free half.**
  PROTO-8BD showed `tsc --noEmit` clean (no depth-cliff) but produced **no instantiation-count
  number** under the tightened signature. The budget-as-a-figure is still un-disclosed.

## 8C — ISOMORPHIC MIRROR — **70%**

**Best-resolved sharp uncertainty in the lock.** PROTO-2 both built the priority-DFA (verified:
floor-tests-with-pfa.txt = 44/44; 1.00× state ratio; Hopcroft-safe) AND retired it latent-only
via a real census — a genuine, well-evidenced negative result that dissolves the pass-2
8C-guard⟂LOCK-5 collision. The regex row is closed.

**Open gaps (enumerated):**
- **8C-Q1 — the family's PRIMARY DELIVERABLE, `LEDGER.tsv`, IS UN-BUILT.** Gate 1 ("generate
  LEDGER.tsv; prove a synthetic new symbol fails CI") never fired. The synthesis reframes this as
  *"a construction owed, not an uncertainty"* — but the construction is the elegant-reduction
  trap in the flesh: "a census walks both trees (ts-morph + `cargo public-api`) **and then emits
  LEDGER.tsv**, and the ledger IS 6A's whitelist" is a load-bearing *and-then* whose classifier,
  CI gate, and 6A handshake are prose, not artifact. The row *classifications* are asserted from a
  hand census, not machine-generated. Convergence cannot count the deliverable as done.
- **8C-Q3 — 8C cannot reach 100% within-lock** (regen gate 6B, parity oracle 6D live in LOCK-6).
  A structural ceiling, honestly flagged; caps within-lock convergence below 100 by construction.
- New compaction DRIFT row (PROTO-2-compaction) must be *added* to the un-built ledger — widening
  the owed construction, not closing it.

## 8D — CONSUMER-CONTRACT-FIRST — **78%**

Make-or-break crater RAN GREEN post-synthesis (PW-1) — the strongest evidentiary result and it
genuinely de-risks the type axis. But three real residuals and one keystone defect remain.

**Open gaps (enumerated):**
- **PW-2 applies here first** — Tier B's Y.13 discharge rests on the unverified/misattributed
  `proof:no-dead-combinator` gate. 8D-Q3 (does the gate cover mapState?) is itself unconfirmed,
  so even the gate's *coverage* is asserted, not audited.
- **8D Gate 4 (`state.diag` prototype) is UN-RUN — the mechanism is SPEC_ONLY.** research-D:183
  states the probe *"is `%HaveSameMap(stateDiagTrue,stateDiagFalse)===true`"* and lists it (line
  282) as a probe **to run**; no prototype ran it. PROTO-1 measured `%HaveSameMap` for diagnostic
  *seeding* (existing fields), NOT for a **newly added `state.diag` boolean constructor field** —
  a different object shape. The synthesis banks `state.diag` as "DECIDED" and puts it on the
  "do-not-relitigate" list, but its load-bearing 0-alloc/0-transition claim + the propagation
  obligation across every `state.from`/`unsafeSetValue` mint site are **unmeasured**.
- **The crater tested a 1.0.0 type-tightening, NOT 8D's own headline hazard.** 8D §mechanism
  names the real danger: 1.1.0 drives combinators over 0.6.0-shaped states across *"the widest
  ParserState-shape gap in the package's history (1.0.0 removed *Span, **1.1.0 adds lazy
  fields**)."* PROTO-8BD's tightened tarball adds **no** lazy fields and **no** `state.diag` — it
  is shape-gap-NEUTRAL. So the dual-instance/lazy-field shape-gap hazard (the thing 8D says is
  make-or-break) is **untested**; "clean additive 1.0.0→1.x candidate" over-reads a candidate
  that omits the 1.x shape change.
- **8D-Q2 — the sole live consumer (latex) is pin-frozen off `<0.8.0`; possible fossil.** The
  "consumer-contract-first" family designs backward from a consumer whose pin *"has not moved"*
  and whose crater positive-gate *"tests a migration no one runs."* If latex never migrates, Tier
  A rests on a fossil and the contract collapses to Tier B first-party corpus — which itself
  rests on PW-2. Consumer-substrate risk, owner-gated (P1/P2, XL-G), non-blocking but real.

---

## SUMMARY OF EXACT OPEN GAPS (for the registry)
1. **PW-1**: re-synthesize the stale 22:47 FAMILY-SPECS to fold PROTO-8BD + PROTO-2-compaction;
   the "residue/UN-RUN/Brief 1/Brief 2/PLANE-WIDE-STATE" text is obsolete.
2. **PW-2 / 8D**: verify or restate `proof:no-dead-combinator` — misattributed to research-D,
   unverifiable this pass, load-bearing for the entire Tier B Y.13 discharge.
3. **8A-Q1**: the live arity-≥15 `inlineNode` headroom (the real live perf question) is
   un-prototyped; PROTO-1 tested the dead unroll instead.
4. **8B-Q3**: "no `Parser<any>` anywhere" re-scoped to public-surface without a named supersession;
   route-1 nominal unsoundness accepted; Gate-4 instantiation-count still numberless.
5. **8C-Q1**: LEDGER.tsv (the primary deliverable) un-built — a load-bearing and-then, not merely
   "owed"; 8C-Q3 caps within-lock convergence below 100 (LOCK-6-bounded).
6. **8D Gate 4**: `state.diag` %HaveSameMap(on,off) + mint-site propagation un-prototyped
   (SPEC_ONLY despite the do-not-relitigate label); crater omitted the 1.1.0 lazy-field shape gap;
   latex-fossil (P2) leaves the contract resting on Tier B + PW-2.
