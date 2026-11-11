# LOCK-7 — GRAMMAR TOTALITY & BOOTSTRAP — pass-2 CRIT-2 (adversarial, CROSS-LOCK)

Critic 2 of 2, **re-issued against the 22:30 FAMILY-SPECS re-cut** (supersedes the earlier CRIT-2
that critiqued the 19:33 re-cut). Weight: where this lock **contradicts or double-homes work against
the extant GN/PT codex and the other locks**, where its cross-repo dependencies are phantom or
circular, and where the freshest disk evidence has outrun the synthesis. CROSS-LOCK (XL-A…XL-H) is
consumed as binding fact; my challenges to it route to the gestalts with evidence attached, never
re-litigated inside the lock.

I concur with CRIT-1's within-lock findings (7C's flagship reads constant 1.0; 7B's 50/20/21 split
is an un-run static read summing to 91%; 7A's survival test is unmet). I do not re-derive them. My
job is the seams. Where CRIT-1 lands a number I add the cross-lock dock and move it.

---

## §0 — THE FRESHEST FACT OUTRAN THE SYNTHESIS (a BREACH-1 the re-cut re-committed)

**The FAMILY-SPECS re-cut is dated 22:30. `PROTO-2-UF3.md` is dated 22:54 — twenty-four minutes
LATER — and it RAN and REFUTED the re-cut's single most load-bearing 7A premise.** The synthesis
prides itself on curing BREACH-1 ("synthesis moves current, not stale," §0 preamble; it folded UF-1
and UF-2). It then committed the identical breach one probe downstream: UF-3, the experiment the
re-cut proposed as 7A's decisive next act (§7), **executed and failed**, and the re-cut narrates it
throughout as *un-attempted and runnable today*.

Every one of these FAMILY-SPECS statements is **empirically false against HEAD** (parse-that
`ef10d5b`), per the RAN UF-3:

- §1 gate (c), line 166: *"runnable TODAY on the regex oracle artifact (the regex parser
  self-emits — UF-3)."* — **FALSE.** `bootstrap-regex.sh` yields the empty-file hash and
  **truncates `generated.rs` to 0 bytes**; the regen edge was severed in parse-that commit
  `6142387f1` (B2) when the `bbnf_derive` proc-macro was deleted. The "self-emit" is now a re-export
  stub (`pub use parse_that::regex::generated::{RegexParser, parse_generated};`).
- §1, line 172: *"The one fixpoint signal runnable WITHOUT LOCK-5's engine is UF-3."* — the signal
  **is not runnable at all**; `regen∘regen` presupposes a regen, and G0 is unproducible.
- §4, line 329: *"The `fixpoint` column is fillable via UF-3 without LOCK-5's engine."* — **FALSE;
  structurally unfillable on the current tree** (no live path from `regex.bbnf` to `generated.rs`).
- §7, lines 427/435: *"UF-3 has never been attempted"* / proposes it as a future brief. — **It was
  attempted (RAN_MEASURED) and returned FAIL (gate precondition absent).**

**Consequence for grading, cross-lock.** This collapses 7A's non-redundant existence. CRIT-1
(writing at 20:20, before UF-3) already reduced 7A to "owns nothing post-graft except UF-3, which
has not run," docking to 38% as *a RETIRE-candidate held open by one pending experiment.* **That
experiment has now run and returned a NEGATIVE.** The thing holding 7A open is gone. 7A is not a 40%
bank and not a 38% held-open; it is a RETIRE with a RAN refutation attached (§B → **25%**).

**And this is a constitution-level cross-lock fact, not just a 7A fact.** Constitution clause (i) —
*"the regex grammar IS bootstrapped and bbnf-defined"* — is the premise LOCK-7's whole task exists to
design around. UF-3 shows clause (i)'s **one existing instantiation is inert**: the parse-that regex
bootstrap is *"a frozen artifact of a decommissioned pipeline"* (UF-3), no live or dormant regen edge
on either repo. LOCK-7 must report clause (i)'s regex bootstrap as **DEAD at HEAD**, not merely
"un-generalized." The re-cut instead assumes it self-emits. This is the single most important thing
this lock owes the gestalt, and the synthesis does not say it.

---

## §1 — THE CROSS-REPO EDGE IS A CYCLE, NOT AN EDGE (INC-5 ⊗ LOCK-5)

The re-cut's central architectural move externalizes the bbnf leg's block as a clean born-RED **DAG
edge**: *"INC-5 = leaf-floor(`match_at`, LOCK-5) + front-end(in-lock) + 1 misfire(in-lock)."* CRIT-1
blessed this as "lawfully externalized… the correct handling." **On the cross-lock evidence it is
neither clean nor an edge.** I read LOCK-5's live registry:

- **LOCK-5 REGISTRY line 97–98:** the `regex.bbnf` self-host is *"un-attempted three passes, NOT
  'converged pending LOCK-7'… gated LOCK-7/7A."*
- **line 329:** *"the ONE formulation no pass has touched"* — gated **LOCK-7/7A**.
- **line 378:** *"5A ⊗ LOCK-7 bootstrap. Two divergent `regex.bbnf` (one discards its AST).
  Re-scoped 'promote + de-dup + break-the-discard.'"*

The picture is symmetric and circular: **LOCK-7 homes `match_at` onto LOCK-5 as a deliverable it will
consume; LOCK-5 gates its regex-bootstrap formulation back on LOCK-7/7A and marks it un-attempted.**
Each lock points at the other for the primitive neither has built. INC-5 is a **mutual deferral** —
a **cycle in the very DAG GESTALT-Ω must produce.** An elegant-reduction whose "and-then-the-hard-part"
is externalized to another lock is *lawful* only if that other lock **owns and schedules** the hard
part. It does not: LOCK-5's re-scoped 5A work is *promote/de-dup/break-the-discard on `regex.bbnf`*,
which is **not** "author and expose a callable `fn match_at(pattern, input, start) -> Option<end>`
leaf primitive for skinny's `general_descent`." That primitive appears on **no lock's committed
scope.** It is a **phantom dependency** — consumer-less substrate inverted: LOCK-7 declares a consumer
relation to a LOCK-5 output LOCK-5 does not list.

**What the gestalt must carry, corrected:** not "INC-5 is a born-RED DAG edge to LOCK-5," but
**"INC-5 is a LOCK-5⊗LOCK-7 cycle over an unowned primitive (callable `match_at` leaf, cross-repo);
GESTALT-Ω must assign ownership before either the bbnf-leg or the regex-bootstrap family can leave
RED."** A stronger, more honest cap than the re-cut's one-directional edge — and the cross-lock
finding the within-lock critic could not see.

---

## §2 — DOUBLE-HOMING & SHARED-MECHANISM SWEEP (cross-lock charge)

Credit first: the re-cut catches its own biggest double-home — **7A's fixpoint SUBJECT ≡ 7D's
`ORACLE_REL_V1`** ("same object… do not double-home," §5) — the right call, and why 7A has nothing
left. **7C's counter → 7D ledger `totality_pct` (XP-1)** is a clean producer/consumer. **7B Plane-B
homed in LOCK-3 (XL-B), conditional on 3C**, is correctly policed (Y.13 named if 3C declined). These
are handled. The unhandled ones:

- **7B Plane-B ⊗ LOCK-2/LOCK-3 — the `derive-language-macro` triple-touch.** Plane-B proposes
  *"`ir.bbnf` generates the hand enum (the in-tree `egraph-derive` / memory `derive-language-macro`
  precedent)."* That macro is **already claimed** by LOCK-2 (its `Language` impl "derived from
  existing enums," memory `derive-language-macro`) and by LOCK-3's 3C GRAMMAR-HOMOIOUSIA. Three
  families now propose to drive codegen off the same derive mechanism against the same `ExprKind`
  arena. The re-cut asserts "conditional on LOCK-3 picking 3C" but never names the LOCK-2 overlap.
  **Confirm single ownership at GESTALT-B** (LOCK-3 3C is the natural home; LOCK-7 and LOCK-2 are
  consumers) — else omega inherits a triple-homed generator.

- **7D `OracleRel-bbnf-v1` ⊗ LOCK-5 `simplify_hir`.** 7D wants its bbnf normal form *"homeable as an
  e-graph over `ExprKind` (LOCK-2 engine), mirroring `simplify_hir`."* `simplify_hir` is XL-C's
  **string/format bridge** between the grammar and engine regex tiers — a LOCK-5 asset. Reusing it as
  a general `ExprKind` normalizer is a *different application of a LOCK-5-owned mechanism*; lawful,
  but stated as if free. **Flag for XL-C confirmation** that the bridge's reuse is in scope, not an
  implicit fork.

- **Fixpoint machinery ⊗ the bbnf-lang xtask (the one live regen).** UF-3's postscript notes the
  **only** live fixpoint edge is `cargo xtask regen --check` over the 9-grammar manifest — which
  **omits regex and omits bbnf-self-host.** So 7A's fixpoint LAW, if it survives, applies to the
  bbnf-lang emitter regenerating *json/css/sheets/etc.* — **not a self-host of bbnf-in-bbnf and not
  the regex bootstrap.** That is a regression-hygiene gate for generated grammars that almost
  certainly **already exists** in xtask and is not a LOCK-7 deliverable. 7A's "distinctive asset"
  reduces to a check-target that ships in the tree today — the cross-lock reason 7A owns nothing,
  independent of UF-3.

---

## §3 — CONTRADICTIONS AGAINST THE EXTANT GN/PT CODEX (Constitution ix)

- **XL-C consistency — mostly clean, one now-broken clause.** LOCK-7's INC-5 correctly routes
  `match_at` to parse-that (one-regex-home) and consumes the two-domain-type split as fact — good.
  But XL-C asserts *"the canonical engine home remains parse-that (one-regex-home)"* on the premise of
  a **living** engine. UF-3 shows the parse-that regex **regen** edge is dead (frozen `generated.rs`,
  decommissioned pipeline). The runtime producer `parse_generated -> Hir` still runs (that is what
  7D's oracle measured, parity 0.7759) — so XL-C's *runtime* claim stands, its *bootstrap* claim does
  not. **Route to GESTALT-P with UF-3 attached:** the regex home is a live runtime engine over a
  **frozen, non-regenerable** parser. Constitution (i)/(ii) — "the regex grammar IS bootstrapped" and
  "true isomorphism… the regex HIR" — cannot both be asserted of a parser that no longer regenerates
  from its grammar.

- **XL-F(1) / GN-3 — consistent (credit).** 7B Plane-B's "node-value `->` presuppose LOCK-1
  sum/product, GN-3 Walls 1–2" and 7C's "floor ∈ {0,1,2}, LOCK-1-pending" correctly treat GN-3's
  walls as a LOCK-1-owned floor, matching XL-F(1) (`TypeAnalysis::from_ir` is the real consumer). No
  contradiction.

- **XL-B / tower ownership — consistent.** 7B Plane-B and the IR-plane framing defer the tower shape
  to LOCK-3 and place e-graph work as consumers — matching XL-B (LOCK-3 owns the tower; LOCK-2 engines
  operate within it). No double-claim on the driver seat.

- **The `88s→0.5s` datum — correctly killed (both critics, XL-H).** Caught as a REFUTED legacy LTO
  transplant; quoting it against skinny named a Constitution-ix breach. No forbidden-lever
  (refutation-law iv) claim survives anywhere in the lock. Clean.

---

## §4 — FAILURE-MODE CHECKLIST, CROSS-LOCK LENS

- **Elegant-reduction trap — PRESENT and UPGRADED to a CYCLE (§1).** The bbnf-leg's
  "and-then-LOCK-5-ships-`match_at`" is not a lawful externalization: the target primitive is unowned
  and the two locks defer to each other. The most consequential finding of the pass for the DAG.
- **SPEC_ONLY anchoring a claim — PRESENT, REFUTED (§0).** 7A's survival rested on the SPEC_ONLY
  assertion "the regex parser self-emits today." UF-3 refuted it. Worse than misuse: a load-bearing
  SPEC_ONLY premise a RAN probe overturned, un-folded by the synthesis.
- **Gate-that-cannot-fail / vacuous convergence — PRESENT and PROPAGATING to the consumer.** CRIT-1
  nailed 7C's `totality%` reading constant 1.0000 on the real fleet (no `ExprKind::Host` exists).
  Cross-lock addendum: the counter's *only declared consumer* is 7D's ledger `totality_pct` (XP-1). A
  constant-1.0 producer feeds a constant column — 7D's ledger gains **zero discriminating signal**
  from the flagship 7C metric on every real row. The vacuity is not contained to 7C; it makes one of
  7D's six ledger columns inert too, until a real Tier-2 host lands.
- **Consumer-less substrate (viii/Y.13) — one NEW instance (§1) + one policed.** New: the phantom
  `match_at` consumer relation. Policed: 7B Plane-B (named consumer-less unless 3C).
- **Double-homing — three unnamed shared mechanisms (§2).** derive-language-macro (LOCK-2/3/7);
  `simplify_hir` (LOCK-5); fixpoint-check vs the live xtask.
- **Unverified gestalt — the fixpoint column is now UNFILLABLE, not empty.** UF-3 upgrades 7D's "one
  column short" row from *empty/pending* to *structurally unfillable on the current tree.* The
  ledger's flagship row can never be completed without a regen edge no lock owns.
- **Zero-runtime-cost (Constitution v) — CRIT-1's open hole stands** (7C CONTEXT-tier zero-residual
  invariant un-round-tripped). No cross-lock addition; I concur.

---

## §5 — CONVERGENCE (CRIT-2 numbers vs FAMILY-SPECS vs CRIT-1)

| family | FAMILY-SPECS | CRIT-1 | **CRIT-2** | the cross-lock dock |
|---|---|---|---|---|
| 7A STAGED-BOOTSTRAP | BANKED 40% | 38% | **25%** | sole survival warrant (UF-3 fixpoint-on-regex) **RAN and FAILED** (§0); the only live fixpoint edge is the pre-existing xtask 9-grammar check, not a LOCK-7 deliverable (§2); RETIRE with a RAN refutation, not a bank |
| 7B TOTAL-SELF-HOST | BANKED 32% | 30% | **28%** | Plane-A block is a CYCLE not an edge (§1); Plane-B triple-homes the derive macro (§2, LOCK-2/3/7) and is 3C-conditional (unverifiable in-lock); concur with CRIT-1's un-run static-read dock |
| 7C HOST-FN-LATTICE | ADVANCED 57% | 50% | **48%** | concur with CRIT-1's constant-1.0 finding; add that the vacuity **propagates** — 7C's only consumer (7D ledger `totality_pct`) receives a constant column (§4). Still the survivor; the counter genuinely runs build-side |
| 7D META-CIRC-ORACLE | ADVANCED 51% | 50% | **47%** | strongest MEASURED artifact stands (parity 0.7759 real); but the fixpoint column is now **structurally unfillable** (UF-3, §4), the regex producer is a **frozen decommissioned artifact** (§3), and XL-C's bootstrap clause breaks under it |

**Overall (cross-lock verdict).** LOCK-7 remains the best factually-grounded lock of the campaign —
every disk citation re-verifies true, and I add one it did not have: **the regex self-host regen edge
is severed on both repos** (UF-3, parse-that `6142387f1`). That single fact reshapes the lock:

1. **7A retires.** Its distinctive asset was a fixpoint signal on a self-emitting regex parser; the
   parser does not self-emit; the signal cannot be produced. What survives is 7D's `ORACLE_REL_V1`
   (already 7D's) plus a live-xtask regen check (already the tree's). Nothing standalone.
2. **INC-5 is a cycle, not an edge.** GESTALT-Ω must assign ownership of the callable `match_at` leaf
   primitive — on no lock's scope, with LOCK-5⊗LOCK-7 deferring to each other. Until then **every
   bbnf-leg production/leaf tier stays RED for a reason no lock is scheduled to fix.**
3. **7C + 7D are the two survivors, both correctly capped.** 7C ships a runnable-but-signal-less
   counter (fix by landing a real `ExprKind::Host`); 7D ships the campaign's strongest measured oracle
   whose one negative row can never be completed on the current tree.

**Two next-pass acts, both cheap, both cross-lock-load-bearing:** (i) **fold UF-3** and re-state
clause (i)'s regex bootstrap as DEAD at HEAD — the synthesis is stale on its own proposed probe;
(ii) **surface the LOCK-5⊗LOCK-7 `match_at` cycle to the orchestrator** for a CROSS-LOCK adjudication
assigning ownership, so INC-5 stops being a phantom edge. Landing a real `ExprKind::Host` (CRIT-1's
act i) remains the way to un-vacuum 7C. With 7A now refuted rather than pending, **7C (48%) is the
only member with a credible in-lock path to convergence.**
