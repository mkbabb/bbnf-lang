# LOCK-1 — THE SOLVER SPINE · pass-2 · CRIT-1 (Adversarial Critic 1 of 2 — WITHIN-LOCK view)

*Charter: harden/challenge/refine every family against the failure-mode checklist + the
constitution + prototype-evidence discipline; weight the INTERNAL SOUNDNESS of each family spec
and whether the prototypes tested the SHARPEST uncertainty. Convergence % + exact enumerated gaps
per family. **Every path:line and grep re-run this pass at the TRUE current HEAD `3f2350f15`** —
newer than the FAMILY-SPECS/prior-critic pin `06e70053f` AND the probe pin `ce51c36fc` (both
confirmed ancestors: `git merge-base --is-ancestor` → YES). I took neither the synthesis nor the
probes on their word. This report supersedes the earlier CRIT-1 (mtime 20:02), which was written
against the 19:27/19:37 probe round and predates the 22:21–22:23 PROTO RE-RUNS this report judges.*

---

## §CRIT-0 — HEADLINE: NOTHING LANDED, AND THE NEWEST PROBE OVER-CLAIMS AGAINST THE TREE OF RECORD

The single most important fact this pass, verified cold at HEAD `3f2350f15` with an **empty index**:

**Not one probe deliverable exists in the tree of record. All three probes are worktree-only.**

| artifact the probes claim | HEAD `3f2350f15` reality (re-run) |
|---|---|
| X8 refinement-aware `assign` clash arm | `revise.rs:27-33` is the **raw 3-line LWW** (`if slot==Some(&ty){false} else {*slot=Some(ty);true}`); `grep -cE "is_fully_ground\|record_domain_clashes\|clash"` = **0** |
| `TypeObligation::TypeClash` | `grep -rn TypeClash crates/ir/src` = **0 hits** — absent |
| X8 "already fully staged in the index" (PROTO-2, 22:21) | `git diff --cached --stat` = **EMPTY**. Nothing is staged. |
| `TypeDesc::Row` / `shape.rs` / `TypeShapeAnalysis` (X4 residue, PROTO-2-iso Row half) | `type_desc.rs` has only `HeterogeneousAltJoin` (line 63); `shape.rs` **absent**; `TypeShapeAnalysis` **0 hits** |
| probe test files (`proto1_false_fusion.rs`, `carrier_iso_ts.rs`) | **both ABSENT** from `crates/core/tests/` |
| commits landing X8/Row since `ce51c36fc` | `git log ce51c36fc..HEAD` = **state/journal commits only**; no L1 carrier landing |

**The prototype-evidence-misuse finding (sharpest, within-lock).** The re-run PROTO-2.md (22:21)
escalates its own claim relative to the 19:37 round the prior critics read. Where the earlier probe
said "landed **in worktree**" (correctly flagged SPEC_ONLY by both prior critics), the re-run says
**"already fully staged in the index at the pin … PASS — RAN_MEASURED … X8 landed and verified
carrier-independently."** Against the actual tree of record the index is empty and `assign` is the
raw LWW. Under XL-H (base law) and the FAMILY-SPECS's own G-1B-1 ("every 'byte-identical / N passed'
is a worktree claim → SPEC_ONLY … Land it or the family has shipped nothing"), the escalated
"staged/landed/RAN_MEASURED" language is **an over-claim on a SPEC_ONLY artifact** — the exact
Constitution-vii fault the charter names. The numbers may be honest **in the probe's private
bbnf-worktree** (28 tests green there); they did **not** propagate, and "staged in the index" is
refuted by the empty index. *The tree of record has neither the arm, nor the staging, nor the test.*

**Base-law note (XL-H).** The 22:21–22:23 probes pin to `ce51c36fc` and assert "HEAD == ce51c36fc,"
but the tree of record had already advanced to `06e70053f` (the FAMILY-SPECS pin) and now to
`3f2350f15`. Base-pinning to an ancestor worktree is XL-H-legal; **never forward-reconciling it to
the advancing tree of record is how "RAN_MEASURED" and "empty index" coexist.** Pass-3 must treat
every pass-2 probe number as ancestor-worktree-relative and unshipped.

**Consequence.** The lock's single most-tracked deliverable (X8, "the one carrier-independent
deliverable owed for two passes") is **STILL unshipped after two full passes + a third probe
re-run.** The FAMILY-SPECS §0 (X8 "still the raw 3-line LWW … at HEAD," G-1B-1 "FATAL, unmet ×2")
and the re-run PROTO-2 ("landed") **directly contradict on disk**; my HEAD verification adjudicates
for the FAMILY-SPECS and against the probe. A synthesis and its own commissioned probe disagreeing
on whether the lock's headline deliverable shipped — with the probe wrong against the tree — is a
convergence-blocking evidence-integrity fault, not a bookkeeping nit.

---

## §CRIT-1 — PROBE-COVERAGE AUDIT: THE BUDGET WENT TO THE SAFE (1B-FAVORABLE) UNCERTAINTIES

The entire second-round probe budget bought **α (let-gen), β (X8), iso** — all three 1B-favorable,
none landed. The decisive experiment for **each other family was not run**, and even 1B's own
*distinguishing* thesis (the product-store, B3/B4) was skipped for the easy equality-carrier axes.

| probe | family served | did it test the family's SHARPEST uncertainty? | shipped? |
|---|---|---|---|
| PROTO-1 (false-fusion, Brief α) | 1B (moots 1A distinctive) | **No** — fixture is monomorphic (§CRIT-7 gate-that-cannot-fail); the sharp Q (a *polymorphic* GrammarCall used at two types) needs the broken closure path | test file absent |
| PROTO-2 (X8, Brief β) | carrier-independent | tests the *easy* equality carrier, **not** 1B's distinguishing product-store (B3/B4) | **no** (empty index) |
| PROTO-2-iso | 1B (excludes 1A/1D) | tests TS-*path-existence* (measured, solid negative); the POSITIVE "parallel/iso" is **asserted** (§CRIT-7) | test file absent |
| **NOT RUN** | 1A port-cost race (G-1A-1) | the one probe that would formally close INC-1 | SPEC_ONLY |
| **NOT RUN** | 1C R-C2 make-or-break | the one probe that reactivates 1C | SPEC_ONLY |
| **NOT RUN** | 1D D1 Row-derivation | the true make-or-break | SPEC_ONLY (re-homed GN-1) |

**Within-lock reading:** three of four families are **stale-in-place**, and the family that
"moved" moved on worktree artifacts that did not reach HEAD. The pass did not test the sharpest
uncertainty for 1A, 1C, 1D, **or** for 1B's namesake product-store — it tested the three axes on
which 1B was already ahead. Coverage skew + tested-the-safe-uncertainty; the convergence table
below prices it.

---

## §CRIT-2 — 1A · UNIFICATION-FIRST · **~54%** (architecture endorsed; carrier case near-eliminated but NOT dead)

**Endorsed:** HM(X) restated (prior-art §2), proven at leaf scale (PROVE-08), correctly
BANKED-as-architecture. No theory dispute.

**The pass demotes 1A's carrier case — but the demotion itself rests on an unconverged step.**
- PROTO-1 **moots 1A's one distinctive rightness** (let-gen / false-fusion soundness): there is no
  false fusion to prevent, so gate G3 is moot and the fork collapses onto build-cost + iso, both
  LEAN-1B. **Caveat CRITIC-1 must add:** this mooting is only true *because closures are broken*
  (§CRIT-7 elegant-reduction). 1A's distinctiveness is **dormant behind a LOCK-7 grammar-closures
  decision**, not dead. Do not over-credit "1A near-eliminated" as a settled LOCK-1 fact.
- G-1A-2 (dead displacement lever) **reconfirmed cold**: `TypeDomain::join` defined at
  `domain.rs:66`, **zero call sites** in `passes/types/` (`grep '.join(' ` = empty). The live
  carrier is `assign`. 1A's "running CSP is overwrite-unsound" tilt targets dead code — unearned.
- iso categorically last (PROTO-2-iso negative, measured): a fused monotype has **no
  `type_desc_to_ts` path** (`ts/projection.rs:11` dispatches on `TypeDesc` variants only).

**Enumerated open gaps (1A):**
- **G-1A-1 [port-cost race STILL SPEC_ONLY].** "Activate the scaffold" inverts under XL-A to "port
  PROVE-08 into root + displace the running CSP" (delete ~101 LOC, rewire the `TypeDesc::` surface,
  re-invent the `BoxedEnum→structural` refinement lattice inside union-find). The ONE probe that
  would move 1A was not run; the parity/wall-time race is a claim.
- **G-1A-2 [displacement lever dead].** Confirmed — sinks the carrier rationale, does not close it.
- **G-1A-3 [distinctive advantage mooted-but-contingent, NEW nuance].** G3 mooted by PROTO-1 — but
  contingent on the LOCK-7 closure-staging decision (grammar-closures is a live design line). 1A's
  carrier case has no *present* distinctive win, only distinctive costs; its distinctiveness
  *revives* if closures ever land as un-reduced parametric IR.
- **G-1A-4 [`HeterogeneousAltJoin` retirement unproven].** No probe shows the Join variant retiring
  under unification vs coexisting with a union-find structural arm.
- **G-1A-5/6 [width + row unmeasured].** Zero row-shaped `->` in the real corpus (json arrows are
  scalar leaf projections); Leijen-vs-Gaster-Jones unfalsifiable on this instrument; width residue
  unrun.

**Verdict.** Architecture endorsed; carrier LEAN-against, near-eliminated on build-cost + iso **but
not dead** (its mooting rides a broken feature). ~54%: clear disposition, port-race unbuilt,
distinctiveness dormant-not-refuted.

---

## §CRIT-3 — 1B · ONE-CONSTRAINT-STORE · **~53%** (leading CANDIDATE; the "big mover" did not move the tree)

**1B is the leading carrier CANDIDATE — on iso and cheapest-diff, both real.** But the prior CRIT-1's
~70% over-credited a worktree artifact that, verified at the newer HEAD, **did not land, is not even
staged, and whose test file is absent.** CRITIC-1's within-lock read lands with the harsher number.

- **G-1B-1 (X8) — UNMET, sharpened.** Not "proven-landable, awaiting merge" — at HEAD `3f2350f15`
  the index is empty, `assign` is raw LWW, `TypeClash` absent. Two passes + a re-run and the one
  carrier-independent deliverable has shipped **nothing** to the tree of record (§CRIT-0). The
  §0/G-1B-1 "FATAL, unmet ×2" stands; the probe's "landed" is refuted.
- **G-1B-2 (§0 void / let-gen) — resolved as N-A, honestly.** PROTO-1: false fusion is structurally
  unreachable (IrNode has only `Ref(RuleId)` — verified `types/node.rs:61`; no Call/Apply/Lambda).
  1B ships the (now-struck) floor vacuously. Genuine gap-closure — **but see G-1B-8**.
- **iso (G-XL-2) — 1B is the only carrier with a TS path (measured negative); the POSITIVE iso is
  asserted (G-1B-9).**

**Enumerated open gaps (1B):**
- **G-1B-3 [consumer-less substrate, viii — OPEN + WORSENED].** `TypeClash` is parity-inert
  (codegen reads `ir.types` + `struct_registry`, never `type_obligations`); its only consumer is
  1C's **unbuilt** LSP bridge. Co-named ≠ built.
- **G-1B-4 [gate-that-cannot-fail + producer-inert, NEW-sharpened].** PROTO-2's own text: the refuse
  arm "never fires on real grammars — it fires only on the synthetic clash fixture." So the four
  `project_types_*` byte-identity passes **cannot fail** (the arm structurally never fires → identity),
  AND the `TypeClash` stream is **empty on the entire real corpus**. X8 lands a mechanism that on
  production input **neither fires nor is read** — doubly-inert substrate. The joint wave must carry
  a *real-grammar firing fixture*, not just the bridge.
- **G-1B-5 [X8 is a PARTIAL hardening].** `is_fully_ground` deliberately treats `Enum`/`Named` as
  non-ground; the arm therefore **cannot catch an `Enum`/`Named`-level disagreement** (two
  disagreeing `Named` map-results still LWW-overwrite silently). Whether the ground-only arm is the
  complete X8 or a first slice is unresolved.
- **G-1B-6 [residue census + B4 STILL UNRUN — the DISTINGUISHING thesis untested].** Neither probe
  touched B3 (product-lattice residue census on a modeling instrument with overload/legality/decode
  present) or B4 (per-variant `Sum` store shape vs unsound flat cell — research-B's sharpest
  store-shape constraint). "Residue ≈ 0" **still reads its own coarse output over a zero-overload
  corpus** — spec-cites-itself, unchanged across two passes. What is proven is the
  *structural-equality* store 1B already was; 1B's namesake ("one store, five axes") is unexercised.
- **G-1B-7 [single-store thesis = GESTALT-B].** Literal one-heterogeneous-store = LOCK-1⊕LOCK-2
  merge over the shared vendored `csp-solver` (verified: `csp-solver="0.1"` in both `crates/ir` and
  `crates/core`; `csp_strategy/` + `types/` both build `Csp<_>`). BLOCKED-candidate; caps 1B's
  within-LOCK-1 convergence below 100% — its namesake cannot be seated here.
- **G-1B-8 [soundness contingent on LOCK-7, NEW].** 1B's monomorphic-by-construction soundness holds
  only while closures stay pre-IR-reduced. Grammar-closures is a live design line; if it lands as
  un-reduced parametric IR, the let-gen graft re-enters and INC-1 re-opens on build-cost. This is a
  cross-lock obligation the FAMILY-SPECS neither states nor obtains.
- **G-1B-9 [iso is TS-path-existence, not measured isomorphism, NEW].** PROTO-2-iso proves the
  *discriminating negative* (1A monotype / 1D `TypeENode` have no `type_desc_to_ts` path) — solid,
  measured. But "1B crosses to a **parallel** TS type" is asserted: `Record<string,number>` (a map)
  vs `Vec<(Span,f64)>` (a vec of pairs) are structurally different; the test asserts author-chosen
  strings, not a round-trip/differential oracle. Constitution iii ("measured, not asserted") is met
  for path-existence, **not** for iso-completeness — that remains a LOCK-6 verdict (X11b).

**Verdict.** LEADING carrier CANDIDATE, and the lead (iso path-existence + cheapest diff) is real.
But the "big mover" did not move the tree: X8 unshipped, the distinguishing product-store unrun, the
obligation doubly-inert, soundness LOCK-7-contingent, iso path-not-isomorphism. ~53% — convergence on
"1B is the leading candidate," not on "1B is shipped" or "1B is the one heterogeneous store."

---

## §CRIT-4 — 1C · BIDIRECTIONAL-LOCAL · **~40%** (unmoved; make-or-break not run; consumer worsened)

**Correctly BANKED** as the winning trunk's diagnostics/locality overlay (mechanism clear,
TS-lowerable by construction). No design dispute.

**The decisive experiment was not run.** R-C2 (side-by-side localization on a hand-authored
declared/synthesized *mismatch* fixture) is 1C's self-declared make-or-break and was not the probe
target. 1C's distinct identity — "check moves the span to the `->`" — remains **unfalsified and
unfalsifiable on the real corpus** (json's 4 arrows are scalar leaf projections, none a mismatch).

**Enumerated open gaps (1C):**
- **G-1C-1 [make-or-break has NO INPUT, stands].** No typed-mismatch `->` corpus; blocks on GN-3.W3
  or a hand fixture. The reactivating experiment was not run.
- **G-1C-2 [GN-3.W3 wall].** Inline `(string -> key)` trips the typed-materialization invariant
  (`lower/expression/wrap.rs`); only the rule-level form clears it — caps 1C's addressable surface.
- **G-1C-3 [consumer-less viii — CO-NAMED, WORSENED].** 1C's span-obligation and X8's `TypeClash`
  share the same unbuilt LSP bridge. Per G-1B-4 the `TypeClash` stream is *empty on real grammars*,
  so even once the bridge lands 1C consumes an empty stream on production input. **The joint wave
  must carry a firing fixture, not just the bridge** — and neither exists at HEAD.
- **G-1C-4 [sub-span ceiling, a LOCK-3 field].** `Project.ty` is one whole-expr `String` span; a
  wrong ascription cannot underline the offending sub-term. The thesis's quality ceiling is a
  sub-span the IR does not carry.

**Verdict.** Disposition correct (BANKED, reactivation-gated on R-C2). Convergence static; a pass
that resolves 1B's easy axes while leaving 1C's make-or-break untouched has **deferred** 1C, not
converged it. ~40%.

---

## §CRIT-5 — 1D · TYPES-AS-ECLASSES · **~42%** (engine dead; hook re-homed; retained residue is SPEC_ONLY)

**Correctly BANKED; three legs RETIRED as named negatives** (recursion-keep beats a strawman; "zero
new engine" FALSIFIED — `union` amendment reverted at HEAD, 3-lock blast radius; cost-extraction
IDLE, overload residue = 0). No dispute.

**The surviving hook (structural `Row`-from-body derivation) is UNPROVEN, home is not LOCK-1 —
concur.** Verified: `TypeDesc` at HEAD has **no `Row`, no `Sum`** (only `HeterogeneousAltJoin`), so
the X4 residue the synthesis retains **for LOCK-1** is itself SPEC_ONLY at the tree of record.
Extraction ≠ inference: probes proved 1D can *extract* `List ≠ Row` given a `Row` node; none proved
it *derives* the classification (first-round probe measured `Unknown` on the real `object` body).
The missing primitive needs a `->`-declared `key` role — engine-agnostic, any carrier reads it.

**Enumerated open gaps (1D):**
- **G-1D-1 [D1 derivation UNBUILT, re-homed GN-1].** True make-or-break; not run; correctly a
  LOCK-7⊕LOCK-3 codepath-unification question, not a LOCK-1 recursion race.
- **G-1D-2 [retained residue X4 is SPEC_ONLY at HEAD, sharpened].** LOCK-1's *sole* retained artifact
  from 1D (`TypeDesc::Row`/`LayoutKind::Row`) **does not exist at HEAD** (0 hits; `shape.rs` absent).
  A promise contingent on the GN-1 loom, not a landed fact.
- **G-1D-3 [name-collision, vi].** Net-new `TypeAnalysis` collides with live `driver/analysis.rs:22`.
- **G-1D-4 [no TS path, iii].** `TypeENode` is not a `TypeDesc` → no `type_desc_to_ts` mirror.
- **G-1D-5 [3-lock blast radius].** The `union` amendment fires on LOCK-2 grammar-tier + LOCK-5
  regex-tier — a GESTALT-B/LOCK-2/LOCK-5 saturation-perf call, never a free LOCK-1 hook.

**Verdict.** BANKED; engine thesis dead; LOCK-1's residue = the engine-agnostic vocabulary any
carrier populates — which itself does not yet exist at HEAD. ~42%: clear disposition over a dead
thesis + an unlanded residue.

---

## §CRIT-6 — INCOMPATIBILITY MAP: soundness check (§2)

Internally sound; ratified with two marks:
- **INC-1 (carrier fork).** PROTO-1 removes the let-gen contingency that kept the fork "open" — but
  it removes it via a *broken-closure* argument (§CRIT-7 / G-1B-8), so the fork is CLOSED-for-1B
  **modulo the LOCK-7 closure-staging decision**, not unconditionally. The prior CRIT-1's "record
  INC-1 as CLOSED-for-1B" is too strong; the honest statement is "CLOSED-for-1B contingent on
  closures staying pre-IR-monomorphic." The iso seat still routes to LOCK-6 to *render*.
- **INC-4 / single-store → GESTALT-B.** Correct, unchanged. Likewise Open-X residue double-home
  (X11a) over the shared `csp-solver` — correctly deferred, verified real.

One HOMING nit CRITIC-1 confirms with CRIT-2: **G4 ("Open X two-mode seam … decided independently of
the carrier") is listed as a LOCK-1 gate but seats at GESTALT-B** — it can be *staged* in LOCK-1,
never *closed* here. Demote it, else it is a gate-that-cannot-pass-in-this-lock.

---

## §CRIT-7 — FAILURE-MODE CHECKLIST SWEEP (explicit)

- **Vacuous convergence — FOUND.** Brief α's fixture (`pair = k v`, fixed leaf types) is monomorphic;
  "both uses fuse" is structurally forced, PASS admittedly unreachable. The let-gen floor priced into
  the 43–55% floors is inapplicable — floors measured against a phantom requirement. PROTO-1 honestly
  converts this to a refutation-law, but that is legitimate *only with the invariant gate below*.
- **Spec-cites-itself — FOUND.** 1B "residue ≈ 0" reads its own coarse output over a zero-overload
  corpus (G-1B-6), two passes running.
- **Gate-that-cannot-fail — FOUND ×3.** (a) X8's four byte-identity passes hold *because* the refuse
  arm never fires on real grammars (G-1B-4); (b) PROTO-1's monomorphic fixture cannot exhibit false
  fusion (G-1B-2 evidence); (c) PROTO-2-iso's "parallel/iso-symmetric" is author-asserted
  string-matching, not a round-trip oracle (G-1B-9).
- **Elegant-reduction trap — FOUND.** "Monomorphic by construction" rests on beta-reduction whose
  closure lowering **panics** — verified `lower/expression/mod.rs:168`
  (`node.child(4).expect("closure: missing body child")`) + **5 `#[ignore]`d** closure-body tests
  (`pipeline.rs:708/719/730/742/754`). The load-bearing "and-then-beta-reduction-monomorphizes-it"
  step is broken; the floor is delivered zero-diff only because the polymorphic surface is *both*
  absent from `IrNode` AND non-functional. **Owed: a CI-enforced invariant gate — no
  parametric/`Call`/`Closure` node survives lowering into the type pass — so "monomorphic by
  construction" is a *checked* invariant, not an incidental property of a broken feature
  (G-CRIT-α).** No family carries it.
- **Legacy aliases — FOUND (already flagged).** 1D net-new `TypeAnalysis` name-collides (vi).
- **Masked fallbacks — re-homed.** `has_rule("value")` mask correctly a GN-1 obligation (X9); LOCK-1
  residue = `TypeDesc::Row` vocab, verified still absent at HEAD (a real open IR fix, not done).
- **Unverified gestalt — CLEAN.** single-store→GESTALT-B, iso-seat→LOCK-6, residue→GESTALT-B are
  deferred, not asserted. Good discipline.
- **Consumer-less substrate (viii) — FOUND.** `TypeClash` + span-obligation both feed an unbuilt LSP
  bridge; AND X8 is producer-inert on the real corpus (empty stream). Doubly-inert.
- **Constitution v (zero runtime) — asserted by all four, exercised on a LANDED change by NONE.** The
  probes show `git diff runtime/ = 0` but on WORKTREE changes not at HEAD; the empty index means it
  has never been exercised on a committed change. Named gap for all four.
- **Prototype-evidence misuse — FOUND (sharpest).** PROTO-2 (22:21) "X8 landed/staged/RAN_MEASURED"
  is SPEC_ONLY against the tree of record (empty index, raw LWW, `TypeClash` absent, test file
  absent). All three probes' deliverables are worktree-only; nothing landed. Do not log any pass-2
  probe result as "shipped."

---

## §CRIT-8 — NEXT-PASS DIRECTIVES (what would actually converge the lock)

1. **Correct the ledger to the tree of record.** Record X8 as UNSHIPPED (raw LWW at HEAD, index
   empty), not "landed/proven-landable-staged." Mark all three pass-2 probes ancestor-worktree /
   SPEC_ONLY relative to `3f2350f15`. Reconcile the FAMILY-SPECS↔PROTO-2 contradiction in favor of
   the tree.
2. **Add G-CRIT-α** (CI-enforced monomorphization invariant) and either fix the closure-lowering
   panic or explicitly ledger the polymorphic surface as frozen-monomorphic, so PROTO-1's
   refutation-law is a *checked* invariant, not an incidental property of a broken feature. State
   1B's soundness contingency on the LOCK-7 closure-staging decision (G-1B-8).
3. **Actually land X8 into the tree of record** and **build (not merely co-name) the joint LSP wave**
   with 1C — including a *real-grammar firing fixture* so `TypeClash` is neither producer-inert nor
   consumer-less (G-1B-4/G-1C-3). Decide whether the ground-only arm is complete X8 or a first slice
   (author an `Enum`/`Named`-level disagreement fixture, G-1B-5).
4. **Run the unrun DISTINGUISHING probes** — 1B's B3/B4 (product-lattice residue census + per-variant
   `Sum` store shape) on a *modeling* instrument, and 1C's R-C2 on a hand-authored mismatch fixture.
   These test the sharpest uncertainties the pass skipped; they are what move 1B past ~53% and 1C
   past ~40%.
5. **1A port-cost (G-1A-1)** stays SPEC_ONLY: build Option A to formally close INC-1, or ratify 1A as
   banked-architecture / not-carrier and stop scoring its unbuilt parity race.
6. **Homing hygiene:** demote G4 (Open-X width seam) to "staged for GESTALT-B"; state the carrier's
   **Map-barrier posture** (XL-C holds 5B's functor retirement on a LOCK-1 decision LOCK-1 has not
   made — an unstated cross-lock contradiction risk, ix).

**Lock-wide.** No family is convergence-eligible (pass 2 of ≥3; no clean streak). The honest ceiling:
LOCK-1 can bank **1B as the leading carrier CANDIDATE** but cannot *seat* the carrier — single-store,
iso-seat, and residue all route to GESTALT-B / LOCK-6. The roof is "1B ranked leading, seat exported,
deliverable unshipped," ~53%, until the gestalt renders the exported axes and X8 actually lands.
