# LOCK-1 · pass-2 · CRIT-2 (ADVERSARIAL CRITIC 2 of 2 — the CROSS-LOCK view)

*Weighting per charter: where this lock CONTRADICTS or DOUBLE-HOMES against the extant GN/PT codex
and the sibling locks; the constitution clauses (grammar totality, isomorphism, refutation laws,
zero-runtime, parsimony, consumer-less-substrate viii, extant-tranche-consciousness ix);
prototype-evidence misuse (SPEC_ONLY anchoring a live claim). Read against the current FAMILY-SPECS
(18:46), the two NEWEST probes PROTO-1.md (19:27, Brief α) + PROTO-2.md (19:37, Brief β),
PROTO-2-iso.md, the five research reports, PORTFOLIO + CONSTITUTION, and CROSS-LOCK XL-A…XL-H. Every
load-bearing fact re-run at live HEAD **`06e70053f`** — I took neither the synthesis nor the probes
on their word.*

**Verdict in one line.** The FAMILY-SPECS is a genuine, honest fold — it routes three of the four
carrier-deciding axes OUT of the lock (single-store→GESTALT-B, iso-seat→LOCK-6, residue→GESTALT-B),
correctly banks HM(X) as direction, and marks both probes' worktree claims SPEC_ONLY-adjacent. But it
is now **stale in its own §3**: the two "sharpest OPEN briefs" (α, β) it forwards to pass-3 were both
RUN by probes authored AFTER it (PROTO-1 19:27, PROTO-2 19:37), and **neither probe closes its gap
the way it claims** — Brief α is answered "the floor is INAPPLICABLE because polymorphism is broken"
(the elegant-reduction trap, contingent on a `#[ignore]`d panicking closure path), and Brief β is
answered "X8 landed" on a **worktree** the lock's own base-law reduces to SPEC_ONLY (verified: HEAD
revise.rs is still the raw LWW). The deeper cross-lock finding: **LOCK-1's headline deliverable — WHICH
family is the carrier — is not adjudicable inside LOCK-1.** Every dispositive axis is homed at LOCK-6
or GESTALT-B. The lock can bank 1B as the leading *candidate*; it cannot *seat* the carrier. No family
is convergence-eligible (pass 2 of ≥3; no clean streak).

---

## 0. LIVE-TREE RE-VERIFICATION (HEAD `06e70053f`) — independently run, not taken on word

The FAMILY-SPECS pins to `06e70053f`; current HEAD **is** `06e70053f` (confirmed). Probes pin to
`ce51c36fc` (confirmed ancestor). All facts below re-run this pass:

- **X8 NOT LANDED AT HEAD — PROTO-2.md's "landed" is a worktree claim (SPEC_ONLY by XL-H).**
  `crates/ir/src/passes/types/constraint/revise.rs:27-32` `assign` is verbatim the raw 3-line LWW
  (`if slot.as_ref()==Some(&ty){false} else {*slot=Some(ty);true}`); `grep -cE
  "is_fully_ground|clash|record_domain_clashes"` over revise.rs = **0**. The refinement-aware arm
  exists only as `x8-clash-arm.patch` in the delivery dir. PROTO-2.md's verdict line *"X8 landed and
  RAN_MEASURED"* is precisely the worktree over-claim the FAMILY-SPECS's own X8/G-1B-1 forbids
  ("every 'byte-identical / N passed' is a worktree claim → SPEC_ONLY … Land it or the family has
  shipped nothing"). **After two passes and a third probe, the one carrier-independent deliverable is
  STILL not at HEAD.**
- **TypeDesc::Row NOT LANDED — the 1D residue X4 the synthesis retains for LOCK-1 is SPEC_ONLY.**
  `grep -rcE "Row" crates/ir/src/types/type_desc.rs` = **0**; `shape.rs`/`TypeShapeAnalysis`
  **absent**. §1D banks "LOCK-1's residue from 1D is exactly the engine-agnostic vocabulary
  `TypeDesc::Row`/`LayoutKind::Row` (X4) — which any carrier populates." Nothing populates it; the
  classifier that would (`TypeShapeAnalysis`) does not exist at HEAD. The retained residue is a
  promise, not a landed fact.
- **The closure path is BROKEN and `#[ignore]`d — PROTO-1's load-bearing caveat is real.**
  `crates/core/tests/pipeline.rs:704-754` — five `#[ignore = "AV.0.11 Category A: lower::expression
  closure-body lowering gap"]` tests; the path panics `"closure: missing body child"` at
  `lower/expression.rs:155`. `IrNode` has **no** `Call`/`Closure`/parametric variant (only
  `Ref(RuleId)`, verified `dag/node.rs:77`). PROTO-1's "monomorphic by construction" is HEAD-real —
  *because the one polymorphic surface does not function.*
- **Ref bridge unidirectional (PROTO-1 structural claim holds).** `rule_vars: HashMap<RuleId,
  TypeVarId>` (generate.rs:29 — one rule → one var); `RefConstraint::revise` (reference.rs:98-131)
  reads `vars[rule_var].domain.solved` and suspends via `UnresolvedCompoundRef`, never writes the
  rule var. Confirmed.
- **The doubly-dead displacement lever (G-1A-2) holds.** `TypeDomain::join` (`domain.rs:66`) has no
  call site in `passes/types/`; the live path is `assign`. 1A's "the running CSP is overwrite-unsound"
  tilt stays dead code.
- **The two `from_ir` sinks (XL-F) confirmed** — `driver/analysis.rs:22` + `rust/view/named_types.rs:100`.
- **The double-home (X3/X11a) is real** — `csp-solver = "0.1"` in **both** `crates/ir/Cargo.toml:18`
  and `crates/core/Cargo.toml:20`; `csp_strategy/` (LOCK-2's 2C driver) and `types/` (LOCK-1) both
  build `Csp<_>` over that one vendored crate, both inside `crates/ir`.

---

## 1. THE STALE-SYNTHESIS FINDING (cross-cutting, binds §3 of the FAMILY-SPECS)

The FAMILY-SPECS §3 forwards Brief α (let-gen additive graft) and Brief β (X8 clash arm) as *the two
uncertainties whose resolution most advances the next pass* — i.e. **open**. Both were RUN by probes
timestamped AFTER the synthesis (PROTO-1 19:27 = α; PROTO-2 19:37 = β). A synthesis whose headline
open items are already answered by newer on-disk evidence is not converged — and the answers do NOT
land where the briefs aimed:

- **Brief α → "INAPPLICABLE," not "additive."** PROTO-1's verdict is neither graft nor re-seed: false
  fusion is *unreachable* because the IR is monomorphic by construction. This is honest, but it
  **redefines the banked floor rather than instantiating it.** §0/§5 bank HM(X) = "equality trunk +
  bounded residue + **let-generalization**" as the settled *direction* both carriers share; PROTO-1
  concedes "1A and 1B are one architecture holds only in *outcome*, not in *mechanism*." So 1B ships
  **structural monomorphization, not HM(X)-with-let-gen.** The banked floor is not met — it is
  declared not-applicable. **The elegant-reduction trap, textbook form:** the load-bearing step
  "polymorphic reuse stays structural (beta-reduced before types)" reads *"…and the hard part —
  making grammar-closures actually work AND provably keeping them pre-IR-monomorphic — is a
  `#[ignore]`d panic."* PROTO-1's own caveat concedes it: *"If a future tranche wires a live
  parametric rule that survives into the IR … THEN the false-fusion floor becomes reachable."*
  Grammar-closures is a **documented live design line** (project memory: "First-class closures,
  GrammarCall, @host directive, compile-time beta-reduction"; constitution i grammar-totality). So
  the floor is not dead — it is *deferred behind a broken feature the codex intends to fix.* Banking
  "floor delivered zero-diff" is banking against a feature that panics today.

- **Brief α's fixture is a gate that cannot fail.** PROTO-1's fixture (`pair=k v; a=pair pair;
  b=pair pair`) is monomorphic — `k`,`v` are fixed leaf types — so both `pair` uses resolve to the
  *same* `Tuple([BoxedEnum,BoxedEnum])`, which PROTO-1 states outright ("the uses do not diverge …
  nothing polymorphic to monomorphize"). A false-fusion test on an IR with no polymorphism cannot
  fail. Recording it as "1B ships the floor for free" over-reads a vacuous pass.

- **Brief β → "landed" on a worktree the base-law calls SPEC_ONLY** (verified §0). Worse, PROTO-2's
  own text makes X8 **producer-inert on the real corpus** (below) — a *new, sharper* consumer-less
  finding the FAMILY-SPECS has not absorbed.

The synthesis is not wrong to forward α/β; it is stale in presenting them as unresolved when the
resolutions (with these caveats) already sit on disk one directory over. Pass-3 inherits *interpreted*
probe results, not open briefs.

---

## 2. PER-FAMILY — convergence % + enumerated open gaps (cross-lock-weighted)

### §1A — UNIFICATION-FIRST · carrier candidate #1 (LEAN-against) · **53%** (−2 vs floor 55)

Architecture banked; not re-litigated. Two cross-lock/evidence marks *lower* it this pass:

1. **[NEW — 1A's sole distinctive advantage is now MOOT]** 1A's one distinctive rightness (§1A: "the
   family that, alone, *would carry polymorphism*") is an advantage over a surface that **does not
   exist** at HEAD (closures broken/`#[ignore]`d; `IrNode` has no parametric node — §0). PROTO-1
   removes 1A's unique selling point: there is no polymorphism to carry, so let-generalization
   distinguishes 1A from 1B *only in a future where closures survive into the IR un-reduced* — which
   is a LOCK-7/GN-codex decision (grammar-totality staging), not a LOCK-1 carrier fact. 1A's
   distinctiveness is contingent on a cross-lock feature decision not yet made.
2. **[iso categorically last — HEAD-real, stands]** PROTO-2-iso's negative (`grep 'TypeENode|monotype'
   crates/core/src/backend/` = 0; sole `TypeDesc→TS` sink is `type_desc_to_ts`) means a fused
   monotype has **no** TS path — authoring a brand-new `*_to_ts` sink from scratch. If LOCK-6 rules
   iso is layout-dispositive, 1A is pre-eliminated; if LOCK-6 rules iso is *behavioral* (6D oracle),
   the pre-elimination weakens. **1A's carrier fate is a LOCK-6 verdict, not a LOCK-1 one** (X11b).
3. **[Option A never built]** G1 (terminate + parity + ≤ wall-time) SPEC_ONLY; the `BoxedEnum→structural`
   refinement lattice must be re-invented inside union-find for zero measured gain (G-1A-1).
4. **[research-A §4 unproven]** No probe shows `HeterogeneousAltJoin` *retiring* under unification vs
   *coexisting* with a union-find structural arm (G-1A-4).
5. **[row discipline unmeasurable]** Zero row-shaped `->` in the real corpus (Leijen vs Gaster-Jones /
   Wand's trap unexercised); behind GN-3.W3 (G-1A-5/6).

### §1B — ONE-CONSTRAINT-STORE · carrier candidate #2 (LEADING candidate) · **53%** (−1 vs floor 54)

Still the leading candidate, iso-safe on the Sum half (HEAD-real), cheapest diff. PROTO-1 genuinely
*strengthens* the "cheapest diff" (the let-gen floor costs 0 on the current IR). But three
cross-lock/soundness debits offset it:

1. **[FATAL, X8 STILL not at HEAD after two passes + a third probe]** revise.rs is the raw LWW at
   HEAD (§0). PROTO-2.md's "landed" is a worktree patch; SPEC_ONLY by the lock's own XL-H base-law.
   The one carrier-*independent* deliverable — owed regardless of who wins — has shipped nothing to
   the tree. (G-1B-1.)
2. **[NEW, SHARPER viii — X8 is PRODUCER-INERT, not merely consumer-less]** PROTO-2.md's own text:
   *"in a well-formed grammar no single type variable ever receives two disagreeing fully-ground
   types, so the refuse arm never fires on real grammars — it fires only on the synthetic clash
   fixture."* So the four `project_types_*` "byte-identical" passes are a **gate that cannot fail**
   (the arm structurally never fires on those inputs → identity), and the `TypeClash` obligation
   stream is **empty on the entire real corpus**. Combined with the FAMILY-SPECS's own G-1B-3
   (no consumer until 1C's unbuilt LSP bridge), X8 lands a mechanism that on real grammars **neither
   fires nor is read.** "Ships regardless of carrier" ships a no-op on production input. The joint
   wave (X8 producer + 1C consumer) is not merely unnamed — it must *also* carry a real-grammar
   fixture that makes the arm fire, or it is doubly-inert substrate (viii).
3. **[1B ships a DIFFERENT architecture than the banked floor — §5.1 collapse is outcome-only]**
   Per PROTO-1, 1B = structural monomorphization, HM(X)-with-let-gen = 1A. "One architecture" holds
   only in outcome. So 1B's soundness *depends on the codex keeping closures pre-IR-reduced* — a
   cross-lock (LOCK-7/grammar-totality) commitment the FAMILY-SPECS neither states nor obtains. If
   GN-codex closures land as live parametric IR, 1B's monomorphic-by-construction soundness breaks
   and the let-gen graft (or per-call specialization) re-enters — re-opening INC-1 on build-cost.
   **This contingency is a cross-lock obligation, not the closed axis PROTO-1 declares.**
4. **[single-store thesis BLOCKED-candidate — cross-lock, correctly routed]** The literal one-store =
   LOCK-1⊕LOCK-2 merge over the shared `csp-solver` (§0 verified: two `Csp<_>` in `crates/ir`).
   GESTALT-B. Once banked, 1B's residual identity is "cheapest diff," itself contingent on (3).
5. **[residue census / B4 store-shape on paper — spec-cites-itself, stands]** "Residue ≈ 0" reads its
   own coarse output over a corpus with **zero overload code** (G-1B-5/6). Unrun across two passes.

### §1C — BIDIRECTIONAL-LOCAL · BANKED (reactivation-gated) · **40%** (held)

Correctly banked as the winner's diagnostics/locality overlay. No pass-2 evidence moved it; the newest
probes *reconfirm* its walls and *worsen* its consumer story.

1. **[make-or-break has no input]** R-C2 needs a declared/synthesized-*mismatch* `->` fixture; the
   greenfield json's 4 arrows are all scalar leaf projections. Unfalsifiable until GN-3.W3 or a hand
   fixture (G-1C-1).
2. **[GN-3.W3 wall reconfirmed]** inline `(string -> key)` trips the typed-materialization invariant
   (`wrap.rs:242`); caps 1C's addressable surface (G-1C-2).
3. **[consumer worsened by X8's inertness]** 1C's LSP bridge is the shared consumer for both its span
   obligation and X8's `TypeClash`. But per §2.2 the `TypeClash` stream is *empty on real grammars* —
   so even once the bridge lands, 1C consumes an empty stream on production input until a mismatch
   fixture is authored. The joint wave must carry the fixture, not just the bridge (G-1C-3, sharpened).
4. **[sub-span ceiling, a LOCK-3 field]** `Project.ty` is one whole-expr `String` span; a wrong
   ascription cannot underline the offending sub-term (G-1C-4).

### §1D — TYPES-AS-ECLASSES · BANKED (structural-μ UNPROVEN) · **42%** (−1 vs floor 43)

Three legs correctly RETIRED. The one surviving hook (structural `List ≠ Row`) is UNPROVEN and
re-homed to GN-1 (X9) — correct. Lowered −1 because the residue the synthesis *keeps* for LOCK-1 is
itself unlanded:

1. **[the retained residue X4 is SPEC_ONLY at HEAD]** §1D/§2 INC-2 bank "1D's residue = `TypeDesc::Row`,
   which any carrier populates" as the *compatible* leftover. Verified §0: `TypeDesc::Row` = 0 hits,
   `shape.rs` absent. LOCK-1's sole retained artifact from 1D does not exist in the tree — it is a
   promise contingent on the GN-1 loom.
2. **[name-collision, vi]** A net-new `TypeAnalysis` collides with the live `driver/analysis.rs:22`
   struct (XL-F confirmed).
3. **[no TS mirror, iii]** `TypeENode` is not a `TypeDesc` → no `type_desc_to_ts` path (ranks last on
   iso with 1A, PROTO-2-iso negative).
4. **[3-lock blast radius, cross-lock]** the `union`-scheduling amendment fires on LOCK-2 grammar-tier
   + LOCK-5 regex-tier — a GESTALT-B/LOCK-2/LOCK-5 saturation-perf call, never a free LOCK-1 hook.
   Must not silently re-enter via saturation claims.
5. **[structural-μ = original-difficulty, engine-agnostic]** Deriving record-ness from an un-annotated
   body is as hard as the source problem and needs a `->`-declared `key` role *any* engine reads;
   1D's e-graph supplies no free advantage. Reactivate only if GN-1 proves the hook NEEDED **and**
   e-graph-specific (D1 UNBUILT — measured `Unknown` on the real body).

---

## 3. CROSS-LOCK — where LOCK-1 contradicts / double-homes the codex and siblings

**XL-1 [THE STRUCTURAL FINDING] — LOCK-1's headline deliverable is not adjudicable inside LOCK-1.**
The lock's task is "the full type system … inferred, solved, erased" — operationally, *which family
seats the carrier.* The FAMILY-SPECS routes **every dispositive axis out of the lock**: single-store →
GESTALT-B (X3); iso-carrier-seat → LOCK-6 (X11b, constitution iii "measured, not asserted"); residue
defaulting-lattice → GESTALT-B (X11a). What remains intra-lock is (a) build-cost, and (b) let-gen
additivity — and PROTO-1 dissolved (b) into a LOCK-7 closure-staging question. So LOCK-1 can bank 1B as
the leading *candidate* but **cannot seat the carrier**; the seat is a LOCK-6 ∥ GESTALT-B call. The
FAMILY-SPECS should say this plainly and cap its own convergence at "carrier candidate ranked, seat
deferred" — not present "1B LEADING carrier" as a lock-internal resolution. This is the honest ceiling
on the whole lock: ~55% is the roof until the gestalt renders the axes LOCK-1 has (correctly) exported.

**XL-2 [UNADDRESSED cross-lock obligation to LOCK-5] — the Map-barrier posture is never stated.**
XL-C (RATIFIED): 5B's functor is RETIRED, "conditional re-open **only if LOCK-1 abandons Map-barrier
typing**." The FAMILY-SPECS's carrier mechanisms *touch* Map typing (1A: "runtime-span keys → open
`Row`/Map"; the widened product-lattice over `TypeDesc`), yet **nowhere states whether the winning
carrier keeps or abandons the Map barrier.** LOCK-5 is holding a re-open condition on a LOCK-1 decision
LOCK-1 has not made. Named gap: the carrier spec must declare its Map-barrier posture, or LOCK-5's 5B
retirement rests on an unstated assumption (constitution ix — a silent contradiction risk).

**XL-3 [double-home, correctly named but gate mis-homed]** Open X (residue) + 1B's single-store both
route to GESTALT-B over the shared `csp-solver` (X11a — verified). *But* the 1B gate list still carries
G4 ("Open X two-mode seam on the width micro-fixture, decided independently of the carrier") as a
LOCK-1 gate. A gate whose seat is at GESTALT-B cannot be *closed* in LOCK-1 — it can only be *staged*.
The FAMILY-SPECS should demote G4 to "staged for GESTALT-B," else it is a gate-that-cannot-pass-here.

**XL-4 [XL-F absorbed — good]** The GN-3 §1.5 supersession (real sink is `TypeAnalysis::from_ir`, not
the GN-3-framed consumer) is correctly carried as a named supersession (constitution ix). No contradiction.

**XL-5 [zero-runtime invariant, v — exercised only on worktrees]** All three probes show `git diff
crates/core/src/runtime/ = 0`, but on *worktree* changes (X8, Row) that are **not at HEAD**. The
invariant has never been exercised on a LANDED change (the FAMILY-SPECS names this for all four —
correct). Until X8 or Row commits, the zero-runtime claim is worktree-only.

---

## 4. CONVERGENCE SUMMARY (cross-lock, CRIT-2 pass-2)

| family | disposition | CRIT-2 % | Δ vs floor | binding cross-lock reason |
|---|---|---:|---:|---|
| 1A UNIFICATION-FIRST | ADVANCED (carrier cand. #1) | **53** | −2 | sole distinctiveness (polymorphism) MOOT — no polymorphic surface at HEAD (closures broken); iso categorically last but seat is a LOCK-6 call; Option A unbuilt |
| 1B ONE-CONSTRAINT-STORE | ADVANCED (leading candidate) | **53** | −1 | X8 STILL not at HEAD (PROTO-2 "landed" = worktree/SPEC_ONLY); X8 producer-inert on real corpus (new viii); ships structural-mono not HM(X)-let-gen (contingent on LOCK-7 closure staging); single-store→GESTALT-B |
| 1C BIDIRECTIONAL-LOCAL | BANKED (reactivation-gated) | **40** | 0 | make-or-break has no input; GN-3.W3 wall; consumer worsened — X8's `TypeClash` stream empty on real grammars; joint wave must carry a fixture, not just the bridge |
| 1D TYPES-AS-ECLASSES | BANKED (structural-μ UNPROVEN) | **42** | −1 | retained residue `TypeDesc::Row` is SPEC_ONLY at HEAD (0 hits); name-collision (vi); no TS mirror (iii); 3-lock blast radius; re-homed to GN-1 |

**Lock-wide.** The fold is honest and well-routed, but pass-2 does not converge the lock, and the two
newest probes *sharpen* rather than settle it. **(1)** The synthesis is stale in §3: Briefs α/β were run
by later probes, and both "resolutions" are softer than claimed — α is "floor inapplicable because
polymorphism is broken" (elegant-reduction trap, contingent on the LOCK-7/GN closure line), β is "X8
landed" on a worktree the lock's own base-law calls SPEC_ONLY (verified: HEAD is still the raw LWW).
**(2)** X8 is not just consumer-less — it is **producer-inert on the real corpus** (PROTO-2's own
admission), so its byte-identity gate cannot fail and its obligation stream is empty on production
input; the joint wave with 1C must carry a firing fixture. **(3)** The decisive cross-lock finding:
**LOCK-1 cannot seat its own carrier** — single-store, iso-seat, and residue all route to GESTALT-B /
LOCK-6; the lock's roof is "1B ranked leading candidate, seat exported," not "carrier chosen." **(4)**
An unstated cross-lock obligation to LOCK-5 (XL-C Map-barrier posture) must be declared or 5B's
retirement rests on an assumption LOCK-1 never made. No family is convergence-eligible (pass 2 of ≥3;
no clean-pass streak); floors hold modulo the four marks above.
