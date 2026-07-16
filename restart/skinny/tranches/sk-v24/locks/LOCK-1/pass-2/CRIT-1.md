# LOCK-1 · pass-2 · CRIT-1 (ADVERSARIAL CRITIC 1 of 2 — the WITHIN-LOCK view)

*Weighting: internal soundness of each family spec + whether the prototypes tested the sharpest
uncertainty. Read against the HARDENED re-fold FAMILY-SPECS.md (15:03), PROTO-1 / PROTO-2 /
**PROTO-2-iso (15:23, authored AFTER the re-fold — not yet incorporated)**, the five research reports,
CROSS-LOCK, and the live tree re-verified this pass at **HEAD `c43126422`**. Constitution + the full
failure-mode checklist applied to every family. I ran the tree; I did not take the probes — or the
re-fold's reconciliation of them — on their word.*

**Verdict in one line.** The hardened re-fold has HONESTLY absorbed both pass-1 critiques — it downgrades
the two headlines (carrier seat, Open-R dissolution) to LEAN/UNPROVEN, marks both probes SPEC_ONLY, and
adopts the refinement-aware B1 form. Its remaining defects are three, all NEW this pass: (1) the re-fold
**mis-ranked PROTO-1's own dispositive finding** — the WORKSPACE DIVORCE that makes Open R physically
unable to reach the mask — and **Brief 1 (its pass-3 brief) repeats the exact plan PROTO-1 refuted**;
(2) **PROTO-2-iso is un-incorporated and half-MEASURED** — its Row crossing rides unlanded vocabulary
(SPEC_ONLY), though its structural negative against 1A/1D is HEAD-real and genuinely advances the carrier
seat; (3) the leading carrier **1B does not ship the banked HM(X) architecture** — the running CSP has
**zero let-generalization**, so §5.1's "1A and 1B collapsed onto one architecture" is unsound at the
polymorphism layer and no 1B gate checks false fusion. Floors hold; no family is convergence-eligible.

---

## 0. LIVE-TREE RE-VERIFICATION (HEAD `c43126422`) — the load-bearing facts

All confirmed at current HEAD (the re-fold's own pin), not the probe pin `ce51c36fc`:

- **CONFIRMED — both probes SPEC_ONLY.** `constraint/shape.rs` **absent**; `TypeDesc::Row` grep in
  `type_desc.rs` = **0**; `LayoutKind` = `{Struct, TaggedEnum, UntaggedEnum, NewtypeWrapper}` (no Row);
  `assign` (`revise.rs:27`) is the raw 3-line LWW (`if slot==Some(&ty){false}else{*slot=Some(ty);true}`),
  **not** the refinement-aware `match`; `carrier_iso_ts.rs` / `proto2_row_shape.rs` **absent**. The
  re-fold's §0(a–c) SPEC_ONLY labeling is accurate. X8 remains **UNMET after two passes.**
- **CONFIRMED — the doubly-dead defect.** `TypeDomain::join` (`domain.rs:66`) zero call sites; the pass's
  one genuine advance stands.
- **CONFIRMED — PROTO-1's WORKSPACE DIVORCE (dispositive, and UNDER-weighted by the re-fold).** `skinny/`
  declares its **own** `[workspace]`; the main `Cargo.toml` does **not** list skinny. `skinny/crates/ir`
  is package **`ir`**; `crates/ir` is package **`bbnf-ir`** — two divorced trees. `grep -rn
  "TypeDesc|StructRegistry|LayoutKind|struct_registry" skinny/crates/` = **0 hits**. The mask
  (`skinny/crates/codegen/src/runtime_generator.rs:98 has_rule("value")`) is **present**; `TypeAnalysis::
  from_ir` — Open R's lowering sink — lives in `crates/core` (`backend/driver/analysis.rs:22`), the
  **bbnf-ir side**. Open R's classification physically **cannot reach** the skinny mask.
- **CONFIRMED — the iso negative is HEAD-real.** `HeterogeneousAltJoin(Vec<TypeDesc>)` present
  (`type_desc.rs:63`); `type_desc_to_ts` is the sole `TypeDesc→TS` sink (`ts/projection.rs:11`). A fused
  monotype (1A) and a `TypeENode` (1D) are not `TypeDesc` values → **no `type_desc_to_ts` path**.
- **CONFIRMED — the running CSP carries NO polymorphism.** `grep -rni
  "generali|instantiat|polymorph|scheme" crates/ir/src/passes/types/` = **1 hit, a module doc-comment.**
  `project_types` (`mod.rs:51`) is a monotone `csp.propagate()` sweep with `RefConstraint` suspension +
  `UnresolvedCompoundRef` obligations — **not** let-generalization, **not** instantiate-fresh-per-`Ref`.

---

## FAMILY 1A — UNIFICATION-FIRST · convergence **55%** (held at re-fold floor)

The HM(X) *architecture* is banked consensus; not re-litigated. As a distinct *carrier*, 1A is now marked
**last on iso by a HEAD-real structural fact**, not merely "unshown": a fused monotype is not a `TypeDesc`
and has no `type_desc_to_ts` path (§0). This is a categorical iso failure, not a parity-margin loss — a
sharper mark than the re-fold's "no differential produced." Architecture intact; Option A unbuilt blocks a
larger cut.

**Enumerated open gaps:**

1. **The carrier race is still HALF-RUN — Option A is SPEC_ONLY (unchanged, correctly conceded).** The
   re-fold now says LEAN-1B not CLOSED; good. Option A (union-find re-seed) was "sized from source," never
   built; 1A's G1 (terminate + parity + ≤ wall-time) is unexecuted.
2. **Zero direct in-tree 1A prototype (G1–G7 all asserted).** PROVE-08 supplies only the 5-leaf flat
   unifier; the `Sum`/`Row`/`Vec`/`Option` arms + occurs-check are new code with no proven seed.
3. **The iso floor is now categorically unmet for 1A (O4, sharpened).** PROTO-2-iso's HEAD-real negative
   (no `type_desc_to_ts` for a non-`TypeDesc` carrier) converts "TS mirror unshown" into "TS mirror
   requires authoring a brand-new `*_to_ts` sink from scratch." The re-fold flags X7 but does not fold
   this categorical form into 1A's ranking.
4. **Row discipline unmeasurable — zero row-shaped `->` in the real corpus (verified).** Leijen vs
   Gaster–Jones (Wand's trap) cannot be exercised; behind GN-3.W3. A bet, not a finding.
5. **The `HeterogeneousAltJoin`-retires-under-unification claim (research-A §4) is unproven.** No probe
   shows the Sum obligation *retiring* rather than coexisting with union-find's structural arm.

---

## FAMILY 1B — ONE-CONSTRAINT-STORE · convergence **55%** (−1 from re-fold floor 56)

Still the leading carrier candidate and iso-safe; PROTO-2-iso genuinely advances that on a HEAD-real axis.
But this pass surfaces a **new within-lock soundness hole**: the running CSP is **not** the banked HM(X)
architecture — it lacks let-generalization entirely — so the "1B = cheapest diff / net-0 / keep the
running code" story overstates what 1B delivers. That NEW debit slightly outweighs the (real) iso advance.

**Enumerated open gaps:**

1. **[NEW, SHARPEST] 1B DOES NOT SHIP THE BANKED HM(X) ARCHITECTURE — the running CSP has no
   let-generalization (§0, verified).** §5.1 banks "HM(X) = equality trunk + bounded residue +
   **let-generalization**" as the settled floor **both** 1A and 1B share; §1A's mechanism calls 1A "the
   **ONLY** family that carries polymorphism (each rule `generalize(infer(body))`, each `Ref`
   instantiates fresh)." These two statements contradict: if 1A is uniquely polymorphic, then 1B — the
   monotone `propagate()` sweep with `RefConstraint` suspension — does **not** carry the let-generalization
   the consensus bakes. Consequences the floor hides: (a) "1A and 1B collapsed onto one architecture"
   (§5.1) is false at the polymorphism layer; (b) reaching the banked HM(X) from 1B requires **growing
   let-generalization into the monotone CSP** — unbuilt, unmeasured, and emphatically **not** "net-0
   additive / change nothing that fires"; (c) **no 1B gate checks false fusion** (G3 "generalization
   prevents false fusion" is 1A-ONLY; 1B's G1–G5 are hardening/census/width/convergence). So the leading
   carrier's polymorphic soundness — the exact property let-gen exists to guarantee — is **untested**, and
   whether `RefConstraint` suspension is observationally equivalent to instantiate-fresh is asserted, never
   shown. Pass 3 must add a false-fusion fixture for 1B and cost the let-gen graft, or stop calling the
   1B/1A collapse "one architecture."
2. **[X8 UNMET, two passes] the additive `assign` hardening is STILL NOT LANDED (§0, verified LWW).** The
   single carrier-INDEPENDENT deliverable both critics + the registry called "ships regardless of winner"
   is absent at HEAD. G1's byte-identity was measured on a worktree patch — SPEC_ONLY by XL-H.
3. **[consumer-less-substrate knot, viii — correctly named, not scheduled] B1's clash obligation is
   parity-inert** (codegen reads `ir.types` + `struct_registry`, never `type_obligations`) and its only
   consumer is the 1C LSP bridge — **also unbuilt**. So the "ships regardless" deliverable is, by the
   re-fold's own §1B text, coupled to a BANKED family's unbuilt bridge: landing B1 alone enriches a stream
   nothing reads. "Ships regardless of carrier" and "consumer-less until 1C lands" cannot both hold
   unscheduled — name the joint wave.
4. **[iso advance — REAL, un-banked] PROTO-2-iso's structural negative genuinely strengthens 1B, but its
   label overstates.** The Sum→TS crossing (`HeterogeneousAltJoin` → union name) is HEAD-real; the Row→TS
   crossing rides Brief-1's **uncommitted** vocabulary → SPEC_ONLY by XL-H. Honest split: *Sum-crossing
   HEAD-real; Row-crossing SPEC_ONLY; the "no `*_to_ts` for monotype/`TypeENode`" negative HEAD-real and
   decisive.* The re-fold predates this probe and has not banked even the HEAD-real half — bank it as "1B
   is the only carrier that already crosses (Sum verified), pending Row-landing."
5. **[spec-cites-itself, uncured] the product-domain census (G2) + B4 store-shape (G3) never re-run/built
   across two passes.** "Residue ≈ 0 / decidable-core dominates" still reads the pass-1 census's own coarse
   output over a corpus with zero overload code. The entire "widen the store" half of 1B is on paper.
6. **[distinctiveness near-vacuous — stands] once the single-store thesis is BLOCKED-candidate (X3), the
   architecture 1B ships IS HM(X) = 1A's.** Its residual identity is "cheapest diff" — and gap 1 shows even
   that is contingent on an unbuilt let-gen graft.

---

## FAMILY 1C — BIDIRECTIONAL-LOCAL · convergence **40%** (held)

Correctly BANKED as the winner's diagnostics/locality discipline. No pass-2 evidence moved it; PROTO-2's
GN-3.W3 discovery *reconfirms* its substrate wall rather than lifting it.

**Enumerated open gaps:**

1. **Make-or-break (G1/R-C2) has no input — reconfirmed.** A declared/synthesized-*mismatch* `->` fixture
   must be hand-authored; the greenfield json's 4 arrows are all scalar leaf projections, none a mismatch.
   1C's distinct identity is unfalsifiable until GN-3.W3 or that fixture lands. The re-fold's "not
   structurally impossible" is exact; still unbuilt.
2. **The `->` front-end thread is broken (reconfirmed by PROTO-2 §method).** The natural inline
   `(string -> key)` trips the typed-materialization-invariant (`wrap.rs:242`) — direct evidence GN-3.W3
   is unbuilt. 1C's 40% is a ceiling until that wall falls.
3. **Consumer-less-substrate bridge (viii) named, undelivered.** `type_obligations → lsp_types::
   Diagnostic{range}` = zero hits. Until it lands the span-add is itself the consumer-less substrate 1C
   invokes against skinny's empty `TypeObligation.span`.
4. **Sub-span ceiling (O3, a LOCK-3 field).** `Project.ty` is one whole-expr `String` span; a wrong
   ascription cannot underline the `T`. Correctly flagged for banking regardless of carrier — but **no gate
   schedules the banked items** (the re-fold says "name the wave"; the wave is still unnamed).

---

## FAMILY 1D — TYPES-AS-ECLASSES · convergence **43%** (held)

Recursion-keep + "zero new engine" + cost-extraction correctly RETIRED. The one surviving hook (structural
`List ≠ Row`) is **UNPROVEN, not dissolved** — the re-fold's honest downgrade is right, but it **states the
blocker too small.** PROTO-1 proved the blocker is not just "vocabulary + who-declares-key" — it is a
**whole-workspace unification** (§0 divorce), a LOCK-7 totality decision strictly larger than the re-fold's
framing.

**Enumerated open gaps:**

1. **[the divorce refutes "dissolve" AND under-sizes the re-fold's blocker] Open R physically cannot close
   the mask (§0, verified at HEAD).** PROTO-2's "Open R DISSOLVES 1D — the classifier closes the mask" and
   PROTO-1's "BLOCKED — the mask is in a divorced workspace" are **flatly contradictory verdicts on the
   same question.** The tree adjudicates for PROTO-1: the Row classifier lands in `bbnf-ir` and flows to
   `TypeAnalysis::from_ir` (`crates/core`); the mask is in package `ir` (`skinny/`), which references **no**
   `TypeDesc`. PROTO-2 lowered Row through the bbnf-ir sink and never touched — never *could* touch — the
   skinny mask (its own `git diff skinny/` = empty). The re-fold reconciles the two by declaring both
   SPEC_ONLY (Row unlanded) — correct as far as it goes — but then frames the blocker as
   "vocabulary + GN-3.W3 + who-declares-key," **losing PROTO-1's dispositive §1**: even a perfectly-landed,
   perfectly-annotated Row derivation is unreachable across the workspace boundary. The structural axis is
   blocked behind **codepath unification** (GN-1 the loom, a LOCK-7⊕LOCK-3 totality build), not merely a
   grammar annotation.
2. **[Brief 1 is INCOHERENT with PROTO-1 — the pass-3 brief repeats the refuted plan].** §7 Brief 1 plans:
   "Land `TypeDesc::Row` at HEAD [bbnf-ir] … take the shipping `json.bbnf` … **DELETE `has_rule("value")`
   (`skinny/crates/codegen/…`)** … `cargo xtask regen` and diff generated + runtime output." This asks to
   land Row in one workspace and delete a mask in another that **does not link it** — precisely the plan
   PROTO-1's §1 proved cannot work, and PROTO-1's §5 shows deleting the mask **regresses** (routes JSON to
   `emit_general_descent`, which has no Row/List classifier to substitute). So Brief 1's pass-condition
   ("mask deletes AND regen byte-identical except Row-vs-List") is, per its own probe, **unachievable on the
   retiring substrate** — a gate pre-set to fail, or testing the wrong artifact. The brief must be re-posed
   against GN-1 (post-unification) or explicitly routed to GESTALT-B as "blocked pending codepath
   unification," not run again as written.
3. **[the `->` key requirement is the "hint required" branch, relabeled — stands].** Shipping `json.bbnf`
   lacks the `key` role; on its BARE body the classifier returns `Unknown`. Closing the mask needs a
   grammar edit (`-> … : key`) whose regen-byte-identity and parse-semantics preservation are unshown, and
   whose natural inline form trips GN-3.W3. "No *byte* hint" ≠ "no declaration the real corpus lacks."
4. **`TypeDesc::Row` / `LayoutKind::Row` unlanded (verified 0 hits); Row→TS is SPEC_ONLY (PROTO-2-iso
   rides the uncommitted vocabulary).** Constitution-iii floor unmet, same as 1A. The `TypeENode` also
   name-collides with the live `TypeAnalysis` driver struct (X2/vi) and has no `type_desc_to_ts` mirror.
5. **`union`-scheduling amendment = 3-lock blast radius (X6), unmeasured — correctly deferred to
   GESTALT-B.** Must not silently re-enter via 1D's saturation claims.

---

## CROSS-FAMILY GAPS (bind the whole lock)

- **G-XL-1 — Open S (§5.6) is asserted-as-reading, still unproven.** The re-fold now honestly labels it
  "a reading to be shown faithful, not built — an OPEN gap" (crediting CRIT-1). Good absorption; it remains
  open — either demonstrate the running `mod.rs:72-127` cycle-break IS a monotone-lattice circular AG, or
  stop counting Open S resolved. No erosion.
- **G-XL-2 — the TS isomorphism floor is PARTLY met, not "un-probed."** The re-fold's G-XL-2 ("largest
  un-probed surface") is now stale: PROTO-2-iso probed it. The HEAD-real result — only `TypeDesc` crosses,
  monotype/`TypeENode` categorically cannot — **seats the carrier on iso against 1A and 1D independent of
  build-cost.** This reframes the whole carrier fork (below).
- **G-XL-3 — the carrier fork may be MISFRAMED.** The re-fold treats the open carrier question as "build
  Option A + race parity" (build-cost). But if 1A/1B ship the same architecture (§5.1) — a claim gap 1B.1
  now disputes — then the deciding axis is **iso**, and PROTO-2-iso's structural negative already
  eliminates a fused monotype (1A-as-carrier) categorically: it has NO TS path at all. Under that lens
  building Option A measures a race whose loser is pre-eliminated on iso. Pass 3 should decide: is the
  carrier seat a **build-cost** question (then run Option A) or an **iso** question (then it is nearly
  closed FOR 1B by PROTO-2-iso's HEAD-real negative — modulo landing Row)? The re-fold demands both briefs
  without stating which is dispositive.
- **G-XL-4 — Open X residue (§5.5) UNRUN across two passes.** Width defaulting-vs-search never exercised on
  a width fixture; "defaulting dominates" is prior-art-plausible, unmeasured on this pass's instrument.

---

## CONVERGENCE SUMMARY (within-lock, CRIT-1 pass-2)

| family | disposition | CRIT-1 % | Δ vs re-fold floor | binding within-lock reason |
|---|---|---:|---:|---|
| 1A UNIFICATION-FIRST | ADVANCED (carrier cand. #1) | **55** | 0 | iso now CATEGORICALLY last (no `type_desc_to_ts` for a monotype — HEAD-real); Option A unbuilt; zero direct probe; row unmeasurable |
| 1B ONE-CONSTRAINT-STORE | ADVANCED (carrier cand. #2, leading) | **55** | −1 | NEW: running CSP has no let-gen → NOT the banked HM(X); no false-fusion gate; "net-0 cheapest diff" overstated. Offset by HEAD-real iso advance. X8 still unmet; census/B4 on paper |
| 1C BIDIRECTIONAL-LOCAL | BANKED (reactivation-gated) | **40** | 0 | make-or-break has no input; GN-3.W3 wall reconfirmed broken; LSP bridge (viii) unbuilt; banked wave unnamed |
| 1D TYPES-AS-ECLASSES | BANKED (structural-μ UNPROVEN) | **43** | 0 | divorce refutes "dissolve" AND under-sizes the re-fold blocker (whole-workspace unification, not annotation); Brief 1 incoherent with PROTO-1; Row/TS SPEC_ONLY |

**Lock-wide.** The hardened re-fold is a genuine, honest advance: it converted both prior headlines from
"settled" to LEAN/UNPROVEN and correctly banked the HM(X) architecture as the durable floor. Three defects
remain, all within-lock and all NEW this pass. **(1)** The re-fold **mis-ranks PROTO-1's dispositive
finding** — the workspace divorce that makes Open R physically unable to reach the mask — so its 1D blocker
is stated too small and its own pass-3 **Brief 1 repeats the plan PROTO-1 refuted**; the structural axis is
blocked behind GN-1 codepath unification (LOCK-7⊕LOCK-3), not a grammar annotation. **(2)** PROTO-2-iso is
un-incorporated: its Row crossing is SPEC_ONLY, but its structural negative (only `TypeDesc` crosses to TS;
monotype and `TypeENode` categorically cannot) is HEAD-real and should be banked as the iso-axis carrier
argument — possibly the *dispositive* one, reframing G-XL-3. **(3)** The leading carrier **1B does not ship
the banked HM(X)**: the running CSP carries no let-generalization, so §5.1's "one architecture" collapse is
unsound at the polymorphism layer, the "cheapest net-0 diff" claim omits an unbuilt let-gen graft, and no
1B gate tests false fusion. No family is convergence-eligible (pass 2 of ≥3; no clean-pass streak). Floors
otherwise hold; the two nudges (1A 0, 1B −1) reflect within-lock soundness, not architecture change.
