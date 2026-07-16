# LOCK-1 · pass-2 · CRIT-2 (ADVERSARIAL CRITIC 2 of 2 — the CROSS-LOCK view)

*Charge: harden/challenge/refine every family against the failure-mode checklist (vacuous
convergence, spec-cites-itself, gates-that-cannot-fail, the elegant-reduction trap, legacy aliases,
masked fallbacks, unverified gestalt, consumer-less substrate) + constitution violations +
prototype-evidence misuse — and, as critic 2, WEIGHT THE CROSS-LOCK VIEW: where this lock
contradicts, double-homes against, or defers to the GN/PT codex, CROSS-LOCK adjudications, and the
other locks. Read against the HARDENED re-fold FAMILY-SPECS (15:03), PROTO-1 (15:16, the workspace
divorce — authored AFTER the re-fold), PROTO-2 (13:40), PROTO-2-iso (15:23 — also after the re-fold),
CRIT-1 (15:33), the five research reports, CROSS-LOCK, and the live tree. Every load-bearing fact
re-verified at HEAD `c43126422` — I did not take the probes, the re-fold's reconciliation of them, or
CRIT-1 on their word; I ran the tree.*

**Verdict in one line.** The hardened re-fold is an honest advance — it downgrades both headlines to
LEAN/UNPROVEN and banks HM(X) as the floor. But it was written BEFORE its own two sharpest probes
(PROTO-1 workspace-divorce, PROTO-2-iso) and so carries four cross-lock defects it could not have
seen: **(1)** the structural axis is not "vocabulary + who-declares-key" — it is blocked behind
**GN-1 codepath unification (the loom)**, a LOCK-7⊕LOCK-3 totality build, and the mask it targets
lives on the **retiring** substrate (XL-A) — so **Brief 1 tests the wrong artifact and repeats the
plan PROTO-1 refuted**; **(2)** the pass's banked "durable output" — HM(X) with let-generalization —
**is itself unbuilt**: the running CSP (leading carrier 1B) carries **zero** let-gen (verified), so
§5.1's "one architecture" collapse is vacuous at the polymorphism layer and the floor the whole lock
rests on is SPEC_ONLY; **(3)** the residue solver (Open X) and the iso-carrier argument (PROTO-2-iso)
**double-home** against LOCK-2's csp-solver and LOCK-6's TS plane respectively — neither is LOCK-1's
to seat alone; **(4)** both pass-2 probes are **SPEC_ONLY** (verified unlanded) and X8 is **unmet
after two passes**. Floors hold within ±2; no family is convergence-eligible.

---

## 0. WHAT I VERIFIED AT SOURCE (HEAD `c43126422`) — the load-bearing facts

Every claim below is a command I ran, not a report I trusted.

- **[CONFIRMED — both probes SPEC_ONLY; X8 unmet TWICE].** `assign` (`constraint/revise.rs:27`) at
  HEAD is the raw 3-line last-writer-wins (`if slot==Some(&ty){false}else{*slot=Some(ty);true}`),
  **NOT** the refinement-aware `match` PROTO-1(old)/§1B-B1 claim delivered. `TypeDesc::Row` grep in
  `type_desc.rs` = **0**; `constraint/shape.rs` **absent**; `LayoutKind` = `{Struct, TaggedEnum,
  UntaggedEnum, NewtypeWrapper}` — **no Row**. `carrier_iso_ts.rs` / `proto2_row_shape.rs` are patch
  artifacts, not tree files. By XL-H every pass-2 "MEASURED/byte-identical/N passed" is a worktree
  claim → **SPEC_ONLY**. X8 (pass-1: "land the additive `assign` hardening regardless of carrier")
  is unmet after **two** passes — the single carrier-INDEPENDENT deliverable is still absent.
- **[CONFIRMED — the doubly-dead defect, the pass's one genuine advance].** `grep '\.join('
  crates/ir/src/passes/types/` = empty; `TypeDomain::join` (`domain.rs:66`) has zero call sites. The
  live carrier is `assign` (LWW). PROTO-1(old)'s premise-demolition (1A's "running CSP unsound" lever
  is void) is sound and verified. It tilts the carrier toward 1B — but see §1B.
- **[CONFIRMED — the WORKSPACE DIVORCE is real and dispositive (PROTO-1, §1 below)].**
  `skinny/Cargo.toml` declares its OWN `[workspace]`; the main `Cargo.toml` does not list skinny.
  `skinny/crates/ir` is package **`ir`**; `crates/ir` is package **`bbnf-ir`** — two divorced trees.
  `grep -rn "TypeDesc|StructRegistry|LayoutKind|struct_registry" skinny/crates/` = **0 hits**. The
  mask (`skinny/crates/codegen/src/runtime_generator.rs:98 has_rule("value")`) is **present**; Open
  R's lowering sink `TypeAnalysis::from_ir` lives in `crates/core` (bbnf-ir side). **Open R's
  classification physically cannot reach the skinny mask.**
- **[CONFIRMED — the running CSP has NO let-generalization].** `grep -rniE
  "generali|instantiat|polymorph|scheme" crates/ir/src/passes/types/` = **1 hit, a module
  doc-comment** ("the *generalized* csp-solver crate" — the crate's name, not let-gen). `project_types`
  is a monotone `csp.propagate()` sweep with `RefConstraint` suspension — not `generalize`/`instantiate`.
- **[CONFIRMED — the iso path is a REAL shipping consumer, not a stub].** `HeterogeneousAltJoin(Vec
  <TypeDesc>)` present (`type_desc.rs:63`). `type_desc_to_ts` (`ts/projection.rs:11`) is the sole
  `TypeDesc→TS` sink and is consumed by the **build-time TS emitter** (`ts/emitter/grammar.rs:55,121`)
  — i.e. it sits on the FAST TS face XL-D blessed, not a refuted runtime decoder. A fused monotype (1A)
  and a `TypeENode` (1D) are not `TypeDesc` values → **no `type_desc_to_ts` path**.
- **[CONFIRMED — the structural axis is void on the REAL grammar].** `grammar/json/json.bbnf`:
  `pair = string, colon >> value` (no `key` role), `object = "{" >> ((pair << comma?)*)?w << "}"`. On
  this body PROTO-2's own BARE case measured `class(object) = Unknown`. The `json_templates` mask
  stays uncovered on the exact grammar it exists for.
- **[CONFIRMED — X3 single-store = LOCK-1⊕LOCK-2 merge].** `csp_strategy/constraints/{dispatch,
  layout}.rs` install into `Csp<StrategyDomain>`; `passes/types/constraint` installs into
  `Csp<TypeDomain>`; both over the same `csp-solver = "0.1"` (`crates/ir/Cargo.toml:18`). Two federated
  stores, one engine.

---

## 1. THE FOUR CROSS-LOCK CROSS-CUTTING FINDINGS (critic-2's lens)

### 1.1 [XL-A + Constitution ix] The structural axis is a GN-1 LOOM obligation — Brief 1 tests the RETIRING artifact

The re-fold frames `List ≠ Row` closure as "(a) land `TypeDesc::Row`, (b) LOCK-7 who-declares-key,
(c) GN-3.W3, (d) delete `has_rule("value")`" (§5.4) and hands it to **Brief 1** — "land Row in
`bbnf-ir` … then DELETE `has_rule("value")` (`skinny/crates/codegen/…`) … regen and diff." PROTO-1
(authored after the re-fold) proves this is **physically incoherent**: Row lands in `bbnf-ir`; the
mask lives in package `ir` (skinny), a divorced workspace that references **no** `TypeDesc` (§0). A
perfectly-landed, perfectly-annotated Row derivation **cannot cross the workspace boundary** to inform
`has_rule("value")`; and PROTO-1 §5 shows deleting the mask **regresses** (routes JSON to
`emit_general_descent`, which has no Row/List classifier). So Brief 1's pass-condition ("mask deletes
AND regen byte-identical") is **pre-set to fail on the retiring substrate** — a gate-that-cannot-pass,
the mirror of a gate-that-cannot-fail.

**The cross-lock weight CRIT-1 under-states:** this is not merely "the blocker is bigger." Per **XL-A**,
skinny@HEAD is the MEASUREMENT SUBSTRATE ONLY — it RETIRES; **GN-1 (the loom) is the
tree-of-record-to-be**, seeded by unifying skinny's sink-only/general-descent codegen with bbnf-ir's
`StructRegistry`/`TypeDesc` classifier. The mask closure Brief 1 chases is **exactly the GN-1
codepath-unification work** (Task #54: GN-1 loom) — a LOCK-7⊕LOCK-3 totality build, **not a LOCK-1
pass-3 gate and not even a pass-3 question.** Running Brief 1 against skinny@HEAD tests-the-wrong-
artifact (XL-H wrong-artifact-risk) and re-litigates a codepath divorce that GN-1 exists to dissolve.
**Directive:** re-pose the structural axis as a NAMED GN-1 obligation routed to GESTALT-B, not a
LOCK-1 brief; the LOCK-1 residue is only the engine-agnostic vocabulary widening (`TypeDesc::Row`),
which is a LOCK-3 record-expressiveness fix (X4) any carrier consumes.

### 1.2 [Vacuous-convergence] The banked "durable output" — HM(X) with let-generalization — is ITSELF unbuilt

§5.1 banks "HM(X) = equality trunk + bounded residue + **let-generalization**" as "the pass's real,
durable output … NOT re-litigated," and §1A calls 1A "the **ONLY** family that carries polymorphism."
These contradict, and the tree adjudicates against the floor: the running CSP (leading carrier **1B**)
has **zero** let-generalization (§0, verified — 1 doc-comment hit). Consequences the convergence
percentages hide:

- **The floor both families "share" is shipped by neither.** 1A (which would carry let-gen) is
  SPEC_ONLY, unbuilt; 1B (running) omits it. So "1A and 1B collapsed onto one architecture" (§5.1) is
  **false at the polymorphism layer** — the exact layer let-gen exists to guarantee (false-fusion
  soundness). The pass's "durable output" is a target, not an artifact.
- **No 1B gate checks false fusion.** G3 ("generalization prevents false fusion") is 1A-only; 1B's
  G1–G5 are hardening/census/width/convergence. Whether `RefConstraint` suspension is observationally
  equivalent to instantiate-fresh-per-`Ref` is **asserted, never shown** — a spec-cites-itself at the
  soundness core.
- **"1B = cheapest net-0 additive diff" overstates.** Reaching the banked HM(X) from 1B requires
  **growing let-generalization into the monotone CSP** — unbuilt, unmeasured, emphatically not "change
  nothing that fires." The carrier lean's own cost premise omits this graft.

This is the deepest failure-mode hit in the pass: the convergence floors (43–56%) are computed against
an architecture **no family is shown to instantiate**. Pass 3 must either build a false-fusion fixture
for 1B and cost the let-gen graft, or stop banking the 1A/1B collapse as "one architecture" and re-open
§5.1.

### 1.3 [Double-home] The residue solver (Open X) is LOCK-2's engine, not a private LOCK-1 seam

Open X routes widths/overloads/decode-class to "a bounded post-solve … the same seam whichever carrier
wins" over `decision_csp.rs`'s two-mode `csp-solver`. But that IS the vendored `csp-solver = "0.1"` that
LOCK-2's `csp_strategy/` (2C CSP-CENTRAL — "the decision driver IS a CSP") already claims as its
decision-driver engine (§0, X3 verified: two federated `Csp<_>` stores, one crate). So the
width-defaulting lattice is **double-homed**: is it LOCK-1's Open X residue seam or LOCK-2's
decision-driver CSP? The re-fold flags X3 for the *literal single store* but does not name that **Open
X and 2C are the same engine** — a GESTALT-B consolidation. Compounding it: the census that claims "the
residue ≈ 0 everywhere" (justifying "route only the residue through CSP") **read its own coarse output**
over a corpus with zero overload code (X4) — spec-cites-itself. If the residue is truly 0, then 1B's
CSP-store distinctiveness AND 1D's cost-extraction AND the whole HM(X)-vs-pure-HM stratification are
**near-vacuous**; if it is not 0, the census must run on a modeling instrument. Either way "defaulting
dominates" is unrun on this pass's instrument (Open X UNRUN two passes running).

### 1.4 [Double-home] The carrier-on-iso argument is a LOCK-6 input, not LOCK-1's to seat

PROTO-2-iso's HEAD-real negative — only `TypeDesc` crosses `type_desc_to_ts`; a monotype (1A) and a
`TypeENode` (1D) categorically have **no** TS path (§0) — is genuine and decisive, and I bank the
**Sum half** (`HeterogeneousAltJoin → type_desc_to_ts`) as HEAD-real. But two cross-lock qualifiers the
re-fold (predating the probe) and even CRIT-1 under-weight:

- The **Row half** rides Brief-1's uncommitted vocabulary → **SPEC_ONLY**. Honest split: *Sum-crossing
  HEAD-real; Row-crossing SPEC_ONLY; the monotype/`TypeENode`-have-no-`*_to_ts` negative HEAD-real and
  decisive.*
- Constitution iii is **LOCK-6's verdict to render** ("LOCK-6 decides where reasonable ends; measured,
  not asserted"). The carrier-on-iso seat is a **LOCK-6 input**, conveyed — LOCK-1 may bank the negative
  as a fact but cannot *seat the carrier on iso alone*. It reframes the carrier fork (CRIT-1's G-XL-3):
  if 1A/1B ship the same architecture (disputed by §1.2) the deciding axis is iso, and 1A-as-carrier is
  **pre-eliminated on iso** — building Option A then measures a race whose loser is already out. Pass 3
  must state which axis is dispositive (build-cost → run Option A; iso → nearly closed FOR 1B by the
  HEAD-real negative, modulo landing Row), and route the seat to LOCK-6/GESTALT-B for ratification.

---

## 2. FAMILY 1A — UNIFICATION-FIRST · convergence **55%** (held at floor)

The HM(X) architecture is banked consensus (but §1.2: unbuilt). As a distinct *carrier*, 1A is
**categorically last on iso** by a HEAD-real fact (a monotype is not a `TypeDesc` → no
`type_desc_to_ts`; §0/§1.4) — sharper than the re-fold's "no differential produced." Its sole
displacement justification (running CSP unsound) is doubly-dead (§0). Architecture intact; Option A
unbuilt blocks a larger cut. Held, not cut, only because §1.2's let-gen finding is the one place 1A is
*distinctively* right (it alone would carry polymorphism).

**Enumerated open gaps:**
1. **[carrier, UNRUN] Option A (union-find re-seed) never built — G1–G7 all asserted.** 1A must delete
   ~101 LOC + rewire ~91 sites + re-invent the `BoxedEnum`→structural refinement lattice inside
   union-find, for zero measured gain. Its make-or-break parity race is SPEC_ONLY.
2. **[cross-lock iii, categorical] no TS mirror — and no path to one.** A fused monotype requires
   authoring a brand-new `*_to_ts` sink from scratch (§1.4). Ranks 1A LAST on iso, HEAD-real, not a
   parity-margin loss.
3. **[cross-lock] LOCK-3 sink named, lowering unshown.** No artifact shows a fused monotype lowering
   through `TypeAnalysis::from_ir` (the incumbent `TypeDesc` does).
4. **[row unmeasurable] zero row-shaped `->` in the real corpus (verified: json's arrows are all scalar
   leaf projections).** Leijen vs Gaster–Jones (Wand's trap) cannot be exercised; behind GN-3.W3.
5. **[research-A §4 unproven] the `HeterogeneousAltJoin`-retires-under-unification claim is unshown** —
   no probe shows the Sum obligation retiring rather than coexisting with union-find's structural arm.
6. **numeric-width residue UNMEASURED** on this pass's instrument (Open X, §1.3).

## 3. FAMILY 1B — ONE-CONSTRAINT-STORE · convergence **54%** (−2 from floor 56) · single-store BLOCKED-candidate

The leading carrier candidate and iso-safe (Sum-crossing HEAD-real; §1.4). But three compounding
cross-lock debits pull it **below** CRIT-1's 55 and the stale-CRIT-2's 56: **X8 unmet TWICE** (a real
stagnation signal — the one "ships-regardless" deliverable absent across two passes), **it does not
ship the banked HM(X)** (no let-gen, §1.2), and **its clash obligation is consumer-less** (§viii). The
HEAD-real Sum-iso advance offsets only partially. Its residual distinctiveness ("cheapest diff") is
itself contingent on an unbuilt let-gen graft.

**Enumerated open gaps:**
1. **[FATAL, X8 unmet ×2] the additive `assign` hardening is STILL NOT LANDED (§0, verified LWW).** The
   one carrier-INDEPENDENT deliverable is absent at HEAD after two passes; G1's byte-identity was
   measured on a worktree — SPEC_ONLY by XL-H. Land it or the family has shipped nothing.
2. **[NEW, §1.2] 1B does NOT ship the banked HM(X) — no let-generalization (verified).** So §5.1's "one
   architecture" collapse is unsound at the polymorphism layer; no 1B gate tests false fusion; "net-0
   cheapest diff" omits the unbuilt let-gen graft. The leading carrier's polymorphic soundness is
   untested.
3. **[consumer-less substrate, viii] B1's `TypeClash` obligation is parity-inert** — codegen reads
   `ir.types` + `struct_registry`, never `type_obligations`. Its only consumer is 1C's LSP bridge,
   **also unbuilt**. "Ships regardless of carrier" and "consumer-less until 1C lands" cannot both hold
   unscheduled — the joint wave is unnamed.
4. **[cross-lock, X3 double-home] single-store thesis = LOCK-1⊕LOCK-2 merge (verified).** The literal
   one-heterogeneous-store is a GESTALT-B consolidation; the residue solver double-homes with LOCK-2's
   csp-solver (§1.3). BLOCKED-candidate; bank the literal thesis, ship the federation (= HM(X) = 1A's).
5. **[spec-cites-itself, uncured ×2] the residue census (G2) + B4 store-shape (G3) never re-run/built.**
   "Residue ≈ 0 / decidable-core dominates" still reads the pass-1 census's own coarse output over a
   corpus with zero overload code. B4 (decode-class-depends-on-sum-variant: flat unsound cell vs
   per-variant `Sum` cells) — the family's sharpest store-shape decision — is on paper across two passes.
6. **[distinctiveness near-vacuous] once single-store is BLOCKED-candidate, 1B ships IS HM(X) = 1A's;**
   its residual identity ("cheapest diff") is contingent (gap 2).

## 4. FAMILY 1C — BIDIRECTIONAL-LOCAL · convergence **40%** (held) · BANKED (reactivation-gated)

Correctly banked as the winner's diagnostics/locality discipline. No pass-2 evidence moved it; PROTO-2
*reconfirms* the substrate wall rather than lifting it.

**Enumerated open gaps:**
1. **[structurally unrunnable, stands] make-or-break (R-C2/G1) has no input.** A declared/synthesized-
   *mismatch* `->` fixture must be hand-authored; the greenfield json's arrows are all scalar leaf
   projections. 1C's distinct identity is unfalsifiable until GN-3.W3 or that fixture lands.
2. **[reconfirmed §0b] the `->` front-end thread is broken.** The natural inline `(string -> key)` trips
   the typed-materialization-invariant (`lower/expression/wrap.rs:242`) — direct evidence GN-3.W3 is
   unbuilt. 1C's 40% is a ceiling until that wall falls.
3. **[consumer-less substrate, viii] the LSP bridge is unbuilt.** `type_obligations →
   lsp_types::Diagnostic{range}` = zero hits. Enriching obligations without a consumer fails the bar —
   and this is the same bridge 1B's B1 clash arm depends on (§3.3): **name the joint wave.**
4. **[LOCK-3 field] sub-span ceiling.** `Project.ty` is one whole-expr `String` span; a wrong ascription
   cannot underline the `T`. Bank the `PassDiagnostic.span` add + LSP bridge regardless of carrier — the
   re-fold says "name the wave"; **the wave is still unnamed** (falls between carrier-selection and
   GESTALT-B).

## 5. FAMILY 1D — TYPES-AS-ECLASSES · convergence **43%** (held) · structural-μ UNPROVEN (not "dissolved")

recursion-keep + "zero new engine" + cost-extraction correctly RETIRED. The one surviving hook
(structural `List ≠ Row`) is **UNPROVEN**, and — the finding CRIT-1 sizes right and the re-fold sizes
too small — blocked behind **GN-1 codepath unification** (§1.1), strictly larger than "vocabulary +
who-declares-key." PROTO-2 ("DISSOLVES 1D") and PROTO-1 ("BLOCKED") are **flatly contradictory verdicts
on the same question**; the tree adjudicates for PROTO-1 (§0).

**Enumerated open gaps:**
1. **[the divorce refutes "dissolve"] Open R physically cannot close the mask (§0/§1.1, verified).** The
   Row classifier lands in `bbnf-ir`; the mask is in package `ir` (skinny), which references no
   `TypeDesc`. PROTO-2's own `git diff skinny/` = empty — it never touched, never *could* touch, the
   mask. 1D is stuck, not superseded — and its structural axis is a **GN-1 loom / LOCK-7⊕LOCK-3** build.
2. **[make-or-break UNBUILT] Row-inference from an un-annotated body.** On the real json `object` body
   the classifier returns `Unknown` (§0). 1D's `TypeENode` deriving `Row` without the `key` role is
   unshown; 1D faces the identical missing ingredient as every engine (extraction ≠ inference).
3. **[cross-lock] "zero new engine" FALSIFIED — 3-lock blast radius, unmeasured.** The `union`
   scheduling amendment fires on LOCK-2 grammar-tier + LOCK-5 regex-tier (`egraph/rules/regex.rs`); a
   GESTALT-B call, cannot be settled in LOCK-1. Must not silently re-enter via 1D's saturation claims.
4. **[cross-lock vi/iii] name-collision + no TS path.** Net-new `TypeAnalysis` collides with the live
   `backend/driver/analysis.rs` driver struct; `TypeENode` has no `type_desc_to_ts` mirror. Ranks last
   on iso (§1.4).
5. **[elegant-reduction, uncured] AC-sum + positional injectivity coexistence UNBUILT** — injectivity is
   unsound for AC `Sum`; every probe stayed sound via separate e-graphs (tested *around*, never *through*).
6. **recursion-keep RETIRED (verified), cost-extraction IDLE (residue 0, §1.3).** Named negatives stand.

---

## 6. CONSTITUTION & PROTOTYPE-EVIDENCE AUDIT (cross-lock weighting)

- **vii (concrete artifacts or SPEC_ONLY) — VIOLATED at the probe level (§0).** All three pass-2 probes
  are patch-only. PROTO-1's own verdict is BLOCKED (not a positive landing); PROTO-2's DISSOLVE and
  PROTO-2-iso's Row-PASS both ride unlanded vocabulary. Only PROTO-2-iso's **Sum-half** negative is
  HEAD-real. Every "byte-identical / 1 passed / 4 passed" is a worktree claim — SPEC_ONLY by XL-H.
- **i (grammar totality) + no-workarounds + typed-materialization-invariant — masked fallback NOT closed
  (§0/§1.1).** `json_templates/value.rs` + `has_rule("value")` present; on the real grammar the
  classifier returns `Unknown`. The natural annotation that would close it trips `wrap.rs:242` — a second
  clause-i hazard. Owned jointly with LOCK-7, dischargeable only at GN-1.
- **v (zero runtime) — asserted by all, exercised by none on a LANDED change.** PROTO-1/PROTO-2-iso's
  "empty runtime diff" is trivially empty (they land nothing that touches runtime) and worktree-only.
- **iii (rust/ts isomorphism) — decides the carrier but is LOCK-6's verdict, conveyed (§1.4).** LOCK-1
  banks the HEAD-real negative (only `TypeDesc` crosses; monotype/`TypeENode` cannot) as a fact; it
  cannot seat the carrier on iso alone. The Row-crossing positive is SPEC_ONLY.
- **vi (parsimony / no name-gate) — 1D `TypeAnalysis` name-collision stands (verified).**
- **viii (consumer-less substrate) — TWO live breaches:** 1B's `TypeClash` obligation is parity-inert
  (codegen never reads `type_obligations`); 1C's LSP bridge is unbuilt (zero hits). They are the SAME
  unbuilt consumer — an unnamed joint wave.
- **ix (extant-tranche consciousness) — TWO owed:** (a) the GN-3 §1.5 supersession is recorded WORKING
  in XL-F(1) but still owed as a formal LOCK-7-signed entry, not the inline "factually void" the specs
  use; (b) **Brief 1 silently contradicts XL-A** by testing structural closure against the retiring
  skinny substrate rather than naming the GN-1 loom obligation — a supersession masquerading as a gate.
- **Gate-that-cannot-fail / cannot-pass — TWO stand:** PROTO-2's grep-gate forbids two hint shapes and is
  silent on the third (a `->`-declared `: key` annotation), so it cannot detect the goalpost-shift from
  "derive from body structure alone" (which measured `Unknown`); Brief 1's "mask deletes AND regen
  byte-identical" is pre-set to fail on the divorced/retiring substrate (§1.1).
- **Elegant-reduction trap — §5.4 in the flesh:** "and then structure is derived engine-agnostically
  (Open R)" reads *and-then-the-hard-part*; the hard part (real-grammar derivation) measured `Unknown`
  and is blocked behind GN-1.

## 7. PASS-3 CROSS-LOCK DIRECTIVES

1. **LAND the additive `assign` clash arm in-tree (X8, third call).** Until it is HEAD-reproducible with
   `types_heterogeneous_alt` green, no carrier verdict is MEASURED and 1B has shipped nothing.
   Co-schedule its consumer (the 1C LSP bridge) — name the joint wave, or B1 enriches a stream nothing
   reads (viii).
2. **Decide the carrier axis, then build the ONE decisive artifact.** If build-cost is dispositive:
   build Option A (union-find re-seed) and run the parity race — the seat is LEAN-1B, not CLOSED. If iso
   is dispositive (§1.4): it is nearly closed FOR 1B by the HEAD-real negative — land `TypeDesc::Row` so
   the Row-crossing joins the HEAD-real Sum-crossing, then route the seat to **LOCK-6/GESTALT-B** for
   ratification. State which axis governs; do not demand both briefs without a dispositive one.
3. **Build a false-fusion fixture for 1B and cost the let-gen graft (§1.2), or re-open §5.1.** The banked
   HM(X) is unbuilt at the polymorphism layer; the "one architecture" collapse cannot be banked while no
   family instantiates it.
4. **[GN-1 / GESTALT-B, NOT a LOCK-1 brief] Re-home the structural axis (§1.1).** The `List ≠ Row`
   closure is GN-1 codepath unification (LOCK-7⊕LOCK-3) against the tree-of-record, not a mask deletion
   on retiring skinny. LOCK-1's residue is only the engine-agnostic `TypeDesc::Row` vocabulary widening
   (a LOCK-3 fix). Withdraw Brief 1 as written; route it to GESTALT-B as "blocked pending GN-1 loom."
5. **[GESTALT-B] Adjudicate the Open X ⊕ LOCK-2 csp-solver double-home (§1.3)** and run the residue
   census on a modeling instrument (overload/legality/decode-class) before "defaulting dominates" is
   banked; build B4 both ways.
6. **[LOCK-7 / ix] Formalize the GN-3 §1.5 supersession with LOCK-7 sign-off; correct Brief 1's silent
   XL-A contradiction into a named supersession.**

## 8. CONVERGENCE SUMMARY (CRIT-2, cross-lock)

| Family | Disposition | CRIT-2 % | Δ vs floor | Dominant cross-lock open gap |
|---|---|---:|---:|---|
| **1A** UNIFICATION-FIRST | ADVANCED (carrier cand. #1) | **55** | 0 | iso CATEGORICALLY last (monotype has no `type_desc_to_ts`, HEAD-real); Option A unbuilt; row unmeasurable; distinctively right only where §1.2's let-gen holds |
| **1B** ONE-CONSTRAINT-STORE | ADVANCED (carrier cand. #2, leading) | **54** | −2 | X8 unmet ×2 (ships-regardless deliverable absent); does NOT ship banked HM(X) (no let-gen); clash obligation consumer-less; single-store = LOCK-1⊕LOCK-2 merge; census self-cites. Offset by HEAD-real Sum-iso advance |
| **1C** BIDIRECTIONAL-LOCAL | BANKED (reactivation-gated) | **40** | 0 | make-or-break has no input; GN-3.W3 wall reconfirmed broken; LSP bridge unbuilt (viii); banked wave unnamed |
| **1D** TYPES-AS-ECLASSES | BANKED (structural-μ UNPROVEN) | **43** | 0 | workspace divorce → structural axis is a GN-1 loom build (LOCK-7⊕LOCK-3), not annotation; PROTO-2 contradicts PROTO-1, tree adjudicates BLOCKED; Row/TS SPEC_ONLY; 3-lock/name-collision stand |

**Lock-wide (cross-lock).** The pass-2 architecture — HM(X) trunk + defaulting residue + 1C folded +
structure via engine-agnostic widening — is prior-art-consonant and probably right *as a target*. The
hardened re-fold honestly downgraded both headlines. But it was authored before its two sharpest probes,
and the tree adjudicates four cross-lock defects it could not see: **(1)** the structural axis is a
**GN-1 loom obligation** (XL-A), so Brief 1 tests the retiring artifact and repeats the plan PROTO-1
refuted; **(2)** the banked "durable output" HM(X)-with-let-gen is **itself unbuilt** — the running
carrier omits let-generalization, so the convergence floors measure against an architecture no family
instantiates (vacuous-convergence); **(3)** the residue solver and the iso-carrier seat **double-home**
against LOCK-2's csp-solver and LOCK-6's TS plane — neither is LOCK-1's to settle; **(4)** all three
probes are SPEC_ONLY and X8 is unmet after two passes. The carrier seat is **LEAN-1B, not CLOSED**; the
structural axis is **UNSOLVED by every family and re-homed to GN-1**; the banked HM(X) is a target, not
an artifact. No family is convergence-eligible (pass 2 of ≥3; no clean-pass streak). Floors hold within
±2; the one cut (1B −2) reflects two-pass stagnation + the let-gen soundness hole, not architecture change.
