# LOCK-1 · pass-2 · CRIT-1 (ADVERSARIAL CRITIC 1 of 2 — the WITHIN-LOCK view)

*Weighting: internal soundness of each family spec + whether the two prototypes actually tested the
sharpest uncertainty. Read against FAMILY-SPECS.md, PROTO-1/PROTO-2, the five research reports, the
pass-1 REGISTRY floors (1A 56 · 1B 54 · 1C 40 · 1D 44, CRIT-2 binding), CROSS-LOCK, and the live tree
(HEAD-verified this pass). Constitution + failure-mode checklist applied to every family.*

**Verdict in one line.** The *architecture* (HM(X) trunk + ordered-defaulting residue + 1C diagnostics
overlay) is genuinely converged and correctly banked. But **the two prototypes this pass over-claim on
their own pass/fail criteria**: Brief 1's carrier race ran only *one* of its two carriers, and Brief 2's
"Open R dissolves 1D / closes the masked fallback" is **contradicted by the live tree** — the fallback is
still present, and the classifier *refuses* on the shipping grammar. Floors hold; two "decided" questions
are re-opened as **half-tested**, not settled.

---

## 0. LIVE-TREE RE-VERIFICATION (the load-bearing corrections this critic adds)

Three claims in FAMILY-SPECS/PROTOs I re-checked against HEAD (`ce51c36fc` per the PROTO base-pins):

- **CONFIRMED — `TypeDomain::join` is dead.** `grep '\.join(' crates/ir/src/passes/types/` → **empty**.
  The "unsound as shipped" lever is void; §0's doubly-dead finding stands. No challenge here.
- **REFUTED (partial) — "the live JSON grammar has zero `->`" (research-D §2, echoed in PROTO-2 method).**
  The *shipping greenfield* `grammar/json/json.bbnf` carries **four `->` arrows** (`null -> 0u8`,
  `bool -> true|false`, `number -> f64`, `string -> decode_json_string_to_arena(input) : String`). The
  *substantive* point survives — **`pair = string, colon >> value` declares NO key role** — but the
  premise as written is false, and the correction is load-bearing for Brief 2 (below).
- **REFUTED — "Open R closes the masked `json_templates` fallback" (Brief 2 pass/fail; PROTO-2 verdict;
  FAMILY-SPECS §5.4).** The mask is **still in the tree at HEAD**:
  `skinny/crates/codegen/src/runtime_generator.rs:98 — if !sink_only.has_rule("value")`. PROTO-2's own
  report states `git diff crates/core/src/runtime/` is **empty** and it never touched
  `runtime_generator.rs`. **The fallback it was chartered to close was never removed, never even
  regression-tested for removal.** A parallel `Row` path was built on a *synthetic* grammar in a
  *different crate* (`crates/ir/`); the mask lives in the retiring measurement substrate (`skinny/`).
  These do not meet.

---

## FAMILY 1A — UNIFICATION-FIRST · convergence **55%** (was 56)

The trunk *architecture* (HM(X): union-find equality + let-generalization + bounded residue) is
prior-art-solid and correctly BANKED as consensus. The *family-1A-specific* claim — union-find as the
**equality carrier** — is now further disfavored, and the pass did not actually test it.

**Enumerated open gaps:**

1. **THE CARRIER RACE WAS HALF-RUN (prototype-evidence misuse — Option A is SPEC_ONLY).** Brief 1's
   pass/fail was explicit: *"both must terminate and produce byte-identical `ir.types`."* PROTO-1 built
   and ran **only Option B**; Option A ("union-find re-seed") was **"NOT built (cost-bounded)"** and
   *sized from source*. The FAMILY-SPECS text nonetheless declares "THE EQUALITY-CARRIER SEAT GOES TO
   1B" and treats 1A-as-carrier as adjudicated. It is not adjudicated — it is **estimated**. 1A's cost
   argument (deletes 101 LOC + rewires 91 sites + re-derives the refinement lattice for zero measured
   gain) is *directionally persuasive* and the void-defect finding is real, but a race with one runner is
   not a race. **The seat tilts to 1B on cost asymmetry; it is not closed by measurement.**
2. **ZERO direct in-tree prototype of 1A, still (pass-1 gap uncleared).** No union-find HM has been run
   against root on any corpus. Its G1 (terminates + parity + ≤ wall-time vs the running pass) is
   unexecuted. The `Sum`/`Row`/`Vec`/`Option` structural arms + occurs-check are *new code with no proven
   seed* — PROVE-08 supplies only the 5-leaf flat unifier (PROTO-1 confirms this from source).
3. **Row-discipline (O2) is unmeasurable — zero `->` corpus.** Leijen-vs-Gaster-Jones is
   principal-type-critical (Wand's trap) but cannot be exercised: no typed-`->` grammar exists in tree.
   Unchanged from pass-1; a bet, not a finding.
4. **TS mirror unshown (O4 / X7).** The fused monotype's path to `type_desc_to_ts` via `TypeDesc` is
   asserted, never exercised by a differential. No prototype produced a Rust/TS parity crossing for a
   `Sum`/`Row`-bearing type. Constitution-iii isomorphism floor unmet for 1A.
5. **The Join-obligation-subsumption claim (research-A §4) is unproven.** 1A asserts `Row`/`Sum`
   unification makes `HeterogeneousAltJoin` unreachable; no probe shows the variant retiring rather than
   coexisting (two carriers for one fact — the risk 1A itself names).

---

## FAMILY 1B — ONE-CONSTRAINT-STORE · convergence **58%** (was 54)

The strongest family on *within-lock soundness*: it runs on the surviving base, PROTO-1 measured its
hardening as **byte-identical + parity wall-time (1.42s vs 1.43s) + net-0 LOC + G-1B-5 green**. But its
*distinguishing* value is thin, and the spec text contains a proven-wrong instruction.

**Enumerated open gaps:**

1. **THE B1 TEXT IN FAMILY-SPECS IS THE FORM PROTO-1 PROVED WRONG.** §1B integration seam and Brief 1(B)
   both say: *"On `Some(existing) != Some(ty)` record `TypeObligation::TypeClash` and refuse the
   overwrite."* PROTO-1 measured that **literal** form refusing **all 69** `BoxedEnum→structural`
   refinements → freezes vars → **not byte-identical** (regresses generated output). The *working* form
   is **refinement-aware** (`existing != BoxedEnum` guard). The spec's own gate G1 ("byte-identical")
   would FAIL against the spec's own B1 prose. The spec must adopt the refinement-aware form verbatim
   (PROTO-1's net-0 `match`) — not a cosmetic edit; the naive form is a live regression.
2. **THE DISTINGUISHING DELIVERABLE FIRES ZERO TIMES AND IS CONSUMED BY NOTHING (Constitution viii +
   gate-that-cannot-fail).** The concrete-clash arm is **dormant (0 firings on json/css/sheets — measured
   69/69 were BoxedEnum refinements)**, and the `TypeObligation::TypeClash` it records is **parity-inert**
   (PROTO-1: *"codegen consumes `ir.types` + `struct_registry`, never `type_obligations`"*). So 1B's
   *unique* contribution is a diagnostic channel that (a) never triggers on any corpus and (b) has no
   downstream reader. FAMILY-SPECS honestly labels this "latent-hazard closure, not defect repair" — good
   — but that means **1B-as-carrier delivers, functionally, "keep the running code and change nothing that
   fires."** The carrier question (1A vs 1B) is therefore nearly *vacuous*: both roads end at "the running
   CSP stays"; they differ only in whether one *also* ports union-find. This should be stated as the
   finding, not buried.
3. **The single-heterogeneous-store thesis is BLOCKED-candidate (X3) and remains so.** Literally a
   LOCK-1⊕LOCK-2 store merge; not buildable inside LOCK-1; a GESTALT-B call. Correctly banked. The
   *architecture* 1B ships is HM(X) — identical to 1A — so 1B has no architecture-level distinctiveness
   left once the store thesis is banked. Its identity is purely "cheapest diff."
4. **TS mirror / product-lattice census unrun (R5, B3).** The product domain (width/decode/legality
   axes) was never instantiated as a running `Csp`; the pass-1 census "read its own coarse output"
   (spec-cites-itself, named in G2) and pass-2 did not re-cut it. B4 (flat-cell-vs-per-variant decode) was
   not built either way. These are the entire "widen the store" half of 1B — unexecuted.

---

## FAMILY 1C — BIDIRECTIONAL-LOCAL · convergence **40%** (held)

Correctly BANKED as the winner's diagnostics/locality discipline, not a fourth engine. No pass-2 probe
targeted it; nothing moves it.

**Enumerated open gaps:**

1. **Make-or-break (G1/R-C2) still unrun.** The side-by-side localization proof needs a *typed `->`
   mismatch* fixture. FAMILY-SPECS calls the corpus "zero `->`" — imprecise: the greenfield json now
   carries 4 arrows, incl. a host-fn projection `-> decode_json_string_to_arena(input) : String`. So a
   fixture is *not structurally impossible* (arrows exist); what is missing is a **declared/synthesized
   mismatch** fixture, which must be hand-authored (research-C §7.1's `frac -> Ratio`). "Structurally
   unrunnable" overstates; "unbuilt, and owed a hand-authored fixture" is exact.
2. **The consumer-less-substrate bridge (viii) is named but undelivered.** FAMILY-SPECS §1C integration
   requires an `analysis`-crate `type_obligations → lsp_types::Diagnostic{range}` bridge ("zero hits
   today; enriching the stream without a consumer fails the bar"). No prototype built it. Until it lands,
   the span-add is itself a consumer-less substrate — the exact edict 1C invokes against skinny's empty
   `TypeObligation.span`.
3. **Sub-span ceiling (O3) unresolved — a LOCK-3 field.** `Project.ty` is one `String` carrying the
   whole-expr span; a wrong ascription cannot underline the `T`. 1C's quality thesis is capped by an IR
   field it does not own. Correctly flagged for banking regardless of carrier; still open.
4. **The banked items (`PassDiagnostic.span` add + LSP bridge) have no owner-wave.** They are "bank
   regardless of winner" — but no gate schedules them, so they risk falling between carrier-selection and
   GESTALT-B. Name the wave.

---

## FAMILY 1D — TYPES-AS-ECLASSES · convergence **44%** (held)

Recursion-keep + "zero new engine" + cost-extraction correctly RETIRED as named negatives. The one
surviving hook (structural `List ≠ Row`) was the target of Brief 2 — and **Brief 2's verdict over-claims
against the live tree.** 1D is neither cleanly dissolved (as PROTO-2 asserts) nor validated; it is *stuck*
on an unclosed gap that no engine closes on the shipping grammar.

**Enumerated open gaps:**

1. **THE "DISSOLVE" VERDICT IS OVER-CLAIMED — THE CLASSIFIER REFUSES ON THE SHIPPING GRAMMAR
   (elegant-reduction trap + gate-that-cannot-fire-on-the-real-artifact).** PROTO-2 produced `Row` **only
   on a synthetic TYPED grammar** it authored (`keystr = /"[^"]*"/ -> input : key`). On its **BARE** case
   — the one that mirrors the shipping `grammar/json/json.bbnf`, whose `pair = string, colon >> value`
   declares **no key role** — the classifier returns **`Unknown` (honest refusal)**. So for the *actual*
   corpus the record/list distinction is **still not derived**; `object` stays indistinguishable from a
   list-of-pairs. The probe demonstrated the classifier on a grammar that **is not the one shipping**.
2. **THE MASKED FALLBACK WAS NEVER CLOSED (consumer-less / gate-that-cannot-fail).** Brief 2's pass/fail:
   *"the derived `Row` lowers through `from_ir` **and closes the `json_templates/value.rs` masked
   fallback**."* Verified at HEAD: the mask (`runtime_generator.rs:98 has_rule("value")`) is **still
   present**; PROTO-2's `git diff` over runtime is **empty**; it edited `crates/ir/` while the mask lives
   in `skinny/crates/codegen/`. A `Row` value was *produced in parallel*; the fallback it was meant to
   *replace* was neither removed nor shown removable without regression. **The gate's second clause was
   not exercised.** This is precisely a load-bearing step reading "…and-then-close-the-fallback" left
   unconverged.
3. **THE `->` KEY REQUIREMENT *IS* THE "HINT REQUIRED" BRANCH, RELABELED.** PROTO-2's own caveat: *"the
   record bit requires the key role to be declared (`->`)."* Brief 2's disjunction was: *hint required →
   1D BLOCKED-candidate; no hint → dissolve.* The shipping json.bbnf lacks the key role, so closing the
   mask on the real grammar requires **re-authoring the grammar** to add `-> key`. Whether one calls that
   "a hint" or "the honest locus," it is a **per-grammar authoring input the shipping artifact does not
   carry** — functionally the BLOCKED-candidate branch. FAMILY-SPECS §5.4/§6 record "NO hint → Open R
   dissolves 1D" as the outcome; the evidence supports at most "no *byte* hint, but a *declaration* the
   real corpus lacks." **The dissolution is conditional on a grammar edit whose regen-byte-identity and
   parse-semantics preservation are unshown.**
4. **`TypeDesc::Row` / `LayoutKind::Row` land only in a probe branch (not at HEAD).** `grep Row
   crates/ir/src/types/type_desc.rs` → empty at HEAD; `shape.rs` does not exist at HEAD. The vocabulary
   widening (Open R) is real and engine-agnostic (X4), but it is *unlanded*, and its TS projection
   (`Record<string,T>`) has **no differential test** — Constitution-iii floor unmet, same as 1A/1B.
5. **Recursion-keep/cost-extraction RETIRED — no residual gap, correctly.** These are clean negatives; no
   challenge.
6. **`union`-scheduling amendment = 3-lock blast radius (X6), unmeasured.** Correctly deferred to
   GESTALT-B; not a within-lock gap, but it must not silently re-enter via 1D's saturation claims.

---

## CROSS-FAMILY GAPS (bind the whole lock)

- **G-XL-1 — Open S is asserted-as-reading, unproven (elegant-reduction trap).** §5.6: *"a reading of the
  running CSP, to be shown faithful, not built."* The clean mapping (SYNTH=synthesized attr,
  CHECK=inherited attr, cycle-break=bounded circular AG) reads as convergence but the **fidelity proof is
  deferred**. A load-bearing "to be shown faithful" is an open gap, not a closed one. Either demonstrate
  the running `mod.rs:72-127` cycle-break IS a monotone-lattice circular AG, or stop counting Open S as
  resolved.
- **G-XL-2 — the TS isomorphism floor is unmet lock-wide.** Every carrier record (1A monotype, 1B
  `TypeDesc`, 1D `TypeENode`→`Row`) names `type_desc_to_ts` as its mirror (X7). **No prototype this pass
  produced a single Rust/TS differential.** Constitution v (zero-runtime-cost) is argued by construction;
  Constitution iii (rust/ts isomorphism) is entirely unexercised. This is the largest un-probed surface
  in the lock.
- **G-XL-3 — the carrier fork's "decided" framing outruns its evidence.** With Brief 1 half-run and
  Brief 2 over-claimed, the pass produced **one** clean measurement (1B hardening is byte-safe + net-0)
  and **two** estimates dressed as verdicts (1A cost, Open-R dissolution). The honest pass-2 state: *the
  running CSP stays; whether to also port union-find is unraced; whether `List≠Row` is inference-derivable
  on the shipping corpus is UNSHOWN (refusal + unremoved mask).* Pass 3 must (i) build Option A and run
  the real race, and (ii) run Brief 2 on the **actual** `grammar/json/json.bbnf` (or a documented
  re-authoring with regen-byte-identity), and actually **delete** `has_rule("value")` under test.

---

## CONVERGENCE SUMMARY (within-lock, CRIT-1)

| family | disposition | CRIT-1 % | movement | binding reason |
|---|---|---|---|---|
| 1A UNIFICATION-FIRST | ADVANCED | **55** | −1 | carrier role estimated not raced (Option A unbuilt); zero in-tree probe; row/TS unmeasured |
| 1B ONE-CONSTRAINT-STORE | ADVANCED | **58** | +4 | measured byte-safe hardening; but B1 spec text is the proven-wrong form, its clash arm is dormant+unconsumed, product-census/TS unrun |
| 1C BIDIRECTIONAL-LOCAL | BANKED | **40** | 0 | make-or-break unbuilt; LSP bridge (viii) undelivered; sub-span ceiling open |
| 1D TYPES-AS-ECLASSES | BANKED | **44** | 0 | "dissolve" over-claimed — real-corpus refusal + unremoved mask + `->`-key = relabeled hint; Row/TS unlanded |

No family is convergence-eligible (pass 2 of ≥3; two consecutive clean passes not reached). The
architecture is banked and sound; the two *specific* decisions the pass claims to settle (carrier seat,
Open-R dissolution) are re-opened as **half-tested**. Floors otherwise hold — the CRIT-2 pass-1 floors
were not eroded by any load-bearing error; the two nudges (1A −1, 1B +4) reflect *within-lock soundness*
(1B measured; 1A's carrier estimate + 1D's over-claim), not architecture change.
