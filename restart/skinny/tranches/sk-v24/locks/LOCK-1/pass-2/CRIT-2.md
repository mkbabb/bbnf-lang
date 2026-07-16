# LOCK-1 — THE SOLVER SPINE · pass-2 · CRIT-2 (adversarial critic 2 of 2 — CROSS-LOCK view)

*Charge: harden/challenge/refine every family against the failure-mode checklist (vacuous
convergence, spec-cites-itself, gates-that-cannot-fail, the elegant-reduction trap, legacy aliases,
masked fallbacks, unverified gestalt, consumer-less substrate) + constitution violations +
prototype-evidence misuse — and, as critic 2, weight the CROSS-LOCK view. Every load-bearing fact
re-verified at HEAD `c93fc43143` (a descendant of the probe base `ce51c36fc` —
`git merge-base --is-ancestor ce51c36fc HEAD` = TRUE, so HEAD sees everything the probes could have
landed). I did not take the two pass-2 probes on their word; I ran the tree.*

---

## 0. WHAT I VERIFIED AT SOURCE — the load-bearing correction of pass 2

The pass-2 FAMILY-SPECS is built on two probes whose verdicts it conveys as settled MEASURED fact
("Option B RAN, byte-identical"; "PROTO-2 RAN, 1 passed → Open R DISSOLVES 1D"). **Neither probe is
in the tree, and the one that claims to close the structural gap does not close it on the real
grammar.** Five verified facts, three of them fatal to the pass-2 synthesis floor.

### 0a. [FATAL] NEITHER pass-2 probe is landed — both are SPEC_ONLY by XL-H's own definition
XL-H is explicit: "MEASURED = a landed, HEAD-reproducible probe; a `.patch.diff`-only artifact does
not count." At HEAD:
- `git diff --stat ce51c36fc HEAD -- crates/ir/src/passes/types/ …/type_desc.rs …/registry/struct.rs`
  = **empty**. The type pass is byte-untouched since before the probes ran.
- **PROTO-1 optB is NOT landed.** `assign` (`constraint/revise.rs:27`) at HEAD is the original
  3-line last-writer-wins carrier (`if slot==Some(&ty){false}else{*slot=Some(ty);true}`), **not**
  the "net-0 refinement-aware `match`" PROTO-1 says it delivered. Delivered as `PROTO-1-optB.patch`.
- **PROTO-2 is NOT landed.** `constraint/shape.rs` **absent**; `TypeShapeAnalysis` **zero grep
  hits**; `crates/core/tests/proto2_row_shape.rs` **absent**; `TypeDesc::Row` **absent**;
  `LayoutKind` is still `{Struct, TaggedEnum, UntaggedEnum, NewtypeWrapper}` — **no Row, no Map**
  (`registry/struct.rs:58`, verified). Delivered as `PROTO-2.patch`.

X8 (pass-1 directive) ordered "land or retire the four probes in-tree; land the additive `assign`
hardening regardless of carrier." **After two passes it is unmet: at HEAD nothing landed.** The
single carrier-INDEPENDENT deliverable both critics + the registry called "ships regardless of the
winner" — the additive `assign` clash arm — is still absent. Every "MEASURED" in the pass-2
FAMILY-SPECS is a worktree artifact; by the constitution's own bar (vii + XL-H) they are SPEC_ONLY.

### 0b. [FATAL] PROTO-2 does NOT close the masked fallback on the REAL grammar — the `key` role it requires does not exist there
PROTO-2's classifier derives `Row` iff the repeated element "carries a `->`-declared key role"
(`TypeDesc::Named("key")`). Its TYPED fixture **hand-authored a new rule** `keystr = … : key` to
supply it. **The real `grammar/json/json.bbnf` carries no such role:**
```
pair   = string, colon >> value ;              # key leg = `string`, typed  -> … : String  (NOT : key)
object = "{" >> (( pair << comma ? ) *)?w << "}" ;
array  = "[" >> (( value << comma ? ) *)?w << "]" ;
```
On real json.bbnf the `object` element is a repeated 2-leg tuple with **no declared `key` role** —
precisely PROTO-2's own BARE case, whose measured output is **`class(object) = Unknown` (honest
refusal), not Row.** Therefore the `json_templates/value.rs` masked fallback (present in the tree,
verified) **stays uncovered** on the exact grammar it exists for. PROTO-2 demonstrated Row-derivation
*only* on a grammar re-authored with an annotation the defect-grammar lacks — and self-admits the
**natural inline form of that annotation (`string -> key`) trips the typed-materialization-invariant**
(`lower/expression/wrap.rs:242`) and a bare `-> key` "collapses to `Span`." So the one form that
would derive Row violates a standing law; the form that compiles doesn't exist in the real grammar.
**The pass-2 structural probe closes nothing in-tree and nothing on the real grammar.**

### 0c. PROTO-2's grep-gate is a gate-that-cannot-fail-in-the-way-that-matters
The gate ("zero `{`/`}` byte, zero `has_rule(…)`") passes while the classifier relocates the
record-ness fact from a brace-byte to an author-declared `: key` annotation, then *defines* that
annotation as "not a hint … the honest locus of record-ness." The gate forbids two specific hint
shapes and is silent on a third (a semantic annotation added to the grammar), so it cannot detect
the goalpost-shift — the brief asked "derive from **body structure alone**," and structure-alone
(BARE) measured **Unknown**. This is exactly the definitional-gate failure my pass-1 predecessor
named ("a gate whose outcome is chosen by defining a word"), now realized.

### 0d. The doubly-dead-defect fact is REAL and holds (the one PROTO-1 corollary I confirm)
`grep '\.join(' crates/ir/src/passes/types/` = **empty**; `TypeDomain::join` (`domain.rs:66`) has
**zero call sites** — dead. The live carrier is `assign` (LWW, unreachable for concrete clashes;
Alt-join raises `HeterogeneousAltJoin`). PROTO-1's premise-demolition (1A's "running CSP unsound as
shipped" lever is void) is **sound and verified.** This is the pass's one genuine advance, and it
tilts the carrier seat toward 1B — but see §0e for why "resolved" overstates it.

### 0e. PROTO-1 is a one-sided race — Option A was NOT built (SPEC_ONLY half)
PROTO-1 states plainly: "**Option A NOT built** (cost-bounded, not stubbed) … sized from source."
So the carrier verdict "seat 1B" is **B-measured (on a worktree) vs A-estimated (from source
reading)**. The *direction* is defensible — 1A-as-carrier must delete ~101 LOC + rewire ~91 sites +
re-derive the refinement lattice inside union-find, for zero measured gain. But A's decisive
number — does a Sum/Row-grown union-find match parity at equal-or-lower derive wall-time — is
**asserted, never run.** Per Constitution vii, Option A's side of the "race" is SPEC_ONLY. The
carrier fork is **LEAN-1B, not CLOSED.**

*(Also verified, unchanged: `csp_strategy/` second `Csp` present (1B single-store = LOCK-1⊕LOCK-2
merge stands); `TypeAnalysis`+`from_ir` live at `backend/driver/analysis.rs:16/22` (1D name-collision
stands); `egraph.rs:237` `analysis_pending.push(dst)` present, `rebuild_pass_count` absent (1D
amendment reverted — 3-lock: `egraph/rules/regex.rs` present); only `TypeDesc` has a TS consumer,
`type_desc_to_ts` `ts/projection.rs:11` (1A monotype + 1D `TypeENode` still have no TS mirror).)*

---

## 1. THE CROSS-LOCK CROSS-CUTTING FINDING — the structural axis is UNSOLVED in-tree, and it is a LOCK-3⊕LOCK-7 question, not a LOCK-1 one

The pass-2 synthesis floor asserts: "structural fidelity supplied **orthogonally** by the
engine-agnostic record-widening (Open R), not by the carrier choice" (§6), and PROTO-2 titles its
verdict "Open R **DISSOLVES** 1D." Both rest on the single conditional §5.4 hands to Brief 2 — *"IF
the classification can be derived by inference rather than a per-grammar hint."* **§0a/§0b show that
conditional resolved the wrong way in-tree:** the derivation is not landed, and on the real grammar
it returns `Unknown`. So §5.4 is the **elegant-reduction trap** in the flesh — a load-bearing step
("and then structure is derived engine-agnostically") that reads *and-then-the-hard-part*, and the
hard part is unconverged.

The honest cross-lock state: **`List ≠ Row` on real (un-annotated) grammar body structure is
derivable by NO family** — not 1D's e-graph (extraction ≠ inference; it too needs the `key` fact),
not Open R's widened CSP (returns Unknown), not the running pass (masks via `json_templates`). The
missing ingredient is identical for every engine: *who declares that `object` is a keyed record?*
That is a **LOCK-7 grammar-totality** question (author-declared `key` role — which then needs the
**LOCK-3** front-end wall GN-3.W3 fixed so the annotation survives lowering, per §0b's
typed-materialization-invariant trip) **plus a LOCK-3 record-widening** (`LayoutKind::Row`,
unbuilt). LOCK-1's engine choice is genuinely orthogonal to it — **but "orthogonal" cuts both ways:
neither PROTO-2's dissolution of 1D nor Open R's closure of the fallback is earned.** 1D is neither
rescued nor cleanly dissolved; it is stuck behind the same unbuilt inference every family shares.

This is the pass's largest blind spot: the FAMILY-SPECS writes the structural question as *settled
in Open R's favor* on the strength of a probe that, run at HEAD, settles nothing.

---

## 2. FAMILY 1A — UNIFICATION-FIRST · convergence **55%**

The HM(X) *architecture* (equality trunk + bounded residue + let-generalization) is banked consensus
and I do not re-litigate it. As a distinct *carrier* family, 1A **weakened** this pass: PROTO-1 (even
one-sided) plus the verified doubly-dead defect (§0d) removes its sole displacement justification and
puts the build-cost delta against it; and the isomorphism clause (iii) ranks its fused monotype last
(no TS mirror — verified `type_desc_to_ts` consumes only `TypeDesc`). Down 1 from 56: the carrier
tilt moved away, but the architecture is intact and Option A being unbuilt (§0e) blocks a larger cut.

Enumerated open gaps:
1. **[carrier, now leaning against 1A] the one measured race puts the seat on 1B (§0d/§0e).** 1A must
   delete ~101 LOC + rewire ~91 sites + re-invent the `BoxedEnum`-refinement lattice inside union-find
   to re-clear 69 live sites, for zero measured gain. Its justifying defect is void.
2. **[SPEC_ONLY] zero direct 1A prototype — every gate (G1–G7) asserted.** Termination/parity rides
   the *incumbent's* nominal fold (probe2), never a union-find re-seed's. Constitution vii.
3. **[cross-lock iii] no TS mirror shown.** A fused monotype must reach `type_desc_to_ts`; only
   `TypeDesc` has a shipping TS consumer (verified). Asserted "genre-standard," never demonstrated.
4. **[cross-lock] LOCK-3 sink named but lowering unshown.** `TypeAnalysis::from_ir` is now correctly
   cited, but no artifact shows a fused monotype lowering through it (the incumbent `TypeDesc` does).
5. **row discipline unmeasurable — reconfirmed at source.** Real json.bbnf's only `->` are scalar leaf
   projections (`-> f64`, `-> 0u8`, `: String`); **zero row-shaped `->`** exist to decide Leijen vs
   Gaster–Jones. Behind GN-3.W3.
6. **numeric-width residue UNMEASURED.** No pass-2 width fixture ran (PROTO-1 = carrier race, PROTO-2 =
   Row). "Defaulting dominates" is prior-art-plausible, unrun on this pass's instrument.

## 3. FAMILY 1B — ONE-CONSTRAINT-STORE · convergence **56%** · single-store thesis BLOCKED-candidate

The leading carrier candidate after PROTO-1, and iso-safe (keeps `TypeDesc` → both `from_ir` sinks +
`type_desc_to_ts`, verified). Up 2 from 54: the carrier race (one-sided) + the doubly-dead-defect
confirmation genuinely advance "harden the running CSP, delete nothing" as the cheapest sound spine.
**But the family's distinguishing store-shape work is entirely on paper across two passes, and its
one carrier-independent deliverable never landed.**

Enumerated open gaps:
1. **[FATAL, X8 unmet] the additive `assign` hardening is STILL NOT LANDED (§0a).** The one thing
   "ships regardless of carrier" is absent at HEAD after two passes; `assign` is the raw LWW incumbent.
   The clash arm's backward-safety (G-1B-5, `types_heterogeneous_alt` green) was measured only on a
   worktree patch — SPEC_ONLY by XL-H. Land it or the family has shipped nothing.
2. **[cross-lock, stands] single-store = LOCK-1⊕LOCK-2 merge.** `csp_strategy/` second `Csp` verified
   present; the literal one-heterogeneous-store is a GESTALT-B consolidation 1B cannot make alone.
   BLOCKED-candidate; bank the literal thesis, ship the federation (= HM(X), = 1A's stratification).
3. **[spec-cites-itself, uncured] the residue census was NOT re-run on a modeling instrument.** B3
   (directive 3) demanded a census over an instrument that MODELS overload/legality/decode-class AND
   expresses `List ≠ Row`. No such census ran in pass-2. "Residue ≈ 0 / decidable-core dominates" still
   rests on the pass-1 census reading its own coarse output over a pass with zero overload code (X4).
4. **[UNBUILT, two passes] B4 store-shape — the family's sharpest decision — never built either way.**
   Decode-class-depends-on-sum-variant: flat cell (unsound) vs per-variant `Sum` cells (sound) +
   the implication-constraint scheduling tooth. Still on paper. The representability claim that
   motivates `TypeDesc::Sum` is unexercised.
5. **product-lattice monotone convergence (G5) unproven.** The widened product domain's
   oscillation-freedom (R1) was asserted, not driven on a re-refining fixture.

## 4. FAMILY 1C — BIDIRECTIONAL-LOCAL · convergence **40%** · BANKED (reactivation-gated)

Flat. The fold (checking front + SCC seam + span field over the winning reconciler) is correct and
banked. No pass-2 evidence moved it; its make-or-break remains structurally unrunnable, and PROTO-2
*reconfirms* the substrate wall rather than lifting it.

Enumerated open gaps:
1. **[structurally unrunnable, stands] R-C2 make-or-break has no input.** Zero conflicting-`->` corpus;
   PROTO-2's typed fixture is about record classification, not diagnostic localization, and is not
   landed. 1C's distinct identity is unfalsifiable, not merely un-run.
2. **[reconfirmed by §0b] the `->` front-end thread is broken.** PROTO-2's discovery that the natural
   inline `(string -> key)` trips `lower/expression/wrap.rs:242` (typed-materialization-invariant) is
   direct evidence GN-3.W3 (the front-end `->` wall) is unbuilt — 1C's substrate. Its 40% is a ceiling
   until that wall falls.
3. **[consumer-less substrate, viii] the LSP bridge is unbuilt.** The spec's own edict — the diff MUST
   include `type_obligations → lsp_types::Diagnostic{range}` — has zero hits; `crates/lsp`+`crates/
   analysis` exist but carry no such bridge. Enriching obligations without a consumer fails the bar.
4. **[LOCK-3 field] sub-span ceiling.** `Project.ty` is one `String` (whole-expr span); a wrong
   ascription cannot underline the `T`. The quality ceiling on 1C's thesis is an IR field it lacks.

## 5. FAMILY 1D — TYPES-AS-ECLASSES · convergence **43%** · BANKED (structural-μ hook now UNPROVEN, not "dissolved")

The most-contested disposition, and PROTO-2 was its make-or-break. The result is **not** the clean
"dissolved" the FAMILY-SPECS records — it is a **null**: the probe that would settle 1D's structural
axis is unlanded (§0a) and does not close the real defect (§0b). Down 1 from 44: 1D's "extraction
works" evidence is now itself SPEC_ONLY (off-tree), and its make-or-break (Row *inference*) is still
unbuilt-in-tree — no better than pass 1, minus the credibility of the pass-2 attempt.

Enumerated open gaps:
1. **[the make-or-break, still UNBUILT] Row-inference from an un-annotated body.** PROTO-2 proves the
   NEGATIVE (structure-alone → Unknown) and its POSITIVE only on a re-authored grammar (§0b). 1D's
   `TypeENode` deriving `Row` from real json.bbnf's `object` body *without* the `key` role is unshown —
   1D faces the identical missing ingredient as every other engine.
2. **[not "dissolved"] Open R does not dissolve 1D in-tree, because Open R itself is unbuilt.**
   `TypeDesc::Row` / `LayoutKind::Row` absent at HEAD (verified); the "any engine populates it once
   widened" claim is unexercised. 1D is stuck, not superseded — a distinction the synthesis floor erases.
3. **[cross-lock] "zero new engine" FALSIFIED — 3-lock blast radius, un-measured.** `analysis_pending`
   guard present, `rebuild_pass_count` absent (reverted); `egraph/rules/regex.rs` present → fires on
   LOCK-2 grammar-tier + LOCK-5 regex-tier saturation. Cannot be settled in LOCK-1.
4. **[cross-lock vi/iii] name-collision + no TS path.** Net-new `TypeAnalysis` collides with the live
   `backend/driver/analysis.rs:16` struct; `TypeENode` has no `type_desc_to_ts` mirror. Ranks last on iso.
5. **[elegant-reduction, uncured] AC-sum + positional injectivity coexistence UNBUILT.** Injectivity is
   unsound for AC `Sum`; every probe stayed sound by separate e-graphs — tested *around*, never *through*.
6. **recursion-keep RETIRED (verified), cost-extraction IDLE (residue 0).** Named negatives stand.

---

## 6. CONSTITUTION & PROTOTYPE-EVIDENCE AUDIT (cross-lock weighting)

- **vii (concrete artifacts or SPEC_ONLY) — VIOLATED at the probe level (§0a).** Both pass-2 probes
  are `.patch`-only; the FAMILY-SPECS conveys their verdicts as MEASURED. By XL-H a `.patch`-only
  artifact "does not count." Every pass-2 "RAN/byte-identical/1 passed" is a worktree claim not
  HEAD-reproducible. **The two probes must be labelled SPEC_ONLY until landed, and the synthesis
  floor re-derived without treating "Open R dissolves 1D" and "seat 1B, byte-identical" as settled.**
- **i (grammar totality) — the masked fallback is NOT closed (§0b).** `json_templates/value.rs`
  present; PROTO-2 returns Unknown on the real grammar. The clause-i / no-workarounds /
  typed-materialization-invariant defect stands, un-closed, owned jointly with LOCK-7. The natural
  annotation form that would close it *trips* the typed-materialization-invariant — a second clause-i
  hazard the pass surfaced but did not resolve.
- **v (zero runtime) — asserted by all four, exercised by none.** PROTO-1's "empty runtime diff" is
  trivially empty (no `crates/runtime/` on the root path) and worktree-only; no family drove it on a
  landed change.
- **iii (rust/ts isomorphism) — decides the carrier and is under-weighted.** Only `TypeDesc` has a
  shipping TS consumer (verified). This is an independent cross-lock argument FOR 1B (and against 1A's
  monotype / 1D's `TypeENode`) beyond PROTO-1's build-cost — the FAMILY-SPECS flags it (X7) but does
  not fold it into the carrier lean.
- **vi (parsimony / no name-gate) — 1D `TypeAnalysis` name-collision stands (verified).**
- **ix (extant-tranche consciousness) — GN-3 §1.5 supersession still owed as a formal LOCK-7-signed
  entry**, not the inline "factually void" the specs use. XL-F(1) records it WORKING; unresolved.
- **viii (consumer-less substrate) — 1C's LSP bridge unbuilt (§4.3); 1B's clash obligation is
  parity-inert (codegen never reads `type_obligations`)** — the hardening's *consumer* is the LSP
  bridge that does not exist, so B1 without §4.3 enriches a stream nothing reads.
- **Gate-that-cannot-fail — PROTO-2's grep-gate (§0c)** and the carried-forward PROTO-1 residue gate
  (numeric-axis-blind, X4) both stand.

## 7. PASS-3 CROSS-LOCK DIRECTIVES (what must be built or adjudicated)

1. **LAND the two probes or mark them SPEC_ONLY (§0a, X8 second call).** Minimum: land the additive
   `assign` clash arm + G-1B-5 green **in-tree** (ships regardless of carrier). Until it is HEAD-
   reproducible, no carrier verdict is MEASURED.
2. **Build Option A of the carrier race (§0e).** The seat is LEAN-1B, not CLOSED; a Sum/Row-grown
   union-find must actually be run for derive wall-time + parity, or 1B wins by A's forfeit, not by
   measurement.
3. **[LOCK-3⊕LOCK-7, the real structural pivot] Re-pose Row-derivation on the REAL grammar (§0b/§1).**
   Land `TypeDesc::Row`/`LayoutKind::Row`; then decide who declares record-ness: (a) a `key`-role
   annotation authored into real json.bbnf — which requires GN-3.W3 to fix the typed-materialization-
   invariant trip so the annotation survives lowering; or (b) grammar-total inference. PROTO-2 proved
   structure-alone is insufficient (BARE → Unknown); the question is a LOCK-7 totality call, not an
   engine race. Only then is "Open R dissolves 1D" earned — or refuted.
4. **[within-lock] Build B4 both ways (§3.4)** — the family's sharpest store-shape decision, unbuilt
   across two passes.
5. **[LOCK-2/LOCK-5] Route the `union` amendment through the 3-tier saturation measurement**; land or
   retire in-tree. **[LOCK-6] Show a TS mirror for whichever record wins** before it is chosen.
6. **[LOCK-7] Formalize the GN-3 §1.5 supersession** with LOCK-7 sign-off (ix).

## 8. CONVERGENCE SUMMARY

| Family | Disposition | CRIT-2 | Δ vs pass-1 | Dominant cross-lock open gap |
|---|---|---:|---:|---|
| **1A** UNIFICATION-FIRST | ADVANCED (carrier cand. #1) | **55%** | −1 | carrier tilt moved AWAY (§0d/§0e); no direct prototype; no TS mirror; row unmeasurable (verified: zero row-shaped `->` in real corpus) |
| **1B** ONE-CONSTRAINT-STORE | ADVANCED (carrier cand. #2) | **56%** | +2 | leading + iso-safe, BUT the "ships-regardless" hardening never landed (X8 unmet); single-store = LOCK-1⊕LOCK-2 merge; census self-cites; B4 unbuilt |
| **1C** BIDIRECTIONAL-LOCAL | BANKED (reactivation-gated) | **40%** | 0 | make-or-break has no input; `->` front-end wall reconfirmed broken (§0b); LSP bridge unbuilt (viii) |
| **1D** TYPES-AS-ECLASSES | BANKED (structural-μ UNPROVEN) | **43%** | −1 | PROTO-2 neither landed nor closes the real fallback (§0b); make-or-break still unbuilt; 3-lock/name-collision/no-TS stand |

**Lock-wide (cross-lock).** The pass-2 architecture — HM(X) trunk + defaulting residue + 1C folded +
structure via engine-agnostic widening — is prior-art-consonant and probably right *as a target*.
But the two probes meant to convert it from probable to proven **did not land** (§0a), and the one
that claimed to settle the structural axis **returns `Unknown` on the real grammar** (§0b). So the
pass's headline conclusions are both unearned in-tree: the carrier seat is **LEAN-1B, not CLOSED**
(Option A unbuilt), and the structural axis is **UNSOLVED by every family** (`List ≠ Row` on
un-annotated real bodies is derivable by none — it is a LOCK-3 record-widening ⊕ LOCK-7 totality
call, not an engine choice). No family is convergence-eligible; none is cleanly retired or dissolved.
The type-of-record is not LOCK-1's to choose alone — and the structural fidelity the specs hand to
"Open R, orthogonally" is a cross-lock obligation that pass 2 surfaced but did not discharge.
