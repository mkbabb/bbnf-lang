# LOCK-1 — THE SOLVER SPINE · pass-2 · CRIT-2 (Adversarial Critic 2 of 2 — CROSS-LOCK view)

*Charter: harden/challenge/refine every family against the failure-mode checklist + constitution +
prototype-evidence discipline, weighting the CROSS-LOCK axis — where this lock contradicts or
double-homes work against the extant GN/PT codex and the other locks. Convergence % + exact
enumerated gaps per family. All path:line, grep, and tree-identity facts below independently re-run
at HEAD `3f2350f15af1cc886e2968be70e7bee610ae0e2a` (newer than FAMILY-SPECS' pin `06e70053f` and the
probe pin `ce51c36fc`, both confirmed ancestors). This report is CROSS-LOCK-weighted by design;
CRIT-1 carries the within-lock soundness read — I do not duplicate it, I stress the seams it accepts.*

---

## §CRIT-0 — THE HEADLINE CROSS-LOCK FINDING: `crates/ir` IS THE ARCHIVING LEGACY TREE, SO THE INC-1 BUILD-COST LEVER IS WRONG-TREE — AND NEITHER CRITIC NOR THE SYNTHESIS CAUGHT IT

The synthesis (FAMILY-SPECS §0 X1, §1B) and CRIT-1 (§CRIT-6, "close INC-1 for-1B") both rest the
carrier fork's surviving lever on **build-cost**, framed as: *1A = expensive port into "root" +
displace the running CSP; 1B = additive net-0 on the **surviving base**.* CRIT-1 escalates this to
"record INC-1 as CLOSED-for-1B-as-carrier." **This framing contradicts the GN codex it is bound to
honor (Constitution ix), and I verified the contradiction at source:**

- **GN-0.md line 17** — the archive act: *"then the legacy tree — the root `crates/`"* is committed
  to the historical archive. The **root `crates/`** — which contains `crates/ir` (1B's carrier,
  `passes/types/`) and `crates/core` (the `bbnf` crate) where **all three probes ran** (`-p bbnf`,
  `-p bbnf-ir`) — **is the legacy tree that archives at GN-0.**
- **GN-0.md line 105** — *"`skinny/`'s retained state is GN-1's port source; skinny archives at GN-1
  close."* The **tree-of-record** is the greenfield **GN-1 loom**, which **does not exist yet**
  (verified: `docs/tranches/sk-v24/` holds only `GN-*.md` specs; no `greenfield/` workspace dir), and
  it is seeded from **skinny organs**, not from `crates/ir`.
- **XL-A (binding)** agrees: skinny@HEAD is the **MEASUREMENT SUBSTRATE only**; designs target
  greenfield; "the legacy top-level `crates/` archives at GN-0."

**Consequence the pass has not absorbed.** After PROTO-1 mooted let-generalization (the fork's only
architectural differentiator), INC-1 rests on exactly **two** levers: build-cost and iso. **The
build-cost lever is measured on the wrong tree.** 1B's "additive net-0 diff on the surviving base" is
a diff against `crates/ir` — a tree scheduled for GN-0 archival. When the carrier actually lands in
the tree-of-record (greenfield, seeded from a **skinny** organ per XL-A/GN-1), **1B is *also* a
port**, not an additive edit; the real build-cost question is *which carrier organ ports most cleanly
into the loom* — an **UNMEASURED** question, because the loom does not exist. Under XL-H (base law:
wrong-base numbers are wrong-artifact-risk by definition), the INC-1 build-cost adjudication is
**wrong-TREE, which is strictly worse than stale-base.** CRIT-1's "CLOSED-for-1B" is therefore
**premature**: it closes the fork on a lever whose ground is the archiving legacy root. This does not
*flip* the fork to 1A — the **iso** lever (below) still LEANs 1B — but it **de-rates** the confidence
the pass is banking, and it **double-homes the carrier port as a GN-1 loom obligation** (the same
class as X9's structural axis), not a clean intra-LOCK-1 choice. **This is the single largest
cross-lock correction the pass owes, and it is NEW to this critic.**

---

## §CRIT-1 — PROTOTYPE-EVIDENCE DISCIPLINE: X8 IS WORKTREE, NOT TREE-OF-RECORD (independently re-verified)

I re-read `crates/ir/src/passes/types/constraint/revise.rs` at HEAD `3f2350f15`. `assign` is **still
the raw 3-line LWW** (`if slot.as_ref() == Some(&ty) { false } else { *slot = Some(ty); true }`) —
verbatim. **PROTO-2's "already fully staged in the index / landed and verified" is a worktree/index
artifact; nothing is in the tree of record.** PROTO-2's own header ("PASS — RAN_MEASURED... landed")
over-claims against Constitution vii + XL-H. Independently:

- **X8 patch blast radius (re-read the patch file):** touches ONLY `crates/ir/src/passes/types/
  {domain,revise,mod,obligation}.rs` + `crates/ir/tests/type_clash_arm.rs`. It **does not modify the
  vendored `csp-solver` crate** — it only `use`s `csp_solver::variable::Variable` and `csp_solver::Csp`.
  This is a genuine cross-lock *positive*: the clash arm is carrier-independent **and** does not
  disturb LOCK-2's shared use of `csp-solver`. Credit where due — but "carrier-independent + cross-lock
  clean" is a property of the *diff*, not evidence it is *shipped*.
- **The convergence ledger must not score X8 closed.** It is **proven-landable** on `ce51c36fc`, not
  landed at HEAD. CRIT-1 caught this too; I ratify and reinforce from the base-law angle.

PROTO-2-iso carries a related discipline slip: its Row→`Record<string,T>` crossing runs on
**uncommitted worktree vocabulary** — `TypeDesc::Row` is **absent at HEAD** (verified: `type_desc.rs`
enum has `BoxedEnum`, `HeterogeneousAltJoin`, scalars, `Option/Vec/Tuple/Enum/Named` — **no `Row`, no
`Sum`, no `Var`**). Only the **Sum arm (`HeterogeneousAltJoin`) is HEAD-real.** The synthesis flags
this honestly ("Sum-half HEAD-real") — but the *positive* iso demonstration for 1B is half-worktree,
and the Row half is not even carrier-distinctive (§CRIT-3 CL-5).

**Perf-claim hygiene (clean):** PROTO-1/PROTO-2/PROTO-2-iso make **no** perf claim — they are
functional/categorical gates. No SPEC_ONLY artifact anchors a perf number. 1A's port-cost parity/
wall-time race remains SPEC_ONLY and is correctly labeled. No refutation-law (iv) violation.

---

## §CRIT-2 — THE CROSS-LOCK LEDGER (contradictions & double-homes this lock owes)

| # | seam | verdict | evidence |
|---|---|---|---|
| **CL-1** | LOCK-1 build-cost lever ⊥ **GN-0 / XL-A** | **CONTRADICTION (Constitution ix), NEW** | `crates/ir` = archiving legacy root (GN-0.md:17); tree-of-record = greenfield loom, unbuilt, skinny-seeded. INC-1 build-cost is wrong-tree. |
| **CL-2** | Single-store (1B literal) + Open-X residue ⊥ **LOCK-2 2C** over `csp-solver` | double-home → **GESTALT-B**, firmed | `csp-solver = "0.1"` in BOTH `crates/core` + `crates/ir`; `Csp<TypeDomain>` (`passes/types/generate.rs`) **and** `Csp<StrategyDomain>` (`passes/csp_strategy/*`) in one crate; **GN-0.W2 re-homes csp-solver to `crates-infra/`** (GN-0.md:105). |
| **CL-3** | PROTO-1 monomorphism refutation-as-law ⊥ **LOCK-7** | latent contradiction, **UN-NAMED** | Closures non-functional at HEAD (`lower/expression/mod.rs:168` `.expect("closure: missing body child")`; tests `#[ignore]` AV.0.11). The `grammar-closures` memo names GrammarCall/@host/parametric rules. If LOCK-7 (7B/7C) revives parametric rules into the IR, false-fusion becomes reachable and 1B's "monomorphic by construction" **voids**. |
| **CL-4** | Carrier record ⊥ **double** `TypeAnalysis::from_ir` sink | ratified (X2/XL-F) | `driver/analysis.rs:22` **and** `rust/view/named_types.rs:100` both `from_ir`; any carrier lowers through BOTH; 1A/1D name-collide (vi). |
| **CL-5** | Iso seat = **LOCK-6 verdict** (Constitution iii) | ratified; Row-arm caveat | Sole `TypeDesc→TS` sink dispatches on `TypeDesc` variants → 1A monotype / 1D `TypeENode` have **no** path (HEAD-real elimination). But the *positive* Row crossing is worktree + `TypeDesc::Row` is an **X4 LOCK-3** fix any carrier consumes → not 1B-distinctive. |
| **CL-6** | 1D e-graph substrate ⊥ GN-0 re-home | minor (1D banked-dead) | `crates/egraph`/`egraph-derive` re-home to `crates-infra/` at GN-0.W2; top-level `bbnf-regex` archives (XL-C). 1D's 3-lock `union` blast radius fires against relocating trees. |

---

## §CRIT-3 — PER-FAMILY (cross-lock-weighted; I state only where I diverge from CRIT-1)

### 1A — UNIFICATION-FIRST · ~52% (CRIT-1 57%, synth 55%)
Architecture endorsed as HM(X) (PROVE-08, leaf-scale). I *lower* CRIT-1's number because CRIT-1's
"near-eliminated as carrier" leans on the very build-cost delta (1A delete-101-LOC-+-rewire-62-files
vs 1B additive net-0) that **CL-1 shows is wrong-tree** — 1A's port cost and 1B's additive-diff are
*both* measured on the archiving `crates/ir`; the tree-of-record port cost of *either* organ into the
greenfield loom is unmeasured. So 1A is not *near-eliminated on build-cost* — build-cost is simply
**unadjudicated at the correct tree.** 1A remains LEAN-against on **iso** (CL-5, HEAD-real: a fused
monotype has no `type_desc_to_ts` path — categorical). Disposition (bank-as-architecture, do-not-seat)
is clear; the carrier verdict is not cleanly closed.
- **G-1A-1 [port-cost UNRUN].** Option A never built; parity/wall-time race SPEC_ONLY. The one probe
  that would move 1A was not run.
- **G-1A-2 [displacement lever dead].** `TypeDomain::join` 0 call sites (live carrier = `assign`,
  sound). 1A's "running CSP overwrite-unsound" tilt is unearned.
- **G-1A-3 [distinctive advantage MOOTED].** G3/let-gen retired by PROTO-1 — no false fusion to prevent.
- **G-1A-4 [CL-1 wrong-tree].** 1A's port-cost inflation is measured against an archiving base; the
  greenfield-loom port cost of 1A vs 1B is the real INC-1 lever and is UNMEASURED.
- **G-1A-5 [`HeterogeneousAltJoin` retirement unproven].** No probe shows Join *retiring* under unify.
- **G-1A-6 [width/row unmeasured].** Zero row-shaped `->` in the real corpus; width residue unrun.

### 1B — ONE-CONSTRAINT-STORE · ~58% (CRIT-1 70%, synth 54%)
LEADING carrier — the lead is real on the **iso elimination of rivals** (CL-5, HEAD-real) and
"cheapest architecture" *relative to the archiving base*. I *sharply discount* CRIT-1's 70%: it
prices X8 as a near-close and the build-cost lever as decisive, both of which the cross-lock read
punctures.
- **G-1B-1 [X8 WORKTREE, not shipped].** Raw LWW at HEAD (re-verified). Proven-landable ≠ landed
  (Constitution vii, XL-H). Do not score closed.
- **G-1B-2 [consumer-less substrate, viii].** `TypeClash` + 1C's span-obligation both feed an
  **unbuilt** `type_obligations → lsp_types::Diagnostic{range}` bridge (grep: absent in `crates/lsp`/
  `crates/analysis`). Co-named, **unscheduled** — risks falling between carrier-selection and GESTALT-B.
- **G-1B-3 [gate-that-cannot-fail].** X8 byte-identity holds *because* the refuse arm never fires on
  any real grammar (fires only on the synthetic fixture) → the clash-detection deliverable has **zero
  real-corpus exercise**. Prospective hardening, not an exercised shipping feature.
- **G-1B-4 [X8 PARTIAL].** `is_fully_ground` treats `Enum`/`Named` as non-ground → two disagreeing
  `Named` map-results still LWW-overwrite silently. Whether ground-only is the complete X8 is open.
- **G-1B-5 [B3/B4 UNRUN — the DISTINGUISHING thesis untested, spec-cites-itself].** The "one store
  carries all five judgment axes" namesake is unexercised; "residue ≈ 0" reads its own coarse output
  over a **zero-overload corpus** — two passes running. What is proven is the structural-equality store
  it already was.
- **G-1B-6 [namesake = GESTALT-B, CL-2].** The literal single-store is a LOCK-1⊕LOCK-2 merge over a
  crate that GN-0.W2 physically relocates to `crates-infra/`. Caps within-LOCK-1 convergence <100%.
- **G-1B-7 [CL-1 wrong-tree build-cost].** 1B's primary post-PROTO-1 lever is measured on the archiving
  `crates/ir`; the tree-of-record port cost is unmeasured. INC-1 is not CLOSED-for-1B on this basis.
- **G-1B-8 [iso Row-arm worktree, CL-5].** Positive Row crossing rests on uncommitted `TypeDesc::Row`;
  only the Sum arm is HEAD-real; the Row half is X4 (LOCK-3), not 1B-distinctive.

### 1C — BIDIRECTIONAL-LOCAL · ~40% (CRIT-1 42%, synth 40%)
Correctly BANKED as compatible overlay. Essentially unmoved — the decisive experiment was not run.
- **G-1C-1 [R-C2 NOT RUN, no input].** Make-or-break localization proof unbuilt; json's 4 arrows are
  scalar leaf projections, none a mismatch. Distinct identity unfalsifiable until a hand fixture exists.
- **G-1C-2 [GN-3.W3 wall].** Inline `(string -> key)` trips typed-materialization (`wrap.rs`); only the
  rule-level form clears it. Caps addressable surface — a **LOCK-3⊕GN-3** dependency, not intra-1C.
- **G-1C-3 [consumer-less, shared with G-1B-2].** Same unbuilt LSP bridge; named, unscheduled.
- **G-1C-4 [sub-span ceiling, LOCK-3 field].** `Project.ty` is one whole-expr `String` span; a wrong
  ascription cannot underline the offending sub-term — the quality ceiling is a sub-span the IR does
  not carry (a LOCK-3 record-expressiveness gap, not intra-1C).

### 1D — TYPES-AS-ECLASSES · ~43% (CRIT-1 44%, synth 43%)
Correctly BANKED; engine thesis refuted (three legs retired); residue = vocabulary. Clear disposition
over a dead thesis.
- **G-1D-1 [D1 derivation UNBUILT, re-homed to GN-1].** Deriving `Row` from an un-annotated body
  measured `Unknown`; needs a `->`-declared `key` role (engine-agnostic). Extraction ≠ inference.
- **G-1D-2 [name-collision, vi — CL-4].** Net-new `TypeAnalysis` collides with `driver/analysis.rs:22`.
- **G-1D-3 [no TS path, iii — CL-5].** `TypeENode` is not `TypeDesc` → ranks last on iso with 1A.
- **G-1D-4 [3-lock blast radius, CL-6].** `union` amendment fires LOCK-2 grammar-tier + LOCK-5
  regex-tier, over substrate GN-0.W2 relocates to `crates-infra/`. Never a free LOCK-1 hook.
- **G-1D-5 [X4 residue still ABSENT at HEAD].** LOCK-1's *only* residue from 1D (`TypeDesc::Row`) is a
  real open IR fix (verified absent), not a done deal — a masked-fallback the ledger must keep RED.

---

## §CRIT-4 — CHECKLIST SWEEP (explicit)

- **Vacuous convergence:** FOUND ×2 — (a) the 43–55% floors were measured against HM(X)-with-let-gen, a
  phantom requirement PROTO-1 retires (concur CRIT-1); (b) NEW — 1B's build-cost convergence is measured
  against the archiving `crates/ir`, vacuous relative to the greenfield tree-of-record (CL-1).
- **Spec-cites-itself:** FOUND — 1B's "residue ≈ 0" reads its own coarse output over a zero-overload
  corpus (G-1B-5), unchanged across two passes.
- **Gate-that-cannot-fail:** FOUND — X8 byte-identity holds *because* the refuse arm never fires on real
  grammars (G-1B-3); clash detection has zero real-corpus exercise.
- **Elegant-reduction trap:** FOUND — "monomorphic by construction" (PROTO-1's load-bearing step) rests
  on beta-reduction that does not run: closure lowering `.expect`s child(4) and its tests are `#[ignore]`d
  (verified). CROSS-LOCK escalation (CL-3): the refutation-as-law is **LOCK-7-contingent** — G-CRIT-α
  (a CI monomorphization-invariant gate) must be **co-owned LOCK-1⊕LOCK-7**, not booked to LOCK-1 alone.
- **Legacy aliases / name-gates:** 1D `TypeAnalysis` collision (CL-4); `csp-solver = "0.1"` pinned in two
  workspace crates instantiating two federated stores (CL-2).
- **Masked fallbacks:** `has_rule("value")` mask correctly re-homed to GN-1 (X9); LOCK-1's residue
  `TypeDesc::Row` verified **still absent at HEAD** — X4 stays RED (G-1D-5), not a done deal.
- **Unverified gestalt:** the two GESTALT-B routings (single-store merge, Open-X residue) correctly
  deferred — and now *firmed*, not weakened, by GN-0.W2 co-homing `csp-solver` to `crates-infra/`.
- **Consumer-less substrate (viii):** FOUND — `TypeClash` (X8) + 1C's span-obligation both feed an
  unbuilt, unscheduled LSP bridge (G-1B-2 / G-1C-3).
- **Constitution v (zero-runtime):** exercised on X8 (`git diff crates/core/src/runtime/` = 0, patch
  touches only `crates/ir`) — but on a **worktree** change; asserted-not-shipped for the tree of record,
  asserted-not-exercised for 1A/1C/1D.
- **Prototype-evidence misuse:** X8 "landed" is worktree (raw LWW at HEAD — verified); PROTO-2-iso Row
  arm on uncommitted vocabulary. No perf claim is falsely anchored (probes are functional/categorical).

---

## §CRIT-5 — NEXT-PASS DIRECTIVES (cross-lock)

1. **Re-base INC-1 onto the tree of record (CL-1).** Strike "1B runs on the surviving base" — `crates/ir`
   archives at GN-0. Re-frame the build-cost lever as *greenfield-loom port cost of the 1B organ vs the
   1A organ*, and mark it **UNMEASURED until GN-1 exists**. Do **not** record INC-1 CLOSED-for-1B on
   build-cost; it survives LEAN-1B on **iso** alone (CL-5) — record that precisely.
2. **Co-own G-CRIT-α with LOCK-7 (CL-3).** PROTO-1's monomorphism refutation-as-law is contingent on
   LOCK-7 not reviving parametric rules into the IR. Route a named-supersession note to LOCK-7 (7B/7C):
   *if GrammarCall/@host survives lowering, false-fusion reopens and 1B's carrier soundness needs the
   union-find graft after all.* Fix the closure panic or ledger the surface frozen-monomorphic, CI-gated.
3. **Merge X8 into HEAD** (raw LWW → refinement-aware arm) and **schedule** the joint LSP wave; decide
   ground-only vs full X8 (G-1B-4). Until merged + consumed, X8 is proven-landable substrate, not shipped.
4. **Run the unrun distinguishing probes** — 1B's B3/B4 (product-lattice residue census + per-variant
   `Sum` store shape) on a *modeling* instrument with real overload/legality/decode-class; 1C's R-C2 on a
   hand-authored mismatch fixture. These move 1B past its equality-carrier ceiling and reactivate 1C.
5. **Land `TypeDesc::Row` (X4) into HEAD as a LOCK-3 record-expressiveness fix** so the iso Row crossing
   ceases to be worktree-only (G-1B-8) — an any-carrier fix, not a 1B distinctive.
