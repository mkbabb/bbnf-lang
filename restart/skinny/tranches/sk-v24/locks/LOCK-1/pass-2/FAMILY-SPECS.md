# LOCK-1 — THE SOLVER SPINE · pass-2 FAMILY-SPECS (SYNTHESIS, Fable-tier)

*Distilled from research-{A,B,C,D,priorart} (pass-2), the REGISTRY pass-1/pass-2 folds,
CROSS-LOCK XL-A…XL-H, both on-disk critics, and the three pass-2 probes (PROTO-1 workspace
divorce, PROTO-2 Row-inference, PROTO-2-iso carrier-iso floor). Every path:line re-verified at
HEAD `06e70053f` (newer than the critics' pin `c43126422`; probe pin `ce51c36fc` is an ancestor —
all load-bearing facts re-hold). One cogent spec per family, then the incompatibility map, then
the two sharpest briefs.*

---

## §0 — THE SETTLED FLOOR AND THE VOID INSIDE IT (binding on every family)

**Consensus, banked as the *direction* (do not re-litigate the theory).** Prior-art §2/§4/§8 and
both critics converge: the field's settled answer is **HM(X)** — a decidable equality trunk
(union-find / congruence closure over ADT-row-sum constructors) + a **bounded, dedicated** residue
solver only where equality is undecidable (numeric widths, overloads/decode-class) + **let-
generalization** per rule for polymorphic-helper reuse. 1A is a verbatim restatement of HM(X)
(Odersky/Sulzmann/Wehr); 1C is its diagnostics-first bidirectional *front-end* (Dunfield &
Krishnaswami), not a rival engine; 1B is the *same* trunk written as one constraint vocabulary
(Sulzmann/Stuckey CLP(X)); 1D shares only the union-find *primitive* — its saturation is an
optimization tool (LOCK-2's), a liability for decidable unify. **The engine axis is settled; the
carrier and the residue are not.**

**The void the floor hides (X10 — the pass's deepest finding).** *No family instantiates the banked
floor at the polymorphism layer.* Verified at HEAD: `grep -rniE "generali|instantiat|polymorph|
scheme" crates/ir/src/passes/types/` = **1 hit, a module doc-comment** (the crate's *name*, "the
generalized csp-solver"). The running carrier (1B) is a monotone `csp.propagate()` sweep with
`RefConstraint` suspension — **zero `generalize`/`instantiate`**. 1A (which alone *would* carry
let-gen) is SPEC_ONLY, unbuilt. So the "1A and 1B collapsed onto one architecture" claim is
**vacuous at exactly the layer let-generalization exists to make sound (false-fusion soundness)**,
and the convergence floors (43–55%) are measured against an architecture no family ships. This is a
**redirect inward**, not a new family: the next pass must *build* the floor (a false-fusion fixture +
the let-gen graft) or stop banking the 1A/1B collapse. **→ Brief α.**

**Four binding cross-lock facts (CROSS-LOCK, conveyed as settled):**
- **XL-A / X1** — skinny **retires**; the greenfield/root workspace is the tree-of-record-to-be;
  skinny@HEAD is the **MEASUREMENT SUBSTRATE only**. Designs target greenfield; ports scope
  organ-level. The 1A/1C scaffold sits on the *retiring* tree → their mechanism is a PROVE-08 **port
  into root + reconcile-with/displace the running CSP**, not "activate the dormant `TypeFacts`."
- **XL-F / X2** — the real LOCK-1→LOCK-3 sink is **`TypeAnalysis::from_ir`**
  (`crates/core/src/backend/driver/analysis.rs:22`) **+ a second `from_ir`**
  (`rust/view/named_types.rs:100`). Whichever record wins lowers through **both**. 1D's proposed
  net-new `TypeAnalysis` is a **name-collision** (Constitution vi).
- **X9 (NEW) — the structural axis (`List ≠ Row`) is a GN-1 LOOM obligation, NOT a LOCK-1 brief.**
  PROTO-1 (RAN): Open R lands in `bbnf-ir`; the `has_rule("value")` mask lives in package `ir`
  (skinny), a **divorced workspace** referencing `TypeDesc` **0×**; the classifier physically cannot
  cross the boundary, and deleting the mask **regresses** (routes JSON to `emit_general_descent`,
  which has no Row/List classifier). Closing it = **codepath unification** (LOCK-7⊕LOCK-3) against
  the tree-of-record. **LOCK-1's residue is ONLY the engine-agnostic vocabulary widening
  `TypeDesc::Row`/`LayoutKind::Row` (X4, a LOCK-3 record-expressiveness fix any carrier consumes).**
  The old "land Row then delete `has_rule` and diff byte-identical" brief is **WITHDRAWN** as a
  gate-that-cannot-pass on the retiring substrate (correct it to a named GN-1 supersession,
  Constitution ix).
- **X11 (NEW) — two double-homes route to GESTALT-B.** (a) **Open X** (the residue defaulting-lattice)
  is the *same vendored `csp-solver = "0.1"`* LOCK-2's `csp_strategy/` claims as its decision driver
  (2C) — two federated `Csp<_>` stores, one crate; the residue seat is a GESTALT-B consolidation.
  (b) The **iso-carrier seat** is a **LOCK-6 verdict** (Constitution iii — "measured, not asserted");
  LOCK-1 banks the HEAD-real iso *facts* but cannot seat the carrier on iso alone.

**X8 (THIRD CALL, carrier-independent) — the one deliverable unmet after two passes.** The additive
raise-on-disagreement hardening of the equality carrier `assign`
(`crates/ir/src/passes/types/constraint/revise.rs:8`, still the raw 3-line LWW `if slot.as_ref() ==
Some(&ty) { false } else { *slot = Some(ty); true }` at HEAD) is owed *regardless of carrier winner*
and is STILL absent. Its only consumer (1C's LSP bridge) is also unbuilt → **name the joint wave.**
**→ Brief β.**

**Zero-runtime invariant (Constitution v).** The type plane is generation-time only; every
carrier/record wave shows `git diff -- crates/core/src/runtime/` **empty** (verified: no
`crates/runtime/` exists at this base; the runtime is `crates/core/src/runtime/`). Asserted by all
four families, exercised on a LANDED change by none — a named gap for all four.

---

## §1A — UNIFICATION-FIRST · ADVANCED · carrier candidate #1 (LEAN-AGAINST) · ~55%

**Architecture.** The type plane is a single directed unifier over one union-find store; the HM core
grows three principled axes — **nominal-fold recursion** (each `RuleId` is its own constructor
`Nom(RuleId)`; a `Ref` unifies against the rule's *name*, not its expansion → iso-recursive
termination, the `data` boundary of ML/Haskell), **row polymorphism** (Leijen scoped labels for
document shapes), **let-generalization per SCC** of the rule dependency graph (level/rank-based,
Rémy/Kiselyov, O(1) amortized). CSP is *not* the spine — a bounded post-solve only for the genuinely
undecidable residue (widths, overloads). This is the family that, alone, *would carry polymorphism* —
its one distinctive rightness against the void of §0.

**Mechanism (concrete).** Seed from PROVE-08's proven `Unifier` (`find`/`unify`/`bind`/`resolve`,
221 LOC, perf-neutral by VERIFY) and grow `Ty` to the full term algebra `Var | Con(TyCon) |
App(Nom, [Ty]) | Row(RowVar) | Sum([(Tag,Ty)]) | Width(WVar)`. `Con/Con` stays the honest clash;
`App/Sum` recurse structurally; `Row` unifies by scoped-label row unification. Equality carrier =
"seed nominals up front, run arrows/refs as unification constraints, resolve by `find`" — this
**displaces** the monotone fold at skinny `passes/src/lib.rs:204` and, in root, **displaces the
running lattice-CSP** (`project_types`). Row-without-hint falls out of *what the repeat body unifies
to* (static literal keys → closed `Row`; runtime-span keys → open `Row`/Map; single value → `List`).

**Integration seams.** Substrate to displace: skinny `passes/src/lib.rs:149/178-214/204/234-254`
(the `->` discard + the `Type` enum) — **on the retiring tree** (XL-A → port cost). Root carrier to
displace/reconcile: `crates/ir/src/passes/types/mod.rs` `project_types`. Sinks (XL-F): grow
`TypeDesc` (`crates/ir/src/types/type_desc.rs:13`) with `Row`/`Sum`; lower through
`TypeAnalysis::from_ir` (`driver/analysis.rs:22`) + `named_types.rs:100`; **TS mirror**
`type_desc_to_ts` (`ts/projection.rs:11`). SCC substrate already in-tree:
`csp_strategy/components.rs` `UnionFind` + `partition_by_call_graph` (order let-gen; do **not**
conflate with the type-variable store).

**Gates it carries.** (G1) nominal-fold unifier terminates + parity-matches the running CSP on
JSON/Sheets/CSS L4 at ≤ derive wall-time (baseline = the *running pass*, base-pinned per XL-H).
(G2) Row/Map/List by inference, `List ≠ Row ≠ Map` on one run. (G3) **let-gen prevents false fusion**
(a shared helper reused at two element types instantiates to two monotypes) — *this is the gate §0's
void makes mandatory and 1A alone carries*. (G4) width post-solve: defaulting-lattice vs csp-solver
search on a genuinely under-determined u8/u16/u32 `->` fixture. (G5) resolved `Row`/`Sum` reach the
driver sink + TS mirror. (G6) `git diff crates/…/runtime/` empty. (G7) `HeterogeneousAltJoin`
*retires* (not merely coexists) once Alt-of-records unifies to Sum/widened-Row.

**Exact open questions.**
- **G-1A-1 [carrier port cost, UNRUN].** "Activate the scaffold" inverts under XL-A into "port
  PROVE-08 into root + displace the running CSP" — deletes ~101 LOC, rewires ~91 `TypeDesc` sites,
  and must **re-invent the `BoxedEnum→structural` refinement lattice inside union-find** for zero
  measured gain. Option A never built; the parity race is SPEC_ONLY.
- **G-1A-2 [displacement lever DOUBLY DEAD, reconfirmed].** The justification "the running CSP is
  overwrite-unsound as shipped" is dead code: `TypeDomain::join` has **0 call sites** (the live
  carrier is `assign`), and the live clash path is *sound* (Alt-join raises `HeterogeneousAltJoin` +
  an obligation). 1A's tilt is **unearned**.
- **G-1A-3 [SHARPENED — categorical iso failure].** PROTO-2-iso (HEAD-real negative): a **fused
  monotype is not a `TypeDesc` value → it has NO `type_desc_to_ts` path at all**. Choosing 1A
  requires authoring a brand-new `*_to_ts` sink from scratch. **1A ranks LAST on iso** — and if iso
  is the dispositive axis (LOCK-6's call), 1A is **pre-eliminated categorically**, and building
  Option A measures a race whose loser is already out.
- **G-1A-4 [research-A §4 unproven].** No probe shows `HeterogeneousAltJoin` *retiring* under
  unification rather than *coexisting* with union-find's structural arm.
- **G-1A-5/6.** Width residue UNRUN on this pass's instrument; row discipline (Leijen vs
  Gaster-Jones, Wand's principal-type trap) **unmeasurable** — verified zero row-shaped `->` in the
  real corpus (json's arrows are all scalar leaf projections).

**Verdict.** Architecture endorsed and distinctively right on let-gen (§0). As a *carrier*:
LEAN-against — pre-eliminated on iso, unbuilt on build-cost, displacement lever dead. **Not
BLOCKED-candidate** (the mechanism is proven at leaf scale), but its carrier seat is contingent on
the axis choice going to build-cost, not iso.

---

## §1B — ONE-CONSTRAINT-STORE · ADVANCED · carrier candidate #2 (LEADING) · ~54% · single-store thesis BLOCKED-candidate

**Architecture.** Every judgment — structural equality, numeric width, decode class, overload
legality, `List ≠ Row` — is one constraint system; unification is equality propagation inside a
global store; local propagators discharge the decidable core; a bounded post-solve resolves only the
under-determined width residue. The engine is the **already-running** lattice-CSP
(`passes/types/constraint/*`, `TypeDomain: LatticeDomain`). This family's claim: the store **exists,
runs, and needs hardening + widening, not invention** — it is the only family on the surviving base,
and (PROTO-2-iso, Sum-half HEAD-real) the only carrier whose record **already crosses to TS**.

**Mechanism (concrete).** The store ranges over structural `TypeDesc` today; widen the *same*
variable to a **product lattice** over disjoint axes, each with its own join: structural
(ground-or-refine, unchanged), **width** (`⊥ ⊑ u8 ⊑ u16 ⊑ u32 ⊑ u64`, `join = max` — widening is
lawful, the principled form of the current LWW), **decode class** (small LUB lattice), **overload
legality** (`BitsetDomain` pruned by `NotEqual`/AC-3), **`List ≠ Row`** (`TypeDesc::{Row,Sum}` +
tag-inequality). B1 hardens the disagreement policy: on a *structural* concrete-vs-concrete clash,
refuse the overwrite and `sink.record(TypeObligation::TypeClash{…})` (reuse the existing
`ObligationSink`, drained at `mod.rs`); on the *width* axis take `max` and continue. **B4 (the
sharpest store-shape finding):** decode-class-depends-on-sum-variant is **unsound as a flat cell** —
the store must be **product-shaped per Sum variant** (`Sum[decode₀, decode₁,…]`), *and* the
variant→decode dependency must be an `ImplicationConstraint` in the same worklist (read at the
fixpoint, not mid-sweep — changed-flag discipline), never a per-revise tag read.

**Integration seams.** Carrier: `constraint/revise.rs:8` `assign` (the LWW to harden — **X8**).
Driver: `passes/types/mod.rs` `project_types` + `propagate()`. Domain to widen:
`constraint/domain.rs` `TypeDomain`. Obligation sink (reused for all clashes): `obligation.rs`;
`ThreadSafe` admittance `csp-solver/.../traits.rs:43`. Residue two-mode template already compiles:
`skinny/crates/passes/src/decision_csp.rs` (`propagate()` vs `solve_optimized(&SolveConfig)`). Sinks
+ TS mirror as §0. B4 Sum shape aligns with GN-3.W1 `DirectTypeKind::Sum`.

**Gates it carries.** (G1) **X8: land the refinement-aware clash arm** — refuse concrete-vs-concrete
but **permit `BoxedEnum→structural` refinement** (the naive refuse-all freezes 69 refinements,
measured); confirm `types_heterogeneous_alt` green + `git diff crates/…/runtime/` empty +
`ir.types` byte-identical. (G2) product-domain residue census on a **modeling** instrument
(overload/legality/decode-class present). (G3) **B4 both ways** (flat unsound cell vs per-variant Sum
cells). (G4) Open X two-mode seam on the width micro-fixture, decided independently of the carrier.
(G5) **false-fusion / let-gen** — 1B must show `RefConstraint` suspension is observationally
equivalent to instantiate-fresh-per-`Ref`, or graft let-gen into the monotone sweep (§0 void).
(G6) monotone convergence of the hardened store (no oscillation).

**Exact open questions.**
- **G-1B-1 [FATAL, X8 unmet ×2].** The additive `assign` hardening is STILL the raw LWW at HEAD
  (verified). The one carrier-independent deliverable is absent after two passes; every "byte-
  identical / N passed" is a worktree claim → SPEC_ONLY by XL-H. **Land it or the family has shipped
  nothing.**
- **G-1B-2 [SHARPEST, §0 void].** **1B does NOT ship the banked HM(X)** — zero let-generalization
  (verified). "One architecture" is unsound at the polymorphism layer; **no 1B gate tests false
  fusion**; the "net-0 cheapest additive diff" **omits an unbuilt let-gen graft** into the monotone
  `propagate()` sweep. The leading carrier's polymorphic soundness is *untested*. **→ Brief α.**
- **G-1B-3 [consumer-less substrate, viii].** B1's `TypeClash` obligation is **parity-inert** —
  codegen reads `ir.types` + `struct_registry`, never `type_obligations`. Its only consumer is 1C's
  unbuilt LSP bridge. "Ships regardless of carrier" + "consumer-less until 1C" cannot both hold
  unscheduled — **the joint wave is unnamed.**
- **G-1B-4 [X3 double-home — single-store BLOCKED-candidate].** The literal one-heterogeneous-store =
  a **LOCK-1⊕LOCK-2 merge** (verified: `Csp<TypeDomain>` and `Csp<StrategyDomain>` over one
  `csp-solver`). Cannot be built inside LOCK-1 → **bank the literal thesis, ship the federation**
  (which *is* HM(X) = 1A's architecture). Once banked, 1B's residual distinctiveness ("cheapest
  diff") is **contingent on G-1B-2**.
- **G-1B-5/6.** Residue census (G2) + B4 (G3) unrun/unbuilt across two passes; the "residue ≈ 0"
  claim reads its own coarse output over a corpus with **zero overload code** (spec-cites-itself).

**Verdict.** LEADING carrier — iso-safe (Sum-crossing HEAD-real), cheapest additive diff, runs on
the surviving base. **The single-store thesis is BLOCKED-candidate** (a cross-lock merge, GESTALT-B).
Its "cheapest diff" advantage survives *only if* let-gen grafts additively (Brief α); otherwise it
ships the same HM(X) as 1A and the fork collapses to build-cost + iso, both LEAN-1B.

---

## §1C — BIDIRECTIONAL-LOCAL · BANKED (diagnostics discipline, reactivation-gated) · ~40%

**Architecture.** Not a fourth engine — a **checking front + span field + SCC seam over the winning
trunk** (1A or hardened-1B). Bidirectional typing (Dunfield & Krishnaswami): `synth` where nothing
is declared, `check` at every `->` ascription. The thesis is *localization*: a declared projection
the body cannot satisfy must name the **`->` span**, never a synthesized interior. It degenerates to
the host engine's synthesis on zero-annotation grammars (self-admitted strict superset of 1A) →
banked as the winner's diagnostics-and-locality discipline.

**Mechanism.** Two judgments over the existing IR: structural nodes synthesize as today; a
`Map`/`Project` node with a declared return type switches its body to **check** mode against that
type; on clash emit `ProjectionMismatch{span: <-> span>, declared, synthesized}` (primary = `->`
span, secondary = offending interior). The check arm is **absent today**: the `->` return type is
taken *without verification* (`generate.rs:369-399`, "falling back to Span"; skinny `:204` discards
`target`/`ty`). Cross-rule = the only global step: a declared rule type is a **synthesized
attribute** (SCC-free); only undeclared mutual recursion enters the existing cycle-break fixpoint.
Reuses PROVE-08's `classify_arrow` (ascription-checks / bare-synthesizes) as the mode seed.

**Integration seams.** The one net-new datum: a **span on the obligation record** (`obligation.rs`,
today span-less: `ref_node`/`target_rule`/`structural_type` only), sourced from the failing node's
`NodeId → RuleMeta.source_span` (`rule.rs:29`). Check arm at `generate.rs:369-399`. Skinny mirror:
populate the already-declared-but-empty `TypeObligation.span` (`skinny/passes/src/lib.rs:138-144`,
the standing Y.13 consumer-less-substrate violation). Consumer: a `type_obligations →
lsp_types::Diagnostic{range}` bridge — **the same bridge 1B's B1 clash arm needs** (name the joint
wave). Payload is `TypeDesc` + span only → TS-lowerable by construction (`type_desc_to_ts` covers
every variant).

**Gates it carries.** (R-C2, make-or-break) on a hand-authored declared/synthesized-**mismatch**
fixture (`frac = digits "." digits -> Ratio`, body synthesizes two-Span `Seq`, `Ratio` declared
scalar): synthesis-first reports at a synthesized interior `NodeId`; check reports at the **`->`
span**; print both side by side. If check does not move the span, the thesis is refuted and 1C
retires to synthesis-first. (C.2) additive `PassDiagnostic.span` landing, corpus green, runtime diff
empty. (Record-only census) count current declared-`->` mismatches per corpus before any fail-closed
flip.

**Exact open questions.**
- **G-1C-1 [make-or-break has NO INPUT, stands].** The greenfield json's 4 arrows are **all scalar
  leaf projections — none a mismatch**; no typed `->` corpus exists. 1C's distinct identity is
  **unfalsifiable** until a mismatch fixture is authored (blocks on GN-3.W3 or a hand-fixture).
- **G-1C-2 [GN-3.W3 wall].** The natural inline `(string -> key)` trips the typed-materialization
  invariant (`lower/expression/wrap.rs:242`); the rule-level form (`keystr = /…/ -> input : key`)
  clears it (PROTO-1 §4, RAN). The inline breakage caps 1C's addressable surface.
- **G-1C-3 [consumer-less, viii].** LSP bridge unbuilt — **the same unbuilt consumer as 1B's B1**;
  the joint wave is still UNNAMED (risks falling between carrier-selection and GESTALT-B).
- **G-1C-4 [sub-span ceiling, a LOCK-3 field].** `Project.ty` is one whole-expr `String` span — a
  wrong ascription cannot underline the `T`. The quality ceiling on the whole thesis is a sub-span
  the IR does not carry.

**Verdict.** Correctly BANKED as a compatible overlay on the winning trunk (the named graft:
`infer_expr → synth_expr`, `check_expr` overlays, shared reconciler). Reactivates to ADVANCED only
when R-C2 lands on a hand-authored mismatch fixture. The `PassDiagnostic.span` add + LSP bridge are
owed **regardless of carrier**, co-scheduled with 1B's B1.

---

## §1D — TYPES-AS-ECLASSES · BANKED · structural-μ UNPROVEN (not "dissolved") · ~43%

**Architecture.** The type lattice lives in the in-tree e-graph (`crates/egraph`, a hash-consed
congruence-closure engine); unification = `union`; substitution = `find`; the `Analysis` lattice
classifies record-vs-list by inference (`KeyShape`); AC-`Sum` canonicalizes by sort-at-construction;
projections canonicalize by saturation; extraction picks types by cost. The strong claim: the
e-graph **is** an HM engine, so re-authoring a union-find HM core is redundant (delete PROVE-08's
`hm.rs`, do not port).

**Mechanism + what the probes settled.** Three legs of the pass-1 thesis are **RETIRED as named
negatives**: recursion-keep (the running CSP is finite+terminating on JSON/Sheets/CSS L4, probe2 —
1D's native cyclicity beats only a strawman), "zero new engine" (FALSIFIED — the `union` scheduling
amendment is reverted at HEAD and is a **3-lock blast radius**), cost-extraction (IDLE — overload
residue = 0). The **one surviving hook** is structural `List ≠ Row` via a `KeyShape` `Analysis`
lattice deriving `Row` from a `pair*` body without a `{}/[]` byte read.

**The surviving hook is UNPROVEN, and its home is not LOCK-1.** PROTO-2 ("Open R DISSOLVES 1D",
DISSOLVE) and PROTO-1 ("BLOCKED") are **flatly contradictory verdicts on the same question**; the
tree adjudicates for **PROTO-1** (PROTO-2's own `git diff skinny/` = empty — it never touched, and
*could not* touch, the mask across the workspace divorce). PROTO-2 further showed that on the **real**
json `object` body the classifier returns **`Unknown`** — deriving `Row` requires a **`->`-declared
`key` role** (an engine-agnostic type fact), which is exactly the ingredient the e-graph does *not*
supply for free. **Extraction ≠ inference:** every probe proved 1D's *extraction* (handed a `Row`
node → `List ≠ Row`), none proved 1D *derives* the classification from an un-annotated body. The
missing primitive — recovering record-ness from body structure alone — is **of the same difficulty
as the original problem**, and it is engine-agnostic (the `key` role, once declared, any engine
reads). **→ structural-μ hook is BLOCKED-candidate on the structural axis, re-homed to the GN-1 loom
(X9).**

**Integration seams / cross-lock liabilities.** `crates/egraph/src/{egraph.rs,analysis.rs,
language.rs,scheduler.rs,csp_scheduler.rs}`; `egraph-derive`. **Name-collision:** a net-new
`TypeAnalysis` collides with the live `driver/analysis.rs` struct (Constitution vi). **No TS path:**
a `TypeENode` is not a `TypeDesc` → no `type_desc_to_ts` mirror (ranks last on iso with 1A). **3-lock
blast radius:** the `union` amendment fires on LOCK-2 grammar-tier (`egraph/{node,build_egraph,
rules}`) **and** LOCK-5 regex-tier (`egraph/rules/regex.rs`, verified present) — a GESTALT-B/LOCK-2/
LOCK-5 saturation-perf call, never a free LOCK-1 hook.

**Gates it carries (only if reactivated by the GN-1 loom).** (D1) `TypeENode` DERIVES `Row` from an
un-annotated `object` body WITHOUT a per-grammar hint — the true make-or-break, UNBUILT (measured
`Unknown` on the real body). (D3) AC-`Sum` (order-insensitive, sort-at-construction) ∥ positional
`Tuple` (order-sensitive) co-resident under ONE saturation — tested *around* (separate e-graphs),
never *through*. (D-union) 3-tier saturation-perf measurement.

**Verdict.** BANKED; three legs RETIRED; the structural-μ hook is **UNPROVEN and re-homed to GN-1**
(LOCK-7⊕LOCK-3 codepath unification), not a LOCK-1 recursion race. **LOCK-1's residue from 1D is
exactly the engine-agnostic vocabulary `TypeDesc::Row`/`LayoutKind::Row` (X4)** — which *any* carrier
populates, dissolving 1D's sole advantage into an IR-expressiveness fix. Reactivate only if the GN-1
loom proves the structural hook NEEDED **and** e-graph-specific — which a recursion race cannot decide.

---

## §2 — THE INCOMPATIBILITY MAP (which families cannot coexist, and precisely why)

**The axis structure.** The four families do **not** partition into four rival engines. They
collapse to **one settled architecture (HM(X), §0) + a two-way CARRIER fork + one compatible overlay
+ one banked engine whose residue is vocabulary**. Incompatibility lives almost entirely on the
*carrier* seat.

**INC-1 · 1A ⊥ 1B — THE CARRIER FORK (genuine, mutually exclusive, the pass's live fork).** Both
instantiate the *same* HM(X) architecture (§0), so they are **not** architecturally incompatible —
they are incompatible **as the equality carrier**, because the type pass has exactly one carrier
seat. 1A **re-seeds a union-find** (deletes the running lattice-CSP ~101 LOC + rewires ~91 sites +
re-invents the refinement lattice); 1B **hardens the running CSP** (additive, keeps both backend
consumers). You cannot ship both carriers. Decided on **two axes, both currently LEAN-1B**:
(i) **build-cost** — 1B additive net-0 vs 1A deletes+re-invents (LEAN-1B); (ii) **iso** — 1B's
`TypeDesc` crosses `type_desc_to_ts` HEAD-real, 1A's fused monotype **categorically cannot**
(pre-eliminated). *Caveat that keeps the fork open:* 1B's build-cost win is **contingent on let-gen
grafting additively** (§0 void / G-1B-2). If let-gen requires the union-find re-seed, 1A's cost
re-inflates 1B's and the fork re-opens on build-cost. **This single contingency (Brief α) is what
keeps INC-1 from being CLOSED-for-1B.** The iso seat routes to **LOCK-6/GESTALT-B** to render
(Constitution iii).

**INC-2 · 1D-as-engine ⊥ {1A, 1B} — carrier incompatible; 1D-as-vocabulary compatible.** 1D-as-the-
engine wants to **delete the union-find HM core** and make e-graph saturation THE carrier. This is
incompatible with *both* 1A and 1B as the carrier: it name-collides with the live `TypeAnalysis`
(vi), has **no TS mirror** (iii), and its `union` amendment is a 3-lock blast radius (cross-lock,
cannot be seated in LOCK-1). Its saturation is a *liability* for decidable unify (prior-art §6:
non-termination without a schedule, NP-hard extraction, rebuild overhead with no rules to fire).
**But 1D's *residue* — the `TypeDesc::Row`/`LayoutKind::Row` vocabulary — is fully compatible** with
whichever carrier wins: it is the engine-agnostic record-widening (X4) any carrier populates. So the
incompatibility is **1D-as-carrier vs the fork**, resolved by RETIRING 1D-as-engine and keeping only
its vocabulary. The structural-*inference* that would have justified 1D-as-engine is UNPROVEN and
re-homed to GN-1 (X9).

**INC-3 · 1C ∥ {winning trunk} — COMPATIBLE OVERLAY (not an incompatibility).** 1C is a strict
superset of the trunk's synthesis that degenerates to it on zero-annotation grammars. The named
graft is clean: `infer_expr → synth_expr`, `check_expr` overlays, **the trunk's equality reconciler
is shared** (equality-unify for non-numeric leaves; the resolved width consumed at the subsumption
site — 1C does *not* own a separate width seam). 1C composes with **either** carrier. The only
tension is a **shared dependency, not a conflict**: 1C's span-bearing obligation and 1B's `TypeClash`
obligation need the **same** unbuilt LSP-bridge consumer (viii) — they must land as **one named joint
wave**, or both are consumer-less substrate.

**INC-4 · The single-store (1B literal) ⊥ the intra-lock frame — a cross-lock merge, not a LOCK-1
choice.** 1B's *distinguishing* thesis (one heterogeneous store carrying equality + width + decode +
legality + backend-shape) is incompatible with the current two-federated-`Csp` reality (X3): it
requires **merging LOCK-1's type store with LOCK-2's `csp_strategy` store**. This is a **GESTALT-B
consolidation**, not adjudicable inside LOCK-1 → the literal single-store thesis is **BLOCKED-
candidate**; bank it, ship the federation (each store the HM(X) architecture over the shared
`csp-solver`). Likewise **Open X** (the residue defaulting-lattice) double-homes with 2C's decision
driver over the *same* vendored crate — the residue seat is a GESTALT-B call (X11a).

**Net coexistence table.**

| pairing | coexist? | precise reason |
|---|---|---|
| 1A ⊕ 1B (as carrier) | **NO** | one carrier seat; re-seed-union-find vs harden-running-CSP are mutually exclusive (INC-1) |
| 1A ⊕ 1C | **YES** | 1C overlays the 1A trunk; shared reconciler (INC-3) |
| 1B ⊕ 1C | **YES** | 1C overlays the hardened-CSP trunk; shared LSP-bridge consumer (INC-3) |
| 1D-engine ⊕ 1A/1B | **NO** | e-graph carrier name-collides + no TS path + 3-lock (INC-2) |
| 1D-vocabulary ⊕ any carrier | **YES** | `TypeDesc::Row` is engine-agnostic (X4); any carrier populates it (INC-2) |
| 1B-literal-single-store ⊕ LOCK-2 | **only at GESTALT-B** | requires a LOCK-1⊕LOCK-2 store merge (INC-4 / X3) |
| Open X residue ⊕ LOCK-2 2C | **only at GESTALT-B** | same vendored `csp-solver`, double-homed (X11a) |

---

## §3 — THE TWO SHARPEST BRIEFS (also returned via StructuredOutput)

The two uncertainties whose resolution most advances the next pass are (α) **whether the banked
HM(X) floor can be instantiated additively on the leading carrier** — the premise the entire carrier
fork and every convergence % rests on, and (β) **landing the one carrier-independent deliverable that
has been owed for two passes**, whose sub-question (backward-safety of the refinement-aware clash
arm) has a crisp numeric pass/fail. Both are `bbnf-worktree`, base-pinned to HEAD per XL-H,
runtime-diff-gated. Together they de-risk INC-1: α tests the shared-architecture premise; β ships the
deliverable both carriers and 1C depend on.

### Brief α — Does let-generalization graft additively into the running CSP, or does it force the union-find re-seed? (bbnf-worktree)

The banked HM(X) floor requires per-rule let-generalization (false-fusion soundness). The running
carrier (1B, `crates/ir/src/passes/types/`) has **zero** — verified: `grep -rniE
"generali|instantiat|polymorph|scheme" crates/ir/src/passes/types/` = 1 doc-comment hit. This single
question decides whether "1A and 1B are one architecture" is real and whether 1B's "cheapest additive
diff" survives. **Method:** (1) Author a false-fusion fixture — a shared helper rule reused at two
distinct element types (e.g. `pair = k v ; a = pair⁺ ; b = pair⁺` where the two uses must NOT
monomorphize-to-first-use), in `crates/core/tests/`. (2) Run it through the current `project_types`
and record whether `RefConstraint` suspension already yields per-`Ref` fresh instantiation (if so,
1B ships HM(X) for free — bank it) or fuses the two uses to one monotype (false fusion — the floor is
unbuilt). (3) If fused, attempt the minimal additive graft: a `generalize` at rule-solve + a fresh-
instantiate at each `RefConstraint` fire, into the monotone sweep; report LOC added, whether it
stays monotone (no oscillation), and `git diff crates/…/runtime/` empty. **Pass/fail:** the fixture's
two helper uses resolve to two distinct monotypes with the graft AND the existing corpus
(`project_types_json/sheets/css_l4`) stays byte-identical AND runtime diff empty → the graft is
additive, 1B ships the floor, the fork stays build-cost-LEAN-1B. If the graft cannot stay monotone /
additive, 1A's re-seed cost re-inflates 1B's and INC-1 re-opens.

### Brief β — Land the refinement-aware `assign` clash arm without freezing legal refinements (bbnf-worktree)

X8, unmet after two passes — the one carrier-independent deliverable. At HEAD
`crates/ir/src/passes/types/constraint/revise.rs:8` `assign` is the raw LWW `if slot.as_ref() ==
Some(&ty) { false } else { *slot = Some(ty); true }`. The naive "refuse all overwrites" freezes ~69
legal `BoxedEnum→structural` refinements (measured, per the registry). **Method:** replace the LWW
with a refinement-aware arm: on `Some(existing)` where `existing != ty`, if the transition is a
*refinement* (`BoxedEnum`/under-narrowed → structural, or `⊥`-ward) **permit and continue**; if it
is *concrete-vs-concrete* (two disagreeing grounds), **refuse the overwrite and
`sink.record(TypeObligation::TypeClash{ node, lhs: existing, rhs: ty })`** (reuse the existing
`ObligationSink`, drained at `mod.rs`). Run `cargo test -p bbnf --profile ax-iter --test
types_heterogeneous_alt` and the four `project_types_*` tests, base-pinned to current HEAD.
**Pass/fail:** `types_heterogeneous_alt` green AND all `project_types_*` byte-identical `ir.types`
(no refinement frozen — the 69 must still land) AND the clash-fixture emits exactly one
`TypeObligation::TypeClash` (non-empty obligation stream) AND `git diff -- crates/core/src/runtime/`
= 0 lines. Ships regardless of carrier; co-name the joint wave with 1C's `type_obligations →
lsp_types::Diagnostic{range}` bridge (the obligation's only consumer, viii).
