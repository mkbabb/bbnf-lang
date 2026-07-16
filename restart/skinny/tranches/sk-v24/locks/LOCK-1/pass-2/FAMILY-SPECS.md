# LOCK-1 — THE SOLVER SPINE · pass-2 · FAMILY-SPECS (synthesis)

*Type inference & solving for the bbnf grammar plane: `->` projections, document shapes,
sums/products, capacities, numeric widths, decode classes — inferred, solved, erased to zero
runtime cost, with grammar-sourced diagnostics. GN-3's three walls are the floor.*

One cogent spec per family while the routes remain incompatible, then the incompatibility map,
the BLOCKED-candidate register, and the two sharpest prototype briefs. Grounded against the live
tree this pass (§0), the five pass-2 research reports, and the pass-1 re-anchored fold (registry).

---

## 0. BINDING FACTS THIS PASS — cross-lock (conveyed) + live-tree (re-verified)

**Conveyed as settled (CROSS-LOCK + registry; not re-litigated inside the lock):**

- **XL-A — lineage inverts the carrier cost.** Skinny RETIRES as a tree; the greenfield workspace
  is the tree-of-record-to-be; **skinny@HEAD is the MEASUREMENT SUBSTRATE** only. **Root is the
  greenfield base.** Consequence for LOCK-1: the running CSP type pass (1B/1D substrate) sits on
  the surviving base; the dormant HM `TypeFacts` scaffold (1A/1C) sits on the **retiring** tree, so
  "activate the scaffold" is really "**port PROVE-08 into root + reconcile-with / displace the
  running CSP**." The fork cost tilts *against* 1A/1C, not for them.
- **XL-F / X2 — the real LOCK-3 sink is `TypeAnalysis::from_ir`** (`crates/core/src/backend/driver/
  analysis.rs:22`, struct at `:16`), plus a second `from_ir` at `rust/view/named_types.rs:100`.
  Whichever record wins **lowers through it**. The TS mirror is `type_desc_to_ts`
  (`backend/ts/projection.rs:11`); **only `TypeDesc` has a shipping TS consumer** (X7) — a fused
  monotype (1A) and a `TypeENode` (1D) have no demonstrated TS path and must show one.
- **X3 — 1B's *literal* single store is a LOCK-1⊕LOCK-2 merge.** Root already federates the type
  CSP (`passes/types/`) and the backend-shape CSP (`passes/csp_strategy/`) over the same vendored
  `csp-solver`. The one-heterogeneous-store thesis cannot be built inside LOCK-1 — a GESTALT-B call.
- **X4 — `List ≠ Row` is a LOCK-3 record-expressiveness fix, engine-agnostic.** `LayoutKind` has no
  Row (`registry/struct.rs:58` → {Struct, TaggedEnum, UntaggedEnum, NewtypeWrapper}); `TypeDesc` has
  no Sum/Row (`type_desc.rs`: Option/Vec/HeterogeneousAltJoin only, verified). Adding
  `LayoutKind::Row` / `TypeDesc::{Sum,Row}` lets **any** engine populate the distinction. Owned by
  LOCK-3 + LOCK-7; it dissolves 1D's raison d'être into a vocabulary widening.
- **X6 — 1D's `union` scheduling amendment is a 3-lock blast radius.** It fires on LOCK-2 grammar-tier
  and LOCK-5 regex-tier (`egraph/rules/regex.rs`) unions, not only type merges; needs LOCK-2/LOCK-5
  sign-off + a required saturation-perf measurement across all three tiers. Reverted at HEAD.
- **XL-H — base law.** Every measurement names its HEAD and base-pins its worktree; stale-base numbers
  are wrong-artifact by definition. MEASURED = a landed, HEAD-reproducible probe; a `.patch.diff`-only
  artifact does not count.

**Re-verified live this pass (the load-bearing correction of pass 2):**

- **`TypeDomain::join` (`constraint/domain.rs:66-80`) is DEAD CODE — zero call sites** (re-confirmed:
  `grep '\.join(' crates/ir/src/passes/types/` empty). It is non-commutative last-writer-wins, but
  *unreachable*. **Both research-A (its "equality counterexample the family fixes") and research-B
  (its B1 hardening target) aim their central lever at this dead arm.** The lever is void.
- **The LIVE equality carrier is `assign` (`constraint/revise.rs:27`, `*slot = Some(ty)` at `:32`)** —
  LWW under an uncapped monotone loop. **But the only reachable clash site raises**: two incompatible
  `->` arrows land as `ir.types[0] = HeterogeneousAltJoin([Bool,F64])` + one named obligation
  (`is_single_silently_chosen_ground_leaf = false`, twice-measured in run2). **The live path is SOUND.**
  The residual `assign` hazard is two incompatible *grounds* flip-flopping — **unreachable** under
  well-formed tree IR. So 1B's hardening is *latent-hazard closure*, not *defect repair*, and 1A's
  carrier-displacement justification ("the running CSP is unsound as shipped") is **doubly dead**.

**Consequence for the whole lock.** The architecture converged and is **BANKED as consensus**:
HM(X)/OutsideIn(X) — an equality trunk + a bounded residue post-solve + let-generalization (prior-art
§2A/§3; both critics). It is **not** re-litigated. The remaining live question is *narrower and
sharper than any charter frames it*: **which equality carrier seats the HM(X) trunk** — a union-find
re-seed that deletes running code (1A) or the running CSP hardened additively (1B) — a build-cost
fork, now that the "unsound" lever is dead; **and whether structural fidelity (`List ≠ Row`) is
orthogonal to that choice** via the engine-agnostic record-widening (Open R). Everything else folds.

---

## FAMILY 1A — UNIFICATION-FIRST · disposition: **ADVANCED** (carrier candidate #1) · floor 56%

**Architecture.** One Hindley–Milner inference in the generator (Algorithm J / union-find over
type-variable equivalence classes — the empirically-won core, prior-art §1.1). Union-find is the
decidable, principal, terminating **equality trunk**; the residue solver is a **bounded post-solve**
invoked only where unification is genuinely under-determined (numeric width, overload/decode-class).
Document shapes are row-polymorphic records+variants (Rémy/Leijen scoped labels — the settled
mechanism, §1.2), which dissolves GN-3's ad-hoc `Vec<Ty>` sum + R5 order-sensitivity *at the `unify`
arm*, not in a bridge. Rules are let-generalized schemes; recursion is nominal folding.

**Mechanism.** Monotype `τ ::= Var(u32) | Con(leaf) | Sum(⟨τ⟩) | Row{fields,tail} | Vec(τ) |
Option(τ) | Rule(id)`; `unify` grows the Robinson arms; `Sum` unifies order-insensitively; each rule
`generalize(infer(body))`, each `Ref` `instantiate`s fresh (the false-fusion guard — load-bearing,
the *only* family that carries polymorphism). Widths/overloads drain as obligations solved after the
structural skeleton is fixed (rustc-style late defaulting). **Generalization MUST use the level/rank
trick (okmij, prior-art §2.1), never env-scan**, or a 191-rule CSS grammar pays quadratically.

**Integration seams (port INTO root — XL-A).** Seed the carrier: PROVE-08 `hm.rs` union-find lands
verbatim (`prototypes/PROVE-08`), grown with Sum/Row/Vec/Option arms; **displace the live `assign`
carrier** (`revise.rs:27`) and the `HeterogeneousAltJoin` lift (`revise.rs:116`), NOT the dead
`domain.rs:66` join. Narrow the `csp-solver` call in `project_types` (`passes/types/mod.rs`) to the
width/overload residue. Lower zonked monotypes → `TypeDesc` through `TypeAnalysis::from_ir` (X2) +
`type_desc_to_ts` (X7). Widen the record with Open R (`TypeDesc::{Sum,Row}` + `LayoutKind::Row`).

**Gates.** (G1) union-find + generalization terminates and matches the running pass on JSON / Sheets /
CSS L4 at equal-or-lower derive wall-time — **race the running pass, not a strawman** (base-pinned,
XL-H). (G2) a free-tail `Row` for `pair*`-over-runtime-keys lowers all the way through `from_ir` +
`type_desc_to_ts` to emitted output (Open R at the *lowering* seam, not just inference — R-A2). (G3)
generalization prevents false fusion: a rule referenced at two incompatible sites types clean under
`instantiate`-per-`Ref` and clashes (located) under a monomorphic carrier. (G4) width post-solve
recovers PROVE-08's `U8Tag→U32` loss → `u8`; an ascription conflicting with a literal (`-> u8` on
`256`) is a *located* clash, not a silent widen (no-silent-epsilon, R-A4). (G5) `git diff crates/
runtime/` empty; the second type system deleted, not coexisted. (G6) ascription clash names the arrow
span (folds 1C). (G7) Sum canonicalization is order-insensitive (R5 dissolved at the `unify` arm).

**Open questions.** (O1 — the carrier-port cost, the family's quietest liability, now measurable)
1A's win requires **deleting running root code** and porting PROVE-08 into the surviving base; its
prior justification (running CSP unsound) is doubly dead. Cost = derive wall-time + LOC-deleted +
runtime-diff head-to-head vs the hardened-CSP alternative — **Brief 1.** (O2 — row discipline) Leijen
(scoped labels) vs Gaster–Jones (uniqueness/lacks-kind); principal-type-critical (Wand's trap), **but
unmeasurable — zero `->` corpus** (sequences behind GN-3.W3 / a hand-authored typed micro-fixture).
(O3 — the residue is a lattice, not a search) prior-art §1.4 + PROTO-1: overload/cost residue is
empirically ~0; ordered-lattice defaulting dominates; the CSP is over-engineering for widths. Decide
on the width micro-fixture *independently of the carrier* (Open X). (O4 — TS mirror) the fused
monotype must be shown reaching `type_desc_to_ts` via `TypeDesc`, else the two backends diverge (X7,
R-A5).

---

## FAMILY 1B — ONE-CONSTRAINT-STORE · disposition: **ADVANCED** (carrier candidate #2) · single-store thesis **BLOCKED-candidate** · floor 54%

**Architecture.** Unification is equality propagation inside a global store; local propagators
discharge the decidable core; a bounded post-solve resolves the genuinely under-determined residue
(width defaulting). **Uniquely, this is not a proposal — it is the running architecture on the
surviving base** (`passes/types/mod.rs:51` `project_types` → `csp.propagate()`), to be *hardened and
widened*, not invented. The store already produces `ir.types: Vec<(RuleId, TypeDesc)>` copied
verbatim by `TypeAnalysis::from_ir` — **zero new LOCK-3 seam if the record stays a `TypeDesc`.**

**Mechanism.** One `Csp<TypeDomain>`, one variable per type node, AC-3/monotone sweep to a lattice
fixed point (no search — the domain is a lattice singleton). Additive widening: the store variable
carries a product lattice over disjoint axes — structural equality (ground-or-refine), numeric width
(ordered lattice `⊥⊑u8⊑…⊑u64`, `join = max`), decode class (small LUB lattice), overload legality
(`BitsetDomain` pruned by `NotEqual`). Genuine under-determination survives on **exactly one axis** —
width — and that is the only thing the post-solve touches (matching prior-art §1.4: width is a
monotone least-fixed-point, FIRRTL-style, NOT a backtracking search).

**Integration seams (already wired on the surviving base).** **B1 additive hardening — target the
LIVE `assign` (`revise.rs:27`), NOT the dead `domain.rs:66` join.** On `Some(existing) != Some(ty)`
record `TypeObligation::TypeClash` and refuse the overwrite, reusing the existing `ObligationSink`
(`obligation.rs:182`, admitted by the vacuous `ThreadSafe` marker); the drain at `mod.rs:473` already
publishes it. Widen `TypeDomain` to the product lattice; add `TypeDesc::{Sum,Row}` beside
`HeterogeneousAltJoin` (Open R); keep the shipping backend consumers fed. Engine stays vendored —
additions are consumer-side (the `ThreadSafe` precedent), no csc411 sync-gate touch.

**Gates.** (G1) B1 hardening lands and the heterogeneous-alt corpus still solves — `ir.types`
byte-identical, clash surfaced as obligation, `git diff` over emitted runtime empty; **confirm
backward-safe** (`types_heterogeneous_alt` green — G-1B-5, unproven at pass-1). (G2) product-domain
census on an instrument that MODELS overload/legality/decode-class AND expresses `List ≠ Row` —
per-corpus decidable-core vs residue split, gate pre-registered or re-cut *in advance* (the pass-1
census read its own coarse output — spec-cites-itself). (G3) decode-class-depends-on-sum-variant built
BOTH ways: flat cell (unsound — collapses per-variant decode to a LUB) vs per-variant `Sum` cells
(sound); report which parity-passes (R3/B4). (G4) width via the two-mode seam (`propagate()` lattice
vs `solve_optimized` search) decided on the micro-fixture (Open X). (G5) monotone convergence proven
(the `Changed`-flag honesty drains the worklist — no oscillation under the product lattice).

**Open questions.** (O1 — the carrier seat, THE live fork) with the "unsound" lever dead,
**1B-as-running-CSP-hardened is a live candidate for the carrier seat itself**: it keeps both backend
consumers, has a sound live clash path, needs only additive hardening — no running code deleted. The
spec's pass-1 "federation IS 1A in a coat" may have it backwards; the cheapest sound spine may be *the
running CSP hardened*. Adjudicated by **Brief 1** (vs 1A's re-seed) on derive wall-time + LOC-deleted +
runtime-diff. (O2 — the single-store thesis is BLOCKED-candidate, X3) the *literal* one-heterogeneous-
store is a LOCK-1⊕LOCK-2 merge — a GESTALT-B consolidation 1B cannot make alone. Bank the literal
thesis; keep the federation; the *architecture* 1B ships is HM(X), same as 1A. (O3 — residue) same as
1A O3: ordered defaulting dominates; search reserved for the non-chain overload case that the corpus
does not exhibit.

---

## FAMILY 1C — BIDIRECTIONAL-LOCAL · disposition: **BANKED** (diagnostics discipline over the winning trunk) · reactivation-gated · floor 40%

**Architecture.** Not a fourth engine — a **strict superset of the winning trunk that degenerates to
synthesis where there are no annotations** (self-admitted, R-C2). Type checking is rule-local
bidirectional: a `->` with a declared type **checks** the body against it (inherited/top-down); a bare
`->` **synthesizes** (synthesized/bottom-up); global solving is confined to cross-rule seams. The
design center is the *diagnostic*: a checking failure localizes at the arrow the author wrote, where
synthesis-first (HM) blames a synthesized interior far from the annotation. It reuses PROVE-08's
`classify_arrow` (already a bidirectional mode-switch) and its `Unifier` as the reconciler → shared
mechanism, closest kin to whichever trunk wins.

**Mechanism (the named graft).** `infer_expr` becomes `synth_expr`; `check_expr(ExprId, &Expected)`
overlays it; `Project`/`FnDescriptor::Expr { return_type }` (`fn_descriptor.rs:54`) is the mode switch
(`return_type = Some(T)` ⇒ check, `None` ⇒ synth); `Ref` consults `RuleSig` in SCC order; **the
trunk's equality reconciler is shared** (equality-unify for non-numeric leaves; a subtype/coercion
join at the numeric seam — CHECK is *subsumption*, not `==`, so a lawful widening `int -> f64` is not
a false clash — R1, the sharpest 1C risk). The current CHECK-mode installation site is
`generate.rs:370` (today a *ground*, not a *check* — the annotation silently wins; installing the
compare-and-localize is the whole of CHECK mode).

**Integration seams (against ROOT, additive).** `TypeObligation` gains `span: Option<GrammarSpan>`
(default `None` — byte-stable) + a `ProjectionMismatch { arrow_span, synthesized, ascribed }` variant;
a `GrammarIR.map_spans: HashMap<NodeId, GrammarSpan>` side table (isomorphic to `node_facts`) carries
the arrow byte-range down from lowering — the frontend must **capture the `->` span** (the one
non-additive touch, isolated to parser→lower; verified absent at HEAD). **The consumer-less-substrate
edict (viii) bites: the diff MUST include an `analysis`-crate bridge** `type_obligations →
lsp_types::Diagnostic{range}` — zero hits today; enriching the stream without a consumer fails the bar.

**Gates.** (G1 — the make-or-break, R-C2) on an authored typed `->` micro-fixture (`bidir_width.bbnf`
in research-C §3 — build the fixture first), `check_expr`'s `ProjectionMismatch.arrow_span` equals the
`->` span where synthesis-first reports `HeterogeneousAltJoin{node}` at a span-less synthesized
interior — **two modes, two blame sites, side by side.** (G2) subsumption not equality: `int -> f64`
produces zero `ProjectionMismatch`. (G3) CHECK is diagnostics-only — the projected `TypeDesc` is
byte-identical to synthesis-first (obligation augments, never overrides); regen byte-identical. (G4)
recursive `->` grammar reaches the fixpoint with CHECK edges in bounded iterations (≤ lattice-height ×
var-count). (G5) `git diff` over emitter/generated output empty.

**Open questions.** (O1 — structurally unfalsifiable today) G1 needs a typed `->` corpus that **does
not yet exist** (twitter/citm/canada absent; in-tree grammars carry ~zero `->`); 1C's distinct
identity is a bet until GN-3.W3 or a hand-authored fixture lands. (O2 — reactivation) 1C reactivates
to ADVANCED only if G1 proves the layer earns a name; else it is a footnote on the trunk. (O3 —
sub-span ceiling, a LOCK-3 field) `Project.ty` is one `String` carrying whole-expr span; a wrong
ascription cannot underline the `T` — the quality ceiling on 1C's thesis is a sub-span the IR does not
carry. **Bank the `PassDiagnostic.span` add + the LSP bridge regardless of the carrier winner.**

---

## FAMILY 1D — TYPES-AS-ECLASSES · disposition: **BANKED** · recursion-keep + "zero new engine" + cost-extraction **RETIRED** · structural-μ hook **CONTESTED** (BLOCKED-candidate on the structural axis) · floor 44%

**Architecture.** The type lattice lives in an e-graph; unification = congruence closure
(`union`+`rebuild`); recursive sums are native cyclic e-classes (back-edge through a shared id — the
finite representation the `TypeDescInterner` cannot express, since interning a cyclic `TypeDesc`
diverges); projections canonicalize by saturation; extraction picks a representative by cost. It shares
the LOCK-2 engine and homes the *type-tier* Language.

**Mechanism (net-new, mirroring the grammar tier).** `crates/ir/src/egraph/type_tier/` with a
`TypeENode` (= `TypeDesc` with `Id` children, `#[derive(Language)]` verbatim) adding `Sum(Box<[Id]>)`,
`Row{fields,open}`, `RowField{key,ty}`, `Rec(Id)`; a `TypeShapeAnalysis` deriving record-vs-list from
**body structure, not brace bytes** (`Vec(inner)` where `inner` is a key-carrying 2-tuple ⇒ Record,
else List); positional injectivity + AC-sum canonicalization co-resident on one `Sum` under one
saturation.

**The disposition and why it is CONTESTED, not clean.**
- **Recursion-keep RETIRED (reconfirmed).** The running CSP is finite + terminating on JSON (2 knots),
  Sheets (20 mutual knots), CSS L4 (44 knots), zero overflow (probe2). 1D's native cyclicity beats only
  the unbuilt strawman.
- **"Zero new engine" FALSIFIED + reverted.** The injectivity hook needs the shared-engine `union`
  scheduling amendment — a **3-lock blast radius** (X6), un-measured on LOCK-2/LOCK-5 hot saturation.
- **Cost-extraction IDLE.** Overload/cost residue = 0 everywhere (X4-caveated); the extractor has
  nothing to rank.
- **The ONE surviving hook — structural `List ≠ Row` — is DOUBLY challenged.** (a) **engine-agnostic
  (X4):** the gap is absent because `LayoutKind`/`TypeDesc` have no Row/Sum; widening the *vocabulary*
  lets **any** engine populate it — 1D's advantage reduces to "1D + a widened `LayoutKind`," and the
  real missing primitive (the record can *hold* the distinction) is not an e-graph capability. (b)
  **the make-or-break is Row *inference*, not extraction — and it is UNBUILT.** Every probe proved
  extraction (handed a `Row` node → finite `Sum` with `List ≠ Row`); **none shows the type Language
  *derives* that `object = "{" pair* "}"` is a `Row` rather than a `List` WITHOUT the same semantic
  hint the bespoke `json_templates` template encodes by matching `{ }` vs `[ ]`.** If it needs the
  hint, 1D *relocates* the workaround into the type Language, it does not *close* it. (c) **specific,
  not general (probe2):** Sheets already emits structural μ where the body is Seq/Tuple; the collapse
  is confined to heterogeneous-Alt-of-compounds (JSON `value`).

**Integration seams.** `type_tier/{node,analysis,rules,lower}.rs` net-new (grammar tier `egraph/
node.rs` is the template); extraction emits `TypeDesc` (grown Sum/Row) into `ir.types`; lower through
`TypeAnalysis::from_ir` + `named_types.rs:100` — **but the net-new `TypeAnalysis` 1D proposes
name-collides with the shipping LOCK-3 driver struct** (X2, Constitution vi); no TS mirror exists (X7).

**Gates.** (G1 — the make-or-break, UNBUILT) does `TypeShapeAnalysis` derive `Row` from the
repeat-of-pairs body with **zero** `{`/`}` byte match and **zero** grammar-name predicate (grep-gate
the module)? — **Brief 2.** (G2) the `union` amendment routed through a 3-tier saturation measurement
(grammar+regex+type); land or retire in-tree. (G3) AC-sum canon + positional injectivity co-resident
on one `Sum` under one saturation (not two e-graphs): two orderings ⇒ one class (confluence),
same-tag/distinct-payload ⇒ one clash (soundness) — built *through*, not *around*. (G4) equirecursive
`value ⊇ object ⊇ value` builds a finite `Rec`, the typo cycle clashes-or-`Rec`s, never hangs. (G5)
determinism at 191-rule CSS under `FxHashMap` iteration (proven only on a 10-class toy). (G6) `git
diff crates/runtime/` empty.

**Open questions.** (O1 — the RETIRE/KEEP is architecture-contingent + cross-lock) it turns on
grammar-authoritative (a coarse `TypeDesc` is a wall → 1D closes it) vs codegen-distributed (the
template is the sanctioned home → 1D is cosmetic) — a LOCK-7/GESTALT-B call. (O2 — the masked fallback
is a real clause-i defect **independent of 1D's fate**) `json_templates/value.rs` hand-matches raw
`{ } [ ]` dispatched by a hard-coded per-grammar table — a `no-workarounds` /
`typed-materialization-invariant` breach owned jointly with LOCK-7, closed by Open R regardless of
engine. **Reactivate only if the structural-μ hook is *needed AND* 1D-specific** — which Brief 2
decides.

---

## 5. THE INCOMPATIBILITY MAP

The four families are **not** four coequal rivals. There is **one exclusive axis** (the equality
carrier, three mutually-exclusive members), **one banked consensus architecture** they all share,
**one compatible overlay** (1C), and **one orthogonal vocabulary widening** (Open R) that decouples
structural fidelity from the axis.

### 5.1 The banked consensus — HM(X) architecture (NOT a family, NOT re-litigated)

1A and 1B have collapsed onto **one architecture**: an equality trunk + a bounded residue post-solve
+ let-generalization (prior-art §2A/§3; both critics; the incompatibility map). Per the redirect rule
this is the redundant mechanism → **BANKED as the settled floor.** The redundancy is *architecture-
level, not build-level*, so neither carrier is force-banked.

### 5.2 The exclusive axis — the equality CARRIER (choose exactly one)

| | 1A union-find re-seed | 1B running-CSP hardened | 1D e-graph congruence |
|---|---|---|---|
| equality primitive | Robinson `unify` (near-linear) | `assign` monotone sweep, **+B1 clash arm** (live path already SOUND) | `union`+`rebuild`+injectivity |
| carrier store | union-find map | `Csp<TypeDomain>` (has both backend consumers) | e-class union-find + hashcons (net-new Language) |
| lineage / cost | **port INTO root, DELETE running code** | **additive, keeps running code** | root, net-new + 3-lock engine amendment |
| distinguishing claim | polymorphism (let-generalization) | already-wired, sound, cheapest diff | native equirecursive + AC-sum |

**Why they cannot coexist as "the spine":** each defines equality over a *different* variable store,
produces a *different* type-of-record, and one owner must hold the `Project` discard site. Running two
equality engines is the "two type systems" failure both research-A and research-B indict; the
greenfield ports **one** organ. **1A ⊥ 1B ⊥ 1D pairwise on the seat.**

- **1A ⊥ 1B — a build-cost fork, not an architecture fork.** Same HM(X) architecture; the live
  difference is the carrier: 1A **deletes** the running CSP and re-seeds union-find (displacement cost,
  on the *surviving* tree — XL-A inverts this against 1A); 1B **hardens** the running CSP additively
  (keeps both consumers). The prior tilt to 1A rested on a **doubly-dead defect** (the "unsound as
  shipped" lever is void). **Adjudicated by Brief 1** (derive wall-time + LOC-deleted + runtime-diff).
- **1A ⊥ 1D** — union-find vs congruence-closure. 1D's recursion win is RETIRED (incumbent terminates
  finitely); its cost-extraction IDLE (residue 0); its sole survivor (structural `List ≠ Row`) is
  **subsumed by Open R** (§5.4). One store owns `Project`; not both.
- **1B ⊥ 1D** — both root-lineage; both claimed the same residue; the census empties it for both. 1D
  adds a net-new Language + a 3-lock engine amendment over 1B's additive hardening for no dominant-
  judgment gain. They cannot co-own the `TypeDesc`→schema lowering.

### 5.3 The compatible overlay — 1C is NOT on the axis

**1C ∥ (1A | 1B | 1D):** a checking front + SCC seam + span field over an equality reconciler it does
not define (reuses PROVE-08's `Unifier`). It folds onto whichever carrier wins. Its only
incompatibility is degenerate: with ~zero `->` in the real corpus, **1C ≡ its host engine** until
typed grammars exist. The named graft: `infer_expr → synth_expr`, `check_expr` overlays, the trunk's
reconciler shared.

### 5.4 The orthogonal widening — Open R decouples structure from the axis

`List ≠ Row` is a **LOCK-3 vocabulary gap** (`LayoutKind::Row` / `TypeDesc::{Sum,Row}`), engine-
agnostic (X4). Adding it lets **any** carrier — 1A's union-find, 1B's hardened CSP, or 1D's e-graph —
populate the distinction and lower it through `TypeAnalysis::from_ir`, closing the masked
`json_templates` fallback. **This makes the entire engine axis (5.2) orthogonal to the structural
question that seemed to justify 1D** — *if* the classification can be **derived by inference** rather
than a per-grammar hint. That single conditional (**Brief 2**) is what remains to decide whether Open
R dissolves 1D entirely or 1D retains a genuinely-1D-specific inference capability.

### 5.5 The orthogonal residue seam — Open X (pluggable, carrier-independent)

Widths/overloads/decode-class route to a bounded post-solve that is **the same seam whichever carrier
wins** — an ordered defaulting lattice (prior-art §1.4: monotone least-fixed-point, FIRRTL-style) with
CSP-search as the fallback implementation only for the non-chain overload case the corpus does not
exhibit. `decision_csp.rs` is the existing two-mode (`propagate()` vs `solve_optimized`) proof-of-
compile. Decide defaulting-vs-search on the width micro-fixture **independently of the carrier fork.**

### 5.6 Open S — the cross-rule seam as one attribute-grammar discipline

All three carriers re-derive the cross-rule seam independently (1A: SCC generalize-after-fixpoint; 1B:
federation sequencing; 1C: SCC bounded fixpoint). Read as one discipline: SYNTH = synthesized
attribute, CHECK = inherited attribute, the cycle-break = a bounded circular AG with a finite-height
termination lattice (Knuth/Farrow/JastAdd). A *seam discipline*, not a fifth engine — a reading of the
running CSP, to be shown faithful, not built.

### 5.7 The cross-lock hook — Open R's Map-barrier decision is a LOCK-5 input (XL-C)

The Open R widening has a downstream face XL-C flags: LOCK-5's 5B functor is **conditionally
re-opened *only if* LOCK-1 abandons Map-barrier typing.** Making `List ≠ Row ≠ Map` a first-class
typed record (`TypeDesc::Row`, §5.4) *is* the "abandon Map-barrier" branch — it types *through* what
XL-C's regex axis currently keeps as an opaque `Regex(StringId)` / Map boundary. Keeping the Map
opaque (no first-class `Row`, the running masked fallback) is the "hold Map-barrier" branch, under
which 5B stays retired. **This is not LOCK-1's call to settle** — flag it to LOCK-5 / GESTALT-B as a
named dependency: whichever way Brief 2 resolves the Open-R widening, its verdict conditions 5B's
re-open. A first-class `Row` here is a lawful supersession input there, not a silent contradiction
(Constitution ix).

---

## 6. BLOCKED-CANDIDATE REGISTER + SYNTHESIS FLOOR

- **1A — not blocked. ADVANCED (carrier candidate #1).** Liability: the port cost — deletes running
  code on the surviving tree; its displacement justification is doubly dead (Brief 1 measures it).
- **1B — the LITERAL single heterogeneous store is BLOCKED-candidate** (X3: a LOCK-1⊕LOCK-2 store
  merge, a missing primitive of GESTALT-B character, not buildable in LOCK-1). **But 1B-as-hardened-
  running-CSP is NOT blocked** — it is the additive, sound, consumer-preserving carrier candidate #2.
  Bank the literal thesis; the architecture 1B ships IS HM(X).
- **1C — not blocked; not an engine family. BANKED** as the diagnostics/locality discipline over the
  winner; reactivates only if R-C2 (Brief-C, needs a typed `->` fixture) proves the layer earns a
  name. Bank the `PassDiagnostic.span` add + LSP bridge regardless.
- **1D — structural-μ hook is BLOCKED-candidate on the structural axis IF Row-inference needs a
  per-grammar hint** (Brief 2 decides; if the hint is required, the workaround relocates, not closes).
  recursion-keep + "zero new engine" + cost-extraction RETIRED as named negatives. The `union`
  amendment is a 3-lock GESTALT-B call. **BANKED.**

**Synthesis floor for pass-2 (measured, not tasted).** The spine is **an HM(X) equality trunk + an
ordered defaulting-lattice residue solver (Open X) + a bidirectional diagnostics overlay (1C folded)**,
seeded by **one** carrier — and structural fidelity supplied **orthogonally** by the engine-agnostic
record-widening (Open R), not by the carrier choice. Two live questions remain, both now runnable
against root, both mis-sized by every charter: (i) **which carrier seats the trunk** — union-find
re-seed (deletes running code) vs running-CSP hardened (additive) — a build-cost measurement, no
longer the void "unsound" defect; and (ii) **whether `List ≠ Row` is derivable by inference** (Open R
orthogonal, dissolving 1D) or needs a hint (1D-specific, relocated workaround). 1B's single-store and
1D's cost-extraction are evidence-retired.

---

## 7. THE TWO SHARPEST UNCERTAINTIES → prototype briefs

The pass-1 briefs (CSP-equality soundness; recursion parity) RAN as run2/probe2. These two adjudicate
the *surviving* fork against running root code and most advance the next pass.

**Brief 1 — THE CARRIER RACE (bbnf-worktree).** Seat the HM(X) trunk two ways on the greenfield base
and race them, base-pinned (XL-H). (A) **union-find re-seed:** port PROVE-08 `hm.rs` union-find into
`crates/ir/src/passes/types/`, grown with Sum/Row arms, displacing the live `assign` carrier
(`constraint/revise.rs:27`) and the `HeterogeneousAltJoin` lift. (B) **hardened running CSP:** land
the additive B1 clash arm on the LIVE `assign` (record `TypeObligation::TypeClash`, refuse the
overwrite — NOT the dead `domain.rs:66` join). Run both on `grammar/json/json.bbnf`,
`grammar/css/pretty.bbnf`, `grammar/google-sheets/google-sheets.bbnf`. **Pass/fail:** both must
terminate and produce **byte-identical `ir.types`**; report derive wall-time (median of N), net
LOC-delta (added − deleted), and `git diff crates/runtime/` (must be empty for both). WIN = lower
derive wall-time at equal-or-fewer net LOC with an empty runtime diff. This is the pass's single most
important unadjudicated question — the carrier seat — decided by build cost, not the void defect.

**Brief 2 — ROW-INFERENCE vs HINT, engine-agnostic (bbnf-worktree).** Add `TypeDesc::Row` +
`LayoutKind::Row` (X4). Build a structural analysis — a store propagator (1B form) OR a
`TypeShapeAnalysis` (1D form), the engine is deliberately not the variable — that classifies
record-vs-list from **body structure alone**: `object = "{" pair* "}"`, `pair = string ":" value`,
`array = "[" value* "]"` (delimiter bytes present but **unread**), on `grammar/json/json.bbnf`.
**Pass/fail:** `class(object).kind == Record` and `class(array).kind == List` with **zero** `{`/`}`
byte match and **zero** grammar-name predicate (grep-gate the module — no `has_rule("value")`, no
literal-brace match); then the derived `Row` lowers through `TypeAnalysis::from_ir` and closes the
`json_templates/value.rs` masked fallback. Report whether a per-grammar hint was required. If NO hint
→ structural fidelity is orthogonal to engine choice (Open R dissolves the whole engine axis's claim
on structure, and 1D's raison d'être with it). If a hint IS required → the workaround relocates into
the type layer → 1D's structural-μ hook is BLOCKED-candidate. Either verdict decisively moves the lock.
