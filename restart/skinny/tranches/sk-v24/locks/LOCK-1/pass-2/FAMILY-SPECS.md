# LOCK-1 — THE SOLVER SPINE · pass-2 · FAMILY-SPECS (synthesis, hardened re-fold)

*Type inference & solving for the bbnf grammar plane: `->` projections, document shapes,
sums/products, capacities, numeric widths, decode classes — inferred, solved, erased to zero
runtime cost, with grammar-sourced diagnostics. GN-3's three walls are the floor.*

One cogent spec per family while the routes remain incompatible, then the incompatibility map, the
BLOCKED-candidate register, and the two sharpest prototype briefs. This is the **hardened re-fold**:
it consumes the pass-2 probes (PROTO-1 carrier race; PROTO-2 Row-inference) **at their honest
strength, not their headline**, corrected against the live tree re-verified this pass (§0) — because
both probes' own text, and both on-disk critics, contradict the headline verdicts the first synthesis
draft conveyed as settled.

---

## 0. BINDING FACTS THIS PASS — cross-lock (conveyed) + live-tree (re-verified at HEAD `c43126422`)

**Conveyed as settled (CROSS-LOCK + registry; not re-litigated inside the lock):**

- **XL-A — lineage inverts the carrier cost.** Skinny RETIRES as a tree; the greenfield workspace is
  the tree-of-record-to-be; **skinny@HEAD is the MEASUREMENT SUBSTRATE only.** **Root is the greenfield
  base.** The running CSP type pass (1B/1D substrate) sits on the surviving base; the dormant HM
  `TypeFacts` scaffold (1A/1C) sits on the **retiring** tree, so "activate the scaffold" is really
  "**port PROVE-08 into root + reconcile-with / displace the running CSP**." The fork cost tilts
  *against* 1A/1C.
- **XL-F / X2 — the real LOCK-3 sink is `TypeAnalysis::from_ir`** (`backend/driver/analysis.rs:22`,
  struct `:16`) + a second `from_ir` at `rust/view/named_types.rs:100`. **Only `TypeDesc` has a
  shipping TS consumer** (`type_desc_to_ts`, `backend/ts/projection.rs:11`) — X7. A fused monotype
  (1A) and a `TypeENode` (1D) have **no demonstrated TS path** and must show one before the record is
  chosen.
- **X3 — 1B's *literal* single store is a LOCK-1⊕LOCK-2 merge.** Root already federates the type CSP
  (`passes/types/`) and the backend-shape CSP (`passes/csp_strategy/`, second `Csp` verified present)
  over the same vendored `csp-solver`. The one-heterogeneous-store thesis cannot be built inside LOCK-1
  — a GESTALT-B call.
- **X4 — `List ≠ Row` is a LOCK-3 record-expressiveness fix, engine-agnostic.** `LayoutKind` has no
  Row (`registry/struct.rs:58` → {Struct, TaggedEnum, UntaggedEnum, NewtypeWrapper}, **re-verified**);
  `TypeDesc` has no Row/Sum (**re-verified: `grep Row type_desc.rs` empty at HEAD**). Widening the
  *vocabulary* lets any engine populate the distinction — **but widening alone does not derive it**
  (§0c).
- **X6 — 1D's `union` scheduling amendment is a 3-lock blast radius.** Fires on LOCK-2 grammar-tier and
  LOCK-5 regex-tier (`egraph/rules/regex.rs`) unions, not only type merges; needs LOCK-2/LOCK-5
  sign-off + a saturation-perf measurement across all three tiers. Reverted at HEAD.
- **XL-H — base law / MEASURED bar.** Every measurement names its HEAD and base-pins. **MEASURED = a
  landed, HEAD-reproducible probe; a `.patch`-only artifact does NOT count** (Constitution vii).

**Re-verified live this pass — the load-bearing correction the FIRST draft missed:**

- **(a) `TypeDomain::join` (`constraint/domain.rs:66`) is DEAD — zero call sites** (`grep '\.join('
  crates/ir/src/passes/types/` empty, re-confirmed). Non-commutative LWW, but unreachable. **Both
  research-A's "equality counterexample" and research-B's B1 target aimed at this dead arm** — the
  lever is void. This finding is **REAL and it is the pass's one genuine advance.**
- **(b) The LIVE carrier is `assign` (`constraint/revise.rs:27`)** — and **at HEAD it is still the raw
  3-line last-writer-wins** (`if slot==Some(&ty){false}else{*slot=Some(ty);true}`), **NOT** the
  refinement-aware `match` PROTO-1 says it delivered. The hardening is **unlanded.**
- **(c) `TypeDesc::Row`/`LayoutKind::Row` are ABSENT at HEAD; `constraint/shape.rs` does not exist;
  `has_rule("value")` mask is STILL present** (`skinny/crates/codegen/src/runtime_generator.rs:98`,
  re-verified). **Neither pass-2 probe landed.** By XL-H both are SPEC_ONLY.

**Consequence for the whole lock — the two headline claims are DOWNGRADED, honestly:**

1. **The architecture converged and is BANKED as consensus:** HM(X)/OutsideIn(X) — an equality trunk
   + a bounded residue post-solve + let-generalization (prior-art §2A/§3; both critics). **Not
   re-litigated.** This is the pass's real, durable output.
2. **The equality-carrier seat is LEAN-1B, NOT CLOSED.** PROTO-1 measured only Option B (hardened
   running CSP): byte-identical regen, net-0 LOC, parity wall-time (1.42 s vs 1.43 s), `G-1B-5` green —
   but *on a worktree patch, not landed*, and **Option A (union-find re-seed) was never built** ("sized
   from source"). A race with one runner is not a race. The seat *tilts* to 1B on cost asymmetry (1A
   deletes ~101 LOC + rewires ~91 sites + re-derives the refinement lattice for zero measured gain) +
   the iso floor (only `TypeDesc` crosses to TS) — but the decisive parity number for A is asserted,
   never run.
3. **The structural axis (`List ≠ Row`) is UNSOLVED in-tree — it is a LOCK-3⊕LOCK-7 question, not a
   LOCK-1 one.** PROTO-2 derived `Row` **only on a synthetic grammar it hand-authored** with a
   `-> … : key` role; on its **BARE** case — which mirrors the shipping `grammar/json/json.bbnf`
   (`pair = string, colon >> value`, **no key role**) — the classifier returns **`Unknown` (honest
   refusal)**, and the `has_rule("value")` mask **was never removed nor shown removable**. PROTO-2's
   own caveat concedes the record bit **requires the key role declared** — and the natural inline form
   (`string -> key`) **trips the typed-materialization-invariant** (`lower/expression/wrap.rs:242`,
   GN-3.W3 unbuilt). So "Open R DISSOLVES 1D" is **over-claimed**: on the real grammar every engine
   faces the *same* missing ingredient — *who declares that `object` is a keyed record?*

The remaining live questions are therefore **sharper than any charter frames them, and neither is
closed:** (i) whether porting union-find (1A) can match the running CSP (1B) at equal cost + cross to
TS — the carrier seat, LEAN-1B; and (ii) whether the structural distinction is derivable on the
**real** grammar at all, or is blocked behind a LOCK-7 totality decision (author-declared `key` role)
⊕ a LOCK-3 front-end fix (GN-3.W3) ⊕ the LOCK-3 vocabulary widening (`TypeDesc::Row`). Everything else
folds.

---

## FAMILY 1A — UNIFICATION-FIRST · disposition: **ADVANCED** (carrier candidate #1) · floor 55%

**Architecture.** One Hindley–Milner inference in the generator (Algorithm J / union-find over
type-variable equivalence classes — the empirically-won core, prior-art §1.1). Union-find is the
decidable, principal, terminating **equality trunk**; the residue solver is a **bounded post-solve**
invoked only where unification is genuinely under-determined (numeric width, overload/decode-class).
Document shapes are row-polymorphic records+variants (Rémy/Leijen scoped labels, §1.2), dissolving
GN-3's ad-hoc `Vec<Ty>` sum + R5 order-sensitivity *at the `unify` arm*. Rules are let-generalized
schemes; recursion is nominal folding.

**Mechanism.** Monotype `τ ::= Var(u32) | Con(leaf) | Sum(⟨τ⟩) | Row{fields,tail} | Vec(τ) |
Option(τ) | Rule(id)`; `unify` grows the Robinson arms; `Sum` unifies order-insensitively; each rule
`generalize(infer(body))`, each `Ref` `instantiate`s fresh (the false-fusion guard — the *only* family
that carries polymorphism). Widths/overloads drain as obligations solved after the structural skeleton
is fixed (rustc-style late defaulting). **Generalization MUST use the level/rank trick (okmij,
prior-art §2.1), never env-scan**, or a 191-rule CSS grammar pays quadratically.

**Integration seams (port INTO root — XL-A).** PROVE-08 `hm.rs` seeds the carrier — **but it supplies
only the flat 5-leaf unifier** (`Ty=Var|Con(TyCon)`, no structural constructor, no refinement notion);
the Sum/Row/Vec/Option arms + occurs-check are **new code with no proven seed** (PROTO-1, verified from
source). Displacing the live `assign` carrier deletes ~101 lines of corpus-tested code + rewires ~91
sites, and must **re-invent the `BoxedEnum→structural` refinement lattice inside union-find** to
re-clear the 69 live refinement sites PROTO-1 counted. Lower zonked monotypes → `TypeDesc` through
`TypeAnalysis::from_ir` (X2) + `type_desc_to_ts` (X7). Widen the record with Open R.

**Gates.** (G1) union-find + generalization terminates and matches the running pass byte-identically on
JSON / Sheets / CSS L4 at ≤ derive wall-time — **race the running pass, base-pinned; UNRUN (Option A
never built).** (G2) a free-tail `Row` lowers all the way through `from_ir` + `type_desc_to_ts` to
emitted output. (G3) generalization prevents false fusion. (G4) width post-solve recovers PROVE-08's
`U8Tag→U32` loss → `u8`; `-> u8` on `256` is a *located* clash, not a silent widen. (G5) `git diff
crates/runtime/` empty; the second type system deleted, not coexisted. (G6) ascription clash names the
arrow span (folds 1C). (G7) Sum canonicalization order-insensitive.

**Open questions.** (O1 — carrier port cost, now partly measured against it) 1A's win requires deleting
running code + porting PROVE-08 + re-deriving the refinement lattice; its prior justification (running
CSP unsound) is doubly dead. Cost = derive wall-time + LOC-delta + runtime-diff vs the hardened-CSP
alternative — **Brief-owed but LEAN-against (PROTO-1 sized it; Option A is the missing runner).** (O2 —
row discipline UNMEASURABLE) Leijen (scoped labels) vs Gaster–Jones; principal-type-critical (Wand's
trap), **zero row-shaped `->` in the real corpus** (json's arrows are all scalar leaf projections —
verified); behind GN-3.W3. (O3 — residue is a lattice, not a search) overload/cost residue empirically
~0; ordered-lattice defaulting dominates; **UNRUN on this pass's width instrument.** (O4 — TS mirror,
Constitution iii) the fused monotype must reach `type_desc_to_ts` via `TypeDesc`; **no differential
produced — the lock's largest un-probed surface, and it ranks 1A LAST on iso.**

---

## FAMILY 1B — ONE-CONSTRAINT-STORE · disposition: **ADVANCED** (carrier candidate #2 — LEADING) · single-store thesis **BLOCKED-candidate** · floor 56%

**Architecture.** Unification is equality propagation inside a global store; local propagators
discharge the decidable core; a bounded post-solve resolves the genuinely under-determined residue
(width defaulting). **Uniquely, this is not a proposal — it is the running architecture on the
surviving base** (`passes/types/mod.rs:51` `project_types` → `csp.propagate()`), to be *hardened and
widened*, not invented. It produces `ir.types: Vec<(RuleId, TypeDesc)>` copied verbatim by
`TypeAnalysis::from_ir` and crossing to TS via `type_desc_to_ts` — **zero new LOCK-3/LOCK-6 seam if the
record stays a `TypeDesc`** (the iso-safety that, per Constitution iii, independently favors 1B).

**Mechanism.** One `Csp<TypeDomain>`, one variable per type node, AC-3/monotone sweep to a lattice
fixed point (no search — the domain is a lattice singleton). Additive widening: the store variable
carries a product lattice over disjoint axes — structural equality (ground-or-refine), numeric width
(ordered lattice `⊥⊑u8⊑…⊑u64`, `join=max`), decode class (small LUB lattice), overload legality
(`BitsetDomain` pruned by `NotEqual`). Genuine under-determination survives on **exactly one axis** —
width — the only thing the post-solve touches (prior-art §1.4: monotone least-fixed-point, FIRRTL-style,
not a backtracking search).

**Integration seams (already wired on the surviving base).** **B1 additive hardening — the CORRECT,
refinement-aware form (PROTO-1 proved the naive form WRONG):** target the LIVE `assign`
(`revise.rs:27`), NOT the dead `domain.rs:66` join.
```rust
match slot.as_ref() {
    Some(e) if *e == ty => false,
    Some(e) if *e != TypeDesc::BoxedEnum => false, // B1: refuse concrete-vs-concrete clash + record TypeClash
    _ => { *slot = Some(ty); true }                // None or BoxedEnum refinement (the load-bearing carrier)
}
```
**The naive "refuse all disagreements" freezes all 69 `BoxedEnum→structural` refinements → regresses
generated output** (measured). The concrete-clash arm is **dormant (0 firings on json/css/sheets)** and
the `TypeObligation::TypeClash` it records is **parity-inert** (codegen reads `ir.types` +
`struct_registry`, never `type_obligations`) — so its *consumer* is the 1C LSP bridge, which must land
with it or B1 enriches a stream nothing reads (Constitution viii). Net-0 LOC; reuses the existing
`ObligationSink` (`obligation.rs:182`) drained at `mod.rs:473`; engine stays vendored (`ThreadSafe`
precedent — no csc411 sync-gate touch).

**Gates.** (G1) B1 hardening lands **in-tree** and the heterogeneous-alt corpus still solves —
`ir.types` byte-identical, `git diff` over emitted runtime empty, `types_heterogeneous_alt` green.
**STATUS: measured on a worktree patch, NOT landed at HEAD (X8 unmet after two passes — the single
carrier-INDEPENDENT deliverable is still absent).** (G2) product-domain census on an instrument that
MODELS overload/legality/decode-class AND expresses `List ≠ Row` — **UNRUN; the pass-1 census read its
own coarse output (spec-cites-itself).** (G3) decode-class-depends-on-sum-variant built BOTH ways: flat
cell (unsound) vs per-variant `Sum` cells (sound) — **UNBUILT across two passes; the family's sharpest
store-shape decision.** (G4) width via the two-mode seam. (G5) monotone convergence proven.

**Open questions.** (O1 — carrier seat, LEADING but not CLOSED) 1B-as-hardened-running-CSP is the
cheapest sound spine: keeps both backend consumers, sound live clash path, additive, iso-safe. It leads
1A on cost + iso — but "seat 1B" is not MEASURED until (a) the hardening lands in-tree and (b) Option A
is actually built and loses the race. (O2 — single-store thesis BLOCKED-candidate, X3) the *literal*
one-heterogeneous-store is a LOCK-1⊕LOCK-2 merge — bank it; keep the federation; **the architecture 1B
ships IS HM(X), identical to 1A — so 1B's only distinctiveness is "cheapest diff," not architecture.**
(O3 — residue) same as 1A O3, unrun.

---

## FAMILY 1C — BIDIRECTIONAL-LOCAL · disposition: **BANKED** (diagnostics discipline over the winning trunk) · reactivation-gated · floor 40%

**Architecture.** Not a fourth engine — a **strict superset of the winning trunk that degenerates to
synthesis where there are no annotations** (self-admitted, R-C2). Rule-local bidirectional checking: a
`->` with a declared type **checks** the body against it (inherited/top-down); a bare `->`
**synthesizes** (bottom-up); global solving is confined to cross-rule seams. The design center is the
*diagnostic*: a checking failure localizes at the arrow the author wrote, where synthesis-first (HM)
blames a synthesized interior far from the annotation. Reuses PROVE-08's `classify_arrow` (already a
bidirectional mode-switch) and its `Unifier` as the reconciler → closest kin to whichever trunk wins.

**Mechanism (the named graft).** `infer_expr → synth_expr`; `check_expr(ExprId, &Expected)` overlays
it; `Project`/`FnDescriptor::Expr { return_type }` is the mode switch (`Some(T)` ⇒ check, `None` ⇒
synth); `Ref` consults `RuleSig` in SCC order; **the trunk's equality reconciler is shared**
(equality-unify for non-numeric leaves; a subtype/coercion join at the numeric seam — CHECK is
*subsumption*, not `==`, so a lawful widening `int -> f64` is not a false clash — R1, the sharpest 1C
risk). The current CHECK-mode site (`generate.rs:370`) today *grounds* rather than *checks* — installing
the compare-and-localize is the whole of CHECK mode.

**Integration seams (against ROOT, additive).** `TypeObligation` gains `span: Option<GrammarSpan>`
(default `None` — byte-stable) + a `ProjectionMismatch { arrow_span, synthesized, ascribed }` variant;
a `GrammarIR.map_spans: HashMap<NodeId, GrammarSpan>` side table carries the arrow byte-range from
lowering (the frontend must **capture the `->` span** — the one non-additive touch, isolated to
parser→lower). **Consumer-less-substrate edict (viii) bites: the diff MUST include an `analysis`-crate
bridge** `type_obligations → lsp_types::Diagnostic{range}` — zero hits today; enriching the stream
without a consumer fails the bar.

**Gates.** (G1 — make-or-break, R-C2) on an authored typed `->` mismatch micro-fixture (`frac = digits
"." digits -> Ratio`, research-C §7.1 — **build the fixture first; not structurally impossible, the
greenfield json now carries 4 arrows, but a declared/synthesized-*mismatch* fixture must be
hand-authored**), `check_expr`'s `ProjectionMismatch.arrow_span` equals the `->` span where
synthesis-first reports `HeterogeneousAltJoin{node}` at a span-less interior — two modes, two blame
sites, side by side. (G2) subsumption not equality: `int -> f64` → zero `ProjectionMismatch`. (G3) CHECK
is diagnostics-only — projected `TypeDesc` byte-identical to synthesis-first; regen byte-identical. (G4)
recursive `->` reaches the fixpoint in bounded iterations. (G5) `git diff` over emitter/generated empty.

**Open questions.** (O1 — the make-or-break has no input) a declared/synthesized-*mismatch* `->`
fixture must be hand-authored; 1C's distinct identity is a bet until GN-3.W3 or that fixture lands.
(O2 — reactivation) 1C reactivates to ADVANCED only if G1 proves the layer earns a name. (O3 — sub-span
ceiling, a LOCK-3 field) `Project.ty` is one `String` carrying whole-expr span; a wrong ascription
cannot underline the `T`. **Bank the `PassDiagnostic.span` add + the LSP bridge regardless of the
carrier winner — and NAME THE WAVE (no gate schedules them; they risk falling between carrier-selection
and GESTALT-B).**

---

## FAMILY 1D — TYPES-AS-ECLASSES · disposition: **BANKED** · recursion-keep + "zero new engine" + cost-extraction **RETIRED** · structural-μ hook **UNPROVEN (not "dissolved")** · floor 43%

**Architecture.** The type lattice lives in an e-graph; unification = congruence closure
(`union`+`rebuild`); recursive sums are native cyclic e-classes; projections canonicalize by
saturation; extraction picks a representative by cost. Shares the LOCK-2 engine and homes the
*type-tier* Language.

**Mechanism (net-new, mirroring the grammar tier).** `crates/ir/src/egraph/type_tier/` with a
`TypeENode` (= `TypeDesc` with `Id` children, `#[derive(Language)]` verbatim) adding `Sum(Box<[Id]>)`,
`Row{fields,open}`, `RowField{key,ty}`, `Rec(Id)`; a `TypeShapeAnalysis` deriving record-vs-list from
**body structure, not brace bytes**; positional injectivity + AC-sum canonicalization co-resident on
one `Sum` under one saturation.

**The disposition and why it is UNPROVEN, not clean.**
- **Recursion-keep RETIRED (reconfirmed).** The running CSP is finite + terminating on JSON (2 knots),
  Sheets (20 mutual knots), CSS L4 (44 knots), zero overflow (probe2). 1D's native cyclicity beats only
  the unbuilt strawman.
- **"Zero new engine" FALSIFIED + reverted.** The injectivity hook needs the shared-engine `union`
  scheduling amendment — a **3-lock blast radius** (X6), un-measured on LOCK-2/LOCK-5 hot saturation.
- **Cost-extraction IDLE.** Overload/cost residue = 0 everywhere (X4-caveated).
- **The ONE surviving hook — structural `List ≠ Row` — is NEITHER validated NOR cleanly dissolved.**
  PROTO-2 (SPEC_ONLY — unlanded, §0c) proved the **NEGATIVE**: structure-alone on the real grammar's
  `object` body → **`Unknown`**. Its POSITIVE holds only on a **re-authored** grammar carrying a
  `-> … : key` role the shipping json.bbnf lacks; and the natural inline form of that role
  **trips the typed-materialization-invariant** (`wrap.rs:242`). So the "dissolve" verdict is
  over-claimed: **`List ≠ Row` on un-annotated real bodies is derivable by NO family** — not 1D's
  e-graph (extraction ≠ inference; it too needs the `key` fact), not Open R's widened CSP (returns
  Unknown), not the running pass (masks via `json_templates`). 1D is **stuck behind the same unbuilt
  inference every family shares — not superseded.**

**Integration seams.** `type_tier/{node,analysis,rules,lower}.rs` net-new; extraction emits `TypeDesc`
(grown Sum/Row) into `ir.types`; lower through `TypeAnalysis::from_ir` + `named_types.rs:100` — **but
the net-new `TypeAnalysis` 1D proposes name-collides with the shipping LOCK-3 driver struct** (X2,
Constitution vi); **no TS mirror exists** (X7), ranking 1D last on iso.

**Gates.** (G1 — make-or-break, UNBUILT-IN-TREE) does `TypeShapeAnalysis` derive `Row` from the
repeat-of-pairs body **on the REAL `grammar/json/json.bbnf`** (not a synthetic re-authoring) with zero
`{`/`}` byte match and zero grammar-name predicate — **and does `has_rule("value")` actually delete
under regen-byte-identity? — Brief 1.** (G2) the `union` amendment routed through a 3-tier saturation
measurement; land or retire in-tree. (G3) AC-sum canon + positional injectivity co-resident on one
`Sum` — built *through*, not *around*. (G4) equirecursive `value ⊇ object ⊇ value` builds a finite
`Rec`. (G5) determinism at 191-rule CSS under `FxHashMap` iteration. (G6) `git diff crates/runtime/`
empty.

**Open questions.** (O1 — the RETIRE/KEEP is architecture-contingent + cross-lock) grammar-authoritative
(a coarse `TypeDesc` is a wall → 1D closes it) vs codegen-distributed (the template is sanctioned → 1D
cosmetic) — a LOCK-7/GESTALT-B call. (O2 — the masked fallback is a real clause-i defect **independent
of 1D's fate**) `json_templates/value.rs` + `has_rule("value")` (`runtime_generator.rs:98`, present at
HEAD) — a `no-workarounds` / `typed-materialization-invariant` breach owned jointly with LOCK-7, closed
by Open R **only if the real-grammar derivation lands** (Brief 1). **Reactivate only if the structural
hook is *needed AND* 1D-specific** — which the real-grammar probe decides.

---

## 5. THE INCOMPATIBILITY MAP

The four families are **not** four coequal rivals. There is **one exclusive axis** (the equality
carrier, three mutually-exclusive members), **one banked consensus architecture** they all share,
**one compatible overlay** (1C), and **one orthogonal vocabulary widening** (Open R) that *would*
decouple structural fidelity from the axis — *if* the structure were derivable, which pass 2 could not
show on the real grammar.

### 5.1 The banked consensus — HM(X) architecture (NOT a family, NOT re-litigated)

1A and 1B have collapsed onto **one architecture**: an equality trunk + a bounded residue post-solve +
let-generalization (prior-art §2A/§3; both critics). Per the redirect rule this is the redundant
mechanism → **BANKED as the settled floor.** The redundancy is *architecture-level, not build-level*,
so neither carrier is force-banked.

### 5.2 The exclusive axis — the equality CARRIER (choose exactly one)

| | 1A union-find re-seed | 1B running-CSP hardened | 1D e-graph congruence |
|---|---|---|---|
| equality primitive | Robinson `unify` (near-linear) | `assign` monotone sweep, **+refinement-aware B1 clash arm** | `union`+`rebuild`+injectivity |
| carrier store | union-find map (**no refinement notion — must re-invent it**) | `Csp<TypeDomain>` (both backend consumers + TS) | e-class UF + hashcons (net-new Language) |
| lineage / cost | **port INTO root, DELETE ~101 LOC, rewire ~91 sites** | **additive, keeps running code, net-0 LOC** | root, net-new + 3-lock engine amendment |
| iso (Constitution iii) | monotype → **NO TS mirror shown** | `TypeDesc` → **crosses to TS today** | `TypeENode` → **NO TS mirror + name-collision** |
| pass-2 measurement | **UNRUN (Option A never built)** | **worktree-measured byte-identical; NOT landed at HEAD** | **SPEC_ONLY (unlanded)** |

**Why they cannot coexist as "the spine":** each defines equality over a *different* variable store,
produces a *different* type-of-record, and one owner must hold the `Project` discard site. The
greenfield ports **one** organ. **1A ⊥ 1B ⊥ 1D pairwise on the seat.**

- **1A ⊥ 1B — a build-cost + iso fork, not an architecture fork.** Same HM(X) architecture; the live
  difference is the carrier. 1A **deletes** running code + re-invents the refinement lattice inside
  union-find (its "unsound as shipped" justification is doubly dead); 1B **hardens** additively and
  keeps the TS-crossing `TypeDesc`. **LEAN-1B on cost + iso; NOT CLOSED (Option A unbuilt).**
- **1A ⊥ 1D** — union-find vs congruence-closure. 1D's recursion win RETIRED; cost-extraction IDLE;
  its sole survivor (structural `List ≠ Row`) is UNPROVEN on the real grammar (§5.4). One store owns
  `Project`.
- **1B ⊥ 1D** — both root-lineage; both claimed the same residue; the census empties it for both. 1D
  adds a net-new Language + a 3-lock engine amendment over 1B's additive hardening for no
  dominant-judgment gain, and loses the iso floor.

### 5.3 The compatible overlay — 1C is NOT on the axis

**1C ∥ (1A | 1B | 1D):** a checking front + SCC seam + span field over an equality reconciler it does
not define (reuses PROVE-08's `Unifier`). It folds onto whichever carrier wins. Its only
incompatibility is degenerate: with ~zero *mismatch* `->` in the real corpus, **1C's make-or-break has
no input** until typed grammars exist. The named graft: `infer_expr → synth_expr`, `check_expr`
overlays, the trunk's reconciler shared.

### 5.4 The orthogonal widening — Open R decouples structure from the axis ONLY IF derivation lands

`List ≠ Row` is a **LOCK-3 vocabulary gap** (`LayoutKind::Row` / `TypeDesc::{Sum,Row}`), engine-agnostic
(X4). Adding it lets **any** carrier populate the distinction and lower it through `from_ir` — **but
adding the vocabulary is not deriving the fact.** PROTO-2 showed that on the real (un-annotated) json
body, structure-alone yields `Unknown`; the record bit lives in a `->`-declared `key` role the shipping
grammar lacks, and the natural form of that role trips GN-3.W3. **So the conditional the first draft
resolved in Open R's favor is UNRESOLVED in-tree:** whether structural fidelity is orthogonal to the
engine axis turns on **(a)** landing `TypeDesc::Row`, **(b)** a LOCK-7 totality decision on who declares
record-ness, **(c)** a LOCK-3 GN-3.W3 fix so the annotation survives lowering, and **(d)** actually
deleting `has_rule("value")` under regen-byte-identity. **Brief 1** attacks exactly this. Until it
lands, Open R neither dissolves 1D nor closes the masked fallback.

### 5.5 The orthogonal residue seam — Open X (pluggable, carrier-independent)

Widths/overloads/decode-class route to a bounded post-solve that is **the same seam whichever carrier
wins** — an ordered defaulting lattice (prior-art §1.4) with CSP-search as a fallback only for the
non-chain overload case the corpus does not exhibit. `decision_csp.rs` is the existing two-mode
(`propagate()` vs `solve_optimized`) proof-of-compile. Decide defaulting-vs-search on the width
micro-fixture **independently of the carrier fork.** UNRUN this pass.

### 5.6 Open S — the cross-rule seam as one attribute-grammar discipline (asserted, NOT shown)

All three carriers re-derive the cross-rule seam independently (1A: SCC generalize-after-fixpoint; 1B:
federation sequencing; 1C: SCC bounded fixpoint). Read as one discipline: SYNTH = synthesized attribute,
CHECK = inherited attribute, the cycle-break = a bounded circular AG with a finite-height termination
lattice (Knuth/Farrow/JastAdd). **This is a reading "to be shown faithful, not built" — an OPEN gap, not
a closed one** (CRIT-1 G-XL-1): either demonstrate the running `mod.rs:72-127` cycle-break IS a
monotone-lattice circular AG, or stop counting Open S as resolved.

### 5.7 The cross-lock hook — Open R's Map-barrier decision is a LOCK-5 input (XL-C)

Making `List ≠ Row ≠ Map` a first-class typed record (`TypeDesc::Row`) *is* XL-C's "abandon
Map-barrier" branch, under which LOCK-5's 5B functor conditionally re-opens; keeping the Map opaque (the
running masked fallback) is the "hold Map-barrier" branch, under which 5B stays retired. **This is not
LOCK-1's call to settle** — flag it to LOCK-5 / GESTALT-B as a named dependency: whichever way Brief 1
resolves the Open-R derivation, its verdict conditions 5B's re-open (Constitution ix, a lawful
supersession input, never a silent contradiction).

---

## 6. BLOCKED-CANDIDATE REGISTER + SYNTHESIS FLOOR

- **1A — not blocked. ADVANCED (carrier candidate #1), but LEAN-against.** Liability: deletes running
  code on the surviving tree + re-invents the refinement lattice + has no TS mirror; its displacement
  justification is doubly dead. Its own make-or-break (Option A parity race) is **UNRUN**.
- **1B — the LITERAL single heterogeneous store is BLOCKED-candidate** (X3: a LOCK-1⊕LOCK-2 store
  merge). **But 1B-as-hardened-running-CSP is NOT blocked — it is the LEADING carrier candidate**
  (additive, sound, iso-safe, net-0). Bank the literal thesis; the architecture 1B ships IS HM(X). **Its
  one carrier-independent deliverable (the additive `assign` hardening) is STILL NOT LANDED after two
  passes (X8 unmet) — the pass-3 must-do.**
- **1C — not blocked; not an engine family. BANKED** as the diagnostics/locality discipline over the
  winner; reactivates only if the make-or-break (needs a hand-authored typed `->` mismatch fixture)
  proves the layer earns a name. Bank the `PassDiagnostic.span` add + LSP bridge regardless — **name the
  wave.**
- **1D — structural-μ hook is UNPROVEN, not dissolved.** On the real grammar the derivation returns
  `Unknown` and the masked fallback stands; it is **stuck behind the same unbuilt inference every family
  shares**, which is a LOCK-3⊕LOCK-7 question. recursion-keep + "zero new engine" + cost-extraction
  RETIRED as named negatives. The `union` amendment is a 3-lock GESTALT-B call. **BANKED.**

**Synthesis floor for pass-2 (honest, measured).** The durable output is **an HM(X) equality trunk + an
ordered defaulting-lattice residue solver (Open X) + a bidirectional diagnostics overlay (1C folded)**,
seeded by **one** carrier — **LEANING to 1B-hardened-running-CSP** on build-cost + Constitution-iii iso,
**not yet CLOSED** (Option A unbuilt; the hardening unlanded). Structural fidelity (`List ≠ Row`) is
**NOT yet supplied by anything in-tree** — it is a cross-lock LOCK-3 (vocabulary + GN-3.W3) ⊕ LOCK-7
(who declares record-ness) obligation that pass 2 surfaced but did not discharge. The two live questions
pass 3 must convert from LEAN/UNPROVEN to MEASURED: **(i)** land the additive hardening + build Option A
+ show the TS crossing → CLOSE the carrier seat; **(ii)** land `TypeDesc::Row` + decide who declares the
`key` role + delete `has_rule("value")` under regen-byte-identity → resolve the structural axis (and
1D's fate) on the REAL grammar. 1B's single-store and 1D's recursion/cost-extraction are evidence-retired.

---

## 7. THE TWO SHARPEST UNCERTAINTIES → prototype briefs

The pass-2 briefs (carrier race; Row-inference) ran but **did not land and over-claimed** (§0). These
two attack the exact residue the critics isolate — the structural axis on the *real* grammar, and the
carrier's un-probed iso floor — and most advance pass 3 toward convergence.

**Brief 1 — REAL-GRAMMAR ROW CLOSURE: does Open R actually close the mask, or is it a LOCK-7 blocker?
(bbnf-worktree).** Land `TypeDesc::Row` + `LayoutKind::Row` at HEAD (exhaustive-match arms + Rust/TS
projections). Then take the **shipping** `grammar/json/json.bbnf` unchanged and run the structural
classifier: measure `class(object)` and `class(array)`. It will refuse (`Unknown`) — reproduce that.
Now author the `key` role into the REAL grammar the honest way (rule-level `keystr = /"[^"]*"/ -> input
: key`, the form that does NOT trip `lower/expression/wrap.rs:242`), regen, and (a) confirm
`class(object)==Row ∧ class(array)==List`, (b) **DELETE `has_rule("value")`
(`skinny/crates/codegen/src/runtime_generator.rs:98`) and its `json_templates/value.rs` mask**, (c)
`cargo xtask regen` and diff generated + runtime output. **Pass/fail:** Row derives on the annotated
real grammar AND the mask deletes AND regen is byte-identical except the intended Row-vs-List
distinction (record the exact diff) → **Open R dissolves 1D, the clause-i defect closes.** If the
annotation cannot survive lowering (GN-3.W3 trips) OR the mask cannot delete without regressing → **the
structural axis is BLOCKED behind a LOCK-7 totality decision + GN-3.W3**, 1D's hook is BLOCKED-candidate,
and the finding routes to GESTALT-B. Either verdict decisively moves the lock and discharges CRIT-2
directive 3.

**Brief 2 — THE CARRIER'S ISO FLOOR: does the winning record cross to TS byte-exact? (bbnf-worktree).**
Constitution iii decides the carrier and is entirely un-probed (G-XL-2). With `TypeDesc::Row` +
`TypeDesc::Sum` landed (Brief 1's vocabulary), construct one rule whose inferred type is a
`Row`-bearing `Sum` (e.g. a two-variant tagged record), lower it through **both** LOCK-3 sinks
(`TypeAnalysis::from_ir`, `rust/view/named_types.rs:100`) to Rust output AND through `type_desc_to_ts`
(`backend/ts/projection.rs:11`) to TS output; run `cargo test` on a differential asserting the two
backends name the *same* record shape (Rust `enum`/`Vec<(Span,T)>` ↔ TS discriminated-union /
`Record<string,T>`). **Pass/fail:** the `Row`/`Sum`-bearing `TypeDesc` crosses to TS with a
well-formed parallel type and `git diff crates/runtime/` empty → **1B's iso-safety is MEASURED, and the
carrier seats on iso independently of build-cost** (a fused monotype and a `TypeENode` have no such
path — name that gap concretely). If a `TypeDesc`-borne Row/Sum does NOT cross → the iso floor is unmet
for *every* carrier and the record vocabulary itself needs an XL-C/LOCK-6 amendment before any carrier
is chosen. This is the lock's largest un-probed surface; resolving it closes the carrier fork that
Option A alone (build-cost only) cannot.
