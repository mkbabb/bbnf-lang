# LOCK-1 — THE SOLVER SPINE · pass-1 · FAMILY SPECS (synthesis)

*Type inference & solving for the bbnf grammar plane: `->` projections, document shapes,
sums/products, capacities, numeric widths, decode classes — inferred, solved, erased to
zero runtime cost, with grammar-sourced diagnostics. GN-3's three walls are the floor.*

One cogent spec per family while routes remain incompatible, then the incompatibility map.
Elegance earns nothing on its own; a family stalled at a missing primitive equal in
difficulty to the original problem is marked BLOCKED-candidate. Verified against source, not
taken on the reports' word (§0).

---

## 0. THE DECISIVE FACT THE CHARTERS SPLIT ON — verified, and it re-frames the whole lock

The four charters silently reference **two different code lineages**, and the prior-art
report's sharpest constraint (its C4: *"Current CSP consumer = LOCK-2, not LOCK-1 … families
1B/1D inherit no type-plane consumer … forbidden consumer-less substrate"*) is **factually
false for the tree the greenfield is being cut from.** Verified this pass:

- **`skinny/crates/` (retired lineage).** Carries 1A's dormant HM scaffold —
  `passes::types::TypeFacts { subst: Substitution, obligations: Vec<TypeObligation{ span,
  solver_stage } }` (`skinny/crates/passes/src/lib.rs:126-145`), **populated by nothing**; the
  `->` discard is verbatim at `:204` (`ExprKind::Project { body, .. } => self.infer_expr(*body)?`).
  This scaffold is *itself* consumer-less substrate today (a Y.13 violation), which 1A's
  activation repairs. This is the lineage 1A and 1C target.
- **root `crates/` (live lineage).** Carries a **running CSP type inference** —
  `crates/ir/src/passes/types/constraint/{domain,alt,seq,operators,reference,grounds,revise}.rs`,
  driven by `project_types()` → `csp.propagate()` (`crates/ir/src/passes/types/mod.rs:51,67`),
  emitting `TypeDesc::HeterogeneousAltJoin` (`crates/ir/src/types/type_desc.rs:63,209`),
  threaded through `GrammarIR::type_obligations` (`grammar.rs:428`) into **both backends**
  (`core/src/backend/rust/{ir_types.rs:342, emitter/registry_emit.rs:198}`,
  `core/src/backend/ts/{projection.rs:37, emitter/grammar.rs:30}`), asserted end-to-end
  (`core/tests/{types_heterogeneous_alt,project_types_json,project_types_css_l4}.rs`,
  `ir/tests/lattices/types.rs`). This is the lineage 1B targets — **it is not speculative
  substrate; it is the type system of record that already ships.** The e-graph engine 1D wants
  (`crates/egraph/` + `csp_scheduler.rs`) is equally live, with a working `Language` consumer
  (`crates/ir/src/egraph/node.rs`).

**Consequence.** The lock is not "green HM core vs three exotic proposals." It is a genuine
**engine-of-record fork**: a *proven-elsewhere* union-find HM core (1A/1C, PROVE-08 + skinny
scaffold) versus an *already-running-here* lattice-CSP inference (1B) versus an
*already-shipping-engine* e-graph (1D). C4's "consumer-less" veto **does not bind 1B or 1D**;
the honest gate on all four is the prior-art report's *other* verdict (its §2G, §4.3, R-B4):
**is the non-equality residue rich enough to justify any solver beyond an ordered defaulting
lattice?** That single question — not a metatheory dispute — adjudicates the fork, and it is
measurable now (Brief 1).

---

## FAMILY 1A — UNIFICATION-FIRST (the HM core grows) · disposition: VIABLE

### Architecture
One Hindley–Milner inference over an open, structural type language, in the generator. Every
`->` drives first-order unification; every rule is a let-generalized scheme; document shapes
are **row-polymorphic records** (Leijen scoped labels — JSON permits duplicate keys, and
skinny already carries `DuplicatePolicy::LastWins`). Unification is the decidable, principal,
terminating **trunk**; the vendored `csp-solver` is a **bounded post-solve** for the genuine
residue only (widths, decode-class/overload, capacity). This is HM(X)/OutsideIn(X) verbatim:
generate equality constraints by unification, defer the theory constraints, solve them in a
finite/lattice domain X. It is the exact shape the prior-art convergent verdict (§5) endorses.

### Mechanism
Fuse skinny's two half-built type reps into one monotype in `passes`: grow
`passes::types::Type` with `Prod(ρ)`/`Sum(ρ)` over a row `ρ = {l:τ… | r}`; port PROVE-08's
proven union-find `unify`/`find`/`bind`/`resolve` (patch.diff:255-353) verbatim for the leaf
half; add Robinson decompose arms for `List`/`Option`/`Prod`/`Sum`; `unify(Rule i, Rule j) =
i==j` is the **iso-recursive nominal fold** — unification never descends through a rule
boundary, so the mutually-recursive `value` types without an occurs-check divergence. Rows
unify by label-splitting; the open tail `r` *is* `DirectTypeKind::Struct{unknown_fields:Skip}`
promoted from policy-enum to type-theoretic object — **Wall 1 falls by openness, not by
exhaustive per-document closure.** `Rule(id)` becomes a `let`-scheme: `generalize(infer(body))`
+ `instantiate` at each `Ref`; SCCs seeded monomorphic, generalized after the fixpoint (the ML
recipe). The `->` arm becomes the bidirectional driver (ascription checks, bare target
synthesizes via `classify_arrow`), replacing the `:204` discard.

### Substrate & seams (each with an in-wave consumer — Y.13)
Activate `TypeFacts.subst`/`.obligations`; replace the pure fold `infer_expr`
(`skinny/…/lib.rs:178-231`); new `types/{row,scheme,solve_annotations}.rs`; grow
`schema/direct_schema.rs:52` with `DirectTypeKind::Sum` + a width field on `DirectScalar`; feed
`json_typed_direct.rs`. Delete the parallel `codegen::type_plane::hm::Ty` duplicate — the
union-find core moves into `passes`, `codegen` becomes a consumer (repairs the R6 §5 two-systems
split). Width/overload/capacity route into `obligations` under `solver_stage ∈
{"width-csp","overload-csp"}`, solved over `csp-solver`'s `BitsetLatticeDomain` +
`CostFiniteDomain` — **or, per prior-art §2G/§4.3, an ordered defaulting lattice**, which the
census (Brief 1) may show suffices without a CSP at all.

### Gates it carries
(i) recursive `value` infers one 6-variant `Sum`, terminating, order-insensitive (rows are
unordered — GN-3 R5 for free); (ii) open row absorbs extra JSON keys while closed fields check;
(iii) let-gen: a shared `pair`/`entry` rule instantiated at two field types does **not** unify
its payloads; (iv) width post-solve recovers PROVE-08's admitted `U8Tag→U32` loss to `u8`;
(v) ascription clash raises a unify error carrying the arrow `SourceSpan`; (vi) `codegen`
duplicate deleted, `git diff crates/runtime/` empty, derive wall-time within noise of GN-3's
~230µs.

### Exact open questions
- **Row discipline: Leijen scoped-labels vs Gaster–Jones lacks-predicates.** The report splits
  — raw JSON duplicates keys (Leijen) but a *derived struct schema* wants uniqueness
  (Gaster–Jones). 1A must pick one and state it; "records, informally" re-derives Wand's
  principal-type bug (prior-art §2C, trap 2). **Unresolved.**
- **Is the width residue a CSP or a defaulting lattice?** Prior-art §2G calls width *defaulting,
  not search*; over-modelling it as CSP is the named over-engineering trap. Depends on the
  judgment-mix census (Brief 1). **Unresolved — but cheaply.**
- **Generalization criterion (value restriction).** Rules are pure schema producers so the risk
  is low, but the criterion must be explicit (prior-art trap 4). **Named, not yet stated.**

### Why not BLOCKED
No missing primitive: the union-find core is proven (PROVE-08 CONFIRMED_DELTA), the scaffold
exists, every wall-cure is a known arm of Robinson + row theory. Its residue solver has a cheap
fallback (defaulting). Solidly viable; it is the prior-art report's endorsed spine.

---

## FAMILY 1B — ONE-CONSTRAINT-STORE · disposition: VIABLE (with one confirmed defect)

### Architecture
Every judgment — leaf equality, shape, sum membership, width, capacity, decode class, cost — is
a constraint over inference variables in one engine under one propagation discipline.
Unification is **not** a separate algorithm; it is equality propagation *as a lattice join*
inside the `csp-solver` store, driven to a fixed point by `propagate_monotonic`. This is
HM(X)=CLP(X): type inference is constraint solving over the parametric domain X, and X is the
vendored solver. **The decisive differentiator from every sibling: 1B is not a proposal — it is
the running type system of record in root `crates/`** (§0). Its work is to *name* that
architecture, close its open judgment kinds, and refuse to resurrect a second (union-find) core.

### Mechanism (verified running)
`TypeDomain { solved: Option<TypeDesc> }` implements `LatticeDomain`: `bottom()=None`,
`join` assigns on `None→Some` (a bottom→ground unification step). One propagator family per
grammar former (`alt`→sum/join, `seq`→product, `reference`→referent, `operators`→Vec/Option,
`grounds`→leaf); `revise.rs` lifts heterogeneous alternations that cannot collapse to a
homogeneous payload into `HeterogeneousAltJoin`, recorded as a **named obligation** carrying
the branch types + grammar node id (the diagnostics-first channel, drained at convergence).
AC-3 `propagate()` resolves it with no search/backtracking on the equality core (`mod.rs`
docstring: *"no search/backtracking needed"*) — which is precisely how 1B **sidesteps the Swift
exponential-blowup trap** (prior-art §2B): equality stays in monotone propagation, never in
disjunction search. Widths/capacities are the net-new work: `CostFiniteDomain` variables in a
coupled `Csp`, `solve_optimized` branch-and-bound — the same pattern `decision_csp.rs` already
runs for backend shape.

### Substrate & seams
All live: `constraint/domain.rs` (the join cell), the per-former propagators, `obligation.rs`
(`TypeObligation{UnresolvedCompoundRef, HeterogeneousAltJoin}` + `ObligationSink` — an
`Rc<RefCell<…>>` the csp-solver was *deliberately reshaped upstream to admit*, the `ThreadSafe`
vacuous marker at `csp-solver/…/traits.rs:19-50`), `type_desc.rs:209`, the two backend
consumers. Net-new domains — width `CostFiniteDomain`, decode-class `FiniteDomain`, capacity
cost var — all ride existing `LatticeDomain`/`Constraint`/`CostDomain` public traits: **stay on
the consumer side; any engine change is a csc411 tranche, not a bbnf hand-patch** (the vendor
sync gate is byte-diffed).

### Gates it carries
(i) the six-arm alternation yields one canonical **order-insensitive** sum; (ii) two
incompatible arrows on one rule yield a **clash obligation, not last-writer-wins**; (iii) the
judgment-mix census shows the non-equality residue is non-trivial (else 1B collapses toward 1A
and must say so — R-B4); (iv) the store-shape decision (federated vs single sum-domain) is made
with a *measured* reason; (v) `git diff crates/runtime/` empty.

### Exact open questions
- **R-B2 — CONFIRMED DEFECT (not a risk — a bug in live code).** `TypeDomain::join`
  (`constraint/domain.rs:66-75`) has the arm `(Some(existing), Some(ty)) if existing != ty => {
  self.solved = Some(ty.clone()); }` with the comment *"may refine types during convergence."*
  This is **last-writer-wins on a genuine unification clash** — non-monotone (not a lattice
  join), order-dependent, and a **silent overwrite where `no-silent-epsilon` demands a raised
  obligation.** Two `->` arrows declaring incompatible leaves are silently reconciled instead of
  reported. This must become a fail-explicit obligation; refinement reserved for bottom→ground
  only. **The sharpest concrete finding of this pass.**
- **R-B1 — the monomorphic-`Csp<D>` wall.** `Csp<D>` holds one domain; a literally-single
  heterogeneous store needs either a sum `JudgmentValue` domain (a tag-branch per revise,
  fighting the type system) or a **federation** (one typed `Csp` per lattice, sequenced by
  `solve_with_given`). The tree runs the federation. "One store" then honestly means *one engine
  + one obligation channel + one discipline*, not one variable arena. The decode-class-depends-
  on-sum-variant counterexample forces the decision. **Unresolved — needs a prototype, not paper.**
- **R-B4 — worth it over union-find for equality alone?** For pure equality, union-find is
  asymptotically better than the worklist re-scan. 1B's win is *only* that widths, capacities,
  classes, cost, FIRST/FOLLOW, and dispatch are already one substrate. If the census shows
  equality dominates and residue is singletons, 1B collapses to 1A. **Measurable — Brief 1.**

### Why not BLOCKED
No missing primitive — it runs. Its open questions are bounded engineering (a bug fix, a
store-shape choice), not a problem equal in difficulty to inference itself. But its *distinctness*
from 1A is contingent on R-B4: a thin residue retires 1B's thesis, not its code.

---

## FAMILY 1C — BIDIRECTIONAL-LOCAL · disposition: VIABLE (distinctness falsifiable)

### Architecture
Two judgments per rule — SYNTHESIZE (`⇒`, read a type off structure) and CHECK (`⇐`, verify
structure against a declared `-> T`). Pfenning recipe (Dunfield–Krishnaswami): intro forms
check, elim forms synthesize, a single subsumption site is *the sole place a type error is
raised*. The organizing thesis and the reason it is a distinct center: **the expected type is
in hand at the failure site, so every diagnostic names the exact grammar span** — error
locality is a structural property of the algorithm, not a message afterthought. Rule-local;
"global" is bounded to the SCC condensation of the `Ref` graph.

### Mechanism
Rename skinny's `infer_expr` to `synth_expr` (it is already the synthesis half in embryo); add a
dual `check_expr(ExprId, &Expected) -> Result<(), TypeError>`; the two are mutually recursive.
The `:204` `Project` arm becomes the **mode switch**: `ty:Some(A)` ⟹ `check(body, resolve(A))`,
bare ⟹ `synth` via `classify_arrow`. The `Ref` arm consults a `RuleSig: HashMap<RuleId, Type>`
populated in **reverse-topological SCC order**; an acyclic rule is purely local, a cyclic SCC
(json's `value/object/array/pair`, 4 rules) is a **bounded local fixpoint** with a `Changed`
flag — *this* is the precise reading of "global solving only at cross-rule seams." Structure is
pinned by checking; numeric width/overload is a bounded lattice post-solve at the subsumption
seam (subtype-join over `u8≤u16≤u32≤u64`, MLsub-style — **not** equality-unify, which would
spuriously clash `-> 0u8` against `-> 70000`).

### Substrate & seams
`synth`/`check` split on `Infer`; `RuleSig` + SCC pre-pass over the `Ref` graph;
`TypeError{span, expected, got, rule}` + a new `PassDiagnosticCode::TypeMismatch`; the
subsumption reconciler reuses PROVE-08's `Unifier` for equality and `classify_arrow` for
bare-leaf synth; width post-solve over `BitsetLatticeDomain`. **Named substrate gap (R-C5):**
`PassDiagnostic` (`skinny/…/diagnostics.rs`) is `RuleId`-only, *no span* — 1C must add
`span: Option<SourceSpan>` and thread `Expr.span`/`Rule.span`. Consumers: the mode-switch arm,
`derive_materialization_plan` filling `DirectBuildField.target`, the .bbnf-anchored error report
(the family's headline artifact).

### Gates it carries
(i) the mode switch replaces the discard *via checking*, filling non-`None` targets for the four
leaf rules; (ii) **the diagnostic head-to-head** — an erroneous `string -> decode_...(input) :
Bool` reports `TypeError.span == Project.SourceSpan` under 1C while unify-first reports at a
synthesized interior; (iii) the SCC census: json's 4-rule cycle solved by bounded fixpoint,
leaf rules purely directional; (iv) width CSP terminates in one AC-3 pass; (v) sheets regen
byte-identical after the `ProjectionKind` fold retires; `git diff crates/runtime/` empty.

### Exact open questions
- **R-C2 — the bidirectional critique, self-admitted.** `grammars/json.bbnf` has **zero `->`**;
  with nothing to check against, 1C *degenerates to pure synthesis = 1A*. Its distinct value
  exists **only on typed grammars** (GN-3 W3's twitter/citm/canada `->` chains). The honest
  claim — *on generic json, 1C ≡ 1A; on typed grammars, 1C localizes where 1A does not* — is
  **falsifiable and unproven.** If unify-first reports the same span on the same erroneous
  grammar, 1C carries no distinct value and folds into 1A. **This is the family's make-or-break
  and it is measurable (Brief candidate).**
- **R-C4 — subtyping-not-equality at the seam.** Getting the numeric case wrong collapses 1C into
  spurious-clash (too strict) or lost-width (too loose). The join must be MLsub subtype-join,
  equality-unify reserved for non-numeric leaves. **Design-fixed, not yet built.**
- **R-C3 — span granularity.** `Project.ty` is one `String`; a wrong ascription should underline
  the `T`, but `Project` carries only whole-expr span. Sub-span is the quality ceiling.
  **Named gap.**

### Why not BLOCKED
No missing primitive — it is 1A's Unifier plus a checking overlay plus an additive span field.
The risk is not blockage but **redundancy**: 1C is a strict superset of 1A that must *earn* its
separate identity on the diagnostic axis, or be absorbed as 1A's diagnostics layer.

---

## FAMILY 1D — TYPES-AS-ECLASSES · disposition: VIABLE-AT-RISK (idle-machinery caveat)

### Architecture
The type lattice lives in an e-graph. Unification = congruence closure + injective
decomposition; recursive sums are native cyclic class graphs (no occurs-check, no μ-binder);
width is an analysis-lattice fact on one leaf class; a representative type is chosen by
cost-model extraction. The claim: the **already-shipping** `crates/egraph` engine (union-find +
hashcons + `rebuild` congruence + `Analysis` monotone lattice + `Extractor` + `csp_scheduler`)
subsumes the bespoke HM core and reaches all three GN-3 walls with **zero new engine** — only a
third `Language`. Type inference becomes the third instance of a proven saturated pattern
(GrammarENode, RegexENode, TypeENode).

### Mechanism
`TypeENode` derives `Language` (verified: the derive handles `Sum(Box<[Id]>)`, `Arrow([Id;2])`,
`List(Id)`, `Row{fields,tail}`). `unify(a,b) = union(a,b); rebuild()` — `rebuild` gives the
*forward* congruence direction free. The **one net-new primitive** is the backward direction:
an **injectivity / decomposition propagator** (`Analysis::modify` hook, ~30 LOC) — when a class
holds two same-constructor-same-arity nodes, union their positional children pairwise, to
fixpoint via the existing `pending`/rebuild machinery. This is the classical CC⇒unification
delta (Downey–Sethi–Tarjan, Nelson–Oppen). Sum canonicalization (`sum-flatten`/`sort`/`absorb`
rewrites) makes order-insensitivity a *theorem of saturation*. Width is a `WidthBound{min,max}`
join on the `Int` leaf's `Analysis` data (mirroring the extant `EClassFacts.width`); the
extractor's cost function picks the minimal covering width — **the width post-solve is the
extractor, not a separate CSP.** Provenance `ExprId` rides each leaf as `#[language(skip)]`
metadata; a clash is two irreconcilable leaves in one class, reported via the explain-tree
(Nieuwenhuis–Oliveras) as a span-path.

### Substrate & seams
`crates/egraph` reused verbatim; the mirror of the live grammar tier `crates/ir/src/egraph/`
(node/analysis/rules/cost/write_back). Home: e-node+rules+analysis beside the engine's other
Languages (`crates/ir/src/egraph/`); the schema-lowering `derive.rs` in the codegen consumer.
Repurposes the `:204` discard as a `union` (declared type checks synthesized body); subsumes the
`Type` enum; lowers `TypeENode::Sum`→`DirectTypeKind::Sum`; replaces the hand schema. Rides
`csp_scheduler` unchanged — **this is the LOCK-2 engine-share the charter names.**

### Gates it carries
(i) **injectivity = unification**: `Sum([a,b])=Sum([c,d]) ⟹ a=c,b=d`, Arrow domains unify;
(ii) recursive `value` extracts to one finite 6-variant Sum with a back-edge, order-invariant;
(iii) width fact joins to `u32`, extraction picks minimal cover; (iv) **byte-identical derive
across two runs** (deterministic `classes()` iteration + stable tie-break — real teeth: FxHashMap
reorder → non-identical `DirectSchemaSet` → RED); (v) span-precise clash via explain-tree;
(vi) `git diff crates/runtime/` empty; (vii) head-to-head reproduces PROVE-08's leaf census.

### Exact open questions
- **The idle-machinery caveat (prior-art §2F — the sharpest, and why "elegance earns nothing").**
  Equality saturation's core value is *extraction-by-cost over many equal forms* — but **pure HM
  has a unique principal type, so there is nothing to extract.** The e-graph's distinctive engine
  is **idle on the equality core.** It only earns its keep where the type is genuinely
  multi-valued (widths, overloads, decode class) — *the same residue as 1B*, where it competes
  head-to-head with plain defaulting (prior-art §2G). If the census (Brief 1) shows a thin
  residue, 1D's advantage **narrows to "free sum + recursion"** — which 1A's nominal fold reaches
  more cheaply. This is not a missing primitive; it is an **unproven raison d'être.**
- **R1 — the injectivity propagator's own fixpoint.** Omit it and `Sum=Sum` does not force
  children equal (unsound extraction). Present, it may interact pathologically with the
  width-lattice join and fail to converge deterministically (1D's own stated retirement
  condition). **Unverified — the family's existence proof (Brief 2).**
- **R3 — width as analysis-lattice, not constructor.** If `Int8`/`Int16` are distinct
  *constructors*, injectivity has nothing to decompose and extraction tie-breaks
  non-deterministically. Must be an analysis fact on one `Int` class. **Design-fixed, must hold.**
- **R4 — determinism for byte-identical regen.** Extraction fixpoint + iteration order + equal-
  cost tie-break must be total and stable. **A gate with teeth.**

### Why VIABLE-AT-RISK, not BLOCKED
The one net-new primitive (injectivity, ~30 LOC, classical, riding the shipped fixpoint) is
**not** equal in difficulty to the original problem — so 1D clears the BLOCKED bar. But it
carries the lock's sharpest "elegance earns nothing" caveat: its distinctive engine is idle on
the equality core and must demonstrate value on a residue that a defaulting lattice already
serves. **Two convergent negative results retire it:** a thin residue (Brief 1) *and* a nominal
fold that reaches the recursive sum as cheaply (Brief 2). If both land against 1D, it is a
sound-but-superfluous engine for this plane.

---

## INCOMPATIBILITY MAP — which families cannot coexist, and precisely why

The four families are distinguished by their **equality engine** and by *where the residue is
solved*. They all write the **same downstream sink** — one `TypeDesc`/`DirectSchemaSet`, one
`HeterogeneousAltJoin`/`Sum` node, the two backends. Because that sink admits exactly one type
system of record, engines for it are mutually exclusive.

| Pair | Relation | Precise reason |
|---|---|---|
| **1A ⊥ 1B** | **HARD-INCOMPATIBLE** | Two different equality engines for one seam. 1A: near-linear union-find, equality as syntactic unification, CSP as a *bounded post-solve on the residue only*. 1B: equality *is* a lattice join inside the one CSP store, no separate union-find. They cannot both own `TypeFacts`/`project_types()`. Prior-art §2B/R-B4 sharpen it: for pure equality, union-find dominates the worklist re-scan asymptotically, so 1B is *only* justified by residue-richness — the exact axis Brief 1 measures. The choice is: adopt the running root CSP (1B) or replace it with a ported HM core (1A). |
| **1A ⊥ 1D** | **HARD-INCOMPATIBLE** | Union-find HM vs congruence-closure e-graph — different engines for one seam, and 1D's recursion story (native cyclic classes, no occurs-check) directly contradicts 1A's (nominal `Rule(id)` fold guard). Both reach the recursive 6-arm sum; only one owns the inference. Brief 2 tests whether 1D's native cyclicity is a *real* advantage over 1A's fold or merely a costlier route to the same result. |
| **1B ⊥ 1D** | **SOFT-INCOMPATIBLE (engine-exclusive, substrate-shared)** | As *type inference centers* they are exclusive: lattice-CSP vs e-graph, one `TypeDomain`/`TypeENode`. **But** they share the `csp-solver` substrate — 1D's saturation is *driven by* `csp_scheduler` (a `Csp<DirtyDomain>`). They cannot both be the type engine, yet the loser's substrate survives under LOCK-2. This is the *only* pair whose conflict is resolvable by subordination rather than deletion. |
| **1C ∥ 1A** | **COMPATIBLE (overlay on trunk)** | 1C is a strict superset of 1A: it *reuses* PROVE-08's `Unifier` as its subsumption reconciler and degenerates to 1A's synthesis where there are no annotations (R-C2, self-admitted). Their natural form is **one family: a union-find trunk (1A) with a bidirectional checking + diagnostics layer (1C).** They do not compete; 1C is 1A's error-locality discipline. The open question is whether 1C's diagnostic gain is real enough to make it a *named layer* rather than a footnote. |
| **1C ∥ 1B, 1C ∥ 1D** | **COMPATIBLE-IN-PRINCIPLE, UNNATURAL** | Bidirectionality (mode-directed check/synthesize) is orthogonal to the equality engine — checking mode could in principle drive a lattice-CSP or an e-graph. But 1C-as-specified targets union-find (reuses `Unifier`, threads `RuleSig` by SCC), so pairing it with 1B/1D would require re-expressing its reconciler over their engines. Not incompatible, but not free. |

**The load-bearing incompatibility is the 1A/1B/1D three-way engine fork** (all pairwise
hard/soft-incompatible), with **1C not a fourth engine but a discipline that layers onto the
winner** (naturally onto 1A). The lock does not resolve to "pick one of four"; it resolves to
**"pick one equality engine (1A vs 1B vs 1D), then decide whether 1C's bidirectional layer earns
naming on top of it."**

### The two axes that adjudicate the engine fork (both measurable now)
1. **Residue richness** (retires 1B *and* 1D toward 1A if thin; vindicates them if rich).
   Prior-art §2G/§4.3, R-B4. → **Brief 1.**
2. **Recursion handling** (retires 1D toward 1A if the nominal fold reaches the recursive sum as
   cheaply as native cyclicity; the make-or-break both 1A and 1D name first). → **Brief 2.**
A thin residue **and** a sufficient nominal fold together collapse the fork to **1A + optional
1C layer**, which is exactly the prior-art convergent verdict (§5). A rich residue keeps 1B
live (its code already runs) and forces the store-shape decision (R-B1); it does *not* on its
own vindicate 1D over 1B, because 1D must additionally beat defaulting *and* justify its idle
extraction engine on the same residue 1B already serves.

---

## SYNTHESIS VERDICT (pass-1)

- **No family is BLOCKED-candidate.** None is stalled at a primitive equal in difficulty to
  inference itself: 1A's core is proven, 1B runs, 1C is an additive overlay, 1D's one net-new
  primitive (injectivity) is classical ~30 LOC on a shipped engine.
- **1D carries the sharpest "elegance earns nothing" caveat** (VIABLE-AT-RISK): its distinctive
  machinery is idle on the equality core and must prove value on a residue a defaulting lattice
  already serves. Two convergent negatives retire it.
- **1B carries the pass's one confirmed defect**: the non-monotone last-writer-wins clash arm at
  `constraint/domain.rs:72`, a silent overwrite where `no-silent-epsilon` demands an obligation.
- **1C's distinctness is falsifiable** and must be measured, not asserted (R-C2).
- **The prior-art C4 veto ("1B/1D are consumer-less substrate") is void** for the root lineage —
  both engines ship with live backend consumers. The honest adjudicator is residue richness, not
  a metatheory prohibition.
- **Pass-2 needs:** the judgment-mix census (Brief 1) to fix the residue axis, and the 1D
  injectivity + cyclic-sum existence proof (Brief 2) to fix the recursion axis. Together they
  determine whether the lock converges to **1A-trunk (+1C layer)** or keeps the **1B engine of
  record** live — and whether **1D** survives as anything but LOCK-2 substrate.
