# LOCK-1 · Family 1D — TYPES-AS-ECLASSES (pass-1 research)

The type lattice lives in an e-graph. Unification is congruence closure; projections
canonicalize by saturation; a representative type per rule is chosen by cost-model
extraction. The family's claim is not "an e-graph is a cute alternative to Hindley–Milner"
— it is that the substrate this repo *already ships and runs* (`crates/egraph`, two live
consumer Languages, a CSP scheduler, a calibrated cost model) subsumes the entire
PROVE-08 hand-rolled HM core AND reaches the three walls GN-3 declared net-new
architecture (sum node, recursion, width post-solve) with **zero new engine** — only a
third `Language` and its rules. Type inference becomes the third instance of a proven,
saturated pattern, satisfying constitution clauses ii/iii (one system, isomorphic tiers)
and Y.13 (a consumer for existing substrate, never orphan substrate).

---

## 1. The mechanism, made concrete

### 1.1 The theoretical spine — unification *is* congruence closure + injective decomposition

First-order syntactic unification decomposes into three primitives: (a) a union-find over
type variables, (b) hash-consing of constructor terms, (c) argument-wise decomposition of
equated constructors. An e-graph is *exactly* (a)+(b) plus equality saturation and cost
extraction. The mapping is total:

| HM concept | E-graph realization | Seam (verbatim reuse) |
|---|---|---|
| fresh type variable | fresh singleton e-class | `EGraph::add` → `unionfind.make_set` (`egraph/src/egraph.rs:155,162`) |
| `unify(a, b)` | `egraph.union(a, b)` then `rebuild()` | `egraph.rs:202`, `:251` |
| constructor term `Ctor(t₁..tₙ)` | e-node `Ctor([Id;n])` interned by hashcons | `add` memo, `egraph.rs:157,194` |
| representative / `find` | `unionfind.find` (path-halving) | `egraph.rs:114`, `unionfind.rs` |
| congruence (fwd): `tᵢ=uᵢ ⟹ C(t)=C(u)` | `rebuild` parent-edge re-canon + hashcons collision→union | `egraph.rs:251-296` |
| **decomposition (bwd): `C(t)=C(u) ⟹ tᵢ=uᵢ`** | **injectivity propagator — the ONE net-new primitive** | §1.2 |
| clash error | two irreconcilable concrete leaves forced into one class | detected at extract/validate, §1.5 |
| `let`-generalization | class-copy with fresh vars (type scheme instantiation) | §1.4 |

The e-graph's `rebuild()` already performs the *forward* congruence direction for free:
when a union makes two parent e-nodes canonicalize to the same shape, the hashcons detects
the collision and unions their classes (`egraph.rs:262-266`). PROVE-08's 221-LOC `hm.rs`
hand-writes the union-find (`find` path-halving) and the leaf-constructor binding that this
substrate already owns — so the bespoke core is **subsumed, not merely matched**.

### 1.2 The one net-new primitive: constructor injectivity (the decomposition propagator)

Congruence closure gives forward equality; unification also needs the *backward* direction.
egg does **not** do this by default (a known, named gap — egg represents equivalence, not
injectivity). Family 1D adds it as an `Analysis::modify` hook (`analysis.rs:41`, the
post-merge side-effect seam) or a dedicated propagator invoked in the saturation loop:

> When an e-class contains two e-nodes with the same constructor **and arity**
> (`Sum` len-n vs `Sum` len-n, `Arrow` vs `Arrow`, `List` vs `List`), union their
> positional children pairwise. Re-run to fixpoint via the existing `pending`/rebuild
> machinery.

This is the classical result that first-order unification = congruence closure augmented
with argument decomposition (Downey–Sethi–Tarjan, *Variations on the common subexpression
problem*, JACM 1980; Nelson–Oppen congruence closure, 1980). The propagator is ~30 LOC and
rides `EGraph::union` + `rebuild` for its own fixpoint — it schedules unions, the engine
converges them. **The prototype's decisive first test is this propagator** (§7).

### 1.3 What the e-graph gives that no bespoke HM core can — the wall-cures fall out

- **Wall 2 (recursive sum) dissolves.** `value = object | array | string | number | bool
  | null`, where `object`/`array` reference `value`, is a *cyclic* class graph. E-graphs
  are natively cyclic — no occurs-check, no explicit μ-binder. Extraction already carries a
  `visited` set (`extract.rs:192,204`) that terminates on the cycle and emits a back-edge
  (a `Ref`/`Rec` node) → a finite equi-recursive representative. PROVE-08's `TyCon` is a
  *closed leaf vocabulary* `{Bool,F64,U8Tag,StringDecoded,StringSpan}` with no composite
  and no recursion; GN-3 W1 adds `DirectTypeKind::Sum` by hand. In 1D the sum is a
  `TypeENode::Sum(Box<[Id]>)` variant that the derive macro handles for free, and recursion
  needs *nothing*.
- **GN-3 risk R5 (sum order-sensitivity) dissolves.** Saturation rules `sum-flatten`
  (`Sum` of `Sum` → flat), `sum-sort` (canonical variant order), `sum-absorb` (`T|T → T`)
  make any ordering hashcons to one canonical node. This is precisely retention criterion 2
  in the live rule set — "cost-guided canonical form selection" (`ir/src/egraph/rules/mod.rs`
  docstring). The W1 gate's "two differently-ordered alternations unify identically" is a
  *theorem of saturation*, not a hand-coded canonicalizer.
- **Numeric widths absorbed into extraction — no separate CSP.** Model integer/float width
  as a **lattice on the `Analysis` data of a single `Int`/`Float` leaf class**
  (`u8 ⊑ u16 ⊑ u32 ⊑ u64`, `f32 ⊑ f64`), NOT as distinct constructors (§6 R3). A projection
  `-> 875` sets a width-fact `≥u16`; multiple constraints join via `Analysis::merge`
  (`analysis.rs:34`, the monotone lattice join). Extraction's cost model picks the *minimal
  width covering all constraints*. The "CSP appears where unification is undecidable
  (widths, overloads)" post-solve that sibling families bolt on **is just the extractor's
  cost function here** — one mechanism, not two. The grammar tier already carries a
  `WidthBound{min,max}` lattice in `EClassFacts` (`ir/src/egraph/analysis/mod.rs`,
  `width` field) — the exact shape to mirror.
- **Structural sharing is free.** `Vec<Vec<Vec<f64>>>` (Canada geometry) shares its inner
  classes by hashcons — O(depth) classes, not O(depth) duplicated trees. The CITM 31-field
  instrument's repeated sub-shapes intern once.
- **Row polymorphism for document shapes.** Rows are e-classes; an open row is a fresh
  var-class ρ; field-permutation canonicalizes by a `row-sort` rewrite. `{statuses: τ | ρ}`
  unifies structurally. This is the honest representation of Twitter/CITM document shapes
  the GN-3 W3 typed grammars declare via `->` chains.

### 1.4 Let-generalization and schemes

A rule's inferred type-class, when referenced polymorphically, is *instantiated* by copying
the class subtree with fresh variable-classes (the standard "copy under a substitution").
The e-graph's structural sharing means the *scheme* is one class; each instantiation is a
shallow re-add with renamed leaf vars. For skinny's grammar plane, most rules are
monomorphic (concrete document shapes), so generalization is a bounded, opt-in facility —
not on the hot path.

### 1.5 Diagnostics-first — proof-producing closure gives span-precise errors

Every leaf e-node carries its provenance `ExprId` as a `#[language(skip)]` metadata field
(the exact pattern `GrammarENode::TokenDispatch` uses for `arms`/`fallback`,
`ir/src/egraph/node.rs:63-70`). A clash — e.g. injectivity forces `Bool` and `F64` into one
class — is detected when extraction finds a class with two irreconcilable concrete leaves,
and reported as `DeriveError::Unify { span_a, span_b, ty_a, ty_b }` naming **both** grammar
`->` sites. The union-find explain-tree / proof-forest technique (Nieuwenhuis & Oliveras,
*Proof-Producing Congruence Closure*, RTA 2005 — the basis of egg's `Explanation` feature)
reconstructs the equation chain that forced the clash, so the diagnostic can show the
*path* from one projection to the conflicting one. This satisfies §56 (no silent epsilon —
an explicit typed error) and the 1C-adjacent "every failure names its grammar span" bar
without ceding the family's e-graph center.

---

## 2. The substrate — what exists and is reused verbatim

`crates/egraph` (1885 LOC, domain-agnostic, `#![warn(missing_docs)]`, production):

- `language.rs` — the `Language` trait (`children`/`children_mut`/`map_children`);
  auto-derivable. Type e-node derives it.
- `egraph.rs` — `add` (hashcons+analysis+parent-edges, `:155`), `union` (size-weighted,
  `:202`), `rebuild` (congruence via parent re-canon → hashcons collision → union, `:251`),
  `canonicalize` (`:144`). **This is the congruence-closure engine, complete.**
- `analysis.rs` — `Analysis<N>` monotone lattice: `make` (per-node), `merge` (join +
  changed-flag for saturation detection, `:34`), `modify` (post-merge side-effect hook —
  **the injectivity propagator's home**, `:41`). `NoAnalysis` default.
- `extract.rs` — `Extractor` (greedy bottom-up fixed-point, `:134`), `CostModel<N>` with a
  `Lattice` cost type (`:36-94`), `AstSize` default, and — critically — the comment at
  `:25-41` already anticipates **multi-objective Pareto extraction** via `Scalar<T>` +
  `Lattice::dominated_by`. A `TypeCost` product lattice (concreteness ≻ width ≻ size) drops
  straight in.
- `unionfind.rs` — path-halving union-find (105 LOC). The HM variable store, already built.
- `csp_scheduler.rs` — `CspScheduler` drives saturation by modelling "which classes are
  dirty this iteration" as a `csp-solver` CSP over a `DirtyDomain` monotone lattice
  (`:1-45`). Isomorphic to the grammar tier's `csp-solver` usage. The type e-graph rides it
  unchanged — **this is the LOCK-2 engine-share the 1D charter names.**
- `cost_config.rs` / `cost_weights.rs` — one `CostConfig` substrate + `CALIBRATED_WEIGHTS`
  read by both live tiers; the type tier's domain knobs live on its own cost wrapper (the
  documented split, `cost_config.rs:16-24`), keeping `egraph` domain-agnostic.

`crates/egraph-derive` — `#[derive(Language)]` projects `Id`/`Vec<Id>`/`Box<[Id]>`/`[Id;N]`
fields to the child slice automatically (`egraph-derive/src/lib.rs:266-277`). Constraint:
multi-child variants must store children in one `[Id;N]`/`Box<[Id]>` field (`:198-212`) —
`Sum(Box<[Id]>)`, `Arrow([Id;2])`, `List(Id)`, `Row{fields: Box<[Id]>, tail: Id}` all fit.

`crates/csp-solver` — vendored solver; `domain/lattice.rs` `BitsetLatticeDomain` (monotone
`join`, AC-3 fixpoint, no backtracking) is the exact width-lattice shape; `domain/cost_finite.rs`
exists for cost domains.

---

## 3. The mirror pattern — the type tier is a copy of a proven tier

The grammar tier `crates/ir/src/egraph/` is the template, variant-for-variant:

| Grammar tier file | Type tier analogue |
|---|---|
| `node.rs` — `GrammarENode` derives `Language`, mirrors `DagNode` (`node.rs:22-72`) | `TypeENode` derives `Language`, mirrors the `Type` enum (§4) |
| `build_egraph.rs` — `insert_ir`/`insert_node` walk `IrNode` → e-classes (`:9-60`) | `insert_types` walks `ExprKind` (esp. `Project`) → type e-classes |
| `analysis/mod.rs` — `GrammarAnalysis` computes `EClassFacts` (first-set, nullable, `WidthBound`) | `TypeAnalysis` computes width-lattice + concreteness + provenance |
| `rules/mod.rs` — `default_rules` (sum/dedup/absorb families) | `type_rules` (sum-flatten/sort/absorb, row-sort, injectivity) |
| `cost.rs` — `GrammarCostModel` over `CostWeights` | `TypeCost` product lattice (concreteness ≻ width ≻ size) |
| `write_back.rs` — extract → IR | `derive.rs` — extract → `DirectSchemaSet` |

Home options, all constitution-legal (colocation, no god module, directory module §14/§15):
either a **fourth sibling under `crates/ir/src/egraph/type_node.rs` + `type_analysis/` +
`type_rules/`** (maximal engine-colocation with the two extant Language tiers), or a
directory-module `crates/codegen/src/schema/type_plane/{node,analysis,rules,solve,extract,
derive}.rs` (colocation with the schema consumer GN-3 names). The former maximizes the
"one system, three Languages" isomorphism; the latter keeps the type plane beside
`direct_schema` + `json_typed_direct`. **Recommendation: the e-node + rules + analysis live
beside the engine's other Languages (`crates/ir/src/egraph/`), and the schema-lowering
`derive.rs` lives in `codegen/src/schema/type_plane/`** — the Language is engine-tier
infrastructure; the schema bridge is a codegen concern.

---

## 4. Integration seams — exact path:line

- **The discard site 1D repurposes**: `skinny/crates/passes/src/lib.rs:204` —
  `ExprKind::Project { body, .. } => self.infer_expr(*body)?` throws away `target`+`ty`.
  1D: `Project` mints a leaf type-class from the declared `ty`/`target` (with the `ExprId`
  as provenance) and **unions** it with the body's inferred class — that union *is* the
  bidirectional check (declared type checks the synthesized body).
- **The structural inference 1D subsumes**: `passes/src/lib.rs:159-232` (`Infer`), the
  `Type` enum at `:235` (`Var(TypeVarId)`/`Builtin`/`Seq`/`Alt`/`List`/`Option`/`Rule`),
  `BuiltinTy` at `:246`. These map 1:1 to `TypeENode` variants; `Type::Var(TypeVarId)` is
  already the e-class-variable seam. The recursive `infer_expr` becomes `insert_types` +
  saturate + extract.
- **The 4-way fold that dies** (GN-3 W2): `crates/codegen/src/emit/general_descent/view.rs:20-46`
  (`ProjectionKind {Float,Bool,Tag,Span}` + `classify_projection`) — replaced by reading the
  extracted leaf type. R6 flags this as the *entire* type expressiveness reaching emission today.
- **The schema IR target** (GN-3 W1 / PROVE-08 Wall 2): `crates/codegen/src/schema/
  direct_schema.rs:52-58` (`DirectTypeKind` = one `Struct` variant). Extraction lowers
  `TypeENode::Sum` → a new `DirectTypeKind::Sum`; the byte-dispatch discriminator reuses the
  runtime's existing `at_cursor` first-byte classifier (`grammars/json/value.rs:28-47`).
- **The hand schema that dies** (GN-3 W3): `xtask/src/real_typed_schema.rs:7` (1014 LOC / 46
  struct / 19 root) → replaced by `type_plane::derive(&grammar, &typed_source)?.schema`; the
  emitter entry `codegen/src/lib.rs:130` (`emit_typed_from_source`) already takes
  `&DirectSchemaSet`, so the swap is at the caller.
- **Width lattice source**: `WidthBound` in `crates/ir/src/egraph/analysis/mod.rs`
  (`EClassFacts.width`) — mirror its lattice shape for the type width-fact.
- **Zero-runtime invariant seam**: `git diff --name-only <base>..HEAD | grep crates/runtime/`
  must be empty (the type e-graph runs at generation time only, like GrammarENode saturation).

---

## 5. Prior art, with citations

- **egg** (Willsey, Nandi, Zhang, Panchekha, Grossman, Tatlock — *egg: Fast and Extensible
  Equality Saturation*, POPL 2021). E-class analyses, deferred rebuilding, cost extraction —
  the model `crates/egraph` implements. egg's `Analysis` = this repo's `Analysis`; its
  `Extractor::find_best` = `extract.rs`; its explanations = §1.5 diagnostics.
- **egglog** (Zhang, Wang, Willsey, Tatlock — PLDI 2023) — unifies Datalog + e-graphs; its
  *sort system* is a live example of typed terms living in an e-graph (types-as-eclasses is
  not speculative). Relevant to row/sum modelling.
- **Congruence closure ⇒ unification**: Downey–Sethi–Tarjan (JACM 1980); Nelson & Oppen,
  *Fast decision procedures based on congruence closure* (JACM 1980). The injectivity /
  argument-decomposition step (§1.2) is the exact delta between CC and syntactic unification.
- **Proof-producing CC**: Nieuwenhuis & Oliveras (RTA 2005) — the explain-tree that turns a
  clash into a span-path diagnostic (§1.5).
- **rustc**: type variables live in a union-find (`ena` crate); integer/float type vars
  (`?i`/`?f`) default (i32/f64) when unconstrained — the width-lattice + minimal-cover
  extraction (§1.3) is the principled generalization of rustc's ad-hoc literal fallback.
  rustc has NO congruence engine — 1D's e-graph is union-find PLUS congruence PLUS extraction.
- **Equi-recursive types via graphs**: regular-tree/graph type representations (Cardelli;
  the OCaml `-rectypes` cyclic unifier). The e-graph's native cycles give equi-recursion for
  the recursive `JsonValue` sum with no μ-binder.
- **In-repo proof**: `crates/ir/src/egraph/*` (grammar Language + analysis + rules) and
  `bbnf_regex::egraph::RegexExtractionCost` (regex HIR Language) are two working
  consumers — the pattern is de-risked; 1D is the third instance.

---

## 6. Named risks, with counterexamples

- **R1 — congruence ≠ unification (the load-bearing risk).** Omit the injectivity
  propagator and `Sum([a,b]) = Sum([c,d])` does NOT force `a=c`. Counterexample: two
  `Arrow([dom,cod])` nodes land in one class; domains never unify; extraction picks one
  arbitrarily → unsound. **Mitigation:** the §1.2 propagator (`Analysis::modify` hook) is
  mandatory and is the prototype's first gate (§7.1).
- **R2 — ill-founded recursion.** Equi-recursion is *correct* for the intended `value`
  cycle, but `t = Sum([t])` with no non-recursive arm has no finite representative.
  **Mitigation:** extraction fails explicitly (`DeriveError::Unbounded`) when a class has no
  finite-cost node — no silent epsilon (§56). A sum must have ≥1 base arm; the JSON value
  sum has four leaf arms, so it is well-founded.
- **R3 — width modelling: constructor vs analysis-lattice.** If `Int8`/`Int16` are distinct
  *constructors*, injectivity has nothing to decompose and extraction tie-breaks
  non-deterministically. **Mitigation:** model width as an **analysis-lattice fact on one
  `Int` leaf class** (§1.3), so u8/u16 are one class whose width-fact *joins*; extraction
  reads the fact. Design decision, must be fixed before W1.
- **R4 — determinism / byte-identical regen (GN-3 W3 gate, §53).** Extraction fixed-point +
  `classes()` iteration order must be deterministic, and equal-cost representatives need a
  stable tie-break (lowest node Id / canonical variant order). Counterexample: FxHashMap
  iteration reorders → non-identical `DirectSchemaSet` → W3 RED. **Mitigation:** total order
  on `TypeENode`; deterministic tie-break in `TypeCost`; prototype asserts byte-identical
  derive across two runs (§7.4).
- **R5 — un-calibrated cost model.** `CALIBRATED_WEIGHTS` covers grammar/regex, not types.
  **Mitigation:** `TypeCost` is a *lexicographic product lattice* (concrete ≻ minimal-width ≻
  fewest-nodes), not tuned magic numbers — principled, so no calibration bench needed; the
  extractor's `Lattice`/`dominated_by` surface (`extract.rs:36-62`) already supports it.
- **R6 — LOC-vs-capability framing.** A skeptic asks whether the e-graph is heavier than
  PROVE-08's 221-LOC bespoke HM. **Answer:** the engine is *already paid for* (shipped,
  tested, two consumers). 1D's net-new code is the `TypeENode` + ~4 rewrite rules + the
  injectivity propagator + the width lattice + the schema-lowering extractor — and it reaches
  sum, recursion, width, and row cases the bespoke `TyCon` (closed leaf vocab) **cannot
  express at all**. The comparison is capability, not line-count; the bespoke core would have
  to grow all four net-new subsystems by hand (exactly GN-3 W1's +420 sum node), which the
  shared engine supplies for free.
- **R7 — zero-runtime-cost (Constitution v).** Satisfied by construction: the type e-graph
  runs in the generator, emits a `DirectSchemaSet`, adds no runtime instruction — identical
  in kind to the grammar tier's compile-time saturation. The gate is `git diff crates/runtime/`
  empty on every wave.

---

## 7. What a prototype must test to move this family

A running `EGraph<TypeENode, TypeAnalysis>` prototype, decisive experiments in order:

1. **Injectivity = unification.** Union two same-constructor classes; run the §1.2
   propagator + `rebuild`; assert positional children unified (`Sum([a,b])=Sum([c,d]) ⟹
   a=c, b=d`; `Arrow` domains unify). This is the family's existence proof — congruence
   closure promoted to unification.
2. **Recursive sum, order-invariant.** Encode `value = object|array|string|number|bool|null`
   with `object`/`array` referencing `value`; saturate sum-canon rules; extract → one
   `DirectTypeKind::Sum` of exactly 6 variants. Assert two *different source orderings*
   extract byte-identically (GN-3 R5 dissolved). Assert the cyclic reference extracts to a
   finite representative with a back-edge (Wall 2 fallen without a μ-binder).
3. **Width lattice absorbed into extraction.** Two projections `-> 200` and `-> 70000` on
   one field → width-fact joins to `u32`; extraction picks the minimal covering width.
   Proves the "CSP post-solve for widths" is the extractor's cost function, not a separate
   subsystem.
4. **Determinism / regen byte-identity.** Run `derive` twice on the same typed grammar;
   assert byte-identical `DirectSchemaSet` (the GN-3 W3 gate has real teeth here — non-
   deterministic extraction fails it).
5. **Span-precise clash.** Force `Bool` and `F64` into one class via conflicting `->`
   projections; assert `DeriveError::Unify` names *both* grammar spans (provenance survives
   through the e-graph; §1.5).
6. **Zero runtime.** `git diff --name-only <base>..HEAD | grep crates/runtime/` empty.
7. **Head-to-head vs PROVE-08's `hm.rs`.** On the leaf slice (5 arrows) the e-graph core
   reproduces PROVE-08's census (`arrows=5, rules=4, unify=1`, same resolved leaves) — and
   then reaches the sum/recursion/width/row cases the closed-`TyCon` bespoke core cannot,
   demonstrating superset capability at bounded net-new LOC.

The move that would *retire* this family: if experiment 1 (injectivity) proves unstable —
e.g. the propagator's own fixpoint interacts pathologically with the width-lattice join and
fails to converge deterministically — then the e-graph's advantage over a bespoke unifier
narrows to "free sum + recursion," which a sibling family may reach more cheaply. Experiments
1, 3, and 4 together are the family's make-or-break.
