# LOCK-1 · Family 1D — TYPES-AS-ECLASSES · pass-2 research (D)

The type lattice lives in an e-graph; unification is congruence closure; projections
canonicalize by saturation; extraction picks representative types by cost. The claim of
this family is stronger than "use an e-graph as a solver": the e-graph substrate already
in-tree (`crates/egraph`) **is** a Hindley–Milner engine, and re-authoring a separate
union-find HM core is redundant. Everything below is anchored to that substrate and to the
real record/sink seams.

---

## 1. The substrate is already an HM engine — do not re-author union-find

`crates/egraph/src/egraph.rs` is a hash-consed congruence-closure engine:

- `add(node)` (`egraph.rs:155`) canonicalizes children to representatives, then interns
  against `memo` (`:157`) — this is **term hash-consing**: two structurally-equal type
  terms collapse to one e-class automatically.
- `union(a,b)` (`egraph.rs:202`) is the union-find merge; `rebuild()` (`egraph.rs:251`)
  re-canonicalizes parents and re-memoizes until fixed point — **this is congruence
  closure**: after `union(u32_a, u32_b)`, every `Vec(u32_a)` and `Vec(u32_b)` parent
  merges automatically through the memo-collision path (`egraph.rs:262-266`).

HM unification over a free term algebra *is* congruence closure plus occurs-check
(Downey–Sethi–Tarjan value numbering; Nelson–Oppen 1980; the egg paper §2 makes the
identity explicit). Therefore:

> **1D's core assertion.** A type variable is a fresh e-class; `unify(t1,t2)` is
> `egraph.union(t1,t2)`; substitution is `find`; the occurs-check is the only piece not
> free (the e-graph is happily cyclic — it must be gated, §7 R3). The proven 221-LOC HM
> core in `prototypes/PROVE-08/hm.rs` becomes **net-zero**: its union-find + `TyCon`
> binding are exactly `unionfind.rs` + `memo`. 1D deletes it rather than porting it.

The `Language` node type (`language.rs:16`) and its derive (`egraph-derive/src/lib.rs`,
auto-projects `Vec<Id>`/`Box<[Id]>`/`Id` to a child slice) give the type language for free:

```rust
#[derive(Clone, Eq, PartialEq, Hash, egraph_derive::Language)]
enum TypeNode {
    Bool, F64, U8, U16, U32, U64, I8, I16, I32, I64, // width leaves (never inter-unify, §7 R2)
    Span, StringDecoded,                              // decode-class leaves
    Var,                                              // fresh e-class = type variable
    Opt(Id), Vec(Id),                                 // single-child (auto)
    Tuple(Box<[Id]>),                                 // positional, order-SENSITIVE
    Field { key: StringId, val: Id },                 // key is a leaf label, val is a child
    Row(Box<[Id]>),                                   // variadic of Field, order-canonical
    Sum(Box<[Id]>),                                   // variadic of variant, order-canonical
}
```

The existing in-tree consumer proving this exact pattern end-to-end is
`skinny/crates/passes/src/backend_egraph.rs` — `EGraph` + `Extractor` + `CostModel` +
`Lattice` selecting a `BackendShape`. 1D is the type-theoretic twin of that decision graph.

---

## 2. Thread D1 (make-or-break) — deriving `Row` from `pair*` WITHOUT a `{}`/`[]` hint

The recon (`R6-value-type-plane.md §5c`, `passes/src/lib.rs:1403-1490`) shows today's
record/list split is *heuristic byte-matching*: `{`+`}`→container, `[`+`]`→sequence. 1D's
make-or-break is to replace that with **an e-class `Analysis` lattice that classifies by
inference**, so the discriminator is a property of the child key-classes, not of the
bracket literals.

Mechanism. Give the type e-graph an `Analysis<TypeNode>` (`analysis.rs:18`) whose `Data`
is a monotone `KeyShape` lattice computed at `make` and joined at `merge`:

```
KeyShape = Bottom
         | ClosedKeys(SortedSet<StringId>)   // every child Field.key is a distinct literal
         | OpenKey(leaf)                       // the key position is a Regex/Span leaf class
         | Conflict                            // saw both -> partial record (the hard case)
```

- `Row(fields)`: `make` folds the children's key-classes. If each `Field.key` resolves to
  a **singleton literal** e-class and the set is duplicate-free → `ClosedKeys` → the class
  extracts as `Row`. If the key position is one **homogeneous Regex/Span leaf** (runtime
  string keys, the JSON `pair*` case) → `OpenKey` → the class extracts as `Vec<(K,V)>` /
  `Map` (a *list of entries*, not a record).
- The join (`merge`, `analysis.rs:34`) is the lattice lub; `ClosedKeys ⊔ OpenKey =
  Conflict`. This is what distinguishes `object` from `array` *without ever reading `{`* —
  an `array` body is `Vec(elem)` (no `Field` children at all → not a `Row` candidate); an
  `object` body is `Row`/`Map` decided by whether its `Field.key` classes saturate to a
  closed literal set.

> **Answer to D1, pre-registered.** A per-grammar hint is **not** required *when the typed
> grammar's `->` chains carry the key literals* (closed record) or a key regex (open map):
> the closed-vs-open key-class is the discriminator, derived by saturation. A hint **is**
> required only for the `Conflict` point — a document with *both* fixed and runtime keys
> (JSON-LD `@type` + arbitrary members; a struct with an overflow map). The prototype must
> report whether `Conflict` collapses to `Map-with-known-fields` soundly or forces an
> annotation. That single lattice point is the honest make-or-break — everything else is
> hint-free.

Counterexample the prototype MUST author (§7 R1): `object = ( ("id" -> …) | (string ->
…) )*` — a closed field beside an open one. If the analysis silently picks `Row` it drops
the open members; if it picks `Map` it loses the typed `id`. The verdict on this fixture is
the family's decisive evidence.

---

## 3. Thread D2 — the shared-`union` scheduling amendment across three tiers

The amendment already exists in-tree as `CspScheduler` (`csp_scheduler.rs:204`): it models
"which e-classes to re-probe" as a CSP over `DirtyDomain` (a boolean lattice, `:56`) with
one `ParentDirtyProp` constraint per child→parent edge (`:120`), and computes the dirty
transitive closure with a single `csp.propagate()` (`:235`) — AC-3 worklist. This is the
`union`-scheduling amendment; 1D's task is to route it through the **type tier** and
measure it against the grammar and regex tiers under one saturation harness.

Per-tier prediction, to be measured and then landed or retired in-tree:

- **Grammar tier** (rule-rich `EGraph`, the `backend_egraph.rs` consumer): CSP wins — the
  dirty frontier is a small subtree, `build_csp` amortizes over dozens of iterations.
- **Regex tier** (LOCK-5's HIR canon): CSP likely wins on large alternations, marginal on
  small literals.
- **Type tier** (this family): a grammar's type e-graph is *tiny* — a few dozen classes.
  `build_csp` rebuilds the whole propagation graph **every iteration** (`:229`, comment
  `:222-228`), O(classes+edges) each time; that fixed cost plausibly **exceeds** the
  dirty-skip saving at this scale. Pre-registered verdict: **retire CSP scheduling for the
  type tier, keep `BackoffScheduler` (`scheduler.rs:70`); the amendment lands only where
  the frontier-skip pays** — a per-tier decision, not a global one. The measurement is
  iterations + final_nodes + wall for each `Scheduler` impl on each tier (both are already
  swappable behind the `Scheduler` trait, `scheduler.rs:43`).

---

## 4. Thread D3 — AC-`Sum` canonicalization and positional injectivity, co-resident

Two disciplines must share one `EGraph<TypeNode>` saturation:

- **AC-`Sum`** (order-*insensitive*): `object|array|string|number|bool|null` must unify to
  the same e-class regardless of author order (GN-3 R5). egg has **no native AC**; adding
  commutativity/associativity as rewrite rules is the classic exponential blowup (n! e-node
  copies of a 6-arm sum — the counterexample that kills the naive route). The confluent
  route is **canonical-by-construction**: `Sum` sorts its child `Id` slice *before*
  `add`/hash-cons, so all permutations intern to one memo key at `egraph.rs:157`. This is
  the egglog canonical-n-ary approach (arXiv 2304.04332) and it needs **no rewrite rule**.
- **Positional `Tuple`** (order-*sensitive*): `Tuple([a,b])` and `Tuple([b,a])` are
  distinct types and must remain distinct e-classes. Because `Tuple`'s child slice is
  **not** sorted, hash-cons keeps them apart naturally.

Co-residence is sound because AC-ness is a **per-constructor** property carried by whether
that variant sorts at construction — both live under one `EGraph<TypeNode>`, one
`rebuild()`, one saturation. Confluence: sorting-at-construction is a deterministic normal
form (idempotent), so the e-graph is confluent w.r.t. Sum ordering; positional injectivity
is preserved because no rewrite ever reorders a `Tuple`. The single soundness hazard to
pre-register (§7 R3): a rewrite rule that tries to canonicalize `Tuple` as if it were `Sum`
would collapse `(A,B)` with `(B,A)` — forbidden by construction (distinct variants), and
the prototype asserts it by counterexample.

---

## 5. Open R — engine-agnostic record widening (the `List ≠ Row` closure)

The real greenfield record is `bbnf_ir::TypeDesc` (`crates/ir/src/types/type_desc.rs:13`).
It has `Vec`, `Tuple`, `Named`, and — critically — `HeterogeneousAltJoin(Vec<TypeDesc>)`
(`:48-63`), which is *exactly the masked fallback the directive names*: the type-projection
CSP emits it when an `Alt`'s branches disagree, and it "expands to the same `Box<Enum>`
layout that `BoxedEnum` expanded to" (`:54-56`). There is **no `Sum` and no `Row`** — so a
tagged sum and a keyed record are both currently laundered through `HeterogeneousAltJoin` /
`Box<Enum>`, losing `List ≠ Row`.

Widening (the concrete seam):

```rust
// crates/ir/src/types/type_desc.rs:63 — beside HeterogeneousAltJoin
Sum(Vec<TypeDesc>),                    // tagged, AC-canonical variant set
Row(Vec<(StringId, TypeDesc)>),        // keyed record, distinct from Vec/Map
```

- **Any engine populates it.** The e-graph extracts a winning class → `TypeNode::{Sum,Row}`
  → `TypeDesc::{Sum,Row}`; but so can the *running CSP* (`passes::types::constraint::
  revise::join_types`) once `TypeDesc` carries the variants — that is the point of the
  directive: structural fidelity becomes **orthogonal to engine choice**. The e-graph is
  one populator, the sharpest one, not the only one.
- **`List ≠ Row` by construction.** `Vec(elem)` and `Row(fields)` are distinct
  constructors; congruence closure never merges distinct constructors, so a list and a
  record can never collapse — the exact property `HeterogeneousAltJoin`→`Box<Enum>` erases.
- **The lowering sink (XL-F).** Both records lower through
  `TypeAnalysis::from_ir` (`crates/core/src/backend/driver/analysis.rs:22`, copies
  `ir.types` + `type_map.scratch_types`) and the second `from_ir` at
  `crates/core/src/backend/rust/view/named_types.rs:100`; the TS mirror is
  `backend/ts/projection.rs`. Adding `Sum`/`Row` to `TypeDesc` requires arms in each — the
  directive's "names BOTH its LOCK-3 sink and its TS mirror" is discharged here.
- **Does it close the masked fallback?** Yes: a heterogeneous alt that is really a tagged
  union extracts as `Sum` (not `HeterogeneousAltJoin`), and an object extracts as `Row`
  (not `Vec`/`Box<Enum>`). The `json_templates` / general-descent 4-way `ProjectionKind`
  fold (`R6 §5b`, `general_descent/view.rs:20-46`) is subsumed because the leaf now arrives
  typed. The prototype must show the heterogeneous-alt corpus stops producing
  `HeterogeneousAltJoin` once `Sum` exists.

---

## 6. Open X and Open S — the e-graph gives both seams for free

- **Open X (pluggable residue).** Under-determined widths (`-> u_` between u8/u16/u32) are
  **not** a union problem — unioning `U8` with `U16` is a constructor clash (they are
  distinct leaves, §7 R2). They are an **extraction** problem: a fresh `Var` class holds
  all admissible widths as candidate e-nodes, and the `CostModel`/`Extractor` (already the
  seam in `backend_egraph.rs`) picks one. That makes the residue solver *pluggable by
  construction*: **one seam = the `CostModel` trait; two implementations** = (a) a
  defaulting-*lattice* cost that ranks narrower widths cheaper (ordered-lattice defaulting),
  (b) a *search* via `CspScheduler` extraction. Decide on the width micro-fixture by
  swapping only the `CostModel` — independent of any equality carrier. That is Open X's
  "one seam, two swappable implementations" delivered natively.
- **Open S (attribute-grammar seam).** `Analysis<TypeNode>` (`analysis.rs:18`) *is* an
  attribute engine: `make` computes a **synthesized** attribute bottom-up; a bidirectional
  check pushing an expected type down is the **inherited** direction; the fixed-point over
  `analysis_pending` in `rebuild` (`egraph.rs:252`, `:237`) is the **circular** attribute
  solved by the monotone `merge` returning `changed` — the LLVM-style changed-flag
  convergence, not a structural hash. SCC-generalize, federation-sequencing, and
  bounded-fixpoint all become "the `merge` lattice reached fixed point" — one saturation
  discipline, not a fifth engine. `KeyShape` (§2) is the first instance of this attribute.

---

## 7. Named risks & counterexamples (pre-registered)

- **R1 — the `Conflict` key-shape (mixed record).** `{ "id":…, <string>:… }` sits at the
  lattice top where closed and open keys meet. If the analysis is a 3-point lattice it must
  choose `Map-with-typed-fields` or force a hint. **This is the D1 make-or-break fixture**;
  a family that reports "hint-free" without exercising it is unproven.
- **R2 — widths are not unifiable.** The e-graph must **not** union distinct width leaves;
  doing so yields an inconsistent (multi-constructor) class. Widths are `Var` + cost
  extraction (Open X), not equality. Counterexample: `union(U8,U16)` then a
  `Vec(U8)`/`Vec(U16)` parent collision merges *containers of different widths* — silent
  corruption. The prototype asserts width classes never inter-union.
- **R3 — cyclic types / occurs-check.** The e-graph is cheerfully cyclic; HM needs an
  occurs-check to reject `t = Vec(t)`. Congruence closure won't catch it. Gate: a rewrite/
  analysis that flags a class reachable from its own child, OR restrict recursion to
  `Named` rule references (the natural fixpoint boundary) — the latter is preferred and
  matches how recursive grammars already close over rule names.
- **R4 — AC blowup if done by rewrite.** Adding commutativity as a rule instead of
  canonical-sorted-children reintroduces n! Sum copies. Counterexample: the 6-arm `value`
  sum. The family's route (sort-at-construction) must be the one tested; a rewrite-based AC
  is a named negative result.
- **R5 — zero-runtime-cost invariant.** The type e-graph runs at generation time only
  (Constitution v; GN-3 §1.4). A `git diff` over the runtime crate must be empty on every
  carrier wave; any runtime instruction refutes the family regardless of its own gate.

---

## 8. What a prototype must test to move this family

1. **Row-from-`pair*` without a bracket hint.** Build `TypeNode` (derive), populate from a
   typed `object = (pair)*` grammar in two variants — (a) closed literal keys, (b) open
   `string` key — saturate, assert the `KeyShape` analysis extracts (a)→`Row`, (b)→`Map`/
   `Vec<(K,V)>` with **zero `{`/`[` branch**. Then run the R1 mixed fixture and report
   whether `Conflict` resolves soundly or needs an annotation. (Answers D1.)
2. **AC-`Sum` ∥ positional `Tuple` co-resident.** Intern a 6-arm `Sum` in two orders →
   assert **one** e-class; intern a `Tuple` in two orders → assert **two** e-classes; both
   under one `EGraph<TypeNode>` saturation; assert no `Tuple` was reordered. (Answers D3;
   confluence + soundness.)
3. **`List ≠ Row` through the sink.** Add `TypeDesc::{Sum,Row}` (`type_desc.rs:63`), extract
   winning classes into them, thread `TypeAnalysis::from_ir` (`analysis.rs:22`) +
   `named_types.rs:100` + TS `projection.rs`; assert the heterogeneous-alt corpus stops
   emitting `HeterogeneousAltJoin`/`Box<Enum>` and that a list and a record never share a
   class. (Answers Open R.)
4. **Scheduler amendment, per tier.** Run `CspScheduler` vs `BackoffScheduler` on the type
   e-graph; report iterations, final_nodes, wall; land or retire per tier. (Answers D2 —
   pre-registered: retire on the type tier, keep on grammar.)
5. **Width residue behind one `CostModel` seam.** On a `-> u_` under-determined fixture,
   swap a lattice-order `CostModel` for a `CspScheduler`-search extractor with no other
   change; report which the fixture selects. (Answers Open X.)

Evidence bar: MEASURED = landed, HEAD-reproducible; every carrier wave shows the runtime
`git diff` empty (R5). The union-find HM core is deleted, not ported — the e-graph is the
engine.

---

## 9. Prior art (cited)

- **egg: Fast and Extensible Equality Saturation**, Willsey et al., POPL 2021 —
  e-class analysis (the `Analysis` lattice here), rebuild-based congruence closure (the
  substrate), cost-model extraction. `https://dl.acm.org/doi/10.1145/3434304`.
- **Better Together: Unifying Datalog and Equality Saturation** (egglog), Zhang et al.,
  PLDI 2023 — semi-naive saturation and the canonical-n-ary form for AC operators (the
  sort-at-construction route in §4). `https://arxiv.org/pdf/2304.04332`.
- **Downey, Sethi, Tarjan, "Variables and common subexpression elimination" (1980)** and
  **Nelson–Oppen congruence closure (1980)** — value numbering ≡ congruence closure ≡ HM
  unification over a free algebra (§1's identity).
- **Rémy; Leijen, "Extensible records with scoped labels" (2005)** — row polymorphism; the
  `Row`/`KeyShape` discriminator in §2.
- **Rewrite Rule Inference Using Equality Saturation**, Nandi et al., OOPSLA 2021 —
  analysis-guided rule firing (the `should_apply` guard, `rewrite.rs:53`).

In-tree substrate: `crates/egraph/src/{egraph.rs:155/202/251, analysis.rs:18, language.rs:16,
scheduler.rs:70, csp_scheduler.rs:204}`, `crates/egraph-derive/src/lib.rs`; existing
consumer `skinny/crates/passes/src/backend_egraph.rs`; record `crates/ir/src/types/
type_desc.rs:13-63`; sink `crates/core/src/backend/driver/analysis.rs:22` +
`rust/view/named_types.rs:100`; HM seed to delete `prototypes/PROVE-08/hm.rs`.
