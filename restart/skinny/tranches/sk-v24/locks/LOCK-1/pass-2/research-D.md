# LOCK-1 · Family 1D — TYPES-AS-ECLASSES · pass-2 research

The type-theoretic center: the type lattice lives in an e-graph; unification is
congruence closure; projections canonicalize by saturation; extraction picks the
representative type by cost. This family shares the LOCK-2 engine and homes the
*type-tier* Language, analysis, and rewrite set. The claim it must earn: record-vs-list,
sum canonicalization, row polymorphism, and width/overload residue are all one
saturation over one type e-node, populating `List ≠ Row` and lowering it through the
LOCK-3 sink at zero runtime cost.

Substrate reality: the engine already exists and is production-grade. Nothing in this
family needs a new e-graph — it needs one new Language, one new Analysis, and a small
rewrite set, all mirroring an established in-tree pattern.

---

## 1. The substrate that already exists (the family builds on these, not on a strawman)

**The engine** — `crates/egraph/`, 4.8k LOC total across grammar+ir e-graph homes:
- `egraph.rs:155 add` / `:202 union` / `:251 rebuild` — hash-cons `memo: FxHashMap<N,Id>`
  (`egraph.rs:32`) IS congruence closure: structurally-equal nodes intern to one class;
  `rebuild` re-canonicalizes children after unions, re-memoizes, and propagates parent
  edges to fixpoint. This is exactly congruence closure over a type signature — the
  family's "unification = congruence closure" thesis is a rename of existing behavior.
- `analysis.rs:18 trait Analysis<N>` with `make` (`:26`, bottom-up e-node fact),
  `merge` (`:34`, monotone lattice join returning changed-bool), and — load-bearing —
  `modify` (`:41`, the post-union hook egg uses for analysis-driven unions and clash
  detection). `modify` is where positional injectivity and width defaulting live.
- `extract.rs:36 trait Lattice` / `:85 CostModel<N>` / `:115 Extractor` — cost-guided
  extraction already multi-objective-ready (`Lattice` bound anticipates dominance). This
  IS "extraction picks representative types by cost."
- `csp_scheduler.rs:204` — `CspScheduler` is generic `<N: Language, A: Analysis<N>>`;
  it drives saturation by modelling the dirty frontier as a `csp-solver` problem
  (`DirtyDomain` lattice + `ParentDirtyProp` AC-3 closure). Already shared across tiers.
- `scheduler.rs`, `rewrite.rs:RewriteFn/run_on_dirty`, `cost_config.rs`,
  `cost_weights.rs`, `csp_scheduler`/`unionfind.rs` (path-halving) round it out.

**The derive** — `egraph-derive/src/lib.rs`: `#[derive(Language)]` auto-classifies fields
by type — `Id → SingleId`, `Box<[Id]>`/`Vec<Id>` → `SliceId`, `[Id;N]` → `ArrayId`,
everything else → `Leaf` (`lib.rs:266-277`). A recursive enum whose recursive fields are
`Id`-shaped derives `Language` with zero hand-written glue. The one constraint
(`lib.rs:198-212`): multiple recursive fields in one variant must be a single `[Id;N]`
— e.g. a `Sum` with two positional payloads packs `[Id;2]`.

**The established mirror pattern** — `crates/ir/src/egraph/node.rs`: `GrammarENode`
mirrors the tree `DagNode` variant-for-variant with `Id` children and derives `Language`.
`analysis/mod.rs`: `GrammarAnalysis: Analysis<GrammarENode>` computes first_set /
nullable / width / elidable / fixed_shape as monotone lattice facts, bottom-up in `make`,
joined in `merge`. This is the exact template the type tier replicates one tier up.

**The type vocabulary** — `crates/ir/src/types/type_desc.rs`: `TypeDesc` (recursive:
`Vec(Box)`, `Option(Box)`, `Tuple(Vec)`, scalars, `Named(StringId)`, `Enum`,
`BoxedEnum`, and `HeterogeneousAltJoin(Vec<TypeDesc>)` at `:63` — the running pass's
alt-disagreement obligation). It has a **`TypeDescInterner`** (structural hash-consing;
tests at `tests/egraph/type_desc_interner.rs`) — congruence *without* union-find or
rewrite. Crucially **absent: `Sum` and `Row`** (GN-3 Wall 2, `direct_schema.rs:52-58`).

**The running baseline (race target, not strawman)** — `crates/ir/src/passes/types/
constraint/`: `TypeDomain` CSP variables, `alt.rs:60`/`revise.rs:116 join_types` folds an
Alt's disagreeing branches to `HeterogeneousAltJoin`. `passes/csp_domains.rs` runs
Bool/CharSet domains through the same `csp-solver`. This is the constraint store the
convergent loop pits family 1D against on derive wall-time + runtime-diff + LOC.

**The LOCK-3 sink (XL-F, the lowering that must win)** —
`crates/core/src/backend/driver/analysis.rs:22 TypeAnalysis::from_ir`; and
`backend/rust/view/named_types.rs:100 RustNamedTypes::from_ir` which infers a
scalar-tuple shape from a rule body via `infer_named_shape`. TS mirror:
`backend/ts/projection.rs`. Whatever type record family 1D produces must lower through
these two `from_ir` calls. `registry/struct.rs:58 LayoutKind` (Struct / TaggedEnum /
UntaggedEnum / NewtypeWrapper) is the layout discriminator — and today record-vs-list is
a **structural match on IrNode family** (`Alt`→enum, `Seq`→struct, `Repeat` inner→Vec,
`struct.rs:38-72`), never an inference. That match is what Thread D #1 replaces.

---

## 2. The mechanism made concrete

### 2.1 The type e-node (`TypeENode`) + derived Language

A colocated directory-module `crates/ir/src/egraph/type_tier/` (node.rs, analysis.rs,
rules.rs, lower.rs; tests in `crates/ir/tests/egraph/`), mirroring `egraph/node.rs`.
`TypeENode` is `TypeDesc` with `Id` children so `#[derive(Language)]` applies verbatim:

```
enum TypeENode {
  Span, F64, Bool, I8..U64,                 // leaves (unchanged vocabulary)
  Option(Id), Vec(Id),                      // Box<TypeDesc> → Id
  Tuple(Box<[Id]>),                         // Vec<TypeDesc> → Box<[Id]>
  Named(StringId),                          // leaf
  Sum(Box<[Id]>),                           // NEW — AC variant set, ids sorted canonical
  Row { fields: Box<[Id]>, open: bool },    // NEW — RowField ids; open ⇒ runtime keys
  RowField { key: StringId, ty: Id },       // NEW — one row entry (packs as [Id;1]+leaf)
  Rec(Id),                                  // NEW — equirecursive binder (value ⊇ object ⊇ value)
}
```

`Sum`/`Row` cure Wall 2; `Rec` gives the recursive `value = object | array | …` sum a
finite e-class home (the child cycle closes through the shared class id, which the memo
table already deduplicates — recursion is a *back-edge in the e-graph*, not an infinite
tree). Equirecursive congruence is exactly what an e-graph expresses natively that the
`TypeDescInterner` cannot (interning a cyclic `TypeDesc` diverges; interning a cyclic
`TypeENode` terminates because children are ids). **This is the singular reason the type
tier wants the e-graph and not the existing interner.**

### 2.2 Unification = congruence closure = `union` + `rebuild`

Unifying two rule types is `egraph.union(ta, tb)` followed by `rebuild`. Two Alt branches
that project to the same shape land in one class automatically (hash-cons); genuinely
disagreeing branches trigger the clash path (§2.4). Row polymorphism: a `Row{open:true}`
unifies with a `Row{fields ⊇}` by field-wise congruence over the shared keys and a fresh
open tail — implemented as an analysis-driven union in `modify`, not a bespoke solver.

### 2.3 The record-vs-list Analysis (Thread D #1, the make-or-break)

`TypeShapeAnalysis: Analysis<TypeENode>`, `Data = ShapeFact`:

```
struct ShapeFact {
  kind: ShapeKind,          // Scalar | List(elem) | Record(RowFact) | SumFact | Unknown
  key_carrying: bool,       // does the repeated inner carry a key leaf?
  key_literal: bool,        // is that key a Literal (closed) or Regex-string (open)?
  homogeneous_value: Option<Id>,
}
```

`make` derives `kind` from **body structure, never from brace bytes**:
- `Vec(inner)` / `Repeat{inner}` where `class(inner).kind == Record-pair` (an inner whose
  fact is a 2-tuple of a string-key leaf + a value) ⇒ **`Record(RowFact)`**.
- `Vec(inner)` where `class(inner).kind` is a bare value ⇒ **`List(elem)`**.
- `Sum(children)` ⇒ `SumFact` (AC-canonicalized, §2.4).

The discriminator is *the inner carries a key leaf*, recovered from the grammar's own
`pair = string ":" value` — present whether the delimiters are `{}` or `[]` or nothing.
No `has_rule("value")` name-gate (`R6 §6`, `runtime_generator.rs:99`), no `{`/`}` literal
match (`passes/src/lib.rs:1433-1448`). **Record-vs-list falls out of inference.**

The honest boundary the prototype must report: *record-vs-list* needs no hint, but
*closed-named-struct vs open-map* is a second inference on the key leaf — `key_literal`
(`"statuses" ":"`) ⇒ closed `Row` with named fields; `Regex(String) ":"` ⇒ open
`Row{open:true}` = a map `{[k:String]: V}`. A generic `json.bbnf` has runtime keys, so it
derives an **open map**, never the 46-struct document taxonomy (this is GN-3 Wall 1 /
PROVE-08 §4, restated *soundly*: the shape the grammar declares is the shape you get). The
family's cure to Wall 1 is not "infer harder" — it is per-document typed grammars whose
literal keys make the row closed. The e-graph's contribution over the running pass: the
open-map type `Row{open}` is a *first-class node* that lowers to a real map layout instead
of falling back to the structural `json_templates` (Open R).

### 2.4 AC-sum canonicalization + positional injectivity, co-resident (Thread D #3)

Both constraints live on one `Sum` class under one saturation:
- **AC canonicalization** is enforced in `make`: `Sum(children)` sorts child ids by their
  extracted discriminant before interning, so `object|array|string|number|bool|null` and
  any re-ordering hash-cons to the *same* class (GN-3 R5). Order-insensitivity is a
  normalization at construction — associative because `Sum` is flat (nested sums splice
  in a `FlattenSum` rewrite, the AC analog of `ConcatLiterals`, `rules/universal.rs:13`),
  commutative because ids are sorted.
- **Positional injectivity** is a *guard in `modify`* (post-union): when two variant ids
  in a `Sum` resolve to the same discriminant slot but distinct payload classes, that is a
  clash — surfaced as `DeriveError::Unify` / a `HeterogeneousAltJoin` obligation
  (`type_desc.rs:63`, `revise.rs join_types`), never a silent `BoxedEnum` (no-silent-epsilon).
- **Confluence / soundness**: the two commute because they act on orthogonal facets —
  AC-canon reorders (a total order on tags), injectivity rejects (a partial function
  tag→payload). Canonicalizing first then checking injectivity, or checking then
  canonicalizing, reach the same normal form because sorting does not change the tag→payload
  relation. The prototype must *demonstrate* this: two orderings ⇒ one class (confluence),
  and a same-tag-distinct-payload pair ⇒ one clash (soundness), both in a single e-graph,
  not two.

### 2.5 The shared-engine union-scheduling amendment (Thread D #2)

`CspScheduler` already saturates by tracking node-growth per class, but on *pure-union*
iterations (unions that add no nodes) it falls back to "everything dirty"
(`csp_scheduler.rs:311-315`). The amendment: track union-touched classes precisely via the
`Analysis::modify`/post-union hook, feeding them as the next iteration's seed so the dirty
frontier stays tight even when a wave is union-heavy — which the *type* tier is (unify is
almost all unions, few new nodes). Because the scheduler is already generic over
`<N: Language, A: Analysis<N>>`, the amendment lands once and serves grammar (regex-algebra
rules, `rules/regex.rs`), regex, and type tiers. Measure per-tier saturation cost
(tight-union-frontier vs conservative fallback) and land-or-retire in-tree; the type tier
is the tier that most rewards it, so it is the strongest evidence carrier.

### 2.6 Zero runtime cost + LOC-3 lowering (Open R)

The whole tier runs at generation time (constitution v; PROVE-08 §3 held zero runtime
delta). Extraction emits a `TypeDesc` (grown with `Sum`/`Row`) into `ir.types`; the
existing `TypeAnalysis::from_ir` (`analysis.rs:22`) and `RustNamedTypes::from_ir`
(`named_types.rs:100`) lower it, with `LayoutKind::Row` added beside the four existing
kinds (`struct.rs:58`) and `DirectTypeKind::{Sum,Row}` beside `Struct`
(`direct_schema.rs:52`). The record-widening is *engine-agnostic*: the running CSP pass
could populate `List ≠ Row` into the same widened vocabulary — so structural fidelity
becomes orthogonal to engine choice (Open R's thesis, which family 1D satisfies by making
the *vocabulary*, not the *engine*, the carrier of `List ≠ Row`).

---

## 3. Integration seams (path:line)

| Seam | Where | Role for 1D |
|---|---|---|
| Language derive | `crates/egraph-derive/src/lib.rs:266` | `TypeENode` derives for free once children are `Id` |
| Mirror template | `crates/ir/src/egraph/node.rs:23` `GrammarENode` | copy pattern for `type_tier/node.rs` |
| Analysis template | `crates/ir/src/egraph/analysis/mod.rs:39` | copy pattern for `TypeShapeAnalysis` |
| `modify` hook | `crates/egraph/src/analysis.rs:41` | positional injectivity + row-union |
| Congruence | `crates/egraph/src/egraph.rs:155,202,251` | unify = add/union/rebuild |
| Scheduler | `crates/egraph/src/csp_scheduler.rs:204,311` | union-frontier amendment site |
| Extraction | `crates/egraph/src/extract.rs:85,115` | cost-picked representative type |
| Type vocab | `crates/ir/src/types/type_desc.rs:13,63` | add `Sum`/`Row`/`Rec`; `HeterogeneousAltJoin` = clash sink |
| Interner (superseded) | `type_desc_interner.rs` | cyclic interning diverges → the reason for e-graph |
| Running baseline | `crates/ir/src/passes/types/constraint/{alt.rs:60,revise.rs:116}` | race target `join_types` |
| Layout discriminator | `crates/ir/src/registry/struct.rs:58` | add `LayoutKind::Row`; delete brace-match |
| LOCK-3 sink | `crates/core/src/backend/driver/analysis.rs:22` + `rust/view/named_types.rs:100` | must lower `Row`/`Sum` |
| TS mirror | `crates/core/src/backend/ts/projection.rs` | TS lowering of same |
| Name-gate to kill | `passes/src/lib.rs:1433-1448` (`{`/`}` match); `runtime_generator.rs:99` | inference replaces both |

---

## 4. Prior art (citations)

- **egg** (Willsey et al., POPL 2021, *egg: Fast and Extensible Equality Saturation*):
  `Analysis` with `make`/`merge`/`modify`; e-class analyses for constant folding and
  interval facts. The in-tree `Analysis` trait is a faithful port; type facts are a
  textbook `modify`-driven analysis.
- **egglog** (Zhang et al., PLDI 2023): unifies datalog + e-graphs; congruence + lattice
  analyses in one engine — the model for "one saturation, many facts co-resident" (D #3).
- **Row polymorphism**: Rémy (1989) / Gaster–Jones (1996) *scoped labels* / the PureScript
  and Ur/Web row systems. `Row{fields, open}` is the presence/absence row with an open
  tail; unifying open rows = the row-tail unification these systems formalize.
- **Equirecursive types** via congruence: Considine; the coinductive subtyping of Amadio–
  Cardelli (1993). An e-graph's back-edge-through-shared-id is the standard finite
  representation of a regular (cyclic) type — precisely why `Rec` terminates where the
  interner diverges.
- **AC rewriting / ordered rewriting**: Bachmair–Plaisted; the AC-canonicalization of
  flat commutative operators is standard (sort children under a total order); `FlattenSum`
  + id-sort is the AC-normal-form of `Sum`.
- **Hindley–Milner as union-find**: the proven PROVE-08 core (`hm.rs`, 221 LOC,
  `patch.diff:230-456`) — union-find `find` with path halving, constructor-binding
  `unify`, honest clash. The e-graph's `unionfind.rs` + hash-cons subsumes it: HM unify is
  the *scalar-leaf* fragment; the e-graph adds `Sum`/`Row`/`Rec` congruence the flat HM
  core cannot express. The family carries PROVE-08's leaf vocabulary verbatim and extends.
- **rustc / GHC**: rustc's `TyKind` interned in a type-arena with structural sharing =
  the `TypeDescInterner` today; GHC's coercion/OutsideIn defer hard constraints — the
  analog of a bounded post-solve residue (widths/overloads) after the decidable core.

---

## 5. Named risks + counterexamples

- **R-D1 — the map/record boundary is real, not a defect to engineer away.** Generic JSON
  keys are runtime strings ⇒ the honest derived type is `Row{open}` (a map), never the
  46-struct taxonomy (GN-3 Wall 1, PROVE-08 §4, VERIFY CONFIRMED). The prototype must NOT
  claim to infer closed structs from a generic grammar; it claims to (a) classify
  record-vs-list with no hint, and (b) derive a *sound open map* where the running pass
  falls back to the structural `json_templates`. Overclaiming here is the sharpest failure.
- **R-D2 — equirecursive termination.** A naive `TypeENode` builder that recurses
  `value → object → value` before closing the id back-edge diverges. The build must be the
  standard two-phase e-graph construction (allocate class ids first, then wire children),
  or `Rec` insertion must break the cycle explicitly. Counterexample: `value = object |
  array | value` (a grammar author's typo) must produce a clash or a finite `Rec`, never a
  hang.
- **R-D3 — AC-canon vs extraction cost interaction.** Sorting `Sum` children by
  discriminant in `make` fixes canonical form, but extraction cost
  (`extract.rs CostModel`) might prefer a different variant *order* for codegen (e.g.
  most-frequent-first byte dispatch). If canonicalization and cost disagree, extraction
  must re-order at lowering, not fight the e-class. The prototype must show the two are
  separable.
- **R-D4 — the running store may match at lower cost.** Thread B's hardened constraint
  store solves the same `List ≠ Row` with a smaller diff (no new Language, no new e-node
  home). Family 1D must justify the e-graph on the *equirecursive + AC-sum + co-resident
  injectivity* trio that the flat store cannot express confluently — if the corpus never
  exercises recursion or sum-reordering, the e-graph is unpaid-for machinery (the KISS /
  consumer-less-substrate edict, Y.13). The wall-time race is the adjudicator.
- **R-D5 — `HeterogeneousAltJoin` double-home.** The running pass already emits this
  obligation (`type_desc.rs:63`). If the e-graph tier ALSO emits it, there are two clash
  sinks. The family must displace, not co-exist — the e-graph's `modify` clash IS the
  obligation producer, and `join_types` retires.
- **R-D6 — LOC-3 lowering must accept `Row`/`Sum`.** `named_types.rs:100 infer_named_shape`
  today infers a *scalar tuple*; a `Row`/`Sum` type_desc it does not recognize would fall
  through. The widening is not complete until `from_ir` on both backends resolves the new
  variants — else the tier populates a vocabulary nothing consumes (Y.13 breach).

---

## 6. Exactly what a prototype must test to move family 1D

1. **Make-or-break (D #1).** Build `type_tier/{node.rs,analysis.rs}`: `TypeENode` +
   `#[derive(Language)]` + `TypeShapeAnalysis`. Feed the structural bodies
   `object = "{" pair* "}"`, `pair = string ":" value`, `array = "[" value* "]"` (bytes
   present but unread). Assert: `class(object).kind == Record`, `class(array).kind == List`,
   with **zero** `{`/`}` byte match and zero grammar-name predicate (grep-gate the module).
   Then flip keys literal vs regex and assert closed-`Row` vs open-`Row{open}`. Report
   whether a hint was required (expected answer: **no hint for record-vs-list; a
   key-leaf-kind inference — not a hint — for closed-vs-open**).
2. **Congruence race (D #4 / Open R).** Race e-graph unify (`union`+`rebuild`) against the
   running `passes/types/constraint::join_types` on JSON / Sheets / CSS-L4: derive
   wall-time, does `List ≠ Row` populate, and does it lower byte-identically through
   `TypeAnalysis::from_ir`. Head-to-head LOC-deleted vs the flat store.
3. **AC-sum + injectivity co-residence (D #3).** In ONE e-graph: (a) two orderings of the
   six-arm `value` alternation ⇒ identical `Sum` class (confluence); (b) a
   same-discriminant / distinct-payload variant pair ⇒ exactly one `DeriveError::Unify`
   clash, no silent `BoxedEnum` (soundness). Both under a single `CspScheduler.run`.
4. **Union-frontier amendment (D #2).** Instrument `CspScheduler` per-tier
   (grammar/regex/type) saturation cost with the precise union-frontier vs the conservative
   fallback; land if the type tier wins, retire in-tree with the numbers if it does not.
5. **Equirecursive termination (R-D2).** `value ⊇ object ⊇ value` builds a finite `Rec`
   e-class and saturates; the typo cycle `value = object | value` clashes-or-`Rec`s, never
   hangs.
6. **Invariant gate every wave.** `git diff --name-only <base>..HEAD | grep crates/runtime`
   empty — the tier is compile-time only (constitution v; PROVE-08 zero-runtime held).
