# LOCK-1 · Family 1D — TYPES-AS-ECLASSES · pass-2 research

The type lattice lives in an e-graph. Unification is congruence closure; a fresh
e-class *is* a type variable; recursive document shapes are cycles, not μ-binders;
projections canonicalize by saturation; extraction picks the representative type by
cost. The family's decisive thesis: the same e-graph substrate LOCK-2 runs is a *better*
unification engine than a tree-walking Hindley–Milner core, precisely for the three
things the grammar plane needs and HM struggles with — equirecursive document types,
associative-commutative sum canonicalization, and cost-driven width defaulting.

---

## 0. The substrate already exists and is proven in-tree

A complete egg-style e-graph lives at `crates/egraph/` (1885 LOC) with every organ this
family needs, already exercised by two live consumers:

- **`Language` trait** + **auto-derive** — `crates/egraph/src/language.rs:16`,
  `crates/egraph-derive/src/lib.rs:49`. The derive projects `Id`, `Box<[Id]>`,
  `Vec<Id>`, `[Id; N]` fields into one `children() -> &[Id]` slice with zero per-field
  attributes; non-`Id` fields auto-classify as leaf metadata; `#[language(skip)]` forces
  metadata. A recursive type enum drops into this unchanged — the grammar tier already
  does it at `crates/ir/src/egraph/node.rs:23` (`GrammarENode`, 15 variants, `Box<[Id]>`
  and `[Id; 2]` children, `#[language(skip)]` on `TokenDispatch.arms`); the
  backend-decision tier does a leaf-only version at
  `skinny/crates/passes/src/backend_egraph.rs:181`.
- **Congruence closure** = `EGraph::add` / `union` / `rebuild` —
  `crates/egraph/src/egraph.rs:155,202,251`. `add` hashconses on canonicalized children
  (`memo: FxHashMap<N, Id>`); `union` merges classes; `rebuild` re-canonicalizes parent
  edges to a fixpoint. This *is* unification-with-congruence: two type e-nodes with the
  same constructor whose children are already unified hashcons to one class
  automatically. No separate occurs-check, no substitution map.
- **Per-class lattice analysis** — `crates/egraph/src/analysis.rs:18`. `make` computes a
  monotone value from children's classes; `merge` joins on `union`, returning a `changed`
  bool feeding saturation detection. A `WidthBound` lattice (`min` join by `min`, `max`
  join by `max`, unbounded = `None`) already lives at
  `crates/ir/src/egraph/analysis/facts.rs` — the numeric-width defaulting Thread A/B treat
  as a post-solve CSP *is already a monotone e-class analysis here*.
- **Cost extraction** — `crates/egraph/src/extract.rs:85,115`. `Extractor` +
  `CostModel<N>` + `Lattice`; greedy bottom-up fixpoint (`compute_best`). This is how
  "extraction picks the representative type by cost" is realized — no new machinery.
- **Scheduling** — `crates/egraph/src/csp_scheduler.rs:204` (`CspScheduler`) and
  `scheduler.rs` (`BackoffScheduler`). The CSP scheduler models "which classes need
  re-probing" as an AC-3 dirty-closure over parent edges; its doc-comment
  (`csp_scheduler.rs:20-23`) asserts isomorphism to the grammar tier's `csp-solver`
  usage. This is the shared-engine seam Thread D#2 asks about — already built.

**Consequence.** Family 1D lands *no new engine*. It lands one new `Language` enum
(`TypeENode`), a handful of rewrite rules, one cost model, and one sink-type widening
(`TypeDesc::Row`). Everything else is a second consumer of a substrate that already has
two.

---

## 1. The mechanism made concrete — `TypeENode`

```rust
// crates/ir/src/egraph/type_node.rs (sibling of node.rs; colocation edict)
#[derive(Clone, PartialEq, Eq, Hash, Debug, egraph_derive::Language)]
pub enum TypeENode {
    // ── leaves (mirror TypeDesc scalars, type_desc.rs:13) ──
    Span, F64, Bool, U8, U16, U32, I8, I16, I32, I64, U64,
    // ── unary / list ──
    Option(::egraph::Id),
    Vec(::egraph::Id),
    // ── record: children = per-field PAYLOAD e-classes; keys are metadata ──
    Row { fields: Box<[::egraph::Id]>, #[language(skip)] keys: Box<[StringId]> },
    // ── variant / sum: children = per-arm payload e-classes ──
    Sum { arms: Box<[::egraph::Id]>, #[language(skip)] tags: Box<[StringId]> },
    // ── positional product (tuple); position is significant ──
    Tuple(Box<[::egraph::Id]>),
}
```

There is **no `Var` node**. A type variable is a *fresh e-class* (`unionfind.make_set`,
`egraph.rs:162`); unifying two variables is `egraph.union(a, b)`; unifying a variable
with a constructor unions the var's class with the constructor's class. This is the
union-find of a classical HM engine (rustc's `InferCtxt`/`ena`, GHC's `TcS`) with
congruence added for free. The `keys`/`tags` metadata rides as `#[language(skip)]`
exactly as `GrammarENode::TokenDispatch.arms` does (`node.rs:63`).

**Recursion is a cycle, not a μ-binder.** JSON's `value = object | array | string |
number | bool | null`, where `object`'s payload contains `value`, is a cyclic reference
graph: the `Sum` class for `value` has a `Row`/`Vec` arm whose child *is the `value`
class itself*. The e-graph stores children as `Id`s, so a self-referencing class is legal
and terminating — `rebuild` canonicalizes and stops. Tree-based HM (PROVE-08's 221-LOC
union-find core, `prototypes/PROVE-08/patch.diff:230-456`) cannot represent this without
an explicit occurs-check-defeating μ-type; the e-graph gives **equirecursive
(regular-tree) unification for free**. This is the family's single sharpest technical
claim and the first thing a prototype must demonstrate.

---

## 2. Thread D#1 — the make-or-break: derive `Row` vs `Vec` by inference, no hint

**The hint to kill.** Today the record-vs-list decision is a byte-literal gate: the role
classifier marks a rule as container iff its body contains the `{` and `}` literals,
sequence iff `[` and `]` (`skinny/crates/passes/src/lib.rs`, role classifier at
`:1403-1490`). Both `object` and `array` are the *same* `ExprKind::Repeat`
(`array = "[" value* "]"`, `object = "{" pair* "}"`); only the bracket bytes distinguish
them. That is the per-grammar hint the family must remove.

**The e-graph classification is congruence over the element shape.** In `TypeENode`:

- `array`'s repeat element unifies to the `value` class directly → the repeat's type is
  `Vec(value)`.
- `object`'s repeat element is `pair = string ":" value`, a two-leg product whose first
  leg carries a **key projection**. Its element class is a keyed pair → a repeat of keyed
  pairs canonicalizes to `Row`/map.

The discriminator is **"does the repeated element carry a key leg?"** — a congruence fact
about the element's e-class, *not* a read of `{`/`[` bytes. A rewrite rule
`repeat-of-keyed-pair ⇒ Row` fires on the first; `repeat-of-bare ⇒ Vec` on the second.

**Honest finding — the family's answer.** Record-vs-list *is* derivable without the
bracket hint — **but only when the key-ness of the leg is a grammar fact**, i.e. a `->`
projection marks which leg is the key (`string -> key`). The live JSON grammar has
*zero* `->` projections (value/type recon: `json.bbnf` is pure structural EBNF), so a
generic `json.bbnf` cannot distinguish a keyed map from a positional list-of-pairs
because the information is not present — this is PROVE-08's **Wall 1** (`GN-3.md §1.2`)
restated at the e-graph: *the e-graph removes the byte-literal hint but not the
projection requirement*. The residual that genuinely cannot be inferred is **keyed-map vs
positional-tuple for a 2-field product** — resolved by the projection *target* (named
field ⇒ `Row`, positional ⇒ `Tuple`), never by a grammar-name gate. A prototype must
build exactly this on bare `json.bbnf` + a typed variant with `string -> key` + a
synthetic `array = "[" pair* "]"` (array-of-pairs) grammar, and report: (a) zero
bracket-byte reads in the classifier; (b) whether keyed vs positional disambiguates from
projection facts alone. Expected verdict: **hint-free for the Row/Vec split; the only
irreducible input is the `->` projection, which is legitimate declared information, not a
heuristic.**

---

## 3. Thread D#3 — AC-Sum + positional injectivity, co-resident, one saturation

Two rules with opposite symmetry must live in the same e-graph under one `rebuild`
fixpoint:

- **AC-Sum canonicalization.** `object|array|string|number|bool|null` must equal every
  reordering (the R5 order-sensitivity hazard, `GN-3.md §7`). Realized as a rewrite that
  installs a canonically-ordered `Sum` (sort arms by a stable variant key) and `union`s
  it with the source `Sum`; congruence then hashconses two `Sum`s with the same *set* of
  arm classes into one class. Because rewrites are **additive** (`rewrite.rs:6-7` —
  applying never removes the original), sorted and unsorted forms co-exist and collapse
  by hashcons; no destructive loop.
- **Positional injectivity.** `Tuple(A,B) ≠ Tuple(B,A)` and `Row` field order is
  significant for lowering. Realized by *not* giving `Tuple`/`Row` an AC rule — their
  `Box<[Id]>` positional slice is the hashcons key, so `Tuple(A,B)` and `Tuple(B,A)` are
  two distinct memo keys and two distinct classes.

**Confluence / soundness argument.** The AC rewrite is **constructor-scoped**: its
`search` (the `Rewrite` trait, `rewrite.rs:34-71`) matches only `Sum` nodes, never
`Tuple`/`Row`. Congruence closure is per-constructor (the memo key includes the
discriminant), so no `Sum` rule can reorder a `Tuple`'s children. Termination follows
from additivity + hashcons: each application either adds one canonical `Sum` (bounded by
the number of distinct arm-sets) or is a no-op the `should_apply` guard (`rewrite.rs:58`)
suppresses. Proof obligation for a prototype: one `EGraph<TypeENode>`, one
`CspScheduler::run`, assert (i) two arm-reorderings of a 6-arm `Sum` land in **one**
class; (ii) `Tuple(A,B)`/`Tuple(B,A)` stay in **two** classes; (iii) saturation
terminates (`report.saturated`, not `iter_limit_hit`). Sharp counterexample to guard: a
`Sum` with two *equal* arm classes (duplicate variant) — the AC-sort must dedup or the
class count oscillates.

---

## 4. Thread D#2 — shared-engine union scheduling across the three tiers

The "union scheduling amendment" is: route the type tier through the **same**
`CspScheduler` (`csp_scheduler.rs:204`) the grammar tier's e-graph already uses, rather
than a bespoke type-solver loop, so grammar + regex + type saturation share one
dirty-closure scheduler. The scheduler is generic over `<N: Language, A: Analysis<N>>`,
so `TypeENode` plugs in with no change. Per-tier cost = iterations + `union`/`rebuild`
cycles to saturation (`RunReport.iterations`, `total_applied`).

**Predicted per-tier judgment (confirm or retire in-tree).** The dirty-closure pays off
when the graph is large and the dirty frontier is a small subtree — true for the grammar
tier (hundreds of nodes). The type tier's graphs are *tiny* (the live typed `json.bbnf`
has 5 arrows; even CSS-L4 is a few dozen type nodes), so iteration-1 "everything dirty"
dominates and the CSP build is pure overhead. Honest expected verdict: **land the shared
scheduler for the grammar tier (already there), use `BackoffScheduler` — or a single
`rebuild` — for the type tier, and report the node-count crossover** where CSP scheduling
starts to win. A prototype must measure both schedulers on json/sheets/CSS-L4 type
e-graphs and give the per-tier number; the amendment is *retired for the type tier* unless
the measured crossover sits below CSS-L4's size.

---

## 5. Open R — engine-agnostic record-widening (the sink)

The LOCK-1→LOCK-3 handoff is `TypeAnalysis::from_ir` at
`crates/core/src/backend/driver/analysis.rs:22`, which clones `ir.types:
Vec<(RuleId, TypeDesc)>` (second `from_ir` at `rust/view/named_types.rs:100`); the TS
mirror is `crates/core/src/backend/ts/projection.rs`. The sink type is `TypeDesc`
(`crates/ir/src/types/type_desc.rs:13`): `Span, F64, Bool, I8..U64, Option, Vec, Tuple,
BoxedEnum, Enum, Named`, plus a heterogeneous-alt join-obligation variant — **but no
`Row`** (no record) and no first-class `Sum`. That absence is *why* `List ≠ Row` cannot
be populated today and *why* the JSON path falls back to the `sink_only.has_rule("value")`
name-gate (`runtime_generator.rs:99`).

Family-1D position on Open R: add **`TypeDesc::Row { keys: Vec<StringId>, fields:
Vec<TypeDesc> }`** (and promote the sum representation) at `type_desc.rs:13`. Then the
e-graph `Extractor` extracts a `TypeENode::Row` into `TypeDesc::Row`, and the *running
structural fold* can populate the same variant independently — **structural fidelity
becomes orthogonal to engine choice**, exactly Open R's ask. The e-graph is the *producer*
that decides Row-vs-Vec by inference (§2); `TypeDesc::Row` is the shared representation
both the e-graph extraction and any other engine write into, and both lower through
`from_ir`. Adding `Row` is the single edit that closes the masked `json_templates`
fallback: once `List ≠ Row` is an inferred *type* fact reaching `from_ir`, the
`has_rule("value")` string gate has an inference-derived replacement.

---

## 6. Open X — the width micro-fixture is an extraction cost, not a search

A `->` genuinely under-determined between `u8`/`u16`/`u32` (the value fits any) is the
canonical residue Thread A#3 / Open X want resolved. Family 1D resolves it **by
extraction cost, not by CSP search**: the numeric leaves `U8`/`U16`/`U32` sit in one
e-class (all valid widths of the same literal), a `CostModel<TypeENode>`
(`extract.rs:85`) assigns *narrowest-width-cheapest* (`U8 < U16 < U32`), and `Extractor`
picks `U8`. Ordered-lattice defaulting is thus a monotone cost, and the pluggable seam
Open X asks for is the `CostModel` swap: one implementation = narrowest-width lattice, the
swappable second = a CSP search over a legality constraint. Evidence for the family: one
micro-fixture, one e-class holding three width nodes, two cost models behind the same
`Extractor::new(&graph, &cost_model)` call — the seam is the `&C: CostModel` parameter,
already generic. The `WidthBound` analysis (`facts.rs`) supplies the legality bound; the
cost model supplies the defaulting order; they are *already two orthogonal knobs* on this
substrate.

---

## 7. Integration seams (path:line)

| Concern | Reuse / extend | Location |
|---|---|---|
| e-node derive | `#[derive(Language)]` verbatim | `crates/egraph-derive/src/lib.rs:49`; pattern `crates/ir/src/egraph/node.rs:23` |
| unification | `add`/`union`/`rebuild` (congruence) | `crates/egraph/src/egraph.rs:155,202,251` |
| var = fresh class | union-find `make_set` | `crates/egraph/src/egraph.rs:162` |
| width/decode analysis | `Analysis<TypeENode>` + `WidthBound` | `crates/egraph/src/analysis.rs:18`; `crates/ir/src/egraph/analysis/facts.rs` |
| defaulting extraction | `Extractor` + `CostModel` + `Lattice` | `crates/egraph/src/extract.rs:85,115` |
| shared scheduler | `CspScheduler` / `BackoffScheduler` | `crates/egraph/src/csp_scheduler.rs:204` |
| rewrite rules (AC-Sum, Row-classify) | `Rewrite` + `should_apply` guard | `crates/egraph/src/rewrite.rs:34,58` |
| **sink widening** | add `TypeDesc::Row` | `crates/ir/src/types/type_desc.rs:13` |
| **LOCK-3 handoff** | `TypeAnalysis::from_ir` | `crates/core/src/backend/driver/analysis.rs:22`; `rust/view/named_types.rs:100` |
| **TS mirror** | `projection.rs` | `crates/core/src/backend/ts/projection.rs` |
| discard site reclaimed | `Project` arm drops `target,ty` | `skinny/crates/passes/src/lib.rs:204` |
| byte-literal hint to delete | role classifier | `skinny/crates/passes/src/lib.rs:1403-1490` |
| 4-way fold to delete | `ProjectionKind`/`classify_projection` | `crates/codegen/src/emit/general_descent/view.rs:20-46` |
| name-gate closeable | `has_rule("value")` | `runtime_generator.rs:99` |

---

## 8. Prior art (citations)

- **egg** (Willsey, Nandi, Wang, Flatt, Tatlock, Panchekha, *POPL 2021, "egg: Fast and
  Extensible Equality Saturation"*) — the e-graph + `Analysis` + cost-extraction design
  this crate ports faithfully. E-class analyses as monotone lattices is egg's key idea,
  reused directly for width/decode.
- **Congruence-closure = unification** — Nelson & Oppen (1980), Downey–Sethi–Tarjan
  congruence closure. Hashcons-on-canonical-children *is* congruence; `union` merges
  equivalence classes as HM's `unify` merges variables. rustc's `InferCtxt` (the `ena`
  union-find crate) and GHC's constraint solver are union-find engines — the e-graph is
  their congruence-closed generalization.
- **Equirecursive / regular-tree types** — Colmerauer's rational-tree unification
  (Prolog II), Cardelli's Amber equirecursive subtyping, Pottier "recursive types and
  unification." The e-graph's cyclic `Id` references give μ-free equirecursive unification
  — the JSON `value` cycle is a rational tree, terminating without an occurs-check.
- **Row polymorphism** — Rémy/Wand extensible records, Leijen "Extensible records with
  scoped labels." `Row{fields, keys}` with saturation-canonicalized field order is the
  e-graph realization; label scoping becomes a canonicalization rewrite.
- **AC in e-graphs** — Bachmair–Ganzinger AC completion; egg's additive-rewrite +
  hashcons handling of commutativity. The constructor-scoped AC-Sum rule is the standard
  egg treatment.
- **Scheduled multi-tier saturation** — egglog (Zhang et al. 2023, "Better Together:
  Unifying Datalog and Equality Saturation") and MLIR/PDL, for the shared-scheduler
  amendment across grammar/regex/type tiers.

---

## 9. Named risks & counterexamples

- **R-D1 — extraction ≠ principality.** Greedy min-cost extraction (`extract.rs:134`) may
  pick a *cheaper* type that is not the *principal* type (two structurally distinct rows
  sharing a cost). Mitigation: the cost model must be structure-injective — principal-type
  = extraction under a structure-preserving cost, not `AstSize`. A prototype must confirm
  the extracted type equals the HM principal type on the PROVE-08 json fixture.
- **R-D2 — the D#1 boundary (Wall 1 restated).** With zero `->` projections the e-graph
  cannot separate keyed-map from list-of-pairs; the family removes the *bracket-byte*
  hint, not the *projection* requirement. This is the honest scope, not a defect papered
  over.
- **R-D3 — AC-Sum non-termination.** A naive "sort children" rewrite re-fires every
  iteration; must be guarded by a canonical-form `should_apply` check (`rewrite.rs:58`)
  and arm-dedup, or saturation never converges.
- **R-D4 — co-residence corruption.** The AC-Sum rule *must* be constructor-scoped in
  `search`, or it reorders `Row`/`Tuple` children and destroys positional identity — the
  D#3 soundness proof obligation.
- **R-D5 — GN-3 supersession.** `GN-3.md §1.5` explicitly scopes CSP/e-graph **out** of
  the extant type-plane tranche ("no CSP or e-graph consumer exists in this tranche").
  Family 1D is a *named supersession* of that scoping (Constitution ix), justified only if
  the e-graph's equirecursive + AC-canonicalization + cost-defaulting wins are measured to
  beat the 221-LOC tree HM. The e-graph carries more build-time weight; the
  zero-runtime-cost invariant (Constitution v, `GN-3.md §1.4`) holds by construction
  (generation-time only), but the *build-time* cost must be measured, not assumed.

---

## 10. What a prototype must test to move Family 1D

1. **Equirecursive proof.** Build `TypeENode`; unify `json.bbnf`'s recursive `value` as a
   **cyclic** e-class; assert `rebuild` terminates and the class is well-formed — the
   advantage over PROVE-08's tree HM that no other family can claim.
2. **D#1 make-or-break.** On bare `json.bbnf` + typed (`string -> key`) + a synthetic
   `array = "[" pair* "]"`: classify Row-vs-Vec with **zero bracket-byte reads**; report
   whether keyed-vs-positional disambiguates from `->` projection targets alone.
3. **D#3 co-residence.** One `EGraph<TypeENode>`, one `CspScheduler::run`: 6-arm `Sum`
   reorderings collapse to one class; `Tuple(A,B)`/`Tuple(B,A)` stay distinct; saturates
   (not `iter_limit_hit`); duplicate-arm `Sum` dedups.
4. **D#2 per-tier scheduler cost.** Backoff vs Csp scheduler on json/sheets/CSS-L4 type
   e-graphs; report iterations + wall; give the node-count crossover; land-or-retire per
   tier.
5. **Open R sink.** Add `TypeDesc::Row`; extract from the type e-graph into it; lower
   through `TypeAnalysis::from_ir`; show `List ≠ Row` populated and the
   `has_rule("value")` name-gate (`runtime_generator.rs:99`) closeable.
6. **Open X width.** One e-class holding `U8`/`U16`/`U32`; two `CostModel`
   implementations (narrowest-lattice vs legality search) behind the same `Extractor::new`;
   head-to-head on a `->` micro-fixture — the pluggable seam is the `&C: CostModel`
   parameter, already generic.
