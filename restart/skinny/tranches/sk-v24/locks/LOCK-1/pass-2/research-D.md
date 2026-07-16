# LOCK-1 · pass-2 · Family 1D — TYPES-AS-ECLASSES

**The type-theoretic center.** The type lattice lives in an e-graph; unification is
congruence closure over a type-node `Language`; projection canonicalization is
saturation under rewrite groups; the winning representative type per rule is extraction
by cost. This family does not carry its own solver — it consumes the production e-graph
substrate (`crates/egraph/`, shared with LOCK-2) and adds exactly one net thing: a
**type-node Language + a per-e-class type lattice**, so that record-vs-list, sum
canonicalization, and width defaulting are all facts an Analysis computes, never facts a
bracket heuristic matches.

---

## 1. The substrate is already production-grade (no new engine)

The e-graph the family needs exists and is live in the tree, not a research spike:

- `Language` trait — `crates/egraph/src/language.rs:16` (`children()/children_mut()`
  return contiguous `&[Id]` slices; `matches()` is the discriminant-equal congruence
  test at `:30`).
- `#[derive(Language)]` — `crates/egraph-derive/src/lib.rs:48`. It projects each
  child-bearing variant to a slice; multi-`Id` variants must be a single `[Id; N]` /
  `Box<[Id]>` field (`:183`). A **type-node** enum derives its `Language` impl
  identically — the exact pattern `GrammarENode` already uses at
  `crates/ir/src/egraph/node.rs:23` (`Seq(Box<[Id]>)`, `Alt(Box<[Id]>, Option<..>)`).
- Congruence closure = `EGraph::union` + `rebuild` — `crates/egraph/src/egraph.rs:202,251`
  over `crates/egraph/src/unionfind.rs`. `add` hash-conses e-nodes (`:155`);
  `canonicalize` rewrites children to canonical ids (`:144`). **This is unification: two
  type e-nodes with congruent children land in one class.**
- Per-class lattice — `Analysis<N>` trait, `crates/egraph/src/analysis.rs:19`:
  `make(egraph,node)->Data` computes a fact from a fresh e-node given its children's
  facts; `merge(a,b)->bool` is the monotone join fired on every union, returning the
  LLVM-style Changed flag the saturation loop reads for fixpoint. This is the machinery
  that makes "Row" a *derived fact*, not a syntactic match.
- Extraction by cost — `Extractor` + `CostModel::cost(node, child_cost)` + `Lattice`,
  `crates/egraph/src/extract.rs:85,115` (`best_node`/`best_cost`/`extract_tree`). The
  "representative type" the family promises is literally `Extractor::best_node`.
- CSP-backed saturation schedule — `CspScheduler`, `crates/egraph/src/csp_scheduler.rs:175`
  with `DirtyDomain`/`ParentDirtyProp` AC-3 dirty propagation (`:57,120`). This is the
  `union` scheduling seam Thread D.2 targets.

**Precedent that a real grammar already saturates through this:** `GrammarAnalysis`
(a live `Analysis<GrammarENode>` lattice) is exercised on a compiled fixture in
`crates/ir/tests/egraph/egraph_analysis.rs` (build+saturate → per-class `EClassFacts`
via `egraph.class(id).data`). The type plane is the same shape one tier up.

So family 1D lands **one type-node Language + one type Analysis**, not an engine.

---

## 2. The core mechanism — types as e-classes

Define a type-node Language mirroring the schema IR's shape (the `DirectTypeKind` /
`TypeDesc` vocabulary), child-bearing variants only:

```
#[derive(Clone, Eq, PartialEq, Hash, Language)]
enum TyNode {
    Var,                         // leaf — a fresh unification variable seeds a class
    Leaf(LeafCon),               // Bool/F64/U8..U64/StringSpan/StringDecoded (leaf)
    Row(Box<[Id]>),              // record: children are Field e-classes (key-tagged)
    List(Id),                    // homogeneous sequence  → TypeDesc::Vec
    Sum(Box<[Id]>),              // n-ary alternation of variant e-classes
    Field { key: StringId, ty: [Id;1] },   // a keyed member (metadata key, one child)
    Opt(Id),                     // TypeDesc::Option
}
```

Unification of two rule types = `egraph.union(ty_a, ty_b)`; congruence `rebuild`
propagates it. **The clash discipline** (SPEC §56 no-silent-epsilon; the
`HeterogeneousAltJoin` obligation at `crates/ir/src/types/type_desc.rs`): a `merge` that
would join two *incompatible constructors* (e.g. `Leaf(F64)` vs `Row`) does not silently
pick one — the type Analysis records a **clash obligation** in the class `Data` (a
lattice `⊤`/conflict element carrying both branch types), which extraction surfaces as a
diagnostic and lowers to the existing `TypeDesc::HeterogeneousAltJoin(Vec<TypeDesc>)`
sink — the join obligation the constraint plane already emits at
`passes::types::constraint::revise::join_types` (per the `HeterogeneousAltJoin` doc). The
e-graph does not need a bespoke error path; the obligation *is* a lattice value.

---

## 3. Thread D.1 (make-or-break) — Row derived by inference, no bracket hint

**The wall today.** R6 §5c names the sin exactly: `derive_materialization_roles`
(`skinny/crates/passes/src/lib.rs:1403-1490`) classifies rules **by byte-literal**:
`{`+`}` → container (`:1433`), `[`+`]` → sequence (`:1441`). `TypeDesc`
(`crates/ir/src/types/type_desc.rs:13`) has **no `Row` variant at all** — records and
lists both collapse into `Vec`/`Tuple`/`Enum`, which is the masked `List ≠ Row` gap
Open R names.

**The e-graph route — classify by the repeated element's shape, not the delimiter.**
The discriminant lives in the *body*, not the brackets: an `object` rule is
`"{" pair* "}"` where `pair = key ":" value` — its `Repeat` element is a **Field**
(a key-tagged binding). An `array` rule is `"[" value* "]"` — its `Repeat` element is a
**bare value** with no key. So the rule is:

> A `Repeat` (or `Seq` of alternating members) whose element e-class carries the
> `keyed` fact canonicalizes to `Row`; whose element is un-keyed canonicalizes to `List`.

This is a `TyAnalysis::make` reading children facts, plus two rewrite rules under
saturation:

- `Repeat(x)` where `analysis(x).keyed == true` ⇒ union with `Row([x])`.
- `Repeat(x)` where `analysis(x).keyed == false` ⇒ union with `List(x)`.

`keyed` is set by `make` on a `Field` node (it carries a `key: StringId`) and propagated
by `merge` up through the repeat. **No `{`/`}` literal is ever consulted** — the fact
comes from whether the grammar's `pair` rule projects a key. This directly answers the
directive: **a hint is NOT required** *iff* the grammar's pair/member structure is
recoverable (the `key ":" value` shape, or a `-> key` projection on the member). The
honest caveat: json.bbnf's `pair` (R6 §0: 18 lines, zero `->`) makes "keyed" recoverable
structurally (a `Seq` whose first element is a string-typed leaf followed by a `:`
literal); a fully anonymous `value*` body is genuinely a List and *should* classify as
one. The prototype must confirm the structural `keyed` predicate fires on `pair` without
a name-gate on the rule string `"object"` — that is the make-or-break assertion.

**Why the e-graph specifically (vs. a plain fold).** The same `value` alternation is
referenced from both object-value and array-element positions; congruence closure lets
the *one* `value` e-class be classified once and shared, and lets a re-ordered
alternation (`bool|null|...` vs `null|bool|...`) land in the same class by hash-consing —
the property a linear role fold cannot give and R5 (GN-3 §7) flags as the equality risk.

---

## 4. Thread D.2 — the `union` scheduling amendment, measured across three tiers

**What the amendment is.** The `CspScheduler` seeds each next iteration's dirty set from
**node-count growth** (`csp_scheduler.rs:293-306`). But a *pure union* (a congruence
merge that adds no node — exactly what type unification does) grows no class, so the
scheduler falls to the **conservative "everything dirty" reseed**
(`csp_scheduler.rs:307-317`, verbatim: "If we applied work but no class is grown (pure
unions that don't add nodes), fall back to the conservative everything dirty seed"). For
a type tier that is almost entirely unions and almost no node growth, that fallback
discards the whole point of dirty propagation — every iteration re-visits every class.
The amendment: route `union_count()` deltas (`egraph.rs:109`) through `ParentDirtyProp`
(`csp_scheduler.rs:120`) so a union seeds *only the merged class's parents* dirty, not
the universe.

**The three-tier saturation measurement the directive demands** (land or retire in-tree):

| tier | e-node population | union:node ratio | amendment expected to |
|---|---|---|---|
| grammar (`GrammarENode`, `crates/ir/src/egraph/node.rs`) | node-growth-heavy (inline/unroll rewrites *add* nodes) | low | ~no change — growth reseed already precise |
| regex HIR (LOCK-5's tier) | mixed | medium | modest |
| **type (`TyNode`, this family)** | union-heavy, node-growth near-zero | **high** | large — this is where "everything dirty" dominates |

The prototype instruments `RunReport` (`crates/egraph/src/scheduler.rs`, per-rule applied
counts + `final_classes`) on the same fixture with/without the amendment and reports
iterations-to-saturation and classes-visited per tier. **Retirement criterion:** if the
type tier's saturation is already O(1) iterations (types are shallow — a JSON value
lattice is ~7 nodes deep), the amendment is a no-op win and should be *retired as
unnecessary*, not landed for its own sake — the honest negative result Thread D.2 invites.
The measurement decides; it is cheap because the fixture already exists
(`egraph_analysis.rs`).

---

## 5. Thread D.3 — AC-sum canonicalization + positional injectivity, one `Sum`, one saturation

**The gap.** The e-graph has **no associative-commutative / multiset support** — a grep
of `crates/egraph/src/` for `commut`/`multiset`/`assoc` returns nothing. So
`Sum(bool|null|...)` and a re-ordered `Sum(null|bool|...)` are *distinct* e-nodes (the
`Language::matches` congruence test is order-sensitive over the child slice). GN-3 R5
(the order-sensitivity risk) and the directive's "AC-sum canonicalization" both live here.

**Two routes, and why they must be co-resident on ONE `Sum`, not two e-graphs:**

1. **AC via explicit commutativity rewrites** — the egg classical approach: add
   `Sum([a,b]) = Sum([b,a])` rewrites. This is **combinatorially explosive** (n! orderings
   materialize as e-nodes) and is the known egg failure mode for large sums. Reject for
   any sum wider than ~4 arms.
2. **AC via canonical-ordering in the builder** — a bespoke normalization: a `TyNode`-local
   `canonical()` sorts a `Sum`'s child slice to a **sorted-by-canonical-id multiset**
   *before* the hash-cons at `egraph.rs:155`. Then `Sum(bool|null)` and `Sum(null|bool)`
   *hash-cons to the same node* with zero extra e-nodes. This is the tree-normalization
   discipline (Hopcroft-style normal form), and it is what the family should land — it
   makes AC a property of the node, not a rewrite.

**Positional injectivity co-resident.** A `Sum` is also a *tagged* union: each variant
carries a discriminant (the byte the runtime's `at_cursor` dispatches on,
`skinny/crates/runtime/src/grammars/json/value.rs:28-47`). Injectivity = no two variants
share a tag. This is a **second fact on the same `Sum` class**: the type Analysis's
`make` for `Sum` computes both (a) the AC-canonical variant multiset and (b) the tag→variant
injectivity map, and `merge` joins both under one saturation. Confluence/soundness to
report: because AC-canonicalization is a *pre-hash-cons normal form* (not a rewrite that
races other rewrites), it commutes with every other rule — the e-graph reaches the same
fixpoint regardless of union order, which is the confluence claim. The counterexample to
test: two variants that AC-canonicalize equal (same payload type) but carry *different*
tags — the injectivity fact must keep them distinct (a `"px"->0u8 | "em"->1u8` split with
identical `U8` payloads), proving the two facts are genuinely orthogonal on one class, not
a single merged notion.

---

## 6. Open R (engine-agnostic Row-widening) — this family's lowering contract

The family's output must lower through the real handoff. **The seam is
`TypeAnalysis::from_ir`** (`crates/core/src/backend/driver/analysis.rs:22`) — it simply
clones `ir.types: Vec<(RuleId, TypeDesc)>` + `ir.type_map` scratch types (the second
`from_ir` is `crates/core/src/backend/rust/view/named_types.rs:100`). So the e-graph never
reaches the backend directly — **extraction emits `TypeDesc`, and the backend is
engine-blind.** That is the point: adding `TypeDesc::Row(Vec<(StringId, TypeDesc)>)` and
`TypeDesc::Sum(Vec<TypeDesc>)` beside the existing variants
(`crates/ir/src/types/type_desc.rs:13`) lets **any** engine — the e-graph here, the
hardened constraint store, or the running fold — populate `List ≠ Row` and lower it
identically. Structural fidelity becomes **orthogonal to engine choice** exactly because
the e-graph's only export is `TypeDesc` via `Extractor::best_node`, cloned by `from_ir`,
mirrored to TS by `type_desc_to_ts` (`crates/core/src/backend/ts/projection.rs:11`).

This closes the masked `json_templates` fallback: today `object` and `array` both resolve
to bracket-matched roles and the JSON lazy view is a static `include_str!` template (R6
§0, `runtime_generator.rs:99` name-gate on rule `"value"`). Once `Row` is a `TypeDesc` the
derived schema distinguishes record from list *structurally*, and the `has_rule("value")`
gate loses its reason to exist — the family's contribution to Open R is: **the
discriminant moves from the emitter's name-gate into the type lattice.**

---

## 7. Integration seams (path:line)

- Type-node Language home: new `crates/ir/src/types/eclass/` (colocation edict; sibling
  to `type_desc.rs`), deriving `Language` exactly as `crates/ir/src/egraph/node.rs:23`.
- Congruence/union: `crates/egraph/src/egraph.rs:202` (`union`), `:251` (`rebuild`),
  `:155` (`add`/hash-cons — the pre-cons canonicalization call site).
- Type lattice: `crates/egraph/src/analysis.rs:19` (`Analysis::make/merge`); model on the
  live `GrammarAnalysis` in `crates/ir/src/egraph/analysis/`.
- Saturation schedule + D.2 amendment: `crates/egraph/src/csp_scheduler.rs:293-317` (the
  growth-vs-everything reseed), `:120` (`ParentDirtyProp`), union deltas at
  `crates/egraph/src/egraph.rs:109`.
- Extraction of representative type: `crates/egraph/src/extract.rs:115,183`
  (`Extractor::best_node`).
- The clash/obligation sink already in `TypeDesc`:
  `crates/ir/src/types/type_desc.rs` `HeterogeneousAltJoin(Vec<TypeDesc>)`.
- Lowering handoff (Open R): `crates/core/src/backend/driver/analysis.rs:22`
  (`TypeAnalysis::from_ir`) + `crates/core/src/backend/rust/view/named_types.rs:100`.
- TS mirror: `crates/core/src/backend/ts/projection.rs:11` (`type_desc_to_ts`).
- The heuristic this family displaces: `skinny/crates/passes/src/lib.rs:1403-1490`
  (`derive_materialization_roles`, bracket-literal classification), the discard site
  `skinny/crates/passes/src/lib.rs:203` (`Project` type thrown away), the 4-way fold
  `skinny/crates/codegen/src/emit/general_descent/view.rs:20-46`.
- Measurement substrate: `skinny` @ HEAD is the sole living tree (the running fold is the
  baseline to race); the greenfield `crates/` is the port target. The e-graph prototype
  builds against `crates/egraph` + `crates/ir` in the greenfield workspace.

---

## 8. Prior art (citations)

- **egg / egglog** (Willsey et al., *POPL 2021*, "egg: Fast and Extensible Equality
  Saturation"): e-class analysis as a monotone lattice (`make`/`merge`/`modify`) is
  lifted verbatim — the in-tree `Analysis` trait is this design. egg's known AC blow-up
  (commutativity rewrites materialize orderings) is the reason §5 chooses canonical-order
  normalization over rewrites; egglog's later work on *containers/multisets* is the
  precedent for a first-class multiset e-node.
- **Congruence-closure = unification**: Nelson–Oppen congruence closure and the
  Downey–Sethi–Tarjan hash-cons underlie `EGraph::add`/`rebuild`; type unification as
  congruence closure over a term e-graph is the standard type-e-graph construction.
- **Row polymorphism** (Wand 1987; Rémy; Leijen's *Extensible records with scoped
  labels*): the `Row([Field..])` node with keyed fields is a closed-row instance;
  extension/restriction stay out of scope (grammars produce closed rows), but the
  keyed-field discriminant that separates Row from List is exactly Leijen's label
  discipline.
- **rustc / HM**: let-generalization and the union-find-of-type-variables core (PROVE-08's
  221-LOC `hm.rs`, `prototypes/PROVE-08/patch.diff:230-456`) is subsumed — the e-graph's
  union-find (`crates/egraph/src/unionfind.rs`) *is* the HM store, with congruence added
  for free.
- **Hopcroft minimization as rewrite-to-normal-form** (mirrors LOCK-5's automata line):
  the AC-canonicalization of `Sum` is the same "canonical normal form via ordered
  partition" idea applied to variant sets.
- **MLIR/PDL & e-graph extraction cost models**: `CostModel`/`Extractor` mirror MLIR's
  greedy pattern-rewrite + cost-directed lowering; representative-type-by-cost is
  cost-directed extraction.

---

## 9. Named risks & counterexamples

- **R-D1 — the `keyed` predicate needs the pair structure, and json's pair is
  brackets-and-colon, not a `->`.** If the only recoverable signal is the `:` literal
  between key and value, then "structural, not a name-gate" is thin — a `:` scan is still
  a literal match, just a different literal. Counterexample: a grammar whose members are
  `key value` (space-separated, no colon) would misclassify. Mitigation: the `keyed` fact
  keys on *a string-typed leaf in head position of the member*, colon-agnostic; the
  prototype must show it fires without any single delimiter being load-bearing.
- **R-D2 — types are shallow; the e-graph may be over-engineering.** A JSON value lattice
  saturates in O(1) iterations over ~7 nodes. If the constraint-store or plain-fold
  families reach `List ≠ Row` with less machinery, the e-graph's *marginal* value is only
  the shared-`value` congruence (§3) and re-order-invariant sums (§5). The family must
  win on **those two properties specifically**, or be honestly returned as
  "engine-agnostic `TypeDesc::Row` (Open R) is the real deliverable; the e-graph is one of
  several populators." This is the sharpest risk — the substrate is powerful but the
  problem may be too small to need it.
- **R-D3 — AC-canonical-order is a builder concern that must not leak into the shared
  engine.** Reordering `Sum` children before hash-cons is a `TyNode`-specific concern that
  must not touch `GrammarENode` hash-consing. Mitigation: canonicalization lives in a
  `TyNode`-local `canonical()` called before `egraph.add`, never in `EGraph::add` itself —
  the engine stays language-agnostic (general-infra-crate law).
- **R-D4 — zero-runtime-cost invariant.** The type e-graph runs at generation time only
  (GN-3 §1.4). Any wave must show `git diff` over `crates/runtime/` empty; the e-graph is
  a schema *producer*, its product is `TypeDesc` erased before emission.
- **R-D5 — clash obligation vs. congruence merge.** A `merge` that joins incompatible
  constructors must produce a lattice conflict, not a union that silently picks a winner.
  If `merge` returns `false` (no change) on a real clash, saturation terminates with a
  wrong type. The `HeterogeneousAltJoin` sink must be wired as a `⊤` element that *always*
  reports Changed on first conflict.

---

## 10. What a prototype must test to move this family

1. **The make-or-break (D.1):** build `TyNode` + `TyAnalysis`; run it on json.bbnf's
   `object`/`array` and assert `object → Row`, `array → List` with **zero** reference to
   the rule name strings `"object"`/`"array"` and **zero** `{`/`[` literal branch in the
   classifier — the `keyed` fact alone decides. Report the exact predicate that fires and
   whether json's colon is load-bearing (R-D1).
2. **Order-invariance (D.3):** construct `value = bool|null|number|...` and a shuffled
   `value` and assert both hash-cons to one `Sum` e-class with identical canonical variant
   order — no n! e-node blow-up (assert `total_nodes()` bounded).
3. **Co-resident facts (D.3):** the `"px"->0u8 | "em"->1u8` tag-split with identical `U8`
   payloads — assert injectivity keeps two same-payload variants distinct while
   AC-canon treats their payloads equal.
4. **Union schedule (D.2):** run the same fixture through `CspScheduler` with and without
   the union-dirty amendment; report iterations-to-saturation + classes-visited per tier;
   land iff the type tier shows a real reduction, retire (negative result) otherwise.
5. **Open-R lowering:** add `TypeDesc::Row`/`Sum`, extract via `Extractor::best_node`,
   push through `TypeAnalysis::from_ir`, and show the JSON schema now carries `Row ≠ List`
   *and* the `has_rule("value")` name-gate (`runtime_generator.rs:99`) has no remaining
   discriminating role — with `git diff` over `crates/runtime/` empty and the TS mirror
   (`type_desc_to_ts`) producing the parallel type.
6. **Head-to-head honesty (R-D2):** race the derive wall-time of the e-graph populator
   against the running structural fold on JSON/Sheets/CSS-L4; if the e-graph is not at
   equal-or-lower wall-time AND does not uniquely deliver the shared-`value` congruence or
   order-invariant sum, report it as a populator-of-`TypeDesc::Row`, not the winner.
