# LOCK-1 · Family 1A — UNIFICATION-FIRST · pass-2 research

## Thesis

The type plane is a Hindley–Milner unification engine whose single carrier of
*equality* is a union-find over type variables. The engine grows to the full
grammar surface — algebraic sums for `value = object | array | …`, row
polymorphism for document shapes, let-generalization so a rule referenced from
two sites is not falsely fused — and it is *complete on its own* for every
judgment that is a decidable equality. Constraint search (the vendored
`csp-solver`, `crates/csp-solver/`) does not carry equality at all; it is
demoted to a **bounded post-solve** invoked only where unification is genuinely
undecidable: numeric-width defaulting under an ordered lattice, and overload /
decode-class resolution where a leaf is under-determined between admissible
constructors. The union-find is the spine; the CSP is a leaf oracle the spine
calls after the equality skeleton is fixed.

This is a deliberate inversion of the running greenfield pass, which is
CSP-first: `project_types` (`crates/ir/src/passes/types/mod.rs`) drives *all*
type inference through AC-3 over a lattice domain, and equality is carried by
that lattice's `join`. That `join` is the family's central counterexample
(below): it is not an equality carrier, it is a last-writer-wins overwrite.

## The substrate — one type language, one carrier

The engine's monotype language is a strict superset of PROVE-08's proven core
(`prototypes/PROVE-08/patch.diff:230-456`, the 221-LOC union-find `hm.rs`
certified perf-neutral and clash-honest by `PROVE-08/VERIFY.md`). That core
ships: `Ty::{Var(u32), Con(TyCon)}`, path-halving `find`, constructor-binding
`unify` with an honest `UnifyError{lhs,rhs}` clash, and `resolve`/zonk. Family
1A grows exactly four constructors onto it, each with a decidable `unify` arm:

- **`Con` leaves** — the existing closed vocabulary, widened to mirror the sink
  lattice `TypeDesc` (`crates/ir/src/types/type_desc.rs:13-64`):
  `Span, F64, Bool, I8..U64, Str`. Widths are leaves, not variables (see §Width).
- **`Sum(Vec<Ty>)`** — an N-arm alternation as one composite. `unify(Sum a,
  Sum b)` is *set-equal variant unification*: order-insensitive, canonicalized
  by a total order on constructors, so `object|array|string|number|bool|null`
  and any re-ordering unify to one canonical sum (the R5 order-sensitivity
  hazard, `GN-3.md:501-506`, is dissolved at the `unify` arm, not patched in a
  bridge). This is the recursive node the schema IR cannot represent today
  (`DirectTypeKind` = one `Struct` variant; PROVE-08 Wall 2).
- **`Row{fields: Vec<(Label, Ty)>, tail: RowVar}`** — a record with an open
  tail variable (Rémy/Wand row polymorphism). A grammar `object = "{" pair* "}"`
  with statically-known keys unifies its fields by label; an unknown-key object
  (`pair*` over runtime string keys) closes to `Row{[], tail}` with `tail`
  free — which is *structurally* a map, and is precisely what distinguishes
  `List ≠ Row` (Open R) at the type level with no `{ }`-vs-`[ ]` syntactic gate.
- **`Vec(Ty)`, `Option(Ty)`** — cardinality from `Repeat`/`Optional`, congruent
  unification on the element.

Equality is carried by *one* mechanism: `unify` mutating the union-find. There
is no second equality path. A rule referenced twice does not get "joined"; its
scheme is *instantiated* fresh at each `Ref` (let-generalization, §Generalize),
so two independent uses never alias unless a real constraint forces it — the
soundness property the running overwrite lattice lacks.

## The equality counterexample the family fixes (path:line)

`crates/ir/src/passes/types/constraint/domain.rs:66-80` — `TypeDomain::join`:

```
(Some(existing), Some(ty)) if existing != ty => { self.solved = Some(ty.clone()); true }
```

On disagreement the lattice **silently overwrites** the earlier type and reports
`changed = true`, so AC-3 keeps iterating and the *last* writer in propagation
order wins. This is not equality: it is neither a fixpoint (two constraints that
disagree oscillate until iteration order settles them) nor a clash (a genuine
type error is masked as "refinement"). The comment concedes it — "Re-assignment
to a different type: allow it." The Alt join downstream
(`constraint/revise.rs:116` `join_types`) papers over the residue by lifting
disagreeing branches into `TypeDesc::HeterogeneousAltJoin`
(`type_desc.rs:48-63`) — a boxed-enum obligation — rather than deciding whether
the branches are a lawful `Sum`. Family 1A replaces both: `unify` either
succeeds (the types are equal, or one is a variable that absorbs the other) or
raises a located `UnifyError`; a real N-arm alternation becomes a `Sum` by
variant unification, never a "join obligation." The refutation the family must
survive: does union-find *terminate and match at equal-or-lower derive wall-time*
than this running AC-3 loop on JSON / Sheets / CSS L4? Union-find with
path-halving is near-linear `O(nα(n))`; AC-3 is `O(ed³)` in the worst case and
here re-fires on every overwrite. The wall-time claim is credible but MUST be
measured head-to-head against the running pass, not a strawman.

## Let-generalization over rules (the correctness lever, not a nicety)

Each grammar rule is a `let`-bound scheme. `infer` (the whole-grammar driver,
PROVE-08 `hm.rs` `infer`, generalized) computes each rule's monotype, then
**generalizes** its free variables into a `Scheme(Vec<Var>, Ty)`; every `Ref`
to that rule `instantiate`s a fresh copy. Without this, a polymorphic helper
rule (e.g. a generic `list<T> = "[" T ("," T)* "]"` once grammar closures land,
`project_grammar_closures`) unifies its element variable across *all* call
sites and collapses `list<Number>` and `list<String>` into one clash — the
false-fusion bug. Skinny's live inference has no generalization at all: it is a
monotone structural fold (`skinny/crates/passes/src/lib.rs:178-214`
`Infer::infer_expr`), every node typed once by shape, `Project` discarded at
`:204` (`self.infer_expr(*body)?` throws away `target`+`ty`). The greenfield CSP
has no generalization either (variables are per-`NodeId`, monomorphic). Family
1A is the *only* center that carries polymorphism, and it is the load-bearing
reason to be unification-first: generalization is a natural HM operation
(quantify over free vars not free in the environment) and is awkward-to-absurd
to express as a CSP over a fixed variable set.

## Numeric width & overload — the bounded post-solve (Thread A #3)

Widths are where unification is legitimately *under-determined*: a `->` on a
digit run genuinely admits `U8 ⊑ U16 ⊑ U32 ⊑ U64` (and signed siblings). Family
1A resolves the equality skeleton first (unify pins every non-numeric leaf and
every structural node), leaving a residue of numeric variables each carrying an
**ordered-lattice domain** rather than a single `Con`. The post-solve is a
defaulting walk over the width lattice — the smallest constructor consistent
with all observed literals and any ascription — which is `O(residue)` and needs
no search when the lattice is a chain. The CSP (`csp-solver`) is invoked *only*
when the residue is genuinely a search: mutually-constraining overloads, or a
decode-class that depends on a sum variant (Thread B #4's counterexample). The
family's testable claim: author a `->` micro-fixture under-determined between
u8/u16/u32 and measure **ordered-lattice defaulting vs CSP search** on it — the
prediction is defaulting wins decisively (no backtracking, no arc queue) and is
the honest KISS choice; search is reserved for the non-chain case. This is the
"CSP appears only where unification is undecidable, as a bounded post-solve"
charter made concrete, and it is *pluggable* (Open X): the defaulting order and
the decode-class registry are data behind one seam, with defaulting-lattice and
csp-search as the two swappable implementations.

## Integration seams (path:line — where extant code serves)

- **Seed the carrier**: PROVE-08 `hm.rs` union-find lands verbatim
  (`prototypes/PROVE-08/patch.diff:230-456`); `TyCon`/`Ty`/`unify` grow the
  `Sum`/`Row`/`Vec`/`Option` arms. Certified additive, aarch64-clean,
  no name-gates (`PROVE-08/VERIFY.md` attacks 2–5 HELD).
- **Displace the equality lattice**: `constraint/domain.rs:66` `TypeDomain::join`
  and `constraint/revise.rs:116` `join_types` are the two sites the union-find
  supersedes. The `csp-solver` invocation in `project_types`
  (`crates/ir/src/passes/types/mod.rs`) narrows to the width/overload residue.
- **LOCK-3 sink (mandatory, XL-F)**: whatever record wins lowers through
  `TypeAnalysis::from_ir` (`crates/core/src/backend/driver/analysis.rs:22`,
  reading `ir.types: Vec<(RuleId, TypeDesc)>` + `type_map.scratch_types()`,
  `type_map.rs:95`) and the second `from_ir` at
  `crates/core/src/backend/rust/view/named_types.rs:100`
  (`RustNamedTypes`, admits `TypeDesc::Named`). The union-find's zonked monotypes
  must serialize to `TypeDesc`.
- **Open R widening**: `TypeDesc` (`type_desc.rs:13`) gains `Sum` and `Row`
  beside `HeterogeneousAltJoin`/`Vec`; `StructRegistry`/`LayoutKind`
  (`crates/ir/src/registry/struct.rs:58`, variants
  `Struct/TaggedEnum/UntaggedEnum/NewtypeWrapper`) gains a `Row` layout so
  `List ≠ Row` populates and lowers — closing the masked `json_templates`
  fallback (R6 §5, `research/R6-value-type-plane.md`).
- **TS mirror (mandatory, evidence bar)**: `type_desc_to_ts`
  (`crates/core/src/backend/ts/projection.rs:11`) is the isomorphic sink; a `Row`
  lowers to a TS object/`Record` type, a `Sum` to a discriminated union. Both
  backends consume the SAME `TypeDesc` — the record is backend-agnostic by
  construction (Constitution iii, `backend-agnostic-types`).
- **Measurement substrate (XL-A)**: skinny@HEAD `Infer::infer_expr`
  (`skinny/crates/passes/src/lib.rs:178`) is the running fold to race against for
  the derive-wall baseline; its `Project`-discard at `:204` is the exact wire the
  family re-threads.

## Prior art (citations)

- **Algorithm W / union-find HM**: the path-halving `find` + constructor-binding
  `unify` is textbook Damas–Milner; rustc's HM-adjacent inference uses a
  union-find `InferCtxt`/`TypeVariableTable` (rust-lang/rust
  `compiler/rustc_infer`) — the reference for *let-generalization over a
  recursion graph* and for canonicalization of inference vars at region/scope
  boundaries.
- **Row polymorphism**: Rémy "Type inference for records in a natural extension
  of ML" and Wand's row-variable formulation; Leijen's *Extensible records with
  scoped labels* (Koka) is the cleanest `unify`-over-rows with an open tail — the
  exact mechanism for `Row{fields, tail}` and for distinguishing a closed record
  from an open map. This is what makes `List ≠ Row` a *type* fact, not a syntax
  gate.
- **Recursive sums by set-equal unification**: GHC's `TcUnify`/`unifyType` and
  its treatment of data constructors; the order-insensitive variant reconcile is
  the standard nominal-sum unification.
- **Ordered-lattice numeric defaulting**: Haskell's defaulting rules
  (Report §4.3.4) and rustc's integer-literal fallback (`ty::IntVar` →
  default `i32`) are the precedent for *lattice defaulting over search* — a
  post-solve chain walk, not a CSP.
- **When to reach for CSP/e-graph**: egg/egglog and MiniZinc/Z3 are the
  precedent for the *bounded* case only — genuine search over non-chain overload
  sets. The family's stance (unify first, search the residue) mirrors rustc's
  own architecture: HM carries equality; trait selection (a bounded search) runs
  after, not instead.

## Named risks & counterexamples

- **R-A1 · derive-wall regression.** If union-find + generalization is *slower*
  than the running AC-3 fold on real corpora, the family loses its headline
  claim. Must be measured on JSON / Sheets / CSS L4 against the live pass, HEAD-
  pinned (XL-H). Counter-hypothesis: generalization's fresh instantiation per
  `Ref` could blow up on a deeply-shared rule graph — bound it with scheme
  memoization.
- **R-A2 · Row for map-shaped objects.** A generic JSON object is `pair*` over
  *runtime* keys; its row has an empty known-field set and a free tail. If the
  sink (`StructRegistry`) cannot represent "open row = map," the widening does
  not close the `json_templates` fallback and Open R fails at the lowering seam,
  not the inference seam. The prototype must lower a free-tail `Row` all the way
  through `TypeAnalysis::from_ir` to emitted output.
- **R-A3 · decode-class depends on sum variant.** A leaf whose decode class is a
  function of which `Sum` arm was taken (Thread B #4) is NOT a pure equality —
  it is exactly the residue the bounded post-solve must own. If the family tries
  to carry it in union-find it either over-constrains (false clash) or needs a
  second store — which would concede the CSP-first critique. The clean answer:
  the sum-variant is unified; the decode-class is a post-solve dispatch keyed by
  the resolved variant. Prove it resolves soundly as a post-solve.
- **R-A4 · width under an ascription that conflicts with a literal.** `-> u8`
  ascribed on a rule whose literal is `256` must be a *located* clash, not a
  silent widen. The defaulting walk must fail explicitly (no silent epsilon,
  `no-silent-epsilon`).
- **R-A5 · zonk-to-TypeDesc lossiness.** If the union-find's richer monotype
  (open rows, quantified schemes) cannot round-trip to the flatter `TypeDesc`
  sink without loss, the two backends diverge. The serialization must be total
  and both sinks (`analysis.rs:22`, `projection.rs:11`) must agree.

## What a prototype must test to move this family

1. **Union-find equality vs the running lattice, head-to-head.** Race the
   union-find carrier (with `Sum` + generalization) against the live
   `project_types` AC-3 pass on JSON / Sheets / CSS L4: prove it *terminates*,
   *matches* the resolved types, and does so at *equal-or-lower derive wall-time*.
   Baseline is the running pass at a named HEAD, not a strawman. Report LOC-deleted
   (`domain.rs:66` join + `revise.rs:116` join_types displaced) and empty
   runtime-surface `git diff` (Constitution v — the plane costs zero at runtime).
2. **`List ≠ Row` populates and lowers (Open R).** Build a free-tail `Row` for a
   generic `pair*` object, add `LayoutKind::Row` + `TypeDesc::Row`, and show it
   lowers through `TypeAnalysis::from_ir` AND `type_desc_to_ts` — closing the
   masked `json_templates` fallback. Make structural fidelity orthogonal to
   engine choice.
3. **The width micro-fixture (Thread A #3 / Open X).** Author a `->` fixture
   genuinely under-determined between u8/u16/u32; implement BOTH ordered-lattice
   defaulting and csp-search behind one seam; measure and decide independently of
   the equality carrier. Evidence = one pluggable seam, two swappable impls.
4. **Generalization prevents false fusion.** A grammar with a rule referenced at
   two sites requiring incompatible instantiations must type without a spurious
   clash under the family's `instantiate`-per-`Ref`, and MUST clash (located)
   under a naive monomorphic carrier — proving generalization is load-bearing,
   not decorative.
5. **Sum canonicalization is order-insensitive.** Two differently-ordered
   `value` alternations unify to the identical canonical `Sum` (R5 dissolved at
   the `unify` arm).
