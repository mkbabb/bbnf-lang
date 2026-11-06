# LOCK-1 / Family 1A — UNIFICATION-FIRST: the HM core grows (pass-1 research)

## 0. Thesis

The grammar type plane is **one Hindley–Milner inference over an open, structural type
language**, in which every `->` projection drives unification, every rule is a
let-generalized type scheme, and document shapes are **row-polymorphic records**. First-order
unification is the spine because it is decidable, terminating, and yields *principal* types —
the strongest inference a compile-time-only plane can want. Constraint solving (the vendored
`csp-solver`) appears **only** where the judgment is genuinely not a structural equality:
numeric widths, decode-class/host-overload choice, and capacity. That two-phase shape —
generate structural constraints by unification, defer the non-structural ones, solve the
deferral with a bounded finite/lattice CSP — is exactly the **HM(X) / OutsideIn(X)** framework
(HM parametrized over a constraint theory X), with X instantiated as `{ equality, row-lacks,
width-lattice, overload-finite }`. The family's differentiator over the extant GN-3 floor
(a closed 5-leaf `TyCon` union-find that re-lexes arrow text) is **rows + let-generalization +
recursion-by-nominal-fold**, which cure Wall 1 by *openness* rather than by forcing
per-document grammars to be fully closed.

This family develops that route alone; it does not survey siblings (1B one-store, 1C
bidirectional-local, 1D types-as-eclasses) except to name a boundary where one shares a
mechanism.

## 1. The mechanism, made concrete

### 1.1 The type language (grows from BOTH extant enums, fused into one)

Skinny carries **two disjoint, half-built type representations** that Family 1A merges:

- `passes::types::Type` (`skinny/crates/passes/src/lib.rs:235-243`): `Var(TypeVarId) |
  Builtin(BuiltinTy) | Seq(Vec<Type>) | Alt(Vec<Type>) | List(Box) | Option(Box) | Rule(RuleId)`.
  It already has a `Var` arm and the structural constructors — but inference never unifies (it
  is a pure fold, §1.3).
- `type_plane::hm::Ty` (`prototypes/PROVE-08/patch.diff:255-260`): `Var(u32) | Con(TyCon)` with
  a *real* union-find `unify`, but over a **closed 5-leaf** `TyCon {Bool,F64,U8Tag,
  StringDecoded,StringSpan}` and no composite arms.

The fused HM monotype (one type, in `passes`, consumed by `codegen`):

```
τ ::= Var(v)                          -- unification variable (union-find root)
    | Leaf(c)                         -- Bool | F64 | U8..U64 | StrSpan | StrDecoded | Unit | Bytes
    | List(τ)                         -- from ExprKind::Repeat   → Vec<τ>
    | Option(τ)                       -- from ExprKind::Optional → Option<τ>
    | Prod(ρ)                         -- from a struct projection (record / product)
    | Sum(ρ)                          -- from ExprKind::Alt (the value = obj|arr|… sum; Wall 2)
    | Rule(id)                        -- nominal fold point: the µ-guard AND the let-scheme site
ρ ::= { l₁:τ₁, …, lₙ:τₙ | r }         -- ROW: known labels + a row-variable tail r  (or • closed)
```

`Prod`/`Sum` share the row `ρ`: a product is a record of fields, a sum is a record of variants.
The **row-variable tail `r`** is the family's keystone — it is the type-theoretic object that
`DirectTypeKind::Struct { unknown_fields: Skip }` (`schema/direct_schema.rs`) today expresses as
a *policy enum*. An open row `{ statuses: List(Rule(tweet)) | r }` types a `TwitterSearch`
that declares `statuses` and lets `r` absorb `search_metadata`, `since_id`, … — **Wall 1 falls
by openness, not by exhaustive per-document closure**.

### 1.2 Unification, grown

`hm::unify` (patch.diff:307-353, the proven union-find with path-halving `find`) grows exactly
the arms Robinson's algorithm prescribes:

- `unify(List a, List b) = unify(a,b)`; `Option` likewise (decompose-and-recurse).
- `unify(Leaf x, Leaf y) = x==y ? ok : clash` — unchanged, the honest HM clash (kept as-is).
- `unify(Prod ρ₁, Prod ρ₂)` and `unify(Sum ρ₁, Sum ρ₂)` = **row unification** (§1.4).
- `unify(Rule i, Rule j) = i==j ? ok : clash` — nominal; unification **never crosses a rule
  boundary structurally** (this is the recursion guard, §7-R1).

### 1.3 Activating the DORMANT unification spine (the decisive integration finding)

`passes::types::TypeFacts` (`skinny/crates/passes/src/lib.rs:126-145`) **already declares the
full HM(X) scaffold, populated by nothing**:

```
pub struct TypeFacts { rule_types, node_types,
    subst: Substitution,               // vars: HashMap<TypeVarId, Type>   ← the substitution
    obligations: Vec<TypeObligation> } // { span, expected_from, actual_from, solver_stage } ← deferred X
```

`Substitution` (`:134`) is the union-find map; `TypeObligation` (`:138`) carries a
`SourceSpan` and a **`solver_stage: &'static str`** — a deferred constraint tagged with *which
solver* discharges it. That is the OutsideIn(X) shape (unify now, defer the theory constraints)
sitting inert: `infer()` (`:148`) calls a pure fold that never writes `subst`, never pushes an
obligation, and `Project` discards its type at `:204`. **Consumer-less substrate is forbidden
(Constitution viii / Y.13)** — so activating this dormant spine is simultaneously the family's
mechanism *and* a constitution repair. Family 1A's `infer_expr` populates `subst` by
unification and routes width/overload/capacity judgments into `obligations` under
`solver_stage ∈ {"unify","width-csp","overload-csp"}`.

### 1.4 Row polymorphism (the document-shape engine)

Rows unify by the standard label-splitting rule (Rémy/Wand; Leijen's *scoped labels* variant,
which permits duplicate labels — matching JSON's duplicate keys and `DuplicatePolicy::LastWins`
in `direct_schema.rs`):

```
unify({l:τ | r₁}, ρ₂):  find l in ρ₂ → tail ρ₂';  unify(τ, τ_l);  unify(r₁, ρ₂')
                        l absent, ρ₂ open with tail s → s := {l:τ | s'}  (grow the tail)
                        l absent, ρ₂ closed → clash (missing required field, span-carried)
```

A closed row (`| •`) is a struct with `unknown_fields: Forbid`; an open row (`| r`) is
`unknown_fields: Skip`. The six-arm `value = object|array|string|number|bool|null` unifies —
**order-insensitively** (rows are unordered) — to one canonical `Sum` row, which is precisely
GN-3's R5 order-insensitivity requirement obtained *for free* from row theory rather than from
a hand-written set-equal in `sum.rs`.

### 1.5 Let-generalization over rules (Damas–Milner)

`Rule(id)` becomes a **let binding**: a rule's inferred monotype is *generalized* to a scheme
`∀ᾱ. τ` (quantifying the type/row variables not free in the enclosing environment), and each
`ExprKind::Ref` **instantiates** with fresh variables. This is what lets one generic rule —
`pair = string ":" value`, or a shared `entry` — be reused at Tweet-shape and Instrument-shape
in the same grammar without the two uses unifying their payloads. The extant `seed_rule_types`
(`:172`) already binds `rule_types[id] = Rule(id)` nominally; Family 1A replaces the nominal
seed with `generalize(infer(body))` and the `Ref` arm (`:195-201`) with `instantiate(scheme)`.
Mutual recursion is handled the ML way: seed each SCC rule with a fresh monomorphic `Var`,
infer the bodies, unify, *then* generalize the SCC — never generalize inside its own fixpoint.

### 1.6 The bidirectional projection arm (the `->` driver)

`ExprKind::Project { body, target, ty }` (`ir/src/lib.rs:261`) drives inference in **two modes**:

- **check** (`-> x : T`): synthesize `τ_body`, unify `τ_body ⊑ ⟦T⟧` — an ascription *checks*.
- **synthesize** (bare `-> target`): classify the target literal to a leaf (the proven
  `classify_arrow`, patch.diff:374-406) and unify into the rule's var.

This is the same synth/check split the PROVE-08 `classify_arrow` doc-comment names, lifted from
a string lexer to a real bidirectional rule. It replaces the discard at
`passes/src/lib.rs:204`.

### 1.7 CSP as the bounded post-solve (where unification lawfully stops)

Three judgments are **not** structural equality and are deferred as `TypeObligation`s, then
solved once the HM skeleton is fixed:

1. **Numeric width.** `null -> 0u8`, tags, and downstream layout want the *minimal* width in
   `{u8,u16,u32,u64}` satisfying every literal-and-layout constraint. This is a monotone
   fixed-point / finite optimization, not unification. `csp-solver`'s `domain::lattice`
   **already names types as its consumer** — `BitsetLatticeDomain` doc: *"monotonic CSP (types,
   FIRST/FOLLOW sets)"* (`crates/csp-solver/src/domain/lattice.rs:1`). Width = a lattice var
   growing by `join`; the objective (minimize layout bytes) uses `CostFiniteDomain` +
   `solver::optimize`. This recovers the prototype's **admitted loss** — REPORT §5 hard-maps
   `U8Tag → U32` because "no U8 in the schema scalar set"; the width CSP picks u8.
2. **Decode-class / host overload.** `-> decode_json_string_to_arena(input) : String` (arena-own)
   vs bare `: String` (borrow-span) is an *overload* choice under ambient legality (does the
   field escape? is it reused?). Undecidable inside unalloyed unification (needs the context),
   decidable as a finite CSP once the shape is fixed. Today it is a `contains("decode")` string
   heuristic (patch.diff:388) — a name-gate smell Family 1A replaces with a legality-constrained
   finite variable.
3. **Capacity** (GN-3 R1: `875`, `10_800`, `184`). These are *corpus counts*, not grammar
   facts — Family 1A **concedes** them exactly as GN-3 does: an annotation `-> Vec<T> @cap(875)`
   on the row field, read (never invented) by the width/capacity CSP pass.

The split is principled and citable: **HM decides shape (structural equality); the CSP decides
the annotations that ride on resolved leaves (width, overload, capacity)** — constraint
*generation* (unification) separated from constraint *solving* (finite/lattice), the exact
architecture of GHC's OutsideIn(X) and rustc's fulfillment context.

## 2. The substrate

- **One inference, in `passes`** (not two): grow `passes::types::Type` with `Prod/Sum(Row)`,
  activate `subst`/`obligations`, move the union-find core (proven, patch.diff:255-353) beside
  it; `codegen` becomes a *consumer* of `TypeFacts`, deleting the parallel `type_plane::hm`
  duplicate. Resolves the "two type systems" split R6 §5 documents.
- **Row module** `passes/src/types/row.rs` (new): row representation (sorted label vec + tail
  var), `unify_row`, `lacks`/scoped-label handling.
- **Generalization** `passes/src/types/scheme.rs` (new): `Scheme{ vars, ty }`, `generalize`,
  `instantiate`, SCC-aware `infer_recursive_group`.
- **CSP post-solve** `passes/src/types/solve_annotations.rs` (new): builds a `Csp` over
  `csp-solver` — width slots as `BitsetLatticeDomain`/`FiniteDomain`, overload slots as
  `FiniteDomain`, objective via `CostFiniteDomain` + `OptimizationMode`; reads `obligations`,
  writes resolved widths/decode-classes back into the schema leaves.
- **Sum node in the schema IR** (`schema/direct_schema.rs:52` grows `DirectTypeKind::Sum` — the
  same net-new node GN-3 W1 names; Family 1A's row-`Sum` lowers into it) + a width field on
  `DirectScalar` so u8/u16/u32/u64 survive to emission.
- **Grammar-sourced diagnostics**: `UnifyError`/row-clash carry the arrow's `SourceSpan` (the
  offset already in `RuntimeTypedProjection.offset`, patch.diff:78-88), surfacing "field `text`:
  Bool vs String" against the grammar span — the charter's diagnostics-first requirement, and
  the `TypeObligation.span` field is already there to hold it.

## 3. Integration seams (path:line, extant code that serves)

- Dormant HM(X) scaffold to activate: `skinny/crates/passes/src/lib.rs:126-145`
  (`TypeFacts.subst`, `.obligations`, `TypeObligation.solver_stage`).
- The pure fold to replace with unification: `passes/src/lib.rs:178-231` (`infer_expr`).
- The `->` discard that dies: `passes/src/lib.rs:204`
  (`ExprKind::Project { body, .. } => self.infer_expr(*body)?`).
- Nominal seed → generalized scheme: `passes/src/lib.rs:172` (`seed_rule_types`), `Ref` arm
  `:195-201`.
- Proven union-find core to grow (verbatim leaf half): `PROVE-08/patch.diff:255-353`
  (`Unifier`, `find`, `unify`, `bind`, `resolve`); bidirectional leaf lexer `:374-406`
  (`classify_arrow`).
- `->` in the IR (Wall 3 already fallen at HEAD): `ir/src/lib.rs:261`
  (`ExprKind::Project { body, target, ty }`).
- The `None`-slot the resolved leaf fills: `passes/src/lib.rs:1149-1287`
  (`derive_materialization_plan`, every `DirectBuildField.target = None` today, R6 §5d).
- The 4-way lossy fold to delete (GN-3 W2 shares this): `emit/general_descent/view.rs:20-46`
  (`ProjectionKind`/`classify_projection`) — its 24 call sites read the threaded leaf instead.
- Schema IR to grow the `Sum` node + width: `schema/direct_schema.rs:52-58` (`DirectTypeKind`).
- Renderer that consumes it (Y.13 in-wave consumer): `schema/json_typed_direct.rs:10,313`.
- CSP engine: `crates/csp-solver/src/domain/lattice.rs:1` (types named as consumer),
  `domain/cost_finite.rs`, `solver/optimize.rs`, `config.rs` (`Csp`, `OptimizationMode`,
  `SolveConfig`).

## 4. Prior art (citations)

- **Damas & Milner 1982; Damas 1984 (thesis)** — Algorithm W, let-generalization, principal
  types. The generalize/instantiate spine of §1.5.
- **Rémy 1989; Wand 1987** — row variables for records. The open-tail `ρ` of §1.4.
- **Ohori 1995** — a polymorphic record calculus with *efficient* (kinded) compilation —
  evidence the row plane compiles to zero-cost layout, matching the zero-runtime invariant.
- **Leijen 2005, "Extensible records with scoped labels" (Koka)** — the practical row system
  that admits **duplicate labels**; chosen over Rémy because JSON keys duplicate and skinny
  already has `DuplicatePolicy::LastWins`.
- **Odersky, Sulzmann & Wehr 1999, "Type inference with constrained types" (HM(X))** and
  **Vytiniotis, Peyton Jones, Schrijvers, Sulzmann 2011, "OutsideIn(X)"** — HM parametrized
  over a constraint theory; the generate-then-solve architecture of §1.7. The literal template
  for the `TypeObligation.solver_stage` deferral.
- **rustc `InferCtxt` + fulfillment** — unification eagerly, `obligations` deferred and
  discharged by trait selection; and rustc's **integer-literal defaulting** (`{integer}` → i32)
  is *exactly* the width-inference problem — rustc *defaults*, Family 1A *optimizes* it via CSP.
- **GHC kind system / lacks-constraints** — row absence as a solved constraint, informing the
  closed-vs-open (`Forbid`/`Skip`) discharge.
- **egg/egglog, MiniZinc/Z3** — named as *boundaries*: e-graph congruence is sibling 1D's
  center, not this family's; a general SMT/MiniZinc solver is over-powered where the vendored
  finite/lattice `csp-solver` is the in-house, deterministic consumer (parsimony §1.5).

## 5. Named risks + counterexamples

- **R1 — recursion / occurs-check divergence (make-or-break).** `value = object|array|…` with
  `object = "{" pair* "}"`, `pair = string ":" value` is mutually recursive; naive HM with
  occurs-check *rejects* it as an infinite type. **Resolution:** iso-recursive **nominal
  folding** — `unify(Rule i, Rule j)` compares ids and never descends, so the `Rule(id)`
  indirection (which the extant fold already uses) *is* the µ-fold guard; unification unifies a
  rule's *scheme instance*, whose recursive occurrences are `Rule(id)` references, not
  structure. Counterexample to defeat: self-referential `value` must infer one `Sum` and
  terminate.
- **R2 — principality lost by width defaulting.** A rule generalized *before* its width is
  CSP-resolved could wrongly unify two widths. **Resolution:** stage it (rustc's late integer
  defaulting) — generalize the *structural* scheme only; width/overload vars are monomorphic
  `obligations`, NOT quantified, resolved after generalization. Counterexample: a rule used at
  u8 and u16 must not collapse to one width.
- **R3 — duplicate / absent labels.** Rémy rows forbid duplicates; JSON permits them.
  **Resolution:** Leijen scoped labels (§1.4). Counterexample: `{a:T, a:U | r}`.
- **R4 — let-gen worst-case blow-up.** ML inference is DEXPTIME in pathological nesting
  (Kfoury–Tiuryn–Urzyczyn / Mairson). **Resolution:** grammar rule-DAGs are small and shallow;
  compile-time only (§0 zero-runtime); *measure* derive wall-time, don't assume — GN-3's
  ~230µs is the baseline to hold.
- **R5 — Wall-1 capacity numbers are not grammar facts (GN-3 R1).** Rows recover *fields*, not
  `875`. **Resolution:** capacity is a conceded annotation read by the CSP, never inferred —
  Family 1A does not over-claim.
- **R6 — two type systems must actually merge, not coexist.** If `passes::Type` and
  `codegen::type_plane::hm::Ty` both survive, the family fails its own "one inference" claim
  (and re-creates the R6 §5 split). **Resolution:** the union-find core moves into `passes`;
  `codegen` consumes `TypeFacts`; the `type_plane::hm` duplicate is deleted, not wrapped.

## 6. What a prototype must test to move Family 1A

1. **Recursive-sum inference terminates** (R1): infer `value = object|array|string|number|
   bool|null` with `object`/`array` referencing `value` via nominal `Rule` folds; assert one
   `Sum` of exactly 6 variants, no infinite-type error, bounded wall-time. *The gate.*
2. **Row-polymorphic open record** (Wall 1 cure): `TwitterSearch { statuses: Vec<Tweet> | r }`
   unifies against a shape with extra keys; assert `r` absorbs them and the closed fields
   check — *without* a fully-closed per-document grammar.
3. **Let-generalization reuse**: a generic `pair`/`entry` rule instantiated at two distinct
   field types in one grammar; assert the two payloads do NOT unify (fresh instantiation).
4. **CSP width post-solve recovers the loss**: `null -> 0u8` + a literal forcing u16 elsewhere;
   assert the width CSP emits u8/u16 (not the prototype's forced u32), driven off `obligations`,
   consumed by the emitted `DirectScalar` width.
5. **Bidirectional check + span diagnostic**: an ascribed arrow `-> x : Bool` clashing with body
   synthesis raises a unify error carrying the arrow's grammar `SourceSpan` (diagnostics-first).
6. **One-inference + zero-runtime**: `passes::TypeFacts.subst`/`.obligations` populated, the
   `codegen` duplicate deleted, `git diff` over `crates/runtime/` empty, derive wall-time within
   noise of GN-3's ~230µs. Proves the merge and the compile-time-only invariant simultaneously.
