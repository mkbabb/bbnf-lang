# 21 — Type Inference & Resolution: Audit + Architecture

## Verdict

The system described in the architecture documents and the system that exists at
HEAD are two different things, and the gap is load-bearing. The docs promise a
"rank-1 Hindley-Milner principal-scheme core + Pierce-Turner local check/synth +
finite CSP choices," with host-signature unification, bidirectional checking at
annotation/host/chain edges, and generic rules. What is actually wired is a
**monotone lattice propagation over a closed `TypeDesc` enum** — no type
variables, no unification, no principal types, no bidirectional flow, no host
boundary checking, and no inference of `->` map-expression bodies. The CSP solver
and the egraph ARE wired, but neither does type *inference* in the PL sense: the
CSP is a structural fold (Parol-class "AST generation"), and the egraph consumes
`TypeDesc` read-only as a cost/rewrite side-table. The skinny tree has no type
inference at all. The honesty problem is acute because the project's own
`restart/corpora/SOTA.md` flags exactly this misnomer when critiquing Parol — and
then commits it.

## What exists today (WIRED)

**`passes::types` (full tree only) is a CSP lattice fold.**
`crates/ir/src/passes/types/mod.rs:51` `project_types` builds a constraint system
(`generate.rs:69`), runs `system.csp.propagate()` (`mod.rs:67`), and extracts
solved `TypeDesc` into a `TypeMap` keyed by `NodeId`. The "solver" is the generic
`csp-solver` crate used in its monotone-lattice mode
(`crates/csp-solver/src/domain/lattice.rs`, `domain/traits.rs` `LatticeDomain`).
Each variable's domain is `Option<TypeDesc>` — a *singleton*, not a candidate set
(`constraint/mod.rs:4-12`). There is no search, no backtracking, no narrowing of a
set: every `revise` either assigns a concrete `TypeDesc` or leaves the slot `None`
(`constraint/operators.rs:44-63`, `revise.rs:27-35`). This is a worklist dataflow
computation, not constraint *satisfaction*. Calling it "AC-3"
(`constraint/mod.rs:9`) dignifies a fixed-point fold.

**The constraint set is fixed-shape structural projection.**
`generate.rs:146` `generate_node` walks `IrNode` and emits one constraint per
construct: `Seq → project_seq_type` (span-compression + `try_flatten_pair`),
`Alt → join_types` (LUB), `Repeat → Vec<T>`, `Optional → Option<T>`,
`Skip/Next/Minus → ProjectConstraint` (keep one side), `Map → MapConstraint`
(the annotated return type), `Ref → RefConstraint` (bridge to target rule's var).
Every operation is a total function of child types. There are no unknowns to solve
for — the "CSP" computes a value, it does not satisfy a constraint.

**`Ref` propagation is the one genuinely iterative edge.**
`constraint/reference.rs:98` suspends (`Revision::Unchanged`) while the target
rule is unsolved, relying on the solver's `Changed`-bool fixed point to converge.
Rule-to-rule cycles never reach a solved state under that policy, so
`mod.rs:83-129` grounds survivors to `BoxedEnum` and re-propagates. This is the
closest thing to inference in the codebase, and it is a unidirectional scalar push
with a cycle-break fallback — not unification.

**`csp-solver` and `egraph` are both heavily imported and real.**
Census of `use csp_solver`/`use bbnf_egraph` (HEAD): `csp-solver` is consumed from
`ir/passes/types/constraint/*`, `ir/passes/sets/*` (FIRST/FOLLOW), `ir/passes/
csp_strategy/*` (layout/strategy selection), `egraph/csp_scheduler.rs`,
`core/backend/strategy`, and `core/backend/rust/analysis/inline`. `egraph` is
consumed from `ir/egraph/*`, `ir/passes/materialization`, and
`core/backend/rust/analysis/inline`. Both are load-bearing — but for FIRST/FOLLOW
sets, strategy/layout choice, and rewrite/cost extraction. **Neither participates
in type resolution.** The egraph reads `TypeDesc` as a read-only side-table to
decide scalar-vs-compound rewrite eligibility (`ir/egraph/rules/universal.rs:141-
171`, `rules/mod.rs:79-82`); it never produces or refines a type. Type projection
runs as a separate pass; the egraph runs after.

## What is missing or dishonest (the gap)

**1. There is no Hindley-Milner anything. `passes::layout` does not exist.**
The docs route HM + bidirectional + host-signature unification through a module
named `passes::layout` (`ARCHITECTURE.md:852,1082,1092,1609`; `MIGRATION.md`).
`find` for any `layout` directory or `passes/layout` path under `crates/` and
`skinny/` returns nothing. `TypeFacts` (`ARCHITECTURE.md:1092`, "HM + bidirectional
checker") does not exist. The whole §"rank-1 Hindley-Milner principal-scheme core"
(`ARCHITECTURE.md:1807-1839`, `README.md:262-266`) is specification, not
implementation. No `unify`, no `TyVar`, no substitution, no `instantiate`/
`generalize`, no `Scheme`. This is the single largest framing defect: the README's
headline claim ("the internals are the apotheosis ... rank-1 HM + local
bidirectional checking", `README.md:5`) describes vaporware.

**2. `-> map` bodies are not type-checked or inferred.**
`MapExpr` (`ir/types/map_expr.rs:17`) is a full expression tree — `IntLit`,
`FloatLit`, `BinOp`, `FnCall`, `InputProp` — with operators, precedence, and
constant folding. Its *type* is never computed. The Map constraint takes the type
**only** from the explicit `return_type` annotation, falling back to `Span`
(`generate.rs:370-371`: `return_type.clone().unwrap_or(TypeDesc::Span)`;
`constraint/operators.rs:194-213` `MapConstraint` just stamps the annotation). A
grammar author who writes `-> input.len * 2` with no annotation gets `Span`,
silently wrong. `MapExpr::constant_fold` exists but no `MapExpr::infer_type` does.
The arithmetic structure that would make inference trivial (Add of two `F64` is
`F64`; `==` is `Bool`) is present and unused. The most embarrassing single gap:
the tree is built, parsed, folded — and discarded for typing.

**3. `@host fn` signatures do not flow into types and are not checked.**
`HostFnDecl` (`core/src/types.rs:79`, "`@host funcName : ReturnType ;`") is parsed
but its return type reaches the type system only as a hand-written `-> T`
annotation on the call site, recovered by `unwrap_value_ident_str`
(`core/src/lower/value_expr/unwrap.rs`). There is no signature table consulted at
`FnCall { name, args }` sites; `FnCall` has no type rule at all. The promised
`BBNF-HOST-SIGNATURE-MISMATCH` diagnostic (`ARCHITECTURE.md:1609`) cannot fire —
nothing checks a host body against an inferred signature. The "bidirectional check
at each adjacent edge" of `-> f1 -> f2 -> f3` chains (`README.md:266`) is
unimplemented; chains lower structurally with the terminal annotation winning.

**4. Closures / GrammarCall erase to nothing typeable.**
Grammar closures are beta-reduced at *lowering* time
(`core/src/lower/expression/closures.rs:48` `lower_grammar_call`): the body is
re-lowered with parameters bound to argument CST views, producing inlined
`IrNode`. By the time `project_types` runs, the closure is gone — no `GrammarCall`
node in the typed IR, no polymorphic scheme, no per-call instantiation. Fine for
monomorphic expansion but it forecloses the "V1 generic rules `Object<V>`" with
principal schemes the docs promise (`README.md:262`, `MASTER-PLAN.md` C.W2).
Generics-as-macro-expansion is a defensible KISS choice, but the docs sell it as
HM polymorphism.

**5. Heterogeneous Alt joins are LUB-to-`BoxedEnum`, dressed as obligations.**
`join_types` (`revise.rs:116-142`) returns the common type if branches agree (with
an AU.2.5 factored-prefix homogeneity escape hatch), else lifts to
`TypeDesc::HeterogeneousAltJoin(branches)` — which `is_boxed_enum_layout` collapses
to the same `Box<Enum>` codegen (`type_desc.rs:206`). The `TypeObligation` stream
(`obligation.rs`) renames the old silent `BoxedEnum` fallback into a "named
obligation." This is honest about *what* collapsed but does not *resolve* it: real
sum-type inference would compute a tagged union with typed variants, not erase to a
boxed enum. The obligation is a diagnostic, not a type. The AU.2.5 escape hatch
(`revise.rs:125-134`, stripping `Tuple([Span, T])` prefixes from trie-factored
alternations) is a CSS-named special case (`absoluteLengthUnit`, `namedColor`,
`revise.rs:104,152`) leaking into the generic join — a Lock-14 smell.

**6. The skinny tree has zero type inference.**
`skinny/crates/ir` is `lib.rs` (678) + `cost.rs` (342). No `project_types`, no
`TypeMap`, no `TypeDesc`. Types are degraded to `SpanKind`/`TapeKind`/
`DirectBuildTypeRef::F64` (`skinny/crates/ir/src/lib.rs:391-504`).
`skinny/crates/passes` has no type pass (its `infer_*` functions are structural
shape/capacity inference, not type inference). This is the documented SK-V18 gap:
the skinny IR parser rejects Sheets `->`/`?w/` projections. So the "typed value
layer" is full-tree only, and the skinny proving tree — the one that must beat
SOTA — has no typing chain to carry `->` projections.

**7. `Named(StringId)` is an opaque sink.**
The lattice has no rule for `Named` beyond "treat as compound, wrap in `BoxedEnum`"
(`reference.rs:144-146`). There is no row/record type, no field projection at the
type level. `StructRegistry` (`ir/registry/struct.rs`) projects struct layouts
*after* typing from `IrNode` shape, not from a row-polymorphic type. So `obj.field`
path access (`core/src/path/type_check.rs`) resolves against a post-hoc layout
table, not an inferred record type — the inverse of the row polymorphism the
architecture gestures at.

## Architecture: full type resolution for BBNF

The reasonable target is **not** full Damas-Milner. BBNF rules are first-order,
generics are macro-expanded, and the value algebra (`MapExpr`) is a closed set of
scalar operators. The honest, achievable design is **bidirectional checking over a
finite type lattice with a small unification core for host/chain edges** — keep
the lattice fold for grammar structure (correct and fast), and add the three
inference capabilities actually missing: map-expr synthesis, host-edge checking,
and sum-type resolution.

- **Keep the lattice fold for grammar structure.** `Seq/Alt/Repeat/Optional/Skip`
  projection is a total function of child types; it needs no unification. This is
  the Parol-class structural derivation and genuinely the right tool. Stop calling
  it HM or AC-3; call it *structural type projection* (matching `SOTA.md`'s own
  correct vocabulary).

- **Add `MapExpr::synth(&self, env) -> TypeDesc`.** Bottom-up synthesis over the
  closed operator set: numeric `BinOp` of two `F64` is `F64`; comparison is `Bool`;
  `IntLit`/`FloatLit`/`BoolLit` are immediate; `InputProp{len}` is a width-typed
  integer; `FnCall` looks up the host signature. ~80 lines, trivially principal
  because the algebra is monomorphic, and it eliminates the `unwrap_or(Span)` lie.
  Annotations then *check* (`synth == annotated`) rather than *replace* — restoring
  the `ARCHITECTURE.md:1820` "annotations check inferred types rather than
  bypassing them" contract that is currently violated.

- **Introduce a `HostSigTable: name → (params: Vec<TypeDesc>, ret: TypeDesc)`**
  populated from `@host fn` declarations, consulted at `FnCall` synthesis. Check
  arg types against params (bidirectional: args synth, params check). This is the
  one place a tiny first-order unifier earns its keep — for `-> f1 -> f2` chains,
  unify `f1`'s output with `f2`'s input. Wire the `BBNF-HOST-SIGNATURE-MISMATCH`
  diagnostic to a real failure.

- **Resolve heterogeneous Alts to typed sum types, not `BoxedEnum`.** Replace
  `HeterogeneousAltJoin` with `TypeDesc::Sum(Vec<(VariantTag, TypeDesc)>)` that the
  registry consumes directly, so the tagged union is inferred, not post-projected.
  The obligation stream stays as a diagnostic surface but no longer doubles as the
  type.

- **Row-typed records for struct projection.** Promote `StructRegistry` from a
  post-typing layout pass to a `TypeDesc::Record(Row)` produced *during* projection,
  so `obj.field` path resolution and CSS visitor access type-check against an
  inferred row rather than a side-table reconstruction. Row polymorphism is the
  right tool for "same field across heterogeneous declaration shapes" (CSS) and
  "generic `Object<V>`", and composes with sum types for the Alt case. Highest
  leverage upgrade, and bounded: closed-world rows (no extension variables) suffice
  for the grammar domain.

- **Egraph stays cost/rewrite, not typing.** Do NOT push typing into the egraph;
  its read-only `TypeDesc` side-table dependency is correct. After the inference
  upgrades, the egraph consumes richer types (Sum, Record) for better rewrite
  eligibility — a pure consumer relationship.

- **CSP stays finite choices, not types.** Per the docs' own correct statement
  (`README.md:262`), CSP solves bounded implementation choices (layout, recognizer
  eligibility, materialization). Keep it there. Re-home the type lattice fold *out*
  of the `csp-solver` dependency and into a plain worklist — using the generic CSP
  crate for a singleton-domain monotone fold is a category error that misleads
  every reader into believing search happens.

## Wave plan ("inference composes types, never loses them")

- **W0 — Honesty + vocabulary.** Strike "rank-1 Hindley-Milner principal-scheme
  core," "AC-3," and "HM + bidirectional" from `README.md`, `ARCHITECTURE.md`,
  `HANDOFF.md` where no such code exists. Rename the pass *structural type
  projection*; document the `csp-solver` use as monotone lattice fold. Disclose
  that map-expr bodies, host signatures, and chains are currently un-inferred.
  Unblocks every downstream claim from being a lie.

- **W1 — Map-expr synthesis.** `MapExpr::synth` + annotation-as-check. Kills the
  `unwrap_or(Span)` default. Every `->` body now reaches the emitter with a real
  type; nothing is lost. (Directly serves the typed-materialization invariant.)

- **W2 — Host signature table + edge checking.** `HostSigTable` from `@host fn`;
  bidirectional `FnCall` check; first-order unifier for `-> f1 -> f2 -> f3` chains;
  wire `BBNF-HOST-SIGNATURE-MISMATCH`. The chain is the documented feature that
  currently does not exist.

- **W3 — Typed sum + row records.** `TypeDesc::Sum` and `TypeDesc::Record(Row)`;
  registry consumes them during projection; path/visitor access type-checks against
  the row. Retire `HeterogeneousAltJoin`-as-type. Remove the CSS-named AU.2.5 escape
  hatch by making prefix-Span stripping a structural rewrite the generic join
  already sees.

- **W4 — Skinny parity.** Port the structural projection + W1–W3 to skinny
  `ir`/`passes` so the proving tree carries `->` projections (closes the SK-V18
  Sheets-rejection gap). Gate for any honest >SOTA claim, since the benchmarked tree
  must type what it parses.

- **W5 — Re-home the lattice off `csp-solver`.** Replace the singleton-domain CSP
  with a plain worklist fold; keep `csp-solver` for genuine finite-choice solving
  only. Removes the "AC-3 type inference" category error at the source.

## Load-bearing claims to verify

1. `passes::layout` (the documented HM+bidirectional home) does not exist at HEAD.
2. `FnDescriptor::Expr` map type comes only from the annotation, defaulting to
   `Span` — map-expr bodies are never typed.
3. The egraph and CSP are wired but neither does type inference; the type pass is a
   monotone lattice fold over a singleton `Option<TypeDesc>` domain.
