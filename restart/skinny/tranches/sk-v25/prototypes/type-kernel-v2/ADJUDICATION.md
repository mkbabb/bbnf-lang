# Type-kernel v2 apotheosis adjudication

Status: **superseding evidence; v2 is rejected for production adoption**.

This document supersedes the judgments in `CRITIC-A.md` and `REVIEW-B.md`; it
does not edit or erase them. It was produced by the fresh third adjudicator on
`gpt-5.6-sol` at `xhigh`, against repository HEAD
`af15f63e0d2d3d719938c13b906a50acbb92ea3b`. Both reviews were treated as
claims, not authority.

The adjudicator read the binding SK-V25 handoff, prompts, lock constitution,
inventory, every non-target file in this prototype, and the live production IR,
type-projection, pipeline, layout-consumer, and emitter paths. Counterexamples
were built outside the repository at
`/tmp/bbnf-type-kernel-v2-adjudication.QxcqeP`; prototype source was not changed.

## Verdict and convergence

**REJECT production adoption.** The checked-in suite is green, but the green
suite coexists with semantic unsoundness, incompleteness, non-normalized
solutions, broken qualified schemes, non-total budgets, non-causal provenance,
a non-physical object called an Apple ARM64 layout, invalid code generation, and
zero production connectivity.

Convergence is stated against gates, not as an aesthetic percentage:

- **0/5 v2 load-bearing authority families close.** Store construction fails its
  recursive-binder contract; solve fails rows/occurs/normalization; schemes fail
  lacks and environment rigids; layout is not physical; projection is neither
  target-safe nor a real production consumer.
- **0/8 BB-1 born-RED rows close in full.** Some mechanisms are useful evidence,
  but no constitutional row is wholly satisfied. In particular, algebraic laws,
  order-independent semantic completeness, row occurs, no-fallback behavior,
  one immutable production solution, and real consumers remain RED.
- Therefore production-adoption convergence is **0% by gate closure**. The
  critics' 35%, 39%, and 55–60% estimates have no defined denominator and are
  not adopted.

The retained evidence is narrower: safe opaque store-scoped handles, immutable
interning, cross-store and duplicate-label rejection, ordinary type occurs,
elementary equality/row examples, private layout-plan fields, simple
alpha-stable fingerprints, deterministic benign generation, and 21 checked-in
tests are real. None repairs a failed load-bearing family.

Ruling key: **CORRECT** means the numbered fact was reproduced or follows
directly from executable/source truth; **INCORRECT** means the fact as written
contains a disproved assertion; **UNPROVEN** would name a deciding experiment.

## Critic A: numbered-fact adjudication

| # | Ruling | Proof |
|---|---|---|
| A1 | **CORRECT** | `src/build.rs:109-144` represents binder validity only as maximum required depth and pops only after the callback returns. The public probe accepted `mu outer. mu inner. outer`, rejected a compositionally prebuilt outer reference as `BinderEscapes`, and, after a caught callback panic, still accepted `builder.bound(leaked_binder)`. |
| A2 | **CORRECT** | `src/solve/zonk.rs:89-110` and `:201-218` insert row IDs into a traversal-global set and never remove them. The sibling witness resolved to `(record{a:I64,c:I64\|closed},record{b:I64\|?r0})`; the second sibling is not zonked. |
| A3 | **CORRECT** | Open/open unification creates `Tail::Residual` at `src/solve/state.rs:224-240`; `unify_tails` only accepts identical residual numbers at `:264-300`. Two compatible joins followed by their common equation failed with residuals `!r0` and `!r1`, although `{a,b,c,d \| s}` is a model. |
| A4 | **CORRECT** | `Solution` stores lacks at `src/solve.rs:147-154`, but `Scheme::generalize` reads only `resolve` at `src/scheme.rs:64-99`; `poly_row` at `:197-219` carries only the tail identity. After generalizing `r lacks x`, instantiation and equality with `{x:I64}` succeeded. |
| A5 | **CORRECT** | Non-generalized metas become `PolyType::Rigid` at `src/scheme.rs:164-173`, and every rigid instantiation returns `DanglingMeta` at `:222-246`. The environment-held-meta probe reproduced that exact result. |
| A6 | **CORRECT** | Origin collection and constraint sorting occur before `Solver` and `tick` at `src/solve/state.rs:4-29`; sort keys recursively render terms at `:615-628` through `src/term.rs:126-170`. `Solution::resolve` calls `load(..., usize::MAX)` at `src/solve.rs:156-174`. Both 20,000-deep, 512-KiB-stack probes aborted with stack overflow (exit 134), rather than returning typed exhaustion. |
| A7 | **CORRECT** | `Abi` is copied into the result but never participates in lowering (`src/layout.rs:40-57`). `Layout` has no total size, aggregate alignment, offsets, padding, tag width, or niche (`:10-24`), and `Str`, `List`, functions, and `Mu` collapse to `Pointer` (`:73-76`). The native Apple ARM64 probe measured `String`, `Vec<String>`, and `Option<String>` at 24 bytes/align 8 while v2 returned `Pointer`, `Pointer`, and `Tagged[Zero, Pointer]`. |
| A8 | **CORRECT** | `valid_name` checks ASCII spelling but not target keywords (`src/project.rs:50-55`); record fields are interpolated at `:62-69` and `:163-172`; Rust singleton tuples omit the comma at `:116-123`. Generated Rust keyword, bad-label, and normalized-collision files failed `rustc`; TypeScript `class` and `bad-label` failed `tsc`; `(i64)` compiled and accepted integer `1`. |
| A9 | **CORRECT** | `tests/fixtures.rs:8-55` imports no `bbnf_ir::IrNode`, builds invented terms, and calls `solve` with `[]` at line 48. The actual production mapping is defined over `IrNode` at `crates/ir/src/passes/types/generate.rs:143-420`; the disposable actual-IR probe produced `Literal=Span`, `Regex=Span`, `Epsilon=Span`, `Negate=Tuple([])`, and `Map=Bool`, contradicting the v2 aliases. |
| A10 | **CORRECT** | `GrammarShape::from_semantic` maps every non-unit primitive to `Scalar` and every function to `Function` without its signature (`src/shape.rs:26-46`). The probe showed `I64 == Bool` and `(I64)->Bool == (Bool,Bool)->I64` in this classifier. It is a lossy shape classifier, not semantic equivalence. |
| A11 | **CORRECT** | `CrossStore`, `DanglingType`, and `Exhausted` carry no origin (`src/error.rs:23-40`). Conversely, the solver pre-collects all input origins at `src/solve/state.rs:9-14` and clones that flat set into failures, e.g. `:304-345` and `:497-514`. A mismatch included an unrelated tautology's origin; a step exhaustion had none. |
| A12 | **CORRECT** | The success loop permutes four constraints (`tests/solve.rs:125-163`), so its mathematical ceiling is 24 orders; replay found exactly 24 despite 1,000 iterations. The failure loop alternates only forward/reverse (`:216-241`), and the 256 panic cases repeat one shape (`tests/public_api.rs:8-43`). Thus these counts do not cover rows, schemes, budgets, layout, or emitters. The critic's separate ordinary-equality gate is not needed for this ruling. |

## Reviewer B: numbered-fact adjudication

| # | Ruling | Proof |
|---|---|---|
| B1 | **CORRECT** | `occurs_row` examines only row tails (`src/solve/state.rs:478-495`) and never traverses `row.fields`. The infinite equation `{a \| r} = {a, x:{\|r}}` returned `Ok`, producing a self-containing solved row instead of `RowOccurs`. |
| B2 | **CORRECT** | Same residual witness and source as A3. Four source rows formed two solver residuals; the later compatible join failed solely because residual identities differed. |
| B3 | **CORRECT** | Same sibling witness and source as A2. The exact unresolved second sibling was reproduced. |
| B4 | **CORRECT** | Same binder witnesses and source as A1. Nested vacuity is accepted, while a prebuilt outer reference under a new binder is rejected because indices are not shifted compositionally. |
| B5 | **CORRECT** | Same native Apple ARM64 evidence and source as A7. On `aarch64-apple-darwin`, the three Rust values are 24-byte values, not v2 pointer leaves. |
| B6 | **INCORRECT AS WRITTEN** | The load-bearing emitter defect is real, but one named subclaim is false: TypeScript 5.9.3 accepts `export type type = number` (exit 0). Corrected fact: Rust rejects top-level `type`; Rust and TypeScript reject `bad-label`; TypeScript rejects the keyword `class`; Rust singleton `(i64)` compiles as scalar `i64`, losing tuple semantics. |
| B7 | **CORRECT** | Same environment-rigid witness and source as A5. There is no owned rigid substitution context, so lawful non-generalization yields an unusable scheme. |
| B8 | **CORRECT** | The 10,000-lacks/zero-step probe returned `Ok`. Lacks seeding and insertion at `src/solve/state.rs:28-34`, `:79-87`, and `:358-375`, finish/zonk at `:517-555`, public rendering/resolution at `src/solve.rs:156-199`, and preprocessing are not comprehensively charged. The origin claims are proven as in A11. |
| B9 | **CORRECT** | `tests/fixtures.rs:8-55` supplies names plus invented terms, not actual nodes or production constraints. Production grounds literal/regex/epsilon to `Span` (`crates/ir/src/passes/types/generate.rs:155-162`), makes negate `Tuple([])` (`:339-346`), maps through `FnDescriptor` return types (`:360-378`), and joins token-dispatch continuations (`:380-416`). Production `TypeDesc` also contains `Span`, exact integer widths, `Enum`, `BoxedEnum`, `Named`, and heterogeneous-join obligations (`crates/ir/src/types/type_desc.rs:13-64`) for which v2 has no bridge. |
| B10 | **CORRECT** | Repository search outside the prototype found no v2 reference. `Cargo.toml:10-12` has zero dependencies, confirmed by `cargo metadata`. Production invokes its existing `project_types` for Rust/VM/TS/WASM at `crates/core/src/pipeline/compile/target.rs:25-123` and `crates/core/src/backend/driver/analysis.rs:132-164`; emitters consume `ir.types`, `type_map`, `struct_registry`, and `payload_layouts`. v2 consumes none of these and supplies none of them. |

No numbered fact remains UNPROVEN. B6 is the single false composite statement;
its corrected emitter defect is independently production-blocking.

## Independent facts established by the adjudicator

### I1 — production silently discards type-solver failure

**CORRECT, independent, production-blocking.** The live production path calls
`let _ = system.csp.propagate()` twice at
`crates/ir/src/passes/types/mod.rs:63-68` and `:95-128`, even though
`Csp::propagate` returns `Result<(), Unsatisfiable>`
(`crates/csp-solver/src/csp/solve.rs:15-25`). It then substitutes
`TypeDesc::Span` for unsolved child variables at
`crates/ir/src/passes/types/mod.rs:184-232`. This is exactly the constitutional
combination of an ignored solver result and a masking `Span` fallback. V3 must
return a typed error; it may not preserve this path behind compatibility code.

### I2 — target normalization collisions are accepted

**CORRECT, independent extension of the emitter finding.** Distinct legal row
labels `foo_bar` and `foo__bar` both normalize through `upper_camel`
(`src/project.rs:199-211`) to `FooBar`. V2 emitted two identically named enum
variants; `rustc` failed with E0428. Target allocators therefore need a collision
table after normalization, not merely a keyword list.

### I3 — Goldilocks structure and test isomorphism are RED

**CORRECT.** `src/solve/state.rs` is 696 lines and owns preprocessing, sorting,
budgeting, loading, equality, type binding, row binding, lacks, both occurs
checks, diagnostics, rendering, normalization, and finalization. This is a
semantic god-module, not merely a long file. Conversely, `src/error.rs`,
`id.rs`, `project.rs`, `shape.rs`, `term.rs`, `solve/state.rs`, and
`solve/zonk.rs` have no source-isomorphic external test path. `tests/fixtures.rs`
and `tests/public_api.rs` are cross-cutting suites, not counterparts. The exact
four-way split proposed by critic A is a design suggestion rather than a proved
unique answer, but the god-module and non-isomorphism facts are proved.

## Required explicit domain rulings

### Actual `IrNode`, `TypeDesc`, and connectivity

The fourteen v2 fixture names are not a bridge. The important mismatches are:

| Production node | Live production meaning | V2 fixture |
|---|---|---|
| `Literal`, `Regex`, `Epsilon` | all ground to `TypeDesc::Span` | `Str`, `Str`, `Unit` |
| `Seq` | span collapse or projected tuple under production flags | one invented `(Str, Bool)` |
| `Alt` | branch join, possibly a named heterogeneous obligation | one closed variant |
| `Repeat` | span/option/vector depends on bounds and vector context | one `List<Str>` |
| `Ref` | target scalar or `BoxedEnum`; vector context uses `Enum` | `Str` |
| `Skip`, `Next`, `Minus` | project the selected production child | three unrelated concrete aliases |
| `Negate` | `Tuple([])` | `Unit` |
| `Map` | `FnDescriptor` return (`Enum`, numeric, span, or declared type) | semantic function `(Str)->Bool` |
| `OptionalWhitespace` | transparent | `Str` |
| `TokenDispatch` | join of mapped continuations plus fallback | closed known/fallback variant |

Production `TypeDesc` is itself a backend projection vocabulary, not an HM term
language: it has no metavariables, rows, functions, explicit records, or
recursive binders. V2's semantic vocabulary, conversely, lacks production
`Span`, most integer widths, `Named`, `Enum`, `BoxedEnum`, and heterogeneous
obligations. V3 needs two stages with different invariants—one semantic type and
one derived lowering plan—not two competing inference paths.

Production currently stores parallel mutable authorities on `GrammarIR`:
`types` (`crates/ir/src/types/grammar.rs:40-42`), `type_map` (`:70-74`),
`payload_layouts` (`:257-275`), `struct_registry` (`:344-362`), and
`type_obligations` (`:396-428`). A production cut must replace these with one
sealed `TypeSolution` plus one derived `LoweringPlan`; adopting v2 beside them
would create the forbidden dual path.

### Row algebra and zonking

V2 has three independent row failures: row occurs ignores field types,
open/open solving freezes fresh residuals into rigid public numbers, and zonking
uses a traversal-global visited set. Together they disprove soundness,
completeness, and normalization. A correct v3 must keep source and solver-created
row variables under one flexible substitution authority, carry lacks on
representatives, traverse row fields during occurs, and expose public residual
IDs only after a path-local, memoized final zonk. Required algebra/property gates
cover identity, idempotence, commutativity, associativity, and monotonicity where
defined, plus permutation equivalence over distinct row-heavy systems—not loop
counts over a four-constraint seed.

### Recursive binders

Maximum required depth cannot prove binder occurrence and cannot shift prebuilt
terms. V3 should eliminate the mutable active-binder stack: allocate a
store-scoped `RecVar`, build terms containing that identity, and validate exact
free-variable sets when closing `Mu`. Closing subtracts the current identity;
store finish rejects every remaining free identity. This makes construction
compositional and panic-safe without a shim, `catch_unwind`, or unsafe guard.

### Schemes

V2 implements neither qualified row schemes nor a usable rigid environment.
V3 schemes must be owned values of the form
`forall type_vars, row_vars. lacks predicates => body`. Instantiation must
freshen quantified variables, re-emit lacks predicates, and resolve environment
rigids through an owned rigid context. Generalization must use levels and a
named value restriction; a `Rigid(String)` that always errors is not a context.

### Budgets and provenance

The v2 budget measures only selected recursive solver operations. V3 must use
one operation context across ingestion, validation, ordering, loading, pruning,
occurs, unification, normalization, fingerprinting, layout, and projection. Deep
input traversal must be iterative or use an explicit heap stack so exhaustion is
typed rather than process-fatal. Every failure needs deterministic provenance:
constraint failures carry the minimal reachable proof-DAG slice; boundary and
construction failures carry their named boundary/site. A flat set of all input
origins is complete only in the vacuous sense and is not causal evidence.

### Apple ARM64 physical layout

V2's `Layout` is a semantic representation tree. The `AppleAarch64` label has no
effect, and the result cannot answer size, alignment, field offset, padding,
discriminant, or niche questions. Furthermore, `String` and `Vec` are Rust
library/compiler representations, not an Apple platform ABI contract. The
native result above is scoped to rustc 1.96.0-nightly
`02c7f9bec`, target `aarch64-apple-darwin`.

The KISS-forward v3 rule is: physical layout applies only to kernel-owned,
explicit representations. `PhysicalLayout` records total size/alignment,
field offsets, padding, tag representation, and target/toolchain provenance.
Generated aggregates whose offsets are relied upon use an explicit `repr(C)` or
another pinned representation. `String`, `Vec`, functions, and recursive values
are rejected as opaque unless the kernel first defines an explicit carrier;
they must not silently become `Pointer`. Native `size_of`, `align_of`, and
`offset_of` probes on Apple ARM64 must match the plan. If consumers only need a
semantic tree, name it `LayoutShape` and make no physical claim; do not expose
both meanings under one API.

### Emitters

V3 needs a target-specific identifier allocator, not direct interpolation. It
must validate/escape keywords, quote labels where the target permits, escape
string literals, detect collisions after case/normalization, and emit Rust
singleton tuples with a trailing comma. Hostile matrices must compile generated
Rust and TypeScript, including keywords, punctuation, Unicode policy, empty and
leading-digit cases, normalized collisions, nested rows, variants, functions,
and singleton/empty tuples. Each artifact must carry source digest, semantic
manifest digest, generator version, target/toolchain identity, and the exact
reproduction command.

## Smallest coherent v3 architecture

Do not patch v2. Freeze it as falsified evidence. The next carrier should have
this Goldilocks topology (the test tree mirrors it exactly):

```text
crates/ir/src/type_kernel.rs              crates/ir/tests/type_kernel.rs
crates/ir/src/type_kernel/store.rs        crates/ir/tests/type_kernel/store.rs
crates/ir/src/type_kernel/infer.rs        crates/ir/tests/type_kernel/infer.rs
crates/ir/src/type_kernel/solve.rs        crates/ir/tests/type_kernel/solve.rs
crates/ir/src/type_kernel/solve/unify.rs  crates/ir/tests/type_kernel/solve/unify.rs
crates/ir/src/type_kernel/solve/row.rs    crates/ir/tests/type_kernel/solve/row.rs
crates/ir/src/type_kernel/solve/zonk.rs   crates/ir/tests/type_kernel/solve/zonk.rs
crates/ir/src/type_kernel/scheme.rs       crates/ir/tests/type_kernel/scheme.rs
crates/ir/src/type_kernel/layout.rs       crates/ir/tests/type_kernel/layout.rs
crates/ir/src/type_kernel/project.rs      crates/ir/tests/type_kernel/project.rs
```

- `store`: semantic types, opaque IDs, immutable terms, explicit recursive
  identities; it absorbs v2's sand-sized `id`/`term`/`build` split.
- `infer`: exhaustive, wildcard-free traversal of actual `IrNode` and actual
  `FnDescriptor`; it emits non-empty constraints and source provenance.
- `solve`: the public `Result<TypeSolution, TypeError>`, shared budget/proof
  context, and coordinator. `unify`, `row`, and `zonk` own distinct invariants.
- `scheme`: qualified schemes, levels, rigid contexts, and value restriction.
- `layout`: the one physical, kernel-owned target contract described above.
- `project`: target-safe lowering and artifact provenance. The lossy
  `GrammarShape` authority deletes unless a named consumer earns it as an
  explicitly lossy classifier.

The production value flow is singular:

```text
immutable Grammar/IrNode + host signatures
  -> infer/solve -> immutable TypeSolution
  -> lower(target) -> immutable LoweringPlan
  -> Rust / TypeScript / VM emitters
```

`TypeSolution` owns rule/node semantic types, normalized row facts, qualified
predicates, proof causes, and stable solution identity. `LoweringPlan` owns the
former registry, backend type, payload-layout, and obligation projections. There
is no adapter that writes the old `GrammarIR.types`, `type_map`,
`struct_registry`, `payload_layouts`, or `type_obligations` fields.

### Born-RED v3 gates

1. **Store/binder:** reproduce all three binder witnesses; nested outer use
   succeeds, nested vacuity fails, prebuilt composition shifts lawfully, and a
   panic leaves no semantic state.
2. **Rows/zonk:** reproduce B1/A2/A3; add closed/open/variant laws, lacks merge,
   type- and row-occurs, distinct row-heavy permutations, idempotent repeated
   zonk, and solution-with-no-meta/no-internal-residual assertions.
3. **Schemes:** lacks survives generalize/instantiate; environment rigids remain
   usable and shared; instantiations are fresh; level and value-restriction
   fixtures have named outcomes.
4. **Budget/proof:** zero-step preprocessing fails typed; both 20,000-deep
   programs return typed exhaustion; every error has non-empty attributable
   provenance; unrelated facts are absent from minimal proof slices.
5. **Production semantics:** construct every actual `IrNode` variant, exhaustively
   map the complete live vocabulary, produce non-empty constraints, and compare
   real rule/node results with production fixtures. Adding an `IrNode` variant
   must make the match fail to compile.
6. **Layout:** Apple ARM64 size/alignment/offset/tag probes match explicit
   kernel-owned representations; opaque Rust containers reject rather than
   masquerade as pointers.
7. **Projection:** hostile name/collision/string/tuple matrices compile under
   `rustc -D warnings` and `tsc --strict`; two clean generations are byte-identical
   and provenance-complete.
8. **Atomic production cut:** `project_types` returns/propagates errors; all
   `unwrap_or(TypeDesc::Span)` masking paths and ignored `propagate` results are
   gone; old type sidecars and the old type pass delete in the same cut. No
   feature flag, alias, shim, or dual runtime path.
9. **Consumers/performance:** Rust, TS, and VM consume the same sealed solution on
   BBNF/JSON/CSS/CSV/math/Sheets fixtures; generated Rust and TS execute real
   values; cold compile/allocation/bytes are measured before representation
   freeze. Two independent clean audits close adoption.

## Recommended next implementation cut

The next cut is **P3.0 — v3 kill-carrier, not v2 repair**:

1. Implement the topology above in a new proof carrier that imports actual
   `bbnf-ir` nodes and host descriptors. It is an evidence artifact, not a
   production feature or alternate runtime path.
2. Land gates 1–7 first, including the exact counterexamples in this document,
   hostile compiler consumers, and native Apple ARM64 probes.
3. Only after those gates and a fresh hostile audit, perform gate 8 as one atomic
   breaking transposition in production: move the proved carrier into
   `crates/ir`, switch all four compile targets and emitters to the returned
   `TypeSolution`/`LoweringPlan`, and delete the losing projector and sidecars.
4. Run gate 9 and two clean audits. Until then, v3 is not production authority.

This is the smallest coherent sequence because attempting to adopt only v2's
store, solver, layout, or projector would create either a semantic hole or the
forbidden dual path.

## Exact commands and results

All repository commands ran from `/Users/mkbabb/Programming/bbnf-lang`.

### Checked-in v2 gates

```sh
cargo fmt --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v2/Cargo.toml --check
cargo clippy --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v2/Cargo.toml --all-targets -- -D warnings
cargo test --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v2/Cargo.toml
cargo test --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v2/Cargo.toml
rustc --edition=2021 --crate-type=lib restart/skinny/tranches/sk-v25/prototypes/type-kernel-v2/generated/consumer_payload.rs -o /tmp/libconsumer_payload_adjudication.rlib
npx --yes tsc --strict --noEmit --lib es2022,dom restart/skinny/tranches/sk-v25/prototypes/type-kernel-v2/generated/consumer_payload.ts
```

Result: all exit 0. Clippy emitted only workspace unused-patch warnings. Each test
run passed 21 integration tests; Rust and TypeScript benign consumers compiled.

### Disposable adjudication crate

```sh
cargo fmt --manifest-path /tmp/bbnf-type-kernel-v2-adjudication.QxcqeP/Cargo.toml --check
cargo clippy --manifest-path /tmp/bbnf-type-kernel-v2-adjudication.QxcqeP/Cargo.toml --all-targets -- -D warnings
cargo test --manifest-path /tmp/bbnf-type-kernel-v2-adjudication.QxcqeP/Cargo.toml
cargo test --manifest-path /tmp/bbnf-type-kernel-v2-adjudication.QxcqeP/Cargo.toml -- --nocapture
```

Result: all exit 0; 11/11 grouped adjudication tests pass. Salient outputs:

```text
nested-vacuous=Ok(...)
prebuilt-outer=Err(BinderEscapes)
bound-after-unwind=Ok(...)
sibling-zonk=(record{a:I64,c:I64|closed},record{b:I64|?r0})
residual-join=Err(Mismatch { ... !r0 ... !r1 ... })
field-recursive-row=Ok(...)
reintroduced-lacks=Ok(...)
environment-instantiation=Err(DanglingMeta)
ten-thousand-lacks-zero-budget=true
mismatch-origins=[bind integer, conflicting boolean, irrelevant tautology]
originless-exhaustion=Exhausted { kind: Steps, limit: 0 }
successful-loop-distinct-orders=24
production literal=Span regex=Span epsilon=Span negate=Tuple([]) map=Bool
native String=24/8, Vec<String>=24/8, Option<String>=24/8, pointer=8/8
```

### Stack safety

```sh
cargo run --quiet --manifest-path /tmp/bbnf-type-kernel-v2-adjudication.QxcqeP/Cargo.toml --bin deep_sort
cargo run --quiet --manifest-path /tmp/bbnf-type-kernel-v2-adjudication.QxcqeP/Cargo.toml --bin deep_resolve
```

Both exited 134 with `fatal runtime error: stack overflow, aborting` on explicitly
created 512-KiB threads at depth 20,000.

### Hostile emitters

```sh
cargo run --quiet --manifest-path /tmp/bbnf-type-kernel-v2-adjudication.QxcqeP/Cargo.toml --bin emit_hostile
rustc --edition=2021 --crate-type=lib /tmp/bbnf-type-kernel-v2-adjudication.QxcqeP/out/keyword.rs -o /tmp/bbnf-type-kernel-v2-adjudication.QxcqeP/out/keyword.rlib
rustc --edition=2021 --crate-type=lib /tmp/bbnf-type-kernel-v2-adjudication.QxcqeP/out/field.rs -o /tmp/bbnf-type-kernel-v2-adjudication.QxcqeP/out/field.rlib
rustc --edition=2021 --crate-type=lib /tmp/bbnf-type-kernel-v2-adjudication.QxcqeP/out/collision.rs -o /tmp/bbnf-type-kernel-v2-adjudication.QxcqeP/out/collision.rlib
rustc --edition=2021 --crate-type=lib /tmp/bbnf-type-kernel-v2-adjudication.QxcqeP/out/singleton.rs -o /tmp/bbnf-type-kernel-v2-adjudication.QxcqeP/out/singleton.rlib
npx --yes tsc --strict --noEmit --lib es2022,dom /tmp/bbnf-type-kernel-v2-adjudication.QxcqeP/out/keyword.ts
npx --yes tsc --strict --noEmit --lib es2022,dom /tmp/bbnf-type-kernel-v2-adjudication.QxcqeP/out/field.ts
npx --yes tsc --strict --noEmit --lib es2022,dom /tmp/bbnf-type-kernel-v2-adjudication.QxcqeP/out/ts_keyword.ts
```

Results in order: generator 0; Rust keyword 1; Rust bad-label 1; Rust normalized
collision 1/E0428; Rust singleton 0 with `unnecessary parentheses` and an integer
consumer accepted; TypeScript `type` 0; TypeScript bad-label 2; TypeScript
`class` 2/TS2457.

### Connectivity and host identity

```sh
rg -n "bbnf[-_]type[-_]kernel[-_]v2|type-kernel-v2" crates skinny Cargo.toml --glob '*.rs' --glob '*.toml' || true
cargo metadata --no-deps --format-version 1 --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v2/Cargo.toml | jq -r '.packages[] | [.name, (.dependencies|length), .manifest_path] | @tsv'
uname -m
sw_vers
rustc -vV
cargo -V
node --version
npx tsc --version
```

Result: search had no match; metadata reported `bbnf-type-kernel-v2`, zero
dependencies; host is arm64 macOS 26.4.1; rustc is 1.96.0-nightly
`02c7f9bec` with host `aarch64-apple-darwin`; Cargo is 1.96.0-nightly; Node is
26.0.0; TypeScript is 5.9.3.
