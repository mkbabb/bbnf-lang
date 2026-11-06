# Production type-integration map

Status: measured seam map for BB-B; **prototype v2 is adjudicated RED at 0/5
load-bearing families and 0/8 BB-1 gates**. A quarantined v3 kill-carrier is
active; it is not production authority. This map prevents a green standalone
crate from being mistaken for production progress.

## Present authority and failure shape

`GrammarIR` is simultaneously the grammar program, mutable pass workspace,
analysis cache, type solution, layout plan, and backend handoff. Its public
fields include syntax (`rules`, `entry`, `strings`, `fns`), inferred semantics
(`types`, `type_map`, `type_obligations`, `struct_registry`,
`type_desc_interner`), and later decisions (`materialization`,
`payload_layouts`, recognizer maps, shape assignments, path resolver, e-class
facts). A consumer can therefore observe or mutate an impossible partial stage.

The present type path is:

```text
mutable GrammarIR
  -> generate_constraints(&GrammarIR)
  -> csp_solver::propagate() result discarded
  -> unresolved rule variables overwritten with BoxedEnum
  -> TypeMap + Vec<(RuleId, TypeDesc)>
  -> StructRegistry + TypeObligation sidecars
  -> path resolver + materialization + payload layouts
  -> backend snapshots / direct GrammarIR reads
```

The decisive defects are structural, not cosmetic:

1. `TypeDomain::join` replaces a different solved value with the right operand;
   it is neither a lawful join nor a proof of refinement.
2. `project_types` discards the propagation result, mutates solver variables to
   break cycles, re-propagates, and publishes several independently mutable
   fields. An exhausted or inconsistent solve has no sealed failure boundary.
3. `FactAuthority` is a read facade over public mutable maps. It does not own,
   version, seal, or prove completeness of facts.
4. semantic types and physical payload decisions share `TypeDesc`-driven paths;
   downstream code can infer layout from a type without a checked lowering
   result.
5. target dispatch repeats the same partial sequence for VM, TS, and WASM while
   Rust reaches it through `prepare_grammar`/`analyze_grammar`. The targets do
   not receive one common completed product.

## Load-bearing production seams

| Seam | Current producer or reader | Required single-path replacement |
| --- | --- | --- |
| Type entry | `passes::types::project_types` | `infer(&Program, &DependencyGraph) -> Result<TypeSolution, TypeFailure>` |
| Rule/node types | `GrammarIR::types`, `GrammarIR::type_map` | private, total queries on a revision-matched `TypeSolution` |
| Public reuse | ad-hoc rule-variable projection | explicit boundary schemes generalized only after solving |
| Diagnostics | `GrammarIR::type_obligations` | success facts or typed failure/proof DAG; no successful defaulting obligation |
| Named shapes | `GrammarIR::struct_registry` | derivation from `TypeSolution`, owned by immutable `AnalysisFacts` or `LoweringPlan` |
| Layout | `compute_payload_layouts(&GrammarIR)` | checked `lower_layout(TypeSolution, AnalysisFacts, AppleAarch64)` returning sealed plans |
| Path typing | `run_path_check` after registry mutation | typed path plan derived from the same solution revision |
| Analysis reads | `FactAuthority<'_, GrammarIR>` | typed fields/methods on a complete immutable `AnalysisFacts` value |
| Backend prep | `TypeAnalysis::from_ir` plus direct IR reads | `LoweringPlan` as the sole backend-neutral handoff |
| Target dispatch | repeated mutation in `finalize_compile` | one fallible stage builder before Rust/VM/TS/WASM branching |

Specific consumers that must move in the atomic cut include:

- Rust: `backend/driver/analysis.rs`, `backend/rust/ir_types.rs`,
  `backend/rust/view/named_types.rs`, shape emitters, path planning, and payload
  emission;
- TypeScript: `backend/ts/emitter/grammar.rs` and
  `backend/ts/projection.rs`;
- shared/VM/WASM: driver sequence/repeat/reference paths, payload planning,
  audit coverage, call strategies, and target finalization;
- generated/runtime: `bbnf-path`, emitted struct registries, typed accessors, and
  generated grammar builders.

A repository search must be repeated after each cut. The absence gate is not a
fixed filename ledger: no production read of `ir.types`, `ir.type_map`,
`ir.struct_registry`, `ir.type_obligations`, or `ir.payload_layouts` may remain.

## Target stage carrier

```text
Program
  -> DependencyGraph
  -> TypeSolution
  -> AnalysisFacts
  -> LoweringPlan
  -> Rust | VM | TypeScript | WASM
```

Each arrow is a total function returning either the next immutable value or a
typed failure. Every value carries the exact upstream revision identities it
was derived from. Constructors and fields are private. Mismatched revisions,
missing nodes, budget exhaustion, open semantic rows at a layout boundary, and
unlowerable types are errors; no branch publishes a partial value.

Responsibilities remain narrow:

- `Program`: qualified stable identities, grammar nodes, host signatures,
  source provenance; no analysis maps.
- `DependencyGraph`: typed reference/call/containment edges, SCCs, deterministic
  schedules; no semantic type terms.
- `TypeSolution`: solved semantic terms, rule schemes, node/rule queries, and
  minimal deterministic proof provenance; no ABI sizes or emitter policy.
- `AnalysisFacts`: total grammar facts and typed exclusions, keyed to the
  program and solution revisions; no mutable authority facade.
- `LoweringPlan`: validated target layouts, materialization, call and emission
  decisions; the only input shared by code-generating backends.

An e-graph or CSP can exist only inside a bounded implementation island. Its
internal state never crosses a stage boundary, and its output must validate
against a direct semantic oracle before construction of the next carrier.

## Goldilocks module topology

The v2 apotheosis proved its 696-line solve state to be a semantic god-module and
its cross-cutting tests non-isomorphic. The smallest adjudicated v3 proof-carrier
shape is therefore:

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

`store` owns terms, opaque/store-scoped identities, and explicit `RecVar`s;
`infer` exhaustively maps the actual `IrNode` and `FnDescriptor` vocabulary;
`solve` owns a single total budget/proof context while its `unify`, `row`, and
`zonk` children own distinct algorithms; `scheme` owns levels, qualified lacks,
rigid contexts, and the named value restriction; `layout` owns only explicit
kernel representations; `project` owns target identifier allocators and artifact
provenance. Names inside a module omit their parent name. Tests stay outside
`src` and mirror this shape exactly.

This is binding for the proof carrier, not inertia for the final repository.
Graph P3 and the BB-B critics may merge or split a boundary only with measured
cohesion and dependency evidence. One-function shards with identical dependencies
merge; files spanning distinct invariants split.

## Atomic vertical transposition

The implementation order refines BB-B.W0–W7; it does not add a compatibility
era.

1. **Oracle closure (W0).** Freeze real grammar outcomes and mutations for
   unlawful join, ignored propagation failure, cycle defaulting, cross-revision
   handles, row errors, depth/step exhaustion, and arbitrary layouts. Observe
   values, spans, diagnostics, types, generated Rust/TS, and packed payloads.
2. **Program and graph (W1–W2).** Construct immutable identities and schedules
   from the frontend. Keep the incumbent compiler unchanged until the new
   carrier passes serialization, permutation, malformed-input, and corpus
   oracles.
3. **Kernel proof (W3).** Preserve v2 as falsified evidence; do not repair it.
   Build v3 against actual `bbnf-ir` nodes and host descriptors, close the
   binder/row/scheme/budget/proof/layout/projection gates, then run two fresh
   reviews and a third adjudicator. Promote only independently reproduced algebra
   and APIs, never prototype directory structure by inertia.
4. **Real constraint generation (W4).** Generate a qualified metavariable and
   provenance edge for every value-bearing production node. Solve all manifest
   grammars and compile generated Rust/TS consumers. This is the first
   load-bearing proof; a detached fixture is insufficient.
5. **Facts and lowering (W5).** Derive total immutable facts, paths, named shapes,
   materialization, and Apple/aarch64 layouts. Disconnect tests must fail through
   types or behavior when any product is absent or from another revision.
6. **One production cut (W6).** Change `finalize_compile` and
   `prepare_grammar` to consume the new stage builder, move every backend/runtime
   reader in the same routed slice, then delete `GrammarIR`, the old CSP type
   projector/domain, `FactAuthority`, mutable type/layout sidecars, and old
   registry writers. No aliases, feature flags, shadow writes, fallback solver,
   or dual target path may land.
7. **Apotheosis (W7).** Two fresh critics partition the actual program/type/
   fact/lowering/consumer graph and assume it is wrong. A third independently
   proves each finding correct, incorrect, or unproven. Repair by superseding
   evidence and repeat until semantic/state-ownership findings are closed.

## Cut gates

- all manifest grammars complete or return typed failures; there is no silent
  `Span`, `BoxedEnum`, closed-row, layout, or target default;
- constraint and input permutations produce byte-identical solutions/failures;
- one solution drives at least Rust and TypeScript generated values and the
  runtime payload path, with disconnect witnesses;
- only Apple/aarch64 layout vocabulary exists; physical claims cover explicit
  kernel-owned representations with size/alignment/offset/tag/toolchain evidence,
  while opaque `String`, `Vec`, function, and recursive carriers fail rather than
  silently becoming pointers;
- `finalize_compile` contains one stage construction path before target dispatch;
- production search finds none of the incumbent authority symbols or fallback
  paths;
- tests are external and source-isomorphic; graph P2 reports no forbidden
  source-test containment or stale production edge;
- two consecutive clean BB-B gestalt adjudications agree on the same measured
  artifact hashes.

Until these gates close, prototype test success is evidence about a candidate
kernel only; it is not evidence that bbnf-lang has a type system in production.
