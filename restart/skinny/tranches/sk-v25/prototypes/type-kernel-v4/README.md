# BBNF type-kernel V4 isolated assay

Status: **INCOMPLETE_ASSAY — prototype evidence only; never production authority or
portfolio selection.**

This dependency-free Rust crate is the clean-break successor assay prescribed by
the V3 adjudication. It tests whether a small semantic equality kernel can carry
consumed draft closure, flat type/row/poly arenas, batch schemes, complete
record/variant row mechanics, bounded traversal, content identity, explicit
semantic/layout profiles, and independent Rust/TypeScript projections without
touching production.

The crate has its own empty `[workspace]`, lockfile, and target graph. It does not
depend on `bbnf-ir` or any production package. No production file, root lockfile,
playground, xtask, parse-that tree, or sibling repository is an output of this
assay.

## Authority topology

The ten source authorities are mirrored by ten external test authorities:

```text
src/type_kernel.rs                       tests/type_kernel.rs
src/type_kernel/store.rs                 tests/type_kernel/store.rs
src/type_kernel/infer.rs                 tests/type_kernel/infer.rs
src/type_kernel/solve.rs                 tests/type_kernel/solve.rs
src/type_kernel/solve/unify.rs           tests/type_kernel/solve/unify.rs
src/type_kernel/solve/row.rs             tests/type_kernel/solve/row.rs
src/type_kernel/solve/zonk.rs            tests/type_kernel/solve/zonk.rs
src/type_kernel/scheme.rs                tests/type_kernel/scheme.rs
src/type_kernel/layout.rs                tests/type_kernel/layout.rs
src/type_kernel/project.rs               tests/type_kernel/project.rs
```

`src/lib.rs` only exports the bounded facade. There are zero source-colocated,
ignored, or unsafe tests.

## Direct ownership flow

```text
GrammarSpec -> KernelInput -> infer -> ClosedProblem -> solve -> TypeSolution
                                                        |          |
                                                        v          v
                                                      Scheme   LoweringPlan
                                                                   |
                                                           Rust / TypeScript
```

- `ProblemBuilder` owns draft handles. `close` consumes them, validates every
  export and constraint endpoint, and publishes only closed roots.
- `TypeSolution`, `Scheme`, and `LoweringPlan` are flat arena values. Recursive
  numbering is root-local; successful 20,000-deep traversal and destruction do
  not depend on the call stack.
- One checked `OperationContext` passes through snapshot, closure, inference,
  solve, row work, zonk, schemes, identity, layout, and projection.
- Semantic digests contain canonical content only. Private validation revision,
  process order, pointer identity, and alpha allocation do not enter them.
- `TargetProfile` names semantic intent separately from physical layout. The
  Apple-arm64 carrier evidence is narrow and toolchain-qualified.

## Explicit incompleteness

This assay does **not** infer an exact product Rust API solely from BBNF module
files. `HostTypeNode` is an explicit host-signature boundary used to test flat,
charged bridging; until those signatures are derived from validated BBNF module
syntax, that boundary remains RED. The local grammar vocabulary is an isolated
model, not the live production enum oracle.

The CSS fixture proves only that units, dimensions, functions, token/value
variants, and effect-bearing host signatures fit the generic type constructors.
It does not implement CSS L4, tape execution, a value fabric, any of the ten-plus
complete profiled portfolio candidates, or product/backend adoption.

See `EVIDENCE.md`, `SOURCE-RECEIPT-LEDGER.md`, and `GESTALT-HANDOFF.md`. Author
receipts cannot close this assay; two fresh hostile gestalt passes and a fresh
adjudication own any stronger ruling.

