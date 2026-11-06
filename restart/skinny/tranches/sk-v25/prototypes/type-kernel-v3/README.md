# BBNF type-kernel v3 kill-carrier

Status: **quarantined proof carrier; not production authority**.

This crate supersedes the falsified v2 prototype without patching it. It proves
the adjudicated kernel topology against the live `bbnf-ir` enums through a local
path dependency. It does not write `GrammarIR` sidecars, replace
`project_types`, add a feature flag, or expose a compatibility path.

## Authority boundaries

```text
src/type_kernel.rs
src/type_kernel/store.rs
src/type_kernel/infer.rs
src/type_kernel/solve.rs
src/type_kernel/solve/{unify,row,zonk}.rs
src/type_kernel/scheme.rs
src/type_kernel/layout.rs
src/type_kernel/project.rs
```

The external test tree mirrors these files under `tests/type_kernel/`; there are
no tests inside `src`.

- `store` owns immutable semantic terms, store-qualified opaque IDs, and
  explicit `RecVar` identities. Closing `mu` subtracts the exact identity from
  the body's free-recursion set. There is no mutable active-binder stack.
- `infer` exhaustively matches the real `bbnf_ir::IrNode`, `FnDescriptor`, and
  `TypeDesc` vocabularies. Every actual node receives a meta, classification,
  path origin, and non-empty constraint. An unannotated `FnDescriptor::Expr`
  is a typed `MissingHostSignature`, never `Span`.
- `solve` uses one flexible row substitution authority for source and
  solver-created tails. Representative lacks facts propagate through binding;
  row occurs walks field types; public free-row IDs are allocated only during
  path-local final reification. Internal residual numbers never escape.
- `scheme` owns qualified `forall type_vars, row_vars. lacks => body` values,
  level-based generalization, and reusable rigid contexts.
- `layout` is physical only for explicit kernel-owned Apple arm64 carriers. It
  records size, alignment, offsets, padding, tag representation, target, and
  whether a claim is modeled or measured. Rust `String`, `Vec`, and `Option`
  observations are evidence, not lowering authority; opaque Rust containers are
  rejected.
- `project` has independent Rust and TypeScript identifier allocation,
  normalization-collision tables, target keyword policies, string escaping,
  tuple-arity preservation, and reproducible artifact manifests.

## Value restriction

The named policy is `ValueRestriction::SyntacticValuesOnly`.

`BindingForm::SyntacticValue` generalizes solved type and row variables whose
levels are strictly greater than the environment level. Literals,
constructors, tuples/records of values, and functions are the intended source
forms. `BindingForm::Expansive` generalizes nothing: parser execution, host
calls, maps, and other computations remain rigid. Reusing one `RigidContext`
across instantiations preserves those environment variables, while quantified
variables are fresh on every instantiation. Qualified lacks predicates are
re-emitted as constraints.

## Total budget and diagnostics

Inference and solve accept the same `OperationContext`; fingerprint, layout,
and projection also charge that context. Ingestion, root validation, ordering,
row-prefix validation, pruning, occurs checks, binding, zonking, solution
fingerprinting, layout, and projection are charged. Exhaustion is a `TypeError`
with a non-empty named proof slice. Solver errors retain only parent-reachable
constraint causes; unrelated input origins are absent.

## Verification

Run from the repository root:

```sh
cargo fmt --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v3/Cargo.toml --check
cargo clippy --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v3/Cargo.toml --all-targets -- -D warnings
cargo test --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v3/Cargo.toml
cargo test --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v3/Cargo.toml
```

The toolchain-consumer test invokes `rustc --edition=2024 -D warnings` and
`npx --yes tsc --strict --noEmit --lib es2022,dom` over generated hostile values.
See [EVIDENCE.md](EVIDENCE.md) for exact counts, measurements, and every gate
that remains RED.

