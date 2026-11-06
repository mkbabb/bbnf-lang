# Type-kernel V4 deterministic evidence receipt

Disposition: **INCOMPLETE_ASSAY**. Date: **2026-07-18**. Scope: this directory
only. These are author execution receipts, not an independent review and not a
production-selection claim.

## Implemented evidence slices

| Slice | Direct evidence |
| --- | --- |
| Closure carrier | Runtime escaping-recursion rejection plus an external Cargo compile-fail crater proving `solve` cannot accept `DraftTypeId`. |
| Flat semantics | Separate draft type/row arenas, solved type/row arenas, and scheme poly type/row arenas; 20,000-deep success and typed exhaustion on an explicit 512-KiB thread. |
| Multi-root recursion/schemes | Two independently allocated alpha-equivalent recursive roots have equal canonical bytes, batch-generalize, instantiate, close, and solve again. |
| Record and variant rows | The same matrix runs for both kinds: closed permutation/difference, open/closed symmetry, open/open disjoint residuals, lacks conflict, direct/field occurs, type occurs through rows, level capture, disjoint extension order, and duplicate extension rejection. |
| Permutations/metamorphism | 1,024 genuinely distinct seven-operation mixed equality/lacks permutations per row kind; independent stores, unrelated allocations, root alpha allocation, input rule/node order, and repeat generation preserve content identity. |
| Transactionality/proof | Contradictions and zero budgets return typed errors without a partial `TypeSolution`; parent-reachable proofs exclude an unrelated tautology in the checked witness. |
| Generic inference | Immutable canonical snapshot; whole local grammar vocabulary; recursive JSON; missing host signature; a non-JSON CSS-shape representability witness. |
| Projection | Independent flat Rust and TypeScript emitters; repeated semantic names reuse one spelling, distinct normalized collisions suffix, option-of-function is grouped, nested rows/variants/empty variants emit, and real rustc/tsc consumers compile. |
| Layout boundary | Explicit semantic profile versus Apple-arm64 physical profile; modeled carrier matrix; exact toolchain identity changes measurement identity without changing semantic digest. |
| Hostile work accounting | Descriptor depth and million-byte export names consume proportional checked work and return typed exhaustion at small limits. |

## Exact commands

Run from `/Users/mkbabb/Programming/bbnf-lang`:

```sh
cargo fmt --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/Cargo.toml --check
cargo clippy --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/Cargo.toml --all-targets -- -D warnings
cargo test --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/Cargo.toml -- --test-threads=1
cargo test --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/Cargo.toml -- --test-threads=1
```

Expected author replay: format exit 0; clippy exit 0 with warnings denied; each
test replay reports 22 external integration tests passed, 0 failed, 0 ignored,
0 source-unit tests, and 0 doc tests. Cargo may print the root repository's
pre-existing unused-patch notices; the standalone V4 graph contains no
dependency.

The projection test independently invokes:

```sh
rustc --edition=2024 -D warnings --crate-type=lib artifact.rs
npx --yes tsc --strict --noEmit --lib es2022,dom artifact.ts
```

The draft-capability test also creates a disposable external Cargo crater and
requires compilation to fail with `expected ClosedProblem, found DraftTypeId`.

## Deterministic receipt checks

```sh
cargo metadata --manifest-path restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/Cargo.toml --no-deps --format-version 1
rg -n '#\[(test|cfg\(test\)|ignore)' restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/src
find restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/src/type_kernel -type f | sort
find restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/tests/type_kernel -type f | sort
shasum -a 256 restart/skinny/tranches/sk-v25/prototypes/type-kernel-v4/Cargo.lock
shasum -a 256 Cargo.lock
```

The exact hashes captured after the final two replays live in
`SOURCE-RECEIPT-LEDGER.md`.

## RED and routed

- No live `GrammarIR`/BBNF-module oracle or exact module-derived host signature.
- No complete semantic agreement with the production projector.
- No complete full Rust/TypeScript matrix claim beyond the checked external
  consumers; recursive target carriers remain intentionally rejected.
- No production layout authority, tape layout/execution, runtime values, CSS L4
  completeness, backend, corpus, cost, performance, adoption, or atomic cut.
- No claim that the author suite closes V4. Fresh hostile review and adjudication
  remain mandatory.
