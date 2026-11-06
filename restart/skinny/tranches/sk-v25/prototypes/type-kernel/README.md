# Type-kernel prototype v1

Status: **FALSIFIED; retained as evidence, not an implementation base**.

## What and why

This standalone crate is the smallest executable carrier for the BB-1 question:
can bbnf-lang represent semantic types and solve equality constraints without
collapsing them into storage layout or finite shape propagation? It implements
stable arena and variable IDs, substitutions with occurs-check, tuples, lists,
options, functions, open/closed record and variant rows, explicit `mu` types,
named-boundary generalization and fresh instantiation, and provenance-bearing
typed errors. Constraint sorting plus minimum-ID variable orientation makes a
permuted constraint set produce the same solution state.

Run it from any directory:

```sh
cargo test --manifest-path /Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v25/prototypes/type-kernel/Cargo.toml
```

The crate has no dependencies and places all tests outside `src/`.

## Hostile findings

The first hostile audit disproved the claimed carrier. In particular:

- public numeric IDs can be forged or crossed between stores and revisions;
- public insertion accepts dangling, capture-prone, and cyclic terms, including a
  cycle that can overflow recursive traversals;
- solving mutates the term store, does not return a completely zonked immutable
  solution, and converges on only part of the constraint-permutation corpus;
- row construction silently overwrites duplicate labels, has no `lacks` evidence,
  and admits an implicit-infinite row-tail hole;
- recursive equality depends on a non-alpha-stable fingerprint and rejects
  otherwise solvable free variables;
- generalization is performed against pre-solution terms rather than the solved
  environment;
- diagnostics are orientation-sensitive and do not retain complete constraint
  provenance;
- layout plans accept arbitrary physical layouts instead of checked lowering from
  a semantic solution;
- no grammar node or generated Rust/TypeScript consumer proves the type result is
  load-bearing.

V2 must replace this directory's public model with store-qualified opaque IDs,
validated immutable construction, separate metavariables and recursive binders,
row equations plus lacks constraints, fully zonked deterministic solutions,
post-solve generalization, checked layout lowering, and real grammar consumers.

## Limits

This prototype proves only that the kernel concepts compose on focused examples.
It does **not** prove the BB-1 production claims: grammar-derived constraint
generation, principal types for a language, let/value restriction, subtyping,
effect rows, diagnostic quality on a corpus, incremental solving, e-graph or CSP
integration, IR authority, runtime lowering, serialization stability, throughput,
or adoption by generated Rust/TypeScript consumers. Recursive `mu` equality is
alpha-structural rather than equi-recursive, and row polymorphism has equality
constraints only. Those are deliberate walls, not silent fallbacks.
