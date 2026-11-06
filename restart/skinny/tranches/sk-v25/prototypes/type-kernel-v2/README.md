# Type-kernel prototype v2

Status: **executable replacement candidate; not production authority**.

## What

This standalone, dependency-free crate repairs the falsified v1 carrier without
editing it. It supplies five deliberately separate authorities:

1. `StoreBuilder` validates and interns immutable type terms. Its opaque handles
   carry a unique store revision, so handles cannot be forged through the safe
   API or crossed between stores. Terms only reference already-built terms, so
   graph cycles and dangling handles are unrepresentable. Duplicate row labels,
   foreign handles, escaped recursive binders, and vacuous `mu` binders are typed
   construction errors.
2. `solve(&TypeStore, ..)` never mutates the store. It copies terms into a private
   equation language, solves equality plus open record/variant rows, maintains
   explicit and implicit lacks evidence, performs type and row-tail occurs
   checks, and returns completely zonked immutable bindings. Open/open row
   equations retain canonical residual rows instead of hiding an arbitrary
   closure.
3. `Scheme::generalize` runs only over a solved type at a named `Boundary`.
   Quantified type and row variables are freshly instantiated through another
   validated builder. Environment-held variables remain rigid and cannot be
   accidentally instantiated as generalized variables.
4. `lower_layout` is the only constructor for `LayoutPlan`; its fields are
   private. It rejects unresolved semantic variables and open rows. The sole ABI
   in this prototype is `AppleAarch64`; there is no x86 vocabulary or path.
5. `GrammarShape` is the deliberately small comparison algebra and `Projection`
   emits Rust and TypeScript from solved types. No algebraic subtyping or effect
   rows were added: the executable grammar fixtures do not require either.

Flexible metavariables and recursive bound variables are different types in the
internal term language. `mu` uses de Bruijn indices, making fingerprints and
equality alpha-stable without binder-name or allocation-order dependence.
Constraint orientation, diagnostic provenance, maps, row labels, and residual
numbering are canonical. Every error carries the complete sorted origin set, so
input permutation cannot erase the fact chain. Step and depth budgets report
typed `Exhausted` errors.

## Why this is load-bearing

The external tests construct semantic results for every current `IrNode` kind:
`Literal`, `Regex`, `Epsilon`, `Seq`, `Alt`, `Repeat`, `Ref`, `Skip`, `Next`,
`Minus`, `Negate`, `Map`, `OptionalWhitespace`, and `TokenDispatch`. Additional
fixtures cover an explicit recursive grammar, generic host map, parse result plus
diagnostics, `CssValue`, and a heterogeneous alternation.

The parse-result solution is consumed by the projection boundary. The test
requires byte-exact equality with checked-in generated Rust and TypeScript:

- `generated/consumer_payload.rs`
- `generated/consumer_payload.ts`

Those artifacts are independently compiler-checked. A physical layout is also
derived from a solved closed record; callers cannot assemble a `LayoutPlan` by
assertion.

## Proofs and exact commands

Run from any directory:

```sh
cargo fmt --manifest-path /Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v25/prototypes/type-kernel-v2/Cargo.toml --check
cargo clippy --manifest-path /Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v25/prototypes/type-kernel-v2/Cargo.toml --all-targets -- -D warnings
cargo test --manifest-path /Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v25/prototypes/type-kernel-v2/Cargo.toml
cargo test --manifest-path /Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v25/prototypes/type-kernel-v2/Cargo.toml
rustc --edition=2021 --crate-type=lib /Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v25/prototypes/type-kernel-v2/generated/consumer_payload.rs -o /Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v25/prototypes/type-kernel-v2/target/libconsumer_payload.rlib
npx --yes tsc --strict --noEmit --lib es2022,dom /Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v25/prototypes/type-kernel-v2/generated/consumer_payload.ts
```

The suite includes 1,000 successful constraint permutations, 1,000 failing
permutations with byte-identical complete diagnostics, repeat-solve byte
identity, alpha equivalence, duplicate/lacks/type-occurs/row-occurs/cross-store/
binder-escape failures, typed deep-budget failure, and 256 caught public error
corpus cases. Tests are outside `src/` and mirror the source modules.

## Limits and remaining RED facts

- This is a standalone carrier. No production `GrammarIR`, parser, backend, or
  frontend currently calls it; production adoption and migration remain RED.
- It proves equality and row polymorphism on grammar-shaped fixtures, not a
  principal-type theorem, value restriction, subtyping, effect inference,
  incremental solving, or diagnostic quality across the full corpus.
- Recursive types are iso-recursive and alpha-structural, not equi-recursive.
  Layout lowers recursive values to pointers; it does not compute recursive
  aggregate layouts.
- Terms are interned within one append-only build revision, not globally or
  incrementally across revisions. Safe code cannot create a dangling handle;
  corruption through `unsafe` is outside this prototype's claim.
- Complete provenance is intentionally conservative: a failure carries every
  input constraint origin. A production solver should retain a minimal proof DAG
  while preserving completeness.
- Generated Rust supports closed top-level records/variants and non-nested field
  types. TypeScript supports nested closed rows. Recursive/nested Rust schema
  naming is not designed here.
- Performance, allocation ceilings, fuzzing, serialization stability, parallel
  solving, and production-scale adversarial depth remain unproven.
