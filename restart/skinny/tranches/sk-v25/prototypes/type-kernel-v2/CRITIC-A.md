# Type-kernel v2 hostile critic A

Session `019f76df-09aa-7931-89de-043f0beb612f`; recorded model
`gpt-5.6-sol`, effort `xhigh`. The review was read-only and assumed every carrier
claim was wrong.

## Verdict

**RED — 39% ±5 convergence toward the carrier contract; zero production
adoption.** Twenty-one checked-in tests and the benign Rust/TypeScript consumers
pass, but twelve load-bearing claim families are false.

## Proved failures

1. `mu outer. mu inner. outer` is accepted even though the inner binder is
   vacuous. A panic inside `mu` also leaves the binder stack live after unwind.
2. Row zonking uses a traversal-global visited set. Two sibling occurrences of a
   bound row normalize differently; the second remains unresolved.
3. Solver-created residual rows are rigid numeric tokens. A later equation
   cannot unify two fresh residuals even when the complete system has a model.
4. `Scheme::generalize` discards explicit row-lacks evidence; a forbidden field
   can be reintroduced after instantiation.
5. Environment-held variables become `PolyType::Rigid`, then instantiation
   always reports `DanglingMeta`. The scheme avoids over-generalization by
   becoming unusable.
6. Constraint sorting and rendering run outside the declared budget. A
   depth-20,000 input on a 512 KiB stack aborts before returning typed
   exhaustion; `Solution::resolve` also uses unbounded depth.
7. `AppleAarch64` is only a stored label. `String`, `Vec`, and recursive values
   lower to a pointer without total size, alignment, offsets, padding, tag, or
   niche policy. This is a semantic shape, not a physical ABI layout.
8. Names and row labels are interpolated directly. `x-y` and language keywords
   yield Rust and TypeScript that `generate()` accepts but the compilers reject.
9. The fourteen `IrNode` fixtures are variant-name aliases over hand-built
   concrete types. They construct no `IrNode`, solve an empty constraint set,
   and do not consume production inference.
10. `GrammarShape` collapses semantically different primitives and function
    signatures, so it cannot establish semantic equivalence.
11. Several errors have no origin; solver failures attach all input origins
    rather than a proved cause set. “Every error carries complete origins” is
    false.
12. The checked-in permutation and panic tests repeat a small number of distinct
    shapes. A separate 1,000-distinct-order ordinary equality gate passed, but it
    says nothing about rows, schemes, budgets, layouts, or emitters.

## Facts retained

Opaque store-scoped handles, immutable interning, cross-store rejection,
duplicate-label rejection, basic equality and occurs checks, elementary row
equations and lacks constraints, private layout fields, simple alpha-stable
fingerprints, and the checked-in benign consumer are supported. No claim of
production connectivity is supported.

## Required P2c cut

- Track exact binder occurrences and shift or level bound references correctly;
  make construction unwind-safe.
- Give source and solver-created row variables one flexible substitution
  authority; preserve lacks through representative merges and assign public
  residual identities only after normalization.
- Replace traversal-global visited sets with path-local gray sets plus completed
  memoization, and make successful solutions fully normalized.
- Carry qualified type/row schemes, lacks predicates, owned environments,
  metavariable levels, and a value restriction.
- Charge sorting, loading, pruning, occurs, unification, normalization,
  rendering, fingerprinting, and resolution; deep inputs must return typed
  exhaustion without stack overflow.
- Build deterministic proof causes rather than flat all-origin attachment.
- Either implement and native-probe a physical Apple ARM64 layout or delete the
  physical-layout claim and name the result `LayoutShape`.
- Allocate or escape identifiers per target, detect normalized collisions, and
  compile a hostile name matrix including singleton Rust tuples.
- Construct all real production `IrNode` variants, generate non-empty
  constraints, map the complete `TypeDesc` vocabulary, and run generated values
  through real Rust and TypeScript consumers.
- Split the 696-line solver state into equation/normalization, type unification,
  row unification, and proof/budget authorities with isomorphic external tests.

