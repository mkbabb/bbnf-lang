MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: R4 born-RED native recorder.

# BB-C.W0 ambient optimizer authority — R4 Stage-A born-RED native receipt

- Executed: `2026-07-22`
- Host route: Apple `arm64`; target `aarch64-apple-darwin`
- Build jobs: `1`
- Retry count: `0`
- x86 route: **none**
- Terminal status: **three deterministic born-RED witnesses reproduced**
- Credit: **zero BB-C.W0, wave, lock, tranche, or movement credit**
- Advancement: **BB-C.W1 may not begin**

Each command below was executed exactly once by the root native pass after the
Stage-A static freeze and before any Stage-B repair. Complete combined output
is retained at the named raw path; every digest and byte/line count recomputes
from that file.

## RED-1 — pre-cancelled factorized grammar

```text
/usr/bin/arch -arm64 env CARGO_BUILD_JOBS=1 cargo test --target aarch64-apple-darwin -p bbnf-ir --lib passes::csp_strategy::tests::pre_cancelled_factorized_grammar_rejects_without_output -- --exact --nocapture
```

- Exit: `101`
- Raw log: `restart/skinny/tranches/sk-v25/waves/BB-C/evidence/W0-AMBIENT-R4-RED-1.log`
- Raw SHA-256: `a8a0f33765ac04956759db2ab58b577c16b5bc4b8cb412c747df9b076504899e`
- Raw size: `72 lines / 2338 bytes`
- Observed inversion: the pre-cancelled zero-constraint component returned a
  complete `GrammarSolveOutput` with `FactorizedExact` report instead of typed
  `Cancelled`, and the no-output oracle panicked.

## RED-2 — cancellation on the final singleton leaf

```text
/usr/bin/arch -arm64 env CARGO_BUILD_JOBS=1 cargo test --target aarch64-apple-darwin -p bbnf-ir --lib passes::csp_strategy::tests::cancellation_on_final_singleton_leaf_discards_incumbent -- --exact --nocapture
```

- Exit: `101`
- Raw log: `restart/skinny/tranches/sk-v25/waves/BB-C/evidence/W0-AMBIENT-R4-RED-2.log`
- Raw SHA-256: `7ed3e288c7c149c76f59fa798b681c2cbb98e5b76f65c3614af02899e83cb615`
- Raw size: `18 lines / 902 bytes`
- Observed inversion: `solve_exact_constrained` returned the incumbent
  `[Alt(Checkpoint)]` after the sole complete-assignment constraint flipped
  the token, so the typed-error oracle panicked.

## RED-3 — valid parenthesized inner map

```text
/usr/bin/arch -arm64 env CARGO_BUILD_JOBS=1 cargo test --target aarch64-apple-darwin -p bbnf --lib lower::expression::wrap::tests::one_parenthesized_inner_map_lowers_only_the_inner_mapped_factor -- --exact --nocapture
```

- Exit: `101`
- Raw log: `restart/skinny/tranches/sk-v25/waves/BB-C/evidence/W0-AMBIENT-R4-RED-3.log`
- Raw SHA-256: `aec4885bd10f8ea830229ee96146638bdfaa88f1fa1ec34f833f9a244509910d`
- Raw size: `1131 lines / 42900 bytes`
- Observed inversion: the valid `("x" -> 0u8)` group reached
  `lower_mapped_factor`, whose descendant-wide arrow predicate panicked on the
  outer mapped factor with one structural child.

## Preservation

`Cargo.lock` remained byte-identical at
`480ce3dcd92ed7778e5fc922cc531039e187a96258ec78f6fa6937defb6a40e9`.
The pass performed no retry, x86 execution, benchmark, package/lock edit,
source repair, commit, or staging action. Stage B starts only after these raw
RED preimages are frozen.

**BORN-RED NATIVE RECEIPT ONLY. THREE EXITS 101. CREDIT 0. BB-C.W1 MAY NOT BEGIN.**
