# AZ-II O3a Six-Agent Audit Synthesis - 2026-04-29

Status: orchestration ledger for the halted implementation boundary.
No source redress is authorized by this document; it specifies the next
deployable wave series before AZ-II implementation resumes.

Audited worktrees:

- `/Users/mkbabb/Programming/bbnf-wt-azii-audit-plan-waves`
- `/Users/mkbabb/Programming/bbnf-wt-azii-audit-instructions-process`
- `/Users/mkbabb/Programming/bbnf-wt-azii-audit-substrate-deadcode`
- `/Users/mkbabb/Programming/bbnf-wt-azii-audit-failure-baseline`
- `/Users/mkbabb/Programming/bbnf-wt-azii-audit-testing-throughput`
- `/Users/mkbabb/Programming/bbnf-wt-azii-audit-future-sota`

## Verdict

AZ-II remains open. The next implementation step is not a benchmark
optimization pass; it is semantic substrate closure. O3a must close
before O3 implementation resumes, because the post-O2 baseline exposed
84 failed tests and one failed JSON bench lane across five owned
cohorts.

The six audits agree on these blockers:

1. O3 cannot be active while O3a is still assigning and splitting the
   failure surface. O3 is now blocked/planned until O3a closes.
2. J1, C1, and S1 were exhaustive but too broad. They now have
   deployable child wave specs with research, plan, wave-creation, and
   redress lanes.
3. P1 belongs at the O3 boundary unless research proves it needs a
   separate O3b. It blocks generated-view purge close.
4. A1 must split live analysis/LSP repair from historical
   `json-prototype` archive/delete. It blocks tape deletion if left
   tape-shaped.
5. O6 must own actual post-O5 truth: nextest parity commands, IR audit,
   StructRegistry re-verification, bootstrap reproducibility, zero
   placeholders in `post-AZ-II.json`, and the `data_xl` timeout delta.
6. O7 may carry only non-gate risks. Tape deletion, `Parsed<R>`,
   `TapeDirect`, stale benches, and parity failures remain AZ-II close
   blockers, not BA/BB debt.

## Failure Cohorts

| Cohort | Failures owned | Bench owned | Child spec | Terminal owner |
|---|---:|---:|---|---|
| J1 | 24 | `json_monolithic::data_xl` timeout | `../waves/cutover/O3a-J1.md` | O3/O4/O6, depending root cause |
| C1 | 17 | none | `../waves/cutover/O3a-C1.md` | O6, with source redress before parity claims |
| S1 | 33 | none | `../waves/cutover/O3a-S1.md` | O4/O6, depending return/payload split |
| P1 | 1 | none | `../waves/cutover/O3a-P1.md` | O3 or O3b |
| A1 | 9 | none | `../waves/cutover/O3a-A1.md` | O5 and O7 |

The failure artifact is
`docs/benchmarks/AZ-II/cutover/O3a-test-failures.txt`. The bench
artifact is `docs/benchmarks/post-AY-az-ii-doc-baseline-json.txt`.

## Substrate Findings

| Surface | Finding | Owner |
|---|---|---|
| Generated node views | StructDirect generated files still emit node-view, `TapeCursor`, and `ValueRoot` surfaces even though parse returns concrete documents | O3/P1 |
| `Parsed<R>` | Production-dead after O2 but still exported, emitted by `TapeDirect`, and tested | O4 |
| `TapeDirect` | `EmitStrategy` still has a fallback; unknown production grammars must fail loudly | O4 |
| `crates/tape` | Still a workspace member and consumed by simd-scan/json-prototype/runtime symbols | O5 |
| `bootstrap_parser.rs` | Live BBNF bridge, not dead code; requires a retirement/proof gate | O3a-A1 and O6 |
| `crates/gorgeous/src/jit.rs` | Legacy derive-shaped JIT temp project surface; not wired to xtask regen | O3a-A1 |
| CSP/egraph/type inference | shape-dict is no-op, egraph facts are not durable consumers, and type fallback remains `BoxedEnum` | O7 names BB-only residual unless an O3a cohort proves it blocks AZ-II gates |

## Process Corrections

- Replace active "partial close" phrasing with "interim manifest" or
  "routed to cutover.O" until O7 converts `FINAL.md`.
- All sub-agents, including profiling agents, use sibling
  `/Users/mkbabb/Programming/bbnf-wt-*` worktrees. Historical
  `/private/tmp` worktrees are nonconforming provenance only.
- O6 test gates use nextest, not bare `cargo test`.
- O6 bench artifact names and command surfaces must agree with the
  actual Makefile/cargo alias surface before measurement begins.
- BA and BB remain blocked on AZ-II terminal close. BB scaffold may
  continue only if it cannot affect production codegen or close claims.

## Next Wave Series

1. **O3a child specs and triads.** Dispatch J1/C1/S1/P1/A1 research,
   plan, wave-creation, and redress lanes under their child specs.
   Redress may edit source only after the plan lane has created or
   amended the owning wave spec.
2. **O3 generated-view purge.** Close P1; delete StructDirect generated
   tape-view residue; prove document-owned APIs replace any consumed
   view surface.
3. **O4 return-model deletion.** Delete production `Parsed<R>` and
   `TapeDirect`, including the fallback strategy.
4. **O5 tape deletion.** Delete `crates/tape` after relocating only
   genuine non-tape scan/index primitives.
5. **O6 semantic/performance close.** Re-run parity and the close
   matrix on the post-O5 tape-free path; publish `post-AZ-II.json`
   with no placeholders and cite the O3a `data_xl` delta.
6. **O7 terminal conversion.** Convert `FINAL.md` from interim
   manifest to terminal close only after O0-O6 hard gates are green.

## Close Rule

AZ-II must not close with active gate misses. Any residual in O7 must
be non-blocking for direct-to-struct, tape deletion, semantic parity, or
post-O6 performance truth and must cite its exact BA/BB owner.
