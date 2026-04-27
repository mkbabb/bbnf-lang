# B7 — Progress Log

Dated execution log for tranche B7.

- `Status`: planned + dispatched (W0 + W1 + W2 in flight)
- `Current wave`: W0 / W1 / W2 parallel
- `Next wave`: tranche close after all three pools land

---

## 2026-04-27 — B7 planned + dispatched

B7 opens against bbnf-lang master HEAD `6d11f3b5` (post-B6.W2
status-stamp close; 1477/1477 nextest green; 45.27 s wall on
`--profile ax-iter`). The tranche is the cross-repo modernization
annex AY-III's deferral surfaces: AZ-I.W0 needs a coherent
build/test/bench discipline across all three repos in the
ecosystem before the architectural lever dispatches.

### Pre-tranche audit findings

Three Explore agents in parallel returned concordant findings on
the cross-repo modernization gap:

1. **bbnf-lang internal — PARTIAL but mostly clean.** Most benches
   already on divan; nextest in CI; toolchain pinned; tests under
   `tests/`. Two residual items: `crates/simd-scan/benches/
   stage1_throughput.rs` uses manual `std::time::Instant` (not
   divan); B6's warm-wall observation pointed at
   `bench_lsp::bench_lsp_actions` (16.5 s) + simd-scan proptest
   fuzzers (saturating to 17–20 s) as the slow-test partition
   debt.
2. **parse-that — PARTIAL / NEEDS-CATCHUP.** Toolchain pin
   matches; edition 2024; cross-repo patch contract valid. But:
   16 bench targets use `bencher = "0.1.5"`; CI uses bare `cargo
   test --workspace` (not nextest); 69 inline `#[test]` in `src/`;
   `.cargo/config.toml` declares only `ax-iter` (missing
   `ay-final`, `bench-ci`); CI workflow uses floating
   `dtolnay/rust-toolchain@nightly`.
3. **pprint — PARTIAL.** Toolchain pin matches; edition 2024;
   tests/ clean; no inline `#[test]`; no `#[allow(...)]`. But: 38
   bench fns use legacy `#![feature(test)]` + `extern crate test`;
   CI uses bare `cargo test --workspace`; local
   `rust/.cargo/config.toml` patches `bbnf-ser` redundantly,
   shadowing bbnf-lang's canonical patch.

### Annex contract verification

Per SPEC §Prelude annexes:

1. B7 owns no parity-critical runtime architecture. All work
   resides in `benches/`, `.github/workflows/`, `.cargo/config.toml`,
   `tests/` migration, and doc scrubs. No tape, columns, lower,
   or emitter source lines change in any repo.
2. B7 exists only to remove cross-repo build/test/bench drag the
   AZ-I.W2 twitter ≥ 1967 MB/s gate would otherwise carry.
3. B7 is bounded: 3 repo-isolated waves, no successor debt tree.
   The single forwarded item (parse-that's deprecated `bbnf_derive
   0.2` dep) routes to AZ-I as cross-repo cleanup, not a B8.
4. AZ-I.W0 names B7 explicitly in its open precondition.
5. B7 is not a refuge for hard work. Every parity-critical AY-III
   gate stays in AY-III's deferred-ledger destination (AZ-I.W4 /
   AZ-II.W2).
6. B7's scope cannot grow to compete with AZ-I; the floor check
   at plan time confirms each wave's structural lower bound is
   well below its declared gate.

### 10-agent dispatch shape

Three parallel waves, ten agents total. Per-wave parallel-agent
counts (3, 4, 3) all sit within SPEC §Parallelism max-6.

| Wave | Agents | Repo | Wall cap (longest agent) |
|------|-------:|------|--------------------------|
| W0 | 3 (A1, A9, A10) | bbnf-lang | 3 h (A9) |
| W1 | 4 (A2, A3, A4, A5) | parse-that | 4 h (A2 / A3) |
| W2 | 3 (A6, A7, A8) | pprint | 4 h (A6) |

Real wall ~4–5 h to tranche close (the longest single agent
caps at 4 h; all three waves run in parallel with repo-isolated
worktree pools).

### Worktree pool layout

- `/tmp/b7-bbnf-lang-{a1,a9,a10}` — bbnf-lang worktree pool;
  3 worktrees.
- `/tmp/b7-parse-that-{a2,a3,a4,a5}` — parse-that worktree pool;
  4 worktrees.
- `/tmp/b7-pprint-{a6,a7,a8}` — pprint worktree pool; 3 worktrees.

Each worktree carries its own `target/` directory; no cargo lock
contention. The orchestrator cherry-picks each repo's commits onto
that repo's master HEAD as they land.

### Planned wave-status table

| Wave | Status | Hard gate (one-line) |
|------|--------|----------------------|
| W0 | dispatched | warm nextest wall ≤ 14 s; simd-scan divan; A9 partition + T1.a doc scrub; A10 plan docs + T1.b/T1.c. |
| W1 | dispatched | parse-that all-divan; CI nextest + pinned toolchain; no inline `#[test]` in `src/`; bench-median delta ±5 %. |
| W2 | dispatched | pprint all-divan; CI nextest; `bbnf-ser` patch single-source-of-truth in bbnf-lang; bench-median delta ±5 %. |

### Risk register

1. **A2 / A3 parse-that bench-median deltas exceed ±5 %.** divan
   vs bencher use different sample-and-warmup defaults.
   Mitigation: capture pre-migration median artefact before
   migration; declare ±5 % gate; if missed, the fix is divan
   tuning (sample count, warmup), not architectural pivot.
2. **A6 pprint bench-fn shape incompatible with divan macro.**
   `test::Bencher::iter` accepts `FnMut`; divan's
   `#[divan::bench]` accepts `Fn` over `&mut Bencher`. Mitigation:
   divan's `Bencher::bench` covers the full surface; mechanical
   port.
3. **A9 test-surface partition introduces flake.** Re-routing
   `bench_lsp_actions` to `[[bench]]` may surface a state-
   leakage bug masked by the test surface's process-isolation.
   Mitigation: A9 commits the re-categorization separate from
   the sample-cap; orchestrator can revert one independently.
4. **Cross-repo CI floating-pin mismatch.** parse-that's
   `dtolnay/rust-toolchain@nightly` (floating) may differ from
   bbnf-lang's pin at any moment. A4's lever fixes this by
   reading `rust-toolchain.toml`.

### Operational discipline

1. **Worktree isolation.** Every sub-agent operates inside its
   worktree pool entry; the orchestrator owns each repo's master
   HEAD; cherry-pick discipline applies per repo.
2. **Hard cap per dispatch.** Every dispatch carries an explicit
   time cap. At 0.9× cap, commit the deliverable; at cap, halt
   and return per SPEC §Diagnostic-loop relinquish.
3. **No polling.** Per `feedback_no_polling_loops`, dispatches
   ride `run_in_background=true` plus a Monitor call.
4. **Single cargo per `CARGO_TARGET_DIR`.** Repo-isolated worktree
   pools mean each pool has its own `target/`; no cross-pool
   serialization on the build lock.
5. **Status tick cadence.** The orchestrator emits a one-line
   status tick every ~5 min of orchestrator-silent wait.
6. **`#[allow(dead_code)]` prohibition.** B5 invariant 7 carries
   into B7; no modernization edit has warrant to mask a lint
   surface.
