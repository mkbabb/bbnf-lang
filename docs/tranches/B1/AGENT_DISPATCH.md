# B1 — Agent Dispatch

Dispatch B1 before any further AY-II runtime work.

This is not a research brief. B1's design space is already constrained
by AY-II's infra audits, `TOOLCHAIN-SOTA.md`, `TOOLCHAIN-MIGRATION.md`,
the 7 patch drafts under `patches/`, and the per-script verdicts in
`docs/tranches/meta-audit/08-abrogation-catalog.md`. The job now is
execution: pin the substrate, rewrite the alias surface, migrate the
bench harness, rewire CI, absorb the abrogation catalog, propagate
cross-repo, refresh docs, and unblock AY-II.W0' close.

## Read first

1. `docs/instructions/README.md`
2. `docs/instructions/PROFILING.md`
3. `docs/instructions/tranche/SPEC.md`
4. `docs/instructions/tranche/WAVE_SPEC.md`
5. `docs/tranches/B0/FINAL.md`
6. `docs/tranches/AY-II/audit/W0p-infra-root-cause.md`
7. `docs/tranches/AY-II/audit/W0p-infra-fix-plan.md`
8. `docs/tranches/AY-II/audit/W0-iter-surface-verification.md`
9. `docs/tranches/B1/B1.md`
10. `docs/tranches/B1/TOOLCHAIN-SOTA.md`
11. `docs/tranches/B1/TOOLCHAIN-MIGRATION.md`
12. `docs/tranches/B1/patches/*`
13. `docs/tranches/meta-audit/04-toolchain-pain.md`
14. `docs/tranches/meta-audit/07-appurtenant-assay.md`
15. `docs/tranches/meta-audit/08-abrogation-catalog.md`
16. `docs/tranches/B1/waves/W0.md`, `W1.md`, `W2.md`, or `W3.md` as
    appropriate

## Program order

1. B1.W0 — Steps 1-4 — substrate pin, `.cargo/config.toml` rewrite,
   `.config/nextest.toml` rewrite, `Makefile` rewrite + ICE-clean gate.
2. B1.W1 — Steps 5-8 — exemplar divan port, 18 remaining bench ports,
   `bencher` dep removal, iai-callgrind feature + draft workflow.
3. B1.W2 — Steps 9-11 — CI rewire, script abrogation per catalog,
   cross-repo pin propagation to parse-that + pprint.
4. B1.W3 — Step 12 + close — PROFILING.md doc pass, post-B1.json
   aggregate, FINAL.md, AY-II handoff.
5. AY-II.W0' close ceremony resumes only after B1.W3 closes.
6. AY-II.W1-W5 then execute sequentially.

## Non-negotiables

- No quick solutions, no workarounds, no fallbacks, no dual-paths.
- No runtime-architecture edits in B1.
- No stale workflow prose left behind.
- No parallel B1/AY-II execution.
- No dead command aliases, comments, or docs preserved for sentiment.
- No script deleted until its REPLACE target is live on a cargo alias.
- No divan port begins until the pinned toolchain has cleared the
  ICE cluster (W0.d hard gate).
- No dual-harness window: `bencher` removal and final bench port land
  in the same commit.

## Wave-level dispatch templates

Every dispatched sub-agent receives:

1. **Hard cap** (default by wave): W0.a/b/c = 20 min, W0.d = 40 min,
   W1.a = 90 min, W1.b agents = 90 min, W1.c = 60 min, W2 agents =
   ranges per W2.md, W3 = 60 min. At 0.9× cap, the agent commits; at
   1.0× cap, the agent halts and returns.
2. **File-bound disjointness** per the wave spec's file-bounds table.
3. **Shared artefact disjoint-row prefixes** where append-only files
   are referenced.
4. **Read-first list** from §Read first above.
5. **Return discipline** from §Return discipline below.
6. **Triumvirate escalation**: if the JSONL quiets >15 min OR the
   first pass produces no commit, the orchestrator dispatches the
   3-agent triumvirate (research / plan / redress) before redispatch.

### W0 dispatch (3 parallel + 1 serial closer)

- **W0.a — Substrate pin (Step 1)**
  Files (owner-only): `rust-toolchain.toml`.
  Shared append-only rows: `rustc-version`, `toolchain-smoke` in
  `docs/benchmarks/archive/post-B1-W0-proof.txt`.
  Job: copy `patches/rust-toolchain.toml.draft` to repo root; verify
  the pin matches the orchestrator-approved date; smoke-test
  `cargo check -p bbnf --profile ax-iter`. Hard cap 20 min.

- **W0.b — `.cargo/config.toml` rewrite (Step 2)**
  Files (owner-only): `.cargo/config.toml`.
  Shared append-only rows: `alias-resolve-*` in
  `post-B1-W0-proof.txt`.
  Job: copy `patches/config.toml.draft`; verify every alias resolves;
  verify the cost-model comment block names all four surfaces
  (routine / per-exclude fast-paths / close-gate / bench-json);
  smoke `cargo build --profile ax-iter -p bbnf`. Hard cap 25 min.

- **W0.c — `.config/nextest.toml` rewrite (Step 3)**
  Files (owner-only): `.config/nextest.toml`.
  Shared append-only rows: `nextest-*` in `post-B1-W0-proof.txt`.
  Job: copy `patches/nextest.toml.draft`; verify 4 profiles
  (default / ax-iter / ci / close); dry-run under `ax-iter` and
  `close`. Hard cap 15 min.

- **W0.d — `Makefile` rewrite + ICE-clean close gate (Step 4, closer)**
  Files (owner-only): `Makefile`, `scripts/test-tier.sh`,
  `docs/benchmarks/archive/post-B1-W0-routine.txt`.
  Shared append-only rows: `iter-check-full-cold-pinned`,
  `ay-prime-fresh` in `post-B1-W0-proof.txt`.
  Job: copy `patches/Makefile.draft`; delete GNU-timeout cascade;
  align `scripts/test-tier.sh`; measure routine alias wall-clocks;
  measure `cargo iter-check-full` cold under the pin; **verify
  `ls target/rustc-ice-*.txt | wc -l == 0`**. Hard cap 40 min.
  Opens only after W0.a/b/c sub-gates green.

### W1 dispatch (1 + 4 parallel + 1)

- **W1.a — Exemplar bench port + bencher baseline capture (Step 5)**
  Files (owner-only): `crates/core/benches/compile_pipeline.rs`,
  `benches/common/timeout.rs`, `crates/core/Cargo.toml` (divan
  dev-dep add only).
  Output artefacts: `docs/benchmarks/post-B1-W1-baseline.json`,
  `docs/benchmarks/post-B1-W1-divan-compile.json`.
  Job: capture bencher baseline FIRST; add divan dev-dep; rewrite
  harness shim; port compile_pipeline bench; verify parity ±5%.
  Hard cap 90 min. Opens only after W0 close + ICE-clean gate.

- **W1.b — 18 remaining ports (Step 6, 4 parallel agents)**
  Each agent owns one grammar family's bench files:
  - W1.b.1 JSON (5 files), output `post-B1-W1-divan-json.json`
  - W1.b.2 CSS (4 files), output `post-B1-W1-divan-css.json`
  - W1.b.3 Sheets (2 files), output `post-B1-W1-divan-sheets.json`
  - W1.b.4 BBNF + misc (7 files), output
    `post-B1-W1-divan-bbnf.json`
  Hard cap 90 min per agent. Opens after W1.a sub-gate.

- **W1.c — `bencher` removal + iai-callgrind feature (Steps 7-8)**
  Files (owner-only): `crates/core/Cargo.toml` (bencher removal + iai
  feature + `[[bench]]` entry for json_callgrind),
  `crates/core/benches/json_callgrind.rs`,
  `docs/tranches/B1/patches/bench-iai.yml.draft`,
  `docs/benchmarks/archive/post-B1-W1-parity.txt`.
  Job: strip the bencher dep in the same commit that lands the final
  port; add iai-callgrind feature-gated bench; draft workflow yml
  under `patches/`; author parity table with `pass` per bench.
  Hard cap 60 min. Opens after all W1.b agents sub-gate green.

### W2 dispatch (3 parallel)

- **W2.a — CI workflow rewire (Step 9)**
  Files (owner-only): `.github/workflows/ci.yml`,
  `.github/workflows/bench-iai.yml`,
  `docs/benchmarks/iai-baselines/*.json`,
  `docs/benchmarks/archive/post-B1-W2-ci.txt`.
  Job: rewrite ci.yml to install nextest + run under `ci` profile;
  promote bench-iai.yml from `patches/`; seed iai baseline.
  Hard cap 40 min.

- **W2.b — Script abrogation (Step 10)**
  Files (owner-only): every script in
  `docs/tranches/meta-audit/08-abrogation-catalog.md §Part 1`;
  `docs/benchmarks/archive/post-B1-W2-abrogation-ledger.txt`.
  Job: single owner for the scripts/ directory to avoid racing;
  execute each catalog action verbatim (DELETE / REWRITE / KEEP);
  high-impact targets — `bootstrap-bbnf.sh` content-hash guard,
  `bench_regression.sh` divan-JSON rewrite, `profile.sh` deletion,
  `check-cst-invariants.sh` deletion with CI step removed in same
  commit. Hard cap 3 hours.

- **W2.c — Cross-repo pin propagation (Step 11)**
  Files (owner-only): `../parse-that/rust-toolchain.toml`,
  `../parse-that/.cargo/config.toml` (if absent),
  `../pprint/rust-toolchain.toml`,
  `../pprint/.cargo/config.toml` (if absent),
  `docs/benchmarks/archive/post-B1-W2-cross-repo.txt`.
  Job: mirror bbnf-lang's pin to parse-that + pprint ONLY; verify
  each sibling `cargo check` exits 0 under the pin; verify
  bbnf-lang's `cargo iter-check` exits 0 with the triad synced.
  Wider fleet (gorgeous / csp-solver / csc411 / crates/ai) is named
  in the cross-repo ledger as DEFERRED.
  Hard cap 90 min.

### W3 dispatch (1 serial)

- **W3.a — Documentation pass + FINAL + AY-II handoff (Step 12 + close)**
  Files: `docs/instructions/PROFILING.md`,
  `docs/benchmarks/post-B1.json`,
  `docs/tranches/B1/FINAL.md`,
  `docs/tranches/B1/PROGRESS.md`,
  `docs/tranches/AY-II/PATH-FORWARD.md`,
  `docs/tranches/AY-II/PROGRESS.md`,
  `docs/tranches/AY-II/AY-II.md`,
  `docs/tranches/AY-II/waves/W0p.md`-`W5.md`.
  Job: refresh PROFILING.md (ICE recovery, dev-host setup, bench
  alias surface); aggregate every W0/W1/W2 artefact into
  `post-B1.json`; author FINAL.md invariant + hard-gate + ledger
  tables; unblock AY-II.W0' in planning docs.
  Hard cap 60 min.

## Return discipline

Every sub-agent returns:

1. Worktree path + branch name.
2. Commit SHAs in order.
3. Exact artefact paths (benchmark rows, proof files).
4. Hard-gate status per item (per the relevant wave's hard-gate list).
5. Any stale surface deleted (the file path and the line count).
6. Empty `git status --short` except expected generated artefacts or
   target symlinks.
7. For W0.d: explicit ICE-clean verification output
   (`ls target/rustc-ice-*.txt | wc -l`).
8. For W1.c: `rg -w bencher` output (must be empty).
9. For W2.c: `cd bbnf-lang && cargo iter-check` exit code under the
   triad pin.

## Empty-return redispatch

Per `redispatch-empty-return` feedback: if a sub-agent returns empty,
the orchestrator redispatches the original brief verbatim with a
prior-worktree pointer; the empty return is not a scope-reveal.
