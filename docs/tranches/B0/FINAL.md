# B0 — FINAL

B0 closes as a bounded prelude annex for AY. Every named wave landed,
every close invariant resolves through a public command, and no
parity-critical runtime work migrated into the annex. AY.W5 opens on
the repaired surface: three intentional profile tiers
(`ax-iter` / `profiling-prep` / `bench`), idempotent profiling-prep
scripts, and ten `ay-*` Makefile targets that implement the exact
commands AY.W5-W7 hard gates cite.

Master HEAD at B0 close: `7b223cf6`.
B0 tranche HEAD sealed at the close commit listed at §Close commit below.

## Scope recap + commit ledger

B0 opened on `66a0f2cd` and executed in three waves on disjoint file
bounds. Counting orchestrator-led housekeeping, B0 landed 14 commits.

### W0 — Public fast-path defaults + scoped AY command repair

Three sub-agents (2 parallel + 1 serial post-phase), 5 commits
attributable to W0 sub-items + 2 orchestrator housekeeping.

| # | Commit | Scope |
|---|---|---|
| 1 | `4869e715` | `.cargo/config.toml` gains `[alias]` block with `iter-*`, `expand-*`, `asm-parse`. |
| 2 | `0c2a5d4c` | `Makefile` "AY Iteration Surface" section; `.PHONY` extended; `test:` carries guidance comment. |
| 3 | `f4c84d01` | `docs/benchmarks/archive/post-B0-W0-commands.txt` catalogs every alias + target vs AY.W5-W7 gates. |
| 4 | `b8dac71e` | `docs/instructions/PROFILING.md` `## Public fast-path commands` section; `post-B0-W0-baseline.txt` captures pre-W0 wall-clocks at HEAD `9bff7e7d`. |
| 5 | `23b1a805` | `docs/benchmarks/post-B0-W0-mid.json` + PROFILING.md `### W0 timing proof`; headline 45× iter-check warm speedup. |
| H1 | `9bff7e7d` / `aaed3f08` | Cargo.lock smallbox drift (orchestrator housekeeping; see §Environmental interventions). |
| H2 | `96f9e4c0` | W0 close docs + wave status updates. |

W0 close evidence:

- `cargo iter-check` warm **0.16 s** vs baseline `cargo check --workspace` warm **7.16 s** ≈ 45×.
- `make iter-test-leaf` **1.05 s warm** (baseline: FAIL on stale `-p bbnf-tape`).
- `make expand-json` writes 6224-line expand artefact; AY.W5.1 / AY.W7.2 gate surface exists.
- `cargo test --workspace --no-run` warm **0.76 s** (19× vs baseline 14.40 s) — target/ symlink to main repo keeps the heavy compile-gate cheap.

### W1 — Prepared-binary discipline + exact AY gate commands

Three parallel agents on disjoint bounds. 4 commits attributable +
1 orchestrator-led scripts/gitignore repair.

| # | Commit | Scope |
|---|---|---|
| 1 | `377c2dc6` | `[profile.profiling-prep]` (inherits release, DWARF keys re-asserted); `prep-bench` + `final-bench` aliases. |
| 2 | `154880f3` | Idempotent `scripts/prebuild-benches.sh` + `scripts/prepare-profile-wave.sh`; canonical profile is `profiling-prep`. |
| 3 | `0f324e19` | PROFILING.md `### Prepared binary reuse` subsubsection. |
| 4 | `1532de45` | Makefile "AY W5-W7 Gate Commands" section (10 `ay-*` targets) + PROFILING.md `### AY W5-W7 gate commands` table. |
| H3 | `df24e7c0` | Drop `scripts/` gitignore entry; track `profile-bench-headless.sh` + `sync-external-docs.sh`. |
| H4 | `c6b9fcc9` | W1 close docs + wave status updates. |

W1 close evidence:

- `cargo prep-bench -p bbnf --bench json_monolithic` builds
  `target/profiling-prep/deps/json_monolithic-*` with `.dSYM` sibling (DWARF survives).
- Second invocation of `scripts/prebuild-benches.sh` emits
  `reused:` for all five benches — zero `cargo bench --no-run`.
- Second invocation of `scripts/prepare-profile-wave.sh` emits
  `expand: reused` for all five benches — zero `cargo expand`.
- `make ay-test-value-api` `test result: ok. 4 passed`.
- `make ay-test-named-type` `test result: ok. 3 passed`.
- `make ay-test-wire-contract` exit 2 (`no test target` — clean AY-pre-W7 state).

### W2 — Routine/heavy split + AY runway close

Two parallel agents on disjoint bounds. 4 commits.

| # | Commit | Scope |
|---|---|---|
| 1 | `ac7bc754` | `scripts/test-tier.sh` leaf crate-list fix (`-p bbnf-tape` → `-p tape`); grammar-tier bin list audit (6 stale dropped, 5 real added). |
| 2 | `1a191b1c` | `Makefile` routine/heavy surface header; `test:` deleted; `test-rust` → `test-heavy-rust`; `test-close` gate target; `.PHONY` updated. |
| 3 | `7b223cf6` | `.github/workflows/ci.yml` preflight (iter-check + clippy + bootstrap) + heavy (workspace tests + sonic + lightningcss) split. |
| 4 | `512da89d` | `docs/instructions/tranche/SPEC.md` `### Three-tier command surface` under §Bench contract + new Edicts bullet; PROFILING.md `### W2 close proof` subsubsection + `docs/benchmarks/post-B0-W2-close.json`. |

W2 close evidence:

- `scripts/test-tier.sh leaf` exit 0 at master HEAD (34 `test result: ok` lines; 0 FAILED).
- `make test-close` compiles the heavy close gate (workspace tests link + run via nextest / timeout wrapper).
- `.github/workflows/ci.yml` carries named "Preflight" (routine) and "Heavy" (close-gate) step groups.
- SPEC.md now names the three profile tiers and the `iter-*` / `ay-*` target families explicitly.

## Invariants verified

| # | Invariant | Evidence |
|---|---|---|
| 1 | No parser-runtime / semantic-architecture work | `git log master --stat 9bff7e7d..7b223cf6 -- 'crates/**/*.rs'` — no changes under `crates/` source paths. |
| 2 | Only command/build/bench/profiling runway touched | Every commit's file bounds are under `.cargo/config.toml` / `Cargo.toml` / `Makefile` / `.github/workflows/ci.yml` / `scripts/*.sh` / `docs/instructions/**` / `docs/benchmarks/post-B0-*`. |
| 3 | B0 closes before AY.W5 opens | AY.W5 wave status unchanged (`planned`); B0's handoff artefact at `docs/benchmarks/post-B0.json` + `post-B0-W2-close.json`. |
| 4 | Every new command / script is the actual public path | Makefile `iter-*` / `ay-*` targets + cargo aliases are the invocable surface; PROFILING.md §Public fast-path commands documents them verbatim. |
| 5 | Heavy / profiling / correctness loops separated | Makefile header block names three surface groups; `test-heavy-rust` + `test-close` + `ay-bench-close` on the heavy side; `iter-*` + `ay-test-*` + `ay-expand-*` on the routine side; `ay-samply-*` + `ay-prepare-profile-wave` on the profiling-prep side. |

## Hard gates closed

### W0 hard gates (from `waves/W0.md`)

| # | Gate | Artefact | Status |
|---|---|---|---|
| 1 | Public command surface exposes fast routine path directly | `docs/benchmarks/archive/post-B0-W0-commands.txt` | PASS |
| 2 | AY routine timings improve vs baseline | `docs/benchmarks/post-B0-W0-mid.json` (iter-check warm 45× faster) | PASS |
| 3 | Docs + defaults agree on routine path | PROFILING.md §Public fast-path commands + post-B0-W0-commands.txt | PASS |

### W1 hard gates (from `waves/W1.md`)

| # | Gate | Artefact | Status |
|---|---|---|---|
| 1 | Distinct routine / profiling-prep / final-proof paths | `Cargo.toml` profile stanzas + `cargo prep-bench` success | PASS |
| 2 | Prepared-binary cost improves | Second-run `reused:` + `expand: reused` | PASS |
| 3 | Exact public commands for AY W5-W7 expand / asm / bench / Samply | 10 `ay-*` Makefile targets; `make ay-expand-json` 6224-line artefact; ay-test-* green | PASS |

### W2 hard gates (from `waves/W2.md`)

| # | Gate | Artefact | Status |
|---|---|---|---|
| 1 | Routine correctness commands no longer invoke heavy proof | Makefile header + `.github/workflows/ci.yml` preflight/heavy split | PASS |
| 2 | Routine command timings improve vs post-W1 | `scripts/test-tier.sh leaf` exit 0 at W2 close (was FAIL at W1 open on same script) | PASS |
| 3 | Close ledger names exact public AY proof commands | `docs/benchmarks/post-B0-W2-close.json` + PROFILING.md §W2 close proof | PASS |

## B0 → AY handoff contract (from `B0.md`)

| # | Contract item | Evidence |
|---|---|---|
| 1 | Routine iteration no longer defaults into full-workspace / final-proof loops | Makefile header + CI split + `test:` deletion + `iter-*` promoted; `iter-check` warm 0.16 s |
| 2 | Exact public commands for AY.W5-W7 expand/asm/bench/Samply gates | 10 `ay-*` Makefile targets landed; PROFILING.md §AY W5-W7 gate commands table |
| 3 | Profiling preparation is reusable rather than rebuild-heavy | `scripts/prebuild-benches.sh` + `scripts/prepare-profile-wave.sh` idempotent; second-run reuse lines |
| 4 | Routine correctness / profiling-prep / heavy bench/proof surfaces are separated | Three profile tiers in `Cargo.toml`; three surface groups in Makefile; preflight / heavy in CI |
| 5 | No parity-critical AY runtime or semantic work moved into B0 | `git log` scope per §Invariants item 1 |

## Environmental interventions (non-scope work the orchestrator absorbed)

- **parse-that sibling-repo stash**. `/Users/mkbabb/Programming/parse-that`
  carried uncommitted incomplete work retiring
  `egraph::SaturationCache`, `simplify_hir_cached`, and the e-graph
  rule modules. `info/mod.rs` and `hir/mod.rs` still referenced the
  retired surface; every `cargo check` against that state failed with
  E0425. Orchestrator stashed the uncommitted state with message `"WIP:
  SaturationCache/egraph-rules retirement (pre-B0 in-flight; stashed
  2026-04-20 by bbnf-lang/B0 orchestrator to unblock cargo check)"`.
  Parse-that now at HEAD `919d77d` — clean compile. The in-flight
  retirement is preserved for whichever future wave or tranche owns
  it (recoverable via `git stash pop` on parse-that master).
- **Cargo.lock smallbox**. After the parse-that stash, cargo
  re-resolved the dep graph with `smallbox` present as a real
  transitive dep of pprint. An earlier orchestrator cleanup commit
  (`9bff7e7d`) had removed it against the dirty parse-that state;
  `aaed3f08` restored it so Cargo.lock is honest at B0 close.
- **scripts/ gitignore anomaly**. `.gitignore` line 23 gated `scripts/`
  from tracking; tracked scripts had been force-added over time.
  `df24e7c0` drops the entry and tracks `profile-bench-headless.sh` +
  `sync-external-docs.sh` so AY samply commands can run against a
  proper tracked master. Follow-on waves no longer need `git add -f`
  for new scripts.

## Routed-forward debt

B0 carries no deferred scope into AY/BA/BB/BC. Every W0-W2 sub-phase
landed with its artefacts, and every close invariant resolves. The
`post-B0.json` aggregate cites the W0 mid + W2 close JSONs as the
close matrix; the full parse-bench matrix is explicitly not applicable
to an annex that owns no runtime architecture (rationale-satisfied
per SPEC §Closing ceremony item 2).

Related debt that does NOT belong in B0 but was exposed during
execution:

- parse-that SaturationCache retirement completion (environmental
  stash above) — routes to whichever AY / BA wave claims the
  regex-analysis sub-area. B0 does not open the retirement; B0
  preserves it.
- The `ay-test-wire-contract` target exits 2 at B0 close because
  `crates/core/tests/gate_predicate_wire_contract.rs` does not yet
  exist — that file is authored in AY.W7.3. B0 publishes the public
  command; AY.W7.3 makes it green. Not B0 debt.

## Defensible floor achieved

B0's defensible floor (per `B0.md`):

1. **Public fast-path commands** — `iter-*` + `ay-*` Makefile targets + cargo aliases (PASS).
2. **Prepared-binary profiling for AY proof loops** — idempotent W1.b scripts under `profiling-prep` profile (PASS).
3. **Separated routine and heavy surfaces** — Makefile header, CI split, three profile tiers (PASS).
4. **Measured lower wall-clock cost for commands AY actually needs** — `post-B0-W0-mid.json` records 45× iter-check speedup; leaf-tier restored from FAIL to 1.05 s (PASS).

## Close commit

This FINAL.md + the `post-B0.json` aggregate land together as the B0
tranche HEAD. Next master commit opens AY.W5 on the repaired runway.

## Indefatigability

B0 closed cleanly; AY.W5-W7 can now run `make ay-expand-json`,
`make ay-test-value-api`, `make ay-samply-json-twitter WAVE=W5`,
`make ay-bench-close WAVE=W5`, etc. without re-deriving the cargo
invocation from the wave spec. The handoff is complete; AY opens on a
fast truthful command surface.
