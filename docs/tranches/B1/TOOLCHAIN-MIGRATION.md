# B1 — Toolchain Migration Draft

**Status**: draft migration plan; input to B1.W0 wave specs.
**Authored**: Wave 1 Agent D (disjoint; this file + patches/ only).
**Source of decisions**: `TOOLCHAIN-SOTA.md` (Validation Agent 2; 819 LOC).
**Source of measurements**: `docs/tranches/meta-audit/04-toolchain-pain.md`
against master `ab4d9378`.

This document is the concrete migration counterpart to Agent 2's SOTA
research. Agent 2 surveyed the decision space and produced a ranked
recommendation list; this document authors the patch files that B1.W0 will
apply, names the validation that proves each patch landed, and enumerates
the risk register for the migration window.

The migration is **infra-first** (feedback: `build-infra-first`): every
listed change lands before any AY-II/runtime work resumes, because the
iteration-loop wall dominates every downstream measurement. Half of B1.W0's
scope exists because the toolchain bled 93 ICEs and ≥130s bootstrap walls
in the 7 days meta-audit 04 observed.

---

## 1. Decision ledger

The following choices are verbatim and frozen for B1:

### 1.1 Primary bench harness: **divan** (not criterion, not bencher)

**Source**: `TOOLCHAIN-SOTA.md §SOTA bench framework decision`.
**Justification**: criterion is warm-bench-first by architecture, which
directly conflicts with feedback `no-warm-benches`; bencher is libtest-shim
with no allocation-counting and no JSON output. Divan (nvzqz/divan 0.1+)
provides cold-per-parse as the default expression, measures allocations per
sample without a separate mode, emits structured JSON, and keeps the
`harness = false` layout the workspace already uses.
**Patch**: `patches/divan-migration.md` (20-file mechanical port).

### 1.2 Secondary bench surface (CI-only): **iai-callgrind**

**Source**: `TOOLCHAIN-SOTA.md §Why add iai-callgrind as a secondary surface`.
**Justification**: instruction-count regression detection via valgrind
callgrind; zero wall-clock noise; 0.1% instruction-count deltas become
CI-observable. Linux-only (valgrind constraint), so local dev uses divan
and CI's Linux runner uses iai-callgrind.
**Patch**: `patches/divan-migration.md §iai-callgrind CI`.

### 1.3 Test runner: **cargo-nextest (required)**

**Source**: `TOOLCHAIN-SOTA.md §SOTA test runner decision`.
**Justification**: 3× faster on multi-binary workspaces; 60% faster on
I/O-bound tests; existing `.config/nextest.toml` freezing guards (Y.-1.c)
remain. The predecessor config treats nextest as optional with a
GNU-timeout fallback ladder; B1 upgrades to required.
**Patch**: `patches/nextest.toml.draft`, `patches/Makefile.draft`.

### 1.4 Rust toolchain: **pinned nightly in `rust-toolchain.toml`**

**Source**: `TOOLCHAIN-SOTA.md §ICE investigation + Rank 2`.
**Justification**: 93 `rustc-ice-*.txt` in 7 days, every one at the same
`on_disk_cache.rs:663: cannot decode AttrId` query stack, every one from
the ambient `1.96.0-nightly (9602bda1d 2026-04-05)` the developer's rustup
happened to land on. Pinning makes ICE reproduction deterministic and
bisectable.
**Pin**: `nightly-2026-04-11` (6 days past the ICE baseline).
**Patch**: `patches/rust-toolchain.toml.draft`.

### 1.5 Parallel rustc front-end: **`-Zthreads=8`**

**Source**: `TOOLCHAIN-SOTA.md §Tier 2 T2.1`.
**Justification**: blog.rust-lang.org/2023/11/09/parallel-rustc measured
22% wall reduction on dev builds, up to 50% on large crates. One line in
`.cargo/config.toml`. Amplifies ICE count (one per worker per poisoned
query) — remediation is the `make clean-incr` target, NOT dropping the flag.
**Patch**: `patches/config.toml.draft [build]`.

### 1.6 Generics sharing: **`-Zshare-generics=y`**

**Source**: `TOOLCHAIN-SOTA.md §Tier 2 T2.2`.
**Justification**: 7.3% overall speedup measured on real-world projects.
Nightly-only, well-exercised flag.
**Patch**: `patches/config.toml.draft [build]`.

### 1.7 Cranelift codegen-backend: **dev + ax-iter only (nightly-conditional)**

**Source**: `TOOLCHAIN-SOTA.md §Tier 2 T2.3`.
**Justification**: ~30% codegen-phase reduction, translating to 5-15%
total dev-build wall reduction. `[profile.release]` / `[profile.ay-final]`
/ `[profile.bench-ci]` stay on LLVM (required for "beat lightningcss"
benchmark correctness).
**Patch**: `rust-toolchain.toml.draft` (component), `config.toml.draft`
(commented until the pinned nightly's cranelift is verified non-regressive).

### 1.8 Fast linker on macOS arm64: **lld** (optional; ld64 fallback)

**Source**: `TOOLCHAIN-SOTA.md §Tier 3 T3.2`.
**Justification**: `wild` (Linux-only); `mold` (commercial license on
macOS 2.0+); `lld` is the only free-and-available option. Measured 30-50%
link-time reduction, 10-20% rebuild-time reduction. Requires `brew install
llvm`.
**Patch**: `patches/config.toml.draft [target.aarch64-apple-darwin]`.

---

## 2. Abrogation

The following components are removed; no shim, no fallback, no dual-path
(feedback: `no-workarounds`, `no-backward-compat`).

### 2.1 `bencher = "0.1"` — REMOVED

Replaced by divan. 19 `[[bench]]` entries in `crates/core/Cargo.toml`
migrate mechanically. Removed in one commit after all 19 benches are ported
(see `patches/divan-migration.md §Migration order`).

### 2.2 `criterion` — NEVER ADDED

Evaluated in `TOOLCHAIN-SOTA.md §Why NOT criterion`; rejected on warm-bench
architecture + report-artefact cost + startup overhead. Documented as
rejected so no future agent re-evaluates without cause.

### 2.3 `sccache` — NOT ADOPTED

Evaluated in `TOOLCHAIN-SOTA.md §Tier 3 T3.1`; rejected because sccache
cannot cache proc-macro crates, and bbnf-derive + egraph-derive are the
exact crates dominating the cold wall. Reconsider only if cross-runner CI
cache-hit rate is measured ≥80%.

### 2.4 `watt` (dtolnay/watt) — NOT ADOPTED IN B1

Evaluated in `TOOLCHAIN-SOTA.md §Tier 4 T4.1`; rejected for B1 because
bbnf-derive's pipeline transitively pulls `bbnf-ir`, `parse_that`,
`regex-syntax`, etc. — whole-compile cross-compile to WASM is a 2-4 week
structural change. Routed to BA/BB.

### 2.5 GNU-timeout / gtimeout test fallback — REMOVED

Replaced by nextest-required policy. The current `Makefile:47-68` cascade
(HAS_NEXTEST → HAS_TIMEOUT → HAS_GTIMEOUT → plain `cargo test`) is deleted;
nextest is installed as a preflight requirement (documented in
PROFILING.md §Dev-host setup).

### 2.6 Ambient nightly (no `rust-toolchain.toml`) — REMOVED

Replaced by explicit pin. The repo no longer rides whichever nightly each
developer last ran `rustup update` against.

---

## 3. Migration sequence

Ordered for B1.W0 execution. Each step cites its patch file and its
validation artefact.

### Step 1 — Land `rust-toolchain.toml`

- **Patch**: `patches/rust-toolchain.toml.draft`.
- **Action**: `cp patches/rust-toolchain.toml.draft rust-toolchain.toml`.
- **Validation**: `rustc --version` shows the pinned nightly; every
  developer's first invocation auto-downloads the pin.
- **Cost**: 3 min.
- **Depends on**: nothing.

### Step 2 — Rewrite `.cargo/config.toml`

- **Patch**: `patches/config.toml.draft`.
- **Action**: `cp patches/config.toml.draft .cargo/config.toml`.
- **Validation**: `cargo iter-check --help` resolves the alias;
  `cargo build --profile ax-iter -p bbnf` compiles.
- **Cost**: 15 min (including verifying each alias resolves).
- **Depends on**: Step 1 (alias profiles presume pinned nightly).

### Step 3 — Land `.config/nextest.toml`

- **Patch**: `patches/nextest.toml.draft`.
- **Action**: `cp patches/nextest.toml.draft .config/nextest.toml`.
- **Validation**: `cargo nextest run --workspace --profile ax-iter` runs
  with the new `test-threads = 4` limit; `--profile close` produces
  `target/nextest/close/junit.xml`.
- **Cost**: 10 min.
- **Depends on**: nothing (nextest profiles are harness config; independent
  of cargo profile).

### Step 4 — Rewrite `Makefile`

- **Patch**: `patches/Makefile.draft`.
- **Action**: `cp patches/Makefile.draft Makefile`.
- **Validation**: `make test` delegates to `cargo nextest run`;
  `make bench-json` delegates to `cargo bench-json`; every target exits 0
  on a clean tree.
- **Cost**: 30 min.
- **Depends on**: Steps 1, 2, 3.

### Step 5 — Port one bench to divan (exemplar)

- **Patch**: `patches/divan-migration.md §Exemplar`.
- **Action**: port `crates/core/benches/compile_pipeline.rs`; add `divan =
  "0.1"` to `crates/core/Cargo.toml [dev-dependencies]`; update
  `benches/common/timeout.rs` shim signature.
- **Validation**: `cargo bench --bench compile_pipeline` produces divan
  output within ±5% of pre-migration bencher wall.
- **Cost**: 1 hour.
- **Depends on**: Steps 2, 4.

### Step 6 — Port remaining 18 benches

- **Patch**: `patches/divan-migration.md §Migration order`.
- **Action**: mechanically port each of the 18 remaining bench files using
  Step 5's exemplar pattern. Batch into commit groups by grammar
  (5 JSON → 4 CSS → 2 sheets → rest).
- **Validation**: every bench produces divan JSON output; parity pass
  against bencher baseline within ±5%.
- **Cost**: ~1 agent-day.
- **Depends on**: Step 5.

### Step 7 — Remove `bencher = "0.1"`

- **Patch**: remove `bencher = "0.1"` from `crates/core/Cargo.toml:40`.
- **Validation**: `cargo check -p bbnf` exits 0; no `use bencher` imports
  remain (`rg -w bencher` returns 0 results).
- **Cost**: 5 min.
- **Depends on**: Step 6.

### Step 8 — Add iai-callgrind CI target

- **Patch**: `patches/divan-migration.md §iai-callgrind CI`.
- **Action**: add `benches/json_callgrind.rs`, add `iai = ["iai-callgrind"]`
  feature, add `[[bench]] name = "json_callgrind"` entry.
  Add `.github/workflows/bench-iai.yml`.
- **Validation**: CI run of the iai workflow completes on Linux; regression
  threshold 1% instruction-count firing.
- **Cost**: half-day.
- **Depends on**: Step 7.

### Step 9 — Update CI `.github/workflows/ci.yml`

- **Action**: add `taiki-e/install-action@nextest`; replace `cargo test
  --workspace` with `cargo nextest run --workspace --profile ci`.
- **Validation**: CI runs green; junit artefact uploaded.
- **Cost**: 15 min.
- **Depends on**: Step 3.

### Step 10 — Script rewrites

- **Patches**: `patches/scripts/*.action` (one per script).
- **Action**: apply per-script action (delete / keep / rewrite-to-<path>).
  Reconcile TENTATIVE markers against Wave 1-C's script catalog when it
  lands.
- **Validation**: `scripts/bootstrap-bbnf.sh` completes in ≤10s on a cache-
  hit (down from ≥130s); `scripts/bench_regression.sh` parses divan JSON.
- **Cost**: 2-3 hours.
- **Depends on**: Steps 2, 4, 7.

### Step 11 — Cross-repo propagation

- **Patch**: `patches/cross-repo-propagation.md`.
- **Action**: land `rust-toolchain.toml` + minimal `.cargo/config.toml` in
  `../parse-that` and `../pprint`. Mirror pin from Step 1.
- **Validation**: `cd bbnf-lang && cargo iter-check` succeeds with all three
  repos pinned to the same nightly.
- **Cost**: 1.5 agent-hours.
- **Depends on**: Step 1.

### Step 12 — Documentation pass (PROFILING.md)

- **Action**: add §ICE recovery (`make clean-incr`), §Dev-host setup
  (`rustup component add rustc-codegen-cranelift`, `brew install llvm`,
  `cargo install cargo-nextest --locked`). Document the bench-alias surface.
- **Validation**: doc renders; `rg` over the repo confirms every cargo
  alias appears once in PROFILING.md.
- **Cost**: 1 hour.
- **Depends on**: all prior steps.

**Total B1.W0 budget**: ~2-2.5 agent-days. Fits the bounded-annex remit.

---

## 4. Validation

Each step above names its validation artefact. Aggregated:

### 4.1 Cold iteration wall

`docs/benchmarks/post-B1-W0-iter-check-full-cold.txt` captures the cold-
wall timing of `cargo iter-check-full` under the new profile. This number
IS the ceiling for B1.W0 (feedback: `build-infra-first` promotes ceilings
to hard gates). Any subsequent measurement exceeding the ceiling re-opens
B1.W0.c.

Reproduction:
```bash
cargo clean && rm -rf target/.bbnf-cache/
time cargo iter-check-full > /tmp/iter-check-full-cold.log 2>&1
```

### 4.2 Divan output shape

`docs/benchmarks/post-B1-W0-divan-{json,css,bbnf,sheets,compile}.json`
captures divan's structured output per bench group. Each JSON file is a
single-command output (feedback: `bench-single-run`):

```bash
DIVAN_BENCH_FORMAT=json cargo bench-json > docs/benchmarks/post-B1-W0-divan-json.json
```

### 4.3 iai-callgrind CI

`.github/workflows/bench-iai.yml` runs on every PR; comment bot posts the
delta. Baseline stored in `docs/benchmarks/iai-baselines/`.

### 4.4 Nextest per-test timing

`docs/benchmarks/post-B1-W0-nextest-close.json` — junit XML is parsed to
produce per-test timing for the ceremony artefact. Feedback
`test-output-to-file` is preserved: long cargo runs redirect to a file
once, grep/tail over the file.

### 4.5 Bootstrap wall

`docs/benchmarks/post-B1-W0-bootstrap-wall.txt` captures `time
scripts/bootstrap-bbnf.sh` under both cache-cold and cache-hit scenarios.
Target: cold ≥130s (unchanged; fundamental proc-macro expansion cost);
cache-hit ≤10s (new; enabled by Step 10).

---

## 5. Risk register

Two risks carry real probability of firing during B1 execution.

### 5.1 Cranelift on pinned nightly breaks

**Probability**: medium. Cranelift's support matrix drifts across nightlies;
the pinned `nightly-2026-04-11` is 6 days past the ICE baseline and may
land on a commit where `rustc-codegen-cranelift-preview` regresses on macOS
arm64 (e.g. linker symbol resolution or debuginfo format mismatch).

**Mitigation**: the `codegen-backend = "cranelift"` line in
`config.toml.draft` is **commented out by default**. Enable only after
`cargo build --profile ax-iter -p bbnf` succeeds on the pinned nightly with
cranelift explicit; if it fails, leave commented and re-evaluate on the
next pin bump. Fallback is transparent (LLVM); no dev-loop breakage.

**Detection**: B1.W0 smoke test: `cargo build --profile ax-iter -p bbnf`
on both macOS arm64 and Linux x86_64. If either fails with cranelift
enabled, revert to LLVM for that profile.

### 5.2 `lld` path missing on macOS (ld64 fallback)

**Probability**: medium-high. The `.cargo/config.toml [target.aarch64-
apple-darwin]` block hard-codes `/opt/homebrew/opt/llvm/bin/ld.lld`. If a
developer's `brew install llvm` either was never run, was installed under a
non-default prefix, or was uninstalled, every build fails at link time
with a confusing error ("linker not found").

**Mitigation**: `patches/config.toml.draft` documents the prerequisite
inline and PROFILING.md §Dev-host setup names `brew install llvm` as the
install. If the error class proves painful, add a
`scripts/check-dev-host.sh` preflight that verifies the linker exists and
emits a clear error if not.

**Detection**: first developer who runs `cargo iter-check` without the
install will surface the error within minutes of landing.

**Fallback**: commenting the `rustflags = ["-C", "link-arg=-fuse-ld=..."]`
line in `.cargo/config.toml` reverts to Apple `ld64`. 10-20% rebuild-wall
regression but everything builds.

### 5.3 (secondary) `rust-toolchain.toml` pin too aggressive

**Probability**: low. The pin is 6 days past the ICE baseline; that
window is short enough that crate-compat regressions are unlikely but
long enough that the nightly-only flags may have stabilised or drifted.

**Mitigation**: the upgrade procedure in `rust-toolchain.toml.draft`
documents bisect-forward on ICE. If a bisect reveals any new crate-compat
failure on the pinned nightly, the pin rotates to the closest ICE-free
nightly that also compiles all workspace crates.

**Detection**: Step 5 (exemplar bench port) is the first full compile
under the pin; any pin incompatibility surfaces there.

### 5.4 (secondary) Divan baseline parity within ±5%

**Probability**: low. Divan and bencher use different inner-loop shapes
(divan: explicit per-sample setup/body split; bencher: a single `iter`
closure). A grammar whose bencher-measured wall was dominated by
setup-inside-iter could report a different divan wall.

**Mitigation**: Step 5 (exemplar) measures ±5%. If the delta exceeds ±5%,
the shim in `benches/common/timeout.rs` needs adjustment (the `setup_input`
clone may double-count an expensive load). Re-measure before porting the
remaining 18.

---

## 6. Cost

Per-step estimate, aggregated:

| Step | Cost | Kind |
|---|---|---|
| 1. rust-toolchain.toml | 3 min | config |
| 2. .cargo/config.toml | 15 min | config |
| 3. nextest.toml | 10 min | config |
| 4. Makefile | 30 min | config |
| 5. Exemplar bench port | 1 hr | migration |
| 6. Remaining 18 benches | ~1 day | migration |
| 7. Remove `bencher` dep | 5 min | cleanup |
| 8. iai-callgrind CI | half-day | new infra |
| 9. CI workflow update | 15 min | config |
| 10. Script rewrites | 2-3 hr | migration |
| 11. Cross-repo propagation | 1.5 hr | config |
| 12. Documentation | 1 hr | prose |
| **Total** | **~2-2.5 agent-days** | |

### Deferred (not counted)

| Item | Cost | Target tranche |
|---|---|---|
| Derive cache lift to $XDG_CACHE_HOME | 1-2 days | BA |
| Watt proc-macro precompilation | 2-4 weeks | BA/BB |
| Micro-bench architecture split (`crates/bench-*`) | 1 week | BA |
| Parametric bench collapsing | 1 day | post-B1 polish |
| sccache (if CI cache-hit rate ≥80% measured) | half-day | TBD |

---

## 7. What this document does NOT land

Feedback `no-deferrals` applies — B1 integrates everything listed in the
migration sequence in one pass. Items in §6 Deferred above are
**structural** changes whose scope exceeds B1's bounded-infra remit; they
are named here and routed to BA/BB per Agent 2's scope recommendations.
The feedback applies to **optimizations within B1 scope**; structural
changes outside the declared scope are correctly deferred.

### Items deliberately excluded from B1

- Derive cache relocation (`$XDG_CACHE_HOME`). B1's bootstrap-script fix
  captures the primary win (≥130s → ≤10s) without the relocation; BA owns
  the structural move.
- Bench architecture restructure (`crates/bench-*`). Decoupling bench
  compilation-units from `crates/core` is a week-class refactor.
- Watt / WASM-precompiled proc-macros. 2-4 weeks; structural.

### Items B1 DOES land (again, for clarity)

- `rust-toolchain.toml` with pin.
- Rewritten `.cargo/config.toml` with alias surface + profiles + build flags.
- Nextest as required; updated `.config/nextest.toml` with 4 profiles.
- Makefile simplification (470 → ~150 lines).
- Divan migration (all 19 bench binaries + iai-callgrind secondary).
- Removal of `bencher`, dead fallback ladders, optional-nextest detection.
- Script rewrites (bootstrap content-hash guard; profile.sh samply-only;
  bench_regression.sh parses divan JSON).
- Cross-repo pin propagation (`../parse-that`, `../pprint`).
- CI workflow update (nextest + iai-callgrind).
- PROFILING.md updates (ICE recovery, Dev-host setup, alias documentation).

---

## 8. Feedback-memory alignment

- `build-infra-first`: every B1.W0 step is infra. Runtime work (AY-II etc.)
  resumes only after Step 12.
- `no-warm-benches`: divan configured with `sample_size = 1` and cold-per-
  parse (§divan-migration.md).
- `bench-single-run`: every `cargo bench-*` alias is one invocation
  (§config.toml.draft [alias]). Makefile `bench-json` target delegates to
  the alias; no internal N-way sweep.
- `bench-sequential-regression`: divan runs regression-check inside the
  harness, serially.
- `single-cargo-per-target`: the alias surface is per-working-set, not
  per-package. Fleet orchestrators use `iter-check-full` instead of
  N parallel `cargo check -p X`.
- `iter-profile-always`: every `iter-*` alias carries `--profile ax-iter`.
- `clean-instrumentation`: Makefile has no `eprintln`, no `tee`, no timing
  hooks. Callers redirect stdout themselves.
- `no-workarounds`: cranelift disabled (commented) rather than shimmed
  when unavailable; lld path hard-coded rather than probed; no fallback
  ladder for nextest.
- `no-backward-compat`: `bencher` removed, not dual-path alongside divan.
- `no-deferrals` (within B1 scope): every in-scope optimization lands in
  this migration, not split across later tranches.
- `test-output-to-file`: close-ceremony nextest profile writes junit; does
  not re-invoke for variants.
- `high-parallelization`: Step 6 (18 bench ports) parallelizes naturally
  across agents (each agent owns one grammar's sub-dir; disjoint).

---

## 9. Patch file index

All under `docs/tranches/B1/patches/`:

| File | Purpose |
|---|---|
| `rust-toolchain.toml.draft` | Pinned nightly; components; upgrade cadence |
| `config.toml.draft` | Rewritten `.cargo/config.toml` with profiles + aliases + build flags + target overrides |
| `nextest.toml.draft` | Rewritten `.config/nextest.toml` with 4 profiles (default / ax-iter / ci / close) |
| `divan-migration.md` | Mechanical bencher → divan port; 20-file enumeration; exemplar before/after; iai-callgrind CI example |
| `Makefile.draft` | Simplified Makefile (470 → ~150 lines); delegates to cargo aliases |
| `scripts/bench_regression.sh.action` | Rewrite to parse divan JSON instead of bencher stdout |
| `scripts/bisect-fastpath.sh.action` | Keep; minor cargo-command substitution |
| `scripts/bootstrap-bbnf.sh.action` | Rewrite: delete `rm -rf target/.bbnf-cache/`; add content-hash guard |
| `scripts/check-cst-invariants.sh.action` | Keep; optional nextest substitution |
| `scripts/deploy.sh.action` | Keep; orthogonal |
| `scripts/extract_hotspots.py.action` | Keep; orthogonal |
| `scripts/profile.sh.action` | Rewrite: drop bencher-specific flags; delegate to `cargo bench-*` aliases |
| `derive-cache-design.md` | DEFERRED to BA; design captured for successor tranche |
| `cross-repo-propagation.md` | Sibling-repo (`../parse-that`, `../pprint`) pin/config files |

---

## 10. Confirmation checklist for the orchestrator

Before B1.W0 commits live files:

- [ ] Agent 2's research merged to master (or cherry-picked from
      `worktree-agent-a9c6ca4b`, commit `e0f556d6`).
- [ ] User confirms pinned nightly (`2026-04-11`) is acceptable; if a
      different date is required, update `rust-toolchain.toml.draft`.
- [ ] Wave 1-C's script catalog reconciled against `patches/scripts/*.action`
      TENTATIVE markers.
- [ ] Wave 1-B's sibling-repo matrix reconciled against
      `patches/cross-repo-propagation.md`.
- [ ] CI runner (Linux) has valgrind available; confirm via
      `.github/workflows/bench-iai.yml` dry-run before gating.
- [ ] `brew install llvm` documented in README + PROFILING.md.
- [ ] `cargo install cargo-nextest --locked` documented.

Once confirmed, B1.W0 executes Steps 1-12 in order. Expected landing
window: 2-3 business days.
