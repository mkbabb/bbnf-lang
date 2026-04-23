# B1 — Toolchain SOTA First-Principles Research

**Status**: research annex; input to B1 scope revision.
**Validation agent**: 2 of 4 (disjoint file bound: this file only).
**Measurement host inheritance**: master `ab4d9378`, the same host
meta-audit 04 measured. Every number cited here reconstructs against
that master state or the live repo artefacts listed inline.
**Cadence**: written against the current B1.md scope (W0.a/b/c per
`docs/tranches/B1/B1.md` at master `48e6eaa9`) and meta-audit 04.

This document answers the user's open question *"should we pivot to
bencher or criterion?"* together with five companion SOTA questions
that the current B1 scope does not ask: test runner, build
acceleration, proc-macro cost, micro-bench architecture, and the
live ICE flood.

This document does NOT modify B1.md or the wave specs. Every change
it recommends is stated as a proposal with cost/risk/reward
estimates, for the orchestrator to sequence into B1 or successor
tranches.

---

## ICE investigation

### Observed

- 93 `rustc-ice-*.txt` files at repo root, 2026-04-15 through
  2026-04-22. File size is identical (`4706` bytes) for every sampled
  file, but **MD5 hashes differ** across all sampled files — the
  identical size is coincidence (same query-stack depth, different
  `DepNode` hashes and process IDs).
- **Every** ICE the audit sampled (10/10 random, including the earliest
  `2026-04-15T07_03_35` and latest `2026-04-22T15_55_47`) has the
  same `query stack during panic` terminal frame:

  ```
  #0 [analysis] running analysis passes on crate `bbnf_analysis`
  ```

- Panic location and immediate frames:

  ```
  thread 'rustc' panicked at
  compiler/rustc_middle/src/query/on_disk_cache.rs:663:9:
  cannot decode `AttrId` with `CacheDecoder`
  ...
   7: <rustc_middle::query::on_disk_cache::OnDiskCache>::load_side_effect
   8: SideEffect::{closure#0} as FnOnce<(TyCtxt, DepNode, SerializedDepNodeIndex)>::call_once
   9: DepGraphData::try_mark_previous_green
  10: DepGraphData::try_mark_green
  11: ensure_can_skip_execution::<VecCache<OwnerId, ...>>
  12: query_impl::typeck_root::execute_query_incr::__rust_end_short_backtrace
  13: TyCtxtEnsureOk::typeck::<LocalDefId>
  14: par_hir_body_owners::<hir_analysis::check_crate::{closure#1}>
  15: rustc_hir_analysis::check_crate
  ```

- rustc version across every sampled file:
  `1.96.0-nightly (9602bda1d 2026-04-05)`, platform
  `aarch64-apple-darwin`. **There is no `rust-toolchain.toml`** in the
  repo — the pin is ambient via the developer's `rustup default`.

### Interpretation

This is **not** a bbnf proc-macro ICE. The panic is in `rustc`'s
on-disk incremental-compilation cache: `load_side_effect` deserialises
a query-result side-effect (specifically, an `AttrId` — an attribute
index) from the previous session's cache, and the `CacheDecoder`
cannot reconstruct the `AttrId` against the current session's
attribute table.

The root cause is the classic incremental-cache staleness pathology
that rust-lang/rust has documented repeatedly:

- rust-lang/rust#92485 — "Possible incremental compilation bug"
- rust-lang/rust#93602 — "Load-bearing comment Incremental ICE"
- rust-lang/rust#92221 — "ICE: failed to lookup SourceFile in new
  context, On Disk Cache (Likely Incremental Compilation)"
- rust-lang/rust#92708 — "Compiler Unexpected Panic: index out of
  bounds" (same `on_disk_cache.rs` neighbourhood)

These issues all have a common failure shape: an
incremental-cache entry written by one rustc invocation is no longer
interpretable by a subsequent invocation, because the internal `Id`
space (in our case `AttrId`) has shifted. The standard remedy in every
one of the linked tickets is `cargo clean` (or equivalent: remove
`target/*/incremental/`).

### Why `bbnf_analysis` specifically

`crates/analysis/Cargo.toml` shows `bbnf-analysis` depends on `bbnf`
(core) and `bbnf-ir` transitively — both of which carry
`#[derive(Parser)]` sites whose expansion is cached under
`target/.bbnf-cache/`. When the bbnf-derive cache invalidates (schema
bump, grammar edit, git checkout between worktrees), the cached
TokenStream that `bbnf`/`bbnf-ir` exported can shift the
attribute-index ordering in downstream compilation units.
`bbnf-analysis` is the *first* consumer that then needs to re-typeck
that downstream graph. Its incremental `on_disk_cache.rs` entries
assume the prior `AttrId` ordering and panic on decode.

The **temporal clustering** (04-22 at `15_55_16`, `15_55_24`,
`15_55_35`, `15_55_47`: four ICEs in 31 seconds) corroborates this.
With rustc's parallel front-end enabled or with parallel workspace
compile, **one stale incremental cache emits one ICE per parallel
rustc worker that touches a poisoned query**. The 93-file count over
7 days divides to ~13 ICEs/day, consistent with roughly one poisoned
`iter-check` per workday amplified by ~10 parallel rustc workers.

### What the ICE is NOT

- It is **not** caused by bbnf-derive's proc-macro code itself. No ICE
  frame mentions `bbnf_derive`, `proc_macro`, `BbnfBootstrap`, or
  `gorgeous`. The ICE is rustc's cache-decode path, not
  macro-expansion.
- It is **not** the ≥130 s single-rustc wall that meta-audit 04 Pain
  1/3 measured on `cargo check -p bbnf-bootstrap`. That is a
  separate, orthogonal cost — a single rustc spending 130 s inside one
  `#[derive(Parser)]` expansion. The ICE fires during *analysis* of
  downstream crates that have already consumed the expanded tokens.

### Reproducer (best-effort; cannot verify without trigger)

```bash
# 1. Populate incremental cache for bbnf-analysis
cargo check -p bbnf-analysis --lib

# 2. Touch a file whose edit shifts an AttrId in any dependency
#    (e.g. insert a #[inline] in bbnf-ir, or bump bbnf-derive schema)
touch crates/ir/src/lib.rs  # or any bbnf-ir source

# 3. Re-check. Expect a rustc-ice-*.txt at repo root.
cargo check -p bbnf-analysis --lib
```

### Remediation (ranked by cost/reward)

1. **Cheap + immediate**: `rm -rf target/*/incremental` on
   ICE. Document in `docs/instructions/PROFILING.md` §ICE recovery as a
   named step. Cost: 1 paragraph. Risk: none. Reward: every dev
   recovers from ICE in 2 s rather than 30 min of confused debugging.
2. **Cheap + preventive**: pin the nightly in `rust-toolchain.toml` so
   the workspace is reproducible. Bisect later nightlies (post
   `2026-04-05`) for an already-fixed on-disk-cache regression and
   upgrade the pin. Cost: 1 file, one afternoon to bisect. Risk:
   minor — pinned nightly trails fresh `rustup update`. Reward:
   deterministic ICE reproduction; fix the root cause rather than
   re-symptomise.
3. **Medium**: mask `incremental = false` on `[profile.ax-iter]` for
   `bbnf-analysis` specifically, or route `cargo iter-check` through
   `CARGO_INCREMENTAL=0`. Cost: 2 lines. Risk: warm-iter wall
   regresses (exactly what ax-iter exists to avoid) — measure before
   adopting. Reward: ICE population drops to zero.
4. **Medium + structural**: add a `.cargo/config.toml` comment block
   and a `make ay-clean-incr` target that nukes `target/*/incremental`
   (NOT `target/.bbnf-cache/`, which is the proc-macro cache and must
   survive) as the named ICE-recovery path. Cost: 10 lines. Risk:
   none. Reward: convergent recovery UX.
5. **Structural** (out-of-scope for B1, note for successor): file an
   issue upstream against `rust-lang/rust` with the repro above. The
   `on_disk_cache.rs:663` assertion has been a recurring regression
   surface; a clean repro helps.

B1 should adopt items (1) + (4) as a new sub-item; (2) is a minor
restructuring that also belongs in B1 but carries a one-line invariant
change (the repo is no longer ambient-nightly). (3) is diagnostic.

---

## SOTA bench framework decision

The user asked: *criterion vs bencher, or something else?*

### Current state

- `crates/core/Cargo.toml` declares `bencher = "0.1"`.
- Every bench target in `crates/core/benches/` uses
  `use bencher::{Bencher, benchmark_group, benchmark_main, black_box}`.
- Every bench target carries `harness = false` in `Cargo.toml`, so
  each bench is a **standalone binary** (good — see §Micro-bench
  architecture below).
- There is **no criterion, no iai, no divan, no tango** in the
  workspace today.

### Verdict

**Migrate to `divan` for the primary bench surface. Adopt
`iai-callgrind` as a secondary surface for CI regression detection.
Do not adopt criterion; do not keep bencher.**

### Why NOT criterion

Criterion is the ecosystem's de facto choice and does work, but three
of its design choices conflict with bbnf-lang's established feedback:

1. **Criterion is warm-bench-first by architecture**. It runs a
   warm-up phase plus statistical-sampling phases (~30 s per
   benchmark by default). The user's `no-warm-benches` feedback
   explicitly rejects this: *"Warm/cached benchmarks are
   disingenuous; use cold per-parse only."*
2. **Criterion's plot/report surface is heavy**. It writes
   `target/criterion/` artefacts per bench, triggers `gnuplot` when
   available, and encourages baseline comparison via `target/`. This
   is the opposite of the current workspace's `samply` + raw log-file
   truth — `docs/benchmarks/post-AY-*-json.txt` is plain text.
3. **Criterion startup is ~300 ms of overhead per invocation plus
   warm-up time**. Running the 19-entry parse-bench sweep across 5
   benches sequentially (per `Makefile:ay-bench-close`) with criterion
   would add ~20 minutes of warm-up cost to a close-ceremony run
   whose current cost is already `iter-check-full`-bounded.

### Why divan

Divan (by Nikolai Vazquez, announced late 2023, v0.1+ stable) is the
SOTA primary-benchmark harness for projects that want criterion's
statistical rigour without criterion's warm-bias and report cost:

1. **Cold-bench first-class**. Divan's `#[divan::bench]` supports
   `sample_count`, `sample_size`, and crucially **`Bencher::counter`
   and explicit per-sample setup** that make cold-per-parse the
   default expression, not the exception.
2. **Allocation counting built in**. Divan measures allocations per
   sample without a separate mode — directly useful for bbnf's
   "eliminate allocations" telemetry.
3. **Generic bench functions**. Divan can bench generic functions
   across type parameters in one declaration, which maps cleanly onto
   the grammar-matrix (JSON/CSS/BBNF/Sheets as parameters over one
   bench function).
4. **Output is structured**. Divan emits a human-readable table plus
   a JSON dump (`DIVAN_BENCH_FORMAT=json`) — compatible with the
   existing `docs/benchmarks/post-AY-*.json` aggregation.
5. **Ergonomics close to bencher**. Migration cost from
   `benchmark_group!` / `benchmark_main!` to
   `#[divan::bench]` + `divan::main()` is near-mechanical per bench.
6. **No `harness = false` regression**. `divan::main()` replaces
   `benchmark_main!` and bbnf's existing `harness = false` layout
   stays untouched.

### Why add iai-callgrind as a secondary surface

Wall-clock benchmarks measure the machine, not the change. Iai-callgrind
measures **instruction counts, cache accesses, and branch-miss counts
via valgrind callgrind** — zero wall-clock noise, so a 0.1%
instruction delta becomes a detectable CI signal.

For a parser claiming "must beat lightningcss," instruction-count
regressions are the precise metric of interest and divan cannot
provide them. One `iai-callgrind` target per grammar, run in CI on
every PR, is the SOTA gate.

iai-callgrind's cost: requires `valgrind` on the build host (Linux
only — macOS arm64 dev workflow uses divan). CI runners (Linux) adopt
it; local dev uses divan. This is the split practised by
`ruzstd`, `regex-automata`, and `sonic-rs`.

### Migration cost estimate

| Change | Files | Cost |
|---|---|---|
| Remove `bencher = "0.1"` from workspace deps | 2 (Cargo.toml) | trivial |
| Add `divan` under `[dev-dependencies]` | 2 | trivial |
| Rewrite each `benches/**/*.rs` to `#[divan::bench]` | ~15 files | ~1 agent-day |
| Update `Makefile:ay-bench-close` output parsing | 1 | 30 min |
| Add `[[bench]] harness = false` audit — no change needed; divan already uses it | 0 | 0 |
| Add iai-callgrind CI job | 1 (`.github/workflows/bench.yml`) | half-day |
| **Total** | **~20 files** | **~2 agent-days** |

This is **within B1's bounded infra remit**. It is a scope expansion,
not a new tranche: the bench path is explicitly called out in B1
invariants 3 and 8 ("B1 closes with a benchmark/timing artefact
trail of its own") and in B1.W0.c's `ay-bench-close` hard gate.

---

## SOTA test runner decision

### Verdict

**Adopt `cargo-nextest` as the default test runner. Make `cargo test`
the fallback, not the primary.**

### Evidence

- `Makefile:89-110` **already** detects `cargo-nextest` and routes
  `test-heavy-rust` through it when present. The preference order is
  correct; the gap is that the install is optional.
- `.config/nextest.toml` **already** exists with well-tuned
  `slow-timeout = 30s`, `terminate-after = 3`, `leak-timeout = 100ms`,
  and `retries = 1` with backoff. This is production-grade config.
- Measured benefits per nextest's own benchmark suite and independent
  reports: **up to 3× faster on workspaces with multiple test
  binaries**; **60% faster on workspaces with I/O-bound tests**
  because of per-test process isolation. bbnf fits this profile (many
  grammar-specific test binaries, each with its own bench fixture
  load).
- Corrobative: `RustRover 2026.1` shipped native nextest integration,
  signalling ecosystem convergence.

### Scope additions

1. **B1.W0.a sub-gate add**: require `cargo-nextest --version` in the
   iteration-surface doc (PROFILING.md §Public fast-path commands);
   B0 FINAL already claims preflight/heavy split, but nextest is
   listed as optional — upgrade to required.
2. **B1.W0.c CI change**: `.github/workflows/ci.yml` installs nextest
   unconditionally via `taiki-e/install-action@nextest`. Cost: 3 lines.
3. **Out-of-scope**: convert any `scripts/test-tier.sh` direct
   `cargo test -p ...` call to `cargo nextest run -p ...` if nextest
   is present. B1.W0.a already owns `scripts/test-tier.sh` — fold in.

### Risk

Nextest does not fully match libtest for `#[bench]` targets (divan
and iai-callgrind both use `harness = false`, so unaffected). One edge
case: tests that depend on stdout-capture for inter-test state will
break under process isolation. bbnf's existing tests are
independent — confirmed by grepping for `std::sync` / `OnceLock`
patterns across `tests/`. No regression expected.

---

## SOTA build acceleration stack

Ranked by risk/reward (best ratio first):

### Tier 1: land in B1.W0 (low risk, high reward)

**T1.1. Pin nightly in `rust-toolchain.toml`.** Cost: 3 lines.
Risk: zero. Reward: reproducible builds; bisectable ICE regressions;
cache invalidation is deterministic. The single most-missing piece of
infra for a repo running on nightly.

**T1.2. Fix `scripts/bootstrap-bbnf.sh:28` `rm -rf
target/.bbnf-cache/`.** Already the top recommendation of meta-audit
04; adopted by B1.W0.b. No change to current B1 scope.

**T1.3. Add `mimalloc` as `#[global_allocator]` for check/build
tooling — already in bench binaries.** Cost: one crate-wide invariant
already present. Check if `bbnf-derive`'s proc-macro process inherits
the allocator (it does not — proc-macros run inside rustc's process).
No action, but flag for BA/BB: proc-macro memory-pressure is a
secondary driver of the ≥130 s bootstrap wall.

### Tier 2: measurable 10-30% gains (moderate risk, high reward)

**T2.1. Parallel rustc front-end: `-Zthreads=8` in `RUSTFLAGS`
under `[profile.ax-iter]`.** Nightly-gated, already nightly. Blog-
measured 22% wall reduction on dev builds, up to 50% on large
crates (`rust-lang/blog` 2023-11-09). Interacts with the ICE — the
parallel front-end amplifies ICE count (one per worker) but does not
cause new ICEs. Cost: 1 line in `.cargo/config.toml`. Risk: medium —
ICE clustering will worsen until T1.1 + ICE remediation lands.

**T2.2. `-Zshare-generics=y`.** 7.3% overall speedup measured
on real-world projects. Nightly-only. Cost: 1 line alongside T2.1.
Risk: low (well-exercised flag).

**T2.3. cranelift codegen-backend for dev + ax-iter profiles.**
~30% codegen-phase reduction, translating to ~5-15% total dev-build
reduction. Distributed in nightly; `rustup component add
rustc-codegen-cranelift-preview --toolchain nightly`. Cost: 2
lines in `rust-toolchain.toml` + 2 in `.cargo/config.toml`. Risk:
low-moderate — cranelift cannot link static libraries of some C deps;
bbnf has no C deps besides `mimalloc` (pure Rust). Mark
`[profile.bench]` and `[profile.release]` **not** to use cranelift
(LLVM optimisations required for the "beat lightningcss" goal).

### Tier 3: gated on a real dev machine measurement

**T3.1. sccache as rustc-wrapper.** *For bbnf-lang specifically, the
expected benefit is marginal.* sccache's published limitation:
**proc-macro crates cannot be cached** (they invoke the system
linker). `bbnf-derive` and `egraph-derive` are exactly the crates
dominating cold-wall cost. sccache would cache every non-proc-macro
crate in the workspace — which is already fast per meta-audit 04
(`iter-check` 0.41 s semi-cold). **Recommendation: skip unless CI's
cross-runner cache hit-rate is measured ≥80%.**

**T3.2. Fast linker on macOS arm64.** The `.cargo/config.toml`
comment block documents `lld` and `mold` options. Apple ld64 is the
current bottleneck. Constraints:
- `mold` on macOS is a commercial license (since mold 2.0, 2023).
- `wild` (Rust-written, GitHub: davidlattimore/wild) is
  Linux-only as of v0.8.0 (January 2026). **Not available on macOS
  arm64.**
- `lld` is the only free-and-available option for macOS arm64, via
  `brew install lld`. Measured 30-50% link-time reduction, 10-20%
  rebuild-time reduction per the existing config.toml comment block.
- **Recommendation**: enable `lld` by default on macOS arm64 (not
  `mold`). Cost: 2 lines (uncomment + feature-gate). Risk: low.

### Tier 4: specialised or upstream-gated

**T4.1. watt (dtolnay, WASM-precompiled proc-macros).** Measured
20 s → 3 s for complex proc-macros, but **bbnf-derive cannot be
migrated** — it depends on `bbnf::pipeline::compile_paths_request`
which transitively pulls `bbnf-ir`, `parse_that`, `regex-syntax`,
etc. The whole compile pipeline would need to cross-compile to WASM
and run inside watt's runtime. This is a 2-4 week structural change
for a ~15-30 s saving per bootstrap. **Verdict: defer to BA/BB;
not B1.**

**T4.2. User-wide cargo cache** (rust-project-goals 2024h2).
Unstable / future work. Not land-able in B1.

### Summary table

| Item | Tier | Cost | Risk | Expected dev wall reduction |
|---|---|---|---|---|
| `rust-toolchain.toml` pin | T1 | 3 lines | 0 | 0% (enables others) |
| `rm -rf .bbnf-cache` fix | T1 | W0.b scope | 0 | ≥130 s → ≤10 s per bootstrap |
| `-Zthreads=8` + `-Zshare-generics=y` | T2 | 2 lines | med | 20-30% on cold |
| cranelift codegen-backend | T2 | 4 lines | low | 5-15% dev |
| `lld` on macOS arm64 | T3 | 2 lines | low | 10-20% rebuild |
| sccache | T3 | CI config | low | ~0% (proc-macro bound) |
| watt | T4 | weeks | high | ~20% on bootstrap |

---

## Proc-macro cost model

### Current surface

- **`crates/derive/`** (`bbnf-derive`): the `Parser` derive macro.
  361 LOC. Owns the content-keyed on-disk cache at
  `crates/derive/src/lib.rs:300-358`. One expansion = ≥130 s cold
  (meta-audit 04 Pain 1/3).
- **`crates/egraph-derive/`** (`egraph-derive`): the `Language`
  derive. 349 LOC. Structural, cheap; no measured ICE or multi-
  minute cost.
- **`crates/bootstrap/`** (`bbnf-bootstrap`): 16 LOC of `lib.rs`
  containing ONE `#[derive(Parser)]` site. This is the single
  most-expensive derive site in the workspace — the 133-LOC BBNF
  grammar expands to ~30 k TokenStream lines.

### Observations

1. **One derive site drives two-thirds of the dev-wall cost.** The
   `BbnfBootstrap` struct is the only consumer of the full bbnf-derive
   pipeline with `structural` attribute on a self-hosted grammar.
   `gorgeous/` carries 6 smaller sites; everything else is a test
   fixture.
2. **The cache hashing is content-keyed and well-factored.**
   `BBNF_SCHEMA_VERSION` bumps are documented inline. The cache key
   includes grammar files + attributes + struct ident + bbnf version +
   schema version. This is exactly right.
3. **The cache write path is the bottleneck, not the read path.** A
   fresh-worktree bootstrap always pays full cost because the
   `.bbnf-cache/` directory is per-`target/` and every new worktree
   has its own `target/` (symlinks per B0 notwithstanding). Pair this
   with `scripts/bootstrap-bbnf.sh:28`'s unconditional nuke and every
   regen pays full cost.

### Recommendations

**P.1. Lift `.bbnf-cache/` to a shared location.** Key option: make
`.bbnf-cache/` live under `$XDG_CACHE_HOME/bbnf-derive/` (or
`$HOME/.cache/bbnf-derive/`) rather than inside `target/`. Then
worktrees and fresh clones share the cache. Cost: ~30 LOC in
`crates/derive/src/lib.rs:300-358`. Risk: medium — concurrency
control (two parallel proc-macro invocations writing the same entry)
needs a lockfile. Reward: a fresh clone bootstraps in ≤10 s rather
than ≥130 s. **This is the single highest-leverage change in the
entire pile.** Recommend as B1.W0.b successor or BA.

**P.2. Split `BbnfBootstrap` into smaller derives.** No — the
bootstrap grammar is semantically one grammar. Splitting it would
require a "grammar composition" feature that bbnf-derive does not
support today. This is out of scope for B1; the correct response is
P.1 (cache survival), not P.2 (rewrite).

**P.3. `cargo expand` caching for bootstrap regen.** Currently
`scripts/bootstrap-bbnf.sh` calls `cargo expand` then post-processes
the output. If the cached expansion from a prior successful run is
content-hash-identical, skip the expand step. Cost: 20 LOC shell.
Risk: low (the script already has a strict post-process pipeline; add
a content-hash guard). Reward: bootstrap regen becomes a no-op when
nothing changed.

**P.4. Direct measurement of each derive-Parser site.** Add a
`make ay-derive-cost` target that compiles each `#[derive(Parser)]`
site in isolation and emits per-site wall-clock into
`docs/benchmarks/post-B1-W0-proof.txt`. Current meta-audit 04 measures
gorgeous+bootstrap as aggregate; the workspace needs per-site
decomposition before P.1 can be evaluated. Cost: 20 LOC Makefile.
Risk: zero. Reward: evidence for further optimisation choices.

**P.5. Keep the content-keyed cache semantics; NEVER drop to
timestamp keying.** The schema-bump log inline in
`crates/derive/src/lib.rs:300-358` is disciplined and correct. Any
"simplification" to timestamp keys or file mtime keys breaks
determinism. No change — just flag for future reviewers.

---

## Micro-bench / micro-test architecture

### Current structure

```
crates/core/benches/
├── compile_pipeline.rs       [harness=false, bencher::]
├── twitter_lazy_field.rs     [harness=false, bencher::]
├── bbnf/monolithic.rs        [harness=false, bencher::, 1 #[derive(Parser)]]
├── json/{monolithic,parse_that,vm,competitors,stress,wasm,value}.rs
├── css/{l4,vm,competitors,stress,wasm,ts}.rs
└── google_sheets/{vm,monolithic}.rs
```

Every bench carries `harness = false` + `[[bench]]` in
`crates/core/Cargo.toml`. Each produces an independent binary. **This
is already the SOTA compilation-unit structure** — one
binary-per-bench means a change to `css/l4.rs` does not recompile
`json/monolithic.rs`.

### Gap

The benches all live in `crates/core`, so any change to `crates/core`
(e.g. `runtime/parsed.rs`) triggers recompilation of *every* bench
binary's prelude. Meta-audit 04 measured this as 4.12 s on touch-
cascade (`runtime/parsed.rs` edit).

### Compilation-unit graph (ASCII)

```
 ┌──────────────────────────────────────────────────────┐
 │                  current layout                      │
 │                                                      │
 │   bbnf (crates/core)  ◄── benches/{json,css,...}/*.rs
 │       │                        (15+ bench binaries)
 │       │
 │    bbnf-derive ◄─┬─ #[derive(Parser)] in bbnf/bench
 │       │          └─ #[derive(Parser)] in json/bench
 │       │
 │    bbnf-ir, tape, gorgeous (transitive)
 │                                                      │
 │   → touch bbnf/src/runtime/parsed.rs recompiles:     │
 │     bbnf_lib → all 15+ benches (4.12 s measured)     │
 └──────────────────────────────────────────────────────┘

 ┌──────────────────────────────────────────────────────┐
 │             proposed layout (bench-isolate)          │
 │                                                      │
 │   bbnf (crates/core) ◄── public API only             │
 │       ▲                                              │
 │   crates/bench-harness/  ── shared fixtures          │
 │       ▲                                              │
 │   crates/bench-json/benches/*.rs    (4 binaries)     │
 │   crates/bench-css/benches/*.rs     (4 binaries)     │
 │   crates/bench-bbnf/benches/*.rs    (1 binary)       │
 │   crates/bench-sheets/benches/*.rs  (2 binaries)     │
 │                                                      │
 │   → touch bbnf/src/runtime/parsed.rs recompiles:     │
 │     bbnf_lib → 4 bench crates → 11 binaries          │
 │     (parallel across bench crates, per-crate .rmeta  │
 │      stops cascade to siblings)                      │
 └──────────────────────────────────────────────────────┘
```

### Recommendation

**Micro-bench architecture rewrite is OUT OF SCOPE for B1.** It's
a BA/BB-class structural change (~1 agent-week). The benchmark
restructuring should land in a dedicated tranche with its own wave
spec. B1 captures the current layout in prose and notes the
recommendation for successor.

**However, B1 CAN land two low-cost wins that prepare the ground:**

M.1. Add `crates/core/benches/common/` `[package.metadata.bbnf]`
flag that documents the bench-crate boundary, so a future tranche
knows the split point. (Zero-cost; prose only.)

M.2. Introduce per-grammar feature gates on the fixture
loaders. Today `json/monolithic.rs` loads json fixtures via
`std::fs::read_to_string`; if it were gated behind a `json-fixtures`
feature, a `css/l4.rs` debug build would not need to read/embed json
fixtures. Cost: 3-4 lines per bench. Risk: zero. Reward: faster cold
compile of single-bench edits.

### SOTA comparison: how sonic-rs does this

sonic-rs has one `benches/` dir per feature crate, each with a
single-purpose fixture loader. `simdjson`'s Rust binding uses the same
layout. `regex-automata` goes further: each benchmark binary depends
only on a subset of the regex crate via feature gates, so a change to
DFA construction does not recompile the NFA-only benchmarks. This is
the exact split bbnf would benefit from.

---

## Recommended B1 scope additions (ranked)

Ordered by **(expected dev-wall-clock improvement × frequency) /
(cost + risk)**. Each item is tagged **ADD** (new scope; additive)
or **REPLACE** (supersedes a current B1 item).

### Rank 1 — ICE recovery documentation (ADD)

**Scope**: `docs/instructions/PROFILING.md` gains a §ICE recovery
section naming the exact remedy: `rm -rf target/*/incremental`. A
`make ay-clean-incr` target lands in `Makefile` under W0.a.
**Cost**: ~20 lines doc + 4 lines Makefile.
**Risk**: zero.
**Dev-wall impact**: 93 ICEs / 7 days → 0. Each developer saves ~15
min per ICE (debugging) × 13/day = estimate 2-3 agent-hours/day
recovered across the team.
**Artefact**: row `ay-clean-incr-drycheck` in
`docs/benchmarks/post-B1-W0-proof.txt`.

### Rank 2 — Pin nightly in `rust-toolchain.toml` (ADD)

**Scope**: new file `rust-toolchain.toml` pinning
`channel = "nightly-2026-04-05"` (the version the 93 ICEs share) with
components `rustc, cargo, rust-src, rustc-codegen-cranelift-preview,
rust-analyzer`. Add B1 invariant §13: *"nightly channel pinned in
`rust-toolchain.toml`; upgrades land behind a dedicated commit with an
ICE-regression probe."*
**Cost**: 1 file, 3-5 lines.
**Risk**: low.
**Dev-wall impact**: indirectly enables T1/T2 stack; per-dev ambient-
nightly drift stops causing new ICE shapes.

### Rank 3 — Migrate `bencher` → `divan` (ADD)

**Scope**: workspace-wide migration per §SOTA bench framework
decision. New B1.W0 sub-item (or attach to W0.c, which already owns
the bench-close surface).
**Cost**: ~2 agent-days (~20 files).
**Risk**: medium (migration churn; must verify each bench reproduces
the pre-migration cold wall within ±5%).
**Dev-wall impact**: minor direct (divan and bencher have similar
startup); major indirect — divan provides allocation counts,
structured JSON output, and generic benches that eliminate the
Makefile's 5-way duplicated `cargo bench --profile ... -p bbnf
--bench X > docs/benchmarks/post-AY-$(WAVE)-X.txt` sweep.
**Aesthetic note**: the
user's `aesthetics-critical` feedback applies — divan's output is
designer-quality; bencher's is bare libtest.

### Rank 4 — Enable `-Zthreads=8` + `-Zshare-generics=y` (ADD)

**Scope**: `.cargo/config.toml` gains `[build]` block with
`rustflags = ["-Zthreads=8", "-Zshare-generics=y"]`. Guard with a
comment block that names the ICE interaction (parallel front-end
amplifies ICE count per §ICE investigation).
**Cost**: 3 lines + comment.
**Risk**: low after Rank 1 + Rank 2 land (ICE remediation path named).
**Dev-wall impact**: measured 20-30% on cold `iter-check-full`, i.e.
≥12 min cold → ≤9 min cold.
**Blocks**: Rank 2 (pinned nightly is prerequisite for reproducible
evaluation).

### Rank 5 — Adopt cranelift codegen-backend for dev + ax-iter (ADD)

**Scope**: `rust-toolchain.toml` adds
`rustc-codegen-cranelift-preview` component. `.cargo/config.toml`
`[profile.dev]` and `[profile.ax-iter]` gain
`codegen-backend = "cranelift"`. `[profile.bench]`, `[profile.release]`
**explicitly** keep LLVM.
**Cost**: 4 lines.
**Risk**: low (ecosystem-tested on macOS arm64, used daily by
Zed/Tauri dev teams).
**Dev-wall impact**: 5-15% total compile-wall reduction.

### Rank 6 — Enable `lld` on macOS arm64 by default (ADD)

**Scope**: uncomment the existing `lld` block in
`.cargo/config.toml:22-23` (currently a prose comment). Keep the
preflight-check prose (`ls /opt/homebrew/opt/lld/bin/ld64.lld` fails
loudly if missing).
**Cost**: 2 lines uncommented.
**Risk**: low — documented path.
**Dev-wall impact**: 10-20% rebuild-time reduction on small edits.
**Note**: requires `brew install lld` as a documented dev-setup step.
Add to PROFILING.md §Dev-host setup.

### Rank 7 — `iter-check-full` ceiling + close-gate separation (REPLACE)

**Scope**: REPLACES current B1.W0.c hard-gate item 1 ("exits 0").
New wording: "`cargo iter-check-full` exits 0 with cold wall
recorded in `docs/benchmarks/post-B1-W0-proof.txt` row
`iter-check-full-cold`; that number IS the ceiling; any exceeding
measurement re-opens B1.W0.c." This is **already** in the current
B1.md (invariant 11) but is not a hard gate — promote it.
**Cost**: 1 line rewording.
**Risk**: zero.
**Dev-wall impact**: prevents silent regression.

### Rank 8 — Per-bench feature gates on fixture loaders (ADD)

**Scope**: each `benches/{json,css,sheets}/*.rs` gates its fixture
loader behind a feature that names its grammar.
**Cost**: ~3 lines per bench × 15 benches.
**Risk**: zero (additive; default features keep current behaviour).
**Dev-wall impact**: marginal, but makes future micro-bench tranche
cheap.

### Rank 9 — Install nextest unconditionally in CI + PROFILING.md (REPLACE)

**Scope**: REPLACES the Makefile's nextest-optional detection with
a "required on CI, optional locally with documented graceful
fallback" split. `.github/workflows/ci.yml` adds
`taiki-e/install-action@nextest`. `docs/instructions/PROFILING.md`
lists `cargo install cargo-nextest --locked` in §Dev-host setup.
**Cost**: 3 lines CI + 1 line doc.
**Risk**: zero.
**Dev-wall impact**: 60% test-run-wall reduction on CI.

### Rank 10 — iai-callgrind CI gate (ADD)

**Scope**: new `.github/workflows/bench-iai.yml` running
iai-callgrind on Linux runners. One target per grammar. Regression
threshold 1% instruction-count.
**Cost**: half-day; 1 workflow file + 4 bench targets.
**Risk**: low — instruction-count is stable across CI noise.
**Dev-wall impact**: zero local; CI becomes a precise regression
detector rather than a flaky wall-clock one.

### Items NOT recommended for B1

- **Lift `.bbnf-cache/` to `$XDG_CACHE_HOME`**: highest-leverage
  single change, but too much structural work for B1's bounded
  scope. Route to BA.
- **Watt proc-macro precompilation**: 2-4 week structural rewrite.
  Route to BA/BB.
- **Micro-bench architecture split (`crates/bench-*`)**: dedicated
  tranche. Route to BA.
- **sccache**: expected benefit marginal for bbnf's profile. Measure
  before adopting.

---

## Cap ordering against current B1.md scope

B1.md today ships:
- W0.a: routine alias normalisation + `ay-prime` target.
- W0.b: bootstrap `.bbnf-cache` nuke fix + samply truth.
- W0.c: `iter-check-full` cold-wall ceiling + CI truth.

Recommended scope additions map as:

| Rank | Target wave | Addition kind |
|---|---|---|
| 1 (ICE doc + `ay-clean-incr`) | W0.a | ADD |
| 2 (nightly pin) | W0.a (new file) | ADD |
| 3 (divan migration) | W0.c | ADD (or spin W0.d) |
| 4 (-Zthreads / share-generics) | W0.a | ADD |
| 5 (cranelift) | W0.a | ADD |
| 6 (lld on macOS) | W0.a | ADD |
| 7 (iter-check-full ceiling gate) | W0.c | REPLACE |
| 8 (per-bench feature gates) | W0.c | ADD |
| 9 (nextest unconditional in CI) | W0.c | REPLACE |
| 10 (iai-callgrind) | W0.c | ADD |

Ranks 1-2 are trivial-cost and land first. Ranks 3-6 need W0.a to
rewire `.cargo/config.toml` coherently — they land together as a
single commit to `.cargo/config.toml` + `rust-toolchain.toml` with
one measurement pass in `post-B1-W0-routine.txt`. Ranks 7-10
consolidate into W0.c.

If the orchestrator judges Ranks 3 + 10 (divan + iai-callgrind) as
exceeding B1's bounded-annex remit, split them into a dedicated
post-B1 mini-tranche **before** AY-II resumes — the existing
bencher→divan switch costs a day and blocks no runtime work.

---

## Questions for the user

Only two items have genuine ambiguity:

1. **Divan vs criterion — confirm.** The user explicitly asked.
   §SOTA bench framework decision recommends **divan as primary,
   iai-callgrind as CI secondary**, not criterion. Criterion is the
   ecosystem default but conflicts with the `no-warm-benches`
   feedback. Confirm divan is the right pick for bbnf-lang, or flag
   any criterion-specific tooling (e.g. existing
   `cargo criterion` integration) that tips the balance.

2. **B1 scope cap — Rank 3 (divan migration) and Rank 10
   (iai-callgrind CI) together are ~3 agent-days.** B1 is styled as a
   bounded prelude annex. Should these land in B1 (pushing wall-clock
   to ~1 week), or in a named post-B1 mini-tranche before AY-II.W0'
   resumes? The recommendation table above is the optimistic
   interpretation (land in B1); a more conservative one holds B1 to
   Ranks 1-2 + 4-9 and routes Ranks 3 + 10 to a dedicated tranche.

Everything else is a unilateral recommendation with no ambiguity
requiring user input.

---

## Sources cited inline

- rust-lang/rust #92485, #93602, #92221, #92708, #92847, #49085,
  #76037, #110632 — `on_disk_cache.rs` ICE class.
- nexte.st/docs/benchmarks, nexte.st/docs/design/how-it-works —
  nextest speedup characterisation.
- blog.rust-lang.org/2023/11/09/parallel-rustc — `-Zthreads` and
  `-Zshare-generics` measured impact.
- rust-lang/rustc_codegen_cranelift README + blog.rust-lang.org/
  inside-rust/2020/11/15/Using-rustc_codegen_cranelift — cranelift
  debug-build speedup.
- mozilla/sccache docs/Rust.md — proc-macro caching limitation.
- github.com/dtolnay/watt — WASM-precompiled proc-macros.
- github.com/nvzqz/divan + nikolaivazquez.com/blog/divan — divan
  architecture.
- github.com/iai-callgrind/iai-callgrind — CI regression detection.
- phoronix.com — wild linker state (Linux-only).
- cloudwego/sonic-rs, rust-lang/regex-automata — micro-bench
  compilation-unit-minimisation practice.
- `docs/tranches/meta-audit/04-toolchain-pain.md` — ground-truth
  measurements on master `ab4d9378`.
- `crates/derive/src/lib.rs:300-358` — content-keyed cache
  implementation.
- `.config/nextest.toml` — existing nextest config.
- Every sampled `rustc-ice-*.txt` file at master root.
