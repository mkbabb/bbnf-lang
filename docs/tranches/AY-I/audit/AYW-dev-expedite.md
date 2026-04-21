# AY audit — dev-cycle expedite

Audit of build / test / bench / regen / profile loops at master
HEAD `a91633e3`. Read-only; no code runs beyond artefact inspection.
All wall-times cite saved artefacts.

## § 1. Current dev-cycle latencies

| Phase | Wall | Source |
| --- | ---: | --- |
| Cold `cargo build --workspace --profile ax-iter` | **67.21 s** | `docs/tranches/AX/audit/next-tranche/A4-compile-time-fresh.md` §2 (HEAD `9074a685`) |
| Cold `cargo test --workspace --profile ax-iter` | **≈ 7-9 min** total (compile ≈ 5 min + run ≈ 2 min) | `/tmp/ay-w0-final.txt` (AY.W0 full workspace — 1491 pass / 40 ignore) |
| `cargo bench` total wall (4 grammar families) | **113 s** | `docs/benchmarks/post-AY-W4-close.json` `wall_time_seconds` |
| — json_monolithic | 57 s | same |
| — css_l4 | 31 s | same |
| — google_sheets_monolithic | 14 s | same |
| — bbnf_monolithic | 11 s | same |
| Bootstrap regen (single pass) | **≈ 30-60 s** | `cargo expand -p bbnf-bootstrap --lib` wall on a warm cache; CI idempotency doubles this |
| `prepare-profile-wave.sh` full prepare | **≈ 3-5 min** (5 bench bins × cold build + cargo expand per bench) | `docs/instructions/PROFILING.md` §Prepare artefacts; script itself is not in `scripts/` — it is assumed, not shipped |
| Incremental rebuild (single test edit) | target **≤ 10 s** (unmet — no measurement artefact); aggregate binaries historically 11-14 s at `CARGO_BUILD_JOBS=4` | W6 hard-gate + `docs/tranches/AX/audit/R3-infra-speedups.md` §T6 |
| Per-grammar test binary cold compile | 0.39 s (ebnf) … **1.81 s** (css_l4) | A4 §1 table |
| Workspace `.bbnf-cache` | **15 MB / 241 kloc** (css_l4 = 85.8 %) | A4 §1 |
| `target/` directory | ≈ 13 GB | R3-infra-speedups §T1 |

**Total single-agent dev loop (cold)**: build 67 s + test 7-9 min + bench 113 s = **≈ 12-15 minutes** before an edit-bench-profile iteration can close. Incremental loops (warm deps) hover around 3-5 minutes limited by the 113 s bench + cache clears.

Per-grammar cache dominance verified: `css_l4 = 13.0 MB / 196 kloc` is 16.6× the next grammar (sheets 781 KB / 14 kloc). Every regen / `.bbnf-cache` clear pays the css_l4 proc-macro expansion cost.

## § 2. Friction points per phase

### Build

1. **`parse_that` monomorphisation storm** — 107.38 s combined rustc time (49 % of top-5 workspace units). Single dominant blocker. Path-dep at `../parse-that/rust/parse_that` (21,782 LOC, heavy `Parser<'a, Output>` + `ParserFn` trait tower). `opt-level=1` already applied via `Cargo.toml:35`; next lever is type-level. A4 §L-C, W6.1 scope.
2. **CSS L4 macro_expand_crate** — 0.74 s + 295 MB RSS dedicated to `#[derive(Parser)]` expansion of the 13 MB TokenStream (197 kloc). Dominates the css_l4 test-binary rustc total (1.57 s / 648 MB). A4 §3.
3. **`.bbnf-cache` invalidation under contact** — any `BBNF_SCHEMA_VERSION` bump (v15 at `crates/derive/src/lib.rs:81`) invalidates the entire workspace cache. AY has bumped v13 → v14 (W0d) → v15 (W4.3) — two full-workspace re-expansions inside one tranche. The orchestrator's `find . -name .bbnf-cache -exec rm -rf {} +` is mandatory before any bench / regen / proc-macro test (README §Cache clearing).

### Test

1. **Aggregate test binaries hit LLVM codegen super-linearity** — README §Memory discipline documents a 26 GB RSS OOM on a 5-derive-site `tape_parity` aggregate under release profile. Mitigation: per-grammar split + `CARGO_BUILD_JOBS=4`. `serialize_roundtrip.rs` still carries **9 `#[parser(...)]` sites** in a single 294 LOC binary (R3 §T3) — tallest surviving aggregate.
2. **Flaky threshold-asserting perf test** — `tape::tests::packed_cache::packed_cache_read_beats_soa_materialise` asserts a 1.3× perf ratio near the system noise floor; passes ~3/5 runs. Known pre-existing (AY PROGRESS §W1 handoff).
3. **nightly `bbnf-analysis` ICEs** — recurring issue; `cargo clean -p bbnf-analysis` is documented workaround (README §Cache clearing). Adds a failure-diagnosis detour to the test loop every few runs.

### Bench

1. **Mandatory sequential execution** — the file-first + `mimalloc` global allocator contract requires one bench invocation at a time (`README.md:382-396`, feedback `bench-sequential-regression`). Parallel is disallowed; 113 s is the irreducible wall.
2. **Cold-only contract** — feedback `no-warm-benches` forbids warm caches; every bench pays full load time. Reasonable for truth, but costly for iteration.
3. **Full-matrix-every-time** — 19 parse entries × 4 families per wave close; no per-fixture subset convention when iterating a single lever.

### Regen

1. **Idempotency-check doubles cost** — orchestrator convention is regen-twice-and-diff (`scripts/check-bootstrap-clean.sh`). Second pass is a full proc-macro re-expansion against the just-emitted `generated.rs`; byte-identical in 99 % of cases but always paid (~30-60 s extra).
2. **Self-hosting circular-dependency escape** — README §Self-host circular-dependency escape documents a multi-step recipe requiring a pre-rewrite `generated.rs` checkout when the parser rewrite breaks `bbnf.bbnf` parsing. Rare but expensive when it fires (AW-I.W4ζ template).
3. **Bootstrap expands bbnf-bootstrap from scratch every invocation** (R3 §T2); no delta-regen. `cargo expand -p bbnf-bootstrap --lib` is the floor.

### Profile

1. **`scripts/prepare-profile-wave.sh` is referenced everywhere but missing from `scripts/`** — PROFILING.md §Prepare a wave documents it as canonical; current worktree has no such file. Either deleted or externalised; friction point for any new profiling wave.
2. **Per-entry samply artefact ceremony** — every `(bench, entry)` pair needs seven artefacts (`bench.txt`, `build.txt`, `record.txt`, `load.txt`, `profile.json.gz`, `profile.json.syms.json`, `syms-proof.txt`). PROFILING.md §Profile a single entry. Correct, but not skippable for incremental profile runs.
3. **Port reservation + worktree stale deps** — AY orchestrator lesson: stale `target/deps/*` symbols reference removed worktree paths when a sub-agent's worktree is pruned while its binary is still installed at the shared target.

### Orchestrator (worktree + cache)

1. **Orphan worktree accumulation** — 37 orphans pruned at AY.W0 close (PROGRESS §W0 close). `scripts/worktree-status.sh` + `scripts/kill-all-rust.sh` land but require manual invocation. No session-start auto-sweep.
2. **`target/` symlink correctness** — `seed-worktree.sh` owns this by default; still a manual `--no-target` trap when an agent investigates rebuild-cache bugs.
3. **`.profiles/` stale files** — 2,098 Apr 15-17 files pruned at AY.W0; no retention policy codified.

## § 3. Expedite opportunities

Per phase, ROI-ordered.

### Build

- **Apply W6.1 `parse_that` de-generic (A4 §L-C)** — target ≤ 70 s combined rustc; **est. –35 s off workspace cold wall-clock (–52 %)**. Largest single lever. Profile-driven via `cargo llvm-lines -p parse_that --profile ax-iter`.
- **`ax-iter codegen-units=256` + verified `incremental=true` (W6.2)** — config-only. Current `[profile.ax-iter]` inherits `dev` (cgu=16); raising to 256 maximises LLVM parallelism. **est. –7 s (–10 %) workspace cold; –2-3 s iterative.**
- **CSS L4 `@import`-split emission (W5.1 / A4 §L-A)** — `mod __part_N { … }` per `@import` file; shards typeck + MIR borrow-check across CGUs. **est. –0.5-0.7 s css_l4 wall; bigger under release cgu=1 where aggregate binaries hit 26 GB RSS.**
- **Hoist DFA + keyword + byte-class tables to `pub(crate) const`** (W5.2 / A4 §L-B) — relocates 20-30 % of css_l4 cache lines out of fn bodies to const-items (no borrow-check, no MIR). **est. –0.3-0.5 s css_l4; proportional on sheets/bbnf.**
- **Split `serialize_roundtrip.rs` aggregate** (9 derive sites → 9 binaries, R3 §T3) — mirrors tape_parity precedent. **est. –5-10 min on any run that touches it cold; eliminates the last high-risk super-linearity site.**
- **Install `lld` / `mold`** (currently commented in `.cargo/config.toml`) — **est. 30-50 % link-time reduction, 10-20 % total rebuild on small edits** per R3 §T1.

### Test

- **Formalise `scripts/test-tier.sh <leaf|grammar|workspace>`** as the canonical test command in orchestrator + agent briefings. Already landed (R3 §T4); under-used. **est. –5-10 min per emitter-iteration cycle** (skips workspace for leaf work).
- **Per-grammar test invocation (`cargo test --test <name>`)** on single-file edits — reduces compile to one derive-Parser site (11-14 s at `CARGO_BUILD_JOBS=4`). Documented in README §Testing; promote to standard practice.
- **Reclassify `packed_cache_read_beats_soa_materialise` as `#[ignore]` + bench** — move the threshold assertion out of `cargo test --workspace` into a dedicated criterion/bencher entry. Removes the 2/5 flaky fail rate.
- **`cargo clean -p bbnf-analysis` as first-class `test-tier.sh` fallback** — when the nightly ICE fires, the ladder should rerun the carved crate before escalating.

### Bench

- **Per-fixture subset runner** — `scripts/bench-subset.sh <family> <entry>` to run one entry (bencher filter is a substring match, documented in PROFILING.md). **est. –80-90 % of 113 s** when iterating a single lever. Cold-only contract preserved (one fixture, cold).
- **Pre-computed baseline cache with delta-only reporting** — `scripts/bench_regression.sh` already does this for 5 pinned entries against `data/bench_baseline.json` with a 5 % threshold; extend to all 19 entries and wire as the wave-close gate rather than re-running the full matrix narrative.
- **Sequential constraint: diagnose whether it is thermal, mimalloc-global, or measurement-noise.** Per feedback `bench-sequential-regression`, current discipline is sequential. If the root cause is mimalloc's global state (one allocator per process), bench-per-process parallel is *safe* across separate OS processes. If thermal/noise, parallel is still disallowed. Diagnosis worth ~60 s per bench wave if the parallel path opens.

### Regen

- **Skip the idempotency second pass when `scripts/check-bootstrap-clean.sh` exits clean** — the CI script already does copy-then-regen-then-diff in a single pass. Promote its workflow to the orchestrator's standard: one regen, diff against committed, done. **est. –30-60 s per regen** (R3 §T2 documents this as already-landed-as-discipline).
- **Surgical `BBNF_SCHEMA_VERSION` invalidation** — current hash input mixes the version const with every grammar file + every attribute. A version bump invalidates *everything*. Splitting the cache key into a (version, per-grammar-hash) pair would let a schema bump coexist with an unchanged grammar cache entry — *no cache loss on schema bumps that don't touch the grammar's emission surface*. Substantial rework; defer to BB.

### Profile

- **Ship `scripts/prepare-profile-wave.sh`** — referenced but missing. Materialise the canonical script so PROFILING.md's contract is not a paper tiger. **est. –15-20 min per profile wave** (eliminates agent re-derivation).
- **`sub-agent profile dispatch template` with `wave.tsv` as the sole contract** — PROFILING.md §Orchestration contract describes this; needs a matching template file in `docs/instructions/tranche/` so agents can copy-paste.
- **Content-hash keyed `cargo expand` cache** (R3 §T5) — per-wave cache re-expands unconditionally; keying by `generated.rs` hash + bench source hash enables wave-to-wave reuse. Deferred in R3 to AX-W13. **est. –1-2 min per profile wave.**

### Orchestrator

- **Session-start auto-sweep** — invoke `scripts/worktree-status.sh --dirty` + `scripts/kill-all-rust.sh --dry-run` at every orchestrator resume. Both scripts exist; discipline is ad-hoc.
- **`seed-worktree.sh --target` as default + drift-guard** — already default; extend to re-point stale symlinks rather than warn-and-skip when `readlink != ROOT/target`. Eliminates the "stale target/deps referencing removed worktree" AY lesson.
- **Per-wave shared target directory split** — current discipline is one absolute `CARGO_TARGET_DIR` per profile wave. For parallel waves (2 + in flight), separate target dirs avoid cross-wave cache contamination. Already documented in PROFILING.md §Shared-target discipline; needs orchestrator workflow.

## § 4. Canonical iterate-fast workflow

90-second target cycle: **code edit → test pass → bench delta → samply attribution**.

| Step | Command | Wall |
|------|---------|------|
| 1. Edit source | (human) | 0 s |
| 2. Compile-gate single crate | `cargo check -p bbnf --profile ax-iter` | **3-8 s** (warm deps, cgu=16/256, no debuginfo) |
| 3. Targeted test | `scripts/test-tier.sh leaf` OR `cargo test -p bbnf --test <single-binary> --profile ax-iter` | **10-20 s** cold derive-Parser site; <15 s warm |
| 4. Single-fixture bench | `cargo bench -p bbnf --bench json_monolithic -- twitter` (one substring filter) | **8-12 s** (one fixture × cold-only contract; bencher 0.1 default iteration model) |
| 5. Samply spot-check | `scripts/profile-bench-headless.sh --bench json_monolithic --entry twitter ...` | **20-40 s** (record + symbolicate + load; prebuilt binary assumed) |

**Total warm: ≈ 45-90 s per iteration.** Cold-start (fresh cache, ax-iter rebuild) adds ~30 s on the first cycle; subsequent edits stay in the warm envelope.

Non-negotiable friction that stays in the 45-90 s budget:
- `.bbnf-cache` clear costs ≈ 0.1 s; the re-expansion cost happens at step 2's first compile-gate after a cache clear.
- `mimalloc` global allocator means a process restart per bench entry (cold-only) — bencher handles this correctly.
- Samply port reservation preflights must run before step 5.

**Dependencies to make this real**:
- Per-grammar test binaries established (done for tape_parity; `serialize_roundtrip` pending, R3 §T3).
- `ax-iter codegen-units = 256` applied (W6.2, pending).
- `parse_that` de-generic landed (W6.1, pending).
- `scripts/bench-subset.sh` shipped.
- `scripts/prepare-profile-wave.sh` shipped.

Without those five, the realistic cycle is **3-5 minutes**, bounded by the 113 s full bench matrix + full workspace `cargo test` re-link.

## § 5. Bench runner rewrite candidacy

- **Current harness**: `bencher = "0.1"` (cargo `dev-dependency`, `crates/core/Cargo.toml:43`). Custom-harness per bench file (`harness = false` on every entry). Substring-filter via `--bench <substring>`.
- **Wrap**: `crates/core/benches/common/timeout.rs` installs `bench_with_timeout(b, limit, f)` — per-iteration wall-clock guard (CSP node-budget composes for freezes). `black_box` applied to inputs + outputs. `mimalloc` as `#[global_allocator]` in every bench binary.
- **Overhead per entry**: `bencher 0.1` uses a simple iteration model (no statistical confidence intervals, no warm-up runs shown, no criterion-style outlier detection). Cold-only contract is honoured because the harness panics on freeze rather than warming a cache.
- **Alternatives considered**:
  - **criterion** — adds warm-up (disallowed per feedback `no-warm-benches`) + statistical reports. Overhead ≈ 2-3× wall. Disqualified.
  - **divan** — newer, lighter than criterion; similar warm-up + report footprint. Same objection.
  - **tinybench** — banned per memory `vitest-bench` (applies to `bbnf-buddy` JS side; Rust `bbnf-lang` was never tinybench). No impact.
  - **Hand-rolled harness** — `std::time::Instant` + N iterations + panic-on-freeze. Would strip bencher overhead entirely. Code-cost: ~80 LOC per bench binary × 19 benches = 1.5 kloc. Not worth the ≈ 5-10 % wall reduction.

**Verdict**: `bencher 0.1` is well-chosen. No rewrite. Invest expedite effort in:
- **Per-fixture subset runner** (§3 Bench) — bigger win than a harness swap.
- **Baseline cache + delta reporting** (`scripts/bench_regression.sh` extended to all 19 entries) — makes each wave-close a 5-line diff rather than a 113 s rerun-and-narrate.

## § 6. Recommendations (top 5)

Ordered by impact × simplicity:

1. **Land W6.1 + W6.2 `parse_that` de-generic + `ax-iter codegen-units=256`**. Single largest lever: **–37-42 s off workspace cold build (55 % reduction)**; unlocks the 10 s incremental target. One profile-driven edit on path-dep + one Cargo.toml block.
2. **Ship `scripts/bench-subset.sh <family> <entry>` + extend `scripts/bench_regression.sh` to 19 entries**. Eliminates the 113 s full-matrix rerun when iterating a single lever. **–80-90 s per bench iteration; –60-90 s per wave close.** Config + a small Python extension; no architectural change.
3. **Ship `scripts/prepare-profile-wave.sh`** (materialise the referenced-but-missing canonical script). **–15-20 min per profile wave**; unblocks sub-agent profile dispatch without re-derivation. Prerequisite for the PROFILING.md contract to be operational.
4. **Split `crates/core/tests/serialize_roundtrip.rs`** into 9 per-grammar binaries per R3 §T3. Removes the last high-risk aggregate site (9 derives × 294 LOC) before it hits the 26 GB RSS OOM that `tape_parity` already did. **–2-5 min on every cold run touching it**; pre-empts a future-tranche emergency split.
5. **Adopt `scripts/test-tier.sh <leaf|grammar|workspace>` + single-binary `cargo test --test <name>` as canonical iteration commands** in every agent briefing. The ladder exists (R3 §T4), is under-used. **–5-10 min per emitter-iteration cycle**; zero implementation cost.

### Secondary (nice-to-have; deferred ROI)

- Reclassify `packed_cache_read_beats_soa_materialise` as `#[ignore]` bench.
- Surgical `BBNF_SCHEMA_VERSION` cache-key split (BB scope).
- `cargo expand` content-hash cache (R3 §T5; deferred).
- `lld` / `mold` installer block in `.cargo/config.toml` (already correct comment; just uncomment after install).
