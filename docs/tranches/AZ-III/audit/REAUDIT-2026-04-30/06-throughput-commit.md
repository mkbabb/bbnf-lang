# AZ-III REAUDIT Lane 6 — Throughput / Build / Test / Bench / Commit Discipline

**Audit date**: 2026-04-30 (read-only).
**HEAD**: `d5179b8a` (master, 1397 commits ahead of `origin/master`, 0 behind — **the entire rewrite span is local-only**).
**Toolchain pin**: `nightly-2026-04-11` (`rustc 1.96.0-nightly`).
**Author**: Lane 6 sub-agent of the AZ-III continuation reaudit.

This audit cites only files and line ranges read during this session. No
benchmark, samply capture, or cargo invocation was issued. Wall-time
numbers in §1 and §4 are reproduced verbatim from
`/Users/mkbabb/Programming/bbnf-lang/docs/benchmarks/post-B6-W*-walls.txt`
and the AZ-II `FINAL.md` H.6 row; this audit does not re-measure them.

## 1. Iteration loop inventory

| Surface                       | Cargo alias / Make target                        | Cold (claimed)                        | Warm (claimed)                       | Source of truth                                                  |
| ----------------------------- | ------------------------------------------------ | ------------------------------------- | ------------------------------------ | ---------------------------------------------------------------- |
| `cargo iter-check` (routine)  | `.cargo/config.toml:133`                         | ~11 s                                 | 0.13–0.14 s (median run 2–3)         | `docs/instructions/PROFILING.md:78–82`; `post-B6-W1-walls.txt`   |
| `cargo iter-check-full`       | `.cargo/config.toml:137`                         | 17.02 s (post-B6.W0 first cold)       | 0.14 s                               | `post-B6-W1-walls.txt` §"Pre-W1 cold `iter-check-full` baseline" |
| `cargo iter-test --no-run`    | `.cargo/config.toml:141`                         | 129.04 s (first compile)              | 0.66–0.76 s                          | `post-B6-W1-walls.txt` §"Pre-W1 `iter-test --no-run`"            |
| `scripts/test-tier.sh leaf`   | `scripts/test-tier.sh:74–84`                     | 45.89 s for 582 tests                 | (not measured; warm run not stored)  | `docs/instructions/PROFILING.md:97–98`                           |
| `cargo iter-test`             | `.cargo/config.toml:141`                         | (no archived cold wall)               | (no archived warm wall)              | gap — see §6                                                     |
| `cargo iter-clippy`           | `.cargo/config.toml:160`                         | (no archived wall)                    | (no archived wall)                   | gap — see §2                                                     |
| `cargo xtask regen`           | `Makefile:198`; `xtask/src/regen.rs:425`         | 88.26 s (median pre-B6.W0; 98 % cargo build) | < 76 ms post-build IR pipeline | `post-B6-W0-walls.txt` §"Pre-W0 cold-wall decomposition"         |
| `cargo xtask regen --check`   | `Makefile:204`; `xtask/src/regen.rs:444`         | (no archived cold wall)               | n/a                                  | invoked by pre-commit hook + CI (`scripts/hooks/pre-commit:14`) |
| **Bench compile (fat-LTO)**   | `cargo bench-*` aliases (`.cargo/config.toml:164–169`) | **>10 min per harness**               | n/a (LTO defeats incremental)        | `docs/tranches/AZ-II/FINAL.md:149` (row H.6); `cutover.I-PARTIAL.md:69` |
| `make ay-bench-close WAVE=close` | `Makefile:384` (sequential 5-bench sweep) | **>50 min total** (5 × 10 min, fat-LTO; serialized per `bench-single-run`) | n/a | derived from H.6 |
| `cargo bench-iai` (Linux CI)  | `.cargo/config.toml:172`                         | (PR-gated; no local cold-wall)        | n/a                                  | `.github/workflows/bench-iai.yml:46–47`                          |

**Pain points:**

1. **Bench compile is the dominant blocker.** Post-AZ-II close-matrix
   refresh requires five harness compiles (json/css/sheets/bbnf/compile)
   under fat-LTO; AZ-II.O6/O7 cited >10 min per harness as the reason
   the bench-refresh row deferred to AZ-III. The AZ-III hard-gate
   §AZ-III.md:107–108 ("17-entry matrix refreshed in
   `post-AZ-III.json`") cannot be satisfied without W0-level relief.
   The current `bench-single-run` invariant
   (`.cargo/config.toml:18–22`) is correct but the underlying
   `[profile.bench]` settings (`Cargo.toml:60–70`) inherit fat LTO
   plus codegen-units=1 plus debug=true — the fat LTO is the cost.
2. **`cargo iter-test` cold wall absent from the archived ledger.** The
   `--no-run` cold of 129 s is captured; the *full* test wall is not.
   Wave dispatch under W4-W5 currently has no committed cold-wall
   number to compare against.
3. **`scripts/test-tier.sh leaf` cold wall is the smallest archived
   correctness tier (45.89 s for 582 tests).** That is the
   pre-dispatch validation surface today. Two missing pieces: it
   doesn't report a warm wall, and it doesn't accept `--retries=0
   --fail-fast` flags inline (callers pass them through extra args,
   `:64`).
4. **No iter-clippy wall measurement archived.** The `iter-clippy`
   alias (`.cargo/config.toml:160`) carries `-D warnings` over
   `--all-targets`, which forces a full bench-target compile every
   invocation. CI gives it a 10-minute timeout (`ci.yml:42`) but no
   benchmark exists.

**Reading order constraints:** the routine surface is excellent —
`iter-check` is 0.14 s warm and the dev-loop is sound for typecheck.
Test and bench paths are where the bottleneck lives.

## 2. Profile audit

`Cargo.toml:53–129` and `.cargo/config.toml:65–89` define five profiles
with overlapping intent:

| Profile         | Defined in                              | Inherits | LTO       | codegen-units | debug             | strip       | incremental | split-debuginfo |
| --------------- | --------------------------------------- | -------- | --------- | ------------- | ----------------- | ----------- | ----------- | --------------- |
| `dev`           | `Cargo.toml:96–98`                      | n/a      | n/a       | 16            | (default = 2)     | n/a         | true        | n/a             |
| `ax-iter` (root)| `Cargo.toml:125–129`                    | dev      | n/a       | 16            | 0                 | "debuginfo" | (inherits)  | "off"           |
| `ax-iter` (alias)| `.cargo/config.toml:65–70`             | dev      | n/a       | **256**       | "line-tables-only"| (inherits)  | true        | (default)       |
| `release`       | `Cargo.toml:53–58`                      | n/a      | "thin"    | 1             | true              | false       | (default)   | "packed"        |
| `bench`         | `Cargo.toml:60–70`                      | n/a      | **"fat"** | 1             | true              | (default)   | (default)   | (default)       |
| `profiling-prep`| `Cargo.toml:90–94`                      | release  | "thin"    | 1             | true              | false       | (default)   | "packed"        |
| `ay-final`      | `.cargo/config.toml:74–80`              | release  | **"fat"** | 1             | 1                 | false       | (default)   | "packed"        |
| `bench-ci`      | `.cargo/config.toml:85–89`              | release  | "off"     | 1             | "full"            | (default)   | (default)   | (default)       |

**Findings:**

1. **`ax-iter` is defined twice with conflicting settings.** Root
   `Cargo.toml:125–129` says `debug=0`, `split-debuginfo="off"`,
   `strip="debuginfo"`, codegen-units=16 (inherited from dev). The
   alias-side override at `.cargo/config.toml:65–70` says
   `debug="line-tables-only"`, `incremental=true`,
   **`codegen-units=256`**, `opt-level=0`. Cargo merges these, with
   the per-config overlay winning, producing the alias-side values —
   but the redundancy plus the implicit-vs-explicit divergence
   violates the precept memory `feedback_no_orthogonal_codepaths` and
   makes profile drift easy. **Pick one location** (recommend
   `.cargo/config.toml`, since the alias surface is colocated there)
   and delete the other.
2. **`bench` (root) and `ay-final` (alias) both carry fat LTO** and
   are functionally duplicates, plus `bench-iai` (alias, LTO=off) is
   a third bench profile — three profiles for two intents (publish
   numerics vs Linux CI iai-callgrind). This is what produces the
   >10 min per harness wall in §1.
3. **No "bench-iter" profile exists.** Routine bench iteration (e.g.
   "did my emitter change move json::canada by more than noise?")
   has only fat-LTO bench available; `profiling-prep` exists for
   samply but is not wired into the `cargo bench-*` aliases. Today
   a bench iteration costs >10 min per harness; W4 measurement
   serial budget is >50 min minimum for one sweep.
4. **`incremental=true` is set on the alias `ax-iter`** but **NOT on
   root `[profile.dev]`** (`Cargo.toml:96–98` only sets
   `codegen-units=16`). cargo defaults `dev.incremental=true` so the
   net effect is correct — but the explicit asymmetry suggests the
   profiles were written by different agents.
5. **`split-debuginfo` on `ax-iter` is "off" at root and unset at
   alias-level**, so the alias inherits dev's default (which on macOS
   arm64 is "unpacked"). On macOS this means rustc writes individual
   .o.dwarf files into target/, slowing relinking. **Set it
   explicitly to "unpacked" on macOS or "off" everywhere** —
   `feedback_samply_symbols` doesn't apply to ax-iter (samply uses
   `profiling-prep`), so `off` is safe.
6. **No profile sets `RUSTC_BOOTSTRAP=1`** to enable
   `-Zshare-generics` selectively. The §"Build flags" retrospective
   in `.cargo/config.toml:202–221` removed `-Zthreads=8 -Zshare-generics=y`
   on the strength of a 28× warm regression. That measurement was
   against a small-crate set; the symmetric question — does
   `-Zshare-generics` help the cold close-gate `iter-check-full`? —
   is unanswered.
7. **Per-package opt-levels** at `Cargo.toml:100–107` apply only to
   `dev` (and therefore `ax-iter` via inheritance) for `bbnf-ir`,
   `csp-solver`, `parse_that`. **They do not apply to test runs**
   under nextest because nextest uses the cargo-profile chosen by
   `--cargo-profile`. This is intentional but worth noting if
   correctness-test cold walls regress.

**What's missing (proposed):**

- `[profile.ax-iter] debug = "line-tables-only"` (already present at
  alias-level; remove the root duplicate).
- `[profile.bench-iter]` with `inherits = "release"`, `lto = "off"`,
  `codegen-units = 16`, `debug = 1`, for routine
  bench-during-iteration (where +/- 1 % is acceptable noise). This
  costs ~30 s per harness compile vs 10 min, is sufficient for
  "did my change regress hot-path", and lives alongside the
  publish-grade `bench` profile.

## 3. Linker / sccache / target-dir audit

| Item                              | Current                                                                  | Source                                                          | Recommendation                                                                                |
| --------------------------------- | ------------------------------------------------------------------------ | --------------------------------------------------------------- | --------------------------------------------------------------------------------------------- |
| sccache                           | `[build] rustc-wrapper = "sccache"` mandatory                            | `.cargo/config.toml:42`                                         | retain — already correct; `feedback_no_workarounds` rules out fallback                        |
| macOS arm64 linker                | system `ld64` via clang; `lld` opt-in only                               | `.cargo/config.toml:251–257` (commented block)                  | **uncomment after `brew install lld` verification**; ~10–20 % rebuild win (cited §line 251)   |
| Linux x86_64 linker               | rust-lld via `-fuse-ld=lld`                                              | `.cargo/config.toml:272–275`                                    | retain                                                                                        |
| `RUSTC_FORCE_INCREMENTAL`         | unset                                                                    | n/a                                                             | NOT recommended — `incremental=true` already on `dev`/`ax-iter`; force flag is for CI only    |
| `CARGO_TARGET_DIR` per worktree   | hardlink-cloned via `cp -al` per `seed-worktree.sh:54–80`                | `scripts/seed-worktree.sh:69–81`                                | retain — H2 hygiene cut delivers 3–5 min cold reclaim per worktree                            |
| `CARGO_BUILD_JOBS`                | unset (cargo defaults to nproc)                                          | n/a                                                             | leave default; `post-B6-W*-walls.txt` measurements used `CARGO_BUILD_JOBS=4` on M-class       |
| Cranelift backend                 | rust-toolchain.toml component installed but disabled at config level     | `rust-toolchain.toml:44`; `.cargo/config.toml:265–266` (commented) | **enable** for `[profile.ax-iter]` only — `feedback_no_workarounds` says measure first; component is staged ready to cut on |
| `-Zthreads=N`                     | removed per audit-α (`.cargo/config.toml:202–221`)                       | `.cargo/config.toml:204`                                        | retain removal; warm regression of 28× justified it                                           |
| `-Zshare-generics=y`              | removed per audit-α                                                      | `.cargo/config.toml:212–214`                                    | retain removal — same audit                                                                   |

**`mold` / `wild` / `ld.lld` audit:**
- `wild` linker (`davidlattimore/wild`) cited as "Linux-only as of v0.8.0
  (January 2026)" at `.cargo/config.toml:240–241` — accept as current.
- `mold` 2.0+ is commercial on macOS (cited at `:241`) — not viable.
- **`lld` is the fast-linker option on every supported platform.**
  Linux uses bundled rust-lld (`:272–275`); macOS arm64 must have
  `brew install lld` (`:233–235` opt-in). The macOS opt-in remains
  commented because the audit's caveat — "verify path exists on host
  before uncommenting" — is unmet.

**Concrete proposal for AZ-III.W0:** ship a one-liner `make doctor`
target that probes `which sccache && [ -x /opt/homebrew/opt/lld/bin/ld.lld ]`
and prints `cargo: lld available — uncomment .cargo/config.toml:253–257`
when both are true. Also probe `cargo-nextest` and `samply`. The probe
is non-cargo, runs in <1 s, and gates W4 measurement readiness.

## 4. Bench harness audit

| Crate / file                                        | Bench count | Source size (LOC) | Profile      |
| --------------------------------------------------- | ----------: | ----------------: | ------------ |
| `crates/core/Cargo.toml` `[[bench]]` entries        | 19          | 3 342 total        | `ay-final`   |
| `crates/core/benches/json/`                         | 7           | 1 396              | `ay-final`   |
| `crates/core/benches/css/`                          | 6           | 989                | `ay-final`   |
| `crates/core/benches/google_sheets/`                | 2           | 294                | `ay-final`   |
| `crates/core/benches/bbnf/`                         | 1           | 168                | `ay-final`   |
| `crates/core/benches/compile_pipeline.rs`           | 1           | 114                | `ay-final`   |
| `crates/core/benches/json_callgrind.rs`             | 1 (gated)   | 43                 | `bench-ci`   |

**Findings:**

1. **Five harness compiles cost >50 min** for the close-matrix sweep
   (W4 hard gate AZ-III.md:107–108). This is the single largest cost
   anywhere in the iteration loop. AZ-II's H.6 row deferred bench
   refresh exclusively because of this wall.
2. **Feature-gating already present.** `crates/core/Cargo.toml`
   `[features]` block (`stress`, `vm`, `competitor`, `wasm-bench`,
   `callgrind`) keeps the routine bench surface to 5 always-on
   harnesses. Good — already aligned with `bench-single-run`.
3. **`prepare-profile-wave.sh` already supports cached-binary
   reuse.** `.profiles/samply/prebuild/binaries.tsv` and the
   `bench_binary_fresh()` mtime check
   (`scripts/prebuild-benches.sh:93–105`) skip recompile when the
   bench source is newer than the binary; only the changed-source
   bench rebuilds. **This is the model `bench-iter` should follow
   for routine bench iteration too.**
4. **No bench gate on `cold` semantics.** divan provides
   `sample_size` and `skip_ext_time`; the harness sources I sampled
   (e.g. `crates/core/benches/json/monolithic.rs`, 67 LOC) honour
   cold-per-parse via divan's own controls. `feedback_no_warm_benches`
   is enforced at the harness level — good.
5. **Bench profile is fat-LTO + codegen-units=1.** That choice is
   load-bearing for "beat lightningcss in every metric"
   (`feedback_beat_lightning`) — relaxing it for publish-grade close
   matrix is a *non*-starter. The relief lever is splitting the
   profile audience: routine iteration bench → `bench-iter` (lto=off,
   codegen-units=16); close-gate publish → `bench` (fat LTO).
6. **`json_callgrind` is the only Linux-CI-gated harness.** PR-time
   regression check via `bench-iai.yml`. macOS hosts can't run it
   (valgrind is Linux). This is correct as-is.
7. **No bench is marked `harness = false`.** All 19 use divan's
   default libtest-shim harness, which means they each link the
   workspace standard test framework. For `bench-iter` purposes this
   adds ~1 s linking per harness — small but multiplicative on a
   5-harness sweep.

## 5. xtask audit

`xtask/src/main.rs:48–57` (44 LOC) + `xtask/src/regen.rs` (489 LOC).
Single subcommand: `Regen { grammar, check, output }`. Reads the
`[workspace.metadata.bbnf.grammars]` table via `cargo_metadata`
(`regen.rs:186–208`).

**What it does well:**

- **Content-equality skip** at `regen.rs:386–391`. If regen output
  byte-equals the on-disk file, skip the write — preserves mtime,
  avoids cargo rebuild cycle. Cited in the comment block as the
  192× cold-wall speedup from B6 (`B6.W0 close ceremony`).
- **Per-grammar narrow path** (`--grammar <ident>`). Pre-commit
  hook (`scripts/hooks/pre-commit:14`) only runs `--check` when
  staged paths touch `grammar/` or `crates/core/src/grammar/generated/` —
  good gating.
- **Tempdir-based `--check`** (`regen.rs:444–489`). Read-only diff
  against checked-in tree; never writes the working tree on a
  drift-found run.
- **Provenance instrumentation.** `regen.rs:269–339` emits per-stage
  timings on stderr (`compile_paths_request`, `generate_all`,
  `prettyplease`). 76 ms total post-build; the 88 s wall is cargo's
  rebuild of `bbnf` core. This is the visibility that drove the
  B6.W0 "self-invalidation cycle" diagnosis.

**What's missing:**

1. **No `regen --check --incremental` mode.** Pre-commit currently
   regenerates **every** grammar in `[workspace.metadata.bbnf.grammars]`
   (8 entries) even when only one grammar source is touched. The
   `regen_check` function at `regen.rs:444` walks every entry. For
   the touched-grammar fast path: read the staged-paths argument,
   restrict the loop to grammars whose source path overlaps, return
   early with "0 of 0 grammars drifted" otherwise.
2. **No `regen --watch` mode** for grammar iteration. `cargo-watch`
   (`Makefile:228`) exists for `bbnf-lsp` `cargo check`, but grammar
   authors editing `grammar/<ident>/<file>.bbnf` get no auto-regen
   loop. A `make iter-grammar GRAMMAR=...` macro
   (`Makefile:217–221`) is sequential and explicit.
3. **No `regen --diff` mode for human review.** `--check` exits
   non-zero on drift but does not print the diff; CI prints
   "drifted" with the file path. A `regen --diff` would show the
   patch the next regen would apply, useful for "did this rule
   change break grammar X" investigations.
4. **No timing budget gate.** B6.W0 measured 88.26 s median cold;
   there's no automated regression detector on regen wall. Adding a
   `MAX_REGEN_WALL_S` env-var would catch silent regen-cost
   regressions before they accumulate.

## 6. Test partitioning

`.config/nextest.toml` (104 LOC, four profiles) governs the test
runner. Highlights:

- **`[profile.ax-iter] test-threads = 4`** (line 64). Cap at 4
  threads for fast-iter `cargo iter-test*`. Reasonable for M-class
  arm64.
- **No sharding.** `nextest --partition` is not configured anywhere
  — neither in `.config/nextest.toml` nor in `Makefile`. The CI
  workflow (`ci.yml:55`) runs `cargo nextest run --workspace` with
  no partition flag. **Sharding is the natural lever for
  W4-W5 measurement parallelism**, and it's unused.
- **`scripts/test-tier.sh leaf|grammar|workspace`** is the partition
  surface today (`scripts/test-tier.sh:74–110`). It is essentially
  hand-rolled package-level partition. The `grammar` tier walks 10
  per-grammar test binaries serially (`:90–104`), which serializes
  what nextest could parallelise.
- **Slow-test tagging.** `[[profile.default.overrides]]` at
  `.config/nextest.toml:44–46` boosts `compile|pipeline` filters to
  60 s × 3. No other slow tag exists. Tagging slow integration tests
  (`json_parity`, `lightningcss_parity`, `sonic_rs_parity`) with a
  filter expression like `kind(/parity/)` would let `iter-test`
  default-skip them.
- **Integration vs unit separation.** Per
  `feedback_no_inline_tests`, all tests live under `crates/*/tests/`.
  This is enforced at the source level. nextest sees them as
  separate test binaries. CI (`ci.yml:55`) runs all in one pass
  under the `ci` profile.

**Test partitioning gaps for AZ-III.W0:**

- `nextest --partition count:1/N` is not set anywhere.
- No environment variable toggles per-shard test count.
- The leaf tier is the smallest correctness gate (45.89 s / 582
  tests); per `Cargo.toml:2`, the workspace has 12 members. A
  per-package partition matrix for CI would scale linearly with
  runner count.

## 7. Commit discipline forensics

**Sample**: last 500 commits ending at HEAD = `d5179b8a` (gathered via
`git log --max-count=500`; one trailing record off-by-one yields 499
unique hashes).

| Metric                                                      | Count / value | % of 499 |
| ----------------------------------------------------------- | ------------: | -------: |
| Conventional-Commits prefix (`type(scope):`)                | 497           | 99.6 %   |
| Bare-type (no scope)                                        | 14            | 2.8 %    |
| Bodyless                                                    | 34            | 6.8 %    |
| Body-bearing                                                | 465           | 93.2 %   |
| **Templated body** (identical "AZ-III W0 history repair" copy) | **68**     | **13.6 %** |
| Subject ≤ 40 chars                                          | 10            | 2.0 %    |
| Subject 60–100 chars (good band)                            | 337           | 67.5 %   |
| Subject ≥ 100 chars                                         | 36            | 7.2 %    |
| Subject longest                                             | 136 chars     | n/a      |
| Subject median (p50)                                        | 72 chars      | n/a      |
| Single-token-scope (e.g. `b1`, `az-i`, `b6`)                | ~181          | 36.3 %   |
| Slash/comma scope (e.g. `emitter/struct-direct`)            | ~112          | 22.4 %   |

**Specific Claude /commit skill violations
(`/Users/mkbabb/.claude/commands/commit.md:23–46`):**

| Skill rule (file:line)                                                      | Violated by                  | Severity |
| --------------------------------------------------------------------------- | ---------------------------- | -------- |
| `:30` "Make the first line as long as needed to name the **exact** change"  | The 14 bare-type commits (`docs:`, `fix:`) | minor |
| `:32–34` "Scope implementation commits to the **mechanism or owned surface**, not only the tranche" | The 92 single-tranche-scope commits (`docs(b1):`, `fix(b6):`) | major |
| `:35–37` "Include a body when the commit touches multiple subsystems, generated files, deletion sweeps…" | The 68 templated bodies — body exists but does NOT state evidence-per-commit | **major** |
| `:38–39` "Body-bearing commits should state **why the change exists, what landed, what evidence was run or saved**, and what remains routed elsewhere" | All 68 templated bodies share a copy-pasted "evidence: message-only AZ-III W0 history repair" string | **major** |
| `:40–41` "Include bullet points… when befitting"                            | partial — bodies have bullets but they are identical | n/a |
| `:43–44` "be entirely utilitarian and pragmatic"                            | mostly compliant | n/a |
| `:45` "Absolutely NO Claude or AI authorship"                               | 0 violations sampled | clean |

**Diagnosis of templated-body event:** 68 commits in the span
`53d3e6b203...HEAD` were rewritten on 2026-04-30 by `git filter-repo`
(or equivalent) with a single message template per
`docs/tranches/AZ-III/audit/W0-commit-repair-plan.md:1–46`. The plan
acknowledges this at `:38–39` ("The repair deliberately does not claim
that historical commits have newly passed tests") and routes
per-commit evidence through W1–W5. The repair traded **34 bodyless +
"too-terse" commits** for **68 commits with templated body** — a net
quality move (adds *some* context, signals AZ-III ownership, points
forward to W1–W5 evidence) but it leaves the per-commit evidence
specificity unimproved. The §"Remaining Discipline" clause at
`W0-commit-repair-plan.md:42–46` is the binding mandate going
forward — **future commits must carry per-commit specific bodies, not
re-templated copy**.

**Remediation path:** all 68 templated commits are local
(`origin/master..HEAD = 1397`, `HEAD..origin/master = 0`). Per Claude
/commit skill's silent corollary that an unpushed branch is safe to
rewrite, **all 68 are eligible for individualized-body rewrite**.

## 8. Top-30 commits to rewrite

Ordering: most-recent first. **Rewrite is safe for every commit
listed**: HEAD is 1397 ahead of origin and the templated commits are
all in the local-only span. Subjects in the "current" column are
already concrete; the recommendation is to **replace the templated
body with per-commit specific evidence** (what files, why, what was
deleted, what remains).

| #  | Hash       | Current subject                                                           | Recommended treatment                                                                 |
| -: | ---------- | ------------------------------------------------------------------------- | ------------------------------------------------------------------------------------- |
| 1  | `dcb41e67` | refactor(lower/view-walk): rename tape traversal helpers                  | Body: name the four helper renames; cite `crates/core/src/lower/view_walk.rs` lines  |
| 2  | `fb46a734` | chore(bench/cutover): add O5 close target                                 | Body: name the make-target line in `Makefile:389–393` and the artifact stem            |
| 3  | `50b21cd8` | docs(parity/tape): clear stale tape parity comments                       | Body: name the parity test files; cite the residual reference count before/after     |
| 4  | `99413e42` | test(goldens/tape): delete stale tape fixtures                            | Body: list the 20 fixture filenames or summarize by directory                          |
| 5  | `219eb086` | refactor(runtime/compound-record): rename handle offset                   | Body: name the field rename and consumer count                                       |
| 6  | `11fcddf7` | fix(dispatch/alt): wire pure AltDispatch chains                          | Body: cite the dispatch fix and the test that proves it; replace template            |
| 7  | `c3f86944` | fix(grammar/generated): refresh generated grammar outputs                 | Body: list which grammars regenerated; cite `cargo xtask regen` output                |
| 8  | `38a13ef8` | fix(bench/json-competitors): repair competitor benches                    | Body: name the harness fixes and which competitors were affected                      |
| 9  | `ec18aaa6` | fix(projection/materializer): delete retired residue                      | Body: name the retired functions/types deleted                                         |
| 10 | `452aff1a` | test(emitter/struct-direct): harden generated goldens                     | Body: name the struct-direct shapes covered                                           |
| 11 | `6a6ca1fd` | fix(runtime/tape): delete tape crate                                      | Body: cite that 42 files moved/deleted from `crates/tape`; name the carve            |
| 12 | `5e99871d` | refactor(lower/pratt): rename dispatch shim away from tape                | Body: cite the rename map and consumers                                              |
| 13 | `15bd381a` | fix(emitter/wrap-tape): delete wrap tape dispatch                         | Body: name the deleted module and shape                                              |
| 14 | `de522995` | fix(emitter/inline-tape): delete inline tape emitters                     | Body: name the 7 deleted emitter modules                                              |
| 15 | `6effcb0b` | fix(emitter/shape-tape): delete shape tape branches                       | Body: name the deleted shape arms                                                    |
| 16 | `38e099af` | fix(emitter/tape-surgery): delete tape surgery context                    | Body: cite the context-struct deletion                                               |
| 17 | `18104935` | fix(runtime/tape-view): delete tape view surfaces                         | Body: list the 12 deleted view-surface files                                          |
| 18 | `9bdb065e` | fix(dispatch/visitor): delete visitor dispatch helpers                    | Body: name the deleted helpers + downstream consumer purge                            |
| 19 | `4c0e7731` | fix(emitter/inline-visitor): delete inline visitor helpers                | Body: name the 5 deleted helper functions                                             |
| 20 | `bd49fbfb` | fix(emitter/embedded-visitor): delete embedded visitor emitters           | Body: name the 7 deleted emitters                                                    |
| 21 | `bcd0f99a` | fix(emitter/shape-visitor): delete shape visitor modules                  | Body: name the 13 deleted modules                                                    |
| 22 | `72f05435` | fix(ir/visitor): delete visitor recognizer                                | Body: name the recognizer module + last-consumer test                                 |
| 23 | `aa562f4a` | fix(emitter/visitor): delete visitor kernel emitter                       | Body: cite the kernel module path                                                    |
| 24 | `6e7a57c5` | fix(gorgeous/jit): delete retired JIT surface                             | Body: list the 3 deleted JIT modules                                                 |
| 25 | `626091c3` | fix(json-prototype): archive retired prototype                            | Body: cite the archive destination + reason                                          |
| 26 | `06d23788` | fix(bench/json): remove tape path from json value bench                   | Body: name the bench file and the deleted lines                                      |
| 27 | `e905fe59` | test(runtime/tape): delete tape-only tests                                | Body: list the 3 deleted tests                                                       |
| 28 | `d26300fd` | fix(runtime/tape): remove runtime tape shim                               | Body: name the deleted shim + replacement                                             |
| 29 | `cd418c39` | fix(paths/tape): retarget remaining tape paths to crate ownership         | Body: list the path renames                                                          |
| 30 | `dc1999ed` | fix(grammar/generated): retire visitor path                               | Body: list the 12 generated-tree changes                                              |

**Rewrite mechanism:** `git rebase -i 53d3e6b2~1`, mark each commit
`reword`, replace the templated body with per-commit evidence. Per
the brief's instruction to keep hooks intact, the regen pre-commit
hook (`scripts/hooks/pre-commit:14`) will fire on any commit that
touches `grammar/` or `crates/core/src/grammar/generated/` — that's
several of the 30 above (`c3f86944`, `dc1999ed`, `8aa4c5df`,
`07d19d2f`, `e7306d6d`). Rebase plan: stage `cargo xtask regen` once
before starting; each touched-generated commit's tree is unchanged so
the hook will run `--check` against the same tree, see no drift, and
pass.

## 9. Throughput proposals for AZ-III

Eleven concrete proposals, ordered by reclaim per agent-hour. All
respect `feedback_build_infra_first` — proposals 1–5 land in W0.

### P1. Add `[profile.bench-iter]` for routine bench iteration. **W0**

```toml
# Cargo.toml — append after [profile.bench]
[profile.bench-iter]
inherits = "release"
lto = "off"
codegen-units = 16
debug = 1
```

```toml
# .cargo/config.toml — append in [alias] section
bench-iter            = "bench --profile bench-iter -p bbnf"
bench-iter-json       = "bench --profile bench-iter -p bbnf --bench json_monolithic"
bench-iter-css        = "bench --profile bench-iter -p bbnf --bench css_l4"
bench-iter-sheets     = "bench --profile bench-iter -p bbnf --bench google_sheets_monolithic"
bench-iter-bbnf       = "bench --profile bench-iter -p bbnf --bench bbnf_monolithic"
bench-iter-compile    = "bench --profile bench-iter -p bbnf --bench compile_pipeline"
```

**Reclaim:** ~10 min → ~30 s per harness compile (~20× per harness;
~50 min → ~2.5 min for the 5-harness sweep). Numbers are correctness
within ~1 % bench noise band; close-matrix publish-grade still uses
`bench` profile under `make ay-bench-close WAVE=close`.

**Hard-gate adjustment:** `feedback_no_warm_benches` and
`feedback_beat_lightning` apply only to publish-grade close-gate
runs. `bench-iter` is iteration-only; close-matrix retains fat-LTO.
The two profiles are explicitly separated so neither bleeds into
the other.

### P2. Delete the `[profile.ax-iter]` redefinition at root. **W0**

`Cargo.toml:125–129` and `.cargo/config.toml:65–70` both define
`ax-iter`. Drop the root copy and rely on the alias-side `inherits =
"dev"` chain. Reclaim is *clarity*, not wall — but eliminating
profile drift before W4 measurement is hygienic.

### P3. Add `make doctor` host-readiness probe. **W0**

```make
doctor:
	@if ! command -v sccache >/dev/null; then echo 'missing: sccache (brew install sccache)' >&2; exit 1; fi
	@if ! command -v cargo-nextest >/dev/null; then echo 'missing: cargo-nextest (cargo install cargo-nextest --locked)' >&2; exit 1; fi
	@if ! command -v samply >/dev/null; then echo 'missing: samply (cargo install samply)' >&2; exit 1; fi
	@if [ -x /opt/homebrew/opt/lld/bin/ld.lld ]; then \
	  echo 'lld available — see .cargo/config.toml:253–257 to opt in'; \
	fi
	@echo 'doctor: green'
```

Probes the dev-host before W4 measurement; <1 s wall.

### P4. `nextest --partition` for CI test sharding. **W0** (CI-side)

```yaml
# .github/workflows/ci.yml — replace the `heavy — workspace tests` step
- name: heavy — workspace tests (nextest, ci profile, partition 1/3)
  run: cargo nextest run --workspace --profile ci --partition count:1/3
- name: heavy — workspace tests (nextest, ci profile, partition 2/3)
  run: cargo nextest run --workspace --profile ci --partition count:2/3
- name: heavy — workspace tests (nextest, ci profile, partition 3/3)
  run: cargo nextest run --workspace --profile ci --partition count:3/3
```

Reclaim: **CI wall ÷ 3** (parallel via matrix-job) for the heavy
tier. Local `iter-test` retains single-host execution; partition is
CI-only.

### P5. `xtask regen --check --staged` incremental mode. **W0**

```rust
// xtask/src/regen.rs — add CLI flag in main.rs, then in regen.rs:
//
// fn regen_check(workspace_root: &Path, grammars: &[GrammarEntry], staged_only: bool) -> Result<()> {
//   let touched = if staged_only { staged_grammar_idents(workspace_root)? } else { None };
//   for entry in grammars {
//     if let Some(ref t) = touched { if !t.contains(&entry.ident) { continue; } }
//     ...
//   }
// }
```

Pre-commit hook (`scripts/hooks/pre-commit:14`) gains
`--staged` automatically — only regenerates the grammars whose
sources are in the staged set. Reclaim: 88.26 s → ~30 s for a
single-grammar edit (skips 7 of 8 grammars).

### P6. Tag slow integration tests + filter from `iter-test`. **W4-prep**

```toml
# .config/nextest.toml — add to [profile.ax-iter]
default-filter = "kind(test) - (test(/parity/) | test(/lightningcss/) | test(/sonic_rs/))"
```

Reclaim: per-iteration `iter-test` skips ~3 expensive parity binaries
unless explicitly invoked. Routine workflow: ~1 s nextest discovery
on warm cache.

### P7. Enable Cranelift backend on `ax-iter`. **W0** (gated on measurement)

```toml
# .cargo/config.toml — uncomment :265–266
[profile.ax-iter]
codegen-backend = "cranelift"
```

Component is already installed via `rust-toolchain.toml:44`. PROFILING.md
:31 cites "~5–15 % wall reduction on dev builds". Gate: measure
before/after `iter-check-full` cold and `iter-test --no-run` cold;
keep if both improve, revert otherwise (`feedback_actual_profiling`).

### P8. Enable lld linker on macOS arm64. **W0** (gated on host probe)

`.cargo/config.toml:251–257` — uncomment after `make doctor` returns
green for `/opt/homebrew/opt/lld/bin/ld.lld`. Reclaim: 10–20 % on
`iter-check-full` cold relink wall (~17 s → ~14 s post-B6).

### P9. `make iter-bench GRAMMAR=<ident>` mirroring iter-grammar. **W4-prep**

```make
GRAMMAR ?=
iter-bench:
	@if [ -z "$(GRAMMAR)" ]; then echo "usage: make iter-bench GRAMMAR=<ident>" >&2; exit 2; fi
	cargo bench-iter-$(GRAMMAR)
```

Mirrors the existing `make iter-grammar GRAMMAR=...` pattern (`Makefile:217–221`).

### P10. xtask regen wall regression detector. **W0**

```rust
// xtask/src/regen.rs — wrap the per-grammar timing block
if let Ok(budget_s) = std::env::var("XTASK_REGEN_MAX_S").map(|s| s.parse::<f64>()) {
    let elapsed_s = t0.elapsed().as_secs_f64();
    if elapsed_s > budget_s? {
        bail!("regen for `{}` took {:.2} s (budget {:.2} s)", entry.ident, elapsed_s, budget_s?);
    }
}
```

CI sets `XTASK_REGEN_MAX_S=120`; local invocations don't set the env
var (no behaviour change). Catches a B6-style self-invalidation
regression before it lands.

### P11. Per-tranche `cargo nextest --status-level=fail --hide-progress-bar` for sub-agent reports. **W0**

The `feedback_test_output_to_file` precept mandates redirecting long
runs to a file once. Sub-agents currently re-invoke nextest per
filter; `nextest --message-format=libtest-json` would let an agent
parse one capture rather than re-invoke. **No config change required**
— it's an invocation-level discipline for the dispatch packets.

## 10. AZ-III W0 wiring

**AZ-III.W0 already exists** as the "Quarantine and Dispatch Repair"
wave (`docs/tranches/AZ-III/waves/W0.md`, 129 LOC). Its scope (lines
9–22) is currently doc-only: state ledger, commit repair plan,
precepts migration, dispatch-packet authoring. The file bounds at
:24–37 explicitly **forbid touching source code**. The hard gate at
:81–94 is doc-reconciliation and dispatch-packet sufficiency.

**Per project memory `feedback_build_infra_first`: AZ-III hits the
condition (iteration is the bottleneck — bench compile >10 min, W4
hard-gate is unreachable without relief).** W0 must therefore
absorb infrastructure improvements **before** W1–W5 dispatch.

### Proposed W0 scope expansion

Rename W0 to **"W0 — Quarantine, Dispatch Repair, and Throughput
Substrate"**. Add a fifth scope item to `W0.md:9–22`:

> 7. Land throughput-substrate improvements that block W4 measurement
>    feasibility — the `bench-iter` profile, the `xtask regen
>    --staged` mode, the `make doctor` host-probe, the
>    profile-redefinition cleanup, and (gated on host-probe + measured
>    wall delta) cranelift + lld activation.

Add file bounds at `W0.md:24–37`:

| File                                             | Access      |
| ------------------------------------------------ | ----------- |
| `Cargo.toml` (`[profile.*]` section only)        | modify-carve |
| `.cargo/config.toml` (`[profile.*]` and `[alias]`) | modify-carve |
| `Makefile` (top-level new targets only)          | modify-carve |
| `xtask/src/main.rs` + `xtask/src/regen.rs`       | modify-carve (new flag, behaviour-preserving when flag absent) |
| `.config/nextest.toml`                           | modify-carve (per-profile only, no global) |
| `.github/workflows/ci.yml`                       | modify-carve (sharding only) |

Add to W0 Hard Gate (`W0.md:82–94`):

> 6. `make doctor` returns green on the dispatch host.
> 7. `cargo bench-iter-json --no-run` completes in <60 s on warm cache.
> 8. `cargo xtask regen --check --staged` returns 0 on a no-grammar-touched stage; <1 s wall.
> 9. The 30-commit rewrite plan in
>    `audit/REAUDIT-2026-04-30/06-throughput-commit.md` §8 has been
>    enacted via `git rebase -i` and is verifiable by `git log
>    --grep="message-only AZ-III W0 history repair" | wc -l == 0`.

### Proposed new wave: AZ-III.W0p — Throughput Substrate

If the orchestrator prefers a clean separation (W0 stays doc-only,
infrastructure splits to its own wave), open `W0p.md` named
**"W0p — Throughput Substrate (Build/Test/Bench/Regen)"** with the
following structure:

```markdown
# AZ-III.W0p — Throughput Substrate

**Name**: W0p — Throughput Substrate
**Opens after**: AZ-III.W0 close.
**Agents**: up to 5 parallel (one per substrate change; minimal merge surface).
**Hard gate**: every substrate change is measured cold-vs-cold; reverts on regression.
**Status**: planned

## Scope (matches §9 P1–P10 above)

1. `[profile.bench-iter]` — routine bench iteration profile.
2. Profile-redefinition cleanup — single ax-iter source of truth.
3. `make doctor` host-readiness probe.
4. `cargo nextest --partition` CI sharding.
5. `cargo xtask regen --check --staged` incremental gate.
6. Slow-test tag filtering in `[profile.ax-iter]`.
7. Cranelift backend activation (gated on measured win).
8. lld linker activation (gated on host probe).
9. `make iter-bench GRAMMAR=<ident>` parity with iter-grammar.
10. xtask regen wall regression detector.

## File Bounds (mirror W0 file bounds + add evidence under docs/benchmarks/AZ-III/W0p-*.txt)

## Verification Artefacts

- docs/benchmarks/AZ-III/W0p-walls.txt
  (cold-iter-check-full, cold-bench-iter-json compile, cold-bench-json compile, regen --staged wall)

## Dependencies

- **Depends on**: AZ-III.W0 close.
- **Blocks**: AZ-III.W4 measurement gate (bench-refresh feasibility).
```

Rationale for the split: W0 is a doc-and-orchestration wave (no
source touched per :37); W0p is exclusively build-infra. Mixing them
violates W0's existing scope-statement at :37. The dependency chain
becomes W0 → W0p → W1/W2/W3 → W4 → W5, which respects the project
memory rule that infrastructure improvements LAND FIRST.

## Verification

This audit cited:

- `/Users/mkbabb/Programming/bbnf-lang/Makefile` (full).
- `/Users/mkbabb/Programming/bbnf-lang/Cargo.toml` lines 1–129.
- `/Users/mkbabb/Programming/bbnf-lang/.cargo/config.toml` lines 1–276.
- `/Users/mkbabb/Programming/bbnf-lang/rust-toolchain.toml` lines 1–46.
- `/Users/mkbabb/Programming/bbnf-lang/.config/nextest.toml` lines 1–104.
- `/Users/mkbabb/Programming/bbnf-lang/xtask/src/main.rs` lines 1–57.
- `/Users/mkbabb/Programming/bbnf-lang/xtask/src/regen.rs` lines 1–489.
- `/Users/mkbabb/Programming/bbnf-lang/scripts/test-tier.sh` lines 1–125.
- `/Users/mkbabb/Programming/bbnf-lang/scripts/prebuild-benches.sh` lines 1–148.
- `/Users/mkbabb/Programming/bbnf-lang/scripts/prepare-profile-wave.sh` lines 1–199.
- `/Users/mkbabb/Programming/bbnf-lang/scripts/profile-bench-headless.sh` lines 1–237.
- `/Users/mkbabb/Programming/bbnf-lang/scripts/seed-worktree.sh` lines 1–84.
- `/Users/mkbabb/Programming/bbnf-lang/scripts/hooks/pre-commit` lines 1–19.
- `/Users/mkbabb/Programming/bbnf-lang/.github/workflows/ci.yml` lines 1–82.
- `/Users/mkbabb/Programming/bbnf-lang/.github/workflows/bench-iai.yml` lines 1–92.
- `/Users/mkbabb/Programming/bbnf-lang/docs/instructions/PROFILING.md` lines 1–409.
- `/Users/mkbabb/Programming/bbnf-lang/docs/instructions/CHANGELOG.md` lines 1–192.
- `/Users/mkbabb/Programming/bbnf-lang/docs/instructions/README.md` lines 1–30.
- `/Users/mkbabb/Programming/bbnf-lang/docs/benchmarks/post-B6-W0-walls.txt` (excerpts).
- `/Users/mkbabb/Programming/bbnf-lang/docs/benchmarks/post-B6-W1-walls.txt` (excerpts).
- `/Users/mkbabb/Programming/bbnf-lang/docs/benchmarks/post-AZ-II.json` (excerpts).
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AZ-II/FINAL.md:149` (H.6 row).
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AZ-III/AZ-III.md` lines 1–118.
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AZ-III/waves/W0.md` lines 1–129.
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AZ-III/waves/W4.md` lines 1–124.
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AZ-III/audit/W0-commit-repair-plan.md` lines 1–46.
- `/Users/mkbabb/.claude/commands/commit.md` lines 1–46.
- 500-commit window via `git log --max-count=500` ending HEAD `d5179b8a`.

No cargo command, samply capture, or write outside this audit file
was issued.
