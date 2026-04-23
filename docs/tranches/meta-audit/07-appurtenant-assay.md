# 07 — Appurtenant Repo Toolchain Assay

**Scope**: 16 repos (2 sibling, 14 workspace-member). Uniform per-repo posture
record followed by a cross-repo synthesis. Written for the fleet modernization
tranche that will abrogate `bencher`/`criterion`, pin the rust toolchain, adopt
`divan` + `nextest`, and harmonize the `.cargo/config.toml` alias surface.

**Context**: The workspace `Cargo.toml` enumerates 14 members at `crates/*`.
Two of those (`gorgeous`, `csp-solver`) are vendored — their upstreams live in
sibling trees at `../gorgeous` and
`../../csc411/CSC411_HW2_ProgrammingQuestion/csp-solver` respectively. Two
further sibling trees (`../parse-that`, `../pprint`) are path-patched in via
workspace `.cargo/config.toml`. That gives 16 distinct rust artefacts in the
appurtenant surface.

**Unit of assay**: per-repo role, git posture, toolchain, `Cargo.toml` surface,
`.cargo` overrides, test setup, CI, scripts, proc-macro footprint, current pain
signal, modernization verdict.

---

## Part A — Per-repo posture

### parse-that (sibling)

**Role in the ecosystem**: Parser-combinator substrate + bespoke regex engine.
Hosts `parse_that`, `bbnf-regex`, and a `regex-bootstrap` crate; every bbnf
grammar site lowers through `parse_that` combinators, and `bbnf-regex` carries
the HIR→NFA→DFA pipeline that `bbnf-ir` consumes for class scanners.

**Git posture**:
- Path: `/Users/mkbabb/Programming/parse-that`
- Separate repo: **Y**
- HEAD SHA: `919d77d18cd867dfdf695f2f10ea9e2893f7d901`
- `git log --oneline | wc -l`: 479
- Uncommitted changes: **Y** — a stray `rustc-ice-2026-04-22T15_05_48-88533.txt`
  at `rust/parse_that/` (untracked)
- Branch: `master`

**Rust toolchain**:
- `rust-toolchain.toml`: **absent** (neither at repo root nor at `rust/`)
- Rust edition: `2024` (every member)
- MSRV: **not declared**
- Pinned nightly: **no**; CI uses ambient `dtolnay/rust-toolchain@nightly`

**`Cargo.toml` surface**:
- Workspace-root at `rust/Cargo.toml`; members: `src`, `parse_that`, `bootstrap`,
  `regex`
- Workspace root declares `[profile.release-lto]` (`inherits = release`,
  `lto = true`) and `[profile.bench]` (fat LTO, `codegen-units = 1`,
  `opt-level = 3`)
- `parse_that`: lib crate, 2 features (`combinators` default, `diagnostics`);
  **13 `[[bench]]` entries**, all `harness = false`; `nightly` feature reachable
  only indirectly via `smallbox = { version = "0.8", features = ["nightly"] }`
- `bbnf-regex`: lib crate at `rust/regex/`; 1 feature (`serde`); **4
  `[[bench]]` entries**, all `harness = false`; depends on the bbnf-lang egraph
  substrate via `[patch.crates-io]`
- `bootstrap` (`regex-bootstrap`): bin-less publish=false crate, 1 bench
- Proc-macro: **no**

**`.cargo/config.toml`**:
- Present at `rust/.cargo/config.toml` (NOT tracked — listed as
  `.gitignored`-per-header)
- `[patch.crates-io]` reaches back into `bbnf-lang` for `bbnf`, `bbnf-ir`,
  `bbnf-ser`, `bbnf_derive`, `egraph`, `egraph-derive`, and reaches
  `../../csc411/.../csp-solver` for `csp-solver`
- No `[alias]` block, no profile overrides, no rustflags

**Test setup**:
- `[dev-dependencies]` in `parse_that`: `serde_json`, `serde`, **`bencher =
  "0.1.5"`**, `pest`, `pest_grammars`, `nom`, `jiter`, `simd-json`, `sonic-rs`,
  `winnow`, `serde_json_borrow`, `lightningcss`, `cssparser`, `mimalloc`,
  `fast-float2`, `regex`
- Integration tests: present under `rust/parse_that/tests/` and
  `rust/regex/tests/`
- `.config/nextest.toml`: **absent**

**CI**:
- `.github/workflows/ci.yml` — TypeScript job (tsc, npm test, build) + Rust job
  (`cargo clippy --workspace -- -D warnings`, `cargo test --workspace`, then
  same with `--features diagnostics`). Single workflow, no matrix, no bench
  invocation.

**Ad-hoc scripts / tool hacks**:
- `justfile` at repo root: `ts-build`, `ts-test`, `ts-check`, `rs-clippy`,
  `rs-test`, `rs-build`, and `all` — thin wrappers over cargo/npm. No
  divergence from CI.
- `rust/scripts/bootstrap-regex.sh`: `cargo expand -p regex-bootstrap | python3
  ... > parse_that/src/regex/generated.rs`. Rewrites `::parse_that::` →
  `crate::` and strips `core::panicking::panic_fmt` nightly idioms. Mirror of
  bbnf-lang's `scripts/bootstrap-bbnf.sh`.

**Proc-macro footprint**:
- Consumes: `bbnf_derive` (at `bootstrap/` via path-patch) — 1 site.
- Does NOT define any proc-macro itself.

**Current pain signal**:
- 1 uncommitted `rustc-ice-*.txt` at `rust/parse_that/` — LTO bitcode
  write-after-target-deletion class (worker panicked writing
  `pre-lto.bc` to a `target/debug/incremental/` dir that vanished underneath
  it). Different pathology from the bbnf-lang `on_disk_cache.rs:663` cluster.
- 3 `[dev-dependencies]` sites still on `bencher = "0.1"` — the abrogation
  target.
- `bbnf-regex` per `feedback_regex_crate_isomorphic`: must internally use the
  same egraph/CSP architecture as bbnf-ir's regex analysis. It does (both
  depend on `egraph` + `egraph-derive` + `csp-solver` through the same
  `[patch.crates-io]` block). Posture **compliant**.

**Modernization verdict**:
**Medium.** Drop 3 `bencher` sites for `divan`, install
`.config/nextest.toml` mirror of bbnf-lang's, add `rust-toolchain.toml`, and
fold `justfile` into whatever toolchain alias scheme the workspace settles on.
No architectural friction — the crate topology is already healthy. Primary
hazard: the `.cargo/config.toml` is not tracked, so CI's matrix tests against
crates.io versions of `bbnf-*`, not path-patched-local. That means a
regression in this repo can go undetected until bbnf-lang pulls the published
version.

---

### pprint (sibling)

**Role in the ecosystem**: Pretty-printing substrate. Provides `pprint` +
`pprint_derive` which every BBNF-consuming crate depends on (core, gorgeous,
parse_that, csp-solver via derive). The only appurtenant repo that publishes
an MSRV.

**Git posture**:
- Path: `/Users/mkbabb/Programming/pprint`
- Separate repo: **Y**
- HEAD SHA: `928c17a7b01731a000b09c26bc99ad525441b090`
- `git log --oneline | wc -l`: 78
- Uncommitted changes: **N**
- Branch: `master`

**Rust toolchain**:
- `rust-toolchain.toml`: **absent**
- Rust edition: `2024`
- MSRV: **declared** — `rust-version = "1.85"` (both `pprint` and
  `pprint_derive`). This is the only repo in the appurtenant surface with an
  explicit rust-version. Note that MSRV 1.85 contradicts every other repo's
  implicit nightly-ambient posture.
- Pinned nightly: no, but CI uses nightly anyway

**`Cargo.toml` surface**:
- Not a workspace. `rust/Cargo.toml` is a plain `[package]`; `rust/derive/`
  is a sibling package
- `pprint`: lib; 3 features (`default = []`, `regex`, `ser`); **2 `[[bench]]`
  entries** (`digit_count.rs`, `pprint.rs`)
- `pprint_derive`: proc-macro lib
- `[profile.bench]` present but empty (`# opt-level = 0` commented) — almost
  certainly stale from an earlier debugging session; should be removed

**`.cargo/config.toml`**:
- Present at `rust/.cargo/config.toml`
- `[patch.crates-io]` has exactly one entry: `bbnf-ser = { path =
  "../../bbnf-lang/crates/ser" }`. Narrower than every other appurtenant
  repo's patch table.
- No aliases, no profile overrides

**Test setup**:
- `[dev-dependencies]`: `rand`, `pretty` — **no bencher, no criterion**. The
  `[[bench]]` entries therefore depend on `test::Bencher` (unstable, requires
  nightly `#![feature(test)]`). This is the 1960s-feeling surface of the
  appurtenant fleet.
- Integration tests: `rust/tests/` — `builder_tests.rs`, `derive_tests.rs`,
  `digit_count.rs`, `pretty_tests.rs`
- `.config/nextest.toml`: **absent**

**CI**:
- `.github/workflows/ci.yml` — single job, `cargo clippy --workspace -- -D
  warnings` + `cargo test --workspace`, `working-directory: rust`. No bench,
  no matrix, no separate release-profile run.

**Ad-hoc scripts / tool hacks**:
- None. No `scripts/`, no `justfile`, no `Makefile`.

**Proc-macro footprint**:
- Defines: `pprint_derive` (`#[derive(PrettyPrint)]`). Consumed by pprint's
  own tests and by bbnf-lang's pprint consumers. Expansion is small by
  appurtenant-repo standards (no `#[derive(Parser)]` surface).

**Current pain signal**:
- 0 ICE files.
- Inline `[[bench]]` depending on unstable `test::Bencher` API is a silent
  blocker: no CI exercises the benches, so a bitrot in them is invisible.
- MSRV = 1.85 but every dependent crate assumes ambient nightly 2026-04 —
  any nightly-only feature that leaks into pprint's public API breaks its
  MSRV claim.

**Modernization verdict**:
**Low-friction.** Smallest surface in the fleet. Add `divan` + 2 benches
trivially rewritten; install a `.config/nextest.toml`. The MSRV claim should
either be raised to match the bbnf-lang ambient or be documented as "MSRV
intent" rather than fact. No architectural change needed.

---

### gorgeous (sibling — parallel copy of `crates/gorgeous`)

**Role in the ecosystem**: Grammar-driven pretty-printer binary (`gorg`) and
library. Path-patched out of the bbnf-lang workspace (`crates/gorgeous`
shadows the sibling via workspace `.cargo/config.toml` line 48). This repo
exists to allow downstream `gorg` releases independent of the bbnf-lang
version bump cadence.

**Git posture**:
- Path: `/Users/mkbabb/Programming/gorgeous`
- Separate repo: **Y**
- HEAD SHA: `df45aca46a1656c4048b2745b519b9c6236d0669`
- `git log --oneline | wc -l`: 72
- Uncommitted changes: **Y** — `src/bbnf.rs.bak` untracked (.bak artefact).
- Branch: `master`

**Rust toolchain**:
- `rust-toolchain.toml`: **absent**
- Rust edition: `2024`
- MSRV: `rust-version = "1.85"` (matches pprint's claim)
- Pinned nightly: no

**`Cargo.toml` surface**:
- Single-crate (not a workspace). bin (`gorg`) + lib (`gorgeous`).
- 2 features: `default = []`, `vm = ["dep:bbnf-ir"]`
- **2 `[[bench]]` entries**: `gorgeous.rs`, `competitors.rs`, both
  `harness = false`
- `[profile.bench]`: fat LTO, `codegen-units = 1`, `opt-level = 3`
- Biome versions pinned at `=0.4.0` (contrast with the path-patched copy at
  `crates/gorgeous` which pins to the monorepo's `cli/v1.9.4` tag to avoid
  the 0.5.7/0.5.8 rowan skew — this sibling is therefore vulnerable to the
  same uncompilable combination on upstream update)

**`.cargo/config.toml`**:
- Present at repo root `/Users/mkbabb/Programming/gorgeous/.cargo/config.toml`
- `[patch.crates-io]` block points to the **pre-architectural-consolidation
  paths** (`../bbnf-lang/rust/bbnf`, `../bbnf-lang/rust/bbnf-derive`,
  `../bbnf-lang/rust/bbnf-ir`). Those paths no longer exist — the
  `rust/→crates/` restructuring landed April 2026 per
  `project_arch_consolidation`. This patch table is **broken**; any `cargo
  check` run from `/Users/mkbabb/Programming/gorgeous` will fail resolution
  and fall through to crates.io.
- No aliases, no profile overrides

**Test setup**:
- `[dev-dependencies]`: **`bencher = "0.1"`**, `biome_css_parser =
  "=0.4.0"`, `biome_css_formatter =
  "=0.4.0"`, `biome_formatter = "=0.4.0"`, `biome_css_syntax = "=0.4.0"`,
  `cssparser`, `lightningcss`
- Integration tests: `tests/biome_compare.rs`, `tests/biome_compare2.rs`,
  `tests/vm.rs`
- `.config/nextest.toml`: **absent**

**CI**:
- `.github/workflows/ci.yml` — checks out `mkbabb/parse-that`,
  `mkbabb/bbnf-lang`, `mkbabb/pprint` as siblings (the repo's one genuine
  multi-checkout), then `sed`s the path patches in `Cargo.toml` to point at
  those sibling checkouts. The `sed` rewrites target paths (`rust/bbnf`,
  `rust/bbnf-derive`, `rust/bbnf`) that **also no longer exist in
  bbnf-lang**. CI must be broken for this repo on HEAD unless CI is running
  against a pinned older bbnf-lang SHA.

**Ad-hoc scripts / tool hacks**:
- None beyond the CI sed-rewrites.

**Proc-macro footprint**:
- Consumes: `bbnf_derive` via 6 `#[derive(Parser)]` sites — `jit.rs`, `bnf.rs`,
  `ebnf.rs`, `json.rs`, `bbnf.rs`, `css.rs`, `google_sheets.rs` (plus a
  `bbnf.rs.bak` artefact that still carries a site). Aggregate expansion is
  the single heaviest in the workspace — the bbnf-lang workspace
  `iter-check` alias explicitly excludes `gorgeous` because the aggregate
  emits ~30 k-LOC TokenStream per site and serialises inside a single rustc
  (9+ min cold per workspace per the alias rationale in workspace
  `.cargo/config.toml`).
- Defines: none.

**Current pain signal**:
- 0 ICE files at this repo (contrast the 93 at bbnf-lang).
- The `bbnf.rs.bak` untracked leftover is a yellow flag — someone edited
  `bbnf.rs` and kept a timestamped copy. `feedback_generated_files_clean_regen`
  explicitly forbids hand-patching generated files; the presence of a `.bak`
  next to the generated `bbnf.rs` invites exactly that violation.
- Two layers of staleness (config paths + CI sed) prove this sibling is
  effectively unused in day-to-day iteration; all real work happens against
  `crates/gorgeous`.

**Modernization verdict**:
**Medium.** The repo needs a path-patch realignment
(`../bbnf-lang/rust/bbnf` → `../bbnf-lang/crates/core`, etc.) and a CI sed
rewrite to match, plus the standard bencher→divan + nextest pass. The
sideloaded biome pins (`=0.4.0`) must be reconciled with the workspace copy
(`cli/v1.9.4`) or the two diverge silently. If the intent is to retire this
sibling and make `crates/gorgeous` authoritative, that should happen first;
modernizing a near-dead mirror is wasted work.

---

### csp-solver (sibling at `csc411/CSC411_HW2_ProgrammingQuestion/csp-solver`)

**Role in the ecosystem**: Upstream of the vendored `crates/csp-solver`. Hosts
the CSP/COP substrate plus its PyO3 binding, WASM sub-crate (`wasm/`), and
`wasm-morph/` + `morph-core` twin (separate substrate for the morph-compare
experiment, distinct concern from bbnf-lang's grammar-CSP consumer). The
exemplar of the `feedback_wasm_subcrate_pattern` — wasm bindings as a
workspace-member cdylib inside the parent.

**Git posture**:
- Path: `/Users/mkbabb/Programming/csc411/CSC411_HW2_ProgrammingQuestion` (the
  parent repo; `csp-solver` is a sub-directory crate not a separate git root)
- Separate repo: **Y** (git root is the parent `CSC411_HW2_ProgrammingQuestion/`)
- HEAD SHA: `b70098676f2fc09979f1969341f5115bd774cbd5`
- `git log --oneline | wc -l`: 302
- Uncommitted changes: **Y** — but only in `web/frontend/node_modules/*`
  (deletions + a `.package-lock.json` mod). No uncommitted changes within
  `csp-solver/` itself.
- Branch: `master`

**Rust toolchain**:
- `rust-toolchain.toml`: **absent**
- Rust edition: `2024`
- MSRV: **not declared**
- Pinned nightly: no

**`Cargo.toml` surface**:
- `csp-solver`: lib (`crate-type = ["lib"]`; cdylib added by maturin via
  `pyproject.toml`). Features: `default = []`, `py = ["dep:pyo3"]`. **6
  `[[bench]]` entries**: `sudoku`, `queens`, `map_coloring`, `lattice`,
  `assignment`, `cost_finite_domain`. All `harness = false`.
- `morph-core`: lib, publish=false, 2 benches
- `wasm/`: cdylib+rlib, 0 benches, carries `[package.metadata.wasm-pack.profile.release]`
  for `wasm-opt = ["-Oz"]`
- `wasm-morph/`: parallel cdylib for the morph-core binding

**`.cargo/config.toml`**:
- **Absent** at both the parent and the crate level
- No aliases, no profile overrides

**Test setup**:
- `[dev-dependencies]`: **`criterion = "0.5"`** (HTML reports enabled),
  `hungarian`, `proptest`. This is the only appurtenant repo still on
  criterion; every other is bencher or neither.
- `morph-core`'s `[dev-dependencies]`: `approx`, `criterion`, `proptest`
- Integration tests: `tests/` present
- `.config/nextest.toml`: **absent**

**CI**:
- `.github/workflows/deploy.yml.disabled` — **disabled** (the `.disabled`
  suffix is load-bearing). Covers Python backend tests (uv + pytest) +
  frontend build (node). Comment notes production deploy is behind VPN,
  Rust tests are not exercised at all.
- No active Rust CI in this repo.

**Ad-hoc scripts / tool hacks**:
- `scripts/bench-compare.sh`: `git worktree add` twin-ref bench comparison
  using criterion `--baseline`. Exactly the pattern that divan's built-in
  `--baseline` and `cargo bench --profile bench` + samply should replace.

**Proc-macro footprint**:
- None defined, none consumed.

**Current pain signal**:
- 0 ICE files.
- `criterion` as primary bench framework — the single biggest abrogation
  target in the fleet (7 benches across `csp-solver` + `morph-core`).
- No active Rust CI: regressions in the upstream land invisibly until
  bbnf-lang pulls.
- `isomorphic-api`: PyO3 bindings exist (`py` feature + `pyproject.toml`).
  WASM bindings exist as sub-crate per `wasm-subcrate-pattern`. API drift
  between Python and Rust not checked here (flagged per scope, not fixed).

**Modernization verdict**:
**Medium-to-deep.** Criterion migration touches 8 bench files; `bench-compare.sh`
needs rewrite around divan's native baseline; CI must be re-enabled on a
non-deploy workflow; the `web/frontend/node_modules` git churn suggests that
directory should be gitignored. Upstream-vs-vendored divergence from
`crates/csp-solver` needs reconciliation — see §Part B.4.

---

### crates/core (`bbnf`)

**Role in the ecosystem**: The BBNF compiler crate. Depends on parse_that +
pprint + bbnf-ir + bbnf-ser + tape + simd-scan + egraph + csp-solver.
Consumes `bbnf_derive`. Hosts every grammar bench (JSON, CSS L4, Google
Sheets, BBNF self-host, WASM, TypeScript comparators).

**Git posture**:
- Path: `/Users/mkbabb/Programming/bbnf-lang/crates/core`
- Separate repo: **N** (workspace member of bbnf-lang)
- Workspace HEAD SHA: `48e6eaa9ff78ee8cbbc32ae0e89f7890b5734ff1`; workspace log
  length 1842 commits

**Rust toolchain** (inherits bbnf-lang workspace posture):
- `rust-toolchain.toml`: **absent**
- Rust edition: `2024`
- MSRV: **not declared**
- Pinned nightly: no

**`Cargo.toml` surface**:
- lib (`bbnf`). 1 feature: `dhat-heap = ["dep:dhat"]`.
- **19 `[[bench]]` entries**, all `harness = false`. This is the densest
  bench surface in the fleet.
- `[dev-dependencies]`: **`bencher = "0.1"`**, `mimalloc`, `sonic-rs`,
  `serde_json`, `serde`, `simd-json`, `jiter`, `serde_json_borrow`, `nom`,
  `winnow`, `pest`, `pest_grammars`, `tree-sitter`, `tree-sitter-json`,
  `cssparser`, `lightningcss = "1.0.0-alpha.71"`, `wasmtime`, `fast-float2`
- Proc-macro: **no**

**`.cargo/config.toml`**: inherits workspace (none local)

**Test setup**:
- Workspace nextest config applies
- Integration tests at `tests/` — 25+ parity harnesses including
  `sonic_rs_parity.rs`, `lightningcss_parity.rs`, `css_l4_parity.rs`,
  `value_api_apples_to_apples.rs`, etc.
- `.config/nextest.toml`: inherited from workspace root

**CI**: workspace CI (`bbnf-lang/.github/workflows/ci.yml`) exercises
  `cargo test -p bbnf --test sonic_rs_parity --release` and
  `--test lightningcss_parity --release` as heavy close gates.

**Ad-hoc scripts / tool hacks**: inherits workspace scripts

**Proc-macro footprint**:
- Consumes `bbnf_derive` at **~30+ sites** across `tests/`, `benches/`,
  `examples/`, plus `src/runtime/mod.rs` + `src/runtime/parsed.rs`. Heavy by
  any measure; the workspace `iter-check` alias in
  `.cargo/config.toml` skips `bootstrap` + `gorgeous` but still includes
  `core` because `core`'s sites are test/bench-gated, not lib-gated.

**Current pain signal**:
- 93 `rustc-ice-*.txt` at the workspace root, all the `on_disk_cache.rs:663 —
  cannot decode AttrId with CacheDecoder` class. Dates cluster 2026-04-15
  through 2026-04-22; 45 of them land on 2026-04-15 alone. The class is
  incremental-cache staleness, typically triggered by a derive-macro
  site expansion changing shape between incremental runs. Given `core`
  hosts ~30 `#[derive(Parser)]` consumer sites, it is the primary victim.
- `bencher` in dev-deps is the fleet-wide abrogation target.
- No ad-hoc bench scripts for `core` — benches drive through the workspace
  `prep-bench` / `final-bench` alias from the root `.cargo/config.toml`.

**Modernization verdict**:
**Medium.** 19-bench divan migration is non-trivial but mechanical. The ICE
cluster is the single largest current-pain signal in the fleet; it must be
addressed before divan migration lands, otherwise every regenerated expansion
will re-trigger the staleness. `cargo clean` + incremental-cache purge is the
workaround; root cause is a nightly rustc bug on Apple Silicon in this
window.

---

### crates/derive (`bbnf_derive`)

**Role in the ecosystem**: Proc-macro emitter for the `#[derive(Parser)]`
attribute. Emits ~30 k-LOC TokenStream per site. Consumed by every grammar
consumer in the fleet.

**Git posture**: workspace member (inherits).

**Rust toolchain**: inherits (edition 2024, no MSRV, no pin).

**`Cargo.toml` surface**:
- `proc-macro = true`
- No features
- No benches
- `[dependencies]`: `bbnf`, `bbnf-ir`, `pprint`, `parse_that`, `syn 2`,
  `quote 1`, `proc-macro2 1`, `indexmap 2`

**`.cargo/config.toml`**: inherits workspace

**Test setup**: no dev-deps, no tests/ — derive is exercised transitively.

**CI**: inherits workspace CI; clippy + workspace test cover it.

**Ad-hoc scripts**: `build.rs` present (one of the few in the workspace)

**Proc-macro footprint**:
- **Defines `#[derive(Parser)]`**. Consumer count across the fleet: ~87
  grep hits. Expansion cost per site is the single largest compile-time
  driver in the ecosystem — exceeds `pprint_derive`, `egraph-derive`, and
  `bbnf_ser`'s emission by an order of magnitude.

**Current pain signal**: directly implicated in the 93-ICE cluster —
the `on_disk_cache.rs:663 — cannot decode AttrId` panic is triggered by
incremental re-compilation of derive expansions that change between runs.
Every time a grammar file is edited, the derive site re-emits a different
TokenStream, invalidating incremental cache entries, and occasionally the
decoder panics on stale `AttrId`s.

**Modernization verdict**:
**Deep.** Not because of divan/nextest — derive has no benches — but
because the expansion-time cost is the single largest compile-time tax in
the fleet and drives the ICE cluster. Post-modernization work should
include: (1) a stable expansion-cost regression gate (bench the derive
expansion against a canonical grammar corpus with divan), (2) an
incremental-cache discipline pass (`cargo clean` scheduling, or a nightly
pin that dodges the bug). The bench gate is the hardest architectural ask
in the fleet — emission cost is what `iter-check` excludes gorgeous +
bootstrap for.

---

### crates/analysis (`bbnf-analysis`)

**Role in the ecosystem**: Pure analysis logic for BBNF grammars, shared by
LSP + WASM. Per `project_analysis_consolidation`, AST analysis is being
consolidated out of this crate into IR passes — i.e., this crate is on the
deprecation glide-path.

**Git posture**: workspace member

**Rust toolchain**: inherits

**`Cargo.toml` surface**:
- lib (`bbnf-analysis`)
- No features, no benches
- `[dependencies]`: `ls-types`, `bbnf`, `bbnf-ir`, `pprint`, `indexmap`,
  `self_cell = "1.2.2"`

**`.cargo/config.toml`**: inherits

**Test setup**: no dev-deps listed → relies on workspace-level harness only.

**CI**: workspace CI runs `cargo clippy --all-targets -- -D warnings` which
exercises it. Note: workspace `iter-check` alias EXCLUDES `bbnf-analysis`
alongside `gorgeous`, `bootstrap`, `lsp` — so compile-gate iteration is not
running against this crate.

**Ad-hoc scripts**: none specific

**Proc-macro footprint**: consumes none, defines none

**Current pain signal**: this crate's existence is itself the pain signal
— the consolidation intent flags it for deletion. Continuing to modernize
deprecated code wastes effort.

**Modernization verdict**:
**Low-friction** (pure divan + nextest pass would be trivial) but
**should be deferred** until the consolidation completes. Hold.

---

### crates/ir (`bbnf-ir`)

**Role in the ecosystem**: Canonical Grammar IR for the BBNF compiler
pipeline. Hosts the grammar e-graph rewrite rules and CSP-scheduled pass
manager. Depends on `bbnf-regex` (for leaf classification e-graph), `egraph`,
`egraph-derive`, `csp-solver`. Paired with `parse-that/rust/regex/` per
`feedback_regex_crate_isomorphic`.

**Git posture**: workspace member

**Rust toolchain**: inherits

**`Cargo.toml` surface**:
- lib, no features
- No benches
- `[dependencies]`: `serde` with `rc` feature, `serde_json`, `rmp-serde`,
  `parse_that`, `bbnf-regex` with `serde`, `rustc-hash`, `rayon`, `smallvec`,
  `csp-solver`, `egraph`, `egraph-derive`

**`.cargo/config.toml`**: inherits workspace; workspace-level `[profile.dev]`
gives `bbnf-ir` `opt-level = 1` explicitly

**Test setup**: no `dev-dependencies`; relies on `cargo test --workspace`

**CI**: workspace CI; included in `iter-check`

**Ad-hoc scripts**: none

**Proc-macro footprint**: consumes `egraph-derive` (the `#[derive(Language)]`
per `feedback_derive_language`); no proc-macro sites in this crate itself.

**Current pain signal**: implicated indirectly in the ICE cluster via its
derive-Language sites. No benches to migrate.

**Modernization verdict**:
**Low-friction.** Add benches for e-graph saturation (currently measured via
core's `compile_pipeline.rs` — divided attention). Otherwise inherits every
workspace modernization.

---

### crates/lsp (`bbnf-lsp`)

**Role in the ecosystem**: Language-server binary for BBNF grammar files.
Shipped as the VSCode extension's server backend. Depends on `bbnf`,
`bbnf-ir`, `bbnf-analysis`.

**Git posture**: workspace member

**Rust toolchain**: inherits

**`Cargo.toml` surface**:
- lib + bin (`bbnf-lsp`)
- No features, no benches
- `[dependencies]`: `tower-lsp-server = "0.23"`, `tokio` full features,
  `serde`, `serde_json`, `bbnf`, `bbnf-ir`, `bbnf-analysis`
- `[dev-dependencies]`: `tempfile`

**`.cargo/config.toml`**: inherits

**Test setup**: integration tests likely in `tests/` (not enumerated here)

**CI**: `bbnf-lang/.github/workflows/release.yml` is primarily a `cargo
build --release -p bbnf-lsp` matrix across 5 targets (Linux x86_64/aarch64,
macOS x86_64/aarch64, Windows x86_64) for VSCode extension releases. Most
infrastructure-heavy CI job in the fleet.

**Ad-hoc scripts**: `Makefile` at workspace root has `build-lsp` / `dev`
targets that copy `target/release/bbnf-lsp` to `server/`.

**Proc-macro footprint**: none local; indirect via `bbnf`.

**Current pain signal**: EXCLUDED from `iter-check` alongside analysis,
gorgeous, bootstrap — so compile-gate iteration does not cover it.
Regressions here surface only at the workspace `cargo test --workspace`
heavy gate.

**Modernization verdict**:
**Low-friction.** Inherits workspace modernization; no benches to migrate.
The release.yml matrix should adopt the same rust-toolchain pin the rest
of the workspace settles on.

---

### crates/ser (`bbnf-ser`)

**Role in the ecosystem**: Grammar-guided Serializer/Deserializer traits.
Smallest lib in the workspace. Consumed optionally by pprint (via its `ser`
feature).

**Git posture**: workspace member

**Rust toolchain**: inherits

**`Cargo.toml` surface**:
- lib
- No features, no benches, no dev-dependencies
- `[dependencies]`: `ryu = "1"`, `itoa = "1"` (numeric formatting only)

**`.cargo/config.toml`**: inherits

**Test setup**: no tests/, no benches

**CI**: `iter-test-leaf` alias includes `bbnf-ser` — one of the tagged
leaf-tier crates

**Ad-hoc scripts**: none

**Proc-macro footprint**: none

**Current pain signal**: none. Possibly the only repo/crate in the fleet
with zero modernization surface area.

**Modernization verdict**:
**Low-friction** (nothing to do). Already minimal.

---

### crates/gorgeous (workspace copy)

**Role in the ecosystem**: Authoritative `gorgeous` source path-patched into
the workspace. Mirrors the sibling `/Users/mkbabb/Programming/gorgeous` but
diverges in biome pins (uses `cli/v1.9.4` tag vs. sibling's `=0.4.0`). Lives
in-tree because the derive-Parser aggregate here is the heaviest compile-gate
site; pulling it into the workspace makes `cargo expand` + `prep-bench`
flows coherent.

**Git posture**: workspace member (same SHA as bbnf-lang)

**Rust toolchain**: inherits

**`Cargo.toml` surface**:
- lib + bin (`gorg`)
- **8 features**: `default` (re-exports all 6 grammar features plus
  `bbnf-grammar`), `bbnf-grammar`, `json-grammar`, `css-grammar`,
  `ebnf-grammar`, `bnf-grammar`, `sheets-grammar`, `bin-full`, `vm`. Features
  per-grammar-gate were introduced per the workspace comment "so a downstream
  consumer pays only for the grammars it imports" — i.e., the feature surface
  is the compile-cost knob for the 6 derive-Parser sites inside this crate.
- No benches in this crate copy (sibling has 2; benches live at
  `/Users/mkbabb/Programming/gorgeous/benches/`)
- `[dev-dependencies]`: **`biome_css_parser` + `biome_css_formatter`** pinned
  via git tag `cli/v1.9.4` (rationale: monorepo-tag dodges the 0.5.7/0.5.8
  rowan skew that breaks crates.io published builds on modern nightly)

**`.cargo/config.toml`**: inherits workspace (no local)

**Test setup**: inherits workspace nextest

**CI**: workspace CI — but **EXCLUDED from `iter-check`** per the alias
rationale ("≥1 `#[derive(Parser)]` site that triggers the full bbnf-derive
proc-macro pipeline"). Covered only at workspace-test time.

**Ad-hoc scripts**: none specific

**Proc-macro footprint**:
- Consumes `bbnf_derive` at 6 sites (one per grammar feature). Each site's
  expansion is gated by its feature flag, so enabling only `json-grammar`
  pays only that one site's ~30 k-LOC.

**Current pain signal**:
- Primary contributor to the 93-ICE cluster — its 6 aggregated Parser sites
  are the highest AttrId churn in the workspace.
- Biome dev-deps live on a git-tag pin, which is fragile but currently the
  only working path (see sibling gorgeous's `=0.4.0` pin that is broken).

**Modernization verdict**:
**Medium.** The feature surface is architecturally valuable — it IS the
compile-cost knob. Divan migration: defer benches to the sibling copy.
Primary modernization action: continue to use the sibling as the bench
host, make the sibling's path-patches tracked (not gitignored) so its
modernization pass can follow-through on the workspace's.

---

### crates/bootstrap (`bbnf-bootstrap`)

**Role in the ecosystem**: Generates self-hosted BBNF grammar parser via
`cargo expand`. Publish=false. The workspace's equivalent of parse-that's
`regex-bootstrap`.

**Git posture**: workspace member

**Rust toolchain**: inherits

**`Cargo.toml` surface**:
- lib (implicit from `[dependencies]`; no `[lib]` section, no `[bin]`)
- publish=false
- No features, no benches, no dev-deps
- `[dependencies]`: `bbnf_derive`, `bbnf`, `bbnf-ir`, `parse_that`, `pprint`

**`.cargo/config.toml`**: inherits

**Test setup**: no tests/

**CI**: **EXCLUDED from `iter-check`** (carries the 133-LOC self-host grammar
as a single `#[derive(Parser)]` site, ~3-5 min cold expansion per the
workspace alias comment). Covered only by `scripts/bootstrap-bbnf.sh`
and `scripts/check-bootstrap-clean.sh` (which the CI workflow DOES run as
first preflight step).

**Ad-hoc scripts**: `scripts/bootstrap-bbnf.sh` at workspace root drives
this crate's regen pipeline.

**Proc-macro footprint**:
- 1 site of `bbnf_derive::Parser`. Aggregate expansion is the second-heaviest
  after gorgeous.

**Current pain signal**:
- Per `feedback_generated_files_clean_regen`, this crate's output must be
  fresh regen. The `check-bootstrap-clean.sh` preflight guards against
  hand-patching. No current violation.
- Contributes to the ICE cluster.

**Modernization verdict**:
**Medium.** Inherits the derive-expansion ICE liability. No direct
modernization work — nothing to migrate — but the incremental-cache issue
blocks clean iteration through this crate.

---

### crates/egraph

**Role in the ecosystem**: General-purpose e-graph substrate: equality
saturation, rewrite rules, cost-model extraction. Per
`feedback_general_infra_crates` this is deliberately a general-purpose
crate, not a bbnf-specific module. Consumed by `bbnf-ir` (grammar e-graph)
and `bbnf-regex` (HIR e-graph).

**Git posture**: workspace member

**Rust toolchain**: inherits

**`Cargo.toml` surface**:
- lib
- No features, no benches
- `[dependencies]`: `smallvec`, `rustc-hash`, `csp-solver`
- `[dev-dependencies]`: `egraph-derive` (path)

**`.cargo/config.toml`**: inherits

**Test setup**: `tests/` present; exercises `#[derive(Language)]` via the
`egraph-derive` dev-dep

**CI**: workspace CI; included in `iter-test-leaf` alias

**Ad-hoc scripts**: none

**Proc-macro footprint**: consumes `egraph-derive` for testing; defines
none itself.

**Current pain signal**: none directly. Has no benches of its own — the
CSP-driven saturation schedule is benched in `core` and `bbnf-ir` indirectly.

**Modernization verdict**:
**Low-friction.** Primary modernization opportunity: add divan benches for
`add` / `congruence` / `apply_rules` / `extract` — currently the e-graph
performance is measured only at the parse-pipeline bench level, which is
too coarse for regression detection. Per `feedback_general_infra_crates`
this deserves its own bench surface.

---

### crates/egraph-derive

**Role in the ecosystem**: `#[derive(Language)]` proc-macro for the egraph
crate. Per `feedback_derive_language` it projects recursive enum fields
into `Id` references trivially.

**Git posture**: workspace member

**Rust toolchain**: inherits

**`Cargo.toml` surface**:
- proc-macro lib
- No features, no benches, no dev-deps
- `[dependencies]`: `syn`, `quote`, `proc-macro2`

**`.cargo/config.toml`**: inherits

**Test setup**: none local (exercised via `egraph` dev-deps)

**CI**: workspace CI covers it transitively

**Ad-hoc scripts**: none

**Proc-macro footprint**:
- Defines `#[derive(Language)]`. Consumed by `bbnf-ir` (grammar Language
  enum), `bbnf-regex` (HIR Language enum), and `egraph` tests.

**Current pain signal**: contributes minor ICE liability (consumer-count is
small: ~3-5 sites total). Orders of magnitude lighter than `bbnf_derive`.

**Modernization verdict**:
**Low-friction.** No direct work.

---

### crates/csp-solver (workspace copy)

**Role in the ecosystem**: Vendored from the sibling csc411 repo per Tranche
AA.2. Provides the generalized CSP/COP substrate used by both `bbnf-ir`
(grammar pass scheduling) and `egraph` (saturation scheduling per
`feedback_csp_always_optimize`).

**Git posture**: workspace member (vendored; see §Part B.4)

**Rust toolchain**: inherits

**`Cargo.toml` surface**:
- lib (`crate-type = ["lib"]`)
- 2 features: `default = []`, `py = []` ("not built by the bbnf-lang
  workspace. Present for symmetry with the upstream csc411 repo")
- **No benches** in this copy (sibling has 6!)
- No `[dependencies]` declared (the `py` feature has no `dep:pyo3` because
  PyO3 isn't in the workspace; this is a subtle deviation from the sibling's
  `py = ["dep:pyo3"]`)
- No `[dev-dependencies]` — including no `criterion`

**`.cargo/config.toml`**: inherits

**Test setup**: workspace-level only; included in `iter-test-leaf` alias

**CI**: workspace CI

**Ad-hoc scripts**: none in this copy (sibling has `scripts/bench-compare.sh`)

**Proc-macro footprint**: none

**Current pain signal**: drift from sibling (see §Part B.4). No local benches
means no bbnf-lang perf gate on the CSP substrate at all — a concerning gap
given `feedback_csp_always_optimize` flags it as foundational.

**Modernization verdict**:
**Medium.** The sibling-vs-vendored divergence is the architectural issue
(Part B.4); the mechanical work is to either re-vendor the sibling's
`[[bench]]` entries + migrate to divan, or establish an authoritative
vendoring cadence with `cargo vendor`-style discipline.

---

### crates/tape

**Role in the ecosystem**: Tape representation for bbnf-lang parser output —
the eager-AST replacement. Columnar (Struct-of-Arrays) record substrate.
Per feedback `preserve-rich-ast` its presence is the ONE sanctioned speed
optimisation that doesn't sacrifice AST richness.

**Git posture**: workspace member

**Rust toolchain**: inherits

**`Cargo.toml` surface**:
- lib
- 2 features: `default = ["rayon"]`, `dta-replay` (AW.1.7 — "decision log +
  resumable snapshot" per the workspace comment; off by default so LLVM has
  no hot-path branch to hoist)
- **1 `[[bench]]`**: `reduce_column` (AW-IV.W5.1 — "≥ 6× SIMD speedup over
  scalar left-fold" hard gate)
- `[dependencies]`: `serde` optional, `rayon` optional
- `[dev-dependencies]`: empty

**`.cargo/config.toml`**: inherits

**Test setup**: `tests/`; covered by workspace harness + `iter-test-leaf`

**CI**: workspace CI

**Ad-hoc scripts**: none specific

**Proc-macro footprint**: none

**Current pain signal**: the 1 bench uses Rust's unstable `test::Bencher` API
(same as pprint) — no bencher/criterion dep but also no modern harness. The
`harness = false` flag is set but no framework is wired in; the bench must be
providing its own main.

**Modernization verdict**:
**Low-friction.** Migrate the single bench to divan, validate the 6× SIMD
gate still holds.

---

### crates/simd-scan

**Role in the ecosystem**: Architecture-neutral SIMD structural-bitmap kernel
for the DTA driver. Builds a StructuralIndex (positions + kinds) once per
parse via per-arch kernels (NEON / AVX2 / AVX-512 / WASM SIMD / portable
scalar) with runtime feature detection.

**Git posture**: workspace member

**Rust toolchain**: inherits (but note: `avx512` feature requires
`RUSTFLAGS="-C target-feature=+avx512vbmi2"` per the crate comment — an
additional toolchain knob not captured by `rust-toolchain.toml`)

**`Cargo.toml` surface**:
- lib
- 2 features: `default = []`, `avx512` (opt-in)
- **1 `[[bench]]`**: `stage1_throughput`
- `[dependencies]`: `tape` (default-features=false), `proc-macro2`, `syn`,
  `quote` — odd: the proc-macro2/syn/quote deps are usually proc-macro
  toolchain, but this is a plain lib. Presumably used for compile-time SIMD
  kernel codegen or build.rs; that should be audited.
- `[dev-dependencies]`: `proptest`

**`.cargo/config.toml`**: inherits

**Test setup**: `tests/` + proptest-based fuzz

**CI**: workspace CI

**Ad-hoc scripts**: none specific

**Proc-macro footprint**: none defined, none consumed as proc-macro (the
`syn`/`quote` usage is at runtime, not macro-time)

**Current pain signal**:
- The `syn`/`quote` runtime deps look wrong for a simd-kernel lib — likely
  a stale dependency leftover from an earlier code-generation approach.
  Should be audited; if unused, drop.

**Modernization verdict**:
**Low-friction** for divan. Needs a dependency audit to drop the
`proc-macro2`/`syn`/`quote` runtime deps unless they are load-bearing.

---

### crates/json-prototype

**Role in the ecosystem**: JSON-only hand-tuned per-shape inline parser
prototype. AW-V.W2 speed-ceiling validation against sonic-rs via twin-pair
benches.

**Git posture**: workspace member

**Rust toolchain**: inherits

**`Cargo.toml` surface**:
- lib
- No features
- **1 `[[bench]]`**: `json_value`
- `[dependencies]`: `tape`, `simd-scan`, `parse_that` (inlines Eisel-Lemire
  `compute_f64` via workspace LTO)
- `[dev-dependencies]`: **`bencher = "0.1"`**, `mimalloc`, `sonic-rs = "0.3"`,
  `serde_json`

**`.cargo/config.toml`**: inherits

**Test setup**: no tests/ — prototype-only

**CI**: workspace CI

**Ad-hoc scripts**: none specific

**Proc-macro footprint**: none

**Current pain signal**:
- `bencher` dep — abrogation target
- `sonic-rs = "0.3"` here vs. `sonic-rs = "0.5"` in core — version drift
  between the two JSON benches that compete against each other

**Modernization verdict**:
**Low-friction.** Migrate 1 bench to divan, pin sonic-rs version to match
core.

---

## Part B — Cross-repo synthesis

### B.1 Bench-framework distribution

| repo | criterion | bencher | divan | iai-callgrind | test::Bencher | none |
|---|---|---|---|---|---|---|
| parse-that (parse_that) | . | **13 benches** | . | . | . | . |
| parse-that (regex) | . | **4 benches** | . | . | . | . |
| parse-that (bootstrap) | . | **1 bench** | . | . | . | . |
| pprint | . | . | . | . | **2 benches** | . |
| gorgeous (sibling) | . | **2 benches** | . | . | . | . |
| csp-solver (sibling) | **6 benches** | . | . | . | . | . |
| csp-solver/morph-core | **2 benches** | . | . | . | . | . |
| crates/core | . | **19 benches** | . | . | . | . |
| crates/derive | . | . | . | . | . | **yes** |
| crates/analysis | . | . | . | . | . | **yes** |
| crates/ir | . | . | . | . | . | **yes** |
| crates/lsp | . | . | . | . | . | **yes** |
| crates/ser | . | . | . | . | . | **yes** |
| crates/gorgeous | . | . | . | . | . | **yes** (benches live in sibling) |
| crates/bootstrap | . | . | . | . | . | **yes** |
| crates/egraph | . | . | . | . | . | **yes** |
| crates/egraph-derive | . | . | . | . | . | **yes** |
| crates/csp-solver | . | . | . | . | . | **yes** (benches live in sibling) |
| crates/tape | . | . | . | . | **1 bench** | . |
| crates/simd-scan | . | . | . | . | **1 bench** | . |
| crates/json-prototype | . | **1 bench** | . | . | . | . |

**Totals**: 8 criterion benches (csp-solver sibling + morph-core), 40 bencher
benches (parse-that 18 + gorgeous 2 + core 19 + json-prototype 1), 4 unstable
`test::Bencher` benches (pprint 2, tape 1, simd-scan 1). **ZERO divan benches.**

**Most invasive migration**: `crates/core` (19 benches) and `parse-that/rust/
parse_that` (13 benches) together account for 32 of the 40 bencher benches.
These two repos are the primary divan-migration heavy-lift. The criterion
migration is smaller (8 benches) but spread across a sibling repo with no
active CI — doing that migration first, on a repo with no concurrent
regressions, is the safest foothold.

### B.2 Rust-toolchain alignment

**No repo has a `rust-toolchain.toml`.**

MSRV declarations:
- `pprint` + `pprint_derive`: `rust-version = "1.85"`
- `gorgeous` (sibling): `rust-version = "1.85"`
- every other repo: none

Ambient nightly used by every CI workflow (`dtolnay/rust-toolchain@nightly`
in 4 of the 4 active workflows).

Verified-working nightly from the ICE files: `1.96.0-nightly (9602bda1d
2026-04-05)` — that's the rustc that the 93 `on_disk_cache.rs:663`
panics were emitted against, and the 1 parse-that LTO panic.

**Unified pin recommendation**: `rust-toolchain.toml` at every repo pinning
to a nightly one step older than the observed-buggy `9602bda1d` (2026-04-05)
— or whichever is next known-clean. The consistency gain is enormous: right
now any developer on a different nightly sees different expansion shapes
from the derive sites, which feeds directly into the ICE cluster.

Secondary action: raise the pprint + gorgeous MSRV claim to match the
workspace, or drop it entirely. An MSRV claim that conflicts with the
required nightly features is worse than no claim.

### B.3 `.cargo/config.toml` alias surface across repos

| repo | `.cargo/config.toml` | aliases | patch-crates-io entries | rustflags |
|---|---|---|---|---|
| bbnf-lang (workspace) | tracked | **9 aliases** (`iter-check`, `iter-check-full`, `iter-test-leaf`, `iter-test-grammar`, `expand-json`, `expand-css`, `expand-bbnf`, `expand-sheets`, `asm-parse`, `prep-bench`, `final-bench`) | 11 entries | commented (mold/lld ready) |
| parse-that (rust/.cargo) | gitignored | 0 | 8 entries (reaches back to bbnf-lang + csp-solver) | none |
| pprint (rust/.cargo) | — | 0 | 1 entry (bbnf-ser only) | none |
| gorgeous (sibling .cargo) | gitignored | 0 | 6 entries (**STALE PATHS** — `../bbnf-lang/rust/*` no longer exists) | none |
| csp-solver (sibling) | absent | — | — | — |

**Contradictions**:
- Only bbnf-lang has aliases. Every other repo invokes `cargo test --workspace`
  directly. The bbnf-lang alias surface is carefully tuned (profile-tier routing,
  per-crate exclusions for heavy derive sites) and is nontrivial to replicate
  in sibling repos that don't carry the same workspace structure. If those
  siblings ever need a similar discipline, they need an equivalent alias
  scheme.
- parse-that's `.cargo/config.toml` is `.gitignored` (per the header) but
  contains the path-patches that make CI pass — which means CI is actually
  running against whatever was most recently put there, not a tracked
  truth. Same for gorgeous.

**Most urgent alignment action**: fix gorgeous's stale `rust/bbnf` paths
(should be `crates/core` / `crates/derive` / `crates/ir` per the
`project_arch_consolidation` migration). That repo's CI sed-rewrites point
at paths that no longer exist in bbnf-lang HEAD.

### B.4 Proc-macro expansion graph

**Derive crates defined in the fleet**:

| derive crate | consumers | approximate site count |
|---|---|---|
| `bbnf_derive` (`#[derive(Parser)]`) | `crates/core` (tests+benches+examples+src), `crates/gorgeous` (6 grammar sites), `crates/bootstrap` (1 self-host site), `crates/derive` (build.rs pre-emits), `parse-that/rust/bootstrap`, `gorgeous` sibling (6 sites), `gorgeous` sibling `bbnf.rs.bak` | **~87 workspace+sibling sites** |
| `pprint_derive` (`#[derive(PrettyPrint)]`) | not enumerated here (grep miss) — but consumed broadly across pprint tests + examples | small (est. <20 sites, light emission) |
| `egraph-derive` (`#[derive(Language)]`) | `bbnf-ir`, `bbnf-regex`, `egraph` tests | 3-5 sites |

**Connection to the ICE cluster**: the 93 `on_disk_cache.rs:663` panics are
the exact pathology of an incremental-rustc AttrId decoder failing on a
side-effect cache entry. That cache is re-populated per derive-macro
expansion; when a derive site re-emits a differently-shaped TokenStream
between runs, the cached AttrId space shifts, and the decoder can panic
mid-load. `bbnf_derive` at ~87 sites is the overwhelming contributor.

**Known mitigations** (none adopted in the fleet yet):
1. `cargo clean` between bbnf_derive edits — slow but deterministic
2. `CARGO_INCREMENTAL=0` on CI — slow, but removes the decoder path entirely
3. Pin to a known-clean nightly (per §B.2)
4. Reduce derive-emission variance: emit stable symbol names that don't
   migrate between incremental runs. This is an architectural ask on
   `bbnf_derive` itself.

### B.5 CI matrix divergence

| repo | CI file | rust | nextest | bench in CI | close gate |
|---|---|---|---|---|---|
| bbnf-lang | `.github/workflows/ci.yml` | `@nightly` | **N** (workspace-level nextest.toml exists but CI uses `cargo test --workspace`) | **N** | sonic-rs + lightningcss parity `--release` |
| parse-that | `.github/workflows/ci.yml` | `@nightly` | N | N | `--features diagnostics` second pass |
| pprint | `.github/workflows/ci.yml` | `@nightly` | N | N | none |
| gorgeous (sibling) | `.github/workflows/ci.yml` | `@nightly` | N | N | sed-rewritten path-patch (**broken**) |
| csp-solver (sibling) | `.github/workflows/deploy.yml.disabled` | **disabled entirely** | — | — | — |

**Divergences**:
- bbnf-lang has a workspace `nextest.toml` with freezing guards (Tranche
  Y.-1.c) but CI uses `cargo test --workspace` — the guards are off in the
  most important place they could be on. `Makefile` has conditional nextest
  invocation; CI should adopt it.
- Only bbnf-lang runs a release-profile parity gate (sonic-rs, lightningcss).
  Every other repo's CI runs debug-only, so a perf regression only shows
  up when code lands in bbnf-lang.
- csp-solver has no active Rust CI at all. Regressions to the CSP
  substrate — which `bbnf-ir` + `egraph` both rely on per
  `feedback_csp_always_optimize` — land invisibly in the upstream until
  bbnf-lang pulls.
- gorgeous sibling's CI is broken (stale sed-rewrite paths). Needs to be
  fixed or the repo retired.

### B.6 Cross-repo path-patching analysis

The workspace `.cargo/config.toml` defines 11 `[patch.crates-io]` entries:

```toml
pprint             = { path = "../pprint/rust" }
pprint_derive      = { path = "../pprint/rust/derive" }
parse_that         = { path = "../parse-that/rust/parse_that" }
bbnf-regex         = { path = "../parse-that/rust/regex" }
gorgeous           = { path = "crates/gorgeous" }
bbnf               = { path = "crates/core" }
bbnf-ir            = { path = "crates/ir" }
bbnf_derive        = { path = "crates/derive" }
csp-solver         = { path = "crates/csp-solver" }
egraph             = { path = "crates/egraph" }
egraph-derive      = { path = "crates/egraph-derive" }
```

**What it buys**: any change in the sibling repos (pprint, parse-that) shows
up in bbnf-lang immediately on next `cargo check`. Avoids the publish-cadence
lag between a parse-that regex rewrite and a bbnf-lang consumer. Also makes
the reverse patch (parse-that's `.cargo/config.toml` pointing back to
bbnf-lang) coherent: the closed loop enables cross-repo edits to validate
in both directions without a crates.io round-trip.

**What it costs**:
1. **Divergence hazard**: parse-that CI uses crates.io versions of the
   bbnf-lang crates, while local development uses path-patched. A bug that
   only manifests under the path-patched edge exists invisibly until
   someone runs `cargo test` locally. Inverse also true.
2. **Version-pin laundering**: the `path = "..."` form overrides `version =
   "0.2"` in the regular `[dependencies]` table. Bumping a sibling crate's
   major version has no effect on consumers — they silently keep using
   the path-patched HEAD. This makes semver contracts non-binding
   inside the appurtenant surface.
3. **Fragility**: gorgeous sibling's `.cargo/config.toml` is the worst case
   — stale paths that no longer exist, masked by CI's sed-rewrite that
   also references dead paths. Path-patching requires active maintenance;
   without it, repos silently fall back to crates.io and lose every recent
   change.
4. **.gitignore paradox**: parse-that + gorgeous `.cargo/config.toml` files
   are gitignored (per their headers) while bbnf-lang's is tracked. That
   means identical developer intent (reach sibling repo) manifests as
   untracked state in two places and tracked state in one — an isomorphism
   failure that directly enables the gorgeous staleness.

**Recommendation**: track every `.cargo/config.toml` in every appurtenant
repo (bbnf-lang's is already tracked; parse-that + gorgeous + pprint must
follow). Centralise the 11-entry patch table as the source of truth and
derive every sibling's patch section from it. CI in every repo should run
at least one integration test against the path-patched tree so the
version-pin-laundering hazard is visible.

### B.7 Feedback-memory cross-checks

**`isomorphic-api`** (Python ⟷ Rust signature parity):
- parse-that: TypeScript sibling exists at `typescript/`. Go-parity is
  mechanical (same combinator names). Not checked for drift per scope.
- csp-solver: PyO3 + WASM bindings both exist. WASM subcrate at `wasm/`
  follows `wasm-subcrate-pattern` exactly (cdylib+rlib, workspace member,
  wasm-pack metadata for `wasm-opt = -Oz`). Python binding via `pyproject.toml`.
  Drift not checked per scope.

**`wasm-subcrate-pattern`**:
- csp-solver sibling: **compliant** (`wasm/` is cdylib+rlib, workspace
  member, path-dep of parent)
- bbnf-lang: `wasm/` lives at `/Users/mkbabb/Programming/bbnf-lang/wasm`
  (excluded from workspace per `Cargo.toml` `exclude = ["wasm"]`). That
  is a deliberate pattern divergence; the comment is silent on the
  rationale. Compliant with the pattern-as-cdylib but diverges on
  workspace-membership.
- Other repos: no wasm bindings (pprint, gorgeous, parse-that lacks one too)

**`regex-crate-isomorphic`**:
- `parse-that/rust/regex/` (`bbnf-regex`) uses `egraph` + `egraph-derive` +
  `csp-solver` as dependencies — the same substrate as `bbnf-ir`. **Compliant.**

---

## Part C — Fleet modernization blocking signals

**Blocker 1 (critical)**: The 93-ICE cluster in bbnf-lang + 1 ICE in
parse-that. The `on_disk_cache.rs:663 — cannot decode AttrId with
CacheDecoder` panic is deterministic on the observed nightly
(`9602bda1d 2026-04-05`) and triggered by derive-macro re-expansion in
incremental builds. Until a rust-toolchain pin is adopted fleet-wide,
every derive-edit carries ICE risk. **Divan migration cannot land safely
while this cluster is active** — the bench files themselves are re-expanded
and re-incrementally-cached on every bench-edit, compounding the failure.

**Blocker 2 (architectural)**: gorgeous sibling's broken CI + stale path
patches. If fleet modernization intends to uniformly land divan/nextest/
rust-toolchain in every sibling, that repo either needs its path-patch
discipline re-established or retirement — continuing to modernize a
broken mirror wastes cycles.

**Blocker 3 (governance)**: sibling-vs-vendored csp-solver. The workspace
copy at `crates/csp-solver` lacks the 6 criterion benches that exist in
the sibling. A divan migration of "csp-solver" must pick which copy is
authoritative. `feedback_csp_always_optimize` flags this as a foundational
library and contradicts running without a bench-gate at all, which is
the current bbnf-lang state.

**Hidden blocker**: bbnf-lang's `iter-check` alias excludes 4 crates
(`gorgeous`, `bbnf-bootstrap`, `bbnf-analysis`, `bbnf-lsp`) from the
compile-gate. Modernization passes that iterate on those crates will
discover divan-migration breakage only at workspace-test time. If fleet
modernization commits to a rust-toolchain pin and eliminates the
derive-expansion ICE, `iter-check` should be progressively re-widened to
recover compile-gate coverage of those crates.

**Parallel blocker**: pprint's inline `[[bench]]` entries rely on the
unstable `test::Bencher` API. Converting them to divan is trivial, but
during the conversion window any MSRV 1.85 consumer would see a
test-feature regression. Resolve by dropping the MSRV claim in pprint
before divan lands there.

---

## Part D — Summary dashboard

**Repos requiring divan migration**: 6 of 16 have benches (core, parse-that
parse_that/regex/bootstrap, gorgeous sibling, csp-solver sibling+morph-core,
json-prototype) + 3 with unstable `test::Bencher` (pprint, tape, simd-scan)
= **9 of 16 repos** carry bench-framework modernization work.

**Repos with zero modernization surface**: ser, derive, egraph-derive
(proc-macro crates and the minimal numeric-format crate).

**Repos architecturally deprecated / in flux**: analysis (consolidation
glide-path), gorgeous sibling (path-patch staleness suggests effective
dead-mirror), csp-solver sibling (vendoring drift).

**Critical path for fleet modernization**:
1. Pin rust-toolchain across all 5 active git repos (bbnf-lang, parse-that,
   pprint, gorgeous, csc411).
2. Clear the 93-ICE backlog via `cargo clean` on bbnf-lang + CARGO_INCREMENTAL
   audit.
3. Resolve gorgeous sibling (either re-track its `.cargo/config.toml` with
   correct paths, or retire in favour of `crates/gorgeous`).
4. Decide csp-solver authoritativeness (vendored-in-workspace vs.
   sibling-with-CI).
5. Divan migration on the 32-bench heavy axis (core + parse-that parse_that),
   8-bench criterion axis (csp-solver), and 4-bench test::Bencher axis
   (pprint, tape, simd-scan).
6. Track every `.cargo/config.toml` across the fleet; install
   `.config/nextest.toml` in every repo; re-enable csp-solver's CI.
