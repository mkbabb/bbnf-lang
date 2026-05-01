# csp-solver (sibling, at csc411) — Modernization Plan

## Role in the fleet
Upstream of the vendored `crates/csp-solver`. Lives inside
`/Users/mkbabb/Programming/csc411/CSC411_HW2_ProgrammingQuestion/csp-solver`;
not a separate git root (parent `CSC411_HW2_ProgrammingQuestion/` is the
git root). Hosts the CSP/COP substrate plus its PyO3 binding, WASM sub-crate
(`wasm/`), and a `wasm-morph/` + `morph-core/` twin (separate substrate for
morph-compare experiment, distinct concern from bbnf-lang's grammar-CSP
consumer). Exemplar of `feedback_wasm_subcrate_pattern` — wasm bindings as a
workspace-member cdylib inside the parent. Also exemplar of
`feedback_isomorphic_api` — PyO3 + WASM bindings sharing the Rust API.

## Current posture (from Wave 1-B assay)
- Parent repo HEAD `b7009867` on `master`; 302 commits. Uncommitted changes
  exist but only in `web/frontend/node_modules/*` (deletions + `.package-lock`
  mod). No uncommitted changes inside `csp-solver/`.
- `csp-solver`: lib with features `default = []`, `py = ["dep:pyo3"]`.
  `crate-type = ["lib"]`; cdylib added by maturin via `pyproject.toml`.
- **6 `[[bench]]` entries**: `sudoku`, `queens`, `map_coloring`, `lattice`,
  `assignment`, `cost_finite_domain`. All `harness = false`, **`criterion =
  "0.5"`** (the ONLY criterion consumer in the fleet).
- `morph-core`: lib, publish=false, **2 criterion benches** (`approx`,
  `criterion`, `proptest`).
- `wasm/`: cdylib+rlib; 0 benches; carries
  `[package.metadata.wasm-pack.profile.release]` for `wasm-opt = ["-Oz"]`.
- `wasm-morph/`: parallel cdylib binding for morph-core.
- `rust-toolchain.toml` **absent**; edition 2024; MSRV not declared.
- `.cargo/config.toml` **absent** at both parent and crate level. No aliases,
  no profile overrides, no path-patches (unlike other siblings).
- CI: `.github/workflows/deploy.yml.disabled` — **disabled** (the `.disabled`
  suffix is load-bearing). Covers Python backend + frontend build; comment
  notes production deploy behind VPN and Rust tests are not exercised.
- `scripts/bench-compare.sh`: `git worktree add` twin-ref baseline comparison
  using criterion's `--baseline`. Exactly the pattern divan's built-in
  `--baseline` + cargo bench alias + samply should replace.
- Proc-macro: neither defined nor consumed.
- Zero ICE files.

## Target posture
- Pinned nightly identical to bbnf-lang.
- **Criterion removed, divan adopted** on all 8 benches (6 csp-solver + 2
  morph-core). This is the only criterion abrogation in the fleet.
- `.cargo/config.toml` installed at `csp-solver/` crate level with patch table
  reaching back to bbnf-lang (mirror of the reverse direction bbnf-lang
  already has).
- `.config/nextest.toml` installed.
- CI **re-enabled** on a new workflow (`rust-ci.yml` or similar) that is
  scoped narrowly to the `csp-solver/` sub-tree — independent of the
  deploy.yml.disabled which is production-facing.
- `scripts/bench-compare.sh` rewritten around divan's native `--baseline`
  + samply; eliminate the `git worktree add` dance.
- Python + WASM API isomorphism preserved (`feedback_isomorphic_api`) — no
  public API changes.

## Gap — what must change
1. Install `rust-toolchain.toml` at `csp-solver/` crate level (3 min).
2. Install `csp-solver/.cargo/config.toml` with `[patch.crates-io]` pointing
   back to bbnf-lang (`pprint`, `bbnf-ser`, etc. — same 11-entry table) and
   `[alias]` + `[profile.ax-iter]` block (45 min).
3. Port 6 csp-solver benches + 2 morph-core benches from criterion to divan
   (4–5 hours; criterion shape is more invasive than bencher).
4. Rewrite `scripts/bench-compare.sh` to use divan baselines + samply; delete
   the git-worktree comparison dance (1.5 hours).
5. Install `.config/nextest.toml` (15 min).
6. Re-enable Rust CI via new `.github/workflows/rust-ci.yml` scoped to
   `csp-solver/`; do NOT touch the deploy.yml.disabled (1 hour).
7. Gitignore `web/frontend/node_modules/` (5 min).
8. Verify PyO3 `py` feature still binds after divan migration (30 min).
9. Verify WASM subcrate compiles post-pin (30 min).

**Total**: ~9–10 hours. Non-trivial; criterion → divan is the heaviest
single-repo port in the fleet.

## Sequencing — when this repo lands
- **Phase A (during bbnf-lang B1)**: items 1, 2, 5 (~1 hour). Needed for the
  bbnf-lang path-patch to resolve deterministically against pinned nightly.
- **Phase B (post-B1, before BA)**: items 3, 4, 6, 7 (~7–8 hours). Criterion
  migration is the largest work-item in this repo; can happen in parallel
  with bbnf-lang's divan port.
- **Phase C**: items 8, 9 (~1 hour). Binding-surface validation.

## Dependencies
- **Upstream blockers**: bbnf-lang B1 Step 1 (pin draft).
- **Downstream blocks**: `crates/csp-solver` vendoring reconciliation (see
  `INDEX.md`). The sibling's 6 benches currently have no counterpart in the
  workspace copy; `feedback_csp_always_optimize` flags this as foundational
  — the workspace **must** gain those benches regardless of vendoring
  direction.
- **B1 coupling**: Step 11 propagation.

## Risks
- **Criterion → divan shape mismatch**: criterion's `bench_function` +
  `Criterion::setup` idiom does not map 1:1 to divan's explicit sample-body
  split. The 8 benches carry criterion-specific `Criterion::default()`
  configuration that must be re-expressed as divan attributes.
- **CI re-enable risk**: the deploy.yml.disabled pattern exists for a reason
  (VPN gating). New `rust-ci.yml` must avoid accidentally re-enabling the
  deploy surface.
- **PyO3/WASM API isomorphism**: criterion removal is `[dev-dependencies]`
  only, so no public-API impact — but confirm during Phase C.
- **`feedback_csp_always_optimize`**: this repo is foundational; its bench
  gate must be active for the fleet. Migration priority is HIGH.
- **Parent-repo gitignore noise**: `web/frontend/node_modules` churn is
  unrelated to Rust modernization but confuses any git-status check.
- **Criterion's HTML reports** disappear when divan replaces it — downstream
  consumers of the HTML are lost. Audit who reads those reports.

## Verification
```bash
cd /Users/mkbabb/Programming/csc411/CSC411_HW2_ProgrammingQuestion/csp-solver
rustc --version                         # pinned nightly
cargo iter-check                        # resolves patch table
cargo nextest run                       # all tests pass
cargo bench --bench sudoku              # divan JSON
cargo bench --bench queens              # divan JSON (all 6)
cd morph-core && cargo bench            # divan JSON (both benches)
./scripts/bench-compare.sh HEAD~5 HEAD  # divan-based baseline diff
cd ../../../bbnf-lang && cargo iter-check  # path-patch resolves
```

## Specific changes (patch-ready)
- `csp-solver/rust-toolchain.toml` — identical to bbnf-lang's draft.
- `csp-solver/.cargo/config.toml` — new file:
  ```toml
  [build]
  rustflags = ["-Zthreads=8", "-Zshare-generics=y"]

  [patch.crates-io]
  pprint        = { path = "../../../pprint/rust" }
  pprint_derive = { path = "../../../pprint/rust/derive" }

  [profile.ax-iter]
  inherits = "dev"; opt-level = 0; debug = "line-tables-only"
  incremental = true; codegen-units = 256

  [alias]
  iter-check = "check --profile ax-iter"
  iter-test  = "nextest run --cargo-profile ax-iter"
  ```
- `csp-solver/Cargo.toml` `[dev-dependencies]`: drop `criterion`, add `divan`.
- `csp-solver/morph-core/Cargo.toml` `[dev-dependencies]`: drop `criterion`,
  add `divan`.
- All 8 bench files: rewrite criterion boilerplate to divan.
- `scripts/bench-compare.sh`: rewrite to divan `--baseline` semantics.
- `.github/workflows/rust-ci.yml` — new workflow; `taiki-e/install-action@nextest`;
  `cargo nextest run --workspace` in `csp-solver/` subtree.
- `.gitignore`: `web/frontend/node_modules/`.
