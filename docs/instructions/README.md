# Instructions

Read `docs/precepts/instructions/` first. Local bbnf-lang rules:

- Rust toolchain is pinned by `rust-toolchain.toml`; do not change nightly,
  components, or workspace resolver policy as a side effect of tranche work.
- Cargo surfaces are local: `.cargo/config.toml`, `ax-iter`, `ay-final`,
  `profiling-prep`, `bench`, and `bench-ci`. Run one cargo command per
  `CARGO_TARGET_DIR`; concurrent cargo in the same target directory is
  forbidden.
- Testing is nextest-first. Use `cargo iter-check` and
  `scripts/test-tier.sh leaf|grammar|workspace` for iteration. Leaf tests
  include `crates/csp-solver`.
- Bootstrap regeneration is local substrate. Use `cargo xtask regen`,
  `cargo xtask regen --grammar <name>`, and `cargo xtask regen --check`.
  Generated output under `crates/core/src/grammar/generated/<ident>.rs` is
  never hand-edited.
- Profiling and benchmarks stay in `docs/instructions/PROFILING.md`: Divan
  cold-per-parse runs, samply captures, `.profiles/`, and the profile helper
  scripts.
- Architecture invariants are bbnf-local: one codegen path, one regex HIR,
  typed materialisation totality, grammar-derived emitted data, and
  end-to-end wire-contract tests.
- Workspace crates are owned and modifiable within declared tranche scope.
  Sibling repos `../parse-that` and `../pprint` are also owned when a tranche
  explicitly names them.
- `crates/csp-solver` is the local CSP/COP substrate. It inherits shared
  precepts through the bbnf-lang top-level `docs/precepts`; see
  `crates/csp-solver/docs/instructions/README.md` for its local note.
