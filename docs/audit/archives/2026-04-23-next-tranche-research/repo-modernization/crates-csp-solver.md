# crates/csp-solver (workspace copy) — Modernization Plan

## Role in the fleet
Vendored from the sibling csc411 repo per Tranche AA.2. Provides the
generalized CSP/COP substrate used by both `bbnf-ir` (grammar pass scheduling)
and `egraph` (saturation scheduling per `feedback_csp_always_optimize`).
Path-patched into bbnf-lang. Currently **missing all 6 sibling benches** —
the single biggest architectural gap in the fleet per
`feedback_csp_always_optimize`.

## Current posture (from Wave 1-B assay)
- Workspace member. lib (`crate-type = ["lib"]`).
- 2 features: `default = []`, `py = []` ("not built by the bbnf-lang
  workspace. Present for symmetry with the upstream csc411 repo").
- **No benches** in this copy (sibling has 6: `sudoku`, `queens`,
  `map_coloring`, `lattice`, `assignment`, `cost_finite_domain`).
- No `[dependencies]` declared (the `py` feature has no `dep:pyo3` because
  PyO3 isn't in the workspace; subtle deviation from sibling's `py =
  ["dep:pyo3"]`).
- **No `[dev-dependencies]` — no criterion.**
- Inherits workspace toolchain + `.cargo/config.toml` + `.config/nextest.toml`.
- Included in `iter-test-leaf` alias.
- No scripts here (sibling has `scripts/bench-compare.sh`).
- No proc-macro.
- `feedback_csp_always_optimize` flags it as foundational; lacking a bench
  gate is a contradiction.

## Target posture
- Inherits fleet pin.
- **Imports all 6 sibling benches** — the sibling-vs-vendored reconciliation
  decision is "import sibling benches to the workspace copy, preserving the
  sibling repo's criterion sources but migrating the workspace copy directly
  to divan."
- Optionally: make the workspace copy the authoritative source; mark sibling
  as the "standalone Python/WASM binding host" and eliminate bench surface
  there. This is the cleanest reconciliation; see `INDEX.md §7`.
- Inherits nextest.
- No `py` feature change in the workspace copy (stays `py = []` no-op;
  PyO3 lives only in the sibling).

## Gap — what must change
1. Inherit workspace pin (0 min).
2. Import the 6 sibling bench sources (`sudoku.rs`, `queens.rs`,
   `map_coloring.rs`, `lattice.rs`, `assignment.rs`, `cost_finite_domain.rs`)
   into `crates/csp-solver/benches/` (30 min).
3. Migrate each bench from criterion to divan directly (skip criterion
   adoption step; the workspace copy has no criterion history) (3 hours).
4. Add `[dev-dependencies]` with `divan` (5 min).
5. Add 6 `[[bench]]` entries to `Cargo.toml` (10 min).
6. Verify the benches compile and execute under the path-patched workspace
   (30 min).

**Total**: ~4 hours.

## Sequencing — when this repo lands
- **Phase A (during bbnf-lang B1)**: item 1 (automatic).
- **Phase B (post-B1, before BA)**: items 2, 3, 4, 5, 6. The sibling's
  criterion migration happens in parallel; the workspace copy migrates
  directly to divan.
- **Phase C**: nothing.

## Dependencies
- **Upstream blockers**: B1 pin; divan exemplar from core (B1 Step 5);
  csp-solver-sibling reconciliation decision.
- **Downstream blocks**: `feedback_csp_always_optimize` demands this bench
  gate exist. `bbnf-ir` + `egraph` both rely on the substrate; regressions
  here silently propagate.
- **B1 coupling**: Step 1 indirectly. Main work is post-B1.

## Risks
- **Sibling-vs-vendored drift**: if the sibling fork continues to accrete
  benches or changes, the workspace copy falls behind. Governance:
  canonicalise a `cargo vendor`-like sync discipline OR declare the
  workspace copy authoritative (recommended; see `INDEX.md §7`).
- Sibling's criterion benches use `criterion::Criterion::default()`
  configuration that does not map 1:1 to divan. Mechanical port is non-trivial.
- The 6 bench corpus references stable CSP problem instances (sudoku,
  queens, etc.) — corpus stability is fine, but algorithmic changes to the
  substrate may shift absolute numbers. Establish baseline on pinned
  nightly first.

## Verification
```bash
cd bbnf-lang
cargo iter-test-leaf                              # csp-solver included
cargo bench -p csp-solver --bench sudoku          # divan JSON
cargo bench -p csp-solver --bench queens          # divan JSON
# ... all 6 benches emit JSON
cargo bench -p csp-solver                         # all 6 in one invocation
```

## Specific changes (patch-ready)
- `crates/csp-solver/Cargo.toml`:
  ```toml
  [dev-dependencies]
  divan = "0.1"

  [[bench]]
  name    = "sudoku"
  harness = false

  [[bench]]
  name    = "queens"
  harness = false

  [[bench]]
  name    = "map_coloring"
  harness = false

  [[bench]]
  name    = "lattice"
  harness = false

  [[bench]]
  name    = "assignment"
  harness = false

  [[bench]]
  name    = "cost_finite_domain"
  harness = false
  ```
- 6 bench files under `crates/csp-solver/benches/` — ported from sibling,
  direct criterion → divan migration.
