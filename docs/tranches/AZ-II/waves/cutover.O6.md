# AZ-II.cutover.O6 — Semantic and Performance Close
**Opens after**: AZ-II.cutover.O5 close
**Agents**: up to 10 parallel
**Hard gate**: JSON sonic-rs parity, CSS lightningcss parity, and the 17-entry post-AZ-II close matrix are refreshed on the tape-free StructDirect path.
**Status**: planned

## Scope

1. Refresh semantic parity against sonic-rs and lightningcss after tape deletion.
2. Compile every close-matrix bench target through the O0-repaired feature surfaces.
3. Run the 17-entry close matrix sequentially, with no parallel bench invocations.
4. Archive `docs/benchmarks/post-AZ-II.json` with real post-O5 values and no placeholders.
5. Capture symbol/profile evidence needed to keep later optimization work honest.
6. Replace the failed O3a JSON baseline with a post-O5 JSON lane
   measurement and cite the exact delta from the timeout artifact.
7. Update AZ-II progress docs with measured throughput and parity status.

## File bounds

| File | Access |
|---|---|
| `crates/core/tests/sonic_rs_parity.rs` | modify |
| `crates/core/tests/json_{canonical_parity,value_parity,parity_struct}.rs` | modify |
| `crates/core/tests/lightningcss_parity.rs` | modify |
| `crates/core/tests/css_l4_{canonical_parity,color_view,named_color_parity,dimensions}.rs` | modify |
| `crates/core/tests/sheets_{parity,self_parity,expr_parity}.rs` | modify |
| `crates/core/tests/bbnf_{self_parity,ast_parity,parity}.rs` | modify |
| `crates/core/benches/{json_monolithic,css_l4,google_sheets_monolithic,bbnf_monolithic,compile_pipeline}.rs` | modify |
| `crates/core/benches/json/value.rs` | modify |
| `.cargo/config.toml` | modify |
| `Makefile` | modify |
| `scripts/prebuild-benches.sh` | modify |
| `scripts/prepare-profile-wave.sh` | modify |
| `scripts/profile-bench-headless.sh` | modify |
| `docs/benchmarks/post-AZ-II.json` | create |
| `docs/benchmarks/post-AZ-II-O6-*.txt` | create |
| `docs/benchmarks/profiles/AZ-II/cutover/O6/**` | create |
| `docs/tranches/AZ-II/PROGRESS.md` | modify |
| `docs/tranches/AZ-II/waves/cutover.md` | modify |

**Do NOT touch**: emitter source, runtime builders, generated parser files, `crates/tape` deletion state, `Parsed<R>` deletion state. O6 measures and tightens parity; it does not reopen substrate.
Deployment invariant: every sub-agent runs in a sibling
fully-contained worktree seeded with `scripts/seed-worktree.sh`, with
explicit allow/forbidden lists; bench and profile invocations are
sequentially scheduled by the orchestrator even when preparation and
parity repair lanes fan out.

## Phase sub-items

### AZ-II.cutover.O6.1 JSON Sonic-RS Parity

Mechanism: run and, if needed, repair JSON semantic parity tests so they compare concrete `JsonDocument` projections against sonic-rs without tape adapters.

Files touched: `crates/core/tests/sonic_rs_parity.rs`, `crates/core/tests/json_{canonical_parity,value_parity,parity_struct}.rs`.

Sub-gate: `cargo test -p bbnf --profile ax-iter --test sonic_rs_parity -- --nocapture` passes.

### AZ-II.cutover.O6.2 CSS LightningCSS Parity

Mechanism: run and, if needed, repair CSS admission/canonical/color parity tests against lightningcss on the StructDirect document path.

Files touched: `crates/core/tests/lightningcss_parity.rs`, `crates/core/tests/css_l4_{canonical_parity,color_view,named_color_parity,dimensions}.rs`.

Sub-gate: lightningcss parity tests pass or record named semantic gaps in FINAL without claiming parity for them.

### AZ-II.cutover.O6.3 Sheets and BBNF Semantic Smokes

Mechanism: run Sheets self/parity and BBNF self/AST parity on the post-tape path to catch regressions outside JSON/CSS.

Files touched: `crates/core/tests/sheets_{parity,self_parity,expr_parity}.rs`, `crates/core/tests/bbnf_{self_parity,ast_parity,parity}.rs`.

Sub-gate: focused parity suites pass or produce named residuals that block O7.

### AZ-II.cutover.O6.4 Bench Command Surface Final Check

Mechanism: compile all close-matrix bench targets and repair only command/feature surfaces that fail to reach the intended bench binary.

Files touched: `.cargo/config.toml`, `Makefile`, `scripts/prebuild-benches.sh`, `scripts/prepare-profile-wave.sh`, `scripts/profile-bench-headless.sh`.

Sub-gate: every O6 bench target compiles before measurement starts.

### AZ-II.cutover.O6.5 JSON Bench Lane

Mechanism: run JSON close entries sequentially: data, twitter, citm, canada, data_xl, plus json_value if still part of the value API close surface.

Files touched: `crates/core/benches/json_monolithic.rs`, `crates/core/benches/json/value.rs`, `docs/benchmarks/post-AZ-II-O6-json.txt`.

Sub-gate: JSON entries land in `post-AZ-II.json` with fixture names, units, and command provenance.

### AZ-II.cutover.O6.6 CSS Bench Lane

Mechanism: run CSS L4 normalize, bootstrap, and tailwind entries sequentially.

Files touched: `crates/core/benches/css_l4.rs`, `docs/benchmarks/post-AZ-II-O6-css.txt`.

Sub-gate: CSS entries land in `post-AZ-II.json` with no placeholder aborts.

### AZ-II.cutover.O6.7 Sheets Bench Lane

Mechanism: run Sheets parse_simple, parse_nested, and parse_stress entries sequentially.

Files touched: `crates/core/benches/google_sheets_monolithic.rs`, `docs/benchmarks/post-AZ-II-O6-sheets.txt`.

Sub-gate: Sheets entries land in `post-AZ-II.json` with no stale cutover.H placeholders.

### AZ-II.cutover.O6.8 BBNF and Compile Bench Lane

Mechanism: run BBNF grammar fixtures and compile_pipeline entries sequentially.

Files touched: `crates/core/benches/bbnf_monolithic.rs`, `crates/core/benches/compile_pipeline.rs`, `docs/benchmarks/post-AZ-II-O6-bbnf-compile.txt`.

Sub-gate: BBNF self-parse and compile entries land in `post-AZ-II.json`.

### AZ-II.cutover.O6.9 Profile and Symbol Evidence

Mechanism: capture `nm` and samply evidence for the close binaries sufficient to prove no tape symbols are present in hot bench paths.

Files touched: `docs/benchmarks/profiles/AZ-II/cutover/O6/**`, `docs/benchmarks/post-AZ-II-O6-nm.txt`.

Sub-gate: archived symbol scans show tape symbols absent from close bench binaries.

### AZ-II.cutover.O6.10 Benchmark JSON and Progress

Mechanism: assemble the close matrix and update progress docs with exact commands, dates, and residual named gaps.

Files touched: `docs/benchmarks/post-AZ-II.json`, `docs/tranches/AZ-II/PROGRESS.md`, `docs/tranches/AZ-II/waves/cutover.md`.

Sub-gate: `post-AZ-II.json` contains no placeholders and docs name O7 as FINAL conversion.

### AZ-II.cutover.O6.11 O3a Cohort Close Matrix

Mechanism: consume the O3a J1, C1, and S1 triad outputs before
claiming parity. JSON sonic-rs parity, CSS lightningcss parity, Sheets
self-parity, and the `json_monolithic::data_xl` timeout must either be
green on the post-O5 path or block O7 with named residuals and a
committed successor owner.

Files touched: `docs/tranches/AZ-II/audit/O3a-J1-*.md`,
`docs/tranches/AZ-II/audit/O3a-C1-*.md`,
`docs/tranches/AZ-II/audit/O3a-S1-*.md`,
`docs/benchmarks/post-AY-az-ii-doc-baseline-json.txt`,
`docs/benchmarks/post-AZ-II.json`.

Sub-gate: O6 close matrix names the O3a baseline deltas and assigns
every parity or throughput failure to a closed cohort or successor
owner.

## Hard gate

1. `cargo test -p bbnf --profile ax-iter --test sonic_rs_parity -- --nocapture` passes.
2. `cargo test -p bbnf --profile ax-iter --test lightningcss_parity -- --nocapture` passes or blocks O7 with named gaps.
3. All 17 close-matrix bench entries are present in `docs/benchmarks/post-AZ-II.json` with no placeholder values.
4. `docs/benchmarks/post-AZ-II-O6-nm.txt` records tape symbols absent from close bench binaries.
5. `docs/benchmarks/profiles/AZ-II/cutover/O6/**` contains the required profile artifacts for later optimization claims.
6. O3a J1/C1/S1 cohorts are closed or block O7 with named successor
   owners; `json_monolithic::data_xl` no longer has an unexplained
   timeout baseline.

## Verification artefacts

- `/tmp/az-ii-o6-sonic-rs-parity.txt`
- `/tmp/az-ii-o6-lightningcss-parity.txt`
- `/tmp/az-ii-o6-bench-compile.txt`
- `docs/benchmarks/post-AZ-II-O6-json.txt`
- `docs/benchmarks/post-AZ-II-O6-css.txt`
- `docs/benchmarks/post-AZ-II-O6-sheets.txt`
- `docs/benchmarks/post-AZ-II-O6-bbnf-compile.txt`
- `docs/benchmarks/post-AZ-II-O6-nm.txt`
- `docs/benchmarks/profiles/AZ-II/cutover/O6/**`
- `docs/tranches/AZ-II/audit/O3a-{J1,C1,S1}-*.md`
- `docs/benchmarks/post-AZ-II.json`
- O6 close commit hashes recorded in `docs/tranches/AZ-II/PROGRESS.md`.

## Dependencies

- **Depends on**: AZ-II.cutover.O5, O3a J1/C1/S1 cohort dispositions
- **Blocks**: AZ-II.cutover.O7

## Archaeology

AZ-I and AZ-II partial closes carried stale or waived performance evidence. O6 is the first valid post-tape measurement point and must not reuse cutover.E/H placeholder numbers.
