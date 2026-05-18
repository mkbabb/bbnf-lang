# SK-V8 W0 Hardening V3 CH4 Review

Reviewed commit: `61d5d30407d96ed176cc59e410f7884e30ed30ba`
(`fix(sk-v8-wave0): fold hardening V2 gate blockers`).

Lens: Criterion metadata capture coherence before `--update-results`,
check-only/update semantics, volatile probe write rejection, RESULTS
reproducibility, advisory versus blocking behavior, and stable `run_id`.

## Verdict

VERDICT REJECT.

Confidence: 93%.

## Material Findings

1. Blocking: de-rendered volatile probe estimates still participate in the
   committed `run_id`.

   The fold correctly rejects `--include-volatile-probes` with
   `--update-results` / `--write-results` (`skinny/crates/bbnf-bench/src/bin/gate.rs:20`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:29`). However `RunFacts::probe`
   builds every row id from `criterion_fingerprint(criterion_root)`
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:373`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:379`), and that fingerprint
   recursively includes every file named `estimates.json` or `metadata.toml`
   under the full Criterion root (`skinny/crates/bbnf-bench/src/bin/gate.rs:662`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:678`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:691`). The full JSON benchmark
   always creates probe Criterion groups after each fixture
   (`skinny/crates/bbnf-bench/benches/json_parity.rs:353`,
   `skinny/crates/bbnf-bench/benches/json_parity.rs:381`), but those probe rows
   are rendered only when `include_volatile_probes` is true
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:1373`).

   Dynamic check: copying `/tmp/skv8-w0-target/criterion`, changing only
   `json_probes_twitter/host_call_dispatch_overhead/new/estimates.json`, and
   running
   `CARGO_TARGET_DIR=/tmp/skv8-w0-probehash.30872 RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`
   failed with `RESULTS.md is stale`; status was `1` through `xtask`. No main
   row estimate or rendered probe section changed. This means a volatile,
   de-rendered probe can churn every rendered `Run id` cell
   (`skinny/crates/bbnf-bench/src/report.rs:562`,
   `skinny/crates/bbnf-bench/src/report.rs:574`; current example:
   `skinny/RESULTS.md:48`). That violates the CH4 requirement that volatile
   probes cannot perturb committed W0 evidence and that `run_id` be stable for
   the validated W0 row set.

2. Blocking: SIMD Criterion metadata is loaded after the new capture-coherence
   validator and is not semantically bound before update.

   The main-row metadata validator is materially improved: `gate-json` reads
   main fixture metadata fallibly, validates fixture hash/bytes, rejects mixed
   capture fields, and requires all expected benchmark specs before rendering
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:50`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1013`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1023`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1031`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1043`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1045`). But the SIMD scan
   metadata row is read and appended only after that validator returns
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:60`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:61`), and
   `read_simd_metadata_row` silently maps read or TOML errors to `None`
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:1312`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1320`). The SIMD bench does emit
   full `RowMetadata` including fixture hash/bytes and capture fields
   (`skinny/crates/bbnf-bench/benches/simd_scan.rs:16`,
   `skinny/crates/bbnf-bench/benches/simd_scan.rs:42`,
   `skinny/crates/bbnf-bench/benches/simd_scan.rs:49`), but W0 never compares
   those fields to the main capture before the write path.

   Dynamic check: copying the Criterion root, corrupting only
   `simd_structural_scan/canada_simd/metadata.toml`'s `bbnf_commit`, and running
   `CARGO_TARGET_DIR=/tmp/skv8-w0-badsimd.6107 RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`
   failed as a stale RESULTS diff, not as `metadata invalid`; status was `1`
   through `xtask`. Because the write path validates first, then writes when
   `update_results` is true (`skinny/crates/bbnf-bench/src/bin/gate.rs:308`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:319`), this is still a promotion
   gap for `--update-results`: mismatched SIMD capture metadata can affect the
   report/run id and Canada SIMD outcome path without the new capture validator
   rejecting the root.

## Accepted Evidence

- Check-only/update semantics are substantially fixed. The gate validates before
  mutation, writes only on explicit `--update-results` / `--write-results`, and
  otherwise compares the render against checked-in `RESULTS.md`
  (`skinny/crates/bbnf-bench/src/bin/gate.rs:308`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:319`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:321`).
- Validation failure no longer rewrites `RESULTS.md`; the failure branch renders
  with a validation note and exits invalid before the write branch
  (`skinny/crates/bbnf-bench/src/bin/gate.rs:308`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:315`).
- Advisory versus blocking behavior matches the current W0 posture. Advisory
  mode blocks only hard evidence failures `I/J/K`, while non-advisory mode exits
  on the worst current outcome (`skinny/crates/bbnf-bench/src/bin/gate.rs:282`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:330`). My non-advisory
  `gate-json --check-results` run failed through `xtask` because the rendered
  report remains `N-direct / NoGo`, while the advisory check passed on the
  committed W0 target.
- Main-row Criterion metadata coherence is now enforced. A temp-root negative
  with `json_twitter/track2_handcoded/metadata.toml` `bbnf_commit` changed to
  `corrupted-capture` failed with
  `twitter metadata invalid: bbnf_bench::track2::json::parse metadata is from a mixed Criterion capture`.
- Path-only run-id volatility is fixed. Copying the same Criterion root to
  `/tmp/skv8-w0-copy.KdZ3WS` and rerunning
  `CARGO_TARGET_DIR=/tmp/skv8-w0-copy.KdZ3WS RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`
  exited `0`.

## Commands Run

- `cargo test -p bbnf-bench`: passed 49 library tests, 6 gate-bin tests, and doc
  tests.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`: passed and matched committed
  `skinny/RESULTS.md`.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --update-results --include-volatile-probes --advisory`: failed
  before report generation with the expected incompatible-flags error.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --check-results`: failed through `xtask` because the non-advisory
  gate preserves blocking `N-direct / NoGo` behavior.
- Copied-root reproducibility, bad main metadata, bad probe estimate, and bad
  SIMD metadata checks as described above.

## Required Fold Items

1. Restrict `criterion_fingerprint` to validated W0 main-row inputs, or build a
   separate stable capture id from the validated 38-row manifest fields. Exclude
   `json_probes_*`, `base/`, unrelated later-wave Criterion groups, and any other
   non-rendered or non-validated files from the committed `run_id`.
2. Include SIMD scan metadata in the same capture-coherence validator, with
   explicit fixture hash/bytes, host/profile/rustflags/target CPU/sample policy,
   `bbnf_commit`, workload/track/materialisation, and required hash semantics.
   Missing or malformed SIMD metadata should be a metadata error, not an
   optional row that later becomes a stale-render diff.
3. Add negative tests or scripted gate checks for both blockers: mutate only a
   de-rendered probe estimate and prove `run_id` is unchanged; mutate SIMD
   metadata capture fields and prove `gate-json` rejects before any update path.
