# SK-V8 W0 Hardening V1 CH4 Review

## Decision

REJECT.

Acceptance probability: 61%.

## Blocking Findings

1. `gate-json` is still a mutating command, including on validation failure. `skinny/crates/bbnf-bench/src/bin/gate.rs:281-292` validates the freshly rendered report and then writes `RESULTS.md` both on the failure path and the success path. `skinny/xtask/src/main.rs:241-250` exposes that behavior through `cargo xtask gate-json` with no explicit write flag, so a user can run what looks like a gate check and clobber the checked-in W0 report from stale, missing, or different Criterion data. This is a W0 reproducibility blocker because the command is not check-only by default and can rewrite the evidence artifact before returning an invalid exit.

2. The rendered W0 run id is path-volatile. `skinny/crates/bbnf-bench/src/bin/gate.rs:333-342` constructs `run_id` from the current git short hash plus `criterion_root.display()`, and `criterion_root` depends on `CARGO_TARGET_DIR` or the workspace target path at `skinny/crates/bbnf-bench/src/bin/gate.rs:22-25`. The committed report therefore embeds `/tmp/skv8-w0-target/criterion` in every W0 manifest row, for example `skinny/RESULTS.md:48`. A rerun from the same Criterion data under a different temp path or local target directory produces a report diff unrelated to benchmark content.

3. Probe/RSS output remains volatile but is rendered into `RESULTS.md` outside the W0 baseline validator. `skinny/crates/bbnf-bench/src/bin/gate.rs:949-990` renders masking probe rows from Criterion slopes, while `skinny/crates/bbnf-bench/src/bin/gate.rs:778-817` runs RSS subprocess probes and buckets the notes. `skinny/crates/bbnf-bench/src/report.rs:489-515` validates only the 38 main W0 rows against `SK_V8_OPEN_BASELINE`; the probe table and RSS notes are rendered later at `skinny/crates/bbnf-bench/src/report.rs:595-617` without a stability gate. Current `RESULTS.md` includes those volatile sections at `skinny/RESULTS.md:87-192` and `skinny/RESULTS.md:199-261`, so reruns can churn committed evidence while still passing W0 validation.

4. The gate can relabel stale Criterion metadata as the current run. `skinny/crates/bbnf-bench/src/bin/gate.rs:864-885` silently loads whatever metadata exists under the selected Criterion root; `skinny/crates/bbnf-bench/src/bin/gate.rs:374-410` consumes only selected metadata fields for display; and `skinny/crates/bbnf-bench/src/bin/gate.rs:333-342` stamps the rendered rows with the current git/path run id. There is no W0 check that every loaded `RowMetadata::bbnf_commit`, build profile, rustflags, target CPU, host, sample policy, and input hash belongs to the same committed `SK-V8-open` capture. This leaves a stale-target blind spot: old target data can be presented as a fresh W0 run if the throughput numbers fit the baked baseline tolerance.

## Nonblocking Findings

- RSS is at least bucketed to 2 MiB in `skinny/crates/bbnf-bench/src/bin/gate.rs:813-817`, which reduces but does not eliminate churn.
- Historical C++ sidecar values are correctly marked as historical or absent in the manifest, and `skinny/crates/bbnf-bench/src/gate.rs:133-169` rejects stale sidecars for strict admission. That boundary looks directionally sound for W0.
- The Lock 14 allowlist is gate-consumed and checks duplicate paths, unsupported classes, unsupported mutability, forbidden names, and missing files in `skinny/crates/bbnf-bench/src/lock14_baseline.rs:293-327`; it is not the source of this rejection.

## Evidence Inspected

- Commit `6d8cb70138a73e87252aab5e0ea712390801a6a0` (`feat(sk-v8-wave0): enforce telemetry manifest gate`) and its touched files.
- Current tree at `HEAD` for `skinny/crates/bbnf-bench/src/bin/gate.rs`, `skinny/crates/bbnf-bench/src/gate.rs`, `skinny/crates/bbnf-bench/src/report.rs`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs`, `skinny/xtask/src/main.rs`, and `skinny/RESULTS.md`.
- I did not run `cargo xtask gate-json` because the reviewed implementation can rewrite `skinny/RESULTS.md` as part of the command under review.

## Exact Remediation If Rejected

1. Split gate rendering from report mutation. Make `cargo xtask gate-json` check-only by default: render to memory or a temp path, validate, compare against `skinny/RESULTS.md`, and fail with a diff if the checked-in report is stale. Require an explicit flag such as `--write-results` or `--update-results` for mutation, and never write `RESULTS.md` on validation failure.
2. Stabilize run identity. Remove absolute `criterion_root.display()` from rendered `run_id`; use a deterministic capture id from commit, normalized host/build fingerprint, and a content hash of the Criterion/report inputs. Keep local filesystem paths out of committed report rows or put them in non-rendered diagnostics.
3. Gate or de-render volatile probe/RSS evidence. Either exclude masking probes and RSS notes from the committed W0 report by default, or add explicit baselines/tolerances and validation for the rendered probe/RSS sections before any write is allowed.
4. Bind Criterion metadata to the capture. Validate that every loaded metadata row has the expected commit, input SHA, rustflags/target CPU, host identity, sample size, measurement time, and profile before rendering `SK-V8-open`. Reject mixed or stale roots instead of stamping them with the current run id.
