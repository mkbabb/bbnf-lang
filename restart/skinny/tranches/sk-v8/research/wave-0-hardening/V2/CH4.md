# SK-V8 W0 Hardening V2 CH4 Review

## Verdict

REJECT.

Confidence: 84%.

## Scope

Reviewed current HEAD `cb0fdba0` against the W0 V1 CH4 blockers and the ORCHESTRATOR challenge discipline. CH4 is being used here in the W0 hardening sense requested by dispatch: report mutation semantics, check-only versus update behavior, idempotence, stable run IDs, volatile probe/RSS gating, and stale Criterion metadata relabeling. ORCHESTRATOR requires one challenge file per lens and fold-before-advance convergence (`restart/prompts/ORCHESTRATOR.md:74`, `restart/prompts/ORCHESTRATOR.md:110`, `restart/prompts/ORCHESTRATOR.md:120`).

## Findings

1. Blocking: stale and mixed Criterion metadata can still be promoted by the explicit update path. The fold replaces path/HEAD run IDs with `sk-v8-open:criterion-fnv64-*`, but the hash is only an identifier for whatever files are under the selected Criterion root, not a validator that those files are one coherent SK-V8-open capture. `criterion_fingerprint` hashes `estimates.json` and `metadata.toml` bytes (`skinny/crates/bbnf-bench/src/bin/gate.rs:648`-`skinny/crates/bbnf-bench/src/bin/gate.rs:661`), while metadata loading silently accepts whatever rows parse and drops missing/malformed rows (`skinny/crates/bbnf-bench/src/bin/gate.rs:953`-`skinny/crates/bbnf-bench/src/bin/gate.rs:974`). The telemetry builder consumes only selected Track 1 display fields such as profile/rustflags/target CPU, host, feature mask, and sample count (`skinny/crates/bbnf-bench/src/bin/gate.rs:401`-`skinny/crates/bbnf-bench/src/bin/gate.rs:446`), then `--update-results` writes after W0 validation succeeds (`skinny/crates/bbnf-bench/src/bin/gate.rs:304`-`skinny/crates/bbnf-bench/src/bin/gate.rs:307`). The validator only checks `sample_count != 0` and a syntactic `sample_cost` field (`skinny/crates/bbnf-bench/src/report.rs:335`-`skinny/crates/bbnf-bench/src/report.rs:341`); it does not compare `RowMetadata::bbnf_commit`, `input_sha256`, `input_bytes`, sample policy, profile, rustflags, target CPU, host, or measurement time across the loaded rows even though those fields exist (`skinny/crates/bbnf-bench/src/metadata.rs:20`-`skinny/crates/bbnf-bench/src/metadata.rs:64`). A stale or mixed target root can therefore become a new committed `SK-V8-open` manifest if the numeric baselines fit tolerance.

2. Nonblocking but still soft: volatile probe/RSS output is de-rendered by default, which resolves the V1 committed-report churn path for ordinary `gate-json`. The remaining escape hatch is that `--include-volatile-probes` is accepted by `xtask` (`skinny/xtask/src/main.rs:244`-`skinny/xtask/src/main.rs:254`), enables RSS probes and masking probe rows (`skinny/crates/bbnf-bench/src/bin/gate.rs:81`-`skinny/crates/bbnf-bench/src/bin/gate.rs:84`, `skinny/crates/bbnf-bench/src/bin/gate.rs:236`-`skinny/crates/bbnf-bench/src/bin/gate.rs:243`), and can still be combined with `--update-results`. The renderer can emit those sections without a baseline validator (`skinny/crates/bbnf-bench/src/report.rs:598`-`skinny/crates/bbnf-bench/src/report.rs:613`). Because the flag name is explicit and the committed `RESULTS.md` no longer contains `## Masking Probes` or peak-RSS notes, I do not count this alone as a hard reject; it should still be tightened before W0 final closure.

## Accepted V2 Folds

- `gate-json` is check-only by default. It validates before any write, writes only when `--update-results`/`--write-results` is present, and otherwise compares the render against checked-in `RESULTS.md` (`skinny/crates/bbnf-bench/src/bin/gate.rs:294`-`skinny/crates/bbnf-bench/src/bin/gate.rs:315`).
- Validation failure no longer rewrites `RESULTS.md`; the failure branch renders to stdout and exits invalid (`skinny/crates/bbnf-bench/src/bin/gate.rs:294`-`skinny/crates/bbnf-bench/src/bin/gate.rs:303`).
- `bench-json` intentionally passes `--update-results` only after a full benchmark run (`skinny/xtask/src/main.rs:208`-`skinny/xtask/src/main.rs:232`), while direct `gate-json` accepts explicit check/update flags (`skinny/xtask/src/main.rs:240`-`skinny/xtask/src/main.rs:265`).
- Absolute Criterion-root paths and current-HEAD IDs are gone from rendered run IDs; committed rows now use a content-fingerprint ID such as `sk-v8-open:criterion-fnv64-2dcb1beddbcc83fd` (`skinny/RESULTS.md:48`).

## Evidence

- Ran `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --advisory --check-results` from `skinny/`; it exited `0`.
- Verified the command left the worktree clean with `git status --short`.
- Inspected `skinny/crates/bbnf-bench/src/bin/gate.rs`, `skinny/crates/bbnf-bench/src/report.rs`, `skinny/crates/bbnf-bench/src/metadata.rs`, `skinny/xtask/src/main.rs`, `skinny/RESULTS.md`, W0 V1 CH4, and the V1 consolidated fold set.

## Missed Tests Or Evidence

- No negative test corrupts a `metadata.toml` `bbnf_commit` while leaving estimates in tolerance and proves `gate-json --update-results` rejects it.
- No negative test mixes host/profile/rustflags/target CPU/sample policy across Criterion rows and proves W0 rejects the root before writing.
- No test verifies `input_sha256` and `input_bytes` in metadata against the fixture bytes for every rendered row.
- No test proves malformed or missing metadata for a native comparator is a hard W0 error rather than being silently dropped and replaced by partial display fields.
- No test blocks or labels `--update-results --include-volatile-probes` so volatile probe/RSS sections cannot accidentally become committed W0 evidence.

## Mandatory Fold Items

1. Add a W0 capture validator before rendering or writing. It must reject missing/malformed metadata and enforce one coherent capture across all loaded rows: expected `bbnf_commit` or explicit capture id, fixture `input_sha256`, `input_bytes`, profile, rustflags, target CPU, host identity, sample size, measurement time, and any required workload/track tags.
2. Treat `criterion_fingerprint` as an immutable capture identifier only after metadata validation passes. Do not let the fingerprint substitute for semantic freshness/coherence checks.
3. Add stale-root and mixed-root negative tests that exercise the real `gate-json` update path, not only report helper structs.
4. Either make `--include-volatile-probes` incompatible with `--update-results`, or render those sections with explicit non-admission labeling plus baseline validation so they cannot churn committed evidence while W0 passes.
