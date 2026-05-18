# SK-V8 W0 Hardening V12 CH1 - Correctness

Date: 2026-05-18.

Target reviewed: `61d5cc3b4312883e026060174e876a0c18b34703`
(`fix(sk-v8-wave0): fold hardening V10 cost and metadata blockers`).

Verdict: ACCEPT.

Confidence: 96%.

This unchanged re-challenge preserves the V11 CH1 acceptance. HEAD is the V11
archive commit `b34dbeb81da7b29bb8135de4d54238d12765ed24`, with no drift in
the W0 validation/report/results/hardening paths since that commit. W0 manifest
semantics remain executable, strict admission rejects non-GO and hard-failure
outcomes, strict-vs-strict comparator discipline holds, run-id/profile/hot-leaf/
sample-cost/build metadata validation is measurable, and the evidence commands
still resolve from the `skinny/` workspace.

## Scope Reviewed

- CH1 requires resolving claims to file lines, SHAs, rows, or REDRESS entries,
  with measurable gates and strictness-plane discipline
  (`restart/prompts/ORCHESTRATOR.md:83`). The convergence rule requires
  `>=95% ACCEPT` for two consecutive cycles with no open critical defects or
  unresolved REVISE (`restart/prompts/ORCHESTRATOR.md:118`-
  `restart/prompts/ORCHESTRATOR.md:120`).
- SK-V8 strict admission is executable: comparator plane, strictness, same-run
  native freshness, and measured-row validation must all hold; sidecar same-run
  claims reject until a structured manifest exists
  (`restart/skinny/tranches/sk-v8/SPEC.md:73`-
  `restart/skinny/tranches/sk-v8/SPEC.md:81`).
- W0 permits `S` and hard-failure outcomes such as `L`, but neither hard failure
  nor `S` may support strict SOTA admission
  (`restart/skinny/tranches/sk-v8/SPEC.md:97`-
  `restart/skinny/tranches/sk-v8/SPEC.md:101`).
- Required W0 telemetry must be consumed by `gate-json`; missing fields,
  unsupported outcomes, strictness mismatch, stale sidecar evidence, or
  producer-only telemetry rejects
  (`restart/skinny/tranches/sk-v8/SPEC.md:110`-
  `restart/skinny/tranches/sk-v8/SPEC.md:146`).
- W0 tasks and exit gates require hot leaf, profile artifact, run id, host/build
  metadata, feature mask, sample cost, SK-V8-open delta, sidecar rejection, and
  same-wave `gate-json` consumption
  (`restart/skinny/tranches/sk-v8/SPEC.md:312`-
  `restart/skinny/tranches/sk-v8/SPEC.md:320`,
  `restart/skinny/tranches/sk-v8/SPEC.md:346`-
  `restart/skinny/tranches/sk-v8/SPEC.md:361`).
- V10 rejected on cost footprint and empty host/feature metadata; V11 accepted
  the fold and recorded that it was only the first qualifying ACCEPT after the
  reset (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:25`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:31`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V11/HARDENING-W0-V11-CONSOLIDATED.md:10`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V11/HARDENING-W0-V11-CONSOLIDATED.md:14`-
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V11/HARDENING-W0-V11-CONSOLIDATED.md:17`).

## Evidence

1. No drift since V11. `git rev-parse HEAD` returns
   `b34dbeb81da7b29bb8135de4d54238d12765ed24`, and `git diff --name-status
   b34dbeb8..HEAD -- skinny/crates/bbnf-bench/src/report.rs
   skinny/crates/bbnf-bench/src/gate.rs skinny/crates/bbnf-bench/src/bin/gate.rs
   skinny/RESULTS.md restart/skinny/tranches/sk-v8/research/wave-0-hardening`
   returns empty output. The unchanged target remains one report-validator
   compaction: `git show --numstat 61d5cc3b --
   skinny/crates/bbnf-bench/src/report.rs` returns `58 109`, while the live
   post-V9 fold footprint remains `118 13` from `00c3485a..61d5cc3b`.

2. W0 manifest semantics are executable. Row validation rejects empty required
   telemetry fields, binds row identity, outcome, wave id, run id, sample count,
   sample cost, profile artifact, and hot leaf before calling
   `validate_w0_manifest_semantics()`
   (`skinny/crates/bbnf-bench/src/report.rs:275`-
   `skinny/crates/bbnf-bench/src/report.rs:355`). Full report validation fixes
   the row count, row ids, exact outcomes/verdicts, and Track 1/Track 2 baseline
   deltas against `SK_V8_OPEN_BASELINE`
   (`skinny/crates/bbnf-bench/src/report.rs:494`-
   `skinny/crates/bbnf-bench/src/report.rs:525`).

3. The V10 empty metadata blocker remains closed. W0 manifest validation requires
   exact pre-W1 CostFacts/redress/Track 2 sentinels
   (`skinny/crates/bbnf-bench/src/report.rs:1007`-
   `skinny/crates/bbnf-bench/src/report.rs:1019`), exact benchmark build flags
   (`skinny/crates/bbnf-bench/src/report.rs:1027`-
   `skinny/crates/bbnf-bench/src/report.rs:1038`), a structured non-empty host,
   `arch`, and `cpu` (`skinny/crates/bbnf-bench/src/report.rs:1039`-
   `skinny/crates/bbnf-bench/src/report.rs:1052`), and non-empty feature
   `arch`, `os`, `simd` plus exact `target_cpu=native`
   (`skinny/crates/bbnf-bench/src/report.rs:1053`-
   `skinny/crates/bbnf-bench/src/report.rs:1062`). Focused tests mutate empty
   host and feature payloads and require failure
   (`skinny/crates/bbnf-bench/src/report.rs:2065`-
   `skinny/crates/bbnf-bench/src/report.rs:2068`).

4. Strict admission still rejects non-GO and hard-failure outcomes. The outcome
   classifier marks `G`, `I`, `K`, `L`, `M`, `N-direct`, and `S` as `NoGo`, and
   `J` as `Invalid` (`skinny/crates/bbnf-bench/src/gate.rs:72`-
   `skinny/crates/bbnf-bench/src/gate.rs:90`). `validate_strict_admission()`
   rejects any parsed outcome whose verdict is not `Go` before strictness or
   comparator evidence can admit it (`skinny/crates/bbnf-bench/src/gate.rs:135`-
   `skinny/crates/bbnf-bench/src/gate.rs:144`). The strict test covers `D`, `E`,
   `F-positive`, `F-noise`, `G`, `I`, `J`, `K`, `L`, `M`, `N-direct`, and `S`
   as rejects (`skinny/crates/bbnf-bench/src/gate.rs:459`-
   `skinny/crates/bbnf-bench/src/gate.rs:482`). W0 report validation also keeps
   all current rows deferred/view-boundary (`skinny/crates/bbnf-bench/src/report.rs:1096`-
   `skinny/crates/bbnf-bench/src/report.rs:1121`).

5. Strict-vs-strict comparator discipline still holds. Strict admission requires
   strict row/comparator evidence, measured-row UTF-8 and validation, complete
   escape validation, plane match, same-run native comparator freshness, and
   `sidecar_freshness=n/a` (`skinny/crates/bbnf-bench/src/gate.rs:145`-
   `skinny/crates/bbnf-bench/src/gate.rs:173`). Report validation requires
   comparator evidence slots, finite positive Mbps where populated, explicit
   absent reasons for missing sidecars, and no unsupported comparator ids
   (`skinny/crates/bbnf-bench/src/report.rs:1135`-
   `skinny/crates/bbnf-bench/src/report.rs:1227`). Sidecar same-run claims reject
   without a structured manifest (`skinny/crates/bbnf-bench/src/report.rs:1263`-
   `skinny/crates/bbnf-bench/src/report.rs:1292`), while native strict
   comparators are workload-bound to same-run bench paths and matching planes
   (`skinny/crates/bbnf-bench/src/report.rs:1313`-
   `skinny/crates/bbnf-bench/src/report.rs:1368`).

6. Run id, profile artifact, hot leaf, sample cost, and required metadata are
   measurable on the generated gate path. `gate-json` validates schema and W0
   telemetry before writing/comparing `RESULTS.md`
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:319`-
   `skinny/crates/bbnf-bench/src/bin/gate.rs:327`). The gate creates
   `sample_cost` only from finite positive Track 1 nanoseconds and non-zero bytes
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:463`-
   `skinny/crates/bbnf-bench/src/bin/gate.rs:471`), while report validation
   rejects zero sample count, `n/a` sample cost, missing `ns_per_byte=`, profile
   path drift, and hot-leaf/profile mismatch
   (`skinny/crates/bbnf-bench/src/report.rs:342`-
   `skinny/crates/bbnf-bench/src/report.rs:355`,
   `skinny/crates/bbnf-bench/src/report.rs:968`-
   `skinny/crates/bbnf-bench/src/report.rs:990`). Full-baseline tests cover
   throughput drift, outcome/verdict drift, single-row and uniform run-id drift,
   strict hard-failure drift, metadata sentinels, and substrate tuple drift
   (`skinny/crates/bbnf-bench/src/report.rs:1995`-
   `skinny/crates/bbnf-bench/src/report.rs:2070`).

7. The rendered report still matches the closed boundary. `skinny/RESULTS.md`
   renders the W0 manifest columns (`skinny/RESULTS.md:46`-
   `skinny/RESULTS.md:47`), sample row metadata with run id, validation path,
   profile artifact, sample cost, build flags, host triple, feature mask,
   CostFacts, substrate tuple, Track 2, and comparator evidence
   (`skinny/RESULTS.md:48`-`skinny/RESULTS.md:54`). The report remains
   `Overall outcome N-direct / NoGo`, and its W0 note states that native Rust
   comparators are same-run while C++ sidecars are historical or absent and never
   W0 strict anchors (`skinny/RESULTS.md:138`-`skinny/RESULTS.md:141`).

## Commands Run

- `git rev-parse HEAD` -> `b34dbeb81da7b29bb8135de4d54238d12765ed24`.
- `git diff --name-status b34dbeb8..HEAD -- skinny/crates/bbnf-bench/src/report.rs skinny/crates/bbnf-bench/src/gate.rs skinny/crates/bbnf-bench/src/bin/gate.rs skinny/RESULTS.md restart/skinny/tranches/sk-v8/research/wave-0-hardening` -> empty.
- `git diff --numstat 00c3485a..61d5cc3b -- skinny/crates/bbnf-bench/src/report.rs` -> `118 13`.
- `git show --numstat 61d5cc3b -- skinny/crates/bbnf-bench/src/report.rs` -> `58 109`.
- `git diff --name-only 0bd16f6d..61d5cc3b -- skinny/crates/runtime skinny/crates/bbnf-simd skinny/crates/codegen skinny/crates/generated-json skinny/crates/test-fixtures skinny/crates/bbnf-bench/benches skinny/crates/bbnf-bench/src/direct.rs skinny/crates/bbnf-bench/src/scan.rs skinny/crates/bbnf-bench/src/real_typed_struct.rs skinny/crates/bbnf-bench/src/track2.rs skinny/crates/bbnf-bench/src/parity.rs skinny/crates/bbnf-bench/src/materialization.rs` -> empty.
- `git diff --check 00c3485a..61d5cc3b -- skinny/crates/bbnf-bench/src/report.rs` -> clean.
- `git diff --check` -> clean.
- From `skinny/`: `cargo test -p bbnf-bench w0_ -- --nocapture` -> passed 12 report W0 tests and 8 gate-bin W0 tests.
- From `skinny/`: `cargo test -p bbnf-bench strict -- --nocapture` -> passed 5 focused strict tests.
- From `skinny/`: `cargo test -p bbnf-bench sidecar_same_run -- --nocapture` -> passed 1 focused sidecar-same-run test.
- From `skinny/`: `cargo test -p bbnf-bench` -> passed 52 library tests, 8 gate-bin tests, and doc-tests.
- From `skinny/`: `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results` -> exit 0; output retained `Overall outcome N-direct / NoGo.`
- From `skinny/`: `cargo xtask check-json` -> exit 0.
- From `skinny/`: `cargo xtask check-real-typed` -> exit 0.
- From `skinny/`: `cargo xtask check-conformance` -> exit 0 with `21 valid fixtures accepted; 7 invalid fixtures rejected`.
- `git status --short` -> clean before writing this CH1 file.

## Blockers

None for CH1.

## Required Fold If Rejecting

Not applicable. This CH1 disposition is ACCEPT.

## Residual Risk

- This is a CH1 acceptance only. W0 closes under ORCHESTRATOR Section 3Z only if
  the full V12 challenge cycle reaches the second consecutive qualifying ACCEPT
  with no critical defect and no unresolved REVISE.
- `sample_cost` report validation is still shape-based in `report.rs`: it rejects
  `n/a` and requires an `ns_per_byte=` token, but does not parse that token as a
  finite numeric value (`skinny/crates/bbnf-bench/src/report.rs:345`). This is
  not a V12 blocker because the generated `gate-json` path constructs the field
  from finite positive timing data (`skinny/crates/bbnf-bench/src/bin/gate.rs:463`-
  `skinny/crates/bbnf-bench/src/bin/gate.rs:471`), but a later external manifest
  parser should make the report-side check numeric.
- Row-level `validate_w0_outcome()` still accepts internal W0 outcome spellings
  beyond the rendered SPEC set before full-report baseline validation
  (`skinny/crates/bbnf-bench/src/report.rs:955`-
  `skinny/crates/bbnf-bench/src/report.rs:964`). Full W0 report validation still
  binds exact row ids, outcomes, verdicts, and throughput to the opening baseline
  (`skinny/crates/bbnf-bench/src/report.rs:494`-
  `skinny/crates/bbnf-bench/src/report.rs:525`), so this is not a current
  admission route.
