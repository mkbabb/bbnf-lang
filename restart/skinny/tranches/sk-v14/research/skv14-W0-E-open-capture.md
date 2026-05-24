# SK-V14 W0E: SK-V14-open Capture

Date: 2026-05-24.
Scope: W0 baseline capture method and no-behavior-change proof.
Output: this file.

## §1 — Findings (concrete file:line cited)

1. W0 is a baseline-capture and telemetry-schema wave, not a behavior wave. The SPEC says W0 starts from the current `skinny/RESULTS.md` SK-V13 close baseline, and the W0 plan must name the `SK-V14-open` capture method plus the no-behavior-change proof (`/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v14/SPEC.md:333`). The W0 tasks explicitly include capturing the current report as `SK-V14-open`, adding the new telemetry fields, and populating run id, host, build, feature mask, sample cost, audit overlay, sidecar validation, and SK-V14-open deltas (`/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v14/SPEC.md:341`). The exit gate requires all 75 rows to satisfy the telemetry section, all W0 throughput cells to stay within ±1.0%, and no behavior, source, or generated output changes (`/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v14/SPEC.md:352`).

2. The required SK-V14 telemetry surface is wider than the current visible RESULTS table. Section 0.4 allows the existing 26-column table to remain but adds mandatory report/gate fields such as `track2_entry_point`, `comparator_plane`, `per_iter_equality`, `audit_overlay_verdict`, sidecar freshness, track2 independence, host/run/build data, and `SK-V14-open delta` (`/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v14/SPEC.md:118`). The SPEC also requires every emitted field to be consumed by `xtask gate-json`, with missing required fields and W0 behavior drift rejected (`/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v14/SPEC.md:162`). Current `RESULTS.md` still has the 26-column main table header (`/Users/mkbabb/Programming/bbnf-lang/skinny/RESULTS.md:3`) and a `SK-V9 W0 Telemetry Manifest`, not SK-V14 manifest fields (`/Users/mkbabb/Programming/bbnf-lang/skinny/RESULTS.md:51`).

3. The current SK-V13 rolling ledger is not a valid SK-V14 admit ledger. It identifies itself as `schema_version: sk-v13-rolling-sota-delta-v1` and `run_id: SK-V13-open` (`/Users/mkbabb/Programming/bbnf-lang/restart/skinny/ROLLING-SOTA-DELTA.md:3`). It carries 51 JSON target rows (`/Users/mkbabb/Programming/bbnf-lang/restart/skinny/ROLLING-SOTA-DELTA.md:12`) and 24 CSS L4 rows (`/Users/mkbabb/Programming/bbnf-lang/restart/skinny/ROLLING-SOTA-DELTA.md:66`), including admitted rows that SK-V14 Section 13 later requires the W0 overlay to falsify before re-admission. The SPEC says JSON parse-only, direct, typed, and CSS L4 prior admits must be audit-zeroed until the listed W9/W10/W4 evidence exists (`/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v14/SPEC.md:1122`).

4. `bench-json` is not a safe SK-V14-open capture command because it runs warm Criterion benches and then mutates the report on full success. `skinny/README.md` lists `cargo run -p xtask -- bench-json` and `cargo run -p xtask -- gate-json` as the bench/report commands (`/Users/mkbabb/Programming/bbnf-lang/skinny/README.md:5`). The xtask implementation builds `cargo bench -p bbnf-bench` and, when no Criterion filter arguments are supplied, follows a successful full bench with `gate_json(... "--update-results" ...)` (`/Users/mkbabb/Programming/bbnf-lang/skinny/xtask/src/main.rs:209`). For SK-V14-open, that means the capture method should not be "rerun `bench-json`"; it should snapshot the committed report cells and attach gate-consumed SK-V14 telemetry.

5. `gate-json` is the current report consumer and writer, but its passthrough surface is still SK-V13-era. `gate-json` invokes `cargo run -p bbnf-bench --bin gate -- <passthrough>` (`/Users/mkbabb/Programming/bbnf-lang/skinny/xtask/src/main.rs:242`) and validates passthrough flags such as `--check-results`, `--update-results`, `--write-results`, and SK-V12/SK-V13 report flags (`/Users/mkbabb/Programming/bbnf-lang/skinny/xtask/src/main.rs:265`). The P3 telemetry design says W0 must add SK-V14-specific report flags for comparator rebind, per-iter equality, audit overlay, and track2 entry point, and gate must ingest each (`/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md:98`).

6. The JSON and SIMD bench commands are warm Criterion commands. `json_parity` sets warm-up, measurement, sample size, and throughput per fixture before benchmarking Track 1, Track 2, sonic-rs, simd-json, serde, direct, and typed lanes (`/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/benches/json_parity.rs:31`). Its Criterion config uses a 3-second warm-up, 5-second measurement, and sample size 100 (`/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/benches/json_parity.rs:517`). `simd_scan` also uses Criterion with the same default warm-up/measurement/sample policy (`/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/benches/simd_scan.rs:84`). The CSS L4 bench reads tiny fixtures and runs Criterion groups for Track 1, oracle, and lightningcss lanes (`/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs:5`).

7. There is no implemented authoritative cold-per-parse capture for SK-V14-open. BENCH.md specifies a report-only cold-cache first-parse probe using `iter_custom` and explicit cache eviction between iterations (`/Users/mkbabb/Programming/bbnf-lang/restart/skinny/BENCH.md:1368`). The current `json_parity` probe named `cold_first_parse` instead uses `iter_batched` with cloned fixture bytes and no explicit cache eviction (`/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/benches/json_parity.rs:422`). It is under `json/probes/<corpus>`, and W0 criterion fingerprinting excludes JSON probe groups from the run id inputs (`/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/bin/gate.rs:2939`). Therefore any "cold per-parse" language at W0 must be labelled as probe-only/non-admission unless the eviction implementation is added in a later behavior-approved wave.

8. Current run metadata comes from Criterion metadata TOML plus runtime probes. `HostFacts::probe()` reads `RUSTFLAGS`, derives `target_cpu`, captures CPU model, CPU arch, kernel string, and `git rev-parse HEAD` (`/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/metadata.rs:110`). `RowMetadata::from_bench()` stamps profile, warm-up/sample/measurement settings, allocator, and `cold_cache_mode = "warm"` (`/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/metadata.rs:296`). The gate resolves Criterion data from `CRITERION_HOME`, then `CARGO_TARGET_DIR/criterion`, then workspace `target/criterion` (`/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/bin/gate.rs:4522`).

9. Current `run_id` is a criterion-artifact fingerprint, not a committed-results snapshot id. `RunFacts::probe()` builds `run_id` as `sk-v9-open:criterion-fnv64-<hash>` and derives host/build/feature fields from rustc, env vars, and active backend facts (`/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/bin/gate.rs:2603`). The fingerprint includes selected JSON and SIMD estimate/metadata files (`/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/bin/gate.rs:2911`) but excludes JSON probes and CSS/non-json rows from W0 run-id inputs (`/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/bin/gate.rs:2939`). A SK-V14-open capture id should therefore be a committed-report/manifest hash over all 75 rows or an explicit two-part JSON+CSS capture id, not a reused `sk-v9-open` fingerprint.

10. The present gate already enforces some W0 metadata integrity, but not the SK-V14 semantic overlay. It reads metadata rows from `target/criterion/json_<fixture>/<bench>/metadata.toml` for Track 1, Track 2, sonic, simd-json, serde, direct, and typed rows (`/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/bin/gate.rs:3819`). It rejects missing metadata, missing input hashes/byte counts, non-`bench` profile, non-native target CPU, mixed capture metadata, and missing coherent metadata specs (`/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/bin/gate.rs:3868`). It also checks report-wide CPU, arch, kernel, rustflags, target CPU, profile, and commit identity (`/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/bin/gate.rs:3942`). SK-V14 still needs same-wave gates for comparator plane, per-iter equality, audit overlay, track2 entry point, and W0 delta.

11. The current parse-only comparator metadata is explicitly the pattern SK-V14 must not admit. `json_parity` names `sonic_rs_anchor` as `sonic_rs::from_slice::<sonic_rs::Value>` (`/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/benches/json_parity.rs:87`). SPEC pattern P-2 blocks mislabelling `sonic_rs::from_slice::<Value>` as a strict comparator (`/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v14/SPEC.md:1074`). W0 can preserve that number as a historical captured cell, but the audit overlay must mark the old admit as falsified rather than strict.

12. The no-behavior-change proof can be deterministic and does not require any warm benches. Since SK-V14-open is defined as the W0 captured current report, every throughput cell can be copied from committed `skinny/RESULTS.md` and declared `baseline`/`0.00%` against the SK-V14-open seed. The falsifiable proof is then: parse all 51 JSON plus 24 CSS main rows, compare each SK-V14-open throughput cell to the source report cell, reject any absolute delta greater than 1.0%, and reject any behavior/source/generated-output diff. This matches the W0 target that throughput cells stay within ±1.0% at W0 close (`/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v14/SPEC.md:187`) while avoiding `bench-json` report churn.

## §2 — Recommendations (named falsifiability gates)

1. `G-W0E-SNAPSHOT-SEED`: Capture `SK-V14-open` from committed `skinny/RESULTS.md` plus the current rolling row inventory, not from a fresh benchmark. Store the source commit, source file hash, row count, and a stable all-row capture hash in the new SK-V14 manifest/payload.

2. `G-W0E-ROW-COUNT-COVERAGE`: Require exactly 51 JSON workload rows and 24 CSS L4 rows before W0 can close. Reject missing, duplicate, or extra rows before checking deltas.

3. `G-W0E-DELTA-ZERO`: For every W0 throughput cell, compare the rendered SK-V14-open value back to the captured source cell. Because W0 is the baseline, expected delta is `0.00%`; reject any absolute delta greater than 1.0% and print the row id, workload, source value, rendered value, and percent delta.

4. `G-W0E-CHECK-ONLY-CONSUMER`: Add SK-V14 telemetry as a gate-consumed report extension or sidecar consumed by `cargo xtask gate-json --check-results`. The capture path must not require `cargo xtask bench-json`, and `--update-results` must remain explicit.

5. `G-W0E-METADATA-RENAME`: Replace the W0-facing run identity with `SK-V14-open:<all-75-row-hash>` or equivalent. Do not reuse the existing `sk-v9-open:criterion-fnv64-*` as the SK-V14 capture id because it excludes CSS rows and probes.

6. `G-W0E-COLD-PROBE-LABEL`: Treat `json/probes/<corpus>/cold_first_parse` as non-admission telemetry until the implementation matches BENCH.md's explicit cache-eviction contract. W0 should not use the current probe as no-behavior-change evidence.

7. `G-W0E-AUDIT-OVERLAY`: Require `audit_overlay_verdict` on every prior admit. Current JSON parse-only, direct, typed, and CSS L4 admits should render as captured historical cells but audit-falsified/open under SK-V14 until their listed re-admit evidence exists.

8. `G-W0E-NO-BEHAVIOR-DIFF`: For actual W0 implementation, reject diffs under parser, scanner, SIMD, runtime, codegen, generated output, fixture, or benchmark-behavior paths. Allow only report/gate/telemetry plumbing needed to consume the SK-V14 schema. For this research pass, only this markdown file should change.

## §3 — Risks (REDRESS entries to pre-block)

1. `REDRESS-W0E-BENCH-RERUN-CHURN`: Pre-block any SK-V14-open capture method that reruns `cargo xtask bench-json` or full Criterion benches. It can warm the host, rewrite `RESULTS.md`, and convert a baseline snapshot into a new measurement wave.

2. `REDRESS-W0E-RUNID-PARTIAL`: Pre-block reusing `sk-v9-open:criterion-fnv64-*` or `SK-V13-open` as the SK-V14 capture identity. Current run-id hashing does not cover all 75 rows and the rolling file is explicitly SK-V13.

3. `REDRESS-W0E-PARSE-ONLY-COMPARATOR`: Pre-block parse-only admits tied to `sonic_rs::from_slice::<Value>` unless they render as audit-falsified historical cells. This closes the P-2/P-4 route and the REDRESS 154-158 parse-only family.

4. `REDRESS-W0E-DIRECT-TYPED-COMPARATOR`: Pre-block direct/typed admits without strict struct-deserialization comparators and per-iteration equality. This covers the REDRESS 131-135/141 direct family and 143/145-153/160 typed family.

5. `REDRESS-W0E-CSS-TINY-FIXTURE`: Pre-block CSS L4 admits based on tiny fixtures, generated/fact-stream labels, or scaffold-only evidence. Current CSS fixtures are small enough that P-1/P-3/P-5 must remain blocked until W4 provides the grammar-derived production-corpus path.

6. `REDRESS-W0E-STARTUP-EQUALITY`: Pre-block startup-only checksum or one-time equality proofs. SK-V14 W0 needs empty/missing `per_iter_equality` to reject, matching the REDRESS 28/33 risk class.

7. `REDRESS-W0E-STALE-SIDECAR`: Pre-block stale or historical sidecars as strict anchors. Native same-run comparators may be captured as evidence; C++ sidecars absent or historical must stay non-anchor until freshness is proven.

8. `REDRESS-W0E-PRODUCER-ONLY-TELEMETRY`: Pre-block any SK-V14 telemetry field that is emitted but not consumed by `gate-json`. This covers the REDRESS 80/82-84/88/89 and 96-98 risk classes.

9. `REDRESS-W0E-TRACK2-COUPLING`: Pre-block Track 1 == Track 2 dishonesty or any Track 2 path whose entry point cannot prove independence from Track 1 beyond the public tape/offset interface. This covers REDRESS 126 and SPEC P-7.

10. `REDRESS-W0E-COLD-PROBE-MISLABEL`: Pre-block using current `cold_first_parse` as true cold-cache evidence. Until the implementation has explicit cache eviction and `cold_cache_mode` metadata, it remains a probe-only row, not a W0 proof source.

## §4 — Sources

- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v14/SPEC.md`
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md`
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md`
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v14/research/p3/p3b-wave-sequencing.md`
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/BENCH.md`
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/SUBSTRATE.md`
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/ROLLING-SOTA-DELTA.md`
- `/Users/mkbabb/Programming/bbnf-lang/skinny/README.md`
- `/Users/mkbabb/Programming/bbnf-lang/skinny/Cargo.toml`
- `/Users/mkbabb/Programming/bbnf-lang/skinny/RESULTS.md`
- `/Users/mkbabb/Programming/bbnf-lang/skinny/xtask/src/main.rs`
- `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/Cargo.toml`
- `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/benches/json_parity.rs`
- `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/benches/simd_scan.rs`
- `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs`
- `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/metadata.rs`
- `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/bin/gate.rs`
