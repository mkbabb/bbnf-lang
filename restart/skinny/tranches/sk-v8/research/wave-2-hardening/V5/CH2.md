# SK-V8 W2 Hardening V5 CH2

Verdict: ACCEPT

Confidence: 97%

Scope: re-challenged unchanged V4-folded target at HEAD `bf2f073d`
(`docs(sk-v8-wave2-hardening): record V4 accept cycle`), with the folded report
gate implementation at `74fe4e1b`. Focus was W2 source/product parity,
Apache/CITM typed fixture admission, Canada route-out, Track 1/serde_json
oracle/sonic parity wording, and absence of `skinny/RESULTS.md` row-table
overclaim.

## Findings

1. The V3 report-gate blocker remains folded at HEAD. The checked report path
   passes `w0_real_typed_metadata_expected(&fixture.name)` into W0 metadata
   validation (`skinny/crates/bbnf-bench/src/bin/gate.rs:57-62`), and that
   helper now derives expectation from `sk_v8_open_baseline("json/{fixture}/real_typed_struct/main")`
   rather than the real typed source fixture map
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:1115-1117`). The regression test
   keeps `twitter` and `update_center` expected while excluding source-only
   `apache_builds` and `citm_catalog`
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:1718-1723`).

2. Apache/CITM are admitted as W2 source/product parity fixtures, not measured
   benchmark rows. They are present in the real typed fixture map and generated
   Track 1 dispatch (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:182-187`,
   `skinny/crates/bbnf-bench/src/real_typed_struct.rs:225-238`), and the
   full-fixture parity test covers both source fixtures
   (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:609-617`). REDRESS and
   HANDOFF preserve the same boundary: source/product parity only until a later
   accepted benchmark row-table wave (`skinny/REDRESS.md:2622-2657`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:175-192`).

3. Track 1, serde_json oracle, and sonic parity wording is acceptable for W2.
   The code shape is generated DirectBuild Track 1, Track 2 delegating to the
   serde_json-backed typed path, and sonic-rs as a separate strict checksum lane
   (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:251-323`). Report
   telemetry still names Track 2 as a structural oracle, not the SOTA gate
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:244-255`), while REDRESS states
   the W2 proof as generated Track 1 DirectBuild plus serde_json as
   Track 2/oracle with a separate sonic-rs checksum parity lane
   (`skinny/REDRESS.md:2632-2636`).

4. Canada remains correctly routed out. REDRESS records that
   `canada/real_typed_struct` failed full-fixture DirectBuild-versus-serde
   checksum parity on long decimal coordinate payloads, and W2 routes it out
   instead of weakening the proof to length-only or digest-only evidence
   (`skinny/REDRESS.md:2637-2640`). No current source fixture admission or
   report-gate path reopens Canada.

5. There is no `RESULTS.md` row-table overclaim. The measured report table still
   has exactly four `real_typed_struct` rows: `twitter`, `update_center`,
   `mesh`, and `marine_ik` (`skinny/RESULTS.md:7`, `skinny/RESULTS.md:18`,
   `skinny/RESULTS.md:21`, `skinny/RESULTS.md:28`). The manifest rows follow the
   same four-row set, and no `apache_builds/real_typed_struct`,
   `citm_catalog/real_typed_struct`, or `canada/real_typed_struct` measured row
   exists.

## Verification

- `git rev-parse --short HEAD`: `bf2f073d`.
- `git status --short`: clean before writing this CH2 file.
- `cargo test -p bbnf-bench w0_real_typed_metadata_expectation_uses_measured_baseline_not_source_fixtures -- --nocapture` from `skinny/`: passed.
- `cargo test -p bbnf-bench real_typed -- --nocapture` from `skinny/`: passed; 7 library tests and 1 gate test passed.
- `cargo xtask check-real-typed` from `skinny/`: passed.
- `cargo xtask gate-json --with-cost-facts --advisory --check-results` from `skinny/`: passed with empty gate diagnostics.
- `awk` audit of `skinny/RESULTS.md` printed only `twitter/real_typed_struct`, `update_center/real_typed_struct`, `mesh/real_typed_struct`, and `marine_ik/real_typed_struct`.
- `rg` audit for `apache_builds`, `citm_catalog`, or `canada` measured `real_typed_struct` rows in `skinny/RESULTS.md`: no matches.
- `git diff --exit-code 8ce03af4..HEAD -- skinny/RESULTS.md`: clean.
- `git diff --exit-code HEAD -- skinny/RESULTS.md`: clean.

## Required Folds

None.

Keep the W2 boundary unchanged: Apache/CITM are source/product typed parity
admissions only, Canada stays routed out, Track 2 remains the serde_json-backed
oracle path with sonic-rs separate, and measured benchmark row-table admission
is reserved for a later accepted wave.
