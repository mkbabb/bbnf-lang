# SK-V8 W2 Hardening V4 CH2

Verdict: ACCEPT

Confidence: 96%

Scope: reviewed commit `74fe4e1b` against the W2 source/product parity posture,
Apache/CITM typed fixture admission, Canada route-out, Track 1/Track 2/oracle
independence wording, and the requirement that `skinny/RESULTS.md` not claim
unadmitted measured `real_typed_struct` rows.

## Findings

1. The V3 checked-report blocker is folded. The report gate now passes
   `w0_real_typed_metadata_expected(&fixture.name)` into W0 metadata validation
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:57-62`), and that helper derives
   expectation from `sk_v8_open_baseline("json/{fixture}/real_typed_struct/main")`
   instead of the broader source fixture map (`skinny/crates/bbnf-bench/src/bin/gate.rs:1115-1116`).
   The regression test proves W0 still expects `twitter` and `update_center`
   typed metadata but not source-only `apache_builds` or `citm_catalog`
   metadata (`skinny/crates/bbnf-bench/src/bin/gate.rs:1718-1723`).

2. Apache/CITM remain admitted as source/product typed fixtures, not measured
   benchmark rows. They are present in the real typed fixture map and generated
   Track 1 dispatch (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:182-187`,
   `skinny/crates/bbnf-bench/src/real_typed_struct.rs:225-238`), and the full
   fixture test covers both names (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:609-617`).
   REDRESS 91 and HANDOFF keep the same boundary: Apache/CITM are
   source/product rows until a later accepted benchmark row-table wave
   (`skinny/REDRESS.md:2622-2636`, `skinny/REDRESS.md:2653-2659`;
   `restart/skinny/tranches/sk-v8/HANDOFF.md:175-192`).

3. Track 1/Track 2/oracle wording is now independent enough for W2. Track 1 is
   generated DirectBuild, Track 2 delegates to the serde_json-backed typed path,
   and sonic-rs is a separate strict checksum lane
   (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:251-323`). The ledger
   wording matches that shape: generated Track 1 plus serde_json as the
   Track 2/oracle path, with sonic-rs separate (`skinny/REDRESS.md:2632-2636`;
   `restart/skinny/tranches/sk-v8/HANDOFF.md:175-179`).

4. Canada remains routed out. REDRESS records that
   `canada/real_typed_struct` is rejected after the full-fixture DirectBuild
   versus serde checksum mismatch on long decimal coordinates, and explicitly
   rejects weakening the proof to length-only or digest-only evidence
   (`skinny/REDRESS.md:2637-2640`). No current source admission or report-gate
   change reopens Canada.

5. There is no RESULTS row-table overclaim. `skinny/RESULTS.md` still contains
   exactly four measured `real_typed_struct` rows: `twitter`, `update_center`,
   `mesh`, and `marine_ik` (`skinny/RESULTS.md:7`,
   `skinny/RESULTS.md:18`, `skinny/RESULTS.md:21`, `skinny/RESULTS.md:28`;
   manifest rows at `skinny/RESULTS.md:50`, `skinny/RESULTS.md:61`,
   `skinny/RESULTS.md:64`, and `skinny/RESULTS.md:71`). There are no measured
   `apache_builds/real_typed_struct`, `citm_catalog/real_typed_struct`, or
   `canada/real_typed_struct` rows, and commit `74fe4e1b` has no
   `skinny/RESULTS.md` diff.

## Verification

- `cargo test -p bbnf-bench w0_real_typed_metadata_expectation_uses_measured_baseline_not_source_fixtures -- --nocapture` from `skinny/`: passed.
- `cargo test -p bbnf-bench real_typed -- --nocapture` from `skinny/`: passed; 7 library tests and 1 gate test passed.
- `cargo xtask check-real-typed` from `skinny/`: passed.
- `cargo xtask gate-json --with-cost-facts --advisory --check-results` from `skinny/`: passed with zero gate diagnostics.
- `cargo xtask gate-json --advisory --check-results` from `skinny/`: failed only on the already recorded W0 run-id strict validator drift (`json/twitter/parse_only/main` moved from `sk-v8-open:criterion-fnv64-9a37562ed3d0383a` to `sk-v8-open:criterion-fnv64-b9435757f85b6da0`); it did not fail on missing Apache/CITM real typed metadata.
- `awk` audit of `skinny/RESULTS.md`: printed only `twitter`, `update_center`, `mesh`, and `marine_ik` as measured `real_typed_struct` rows.
- `git diff --exit-code 74fe4e1b^ 74fe4e1b -- skinny/RESULTS.md`: clean.

## Required Folds

None.

Preserve the current boundary: W2 admits Apache/CITM source/product parity only,
routes Canada out, keeps Track 2 as the serde_json-backed oracle path with
sonic-rs separate, and rejects benchmark row-table admission until a later
accepted measured-row wave.
