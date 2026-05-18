# CH1 W0 V2 Hardening Challenge

Verdict: ACCEPT.

Confidence: 96%.

Scope: correctness review of current HEAD `cb0fdba0` against the W0/V1 strict-admission blockers, outcome admission semantics, `parse_utf8` / `escape_complete`, and unsupported/legacy outcome rejection. This review did not edit source.

## Findings

1. Strict-admission validation is now live on the W0 gate path. `gate-json` still validates the rendered report through `report.validate_schema_v3().and_then(|_| report.validate_sk_v8_w0())` before check/update completion (`skinny/crates/bbnf-bench/src/bin/gate.rs:296`), and `TelemetryRow::validate_sk_v8_w0` now calls `validate_w0_admission_boundary(self)?` before comparator evidence acceptance (`skinny/crates/bbnf-bench/src/report.rs:328`, `skinny/crates/bbnf-bench/src/report.rs:368`, `skinny/crates/bbnf-bench/src/report.rs:369`). The W0 admission helper constructs `StrictAdmissionEvidence` from the actual row and comparator fields, then calls `gate::validate_strict_admission` (`skinny/crates/bbnf-bench/src/report.rs:917`, `skinny/crates/bbnf-bench/src/report.rs:937`, `skinny/crates/bbnf-bench/src/report.rs:949`). This closes the V1 paper-wiring blocker.

2. `parse_utf8` and `escape_complete` are part of the strict predicate. `StrictAdmissionEvidence` carries both fields (`skinny/crates/bbnf-bench/src/gate.rs:59`, `skinny/crates/bbnf-bench/src/gate.rs:62`, `skinny/crates/bbnf-bench/src/gate.rs:63`), and `validate_strict_admission` rejects `parse_utf8 != measured-row` and `escape_complete != yes` (`skinny/crates/bbnf-bench/src/gate.rs:151`, `skinny/crates/bbnf-bench/src/gate.rs:154`). The report-level strict-claim test mutates an `A` row to `strict` / `measured-row` while retaining `parse_utf8=view-boundary`, and the same W0 validator rejects it (`skinny/crates/bbnf-bench/src/report.rs:1438`, `skinny/crates/bbnf-bench/src/report.rs:1456`, `skinny/crates/bbnf-bench/src/report.rs:1459`, `skinny/crates/bbnf-bench/src/report.rs:1460`). This matches SPEC's rule that `Strictness=deferred`, `parse_utf8=view-boundary`, stale sidecars, and plane mismatch are guard telemetry only (`restart/skinny/tranches/sk-v8/SPEC.md:73`).

3. Unsupported and legacy outcomes are rejected in the W0 row validator. The shared parser still recognizes the broader enum for classifier use (`skinny/crates/bbnf-bench/src/gate.rs:114`), but W0-specific validation rejects any unknown id and any non-W0 legacy outcome outside the admitted set (`skinny/crates/bbnf-bench/src/report.rs:865`, `skinny/crates/bbnf-bench/src/report.rs:869`). The direct report test rejects `F-positive` through `row.validate_sk_v8_w0()` (`skinny/crates/bbnf-bench/src/report.rs:1453`, `skinny/crates/bbnf-bench/src/report.rs:1454`), while the lower-level strict-admission test rejects an unknown `Q` id (`skinny/crates/bbnf-bench/src/gate.rs:453`, `skinny/crates/bbnf-bench/src/gate.rs:457`, `skinny/crates/bbnf-bench/src/gate.rs:458`).

4. Parse outcome semantics now preserve hard failures instead of over-demoting them to `S`. The W0 report admits parse rows only as `I`, `J`, `K`, `L`, `M`, or `S` (`skinny/crates/bbnf-bench/src/report.rs:360`), and the producer-side `w0_parse_non_admission` preserves `I`, `J`, `K`, `L`, and `M` while demoting admission-capable parse outcomes to reserved `S` (`skinny/crates/bbnf-bench/src/bin/gate.rs:339`). The bin tests cover both preservation and demotion (`skinny/crates/bbnf-bench/src/bin/gate.rs:1215`, `skinny/crates/bbnf-bench/src/bin/gate.rs:1239`). This resolves the V1 consolidated blocker requiring `I`, `J`, `K`, `L`, and `M` hard failures to remain visible (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V1/HARDENING-W0-V1-CONSOLIDATED.md:31`).

5. The current `A / GO` direct and typed rows remain deferred W0 guard/product rows, not strict SOTA admissions. SPEC separately preserves direct guard and real-typed GO rows (`restart/skinny/tranches/sk-v8/SPEC.md:152`, `restart/skinny/tranches/sk-v8/SPEC.md:153`, `restart/skinny/tranches/sk-v8/SPEC.md:154`), while strict admission is only attempted when a row claims `strict` or `measured-row` validation (`skinny/crates/bbnf-bench/src/report.rs:917`, `skinny/crates/bbnf-bench/src/report.rs:919`). Current rendered rows still show deferred/view-boundary for those GO rows, making the non-strict status explicit (`skinny/RESULTS.md:7`, `skinny/RESULTS.md:18`, `skinny/RESULTS.md:21`, `skinny/RESULTS.md:28`).

## Missed Tests / Evidence

- Non-blocking: there is no report-level test that mutates `row.outcome_id` to an unknown string such as `Q`; the report-level negative uses `F-positive` (`skinny/crates/bbnf-bench/src/report.rs:1453`). This is acceptable for V2 because `validate_w0_outcome` directly calls `gate::parse_outcome_id` before the W0 allowlist (`skinny/crates/bbnf-bench/src/report.rs:865`), and `parse_outcome_id("Q")` is already covered in the gate tests (`skinny/crates/bbnf-bench/src/gate.rs:453`).
- Evidence run during this review: `cargo test -p bbnf-bench` passed 45 library tests plus 2 gate-bin tests.
- Evidence run during this review: `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --advisory --check-results` exited 0 and left the worktree clean.

## Mandatory Fold Items

None for CH1. The strict-admission live wiring, outcome admission semantics, `parse_utf8` / `escape_complete` rejection, unsupported/legacy outcome rejection, and hard parse failure preservation are sufficient for W0 V2 under this lens.
