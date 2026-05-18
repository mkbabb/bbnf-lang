# CH5 W0 V2 Hardening Challenge

Verdict: REJECT

Confidence: 94%

Scope: current HEAD `cb0fdba0` against SK-V8 W0/V1 blockers, with CH5 focus on manifest/report schema completeness, telemetry row provenance, hot-leaf/profile artifacts, row_id/workload consistency, and whether W0 honestly blocks W1-W6.

## Findings

1. Blocking: W0 validation does not bind `sk_v8.row_id` back to the rendered row `corpus` and `workload`.

   `TelemetryRow` carries independent `corpus`, `workload`, and `sk_v8.row_id` fields (`skinny/crates/bbnf-bench/src/report.rs:71`). `TelemetryRow::validate_sk_v8_w0` validates required text, grammar/domain, outcome, W0 marker, sample fields, profile artifact, hot leaf, consumer class, parse-only guard, admission boundary, and comparator evidence (`skinny/crates/bbnf-bench/src/report.rs:275`), but it never compares `self.corpus` and `self.workload` with the corpus/workload parsed from `telemetry.row_id`. The report-level validator then deduplicates and baselines rows by `row.sk_v8.row_id` only (`skinny/crates/bbnf-bench/src/report.rs:492`).

   Impact: a malformed W0 row can claim `row_id=json/twitter/parse_only/main` while rendering as `workload=direct_to_struct`; the parse-only substrate guard is keyed to `self.workload` (`skinny/crates/bbnf-bench/src/report.rs:360`), so this mismatch can bypass the W0 parse non-admission rule while still satisfying the baseline row-id ledger. This violates SPEC's requirement that every emitted telemetry field be consumed by `gate-json` and that all 38 current rows satisfy the W0 schema (`restart/skinny/tranches/sk-v8/SPEC.md:140`, `restart/skinny/tranches/sk-v8/SPEC.md:362`).

2. Blocking: native comparator planes are not validated for non-strict W0 rows.

   V2 correctly generates workload-specific native comparator source paths in the report producer (`skinny/crates/bbnf-bench/src/bin/gate.rs:476`) and rejects source-artifact mismatches in the report validator (`skinny/crates/bbnf-bench/src/report.rs:1046`). However, `validate_comparator_evidence` receives `row_id`, `workload`, and comparator evidence only (`skinny/crates/bbnf-bench/src/report.rs:961`), and `validate_native_comparator_source` checks the expected Criterion path but not the expected comparator plane (`skinny/crates/bbnf-bench/src/report.rs:1057`). The only plane comparison lives in strict-admission validation, which W0 skips for all deferred/view-boundary rows (`skinny/crates/bbnf-bench/src/report.rs:917`).

   Impact: a direct or typed row can carry correct `sonic_rs_direct_to_struct` or `sonic_rs_real_typed_struct` source artifacts while advertising `comparator_plane=DOM`, and W0 validation will still pass because the row is not a strict measured-row claim. That leaves `comparator_plane` producer-trusted rather than gate-consumed, contrary to the required telemetry rule (`restart/skinny/tranches/sk-v8/SPEC.md:114`, `restart/skinny/tranches/sk-v8/SPEC.md:140`).

## Accepted V2 Fold Items

- Hot-leaf/profile placeholders are no longer accepted by shape. W0 now requires `criterion-slope-profile:` plus the exact row-derived Criterion path (`skinny/crates/bbnf-bench/src/report.rs:878`) and requires the hot leaf to equal the profile artifact plus the row id (`skinny/crates/bbnf-bench/src/report.rs:891`).
- Workload-specific native source artifacts are generated for parse, direct, and real typed rows (`skinny/crates/bbnf-bench/src/bin/gate.rs:487`) and validated against the expected row/workload source path (`skinny/crates/bbnf-bench/src/report.rs:1057`).
- C++ sidecars remain historical or absent, and populated/absent freshness conflicts are rejected (`skinny/crates/bbnf-bench/src/report.rs:1005`). This preserves the CH5 V1 boundary that sidecars are not strict admission anchors.
- The packet blocks W1-W6 honestly at the document level: W0 rejection blocks W1-W6 (`restart/skinny/tranches/sk-v8/SPEC.md:385`), W1 entry requires W0 admission and SK-V8-open telemetry (`restart/skinny/tranches/sk-v8/SPEC.md:404`), and final closure requires W0-W5 dispositions (`restart/skinny/tranches/sk-v8/SPEC.md:732`).

## Evidence Run

- `cargo test -p bbnf-bench`: passed 45 library tests and 2 `gate` bin tests.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --advisory --check-results`: passed and left the worktree clean.

## Missed Tests / Evidence

- Add a W0 negative test that mutates `sk_v8.row_id` to a different corpus and workload than `TelemetryRow::{corpus, workload}` and proves `validate_sk_v8_w0` rejects it.
- Add a W0 negative test that keeps a direct or typed row's native source artifact correct but changes `comparator_plane` to `DOM`, proving the report validator rejects native plane/workload mismatches even when the row is deferred/view-boundary.
- Add a positive assertion that the generated manifest's row id, row corpus, row workload, native comparator source, native comparator plane, profile artifact, and hot leaf all derive from one consistent row identity.

## Mandatory Fold Items

1. In `TelemetryRow::validate_sk_v8_w0`, parse `sk_v8.row_id` once and require its corpus and workload to equal `self.corpus` and `self.workload` before any outcome, profile, hot-leaf, parse-guard, or baseline validation.
2. Extend native comparator validation to require expected comparator planes by workload: parse rows use `DOM`, direct rows use `digest`, and real typed rows use `typed direct` for `sonic_rs_strict` and `serde_json`.
3. Keep W0 open and W1-W6 blocked until these schema holes are folded and the V2/V3 challenge discipline reaches the required acceptance threshold.
