# SK-V8 W0 Hardening V1 CH2

## Decision

REJECT

## Acceptance Probability

40%

The W0 implementation is close on row completeness, baseline-delta enforcement, and same-wave manifest validation, but it still accepts evidence that is internally inconsistent or profile-placeholder-shaped. Those are blocking defects for a hardening pass whose stated target is telemetry manifest integrity rather than parser behavior.

## Blocking Findings

1. Native comparator evidence sources are wrong for direct and typed rows, and W0 validation does not catch the mismatch.

   `skinny/crates/bbnf-bench/src/bin/gate.rs:705` through `skinny/crates/bbnf-bench/src/bin/gate.rs:718` read direct and real-typed comparator values from workload-specific Criterion measurements such as `sonic_rs_direct_to_struct`, `serde_json_direct_to_struct`, `sonic_rs_real_typed_struct`, and `serde_json_real_typed_struct`. However, `w0_comparator_evidence` hard-codes native comparator source paths to `criterion:json_{corpus}/sonic_rs_anchor/new/estimates.json` and `criterion:json_{corpus}/serde_json/new/estimates.json` at `skinny/crates/bbnf-bench/src/bin/gate.rs:461` through `skinny/crates/bbnf-bench/src/bin/gate.rs:478`, regardless of whether the row is parse-only, direct-to-struct, or real-typed.

   The rendered manifest exposes the defect: `skinny/RESULTS.md:49` reports `json/twitter/direct_to_struct/main` with direct comparator Mbps values but `source=criterion:json_twitter/sonic_rs_anchor/new/estimates.json` and `source=criterion:json_twitter/serde_json/new/estimates.json`; `skinny/RESULTS.md:50` does the same for `json/twitter/real_typed_struct/main`. The validator at `skinny/crates/bbnf-bench/src/report.rs:862` through `skinny/crates/bbnf-bench/src/report.rs:942` checks presence, sidecar slots, freshness, and value syntax, but it does not assert that native source artifacts match the row workload. This leaves a same-wave consumption loophole: the gate consumes comparator fields, but not the relationship that makes those fields evidentiary.

2. `profile_artifact` and hot-leaf evidence are still report-shaped placeholders rather than a real profile/hot-leaf proof.

   The SK-V8 W0 requirements call for each row to carry sample counts, host/build/profile placeholders filled from the generated path, and hot-leaf/profile evidence rather than deferral: `restart/skinny/tranches/sk-v8/SPEC.md:353` through `restart/skinny/tranches/sk-v8/SPEC.md:356`, `restart/skinny/tranches/sk-v8/SPEC.md:364` through `restart/skinny/tranches/sk-v8/SPEC.md:372`, and `restart/skinny/tranches/sk-v8/SPEC.md:374` through `restart/skinny/tranches/sk-v8/SPEC.md:375`.

   The current implementation sets `profile_artifact` to the Criterion estimates path and `profile_hot_leaf` to `criterion:<group>/new/estimates.json;hot-leaf=criterion-slope;row=<row-id>` at `skinny/crates/bbnf-bench/src/bin/gate.rs:550` through `skinny/crates/bbnf-bench/src/bin/gate.rs:558`. The rendered report shows this for the main row and manifest row at `skinny/RESULTS.md:5` and `skinny/RESULTS.md:48`. The validator only rejects empty values and strings containing `unprofiled` at `skinny/crates/bbnf-bench/src/report.rs:341` through `skinny/crates/bbnf-bench/src/report.rs:347`; it would accept any Criterion estimate path as a profile artifact and any non-`unprofiled` hot-leaf token. That is report-only evidence, not a hard gate on actual profile provenance.

## Nonblocking Findings

- Row completeness is strong. The generated report has 38 main rows and 38 SK-V8 manifest rows, matching `SK_V8_OPEN_BASELINE` in `skinny/crates/bbnf-bench/src/report.rs:630` through `skinny/crates/bbnf-bench/src/report.rs:827`. `Report::validate_sk_v8_w0` enforces exact row count, duplicate detection, unknown-row rejection, and missing-row rejection at `skinny/crates/bbnf-bench/src/report.rs:489` through `skinny/crates/bbnf-bench/src/report.rs:515`.

- Baseline deltas are materially enforced for current W0. `validate_baseline_delta` rejects Track 1 or Track 2 movement greater than 1.0% at `skinny/crates/bbnf-bench/src/report.rs:844` through `skinny/crates/bbnf-bench/src/report.rs:860`, and the exact-opening-baseline test covers acceptance plus 2% rejection at `skinny/crates/bbnf-bench/src/report.rs:1256` through `skinny/crates/bbnf-bench/src/report.rs:1304`. The remaining risk is governance rather than current behavior: the baseline constants live in editable bench-report source, so future changes must remain freeze-reviewed.

- Same-wave consumption exists but is incomplete. The gate calls `report.validate_schema_v3()` and then `report.validate_sk_v8_w0()` before a successful report write at `skinny/crates/bbnf-bench/src/bin/gate.rs:281` through `skinny/crates/bbnf-bench/src/bin/gate.rs:290`, and `skinny/RESULTS.md:265` accurately says W0 consumes the manifest. The blocker is that the consumed schema does not consume enough cross-field provenance to catch the comparator-source and profile-evidence defects above.

- Strict-vs-strict admission discipline appears intact for Wave 0. Parse non-admission `S` is mapped to `NO-GO` at `skinny/crates/bbnf-bench/src/gate.rs:70` through `skinny/crates/bbnf-bench/src/gate.rs:109`, and strict admission rejects `K`, `S`, `deferred`, `view-boundary`, plane mismatches, stale freshness, and sidecar freshness at `skinny/crates/bbnf-bench/src/gate.rs:133` through `skinny/crates/bbnf-bench/src/gate.rs:170`.

## Evidence Inspected

- Commit `6d8cb701` (`feat(sk-v8-wave0): enforce telemetry manifest gate`) and current tree. `git diff --name-only 6d8cb701..HEAD` returned no changed files, so the reviewed current tree matches the commit for W0-relevant files.
- `skinny/RESULTS.md`, especially the 38 generated rows at `skinny/RESULTS.md:5` through `skinny/RESULTS.md:42`, the SK-V8 W0 telemetry manifest at `skinny/RESULTS.md:44` through `skinny/RESULTS.md:85`, and W0 notes at `skinny/RESULTS.md:260` through `skinny/RESULTS.md:265`.
- `skinny/crates/bbnf-bench/src/bin/gate.rs`, including telemetry construction, run facts, comparator evidence, substrate facts, and profile/hot-leaf helpers.
- `skinny/crates/bbnf-bench/src/report.rs`, including SK-V8 telemetry schema, manifest rendering, W0 validation, baseline constants, comparator validation, and W0 tests.
- `skinny/crates/bbnf-bench/src/gate.rs`, including strict admission evidence and `S` non-admission handling.
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`, confirming the Wave 0 source edits are scoped as telemetry/gate schema allowlist entries.
- Local verification: `cargo test -p bbnf-bench --lib` passed 40 tests.

## Exact Remediation If Rejected

1. Make `w0_comparator_evidence` carry the correct native source path per workload. Parse-only rows may point at `sonic_rs_anchor` and `serde_json`; direct-to-struct rows must point at `sonic_rs_direct_to_struct` and `serde_json_direct_to_struct`; real-typed rows must point at `sonic_rs_real_typed_struct` and `serde_json_real_typed_struct`. Preserve sidecar `historical:` and `absent:` semantics as separate evidence classes.

2. Extend `validate_comparator_evidence` or row-level W0 validation so native comparator source paths are checked against `row_id` and `measured_validation_path`. Add a regression test that mutates a direct or typed row to use `sonic_rs_anchor` or generic `serde_json` source paths and requires rejection.

3. Replace Criterion-estimate-only `profile_artifact` and `profile_hot_leaf` with real profile provenance, or explicitly rename and gate them as sample-cost evidence rather than profile evidence. If W0 is allowed to use Criterion only, the SPEC and manifest labels must say so directly; otherwise require a profiler artifact class and hot-leaf identifier that is validated for shape and, where possible, file existence.

4. Add a W0 validator test that rejects `profile_artifact=criterion:*estimates.json` when no profiler artifact is present and rejects `hot-leaf=criterion-slope` as the sole hot-leaf proof.

5. Regenerate `skinny/RESULTS.md` through the gate, then rerun `cargo test -p bbnf-bench --lib` plus the W0 advisory gate command and confirm all 38 throughput cells remain within the existing +/-1.0% baseline window.
