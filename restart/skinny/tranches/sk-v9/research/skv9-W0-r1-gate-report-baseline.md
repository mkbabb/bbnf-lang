# SK-V9 W0 R1: Gate/Report Baseline Migration

Date: 2026-05-18.
Scope: Gate/report source changes needed for SK-V9-open telemetry-lock, with no behavior movement and no row admission.
Output: `restart/skinny/tranches/sk-v9/research/skv9-W0-r1-gate-report-baseline.md`.

## §1 — Findings (concrete, file:line cited)

1. W0 is mandatory and gate-only. The SK-V9 handoff says S-P1 V1 is an opening gap ledger, not a completed profile, and requires a recovery W0 telemetry-lock before behavior candidates proceed (`restart/skinny/tranches/sk-v9/HANDOFF.md:5-9`). The next move requires `gate-json` as same-wave consumer, no parser/scanner/SIMD/codegen behavior movement, no row admission, and no strict admission from deferred/view-boundary rows (`restart/skinny/tranches/sk-v9/HANDOFF.md:70-80`). The Alpha shortlist narrows this candidate to gate/report refresh only and forbids parser, scanner, SIMD, asm, codegen, generated output, product behavior, or row throughput changes (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:424-426`).

2. S-P1 V1 blocks behavior waves because SK-V9-open evidence is absent. The hardening consolidation records 2/6 ACCEPT, no SK-V9-open manifest, no fresh 17-corpus samply captures, no top-symbol self-time tables, no PMU/cycles rows, no masking-probe telemetry, and no fresh delta against a SK-V9 run id (`restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md:8-25`). It folds the next executable action to a recovery W0 manifest consumed by `gate-json`, behavior frozen, with no throughput-cell movement or Apache/CITM measured row admission (`restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md:38-58`).

3. The current report/gate source is partially relabeled to SK-V9-open but still structurally named and guarded as SK-V8 W0. `TelemetryRow` still owns a `sk_v8: SkV8Telemetry` field (`skinny/crates/bbnf-bench/src/report.rs:70-91`), the validator is still named `validate_sk_v8_w0` (`skinny/crates/bbnf-bench/src/report.rs:275-275`, `skinny/crates/bbnf-bench/src/report.rs:494-494`), and the baseline type/function are still `SkV8OpenBaseline` / `sk_v8_open_baseline` even though current diagnostics now say SK-V9-open (`skinny/crates/bbnf-bench/src/report.rs:647-669`, `skinny/crates/bbnf-bench/src/report.rs:931-935`). This is a migration seam that should be resolved intentionally, not by further string-only replacement.

4. The row set lock is the strongest existing no-admission guard and should be preserved. `validate_sk_v8_w0` requires the report row count to equal the frozen baseline, rejects duplicate or unknown row ids, rejects changed outcome/verdict, and validates Track 1 and Track 2 against baseline deltas (`skinny/crates/bbnf-bench/src/report.rs:494-525`). The baseline delta helper rejects movement above 1.0% (`skinny/crates/bbnf-bench/src/report.rs:937-953`). The baseline array is 38 current rows and still excludes Apache/CITM `real_typed_struct` measured rows (`skinny/crates/bbnf-bench/src/report.rs:669-918`; current manifest rows `skinny/RESULTS.md:44-85`).

5. The current source/RESULTS state is internally split. Current `report.rs` renders a `## SK-V9 W0 Telemetry Manifest` heading and `SK-V9-open delta` column (`skinny/crates/bbnf-bench/src/report.rs:575-578`), while `skinny/RESULTS.md` still renders `## SK-V8 W0 Telemetry Manifest`, `SK-V8-open` rows, and the SK-V8 run id (`skinny/RESULTS.md:44-48`). The binary now emits an SK-V9 note (`skinny/crates/bbnf-bench/src/bin/gate.rs:315-318`), but `RESULTS.md` still notes SK-V8 W0 telemetry (`skinny/RESULTS.md:138-141`). Therefore the migration needs a same-wave report refresh or the checked-report path will correctly see stale output.

6. The SK-V9 schema requires SK-V8 delta, not an ambiguous "SK-V9-open delta." Pass Alpha requires `Delta vs SK-V8`, `Run id`, host/build metadata, comparator identity/plane/strictness/freshness, validation path, substrate fields, consumer class, Track 2 independence, and signal (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:242-297`). Current `report.rs` changed the manifest header to `SK-V9-open delta` (`skinny/crates/bbnf-bench/src/report.rs:575-578`) while the telemetry struct field remains `sk_v8_open_delta` (`skinny/crates/bbnf-bench/src/report.rs:59-61`, `skinny/crates/bbnf-bench/src/bin/gate.rs:489-492`). W0 should distinguish `wave_id=SK-V9-open` from `delta_vs_skv8=baseline/same`, not collapse them.

7. Strict admission is already fail-closed in the reusable gate helper and in the W0 report validator. `validate_strict_admission` rejects non-native comparators, non-GO outcomes, non-strict rows, non-measured UTF-8, output-plane mismatches, non-measured validation paths, stale/historical/absent comparator freshness, and sidecar freshness for native strict admission (`skinny/crates/bbnf-bench/src/gate.rs:136-183`). W0 report validation also rejects non-deferred strictness, non-view-boundary validation, non-view-boundary `parse_utf8`, and incomplete escape validation (`skinny/crates/bbnf-bench/src/report.rs:1096-1122`). These checks are load-bearing for the SK-V9-open lock.

8. Sidecar evidence remains historical/absent unless a structured same-run sidecar manifest lands later. The Alpha shortlist requires W0 negative tests for mixed capture, run-id drift, stale sidecar strict claims, and producer-only telemetry (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:447-453`). The current report validator rejects sidecar-same-run claims without a structured manifest and validates historical/absent sidecar source shapes (`skinny/crates/bbnf-bench/src/report.rs:1263-1311`). Current `RESULTS.md` states C++ sidecars are historical or absent and never strict anchors in W0 (`skinny/RESULTS.md:141`).

9. `gate-json --check-results` needs explicit SK-V9-open run-id consumption, not text fallback. `xtask` accepts `--check-results` and passes it through to the gate binary (`skinny/xtask/src/main.rs:241-272`). The gate binary compares rendered markdown to `RESULTS.md` when not updating results (`skinny/crates/bbnf-bench/src/bin/gate.rs:329-339`). The cost-facts check has a separate text scan for 38 manifest rows and required SK-V9 markers (`skinny/xtask/src/main.rs:305-321`), but that scan is string containment, not row-wise manifest validation. W0 should use the same report validator for row-wise identity and run-id drift instead of relying only on `text.contains`.

10. Diagnostic surfaces need non-producer fences before they can be rendered as SK-V9-open telemetry. P1 hardening CH5 requires structural-scan-only evidence to be labeled `diagnostic_nonproducer` and not feed row admission, tape/cursor state, or `ValueRef` contracts (`restart/skinny/tranches/sk-v9/research/p1/hardening/V1/CH5.md:40-61`). It also requires `cycles_per_byte` and masking probes to remain diagnostic sections, not Track 1, Track 2, strict admission, direct product proof, or Apache/CITM measured-row evidence (`restart/skinny/tranches/sk-v9/research/p1/hardening/V1/CH5.md:63-85`, `restart/skinny/tranches/sk-v9/research/p1/hardening/V1/CH5.md:109-130`). Current probe report rows only render corpus/probe/Mbps/ns/ratio/signal (`skinny/crates/bbnf-bench/src/report.rs:93-101`, `skinny/crates/bbnf-bench/src/report.rs:612-625`), and `bin/gate.rs` can populate multiple probes (`skinny/crates/bbnf-bench/src/bin/gate.rs:1501-1540`).

## §2 — Recommendations (named falsifiability gates)

1. Recommendation R1 - make the open-baseline model explicit, not SK-V8/SK-V9 string-only.
   Source change: rename or wrap `SkV8Telemetry`, `SkV8OpenBaseline`, `SK_V8_OPEN_RUN_ID`, `sk_v8_open_baseline`, and `validate_sk_v8_w0` behind a neutral `OpenBaseline`/`TelemetryLock` API, or add an explicit `BaselineEpoch { source_baseline: SK-V8-open, wave_id: SK-V9-open }` object. Keep the existing 38-row SK-V8 throughput/outcome table as the comparison floor.
   Falsifiability gate `skv9_open_baseline_identity`: `cargo xtask gate-json --advisory --check-results` passes only when every manifest row has `wave_id=SK-V9-open`, the chosen `sk-v9-open:*` run id, and a separate `Delta vs SK-V8`/baseline field; it fails if any row silently falls back to `SK-V8-open`, if type/validator code accepts mixed wave ids, or if the rendered manifest replaces SK-V8 delta semantics with a self-delta.

2. Recommendation R2 - preserve the 38-row no-admission lock.
   Source change: keep `SK_V8_OPEN_BASELINE` as the row-admission floor for W0, or rename it without changing contents. Do not add Apache/CITM `real_typed_struct` rows in W0. Keep `w0_real_typed_metadata_expected` tied to measured baseline rows, not source/product fixture existence (`skinny/crates/bbnf-bench/src/bin/gate.rs:1116-1118`).
   Falsifiability gate `skv9_open_manifest_count`: the manifest has exactly 38 main rows; `json/apache_builds/real_typed_struct/main` and `json/citm_catalog/real_typed_struct/main` are absent unless a separate measured typed-row gate has already passed; any added/removed row, outcome/verdict change, or Track 1/Track 2 movement above 1.0% fails.

3. Recommendation R3 - wire `check-results` to row-wise SK-V9 run-id validation.
   Source change: have `gate-json --check-results` parse or reuse the rendered report model and validate the chosen SK-V9-open run id, row count, row ids, outcome/verdict lock, and delta lock. Replace the cost-facts text-containment marker check with row-wise manifest validation or call into the same validator.
   Falsifiability gate `skv9_run_id_drift_detector`: a coherent same-run Criterion capture passes; a mixed capture, input hash mismatch, byte mismatch, missing metadata row, per-row run-id drift, or dynamic admitted-row mutation fails closed. A `RESULTS.md` containing both SK-V8-open and SK-V9-open manifest rows must fail.

4. Recommendation R4 - keep strict admission impossible in W0.
   Source change: preserve `validate_strict_admission` and `validate_w0_admission_boundary` behavior while migrating names. W0 may render validation state more explicitly, but cannot set row strictness to `strict`, `parse_utf8` to `measured-row`, or sidecar freshness to same-run sidecar as strict evidence.
   Falsifiability gate `skv9_strict_boundary`: mutating any W0 row from `Strictness=deferred` to `strict`, any validation path from `view-boundary` to `measured-row`, or any historical/absent sidecar into strict same-run evidence makes `cargo test -p bbnf-bench --lib --bins` or `cargo xtask gate-json --advisory --check-results` fail.

5. Recommendation R5 - add diagnostic non-producer metadata before probe/PMU expansion.
   Source change: extend `ProbeReportRow` and probe rendering/validation with fields like `producer_class`, `track_role`, `substrate_output`, and `strict_admission`, and add equivalent manifest fields for `cycles_per_byte` if it is rendered. Do not reuse main Track 1/Track 2 columns for probes.
   Falsifiability gate `skv9_diagnostic_nonproducer_fence`: any `cycles_per_byte`, structural-scan, or masking-probe row that claims Track 1, Track 2, strict admission, product proof, retained cursor state, sidecar producer status, or Apache/CITM measured-row evidence fails. A probe row without explicit non-producer metadata also fails.

6. Recommendation R6 - prove behavior freeze at source and output boundaries.
   Source change: limit W0 redress to gate/report/metadata/xtask and the rendered report/REDRESS surfaces authorized by the Alpha shortlist (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:428-436`). Do not touch parser, scanner, SIMD, asm, codegen, generated output, runtime JSON behavior, or typed/direct product behavior.
   Falsifiability gate `skv9_refresh_no_behavior_drift`: `cargo xtask check-json`, `cargo xtask check-real-typed`, `cargo xtask check-conformance`, `cargo test -p bbnf-bench --lib --bins`, and a generated-output diff all pass; throughput cells remain within +/-1.0% of SK-V8-open unless a separate row-admission gate is explicitly selected and accepted.

## §3 — Risks (REDRESS entries to pre-block)

1. REDRESS 91 overclaim risk: Apache/CITM are source/product parity rows only, not measured `RESULTS.md` rows (`skinny/REDRESS.md:2620-2659`). W0 must not turn source-only typed fixtures into measured `real_typed_struct A / GO` rows.

2. REDRESS 91 Canada shortcut risk: `canada/real_typed_struct` remains rejected on full-fixture DirectBuild-vs-serde checksum mismatch, and W0 must not weaken that to length-only or digest-only proof (`skinny/REDRESS.md:2637-2640`).

3. REDRESS 92 structural parse risk: structural-heavy parse implementation remains routed until retained class/event grammar and retained `ValueRef` cursor proof exist; W0 telemetry must not create parser-owned cursor/fact slots or a side substrate (`skinny/REDRESS.md:2661-2690`).

4. REDRESS 93 direct digest risk: direct digest misses remain guard-plane evidence until a direct output contract or control-path tranche exists; W0 must not turn digest evidence into product proof or reopen scalar-parent folding under another name (`skinny/REDRESS.md:2692-2729`).

5. Partial migration risk: current source has in-flight SK-V9 relabeling while `RESULTS.md` still carries SK-V8 manifest text (`skinny/crates/bbnf-bench/src/report.rs:575-578`; `skinny/RESULTS.md:44-48`). A redress commit that changes labels without same-wave `RESULTS.md` refresh and row-wise gate consumption will create stale-report failure instead of a telemetry lock.

6. Generic policy leak risk: the SK-V9 telemetry contract says `gate-json` is the JSON instance of a grammar-aware report contract and generic report/gate code must not encode JSON comparator policy as universal schema (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:242-248`). Keep JSON comparator details local to the JSON gate/report path.

## §4 — Sources (every external citation)

No external sources were used. Repository sources read:

- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/skinny/tranches/sk-v9/HANDOFF.md`
- `restart/skinny/tranches/sk-v9/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v9/research/p1/hardening/V1/CH4.md`
- `restart/skinny/tranches/sk-v9/research/p1/hardening/V1/CH5.md`
- `restart/skinny/tranches/sk-v9/research/p1/hardening/V1/CH6.md`
- `restart/skinny/tranches/sk-v9/research/p1/p1d-pmu-cycles.md`
- `restart/skinny/tranches/sk-v9/research/p1/p1f-results-delta.md`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/gate.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/xtask/src/main.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
