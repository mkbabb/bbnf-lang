# SK-V14 W0A: Telemetry Schema
Date: 2026-05-24.
Scope: W0 telemetry fields and current RESULTS/gate support.
Output: this file.

## §1 — Findings (concrete file:line cited)

1. SK-V14 does not require the visible `skinny/RESULTS.md` table to grow if the added fields are gate-consumed elsewhere: Section 0.4 permits rendered columns, a manifest, or a JSON payload, but says `xtask gate-json` must consume the fields in the same wave (`restart/skinny/tranches/sk-v14/SPEC.md:120-124`). The required additions are `track2_entry_point`, `comparator_plane`, `per_iter_equality`, `audit_overlay_verdict`, the Lock 1 triple, sidecar freshness, and `SK-V14-open delta` (`restart/skinny/tranches/sk-v14/SPEC.md:135-159`), with missing fields, stale sidecars, producer-only telemetry, and W0 behavior drift rejecting the wave (`restart/skinny/tranches/sk-v14/SPEC.md:162-167`).

2. Section 3 makes this W0 scope report/gate-only: capture `SK-V14-open`, add the Section 0.4 fields, populate profile/run/host/build/cost/freshness/audit/delta telemetry, reject stale or malformed sidecar evidence, create the Lock 14 baseline, and leave parser/scanner/SIMD/codegen/generated behavior unchanged (`restart/skinny/tranches/sk-v14/SPEC.md:343-361`). Same-wave consumption is explicit: `xtask gate-json` must consume every emitted telemetry field and reject malformed or missing evidence in the W0 slice (`restart/skinny/tranches/sk-v14/SPEC.md:363-365`).

3. The current visible results table already carries many SK-V8 carry-forward fields: corpus, workload, outcome, verdict, strictness, `parse_utf8`, `escape_complete`, `flaw_probe`, output plane, Track 1/2 Mbps, comparator Mbps, deltas, hot leaf, and signal (`skinny/RESULTS.md:3`). The Rust renderer hard-codes that 26-column schema as `SCHEMA_V3_HEADER` (`skinny/crates/bbnf-bench/src/report.rs:8`) and renders it row-by-row without SK-V14 visible columns (`skinny/crates/bbnf-bench/src/report.rs:3428-3466`).

4. The current hidden manifest already carries row/run/build/profile/cost/substrate/comparator evidence, but it is labeled `SK-V9 W0`, not `SK-V14-open`: the manifest header is `Row id ... Comparator evidence` (`skinny/RESULTS.md:51-53`), a representative row shows same-run native `sonic_rs_strict` plus historical or absent sidecar comparators (`skinny/RESULTS.md:55`), and the renderer emits the manifest with `SK-V9-open delta`, `Substrate`, `Structural projection`, `Cardinality`, `Track 2`, and `Comparator evidence` columns (`skinny/crates/bbnf-bench/src/report.rs:3468-3503`).

5. The Rust schema has comparator-plane and sidecar freshness only as per-comparator evidence, not as SK-V14 per-row columns: `SkV8ComparatorEvidence` contains `comparator_plane`, `comparator_strictness`, `comparator_freshness`, and `sidecar_freshness` (`skinny/crates/bbnf-bench/src/report.rs:34-43`). `SkV8Telemetry` contains row id, validation path, profile artifact, sample cost/count, build/host/feature metadata, costfacts, redress/wave/run, `sk_v9_open_delta`, substrate surface/projection/cardinality, same-wave consumer, and track2 independence (`skinny/crates/bbnf-bench/src/report.rs:48-73`). `TelemetryRow` has no `per_iter_equality`, `audit_overlay_verdict`, `track2_entry_point`, `substrate_target`, `retention_lifetime`, `policy_owner`, or `sk_v14_open_delta` field (`skinny/crates/bbnf-bench/src/report.rs:75-97`).

6. Criterion metadata captures a useful subset but not the SK-V14 additions. `RowMetadata` has `api_symbol`, `sidecar_freshness`, hot leaf, profile/build/host-ish fields, and workload semantics (`skinny/crates/bbnf-bench/src/metadata.rs:20-65`), and `required_fields_present` checks those fields are non-empty (`skinny/crates/bbnf-bench/src/metadata.rs:355-390`). It does not expose a row-level `track2_entry_point`, per-row comparator-plane binding, per-iter equality, audit overlay enum, Lock 1 triple, or SK-V14 delta.

7. `gate-json` currently runs two layers. `xtask` validates `--check-results` only via `validate_w0_results_snapshot` before invoking `bbnf-bench --bin gate` (`skinny/xtask/src/main.rs:242-260`). That snapshot validator reads `RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md` (`skinny/xtask/src/main.rs:355-368`), but the rolling authority is still `schema_version: sk-v13-rolling-sota-delta-v1` and `run_id: SK-V13-open` (`restart/skinny/ROLLING-SOTA-DELTA.md:3-8`), so no `SK-V14-open` identity or delta is currently validated.

8. The bench gate validates generated reports before rendering: it calls `report.validate_schema_v3().and_then(|_| report.validate_sk_v8_w0())` and exits invalid on error (`skinny/crates/bbnf-bench/src/bin/gate.rs:720-729`). `validate_schema_v3` requires the visible 26-column values and required Track/comparator Mbps (`skinny/crates/bbnf-bench/src/report.rs:3017-3070`), while `validate_sk_v8_w0` requires the hidden manifest fields and checks W0 sentinels, run id, sample cost, profile/hot leaf, manifest semantics, comparator evidence, and W0 admission boundary (`skinny/crates/bbnf-bench/src/report.rs:3073-3182`).

9. Outcome validation exists, but it is SK-V8/W0 shaped rather than SK-V14 audit shaped. `gate::parse_outcome_id` recognizes A/B/C/D/E/F/G/I/J/K/L/M/N-direct/S (`skinny/crates/bbnf-bench/src/gate.rs:115-132`), while W0 report validation narrows this to A/C/G/I/J/K/L/M/N-direct/S (`skinny/crates/bbnf-bench/src/report.rs:3855-3864`). There is no outcome cross-check against `audit_overlay_verdict` because that field is absent.

10. Strict comparator validation is partially present. `validate_strict_admission` rejects non-native strict comparators, non-GO outcomes, non-strict rows, view-boundary UTF-8, row/comparator plane mismatch, non-measured validation, stale/historical/absent comparator freshness, and sidecar-backed strict admission (`skinny/crates/bbnf-bench/src/gate.rs:136-183`). The current W0 path also validates comparator slots, sidecar absence reasons, and rejects `sidecar-same-run` without a structured manifest (`skinny/crates/bbnf-bench/src/report.rs:4548-4640`, `skinny/crates/bbnf-bench/src/report.rs:4676-4724`). This is close to the sidecar freshness slice, but still lacks SK-V14 per-row `comparator_plane`, `per_iter_equality`, and audit overlay consumption.

11. Native comparator source/plane validation exists for current JSON shapes: `sonic_rs_strict` parse-only must be `DOM`, direct must be `digest`, typed must be `typed direct`, and both `sonic_rs_strict`/`serde_json` must be same-run native with `sidecar=n/a` and a matching Criterion source (`skinny/crates/bbnf-bench/src/report.rs:4735-4788`). SK-V14 R1 is stricter: p3d says `comparator_plane` is re-bound from this per-comparator subfield into a per-row required column that rejects asymmetric work such as eager DOM materialization for direct/typed rows (`restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md:18-23`).

12. Lock 1 v+1 vocabulary is defined outside the skinny report schema: candidates/consumers must declare `substrate_target`, `retention_lifetime`, and `policy_owner`, with allowed values `local_temp_only|existing_tape|direct_sink|admitted_fact_output`, `local_loop|generated_function|output_row`, and `generated_grammar|caller_data|none` (`restart/locks/LOCKS.md:118-124`). Current report fields `substrate_surface`, `structural_projection_status`, and `substrate_cardinality` are not the required triple (`skinny/crates/bbnf-bench/src/report.rs:65-70`, `skinny/crates/bbnf-bench/src/report.rs:4420-4494`).

13. The Lock 16/overlay binding already says every gate-consumed `RESULTS.md` row must carry `track2_entry_point`, `comparator_plane`, `per_iter_equality`, and `audit_overlay_verdict`, and `xtask gate-json` rejects any missing one (`restart/locks/LOCKS.md:213-217`). Current `RESULTS.md` has none of those four as row-level columns or manifest fields, and a repository search found SK-V14 field names only in docs/locks, not in `skinny/crates`, `skinny/xtask`, or `skinny/RESULTS.md`.

14. The row universe needs an explicit W0 decision. The SK-V14 gate target is 51 JSON cells plus 24 CSS L4 features (`restart/skinny/tranches/sk-v14/SPEC.md:354-360`; `restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md:23-25`). Current `skinny/RESULTS.md` contains 45 JSON main table rows and 31 `css_l4/` manifest rows by static `rg` count, while `ROLLING-SOTA-DELTA.md` lists 51 JSON rows including six `absent:product-surface-not-generated` typed rows (`restart/skinny/ROLLING-SOTA-DELTA.md:14-64`) and 24 CSS target rows (`restart/skinny/ROLLING-SOTA-DELTA.md:66-80`). W0 should make `gate-json` validate the SK-V14 75-row set explicitly rather than inheriting this mixed surface.

## §2 — Recommendations (named falsifiability gates)

1. **G-W0A-SKV14-SCHEMA-PRESENCE** — Add a gate-consumed SK-V14 manifest/payload beside the existing 26-column table, not a behavior harness change. The payload should be one record per SK-V14 main row with: `row_id`, `track1_entry_point`, `track2_entry_point`, row-level `comparator_plane`, `per_iter_equality`, `audit_overlay_verdict`, optional `audit_overlay_reference`, `substrate_target`, `retention_lifetime`, `policy_owner`, `sidecar_freshness`, and `sk_v14_open_delta`. Falsifier: delete any field from any row and `cargo xtask gate-json --check-results` must print the row id + field and exit non-zero.

2. **G-W0A-ROWSET-75** — Define the W0 row set in gate code as 51 JSON cells plus 24 CSS L4 features. For JSON typed cells without product surface, require explicit non-admission telemetry rather than omitting the row. For CSS, either filter to the 24 SK-V14 target features or explain and gate every extra feature as out-of-scope/non-main. Falsifier: remove one JSON cell or add an unclassified CSS feature and `gate-json --check-results` rejects before comparator logic.

3. **G-W0A-R1-COMPARATOR-PLANE** — Lift `comparator_plane` out of `SkV8ComparatorEvidence` into the SK-V14 per-row payload while preserving the existing per-comparator evidence. Gate mapping should be plane-specific: parse-only `sonic_rs::Skipper`/structural skip, direct `<corpus>::strict_struct_deser`, typed `<corpus>::typed_strict_struct_deser`, CSS `lightningcss full-parse` plus CSS oracle. Falsifier: set any direct/typed row to `sonic_rs::from_slice::<Value>` or any comparator whose work is DOM-shaped while the output plane is digest/typed; gate must reject.

4. **G-W0A-R2-PER-ITER-EQUALITY** — Require non-empty `per_iter_equality` on every W0 row. For rows claiming `A/GO`, require `pass_all_iters=true`, `iter_count>0`, and `mismatch_count=0`; for W0 non-admits or audit-falsified rows, permit only explicit non-admit values such as `not_admitted:pre-W1-no-in-row-equality` and ensure they cannot support GO. Falsifier: blank the field, set `mismatch_count=1`, or pair non-admit equality with `A/GO`; gate rejects.

5. **G-W0A-AUDIT-OVERLAY** — Add enum parsing for `AUDIT-FALSIFIED|AUDIT-SUSTAINED|AUDIT-PENDING`. W0 should pre-populate falsified CSS/JSON rows per SPEC Section 3 and make `AUDIT-FALSIFIED + A/GO` illegal unless a fresh material differential and validation-pack reference are present. Falsifier: leave a current audited CSS row at `A/GO` with `AUDIT-FALSIFIED` and no reference; gate rejects.

6. **G-W0A-TRACK2-ENTRY-CH5** — Require `track1_entry_point` and `track2_entry_point` symbol paths, then common-prefix check them. JSON should bind Track 1 to generated runtime entry points and Track 2 to the independent bench oracle; CSS should bind generated parser vs cssparser/golden/lightningcss oracle as applicable. Falsifier: set Track 2 to any generated Track 1 path or to a private `runtime::tape::*` internal beyond public `Tape`/`OffsetFlags`; gate rejects.

7. **G-W0A-LOCK1-TRIPLE** — Add a deterministic mapping from current substrate facts to the Lock 1 triple, then validate the lock vocabulary from `LOCKS.md`. Suggested W0 mapping: parse-only uses `existing_tape/local_loop/none`; direct sink rows use `direct_sink/generated_function/generated_grammar`; typed direct rows use `direct_sink/generated_function/generated_grammar`; CSS fact-stream rows use `admitted_fact_output/output_row/generated_grammar`. Falsifier: any value outside the allowed sets or any SIMD/union/cost-shape consumer without the triple rejects.

8. **G-W0A-SIDECAR-FRESHNESS** — Keep the existing per-comparator `sidecar_freshness` validation, but surface a row-level summary in SK-V14 telemetry: `absent:<reason>` when no sidecar Mbps is cited, `historical:<source>` for planning-only values, and no `sidecar-same-run` until a structured sidecar manifest parser exists. Falsifier: populate sidecar Mbps with `absent:*`, cite `sidecar-same-run`, or use historical sidecar evidence as a strict anchor; gate rejects.

9. **G-W0A-SKV14-OPEN-DELTA** — Dual-write or rename `sk_v9_open_delta` to `sk_v14_open_delta` in the new payload. W0 rows should read `baseline`; future maintain rows should carry a numeric delta checked against the ±1.0% W0 budget. Also update `validate_w0_results_snapshot` away from the SK-V13 rolling identity so `SK-V14-open` is the captured authority. Falsifier: leave `run_id: SK-V13-open`, omit `sk_v14_open_delta`, or move a throughput cell beyond ±1.0% without REDRESS; gate rejects.

10. **G-W0A-NO-BEHAVIOR-DIFF** — Scope the implementation to `skinny/crates/bbnf-bench/src/report.rs`, `skinny/crates/bbnf-bench/src/bin/gate.rs`, `skinny/crates/bbnf-bench/src/gate.rs`, `skinny/xtask/src/main.rs`, tests, and `skinny/RESULTS.md`/rolling docs if the implementing agent is authorized. No parser/runtime/codegen/SIMD/generated output edits. Falsifier: any file outside the allowed W0 telemetry/gate/report surface changes; gate or review rejects.

## §3 — Risks (REDRESS entries to pre-block)

1. **REDRESS 50-55 / P-2 comparator weakness** — Pre-block any route that treats per-comparator `comparator_plane` as sufficient, reuses `sonic_rs::from_slice::<Value>` for direct/typed admission, or lets lossy/permissive comparators become SOTA anchors. The p3d schema explicitly ties these entries to the new per-row R1 field and asymmetric-work rejection (`restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md:130`, `restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md:137-140`).

2. **REDRESS 28/33 and P-3 startup-only oracle** — Pre-block `per_iter_equality` values that are startup checksums, one-shot parity, or post-timing validation. p3d calls out that allowing startup-only values would reopen REDRESS 28/33 (`restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md:146-150`).

3. **REDRESS 60-72 stale sidecar/SOTA anchor** — Pre-block historical C/C++ sidecars as strict anchors and any row with populated sidecar Mbps but missing freshness/source coverage. p3d maps REDRESS 60-72 to stale sidecar freshness and requires same-run-native enforcement on strict anchors (`restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md:131`).

4. **REDRESS 80, 82-84, 88, 89 / P-4 producer-only telemetry** — Pre-block any SK-V14 field that is rendered but not parsed by `gate-json`. p3d says every emitted column must be consumed in the same wave and producer-only telemetry fails the wave (`restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md:12`, `restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md:132`).

5. **REDRESS 119/120 / P-5 substrate union breach** — Pre-block schema wording that normalizes sidecar event vectors, aux density tables, parser-owned cursors, or retained structural projections as admissible substrate. p3d ties REDRESS 119/120 to `structural_projection_status`, `substrate_cardinality`, and Lock 1 substrate-union enforcement (`restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md:134`).

6. **REDRESS 126 / P-6 hidden coupling** — Pre-block any Track 2 proof that shares Track 1 generated entry points or private runtime tape internals. p3d makes `track2_entry_point` plus common-ancestor rejection the kill switch for REDRESS 126 (`restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md:135`).

7. **Lock 14 fake generated header route** — Pre-block adding or preserving fake `@generated` headers outside a recognized emission roster. W0 SPEC requires `xtask gate-json` to reject new fake generated headers (`restart/skinny/tranches/sk-v14/SPEC.md:349-360`), while current gate only calls `lock14_baseline::validate` before report validation (`skinny/crates/bbnf-bench/src/bin/gate.rs:45-48`); the W0 slice should keep this as a companion lint, not a telemetry-only note.

## §4 — Sources

- `restart/skinny/tranches/sk-v14/SPEC.md` Section 0.4 and Section 3.
- `restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md`.
- `restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md`.
- `restart/locks/LOCKS.md`.
- `skinny/RESULTS.md`.
- `restart/skinny/ROLLING-SOTA-DELTA.md`.
- `skinny/xtask/src/main.rs`.
- `skinny/crates/bbnf-bench/src/report.rs`.
- `skinny/crates/bbnf-bench/src/bin/gate.rs`.
- `skinny/crates/bbnf-bench/src/gate.rs`.
- `skinny/crates/bbnf-bench/src/metadata.rs`.
