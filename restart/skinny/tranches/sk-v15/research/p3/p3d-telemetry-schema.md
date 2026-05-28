# SK-V15 P3-D: Telemetry Schema Binding

Pass: S-P3 Synthesis-Plan. Cycle: V2.
Date: 2026-05-28.
Scope: Bind the `skinny/RESULTS.md` telemetry schema for SK-V15, carrying the SK-V8 schema-v3 surface and adding SK-V15 anti-broadcast and gate-exclusion fields.
Output: this file.
Pass Alpha goalset: SK-V15 closes only with 51 / 51 JSON rows still strict and same-plane, no CSS 24-row broadcast admit, CSS typed value/CSSOM truth before CSS admission, full Lock 14 / Lock 16 scan coverage, native Apple M5 Max / aarch64 admission, and executable close evidence.
Candidate pool: `research/p2/` post-CHALLENGE survivors locked by `HARDENING-S-P2-V3-CONSOLIDATED.md`.

## Section 1 - Synthesis

P3-D is a gate contract, not an implementation plan. S-P3 requires this artifact to bind the `skinny/RESULTS.md` column schema, carry the SK-V8 schema-v3 surface, name SK-V15 additions, and specify `gate-json` rejection rules for missing columns (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:61`, `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:239-244`).

SK-V15 inherits two already-live surfaces:

- The rendered schema-v3 RESULTS table headed by `Corpus` through `Signal` (`skinny/RESULTS.md:3`, `skinny/crates/bbnf-bench/src/report.rs:8`). The prompt shorthand says "24-column"; SK-V8 already resolved that as the schema-v3 visible surface while the live rendered header has 26 columns. SK-V15 preserves the live order and names exactly.
- The SK-V8 report/gate telemetry fields from SPEC Section 0.4. They may render as table columns, a gate-consumed manifest, or a gate-consumed JSON payload, but every emitted field must be consumed by `gate-json`; missing required fields, stale sidecars, producer-only telemetry, Lock 14 generic leaks, and cap overflow reject the wave (`restart/skinny/tranches/sk-v8/SPEC.md:103-146`).

The current carrier is the SK-V14 W0 telemetry manifest. `xtask` parses 32 manifest cells, expects 51 JSON rows plus 24 CSS rows, and validates row uniqueness, manifest presence, and SK-V14 sustained-row semantics (`skinny/xtask/src/main.rs:486-584`). That carrier is not sufficient for SK-V15 because the CSS manifest still repeats the same W8R timing tuple across 24 conceptual rows (`skinny/RESULTS.md:112-135`), and SK-V15 requires no hidden one-to-N measurement stamps (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:121-127`).

SK-V15 therefore extends the carrier with ten required fields:

`measurement_row_id`, `measurement_origin`, `value_plane`, `css_comparator_workload`, `generator_source`, `lock14_scan_scope`, `lock16_status`, `checkasm_or_parity_status`, `gate_exclusion_report`, and `broadcast_group_id`.

The W0..W11 topology consumes those fields in four load-bearing places:
W0 creates the carrier/schema, W1 collapses or demotes the CSS broadcast
admission, W5 provides typed CSS value/document output, and W6 performs
the fresh same-workload retiming that can set a CSS typed-admission floor.

## Section 2 - Deliverable

### 2.1 Carrier Surface

The SK-V15 schema is three-layered:

1. **Visible schema-v3 table, unchanged.** Preserve every current visible column from `Corpus` through `Signal`; do not rename `Output plane`, Track 1/2 Mbps, comparator Mbps, deltas, `Hot leaf`, or `Signal`.
2. **SK-V8 required telemetry, unchanged.** Preserve the SK-V8 fields: `row_id`, `grammar_id`, `domain`, comparator identity/plane/strictness/freshness, measured validation path, profile/sample/build/host/feature metadata, CostFacts fields, redress/wave/run ids, sidecar freshness, open delta, substrate surface/projection/cardinality, same-wave consumer, and Track 2 independence.
3. **SK-V15 required additions.** Add the ten fields below to the manifest or to an equivalent gate-consumed JSON payload. If rendered beside the SK-V14 manifest, this is a 42-slot row: the existing 32 manifest slots plus the 10 SK-V15 slots.

No field is optional by omission. A non-applicable value must be explicit, such as `n/a:not-css`, `not-applicable:no-simd-or-asm`, or `none:full-surface-scan`.

### 2.2 SK-V15 Additions

| Field | Required meaning | Gate rejection rule |
|---|---|---|
| `measurement_row_id` | Stable identity for the timing row that produced the row's Track 1 / comparator tuple. A conceptual feature row and its measurement row are not the same thing unless explicitly aggregate. | Empty or missing rejects. Duplicate value across multiple `A / GO` rows rejects unless every duplicate row shares an explicit aggregate `broadcast_group_id` and is non-admission diagnostic, or the schema has collapsed those rows into one aggregate row. |
| `measurement_origin` | Command, artifact, TSV row, corpus slice, and run id that produced the timing tuple. | Empty or unresolved artifact rejects. CSS rows reject if this points to a shared full-parse profile while the row claims an independent feature admit. |
| `value_plane` | Semantic output plane measured by Track 1: JSON parse-only, JSON direct strict product, JSON typed direct, CSS typed value, CSS document/value view, CSSOM, fact-stream, full-parse summary, or diagnostic aggregate. | Empty rejects. CSS `fact-stream`, `full_parse_summary`, or brace-counter output cannot close CSS Value API or CSS SOTA. `value_plane` must agree with `Output plane` and `substrate_target`. |
| `css_comparator_workload` | CSS comparator workload plane, for example `cssparser:stylesheet_full_parse`, `cssparser:typed_value`, `lightningcss:cssom`, or `n/a:not-css`. | Required on all rows. CSS rows reject if lightningcss is used as an admission anchor before Track 1 emits comparable CSSOM/value output. Workload mismatch against `value_plane` rejects. |
| `generator_source` | Provenance for generated runtime/provider code: grammar-derived source path and generator id, or explicit hand-written status. | Empty rejects. Live CSS admission rejects `hand_written:*`, `CSS_GENERATED_RS`, string-literal tokenizer provenance, or Pattern H files lacking line-1 generator provenance. |
| `lock14_scan_scope` | Exact scan roots used for Lock 14, including generic crates, generated roots, providers, benches, xtask/report/gate files, and any intentionally excluded paths. | Empty rejects. `incomplete:*` cannot close. A scope that omits previously excluded leak files or generic gate/report files rejects unless the row is a non-admission diagnostic finding. |
| `lock16_status` | Primitive status for SIMD/ASM/parity: `not-applicable`, `scalar-only`, `simd-claimed`, `asm-claimed`, `source-present-unwired`, `deleted`, or `architectural-block-with-redress`. | Empty rejects. SIMD/ASM claims on non-native or x86/AVX-512 admission rows reject. `source-present-unwired` cannot close unless paired with deletion, scalar-delegate status, or REDRESS. |
| `checkasm_or_parity_status` | Executable parity proof for the row: scalar oracle, checkasm command, corpus equality command, or explicit non-applicable reason. | Empty, `pending`, smoke-only, or commandless pass rejects. If `lock16_status` is `simd-claimed` or `asm-claimed`, strict checkasm/parity on Apple M5 Max / aarch64 is mandatory. |
| `gate_exclusion_report` | Machine-readable Lock 14 / Lock 16 exclusion report: empty-list proof, reported exclusions with owner/reason, or failure. | Missing rejects. `self-exempting:*` rejects. Any exclusion of the validator, scan roots, checkasm target, generated runtime roots, or files under test rejects unless reported as a finding and the row is non-admission. |
| `broadcast_group_id` | Broadcast/aggregate classification. Empty or `none` means independent measurement. Non-empty groups identify rows sharing one measurement. | A non-empty group cannot produce multiple independent `A / GO` conceptual wins. Rows in a group must be one aggregate row or diagnostic/non-admission rows. Hidden grouping detected from identical origin/metrics with `broadcast_group_id=none` rejects. |

### 2.3 Gate-Json Rejection Rules

`gate-json` or its SK-V15 successor must reject before computing an admit verdict when any of these predicates fail:

1. **Missing visible schema-v3 column.** Any row missing a visible column from `Corpus` through `Signal` rejects.
2. **Missing SK-V8 field.** Any row missing a SK-V8 required telemetry field rejects. Empty strings reject; explicit `n/a:<reason>` values are allowed only where the field definition permits them.
3. **Missing SK-V15 field.** Any row missing one of the ten SK-V15 fields rejects after W0. W0 may populate explicit non-applicable values for JSON guard rows, but not blanks.
4. **Producer-only telemetry.** Any emitted field not parsed and validated by the gate rejects. Unknown payload fields reject unless the schema version is deliberately bumped and the validator consumes them.
5. **Self-exempting exclusion.** `gate_exclusion_report` values that exclude the gate, report parser, lock validator, checkasm suite, generated roots, or the files under test reject. A grep/checkasm rule cannot close by exempting itself.
6. **Hidden one-to-N measurement.** The gate builds both a `measurement_row_id -> row_id list` map and a measurement signature map over `measurement_origin`, sample count, Track 1 Mbps, CSS comparator Mbps, run id, and profile artifact. Reuse across multiple admitting CSS rows rejects unless those rows are explicitly aggregate and non-admission.
7. **CSS plane mismatch.** CSS rows on `full_parse_summary`, `fact_stream`, or brace-counter output cannot satisfy typed Value API or CSS SOTA close. lightningcss cannot count as the strict anchor until the Track 1 value plane is comparable CSSOM/value output.
8. **Generator contrivance.** `generator_source` pointing to `CSS_GENERATED_RS`, hand-written CSS tokenizers, profile-control matches, or missing Pattern H provenance rejects live CSS admission.
9. **Lock 14 / Lock 16 incompleteness.** Incomplete scan scope, unreported exclusions, `simd-claimed` without strict checkasm/parity, or source-present unwired primitives reject close.
10. **Native platform mismatch.** Admission rows with `Host triple` / feature mask outside Apple M5 Max / aarch64 are diagnostic only and cannot close SK-V15.

### 2.4 Broadcast Binding

The existing CSS W8R residue is the negative fixture: 24 CSS rows currently carry `track1_mbps=2319.041`, `cssparser_mbps=2362.037`, and `lightningcss_mbps=929.281` from the same W8R full-parse run (`skinny/RESULTS.md:112-135`; `restart/skinny/tranches/sk-v15/research/alpha/alpha-A-results-extraction.md:28-34`). Under SK-V15, that shape has only two legal outcomes:

- collapse to one aggregate diagnostic row with a shared `broadcast_group_id`, non-admission verdict, and explicit `value_plane=diagnostic_aggregate` or `value_plane=full_parse_summary`; or
- retime each feature independently on typed CSS value/CSSOM output, with distinct `measurement_row_id`, distinct `measurement_origin`, compatible `css_comparator_workload`, and no shared hidden signature.

It is illegal to keep 24 `A / GO` CSS feature rows by assigning 24 fresh row ids to one timing tuple.
P3-A and P3-C must not cite these numbers as floors; W6 derives any CSS
typed-admission floor from fresh same-workload typed measurements.

## Section 3 - Falsifiability Binding

Schema validation is presence- and consistency-based. Throughput thresholds belong to P3-C, but P3-C must assume the following schema gates are hard prerequisites:

| Fixture | Expected result |
|---|---|
| JSON 51 rows with all SK-V8 fields plus SK-V15 fields set to explicit `n/a:not-css` where appropriate | PASS as guard baseline, subject to unchanged JSON strict/equality gates |
| CSS 24 rows sharing one `measurement_row_id` and claiming `A / GO` | FAIL: visible broadcast |
| CSS 24 rows with unique `measurement_row_id` but identical `measurement_origin`, sample count, Track 1 Mbps, cssparser Mbps, lightningcss Mbps, and profile artifact | FAIL: hidden one-to-N measurement stamp |
| CSS aggregate diagnostic row with one `broadcast_group_id`, one measurement, and non-admission verdict | PASS for prune visibility, not for CSS admission |
| CSS row with `value_plane=full_parse_summary` claiming CSS Value API close | FAIL: wrong value plane |
| Row with `gate_exclusion_report=self-exempting:*` | FAIL |
| Row with `lock16_status=asm-claimed` and `checkasm_or_parity_status=pending` | FAIL |
| Row with `generator_source=hand_written:CSS_GENERATED_RS` and `A / GO` CSS admission | FAIL |

Named row universe for W0 schema lock:

- JSON guard rows: 17 corpora x 3 workloads = 51 rows. P1-F confirms these remain in RESULTS as strict JSON rows and fresh P1 evidence is external profile/PMU evidence, not a RESULTS rewrite (`restart/skinny/tranches/sk-v15/research/p1/p1f-results-delta.md:18-27`).
- CSS prune rows: 24 CSS L4 full-parse rows remain in RESULTS, are audit-demoted, and are not admits in SK-V15 (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:55-68`; `restart/skinny/tranches/sk-v15/research/p1/p1f-results-delta.md:29-32`).

## Section 4 - Pre-Blocked Routes

The telemetry schema must prevent these routes from re-entering under renamed fields:

- CSS 24-row broadcast admission. Distinct conceptual rows require distinct measurements, or one aggregate diagnostic row.
- CSS full-parse summary, fact-stream-only `parse()` output, or brace-counter output as a typed CSS Value API close.
- Hand-written CSS tokenizer or `CSS_GENERATED_RS` provenance as generated grammar evidence.
- Silent Lock 14 / Lock 16 scan-root exclusions, including self-exempting grep/checkasm rules.
- PMULL hot-body, CSSC bulk-consumer, x86/AVX-512, retained structural/cursor/class streams, schema-shaped builders, harness hashes, or CSS broadcast evidence as S-P3 survivor proof (`restart/skinny/tranches/sk-v15/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md:33-49`).
- Any new telemetry field emitted without same-wave gate consumption.
- Documentation-only close evidence. Every close row must carry executable command/artifact provenance or route to REDRESS.

## Section 5 - Sources

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:61` - P3-D scope.
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:239-244` - telemetry binding is load-bearing; producer-only telemetry fails.
- `restart/skinny/tranches/sk-v15/SYNTHESIS.md:36-50` - close gates: CSS honesty, value API, Lock 14 / Lock 16, native platform, executable evidence.
- `restart/skinny/tranches/sk-v15/SYNTHESIS.md:55-80` - JSON guard state, CSS audit-demoted state, prune/rebuild receivers.
- `restart/skinny/tranches/sk-v15/SYNTHESIS.md:121-127` - ten SK-V15 telemetry fields and rejection classes.
- `restart/skinny/tranches/sk-v15/research/alpha/alpha-A-results-extraction.md:28-61` - CSS repeated-measurement finding and field meanings.
- `restart/skinny/tranches/sk-v15/research/p1/p1f-results-delta.md:18-32` - 51 JSON rows plus 24 CSS rows and CSS broadcast anomaly.
- `restart/skinny/tranches/sk-v15/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md:33-49` - S-P2 survivor and rejection boundaries.
- `restart/skinny/tranches/sk-v8/SPEC.md:103-146` - SK-V8 required telemetry and gate rejection posture.
- `skinny/RESULTS.md:3` - visible schema-v3 header.
- `skinny/RESULTS.md:57-59` - current SK-V14 manifest carrier.
- `skinny/RESULTS.md:112-135` - CSS W8R repeated full-parse measurement rows.
- `skinny/crates/bbnf-bench/src/report.rs:8-10` - live schema-v3 and SK-V14 manifest header constants.
- `skinny/crates/bbnf-bench/src/report.rs:151-198` - current `SkV8Telemetry` binding.
- `skinny/xtask/src/main.rs:486-584` - current SK-V14 manifest parser and row count/duplicate checks.
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` - per-wave research/plan/redress, same-wave consumer, and no-orphan discipline.
