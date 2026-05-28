# SK-V15 S-P3 V2 Gate-Table Fold Notes

Pass: S-P3 V2 fold support.
Scope: measurable per-wave gates only.
Owned output: this file.
Inputs read: P3-A, P3-C, P3-D, SPEC, DISPATCH, CH1, CH5, CH7.
Disposition: fold required before S-P3 can move from V1 REVISE to V2.

## 1. Fold Target

The V1 packet has the correct intent but still lets measurable gates degrade
into prose. V2 should make P3-C the single source for row universes,
threshold formulas, canonical telemetry fields, CSS diagnostic-only treatment,
same-wave consumers, and reject routes. SPEC and DISPATCH should then carry
the same columns in shorter executable form.

The final topology is:

`W0 -> W1 -> W2 -> W3 -> W4 -> W5 -> W6 -> W7 -> W8 -> W9`

The V2 fold must remove stale P3-C language that binds only the old combined
W6/W7 receiver set. W6 is Decision Engine spine, W7 is BackendShape lowerers,
W8 is FNV quarantine, and W9 is close reconciliation.

## 2. SK-V15-open Row Universe

P3-C, SPEC, and DISPATCH should use this exact row universe wording.

| Universe | Rows | Gate use |
|---|---:|---|
| JSON guard | 51 | All 17 corpora crossed with `parse_only`, `direct_to_struct`, and `real_typed_struct`: `twitter`, `citm_catalog`, `canada`, `apache_builds`, `github_events`, `update_center`, `mesh`, `random`, `gsoc-2018`, `marine_ik`, `instruments`, `numbers`, `unicode_mixed`, `unicode_escapes`, `unicode_basic`, `distinct_values`, `y_string_unicode`. W0 captures Track 1 and Track 2 Mbps as `SK-V15-open`. |
| CSS prune | 24 | P3-C Appendix A CSS L4 rows. They are diagnostic inputs until W5 rebuilds typed CSS output. They cannot close as live admits while they share W8R broadcast identity. |
| CSS aggregate diagnostic | 0 or 1 | Legal replacement for the 24 broadcast rows before typed rebuild. It is non-admission and carries the W8R tuple only as a negative fixture. |
| CSS typed admit | 0..24 plus optional aggregate | Legal only after W5 typed value/document output exists and each admitting row has fresh same-run comparator data, unique `measurement_row_id`, and matching value plane. |
| FNV strict-product guard | 6 | `json/gsoc-2018/{direct_to_struct,real_typed_struct}/main`, `json/unicode_mixed/{direct_to_struct,real_typed_struct}/main`, and `json/y_string_unicode/{direct_to_struct,real_typed_struct}/main`. W8 must prove FNV cannot arbitrate these rows. |

## 3. Budget Definitions

These definitions should be copied into P3-C and referenced, not reworded, in
SPEC and DISPATCH.

| Budget | Applies to | Binding formula |
|---|---|---|
| `M0` | W0 and ledger/gate-only waves | No production parser/codegen/runtime behavior diff. `skinny/RESULTS.md` has no Mbps diff unless the wave explicitly reruns W0 capture. If rerun, every non-target JSON Track 1 and Track 2 cell is within +/-1.0% of `SK-V15-open`, no JSON verdict downgrades, and CSS rows remain diagnostic unless independently retimed. |
| `M1` | Behavior waves | Every selected target row meets its target formula. Every non-target JSON Track 1 and Track 2 cell is `>= SK-V15-open * 0.98`. All 51 JSON rows remain `A / GO / strict / measured-row` on native Apple M5 Max / aarch64. Existing or rebuilt CSS live rows keep their declared comparator plane. |
| `M-css-prune` | W1 only | The 24 current CSS rows may lose live-admit status because that is the targeted false admit. JSON 51/51 obeys `M0`. No replacement 24-row CSS admit is legal unless each admitting row has a distinct measurement identity and same-workload comparator proof. |

## 4. Canonical Telemetry Fields

P3-D and SPEC already name the canonical fields. V2 should make aliases
illegal unless a schema version bump maps them and the gate consumes that map.
P3-B-style names such as `sample_count`, `row_claim_scope`,
`comparator_workload_id`, `producer_path`, `generator_source_id`,
`semantic_output_kind`, and `strictness_source` are not substitutes.

| Field | Fold rule |
|---|---|
| `measurement_row_id` | Required for every row after W0. Duplicates across multiple CSS `A / GO` rows reject unless the rows are one explicit aggregate diagnostic and non-admission. |
| `measurement_origin` | Must identify command, artifact, corpus slice, profile/run id, and measurement source. Hidden shared origins reject CSS feature admits. |
| `value_plane` | Must agree with visible output plane. CSS `fact_stream`, `full_parse_summary`, and brace-counter outputs cannot close W5 typed Value API. |
| `css_comparator_workload` | Required on all rows, including explicit `n/a:not-css`. CSS admits require same-workload typed value/document comparison, normally `cssparser` after W5. |
| `generator_source` | Live CSS/generated admission rejects hand-written tokenizer, `CSS_GENERATED_RS`, and missing Pattern H provenance. |
| `lock14_scan_scope` | Must include roots and exclusions. `incomplete:*` cannot close. |
| `lock16_status` | Must classify primitives as `not-applicable`, `scalar-only`, `simd-claimed`, `asm-claimed`, `source-present-unwired`, `deleted`, or `architectural-block-with-redress`. |
| `checkasm_or_parity_status` | SIMD/ASM requires strict Apple M5 Max / aarch64 parity before product routing. Smoke-only and pending reject. |
| `gate_exclusion_report` | Must list included roots, excluded roots, exclusion reasons, and the scan of the exclusion list itself. Self-exempting reports reject. |
| `broadcast_group_id` | Required when measurements are shared. Non-empty groups cannot produce multiple live admits. Hidden grouping detected from identical metric signatures rejects. |

## 5. CSS W8R Diagnostic-Only Treatment

V2 must remove W8R metrics from live typed CSS floors. The tuple
`track1_mbps=2319.041`, `cssparser_mbps=2362.037`, and
`lightningcss_mbps=929.281` is a negative fixture showing one broadcast
measurement repeated over 24 conceptual rows.

| Surface | Required V2 treatment |
|---|---|
| P3-A candidate 8 | Replace any live floor based on `2319.041` or `2362.037` with `fresh_css_typed_track1_mbps >= same_run_cssparser_typed_mbps`. The W8R tuple may appear only in a diagnostic-negative note. |
| P3-C W5 | Remove `2362.037 Mbps` as an explicit typed-admission floor. W5 first captures same-run `cssparser` typed value/document comparator data after Track 1 emits typed CSS output. The admission formula is `track1_typed_mbps >= cssparser_typed_mbps` for the same corpus, plane, host, and run bracket. |
| SPEC W5 | State that W8R values are not thresholds. They are diagnostic-only until replaced by fresh typed-output measurements. |
| DISPATCH W5 | Plan must name the fresh typed comparator command and prove no CSS row reuses W8R `measurement_origin`, `measurement_row_id`, or hidden metric signature. |

## 6. Per-Candidate Threshold Rebinding

P3-C needs a candidate gate table for all eight P3-A candidates. Use these
columns exactly.

| Column | Required content |
|---|---|
| `candidate_id` | P3-A candidate number 1..8. |
| `candidate_surface` | Primitive/API/template being admitted. |
| `owning_wave` | W5, W6, W7, or later only if same-wave consumer exists; otherwise `queued-after-W9` or `reject`. |
| `target_rows` | Exact row ids eligible to prove movement. |
| `listed_floor_source` | P3-A listed floor values, if any. |
| `open_bound_formula` | `final_floor(row) = max(P3A_listed_floor(row), SK-V15-open.track1_mbps(row) * target_factor)` for JSON performance candidates. Default `target_factor = 1.03` unless P3-A listed floor is stricter. For CSS typed rows use `fresh_track1_typed_mbps >= same_run_cssparser_typed_mbps`, not W8R numbers. |
| `guard_rows` | JSON 51 minus target rows, or explicit strict-product/FNV guard rows. |
| `guard_formula` | `track1 >= SK-V15-open.track1 * 0.98` and `track2 >= SK-V15-open.track2 * 0.98` for behavior waves; `M0` for gate-only waves. |
| `same_wave_consumer` | The hot path, generated runtime, typed CSS provider, lowerer, or gate that consumes the candidate in the same wave. |
| `scalar_oracle` | Required scalar reference, executable oracle, or explicit non-applicable proof. |
| `parity_or_checkasm` | Required checkasm/parity command for SIMD/ASM/mask candidates, or explicit non-applicable reason. |
| `reject_or_demote_action` | REDRESS row, scalar-only demotion, deletion, or intrinsic-block path if target rows do not move or guards fail. |

Minimum candidate formulas:

| Candidate | Formula |
|---:|---|
| 1 | Target `apache_builds/parse_only/main`: `max(13523.211, open * 1.03)`. Guards named in P3-A use `>= open * 0.98`. |
| 2 | Any admitted target among P3-A structural/direct rows: `max(listed_floor, open * 1.03)`; at least two target rows must move or demote. |
| 3 | String/literal target rows: `max(listed_floor, open * 1.03)`; guard full JSON table at `M1`. |
| 4 | UTF-8 target rows: `max(listed_floor, open * 1.03)` unless P3-A listed Unicode floor is stricter; run-level scalar oracle mandatory. |
| 5 | Escape/segment targets: `max(listed_floor, open * 1.03)`; generic JSON-byte replay rejects without non-JSON consumer. |
| 6 | Direct cursor/FIRST-set targets: at least two listed direct rows meet `max(listed_floor, open * 1.03)`; no retained cursor state. |
| 7 | Same-tape policy: performance uses `M1`; materialization ratio must be `<= SK-V15-open` diagnostic ratio for the named row, with equal traversal semantics. |
| 8 | JSON structural use uses `max(listed_floor, open * 1.03)` on target parse rows. CSS typed use has no W8R floor; it must beat same-run `cssparser` typed comparator after W5 output exists. |

## 7. P3-C Tables To Add Or Rewrite

P3-C should contain four gate tables.

### 7.1 Wave Gate Table

| Column | Required content |
|---|---|
| `wave` | W0..W9. |
| `receiver` | Final receiver name from SPEC. |
| `gate_class` | `M0`, `M1`, `M-css-prune`, or `close-reconciliation`. |
| `entry_row_universe` | JSON 51, CSS 24 diagnostic, CSS aggregate, CSS typed rows, FNV guard rows, or generated/provenance scan universe. |
| `target_rows_or_scan` | Exact rows or scan roots that prove the wave. |
| `guard_rows` | Rows that must not regress. |
| `threshold_formula` | Exact `SK-V15-open` formula or no-behavior/no-Mbps-diff proof. |
| `css_treatment` | `n/a:not-css`, `diagnostic-only`, `typed-fresh`, or `aggregate-non-admit`. |
| `required_telemetry_fields` | The ten canonical fields, or explicit subset plus reason for non-row scans. |
| `same_wave_consumer` | Gate, report, hot path, lowerer test, or generated runtime that consumes the output. |
| `proof_command_or_artifact` | Command/artifact family required in the wave plan. |
| `reject_redress_action` | Revert, REDRESS, demotion, scalar-delegate, delete, or intrinsic-block action. |

### 7.2 Candidate Rebind Table

Use the columns in Section 6.

### 7.3 Gate-Exclusion Report Table

| Column | Required content |
|---|---|
| `wave` | W2, W3, W4, W6, W7, W8, plus any wave using gate cleanliness. |
| `gate_name` | Lock 14, Lock 16, Pattern H provenance, Decision Engine, BackendShape lowerer, FNV, or CSS anti-broadcast. |
| `included_roots` | Exact roots scanned. |
| `excluded_roots` | Exact exclusions; `none` must be explicit. |
| `exclusion_reason` | Why each exclusion exists. |
| `self_scan` | Proof that the gate scans its own exclusion list/report path. |
| `gate_consumer` | Executable gate consuming the report in the same wave. |
| `fail_predicate` | Missing roots, silent exclusions, self-exemption, stale report, or producer-only report. |

### 7.4 EventTape / Sidecar Guard Table

| Column | Required content |
|---|---|
| `surface` | W7 EventTape lowerer and any generated output it touches. |
| `allowed_output` | Existing `BackendShape::EventTape` lowering into accepted runtime substrate or gate-consumed rejected alternative. |
| `forbidden_output` | Sidecar event vector, retained parser-owned stream, sixth BackendShape, public substrate API, alternate document projection, second tape, aux density/projection table, retained cursor/list, parser-owned structural projection. |
| `test_or_gate` | Test that fails on old label-string scaffold and fails on any sidecar/event-vector materialization. |
| `redress_action` | Revert lowerer diff and record REDRESS if the allowed output is not executable. |

## 8. SPEC Columns To Add

SPEC should keep prose short and add one measured gate table under Section 2
or under each wave envelope. Use these exact columns.

| Column | Required content |
|---|---|
| `wave` | W0..W9. |
| `row_universe_or_scan` | JSON 51, CSS 24 diagnostic, CSS typed row set, FNV guard rows, Pattern H 67 files, Lock scan roots, or Decision Engine/lowerer fixtures. |
| `gate_class` | `M0`, `M1`, `M-css-prune`, `close-reconciliation`. |
| `target_threshold` | Exact row formula, same-run CSS comparator formula, no-behavior proof, or scan pass criterion. |
| `guard_threshold` | JSON 51 `M0` or `M1`; CSS diagnostic status; strict-product/FNV rows where relevant. |
| `telemetry_and_exclusion_fields` | Ten canonical fields plus gate-exclusion report requirements where relevant. |
| `same_wave_consumer` | Gate/hot path/lowerer/test consuming the artifact. |
| `forbidden_close_evidence` | Broadcast W8R values, fact-stream/summary CSS proof, producer-only telemetry, self-exempting scan, source-present unwired primitive, sidecar EventTape, FNV runtime arbiter, docs-only proof. |
| `proof_command_family` | Command family the wave plan must instantiate. |
| `revert_redress_route` | Revert slice and REDRESS/intrinsic-block action. |

SPEC Section 1 and Section 13 should also carry the full CH5 forbidden set:
parser-owned structural projection, retained cursor/list, aux density or
projection table, sidecar event vector, parallel source pass, second tape,
public `UnionTape`, retained sidecar table, class column, whitespace bitmap,
and new substrate/API. W7 must explicitly say EventTape is only an existing
BackendShape lowerer and cannot materialize any of those forbidden outputs.

## 9. DISPATCH Columns To Add

DISPATCH should force each wave plan/redress prompt to instantiate P3-C and
SPEC, not summarize them.

| Column | Required content |
|---|---|
| `wave` | W0..W9. |
| `research_must_read` | P3-C wave row, P3-D fields, SPEC measured gate row, relevant hardening findings, RESULTS/REDRESS roots. |
| `plan_must_name` | Owner paths, row universe, target rows/scans, budget class, formulas, same-wave consumer, proof commands, revert slice. |
| `redress_must_prove` | Command output/artifacts satisfying P3-C formulas and SPEC forbidden-evidence checks. |
| `telemetry_fields` | The ten canonical SK-V15 fields; aliases reject unless schema bump plus consumed mapping exists. |
| `gate_exclusion_report` | Included roots, excluded roots, reasons, self-scan, gate consumer. |
| `css_w8r_rule` | Diagnostic-only; never a typed-admission floor. |
| `reject_triggers` | Missing field, producer-only field, hidden broadcast, stale W6/W7/W8 mapping, sidecar EventTape, self-exempting scan, row guard miss, no same-wave consumer. |
| `split_trigger` | Any plan that cannot satisfy redress within 30 minutes must split before redress. |

DISPATCH W5 must require fresh same-run `cssparser` typed comparator capture.
DISPATCH W7 must require EventTape anti-sidecar proof. DISPATCH W8 must require
FNV adversarial semantic fixtures and production-root scan proof.

## 10. Immediate V2 Edit Order

1. Rewrite P3-C first so it owns W0-W9 rows, budgets, candidate formulas,
   CSS diagnostic-only language, gate-exclusion table, and EventTape guard.
2. Patch P3-A candidate 8 to remove W8R metrics as live typed floors.
3. Patch SPEC Section 2 and each W0-W9 envelope to carry measured gate rows.
4. Patch DISPATCH Section 4 with plan/redress columns and canonical telemetry
   vocabulary.
5. Patch any remaining P3-B/P3-F references that use alias telemetry fields
   or old W6/W7 topology.

CH1, CH5, and CH7 can accept only when these tables make the blocked routes
unexecutable, not merely discouraged.
