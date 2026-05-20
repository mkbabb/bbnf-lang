# SK-V11 W1b Phase 2 Plan: Gate/Challenge Matrix

Date: 2026-05-20.
Owned artifact: `restart/skinny/tranches/sk-v11/research/w1b/w1b-plan-gate-matrix.md`.
Source edit policy: plan only. Do not edit source, generated output,
`skinny/RESULTS.md`, or work by others.

## Authorities Read

- `restart/skinny/tranches/sk-v11/SPEC.md` Section 5.
- `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md` W1b gate and CHALLENGE
  clauses.
- `restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md`.
- `restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-plan-gate-matrix.md`.
- W1a gate implementation surfaces:
  `skinny/crates/bbnf-bench/src/report.rs`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs`, and W1a fixtures under
  `restart/skinny/tranches/sk-v11/research/w1a/fixtures/`.
- W1b research artifacts R1-R6 under
  `restart/skinny/tranches/sk-v11/research/w1b/`.

## Phase 2 Decision

W1b is a generated non-JSON baseline and oracle lane. It may create exactly one
generated non-JSON direct parser baseline report, measure generated Track 1 and
independent Track 2/oracle, prove strict same-output equality, and feed that
report into a W1b gate. It must not admit an intervention, move a JSON row, or
close the SK-V11 non-JSON intervention axis.

CHALLENGE selection is:

```text
grammar_id: css_l4
domain: css_l4_bench
corpus: declaration_values
workload: direct
row_id: css_l4/declaration_values/direct/main
output_plane: css_l4_declaration_value_fact_bytes
oracle: css_l4_decl_value_fact_oracle
```

This follows R1/R2/R4/R6's narrowest admissible baseline recommendation. R4's
digest wording and R5's typed-oracle recommendation are superseded by this Phase
2 selection. R5's typed CSS oracle is stronger semantically, but current skinny
W1b cannot produce an admissible generated typed CSS Track 1 or add the
dependency/manifest owners needed for that route. The direct route must expose
full stable fact bytes for strict equality; digest equality alone is not enough.

Do not render the W1b non-JSON row into `skinny/RESULTS.md`. The safe W1b shape
is one generated companion report consumed by `bbnf-bench --bin gate`.

## Exact Exit Gate

`G-W1b-NONJSON-BASELINE` passes only if all of these conditions are true:

1. W1a is preserved: W1a pass fixture still passes and W1a negative fixtures
   still fail.
2. Exactly one W1b non-JSON baseline report row exists.
3. The row identity is the CHALLENGE-selected target, preferably
   `css_l4/declaration_values/direct/main`.
4. Generated Track 1 exists for the selected non-JSON parser row and is produced
   from named selected grammar/schema inputs, not hand-patched output.
5. Independent Track 2/oracle exists on the same output plane and does not call
   generated Track 1, generated SinkOnly helpers, generated typed helpers,
   generated JSON runtime, JSON providers, root hand CSS runtime, runtime
   witness paths, benchmark-private parser code, stale sidecars, or prose-only
   sources.
6. Strict byte equality passes between generated Track 1 and Track 2/oracle fact
   streams on the selected corpus.
7. Baseline throughput is rendered with run id, host, build flags, feature mask,
   sample count, sample cost, output plane, Track 1 Mbps, Track 2/oracle Mbps,
   Track 1 source kind, Track 1 source artifact, generated input/output
   artifacts, strict equality artifact, profile artifact, oracle status, and
   oracle source artifact.
8. The W1b gate consumes every emitted field. Unknown or producer-only fields
   fail closed.
9. No JSON policy appears in generic crates or runtime outside generated
   per-grammar modules.
10. No behavior row admits, no parse-only SOTA claim appears, no SK-V11 close is
    claimed, and no JSON `RESULTS.md` row moves.

## Exact W1b Report Fields

The W1b companion report remains a JSON file format even though the benchmarked
grammar is non-JSON. The report path should be:

```text
restart/skinny/tranches/sk-v11/research/w1b/reports/nonjson-baseline-css-l4-direct.json
```

Top-level fields:

| Field | Required W1b value |
|---|---|
| `schema_version` | `sk-v11-w1b-nonjson-baseline-v1` |
| `wave_id` | `SK-V11-W1b` |
| `run_id` | `sk-v11-w1b:criterion-fnv64-<16 lowercase hex>` |
| `rows` | exactly one row |

Required row fields:

| Field | Required W1b value |
|---|---|
| `corpus` | `declaration_values` for CSS selection |
| `workload` | `direct` |
| `outcome_id` | `S` |
| `verdict` | `NO-GO` |
| `strictness` | `strict` |
| `parse_utf8` | `measured-row` |
| `escape_complete` | `yes` |
| `flaw_probe` | `none` |
| `output_plane` | `css_l4_declaration_value_fact_bytes` |
| `track1_mbps` | finite positive generated Track 1 Mbps |
| `track2_mbps` | finite positive independent oracle/Track 2 Mbps |
| `track1_source_kind` | `generated_non_json_direct` |
| `track1_source_artifact` | generated Track 1 source path under a W1b owner path, never JSON or root hand runtime |
| `generated_input_artifact` | selected CSS L4 grammar/schema input artifact used to generate Track 1 |
| `generated_output_artifact` | selected generated CSS L4 direct output artifact used as Track 1 |
| `strict_equality_status` | `fact_bytes_equal` |
| `strict_equality_artifact` | same-run fact-byte comparison artifact or equality proof path |
| `fact_bytes_mismatch_artifact` | `n/a` on pass; concrete mismatch artifact path on equality failure |
| `competitors` | all JSON competitor fields `null` unless CHALLENGE names a same-plane non-JSON comparator |
| `delta_vs_skv6` | `n/a` |
| `delta_vs_sonic_strict` | `null` |
| `delta_vs_simdjson_dom` | `null` |
| `delta_vs_yyjson` | `null` |
| `hot_leaf` | W1b profile or generated baseline identifier, not `fixture:w1a:*` |
| `signal` | baseline-only text that states no admission and W2 seed status |
| `sk_v8` | required telemetry object below |

Required `sk_v8` telemetry fields:

| Field | Required W1b value |
|---|---|
| `row_id` | `css_l4/declaration_values/direct/main` |
| `grammar_id` | `css_l4`; fallback ids are only `sheets` or `bbnf_self` |
| `domain` | `css_l4_bench`; fallback domains are `sheets_bench` or `bbnf_self_bench` |
| `measured_validation_path` | `measured-row` |
| `profile_artifact` | Criterion/source artifact for the W1b generated baseline, not a W1a fixture sentinel |
| `sample_cost` | includes `ns_per_byte=<finite>` and selected byte/sample context |
| `sample_count` | positive same-run sample count |
| `build_flags` | nonempty bench build flags, including target CPU context |
| `host_triple` | nonempty same-run host triple |
| `feature_mask` | nonempty feature/fallback mask |
| `costfacts_rule_id` | `none:w1b-nonjson-baseline` unless a consumed W1b rule id exists |
| `costfacts_chosen_shape` | `none:w1b-nonjson-baseline` unless consumed |
| `costfacts_rejected_alternative_ids` | nonempty list, e.g. `["none:w1b-nonjson-baseline"]` |
| `redress_entry` | `none:w1b-baseline` for pass; REDRESS id only on failed/rejected route |
| `wave_id` | `SK-V11-W1b` |
| `run_id` | same as report `run_id` |
| `sk_v9_open_delta` | `nonjson-baseline-only` |
| `substrate_surface` | same as `output_plane` |
| `structural_projection_status` | `n/a` unless CHALLENGE names a consumed non-JSON projection proof |
| `substrate_cardinality` | `zero_or_inert` unless the selected generated output has a consumed cardinality proof |
| `same_wave_consumer_class` | `generated_non_json_direct_baseline` |
| `track2_independence_status` | `independent_verified` |
| `diagnostic_nonproducer_status` | `pmu+cycles+profiles:nonproducer` |
| `comparators` | exactly one independent oracle evidence object |

Required comparator/oracle fields:

| Field | Required W1b value |
|---|---|
| `comparator_id` | `css_l4_decl_value_fact_oracle` |
| `comparator_plane` | same as row `output_plane` |
| `comparator_strictness` | `strict` |
| `comparator_freshness` | `same-run-oracle` |
| `sidecar_freshness` | `n/a` |
| `value_mbps` | finite positive same-run oracle/Track 2 Mbps |
| `source_artifact` | concrete W1b oracle source/profile path in a reviewable W1b source module; never `oracle:w1a:*` or a Criterion-harness-only parser |

## Required Fixture Matrix

The future implementation may use one real generated pass report plus unit-test
builders for negative cases. Fixture names below are normative failure classes;
exact Rust test names may vary only if the failing class remains visible.

| Fixture or test | Input shape | Expected | Gate obligation |
|---|---|---:|---|
| `w1b_non_json_baseline_accepts_exact_generated_css_l4_report` | One generated CSS L4 row with W1b schema, same-run Track 1 and oracle, strict equality, and consumed provenance. | PASS | Establishes W1b baseline authority only. |
| `w1a_pass_fixture_still_passes` | Existing `nonjson-pass-css-l4.json` via `--w1a-non-json-report`. | PASS | W1b does not loosen or replace W1a. |
| `json_gate_costfacts_check_results_still_passes` | Current JSON `RESULTS.md` and CostFacts path. | PASS | JSON preservation remains green. |
| `w1b_rejects_zero_rows` | W1b schema with empty `rows`. | FAIL | A produced report without baseline row is not evidence. |
| `w1b_rejects_multiple_rows` | Two CSS rows or CSS plus fallback row. | FAIL | W1b creates exactly one target. |
| `w1b_rejects_wrong_schema_or_wave` | W1a schema, W2 schema, or `wave_id != SK-V11-W1b`. | FAIL | W1a sentinels and W2 interventions cannot become W1b authority. |
| `w1b_rejects_invalid_run_id` | Missing prefix or non-16-hex suffix. | FAIL | Same-run Criterion identity must be machine checked. |
| `w1b_rejects_mixed_run_id` | Report and row/oracle run contexts differ. | FAIL | Track 1 and oracle cannot be spliced from different runs. |
| `w1b_rejects_unknown_grammar` | `google_sheets`, `json`, or unregistered grammar id. | FAIL | Canonical non-JSON ids are exact. |
| `w1b_rejects_domain_mismatch` | `grammar_id=css_l4`, `domain=json_bench` or `css_l4`. | FAIL | Domain must be `<grammar_id>_bench`. |
| `w1b_rejects_row_id_mismatch` | Row id grammar/corpus/workload does not match row fields. | FAIL | Row identity is not display text. |
| `w1b_rejects_unselected_workload` | Typed row or any non-selected direct row when direct was selected. | FAIL | Exactly one target is allowed. |
| `w1b_rejects_missing_track1` | `track1_mbps = null`, zero, negative, NaN, or absent. | FAIL | Generated Track 1 throughput is required. |
| `w1b_rejects_missing_track1_source_artifact` | `track1_source_kind`, `track1_source_artifact`, generated input, or generated output artifact absent. | FAIL | Track 1 source authority must be machine checked. |
| `w1b_rejects_missing_track2_oracle` | `track2_mbps` or comparator `value_mbps` absent/non-finite. | FAIL | Baseline needs independent oracle measurement. |
| `w1b_rejects_missing_strict_equality` | Equality status absent, equality artifact absent, mismatch artifact says unequal, or strictness not `strict`. | FAIL | W1b proves strict equality, not parse reach. |
| `w1b_rejects_plane_mismatch` | Row plane `css_l4_declaration_value_fact_bytes`, comparator plane `digest` or `DOM`. | FAIL | Track 1 and oracle must compare the same output plane. |
| `w1b_rejects_direct_digest_as_typed` | Digest row claims typed baseline or typed oracle proof. | FAIL | Digest evidence cannot masquerade as typed product facts. |
| `w1b_rejects_parse_or_count_plane` | Acceptance, rule count, offsets, pretty CSS, canonical CSS string, or digest only. | FAIL | Output plane must be direct fact bytes. |
| `w1b_rejects_w1a_fixture_sentinels` | `profile_artifact=fixture:w1a:*`, `source_artifact=oracle:w1a:*`, or `measured_validation_path=schema-only`. | FAIL | W1a schema fixtures are not W1b baseline authority. |
| `w1b_rejects_gate_only_or_schema_only_consumer` | `same_wave_consumer_class=gate_only` or `non_json_gate_schema_only`. | FAIL | W1b needs generated non-JSON baseline consumer class. |
| `w1b_rejects_admission_claim` | `outcome_id=A`, `verdict=GO`, `baseline_authority` used as admission, or `signal` claims close. | FAIL | W1b is baseline-only. |
| `w1b_rejects_parse_only_sota_claim` | Workload `parse_only` or parse-plane SOTA wording. | FAIL | Parse-only evidence remains diagnostic. |
| `w1b_rejects_json_results_row_movement` | Any diff in `skinny/RESULTS.md`. | FAIL | No JSON row movement and no non-JSON row insertion in W1b. |
| `w1b_rejects_unknown_producer_fields` | Extra report key such as `pmu_cycles`, `profile_slope`, or `baseline_authority` not consumed by validator. | FAIL | Producer-only telemetry is forbidden. |
| `w1b_rejects_validator_only_requirement` | Validator requires a field not emitted by report. | FAIL | Gate and report schema move together. |
| `w1b_rejects_json_comparator_fields` | sonic/simdjson/yyjson/serde_json comparators used as non-JSON oracle without same-plane proof. | FAIL | JSON comparators do not prove CSS output. |
| `w1b_rejects_generated_track1_calling_oracle` | Oracle source imports or calls the selected generated Track 1 parser, serializer, fact projector, or output module. | FAIL | Oracle must be independent of Track 1. |
| `w1b_rejects_generated_helper_oracle` | Oracle source calls generated SinkOnly helpers, generated typed helpers, or selected generated direct/typed code. | FAIL | Generated helpers cannot be the oracle. |
| `w1b_rejects_generated_json_oracle` | Oracle or Track 1 proof uses generated JSON runtime or JSON bench Track 1. | FAIL | JSON reuse cannot prove non-JSON. |
| `w1b_rejects_json_provider_oracle` | Oracle or generated baseline transits `json_provider::*` as generality evidence. | FAIL | JSON provider is not a Lock 14 proof. |
| `w1b_rejects_root_css_runtime_oracle` | Track 1 or oracle authority uses root `crates/core` CSS runtime or `CssL4Parser`. | FAIL | Old hand runtime is not skinny generated Track 1. |
| `w1b_rejects_benchmark_private_oracle` | Oracle parser/fact projection is hidden in the Criterion harness body. | FAIL | Oracle source must be a reviewable W1b module. |
| `w1b_rejects_stale_sidecar_or_old_report` | Oracle value comes from checked-in expected bytes, stale sidecar, old report, or W1a sentinel. | FAIL | Oracle freshness must be same-run. |

## Oracle Independence Failure Matrix

The gate must read both `track2_independence_status` and the oracle source
artifact. A self-attested `independent_verified` string is not sufficient. Each
failure class in this table is a required implementation test or fixture class,
not advisory prose.

| Failure | Reject when source or evidence shows |
|---|---|
| `coupled_status` | `track2_independence_status != independent_verified`. |
| `generated_track1_call` | Oracle imports or calls the selected generated Track 1 parser, serializer, fact projector, or output module. |
| `generated_sink_or_typed_helper_call` | Oracle calls generated SinkOnly helpers, generated typed helpers, or selected generated direct/typed code. |
| `generated_json_reuse` | Oracle or Track 1 proof uses `skinny/crates/runtime/src/grammars/json/`, JSON generated helpers, or JSON bench Track 1. |
| `json_provider_reuse` | Oracle or generated baseline transits `json_provider::*` as a generality proof. |
| `root_hand_runtime_reuse` | W1b Track 1 authority is the old `crates/core/src/runtime/css_l4/` hand runtime or root `CssL4Parser` stack without approved generated skinny output. |
| `benchmark_private_parser` | Oracle hides a parser in benchmark code and does not expose a source path/fact schema for gate review. |
| `runtime_witness_path` | Oracle relies on witness/proof-only runtime modules rather than independent same-plane fact extraction. |
| `stale_sidecar` | Oracle value comes from historical sidecar, checked-in expected bytes, old report, or fixture sentinel. |
| `shared_projection_logic` | Track 1 and oracle share parser-shaped value projection logic; only a trivial stable byte encoder may be shared after independent facts are materialized. |
| `normalizer_erases_mismatch` | `token_normalize`, canonical CSS output, shorthand expansion, declaration reorder, calc folding, or recovery behavior can erase semantic differences. |
| `grammar_expected_values` | Oracle derives expected facts from the same BBNF/CSS grammar source instead of an independent parser/fact source. |
| `digest_only_hidden_mismatch` | Only digest equality is available; full fact bytes or mismatch dump cannot be inspected. |
| `prose_only_oracle` | Report names an oracle in text but no source artifact or gate-consumed comparator object exists. |

## JSON-Policy Leak Failure Matrix

W1b proves grammar generality by one generated non-JSON baseline plus oracle. It
does not prove generality by renaming JSON paths.

| Failure | Reject if |
|---|---|
| `json_provider_as_generality` | `json_provider::ensure_runtime_profile` or JSON provider templates emit the selected non-JSON baseline as the generality proof. |
| `json_template_with_css_labels` | JSON object/array/string/number policy remains in a renderer relabeled as CSS/Sheets/BBNF-self. |
| `generic_json_role_branch` | Generic crates branch on JSON roles, object/array layout, field names, JSON strings, JSON numbers, bool/null, or JSON structural alphabets. |
| `sink_direct_json_policy` | `sink_direct.rs` JSON `JsonSink`, JSON parse errors, or JSON value dispatch becomes the non-JSON parser. |
| `typed_direct_json_policy` | `typed_direct.rs` JSON string/number/bool/null/skip semantics are treated as a CSS typed parser without per-grammar replacement. |
| `generated_json_runtime_reuse` | `skinny/crates/runtime/src/grammars/json/` is reused for Track 1 or oracle proof. |
| `serde_or_sonic_as_css_oracle` | JSON strict comparators are presented as CSS/Sheets/BBNF-self oracle evidence. |
| `costfacts_as_performance` | CostFacts or profile metadata is used as throughput or correctness proof. |
| `old_hand_nonjson_runtime` | Legacy hand non-JSON runtime substitutes for generated skinny Track 1. |
| `hidden_substrate_policy` | W1b adds a directive, BIR variant, `BackendShape`, public substrate API, sidecar, structural-position vector, alternate tape, or W3 substrate route to pass the baseline. |

## Same-Wave Consumption Proof

Every W1b field must have one gate predicate and at least one mutation failure.

| Field family | Required consumer proof | Failure class |
|---|---|---|
| Strict schema keys | Deserializer rejects unknown keys at every report level. | unknown producer field |
| Report identity | Validator checks `schema_version`, `wave_id`, report `run_id`, row `run_id`, and one-row cardinality. | wrong schema, mixed run, multiple rows |
| Row identity | Validator checks `row_id`, `grammar_id`, `domain`, `corpus`, `workload`, and selected target. | grammar/domain/row mismatch |
| Non-admission outcome | Validator requires `S / NO-GO` and baseline-only signal. | admission and parse-only claims |
| Output plane | Validator checks row plane, comparator plane, and direct fact-byte boundary. | plane mismatch, direct-as-typed |
| Track 1 | Validator reads positive `track1_mbps`, `track1_source_kind`, `track1_source_artifact`, and generated-input/output provenance. | missing Track 1, hand-patched generated output |
| Track 2/oracle | Validator reads positive `track2_mbps`, comparator value, oracle id, strictness, freshness, source artifact, and independence proof. | missing oracle, stale/coupled/shared source |
| Strict equality | Validator consumes `strict_equality_status`, `strict_equality_artifact`, and mismatch artifact status on the same plane. | parse/count/pretty-only proof, mismatch |
| Run/provenance | Validator checks profile artifact, sample cost/count, build flags, host triple, and feature mask. | missing profile/sample/host/build/feature |
| JSON-policy boundary | Validator or tests prove selected path bypasses JSON provider/templates or leaves them untouched. | JSON-policy leak failures |
| Diagnostic status | PMU/cycles/profile slopes remain non-producers unless same-wave gate consumes them. | producer-only telemetry |

## CH1-CH6 Challenge Matrix

| Lens | W1b risk | Challenge test | Reject if |
|---|---|---|---|
| CH1 correctness and measurable row gates | A generated parser exists but equality is parse-only, count-only, digest-only, or not same-plane. | Require strict byte equality of direct fact bytes, with positive Track 1 and oracle Mbps. | No same-plane fact stream exists, strict equality is unmeasured, or throughput is missing. |
| CH2 generality and Lock 14 | JSON provider, JSON templates, or old hand non-JSON runtime are treated as grammar-neutral generation. | Source-path audit must show selected non-JSON generated Track 1 and no generic JSON policy outside generated per-grammar modules. | Track 1 is JSON relabeling, root hand CSS runtime, or prose-only Lock 14 evidence. |
| CH3 REDRESS regression/preblocks | W1b reopens coupled oracle, hidden substrate, W3, or row-admission routes. | Cross-check REDRESS 34, 35, 36, 37, 38, 48, 85, 86, 87, 92, 96, 97, 98, 100, 101, 102, 109, and 110. | Any stale sidecar, shared parser, generic JSON policy, hidden substrate, or baseline-as-admission route passes. |
| CH4 cost and micro-proof adequacy | W1b turns into a broad generated CSS typed runtime or W2 intervention. | Keep source/test/gate LOC <=360, one target, one oracle, selected generated outputs only, and <=90 min redress. | The plan needs multiple workloads, broad Tailwind/full stylesheet parity, C1-C7 intervention, or unbounded generated output. |
| CH5 hidden coupling and Lock 1 | Oracle shares Track 1 parser/projection logic or benchmark-private parser code. | Require consumed Track 1 source/provenance fields plus negative coupling fixtures for generated Track 1, generated helper, generated JSON, JSON provider, hand runtime, benchmark-private parser, stale sidecar, and W1a sentinel reuse. | Gate trusts `independent_verified` without source provenance or cannot distinguish shared parser evidence. |
| CH6 anti-paper-close and same-wave consumer | Report text claims W1b closes SK-V11 non-JSON intervention or lets W2 invent the first baseline. | Require `S / NO-GO`, generated baseline consumer class, W1b gate command, and explicit W2 seed wording. | Any `A / GO`, SK-V11 close, W2 intervention, parse-only SOTA, or unconsumed report passes. |

## Measurement And Verification Commands

Run from `/Users/mkbabb/Programming/bbnf-lang/skinny` unless noted.

Focused preservation commands:

```sh
cargo test -p bbnf-bench report::tests::w1a -- --nocapture
cargo test -p bbnf-bench --bin gate w1a -- --nocapture
cargo run -p bbnf-bench --bin gate -- --w1a-non-json-report ../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-pass-css-l4.json
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
git -C .. diff --exit-code -- skinny/RESULTS.md
```

Required W1b implementation commands once the gate/report path exists:

```sh
cargo test -p bbnf-bench report::tests::w1b -- --nocapture
cargo test -p bbnf-bench --bin gate w1b -- --nocapture
CRITERION_HOME=/tmp/skv11-w1b-nonjson-css-l4 RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench nonjson_baseline -- css_l4_declaration_values
CRITERION_HOME=/tmp/skv11-w1b-nonjson-css-l4 RUSTFLAGS="-C target-cpu=native" cargo run -p bbnf-bench --bin gate -- --write-w1b-non-json-baseline-report ../restart/skinny/tranches/sk-v11/research/w1b/reports/nonjson-baseline-css-l4-direct.json
cargo run -p bbnf-bench --bin gate -- --w1b-non-json-baseline-report ../restart/skinny/tranches/sk-v11/research/w1b/reports/nonjson-baseline-css-l4-direct.json
```

Required negative gate commands or equivalent unit tests:

```sh
if cargo run -p bbnf-bench --bin gate -- --w1b-non-json-baseline-report ../restart/skinny/tranches/sk-v11/research/w1b/fixtures/nonjson-w1b-multiple-rows.json; then exit 1; fi
if cargo run -p bbnf-bench --bin gate -- --w1b-non-json-baseline-report ../restart/skinny/tranches/sk-v11/research/w1b/fixtures/nonjson-w1b-w1a-sentinel.json; then exit 1; fi
if cargo run -p bbnf-bench --bin gate -- --w1b-non-json-baseline-report ../restart/skinny/tranches/sk-v11/research/w1b/fixtures/nonjson-w1b-track2-coupled.json; then exit 1; fi
if cargo run -p bbnf-bench --bin gate -- --w1b-non-json-baseline-report ../restart/skinny/tranches/sk-v11/research/w1b/fixtures/nonjson-w1b-json-policy-leak.json; then exit 1; fi
if cargo run -p bbnf-bench --bin gate -- --w1b-non-json-baseline-report ../restart/skinny/tranches/sk-v11/research/w1b/fixtures/nonjson-w1b-admission-claim.json; then exit 1; fi
```

The pass report command must pass. The multiple-row, W1a-sentinel, coupled
oracle, JSON-policy leak, and admission-claim commands must fail. Negative cases
may be implemented as Rust test builders instead of checked-in fixture files if
the failure class is explicit in test names.

## No-Admission And No-RESULTS Movement Proof

W1b must prove the baseline is seed evidence only:

- Report row uses `outcome_id = S` and `verdict = NO-GO`.
- `signal` says generated non-JSON baseline only; W2 or later owns admission.
- No direct, typed, parse-only, non-JSON, or JSON row admits.
- No `baseline_authority`, `close`, `A / GO`, SOTA, or intervention flag exists
  unless the W1b validator rejects it.
- No non-JSON row is inserted into `skinny/RESULTS.md`.
- No JSON `RESULTS.md` row moves, including whitespace/table churn.

File proof:

```sh
cd /Users/mkbabb/Programming/bbnf-lang
git diff --exit-code -- skinny/RESULTS.md
```

Semantic proof:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
cargo run -p bbnf-bench --bin gate -- --w1b-non-json-baseline-report ../restart/skinny/tranches/sk-v11/research/w1b/reports/nonjson-baseline-css-l4-direct.json
```

If either proof fails, W1b does not close. If `skinny/RESULTS.md` differs for
any reason during W1b, the gate fails unless CHALLENGE first revises W1b
ownership and updates every affected consumer.

## Revert And REDRESS Trigger

The future W1b revert unit is one slice: selected codegen/runtime generated
baseline, bench harness, oracle/Track 2, gate/report, generated report, and
selected generated output. Preserve the failed proof in `skinny/REDRESS.md`.

REDRESS is mandatory if generated Track 1 is absent or hand-patched, more than
one target is added, Track 2/oracle is coupled, strict equality fails, report
fields are producer-only, JSON policy leaks into generic code, a hidden substrate
route is added, any behavior row admits, any JSON row moves, or W2-style
intervention lands in W1b.

The REDRESS record must name the selected target, generated Track 1 path,
independent oracle/Track 2 path, output plane, run id, strict equality result,
baseline Mbps if measured, gate command result, and exact failure reason.

Self-verdict: ACCEPT as a Phase 2 plan artifact. This file changes no source,
generated output, or `skinny/RESULTS.md` row.
