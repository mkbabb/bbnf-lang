# SK-V11 W1a Phase 2 Plan: Gate/Challenge Matrix

Date: 2026-05-20.
Owned artifact: `restart/skinny/tranches/sk-v11/research/w1a/w1a-plan-gate-matrix.md`.
Source edit policy: no source edits, no generated output, no `skinny/RESULTS.md`
movement.

## Authorities Read

- `restart/skinny/tranches/sk-v11/SPEC.md` Section 4.
- `restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md`.
- `restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md`.
- `restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R1-gate-validator.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R2-report-metadata.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R3-fixtures-tests.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R4-nonjson-row-shape.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R5-telemetry-contract.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R6-redress-boundaries.md`.

## Phase 2 Decision

W1a is a gate/report schema lane only. It may make non-JSON evidence
gate-consumable, but it must not create a parser, generated baseline, behavior
intervention, JSON row movement, or non-JSON admission.

The lower-risk W1a shape is a strict companion non-JSON report or sibling gate
path. Existing JSON schema-v3 and `validate_sk_v8_w0` behavior must remain
intact. If implementation later chooses to render non-JSON rows in
`skinny/RESULTS.md`, it inherits the same matrix plus an explicit extension of
every `RESULTS.md` consumer. W1a should prefer the companion report unless
CHALLENGE approves the higher-risk rendered-row path.

## Exact Exit Gate

`G-W1a-NONJSON-GATE` passes only if all of these conditions are true:

1. Missing required non-JSON fields are rejected by the gate.
2. Producer-only non-JSON telemetry is rejected.
3. JSON `gate-json --with-cost-facts --check-results` remains green.
4. No JSON `RESULTS.md` row moves.
5. No generated non-JSON baseline authority is claimed.
6. The same-wave non-JSON report or fixture is consumed by a named gate command
   or validator path, not merely produced.
7. The validator proves all emitted non-JSON fields are read, and every validator
   requirement is emitted by the report or fixture.

Failure of any item is a W1a gate failure, not a REDRESS opportunity inside a
behavior wave.

## Required Fixture Matrix

All fixtures are schema fixtures. They must not require a real non-JSON
Criterion benchmark or generated runtime. Fixture names below are normative for
the implementation plan; exact Rust test function names may be adjusted only if
the same failure class remains obvious in test output.

| Fixture | Input shape | Expected result | Gate obligation |
|---|---|---:|---|
| `json_w0_report_still_passes` | Existing exact schema-v3/W0 JSON report with SK-V11 opening run id. | PASS | JSON validation remains unchanged before non-JSON checks run. |
| `gate_json_costfacts_check_results_still_passes` | Current `skinny/RESULTS.md` plus CostFacts path. | PASS | `gate-json --with-cost-facts --check-results` is still green. |
| `non_json_css_l4_companion_minimal_passes` | One companion `css_l4/declaration_values/direct/main` schema-only row. | PASS | Required non-JSON identifiers are emitted and consumed without admission. |
| `non_json_sheets_companion_minimal_passes` | One companion `sheets/formula/direct/main` schema-only row. | PASS | The gate uses canonical `sheets`, not `google_sheets`, unless SPEC is updated. |
| `non_json_bbnf_self_companion_minimal_passes` | One companion `bbnf_self/grammar/direct/main` schema-only row. | PASS | Fallback grammar values stay in the same allowlist discipline. |
| `non_json_rejects_missing_grammar_id` | Empty or absent `grammar_id`. | FAIL | Required grammar identity cannot be inferred from prose. |
| `non_json_rejects_unknown_grammar_id` | `grammar_id=google_sheets` or any unregistered grammar. | FAIL | Grammar allowlist is exact: `css_l4`, `sheets`, `bbnf_self`. |
| `non_json_rejects_row_id_grammar_mismatch` | `row_id=json/declaration_values/direct/main`, `grammar_id=css_l4`. | FAIL | Row id grammar prefix must match `grammar_id`. |
| `non_json_rejects_malformed_row_id` | Missing corpus/workload or suffix not `main`. | FAIL | Row identity must be `<grammar_id>/<corpus>/<workload>/main`. |
| `non_json_rejects_domain_mismatch` | `grammar_id=css_l4`, `domain=json_bench` or `css_l4`. | FAIL | Domain mapping must be exact: `<grammar_id>_bench`. |
| `non_json_rejects_unknown_workload` | Workload outside selected generated direct/typed tokens. | FAIL | W1a admits only SPEC-named future W1b workload shapes. |
| `non_json_rejects_missing_output_plane` | Empty or unknown `output_plane`. | FAIL | Output plane is required evidence, not display text. |
| `non_json_rejects_plane_mismatch` | Row plane `digest`, oracle plane `typed direct`, or equivalent mismatch. | FAIL | Strict/oracle evidence must be on the same output plane. |
| `non_json_rejects_direct_digest_as_typed` | Direct digest fixture claims typed product proof. | FAIL | Direct digest cannot maintain or admit typed product evidence. |
| `non_json_rejects_missing_oracle_id` | No comparator/oracle identity. | FAIL | Non-JSON evidence must name a comparator or independent oracle. |
| `non_json_rejects_oracle_without_source` | Oracle has empty `comparator_source_artifact`. | FAIL | Oracle provenance must be gate-consumed. |
| `non_json_rejects_stale_or_sidecar_oracle` | Historical, absent, or sidecar-only oracle freshness. | FAIL | W1a may not launder stale sidecars into same-run proof. |
| `non_json_rejects_missing_track1` | Missing or non-finite Track 1 Mbps. | FAIL | Future baseline shape must carry Track 1 throughput, fixture-only in W1a. |
| `non_json_rejects_missing_track2_or_oracle` | Missing Track 2/oracle Mbps or status. | FAIL | A Track 1-only report cannot pass the gate. |
| `non_json_rejects_track2_coupling` | `track2_independence_status=coupled_to_track1` or source calls Track 1. | FAIL | Track 2/oracle independence is mandatory. |
| `non_json_rejects_track2_shared_source` | `track2_independence_status=independent_verified` but source names generated Track 1, `generated_json`, SinkOnly/typed helper reuse, benchmark-private parser reuse, runtime witness tests, JSON providers, stale sidecars, old hand-runtime non-JSON proof, or prose-only oracle evidence. | FAIL | Independence status must be proven by source provenance, not self-attestation. |
| `non_json_rejects_missing_run_id` | Empty or malformed `run_id`. | FAIL | Every accepted report needs a validator-known run id. |
| `non_json_rejects_mixed_run_id` | Two rows in one report use different run ids. | FAIL | One report cannot splice runs. |
| `non_json_rejects_missing_profile_artifact` | Empty profile/source path. | FAIL | W1a requires structured provenance even for schema-only fixtures. |
| `non_json_rejects_missing_sample_cost_or_count` | Missing `sample_cost` or `sample_count=0`. | FAIL | Sample shape must remain reconstructable and consumed. |
| `non_json_rejects_missing_build_flags` | Empty build flags. | FAIL | Measurement context cannot be optional. |
| `non_json_rejects_missing_host` | Empty or malformed `host_triple`. | FAIL | Host context is part of same-run proof. |
| `non_json_rejects_missing_feature_mask` | Empty or unparsed `feature_mask`. | FAIL | Feature/fallback context cannot be producer-only prose. |
| `non_json_rejects_gate_only_behavior_claim` | Non-JSON row claims close/admission with `same_wave_consumer_class=gate_only`. | FAIL | Gate-only is valid for schema checks, not row movement. |
| `non_json_rejects_unknown_consumer_class` | Consumer class not in the same-wave validator allowlist. | FAIL | Consumer identity must be consumed by the gate. |
| `non_json_rejects_unknown_extra_field` | Extra companion-report key such as `pmu_cycles`. | FAIL | Producer-only telemetry fails closed. |
| `non_json_rejects_validator_only_requirement` | Validator requires a field the fixture/report does not emit. | FAIL | Schema and gate must move together. |
| `non_json_rejects_generated_baseline_claim` | `baseline_authority=true`, `wave_id=SK-V11-W1b`, or baseline verdict. | FAIL | W1a cannot create W1b authority. |
| `non_json_rejects_row_admission_claim` | `A / GO`, close flag, or admission verdict on a W1a non-JSON fixture. | FAIL | W1a cannot admit direct, typed, parse-only, or non-JSON rows. |
| `non_json_rejects_parse_only_sota_claim` | `parse_only` non-JSON fixture claims SOTA/close. | FAIL | Parse-only remains diagnostic. |
| `non_json_rejects_json_provider_generality_claim` | JSON-provider emission or renamed JSON helper is presented as Lock 14 proof. | FAIL | JSON-only emission cannot prove non-JSON generality. |
| `non_json_rejects_hand_runtime_generality_claim` | Old hand-written non-JSON runtime is presented as generated parser proof. | FAIL | W1b/W2 require generated direct/typed evidence, not hand-only proof. |

## Same-Wave Consumption Proof

The W1a implementation must prove consumption in the same wave, using tests and
a named command. A passing report alone is not evidence unless each identifier
below has a validator predicate and at least one mutation fixture that fails.

| Field family | Required consumer proof | Failure fixture class |
|---|---|---|
| Strict schema keys | Parser rejects unknown keys before semantic validation. | `non_json_rejects_unknown_extra_field` |
| Row identity | Validator checks `row_id`, `grammar_id`, `domain`, corpus, workload, suffix, and uniqueness. | grammar/domain/row-id failures |
| Outcome/verdict | Existing enum only; no new W1a outcome variant. | row admission and parse-only failures |
| Output plane | Validator checks row plane, comparator/oracle plane, and strict plane equality. | missing plane, mismatch, direct-as-typed failures |
| Track 1 | Validator reads finite positive `track1_mbps` but does not treat it as baseline authority. | missing Track 1 and baseline-claim failures |
| Track 2/oracle | Validator reads Track 2/oracle value, source, freshness, and independence. | missing oracle, stale oracle, coupling failures |
| Comparator/oracle source | Validator reads id, strictness, freshness, value, plane, source artifact, and source-provenance class. It accepts only W1a schema-owned `oracle:w1a:` sentinels and rejects generated/runtime/shared parser evidence even when the status string claims independence. | missing id/source, stale freshness, shared-source failures |
| Run and provenance | Validator checks uniform run id, profile artifact, sample cost, and sample count. | missing/mixed run and profile/sample failures |
| Host and features | Validator checks build flags, host triple, and feature mask. | missing host/build/feature failures |
| Consumer class | Validator checks that W1a fixtures are schema-only and any behavior-shaped claim is not `gate_only`. | gate-only behavior and unknown consumer failures |
| Diagnostic nonproducer status | Validator keeps PMU/cycles/scan/probe/slope facts non-producing unless a same-wave SPEC/gate change consumes them. | unknown extra field and diagnostic producer failures |

The named proof command is the `bbnf-bench` gate path because W1a does not own
`skinny/xtask/src/main.rs`:

```sh
cargo run -p bbnf-bench --bin gate -- --w1a-non-json-report ../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-pass-css-l4.json
```

The gate name, manifest path, strict parser, unknown-field rejection, shared
source rejection, and JSON preservation command must all be present in the same
wave. If the direct `bbnf-bench` command does not exist, W1a cannot claim report
consumption.

## Row Movement Prohibition

W1a must preserve these prohibitions:

- No parser row moves in W1a.
- No JSON `RESULTS.md` row moves.
- No direct, typed, parse-only, or non-JSON row admission.
- No generated non-JSON baseline authority.
- No W0-clamped row admission from opening throughput.
- No generated CSS, Sheets, or BBNF-self runtime output.
- No change to JSON-provider emission as a generality proof.
- No new directive, BIR variant, `BackendShape`, public substrate API, hidden
  sidecar, or hidden schema fact.
- No report column, manifest field, comparator field, profile field, PMU/cycles
  field, or companion-report key may be emitted without a same-wave consumer.

The row-movement proof is both semantic and file-based:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
git diff --exit-code -- RESULTS.md
```

Any diff in `skinny/RESULTS.md` during W1a is a gate failure unless CHALLENGE has
explicitly revised W1a ownership. JSON row byte identity is the default rule.

## CH1-CH6 Challenge Risk Matrix

| Lens | W1a risk | Challenge test | Reject if |
|---|---|---|---|
| CH1 correctness and measurable gates | Schema wording passes while required fields are absent or optional. | Mutate every required non-JSON identifier and require the gate to fail. | Any missing grammar/domain/output-plane/oracle/Track 2/run/host/feature/consumer field passes. |
| CH2 generality and Lock 14 | JSON-provider emission, JSON helper renames, or hand-only non-JSON runtime is treated as generality. | Require canonical `css_l4`, `sheets`, or `bbnf_self` row identity and gate-consumed oracle fields. | JSON-only evidence, `google_sheets` spelling drift without SPEC update, or hand runtime proof passes. |
| CH3 regression and REDRESS preblocks | W1a reopens blocked route families under schema plumbing. | Cross-check REDRESS 34, 35, 36, 37, 38, 48, 85, 86, 87, 100, 101, 109, and 110. | Any sidecar, shared Track 1/Track 2 parser, stale comparator, row movement, or producer-only field passes. |
| CH4 cost and budget | Behavior work hides inside the gate lane. | Keep future W1a redress to C9 gate/report/fixture scope, <=260 handwritten source/test/gate LOC, and 0 generated LOC unless fixtures are named. | Parser/codegen/runtime/generated changes are needed to pass W1a. |
| CH5 hidden coupling and Lock 1 | Track 2/oracle evidence is actually generated Track 1, SinkOnly helper reuse, or benchmark-private parser reuse. | Require source-path separation and a failing coupled-source fixture. | Gate cannot distinguish independent oracle evidence from shared parser evidence. |
| CH6 anti-paper-close | W1a claims non-JSON close from schema presence or gate-only consumers. | Require explicit non-admitting W1a status and reject baseline/admission flags. | Fixture or report can claim W1b baseline authority, W2 intervention admission, or SK-V11 close. |

## Measurement And Verification Commands

Run from `/Users/mkbabb/Programming/bbnf-lang/skinny` unless noted.

JSON preservation and focused unit surfaces:

```sh
cargo test -p bbnf-bench report::tests -- --nocapture
cargo test -p bbnf-bench gate::tests -- --nocapture
cargo test -p bbnf-bench metadata -- --nocapture
cargo test -p bbnf-bench --bin gate w0_ -- --nocapture
cargo test -p xtask costfacts -- --nocapture
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
git diff --exit-code -- RESULTS.md
```

Non-bench proof surfaces that must remain non-admitting:

```sh
cargo test -p test-fixtures -- --nocapture
cargo test -p runtime event_grammar -- --nocapture
cargo test -p runtime --features proof event_grammar -- --nocapture
```

Required W1a implementation commands, once the gate exists:

```sh
cargo test -p bbnf-bench report::tests::w1a -- --nocapture
cargo test -p bbnf-bench --bin gate w1a -- --nocapture
cargo run -p bbnf-bench --bin gate -- --w1a-non-json-report ../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-pass-css-l4.json
cargo run -p bbnf-bench --bin gate -- --w1a-non-json-report ../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-producer-only-extra-field.json
cargo run -p bbnf-bench --bin gate -- --w1a-non-json-report ../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-track2-coupled.json
cargo run -p bbnf-bench --bin gate -- --w1a-non-json-report ../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-track2-shared-source.json
cargo run -p bbnf-bench --bin gate -- --w1a-non-json-report ../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-admission-claim.json
```

The pass fixture command must pass. The producer-only, coupled-status,
shared-source, and admission-claim commands must fail.

Optional full W0 rerun is not required for W1a. If a later approved redress
refreshes Criterion data, use an isolated target and criterion root:

```sh
CARGO_TARGET_DIR=/tmp/skv11-w1a-target CRITERION_HOME=/tmp/skv11-w1a-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- bench-json --advisory
CRITERION_HOME=/tmp/skv11-w1a-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
git diff --exit-code -- RESULTS.md
```

## Revert And REDRESS Trigger

The future W1a revert unit is one slice: gate/report/metadata/fixture changes
and any companion W1a report fixtures. Revert is mandatory if the gate accepts
missing non-JSON required fields, accepts producer-only telemetry, weakens JSON
`gate-json`, moves any JSON row, admits any row, claims generated non-JSON
baseline authority, leaks JSON policy into generic proof, or adds hidden
directive/BIR/substrate/schema facts.

The REDRESS record for a W1a miss must preserve the failed fixture or command,
the rejected field/provenance pattern, whether JSON gate behavior changed, and
why the schema could not consume non-JSON evidence without weakening JSON.

Self-verdict: ACCEPT as a Phase 2 plan artifact. This file changes no source,
generated output, or `skinny/RESULTS.md` row.
