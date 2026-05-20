# SK-V11 W1a Phase 2 Plan: Non-JSON Gate/Report Schema Lane

Status: P1 implementation contract.
Owned artifact for this turn: `restart/skinny/tranches/sk-v11/research/w1a/w1a-plan-implementation.md`.
Source edits in this turn: none.

## Selected Intervention

Select SPEC Section 4 C9 accounting: add a companion, gate-consumed non-JSON
evidence report lane for `G-W1a-NONJSON-GATE`.

This is not a parser, baseline, or row-movement intervention. The redress patch
must teach the report/gate path to accept or reject the exact non-JSON evidence
shape that W1b will later need, while leaving JSON `gate-json` validation
byte-for-byte strict.

Implementation shape:

- Add strict non-JSON companion-report structs and validation in
  `skinny/crates/bbnf-bench/src/report.rs`.
- Add one direct CLI hook in `skinny/crates/bbnf-bench/src/bin/gate.rs`:
  `--w1a-non-json-report <path>`.
- Add named W1a JSON fixtures under
  `restart/skinny/tranches/sk-v11/research/w1a/fixtures/`.
- Do not add a new `xtask` command; `skinny/xtask/src/main.rs` is outside the
  W1a owner set.

## Owner Paths

Allowed for the later redress patch:

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `restart/skinny/tranches/sk-v11/research/w1a/fixtures/`

Explicitly unused owner paths:

- `skinny/crates/bbnf-bench/src/metadata.rs`: no new metadata keys.
- `skinny/crates/bbnf-bench/benches/`: no non-JSON Criterion benchmark in W1a.

## No-Source Boundary

Do not edit:

- `skinny/crates/bbnf-bench/src/gate.rs`
- `skinny/xtask/src/main.rs`
- `skinny/RESULTS.md`
- `skinny/crates/codegen/`
- `skinny/crates/runtime/`
- `skinny/crates/bbnf-simd/`
- grammar files
- generated parser/runtime files
- any parser, oracle, direct, typed, string, numeric, dispatch, escape, sink, or
  substrate behavior source

No JSON row may move. No non-JSON generated baseline authority may be claimed.
No generated output is allowed.

## Structs And Functions To Add

In `skinny/crates/bbnf-bench/src/report.rs`:

- `pub const W1A_NON_JSON_REPORT_SCHEMA: &str = "sk-v11-w1a-nonjson-v1";`
- `#[serde(deny_unknown_fields)] pub struct NonJsonEvidenceReport`
  - `schema_version: String`
  - `wave_id: String`
  - `run_id: String`
  - `rows: Vec<NonJsonEvidenceRow>`
- `#[serde(deny_unknown_fields)] pub struct NonJsonEvidenceRow`
  - carries the required schema-v3 semantic identifiers: row identity,
    grammar/domain/corpus/workload, outcome/verdict, strictness, UTF-8/escape,
    output plane, Track 1/Track 2 Mbps, measured validation path, profile,
    sample cost/count, build/host/feature facts, CostFacts sentinels, redress,
    SK-V9-open delta, substrate tuple, consumer class, Track 2 independence,
    diagnostic nonproducer status, and one oracle entry.
- `#[serde(deny_unknown_fields)] pub struct NonJsonOracleEvidence`
  - `oracle_id`
  - `oracle_plane`
  - `oracle_strictness`
  - `oracle_freshness`
  - `sidecar_freshness`
  - `value_mbps`
  - `source_artifact`
- `impl NonJsonEvidenceReport`
  - `pub fn from_json_str(text: &str) -> Result<Self, String>`
  - `pub fn validate_w1a_non_json_gate(&self) -> Result<(), String>`
- Private helpers:
  - `validate_w1a_non_json_row`
  - `parse_w1a_non_json_row_id`
  - `w1a_domain_for_grammar`
  - `validate_w1a_oracle`
  - `validate_w1a_structured_context`

Required accepted fixture values:

- `schema_version = "sk-v11-w1a-nonjson-v1"`
- `wave_id = "SK-V11-W1a"`
- `run_id = "sk-v11-w1a:fixture-fnv64-<16 lowercase hex>"`
- `grammar_id` in `css_l4`, `sheets`, `bbnf_self`
- exact domains: `css_l4_bench`, `sheets_bench`, `bbnf_self_bench`
- row id shape: `<grammar_id>/<corpus>/<workload>/main`
- W1a pass fixture row: `css_l4/declaration_values/direct/main`
- `outcome_id = "S"` and `verdict = "NO-GO"` for schema-only evidence
- `same_wave_consumer_class = "non_json_gate_schema_only"`
- `track2_independence_status = "independent_verified"`
- oracle id `internal_oracle`, same output plane, strictness `strict`,
  freshness `same-run-oracle`, sidecar `n/a`, finite positive Mbps, nonempty
  source artifact

Required rejections:

- missing/unknown `grammar_id`
- domain mismatch
- row id mismatch or duplicate row id
- missing output plane
- oracle plane mismatch
- missing oracle/source artifact
- Track 2/oracle coupling
- missing, malformed, or mixed run id
- missing host, build flags, sample count, sample cost, or feature mask
- `gate_only` consumer on non-JSON evidence
- unknown extra companion-report key
- `outcome_id = "A"`, `verdict = "GO"`, `wave_id = "SK-V11-W1b"`, or any
  generated baseline/admission claim
- diagnostic PMU/cycles/profile fields used as producer evidence

## CLI To Add

In `skinny/crates/bbnf-bench/src/bin/gate.rs`:

- Import `NonJsonEvidenceReport`.
- Before the existing JSON Criterion/report path, recognize:

```text
--w1a-non-json-report <path>
```

- Behavior:
  - read the JSON file;
  - parse with `NonJsonEvidenceReport::from_json_str`;
  - call `validate_w1a_non_json_gate`;
  - print `G-W1a-NONJSON-GATE PASS <path>` on success;
  - return a nonzero error on any validation failure;
  - reject combining this flag with `--update-results`, `--write-results`, or
    `--include-volatile-probes`.

This keeps the non-JSON gate in the W1a owner surface and avoids `xtask`
expansion.

## Tests And Fixtures

Named fixtures to add:

- `restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-pass-css-l4.json`
- `restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-producer-only-extra-field.json`
- `restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-track2-coupled.json`
- `restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-admission-claim.json`

Focused `report.rs` tests to add:

- `w1a_non_json_report_accepts_css_l4_schema_fixture`
- `w1a_non_json_report_rejects_identity_domain_and_row_id_mismatch`
- `w1a_non_json_report_rejects_missing_required_context`
- `w1a_non_json_report_rejects_oracle_plane_source_and_coupling`
- `w1a_non_json_report_rejects_gate_only_and_admission_claims`
- `w1a_non_json_report_rejects_unknown_producer_fields`
- keep existing `w0_report_accepts_exact_opening_baseline` unchanged and green

Focused `bin/gate.rs` tests to add only if the CLI helper is factored:

- `w1a_non_json_report_arg_extracts_single_path`
- `w1a_non_json_report_arg_rejects_update_results_combination`

Do not add Criterion benches for W1a.

## LOC Budget

Hard budget from SPEC: <=260 handwritten source/test/gate LOC and 0 generated
LOC unless fixtures are named.

Budget for redress:

- `report.rs`: <=190 LOC for structs, validation, and focused unit tests.
- `bin/gate.rs`: <=35 LOC for CLI parsing/hook and optional tests.
- named JSON fixtures: <=40 data lines total, compact formatting permitted.
- total handwritten source/test/gate target: <=225 LOC, hard stop at 260 LOC.

If the implementation cannot fit under the hard budget without weakening
validation, stop and return to CHALLENGE rather than broadening scope.

## Verification Commands

Run from `/Users/mkbabb/Programming/bbnf-lang/skinny`.

Focused unit tests:

```sh
cargo test -p bbnf-bench report::tests::w1a -- --nocapture
cargo test -p bbnf-bench --bin gate w1a -- --nocapture
```

W1a pass fixture:

```sh
cargo run -p bbnf-bench --bin gate -- --w1a-non-json-report ../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-pass-css-l4.json
```

W1a required failing fixtures:

```sh
if cargo run -p bbnf-bench --bin gate -- --w1a-non-json-report ../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-producer-only-extra-field.json; then exit 1; fi
if cargo run -p bbnf-bench --bin gate -- --w1a-non-json-report ../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-track2-coupled.json; then exit 1; fi
if cargo run -p bbnf-bench --bin gate -- --w1a-non-json-report ../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-admission-claim.json; then exit 1; fi
```

JSON preservation:

```sh
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
git -C .. diff --exit-code -- skinny/RESULTS.md
git -C .. diff --exit-code -- skinny/crates/codegen skinny/crates/runtime skinny/crates/bbnf-simd
```

Do not run a non-JSON benchmark in W1a. W1b owns the first generated non-JSON
baseline and oracle measurement.

## Entry, Challenge, And Exit

Entry gate:

- W0 is closed.
- CHALLENGE accepts this W1a companion-report gate extension.
- No behavior row movement is included.

CHALLENGE focus:

- CH1: fail-closed missing-field and producer-only fixtures.
- CH2: grammar/domain allowlist is non-JSON and does not reuse JSON provider
  proof.
- CH3: REDRESS 34, 35, 36, 37, 38, 48, 85, 86, 87, 100, 101, 109, and 110
  remain preblocked.
- CH4: LOC stays under the budget above.
- CH5: oracle source and Track 2 independence are consumed by the gate.
- CH6: W1a proves schema consumption only; W1b/W2 retain baseline and
  intervention authority.

Exit gate `G-W1a-NONJSON-GATE`:

- pass fixture succeeds through the new CLI;
- all named fail fixtures fail;
- JSON `gate-json --with-cost-facts --check-results` remains green;
- `skinny/RESULTS.md` has no diff;
- no generated non-JSON baseline or parser authority is claimed.

## Revert Protocol

Revert the W1a implementation as one slice if any required predicate weakens:

- non-JSON missing fields accepted;
- producer-only fields accepted;
- Track 2/oracle coupling accepted;
- JSON gate regresses;
- `skinny/RESULTS.md` row moves;
- generated baseline or behavior admission is claimed;
- JSON policy leaks into generic/runtime/codegen paths.

Audit before reverting:

```sh
git -C /Users/mkbabb/Programming/bbnf-lang diff -- skinny/crates/bbnf-bench/src/report.rs skinny/crates/bbnf-bench/src/bin/gate.rs restart/skinny/tranches/sk-v11/research/w1a/fixtures
```

Revert only the W1a slice:

```sh
git -C /Users/mkbabb/Programming/bbnf-lang restore -- skinny/crates/bbnf-bench/src/report.rs skinny/crates/bbnf-bench/src/bin/gate.rs restart/skinny/tranches/sk-v11/research/w1a/fixtures
```

If reverted after redress starts, preserve the failed command, failing fixture,
and rejected predicate in the wave redress record. Do not use `git reset
--hard`.
