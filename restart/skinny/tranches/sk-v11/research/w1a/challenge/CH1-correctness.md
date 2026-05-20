# SK-V11 W1a CH1 Correctness Challenge

Scope: CH1 correctness and measurable gate review for the W1a non-JSON
gate/report schema lane.

Read set:

- `restart/skinny/tranches/sk-v11/SPEC.md` section 4.
- `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md` W1a, falsifiability,
  telemetry, and outcome-discipline references.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R1-gate-validator.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R2-report-metadata.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R3-fixtures-tests.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R4-nonjson-row-shape.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R5-telemetry-contract.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R6-redress-boundaries.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-plan-gate-matrix.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-plan-implementation.md`.

## CH1 Verdict

The W1a implementation plan is correct and implementable for CH1 if implemented
as written: an additive companion non-JSON evidence lane in
`skinny/crates/bbnf-bench/src/report.rs`, reached directly from
`skinny/crates/bbnf-bench/src/bin/gate.rs` with
`--w1a-non-json-report <path>`, with fixtures under the W1a research tree. This
does not require relaxing the existing JSON W0 `Report::validate_schema_v3()` or
`Report::validate_sk_v8_w0()` path, does not require `skinny/xtask`, and does
not require `skinny/RESULTS.md` movement.

The decisive correctness property is fail-closed schema consumption. The
planned `#[serde(deny_unknown_fields)]` top-level report, row, and oracle
structs are the right shape. Missing required fields should fail through strict
deserialization where possible, and semantic mismatches should fail in
`validate_w1a_non_json_gate()`. The implementation must not parse into
`serde_json::Value` and then ignore unknown keys, because that would preserve
the producer-only telemetry bug W1a is meant to close.

## Required Gate Predicates

The plan's validator must consume, not merely carry, these non-JSON identifiers:

- Report identity: `schema_version = sk-v11-w1a-nonjson-v1`,
  `wave_id = SK-V11-W1a`, uniform W1a `run_id`, non-empty `rows`.
- Row identity: exact `grammar_id` allowlist `css_l4`, `sheets`, `bbnf_self`;
  exact domain mapping to `<grammar_id>_bench`; row id shape
  `<grammar_id>/<corpus>/<workload>/main`; no duplicate row ids.
- Evidence shape: `output_plane`, strictness, Track 1 Mbps, Track 2/oracle
  Mbps, measured validation path, profile artifact, sample cost, sample count,
  build flags, host triple, feature mask, CostFacts sentinels, redress status,
  SK-V9-open delta, substrate tuple, same-wave consumer class, Track 2/oracle
  independence status, diagnostic nonproducer status, and exactly one oracle
  entry.
- Oracle shape: oracle id, plane, strictness, freshness, sidecar freshness,
  positive finite Mbps, and non-empty source artifact.
- Boundary shape: W1a pass evidence remains schema-only with
  `outcome_id = S`, `verdict = NO-GO`, and
  `same_wave_consumer_class = non_json_gate_schema_only`; any `A / GO`, W1b
  wave id, baseline authority, generated-baseline, admission, or close claim
  fails.

The selected CSS pass row `css_l4/declaration_values/direct/main` is valid for
W1a because it is a schema-only future-row shape, not a benchmark baseline. The
canonical non-JSON grammar spellings should remain `css_l4`, `sheets`, and
`bbnf_self`; `google_sheets` must reject unless a same-wave SPEC revision adds
that spelling.

## Fixture And Test Challenge

The named file fixtures in the plan are acceptable as the CLI proof surface:

- `nonjson-pass-css-l4.json` must pass.
- `nonjson-producer-only-extra-field.json` must fail by strict unknown-field
  rejection.
- `nonjson-track2-coupled.json` must fail by Track 2/oracle independence
  validation.
- `nonjson-admission-claim.json` must fail by W1a no-admission/no-baseline
  validation.

Those four files are not enough by themselves. CH1 requires focused unit tests
or compact fixture mutations for every required rejection class named in the
plan: missing or unknown grammar id, domain mismatch, row id mismatch,
duplicate row id, missing output plane, oracle plane mismatch, missing oracle
source, missing or mixed run id, missing host, missing build flags, missing
sample count or sample cost, missing feature mask, `gate_only` non-JSON
consumer misuse, unsupported outcome/verdict, wrong wave id, generated baseline
claim, and diagnostic producer misuse. If any of those mutations can pass, W1a
does not satisfy `G-W1a-NONJSON-GATE`.

The direct CLI consumer must short-circuit before the existing JSON Criterion
report generation path, read only the supplied report, validate it, print
`G-W1a-NONJSON-GATE PASS <path>` on success, and reject combinations with
`--update-results`, `--write-results`, or `--include-volatile-probes`. It should
also fail on a missing path, repeated `--w1a-non-json-report`, or unrelated
extra arguments so the companion lane cannot accidentally inherit JSON gate
flags.

## JSON Preservation Challenge

The existing JSON gate remains the control. W1a may add code to `report.rs` and
`bin/gate.rs`, but must not broaden JSON row identity, JSON comparator rules,
or W0 manifest semantics. Required evidence after implementation:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo test -p bbnf-bench report::tests::w1a -- --nocapture
cargo test -p bbnf-bench --bin gate w1a -- --nocapture
cargo run -p bbnf-bench --bin gate -- --w1a-non-json-report ../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-pass-css-l4.json
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
git -C .. diff --exit-code -- skinny/RESULTS.md
```

The producer-only, Track 2 coupled, and admission-claim fixtures must be run as
expected failures. JSON preservation is not optional; if JSON `gate-json`
requires weakening to make W1a pass, W1a fails CH1.

## Implementability

The implementation is feasible inside the requested owner paths:

- `report.rs` can host the strict companion structs, parser, semantic
  validator, and focused unit tests without touching JSON W0 validation.
- `bin/gate.rs` can add a narrow argument extractor and direct report consumer
  before Criterion discovery.
- `research/w1a/fixtures/` can hold compact pass/fail JSON fixtures.

The LOC budget is tight but credible if the implementation keeps the row struct
flat, uses small allowlist helpers, and uses one valid fixture builder plus
targeted mutations for most tests. Adding `xtask`, metadata keys, benches,
generated parsers, runtime/codegen behavior, or `RESULTS.md` rows would be a
wrong-wave expansion rather than a CH1 implementation need.

Required changes: none to the current W1a plan; the field-level rejection tests
above are mandatory implementation obligations.

DISPOSITION: ACCEPT
