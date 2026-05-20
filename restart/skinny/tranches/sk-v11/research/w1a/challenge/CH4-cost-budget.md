# SK-V11 W1a CH4 Cost/Budget Challenge

Date: 2026-05-20.
Lens: CH4 cost/budget.
Scope: W1a companion non-JSON gate evidence lane after S-P3 V4 convergence.

## Authorities Read

- `restart/skinny/tranches/sk-v11/SPEC.md` Section 4 and wave budget table.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-plan-implementation.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-plan-gate-matrix.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R1-gate-validator.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R2-report-metadata.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R3-fixtures-tests.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R4-nonjson-row-shape.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R5-telemetry-contract.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R6-redress-boundaries.md`.

## Budget Finding

W1a's hard cap is `<=260` handwritten source/test/gate LOC, `0` generated LOC
unless fixtures are named, and a `<=90 min` redress cap. The implementation plan
targets `<=225` LOC: `report.rs <=190`, `bin/gate.rs <=35`, and compact named
fixtures. That is tight but feasible only if the later patch stays additive and
table-driven.

The accepted cost shape is:

- Add strict serde-deserialized companion report structs and validation in
  `skinny/crates/bbnf-bench/src/report.rs`.
- Add exactly one direct gate binary hook,
  `--w1a-non-json-report <path>`, in
  `skinny/crates/bbnf-bench/src/bin/gate.rs`.
- Add only named schema fixtures under
  `restart/skinny/tranches/sk-v11/research/w1a/fixtures/`.
- Keep `skinny/xtask`, `skinny/RESULTS.md`, `metadata.rs`, benches, codegen,
  runtime, parser, ASM, grammar behavior, and generated output untouched.

The plan passes CH4 because it selects a companion evidence lane instead of
rendering non-JSON rows into `RESULTS.md`, avoids a new `xtask` command, avoids
Criterion benchmark work, and keeps W1b/W2 responsible for generated non-JSON
baseline and behavior evidence.

## Fixture And Test Cost

The fixture set is cost-acceptable if file fixtures remain small and gate-facing:

- one passing CSS L4 schema-only companion report;
- one producer-only unknown-field rejection;
- one Track 2/oracle coupling rejection;
- one admission or generated-baseline claim rejection.

Identity/domain/missing-context/oracle-plane cases may be covered by focused
`report.rs` unit tests using the same serde path, rather than expanded into a
large file-fixture matrix. Do not add real non-JSON benchmark fixtures,
`test-fixtures` corpora, or Criterion data in W1a.

The implementation must still prove same-wave consumption: every accepted
non-JSON field needs a validator predicate, and each required field family needs
at least one focused failing test or named failing fixture. If that coverage
cannot fit under the hard cap, stop before source work and return to CHALLENGE.

## Command Cost

The direct command path is accepted:

```sh
cargo run -p bbnf-bench --bin gate -- --w1a-non-json-report ../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-pass-css-l4.json
```

Required failure commands should use the same flag and should fail nonzero for
producer-only fields, Track 2/oracle coupling, and admission or baseline claims.

JSON preservation remains separate and mandatory:

```sh
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
git -C .. diff --exit-code -- skinny/RESULTS.md
git -C .. diff --exit-code -- skinny/crates/codegen skinny/crates/runtime skinny/crates/bbnf-simd
```

Do not add `cargo run -p xtask -- gate-non-json` for W1a. The plan's direct
`bbnf-bench --bin gate` hook is the lower-cost owner-path-compliant route.

## Parsing And LOC Guardrails

The companion report must use serde deserialization with
`#[serde(deny_unknown_fields)]` on the report, row, and oracle structs. The gate
must not use ad hoc text parsing for JSON report content. Limited row-id token
splitting is acceptable after serde has parsed the typed report.

To remain inside CH4:

- keep JSON `Report::validate_schema_v3()` and `Report::validate_sk_v8_w0()`
  behavior unchanged;
- do not broaden comparator, row-id, or W0 profile validators in place for
  non-JSON;
- avoid new metadata keys, new report columns, new manifest fields, new benches,
  and new generated files;
- reject flag combinations with `--update-results`, `--write-results`, or
  `--include-volatile-probes`;
- prefer compact helper functions and mutation-style unit tests over a broad
  custom harness.

## Redress Scope

The redress unit remains one slice: `report.rs`, `bin/gate.rs`, and named W1a
fixtures. Any need to touch `skinny/xtask`, `RESULTS.md`, codegen/runtime/parser
behavior, benchmark harnesses, or generated output is outside W1a and should
return REVISE before implementation.

Hard stop triggers:

- implementation exceeds `260` handwritten source/test/gate LOC;
- fixtures expand into benchmark/runtime corpora or generated artifacts;
- JSON `gate-json` behavior is weakened or row bytes move;
- a non-JSON baseline, admission, or close claim appears;
- the validator accepts producer-only fields or missing required non-JSON
  evidence.

Required changes: none before source work. Keep the later patch within the
owner paths, direct CLI hook, serde-only companion parsing, compact fixture set,
and focused tests described above.

DISPOSITION: ACCEPT
