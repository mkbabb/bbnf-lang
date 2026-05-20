# SK-V12 W0 Redress: Telemetry Lock Gate Surface

Date: 2026-05-20.
Wave: W0 - Baseline Profile And Telemetry Lock.
Gate: `G-W0-SK-V12-OPEN`.
Disposition: ADMITTED.

## Implementation

W0 landed a report/gate-only implementation slice:

- `skinny/crates/bbnf-bench/src/report.rs` defines and validates
  `sk-v12-nonjson-generated-v1` companion reports.
- `skinny/crates/bbnf-bench/src/bin/gate.rs` accepts
  `--skv12-non-json-report <path>` and runs Lock 14 validation before any
  non-JSON companion gate returns.
- `skinny/xtask/src/main.rs` passes companion non-JSON report flags through
  `xtask gate-json`.
- `restart/skinny/tranches/sk-v12/research/skv12-W0-nonjson-pass.json` is the
  W0 passing companion report used to prove the gate lane.

No parser, scanner, SIMD/ASM, generated runtime output, codegen behavior,
benchmark body, `skinny/RESULTS.md`, or `skinny/REDRESS.md` change is included.
`skinny/REDRESS.md` is intentionally untouched because SPEC Section 3 records a
W0 REDRESS entry only if the lock fails.

## Gate Evidence

All commands below ran from `/Users/mkbabb/Programming/bbnf-lang/skinny` with
`RUSTFLAGS="-C target-cpu=native"` where shown.

| Check | Result |
|---|---|
| `cargo test -p bbnf-bench skv12_non_json_report --lib` | PASS, 3 tests |
| `cargo test -p bbnf-bench w1a_non_json_report --lib` | PASS, 6 tests |
| `cargo test -p bbnf-bench --bin gate skv12_non_json_report_arg` | PASS, 1 test |
| `cargo test -p xtask gate_json_passthrough` | PASS, 1 test |
| `cargo run -p xtask -- gate-json --skv12-non-json-report ../restart/skinny/tranches/sk-v12/research/skv12-W0-nonjson-pass.json` | PASS, printed `G-W0-SK-V12-NONJSON-GATE PASS` |
| `cargo run -p xtask -- gate-json --w1a-non-json-report ../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-pass-css-l4.json` | PASS, printed `G-W1a-NONJSON-GATE PASS` |
| `cargo run -p xtask -- check-json` | PASS |
| `cargo run -p xtask -- check-real-typed` | PASS |
| `cargo run -p xtask -- check-conformance` | PASS, 21 valid fixtures accepted and 7 invalid fixtures rejected |
| `CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df cargo run -p xtask -- gate-json --advisory --check-results` | PASS, overall remains expected `N-direct / NO-GO` |
| `CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df cargo run -p xtask -- gate-json --with-cost-facts --check-results` | PASS |

## Gate Result

`G-W0-SK-V12-OPEN` passes:

- The 41 JSON main rows keep the SK-V12 opening outcomes.
- The retained opening Criterion authority at
  `/tmp/skv11-open-criterion-3ce75df` is consumed by `gate-json`.
- The SK-V12 companion non-JSON report lane is executable through `xtask
  gate-json` and rejects producer-only/malformed evidence through
  `deny_unknown_fields` plus explicit semantic validation.
- Lock 14 validation now runs before W1a or SK-V12 companion non-JSON report
  gates return.
- No behavior source, generated runtime output, benchmark body, RESULTS, or
  REDRESS drift is present in the W0 slice.

Next wave: W1 may dispatch the generated non-JSON baseline only after this W0
redress commit lands.
