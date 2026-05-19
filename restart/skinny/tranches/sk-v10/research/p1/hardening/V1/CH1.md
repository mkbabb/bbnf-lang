# SK-V10 S-P1 V1 CH1: Correctness

Disposition: ACCEPT AFTER REVISE.
Date: 2026-05-19.
Scope: symbol correctness, row coverage, PMU provenance, and falsifiability of
the S-P1 V1 profile packet.
Output: this file.

## Findings

CH1 returned REVISE on four correctness defects:

- P1-C's Mode III throughput table did not state that SK JSON Mbps is
  megabits per second, so the values looked like an unexplained 8x multiplier
  over Criterion byte throughput.
- P1-C cited nonexistent bench paths, `skinny/benches/json_parity.rs` and
  `skinny/benches/simd_scan.rs`.
- P1-A/P1-B/P1-E carried drifted source anchors for
  `validate_string_escape` and `match_number_span_from_first`.
- P1-A carried drifted generated-parser anchors for `dispatch_value`,
  `parse_value_at`, and `consume_array_next`.

Checks that passed before the fold:

- P1-A/P1-B Time Profiler percentages matched the exported summaries.
- P1-D PMU table matched `/tmp/skv10-p1/parse-xctrace/pmu_rows.tsv`.
- P1-F row counts matched `skinny/RESULTS.md`: 17 parse, 17 direct, 6 typed.

## Fold

- P1-C now states the formula `bytes * 8_000 / mean_ns`, names
  `new/benchmark.json` as the byte-count source, and says there is no hidden
  batch factor.
- P1-C sources now point at
  `skinny/crates/bbnf-bench/benches/json_parity.rs` and
  `skinny/crates/bbnf-bench/benches/simd_scan.rs`.
- P1-A/P1-B/P1-E source anchors now use function-start lines:
  `validate_string_escape` at `parse-that-regex/src/lib.rs:284`,
  `match_number_span_from_first` at `number/mod.rs:38`,
  `dispatch_value` at `generated.rs:47`, `parse_value_at` at
  `generated.rs:37`, and `consume_array_next` at `generated.rs:348`.

## Disposition

ACCEPT. The remaining profile numbers are falsifiable from retained trace,
Criterion, and PMU artefacts.
