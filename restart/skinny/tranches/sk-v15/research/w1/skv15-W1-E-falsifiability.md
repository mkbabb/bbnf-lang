# SK-V15 W1-E: Falsifiability Gates

Date: 2026-05-28.
Scope: W1 test and command matrix for rejecting CSS live admits from W8R broadcast evidence.
Output: this file.

## Findings

- `gate-json --check-results` calls `xtask::skv15_w0::validate_results`
  before legacy checks at `skinny/xtask/src/main.rs:404`.
- The W0 validator parses 42-cell manifest rows, validates JSON/CSS rows, and
  validates broadcast groups at `skinny/xtask/src/skv15_w0.rs:118`.
- Existing tests cover acceptance, missing appended fields, visible CSS live
  admit, self-exempting exclusion, and one hidden JSON broadcast in
  `skinny/xtask/tests/skv15_w0.rs:6`.
- Report-side producer/capture tests already cover blank SK-V15 telemetry and
  W0 capture demotion at `skinny/crates/bbnf-bench/src/report.rs:9603`.

## Recommendations

- Add W1-specific xtask tests around rolling-delta and CSS live-admit
  rejection rather than relying only on RESULTS validator tests.
- Negative fixtures should reject:
  - 24 CSS rows changed back to `PASS:*`, `AUDIT-SUSTAINED`, and `admitted:*`
    while retaining the W8R tuple.
  - 24 CSS rows with unique fake `measurement_row_id` and
    `broadcast_group_id=none:independent` but identical W8R run/profile/sample
    evidence.
  - CSS rows outside the declared `CSS_FEATURES` universe.
  - CSS live admission from `full_parse_summary`, fact-stream provenance, or
    `CSS_GENERATED_RS` provenance.
  - Visible JSON demotion, missing JSON rows, non-independent JSON broadcast
    groups, or JSON CSS comparator workload.
- Add rolling-delta tests that prove demoted CSS rows must be `OPEN` when
  RESULTS marks the corresponding CSS row as `not_admitted` and
  `AUDIT-FALSIFIED`.

## Risks

- A pure producer test is insufficient because W1's live admission risk is a
  gate/ledger interpretation risk.
- If fixtures only mutate visible CSS admission markers, unique fake
  measurement ids can hide a one-to-N broadcast signature.

## Sources

- `skinny/xtask/src/main.rs:404`
- `skinny/xtask/src/skv15_w0.rs:118`
- `skinny/xtask/tests/skv15_w0.rs:6`
- `skinny/crates/bbnf-bench/src/report.rs:9603`
