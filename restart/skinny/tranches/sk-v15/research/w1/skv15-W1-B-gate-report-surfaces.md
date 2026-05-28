# SK-V15 W1-B: Gate And Report Live-Admit Paths

Date: 2026-05-28.
Scope: CSS gate, report, and bench surfaces that can reinterpret W8R broadcast evidence.
Output: this file.

## Findings

- `restart/skinny/ROLLING-SOTA-DELTA.md` is the live stale admission surface:
  all 24 CSS L4 rows still use `css_l4_full_parse` and `ADMITTED` at
  `restart/skinny/ROLLING-SOTA-DELTA.md:70` through
  `restart/skinny/ROLLING-SOTA-DELTA.md:93`, even though the gate note says
  positive diagnostic parse margins remain `OPEN`.
- The rolling-delta gate currently allows that state. It detects demoted CSS
  rows from `not_admitted:` plus `AUDIT-FALSIFIED` in
  `skinny/xtask/src/main.rs:1646`, but `validate_skv13_rolling_delta` still
  permits `ADMITTED` when numeric CSS evidence exists at
  `skinny/xtask/src/main.rs:1480`.
- The historical W8R harness can still emit a 24-row admit from one aggregate
  profile tuple. `run_production_attempt` computes one aggregate pass, maps it
  to `W8_SELECTED_CSS_ROWS`, and returns `W8Disposition::Admitted` at
  `skinny/crates/bbnf-bench/src/css_l4_w8.rs:136`,
  `skinny/crates/bbnf-bench/src/css_l4_w8.rs:144`, and
  `skinny/crates/bbnf-bench/src/css_l4_w8.rs:474`.
- Legacy SK-V14 validators still accept old W8R CSS sustained rows as valid
  admission authority at `skinny/crates/bbnf-bench/src/report.rs:4268` and
  `skinny/xtask/src/main.rs:1011`.
- The SK-V15 W0 validator is not the remaining hole: it rejects CSS rows with
  `AUDIT-SUSTAINED`, `PASS:*`, or `admitted:*`, and rejects broadcast groups
  containing live-admission evidence at `skinny/xtask/src/skv15_w0.rs:500`
  and `skinny/xtask/src/skv15_w0.rs:657`.

## Recommendations

- In `validate_skv13_rolling_delta`, require `OPEN` whenever the corresponding
  RESULTS CSS metric is audit-falsified diagnostic evidence. Reject
  `ADMITTED` in that state.
- Change the 24 CSS rows in `restart/skinny/ROLLING-SOTA-DELTA.md` from
  `ADMITTED` to `OPEN`.
- Remove W8R CSS from the sustained-admission allowlists in both report and
  xtask validators.
- Demote `css_l4_w8::run_production_attempt` to diagnostic output with zero
  admitted rows.

## Risks

- A rolling-delta-only edit would leave the old W8R harness and legacy
  validators able to recreate a live admit. W1 close requires all three paths
  to be fail-closed or diagnostic-only.
- Editing `css_l4_w8.rs` must be isolated from existing dirty formatting
  changes and must not delete providers or generated runtime files.

## Sources

- `restart/skinny/ROLLING-SOTA-DELTA.md:70`
- `restart/skinny/ROLLING-SOTA-DELTA.md:93`
- `skinny/xtask/src/main.rs:1480`
- `skinny/xtask/src/main.rs:1646`
- `skinny/crates/bbnf-bench/src/css_l4_w8.rs:136`
- `skinny/crates/bbnf-bench/src/css_l4_w8.rs:144`
- `skinny/crates/bbnf-bench/src/css_l4_w8.rs:474`
- `skinny/crates/bbnf-bench/src/report.rs:4268`
- `skinny/xtask/src/main.rs:1011`
- `skinny/xtask/src/skv15_w0.rs:500`
- `skinny/xtask/src/skv15_w0.rs:657`
