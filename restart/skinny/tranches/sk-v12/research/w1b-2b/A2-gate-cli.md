# A2 Gate CLI And No-Write Matrix

Scope: SK-V12 W1b-2b research lens A2 for `gate-json` companion report
handling. This artifact is source-read only and does not request source edits.

## Required W1b-2b CLI Surface

Section 7.2 requires a new companion report flag:

```text
--skv12-css-l4-sota-report <path>
```

The flag should behave like the existing companion report flags in
`skinny/crates/bbnf-bench/src/bin/gate.rs`:

- read exactly one path argument;
- reject missing path, flag-as-path, duplicate/mixed companion report flags, and
  unrelated extra arguments;
- validate the JSON report and print a dedicated W1b-2b gate PASS line;
- return immediately after the companion report passes unless a JSON guard flag
  requests the normal JSON RESULTS path;
- allow only no-write JSON guard flags beside the companion report:
  `--advisory`, `--check-results`, and `--with-cost-facts`.

Recommended PASS line:

```text
G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA PASS <path>
```

The new flag should be included in the shared companion flag count so it cannot
be combined with `--w1a-non-json-report` or `--skv12-non-json-report`.

## No-Write / Probe Rejection Matrix

The W1b-2b companion report is a gate consumer, not a writer or volatile probe.
It must fail closed with these combinations:

| Invocation shape | Required disposition |
| --- | --- |
| `--skv12-css-l4-sota-report <path> --update-results` | reject |
| `--skv12-css-l4-sota-report <path> --write-results` | reject |
| `--skv12-css-l4-sota-report <path> --include-volatile-probes` | reject |
| `--skv12-css-l4-sota-report <path> --w1a-non-json-report <path>` | reject |
| `--skv12-css-l4-sota-report <path> --skv12-non-json-report <path>` | reject |
| `--skv12-css-l4-sota-report <path> --rss-probe ...` | reject by top-level probe path, not report mode |
| `--skv12-css-l4-sota-report <path> --check-results` | allow and continue into JSON RESULTS check |
| `--skv12-css-l4-sota-report <path> --with-cost-facts` | allow and continue into JSON RESULTS check |
| `--skv12-css-l4-sota-report <path> --advisory --check-results` | allow and continue into advisory JSON RESULTS check |

The current shared helper already rejects `--update-results`,
`--write-results`, and `--include-volatile-probes` for existing companion
reports. W1b-2b should extend that helper rather than adding a looser parser.

## Stale RESULTS Guidance

`skinny/RESULTS.md` must not move for W1b-2b measured baseline outcomes.
Section 7.2 says PASS-MEASURED-BASELINE records REDRESS evidence and does not
move `skinny/RESULTS.md`.

For W1b-2b, stale `RESULTS.md` guidance should distinguish two cases:

- Companion report only: do not tell the operator to rerun
  `cargo xtask gate-json --update-results`; the SOTA report is no-write.
- Companion report plus JSON guard (`--check-results` or `--with-cost-facts`):
  if the existing JSON RESULTS check reaches the stale branch, keep the existing
  JSON guidance because that stale state belongs to the JSON guard surface, not
  the CSS SOTA report.

If the CSS SOTA report validates but produces PASS-MEASURED-BASELINE, the gate
should report the CSS disposition through the report/gate status and route the
remainder to REDRESS, not rewrite `skinny/RESULTS.md`.

## JSON Guard Handling

Section 7.2 requires JSON guards to run against an accepted JSON Criterion root
or a fresh populated JSON guard capture, not an empty CSS-only Criterion
directory.

CLI handling should preserve the existing companion-report guard pattern:

- no `--check-results` / `--with-cost-facts`: validate the CSS SOTA report and
  return without entering the JSON Criterion/RESULTS path;
- `--check-results` or `--with-cost-facts`: validate the CSS SOTA report first,
  then continue through the existing JSON gate path so stale or failing JSON
  guards can demote/fail the overall command;
- the SOTA report schema must carry `json_guard_state`, and validation should
  accept only `not_refreshed:no_behavior_drift` or a fresh
  `refreshed:<run-id>:guards-pass`-style state;
- a report that points JSON guards at CSS-only artifacts should be rejected as
  incomplete gate context.

The existing `companion_report_runs_json_check` predicate should be reused or
extended so W1b-2b gets identical guard semantics.

## Test Names Needed

Add gate CLI tests in `skinny/crates/bbnf-bench/src/bin/gate.rs`:

- `skv12_css_l4_sota_report_arg_extracts_single_path`
- `skv12_css_l4_sota_report_arg_rejects_write_and_probe_flags`
- `skv12_css_l4_sota_report_arg_rejects_mixed_companion_flags`
- `skv12_css_l4_sota_report_arg_allows_no_write_json_check_flags`
- `skv12_css_l4_sota_report_arg_rejects_missing_or_flag_path`

Add report validation tests in `skinny/crates/bbnf-bench/src/report.rs`:

- `skv12_css_l4_sota_report_accepts_admit_candidate`
- `skv12_css_l4_sota_report_accepts_measured_baseline_without_results_move`
- `skv12_css_l4_sota_report_rejects_missing_lightningcss_evidence`
- `skv12_css_l4_sota_report_rejects_threshold_without_margin`
- `skv12_css_l4_sota_report_rejects_stale_or_css_only_json_guard`
- `skv12_css_l4_sota_report_rejects_unknown_producer_fields`

These names intentionally mirror the existing
`skv12_non_json_report_arg_*` and `skv12_non_json_report_*` tests while making
the W1b-2b no-write and JSON-guard contract explicit.
