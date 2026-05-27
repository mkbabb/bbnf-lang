# SK-V14 W10 Redress: JSON parse_only Distinct Path

Status: MIXED
Gate: G-SK-V14-W10-JSON-PARSE-ONLY-READMIT
Redress: REDRESS-217

## Disposition

W10 shipped the unconditional Stage-0 F-V2-P1ABC-RERECORD evidence before row
admission, then re-attempted all 17 JSON parse_only rows through the distinct
`runtime::generated_json::parse_only` path.

Six rows admit:

| row | Track 1 Mbps | Skipper Mbps | margin over Skipper+1 |
|---|---:|---:|---:|
| json/mesh/parse_only/main | 11669.302 | 6589.818 | 5078.484 |
| json/marine_ik/parse_only/main | 9505.490 | 5338.935 | 4165.555 |
| json/numbers/parse_only/main | 14472.308 | 7452.774 | 7018.534 |
| json/unicode_escapes/parse_only/main | 7897.449 | 2984.079 | 4912.370 |
| json/unicode_basic/parse_only/main | 9445.728 | 7059.901 | 2384.827 |
| json/y_string_unicode/parse_only/main | 3169.901 | 2417.909 | 750.992 |

Eleven rows remain open: `twitter`, `citm_catalog`, `canada`,
`apache_builds`, `github_events`, `update_center`, `random`, `gsoc-2018`,
`instruments`, `unicode_mixed`, and `distinct_values`.

## Evidence

- Stage-0 profile:
  `restart/skinny/tranches/sk-v14/research/skv14-W10-stage0-profile.json.gz`
- Cold profile sweep:
  `restart/skinny/tranches/sk-v14/research/skv14-W10-profile-direct.tsv`
- Raw run log:
  `restart/skinny/tranches/sk-v14/research/skv14-W10-profile-direct.raw.log`
- Unit evidence:
  `cargo test -p runtime generated_parse_only_accepts_and_rejects_json -- --nocapture`
- Codegen evidence:
  `cargo test -p codegen emits_distinct_json_parse_only_path_without_tape_builder -- --nocapture`
- Gate/report evidence:
  `cargo test -p bbnf-bench skv14_json_parse_only_report_accepts -- --nocapture`
  and `cargo test -p xtask -- --nocapture`

## Routed Residual

The open rows are measured residuals, not governance blocks. They remain OPEN
because cold Track 1 does not clear same-corpus Skipper + 1.0 Mbps. W11 may
close SK-V14 with this mixed W10 disposition; no open W10 row may be cited as
admitted evidence.
