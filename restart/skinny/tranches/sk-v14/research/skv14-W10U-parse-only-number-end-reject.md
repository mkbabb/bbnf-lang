# SK-V14 W10U Parse-Only Number-End Reject

## Scope

W10U tested a parse-only-only number scanner that returned just the validated
number end offset instead of constructing `NumberSpan`. The intended benefit was
to remove unused mantissa/exponent bookkeeping from
`runtime::generated_json::parse_only`.

The source change was abrogated after measurement. It is not present in HEAD.

## Result

The attempt is `REJECTED`. No open row cleared the strict
`sonic_rs::Skipper + 1.0 Mbps` gate, and the same binary made existing admits
fail in the cold evidence:

| row | Track 1 | Skipper | threshold | status |
|---|---:|---:|---:|---|
| `json/instruments/parse_only/main` | 4161.964 | 4517.452 | 4518.452 | REGRESSED-ADMIT |
| `json/unicode_mixed/parse_only/main` | 2686.532 | 4886.333 | 4887.333 | REGRESSED-ADMIT |

Representative open-row misses:

| row | Track 1 | Skipper | threshold | margin |
|---|---:|---:|---:|---:|
| `json/citm_catalog/parse_only/main` | 7597.992 | 12269.369 | 12270.369 | -4672.377 |
| `json/apache_builds/parse_only/main` | 10320.063 | 11993.874 | 11994.874 | -1674.811 |
| `json/update_center/parse_only/main` | 7542.431 | 12586.788 | 12587.788 | -5045.357 |
| `json/distinct_values/parse_only/main` | 5416.433 | 10801.523 | 10802.523 | -5386.090 |

## Disposition

Do not land the standalone end-offset number scanner. The current full
`NumberSpan` matcher stays canonical for generated JSON `parse_only` until a
replacement beats same-run strict evidence without regressing existing admits.

## Artifacts

- TSV:
  `restart/skinny/tranches/sk-v14/research/skv14-W10U-parse-only-number-end-reject.tsv`
  (`sha256=f494b5fa37f041a82f3e22ae291990c2c1e41bddebd12b654ab11159c851a859`)
- Raw log:
  `restart/skinny/tranches/sk-v14/research/skv14-W10U-parse-only-number-end-reject.raw.log`
  (`sha256=83094837d262f0a982cb753cb9ae85c8cc6311ab385074981555b98bcd27c32f`)
