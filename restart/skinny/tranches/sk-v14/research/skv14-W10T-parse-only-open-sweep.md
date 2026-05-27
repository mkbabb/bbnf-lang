# SK-V14 W10T Parse-Only Open-Row Sweep

## Scope

W10T re-ran the remaining open JSON `parse_only` rows after the W10S source
implementation at `0aa35543c`. It is an evidence sweep, not a source patch:
Track 1 remains `runtime::generated_json::parse_only`, Track 2 remains the
independent structural oracle, and the strict comparator remains
`sonic_rs::Skipper`.

## Result

Only `json/instruments/parse_only/main` clears the admission threshold in the
fresh cold evidence:

| row | Track 1 | Track 2 | Skipper | threshold | margin |
|---|---:|---:|---:|---:|---:|
| `json/instruments/parse_only/main` | 4281.770 | 2748.324 | 3457.276 | 3458.276 | 823.494 |

The other eight rows remain open: `twitter`, `citm_catalog`, `apache_builds`,
`github_events`, `update_center`, `random`, `gsoc-2018`, and
`distinct_values`.

## Artifacts

- TSV:
  `restart/skinny/tranches/sk-v14/research/skv14-W10T-parse-only-open-sweep.tsv`
  (`sha256=864408ccd378266136c05cd2476286b1fb75e2c5a38f33e3aa17e5fb71a942a4`)
- Raw log:
  `restart/skinny/tranches/sk-v14/research/skv14-W10T-parse-only-open-sweep.raw.log`
  (`sha256=5b3d5d0c13add3b63f7102a61f2aafff223f3264fa845baaf2835276ae4c995a`)

Command shape:

```sh
./target/release/profile_direct 400 <corpus> <parse_only_mode> 0
```

where `<corpus>` is each remaining open parse-only corpus and
`<parse_only_mode>` is one of `parse_only_track1`, `parse_only_track2`,
`parse_only_sonic`, and `parse_only_serde`.
