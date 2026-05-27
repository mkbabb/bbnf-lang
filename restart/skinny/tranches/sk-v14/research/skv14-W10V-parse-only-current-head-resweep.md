# SK-V14 W10V JSON parse_only Current-HEAD Resweep

## Scope

W10V is an evidence-only continuation after REDRESS-221. It does not land a
source patch. The candidate source routes tested before the final resweep were
abrogated:

- object key/colon value-byte carry: rejected as REDRESS-84-adjacent and not
  reopened;
- constant-width literal matching: no open row admitted;
- container entry/classification cleanup: no open row admitted;
- parse-only string-tail scalar finish: A/B showed `citm_catalog` admits on
  current baseline, while the source patch risks string-heavy regressions.

## Evidence

Generated artifacts:

- `skv14-W10V-parse-only-current-head-resweep.raw.log`
  sha256 `2034752a28feae8f3cea8904d956bdae9ddae095e7d5658189b0e1581272579e`
- `skv14-W10V-parse-only-current-head-resweep.tsv`
  sha256 `579401dfa1a4f274debbebdd3187e7e33521c3342d8adb95e6c5e69e56c59757`

Cold command shape:

```sh
RUSTC_WRAPPER= RUSTFLAGS='-C target-cpu=native' \
  cargo build --release -p bbnf-bench --bin profile_direct

./target/release/profile_direct 400 <corpus> <parse_only_mode> 0
```

No warmup iterations were used.

## Admission

`json/citm_catalog/parse_only/main` is admitted by the W10V current-HEAD
resweep:

| corpus | Track 1 | Track 2 | Skipper | serde | threshold | margin |
|---|---:|---:|---:|---:|---:|---:|
| citm_catalog | 9079.838 | 13566.569 | 8335.772 | 5121.472 | 8336.772 | 743.066 |

The row uses generated Track 1 `runtime::generated_json::parse_only`, the
independent Track 2 structural oracle, strict `parse_only/sonic_rs::Skipper`,
and 400 cold iterations.

## Remaining Open

Seven parse_only rows remain open after W10V:

- `twitter`
- `apache_builds`
- `github_events`
- `update_center`
- `random`
- `gsoc-2018`
- `distinct_values`

Current parse_only state is 10 / 17 admitted and 7 / 17 open.
