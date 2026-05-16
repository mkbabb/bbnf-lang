# SK-V6 R1 parse-only attribution: regressed-from-PASS retained rows

Date: 2026-05-14
Workspace: `/Users/mkbabb/Programming/bbnf-lang`

## Scope and baseline

Authority read: `restart/skinny/tranches/sk-v6/DISPATCH-PROMPT.md` sections 1, 2, 3, and Wave 1; `restart/skinny/tranches/sk-v5/SYNTHESIS.md` post-assay header; `skinny/RESULTS.md`; latest SK-V6 section of `skinny/REDRESS.md`; and `restart/skinny/tranches/sk-v5/research/skv5-B1-parse-attribution.md`.

This profiles the NEW Track 1 retained parse baseline, not SK-V4. The profiled binary is `/tmp/skv6-cargo/R1/release/profile-lazy`, built from `skinny` with:

```sh
export CARGO_TARGET_DIR=/tmp/skv6-cargo/R1
cargo build --release -p xtask --bin profile-lazy --features runtime/parse-attribution
```

Profiles were recorded with `samply record --rate 4000 --main-thread-only --unstable-presymbolicate --save-only --no-open`. Most manifest-backed fixtures are not directly located by `profile-lazy`, so I passed `skinny/test_data/*.json` paths while keeping normalized profile filenames.

Scope note: current `skinny/RESULTS.md` marks `marine_ik` retained parse as A/GO, not regressed. It is still included because R1 explicitly listed it; treat it as a number/structural control row.

## Extraction method and confidence

The saved samply JSON files have `symbolicated=false`; `--unstable-presymbolicate` did not embed native symbol names. I extracted main-thread leaf frame addresses, mapped profile-lazy addresses to the nearest preceding `nm -n /tmp/skv6-cargo/R1/release/profile-lazy` symbol, demangled with `rustfilt`, and spot-checked representative addresses with `atos -arch arm64 -o ... 0x10000...`.

Shares below are approximate leaf self-sample percentages over the main thread. Parse/generated stacks account for 99.7-100.0% of samples on all rows. Inner `parse-that-regex` trusted string helpers are inlined into `runtime::generated_json::generated::match_string_at_quote`, so R1 can split the generated wrapper boundaries but not the inner trusted scanner PCs. Confidence is high for generated symbol boundaries, medium for sub-boundary string interpretation.

## Per-row results

| Row | Profile path | Iterations | profile-lazy Mbps | Dominant generated parse boundary | Approx share | Pathology classification | Change vs SK-V5 B1 attribution |
|---|---|---:|---:|---|---:|---|---|
| `citm_catalog` | `/tmp/skv6-R1-profiles/citm_catalog.profile.json.gz` | 4000 | 16321 | `consume_container_next` | 21.5% | structural/container churn with short-string and offset emission secondary | B1 `citm` was PASS and structural-heavy: tape reserve 34.8% + container next 17.2%. New generated runtime keeps the structural shape, but the cost is split across `consume_container_next`, `skip_ws`, `consume_structural`, `emit_plain_offset`, and short-string matching; it is no longer one fused `parse_value_at` PC region. |
| `apache_builds` | `/tmp/skv6-R1-profiles/apache_builds.profile.json.gz` | 30000 | 10692 | `match_tiny_plain_string` | 40.4% | short ASCII key/string scalar boundary; 68.2% total string boundary | No row-specific B1 entry. Unlike B1's original parse-G rows, there is no visible `validate_utf8_codepoint` boundary; generated runtime spends most samples in short string matching plus trusted full-string fallback. |
| `github_events` | `/tmp/skv6-R1-profiles/github_events.profile.json.gz` | 60000 | 10886 | `match_tiny_plain_string` / `match_string_at_quote` tie | 31.1% / 30.9% | mixed short-string and trusted-string scan boundary; 67.1% total string boundary | No row-specific B1 entry. The generated baseline shifts the hot cost to generated string wrappers, not B1's raw UTF-8 validator. |
| `update_center` | `/tmp/skv6-R1-profiles/update_center.profile.json.gz` | 7000 | 7793 | `match_tiny_plain_string` | 39.2% | string-heavy object/array row with short-string scalar fast path and full trusted fallback | No row-specific B1 entry. This is a generated string-wrapper row; numbers are absent and B1's `validate_utf8_codepoint` diagnosis does not transfer. |
| `gsoc-2018` | `/tmp/skv6-R1-profiles/gsoc-2018.profile.json.gz` | 2000 | 19804 | `match_string_at_quote` | 63.0% | long/trusted string delimiter scan; 84.8% total string boundary | No row-specific B1 entry. It resembles B1 string-heavy rows in that string scan dominates, but the changed runtime uses trusted UTF-8 string matching from `&str`; no separate raw UTF-8 validator symbol appears. |
| `instruments` | `/tmp/skv6-R1-profiles/instruments.profile.json.gz` | 20000 | 9225 | `match_tiny_plain_string` | 33.5% | mixed short-string, key/colon, container, and number row | No row-specific B1 entry. The hot shape is distributed generated runtime work: string 47.0%, structural/container 35.0%, number 9.9%; not a single B1 UTF-8-validator row. |
| `distinct_values` | `/tmp/skv6-R1-profiles/distinct_values.profile.json.gz` | 40000 | 5261 | `match_tiny_plain_string` | 55.9% | dense short-string scalar boundary; 81.3% total string boundary | No row-specific B1 entry. The dominant boundary is the existing generated tiny-string scalar path; this is attribution only and does not reopen Class A retained NEON wiring. |
| `y_string_unicode` | `/tmp/skv6-R1-profiles/y_string_unicode.profile.json.gz` | 100000 | 5438 | `match_string_at_quote` | 62.6% | long Unicode string trusted delimiter scan plus flag/offset state | Closest to B1 Unicode rows, but materially changed: B1 attributed 38-40% to `validate_utf8_codepoint`; generated runtime calls the trusted UTF-8 string matcher, so raw validation is gone from the hot symbol path and the cost is inside `match_string_at_quote`. |
| `marine_ik` | `/tmp/skv6-R1-profiles/marine_ik.profile.json.gz` | 2000 | 9266 | `match_number_at_digit` | 33.5% | number scan plus container/dispatch; retained parse control row | B1 `marine_ik` was PASS and number-bound at 27.4%. New generated runtime reconfirms number dominance, with structural/container cost now nearly co-equal (`consume_container_next` 23.3%, `dispatch_value` 10.1%, `parse_number` 8.0%). This row remains GO in current results. |

## Cluster summary

String-wrapper cluster: `apache_builds`, `github_events`, `update_center`, `gsoc-2018`, `distinct_values`, `y_string_unicode`, and the largest part of `instruments`. The dominant generated boundaries are `match_tiny_plain_string` for short-string rows and `match_string_at_quote` for longer or Unicode string rows. This is the major change from B1: generated retained parse now enters `match_json_string_at_quote_trusted_utf8` because the input is already `&str`, so the B1 `validate_utf8_codepoint` hot boundary is not present in these profiles.

Structural/container cluster: `citm_catalog`, with a structural share of 57.0%. B1 already classified `citm` as structural-heavy, but the generated runtime exposes smaller boundary costs instead of one fused `parse_value_at` PC bucket: `consume_container_next`, `skip_ws`, `consume_structural`, `emit_plain_offset`, and key string matching.

Number/container control cluster: `marine_ik`, with number 41.5% and structural/container 42.8%. B1's number-bound reading is reconfirmed; this row is not evidence for the string regression cluster.

## Candidate hypotheses pending synthesis

One candidate for the string-wrapper cluster, pending SK-V6 synthesis and same-row falsification: investigate a generated retained trusted-string boundary that reduces wrapper/state churn around quote consumption, short-string result handling, full trusted string scan, offset emit, and flag patching. This is not a proposal to revive Class A retained NEON tiny-string wiring, not a UTF-8 fusion prescription, and not REDRESS 54/55 decoded/quote-source materialization.

No R1 candidate for the structural/container or number/container clusters. The obvious routes are pre-blocked or already falsified for this dispatch: retained side tables, EventCursor/sidecar prepasses, capacity prescan, and generic SWAR whitespace. `citm_catalog` and `marine_ik` need the R4/R6 diff and PMU evidence before a kernel candidate is credible.

## Blocked routes not reopened

This report does not re-propose REDRESS 50, 51, 53, 54, 55, Class A retained wiring, sidecar prepasses, EventCursor, capacity prescan, or generic SWAR whitespace. It also does not transfer the SK-V5 B1 UTF-8 fusion prescription to the generated runtime baseline.
