# SK-V15 S-P0 A1 Measurement Integrity Audit

Date: 2026-05-27.
Axis: A1 Measurement integrity.
Scope: corpora size, repeated-number clusters, throughput plausibility, same-run comparator discipline, cold per-parse evidence, and broadcast admits.

## Verdict Table

| ID | Severity | Verdict | Evidence | Receiver |
|---|---|---|---|---|
| A1-001 | CRITICAL | CSS L4 has 24 ADMITTED rows backed by one identical measurement tuple, not 24 distinct measurements. | `restart/skinny/ROLLING-SOTA-DELTA.md:70` through `restart/skinny/ROLLING-SOTA-DELTA.md:93` repeat `2319.04 / 930.28 / 1388.76`; retained TSV repeats `2319.041 / 929.281 / 2362.037 / 930.281 / 1388.760` from `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:2` through `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:25`; `skinny/crates/bbnf-bench/src/css_l4_w8.rs:17` hardcodes 24 selected rows, while only seven `TRACK1_PROFILES` exist at `skinny/crates/bbnf-bench/src/css_l4_w8.rs:60`; `skinny/crates/bbnf-bench/src/css_l4_w8.rs:144` converts one aggregate admit boolean into `W8_SELECTED_CSS_ROWS`. | PRUNE-WAVE-A |
| A1-002 | HIGH | CSS SOTA admission comparator is not disciplined to the best same-run same-workload comparator: cssparser beats Track 1 in the retained run. | `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:2` records Track 1 `2319.041` Mbps and cssparser `2362.037` Mbps; `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:29` records the workload mismatch and notes cssparser wins by about 43 Mbps; the benchmark measures lightningcss and cssparser in the same aggregate function at `skinny/crates/bbnf-bench/src/css_l4_w8.rs:229` and `skinny/crates/bbnf-bench/src/css_l4_w8.rs:243`, but admission floor is lightningcss + 1 at `restart/skinny/ROLLING-SOTA-DELTA.md:97`. | PRUNE-WAVE-A, REBUILD-WAVE-E |
| A1-003 | HIGH | CSS cold evidence is aggregate-run evidence, not per-feature or per-row cold parse evidence. | The retained proof says `profile_iters=8 profiled_bytes=54859728` at `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-prototype.md:52`; the code computes that as `total_bytes(corpora) * TRACK1_PROFILES.len() * W8_PROFILE_ITERS` at `skinny/crates/bbnf-bench/src/css_l4_w8.rs:217`, then times one nested aggregate loop at `skinny/crates/bbnf-bench/src/css_l4_w8.rs:218`; the W8R proof still admits 24 rows at `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-prototype.md:63`. | PRUNE-WAVE-A |
| A1-004 | MEDIUM | Older retained JSON raw evidence has incomplete cold-field retention in two sources, though the ledger itself labels the rows cold. | `restart/skinny/tranches/sk-v14/research/skv14-W9-profile-direct.tsv:1` has no `warmup_iters` column while W9 typed rows cite it as cold in `skinny/RESULTS.md:7`; `restart/skinny/tranches/sk-v14/research/skv14-W11U-unicode-escapes-raw-lexeme-product.tsv:1` omits corpus/iters/warmup fields while `skinny/RESULTS.md:45` and `skinny/RESULTS.md:46` call the W11U rows cold. Newer retained TSVs do carry `warmup_iters`, for example `restart/skinny/tranches/sk-v14/research/skv14-W11A-direct-strict-product.tsv:1` and `restart/skinny/tranches/sk-v14/research/skv14-W11W-parse-only-memchr.tsv:1`. | JSON guard / telemetry binding |
| A1-005 | CLEAN | Corpus-size floor is satisfied for admitted JSON and CSS rows. | JSON corpus manifest sizes are all above 1 KB, with the smallest `y_string_unicode` at 35,601 bytes in `skinny/crates/test-fixtures/corpus/json/manifest.toml:99`; CSS corpus manifest totals 979,638 bytes at `skinny/corpora/css-l4-sk-v14/manifest.md:17`; CSS code enforces an 800 KiB minimum at `skinny/crates/bbnf-bench/src/css_l4_corpus.rs:3` and `skinny/crates/bbnf-bench/src/css_l4_w8.rs:95`. | None |
| A1-006 | CLEAN | JSON repeated-number and same-run discipline do not show a broadcast cluster. | JSON rows in `skinny/RESULTS.md:5` through `skinny/RESULTS.md:55` carry distinct Track 1/Track 2/SOTA tuples and every row signal reports cold Track 1 plus per-iteration equality PASS; the retained notes bind native Rust comparators as same-run strict anchors at `skinny/RESULTS.md:150` through `skinny/RESULTS.md:152`. | None |
| A1-007 | CLEAN | Absolute throughput numbers are physically plausible; the measurement-integrity failure is admission attribution, not impossible bandwidth. | Highest JSON Track 1 is 33,366.495 Mbps for citm direct at `skinny/RESULTS.md:9`, about 4.17 GB/s; CSS Track 1 is 2,319.041 Mbps at `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:2`, about 0.29 GB/s. Both are plausible on the recorded Apple M5 Max native host, cited for CSS in `skinny/RESULTS.md:112`. | None |

## Commands Run

| Command | Result | Evidence cited |
|---|---|---|
| `find skinny/corpora/css-l4-sk-v14 -maxdepth 1 -type f -name '*.css' -print0 \| xargs -0 wc -c` | CSS files total 979,638 bytes; individual files are 71,750 to 495,454 bytes. | `skinny/corpora/css-l4-sk-v14/manifest.md:12`, `skinny/corpora/css-l4-sk-v14/manifest.md:17` |
| `wc -c skinny/test_data/*.json \| sort -n` | Smallest retained JSON corpus is `y_string_unicode.json` at 35,601 bytes. | `skinny/crates/test-fixtures/corpus/json/manifest.toml:97`, `skinny/crates/test-fixtures/corpus/json/manifest.toml:99` |
| `awk -F'\t' 'NR==1 {next} {key=$2"/"$3"/"$4"/"$5"/"$6; count[key]++; rows[key]=rows[key] NR":"$1} END {for (k in count) if (count[k]>1) print count[k], k, rows[k]}' restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv` | One 24-row duplicate measurement tuple. | `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:2`, `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv:25` |
| `awk -F'\|' 'NR>=5 && NR<=55 {...}' skinny/RESULTS.md` | No duplicate JSON full numeric tuple and no duplicate JSON Track 1 value. | `skinny/RESULTS.md:5`, `skinny/RESULTS.md:55` |
| `rg -n "warmup_iters" restart/skinny/tranches/sk-v14/research/*.tsv skinny/RESULTS.md` | Most modern TSVs carry `warmup_iters`; W9 and W11U retained raw TSV headers do not. | `restart/skinny/tranches/sk-v14/research/skv14-W9-profile-direct.tsv:1`, `restart/skinny/tranches/sk-v14/research/skv14-W11U-unicode-escapes-raw-lexeme-product.tsv:1` |

## Measurement Notes

The CSS corpus is not too small. Four pinned production CSS files total 979,638 bytes, which exceeds both the A1 1 KB representative-corpus floor and the local 800 KiB CSS floor. The problem is row attribution: one aggregate loop over four corpora, seven profiles, and eight iterations is projected onto 24 conceptual CSS feature rows.

CSS same-run status is not enough to rescue the admission. The measurement function does run Track 1, lightningcss, and cssparser in the same source file and same release-native run, but the retained numbers show Track 1 loses to cssparser on the same full-parse probe. Using lightningcss + 1 Mbps as the floor admits the row family only because the workload is not equivalent to the same-workload cssparser probe.

JSON measurement integrity remains clean for A1. The 51 JSON rows use corpora far above 1 KB, have distinct row-level numbers in the result ledger, and cite same-run strict comparators and per-iteration equality. The only JSON issue found by this axis is retention quality: two older raw TSV formats do not independently encode `warmup_iters=0`, so the ledger's cold label is stronger than the raw artifact for those rows.

## Prune Receiver Routing

| Receiver | A1 routing |
|---|---|
| PRUNE-WAVE-A | Collapse CSS 24-row broadcast to one diagnostic aggregate unless each feature has an independently timed row; require `measurement_row_id` or equivalent to be unique per non-aggregate admit; stop using the W8R aggregate TSV as 24 admits. |
| REBUILD-WAVE-E | Re-time CSS after the typed Value/CSSOM path exists; compare against cssparser for same-workload near-term SOTA and only compare against lightningcss once Track 1 emits comparable CSSOM/value output. |
| JSON guard / telemetry binding | Preserve JSON rows as admitted, but require retained TSV evidence for future admits to carry explicit `iters`, `bytes` or `corpus_bytes`, and `warmup_iters=0` rather than relying on ledger prose. |

## Bottom Line

A1 rejects the CSS L4 24-row admission as measurement-invalid. The honest A1 state is 51 JSON rows clean, CSS corpus size clean, and CSS measurement demoted to one aggregate diagnostic row until PRUNE-WAVE-A or REBUILD-WAVE-E supplies independent row timings on a same-workload typed CSS output plane.
