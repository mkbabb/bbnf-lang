# SK-V6 Wave 1b R4b: String Distribution vs Retained Parse Deltas

Method: one-off byte lexer in `/tmp/skv6_string_distribution.py`, reading the 17 corpus paths from `skinny/crates/test-fixtures/corpus/json/manifest.toml`. Lengths are raw source bytes between quotes, because retained parse scans source string boundaries; `tiny` means the existing `match_tiny_plain_string` can succeed (raw length <= 7, no backslash/control before the close quote). `long64` is the population Candidate 2's 64-byte trusted special scanner should help most directly.

Candidate 2 correlation against full-gate Track 1 deltas: long64-byte share r=0.53; 32+ byte share r=0.68; tiny-count share r=0.08; string-density r=0.74; escaped-string share r=0.18.
Raw unescaped control bytes inside strings: 0 across all 17 corpora. Escape-heavy rows by escaped-string share: `y_string_unicode` 81.8% esc, `\u` 151.7 / KB, `unicode_escapes` 33.3% esc, `\u` 134.8 / KB, `unicode_mixed` 7.8% esc, `\u` 0.0 / KB, `gsoc-2018` 5.0% esc, `\u` 0.4 / KB, `twitter` 1.7% esc, `\u` 0.0 / KB, `update_center` 0.8% esc, `\u` 0.0 / KB.

| Corpus | Class | T1/S | Cand2 Δ | strings | str bytes/file | tiny ct% | tiny byte% | 32+ byte% | long64 byte% | esc str% | `\u`/KB | key byte% |
|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| `twitter` | medium/long-value mixed | 58.1% | -1.3% | 18099 | 58.5% | 29.4 | 6.2 | 46.6 | 36.9 | 1.7 | 0.0 | 45.3 |
| `citm_catalog` | key/mid-string dominated | 81.8% | -2.0% | 26604 | 12.8% | 47.1 | 32.0 | 4.2 | 0.0 | 0.0 | 0.0 | 92.6 |
| `canada` | structural / non-string | 129.3% | -9.8% | 12 | 0.0% | 58.3 | 40.0 | 0.0 | 0.0 | 0.0 | 0.0 | 58.9 |
| `apache_builds` | medium/long-value mixed | 70.7% | +14.6% | 5289 | 60.5% | 64.8 | 17.9 | 61.4 | 7.3 | 0.0 | 0.0 | 13.9 |
| `github_events` | medium/long-value mixed | 56.7% | +7.0% | 1891 | 70.5% | 39.9 | 7.1 | 74.5 | 42.7 | 0.3 | 0.0 | 17.2 |
| `update_center` | medium/long-value mixed | 47.5% | +4.8% | 27229 | 82.7% | 45.0 | 12.9 | 45.1 | 32.7 | 0.8 | 0.0 | 26.2 |
| `mesh` | structural / non-string | 113.0% | -1.4% | 11 | 0.0% | 45.5 | 33.7 | 0.0 | 0.0 | 0.0 | 0.0 | 100.0 |
| `random` | short-count dominated | 50.4% | +7.9% | 33005 | 65.4% | 58.3 | 25.0 | 3.9 | 0.0 | 0.0 | 0.0 | 27.2 |
| `gsoc-2018` | long-byte dominated | 44.9% | +15.4% | 34128 | 89.1% | 45.6 | 2.5 | 90.9 | 88.7 | 5.0 | 0.4 | 4.5 |
| `marine_ik` | short-count dominated | 128.5% | -2.9% | 38268 | 4.3% | 99.7 | 98.9 | 0.3 | 0.0 | 0.0 | 0.0 | 99.1 |
| `instruments` | key/mid-string dominated | 60.3% | -7.5% | 6889 | 31.7% | 39.8 | 15.3 | 2.9 | 0.0 | 0.0 | 0.0 | 98.6 |
| `numbers` | structural / non-string | 138.6% | +0.2% | 0 | 0.0% | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 |
| `unicode_mixed` | long-byte dominated | 55.6% | +8.9% | 25121 | 85.0% | 83.3 | 7.9 | 92.1 | 91.3 | 7.8 | 0.0 | 5.6 |
| `unicode_escapes` | long + escape dominated | 67.3% | +3.9% | 5636 | 96.5% | 66.7 | 0.6 | 99.4 | 99.4 | 33.3 | 134.8 | 0.6 |
| `unicode_basic` | long-byte dominated | 69.2% | +7.6% | 57590 | 70.4% | 78.0 | 25.7 | 65.9 | 54.6 | 0.0 | 0.0 | 14.8 |
| `distinct_values` | mid-value dominated | 34.2% | +4.5% | 9796 | 72.6% | 14.2 | 5.5 | 0.0 | 0.0 | 0.0 | 0.0 | 37.1 |
| `y_string_unicode` | long + escape dominated | 44.6% | +7.1% | 2200 | 81.5% | 18.2 | 7.6 | 45.5 | 45.5 | 81.8 | 151.7 | 0.0 |

Bucket details by raw source string length:

| Corpus | 0 byte% | 1-7 byte% | 8-15 byte% | 16-31 byte% | 32-63 byte% | 64-127 byte% | 128-255 byte% | 256+ byte% | key byte% |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| `twitter` | 0.0 | 6.2 | 18.8 | 28.5 | 9.7 | 12.6 | 7.9 | 16.4 | 45.3 |
| `citm_catalog` | 0.0 | 32.0 | 53.6 | 10.2 | 4.2 | 0.0 | 0.0 | 0.0 | 92.6 |
| `canada` | 0.0 | 40.0 | 41.1 | 18.9 | 0.0 | 0.0 | 0.0 | 0.0 | 58.9 |
| `apache_builds` | 0.0 | 17.9 | 5.8 | 15.0 | 54.0 | 6.7 | 0.0 | 0.6 | 13.9 |
| `github_events` | 0.0 | 7.1 | 11.5 | 6.9 | 31.8 | 14.6 | 16.1 | 12.1 | 17.2 |
| `update_center` | 0.0 | 12.9 | 19.6 | 22.5 | 12.4 | 23.3 | 6.7 | 2.6 | 26.2 |
| `mesh` | 0.0 | 33.7 | 66.3 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 100.0 |
| `random` | 0.0 | 25.0 | 22.7 | 48.4 | 3.9 | 0.0 | 0.0 | 0.0 | 27.2 |
| `gsoc-2018` | 0.0 | 2.5 | 2.3 | 4.3 | 2.1 | 7.5 | 1.1 | 80.1 | 4.5 |
| `marine_ik` | 0.0 | 98.9 | 0.7 | 0.1 | 0.3 | 0.0 | 0.0 | 0.0 | 99.1 |
| `instruments` | 0.0 | 15.3 | 49.1 | 32.7 | 2.9 | 0.0 | 0.0 | 0.0 | 98.6 |
| `numbers` | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 |
| `unicode_mixed` | 0.0 | 7.9 | 0.0 | 0.0 | 0.8 | 10.6 | 40.3 | 40.5 | 5.6 |
| `unicode_escapes` | 0.0 | 0.6 | 0.0 | 0.0 | 0.0 | 0.0 | 5.0 | 94.4 | 0.6 |
| `unicode_basic` | 0.0 | 25.7 | 7.8 | 0.6 | 11.3 | 38.6 | 16.0 | 0.0 | 14.8 |
| `distinct_values` | 0.0 | 5.5 | 47.1 | 47.4 | 0.0 | 0.0 | 0.0 | 0.0 | 37.1 |
| `y_string_unicode` | 0.0 | 21.4 | 33.1 | 0.0 | 0.0 | 45.5 | 0.0 | 0.0 | 0.0 |

Classification:

- Short-count or key/mid-string dominated: `citm_catalog`, `random`, `marine_ik`, `instruments`. These rows have little or no 64+ string-byte population, or most string bytes are object keys. Candidate 1 regressed because deleting the scalar early-out forced many small plain keys/strings through the larger trusted full-string matcher. Candidate 2 is neutral-to-bad when there are few value-side medium/long bytes to amortize (`marine_ik`, `instruments`, and the near-non-string `canada`/`mesh` shape).
- Medium/long-value mixed: `twitter`, `apache_builds`, `github_events`, `update_center`. These are not purely long-string rows, but 32+ byte value strings are material. This explains Candidate 2's strong `apache_builds` result (+14.6%) even with only 7.3% of bytes at 64+: most bytes sit in 32-63 byte values.
- Long-byte dominated: `gsoc-2018`, `unicode_mixed`, `unicode_basic`. These rows have most string bytes in 64+ byte bodies; Candidate 2 moved them because its 64-byte scanner reduced special-byte discovery cost. `gsoc-2018` is the clearest case (+15.4%).
- Long + escape dominated: `unicode_escapes`, `y_string_unicode`. These rows have long source strings, but escaped backslashes interrupt the pure quote/control scan. `unicode_escapes` is almost entirely source-escaped text; quote/backslash/control scanning helps less than the escape path, so Candidate 2 was only +3.9% despite huge string density.
- Mid-value dominated: `distinct_values`. These are string-populated but lack a large 64+ population; they need a smaller threshold or a different medium-string fast path before the long scanner matters.
- Structural/non-string: `canada`, `mesh`, `numbers`. String scanning should be gated away on this shape.

Interpretation against retained Track 1 gaps:

- The deepest SOTA misses (`distinct_values` 34.2% S, `y_string_unicode` 44.6%, `gsoc-2018` 44.9%, `update_center` 47.5%, `random` 50.4%) are all string-populated, but not one distribution. `distinct_values` is mid-value dominated with no 64+ population, `gsoc-2018` is long-byte dominated, `y_string_unicode` is long+escape dominated, and `random` is short/mid with raw non-ASCII content. A single always-on scanner cannot cover all four.
- Candidate 1 confirms the left side of the cost model: tiny early-out is load-bearing on rows with dense short keys/values, and still matters as a guard on long-byte rows because those corpora also contain many small keys/atoms. Removing it regressed every measured row.
- Candidate 2 confirms the right side: long64 share and especially 32+ value-byte share predict positive movement, but the full gate exposes fixed overhead or branch/i-cache cost on rows whose string density, value-byte share, or long-byte population is too small (`canada`, `instruments`, `marine_ik`, `citm_catalog`).

Admissible hybrid candidate:

Keep `match_tiny_plain_string` exactly on the retained parse path. Add a length/position-gated long scanner only after the tiny probe fails and only when there is evidence of a long body: either the current cursor has at least 64 bytes before file end and a cheap 16-byte precheck sees no quote/backslash/control, or the structural scan/tape cursor can prove the next quote is not in the first 16 bytes without adding retained sidecar state. The long scanner must remain trusted-UTF-8 only and must not add parse-time projection tables, second structural scans, or direct SinkOnly source-hook changes.

Falsifiability gate:

Run the full retained advisory gate plus focused `profile-lazy` on `gsoc-2018`, `unicode_mixed`, `apache_builds`, `random`, `distinct_values`, `y_string_unicode`, `twitter`, `canada`, `instruments`, and `marine_ik`. Pass requires: `gsoc-2018` >= +12%, `unicode_mixed` >= +8%, `apache_builds` >= +8%, and `random`/`y_string_unicode`/`distinct_values` each >= +3%; no row in the 17-corpus retained matrix may regress more than 2%, with `canada`, `instruments`, and `marine_ik` specifically bounded at <=1% regression. The candidate must expose a separate noinline attribution symbol for the long scanner or report c/B deltas, because Candidate 2 proved wrapper self-time is not a valid success metric.
