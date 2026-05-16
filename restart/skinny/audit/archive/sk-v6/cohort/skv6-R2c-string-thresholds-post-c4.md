# SK-V6 Wave 1c R2c: Medium/Long String Distribution and Threshold Plan Post-Candidate4

Scope: retained parse string-distribution analysis after Candidate4 (`ContainerNext`) landed. This report does not edit repo files. It reads `skinny/REDRESS.md` entries 60-63, `restart/skinny/audit/SK-V6-COHORT/skv6-R4b-string-distribution.md`, and current `skinny/RESULTS.md`, then recomputes corpus string buckets directly from the manifest paths in `skinny/crates/test-fixtures/corpus/json/manifest.toml`.

Hard cap: 30 minutes. Report path: `/tmp/skv6-R2c-string-thresholds-post-c4.md`.

## Inputs and Method

- Manifest authority: `skinny/crates/test-fixtures/corpus/json/manifest.toml`.
- String lexer: one-pass byte scanner over JSON source strings. Lengths are raw source bytes between quotes, which is the retained parse scanner's relevant unit.
- Key/value split: cheap lexical heuristic, classifying a string as a key when the next non-whitespace byte after the closing quote is `:`.
- Escape metrics: `escaped str%` is the percentage of string literals containing at least one backslash; `slash/KB` is backslash bytes per input KiB; `u/KB` is `\u` or `\U` escape introducers per input KiB.
- Candidate deltas:
  - Candidate2: REDRESS 61 full advisory deltas for the always-wide trusted 64-byte scanner.
  - Candidate3: REDRESS 62 smoke deltas for the delayed-wide scanner.
  - Candidate4: REDRESS 63 smoke deltas for `ContainerNext` / next-byte carry.

## Pre-Blocked Routes

- REDRESS 60 blocks deleting retained `match_tiny_plain_string`. It regressed every measured row and is load-bearing for dense short strings and keys.
- REDRESS 61 blocks the always-wide retained trusted scanner as shipped. It helped long/value-string rows but failed the full matrix through non-string and key-heavy regressions.
- REDRESS 62 blocks the delayed-wide retained trusted scanner. It preserved the tiny probe and deferred the 64-byte scan, yet still regressed almost every sentinel row.
- REDRESS 63 admits `ContainerNext`, but it is parser-control recovery, not a retained string scanner. Its result must not be used as evidence for another medium/long string threshold.

## Current Distribution

Bucket columns are byte-share of all raw string bytes in that corpus.

| Corpus | strings | string bytes/file | <=7 byte% | 8-15 byte% | 16-31 byte% | 32-63 byte% | 64+ byte% | escaped str% | slash/KB | u/KB | key byte% | Cand2 Δ | Cand3 Δ | Cand4 Δ |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| `twitter` | 18099 | 58.5% | 6.2 | 18.8 | 28.5 | 9.7 | 36.9 | 1.7 | 2.0 | 0.0 | 45.3 | -1.3% | -7.46% | +0.76% |
| `citm_catalog` | 26604 | 12.8% | 32.0 | 53.6 | 10.2 | 4.2 | 0.0 | 0.0 | 0.0 | 0.0 | 92.6 | -2.0% | -1.23% | +6.06% |
| `canada` | 12 | 0.0% | 40.0 | 41.1 | 18.9 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 58.9 | -9.8% | -1.92% | +10.66% |
| `apache_builds` | 5289 | 60.5% | 17.9 | 5.8 | 15.0 | 54.0 | 7.3 | 0.0 | 0.2 | 0.0 | 13.9 | +14.6% | -2.53% | +1.62% |
| `github_events` | 1891 | 70.5% | 7.1 | 11.5 | 6.9 | 31.8 | 42.7 | 0.3 | 2.4 | 0.0 | 17.2 | +7.0% | -4.29% | +1.52% |
| `update_center` | 27229 | 82.7% | 12.9 | 19.6 | 22.5 | 12.4 | 32.7 | 0.8 | 0.5 | 0.0 | 26.2 | +4.8% | -5.28% | +1.10% |
| `mesh` | 11 | 0.0% | 33.7 | 66.3 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 100.0 | -1.4% | -1.84% | +7.53% |
| `random` | 33005 | 65.4% | 25.0 | 22.7 | 48.4 | 3.9 | 0.0 | 0.0 | 0.0 | 0.0 | 27.2 | +7.9% | -4.09% | +6.12% |
| `gsoc-2018` | 34128 | 89.1% | 2.5 | 2.3 | 4.3 | 2.1 | 88.7 | 5.0 | 4.6 | 0.4 | 4.5 | +15.4% | -3.47% | +0.50% |
| `marine_ik` | 38268 | 4.3% | 98.9 | 0.7 | 0.1 | 0.3 | 0.0 | 0.0 | 0.0 | 0.0 | 99.1 | -2.9% | -2.67% | +10.69% |
| `instruments` | 6889 | 31.7% | 15.3 | 49.1 | 32.7 | 2.9 | 0.0 | 0.0 | 0.0 | 0.0 | 98.6 | -7.5% | -4.49% | +5.22% |
| `numbers` | 0 | 0.0% | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | +0.2% | +0.19% | +10.33% |
| `unicode_mixed` | 25121 | 85.0% | 7.9 | 0.0 | 0.0 | 0.8 | 91.3 | 7.8 | 52.2 | 0.0 | 5.6 | +8.9% | +5.02% | +10.72% |
| `unicode_escapes` | 5636 | 96.5% | 0.6 | 0.0 | 0.0 | 0.0 | 99.4 | 33.3 | 217.2 | 133.2 | 0.6 | +3.9% | -0.47% | +8.40% |
| `unicode_basic` | 57590 | 70.4% | 25.7 | 7.8 | 0.6 | 11.3 | 54.6 | 0.0 | 0.0 | 0.0 | 14.8 | +7.6% | -1.15% | +2.79% |
| `distinct_values` | 9796 | 72.6% | 5.5 | 47.1 | 47.4 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 37.1 | +4.5% | -8.49% | +1.28% |
| `y_string_unicode` | 2200 | 81.5% | 21.4 | 33.1 | 0.0 | 0.0 | 45.5 | 81.8 | 132.3 | 126.6 | 0.0 | +7.1% | -0.96% | +3.18% |

# correlations
| Candidate | Feature | r |
|---|---|---:|
| Candidate2 always-wide | `string_density` | 0.74 |
| Candidate2 always-wide | `key_byte_share` | -0.72 |
| Candidate2 always-wide | `value_byte_share` | 0.72 |
| Candidate2 always-wide | `32+_byte_share` | 0.68 |
| Candidate2 always-wide | `64+_byte_share` | 0.53 |
| Candidate2 always-wide | `8-15_byte_share` | -0.52 |
| Candidate2 always-wide | `32-63_byte_share` | 0.46 |
| Candidate2 always-wide | `<=7_byte_share` | -0.39 |
| Candidate3 delayed-wide | `16-31_byte_share` | -0.71 |
| Candidate3 delayed-wide | `<=7_count_share` | 0.45 |
| Candidate3 delayed-wide | `64+_byte_share` | 0.38 |
| Candidate3 delayed-wide | `64+_count_share` | 0.38 |
| Candidate3 delayed-wide | `escape_per_kb` | 0.36 |
| Candidate3 delayed-wide | `8-15_byte_share` | -0.30 |
| Candidate3 delayed-wide | `32+_byte_share` | 0.30 |
| Candidate3 delayed-wide | `escaped_string_share` | 0.25 |
| Candidate4 ContainerNext smoke | `string_density` | -0.56 |
| Candidate4 ContainerNext smoke | `32-63_byte_share` | -0.48 |
| Candidate4 ContainerNext smoke | `<=7_byte_share` | 0.42 |
| Candidate4 ContainerNext smoke | `16-31_byte_share` | -0.33 |
| Candidate4 ContainerNext smoke | `<=7_count_share` | 0.31 |
| Candidate4 ContainerNext smoke | `32+_byte_share` | -0.31 |
| Candidate4 ContainerNext smoke | `key_byte_share` | 0.26 |
| Candidate4 ContainerNext smoke | `value_byte_share` | -0.26 |

## Interpretation

Candidate2 confirmed the old R4b right-side signal but not an admissible route. Its strongest correlations are string density (r=0.74), value-byte share (r=0.72), and 32+ byte share (r=0.68), while key-byte share is strongly negative (r=-0.72). That explains why `apache_builds`, `gsoc-2018`, `unicode_mixed`, and `unicode_basic` moved, and why key-heavy or non-string rows (`citm_catalog`, `canada`, `marine_ik`, `instruments`) broke the gate.

Candidate3 falsifies the obvious threshold repair. It kept the tiny fast path and delayed the wide scanner, but its correlations are weak or backwards for an implementation candidate: 16-31 byte share is strongly negative (r=-0.71), 64+ byte share is only moderately positive (r=0.38), and the measured row deltas were mostly regressions. That means a retained parse threshold like "use the wide scanner after the first local block" is not enough; the overhead lands before the rows with mixed string distributions can amortize it.

Candidate4 is the control-flow counterexample. It improves `canada`, `marine_ik`, `numbers`, `mesh`, and `unicode_mixed` while being negatively correlated with string density (r=-0.56) and 32+ byte share (r=-0.31). Its acceptance proves the remaining parse recovery can come from generated parser cadence, but it does not identify a string-length threshold.

The row taxonomy after Candidate4 is therefore split:

| Class | Rows | Threshold implication |
|---|---|---|
| Long/value dominated | `gsoc-2018`, `unicode_mixed`, `unicode_basic`, `github_events`, parts of `update_center` | Candidate2 showed possible upside, but Candidate3 showed the retained threshold shape is still too expensive. |
| Long + escape dominated | `unicode_escapes`, `y_string_unicode` | Quote/backslash discovery is not the only cost; escape handling interrupts plain-span scan. A raw-length threshold over-predicts impact. |
| Mid-value dominated | `distinct_values`, `random`, parts of `apache_builds` | A 64+ threshold misses the row; a 32+ threshold would fire broadly but has no successful measurement and risks Candidate3's 16-31/32-63 overhead class. |
| Key-heavy / short-heavy | `citm_catalog`, `marine_ik`, `instruments` | Tiny fast path remains mandatory; wide scan should not fire. |
| Non-string / structural | `canada`, `mesh`, `numbers` | Any retained string scanner work should be gated away; Candidate4, not string scanning, moved these rows. |

## Threshold Decision

No defensible retained string scanner threshold remains after Candidate4.

The tempting threshold family is `raw_len >= 64` or `raw_len >= 32`, gated after `match_tiny_plain_string`. Both are non-canonical on current evidence:

- `raw_len >= 64` was effectively tested by Candidate2 and Candidate3. The always-wide shape had row-local wins but failed the full matrix; the delayed-wide shape still regressed the matrix.
- `raw_len >= 32` is not cheaply knowable before scanning the string unless the parser adds retained projection side state, a second quote finder, or a corpus-trained predictor. Those are blocked by Lock 1 / Lock 14 and REDRESS 50, 53, 60-62.
- Corpus-level distribution is not an admissible gate. It would special-case benchmark rows rather than lower a grammar-neutral primitive from local parser facts.
- Escape-heavy rows prove raw length alone is insufficient: `unicode_escapes` and `y_string_unicode` have large source strings, but backslash handling dominates enough that plain-span quote discovery does not close the row.

The only admissible statement is a negative gate: keep `match_tiny_plain_string`, do not add another retained parse string scanner threshold unless a future profile names a new local fact that is available without rescanning or retained side state. Examples of such a fact would have to be same-pass and grammar-neutral, not corpus metadata: a codegen-known fixed token length, a structural event already consumed by the canonical substrate, or a direct-to-struct field layout that removes retained string materialization from the path. None exists in the current retained parser.

## Recommendation for Wave 2 Planning

Do not spend the next Wave 2 on a retained string scanner threshold. The string distributions remain useful for falsifying future candidates, but the post-Candidate4 evidence says the threshold class is exhausted for retained parse.

The next plan should choose a different profile-backed surface:

- retained parser-control / offset emission if R1c or R3c names a non-string boundary with row impact; or
- Wave 3 direct-to-struct field-layout materializer if direct profiles show the default allocate-then-contiguous-hash baseline is now the larger, admissible close.

If a future agent insists on a string route, its falsifiability gate must be harsher than R4b's pre-Candidate4 gate: it must improve `gsoc-2018`, `unicode_mixed`, `apache_builds`, `distinct_values`, and `y_string_unicode` together, while bounding `canada`, `citm_catalog`, `marine_ik`, and `instruments` at <=1% regression. Based on REDRESS 60-62, no current threshold proposal meets that bar.
