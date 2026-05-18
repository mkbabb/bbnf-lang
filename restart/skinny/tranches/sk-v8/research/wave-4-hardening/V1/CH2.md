# SK-V8 W4 Hardening V1 CH2

Verdict: ACCEPT.

Confidence: 97%.

## Findings

- W4 floor math matches `skinny/RESULTS.md` using
  `ceil(sonic-rs strict Mbps / 1.10)`.
- `apache_builds`: `8852 / 1.10 = 8047.27`, floor `8048`; Track 1 `8306`
  clears, Track 2 `7796` misses by `+3.2%`.
- `numbers`: `7953 / 1.10 = 7230`, floor `7230`; Track 1 `9773` clears,
  Track 2 `6966` misses by `+3.8%`.
- `random`: `8141 / 1.10 = 7400.91`, floor `7401`; Track 1 `7751` clears,
  Track 2 `6952` misses by `+6.5%`.
- Selected rows are valid `N-direct / NO-GO` Track2-only misses. No selected
  row should be removed on numeric grounds.
- Additional Track2-only misses exist but should not be added under the <=3 row
  W4 cap: `twitter` needs +18.6%, and `github_events` needs +21.7%.
- Rows that fail Track 1 as well remain correctly excluded: `canada`,
  `update_center`, `mesh`, `gsoc-2018`, `instruments`, `unicode_mixed`,
  `unicode_escapes`, `distinct_values`, and `y_string_unicode`.

## Required Folds

None required. Optional cleanup: spell the threshold as
`ceil(sonic-rs strict Mbps / 1.10)` in W4 research/plan prose; the selected
table already uses the correct ceiling values.
