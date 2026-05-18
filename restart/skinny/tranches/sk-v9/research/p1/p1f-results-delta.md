# SK-V9 P1-F: RESULTS Extraction And Delta

Pass: S-P1 Profile. Cycle: V2 post-W0 rerun.
Date: 2026-05-18.
Scope: extract SK-V9-open `skinny/RESULTS.md` rows and delta them against the
prior SK-V8-open RESULTS snapshot.
Output: this file.
Baseline: SK-V9-open at commit `90609aee`, run
`sk-v9-open:criterion-fnv64-cd1673844eeea12f`.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
Build flags: `profile=bench;rustflags=-C target-cpu=native;target_cpu=native`.
Profile tool: row extraction, not samply.
Corpus coverage: 38/38 manifest rows.

## §1 - Method

Commands:

```bash
git status --short
git log --oneline -- skinny/RESULTS.md
git show HEAD^:skinny/RESULTS.md
python3 <row extractor> skinny/RESULTS.md HEAD^:skinny/RESULTS.md
```

The extractor parses the main RESULTS table and the
`## SK-V9 W0 Telemetry Manifest` table, then joins by row id.

## §2 - Findings

Manifest:

- Rows: 38.
- Run id: `sk-v9-open:criterion-fnv64-cd1673844eeea12f`.
- Families: 17 `parse_only`, 17 `direct_to_struct`, 4 `real_typed_struct`.
- Classes: 17 `parse_only S/NO-GO`, 3 `direct_to_struct A/GO`, 14
  `direct_to_struct N-direct/NO-GO`, 4 `real_typed_struct A/GO`.
- Diagnostic fence on every manifest row:
  `structural_scan+masking_probes+pmu+cycles:nonproducer`.

## §3 - Delta vs SK-V8

| Row | Class | Track 1 | Prior Track 1 | Delta | Prior Class |
|---|---|---:|---:|---:|---|
| `json/twitter/parse_only/main` | `S/NO-GO` | 13188 | 9581 | +37.6% | `S/NO-GO` |
| `json/twitter/direct_to_struct/main` | `N-direct/NO-GO` | 11166 | 11859 | -5.8% | `N-direct/NO-GO` |
| `json/twitter/real_typed_struct/main` | `A/GO` | 14761 | 15333 | -3.7% | `A/GO` |
| `json/citm_catalog/parse_only/main` | `S/NO-GO` | 29215 | 28644 | +2.0% | `S/NO-GO` |
| `json/citm_catalog/direct_to_struct/main` | `A/GO` | 20229 | 21151 | -4.4% | `A/GO` |
| `json/canada/parse_only/main` | `S/NO-GO` | 16190 | 15497 | +4.5% | `L/NO-GO` |
| `json/canada/direct_to_struct/main` | `N-direct/NO-GO` | 9475 | 6586 | +43.9% | `N-direct/NO-GO` |
| `json/apache_builds/parse_only/main` | `S/NO-GO` | 11917 | 12694 | -6.1% | `S/NO-GO` |
| `json/apache_builds/direct_to_struct/main` | `N-direct/NO-GO` | 10577 | 8306 | +27.3% | `N-direct/NO-GO` |
| `json/github_events/parse_only/main` | `S/NO-GO` | 14302 | 10689 | +33.8% | `S/NO-GO` |
| `json/github_events/direct_to_struct/main` | `N-direct/NO-GO` | 11430 | 9088 | +25.8% | `N-direct/NO-GO` |
| `json/update_center/parse_only/main` | `S/NO-GO` | 9857 | 11926 | -17.3% | `S/NO-GO` |
| `json/update_center/direct_to_struct/main` | `N-direct/NO-GO` | 7245 | 7863 | -7.9% | `N-direct/NO-GO` |
| `json/update_center/real_typed_struct/main` | `A/GO` | 11345 | 11958 | -5.1% | `A/GO` |
| `json/mesh/parse_only/main` | `S/NO-GO` | 12435 | 9367 | +32.8% | `S/NO-GO` |
| `json/mesh/direct_to_struct/main` | `N-direct/NO-GO` | 8489 | 8640 | -1.7% | `N-direct/NO-GO` |
| `json/mesh/real_typed_struct/main` | `A/GO` | 8919 | 9623 | -7.3% | `A/GO` |
| `json/random/parse_only/main` | `S/NO-GO` | 9382 | 10011 | -6.3% | `S/NO-GO` |
| `json/random/direct_to_struct/main` | `N-direct/NO-GO` | 7590 | 7751 | -2.1% | `N-direct/NO-GO` |
| `json/gsoc-2018/parse_only/main` | `S/NO-GO` | 22184 | 23209 | -4.4% | `S/NO-GO` |
| `json/gsoc-2018/direct_to_struct/main` | `N-direct/NO-GO` | 14362 | 15042 | -4.5% | `N-direct/NO-GO` |
| `json/marine_ik/parse_only/main` | `S/NO-GO` | 12073 | 13100 | -7.8% | `S/NO-GO` |
| `json/marine_ik/direct_to_struct/main` | `A/GO` | 8696 | 9357 | -7.1% | `A/GO` |
| `json/marine_ik/real_typed_struct/main` | `A/GO` | 11259 | 11783 | -4.4% | `A/GO` |
| `json/instruments/parse_only/main` | `S/NO-GO` | 16189 | 13320 | +21.5% | `S/NO-GO` |
| `json/instruments/direct_to_struct/main` | `N-direct/NO-GO` | 11327 | 8494 | +33.4% | `N-direct/NO-GO` |
| `json/numbers/parse_only/main` | `S/NO-GO` | 17956 | 12818 | +40.1% | `S/NO-GO` |
| `json/numbers/direct_to_struct/main` | `N-direct/NO-GO` | 12177 | 9773 | +24.6% | `N-direct/NO-GO` |
| `json/unicode_mixed/parse_only/main` | `S/NO-GO` | 6803 | 6390 | +6.5% | `S/NO-GO` |
| `json/unicode_mixed/direct_to_struct/main` | `N-direct/NO-GO` | 4215 | 3596 | +17.2% | `N-direct/NO-GO` |
| `json/unicode_escapes/parse_only/main` | `S/NO-GO` | 12047 | 12731 | -5.4% | `S/NO-GO` |
| `json/unicode_escapes/direct_to_struct/main` | `N-direct/NO-GO` | 4821 | 4020 | +19.9% | `N-direct/NO-GO` |
| `json/unicode_basic/parse_only/main` | `S/NO-GO` | 11348 | 11189 | +1.4% | `S/NO-GO` |
| `json/unicode_basic/direct_to_struct/main` | `A/GO` | 8179 | 9363 | -12.6% | `A/GO` |
| `json/distinct_values/parse_only/main` | `S/NO-GO` | 8972 | 10279 | -12.7% | `S/NO-GO` |
| `json/distinct_values/direct_to_struct/main` | `N-direct/NO-GO` | 5761 | 4438 | +29.8% | `N-direct/NO-GO` |
| `json/y_string_unicode/parse_only/main` | `S/NO-GO` | 5428 | 5577 | -2.7% | `S/NO-GO` |
| `json/y_string_unicode/direct_to_struct/main` | `N-direct/NO-GO` | 4583 | 4828 | -5.1% | `N-direct/NO-GO` |

## §4 - Anomalies + Masking Signals

- The only class movement is `canada/parse_only`: `L/NO-GO` to `S/NO-GO`.
  It remains diagnostic `NO-GO`, not an admission.
- `apache_builds`, `instruments`, and `numbers` direct rows keep
  `N-direct/NO-GO` under the W0 no-admission clamp despite pass-shaped fresh
  numbers.
- No Apache/CITM/Canada measured `real_typed_struct` rows exist.
- No parse-only row is admissible under deferred/view-boundary strictness.

## §5 - Sources

- `skinny/RESULTS.md`
- `HEAD^:skinny/RESULTS.md`
- `/tmp/skv9-p1-rerun/p1f-delta.md`
