# SK-V13 P1-D: PMU + Cycles-Per-Byte

Pass: S-P1 Profile. Cycle: V13 / S-P1 V2 fold.
Date: 2026-05-21.
Scope: PMU/proc counters, xctrace export availability, and derived cycles-per-byte for JSON parse/direct/typed plus mode-III probes.
Output: this file.
Baseline: SK-V13-open (`7ee299096be7d7fdaa0e69344a6cd18bbd55524f`; no behavior-source delta from the V1 profile binary).
Host triple: aarch64-apple-darwin.
Build flags: release profile, `debug=true`, `RUSTFLAGS="-C target-cpu=native"`.
Profile tool: V1 `/tmp/skv13-p1/pmu/pmu_rows.tsv`, V2 direct logs, V2 mode-III `PROBE_RESULT` rows, xctrace export check.
Corpus coverage: parse 17/17, direct 17/17, typed 7/7 existing typed rows, mode III 17/17 x 5 probes.

V3 fold note: counter availability is canonicalized in
`support/evidence-ledger-v3.md`; branch/L1/LLC remain
`unavailable_from_current_export`.

## §1 - Method (commands run; verbatim, reproducible)

V1 PMU authority retained where no source changed:

```bash
cat /tmp/skv13-p1/artifacts/identity.txt
bash /tmp/skv13-p1/pmu/run-pmu.sh
awk -F '\t' 'NR>1{n++; bad+=($4!=0)} END{print n,bad+0}' \
  /tmp/skv13-p1/pmu/pmu_rows.tsv
# 130 0
```

V2 direct/mode-III counter authorities:

```bash
grep '^PROBE_RESULT' /tmp/skv13-p1-v2/samply/logs/direct__twitter__track1.log
awk -F '\t' 'NR>1{n++; bad+=($4!=0)} END{print n,bad+0}' \
  /tmp/skv13-p1-v2/mode3/mode3_rows.tsv
# 85 0
```

xctrace export probe:

```bash
mkdir -p /tmp/skv13-p1-v2/xctrace-export
xcrun xctrace export \
  --input /tmp/skv13-p1/xctrace/traces/cpu_counters__parse__random__track1.trace \
  --toc
xcrun xctrace export \
  --input /tmp/skv13-p1/xctrace/traces/cpu_counters__parse__random__track1.trace \
  --xpath '/trace-toc/run[@number="1"]/data/table[@schema="cpu-state"]' \
  --output /tmp/skv13-p1-v2/xctrace-export/cpu-state.xml
```

The `cpu-state.xml` export exists and is about 16 MiB. Grepping exported tables
for `branch|miss|l1|llc|counter|cycle|instruction|cache` found trace names and
kdebug event labels, but no usable per-row numeric branch-miss, L1-miss, or
LLC-miss columns. Those fields are therefore `unavailable_from_current_export`,
not zero.

## §2 - Findings (per-corpus PMU tables)

### §2.1 Coverage Summary

| Surface | Rows | Bad rc | Counter fields |
|---|---:|---:|---|
| V1 PMU `parse` | 34 | 0 | Mbps, cycles, instructions, c/B, CPI, checksum |
| V1 PMU `direct` | 68 | 0 | Track 1/Track 2/sonic/serde, same fields |
| V1 PMU `typed` | 28 | 0 | seven generated typed rows x four modes |
| V2 direct logs | 34 | 0 | Mbps, cycles, instructions, c/B, CPI |
| V2 mode III | 85 | 0 | Mbps, cycles, instructions, c/B, CPI |
| xctrace CPU Counters/Time Profiler | 164 trace statuses | 0 | trace bundles; branch/L1/LLC not exported as tabular row fields |

### §2.2 Direct Product Plane, V2 Samply-Log Counters

These values come from the non-panic V2 direct logs and are the current
symbol-aligned direct counter rows.

| Corpus | Track 1 Mbps | Track 1 c/B | Track 2 Mbps | Track 2 c/B | Counter note |
|---|---:|---:|---:|---:|---|
| twitter | 11821.161 | 2.969 | 10841.589 | 3.224 | generated direct envelope dominates Track 1 |
| citm_catalog | 21968.958 | 1.605 | 20806.401 | 1.694 | generated direct envelope dominates Track 1 |
| canada | 10547.205 | 3.262 | 10148.055 | 3.332 | array direct envelope dominates |
| apache_builds | 11071.291 | 3.081 | 10128.990 | 3.355 | object direct envelope dominates |
| github_events | 11885.718 | 2.839 | 11062.206 | 3.085 | object direct envelope dominates |
| update_center | 8206.081 | 4.140 | 7334.045 | 4.622 | object direct envelope dominates |
| mesh | 8786.959 | 3.865 | 8063.295 | 4.205 | array direct envelope dominates |
| random | 7661.152 | 4.425 | 6839.907 | 4.957 | object direct envelope dominates |
| gsoc-2018 | 14522.580 | 2.337 | 13954.747 | 2.432 | object direct envelope dominates |
| marine_ik | 9241.327 | 3.673 | 9224.560 | 3.663 | Track 1/Track 2 cB effectively tied |
| instruments | 11738.320 | 2.882 | 10895.383 | 3.112 | Option leaf visible in Track 1 rank 1 |
| numbers | 12216.215 | 2.777 | 11950.227 | 2.832 | array direct envelope dominates |
| unicode_mixed | 4422.918 | 7.667 | 4283.724 | 7.878 | unicode-heavy direct cost |
| unicode_escapes | 4771.925 | 7.074 | 4259.928 | 7.578 | `unescape_string` rank-1 |
| unicode_basic | 8858.170 | 3.817 | 8043.084 | 4.209 | string-heavy direct cost |
| distinct_values | 6097.397 | 5.559 | 5458.584 | 6.208 | array-element direct envelope at generated.rs:542 |
| y_string_unicode | 3101.039 | 10.942 | 2975.830 | 11.408 | worst direct c/B in V2 |

### §2.3 Mode-III Counter Outliers

| Probe family | Lowest Mbps row | Highest Mbps row | Load-bearing c/B signal |
|---|---|---|---|
| `host_call_eager_decode` | `y_string_unicode` 1181.961 Mbps / 23.152 c/B | `numbers` 7191.784 Mbps / 3.539 c/B | host-call/eager decode is most expensive on unicode/string rows |
| `alternate_scalar_plan` | `random` 1513.999 Mbps / 18.590 c/B | `gsoc-2018` 7153.570 Mbps / 3.842 c/B | scalar alternate beats eager on some string/unicode rows, but not all |
| `cold_first_parse` | `y_string_unicode` 4484.753 Mbps / 6.006 c/B | `gsoc-2018` 19729.022 Mbps / 1.642 c/B | cold-first remains much faster than eager probe for every row |
| structural scalar | `citm_catalog` 6406.625 Mbps / 4.309 c/B | `unicode_escapes` 13182.510 Mbps / 2.524 c/B | scalar scan is dominated by `scan_tail` |
| structural SIMD | `random` 10465.696 Mbps / 2.688 c/B | `canada` 36406.064 Mbps / 0.876 c/B | SIMD scan beats scalar scan on all 17 rows |

## §3 - Delta vs SK-V12 and JSON-vs-SOTA Margins

No prior SK-V12 PMU TSV with the same schema is checked in. Delta vs SK-V12 is
therefore unavailable for c/B. V2 does provide the current empirical floor for
S-P2:

- Direct worst c/B: `y_string_unicode` 10.942, `unicode_mixed` 7.667,
  `unicode_escapes` 7.074, `distinct_values` 5.559, `random` 4.425.
- Mode-III host-call worst c/B: `y_string_unicode` 23.152,
  `unicode_mixed` 18.728, `unicode_escapes` 16.017, `random` 14.264.
- Structural SIMD/scalar ratios: all positive; biggest `mesh` 5.04x,
  `canada` 5.01x, `numbers` 4.96x.

These are profile facts, not row admissions. SOTA admission still requires the
gate-consumed strict comparator workflow in later waves.

## §4 - Anomalies + masking signals (flagged for S-P2)

1. Branch/L1/LLC misses are unavailable from the current xctrace export, not
   zero. S-P2 must not use missing cache counters as a proof of memory behavior.
2. Direct V2 counters are sampled-log `PROBE_RESULT` rows, not Criterion gate
   rows. Use them for hot-leaf/cost attribution only.
3. Parse comparator PMU still lacks same-run sonic parse counters; parse SOTA
   margins remain anchored to `skinny/RESULTS.md`.
4. Typed PMU coverage remains 7 generated rows; the ten absent typed rows are
   product-surface gaps, not profiler omissions.
5. Structural SIMD scan is fast, but prior union-substrate regressions
   (REDRESS 96/97/98) remain binding history. A new union route must name a
   material differential.

## §5 - Sources (artefact paths + run id)

- `/tmp/skv13-p1/artifacts/identity.txt`
- `/tmp/skv13-p1/pmu/pmu_rows.tsv`
- `/tmp/skv13-p1/pmu/capture_status.tsv`
- `/tmp/skv13-p1/xctrace/capture_status.tsv`
- `/tmp/skv13-p1-v2/artifacts/identity.txt`
- `/tmp/skv13-p1-v2/samply/logs/direct__{corpus}__track{1,2}.log`
- `/tmp/skv13-p1-v2/mode3/mode3_rows.tsv`
- `/tmp/skv13-p1-v2/mode3/capture_status.tsv`
- `/tmp/skv13-p1-v2/xctrace-export/cpu-state.xml`
- `/tmp/skv13-p1-v2/summary/direct_summary.tsv`
- `/tmp/skv13-p1-v2/summary/mode3_summary.tsv`
- `restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md`
- `restart/skinny/tranches/sk-v13/research/p1/support/profile-provenance-v3.md`
- `restart/skinny/tranches/sk-v13/research/p1/support/summarize_profile_rows.py`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
