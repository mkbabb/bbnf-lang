# SK-V12 P1-A: Parse-Only Pin Profile

Pass: S-P1 Profile. Cycle: V12 user-pin rerun.
Date: 2026-05-20.
Scope: pin-aware parse-only profile for current HEAD `cf7848b2`, JSON
17-corpus Track 1/Track 2 parse workload, with CSS L4 target treatment.
Output: this file.
Baseline: SK-V12 pin-aware G-Alpha head `cf7848b2`.
Host triple: `aarch64-apple-darwin`; `Darwin 25.4.0 arm64`; CPU `Apple M5 Max`.
Build flags: release profile with debug symbols; `RUSTFLAGS="-C target-cpu=native"`;
fresh target directory `/tmp/skv12-pin-profile-target-cf7848b2`.
Profile tool: parent-owned fresh PMU replay under `/tmp/skv12-pin-p1`; final
PMU authorities `/tmp/skv12-pin-p1/pmu/parse_pmu_rows.tsv` and
`/tmp/skv12-pin-p1/pmu/capture_status.tsv`; `samply 0.13.1`; `xctrace version
26.0 (17A5241e)`.
Corpus coverage: JSON parse PMU 17/17 x Track 1/Track 2 complete; samply
parse 34/34; xctrace Time Profiler parse 34/34; xctrace CPU Counters parse
34/34. CSS L4 parse artifacts remain 0/0 because no generated skinny CSS L4
runtime exists.
Disposition: PASS for JSON parse profile authority; CSS L4 is recorded as
unprofiled until W1 creates generated Track 1.

## Final Orchestrator Fold - 2026-05-20

This fold supersedes the partial-capture blocker text that remains below from
the lane agent's inspection window. The final pin-era profile root is
`/tmp/skv12-pin-p1`:

| Artifact | Coverage | Authority |
|---|---:|---|
| PMU parse rows | 34/34 PASS | `/tmp/skv12-pin-p1/pmu/parse_pmu_rows.tsv` |
| samply parse captures | 34/34 PASS | `/tmp/skv12-pin-p1/samply/capture_status.tsv` |
| xctrace parse Time Profiler | 34/34 PASS | `/tmp/skv12-pin-p1/xctrace/capture_status.tsv` |
| xctrace parse CPU Counters | 34/34 PASS | `/tmp/skv12-pin-p1/xctrace/capture_status.tsv` |
| Time Profiler XML exports | 34/34 parse, 48/48 product | `/tmp/skv12-pin-p1/time_profile_export_status.tsv` |
| Derived hot-leaf summary | 82 data rows | `/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv` |
| Derived hot-leaf details | 410 data rows | `/tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv` |

Validation:

```bash
awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++}
  END{print total, bad+0}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv
# 212 0

awk -F '\t' 'NR>1 {n++; if($16 ~ /:0([^0-9]|$)/ || $16 ~ /unknown/ || $15=="none") bad++}
  END{print n, bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv
# 82 0

awk -F '\t' 'NR>1 {n++; if($9 ~ /:0([^0-9]|$)/ || $9 ~ /unknown/ || $8=="none") bad++}
  END{print n, bad+0}' /tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv
# 410 0
```

Parse hot-leaf authority is the `plane=parse` subset of
`/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv` and
`/tmp/skv12-pin-p1/time_profile_parse_table.md`. The leading parse families
in the final summary are `container_dispatch` (18 rows),
`bounded_plain_string_scan` (12), `unicode_escape_hex_decode` (2),
`number_digit_span` (1), and `simd_movemask` (1). Parse-only remains
diagnostic-only under the user pin.

## §1 - Method

P1-A did not run cargo, xctrace, or samply. The parent orchestrator owns the
fresh capture. P1-A inspected the parent-produced root and the replay ledger:

```bash
git rev-parse --short HEAD
ls -l /tmp/skv12-pin-profile-target-cf7848b2/release/xctrace_probe \
      /tmp/skv12-pin-profile-target-cf7848b2/release/profile_direct
awk -F '\t' 'NR>1 {count[$2 FS $3]++} END {for (k in count) print count[k], k}' \
  /tmp/skv12-pin-p1/pmu/capture_status.tsv
sed -n '1,60p' /tmp/skv12-pin-p1/pmu/parse_pmu_rows.tsv
find /tmp/skv12-pin-p1/samply/parse -maxdepth 1 -type f
find /tmp/skv12-pin-p1/parse-xctrace -maxdepth 3 -type f
find skinny/crates/runtime/src/grammars -maxdepth 3 -type f | sort
```

The parent replay command is a PMU-only projection of
`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv` with
`/tmp/skv12-p1` rewritten to `/tmp/skv12-pin-p1` and
`/tmp/skv12-profile-target-50bd1648` rewritten to
`/tmp/skv12-pin-profile-target-cf7848b2`. The completed PMU replay supplies
fresh parse rows in `/tmp/skv12-pin-p1/pmu/parse_pmu_rows.tsv`. It does not
create `/tmp/skv12-pin-p1/samply/parse/`,
`/tmp/skv12-pin-p1/parse-xctrace/time-profiler/`,
`/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/`, or
`/tmp/skv12-pin-p1/parse-xctrace/exports/`.

## §2 - Findings

Fresh JSON parse PMU replay is complete for all 17 corpora and both tracks.
The table below is copied from the final
`/tmp/skv12-pin-p1/pmu/parse_pmu_rows.tsv` authority.
These are PMU wrapper measurements, not samply or xctrace self-time
attribution. They cannot satisfy the P1-A top-20 symbol table requirement by
themselves.

| Corpus | Track | Mbps | cycles/B | CPI | checksum |
|---|---|---:|---:|---:|---:|
| `apache_builds` | `track1` | 9270.515 | 2.754572 | 0.216186 | 28272 |
| `apache_builds` | `track2` | 9079.080 | 2.844784 | 0.240956 | 28272 |
| `canada` | `track1` | 12242.138 | 2.009252 | 0.121070 | 892944 |
| `canada` | `track2` | 11492.206 | 2.089786 | 0.127072 | 892944 |
| `citm_catalog` | `track1` | 22483.527 | 1.117469 | 0.145934 | 0 |
| `citm_catalog` | `track2` | 14880.233 | 1.665988 | 0.209537 | 0 |
| `distinct_values` | `track1` | 7255.305 | 3.630433 | 0.194069 | 44472 |
| `distinct_values` | `track2` | 4595.925 | 5.693229 | 0.300756 | 44472 |
| `github_events` | `track1` | 11820.946 | 2.314915 | 0.211770 | 0 |
| `github_events` | `track2` | 8832.828 | 2.680254 | 0.255828 | 0 |
| `gsoc-2018` | `track1` | 16642.690 | 1.524574 | 0.225123 | 0 |
| `gsoc-2018` | `track2` | 14949.855 | 1.591085 | 0.243263 | 0 |
| `instruments` | `track1` | 12415.798 | 2.057424 | 0.161882 | 0 |
| `instruments` | `track2` | 9337.860 | 2.940731 | 0.231788 | 0 |
| `marine_ik` | `track1` | 9573.793 | 2.686087 | 0.146592 | 1438252 |
| `marine_ik` | `track2` | 8934.040 | 2.780725 | 0.151605 | 1438252 |
| `mesh` | `track1` | 9266.265 | 2.648959 | 0.133469 | 0 |
| `mesh` | `track2` | 8522.801 | 2.793515 | 0.139630 | 0 |
| `numbers` | `track1` | 13514.851 | 1.816498 | 0.143769 | 40012 |
| `numbers` | `track2` | 10005.747 | 1.959424 | 0.153391 | 40012 |
| `random` | `track1` | 6184.217 | 3.643760 | 0.192707 | 0 |
| `random` | `track2` | 6247.977 | 4.426358 | 0.240094 | 0 |
| `twitter` | `track1` | 11331.782 | 2.244937 | 0.199605 | 0 |
| `twitter` | `track2` | 9368.936 | 2.862404 | 0.264235 | 0 |
| `unicode_basic` | `track1` | 9389.461 | 2.916229 | 0.197410 | 368584 |
| `unicode_basic` | `track2` | 8231.577 | 3.207192 | 0.217403 | 368584 |
| `unicode_escapes` | `track1` | 8671.757 | 3.039303 | 0.233993 | 45096 |
| `unicode_escapes` | `track2` | 10187.882 | 2.790217 | 0.215341 | 45096 |
| `unicode_mixed` | `track1` | 5851.791 | 4.570988 | 0.384490 | 167480 |
| `unicode_mixed` | `track2` | 6788.599 | 4.109783 | 0.356148 | 167480 |
| `update_center` | `track1` | 9258.339 | 2.907333 | 0.201789 | 141124 |
| `update_center` | `track2` | 6784.238 | 3.761308 | 0.271572 | 141124 |
| `y_string_unicode` | `track1` | 4934.259 | 5.623887 | 0.236395 | 0 |
| `y_string_unicode` | `track2` | 4391.643 | 5.935089 | 0.251973 | 0 |

No fresh parse hot-leaf table is admissible from the current artifact set:

- `find /tmp/skv12-pin-p1/samply/parse -maxdepth 1 -type f` found no parse
  samply directory or parse samply files.
- `find /tmp/skv12-pin-p1/parse-xctrace -maxdepth 3 -type f` found no parse
  xctrace directory, Time Profiler traces, CPU Counters traces, or exports.
- `/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv` is absent.
- `/tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv` is absent.

CSS L4 target treatment under the user pin:

- There is no generated CSS L4 runtime under
  `skinny/crates/runtime/src/grammars/`. The only runtime grammar directories
  are `json` and `sheets_witness`.
- `skinny/crates/runtime/src/grammars/css_l4/generated.rs` is absent.
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs`
  is absent.
- The existing `nonjson-pass-css-l4.json` report fixture and report schema
  helpers are not a generated CSS parser and cannot serve as P1-A admission or
  profile authority.
- Therefore this P1-A cycle has no CSS L4 parse-only profile. Root-workspace
  CSS snippets, report fixtures, or lightningcss-only runs are not substituted
  for the missing skinny generated Track 1 parser.

## §3 - Delta vs SK-V11

No row-level delta is claimed here. P1-F owns RESULTS extraction and delta
classification. P1-A only records that fresh parse PMU at `cf7848b2` exists,
while the required fresh self-time evidence is absent.

Parse-only remains diagnostic under the user pin. `skinny/RESULTS.md` records
JSON parse rows as `S`/`L` NO-GO diagnostic rows, not SK-V12 admission targets.
The campaign close target is generated CSS L4 Track 1 throughput strictly
greater than `lightningcss_mbps + 1` on the same output plane with strict
equality.

## §4 - Anomalies + Masking Signals

- This artifact is intentionally a blocker, not a degraded profile. P1-A cannot
  satisfy PASS-1 §2.2 or CH1/CH6 without fresh symbol-bearing samply and/or
  xctrace Time Profiler exports for all 34 JSON parse rows.
- The fresh PMU replay proves the current-head parse workload is runnable and
  produces checksummed Track 1/Track 2 outputs, but PMU logs alone do not name
  top self-time symbols, source file:line anchors, or top-20 per-corpus stacks.
- The CSS L4 row required by the pin is not profileable yet because the skinny
  runtime has no generated CSS L4 parser. This is an S-P1 finding; it does not
  authorize falling back to Sheets or BBNF-self before a CSS L4 redress attempt.
- No intervention is proposed. Union-substrate and ASM-gen categories remain
  campaign-unblocked by the pin, but P1-A supplies no fresh hot-leaf authority
  for scoping those routes.

## §5 - Sources

- `restart/prompts/skinny/PASS-1-PROFILE.md`
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md`
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `/tmp/skv12-pin-profile-target-cf7848b2/release/xctrace_probe`
- `/tmp/skv12-pin-profile-target-cf7848b2/release/profile_direct`
- `/tmp/skv12-pin-p1/pmu/capture_status.tsv`
- `/tmp/skv12-pin-p1/pmu/parse_pmu_rows.tsv`
- `/tmp/skv12-pin-p1/pmu/product_pmu_rows.tsv` (final replay companion; product
  rows are outside P1-A's parse-only ownership)
- `/tmp/skv12-pin-p1/logs/pmu-parse-*.log.out`

## §6 - Exact Missing Fresh Parse Artifacts

The replay ledger expects the following fresh parse paths after rewriting the
old root to `/tmp/skv12-pin-p1`. Each path below remained absent when this
P1-A artifact was updated after PMU replay completion.

Missing samply parse captures:

- `/tmp/skv12-pin-p1/samply/parse/apache_builds__track1.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/apache_builds__track2.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/canada__track1.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/canada__track2.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/citm_catalog__track1.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/citm_catalog__track2.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/distinct_values__track1.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/distinct_values__track2.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/github_events__track1.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/github_events__track2.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/gsoc-2018__track1.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/gsoc-2018__track2.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/instruments__track1.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/instruments__track2.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/marine_ik__track1.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/marine_ik__track2.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/mesh__track1.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/mesh__track2.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/numbers__track1.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/numbers__track2.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/random__track1.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/random__track2.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/twitter__track1.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/twitter__track2.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/unicode_basic__track1.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/unicode_basic__track2.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/unicode_escapes__track1.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/unicode_escapes__track2.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/unicode_mixed__track1.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/unicode_mixed__track2.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/update_center__track1.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/update_center__track2.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/y_string_unicode__track1.json.gz`
- `/tmp/skv12-pin-p1/samply/parse/y_string_unicode__track2.json.gz`

Missing xctrace Time Profiler parse traces:

- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/apache_builds__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/apache_builds__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/canada__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/canada__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/citm_catalog__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/citm_catalog__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/distinct_values__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/distinct_values__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/github_events__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/github_events__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/gsoc-2018__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/gsoc-2018__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/instruments__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/instruments__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/marine_ik__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/marine_ik__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/mesh__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/mesh__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/numbers__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/numbers__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/random__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/random__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/twitter__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/twitter__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/unicode_basic__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/unicode_basic__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/unicode_escapes__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/unicode_escapes__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/unicode_mixed__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/unicode_mixed__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/update_center__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/update_center__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/y_string_unicode__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/time-profiler/y_string_unicode__track2.trace`

Missing xctrace CPU Counters parse traces:

- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/apache_builds__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/apache_builds__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/canada__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/canada__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/citm_catalog__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/citm_catalog__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/distinct_values__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/distinct_values__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/github_events__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/github_events__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/gsoc-2018__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/gsoc-2018__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/instruments__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/instruments__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/marine_ik__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/marine_ik__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/mesh__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/mesh__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/numbers__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/numbers__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/random__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/random__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/twitter__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/twitter__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/unicode_basic__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/unicode_basic__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/unicode_escapes__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/unicode_escapes__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/unicode_mixed__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/unicode_mixed__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/update_center__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/update_center__track2.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/y_string_unicode__track1.trace`
- `/tmp/skv12-pin-p1/parse-xctrace/cpu-counters/y_string_unicode__track2.trace`

Missing xctrace Time Profiler parse exports:

- `/tmp/skv12-pin-p1/parse-xctrace/exports/apache_builds__track1.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/apache_builds__track2.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/canada__track1.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/canada__track2.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/citm_catalog__track1.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/citm_catalog__track2.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/distinct_values__track1.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/distinct_values__track2.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/github_events__track1.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/github_events__track2.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/gsoc-2018__track1.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/gsoc-2018__track2.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/instruments__track1.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/instruments__track2.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/marine_ik__track1.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/marine_ik__track2.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/mesh__track1.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/mesh__track2.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/numbers__track1.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/numbers__track2.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/random__track1.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/random__track2.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/twitter__track1.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/twitter__track2.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/unicode_basic__track1.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/unicode_basic__track2.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/unicode_escapes__track1.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/unicode_escapes__track2.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/unicode_mixed__track1.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/unicode_mixed__track2.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/update_center__track1.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/update_center__track2.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/y_string_unicode__track1.time-profile.xml`
- `/tmp/skv12-pin-p1/parse-xctrace/exports/y_string_unicode__track2.time-profile.xml`
