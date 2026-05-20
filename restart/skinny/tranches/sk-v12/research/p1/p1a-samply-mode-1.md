# SK-V12 P1-A: Parse-Only Pin Profile

Pass: S-P1 Profile. Cycle: V12 user-pin rerun.
Date: 2026-05-20.
Scope: pin-aware parse-only profile for capture source commit `cf7848b2`, JSON
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

This fold records the final pin-era profile root, `/tmp/skv12-pin-p1`:

| Artifact | Coverage | Authority |
|---|---:|---|
| PMU parse rows | 34/34 PASS | `/tmp/skv12-pin-p1/pmu/parse_pmu_rows.tsv` |
| samply parse captures | 34/34 PASS | `/tmp/skv12-pin-p1/samply/capture_status.tsv` |
| xctrace parse Time Profiler | 34/34 PASS | `/tmp/skv12-pin-p1/xctrace/capture_status.tsv` |
| xctrace parse CPU Counters | 34/34 PASS | `/tmp/skv12-pin-p1/xctrace/capture_status.tsv` |
| Time Profiler XML exports | 34/34 parse, 48/48 product present; status TSV records `SKIP` for already-existing XML | `/tmp/skv12-pin-p1/time_profile_export_status.tsv` |
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

The completed pin replay supplies PMU, samply, xctrace Time Profiler, xctrace
CPU Counters, and derived hot-leaf evidence under `/tmp/skv12-pin-p1`. The
pre-pin replay ledger remains historical only; pin replay authority is the pin
command ledgers under `/tmp/skv12-pin-p1/{pmu,samply}/`, the
`/tmp/skv12-pin-p1/xctrace/capture_status.tsv` status rows, the tracked
`skv12-p1-pin-replay.tsv` ledger, and the pin replay manifest addendum.

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

Fresh parse hot-leaf attribution is admissible for JSON diagnostic/profile
purposes. The `plane=parse` subset of
`/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv` has 34 rows and the
detail table has 170 parse rows, with concrete source anchors in every
load-bearing symbol/source field. Split by mode, the leading parse families are:

| Mode | Leading families |
|---|---|
| `track1` | `bounded_plain_string_scan` 7; `container_dispatch` 7; `number_digit_span` 1; `simd_movemask` 1; `unicode_escape_hex_decode` 1 |
| `track2` | `container_dispatch` 11; `bounded_plain_string_scan` 5; `unicode_escape_hex_decode` 1 |

These are parse-only diagnostics under the user pin, not CSS L4 admission
evidence.

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
classification. P1-A records that fresh parse PMU and parse self-time evidence
exist at `cf7848b2`, while CSS L4 remains absent until generated Track 1 and
lightningcss same-plane comparator rows exist.

Parse-only remains diagnostic under the user pin. `skinny/RESULTS.md` records
JSON parse rows as `S`/`L` NO-GO diagnostic rows, not SK-V12 admission targets.
The campaign close target is generated CSS L4 Track 1 throughput strictly
greater than `lightningcss_mbps + 1` on the same output plane with strict
equality.

## §4 - Anomalies + Masking Signals

- This artifact is not a profile blocker for JSON parse diagnostics: the final
  pin root contains PMU, samply, xctrace, XML export artifacts, and derived
  parse hot-leaf tables for all 34 JSON parse rows.
- The fresh PMU replay proves the capture-source parse workload is runnable and
  produces checksummed Track 1/Track 2 outputs. Source hot-leaf attribution is
  supplied separately by the xctrace-derived summary and detail tables.
- The CSS L4 row required by the pin is not profileable yet because the skinny
  runtime has no generated CSS L4 parser. This is an S-P1 finding; it does not
  authorize falling back to Sheets or BBNF-self before a CSS L4 redress attempt.
- No intervention is proposed. Union-substrate and ASM-gen categories remain
  campaign-unblocked by the pin, but P1-A supplies profile evidence only; any
  route still needs S-P2 material differential and micro-proof before scoping.

## §5 - Sources

- `restart/prompts/skinny/PASS-1-PROFILE.md`
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md`
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `/tmp/skv12-pin-profile-target-cf7848b2/release/xctrace_probe`
- `/tmp/skv12-pin-profile-target-cf7848b2/release/profile_direct`
- `/tmp/skv12-pin-p1/pmu/capture_status.tsv`
- `/tmp/skv12-pin-p1/pmu/parse_pmu_rows.tsv`
- `/tmp/skv12-pin-p1/pmu/product_pmu_rows.tsv` (final replay companion; product
  rows are outside P1-A's parse-only ownership)
- `/tmp/skv12-pin-p1/logs/pmu-parse-*.log.out`
