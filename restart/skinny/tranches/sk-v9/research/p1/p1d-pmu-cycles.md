# SK-V9 P1-D: PMU + Cycles-Per-Byte

Pass: S-P1 Profile. Cycle: V9.
Date: 2026-05-18.
Scope: PMU counters, cycles-per-byte eligibility, and row-metadata extraction for every JSON corpus/workload.
Output: this file.
Baseline: current HEAD `b258a406ff7f46298c0baeaaf38d2c00add377fd` as Alpha-closed opening authority; fresh `SK-V9-open` PMU counters are absent until W0 telemetry-lock.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max` from the current W0 telemetry manifest.
Build flags: `profile=bench;rustflags=-C target-cpu=native;target_cpu=native` from the current W0 telemetry manifest.
Profile tool: no PMU source admitted in the current authority; Criterion slope/sample metadata only.
Corpus coverage: 17/17 corpora enumerated; authoritative PMU/cycles-per-byte coverage is 0/17 until W0 telemetry-lock.

## §1 — Method (commands run; verbatim, reproducible)

This artifact is an extraction and gate-routing artifact, not a fresh PMU run.
The S-P1 contract requires P1-D to collect PMU counters and derive cycles per
byte for every corpus/workload (`restart/prompts/skinny/PASS-1-PROFILE.md:55`),
and its CH1 lens rejects c/B figures that are estimated rather than derived
from real PMU counters (`restart/prompts/skinny/PASS-1-PROFILE.md:123`-
`restart/prompts/skinny/PASS-1-PROFILE.md:127`). The same contract requires all
17 JSON corpora with no float-heavy subset (`restart/prompts/skinny/PASS-1-PROFILE.md:67`-
`restart/prompts/skinny/PASS-1-PROFILE.md:86`).

Commands run:

```sh
git status --short
git rev-parse HEAD
rg --files -g 'skinny/RESULTS.md' -g 'skinny/REDRESS.md' -g 'restart/skinny/tranches/sk-v9/SYNTHESIS.md' -g '*HANDOFF.md' -g '*PASS*' -g '*metadata*' -g '*gate*'
nl -ba restart/prompts/skinny/PASS-1-PROFILE.md
nl -ba restart/skinny/tranches/sk-v9/HANDOFF.md
nl -ba restart/skinny/tranches/sk-v9/SYNTHESIS.md
nl -ba skinny/RESULTS.md
nl -ba skinny/REDRESS.md
nl -ba skinny/crates/bbnf-bench/src/metadata.rs
nl -ba skinny/crates/bbnf-bench/src/gate.rs
nl -ba skinny/crates/bbnf-bench/src/bin/gate.rs
nl -ba skinny/crates/bbnf-bench/src/report.rs
```

Opening authority and routing:

- The user supplied current HEAD as the Alpha-closed opening authority for this
  S-P1 slice. The checked SK-V9 tranche text still says implementation is not
  dispatched until G-Alpha and no SK-V9 `SPEC.md` or dispatch prompt exists
  (`restart/skinny/tranches/sk-v9/HANDOFF.md:5`-
  `restart/skinny/tranches/sk-v9/HANDOFF.md:8`;
  `restart/skinny/tranches/sk-v9/SYNTHESIS.md:5`-
  `restart/skinny/tranches/sk-v9/SYNTHESIS.md:9`). This file therefore records
  what can be extracted from current authority and routes the missing PMU work to
  the future W0 telemetry-lock.
- SK-V9 Alpha names `SK-V9-open telemetry/gate refresh` as a gate-only
  prerequisite with no behavior or throughput movement and no measured row
  additions without their own gate (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:116`-
  `restart/skinny/tranches/sk-v9/SYNTHESIS.md:117`).
- Strict admission is rejected when measured validation, c/B or sample cost, or
  hot-leaf attribution is missing (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:222`-
  `restart/skinny/tranches/sk-v9/SYNTHESIS.md:226`). The telemetry schema
  requires `c/B or sample cost`, profile artifact, run id, and host/build
  metadata (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:279`-
  `restart/skinny/tranches/sk-v9/SYNTHESIS.md:282`).
- The current `RowMetadata` source carries input bytes, sample size, measurement
  time, workload, and hardware/build fields, but no fields for cycles,
  instructions, branch misses, L1 misses, or LLC misses
  (`skinny/crates/bbnf-bench/src/metadata.rs:20`-
  `skinny/crates/bbnf-bench/src/metadata.rs:65`). The rendered telemetry carries
  `sample_cost` and `sample_count`, not PMU counters
  (`skinny/crates/bbnf-bench/src/report.rs:43`-
  `skinny/crates/bbnf-bench/src/report.rs:68`).

Derivation rule used here: no c/B is derived unless same-run cycles and input
bytes are present. The current authority has bytes and Criterion slope time, but
no admitted cycles counter, so every c/B cell is `blocked: no PMU cycles`.

## §2 — Findings (per-corpus table; file:line on every hot-leaf claim)

Current row inventory:

- `parse_only`: 17/17 rows, all `NO-GO` (`S` except `canada` as `L`) in
  `skinny/RESULTS.md:5`-`skinny/RESULTS.md:42`.
- `direct_to_struct`: 17/17 rows, 3 `A / GO` and 14 `N-direct / NO-GO` in
  `skinny/RESULTS.md:6`-`skinny/RESULTS.md:42`; the SK-V9 synthesis repeats
  this state (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:31`-
  `restart/skinny/tranches/sk-v9/SYNTHESIS.md:35`).
- `real_typed_struct`: 4 measured `A / GO` rows in the current table:
  `twitter`, `update_center`, `mesh`, and `marine_ik`
  (`skinny/RESULTS.md:7`, `skinny/RESULTS.md:18`,
  `skinny/RESULTS.md:21`, `skinny/RESULTS.md:28`).
- `cycles_per_byte`: the metadata source defines a SIMD structural-scan
  workload named `cycles_per_byte` (`skinny/crates/bbnf-bench/src/metadata.rs:248`-
  `skinny/crates/bbnf-bench/src/metadata.rs:293`), and the gate validates those
  rows as `track=SimdScan`, `workload=cycles_per_byte`, and
  `output_plane=offset bitmap` (`skinny/crates/bbnf-bench/src/bin/gate.rs:1389`-
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1419`). The current report does not
  render PMU counters or c/B; it renders only the Canada structural-scan Mbps
  note (`skinny/RESULTS.md:97`).

Hot-leaf posture: every current main-table hot-leaf cell is a Criterion slope
artifact binding such as `criterion-slope-profile:...;row=...`, not a resolved
PMU or symbol attribution (`skinny/RESULTS.md:5`-`skinny/RESULTS.md:42`). P1-D
makes no symbol-level hot-leaf claim in this artifact.

### Corpus/workload coverage

| Corpus | `parse_only` | `direct_to_struct` | `real_typed_struct` | `cycles_per_byte` / PMU |
|---|---|---|---|---|
| `twitter` | present, `S / NO-GO` (`skinny/RESULTS.md:5`) | present, `N-direct / NO-GO` (`skinny/RESULTS.md:6`) | present, `A / GO` (`skinny/RESULTS.md:7`) | not rendered; W0 telemetry-lock required |
| `citm_catalog` | present, `S / NO-GO` (`skinny/RESULTS.md:8`) | present, `A / GO` (`skinny/RESULTS.md:9`) | absent measured; source/product parity only (`skinny/REDRESS.md:2622`-`skinny/REDRESS.md:2625`) | not rendered; W0 telemetry-lock required |
| `canada` | present, `L / NO-GO` (`skinny/RESULTS.md:10`) | present, `N-direct / NO-GO` (`skinny/RESULTS.md:11`) | rejected/routed for checksum mismatch (`skinny/REDRESS.md:2637`-`skinny/REDRESS.md:2640`) | structural-scan Mbps note only (`skinny/RESULTS.md:97`); no PMU/c/B |
| `apache_builds` | present, `S / NO-GO` (`skinny/RESULTS.md:12`) | present, `N-direct / NO-GO` (`skinny/RESULTS.md:13`) | absent measured; source/product parity only (`skinny/REDRESS.md:2622`-`skinny/REDRESS.md:2625`) | not rendered; W0 telemetry-lock required |
| `github_events` | present, `S / NO-GO` (`skinny/RESULTS.md:14`) | present, `N-direct / NO-GO` (`skinny/RESULTS.md:15`) | absent measured from current row table (`skinny/RESULTS.md:5`-`skinny/RESULTS.md:42`) | not rendered; W0 telemetry-lock required |
| `update_center` | present, `S / NO-GO` (`skinny/RESULTS.md:16`) | present, `N-direct / NO-GO` (`skinny/RESULTS.md:17`) | present, `A / GO` (`skinny/RESULTS.md:18`) | not rendered; W0 telemetry-lock required |
| `mesh` | present, `S / NO-GO` (`skinny/RESULTS.md:19`) | present, `N-direct / NO-GO` (`skinny/RESULTS.md:20`) | present, `A / GO` (`skinny/RESULTS.md:21`) | not rendered; W0 telemetry-lock required |
| `random` | present, `S / NO-GO` (`skinny/RESULTS.md:22`) | present, `N-direct / NO-GO` (`skinny/RESULTS.md:23`) | absent measured from current row table (`skinny/RESULTS.md:5`-`skinny/RESULTS.md:42`) | not rendered; W0 telemetry-lock required |
| `gsoc-2018` | present, `S / NO-GO` (`skinny/RESULTS.md:24`) | present, `N-direct / NO-GO` (`skinny/RESULTS.md:25`) | absent measured from current row table (`skinny/RESULTS.md:5`-`skinny/RESULTS.md:42`) | not rendered; W0 telemetry-lock required |
| `marine_ik` | present, `S / NO-GO` (`skinny/RESULTS.md:26`) | present, `A / GO` (`skinny/RESULTS.md:27`) | present, `A / GO` (`skinny/RESULTS.md:28`) | not rendered; W0 telemetry-lock required |
| `instruments` | present, `S / NO-GO` (`skinny/RESULTS.md:29`) | present, `N-direct / NO-GO` (`skinny/RESULTS.md:30`) | absent measured from current row table (`skinny/RESULTS.md:5`-`skinny/RESULTS.md:42`) | not rendered; W0 telemetry-lock required |
| `numbers` | present, `S / NO-GO` (`skinny/RESULTS.md:31`) | present, `N-direct / NO-GO` (`skinny/RESULTS.md:32`) | absent measured from current row table (`skinny/RESULTS.md:5`-`skinny/RESULTS.md:42`) | not rendered; W0 telemetry-lock required |
| `unicode_mixed` | present, `S / NO-GO` (`skinny/RESULTS.md:33`) | present, `N-direct / NO-GO` (`skinny/RESULTS.md:34`) | absent measured from current row table (`skinny/RESULTS.md:5`-`skinny/RESULTS.md:42`) | not rendered; W0 telemetry-lock required |
| `unicode_escapes` | present, `S / NO-GO` (`skinny/RESULTS.md:35`) | present, `N-direct / NO-GO` (`skinny/RESULTS.md:36`) | absent measured from current row table (`skinny/RESULTS.md:5`-`skinny/RESULTS.md:42`) | not rendered; W0 telemetry-lock required |
| `unicode_basic` | present, `S / NO-GO` (`skinny/RESULTS.md:37`) | present, `A / GO` (`skinny/RESULTS.md:38`) | absent measured from current row table (`skinny/RESULTS.md:5`-`skinny/RESULTS.md:42`) | not rendered; W0 telemetry-lock required |
| `distinct_values` | present, `S / NO-GO` (`skinny/RESULTS.md:39`) | present, `N-direct / NO-GO` (`skinny/RESULTS.md:40`) | absent measured from current row table (`skinny/RESULTS.md:5`-`skinny/RESULTS.md:42`) | not rendered; W0 telemetry-lock required |
| `y_string_unicode` | present, `S / NO-GO` (`skinny/RESULTS.md:41`) | present, `N-direct / NO-GO` (`skinny/RESULTS.md:42`) | absent measured from current row table (`skinny/RESULTS.md:5`-`skinny/RESULTS.md:42`) | not rendered; W0 telemetry-lock required |

### Extracted Mbps, duration, and row metadata

`track1_ns` and `ns/B` come from the rendered `Sample cost` column. They are
Criterion slope/sample metadata, not PMU counters. `c/B` is not derived.

| Row | Result src | Metadata src | T1 Mbps | T2 Mbps | sonic strict Mbps | serde Mbps | ns/B | track1_ns | bytes | samples | c/B |
|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---|
| `json/twitter/parse_only/main` | `skinny/RESULTS.md:5` | `skinny/RESULTS.md:48` | 9581 | 9741 | 18176 | 3829 | 0.834967 | 527293.98 | 631515 | 100 | not derived: no PMU cycles |
| `json/twitter/direct_to_struct/main` | `skinny/RESULTS.md:6` | `skinny/RESULTS.md:49` | 11859 | 9881 | 12890 | 6673 | 0.674605 | 426023.11 | 631515 | 100 | not derived: no PMU cycles |
| `json/twitter/real_typed_struct/main` | `skinny/RESULTS.md:7` | `skinny/RESULTS.md:50` | 15333 | 14516 | 13646 | 15046 | 0.521764 | 329501.71 | 631515 | 100 | not derived: no PMU cycles |
| `json/citm_catalog/parse_only/main` | `skinny/RESULTS.md:8` | `skinny/RESULTS.md:51` | 28644 | 19214 | 21717 | 7401 | 0.279290 | 482391.33 | 1727204 | 100 | not derived: no PMU cycles |
| `json/citm_catalog/direct_to_struct/main` | `skinny/RESULTS.md:9` | `skinny/RESULTS.md:52` | 21151 | 19434 | 18241 | 12992 | 0.378240 | 653298.12 | 1727204 | 100 | not derived: no PMU cycles |
| `json/canada/parse_only/main` | `skinny/RESULTS.md:10` | `skinny/RESULTS.md:53` | 15497 | 12171 | 8729 | 4050 | 0.516215 | 1162027.13 | 2251051 | 50 | not derived: no PMU cycles |
| `json/canada/direct_to_struct/main` | `skinny/RESULTS.md:11` | `skinny/RESULTS.md:54` | 6586 | 9769 | 12430 | 7080 | 1.214778 | 2734527.46 | 2251051 | 50 | not derived: no PMU cycles |
| `json/apache_builds/parse_only/main` | `skinny/RESULTS.md:12` | `skinny/RESULTS.md:55` | 12694 | 11715 | 16904 | 4278 | 0.630235 | 80213.22 | 127275 | 100 | not derived: no PMU cycles |
| `json/apache_builds/direct_to_struct/main` | `skinny/RESULTS.md:13` | `skinny/RESULTS.md:56` | 8306 | 7796 | 8852 | 6750 | 0.963175 | 122588.06 | 127275 | 100 | not derived: no PMU cycles |
| `json/github_events/parse_only/main` | `skinny/RESULTS.md:14` | `skinny/RESULTS.md:57` | 10689 | 10073 | 16408 | 4675 | 0.748431 | 48746.81 | 65132 | 100 | not derived: no PMU cycles |
| `json/github_events/direct_to_struct/main` | `skinny/RESULTS.md:15` | `skinny/RESULTS.md:58` | 9088 | 7337 | 9818 | 8152 | 0.880328 | 57337.54 | 65132 | 100 | not derived: no PMU cycles |
| `json/update_center/parse_only/main` | `skinny/RESULTS.md:16` | `skinny/RESULTS.md:59` | 11926 | 9312 | 18769 | 4131 | 0.670820 | 357666.41 | 533178 | 100 | not derived: no PMU cycles |
| `json/update_center/direct_to_struct/main` | `skinny/RESULTS.md:17` | `skinny/RESULTS.md:60` | 7863 | 7514 | 10525 | 8218 | 1.017416 | 542463.76 | 533178 | 100 | not derived: no PMU cycles |
| `json/update_center/real_typed_struct/main` | `skinny/RESULTS.md:18` | `skinny/RESULTS.md:61` | 11958 | 10367 | 11952 | 10296 | 0.669015 | 356704.17 | 533178 | 100 | not derived: no PMU cycles |
| `json/mesh/parse_only/main` | `skinny/RESULTS.md:19` | `skinny/RESULTS.md:62` | 9367 | 10000 | 8143 | 4123 | 0.854072 | 618004.02 | 723597 | 100 | not derived: no PMU cycles |
| `json/mesh/direct_to_struct/main` | `skinny/RESULTS.md:20` | `skinny/RESULTS.md:63` | 8640 | 9049 | 9967 | 7176 | 0.925969 | 670028.27 | 723597 | 100 | not derived: no PMU cycles |
| `json/mesh/real_typed_struct/main` | `skinny/RESULTS.md:21` | `skinny/RESULTS.md:64` | 9623 | 7674 | 9305 | 8212 | 0.831369 | 601576.21 | 723597 | 100 | not derived: no PMU cycles |
| `json/random/parse_only/main` | `skinny/RESULTS.md:22` | `skinny/RESULTS.md:65` | 10011 | 8018 | 15639 | 3486 | 0.799114 | 407928.71 | 510476 | 100 | not derived: no PMU cycles |
| `json/random/direct_to_struct/main` | `skinny/RESULTS.md:23` | `skinny/RESULTS.md:66` | 7751 | 6952 | 8141 | 5922 | 1.032090 | 526856.97 | 510476 | 100 | not derived: no PMU cycles |
| `json/gsoc-2018/parse_only/main` | `skinny/RESULTS.md:24` | `skinny/RESULTS.md:67` | 23209 | 21857 | 49101 | 10741 | 0.344694 | 1147083.03 | 3327831 | 100 | not derived: no PMU cycles |
| `json/gsoc-2018/direct_to_struct/main` | `skinny/RESULTS.md:25` | `skinny/RESULTS.md:68` | 15042 | 14380 | 23356 | 19398 | 0.531838 | 1769865.96 | 3327831 | 100 | not derived: no PMU cycles |
| `json/marine_ik/parse_only/main` | `skinny/RESULTS.md:26` | `skinny/RESULTS.md:69` | 13100 | 12164 | 9921 | 4091 | 0.610675 | 1821927.79 | 2983466 | 100 | not derived: no PMU cycles |
| `json/marine_ik/direct_to_struct/main` | `skinny/RESULTS.md:27` | `skinny/RESULTS.md:70` | 9357 | 9488 | 8559 | 7018 | 0.854932 | 2550660.54 | 2983466 | 100 | not derived: no PMU cycles |
| `json/marine_ik/real_typed_struct/main` | `skinny/RESULTS.md:28` | `skinny/RESULTS.md:71` | 11783 | 8321 | 6951 | 7450 | 0.678927 | 2025554.85 | 2983466 | 100 | not derived: no PMU cycles |
| `json/instruments/parse_only/main` | `skinny/RESULTS.md:29` | `skinny/RESULTS.md:72` | 13320 | 11351 | 17976 | 3028 | 0.600598 | 132339.34 | 220346 | 100 | not derived: no PMU cycles |
| `json/instruments/direct_to_struct/main` | `skinny/RESULTS.md:30` | `skinny/RESULTS.md:73` | 8494 | 8766 | 9872 | 7576 | 0.941843 | 207531.30 | 220346 | 100 | not derived: no PMU cycles |
| `json/numbers/parse_only/main` | `skinny/RESULTS.md:31` | `skinny/RESULTS.md:74` | 12818 | 13537 | 9854 | 4422 | 0.624132 | 93697.21 | 150124 | 100 | not derived: no PMU cycles |
| `json/numbers/direct_to_struct/main` | `skinny/RESULTS.md:32` | `skinny/RESULTS.md:75` | 9773 | 6966 | 7953 | 5753 | 0.818587 | 122889.53 | 150124 | 100 | not derived: no PMU cycles |
| `json/unicode_mixed/parse_only/main` | `skinny/RESULTS.md:33` | `skinny/RESULTS.md:76` | 6390 | 4970 | 9943 | 2654 | 1.251972 | 1318433.73 | 1053086 | 100 | not derived: no PMU cycles |
| `json/unicode_mixed/direct_to_struct/main` | `skinny/RESULTS.md:34` | `skinny/RESULTS.md:77` | 3596 | 3694 | 10077 | 4911 | 2.224411 | 2342496.05 | 1053086 | 100 | not derived: no PMU cycles |
| `json/unicode_escapes/parse_only/main` | `skinny/RESULTS.md:35` | `skinny/RESULTS.md:78` | 12731 | 8521 | 13851 | 4040 | 0.628379 | 660298.65 | 1050797 | 100 | not derived: no PMU cycles |
| `json/unicode_escapes/direct_to_struct/main` | `skinny/RESULTS.md:36` | `skinny/RESULTS.md:79` | 4020 | 4016 | 13999 | 3720 | 1.990130 | 2091222.45 | 1050797 | 100 | not derived: no PMU cycles |
| `json/unicode_basic/parse_only/main` | `skinny/RESULTS.md:37` | `skinny/RESULTS.md:80` | 11189 | 10040 | 15797 | 3611 | 0.714981 | 749719.23 | 1048586 | 100 | not derived: no PMU cycles |
| `json/unicode_basic/direct_to_struct/main` | `skinny/RESULTS.md:38` | `skinny/RESULTS.md:81` | 9363 | 8420 | 8971 | 6002 | 0.854415 | 895927.13 | 1048586 | 100 | not derived: no PMU cycles |
| `json/distinct_values/parse_only/main` | `skinny/RESULTS.md:39` | `skinny/RESULTS.md:82` | 10279 | 6457 | 18282 | 3158 | 0.778263 | 119564.51 | 153630 | 100 | not derived: no PMU cycles |
| `json/distinct_values/direct_to_struct/main` | `skinny/RESULTS.md:40` | `skinny/RESULTS.md:83` | 4438 | 4151 | 8950 | 5598 | 1.802505 | 276918.86 | 153630 | 100 | not derived: no PMU cycles |
| `json/y_string_unicode/parse_only/main` | `skinny/RESULTS.md:41` | `skinny/RESULTS.md:84` | 5577 | 5480 | 12009 | 5657 | 1.434564 | 51071.91 | 35601 | 100 | not derived: no PMU cycles |
| `json/y_string_unicode/direct_to_struct/main` | `skinny/RESULTS.md:42` | `skinny/RESULTS.md:85` | 4828 | 3563 | 9065 | 7599 | 1.656872 | 58986.29 | 35601 | 100 | not derived: no PMU cycles |

## §3 — Delta vs SK-V8 (per row; Mbps + c/B + classification)

No SK-V9-open PMU run exists in the current authority. The table in §2 is the
per-row SK-V8-open extraction; every SK-V9 delta is therefore `not computed`.
The current report itself marks the telemetry delta as `baseline` in the W0
manifest (`skinny/RESULTS.md:48`-`skinny/RESULTS.md:85`), and SK-V9 synthesis
says the current benchmark authority remains the W0-rendered report
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:21`-
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:27`).

| Row family | Rows | Current classification | SK-V9 Mbps delta | SK-V9 c/B delta |
|---|---:|---|---|---|
| `parse_only` | 17 | 16 `S / NO-GO`, 1 `L / NO-GO` (`restart/skinny/tranches/sk-v9/HANDOFF.md:29`-`restart/skinny/tranches/sk-v9/HANDOFF.md:33`) | not computed: no SK-V9-open run | not derived: no PMU cycles |
| `direct_to_struct` | 17 | 3 `A / GO`, 14 `N-direct / NO-GO` (`restart/skinny/tranches/sk-v9/HANDOFF.md:29`-`restart/skinny/tranches/sk-v9/HANDOFF.md:33`) | not computed: no SK-V9-open run | not derived: no PMU cycles |
| `real_typed_struct` | 4 measured | 4 `A / GO`; Apache/CITM source/product parity is not measured row-table progress (`skinny/REDRESS.md:2648`-`skinny/REDRESS.md:2657`) | not computed: no SK-V9-open run | not derived: no PMU cycles |
| `cycles_per_byte` | 0 rendered PMU rows | semantic SIMD scan metadata exists in source; no rendered PMU/cycles baseline (`skinny/crates/bbnf-bench/src/bin/gate.rs:1412`-`skinny/crates/bbnf-bench/src/bin/gate.rs:1419`) | not computed: no SK-V9-open run | W0 gate requirement |

Per-row classification status:

| Row | Current class | Delta vs SK-V8 | c/B delta |
|---|---|---|---|
| `json/twitter/parse_only/main` | `S / NO-GO` (`skinny/RESULTS.md:5`) | not computed | not derived |
| `json/twitter/direct_to_struct/main` | `N-direct / NO-GO` (`skinny/RESULTS.md:6`) | not computed | not derived |
| `json/twitter/real_typed_struct/main` | `A / GO` (`skinny/RESULTS.md:7`) | not computed | not derived |
| `json/citm_catalog/parse_only/main` | `S / NO-GO` (`skinny/RESULTS.md:8`) | not computed | not derived |
| `json/citm_catalog/direct_to_struct/main` | `A / GO` (`skinny/RESULTS.md:9`) | not computed | not derived |
| `json/canada/parse_only/main` | `L / NO-GO` (`skinny/RESULTS.md:10`) | not computed | not derived |
| `json/canada/direct_to_struct/main` | `N-direct / NO-GO` (`skinny/RESULTS.md:11`) | not computed | not derived |
| `json/apache_builds/parse_only/main` | `S / NO-GO` (`skinny/RESULTS.md:12`) | not computed | not derived |
| `json/apache_builds/direct_to_struct/main` | `N-direct / NO-GO` (`skinny/RESULTS.md:13`) | not computed | not derived |
| `json/github_events/parse_only/main` | `S / NO-GO` (`skinny/RESULTS.md:14`) | not computed | not derived |
| `json/github_events/direct_to_struct/main` | `N-direct / NO-GO` (`skinny/RESULTS.md:15`) | not computed | not derived |
| `json/update_center/parse_only/main` | `S / NO-GO` (`skinny/RESULTS.md:16`) | not computed | not derived |
| `json/update_center/direct_to_struct/main` | `N-direct / NO-GO` (`skinny/RESULTS.md:17`) | not computed | not derived |
| `json/update_center/real_typed_struct/main` | `A / GO` (`skinny/RESULTS.md:18`) | not computed | not derived |
| `json/mesh/parse_only/main` | `S / NO-GO` (`skinny/RESULTS.md:19`) | not computed | not derived |
| `json/mesh/direct_to_struct/main` | `N-direct / NO-GO` (`skinny/RESULTS.md:20`) | not computed | not derived |
| `json/mesh/real_typed_struct/main` | `A / GO` (`skinny/RESULTS.md:21`) | not computed | not derived |
| `json/random/parse_only/main` | `S / NO-GO` (`skinny/RESULTS.md:22`) | not computed | not derived |
| `json/random/direct_to_struct/main` | `N-direct / NO-GO` (`skinny/RESULTS.md:23`) | not computed | not derived |
| `json/gsoc-2018/parse_only/main` | `S / NO-GO` (`skinny/RESULTS.md:24`) | not computed | not derived |
| `json/gsoc-2018/direct_to_struct/main` | `N-direct / NO-GO` (`skinny/RESULTS.md:25`) | not computed | not derived |
| `json/marine_ik/parse_only/main` | `S / NO-GO` (`skinny/RESULTS.md:26`) | not computed | not derived |
| `json/marine_ik/direct_to_struct/main` | `A / GO` (`skinny/RESULTS.md:27`) | not computed | not derived |
| `json/marine_ik/real_typed_struct/main` | `A / GO` (`skinny/RESULTS.md:28`) | not computed | not derived |
| `json/instruments/parse_only/main` | `S / NO-GO` (`skinny/RESULTS.md:29`) | not computed | not derived |
| `json/instruments/direct_to_struct/main` | `N-direct / NO-GO` (`skinny/RESULTS.md:30`) | not computed | not derived |
| `json/numbers/parse_only/main` | `S / NO-GO` (`skinny/RESULTS.md:31`) | not computed | not derived |
| `json/numbers/direct_to_struct/main` | `N-direct / NO-GO` (`skinny/RESULTS.md:32`) | not computed | not derived |
| `json/unicode_mixed/parse_only/main` | `S / NO-GO` (`skinny/RESULTS.md:33`) | not computed | not derived |
| `json/unicode_mixed/direct_to_struct/main` | `N-direct / NO-GO` (`skinny/RESULTS.md:34`) | not computed | not derived |
| `json/unicode_escapes/parse_only/main` | `S / NO-GO` (`skinny/RESULTS.md:35`) | not computed | not derived |
| `json/unicode_escapes/direct_to_struct/main` | `N-direct / NO-GO` (`skinny/RESULTS.md:36`) | not computed | not derived |
| `json/unicode_basic/parse_only/main` | `S / NO-GO` (`skinny/RESULTS.md:37`) | not computed | not derived |
| `json/unicode_basic/direct_to_struct/main` | `A / GO` (`skinny/RESULTS.md:38`) | not computed | not derived |
| `json/distinct_values/parse_only/main` | `S / NO-GO` (`skinny/RESULTS.md:39`) | not computed | not derived |
| `json/distinct_values/direct_to_struct/main` | `N-direct / NO-GO` (`skinny/RESULTS.md:40`) | not computed | not derived |
| `json/y_string_unicode/parse_only/main` | `S / NO-GO` (`skinny/RESULTS.md:41`) | not computed | not derived |
| `json/y_string_unicode/direct_to_struct/main` | `N-direct / NO-GO` (`skinny/RESULTS.md:42`) | not computed | not derived |

## §4 — Anomalies + masking signals (flagged for S-P2)

1. PMU/cycles-per-byte is blocked at the gate, not inferred.
   The current metadata/report stack validates sample metadata and sample cost,
   but it has no admitted PMU counter surface
   (`skinny/crates/bbnf-bench/src/metadata.rs:20`-
   `skinny/crates/bbnf-bench/src/metadata.rs:65`;
   `skinny/crates/bbnf-bench/src/report.rs:275`-
   `skinny/crates/bbnf-bench/src/report.rs:349`). W0 telemetry-lock must add
   same-run `cycles`, `instructions`, `branch_misses`, `l1_misses`,
   `llc_misses`, and the derivable `cycles_per_byte = cycles / input_bytes`
   for each admitted row.
2. Masking probes are not currently rendered in `skinny/RESULTS.md`.
   REDRESS records masking probes as a report artifact with Mbps, ns/iter,
   Track 1 ratio, and signal fields (`skinny/REDRESS.md:163`-
   `skinny/REDRESS.md:170`), and records eager decode as a current MASKING
   signal (`skinny/REDRESS.md:236`-`skinny/REDRESS.md:244`). The gate source
   still treats volatile probes as an optional read path that cannot be combined
   with `--update-results` (`skinny/crates/bbnf-bench/src/bin/gate.rs:26`-
   `skinny/crates/bbnf-bench/src/bin/gate.rs:34`) and lists the probe names in
   `push_probe_rows` (`skinny/crates/bbnf-bench/src/bin/gate.rs:1500`-
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1515`). P1-D cannot attribute
   MASKING from the current rendered report; W0 telemetry-lock must render or
   separately manifest those rows.
3. Apache/CITM typed rows remain source/product parity only.
   REDRESS says those rows are not measured in current W0, and source-only
   parity must not be counted as six measured `real_typed_struct A / GO` rows
   (`skinny/REDRESS.md:2622`-`skinny/REDRESS.md:2657`). SK-V9 synthesis repeats
   the fresh run-id/metadata requirement before presenting them as measured
   rows (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:100`-
   `restart/skinny/tranches/sk-v9/SYNTHESIS.md:103`;
   `restart/skinny/tranches/sk-v9/SYNTHESIS.md:214`-
   `restart/skinny/tranches/sk-v9/SYNTHESIS.md:218`).
4. No PMULL, CTZ/bulk, or feature-specific rewrite is reopened by this profile.
   SK-V9 pre-blocks PMULL prefix-XOR and CTZ/bulk production rewires as default
   hot paths (`restart/skinny/tranches/sk-v9/HANDOFF.md:79`-
   `restart/skinny/tranches/sk-v9/HANDOFF.md:99`), and REDRESS keeps those
   feature routes unadmitted until exact profiles point there
   (`skinny/REDRESS.md:2123`-`skinny/REDRESS.md:2125`).

W0 telemetry-lock gate requirement:

- Produce a `SK-V9-open` run id and manifest row set for all 17 corpora and all
  admitted workloads: `parse_only`, `direct_to_struct`, measured
  `real_typed_struct`, masking probes, and `cycles_per_byte`.
- For every row, emit same-run `input_bytes`, `track1_ns` or measurement window,
  `sample_count`, `cycles`, `instructions`, `branch_misses`, `l1_misses`,
  `llc_misses`, `profile_artifact`, `run_id`, `host_triple`, `build_flags`, and
  `feature_mask`.
- Derive c/B only as `cycles / input_bytes` when both fields are same-run,
  finite, and non-zero. Do not infer cycles from wall time, ns/B, CPU model,
  frequency, or throughput.
- Reject report rendering or strict admission when PMU fields are missing for a
  row that claims `cycles_per_byte` or strict SK-V9 admission.

## §5 — Sources (every artefact path + run id)

Primary sources read:

- `restart/prompts/skinny/PASS-1-PROFILE.md`: P1-D scope and mandatory coverage
  (`restart/prompts/skinny/PASS-1-PROFILE.md:44`-
  `restart/prompts/skinny/PASS-1-PROFILE.md:57`), 17-corpus list
  (`restart/prompts/skinny/PASS-1-PROFILE.md:67`-
  `restart/prompts/skinny/PASS-1-PROFILE.md:86`), output schema
  (`restart/prompts/skinny/PASS-1-PROFILE.md:88`-
  `restart/prompts/skinny/PASS-1-PROFILE.md:110`), and masking-probe rule
  (`restart/prompts/skinny/PASS-1-PROFILE.md:258`-
  `restart/prompts/skinny/PASS-1-PROFILE.md:262`).
- `restart/skinny/tranches/sk-v9/HANDOFF.md`: current state, candidate
  boundaries, and pre-blocks (`restart/skinny/tranches/sk-v9/HANDOFF.md:22`-
  `restart/skinny/tranches/sk-v9/HANDOFF.md:52`;
  `restart/skinny/tranches/sk-v9/HANDOFF.md:79`-
  `restart/skinny/tranches/sk-v9/HANDOFF.md:105`).
- `restart/skinny/tranches/sk-v9/SYNTHESIS.md`: Alpha authority, current row
  state, telemetry binding, strict comparator gate, and c/B/sample-cost
  requirement (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:21`-
  `restart/skinny/tranches/sk-v9/SYNTHESIS.md:52`;
  `restart/skinny/tranches/sk-v9/SYNTHESIS.md:220`-
  `restart/skinny/tranches/sk-v9/SYNTHESIS.md:296`).
- `skinny/RESULTS.md`: current row table (`skinny/RESULTS.md:3`-
  `skinny/RESULTS.md:42`), W0 telemetry manifest (`skinny/RESULTS.md:44`-
  `skinny/RESULTS.md:85`), and notes (`skinny/RESULTS.md:87`-
  `skinny/RESULTS.md:141`).
- `skinny/REDRESS.md`: masking probes, comparator/process caveats, and
  Apache/CITM measured-row routing (`skinny/REDRESS.md:163`-
  `skinny/REDRESS.md:170`; `skinny/REDRESS.md:2098`-
  `skinny/REDRESS.md:2126`; `skinny/REDRESS.md:2620`-
  `skinny/REDRESS.md:2659`).
- `skinny/crates/bbnf-bench/src/metadata.rs`: metadata fields and
  `cycles_per_byte` SIMD scan facts (`skinny/crates/bbnf-bench/src/metadata.rs:20`-
  `skinny/crates/bbnf-bench/src/metadata.rs:65`;
  `skinny/crates/bbnf-bench/src/metadata.rs:248`-
  `skinny/crates/bbnf-bench/src/metadata.rs:293`).
- `skinny/crates/bbnf-bench/src/bin/gate.rs`: W0 report/gate path, telemetry
  construction, metadata validation, and probe rendering path
  (`skinny/crates/bbnf-bench/src/bin/gate.rs:54`-
  `skinny/crates/bbnf-bench/src/bin/gate.rs:80`;
  `skinny/crates/bbnf-bench/src/bin/gate.rs:414`-
  `skinny/crates/bbnf-bench/src/bin/gate.rs:499`;
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1029`-
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1448`;
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1500`-
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1589`).
- `skinny/crates/bbnf-bench/src/gate.rs` and
  `skinny/crates/bbnf-bench/src/report.rs`: strict admission/schema and rendered
  telemetry columns (`skinny/crates/bbnf-bench/src/gate.rs:136`-
  `skinny/crates/bbnf-bench/src/gate.rs:183`;
  `skinny/crates/bbnf-bench/src/report.rs:8`-
  `skinny/crates/bbnf-bench/src/report.rs:17`;
  `skinny/crates/bbnf-bench/src/report.rs:43`-
  `skinny/crates/bbnf-bench/src/report.rs:68`;
  `skinny/crates/bbnf-bench/src/report.rs:575`-
  `skinny/crates/bbnf-bench/src/report.rs:610`).

Current authoritative run id extracted from the manifest:

- `sk-v8-open:criterion-fnv64-9a37562ed3d0383a`
  (`skinny/RESULTS.md:48`-`skinny/RESULTS.md:85`).

Profile artifacts currently cited by the row table are Criterion slope profiles
under `criterion-slope-profile:json_<corpus>/<bench>/new/estimates.json`. They
are evidence for Mbps and sample cost only. They are not PMU artifacts and do
not authorize c/B derivation.
