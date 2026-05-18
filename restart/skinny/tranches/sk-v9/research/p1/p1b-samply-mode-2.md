# SK-V9 P1-B: Samply Mode II Direct And Real Typed Product Profile

Pass: S-P1 Profile. Cycle: V9.
Date: 2026-05-18.
Scope: Mode II product workloads, covering `direct_to_struct` and `real_typed_struct`.
Output: this file.
Baseline: SK-V9 Alpha-closed opening authority at HEAD `b258a406ff7f46298c0baeaaf38d2c00add377fd`; measured row authority remains the W0-rendered `SK-V8-open` run `sk-v8-open:criterion-fnv64-9a37562ed3d0383a` until SK-V9-open W0 telemetry-lock.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max` from `skinny/RESULTS.md:49-85`.
Build flags: `profile=bench;rustflags=-C target-cpu=native;target_cpu=native`; feature mask `arch=aarch64;os=macos;simd=Scalar;target_cpu=native` from `skinny/RESULTS.md:49-85`.
Profile tool: `samply` fresh SK-V9-open capture `absent:sk-v9-open-w0-telemetry-lock-not-produced`; opening authority uses Criterion slope profile artifact strings from `skinny/RESULTS.md:49-85`.
Corpus coverage: `direct_to_struct` 17/17 measured opening rows; `real_typed_struct` 17/17 status rows, with 4 measured rows, 2 source/product parity gaps, 1 rejected row, and 10 `absent:unsupported-real-typed-fixture`.

## §1 - Method (commands run; verbatim, reproducible)

This artifact does not run a new samply capture. The user dispatch binds current
HEAD as the Alpha-closed opening authority, while fresh SK-V9-open samply
profiles remain `absent:sk-v9-open-w0-telemetry-lock-not-produced` until W0
telemetry-lock. The repository handoff records that the current benchmark
authority is still the W0-rendered `skinny/RESULTS.md` report, with 17 direct
rows and 4 measured real typed rows (`restart/skinny/tranches/sk-v9/HANDOFF.md:24-37`).
S-P1 normally requires a checked SK-V{N}-open baseline and comparator declarations
before profiling (`restart/prompts/skinny/PASS-1-PROFILE.md:24-38`); the P1-B
scope is mode II `direct_to_struct` plus `real_typed_struct` with the same
per-corpus discipline (`restart/prompts/skinny/PASS-1-PROFILE.md:50-57`).

Commands run:

```sh
git rev-parse HEAD
git symbolic-ref --short HEAD
nl -ba restart/prompts/skinny/PASS-1-PROFILE.md
nl -ba restart/skinny/tranches/sk-v9/HANDOFF.md
nl -ba restart/skinny/tranches/sk-v9/SYNTHESIS.md
nl -ba skinny/RESULTS.md
nl -ba skinny/REDRESS.md
awk -F'|' 'NR>4 && ($3 ~ /direct_to_struct/ || $3 ~ /real_typed_struct/) { ... }' skinny/RESULTS.md
rg -n "direct_to_struct|real_typed_struct|profile_direct|gate-json" skinny/crates skinny/xtask
```

No `samply record` command was run. Any top-symbol, self-time percent, flame
artifact, or PC-level file:line claim for SK-V9-open is therefore
`absent:sk-v9-open-w0-telemetry-lock-not-produced`, not estimated. The existing
profiling binary supports direct and real-typed modes (`skinny/crates/bbnf-bench/src/bin/profile_direct.rs:57-93`),
but its comment shows `--save-only` (`skinny/crates/bbnf-bench/src/bin/profile_direct.rs:1-6`),
while PASS-1 requires interactive `samply record` with symbol resolution
(`restart/prompts/skinny/PASS-1-PROFILE.md:251-254`).

## §2 - Findings (per-corpus table; file:line on every hot-leaf claim)

Mode II opening facts:

- `direct_to_struct`: 17 measured opening rows, 3 `A / GO` and 14
  `N-direct / NO-GO` (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:31-35`).
- `real_typed_struct`: 4 measured opening rows, all `A / GO`
  (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:31-35`).
- Direct rows stay digest guard-plane rows until S-P3 defines a direct output
  contract or control-path tranche (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:178-181`).
- Current GO rows must maintain unless a later accepted gate sets stricter
  selected-row floors (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:200-210`).

### Direct Row Inventory

| Source | Corpus | Outcome | Verdict | Plane | Track 1 Mbps | Track 2 Mbps | sonic strict Mbps | serde_json Mbps | Criterion profile artifact | SK-V9 samply symbol status |
|---|---|---|---|---|---:|---:|---:|---:|---|---|
| `skinny/RESULTS.md:6`; manifest `skinny/RESULTS.md:49` | `twitter` | `N-direct` | `NO-GO` | digest | 11859 | 9881 | 12890 | 6673 | `criterion-slope-profile:json_twitter/track1_direct_to_struct/new/estimates.json` | `absent:sk-v9-open-w0-telemetry-lock-not-produced` |
| `skinny/RESULTS.md:9`; manifest `skinny/RESULTS.md:52` | `citm_catalog` | `A` | `GO` | digest | 21151 | 19434 | 18241 | 12992 | `criterion-slope-profile:json_citm_catalog/track1_direct_to_struct/new/estimates.json` | `absent:sk-v9-open-w0-telemetry-lock-not-produced` |
| `skinny/RESULTS.md:11`; manifest `skinny/RESULTS.md:54` | `canada` | `N-direct` | `NO-GO` | digest | 6586 | 9769 | 12430 | 7080 | `criterion-slope-profile:json_canada/track1_direct_to_struct/new/estimates.json` | `absent:sk-v9-open-w0-telemetry-lock-not-produced` |
| `skinny/RESULTS.md:13`; manifest `skinny/RESULTS.md:56` | `apache_builds` | `N-direct` | `NO-GO` | digest | 8306 | 7796 | 8852 | 6750 | `criterion-slope-profile:json_apache_builds/track1_direct_to_struct/new/estimates.json` | `absent:sk-v9-open-w0-telemetry-lock-not-produced` |
| `skinny/RESULTS.md:15`; manifest `skinny/RESULTS.md:58` | `github_events` | `N-direct` | `NO-GO` | digest | 9088 | 7337 | 9818 | 8152 | `criterion-slope-profile:json_github_events/track1_direct_to_struct/new/estimates.json` | `absent:sk-v9-open-w0-telemetry-lock-not-produced` |
| `skinny/RESULTS.md:17`; manifest `skinny/RESULTS.md:60` | `update_center` | `N-direct` | `NO-GO` | digest | 7863 | 7514 | 10525 | 8218 | `criterion-slope-profile:json_update_center/track1_direct_to_struct/new/estimates.json` | `absent:sk-v9-open-w0-telemetry-lock-not-produced` |
| `skinny/RESULTS.md:20`; manifest `skinny/RESULTS.md:63` | `mesh` | `N-direct` | `NO-GO` | digest | 8640 | 9049 | 9967 | 7176 | `criterion-slope-profile:json_mesh/track1_direct_to_struct/new/estimates.json` | `absent:sk-v9-open-w0-telemetry-lock-not-produced` |
| `skinny/RESULTS.md:23`; manifest `skinny/RESULTS.md:66` | `random` | `N-direct` | `NO-GO` | digest | 7751 | 6952 | 8141 | 5922 | `criterion-slope-profile:json_random/track1_direct_to_struct/new/estimates.json` | `absent:sk-v9-open-w0-telemetry-lock-not-produced` |
| `skinny/RESULTS.md:25`; manifest `skinny/RESULTS.md:68` | `gsoc-2018` | `N-direct` | `NO-GO` | digest | 15042 | 14380 | 23356 | 19398 | `criterion-slope-profile:json_gsoc-2018/track1_direct_to_struct/new/estimates.json` | `absent:sk-v9-open-w0-telemetry-lock-not-produced` |
| `skinny/RESULTS.md:27`; manifest `skinny/RESULTS.md:70` | `marine_ik` | `A` | `GO` | digest | 9357 | 9488 | 8559 | 7018 | `criterion-slope-profile:json_marine_ik/track1_direct_to_struct/new/estimates.json` | `absent:sk-v9-open-w0-telemetry-lock-not-produced` |
| `skinny/RESULTS.md:30`; manifest `skinny/RESULTS.md:73` | `instruments` | `N-direct` | `NO-GO` | digest | 8494 | 8766 | 9872 | 7576 | `criterion-slope-profile:json_instruments/track1_direct_to_struct/new/estimates.json` | `absent:sk-v9-open-w0-telemetry-lock-not-produced` |
| `skinny/RESULTS.md:32`; manifest `skinny/RESULTS.md:75` | `numbers` | `N-direct` | `NO-GO` | digest | 9773 | 6966 | 7953 | 5753 | `criterion-slope-profile:json_numbers/track1_direct_to_struct/new/estimates.json` | `absent:sk-v9-open-w0-telemetry-lock-not-produced` |
| `skinny/RESULTS.md:34`; manifest `skinny/RESULTS.md:77` | `unicode_mixed` | `N-direct` | `NO-GO` | digest | 3596 | 3694 | 10077 | 4911 | `criterion-slope-profile:json_unicode_mixed/track1_direct_to_struct/new/estimates.json` | `absent:sk-v9-open-w0-telemetry-lock-not-produced` |
| `skinny/RESULTS.md:36`; manifest `skinny/RESULTS.md:79` | `unicode_escapes` | `N-direct` | `NO-GO` | digest | 4020 | 4016 | 13999 | 3720 | `criterion-slope-profile:json_unicode_escapes/track1_direct_to_struct/new/estimates.json` | `absent:sk-v9-open-w0-telemetry-lock-not-produced` |
| `skinny/RESULTS.md:38`; manifest `skinny/RESULTS.md:81` | `unicode_basic` | `A` | `GO` | digest | 9363 | 8420 | 8971 | 6002 | `criterion-slope-profile:json_unicode_basic/track1_direct_to_struct/new/estimates.json` | `absent:sk-v9-open-w0-telemetry-lock-not-produced` |
| `skinny/RESULTS.md:40`; manifest `skinny/RESULTS.md:83` | `distinct_values` | `N-direct` | `NO-GO` | digest | 4438 | 4151 | 8950 | 5598 | `criterion-slope-profile:json_distinct_values/track1_direct_to_struct/new/estimates.json` | `absent:sk-v9-open-w0-telemetry-lock-not-produced` |
| `skinny/RESULTS.md:42`; manifest `skinny/RESULTS.md:85` | `y_string_unicode` | `N-direct` | `NO-GO` | digest | 4828 | 3563 | 9065 | 7599 | `criterion-slope-profile:json_y_string_unicode/track1_direct_to_struct/new/estimates.json` | `absent:sk-v9-open-w0-telemetry-lock-not-produced` |

The direct table makes no hot-leaf symbol claim. The only visible hot-leaf field
in `RESULTS.md` is the Criterion binding string on each cited row, and PASS-1
requires a samply symbol path plus percent self-time before claiming a resolved
hot leaf (`restart/prompts/skinny/PASS-1-PROFILE.md:123-127`,
`restart/prompts/skinny/PASS-1-PROFILE.md:155-160`).

### Real Typed Row Inventory

| Source | Corpus | Measured row state | Product source state | Plane | Track 1 Mbps | Track 2 Mbps | sonic strict Mbps | serde_json Mbps | SK-V9 samply symbol status |
|---|---|---|---|---|---:|---:|---:|---:|---|
| `skinny/RESULTS.md:7`; manifest `skinny/RESULTS.md:50` | `twitter` | `A / GO` | supported by current fixture map | typed direct | 15333 | 14516 | 13646 | 15046 | `absent:sk-v9-open-w0-telemetry-lock-not-produced` |
| `skinny/RESULTS.md:absent`; REDRESS `skinny/REDRESS.md:2622-2659` | `citm_catalog` | `absent:source-product-only-not-measured-row` | source/product parity admitted; fixture exists in `skinny/crates/bbnf-bench/src/real_typed_struct.rs:182-191` and schema root exists in `skinny/xtask/src/real_typed_schema.rs:27-30` | typed direct | `absent:source-product-only-not-measured-row` | `absent:source-product-only-not-measured-row` | `absent:source-product-only-not-measured-row` | `absent:source-product-only-not-measured-row` | `absent:sk-v9-open-w0-telemetry-lock-not-produced` |
| `skinny/RESULTS.md:absent`; REDRESS `skinny/REDRESS.md:2637-2640`; SYNTHESIS `restart/skinny/tranches/sk-v9/SYNTHESIS.md:218` | `canada` | `absent:rejected-full-fixture-checksum-mismatch` | pre-blocked until fresh full-fixture DirectBuild-vs-serde checksum proof | `absent:rejected-full-fixture-checksum-mismatch` | `absent:rejected-full-fixture-checksum-mismatch` | `absent:rejected-full-fixture-checksum-mismatch` | `absent:rejected-full-fixture-checksum-mismatch` | `absent:rejected-full-fixture-checksum-mismatch` | `absent:sk-v9-open-w0-telemetry-lock-not-produced` |
| `skinny/RESULTS.md:absent`; REDRESS `skinny/REDRESS.md:2622-2659` | `apache_builds` | `absent:source-product-only-not-measured-row` | source/product parity admitted; fixture exists in `skinny/crates/bbnf-bench/src/real_typed_struct.rs:182-191` and schema root exists in `skinny/xtask/src/real_typed_schema.rs:22-25` | typed direct | `absent:source-product-only-not-measured-row` | `absent:source-product-only-not-measured-row` | `absent:source-product-only-not-measured-row` | `absent:source-product-only-not-measured-row` | `absent:sk-v9-open-w0-telemetry-lock-not-produced` |
| `skinny/RESULTS.md:absent`; source `skinny/crates/bbnf-bench/src/real_typed_struct.rs:182-191` | `github_events` | `absent:unsupported-real-typed-fixture` | no current real typed fixture mapping | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` |
| `skinny/RESULTS.md:18`; manifest `skinny/RESULTS.md:61` | `update_center` | `A / GO` | supported by current fixture map | typed direct | 11958 | 10367 | 11952 | 10296 | `absent:sk-v9-open-w0-telemetry-lock-not-produced` |
| `skinny/RESULTS.md:21`; manifest `skinny/RESULTS.md:64` | `mesh` | `A / GO` | supported by current fixture map | typed direct | 9623 | 7674 | 9305 | 8212 | `absent:sk-v9-open-w0-telemetry-lock-not-produced` |
| `skinny/RESULTS.md:absent`; source `skinny/crates/bbnf-bench/src/real_typed_struct.rs:182-191` | `random` | `absent:unsupported-real-typed-fixture` | no current real typed fixture mapping | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` |
| `skinny/RESULTS.md:absent`; source `skinny/crates/bbnf-bench/src/real_typed_struct.rs:182-191` | `gsoc-2018` | `absent:unsupported-real-typed-fixture` | no current real typed fixture mapping | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` |
| `skinny/RESULTS.md:28`; manifest `skinny/RESULTS.md:71` | `marine_ik` | `A / GO` | supported by current fixture map | typed direct | 11783 | 8321 | 6951 | 7450 | `absent:sk-v9-open-w0-telemetry-lock-not-produced` |
| `skinny/RESULTS.md:absent`; source `skinny/crates/bbnf-bench/src/real_typed_struct.rs:182-191` | `instruments` | `absent:unsupported-real-typed-fixture` | no current real typed fixture mapping | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` |
| `skinny/RESULTS.md:absent`; source `skinny/crates/bbnf-bench/src/real_typed_struct.rs:182-191` | `numbers` | `absent:unsupported-real-typed-fixture` | no current real typed fixture mapping | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` |
| `skinny/RESULTS.md:absent`; source `skinny/crates/bbnf-bench/src/real_typed_struct.rs:182-191` | `unicode_mixed` | `absent:unsupported-real-typed-fixture` | no current real typed fixture mapping | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` |
| `skinny/RESULTS.md:absent`; source `skinny/crates/bbnf-bench/src/real_typed_struct.rs:182-191` | `unicode_escapes` | `absent:unsupported-real-typed-fixture` | no current real typed fixture mapping | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` |
| `skinny/RESULTS.md:absent`; source `skinny/crates/bbnf-bench/src/real_typed_struct.rs:182-191` | `unicode_basic` | `absent:unsupported-real-typed-fixture` | no current real typed fixture mapping | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` |
| `skinny/RESULTS.md:absent`; source `skinny/crates/bbnf-bench/src/real_typed_struct.rs:182-191` | `distinct_values` | `absent:unsupported-real-typed-fixture` | no current real typed fixture mapping | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` |
| `skinny/RESULTS.md:absent`; source `skinny/crates/bbnf-bench/src/real_typed_struct.rs:182-191` | `y_string_unicode` | `absent:unsupported-real-typed-fixture` | no current real typed fixture mapping | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` | `absent:unsupported-real-typed-fixture` |

### Apache/CITM Typed Row Gap Classification

Gap class: `source-product-parity-without-measured-row-table-admission`.

REDRESS 91 admits the Apache/CITM typed source slice, but states those rows are
not present as measured rows in the W0 `skinny/RESULTS.md` manifest
(`skinny/REDRESS.md:2622-2625`). It also states `skinny/RESULTS.md` is unchanged,
row-table admission was rejected because local Criterion metadata failed the W0
run-id validator, and the wave does not claim six measured
`real_typed_struct A / GO` rows (`skinny/REDRESS.md:2648-2652`). SK-V9 may admit
Apache/CITM measured typed rows only with fresh run-id/metadata validation,
generated Track 1 DirectBuild, independent serde/oracle proof, sonic parity, and
rendered `A / GO` rows (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:216-217`).
The candidate shortlist repeats that source evidence alone must not enter
`RESULTS.md`; same-run metadata, sample count 100, input hash/bytes, checksum
parity, and `sonic_rs_strict` on the typed direct plane are required
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:83-115`).

## §3 - Delta vs SK-V8 (per row; Mbps + c/B + classification)

No SK-V9-open samply or Criterion rerun exists in this artifact. Therefore every
SK-V9-vs-SK-V8 delta, c/B measurement, and top-symbol self-time value for mode II
is `absent:sk-v9-open-w0-telemetry-lock-not-produced`.

The opening row table still exposes W0 Criterion sample cost as `ns_per_byte`,
`track1_ns`, and bytes in the manifest for each direct and measured typed row
(`skinny/RESULTS.md:49-85`). PASS-1 requires real PMU counters for c/B rather
than estimates (`restart/prompts/skinny/PASS-1-PROFILE.md:123-127`), so this file
does not convert ns/B to c/B.

| Row family | Rows | Opening Mbps authority | SK-V9 delta | c/B | Classification |
|---|---:|---|---|---|---|
| `direct_to_struct` measured rows | 17 | `skinny/RESULTS.md:6`, `skinny/RESULTS.md:9`, `skinny/RESULTS.md:11`, `skinny/RESULTS.md:13`, `skinny/RESULTS.md:15`, `skinny/RESULTS.md:17`, `skinny/RESULTS.md:20`, `skinny/RESULTS.md:23`, `skinny/RESULTS.md:25`, `skinny/RESULTS.md:27`, `skinny/RESULTS.md:30`, `skinny/RESULTS.md:32`, `skinny/RESULTS.md:34`, `skinny/RESULTS.md:36`, `skinny/RESULTS.md:38`, `skinny/RESULTS.md:40`, `skinny/RESULTS.md:42` | `absent:sk-v9-open-w0-telemetry-lock-not-produced` | `absent:no-pmu-counters-in-p1b-opening-artifact` | 3 `A / GO`, 14 `N-direct / NO-GO` |
| `real_typed_struct` measured rows | 4 | `skinny/RESULTS.md:7`, `skinny/RESULTS.md:18`, `skinny/RESULTS.md:21`, `skinny/RESULTS.md:28` | `absent:sk-v9-open-w0-telemetry-lock-not-produced` | `absent:no-pmu-counters-in-p1b-opening-artifact` | 4 `A / GO` |
| Apache/CITM source/product typed gap | 2 | `absent:source-product-only-not-measured-row` | `absent:source-product-only-not-measured-row` | `absent:source-product-only-not-measured-row` | pre-admission gap, not measured row progress |
| Other real typed corpus statuses | 11 | 1 `absent:rejected-full-fixture-checksum-mismatch`; 10 `absent:unsupported-real-typed-fixture` | `absent:no-measured-real-typed-row` | `absent:no-measured-real-typed-row` | unsupported or rejected, not estimated |

## §4 - Anomalies + masking signals (flagged for S-P2)

1. Fresh SK-V9-open samply symbols are absent. This is not a hot-leaf close.
   PASS-1 rejects any self-report of profiling without resolvable samply artifacts
   and symbols (`restart/prompts/skinny/PASS-1-PROFILE.md:155-160`).
2. Direct `A / GO` rows remain digest guard-plane evidence, not product-plane
   typed proof. SK-V9 requires a direct output contract or control-path tranche
   before direct misses become product-capable (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:178-181`).
3. Apache/CITM typed rows are source/product parity only. Treating them as
   measured rows would reopen REDRESS 91 under another name
   (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:303-314`).
4. `canada/real_typed_struct` remains pre-blocked until fresh full-fixture
   DirectBuild-vs-serde checksum proof exists (`skinny/REDRESS.md:2637-2640`;
   `restart/skinny/tranches/sk-v9/SYNTHESIS.md:216-218`).
5. C++ sidecars are historical or absent in the current authority; native Rust
   comparators are same-run in W0, but C++ sidecars cannot act as strict anchors
   until a structured same-run sidecar manifest exists (`skinny/RESULTS.md:141`;
   `restart/skinny/tranches/sk-v9/SYNTHESIS.md:220-240`).

## §5 - Sources (every artefact path + run id)

- `skinny/RESULTS.md:3-4`: main result schema.
- `skinny/RESULTS.md:5-42`: current measured main rows.
- `skinny/RESULTS.md:44-85`: W0 telemetry manifest with row ids, run id
  `sk-v8-open:criterion-fnv64-9a37562ed3d0383a`, profile artifact strings,
  sample costs, build flags, host triple, feature mask, substrate, consumer, and
  comparator evidence.
- `skinny/RESULTS.md:138-141`: overall `N-direct / NoGo`, Track 1/Track 2 role
  notes, and W0 comparator freshness statement.
- `skinny/REDRESS.md:2620-2659`: REDRESS 91 typed product-plane source admission,
  Apache/CITM measured-row rejection, Canada typed rejection, and unchanged
  `RESULTS.md`.
- `restart/skinny/tranches/sk-v9/HANDOFF.md:24-37`: current state and row-family
  counts.
- `restart/skinny/tranches/sk-v9/HANDOFF.md:45-52`: Apache/CITM, direct, and
  SK-V9-open telemetry boundaries.
- `restart/skinny/tranches/sk-v9/SYNTHESIS.md:31-45`: opening state and candidate
  boundaries.
- `restart/skinny/tranches/sk-v9/SYNTHESIS.md:178-218`: direct targets, current
  GO rows, and typed row-table candidates.
- `restart/skinny/tranches/sk-v9/SYNTHESIS.md:220-292`: strict comparator and
  telemetry binding fields.
- `restart/prompts/skinny/PASS-1-PROFILE.md:88-110`: required P1 frontmatter and
  section shape.
- `restart/prompts/skinny/PASS-1-PROFILE.md:239-266`: bbnf-lang S-P1 profiling
  discipline.
- `skinny/crates/bbnf-bench/src/bin/profile_direct.rs:57-93`: direct and real
  typed profiling mode dispatch surface.
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs:9-17`: current real typed
  fixture enum.
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs:182-191`: current real typed
  fixture name map.
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs:225-324`: generated Track 1,
  serde-backed Track 2/oracle, sonic, and checksum parity functions.
- `skinny/xtask/src/real_typed_schema.rs:7-42`: generated real typed schema roots,
  including Apache/CITM source roots.
