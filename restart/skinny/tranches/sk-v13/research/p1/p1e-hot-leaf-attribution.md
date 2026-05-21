# SK-V13 P1-E: Hot-Leaf Attribution

Pass: S-P1 Profile. Cycle: V13.
Date: 2026-05-21.
Scope: per-corpus hot-leaf synthesis for JSON parse/direct/typed rows plus CSS L4 declaration-values measurement.
Output: this file.
Baseline: SK-V13-open capture identity `f8be692068e9e464b6ed24027ab26edfd05303fd` (`/tmp/skv13-p1/artifacts/identity.txt`).
Host triple: aarch64-apple-darwin; samply profile metadata reports macOS 26.4.1.
Build flags: release binaries under `/tmp/skv13-profile-target-0a7b41c5/release`; explicit build RUSTFLAGS were not recorded in the capture scripts.
Profile tool: samply JSON plus `.json.syms.json` sidecars; PMU from proc_pid_rusage rows; CSS measurement TSV.
Corpus coverage: JSON parse 17/17; JSON direct 17/17 PMU but 0/17 valid samply hot-leaf profiles; typed 7/17 by supported typed-row subset; CSS measurement 1 row, no samply profile.

## §1 — Method (commands run; verbatim, reproducible)

Read the S-P1 contract and tranche context:

```bash
sed -n '1,220p' restart/prompts/skinny/PASS-1-PROFILE.md
sed -n '1,220p' skinny/RESULTS.md
sed -n '1,220p' skinny/REDRESS.md
sed -n '1,260p' restart/skinny/tranches/sk-v13/HANDOFF.md
sed -n '1,240p' restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-profile-truth.md
sed -n '1,220p' restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md
```

Inspect fresh capture and peer outputs:

```bash
find restart/skinny/tranches/sk-v13/research/p1 -maxdepth 2 -type f -print | sort
find /tmp/skv13-p1 -maxdepth 3 -type f -print | sort
head -5 /tmp/skv13-p1/samply/capture_status.tsv
head -5 /tmp/skv13-p1/pmu/pmu_rows.tsv
cat /tmp/skv13-p1/css/css_l4_declaration_values_measurement.tsv
cat /tmp/skv13-p1/artifacts/identity.txt
```

Extract top leaf samples from Firefox-profile JSON and resolve addresses through the sidecar symbol tables:

```bash
node - <<'NODE'
// For each /tmp/skv13-p1/samply/profiles/<row>.json.gz:
// 1. read threads[0].samples.stack;
// 2. map leaf stackTable.frame -> frameTable.address/resource;
// 3. resolve address against <row>.json.syms.json symbol_table;
// 4. aggregate leaf sample weights and report top self-time symbol.
NODE
```

Cross-agent note: no P1-A, P1-B, or P1-C artifacts existed under
`restart/skinny/tranches/sk-v13/research/p1/` while this ran, so this artifact
uses raw `/tmp/skv13-p1` captures. Peer synthesis remains fold-cycle-dependent.

## §2 — Findings (per-corpus table; file:line on every hot-leaf claim)

`parse_only` is well-resolved and dominated by generated JSON dispatch in every
corpus. `direct_to_struct` has valid PMU rows but invalid samply workload
profiles: each direct samply log either panics at fixture lookup or fixture
read before the timed loop. `real_typed_struct` exists only for the seven
supported typed corpora in the capture script.

| Corpus | parse_only Track 1 hot leaf (% self, class, source) | direct_to_struct Track 1 hot leaf | real_typed_struct Track 1 hot leaf |
|---|---|---|---|
| twitter | `runtime::generated_json::generated::dispatch_value`, 97.3%, dispatch, `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs:46` | unprofiled: direct samply panic; PMU 2.980 c/B | `<bbnf_bench::generated_real_typed::DirectParser>::skip_value`, 72.1%, dispatch, `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/generated_real_typed.rs:1739` |
| citm_catalog | `runtime::generated_json::generated::dispatch_value`, 98.9%, dispatch, `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs:45` | unprofiled: direct samply panic; PMU 1.615 c/B | `<bbnf_bench::generated_real_typed::DirectParser>::skip_value`, 77.9%, dispatch, `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/generated_real_typed.rs:1739` |
| canada | `runtime::generated_json::generated::dispatch_value`, 99.5%, dispatch, `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs:45` | unprofiled: direct samply panic; PMU 3.252 c/B | unprofiled: typed row not captured |
| apache_builds | `runtime::generated_json::generated::dispatch_value`, 100.0%, dispatch, `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs:46` | unprofiled: direct samply panic; PMU 3.086 c/B | `bbnf_bench::generated_real_typed::parse_option_scalar_string`, 43.7%, string, `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/generated_real_typed.rs:1199` |
| github_events | `runtime::generated_json::generated::dispatch_value`, 87.5%, dispatch, `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs:49` | unprofiled: direct samply panic; PMU 2.839 c/B | `<bbnf_bench::generated_real_typed::DirectParser>::skip_value`, 38.8%, dispatch, `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/generated_real_typed.rs:1740` |
| update_center | `runtime::generated_json::generated::dispatch_value`, 98.7%, dispatch, `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs:45` | unprofiled: direct samply panic; PMU 4.142 c/B | `bbnf_bench::generated_real_typed::parse_type_plugin`, 48.0%, structural, `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/generated_real_typed.rs:473` |
| mesh | `runtime::generated_json::generated::dispatch_value`, 97.8%, dispatch, `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs:45` | unprofiled: direct samply panic; PMU 3.890 c/B | `bbnf_bench::generated_real_typed::parse_type_mesh`, 40.3%, structural, `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/generated_real_typed.rs:828` |
| random | `runtime::generated_json::generated::dispatch_value`, 98.9%, dispatch, `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs:45` | unprofiled: direct samply panic; PMU 4.436 c/B | unprofiled: typed row not captured |
| gsoc-2018 | `runtime::generated_json::generated::dispatch_value`, 99.6%, dispatch, `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs:45` | unprofiled: direct samply panic; PMU 2.920 c/B | unprofiled: typed row not captured |
| marine_ik | `runtime::generated_json::generated::dispatch_value`, 99.7%, dispatch, `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs:45` | unprofiled: direct samply panic; PMU 3.684 c/B | `bbnf_bench::generated_real_typed::parse_type_marine_geometry_data`, 41.6%, structural, `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/generated_real_typed.rs:1015` |
| instruments | `runtime::generated_json::generated::dispatch_value`, 90.9%, dispatch, `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs:46` | unprofiled: direct samply panic; PMU 2.870 c/B | unprofiled: typed row not captured |
| numbers | `runtime::generated_json::generated::dispatch_value`, 100.0%, dispatch, `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs:45` | unprofiled: direct samply panic; PMU 2.775 c/B | unprofiled: typed row not captured |
| unicode_mixed | `runtime::generated_json::generated::dispatch_value`, 98.7%, dispatch, `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs:45` | unprofiled: direct samply panic; PMU 7.537 c/B | unprofiled: typed row not captured |
| unicode_escapes | `runtime::generated_json::generated::dispatch_value`, 98.8%, dispatch, `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs:45` | unprofiled: direct samply panic; PMU 6.834 c/B | unprofiled: typed row not captured |
| unicode_basic | `runtime::generated_json::generated::dispatch_value`, 98.6%, dispatch, `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs:45` | unprofiled: direct samply panic; PMU 3.789 c/B | unprofiled: typed row not captured |
| distinct_values | `runtime::generated_json::generated::dispatch_value`, 92.6%, dispatch, `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs:49` | unprofiled: direct samply panic; PMU 5.513 c/B | unprofiled: typed row not captured |
| y_string_unicode | `runtime::generated_json::generated::dispatch_value`, 100.0%, dispatch, `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs:49` | unprofiled: direct samply panic; PMU 10.621 c/B | unprofiled: typed row not captured |

CSS L4 row:

| Row | Hot leaf | Classification | Evidence |
|---|---|---|---|
| `css_l4/declaration_values/direct_to_struct/main` | unprofiled: no samply/xctrace hot-leaf artifact under `/tmp/skv13-p1/samply/profiles` | CSS unresolved | Measurement TSV reports strict equality pass and throughput only: Track 1 48.862573 Mbps, cssparser 24.333970 Mbps, lightningcss 17.592299 Mbps, 187 bytes, 100000 iterations (`/tmp/skv13-p1/css/css_l4_declaration_values_measurement.tsv`). |

## §3 — Delta vs SK-V12 (per row; Mbps + c/B + classification)

No SK-V12 samply hot-leaf artifact exists for a like-for-like symbol delta in
the checked tree. The SK-V13 scoping audit states the prior PMU TSV is pre-pin
JSON-only and that CSS L4 had no samply/xctrace capture. Therefore hot-leaf
delta is not computable from committed prior profile artifacts.

The fresh PMU rows do provide current c/B context: parse Track 1 ranges from
1.135 c/B (`citm_catalog`) to 5.674 c/B (`y_string_unicode`); direct Track 1
ranges from 1.615 c/B (`citm_catalog`) to 10.621 c/B
(`y_string_unicode`). The unresolved direct samply cells are the blocker for
per-symbol delta, not the PMU rows.

| Corpus | parse_only T1 Mbps | parse_only c/B | parse hot-leaf class | direct_to_struct T1 Mbps | direct c/B | direct hot-leaf class |
|---|---:|---:|---|---:|---:|---|
| twitter | 15093.534 | 2.256 | dispatch | 11433.616 | 2.980 | unprofiled |
| citm_catalog | 30057.320 | 1.136 | dispatch | 21099.300 | 1.615 | unprofiled |
| canada | 17413.747 | 1.941 | dispatch | 10508.989 | 3.252 | unprofiled |
| apache_builds | 11999.703 | 2.822 | dispatch | 10895.295 | 3.086 | unprofiled |
| github_events | 13476.315 | 2.407 | dispatch | 12009.588 | 2.839 | unprofiled |
| update_center | 11102.273 | 3.058 | dispatch | 8196.715 | 4.142 | unprofiled |
| mesh | 13020.341 | 2.632 | dispatch | 8778.456 | 3.890 | unprofiled |
| random | 9847.224 | 3.482 | dispatch | 7607.363 | 4.436 | unprofiled |
| gsoc-2018 | 18942.648 | 1.599 | dispatch | 9982.229 | 2.920 | unprofiled |
| marine_ik | 13000.471 | 2.635 | dispatch | 9238.420 | 3.684 | unprofiled |
| instruments | 17118.927 | 2.014 | dispatch | 11960.749 | 2.870 | unprofiled |
| numbers | 18568.751 | 1.868 | dispatch | 12369.310 | 2.775 | unprofiled |
| unicode_mixed | 7301.893 | 4.711 | dispatch | 4558.814 | 7.537 | unprofiled |
| unicode_escapes | 10518.310 | 3.264 | dispatch | 5018.257 | 6.834 | unprofiled |
| unicode_basic | 11701.999 | 2.920 | dispatch | 9038.879 | 3.789 | unprofiled |
| distinct_values | 9361.440 | 3.664 | dispatch | 6255.739 | 5.513 | unprofiled |
| y_string_unicode | 6080.632 | 5.674 | dispatch | 3232.485 | 10.621 | unprofiled |

## §4 — Anomalies + masking signals (flagged for S-P2)

- CH6 direct profile risk: all 34 JSON direct samply logs contain a panic, so
  direct rows remain `unprofiled` at symbol level despite valid PMU. The
  non-`update_center` failure is fixture lookup at
  `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/bin/profile_direct.rs:204`;
  `update_center` passes a quoted absolute path and fails reading it at
  `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/bin/profile_direct.rs:91`.
- CH6 CSS profile risk: CSS L4 has throughput/equality measurement but no
  hot-leaf profile artifact. The admitted CSS row cannot yet be attributed to
  `CSS`-class leaves.
- CH4 reproducibility risk: `/tmp/skv13-p1/samply/run-samply.sh` and
  `/tmp/skv13-p1/pmu/run-pmu.sh` identify release binaries but do not record
  the build command or `RUSTFLAGS`; the capture identity records only
  `bin=/tmp/skv13-profile-target-0a7b41c5/release` and commit
  `f8be692068e9e464b6ed24027ab26edfd05303fd`.
- Masking signal: `parse_only` is not a scan/number/string attribution at this
  granularity; it resolves to generated dispatch in the sampled leaf view. That
  means any S-P2 primitive aimed at unicode/number/string must first obtain a
  deeper direct or typed profile rather than infer it from parse-only PMU.

## §5 — Sources (every artefact path + run id)

- `/tmp/skv13-p1/artifacts/identity.txt`: root, binary directory, capture commit, capture date.
- `/tmp/skv13-p1/samply/capture_status.tsv`: samply row inventory and commands.
- `/tmp/skv13-p1/samply/run-samply.sh`: samply capture script.
- `/tmp/skv13-p1/samply/profiles/*.json.gz`: Firefox-profile JSON profiles.
- `/tmp/skv13-p1/samply/profiles/*.json.syms.json`: symbol sidecars used for file:line attribution.
- `/tmp/skv13-p1/samply/logs/*.log`: workload success/panic evidence.
- `/tmp/skv13-p1/pmu/pmu_rows.tsv`: Mbps, cycles, instructions, cycles-per-byte, CPI.
- `/tmp/skv13-p1/pmu/run-pmu.sh`: PMU capture script.
- `/tmp/skv13-p1/css/css_l4_declaration_values_measurement.tsv`: CSS L4 strict-equality and throughput measurement.
- `restart/prompts/skinny/PASS-1-PROFILE.md`: S-P1/P1-E output schema and CH1-CH6 requirements.
- `skinny/RESULTS.md`: current bench-gate authority and prior `criterion-slope-profile` hot-leaf placeholders.
- `skinny/REDRESS.md`: rejected-route and current bench-fact ledger.
- `restart/skinny/tranches/sk-v13/HANDOFF.md`: SK-V13 sequencing and no-source-edit constraints.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-profile-truth.md`: fresh profile requirements and prior-profile staleness.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md`: CSS L4 row scope and parity gap.
