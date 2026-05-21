# SK-V13 P1-B: Samply Mode II Product-Plane Profile

Pass: S-P1 Profile. Cycle: V13.
Date: 2026-05-21.
Scope: samply profiling mode II, cold per-parse `direct_to_struct` plus `real_typed_struct` workloads.
Output: this file.
Baseline: SK-V13-open (f8be692068e9e464b6ed24027ab26edfd05303fd).
Host triple: aarch64-apple-darwin.
Build flags: release profile, debug=true expectation; profiling binary root `/tmp/skv13-profile-target-0a7b41c5/release`; capture identity records no complete build command or RUSTFLAGS.
Profile tool: samply 0.13.1, `samply record --save-only --unstable-presymbolicate -r 1000`; PMU/proc_pid_rusage cross-check from `/tmp/skv13-p1/pmu/pmu_rows.tsv`.
Corpus coverage: direct PMU 17/17; direct samply 0/17 valid hot-leaf profiles because the workload panicked before timed parsing; typed samply/PMU 7/17 supported generated typed rows.

## §1 - Method (commands run; verbatim, reproducible)

Identity:

```bash
cat /tmp/skv13-p1/artifacts/identity.txt
# root=/tmp/skv13-p1
# bin=/tmp/skv13-profile-target-0a7b41c5/release
# commit=f8be692068e9e464b6ed24027ab26edfd05303fd
# date=2026-05-21T06:01:45Z
```

Direct and typed samply capture inventory:

```bash
awk -F '\t' 'NR==1 || $1=="direct" || $1=="typed" {print}' \
  /tmp/skv13-p1/samply/capture_status.tsv

sed -n '1,80p' /tmp/skv13-p1/samply/logs/direct__twitter__track1.log
sed -n '1,80p' /tmp/skv13-p1/samply/logs/typed__twitter__real_typed_track1.log
```

PMU cross-check:

```bash
awk -F '\t' 'NR==1 || ($1=="direct" && ($3=="track1" || $3=="track2" || $3=="sonic")) || \
  ($1=="typed" && ($3=="real_typed_track1" || $3=="real_typed_track2" || $3=="real_typed_sonic")) {print}' \
  /tmp/skv13-p1/pmu/pmu_rows.tsv
```

Representative samply commands from `/tmp/skv13-p1/samply/capture_status.tsv`:

```bash
samply record --save-only --unstable-presymbolicate -r 1000 \
  -o /tmp/skv13-p1/samply/profiles/direct__twitter__track1.json.gz \
  /tmp/skv13-profile-target-0a7b41c5/release/profile_direct 3000 twitter track1

samply record --save-only --unstable-presymbolicate -r 1000 \
  -o /tmp/skv13-p1/samply/profiles/typed__twitter__real_typed_track1.json.gz \
  /tmp/skv13-profile-target-0a7b41c5/release/profile_direct 2000 twitter real_typed_track1
```

The direct samply status rows have `rc=0` at the samply wrapper level, but
their workload logs show `profile_direct` panicked before the timed loop. The
PMU direct rows are valid because those were captured through `xctrace_probe`,
not the failing samply direct wrapper.

## §2 - Findings (per-corpus table; file:line on every hot-leaf claim)

No direct-to-struct self-time symbol is admitted from the samply profiles in
this cycle. Every direct samply log for named corpora reports:

```text
thread 'main' panicked at crates/bbnf-bench/src/bin/profile_direct.rs:204:5:
could not locate fixture '<corpus>'.json under crates/test-fixtures/corpus/json
```

`update_center` fails through the quoted absolute-path variant at
`skinny/crates/bbnf-bench/src/bin/profile_direct.rs:91`. Typed profiles are
valid for the seven generated typed rows and line up with P1-E's sidecar symbol
extraction.

| Corpus | Direct samply profile | Direct PMU Track 1 / Track 2 / sonic Mbps | Direct c/B T1/T2 | Typed samply profile | Typed PMU Track 1 / Track 2 / sonic Mbps | Typed hot leaf status |
|---|---|---:|---:|---|---:|---|
| twitter | invalid: fixture lookup panic | 11433.616 / 10482.591 / 10284.590 | 2.979732 / 3.243479 | valid `typed__twitter__real_typed_track{1,2}` | 18492.440 / 16187.169 / 15303.652 | `DirectParser::skip_value`, file:line resolved in P1-E |
| citm_catalog | invalid: fixture lookup panic | 21099.300 / 20057.601 / 14852.637 | 1.615142 / 1.697828 | valid `typed__citm_catalog__real_typed_track{1,2}` | 35379.335 / 19178.971 / 21888.586 | `DirectParser::skip_value`, file:line resolved in P1-E |
| canada | invalid: fixture lookup panic | 10508.989 / 10201.315 / 12173.527 | 3.252040 / 3.340186 | not captured | n/a | typed row absent from generated typed set |
| apache_builds | invalid: fixture lookup panic | 10895.295 / 10063.830 / 9297.831 | 3.085860 / 3.364679 | valid `typed__apache_builds__real_typed_track{1,2}` | 8549.791 / 5717.341 / 6695.326 | `parse_option_scalar_string`, file:line resolved in P1-E |
| github_events | invalid: fixture lookup panic | 12009.588 / 11048.461 / 11539.159 | 2.839007 / 3.085457 | valid `typed__github_events__real_typed_track{1,2}` | 12484.918 / 11284.802 / 11643.157 | `DirectParser::skip_value`, file:line resolved in P1-E |
| update_center | invalid: quoted-path read panic | 8196.715 / 7320.996 / 8033.542 | 4.142268 / 4.645216 | valid `typed__update_center__real_typed_track{1,2}` | 12131.792 / 9666.001 / 11398.226 | `parse_type_plugin`, file:line resolved in P1-E |
| mesh | invalid: fixture lookup panic | 8778.456 / 8440.766 / 9632.793 | 3.889578 / 4.046466 | valid `typed__mesh__real_typed_track{1,2}` | 9082.600 / 7108.844 / 8504.545 | `parse_type_mesh`, file:line resolved in P1-E |
| random | invalid: fixture lookup panic | 7607.363 / 6893.381 / 5712.266 | 4.436292 / 4.900058 | not captured | n/a | typed row absent from generated typed set |
| gsoc-2018 | invalid: fixture lookup panic | 9982.229 / 12425.141 / 19948.066 | 2.919522 / 2.568889 | not captured | n/a | typed row absent from generated typed set |
| marine_ik | invalid: fixture lookup panic | 9238.420 / 9241.584 / 7609.643 | 3.683827 / 3.668345 | valid `typed__marine_ik__real_typed_track{1,2}` | 12037.220 / 9349.449 / 8833.256 | `parse_type_marine_geometry_data`, file:line resolved in P1-E |
| instruments | invalid: fixture lookup panic | 11960.749 / 11016.685 / 7960.651 | 2.870026 / 3.111853 | not captured | n/a | typed row absent from generated typed set |
| numbers | invalid: fixture lookup panic | 12369.310 / 12167.361 / 12791.743 | 2.775024 / 2.816313 | not captured | n/a | typed row absent from generated typed set |
| unicode_mixed | invalid: fixture lookup panic | 4558.814 / 4451.886 / 8890.244 | 7.536545 / 7.706815 | not captured | n/a | typed row absent from generated typed set |
| unicode_escapes | invalid: fixture lookup panic | 5018.257 / 4839.622 / 13491.178 | 6.833959 / 7.051697 | not captured | n/a | typed row absent from generated typed set |
| unicode_basic | invalid: fixture lookup panic | 9038.879 / 8116.719 / 6624.874 | 3.789464 / 4.229386 | not captured | n/a | typed row absent from generated typed set |
| distinct_values | invalid: fixture lookup panic | 6255.739 / 5581.609 / 8033.781 | 5.512909 / 6.174830 | not captured | n/a | typed row absent from generated typed set |
| y_string_unicode | invalid: fixture lookup panic | 3232.485 / 2919.435 / 8634.754 | 10.621480 / 11.728215 | not captured | n/a | typed row absent from generated typed set |

Product-plane throughput finding: the fresh direct PMU rows beat same-run
sonic on eight direct rows (`twitter`, `citm_catalog`, `apache_builds`,
`github_events`, `update_center`, `random`, `marine_ik`, `instruments`,
`unicode_basic`) and miss on eight direct rows plus `numbers` by a small
margin. This is not an admission because equality/provenance gate consumption
and valid direct hot-leaf profiles remain separate obligations.

## §3 - Delta vs SK-V12 (per row; Mbps + c/B + classification)

The checked SK-V12 close document and current `skinny/RESULTS.md` are not a
single machine-readable schema for product-plane deltas. This P1-B cycle
therefore records fresh SK-V13-open product-plane values and flags SK-V12
delta as unresolved except where the close narrative gives the same row.

| Plane | Current fresh state | Classification under SK-V13 addendum |
|---|---|---|
| direct_to_struct | 17/17 PMU rows valid; 0/17 valid samply hot-leaf rows; 9/17 Track 1 rows currently exceed same-run sonic by >1 Mbps in the fresh PMU ledger | profile incomplete; row admissions need equality, gate consumption, and direct symbol capture |
| real_typed_struct | 7/17 generated typed rows captured; all 7 exceed same-run sonic Track 1 by >1 Mbps; 10/17 typed rows absent | partial product-plane profile; missing typed rows are explicit G5 obligations |

Rows still below same-run sonic in the fresh direct PMU ledger are:
`canada`, `mesh`, `gsoc-2018`, `numbers`, `unicode_mixed`,
`unicode_escapes`, `distinct_values`, and `y_string_unicode`. The widest
misses are the unicode/string-heavy rows and `gsoc-2018`; those remain the
highest-value S-P2 direct-profile targets once the direct samply wrapper is
fixed.

## §4 - Anomalies + masking signals (flagged for S-P2)

- CH6 direct-profile blocker: direct samply profiles are artifacts on disk, but
  they profile a panic path rather than the parser. Treat them as invalid even
  though `capture_status.tsv` reports `rc=0` at the wrapper level.
- CH1 product-plane gap: direct PMU is strong enough for throughput/cycles
  accounting, but not for file:line hot-leaf attribution. P1-E correctly leaves
  direct leaves unresolved.
- CH2/Lock 14 gap: typed hot leaves live in
  `skinny/crates/bbnf-bench/src/generated_real_typed.rs`, not a
  grammar-neutral runtime surface. S-P2 must not generalize those typed leaves
  to CSS without a separate non-JSON consumer.
- CH4 reproducibility gap: the capture identity records binary root and commit
  but not the full build command. Re-running mode II requires reconstructing
  the profile target build from repository scripts or `/tmp/skv13-p1/*/run-*`.
- CH5 plane separation: Track 1 generated/direct, Track 2 independent oracle,
  and sonic strict sidecar are distinct. The same-run PMU values should not be
  collapsed into a single parser authority.

## §5 - Sources (every artefact path + run id)

- `/tmp/skv13-p1/artifacts/identity.txt`: run root, binary root, baseline commit, timestamp.
- `/tmp/skv13-p1/samply/capture_status.tsv`: samply mode II inventory and commands.
- `/tmp/skv13-p1/samply/logs/direct__*.log`: direct workload panic evidence.
- `/tmp/skv13-p1/samply/logs/typed__*.log`: valid typed workload evidence.
- `/tmp/skv13-p1/samply/profiles/direct__*.json.gz`: invalid direct profile artifacts; do not use for parser hot-leaf attribution.
- `/tmp/skv13-p1/samply/profiles/typed__*.json.gz` and `.json.syms.json`: valid typed profile artifacts and sidecars.
- `/tmp/skv13-p1/pmu/pmu_rows.tsv`: direct and typed Mbps/cycles/cpi/cB rows.
- `/tmp/skv13-p1/pmu/logs/direct__*.log`, `/tmp/skv13-p1/pmu/logs/typed__*.log`: PMU workload logs.
- `restart/prompts/skinny/PASS-1-PROFILE.md`: S-P1/P1-B contract.
- `skinny/RESULTS.md`: current result authority.
- `skinny/REDRESS.md`: route and probe history.
- `restart/skinny/tranches/sk-v13/HANDOFF.md`: SK-V13 pass sequencing.
- `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md`: cross-plane symbol extraction used only where it cites raw sidecar evidence.
