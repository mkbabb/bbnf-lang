# SK-V13 P1-A: Samply Mode I Parse-Only Profile

Pass: S-P1 Profile. Cycle: V13 / S-P1 V2 fold.
Date: 2026-05-21.
Scope: samply profiling mode I, cold per-parse `parse_only` workload, Track 1 generated JSON and Track 2 independent hand JSON parser, all 17 JSON corpora.
Output: this file.
Baseline: SK-V13-open (parse capture `f8be692068e9e464b6ed24027ab26edfd05303fd`; V2 fold head `7ee299096be7d7fdaa0e69344a6cd18bbd55524f`, with no `skinny/crates/` source delta).
Host triple: aarch64-apple-darwin.
Build flags: release profile, debug=true, split-debuginfo=packed, LTO profile per `skinny/Cargo.toml`; profiling binary root `/tmp/skv13-profile-target-0a7b41c5/release`; native target CPU per SK profile scripts.
Profile tool: samply 0.13.1, `samply record --save-only --unstable-presymbolicate -r 1000`; PMU cross-check from proc_pid_rusage rows in `/tmp/skv13-p1/pmu/pmu_rows.tsv`.
Corpus coverage: 17/17.

V2 fold note: the S-P1 V1 challenge did not reject parse corpus coverage; it
rejected direct panic captures, absent mode-III coverage, missing CSS
hot-leaf evidence, and unresolved counter exports. This artefact therefore
retains the V1 parse-only capture as the parse authority, with the limitation
that it remains a save-only/offline-symbolicated samply profile. The source
tree under `skinny/crates/` did not change between the parse capture and the
V2 profile fold.

V3 fold note: parse rows are classified in
`support/evidence-ledger-v3.md` as `json-parse-envelope`,
`function-only-sidecar`, or `resolved-json-unicode-candidate`; no
`dispatch_value` row is grammar-neutral primitive evidence.

## §1 - Method (commands run; verbatim, reproducible)

Identity:

```bash
cat /tmp/skv13-p1/artifacts/identity.txt
# root=/tmp/skv13-p1
# bin=/tmp/skv13-profile-target-0a7b41c5/release
# commit=f8be692068e9e464b6ed24027ab26edfd05303fd
# date=2026-05-21T06:01:45Z
```

Samply parse capture used `/tmp/skv13-p1/samply/run-samply.sh`; the parse lane loop is:

```bash
ROOT=/tmp/skv13-p1
BIN=/tmp/skv13-profile-target-0a7b41c5/release
REPO=/Users/mkbabb/Programming/bbnf-lang
SKINNY="$REPO/skinny"
OUT="$ROOT/samply"

samply record --save-only --unstable-presymbolicate -r 1000 \
  -o "$OUT/profiles/parse__${corpus}__track1.json.gz" \
  "$BIN/xctrace_probe" "$p" track1 200

samply record --save-only --unstable-presymbolicate -r 1000 \
  -o "$OUT/profiles/parse__${corpus}__track2.json.gz" \
  "$BIN/xctrace_probe" "$p" track2 200
```

PMU cross-check used `/tmp/skv13-p1/pmu/run-pmu.sh`; the parse lane loop is:

```bash
ROOT=/tmp/skv13-p1
BIN=/tmp/skv13-profile-target-0a7b41c5/release
REPO=/Users/mkbabb/Programming/bbnf-lang
SKINNY="$REPO/skinny"
OUT="$ROOT/pmu"

(cd "$SKINNY" && "$BIN/xctrace_probe" "$p" track1 40) \
  > "$OUT/logs/parse__${corpus}__track1.log" 2>&1

(cd "$SKINNY" && "$BIN/xctrace_probe" "$p" track2 40) \
  > "$OUT/logs/parse__${corpus}__track2.log" 2>&1
```

Corpus path mapping from both scripts:

```bash
twitter|citm_catalog|canada -> skinny/crates/test-fixtures/corpus/json/${corpus}.json
update_center -> skinny/test_data/update-center.json
all other rows -> skinny/test_data/${corpus}.json
```

Symbol extraction method for this artifact: for each `parse__*__track*.json.gz`, count leaf sample stack entries and join frame addresses to the matching `.json.syms.json` sidecar. Because the capture was `--save-only` and the saved profiles report `"symbolicated": false`, the table below names only symbols extractable from sidecars and flags unresolved/line-poor cells as CH6 risk. This is not a clean interactive `samply record` pass.

## §2 - Findings (per-corpus table; file:line on every hot-leaf claim)

Common run id/build for every row: `skv13-p1/f8be692068e9e464b6ed24027ab26edfd05303fd/2026-05-21T06:01:45Z`; binary root `/tmp/skv13-profile-target-0a7b41c5/release`; release + debug=true. Profile path pattern: `/tmp/skv13-p1/samply/profiles/parse__{corpus}__track{1,2}.json.gz`; sidecar pattern: same basename with `.json.syms.json`.

| Corpus | Profile artifacts | Track 1 top self-time symbols | Track 2 top self-time symbols | PMU cross-check | Anomalies |
|---|---|---|---|---|---|
| twitter | `parse__twitter__track1/2` | `runtime::generated_json::generated::dispatch_value` 97.3% (`skinny/crates/runtime/src/grammars/json/generated.rs:46`) | `<bbnf_bench::track2::json::Parser>::parse_value_at` 97.7% (`skinny/crates/bbnf-bench/src/track2/json.rs:53`) | T1 15093.534 Mbps, 2.256353 c/B; T2 11771.644 Mbps, 2.871984 c/B | save-only profile; minor `std::fs::open_c` sample from harness setup |
| citm_catalog | `parse__citm_catalog__track1/2` | `dispatch_value` 98.9% (`generated.rs:45`) | `parse_value_at` 99.3% (`track2/json.rs:53`) | T1 30057.320 Mbps, 1.135500 c/B; T2 20168.689 Mbps, 1.689439 c/B | save-only profile |
| canada | `parse__canada__track1/2` | `dispatch_value` 99.5% (`generated.rs:45`) | `parse_value_at` 98.7% (`track2/json.rs:53`) | T1 17413.747 Mbps, 1.941305 c/B; T2 16529.453 Mbps, 2.058898 c/B | Track 2 has two 0.4% unresolved leaves; CH6 risk |
| apache_builds | `parse__apache_builds__track1/2` | `dispatch_value` 100.0% (`generated.rs:46`) | `bbnf_bench::track2::json::match_tiny_plain_string` 100.0% (no file:line in sidecar) | T1 11999.703 Mbps, 2.821539 c/B; T2 12095.032 Mbps, 2.849313 c/B | Track 2 symbol lacks file:line; CH6 risk; small sample count |
| github_events | `parse__github_events__track1/2` | `<u16 as core::convert::From<u8>>::from` 87.5% (`core/src/convert/num.rs:82`) | `parse_value_at` 88.9% (`track2/json.rs:53`) | T1 13476.315 Mbps, 2.406529 c/B; T2 12641.874 Mbps, 2.709563 c/B | Track 1 has 12.5% unresolved leaf and only 8 samples; CH6 risk |
| update_center | `parse__update_center__track1/2` | `dispatch_value` 98.7% (`generated.rs:45`) | `parse_value_at` 97.0% (`track2/json.rs:53`) | T1 11102.273 Mbps, 3.058022 c/B; T2 8893.596 Mbps, 3.817302 c/B | Track 2 has unresolved 1.0% leaf and harness `xctrace_probe::main` sample |
| mesh | `parse__mesh__track1/2` | `dispatch_value` 97.8% (`generated.rs:45`) | `parse_value_at` 97.0% (`track2/json.rs:53`) | T1 13020.341 Mbps, 2.632188 c/B; T2 11489.205 Mbps, 2.976912 c/B | Track 2 has unresolved 1.0% leaf and UTF-8 validation leaf |
| random | `parse__random__track1/2` | `dispatch_value` 98.9% (`generated.rs:45`) | `parse_value_at` 98.2% (`track2/json.rs:53`) | T1 9847.224 Mbps, 3.481770 c/B; T2 7725.371 Mbps, 4.408702 c/B | Track 2 has unresolved 0.9% leaf; UTF-8 validation leaf |
| gsoc-2018 | `parse__gsoc-2018__track1/2` | `dispatch_value` 99.6% (`generated.rs:45`) | `parse_value_at` 99.6% (`track2/json.rs:53`) | T1 18942.648 Mbps, 1.599432 c/B; T2 17695.089 Mbps, 1.828107 c/B | save-only profile; minor file-read sample |
| marine_ik | `parse__marine_ik__track1/2` | `dispatch_value` 99.7% (`generated.rs:45`) | `parse_value_at` 99.2% (`track2/json.rs:53`) | T1 13000.471 Mbps, 2.634606 c/B; T2 12598.674 Mbps, 2.719645 c/B | Track 2 has unresolved 0.3% leaf |
| instruments | `parse__instruments__track1/2` | `dispatch_value` 95.5% (`generated.rs:46`); `parse_value_at` 4.5% (`generated.rs:40`) | `parse_value_at` 100.0% (`track2/json.rs:54`) | T1 17118.927 Mbps, 2.014174 c/B; T2 11729.711 Mbps, 2.939354 c/B | small sample count |
| numbers | `parse__numbers__track1/2` | `dispatch_value` 100.0% (`generated.rs:45`) | `parse_value_at` 100.0% (`track2/json.rs:53`) | T1 18568.751 Mbps, 1.867778 c/B; T2 18336.050 Mbps, 1.891456 c/B | very small sample count |
| unicode_mixed | `parse__unicode_mixed__track1/2` | `dispatch_value` 98.7% (`generated.rs:45`) | `parse_value_at` 99.0% (`track2/json.rs:53`) | T1 7301.893 Mbps, 4.711000 c/B; T2 7326.685 Mbps, 4.463165 c/B | Track 1 has two unresolved 0.4% leaves; Track 2 has harness sample |
| unicode_escapes | `parse__unicode_escapes__track1/2` | `dispatch_value` 99.4% (`generated.rs:45`) | `parse_value_at` 99.4% (`track2/json.rs:53`) | T1 10518.310 Mbps, 3.264229 c/B; T2 11229.275 Mbps, 3.054168 c/B | UTF-8/write minor leaves; save-only profile |
| unicode_basic | `parse__unicode_basic__track1/2` | `dispatch_value` 98.6% (`generated.rs:45`) | `parse_value_at` 99.4% (`track2/json.rs:53`) | T1 11701.999 Mbps, 2.919726 c/B; T2 10765.439 Mbps, 3.179719 c/B | Track 1 has unresolved 0.7% leaf and line-poor UTF-8 leaf |
| distinct_values | `parse__distinct_values__track1/2` | `runtime::generated_json::generated::match_tiny_plain_string_with_cap::<16>` 96.3% (no file:line in sidecar) | `parse_value_at` 100.0% (`track2/json.rs:53`) | T1 9361.440 Mbps, 3.664440 c/B; T2 5913.792 Mbps, 5.832820 c/B | Track 1 hot leaf lacks file:line and has unresolved 3.7% leaf; CH6 risk |
| y_string_unicode | `parse__y_string_unicode__track1/2` | `parse_that_regex::read_hex_unit_scalar` 100.0% (`skinny/crates/parse-that-regex/src/lib.rs:946`) | `<core::option::Option<&u8>>::copied` 100.0% (`core/src/option.rs:2141`) | T1 6080.632 Mbps, 5.674081 c/B; T2 5301.732 Mbps, 6.253064 c/B | very small sample count; Track 2 leaf is inlined generic, not semantic parser attribution |

Interpretation boundary: the dominant generated Track 1 profile usually collapses into `dispatch_value`, so this pass identifies the hot generated function envelope, not always the inner primitive. P1-E must not treat that as full primitive attribution without interactive or higher-fidelity symbol capture.

## §3 - Delta vs SK-V12 (per row; Mbps + c/B + classification)

`skinny/RESULTS.md` does not provide a machine-readable SK-V12 parse_only baseline. Its parse table still reports `n/a (no machine-readable SK-V6 baseline in W0b)` in the delta column, so this P1-A artifact cannot honestly compute row deltas vs SK-V12 from checked-in authority. The table below therefore reports the checked-in SK-V13-open parse_only authority vs sonic-rs strict, plus fresh PMU c/B from `/tmp/skv13-p1/pmu/pmu_rows.tsv`.

| Corpus | RESULTS Track 1 Mbps | RESULTS Track 2 Mbps | sonic-rs strict Mbps | Delta vs sonic strict | Classification | Fresh PMU Track 1 c/B | Fresh PMU Track 2 c/B |
|---|---:|---:|---:|---:|---|---:|---:|
| twitter | 13490 | 10867 | 18716 | -27.9% | S / NO-GO | 2.256353 | 2.871984 |
| citm_catalog | 24140 | 18580 | 20645 | +16.9% | S / NO-GO | 1.135500 | 1.689439 |
| canada | 7678 | 7384 | 4302 | +78.5% | S / NO-GO | 1.941305 | 2.058898 |
| apache_builds | 5434 | 5594 | 8919 | -39.1% | S / NO-GO | 2.821539 | 2.849313 |
| github_events | 7026 | 6803 | 12263 | -42.7% | S / NO-GO | 2.406529 | 2.709563 |
| update_center | 5344 | 4242 | 13836 | -61.4% | S / NO-GO | 3.058022 | 3.817302 |
| mesh | 9895 | 8065 | 8980 | +10.2% | S / NO-GO | 2.632188 | 2.976912 |
| random | 4156 | 3689 | 7116 | -41.6% | S / NO-GO | 3.481770 | 4.408702 |
| gsoc-2018 | 9129 | 10590 | 16925 | -46.1% | S / NO-GO | 1.599432 | 1.828107 |
| marine_ik | 10024 | 9428 | 7333 | +36.7% | S / NO-GO | 2.634606 | 2.719645 |
| instruments | 10598 | 7602 | 15207 | -30.3% | S / NO-GO | 2.014174 | 2.939354 |
| numbers | 14464 | 14446 | 10231 | +41.4% | S / NO-GO | 1.867778 | 1.891456 |
| unicode_mixed | 4568 | 3215 | 6942 | -34.2% | S / NO-GO | 4.711000 | 4.463165 |
| unicode_escapes | 4741 | 9398 | 14603 | -67.5% | S / NO-GO | 3.264229 | 3.054168 |
| unicode_basic | 9924 | 9025 | 12757 | -22.2% | S / NO-GO | 2.919726 | 3.179719 |
| distinct_values | 9198 | 6102 | 17080 | -46.2% | S / NO-GO | 3.664440 | 5.832820 |
| y_string_unicode | 6313 | 6023 | 13842 | -54.4% | S / NO-GO | 5.674081 | 6.253064 |

All 17 rows remain `S / NO-GO` in the checked-in report even where Track 1 exceeds sonic-rs strict, because SK-V13 G5 requires the full JSON row/plane to clear strict SOTA accounting rather than preserve the old diagnostic parse concession.

## §4 - Anomalies + masking signals (flagged for S-P2)

- CH6 anti-paper-close risk: samply was run with `--save-only --unstable-presymbolicate`; saved profiles report `symbolicated=false`. Sidecars make many top leaves extractable, but this does not satisfy the prompt's clean interactive symbol-resolution ideal.
- CH1/CH6 line risk: some hot leaves lack file:line in the sidecar (`match_tiny_plain_string`, `match_tiny_plain_string_with_cap::<16>`) or resolve to generic inlined std/core functions. Those rows need interactive samply, xctrace Time Profiler, or addr2line-backed post-processing before S-P2 treats the leaf as precise.
- CH1 coverage: parse_only corpus coverage is complete, 17/17, for both Track 1 and Track 2. The direct and typed lanes in the same samply run are not part of this artifact and had separate panics/quoting issues; they do not invalidate the parse lane evidence here.
- CH5 substrate separation: Track 1 hot leaves come from `runtime::generated_json::*`; Track 2 hot leaves come from `bbnf_bench::track2::json::*`. This artifact keeps those planes separate and does not collapse Track 2 into generated runtime attribution.
- Masking signal: Track 1 often collapses to `dispatch_value`, hiding inner string/number/structural primitive attribution. `distinct_values` and `y_string_unicode` are the clearest exceptions, surfacing tiny plain string and hex escape scalar leaves.
- CH3 route guard: parse `dispatch_value`, tiny-string, and hex-escape signals
  do not reopen pre-pin rejected dispatch-table/function-pointer,
  parser-local cursor, event sidecar, source-method digest, or decoded-string
  stats routes. Any S-P2 route using these signals must cite the relevant
  REDRESS family and name a material differential.
- Delta gap: no SK-V12 parse_only machine-readable delta was found in the required authority files. Treat any SK-V12 row movement claim as absent until P1-F or a later hardening pass supplies a same-schema comparator.

## §5 - Sources (every artefact path + run id)

- Run id/build identity: `/tmp/skv13-p1/artifacts/identity.txt`; commit `f8be692068e9e464b6ed24027ab26edfd05303fd`; timestamp `2026-05-21T06:01:45Z`.
- V2 fold identity: `/tmp/skv13-p1-v2/artifacts/identity.txt`; head `7ee299096be7d7fdaa0e69344a6cd18bbd55524f`; no `skinny/crates/` source delta from the parse capture.
- V3 canonical evidence ledger: `restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md`.
- V4 profile provenance and durable harness rebuilds: `restart/skinny/tranches/sk-v13/research/p1/support/profile-provenance-v3.md`.
- Samply status ledger: `/tmp/skv13-p1/samply/capture_status.tsv`.
- Samply run script: `/tmp/skv13-p1/samply/run-samply.sh`.
- Samply profile root: `/tmp/skv13-p1/samply/profiles/`.
- Parse profile artifacts: `/tmp/skv13-p1/samply/profiles/parse__{twitter,citm_catalog,canada,apache_builds,github_events,update_center,mesh,random,gsoc-2018,marine_ik,instruments,numbers,unicode_mixed,unicode_escapes,unicode_basic,distinct_values,y_string_unicode}__track{1,2}.json.gz`.
- Parse symbol sidecars: `/tmp/skv13-p1/samply/profiles/parse__{corpus}__track{1,2}.json.syms.json`.
- Parse samply logs: `/tmp/skv13-p1/samply/logs/parse__{corpus}__track{1,2}.log`.
- PMU row ledger: `/tmp/skv13-p1/pmu/pmu_rows.tsv`.
- PMU run script: `/tmp/skv13-p1/pmu/run-pmu.sh`.
- Required authorities read: `restart/prompts/skinny/PASS-1-PROFILE.md`, `skinny/RESULTS.md`, `skinny/REDRESS.md`, `restart/skinny/tranches/sk-v13/HANDOFF.md`, `restart/skinny/tranches/sk-v13/SYNTHESIS.md`, `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-profile-truth.md`, `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md`.
