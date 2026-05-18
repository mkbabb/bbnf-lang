# SK-V9 P1-C: Samply Mode III Masking And Structural-Scan Profile

Pass: S-P1 Profile. Cycle: V9.
Date: 2026-05-18.
Scope: Mode III masking-probe workloads and the structural-scan-only path.
Output: this file.
Baseline: current HEAD `b258a406ff7f46298c0baeaaf38d2c00add377fd` as Alpha-closed opening authority; fresh SK-V9-open samply/PMU profiles are `absent:sk-v9-w0-telemetry-lock-not-produced`.
Host triple: `aarch64-apple-darwin` from the current W0 telemetry manifest (`skinny/RESULTS.md:48`).
Build flags: `profile=bench;rustflags=-C target-cpu=native;target_cpu=native` from the current W0 telemetry manifest (`skinny/RESULTS.md:48`).
Profile tool: samply `absent:sk-v9-w0-telemetry-lock-not-produced`.
Corpus coverage: 17/17 rows recorded; every profile measurement cell is `absent:sk-v9-w0-telemetry-lock-not-produced`.

Authority note: the SK-V9 synthesis says the current benchmark authority is still the W0-rendered `skinny/RESULTS.md` report (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:23`, `restart/skinny/tranches/sk-v9/SYNTHESIS.md:27`), while the task dispatch makes current HEAD the Alpha-closed opening authority. This artifact therefore records opening evidence and absences only; it does not relabel SK-V8-open rows as fresh SK-V9 measurements.

## §1 - Method

Commands run:

```sh
git rev-parse HEAD
git status --short --branch
nl -ba skinny/RESULTS.md
nl -ba skinny/REDRESS.md
nl -ba restart/skinny/tranches/sk-v9/SYNTHESIS.md
nl -ba restart/skinny/tranches/sk-v9/HANDOFF.md
nl -ba restart/prompts/skinny/PASS-1-PROFILE.md
nl -ba skinny/crates/bbnf-bench/src/probes.rs
nl -ba skinny/crates/bbnf-bench/benches/json_parity.rs
nl -ba skinny/crates/bbnf-bench/benches/simd_scan.rs
nl -ba skinny/crates/bbnf-bench/src/bin/gate.rs
nl -ba skinny/crates/bbnf-bench/src/report.rs
```

Commands intentionally not run:

```sh
absent:sk-v9-w0-telemetry-lock-not-produced
```

Rationale: S-P1 normally requires mode III samply artifacts for `host_call_eager_decode`, `alternate_scalar_plan`, `cold_first_parse`, and the structural-scan-only path (`restart/prompts/skinny/PASS-1-PROFILE.md:54`). The same prompt requires real profile files and no uncaused `n/a` cells (`restart/prompts/skinny/PASS-1-PROFILE.md:155`, `restart/prompts/skinny/PASS-1-PROFILE.md:160`). Because fresh SK-V9-open profiles are absent until W0 telemetry-lock, every missing measurement below uses an explicit `absent:<reason>` instead of a guessed symbol, Mbps value, or cycles-per-byte value.

## §2 - Findings

Mode III source coverage is present in the bench harness, but not in fresh SK-V9 profile artifacts. `json_parity` always defines the probe group after each fixture bench (`skinny/crates/bbnf-bench/benches/json_parity.rs:354`, `skinny/crates/bbnf-bench/benches/json_parity.rs:361`), and `simd_scan` iterates every loaded fixture for scalar/SIMD parity plus structural-scan benchmarking (`skinny/crates/bbnf-bench/benches/simd_scan.rs:16`, `skinny/crates/bbnf-bench/benches/simd_scan.rs:37`). The report only renders masking probe rows when probe rows exist (`skinny/crates/bbnf-bench/src/report.rs:612`, `skinny/crates/bbnf-bench/src/report.rs:625`), and current `skinny/RESULTS.md` has no `## Masking Probes` section.

| Corpus | W0 parse row source | Mode III profile artifact | Top self-time symbols | Masking probe signal status | Structural-scan status |
|---|---|---|---|---|---|
| twitter | `skinny/RESULTS.md:5` | `absent:sk-v9-w0-telemetry-lock-not-produced` | `absent:samply-not-run-before-w0-telemetry-lock` | six configured probes recorded in §4; all measured signals `absent:sk-v9-w0-telemetry-lock-not-produced` except invalid duplicate-dispatch status | `absent:sk-v9-w0-telemetry-lock-not-produced`; source path exists in `simd_scan` (`skinny/crates/bbnf-bench/benches/simd_scan.rs:29`, `skinny/crates/bbnf-bench/benches/simd_scan.rs:37`) |
| citm_catalog | `skinny/RESULTS.md:8` | `absent:sk-v9-w0-telemetry-lock-not-produced` | `absent:samply-not-run-before-w0-telemetry-lock` | six configured probes recorded in §4; all measured signals `absent:sk-v9-w0-telemetry-lock-not-produced` except invalid duplicate-dispatch status | `absent:sk-v9-w0-telemetry-lock-not-produced`; source path exists in `simd_scan` (`skinny/crates/bbnf-bench/benches/simd_scan.rs:29`, `skinny/crates/bbnf-bench/benches/simd_scan.rs:37`) |
| canada | `skinny/RESULTS.md:10` | `absent:sk-v9-w0-telemetry-lock-not-produced` | `absent:samply-not-run-before-w0-telemetry-lock` | six configured probes recorded in §4; all measured signals `absent:sk-v9-w0-telemetry-lock-not-produced` except invalid duplicate-dispatch status | current `RESULTS.md` note says 27870 Mbps vs 40000 Mbps floor (`skinny/RESULTS.md:97`); REDRESS says later current full-matrix scan was 69075 Mbps (`skinny/REDRESS.md:27`, `skinny/REDRESS.md:29`); fresh SK-V9 profile is `absent:sk-v9-w0-telemetry-lock-not-produced` |
| apache_builds | `skinny/RESULTS.md:12` | `absent:sk-v9-w0-telemetry-lock-not-produced` | `absent:samply-not-run-before-w0-telemetry-lock` | six configured probes recorded in §4; all measured signals `absent:sk-v9-w0-telemetry-lock-not-produced` except invalid duplicate-dispatch status | `absent:sk-v9-w0-telemetry-lock-not-produced`; source path exists in `simd_scan` (`skinny/crates/bbnf-bench/benches/simd_scan.rs:29`, `skinny/crates/bbnf-bench/benches/simd_scan.rs:37`) |
| github_events | `skinny/RESULTS.md:14` | `absent:sk-v9-w0-telemetry-lock-not-produced` | `absent:samply-not-run-before-w0-telemetry-lock` | six configured probes recorded in §4; all measured signals `absent:sk-v9-w0-telemetry-lock-not-produced` except invalid duplicate-dispatch status | `absent:sk-v9-w0-telemetry-lock-not-produced`; source path exists in `simd_scan` (`skinny/crates/bbnf-bench/benches/simd_scan.rs:29`, `skinny/crates/bbnf-bench/benches/simd_scan.rs:37`) |
| update_center | `skinny/RESULTS.md:16` | `absent:sk-v9-w0-telemetry-lock-not-produced` | `absent:samply-not-run-before-w0-telemetry-lock` | six configured probes recorded in §4; all measured signals `absent:sk-v9-w0-telemetry-lock-not-produced` except invalid duplicate-dispatch status | `absent:sk-v9-w0-telemetry-lock-not-produced`; source path exists in `simd_scan` (`skinny/crates/bbnf-bench/benches/simd_scan.rs:29`, `skinny/crates/bbnf-bench/benches/simd_scan.rs:37`) |
| mesh | `skinny/RESULTS.md:19` | `absent:sk-v9-w0-telemetry-lock-not-produced` | `absent:samply-not-run-before-w0-telemetry-lock` | six configured probes recorded in §4; all measured signals `absent:sk-v9-w0-telemetry-lock-not-produced` except invalid duplicate-dispatch status | `absent:sk-v9-w0-telemetry-lock-not-produced`; source path exists in `simd_scan` (`skinny/crates/bbnf-bench/benches/simd_scan.rs:29`, `skinny/crates/bbnf-bench/benches/simd_scan.rs:37`) |
| random | `skinny/RESULTS.md:22` | `absent:sk-v9-w0-telemetry-lock-not-produced` | `absent:samply-not-run-before-w0-telemetry-lock` | six configured probes recorded in §4; all measured signals `absent:sk-v9-w0-telemetry-lock-not-produced` except invalid duplicate-dispatch status | `absent:sk-v9-w0-telemetry-lock-not-produced`; source path exists in `simd_scan` (`skinny/crates/bbnf-bench/benches/simd_scan.rs:29`, `skinny/crates/bbnf-bench/benches/simd_scan.rs:37`) |
| gsoc-2018 | `skinny/RESULTS.md:24` | `absent:sk-v9-w0-telemetry-lock-not-produced` | `absent:samply-not-run-before-w0-telemetry-lock` | six configured probes recorded in §4; all measured signals `absent:sk-v9-w0-telemetry-lock-not-produced` except invalid duplicate-dispatch status | `absent:sk-v9-w0-telemetry-lock-not-produced`; source path exists in `simd_scan` (`skinny/crates/bbnf-bench/benches/simd_scan.rs:29`, `skinny/crates/bbnf-bench/benches/simd_scan.rs:37`) |
| marine_ik | `skinny/RESULTS.md:26` | `absent:sk-v9-w0-telemetry-lock-not-produced` | `absent:samply-not-run-before-w0-telemetry-lock` | six configured probes recorded in §4; all measured signals `absent:sk-v9-w0-telemetry-lock-not-produced` except invalid duplicate-dispatch status | `absent:sk-v9-w0-telemetry-lock-not-produced`; source path exists in `simd_scan` (`skinny/crates/bbnf-bench/benches/simd_scan.rs:29`, `skinny/crates/bbnf-bench/benches/simd_scan.rs:37`) |
| instruments | `skinny/RESULTS.md:29` | `absent:sk-v9-w0-telemetry-lock-not-produced` | `absent:samply-not-run-before-w0-telemetry-lock` | six configured probes recorded in §4; all measured signals `absent:sk-v9-w0-telemetry-lock-not-produced` except invalid duplicate-dispatch status | `absent:sk-v9-w0-telemetry-lock-not-produced`; source path exists in `simd_scan` (`skinny/crates/bbnf-bench/benches/simd_scan.rs:29`, `skinny/crates/bbnf-bench/benches/simd_scan.rs:37`) |
| numbers | `skinny/RESULTS.md:31` | `absent:sk-v9-w0-telemetry-lock-not-produced` | `absent:samply-not-run-before-w0-telemetry-lock` | six configured probes recorded in §4; all measured signals `absent:sk-v9-w0-telemetry-lock-not-produced` except invalid duplicate-dispatch status | `absent:sk-v9-w0-telemetry-lock-not-produced`; source path exists in `simd_scan` (`skinny/crates/bbnf-bench/benches/simd_scan.rs:29`, `skinny/crates/bbnf-bench/benches/simd_scan.rs:37`) |
| unicode_mixed | `skinny/RESULTS.md:33` | `absent:sk-v9-w0-telemetry-lock-not-produced` | `absent:samply-not-run-before-w0-telemetry-lock` | six configured probes recorded in §4; all measured signals `absent:sk-v9-w0-telemetry-lock-not-produced` except invalid duplicate-dispatch status | `absent:sk-v9-w0-telemetry-lock-not-produced`; source path exists in `simd_scan` (`skinny/crates/bbnf-bench/benches/simd_scan.rs:29`, `skinny/crates/bbnf-bench/benches/simd_scan.rs:37`) |
| unicode_escapes | `skinny/RESULTS.md:35` | `absent:sk-v9-w0-telemetry-lock-not-produced` | `absent:samply-not-run-before-w0-telemetry-lock` | six configured probes recorded in §4; all measured signals `absent:sk-v9-w0-telemetry-lock-not-produced` except invalid duplicate-dispatch status | `absent:sk-v9-w0-telemetry-lock-not-produced`; source path exists in `simd_scan` (`skinny/crates/bbnf-bench/benches/simd_scan.rs:29`, `skinny/crates/bbnf-bench/benches/simd_scan.rs:37`) |
| unicode_basic | `skinny/RESULTS.md:37` | `absent:sk-v9-w0-telemetry-lock-not-produced` | `absent:samply-not-run-before-w0-telemetry-lock` | six configured probes recorded in §4; all measured signals `absent:sk-v9-w0-telemetry-lock-not-produced` except invalid duplicate-dispatch status | `absent:sk-v9-w0-telemetry-lock-not-produced`; source path exists in `simd_scan` (`skinny/crates/bbnf-bench/benches/simd_scan.rs:29`, `skinny/crates/bbnf-bench/benches/simd_scan.rs:37`) |
| distinct_values | `skinny/RESULTS.md:39` | `absent:sk-v9-w0-telemetry-lock-not-produced` | `absent:samply-not-run-before-w0-telemetry-lock` | six configured probes recorded in §4; all measured signals `absent:sk-v9-w0-telemetry-lock-not-produced` except invalid duplicate-dispatch status | `absent:sk-v9-w0-telemetry-lock-not-produced`; source path exists in `simd_scan` (`skinny/crates/bbnf-bench/benches/simd_scan.rs:29`, `skinny/crates/bbnf-bench/benches/simd_scan.rs:37`) |
| y_string_unicode | `skinny/RESULTS.md:41` | `absent:sk-v9-w0-telemetry-lock-not-produced` | `absent:samply-not-run-before-w0-telemetry-lock` | six configured probes recorded in §4; all measured signals `absent:sk-v9-w0-telemetry-lock-not-produced` except invalid duplicate-dispatch status | `absent:sk-v9-w0-telemetry-lock-not-produced`; source path exists in `simd_scan` (`skinny/crates/bbnf-bench/benches/simd_scan.rs:29`, `skinny/crates/bbnf-bench/benches/simd_scan.rs:37`) |

No row above claims a sampled hot leaf. The only current hot-leaf fields in `skinny/RESULTS.md` are Criterion slope placeholders such as `criterion-slope-profile:json_twitter/track1_generated/new/estimates.json` (`skinny/RESULTS.md:5`), not resolved samply symbols with file:line. P1-C therefore leaves symbol attribution to `absent:samply-not-run-before-w0-telemetry-lock`.

## §3 - Delta vs SK-V8

Delta is blocked for mode III because there is no fresh SK-V9-open profile, Mbps, or c/B row. The current opening report is SK-V8-open with run id `sk-v8-open:criterion-fnv64-9a37562ed3d0383a` (`skinny/RESULTS.md:48`) and current overall outcome `N-direct / NoGo` (`skinny/RESULTS.md:138`).

| Row family | SK-V8-open source | SK-V9 mode III Mbps | SK-V9 c/B | Classification |
|---|---|---:|---:|---|
| `host_call_dispatch_overhead` x 17 corpora | probe configured in `skinny/crates/bbnf-bench/src/probes.rs:32` | `absent:sk-v9-w0-telemetry-lock-not-produced` | `absent:p1d-pmu-and-w0-telemetry-lock-not-produced` | `absent:no-sk-v9-mode-iii-measurement` |
| `host_call_eager_decode` x 17 corpora | probe configured in `skinny/crates/bbnf-bench/src/probes.rs:33`; benchmark body parses then eagerly decodes strings (`skinny/crates/bbnf-bench/benches/json_parity.rs:399`, `skinny/crates/bbnf-bench/benches/json_parity.rs:405`) | `absent:sk-v9-w0-telemetry-lock-not-produced` | `absent:p1d-pmu-and-w0-telemetry-lock-not-produced` | `absent:no-sk-v9-mode-iii-measurement` |
| `alternate_scalar_plan` x 17 corpora | probe configured in `skinny/crates/bbnf-bench/src/probes.rs:34`; benchmark uses `serde_json::from_str` (`skinny/crates/bbnf-bench/benches/json_parity.rs:407`, `skinny/crates/bbnf-bench/benches/json_parity.rs:412`) | `absent:sk-v9-w0-telemetry-lock-not-produced` | `absent:p1d-pmu-and-w0-telemetry-lock-not-produced` | `absent:no-sk-v9-mode-iii-measurement` |
| `alternate_dispatch_table_plan` x 17 corpora | probe configured in `skinny/crates/bbnf-bench/src/probes.rs:35`; gate marks it invalid (`skinny/crates/bbnf-bench/src/bin/gate.rs:1516`, `skinny/crates/bbnf-bench/src/bin/gate.rs:1524`) | `absent:invalid-duplicate-probe-disabled` | `absent:invalid-duplicate-probe-disabled` | `invalid:duplicate-probe-disabled` |
| `alternate_pext_mask_plan` x 17 corpora | probe configured in `skinny/crates/bbnf-bench/src/probes.rs:36`; benchmark compiled only for x86/x86_64 (`skinny/crates/bbnf-bench/benches/json_parity.rs:414`, `skinny/crates/bbnf-bench/benches/json_parity.rs:420`) | `absent:host-aarch64-no-pext-bench` | `absent:host-aarch64-no-pext-bench` | `absent:host-aarch64-no-pext-bench` |
| `cold_first_parse` x 17 corpora | probe configured in `skinny/crates/bbnf-bench/src/probes.rs:37`; benchmark clones fixture bytes then parses (`skinny/crates/bbnf-bench/benches/json_parity.rs:422`, `skinny/crates/bbnf-bench/benches/json_parity.rs:431`) | `absent:sk-v9-w0-telemetry-lock-not-produced` | `absent:p1d-pmu-and-w0-telemetry-lock-not-produced` | `absent:no-sk-v9-mode-iii-measurement` |
| structural-scan-only x 17 corpora | `simd_scan` fixture loop and parity/bench source (`skinny/crates/bbnf-bench/benches/simd_scan.rs:16`, `skinny/crates/bbnf-bench/benches/simd_scan.rs:37`) | `absent:sk-v9-w0-telemetry-lock-not-produced` | `absent:p1d-pmu-and-w0-telemetry-lock-not-produced` | `absent:no-sk-v9-mode-iii-measurement` |

## §4 - Anomalies + Masking Signals

### §4.1 Masking Probe Signal Ledger

The configured masking matrix contains six probes (`skinny/crates/bbnf-bench/src/probes.rs:30`, `skinny/crates/bbnf-bench/src/probes.rs:45`). P1-C primary scope from the pass prompt names `host_call_eager_decode`, `alternate_scalar_plan`, `cold_first_parse`, and structural-scan-only (`restart/prompts/skinny/PASS-1-PROFILE.md:54`); the gate/report code additionally carries dispatch, dispatch-table, and PEXT probe rows.

| Probe | Workload body | Gate signal rule | P1-C opening signal |
|---|---|---|---|
| `host_call_dispatch_overhead` | calls a registry-like function pointer (`skinny/crates/bbnf-bench/benches/json_parity.rs:394`, `skinny/crates/bbnf-bench/benches/json_parity.rs:397`) | PASS if `<=50ns`, else FAIL (`skinny/crates/bbnf-bench/src/bin/gate.rs:1553`, `skinny/crates/bbnf-bench/src/bin/gate.rs:1559`) | `absent:sk-v9-w0-telemetry-lock-not-produced` |
| `host_call_eager_decode` | parses Track 1 then walks all object keys and string values via `as_str()` (`skinny/crates/bbnf-bench/benches/json_parity.rs:399`, `skinny/crates/bbnf-bench/benches/json_parity.rs:405`, `skinny/crates/bbnf-bench/benches/json_parity.rs:440`, `skinny/crates/bbnf-bench/benches/json_parity.rs:454`) | PASS if within corpus ratio, otherwise `MASKING` (`skinny/crates/bbnf-bench/src/bin/gate.rs:1561`, `skinny/crates/bbnf-bench/src/bin/gate.rs:1575`) | `absent:sk-v9-w0-telemetry-lock-not-produced` |
| `alternate_scalar_plan` | parses through serde_json (`skinny/crates/bbnf-bench/benches/json_parity.rs:407`, `skinny/crates/bbnf-bench/benches/json_parity.rs:412`) | generic reported signal (`skinny/crates/bbnf-bench/src/bin/gate.rs:1587`) | `absent:sk-v9-w0-telemetry-lock-not-produced` |
| `alternate_dispatch_table_plan` | configured, but no valid distinct measurement in gate | invalid duplicate probe, disabled because a real function-pointer table regressed (`skinny/crates/bbnf-bench/src/bin/gate.rs:1516`, `skinny/crates/bbnf-bench/src/bin/gate.rs:1524`); REDRESS records the invalidation (`skinny/REDRESS.md:216`, `skinny/REDRESS.md:224`) | `invalid:duplicate-probe-disabled` |
| `alternate_pext_mask_plan` | scalar structural offsets behind x86/x86_64 cfg (`skinny/crates/bbnf-bench/benches/json_parity.rs:414`, `skinny/crates/bbnf-bench/benches/json_parity.rs:420`) | generic reported signal if present (`skinny/crates/bbnf-bench/src/bin/gate.rs:1587`) | `absent:host-aarch64-no-pext-bench` |
| `cold_first_parse` | clones fixture bytes, UTF-8 checks, and parses in a large-input batch (`skinny/crates/bbnf-bench/benches/json_parity.rs:422`, `skinny/crates/bbnf-bench/benches/json_parity.rs:431`) | PASS if `<=2.00x T1`, else cold-sensitive (`skinny/crates/bbnf-bench/src/bin/gate.rs:1577`, `skinny/crates/bbnf-bench/src/bin/gate.rs:1585`) | `absent:sk-v9-w0-telemetry-lock-not-produced` |

The report path explains why these signals are not in current `skinny/RESULTS.md`: volatile probes are only pushed when `--include-volatile-probes` is present (`skinny/crates/bbnf-bench/src/bin/gate.rs:261`, `skinny/crates/bbnf-bench/src/bin/gate.rs:269`), that flag is rejected with `--update-results` (`skinny/crates/bbnf-bench/src/bin/gate.rs:26`, `skinny/crates/bbnf-bench/src/bin/gate.rs:35`), and the W0 fingerprint deliberately excludes derendered probe estimate files (`skinny/crates/bbnf-bench/src/bin/gate.rs:1782`, `skinny/crates/bbnf-bench/src/bin/gate.rs:1816`).

### §4.2 Structural-Scan Observations

Structural-scan-only source is tied to the `simd_scan` bench, which computes scalar and SIMD offsets, hashes them, and asserts equality before benchmarking (`skinny/crates/bbnf-bench/benches/simd_scan.rs:16`, `skinny/crates/bbnf-bench/benches/simd_scan.rs:26`). The gate reads only the Canada SIMD structural estimate for the floor check (`skinny/crates/bbnf-bench/src/bin/gate.rs:1591`, `skinny/crates/bbnf-bench/src/bin/gate.rs:1597`) and sets the floor to 5.0 GB/s on aarch64/arm or 7.0 GB/s otherwise (`skinny/crates/bbnf-bench/src/bin/gate.rs:1599`, `skinny/crates/bbnf-bench/src/bin/gate.rs:1605`).

Current source/telemetry conflict: `skinny/RESULTS.md` records a Canada structural-scan note of 27870 Mbps against a 40000 Mbps floor (`skinny/RESULTS.md:97`), while `skinny/REDRESS.md` says the current full matrix folded item 56 and reports Canada structural-only scan at 69075 Mbps against the same floor (`skinny/REDRESS.md:27`, `skinny/REDRESS.md:29`) and later repeats that structural scan is not the active blocker (`skinny/REDRESS.md:63`, `skinny/REDRESS.md:67`). P1-C does not adjudicate that conflict with an inferred value; the SK-V9 structural-scan profile remains `absent:sk-v9-w0-telemetry-lock-not-produced`.

### §4.3 Pre-Blocked Route Guardrails

The SK-V9 contract prevents structural-heavy parse implementation before retained class/event grammar and `ValueRef` cursor proof (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:86`, `restart/skinny/tranches/sk-v9/SYNTHESIS.md:87`). The handoff repeats that W3 structural implementation remains blocked without that proof (`restart/skinny/tranches/sk-v9/HANDOFF.md:88`, `restart/skinny/tranches/sk-v9/HANDOFF.md:89`). Therefore this P1-C artifact records measurements and absences only; it does not propose a structural cursor, sidecar, parser-owned fact slot, or retained implementation route.

## §5 - Sources

Primary authority and report surfaces:

| Source | Use |
|---|---|
| `b258a406ff7f46298c0baeaaf38d2c00add377fd` | Current HEAD used as Alpha-closed opening authority. |
| `skinny/RESULTS.md:3` | Main report schema header. |
| `skinny/RESULTS.md:5` through `skinny/RESULTS.md:42` | Current 17-corpus opening rows used for corpus coverage and W0 parse/direct context. |
| `skinny/RESULTS.md:46` through `skinny/RESULTS.md:85` | W0 telemetry manifest, run id, host triple, build flags, sample costs, and comparator freshness. |
| `skinny/RESULTS.md:87` through `skinny/RESULTS.md:141` | Notes, materialization counters, Canada structural note, overall outcome, and comparator freshness note. |
| `skinny/REDRESS.md:163` through `skinny/REDRESS.md:170` | Masking-probe report artifact contract. |
| `skinny/REDRESS.md:216` through `skinny/REDRESS.md:224` | Invalidated duplicate dispatch-table probe route. |
| `skinny/REDRESS.md:2661` through `skinny/REDRESS.md:2690` | REDRESS 92 structural parse precursor routing. |

Bench and gate source:

| Source | Use |
|---|---|
| `restart/prompts/skinny/PASS-1-PROFILE.md:54` | P1-C mode III scope. |
| `restart/prompts/skinny/PASS-1-PROFILE.md:67` through `restart/prompts/skinny/PASS-1-PROFILE.md:77` | Mandatory 17-corpus coverage. |
| `restart/prompts/skinny/PASS-1-PROFILE.md:88` through `restart/prompts/skinny/PASS-1-PROFILE.md:110` | Mandatory P1 artifact section schema. |
| `restart/prompts/skinny/PASS-1-PROFILE.md:258` through `restart/prompts/skinny/PASS-1-PROFILE.md:266` | Masking probes and substrate-union requirements. |
| `skinny/crates/bbnf-bench/src/probes.rs:30` through `skinny/crates/bbnf-bench/src/probes.rs:57` | Configured probe matrix and default thresholds. |
| `skinny/crates/bbnf-bench/benches/json_parity.rs:381` through `skinny/crates/bbnf-bench/benches/json_parity.rs:438` | Mode III probe benchmark bodies. |
| `skinny/crates/bbnf-bench/benches/json_parity.rs:440` through `skinny/crates/bbnf-bench/benches/json_parity.rs:455` | Eager string-decode walker body. |
| `skinny/crates/bbnf-bench/benches/simd_scan.rs:9` through `skinny/crates/bbnf-bench/benches/simd_scan.rs:40` | Structural-scan-only benchmark body. |
| `skinny/crates/bbnf-bench/src/bin/gate.rs:1500` through `skinny/crates/bbnf-bench/src/bin/gate.rs:1589` | Probe row ingestion and signal classification. |
| `skinny/crates/bbnf-bench/src/bin/gate.rs:1591` through `skinny/crates/bbnf-bench/src/bin/gate.rs:1605` | Canada structural-scan floor ingestion and architecture floor. |
| `skinny/crates/bbnf-bench/src/report.rs:465` through `skinny/crates/bbnf-bench/src/report.rs:482` | Probe report row storage. |
| `skinny/crates/bbnf-bench/src/report.rs:612` through `skinny/crates/bbnf-bench/src/report.rs:625` | Probe report rendering. |

Profile artifacts:

| Artifact | Status |
|---|---|
| `/tmp/skv9-p1/p1c-samply-mode-3/*.json` | `absent:sk-v9-w0-telemetry-lock-not-produced` |
| `/tmp/skv9-p1/p1c-samply-mode-3/*.profile` | `absent:sk-v9-w0-telemetry-lock-not-produced` |
| `/tmp/skv9-p1/p1c-samply-mode-3/*.svg` | `absent:sk-v9-w0-telemetry-lock-not-produced` |
| PMU/cycles-per-byte rows | `absent:p1d-pmu-and-w0-telemetry-lock-not-produced` |
