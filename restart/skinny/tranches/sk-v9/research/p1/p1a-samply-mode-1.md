# SK-V9 P1-A: samply mode I parse_only baseline

Pass: S-P1 Profile. Cycle: V9.
Date: 2026-05-18.
Scope: samply mode I over the `parse_only` workload: cold per-parse, release plus `debug=true`, all seventeen JSON corpora.
Output: this file.
Baseline: current HEAD `b258a406ff7f46298c0baeaaf38d2c00add377fd` as the Alpha-closed opening authority; measured row authority remains `SK-V8-open` run `sk-v8-open:criterion-fnv64-9a37562ed3d0383a` until the SK-V9-open telemetry refresh exists.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max` from the W0 manifest, not a fresh SK-V9-open samply run.
Build flags: W0 manifest `profile=bench;rustflags=-C target-cpu=native;target_cpu=native`; required P1-A `release + debug=true` samply build is `absent:W0-telemetry-lock-no-fresh-SK-V9-open-samply`.
Profile tool: `absent:W0-telemetry-lock-no-fresh-SK-V9-open-samply`; historical profile reports used `samply 0.13.1`, but no SK-V9-open P1-A samply captures are present.
Corpus coverage: W0 `parse_only` row enumeration is 17/17; fresh SK-V9-open samply artifact coverage is 0/17 (`absent:W0-telemetry-lock`).

## §1 — Method (commands run; verbatim, reproducible)

This artifact uses current HEAD as the Alpha-closed opening authority because the user invoked S-P1 after the G-Alpha packet. The G-Alpha presentation says the recommended decision is `G-Alpha closed` and that after such closure the next move is the SK-V9 skinny pass sequence (`restart/skinny/tranches/sk-v9/research/g-alpha/G-ALPHA-PRESENTATION.md:7-18`, `restart/skinny/tranches/sk-v9/research/g-alpha/G-ALPHA-PRESENTATION.md:81-86`). The opening authority commit is:

```bash
git rev-parse HEAD
# b258a406ff7f46298c0baeaaf38d2c00add377fd
```

Fresh SK-V9-open samply profiles are not yet available. The SK-V9 synthesis keeps the current benchmark authority on the W0-rendered `SK-V8-open` report (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:21-28`) and names SK-V9-open telemetry refresh as a gate prerequisite, not a behavior or throughput movement (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:47-53`, `restart/skinny/tranches/sk-v9/SYNTHESIS.md:116-117`). Alpha-E makes the same lock explicit: SK-V8 W0 is an executable telemetry lock, a SK-V9-open manifest must be produced and consumed by `gate-json`, and the refresh must not change parser, scanner, SIMD, asm, codegen, generated output, product behavior, or throughput (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:416-426`, `restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:447-475`). Therefore this file records W0 telemetry and absence reasons; it does not invent samply artifacts.

Commands used to extract the evidence:

```bash
git rev-parse HEAD
git log -1 --oneline --decorate
nl -ba restart/prompts/skinny/PASS-1-PROFILE.md
nl -ba restart/skinny/tranches/sk-v9/SYNTHESIS.md
nl -ba restart/skinny/tranches/sk-v9/HANDOFF.md
nl -ba restart/skinny/tranches/sk-v9/research/g-alpha/G-ALPHA-PRESENTATION.md
nl -ba restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md
nl -ba skinny/RESULTS.md
nl -ba skinny/REDRESS.md
find skinny/profile -maxdepth 3 -type f -print
perl -F'\\|' -lane 'next unless $F[1] && $F[1]=~m{json/.*/parse_only/main}; printf "%d | row=%s | run=%s | profile=%s | sample=%s | count=%s | build=%s | host=%s | features=%s | substrate=%s | projection=%s | cardinality=%s | consumer=%s | track2=%s\n", $., map { s/^\\s+|\\s+$//gr } @F[1,5,7,8,9,10,11,12,16,17,18,19,20]' skinny/RESULTS.md
```

The PASS-1 contract requires P1-A to profile every corpus in `parse_only` mode and record flame artifacts, top-20 self-time symbols, run id, host triple, and build flags (`restart/prompts/skinny/PASS-1-PROFILE.md:50-57`). It also requires all seventeen corpora, not a subset (`restart/prompts/skinny/PASS-1-PROFILE.md:67-86`), and samply symbol resolution requires interactive `samply record` with `debug=true`, not a `--save-only` paper close (`restart/prompts/skinny/PASS-1-PROFILE.md:251-254`, `restart/prompts/skinny/PASS-1-PROFILE.md:155-160`). That command has not been run for SK-V9-open:

```bash
# not run in this artifact; blocked by absent SK-V9-open telemetry/gate refresh
absent:W0-telemetry-lock-no-fresh-SK-V9-open-samply
```

## §2 — Findings (per-corpus table; file:line on every hot-leaf claim)

The table below is the complete 17-corpus mode-I coverage ledger. Each row has a W0 row and manifest citation. Each fresh samply artifact and symbol/self-time cell is explicitly absent because the SK-V9-open samply profile set does not exist yet. The only current hot-leaf field in `skinny/RESULTS.md` is a Criterion slope-profile binding string, which the SK-V9 telemetry schema permits as a criterion artifact binding (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:278-280`) but which is not a samply symbol percentage. It is therefore a proxy, not a completed P1-A hot-leaf attribution.

| Corpus | W0 row | Outcome | T1/T2 Mbps | sonic strict | W0 sample cost | W0 profile proxy | Fresh P1-A samply artifact | P1-A symbol/self-time |
|---|---|---|---:|---:|---|---|---|---|
| `twitter` | `skinny/RESULTS.md:5`; manifest `skinny/RESULTS.md:48` | S / NO-GO | 9581 / 9741 | 18176 | `ns_per_byte=0.834967;track1_ns=527293.98;bytes=631515` | `criterion-slope-profile:json_twitter/track1_generated/new/estimates.json`; hot-leaf proxy only | `absent:W0-telemetry-lock-no-fresh-SK-V9-open-samply` | `absent:no-samply-symbol-self-time`; historical fused proxy `runtime::generated_json::generated::parse_value_at` 99.7% at `skinny/profile/reassay-skv4-2026-05-13/PROFILE-REPORT.md:42`, source `skinny/crates/runtime/src/grammars/json/generated.rs:35-43` |
| `citm_catalog` | `skinny/RESULTS.md:8`; manifest `skinny/RESULTS.md:51` | S / NO-GO | 28644 / 19214 | 21717 | `ns_per_byte=0.279290;track1_ns=482391.33;bytes=1727204` | `criterion-slope-profile:json_citm_catalog/track1_generated/new/estimates.json`; hot-leaf proxy only | `absent:W0-telemetry-lock-no-fresh-SK-V9-open-samply` | `absent:no-samply-symbol-self-time` |
| `canada` | `skinny/RESULTS.md:10`; manifest `skinny/RESULTS.md:53` | L / NO-GO | 15497 / 12171 | 8729 | `ns_per_byte=0.516215;track1_ns=1162027.13;bytes=2251051` | `criterion-slope-profile:json_canada/track1_generated/new/estimates.json`; hot-leaf proxy only | `absent:W0-telemetry-lock-no-fresh-SK-V9-open-samply` | `absent:no-samply-symbol-self-time` |
| `apache_builds` | `skinny/RESULTS.md:12`; manifest `skinny/RESULTS.md:55` | S / NO-GO | 12694 / 11715 | 16904 | `ns_per_byte=0.630235;track1_ns=80213.22;bytes=127275` | `criterion-slope-profile:json_apache_builds/track1_generated/new/estimates.json`; hot-leaf proxy only | `absent:W0-telemetry-lock-no-fresh-SK-V9-open-samply` | `absent:no-samply-symbol-self-time` |
| `github_events` | `skinny/RESULTS.md:14`; manifest `skinny/RESULTS.md:57` | S / NO-GO | 10689 / 10073 | 16408 | `ns_per_byte=0.748431;track1_ns=48746.81;bytes=65132` | `criterion-slope-profile:json_github_events/track1_generated/new/estimates.json`; hot-leaf proxy only | `absent:W0-telemetry-lock-no-fresh-SK-V9-open-samply` | `absent:no-samply-symbol-self-time` |
| `update_center` | `skinny/RESULTS.md:16`; manifest `skinny/RESULTS.md:59` | S / NO-GO | 11926 / 9312 | 18769 | `ns_per_byte=0.670820;track1_ns=357666.41;bytes=533178` | `criterion-slope-profile:json_update_center/track1_generated/new/estimates.json`; hot-leaf proxy only | `absent:W0-telemetry-lock-no-fresh-SK-V9-open-samply` | `absent:no-samply-symbol-self-time` |
| `mesh` | `skinny/RESULTS.md:19`; manifest `skinny/RESULTS.md:62` | S / NO-GO | 9367 / 10000 | 8143 | `ns_per_byte=0.854072;track1_ns=618004.02;bytes=723597` | `criterion-slope-profile:json_mesh/track1_generated/new/estimates.json`; hot-leaf proxy only | `absent:W0-telemetry-lock-no-fresh-SK-V9-open-samply` | `absent:no-samply-symbol-self-time` |
| `random` | `skinny/RESULTS.md:22`; manifest `skinny/RESULTS.md:65` | S / NO-GO | 10011 / 8018 | 15639 | `ns_per_byte=0.799114;track1_ns=407928.71;bytes=510476` | `criterion-slope-profile:json_random/track1_generated/new/estimates.json`; hot-leaf proxy only | `absent:W0-telemetry-lock-no-fresh-SK-V9-open-samply` | `absent:no-fresh-samply-symbol-self-time`; historical fused proxy `parse_value_at` 99.6% at `skinny/profile/reassay-skv4-2026-05-13/PROFILE-REPORT.md:45`, source `skinny/crates/runtime/src/grammars/json/generated.rs:35-43` |
| `gsoc-2018` | `skinny/RESULTS.md:24`; manifest `skinny/RESULTS.md:67` | S / NO-GO | 23209 / 21857 | 49101 | `ns_per_byte=0.344694;track1_ns=1147083.03;bytes=3327831` | `criterion-slope-profile:json_gsoc-2018/track1_generated/new/estimates.json`; hot-leaf proxy only | `absent:W0-telemetry-lock-no-fresh-SK-V9-open-samply` | `absent:no-samply-symbol-self-time` |
| `marine_ik` | `skinny/RESULTS.md:26`; manifest `skinny/RESULTS.md:69` | S / NO-GO | 13100 / 12164 | 9921 | `ns_per_byte=0.610675;track1_ns=1821927.79;bytes=2983466` | `criterion-slope-profile:json_marine_ik/track1_generated/new/estimates.json`; hot-leaf proxy only | `absent:W0-telemetry-lock-no-fresh-SK-V9-open-samply` | `absent:no-samply-symbol-self-time` |
| `instruments` | `skinny/RESULTS.md:29`; manifest `skinny/RESULTS.md:72` | S / NO-GO | 13320 / 11351 | 17976 | `ns_per_byte=0.600598;track1_ns=132339.34;bytes=220346` | `criterion-slope-profile:json_instruments/track1_generated/new/estimates.json`; hot-leaf proxy only | `absent:W0-telemetry-lock-no-fresh-SK-V9-open-samply` | `absent:no-samply-symbol-self-time` |
| `numbers` | `skinny/RESULTS.md:31`; manifest `skinny/RESULTS.md:74` | S / NO-GO | 12818 / 13537 | 9854 | `ns_per_byte=0.624132;track1_ns=93697.21;bytes=150124` | `criterion-slope-profile:json_numbers/track1_generated/new/estimates.json`; hot-leaf proxy only | `absent:W0-telemetry-lock-no-fresh-SK-V9-open-samply` | `absent:no-fresh-samply-symbol-self-time`; historical fused proxy `parse_value_at` 97.2%, `_platform_memmove` 1.3%, `TapeBuilder::new` 0.7% at `skinny/profile/reassay-skv4-2026-05-13/PROFILE-REPORT.md:43`, source `skinny/crates/runtime/src/grammars/json/generated.rs:35-43` |
| `unicode_mixed` | `skinny/RESULTS.md:33`; manifest `skinny/RESULTS.md:76` | S / NO-GO | 6390 / 4970 | 9943 | `ns_per_byte=1.251972;track1_ns=1318433.73;bytes=1053086` | `criterion-slope-profile:json_unicode_mixed/track1_generated/new/estimates.json`; hot-leaf proxy only | `absent:W0-telemetry-lock-no-fresh-SK-V9-open-samply` | `absent:no-fresh-samply-symbol-self-time`; historical fused proxy `parse_value_at` 99.5% at `skinny/profile/reassay-skv4-2026-05-13/PROFILE-REPORT.md:44`, source `skinny/crates/runtime/src/grammars/json/generated.rs:35-43` |
| `unicode_escapes` | `skinny/RESULTS.md:35`; manifest `skinny/RESULTS.md:78` | S / NO-GO | 12731 / 8521 | 13851 | `ns_per_byte=0.628379;track1_ns=660298.65;bytes=1050797` | `criterion-slope-profile:json_unicode_escapes/track1_generated/new/estimates.json`; hot-leaf proxy only | `absent:W0-telemetry-lock-no-fresh-SK-V9-open-samply` | `absent:no-samply-symbol-self-time` |
| `unicode_basic` | `skinny/RESULTS.md:37`; manifest `skinny/RESULTS.md:80` | S / NO-GO | 11189 / 10040 | 15797 | `ns_per_byte=0.714981;track1_ns=749719.23;bytes=1048586` | `criterion-slope-profile:json_unicode_basic/track1_generated/new/estimates.json`; hot-leaf proxy only | `absent:W0-telemetry-lock-no-fresh-SK-V9-open-samply` | `absent:no-samply-symbol-self-time` |
| `distinct_values` | `skinny/RESULTS.md:39`; manifest `skinny/RESULTS.md:82` | S / NO-GO | 10279 / 6457 | 18282 | `ns_per_byte=0.778263;track1_ns=119564.51;bytes=153630` | `criterion-slope-profile:json_distinct_values/track1_generated/new/estimates.json`; hot-leaf proxy only | `absent:W0-telemetry-lock-no-fresh-SK-V9-open-samply` | `absent:no-samply-symbol-self-time` |
| `y_string_unicode` | `skinny/RESULTS.md:41`; manifest `skinny/RESULTS.md:84` | S / NO-GO | 5577 / 5480 | 12009 | `ns_per_byte=1.434564;track1_ns=51071.91;bytes=35601` | `criterion-slope-profile:json_y_string_unicode/track1_generated/new/estimates.json`; hot-leaf proxy only | `absent:W0-telemetry-lock-no-fresh-SK-V9-open-samply` | `absent:no-samply-symbol-self-time` |

The historical `parse_value_at` proxy is deliberately not generalized beyond the four rows it measured. The re-assay itself says symbol-level samply was too fused and that no-inline or PC-level attribution was needed before prescribing a kernel (`skinny/profile/reassay-skv4-2026-05-13/PROFILE-REPORT.md:56-60`). Treat those four cells as stale, fused proxy evidence only.

## §3 — Delta vs SK-V8 (per row; Mbps + c/B + classification)

No SK-V9-open Mbps, c/B, or samply sample-cost delta exists yet. The SK-V9 Alpha contract says any admitted change must tie to current `SK-V8-open` rows and fresh measured evidence (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:77-97`), while the SK-V9-open telemetry refresh is gate-only and must not move throughput without a separate admitted measurement wave (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:467-475`). Therefore every SK-V9-vs-SK-V8 delta in P1-A is `absent:W0-telemetry-lock-no-SK-V9-open-profile`.

| Corpus | Current classification | Current T1 Mbps | Current sample cost | SK-V9-open T1 Mbps | Delta vs SK-V8 | c/B |
|---|---|---:|---|---|---|---|
| `twitter` | S / NO-GO (`skinny/RESULTS.md:5`) | 9581 | 0.834967 ns/B (`skinny/RESULTS.md:48`) | `absent:W0-telemetry-lock` | `absent:no-SK-V9-open-run-id` | `absent:P1-D-PMU-not-collected` |
| `citm_catalog` | S / NO-GO (`skinny/RESULTS.md:8`) | 28644 | 0.279290 ns/B (`skinny/RESULTS.md:51`) | `absent:W0-telemetry-lock` | `absent:no-SK-V9-open-run-id` | `absent:P1-D-PMU-not-collected` |
| `canada` | L / NO-GO (`skinny/RESULTS.md:10`) | 15497 | 0.516215 ns/B (`skinny/RESULTS.md:53`) | `absent:W0-telemetry-lock` | `absent:no-SK-V9-open-run-id` | `absent:P1-D-PMU-not-collected` |
| `apache_builds` | S / NO-GO (`skinny/RESULTS.md:12`) | 12694 | 0.630235 ns/B (`skinny/RESULTS.md:55`) | `absent:W0-telemetry-lock` | `absent:no-SK-V9-open-run-id` | `absent:P1-D-PMU-not-collected` |
| `github_events` | S / NO-GO (`skinny/RESULTS.md:14`) | 10689 | 0.748431 ns/B (`skinny/RESULTS.md:57`) | `absent:W0-telemetry-lock` | `absent:no-SK-V9-open-run-id` | `absent:P1-D-PMU-not-collected` |
| `update_center` | S / NO-GO (`skinny/RESULTS.md:16`) | 11926 | 0.670820 ns/B (`skinny/RESULTS.md:59`) | `absent:W0-telemetry-lock` | `absent:no-SK-V9-open-run-id` | `absent:P1-D-PMU-not-collected` |
| `mesh` | S / NO-GO (`skinny/RESULTS.md:19`) | 9367 | 0.854072 ns/B (`skinny/RESULTS.md:62`) | `absent:W0-telemetry-lock` | `absent:no-SK-V9-open-run-id` | `absent:P1-D-PMU-not-collected` |
| `random` | S / NO-GO (`skinny/RESULTS.md:22`) | 10011 | 0.799114 ns/B (`skinny/RESULTS.md:65`) | `absent:W0-telemetry-lock` | `absent:no-SK-V9-open-run-id` | `absent:P1-D-PMU-not-collected` |
| `gsoc-2018` | S / NO-GO (`skinny/RESULTS.md:24`) | 23209 | 0.344694 ns/B (`skinny/RESULTS.md:67`) | `absent:W0-telemetry-lock` | `absent:no-SK-V9-open-run-id` | `absent:P1-D-PMU-not-collected` |
| `marine_ik` | S / NO-GO (`skinny/RESULTS.md:26`) | 13100 | 0.610675 ns/B (`skinny/RESULTS.md:69`) | `absent:W0-telemetry-lock` | `absent:no-SK-V9-open-run-id` | `absent:P1-D-PMU-not-collected` |
| `instruments` | S / NO-GO (`skinny/RESULTS.md:29`) | 13320 | 0.600598 ns/B (`skinny/RESULTS.md:72`) | `absent:W0-telemetry-lock` | `absent:no-SK-V9-open-run-id` | `absent:P1-D-PMU-not-collected` |
| `numbers` | S / NO-GO (`skinny/RESULTS.md:31`) | 12818 | 0.624132 ns/B (`skinny/RESULTS.md:74`) | `absent:W0-telemetry-lock` | `absent:no-SK-V9-open-run-id` | `absent:P1-D-PMU-not-collected` |
| `unicode_mixed` | S / NO-GO (`skinny/RESULTS.md:33`) | 6390 | 1.251972 ns/B (`skinny/RESULTS.md:76`) | `absent:W0-telemetry-lock` | `absent:no-SK-V9-open-run-id` | `absent:P1-D-PMU-not-collected` |
| `unicode_escapes` | S / NO-GO (`skinny/RESULTS.md:35`) | 12731 | 0.628379 ns/B (`skinny/RESULTS.md:78`) | `absent:W0-telemetry-lock` | `absent:no-SK-V9-open-run-id` | `absent:P1-D-PMU-not-collected` |
| `unicode_basic` | S / NO-GO (`skinny/RESULTS.md:37`) | 11189 | 0.714981 ns/B (`skinny/RESULTS.md:80`) | `absent:W0-telemetry-lock` | `absent:no-SK-V9-open-run-id` | `absent:P1-D-PMU-not-collected` |
| `distinct_values` | S / NO-GO (`skinny/RESULTS.md:39`) | 10279 | 0.778263 ns/B (`skinny/RESULTS.md:82`) | `absent:W0-telemetry-lock` | `absent:no-SK-V9-open-run-id` | `absent:P1-D-PMU-not-collected` |
| `y_string_unicode` | S / NO-GO (`skinny/RESULTS.md:41`) | 5577 | 1.434564 ns/B (`skinny/RESULTS.md:84`) | `absent:W0-telemetry-lock` | `absent:no-SK-V9-open-run-id` | `absent:P1-D-PMU-not-collected` |

## §4 — Anomalies + masking signals (flagged for S-P2)

1. Fresh SK-V9-open samply coverage is 0/17. This is not a profiling success; it is a telemetry-lock gap. The SK-V9-open manifest/gate refresh is a named prerequisite and must be same-wave consumed by `gate-json` before rows can be treated as SK-V9-open telemetry (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:449-475`).

2. Current `parse_only` rows are guard telemetry, not strict admission. G-Alpha states that all current main rows remain `Strictness=deferred`, `parse_utf8=view-boundary`, and `escape_complete=yes`, and that parse rows remain non-admission guard telemetry under the borrowed-view-vs-DOM plane (`restart/skinny/tranches/sk-v9/research/g-alpha/G-ALPHA-PRESENTATION.md:20-34`). The visible row plane is `borrowed view over offset tape vs DOM` across all 17 parse rows (`skinny/RESULTS.md:5-42`).

3. The W0 hot-leaf field is not a resolved samply symbol. PASS-1 requires every hot-leaf claim to cite a samply symbol path and self-time percentage, and rejects unresolved paper-close cells (`restart/prompts/skinny/PASS-1-PROFILE.md:123-127`, `restart/prompts/skinny/PASS-1-PROFILE.md:155-160`). W0 provides Criterion slope artifacts and sample cost, not top-20 samply self-time symbols (`skinny/RESULTS.md:44-85`).

4. Historical profile proxies should not drive S-P2 design by themselves. The four measured parse proxies from the SK-V4 re-assay are fused around `parse_value_at` and explicitly say no-inline or PC-level attribution is required (`skinny/profile/reassay-skv4-2026-05-13/PROFILE-REPORT.md:38-46`, `skinny/profile/reassay-skv4-2026-05-13/PROFILE-REPORT.md:56-60`). The current source shows `parse_value_at` as a generic dispatch hub over value classes, not an attributed primitive leaf (`skinny/crates/runtime/src/grammars/json/generated.rs:35-58`).

5. Sidecar comparator evidence remains planning-grade where historical or absent. The W0 report says native Rust comparators are same-run while C++ sidecars are historical or explicitly absent and never strict anchors in W0 (`skinny/RESULTS.md:141`). SK-V9 strict comparator requirements likewise require same-run, same-plane evidence before strict admission (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:220-240`).

6. Do not reopen rejected routes from this profile gap. REDRESS 91 keeps Apache/CITM as source/product parity only and rejects row-table overclaim (`skinny/REDRESS.md:2620-2659`). REDRESS 92 rejects structural-projection implementation until the retained class/event grammar and `ValueRef` cursor contract are proven (`skinny/REDRESS.md:2661-2690`). REDRESS 93 rejects scalar-parent/direct digest folding without a V9-aware checked gate, full-table maintain measurement, and independent Track 2 backstop (`skinny/REDRESS.md:2692-2729`). Alpha-C binds these as SK-V9 pre-blocks unless fresh evidence and changed framing exist (`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:215-235`).

## §5 — Sources (every artefact path + run id)

Primary authority:

| Source | Use |
|---|---|
| `HEAD b258a406ff7f46298c0baeaaf38d2c00add377fd` | Alpha-closed opening authority for this artifact. |
| `restart/skinny/tranches/sk-v9/research/g-alpha/G-ALPHA-PRESENTATION.md:7-18` | G-Alpha close recommendation and convergence state. |
| `restart/skinny/tranches/sk-v9/SYNTHESIS.md:21-28` | Current benchmark authority remains W0-rendered `skinny/RESULTS.md`. |
| `restart/skinny/tranches/sk-v9/HANDOFF.md:22-37` | Current state summary: W0 report authority and sidecar freshness caveat. |
| `restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:416-493` | W0 telemetry-lock and SK-V9-open refresh boundaries. |
| `restart/prompts/skinny/PASS-1-PROFILE.md:50-57` | P1-A scope and output path. |
| `restart/prompts/skinny/PASS-1-PROFILE.md:67-86` | Mandatory 17-corpus coverage. |
| `restart/prompts/skinny/PASS-1-PROFILE.md:88-110` | Required P1 frontmatter and sections. |
| `restart/prompts/skinny/PASS-1-PROFILE.md:239-279` | Cold per-parse, sequential, samply, and no-hypothesis constraints. |

Measured row authority:

| Artifact | Run id / status |
|---|---|
| `skinny/RESULTS.md:3-42` | Main W0 row table; 17 `parse_only` rows, all `NO-GO`. |
| `skinny/RESULTS.md:44-85` | W0 manifest; run id `sk-v8-open:criterion-fnv64-9a37562ed3d0383a`; sample cost and profile proxy paths. |
| `skinny/RESULTS.md:138-141` | Overall `N-direct / NoGo`; W0 native Rust comparator same-run and C++ sidecar freshness caveat. |
| `skinny/REDRESS.md:2620-2659` | REDRESS 91 row-table boundary. |
| `skinny/REDRESS.md:2661-2690` | REDRESS 92 structural-projection boundary. |
| `skinny/REDRESS.md:2692-2729` | REDRESS 93 direct guard boundary. |

Existing profile artifacts read:

| Artifact | Relevance |
|---|---|
| `skinny/profile/reassay-skv4-2026-05-13/PROFILE-REPORT.md:38-46` | Historical parse-only samply proxy for four corpora only; stale and fused. |
| `skinny/profile/reassay-skv4-2026-05-13/PROFILE-REPORT.md:62-80` | Historical commands used `--save-only`; PASS-1 now requires interactive symbol-resolving samply. |
| `skinny/profile/native-sidecars/PROFILE-REPORT.md:163-218` | Historical sidecar hot-leaf landscape, comparator-only. |
| `skinny/profile/simdjson-expanded/PROFILE-REPORT.md:198-233` | Historical simdjson sidecar artifacts and run details. |
| `skinny/profile/sonic-rs-expanded/PROFILE-REPORT.md:1-20` | Historical sonic-rs profile scope; incomplete corpus coverage for samply. |
| `skinny/profile/serde_json/PROFILE-REPORT.md:1-10` | Historical serde_json floor comparator profile setup. |
| `skinny/profile/rapidjson/PROFILE-REPORT.md:1-10` | Historical RapidJSON floor comparator profile setup. |
| `skinny/profile/yyjson/PROFILE-REPORT.md:1-18` | Historical yyjson sidecar setup. |

Fresh P1-A samply artifacts:

| Expected class | Status |
|---|---|
| `/tmp/skv9-p1/` or bench-harness profile directory, 17 mode-I `parse_only` samply profiles | `absent:W0-telemetry-lock-no-fresh-SK-V9-open-samply` |
| Top-20 self-time symbols per corpus with source file:line | `absent:no-samply-symbol-self-time` |
| SK-V9-open run id | `absent:no-SK-V9-open-manifest` |
| SK-V9-open c/B or PMU rows | `absent:P1-D-PMU-not-collected` |
