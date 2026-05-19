# SK-V11 Pass Alpha V1 - alpha-B Competitor Deltas

Pass: Alpha V1.
Agent: alpha-B.
Date: 2026-05-19.
Scope: SK-V10 close competitor delta extraction for SK-V11 input.

## Contract

PASS-ALPHA assigns alpha-B to extract comparator deltas for every comparator,
with strictness and output plane disclosed for each row
(`restart/prompts/pass-contracts/PASS-ALPHA.md:18-29`). The same prompt binds
the strict comparator gate to sonic-rs strict, simdjson DOM/On Demand, yyjson,
asmjson, RapidJSON, and serde_json where runnable, with Mbps, percent delta,
strictness plane, output plane, and hot leaf per row
(`restart/prompts/pass-contracts/PASS-ALPHA.md:64-75`). ORCHESTRATOR binds this
work to the skinny Pass Alpha fan-out and CH1 correctness checks for strictness
plane accuracy (`restart/prompts/ORCHESTRATOR.md:57-65`,
`restart/prompts/ORCHESTRATOR.md:81-89`).

The SK-V10 close authority is the W10 full native Criterion render with run id
`sk-v9-open:criterion-fnv64-6f007527061ee26d`
(`restart/skinny/tranches/sk-v10/research/close/close-redress.md:33-47`).
REDRESS 110 records the same close state: 17 `parse_only` rows remain
`S / NO-GO`, `direct_to_struct` is 6 `A / GO` and 11 `N-direct / NO-GO`, and
`real_typed_struct` is 7 `A / GO`
(`skinny/REDRESS.md:3257-3280`). SK-V10 SYNTHESIS carries that same final
surface and marks parse-only as diagnostic while the direct plane remains the
largest JSON frontier (`restart/skinny/tranches/sk-v10/SYNTHESIS.md:56-67`,
`restart/skinny/tranches/sk-v10/SYNTHESIS.md:213-225`).

## Strictness And Output Planes

The compact RESULTS table publishes the row schema used here: corpus, workload,
outcome, verdict, strictness, UTF-8 validation, output plane, Track 1, Track 2,
sonic-rs strict, side comparators, serde_json, and delta versus sonic strict
(`skinny/RESULTS.md:1-4`).

`parse_only` is diagnostic only for SK-V11. Every parse row is `S / NO-GO`,
strictness `deferred`, `parse_utf8=view-boundary`, and output plane `borrowed
view over offset tape vs DOM`; REDRESS 102 also records the 17-row parse-only
firewall and the validator rejection of parse-only SOTA movement
(`skinny/REDRESS.md:3040-3058`). These rows may inform profile and comparator
diagnosis, but they cannot count as SOTA admissions.

`direct_to_struct` is the active SK-V11 JSON frontier. Its output plane is
`digest`. The six admitted rows are `citm_catalog`, `apache_builds`,
`marine_ik`, `instruments`, `numbers`, and `unicode_basic`
(`skinny/RESULTS.md:9`, `skinny/RESULTS.md:14`, `skinny/RESULTS.md:30`,
`skinny/RESULTS.md:33`, `skinny/RESULTS.md:35`, `skinny/RESULTS.md:41`). Three
of those are strict measured-row admissions (`apache_builds`, `instruments`,
`numbers`); three are inherited deferred/view-boundary digest rows
(`citm_catalog`, `marine_ik`, `unicode_basic`). The eleven remaining direct
rows are `N-direct / NO-GO` and all remain deferred/view-boundary
(`skinny/RESULTS.md:6`, `skinny/RESULTS.md:12`, `skinny/RESULTS.md:17`,
`skinny/RESULTS.md:20`, `skinny/RESULTS.md:23`, `skinny/RESULTS.md:26`,
`skinny/RESULTS.md:28`, `skinny/RESULTS.md:37`, `skinny/RESULTS.md:39`,
`skinny/RESULTS.md:43`, `skinny/RESULTS.md:45`).

`real_typed_struct` is the product-plane SOTA surface. It has seven `A / GO`
rows: `twitter`, `citm_catalog`, `apache_builds`, `github_events`,
`update_center`, `mesh`, and `marine_ik` (`skinny/RESULTS.md:7`,
`skinny/RESULTS.md:10`, `skinny/RESULTS.md:15`, `skinny/RESULTS.md:18`,
`skinny/RESULTS.md:21`, `skinny/RESULTS.md:24`, `skinny/RESULTS.md:31`).
`github_events` is strict measured-row evidence admitted by W6; the other typed
rows remain deferred/view-boundary inherited rows.

## Direct Plane - 11 Remaining N-direct Rows

The SK-V11 direct target is the sonic 1.10x digest gate. The floor below is
`ceil(sonic-rs strict direct Mbps / 1.10)`. Both generated Track 1 and
independent Track 2 must clear the row floor unless SK-V11 explicitly replaces
the gate through CHALLENGE.

| Corpus | RESULTS line | Track 1 | Track 2 | sonic strict direct | serde direct | Delta vs sonic | 1.10x floor | Track 1 gap | Track 2 gap |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| `twitter` | `skinny/RESULTS.md:6` | 11905 | 10968 | 15244 | 10562 | -21.9% | 13859 | -1954 | -2891 |
| `canada` | `skinny/RESULTS.md:12` | 10590 | 10286 | 12157 | 7425 | -12.9% | 11052 | -462 | -766 |
| `github_events` | `skinny/RESULTS.md:17` | 12439 | 11430 | 16206 | 12977 | -23.2% | 14733 | -2294 | -3303 |
| `update_center` | `skinny/RESULTS.md:20` | 8425 | 7620 | 11186 | 8218 | -24.7% | 10170 | -1745 | -2550 |
| `mesh` | `skinny/RESULTS.md:23` | 8562 | 8596 | 9422 | 6932 | -9.1% | 8566 | -4 | +30 |
| `random` | `skinny/RESULTS.md:26` | 7887 | 7132 | 8948 | 6551 | -11.9% | 8135 | -248 | -1003 |
| `gsoc-2018` | `skinny/RESULTS.md:28` | 15056 | 14534 | 23437 | 19440 | -35.8% | 21307 | -6251 | -6773 |
| `unicode_mixed` | `skinny/RESULTS.md:37` | 4700 | 4556 | 10480 | 5233 | -55.1% | 9528 | -4828 | -4972 |
| `unicode_escapes` | `skinny/RESULTS.md:39` | 5069 | 5222 | 14147 | 5193 | -64.2% | 12861 | -7792 | -7639 |
| `distinct_values` | `skinny/RESULTS.md:43` | 6303 | 5654 | 11978 | 8195 | -47.4% | 10890 | -4587 | -5236 |
| `y_string_unicode` | `skinny/RESULTS.md:45` | 5067 | 3746 | 9211 | 7532 | -45.0% | 8374 | -3307 | -4628 |

SK-V11 input: `mesh` is a one-track borderline row, missing by 4 Mbps on Track
1 while Track 2 clears the generic floor. `canada` and `random` are modest
floor misses. `unicode_mixed`, `unicode_escapes`, `distinct_values`,
`y_string_unicode`, and `gsoc-2018` are large misses and should not be treated
as small gate repair. `github_events`, `twitter`, and `update_center` are
middle-distance direct rows.

## Direct Plane - 6 A/GO Rows To Hold

| Corpus | RESULTS line | Strictness | Track 1 | Track 2 | sonic strict direct | serde direct | Delta vs sonic | Note |
|---|---:|---|---:|---:|---:|---:|---:|---|
| `citm_catalog` | `skinny/RESULTS.md:9` | deferred/view-boundary | 21595 | 20592 | 20036 | 13363 | +7.8% | inherited direct guard row |
| `apache_builds` | `skinny/RESULTS.md:14` | strict/measured-row | 11469 | 10368 | 11190 | 9921 | +2.5% | W2 direct row reclamation |
| `marine_ik` | `skinny/RESULTS.md:30` | deferred/view-boundary | 9066 | 9025 | 8235 | 6800 | +10.1% | inherited direct guard row |
| `instruments` | `skinny/RESULTS.md:33` | strict/measured-row | 12040 | 11166 | 12674 | 9497 | -5.0% | W10 fixed-floor direct residual admission |
| `numbers` | `skinny/RESULTS.md:35` | strict/measured-row | 12619 | 12296 | 13038 | 8117 | -3.2% | W2 direct row reclamation |
| `unicode_basic` | `skinny/RESULTS.md:41` | deferred/view-boundary | 9030 | 8360 | 8940 | 5918 | +1.0% | inherited direct guard row |

W10 records the `instruments` direct admission as strict measured-row evidence:
Track 1 12040 Mbps, Track 2 11166 Mbps, sonic-rs direct 12674 Mbps, serde_json
direct 9497 Mbps, and fixed W10 floor 11086 Mbps
(`skinny/REDRESS.md:3224-3255`). W2 records `apache_builds` and `numbers` as
strict measured-row direct reclamations, with their gate provenance and same-run
direct comparator requirements (`skinny/REDRESS.md:3003-3038`).

## Typed Plane - 7 A/GO Rows To Hold

| Corpus | RESULTS line | Strictness | Track 1 | Track 2 | sonic typed strict | serde typed | Delta vs sonic | Note |
|---|---:|---|---:|---:|---:|---:|---:|---|
| `twitter` | `skinny/RESULTS.md:7` | deferred/view-boundary | 18241 | 16492 | 15636 | 16513 | +16.7% | typed product win |
| `citm_catalog` | `skinny/RESULTS.md:10` | deferred/view-boundary | 36135 | 19245 | 22066 | 19114 | +63.8% | largest typed delta |
| `apache_builds` | `skinny/RESULTS.md:15` | deferred/view-boundary | 8534 | 7079 | 8321 | 7073 | +2.5% | typed product win |
| `github_events` | `skinny/RESULTS.md:18` | strict/measured-row | 13137 | 12855 | 12926 | 12848 | +1.6% | W6 root typed row |
| `update_center` | `skinny/RESULTS.md:21` | deferred/view-boundary | 12069 | 10603 | 12727 | 10458 | -5.2% | GO by 1.10x slack, below sonic throughput |
| `mesh` | `skinny/RESULTS.md:24` | deferred/view-boundary | 9690 | 8072 | 9253 | 7499 | +4.7% | typed product win |
| `marine_ik` | `skinny/RESULTS.md:31` | deferred/view-boundary | 12186 | 9985 | 9322 | 10012 | +30.7% | typed product win |

SK-V11 input: the typed plane is the carried SOTA surface, but it is not
uniformly strict measured-row evidence. Six of seven typed rows beat sonic-rs
typed strict by throughput; `update_center` is below sonic by 5.2% while still
classified GO under the 1.10x time slack (`skinny/RESULTS.md:21`). `github_events`
is the only SK-V10-added strict measured-row typed admission, and W6 records its
full-fixture generated Track 1, independent Track 2/oracle, serde_json typed,
sonic-rs typed, checksum parity, and gate provenance
(`skinny/REDRESS.md:3106-3148`).

## Parse-only Diagnostic Rows

`parse_only` must stay out of the SK-V11 SOTA scoreboard. The rows can still
identify corpus clusters and sidecar availability.

| Corpus | RESULTS line | Track 1 | Track 2 | sonic strict DOM | serde_json | Delta vs sonic | Sidecar note from compact row |
|---|---:|---:|---:|---:|---:|---:|---|
| `twitter` | `skinny/RESULTS.md:5` | 15738 | 12245 | 21213 | 5986 | -25.8% | simdjson DOM 24522, yyjson 30931, RapidJSON 4020 |
| `citm_catalog` | `skinny/RESULTS.md:8` | 30045 | 21035 | 25360 | 7618 | +18.5% | simdjson DOM 35822, yyjson 20956, RapidJSON 6760 |
| `canada` | `skinny/RESULTS.md:11` | 17285 | 16797 | 14121 | 5263 | +22.4% | simdjson DOM 11493, yyjson 13003, RapidJSON 5187 |
| `apache_builds` | `skinny/RESULTS.md:13` | 12705 | 12333 | 17492 | 6062 | -27.4% | simdjson DOM 36014, yyjson 16275, RapidJSON 3945 |
| `github_events` | `skinny/RESULTS.md:16` | 15549 | 13304 | 23286 | 7739 | -33.2% | simdjson DOM 39642, yyjson 21426 |
| `update_center` | `skinny/RESULTS.md:19` | 11509 | 9367 | 19802 | 4245 | -41.9% | simdjson DOM 30593, yyjson 18540 |
| `mesh` | `skinny/RESULTS.md:22` | 13552 | 12309 | 11927 | 4636 | +13.6% | yyjson absent in compact row |
| `random` | `skinny/RESULTS.md:25` | 9897 | 7787 | 15521 | 3570 | -36.2% | simdjson DOM 20638 |
| `gsoc-2018` | `skinny/RESULTS.md:27` | 23191 | 21928 | 49490 | 16128 | -53.1% | sidecars absent in compact row |
| `marine_ik` | `skinny/RESULTS.md:29` | 13055 | 12266 | 10009 | 3912 | +30.4% | sidecars absent in compact row |
| `instruments` | `skinny/RESULTS.md:32` | 16895 | 11836 | 19644 | 4775 | -14.0% | RapidJSON 7477 |
| `numbers` | `skinny/RESULTS.md:34` | 19053 | 18477 | 13397 | 6353 | +42.2% | sidecars absent in compact row |
| `unicode_mixed` | `skinny/RESULTS.md:36` | 7881 | 8089 | 18105 | 3954 | -56.5% | simdjson DOM 13150 |
| `unicode_escapes` | `skinny/RESULTS.md:38` | 11550 | 11893 | 18819 | 4799 | -38.6% | simdjson DOM 5637 |
| `unicode_basic` | `skinny/RESULTS.md:40` | 11889 | 10739 | 15906 | 3552 | -25.3% | simdjson DOM 16276 |
| `distinct_values` | `skinny/RESULTS.md:42` | 9799 | 6213 | 17904 | 3943 | -45.3% | simdjson DOM 22825 |
| `y_string_unicode` | `skinny/RESULTS.md:44` | 6389 | 6066 | 13794 | 5775 | -53.7% | simdjson DOM 13627 |

These parse rows are mixed versus sonic and historical C++ sidecars, but they
remain `S / NO-GO` and cannot close SK-V11. Their best use is target diagnosis:
unicode and string rows remain large parse and direct misses, but parse-plane
substrate repair is pre-blocked by REDRESS 96/97/98.

## Sidecar Availability And Non-runnable Comparators

For `direct_to_struct`, same-run native comparators are sonic-rs strict direct
and serde_json direct. The manifest explicitly marks simdjson DOM, simdjson On
Demand, yyjson default, asmjson SWAR, asmjson AVX-512, and RapidJSON default as
absent/not-collected for direct rows. Representative rows show the pattern for
baseline direct (`twitter`, `citm_catalog`) and admitted W2 direct
(`apache_builds`): `skinny/RESULTS.md:52`, `skinny/RESULTS.md:55`, and
`skinny/RESULTS.md:60`.

For `real_typed_struct`, same-run native comparators are sonic-rs typed strict
and serde_json typed. The manifest marks simdjson DOM, simdjson On Demand,
yyjson default, asmjson SWAR, asmjson AVX-512, and RapidJSON default as
absent/not-collected for typed rows. Representative baseline and W6 admitted
typed rows show the pattern at `skinny/RESULTS.md:53`,
`skinny/RESULTS.md:56`, and `skinny/RESULTS.md:64`.

For `parse_only`, sonic-rs strict, sonic-rs lossy, and serde_json are same-run
native. Some C++ sidecars are historical SK-V7 profiles, not same-run native
anchors. `twitter/parse_only` records historical simdjson DOM, yyjson default,
and RapidJSON sidecars, but simdjson On Demand and asmjson are absent
(`skinny/RESULTS.md:51`). Other parse rows follow the same mixed sidecar
availability pattern, with many sidecars explicitly absent or historical rather
than runnable in the W10 close capture.

No grammar-domain comparator exists in the SK-V10 close authority for CSS L4,
Sheets, or BBNF-self. SK-V11 must not infer non-JSON competitor deltas from
these JSON rows. PASS-ALPHA requires future grammar-domain comparators if the
scope expands, but the close packet provides none
(`restart/prompts/pass-contracts/PASS-ALPHA.md:1-5`,
`restart/prompts/pass-contracts/PASS-ALPHA.md:191-205`).

## SK-V11 Input Findings

1. The direct plane has exactly 11 remaining SK-V11 target rows. `mesh`,
   `canada`, and `random` are the smallest floor misses; `unicode_escapes`,
   `unicode_mixed`, `distinct_values`, `y_string_unicode`, and `gsoc-2018` are
   not near-threshold rows.
2. The 6 direct `A / GO` rows must be maintained, but only three are strict
   measured-row admissions. Any SK-V11 claim that uses inherited deferred rows
   should name the strictness limitation.
3. The 7 typed `A / GO` rows remain the product-plane SOTA surface. Six beat
   sonic-rs typed strict directly; `update_center` is a gate pass but not a
   throughput win over sonic.
4. `parse_only` is diagnostic only. It has useful comparator signals, but
   REDRESS 102 and REDRESS 96/97/98 pre-block parse-plane substrate repair and
   parse-only SOTA admission.
5. Sidecar coverage is incomplete outside parse-only. Direct and typed planes
   have same-run sonic/serde comparators; simdjson, yyjson, asmjson, and
   RapidJSON are absent for direct and typed rows in the W10 close manifest.
