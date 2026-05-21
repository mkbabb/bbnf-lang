# Alpha-B Competitor Deltas: SK-V13 Pass Alpha

Scope: competitor deltas available at HEAD for the SK-V12 close feeding SK-V13. Required comparator accounting is strict: unavailable comparator cells are coverage debt, not wins. The binding addendum raises the SK-V13 target to full CSS L4 lightningcss parity: 24 features total, 23 remaining after the single admitted declaration-values row; JSON close accounting targets 51 JSON rows, each requiring Track 1 to be greater than `sonic-rs strict Mbps + 1` or carry an architectural intrinsic-block proof.

## Sources

| Source | Use |
|---|---|
| `restart/prompts/pass-contracts/PASS-ALPHA.md:23`, `:64-75`, `:77-110` | Alpha-B scope, strict comparator gate, and required telemetry schema. |
| `restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md:12-21` | SK-V12 admitted CSS row, cssparser/lightningcss numbers, margin, equality. |
| `skinny/RESULTS.md:3-46`, `:94`, `:146-148` | Current JSON/CSS benchmark table, manifest CSS comparator row, Track 1/Track 2 definitions. |
| `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md:13-18`, `:56-90`, `:96-130` | CSS admitted scope and lightningcss parity gap surface. |
| `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-profile-truth.md:32-43`, `:182-185` | CSS row profile/cycles and SK-V13 capture scope. |
| `skinny/REDRESS.md:8-18`, `:24-37` | Current bench authority and direct/typed blocker framing. |

## HEAD Coverage Snapshot

| Plane | Rows exposed at HEAD | Required / expected | Competitor coverage verdict |
|---|---:|---:|---|
| JSON `parse_only` | 17 | 17 | sonic strict and serde present for all; simdjson DOM 13/17; yyjson 6/17; RapidJSON 6/17; On Demand, asmjson SWAR, asmjson AVX-512 0/17 coverage debt. |
| JSON `direct_to_struct` | 17 | 17 | sonic strict and serde present for all; simdjson/yyjson/asmjson/RapidJSON are n/a coverage debt, not wins. |
| JSON `real_typed_struct` | 7 | 17 | 7 rows have sonic strict and serde; 10 typed rows are absent from the top table and manifest, coverage debt against the 51-row binding. |
| CSS L4 `declaration_values/direct_to_struct` | 1 | 24 feature families | admitted same-plane cssparser/lightningcss row; 23 feature families remain parity debt by binding addendum. |

Missing JSON typed rows at HEAD: `canada`, `random`, `gsoc-2018`, `instruments`, `numbers`, `unicode_mixed`, `unicode_escapes`, `unicode_basic`, `distinct_values`, `y_string_unicode`.

## CSS Admitted Row Deltas

| Row | Track 1 Mbps | Comparator | Comparator Mbps | Delta Mbps | Ratio | Strictness / output plane | Verdict |
|---|---:|---|---:|---:|---:|---|---|
| `css_l4/declaration_values/direct_to_struct/main` | 429.344 | cssparser oracle | 217.427 | +211.918 | 1.975x | strict / `css_l4_declaration_value_fact_stream` | comparator beat, same-plane oracle |
| `css_l4/declaration_values/direct_to_struct/main` | 429.344 | lightningcss strict | 168.930 | +260.415 | 2.542x | strict / `css_l4_declaration_value_fact_stream` | PASS-ADMIT |

Citation: close row in `CAMPAIGN-CLOSE-SK-V12-V12.md:12-21`; manifest comparator details in `skinny/RESULTS.md:94`. The admitted row is declaration-values only, not full stylesheet parity (`sk-v13-scoping-css-parity-gap.md:13-18`). Full lightningcss parity remains open: the scoping surface lists selectors, at-rules, variables, calc/var/url, color functions, gradients, transforms, filters, easing, nesting, vendor/custom at-rules, comments/whitespace, diagnostics, and related families as partial/missing (`sk-v13-scoping-css-parity-gap.md:56-130`). Under the binding addendum, count this as 1/24 covered and 23/24 remaining.

## JSON Direct vs sonic-rs Strict

Only 3/17 direct rows satisfy `Track 1 > sonic-rs strict Mbps + 1`. Rows marked `miss` need an architectural intrinsic-block proof or a later admission to satisfy the binding; GO verdicts that do not clear the strict comparator-plus-one boundary are still misses under the new rule.

| Corpus | Track 1 | sonic strict | Delta Mbps | Delta % | RESULTS cite | Binding status |
|---|---:|---:|---:|---:|---|---|
| twitter | 12068 | 15150 | -3082 | -20.3% | `skinny/RESULTS.md:6` | miss |
| citm_catalog | 21623 | 20026 | +1597 | +8.0% | `skinny/RESULTS.md:9` | beats |
| canada | 10362 | 11745 | -1383 | -11.8% | `skinny/RESULTS.md:12` | miss |
| apache_builds | 11397 | 11134 | +263 | +2.4% | `skinny/RESULTS.md:14` | beats |
| github_events | 12362 | 16336 | -3974 | -24.3% | `skinny/RESULTS.md:17` | miss |
| update_center | 8472 | 11239 | -2767 | -24.6% | `skinny/RESULTS.md:20` | miss |
| mesh | 8791 | 9841 | -1050 | -10.7% | `skinny/RESULTS.md:23` | miss |
| random | 7747 | 8907 | -1160 | -13.0% | `skinny/RESULTS.md:26` | miss |
| gsoc-2018 | 15228 | 23439 | -8211 | -35.0% | `skinny/RESULTS.md:28` | miss |
| marine_ik | 9443 | 8503 | +940 | +11.1% | `skinny/RESULTS.md:30` | beats |
| instruments | 12076 | 12433 | -357 | -2.9% | `skinny/RESULTS.md:33` | miss |
| numbers | 12240 | 12676 | -436 | -3.4% | `skinny/RESULTS.md:35` | miss |
| unicode_mixed | 4617 | 10433 | -5816 | -55.7% | `skinny/RESULTS.md:37` | miss |
| unicode_escapes | 5114 | 14134 | -9020 | -63.8% | `skinny/RESULTS.md:39` | miss |
| unicode_basic | 8134 | 8842 | -708 | -8.0% | `skinny/RESULTS.md:41` | miss |
| distinct_values | 6005 | 11503 | -5498 | -47.8% | `skinny/RESULTS.md:43` | miss |
| y_string_unicode | 4975 | 8228 | -3253 | -39.5% | `skinny/RESULTS.md:45` | miss |

## JSON Typed vs sonic-rs Strict

Available typed rows: 7/17. Six satisfy `Track 1 > sonic-rs strict Mbps + 1`. `update_center` is a miss despite its GO verdict because the binding requires the strict comparator-plus-one boundary or intrinsic-block proof.

| Corpus | Track 1 | sonic strict | Delta Mbps | Delta % | RESULTS cite | Binding status |
|---|---:|---:|---:|---:|---|---|
| twitter | 18887 | 15761 | +3126 | +19.8% | `skinny/RESULTS.md:7` | beats |
| citm_catalog | 36430 | 22186 | +14244 | +64.2% | `skinny/RESULTS.md:10` | beats |
| apache_builds | 8613 | 8322 | +291 | +3.5% | `skinny/RESULTS.md:15` | beats |
| github_events | 13098 | 12837 | +261 | +2.0% | `skinny/RESULTS.md:18` | beats |
| update_center | 12335 | 12887 | -552 | -4.3% | `skinny/RESULTS.md:21` | miss |
| mesh | 9821 | 9132 | +689 | +7.5% | `skinny/RESULTS.md:24` | beats |
| marine_ik | 12214 | 9230 | +2984 | +32.3% | `skinny/RESULTS.md:31` | beats |

Coverage debt: typed comparator rows are not available at HEAD for `canada`, `random`, `gsoc-2018`, `instruments`, `numbers`, `unicode_mixed`, `unicode_escapes`, `unicode_basic`, `distinct_values`, and `y_string_unicode`; these cannot be counted as wins.

## JSON parse_only Comparator Availability and Deltas

`parse_only` is now admission-eligible by binding addendum. Current HEAD has sonic-rs strict for all 17 rows. Five rows satisfy `Track 1 > sonic-rs strict Mbps + 1`: `citm_catalog`, `canada`, `mesh`, `marine_ik`, `numbers`. All n/a comparator cells below are coverage debt.

| Corpus | Track 1 | sonic strict | Delta sonic | simdjson DOM delta | yyjson delta | RapidJSON delta | Binding status | RESULTS cite |
|---|---:|---:|---:|---:|---:|---:|---|---|
| twitter | 13490 | 18716 | -5226 | -11032 | -17441 | +9470 | miss | `skinny/RESULTS.md:5` |
| citm_catalog | 24140 | 20645 | +3495 | -11682 | +3184 | +17380 | beats | `skinny/RESULTS.md:8` |
| canada | 7678 | 4302 | +3376 | -3815 | -5325 | +2491 | beats | `skinny/RESULTS.md:11` |
| apache_builds | 5434 | 8919 | -3485 | -30580 | -10841 | +1489 | miss | `skinny/RESULTS.md:13` |
| github_events | 7026 | 12263 | -5237 | -32616 | -14400 | coverage debt | miss | `skinny/RESULTS.md:16` |
| update_center | 5344 | 13836 | -8492 | -25249 | -13196 | coverage debt | miss | `skinny/RESULTS.md:19` |
| mesh | 9895 | 8980 | +915 | +481 | coverage debt | coverage debt | beats | `skinny/RESULTS.md:22` |
| random | 4156 | 7116 | -2960 | -16482 | coverage debt | +630 | miss | `skinny/RESULTS.md:25` |
| gsoc-2018 | 9129 | 16925 | -7796 | coverage debt | coverage debt | coverage debt | miss | `skinny/RESULTS.md:27` |
| marine_ik | 10024 | 7333 | +2691 | coverage debt | coverage debt | coverage debt | beats | `skinny/RESULTS.md:29` |
| instruments | 10598 | 15207 | -4609 | coverage debt | coverage debt | +3121 | miss | `skinny/RESULTS.md:32` |
| numbers | 14464 | 10231 | +4233 | coverage debt | coverage debt | coverage debt | beats | `skinny/RESULTS.md:34` |
| unicode_mixed | 4568 | 6942 | -2374 | -8582 | coverage debt | coverage debt | miss | `skinny/RESULTS.md:36` |
| unicode_escapes | 4741 | 14603 | -9862 | -896 | coverage debt | coverage debt | miss | `skinny/RESULTS.md:38` |
| unicode_basic | 9924 | 12757 | -2833 | -6352 | coverage debt | coverage debt | miss | `skinny/RESULTS.md:40` |
| distinct_values | 9198 | 17080 | -7882 | -13627 | coverage debt | coverage debt | miss | `skinny/RESULTS.md:42` |
| y_string_unicode | 6313 | 13842 | -7529 | -7314 | coverage debt | coverage debt | miss | `skinny/RESULTS.md:44` |

Comparator availability on `parse_only`: sonic-rs strict 17/17, sonic-rs lossy 17/17 as flaw probe only, serde_json 17/17, simdjson DOM 13/17, yyjson 6/17, RapidJSON 6/17, simdjson On Demand 0/17, asmjson SWAR 0/17, asmjson AVX-512 0/17.

## Admission Implications

1. CSS has a real strict same-plane win over lightningcss/cssparser for the single admitted declaration-values row, but the new binding converts the rest of CSS L4 into parity debt: 23 of 24 feature families remain.
2. JSON direct is still the weak plane: 14/17 direct rows miss sonic strict by the new strict comparator-plus-one rule.
3. JSON typed is promising but under-covered: 6/7 available rows beat sonic strict, 1 misses, and 10 typed rows are missing at HEAD.
4. `parse_only` has five admission-eligible beats over sonic strict, but most non-sonic comparator columns are partial or absent; n/a cells must drive SK-V13 telemetry expansion rather than inflate win counts.
