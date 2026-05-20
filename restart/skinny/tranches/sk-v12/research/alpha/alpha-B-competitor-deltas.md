# SK-V12 Pass Alpha - alpha-B Competitor Deltas

Pass: Alpha.
Agent: alpha-B.
Date: 2026-05-20.
Scope: SK-V11 close competitor delta extraction for SK-V12 input.

## Contract And Source Boundary

PASS-ALPHA assigns alpha-B to compute comparator deltas for Track 1, with
strictness and output plane disclosed for every row. The strict comparator gate
requires sonic-rs strict, simdjson DOM/On Demand, yyjson, asmjson, RapidJSON,
and serde_json where runnable, with explicit strictness/output-plane treatment.

The current close authority is the unchanged SK-V11 `skinny/RESULTS.md` table.
SK-V11 W9 made no behavior source, generated runtime, benchmark body, gate
semantic, or `skinny/RESULTS.md` change. The close state is:

| Plane | Current SK-V11 state | Authority |
|---|---:|---|
| `parse_only` | 16 `S / NO-GO`, 1 `L / NO-GO` | `skinny/RESULTS.md:5-45`, `restart/skinny/tranches/sk-v11/research/close/close-redress.md:19-25`, `skinny/REDRESS.md:3539-3541` |
| `direct_to_struct` | 4 `A / GO`, 13 `N-direct / NO-GO` | `skinny/RESULTS.md:5-45`, `restart/skinny/tranches/sk-v11/research/close/close-redress.md:19-25`, `skinny/REDRESS.md:3539-3544` |
| `real_typed_struct` | 7 `A / GO` | `skinny/RESULTS.md:5-45`, `restart/skinny/tranches/sk-v11/research/close/close-redress.md:19-25`, `skinny/REDRESS.md:3539-3541` |
| Overall | `N-direct / NoGo`, measured fixpoint close | `restart/skinny/tranches/sk-v11/research/close/close-redress.md:43-63`, `skinny/REDRESS.md:3531-3553` |

This artifact uses Mbps deltas as `(Track 1 / comparator - 1) * 100`, rounded
to one decimal. The direct residual floor is `ceil(sonic-rs strict direct Mbps /
1.10)`, matching REDRESS 119.

## Strictness And Output-Plane Rules

- `parse_only` is diagnostic only. Its output plane is `borrowed view over
  offset tape vs DOM`; strictness is still `deferred` with
  `parse_utf8=view-boundary`. Raw parse wins do not count as SOTA evidence.
- `direct_to_struct` is the SK-V12 direct residual frontier. Its output plane
  is `digest`. Direct rows compare only against same-run sonic-rs strict direct
  and same-run serde_json direct. Direct sidecars for simdjson, yyjson,
  asmjson, and RapidJSON are absent and cannot be treated as SOTA evidence.
- `real_typed_struct` is the typed product plane. Its output plane is
  `typed direct`. Typed rows compare only against same-run sonic-rs strict
  typed and same-run serde_json typed. Typed sidecars for simdjson, yyjson,
  asmjson, and RapidJSON are absent and cannot be treated as SOTA evidence.
- sonic-rs lossy is a permissive flaw probe where present. It is not a strict
  anchor.
- Historical C++ sidecars in parse-only are diagnosis only. Absent sidecars are
  absence evidence, not negative or positive SOTA evidence.

## Comparator Availability By Plane

Derived from the `SK-V9 W0 Telemetry Manifest` still consumed by the current
gate surface (`skinny/RESULTS.md:47-91`).

| Plane | Same-run native strict comparators | Historical sidecars | Explicitly absent sidecars | SOTA use |
|---|---|---|---|---|
| `parse_only` / borrowed view vs DOM | sonic-rs strict DOM 17/17; serde_json DOM 17/17 | simdjson DOM 13/17; yyjson default 6/17; RapidJSON default 6/17 | simdjson On Demand 17/17; asmjson SWAR 17/17; asmjson AVX-512 17/17 | Diagnostic only; parse-only is not an admission plane. |
| `direct_to_struct` / digest | sonic-rs strict direct 17/17; serde_json direct 17/17 | none | simdjson DOM 17/17; simdjson On Demand 17/17; yyjson default 17/17; asmjson SWAR 17/17; asmjson AVX-512 17/17; RapidJSON default 17/17 | Active direct plane; only same-run sonic/serde rows are comparable. |
| `real_typed_struct` / typed direct | sonic-rs strict typed 7/7; serde_json typed 7/7 | none | simdjson DOM 7/7; simdjson On Demand 7/7; yyjson default 7/7; asmjson SWAR 7/7; asmjson AVX-512 7/7; RapidJSON default 7/7 | Product plane; only same-run sonic/serde rows are comparable. |
| non-JSON grammar-domain | none | none | no CSS L4, Sheets, or BBNF-self comparator rows in SK-V11 close authority | Not available; REDRESS 112/113 block generated non-JSON baseline claims. |

Representative manifest rows: parse `twitter` shows same-run sonic/serde,
permissive sonic lossy, historical simdjson/yyjson/RapidJSON, and absent
simdjson On Demand plus asmjson (`skinny/RESULTS.md:51`). Direct `twitter`
shows same-run sonic/serde direct and absent C++ sidecars (`skinny/RESULTS.md:52`).
Typed `twitter` shows same-run sonic/serde typed and absent C++ sidecars
(`skinny/RESULTS.md:53`).

## Direct Residual Floor Table For SK-V12

This is the load-bearing SK-V12 residual floor table. It uses sonic-rs strict
direct, not parse-only sonic and not typed sonic. REDRESS 119 is the close
authority for the fixpoint proof (`skinny/REDRESS.md:3495-3527`).

| Row | Track 1 | Track 2 | sonic strict direct | serde_json direct | Delta vs sonic | Delta vs serde | 1.10x floor | Track 1 gap | Track 2 gap | SK-V12 residual class |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|
| `twitter/direct_to_struct` | 11613 | 10816 | 15113 | 10286 | -23.2% | +12.9% | 13740 | -2127 | -2924 | dual floor miss; W5/W7 blocked; exhausted in REDRESS 119 |
| `canada/direct_to_struct` | 10316 | 9819 | 11700 | 6967 | -11.8% | +48.1% | 10637 | -321 | -818 | small Track 1 miss, material Track 2 miss; W3 numeric sibling rejected |
| `github_events/direct_to_struct` | 11918 | 10596 | 14743 | 12505 | -19.2% | -4.7% | 13403 | -1485 | -2807 | dual floor miss; W5/W7 blocked |
| `update_center/direct_to_struct` | 8187 | 7474 | 11064 | 8056 | -26.0% | +1.6% | 10059 | -1872 | -2585 | dual floor miss; W5/W7 blocked |
| `mesh/direct_to_struct` | 8561 | 8652 | 9542 | 7037 | -10.3% | +21.7% | 8675 | -114 | -23 | near-threshold dual miss; W3 measured-rejected |
| `random/direct_to_struct` | 7693 | 6949 | 8665 | 6280 | -11.2% | +22.5% | 7878 | -185 | -929 | small Track 1 miss, material Track 2 miss; W4 probe rejected |
| `gsoc-2018/direct_to_struct` | 2665 | 2578 | 4110 | 3364 | -35.2% | -20.8% | 3737 | -1072 | -1159 | large movemask/string residual; no W8 source candidate |
| `instruments/direct_to_struct` | 11569 | 10736 | 9865 | 9218 | +17.3% | +25.5% | 8969 | +2600 | +1767 | numerically above floor but W0-clamped; docs-only admission pre-blocked |
| `numbers/direct_to_struct` | 4479 | 2366 | 2667 | 1782 | +67.9% | +151.3% | 2425 | +2054 | -59 | Track 2 floor miss and W0-clamped; W3 numeric route rejected |
| `unicode_mixed/direct_to_struct` | 3753 | 2427 | 2846 | 1392 | +31.9% | +169.6% | 2588 | +1165 | -161 | Track 2 floor miss and W0-clamped; W6 decoded-source route blocked |
| `unicode_escapes/direct_to_struct` | 1345 | 1341 | 3785 | 2362 | -64.5% | -43.1% | 3441 | -2096 | -2100 | large unicode escape miss; W5/W6 and prior proof-only limits block |
| `distinct_values/direct_to_struct` | 1750 | 1625 | 2923 | 3355 | -40.1% | -47.8% | 2658 | -908 | -1033 | large string/digest miss; W5/W7 blocked |
| `y_string_unicode/direct_to_struct` | 1983 | 1029 | 4344 | 3527 | -54.4% | -43.8% | 3950 | -1967 | -2921 | large unicode/string miss; W5/W6 and prior proof-only limits block |

SK-V12 input finding: the direct plane is a measured fixpoint, not a near-free
closure. `mesh`, `canada`, and `random` are the closest Track 1 residuals, but
their Track 2 gaps and rejected routes are already accounted for in REDRESS
114-119. `instruments`, `numbers`, and `unicode_mixed` have positive Track 1
sonic deltas but are not admissions: `instruments` is W0-clamped, while
`numbers` and `unicode_mixed` still miss the Track 2 floor.

## Direct A/GO Rows To Hold

These rows are digest-plane evidence only. They compare against sonic-rs strict
direct and serde_json direct. They do not acquire simdjson/yyjson/asmjson
evidence because those sidecars are absent for direct rows.

| Corpus | RESULTS line | Strictness / UTF-8 | Track 1 | Track 2 | sonic strict direct | serde_json direct | Delta vs sonic | Delta vs serde | Note |
|---|---:|---|---:|---:|---:|---:|---:|---:|---|
| `citm_catalog` | `skinny/RESULTS.md:9` | deferred / view-boundary | 18563 | 17787 | 15530 | 9540 | +19.5% | +94.6% | inherited direct guard row |
| `apache_builds` | `skinny/RESULTS.md:14` | strict / measured-row | 11254 | 10189 | 10995 | 9723 | +2.4% | +15.7% | W2 direct row reclamation carried forward |
| `marine_ik` | `skinny/RESULTS.md:30` | deferred / view-boundary | 8938 | 9437 | 8473 | 6896 | +5.5% | +29.6% | inherited direct guard row |
| `unicode_basic` | `skinny/RESULTS.md:41` | deferred / view-boundary | 2299 | 2227 | 2353 | 1592 | -2.3% | +44.4% | GO by 1.10x time slack, not throughput win over sonic |

## Typed Product Plane

These rows use sonic-rs strict typed, not sonic-rs strict direct. Track 2 is the
typed oracle/structural reference recorded by the benchmark; the product-plane
SOTA comparison is Track 1 typed direct versus same-run strict typed
comparators. C++ sidecars are absent for every typed row.

| Corpus | RESULTS line | Strictness / UTF-8 | Track 1 typed | Track 2/oracle | sonic strict typed | serde_json typed | Delta vs sonic | Delta vs serde | Note |
|---|---:|---|---:|---:|---:|---:|---:|---:|---|
| `twitter` | `skinny/RESULTS.md:7` | deferred / view-boundary | 17740 | 15912 | 15010 | 15664 | +18.2% | +13.3% | typed product win |
| `citm_catalog` | `skinny/RESULTS.md:10` | deferred / view-boundary | 30539 | 17675 | 20726 | 18295 | +47.3% | +66.9% | largest current typed delta |
| `apache_builds` | `skinny/RESULTS.md:15` | deferred / view-boundary | 8478 | 6892 | 8106 | 6807 | +4.6% | +24.5% | typed product win |
| `github_events` | `skinny/RESULTS.md:18` | strict / measured-row | 11871 | 12275 | 12224 | 12249 | -2.9% | -3.1% | strict measured-row GO by 1.10x time slack |
| `update_center` | `skinny/RESULTS.md:21` | deferred / view-boundary | 11851 | 10358 | 12467 | 10143 | -4.9% | +16.8% | GO by 1.10x time slack, below sonic throughput |
| `mesh` | `skinny/RESULTS.md:24` | deferred / view-boundary | 9403 | 7897 | 8923 | 7562 | +5.4% | +24.3% | typed product win |
| `marine_ik` | `skinny/RESULTS.md:31` | deferred / view-boundary | 11788 | 10096 | 9010 | 10036 | +30.8% | +17.5% | typed product win |

SK-V12 input finding: typed remains the best current product-plane surface, but
it is not uniformly a throughput win over sonic-rs strict typed. `github_events`
and `update_center` are gate passes under time slack, not positive Mbps deltas.
Only `github_events` is strict measured-row typed evidence in the current
surface; the other typed rows remain deferred/view-boundary rows.

## Parse-Only Diagnostic Comparator Deltas

These rows use sonic-rs strict DOM and serde_json DOM. They may identify
clusters, but they are not SOTA evidence because the output plane is borrowed
view over offset tape vs DOM and the strictness path is deferred/view-boundary.
Historical sidecar deltas are shown only where the compact row has values.

| Corpus | Track 1 parse | sonic strict DOM | serde_json DOM | Delta vs sonic | Delta vs serde | Historical sidecar note |
|---|---:|---:|---:|---:|---:|---|
| `twitter` | 10474 | 16988 | 5905 | -38.3% | +77.4% | simdjson DOM -57.3%; yyjson -66.1%; RapidJSON present |
| `citm_catalog` | 26791 | 21564 | 6824 | +24.2% | +292.6% | simdjson DOM -25.2%; yyjson +27.8%; RapidJSON present |
| `canada` | 15544 | 13462 | 4888 | +15.5% | +218.0% | simdjson DOM +35.2%; yyjson +19.5%; RapidJSON present |
| `apache_builds` | 12733 | 17291 | 5906 | -26.4% | +115.6% | simdjson DOM -64.6%; yyjson -21.8%; RapidJSON present |
| `github_events` | 14805 | 22578 | 7578 | -34.4% | +95.4% | simdjson DOM -62.7%; yyjson -30.9% |
| `update_center` | 11493 | 18962 | 4045 | -39.4% | +184.1% | simdjson DOM -62.4%; yyjson -38.0% |
| `mesh` | 13325 | 11679 | 4549 | +14.1% | +192.9% | simdjson DOM +41.5%; yyjson absent |
| `random` | 7747 | 14172 | 2304 | -45.3% | +236.2% | simdjson DOM -62.5%; RapidJSON present |
| `gsoc-2018` | 4887 | 8472 | 2585 | -42.3% | +89.1% | C++ sidecars absent |
| `marine_ik` | 10675 | 9376 | 3689 | +13.9% | +189.4% | C++ sidecars absent |
| `instruments` | 16574 | 19055 | 4589 | -13.0% | +261.2% | RapidJSON present; simdjson/yyjson absent |
| `numbers` | 17941 | 13198 | 4020 | +35.9% | +346.3% | C++ sidecars absent |
| `unicode_mixed` | 1883 | 15137 | 3780 | -87.6% | -50.2% | simdjson DOM -85.7%; yyjson absent |
| `unicode_escapes` | 3733 | 7235 | 1336 | -48.4% | +179.4% | simdjson DOM -33.8%; yyjson absent |
| `unicode_basic` | 3217 | 4354 | 867 | -26.1% | +271.0% | simdjson DOM -80.2%; yyjson absent |
| `distinct_values` | 2335 | 4883 | 1177 | -52.2% | +98.4% | simdjson DOM -89.8%; yyjson absent |
| `y_string_unicode` | 1965 | 6227 | 2473 | -68.4% | -20.5% | simdjson DOM -85.6%; yyjson absent |

SK-V12 input finding: parse-only wins against sonic on `citm_catalog`,
`canada`, `mesh`, `marine_ik`, and `numbers` are diagnostic only. They do not
override REDRESS 96/97/98/102 parse-plane retirement, and they do not repair the
direct residual fixpoint recorded by REDRESS 119.

## Carry-Forward For SK-V12

1. Score SK-V12 against the right plane: direct rows use sonic-rs strict direct;
   typed rows use sonic-rs strict typed; parse-only rows are diagnostic.
2. Do not use absent sidecars as SOTA evidence. For direct and typed rows,
   simdjson DOM/On Demand, yyjson, asmjson SWAR/AVX-512, and RapidJSON are all
   absent in the current authority.
3. Do not use historical parse-only sidecars as same-run strict anchors. They
   can rank targets, but they cannot close a product or direct SOTA claim.
4. Treat the 13 direct residual rows as exhausted unless SK-V12 first names a
   material differential beyond REDRESS 114-119 with fresh profile and
   micro-proof evidence.
5. The material non-JSON remainder is upstream of this competitor table:
   REDRESS 112/113 block grammar-generalized claims until a generated non-JSON
   baseline and independent oracle/report lane exist.
