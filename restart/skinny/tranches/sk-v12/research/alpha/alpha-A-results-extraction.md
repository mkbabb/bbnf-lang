# SK-V12 Alpha-A Results Extraction

Date: 2026-05-20.

Pass: Alpha SK-V11 -> SK-V12, alpha-A.

Sources read:

- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/skinny/tranches/sk-v11/research/close/close-redress.md`
- `restart/skinny/tranches/sk-v11/HANDOFF.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` through REDRESS 120

## Close Surface

SK-V11 closes as a measured fixpoint only. REDRESS 120 states that W9 made no
behavior source, generated runtime, benchmark body, gate semantic, or
`skinny/RESULTS.md` change. The final measured surface is therefore the
unchanged SK-V11-open result surface:

| Workload family | Current surface | Close role |
|---|---:|---|
| `parse_only` | 16 `S / NO-GO`, 1 `L / NO-GO` | Diagnostic only; not SOTA admission |
| `direct_to_struct` | 4 `A / GO`, 13 `N-direct / NO-GO` | Primary direct fixpoint surface |
| `real_typed_struct` | 7 `A / GO` | Guarded product-plane wins |
| Overall | `N-direct / NoGo` | Still not direct close |

No SK-V11 wave moved a `skinny/RESULTS.md` row. W1a admitted only the companion
non-JSON gate/report lane, W1b and W2 blocked the generated non-JSON baseline
path, W3-W7 rejected or blocked direct residual routes, W8 recorded the direct
fixpoint, and W9 only closed the measured fixpoint for Pass Alpha feedback.

The `skinny/RESULTS.md` historical delta column is still `Delta vs SK-V6` and
is `n/a` for these rows because W0b had no machine-readable SK-V6 baseline.
The relevant SK-V11 delta is therefore "no row movement from W0 through W9".

There are no current `path_lookup`, `unicode_string_float`, `memory`, or
`cycles_per_byte` rows in `skinny/RESULTS.md`; the active surface is
`parse_only`, `direct_to_struct`, and `real_typed_struct`.

## Comparator And Plane Notes

- Parse-only rows use `deferred` strictness, `view-boundary` UTF-8, and the
  `borrowed view over offset tape vs DOM` output plane. They publish broader
  parser comparators where available, but every row remains `NO-GO`.
- Direct rows use the `digest` output plane. Their live strict anchors are
  same-run native `sonic-rs strict` direct digest and same-run `serde_json`
  direct digest; C++ DOM comparators are absent for direct rows.
- Typed rows use the `typed direct` output plane. Their live strict anchors are
  same-run native `sonic-rs strict` typed direct and same-run `serde_json`
  typed direct; Track 2 is a structural/product oracle, not the SOTA gate.
- `simdjson On Demand`, `asmjson SWAR`, and `asmjson AVX-512` are absent or
  uncollected in this Apple Silicon close surface. Historical C++ sidecars
  remain disclosure anchors only, not SK-V11 strict close anchors.
- Every row reports `hot-leaf=criterion-slope-profile`; the row identity is
  `json/<corpus>/<workload>/main`.

## Parse-Only Surface

Common plane: `deferred` strictness, `view-boundary` UTF-8, borrowed view over
offset tape vs DOM. All rows are `NO-GO` diagnostic rows and had no SK-V11 row
movement.

| Corpus | Outcome | Track 1 / Track 2 Mbps | Comparator anchors, Mbps | Delta vs anchors | Signal |
|---|---:|---:|---|---|---|
| `twitter` | `S` | 10474 / 7757 | sonic 16988; lossy 20801; simdjson 24522; yyjson 30931; RapidJSON 4020; serde 5905 | sonic -38.3%; simdjson -57.3%; yyjson -66.1% | NO-GO parse gate classified S |
| `citm_catalog` | `S` | 26791 / 18271 | sonic 21564; lossy 21197; simdjson 35822; yyjson 20956; RapidJSON 6760; serde 6824 | sonic +24.2%; simdjson -25.2%; yyjson +27.8% | NO-GO parse gate classified S |
| `canada` | `L` | 15544 / 16215 | sonic 13462; lossy 13554; simdjson 11493; yyjson 13003; RapidJSON 5187; serde 4888 | sonic +15.5%; simdjson +35.2%; yyjson +19.5% | NO-GO parse gate classified L |
| `apache_builds` | `S` | 12733 / 12196 | sonic 17291; lossy 17505; simdjson 36014; yyjson 16275; RapidJSON 3945; serde 5906 | sonic -26.4%; simdjson -64.6%; yyjson -21.8% | NO-GO parse gate classified S |
| `github_events` | `S` | 14805 / 12791 | sonic 22578; lossy 22623; simdjson 39642; yyjson 21426; RapidJSON n/a; serde 7578 | sonic -34.4%; simdjson -62.7%; yyjson -30.9% | NO-GO parse gate classified S |
| `update_center` | `S` | 11493 / 9033 | sonic 18962; lossy 19039; simdjson 30593; yyjson 18540; RapidJSON n/a; serde 4045 | sonic -39.4%; simdjson -62.4%; yyjson -38.0% | NO-GO parse gate classified S |
| `mesh` | `S` | 13325 / 12128 | sonic 11679; lossy 11780; simdjson 9414; yyjson n/a; RapidJSON n/a; serde 4549 | sonic +14.1%; simdjson +41.5%; yyjson n/a | NO-GO parse gate classified S |
| `random` | `S` | 7747 / 7554 | sonic 14172; lossy 15084; simdjson 20638; yyjson n/a; RapidJSON 3526; serde 2304 | sonic -45.3%; simdjson -62.5%; yyjson n/a | NO-GO parse gate classified S |
| `gsoc-2018` | `S` | 4887 / 4544 | sonic 8472; lossy 8558; simdjson n/a; yyjson n/a; RapidJSON n/a; serde 2585 | sonic -42.3%; simdjson n/a; yyjson n/a | NO-GO parse gate classified S |
| `marine_ik` | `S` | 10675 / 11700 | sonic 9376; lossy 9788; simdjson n/a; yyjson n/a; RapidJSON n/a; serde 3689 | sonic +13.8%; simdjson n/a; yyjson n/a | NO-GO parse gate classified S |
| `instruments` | `S` | 16574 / 11587 | sonic 19055; lossy 19122; simdjson n/a; yyjson n/a; RapidJSON 7477; serde 4589 | sonic -13.0%; simdjson n/a; yyjson n/a | NO-GO parse gate classified S |
| `numbers` | `S` | 17941 / 18328 | sonic 13198; lossy 9906; simdjson n/a; yyjson n/a; RapidJSON n/a; serde 4020 | sonic +35.9%; simdjson n/a; yyjson n/a | NO-GO parse gate classified S |
| `unicode_mixed` | `S` | 1883 / 7326 | sonic 15137; lossy 14997; simdjson 13150; yyjson n/a; RapidJSON n/a; serde 3780 | sonic -87.6%; simdjson -85.7%; yyjson n/a | NO-GO parse gate classified S |
| `unicode_escapes` | `S` | 3733 / 2421 | sonic 7235; lossy 5881; simdjson 5637; yyjson n/a; RapidJSON n/a; serde 1336 | sonic -48.4%; simdjson -33.8%; yyjson n/a | NO-GO parse gate classified S |
| `unicode_basic` | `S` | 3217 / 2985 | sonic 4354; lossy 6261; simdjson 16276; yyjson n/a; RapidJSON n/a; serde 867 | sonic -26.1%; simdjson -80.2%; yyjson n/a | NO-GO parse gate classified S |
| `distinct_values` | `S` | 2335 / 1675 | sonic 4883; lossy 5357; simdjson 22825; yyjson n/a; RapidJSON n/a; serde 1177 | sonic -52.2%; simdjson -89.8%; yyjson n/a | NO-GO parse gate classified S |
| `y_string_unicode` | `S` | 1965 / 2695 | sonic 6227; lossy 6282; simdjson 13627; yyjson n/a; RapidJSON n/a; serde 2473 | sonic -68.5%; simdjson -85.6%; yyjson n/a | NO-GO parse gate classified S |

## Direct-To-Struct Surface

Common plane: generated Track 1 SinkOnly digest vs independent hand Track 2
SinkOnly digest. The 13 residual rows below are exhausted in SK-V11 by REDRESS
119 unless a future pass names a material differential beyond REDRESS 114-119
with fresh profile and micro-proof evidence.

| Corpus | Outcome / verdict | Strictness / output | Track 1 / Track 2 Mbps | sonic / serde Mbps | Delta vs sonic | SK-V11 delta or fixpoint note |
|---|---|---|---:|---:|---:|---|
| `twitter` | `N-direct / NO-GO` | deferred / digest | 11613 / 10816 | 15113 / 10286 | -23.2% | Floor 13740; W5 string-span and W7 digest routes blocked; no W8a source candidate remains. |
| `citm_catalog` | `A / GO` | deferred / digest | 18563 / 17787 | 15530 / 9540 | +19.5% | Admitted direct row; unchanged by W8/W9. |
| `canada` | `N-direct / NO-GO` | deferred / digest | 10316 / 9819 | 11700 / 6967 | -11.8% | Floor 10637; W3 numeric route rejected on sibling `mesh`; larger Track 2 floor gap; no W8a numeric candidate remains. |
| `apache_builds` | `A / GO` | strict / digest | 11254 / 10189 | 10995 / 9723 | +2.4% | PASS W2 direct reclamation signal with strict measured-row contract; unchanged by W8/W9. |
| `github_events` | `N-direct / NO-GO` | deferred / digest | 11918 / 10596 | 14743 / 12505 | -19.2% | Floor 13403; W5 string-span blocked; W7 digest visible-bucket math cannot close both tracks; no W8a candidate remains. |
| `update_center` | `N-direct / NO-GO` | deferred / digest | 8187 / 7474 | 11064 / 8056 | -26.0% | Floor 10059; W5 string-span blocked; W7 digest route floor-insufficient; no W8a candidate remains. |
| `mesh` | `N-direct / NO-GO` | deferred / digest | 8561 / 8652 | 9542 / 7037 | -10.3% | Floor 8675; W3 `number_span_emit_slot` measured 3835 / 3614 and was reverted; row remains uncloseable in SK-V11. |
| `random` | `N-direct / NO-GO` | deferred / digest | 7693 / 6949 | 8665 / 6280 | -11.2% | Floor 7878; W4 `container_tail_next` probe measured 3518 / 3498 and was reverted; W5/W7 blocked. |
| `gsoc-2018` | `N-direct / NO-GO` | deferred / digest | 2665 / 2578 | 4110 / 3364 | -35.2% | Floor 3737; movemask/string-scan residual; W5 and W7 leave no accepted source authority. |
| `marine_ik` | `A / GO` | deferred / digest | 8938 / 9437 | 8473 / 6896 | +5.5% | Admitted direct row; unchanged by W8/W9. |
| `instruments` | `N-direct / NO-GO` | deferred / digest | 11569 / 10736 | 9865 / 9218 | +17.3% | Floor 8969; numerically above floor but W0-clamped; docs-only admission is pre-blocked. |
| `numbers` | `N-direct / NO-GO` | deferred / digest | 4479 / 2366 | 2667 / 1782 | +67.9% | Floor 2425; Track 2 misses floor and row is W0-clamped; W3 numeric route rejected. |
| `unicode_mixed` | `N-direct / NO-GO` | deferred / digest | 3753 / 2427 | 2846 / 1392 | +31.9% | Floor 2588; Track 2 misses floor and row is W0-clamped; W6 decoded-source route blocked by REDRESS 117. |
| `unicode_escapes` | `N-direct / NO-GO` | deferred / digest | 1345 / 1341 | 3785 / 2362 | -64.5% | Floor 3441; Unicode escape route blocked by W5/W6 and SK-V10 REDRESS 107/108 proof-only limits. |
| `unicode_basic` | `A / GO` | deferred / digest | 2299 / 2227 | 2353 / 1592 | -2.3% | Admitted direct row within gate; unchanged by W8/W9. |
| `distinct_values` | `N-direct / NO-GO` | deferred / digest | 1750 / 1625 | 2923 / 3355 | -40.1% | Floor 2658; W5 string route blocked; W7 digest bucket insufficient; no W8a candidate remains. |
| `y_string_unicode` | `N-direct / NO-GO` | deferred / digest | 1983 / 1029 | 4344 / 3527 | -54.3% | Floor 3950; Unicode escape/string route blocked by W5/W6 and prior proof-only limits. |

## Real-Typed-Struct Surface

Common plane: generated Track 1 typed product vs independent Track 2 structural
or serde oracle. All seven rows are guarded product-plane `A / GO`; they do not
convert the overall surface out of `N-direct / NoGo`.

| Corpus | Outcome / verdict | Strictness / output | Track 1 / Track 2 Mbps | sonic / serde Mbps | Delta vs sonic | Signal |
|---|---|---|---:|---:|---:|---|
| `twitter` | `A / GO` | deferred / typed direct | 17740 / 15912 | 15010 / 15664 | +18.2% | PASS generated typed output within sonic-rs * 1.10 ns slack; Track 2 oracle structurally different. |
| `citm_catalog` | `A / GO` | deferred / typed direct | 30539 / 17675 | 20726 / 18295 | +47.4% | PASS generated typed output within sonic-rs * 1.10 ns slack; Track 2 oracle structurally different. |
| `apache_builds` | `A / GO` | deferred / typed direct | 8478 / 6892 | 8106 / 6807 | +4.6% | PASS generated typed output within sonic-rs * 1.10 ns slack; Track 2 oracle structurally different. |
| `github_events` | `A / GO` | strict / typed direct | 11871 / 12275 | 12224 / 12249 | -2.9% | PASS W6 github_events root typed admission; Track 2 oracle 12275 Mbps. |
| `update_center` | `A / GO` | deferred / typed direct | 11851 / 10358 | 12467 / 10143 | -4.9% | PASS generated typed output within sonic-rs * 1.10 ns slack; Track 2 oracle structurally different. |
| `mesh` | `A / GO` | deferred / typed direct | 9403 / 7897 | 8923 / 7562 | +5.4% | PASS generated typed output within sonic-rs * 1.10 ns slack; Track 2 oracle structurally different. |
| `marine_ik` | `A / GO` | deferred / typed direct | 11788 / 10096 | 9010 / 10036 | +30.8% | PASS generated typed output within sonic-rs * 1.10 ns slack; Track 2 oracle structurally different. |

## SK-V11 Wave Delta Ledger

| Wave | REDRESS | Result | `RESULTS.md` delta |
|---|---:|---|---|
| W1a | 111 | Non-JSON gate/report lane admitted | None |
| W1b | 112 | Generated non-JSON baseline rejected | None |
| W2 | 113 | CSS generated intervention blocked before measurement | None |
| W3 | 114 | Numeric direct slice measured-rejected | None |
| W4 | 115 | Container-tail dispatch measured-rejected | None |
| W5 | 116 | Bounded string span blocked before source redress | None |
| W6 | 117 | Escaped segment route blocked before source redress | None |
| W7 | 118 | Output digest / host-sink route blocked | None |
| W8 | 119 | Direct residual fixpoint recorded | None; direct rows become fixpoint proofs |
| W9 | 120 | Close and Alpha feedback | None; overall remains `N-direct / NoGo` |

## Alpha-A Carry Forward

The load-bearing SK-V12 starting facts are:

- `skinny/RESULTS.md` is unchanged by W9 and remains the measurement authority.
- Overall outcome remains `N-direct / NoGo`; SK-V11 did not close as overall
  direct `GO`.
- The 13 direct residual rows should be treated as exhausted within SK-V11
  unless SK-V12 names a material differential beyond REDRESS 114-119.
- The non-JSON generated-intervention axis is blocked by REDRESS 112 and 113.
  SK-V12 should solve the generated non-JSON baseline first before spending
  another JSON-only micro-wave.
- Strict-vs-strict comparator discipline must stay intact; parse-only wins,
  absent sidecars, and permissive/lossy comparators are disclosure or flaw
  probes, not SOTA evidence.
