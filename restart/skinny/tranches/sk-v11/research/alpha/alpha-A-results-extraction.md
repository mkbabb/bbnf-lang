# Alpha-A Results Extraction For SK-V10 -> SK-V11

Date: 2026-05-19.

Role: PASS-ALPHA alpha-A results extraction. This artifact extracts the
measured SK-V10 close state for the SK-V11 contract. It does not dispatch
SK-V11 implementation waves.

## Source Boundary

- Alpha-A owns row extraction from `skinny/RESULTS.md`: per-corpus workload
  Mbps, strictness plane, output plane, hot-leaf attribution, and prior-cycle
  deltas where present (`restart/prompts/pass-contracts/PASS-ALPHA.md:18-29`).
- Pass Alpha is six-agent, challenge-reviewed synthesis, not implementation
  (`restart/prompts/pass-contracts/PASS-ALPHA.md:3-5`,
  `restart/prompts/pass-contracts/PASS-ALPHA.md:33-49`).
- CHALLENGE convergence and cross-pass discipline bind this extraction
  (`restart/prompts/ORCHESTRATOR.md:104-128`,
  `restart/prompts/ORCHESTRATOR.md:229-245`).
- SK-V10 close authority is the W10 full native Criterion render under
  `/tmp/skv10-w10-full-criterion`, run id
  `sk-v9-open:criterion-fnv64-6f007527061ee26d`
  (`restart/skinny/tranches/sk-v10/research/close/close-redress.md:33-47`,
  `skinny/REDRESS.md:3268-3274`).
- Main measured table rows are `skinny/RESULTS.md:5-45`; the telemetry
  manifest uses the same run id on the same row family
  (`skinny/RESULTS.md:51-91`).
- Overall close state remains `N-direct / NoGo`
  (`skinny/RESULTS.md:141`,
  `restart/skinny/tranches/sk-v10/research/close/close-redress.md:45-47`).

## Final Row Counts

| Count axis | Final count | Citation |
|---|---:|---|
| Main measured rows | 41 | `skinny/RESULTS.md:5-45` |
| `parse_only` | 17 `S / NO-GO` | `skinny/RESULTS.md:5`, `:8`, `:11`, `:13`, `:16`, `:19`, `:22`, `:25`, `:27`, `:29`, `:32`, `:34`, `:36`, `:38`, `:40`, `:42`, `:44` |
| `direct_to_struct` | 6 `A / GO`, 11 `N-direct / NO-GO` | `skinny/RESULTS.md:6`, `:9`, `:12`, `:14`, `:17`, `:20`, `:23`, `:26`, `:28`, `:30`, `:33`, `:35`, `:37`, `:39`, `:41`, `:43`, `:45` |
| `real_typed_struct` | 7 `A / GO` | `skinny/RESULTS.md:7`, `:10`, `:15`, `:18`, `:21`, `:24`, `:31` |
| Overall | `N-direct / NoGo` | `skinny/RESULTS.md:141` |

SK-V10 opened at 17 parse `S / NO-GO`, 3 direct `A / GO`, 14 direct
`N-direct / NO-GO`, and 6 typed `A / GO` (`skinny/REDRESS.md:2965-2967`).
It closed at 17 parse `S / NO-GO`, 6 direct `A / GO`, 11 direct
`N-direct / NO-GO`, and 7 typed `A / GO` (`skinny/REDRESS.md:3268-3272`).
The derivable SK-V10 count delta is therefore: parse unchanged, direct GO +3,
direct residual NO-GO -3, typed GO +1.

## Direct Residual Table

These 11 rows are the remaining SK-V11 direct frontier. All are
`N-direct / NO-GO`, `Output plane=digest`, and correctness-green by signal,
but each misses the current direct digest time-slack gate.

| Corpus | Track 1 Mbps | Track 2 Mbps | sonic-rs direct Mbps | serde_json direct Mbps | Delta vs sonic | Result citation |
|---|---:|---:|---:|---:|---:|---|
| `twitter` | 11905 | 10968 | 15244 | 10562 | -21.9% | `skinny/RESULTS.md:6` |
| `canada` | 10590 | 10286 | 12157 | 7425 | -12.9% | `skinny/RESULTS.md:12` |
| `github_events` | 12439 | 11430 | 16206 | 12977 | -23.2% | `skinny/RESULTS.md:17` |
| `update_center` | 8425 | 7620 | 11186 | 8218 | -24.7% | `skinny/RESULTS.md:20` |
| `mesh` | 8562 | 8596 | 9422 | 6932 | -9.1% | `skinny/RESULTS.md:23` |
| `random` | 7887 | 7132 | 8948 | 6551 | -11.9% | `skinny/RESULTS.md:26` |
| `gsoc-2018` | 15056 | 14534 | 23437 | 19440 | -35.8% | `skinny/RESULTS.md:28` |
| `unicode_mixed` | 4700 | 4556 | 10480 | 5233 | -55.1% | `skinny/RESULTS.md:37` |
| `unicode_escapes` | 5069 | 5222 | 14147 | 5193 | -64.2% | `skinny/RESULTS.md:39` |
| `distinct_values` | 6303 | 5654 | 11978 | 8195 | -47.4% | `skinny/RESULTS.md:43` |
| `y_string_unicode` | 5067 | 3746 | 9211 | 7532 | -45.0% | `skinny/RESULTS.md:45` |

The closest residual by rendered sonic delta is `mesh` at -9.1%; the worst
residuals are the unicode escape rows (`unicode_escapes` -64.2%,
`unicode_mixed` -55.1%, `y_string_unicode` -45.0%) and `distinct_values`
(-47.4%). These are SK-V11 direct-plane candidates only if a fresh profile and
same-host micro-proof identify a row-local movement path.

## Direct GO Rows

Six direct digest rows are `A / GO`. Three are inherited guard rows, two moved
in W2, and one moved in W10.

| Corpus | Track 1 Mbps | Track 2 Mbps | sonic-rs direct Mbps | serde_json direct Mbps | Delta vs sonic | Admission class | Result citation |
|---|---:|---:|---:|---:|---:|---|---|
| `citm_catalog` | 21595 | 20592 | 20036 | 13363 | +7.8% | inherited guard | `skinny/RESULTS.md:9` |
| `apache_builds` | 11469 | 10368 | 11190 | 9921 | +2.5% | W2 strict measured-row | `skinny/RESULTS.md:14` |
| `marine_ik` | 9066 | 9025 | 8235 | 6800 | +10.1% | inherited guard | `skinny/RESULTS.md:30` |
| `instruments` | 12040 | 11166 | 12674 | 9497 | -5.0% | W10 strict measured-row | `skinny/RESULTS.md:33` |
| `numbers` | 12619 | 12296 | 13038 | 8117 | -3.2% | W2 strict measured-row | `skinny/RESULTS.md:35` |
| `unicode_basic` | 9030 | 8360 | 8940 | 5918 | +1.0% | inherited guard | `skinny/RESULTS.md:41` |

W2 admitted exactly `apache_builds/direct_to_struct` and
`numbers/direct_to_struct` (`skinny/REDRESS.md:3003-3019`). W10 admitted
exactly `instruments/direct_to_struct` and changed no parser runtime,
generated direct caller, SIMD primitive, generic crate, typed product row, or
W3-adjacent substrate path (`skinny/REDRESS.md:3224-3239`). The W10 moved row
is consumed as strict measured-row evidence with `wave_id=SK-V10-W10` and
`redress_entry=REDRESS-109` (`skinny/REDRESS.md:3250-3255`).

## Typed Product Table

All seven typed rows are `A / GO`, `Output plane=typed direct`. Six are
deferred/view-boundary inherited typed rows; `github_events` is the SK-V10 W6
strict measured-row root-typed admission.

| Corpus | Track 1 Mbps | Track 2/oracle Mbps | sonic-rs typed Mbps | serde_json typed Mbps | Delta vs sonic | Result citation |
|---|---:|---:|---:|---:|---:|---|
| `twitter` | 18241 | 16492 | 15636 | 16513 | +16.7% | `skinny/RESULTS.md:7` |
| `citm_catalog` | 36135 | 19245 | 22066 | 19114 | +63.8% | `skinny/RESULTS.md:10` |
| `apache_builds` | 8534 | 7079 | 8321 | 7073 | +2.5% | `skinny/RESULTS.md:15` |
| `github_events` | 13137 | 12855 | 12926 | 12848 | +1.6% | `skinny/RESULTS.md:18` |
| `update_center` | 12069 | 10603 | 12727 | 10458 | -5.2% | `skinny/RESULTS.md:21` |
| `mesh` | 9690 | 8072 | 9253 | 7499 | +4.7% | `skinny/RESULTS.md:24` |
| `marine_ik` | 12186 | 9985 | 9322 | 10012 | +30.7% | `skinny/RESULTS.md:31` |

W6 moved exactly `github_events/real_typed_struct`, proved the typed root as
`Vec<crate::real_typed_struct::GithubEvent<'i>>`, and cleared the W6 floor with
generated Track 1, independent Track 2/oracle, sonic-rs typed, and serde_json
typed evidence (`skinny/REDRESS.md:3106-3122`). `update_center` remains
`A / GO` despite a -5.2% Mbps delta because the gate is time-slack based and
the signal is still PASS (`skinny/RESULTS.md:21`).

## Parse Concession

All 17 `parse_only` rows remain `S / NO-GO`, `Strictness=deferred`,
`parse_utf8=view-boundary`, and `Output plane=borrowed view over offset tape vs
DOM`. They are not SOTA admissions, even when raw Track 1 Mbps exceeds a native
strict comparator.

| Corpus | Track 1 Mbps | Track 2 Mbps | sonic-rs strict Mbps | simdjson DOM Mbps | yyjson default Mbps | Delta vs sonic | Citation |
|---|---:|---:|---:|---:|---:|---:|---|
| `twitter` | 15738 | 12245 | 21213 | 24522 | 30931 | -25.8% | `skinny/RESULTS.md:5` |
| `citm_catalog` | 30045 | 21035 | 25360 | 35822 | 20956 | +18.5% | `skinny/RESULTS.md:8` |
| `canada` | 17285 | 16797 | 14121 | 11493 | 13003 | +22.4% | `skinny/RESULTS.md:11` |
| `apache_builds` | 12705 | 12333 | 17492 | 36014 | 16275 | -27.4% | `skinny/RESULTS.md:13` |
| `github_events` | 15549 | 13304 | 23286 | 39642 | 21426 | -33.2% | `skinny/RESULTS.md:16` |
| `update_center` | 11509 | 9367 | 19802 | 30593 | 18540 | -41.9% | `skinny/RESULTS.md:19` |
| `mesh` | 13552 | 12309 | 11927 | 9414 | n/a | +13.6% | `skinny/RESULTS.md:22` |
| `random` | 9897 | 7787 | 15521 | 20638 | n/a | -36.2% | `skinny/RESULTS.md:25` |
| `gsoc-2018` | 23191 | 21928 | 49490 | n/a | n/a | -53.1% | `skinny/RESULTS.md:27` |
| `marine_ik` | 13055 | 12266 | 10009 | n/a | n/a | +30.4% | `skinny/RESULTS.md:29` |
| `instruments` | 16895 | 11836 | 19644 | n/a | n/a | -14.0% | `skinny/RESULTS.md:32` |
| `numbers` | 19053 | 18477 | 13397 | n/a | n/a | +42.2% | `skinny/RESULTS.md:34` |
| `unicode_mixed` | 7881 | 8089 | 18105 | 13150 | n/a | -56.5% | `skinny/RESULTS.md:36` |
| `unicode_escapes` | 11550 | 11893 | 18819 | 5637 | n/a | -38.6% | `skinny/RESULTS.md:38` |
| `unicode_basic` | 11889 | 10739 | 15906 | 16276 | n/a | -25.3% | `skinny/RESULTS.md:40` |
| `distinct_values` | 9799 | 6213 | 17904 | 22825 | n/a | -45.3% | `skinny/RESULTS.md:42` |
| `y_string_unicode` | 6389 | 6066 | 13794 | 13627 | n/a | -53.7% | `skinny/RESULTS.md:44` |

The parse-only concession is reinforced by SK-V10 W3: the packet audit found
no live dispatch route through W3 union/event substrate, retained class column,
`UnionTape`, structural cursor, streaming cursor, class-lane-only route, or
W4-through-W3 cascade lock (`skinny/REDRESS.md:3040-3058`). The SK-V9
union-substrate thesis is retired rather than merely blocked: REDRESS 96 and
97 falsified the route, and REDRESS 98 bars any SK-V9 wave from forcing,
amending, or splitting W3 to preserve the same substrate thesis
(`skinny/REDRESS.md:2910-2937`). SK-V10 handoff carries that refusal forward
(`restart/skinny/tranches/sk-v10/HANDOFF.md:145-153`).

## SK-V10 Deltas Derivable Here

| Delta | Value | Evidence |
|---|---:|---|
| Direct `A / GO` count | +3 | W0 opening 3 direct GO and 14 residuals (`skinny/REDRESS.md:2965-2967`); close 6 direct GO and 11 residuals (`skinny/REDRESS.md:3268-3272`) |
| Typed `A / GO` count | +1 | W0 opening 6 typed GO (`skinny/REDRESS.md:2965-2967`); close 7 typed GO (`skinny/REDRESS.md:3268-3272`) |
| Parse SOTA count | 0 movement | W0 and close both 17 `S / NO-GO` parse rows (`skinny/REDRESS.md:2965-2967`, `skinny/REDRESS.md:3268-3272`) |
| Row movements | W2 direct +2; W6 typed +1; W10 direct +1 | `skinny/REDRESS.md:3003-3019`, `skinny/REDRESS.md:3106-3122`, `skinny/REDRESS.md:3224-3255` |

`skinny/RESULTS.md` does not contain a machine-readable "delta vs SK-V10
opening" column. Its rendered delta columns are `Delta vs SK-V6`,
`Delta vs sonic-strict`, `Delta vs simdjson DOM`, and `Delta vs yyjson`
(`skinny/RESULTS.md:3`). The row-level `Delta vs SK-V6` cells remain
non-machine-readable or `n/a` for this purpose, so per-row Mbps deltas versus
SK-V10 opening are not derivable from `RESULTS.md` without external baseline
tables. This extraction therefore records only count movement and current
strict-comparator deltas.

## Cost And Hot-Leaf Availability

The main table records each hot leaf as a Criterion slope profile artifact,
for example `criterion-slope-profile:json_twitter/track1_generated/...` on the
first parse row (`skinny/RESULTS.md:5`) and direct/typed analogues on the row
tables above. The telemetry manifest supplies `ns_per_byte`, `track1_ns`, and
`bytes` sample cost per row under the same run id, starting at
`skinny/RESULTS.md:51`. It does not render cycles-per-byte as a main-table
numeric field; SK-V11 should not invent c/B from this artifact without a fresh
PMU/cycle capture.

## Current GO / NO-GO State For SK-V11

- The typed product plane is the strongest SOTA-bearing surface: 7/7 typed rows
  are `A / GO`, with five positive rendered sonic deltas, one small positive
  W6 row, and `update_center` still passing the time-slack gate despite a
  negative Mbps delta (`skinny/RESULTS.md:7`, `:10`, `:15`, `:18`, `:21`,
  `:24`, `:31`).
- The direct digest plane is the active SK-V11 frontier: 6/17 rows are
  `A / GO`; 11/17 remain `N-direct / NO-GO` and are listed above
  (`skinny/RESULTS.md:6-45`, `restart/skinny/tranches/sk-v10/HANDOFF.md:112-118`).
- The parse-only plane is a closed concession: 17/17 rows remain `S / NO-GO`,
  and SK-V10 handoff names direct as the primary JSON frontier while retiring
  parse-only SOTA from the close target (`restart/skinny/tranches/sk-v10/HANDOFF.md:8-21`,
  `restart/skinny/tranches/sk-v10/HANDOFF.md:112-118`).
- Overall SK-V10 close state is `N-direct / NoGo`; SK-V11 must either close the
  11 direct residual rows or prove each uncloseable under fresh measurement.
