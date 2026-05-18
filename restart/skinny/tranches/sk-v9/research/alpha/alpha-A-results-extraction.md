# Alpha-A Results Extraction For SK-V8 -> SK-V9

Date: 2026-05-18.

Role: PASS-ALPHA alpha-A results extraction. This artifact extracts SK-V8
final measured state only. It does not dispatch, authorize, or plan any SK-V9
implementation wave.

## Source Boundary

- PASS-ALPHA alpha-A is required to extract `skinny/RESULTS.md` rows and
  produce the next-tranche results extraction artifact; the pass matrix assigns
  this role at `restart/prompts/pass-contracts/PASS-ALPHA.md:20-29`.
- The telemetry schema expected downstream includes workload, verdict,
  strictness, parse/output planes, Track 1/Track 2 Mbps, comparator Mbps,
  deltas, hot leaf, and signal fields; see
  `restart/prompts/pass-contracts/PASS-ALPHA.md:77-110`.
- SK-V8 final measured authority is the W0-rendered `skinny/RESULTS.md`, not
  later W2 source/product parity. The SK-V8 handoff states the current authority
  remains W0-rendered `skinny/RESULTS.md` and overall `N-direct / NoGo` at
  `restart/skinny/tranches/sk-v8/HANDOFF.md:37-40`.
- W6 close reconciliation repeats that `skinny/RESULTS.md` remains the W0 report
  authority and that W6 makes no source, generated-output, RESULTS, or REDRESS
  change; see
  `restart/skinny/tranches/sk-v8/research/skv8-W6-close-and-alpha-feedback.md:35-55`
  and
  `restart/skinny/tranches/sk-v8/research/wave-6-hardening/V2/HARDENING-W6-V2-CONSOLIDATED.md:44-50`.
- `skinny/REDRESS.md` is used here for the SK-V8 behavior-wave route ledger
  entries 91-93, not to override W0 measured row counts. The W6 research names
  REDRESS 91-93 as aligned at
  `restart/skinny/tranches/sk-v8/research/skv8-W6-close-reconciliation-research.md:39-55`.

## Final Row Counts

Binding measured table: `skinny/RESULTS.md:3-42`. Binding overall outcome:
`skinny/RESULTS.md:138-141`.

| Count axis | Count | Citation |
|---|---:|---|
| Main measured rows | 38 | `skinny/RESULTS.md:5-42` |
| Workload: `parse_only` | 17 | `restart/skinny/tranches/sk-v8/HANDOFF.md:42-46` |
| Workload: `direct_to_struct` | 17 | `restart/skinny/tranches/sk-v8/HANDOFF.md:42-46` |
| Workload: `real_typed_struct` | 4 | `restart/skinny/tranches/sk-v8/HANDOFF.md:42-46` |
| Outcome: `S / NO-GO` | 16 | `restart/skinny/tranches/sk-v8/HANDOFF.md:42-46` |
| Outcome: `L / NO-GO` | 1 | `restart/skinny/tranches/sk-v8/HANDOFF.md:42-46` |
| Outcome: `N-direct / NO-GO` | 14 | `restart/skinny/tranches/sk-v8/HANDOFF.md:42-46` |
| Outcome: `A / GO` | 7 | `skinny/RESULTS.md:7`, `skinny/RESULTS.md:9`, `skinny/RESULTS.md:18`, `skinny/RESULTS.md:21`, `skinny/RESULTS.md:27`, `skinny/RESULTS.md:28`, `skinny/RESULTS.md:38` |
| Verdict: `GO` | 7 | same 7 GO row citations above |
| Verdict: `NO-GO` | 31 | all non-GO rows in `skinny/RESULTS.md:5-42` |
| Strictness: `deferred` | 38 | `restart/skinny/tranches/sk-v8/HANDOFF.md:48-51` |
| `parse_utf8`: `view-boundary` | 38 | `skinny/RESULTS.md:5-42` |
| `escape_complete`: `yes` | 38 | `skinny/RESULTS.md:5-42` |
| Output plane: borrowed view over offset tape vs DOM | 17 | `skinny/RESULTS.md:5-42` |
| Output plane: digest | 17 | `skinny/RESULTS.md:5-42` |
| Output plane: typed direct | 4 | `skinny/RESULTS.md:7`, `skinny/RESULTS.md:18`, `skinny/RESULTS.md:21`, `skinny/RESULTS.md:28` |
| Overall outcome | `N-direct / NoGo` | `skinny/RESULTS.md:138` |

## Manifest And Typed Count

- Manifest count: 38 `SK-V8-open` rows. The manifest table spans
  `skinny/RESULTS.md:44-85`; W6 verified `manifest_rows=38` at
  `restart/skinny/tranches/sk-v8/research/skv8-W6-close-and-alpha-feedback.md:57-64`.
- Manifest workload split: 17 `parse_only`, 17 `direct_to_struct`, 4
  `real_typed_struct`, all under wave `SK-V8-open`, derived from
  `skinny/RESULTS.md:48-85`.
- Measured `real_typed_struct A / GO` count: 4. W6 names the measured rows as
  `twitter`, `update_center`, `mesh`, and `marine_ik` at
  `restart/skinny/tranches/sk-v8/research/skv8-W6-close-and-alpha-feedback.md:37-44`.
- W2 source/product parity added Apache/CITM source rows, but they are not
  measured SK-V8 `RESULTS.md` rows. REDRESS 91 records this at
  `skinny/REDRESS.md:2620-2659`, and the handoff repeats that W2 does not claim
  six measured `real_typed_struct A / GO` rows at
  `restart/skinny/tranches/sk-v8/HANDOFF.md:181-198`.

## Current GO Rows

| Row | Workload | Plane | Track 1 Mbps | Track 2 Mbps | sonic-rs strict Mbps | serde_json Mbps | Citation |
|---|---|---|---:|---:|---:|---:|---|
| `twitter` | `real_typed_struct` | typed direct | 15333 | 14516 | 13646 | 15046 | `skinny/RESULTS.md:7` |
| `citm_catalog` | `direct_to_struct` | digest | 21151 | 19434 | 18241 | 12992 | `skinny/RESULTS.md:9` |
| `update_center` | `real_typed_struct` | typed direct | 11958 | 10367 | 11952 | 10296 | `skinny/RESULTS.md:18` |
| `mesh` | `real_typed_struct` | typed direct | 9623 | 7674 | 9305 | 8212 | `skinny/RESULTS.md:21` |
| `marine_ik` | `direct_to_struct` | digest | 9357 | 9488 | 8559 | 7018 | `skinny/RESULTS.md:27` |
| `marine_ik` | `real_typed_struct` | typed direct | 11783 | 8321 | 6951 | 7450 | `skinny/RESULTS.md:28` |
| `unicode_basic` | `direct_to_struct` | digest | 9363 | 8420 | 8971 | 6002 | `skinny/RESULTS.md:38` |

Interpretation caveat: the 3 `direct_to_struct A / GO` rows are digest-plane
guard rows, not product-plane typed rows. The W6 research explicitly says direct
digest rows remain guard-plane rows and never become product-plane proof in W6 at
`restart/skinny/tranches/sk-v8/research/skv8-W6-close-reconciliation-research.md:22-37`.

## Current Blockers

- Parse-only blockers: all 17 `parse_only` rows are `NO-GO`: 16 are `S /
  NO-GO`, and `canada` is `L / NO-GO`. This count is summarized in
  `restart/skinny/tranches/sk-v8/HANDOFF.md:42-46`; the rows themselves are
  `skinny/RESULTS.md:5`, `skinny/RESULTS.md:8`, `skinny/RESULTS.md:10`,
  `skinny/RESULTS.md:12`, `skinny/RESULTS.md:14`, `skinny/RESULTS.md:16`,
  `skinny/RESULTS.md:19`, `skinny/RESULTS.md:22`, `skinny/RESULTS.md:24`,
  `skinny/RESULTS.md:26`, `skinny/RESULTS.md:29`, `skinny/RESULTS.md:31`,
  `skinny/RESULTS.md:33`, `skinny/RESULTS.md:35`, `skinny/RESULTS.md:37`,
  `skinny/RESULTS.md:39`, and `skinny/RESULTS.md:41`.
- Direct blockers: 14 `direct_to_struct` rows remain `N-direct / NO-GO`.
  The representative notes define the direct gate as Track 1 and Track 2 being
  within `1.10x` sonic-rs time, for example `twitter` at
  `skinny/RESULTS.md:89`, `canada` at `skinny/RESULTS.md:94`, and
  `y_string_unicode` at `skinny/RESULTS.md:135`.
- Typed product-plane blockers are not measured row-table blockers in SK-V8:
  Apache/CITM are source/product parity only, and `canada/real_typed_struct` was
  rejected during W2. See `skinny/REDRESS.md:2622-2640`.
- Overall blocker classification remains `N-direct / NoGo`, not a parse-only
  or typed-only close. The report states this at `skinny/RESULTS.md:138`, and
  the W6 close artifact repeats it at
  `restart/skinny/tranches/sk-v8/research/skv8-W6-close-and-alpha-feedback.md:37-44`.

## Missing Telemetry Caveats

- Every current main row has `Strictness=deferred`; do not count any row as a
  fully strict main-row close until a later accepted report changes this. Handoff
  caveat: `restart/skinny/tranches/sk-v8/HANDOFF.md:48-51`.
- `Delta vs SK-V6` is non-derivable in the current report. The handoff says this
  directly at `restart/skinny/tranches/sk-v8/HANDOFF.md:48-51`, and each row
  carries `n/a (no machine-readable SK-V6 baseline in W0b)` in
  `skinny/RESULTS.md:5-42`.
- Current rows cover only `parse_only`, `direct_to_struct`, and
  `real_typed_struct`. PASS-ALPHA's schema names additional workloads
  `parse_full_traversal`, `path_lookup`, `unicode_string_float`, `memory`, and
  `cycles_per_byte`; those have 0 SK-V8 measured main rows. Schema reference:
  `restart/prompts/pass-contracts/PASS-ALPHA.md:83-85`.
- The manifest contains `Sample cost` as `ns_per_byte`, but no separate
  `cycles_per_byte` workload row or c/B row table is present in
  `skinny/RESULTS.md:44-85`.
- W0 emits criterion slope profile artifacts and non-placeholder hot-leaf
  bindings, but the visible hot leaf is a criterion profile binding string, not
  a symbol plus self-time percentage. Handoff caveat:
  `restart/skinny/tranches/sk-v8/HANDOFF.md:48-56`; row examples:
  `skinny/RESULTS.md:5-7`.
- Native Rust comparators are same-run, but C++ comparator values are historical
  sidecar planning signals or explicitly absent. W0 admits no structured sidecar
  same-run manifest and rejects sidecar-same-run claims until a later wave adds a
  parser and gate. Citations:
  `skinny/RESULTS.md:141` and
  `restart/skinny/tranches/sk-v8/HANDOFF.md:52-56`.
- W1 binds strict comparator ids and rejects lossy, sidecar, and unknown
  comparator ids as strict anchors, but W1 left `skinny/RESULTS.md` unchanged and
  its benchmark refresh attempt was rejected by W0 run-id validation after local
  Criterion metadata drift. Citation:
  `restart/skinny/tranches/sk-v8/HANDOFF.md:170-179`.

## SK-V8 Wave Evidence That Changes Interpretation

| Wave | Evidence | Interpretation impact |
|---|---|---|
| W0 | W0 created `SK-V8-open` telemetry on all 38 rows; closure authority is listed at `restart/skinny/tranches/sk-v8/HANDOFF.md:166-169`. | W0 remains the measured row-table authority for SK-V8 final state. |
| W1 | CostFacts and strict comparator id binding landed; generated/parser/product surfaces and `skinny/RESULTS.md` were unchanged (`restart/skinny/tranches/sk-v8/HANDOFF.md:170-179`). | Comparator strictness policy changed, but measured Mbps rows did not. |
| W2 | Apache/CITM typed source/product parity admitted; benchmark row-table admission rejected; current W0 measured manifest remains four real typed rows (`skinny/REDRESS.md:2620-2659`; `restart/skinny/tranches/sk-v8/HANDOFF.md:181-198`). | Do not report Apache/CITM as measured `real_typed_struct A / GO` rows; do not report six measured typed GO rows. |
| W3 | Tier A tape plus structural-projection implementation rejected/routed because scanner structural index and retained tape event stream are not isomorphic (`skinny/REDRESS.md:2661-2690`; `restart/skinny/tranches/sk-v8/HANDOFF.md:203-216`). | Do not treat structural-projection union as admitted source or row-table evidence. |
| W4 | Scalar-parent fold candidate rejected/routed after selected-row falsification; Apache cleared, but random still missed and numbers regressed (`skinny/REDRESS.md:2692-2729`; `restart/skinny/tranches/sk-v8/HANDOFF.md:218-233`). | Direct digest misses remain; W4 adds no source admission, Lock 14 allowance, or RESULTS change. |
| W5 | Named Lock 14 provider-boundary cleanup admitted with no generated output, row-table, performance claim, or `skinny/RESULTS.md` change (`restart/skinny/tranches/sk-v8/HANDOFF.md:235-248`). | W5 is cleanup evidence only, not performance movement. |
| W6 | W6 V1+V2 accepted close convergence and admits no source, generated-output, benchmark-row, RESULTS, or REDRESS change (`restart/skinny/tranches/sk-v8/research/wave-6-hardening/V2/HARDENING-W6-V2-CONSOLIDATED.md:44-50`). | SK-V8 is closed, but SK-V9 implementation remains behind a new Pass Alpha/G-Alpha boundary. |

Carry-in honesty note: SK-V7 W10 did not admit the PMULL prefix-XOR or CSSC
CTZ/bulk body-fill targets; only B6 stack-canary Stage 1 was admitted with zero
production and `RESULTS.md` diff. SK-V8 handoff records this at
`restart/skinny/tranches/sk-v8/HANDOFF.md:58-68`, so bitmap body fills should not
be read as SK-V8 accepted evidence.

## Measured Row Inventory

| Source | Corpus | Workload | Outcome | Verdict | Plane | Track 1 | Track 2 | sonic strict | simdjson DOM | yyjson | serde_json | Delta sonic | Delta simdjson | Delta yyjson |
|---|---|---|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| `skinny/RESULTS.md:5` | `twitter` | `parse_only` | S | NO-GO | borrowed view over offset tape vs DOM | 9581 | 9741 | 18176 | 24522 | 30931 | 3829 | -47.3% | -60.9% | -69.0% |
| `skinny/RESULTS.md:6` | `twitter` | `direct_to_struct` | N-direct | NO-GO | digest | 11859 | 9881 | 12890 | n/a | n/a | 6673 | -8.0% | n/a | n/a |
| `skinny/RESULTS.md:7` | `twitter` | `real_typed_struct` | A | GO | typed direct | 15333 | 14516 | 13646 | n/a | n/a | 15046 | +12.4% | n/a | n/a |
| `skinny/RESULTS.md:8` | `citm_catalog` | `parse_only` | S | NO-GO | borrowed view over offset tape vs DOM | 28644 | 19214 | 21717 | 35822 | 20956 | 7401 | +31.9% | -20.0% | +36.7% |
| `skinny/RESULTS.md:9` | `citm_catalog` | `direct_to_struct` | A | GO | digest | 21151 | 19434 | 18241 | n/a | n/a | 12992 | +16.0% | n/a | n/a |
| `skinny/RESULTS.md:10` | `canada` | `parse_only` | L | NO-GO | borrowed view over offset tape vs DOM | 15497 | 12171 | 8729 | 11493 | 13003 | 4050 | +77.5% | +34.8% | +19.2% |
| `skinny/RESULTS.md:11` | `canada` | `direct_to_struct` | N-direct | NO-GO | digest | 6586 | 9769 | 12430 | n/a | n/a | 7080 | -47.0% | n/a | n/a |
| `skinny/RESULTS.md:12` | `apache_builds` | `parse_only` | S | NO-GO | borrowed view over offset tape vs DOM | 12694 | 11715 | 16904 | 36014 | 16275 | 4278 | -24.9% | -64.8% | -22.0% |
| `skinny/RESULTS.md:13` | `apache_builds` | `direct_to_struct` | N-direct | NO-GO | digest | 8306 | 7796 | 8852 | n/a | n/a | 6750 | -6.2% | n/a | n/a |
| `skinny/RESULTS.md:14` | `github_events` | `parse_only` | S | NO-GO | borrowed view over offset tape vs DOM | 10689 | 10073 | 16408 | 39642 | 21426 | 4675 | -34.9% | -73.0% | -50.1% |
| `skinny/RESULTS.md:15` | `github_events` | `direct_to_struct` | N-direct | NO-GO | digest | 9088 | 7337 | 9818 | n/a | n/a | 8152 | -7.4% | n/a | n/a |
| `skinny/RESULTS.md:16` | `update_center` | `parse_only` | S | NO-GO | borrowed view over offset tape vs DOM | 11926 | 9312 | 18769 | 30593 | 18540 | 4131 | -36.5% | -61.0% | -35.7% |
| `skinny/RESULTS.md:17` | `update_center` | `direct_to_struct` | N-direct | NO-GO | digest | 7863 | 7514 | 10525 | n/a | n/a | 8218 | -25.3% | n/a | n/a |
| `skinny/RESULTS.md:18` | `update_center` | `real_typed_struct` | A | GO | typed direct | 11958 | 10367 | 11952 | n/a | n/a | 10296 | +0.0% | n/a | n/a |
| `skinny/RESULTS.md:19` | `mesh` | `parse_only` | S | NO-GO | borrowed view over offset tape vs DOM | 9367 | 10000 | 8143 | 9414 | n/a | 4123 | +15.0% | -0.5% | n/a |
| `skinny/RESULTS.md:20` | `mesh` | `direct_to_struct` | N-direct | NO-GO | digest | 8640 | 9049 | 9967 | n/a | n/a | 7176 | -13.3% | n/a | n/a |
| `skinny/RESULTS.md:21` | `mesh` | `real_typed_struct` | A | GO | typed direct | 9623 | 7674 | 9305 | n/a | n/a | 8212 | +3.4% | n/a | n/a |
| `skinny/RESULTS.md:22` | `random` | `parse_only` | S | NO-GO | borrowed view over offset tape vs DOM | 10011 | 8018 | 15639 | 20638 | n/a | 3486 | -36.0% | -51.5% | n/a |
| `skinny/RESULTS.md:23` | `random` | `direct_to_struct` | N-direct | NO-GO | digest | 7751 | 6952 | 8141 | n/a | n/a | 5922 | -4.8% | n/a | n/a |
| `skinny/RESULTS.md:24` | `gsoc-2018` | `parse_only` | S | NO-GO | borrowed view over offset tape vs DOM | 23209 | 21857 | 49101 | n/a | n/a | 10741 | -52.7% | n/a | n/a |
| `skinny/RESULTS.md:25` | `gsoc-2018` | `direct_to_struct` | N-direct | NO-GO | digest | 15042 | 14380 | 23356 | n/a | n/a | 19398 | -35.6% | n/a | n/a |
| `skinny/RESULTS.md:26` | `marine_ik` | `parse_only` | S | NO-GO | borrowed view over offset tape vs DOM | 13100 | 12164 | 9921 | n/a | n/a | 4091 | +32.1% | n/a | n/a |
| `skinny/RESULTS.md:27` | `marine_ik` | `direct_to_struct` | A | GO | digest | 9357 | 9488 | 8559 | n/a | n/a | 7018 | +9.3% | n/a | n/a |
| `skinny/RESULTS.md:28` | `marine_ik` | `real_typed_struct` | A | GO | typed direct | 11783 | 8321 | 6951 | n/a | n/a | 7450 | +69.5% | n/a | n/a |
| `skinny/RESULTS.md:29` | `instruments` | `parse_only` | S | NO-GO | borrowed view over offset tape vs DOM | 13320 | 11351 | 17976 | n/a | n/a | 3028 | -25.9% | n/a | n/a |
| `skinny/RESULTS.md:30` | `instruments` | `direct_to_struct` | N-direct | NO-GO | digest | 8494 | 8766 | 9872 | n/a | n/a | 7576 | -14.0% | n/a | n/a |
| `skinny/RESULTS.md:31` | `numbers` | `parse_only` | S | NO-GO | borrowed view over offset tape vs DOM | 12818 | 13537 | 9854 | n/a | n/a | 4422 | +30.1% | n/a | n/a |
| `skinny/RESULTS.md:32` | `numbers` | `direct_to_struct` | N-direct | NO-GO | digest | 9773 | 6966 | 7953 | n/a | n/a | 5753 | +22.9% | n/a | n/a |
| `skinny/RESULTS.md:33` | `unicode_mixed` | `parse_only` | S | NO-GO | borrowed view over offset tape vs DOM | 6390 | 4970 | 9943 | 13150 | n/a | 2654 | -35.7% | -51.4% | n/a |
| `skinny/RESULTS.md:34` | `unicode_mixed` | `direct_to_struct` | N-direct | NO-GO | digest | 3596 | 3694 | 10077 | n/a | n/a | 4911 | -64.3% | n/a | n/a |
| `skinny/RESULTS.md:35` | `unicode_escapes` | `parse_only` | S | NO-GO | borrowed view over offset tape vs DOM | 12731 | 8521 | 13851 | 5637 | n/a | 4040 | -8.1% | +125.9% | n/a |
| `skinny/RESULTS.md:36` | `unicode_escapes` | `direct_to_struct` | N-direct | NO-GO | digest | 4020 | 4016 | 13999 | n/a | n/a | 3720 | -71.3% | n/a | n/a |
| `skinny/RESULTS.md:37` | `unicode_basic` | `parse_only` | S | NO-GO | borrowed view over offset tape vs DOM | 11189 | 10040 | 15797 | 16276 | n/a | 3611 | -29.2% | -31.3% | n/a |
| `skinny/RESULTS.md:38` | `unicode_basic` | `direct_to_struct` | A | GO | digest | 9363 | 8420 | 8971 | n/a | n/a | 6002 | +4.4% | n/a | n/a |
| `skinny/RESULTS.md:39` | `distinct_values` | `parse_only` | S | NO-GO | borrowed view over offset tape vs DOM | 10279 | 6457 | 18282 | 22825 | n/a | 3158 | -43.8% | -55.0% | n/a |
| `skinny/RESULTS.md:40` | `distinct_values` | `direct_to_struct` | N-direct | NO-GO | digest | 4438 | 4151 | 8950 | n/a | n/a | 5598 | -50.4% | n/a | n/a |
| `skinny/RESULTS.md:41` | `y_string_unicode` | `parse_only` | S | NO-GO | borrowed view over offset tape vs DOM | 5577 | 5480 | 12009 | 13627 | n/a | 5657 | -53.6% | -59.1% | n/a |
| `skinny/RESULTS.md:42` | `y_string_unicode` | `direct_to_struct` | N-direct | NO-GO | digest | 4828 | 3563 | 9065 | n/a | n/a | 7599 | -46.7% | n/a | n/a |

## Dispatch Boundary

SK-V8 close does not authorize SK-V9 implementation. The SK-V8 handoff states
that SK-V9 may only be planned through Pass Alpha and the skinny pass substrate,
then presented for a new G-Alpha decision before implementation dispatch at
`restart/skinny/tranches/sk-v8/HANDOFF.md:347-360`.
