# Alpha-A Results Extraction For SK-V9 -> SK-V10

Date: 2026-05-19.

Role: PASS-ALPHA alpha-A results extraction. This artifact extracts the current
SK-V9 measured state after W1/W2 admission and W3 retirement. It does not
dispatch SK-V10 implementation waves.

## Source Boundary

- Binding measured table: `skinny/RESULTS.md:3-44`.
- Binding manifest table: `skinny/RESULTS.md:46-86`.
- W3 falsification authority: `skinny/REDRESS.md` entries 96, 97, and 98.
- Current contract posture: `restart/skinny/tranches/sk-v9/HANDOFF.md` and
  `restart/skinny/tranches/sk-v9/research/alpha/alpha-G-dispatch-sk-v10.md`.

## Final Row Counts

| Count axis | Count | Citation |
|---|---:|---|
| Main measured rows | 40 | `skinny/RESULTS.md:5-44` |
| Manifest rows | 40 | `skinny/RESULTS.md:50-89` |
| Workload: `parse_only` | 17 | `skinny/RESULTS.md:5`, `:8`, `:11`, `:13`, `:16`, `:18`, `:21`, `:24`, `:26`, `:28`, `:31`, `:33`, `:35`, `:37`, `:39`, `:41`, `:43` |
| Workload: `direct_to_struct` | 17 | `skinny/RESULTS.md:6`, `:9`, `:12`, `:14`, `:17`, `:19`, `:22`, `:25`, `:27`, `:29`, `:32`, `:34`, `:36`, `:38`, `:40`, `:42`, `:44` |
| Workload: `real_typed_struct` | 6 | `skinny/RESULTS.md:7`, `:10`, `:15`, `:20`, `:23`, `:30` |
| Outcome: `S / NO-GO` | 17 | all `parse_only` rows |
| Outcome: `N-direct / NO-GO` | 14 | all direct rows except `citm_catalog`, `marine_ik`, `unicode_basic` |
| Outcome: `A / GO` | 9 | six typed rows plus three direct digest guard rows |
| Strictness | 40 `deferred` | `skinny/RESULTS.md:5-44` |
| `parse_utf8` | 40 `view-boundary` | `skinny/RESULTS.md:5-44` |
| `escape_complete` | 40 `yes` | `skinny/RESULTS.md:5-44` |
| Overall | `N-direct / NoGo` | `skinny/RESULTS.md:142` |

## Typed Product Rows

These are the only current SK-V9 SOTA-bearing rows. They remain
`Strictness=deferred`, so the SK-V10 contract must preserve the measured
validation path rather than overstating strictness.

| Corpus | Track 1 Mbps | Track 2 Mbps | sonic-rs typed Mbps | serde_json Mbps | Delta vs sonic | Citation |
|---|---:|---:|---:|---:|---:|---|
| `twitter` | 18302 | 16512 | 15866 | 16449 | +15.3% | `skinny/RESULTS.md:7` |
| `citm_catalog` | 35102 | 19143 | 22058 | 19322 | +59.1% | `skinny/RESULTS.md:10` |
| `apache_builds` | 8174 | 6728 | 8110 | 6719 | +0.8% | `skinny/RESULTS.md:15` |
| `update_center` | 11847 | 10297 | 12501 | 10405 | -5.2% | `skinny/RESULTS.md:20` |
| `mesh` | 10032 | 7854 | 9270 | 7263 | +8.2% | `skinny/RESULTS.md:23` |
| `marine_ik` | 10728 | 8454 | 8105 | 9359 | +32.4% | `skinny/RESULTS.md:30` |

`update_center` is `A / GO` because the gate is time-slack based: generated
Track 1 remains within sonic-rs * 1.10 ns, even though the rendered Mbps delta
is -5.2%.

## Direct Guard Rows

Three direct digest rows are `A / GO`: `citm_catalog`, `marine_ik`, and
`unicode_basic` (`skinny/RESULTS.md:9`, `:29`, `:40`). They are guard-plane
evidence only. They do not become typed product proof and do not authorize
Canada or other direct rows.

## Parse-Only Rows

All 17 `parse_only` rows are `S / NO-GO`. Some parse rows beat a native strict
comparator on raw Mbps (`citm_catalog`, `canada`, `mesh`, `marine_ik`,
`numbers`), but the output plane is still borrowed view over offset tape vs DOM
and strictness is deferred. They must not be scored as SOTA admissions in
SK-V10.

## Candidate Rows With No Typed Product Row Yet

| Corpus | Current rows | Current evidence | Alpha disposition |
|---|---|---|---|
| `github_events` | parse_only 14905 Mbps; direct 11983 Mbps | both `NO-GO`; no real typed row | typed-plane candidate only after schema + parity + same-run typed comparator |
| `gsoc-2018` | parse_only 22446 Mbps; direct 14676 Mbps | both `NO-GO`; unicode/string heavy | typed-plane or existing-substrate candidate, but no inherited admission |
| `instruments` | parse_only 16880 Mbps; direct 11708 Mbps | both `NO-GO`; no real typed row | typed-plane candidate only after schema + parity + same-run typed comparator |

## Carry-Forward

SK-V10 Alpha should carry exactly this measured state: six typed `A / GO`
product rows, three direct digest `A / GO` guard rows, 17 parse-only
`S / NO-GO` rows, and 14 direct `N-direct / NO-GO` rows. W3 adds no admitted
source or row-table movement.
