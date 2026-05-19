# Alpha-B Competitor Deltas For SK-V9 -> SK-V10

Date: 2026-05-19.

Role: PASS-ALPHA alpha-B competitor delta extraction. The only current
SOTA-bearing comparison plane is `real_typed_struct` typed direct under the
current deferred/view-boundary validation path. Parse-only and direct digest
rows remain guard or planning evidence unless a later SK-V10 gate changes their
validation/output plane.

## Typed Comparator Deltas Under The Deferred Gate

The sonic-rs anchor is typed strict. The bbnf rows remain
`Strictness=deferred` and `parse_utf8=view-boundary`; this table is not a
strict-admission claim until `gate-json` consumes a measured-row strictness and
validation-path change.

| Corpus | bbnf Track 1 | sonic-rs strict | serde_json | Delta vs sonic | Delta vs serde | Status |
|---|---:|---:|---:|---:|---:|---|
| `twitter` | 18302 | 15866 | 16449 | +15.3% | +11.3% | typed product GO |
| `citm_catalog` | 35102 | 22058 | 19322 | +59.1% | +81.7% | typed product GO |
| `apache_builds` | 8174 | 8110 | 6719 | +0.8% | +21.7% | typed product GO |
| `update_center` | 11847 | 12501 | 10405 | -5.2% | +13.9% | typed product GO under 1.10 ns slack |
| `mesh` | 10032 | 9270 | 7263 | +8.2% | +38.1% | typed product GO |
| `marine_ik` | 10728 | 8105 | 9359 | +32.4% | +14.6% | typed product GO |

Citation: `skinny/RESULTS.md:7`, `:10`, `:15`, `:20`, `:23`, `:30`.

## Candidate Typed Rows

`github_events`, `gsoc-2018`, and `instruments` have parse/direct rows but no
`real_typed_struct` row in `skinny/RESULTS.md`. Their current direct rows are
digest plane and are not typed product evidence:

| Corpus | Current direct Track 1 | sonic direct | Delta | Status |
|---|---:|---:|---:|---|
| `github_events` | 11983 | 15800 | -24.2% | `N-direct / NO-GO` |
| `gsoc-2018` | 14676 | 23078 | -36.4% | `N-direct / NO-GO` |
| `instruments` | 11708 | 12194 | -4.0% | `N-direct / NO-GO` |

Citation: `skinny/RESULTS.md:17`, `:27`, `:32`.

## Direct Guard Wins And Frontier

Three direct digest rows currently beat sonic strict, but they remain digest
guard-plane evidence rather than typed product proof:

| Corpus | bbnf Track 1 | sonic strict | Delta | Citation |
|---|---:|---:|---:|---|
| `citm_catalog` | 21129 | 19959 | +5.9% | `skinny/RESULTS.md:9` |
| `marine_ik` | 9205 | 8332 | +10.5% | `skinny/RESULTS.md:29` |
| `unicode_basic` | 8973 | 8625 | +4.0% | `skinny/RESULTS.md:40` |

The remaining 14 direct rows are the primary SK-V10 JSON frontier. They are
closer to a real product boundary than parse-only and have not received a
direct-specific S-P1 profile.

## Parse-Only Caveat

Every parse-only row is `S / NO-GO` (`skinny/RESULTS.md:5-44`). The following
raw Mbps wins are planning signals only, not admissions:

- `citm_catalog/parse_only`: 30383 vs sonic 24910 (+22.0%).
- `canada/parse_only`: 17143 vs sonic 13804 (+24.2%).
- `mesh/parse_only`: 13101 vs sonic 11620 (+12.7%).
- `marine_ik/parse_only`: 12706 vs sonic 9576 (+32.7%).
- `numbers/parse_only`: 17765 vs sonic 13203 (+34.5%).

The row output plane is borrowed view over offset tape vs DOM and strictness is
deferred. SK-V10 must not close on parse-only SOTA deltas unless a later wave
changes the measured validation path and `gate-json` consumes that change.

## Comparator Freshness

Native Rust comparators are same-run. C++ sidecars are historical or absent in
the current report. `simdjson`, `yyjson`, `asmjson`, and `RapidJSON` values may
guide candidate selection, but they are not strict anchors without a same-run
sidecar manifest consumed by the gate.

## Carry-Forward

SK-V10 should score typed product rows first. The strongest live deltas are
`citm_catalog` (+59.1%), `marine_ik` (+32.4%), and `twitter` (+15.3%). The next
typed-plane candidates need fresh same-run generated/serde/sonic typed rows
before any SOTA claim.
