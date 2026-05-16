# SK-V7 W4 R3 Bench/Gate Research

Date: 2026-05-16.

Scope: read-only inspection of `bbnf-bench` fixture wiring, current
`skinny/RESULTS.md` rows, and the SK-V7 SPEC Section 6 falsifiability gate for
`unicode_escapes` and `y_string_unicode`. No source files were edited.

## Authority

- `restart/skinny/tranches/sk-v7/SPEC.md` Section 6 scopes W4 to the B1
  per-`\uXXXX` TBL classifier.
- SPEC Section 6 says the C1 correction narrows W4 to exactly two applicable
  rows: `unicode_escapes` and `y_string_unicode`; `unicode_mixed` and
  `distinct_values` have 0% `\uXXXX` content and move to W5.
- SPEC Section 6 requires benching those two rows in both `parse_only` and
  `direct_to_struct`, requires both named rows to cross their thresholds, and
  rejects any row regression of at least 3%.
- `skinny/RESULTS.md` is the current gate authority.

## Fixture Facts

`bbnf-bench` loads both fixtures through
`skinny/crates/test-fixtures/corpus/json/manifest.toml` and includes them in the
17-fixture canonical JSON corpus list in `skinny/crates/test-fixtures/src/lib.rs`.

| Corpus | Fixture file | Manifest size B | sha256 | `\uXXXX` count | `\uXXXX` bytes / total | W4 reading |
|---|---|---:|---|---:|---:|---|
| `unicode_escapes` | `skinny/test_data/unicode_escapes.json` | 1050797 | `45f2abf7ca9deed1983ef51a92ac263ab6383909b78b2075ca6c21ea6407747c` | 136682 | 78.0% | B1 applies |
| `y_string_unicode` | `skinny/test_data/y_string_unicode.json` | 35601 | `f294861884d59ffa5ac0e994758813e40a30c0c496a99bc40cec9d87b6f5c6bc` | 4400 | 74.2% | B1 applies |

The count formula is `count * 6 / size_bytes`, matching
`restart/skinny/tranches/sk-v7/research/skv7-C1-parse-profile.md`.

## Current RESULTS Rows

Current checked-in rows from `skinny/RESULTS.md`:

```markdown
| unicode_escapes | parse_only | K | NO-GO | deferred | view-boundary | yes | invalid UTF-8 rejected outside hot scan; lossy/permissive competitors are flaw probes | borrowed view over offset tape vs DOM | 12042 | 11146 | 18415 | 18828 | 5637 | n/a | n/a | n/a | n/a | n/a | 4810 | n/a (no machine-readable SK-V6 baseline in W0b) | -34.6% | +113.6% | n/a | unprofiled in W0b; no kernel prescription from this row | NO-GO parse gate classified K |
| unicode_escapes | direct_to_struct | N-direct | NO-GO | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | digest | 4866 | 4973 | 14028 | n/a | n/a | n/a | n/a | n/a | n/a | n/a | 5168 | n/a (no machine-readable SK-V6 baseline in W0b) | -65.3% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 4866, Track 2 4973, sonic 14028 Mbps |
| y_string_unicode | parse_only | K | NO-GO | deferred | view-boundary | yes | invalid UTF-8 rejected outside hot scan; lossy/permissive competitors are flaw probes | borrowed view over offset tape vs DOM | 6216 | 6038 | 13537 | 13551 | 13627 | n/a | n/a | n/a | n/a | n/a | 5704 | n/a (no machine-readable SK-V6 baseline in W0b) | -54.1% | -54.4% | n/a | unprofiled in W0b; no kernel prescription from this row | NO-GO parse gate classified K |
| y_string_unicode | direct_to_struct | N-direct | NO-GO | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | digest | 5029 | 3766 | 9019 | n/a | n/a | n/a | n/a | n/a | n/a | n/a | 7604 | n/a (no machine-readable SK-V6 baseline in W0b) | -44.2% | n/a | n/a | unprofiled in W0b; no kernel prescription from this row | NO-GO sink_only throughput > sonic-rs * 1.10 ns slack; correctness PASS; Track 1 5029, Track 2 3766, sonic 9019 Mbps |
```

Condensed calculation from those rows:

| Corpus | Workload | Track 1 Mbps | Track 2 Mbps | sonic-rs strict Mbps | Track 1 / sonic | Current verdict |
|---|---|---:|---:|---:|---:|---|
| `unicode_escapes` | `parse_only` | 12042 | 11146 | 18415 | 65.4% | `K / NO-GO` |
| `unicode_escapes` | `direct_to_struct` | 4866 | 4973 | 14028 | 34.7% | `N-direct / NO-GO` |
| `y_string_unicode` | `parse_only` | 6216 | 6038 | 13537 | 45.9% | `K / NO-GO` |
| `y_string_unicode` | `direct_to_struct` | 5029 | 3766 | 9019 | 55.8% | `N-direct / NO-GO` |

## Bench/Gate Mechanics

`skinny/crates/bbnf-bench/benches/json_parity.rs` emits, per fixture:

| Workload | Same-row Criterion benches |
|---|---|
| `parse_only` | `track1_generated`, `track2_handcoded`, `sonic_rs_anchor`, `sonic_rs_lossy`, `simd_json_borrowed`, `simd_json_owned`, `serde_json` |
| `direct_to_struct` | `track1_direct_to_struct`, `track2_direct_to_struct`, `sonic_rs_direct_to_struct`, `serde_json_direct_to_struct` |

`skinny/crates/bbnf-bench/src/bin/gate.rs` reads
`target/criterion/json_<corpus>/<bench>/new/estimates.json` and converts
nanoseconds to Mbps as:

```text
Mbps = size_bytes * 8000 / ns
```

The direct workload has an additional built-in gate:

```text
Track 1 ns <= sonic_rs_ns * 1.10
Track 2 ns <= sonic_rs_ns * 1.10
```

Equivalent Mbps form:

```text
Track 1 Mbps >= sonic_rs_mbps / 1.10
Track 2 Mbps >= sonic_rs_mbps / 1.10
```

That direct slack gate is not a substitute for SPEC Section 6. It is a
`gate-json` veto that can still reject a row even if a W4-specific threshold
calculation is reported.

## Exact Commands

Full W4 close protocol, combining SPEC Section 0.4 and the Section 6 parity
exit requirement:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo run -p xtask --release -- check-conformance
cargo run -p xtask --release -- primitive-checkasm
cargo run -p xtask --release -- bench-json
cargo run -p xtask --release -- gate-json
```

Focused W4 measurement loop for the two target rows only. This is not a full
wave close because unmeasured Criterion estimates can be stale:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo bench -p bbnf-bench --bench json_parity -- 'json/(unicode_escapes|y_string_unicode)/(track1_generated|track2_handcoded|sonic_rs_anchor|sonic_rs_lossy|simd_json_borrowed|simd_json_owned|serde_json|track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)$'
cargo run -p bbnf-bench --bin gate --release -- --advisory
```

Row extraction after a full or focused run:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
awk -F'|' '
function trim(s){gsub(/^ +| +$/, "", s); return s}
$2 ~ / (unicode_escapes|y_string_unicode) / && ($3 ~ / parse_only / || $3 ~ / direct_to_struct /) {
  corpus=trim($2); workload=trim($3); outcome=trim($4); verdict=trim($5);
  t1=trim($11)+0; t2=trim($12)+0; sonic=trim($13)+0; signal=trim($27);
  ratio=(sonic>0 ? 100*t1/sonic : 0);
  printf "%s\t%s\t%s\t%s\tTrack1=%d\tTrack2=%d\tsonic=%d\tT1/sonic=%.1f%%\tSignal=%s\n", corpus, workload, outcome, verdict, t1, t2, sonic, ratio, signal
}
' RESULTS.md
```

Raw Criterion estimate extraction for same-row measurements:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
for corpus in unicode_escapes y_string_unicode; do
  bytes=$(awk -v c="$corpus" '
    $0 == "[fixtures." c "]" { in_row=1; next }
    in_row && /^size_bytes = / { print $3; exit }
  ' crates/test-fixtures/corpus/json/manifest.toml)
  printf "%s\n" "$corpus"
  for bench in track1_generated track2_handcoded sonic_rs_anchor sonic_rs_lossy simd_json_borrowed simd_json_owned serde_json track1_direct_to_struct track2_direct_to_struct sonic_rs_direct_to_struct serde_json_direct_to_struct; do
    ns=$(jq -r '.slope.point_estimate // .mean.point_estimate' "target/criterion/json_${corpus}/${bench}/new/estimates.json")
    awk -v bench="$bench" -v bytes="$bytes" -v ns="$ns" 'BEGIN { printf "  %-32s ns=%12.2f Mbps=%8.0f\n", bench, ns, bytes * 8000 / ns }'
  done
done
```

## Same-Row Measurements Required

For each of `unicode_escapes` and `y_string_unicode`, record all of the
following from the same candidate run:

| Required value | Reason |
|---|---|
| `parse_only` Track 1 Mbps from `track1_generated` | B1 target row, generated parser |
| `parse_only` Track 2 Mbps from `track2_handcoded` | substrate/no-regression guard and parse gate input |
| `parse_only` sonic-rs strict Mbps from `sonic_rs_anchor` | threshold denominator |
| `parse_only` sonic-rs lossy and serde_json Mbps | required schema/gate columns |
| `direct_to_struct` Track 1 Mbps from `track1_direct_to_struct` | B1 target row, generated direct receiver |
| `direct_to_struct` Track 2 Mbps from `track2_direct_to_struct` | independent direct receiver and N-direct veto input |
| `direct_to_struct` sonic-rs strict Mbps from `sonic_rs_direct_to_struct` | direct threshold denominator and N-direct veto input |
| `direct_to_struct` serde_json Mbps | required schema/gate column |

The comparison must be same-row. Do not compare a candidate Track 1 row against
an older sonic strict estimate or against a different output plane.

## Pass/Reject Calculation

Use current `skinny/RESULTS.md` as the baseline. For a candidate:

```text
parse_ratio = candidate_parse_track1_mbps / same_run_parse_sonic_strict_mbps
direct_ratio = candidate_direct_track1_mbps / same_run_direct_sonic_strict_mbps

unicode_escapes passes row threshold when:
  parse_ratio >= 0.95
  direct_ratio >= 0.95

y_string_unicode passes row threshold when:
  parse_ratio >= 0.70
  direct_ratio >= 0.70
```

Because SPEC Section 6 now says W4 applies to only these two rows, the exit
condition "2 named rows cross threshold" means both rows must pass. The pass
count is:

```text
pass_count =
  passed(unicode_escapes parse and direct) +
  passed(y_string_unicode parse and direct)

ACCEPT when pass_count == 2 and no measured same-row Track 1 or Track 2 value
regresses by at least 3% versus current RESULTS.

REJECT when pass_count < 2, or any measured same-row Track 1 or Track 2 value
is below 97% of the current RESULTS value.
```

Current checked-in pass/reject calculation:

| Corpus | Workload | Threshold | Current Track 1 Mbps | Target Mbps | Gap Mbps | Current pass? |
|---|---|---:|---:|---:|---:|---|
| `unicode_escapes` | `parse_only` | 95% | 12042 | 17494 | 5452 | no |
| `unicode_escapes` | `direct_to_struct` | 95% | 4866 | 13327 | 8461 | no |
| `y_string_unicode` | `parse_only` | 70% | 6216 | 9476 | 3260 | no |
| `y_string_unicode` | `direct_to_struct` | 70% | 5029 | 6313 | 1284 | no |

Current state: `pass_count = 0`, so the current checked-in rows are a REJECT
for the W4 Section 6 falsifiability gate. A candidate must lift both corpora on
both workloads and preserve the no-regression guard.
