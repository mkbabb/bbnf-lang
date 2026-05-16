# SK-V7 W6 R4 Bench/Gate Research

Date: 2026-05-16.

Scope: read-only inspection of `restart/skinny/tranches/sk-v7/SPEC.md`
Section 8, `skinny/RESULTS.md`, and the `bbnf-bench` bench/gate surfaces for
`citm_catalog` and `instruments`. No source edits are part of this note.

## Authority

- SPEC Section 8 scopes W6 to B6 control/key compaction on the `citm_catalog`
  and `instruments` hot leaves.
- SPEC Section 8 falsifies W6 unless `citm_catalog` parse Track 2 closes and
  `instruments` parse is at least sonic-rs strict.
- SPEC Section 0.1 also assigns `instruments` `direct` to W6 with a target of
  at least sonic-rs strict. Treat that as a W6 threshold row.
- `skinny/RESULTS.md` is the current gate authority. The checked-in RESULTS
  values below supersede SPEC inline "current" percentages when they differ.

## Current W6 Rows

Current checked-in `skinny/RESULTS.md` parse/direct rows:

| Corpus | Workload | Outcome | Verdict | Track 1 Mbps | Track 2 Mbps | sonic-rs strict Mbps | Track 1 / sonic | Track 2 / sonic | W6 reading |
|---|---|---|---|---:|---:|---:|---:|---:|---|
| `citm_catalog` | `parse_only` | `K` | `NO-GO` | 31784 | 20817 | 25509 | 124.6% | 81.6% | Track 1 is already above sonic, but W6's named parse condition is Track 2 >= 90%. Current Track 2 fails. |
| `citm_catalog` | `direct_to_struct` | `A` | `GO` | 21438 | 20280 | 19966 | 107.4% | 101.6% | Guard row only; it must not regress or flip to `N-direct`. |
| `instruments` | `parse_only` | `K` | `NO-GO` | 18038 | 11678 | 16312 | 110.6% | 71.6% | Track 1 already satisfies the >= 100% W6 ratio, but the row still needs a clean same-run gate. Track 2 is below the parse gate's sonic/1.10 hard floor if sonic is the fastest same-run anchor. |
| `instruments` | `direct_to_struct` | `N-direct` | `NO-GO` | 11972 | 11086 | 12673 | 94.5% | 87.5% | W6 direct threshold fails, and the built-in direct projection gate fails because Track 2 is below sonic/1.10. |

`K` is the gate taxonomy's SIMD parity hash failure, so a current `K / NO-GO`
row cannot close W6 even when its Track 1 ratio is numerically above target.

## Exact Thresholds

Use refreshed same-run sonic strict as the denominator for candidate PASS/FAIL.
The Mbps targets below use the current checked-in sonic values only to show the
present gap.

| Row | W6 threshold | Current same-denominator target | Current value | Current gap |
|---|---|---:|---:|---:|
| `citm_catalog` `parse_only` | `Track 2 Mbps / sonic_rs_anchor Mbps >= 0.90` | 22958.1 Track 2 Mbps | 20817 | 2141.1 |
| `instruments` `parse_only` | `Track 1 Mbps / sonic_rs_anchor Mbps >= 1.00` | 16312.0 Track 1 Mbps | 18038 | already +1726.0 |
| `instruments` `direct_to_struct` | `Track 1 Mbps / sonic_rs_direct_to_struct Mbps >= 1.00` | 12673.0 Track 1 Mbps | 11972 | 701.0 |

The direct workload also has a gate-level veto independent of the W6 ratio:

```text
Track 1 ns <= sonic_rs_ns * 1.10
Track 2 ns <= sonic_rs_ns * 1.10
```

Equivalent Mbps form:

```text
Track 1 Mbps >= sonic_rs_mbps / 1.10
Track 2 Mbps >= sonic_rs_mbps / 1.10
```

For current `instruments` direct, that built-in floor is 11520.9 Mbps for both
tracks. Track 1 clears it; Track 2 at 11086 Mbps does not.

Parse rows also have gate-level vetoes after schema/parity/SIMD metadata pass.
The hard parse substrate failure is:

```text
Track 2 ns <= fastest_same_run_anchor_ns * 1.10
```

Equivalent Mbps form:

```text
Track 2 Mbps >= fastest_same_run_anchor_mbps / 1.10
```

If sonic-rs strict is the fastest same-run anchor, the current hard floors are
23190.0 Mbps for `citm_catalog` parse Track 2 and 14829.1 Mbps for
`instruments` parse Track 2. The W6 falsifiability threshold for
`citm_catalog` is still exactly 90% of sonic strict; the gate can be stricter
than the W6 falsifiability row when the refreshed same-run anchors are faster.

## Bench/Gate Surfaces

`skinny/crates/bbnf-bench/benches/json_parity.rs` emits, per fixture:

| Workload | Same-row Criterion benches |
|---|---|
| `parse_only` | `track1_generated`, `track2_handcoded`, `sonic_rs_anchor`, `sonic_rs_lossy`, `simd_json_borrowed`, `simd_json_owned`, `serde_json` |
| `direct_to_struct` | `track1_direct_to_struct`, `track2_direct_to_struct`, `sonic_rs_direct_to_struct`, `serde_json_direct_to_struct` |

`skinny/crates/bbnf-bench/benches/simd_scan.rs` writes the SIMD scalar parity
metadata consumed by the gate. Focused W6 runs must refresh it for
`citm_catalog` and `instruments` or `gate --advisory` can still report a hard
`K` failure.

`skinny/crates/bbnf-bench/src/bin/gate.rs` reads:

```text
target/criterion/json_<corpus>/<bench>/new/estimates.json
```

and converts nanoseconds to Mbps as:

```text
Mbps = size_bytes * 8000 / ns
```

The gate writes `skinny/RESULTS.md`. `--advisory` suppresses non-hard
throughput exit failures from unrelated rows, but it does not make W6 pass; the
focused W6 row calculations below are still authoritative.

## Focused Commands

Profile-first diagnosis before a B6 edit:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny

cargo build --release -p xtask --bin profile-lazy
mkdir -p profile/skv7-w6
samply record --save-only -o profile/skv7-w6/citm_catalog-track1.json.gz ./target/release/profile-lazy 20000 citm_catalog
samply record --save-only -o profile/skv7-w6/instruments-track1.json.gz ./target/release/profile-lazy 60000 instruments

cargo build --release -p bbnf-bench --bin profile_direct
samply record --save-only -o profile/skv7-w6/instruments-direct-track1.json.gz ./target/release/profile_direct 60000 instruments track1
```

Focused W6 measurement loop:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny

cargo run -p xtask --release -- check-conformance

cargo bench -p bbnf-bench --bench simd_scan -- 'simd/structural_scan/(citm_catalog|instruments)/(simd|scalar)$'

cargo bench -p bbnf-bench --bench json_parity -- 'json/(citm_catalog|instruments)/(track1_generated|track2_handcoded|sonic_rs_anchor|sonic_rs_lossy|simd_json_borrowed|simd_json_owned|serde_json|track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)$'

cargo run -p bbnf-bench --bin gate --release -- --advisory
```

If W6 adds or changes a SIMD primitive, also run the primitive gate before the
bench:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo run -p xtask --release -- primitive-checkasm
```

Full non-focused close protocol remains:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo run -p xtask --release -- check-conformance
cargo run -p xtask --release -- bench-json
cargo run -p xtask --release -- gate-json
```

## Row Extraction

After a focused or full gate refresh:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
awk -F'|' '
function trim(s){gsub(/^ +| +$/, "", s); return s}
$2 ~ / (citm_catalog|instruments) / && ($3 ~ / parse_only / || $3 ~ / direct_to_struct /) {
  c=trim($2); w=trim($3); outcome=trim($4); verdict=trim($5);
  t1=trim($11)+0; t2=trim($12)+0; sonic=trim($13)+0; signal=trim($27);
  printf "%s\t%s\t%s/%s\tTrack1=%d\tTrack2=%d\tsonic=%d\tT1/sonic=%.1f%%\tT2/sonic=%.1f%%\t%s\n",
    c, w, outcome, verdict, t1, t2, sonic, 100*t1/sonic, 100*t2/sonic, signal
}
' RESULTS.md
```

Raw same-run estimate extraction:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
for corpus in citm_catalog instruments; do
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

## No-Regression Guard

Use checked-in `skinny/RESULTS.md` as the throughput baseline, and apply the
guard to both Track 1 and Track 2 for the four focused parse/direct rows. Do not
use SPEC inline percentages for this check. Do not let a lower same-run sonic
denominator hide a BBNF regression.

```text
candidate Track 1 Mbps >= current Track 1 Mbps * 0.97
candidate Track 2 Mbps >= current Track 2 Mbps * 0.97
```

If comparing rounded integer Mbps from `RESULTS.md`, require at least these
ceil-rounded floors:

| Corpus | Workload | Track 1 no-regression floor | Track 2 no-regression floor |
|---|---|---:|---:|
| `citm_catalog` | `parse_only` | 30831 | 20193 |
| `citm_catalog` | `direct_to_struct` | 20795 | 19672 |
| `instruments` | `parse_only` | 17497 | 11328 |
| `instruments` | `direct_to_struct` | 11613 | 10754 |

No-regression is a guard, not a substitute for W6 thresholds. For example,
current `instruments` direct would satisfy a 0.97 Track 1 guard only at
11613 Mbps, but W6 still needs at least same-run sonic strict.

## PASS/FAIL

W6 PASS requires all of the following on a refreshed same-run measurement:

- Fresh profile evidence names the B6 container/key bookkeeping leaf for the
  edited path before the edit is admitted.
- `check-conformance` passes.
- If any SIMD primitive changed, `primitive-checkasm` passes.
- `gate --advisory` reports no hard schema, parity, or SIMD metadata failure on
  the focused rows.
- The focused parse rows do not render a gate verdict of `NO-GO`, `INVALID`, or
  `CONDITIONAL` after the advisory refresh.
- `citm_catalog` `parse_only` satisfies `Track 2 / sonic_rs_anchor >= 0.90`.
- `instruments` `parse_only` satisfies `Track 1 / sonic_rs_anchor >= 1.00`.
- `instruments` `direct_to_struct` satisfies
  `Track 1 / sonic_rs_direct_to_struct >= 1.00`.
- `citm_catalog` `direct_to_struct` remains a guard-row PASS: no no-regression
  miss and no `N-direct` flip.
- No focused parse/direct row violates the 0.97 Track 1 or Track 2
  no-regression guard.

W6 FAIL if any named threshold misses, any focused Track 1 or Track 2 value
falls below its no-regression floor, either focused direct row emits
`N-direct`, either focused parse row remains gate-rejected, the focused gate has
a hard `I`, `J`, or `K` failure, or the B6 implementation lacks a fresh PC-level
profile naming the edited hot path.
