# SK-V10 P1-C: Masking And Primitive-Proof Probes

Pass: S-P1 Profile. Cycle: V1.
Date: 2026-05-19.
Scope: record the same-host Mode III Criterion probes that distinguish
primitive opportunity from output-plane or materialization masking.
Output: this file.
Baseline: SK-V10 Alpha inherits W1-rendered `SK-V9-open`, run
`sk-v9-open:criterion-fnv64-a1e8a51ae806d386`.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
Build flags: `RUSTFLAGS=-C target-cpu=native`, release profile.
Criterion root: `/tmp/skv10-p1/mode3-criterion`.

## Section 1 - Method

Commands:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny

CRITERION_HOME=/tmp/skv10-p1/mode3-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo bench -p bbnf-bench --bench json_parity -- json/probes --quiet

CRITERION_HOME=/tmp/skv10-p1/mode3-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo bench -p bbnf-bench --bench simd_scan -- simd/structural_scan --quiet

cd /Users/mkbabb/Programming/bbnf-lang
python3 restart/skinny/tranches/sk-v10/research/p1/tools/extract_mode3_criterion.py \
  /tmp/skv10-p1/mode3-criterion
```

Mode III is not a row-admission bench. It is a masking probe set:

- `cold_first_parse` is the whole generated parse path in the probe harness.
- `host_call_eager_decode` forces eager decoded-value materialization.
- `alternate_scalar_plan` is the scalar competitor/control path and is not a
  proposed bbnf route.
- `host_call_dispatch_overhead` is a harness-control leaf; it is excluded from
  the row table because its throughput is dominated by the empty dispatch
  shape, not by JSON work.
- `simd_structural_scan` isolates structural scanning only. It can prove that a
  primitive is fast in isolation, but SK-V9 REDRESS 96-98 prove that
  materializing the scan as a union/class substrate regresses the parse path.

Throughput below is computed as megabits per second using
`bytes * 8_000 / mean_ns`, where `mean_ns` is the Criterion mean point estimate
from `new/estimates.json` and `bytes` is the Criterion throughput count from
the paired `new/benchmark.json`. This matches the SK JSON ledger's Mbps unit;
there is no hidden batch factor.

## Section 2 - Decode And Scalar Masking

| Corpus | Bytes | cold first parse Mbps | eager decode Mbps | eager/cold time | alternate scalar Mbps | alternate/cold time |
|---|---:|---:|---:|---:|---:|---:|
| `twitter` | 631515 | 12959 | 4412 | 2.94x | 6946 | 1.87x |
| `citm_catalog` | 1727204 | 29291 | 7664 | 3.82x | 8077 | 3.63x |
| `canada` | 2251051 | 15998 | 4281 | 3.74x | 4934 | 3.24x |
| `apache_builds` | 127275 | 11199 | 4681 | 2.39x | 5749 | 1.95x |
| `github_events` | 65132 | 14188 | 4878 | 2.91x | 6342 | 2.24x |
| `update_center` | 533178 | 10329 | 2527 | 4.09x | 3674 | 2.81x |
| `mesh` | 723597 | 12549 | 5331 | 2.35x | 5034 | 2.49x |
| `random` | 510476 | 7845 | 2905 | 2.70x | 3910 | 2.01x |
| `gsoc-2018` | 3327831 | 21827 | 7633 | 2.86x | 18496 | 1.18x |
| `marine_ik` | 2983466 | 12525 | 2496 | 5.02x | 4023 | 3.11x |
| `instruments` | 220346 | 16834 | 5599 | 3.01x | 5269 | 3.19x |
| `numbers` | 150124 | 18735 | 9712 | 1.93x | 6279 | 2.98x |
| `unicode_mixed` | 1053086 | 6301 | 1915 | 3.29x | 5182 | 1.22x |
| `unicode_escapes` | 1050797 | 12872 | 2256 | 5.71x | 5553 | 2.32x |
| `unicode_basic` | 1048586 | 8144 | 2861 | 2.85x | 4230 | 1.93x |
| `distinct_values` | 153630 | 9582 | 3786 | 2.53x | 4158 | 2.30x |
| `y_string_unicode` | 35601 | 5732 | 1899 | 3.02x | 5596 | 1.02x |

Findings:

- Eager decoded-value materialization is slower on every row, from 1.93x slower
  on `numbers` to 5.71x slower on `unicode_escapes`. SK-V10 must not route a
  direct-plane intervention through decoded scratch, semantic facts, or receiver
  materialization without a material differential against REDRESS 66-69.
- `alternate_scalar_plan` is close only on `y_string_unicode` (1.02x slower) and
  partially close on `gsoc-2018` (1.18x slower). That makes those rows
  diagnostic candidates for S-P2, not authorization to replace generated direct
  output with a scalar competitor.
- The unicode rows remain primitive-bound, but the decode probe says the next
  candidate must reduce escape/string work in the existing output plane; it
  cannot win by eagerly materializing all decoded strings.

## Section 3 - Structural Scan Isolation

| Corpus | SIMD scan Mbps | scalar scan Mbps | SIMD/scalar speedup |
|---|---:|---:|---:|
| `twitter` | 23335 | 9249 | 2.52x |
| `citm_catalog` | 25416 | 9288 | 2.74x |
| `canada` | 39958 | 7498 | 5.33x |
| `apache_builds` | 21063 | 9951 | 2.12x |
| `github_events` | 25422 | 10460 | 2.43x |
| `update_center` | 19049 | 10041 | 1.90x |
| `mesh` | 44497 | 7277 | 6.11x |
| `random` | 16937 | 10126 | 1.67x |
| `gsoc-2018` | 25614 | 11002 | 2.33x |
| `marine_ik` | 23413 | 7487 | 3.13x |
| `instruments` | 22132 | 9446 | 2.34x |
| `numbers` | 43974 | 8434 | 5.21x |
| `unicode_mixed` | 19978 | 8539 | 2.34x |
| `unicode_escapes` | 22053 | 13176 | 1.67x |
| `unicode_basic` | 16919 | 9734 | 1.74x |
| `distinct_values` | 17464 | 10917 | 1.60x |
| `y_string_unicode` | 22275 | 11697 | 1.90x |

The isolated SIMD scanner is faster than scalar structural scanning on every
row. That fact is not sufficient to reopen W3. SK-V9 W3 V1/V2 already
implemented the materialized consumption path and regressed every must-improve
and maintain row. The SK-V10 use of this table is narrower: any future kernel
candidate must micro-prove both the primitive and the product-plane caller that
consumes it, not just the primitive alone.

## Section 4 - S-P2 Routing Implications

- `unicode_escapes`, `unicode_mixed`, and `y_string_unicode` stay eligible for
  an existing-substrate escape/string primitive, but only if S-P2 can prove that
  the caller avoids eager decoded materialization.
- `gsoc-2018` is eligible for typed-root or direct-output investigation because
  the scalar-control gap is small and the direct profile shows `simd_movemask`;
  it is not eligible for a new class-column substrate.
- `numbers`, `mesh`, and `canada` show large isolated scan speedups, but P1-D
  and P1-E classify their product-plane costs as number/array work. Structural
  scan alone is therefore a masking signal, not a candidate.
- `instruments`, `github_events`, and `update_center` remain typed-plane
  candidates because their direct/typed hot leaves are generated string or
  whitespace leaves and the eager-decode control rejects materialization.

## Section 5 - Sources

- `/tmp/skv10-p1/mode3-criterion/json_probes_*/`
- `/tmp/skv10-p1/mode3-criterion/simd_structural_scan/`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- `skinny/crates/bbnf-bench/benches/simd_scan.rs`
- `restart/skinny/tranches/sk-v10/research/p1/tools/extract_mode3_criterion.py`
- `restart/skinny/tranches/sk-v10/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1d-pmu-cycles.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md`
