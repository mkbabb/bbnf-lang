# SK-V7 W5 R3 Bench/Gate Research

Date: 2026-05-16.

Scope: read-only inspection of `restart/skinny/tranches/sk-v7/SPEC.md`
Section 7, `skinny/RESULTS.md`, and the `bbnf-bench` bench/gate surfaces.
No source edits are part of this note.

## Authority

- SPEC Section 7 scopes W5 to B2: NEON 16-byte widening for the tiny plain
  string scan, wired through `match_tiny_plain_string_with_cap::<16>`.
- The six named W5 rows are parse rows only: `twitter`, `update_center`,
  `unicode_basic`, `random`, `unicode_mixed`, and `distinct_values`.
- SPEC Section 7 requires at least 4 of those 6 rows to cross their threshold,
  and rejects any named row regression of at least 3%.
- `skinny/RESULTS.md` is the current gate authority. The SPEC inline "current"
  percentages differ from the checked-in RESULTS on several rows; the table
  below uses the current RESULTS Mbps values and recomputes ratios from them.

## Current W5 Parse Rows

Current checked-in `skinny/RESULTS.md` parse rows:

| Corpus | Track 1 Mbps | Track 2 Mbps | sonic-rs strict Mbps | Track 1 / sonic | Track 2 / sonic | W5 threshold |
|---|---:|---:|---:|---:|---:|---:|
| `twitter` | 15752 | 12285 | 21020 | 74.9% | 58.4% | 90% |
| `update_center` | 11193 | 9227 | 19684 | 56.9% | 46.9% | 90% |
| `unicode_basic` | 11416 | 10653 | 15596 | 73.2% | 68.3% | 100% |
| `random` | 9838 | 7804 | 15457 | 63.6% | 50.5% | 85% |
| `unicode_mixed` | 8035 | 7698 | 16180 | 49.7% | 47.6% | 85% |
| `distinct_values` | 6655 | 5633 | 17148 | 38.8% | 32.8% | 85% |

All six current rows are `K / NO-GO` parse outcomes in RESULTS.

## Bench/Gate Surfaces

`skinny/crates/bbnf-bench/benches/json_parity.rs` emits the parse benches
needed for W5 under `json/<corpus>/`:

| W5 value | Criterion bench name |
|---|---|
| Track 1 parse Mbps | `track1_generated` |
| Track 2 parse Mbps | `track2_handcoded` |
| same-run sonic strict Mbps | `sonic_rs_anchor` |
| schema comparator columns | `sonic_rs_lossy`, `simd_json_borrowed`, `simd_json_owned`, `serde_json` |

`skinny/crates/bbnf-bench/src/bin/gate.rs` reads
`target/criterion/json_<corpus>/<bench>/new/estimates.json`, computes
`Mbps = size_bytes * 8000 / ns`, writes `skinny/RESULTS.md`, and classifies the
parse gate. It also reads SIMD parity metadata from the `simd_scan` bench. Its
schema check expects direct-to-struct metadata, so the focused command below
includes direct benches for the same six corpora as gate hygiene. The W5 pass
count still uses only the parse rows.

## Exact Focused Commands

Run from the skinny workspace:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny

BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_parity sk_v3_intrinsic_parity_aarch64 -- --nocapture

cargo bench -p bbnf-bench --bench simd_scan -- 'simd/structural_scan/(twitter|update_center|unicode_basic|random|unicode_mixed|distinct_values)/(simd|scalar)$'

cargo bench -p bbnf-bench --bench json_parity -- 'json/(twitter|update_center|unicode_basic|random|unicode_mixed|distinct_values)/(track1_generated|track2_handcoded|sonic_rs_anchor|sonic_rs_lossy|simd_json_borrowed|simd_json_owned|serde_json|track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)$'

cargo run -p bbnf-bench --bin gate --release -- --advisory
```

Use `--advisory` because unrelated non-W5 direct rows can keep the overall
skinny gate at `N-direct / NoGo`. A W5 close still requires the focused rows to
be same-run with their sonic strict anchors and to leave `gate --advisory`
without schema/parity/SIMD hard failures. On a clean `CARGO_TARGET_DIR`, run a
full-corpus `cargo run -p xtask --release -- bench-json --advisory` instead of
the focused subset so non-W5 gate inputs are not missing.

Focused row extraction after the gate refresh:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
awk -F'|' '
function trim(s){gsub(/^ +| +$/, "", s); return s}
BEGIN {
  split("twitter update_center unicode_basic random unicode_mixed distinct_values", rows, " ");
  for (i in rows) want[rows[i]]=1
}
$3 ~ / parse_only / {
  c=trim($2);
  if (want[c]) {
    t1=trim($11)+0; t2=trim($12)+0; sonic=trim($13)+0;
    printf "%s Track1=%d Track2=%d sonic=%d T1/sonic=%.1f%% T2/sonic=%.1f%% %s/%s\n",
      c, t1, t2, sonic, 100*t1/sonic, 100*t2/sonic, trim($4), trim($5)
  }
}
' RESULTS.md
```

## PASS/FAIL

For each candidate row, compute against the same-run refreshed RESULTS row:

```text
ratio = parse Track 1 Mbps / same-run sonic-rs strict Mbps
```

Row thresholds:

| Corpus | PASS when same-run ratio is... |
|---|---:|
| `twitter` | `>= 0.90` |
| `update_center` | `>= 0.90` |
| `unicode_basic` | `>= 1.00` |
| `random` | `>= 0.85` |
| `unicode_mixed` | `>= 0.85` |
| `distinct_values` | `>= 0.85` |

W5 PASS requires all of the following:

- `checkasm_parity::sk_v3_intrinsic_parity_aarch64` passes on the candidate.
- At least 4 of the 6 named parse rows meet their ratio threshold.
- No named parse row regresses by at least 3% versus current RESULTS. Apply
  this to both Track 1 and Track 2 parse Mbps:

```text
candidate Track 1 Mbps >= current Track 1 Mbps * 0.97
candidate Track 2 Mbps >= current Track 2 Mbps * 0.97
```

W5 FAIL if fewer than 4 rows cross threshold, any named parse row violates the
0.97 no-regression guard, checkasm fails, or the advisory gate reports a
schema/parity/SIMD hard failure. Direct-to-struct rows in the focused command
are schema-support measurements, not W5 threshold rows.
