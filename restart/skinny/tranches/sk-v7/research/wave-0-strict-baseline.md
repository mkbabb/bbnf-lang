# SK-V7 Wave 0 Strict Baseline: sonic-rs comparator-plane repair

Inputs: `restart/skinny/tranches/sk-v7/SPEC.md` §2,
`restart/skinny/tranches/sk-v7/research/wave-0-plan.md`, and REDRESS item
75.

Intervention: remove `utf8_lossy` from the `sonic-rs` bench dependency so the
S anchor no longer accepts lossy UTF-8 on rows marked as strict comparator
rows.

## Verification Commands

| Command | Result | Evidence |
|---|---|---|
| `cargo tree -p bbnf-bench --edges=features | rg 'sonic-rs|utf8_lossy|sort_keys'` | PASS | feature tree showed `sonic-rs feature "sort_keys"` and no `utf8_lossy` |
| `cargo bench -p bbnf-bench --bench json_parity` | PASS | Criterion run completed on the strict rebuild |
| `cargo run -p bbnf-bench --bin gate --release` | WROTE RESULTS, EXIT 5 | `skinny/RESULTS.md` refreshed; process exit remained 5 because the overall gate is still `N-direct / NoGo` |

The source repair is therefore admitted as a comparator-plane fix. The W0
row-flip forecast is not admitted: strict sonic did not produce the expected
3-8% uniform slowdown and did not reclassify `instruments` or `unicode_basic`.

## Parse Row Delta

Baseline is the checked-in `skinny/RESULTS.md` at the W0 plan commit. Current
is the gate output after removing `utf8_lossy`.

| Corpus | T1 Mbps | T2 Mbps | sonic Mbps | sonic delta | T1/S before -> after | Outcome before -> after |
|---|---:|---:|---:|---:|---:|---|
| twitter | 15597 -> 15462 | 12128 -> 11473 | 21184 -> 18972 | -10.4% | 73.6% -> 81.5% | G/NO-GO -> G/NO-GO |
| citm_catalog | 32459 -> 31487 | 20792 -> 13999 | 24910 -> 24821 | -0.4% | 130.3% -> 126.9% | G/NO-GO -> G/NO-GO |
| canada | 18775 -> 18859 | 17133 -> 17123 | 12658 -> 13782 | +8.9% | 148.3% -> 136.8% | A/GO -> A/GO |
| apache_builds | 12638 -> 12732 | 12227 -> 12147 | 16206 -> 17324 | +6.9% | 78.0% -> 73.5% | G/NO-GO -> G/NO-GO |
| github_events | 15268 -> 15358 | 13034 -> 13001 | 22182 -> 22881 | +3.2% | 68.8% -> 67.1% | G/NO-GO -> G/NO-GO |
| update_center | 11912 -> 11778 | 9226 -> 9166 | 19983 -> 19649 | -1.7% | 59.6% -> 59.9% | G/NO-GO -> G/NO-GO |
| mesh | 14330 -> 13659 | 13173 -> 11793 | 11837 -> 10107 | -14.6% | 121.1% -> 135.1% | A/GO -> A/GO |
| random | 10071 -> 9534 | 7800 -> 7012 | 15370 -> 14711 | -4.3% | 65.5% -> 64.8% | G/NO-GO -> G/NO-GO |
| gsoc-2018 | 23161 -> 23220 | 21870 -> 21950 | 43207 -> 49213 | +13.9% | 53.6% -> 47.2% | G/NO-GO -> G/NO-GO |
| marine_ik | 13688 -> 13648 | 12801 -> 12649 | 10064 -> 9803 | -2.6% | 136.0% -> 139.2% | A/GO -> A/GO |
| instruments | 18163 -> 17919 | 11826 -> 11829 | 19737 -> 19572 | -0.8% | 92.0% -> 91.6% | G/NO-GO -> G/NO-GO |
| numbers | 20085 -> 20340 | 18671 -> 18741 | 13567 -> 13625 | +0.4% | 148.0% -> 149.3% | A/GO -> A/GO |
| unicode_mixed | 8914 -> 7978 | 8940 -> 8979 | 15892 -> 16722 | +5.2% | 56.1% -> 47.7% | G/NO-GO -> G/NO-GO |
| unicode_escapes | 12905 -> 11185 | 12931 -> 11987 | 16048 -> 18734 | +16.7% | 80.4% -> 59.7% | G/NO-GO -> G/NO-GO |
| unicode_basic | 12193 -> 12016 | 10782 -> 10634 | 13304 -> 15765 | +18.5% | 91.7% -> 76.2% | G/NO-GO -> G/NO-GO |
| distinct_values | 9783 -> 9001 | 6100 -> 6082 | 16259 -> 17737 | +9.1% | 60.2% -> 50.7% | G/NO-GO -> G/NO-GO |
| y_string_unicode | 6290 -> 6258 | 6034 -> 5879 | 13673 -> 13020 | -4.8% | 46.0% -> 48.1% | G/NO-GO -> G/NO-GO |

No parse row changed outcome class. The strict comparator changed the anchor
plane, but the checked-in W0 expectation of a uniform sonic-rs drop was false:
the sonic column ranged from -14.6% to +18.5%.

## Outcome Reclassification

Parse:

- `instruments` did not pass: Track 1/S moved from 92.0% to 91.6%.
- `unicode_basic` did not pass or remain as a narrow residual: Track 1/S moved
  from 91.7% to 76.2%.
- The A/G parse set is unchanged: `canada`, `mesh`, `marine_ik`, and `numbers`
  stay A/GO; every prior G/NO-GO row stays G/NO-GO.

Direct workloads:

- `github_events direct_to_struct` moved PASS -> NO-GO after the sonic typed
  baseline increased from 10825 Mbps to 15071 Mbps.
- `mesh direct_to_struct`, `marine_ik direct_to_struct`, `numbers
  direct_to_struct`, and `unicode_basic direct_to_struct` moved NO-GO -> PASS.
- `instruments direct_to_struct` moved PASS -> NO-GO.

These direct workload flips are comparator-baseline consequences from the same
bench/gate run, not parser source edits; W0 touched no parser or runtime code.

## Schema v3 Population

The refreshed `skinny/RESULTS.md` still uses the existing gate report shape. It
does populate the current strictness, output-plane, UTF-8, escape, flaw-probe,
Track 1, Track 2, sonic-rs, simd-json, S-anchor, and ratio columns.

It does not yet populate the full PASS-ALPHA §4.3 schema v3 surface:

- no separate `sonic-rs strict` and `sonic-rs lossy` columns;
- no `yyjson`, `asmjson`, or `RapidJSON` columns;
- no per-row strict-vs-lossy delta column;
- no generated `Hot leaf`, `Primitive status`, build flag, feature mask,
  sidecar freshness, corpus hash, hardware, or API-symbol columns.

W0 therefore records a comparator repair plus a report-harness gap. The next
candidate is W0b: keep the strict dependency, add same-run schema-v3 comparator
reporting for strict/lossy provenance and missing S-anchor columns, and rerun
the row-close gate before opening W1.
