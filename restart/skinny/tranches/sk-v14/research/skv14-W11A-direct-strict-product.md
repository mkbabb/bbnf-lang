# SK-V14 W11A JSON direct_to_struct Strict Product Admit

Date: 2026-05-27.

Status: ADMITTED for every JSON direct row with a generated strict product
surface at HEAD. No digest-plane row moved.

## Route

W11A replaces direct digest evidence for supported rows with a strict product
route: generated Track 1 calls the generated DirectBuild typed product, Track 2
uses the independent serde typed product path, and native comparators deserialize
the same strict product through sonic-rs and serde_json. The public workload
remains `direct_to_struct`; the measured materialisation is
`direct_strict_product`, with output plane `direct strict product`.

Rows without a generated product surface remain open on the old digest evidence:
`gsoc-2018`, `unicode_mixed`, `unicode_escapes`, and `y_string_unicode`.

## Cold Evidence

Command shape:

```text
RUSTFLAGS="-C target-cpu=native" cargo build --manifest-path skinny/Cargo.toml --release -p bbnf-bench --bin profile_direct
CARGO_MANIFEST_DIR=/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench RUSTFLAGS="-C target-cpu=native" skinny/target/release/profile_direct 400 <corpus> direct_strict_<mode> 0
```

Retained artifacts:

- `restart/skinny/tranches/sk-v14/research/skv14-W11A-direct-strict-product.tsv`
- `restart/skinny/tranches/sk-v14/research/skv14-W11A-direct-strict-product.raw.log`

| Corpus | Track 1 Mbps | Track 2 Mbps | sonic Mbps | serde Mbps | Margin vs sonic+1 |
|---|---:|---:|---:|---:|---:|
| distinct_values | 8755.197 | 3271.314 | 3907.274 | 3341.595 | 4846.923 |
| citm_catalog | 33366.495 | 17882.926 | 21250.015 | 18438.265 | 12115.480 |
| canada | 4749.599 | 3479.489 | 2733.746 | 3470.303 | 2014.853 |
| random | 7977.902 | 4315.329 | 5754.672 | 4261.612 | 2222.230 |
| apache_builds | 7483.813 | 5400.085 | 6327.769 | 5373.285 | 1155.044 |
| instruments | 18191.796 | 11352.091 | 14488.541 | 11363.556 | 3702.255 |
| unicode_basic | 6177.340 | 3423.127 | 4692.661 | 3446.968 | 1483.679 |
| github_events | 12501.469 | 11021.444 | 11012.854 | 10724.512 | 1487.615 |
| mesh | 9036.398 | 7223.828 | 7875.325 | 7213.871 | 1160.073 |
| marine_ik | 11162.218 | 9644.427 | 8830.443 | 9674.204 | 2330.775 |
| numbers | 12574.721 | 9157.320 | 11309.297 | 9245.482 | 1264.424 |
| twitter | 17585.679 | 15566.973 | 14857.624 | 15524.494 | 2727.055 |
| update_center | 12820.158 | 8839.131 | 10887.271 | 8649.165 | 1931.887 |

## Disposition

- `json/*/direct_to_struct/main` admits for the thirteen supported corpora
  listed above under `SK-V14-W11A`.
- `skinny/RESULTS.md` moves those rows to `A` / `GO`, strict measured-row,
  output plane `direct strict product`.
- `restart/skinny/ROLLING-SOTA-DELTA.md` marks those rows `ADMITTED`.
- Current JSON direct_to_struct state is 13 / 17 ADMITTED and 4 OPEN.
