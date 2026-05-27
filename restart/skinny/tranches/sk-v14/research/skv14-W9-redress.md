# SK-V14 W9 Redress - JSON Direct + Typed Re-Admit

Wave: W9 JSON Direct + Typed Re-Admit (R7).
Gate: G-SK-V14-W9-JSON-DIRECT-TYPED-READMIT.
Disposition: MIXED.

## Summary

W9 selected all 17 JSON `direct_to_struct` rows and all 17 JSON
`real_typed_struct` rows for executable disposition.

- 11 / 17 typed rows admit under cold `profile_direct` measurement.
- 17 / 17 direct rows remain blocked because the executable comparator plane is
  still generic digest evidence, not `<corpus>::strict_struct_deser`.
- 6 / 17 typed rows remain blocked because no typed product surface exists at
  HEAD.

No CSS L4 row changes in W9. No Stage-0 F-V2-P1ABC-RERECORD work lands in W9;
SPEC Section 12 routes that obligation unconditionally to W10 because W10 is
the first parse_only distinct-path wave consuming C1/C3/C7.

## Executable Evidence

Commands run from `skinny/` unless noted otherwise:

```sh
cargo test --profile ax-iter -p bbnf-bench json_w9 -- --nocapture
cargo test --profile ax-iter -p bbnf-bench direct_struct -- --nocapture
cargo test --profile ax-iter -p bbnf-bench real_typed_struct -- --nocapture
CARGO_TARGET_DIR=/tmp/skv14-w9-target RUSTFLAGS="-C target-cpu=native" cargo build --release -p bbnf-bench --bin profile_direct
```

The focused tests passed. A full `cargo test --profile ax-iter -p bbnf-bench
--lib -- --nocapture` was also run and failed only in the pre-existing CSS L4
`nonjson_css_l4::*` fact-stream/full-parse failures already routed by
REDRESS-215.

Cold measurement rows are retained at:

`restart/skinny/tranches/sk-v14/research/skv14-W9-profile-direct.tsv`

## Admitted Typed Rows

Each row cleared `Track 1 Mbps > typed sonic strict Mbps + 1.0` with executable
Track 2 and serde typed comparators present.

| corpus | track1_mbps | typed_sonic_plus_1_mbps | margin_mbps | bytes | iters |
|---|---:|---:|---:|---:|---:|
| twitter | 10705.052 | 8953.253 | 1751.799 | 631515 | 400 |
| citm_catalog | 20512.601 | 12663.292 | 7849.309 | 1727204 | 400 |
| apache_builds | 4352.262 | 3391.813 | 960.449 | 127275 | 400 |
| github_events | 6643.660 | 5976.170 | 667.490 | 65132 | 400 |
| update_center | 6776.277 | 5846.377 | 929.900 | 533178 | 400 |
| mesh | 4580.286 | 4344.219 | 236.067 | 723597 | 400 |
| random | 4354.292 | 3042.021 | 1312.271 | 510476 | 400 |
| marine_ik | 5515.099 | 5241.980 | 273.119 | 2983466 | 400 |
| instruments | 9550.109 | 7780.123 | 1769.986 | 220346 | 400 |
| numbers | 6608.574 | 6023.910 | 584.664 | 150124 | 400 |
| unicode_basic | 3221.328 | 2481.522 | 739.806 | 1048586 | 400 |

These rows move to `AUDIT-SUSTAINED` in `skinny/RESULTS.md` and `ADMITTED` in
`restart/skinny/ROLLING-SOTA-DELTA.md`.

## Direct Row Block

All selected direct rows pass current digest parity, but W9 admission requires
per-corpus strict struct deserialization product evidence. The present direct
surface is:

- Track 1: `bbnf_bench::direct_struct::track1_digest`
- Track 2: `bbnf_bench::direct_struct::track2_digest`
- Comparator: `bbnf_bench::direct_struct::sonic_digest`

That is still `digest`, not `<corpus>::strict_struct_deser`, so all 17 direct
rows remain open and must not be relabeled from digest evidence.

Blocked direct corpora:

`twitter`, `citm_catalog`, `canada`, `apache_builds`, `github_events`,
`update_center`, `mesh`, `random`, `gsoc-2018`, `marine_ik`, `instruments`,
`numbers`, `unicode_mixed`, `unicode_escapes`, `unicode_basic`,
`distinct_values`, `y_string_unicode`.

Block id:
`SKV14-W9-DIRECT-DIGEST-NOT-PER-CORPUS-STRUCT-PRODUCT`.

## Missing Typed Product Surfaces

Six typed rows have no generated typed product/oracle surface at HEAD and stay
`MISSING` / `INTRINSIC-BLOCK:missing-product-surface`.

`canada`, `gsoc-2018`, `unicode_mixed`, `unicode_escapes`,
`distinct_values`, `y_string_unicode`.

Block id:
`SKV14-W9-TYPED-PRODUCT-SURFACE-MISSING`.

## Downstream

W10 is now dispatchable because W9 has a closed disposition and admits typed
rows honestly. W10 must not cite W9 direct rows or missing typed rows as
admitted evidence. W10 still carries the unconditional Stage-0
F-V2-P1ABC-RERECORD obligation before any parse_only distinct-path admission.
