# SK-V13 W15.1 Redress - UpdateCenter Typed Plugin Fast Path

Date: 2026-05-22.
Disposition: ADMIT.
Gate: `G-W15.1-JSON-TYPED-UPDATE-CENTER-PLUGIN`.

## Result

`json/update_center/real_typed_struct/main` moves from `OPEN` to
`ADMITTED`. The wave keeps the typed product contract and adds an ordered
`Plugin` fast path in generated real-typed output, with fallback to the generic
typed parser on any shape mismatch. It does not touch the direct plane,
parse-only plane, union substrate, CSS runtime, or aarch64 SIMD primitives.

## Measurement

Native Criterion was refreshed with:

```text
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/(update_center|twitter|github_events|mesh|marine_ik)/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)'
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/github_events/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)'
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench simd_scan
```

The companion facts measured:

| lane | Mbps |
|---|---:|
| Track 1 generated mean | 13264.676 |
| Track 1 generated lower confidence | 13220.831 |
| Track 2 serde/oracle | 10407.236 |
| sonic-rs strict | 12598.123 |
| serde_json | 10410.665 |
| threshold | 12599.123 |
| mean margin | 665.554 |
| lower-confidence margin | 621.708 |

Typed guard rows held against same-run sonic strict:

| guard row | Track 1 Mbps | sonic strict Mbps | status |
|---|---:|---:|---|
| twitter | 17891.124 | 15483.243 | maintain |
| github_events | 13055.906 | 12619.336 | maintain |
| mesh | 9685.424 | 8856.805 | maintain |
| marine_ik | 12143.344 | 9198.260 | maintain |

The gate-generated `RESULTS.md` row records Track 1 `13191` Mbps, Track 2
`10417` Mbps, sonic strict `12623` Mbps, and rolling margin `567.00` Mbps.
`ROLLING-SOTA-DELTA.md` was regenerated from the same gate-consumed
`RESULTS.md`; W15.1 claims only `json/update_center/real_typed_struct/main`.
Rows that remain `OPEN` in the direct plane, including `mesh`, are carried as
current gate state and are not W15.1 row movement.

The first aggregate check admitted the W15.1 report but rejected stale SIMD
metadata. The SIMD scan bench was rerun only to refresh required gate metadata;
W15.1 does not claim a SIMD admission. A later aggregate render exposed a
borderline stale/variant `github_events` typed guard miss; the focused
confirmation rerun restored the guard and was folded into the retained facts.

## Verification

```text
cargo xtask regen-real-typed
cargo test -p bbnf-bench generated_update_center_typed_parser_matches_sidecars -- --nocapture
cargo test -p bbnf-bench w2_full_real_typed_fixtures_match_sidecars -- --nocapture
cargo test -p codegen emits_typed_direct_consumer_module -- --nocapture
cargo xtask check-real-typed
cargo test -p bbnf-bench real_typed_struct -- --nocapture
cargo test -p bbnf-bench admits_sk_v13_w15_1_parent_diff_under_w15_1_scope -- --nocapture
cargo test -p bbnf-bench w15_update_center_typed_admits_only_strict_sonic_plus_one_pass -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --update-results --advisory
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-typed-product-report ../restart/skinny/tranches/sk-v13/research/w15.1/skv13-W15.1-typed-product.json
```

Earlier in the wave, `cargo test -p codegen json -- --nocapture` failed outside
the W15.1 owner slice in `tests::json_config_policy_fields_are_consumed`
because the test searches for the exact `config::STRING_NEEDS_DECODE` spelling
while current generated code consumes the policy through
`config::needs_decode_flags()` and `config::string_needs_decode`.

## Artifacts

- `restart/skinny/tranches/sk-v13/research/w15.1/update-center-typed-facts.json`
- `restart/skinny/tranches/sk-v13/research/w15.1/skv13-W15.1-typed-product.json`
- SHA-256: `2a652e0b8e3ec3608ca2bdd4c1bf539557f337653112c58f7076c3ee37147112`

## Remainder

UpdateCenter typed is now admitted under the full-SOTA pinned bar. Remaining
JSON work must target rows still `OPEN` or `MISSING` in
`restart/skinny/ROLLING-SOTA-DELTA.md`; W15.1 does not change direct, parse-only,
CSS, union, or SIMD dispositions.
