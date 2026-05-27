# SK-V14 W11R unicode_escapes Fixed-Shape Floor Reject

Date: 2026-05-27.

Status: REJECT. No source patch lands, no `skinny/RESULTS.md` row moves, and
`restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.

## Candidate

W11R tested a transient fixed-shape `unicode_escapes` floor parser in the cold
`profile_direct` binary. The parser consumed the concrete fixture shape
`{"meta":{"mode":"escapes","ensure_ascii":true},"records":[...]}` directly,
validated every root/member token, record id, string escape, raw control-byte
ban, and surrogate pair, and folded every decoded payload UTF-8 byte into the
same semantic product used by the serde_json and sonic-rs sidecars.

This is materially distinct from REDRESS-242 and REDRESS-243. W11M and W11P
tested generated typed/direct product surfaces; W11R removed that generator and
schema overhead to test the lowest honest fixed-shape parser floor. It also
differs from skipped-payload, digest-plane, and parse_only routes because the
candidate validates and consumes every decoded `records[*].v` payload unit and
all record ids before returning a product.

The transient patch was reverted after measurement and retained at
`/tmp/skv14-W11R-unicode-escapes-floor-rejected.patch` with SHA-256
`268b3d5207b9d252df10cdab37319eafeb11a197d4e72e75d4b3a2e85f471f16`.

## Correctness Gates

- `cargo fmt --manifest-path skinny/Cargo.toml --package bbnf-bench`
- `CARGO_TARGET_DIR=/tmp/skv14-w11r-test-target RUSTC_WRAPPER= cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench --bin profile_direct unicode_escapes_floor -- --nocapture`

The focused test gate passed four tests:

- full `unicode_escapes` fixture parity between the floor parser, serde_json,
  and sonic-rs;
- surrogate-pair and simple-escape acceptance;
- invalid escape, missing low surrogate, unexpected low surrogate, and wrong
  mode rejection;
- raw control-character rejection.

## Cold Evidence

Release-native no-warm profile command:

```sh
CARGO_TARGET_DIR=/tmp/skv14-w11r-profile-target RUSTC_WRAPPER= RUSTFLAGS='-C target-cpu=native' cargo build --manifest-path skinny/Cargo.toml --release -p bbnf-bench --bin profile_direct
/tmp/skv14-w11r-profile-target/release/profile_direct 1000 unicode_escapes unicode_escapes_floor_direct 0
/tmp/skv14-w11r-profile-target/release/profile_direct 1000 unicode_escapes unicode_escapes_floor_typed 0
/tmp/skv14-w11r-profile-target/release/profile_direct 1000 unicode_escapes unicode_escapes_floor_sonic 0
/tmp/skv14-w11r-profile-target/release/profile_direct 1000 unicode_escapes unicode_escapes_floor_serde 0
```

Retained evidence:

- `restart/skinny/tranches/sk-v14/research/skv14-W11R-unicode-escapes-fixed-shape-floor.tsv`
- `restart/skinny/tranches/sk-v14/research/skv14-W11R-unicode-escapes-fixed-shape-floor.raw.log`

Measured results:

| Row | Track 1 Mbps | Same-run sonic Mbps | Margin vs sonic | Deficit vs sonic + 1.0 floor |
|---|---:|---:|---:|---:|
| `unicode_escapes/direct_to_struct` | 751.889 | 1191.214 | -439.325 | -440.325 |
| `unicode_escapes/real_typed_struct` | 819.515 | 1191.214 | -371.699 | -372.699 |

## Disposition

W11R proves that this fixed-shape decoded-payload floor parser is not
sufficient to admit the remaining `unicode_escapes` direct or typed rows. The
route is pre-blocked without a fresh material differential. It does not move
the row tables and does not by itself create an architectural intrinsic-block
proof for every possible implementation route.

Current state remains:

- JSON direct_to_struct: 16 / 17 ADMITTED, 1 OPEN: `unicode_escapes`.
- JSON real_typed_struct: 16 / 17 ADMITTED, 1 MISSING: `unicode_escapes`.
- JSON parse_only: 11 / 17 ADMITTED, 6 OPEN.
