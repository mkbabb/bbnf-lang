# SK-V14 W9Y y_string_unicode Typed Product Probe

Date: 2026-05-27.

Disposition: REJECT. No source patch lands, and no `RESULTS.md` or
`ROLLING-SOTA-DELTA.md` row moves.

## Candidate

W9Y tested a generated typed root for `y_string_unicode` through the existing
`regen-real-typed` path, not a bench-private hand parser. The transient source
slice was:

- `skinny/xtask/src/real_typed_schema.rs`: add
  `DirectRootSchema::typed_root("parse_y_string_unicode", "Vec<Cow<'i, str>>",
  vec_with_capacity(string(), 2_200))`.
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`: regenerated
  `parse_y_string_unicode` and `parse_vec_cap_2200_scalar_string`.
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`: transient fixture,
  dispatch, sidecar, checksum, and full-fixture parity wiring.

The earlier row-local hand-written parser route was rejected before commit
because W9 Track 1 is governed by generated DirectBuild/SinkOnly output. The
measured candidate below is the valid generated-route probe.

## Correctness

- `cargo xtask regen-real-typed`
- `cargo xtask check-real-typed`
- `cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p bbnf-bench y_string_unicode_typed -- --nocapture`

The focused generated route tests passed before measurement. The transient
source patch was then reverted because the row did not admit.

## Cold Profile

Build:

```sh
CARGO_TARGET_DIR=/tmp/skv14-ystr-typed-target RUSTFLAGS="-C target-cpu=native" cargo build --manifest-path skinny/Cargo.toml --release -p bbnf-bench --bin profile_direct
```

Run:

```sh
/tmp/skv14-ystr-typed-target/release/profile_direct 400 y_string_unicode real_typed_track1 0
/tmp/skv14-ystr-typed-target/release/profile_direct 400 y_string_unicode real_typed_track2 0
/tmp/skv14-ystr-typed-target/release/profile_direct 400 y_string_unicode real_typed_sonic 0
/tmp/skv14-ystr-typed-target/release/profile_direct 400 y_string_unicode real_typed_serde 0
```

Evidence:

- `restart/skinny/tranches/sk-v14/research/skv14-W9Y-y-string-unicode-typed.tsv`
- `restart/skinny/tranches/sk-v14/research/skv14-W9Y-y-string-unicode-typed.raw.log`

| mode | Mbps | threshold role |
|---|---:|---|
| real_typed_track1 | 3661.016 | candidate |
| real_typed_track2 | 3590.583 | independent sidecar |
| real_typed_sonic | 3906.865 | strict comparator |
| real_typed_serde | 3453.282 | reference sidecar |

Admission threshold: Track 1 must exceed `sonic + 1.0` Mbps. The threshold is
3907.865 Mbps; generated Track 1 reached 3661.016 Mbps, a -246.849 Mbps
margin. Verdict: REJECT.

## Ledger Impact

- JSON real_typed_struct remains 11 / 17 ADMITTED and 6 MISSING.
- `y_string_unicode` remains in the missing typed product queue.
- REDRESS-226 records this generated-route rejection and supersedes no row.
- Source remains unchanged except for the retained rejection artefacts.
