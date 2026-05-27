# SK-V14 W9AC gsoc-2018 Typed Product Probe

Date: 2026-05-27.

Disposition: REJECT. No source patch lands, and no `RESULTS.md` or
`ROLLING-SOTA-DELTA.md` row moves.

## Candidate

W9AC tested a generated typed root for `gsoc-2018` through the existing
`regen-real-typed` path. The fresh material differential over the prior W13.5
reject was generated numeric object-key parsing: root map keys were captured as
`u32` instead of generic `Cow<'i, str>` map-entry keys.

The transient source slice was:

- `skinny/crates/codegen/src/direct_schema.rs`: add `MapU32EntriesVec` as a
  typed root shape for numeric-string JSON object keys.
- `skinny/crates/codegen/src/json_typed_direct.rs`: emit a generated
  `parse_string_u32()` helper and map-entry constructor loop.
- `skinny/xtask/src/real_typed_schema.rs`: add
  `parse_gsoc_2018 -> Vec<GsocProposalEntry<'i>>` with full proposal,
  sponsor, and author product coverage.
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`: transient fixture,
  dispatch, serde/sonic sidecar visitor, checksum, and full-fixture parity
  wiring.
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`: regenerated only via
  `cargo xtask regen-real-typed`.

The candidate preserved proposal identity by retaining the numeric root key and
checksummed every nested proposal, sponsor, and author field.

## Correctness

- `cargo xtask regen-real-typed`
- `cargo xtask check-real-typed`
- `cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p codegen emits_typed_direct_u32_keyed_map_entries -- --nocapture`
- `cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p bbnf-bench gsoc_2018_typed -- --nocapture`

The focused generated route tests passed before measurement. The transient
source patch was then reverted because the row did not admit.

## Cold Profile

Build from `skinny/`:

```sh
RUSTC_WRAPPER= RUSTFLAGS="-C target-cpu=native" cargo build --release -p bbnf-bench --bin profile_direct
```

Run from `skinny/`:

```sh
target/release/profile_direct 400 gsoc-2018 real_typed_track1 0
target/release/profile_direct 400 gsoc-2018 real_typed_track2 0
target/release/profile_direct 400 gsoc-2018 real_typed_sonic 0
target/release/profile_direct 400 gsoc-2018 real_typed_serde 0
```

Evidence:

- `restart/skinny/tranches/sk-v14/research/skv14-W9AC-gsoc-2018-typed.tsv`
- `restart/skinny/tranches/sk-v14/research/skv14-W9AC-gsoc-2018-typed.raw.log`

| mode | Mbps | threshold role |
|---|---:|---|
| real_typed_track1 | 5711.366 | candidate |
| real_typed_track2 | 5631.957 | independent sidecar |
| real_typed_sonic | 6017.313 | strict comparator |
| real_typed_serde | 5639.270 | reference sidecar |

Admission threshold: Track 1 must exceed `sonic + 1.0` Mbps. The threshold is
6018.313 Mbps; generated Track 1 reached 5711.366 Mbps, a -306.947 Mbps
margin. Verdict: REJECT.

## Ledger Impact

- JSON real_typed_struct remains 13 / 17 ADMITTED and 4 MISSING.
- `gsoc-2018` remains in the missing typed product queue.
- REDRESS-229 records this generated-route rejection.
- Source remains unchanged except for the retained rejection artefacts.
