# SK-V13 W13.9 Plan - Canada Typed Product Surface

Date: 2026-05-22.
Wave: W13.9.
Gate: `G-W13.9-TYPED-CANADA`.

## Intervention

Add a generated typed product parser for `json/canada/real_typed_struct/main`.
The implementation introduces:

- `RealTypedFixture::Canada`.
- `CanadaDocument<'i>`, `CanadaFeature<'i>`, `CanadaProperties<'i>`, and
  `CanadaGeometry<'i>`.
- A `parse_canada` generated root in `generated_real_typed`.
- Track 1, Track 2, serde_json, and sonic-rs strict sidecar routing through
  the existing real typed product harness.
- A checksum that folds top-level type, feature count, every feature type,
  property name, geometry type, ring count, point count, and every coordinate
  f64 bit pattern in source order.

## Acceptance

The wave admits only if:

- `cargo xtask regen-real-typed` and `cargo xtask check-real-typed` pass.
- The focused Canada typed tests pass on both a small fixture and the full
  corpus.
- `gate-json` companion tests accept only strict sonic+1 movement.
- Lock 14 owner-path tests admit the W13.9 parent diff and continue to reject
  out-of-scope generic crate changes.
- Native Criterion shows
  `json/canada/track1_real_typed_struct` greater than
  `json/canada/sonic_rs_real_typed_struct + 1 Mbps`.

## Revert Protocol

If parity fails, revert the source patch and record a correctness reject. If
parity passes but throughput misses the pinned threshold, save the rejected
patch at `/tmp/skv13-waveW13.9-rejected.patch`, revert source edits, record
the measured Mbps in `research/w13.9/redress.md`, and append REDRESS without
updating `RESULTS.md` or `ROLLING-SOTA-DELTA.md`.

## Measurement

```text
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/canada/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)'
```
