# SK-V13 W13.7 Plan - Y String Unicode Typed Product Surface

Wave: W13.7 typed product surface completion.
Selected row: `json/y_string_unicode/real_typed_struct/main`.
Risk class: medium; small corpus plus escape-heavy string decoding.

## Owner Paths

- `skinny/xtask/src/real_typed_schema.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/xtask/src/main.rs`
- `skinny/RESULTS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `skinny/REDRESS.md`
- W13.7 report under `restart/skinny/tranches/sk-v13/research/w13.7/`

Any other source path returns REVISE before editing.

## Intervention

Add a generated real-typed parser for the `y_string_unicode` fixture:

```text
Vec<Cow<'i, str>>
```

Implementation steps:

1. Add `RealTypedFixture::YStringUnicode` and
   `RealTypedOutput::YStringUnicode(Vec<Cow<'i, str>>)`.
2. Extend the real-typed schema with
   `DirectRootSchema::typed_root("parse_y_string_unicode", "Vec<Cow<'i, str>>", vec_with_capacity(string(), 2_200))`.
3. Regenerate `generated_real_typed.rs` and verify `cargo xtask check-real-typed`.
4. Add Track 1 / Track 2 / serde / sonic routing, checksum, and synthetic plus
   full-fixture parity tests.
5. Extend the W13 typed-product companion gate for
   `G-W13.7-TYPED-Y-STRING-UNICODE`.
6. Refresh `RESULTS.md`, `ROLLING-SOTA-DELTA.md`, and REDRESS only after
   same-run native measurement.

No new directive, BIR variant, `BackendShape`, public substrate API, direct
digest substitute, hidden typed sink, or unicode codec proof-only close is
allowed.

## Falsifiability Gate

`G-W13.7-TYPED-Y-STRING-UNICODE` admits only if all are true:

- `track1_real_typed_struct`, `track2_real_typed_struct`,
  `serde_json_real_typed_struct`, and `sonic_rs_real_typed_struct` parse the
  full `y_string_unicode` fixture and produce the same checksum.
- Track 1 exceeds same-run sonic strict typed throughput by at least 1 Mbps.
- The row is recorded as `A / GO`, `strict`, `measured-row`,
  `gate_json_typed_contract`, and `independent_verified`.
- Existing admitted typed rows retain their status.
- The W13.7 companion report is consumed by `gate-json` in the same wave.

A threshold miss is a measured reject.

## Same-Wave Consumer

The consumer is the `bbnf-bench` `real_typed_struct` workload for
`y_string_unicode`. The generated parser is admissible only when the Track 1
consumer, independent Track 2, serde, and sonic strict lanes are all captured
in the same native Criterion run.

## Pre-Blocked Routes

W13 typed product inherits the Section 20 row:

`70-72 and 103-110 are MIXED: typed product precedent is allowed, but direct
digest rows, hidden typed sinks, proof-only escape routes, and no-op production
rows are not typed admission.`

Material differential: W13.7 creates a generated root string-vector typed
product for `y_string_unicode` instead of counting parse/direct rows, unicode
codec proofs, or a sidecar checksum. The row admits only by same-run sonic+1
measurement with independent serde Track 2 parity.

## Measurement Commands

- `cargo xtask check-real-typed`
- `cargo test -p bbnf-bench y_string_unicode_typed -- --nocapture`
- `cargo test -p bbnf-bench --bin gate w13_y_string_unicode -- --nocapture`
- `cargo test -p bbnf-bench lock14_baseline::tests::admits_sk_v13_w13_7_parent_diff_under_w13_7_scope -- --nocapture`
- `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/y_string_unicode/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)'`
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-typed-product-report ../restart/skinny/tranches/sk-v13/research/w13.7/skv13-W13.7-typed-product.json`

## Revert Protocol

On FAIL, revert the W13.7 typed root, generated parser, fixture routing,
checksum/tests, gate/report additions, RESULTS/rolling updates, and REDRESS
entry. Save the rejected diff under `/tmp/skv13-waveW13.7-rejected.patch` and
record per-row Track 1 / Track 2 / sonic evidence in REDRESS.
