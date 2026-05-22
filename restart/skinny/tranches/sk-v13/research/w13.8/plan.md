# SK-V13 W13.8 Plan - Unicode Escapes Typed Product Surface

Wave: W13.8 typed product surface completion.
Selected row: `json/unicode_escapes/real_typed_struct/main`.
Risk class: medium-high; full-fixture product is simple, but escaped-string
decode can dominate throughput.

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
- W13.8 report and redress artifacts under
  `restart/skinny/tranches/sk-v13/research/w13.8/`

Any other source path returns REVISE before editing.

## Intervention

Add a generated real-typed parser for the `unicode_escapes` fixture:

```text
UnicodeEscapesDocument<'input> {
  meta: Option<UnicodeEscapesMeta<'input>>,
  records: Vec<UnicodeEscapesRecord<'input>>
}
```

Implementation steps:

1. Add `UnicodeEscapesDocument`, `UnicodeEscapesMeta`, and
   `UnicodeEscapesRecord` typed product structs.
2. Add `RealTypedFixture::UnicodeEscapes` and
   `RealTypedOutput::UnicodeEscapes`.
3. Extend the real-typed schema with
   `DirectRootSchema::struct_root("parse_unicode_escapes", ..., "UnicodeEscapesDocument")`
   plus the three product types.
4. Regenerate `generated_real_typed.rs` and verify
   `cargo xtask check-real-typed`.
5. Add Track 1 / Track 2 / serde / sonic routing, checksum, and synthetic plus
   full-fixture parity tests.
6. Extend the W13 typed-product companion gate for
   `G-W13.8-TYPED-UNICODE-ESCAPES`.
7. Refresh `RESULTS.md`, `ROLLING-SOTA-DELTA.md`, and REDRESS only on
   measured admit. On throughput miss, revert source and retain only measured
   reject evidence.

No new directive, BIR variant, `BackendShape`, public substrate API, direct
digest substitute, hidden typed sink, or unicode codec proof-only close is
allowed.

## Falsifiability Gate

`G-W13.8-TYPED-UNICODE-ESCAPES` admits only if all are true:

- `track1_real_typed_struct`, `track2_real_typed_struct`,
  `serde_json_real_typed_struct`, and `sonic_rs_real_typed_struct` parse the
  full `unicode_escapes` fixture and produce the same checksum.
- Track 1 exceeds same-run sonic strict typed throughput by at least 1 Mbps.
- The row is recorded as `A / GO`, `strict`, `measured-row`,
  `gate_json_typed_contract`, and `independent_verified`.
- Existing admitted typed rows retain their status.
- The W13.8 companion report is consumed by `gate-json` in the same wave.

A threshold miss is a measured reject.

## Same-Wave Consumer

The consumer is the `bbnf-bench` `real_typed_struct` workload for
`unicode_escapes`. The generated parser is admissible only when the Track 1
consumer, independent Track 2, serde_json, and sonic strict lanes are all
captured in the same native Criterion run.

## Pre-Blocked Routes

W13 typed product inherits the Section 20 row:

`70-72 and 103-110 are MIXED: typed product precedent is allowed, but direct
digest rows, hidden typed sinks, proof-only escape routes, and no-op production
rows are not typed admission.`

Material differential: W13.8 creates a generated full-document typed product
for `unicode_escapes`; it does not claim admission from unicode codec
microbenchmarks, parse/direct rows, hidden checksum sinks, or fixture-only
proofs.

## Measurement Commands

- `cargo xtask regen-real-typed`
- `cargo xtask check-real-typed`
- `cargo test -p bbnf-bench unicode_escapes_typed -- --nocapture`
- `cargo test -p bbnf-bench --bin gate w13_unicode_escapes -- --nocapture`
- `cargo test -p bbnf-bench lock14_baseline::tests::admits_sk_v13_w13_8_parent_diff_under_w13_8_scope -- --nocapture`
- `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/unicode_escapes/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)'`
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-typed-product-report ../restart/skinny/tranches/sk-v13/research/w13.8/skv13-W13.8-typed-product.json`

## Revert Protocol

On FAIL, revert the W13.8 typed structs, generated parser, fixture routing,
checksum/tests, gate/report additions, RESULTS/rolling updates, and REDRESS
entry. Save the rejected diff under `/tmp/skv13-waveW13.8-rejected.patch` and
record per-row Track 1 / Track 2 / sonic evidence in REDRESS.
