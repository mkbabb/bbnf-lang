# SK-V13 W13.6 Plan - Unicode Mixed Typed Product Surface

Wave: W13.6 typed product surface completion.
Selected row: `json/unicode_mixed/real_typed_struct/main`.
Risk class: medium-high; generated output plus escape-heavy string throughput.

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
- W13.6 report under `restart/skinny/tranches/sk-v13/research/w13.6/`

Any other source path returns REVISE before editing.

## Intervention

Add a generated real-typed parser for the `unicode_mixed` fixture:

```text
UnicodeMixed<'i> {
  metadata: Option<UnicodeMixedMetadata<'i>>,
  records: Vec<UnicodeMixedRecord<'i>>,
}

UnicodeMixedMetadata<'i> {
  purpose: Option<Cow<'i, str>>,
  classes: Vec<Cow<'i, str>>,
  count: Option<u64>,
}

UnicodeMixedRecord<'i> {
  id: Option<u64>,
  record_type: Option<Cow<'i, str>>,
  value: Option<Cow<'i, str>>,
  n: Option<u64>,
}
```

Implementation steps:

1. Add the product structs, fixture enum/output variants, checksum, sidecar
   routing, and synthetic plus full-fixture parity tests.
2. Extend the real-typed schema with
   `DirectRootSchema::struct_root("parse_unicode_mixed", "crate::real_typed_struct::UnicodeMixed<'i>", "UnicodeMixed")`
   and capacity hints of 4,185 records and 5 metadata classes.
3. Regenerate `generated_real_typed.rs` via `cargo xtask regen-real-typed`
   and verify `cargo xtask check-real-typed`.
4. Extend the W13 typed-product companion gate for
   `G-W13.6-TYPED-UNICODE-MIXED`.
5. Refresh `RESULTS.md`, `ROLLING-SOTA-DELTA.md`, and REDRESS only after
   same-run native measurement.

No new directive, BIR variant, `BackendShape`, public substrate API, parser
sidecar fact slot, direct digest substitute, or hidden typed sink is allowed.

## Falsifiability Gate

`G-W13.6-TYPED-UNICODE-MIXED` admits only if all are true:

- `track1_real_typed_struct`, `track2_real_typed_struct`,
  `serde_json_real_typed_struct`, and `sonic_rs_real_typed_struct` parse the
  full `unicode_mixed` fixture and produce the same checksum.
- Track 1 exceeds same-run sonic strict typed throughput by at least 1 Mbps.
- The row is recorded as `A / GO`, `strict`, `measured-row`,
  `gate_json_typed_contract`, and `independent_verified`.
- Existing admitted typed rows retain their status.
- The W13.6 companion report is consumed by `gate-json` in the same wave.

A threshold miss is a measured reject, not a docs-only close.

## Same-Wave Consumer

The consumer is the `bbnf-bench` `real_typed_struct` workload for
`unicode_mixed`. The generated parser is admissible only when it is called by
Track 1 and measured in the same Criterion capture as Track 2, serde, and
sonic strict comparators.

## Pre-Blocked Routes

W13 typed product inherits the Section 20 row:

`70-72 and 103-110 are MIXED: typed product precedent is allowed, but direct
digest rows, hidden typed sinks, proof-only escape routes, and no-op production
rows are not typed admission.`

Material differential: W13.6 creates an actual generated typed product root
for `unicode_mixed` instead of counting the existing parse/direct rows,
unicode codec proofs, or a partial string-only surface. The row admits only by
same-run sonic+1 measurement with independent serde Track 2 parity.

## Measurement Commands

- `cargo xtask check-real-typed`
- `cargo test -p bbnf-bench unicode_mixed_typed -- --nocapture`
- `cargo test -p bbnf-bench --bin gate w13_unicode_mixed -- --nocapture`
- `cargo test -p bbnf-bench lock14_baseline::tests::admits_sk_v13_w13_6_parent_diff_under_w13_6_scope -- --nocapture`
- `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/unicode_mixed/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)'`
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-typed-product-report ../restart/skinny/tranches/sk-v13/research/w13.6/skv13-W13.6-typed-product.json`

## Revert Protocol

On FAIL, revert the W13.6 typed root, generated parser, fixture routing,
checksum/tests, gate/report additions, RESULTS/rolling updates, and REDRESS
entry. Save the rejected diff under `/tmp/skv13-waveW13.6-rejected.patch` and
record per-row Track 1 / Track 2 / sonic evidence in REDRESS.
