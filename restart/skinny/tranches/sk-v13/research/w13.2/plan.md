# SK-V13 W13.2 Plan - Unicode Basic Typed Product Surface

Wave: W13.2 typed product surface completion.
Selected row: `json/unicode_basic/real_typed_struct/main`.
Risk class: medium; generated output plus gate/status surface.

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
- W13.2 report under `restart/skinny/tranches/sk-v13/research/w13.2/`

Any other source path returns REVISE before editing.

## Intervention

Add a generated real-typed parser for the `unicode_basic` fixture as a
top-level vector of records:

```text
UnicodeBasicRecord<'i> {
  id: Option<u64>,
  script: Option<Cow<'i, str>>,
  text: Option<Cow<'i, str>>,
  len: Option<u64>,
  tags: Vec<Cow<'i, str>>,
}
```

Implementation steps:

1. Add `UnicodeBasicRecord<'i>` to the typed fixture model and checksum.
2. Extend the real-typed schema with
   `DirectRootSchema::typed_root("parse_unicode_basic", "Vec<crate::real_typed_struct::UnicodeBasicRecord<'i>>", vec_with_capacity(ty("UnicodeBasicRecord"), 5_759))`
   plus the matching type schema.
3. Regenerate `generated_real_typed.rs` via `cargo xtask regen-real-typed`
   and verify `cargo xtask check-real-typed`.
4. Add `RealTypedFixture::UnicodeBasic`, `RealTypedOutput::UnicodeBasic`, and
   Track 1 / Track 2 / serde / sonic routing.
5. Add synthetic and full-fixture parity tests proving generated Track 1,
   independent Track 2 (`serde_json`), serde sidecar, and sonic sidecar agree.
6. Extend the W13 typed-product companion gate for `G-W13.2-TYPED-UNICODE-BASIC`.
7. Refresh `RESULTS.md`, `ROLLING-SOTA-DELTA.md`, and REDRESS only after
   same-run native measurement.

No new directive, BIR variant, `BackendShape`, public substrate API, sidecar
substrate, direct digest substitute, hand typed sink, or parser-owned fact
slot is introduced.

## Falsifiability Gate

`G-W13.2-TYPED-UNICODE-BASIC` admits only if all are true:

- `track1_real_typed_struct`, `track2_real_typed_struct`,
  `serde_json_real_typed_struct`, and `sonic_rs_real_typed_struct` parse the
  full `unicode_basic` fixture and produce the same checksum.
- Track 1 exceeds same-run sonic strict typed throughput by at least 1 Mbps.
- The row is recorded as `A / GO`, `strict`, `measured-row`,
  `gate_json_typed_contract`, and `independent_verified`.
- Existing admitted typed rows retain their maintain floors.
- The W13.2 companion report is consumed by `gate-json` in the same wave.

A threshold miss is a measured reject, not a docs-only close.

## Same-Wave Consumer

The consumer is the `bbnf-bench` `real_typed_struct` workload for
`unicode_basic`. The row is not admitted unless the generated parser is called
by Track 1 and the independent Track 2/oracle harness is measured in the same
Criterion capture.

## Pre-Blocked Routes

W13 typed product inherits the Section 20 row:

`70-72 and 103-110 are MIXED: typed product precedent is allowed, but direct
digest rows, hidden typed sinks, proof-only escape routes, and no-op production
rows are not typed admission.`

Material differential: this wave creates a generated borrowed-string product
parser from the existing typed schema surface and forces sonic/serde sidecar
equality plus gate consumption before RESULTS movement. It does not count the
existing `unicode_basic/direct_to_struct` row as typed proof.

## Measurement Commands

- `cargo xtask check-real-typed`
- `cargo test -p bbnf-bench unicode_basic_typed -- --nocapture`
- `cargo test -p bbnf-bench --bin gate w13_unicode_basic -- --nocapture`
- `cargo test -p bbnf-bench lock14_baseline::tests::admits_sk_v13_w13_2_parent_diff_under_w13_2_scope -- --nocapture`
- `cargo test -p xtask gate_json_passthrough_accepts_skv13_typed_product_report_flag -- --nocapture`
- `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/unicode_basic/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)'`
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-typed-product-report ../restart/skinny/tranches/sk-v13/research/w13.2/skv13-W13.2-typed-product.json`

## Revert Protocol

On FAIL, revert the W13.2 typed root, generated parser, fixture routing,
checksum/tests, gate/report additions, RESULTS/rolling updates, and REDRESS
entry. Save the rejected diff under `/tmp/skv13-waveW13.2-rejected.patch` and
record per-row Track 1 / Track 2 / sonic evidence in REDRESS.
