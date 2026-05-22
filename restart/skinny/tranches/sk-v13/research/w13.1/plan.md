# SK-V13 W13.1 Plan - Numbers Typed Product Surface

Wave: W13.1 typed product surface completion.
Selected row: `json/numbers/real_typed_struct/main`.
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
- W13.1 report under `restart/skinny/tranches/sk-v13/research/w13.1/`

Any other source path returns REVISE before editing.

## Intervention

Add a generated real-typed parser for the `numbers` fixture as a top-level
`Vec<f64>` product:

1. Extend the real-typed schema with
   `DirectRootSchema::typed_root("parse_numbers", "Vec<f64>", vec_with_capacity(f64_ty(), 10_001))`.
2. Regenerate `generated_real_typed.rs` via `cargo xtask regen-real-typed`
   and verify `cargo xtask check-real-typed`.
3. Add `RealTypedFixture::Numbers`, `RealTypedOutput::Numbers(Vec<f64>)`,
   routing for Track 1 / Track 2 / serde / sonic, and checksum parity over
   the `f64::to_bits()` stream.
4. Add synthetic and full-fixture parity tests proving generated Track 1,
   independent Track 2 (`serde_json`), serde sidecar, and sonic sidecar agree.
5. Add a gate-consumed W13.1 typed-product report and companion
   `gate-json` passthrough so the row cannot be an emit-only status change.
6. Refresh `RESULTS.md`, `ROLLING-SOTA-DELTA.md`, and REDRESS only after
   same-run native measurement.

No new directive, BIR variant, `BackendShape`, public substrate API, sidecar
substrate, direct digest substitute, hand typed sink, or parser-owned fact
slot is introduced.

## Falsifiability Gate

`G-W13.1-TYPED-NUMBERS` admits only if all are true:

- `track1_real_typed_struct`, `track2_real_typed_struct`,
  `serde_json_real_typed_struct`, and `sonic_rs_real_typed_struct` parse the
  full `numbers` fixture and produce the same checksum.
- Track 1 exceeds same-run sonic strict typed throughput by at least 1 Mbps.
- The row is recorded as `A / GO`, `strict`, `measured-row`,
  `gate_json_typed_contract`, and `independent_verified`.
- Existing admitted typed rows retain their maintain floors.
- The W13.1 companion report is consumed by `gate-json` in the same wave.

A threshold miss is a measured reject, not a docs-only close.

## Same-Wave Consumer

The consumer is the `bbnf-bench` `real_typed_struct` workload for `numbers`.
The row is not admitted unless the generated parser is called by Track 1 and
the independent Track 2/oracle harness is measured in the same Criterion
capture.

## Pre-Blocked Routes

W13 typed product inherits the Section 20 row:

`70-72 and 103-110 are MIXED: typed product precedent is allowed, but direct
digest rows, hidden typed sinks, proof-only escape routes, and no-op production
rows are not typed admission.`

Material differential: this wave does not reuse direct digest, proof-only root,
or a hidden hand sink. It creates a generated product parser from the existing
typed schema surface and forces sonic/serde sidecar equality plus gate
consumption before RESULTS movement.

## Measurement Commands

- `cargo xtask check-real-typed`
- `cargo test -p bbnf-bench real_typed_struct::tests::generated_numbers_typed_parser_matches_sidecars -- --nocapture`
- `cargo test -p bbnf-bench real_typed_struct::tests::w13_full_numbers_typed_fixture_matches_sidecars -- --nocapture`
- `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/numbers/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)'`
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-typed-product-report restart/skinny/tranches/sk-v13/research/w13.1/skv13-W13.1-typed-product.json`

## Revert Protocol

On FAIL, revert the W13.1 typed root, generated parser, fixture routing,
checksum/tests, gate/report additions, RESULTS/rolling updates, and REDRESS
admit entry. Save the rejected diff under `/tmp/skv13-waveW13.1-rejected.patch`
and record per-row Track 1 / Track 2 / sonic evidence in REDRESS.
