# SK-V13 W13.3 Plan - Random Typed Product Surface

Wave: W13.3 typed product surface completion.
Selected row: `json/random/real_typed_struct/main`.
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
- W13.3 report under `restart/skinny/tranches/sk-v13/research/w13.3/`

Any other source path returns REVISE before editing.

## Intervention

Add a generated real-typed parser for the `random` fixture:

```text
RandomDocument<'i> {
  id: Option<u64>,
  jsonrpc: Option<Cow<'i, str>>,
  total: Option<u64>,
  result: Vec<RandomUser<'i>>,
}

RandomUser<'i> {
  id: Option<u64>,
  avatar: Option<Cow<'i, str>>,
  age: Option<u64>,
  admin: Option<bool>,
  name: Option<Cow<'i, str>>,
  company: Option<Cow<'i, str>>,
  phone: Option<Cow<'i, str>>,
  email: Option<Cow<'i, str>>,
  birth_date: Option<Cow<'i, str>>,
  friends: Vec<RandomFriend<'i>>,
  field: Option<Cow<'i, str>>,
}

RandomFriend<'i> {
  id: Option<u64>,
  name: Option<Cow<'i, str>>,
  phone: Option<Cow<'i, str>>,
}
```

Implementation steps:

1. Add the random product structs to the typed fixture model and checksum.
2. Extend the real-typed schema with
   `DirectRootSchema::struct_root("parse_random", "crate::real_typed_struct::RandomDocument<'i>", "RandomDocument")`
   and capacity hints of 1,000 users and 3 friends.
3. Regenerate `generated_real_typed.rs` via `cargo xtask regen-real-typed`
   and verify `cargo xtask check-real-typed`.
4. Add `RealTypedFixture::Random`, `RealTypedOutput::Random`, and Track 1 /
   Track 2 / serde / sonic routing.
5. Add synthetic and full-fixture parity tests proving generated Track 1,
   independent Track 2 (`serde_json`), serde sidecar, and sonic sidecar agree.
6. Extend the W13 typed-product companion gate for `G-W13.3-TYPED-RANDOM`.
7. Refresh `RESULTS.md`, `ROLLING-SOTA-DELTA.md`, and REDRESS only after
   same-run native measurement.

No new directive, BIR variant, `BackendShape`, public substrate API, sidecar
substrate, direct digest substitute, hand typed sink, or parser-owned fact
slot is introduced.

## Falsifiability Gate

`G-W13.3-TYPED-RANDOM` admits only if all are true:

- `track1_real_typed_struct`, `track2_real_typed_struct`,
  `serde_json_real_typed_struct`, and `sonic_rs_real_typed_struct` parse the
  full `random` fixture and produce the same checksum.
- Track 1 exceeds same-run sonic strict typed throughput by at least 1 Mbps.
- The row is recorded as `A / GO`, `strict`, `measured-row`,
  `gate_json_typed_contract`, and `independent_verified`.
- Existing admitted typed rows retain their maintain floors.
- The W13.3 companion report is consumed by `gate-json` in the same wave.

A threshold miss is a measured reject, not a docs-only close.

## Same-Wave Consumer

The consumer is the `bbnf-bench` `real_typed_struct` workload for `random`.
The row is not admitted unless the generated parser is called by Track 1 and
the independent Track 2/oracle harness is measured in the same Criterion
capture.

## Pre-Blocked Routes

W13 typed product inherits the Section 20 row:

`70-72 and 103-110 are MIXED: typed product precedent is allowed, but direct
digest rows, hidden typed sinks, proof-only escape routes, and no-op production
rows are not typed admission.`

Material differential: this wave creates a generated nested typed product
parser from the existing schema surface and forces sonic/serde sidecar equality
plus gate consumption before RESULTS movement. It does not count the existing
`random/direct_to_struct` row or a partial root that skips the `result`
payload as typed proof.

## Measurement Commands

- `cargo xtask check-real-typed`
- `cargo test -p bbnf-bench random_typed -- --nocapture`
- `cargo test -p bbnf-bench --bin gate w13_random -- --nocapture`
- `cargo test -p bbnf-bench lock14_baseline::tests::admits_sk_v13_w13_3_parent_diff_under_w13_3_scope -- --nocapture`
- `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/random/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)'`
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-typed-product-report ../restart/skinny/tranches/sk-v13/research/w13.3/skv13-W13.3-typed-product.json`

## Revert Protocol

On FAIL, revert the W13.3 typed root, generated parser, fixture routing,
checksum/tests, gate/report additions, RESULTS/rolling updates, and REDRESS
entry. Save the rejected diff under `/tmp/skv13-waveW13.3-rejected.patch` and
record per-row Track 1 / Track 2 / sonic evidence in REDRESS.
