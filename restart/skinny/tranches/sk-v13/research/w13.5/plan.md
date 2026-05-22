# SK-V13 W13.5 Plan - GSOC Typed Product Surface

Wave: W13.5 typed product surface completion.
Selected row: `json/gsoc-2018/real_typed_struct/main`.
Risk class: medium; generated map-entry output plus gate/status surface.

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
- W13.5 report under `restart/skinny/tranches/sk-v13/research/w13.5/`

Any other source path returns REVISE before editing.

## Intervention

Add a generated real-typed parser for the `gsoc-2018` fixture:

```text
Vec<GsocProposalEntry<'i>> {
  key: Cow<'i, str>,
  value: GsocProposal<'i>,
}
```

Product types:

- `GsocProposal<'i>` with `@context`, `@type`, `name`, `description`,
  `sponsor`, and `author`.
- `GsocSponsor<'i>` with `@type`, `name`, `disambiguatingDescription`,
  `description`, `url`, and `logo`.
- `GsocAuthor<'i>` with `@type` and `name`.

Implementation steps:

1. Add the GSOC product structs and map-entry type to the typed fixture model.
2. Extend the real-typed schema with
   `DirectRootSchema::typed_root("parse_gsoc_2018", "Vec<crate::real_typed_struct::GsocProposalEntry<'i>>", map_entries(..., 1264, ty("GsocProposal")))`.
3. Regenerate `generated_real_typed.rs` via `cargo xtask regen-real-typed`
   and verify `cargo xtask check-real-typed`.
4. Add `RealTypedFixture::Gsoc2018`, `RealTypedOutput::Gsoc2018`, and
   Track 1 / Track 2 / serde / sonic routing.
5. Add synthetic and full-fixture parity tests proving generated Track 1,
   independent serde Track 2/oracle, serde sidecar, and sonic sidecar agree.
6. Extend the W13 typed-product companion gate for
   `G-W13.5-TYPED-GSOC-2018`.
7. Refresh `RESULTS.md`, `ROLLING-SOTA-DELTA.md`, and REDRESS only after
   same-run native measurement.

No new directive, BIR variant, `BackendShape`, public substrate API, sidecar
substrate, direct digest substitute, hand typed sink, or parser-owned fact
slot is introduced.

## Falsifiability Gate

`G-W13.5-TYPED-GSOC-2018` admits only if all are true:

- `track1_real_typed_struct`, `track2_real_typed_struct`,
  `serde_json_real_typed_struct`, and `sonic_rs_real_typed_struct` parse the
  full `gsoc-2018` fixture and produce the same checksum.
- Track 1 exceeds same-run sonic strict typed throughput by at least 1 Mbps.
- The row is recorded as `A / GO`, `strict`, `measured-row`,
  `gate_json_typed_contract`, and `independent_verified`.
- Existing admitted typed rows retain their maintain status.
- The W13.5 companion report is consumed by `gate-json` in the same wave.

A threshold miss is a measured reject, not a docs-only close.

## Same-Wave Consumer

The consumer is the `bbnf-bench` `real_typed_struct` workload for
`gsoc-2018`. The row is not admitted unless the generated map-entry parser is
called by Track 1 and the independent Track 2/oracle harness is measured in
the same Criterion capture.

## Pre-Blocked Routes

W13 typed product inherits the Section 20 row:

`70-72 and 103-110 are MIXED: typed product precedent is allowed, but direct
digest rows, hidden typed sinks, proof-only escape routes, and no-op production
rows are not typed admission.`

Material differential: W13.5 adds an actual generated typed product map-entry
root for `gsoc-2018`, with modeled nested sponsor and author objects. It does
not count a root key collector, direct digest row, omitted nested object, or
schema-only fixture as typed proof.

## Measurement Commands

- `cargo xtask check-real-typed`
- `cargo test -p bbnf-bench gsoc_2018_typed -- --nocapture`
- `cargo test -p bbnf-bench --bin gate w13_gsoc -- --nocapture`
- `cargo test -p bbnf-bench lock14_baseline::tests::admits_sk_v13_w13_5_parent_diff_under_w13_5_scope -- --nocapture`
- `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/gsoc-2018/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)'`
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-typed-product-report ../restart/skinny/tranches/sk-v13/research/w13.5/skv13-W13.5-typed-product.json`

## Revert Protocol

On FAIL, revert the W13.5 typed root, generated parser, fixture routing,
checksum/tests, gate/report additions, RESULTS/rolling updates, and REDRESS
entry. Save the rejected diff under `/tmp/skv13-waveW13.5-rejected.patch` and
record per-row Track 1 / Track 2 / sonic evidence in REDRESS.
