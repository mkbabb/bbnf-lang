# SK-V10 W4 Research - `instruments` Typed Product Admission

Pass: Wave Research.
Cycle: W4.
Date: 2026-05-19.
Scope: read-only evaluation for `G-W4-INSTRUMENTS-TYPED`.

## Inputs

- W3 is closed under REDRESS 102; the parse-only firewall is active and W4 is
  the next dispatchable wave.
- SPEC Section 7 authorizes exactly one typed product row movement:
  `json/instruments/real_typed_struct/main`.
- Direct digest evidence for `instruments` is already present but remains
  direct-plane only. It cannot admit a typed row.
- The current frozen Criterion authority is
  `CRITERION_HOME=target/skv9-w1/criterion`, but that capture has no
  `instruments/real_typed_struct` rows because `fixture_for_name("instruments")`
  is not wired yet. W4 therefore needs a derived same-run Criterion root seeded
  from the frozen authority and refreshed for `json_instruments`.

## Fixture Shape

The canonical fixture is `skinny/test_data/instruments.json`.

| Field | Shape | Count / bound |
|---|---|---:|
| root | object | 9 keys |
| `instruments` | array of instrument objects | 63 |
| `patterns` | array of pattern objects | 240 |
| `samples` | array of sample objects | 70 |
| instrument envelopes | object with scalar bounds and `nodes` | max 8 nodes |
| pattern `data` | null or event array | max 1 event |

The fixture is a bounded named-object product. It fits the current typed
DirectBuild schema model without root-type generalization.

## Typed Schema Candidate

The W4 schema should add:

- `InstrumentsDocument`
- `Instrument`
- `InstrumentEnvelope`
- `InstrumentEnvelopeNode`
- `InstrumentPattern`
- `InstrumentPatternEvent`
- `InstrumentSample`

Track 1 path:

- `skinny/xtask/src/real_typed_schema.rs` adds the schema root
  `parse_instruments`.
- `cargo xtask regen-real-typed` regenerates
  `skinny/crates/bbnf-bench/src/generated_real_typed.rs`.
- `real_typed_struct::track1_typed` dispatches
  `RealTypedFixture::Instruments` to `generated_real_typed::parse_instruments`.

Track 2/oracle path:

- `real_typed_struct::track2_typed` uses the independent serde typed decode
  path, never the generated typed parser.
- `assert_real_typed_parity` compares Track 1, Track 2, serde_json typed, and
  sonic-rs typed checksums over the full fixture.

## Bench And Gate Surface

`skinny/crates/bbnf-bench/benches/json_parity.rs` already registers the four
typed benches for every fixture returned by `fixture_for_name`:

- `track1_real_typed_struct`
- `track2_real_typed_struct`
- `sonic_rs_real_typed_struct`
- `serde_json_real_typed_struct`

No benchmark-body change is required unless W4 discovers missing metadata.

`gate-json` currently emits real typed rows only for opening-baseline typed
fixtures. W4 must add an explicit instruments typed admission path so the new
row is rendered with:

- `strictness=strict`
- `parse_utf8=measured-row`
- `measured_validation_path=measured-row`
- `same_wave_consumer_class=gate_json_typed_contract`
- `redress_entry=REDRESS-103`
- `wave_id=SK-V10-W4`
- `sk_v9_open_delta=typed-row-added`

`Report::validate_sk_v8_w0` must keep all opening rows exact while accepting
only the single new W4 row when its generated Track 1 and independent Track 2
Mbps both meet `ceil(same-run sonic_typed / 1.10)`.

## Measurement Plan

Use a derived Criterion root:

```text
target/skv10-w4/criterion
```

Seed it from `target/skv9-w1/criterion`, then run the `json_instruments`
criterion group with `RUSTFLAGS="-C target-cpu=native"`. This preserves the
opening rows while adding same-run `instruments` typed metadata and estimates.

Required checks:

```text
cargo xtask check-real-typed
```

```text
cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench instruments -- --nocapture
```

```text
CRITERION_HOME=target/skv10-w4/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo bench --bench json_parity -- json/instruments
```

```text
CRITERION_HOME=target/skv10-w4/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo xtask gate-json --update-results --advisory
```

```text
CRITERION_HOME=target/skv10-w4/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo xtask gate-json --check-results
```

## Recommendation

Proceed to W4 plan. The admissible implementation is a typed product row, not a
direct digest relabel and not an Apache/CITM analogy. If measurement shows the
new row fails either Track 1 or Track 2 floor, record a measured W4 REDRESS
reject and keep the row out of `RESULTS.md`.
