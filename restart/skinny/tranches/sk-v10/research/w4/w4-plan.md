# SK-V10 W4 Plan - `instruments` Typed Product Admission

Pass: Wave Plan.
Cycle: W4.
Date: 2026-05-19.
Scope: measured typed row movement for `G-W4-INSTRUMENTS-TYPED`.

## Entry Gate

PASS.

- W3 closed under REDRESS 102 and removed the dead W3/W4 cascade-lock route.
- SPEC Section 7 authorizes only `json/instruments/real_typed_struct/main`.
- W4 research named the schema, generated typed output path, independent Track
  2/oracle, serde_json typed comparator, sonic-rs typed comparator, run-id
  source, and rollback boundary.

## Selected Intervention

Add `instruments` as a full typed product row using the existing typed
DirectBuild schema path.

Owner paths:

- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/xtask/src/real_typed_schema.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

No parser/runtime parse-only behavior, direct digest implementation,
generic-codegen root-shape work, or aarch64 kernel code is in scope.

## Implementation

- Add `RealTypedFixture::Instruments`.
- Add typed product structs for the bounded `instruments` fixture:
  `InstrumentsDocument`, `Instrument`, `InstrumentEnvelope`,
  `InstrumentEnvelopeNode`, `InstrumentPattern`, `InstrumentPatternEvent`, and
  `InstrumentSample`.
- Add a `parse_instruments` root to `real_typed_schema.rs` and regenerate
  `generated_real_typed.rs` with `cargo xtask regen-real-typed`.
- Extend `track1_typed`, `track2_typed`, `serde_typed`, `sonic_typed`, and
  `typed_checksum` to cover the new fixture.
- Keep the bench body unchanged unless metadata proves missing; the existing
  `json_parity` harness registers typed benches for every fixture returned by
  `fixture_for_name`.
- Add a W4 typed-row contract in `gate-json` and `Report::validate_sk_v8_w0`.
  The contract accepts only `json/instruments/real_typed_struct/main` when:
  - outcome is `A / GO`;
  - output plane is `typed direct`;
  - strictness and validation are `strict` / `measured-row`;
  - same-wave consumer is `gate_json_typed_contract`;
  - REDRESS is `REDRESS-103` and wave is `SK-V10-W4`;
  - Track 2 independence is `independent_verified`;
  - sonic-rs and serde_json typed comparator evidence is same-run native;
  - generated Track 1 and independent Track 2 both meet
    `ceil(same-run sonic_typed / 1.10)`.
- Preserve all opening rows and existing typed maintain floors from SPEC
  Section 0.2.

## CHALLENGE Requirement

Mandatory. W4 moves a typed product row and touches generated typed code plus
gate/report admission logic. Redress starts only after CHALLENGE accepts this
plan.

## Exit Gate

`G-W4-INSTRUMENTS-TYPED` from SPEC Section 7.

Required evidence:

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

## LOC Budget And Risk

Budget: 160-260 source/generated LOC plus 40-80 gate/report LOC.

Risk: MEDIUM. The DirectBuild schema is existing infrastructure, but the wave
adds a new row to the scoreboard and therefore must be executable in report
validation rather than a manual `RESULTS.md` edit.

## Revert Protocol

Revert typed structs, schema, generated code, gate/report changes, and
`RESULTS.md` as one slice. Preserve a rejected patch at
`/tmp/skv10-waveW4-rejected.patch` and record the checksum table plus measured
Track 1, Track 2, serde_json, and sonic-rs typed Mbps in REDRESS.

## Same-Wave Consumer

`json_parity` consumes the new `fixture_for_name("instruments")` typed fixture
while generating Criterion rows. `gate-json` consumes the new Criterion rows
when rendering `RESULTS.md`, and `Report::validate_sk_v8_w0` consumes the W4
typed contract in the same wave.

## Pre-Blocked Routes

- No direct digest evidence admits a typed product row.
- No Apache/CITM-style analogy admission.
- No Canada typed shortcut.
- No generated typed parser without full-fixture checksum parity against Track
  2/oracle, serde_json typed, and sonic-rs typed.
- No new telemetry field or outcome variant.
- No generic root-type work; that remains W5/W6.
