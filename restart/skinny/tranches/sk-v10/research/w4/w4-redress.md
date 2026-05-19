# SK-V10 W4 Redress - `instruments` Typed Product Admission

Pass: Wave Redress.
Cycle: W4.
Date: 2026-05-19.
Gate: `G-W4-INSTRUMENTS-TYPED`.
Disposition: REJECT.

## Patch

W4 implemented the planned `instruments/real_typed_struct` typed product row:

- `RealTypedFixture::Instruments` and typed product structs for the full
  fixture.
- A generated `parse_instruments` typed root in `generated_real_typed.rs`.
- Track 1, independent Track 2/oracle, serde_json typed, sonic-rs typed, and
  checksum paths.
- Same-wave `gate-json` and report validation for the W4 typed row contract.
- Lock 14 authorization for the exact W4 typed-schema owner paths.

The source slice was rejected and reverted because the independent Track 2
oracle missed the W4 floor. The rejected patch is preserved at
`/tmp/skv10-waveW4-rejected.patch`.

## Measurement

The decisive typed capture used one coherent Criterion root:

```text
CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-lang/skinny/target/skv10-w4 \
CRITERION_HOME=/Users/mkbabb/Programming/bbnf-lang/skinny/target/skv10-w4/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo bench --bench json_parity -- 'json/instruments/.*real_typed_struct'
```

Measured typed product row:

| Bench | Mbps |
|---|---:|
| generated Track 1 `real_typed_struct` | 20678 |
| independent Track 2 `real_typed_struct` | 12127 |
| sonic-rs typed strict | 15940 |
| serde_json typed | 12119 |

W4 floor:

```text
ceil(same-run sonic_typed / 1.10) = 14491 Mbps
```

Generated Track 1 passed the floor. Independent Track 2 missed:

```text
12127 < 14491
```

## Gate Evidence

Functional evidence passed before measurement:

```text
cargo xtask check-real-typed
cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench instruments -- --nocapture
cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench w4_ -- --nocapture
cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench report::tests -- --nocapture
cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench --bin gate -- --nocapture
```

The full W4 gate then failed as required by CH6:

```text
CRITERION_HOME=/Users/mkbabb/Programming/bbnf-lang/skinny/target/skv10-w4/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo xtask gate-json --update-results --advisory
```

Result:

```text
Schema/W0 validation failure:
json/instruments/real_typed_struct/main W4 typed contract admits only
A / GO, saw N-direct / NO-GO.
```

## Gate Accounting

- No `RESULTS.md` row moved.
- `parse_only` remains 17 `S / NO-GO`.
- `direct_to_struct` remains 5 `A / GO` and 12 `N-direct / NO-GO`.
- `real_typed_struct` remains 6 `A / GO`.
- W5 may dispatch because the W5 entry gate accepts either W4 admission or W4
  rejection.
