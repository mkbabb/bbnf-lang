# SK-V10 W10 Plan - Instruments Direct Residual Admission

Pass: Wave Plan.
Cycle: W10.
Date: 2026-05-19.
Gate: `G-W10-DIRECT-RESIDUAL`.
Disposition target: PASS if fresh direct measurement holds.

## Selected Intervention

Admit exactly one residual direct row:
`instruments/direct_to_struct`.

The direct-output/control mechanism is the W2 direct reclamation predicate
extended as a W10-limited residual admission. It remains gate/report behavior,
not parser/runtime behavior: the generated Track 1 direct caller is already
`bbnf_bench::direct_struct::track1_digest`, which invokes
`runtime::generated_json::parse_direct(input, &mut sink)`, and the independent
Track 2 oracle is `track2_digest`.

No other residual row is selected. `mesh`, `random`, and `canada` each miss at
least one floor in the current table and are not in the W10 target set.

## Owner Paths

- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v10/research/w10/w10-redress.md`

Read/measure:

- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`

No generated parser, parser runtime, SIMD, W3, scratch/materialization, or
generic crate edit is authorized.

## Implementation

- Add a W10 residual direct predicate for `instruments`.
- Require Track 1 and Track 2 Mbps to meet the Section 0.2 direct floor
  `11086`.
- Render `instruments/direct_to_struct` as `A / GO` only when the predicate
  passes.
- Render the same direct contract fields as W2, but with W10 provenance:
  `strict`, `measured-row`, `gate_json_direct_contract`, `REDRESS-109`,
  `SK-V10-W10`, and `direct-residual`.
- Leave every other residual direct row unchanged.
- Keep `Report::validate_sk_v8_w0` as the same-wave consumer: a moved direct
  row must still pass the executable W1 floor/strictness/comparator contract.

## Falsifiability Gate

`G-W10-DIRECT-RESIDUAL` passes only if:

- `instruments` Track 1 and Track 2 meet `>= 11086` Mbps under the same-run
  direct comparator plane;
- direct guard floors hold for `citm_catalog`, `marine_ik`, and
  `unicode_basic`;
- report validation and `gate-json --check-results` consume the row movement;
- no parse-only, typed, W3, helper-transfer, scalar-parent, sidecar, or scratch
  route is reopened.

If fresh `instruments` measurement misses either floor, W10 rejects and leaves
`RESULTS.md` unchanged.

## Evidence Commands

```text
CARGO_TARGET_DIR=/tmp/skv10-w10-target \
CRITERION_HOME=/tmp/skv10-w10-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo bench --manifest-path skinny/Cargo.toml -p bbnf-bench \
  --bench json_parity -- \
  'json/(instruments|citm_catalog|marine_ik|unicode_basic)/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)$'
```

```text
RUSTFLAGS="-C target-cpu=native" \
cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench \
  w10_direct -- --nocapture
```

```text
RUSTFLAGS="-C target-cpu=native" \
cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench \
  direct_contract -- --nocapture
```

```text
CRITERION_HOME=/tmp/skv10-w10-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo xtask gate-json --update-results --advisory
```

```text
CRITERION_HOME=/tmp/skv10-w10-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo xtask gate-json --with-cost-facts --check-results
```

## Revert Protocol

If any gate fails, revert `gate.rs`, `report.rs`, `RESULTS.md`, and the W10
REDRESS entry as one slice. Save the rejected source patch at
`/tmp/skv10-waveW10-rejected.patch`.

## Pre-Blocked Routes

- REDRESS 73 helper transfer.
- REDRESS 93 scalar-parent fold.
- REDRESS 50-55 sidecars.
- REDRESS 66-69 scratch/materialization/fact routes.
- W3 substrate, parse-only SOTA evidence, typed-row relabeling, and generic
  JSON policy leaks.
