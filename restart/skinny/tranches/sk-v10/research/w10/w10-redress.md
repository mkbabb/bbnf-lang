# SK-V10 W10 Redress - Instruments Direct Residual Admission

Pass: Wave Redress.
Cycle: W10.
Date: 2026-05-19.
Gate: `G-W10-DIRECT-RESIDUAL`.
Disposition: PASS.
REDRESS: 109.

## Scope

W10 admits exactly one residual direct digest row:
`instruments/direct_to_struct`.

The patch extends the existing W2 direct reclamation gate with a W10-limited
residual predicate for `instruments` at the Section 0.2 fixed floor of
11086 Mbps. It does not edit the generated parser, parser runtime, SIMD,
generic crates, direct caller bodies, typed row code, or W3-adjacent substrate
paths.

## Implementation

- Added `w10_residual` to the direct-row decision path in
  `skinny/crates/bbnf-bench/src/bin/gate.rs`.
- Added `w10_direct_residual_floor("instruments") = 11086`.
- Allowed W10 to admit only a baseline `NO-GO` direct row whose classifier is
  absent or `N-direct` and whose generated Track 1 plus independent Track 2
  both clear the W10 floor. Hard correctness failures still block admission.
- Stamped the moved row with `strict`, `measured-row`,
  `gate_json_direct_contract`, `REDRESS-109`, `SK-V10-W10`, and
  `direct-residual`.
- Left every other residual direct row under the pre-existing classifier.

## Measurement

The first targeted W10 Criterion pass under `/tmp/skv10-w10-criterion`
measured:

| Corpus | Track 1 Mbps | Track 2 Mbps | sonic direct Mbps | serde direct Mbps |
|---|---:|---:|---:|---:|
| `instruments` | 12146 | 11288 | 12763 | 9567 |
| `citm_catalog` | 21549 | 20616 | 20055 | 13247 |
| `marine_ik` | 9342 | 9603 | 8552 | 7032 |
| `unicode_basic` | 9092 | 8383 | 8897 | 5862 |

That targeted root proved the selected row and guards, but `gate-json`
correctly rejected a mixed report-wide capture when it was combined with an
older full root. W10 therefore used a full coherent native Criterion capture
under `/tmp/skv10-w10-full-criterion`.

The full capture rendered `skinny/RESULTS.md` with run id
`sk-v9-open:criterion-fnv64-6f007527061ee26d`.

| Corpus | Workload | Track 1 Mbps | Track 2 Mbps | sonic direct Mbps | Floor | Result |
|---|---|---:|---:|---:|---:|---|
| `instruments` | `direct_to_struct` | 12040 | 11166 | 12674 | 11086 | PASS |

Direct guard floors held in the same rendered report:

| Corpus | Track 1 Mbps | Track 2 Mbps | Maintain floor | Result |
|---|---:|---:|---:|---|
| `citm_catalog` | 21595 | 20592 | 18145 | PASS |
| `marine_ik` | 9066 | 9025 | 7575 | PASS |
| `unicode_basic` | 9030 | 8360 | 7841 | PASS |

## Evidence Commands

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
rm -rf /tmp/skv10-w10-target /tmp/skv10-w10-criterion
CARGO_TARGET_DIR=/tmp/skv10-w10-target \
CRITERION_HOME=/tmp/skv10-w10-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo bench --manifest-path skinny/Cargo.toml -p bbnf-bench \
  --bench json_parity -- \
  'json/(instruments|citm_catalog|marine_ik|unicode_basic)/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)$'
```

```text
rm -rf /tmp/skv10-w10-full-criterion
CARGO_TARGET_DIR=/tmp/skv10-w10-target \
CRITERION_HOME=/tmp/skv10-w10-full-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo run -p xtask -- bench-json --advisory
```

```text
CRITERION_HOME=/tmp/skv10-w10-full-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo run -p xtask -- gate-json --with-cost-facts --check-results
```

All listed commands passed. `bench-json --advisory` rendered the W10
`RESULTS.md` snapshot while preserving the global `N-direct / NoGo` outcome.

## Result

`instruments/direct_to_struct` is now `A / GO` as strict measured-row direct
evidence under REDRESS 109. Overall SK-V10 remains `N-direct / NoGo` because
eleven direct residual rows remain `N-direct / NO-GO`.

Close is now dispatchable.
