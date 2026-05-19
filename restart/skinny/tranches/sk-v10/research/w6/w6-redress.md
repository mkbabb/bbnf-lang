# SK-V10 W6 Redress - Root Typed Row Admission

Pass: Wave Redress.
Cycle: W6.
Date: 2026-05-19.
Gate: `G-W6-ROOT-TYPED-ROW`.
Disposition: PASS.

## Patch

W6 admits `github_events/real_typed_struct` as the first root-unblocked typed
row after the W5 root proof:

- `real_typed_struct.rs` adds the `GithubEvent` product graph, generated
  Track 1 dispatch, serde_json and sonic typed sidecars, checksum parity, and
  full-fixture tests.
- `real_typed_schema.rs` registers `parse_github_events` as a
  `Vec<crate::real_typed_struct::GithubEvent<'i>>` root through the W5
  `DirectRootSchema::typed_root` model.
- `generated_real_typed.rs` is regenerated with the github_events parser.
- `gate.rs` and `report.rs` add the W6 typed-row admission predicate and
  optional W6 result-row validation while preserving all W0 baseline rows.
- `lock14_baseline.rs` adds an exact SK-V10 W6 frozen-root allowance for the
  generated typed owner paths only.
- `xtask/src/main.rs` teaches the cost-facts RESULTS snapshot consumer to
  accept the 40-row opening surface plus the single W6 github_events typed row.

## Measurement

Criterion root:

```text
/Users/mkbabb/Programming/bbnf-lang/skinny/target/skv10-w6/criterion
```

Measured W6 row:

| Row | Track 1 Mbps | Track 2 Mbps | sonic-rs typed Mbps | serde_json typed Mbps | Floor | Result |
|---|---:|---:|---:|---:|---:|---|
| `github_events/real_typed_struct` | 12827 | 12645 | 12695 | 12592 | 11541 | PASS |

The floor is `ceil(12695 / 1.10) = 11541`; both generated Track 1 and the
independent Track 2/oracle clear it. `RESULTS.md` renders the row as
`A / GO`, `strict`, `measured-row`, `wave_id=SK-V10-W6`,
`redress_entry=REDRESS-105`, and
`same_wave_consumer_class=gate_json_typed_contract`.

Existing typed maintain rows remained above their Section 0.2 floors in the
same rendered report: `twitter` 18777, `citm_catalog` 36655,
`apache_builds` 8532, `update_center` 12113, `mesh` 9827, and `marine_ik`
12262 Mbps.

## Evidence

Pre-measurement source evidence:

```text
cargo xtask regen-real-typed
cargo xtask check-real-typed
cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench w6_ -- --nocapture
cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench generated_github_events_typed_parser_matches_sidecars -- --nocapture
cargo test --manifest-path skinny/Cargo.toml -p xtask w6_costfacts_snapshot_accepts_single_github_events_typed_row -- --nocapture
```

Measurement evidence:

```text
CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-lang/skinny/target/skv10-w6 \
CRITERION_HOME=/Users/mkbabb/Programming/bbnf-lang/skinny/target/skv10-w6/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo bench --manifest-path skinny/Cargo.toml -p bbnf-bench --bench json_parity

CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-lang/skinny/target/skv10-w6 \
CRITERION_HOME=/Users/mkbabb/Programming/bbnf-lang/skinny/target/skv10-w6/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo bench --manifest-path skinny/Cargo.toml -p bbnf-bench --bench simd_scan
```

Gate evidence:

```text
CRITERION_HOME=/Users/mkbabb/Programming/bbnf-lang/skinny/target/skv10-w6/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo xtask gate-json --update-results

CRITERION_HOME=/Users/mkbabb/Programming/bbnf-lang/skinny/target/skv10-w6/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo xtask gate-json --advisory --check-results

CRITERION_HOME=/Users/mkbabb/Programming/bbnf-lang/skinny/target/skv10-w6/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo xtask gate-json --with-cost-facts --check-results
```

The first command rewrote `skinny/RESULTS.md` and then exited 5 because the
global report remains `N-direct / NoGo`. The row-level gate was consumed by
the second advisory check, which passed. The cost-facts check also passed after
the same-wave snapshot consumer accepted the one W6 row. This matches the
existing row-admit discipline for a partially open skinny bracket.

## Gate Accounting

- One row moved: `github_events/real_typed_struct`.
- No parse-only or direct row moved.
- No W3/union substrate route was reopened.
- No generic JSON policy moved into the core codegen/runtime surface.
- `github_events/direct_to_struct` remains `N-direct / NO-GO`; typed admission
  does not transfer to the digest plane.
- `gsoc-2018/real_typed_struct` remains routed for a later wave only if the
  SPEC is amended or a new root-typed row slice is dispatched.
