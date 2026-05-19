# SK-V10 W2 Redress - Direct Row-Table Reclamation

Pass: Wave Redress.
Cycle: W2.
Date: 2026-05-19.
Gate: `G-W2-DIRECT-RECLAMATION`.
Disposition: PASS.

## Patch

W2 admitted two direct digest rows:

- `apache_builds/direct_to_struct`: `N-direct / NO-GO` -> `A / GO`.
- `numbers/direct_to_struct`: `N-direct / NO-GO` -> `A / GO`.

The wave changed no parser/runtime behavior. It replaced the W0 no-admission
clamp for fresh direct passes with an explicit W2 reclamation predicate in
`skinny/crates/bbnf-bench/src/bin/gate.rs`, limited to rows whose generated
Track 1 and independent Track 2 both meet the SPEC Section 0.2 direct floor.
The renderer emits W1 contract fields for admitted rows: strict digest plane,
`measured-row` validation, `gate_json_direct_contract` consumer,
`REDRESS-101`, and `SK-V10-W2`.

`skinny/crates/bbnf-bench/src/report.rs` now rejects any changed baseline
`N-direct / NO-GO` direct row whose Track 1 or Track 2 Mbps is below its
Section 0.2 floor. This makes the W2 numeric gate executable, not just
documented.

## Measurement

| Corpus | Track 1 | Track 2 | sonic-rs direct | Floor | Result |
|---|---:|---:|---:|---:|---|
| `apache_builds` | 11157 | 10145 | 11021 | 10020 | admitted |
| `numbers` | 12182 | 11803 | 12966 | 11788 | admitted |

Routed remainder:

- `twitter`, `canada`, `github_events`, `update_center`, `mesh`, `random`,
  `gsoc-2018`, `instruments`, `unicode_mixed`, `unicode_escapes`,
  `distinct_values`, and `y_string_unicode` remain `N-direct / NO-GO` because
  at least one of Track 1 or Track 2 misses the Section 0.2 floor.

Guard rows:

- Direct guards hold: `citm_catalog` 21129 >= 18145, `marine_ik` 9205 >=
  7575, `unicode_basic` 8973 >= 7841.
- Typed guards hold: `twitter` 18302 >= 14424, `citm_catalog` 35102 >= 20053,
  `apache_builds` 8174 >= 7373, `update_center` 11847 >= 11365, `mesh` 10032
  >= 8428, `marine_ik` 10728 >= 7369.

## Evidence

W2 decision test:

```text
cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench w2_direct -- --nocapture
```

Result: PASS, 1 test.

Direct contract tests:

```text
cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench direct_contract -- --nocapture
```

Result: PASS, 3 tests.

Report validation tests:

```text
cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench report::tests -- --nocapture
```

Result: PASS, 18 tests.

Results regeneration:

```text
CRITERION_HOME=target/skv9-w1/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo xtask gate-json --update-results --advisory
```

Result: PASS. Rendered report saved at
`/tmp/skv10-w2-gate-json-update.md`.

Cost-facts and stale-results consumer:

```text
CRITERION_HOME=target/skv9-w1/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo xtask gate-json --with-cost-facts --check-results
```

Result: PASS. JSON report saved at `/tmp/skv10-w2-cost-facts.json`.

## Gate Accounting

- `direct_to_struct` is now 5 `A / GO` and 12 `N-direct / NO-GO`.
- `parse_only` remains 17 `S / NO-GO`.
- `real_typed_struct` remains 6 `A / GO`.
- Overall outcome remains `N-direct / NoGo` because residual direct rows still
  miss.
- No parse-only or typed row moved in W2.
