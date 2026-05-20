# SK-V11 W3 Redress Rejection

Date: 2026-05-20.

Disposition: REJECT under `G-W3-NUMERIC-SEQUENCE-DIRECT`.

The accepted W3 plan attempted the scalar `number_span_emit_slot` route: keep
number grammar policy unchanged, factor generated JSON number emission through
a const-generic slot helper, add generated direct-number semantic coverage, and
teach `gate-json`/`report` to consume a W3 numeric direct provenance marker.

## Implementation Attempt

The rejected source slice touched only the W3 owner surface:

- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/codegen/src/sink_direct.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`

The reverted patch is saved at `/tmp/skv11-waveW3-rejected.patch` (1874 patch
lines). No W3 source changes remain in the worktree.

## Pre-Measurement Evidence

These checks passed before measurement:

- `RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- check-json`
- `RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench --bin gate w3 -- --nocapture`
- `RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench generated_direct_number_slots_match_serde -- --nocapture`
- `RUSTFLAGS="-C target-cpu=native" cargo check -p runtime -p codegen`

Probe evidence was mixed and insufficient for admission. Parallel probes showed
`mesh` Track 1 at 3413 Mbps and Track 2 at 3515 Mbps; serial probes showed one
pair at 3523 / 3435 Mbps and a second pair at 3444 / 3271 Mbps. `numbers`
improved Track 1 in one probe but regressed Track 2, and W3 selected `mesh`
because it had the nearest direct residual floor.

## Measurement Evidence

Criterion evidence is under `/tmp/skv11-w3-criterion`. The selected measured
row was `mesh/direct_to_struct`, whose W3 floor is 8675 Mbps.

| Bench | Mean | Mbps |
|---|---:|---:|
| `json/mesh/track1_direct_to_struct` | 1,509,602 ns | 3835 |
| `json/mesh/track2_direct_to_struct` | 1,601,641 ns | 3614 |
| `json/mesh/sonic_rs_direct_to_struct` | 1,311,707 ns | 4413 |
| `json/mesh/serde_json_direct_to_struct` | 1,814,350 ns | 3191 |

Both generated tracks miss the W3 floor by more than 50%. `G-W3-NUMERIC-
SEQUENCE-DIRECT` is therefore falsified, no `skinny/RESULTS.md` row is moved,
and no W3 admission is made.

## Routed Remainder

W3 now has a measured REDRESS disposition. W4 may dispatch under SPEC Section 8
with W2's non-JSON axis still blocked by REDRESS 113. W3 does not reopen number
policy, f64 fallback, mantissa widening, UDOT, or parse-only numeric evidence.
