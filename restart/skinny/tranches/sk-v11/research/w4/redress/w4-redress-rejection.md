# SK-V11 W4 Redress Rejection

Date: 2026-05-20.

Disposition: REJECT under `G-W4-DISPATCH-BYTESET-DIRECT`.

The accepted W4 plan attempted the scalar `container_tail_next` route: factor
the generated Track 1 JSON post-value object/array tail into a JSON-local
helper, mirror the helper independently in the hand Track 2 digest parser, and
teach `gate-json`/`report` to consume a W4-only `random/direct_to_struct`
provenance marker at the 7878 Mbps floor.

## Implementation Attempt

The rejected source slice touched only the W4 owner surface:

- `skinny/crates/codegen/src/sink_direct.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/report.rs`

The generated and hand helpers were deliberately separate implementations. The
helper also rejected trailing comma, duplicate comma, close-after-comma, and EOF
tails before dispatching into the object key parser; this fixed a panic-shaped
malformed-tail path discovered during redress testing.

The reverted patch is saved at `/tmp/skv11-waveW4-rejected.patch` (944 patch
lines). No W4 source changes remain in the worktree.

## Pre-Measurement Evidence

These checks passed before speed probing:

- `RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- regen-json`
- `RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- check-json`
- `RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench w4 -- --nocapture`
- `RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench direct_contract -- --nocapture`
- `RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench --bin gate -- --nocapture`
- `RUSTFLAGS="-C target-cpu=native" cargo check -p codegen -p runtime -p bbnf-bench`
- `RUSTFLAGS="-C target-cpu=native" cargo build --release -p bbnf-bench --bin profile_direct`

The W4 tests proved generated Track 1 and hand Track 2 reject malformed
container tails, valid empty/nested containers preserve digest parity, Track 2
does not call generated Track 1 helper symbols, and gate/report reject stale
W2/W10 provenance or the stale 7734-7877 Mbps false-accept band.

## Probe Evidence

The W4 V2 plan was probe-first: no `RESULTS.md` row could move unless
same-host `profile_direct` probes showed `random` above the 7878 Mbps floor
with margin. The selected row failed immediately:

| Probe | Iters | Mbps | Floor | Outcome |
|---|---:|---:|---:|---|
| `random` Track 1 | 20000 | 3518 | 7878 | FAIL |
| `random` Track 2 | 5000 | 3498 | 7878 | FAIL |

The Track 1 probe recorded `ns_per_byte=2.273764`,
`cycles_per_byte=4.965075`, and `cpi=0.206475`. The Track 2 probe recorded
`ns_per_byte=2.287241`, `cycles_per_byte=5.355144`, and `cpi=0.192781`.

Both independent generated tracks miss the W4 floor by more than 55%. Criterion
was intentionally not run because the accepted plan's probe-first clause makes
this a redress stop condition.

## Routed Remainder

No `skinny/RESULTS.md` row moves. W4 is rejected with measurement and does not
authorize a future generated tail helper, W4 provenance marker, or stale-floor
admission. W5 may dispatch under SPEC Section 9 with REDRESS 113's non-JSON
axis block and W3/W4 direct rejects carried forward.
