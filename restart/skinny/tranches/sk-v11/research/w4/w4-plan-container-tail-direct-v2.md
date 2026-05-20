# SK-V11 W4 Plan V2 - Container-Tail Direct Slice

Date: 2026-05-20.

Disposition: PLAN V2 after CHALLENGE V1 REVISE.

V2 preserves the selected intervention from V1 and binds the CH1/CH4/CH5
revisions before redress.

## Selected Intervention

W4 selects P2-D D1 `container_tail_next`: one scalar JSON-local container-tail
helper over the current post-value byte cursor, consumed by generated Track 1
and independently mirrored in direct Track 2.

Selected target set: exactly `random/direct_to_struct/main`.

W4 does not claim non-JSON generalization. REDRESS 113 remains a blocked
non-JSON route. W4 does not reopen W3, parse-only, numeric-slot, object-carry,
sidecar, directive, BIR, or substrate work.

## CHALLENGE V1 Revisions Folded

CH1 correctness revisions:

- The generated helper contract is exact:

```rust
enum ContainerTail {
    Next,
    Done,
}

fn container_tail_next_direct<'i>(
    input: &'i str,
    bytes: &'i [u8],
    cursor: &mut usize,
    close: u8,
    kind: ParseErrorKind,
) -> Result<ContainerTail, ParseError<'i>>;
```

- The helper computes the post-value tail offset, classifies only `,` or
  `close`, returns `Next` only after advancing to
  `skip_ascii_whitespace(bytes, comma + 1)`, returns `Done` only after
  advancing to `close + 1`, and reports errors at the skipped tail offset.
- It is sink-free, does not handle empty containers, does not treat close after
  comma as success, does not retain a cursor/sidecar, and does not carry object
  key/value bytes across a boundary.
- Valid-row oracle language is corrected: W4 requires exact generated Track 1
  vs independent Track 2 digest equality. Serde_json and sonic-rs remain strict
  same-row direct comparators and valid-shape oracles under the existing
  `assert_direct_struct_parity` contract. Malformed tail fixtures must be
  rejected by generated Track 1, hand Track 2, serde_json, and sonic-rs.

CH4 cost revisions:

- W4 row admission is probe-first. No `RESULTS.md` row movement is allowed
  unless repeated same-host `profile_direct` probes show `random` Track 2
  above the 7878 Mbps floor with noise margin and direct guards still above
  maintain floors.
- REDRESS 115 must record tail-specific evidence explaining why D1, not row
  noise or unrelated string/whitespace/digest effects, accounts for any
  admitted movement.
- `cargo run -p xtask -- regen-json` is mandatory before `check-json` after
  editing `sink_direct.rs`.
- Direct and typed guard measurement is explicit in the redress command set.

CH5 hidden-coupling revisions:

- W4 must add one selected-row floor authority consumed by both the producer
  and validator: `random/direct_to_struct` floor 7878 Mbps. A row in the false
  accept band above the stale 7734 Mbps SK-V10 table but below 7878 Mbps must
  fail both gate and report validation.
- W4 validation rejects stale W2/W10 direct provenance even with passing Mbps.
- Track 1 and Track 2 helpers remain separate source implementations. Track 2
  must not call `runtime::generated_json`, generated SinkOnly helpers,
  `container_tail_next_direct`, or any generated Track 1 tail symbol.
- Malformed-input rejection proof is separate from valid-row digest parity.

## Owner Paths

Behavior/source owner paths:

- `skinny/crates/codegen/src/sink_direct.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/report.rs`

Conditional owner paths:

- `skinny/crates/codegen/src/lower/sink_only.rs` only for renderer metadata or
  tests; semantic lowering changes return to CHALLENGE.
- `skinny/crates/codegen/src/json_templates/generated.rs` only if
  regeneration proves the base JSON template must share a helper.
- `skinny/crates/bbnf-bench/src/track2/json.rs` only for parser-track guard
  coverage; it must not share the generated direct helper.
- `skinny/crates/bbnf-bench/benches/json_parity.rs` only for durable W4 probe
  metadata.
- `skinny/RESULTS.md` only on row-floor pass.
- `skinny/REDRESS.md` always in redress, admit or reject.

## Required Tests Before Measurement

Generated/hand correctness:

- Add generated Track 1 and hand Track 2 malformed-tail tests for:
  `{"a":1 x}`, `{"a":1   `, `{"a":1,}`, `{"a":1,,"b":2}`,
  `[1 2]`, `[1   `, `[1,]`, `[1,,2]`, `{}`, `{ }`, `[]`, `[ ]`,
  and nested object/array close-after-child cases.
- For generated Track 1, assert `ParseErrorKind` and byte offset for
  whitespace-before-bad-byte and whitespace-before-EOF in object and array
  tails.
- Assert generated Track 1, hand Track 2, serde_json, and sonic-rs all reject
  malformed container-tail fixtures.

Hidden-coupling and provenance:

- Add tests proving direct Track 2 does not call generated Track 1 or generated
  helper symbols. Source-level assertions are acceptable if they are explicit
  and checked in the W4 test suite.
- Add W4 gate/report tests that reject:
  - `random` in the 7734-7877 Mbps false-accept band;
  - unselected W4 candidates above their floor;
  - W4 candidate rows carrying `SK-V10-W2`, `SK-V10-W10`, `REDRESS-101`,
    `REDRESS-109`, `direct-reclaimed`, or `direct-residual`;
  - `gate_only`, stale `SK-V9-open`, non-`SK-V11-W4`, missing
    `REDRESS-115`, wrong W4 delta, non-digest output, deferred validation,
    non-`independent_verified` Track 2, and wrong direct comparator source;
  - direct guard floor misses for `citm_catalog`, `apache_builds`,
    `marine_ik`, or `unicode_basic`.

## Exit Gate

Exit gate is `G-W4-DISPATCH-BYTESET-DIRECT` from SPEC Section 8 with target
set restricted to `random/direct_to_struct/main`.

Admission requires:

- `random/direct_to_struct` Track 1 and Track 2 both meet 7878 Mbps in a fresh
  native Criterion capture.
- Exact generated Track 1 vs independent Track 2 digest equality passes.
- Serde_json and sonic-rs strict direct comparator rows are same-run and
  same-shape under the existing oracle contract; malformed tail fixtures reject
  in all four parsers.
- `gate-json` and `report.rs` consume W4 provenance in the same wave with
  `same_wave_consumer_class=gate_json_direct_contract`,
  `wave_id=SK-V11-W4`, `redress_entry=REDRESS-115`, and W4 direct delta
  `direct-dispatch-byteset`.
- Direct guards in SPEC Section 0.5 hold:
  `citm_catalog`, `apache_builds`, `marine_ik`, `unicode_basic`.
- Typed guards in SPEC Section 0.5 are measured and hold:
  `twitter`, `citm_catalog`, `apache_builds`, `github_events`,
  `update_center`, `mesh`, `marine_ik`.
- REDRESS 113's non-JSON axis block is carried forward.

## Measurement Commands

Run from `skinny/`.

Pre-admission probes:

```sh
RUSTFLAGS="-C target-cpu=native" cargo build --release -p bbnf-bench --bin profile_direct
for row in random citm_catalog apache_builds marine_ik unicode_basic; do
  ./target/release/profile_direct 20000 "$row" track1
  ./target/release/profile_direct 20000 "$row" track2
done
```

Build/test:

```sh
RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- regen-json
RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- check-json
RUSTFLAGS="-C target-cpu=native" cargo test -p codegen --lib -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench w4_direct -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench direct_contract -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench --bin gate w4 -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo check -p codegen -p runtime -p bbnf-bench
```

Criterion and gate:

```sh
CRITERION_HOME=/tmp/skv11-w4-criterion RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json_(random|citm_catalog|apache_builds|marine_ik|unicode_basic)/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)'
CRITERION_HOME=/tmp/skv11-w4-criterion RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json_(twitter|citm_catalog|apache_builds|github_events|update_center|mesh|marine_ik)/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)'
CRITERION_HOME=/tmp/skv11-w4-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
```

## Revert Protocol

Revert `sink_direct.rs`, regenerated `generated.rs`, `direct_struct.rs`,
gate/report edits, `RESULTS.md`, and `REDRESS.md` as one slice on any of:

- source helper violates the exact V2 contract;
- malformed-tail parity fails;
- `random` probe evidence cannot clear Track 2 with margin;
- Criterion row-floor miss on `random`;
- output mismatch between generated Track 1 and independent Track 2;
- serde_json or sonic-rs malformed-tail acceptance;
- direct or typed guard regression;
- Track 2 coupling;
- owner-path/Lock 14 violation;
- missing same-wave gate/report W4 consumption.

On reject, save the patch to `/tmp/skv11-waveW4-rejected.patch` and record
REDRESS 115 with measured evidence.
