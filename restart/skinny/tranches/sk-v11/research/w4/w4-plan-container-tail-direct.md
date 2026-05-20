# SK-V11 W4 Plan - Container-Tail Direct Slice

Date: 2026-05-20.

Disposition: PLAN, pending CHALLENGE.

## Selected Intervention

Select P2-D D1 `container_tail_next` for W4. The redress slice factors one
scalar container-tail helper over the current byte cursor and consumes it in the
JSON generated direct parser plus the independent direct Track 2 hand parser.

The selected target row is `random/direct_to_struct/main` only.

Why D1, not D2:

- D2 `direct_slot_dispatch` is mostly a generated Track 1 code-shape refactor.
  W4 admission requires Track 2 to clear the same floor, and `random` Track 2 is
  the limiting side.
- D1 has an honest same-shape independent Track 2 implementation: classify
  comma/close after a value, advance after comma, and close the container on the
  configured end byte. Track 2 remains independent because it owns its own local
  helper and does not call generated Track 1 or generated helper code.
- The helper is scalar, JSON-local generated code. It adds no directive, BIR
  variant, backend shape, class lane, sidecar, retained cursor, object carry, or
  parse-only substrate.

## SPEC Section 8 Correction

W4 research found a source-owner omission. The generated direct parser is
emitted by `skinny/crates/codegen/src/sink_direct.rs`; editing only
`lower/sink_only.rs` and `json_templates/generated.rs` cannot change the
generated direct sink path. The plan therefore updates SPEC Section 8 to add
`skinny/crates/codegen/src/sink_direct.rs` to the W4 owner table.

The plan also corrects the stale entry-gate sentence that said W2's non-JSON
proof remains valid for generic edits. REDRESS 113 blocks W2. W4 carries that
block and does not claim non-JSON generalization.

CHALLENGE must explicitly accept or reject this SPEC correction before redress.
If CHALLENGE rejects it, W4 returns REVISE before source editing.

## Owner Paths

Behavior/source owner paths:

- `skinny/crates/codegen/src/sink_direct.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/report.rs`

Conditional owner paths:

- `skinny/crates/codegen/src/lower/sink_only.rs` only for renderer metadata or
  tests; prefer no semantic edit.
- `skinny/crates/codegen/src/json_templates/generated.rs` only if
  regeneration proves the base JSON template must share a helper.
- `skinny/crates/bbnf-bench/src/track2/json.rs` only if a parser Track 2 guard
  test needs the same local tail helper shape.
- `skinny/crates/bbnf-bench/benches/json_parity.rs` only for durable W4 probe
  metadata; prefer existing Criterion rows.
- `skinny/RESULTS.md` only on row-floor pass.
- `skinny/REDRESS.md` always in redress, admit or reject.

## Exit Gate

Exit gate is `G-W4-DISPATCH-BYTESET-DIRECT` from SPEC Section 8.

Selected row floor:

| Row | Track 1 baseline | Track 2 baseline | Floor |
|---|---:|---:|---:|
| `random/direct_to_struct/main` | 7693 Mbps | 6949 Mbps | 7878 Mbps |

Admission requires:

- `random/direct_to_struct` Track 1 and Track 2 both meet 7878 Mbps in a fresh
  native Criterion capture.
- Same-output digest equality passes across generated Track 1, independent
  Track 2, serde_json, and sonic-rs.
- `gate-json` consumes W4 provenance in the same wave with
  `same_wave_consumer_class=gate_json_direct_contract`,
  `wave_id=SK-V11-W4`, `redress_entry=REDRESS-115`, and a W4 direct delta.
- Direct guards in SPEC Section 0.5 hold:
  `citm_catalog`, `apache_builds`, `marine_ik`, `unicode_basic`.
- Typed guards in SPEC Section 0.5 hold if measured or if `RESULTS.md` movement
  requires report-wide validation.
- W2's non-JSON axis remains blocked and is not used as a generic-code proof.

## Implementation Sketch

Generated Track 1:

- In `sink_direct.rs`, emit a local `ContainerTail` enum and
  `container_tail_next_direct` helper.
- Use the helper in generated `parse_object_direct` and `parse_array_direct`
  after the current child value parse and whitespace skip.
- Preserve empty-container handling, key parsing, colon handling, sink calls,
  and all value dispatch semantics.
- Regenerate `runtime/src/grammars/json/generated.rs` with
  `cargo run -p xtask -- regen-json`.

Independent Track 2:

- In `direct_struct.rs`, add an independent `HandContainerTail` helper or
  equivalent method on `HandParser`.
- Consume it in the hand digest parser's `object` and `array` loops.
- Do not call generated code, generated helper functions, or shared parser
  helpers. Independence must remain structural, not just caller-level.

Gate/report:

- Add a W4 selected-row direct decision branch for `random` before the W0 clamp.
- Add W4 provenance marking and negative tests for missing W4 provenance,
  below-floor rows, and Track 2 coupling if not already covered by direct
  contract tests.
- Add matching report validation for `SK-V11-W4` / `REDRESS-115` without new
  telemetry fields.

## Measurement Commands

Run from `skinny/`:

```sh
RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- check-json
RUSTFLAGS="-C target-cpu=native" cargo test -p codegen --lib -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench direct_struct track2::json -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench --bin gate w4 -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo check -p codegen -p runtime -p bbnf-bench
CRITERION_HOME=/tmp/skv11-w4-criterion RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json_(random|citm_catalog|apache_builds|marine_ik|unicode_basic)/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)'
CRITERION_HOME=/tmp/skv11-w4-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
```

## Revert Protocol

Revert `sink_direct.rs`, regenerated `generated.rs`, `direct_struct.rs`,
gate/report edits, `RESULTS.md`, and `REDRESS.md` as one slice on any of:

- row-floor miss on `random`;
- output mismatch across Track 1, Track 2, serde_json, or sonic-rs;
- guard regression;
- Track 2 coupling;
- owner-path/Lock 14 violation;
- CHALLENGE rejection of the SPEC Section 8 owner correction.

On reject, save the reverted patch to `/tmp/skv11-waveW4-rejected.patch` and
record REDRESS 115 with measured evidence.

## Pre-Blocked Routes

W4 explicitly rejects:

- REDRESS 63 -> 65/84 object carry transfer;
- W3 union/event/class-column/streaming-cursor/class-lane/substrate repair;
- hidden sidecar dispatch facts or retained byte/class masks;
- function-pointer dispatch tables;
- generic JSON policy in generic crates;
- Track 1/Track 2 coupling or Track 2 calling generated helpers;
- W3 numeric-slot laundering from REDRESS 114;
- non-JSON closure by prose while REDRESS 113 remains blocked.
