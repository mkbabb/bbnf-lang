# SK-V13 W11.4 Plan - Direct Cursor Byte Fetch

Date: 2026-05-22.
Wave: W11.4.
Gate: `G-W11.4-JSON-DIRECT-CURSOR-BYTE`.

## Selected Intervention

Replace the hot `bytes.get(*cursor).copied()` direct-dispatch byte fetches with
an explicit bounds check followed by `unsafe { *bytes.get_unchecked(*cursor) }`
in the generated JSON direct parser. The error kind, error offset, cursor
movement, sink calls, digest semantics, and comparator semantics remain
unchanged.

This is the material differential from REDRESS 119/120/143:

- REDRESS 119/120 recorded the pre-pin direct residual fixpoint.
- REDRESS 143 specialized the sink stack parent access and admitted `mesh`,
  but `instruments` still missed.
- W11.4 attacks the profiled instruments hot leaf in the generated direct
  parser envelope: `Option<&u8>::copied`, not the sink parent access layer.

## Owner Paths

- `skinny/crates/codegen/src/json_sink_direct.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `restart/skinny/tranches/sk-v13/research/w11.4/`

Any digest shortcut, comparator change, source fixture branch, SIMD edit,
union-substrate edit, directive/BIR/BackendShape change, or report-only
admission is out of W11.4 scope.

## Implementation Shape

Patch both `json_sink_direct.rs` and the checked-in generated JSON file at the
four direct-dispatch sites:

- `parse_value_direct`
- `parse_object_value_at_direct`
- `parse_array_element_at_direct`
- `parse_array_direct`

Use the same local pattern at each site:

```rust
if *cursor >= bytes.len() {
    return Err(direct_error(input, *cursor, ParseErrorKind::ExpectedValue));
}
let byte = unsafe { *bytes.get_unchecked(*cursor) };
```

No helper is introduced unless code size or borrow shape forces one. Keeping
the pattern local makes the generated output direct and avoids an extra call in
the hot envelope.

## Falsifiability Gate

`G-W11.4-JSON-DIRECT-CURSOR-BYTE` admits only if:

- strict parity passes for direct sinks;
- native Criterion measures at least one target row over same-run sonic strict
  + 1 Mbps;
- the primary target `json/instruments/direct_to_struct/main` is measured;
- Track 2 remains independent;
- no admitted JSON or CSS row silently demotes;
- `RESULTS.md`, `ROLLING-SOTA-DELTA.md`, and REDRESS consume the exact report.

Primary target:

- `json/instruments/direct_to_struct/main`: current Track 1 `12307`, sonic+1
  `12785`, margin `-478`.

Guard probes:

- `json/mesh/direct_to_struct/main`
- `json/random/direct_to_struct/main`
- `json/canada/direct_to_struct/main`
- `json/github_events/direct_to_struct/main`

## Measurement

Run before admit/reject:

```text
cargo test -p bbnf-bench direct_struct::tests -- --nocapture
cargo test -p bbnf-bench direct_contract -- --nocapture
cargo test -p runtime json -- --nocapture
cargo test -p codegen json -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/(instruments|mesh|random|canada|github_events)/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)'
```

If no row admits, save `/tmp/skv13-waveW11.4-rejected.patch`, revert the
source patch, and record measured rejection under REDRESS. If a row admits,
add a W11.4 direct reopen report/gate lane and refresh `RESULTS.md` plus the
rolling delta.

## Revert Protocol

The revert slice is exactly:

- `skinny/crates/codegen/src/json_sink_direct.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- any W11.4 report/gate/status artifacts added during redress

No unrelated CSS sidecar JSON dirty state may be staged with this wave.
