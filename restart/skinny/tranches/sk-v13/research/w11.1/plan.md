# SK-V13 W11.1 Plan - `numbers` Direct Numeric-Array Reopen

Date: 2026-05-22.
Phase: Plan.
Status: selected for CHALLENGE.

## Selection

W11.1 targets `json/numbers/direct_to_struct/main`.

Selected intervention: generated scalar direct numeric-array consumer.
The generated direct array loop peeks the next element byte and, for
`b'-' | b'0'..=b'9'`, calls `parse_number_array_direct` directly instead
of routing through the generic `parse_array_element_at_direct` value
dispatch envelope. All non-number elements continue through the existing
generic dispatcher. `NumberSpan`, strict numeric materialization, sink
semantics, and error behavior remain unchanged.

This is a fresh material differential from REDRESS 119/120 because it
changes generated direct dispatch shape. It is not a digest/hash patch,
source hook, decoded-string hook, one-row materializer change, parser
cursor sidecar, SIMD orphan, or substrate replay.

## Owner Paths

Behavior / generation:

- `skinny/crates/runtime/src/grammars/json/generated.rs`.
- `skinny/crates/codegen/src/json_sink_direct.rs`.

Measurement / gate / docs:

- `skinny/crates/bbnf-bench/src/direct_struct.rs`.
- `skinny/crates/bbnf-bench/src/report.rs`.
- `skinny/crates/bbnf-bench/src/bin/gate.rs`.
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`.
- `skinny/xtask/src/main.rs`.
- `skinny/REDRESS.md`.
- `restart/skinny/tranches/sk-v13/research/w11.1/**`.

Conditional on measured admission or disposition movement:

- `skinny/RESULTS.md`.
- `restart/skinny/ROLLING-SOTA-DELTA.md`.

Out of scope for W11.1:

- `skinny/crates/bbnf-simd/**`.
- `skinny/crates/parse-that-regex/**`.
- `JsonDirectDigest` hashing/folding internals except if a measurement
  helper is required.

## Exit Gate

`G-W11.1-JSON-DIRECT-NUMBERS` passes only if:

1. `json/numbers/direct_to_struct/main` Track 1 is strictly greater than
   same-run sonic-rs strict direct by at least 1 Mbps. Current pinned bar
   from `RESULTS.md` is `12599` Mbps.
2. Strict digest equality holds across Track 1, Track 2, serde, and
   sonic.
3. Existing A/GO rows do not silently demote in the guard refresh.
4. The report is consumed by gate-json under
   `--skv13-json-direct-reopen-report`.
5. Lock 14 owner-path and generic-crate scans pass.

If Track 1 moves but remains below the strict +1 bar, the wave records a
measured REDRESS rejection. No `RESULTS.md` admission is allowed.

## Pre-Blocked Routes

- REDRESS 119/120 are history only and cannot close the row.
- No source-hook replay, decoded-string hash/stat hook, digest-only hash
  patch, one-row number/control patch, parser-local cursor, sidecar
  substrate, public substrate API, new BIR variant, new directive, or new
  `BackendShape`.
- No row-private `numbers` branch. The shape must be generated JSON
  direct array behavior applicable to any numeric array.
- No `bbnf-simd`, UDOT, PMULL, CSSC, EOR3, or other ASM primitive in
  W11.1. SIMD/ASM zero-orphan work is W12.

## Required Evidence

Plan redress should produce:

- `restart/skinny/tranches/sk-v13/research/w11.1/artifacts/direct-row-facts.json`.
- `restart/skinny/tranches/sk-v13/research/w11.1/skv13-W11.1-json-direct-reopen.json`.
- REDRESS item with before/after Track 1, Track 2, sonic, threshold,
  material differential, and route disposition.

Suggested measurement commands:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
RUSTFLAGS="-C target-cpu=native" cargo build --release -p bbnf-bench --bin profile_direct
./target/release/profile_direct 20000 numbers track1
./target/release/profile_direct 20000 numbers track2
./target/release/profile_direct 20000 numbers sonic
./target/release/profile_direct 20000 numbers serde
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/numbers/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)'
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-json-direct-reopen-report ../restart/skinny/tranches/sk-v13/research/w11.1/skv13-W11.1-json-direct-reopen.json
```

## Revert Protocol

If parity fails, generated runtime/template reproducibility fails, Lock 14
fails, or Track 1 remains `<= sonic + 1`, revert the behavior source and
generated output together. Save the rejected behavior diff at
`/tmp/skv13-waveW11.1-rejected.patch`, append REDRESS with row numbers
and material differential, and leave `RESULTS.md` /
`ROLLING-SOTA-DELTA.md` unchanged unless the row disposition changes.

## CHALLENGE Focus

- Correctness for empty arrays, mixed arrays, nested arrays, whitespace,
  trailing-comma rejection, invalid number rejection, and array element
  counting.
- Codegen parity: runtime-only edits are invalid.
- Noise discipline: the pinned gap is small, so the same-run native
  measurement must be explicit.
- Scope discipline: any `bbnf-simd` or digest/hash/source-hook edit
  rejects this plan.
