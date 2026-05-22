# SK-V13 W11.2 Plan - Object-Loop Scalar Direct Dispatch

Date: 2026-05-21.
Wave: W11.2.
Intervention: generated JSON object-loop scalar direct dispatch.

## Selected Intervention

Extend the W11.1 direct-dispatch-envelope material differential from array
numeric elements to object scalar values:

- In `parse_object_direct`, after key, colon, and whitespace, peek the current
  byte.
- Route scalar object values directly from the object loop:
  - `"` -> `parse_string_direct` then `sink.object_string_source`.
  - `-` / digit -> `parse_number_object_direct`.
  - `true` / `false` / `null` -> existing literal consumer and object sink
    method.
- Fall back to `parse_object_value_at_direct` for nested `{`, nested `[`, and
  invalid values.
- Mirror the same generated body in `codegen::json_sink_direct`.

The helper `parse_object_value_at_direct` remains as the generic fallback and
as the non-object caller surface. This is not a new parser and not a digest
shortcut.

## Owner Paths

- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/codegen/src/json_sink_direct.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/xtask/src/main.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `restart/skinny/tranches/sk-v13/research/w11.2/`

Any source path outside this set is REVISE before edit.

## Falsifiability Gate

Gate id: `G-W11.2-JSON-DIRECT-OBJECT-SCALARS`.

Primary target rows:

- `json/twitter/direct_to_struct/main`
- `json/github_events/direct_to_struct/main`
- `json/update_center/direct_to_struct/main`

Guard rows:

- `json/gsoc-2018/direct_to_struct/main`
- `json/unicode_mixed/direct_to_struct/main`
- `json/unicode_basic/direct_to_struct/main`
- all existing JSON direct/typed admits from `RESULTS.md`

Admission requires at least one primary open row to satisfy all of:

1. strict equality against sonic-rs strict on the digest/direct plane;
2. Track 1 > same-run sonic-rs strict + 1 Mbps;
3. independent Track 2/oracle maintained;
4. `RESULTS.md` and `ROLLING-SOTA-DELTA.md` gate-consumed;
5. no prior A/GO row silently demotes.

If no primary row admits but one or more primary rows improve, record
`REJECTED-MEASURED` with per-row movement and leave the source patch reverted.
If any guard regresses below its prior admitted floor, revert and reject.

## Measurement Plan

1. Before patch, capture repeated `profile_direct` probes for the primary rows
   in Track 1 / Track 2 / sonic direct / serde direct where practical.
2. Apply the object-loop scalar dispatch in runtime and generator.
3. Run parity/error-offset tests for object scalar arrays and malformed object
   values.
4. Run Criterion for selected direct rows with
   `RUSTFLAGS="-C target-cpu=native"`.
5. Write a W11.2 companion report and fact artifact; extend `gate-json` to
   consume it.
6. Run the chained companion gate with W5-W9, W11.1, and W11.2 reports.

## Pre-Blocked Routes

- No fixture, corpus, or row-private branch.
- No source-hook, direct digest shortcut, hash-only comparator, or relaxed
  strictness.
- No new parser for strings, numbers, literals, arrays, or objects.
- No SIMD primitive in this wave.
- No new directive, BIR variant, `BackendShape`, public substrate API, or
  second substrate.
- REDRESS 119/120 history must be cited but cannot close the row by itself.

## Revert Protocol

On failed gate or guard regression:

1. Save the behavior diff at `/tmp/skv13-waveW11.2-rejected.patch`.
2. Revert runtime/codegen/test/report/gate changes.
3. Commit a REDRESS rejection with measurements and routed remainder.
