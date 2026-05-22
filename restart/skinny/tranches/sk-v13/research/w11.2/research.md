# SK-V13 W11.2 Research - Object-Value Direct Dispatch Envelope

Date: 2026-05-21.
Scope: W11.N JSON direct residual reopen after W11.1 admitted
`json/numbers/direct_to_struct/main`.

## Authority

- `restart/skinny/tranches/sk-v13/SPEC.md` Section 15 authorizes W11.N direct
  residual reopen subwaves.
- The 2026-05-21 user pin lifts REDRESS 119/120 as closure authority; every
  direct residual remains reopen-eligible when a fresh material differential is
  named.
- W11.1 established that the direct dispatch envelope is a row-moving material
  differential when a generated container loop consumes the already-current
  value byte directly instead of redispatching through the generic element
  wrapper.

## Post-W11.1 State

W11.1 moved `numbers/direct_to_struct` to ADMITTED:

| row | Track 1 | sonic strict | threshold | margin |
|---|---:|---:|---:|---:|
| `json/numbers/direct_to_struct/main` | 13875 | 12918 | 12919 | +956 |

Open direct residuals nearest the selected object/array envelope family:

| row | current Track 1 | sonic+1 | margin | P1 direct hot leaf |
|---|---:|---:|---:|---|
| `json/twitter/direct_to_struct/main` | 11838 | 15231 | -3393 | `parse_object_value_at_direct` 74.0% |
| `json/github_events/direct_to_struct/main` | 12277 | 14836 | -2559 | `parse_object_value_at_direct` 67.7% |
| `json/update_center/direct_to_struct/main` | 8495 | 11278 | -2783 | `parse_object_value_at_direct` 68.3% |
| `json/gsoc-2018/direct_to_struct/main` | 15318 | 23900 | -8582 | `parse_object_value_at_direct` 60.2% |
| `json/unicode_mixed/direct_to_struct/main` | 4808 | 10497 | -5689 | `parse_object_value_at_direct` 55.9% |
| `json/unicode_basic/direct_to_struct/main` | 9189 | 8821 | +368 | `parse_object_value_at_direct` 44.1%; already ADMITTED |

The closest open direct row overall is `instruments`, but P1 classifies it as
`Option<&u8>::copied` inline/noise rather than the direct dispatch envelope.
W11.2 therefore targets the highest-confidence material family, not the
smallest absolute gap.

## Candidate

Apply the W11.1 shape to object values:

1. Keep `parse_object_value_at_direct` as the generic helper.
2. In `parse_object_direct`, immediately after colon/whitespace, peek
   `bytes.get(*cursor).copied()`.
3. Dispatch common scalar object values directly in the object loop:
   string -> `parse_string_direct` + `sink.object_string_source`;
   number -> `parse_number_object_direct`;
   `true`/`false`/`null` -> existing literal consumer + object sink method.
4. Fall back to `parse_object_value_at_direct` for `{`, `[`, and errors.

This is not a new parser and not a row-private branch. It removes one generated
dispatch wrapper from every scalar object member while preserving the existing
strict parser, sink semantics, and error kinds.

## Falsifiability

Selected row family:

- Primary: `json/twitter/direct_to_struct/main`,
  `json/github_events/direct_to_struct/main`, `json/update_center/direct_to_struct/main`.
- Guarded same-family rows:
  `json/gsoc-2018/direct_to_struct/main`,
  `json/unicode_mixed/direct_to_struct/main`,
  `json/unicode_basic/direct_to_struct/main`.

The row admits only when Track 1 clears same-run sonic strict + 1 with strict
equality and Track 2 independence. If no selected open row admits, the wave
records a measured reject or measured movement without admission and leaves
the remaining rows open.

## Pre-Blocked Routes

Still blocked:

- source-hook or fixture/corpus branch;
- new JSON number/string parser;
- direct digest shortcut or hash-only comparator;
- new directive, BIR variant, `BackendShape`, or public substrate API;
- replay of REDRESS 54/55/66-69/73/80/82/84/106-108/114-120 without this
  material differential.

## Required Owner Paths

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
