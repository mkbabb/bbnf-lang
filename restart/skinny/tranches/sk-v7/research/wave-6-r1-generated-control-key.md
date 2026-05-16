# SK-V7 W6 R1 - Generated Control/Key Bookkeeping

Date: 2026-05-16.

Status: read-only diagnosis. No source files were edited.

## Inputs Read

- `restart/skinny/tranches/sk-v7/SPEC.md:285-304`: W6 is B6 control/key compaction. Named owner paths are generated JSON retained parse and possible AArch64 key-byte run scan; tasks require citm/instruments profiling, per-key dispatch optimization, and same-row benching.
- `restart/skinny/tranches/sk-v7/HANDOFF.md:66-93`: pre-blocked routes include SK-V5 UTF-8 fusion, SK-V6 retained/direct materialization routes, object next-key carry, parser-owned decoded scratch, EventCursor/sidecar prepasses, function-pointer dispatch, capacity prescan, generic SWAR whitespace, separator elision, and pair-token fusion.
- `skinny/crates/runtime/src/grammars/json/generated.rs:30-117`, `:239-337`, `:347-377`, `:689-775`: retained value/object/key/container path plus direct-only `emit_number_*` contrast.
- `skinny/crates/codegen/src/json_templates/generated.rs:30-117`, `:239-337`, `:347-377`: source template mirror. Implementation should happen here, with generated runtime regenerated or kept in lockstep.
- Supporting behavior reads: `parser.rs:35-42` for `emit_plain_offset`/`patch_flags`; `tape/assembler.rs:71-113` for offset writes and sparse flag patching; `view.rs:260-386` and `value.rs:28-47` for why close offsets and string-open offsets are load-bearing.

## Executive Finding

The current retained generated parser is already separator-sparse: it emits object/array opens, string opening quotes, scalar starts, and container closes, but not commas or colons. Therefore W6 is not a separator-elision wave. The actual control/key cost is boundary diffusion across object continuation, key string/colon parsing, value byte dispatch, structural open/close emission, whitespace skipping, and tape offset writes.

The highest-value generated compaction shape is local to object pairs: make key/colon parsing produce the first value byte, then dispatch directly, preserving the current tape shape and error kinds. This targets the current `parse_key_colon -> parse_value_at -> dispatch_value` boundary without carrying a next key across iterations, adding a side table, changing direct `SinkOnly`, or reopening the pre-blocked object next-key carry route.

Gate caveat: current `skinny/RESULTS.md` has generated Track 1 above sonic-strict on `citm_catalog` and `instruments`, while retained verdicts remain `K/NO-GO` because Track 2 lags. A generated-only patch can still be the correct owner-line compaction, but it cannot by itself prove the W6 exit if the active gate remains Track 2-sensitive.

## Current Retained Mechanics

`parse_value` skips whitespace, then `parse_value_at` loads the current byte and calls `dispatch_value` (`generated.rs:30-58`). `dispatch_value` enters `parse_object`, `parse_array`, `parse_string`, number, or literal handlers.

`parse_object` consumes `{` through `consume_structural`, which emits the open offset (`generated.rs:62-66`, `:291-304`). It skips whitespace, checks an empty object with `consume(b'}')`, and then loops `parse_pair` followed by `consume_container_next(state, b'}', ExpectedCommaOrObjectEnd)` (`generated.rs:68-78`). For a comma, `consume_container_next` skips whitespace after the comma and emits nothing. For `}`, it emits the close offset and advances (`generated.rs:327-335`).

`parse_key_colon` owns key bookkeeping (`generated.rs:90-116`):

- It records `start = state.cursor`, consumes the opening quote through `consume_quote_at_cursor`, and emits exactly that opening quote offset (`generated.rs:91-94`, `:262-269`).
- The tiny path calls `match_tiny_plain_string(state.bytes, start)` and sets `state.cursor = raw_end` on success (`generated.rs:95-97`).
- The fallback calls `match_string_at_quote`, patches `HAS_ESC` onto the opening quote cursor when needed, then sets `state.cursor = span.raw_end` (`generated.rs:98-103`).
- It checks for an immediate colon first, otherwise calls `skip_json_whitespace` from the raw string end (`generated.rs:105-111`).
- It errors with `ExpectedColon` if no colon is found, then sets `state.cursor` to the first non-whitespace byte after the colon (`generated.rs:112-116`).

`parse_pair` then calls `parse_value_at` (`generated.rs:83-86`). That means the post-colon cursor is already at the value, but the parser still crosses a second value-entry helper that checks EOF, reloads the byte, and dispatches.

Retained `emit_*` is not a generated function family. Retained emission is through `state.emit_plain_offset` at these generated call sites:

- numbers: `generated.rs:205`;
- literals: `generated.rs:232`;
- string/key opening quotes: `generated.rs:267`;
- structural opens and empty-object/empty-array closes: `generated.rs:302`;
- object close after a non-empty object: `generated.rs:333`;
- array close after a non-empty array: `generated.rs:372`.

The `emit_number_direct`, `emit_number_object_direct`, and `emit_number_array_direct` functions at `generated.rs:689-775` are direct `SinkOnly` emitters. They are not retained parser control/key owners for W6.

## Behavior That Must Not Move

- Tape offsets are classified by source byte at view time (`value.rs:28-47`). If an offset is emitted, its source byte must still be one of the JSON structural or scalar start bytes.
- Object iteration assumes the cursor after an object open is either the first key string or the object close (`view.rs:260-296`). It does not expect pair tokens, colon tokens, or comma tokens.
- Array iteration and container spans require close offsets; `span_for_value` finds container end from the close cursor (`view.rs:331-343`) and `next_sibling_cursor` depth-scans through close tokens (`view.rs:353-379`). Close-token elision is not behavior-preserving.
- String values and keys use the opening quote offset. `string_body_range` rematches from `start - 1` (`view.rs:382-386`), and `JsonString::as_str` consults sparse flags on the same cursor (`view.rs:199-217`). Any key compaction must still emit the quote before scanning and patch `HAS_ESC` on that cursor.
- Current error offsets are state-cursor based, not always the skipped-to byte. For example, missing colon after whitespace reports through `error(state, ExpectedColon)` while `state.cursor` is still at the raw string end. Invalid post-value object continuation similarly reports from the pre-skip state cursor. A behavior-preserving patch should either keep these offsets or explicitly gate any error-offset change.
- Commas and colons currently emit nothing. Adding them would alter token streams and lazy tape size; removing them is impossible because they are already absent.

## Likely Hot Leaves

The SK-V7 C1 attribution build names the generated hot boundaries directly. For the W6 rows:

| Corpus | Relevant generated self-time | Diagnosis |
|---|---:|---|
| `citm_catalog` | `consume_container_next` 12.84%, `parse_key_colon` 9.31%, `consume_array_next` 9.08%, `consume_structural` 7.68%, `skip_ws` 6.78%, `emit_offset` 8.81%, `match_tiny_plain_string` 19.94% | Broad container/key/structural diffusion; not one string-only leaf. |
| `instruments` | `match_tiny_plain_string` 31.64%, `consume_container_next` 13.21%, `parse_key_colon` 12.95%, `match_number_at_digit` 7.90%, `emit_offset` 7.23% | Mixed key/string, object continuation, and numeric leaf row. |

Older SK-V6 R6 attribution agrees on shape: `citm_catalog` was fragmented across `consume_container_next`, `match_tiny_plain_string`, `consume_structural`, `skip_ws`, `emit_plain_offset`, and `parse_key_colon`; `instruments` combined `match_tiny_plain_string`, `parse_key_colon`, `consume_container_next`, and `match_number_at_digit`.

There is no source-visible per-key heap allocation in this path. `ParserState` owns one `TapeBuilder`; key parsing emits an offset and optionally patches sparse flags. If profiling later reports an alloca-like PC, it is likely an optimizer spill or by-value match/error artifact around `JsonStringMatch` or `ParseError`, not an explicit per-key allocation in generated source.

## Concrete Owner Lines

Primary generated/template owners:

- Object loop: `skinny/crates/codegen/src/json_templates/generated.rs:62-79` and runtime mirror `skinny/crates/runtime/src/grammars/json/generated.rs:62-79`.
- Pair boundary: template/runtime `:83-86`.
- Key/colon parsing: template/runtime `:90-117`.
- Quote emission: template/runtime `:262-269`.
- Structural emission: template/runtime `:291-304`.
- Object continuation: template/runtime `:309-337`.
- Array continuation, useful as a prior compaction pattern but not the object-key owner: template/runtime `:347-377`.
- Tiny string helper shared by key and value strings: template/runtime `:161-182`.
- Retained emission wrapper: `skinny/crates/runtime/src/grammars/json/parser.rs:35-42`.
- Offset write/flag patch mechanics: `skinny/crates/runtime/src/tape/assembler.rs:71-113`.

If W6 implementation proceeds, edit the template and regenerate/check the runtime mirror. Hand-editing only `runtime/src/grammars/json/generated.rs` would create template drift.

## Candidate Compaction Shapes

### 1. Key-colon returns the value byte

Shape:

```text
parse_pair
  -> parse_key_colon_value_byte(state) -> u8
  -> dispatch_value(state, byte)
```

`parse_key_colon_value_byte` would do the current quote emission, tiny/full string scan, flag patch, colon check, and post-colon whitespace skip, then return the first value byte after checking EOF. This removes the redundant `parse_value_at` boundary after every object key while preserving:

- no colon or comma offsets;
- string-open quote emission and `HAS_ESC` patch cursor;
- `ExpectedColon` and `ExpectedValue` kinds;
- `dispatch_value` as the value semantics owner;
- object-loop behavior after comma and close.

This is the cleanest generated owner for "per-key dispatch" because it compacts only the within-pair transition. It does not carry the next key across iterations, so it is distinct from the pre-blocked object next-key carry route.

Risk: exact error offsets can drift if the helper updates `state.cursor` before the existing error points. Keep the current cursor-write order until tests prove error offsets are intentionally non-contractual.

### 2. Object-specific continuation helper

Shape:

```text
consume_object_next(state) -> Result<bool, ParseError>
```

This would specialize `consume_container_next(state, b'}', ExpectedCommaOrObjectEnd)` into a fixed object helper with the same fast current-byte check, the same whitespace fallback, and the same close-offset emission. It can use a direct `match byte { b',' => ..., b'}' => ..., _ => ... }` without carrying `close` and `error_kind` parameters.

This is behavior-preserving if:

- comma remains non-emitting;
- `}` still emits a close offset;
- EOF after comma still reaches the next key parse as `ExpectedValue`;
- invalid continuation still reports `ExpectedCommaOrObjectEnd` from the same state cursor.

Expected lift is smaller than shape 1 unless PC-level evidence shows real parameter/branch cost after normal inlining. Treat it as a layout cleanup, not a substrate change.

### 3. Key-only string-head fusion

Shape:

```text
parse_key_colon_value_byte
  inline quote check + emit_plain_offset
  inline tiny/full key string result handling
  colon/value-byte resolution
```

The current key path calls `consume_quote_at_cursor`, then `match_tiny_plain_string` or `match_string_at_quote`, then colon logic. A key-only helper can reduce state load/store churn and keep `bytes`, `start`, `open_cursor`, and `cursor` local for the hot path.

This is admissible only if it preserves the value-string path at `parse_string`; it should not delete the shared tiny probe, change `match_string_at_quote`, or alter direct `parse_string_direct`.

### 4. Key plain-close plus colon scanner, later SIMD candidate

SPEC names a possible AArch64 key-byte run scan. The behavior-preserving version is narrow:

```text
from opening quote:
  find first quote/backslash/control within the bounded plain-key window
  accept only if the first special byte is quote
  then prove colon after optional JSON whitespace
  return (raw_end, value_start_or_value_byte)
fallback:
  current parse_key_colon path
```

The scanner must never treat a colon inside the key body as the member separator. It must also preserve missing-colon error behavior. This is a second-stage candidate after the Rust-level key/value-byte compaction, not a reason to add a sidecar structural cursor.

### 5. Low-value or blocked shapes

- Do not elide close offsets. They are required for view spans and sibling traversal.
- Do not add or remove comma/colon tape offsets. The current retained tape already has zero separator offsets.
- Do not add object next-key carry across loop iterations. That is explicitly pre-blocked by HANDOFF via REDRESS 60-72.
- Do not add retained side tables, parser-local structural cursors, EventCursor prepasses, capacity prescans, or generic SWAR whitespace.
- Do not use a function-pointer dispatch table.
- Do not change direct `SinkOnly` emitters or `match_tiny_plain_string_direct::<8>` as part of this retained W6 route.
- Do not move string decode/materialization into parser control.

## Suggested Phase-2 Measurement Gate

Before implementing, confirm the current W6 baseline with a parse-attribution profile on at least `citm_catalog` and `instruments`, preserving source-line attribution for:

- `generated.rs:90` (`parse_key_colon`);
- `generated.rs:309` (`consume_container_next`);
- `generated.rs:347` (`consume_array_next`);
- `generated.rs:48` / `:37` (`dispatch_value` / `parse_value_at`);
- `generated.rs:262` and `parser.rs:35` (`consume_quote_at_cursor` / `emit_plain_offset`).

An admissible candidate should show a local drop in `parse_key_colon + parse_value_at + dispatch_value` or `consume_container_next` attribution without increasing `match_tiny_plain_string`, `match_string_at_quote`, or `emit_plain_offset` enough to erase the gain. For W6's written gate, generated Track 1 movement should be reported separately from Track 2 movement because current retained verdicts are Track 2-sensitive.

## Bottom Line

W6 should treat generated retained control/key compaction as a local object-pair transition problem, not a new JSON substrate. The safe first shape is:

```text
parse_key_colon returns first value byte; parse_pair dispatches that byte directly.
```

That targets the measured key/control leaves while preserving the existing sparse offset tape, close-token semantics, direct path split, and pre-blocked route boundaries.
