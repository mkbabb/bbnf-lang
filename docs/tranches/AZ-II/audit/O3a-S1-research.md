# AZ-II.cutover.O3a-S1 Research - Sheets Failures

**Agent**: AZ-II O3a-S1 research
**Date**: 2026-04-29
**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-azii-o3a-s1-research`
**Scope**: root-cause all 33 Sheets failures in the O3a-S1 cohort; no
source edits.

## Summary

All 33 S1 failures route to generated StructDirect projection/emission
or runtime serializer consumers, not to `Parsed<R>` return-model
deletion. The failure split is:

| Category | Count | Root cause |
|---|---:|---|
| return-model | 0 | No observed failure depends on old `Parsed<R>` / tape-return shape. Tests call `GoogleSheetsParser::parse` and fail after parse admission/materialisation. |
| branch-tag/payload | 23 | Generated branches parse literals or operator tokens without emitting the grammar's declared `->` payload, or emit `push_leaf_with_unit()` for typed `u8` branches. |
| projection | 6 | Generated wrappers/dispatchers do not project sheet-prefix/range/array grammar branches into the Sheets document tree. |
| serializer | 4 | `SheetsDocument::serialize_compact` re-emits admitted tree data lossily, mainly quoted-string bodies without quotes. |
| labelled unknown | 0 | No S1 failure remains unknown. |

## Evidence

Baseline assignment is from
`docs/benchmarks/archive/AZ-II/cutover/O3a-test-failures.txt:33-66`, which lists
the 33 Sheets failures, and the duplicate retry list at lines 118-151.

Focused command evidence:

```text
cargo nextest run -p bbnf --cargo-profile ax-iter --test sheets_parity \
  -E 'test(boolean_first_branch_fires_true_payload) | test(error_literal_error_branch_fires_payload) | test(operator_branches_parse) | test(range_ref_parses_with_and_without_sheet_prefix) | test(unary_prefix_first_branch_fires_0u8)' \
  --no-fail-fast -- --nocapture
```

Observed failures:
- `boolean_first_branch_fires_true_payload`: `got payloads = []`.
- `error_literal_error_branch_fires_payload`: `Payloads = []`.
- `operator_branches_parse` and `unary_prefix_first_branch_fires_0u8`:
  panic at `SheetsStructBuilder::push_leaf_with_unit`.
- `range_ref_parses_with_and_without_sheet_prefix`: fails on
  `"=Sheet1!A1:B2"`.

Focused serializer command evidence:

```text
cargo nextest run -p bbnf --cargo-profile ax-iter --test sheets_self_parity \
  -E 'test(serialize_roundtrip_string_empty) | test(serialize_roundtrip_error_generic) | test(serialize_roundtrip_range_ref_sheet_prefixed) | test(serialize_roundtrip_range_ref_column) | test(serialize_roundtrip_array_literal_single_row) | test(serialize_roundtrip_unary_plus) | test(corpus_simple)' \
  --no-fail-fast -- --nocapture
```

Observed failures:
- `corpus_simple`: non-idempotent at `=CONCATENATE(A1, " ", B1)`;
  first emit `=CONCATENATE(A1, ,B1)`, second emit
  `=CONCATENATE(A1,,B1)`.
- array/range/error/string samples fail reparse during fixed-point
  checking with `Syntax { offset: 1, rule: None }`, except sheet-prefixed
  range which fails at offset 7.
- unary sample panics at `SheetsStructBuilder::push_leaf_with_unit`.

Generated/runtime evidence:
- Grammar declares `boolean -> true/false`, `error_literal -> 0u8..8u8`,
  `sheet_prefix -> 0u8/1u8`, and operator `u8` discriminants in
  `grammar/google-sheets/google-sheets.bbnf:17-49` and `:94-116`.
- Generated `parse_wrap_GoogleSheetsParser_boolean` opens a wrap
  compound but has no branch arms before returning syntax error:
  `crates/core/src/grammar/generated/google_sheets.rs:2938-3004`.
- Generated `parse_flat_GoogleSheetsParser_error_literal` matches all
  error literal byte suffixes but never calls `push_branch_tag` or
  `push_leaf_error`; it only closes an empty compound:
  `crates/core/src/grammar/generated/google_sheets.rs:3027-3414`.
- Generated `parse_wrap_GoogleSheetsParser_sheet_prefix` also has no
  branch arms: `crates/core/src/grammar/generated/google_sheets.rs:3430-3495`.
- Generated keyword operator parsers call `builder.push_leaf_with_unit()`
  for typed `u8` arms, including compare/unary/mul/add:
  `crates/core/src/grammar/generated/google_sheets.rs:3595-3786` and
  `:3799-3990`.
- Runtime confirms `push_leaf_with_unit()` is invalid for Sheets
  projections and debug-panics before depositing synthetic `Tag(0)`:
  `crates/core/src/runtime/google_sheets/builder.rs:283-296`.
- Runtime provides the needed specialised sinks
  `push_leaf_sheet_prefix(tag, value)` and `push_leaf_error(value)`, but
  generated Sheets code does not call them:
  `crates/core/src/runtime/google_sheets/builder.rs:336-355`.
- String parser stores quoted string bodies without delimiters:
  `crates/core/src/grammar/generated/google_sheets.rs:2827-2862`;
  serializer writes `SheetsValue::String` directly without adding quotes:
  `crates/core/src/runtime/google_sheets/document.rs:169-176`.
- Range serializer can reinsert `:` only if the projected `RangeRef`
  has the expected children; sheet-prefix projection is missing:
  `crates/core/src/runtime/google_sheets/document.rs:291-305`.
- Array/range grammar shapes are declared in
  `grammar/google-sheets/google-sheets.bbnf:78-81` and `:156-158`;
  generated primary dispatch does attempt `array_literal` for `{` at
  `crates/core/src/grammar/generated/google_sheets.rs:7391-7408`, so
  the failing array roundtrips are projection/admission failures below
  primary dispatch rather than return-model failures.

## Failure Map

| Failed test | Category | Root cause / evidence |
|---|---|---|
| `bbnf::sheets_parity boolean_first_branch_fires_true_payload` | branch-tag/payload | `boolean` declares bool branch payloads, but generated `parse_wrap_GoogleSheetsParser_boolean` has no arms and emits no `Bool`; focused run observed `payloads = []`. |
| `bbnf::sheets_parity error_literal_error_branch_fires_payload` | branch-tag/payload | `error_literal` parses `#ERROR!` bytes but emits no `Error(7)` / tag; focused run observed `Payloads = []`. |
| `bbnf::sheets_parity error_literal_divzero_branch_fires_payload` | branch-tag/payload | Same `error_literal` body lacks discriminator emission for branch 3. |
| `bbnf::sheets_parity error_literal_first_branch_fires` | branch-tag/payload | Same `error_literal` body lacks discriminator emission for branch 0. |
| `bbnf::sheets_parity error_literal_factored_branch_fires_payload` | branch-tag/payload | Same `error_literal` body lacks discriminator emission for branch 4 after the factored `#N...` path. |
| `bbnf::sheets_parity error_literal_name_branch_fires_payload` | branch-tag/payload | Same `error_literal` body lacks discriminator emission for branch 5. |
| `bbnf::sheets_parity error_literal_num_branch_fires_payload` | branch-tag/payload | Same `error_literal` body lacks discriminator emission for branch 6. |
| `bbnf::sheets_parity error_literal_ref_branch_fires_payload` | branch-tag/payload | Same `error_literal` body lacks discriminator emission for branch 2. |
| `bbnf::sheets_parity error_literal_spill_branch_fires_payload` | branch-tag/payload | Same `error_literal` body lacks discriminator emission for branch 8. |
| `bbnf::sheets_parity error_literal_value_branch_fires_payload` | branch-tag/payload | Same `error_literal` body lacks discriminator emission for branch 1. |
| `bbnf::sheets_parity operator_branches_parse` | branch-tag/payload | Focused run panics in `push_leaf_with_unit`; the test reaches unary/operator typed-branch emission where generated keyword arms call unit instead of tag. |
| `bbnf::sheets_parity range_ref_parses_with_and_without_sheet_prefix` | projection | Focused run fails on `"=Sheet1!A1:B2"`; generated `sheet_prefix` wrapper has no regex branches, so sheet-prefixed range projection/admission is absent. |
| `bbnf::sheets_parity unary_prefix_first_branch_fires_0u8` | branch-tag/payload | Generated `parse_keyword_GoogleSheetsParser_unary_prefix` emits `push_leaf_with_unit()` for `+`/`-`; focused run panics at the runtime debug assertion. |
| `bbnf::sheets_self_parity corpus_simple` | serializer | Focused run proves admitted formula serializes non-idempotently by dropping quotes around `" "` in `CONCATENATE`; string serializer writes raw body. |
| `bbnf::sheets_self_parity corpus_nested` | serializer | Same corpus fixed-point harness; nested corpus contains admitted formulas whose compact serializer is lossy under the same string/operator/tree emission paths. |
| `bbnf::sheets_self_parity corpus_stress` | serializer | Same corpus fixed-point harness; stress corpus failures are serializer fixed-point failures, not return-model failures. |
| `bbnf::sheets_self_parity serialize_roundtrip_array_literal_multi_row` | projection | Array grammar is present and primary dispatch attempts `{`, but generated array rows/literal projection does not produce a reparsable fixed point. |
| `bbnf::sheets_self_parity serialize_roundtrip_array_literal_single_row` | projection | Focused run fails fixed-point checking for `={1,2,3}` with syntax at offset 1; source is the array-literal projection below primary dispatch. |
| `bbnf::sheets_self_parity serialize_roundtrip_error_generic` | branch-tag/payload | First parse can admit error literals through `parse_flat_GoogleSheetsParser_error_literal`, but no error discriminator is emitted, so serializer output cannot reparse. |
| `bbnf::sheets_self_parity serialize_roundtrip_error_na` | branch-tag/payload | Same missing error discriminator for branch 0. |
| `bbnf::sheets_self_parity serialize_roundtrip_error_divzero` | branch-tag/payload | Same missing error discriminator for branch 3. |
| `bbnf::sheets_self_parity serialize_roundtrip_error_name` | branch-tag/payload | Same missing error discriminator for branch 5. |
| `bbnf::sheets_self_parity serialize_roundtrip_error_null` | branch-tag/payload | Same missing error discriminator for branch 4. |
| `bbnf::sheets_self_parity serialize_roundtrip_error_num` | branch-tag/payload | Same missing error discriminator for branch 6. |
| `bbnf::sheets_self_parity serialize_roundtrip_error_ref` | branch-tag/payload | Same missing error discriminator for branch 2. |
| `bbnf::sheets_self_parity serialize_roundtrip_error_spill` | branch-tag/payload | Same missing error discriminator for branch 8. |
| `bbnf::sheets_self_parity serialize_roundtrip_error_value` | branch-tag/payload | Same missing error discriminator for branch 1. |
| `bbnf::sheets_self_parity serialize_roundtrip_range_ref_column` | projection | Focused run fails fixed-point checking at offset 1 for `=A:A`; range endpoint/range projection is not materializing a reparsable tree. |
| `bbnf::sheets_self_parity serialize_roundtrip_range_ref_quoted_sheet` | projection | Same missing `sheet_prefix` projection as sheet-prefixed range parse; quoted prefix wrapper has no branch arms. |
| `bbnf::sheets_self_parity serialize_roundtrip_range_ref_sheet_prefixed` | projection | Focused run fails at offset 7 for `=Sheet1!A1:B2`; `sheet_prefix` wrapper admits no bare-prefix branch. |
| `bbnf::sheets_self_parity serialize_roundtrip_string_empty` | serializer | String parser stores the body without quotes and serializer writes it raw; empty string becomes an unreparsable / lossy formula surface. |
| `bbnf::sheets_self_parity serialize_roundtrip_unary_plus` | branch-tag/payload | Focused run panics in `push_leaf_with_unit`; generated unary prefix emits unit instead of the `0u8` tag. |
| `bbnf::sheets_self_parity serialize_roundtrip_unary_minus` | branch-tag/payload | Same generated unary prefix unit emission for `-` instead of the `1u8` tag. |

## Owner Routing

- **O4 return-model deletion**: no S1 failures should block O4 directly.
  Keep this cohort out of O4 except for ensuring O4 does not hide these
  symptoms behind return-type churn.
- **Source redress child wave**: branch-tag/payload and projection fixes
  need emitter + Sheets runtime coordination:
  `crates/core/src/backend/rust/emitter/shapes/**`,
  `crates/core/src/runtime/google_sheets/**`, and regenerated
  `crates/core/src/grammar/generated/google_sheets.rs`.
- **O6 Sheets truth**: O6 cannot claim Sheets semantic health until
  branch payloads, sheet-prefix/range/array projection, and compact
  serializer fixed points are green.

## Verification Notes

I did not edit source or generated files. I ran two focused nextest
commands under `ax-iter`; both intentionally failed and are cited above
as diagnostic evidence.
