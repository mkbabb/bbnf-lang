# AZ-II.cutover.O3a-S1 Redress Probe

Date: 2026-04-29
Agent: AZ-II O3a-S1 redress/probe
Worktree: `/Users/mkbabb/Programming/bbnf-wt-azii-o3a-s1-redress`
Branch: `codex/azii-o3a-s1-redress`

## Disposition

HALT for source redress, READY for plan amendment.

Focused Sheets evidence is complete: the two S1 nextest files reproduce
exactly 33 failures, matching `docs/tranches/AZ-II/waves/cutover/O3a-S1.md`.
Do not patch source until the S1 plan/wave amendment lands. The failures are
not test-only drift; expanded code shows generated Sheets StructDirect emission
missing payload/specialized leaf routing and runtime serialization losing
quoted-string surface.

## Commands

All cargo commands used the isolated target:

`CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-azii-o3a-s1-redress/target-azii-o3a-s1-redress`

1. `cargo nextest run -p bbnf --test sheets_parity --cargo-profile ax-iter -- --nocapture`
   - Built the fresh isolated target in `1m 52s`.
   - Stopped on fail-fast after 2 of 35 tests: 1 passed, 1 failed.
   - Failure:
     `bbnf::sheets_parity boolean_first_branch_fires_true_payload`
     panicked at `crates/core/tests/sheets_parity.rs:275` with
     `boolean TRUE -> true (1u8) must fire; got payloads = []`.

2. `cargo nextest run -p bbnf --test sheets_parity --cargo-profile ax-iter --no-fail-fast -- --nocapture`
   - Reused the isolated target; finished build in `0.27s`.
   - Summary: `35 tests run: 22 passed, 13 failed, 0 skipped`.
   - Empty typed-payload walk failures:
     `boolean_first_branch_fires_true_payload` and all nine
     `error_literal_*_branch_fires*` tests reported `Payloads = []` or
     `payloads = []`.
   - Operator/unary failures:
     `operator_branches_parse` and `unary_prefix_first_branch_fires_0u8`
     panicked at `crates/core/src/runtime/google_sheets/builder.rs:290` with
     `SheetsStructBuilder::push_leaf_with_unit invoked; Sheets grammar has no
     unit-typed projection`.
   - Range failure:
     `range_ref_parses_with_and_without_sheet_prefix` panicked at
     `crates/core/tests/sheets_parity.rs:512` for
     `range_ref must parse: "=Sheet1!A1:B2"`.

3. `cargo nextest run -p bbnf --test sheets_self_parity --cargo-profile ax-iter --no-fail-fast -- --nocapture`
   - Reused the isolated target; finished build in `0.45s`.
   - Summary: `84 tests run: 64 passed, 20 failed, 0 skipped`.
   - Corpus serialization failures:
     `corpus_nested` line 0 first serialized to a form with `N/A`, then
     reserialized as `N^A`; `corpus_simple` line 4 serialized
     `" "` to a bare space and then to an empty argument; `corpus_stress`
     line 0 serialized `"No results"` to bare text and then to
     `No,results`.
   - Parse-after-serialize failures:
     both array literal roundtrips, all nine error literal roundtrips,
     three range-ref roundtrips, and `serialize_roundtrip_string_empty`
     failed with `sheets parse must succeed: Syntax { offset: ..., rule: None }`.
     Representative offsets: array/string/range-column at offset 1,
     sheet-prefixed range at offset 7.
   - Unary serializer failures:
     `serialize_roundtrip_unary_plus` and `serialize_roundtrip_unary_minus`
     hit the same `push_leaf_with_unit` panic as `sheets_parity`.

4. `cargo expand -p bbnf --lib grammar::generated::google_sheets > target/expand/O3a-S1-google_sheets.rs`
   - Saved expanded artifact:
     `target/expand/O3a-S1-google_sheets.rs`.
   - Finished after checking `bbnf`; expansion has 22,804 lines.

## Expanded-Code Evidence

Primary artifact:
`target/expand/O3a-S1-google_sheets.rs`.

- Boolean emission is effectively empty. `parse_wrap_GoogleSheetsParser_boolean`
  opens a `boolean` compound with `rule_type: TypeDesc::Span`, has no concrete
  branch arms under `match first`, and only pushes `__wrap_branch_idx` after an
  unreachable success path. See expanded lines 2919-2985. This matches
  `typed_u8_payloads("=TRUE") == []`.

- Error literal emission matches bytes but never emits the declared `Nu8`
  payload or `SheetsValue::Error`. `parse_flat_GoogleSheetsParser_error_literal`
  creates a `StructLayout` with `rule_type: TypeDesc::Span`; its branch bodies
  only advance `p` and commit checkpoints, then close the compound. There is no
  `push_leaf_error`, `push_branch_tag`, or scalar payload write in the body. See
  expanded lines 3019-3407. This explains all nine `Payloads = []` error
  literal failures and the error serializer parse failures.

- Sheet prefix emission is also empty. `parse_wrap_GoogleSheetsParser_sheet_prefix`
  has the same no-branch `match first` shape and a Span layout. See expanded
  lines 3424-3482. This explains failure to parse `=Sheet1!A1:B2` and the
  sheet-prefixed range serializer failures.

- HRegex span leaves are not role-specialized. `cell_ref` and `identifier`
  generated bodies both call the trait method `push_leaf_with_str`; they do not
  call `SheetsStructBuilder::push_leaf_cell_ref` or
  `SheetsStructBuilder::push_leaf_identifier`. See expanded lines 3508-3581.
  The runtime builder already documents that the generic trait method deposits
  `SheetsValue::String`, while specialized methods are required for cell,
  identifier, and sheet-prefix variants.

- Keyword StructDirect emission still routes typed `U8` keyword rules through
  `push_leaf_with_unit`. Expanded compare/unary/mul/add keyword functions call
  `builder.push_leaf_with_unit()` for operators. See expanded lines 3583-3975.
  This maps directly to the debug assertion in the Sheets builder and explains
  `operator_branches_parse`, unary parity, and unary serializer failures.

- String parsing strips quotes before depositing `SheetsValue::String`. Expanded
  `parse_string_GoogleSheetsParser_string` slices only the body and calls
  `builder.push_leaf_with_str(body)`. See expanded lines 2806-2882. Runtime
  `write_value` writes `SheetsValue::String(s)` verbatim, so quoted strings
  serialize as bare identifiers/text. This explains the corpus non-idempotency
  and `serialize_roundtrip_string_empty`.

- Array literals do reach a generated parser arm, but generated
  `parse_flat_GoogleSheetsParser_array_literal` delegates the contents to
  `parse_pratt_GoogleSheetsParser_array_rows`. See expanded lines 6260-6325.
  The focused test output shows `={1,2,3}` and `={1,2;3,4}` failing at offset 1,
  so the array-lane owner should inspect Pratt mining/selection for list
  separators before touching runtime serialization.

## Source Evidence

- `crates/core/src/backend/rust/emitter/shapes/keyword/struct_direct.rs:45`
  through `83` hard-codes `TypeDesc::U8` to `builder.push_leaf_with_unit()` and
  explicitly says non-JSON U8 keyword routing was deferred. Sheets is now active
  on StructDirect, so that old assumption is the current S1 operator failure.

- `crates/core/src/backend/rust/emitter/shapes/hregex.rs:800` through `922`
  only has generic scalar/Span routing. It does not branch on the originating
  grammar/rule to call Sheets-specific typed-span methods.

- `crates/core/src/runtime/google_sheets/builder.rs:262` through `280` deposits
  generic `push_leaf_with_str` as `SheetsValue::String`, while lines 324-354
  provide the needed specialized methods: `push_leaf_cell_ref`,
  `push_leaf_identifier`, `push_leaf_sheet_prefix`, and `push_leaf_error`.

- `crates/core/src/runtime/google_sheets/document.rs:169` through `177` writes
  `String`, `CellRef`, `Identifier`, and `SheetPrefix` verbatim and only renders
  error/operator lexemes from `Error`/`Tag` variants. This is correct for cell
  and identifier spans but wrong for quoted string bodies unless the string
  value retains quotes or the serializer re-quotes string bodies.

## Failure Ownership

| Lane | Reproduced failures | Likely owner |
|---|---:|---|
| Bool/error literal payloads | 10 | StructDirect keyword/flat payload emission plus Sheets error-specialized runtime call |
| Operator/unary unit panic | 4 total across both test files | `crates/core/src/backend/rust/emitter/shapes/keyword/struct_direct.rs` |
| Sheet-prefixed/range admission | 4 total across both test files | StructDirect wrap/HRegex emission for `sheet_prefix` and range-end routing |
| String/corpus serializer | 4 direct symptoms, corpus-wide impact | `crates/core/src/runtime/google_sheets/document.rs` plus string emission contract |
| Array literal admission | 2 | Pratt/list separator classification for `array_row` / `array_rows`, likely emitter precedence mining or shape selection |

## Proposed Diff Sketch

No source patch was applied. The next source-redress wave should patch in this
order:

1. In `keyword/struct_direct.rs`, replace the U8/unit assumption with payload
   preserving StructDirect emission:
   - For `TypeDesc::U8` with a branch payload, emit
     `builder.push_branch_tag(payload as u32)` for operator-style rules or a
     grammar/rule-specific specialized call for Sheets `error_literal`.
   - Keep JSON `null` on `push_leaf_with_unit` only when the rule is actually
     unit/null-shaped, not merely `U8`.

2. Add a grammar/rule aware Sheets specialization layer in StructDirect HRegex
   and keyword/flat emission:
   - `cell_ref` -> `builder.push_leaf_cell_ref(span)`.
   - `identifier` -> `builder.push_leaf_identifier(span)`.
   - `sheet_prefix` -> `builder.push_leaf_sheet_prefix(tag, span)`.
   - `error_literal` -> `builder.push_leaf_error(tag)`.
   This can live as a small helper selected by `(grammar_suffix, rule_name,
   TypeDesc)` to avoid broad runtime conditionals.

3. Fix generated `boolean` and `sheet_prefix` wrap bodies so regex/literal-led
   branches are actually emitted. Their current expanded bodies have no branch
   arms despite grammar branches, so the cause is likely before runtime in
   wrap/keyword shape selection or type inference for regex-led `Map` branches.

4. Preserve quoted string roundtrip semantics:
   - Either make `parse_string_GoogleSheetsParser_string` deposit the full
     matched quoted span, or make `SheetsValue::String` serializer quote and
     escape the stored body. The current hybrid stores the body but writes it
     as source text.

5. Investigate array list separator routing after the payload fixes:
   - `array_literal` delegates to `parse_pratt_GoogleSheetsParser_array_rows`;
     focused failures happen at offset 1 immediately after `{`, so confirm
     whether Pratt shape selection is appropriate for comma/semicolon list
     separators or whether arglist/flat emission should own `array_row` and
     `array_rows`.

## Hard Gate

Focused Sheets failure evidence is saved in this document with exact command
summaries. Disposition is HALT/READY: halt source redress in this worktree, and
ready the S1 plan amendment/source-owner wave.
