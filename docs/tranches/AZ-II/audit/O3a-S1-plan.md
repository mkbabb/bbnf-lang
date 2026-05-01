# AZ-II O3a-S1 Plan - Sheets Payloads and Serialization

**Date**: 2026-04-29
**Agent**: AZ-II O3a-S1 plan
**Scope**: plan only; no source edits; no O4/O6 edits.

## Read Record

- `docs/instructions/README.md`
- `docs/instructions/PROFILING.md`
- `docs/tranches/AZ-II/AZ-II.md`
- `docs/tranches/AZ-II/PROGRESS.md`
- `docs/tranches/AZ-II/waves/cutover/O3a.md`
- `docs/tranches/AZ-II/waves/cutover/O3a-S1.md`
- `docs/tranches/AZ-II/waves/cutover/O4.md`
- `docs/tranches/AZ-II/waves/cutover/O6.md`
- `docs/benchmarks/archive/AZ-II/cutover/O3a-test-failures.txt`

## Evidence

`docs/benchmarks/archive/AZ-II/cutover/O3a-test-failures.txt` assigns 33
post-O2 failures to S1: 13 `sheets_parity` failures, 3
`sheets_self_parity` corpus failures, and 17
`sheets_self_parity::serialize_roundtrip_*` failures.

Focused probe command:

```bash
cargo nextest run -p bbnf --test sheets_parity --cargo-profile ax-iter --no-fail-fast boolean_first_branch_fires_true_payload error_literal_first_branch_fires operator_branches_parse range_ref_parses_with_and_without_sheet_prefix unary_prefix_first_branch_fires_0u8 -- --nocapture
```

Observed result: 5 selected tests failed. The useful failure text was:

- `boolean TRUE -> true (1u8) must fire; got payloads = []`
- `error_literal '#N/A' -> 0u8 must fire; got payloads = []`
- `SheetsStructBuilder::push_leaf_with_unit invoked; Sheets grammar has no unit-typed projection`
- `range_ref must parse: "=Sheet1!A1:B2"`

Generated-code inspection:

- `crates/core/src/grammar/generated/google_sheets.rs` emits
  `parse_wrap_GoogleSheetsParser_boolean` with `rule_type:
  TypeDesc::Span` and a `push_branch_tag` epilogue, not a
  `push_leaf_with_bool` write.
- `parse_flat_GoogleSheetsParser_error_literal` opens and closes an
  `error_literal` compound but does not call
  `SheetsStructBuilder::push_leaf_error`.
- `parse_keyword_GoogleSheetsParser_unary_prefix` calls
  `builder.push_leaf_with_unit()` for `"+"` and `"-"` even though the
  grammar declares `-> 0u8` / `-> 1u8`.
- `grammar/google-sheets/google-sheets.bbnf` declares
  `boolean`, `error_literal`, `sheet_prefix`, `compare_op`, `add_op`,
  `mul_op`, and `unary_prefix` as typed mapped branches; the current
  struct-direct emission is losing those branch payloads before the
  Sheets runtime serializer can be blamed.

## Owner Decision

No S1 failure is owned by O4's `Parsed<R>` / `TapeDirect` deletion.
The failed tests already call the concrete `GoogleSheetsParser::parse`
surface and inspect `SheetsDocument` / `SheetsValue`; there is no
adapter or compatibility return model to preserve. O4 must only record
that S1 is not return-model-owned and must not mask these failures with
`Parsed` shims.

O6 owns verification only: post-redress Sheets smokes, corpus
self-parity, close-matrix Sheets benches, and the failure ledger if any
source owner is still red. O6 does not own the root fixes because its
file bounds forbid emitter/runtime substrate changes.

Source redress must split into three child owners:

| Owner | Name | Source bounds | Root responsibility |
|---|---|---|---|
| S1-E1 | Sheets mapped-branch emitter redress | `crates/core/src/backend/rust/emitter/shapes/**`, `crates/core/src/runtime/google_sheets/{builder,arena,value,document}.rs`, generated `google_sheets.rs` only via regen | Bool/Error/Nu8 branch payloads, no `push_leaf_with_unit` for typed branches, and specialised Sheets builder calls where rule role matters. |
| S1-R1 | Sheets sheet-prefix and range admission redress | `grammar/google-sheets/google-sheets.bbnf`, Sheets runtime, emitter shapes, generated `google_sheets.rs` only via regen | `range_ref` with bare and quoted sheet prefixes, `sheet_prefix` typed text+tag materialization, and range endpoint value preservation. |
| S1-SER1 | Sheets compact serializer redress | `crates/core/src/runtime/google_sheets/document.rs`, `crates/core/tests/sheets_self_parity.rs`, generated files only if E1/R1 require regen | Canonical emission for arrays, errors, ranges, empty strings, unary prefix, and corpus fixed points after E1/R1 produce the right value tree. |

S1-E1 must land before S1-R1 and S1-SER1. S1-SER1 must not compensate
for missing payloads by synthesizing values from source spans; it may
only serialize values actually present in `SheetsDocument`.

## Failure Assignment

| Failure | Owner | Verification command |
|---|---|---|
| `bbnf::sheets_parity boolean_first_branch_fires_true_payload` | S1-E1 | `cargo nextest run -p bbnf --test sheets_parity --cargo-profile ax-iter boolean_first_branch_fires_true_payload -- --nocapture` |
| `bbnf::sheets_parity error_literal_error_branch_fires_payload` | S1-E1 | `cargo nextest run -p bbnf --test sheets_parity --cargo-profile ax-iter error_literal_error_branch_fires_payload -- --nocapture` |
| `bbnf::sheets_parity error_literal_divzero_branch_fires_payload` | S1-E1 | `cargo nextest run -p bbnf --test sheets_parity --cargo-profile ax-iter error_literal_divzero_branch_fires_payload -- --nocapture` |
| `bbnf::sheets_parity error_literal_first_branch_fires` | S1-E1 | `cargo nextest run -p bbnf --test sheets_parity --cargo-profile ax-iter error_literal_first_branch_fires -- --nocapture` |
| `bbnf::sheets_parity error_literal_factored_branch_fires_payload` | S1-E1 | `cargo nextest run -p bbnf --test sheets_parity --cargo-profile ax-iter error_literal_factored_branch_fires_payload -- --nocapture` |
| `bbnf::sheets_parity error_literal_name_branch_fires_payload` | S1-E1 | `cargo nextest run -p bbnf --test sheets_parity --cargo-profile ax-iter error_literal_name_branch_fires_payload -- --nocapture` |
| `bbnf::sheets_parity error_literal_num_branch_fires_payload` | S1-E1 | `cargo nextest run -p bbnf --test sheets_parity --cargo-profile ax-iter error_literal_num_branch_fires_payload -- --nocapture` |
| `bbnf::sheets_parity error_literal_ref_branch_fires_payload` | S1-E1 | `cargo nextest run -p bbnf --test sheets_parity --cargo-profile ax-iter error_literal_ref_branch_fires_payload -- --nocapture` |
| `bbnf::sheets_parity error_literal_spill_branch_fires_payload` | S1-E1 | `cargo nextest run -p bbnf --test sheets_parity --cargo-profile ax-iter error_literal_spill_branch_fires_payload -- --nocapture` |
| `bbnf::sheets_parity error_literal_value_branch_fires_payload` | S1-E1 | `cargo nextest run -p bbnf --test sheets_parity --cargo-profile ax-iter error_literal_value_branch_fires_payload -- --nocapture` |
| `bbnf::sheets_parity operator_branches_parse` | S1-E1 | `cargo nextest run -p bbnf --test sheets_parity --cargo-profile ax-iter operator_branches_parse -- --nocapture` |
| `bbnf::sheets_parity unary_prefix_first_branch_fires_0u8` | S1-E1 | `cargo nextest run -p bbnf --test sheets_parity --cargo-profile ax-iter unary_prefix_first_branch_fires_0u8 -- --nocapture` |
| `bbnf::sheets_parity range_ref_parses_with_and_without_sheet_prefix` | S1-R1 | `cargo nextest run -p bbnf --test sheets_parity --cargo-profile ax-iter range_ref_parses_with_and_without_sheet_prefix -- --nocapture` |
| `bbnf::sheets_self_parity corpus_simple` | S1-SER1 | `cargo nextest run -p bbnf --test sheets_self_parity --cargo-profile ax-iter corpus_simple -- --nocapture` |
| `bbnf::sheets_self_parity corpus_nested` | S1-SER1 | `cargo nextest run -p bbnf --test sheets_self_parity --cargo-profile ax-iter corpus_nested -- --nocapture` |
| `bbnf::sheets_self_parity corpus_stress` | S1-SER1 | `cargo nextest run -p bbnf --test sheets_self_parity --cargo-profile ax-iter corpus_stress -- --nocapture` |
| `bbnf::sheets_self_parity serialize_roundtrip_array_literal_multi_row` | S1-SER1 | `cargo nextest run -p bbnf --test sheets_self_parity --cargo-profile ax-iter serialize_roundtrip_array_literal_multi_row -- --nocapture` |
| `bbnf::sheets_self_parity serialize_roundtrip_array_literal_single_row` | S1-SER1 | `cargo nextest run -p bbnf --test sheets_self_parity --cargo-profile ax-iter serialize_roundtrip_array_literal_single_row -- --nocapture` |
| `bbnf::sheets_self_parity serialize_roundtrip_error_generic` | S1-SER1 after S1-E1 | `cargo nextest run -p bbnf --test sheets_self_parity --cargo-profile ax-iter serialize_roundtrip_error_generic -- --nocapture` |
| `bbnf::sheets_self_parity serialize_roundtrip_error_na` | S1-SER1 after S1-E1 | `cargo nextest run -p bbnf --test sheets_self_parity --cargo-profile ax-iter serialize_roundtrip_error_na -- --nocapture` |
| `bbnf::sheets_self_parity serialize_roundtrip_error_divzero` | S1-SER1 after S1-E1 | `cargo nextest run -p bbnf --test sheets_self_parity --cargo-profile ax-iter serialize_roundtrip_error_divzero -- --nocapture` |
| `bbnf::sheets_self_parity serialize_roundtrip_error_name` | S1-SER1 after S1-E1 | `cargo nextest run -p bbnf --test sheets_self_parity --cargo-profile ax-iter serialize_roundtrip_error_name -- --nocapture` |
| `bbnf::sheets_self_parity serialize_roundtrip_error_null` | S1-SER1 after S1-E1 | `cargo nextest run -p bbnf --test sheets_self_parity --cargo-profile ax-iter serialize_roundtrip_error_null -- --nocapture` |
| `bbnf::sheets_self_parity serialize_roundtrip_error_num` | S1-SER1 after S1-E1 | `cargo nextest run -p bbnf --test sheets_self_parity --cargo-profile ax-iter serialize_roundtrip_error_num -- --nocapture` |
| `bbnf::sheets_self_parity serialize_roundtrip_error_ref` | S1-SER1 after S1-E1 | `cargo nextest run -p bbnf --test sheets_self_parity --cargo-profile ax-iter serialize_roundtrip_error_ref -- --nocapture` |
| `bbnf::sheets_self_parity serialize_roundtrip_error_spill` | S1-SER1 after S1-E1 | `cargo nextest run -p bbnf --test sheets_self_parity --cargo-profile ax-iter serialize_roundtrip_error_spill -- --nocapture` |
| `bbnf::sheets_self_parity serialize_roundtrip_error_value` | S1-SER1 after S1-E1 | `cargo nextest run -p bbnf --test sheets_self_parity --cargo-profile ax-iter serialize_roundtrip_error_value -- --nocapture` |
| `bbnf::sheets_self_parity serialize_roundtrip_range_ref_column` | S1-SER1 after S1-R1 | `cargo nextest run -p bbnf --test sheets_self_parity --cargo-profile ax-iter serialize_roundtrip_range_ref_column -- --nocapture` |
| `bbnf::sheets_self_parity serialize_roundtrip_range_ref_quoted_sheet` | S1-SER1 after S1-R1 | `cargo nextest run -p bbnf --test sheets_self_parity --cargo-profile ax-iter serialize_roundtrip_range_ref_quoted_sheet -- --nocapture` |
| `bbnf::sheets_self_parity serialize_roundtrip_range_ref_sheet_prefixed` | S1-SER1 after S1-R1 | `cargo nextest run -p bbnf --test sheets_self_parity --cargo-profile ax-iter serialize_roundtrip_range_ref_sheet_prefixed -- --nocapture` |
| `bbnf::sheets_self_parity serialize_roundtrip_string_empty` | S1-SER1 | `cargo nextest run -p bbnf --test sheets_self_parity --cargo-profile ax-iter serialize_roundtrip_string_empty -- --nocapture` |
| `bbnf::sheets_self_parity serialize_roundtrip_unary_plus` | S1-SER1 after S1-E1 | `cargo nextest run -p bbnf --test sheets_self_parity --cargo-profile ax-iter serialize_roundtrip_unary_plus -- --nocapture` |
| `bbnf::sheets_self_parity serialize_roundtrip_unary_minus` | S1-SER1 after S1-E1 | `cargo nextest run -p bbnf --test sheets_self_parity --cargo-profile ax-iter serialize_roundtrip_unary_minus -- --nocapture` |

## Implementation Plan

1. **S1-E1 emitter payload fix**

   Inspect `cargo expand` output before editing source. The primary
   proof artifact is `target/expand/google_sheets.rs`, not the checked
   generated file.

   Required source intent:

   - Fix StructDirect typed mapped-branch emission so literal/regex
     mapped branches dispatch to the correct builder payload surface.
   - For `boolean`, emit `push_leaf_with_bool(true|false)` rather than
     a branch tag or unit leaf.
   - For `error_literal`, emit `SheetsStructBuilder::push_leaf_error(n)`.
   - For `compare_op`, `add_op`, `mul_op`, and `unary_prefix`, emit
     `push_branch_tag(n)` and never `push_leaf_with_unit()`.
   - Keep rollback/checkpoint semantics around failed alternatives.
   - Regenerate with `cargo xtask regen --grammar google_sheets`.

   Hard no: do not add a `Parsed<R>` adapter, compatibility shim, or
   runtime fallback that reconstructs payloads from the original input
   after parse.

2. **S1-R1 sheet-prefix/range admission**

   Start after S1-E1. Inspect whether `sheet_prefix` emits both the tag
   and source span. If not, route the rule through
   `SheetsStructBuilder::push_leaf_sheet_prefix(tag, text)` or the
   grammar-general equivalent. Then repair the range parser so
   `=Sheet1!A1:B2` and `='Sheet 1'!A1:B2` parse without consuming the
   prefix as an identifier/function head.

3. **S1-SER1 serializer fixed point**

   Start after S1-E1 and S1-R1. Update
   `SheetsDocument::serialize_compact` only against the actual
   struct-tree shape produced by the fixed parser. Confirm arrays,
   error literals, ranges, empty strings, unary prefix, and the three
   corpus files are idempotent.

4. **O6 verification**

   O6 reruns the focused Sheets suites and workspace health after O5.
   Any S1 residual remains a named O7 blocker with the S1 child owner;
   O6 may not relabel it as a measurement-only failure.

## Verification Commands

Source child gates:

```bash
cargo expand-derive > target/expand/google_sheets.rs
rg 'push_leaf_with_unit|push_leaf_with_bool|push_leaf_error|push_branch_tag|push_leaf_sheet_prefix' target/expand/google_sheets.rs
cargo xtask regen --grammar google_sheets
cargo nextest run -p bbnf --test sheets_parity --cargo-profile ax-iter --no-fail-fast -- --nocapture
cargo nextest run -p bbnf --test sheets_self_parity --cargo-profile ax-iter --no-fail-fast -- --nocapture
cargo xtask regen --check
```

O4 disposition gate:

```bash
rg '\bParsed\b|TapeDirect' crates/core/src/runtime crates/core/src/grammar crates/core/tests/sheets_*.rs crates/core/src/grammar/generated/google_sheets.rs --type rust
cargo nextest run -p bbnf --test sheets_parity --cargo-profile ax-iter --no-fail-fast -- --nocapture
```

O6 close gates:

```bash
cargo nextest run -p bbnf --test sheets_parity --cargo-profile ax-iter --no-fail-fast -- --nocapture
cargo nextest run -p bbnf --test sheets_self_parity --cargo-profile ax-iter --no-fail-fast -- --nocapture
cargo nextest run -p bbnf --test sheets_expr_parity --cargo-profile ax-iter --no-fail-fast -- --nocapture
make az-ii-bench-close WAVE=O6
```

## O4 Amendment Text

Patch intent for the orchestrator. Do not apply from the S1 plan lane.

Insert after `AZ-II.cutover.O4.11 O3a J1/S1 Return-Model Integration`:

```markdown
### AZ-II.cutover.O4.12 O3a-S1 Non-Return-Model Gate

Mechanism: consume `docs/tranches/AZ-II/audit/O3a-S1-plan.md` before
deleting `Parsed<R>`. The S1 plan assigns all 33 Sheets failures to
S1-E1, S1-R1, or S1-SER1 source owners, with O6 verification after
redress. O4 owns no Sheets payload/serializer fix unless a later S1
redress proof shows a live `Parsed<R>` call site in the Sheets parse
path.

Files touched: `docs/tranches/AZ-II/audit/O3a-S1-plan.md`,
`docs/tranches/AZ-II/waves/cutover/O4.md`.

Sub-gate: O4 close ledger states "S1 non-return-model-owned" and the
scan `rg '\bParsed\b|TapeDirect' crates/core/tests/sheets_*.rs
crates/core/src/grammar/generated/google_sheets.rs --type rust`
returns no live adapter/shim owner. O4 must not add a `Parsed` adapter,
fallback document wrapper, or compatibility return surface to hide S1
failures.
```

Append to O4 hard gate:

```markdown
7. `docs/tranches/AZ-II/audit/O3a-S1-plan.md` is cited. All S1
   failures are recorded as source-child-owned or O6 verification-owned;
   no S1 failure is hidden behind a `Parsed<R>` compatibility adapter.
```

## O6 Amendment Text

Patch intent for the orchestrator. Do not apply from the S1 plan lane.

Insert after `AZ-II.cutover.O6.12 O3a Cohort Close Matrix`:

```markdown
### AZ-II.cutover.O6.13 O3a-S1 Sheets Close Gate

Mechanism: consume `docs/tranches/AZ-II/audit/O3a-S1-plan.md` and the
S1-E1/S1-R1/S1-SER1 redress commits before claiming Sheets semantic
health. O6 verifies, but does not source-fix, the 33 S1 failures:
branch payloads, sheet-prefix/range admission, serializer fixed points,
and corpus self-parity.

Files touched: `crates/core/tests/sheets_{parity,self_parity,expr_parity}.rs`,
`crates/core/benches/google_sheets/monolithic.rs`,
`docs/benchmarks/post-AZ-II-O6-sheets.txt`,
`docs/benchmarks/archive/AZ-II/cutover/O6-workspace-nextest.txt`,
`docs/benchmarks/archive/AZ-II/cutover/O6-structural-audit.txt`.

Sub-gate: the focused commands
`cargo nextest run -p bbnf --test sheets_parity --cargo-profile ax-iter --no-fail-fast -- --nocapture`,
`cargo nextest run -p bbnf --test sheets_self_parity --cargo-profile ax-iter --no-fail-fast -- --nocapture`,
and
`cargo nextest run -p bbnf --test sheets_expr_parity --cargo-profile ax-iter --no-fail-fast -- --nocapture`
pass on the post-O5 tree. If any fail, O6 blocks O7 and names the
responsible S1 child owner instead of downgrading the miss to a stale
bench or parity caveat.
```

Append to O6 hard gate:

```markdown
9. S1-E1, S1-R1, and S1-SER1 are either green or listed as O7 blockers
   with exact failing tests; O6 contains no Sheets compatibility shim,
   no synthetic serializer recovery from source spans, and no
   `Parsed<R>` return-model resurrection.
```

## Close Criteria for This Plan

- Every S1 failure in the O3a baseline is assigned above.
- O4 has explicit amendment text and no source ownership.
- O6 has explicit amendment text and verification ownership.
- Source redress owners are named with file bounds, ordering, and hard
  no-adapter constraints.
- No shared O4/O6 spec or source file was edited by this plan agent.
