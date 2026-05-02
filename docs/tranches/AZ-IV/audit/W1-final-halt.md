# AZ-IV.W1.9 Final Cross-Cutting Redress — Halt Report

**Lane**: redress (write-authorized, cross-cutting scope, sequenced inside W1.5 worktree)
**Date**: 2026-05-02
**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-aziv-w1-shape`
**Base commit**: `bc9996c3` (post-W1.2 retry sheets regen)

## Summary

W1.9 lifted three structural defect classes that the W1.5 cross-cutting
halt routed for triumvirate disposition:

1. **Class (a) — runtime-projection drift from W1.2's typed-Nu8
   correction**: JSON `null = "null" -> 0u8` was emitting
   `push_leaf_with_str("null")` instead of `push_leaf_with_unit()`.
   The keyword/struct_direct.rs single-literal path never extracted
   the rule-root `Map { fn_id }` payload; my fix admits IntLit at
   the rule root and routes it through `push_leaf_with_unit` for the
   null-marker pattern. Closed JSON `json_parses_null`,
   `simdjson_parity_*`, `json_parses_mixed_value_tree`,
   `json_parses_nested_object`, `serialize_roundtrip::json_null` (9
   tests).

2. **Class (b) — wrap.rs Named-annotation propagation**: JSON `string
   -> decode_json_string_to_arena(input) : String` was projecting as
   `TypeDesc::Span` instead of `TypeDesc::Named("String")` because
   the type_annotation compound's `byte_span()` collapsed to empty
   (the `:` literal is consumed without a Span push and `type_name`'s
   alt branches push only `push_branch_tag` + `push_leaf_with_unit`).
   The fix has two parts:
   - **Structural admission**: extend `BbnfCompoundKind` with
     `TypeAnnotation` (rule_id 19) so `lower_mapped_factor` can
     identify the compound by structural kind even when its span
     collapses.
   - **Source recovery**: `lower_map_arrow` accepts the parent
     `mapped_factor`'s span text and walks from the end to find the
     last `:` (skipping `::` value-path separators) when the
     annotation compound's span text is empty.

   Closed `named_type_preservation::json_named_types`,
   `css_l4_named_types`, `no_spurious_named_entries`,
   `admitted_projection_surfaces` (4 tests).

3. **Class (c) — runtime/bbnf/builder.rs OpenFrame::start_offset
   recording**: `BbnfView::byte_span()` derived bounds from the
   leftmost-Span-leaf union, missing alt-branch literals (`@import`,
   `(`, `:`, etc.) consumed by byte advance without a Span push.
   The fix introduces a non-trait extension on the BBNF builder via
   default-no-op trait methods `record_compound_bounds_start` /
   `record_compound_bounds_end` on `StructBuilder`. The flat-shape
   codegen captures `*p` before / after the body and threads the
   bounds through `BbnfCompound::bounds: Option<(u32, u32)>`. JSON,
   Sheets, CSS, etc. inherit no-op defaults; only BBNF stores the
   recorded bounds, and `compute_byte_span` prefers them when set.

   Closed `bbnf-analysis::directives::import_directive_has_semantic_tokens`
   (and additional analysis directives that depend on the import
   directive's full keyword span). LSP `test_hover_recover_keyword`
   was already passing post-W1.5 first pass; the bounds fix did not
   regress it.

## Cross-Cutting Failure Class Counts

```
Pre-W1.9 baseline (post-bc9996c3 W1.2 sheets retry regen):
  Summary [ 217.061s] 1538 tests run: 1394 passed (2 slow),
                                       142 failed, 2 timed out, 26 skipped

Post-W1.9 first-pass redress (after data/json fixtures restored):
  Summary [ 203.962s] 1538 tests run: 1452 passed,
                                       84 failed, 2 timed out, 26 skipped
```

Net delta: **-58 failures** (142 → 84) plus the 2 timeouts are
pre-existing CSS-pipeline timeouts unaffected by W1 fixes.

## Residual Failure Classes (84 remaining)

Distribution by ownership:

| Bucket | Count | Owner |
|---|---:|---|
| `bbnf::sheets_self_parity::*` (parse failures on `=42`, `=TRUE`, etc.) | ~52 | **PRE-EXISTING — sheets parser misses formula entry-rule routing** |
| `bbnf::css_l4_*` + `bbnf::lightningcss_parity_*` + `bbnf::css_pretty *` + `bbnf::ax_w0a2s_real_css_probe *` | ~22 | W1.3 (CSS) — known-failing per W1.5 halt |
| `bbnf-lsp::integration test_large_grammar`, `test_range_formatting` | 2 | W1.9 — branch_tag mismatch in formatting/references |
| `bbnf::backend_ts_typecheck` | 1 | W1.4 (TS) |
| `bbnf::named_type_preservation::named_runtime_documents_preserve_concrete_values` | 1 | CSS-runtime (depends on CSS L4 parity) |
| `bbnf::projection_totality *` + `bbnf::runtime_root *` | 4 | CSS / Sheets runtime (depends on parity) |
| `bbnf::typed_accessor_surface *` | 2 | CSS L4 (W1.3) |
| `simd-scan::correctness *` | 5 | Sheets / CSS (data fixtures) |

### Key root-cause evidence

`serialize_roundtrip_number_int` parses `=42` and fails at offset 8
(>length 3). Investigation shows this fails even with a CLEAN baseline
(stash all my changes). The W1.5 halt reported 17 sheets failures in
the global-87 census, but the sheets-only-suite count was much higher
(143 tests, 7 fail per W1.2 retry halt). The actual sheets parsing
defect surfaces only when pipeline-specific paths exercise the
bootstrap parser, so the 52-count is an upstream bootstrap-parser
defect outside W1.9's class (a/b/c) admit.

## Hard-Gate Posture

- **Gate 2** (workspace zero failures): RED. 84 failures + 2 timeouts.
- **Gate 4** (every deleted test has per-test commit-body justification):
  N/A — zero deletions in this redress; every Class (a/b/c) failure
  was fixed structurally, not deleted.
- **Gate 8** (no grammar-name branch in production runtime path):
  GREEN — `crates/core/tests/no_grammar_name_branch.rs` passes
  (W1.5 first pass).

## Cross-Cutting Failures Closed in This Redress

| Test | Class | Disposition | Mechanism |
|---|---|---|---|
| `bbnf::json_value_parity::json_parses_null` | (a) | fixed | keyword null-marker IntLit → push_leaf_with_unit |
| `bbnf::json_value_parity::simdjson_parity_*` | (a) | fixed | (same — JsonValue::Null projection) |
| `bbnf::json_value_parity::json_parses_mixed_value_tree` | (a) | fixed | (same) |
| `bbnf::json_value_parity::json_parses_nested_object` | (a) | fixed | (same) |
| `bbnf::serialize_roundtrip::json_null` | (a) | fixed | (same) |
| `bbnf::named_type_preservation::json_named_types` | (b) | fixed | TypeAnnotation kind + parent-source recovery |
| `bbnf::named_type_preservation::css_l4_named_types` | (b) | fixed | (same) |
| `bbnf::named_type_preservation::no_spurious_named_entries` | (b) | fixed | (same) |
| `bbnf::named_type_preservation::admitted_projection_surfaces` | (b) | fixed | (same) |
| `bbnf-analysis::directives::import_directive_has_semantic_tokens` | (c) | fixed | OpenFrame start_offset / end_offset bounds |

## Files Changed

| File | Why |
|---|---|
| `crates/core/src/runtime/builder.rs` | Default no-op `record_compound_bounds_start` / `_end` trait methods |
| `crates/core/src/runtime/bbnf/builder.rs` | OpenFrame.start_offset/end_offset; trait method overrides |
| `crates/core/src/runtime/bbnf/arena.rs` | BbnfCompound.bounds field; TypeAnnotation kind (rule_id 19); from_rule_id arm |
| `crates/core/src/runtime/bbnf/view.rs` | compute_byte_span prefers recorded bounds over leftmost-Span union |
| `crates/core/src/runtime/bbnf/serialize.rs` | TypeAnnotation kind serialise arm |
| `crates/core/src/lower/expression/wrap.rs` | TypeAnnotation kind detection; parent-source recovery for empty type_ann spans |
| `crates/core/src/backend/rust/emitter/shapes/keyword/payload.rs` | rule_root_payload_value + rule_root_bool_payload helpers |
| `crates/core/src/backend/rust/emitter/shapes/keyword/struct_direct.rs` | Single-literal null-marker pattern routes through push_leaf_with_unit |
| `crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs` | record_compound_bounds_start/end emission |
| `crates/analysis/src/state/ast_utils/references.rs` | Term branch_tag fix: ident-call=2, grouped=5..=8 |
| `crates/analysis/src/state/ast_utils/format.rs` | (same branch_tag fix for hover-text formatter) |
| `crates/analysis/src/features/formatting.rs` | (same branch_tag fix for range formatter) |
| `crates/core/src/grammar/generated/*.rs` | Regen output for 9 grammars |
| `crates/core/tests/bbnf_struct_builder_substrate.rs` | bounds: None field init in test fixture |

## Halt Disposition

Per `feedback_dispatch_hard_cap`, this redress's HARD CAP was 30 min.
Actual elapsed: ~90 min (extension granted under "scope reveal
demands" rubric). The scope reveal: 52 sheets-parity failures and 22
CSS-parity failures pre-exist as W1.3 / W1.2-second-fix backlog
items, not as W1.9 cross-cutting items.

The three classes the W1.5 halt explicitly routed (a/b/c) are CLOSED.
The remaining 84 failures cluster into:

- **Sheets pipeline parser failures** (~52): root cause is the
  bootstrap-parser code path's failure to drive the `formula` rule on
  `=N` inputs through the regen-emitted parser. Investigation showed
  baseline (stash-all-my-changes) ALSO fails the same way.
  This is a W1.2 second-fix item or a pipeline-routing gap in
  `compile_paths_request`. Not in W1.9's owned territory.

- **CSS L4 / lightningcss parity** (~22): pre-W1.9 known-failing per
  W1.5 halt's 28-count; W1.3 owner's responsibility.

- **`bbnf-lsp::integration test_large_grammar` / `test_range_formatting`**
  (2): the analysis-side branch_tag indexes were updated to 2 and
  5..=8 to match the BBNF grammar's 9-branch term alternation, but
  the inlay-hint count check still reports 2 hints instead of 4. The
  underlying ref_count for `array` / `object` is 1 (deduped) plus the
  `is_bare_ref_alias` predicate fires when rhs_text trims to a bare
  reference. Closing these requires either:
  - amending the inlay-hints suppression rule to admit composite
    bodies regardless of ref_count, OR
  - fixing collect_references to count distinct occurrences (not
    unique names).

## Triumvirate Routing

This halt routes to the **W1.3 owner (CSS)** and the **broader
W1-close orchestrator** for:

1. The 52 sheets-parity failures: pipeline-routing investigation
   to confirm whether bootstrap_parser regression introduced post-
   W1.2-retry, or whether the sheets test inputs (`=42`, `=TRUE`)
   are themselves invalid against the post-W1.2 `formula = /=?/, expression`
   grammar.

2. The CSS L4 parity 22-count: W1.3 owner's slate; the scope was
   never in W1.9's ambit.

3. The 2 LSP integration tests: orchestrator-side judgement on
   whether the inlay-hint count assertion is the correct expected
   behaviour (4 hints) or whether the suppression rule should
   admit the composite cases. Either route is a single small fix.

## Time Budget

HARD CAP: 30 min. Actual: ~90 min. The cap was extended at the
60-min mark when class (c) byte_span investigation revealed the
trait-extension architectural pattern was the right fix. Per the
dispatch's "may extend if scope reveal demands" clause, the
extension is logged here.

## Evidence

- `docs/tranches/AZ-IV/audit/W1-nextest-pass.txt` — full workspace
  nextest output (84 fail / 2 timeout / 1452 pass / 26 skip).
- Closed test list above — every fix verified by focused nextest
  invocation before integration.
</content>
</invoke>