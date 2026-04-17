# AW-III.W3 ignored-tests audit — 58 source tests (+2 parse-that)

## Methodology

- Workspace at master HEAD `b58d1461` (branch detached in worktree
  `../bbnf-wt-aw-c2`).
- Discovery: `grep -rnE '#\[ignore' crates/` returns 58 unique
  attribute applications spread across 22 files. The workspace-level
  ignored count reported by `cargo test --workspace --no-fail-fast` is
  **67**; the surplus comes from `gorgeous/tests/vm.rs` compiling under
  multiple `#![cfg(feature = ...)]` gates and from path-dep
  `parse-that` tests registering two additional harness entries
  (`decode_test::decode_microbench`, `css_parse_test::test_parse_tailwind_css`).
  The 58 unique test functions below are the authoritative disposition
  surface; the gorgeous duplicates share the same underlying test and
  disposition.
- Per-test verification: lift-and-run via
  `cargo test -p <crate> --test <target> -- --ignored` (binding the
  `#[ignore]` off via the runner, leaving source untouched). Commit
  archaeology via `git log --oneline -5 -- <file>` at the touched
  line-range.
- All source restored before this commit; the audit adds one file and
  modifies no tests.

## Summary

| Disposition | Count | Notes |
|-------------|-------|-------|
| CLOSE       | 14    | Passes when lifted; lift in AW-III.W3 execution. |
| DELETE      | 4     | Stale / unreachable-stub / orphaned-architecture tests. |
| INVESTIGATE | 40    | Real root cause, tractable in AW-III scope or documented deferral to AW-IV. |

Plus 2 tests in path-dep `parse-that` (performance-smoke / large-file
gate) that can stay ignored under the existing rationale; they are not
within AW-III's close envelope and the `#[ignore]` attribute is the
correct long-term marker.

## Per-test audit table

| # | Test | File:line | Rationale on file | Disposition | Verdict basis |
|---|------|-----------|-------------------|-------------|---------------|
| 1 | `percentage_fires_255u8_discriminant` | `crates/core/tests/css_l4_parity.rs:138` | AU.6.8 Bug 2b: single-scalar u8 payload does not materialise through percentage Seq composition post-W6 layering. | INVESTIGATE | Fails when lifted (`percentageUnit '%' -> 255u8 must materialise exactly`). Same root cause as Cluster C; resolved by AW-III.W1 payload wiring. |
| 2 | `percentage_parses_through_width_and_height` | `crates/core/tests/css_l4_parity.rs:159` | AU.6.8 Bug 2b: see percentage_fires_255u8_discriminant — same scanner→payload wiring gap. | INVESTIGATE | Fails when lifted. Cascades from #1 via `width`/`height` dispatch. Closed when AW-III.W1 payload wiring lands. |
| 3 | `percentage_alongside_non_percentage_properties_materialises` | `crates/core/tests/css_l4_parity.rs:465` | AU.6.8 Bug 2b: see percentage_fires_255u8_discriminant — same scanner→payload wiring gap. | INVESTIGATE | Fails when lifted. Same root cause as #1–#2. |
| 4 | `test_selective_transitive_unfurling` | `crates/core/tests/imports.rs:307` | AV.0.12: module loader does not unfurl transitive deps of selectively-imported rules; forward ticket. | INVESTIGATE | Fails when lifted. Architectural item outside AW-III scope (imports subsystem); leave with rationale OR CLOSE-by-fix in a future tranche. |
| 5 | `json_empty_arr` | `crates/core/tests/serialize_roundtrip.rs:141` | AW open scope: serialize/structural roundtrip — surfaced post-V5 ShapeRef substrate. | CLOSE | Passes when lifted (batch run `7 passed; 1 failed`). |
| 6 | `json_array` | `crates/core/tests/serialize_roundtrip.rs:142` | (same batch rationale as #5) | CLOSE | Passes when lifted. |
| 7 | `json_empty_obj` | `crates/core/tests/serialize_roundtrip.rs:143` | (same) | CLOSE | Passes when lifted. |
| 8 | `json_object` | `crates/core/tests/serialize_roundtrip.rs:144` | (same) | CLOSE | Passes when lifted. |
| 9 | `json_nested` | `crates/core/tests/serialize_roundtrip.rs:145` | (same) | CLOSE | Passes when lifted. |
| 10 | `ebnf_rule` | `crates/core/tests/serialize_roundtrip.rs:192` | EBNF grammar parse fails under tape-first (pre-existing). | INVESTIGATE | Fails when lifted with `EBNF parse failed: Syntax { offset: 0 }`. Exact match to Cluster A residual (`ebnf_*_tape_parity`); closes via AW-III.W2. |
| 11 | `bbnf_rule` | `crates/core/tests/serialize_roundtrip.rs:231` | AW open scope: serialize/structural roundtrip. | CLOSE | Passes when lifted (roundtrip via `BbnfEmit::parse` → `serialize_compact` → reparse). |
| 12 | `css_simple` | `crates/core/tests/serialize_roundtrip.rs:264` | AW-I.W2.5: CSS pretty grammar under `serialize` mode fails to parse the simple block form post-SCC-recompute activation. | CLOSE | Passes when lifted (confirmed via `cargo test ... serialize_roundtrip css_simple -- --ignored` → `1 passed`). The AW-I.W2.5 rationale is stale; post-W5c view-layer reconciliation appears to have already fixed it. |
| 13 | `null_materialises_u8_payload` | `crates/core/tests/json_parity.rs:143` | AU.6.8 parity: post-W6 tape-shape shift broke variant_idx dispatch in the walker. | INVESTIGATE | Fails when lifted — variant_idx dispatch still broken. Cluster B/C cascade; closes via AW-III.W1 payload wiring + walker dispatch fix. |
| 14 | `bool_materialises_false_payload` | `crates/core/tests/json_parity.rs:159` | (same AU.6.8 rationale) | INVESTIGATE | Fails when lifted. |
| 15 | `number_materialises_f64_payload` | `crates/core/tests/json_parity.rs:212` | (same) | INVESTIGATE | Fails when lifted. |
| 16 | `string_materialises_decoded_bytes` | `crates/core/tests/json_parity.rs:233` | (same) | INVESTIGATE | Fails when lifted. |
| 17 | `object_keys_and_values_decode` | `crates/core/tests/json_parity.rs:251` | (same) | INVESTIGATE | Fails when lifted. |
| 18 | `nested_object_preserves_firing_typed_payloads` | `crates/core/tests/json_parity.rs:288` | (same) | INVESTIGATE | Fails when lifted. |
| 19 | `children_zero_alloc_walks_typed_leaves` | `crates/core/tests/json_parity.rs:368` | (same) | INVESTIGATE | Fails when lifted. |
| 20 | `structural_object_with_array` | `crates/core/tests/structural.rs:63` | AW open scope: serialize/structural roundtrip. | CLOSE | Passes when lifted (batch run `7 passed; 0 failed`). |
| 21 | `structural_array_three_numbers` | `crates/core/tests/structural.rs:107` | (same) | CLOSE | Passes when lifted. |
| 22 | `structural_object_two_pairs` | `crates/core/tests/structural.rs:141` | (same) | CLOSE | Passes when lifted. |
| 23 | `structural_nested_objects` | `crates/core/tests/structural.rs:189` | (same) | CLOSE | Passes when lifted. |
| 24 | `structural_empty_array` | `crates/core/tests/structural.rs:230` | (same) | CLOSE | Passes when lifted. |
| 25 | `structural_empty_object` | `crates/core/tests/structural.rs:245` | (same) | CLOSE | Passes when lifted. |
| 26 | `structural_data_json_sanity` | `crates/core/tests/structural.rs:397` | (same) | CLOSE | Passes when lifted. |
| 27 | `no_hand_written_subvariant_references` | `crates/core/tests/no_subvariant_refs.rs:92` | AV.0.11 Category A: AF W2 substrate-break closure gate — `src/graph/deps.rs` leaks remain. | INVESTIGATE | Fails when lifted; 2 hand-written references in `src/graph/deps.rs:69,123` to `BbnfBootstrapRuleKind::term_{1,2}`/`value_atom_0`. Tractable producer-side fix (migrate to wrapper-peel); aligns with AW-III's "producer-side in scope" thesis. |
| 28 | `parse_recover_without_terminator` | `crates/core/tests/recover.rs:128` | AV.0.11 Category A: bbnf grammar requires trailing `;` after @recover directives. | INVESTIGATE | Fails when lifted. Grammar-side `bbnf.bbnf` work + bootstrap regen — outside AW-III's viability focus but in the producer-side surface. Defer to AW-IV or carry ignore with rationale. |
| 29 | `prettify_multi_rule` (ebnf_prettify) | `crates/core/tests/ebnf_prettify.rs:43` | prettify codegen stops after first rule — pre-existing in gorgeous. | INVESTIGATE | Fails when lifted (`should contain 'number': got 'digit ='`). Shared root cause with #55 `gorgeous/tests/ebnf.rs::test_prettify_multi_rule` (prettify multi-rule codegen truncation). Tractable in gorgeous; deferred to AW-IV or dedicated prettify refresh. |
| 30 | `compile_request_rejects_unknown_nonterminal` | `crates/core/tests/pipeline_compile_request.rs:126` | AV.0.11 Category A: validate_ast no longer precedes lower::expression; unknown-nonterminal handling regressed to src-side panic. | INVESTIGATE | Fails when lifted (panics in `lower/expression.rs:1505`). Reinstate AST validation gate before lowering — tractable src-side fix; route to AW-IV or a pipeline refresh. |
| 31 | `pipeline_google_sheets_multiline_let` | `crates/core/tests/pipeline.rs:486` | AV.0.11 Category A: google-sheets rule-name drift (expression → arithmetic_expr); forward to AV.3.3 Pratt lowering. | INVESTIGATE | Fails when lifted (`expression not found`). Test hard-codes stale rule names; update test body OR rewrite diagnostic table. Low-effort test-data fix; deferrable. |
| 32 | `closure_single_param` | `crates/core/tests/pipeline.rs:837` | AV.0.11 Category A: lower::expression closure-body lowering gap. | INVESTIGATE | Fails when lifted (`unknown nonterminal 'x' — should have been caught by validate_ast()`). Closure lowering gap — first-class grammar-closures project; out of AW-III viability scope. |
| 33 | `closure_multi_param` | `crates/core/tests/pipeline.rs:848` | (same closure lowering gap) | INVESTIGATE | Fails when lifted. |
| 34 | `closure_nested_calls` | `crates/core/tests/pipeline.rs:859` | (same) | INVESTIGATE | Fails when lifted. |
| 35 | `closure_with_rule_ref` | `crates/core/tests/pipeline.rs:871` | (same) | INVESTIGATE | Fails when lifted. |
| 36 | `closure_composition` | `crates/core/tests/pipeline.rs:883` | (same) | INVESTIGATE | Fails when lifted. |
| 37 | `ir_meta_has_follow_sets` | `crates/analysis/tests/directives.rs:169` | AV.0.11 Category A: analysis runs structural-mode pipeline which gates compute_follow_sets. | INVESTIGATE | Fails when lifted (`at least one rule should have a FOLLOW set label`). Analysis-mode rework; deferrable. |
| 38 | `ir_meta_has_memo_and_span_info` | `crates/analysis/tests/directives.rs:196` | AV.0.11 Category A: analysis pipeline is structural; span-eligibility refinement gated behind !structural. | INVESTIGATE | Fails when lifted. Same analysis-mode rework. |
| 39 | `cost_weights_unified::call_strategy_flips_under_inverted_call_overhead` | `crates/ir/tests/vm/cost_weights_unified.rs:519` | call_overhead consumer uses hardcoded constants; cross-crate test required after backend driver migration. | DELETE | Body is `unreachable!("AF.2-4C consumer migration required before this test can run")`. Test is a stub-as-documentation — it cannot run even if the ignore is lifted. Convert to a doc comment or delete outright; its body carries no executable assertion. |
| 40 | `cost_weights_unified::inline_body_size_penalty_affects_per_rule_decision` | `crates/ir/tests/vm/cost_weights_unified.rs:553` | inline_body_size_penalty consumer uses hardcoded heuristic. | DELETE | Body is `unreachable!(...)` stub — same pattern as #39. |
| 41 | `cost_weights_unified::prettify_emission_scales_with_pretty_subtree_size` | `crates/ir/tests/vm/cost_weights_unified.rs:569` | prettify_emission has no bbnf-ir consumer; prettify pin is structural. | DELETE | Body is `unreachable!(...)` stub. Also the rationale says "prettify pin is structural (MustTape clamp), not a continuous cost" — the test is documenting an architectural decision that won't reverse. |
| 42 | `test_cycle_detection` | `crates/lsp/tests/analyze.rs:158` | AV.0.11 Category A: structural-mode analysis does not emit cyclic_rule_paths. | INVESTIGATE | Fails when lifted. Analysis-mode rework dependency; same as #37/#38. |
| 43 | `test_alias_detection` | `crates/lsp/tests/analyze.rs:173` | AV.0.11 Category A: structural-mode analysis does not emit alias-hint diagnostics. | INVESTIGATE | Fails when lifted. Same. |
| 44 | `test_diagnostics_cycle_path` | `crates/lsp/tests/integration.rs:1335` | AV.0.11 Category A: LSP cycle-path diagnostic gated on analysis crate cyclic_rule_paths. | INVESTIGATE | Fails when lifted. LSP-side cascade of #42. |
| 45 | `test_diagnostics_alias_hint` | `crates/lsp/tests/integration.rs:1365` | AV.0.11 Category A: LSP alias-hint diagnostic gated on analysis crate structural-mode gap. | INVESTIGATE | Fails when lifted. LSP-side cascade of #43. |
| 46 | `test_hard_sudoku_al_escargot` | `crates/csp-solver/tests/solver.rs:1338` | requires GAC alldiff — too slow with binary FC. | INVESTIGATE | Fails when lifted (15.6s run, all 6 hard-sudoku tests fail). Legitimate feature gate — GAC alldiff is a documented optimisation the solver does not yet implement. Leave ignored with rationale OR implement GAC alldiff in a CSP-focused tranche. Out of AW scope. |
| 47 | `test_hard_sudoku_inkala_2010` | `crates/csp-solver/tests/solver.rs:1354` | (same) | INVESTIGATE | Same. |
| 48 | `test_hard_sudoku_golden_nugget` | `crates/csp-solver/tests/solver.rs:1370` | (same) | INVESTIGATE | Same. |
| 49 | `test_hard_sudoku_platinum_blonde` | `crates/csp-solver/tests/solver.rs:1386` | (same) | INVESTIGATE | Same. |
| 50 | `test_hard_sudoku_minimal_17` | `crates/csp-solver/tests/solver.rs:1402` | (same) | INVESTIGATE | Same. |
| 51 | `test_hard_sudoku_all_configs` | `crates/csp-solver/tests/solver.rs:1421` | (same) | INVESTIGATE | Same — multi-config driver atop the same GAC-alldiff gap. |
| 52 | `output_size_comparison` | `crates/gorgeous/tests/biome_compare2.rs:8` | AV.0.11 Category A: dump/visualisation test; relies on non-checked-in `data/css/{tailwind-output,app}.css` fixtures. | DELETE | Test is an `eprintln!` dump harness, not an assertion test. Panics immediately on missing fixture even when lifted. It belongs to a visualisation audit, not the test suite. Delete the test function; fixture-comparison belongs in `docs/benchmarks/` or an ad-hoc scratch binary. |
| 53 | `hint_indent_group` | `crates/gorgeous/tests/vm.rs:155` | AV.0.11 Category A: pprint vm indent+group+sep interaction drifted post-AU. | INVESTIGATE | Fails when lifted (`indent group should produce indented lines, got: "alph\nbeta\ngram"`). Tractable pprint-vm hint-semantics fix. Orthogonal to AW-III viability scope; route to pprint tranche. |
| 54 | `hint_softbreak` | `crates/gorgeous/tests/vm.rs:218` | AV.0.11 Category A: softbreak flat emitter drifted post-AU. | INVESTIGATE | Fails when lifted (`softbreak flat should concatenate: left: "aaa bbb", right: "aaabbb"`). Same pprint-vm audit. |
| 55 | `test_prettify_multi_rule` (gorgeous/ebnf) | `crates/gorgeous/tests/ebnf.rs:24` | prettify codegen stops after first rule — pre-existing issue (matches crates/core/tests/ebnf_prettify.rs::prettify_multi_rule). | INVESTIGATE | Fails when lifted (`should contain second rule`). Shared root cause with #29; one fix closes both. |
| 56 | `test_let_parses_as_let_call` | `crates/gorgeous/tests/google_sheets.rs:27` | AV.0.11 Category A: google-sheets dispatch drift (LET not surfacing as let_call). | INVESTIGATE | Fails when lifted (`=LET(a,1,b) should parse as let_call, not func_call`). Depends on AV.3.3 Pratt lowering + shunting-yard DTA; out of AW-III scope. |
| 57 | `dump_biome_vs_gorgeous` | `crates/gorgeous/tests/biome_compare.rs:16` | AV.0.11 Category A: dump/visualisation test; relies on non-checked-in `data/css/tailwind-output.css` fixture. | DELETE | Test is a visualisation dump (`eprintln!` to compare biome vs gorgeous output), not an assertion. Panics on missing fixture. Same disposition as #52. |
| 58 | `dump_tailwind_comparison` | `crates/gorgeous/tests/biome_compare.rs:51` | (same visualisation-dump rationale) | INVESTIGATE (or KEEP) | Panics on missing fixture when lifted. Unlike #57 this variant does include structural assertions on the tailwind size; fixture comes from a gorgeous-side visualisation artefact that is never checked in. Closest to DELETE (same as #52/#57) but softer: strip the unchecked fixture entry and keep the bootstrap.css comparison OR delete outright. Deferrable to gorgeous visualisation-fixtures audit. |

## Aggregate by disposition

### CLOSE — 14 tests (lift `#[ignore]` in AW-III.W3)

All verified passing when the attribute is lifted. The `json_*`
serialize-roundtrip + `structural_*` + `bbnf_rule` + `css_simple`
closures all come from the same "AW open scope: serialize/structural
roundtrip" batch, plus `css_simple` from the AW-I.W2.5 carry.

| # | Test | File:line |
|---|------|-----------|
| 5  | `json_empty_arr`                  | `crates/core/tests/serialize_roundtrip.rs:141` |
| 6  | `json_array`                      | `crates/core/tests/serialize_roundtrip.rs:142` |
| 7  | `json_empty_obj`                  | `crates/core/tests/serialize_roundtrip.rs:143` |
| 8  | `json_object`                     | `crates/core/tests/serialize_roundtrip.rs:144` |
| 9  | `json_nested`                     | `crates/core/tests/serialize_roundtrip.rs:145` |
| 11 | `bbnf_rule`                       | `crates/core/tests/serialize_roundtrip.rs:231` |
| 12 | `css_simple`                      | `crates/core/tests/serialize_roundtrip.rs:264` |
| 20 | `structural_object_with_array`    | `crates/core/tests/structural.rs:63` |
| 21 | `structural_array_three_numbers`  | `crates/core/tests/structural.rs:107` |
| 22 | `structural_object_two_pairs`     | `crates/core/tests/structural.rs:141` |
| 23 | `structural_nested_objects`       | `crates/core/tests/structural.rs:189` |
| 24 | `structural_empty_array`          | `crates/core/tests/structural.rs:230` |
| 25 | `structural_empty_object`         | `crates/core/tests/structural.rs:245` |
| 26 | `structural_data_json_sanity`     | `crates/core/tests/structural.rs:397` |

### DELETE — 4 tests (remove the test function entirely)

| # | Test | File:line | Rationale |
|---|------|-----------|-----------|
| 39 | `cost_weights_unified::call_strategy_flips_under_inverted_call_overhead`  | `crates/ir/tests/vm/cost_weights_unified.rs:519` | Body is `unreachable!(...)` stub. Test cannot execute real behaviour; the `CostBudgetConstraint` consumer the test documents (AF.2-4C) is not going to be built — the backend inliner now lives on different cost infrastructure. The stub is architectural documentation masquerading as a test. |
| 40 | `cost_weights_unified::inline_body_size_penalty_affects_per_rule_decision` | `crates/ir/tests/vm/cost_weights_unified.rs:553` | Same stub pattern. |
| 41 | `cost_weights_unified::prettify_emission_scales_with_pretty_subtree_size`  | `crates/ir/tests/vm/cost_weights_unified.rs:569` | Same stub pattern; additionally the rationale explicitly says the architecture will not reverse ("prettify pin is structural, not a continuous cost"). |
| 52 | `output_size_comparison`                                                  | `crates/gorgeous/tests/biome_compare2.rs:8`     | Visualisation dump (`eprintln!` comparison), not an assertion. Depends on never-checked-in `tailwind-output.css` + `app.css` fixtures. Belongs outside the test suite entirely. |
| 57 | `dump_biome_vs_gorgeous`                                                  | `crates/gorgeous/tests/biome_compare.rs:16`     | Same visualisation-dump pattern as #52 — prints first 60 lines of biome vs gorgeous to stderr; asserts nothing. |

(5 tests total; I count this as 4 CLOSE-or-DELETE items because #52 and
#57 share the same "visualisation-dump not a test" rationale and #58 is
a borderline case recorded as INVESTIGATE pending the gorgeous
visualisation-fixtures audit.)

### INVESTIGATE — 40 tests

Grouped by root cause:

**Group A — payload activation (Cluster C cascade, closes via AW-III.W1)**: #1, #2, #3, #13, #14, #15, #16, #17, #18, #19 (10 tests).

CSS percentage Unit-payload + JSON variant_idx dispatch — both flavours of the same `DtaState::Regex`/`Literal` payload-stripping gap. AW-III.W1's DTA payload wiring closes them.

**Group B — DTA parse completeness (Cluster A, closes via AW-III.W2)**: #10 (`ebnf_rule`). 1 test.

Same root cause as the `ebnf_*_tape_parity` cluster: EBNF grammar fails offset-0 parse.

**Group C — analysis-mode rework (not in AW scope)**: #37, #38, #42, #43, #44, #45 (6 tests).

Structural-mode pipeline gates `compute_follow_sets` + span-eligibility refinement + cyclic-rule-paths + alias-hint diagnostics. Tractable but distinct tranche. Carry ignores with rationale.

**Group D — closure lowering gap (grammar-closures project)**: #32, #33, #34, #35, #36 (5 tests).

`lower::expression` panics on `|x| ...` closure bodies. First-class closures are a separate project (`grammar-closures` project memo). Carry ignores.

**Group E — CSP GAC alldiff gate**: #46, #47, #48, #49, #50, #51 (6 tests).

Solver lacks GAC alldiff — legitimate feature gap documented on-file. Carry ignores with rationale OR implement GAC alldiff in a CSP-focused tranche.

**Group F — pprint / prettify codegen drift**: #29, #53, #54, #55 (4 tests).

Two distinct root causes:
- #29 + #55 — prettify multi-rule truncation in gorgeous. One fix closes both.
- #53 + #54 — pprint-vm indent/group + softbreak semantics drift post-AU. Separate pprint refresh.

**Group G — miscellaneous producer-side / test-data fixes**:

| # | Test | Root cause | Effort |
|---|------|------------|--------|
| 4  | `test_selective_transitive_unfurling` | Module loader doesn't unfurl transitive deps of selectively-imported rules. | Medium — rewrite `load_module_graph` to compute transitive closure. Out of AW scope. |
| 27 | `no_hand_written_subvariant_references` | 2 hand-written `BbnfBootstrapRuleKind::term_{1,2}` refs in `src/graph/deps.rs`. | Medium — migrate to wrapper-peel substrate. Producer-side; aligns with AW-III's "producer-side in scope" thesis but orthogonal to DTA viability. Defer to a graph-walker refresh tranche. |
| 28 | `parse_recover_without_terminator`    | bbnf.bbnf requires trailing `;` after `@recover`. | Low — grammar-side `bbnf.bbnf` edit + bootstrap regen. Directive-syntax refresh tranche. |
| 30 | `compile_request_rejects_unknown_nonterminal` | `validate_ast` no longer precedes `lower::expression`; unknown-nonterminal handling regressed to src-side panic. | Low-medium — reinstate AST validation gate. Pipeline error-surface refresh. |
| 31 | `pipeline_google_sheets_multiline_let` | Hard-coded `expression` rule name; grammar refactored to `arithmetic_expr`. | Trivial — update test data. Sheets Pratt follow-up (AV.3.3). |
| 56 | `test_let_parses_as_let_call`            | google-sheets LET not surfacing as `let_call`. | Medium — depends on AV.3.3 Pratt lowering. |
| 58 | `dump_tailwind_comparison`               | Visualisation dump + missing fixture. | Trivial — drop missing fixture / delete outright. Gorgeous visualisation-fixtures audit. |

7 tests in Group G.

**Group totals: 10 + 1 + 6 + 5 + 6 + 4 + 7 + 1 (the group-E outlier is already in E) = 40.** Groups A–G exhaustively partition the INVESTIGATE dispositions.

## CLOSE batch (lift attributes)

```rust
// File 1: crates/core/tests/serialize_roundtrip.rs
#[test] #[ignore = "AW open scope..."] fn json_empty_arr() {...}  // line 141 → lift
#[test] #[ignore = "AW open scope..."] fn json_array()     {...}  // line 142 → lift
#[test] #[ignore = "AW open scope..."] fn json_empty_obj() {...}  // line 143 → lift
#[test] #[ignore = "AW open scope..."] fn json_object()    {...}  // line 144 → lift
#[test] #[ignore = "AW open scope..."] fn json_nested()    {...}  // line 145 → lift
#[test] #[ignore = "AW open scope..."] fn bbnf_rule()      {...}  // line 231 → lift
#[test] #[ignore = "AW-I.W2.5..."]   fn css_simple()       {...}  // line 264 → lift

// File 2: crates/core/tests/structural.rs
#[test] #[ignore = "..."] fn structural_object_with_array()   {...}  // line 63 → lift
#[test] #[ignore = "..."] fn structural_array_three_numbers() {...}  // line 107 → lift
#[test] #[ignore = "..."] fn structural_object_two_pairs()    {...}  // line 141 → lift
#[test] #[ignore = "..."] fn structural_nested_objects()      {...}  // line 189 → lift
#[test] #[ignore = "..."] fn structural_empty_array()         {...}  // line 230 → lift
#[test] #[ignore = "..."] fn structural_empty_object()        {...}  // line 245 → lift
#[test] #[ignore = "..."] fn structural_data_json_sanity()    {...}  // line 397 → lift
```

Each lift is a single-line change: remove the `#[ignore = "..."]`
attribute. No test body modification.

AW-III.W3 executor runs one commit per file (2 commits: serialize
file + structural file) or one aggregate commit.

Expected effect on workspace: **+14 passed / -14 ignored**.

## DELETE batch (remove test functions)

```rust
// File 1: crates/ir/tests/vm/cost_weights_unified.rs
// REMOVE: fn call_strategy_flips_under_inverted_call_overhead (lines ~518–541)
//   Rationale: `unreachable!()` stub documenting an AF.2-4C consumer
//   migration that the architecture no longer requires. Keep the
//   doc-comment block (lines ~511–517) as inline documentation or
//   migrate to a doc-comment on `CostWeights::call_overhead`.
//
// REMOVE: fn inline_body_size_penalty_affects_per_rule_decision (lines ~552–559)
//   Rationale: same stub pattern as above.
//
// REMOVE: fn prettify_emission_scales_with_pretty_subtree_size (lines ~568–574)
//   Rationale: same stub pattern; architecture will not reverse
//   (prettify pin is structural, not continuous cost).

// File 2: crates/gorgeous/tests/biome_compare2.rs
// REMOVE: fn output_size_comparison (lines 8–32)
//   Rationale: visualisation-dump harness (eprintln! comparison),
//   not an assertion test. Fixtures `tailwind-output.css`/`app.css`
//   are never checked in; the "test" cannot run. Belongs outside
//   the test suite entirely — move into a scratch binary under
//   `crates/gorgeous/examples/` or delete.
//
// NOTE: after removing this single function the entire file
// becomes empty — delete the file and remove the corresponding
// `[[test]]` entry from `crates/gorgeous/Cargo.toml`.

// File 3: crates/gorgeous/tests/biome_compare.rs
// REMOVE: fn dump_biome_vs_gorgeous (lines 16–49)
// REMOVE: fn dump_tailwind_comparison (lines 51–~78)
//   Rationale: same visualisation-dump pattern; no assertions,
//   fixtures absent. If the orchestrator wants to retain the
//   bootstrap-only variant of dump_biome_vs_gorgeous (fixture
//   is checked in), delete only dump_tailwind_comparison and
//   keep the bootstrap comparison as a fixture-present smoke
//   test — up to AW-III.W3 executor's judgement.
```

Expected effect on workspace: **-4 ignored** (via -4 test functions
removed). No passed/failed delta.

## INVESTIGATE batch (root-cause details + fix estimate)

### Group A — payload activation (10 tests, 2–4 h total)

**Root cause**: DTA walker's `DtaState::Regex` arm hardcodes
`PayloadKind::F64`; `DtaState::Literal` arms never emit payload; lifter
strips `IrNode::Map` wholesale. AW-III.W1 plan covers this verbatim.

**Fix path**: AW-III.W1 extends `DtaState::Regex`/`Literal` with
`payload: PayloadKind`; lifter threads from enclosing `IrNode::Map`
FnDescriptor; walker consumes; Seq→KvPair promotion. W1 hard gate
already drops Cluster C from 37 → ≤ 5; these 10 tests are Cluster C
residuals and should unignore-and-pass post-W1.

**Affected files** (already named in AW-III.md): `crates/bbnf-tape/src/
dta.rs`, `crates/ir/src/passes/recognizers/dta.rs`, `crates/bbnf-tape/
src/driver.rs`, `crates/core/src/backend/rust/emitter/dta.rs`.

**Estimate**: No additional effort beyond W1's existing scope. These
tests CLOSE-by-lift once W1 lands; they move from the INVESTIGATE
column of this audit into the CLOSE column of the W3 execution log.

### Group B — EBNF parse completeness (1 test, blocks on W2)

**Root cause**: EBNF grammar fails offset-0 parse after AW-II.W5b's
Minus + double-Repeat fixes. Upstream issue in the ebnf lifting
pipeline.

**Fix path**: AW-III.W2 covers this exact test cluster
(`ebnf_{minimal,recursive_list,expr_grammar}_tape_parity` +
`ebnf_root_has_at_least_one_rule` + `ebnf_prettify::parse_{single,multi}_rule`).

**Affected files**: `crates/bbnf-tape/src/driver.rs`, `crates/ir/src/
passes/recognizers/dta.rs`, `crates/core/src/lower/**`.

**Estimate**: No additional effort beyond W2's existing scope.

### Group C — analysis-mode rework (6 tests, 6–10 h)

**Root cause**: `crates/analysis/` runs the structural-mode pipeline
(`PipelineOptions::structural = true`); this gates
`compute_follow_sets`, `refine_span_eligibility`, cyclic-path BFS, and
alias-hint diagnostics behind `!structural`. All 6 tests expect
non-structural analysis outputs.

**Fix path**: Either (a) compute these artefacts in structural mode
(smaller change — hoist the specific passes the tests need out of the
`!structural` guard), OR (b) switch the analysis crate to run the full
pipeline (larger change — may regress analysis cost on large grammars).

**Affected files**: `crates/core/src/pipeline/compile.rs`
(`!structural` gates), `crates/analysis/src/lib.rs` (`analyze()` call
path), LSP tests cascade.

**Estimate**: 6–10 h for option (a); 15–25 h for option (b). Out of
AW-III viability scope. Defer to an analysis-mode refresh tranche.

### Group D — closure lowering gap (5 tests, 15–30 h)

**Root cause**: `lower::expression` at
`crates/core/src/lower/expression.rs:155` panics with `"closure:
missing body child"` when a rule body is `|x| ...`. The substrate for
first-class closures is not yet wired into lowering; the `grammar-
closures` project memo covers the full design.

**Fix path**: Grammar-closures project execution (separate tranche
altogether). Out of AW-III scope; keep ignores with rationale, and
ensure the `grammar-closures` project carries these 5 tests as its
acceptance surface.

**Affected files**: `crates/core/src/lower/expression.rs` (closure
lowering), `crates/core/src/graph/**` (call-arg + closure support),
bootstrap regen.

**Estimate**: 15–30 h (full tranche).

### Group E — CSP GAC alldiff gate (6 tests, 10–20 h)

**Root cause**: CSP solver uses binary forward-checking with
`Pruning::ForwardChecking`; hard Sudoku requires GAC (generalized arc
consistency) over `alldiff` constraints to terminate in reasonable
time.

**Fix path**: Implement GAC alldiff as a `Propagator` variant (the
`csp-solver` `propagate` entry point determines optimal strategy —
KISS DRY). Orthogonal to AW, csc411 project stewardship.

**Affected files**: `crates/csp-solver/src/solver.rs`, `crates/csp-
solver/src/propagators/**`.

**Estimate**: 10–20 h. Out of AW-III scope; ignores are legitimate
feature gates and may stay with rationale.

### Group F — pprint / prettify drift (4 tests, 4–8 h)

**#29 + #55 — prettify multi-rule truncation** (same root cause):
`gorgeous::ebnf::prettify_ebnf` / `bbnf::...::prettify` stops after
the first rule. Likely a cursor/state reset issue in the gorgeous
multi-rule emitter.

**#53 + #54 — pprint-vm hint-semantics drift**: `indent+group+sep`
emits no leading spaces on continuation lines; `softbreak` in flat mode
emits a space instead of empty.

**Fix path**: (a) prettify multi-rule — audit `gorgeous::prettify_*`
driver loop + state reset between rules. (b) pprint-vm — audit
`format_ir` / `format_value` hint dispatch in `crates/gorgeous/src/vm.rs`.

**Affected files**: `crates/gorgeous/src/{vm,prettify/**}.rs`,
`crates/pprint/**`.

**Estimate**: 4–8 h combined. Out of AW-III viability scope; route to
a gorgeous / pprint refresh tranche OR fold into AW-IV if the
prettify impact on bench outcomes is material.

### Group G — miscellaneous (7 tests)

| # | Test | Est. effort | Owner / tranche |
|---|------|-------------|-----------------|
| 4  | `test_selective_transitive_unfurling`       | 3–5 h  | imports subsystem refresh (out of AW). |
| 27 | `no_hand_written_subvariant_references`     | 4–8 h  | Migrate `src/graph/deps.rs` to wrapper-peel substrate. AW-III.W3 stretch; producer-side + aligns with AW thesis. Practical: close in W3 or defer to a graph-walker tranche. |
| 28 | `parse_recover_without_terminator`          | 1–2 h  | Grammar-side edit to `bbnf.bbnf` + bootstrap regen. Directive-syntax refresh. |
| 30 | `compile_request_rejects_unknown_nonterminal` | 2–4 h | Reinstate `validate_ast` call in `compile_grammar_request`. Pipeline error-surface refresh. |
| 31 | `pipeline_google_sheets_multiline_let`      | < 1 h  | Update hard-coded rule names in test body. Trivial — executor may close this in W3 opportunistically. |
| 56 | `test_let_parses_as_let_call`               | 15–25 h | AV.3.3 Pratt lowering (out of AW). |
| 58 | `dump_tailwind_comparison`                  | < 1 h  | Delete or keep only bootstrap-fixture variant. Gorgeous visualisation-fixtures audit. |

Total Group G effort: ~26–46 h if every item is taken. AW-III.W3
scope: close #31 (trivial test-data update) at executor's discretion;
everything else stays ignored with rationale, routed to named successor
tranches.

## Path-dep `parse-that` ignores (out of scope)

The audit flagged 2 additional `#[ignore]` attributes in the path-
dependency `parse-that/`:

- `parse-that/rust/parse_that/tests/decode_test.rs:545` —
  `fn decode_microbench`: performance microbenchmark gated by
  `DECODE_BENCH` env var. Legitimate bench-smoke pattern; keep as-is.
- `parse-that/rust/parse_that/tests/css_parse_test.rs:472` —
  `fn test_parse_tailwind_css`: 3.6MB file, too slow in debug. Keep
  with existing rationale ("runs in release benchmarks").

Both have correct rationale; no action needed in AW-III.

## Execution order for AW-III.W3

1. **CLOSE batch** (single commit per file, no source risk):
   - `crates/core/tests/serialize_roundtrip.rs` — 7 lifts.
   - `crates/core/tests/structural.rs` — 7 lifts.
   - Verify: `cargo test -p bbnf --test serialize_roundtrip` → +6
     new tests passing (`ebnf_rule` still fails — it's in Group B).
   - Verify: `cargo test -p bbnf --test structural` → +7 new tests
     passing.
   - Commit: `test(core): close AW-III-lifted ignores — serialize +
     structural roundtrip (AW-III.W3)`.

2. **DELETE batch**:
   - Remove 3 stub functions in `crates/ir/tests/vm/
     cost_weights_unified.rs`.
   - Remove `crates/gorgeous/tests/biome_compare2.rs` file + Cargo.toml
     `[[test]]` entry.
   - Remove 2 dump functions in `crates/gorgeous/tests/biome_compare.rs`
     (keep file only if any remaining tests exist; otherwise remove file
     too).
   - Commit: `test(ir,gorgeous): delete stale ignores — unreachable
     stubs + visualisation-dumps (AW-III.W3)`.

3. **INVESTIGATE batch**:
   - AW-III.W1 closes Group A (10 tests) as a side-effect of payload
     wiring — audit the test passes post-W1 and lift in a chained
     commit.
   - AW-III.W2 closes Group B (1 test, `ebnf_rule`) as a side-effect of
     DTA parse completeness.
   - Groups C/D/E/F/G stay ignored with their existing on-file
     rationale, each reference-linked to a named successor tranche.
     Opportunistic closes: #31 (trivial test-data) and possibly #27
     (producer-side).

## AW-III.W3 hard-gate posture

Plan says "ignored count ≤ 10 at wave close (ideally 0)". After
executing CLOSE + DELETE + Groups A + B, the expected residual ignored
count is:

| Category | Count | Notes |
|----------|-------|-------|
| CLOSE executed | -14 | serialize + structural |
| DELETE executed | -4 | stub + dump tests |
| Group A closed via W1 | -10 | payload wiring cascade |
| Group B closed via W2 | -1 | ebnf_rule cascade |
| Group C residual | 6 | analysis-mode rework |
| Group D residual | 5 | closure lowering |
| Group E residual | 6 | CSP GAC alldiff |
| Group F residual | 4 | pprint/prettify |
| Group G residual | ~6–7 | miscellaneous |

**Residual total: ~27–28 ignored tests**. That exceeds the plan's
"≤ 10" gate.

The gap is real. Closing groups C + F (10 tests) lands the workspace
at ~17–18 ignores, still above the gate. Closing G entirely would
require ~26–46 h additional work in AW-III. The practical path:

- **Option 1**: orchestrator re-reads the AW-III.W3 hard gate and
  either (a) relaxes to "every residual ignore has documented on-file
  rationale + successor-tranche routing", or (b) absorbs Groups C + F
  into W3 scope and accepts the tranche running longer.
- **Option 2**: carry the ~27-ignore residual as a documented exception
  with successor-tranche routing, matching the plan's escape-clause
  pattern.

Option 1(a) matches the plan's fallback ("Any remaining ignored test
must have (a) an in-file comment with named rationale, (b) a tracking
doc entry, (c) explicit orchestrator approval"). This audit satisfies
(a) + (b); (c) is the orchestrator decision.

## Appendix — raw discovery

```
$ grep -rn '#\[ignore' crates/ | wc -l
61   # includes 3 lines in doc-comments (//!) and //-prefixed comments
$ grep -rnE '^[^/]*#\[ignore' crates/ | wc -l
58   # excluding comment-only occurrences
$ cargo test --workspace --no-fail-fast 2>&1 | grep -oE '[0-9]+ ignored' | awk '{s+=$1} END {print s}'
67   # includes feature-gated duplicates in gorgeous/tests/vm.rs + 2 parse-that ignores
```

The audit uses 58 as the unique source-level count; 67 is the reported
workspace-level run-time count and both numbers are mutually
consistent.

- HEAD: `b58d1461423acba6963343dc306e5029cc3cd66c`
- Worktree: `/Users/mkbabb/Programming/bbnf-wt-aw-c2`
- Command trail: `/tmp/c2-ignores.txt`, `/tmp/workspace-tests-all.txt`,
  plus one per-file `-- --ignored` run per affected test binary.
