# AW-III.W3 ignore-routing — successor-tranche mappings

Authoritative successor-tranche disposition for every `#[ignore]`
that survives AW-III.W3 close. The CLOSE + DELETE + cascade batches
are mechanical lifts and are not represented here. INVESTIGATE
Groups C–G are.

Every entry is dual-anchored: file:line at the on-file `#[ignore]`
attribute, and the named successor tranche or project that carries
the residual forward. The closing section flags rationales whose
text points at internal bookkeeping rather than the named
successor tranche.

## Disposition summary

| Group | Count | Successor                          | Status at AW-III.W3 close |
|-------|-------|------------------------------------|---------------------------|
| C — analysis-mode pipeline                | 6 | analysis-mode-refresh tranche  | open with on-file rationale |
| D — closure-body lowering                 | 5 | grammar-closures project       | open with on-file rationale |
| E — CSP GAC alldiff                       | 6 | csc411-csp-tranche             | open with on-file rationale |
| F — pprint / prettify drift               | 4 | gorgeous + pprint refresh      | open with on-file rationale |
| G — miscellaneous (per-test)              | 6–7 | mixed (see per-test routing) | open with on-file rationale |

Counts reflect the audit's projection at W3 plan time. If the
lift wave opportunistically closes `pipeline_google_sheets_multiline_let`
(trivial test-data update) or `no_hand_written_subvariant_references`
(producer-side migration), Group G drops accordingly. The
orchestrator reconciles this table against the actual close
surface in a single follow-up commit if any opportunistic closes
land.

## Per-group detailed routing

### Group C — analysis-mode pipeline (6 tests)

Root cause: `crates/analysis/` runs `PipelineOptions::structural = true`,
which gates `compute_follow_sets`, `refine_span_eligibility`, cyclic-
path BFS, and alias-hint diagnostics behind `!structural`. All six
tests expect non-structural analysis outputs. Two cascade pairs:
the LSP integration tests inherit the analysis crate's gap.

| File:line                                                | Test                              | Successor tranche               | Effort |
|----------------------------------------------------------|-----------------------------------|---------------------------------|--------|
| `crates/analysis/tests/directives.rs:169`                | `ir_meta_has_follow_sets`         | analysis-mode-refresh           | 6–10 h |
| `crates/analysis/tests/directives.rs:196`                | `ir_meta_has_memo_and_span_info`  | analysis-mode-refresh           | 6–10 h |
| `crates/lsp/tests/analyze.rs:158`                        | `test_cycle_detection`            | analysis-mode-refresh           | 6–10 h |
| `crates/lsp/tests/analyze.rs:173`                        | `test_alias_detection`            | analysis-mode-refresh           | 6–10 h |
| `crates/lsp/tests/integration.rs:1335`                   | `test_diagnostics_cycle_path`     | analysis-mode-refresh (cascade) | included above |
| `crates/lsp/tests/integration.rs:1365`                   | `test_diagnostics_alias_hint`     | analysis-mode-refresh (cascade) | included above |

Successor tranche scope: hoist the four passes the tests need
(`compute_follow_sets`, `refine_span_eligibility`, `cyclic_rule_paths`,
alias-hint diagnostics) out of the `!structural` guard at
`crates/core/src/pipeline/compile.rs`, OR switch the analysis crate
to run the full pipeline. Effort 6–10 h for the hoist; 15–25 h for
full-pipeline switch (may regress analysis cost on large grammars).

### Group D — closure-body lowering (5 tests)

Root cause: `crates/core/src/lower/expression.rs:155` panics with
`"closure: missing body child"` when a rule body is `|x| ...`. The
substrate for first-class closures is not yet wired into lowering;
the existing `grammar-closures` project memo carries the full design.

| File:line                              | Test                       | Successor project   |
|----------------------------------------|----------------------------|---------------------|
| `crates/core/tests/pipeline.rs:837`    | `closure_single_param`     | grammar-closures    |
| `crates/core/tests/pipeline.rs:848`    | `closure_multi_param`      | grammar-closures    |
| `crates/core/tests/pipeline.rs:859`    | `closure_nested_calls`     | grammar-closures    |
| `crates/core/tests/pipeline.rs:871`    | `closure_with_rule_ref`    | grammar-closures    |
| `crates/core/tests/pipeline.rs:883`    | `closure_composition`      | grammar-closures    |

Successor project scope: `lower::expression` closure lowering, plus
`crates/core/src/graph/**` call-arg + closure support, plus bootstrap
regen. The five tests are the project's acceptance surface. Effort
estimate 15–30 h (full tranche).

### Group E — CSP GAC alldiff (6 tests)

Root cause: `crates/csp-solver/` uses binary forward-checking via
`Pruning::ForwardChecking`. Hard Sudoku requires GAC (generalised
arc consistency) over `alldiff` constraints to terminate in
reasonable time. The test fixtures (Al Escargot, Inkala 2010,
Golden Nugget, Platinum Blonde, minimal-17) are canonical hard-
sudoku puzzles known to require alldiff propagation.

| File:line                                       | Test                              | Successor tranche      |
|-------------------------------------------------|-----------------------------------|------------------------|
| `crates/csp-solver/tests/solver.rs:1338`        | `test_hard_sudoku_al_escargot`    | csc411-csp-tranche     |
| `crates/csp-solver/tests/solver.rs:1354`        | `test_hard_sudoku_inkala_2010`    | csc411-csp-tranche     |
| `crates/csp-solver/tests/solver.rs:1370`        | `test_hard_sudoku_golden_nugget`  | csc411-csp-tranche     |
| `crates/csp-solver/tests/solver.rs:1386`        | `test_hard_sudoku_platinum_blonde`| csc411-csp-tranche     |
| `crates/csp-solver/tests/solver.rs:1402`        | `test_hard_sudoku_minimal_17`     | csc411-csp-tranche     |
| `crates/csp-solver/tests/solver.rs:1421`        | `test_hard_sudoku_all_configs`    | csc411-csp-tranche     |

Successor tranche scope: implement GAC alldiff as a `Propagator`
variant. The unified `propagate` entry point at
`crates/csp-solver/src/solver.rs` determines optimal strategy
internally — KISS DRY, no propagate-suffix variants. Effort 10–20 h.
This work belongs to the `csc411` solver project rather than the
bbnf-lang correctness arc.

### Group F — pprint / prettify drift (4 tests)

Two distinct root causes share the group label. The prettify multi-
rule pair shares one fix; the pprint-vm hint pair shares another.

| File:line                               | Test                          | Root cause                              | Successor tranche     |
|-----------------------------------------|-------------------------------|-----------------------------------------|------------------------|
| `crates/core/tests/ebnf_prettify.rs:43` | `prettify_multi_rule`         | gorgeous emitter truncates after rule 1 | gorgeous-prettify-refresh |
| `crates/gorgeous/tests/ebnf.rs:24`      | `test_prettify_multi_rule`    | same as above (cascade)                 | gorgeous-prettify-refresh |
| `crates/gorgeous/tests/vm.rs:155`       | `hint_indent_group`           | pprint-vm `indent+group+sep` semantics drifted post-AU | pprint-hint-refresh |
| `crates/gorgeous/tests/vm.rs:218`       | `hint_softbreak`              | pprint-vm `softbreak` flat emitter inserts space, expected empty | pprint-hint-refresh |

Successor tranche scope: (a) audit `gorgeous::prettify_*` driver
loop + state reset between rules at `crates/gorgeous/src/prettify/**`;
(b) audit `format_ir` / `format_value` hint dispatch at
`crates/gorgeous/src/vm.rs` and the corresponding pprint primitives.
Effort 4–8 h combined.

This group is a candidate for AW-IV absorption if prettify drift
becomes a bench-quality concern (W6's prettify-touching benches
include `bbnf_monolithic css_pretty`). The default route is a
dedicated refresh tranche.

### Group G — miscellaneous (6–7 tests)

Per-test routing — root causes do not cluster.

| File:line                                                  | Test                                          | Root cause                                                                           | Successor tranche                                  | Effort |
|------------------------------------------------------------|-----------------------------------------------|--------------------------------------------------------------------------------------|----------------------------------------------------|--------|
| `crates/core/tests/imports.rs:307`                         | `test_selective_transitive_unfurling`         | module loader does not unfurl transitive deps of selectively-imported rules          | imports-subsystem-refresh                          | 3–5 h  |
| `crates/core/tests/no_subvariant_refs.rs:92`               | `no_hand_written_subvariant_references`       | 2 hand-written `BbnfBootstrapRuleKind::term_{1,2}` refs in `src/graph/deps.rs`       | graph-walker-wrapper-peel-migration                | 4–8 h  |
| `crates/core/tests/recover.rs:128`                         | `parse_recover_without_terminator`            | `bbnf.bbnf` requires trailing `;` after `@recover`; terminator-free form unsupported | directive-syntax-refresh (grammar + bootstrap regen) | 1–2 h  |
| `crates/core/tests/pipeline_compile_request.rs:126`        | `compile_request_rejects_unknown_nonterminal` | `validate_ast` no longer precedes `lower::expression`; src-side panic instead        | pipeline-error-surface-refresh                     | 2–4 h  |
| `crates/core/tests/pipeline.rs:486`                        | `pipeline_google_sheets_multiline_let`        | hard-coded `expression` rule name; grammar refactored to `arithmetic_expr`           | trivial test-data update (executor discretion)     | < 1 h  |
| `crates/gorgeous/tests/google_sheets.rs:27`                | `test_let_parses_as_let_call`                 | google-sheets dispatch drift (LET not surfacing as `let_call`)                       | AV.3.3 Pratt lowering follow-up                    | 15–25 h |
| `crates/gorgeous/tests/biome_compare.rs:51`                | `dump_tailwind_comparison`                    | visualisation dump + missing fixture                                                 | gorgeous-visualisation-fixtures-audit              | < 1 h  |

The group's count is 7 if W3.A leaves `pipeline_google_sheets_multiline_let`
ignored (the audit notes it as an opportunistic close at executor
discretion); 6 if W3.A closes it.

## Carry-forward to AW-IV

Group F is a candidate for AW-IV absorption. The four prettify-
adjacent tests exercise the same `gorgeous` and `pprint` surfaces
that AW-IV's W4-W5 prettify-touching benches consume. If prettify
drift becomes a bench-quality concern (notably for the
`bbnf_monolithic css_pretty` entry), AW-IV's plan should fold the
`gorgeous-prettify-refresh` and `pprint-hint-refresh` work into a
shared wave rather than spawn dedicated tranches.

This is a candidate, not a commitment. The default route is dedicated
refresh tranches.

## Carry-forward to AX

None. Every Group C–G residual is a correctness item, not a
snapshot/replay/incremental item. AX inherits the same green
workspace AW-III.W3 closes; the routed residuals do not impede
AX's substrate.

## On-file rationale staleness flags

Three staleness signals surfaced during routing-doc authoring.
The owning lift-wave agent fixes these inline or a follow-up
commit reconciles them; the routing doc itself does not modify
test files.

1. **`crates/core/tests/json_parity.rs:143,159,212,233,251,288,420`**
   — seven Group A tests still ignored at routing-doc authoring
   (workspace 1119/0/64 at W2 close). Rationale text reads
   `"AU.6.8 parity: post-W6 tape-shape shift broke variant_idx
   dispatch in the walker. Route: audit-doc tracks; fix in
   follow-up."` The rationale points at "audit-doc" rather than
   a named successor tranche. If the W1 payload wiring closed the
   underlying defect, the lifts belong in the CLOSE batch and the
   rationale text becomes dead. If W1 did not close them, the
   rationale should name the follow-up tranche directly rather
   than indirect through "audit-doc".

2. **`crates/lsp/**` and `crates/analysis/tests/directives.rs`**
   rationales lead with `"AV.0.11 Category A: "` — that category
   label was a transient bookkeeping device from the AV tranche.
   The substantive rationale (structural-mode gate) remains
   accurate, but the leading category prefix is no longer
   meaningful. Optional cleanup: drop the prefix; keep the
   rationale plus the forward-tranche pointer.

3. **`crates/core/tests/serialize_roundtrip.rs:264`** rationale on
   `css_simple` references "AW-I.W2.5" and routes "to W4.5 or a
   follow-up tranche once the serialize + prettify codegen paths
   reconcile their view-layer conventions". The test passes when
   the attribute is lifted; this rationale is stale. The lift
   belongs in the CLOSE batch.

Entries that drop out of the surface as the lift wave lands are
removed in a single reconciliation commit; entries that survive
get their staleness flags resolved in a test-rationale-refresh
follow-up.
