# BB-C.W0 ambient optimizer authority — R1 hostile critic B

MODEL: `gpt-5.6-sol`; REASONING: `ultra`; WORKFLOW: `v2`

Verdict: **RED / incorrect as BB-C.W0 item 3.**

## Findings

1. **Correct narrow deletion.** `compile_ast_common` no longer calls grammar
   `build_and_saturate`, `write_back_optimized`, or `pool.write_back`; only the
   research definitions remain outside tests/examples/benches.
2. **Incorrect item scope.** `pipeline.rs` still publishes
   `solve_grammar_components` output. `csp_strategy/mod.rs` still returns empty
   maps without a DAG, independent minima after budget exhaustion, and a priority
   fallback after unsat. R1 implements only item 3's egraph half.
3. **Legacy comparison unproven.** The test applies the legacy egraph only after
   `compile_grammar` has already completed later pipeline/type-projection work;
   the removed edge ran earlier. The clone is never compiled or executed, so this
   is not an isomorphic replay of the former production pipeline.
4. **Typed maps and both arms unproven.** Only `!xz` executes; `!yz`, each map's
   expression/`FnId`, `U8` type entry, branch placement, and legacy survival are
   not asserted. VM `Map` erasure makes the span result non-evidence for typing.
5. **Configuration incorrect.** Ambient `BBNF_COST_*` extraction weights and
   scheduler limits can change the asserted legacy shape. No sealed configuration
   exists.
6. **Structure incorrect.** The witness is flat rather than source-isomorphic and
   its substrate half is not grouped under the existing IR egraph suite.
7. **Stale authority incorrect.** Contradictory production claims remain in
   `core/tests/egraph_semantic.rs`, `core/examples/egraph_fire_probe.rs`,
   `core/tests/named_pipeline_probe.rs`, `core/tests/named_type_preservation.rs`,
   `ir/tests/egraph/egraph_analysis.rs`,
   `ir/tests/lattices/materialization_eclass_gate.rs`, `pipeline.rs`,
   `ir/src/types/grammar.rs`, `passes/transform/pattern_dedup.rs`,
   `egraph/rules/suffix.rs`, `passes/mod.rs`, and active optimizer documents.
8. **Census incorrect.** The author searched three grammar-egraph symbols under
   `crates/core/src` but omitted the still-live CSP authority/fallback and
   overstated the mention list as exact.
9. **Preservation correct but limited.** Owned/protected hashes and net `-56`
   tracked-line reduction match. `git diff --check` did not inspect the untracked
   witness, and shared-worktree authorship is not independently proven.
10. **Replay unproven.** Linker SIGKILL occurred before the executable ran and no
    raw R1 replay artifact exists. No semantic, typing, determinism, or mutation
    claim therefore has executable evidence.

The critic performed read-only source analysis and ran no Cargo command.
