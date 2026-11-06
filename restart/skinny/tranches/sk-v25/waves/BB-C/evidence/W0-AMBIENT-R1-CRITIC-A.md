# BB-C.W0 ambient optimizer authority — R1 hostile critic A

MODEL: `gpt-5.6-sol`; REASONING: `ultra`; WORKFLOW: `v2`

Verdict: **RED.** The production grammar-egraph deletion is statically correct
and parsimonious, but R1 does not satisfy W0 item 3 and its executable witness is
source-provably wrong.

## Findings

1. **Incorrect rejection oracle.** `crates/core/tests/direct_pipeline_semantics.rs`
   expects `!x` to return offset `0`, no value, and expected byte `[z]`. The VM
   drops rather than restores the outer checkpoint when the final `Ref(branch)`
   fails (`crates/ir/src/vm/compiler/compound.rs`), constructs the partial tagged
   value (`vm/compiler/rule.rs`, `vm/interpreter/construct.rs`), and returns it on
   failure (`vm/interpreter/mod.rs`). Source semantics are offset `1`, a partial
   `start` value, and an empty terminal-FOLLOW vector. The test would fail if it
   reached execution.
2. **Incorrect item scope.** `waves/BB-C/W0.md` item 3 also removes the
   independent-minimum CSP fallback. Production still calls
   `solve_grammar_components`; `passes/csp_strategy/mod.rs` still publishes
   independent minima on budget exhaustion and priority fallbacks on unsat.
   `W0-AMBIENT-R1-AUTHOR.md` must say “item 3 egraph clause,” not “item 3.”
3. **Typed branch semantics unproven.** The witness counts two `Map` nodes but
   proves neither `FnId`, inferred type, `0u8`/`1u8` expression, nor per-branch
   placement. The VM erases `Map` while lowering and only the `x` arm is run.
4. **Differential closure unproven.** Lowering imports ambient `BBNF_COST_*`
   values. The test does not seal them, and `build_and_saturate` discards its
   scheduler report, so the asserted extracted shape does not prove complete,
   deterministic saturation.
5. **Comment truth incorrect.** Stale production-egraph authority remains in
   `passes/materialization/classify.rs`, `core/tests/egraph_semantic.rs`,
   `egraph/rules/suffix.rs`, `ir/tests/lattices/materialization_eclass_gate.rs`,
   `ir/tests/egraph/egraph_analysis.rs`, and
   `core/examples/egraph_fire_probe.rs`.
6. **Replay unproven.** The author record has no content-addressed raw log or exit
   receipt. Current ARM checks cannot prove the historical SIGKILL invocation.
7. **Structure incorrect.** The flat
   `crates/core/tests/direct_pipeline_semantics.rs` is not isomorphic to
   `crates/core/src/pipeline/compile/pipeline.rs` and worsens the flat test sand.

## Correct narrow facts

- All five subject hashes match the author record; protected Cargo/G3 bytes are
  preserved and the tracked owned diff is `+20/-76`.
- The former production grammar-egraph edge is absent. No flag, alias, shim,
  fallback, or dual egraph production route was added.
- Retaining the substrate for later bounded research is appropriate.
- The author correctly grants zero credit and labels the native build technical
  RED rather than an ownership blocker.

The critic performed read-only source analysis and ran no Cargo command.
