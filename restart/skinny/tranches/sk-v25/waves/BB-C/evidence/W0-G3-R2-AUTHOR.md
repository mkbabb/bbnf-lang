# BB-C.W0 G3 deletion R2 — repair author record

Model: `gpt-5.6-sol`  
Reasoning: `ultra`  
Workflow: `v2`  
Role: repair author

Status: **R2 REPAIR IMPLEMENTED; HOSTILE PASSES RESET TO ZERO.**

This record claims only the narrow G3 evidence repair ordered by
`W0-G3-R1-ADJUDICATION.md`. It claims no BB-A evidence, BB-C.W0 completion,
optimizer, graph, package, performance, wave, or movement credit.

## Frozen R2 subject

The accepted production deletion stayed byte-identical. The unused
`default_rules` API parameters and the left-recursion test's historical
`EliminateEpsilonInAlt` provenance comment were deliberately left unchanged.

| Path | R2 SHA-256 |
|---|---|
| `crates/ir/src/egraph/rules/universal.rs` | `9835616cc8900eb5e0e5fde769e34d149bfcdf8e2493f0aef74b958426768b5b` |
| `crates/ir/src/egraph/rules/mod.rs` | `da0a7b1b9120e6d3110582254824fc6cb6509b181aadd13332c39ab80cc897c4` |
| `crates/ir/tests/egraph/egraph_universal.rs` | `e4e9beab261c1a10a52d537f2d773e8f69c051e763918f3d35814d1252576ede` |
| `crates/core/tests/egraph_semantic.rs` | `bce230664724c15129ce582b7108b4e11c30cfc36a89f07488f7cc1b8046b56f` |

## Repaired executable evidence

`egraph_semantic_nullable_alt_preserves_empty_branch` now retains each complete
`ParseResult` and asserts every exposed plane exactly:

- `""`: success, offset `0`, no diagnostics, and
  `Tagged { tag: 0, span: (0, 0), children: [Nil] }`;
- `"x"`: success, offset `1`, no diagnostics, and
  `Tagged { tag: 0, span: (0, 1), children: [Span(0, 1)] }`;
- diagnostic-bearing rejection of `"x!"`: failure, offset `1`, exact retained
  partial tree
  `Tagged { tag: 2, span: (0, 1), children: [Tagged { tag: 0, span: (0, 1), children: [Span(0, 1), Nil] }] }`,
  and exactly one diagnostic at offset `1` for rule `atom` expecting `[b'z']`.

The IR test payload adds two independent route gates:

1. `default_rules_match_exact_allowlist` executes `default_rules()` and requires
   this exact ordered list:
   `deduplicate-alt-branches`, `superset-absorb-alt`, `union-merge-alt`,
   `fuse-alt-regex-branches`, `common-suffix-factor`, `alt-of-single`,
   `repeat-of-single`, `concat-literals`.
2. `default_saturation_keeps_nullable_alt_distinct_from_literal` constructs
   `Alt([Literal("a"), Epsilon])`, runs the production
   `build_and_saturate` default path, proves its root class is not the literal
   class, then runs `GrammarCostModel` plus `write_back_optimized` and proves
   extraction retains both branches exactly.

## Immutable pre-deletion crater

The crater was archived from immutable commit
`af15f63e0d2d3d719938c13b906a50acbb92ea3b` with
`git archive --format=tar HEAD`. Only the two R2 test files were overlaid. Its
identities were:

| Crater input | SHA-256 |
|---|---|
| pre-deletion `universal.rs` | `2755c54114e25ab5d18c9f7f353926d50e56d7cfe1c09c6fb8d8efa413f91c42` |
| pre-deletion `rules/mod.rs` | `d5f6f17a9a3b460096619e75d3d637ac1008ee8c1e7c3ce892579133a00f0669` |
| overlaid `egraph_universal.rs` | `e4e9beab261c1a10a52d537f2d773e8f69c051e763918f3d35814d1252576ede` |
| overlaid `egraph_semantic.rs` | `bce230664724c15129ce582b7108b4e11c30cfc36a89f07488f7cc1b8046b56f` |
| crater `Cargo.lock` | `a6a75b64633dac671206b06bea3c8b8b18ccd180b04712dc04dcc4c423160908` |

All three focused crater runs exited `101` for the intended pre-deletion
behavior:

```text
cargo test --offline --locked -p bbnf-ir --test egraph \
  egraph_universal::default_rules_match_exact_allowlist -- --exact --nocapture
RED: actual allowlist contained the extra `wrap-of-epsilon-scalar` route.

cargo test --offline --locked -p bbnf-ir --test egraph \
  egraph_universal::default_saturation_keeps_nullable_alt_distinct_from_literal \
  -- --exact --nocapture
RED: class separation failed with `left: e2`, `right: e2`.

cargo test --offline --locked -p bbnf --test egraph_semantic \
  egraph_semantic_nullable_alt_preserves_empty_branch -- --exact --nocapture
RED: `nullable alternation rejected empty input`.
```

The crater lock remained byte-identical at
`a6a75b64633dac671206b06bea3c8b8b18ccd180b04712dc04dcc4c423160908`
after these failures.

## Repaired twin replay

The repaired payload was run twice, serializing test execution:

```text
cargo test --offline --locked -p bbnf-ir --test egraph \
  egraph_universal:: -- --test-threads=1
9 passed; 0 failed; 26 filtered

cargo test --offline --locked -p bbnf --test egraph_semantic \
  egraph_semantic_nullable_alt_preserves_empty_branch \
  -- --exact --test-threads=1
1 passed; 0 failed; 7 filtered
```

Both command groups exited `0`. Test-result lines were retained, execution
durations were normalized to `<TIME>`, and both normalized payloads had the
same SHA-256:
`9784c4ef59e7a70580668a7c29208b3ea220c6344ddc52c0b3a5e35d0c174aab`.

The working-tree `Cargo.lock` was byte-identical before and after at
`480ce3dcd92ed7778e5fc922cc531039e187a96258ec78f6fa6937defb6a40e9`.
`rustfmt --check` on both R2 test files and scoped `git diff --check` passed.

## Named-root forbidden-symbol census

The forbidden pattern was
`WrapOfEpsilonScalar|WrapOfEpsilonMatch|wrap-of-epsilon-scalar|wrap_of_epsilon_scalar|wrap_of_epsilon_`.

| Named root | Paths | Matches |
|---|---|---:|
| active product | `crates/ir/src`, `crates/core/src` | 0 |
| active tests | `crates/ir/tests`, `crates/core/tests` | 0 |
| skinny | `skinny/crates` | 0 |
| Wasm | `wasm/src` | 0 |
| generated | `crates/core/src/grammar/generated` | 0 |
| runtime | `crates/core/src/runtime` | 0 |
| tooling | `xtask/src`, `skinny/xtask/src`, `scripts` | 0 |

## Remaining REDs

No RED remains in the scoped R2 executable payload. Both hostile review passes
restart from zero, so hostile clean count remains **0/2** until independent R2
critics complete. This author record does not adjudicate or move the wave.
