MODEL: gpt-5.6-sol; REASONING: ultra; WORKFLOW: v2

# BB-C.W0 ambient optimizer authority — R2 author record

- Recorded: `2026-07-22T17:56:54Z`
- Repository HEAD: `af15f63e0d2d3d719938c13b906a50acbb92ea3b`
- Scope: smallest cohesive ambient-authority R2 under the explicit
  `bbnf-lang` exception, following `W0-AMBIENT-R1-ADJUDICATION.md`.
- Credit: **zero BB-C.W0, wave, lock, tranche, or movement credit pending two
  fresh hostile critics and fresh adjudication**.

## Frozen R2 subject

| Path | SHA-256 |
|---|---|
| `crates/ir/src/passes/csp_strategy/mod.rs` | `81c61d987ae71f0aa20c99dfd93e2e53ba1f75c18ffca9497acf381cbaaea6bd` |
| `crates/ir/src/passes/csp_strategy/components.rs` | `07a235d333992da0f70c6ff5e27e4b036828c0026bf531ba3f9414c3a910131a` |
| `crates/ir/src/passes/mod.rs` | `ef505f512d129d358992b910b575a02f4c84b0adc455adcecf701bdcafd0b46d` |
| `crates/core/src/pipeline.rs` | `5eac07248f09df5539f5265e2c9f011c6c9e793d3d0049d39dba764b109b82c4` |
| `crates/core/src/pipeline/compile/pipeline.rs` | `bc02d7ea79fe4c39765b164b491d040f233470df9fbd9de8b8370748e7a676ab` |
| `crates/core/src/pipeline/compile/pipeline/tests.rs` | `bff62ebf9b01db50e46b291a793b55884c4ca8f0c7f99d0d68a0a0b449cdffe8` |
| `crates/ir/tests/lattices/csp_authority.rs` | `cf35fc122173668c9819c16c29ad19d348b2983663f0fb021b3b29b46a5ef207` |
| `crates/ir/tests/lattices/csp_materialization.rs` | `5dbaab3fa856d230ee7f7523b2b67f227255aac91e99debe248f481ea16c08f3` |
| `crates/ir/tests/vm/cost_weights_unified.rs` | `08b6f1032dc28e7fbc73fe63a0e8d5127787765abca973289673d100094f4e3d` |
| `crates/ir/tests/vm/cross_rule_csp.rs` | `6ae94ebcf215790869d86e2a24958faa437655f6a6b638a1a90d9e452561041f` |
| `crates/core/examples/egraph_fire_probe.rs` | `039545fa8e85993e77b81c96cadb12ca69263f0965b15e7d1c7a50bdc8f2ad01` |
| `crates/core/tests/named_pipeline_probe.rs` | `e763d8cbbd2cd5cbf04897af3797fb934cdc9c85293dd52c96579d9244c279b7` |
| `crates/core/tests/named_type_preservation.rs` | `4992f15120c9f157bb85eb8757aba4b3dd14c4add1ed38efa29ae43891d3887c` |
| `crates/core/tests/pipeline_compile_request.rs` | `5f72b13b49b3b9d02889f3cad7c401cb37683187cb95276810dd474db326cd54` |
| `crates/ir/src/egraph/rules/suffix.rs` | `536db8f9da8ccdcf8fec1f784a630eb475cf4f41e6ff836becd7ffff6f9cee6a` |
| `crates/ir/tests/egraph/egraph_analysis.rs` | `fd0d5c78c589e7fd3e97acd2ea44c68e41c5944d6d5cedeca80d541a3ab2aa4d` |
| `crates/ir/tests/lattices/materialization_eclass_gate.rs` | `8d94696836dbc2a0e46f9f5dd4d7b7a73bbe42848c649b4c7fb6a1d240e3d253` |
| `crates/ir/src/types/grammar.rs` | `49842eebcb6b015da50c69cf0289e20339da46b16a63c1c2820b429d0111f8bc` |
| `crates/ir/src/passes/transform/pattern_dedup.rs` | `2835ea62e2bbe6b64275f1446dab664a76e79f6bd03e6b209bf3011d22f29086` |
| `crates/ir/src/passes/materialization/classify.rs` | `938085ba30d9c0013e9645f631935430907f2632538e6d307de5b19331b81236` |
| `crates/ir/src/passes/materialization/mod.rs` | `3cc249fa7934dd4bd456ffe7015848b11b1e77384738960dfe7808b5162d010c` |

The deleted R1 flat witness is absent. Its replacement is the private pipeline
module test above, so it exercises the same production function without
introducing a public or integration-test-only pipeline route.

Relative to repository HEAD, the tracked frozen slice is `+569/-438`; the new
142-line private pipeline witness makes the complete frozen code/prose slice
`+711/-438` (net `+273`). Most CSP growth is mutation coverage; the production
solver deletes both fallback families and keeps one local transactional
accumulator.

## Implemented contract

`solve_grammar_components` now returns
`Result<GrammarSolveOutput, OptimizationUnavailable>`. The success value carries
only the complete recognizer and materialization maps. The typed unavailable
record carries one of `MissingDag`, `BudgetExceeded`, `Unsatisfiable`, or
`InvalidAssignment`, plus the failing component root when a DAG partition
exists. There is no success payload inside the error.

Each component solves into private maps. The grammar-local accumulator is never
returned on error, and the production pipeline calls `install_grammar_solution`
before mutating either sidecar. A late component failure therefore publishes no
earlier component output. Hash-consed `NodeId` collisions across disconnected
components are merged only when their values agree; a differing duplicate is
`InvalidAssignment`, not a silent overwrite.

The zero-constraint route is named `decode_exact_factorized_optimum`. It runs
only when the installed-constraint count is exactly zero and proves one site per
variable, non-empty domains, aligned value/cost vectors, finite costs, value
typing/domain membership, and complete coverage. Equal costs retain domain
order deterministically. The constrained route discards every best-so-far
solution when the node budget fires, reports unsatisfiability when no complete
solution exists, and validates the returned assignment before decoding.

The independent-minimum budget fallback, unsat priority fallback, and their
Alt/Wrap/Engine helpers are deleted. Component engine-variable indexing is a
rule-local body walk consulting the component map: it neither relabels prior
rules nor omits a structurally shared node.

## Production-isomorphic witness and mutation tests

The direct witness compiles this grammar through `compile_grammar`:

```bbnf
branch = ("x" -> 0u8), "z" | ("y" -> 1u8), "z" ;
start = "!", branch ;
```

It requires both Alt arms to remain exact two-item Seqs, each arm's Map to wrap
the corresponding `x`/`y` literal, each distinct `FnId` to resolve to the exact
`MapExpr::IntLit(0/1)` with `Some(TypeDesc::U8)`, and `node_type(Map)` to be
`U8`. It then checks the actual VM contract: `!xz` and `!yz` accept at offset 3
with exact nested tags/spans; `!x` rejects with restored offset 1, retains the
partial tagged start value over `(0,1)`, and emits one branch diagnostic at
offset 2 with an empty expected set.

Mutation-focused tests cover budget exhaustion, unsatisfiability,
missing/mistyped/incomplete assignments, non-finite factorized costs,
deterministic ties, missing DAG, late pipeline failure with unchanged sidecars,
and conflicting disconnected rules that share one hash-consed `NodeId`.

## Research differential disposition

No legacy grammar-egraph differential is claimed in R2. The preserved research
builder does not bind both an explicit complete scheduler configuration and a
complete scheduler report, and production must not gain a second route merely
to create a differential. That work remains an item-2 dependency and earns no
credit here.

Routed remainder is explicit: executable GREEN for the direct witness awaits a
separately authorized run on a host that can link `bbnf-path`; the mutation
tests likewise carry no execution credit from this RED host. Historical prose
outside the adjudicated active source/test/example set, and the protected G3
semantic witness, are not re-owned by R2. No further production or differential
path is routed into this item.

## Reproducible censuses

Production grammar-egraph callers:

```text
rg -n 'bbnf_ir::egraph::(build_and_saturate|write_back_optimized|GrammarCostModel)|egraph_build_saturate_writeback|pool\.write_back' crates/core/src
```

Result: no rows, exit `1`.

CSP fallback symbols/routes:

```text
rg -n 'decode_min_cost_per_variable|decode_fallback|falling back to per-variable|fallback_(alt|wrap|engine)' crates/ir/src/passes/csp_strategy crates/core/src/pipeline
```

Result: no rows, exit `1`.

Adjudicated stale production-egraph phrases:

```text
rg -n 'production e-graph construction|production_build_and_saturate|pipeline gates `egraph_build_saturate_writeback`|normalizer \+ e-graph|post-extraction|e-graph saturation block|during egraph saturation' crates/core/tests/named_pipeline_probe.rs crates/core/tests/named_type_preservation.rs crates/core/examples/egraph_fire_probe.rs crates/ir/src/egraph/rules/suffix.rs crates/ir/tests/egraph/egraph_analysis.rs crates/ir/tests/lattices/materialization_eclass_gate.rs crates/ir/src/types/grammar.rs crates/ir/src/passes/transform/pattern_dedup.rs crates/ir/src/passes/materialization/classify.rs crates/ir/src/passes/materialization/mod.rs crates/core/tests/pipeline_compile_request.rs
```

Result: no rows, exit `1`. The protected G3 witness is intentionally excluded.

## Formatting and build/test identity

`rustfmt --edition 2024 --check` over every frozen Rust subject completed with
exit `0` and empty stdout/stderr. Scoped `git diff --check` also completed with
exit `0` and empty stdout/stderr.

Check command (warm target; no clean and no retry):

```text
/usr/bin/arch -arm64 cargo check --target aarch64-apple-darwin -p bbnf-ir -p bbnf --tests
```

Result: exit `101`. Compilation reached `bbnf-ir` and `bbnf`; the unrelated
`bbnf-path` dylib link was killed before the check could complete. Raw terminal
identity ended:

```text
error: linking with `cc` failed: signal: 9 (SIGKILL)
error: could not compile `bbnf-path` (lib) due to 1 previous error
warning: build failed, waiting for other jobs to finish...
```

Sole focused native test command (one build job; no retry):

```text
/usr/bin/arch -arm64 env CARGO_BUILD_JOBS=1 cargo test --target aarch64-apple-darwin -p bbnf --lib pipeline::compile::pipeline::tests -- --nocapture
```

Result: exit `101`; no test executed. Raw terminal identity was:

```text
Compiling bbnf-ir v0.1.6 (/Users/mkbabb/Programming/bbnf-lang/crates/ir)
Compiling bbnf-path v0.1.0 (/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-path)
error: linking with `cc` failed: signal: 9 (SIGKILL)
error: could not compile `bbnf-path` (lib) due to 1 previous error
```

Machine identity: `/usr/bin/arch = arm64`; `uname -m = arm64`. Both linker
invocations contained `-arch arm64`. This is bounded RED execution evidence,
not executable GREEN and not critic credit.

## Preservation

| Protected path | SHA-256 |
|---|---|
| `crates/ir/src/egraph/rules/universal.rs` | `9835616cc8900eb5e0e5fde769e34d149bfcdf8e2493f0aef74b958426768b5b` |
| `crates/ir/src/egraph/rules/mod.rs` | `da0a7b1b9120e6d3110582254824fc6cb6509b181aadd13332c39ab80cc897c4` |
| `crates/ir/tests/egraph/egraph_universal.rs` | `e4e9beab261c1a10a52d537f2d773e8f69c051e763918f3d35814d1252576ede` |
| `crates/core/tests/egraph_semantic.rs` | `ef08223fdce747c821a0d11db54057314aaefb8a57bc41c8f14a9ba12b5aeaf2` |
| `crates/core/Cargo.toml` | `ca7720900e9a573f3ce7057c126ddafd6fe595396791f0e067060f2411a12a85` |
| `Cargo.lock` | `480ce3dcd92ed7778e5fc922cc531039e187a96258ec78f6fa6937defb6a40e9` |
| `.cargo/config.toml` | `6e0d39e574a75a370b32551dd9fa21262ebd1b593185db320179b9398d220518` |

No Cargo manifest, configuration, lockfile, generated/runtime file, Parse-that,
Value, Glass/playground/frontend, or G3-owned byte was intentionally edited by
this R2. Unrelated pre-existing dirt remains untouched.

## Terminal status

R2 is implemented and frozen, but executable evidence remains RED because the
host killed the unrelated dependency link before test execution. Hostile clean
count is **0/2**. No completion, movement, wave, lock, tranche, or item credit is
claimed pending two fresh critics and fresh adjudication.
