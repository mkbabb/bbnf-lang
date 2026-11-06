MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: R4 author.

# BB-C.W0 ambient optimizer authority — R4 Stage-A born-RED author freeze

- Frozen: `2026-07-22T19:44:58Z`
- Repository HEAD: `af15f63e0d2d3d719938c13b906a50acbb92ea3b`
- Host observation: Apple `arm64`
- Scope: Stage A only — canonical configured grammar-solve API plus the three
  pre-repair RED axes ordered by the R3 adjudication
- Execution status: **STATIC BORN-RED FREEZE; NO TEST OR BUILD RUN**
- Credit: **zero BB-C.W0, wave, lock, tranche, or movement credit**
- Hostile clean count: **0/2**
- Advancement: **BB-C.W1 may not begin**

No Cargo command, build, test executable, benchmark, mutation runner, x86
route, package/lock/config edit, lowerer repair, cancellation repair, or other
repository was used. This record freezes APIs and tests before repair; it does
not claim GREEN or acceptance.

## Governing frozen authority

| Record | SHA-256 |
|---|---|
| `waves/BB-C/W0.md` | `00bb99d6b3c379c7c15ad7a70ab7598f8b883f9cd8d3974733323bdbc6026df2` |
| `W0-AMBIENT-R2-ADJUDICATION.md` | `ef7c8c1db203eb42ae40358e35fd2710d4e79d3cabc5495801c619b18995b9f7` |
| `W0-AMBIENT-R3-AUTHOR.md` | `8a327e3da122dd253494f8512e2ca6504ba61107fbf9eeef935c62393ff3afe7` |
| `W0-AMBIENT-R3-NATIVE.md` | `b4fd9be7815a4af19d8f103f454e8395330758faaef85e86da8ac109e7f67f01` |
| `W0-AMBIENT-R3-CRITIC-A.md` | `138c59a8674cd7211fee9c22f950ff674592743dd4bac8ae86531c8560726d7c` |
| `W0-AMBIENT-R3-CRITIC-B.md` | `b4fbcce0430dd346a0c9d9eb05574028ace077ef738895cab818df3c40a542ed` |
| `W0-AMBIENT-R3-ADJUDICATION.md` | `0453ac333914204a3597693838906471999fcdcce77c7fd518be55071151b7fb` |

The R3 adjudication remains controlling. This Stage-A freeze neither edits nor
supersedes it.

## Exact Stage-A subject

`ABSENT` means the source-isomorphic lowerer test body was created by this
stage. Pre-hashes bind the dirty R3 handoff bytes before the Stage-A edits;
post-hashes bind the formatted Stage-A bytes.

| Path | Pre-Stage-A SHA-256 | Stage-A SHA-256 | Lines / bytes |
|---|---|---|---:|
| `crates/ir/src/passes/csp_strategy/mod.rs` | `7fdbaaf5790e09d6f7a22568b91ecadaf54e9f29f4ec4730ab250d59f4084897` | `66aa68ac63034cbb6aa9d7d8dffa1ea7639180be95d4992119805a54fd43be2c` | 1900 / 68608 |
| `crates/ir/src/passes/mod.rs` | `6805c17691a53997354fc818b34e85d8fb60da9799132a8c526e2b0d7b6120f9` | `b642e464f26d3da8012f354e0f86cbc05a26a2060d4af95b9a702897b7796272` | 81 / 3323 |
| `crates/core/src/pipeline/compile/pipeline.rs` | `ee917eeb17347c49f8eb908015f002ac4b4a4bc5bda4f896a3865815f82c3ca8` | `4573d5a8d6ff55b01dfe5fd092f25e666c107be38eaebc665d7343ca17e27278` | 466 / 21055 |
| `crates/ir/tests/lattices/csp_materialization.rs` | `5dbaab3fa856d230ee7f7523b2b67f227255aac91e99debe248f481ea16c08f3` | `07348a0ae1734b56b2ad9bab9bc68dc1e52d3093c80a1f65ce7b249d43f6b8f1` | 284 / 10274 |
| `crates/ir/tests/vm/cross_rule_csp.rs` | `2bff8d52dc0b9464218e33ff544d239c37ca21473cab76ee49f488358c3c2f10` | `6b7e882d8f3d10d936300957da7d8d3545bf0e61f1a37c94633454a59d4e49e2` | 742 / 25763 |
| `crates/ir/tests/vm/cost_weights_unified.rs` | `08b6f1032dc28e7fbc73fe63a0e8d5127787765abca973289673d100094f4e3d` | `7dd75a79852c07874bcb8e4ac078da19f8a231631ddfe24f3f62af90e5e3f348` | 604 / 25260 |
| `crates/ir/tests/lattices/csp_authority.rs` | `cf35fc122173668c9819c16c29ad19d348b2983663f0fb021b3b29b46a5ef207` | `d13b43ddd1df8b0f0a7d62d9edceec3d88d487b6cba1f3d34fbd619a6ae7c396` | 406 / 14765 |
| `crates/ir/tests/passes/csp_strategy/mod.rs` | `e217c090f529932015fb59f91efdfb82fbf02140a0571fd2c44723392a7029b9` | `3b67c4c30c0bfb18d3ec4288d597c068ae684936d310a71adaa7f5eecbccbfb3` | 436 / 13971 |
| `crates/core/src/lower/expression/wrap.rs` | `a626639367f2c7d3d45dfaa3f75fa7603871e28831c4fd117466aa9c622a2805` | `0b119d81599670418359f486f63dce5dea0e1bb35ce1fedf9feae36e7917470e` | 735 / 29963 |
| `crates/core/tests/lower/expression/wrap.rs` | `ABSENT` | `3f7b6e3637eaeed1cec393c27760e3419994254c5d972f917b1d79be3c954e77` | 122 / 4239 |

This evidence record itself was absent before Stage A. Its post-freeze digest is
reported by the author handoff because a file cannot contain its own SHA-256.

## Canonical configured surface

`GrammarSolveControl` is the sole public grammar-transaction control. It
contains only:

- `node_budget: Option<u64>`;
- `cancel: Option<CancelToken>`; and
- `Default = { node_budget: Some(1_000_000), cancel: None }`, matching the
  prior production posture.

The one public `solve_grammar_components` now takes
`(&GrammarIR, &GrammarSolveControl)`. It constructs the internal `SolveConfig`
with fixed `Pruning::ForwardChecking` and
`OptimizationMode::MinimizeCost`, copying only the two public control fields.
The former private `solve_grammar_components_with_config` path is deleted; no
wrapper, alias, or compatibility route remains.

There are exactly 15 call expressions: one production call, thirteen retained
IR test calls, and the new configured factorized-cancellation witness.
Production spells out the exact default control with
`node_budget: Some(1_000_000)` and `cancel: None`; retained tests pass
`GrammarSolveControl::default()`; the born-RED test passes its exact cancelled
control.

## Frozen born-RED witnesses

### Factorized grammar cancellation

`pre_cancelled_factorized_grammar_rejects_without_output` constructs a real
one-rule literal `GrammarIR`, builds its DAG and initial materialization facts,
pre-cancels a token, and calls the new public configured surface. Its only
acceptable result is typed `OptimizationUnavailableReason::Cancelled`; an
`Ok(GrammarSolveOutput)` panics and therefore proves that no output may escape.

It is expected RED against the frozen implementation: the zero-constraint
branch at `csp_strategy/mod.rs:889` decodes and returns `FactorizedExact`
without inspecting the token.

### Final singleton-leaf cancellation

`cancellation_on_final_singleton_leaf_discards_incumbent` creates one singleton
domain and a unary complete-assignment `LambdaConstraint` that flips the token
when the sole/final variable is assigned. It requires
`solve_exact_constrained` to reject the incumbent as typed `Cancelled`.

It is expected RED against the frozen implementation: solver search checks
`stack.is_empty()` at `search.rs:206` before its token guard at `:225`, while
the IR wrapper checks only `csp.stats().cancelled` at
`csp_strategy/mod.rs:1022`. The token is set, the leaf incumbent is accepted,
and the test's `expect_err` rejects that success. The existing
`cancellation_after_first_incumbent_discards_best_so_far` test remains present
and unchanged in meaning.

### Source-isomorphic grouped-map lowering

`crates/core/src/lower/expression/wrap.rs` contains only the exact external
test attachment; lowerer logic is unchanged. The external body freezes:

- `one_parenthesized_inner_map_lowers_only_the_inner_mapped_factor`;
- `multiple_parenthesized_levels_preserve_one_inner_map`;
- `outer_map_around_mapped_group_lowers_to_exact_nested_maps`;
- `literal_and_regex_arrows_are_not_direct_maps`; and
- `direct_typed_map_stays_on_its_source_factor_with_exact_descriptor`.

The first three require exact Map placement for valid grouped syntax; the
quoted/regex control covers double-, single-, and backtick-quoted arrows plus
both arrow spellings inside a regex; the direct control pins the Map to the
middle source factor and requires the exact `FnDescriptor::Expr`, integer
payload, and `U8` scalar type.

At minimum
`one_parenthesized_inner_map_lowers_only_the_inner_mapped_factor` is expected
RED against the unchanged lowerer. Its outer mapped factor still scans the
entire descendant-containing span with `find_unquoted` at `wrap.rs:241` and
panics when the nested arrow has no direct outer `value_expr` child. The
multiple-level and outer-map grouped cases traverse the same unchanged seam.

## Static evidence only

Formatting was applied only to the ten Rust paths in the subject table:

```text
rustfmt --edition 2024 <the ten exact Stage-A Rust paths>
```

Scoped `git diff --check` over the nine tracked Rust paths returned exit `0`
with no output. `git diff --no-index --check /dev/null` over the new lowerer
test returned the expected no-index difference status `1` with no whitespace
diagnostic.

Source censuses established:

- one `pub fn solve_grammar_components` definition;
- zero `solve_grammar_components_with_config` rows;
- zero source matches for a one-argument `solve_grammar_components` call;
- sixteen solve-symbol call/definition rows: one definition plus exactly
  fifteen call expressions;
- zero `#[test]` bodies in either touched production source owner; and
- exactly the two external `#[cfg(test)] #[path = ...] mod tests` attachments.

The unchanged RED seams remain source-visible:

```text
crates/ir/src/passes/csp_strategy/mod.rs:889: if constraint_counts.total() == 0 {
crates/ir/src/passes/csp_strategy/mod.rs:1022: if csp.stats().cancelled {
crates/csp-solver/src/solver/search.rs:206: if stack.is_empty() {
crates/csp-solver/src/solver/search.rs:225: && tok.is_cancelled()
crates/core/src/lower/expression/wrap.rs:241: find_unquoted(raw, "->")
```

No Cargo/build/test/benchmark command was run, so there is intentionally no
runtime output, exit receipt, or GREEN claim in this stage.

## Routed remainder and next root commands

The next root action is to execute each deterministic RED witness once on the
native Apple-ARM route and freeze the raw outputs before any repair:

```text
/usr/bin/arch -arm64 env CARGO_BUILD_JOBS=1 cargo test --target aarch64-apple-darwin -p bbnf-ir --lib passes::csp_strategy::tests::pre_cancelled_factorized_grammar_rejects_without_output -- --exact --nocapture

/usr/bin/arch -arm64 env CARGO_BUILD_JOBS=1 cargo test --target aarch64-apple-darwin -p bbnf-ir --lib passes::csp_strategy::tests::cancellation_on_final_singleton_leaf_discards_incumbent -- --exact --nocapture

/usr/bin/arch -arm64 env CARGO_BUILD_JOBS=1 cargo test --target aarch64-apple-darwin -p bbnf --lib lower::expression::wrap::tests::one_parenthesized_inner_map_lowers_only_the_inner_mapped_factor -- --exact --nocapture
```

Only after those RED preimages are frozen may the next stage repair
transaction-boundary cancellation and direct-depth grouped-arrow detection.
Goldilocks movement, report sorting, mutation matrices, warning cleanup,
lowerer repair, cancellation repair, native GREEN, critics, and adjudication
remain explicitly outside this Stage-A artifact.

**STAGE-A BORN-RED FREEZE ONLY. NO RUN. CREDIT 0. CLEAN COUNT 0/2. BB-C.W1 MAY NOT BEGIN.**
