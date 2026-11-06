# BB-C.W0 ambient optimizer authority — R5 root-native receipt

- Frozen: `2026-07-22`
- Repository HEAD: `af15f63e0d2d3d719938c13b906a50acbb92ea3b`
- Executor: root-owned native gate; no delegated judgment is claimed by this receipt
- Host and target: Apple `arm64` / `aarch64-apple-darwin`; no x86 route was used
- Toolchain: `rustc 1.96.0-nightly (02c7f9bec 2026-04-10)`, LLVM `22.1.2`; `cargo 1.96.0-nightly (eb94155a9 2026-04-09)`
- Governing author receipt: SHA-256 `7dc4da5962e1ee76f32b0816f7ee8e586293271ba49aff4514a419794b4878a1`
- Governing mutation manifest: SHA-256 `e3fa15f54cba41df0067d800787dec3f98f7a5889640cbc9e05c9c1126343512`
- Result: **NATIVE GREEN 7/7; 69/69 selected candidate tests passed; MUTANTS KILLED 9/9 by 10/10 selected tests**
- Retries: none. Every candidate command and every mutant command executed exactly once.
- Credit: **zero BB-C.W0, wave, lock, tranche, or movement credit pending two fresh hostile critics and adjudication**
- Hostile clean count: **0/2**
- Advancement: **BB-C.W1 may not begin**

This receipt is the first post-repair execution of the frozen R5 candidate. The
earlier author-side library checks and generator build remain compile/provenance
observations only. Every command below used `/usr/bin/arch -arm64`,
`CARGO_BUILD_JOBS=1`, and `--target aarch64-apple-darwin`.

## Candidate native gates

| Gate | Exact cargo selection | Result | Raw log SHA-256 | Lines / bytes |
|---|---|---:|---|---:|
| G1 | `cargo test -p bbnf-ir --lib passes::csp_strategy::tests -- --nocapture` | 21 passed | `ccea0668f41d567d1f7150f9a0b402cc6833b7ee5243a1e63599abfce0dab732` | 29 / 2,347 |
| G2 | `cargo test -p bbnf-ir --test vm cross_rule_csp -- --nocapture` | 12 passed | `cfa82d7db4ae5a3c300e281bd5c38ccf801f3dbdf0a8d6e1188377f5718fdf5f` | 20 / 1,238 |
| G3 | `cargo test -p bbnf --lib lower::expression::wrap::tests -- --nocapture` | 8 passed | `e2ee039cd927e99174631a56f8bb2668ce874d635ddfe89e8190733f1fb89bf9` | 1,128 / 42,950 |
| G4 | `cargo test -p bbnf --lib pipeline::compile::pipeline::tests -- --nocapture` | 3 passed | `ad92aa5c70a15d3f18d5d50b994f1fb34f227a17747f073b6f25d50a348d2a0e` | 1,120 / 42,262 |
| G5 | `cargo test -p bbnf-ir --test lattices csp_authority -- --nocapture` | 5 passed | `a552a577fbded2aaf2cefdf49dce2bd1fb89af5b16f409e23af7d0c81591f85d` | 13 / 784 |
| G6 | `cargo test -p bbnf-ir --test lattices csp_materialization -- --nocapture` | 9 passed | `1154e0b0acf541eaa2a3c42e0b08850e92059ce336e434fb57985d27815c6088` | 16 / 936 |
| G7 | `cargo test -p bbnf-ir --test vm cost_weights_unified:: -- --nocapture` | 11 passed | `08d562a21709fdad46687c2e4d7172c822a426e4a0be32c12503180baf9d9999` | 18 / 1,179 |

The 69 selected tests cover the repaired structural map boundary and generated
Pratt spans; grammar-wide transaction entry, acceptance, and final-return
boundaries; factorized and constrained cancellation; invalid-merge report
sanitization; decoder cardinality and variable totality; public VM error and
cancellation propagation; CSP authority/materialization; and unified cost
weighting.

G3 and G4 each report the repository's existing generated-code warning bank as
183 warnings. Their logs contain 184 `warning:` headers because the cargo
summary is itself one header. This debt is preserved, not attributed to R5 and
not reclassified as clean. No warning was emitted by the candidate G1, G2,
G5, G6, or G7 selections.

## Surgical mutation execution

Each exact frozen patch was applied alone to its bound source preimage, its
single named command executed once, the patch was reversed immediately, and
the source SHA-256 was verified before proceeding. Exit `101` is the required
semantic test failure, not infrastructure failure.

| Mutant | Selected oracle(s) | Required observed failure | Exit | Raw log SHA-256 | Lines / bytes |
|---|---|---|---:|---|---:|
| M01 | `division_inside_group_does_not_hide_outer_typed_map` | inferred `U8` becomes `None` | 101 | `63926b12ae2af7d1fca621ac17f2dfe64a490e244e577e8b404027edebca0288` | 1,131 / 42,687 |
| M02 | `conflicting_disconnected_shared_node_is_not_published` | invalid report retains `Some(ObjectiveProof)` | 101 | `f60cbd4bbdc48ff3b9c5d15c6acd27f97628312f572c4c32fea8c61790f2f914` | 21 / 926 |
| M03 | filter `acceptance_observes_boundary_cancellation` | factorized and constrained routes both commit | 101 | `6eef956ffcb5826460e6a22bd0f7988663e936e7de09242946cb94ef41baf41a` | 24 / 1,382 |
| M04 | `completed_component_then_cancel_reports_completed_plus_boundary` | next-component boundary misses cancellation | 101 | `6912104e44be664a3a4c01a3d6b8fcc05b6539e2b23a64aa4161b1cc31c16666` | 28 / 1,309 |
| M05 | `final_return_observes_boundary_cancellation` | complete grammar output escapes | 101 | `8e606af5d621469e89cbfd34afa9b91ed483d216673716d42aa533e11b1d4d8b` | 30 / 2,041 |
| M06 | `solver_stats_cancellation_is_not_publishable` | truth cell `(true, false)` becomes false | 101 | `c77cf9ef21e4f3b1768478072f8510a297bbfdb4689937aafcaee03ad2b097d9` | 28 / 1,405 |
| M07 | `current_token_cancellation_is_not_publishable` | truth cell `(false, true)` becomes false | 101 | `6a761bf026dce5ea70f054116ba648a813bed15ad82dc6806810fca5a2fa4c73` | 28 / 1,412 |
| M08 | `decoder_rejects_incomplete_and_oversized_assignments` | observed tuple is `(Variable(0), success)` rather than `(Search, Search)` | 101 | `0b2d4281dd53a19f7898074c72406c5b88cc6aa79cd7a28d7326750c1efa0ab8` | 21 / 1,004 |
| M09 | `decoder_rejects_foreign_var_and_missing_value` | observed tuple is `(success, Search)` rather than `(Variable(1), Search)` | 101 | `ab9f3cba463e2df2309a7f41563e979fc08fec3cd21efbc80a98f04c74c54eaa` | 21 / 983 |

M03 ran two tests in one filtered command, so the nine command executions kill
ten independently reported test cases. M08 and M09 compute both `Result`
outcomes before their tuple assertion; neither result can prevent observation
of the other.

M01 recompiles the core crate and therefore repeats the same 183-warning
generated bank. M04, M05, M06, and M07 each create exactly one local warning by
making a repaired function, mutability, or parameter unused; those warnings
corroborate the intended surgical deletion and disappear on exact restoration.
No other mutant warning appears.

## Transport friction and non-events

Before M01 executed, the repository editor rejected the stored Git-style
numbered hunk header. It changed no source byte and launched no command. The
transport was then losslessly converted to the editor's unnumbered `@@` form;
the patch body, target preimage, test command, and one-execution rule were
unchanged. This is not a retry because no mutant or oracle ran.

No mutant survived. No command was rerun, broadened after failure, or replaced
with a weaker selector. No benchmark, full workspace suite, x86 binary, package
mutation, stage, commit, or cross-repository write occurred.

## Exact restoration closure

After all execution, the three mutation targets are byte-identical to their
frozen candidate preimages:

| Target | Restored SHA-256 |
|---|---|
| `crates/core/src/lower/expression/wrap.rs` | `10fccfa027a205294991a3e3a05e6e14777f13f6d5746db39e28ade74582a39c` |
| `crates/ir/src/passes/csp_strategy/solve.rs` | `22c90e77ab6debbd74de38e2e88437f4c13131fb573342d9727590c6e91aa0c7` |
| `crates/ir/src/passes/csp_strategy/site.rs` | `5b7b002acec4c3be75d2c7172af059195fce0eb8349bc8bb8ee3cc77776836b9` |

`SOURCE-RESTORATION.sha256` verifies 3/3 from the repository root and
`PATCHES.sha256` verifies 9/9 from the mutation directory. `Cargo.lock` remains
byte-identical at SHA-256
`480ce3dcd92ed7778e5fc922cc531039e187a96258ec78f6fa6937defb6a40e9`.
The generated BBNF parser, registry sidecar, Pratt emitter, lowerer, solver,
external oracles, and restored temporary Apple binding all retain the exact
hashes bound by `W0-AMBIENT-R5-AUTHOR.md`. `git diff --check` is empty.

## Required continuation

Two fresh critics must now assume the R5 wave is wrong and independently assay
the implementation, generator provenance, positive gates, surgical mutation
power, warning debt, tranche fit, KISS/clean-break compliance, and all omitted
obligations. A third fresh adjudicator must prove each disputed fact correct,
incorrect, or unproven. Only that adjudication can award R5 credit or determine
the next technically correct BB-C.W0 action.

**R5 NATIVE GREEN 7/7. MUTANTS KILLED 9/9. SOURCE RESTORATION 3/3. HOSTILE CLEAN 0/2. CREDIT 0. BB-C.W1 MAY NOT BEGIN.**
