MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: BB-C.W0 item-3 R5 repair author.

# BB-C.W0 ambient optimizer authority — R5 mutation manifest

- Frozen: `2026-07-22T22:24:02Z`
- Repository HEAD: `af15f63e0d2d3d719938c13b906a50acbb92ea3b`
- Host observation: Apple `arm64`
- Governing born-RED receipt: `W0-AMBIENT-R5-BORN-RED-AUTHOR.md`
- Scope: the nine one-hunk counterfactuals ordered for the R5 repair
- Execution: **no patch was applied and no mutant, test, build, or benchmark ran**
- Validation: all nine exact patches returned exit `0` from read-only
  `git apply --check`
- Credit: zero native GREEN, mutation, BB-C.W0, wave, lock, tranche, or movement
  credit

These specimens invert one repaired semantic boundary apiece. They are plain
unified patches, not a mutation framework. The root-owned Apple-ARM campaign
must apply each one alone, run only its named selector, restore immediately,
and prove the target preimage hash before proceeding.

## Exact specimens and killing oracles

| ID | Patch SHA-256 | Lines / bytes | Target preimage SHA-256 | Counterfactual | External killing oracle | Required semantic kill |
|---|---|---:|---|---|---|---|
| R5-M01 | `9ddc742011c265d4e2c930645e6b7cff2d3721e9d158f2487d4707afb2e16c08` | 15 / 897 | `crates/core/src/lower/expression/wrap.rs` `10fccfa027a205294991a3e3a05e6e14777f13f6d5746db39e28ade74582a39c` | Remove the exact `Factor`/`->` boundary validation and give suffix inference a slice beginning at the factor end. | `lower::expression::wrap::tests::division_inside_group_does_not_hide_outer_typed_map`; `block_comment_slash_arrow_is_not_a_map` remains the ownership control. | The outer numeric literal loses exact `U8` because `-> 1u8` is not one bounded numeric literal. |
| R5-M02 | `f09981cd6ea09d6b30e47701ffc28eaff988c91327d4d6795f7c6ba1ac2aeff4` | 11 / 589 | `crates/ir/src/passes/csp_strategy/solve.rs` `22c90e77ab6debbd74de38e2e88437f4c13131fb573342d9727590c6e91aa0c7` | Retain a local objective after merge rewrites the report to `InvalidAssignment`. | `cross_rule_csp::conflicting_disconnected_shared_node_is_not_published` | The exact second-report `objective == None` assertion fails after its preceding transaction assertions pass. |
| R5-M03 | `10360927790dc81f10fd462475b494ff3cdafc6c03d5b9b846192904e593fc5b` | 19 / 770 | `crates/ir/src/passes/csp_strategy/solve.rs` `22c90e77ab6debbd74de38e2e88437f4c13131fb573342d9727590c6e91aa0c7` | Delete the sole post-decode/pre-merge cancellation boundary. | `passes::csp_strategy::tests::factorized_acceptance_observes_boundary_cancellation`; `passes::csp_strategy::tests::constrained_acceptance_observes_boundary_cancellation` | Both proven local routes illicitly commit instead of producing one cancelled, objective-free failing report. |
| R5-M04 | `7716a1a7970cfef54bb21212f9b028f617cf971617a84aad2077e08d995b6fc9` | 23 / 849 | `crates/ir/src/passes/csp_strategy/solve.rs` `22c90e77ab6debbd74de38e2e88437f4c13131fb573342d9727590c6e91aa0c7` | Delete the sole before-next-component cancellation boundary. | `passes::csp_strategy::tests::completed_component_then_cancel_reports_completed_plus_boundary` | The second component proceeds past the boundary instead of preserving the first report plus an exact cancelled boundary report. |
| R5-M05 | `ac2df3fb820c22884edfef43c8f063a1ae16959fee41f6db0d662a33abd04a0f` | 18 / 766 | `crates/ir/src/passes/csp_strategy/solve.rs` `22c90e77ab6debbd74de38e2e88437f4c13131fb573342d9727590c6e91aa0c7` | Delete the final-return cancellation boundary. | `passes::csp_strategy::tests::final_return_observes_boundary_cancellation` | Accumulated decisions escape as `Ok` instead of the last report becoming cancelled and objective-free. |
| R5-M06 | `ce8672ba1e3961e5ec93d0051186b45166cab2fc126021d648c562c6d8f7ff9b` | 10 / 427 | `crates/ir/src/passes/csp_strategy/solve.rs` `22c90e77ab6debbd74de38e2e88437f4c13131fb573342d9727590c6e91aa0c7` | Ignore only the solver-stats cancellation leg while retaining the current-token leg. | `passes::csp_strategy::tests::solver_stats_cancellation_is_not_publishable` | A stats-only cancellation is misclassified as publishable. |
| R5-M07 | `d9dc722da04dc09aee25e1596b165d8a38ba042f1191f58e939181e5f3edc6ab` | 10 / 426 | `crates/ir/src/passes/csp_strategy/solve.rs` `22c90e77ab6debbd74de38e2e88437f4c13131fb573342d9727590c6e91aa0c7` | Ignore only the current-token cancellation leg while retaining the solver-stats leg. | `passes::csp_strategy::tests::current_token_cancellation_is_not_publishable` | A current-token-only cancellation is misclassified as publishable. |
| R5-M08 | `24a200d2d169ee916e59507dc518848557f03ea6c58249d64e34d680049bb194` | 18 / 676 | `crates/ir/src/passes/csp_strategy/site.rs` `5b7b002acec4c3be75d2c7172af059195fce0eb8349bc8bb8ee3cc77776836b9` | Remove exact complete-solution cardinality validation while retaining VarId/family checks. | `passes::csp_strategy::tests::decoder_rejects_incomplete_and_oversized_assignments` | One tuple assertion observes both outcomes: incomplete is displaced from exact `Search`, while oversized reaches illicit success. |
| R5-M09 | `fafeb9c4d589df3ceda50c562d2255605856895aa2064b0a61a4059738d5e987` | 12 / 634 | `crates/ir/src/passes/csp_strategy/site.rs` `5b7b002acec4c3be75d2c7172af059195fce0eb8349bc8bb8ee3cc77776836b9` | Clamp a foreign `VarId` onto the last real variable while leaving length validation intact. | `passes::csp_strategy::tests::decoder_rejects_foreign_var_and_missing_value` | One tuple assertion observes both outcomes: the foreign assignment is accepted instead of returning its exact foreign-variable locus, while the missing-site result remains exact `Search`. |

## Oracle identities

| Owner | SHA-256 | Lines / bytes |
|---|---|---:|
| `crates/core/tests/lower/expression/wrap.rs` | `6b1190cd7bd3d26c71d2429b7eec7d0e2c5e2e9ce9ae4138a9e9708243ee8a48` | 212 / 6,749 |
| `crates/ir/tests/passes/csp_strategy/mod.rs` | `8c3f081962485f1c9cff0264848d72c1d8f2ea5045f052de2fbce7669d1f0436` | 679 / 22,695 |
| `crates/ir/tests/vm/cross_rule_csp.rs` | `ac5c2292151284d9193f7025aae84f038e3cbcba182f8c6fd90ae61877b24f52` | 793 / 27,493 |

The M08 and M09 selectors map each `Result` to an `Option<locus>` and compare
both outcomes in one exact tuple assertion. No early assertion can obscure the
second observed outcome.

## Bound restoration identities

After all nine apply-checks, each target remained byte-identical to its bound
preimage:

| Target | Pre-check and post-check SHA-256 |
|---|---|
| `crates/core/src/lower/expression/wrap.rs` | `10fccfa027a205294991a3e3a05e6e14777f13f6d5746db39e28ade74582a39c` |
| `crates/ir/src/passes/csp_strategy/solve.rs` | `22c90e77ab6debbd74de38e2e88437f4c13131fb573342d9727590c6e91aa0c7` |
| `crates/ir/src/passes/csp_strategy/site.rs` | `5b7b002acec4c3be75d2c7172af059195fce0eb8349bc8bb8ee3cc77776836b9` |

`PATCHES.sha256` is the exact digest ledger. `SOURCE-RESTORATION.sha256`
binds the unchanged target bytes. `APPLY-CHECK.log` records the static outcome;
it does not imply dynamic mutant credit.
