MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: BB-C.W0 item-3 R6 KISS repair author.

# BB-C.W0 ambient optimizer authority — R6 mutation manifest

- Frozen: `2026-07-22T23:17:28Z`
- Repository HEAD: `af15f63e0d2d3d719938c13b906a50acbb92ea3b`
- Host observation: Apple `arm64`
- Governing adjudication: `W0-AMBIENT-R5-ADJUDICATION.md`, SHA-256
  `df6c714c8f7c0a487b3f019a8c328b144e119fd661ce6f548e863d209a377da1`
- Scope: exactly eight one-hunk R6 counterfactuals; **there is no M06**
- Author execution: no patch was applied and no test, mutant, build, profile, or
  generator ran
- Static validation: every exact patch has one hunk and returned exit `0` from
  `git apply --check`
- Credit: zero native GREEN, mutation, item, wave, lock, tranche, prototype,
  performance, or movement credit

These are plain unified patches, not a framework. The root Apple-ARM campaign
must apply one specimen alone, run only its bound oracle epoch, restore it, and
verify the exact source restoration hash before beginning the next specimen.

## Exact specimens

| ID | Patch SHA-256 | Lines / bytes | Target and restoration SHA-256 | One semantic counterfactual | Bound oracle epoch |
|---|---|---:|---|---|---|
| R6-M01 | `4ab384e668d8da4824913499fa28b72fbbf6b1407702a07945ebdcfec93cf1e0` | 12 / 772 | `wrap.rs` `7edbb5bca06a63304615afb4b0560f73a0bb3d6df3439b7aa8c633f6cd323f17` | Widen only the RHS start from structural `value_lo` to `factor_hi`. | In one mutant epoch, run the division killer and then the block-comment ownership control. Division must fail by losing exact `U8`; the comment control must remain GREEN with zero map/function. |
| R6-M02 | `f09981cd6ea09d6b30e47701ffc28eaff988c91327d4d6795f7c6ba1ac2aeff4` | 11 / 589 | `solve.rs` `aac939e7a68261d50dcbcf4961c638a206e71d780d95edf26dd71e1017ccf08e` | Retain an objective after merge failure. | `cross_rule_csp::conflicting_disconnected_shared_node_is_not_published` |
| R6-M03 | `10360927790dc81f10fd462475b494ff3cdafc6c03d5b9b846192904e593fc5b` | 19 / 770 | `solve.rs` `aac939e7a68261d50dcbcf4961c638a206e71d780d95edf26dd71e1017ccf08e` | Delete post-decode/pre-merge cancellation. | One filter selecting both `factorized_acceptance_observes_boundary_cancellation` and `constrained_acceptance_observes_boundary_cancellation` |
| R6-M04 | `7716a1a7970cfef54bb21212f9b028f617cf971617a84aad2077e08d995b6fc9` | 23 / 849 | `solve.rs` `aac939e7a68261d50dcbcf4961c638a206e71d780d95edf26dd71e1017ccf08e` | Delete before-next-component cancellation. | `completed_component_then_cancel_reports_completed_plus_boundary` |
| R6-M05 | `ac2df3fb820c22884edfef43c8f063a1ae16959fee41f6db0d662a33abd04a0f` | 18 / 766 | `solve.rs` `aac939e7a68261d50dcbcf4961c638a206e71d780d95edf26dd71e1017ccf08e` | Delete final-return cancellation. | `final_return_observes_boundary_cancellation` |
| R6-M07 | `4b490d8ff3b354bea474b23dce17e6222425bcfc8ca5f921047e94f7b7cc2d53` | 16 / 691 | `solve.rs` `aac939e7a68261d50dcbcf4961c638a206e71d780d95edf26dd71e1017ccf08e` | Delete the actual current-token post-solve guard. | Real production route `cancellation_on_final_singleton_leaf_discards_incumbent` |
| R6-M08 | `24a200d2d169ee916e59507dc518848557f03ea6c58249d64e34d680049bb194` | 18 / 676 | `site.rs` `5b7b002acec4c3be75d2c7172af059195fce0eb8349bc8bb8ee3cc77776836b9` | Remove exact complete-solution cardinality validation. | `decoder_rejects_incomplete_and_oversized_assignments` |
| R6-M09 | `fafeb9c4d589df3ceda50c562d2255605856895aa2064b0a61a4059738d5e987` | 12 / 634 | `site.rs` `5b7b002acec4c3be75d2c7172af059195fce0eb8349bc8bb8ee3cc77776836b9` | Clamp a foreign `VarId` onto a real variable. | `decoder_rejects_foreign_var_and_missing_value` |

M01 no longer deletes the already-retired textual arrow assertion. Its two
explicitly sequenced selectors share one applied-mutant epoch, so the killer
and the no-false-owner control observe the same bytes. M07 no longer assays a
fabricated Boolean helper cell; it removes production code and reaches the
existing constrained solver route that cancels on the final singleton leaf.

## Root-only Apple-ARM command formation

Every command below is a prescription, not an author execution record. Prefix
each command with `/usr/bin/arch -arm64 env CARGO_BUILD_JOBS=1`.

```text
M01-A cargo test --target aarch64-apple-darwin -p bbnf --lib lower::expression::wrap::tests::division_inside_group_does_not_hide_outer_typed_map -- --exact --nocapture
M01-B cargo test --target aarch64-apple-darwin -p bbnf --lib lower::expression::wrap::tests::block_comment_slash_arrow_is_not_a_map -- --exact --nocapture
M02 cargo test --target aarch64-apple-darwin -p bbnf-ir --test vm cross_rule_csp::conflicting_disconnected_shared_node_is_not_published -- --exact --nocapture
M03 cargo test --target aarch64-apple-darwin -p bbnf-ir --lib acceptance_observes_boundary_cancellation -- --nocapture
M04 cargo test --target aarch64-apple-darwin -p bbnf-ir --lib passes::csp_strategy::tests::completed_component_then_cancel_reports_completed_plus_boundary -- --exact --nocapture
M05 cargo test --target aarch64-apple-darwin -p bbnf-ir --lib passes::csp_strategy::tests::final_return_observes_boundary_cancellation -- --exact --nocapture
M07 cargo test --target aarch64-apple-darwin -p bbnf-ir --lib passes::csp_strategy::tests::cancellation_on_final_singleton_leaf_discards_incumbent -- --exact --nocapture
M08 cargo test --target aarch64-apple-darwin -p bbnf-ir --lib passes::csp_strategy::tests::decoder_rejects_incomplete_and_oversized_assignments -- --exact --nocapture
M09 cargo test --target aarch64-apple-darwin -p bbnf-ir --lib passes::csp_strategy::tests::decoder_rejects_foreign_var_and_missing_value -- --exact --nocapture
```

## Bound identities

| Record / owner | SHA-256 | Lines / bytes |
|---|---|---:|
| author preimage ledger | `b2827d05d7705bac285d69314749495b6f52edb74ba1056050c3aa0f790c449b` | 4 / 434 |
| mutation restoration ledger | `e1ba667546e54fb01ccb09cc2a712bdc0e5095c74f145441532a4e55ef046f75` | 3 / 324 |
| `crates/core/src/lower/expression/wrap.rs` | `7edbb5bca06a63304615afb4b0560f73a0bb3d6df3439b7aa8c633f6cd323f17` | 488 / 17,932 |
| `crates/core/tests/lower/expression/wrap.rs` | `31ec57a3bc469d22f4d3ba805a12505365b2aa167ba01ebb74722b817967426c` | 270 / 8,387 |
| `crates/ir/src/passes/csp_strategy/solve.rs` | `aac939e7a68261d50dcbcf4961c638a206e71d780d95edf26dd71e1017ccf08e` | 721 / 26,443 |
| `crates/ir/tests/passes/csp_strategy/mod.rs` | `1e32d3b4ba2aae1db58cb2ee81db236c17357bb048caf256c3080006da27c9e3` | 669 / 22,447 |
| `crates/ir/src/passes/csp_strategy/site.rs` | `5b7b002acec4c3be75d2c7172af059195fce0eb8349bc8bb8ee3cc77776836b9` | 473 / 16,085 |
| `Cargo.lock` | `480ce3dcd92ed7778e5fc922cc531039e187a96258ec78f6fa6937defb6a40e9` | 4,650 / 113,885 |

`PATCHES.sha256` is the exact specimen ledger. `SOURCE-RESTORATION.sha256`
binds the three possible mutation targets. `AUTHOR-PREIMAGES.sha256` binds the
four dirty files before this author changed them. `APPLY-CHECK.log` is static
evidence only.
