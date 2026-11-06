MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: R4 author.

# BB-C.W0 ambient optimizer authority — bounded R2 item-3 mutation manifest

- Frozen: `2026-07-22T20:44:09Z`
- Repository HEAD: `af15f63e0d2d3d719938c13b906a50acbb92ea3b`
- Scope: only the seven counterfactuals ordered by the R2/R3 adjudications
- Execution: **no mutant was applied or run; no Cargo/build/test command ran**
- Validation: each exact unified patch returned exit `0` from read-only
  `git apply --check`; target source hashes remained unchanged
- Credit: zero BB-C.W0 item-5, wave, lock, tranche, or movement credit

These are deliberately small source counterfactuals, not a mutation framework.
The external oracle listed for each specimen rejects the counterfactual by
requiring the repaired behavior. Dynamic mutant execution remains intentionally
absent; the manifest records the exact static inversion and its named oracle.

## Exact specimens and killing oracles

| ID | Patch SHA-256 | Lines / bytes | Counterfactual | External killing oracle | Static inversion |
|---|---|---:|---|---|---|
| M01 | `9633e1d2616ddf81e929d0b179f9af5497c9400b68e97cebed06a3eb8711f04f` | 10 / 460 | restore a pruned value with invented unit cost | `passes::csp_strategy::tests::prune_restore_preserves_canonical_cost_and_order` | restored non-unit cost no longer matches the immutable preimage |
| M02 | `6465c96ae7bde833693582bc264dbe682a83afaa345cbf2ffe6377afe14e0d3a` | 9 / 444 | discard every custom hard constraint at registration | `passes::csp_strategy::tests::implication_matches_exhaustive_immutable_optimum_across_orderings` | the constrained solve becomes the explicitly different unconstrained optimum |
| M03 | `5c1615f10232a42769a098b3c1bff8aeb3ac806205c4f72ae18373b4b7ab9ead` | 12 / 589 | accept an incumbent when the node budget was exhausted | `passes::csp_strategy::tests::budget_after_an_incumbent_discards_best_so_far` | the fixture proves an incumbent exists, so the mutant returns `Ok` instead of typed `BudgetExceeded` |
| M04 | `a4efe104378d6dc7b7d3043adda4d85009b7ba07d4222daf04f85f8d326b5553` | 21 / 787 | synthesize an independent per-variable result after unsat | `passes::csp_strategy::tests::unsatisfiable_problem_has_no_per_variable_fallback` | the not-equal singleton fixture becomes `Ok` instead of typed `Unsatisfiable` |
| M05 | `8ec5bd0a30599a7a0c50194115402b7d69c9951c164211860aa5aa366299aff5` | 12 / 570 | ignore both solver stats and the current cancellation token | `passes::csp_strategy::tests::cancellation_on_final_singleton_leaf_discards_incumbent` (also `cancellation_before_search_discards_every_solution`) | the final-leaf incumbent escapes, or pre-search cancellation is mislabeled |
| M06 | `e7e6139a53e3a903da7a168da4230b926b619f2ea8c9a75c6ae4e090c8d4de42` | 49 / 1922 | ignore solution length, clamp foreign VarIds, and fill missing values from domains | `passes::csp_strategy::tests::incomplete_foreign_and_malformed_assignments_are_rejected` | incomplete, oversized, or foreign assignment input is accepted instead of rejected |
| M07 | `f8f04ff4c2c5557785fd6b878613f500e5752e823643ce58d2d9e93b97fd4e75` | 21 / 923 | return accumulated output after a late component merge conflict | `cross_rule_csp::conflicting_disconnected_shared_node_is_not_published` | a first component's local maps escape as `Ok` after the second component fails |

## Bound source identities and restoration

The same values were observed after all seven apply-checks:

| Target | Baseline and post-check SHA-256 |
|---|---|
| `crates/ir/src/passes/csp_strategy/domain.rs` | `8a8e07ad08e15726e73e9e70127a21493098e7d7738a3b25c2c462d8fd146271` |
| `crates/csp-solver/src/csp/mod.rs` | `be015fe5a97a84ee50ac1e2e3cf2aa0974172f6af7562075db8327baba68a3e3` |
| `crates/ir/src/passes/csp_strategy/solve.rs` | `02cf11b8c3439a2f0f1856aad8b8998d0182121914865e3779dfdfc7772f9581` |
| `crates/ir/src/passes/csp_strategy/site.rs` | `5b7b002acec4c3be75d2c7172af059195fce0eb8349bc8bb8ee3cc77776836b9` |

`PATCHES.sha256` is the copy/paste-verifiable digest list.
`SOURCE-RESTORATION.sha256` binds the unchanged targets.
`APPLY-CHECK.log` freezes every exact check command, exit, and empty output.

This closes only the bounded R2 item-3 specimen requirement. It does not claim
dynamic mutant execution or the broader BB-C.W0 item-5 mutation campaign.
