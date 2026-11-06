MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: root native/mutation executor.

# BB-C.W0 ambient optimizer authority — R4 native ARM and mutation receipt

- Executed: `2026-07-22T20:55:21Z`
- Repository HEAD: `af15f63e0d2d3d719938c13b906a50acbb92ea3b`
- Author freeze: `W0-AMBIENT-R4-AUTHOR.md` SHA-256
  `7e917e2eedbeba69027231358a86091f5cddd0b5e4c394a1f38e70aeb15e7364`
- Host: Apple `arm64`; `uname -m = arm64`
- Rust host/target: `aarch64-apple-darwin`
- Rust: `rustc 1.96.0-nightly (02c7f9bec 2026-04-10)`
- Cargo: `cargo 1.96.0-nightly (eb94155a9 2026-04-09)`
- Build jobs: `1`
- Retry count: `0`
- x86 route: **none**
- Functional native status: **GREEN**
- Mutation status: **7/7 exact counterfactuals killed and restored**
- Hostile clean count: **0/2**
- Credit: **zero pending two hostile critics and fresh adjudication**
- Advancement: **BB-C.W1 may not begin**

Every native command used `/usr/bin/arch -arm64`,
`CARGO_BUILD_JOBS=1`, and `--target aarch64-apple-darwin`. Each frozen baseline
or mutant oracle ran exactly once. There was no retry, x86 process, benchmark,
package/lock edit, staging, commit, or other-repository mutation.

## Baseline gates

| Gate | Exact selector | Exit | Result | Raw log SHA-256 | Lines / bytes |
|---|---|---:|---|---|---:|
| G1 | `-p bbnf-ir --lib passes::csp_strategy::tests -- --nocapture` | 0 | 12 passed | `ab5e6ab06296294b10dffe4f74ac10f4eee07856655f3011de56c2abcfabb129` | 20 / 1511 |
| G2 | `-p bbnf-ir --test vm cross_rule_csp -- --nocapture` | 0 | 12 passed; 62 filtered | `53fcabd42493211ad43c8ea5ed8d874a76bcd3ef649b2cfaa07b023ab874cb42` | 29 / 1592 |
| G3 | `-p bbnf --lib lower::expression::wrap::tests -- --nocapture` | 0 | 5 passed; 20 filtered | `76d6d00f25125e31ebbf5c6efd7f029a709dfb8373de74953597cb9919b49046` | 1135 / 43077 |
| G4 | `-p bbnf --lib pipeline::compile::pipeline::tests -- --nocapture` | 0 | 3 passed; 22 filtered | `58145a86706d877b80c3778d0762f20d4922f9ad2c7ade2ef9d15fcb46e95ad4` | 1130 / 42674 |

G1 proves the bounded exact-solve, cancellation, decode, cost, and report
witnesses. G2 proves the public component transaction, including sorted
reports, mixed-component cancellation, one-family authority, and no
publication after a late merge conflict. G3 proves direct-depth grouped-map
lowering and exact typed-map placement. G4 proves all canonical compile
entrypoints retain the structured error type, optimizer failure publishes no
sidecars, and both typed arms execute under the current VM contract.

## Warning evidence

G2 exposed one R4-local warning: `crate::dag::NodeId` is imported by the new
`csp_strategy/mod.rs` facade but unused in the non-test library build. G3 and
G4 repeat that warning and retain the pre-existing `bbnf` warning bank (183
warnings, predominantly generated impossible unsigned comparisons, plus the
already-visible unused/dead items). The warning did not affect any exit, but
it is evidence for the hostile review and prevents a warning-clean claim.
Nothing was repaired after the frozen run.

## Exact mutation executions

Each row followed the same bounded transaction:

1. verify the exact target preimage;
2. `git apply --check` and apply exactly one frozen patch;
3. run its one named Apple-ARM oracle once;
4. observe Cargo exit `101` because the oracle rejected the mutant;
5. reverse the exact patch; and
6. require the target SHA-256 to equal its preimage before continuing.

| ID | Killing oracle | Cargo exit | Raw log SHA-256 | Lines / bytes | Restored target SHA-256 |
|---|---|---:|---|---:|---|
| M01 | `prune_restore_preserves_canonical_cost_and_order` | 101 | `72f2cf33d15e1c27341dc92d69fcfaa6699ce2d9cc9747066559975e91f12b08` | 21 / 1029 | `domain.rs` `8a8e07ad08e15726e73e9e70127a21493098e7d7738a3b25c2c462d8fd146271` |
| M02 | `implication_matches_exhaustive_immutable_optimum_across_orderings` | 101 | `0f60635de6cbcc609c17521fb7beb7007245f85c05d4461c3ea742cddcc80c99` | 25 / 1387 | `csp/mod.rs` `be015fe5a97a84ee50ac1e2e3cf2aa0974172f6af7562075db8327baba68a3e3` |
| M03 | `budget_after_an_incumbent_discards_best_so_far` | 101 | `09438601a08d4c39718337ace0ea0fc7c6030afb533e0e643138d8195e358043` | 23 / 1299 | `solve.rs` `02cf11b8c3439a2f0f1856aad8b8998d0182121914865e3779dfdfc7772f9581` |
| M04 | `unsatisfiable_problem_has_no_per_variable_fallback` | 101 | `96399adef4950679cc11ab21dbe95605aded70e84f8d1c88d618f6cd16948479` | 19 / 978 | `solve.rs` `02cf11b8c3439a2f0f1856aad8b8998d0182121914865e3779dfdfc7772f9581` |
| M05 | `cancellation_on_final_singleton_leaf_discards_incumbent` | 101 | `38f9b31fded34048ca8c4968a5e83f1349bf64d5167b79a5f8df0206c9a46f73` | 19 / 978 | `solve.rs` `02cf11b8c3439a2f0f1856aad8b8998d0182121914865e3779dfdfc7772f9581` |
| M06 | `incomplete_foreign_and_malformed_assignments_are_rejected` | 101 | `8e31369922dcb04e777ddd7eac6dffb97c16a74f96bb5037d699517cbd6af51f` | 19 / 1176 | `site.rs` `5b7b002acec4c3be75d2c7172af059195fce0eb8349bc8bb8ee3cc77776836b9` |
| M07 | `cross_rule_csp::conflicting_disconnected_shared_node_is_not_published` | 101 | `f059ecf257b36cc2effb9c3c87760063747b9a48bb6542b01699c04dbad584ad` | 28 / 2559 | `solve.rs` `02cf11b8c3439a2f0f1856aad8b8998d0182121914865e3779dfdfc7772f9581` |

All failures are semantic kills, not compilation failures: each executable
entered its named test and failed on the exact inverted invariant. M01 observed
invented restored costs; M02 the unconstrained optimum; M03/M04 an illicit
`Ok` incumbent/fallback; M05 an escaped cancelled leaf; M06 an accepted
incomplete decode; and M07 an `Ok` grammar output after a late merge conflict.

## Postcampaign preservation

`SOURCE-RESTORATION.sha256` verifies all four mutation targets. Every R4 source
hash listed by the author freeze is byte-identical after the campaign.
`Cargo.lock` remains
`480ce3dcd92ed7778e5fc922cc531039e187a96258ec78f6fa6937defb6a40e9`.
Cargo wrote only ordinary target artifacts; no source, test, package, lock,
generated runtime, parse-that, Value, Glass/SCI/Atlas, or playground byte was
mutated by native execution.

## Disposition

R4 is functionally GREEN and its bounded mutation matrix is executable, but it
is not accepted. Two fresh hostile critics must independently challenge the
entire R4 subject, tranche fit, wave adherence, tests, warnings, module split,
and mutation proof. A third fresh agent must then adjudicate every material
claim as correct, incorrect, or unproven. The warning finding is carried
forward explicitly. BB-C.W0 items 1, 2, 4, and 5 remain outside this item-3
repair even if adjudication accepts it.

**NATIVE GREEN; MUTANTS 7/7 KILLED; WARNING-CLEAN FALSE; CREDIT 0; HOSTILE
CLEAN 0/2; BB-C.W1 MAY NOT BEGIN.**
