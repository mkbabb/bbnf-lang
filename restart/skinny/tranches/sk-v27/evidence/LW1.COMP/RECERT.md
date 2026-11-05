# LW1.COMP-2: re-certification of the sk-v26 spine

Observed 2026-11-05 by an Opus seat. Logs are in `~/.dev-logs/bbnf-lockstep/comp/`
(`comp2-*.log`).

## Where it ran

The merged HEAD this unit names does not exist yet. LW1.TOPO-5 has not merged
`sk-lineage/local-master` on `lockstep/merge-skv25`, and the spine merge itself is
LW3.COMP-1. So the receipts were re-proved at the sk-v26 heads, in clean detached
worktrees made from the local object stores, with no dirty state carried over:

| repo | commit | branch (local only; LW1.TOPO-3 pushes it) |
|---|---|---|
| bbnf-lang | `b5fd0ca98` | `codex/sk-v26-bbnf` |
| parse-that | `e31fbfedc` | `codex/sk-v26-parse-that` |

The codex checkout `~/Programming/bbnf-lang-skv26` is dirty: an uncommitted
+1,692/−122 edit to `crates/core/tests/w3_replay_public_contract.rs`. It is not part
of this run. It belongs to LW1.TOPO-3, which captures it read-only.

When TOPO-5 lands, run this again at the merged HEAD.

## ABI-3 receipt (parse-that 0.5.0, KERNEL_ABI 3)

`cargo test --workspace --all-targets --all-features --locked`, toolchain 1.97.1:

| run | rc | passed | failed |
|---|---|---|---|
| 1 | 0 | 91 | 0 |
| 2 | 0 | 91 | 0 |

The counts match `W3-PARSE-THAT-ABI3.json` binary for binary:
- kernel: 58
- allocation: 1
- regex: 32 (2 + 1 + 12 + 1 + 16)

The receipt's total of 92 adds 1 doctest. Doctests were not re-run here
(`--all-targets` leaves them out).

**Result: RE-PROVED.**

## W2 spine and W3 receipts (bbnf 0.4.0)

`cargo test --workspace --all-targets --all-features --locked --no-fail-fast`, toolchain nightly-2026-07-29:

| tree state | run | rc | passed | failed |
|---|---|---|---|---|
| clean worktree | 1 | 101 | 109 | 1 |
| clean worktree | 2 | 101 | 109 | 1 |
| clean worktree plus 7 untracked symlinks (below) | 1 | 0 | 110 | 0 |
| clean worktree plus 7 untracked symlinks (below) | 2 | 0 | 110 | 0 |

The W2 spine laws pass ×2 in every state, 60 in all:
- 14 unit laws;
- carrier_lookup 3;
- dependency_scc 5;
- emitter 4;
- frontend 9;
- identity_opacity 1;
- record_semantic 2;
- replay_edit 6;
- type_analysis 16.

W2-ADMISSION counted 57.

The W3 contracts:
- instance_frontend: 20;
- semantic_pratt: 17;
- replay_public: 13.

**The defect: a receipt that is not hermetic.** One test fails in a clean tree:
`w3_replay_public_contract::active_roots_remain_preselection_only_and_the_guard_self_falsifies`.
Its "frozen preselection basis" expects seven symlinks:
- `data/css/{bootstrap,normalize,tailwind}.css`
- `data/json/{canada,citm_catalog,data,twitter}.json`

Each one points into `restart/skinny/tranches/sk-v26/corpora/`. They exist only in
the codex checkout, and `.gitignore:4` (`data/`) ignores them. A clone cannot
reproduce them. Recreating exactly those seven links turns the run GREEN, so they
are the whole cause.

This is the same class of defect as CB-1a, for test inputs instead of dependency
edges. Filed to LW3.COMP-1. The cure is one of two:
- the test reads the corpora from their tracked path;
- the links become tracked.

The cure is not a re-created untracked state.

**Result: W2 spine RE-PROVED. W3 replay/public receipt RED in a clean tree, for the
reason stated.**
