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

## At the merged HEAD (2026-11-06, after LW1.TOPO-5)

LW1.TOPO-5 made the ordinary merge `077d9338d` (origin/master `c9b93adb4` plus
local master `af15f63e0`) on `lockstep/merge-skv25`. The sk-v26 spine is not in
it: `b5fd0ca98` is 24 commits on top of `af15f63e0`, and merging the spine is
LW3.COMP-1. So "the merged HEAD" for these receipts is the spine over that merge:
an ordinary trial merge, made in a detached worktree and neither pushed nor
kept on a branch.

| | |
|---|---|
| parents | `077d9338d` (TOPO-5 merge), `b5fd0ca98` (sk-v26 spine) |
| trial merge tree | `08e8844be6a8144087c0754a09f3d32257940dd0` (re-made by `git merge --no-ff b5fd0ca98` at `077d9338d`; no conflicts) |
| delta against `b5fd0ca98` | origin's 38 commits only: `typescript/` (the TS package), `release-npm.yml`, 2 `.gitignore` lines. No Rust file differs. |
| parse-that | `e31fbfedc` (`codex/sk-v26-parse-that`, on origin), the 0.5.0 the spine's workspace deps name |

Logs: `~/.dev-logs/bbnf-lockstep/comp/lw1-close/comp2-*.log`. Load 16–19 throughout (NOISY; timing is not a claim).

### ABI-3 (parse-that 0.5.0, KERNEL_ABI 3): RE-PROVED ×2

`cargo test --workspace --all-targets --all-features --locked`, then `--doc`, toolchain 1.97.1:

| run | rc | passed | failed | per binary |
|---|---|---|---|---|
| 1 | 0 | 92 | 0 | kernel 58, kernel_allocation 1, regex 32 (2 + 1 + 12 + 1 + 16), doctest 1 |
| 2 | 0 | 92 | 0 | identical |

The doctest is now run, so the total matches `W3-PARSE-THAT-ABI3.json`'s 92 exactly.

### W2 spine: RE-PROVED ×2

`cargo test --workspace --all-targets --all-features --locked --no-fail-fast`, toolchain nightly-2026-07-29, clean tree:
per-binary counts are identical across the two runs (`comp2-spine-{1,2}.counts`). The 60 W2 laws pass:
unit 14, carrier_lookup 3, dependency_scc 5, emitter 4, frontend 9, identity_opacity 1,
record_semantic 2, replay_edit 6, type_analysis 16. W3: instance_frontend 20, semantic_pratt 17.

### W3 replay/public guard: RED ×2, now for two reasons

`active_roots_remain_preselection_only_and_the_guard_self_falsifies` fails in both runs (109/110).
At the sk-v26 head it had one cause. At the merged HEAD it has two, separated by a second
pair of runs with the 7 corpus symlinks recreated in the throwaway tree (`comp2-w3env-{1,2}.log`,
still RED ×2, and only for cause 2):

1. **The 7 gitignored corpus symlinks** (`data/css/*`, `data/json/*`), as before. Corpus custody: LW2.TOPO-6.
2. **New, brought in by the merge: 5 duplicate grammar authorities.** origin's `0999cb8c3` (X.P.W7.t)
   checked in `typescript/test/fixtures/grammar/lang/{bnf,csv,g4,math,regex}.bbnf`, byte-identical to
   `grammar/bnf/bnf.bbnf` and `grammar/misc/{csv,g4,math,regex}.bbnf`, because the TS front end could
   not read the Rust dialect (`->` projections) of the files that had since moved on. The spine's guard
   forbids a second authority copy. This is the R-b dialect question surfacing as a repository fact.
   Cure, at the spine merge (LW3.COMP-1) once R-b is ruled: the TS tests read the one authority under
   `grammar/` (or X-8's pinned corpus), and the copies are deleted. Re-creating the old state is not a cure.

### Hermeticity of the spine itself

The spine's `[workspace.dependencies]` name `../parse-that-skv26/rust/{parse_that,regex}` by path, the
same class of defect CB-1a cures on sk-v27. `e31fbfedc` is on origin, so at LW3.COMP-1 those edges
become `git` + `rev` edges and `scripts/check-hermetic-build.sh` joins the spine's CI.
The spine also tracks `scripts/hooks/pre-commit` (blob `66d840c4`, the hook LW1.COMP-4 removed); master's
template runs `cargo xtask regen --check --staged` (it builds xtask) on every commit. Neither may be
installed under the machine law; both go to LW3.COMP-1 / LW2.TOPO-3 (CI is the enforcement).

**Result: ABI-3 and W2 spine RE-PROVED at the merged HEAD. W3 replay/public RED ×2 for the two stated
reasons, routed to LW2.TOPO-6 and LW3.COMP-1 (after R-b).**
