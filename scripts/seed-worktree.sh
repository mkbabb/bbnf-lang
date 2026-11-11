#!/usr/bin/env bash
# Seed a freshly-created worktree with gitignored-but-required resources.
#
# The orchestrator creates worktrees via `git worktree add`. Resources
# that are gitignored (large test corpora under `data/`, shared
# `target/` artefacts) don't travel with the worktree. Agents that run
# `cargo test` inside the worktree see environmental failures or waste
# hours re-compiling from scratch until those resources materialise.
#
# Usage:
#   scripts/seed-worktree.sh <worktree-path>
#   scripts/seed-worktree.sh <worktree-path> --no-target  # skip target symlink
#
# Idempotent — re-running on an already-seeded worktree is a no-op.
# Prefers symlinks so the main checkout stays the single source of truth.

set -euo pipefail

WORKTREE_PATH="${1:?usage: $0 <worktree-path> [--no-target]}"
LINK_TARGET=1
if [[ "${2:-}" == "--no-target" ]]; then
    LINK_TARGET=0
fi

ROOT="$(git rev-parse --show-toplevel)"

if [[ ! -d "$WORKTREE_PATH" ]]; then
    echo "error: worktree path does not exist: $WORKTREE_PATH" >&2
    exit 1
fi

# The test, bench and example corpus is not tracked and is not linked from
# the main checkout: its custody is `corpus/MANIFEST.sha256` (sk-v27
# LW2.TOPO-6, ruling R-c). Run `cargo xtask corpus fetch` in the worktree;
# it downloads the release asset, checks its sha256 and every file, and is a
# no-op when the corpus already verifies.

# `target/` hardlink-clone — share compiled artefacts across worktrees
# while giving each worktree its own incremental-cache so parallel
# `cargo` invocations across worktrees do not contend on the
# target-directory lock (feedback: single-cargo-per-target).
#
# AZ-I.W2-act.W0 hygiene cut H2: `cp -al` hardlinks every regular
# file in `$ROOT/target` into `$WORKTREE_PATH/target`. Hardlinks are
# inode references — disk-zero cost; new writes (incremental builds)
# get COW semantics so worktree-local edits do not propagate back to
# the main checkout. Reclaim ≈ 3-5 min cold + 15-20 GB disk per
# fan-out worktree vs full rebuild from scratch.
#
# Pre-AZ-I.W2-act.W0: a single symlink shared the entire `target/`
# tree across worktrees, which silently serialised parallel cargo
# invocations through the target-dir lock. Migrated under the
# no-backward-compat / no-workarounds discipline.
#
# Caller passes --no-target to opt out (e.g. investigating
# rebuild-cache bugs). Idempotent: a pre-existing `target/` is left
# alone; legacy symlinks block re-seed and require manual removal.
if [[ "$LINK_TARGET" == "1" ]]; then
    if [[ ! -e "$WORKTREE_PATH/target" ]]; then
        if [[ -d "$ROOT/target" ]]; then
            cp -al "$ROOT/target" "$WORKTREE_PATH/target"
            echo "hardlink-cloned $ROOT/target → $WORKTREE_PATH/target" >&2
        fi
    elif [[ -L "$WORKTREE_PATH/target" ]]; then
        echo "error: $WORKTREE_PATH/target is a legacy symlink; remove it then re-run seed" >&2
        exit 1
    else
        echo "info: $WORKTREE_PATH/target already exists; skipping (idempotent)" >&2
    fi
fi

echo "seeded: $WORKTREE_PATH"
