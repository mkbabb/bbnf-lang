#!/usr/bin/env bash
# Seed a freshly-created worktree with gitignored-but-required resources.
#
# The orchestrator creates worktrees via `git worktree add`. Resources
# that are gitignored (large test corpora under `data/`, any cached
# `.cargo/` overrides) don't travel with the worktree. Agents that run
# `cargo test` inside the worktree see environmental failures until
# those resources materialise.
#
# Usage: scripts/seed-worktree.sh <worktree-path>
#
# Idempotent — re-running on an already-seeded worktree is a no-op.
# Prefers symlinks so the main checkout stays the single source of truth.

set -euo pipefail

WORKTREE_PATH="${1:?usage: $0 <worktree-path>}"
ROOT="$(git rev-parse --show-toplevel)"

if [[ ! -d "$WORKTREE_PATH" ]]; then
    echo "error: worktree path does not exist: $WORKTREE_PATH" >&2
    exit 1
fi

# `data/` is gitignored (bbnf/, css/, json/, sheets/ corpora live here).
# Symlink the main checkout's copy into the worktree; parse-tests +
# fixture comparisons resolve identically to the main tree.
if [[ -d "$ROOT/data" && ! -e "$WORKTREE_PATH/data" ]]; then
    ln -s "$ROOT/data" "$WORKTREE_PATH/data"
fi

echo "seeded: $WORKTREE_PATH"
