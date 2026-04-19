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

# `data/` is gitignored (bbnf/, css/, json/, sheets/ corpora live here).
# Symlink each top-level sub-corpus individually so a partially-seeded
# worktree (e.g. one that already has data/sheets/ from an earlier run)
# still gets every corpus — the old "top-level data/" symlink guard
# refused to repair partial state because data/ existed. Each sub-
# corpus symlinks iff it isn't already present.
if [[ -d "$ROOT/data" ]]; then
    mkdir -p "$WORKTREE_PATH/data"
    for corpus in "$ROOT"/data/*/; do
        name=$(basename "$corpus")
        target="$WORKTREE_PATH/data/$name"
        if [[ ! -e "$target" ]]; then
            ln -s "$corpus" "$target"
        fi
    done
fi

# `target/` symlink — share compiled artefacts across worktrees so
# parallel waves don't each rebuild the full workspace from scratch.
# AX.W0a.2.d exposed the failure mode: the orchestrator started adding
# `target/` symlinks manually per-wave because forgetting cost an hour
# per agent spin-up. Own it here instead of each sub-agent reinventing
# it. Caller can pass --no-target to opt out when an isolated target
# is explicitly desired (e.g. investigating rebuild-cache bugs).
if [[ "$LINK_TARGET" == "1" ]]; then
    if [[ ! -e "$WORKTREE_PATH/target" ]]; then
        ln -s "$ROOT/target" "$WORKTREE_PATH/target"
    elif [[ -L "$WORKTREE_PATH/target" ]]; then
        existing="$(readlink "$WORKTREE_PATH/target")"
        if [[ "$existing" != "$ROOT/target" ]]; then
            echo "warn: $WORKTREE_PATH/target points at $existing, not $ROOT/target" >&2
        fi
    else
        echo "warn: $WORKTREE_PATH/target exists as a real directory; skipping symlink" >&2
    fi
fi

echo "seeded: $WORKTREE_PATH"
