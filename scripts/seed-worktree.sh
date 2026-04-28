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
