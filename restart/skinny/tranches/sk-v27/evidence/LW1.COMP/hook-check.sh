#!/bin/sh
# LW1.COMP-4 hook falsifier (ruling: the leftover sk-v26 pre-commit hook is a defect).
# RED when the hooks directory git actually uses for this repository holds a
# pre-commit hook (the sk-v26 one runs an unguarded `cargo test`, which breaks the
# machine law on every commit); GREEN when none is present.
# Usage: hook-check.sh [repo-or-worktree]
repo=${1:-.}
hooks=$(git -C "$repo" rev-parse --path-format=absolute --git-path hooks) || exit 2
f="$hooks/pre-commit"
if [ -e "$f" ]; then
    echo "RED: $f present, sha256 $(shasum -a 256 "$f" | cut -d' ' -f1)"
    exit 1
fi
echo "GREEN: no pre-commit hook in $hooks"
