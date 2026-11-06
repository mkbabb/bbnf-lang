#!/bin/sh
# check-hermetic-build.sh — the workspace builds from a clean clone alone (CB-1a).
#
# Three conditions, each read from cargo's own resolution of the graph:
#   1. every path package lives inside this repository (no `../sibling` edge,
#      whether written in a manifest or in a `[patch]` table);
#   2. every git package is pinned by `rev` (a branch or tag edge moves under
#      the lockfile's feet);
#   3. `cargo check --workspace --all-targets --locked` succeeds.
#
# Run it from any directory inside the repository. CI runs it on a fresh
# checkout with no sibling repositories and no credentials, which is the case
# it exists for.
set -eu

root=$(cd "$(git rev-parse --show-toplevel)" && pwd -P)
cd "$root"

meta=$(cargo metadata --locked --format-version 1)

escapes=$(printf '%s' "$meta" | jq -r --arg root "$root/" '
    .packages[]
    | select(.source == null)
    | select(.manifest_path | startswith($root) | not)
    | "  \(.name) \(.version) at \(.manifest_path)"')
unpinned=$(printf '%s' "$meta" | jq -r '
    .packages[]
    | select(.source != null and (.source | startswith("git+")))
    | select(.source | test("[?&]rev=") | not)
    | "  \(.name) \(.version) from \(.source)"')

status=0
if [ -n "$escapes" ]; then
    echo "path packages outside $root:"
    echo "$escapes"
    status=1
fi
if [ -n "$unpinned" ]; then
    echo "git packages not pinned by rev:"
    echo "$unpinned"
    status=1
fi
[ "$status" -eq 0 ] || exit "$status"

echo "graph: every path package is inside the repository; every git package is pinned by rev"
cargo check --workspace --all-targets --locked
echo "hermetic: clean-clone build GREEN"
