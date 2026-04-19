#!/usr/bin/env bash
# Enumerate sibling-directory worktrees and report HEAD + dirty state.
#
# The orchestrator pattern is "sibling worktree per agent"
# (`../bbnf-wt-<tag>/`). After a session interruption the orchestrator
# needs to know: which worktrees exist, which are on what HEAD, which
# carry uncommitted work, which have the `target/` symlink wired. This
# script answers those four questions in one pass.
#
# Usage:
#   scripts/worktree-status.sh             # human-readable table
#   scripts/worktree-status.sh --tsv       # tab-separated for scripts
#   scripts/worktree-status.sh --dirty     # only worktrees with uncommitted work

set -euo pipefail

ROOT="$(git rev-parse --show-toplevel)"
PARENT="$(dirname "$ROOT")"
MAIN_TARGET="$ROOT/target"

MODE="table"
case "${1:-}" in
    --tsv)   MODE="tsv" ;;
    --dirty) MODE="dirty" ;;
    "")      ;;
    *)       echo "unknown mode: $1 (want --tsv|--dirty|)" >&2; exit 2 ;;
esac

if [[ "$MODE" == "table" ]]; then
    printf '%-40s  %-12s  %-7s  %-7s  %s\n' 'WORKTREE' 'HEAD' 'DIRTY' 'TARGET' 'BRANCH'
    printf '%-40s  %-12s  %-7s  %-7s  %s\n' '--------' '----' '-----' '------' '------'
elif [[ "$MODE" == "tsv" ]]; then
    printf 'worktree\thead\tdirty\ttarget\tbranch\n'
fi

for wt in "$PARENT"/bbnf-wt-*/; do
    [[ -d "$wt" ]] || continue
    [[ -d "$wt/.git" ]] || [[ -f "$wt/.git" ]] || continue

    short="$(basename "$wt")"
    head="$(git -C "$wt" rev-parse --short HEAD 2>/dev/null || echo '?')"
    branch="$(git -C "$wt" symbolic-ref --short HEAD 2>/dev/null || echo 'DETACHED')"

    if [[ -n "$(git -C "$wt" status --porcelain 2>/dev/null)" ]]; then
        dirty='dirty'
    else
        dirty='clean'
    fi

    if [[ -L "$wt/target" ]]; then
        link="$(readlink "$wt/target")"
        if [[ "$link" == "$MAIN_TARGET" ]]; then
            tgt='shared'
        else
            tgt='other'
        fi
    elif [[ -d "$wt/target" ]]; then
        tgt='local'
    else
        tgt='none'
    fi

    if [[ "$MODE" == "dirty" && "$dirty" != 'dirty' ]]; then
        continue
    fi

    if [[ "$MODE" == "tsv" ]]; then
        printf '%s\t%s\t%s\t%s\t%s\n' "$short" "$head" "$dirty" "$tgt" "$branch"
    else
        printf '%-40s  %-12s  %-7s  %-7s  %s\n' "$short" "$head" "$dirty" "$tgt" "$branch"
    fi
done
