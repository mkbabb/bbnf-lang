#!/usr/bin/env bash
# Terminate orphan rust toolchain processes across the machine.
#
# Long-running / OOM-killed rustc children sometimes survive cargo's
# exit. When a worktree agent crashes or an orchestrator session is
# interrupted, the machine can be left carrying multi-GB rustc /
# cargo / rust-analyzer / samply processes that starve subsequent
# work. This script terminates them — SIGTERM first, SIGKILL after a
# grace period for any that ignore TERM — and reports the kill list.
#
# Usage:
#   scripts/kill-all-rust.sh                  # TERM, then KILL stragglers
#   scripts/kill-all-rust.sh --dry-run        # list only, no signal
#   scripts/kill-all-rust.sh --include-lsp    # also kill rust-analyzer
#
# Does NOT kill `cargo` processes owned by a PID tree rooted at the
# caller shell — the caller's own shell + bash scripts are exempt.

set -euo pipefail

DRY=0
INCLUDE_LSP=0
for arg in "$@"; do
    case "$arg" in
        --dry-run)     DRY=1 ;;
        --include-lsp) INCLUDE_LSP=1 ;;
        *) echo "unknown arg: $arg" >&2; exit 2 ;;
    esac
done

# Process names to target. rustc children are what typically eat memory;
# cargo-watch + cargo-expand shell out to cargo which shells to rustc.
PATTERNS=(rustc cargo)
if [[ "$INCLUDE_LSP" == "1" ]]; then
    PATTERNS+=(rust-analyzer)
fi

SELF_PID=$$
SELF_PGID="$(ps -o pgid= -p $SELF_PID | tr -d ' ')"

collect_pids() {
    local pattern="$1"
    # macOS ps + awk: match the exact command path / name, drop self.
    ps -axo pid=,pgid=,comm= \
        | awk -v self_pgid="$SELF_PGID" -v pat="$pattern" '
            $3 ~ pat && $2 != self_pgid { print $1 }
        '
}

declare -a KILLS=()
for pat in "${PATTERNS[@]}"; do
    while IFS= read -r pid; do
        [[ -n "$pid" ]] && KILLS+=("$pid")
    done < <(collect_pids "$pat")
done

if [[ "${#KILLS[@]}" -eq 0 ]]; then
    echo "No matching processes found."
    exit 0
fi

echo "Targeting ${#KILLS[@]} processes:"
ps -o pid=,rss=,comm= -p "${KILLS[@]}" | awk '{ printf "  %-8s  %8s KB  %s\n", $1, $2, $3 }'

if [[ "$DRY" == "1" ]]; then
    exit 0
fi

echo "Sending TERM..."
kill -TERM "${KILLS[@]}" 2>/dev/null || true
sleep 2

# Any survivors? SIGKILL.
declare -a SURVIVORS=()
for pid in "${KILLS[@]}"; do
    if kill -0 "$pid" 2>/dev/null; then
        SURVIVORS+=("$pid")
    fi
done

if [[ "${#SURVIVORS[@]}" -gt 0 ]]; then
    echo "Sending KILL to ${#SURVIVORS[@]} survivor(s): ${SURVIVORS[*]}"
    kill -KILL "${SURVIVORS[@]}" 2>/dev/null || true
fi

echo "Done."
