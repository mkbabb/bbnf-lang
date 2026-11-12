#!/bin/sh
# usage: read.sh <log> <cmd...>   (run inside the PT lane worktree's rust/)
log=$1; shift
cd /Users/mkbabb/Programming/parse-that-wt/lw3-s6-kernel/rust || exit 2
export CARGO_TARGET_DIR=/Users/mkbabb/.dev-logs/bbnf-lockstep/lw3/target-parse-that
{ echo "# head $(git rev-parse HEAD) dirty=$(git status --porcelain --untracked-files=no | wc -l | tr -d ' ')"; echo "# cmd $*"; echo "# date $(date -Iseconds)"; } > "$log"
"$@" >> "$log" 2>&1
rc=$?
echo "# rc $rc" >> "$log"
exit $rc
