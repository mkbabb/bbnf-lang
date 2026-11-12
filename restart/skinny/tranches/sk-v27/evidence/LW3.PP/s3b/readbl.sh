#!/bin/sh
# usage: readbl.sh <dir> <log> <cmd...>   (bbnf-lang)
dir=$1; log=$2; shift 2
cd "$dir" || exit 2
export CARGO_TARGET_DIR=/Users/mkbabb/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang
{ echo "# head $(git rev-parse HEAD) dirty=$(git status --porcelain --untracked-files=no | wc -l | tr -d ' ')"; echo "# cmd $*"; echo "# date $(date -Iseconds)"; } > "$log"
"$@" >> "$log" 2>&1
rc=$?
echo "# rc $rc" >> "$log"
exit $rc
