#!/bin/zsh
# read.sh <log> <worktree> <label> -- cmd...   : one falsifier read, logged with head, command and rc.
log=$1; wt=$2; label=$3; shift 3; [[ $1 == -- ]] && shift
export CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang
export BBNF_CORPUS_DIR=${BBNF_CORPUS_DIR:-$HOME/.dev-logs/bbnf-lockstep/lw3/s5b-oracle/corpus}
cd $wt
{
  echo "# $label at $(git rev-parse --short=9 HEAD) $(date -u +%Y-%m-%dT%H:%M:%SZ) (tree $(git status --porcelain --untracked-files=no | wc -l | tr -d ' ') modified tracked files)"
  echo "\$ $*"
} > $log
"$@" >> $log 2>&1
rc=$?
echo "rc=$rc" >> $log
echo "rc=$rc"
