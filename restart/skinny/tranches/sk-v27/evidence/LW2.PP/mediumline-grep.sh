#!/bin/sh
# sk-v27 LW2.PP-2 falsifier: the LW1.PP-1 Mediumline grep, re-read at pinned revs.
# Usage: mediumline-grep.sh <pprint-rev> <bbnf-lang-rev>
# Reads through git (never a working tree). Scope: pprint's code and docs (its
# tranche records excluded: they quote the op by name), and in bbnf-lang
# gorgeous, the emitters (crates/core/src/backend, the generated grammars),
# typescript/ and docs/pprint. Exit 0 iff the total is 0.
set -eu
PP=${PPRINT_REPO:-$HOME/Programming/pprint}
BB=${BBNF_REPO:-$HOME/Programming/bbnf-lang}
pat='mediumline|medium_line'
n=0
hits() { # repo rev paths...
  repo=$1; rev=$2; shift 2
  out=$(git -C "$repo" grep -n -i -E "$pat" "$rev" -- "$@" || true)
  c=$(printf '%s' "$out" | grep -c . || true)
  echo "== $(basename "$repo")@$(git -C "$repo" rev-parse --short=9 "$rev") hits=$c"
  [ -n "$out" ] && printf '%s\n' "$out"
  n=$((n + c))
}
hits "$PP" "$1" rust docs/playground README.md CLAUDE.md
hits "$BB" "$2" crates/gorgeous crates/core/src/backend crates/core/src/grammar/generated typescript docs/pprint
echo "total=$n"
[ "$n" -eq 0 ]
