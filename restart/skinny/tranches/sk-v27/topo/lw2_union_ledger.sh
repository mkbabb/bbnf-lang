#!/usr/bin/env bash
# LW2 §1: union-resolve LEDGER.md inside a conflicted merge (the ledger is
# append-only, so both sides' rows are kept; git merge-file --union).
set -eu
f=restart/skinny/tranches/sk-v27/LEDGER.md
d=$(mktemp -d); trap 'rm -rf "$d"' EXIT
git show :1:$f > "$d/base" 2>/dev/null || : > "$d/base"
git show :2:$f > "$d/ours"; git show :3:$f > "$d/theirs"
git merge-file --union -p "$d/ours" "$d/base" "$d/theirs" > $f
! grep -q '^<<<<<<<\|^>>>>>>>' $f
git add $f
