#!/bin/sh
# S14 P4 (S13-G2): a published version line must move when the public API breaks.
# usage: semver-line.sh <repo> <consumer-pinned-rev> <rev> <manifest> <src-dir>
# RED (rc 1) when a `pub` item line is removed or changed between the pinned rev
# and <rev> while the leftmost non-zero version component is unchanged.
set -eu
repo=$1 base=$2 rev=$3 man=$4 src=$5
ver() { git -C "$repo" show "$1:$man" | sed -n 's/^version *= *"\(.*\)"/\1/p' | head -1; }
line() { echo "$1" | awk -F'[.-]' '{ if ($1 != "0") print $1; else if ($2 != "0") print "0." $2; else print "0.0." $3 }'; }
vb=$(ver "$base"); vr=$(ver "$rev")
removed=$(git -C "$repo" diff "$base" "$rev" -- "$src" | grep -E '^-[[:space:]]*pub (const |unsafe |async )*(fn|struct|enum|trait|type|const|static) ' | grep -v '^---' || true)
n=$(printf '%s' "$removed" | grep -c . || true)
pre=$(echo "$vr" | grep -c -- '-' || true)
echo "base $base version $vb (line $(line "$vb")); rev $rev version $vr (line $(line "$vr"))"
echo "removed-or-changed pub item lines: $n"
printf '%s\n' "$removed" | head -12
if [ "$n" -gt 0 ] && [ "$(line "$vb")" = "$(line "$vr")" ]; then
  echo "RED: breaking public API inside the $(line "$vr") line"; exit 1
fi
echo GREEN; exit 0
