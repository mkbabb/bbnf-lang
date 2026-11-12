#!/bin/zsh
# LW3.ORC2 falsifier (ruling ORC-2): every one of the nine grammars of record reads at least one
# pinned independent oracle. Usage: orc2_check.sh <g1 binary>, run from the tree it was built from.
# GREEN (rc 0) only when `g1 --list-oracles` itself exits 0 and lists each of bbnf json css_l4
# css_pretty google_sheets ebnf bnf csv math with oracles >= 1. A g1 that aborts part-way (a
# transcription that no longer matches its grammar's sha256) is RED: an unlisted grammar is not
# a grammar with an oracle. (S5b ORACLE: the first form, which counted only "oracles=0" lines,
# passed a g1 that aborted after google_sheets; this form refuses it.)
g1=${1:?usage: orc2_check.sh <g1 binary>}
out=$("$g1" --list-oracles 2>&1); rc=$?
print -r -- "$out"
red=0
if (( rc != 0 )); then print "RED: g1 --list-oracles exited $rc"; red=1; fi
for g in bbnf json css_l4 css_pretty google_sheets ebnf bnf csv math; do
  line=$(print -r -- "$out" | grep -E "^grammar=$g oracles=")
  n=${${line#*oracles=}%% *}
  if [[ -z $line ]]; then print "RED: $g not listed"; red=1
  elif (( n < 1 )); then print "RED: $g reads no oracle"; red=1; fi
done
if (( red )); then print "orc2: RED"; exit 1; fi
print "orc2: GREEN (nine grammars, each with at least one oracle)"
