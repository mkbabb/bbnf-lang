#!/bin/sh
# Fresh MANIFEST digests: base (lane parent 785027c44) and cure; the tool is copied in and removed.
set -u
L=$HOME/.dev-logs/bbnf-lockstep/lw3/perf-1d; TOOL=$HOME/Programming/bbnf-lang-wt/lw3-s10/restart/skinny/tranches/sk-v27/evidence/LW3.COMP/comp6
export BBNF_CORPUS_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/s7-spine/corpus
one() { # tree target tag
  cp $TOOL/manifest_digest.rs "$1/crates/core/tests/manifest_digest.rs"
  CARGO_TARGET_DIR=$2 sh $TOOL/manifest_digest.sh "$1" $L/digest-$3.tsv
  rm "$1/crates/core/tests/manifest_digest.rs"
  (cd "$1" && echo "$3 head=$(git rev-parse HEAD) dirty=$(git status --short | grep -vc '^??')") >> $L/digest-heads.txt
}
: > $L/digest-heads.txt
one $HOME/Programming/bbnf-lang-wt/lw3-s9b-base $L/target-base base-785027c44
one $HOME/Programming/bbnf-lang-wt/lw3-s9b $HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang cure-3ab9f31db
