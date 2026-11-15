#!/bin/sh
# Fresh MANIFEST digests: base (integration 80672a988) and cure (lane head); the tool is copied in and removed.
set -u
L=/Users/mkbabb/.dev-logs/bbnf-lockstep/lw3/orc2-ws-c; TOOL=/Users/mkbabb/Programming/bbnf-lang-wt/lw3-s10/restart/skinny/tranches/sk-v27/evidence/LW3.COMP/comp6
export CARGO_INCREMENTAL=0 BBNF_CORPUS_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/s7-spine/corpus
one() {
  cp $TOOL/manifest_digest.rs "$1/crates/core/tests/manifest_digest.rs"
  CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang sh $TOOL/manifest_digest.sh "$1" $L/digest-$2.tsv
  rm "$1/crates/core/tests/manifest_digest.rs"
  (cd "$1" && echo "$2 head=$(git rev-parse HEAD) dirty=$(git status --short | grep -vc '^??')") >> $L/digest-heads.txt
}
: > $L/digest-heads.txt
one $HOME/Programming/bbnf-lang-wt/lw3-s5c-base base-80672a988
one $HOME/Programming/bbnf-lang-wt/lw3-s5c cure-f5e8eab01
