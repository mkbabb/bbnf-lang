#!/bin/sh
# MANIFEST digest, base (sk-v27-lw3 at lw3-int) and cure (the lane): comp6/manifest_digest.{rs,sh}, the tool copied
# in and removed; the shared clean of bbnf (dev) before each tree, so neither reads the other's library.
set -u
L=$HOME/.dev-logs/bbnf-lockstep/lw3/gen-recurse; X=$HOME/Programming/bbnf-lang-wt/lw3-gen-recurse
TOOL=$X/restart/skinny/tranches/sk-v27/evidence/LW3.COMP/comp6; S=$X/restart/skinny/tranches/sk-v27/evidence/LW3.AUDIT/tgt-clean.sh
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang BBNF_CORPUS_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/s7-spine/corpus
one() { # tree tag
  sh $S clean "$1" dev bbnf >> $L/digest-heads.txt 2>&1
  cp $TOOL/manifest_digest.rs "$1/crates/core/tests/manifest_digest.rs"
  sh $TOOL/manifest_digest.sh "$1" $L/digest-$2.tsv
  rm "$1/crates/core/tests/manifest_digest.rs"
  (cd "$1" && echo "$2 head=$(git rev-parse HEAD) dirty=$(git status --short | grep -vc '^??')") >> $L/digest-heads.txt
}
: > $L/digest-heads.txt
one $HOME/Programming/bbnf-lang-wt/lw3-int base
one $X cure
diff $L/digest-base.tsv $L/digest-cure.tsv > $L/digest.diff; echo "diff rc=$? lines=$(grep -c '^[<>]' $L/digest.diff)" >> $L/digest-heads.txt
