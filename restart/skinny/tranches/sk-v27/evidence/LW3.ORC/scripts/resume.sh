#!/bin/zsh
# Resume after the 11:07 cut: corpus-v3 custody x2 (fresh fetch from the release into empty dirs, verify, one-byte tamper refused),
# then green-orc2.sh whole (clean, build, RB/ORC2/COMP-3a GREEN x2, class proof x2, three mutants x2) over the r1 fetch.
S=$HOME/.dev-logs/bbnf-lockstep/lw3/s5b-oracle; R=$S/bin/read.sh; W=$S/red-wt; L=$S/logs
export CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang
cd $W; git checkout -q --detach ea8997adb
for i in 1 2; do
  D=$S/corpus-v3-r$i; rm -rf $D; mkdir -p $D
  BBNF_CORPUS_DIR=$D $R $L/corpus-v3-custody-r$i.log $W "corpus-v3 custody r$i: fetch from the release into an empty dir, verify" -- zsh -c 'cargo xtask corpus fetch && cargo xtask corpus verify'
  cp $D/math/cases/hand-15.txt $S/hand-15.bak; printf 'X' | dd of=$D/math/cases/hand-15.txt bs=1 count=1 conv=notrunc 2>/dev/null
  BBNF_CORPUS_DIR=$D $R $L/corpus-v3-tamper-r$i.log $W "corpus-v3 one-byte tamper r$i (math/cases/hand-15.txt byte 0)" -- cargo xtask corpus verify
  cp $S/hand-15.bak $D/math/cases/hand-15.txt
  BBNF_CORPUS_DIR=$D $R $L/corpus-v3-restore-r$i.log $W "corpus-v3 restored r$i" -- cargo xtask corpus verify
done
sed "s#BBNF_CORPUS_DIR=\$S/corpus\$#BBNF_CORPUS_DIR=\$S/corpus-v3-r1#" $S/bin/green-orc2.sh > $S/bin/green-orc2-v3.sh
grep -n BBNF_CORPUS_DIR= $S/bin/green-orc2-v3.sh
zsh $S/bin/green-orc2-v3.sh
