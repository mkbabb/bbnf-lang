#!/bin/zsh
# LW3.ORC1-RB GREEN r1,r2 and mutant x2 (record checker only; no build). Resumed after the 11:07 cut.
S=$HOME/.dev-logs/bbnf-lockstep/lw3/s5b-oracle; R=$S/bin/read.sh; W=$S/red-wt; L=$S/logs
CK=restart/skinny/tranches/sk-v27/evidence/LW3.ORC/orc_check.py
cd $W; git checkout -q --detach ea8997adb
for i in 1 2; do $R $L/rb-green-r$i.log $W "LW3.ORC1-RB rows GREEN r$i" -- python3 $CK rows HEAD; done
export GIT_INDEX_FILE=$S/mutant.index; git read-tree HEAD
F=crates/oracle/tests/rb_json_ws.rs
blob=$(git show HEAD:$F | sed 's/born-RED R-b J-12 J-13 (SK/born-RED R-b J-12 (SK/' | git hash-object -w --stdin)
git update-index --cacheinfo 100644,$blob,$F; tree=$(git write-tree); mrev=$(git commit-tree $tree -p HEAD -m "mutant: J-13 unnamed")
unset GIT_INDEX_FILE; rm -f $S/mutant.index
for i in 1 2; do $R $L/rb-mutant-r$i.log $W "LW3.ORC1-RB MUTANT r$i (rb_json_ws.rs no longer names J-13; tree $mrev)" -- python3 $CK rows $mrev; done
git status --short
