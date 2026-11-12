#!/bin/zsh
# GREEN x2 and mutants x2 for LW3.ORC1-RB, LW3.ORC2 and the COMP-3a reading, in the clean read tree at the lane head.
S=$HOME/.dev-logs/bbnf-lockstep/lw3/s5b-oracle; R=$S/bin/read.sh; W=$S/red-wt; L=$S/logs
export CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang BBNF_CORPUS_DIR=$S/corpus
OC=$S/oracle-cache; PG=$OC/peggy/0e7f00d12812716b
cd $W; git checkout -q --detach ea8997adb
cargo clean -p bbnf -p bbnf-oracle -p bbnf-bootstrap -p xtask 2>&1 | tail -1 > $L/orc2-green-clean.receipt
cargo build -q -p bbnf-oracle --bin g1 && cargo build -q --release -p bbnf-oracle --bin g1 || exit 3
G=$CARGO_TARGET_DIR/debug/g1; GR=$CARGO_TARGET_DIR/release/g1; CK=restart/skinny/tranches/sk-v27/evidence/LW3.ORC/orc_check.py
NINE='^grammar=(bbnf|json|css_l4|css_pretty|google_sheets|ebnf|bnf|csv|math) '
for i in 1 2; do
  $R $L/rb-green-r$i.log $W "LW3.ORC1-RB rows GREEN r$i" -- python3 $CK rows HEAD
  $R $L/orc2-green-r$i.log $W "LW3.ORC2 list-oracles GREEN r$i" -- zsh -c "$G --list-oracles; n=\$($G --list-oracles | grep -E '$NINE' | grep -c 'oracles=0'); echo \"nine grammars with no oracle: \$n\"; [ \$n -eq 0 ]"
  $R $L/comp3a-g1-r$i.log $W "COMP-3a G1 reading r$i (release g1; corpus-v3 verified)" -- $GR --all --heldout --manifest grammar/MANIFEST
done
for g in ebnf bnf math; do $GR --grammar $g --arms root,ts-package --dump 2>/dev/null | cut -c1-400 > $L/dump-$g.log; done
for i in 1 2; do
  $R $L/wsmodel-green-r$i.log $W "LW3.ORC2 class proof GREEN r$i" -- node restart/skinny/tranches/sk-v27/evidence/LW3.ORC/ws_model.mjs $PG $BBNF_CORPUS_DIR $L/comp3a-g1-r1.log $L/dump-ebnf.log $L/dump-bnf.log $L/dump-math.log
done
# Mutant 1 (ORC2): the grammar moves under its transcription (one byte of grammar/ebnf/ebnf.bbnf).
printf '\n' >> grammar/ebnf/ebnf.bbnf
for i in 1 2; do
  $R $L/orc2-mutant-r$i.log $W "LW3.ORC2 MUTANT r$i (ebnf.bbnf gains a byte; its transcription is stale)" -- zsh -c "$G --list-oracles 2>&1; n=\$($G --list-oracles 2>/dev/null | grep -E '$NINE' | grep -c 'oracles=0'); echo \"nine grammars with no oracle: \$n\"; [ \$n -eq 0 ]"
done
git checkout -q -- grammar/ebnf/ebnf.bbnf
# Mutant 2 (class proof): root's model loses its whitespace before every rule.
M=restart/skinny/tranches/sk-v27/evidence/LW3.ORC/models/root-math.peggy
sed -i '' 's/^number = RW (/number = (/' $M
for i in 1 2; do
  $R $L/wsmodel-mutant-r$i.log $W "LW3.ORC2 MUTANT r$i (root-math model without RW before number)" -- node restart/skinny/tranches/sk-v27/evidence/LW3.ORC/ws_model.mjs $PG $BBNF_CORPUS_DIR $L/comp3a-g1-r1.log $L/dump-ebnf.log $L/dump-bnf.log $L/dump-math.log
done
git checkout -q -- $M
# Mutant 3 (RB): J-13's falsifier stops naming it (a tree object, no ref moved).
export GIT_INDEX_FILE=$S/mutant.index; git read-tree HEAD
F=crates/oracle/tests/rb_json_ws.rs
blob=$(git show HEAD:$F | sed 's/born-RED R-b J-12 J-13 (SK/born-RED R-b J-12 (SK/' | git hash-object -w --stdin)
git update-index --cacheinfo 100644,$blob,$F; tree=$(git write-tree); mrev=$(git commit-tree $tree -p HEAD -m "mutant: J-13 unnamed")
unset GIT_INDEX_FILE; rm -f $S/mutant.index
for i in 1 2; do
  $R $L/rb-mutant-r$i.log $W "LW3.ORC1-RB MUTANT r$i (rb_json_ws.rs no longer names J-13; tree $mrev)" -- python3 $CK rows $mrev
done
git status --short
