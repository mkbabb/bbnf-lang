#!/bin/zsh
# Attribute the regen --check drift: base 492402233 vs S2 HEAD 4e7924776.
set -u
d=~/.dev-logs/bbnf-lockstep/lw2/s2-census
R=~/Programming/bbnf-lang-wt/lw2-s2-census
export CARGO_TARGET_DIR=$R/target
for tag in base:492402233 head:4e7924776; do
  n=${tag%%:*}; sha=${tag#*:}; w=~/Programming/bbnf-lang-wt/lw2-s2-regen-$n
  [ -d $w ] || git -C $R worktree add --detach $w $sha >/dev/null 2>&1
  cd $w
  cargo xtask regen --check > $d/attrib-check-$n.log 2>&1; echo "$n regen --check rc=$? drift=$(grep -c '^drift' $d/attrib-check-$n.log)"
  cargo xtask regen > $d/attrib-regen-$n.log 2>&1; echo "$n regen rc=$?"
  git status --porcelain > $d/attrib-status-$n.txt
  shasum -a 256 crates/core/src/grammar/generated/*.rs | sed "s#  .*/#  #" > $d/attrib-hash-$n.txt
done
diff $d/attrib-hash-base.txt $d/attrib-hash-head.txt > $d/attrib-hash.diff; echo "regenerated base-vs-head hash diff rc=$?"
