#!/bin/sh
# PERF-1d paired bench: base worktree (parent: emitted NEON/AVX2 kernels, pin 6889b29) vs cure lane.
# Builds json_monolithic and css_l4 (bench profile) in each, then runs them interleaved A B A B A B.
set -u
BASE=$1; CURE=$2; L=$HOME/.dev-logs/bbnf-lockstep/lw3/perf-1d/bench; mkdir -p $L
export BBNF_CORPUS_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/s7-spine/corpus
build() { # dir target-dir tag
  cd "$1" || exit 2
  CARGO_TARGET_DIR=$2 cargo bench -p bbnf --locked --bench json_monolithic --bench css_l4 --no-run --message-format=json 2>$L/build-$3.err \
   | python3 -I -c 'import json,sys
for l in sys.stdin:
    m=json.loads(l)
    if m.get("reason")=="compiler-artifact" and m.get("executable") and m["target"]["name"] in ("json_monolithic","css_l4"): print(m["target"]["name"], m["executable"])' > $L/bins-$3.txt
  echo "$3 head=$(git rev-parse HEAD) dirty=$(git status --short | grep -vc '^??')" >> $L/heads.txt
}
: > $L/heads.txt
build "$BASE" $HOME/.dev-logs/bbnf-lockstep/lw3/perf-1d/target-base base
build "$CURE" $HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang cure
for r in 1 2 3; do
  for tag in base cure; do
    while read name exe; do
      dir=$BASE; [ $tag = cure ] && dir=$CURE
      (cd "$dir/crates/core" && "$exe" --bench > $L/$name-$tag-r$r.txt 2>&1; echo "rc=$?" >> $L/$name-$tag-r$r.txt)
    done < $L/bins-$tag.txt
  done
done
