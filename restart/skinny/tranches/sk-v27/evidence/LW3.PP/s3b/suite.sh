#!/bin/sh
# usage: suite.sh <dir> <logprefix> [extra cargo args]
# Runs every pprint test target as its own `guarded --light` run (one test
# file each, two test threads), so no run needs a heavy slot; then clippy.
dir=$1; pre=$2; shift 2
L=/Users/mkbabb/.dev-logs/bbnf-lockstep/lw3/s3b-pp
cd "$dir" || exit 2
tests=$(ls tests/*.rs | sed 's#tests/##; s#\.rs$##')
fail=0
for t in lib $tests; do
  if [ "$t" = lib ]; then sel="--lib"; else sel="--test $t"; fi
  ~/.claude/guard/guarded --light --timeout 20 -- $L/read.sh "$dir" "$pre-$t.log" cargo test --release "$@" $sel -- --test-threads=2 --nocapture || fail=1
done
~/.claude/guard/guarded --light --timeout 20 -- $L/read.sh "$dir" "$pre-doc.log" cargo test --release --doc -- --test-threads=2 --nocapture || fail=1
~/.claude/guard/guarded --light --timeout 20 -- $L/read.sh "$dir" "$pre-clippy.log" cargo clippy --workspace --all-targets -- -D warnings || fail=1
echo "suite rc=$fail"
exit $fail
