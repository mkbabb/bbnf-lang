#!/bin/sh
# sk-v27 LW1 lockstep gate LG-c: value.js consumer gates (the W9pin/W10close recipe), read-only, clean worktree at 2fefc1f2b, under guarded
cd /Users/mkbabb/.dev-logs/bbnf-lockstep/gate/vjs-2fefc1f2b || exit 1
O=/Users/mkbabb/.dev-logs/bbnf-lockstep/gate/lgc-vjs-2fefc1f2b
step(){ echo "=== $1 $(date +%T) load: $(uptime | sed 's/.*averages: //')"; }
for i in 1 2; do
  step "check-$i"; node scripts/gen-grammar.mjs --check > $O/check-$i.txt 2>&1; echo "exit $?"
  step "e6-$i"; for f in grammar color; do printf '%s %s / %s\n' $f "$(npx esbuild --minify --loader=js < src/css/bbnf/generated/$f.js | wc -c | tr -d ' ')" "$(npx esbuild --minify --loader=js < src/css/bbnf/generated/$f.js | gzip | wc -c | tr -d ' ')"; done > $O/e6-$i.txt; cat $O/e6-$i.txt
  step "self-contained-$i"; for f in grammar color; do printf '%s from@mkbabb=%s import=%s\n' $f "$(grep -c 'from "@mkbabb' src/css/bbnf/generated/$f.js)" "$(grep -cE '^\s*import ' src/css/bbnf/generated/$f.js)"; done > $O/selfcontained-$i.txt; cat $O/selfcontained-$i.txt
  step "test-css-$i"; npx vitest run test/css --maxWorkers=2 > $O/test-css-$i.log 2>&1; echo "exit $?"; grep -E 'Test Files|Tests ' $O/test-css-$i.log
  step "mirror-$i"; npm run test:css-equivalence -- --maxWorkers=2 > $O/mirror-$i.log 2>&1; echo "exit $?"; grep -E 'Test Files|Tests |MIRROR' $O/mirror-$i.log | head -4
  step "vc-$i"; npx vite-node bench/wpt-conformance/conformance.ts --misses all > $O/vc-$i.txt 2>&1; echo "exit $?"; tail -1 $O/vc-$i.txt
done
step done
