#!/bin/bash
for a in ad0dc8a 92ff470; do for c in "css css/bootstrap.css" "css css/tailwind-output.css" "json json/twitter.json"; do set -- $c
 echo "$a $1 $2 $(DIGEST=1 /Users/mkbabb/.dev-logs/bbnf-lockstep/s11-arms/bin/perf3-$a $1 /Users/mkbabb/.dev-logs/bbnf-lockstep/corpus-pt-v1/data/$2 0 2>&1 | tail -1)"; done; done
