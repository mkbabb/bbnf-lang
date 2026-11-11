#!/bin/bash
B=~/.dev-logs/bbnf-lockstep/s11-arms/bin; C=~/.dev-logs/bbnf-lockstep/corpus-v1/data
for a in 63054dba0 88b60839f; do for c in "css_l4 css/bootstrap.css" "css_l4 css/tailwind.css" "css_pretty css/bootstrap.css"; do set -- $c
 echo "$a $1 $2 $(DIGEST=1 $B/rx4-$a $1 $C/$2 0 2>&1 | tail -1)"; done; done
