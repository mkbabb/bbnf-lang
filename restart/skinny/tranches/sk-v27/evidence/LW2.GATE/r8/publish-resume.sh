#!/bin/bash
# Resume publish.sh from a crate; on crates.io's new-crate 429, wait until its stated retry time and resume
# from the refused crate (nothing else changes). Usage: publish-resume.sh <from-crate>
D=~/.dev-logs/bbnf-lockstep/lw2/r8; FROM=$1
A=(~/Programming/pprint-wt/lw2-topo-4e-0.4 ~/Programming/parse-that-wt/publish-lw2 ~/Programming/bbnf-lang-wt/publish-lw2)
for i in 1 2 3 4; do
  out=$(sh $D/publish.sh "${A[@]}" $FROM 2>&1); rc=$?; echo "$out"
  [ $rc -eq 0 ] && exit 0
  echo "$out" | grep -q '429 Too Many Requests' || exit $rc
  FROM=$(echo "$out" | awk -F'\t' '$4 ~ /publish rc=/ && $4 !~ /rc=0/ {print $1}' | tail -1)
  t=$(echo "$out" | grep -o 'try again after [^a]*GMT' | sed 's/try again after //')
  e=$(TZ=UTC date -j -f '%a, %d %b %Y %H:%M:%S GMT' "$t" +%s 2>/dev/null || date -u +%s); now=$(date -u +%s)
  w=$(( e - now + 15 )); [ $w -lt 15 ] && w=15; echo "429 on $FROM; waiting ${w}s until $t"; sleep $w
done; exit 9
