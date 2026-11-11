#!/bin/sh
# One LG-2-B attempt: rerun the failed iai job (no workflow change), wait, classify.
# Usage: iai-once.sh <run-id> [norerun]  exit 0 GREEN, 10 network-RED, 5 real RED, 2 still running
R=$1; L=~/.dev-logs/bbnf-lockstep/lw2/r7; B=mkbabb/bbnf-lang
[ "${2:-}" = norerun ] || gh run rerun $R -R $B --failed >/dev/null 2>&1 || gh run rerun $R -R $B >/dev/null 2>&1
sleep 20; i=0
while :; do s=$(gh run view $R -R $B --json status,conclusion,attempt -q '.status+" "+(.conclusion//"")+" "+(.attempt|tostring)' 2>/dev/null)
  set -- $s; [ "$1" = completed ] && break; i=$((i+1)); [ $i -gt 16 ] && { echo "run $R still $s"; exit 2; }; sleep 30; done
a=$3; c=$2; echo "$(date -u +%FT%TZ) run $R attempt $a $c" | tee -a $L/iai-attempts.log
[ "$c" = success ] && exit 0
gh run view $R -R $B --attempt $a --log-failed > $L/iai-$R-a$a.log 2>&1
fs=$(gh run view $R -R $B --attempt $a --json jobs -q '.jobs[].steps[]|select(.conclusion=="failure")|.name' | tr '\n' ',')
if [ "$fs" = "install valgrind," ] && grep -qE "Temporary failure resolving|Could not resolve|Failed to fetch|Unable to connect" $L/iai-$R-a$a.log; then
  echo "  failed step: $fs class=network/archive ($(grep -oE "Temporary failure resolving '[^']+'" $L/iai-$R-a$a.log | sort -u | tr '\n' ' '))" | tee -a $L/iai-attempts.log; exit 10; fi
echo "  failed step: $fs class=REAL" | tee -a $L/iai-attempts.log; exit 5
