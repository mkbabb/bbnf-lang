#!/bin/sh
# PERF1D-B2 (b) merge gate read $1 (A or B): base 785027c44 vs cure, json_monolithic and css_l4, 12 rounds
# interleaved (order alternates per round), plus data_s at 10x iterations (the perf1d_datas10x example, sample_size 10) each round.
set -u
R=${1:?read id}; L=$HOME/.dev-logs/bbnf-lockstep/lw3/perf-1d/r3; O=$L/read-$R; mkdir -p $O
export BBNF_CORPUS_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/s7-spine/corpus
cp $L/bins/heads.txt $O/heads.txt; echo "read $R start $(date '+%F %T') load $(sysctl -n vm.loadavg)" >> $O/heads.txt
for r in 01 02 03 04 05 06 07 08 09 10 11 12; do
  case $r in 01|03|05|07|09|11) order="base cure";; *) order="cure base";; esac
  for v in $order; do
    while read name exe; do "$exe" --bench > $O/$name-$v-r$r.txt 2>&1; done < $L/bins/bins-$v.txt
    "$L/bins/datas10x-$v" --bench > $O/datas10x-$v-r$r.txt 2>&1
  done
done
echo "read $R end $(date '+%F %T') load $(sysctl -n vm.loadavg)" >> $O/heads.txt
