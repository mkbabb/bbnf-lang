#!/bin/bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
    echo "usage: $0 RAW_LOG" >&2
    exit 64
fi

LC_ALL=C sed -E \
    -e 's#/private/tmp/bbnf-g3-r3-[^/ ]+#<REPLAY_ROOT>#g' \
    -e 's#/tmp/bbnf-g3-r3-[^/ ]+#<REPLAY_ROOT>#g' \
    -e 's/\([0-9]+\) panicked/(<THREAD>) panicked/g' \
    -e 's/finished in [0-9.]+s/finished in <TIME>/g' \
    "$1" | awk '
        /^=== COMMAND / ||
        /^=== EXIT / ||
        /^running [0-9]+ test/ ||
        /^test egraph_/ ||
        /^test result:/ ||
        /^failures:$/ ||
        /^    egraph_/ ||
        /^thread .* panicked at/ ||
        /^assertion .* failed/ ||
        /^  left:/ ||
        /^ right:/ ||
        /nullable alternation rejected empty input/ ||
        /^error: test failed/ {
            print
        }
    '
