#!/bin/sh
# LW2.PERF-1 bbnf-lang reads at the seat tree. Usage: perf1-bbnf-reads.sh <tree>
cd "${1:?tree}" || exit 2
cargo +1.99.1 xtask corpus verify >/dev/null 2>&1 || cargo +1.99.1 xtask corpus fetch >/dev/null 2>&1; echo "corpus verify rc=$(cargo +1.99.1 xtask corpus verify >/dev/null 2>&1; echo $?)"
echo "tree $(git rev-parse --short HEAD) (+ working changes: $(git status --short | wc -l | tr -d ' ') paths)"
cargo +1.99.1 xtask regen --check >/tmp/.p1 2>&1; echo "regen --check rc=$?"; grep -E 'drift|differ|stale|OK|up to date' /tmp/.p1 | tail -3
cargo +1.99.1 iter-clippy >/tmp/.p1 2>&1; echo "iter-clippy rc=$?"; grep -E '^(warning|error)' /tmp/.p1 | sort | uniq -c | head
cargo +1.99.1 test -p bbnf-ir --test substrate_audit --locked >/tmp/.p1 2>&1; echo "substrate_audit rc=$?"; grep -E 'test result|zero|fn |substrate' /tmp/.p1 | head -8
cargo +1.99.1 test -p bbnf --locked --test json_parity --test css_l4_parity >/tmp/.p1 2>&1; echo "json_parity+css_l4_parity rc=$?"; grep -E 'test result' /tmp/.p1
rm -f /tmp/.p1
