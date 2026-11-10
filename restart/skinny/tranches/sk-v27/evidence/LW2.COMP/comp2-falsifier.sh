#!/usr/bin/env bash
# LW2.COMP-2 falsifier (S5 COMP). Usage: comp2-falsifier.sh <bbnf-lang tree> <parse-that tree> <log>
# Reads, in order:
#  (1) the order's pinned form. git's ERE on this host (macOS) has no `\s`, so the
#      literal `^\s*let _ = csp\.propagate\(\);` reads 0 everywhere; the measure is
#      its POSIX spelling `^[[:space:]]*let _ = csp\.propagate\(\);` over bbnf-lang
#      `crates`. Also the broad form (any discarded propagate result) over live code
#      in both repos, which counts the site TOPO-1 moved to parse-that-egraph and
#      the two `system.csp.propagate()` discards in bbnf-ir's type projection.
#  (2) the born-RED contract tests (lattice laws, 10^4 generated cases, the
#      Decided/Decided CSP conflict, the contradictory grammar), ignored ones included.
#  (3) `cargo xtask regen --check` (in-tree grammars unchanged).
# rc 0 = GREEN (pinned 0, broad 0, tests pass, regen clean); rc 1 = RED.
set -u
BL=$1; PT=$2; LOG=$3
: > "$LOG"
say() { echo "$*" | tee -a "$LOG"; }
red=0
pinned=$(git -C "$BL" grep -nE '^[[:space:]]*let _ = csp\.propagate\(\);' -- crates | tee -a "$LOG" | wc -l | tr -d ' ')
literal=$(git -C "$BL" grep -nE '^\s*let _ = csp\.propagate\(\);' -- crates | wc -l | tr -d ' ')
say "pinned form (POSIX) over bbnf-lang crates: $pinned   (order's literal \\s form on this host: $literal)"
broad_bl=$(git -C "$BL" grep -nE 'let _ = [A-Za-z_.]*propagate(_with)?\(' -- crates xtask wasm skinny/crates | tee -a "$LOG" | wc -l | tr -d ' ')
broad_pt=$(git -C "$PT" grep -nE 'let _ = [A-Za-z_.]*propagate(_with)?\(' -- rust | tee -a "$LOG" | wc -l | tr -d ' ')
say "broad form: bbnf-lang $broad_bl, parse-that $broad_pt"
[ "$pinned" = 0 ] && [ "$broad_bl" = 0 ] && [ "$broad_pt" = 0 ] || red=1
cd "$BL" || exit 2
say "--- cargo test -p bbnf --lib csp_contract -- --include-ignored"
cargo test -q -p bbnf --lib csp_contract -- --include-ignored >>"$LOG" 2>&1; rc=$?; say "rc=$rc"; [ $rc = 0 ] || red=1
say "--- cargo test -p bbnf --test plan_contract -- --include-ignored"
cargo test -q -p bbnf --test plan_contract -- --include-ignored >>"$LOG" 2>&1; rc=$?; say "rc=$rc"; [ $rc = 0 ] || red=1
say "--- cargo xtask regen --check"
cargo xtask regen --check >>"$LOG" 2>&1; rc=$?; say "rc=$rc"; [ $rc = 0 ] || red=1
grep -E "^test .*(FAILED|ok)$|test result|panicked|cases break|reaches" "$LOG" | head -40 >/dev/null
say "VERDICT: $([ $red = 0 ] && echo GREEN || echo RED)"
exit $red
