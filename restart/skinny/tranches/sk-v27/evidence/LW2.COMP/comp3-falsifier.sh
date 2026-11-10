#!/usr/bin/env bash
# LW2.COMP-3 falsifier (S5 COMP).
#   comp3-falsifier.sh red      <tree> <log>
#   comp3-falsifier.sh baseline <tree> <log> <outcomes-out>
#   comp3-falsifier.sh green    <tree> <log> <outcomes-out> <baseline-outcomes>
# red:      direct dependents of csp-solver in the root workspace, every edge kind
#           (`cargo tree -i csp-solver -e all --depth 1`); GREEN iff exactly {bbnf-ir}.
# baseline: the nextest outcome set ("<binary> <test> <outcome>" per test, corpus fetched
#           and verified first, `cargo nextest run --workspace --profile ci`), plus the
#           substrate_audit item list.
# green:    the dependents read, `cargo xtask regen --check` rc 0, and the nextest set equal to
#           the baseline apart from the LW2.COMP-2 flips (the four csp_contract tests and
#           plan_contract's contradiction test, which pass where the base ignored or lacked
#           them) once the moved unit-test module is renamed (bbnf
#           backend::rust::analysis::inline::csp_contract -> bbnf-ir passes::call_mode::csp_contract);
#           substrate_audit names the same items as the baseline.
# rc 0 = GREEN, rc 1 = RED.
set -u
MODE=$1; T=$2; LOG=$3; OUT=${4:-}; BASE=${5:-}
: > "$LOG"; say() { echo "$*" | tee -a "$LOG"; }
red=0
cd "$T" || exit 2
say "tree $(git rev-parse HEAD)"
if [ "$MODE" != baseline ]; then
  deps=$(cargo tree --locked -i csp-solver -e all --depth 1 --prefix none 2>>"$LOG" | tail -n +2 | awk '{print $1}' | sort -u | tr '\n' ' ')
  say "csp-solver direct dependents: ${deps:-<none>}"
  [ "$deps" = "bbnf-ir " ] || red=1
fi
if [ "$MODE" != red ]; then
  if [ "$MODE" = green ]; then
    cargo xtask regen --check >>"$LOG" 2>&1; rc=$?; say "regen --check rc=$rc"; [ $rc = 0 ] || red=1
  fi
  cargo xtask corpus fetch >>"$LOG" 2>&1 && cargo xtask corpus verify >>"$LOG" 2>&1; say "corpus fetch+verify rc=$?"
  cargo nextest run --workspace --profile ci --no-fail-fast >>"$LOG" 2>&1; rc=$?; say "nextest rc=$rc"
  python3 - target/nextest/ci/junit.xml "$OUT" <<'PY'
import sys, xml.etree.ElementTree as ET
root = ET.parse(sys.argv[1]).getroot()
rows = []
for suite in root.iter("testsuite"):
    for case in suite.iter("testcase"):
        kind = "pass"
        if case.find("failure") is not None or case.find("error") is not None:
            kind = "fail"
        elif case.find("skipped") is not None:
            kind = "skip"
        name, binary = case.get("name"), suite.get("name")
        old = "backend::rust::analysis::inline::csp_contract::"
        if name.startswith(old):
            name, binary = "passes::call_mode::csp_contract::" + name[len(old):], "bbnf-ir"
        rows.append(f"{binary} {name} {kind}")
open(sys.argv[2], "w").write("\n".join(sorted(set(rows))) + "\n")
PY
  say "outcomes: $(wc -l < "$OUT" | tr -d ' ') rows, $(grep -c ' fail$' "$OUT") fail -> $OUT"
  grep ' fail$' "$OUT" | tee -a "$LOG"
  awk '/substrate-audit FAIL/{f=1} f && /^ +(fn|struct|enum|trait|const|static|type|mod|union) /{print $2}' "$LOG" | sort -u > "$OUT.audit"
  say "substrate_audit items: $(tr '\n' ' ' < "$OUT.audit")"
  if [ "$MODE" = green ]; then
    flips='csp_contract::(inline_domain_satisfies_the_lattice_laws|conflicting_forced_call_modes_are_unsatisfiable|inline_domain_join_is_a_lattice_join_over_generated_cases|bitset_lattice_domain_satisfies_the_lattice_laws) |contradictory_call_mode_directives_are_refused '
    d=$(diff <(grep -vE "$flips" "$BASE") <(grep -vE "$flips" "$OUT"))
    if [ -z "$d" ]; then say "nextest set: equal to the baseline outside the COMP-2 flips"; else say "nextest set: DIFFERS from the baseline:"; echo "$d" | tee -a "$LOG"; red=1; fi
    flipped=$(grep -cE "$flips" "$OUT"); flipfail=$(grep -E "$flips" "$OUT" | grep -vc ' pass$')
    say "COMP-2 flip rows now: $flipped, not passing: $flipfail (want 5 and 0)"; { [ "$flipped" = 5 ] && [ "$flipfail" = 0 ]; } || red=1
    if diff -q "$BASE.audit" "$OUT.audit" >/dev/null; then say "substrate_audit items: equal to the baseline"; else say "substrate_audit items DIFFER: base $(tr '\n' ' ' < "$BASE.audit")"; red=1; fi
  fi
fi
say "VERDICT: $([ $red = 0 ] && echo GREEN || echo RED)"
exit $red
