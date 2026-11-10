#!/usr/bin/env bash
# LW2.COMP-3 falsifier (S5 COMP). Usage: comp3-falsifier.sh <bbnf-lang tree> <log> <outcomes-out> [<baseline-outcomes>]
#  (1) direct dependents of csp-solver in the root workspace, every edge kind
#      (`cargo tree -i csp-solver -e all --depth 1`): GREEN iff exactly {bbnf-ir};
#  (2) `cargo xtask regen --check` rc 0;
#  (3) the nextest outcome set (`cargo nextest run --workspace --profile ci`, the corpus
#      fetched and verified first): one line per test "<binary> <test> <outcome>", written
#      to <outcomes-out>. With <baseline-outcomes> (the set read at the COMP-2 cure), GREEN
#      iff the two sets are equal once the moved unit-test module is renamed
#      (bbnf::backend::rust::analysis::inline::csp_contract -> bbnf-ir::passes::call_mode::csp_contract).
# rc 0 = GREEN, rc 1 = RED.
set -u
T=$1; LOG=$2; OUT=$3; BASE=${4:-}
: > "$LOG"; say() { echo "$*" | tee -a "$LOG"; }
red=0
cd "$T" || exit 2
deps=$(cargo tree --locked -i csp-solver -e all --depth 1 --prefix none 2>>"$LOG" | tail -n +2 | awk '{print $1}' | sort -u | tr '\n' ' ')
say "csp-solver direct dependents: ${deps:-<none>}"
[ "$deps" = "bbnf-ir " ] || red=1
cargo xtask regen --check >>"$LOG" 2>&1; rc=$?; say "regen --check rc=$rc"; [ $rc = 0 ] || red=1
cargo xtask corpus fetch >>"$LOG" 2>&1 && cargo xtask corpus verify >>"$LOG" 2>&1; say "corpus fetch+verify rc=$?"
cargo nextest run --workspace --profile ci --no-fail-fast >>"$LOG" 2>&1; rc=$?; say "nextest rc=$rc"
J=target/nextest/ci/junit.xml
python3 - "$J" "$OUT" <<'PY'
import sys, xml.etree.ElementTree as ET
root = ET.parse(sys.argv[1]).getroot()
rows = []
for suite in root.iter("testsuite"):
    for case in suite.iter("testcase"):
        kind = "pass"
        for tag in ("failure", "error", "skipped", "flakyFailure", "rerunFailure"):
            if case.find(tag) is not None:
                kind = {"skipped": "skip", "flakyFailure": "flaky", "rerunFailure": "flaky"}.get(tag, "fail")
                if kind == "fail":
                    break
        name = case.get("name").replace("backend::rust::analysis::inline::csp_contract::", "passes::call_mode::csp_contract::")
        binary = suite.get("name")
        if "passes::call_mode::csp_contract::" in name:
            binary = "bbnf-ir"
        rows.append(f"{binary} {name} {kind}")
open(sys.argv[2], "w").write("\n".join(sorted(rows)) + "\n")
print(f"{len(rows)} tests; fail={sum(r.endswith(' fail') for r in rows)}")
PY
say "outcomes: $(wc -l < "$OUT" | tr -d ' ') rows, $(grep -c ' fail$' "$OUT") fail -> $OUT"
grep ' fail$' "$OUT" | tee -a "$LOG"
if [ -n "$BASE" ]; then
  if diff <(sed 's/^bbnf-ir passes::call_mode/bbnf passes::call_mode/' "$BASE" | sed 's/^bbnf passes::call_mode/bbnf-ir passes::call_mode/' | sort) <(sort "$OUT") >>"$LOG"; then
    say "nextest set: equal to the baseline"
  else
    say "nextest set: DIFFERS from the baseline (diff above)"; red=1
  fi
fi
say "VERDICT: $([ $red = 0 ] && echo GREEN || echo RED)"
exit $red
