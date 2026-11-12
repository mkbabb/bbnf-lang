#!/usr/bin/env bash
# sk-v27 LW3.CI-IAI-0 reads, from the bbnf-lang root:
#   sh restart/skinny/tranches/sk-v27/evidence/LW3.CI/ci-iai-0-reads.sh <label>
# GREEN: the seeded baseline against its own seed run exits 0 (within ±1 %);
#        the entry stub exits 2 (refused), with and without a run.
# Mutants: the seed run's count raised 2 % and lowered 2 % each exit 1.
# Prints one `read=… rc=… want=…` line each and exits non-zero on any miss.
set -u
E=restart/skinny/tranches/sk-v27/evidence/LW3.CI
RUN=$E/seed-run-43148549427/iai-pr.txt
T=$(mktemp -d "${TMPDIR:-/tmp}/iai-reads.XXXXXX"); trap 'rm -rf "$T"' EXIT
python3 - "$RUN" "$T" <<'PY'
import re, sys, pathlib
src = pathlib.Path(sys.argv[1]).read_text()
m = re.search(r"^(\s+Instructions:\s+)([0-9]+)(\|)", src, re.M)
n = int(m.group(2))
for tag, f in (("up2", 1.02), ("down2", 0.98)):
    v = round(n * f)
    pathlib.Path(sys.argv[2], f"iai-{tag}.txt").write_text(src[:m.start(2)] + str(v) + src[m.end(2):])
PY
fail=0
read() { want=$1; name=$2; shift 2; out=$(scripts/iai-compare.sh "$@" 2>&1); rc=$?
  echo "read=$name rc=$rc want=$want :: $(echo "$out" | tail -1)"; [ "$rc" = "$want" ] || fail=1; }
read 0 green-seeded-vs-seed-run docs/benchmarks/iai-baselines/json.json "$RUN"
read 0 green-seeded-self-check  docs/benchmarks/iai-baselines/json.json
read 2 green-stub-refused       "$E/json.stub-28c09a0a5.json" "$RUN"
read 2 green-stub-self-check    "$E/json.stub-28c09a0a5.json"
read 1 mutant-up-2pct           docs/benchmarks/iai-baselines/json.json "$T/iai-up2.txt"
read 1 mutant-down-2pct         docs/benchmarks/iai-baselines/json.json "$T/iai-down2.txt"
echo "label=${1:-} head=$(git rev-parse HEAD) fail=$fail"
exit $fail
