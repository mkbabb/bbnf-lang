#!/usr/bin/env bash
# scripts/iai-compare.sh — the iai-callgrind instruction-count gate.
#
#   iai-compare.sh <baseline-json>                 check the baseline is seeded
#   iai-compare.sh <baseline-json> <iai-output>    compare a run against it
#
# Exit status (sk-v27 LW3.CI-IAI-0):
#   0  every baselined bench is within ±threshold_pct of its seeded count
#      (or, with one argument, the baseline is seeded and well formed);
#   1  some bench moved by more than threshold_pct, in either direction;
#   2  the gate cannot decide: a null or malformed baseline, an unparseable
#      run, a baselined bench missing from the run, or a bench the run
#      reports that the baseline does not carry.
# A null baseline is refused (2); it never reads as a pass.
set -euo pipefail

if [[ $# -lt 1 || $# -gt 2 ]]; then
  echo "usage: $0 <baseline-json> [<iai-output>]" >&2
  exit 2
fi

exec python3 - "$@" <<'PY'
import json
import math
import pathlib
import re
import sys

REFUSE = 2


def refuse(msg: str) -> None:
    print(f"error: {msg}")
    sys.exit(REFUSE)


try:
    baseline = json.loads(pathlib.Path(sys.argv[1]).read_text())
except (OSError, ValueError) as exc:
    refuse(f"baseline unreadable: {exc}")

threshold = baseline.get("threshold_pct")
if not isinstance(threshold, (int, float)) or isinstance(threshold, bool) \
        or not math.isfinite(threshold) or threshold <= 0:
    refuse(f"baseline threshold_pct is not a positive number: {threshold!r}")

benches = baseline.get("benchmarks")
print(f"threshold_pct: {threshold:g}")
if not isinstance(benches, dict) or not benches:
    print("baseline_valgrind_instrs: null")
    refuse("baseline is unseeded (no benchmarks); the gate refuses a null baseline")

for field in ("seeded_at", "commit_sha", "valgrind_version", "runner_image", "source_run"):
    if not baseline.get(field):
        refuse(f"baseline field {field!r} is null or absent")

base_counts: dict[str, int] = {}
for bench_id, metrics in benches.items():
    ir = metrics.get("instructions") if isinstance(metrics, dict) else None
    if not isinstance(ir, int) or isinstance(ir, bool) or ir <= 0:
        refuse(f"baseline instructions for {bench_id} is not a positive integer: {ir!r}")
    base_counts[bench_id] = ir

if len(sys.argv) == 2:
    for bench_id, ir in sorted(base_counts.items()):
        print(f"baseline {bench_id} instructions={ir}")
    print(f"status: baseline seeded at {baseline['commit_sha']} "
          f"(run {baseline['source_run']}, valgrind {baseline['valgrind_version']}, "
          f"{baseline['runner_image']})")
    sys.exit(0)

# iai-callgrind 0.12 prints each bench as an id line `file::group::bench`
# followed by indented `Metric:   current|old   (delta)` lines.
ID_RE = re.compile(r"^([A-Za-z_][\w]*(?:::[\w]+)+)(?:\s.*)?$")
IR_RE = re.compile(r"^\s+Instructions:\s+([0-9]+)\|")

try:
    text = pathlib.Path(sys.argv[2]).read_text(errors="replace")
except OSError as exc:
    refuse(f"iai output unreadable: {exc}")

current: dict[str, int] = {}
bench_id = None
for line in text.splitlines():
    m = ID_RE.match(line)
    if m:
        bench_id = m.group(1)
        continue
    m = IR_RE.match(line)
    if m and bench_id is not None:
        if bench_id in current:
            refuse(f"bench {bench_id} reported twice")
        current[bench_id] = int(m.group(1))
        bench_id = None

if not current:
    refuse("could not parse an instruction count from the iai-callgrind output")

missing = sorted(set(base_counts) - set(current))
extra = sorted(set(current) - set(base_counts))
if missing:
    refuse(f"baselined bench(es) absent from the run: {', '.join(missing)}")
if extra:
    refuse(f"run reports bench(es) the baseline does not carry: {', '.join(extra)}")

worst = 0
for bid in sorted(base_counts):
    base, cur = base_counts[bid], current[bid]
    delta = (cur - base) / base * 100.0
    verdict = "ok" if abs(delta) <= threshold else (
        "regression" if delta > 0 else "improvement-unrebased")
    print(f"{bid}: baseline={base} current={cur} delta_pct={delta:+.3f} -> {verdict}")
    if verdict != "ok":
        worst = 1

print("status: " + ("outside" if worst else "within") + f" ±{threshold:g}%")
sys.exit(worst)
PY
