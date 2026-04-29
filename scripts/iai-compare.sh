#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "usage: $0 <baseline-json> <iai-output>" >&2
  exit 2
fi

python3 - "$1" "$2" <<'PY'
import json
import pathlib
import re
import sys

baseline_path = pathlib.Path(sys.argv[1])
current_path = pathlib.Path(sys.argv[2])

baseline = json.loads(baseline_path.read_text())
current_text = current_path.read_text(errors="replace")


def parse_instruction_count(text: str) -> int | None:
    patterns = [
        r"Instructions:\s*([0-9][0-9,]*)",
        r"\bIr\s*(?:=|:)\s*([0-9][0-9,]*)",
        r"\binstructions?\D+([0-9][0-9,]*)",
    ]
    for pattern in patterns:
        match = re.search(pattern, text, re.IGNORECASE)
        if match:
            return int(match.group(1).replace(",", ""))
    return None


bench_name = baseline.get("bench_name", "json_callgrind")
threshold = float(baseline.get("threshold_pct", 1.0))
base_instrs = baseline.get("valgrind_instrs")
current_instrs = parse_instruction_count(current_text)

print(f"bench: {bench_name}")
print(f"threshold_pct: {threshold:g}")

if current_instrs is None:
    print("error: could not parse an instruction count from iai-callgrind output")
    sys.exit(2)

print(f"current_valgrind_instrs: {current_instrs}")

if base_instrs is None:
    print("baseline_valgrind_instrs: null")
    print("status: baseline is unseeded; regression decision skipped")
    sys.exit(0)

base_instrs = int(base_instrs)
delta_pct = ((current_instrs - base_instrs) / base_instrs) * 100.0

print(f"baseline_valgrind_instrs: {base_instrs}")
print(f"delta_pct: {delta_pct:+.3f}")

if delta_pct > threshold:
    print(f"regression {delta_pct:+.3f}% > {threshold:g}")
else:
    print(f"ok {delta_pct:+.3f}% <= {threshold:g}")
PY
