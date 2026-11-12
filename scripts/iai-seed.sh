#!/usr/bin/env bash
# scripts/iai-seed.sh — write the iai baseline from a downloaded CI artifact.
#
#   gh run download <run-id> --repo mkbabb/bbnf-lang --name iai-pr --dir <dir>
#   scripts/iai-seed.sh <dir> docs/benchmarks/iai-baselines/json.json
#
# The baseline is made from one pinned CI run's `iai-pr` artifact
# (`iai-pr.txt` plus `iai-env.json`), never from a local run and never by a
# commit from inside CI (sk-v27 LW3.CI-IAI-0). Every metric iai-callgrind
# prints is recorded; `instructions` is the gated one.
set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "usage: $0 <artifact-dir> <baseline-json>" >&2
  exit 2
fi

exec python3 - "$@" <<'PY'
import hashlib
import json
import pathlib
import re
import sys

art = pathlib.Path(sys.argv[1])
out = pathlib.Path(sys.argv[2])
pr = (art / "iai-pr.txt").read_bytes()
env = json.loads((art / "iai-env.json").read_text())

for field in ("commit_sha", "run_id", "run_attempt", "valgrind_version",
              "runner_image", "rustc", "iai_callgrind_runner", "finished_at"):
    if not env.get(field):
        sys.exit(f"error: iai-env.json field {field!r} is null or absent")

ID_RE = re.compile(r"^([A-Za-z_][\w]*(?:::[\w]+)+)(?:\s.*)?$")
METRIC_RE = re.compile(r"^\s+([A-Za-z0-9 +/]+?):\s+([0-9]+)\|")

benches: dict[str, dict[str, int]] = {}
bench_id = None
for line in pr.decode("utf-8", "replace").splitlines():
    m = ID_RE.match(line)
    if m:
        bench_id = m.group(1)
        continue
    m = METRIC_RE.match(line)
    if m and bench_id is not None:
        key = re.sub(r"[^a-z0-9]+", "_", m.group(1).lower()).strip("_")
        benches.setdefault(bench_id, {})[key] = int(m.group(2))

if not benches or any("instructions" not in v for v in benches.values()):
    sys.exit("error: no instruction count parsed from iai-pr.txt")

baseline = {
    "_note": "Seeded by scripts/iai-seed.sh from the iai-pr artifact of the CI run named in source_run (sk-v27 LW3.CI-IAI-0). Re-seed only the same way. scripts/iai-compare.sh gates on benchmarks.*.instructions at ±threshold_pct and refuses a null baseline.",
    "grammar": "json/json.bbnf",
    "threshold_pct": 1.0,
    "benchmarks": dict(sorted(benches.items())),
    "seeded_at": env["finished_at"],
    "commit_sha": env["commit_sha"],
    "source_run": f"https://github.com/mkbabb/bbnf-lang/actions/runs/{env['run_id']}/attempts/{env['run_attempt']}",
    "source_artifact_sha256": hashlib.sha256(pr).hexdigest(),
    "valgrind_version": env["valgrind_version"],
    "valgrind_source": env.get("valgrind_source", ""),
    "runner_image": env["runner_image"],
    "rustc": env["rustc"],
    "iai_callgrind_runner": env["iai_callgrind_runner"],
}
out.write_text(json.dumps(baseline, indent=2) + "\n")
print(f"seeded {out}: " + ", ".join(f"{k} instructions={v['instructions']}" for k, v in baseline["benchmarks"].items()))
PY
