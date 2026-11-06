#!/usr/bin/env python3
"""Run authority verification, mutations, and empty-root regeneration twice offline."""

from __future__ import annotations

import argparse
import json
import os
import resource
import subprocess
import sys
import time
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SRC = ROOT / "src"
sys.path.insert(0, str(SRC))

from common import encode, require_toolchain, sha256, store
from manifest import verify as verify_manifest
from regen import ORDER


def run(arguments: list[str], environment: dict[str, str]) -> dict[str, object]:
    started = time.perf_counter_ns()
    result = subprocess.run(arguments, cwd=ROOT, env=environment, capture_output=True)
    wall_ns = time.perf_counter_ns() - started
    usage = resource.getrusage(resource.RUSAGE_CHILDREN)
    receipt = {
        "exit_code": result.returncode,
        "wall_ns": wall_ns,
        "children_max_rss_bytes": int(usage.ru_maxrss if sys.platform == "darwin" else usage.ru_maxrss * 1024),
        "stdout_sha256": sha256(result.stdout),
        "stderr_sha256": sha256(result.stderr),
    }
    if result.returncode:
        raise RuntimeError(json.dumps({**receipt, "stdout": result.stdout.decode(errors="replace")[-4000:], "stderr": result.stderr.decode(errors="replace")[-4000:]}, sort_keys=True))
    return receipt


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--run-root", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    options = parser.parse_args()
    require_toolchain()
    subject = verify_manifest()
    if options.run_root.exists() and (not options.run_root.is_dir() or any(options.run_root.iterdir())):
        raise RuntimeError("proof run root must be a distinct empty directory")
    options.run_root.mkdir(parents=True, exist_ok=True)
    if options.output.exists():
        raise RuntimeError("proof output must not already exist")
    environment = {**os.environ, "CSS_EPOCH3_OFFLINE": "1", "GIT_NO_LAZY_FETCH": "1", "GIT_TERMINAL_PROMPT": "0", "PYTHONDONTWRITEBYTECODE": "1"}
    python = str(Path(sys.executable).resolve())

    bootstrap_runs = [run([python, "-B", "src/bootstrap.py"], environment) for _ in range(2)]
    test_runs = [run([python, "-B", "tests/offline_runner.py"], environment) for _ in range(2)]
    regeneration_runs = []
    outputs = [options.run_root / "regen-a", options.run_root / "regen-b"]
    for index, destination in enumerate(outputs):
        arguments = [python, "-B", "src/regen.py", "--output", str(destination)]
        arguments.extend(["--check-against", str(ROOT / "data" if index == 0 else outputs[0])])
        process = run(arguments, environment)
        receipt = json.loads((destination / "receipt.json").read_text())
        regeneration_runs.append({**process, "stage_timings": receipt["timings"], "artifacts": receipt["artifacts"]})

    matches = {name: (outputs[0] / name).read_bytes() == (outputs[1] / name).read_bytes() for name in ORDER}
    if not all(matches.values()):
        raise RuntimeError(f"deterministic artifact mismatch: {matches}")
    first = json.loads((outputs[0] / "receipt.json").read_text())
    proof = {
        "schema_version": 1,
        "status": "PASS",
        "offline_enforced": True,
        "toolchain": first["toolchain"],
        "bootstrap_runs": bootstrap_runs,
        "test_runs": test_runs,
        "regeneration_runs": regeneration_runs,
        "deterministic_artifacts": matches,
        "input_epoch_reproducible": first["input_epoch_reproducible"],
        "semantic_portfolio": first["semantic_portfolio"],
        "root_blocker_count": first["root_blocker_count"],
        "subject_hygiene": {
            "status": "PASS",
            "symlinks": 0,
            "cache_entries": 0,
            "file_count": subject["file_count"],
            "total_bytes": subject["total_bytes"],
            "subject_sha256": subject["subject_sha256"],
        },
    }
    store(options.output, proof)
    print(json.dumps({"status": proof["status"], "input_epoch_reproducible": proof["input_epoch_reproducible"], "semantic_portfolio": proof["semantic_portfolio"], "artifact_count": len(matches)}, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
