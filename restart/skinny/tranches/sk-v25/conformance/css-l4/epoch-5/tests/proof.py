#!/usr/bin/env python3
"""Two independent two-root replay pairs: four complete empty-root runs."""

from __future__ import annotations

import argparse
import json
import subprocess
import sys
from pathlib import Path

from common import ROOT, encode, load, load_contract, sha256, store
from generate import ORDER


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--run-root", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--skip-stored", action="store_true")
    options = parser.parse_args()
    if options.run_root.exists() and (not options.run_root.is_dir() or any(options.run_root.iterdir())):
        print(json.dumps({"code": "RUN_ROOT_NOT_EMPTY"}, sort_keys=True), file=sys.stderr)
        return 2
    options.run_root.mkdir(parents=True, exist_ok=True)
    contract = load_contract()["python"]
    launcher = ROOT / "src" / "launcher.py"
    run_names = ("pair-1-a", "pair-1-b", "pair-2-a", "pair-2-b")
    receipts = []
    for name in run_names:
        output = options.run_root / name
        command = [sys.executable, "-I", "-B", "-X", "utf8", "-X", "warn_default_encoding", str(launcher), "generate", "--output", str(output)]
        result = subprocess.run(command, capture_output=True, env=contract["environment"])
        if result.returncode:
            print(result.stderr.decode("utf-8", errors="replace"), file=sys.stderr)
            return result.returncode
        receipt = load(output / "generation-receipt.json")
        mutation = load(output / "mutation-results.json")
        receipts.append({"run": name, "stdout": result.stdout.decode("utf-8", errors="strict").strip(), "artifact_files": len(ORDER), "artifact_bytes": sum((output / item).stat().st_size for item in ORDER), "unit_digest": receipt["unit_digest"], "counterexamples": mutation["executed_count"], "result_digest": mutation["result_digest"]})
    baseline = options.run_root / run_names[0]
    mismatches = []
    for name in run_names[1:]:
        candidate = options.run_root / name
        for item in ORDER:
            if (baseline / item).read_bytes() != (candidate / item).read_bytes():
                mismatches.append([name, item])
    stored_mismatches = []
    if not options.skip_stored:
        for item in ORDER:
            if not (ROOT / "data" / item).is_file() or (baseline / item).read_bytes() != (ROOT / "data" / item).read_bytes():
                stored_mismatches.append(item)
    status = "PASS_TWO_COMPLETE_TWO_ROOT_REPLAY_PAIRS" if not mismatches and not stored_mismatches else "FAIL"
    proof = {
        "schema_version": 1,
        "status": status,
        "pair_count": 2,
        "run_count": len(run_names),
        "empty_root_count": len(run_names),
        "byte_mismatches": mismatches,
        "stored_mismatches": stored_mismatches,
        "baseline_subject_sha256": sha256(b"".join(item.encode() + b"\0" + (baseline / item).read_bytes() for item in ORDER)),
        "runs": receipts,
    }
    store(options.output, proof)
    print(json.dumps({key: proof[key] for key in ("status", "pair_count", "run_count", "baseline_subject_sha256")}, sort_keys=True))
    return 0 if status.startswith("PASS") else 1


if __name__ == "__main__":
    raise SystemExit(main())
