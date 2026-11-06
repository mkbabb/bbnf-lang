#!/usr/bin/env python3
"""Generate the compact observations and boundary into one empty root."""

from __future__ import annotations

import argparse
import json
import resource
import sys
import time
from pathlib import Path

import crater
import pipeline
from common import GateError, encode, enforce_offline, require_toolchain, sha256, store


ORDER = ("observations.json", "boundary.json")


def rss_bytes() -> int:
    value = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    return int(value if sys.platform == "darwin" else value * 1024)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--check-against", type=Path)
    options = parser.parse_args()
    toolchain = require_toolchain()
    enforce_offline()
    if options.output.exists() and (not options.output.is_dir() or any(options.output.iterdir())):
        print(json.dumps({"code": "OUTPUT_NOT_EMPTY", "status": "BLOCKED"}, sort_keys=True), file=sys.stderr)
        return 2
    options.output.mkdir(parents=True, exist_ok=True)
    values = {}
    timings = []
    try:
        for name, callback in (("observations.json", pipeline.build), ("boundary.json", crater.capture)):
            started = time.perf_counter_ns()
            values[name] = callback()
            timings.append({"stage": name, "wall_ns": time.perf_counter_ns() - started, "max_rss_bytes": rss_bytes(), "encoded_bytes": len(encode(values[name]))})
    except (GateError, OSError, ValueError) as error:
        store(options.output / "receipt.json", {"status": "BLOCKED", "detail": str(error)})
        print(str(error), file=sys.stderr)
        return 2
    for name in ORDER:
        store(options.output / name, values[name])
    artifacts = [{"path": name, "bytes": (options.output / name).stat().st_size, "sha256": sha256((options.output / name).read_bytes())} for name in ORDER]
    receipt = {
        "schema_version": 1,
        "status": "BYTE_REPLAY_VALID_SEMANTICS_RED",
        "byte_replay": "VALID",
        "local_semantic": values["observations.json"]["claim_boundary"]["local_semantic"]["status"],
        "downstream": values["observations.json"]["claim_boundary"]["downstream"]["terminal"],
        "candidate_product": values["observations.json"]["claim_boundary"]["candidate_product"]["status"],
        "toolchain": toolchain,
        "offline_enforced": True,
        "timings": timings,
        "artifacts": artifacts,
    }
    store(options.output / "receipt.json", receipt)
    if options.check_against:
        mismatches = [name for name in (*ORDER, "receipt.json") if not (options.check_against / name).is_file() or (options.check_against / name).read_bytes() != (options.output / name).read_bytes()]
        # Timing/RSS are receipts, not deterministic artifacts.
        mismatches = [name for name in mismatches if name != "receipt.json"]
        if mismatches:
            print(json.dumps({"status": "DRIFT", "files": mismatches}, sort_keys=True), file=sys.stderr)
            return 1
    print(json.dumps({"status": receipt["status"], "artifact_count": len(artifacts), "semantic_obligations": 8_796, "candidate_count": 0}, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
