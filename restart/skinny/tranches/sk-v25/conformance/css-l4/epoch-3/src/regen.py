#!/usr/bin/env python3
"""Run the epoch-3 stages fail-closed and write compact deterministic receipts."""

from __future__ import annotations

import argparse
import json
import resource
import shutil
import sys
import time
from pathlib import Path
from typing import Any, Callable

import dialects
import boundary
import extract
import matrix
import membership
import modules
from common import DEFAULT_CSSWG, DEFAULT_WPT, GateError, ROOT, blockers, encode, enforce_offline, require_toolchain, sha256, store


ORDER = ("scope.json", "carriers.json", "dialects.json", "modules.json", "matrix.json", "boundary.json")


def _rss_bytes() -> int:
    value = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    return int(value if sys.platform == "darwin" else value * 1024)


def build(csswg: Path, wpt: Path, repository: Path) -> tuple[dict[str, dict[str, Any]], dict[str, Any]]:
    outputs: dict[str, dict[str, Any]] = {}
    timings: list[dict[str, Any]] = []

    def run(name: str, callback: Callable[[], dict[str, Any]]) -> dict[str, Any]:
        started = time.perf_counter_ns()
        value = callback()
        timings.append({"stage": name, "wall_ns": time.perf_counter_ns() - started, "max_rss_bytes": _rss_bytes(), "encoded_bytes": len(encode(value))})
        outputs[name] = value
        return value

    scoped = run("scope.json", lambda: membership.capture(csswg, wpt))
    carriers = run("carriers.json", lambda: extract.capture(scoped, csswg))
    parsed = run("dialects.json", lambda: dialects.capture(carriers))
    closed = run("modules.json", lambda: modules.capture(scoped, carriers, parsed))
    run("matrix.json", lambda: matrix.capture(scoped, carriers, closed, repository, csswg))
    run("boundary.json", boundary.capture)
    open_items = [{"stage": name, **row} for name in ORDER for row in blockers(outputs[name])]
    reproducible = not open_items
    return outputs, {
        "status": "INPUT_EPOCH_REPRODUCIBLE" if reproducible else "INPUT_EPOCH_BLOCKED",
        "input_epoch_reproducible": reproducible,
        "semantic_portfolio": outputs["matrix.json"]["semantic_portfolio"],
        "root_blocker_count": len(open_items),
        "root_blockers": open_items,
        "timings": timings,
    }


def main() -> int:
    toolchain = require_toolchain()
    enforce_offline()
    parser = argparse.ArgumentParser()
    parser.add_argument("--csswg-root", type=Path, default=DEFAULT_CSSWG)
    parser.add_argument("--wpt-root", type=Path, default=DEFAULT_WPT)
    parser.add_argument("--repository-root", type=Path, default=ROOT.parents[6])
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--check-against", type=Path)
    parser.add_argument("--allow-blocked", action="store_true")
    options = parser.parse_args()
    if options.output.exists() and (not options.output.is_dir() or any(options.output.iterdir())):
        print(json.dumps({"status": "BLOCKED_INPUT", "code": "OUTPUT_NOT_EMPTY", "output": str(options.output)}, sort_keys=True), file=sys.stderr)
        return 2
    options.output.mkdir(parents=True, exist_ok=True)
    try:
        outputs, summary = build(options.csswg_root, options.wpt_root, options.repository_root)
    except (GateError, OSError, ValueError) as error:
        summary = {"status": "BLOCKED_INPUT", "detail": str(error)}
        store(options.output / "receipt.json", summary)
        print(json.dumps(summary, sort_keys=True), file=sys.stderr)
        return 2
    for name in ORDER:
        store(options.output / name, outputs[name])
    summary["toolchain"] = toolchain
    summary["offline_enforced"] = True
    summary["artifacts"] = [{"path": name, "bytes": (options.output / name).stat().st_size, "sha256": sha256((options.output / name).read_bytes())} for name in ORDER]
    store(options.output / "receipt.json", summary)
    if options.check_against:
        mismatches = [name for name in ORDER if not (options.check_against / name).is_file() or (options.check_against / name).read_bytes() != (options.output / name).read_bytes()]
        if mismatches:
            print(json.dumps({"status": "DRIFT", "files": mismatches}, sort_keys=True), file=sys.stderr)
            return 1
    print(json.dumps({key: summary[key] for key in ("status", "input_epoch_reproducible", "semantic_portfolio", "root_blocker_count", "artifacts")}, sort_keys=True))
    return 0 if summary["input_epoch_reproducible"] or options.allow_blocked else 2


if __name__ == "__main__":
    raise SystemExit(main())
