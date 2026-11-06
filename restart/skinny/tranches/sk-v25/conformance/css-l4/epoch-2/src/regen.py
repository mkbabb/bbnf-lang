#!/usr/bin/env python3
"""Regenerate or verify the four deterministic epoch-2 stage artifacts."""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

import extract
import grammar
import matrix
import scope
from common import DEFAULT_CSSWG, DEFAULT_WPT, EpochError, ROOT, encoded_json, write_json


FILES = ("scope.json", "carriers.json", "grammar.json", "matrix.json")


def build(csswg: Path, wpt: Path, publications: Path | None) -> dict[str, dict]:
    scoped = scope.capture(csswg, wpt, publications)
    carriers = extract.capture(scoped, csswg)
    grammars = grammar.capture(carriers, scoped)
    matrices = matrix.capture(scoped, carriers, grammars)
    return {
        "scope.json": scoped,
        "carriers.json": carriers,
        "grammar.json": grammars,
        "matrix.json": matrices,
    }


def blockers(outputs: dict[str, dict]) -> list[dict]:
    result = []
    for name in FILES:
        for blocker in outputs[name].get("blockers", []):
            result.append({"stage": name, **blocker})
    return result


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--csswg-root", type=Path, default=DEFAULT_CSSWG)
    parser.add_argument("--wpt-root", type=Path, default=DEFAULT_WPT)
    parser.add_argument("--publications", type=Path, default=ROOT / "inputs" / "publications.json")
    parser.add_argument("--output", type=Path, default=ROOT / "data")
    parser.add_argument("--check", action="store_true")
    parser.add_argument("--allow-blocked", action="store_true")
    options = parser.parse_args()
    try:
        outputs = build(options.csswg_root, options.wpt_root, options.publications)
    except (EpochError, OSError, ValueError) as error:
        print(json.dumps({"status": "BLOCKED_INPUT", "detail": str(error)}, sort_keys=True), file=sys.stderr)
        return 2
    if options.check:
        mismatches = []
        for name, value in outputs.items():
            path = options.output / name
            if not path.exists() or path.read_bytes() != encoded_json(value):
                mismatches.append(name)
        if mismatches:
            print(json.dumps({"status": "DRIFT", "files": mismatches}, sort_keys=True), file=sys.stderr)
            return 1
    else:
        for name, value in outputs.items():
            write_json(options.output / name, value)
    open_blockers = blockers(outputs)
    summary = {
        "status": "BLOCKED" if open_blockers else "CLOSED",
        "artifacts": list(FILES),
        "blocker_count": len(open_blockers),
    }
    print(json.dumps(summary, sort_keys=True))
    return 0 if options.allow_blocked or not open_blockers else 2


if __name__ == "__main__":
    raise SystemExit(main())

