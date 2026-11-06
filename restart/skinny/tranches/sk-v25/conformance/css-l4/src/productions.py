#!/usr/bin/env python3
"""Split CSS production carriers into named definitions without parsing RHS syntax."""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import sys
from collections import Counter
from pathlib import Path

BOUNDARY = re.compile(
    r"(?<![\w<])(?P<lhs><<[^<>]+>>|<[^<>]+>|@?[-*A-Za-z][\w-]*(?:\s*\(\))?)\s*=\s*"
)


def digest(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def definitions(text: str) -> list[dict[str, str]]:
    matches = list(BOUNDARY.finditer(text))
    result: list[dict[str, str]] = []
    for index, match in enumerate(matches):
        end = matches[index + 1].start() if index + 1 < len(matches) else len(text)
        result.append({"lhs": " ".join(match.group("lhs").split()), "rhs": text[match.end() : end].strip()})
    return result


def capture(census_path: Path) -> dict[str, object]:
    census_bytes = census_path.read_bytes()
    census = json.loads(census_bytes)
    carriers: list[dict[str, object]] = []
    counts: Counter[str] = Counter()
    definition_count = 0
    for source in census["records"]:
        if source["kind"] != "prod":
            continue
        parsed = definitions(source["text"])
        definition_count += len(parsed)
        if parsed and all(item["rhs"] for item in parsed):
            disposition = "split"
        elif parsed:
            disposition = "empty-rhs"
        elif "=" in source["text"]:
            disposition = "unrecognized-equals"
        else:
            disposition = "expression-only"
        counts[disposition] += 1
        carriers.append(
            {
                "spec_id": source["spec_id"],
                "path": source["path"],
                "line": source["line"],
                "ordinal": source["ordinal"],
                "source_sha256": source["raw_sha256"],
                "disposition": disposition,
                "definitions": parsed,
                "text": source["text"],
            }
        )
    return {
        "schema_version": 1,
        "syntax_census": {
            "artifact": census_path.name,
            "bytes": len(census_bytes),
            "sha256": digest(census_bytes),
        },
        "counts": dict(sorted(counts.items())),
        "definition_count": definition_count,
        "carriers": carriers,
    }


def main() -> int:
    arguments = argparse.ArgumentParser()
    arguments.add_argument("--census", required=True, type=Path)
    arguments.add_argument("--check", type=Path)
    options = arguments.parse_args()
    actual = capture(options.census)
    if options.check is None:
        json.dump(actual, sys.stdout, indent=2, sort_keys=True)
        sys.stdout.write("\n")
        return 0
    if actual == json.loads(options.check.read_text()):
        print(
            f"CSS production split verified: {actual['definition_count']} definitions "
            f"from {len(actual['carriers'])} carriers"
        )
        return 0
    print("CSS production split drifted; begin a new input epoch", file=sys.stderr)
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
