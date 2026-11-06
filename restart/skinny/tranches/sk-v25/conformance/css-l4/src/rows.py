#!/usr/bin/env python3
"""Extract structured property and descriptor rows from the frozen census."""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import sys
from collections import Counter
from pathlib import Path

LABELS = (
    "Logical property group",
    "Animation type",
    "Computed value",
    "Canonical order",
    "New values",
    "Applies to",
    "Percentages",
    "Used value",
    "Animatable",
    "Inherited",
    "Initial",
    "Media",
    "For",
    "Type",
    "On",
    "Name",
    "Value",
)
FIELD = re.compile(
    rf"(?:^| )(?P<label>{'|'.join(re.escape(label) for label in LABELS)}):",
    re.IGNORECASE,
)


def digest(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def key(label: str) -> str:
    return label.lower().replace(" ", "_")


def fields(text: str) -> tuple[str, dict[str, str], list[str]]:
    matches = list(FIELD.finditer(text))
    prefix = text[: matches[0].start()].strip() if matches else text
    parsed: dict[str, str] = {}
    duplicates: list[str] = []
    for index, match in enumerate(matches):
        name = key(match.group("label"))
        end = matches[index + 1].start() if index + 1 < len(matches) else len(text)
        value = text[match.end() : end].strip()
        if name in parsed:
            if name not in duplicates:
                duplicates.append(name)
            parsed[name] = " ".join(part for part in (parsed[name], value) if part)
        else:
            parsed[name] = value
    return prefix, parsed, duplicates


def capture(census_path: Path) -> dict[str, object]:
    census_bytes = census_path.read_bytes()
    census = json.loads(census_bytes)
    rows: list[dict[str, object]] = []
    dispositions: Counter[str] = Counter()
    for source in census["records"]:
        if source["kind"] not in {"propdef", "descdef"}:
            continue
        prefix, parsed, duplicates = fields(source["text"])
        has_value = bool(parsed.get("value"))
        has_extension = bool(parsed.get("new_values"))
        if parsed.get("name") and has_value and not prefix:
            disposition = "complete"
        elif parsed.get("name") and has_extension and not has_value and not prefix:
            disposition = "partial-extension"
        else:
            disposition = "anomalous"
        dispositions[disposition] += 1
        rows.append(
            {
                "spec_id": source["spec_id"],
                "path": source["path"],
                "line": source["line"],
                "kind": source["kind"],
                "ordinal": source["ordinal"],
                "source_sha256": source["raw_sha256"],
                "disposition": disposition,
                "prefix": prefix,
                "duplicates": duplicates,
                "fields": parsed,
            }
        )
    return {
        "schema_version": 1,
        "syntax_census": {
            "artifact": census_path.name,
            "bytes": len(census_bytes),
            "sha256": digest(census_bytes),
        },
        "counts": dict(sorted(dispositions.items())),
        "rows": rows,
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
        print(f"CSS definition rows verified: {len(actual['rows'])} rows")
        return 0
    print("CSS definition rows drifted; begin a new input epoch", file=sys.stderr)
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
