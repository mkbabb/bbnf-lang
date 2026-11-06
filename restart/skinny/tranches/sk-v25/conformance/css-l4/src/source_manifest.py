#!/usr/bin/env python3
"""Pin every CSS scope member to its file at the frozen CSSWG revision."""

from __future__ import annotations

import argparse
import hashlib
import json
import subprocess
import sys
from pathlib import Path, PurePosixPath

OVERRIDES = {
    "css-font-loading": "css-font-loading-3/Overview.bs",
    "css-fonts-3": "css-fonts-3/Overview.html",
    "css-namespaces": "css-namespaces-3/Overview.bs",
    "css-style-attr": "css-style-attr-1/Overview.src.html",
    "css3-mediaqueries": "mediaqueries-3/Overview.html",
    "selectors-3": "selectors-3/Overview.src.html",
}


def digest(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def source_path(spec_id: str) -> PurePosixPath:
    path = PurePosixPath(OVERRIDES.get(spec_id, f"{spec_id}/Overview.bs"))
    if path.is_absolute() or ".." in path.parts:
        raise ValueError(f"unsafe CSSWG source path for {spec_id}: {path}")
    return path


def git_head(root: Path) -> str:
    result = subprocess.run(
        ["git", "-C", str(root), "rev-parse", "HEAD"],
        check=True,
        capture_output=True,
        text=True,
    )
    return result.stdout.strip()


def capture(scope_path: Path, root: Path) -> dict[str, object]:
    scope_bytes = scope_path.read_bytes()
    scope = json.loads(scope_bytes)
    csswg = scope["csswg_source"]
    revision = csswg["revision"]
    if git_head(root) != revision:
        raise ValueError("CSSWG checkout differs from the scope revision")

    entries: list[dict[str, object]] = []
    seen: set[str] = set()
    for spec in scope["specs"]:
        identifier = spec["id"]
        if identifier in seen:
            raise ValueError(f"duplicate scope identifier: {identifier}")
        seen.add(identifier)
        path = source_path(identifier)
        data = (root / path).read_bytes()
        entries.append(
            {
                "id": identifier,
                "latest_url": spec["latest_url"],
                "path": str(path),
                "bytes": len(data),
                "sha256": digest(data),
            }
        )

    return {
        "schema_version": 1,
        "scope": {
            "artifact": scope_path.name,
            "bytes": len(scope_bytes),
            "sha256": digest(scope_bytes),
        },
        "csswg": {
            "repository": csswg["repository"],
            "revision": revision,
        },
        "specs": entries,
    }


def main() -> int:
    arguments = argparse.ArgumentParser()
    arguments.add_argument("--scope", required=True, type=Path)
    arguments.add_argument("--csswg-root", required=True, type=Path)
    arguments.add_argument("--check", type=Path)
    options = arguments.parse_args()
    actual = capture(options.scope, options.csswg_root)
    if options.check is None:
        json.dump(actual, sys.stdout, indent=2, sort_keys=True)
        sys.stdout.write("\n")
        return 0
    if actual == json.loads(options.check.read_text()):
        print(f"CSSWG source manifest verified: {len(actual['specs'])} union specs")
        return 0
    print("CSSWG source manifest drifted; begin a new input epoch", file=sys.stderr)
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
