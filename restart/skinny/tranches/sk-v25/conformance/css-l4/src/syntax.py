#!/usr/bin/env python3
"""Census normative syntax carriers in the frozen CSSWG source corpus."""

from __future__ import annotations

import argparse
import hashlib
import html
import json
import re
import subprocess
import sys
from collections import Counter
from pathlib import Path

TAGS = "pre|table|span|div|xmp|code"
OPEN = re.compile(rf"<(?P<tag>{TAGS})\b(?P<attrs>[^>]*)>", re.IGNORECASE)
CLASS = re.compile(
    r"\bclass\s*=\s*(?:\"(?P<double>[^\"]*)\"|'(?P<single>[^']*)'|(?P<bare>[^\s>]+))",
    re.IGNORECASE,
)
KINDS = ("propdef", "descdef", "prod", "railroad", "idl")
COMMENT = re.compile(r"<!--.*?-->", re.DOTALL)
MARKUP = re.compile(r"(?<!<)</?[A-Za-z][^>]*>|<![A-Za-z][^>]*>", re.DOTALL)


def digest(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def git_head(root: Path) -> str:
    result = subprocess.run(
        ["git", "-C", str(root), "rev-parse", "HEAD"],
        check=True,
        capture_output=True,
        text=True,
    )
    return result.stdout.strip()


def class_tokens(attributes: str) -> set[str]:
    match = CLASS.search(attributes)
    if match is None:
        return set()
    value = next(group for group in match.groups() if group is not None)
    return {token.lower() for token in value.split()}


def carrier_kind(attributes: str) -> str | None:
    tokens = class_tokens(attributes)
    return next((kind for kind in KINDS if kind in tokens), None)


def closing(document: str, tag: str, start: int) -> tuple[int, int]:
    token = re.compile(rf"</?{re.escape(tag)}\b[^>]*>", re.IGNORECASE)
    depth = 1
    for match in token.finditer(document, start):
        value = match.group(0)
        if value.startswith("</"):
            depth -= 1
            if depth == 0:
                return match.start(), match.end()
        elif not value.rstrip().endswith("/>"):
            depth += 1
    raise ValueError(f"unclosed <{tag}> syntax carrier")


def normalized(body: str) -> str:
    text = COMMENT.sub(" ", body)
    text = MARKUP.sub(" ", text)
    return " ".join(html.unescape(text).split())


def carriers(document: str) -> list[dict[str, object]]:
    records: list[dict[str, object]] = []
    ordinals: Counter[str] = Counter()
    cursor = 0
    while match := OPEN.search(document, cursor):
        kind = carrier_kind(match.group("attrs"))
        if kind is None:
            cursor = match.end()
            continue
        tag = match.group("tag").lower()
        body_end, raw_end = closing(document, tag, match.end())
        raw = document[match.start() : raw_end]
        body = document[match.end() : body_end]
        ordinals[kind] += 1
        text = normalized(body)
        records.append(
            {
                "kind": kind,
                "ordinal": ordinals[kind],
                "tag": tag,
                "line": document.count("\n", 0, match.start()) + 1,
                "raw_bytes": len(raw.encode("utf-8")),
                "raw_sha256": digest(raw.encode("utf-8")),
                "text": text,
                "text_sha256": digest(text.encode("utf-8")),
            }
        )
        cursor = raw_end
    return records


def capture(manifest_path: Path, root: Path) -> dict[str, object]:
    manifest_bytes = manifest_path.read_bytes()
    manifest = json.loads(manifest_bytes)
    revision = manifest["csswg"]["revision"]
    if git_head(root) != revision:
        raise ValueError("CSSWG checkout differs from the source manifest revision")

    records: list[dict[str, object]] = []
    for spec in manifest["specs"]:
        data = (root / spec["path"]).read_bytes()
        if len(data) != spec["bytes"] or digest(data) != spec["sha256"]:
            raise ValueError(f"CSSWG source drifted: {spec['path']}")
        for record in carriers(data.decode("utf-8", errors="strict")):
            record["spec_id"] = spec["id"]
            record["path"] = spec["path"]
            records.append(record)

    counts = Counter(str(record["kind"]) for record in records)
    return {
        "schema_version": 1,
        "source_manifest": {
            "artifact": manifest_path.name,
            "bytes": len(manifest_bytes),
            "sha256": digest(manifest_bytes),
        },
        "csswg_revision": revision,
        "counts": {kind: counts[kind] for kind in KINDS},
        "records": records,
    }


def main() -> int:
    arguments = argparse.ArgumentParser()
    arguments.add_argument("--manifest", required=True, type=Path)
    arguments.add_argument("--csswg-root", required=True, type=Path)
    arguments.add_argument("--check", type=Path)
    options = arguments.parse_args()
    actual = capture(options.manifest, options.csswg_root)
    if options.check is None:
        json.dump(actual, sys.stdout, indent=2, sort_keys=True)
        sys.stdout.write("\n")
        return 0
    if actual == json.loads(options.check.read_text()):
        print(f"CSSWG syntax census verified: {len(actual['records'])} carriers")
        return 0
    print("CSSWG syntax census drifted; begin a new input epoch", file=sys.stderr)
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
