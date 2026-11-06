#!/usr/bin/env python3
"""One-shot polite fetcher for missing authoritative dated-publication bytes."""

from __future__ import annotations

import argparse
import html
import json
import re
import sys
import time
import urllib.request
from pathlib import Path
from urllib.error import HTTPError, URLError
from urllib.parse import urljoin

from common import EPOCH1, ROOT, read_json, sha256, write_json


USER_AGENT = "bbnf-css-l4-epoch-2/1 (+https://github.com/mkbabb/bbnf-lang)"


def once(url: str) -> bytes:
    request = urllib.request.Request(url, headers={"User-Agent": USER_AGENT})
    with urllib.request.urlopen(request, timeout=45) as response:
        return response.read()


def dated_url(latest_url: str, data: bytes) -> str:
    text = data.decode("utf-8", errors="replace")
    marker = re.search(r"This\s+[Vv]ersion\s*:", text)
    if marker is None:
        raise ValueError("no This Version marker")
    match = re.search(r"href\s*=\s*['\"]([^'\"]+)", text[marker.start() : marker.start() + 3000], re.I)
    if match is None:
        raise ValueError("no dated publication link")
    return urljoin(latest_url, html.unescape(match.group(1)))


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--scope", type=Path, required=True)
    parser.add_argument("--output", type=Path, default=ROOT / "inputs" / "publications.json")
    parser.add_argument("--interval", type=float, default=0.75)
    options = parser.parse_args()
    scope = read_json(options.scope)
    old = read_json(EPOCH1 / "data" / "inventory.json")
    existing = {record["latest_url"].rstrip("/") for record in old["foundations"] + old["level_4"]}
    publications = []
    failures = []
    requested = 0
    for spec in scope["specs"]:
        latest_url = spec["latest_url"]
        if latest_url.rstrip("/") in existing:
            continue
        try:
            latest = once(latest_url)
            requested += 1
            version_url = dated_url(latest_url, latest)
            time.sleep(options.interval)
            version = once(version_url)
            requested += 1
            publications.append(
                {
                    "id": spec["id"],
                    "latest_url": latest_url,
                    "version_url": version_url,
                    "bytes": len(version),
                    "sha256": sha256(version),
                    "terminal_state": "PINNED",
                }
            )
        except (HTTPError, URLError, TimeoutError, ValueError) as error:
            failures.append(
                {
                    "spec_id": spec["id"],
                    "latest_url": latest_url,
                    "error_type": type(error).__name__,
                    "detail": str(error),
                    "terminal_state": "BLOCKED_INPUT",
                }
            )
            break
        time.sleep(options.interval)
    result = {
        "schema_version": 1,
        "fetch_policy": "ONE_REQUEST_PER_URL_NO_RETRY_STOP_ON_FIRST_FAILURE",
        "requests_made": requested,
        "publications": publications,
        "failures": failures,
    }
    write_json(options.output, result)
    print(json.dumps({"pinned": len(publications), "failures": failures, "requests": requested}, sort_keys=True))
    return 2 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())

