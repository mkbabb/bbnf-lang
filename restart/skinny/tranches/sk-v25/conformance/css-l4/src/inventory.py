#!/usr/bin/env python3
"""Capture or verify the frozen W3C CSS Level 4 specification inventory."""

from __future__ import annotations

import argparse
import hashlib
import html
import json
import re
import sys
import time
import urllib.request
from urllib.error import HTTPError
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import urljoin, urlsplit

CURRENT_WORK = "https://www.w3.org/Style/CSS/current-work.en.html"
CSSWG_REVISION = "c7573530343759ace8e46438a1fa2c44515b5554"
SNAPSHOT_EDITOR_SOURCE = (
    "https://raw.githubusercontent.com/w3c/csswg-drafts/"
    f"{CSSWG_REVISION}/css-2026/Overview.bs"
)
FOUNDATIONS = (
    ("CSS Snapshot 2026", "https://www.w3.org/TR/css-2026/"),
    ("CSS Syntax Module Level 3", "https://www.w3.org/TR/css-syntax-3/"),
)
SNAPSHOT_SECTIONS = (
    ("snapshot:official", "2.1", "2.2"),
    ("snapshot:reliable", "2.2", "2.3"),
    ("snapshot:fairly-stable", "2.3", "2.4"),
    ("snapshot:rough-interoperability", "2.4", "2.5"),
    ("snapshot:pre-cr-exception", "4", "5"),
)
BIKESHED_SECTIONS = (
    ("snapshot:official", "css-official", "reliable-cr"),
    ("snapshot:reliable", "reliable-cr", "fairly-stable"),
    ("snapshot:fairly-stable", "fairly-stable", "rough-interop"),
    ("snapshot:rough-interoperability", "rough-interop", "css-levels"),
    ("snapshot:pre-cr-exception", "CR-exceptions", "media-type"),
)
FETCH_CACHE: dict[str, bytes] = {}


def normalized(parts: list[str]) -> str:
    return " ".join("".join(parts).replace("\u00a0", " ").split())


class CurrentWorkParser(HTMLParser):
    def __init__(self) -> None:
        super().__init__(convert_charrefs=True)
        self.group: str | None = None
        self.row: dict[str, object] | None = None
        self.cell: dict[str, object] | None = None
        self.rows: list[dict[str, str]] = []

    def finish_cell(self) -> None:
        if self.cell is None or self.row is None:
            return
        cells = self.row["cells"]
        assert isinstance(cells, list)
        cells.append({"text": normalized(self.cell["text"]), "href": self.cell["href"]})
        self.cell = None

    def finish_row(self) -> None:
        if self.row is None:
            return
        self.finish_cell()
        cells = self.row["cells"]
        assert isinstance(cells, list)
        if len(cells) >= 2 and re.search(r"\bLevel\s+4\b", cells[0]["text"]):
            self.rows.append(
                {
                    "id": str(self.row["id"]),
                    "group": str(self.row["group"]),
                    "title": cells[0]["text"],
                    "latest_url": urljoin(CURRENT_WORK, cells[0]["href"]),
                    "status": cells[1]["text"],
                }
            )
        self.row = None

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        values = dict(attrs)
        if tag == "tbody":
            self.finish_row()
            self.group = values.get("id")
        elif tag == "tr" and values.get("id"):
            self.finish_row()
            self.row = {"id": values["id"], "group": self.group or "ungrouped", "cells": []}
        elif tag == "td" and self.row is not None:
            self.finish_cell()
            self.cell = {"text": [], "href": None}
        elif tag == "a" and self.cell is not None and self.cell["href"] is None:
            self.cell["href"] = values.get("href")

    def handle_data(self, data: str) -> None:
        if self.cell is not None:
            text = self.cell["text"]
            assert isinstance(text, list)
            text.append(data)

    def handle_endtag(self, tag: str) -> None:
        if tag == "td":
            self.finish_cell()
        elif tag == "tr":
            self.finish_row()
        elif tag == "tbody":
            self.finish_row()
            self.group = None


def fetch(url: str) -> bytes:
    cached = FETCH_CACHE.get(url)
    if cached is not None:
        return cached
    request = urllib.request.Request(url, headers={"User-Agent": "bbnf-css-l4-inventory/1"})
    for attempt in range(5):
        try:
            with urllib.request.urlopen(request, timeout=30) as response:
                data = response.read()
                FETCH_CACHE[url] = data
                return data
        except HTTPError as error:
            if error.code != 429 or attempt == 4:
                raise RuntimeError(f"cannot fetch {url}: HTTP {error.code}") from error
            time.sleep(min(2**attempt, 8))
    raise AssertionError("retry loop is exhaustive")


def digest(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def canonical_tr_url(url: str) -> str | None:
    parsed = urlsplit(urljoin("https://www.w3.org", url))
    if parsed.netloc.lower() != "www.w3.org":
        return None
    parts = [part for part in parsed.path.split("/") if part]
    if len(parts) < 2 or parts[0] != "TR" or parts[1].isdigit():
        return None
    return f"https://www.w3.org/TR/{parts[1]}/"


def spec_id(url: str) -> str:
    canonical = canonical_tr_url(url)
    if canonical is None:
        raise ValueError(f"not a latest W3C TR URL: {url}")
    return canonical.rstrip("/").rsplit("/", 1)[-1]


def plain(text: str) -> str:
    return " ".join(html.unescape(re.sub(r"<[^>]+>", "", text)).split())


def section(document: str, start: str, stop: str) -> str:
    start_match = re.search(rf"<h[1-6][^>]*data-level=[\"']{re.escape(start)}[\"']", document)
    stop_match = re.search(rf"<h[1-6][^>]*data-level=[\"']{re.escape(stop)}[\"']", document)
    if start_match is None or stop_match is None or start_match.start() >= stop_match.start():
        raise ValueError(f"CSS Snapshot section {start}…{stop} is absent or inverted")
    return document[start_match.start() : stop_match.start()]


def snapshot_members(document: bytes) -> list[dict[str, str]]:
    text = document.decode("utf-8", errors="replace")
    bibliography: dict[str, tuple[str, str]] = {}
    entries = re.finditer(
        r"<dt\s+id=[\"']biblio-([^\"']+)[\"'][^>]*>.*?<dd>(.*?)(?=<dt\s|</dl>)",
        text,
        flags=re.DOTALL | re.IGNORECASE,
    )
    for entry in entries:
        citation = re.search(
            r"<a\s+href=[\"']([^\"']+)[\"'][^>]*>\s*<cite>(.*?)</cite>",
            entry.group(2),
            flags=re.DOTALL | re.IGNORECASE,
        )
        if citation is None:
            continue
        latest_url = canonical_tr_url(citation.group(1))
        if latest_url is not None:
            bibliography[entry.group(1).lower()] = (plain(citation.group(2)), latest_url)

    members: dict[tuple[str, str], dict[str, str]] = {}
    for scope, start, stop in SNAPSHOT_SECTIONS:
        body = section(text, start, stop)
        for identifier in re.findall(r"href=[\"']#biblio-([^\"']+)", body, re.IGNORECASE):
            citation = bibliography.get(identifier.lower())
            if citation is None:
                raise ValueError(f"CSS Snapshot bibliography entry {identifier} has no W3C TR citation")
            title, latest_url = citation
            members[(scope, latest_url)] = {
                "scope": scope,
                "title": title,
                "latest_url": latest_url,
            }
        for raw_url in re.findall(r"href=[\"'](https://www\.w3\.org/TR/[^\"']+)", body):
            latest_url = canonical_tr_url(raw_url)
            if latest_url is None:
                continue
            title = next(
                (known_title for known_title, known_url in bibliography.values() if known_url == latest_url),
                spec_id(latest_url),
            )
            members[(scope, latest_url)] = {
                "scope": scope,
                "title": title,
                "latest_url": latest_url,
            }
    return list(members.values())


def bikeshed_section(document: str, start: str, stop: str) -> str:
    start_match = re.search(rf"<h[1-6]\s+id=[\"']?{re.escape(start)}[\"']?[^>]*>", document)
    stop_match = re.search(rf"<h[1-6]\s+id=[\"']?{re.escape(stop)}[\"']?[^>]*>", document)
    if start_match is None or stop_match is None or start_match.start() >= stop_match.start():
        raise ValueError(f"CSSWG Snapshot source section {start}…{stop} is absent or inverted")
    return document[start_match.start() : stop_match.start()]


def bikeshed_members(document: bytes) -> list[dict[str, str]]:
    text = document.decode("utf-8", errors="strict")
    members: dict[tuple[str, str], dict[str, str]] = {}
    for scope, start, stop in BIKESHED_SECTIONS:
        body = bikeshed_section(text, start, stop)
        for raw_url in re.findall(r"https://www\.w3\.org/TR/[^\s\"'<>\)]+", body):
            latest_url = canonical_tr_url(html.unescape(raw_url))
            if latest_url is not None:
                members[(scope, latest_url)] = {"scope": scope, "latest_url": latest_url}
        if scope == "snapshot:pre-cr-exception":
            for reference in re.findall(r"\[\[!?([A-Za-z0-9-]+)(?:#[^\]|]+)?", body):
                shortname = reference.lower()
                latest_url = f"https://www.w3.org/TR/{shortname}/"
                members[(scope, latest_url)] = {"scope": scope, "latest_url": latest_url}
    return list(members.values())


def scope_capture(seed_path: Path, wpt_revision: str) -> dict[str, object]:
    seed_bytes = seed_path.read_bytes()
    seed = json.loads(seed_bytes)
    if seed.get("schema_version") != 1:
        raise ValueError("scope seed must be the frozen phase-one schema-1 manifest")
    if seed.get("wpt", {}).get("revision") != wpt_revision:
        raise ValueError("scope WPT revision differs from its phase-one seed")
    source = fetch(SNAPSHOT_EDITOR_SOURCE)
    records: dict[str, set[str]] = {}

    def add(latest_url: str, scope: str) -> None:
        latest_url = canonical_tr_url(latest_url) or latest_url
        records.setdefault(latest_url, set()).add(scope)

    for record in seed["foundations"]:
        add(record["latest_url"], "foundation")
    for record in seed["level_4"]:
        add(record["latest_url"], f"current-work:{record['group']}")
    for member in bikeshed_members(source):
        add(member["latest_url"], member["scope"])

    return {
        "schema_version": 1,
        "selection": "union of pinned CSSWG Snapshot 2026 sections 2.1-2.4 and 4, phase-one current-work `Level 4` rows, CSS Snapshot 2026, and CSS Syntax 3",
        "csswg_source": {
            "repository": "https://github.com/w3c/csswg-drafts.git",
            "revision": CSSWG_REVISION,
            "path": "css-2026/Overview.bs",
            "raw_url": SNAPSHOT_EDITOR_SOURCE,
            "bytes": len(source),
            "sha256": digest(source),
        },
        "current_work": seed["current_work"],
        "phase_one_seed": {
            "artifact": "inventory.json",
            "bytes": len(seed_bytes),
            "sha256": digest(seed_bytes),
        },
        "wpt": {
            "repository": "https://github.com/web-platform-tests/wpt.git",
            "revision": wpt_revision,
        },
        "specs": [
            {"id": spec_id(url), "latest_url": url, "scopes": sorted(scopes)}
            for url, scopes in sorted(records.items(), key=lambda item: spec_id(item[0]).lower())
        ],
    }


def dated_version(latest_url: str, document: bytes) -> str:
    text = document.decode("utf-8", errors="replace")
    marker = text.find("This version:")
    if marker < 0:
        marker = text.find("This Version:")
    if marker < 0:
        raise ValueError(f"{latest_url}: no This version marker")
    match = re.search(r"href\s*=\s*[\"']([^\"']+)", text[marker : marker + 2_000])
    if match is None:
        raise ValueError(f"{latest_url}: no dated version URL")
    return urljoin(latest_url, match.group(1))


def pin(title: str, latest_url: str, scopes: set[str], status: str | None) -> dict[str, object]:
    try:
        latest = fetch(latest_url)
    except Exception as error:
        raise RuntimeError(f"cannot resolve latest specification {latest_url}") from error
    version_url = dated_version(latest_url, latest)
    try:
        version = fetch(version_url)
    except Exception as error:
        raise RuntimeError(f"cannot pin {latest_url} through {version_url}") from error
    record: dict[str, object] = {
        "id": spec_id(latest_url),
        "title": title,
        "scopes": sorted(scopes),
        "latest_url": latest_url,
        "version_url": version_url,
        "bytes": len(version),
        "sha256": digest(version),
    }
    if status is not None:
        record["current_work_status"] = status
    return record


def capture(wpt_revision: str) -> dict[str, object]:
    current_work = fetch(CURRENT_WORK)
    parser = CurrentWorkParser()
    parser.feed(current_work.decode("utf-8", errors="replace"))
    parser.finish_row()
    if not parser.rows:
        raise ValueError("current-work yielded no Level 4 specifications")

    snapshot_latest = fetch(FOUNDATIONS[0][1])
    snapshot_url = dated_version(FOUNDATIONS[0][1], snapshot_latest)
    snapshot = fetch(snapshot_url)
    records: dict[str, dict[str, object]] = {}

    def add(title: str, latest_url: str, scope: str, status: str | None = None) -> None:
        latest_url = canonical_tr_url(latest_url) or latest_url
        record = records.setdefault(
            latest_url,
            {"title": title, "scopes": set(), "status": None},
        )
        scopes = record["scopes"]
        assert isinstance(scopes, set)
        scopes.add(scope)
        if status is not None:
            record["status"] = status

    for title, latest_url in FOUNDATIONS:
        add(title, latest_url, "foundation")
    for row in parser.rows:
        add(row["title"], row["latest_url"], f"current-work:{row['group']}", row["status"])
    for member in snapshot_members(snapshot):
        add(member["title"], member["latest_url"], member["scope"])

    specs = [
        pin(
            str(record["title"]),
            latest_url,
            record["scopes"],
            record["status"] if isinstance(record["status"], str) else None,
        )
        for latest_url, record in sorted(records.items(), key=lambda item: spec_id(item[0]).lower())
    ]
    return {
        "schema_version": 2,
        "selection": "union of CSS Snapshot 2026 sections 2.1-2.4 and 4, every current-work `Level 4` row, CSS Snapshot 2026, and CSS Syntax 3",
        "current_work": {
            "url": CURRENT_WORK,
            "bytes": len(current_work),
            "sha256": digest(current_work),
        },
        "snapshot_source": {
            "latest_url": FOUNDATIONS[0][1],
            "version_url": snapshot_url,
            "bytes": len(snapshot),
            "sha256": digest(snapshot),
        },
        "wpt": {
            "repository": "https://github.com/web-platform-tests/wpt.git",
            "revision": wpt_revision,
        },
        "specs": specs,
    }


def main() -> int:
    arguments = argparse.ArgumentParser()
    arguments.add_argument("--wpt-revision", required=True)
    arguments.add_argument("--scope-from", type=Path)
    arguments.add_argument("--check", type=Path)
    options = arguments.parse_args()
    actual = (
        scope_capture(options.scope_from, options.wpt_revision)
        if options.scope_from is not None
        else capture(options.wpt_revision)
    )
    if options.check is None:
        json.dump(actual, sys.stdout, indent=2, sort_keys=True)
        sys.stdout.write("\n")
        return 0

    expected = json.loads(options.check.read_text())
    if actual == expected:
        label = "scope" if options.scope_from is not None else "dated inventory"
        print(f"CSS L4 {label} verified: {len(actual['specs'])} union specs")
        return 0
    print("CSS L4 inventory drifted; recapture and adjudicate before changing the pin", file=sys.stderr)
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
