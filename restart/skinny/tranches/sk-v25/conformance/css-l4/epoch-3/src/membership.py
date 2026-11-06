"""Seal Snapshot/Current-Work membership, source dependencies, publications, and WPT."""

from __future__ import annotations

import html
import re
import subprocess
from collections import defaultdict
from html.parser import HTMLParser
from pathlib import Path
from typing import Any
from urllib.parse import urljoin, urlsplit

from common import (
    CSSWG_REVISION,
    INPUTS,
    WPT_PATH_OID_SHA256,
    WPT_REVISION,
    WPT_TREE,
    fail,
    git,
    load,
    object_bytes,
    require_checkout,
    sha256,
)
from scanner import Element, Token, decoded_map, occurrences, scan


SNAPSHOT_SECTIONS = (
    ("SNAPSHOT_OFFICIAL", "2.1", "2.2"),
    ("SNAPSHOT_RELIABLE", "2.2", "2.3"),
    ("SNAPSHOT_FAIRLY_STABLE", "2.3", "2.4"),
    ("SNAPSHOT_ROUGH_INTEROP", "2.4", "2.5"),
    ("SNAPSHOT_PRE_CR_EXCEPTION", "4", "5"),
)
SOURCE_CHOICES = ("Overview.bs", "Overview.html", "Overview.src.html")
METADATA = re.compile(rb"<pre\s+class\s*=\s*['\"]?metadata['\"]?[^>]*>(.*?)</pre>", re.I | re.S)
DATED_TR = re.compile(r"^https://www\.w3\.org/TR/\d{4}/[A-Z]+[A-Za-z0-9-]*-\d{8}/$")
COMMENT_WPT = re.compile(rb"<wpt\b", re.I)


def canonical_tr(url: str) -> str | None:
    parsed = urlsplit(urljoin("https://www.w3.org", url))
    parts = [part for part in parsed.path.split("/") if part]
    if parsed.netloc.lower() != "www.w3.org" or len(parts) < 2 or parts[0] != "TR" or parts[1].isdigit():
        return None
    return f"https://www.w3.org/TR/{parts[1]}/"


def spec_id(url: str) -> str:
    value = canonical_tr(url)
    if value is None:
        fail("TR_URL_INVALID", "membership citation is not a canonical latest W3C TR URL", url=url)
    return value.rstrip("/").rsplit("/", 1)[-1]


def _plain(text: str) -> str:
    return " ".join(html.unescape(re.sub(r"<[^>]+>", "", text)).split())


def _section(document: str, start: str, stop: str) -> str:
    first = re.search(rf"<h[1-6][^>]*data-level=['\"]{re.escape(start)}['\"]", document, re.I)
    last = re.search(rf"<h[1-6][^>]*data-level=['\"]{re.escape(stop)}['\"]", document, re.I)
    if first is None or last is None or first.start() >= last.start():
        fail("SNAPSHOT_SECTION_MISSING", "dated Snapshot section boundary is absent", start=start, stop=stop)
    return document[first.start() : last.start()]


def snapshot_members(data: bytes) -> list[dict[str, str]]:
    document = data.decode("utf-8")
    bibliography: dict[str, tuple[str, str]] = {}
    for entry in re.finditer(
        r"<dt\s+id=['\"]biblio-([^'\"]+)['\"][^>]*>.*?<dd>(.*?)(?=<dt\s|</dl>)",
        document,
        re.I | re.S,
    ):
        citation = re.search(r"<a\s+href=['\"]([^'\"]+)['\"][^>]*>\s*<cite>(.*?)</cite>", entry.group(2), re.I | re.S)
        if citation is None:
            continue
        latest = canonical_tr(citation.group(1))
        if latest:
            bibliography[entry.group(1).lower()] = (_plain(citation.group(2)), latest)
    found: dict[tuple[str, str], dict[str, str]] = {}
    for scope, start, stop in SNAPSHOT_SECTIONS:
        body = _section(document, start, stop)
        for key in re.findall(r"href=['\"]#biblio-([^'\"]+)", body, re.I):
            citation = bibliography.get(key.lower())
            if citation is None:
                fail("SNAPSHOT_CITATION_UNRESOLVED", "Snapshot membership citation has no W3C TR bibliography target", key=key)
            title, latest = citation
            found[(scope, latest)] = {"scope": scope, "title": title, "latest_url": latest}
        for raw in re.findall(r"href=['\"](https://www\.w3\.org/TR/[^'\"]+)", body, re.I):
            latest = canonical_tr(raw)
            if latest:
                found.setdefault((scope, latest), {"scope": scope, "title": spec_id(latest), "latest_url": latest})
    return sorted(found.values(), key=lambda row: (row["latest_url"], row["scope"]))


class CurrentWork(HTMLParser):
    def __init__(self) -> None:
        super().__init__(convert_charrefs=True)
        self.group: str | None = None
        self.row: dict[str, Any] | None = None
        self.cell: dict[str, Any] | None = None
        self.rows: list[dict[str, str]] = []

    def _finish_cell(self) -> None:
        if self.cell is not None and self.row is not None:
            self.row["cells"].append({"text": " ".join("".join(self.cell["text"]).split()), "href": self.cell["href"]})
        self.cell = None

    def _finish_row(self) -> None:
        if self.row is None:
            return
        self._finish_cell()
        cells = self.row["cells"]
        if len(cells) >= 2 and re.search(r"\bLevel\s+4\b", cells[0]["text"]):
            latest = canonical_tr(urljoin("https://www.w3.org/Style/CSS/current-work.en.html", cells[0]["href"] or ""))
            if latest is None:
                fail("CURRENT_WORK_URL_INVALID", "Level-4 Current Work row lacks a canonical W3C TR link", row=self.row["id"])
            self.rows.append(
                {"row_id": self.row["id"], "group": self.group or "ungrouped", "title": cells[0]["text"], "status": cells[1]["text"], "latest_url": latest}
            )
        self.row = None

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        values = dict(attrs)
        if tag == "tbody":
            self._finish_row()
            self.group = values.get("id")
        elif tag == "tr" and values.get("id"):
            self._finish_row()
            self.row = {"id": values["id"], "cells": []}
        elif tag in {"td", "th"} and self.row is not None:
            self._finish_cell()
            self.cell = {"text": [], "href": None}
        elif tag == "a" and self.cell is not None and self.cell["href"] is None:
            self.cell["href"] = values.get("href")

    def handle_data(self, data: str) -> None:
        if self.cell is not None:
            self.cell["text"].append(data)

    def handle_endtag(self, tag: str) -> None:
        if tag in {"td", "th"}:
            self._finish_cell()
        elif tag == "tr":
            self._finish_row()
        elif tag == "tbody":
            self._finish_row()
            self.group = None


def current_work_members(data: bytes) -> list[dict[str, str]]:
    parser = CurrentWork()
    parser.feed(data.decode("utf-8"))
    parser.close()
    parser._finish_row()
    return sorted(parser.rows, key=lambda row: row["row_id"])


def metadata(data: bytes) -> dict[str, str]:
    found = METADATA.search(data)
    if found is None:
        return {}
    result: dict[str, str] = {}
    for line in found.group(1).decode("utf-8", errors="replace").splitlines():
        if ":" not in line:
            continue
        key, value = line.split(":", 1)
        result.setdefault("_".join(key.lower().split()), " ".join(value.split()))
    return result


def source_index(csswg: Path) -> tuple[dict[str, Path], dict[str, dict[str, str]]]:
    index: dict[str, Path] = {}
    descriptions: dict[str, dict[str, str]] = {}
    for directory in sorted((item for item in csswg.iterdir() if item.is_dir()), key=lambda path: path.name.lower()):
        path = next((directory / name for name in SOURCE_CHOICES if (directory / name).is_file()), None)
        if path is None:
            continue
        info = metadata(path.read_bytes())
        descriptions[directory.name] = info
        keys = {directory.name.upper()}
        short = info.get("shortname")
        level = info.get("level")
        if short:
            keys.add(short.upper())
            if level:
                keys.add(f"{short}-{level}".upper())
        tr = canonical_tr(info.get("tr", ""))
        if tr:
            keys.add(spec_id(tr).upper())
        for key in keys:
            index.setdefault(key, path)
    return index, descriptions


def _path_record(identifier: str, latest: str, scopes: set[str], path: Path, csswg: Path) -> dict[str, Any]:
    data = path.read_bytes()
    info = metadata(data)
    return {
        "id": identifier,
        "latest_url": latest,
        "scopes": sorted(scopes),
        "path": path.relative_to(csswg).as_posix(),
        "bytes": len(data),
        "sha256": sha256(data),
        "metadata": {key: info[key] for key in ("title", "status", "shortname", "level", "tr", "wpt_path_prefix") if key in info},
    }


def _wpt_tree(wpt: Path) -> tuple[dict[str, str], str]:
    require_checkout(wpt, WPT_REVISION, "WPT")
    tree = git(wpt, "rev-parse", "HEAD^{tree}").strip()
    if tree != WPT_TREE:
        fail("WPT_TREE_MISMATCH", "WPT commit resolves to an unexpected tree", expected=WPT_TREE, actual=tree)
    try:
        raw = subprocess.run(
            ["git", "-C", str(wpt), "ls-tree", "-rz", "--full-tree", "HEAD"],
            check=True,
            capture_output=True,
            env={"GIT_NO_LAZY_FETCH": "1", "GIT_TERMINAL_PROMPT": "0"},
        ).stdout
    except subprocess.CalledProcessError as error:
        fail("WPT_TREE_UNREADABLE", "cannot reproduce WPT tree", detail=error.stderr.decode(errors="replace"))
    files: dict[str, str] = {}
    digest_input = bytearray()
    for entry in raw.split(b"\0"):
        if not entry:
            continue
        header, path = entry.split(b"\t", 1)
        _mode, kind, oid = header.split(b" ")
        if kind != b"blob":
            continue
        decoded = path.decode("utf-8", errors="surrogateescape")
        files[decoded] = oid.decode("ascii")
        digest_input.extend(path + b"\0" + oid + b"\0")
    digest = sha256(bytes(digest_input))
    if digest != WPT_PATH_OID_SHA256:
        fail("WPT_RECEIPT_MISMATCH", "compact WPT path/OID replay differs", expected=WPT_PATH_OID_SHA256, actual=digest)
    return files, digest


def validate_authorities(document: dict[str, Any], decisions: dict[str, Any]) -> list[dict[str, Any]]:
    authorities = document.get("authorities")
    if not isinstance(authorities, list):
        fail("AUTHORITY_MANIFEST_INVALID", "authority manifest must contain an authority array")
    allowed_roles = {"snapshot-2026", "current-work", "dated-publication", "external-parsing-authority"}
    seen = set()
    for record in authorities:
        role = record.get("role")
        if role not in allowed_roles or record.get("url") != record.get("response_url"):
            fail("AUTHORITY_RESPONSE_INVALID", "authority role or canonical response target is invalid", role=role, url=record.get("url"), response_url=record.get("response_url"))
        identity = (role, record.get("spec_id", record.get("authority_key", role)))
        if identity in seen:
            fail("AUTHORITY_IDENTITY_DUPLICATE", "authority manifest identity is duplicated", identity=identity)
        seen.add(identity)
        object_bytes(record)
    publications = {record.get("spec_id") for record in authorities if record.get("role") == "dated-publication"}
    if publications != set(decisions["source_paths"]):
        fail("PUBLICATION_AUTHORITY_SET_INVALID", "dated publication authorities do not exactly cover reviewed source paths")
    externals = {record.get("authority_key"): record.get("url") for record in authorities if record.get("role") == "external-parsing-authority"}
    if externals != decisions["external_authorities"]:
        fail("EXTERNAL_AUTHORITY_SET_INVALID", "external parsing authority manifest differs from reviewed primary URLs")
    if sum(record.get("role") == "snapshot-2026" for record in authorities) != 1 or sum(record.get("role") == "current-work" for record in authorities) != 1:
        fail("MEMBERSHIP_AUTHORITY_SET_INVALID", "Snapshot 2026 and Current Work must each have one retained authority")
    return authorities


def _classes(element: Element) -> set[str]:
    return {part.lower() for part in element.attributes.get("class", "").split()}


def _anchor(element: Element, tokens: list[Token]) -> str | None:
    for ancestor in reversed(element and next(token.ancestors for token in tokens if token.kind == "start_tag" and token.start == element.start)):
        if ancestor.attributes.get("id"):
            return ancestor.attributes["id"]
    prior = [token for token in tokens if token.kind == "start_tag" and token.start < element.start and token.name in {"h1", "h2", "h3", "h4", "h5", "h6"} and token.attributes and token.attributes.get("id")]
    return prior[-1].attributes["id"] if prior else None


def wpt_routes(spec: dict[str, Any], data: bytes, tokens: list[Token], tree: dict[str, str]) -> tuple[list[dict[str, Any]], list[dict[str, Any]]]:
    starts = [token for token in tokens if token.kind == "start_tag" and token.name == "wpt" and token.edit == "CURRENT"]
    elements = [Element(index + 1_000_000, "wpt", token.start, token.end, token.attributes or {}) for index, token in enumerate(starts)]
    # Match bodies by source nesting from the token ancestor serial of the real <wpt>.
    actual = {token.start: next((later.ancestors[-1] for later in tokens if later.start >= token.end and later.ancestors and later.ancestors[-1].start == token.start), None) for token in starts}
    records: list[dict[str, Any]] = []
    for ordinal, opening in enumerate(starts, start=1):
        element = actual[opening.start]
        if element is None:
            fail("WPT_STRUCTURE_INVALID", "WPT source element has no body ancestry", spec_id=spec["id"], start=opening.start)
        body_tokens = [token for token in tokens if any(parent.serial == element.serial for parent in token.ancestors)]
        text, _mapping = decoded_map(iter(body_tokens))
        attrs = opening.attributes or {}
        names = [" ".join(line.split()) for line in text.splitlines() if line.split() and not line.lstrip().startswith("#")]
        prefix = attrs.get("pathprefix") or spec.get("metadata", {}).get("wpt_path_prefix") or f"css/{spec['id']}"
        prefix = prefix.strip("/") + "/"
        anchor = _anchor(element, tokens)
        if not names:
            records.append({"ordinal": ordinal, "start": opening.start, "anchor": anchor, "declaration": "NO_DIRECT_TESTS", "reason": attrs.get("title", "empty source WPT block"), "status": "NO_DIRECT_TESTS"})
            continue
        for name in names:
            route = name.lstrip("/") if name.startswith(("/", "css/", "html/", "svg/", "dom/", "url/", "webidl/")) else prefix + name
            oid = tree.get(route)
            records.append({"ordinal": ordinal, "start": opening.start, "anchor": anchor, "declared": name, "route": route, "blob_oid": oid, "status": "PINNED" if oid else "UNMAPPED"})
    commented = []
    for token in tokens:
        if token.kind == "comment":
            for match in COMMENT_WPT.finditer(token.raw):
                commented.append({"start": token.start + match.start(), "source_state": "COMMENT", "status": "EXCLUDED_COMMENT"})
    return records, commented


def capture(csswg: Path, wpt: Path) -> dict[str, Any]:
    require_checkout(csswg, CSSWG_REVISION, "CSSWG")
    authorities = load(INPUTS / "authorities.json")
    decisions = load(INPUTS / "decisions.json")
    authority_rows = validate_authorities(authorities, decisions)
    by_role = {record["role"]: record for record in authority_rows if record["role"] in {"snapshot-2026", "current-work"}}
    if set(by_role) != {"snapshot-2026", "current-work"}:
        fail("MEMBERSHIP_AUTHORITY_MISSING", "Snapshot and Current Work byte authorities are required")
    snapshot = snapshot_members(object_bytes(by_role["snapshot-2026"]))
    current = current_work_members(object_bytes(by_role["current-work"]))
    scopes: dict[str, set[str]] = defaultdict(set)
    urls: dict[str, str] = {}
    for row in snapshot:
        identifier = spec_id(row["latest_url"])
        scopes[identifier].add(row["scope"])
        urls[identifier] = row["latest_url"]
    for row in current:
        identifier = spec_id(row["latest_url"])
        scopes[identifier].add(f"CURRENT_WORK_{row['group'].upper()}")
        urls[identifier] = row["latest_url"]
    for identifier in ("css-2026", "css-syntax-3"):
        scopes[identifier].add("FOUNDATION")
        urls[identifier] = f"https://www.w3.org/TR/{identifier}/"

    index, descriptions = source_index(csswg)
    aliases = decisions["aliases"]
    explicit_paths = decisions["source_paths"]
    for alias, directory in aliases.items():
        pinned_relative = next((relative for relative in explicit_paths.values() if Path(relative).parent.name.lower() == directory.lower()), None)
        target = csswg / pinned_relative if pinned_relative else next((path for path in index.values() if path.parent.name.lower() == directory.lower()), None)
        if target is None:
            fail("ALIAS_TARGET_MISSING", "reviewed dependency alias has no pinned CSSWG source", alias=alias, directory=directory)
        index[alias.upper()] = target
    records: dict[str, dict[str, Any]] = {}
    member_aliases = decisions.get("member_aliases", {})

    normalized_scopes: dict[str, set[str]] = defaultdict(set)
    normalized_urls: dict[str, str] = {}
    for identifier, values in scopes.items():
        canonical = member_aliases.get(identifier, identifier)
        normalized_scopes[canonical].update(values)
        normalized_urls[canonical] = urls[identifier]
    scopes, urls = normalized_scopes, normalized_urls

    def add(identifier: str, reason: str) -> None:
        if identifier in records:
            records[identifier]["scopes"] = sorted(set(records[identifier]["scopes"]) | {reason})
            return
        relative = explicit_paths.get(identifier)
        path = csswg / relative if relative else index.get(identifier.upper())
        if path is None or not path.is_file():
            fail("SOURCE_MEMBER_UNBOUND", "derived scope member has no exact CSSWG source", spec_id=identifier)
        latest = urls.get(identifier) or canonical_tr(metadata(path.read_bytes()).get("tr", "")) or f"https://www.w3.org/TR/{identifier}/"
        records[identifier] = _path_record(identifier, latest, scopes.get(identifier, {reason}) | {reason}, path, csswg)

    for identifier in sorted(scopes):
        add(identifier, "AUTHORITY_DERIVED")

    dependency_rows: list[dict[str, Any]] = []
    seen_occurrences: set[tuple[str, int, str]] = set()
    pending = sorted(records)
    visited: set[str] = set()
    reference_decisions = decisions["reference_decisions"]
    while pending:
        identifier = pending.pop(0)
        if identifier in visited:
            continue
        visited.add(identifier)
        spec = records[identifier]
        data = (csswg / spec["path"]).read_bytes()
        tokens = scan(data)
        for occurrence in occurrences(data, tokens):
            occurrence["from_spec"] = identifier
            key = occurrence["key"]
            identity = (identifier, occurrence["start"], key)
            if identity in seen_occurrences:
                fail("DEPENDENCY_OCCURRENCE_DUPLICATE", "dependency occurrence identity is not unique", identity=identity)
            seen_occurrences.add(identity)
            if occurrence["source_state"] == "COMMENT":
                classification, target = "COMMENT_NO_EDGE", None
            elif occurrence["source_state"] == "DELETED":
                classification, target = "DELETED_HISTORY_NO_EDGE", None
            elif not occurrence["normative"]:
                classification, target = "INFORMATIVE_REFERENCE", None
            elif key in index:
                path = index[key]
                target = next((name for name, row in records.items() if row["path"] == path.relative_to(csswg).as_posix()), None)
                target = target or canonical_tr(metadata(path.read_bytes()).get("tr", ""))
                target = spec_id(target) if isinstance(target, str) and target.startswith("http") else (target or path.parent.name)
                decision = reference_decisions.get(key, "CSS_SOURCE")
                if decision == "NON_PARSING_CSS_SOURCE":
                    classification, target = "NORMATIVE_NON_PARSING", None
                elif decision == "CSS_SOURCE":
                    classification = "NORMATIVE_CSS_SOURCE"
                    if target not in records:
                        scopes[target].add("NORMATIVE_DEPENDENCY")
                        urls.setdefault(target, canonical_tr(metadata(path.read_bytes()).get("tr", "")) or f"https://www.w3.org/TR/{target}/")
                        add(target, "NORMATIVE_DEPENDENCY")
                        pending.append(target)
                else:
                    fail("REFERENCE_DECISION_UNKNOWN", "CSS source reference has unknown reviewed decision", key=key, decision=decision)
            else:
                decision = reference_decisions.get(key)
                if decision not in {"EXTERNAL_PARSING_AUTHORITY", "NORMATIVE_NON_PARSING"}:
                    classification, target = "UNREVIEWED", None
                else:
                    classification, target = decision, None
            dependency_rows.append({**occurrence, "classification": classification, "target_spec": target})

    publications = {record["spec_id"]: record for record in authority_rows if record["role"] == "dated-publication"}
    root_blockers: list[dict[str, Any]] = []
    for spec in records.values():
        publication = publications.get(spec["id"])
        if publication is None:
            root_blockers.append({"code": "PUBLICATION_BYTES_MISSING", "spec_id": spec["id"]})
            spec["publication"] = {"status": "UNMAPPED"}
        elif not DATED_TR.fullmatch(publication["url"]):
            root_blockers.append({"code": "PUBLICATION_URL_NOT_DATED", "spec_id": spec["id"], "url": publication["url"]})
            spec["publication"] = {"status": "INVALID"}
        else:
            object_bytes(publication)
            spec["publication"] = {"url": publication["url"], "bytes": publication["bytes"], "sha256": publication["sha256"], "status": "PINNED"}
    for row in dependency_rows:
        if row["classification"] == "UNREVIEWED":
            root_blockers.append({"code": "DEPENDENCY_RELEVANCE_UNREVIEWED", "spec_id": row["from_spec"], "key": row["key"], "start": row["start"]})
    external_authorities = {record["authority_key"]: record for record in authority_rows if record["role"] == "external-parsing-authority"}
    used_external = {row["key"] for row in dependency_rows if row["classification"] == "EXTERNAL_PARSING_AUTHORITY"}
    for key in sorted(used_external):
        record = external_authorities.get(key)
        if record is None:
            root_blockers.append({"code": "EXTERNAL_PARSING_BYTES_MISSING", "key": key})
        else:
            object_bytes(record)

    alias_receipts = []
    for alias in sorted(aliases):
        path = index[alias.upper()].relative_to(csswg).as_posix()
        target = next((identifier for identifier, record in records.items() if record["path"] == path), None)
        alias_receipts.append(
            {
                "alias": alias,
                "source_path": path,
                "target_spec": target,
                "decision": "RESOLVED_MEMBER" if target else "REVIEWED_NON_MEMBER_SOURCE",
            }
        )

    tree, tree_digest = _wpt_tree(wpt)
    routes: list[dict[str, Any]] = []
    commented_routes: list[dict[str, Any]] = []
    for spec in records.values():
        data = (csswg / spec["path"]).read_bytes()
        spec_routes, comments = wpt_routes(spec, data, scan(data), tree)
        routes.extend({"spec_id": spec["id"], **row} for row in spec_routes)
        commented_routes.extend({"spec_id": spec["id"], **row} for row in comments)

    return {
        "schema_version": 3,
        "stage": "sealed-membership",
        "authority_receipts": {role: {key: by_role[role][key] for key in ("url", "bytes", "sha256")} for role in sorted(by_role)},
        "selection": {
            "snapshot_occurrences": snapshot,
            "current_work_level_4": current,
            "member_count": len(records),
            "aliases": alias_receipts,
        },
        "csswg": {"revision": CSSWG_REVISION, "source_count": len(records), "source_bytes": sum(row["bytes"] for row in records.values())},
        "specs": [records[key] for key in sorted(records)],
        "dependencies": sorted(dependency_rows, key=lambda row: (row["from_spec"], row["start"], row["key"])),
        "external_parsing_authorities": [{key: record[key] for key in ("authority_key", "url", "bytes", "sha256")} for record in sorted(external_authorities.values(), key=lambda row: row["authority_key"])],
        "wpt": {"revision": WPT_REVISION, "tree_oid": WPT_TREE, "file_count": len(tree), "path_oid_sha256": tree_digest, "routes": routes, "comment_exclusions": commented_routes},
        "root_blockers": root_blockers,
    }
