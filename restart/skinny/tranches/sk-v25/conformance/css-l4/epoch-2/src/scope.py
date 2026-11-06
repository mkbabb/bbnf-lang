"""Stage 1: canonical source, dependency, publication, and WPT epoch."""

from __future__ import annotations

import html
import re
from collections import defaultdict
from pathlib import Path
from typing import Any
from urllib.parse import urlsplit

from common import (
    CSSWG_REVISION,
    EPOCH1,
    WPT_REVISION,
    collapse,
    git,
    read_json,
    require_revision,
    sha256,
)


BANK_HASHES = {
    "README.md": "6a528495949457660cfc170657a35f642cc08fe285f4793c209ea8635152846c",
    "data/inventory.json": "b7d6d155705e6654a7f80c72711f11fcc21863b8096a79b714b7f4a5de770fae",
    "data/scope.json": "99e2223fb686443b6e612d659115940a20d3c053014201df51bf9c2ec52be50b",
    "data/source-manifest.json": "ac236359f163d444a1f2bbb20692f930fb677c10b2f0ec6837bfbb0dd91e6300",
    "data/syntax.json": "293573fe4071646c9c9eab97e8358a5b21ec16f28a0c25a796a311c2f2518d78",
    "data/rows.json": "67fbbbcd9e93d0d6f2aafa86aa9031cc00c13e0f4736ffb34bdeb9cefbc1e6f2",
    "data/productions.json": "e0e4cc80af1f8b038b98d22f569271fda5c7b62bfa2de5f73d06b9289421d17a",
    "GESTALT-1-ADJUDICATION.md": "e5a5d85f73dee62c09962b7a7f5a980dea9075c10e8425d0064d61a314e3abf8",
}

SOURCE_OVERRIDES = {
    "css-font-loading": "css-font-loading-3/Overview.bs",
    "css-fonts-3": "css-fonts-3/Overview.html",
    "css-namespaces": "css-namespaces-3/Overview.bs",
    "css-style-attr": "css-style-attr-1/Overview.src.html",
    "css3-mediaqueries": "mediaqueries-3/Overview.html",
    "selectors-3": "selectors-3/Overview.src.html",
}

REFERENCE_ALIASES = {
    "CSS2": "CSS2",
    "CSS21": "CSS2",
    "CSS3BG": "css-backgrounds-3",
    "CSS3COLOR": "css-color-3",
    "CSS3UI": "css-ui-3",
    "SELECT": "selectors-3",
}

PARSING_EXTERNALS = {
    "DOM",
    "ENCODING",
    "FETCH",
    "HTML",
    "INFRA",
    "URL",
    "WEBIDL",
    "XML-NAMES",
}

NORMATIVE_REFERENCE = re.compile(r"\[\[!([^\]|\s]+)")
METADATA = re.compile(rb"<pre\s+class\s*=\s*['\"]?metadata['\"]?[^>]*>(.*?)</pre>", re.I | re.S)
WPT_BLOCK = re.compile(rb"<wpt\b(?P<attrs>(?:[^>'\"]|'[^']*'|\"[^\"]*\")*)>(?P<body>.*?)</wpt\s*>", re.I | re.S)
ATTRIBUTE = re.compile(rb"([A-Za-z_:][-A-Za-z0-9_:.]*)\s*=\s*(?:\"([^\"]*)\"|'([^']*)'|([^\s>]+))")


def canonical_id(url: str) -> str:
    parts = [part for part in urlsplit(url).path.split("/") if part]
    if len(parts) < 2 or parts[0] != "TR":
        raise ValueError(f"not a W3C TR root: {url}")
    return parts[1]


def metadata(data: bytes) -> dict[str, str]:
    match = METADATA.search(data)
    if match is None:
        return {}
    result: dict[str, str] = {}
    for raw in match.group(1).decode("utf-8", errors="replace").splitlines():
        if ":" not in raw:
            continue
        key, value = raw.split(":", 1)
        key = collapse(key).lower().replace(" ", "_")
        if key not in result:
            result[key] = collapse(value)
    return result


def verify_bank() -> list[dict[str, Any]]:
    receipts = []
    for relative, expected in sorted(BANK_HASHES.items()):
        path = EPOCH1 / relative
        data = path.read_bytes()
        actual = sha256(data)
        if actual != expected:
            raise ValueError(f"epoch-1 bank drifted: {relative}: {actual}")
        receipts.append({"path": relative, "bytes": len(data), "sha256": actual})
    return receipts


def source_candidates(csswg: Path) -> tuple[dict[str, Path], dict[str, dict[str, str]]]:
    index: dict[str, Path] = {}
    descriptions: dict[str, dict[str, str]] = {}
    for child in sorted(csswg.iterdir(), key=lambda item: item.name.lower()):
        if not child.is_dir():
            continue
        choices = [child / "Overview.bs", child / "Overview.html", child / "Overview.src.html"]
        path = next((candidate for candidate in choices if candidate.is_file()), None)
        if path is None:
            continue
        info = metadata(path.read_bytes())
        keys = {child.name.upper()}
        shortname = info.get("shortname")
        level = info.get("level")
        if shortname:
            keys.add(shortname.upper())
            if level:
                keys.add(f"{shortname}-{level}".upper())
        tr = info.get("tr")
        if tr:
            try:
                keys.add(canonical_id(tr).upper())
            except ValueError:
                pass
        for key in keys:
            index.setdefault(key, path)
        descriptions[child.name] = info
    for key, directory in REFERENCE_ALIASES.items():
        match = next((path for path in index.values() if path.parent.name == directory), None)
        if match is not None:
            index[key] = match
    return index, descriptions


def path_record(identifier: str, latest_url: str, scopes: list[str], path: Path, csswg: Path) -> dict[str, Any]:
    data = path.read_bytes()
    info = metadata(data)
    return {
        "id": identifier,
        "latest_url": latest_url,
        "scopes": sorted(set(scopes)),
        "path": path.relative_to(csswg).as_posix(),
        "bytes": len(data),
        "sha256": sha256(data),
        "metadata": {
            key: info[key]
            for key in ("title", "status", "shortname", "level", "tr", "wpt_path_prefix")
            if key in info
        },
    }


def wpt_tree(wpt: Path) -> tuple[str, str, dict[str, str], str]:
    require_revision(wpt, WPT_REVISION, "WPT")
    tree = git(wpt, "rev-parse", "HEAD^{tree}")
    raw = git(wpt, "ls-tree", "-r", "--full-tree", "HEAD")
    files: dict[str, str] = {}
    for line in raw.splitlines():
        header, path = line.split("\t", 1)
        _mode, kind, oid = header.split(" ")
        if kind == "blob":
            files[path] = oid
    digest = sha256("".join(f"{path}\0{files[path]}\0" for path in sorted(files)).encode())
    return WPT_REVISION, tree, files, digest


def attributes(raw: bytes) -> dict[str, str]:
    result = {}
    for match in ATTRIBUTE.finditer(raw):
        value = next(group for group in match.groups()[1:] if group is not None)
        result[match.group(1).decode().lower()] = html.unescape(value.decode("utf-8", errors="replace"))
    return result


def wpt_routes(spec: dict[str, Any], source: bytes, tree: dict[str, str]) -> list[dict[str, Any]]:
    metadata_prefix = spec.get("metadata", {}).get("wpt_path_prefix")
    if metadata_prefix:
        default_prefix = metadata_prefix.strip("/") + "/"
    else:
        default_prefix = f"css/{spec['id']}/"
    records = []
    for ordinal, match in enumerate(WPT_BLOCK.finditer(source), start=1):
        attrs = attributes(match.group("attrs"))
        body = html.unescape(match.group("body").decode("utf-8", errors="replace"))
        body = re.sub(r"<!--.*?-->", "", body, flags=re.S)
        names = [collapse(line) for line in body.splitlines() if collapse(line) and not collapse(line).startswith("#")]
        prefix = attrs.get("pathprefix")
        if prefix is not None:
            prefix = prefix.strip("/") + "/"
        else:
            prefix = default_prefix
        if not names:
            records.append(
                {
                    "ordinal": ordinal,
                    "source_offset": match.start(),
                    "declaration": "NO_DIRECT_TESTS",
                    "reason": attrs.get("title", "empty WPT block"),
                    "terminal_state": "CLASSIFIED_NO_DIRECT_TESTS",
                }
            )
            continue
        for name in names:
            if name.startswith("/") or name.startswith(("css/", "html/", "svg/", "dom/", "url/", "webidl/")):
                route = name.lstrip("/")
            else:
                route = prefix + name
            oid = tree.get(route)
            records.append(
                {
                    "ordinal": ordinal,
                    "source_offset": match.start(),
                    "declared": name,
                    "route": route,
                    "blob_oid": oid,
                    "terminal_state": "PINNED" if oid else "UNMAPPED",
                }
            )
    return records


def capture(csswg: Path, wpt: Path, publications_path: Path | None = None) -> dict[str, Any]:
    require_revision(csswg, CSSWG_REVISION, "CSSWG")
    bank = verify_bank()
    old_scope = read_json(EPOCH1 / "data" / "scope.json")
    old_manifest = read_json(EPOCH1 / "data" / "source-manifest.json")
    inventory = read_json(EPOCH1 / "data" / "inventory.json")
    manifest_by_id = {record["id"]: record for record in old_manifest["specs"]}
    source_index, _descriptions = source_candidates(csswg)

    records: dict[str, dict[str, Any]] = {}
    for old in old_scope["specs"]:
        identifier = canonical_id(old["latest_url"])
        old_source = manifest_by_id[identifier]
        path = csswg / old_source["path"]
        records[identifier] = path_record(identifier, old["latest_url"], old["scopes"], path, csswg)

    references: dict[str, set[str]] = defaultdict(set)
    pending = list(sorted(records))
    visited: set[str] = set()
    while pending:
        identifier = pending.pop(0)
        if identifier in visited:
            continue
        visited.add(identifier)
        source = (csswg / records[identifier]["path"]).read_text()
        for raw in NORMATIVE_REFERENCE.findall(source):
            key = raw.split("#", 1)[0].upper()
            references[key].add(identifier)
            dependency = source_index.get(key)
            if dependency is None:
                continue
            dependency_relative = dependency.relative_to(csswg).as_posix()
            if any(record["path"] == dependency_relative for record in records.values()):
                continue
            dependency_id = dependency.parent.name
            if dependency_id in records:
                continue
            info = metadata(dependency.read_bytes())
            latest = info.get("tr", f"https://www.w3.org/TR/{dependency_id}/")
            try:
                dependency_id = canonical_id(latest)
            except ValueError:
                pass
            records[dependency_id] = path_record(
                dependency_id,
                latest,
                ["normative-dependency"],
                dependency,
                csswg,
            )
            pending.append(dependency_id)

    dependencies = []
    for key, owners in sorted(references.items()):
        dependency = source_index.get(key)
        if dependency is not None:
            target = next(
                (record["id"] for record in records.values() if record["path"] == dependency.relative_to(csswg).as_posix()),
                dependency.parent.name,
            )
            classification = "CSSWG_NORMATIVE_SOURCE"
            state = "SOURCE_PINNED"
        elif key in PARSING_EXTERNALS:
            target = None
            classification = "EXTERNAL_PARSING_AUTHORITY"
            state = "UNPROVEN"
        else:
            target = None
            classification = "EXTERNAL_NORMATIVE_REFERENCE"
            state = "CLASSIFIED_EXTERNAL_NON_SOURCE"
        dependencies.append(
            {
                "key": key,
                "from_specs": sorted(owners),
                "classification": classification,
                "target_spec": target,
                "terminal_state": state,
            }
        )

    published: dict[str, dict[str, Any]] = {}
    for record in inventory["foundations"] + inventory["level_4"]:
        published[canonical_id(record["latest_url"])] = {
            "version_url": record["version_url"],
            "bytes": record["bytes"],
            "sha256": record["sha256"],
            "terminal_state": "PINNED",
        }
    fetched = read_json(publications_path) if publications_path and publications_path.exists() else {"publications": []}
    for record in fetched.get("publications", []):
        published[record["id"]] = record
    fetch_failures = {
        record["spec_id"]: record
        for record in fetched.get("failures", [])
        if isinstance(record, dict) and isinstance(record.get("spec_id"), str)
    }

    revision, tree_oid, tree, tree_digest = wpt_tree(wpt)
    all_routes = []
    for identifier in sorted(records):
        spec = records[identifier]
        routes = wpt_routes(spec, (csswg / spec["path"]).read_bytes(), tree)
        spec["publication"] = published.get(identifier, {"terminal_state": "MISSING"})
        spec["wpt_routes"] = routes
        all_routes.extend((identifier, route) for route in routes)

    blockers = []
    for dependency in dependencies:
        if dependency["terminal_state"] in {"UNPROVEN", "MISSING"}:
            blockers.append({"code": "DEPENDENCY_UNPROVEN", "key": dependency["key"]})
    for spec in records.values():
        if spec["publication"]["terminal_state"] != "PINNED":
            failure = fetch_failures.get(spec["id"])
            if failure:
                blockers.append(
                    {
                        "code": "PUBLICATION_FETCH_BLOCKED",
                        "spec_id": spec["id"],
                        "error_type": failure.get("error_type"),
                        "detail": failure.get("detail"),
                    }
                )
            else:
                blockers.append({"code": "PUBLICATION_MISSING", "spec_id": spec["id"]})
        for route in spec["wpt_routes"]:
            if route["terminal_state"] == "UNMAPPED":
                blockers.append({"code": "WPT_ROUTE_UNMAPPED", "spec_id": spec["id"], "route": route["route"]})

    return {
        "schema_version": 2,
        "stage": "canonical-scope",
        "candidate_input_policy": "OBSERVATION_ONLY_FORBID_SCHEMA_OR_TYPE_INPUT",
        "epoch_1_bank": bank,
        "csswg": {
            "repository": "https://github.com/w3c/csswg-drafts.git",
            "revision": CSSWG_REVISION,
        },
        "wpt": {
            "repository": "https://github.com/web-platform-tests/wpt.git",
            "revision": revision,
            "tree_oid": tree_oid,
            "file_count": len(tree),
            "tree_manifest_sha256": tree_digest,
            "manifest": [
                {"path": path, "blob_oid": tree[path]}
                for path in sorted(tree)
            ],
            "route_count": sum(1 for _spec, route in all_routes if "route" in route),
            "unmapped_route_count": sum(1 for _spec, route in all_routes if route.get("terminal_state") == "UNMAPPED"),
        },
        "publication_input": {
            "path": publications_path.name if publications_path else None,
            "sha256": sha256(publications_path.read_bytes()) if publications_path and publications_path.exists() else None,
            "fetch_policy": fetched.get("fetch_policy", "NOT_FETCHED"),
            "requests_made": fetched.get("requests_made", 0),
            "failures": list(fetch_failures.values()),
        },
        "dependencies": dependencies,
        "specs": [records[key] for key in sorted(records)],
        "blockers": blockers,
    }
