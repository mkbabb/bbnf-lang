#!/usr/bin/env python3
"""Capture/verify content-addressed primary authority bytes and pinned Git receipts."""

from __future__ import annotations

import argparse
import html
import json
import re
import subprocess
import time
from pathlib import Path
from urllib.parse import urljoin, urlsplit

from common import (
    CSSWG_REVISION, DEFAULT_CSSWG, DEFAULT_WPT, INPUTS, OBJECTS, ROOT, WPT_PATH_OID_SHA256,
    WPT_REVISION, WPT_TREE, enforce_offline, load, object_bytes, object_path, require_checkout, require_toolchain, sha256, store,
)
from membership import _wpt_tree, metadata


SNAPSHOT = "https://www.w3.org/TR/2026/NOTE-css-2026-20260622/"
CURRENT_WORK = "https://www.w3.org/Style/CSS/current-work.en.html"
MEMBERSHIP_RECEIPTS = {
    "snapshot-2026": (SNAPSHOT, 621957, "fddc6c448e08e5eaa22a362c759a3a7c28045da1cdffad2d8421d679d49839d2"),
    "current-work": (CURRENT_WORK, 78453, "f85a5166c5a20a8d11bf5ef74ac8a4c0d597adc7e7b014f32e318393a3627aa5"),
}
ALLOWED_HOSTS = {"www.w3.org", "dom.spec.whatwg.org", "encoding.spec.whatwg.org", "fetch.spec.whatwg.org", "html.spec.whatwg.org", "infra.spec.whatwg.org", "url.spec.whatwg.org", "webidl.spec.whatwg.org"}
DATED = re.compile(r"^https://www\.w3\.org/TR/\d{4}/[A-Za-z0-9-]+-\d{8}/$")
DATED_LINK = re.compile(r"https://www\.w3\.org/TR/\d{4}/[A-Za-z0-9-]+-\d{8}/")


def fetch(url: str) -> tuple[bytes, str]:
    if urlsplit(url).hostname not in ALLOWED_HOSTS:
        raise ValueError(f"authority host is not an official reviewed primary source: {url}")
    result = subprocess.run(
        ["curl", "-L", "--fail", "--silent", "--show-error", "--max-time", "60", "--user-agent", "bbnf-css-l4-epoch-3/1 (+https://github.com/mkbabb/bbnf-lang)", "--write-out", "\\0%{url_effective}", url],
        check=True,
        capture_output=True,
    )
    data, separator, target_bytes = result.stdout.rpartition(b"\\0")
    if not separator:
        raise ValueError(f"curl did not emit its exact response target: {url}")
    target = target_bytes.decode("utf-8")
    if urlsplit(target).hostname not in ALLOWED_HOSTS:
        raise ValueError(f"authority redirect left reviewed primary hosts: {url} -> {target}")
    if urlsplit(target).hostname == "www.w3.org":
        time.sleep(0.4)
    return data, target


def retain(data: bytes) -> dict[str, object]:
    digest = sha256(data)
    path = object_path(digest)
    path.parent.mkdir(parents=True, exist_ok=True)
    if path.exists() and path.read_bytes() != data:
        raise ValueError(f"content-address collision at {path}")
    path.write_bytes(data)
    return {"bytes": len(data), "sha256": digest}


def retained(expected_bytes: int, digest: str) -> bytes | None:
    path = object_path(digest)
    if not path.is_file():
        return None
    data = path.read_bytes()
    return data if len(data) == expected_bytes and sha256(data) == digest else None


def frozen_publication_seeds() -> dict[str, dict[str, object]]:
    """Use epoch-1/2 receipts only as URL/hash fetch seeds, never as proof."""
    inventory = load(ROOT.parent / "data" / "inventory.json")
    fetched = load(ROOT.parent / "epoch-2" / "inputs" / "publications.json")
    rows = list(inventory["foundations"]) + list(inventory["level_4"]) + list(fetched["publications"])
    result = {}
    for original in rows:
        row = dict(original)
        row["version_url"] = str(row.get("version_url", "")).rstrip("/") + "/"
        if not DATED.fullmatch(row["version_url"]):
            continue
        identifier = urlsplit(str(row["latest_url"])).path.strip("/").split("/")[-1]
        result[identifier] = row
    return result


def dated_url(latest: str, body: bytes) -> str:
    text = body.decode("utf-8", errors="replace")
    marker = re.search(r"This\s+[Vv]ersion\s*:", text)
    if marker:
        match = re.search(r"href\s*=\s*['\"]([^'\"]+)", text[marker.start() : marker.start() + 5000], re.I)
        if match:
            candidate = urljoin(latest, html.unescape(match.group(1)))
            if DATED.fullmatch(candidate):
                return candidate
    shortname = urlsplit(latest).path.strip("/").split("/")[-1]
    history_url = f"https://www.w3.org/standards/history/{shortname}/"
    history, _target = fetch(history_url)
    match = DATED_LINK.search(history.decode("utf-8", errors="replace"))
    if match is None:
        raise ValueError(f"official W3C history has no canonical dated publication for {latest}")
    return match.group()


def capture(csswg: Path) -> dict[str, object]:
    require_checkout(csswg, CSSWG_REVISION, "CSSWG")
    decisions = load(INPUTS / "decisions.json")
    records: list[dict[str, object]] = []
    for role, (url, expected_bytes, digest) in MEMBERSHIP_RECEIPTS.items():
        data = retained(expected_bytes, digest)
        target = url
        if data is None:
            data, target = fetch(url)
        if len(data) != expected_bytes or sha256(data) != digest:
            raise ValueError(f"membership authority differs from its exact retained receipt: {role}")
        records.append({"role": role, "url": url, "response_url": target, **retain(data)})
    seeds = frozen_publication_seeds()
    for identifier, relative in sorted(decisions["source_paths"].items()):
        source = (csswg / relative).read_bytes()
        info = metadata(source)
        latest = info.get("tr", f"https://www.w3.org/TR/{identifier}/")
        seed = seeds.get(identifier)
        publication = retained(int(seed["bytes"]), str(seed["sha256"])) if seed else None
        if publication is not None:
            version = str(seed["version_url"])
            target = version
        else:
            body, latest_target = fetch(latest)
            version = dated_url(latest_target, body)
            publication, target = fetch(version)
        if target != version or not DATED.fullmatch(version):
            raise ValueError(f"dated W3C publication did not preserve its canonical response target: {version} -> {target}")
        records.append({"role": "dated-publication", "spec_id": identifier, "latest_url": latest, "url": version, "response_url": target, **retain(publication)})
    for key, url in sorted(decisions["external_authorities"].items()):
        data, target = fetch(url)
        records.append({"role": "external-parsing-authority", "authority_key": key, "url": url, "response_url": target, **retain(data)})
    return {"schema_version": 1, "capture_policy": "CURL_ONE_REQUEST_PER_CANONICAL_URL_NO_RETRY_400MS_W3C_INTERVAL_OFFICIAL_PRIMARY_HOSTS_ONLY", "authorities": records}


def verify(document: dict[str, object], csswg: Path, wpt: Path) -> dict[str, object]:
    require_checkout(csswg, CSSWG_REVISION, "CSSWG")
    require_checkout(wpt, WPT_REVISION, "WPT")
    authorities = document.get("authorities")
    if not isinstance(authorities, list):
        raise ValueError("authority manifest lacks records")
    seen: set[tuple[object, object]] = set()
    total = 0
    for record in authorities:
        identity = (record.get("role"), record.get("spec_id", record.get("authority_key", record.get("role"))))
        if identity in seen:
            raise ValueError(f"duplicate authority identity: {identity}")
        seen.add(identity)
        total += len(object_bytes(record))
        if record.get("role") == "dated-publication" and (not DATED.fullmatch(str(record.get("url"))) or record.get("url") != record.get("response_url")):
            raise ValueError(f"publication is not a canonical dated response: {identity}")
    tree, digest = _wpt_tree(wpt)
    return {"status": "PASS", "authority_count": len(authorities), "authority_bytes": total, "csswg_revision": CSSWG_REVISION, "wpt_revision": WPT_REVISION, "wpt_tree": WPT_TREE, "wpt_file_count": len(tree), "wpt_path_oid_sha256": digest}


def main() -> int:
    require_toolchain()
    parser = argparse.ArgumentParser()
    parser.add_argument("--csswg-root", type=Path, default=DEFAULT_CSSWG)
    parser.add_argument("--wpt-root", type=Path, default=DEFAULT_WPT)
    parser.add_argument("--capture", action="store_true")
    parser.add_argument("--manifest", type=Path, default=INPUTS / "authorities.json")
    options = parser.parse_args()
    if not options.capture:
        enforce_offline()
    if options.capture:
        document = capture(options.csswg_root)
        store(options.manifest, document)
    else:
        document = load(options.manifest)
    result = verify(document, options.csswg_root, options.wpt_root)
    print(json.dumps(result, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
