#!/usr/bin/env python3
"""One-time offline capture of exact inputs; replay never needs these checkouts."""

from __future__ import annotations

import argparse
import gzip
import json
import os
import shutil
import subprocess
import re
from pathlib import Path
from typing import Any

from common import INPUTS, OBJECTS, ROOT, encode, load, sha256, store


CSSWG_REVISION = "c7573530343759ace8e46438a1fa2c44515b5554"
WPT_REVISION = "37cd7ff74eb974fd41600bf7bc01d37576abf8b5"
WPT_TREE = "12fa64c9afe86e199ee0e737755b6507f363ceff"
WPT_PATH_OID_SHA256 = "cf3aedd08cbb161aeebd3c9e19c10f855f2be0be23fa5ecbbd2035d2e31db699"
GIT_BINARY = Path("/usr/bin/git")
GIT_BINARY_SHA256 = "12bed4523661307059b879b9b54e77a73176e9d27d27a0e40363271d8f0668ba"
SOURCE_CHOICES = ("Overview.bs", "Overview.html", "Overview.src.html")
METADATA = re.compile(rb"<pre\s+class\s*=\s*['\"]?metadata['\"]?[^>]*>(.*?)</pre>", re.I | re.S)


def metadata(data: bytes) -> dict[str, str]:
    found = METADATA.search(data)
    if found is None:
        return {}
    result: dict[str, str] = {}
    for line in found.group(1).decode("utf-8", errors="replace").splitlines():
        if ":" in line:
            key, value = line.split(":", 1)
            result.setdefault("_".join(key.lower().split()), " ".join(value.split()))
    return result


def git(root: Path, *arguments: str, binary: Path = GIT_BINARY, raw: bool = False) -> bytes | str:
    environment = {
        **os.environ,
        "GIT_NO_LAZY_FETCH": "1",
        "GIT_TERMINAL_PROMPT": "0",
    }
    result = subprocess.run(
        [str(binary), "-c", "gc.auto=0", "-c", "maintenance.auto=false", "-C", str(root), *arguments],
        check=True,
        capture_output=True,
        env=environment,
    )
    return result.stdout if raw else result.stdout.decode("utf-8", errors="strict")


def require_checkout(root: Path, revision: str, label: str) -> None:
    actual = str(git(root, "rev-parse", "HEAD")).strip()
    if actual != revision:
        raise ValueError(f"{label} revision differs: {actual}")
    dirty = str(git(root, "status", "--porcelain=v1", "--untracked-files=no"))
    if dirty:
        raise ValueError(f"{label} checkout is dirty")


def retain(data: bytes) -> dict[str, Any]:
    digest = sha256(data)
    path = OBJECTS / digest[:2] / digest[2:]
    path.parent.mkdir(parents=True, exist_ok=True)
    if path.exists() and path.read_bytes() != data:
        raise ValueError(f"content collision: {digest}")
    path.write_bytes(data)
    return {"bytes": len(data), "sha256": digest}


def gzip_zero(data: bytes) -> bytes:
    return gzip.compress(data, compresslevel=9, mtime=0)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--csswg-root", type=Path, required=True)
    parser.add_argument("--wpt-root", type=Path, required=True)
    parser.add_argument("--epoch3-root", type=Path, required=True)
    options = parser.parse_args()
    if sha256(GIT_BINARY.read_bytes()) != GIT_BINARY_SHA256:
        raise ValueError("capture Git binary differs from the reviewed pin")
    require_checkout(options.csswg_root, CSSWG_REVISION, "CSSWG")
    require_checkout(options.wpt_root, WPT_REVISION, "WPT")
    if str(git(options.wpt_root, "rev-parse", "HEAD^{tree}")).strip() != WPT_TREE:
        raise ValueError("WPT root tree differs")

    decisions = json.loads((options.epoch3_root / "inputs" / "decisions.json").read_text())
    authorities = json.loads((options.epoch3_root / "inputs" / "authorities.json").read_text())
    matrix_path = options.epoch3_root / "data" / "matrix.json"
    matrix = json.loads(matrix_path.read_text())

    sources = []
    for spec_id, relative in sorted(decisions["source_paths"].items()):
        data = (options.csswg_root / relative).read_bytes()
        sources.append({"spec_id": spec_id, "path": relative, **retain(data)})

    # The full checkout key catalog is small but essential: ambiguity cannot be
    # detected from the selected source subset alone. Unselected targets retain
    # identities/hashes only and become typed blockers rather than implicit edges.
    key_catalog = []
    for directory in sorted((path for path in options.csswg_root.iterdir() if path.is_dir()), key=lambda path: path.name.lower()):
        path = next((directory / name for name in SOURCE_CHOICES if (directory / name).is_file()), None)
        if path is None:
            continue
        data = path.read_bytes()
        info = metadata(data)
        keys = {directory.name.upper()}
        if info.get("shortname"):
            keys.add(info["shortname"].upper())
            if info.get("level"):
                keys.add(f"{info['shortname']}-{info['level']}".upper())
        tr = info.get("tr", "").rstrip("/").rsplit("/", 1)[-1]
        if tr:
            keys.add(tr.upper())
        key_catalog.append([directory.name, path.relative_to(options.csswg_root).as_posix(), len(data), sha256(data), sorted(keys)])
    key_catalog_raw = encode({"schema_version": 1, "entries": key_catalog})
    key_catalog_object = retain(gzip_zero(key_catalog_raw))

    retained_authorities = []
    for row in authorities["authorities"]:
        if row["role"] not in {"snapshot-2026", "current-work", "external-parsing-authority"}:
            continue
        source = options.epoch3_root / "inputs" / "objects" / "sha256" / row["sha256"][:2] / row["sha256"][2:]
        data = source.read_bytes()
        if len(data) != row["bytes"] or sha256(data) != row["sha256"]:
            raise ValueError(f"epoch-3 authority object drift: {row['sha256']}")
        receipt = retain(data)
        retained_authorities.append({key: row[key] for key in row if key not in {"bytes", "sha256"}} | receipt)

    raw_tree = git(options.wpt_root, "ls-tree", "-rz", "--full-tree", "HEAD", raw=True)
    catalog = bytearray()
    count = 0
    for entry in bytes(raw_tree).split(b"\0"):
        if not entry:
            continue
        header, path = entry.split(b"\t", 1)
        _mode, kind, oid = header.split(b" ")
        if kind != b"blob":
            continue
        path.decode("utf-8", errors="strict")
        catalog.extend(path + b"\0" + oid + b"\0")
        count += 1
    catalog_digest = sha256(bytes(catalog))
    if count != 160_788 or catalog_digest != WPT_PATH_OID_SHA256:
        raise ValueError(f"WPT path/blob-OID catalog differs: {count} {catalog_digest}")
    compressed_catalog = retain(gzip_zero(bytes(catalog)))
    wpt = {
        "revision": WPT_REVISION,
        "tree_oid": WPT_TREE,
        "entry_count": count,
        "catalog_encoding": "gzip-mtime-zero(path_utf8 + NUL + blob_oid_ascii + NUL)",
        "catalog_uncompressed_bytes": len(catalog),
        "catalog_uncompressed_sha256": catalog_digest,
        "catalog_object": compressed_catalog,
        "blob_content_state": "UNRETAINED_TYPED_BLOCKER_FOR_EXACT_FEATURE_OWNERSHIP",
    }

    source_rows = matrix["sources"]
    source_index = {row["id"]: index for index, row in enumerate(source_rows)}
    obligations = {
        "schema_version": 1,
        "role": "INHERITED_OPEN_OBLIGATIONS_NOT_A_SOURCE_SEMANTIC_DENOMINATOR",
        "provenance": {
            "epoch3_matrix_bytes": matrix_path.stat().st_size,
            "epoch3_matrix_sha256": sha256(matrix_path.read_bytes()),
            "epoch3_adjudication_sha256": sha256((options.epoch3_root / "GESTALT-1-ADJUDICATION.md").read_bytes()),
        },
        "sources": [[row["id"], row["spec_id"], row["path"], row["start"], row["end"], row["sha256"], row.get("anchor"), row["source_form"]] for row in source_rows],
        "rows": [[row["id"], row["family"], row["module"], source_index[row["source_id"]]] for row in matrix["rows"]],
    }
    if len(obligations["rows"]) != 8_796:
        raise ValueError("inherited open obligation count differs")
    obligations_raw = encode(obligations)
    obligation_object = retain(gzip_zero(obligations_raw))

    policy = {
        "schema_version": 1,
        "source_paths": decisions["source_paths"],
        "aliases": decisions["aliases"],
        "member_aliases": decisions["member_aliases"],
        "legacy_pre_sites": decisions["pre_sites"],
        "retained_external_keys": sorted(decisions["external_authorities"]),
        "mandatory_unsealed_parsing_keys": ["ISO10646", "UNICODE"],
        "dependency_decision_policy": "ONE_EMITTED_DECISION_PER_LIVE_SOURCE_OCCURRENCE; COLLISIONS_AND_UNSEALED_AUTHORITIES_ARE_TYPED_LOCAL_BLOCKERS",
        "candidate_allowed_prefixes": ["grammar/css/l4/", "tests/fixtures/css/"],
    }
    store(INPUTS / "policy.json", policy)

    governing = {
        "schema_version": 1,
        "documents": [
            {"role": "lock-constitution", "path": "LOCK-CONSTITUTION.md", "sha256": sha256((ROOT.parents[2] / "LOCK-CONSTITUTION.md").read_bytes())},
            {"role": "governing-wave", "path": "waves/BB-VALUE-TAPE-P0.md", "sha256": sha256((ROOT.parents[2] / "waves" / "BB-VALUE-TAPE-P0.md").read_bytes())},
            {"role": "binding-repair", "path": "conformance/css-l4/epoch-3/GESTALT-1-ADJUDICATION.md", "sha256": sha256((options.epoch3_root / "GESTALT-1-ADJUDICATION.md").read_bytes())},
        ],
    }
    store(INPUTS / "governing.json", governing)

    controls = json.loads((options.epoch3_root / "inputs" / "downstream-controls.json").read_text())
    for row in controls["controls"]:
        row["correctness_status"] = "UNRUN"
        row["selection_status"] = "UNRUN"
        row["performance_status"] = "UNRUN"
    controls["policy"] = "Pinned discovery placeholders only; no build, correctness, selection, sample, ratio, or performance evidence exists."
    store(INPUTS / "downstream-controls.json", controls)

    manifest = {
        "schema_version": 1,
        "capture": {
            "network": "FORBIDDEN_AND_UNUSED",
            "csswg_clean": True,
            "wpt_clean": True,
            "git": {"binary": "/usr/bin/git", "binary_sha256": GIT_BINARY_SHA256},
            "private_paths_retained": False,
        },
        "csswg": {
            "revision": CSSWG_REVISION,
            "source_count": len(sources),
            "sources": sources,
            "full_source_key_catalog": {
                "entry_count": len(key_catalog),
                "encoding": "gzip-mtime-zero(canonical-json)",
                "uncompressed_bytes": len(key_catalog_raw),
                "uncompressed_sha256": sha256(key_catalog_raw),
                "object": key_catalog_object,
            },
        },
        "primary_authorities": retained_authorities,
        "wpt": wpt,
        "inherited_obligations": {
            "count": 8_796,
            "encoding": "gzip-mtime-zero(canonical-json)",
            "uncompressed_bytes": len(obligations_raw),
            "uncompressed_sha256": sha256(obligations_raw),
            "object": obligation_object,
        },
    }
    store(INPUTS / "manifest.json", manifest)
    print(json.dumps({"sources": len(sources), "source_bytes": sum(row["bytes"] for row in sources), "authorities": len(retained_authorities), "wpt_entries": count, "obligations": 8_796, "objects": len(list(OBJECTS.rglob("*")))}, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
