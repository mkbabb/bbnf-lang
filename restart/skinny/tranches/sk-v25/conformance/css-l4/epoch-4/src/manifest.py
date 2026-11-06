#!/usr/bin/env python3
"""Seal the final non-review subject and verify the explicit receipt exclusion."""

from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Any

from common import ROOT, encode, fail, load, sha256, store


MANIFEST = ROOT / "data" / "file-manifest.json"
VERIFICATION = ROOT / "data" / "verification.json"
CACHE_DIRECTORIES = {"__pycache__", ".pytest_cache", ".mypy_cache", ".ruff_cache"}
CACHE_SUFFIXES = {".pyc", ".pyo"}


def excluded(path: Path) -> bool:
    relative = path.relative_to(ROOT).as_posix()
    return (
        relative in {"data/file-manifest.json", "data/verification.json"}
        or path.parent == ROOT and path.name.startswith("GESTALT-") and path.suffix == ".md"
    )


def capture() -> dict[str, Any]:
    files = []
    for path in sorted(ROOT.rglob("*"), key=lambda item: item.relative_to(ROOT).as_posix()):
        relative = path.relative_to(ROOT).as_posix()
        if path.is_symlink():
            fail("SUBJECT_SYMLINK_FORBIDDEN", "owned epoch subject contains a symlink", path=relative)
        if any(part in CACHE_DIRECTORIES for part in path.relative_to(ROOT).parts) or path.suffix in CACHE_SUFFIXES or path.name == ".DS_Store":
            fail("SUBJECT_CACHE_FORBIDDEN", "owned epoch subject contains cache or host metadata", path=relative)
        if path.is_file() and not excluded(path):
            data = path.read_bytes()
            files.append({"path": relative, "bytes": len(data), "sha256": sha256(data)})
    digest_input = bytearray()
    for row in files:
        digest_input.extend(row["path"].encode() + b"\0" + bytes.fromhex(row["sha256"]) + b"\0")
    return {
        "schema_version": 1,
        "construction": "sorted(relative_path_utf8 + NUL + file_sha256_bytes + NUL)",
        "excluded": [
            "data/file-manifest.json (self-reference)",
            "data/verification.json (last receipt; binds this manifest and subject)",
            "GESTALT-*.md (future append-only review namespace)",
        ],
        "file_count": len(files),
        "total_bytes": sum(row["bytes"] for row in files),
        "subject_sha256": sha256(bytes(digest_input)),
        "files": files,
    }


def verify(expected: dict[str, Any] | None = None) -> dict[str, Any]:
    expected = load(MANIFEST) if expected is None else expected
    actual = capture()
    if encode(expected) != encode(actual):
        expected_rows = {row["path"]: row for row in expected.get("files", [])}
        actual_rows = {row["path"]: row for row in actual["files"]}
        fail(
            "FILE_MANIFEST_DRIFT",
            "owned epoch subject differs from the final manifest",
            added=sorted(set(actual_rows) - set(expected_rows)),
            removed=sorted(set(expected_rows) - set(actual_rows)),
            changed=sorted(path for path in set(actual_rows) & set(expected_rows) if actual_rows[path] != expected_rows[path]),
        )
    return actual


def validate_receipt(receipt: dict[str, Any], subject: dict[str, Any], manifest_digest: str) -> None:
    if receipt.get("subject_definition") != "file-manifest subject; manifest and this verification receipt excluded by declared self-reference rule":
        fail("VERIFICATION_SUBJECT_UNCLEAR", "verification receipt does not state its self-reference boundary")
    expected = {
        "manifest_sha256": manifest_digest,
        "subject_sha256": subject["subject_sha256"],
        "file_count": subject["file_count"],
        "total_bytes": subject["total_bytes"],
    }
    if any(receipt.get(key) != value for key, value in expected.items()):
        fail("VERIFICATION_MANIFEST_DISAGREEMENT", "final receipt and manifest subject differ", expected=expected)


def verify_final_receipt() -> dict[str, Any]:
    subject = verify()
    receipt = load(VERIFICATION)
    manifest_digest = sha256(MANIFEST.read_bytes())
    validate_receipt(receipt, subject, manifest_digest)
    return receipt


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--write", action="store_true")
    parser.add_argument("--verify-receipt", action="store_true")
    options = parser.parse_args()
    if options.write:
        store(MANIFEST, capture())
    document = verify()
    if options.verify_receipt:
        verify_final_receipt()
    print(json.dumps({key: document[key] for key in ("file_count", "total_bytes", "subject_sha256")}, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
