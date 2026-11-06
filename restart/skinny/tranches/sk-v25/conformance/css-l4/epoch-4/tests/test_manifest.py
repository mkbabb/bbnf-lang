from __future__ import annotations

import unittest
from typing import Any

from _support import DATA_ROOT, ROOT
import crater
import manifest
from common import encode, load, loads, sha256


def object_records(value: Any):
    if isinstance(value, dict):
        if "bytes" in value and "sha256" in value:
            yield value
        for child in value.values():
            yield from object_records(child)
    elif isinstance(value, list):
        for child in value:
            yield from object_records(child)


class ManifestTests(unittest.TestCase):
    def test_subject_has_no_cache_or_symlink(self):
        captured = manifest.capture()
        self.assertGreater(captured["file_count"], 0)

    def test_content_addressed_store_exactly_matches_input_receipts(self):
        input_manifest = load(ROOT / "inputs" / "manifest.json")
        expected = {row["sha256"] for row in object_records(input_manifest)}
        object_root = ROOT / "inputs" / "objects" / "sha256"
        actual = {
            path.parent.name + path.name
            for path in object_root.glob("*/*")
            if path.is_file()
        }
        self.assertEqual(expected, actual)
        for digest in expected:
            path = object_root / digest[:2] / digest[2:]
            self.assertEqual(digest, sha256(path.read_bytes()))

    def test_boundary_and_generation_receipt_match_stored_artifacts(self):
        boundary = DATA_ROOT / "boundary.json"
        self.assertEqual(boundary.read_bytes(), encode(crater.capture()))
        receipt_path = DATA_ROOT / "receipt.json"
        receipt = loads(receipt_path.read_bytes(), source=str(receipt_path))
        recorded = {row["path"]: row for row in receipt["artifacts"]}
        for name in ("observations.json", "boundary.json"):
            path = DATA_ROOT / name
            self.assertEqual(path.stat().st_size, recorded[name]["bytes"])
            self.assertEqual(sha256(path.read_bytes()), recorded[name]["sha256"])


if __name__ == "__main__":
    unittest.main()
