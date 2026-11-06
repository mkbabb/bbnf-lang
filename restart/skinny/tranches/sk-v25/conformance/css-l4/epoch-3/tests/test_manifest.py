from __future__ import annotations

import unittest

from _support import ROOT, stages
from common import encode
import manifest
from regen import ORDER


class ManifestTests(unittest.TestCase):
    def test_file_manifest_covers_every_owned_nonreview_byte(self):
        document = manifest.verify()
        self.assertEqual(document["file_count"], len(document["files"]))
        self.assertEqual(document["total_bytes"], sum(row["bytes"] for row in document["files"]))
        self.assertFalse(any(row["path"].endswith((".pyc", ".pyo")) for row in document["files"]))

    def test_checked_generated_artifacts_match_fresh_stage_bytes(self):
        values = dict(zip(ORDER, stages(), strict=True))
        for name in ORDER:
            with self.subTest(name=name):
                self.assertEqual((ROOT / "data" / name).read_bytes(), encode(values[name]))


if __name__ == "__main__":
    unittest.main()
