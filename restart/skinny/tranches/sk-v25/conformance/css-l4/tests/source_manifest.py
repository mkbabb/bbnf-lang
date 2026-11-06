import importlib.util
import json
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch


SOURCE = Path(__file__).parents[1] / "src" / "source_manifest.py"
SPEC = importlib.util.spec_from_file_location("css_l4_source_manifest", SOURCE)
assert SPEC is not None and SPEC.loader is not None
manifest = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(manifest)


class SourceManifestTest(unittest.TestCase):
    def test_legacy_paths_are_explicit_and_default_is_isomorphic(self) -> None:
        self.assertEqual(str(manifest.source_path("css-color-4")), "css-color-4/Overview.bs")
        self.assertEqual(
            str(manifest.source_path("css-style-attr")),
            "css-style-attr-1/Overview.src.html",
        )

    def test_capture_binds_scope_revision_and_every_source_byte(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / "example-4" / "Overview.bs"
            source.parent.mkdir()
            source.write_bytes(b"exact source\n")
            scope = root / "scope.json"
            scope.write_text(
                json.dumps(
                    {
                        "csswg_source": {
                            "repository": "https://example.invalid/csswg.git",
                            "revision": "frozen",
                        },
                        "specs": [
                            {
                                "id": "example-4",
                                "latest_url": "https://www.w3.org/TR/example-4/",
                            }
                        ],
                    }
                )
            )
            with patch.object(manifest, "git_head", return_value="frozen"):
                actual = manifest.capture(scope, root)
            self.assertEqual(actual["specs"][0]["bytes"], 13)
            self.assertEqual(actual["specs"][0]["path"], "example-4/Overview.bs")
            self.assertEqual(actual["scope"]["sha256"], manifest.digest(scope.read_bytes()))

    def test_revision_mismatch_is_a_hard_failure(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            scope = root / "scope.json"
            scope.write_text(
                json.dumps(
                    {
                        "csswg_source": {"repository": "unused", "revision": "wanted"},
                        "specs": [],
                    }
                )
            )
            with patch.object(manifest, "git_head", return_value="other"):
                with self.assertRaisesRegex(ValueError, "differs"):
                    manifest.capture(scope, root)


if __name__ == "__main__":
    unittest.main()
