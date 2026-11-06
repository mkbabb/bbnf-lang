import importlib.util
import json
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch


SOURCE = Path(__file__).parents[1] / "src" / "syntax.py"
SPEC = importlib.util.spec_from_file_location("css_l4_syntax", SOURCE)
assert SPEC is not None and SPEC.loader is not None
syntax = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(syntax)


class SyntaxCensusTest(unittest.TestCase):
    def test_extracts_quoted_and_bare_carriers_without_eating_value_types(self) -> None:
        document = """
        <pre class=propdef>Name: color\nValue: <<color>></pre>
        <span class='prod'><dfn>&lt;thing></dfn> = <<length>> | none</span>
        """
        records = syntax.carriers(document)
        self.assertEqual([record["kind"] for record in records], ["propdef", "prod"])
        self.assertEqual(records[0]["text"], "Name: color Value: <<color>>")
        self.assertEqual(records[1]["text"], "<thing> = <<length>> | none")

    def test_balances_nested_same_name_tags(self) -> None:
        document = '<div class="prod">a<div>nested</div> = b</div>'
        records = syntax.carriers(document)
        self.assertEqual(records[0]["text"], "a nested = b")
        self.assertEqual(records[0]["raw_bytes"], len(document.encode()))

    def test_capture_rejects_any_source_byte_drift(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / "example" / "Overview.bs"
            source.parent.mkdir()
            source.write_bytes(b"<pre class=prod>a = b</pre>\n")
            manifest = root / "source-manifest.json"
            manifest.write_text(
                json.dumps(
                    {
                        "csswg": {"revision": "frozen"},
                        "specs": [
                            {
                                "id": "example",
                                "path": "example/Overview.bs",
                                "bytes": source.stat().st_size,
                                "sha256": syntax.digest(source.read_bytes()),
                            }
                        ],
                    }
                )
            )
            with patch.object(syntax, "git_head", return_value="frozen"):
                actual = syntax.capture(manifest, root)
                self.assertEqual(actual["counts"]["prod"], 1)
                source.write_bytes(b"changed")
                with self.assertRaisesRegex(ValueError, "drifted"):
                    syntax.capture(manifest, root)


if __name__ == "__main__":
    unittest.main()
