import importlib.util
import json
import tempfile
import unittest
from pathlib import Path


SOURCE = Path(__file__).parents[1] / "src" / "rows.py"
SPEC = importlib.util.spec_from_file_location("css_l4_rows", SOURCE)
assert SPEC is not None and SPEC.loader is not None
rows = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(rows)


class DefinitionRowsTest(unittest.TestCase):
    def test_parses_complete_and_partial_rows_case_insensitively(self) -> None:
        prefix, fields, duplicates = rows.fields(
            "Name: width Value: auto | <<length>> Initial: auto Animation Type: discrete"
        )
        self.assertEqual(prefix, "")
        self.assertEqual(fields["value"], "auto | <<length>>")
        self.assertEqual(fields["animation_type"], "discrete")
        self.assertEqual(duplicates, [])
        self.assertEqual(
            rows.fields("Name: display New Values: grid | inline-grid")[1]["new_values"],
            "grid | inline-grid",
        )
        _, repeated, duplicates = rows.fields("Name: grid Value: none | Value: auto-flow")
        self.assertEqual(repeated["value"], "none | auto-flow")
        self.assertEqual(duplicates, ["value"])

    def test_capture_preserves_anomalies_instead_of_guessing(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            census = Path(temporary) / "syntax.json"
            census.write_text(
                json.dumps(
                    {
                        "records": [
                            {
                                "kind": "propdef",
                                "spec_id": "example",
                                "path": "example/Overview.bs",
                                "line": 1,
                                "ordinal": 1,
                                "raw_sha256": "raw",
                                "text": "unnamed Value: a | b Initial: a",
                            },
                            {"kind": "prod"},
                        ]
                    }
                )
            )
            actual = rows.capture(census)
            self.assertEqual(actual["counts"], {"anomalous": 1})
            self.assertEqual(actual["rows"][0]["prefix"], "unnamed")


if __name__ == "__main__":
    unittest.main()
