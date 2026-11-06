import importlib.util
import json
import tempfile
import unittest
from pathlib import Path


SOURCE = Path(__file__).parents[1] / "src" / "productions.py"
SPEC = importlib.util.spec_from_file_location("css_l4_productions", SOURCE)
assert SPEC is not None and SPEC.loader is not None
productions = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(productions)


class ProductionsTest(unittest.TestCase):
    def test_splits_flattened_named_definitions_and_preserves_literal_equals(self) -> None:
        parsed = productions.definitions(
            "<<one()>> = one( <<number>> ) <two> = [ '=' | <<one()>> ]"
        )
        self.assertEqual(
            parsed,
            [
                {"lhs": "<<one()>>", "rhs": "one( <<number>> )"},
                {"lhs": "<two>", "rhs": "[ '=' | <<one()>> ]"},
            ],
        )

    def test_broken_example_cannot_be_mistaken_for_a_complete_definition(self) -> None:
        self.assertEqual(
            productions.definitions("= keyword | | pattern ="),
            [{"lhs": "pattern", "rhs": ""}],
        )

    def test_capture_keeps_expression_only_and_unrecognized_carriers_red(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            census = Path(temporary) / "syntax.json"
            census.write_text(
                json.dumps(
                    {
                        "records": [
                            {
                                "kind": "prod",
                                "spec_id": "a",
                                "path": "a",
                                "line": 1,
                                "ordinal": 1,
                                "raw_sha256": "one",
                                "text": "[ <<length>> | auto ]",
                            },
                            {
                                "kind": "prod",
                                "spec_id": "b",
                                "path": "b",
                                "line": 2,
                                "ordinal": 1,
                                "raw_sha256": "two",
                                "text": "= malformed =",
                            },
                        ]
                    }
                )
            )
            actual = productions.capture(census)
            self.assertEqual(actual["counts"], {"empty-rhs": 1, "expression-only": 1})


if __name__ == "__main__":
    unittest.main()
