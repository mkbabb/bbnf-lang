from __future__ import annotations

import copy
import hashlib
import tempfile
import unittest
from pathlib import Path

import assay
from common import GateError, encode, git_tree_oid, sha256
from parsers import project_markup
from mutations import run_all
from source import denominator


class Epoch5AssayTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.inputs, cls.values = assay.build_base()
        cls.inputs["_independent_denominator"] = denominator(cls.inputs["sources"], cls.inputs["policy"], cls.inputs["obligations"])
        cls.artifact_bytes = {name: encode(value) for name, value in cls.values.items()}
        cls.values["runtime.json"] = {"test_fixture": "runtime verified by launcher before import"}
        cls.artifact_bytes["runtime.json"] = encode(cls.values["runtime.json"])
        cls.document = assay.observations(cls.artifact_bytes, cls.inputs)
        cls.resolved = {
            "inputs/contract.json": (Path(__file__).resolve().parents[1] / "inputs" / "contract.json").read_bytes(),
            "epoch4/inputs/manifest.json": (Path(__file__).resolve().parents[2] / "epoch-4" / "inputs" / "manifest.json").read_bytes(),
            **cls.artifact_bytes,
        }

    def test_integrated_closed_document(self) -> None:
        assay.validate_document(self.document, self.resolved, self.values, self.inputs)

    def test_source_preserving_css_span(self) -> None:
        raw = b"<dfn><<length>></dfn> | <<percentage>> | auto | inherit"
        projection = project_markup(raw)
        self.assertEqual(projection.text, "<<length>> | <<percentage>> | auto | inherit")
        fragments = projection.fragments(0, len(projection.text))
        self.assertEqual(b"".join(raw[left:right] for left, right in fragments), b"<<length>> | <<percentage>> | auto | inherit")

    def test_atomic_denominator_and_reverse_links(self) -> None:
        value = self.values["denominator.json"]
        assay.validate_denominator(value, self.inputs)
        self.assertEqual(value["unit_count"], len(value["units"]))
        self.assertGreater(value["reverse_link_count"], 0)
        self.assertTrue(all("fragments" in row and "reverse_links" in row for row in value["units"]))

    def test_wpt_prefix_blobs_are_not_tests(self) -> None:
        value = self.values["wpt-prefix-blobs.json"]
        assay.validate_wpt(value, self.inputs)
        statuses = {(kind, status): 0 for kind, status in (("WPT", "DECLARED"), ("WPT", "NO_DIRECT_TESTS"), ("WPT", "EXCLUDED"), ("WPT_REST", "DECLARED"))}
        for row in value["directives"]:
            statuses[(row["kind"], row["status"])] += 1
        self.assertEqual(statuses, {("WPT", "DECLARED"): 1905, ("WPT", "NO_DIRECT_TESTS"): 194, ("WPT", "EXCLUDED"): 6, ("WPT_REST", "DECLARED"): 1})
        self.assertEqual(value["path_blob_count"], 16_147)
        self.assertEqual(value["wpt_rest_prefix_blob_count"], 273)
        self.assertEqual(value["unmapped_declared_path_count"], 526)
        self.assertTrue(all(len(source) == 5 and len(source[3]) == 64 for row in value["rows"] for source in row["source_directives"]))
        self.assertEqual(value["wpt_test_count"], 0)
        self.assertGreater(value["classification_counts"].get("METADATA", 0), 0)

    def test_git_directory_sort_and_pinned_genesis(self) -> None:
        empty_tree = hashlib.sha1(b"tree 0\0").hexdigest()
        empty_blob = hashlib.sha1(b"blob 0\0").hexdigest()
        self.assertEqual(git_tree_oid([("040000", "foo", empty_tree), ("100644", "foo.ext", empty_blob)]), "9c08bd206edb376c95b9ca4f7a8d7d08376c56e9")
        assay.validate_genesis(self.values["genesis.json"], self.inputs)

    def test_historical_and_route_closure_counterexamples(self) -> None:
        result = run_all(self.document, self.values, self.artifact_bytes, self.inputs)
        self.assertEqual(result["historical_count"], 20)
        self.assertEqual(result["supplemental_count"], 3)
        self.assertEqual(result["executed_count"], 23)
        self.assertEqual(result["rejected_count"], 23)


if __name__ == "__main__":
    unittest.main()
