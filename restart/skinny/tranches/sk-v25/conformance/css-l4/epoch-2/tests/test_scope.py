from __future__ import annotations

import hashlib
import unittest

from _support import fixtures, load


class CanonicalScopeTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.expected = fixtures()
        cls.scope = load("scope")

    def test_epoch_one_bank_is_hash_bound(self) -> None:
        receipts = {row["path"]: row["sha256"] for row in self.scope["epoch_1_bank"]}
        self.assertEqual(receipts["GESTALT-1-ADJUDICATION.md"], self.expected["bank"]["adjudication_sha256"])
        self.assertEqual(receipts["data/scope.json"], self.expected["bank"]["scope_sha256"])

    def test_canonical_specs_are_unique_and_source_pinned(self) -> None:
        specs = self.scope["specs"]
        expected = self.expected["scope"]
        self.assertEqual(len(specs), expected["spec_count"])
        self.assertEqual(len({row["id"] for row in specs}), len(specs))
        self.assertEqual(len({row["path"] for row in specs}), len(specs))
        self.assertTrue(all(row["bytes"] > 0 and len(row["sha256"]) == 64 for row in specs))

    def test_normative_dependencies_have_explicit_terminal_states(self) -> None:
        dependencies = self.scope["dependencies"]
        self.assertEqual(len(dependencies), self.expected["scope"]["dependency_count"])
        self.assertTrue(all(row["classification"] and row["terminal_state"] for row in dependencies))
        parsing = {row["key"] for row in dependencies if row["classification"] == "EXTERNAL_PARSING_AUTHORITY"}
        self.assertEqual(parsing, {"DOM", "ENCODING", "FETCH", "HTML", "INFRA", "URL", "WEBIDL", "XML-NAMES"})
        self.assertTrue(all(row["terminal_state"] == "UNPROVEN" for row in dependencies if row["key"] in parsing))

    def test_publication_block_is_exact_and_not_hidden(self) -> None:
        observed = self.scope["publication_input"]
        self.assertEqual(observed["fetch_policy"], "ONE_REQUEST_PER_URL_NO_RETRY_STOP_ON_FIRST_FAILURE")
        self.assertEqual(observed["requests_made"], 123)
        failure = observed["failures"][0]
        for key, value in self.expected["scope"]["publication_failure"].items():
            self.assertEqual(failure[key], value)
        self.assertIn("PUBLICATION_FETCH_BLOCKED", {row["code"] for row in self.scope["blockers"]})

    def test_wpt_commit_tree_and_full_manifest_are_bound(self) -> None:
        expected = self.expected["scope"]["wpt"]
        observed = self.scope["wpt"]
        for key, value in expected.items():
            self.assertEqual(observed[key], value)
        manifest = observed["manifest"]
        self.assertEqual(len(manifest), observed["file_count"])
        self.assertEqual([row["path"] for row in manifest], sorted(row["path"] for row in manifest))
        digest = hashlib.sha256(
            "".join(f"{row['path']}\0{row['blob_oid']}\0" for row in manifest).encode()
        ).hexdigest()
        self.assertEqual(digest, observed["tree_manifest_sha256"])
        routes = [route for spec in self.scope["specs"] for route in spec["wpt_routes"] if "route" in route]
        self.assertEqual(len(routes), observed["route_count"])
        self.assertTrue(all(route["terminal_state"] in {"PINNED", "UNMAPPED"} for route in routes))
        self.assertTrue(all(len(route["blob_oid"]) == 40 for route in routes if route["terminal_state"] == "PINNED"))


if __name__ == "__main__":
    unittest.main()
