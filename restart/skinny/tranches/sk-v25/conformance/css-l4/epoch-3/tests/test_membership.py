from __future__ import annotations

import unittest

from _support import stages
from common import GateError, INPUTS, WPT_PATH_OID_SHA256, WPT_REVISION, WPT_TREE, load, object_bytes
from membership import validate_authorities


class MembershipTests(unittest.TestCase):
    def test_authority_membership_and_compact_wpt_receipt(self):
        scope, *_ = stages()
        self.assertEqual([], scope["root_blockers"])
        self.assertEqual(96, len(scope["specs"]))
        self.assertTrue(all(row["publication"]["status"] == "PINNED" for row in scope["specs"]))
        self.assertEqual(8, len(scope["external_parsing_authorities"]))
        self.assertEqual(WPT_REVISION, scope["wpt"]["revision"])
        self.assertEqual(WPT_TREE, scope["wpt"]["tree_oid"])
        self.assertEqual(WPT_PATH_OID_SHA256, scope["wpt"]["path_oid_sha256"])
        self.assertNotIn("files", scope["wpt"])

    def test_dependency_occurrences_are_stateful_and_aliases_are_reviewed(self):
        scope, *_ = stages()
        identities = {(row["from_spec"], row["start"], row["key"]) for row in scope["dependencies"]}
        self.assertEqual(len(scope["dependencies"]), len(identities))
        self.assertTrue(any(row["classification"] == "COMMENT_NO_EDGE" for row in scope["dependencies"]))
        self.assertTrue(any(row["classification"] == "DELETED_HISTORY_NO_EDGE" for row in scope["dependencies"]))
        aliases = {row["alias"]: row for row in scope["selection"]["aliases"]}
        for key in ("CSS21", "CSS3-SYNTAX", "CSS3-ALIGN", "CSS3-ANIMATIONS", "CSS3-MEDIAQUERIES"):
            self.assertEqual("RESOLVED_MEMBER", aliases[key]["decision"])
            self.assertIsNotNone(aliases[key]["target_spec"])
        self.assertFalse(any(row["classification"] == "UNREVIEWED" for row in scope["dependencies"]))

    def test_retained_object_length_mutation_fails_closed(self):
        scope, *_ = stages()
        receipt = dict(scope["external_parsing_authorities"][0])
        receipt["bytes"] += 1
        with self.assertRaises(GateError) as caught:
            object_bytes(receipt)
        self.assertEqual("OBJECT_DRIFT", caught.exception.code)

    def test_authority_response_target_mutation_fails_closed(self):
        authorities = load(INPUTS / "authorities.json")
        decisions = load(INPUTS / "decisions.json")
        mutated = {**authorities, "authorities": [dict(row) for row in authorities["authorities"]]}
        mutated["authorities"][0]["response_url"] = "https://example.invalid/"
        with self.assertRaises(GateError) as caught:
            validate_authorities(mutated, decisions)
        self.assertEqual("AUTHORITY_RESPONSE_INVALID", caught.exception.code)


if __name__ == "__main__":
    unittest.main()
