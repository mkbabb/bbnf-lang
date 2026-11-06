from __future__ import annotations

import hashlib
import tempfile
import unittest
from pathlib import Path

from _support import ROOT, fixtures, load
from extract import parse_tag, scan_document


class SourceExtractionTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.expected = fixtures()
        cls.document = load("carriers")
        cls.records = cls.document["records"]

    def record_at(self, spec_id: str, line: int) -> dict:
        matches = [row for row in self.records if row["spec_id"] == spec_id and row["line"] == line]
        self.assertEqual(len(matches), 1, (spec_id, line, matches))
        return matches[0]

    def test_independent_required_carriers(self) -> None:
        for expected in self.expected["required_carriers"]:
            row = self.record_at(expected["spec_id"], expected["line"])
            for key in ("kind", "normativity"):
                self.assertEqual(row[key], expected[key], expected)
            if "text" in expected:
                self.assertEqual(row["normalized_text"], expected["text"])

    def test_commented_openings_are_excluded(self) -> None:
        locations = {(row["spec_id"], row["line"]) for row in self.records}
        for expected in self.expected["excluded_comments"]:
            self.assertNotIn((expected["spec_id"], expected["line"]), locations)

    def test_raw_text_and_webidl_generics_survive(self) -> None:
        for expected in self.expected["required_substrings"]:
            row = self.record_at(expected["spec_id"], expected["line"])
            self.assertIn(expected["text"], row["text"])

    def test_quoted_greater_than_does_not_close_opening_tag(self) -> None:
        data = b'<pre class="prod" data-example=">">x</pre>'
        tag = parse_tag(data, 0)
        self.assertIsNotNone(tag)
        self.assertEqual(tag.end, data.rindex(b"x"))
        self.assertEqual(tag.attributes["data-example"], ">")

    def test_comment_and_raw_text_mutation(self) -> None:
        payload = (
            b'<!-- <pre class="prod">bad = nope</pre> -->\n'
            b'<pre class="prod"><also-good> = x <!-- ignored --> | y</pre>\n'
            b'<xmp class="prod"><good> = <length></xmp>\n'
        )
        with tempfile.TemporaryDirectory(dir=ROOT) as directory:
            path = Path(directory) / "Overview.bs"
            path.write_bytes(payload)
            spec = {
                "id": "mutation",
                "path": path.name,
                "bytes": len(payload),
                "sha256": hashlib.sha256(payload).hexdigest(),
            }
            records = scan_document(spec, path.parent)
            self.assertEqual(len(records), 2)
            self.assertEqual(records[0]["normalized_text"], "<also-good> = x | y")
            self.assertEqual(records[1]["normalized_text"], "<good> = <length>")
            mutated = dict(spec, sha256="0" * 64)
            with self.assertRaisesRegex(ValueError, "source pin drifted"):
                scan_document(mutated, path.parent)

    def test_every_scope_spec_has_a_terminal_carrier_decision(self) -> None:
        self.assertEqual(self.document["blockers"], [])
        decisions = {row["spec_id"]: row["terminal_state"] for row in self.document["no_grammar_decisions"]}
        self.assertEqual(decisions["css-2026"], "ADJUDICATED_SCOPE_ONLY")
        self.assertEqual(decisions["css-print"], "ADJUDICATED_OBSOLETE_PROFILE_NO_OWN_GRAMMAR")


if __name__ == "__main__":
    unittest.main()
