from __future__ import annotations

import hashlib
import html
import unittest
from pathlib import Path

from _support import mutations, stages
import extract
from scanner import decoded_map, scan


class ExtractionTests(unittest.TestCase):
    def test_edit_reference_markup_and_nearby_prose_mutation(self):
        data = mutations()["scanner_document"].encode()
        tokens = scan(data)
        opening = next(row for row in tokens if row.kind == "start_tag" and row.name == "pre")
        _element, body, _end = extract._body(opening, tokens)
        text, mapping = decoded_map(iter(body))
        self.assertIn("<<inset-value>>", text)
        self.assertIn("| b", text)
        self.assertNotIn("| c", text)
        self.assertNotIn("nobr", text)
        self.assertEqual("NORMATIVE", extract._normativity(opening)[0])
        self.assertTrue(mapping)

    def test_false_generic_pre_corpus_is_negative(self):
        _scope, carriers, *_ = stages()
        by_spec = {}
        for row in carriers["negative_pre_fixtures"]:
            by_spec.setdefault(row["spec_id"], set()).add(row["classification"])
        for spec in ("compositing-1", "filter-effects-1", "css-fonts-4"):
            self.assertTrue(by_spec.get(spec, set()) & {"NEGATIVE_EQUATION_OR_MATH", "NEGATIVE_PROGRAM_OR_STYLESHEET", "NEGATIVE_MARKUP_EXAMPLE"})
        self.assertIn("SOURCE_NATIVE_SCHEMA_TEMPLATE", by_spec["CSS2"])

    def test_field_source_maps_are_exact_and_reversible(self):
        _scope, carriers, *_ = stages()
        carrier = next(row for row in carriers["records"] if row["kind"] == "propdef" and row.get("fields") and any(field["label"] == "value" for field in row["fields"]))
        field = next(row for row in carrier["fields"] if row["label"] == "value")
        source = field["source"]
        data = (Path("/private/tmp/bbnf-csswg.X76O73/csswg-drafts") / carrier["path"]).read_bytes()
        self.assertGreaterEqual(source["start"], carrier["body_start"])
        self.assertLessEqual(source["end"], carrier["body_end"])
        for item in source["map"]:
            raw = data[item["source_start"]:item["source_end"]]
            self.assertEqual(item["source_sha256"], hashlib.sha256(raw).hexdigest())
            decoded = html.unescape(raw.decode()) if item["transform"] == "HTML_ENTITY_DECODE" else raw.decode()
            self.assertEqual(decoded, carrier["text"][item["text_start"]:item["text_end"]])

    def test_railroad_comment_literals_and_algorithm_steps_survive(self):
        _scope, carriers, *_ = stages()
        railroad = next(row for row in carriers["records"] if row["id"] == "css-syntax-3::railroad::17")
        self.assertIn("T: <!--", railroad["text"])
        self.assertNotIn("<pre", railroad["text"])
        algorithms = [row for row in carriers["records"] if row["kind"] == "algorithm" and row["normativity"] == "NORMATIVE"]
        self.assertTrue(algorithms)
        self.assertTrue(all(row["semantic_blocks"] for row in algorithms))


if __name__ == "__main__":
    unittest.main()
