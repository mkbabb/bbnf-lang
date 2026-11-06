from __future__ import annotations

import copy
import unittest

from _support import fixtures, load
import grammar


class GrammarClosureTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.expected = fixtures()
        cls.scope = load("scope")
        cls.extraction = load("carriers")
        cls.document = load("grammar")

    def test_css_value_precedence_and_multipliers(self) -> None:
        ast = grammar.parse_expression("<a> || [ b && c? ] | d#")
        self.assertEqual(ast["kind"], "alternative")
        self.assertEqual(ast["items"][0]["kind"], "any-order")
        grouped = ast["items"][0]["items"][1]
        self.assertEqual(grouped["kind"], "group")
        self.assertEqual(grouped["value"]["kind"], "all-order")
        self.assertEqual(ast["items"][1]["kind"], "repeat")

    def test_value_definition_boundaries_are_structural(self) -> None:
        chunks, expression = grammar.value_definitions("<a> = <b> |\n  [ c && d ]\n<x> = fn(<y>)")
        self.assertIsNone(expression)
        self.assertEqual(chunks, [("<a>", "<b> | [ c && d ]"), ("<x>", "fn(<y>)")])

    def test_legacy_colon_grammar_is_parsed_as_definitions(self) -> None:
        chunks, expression = grammar.legacy_definitions("root\n : item* ;\nitem : IDENT ;")
        self.assertIsNone(expression)
        self.assertEqual(chunks, [("root", "item*"), ("item", "IDENT")])

    def test_railroad_dialect_preserves_live_comment_literals(self) -> None:
        ast = grammar.railroad_expression("Choice:\n  T: <!--\n  N: <ident-token>")
        self.assertEqual(ast["kind"], "railroad")
        choice = ast["items"][0]
        self.assertEqual(choice["children"][0]["value"]["value"], "<!--")
        self.assertEqual(choice["children"][1]["value"]["name"], "ident-token")

    def test_pinned_css_and_railroad_asts_exist(self) -> None:
        expected = self.expected["stage_counts"]
        self.assertEqual(len(self.document["definitions"]), expected["grammar_definitions"])
        self.assertEqual(len(self.document["entities"]), expected["grammar_entities"])
        self.assertEqual(len(self.document["expressions"]), expected["grammar_expressions"])
        railroads = [row for row in self.document["expressions"] if row["dialect"] == "railroad"]
        self.assertEqual(len(railroads), expected["railroad_expressions"])
        self.assertTrue(all(row["terminal_state"] == "PARSED_RAILROAD" and row["ast"] for row in railroads))
        names = {row["name"]: row for row in self.document["definitions"]}
        self.assertEqual(names["alpha-value"]["ast"]["kind"], "alternative")
        self.assertIn("shape-box", names)

    def test_profile_decisions_do_not_depend_on_carrier_order(self) -> None:
        reversed_extraction = copy.copy(self.extraction)
        reversed_extraction["records"] = list(reversed(self.extraction["records"]))
        replay = grammar.capture(reversed_extraction, self.scope)

        def decisions(document: dict) -> dict:
            return {
                row["name"]: (row["precedence"], row["terminal_state"], sorted(row["active_definition_ids"]))
                for row in document["profiles"]
            }

        self.assertEqual(decisions(replay), decisions(self.document))

    def test_failures_conflicts_and_unmapped_references_stay_visible(self) -> None:
        codes = {row["code"] for row in self.document["blockers"]}
        self.assertEqual(codes, {"GRAMMAR_PARSE_FAILED", "PROFILE_CONFLICT", "REFERENCE_UNMAPPED"})
        self.assertTrue(all(row["terminal_state"] for row in self.document["reference_closure"]))


if __name__ == "__main__":
    unittest.main()
