from __future__ import annotations

import unittest

from _support import mutations, stages
from dialects import DialectError, parse_railroad, parse_value, parse_yacc


class DialectTests(unittest.TestCase):
    def test_every_hostile_css_vds_mutation_is_rejected(self):
        for value in mutations()["invalid_css_vds"]:
            with self.subTest(value=value), self.assertRaises(DialectError):
                parse_value(value)

    def test_strict_valid_vds_extensions_from_retained_sources(self):
        for value in (
            "[ <<number>> | none ]# {3}",
            "( <<condition>> )",
            ": [ left | right ]",
            "*mix( options? , [ value && <<percentage>>? ]# )",
            "{ <<rule-list>> }",
            "<<boolean-expr[ <<test>> ]>>",
            "<<length>>+#",
        ):
            with self.subTest(value=value):
                self.assertIsInstance(parse_value(value), dict)

    def test_every_hostile_railroad_mutation_is_rejected(self):
        for value in mutations()["invalid_railroad"]:
            with self.subTest(value=value), self.assertRaises(DialectError):
                parse_railroad(value)
        self.assertEqual("railroad", parse_railroad("Choice: 1\n  T: a\n  T: b\n")["kind"])

    def test_yacc_honors_quoted_semicolons_comments_and_real_terminators(self):
        parsed = parse_yacc("a /* name */ : ';' b | /* empty */ ;\nb: c ;")
        self.assertEqual(["a", "b"], [row["name"] for row in parsed["productions"]])
        with self.assertRaises(DialectError):
            parse_yacc("a: 'unterminated ;")

    def test_full_strict_stage_is_clean(self):
        _scope, _carriers, parsed, *_ = stages()
        self.assertEqual([], parsed["root_blockers"])
        self.assertTrue(parsed["units"])


if __name__ == "__main__":
    unittest.main()
