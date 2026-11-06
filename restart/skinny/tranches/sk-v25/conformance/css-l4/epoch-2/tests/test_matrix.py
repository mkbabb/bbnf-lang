from __future__ import annotations

import copy
import unittest

from _support import fixtures, load
import matrix


class ExternalMatrixTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.expected = fixtures()
        cls.document = load("matrix")

    def test_every_external_plane_has_a_visible_status(self) -> None:
        self.assertEqual(len(self.document["rows"]), self.expected["stage_counts"]["matrix_rows"])
        for row in self.document["rows"]:
            for plane in matrix.REQUIRED_PLANES:
                self.assertIn("status", row[plane], (row["id"], plane))
            self.assertEqual(row["terminal_state"], matrix.row_state(row))

    def test_bbnf_is_expectation_only_and_never_candidate_input(self) -> None:
        self.assertIs(self.document["schema_or_type_input_allowed"], False)
        self.assertTrue(all(row["bbnf_expectation"]["candidate_input"] is False for row in self.document["rows"]))
        self.assertTrue(all(row["bbnf_expectation"]["module"].endswith(".bbnf") for row in self.document["rows"]))

    def test_all_required_families_reach_the_matrix(self) -> None:
        families = {row["family"] for row in self.document["rows"]}
        self.assertTrue({"prod", "railroad", "legacy-grammar", "propdef", "descdef", "idl", "algorithm"} <= families)
        self.assertTrue(any("grammar_expression_id" in row for row in self.document["rows"] if row["family"] == "railroad"))

    def test_unrun_and_unproven_are_not_coverage(self) -> None:
        self.assertEqual(self.document["status_counts"], {"MAPPED": 0, "UNPROVEN": len(self.document["rows"])})
        self.assertEqual(len(self.document["blockers"]), len(self.document["rows"]))

    def test_policy_and_derived_status_mutations_fail_closed(self) -> None:
        policy = copy.deepcopy(self.document)
        policy["schema_or_type_input_allowed"] = True
        with self.assertRaisesRegex(ValueError, "explicitly reject"):
            matrix.validate(policy)
        state = copy.deepcopy(self.document)
        state["rows"][0]["terminal_state"] = "MAPPED"
        with self.assertRaisesRegex(ValueError, "non-derived"):
            matrix.validate(state)


if __name__ == "__main__":
    unittest.main()
