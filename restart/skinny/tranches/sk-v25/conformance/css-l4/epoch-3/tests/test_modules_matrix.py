from __future__ import annotations

import copy
import unittest
from pathlib import Path

from _support import REPOSITORY, stages
from common import DEFAULT_CSSWG, GateError, encode
import matrix
import modules


class ModuleMatrixTests(unittest.TestCase):
    def test_total_closure_includes_entity_occurrences_and_deletion_fails(self):
        _scope, _carriers, _parsed, closed, *_ = stages()
        modules.validate(closed)
        self.assertTrue(any(row["source_kind"] == "entity-field" for row in closed["reference_closure"]))
        mutated = dict(closed)
        mutated["reference_closure"] = closed["reference_closure"][:-1]
        with self.assertRaises(GateError) as caught:
            modules.validate(mutated)
        self.assertEqual("CLOSURE_NOT_TOTAL", caught.exception.code)

    def test_modules_are_source_qualified_with_extensions_and_no_scalar_priority(self):
        _scope, _carriers, _parsed, closed, *_ = stages()
        self.assertFalse(closed["profile"]["scalar_priority"])
        self.assertEqual("NO_IMPLICIT_LEVEL_SUPERSESSION", closed["supersession"]["decision"])
        self.assertTrue(closed["conflicts"])
        self.assertTrue(all(row["decision"] == "COEXIST_SOURCE_QUALIFIED" for row in closed["conflicts"]))
        self.assertTrue(closed["extensions"])
        self.assertTrue(any(row["name"] == "globalCompositeOperation" for row in closed["entities"]))

    def test_matrix_unknown_status_and_owner_drift_fail_closed(self):
        _scope, _carriers, _parsed, closed, observed, _bounded = stages()
        row = copy.deepcopy(observed["rows"][0])
        source = next(value for value in observed["sources"] if value["id"] == row["source_id"])
        bbnf = next(value for value in observed["bbnf_expectations"] if value["id"] == row["bbnf_expectation_id"])
        wpt = next(value for value in observed["wpt_ownership"] if value["id"] == row["wpt_ownership_id"])
        decision = next(value for value in observed["downstream_decisions"] if value["id"] == row["terminal_downstream_id"])
        document = {"rows": [row], "sources": [source], "bbnf_expectations": [bbnf], "wpt_ownership": [wpt], "downstream_decisions": [decision]}
        row["planes"]["fixtures"] = "TYPO_COUNTS_AS_TERMINAL"
        with self.assertRaises(GateError) as caught:
            matrix.validate(document)
        self.assertEqual("MATRIX_STATUS_UNKNOWN", caught.exception.code)
        row["planes"]["fixtures"] = "UNMAPPED"
        row["responsible_module"] = "fabricated.owner"
        with self.assertRaises(GateError) as caught:
            matrix.validate(document)
        self.assertEqual("MATRIX_OWNER_FABRICATED", caught.exception.code)

    def test_observed_bbnf_expectations_and_anchor_routing_are_compact(self):
        _scope, carriers, _parsed, _closed, observed, _bounded = stages()
        for expectation in observed["bbnf_expectations"]:
            self.assertEqual("UNMAPPED", expectation["status"])
            self.assertTrue(all((REPOSITORY / path).is_file() for path in expectation["observed_rule_name_matches"]))
            self.assertEqual("NAME_MATCH_OBSERVATION_ONLY_NOT_A_SEMANTIC_BINDING_TYPE_FACT_OR_OWNER", expectation["authority"])
        carrier = next(row for row in carriers["records"] if row.get("anchor"))
        relocated = {**carrier, "start": carrier["start"] + 100000, "end": carrier["end"] + 100000}
        self.assertEqual(matrix.wpt_owner_identity(carrier), matrix.wpt_owner_identity(relocated))
        self.assertLess(len(encode(observed)), 12_000_000)
        self.assertEqual(98, len(observed["wpt_ownership"]))

    def test_every_row_is_source_exact_owned_and_terminally_routed(self):
        _scope, _carriers, _parsed, closed, observed, _bounded = stages()
        matrix.validate(observed, closed, DEFAULT_CSSWG)
        self.assertEqual("PASS", observed["author_gate"])
        self.assertEqual("RED", observed["semantic_portfolio"])
        self.assertEqual(len(observed["rows"]), sum(row["planes"]["bbnf"] == "UNMAPPED" for row in observed["rows"]))
        self.assertTrue(all(row["planes"]["owner"] == "UNMAPPED" and row["responsible_module"] is None for row in observed["rows"]))
        self.assertEqual({"Lightning CSS", "sonic-rs"}, {row["control"] for row in observed["downstream_control_gates"]["controls"]})
        self.assertTrue(all(row["selection_status"] == "UNRUN" and row["correctness_status"] == "UNPROVEN" and row["performance_status"] == "UNRUN" for row in observed["downstream_control_gates"]["controls"]))
        decisions = {row["id"]: row for row in observed["downstream_decisions"]}
        self.assertTrue(all(decisions[row["terminal_downstream_id"]]["status"] == "ROUTED_VT_W1" for row in observed["rows"]))
        mutated = dict(observed)
        mutated["rows"] = observed["rows"][:-1]
        with self.assertRaises(GateError) as caught:
            matrix.validate(mutated, closed)
        self.assertEqual("MATRIX_ROWS_NOT_TOTAL", caught.exception.code)
        mutated = dict(observed)
        mutated["sources"] = [dict(row) for row in observed["sources"]]
        mutated["sources"][0]["sha256"] = "0" * 64
        with self.assertRaises(GateError) as caught:
            matrix.validate(mutated, closed, DEFAULT_CSSWG)
        self.assertEqual("MATRIX_SOURCE_DRIFT", caught.exception.code)


if __name__ == "__main__":
    unittest.main()
