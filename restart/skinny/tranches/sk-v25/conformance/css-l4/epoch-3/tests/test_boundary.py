from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from _support import mutations, stages
import boundary
import dialects
from common import GateError, require_toolchain


class BoundaryTests(unittest.TestCase):
    def test_no_candidate_is_fabricated_and_toolchain_is_exact(self):
        *_prefix, bounded = stages()
        self.assertEqual("NO_CANDIDATE_PRESENT", bounded["status"])
        self.assertTrue(bounded["observation_only"])
        self.assertEqual({"lock-constitution", "governing-wave", "binding-successor-contract"}, {row["role"] for row in bounded["governing_documents"]})
        self.assertEqual("3.14.5", require_toolchain()["version"])

    def test_candidate_crater_accepts_bbnf_and_rejects_oracle_authority(self):
        contract = boundary.load(boundary.INPUTS / "future-candidate-contract.json")
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "src").mkdir()
            source = root / "src" / "main.rs"
            source.write_text('const GRAMMAR: &str = "grammar/css/l4/core.bbnf";')
            self.assertEqual("PASS", boundary.inspect_candidate(root, contract)["status"])
            source.write_text('const OBSERVATIONS: &str = "conformance/css-l4/epoch-3/data/observations.json";')
            self.assertEqual("PASS", boundary.inspect_candidate(root, contract)["status"])
            source.write_text(mutations()["forbidden_candidate_line"])
            result = boundary.inspect_candidate(root, contract)
            self.assertEqual("FAIL", result["status"])
            self.assertTrue(any(row["code"] == "FORBIDDEN_ORACLE_AUTHORITY_IMPORT" for row in result["violations"]))
            source.write_text(mutations()["unallowlisted_candidate_line"])
            result = boundary.inspect_candidate(root, contract)
            self.assertTrue(any(row["code"] == "INPUT_NOT_ALLOWLISTED" for row in result["violations"]))

    def test_blocked_stage_cannot_flow_downstream(self):
        _scope, carriers, *_ = stages()
        mutated = dict(carriers)
        mutated["root_blockers"] = [{"code": "MUTATED_CLASSIFICATION"}]
        with self.assertRaises(GateError) as caught:
            dialects.capture(mutated)
        self.assertEqual("PREREQUISITE_BLOCKED", caught.exception.code)

    def test_every_adjudicated_critic_finding_has_an_independent_probe(self):
        found = set(mutations()["critic_finding_coverage"])
        expected = {f"A-F{number:02d}" for number in range(1, 16)} | {f"B-{number:02d}" for number in range(1, 11)}
        self.assertEqual(expected, found)


if __name__ == "__main__":
    unittest.main()
