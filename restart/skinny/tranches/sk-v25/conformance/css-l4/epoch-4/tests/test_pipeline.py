from __future__ import annotations

import json
import unittest

from _support import DATA_ROOT, ROOT, observations
import pipeline
from common import encode


class PipelineTests(unittest.TestCase):
    def test_checked_observation_artifact_is_exact_and_valid(self):
        document = pipeline.build()
        pipeline.validate_document(document)
        self.assertEqual((DATA_ROOT / "observations.json").read_bytes(), encode(document))

    def test_claim_states_are_disjoint_and_honestly_red(self):
        boundary = observations()["claim_boundary"]
        self.assertEqual("VALID", boundary["byte_replay"]["status"])
        self.assertEqual("BLOCKED", boundary["local_semantic"]["status"])
        self.assertEqual("RED_OPEN", boundary["downstream"]["terminal"])
        self.assertEqual("ABSENT_RED", boundary["candidate_product"]["status"])
        self.assertEqual(8_796, boundary["downstream"]["semantic_obligation_count"])
        self.assertEqual(0, boundary["candidate_product"]["candidate_count"])
        self.assertEqual("UNRUN", boundary["downstream"]["default_planes"]["wpt_execution"])

    def test_all_positive_local_states_carry_evidence(self):
        replay = observations()["claim_boundary"]["byte_replay"]
        self.assertTrue(replay["evidence"])
        self.assertTrue(all(row["identity"] and len(row["sha256"]) == 64 for row in replay["evidence"]))


if __name__ == "__main__":
    unittest.main()
