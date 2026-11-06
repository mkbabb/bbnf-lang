from __future__ import annotations

import unittest

from mutations import run_all


class MutationTests(unittest.TestCase):
    def test_all_twenty_adjudicated_findings_have_executable_mutations(self):
        results = run_all()
        self.assertEqual(20, len(results))
        self.assertEqual(20, len({row["id"] for row in results}))


if __name__ == "__main__":
    unittest.main()
