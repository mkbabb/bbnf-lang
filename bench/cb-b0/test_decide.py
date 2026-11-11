"""Falsifiers for the paired decision rule (sk-v27 LW2.CON-2, S11 BENCH).

PREREG §2 fixes the rule: per-pair r = ln(t_a / t_b), the Hodges-Lehmann estimate with a
BCa bootstrap CI (10,000 resamples), a one-sided Wilcoxon signed-rank p, Holm within a
family at alpha 0.01, and the 3 % minimum effect. competition U4 (a) and (b) are the two
falsifiers of that rule: an A/A cell reports nothing beyond its floor, and an injected 15 %
slowdown is detected.

Run:  python3 -m pytest -q bench/cb-b0/test_decide.py
"""
import math
import os
import sys
import unittest

import numpy as np

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import decide  # noqa: E402


def _pairs(rng, n, shift, sd=0.02):
    return list(rng.normal(shift, sd, n))


class HodgesLehmann(unittest.TestCase):
    def test_small_exact(self):
        # Walsh averages of [0, 1, 3]: 0, .5, 1.5, 1, 2, 3 -> median 1.25
        self.assertAlmostEqual(decide.hodges_lehmann(np.array([0.0, 1.0, 3.0])), 1.25)

    def test_shift_equivariant(self):
        x = np.array([0.1, -0.2, 0.05, 0.3, -0.01])
        self.assertAlmostEqual(decide.hodges_lehmann(x + 1.0), decide.hodges_lehmann(x) + 1.0)


class Rule(unittest.TestCase):
    def test_aa_reports_nothing_beyond_floor(self):  # competition U4 (a)
        rng = np.random.default_rng(1)
        c = decide.decide_cell(_pairs(rng, 240, 0.0), seed=7)
        self.assertNotIn(c["verdict"], ("WIN", "LOSS"))
        noise = decide.aa_noise_pct(c)
        floor = decide.proposed_floor_pct(noise)
        self.assertGreaterEqual(floor, 10.0)
        self.assertTrue(decide.within_floor(c, floor))

    def test_injected_slowdown_detected(self):  # competition U4 (b)
        rng = np.random.default_rng(2)
        c = decide.decide_cell(_pairs(rng, 240, math.log(1.15)), seed=7)
        self.assertEqual(c["verdict"], "LOSS")
        self.assertTrue(decide.slowdown_detected(c))

    def test_win_and_tie(self):
        rng = np.random.default_rng(3)
        self.assertEqual(decide.decide_cell(_pairs(rng, 240, math.log(0.85)), seed=7)["verdict"], "WIN")
        self.assertEqual(decide.decide_cell(_pairs(rng, 240, 0.0, sd=0.005), seed=7)["verdict"], "TIE")

    def test_holm_blocks_a_marginal_win(self):
        p = decide.holm([0.004, 0.004, 0.004])
        self.assertTrue(all(abs(a - 0.012) < 1e-12 for a in p))

    def test_too_few_pairs_never_certify(self):
        rng = np.random.default_rng(4)
        c = decide.decide_cell(_pairs(rng, 12, math.log(0.5)), seed=7, certify_min_pairs=200)
        self.assertEqual(c["verdict"], "UNDECIDED")
        self.assertIn("pairs", c["undecided_reason"])

    def test_checksum_mismatch_discards(self):
        pairs = [{"r": 0.0, "checksum_a": "x", "checksum_b": "x"}] * 99 + [{"r": 0.0, "checksum_a": "x", "checksum_b": "y"}] * 2
        kept, discarded = decide.screen_pairs(pairs)
        self.assertEqual((len(kept), discarded), (99, 2))
        self.assertTrue(decide.not_comparable(len(kept), discarded))


if __name__ == "__main__":
    unittest.main()
