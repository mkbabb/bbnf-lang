"""Falsifier for the LW2 decision-plan seal (sk-v27 LW2.CON-2s, competition U6s).  Run: python3 -m unittest bench/test_decision_plan.py -v

The plan is sealed when its sha256 is the one RULINGS.md cites, it was sealed over the PREREG
digest of record, it lists every PREREG-registered cell exactly once, and every FROZEN floor is
max(10 %, 3 x A/A noise) of the A/A record it cites.
"""
import hashlib
import json
import os
import re
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
PLAN = os.path.join(HERE, "DECISION-PLAN.lw2.json")
PREREG = os.path.join(HERE, "PREREG.md")
RULINGS = os.path.join(ROOT, "restart/skinny/tranches/sk-v27/RULINGS.md")


def _sha256(path):
    with open(path, "rb") as f:
        return hashlib.sha256(f.read()).hexdigest()


def _plan():
    with open(PLAN, encoding="utf-8") as f:
        return json.load(f)


class DecisionPlan(unittest.TestCase):
    def test_sha256_is_cited_in_rulings(self):
        self.assertTrue(os.path.exists(PLAN), "bench/DECISION-PLAN.lw2.json is absent (LW2.CON-2s not sealed)")
        with open(RULINGS, encoding="utf-8") as f:
            cited = re.findall(r"DECISION-PLAN\.lw2\.json[^\n]*?sha256 `([0-9a-f]{64})`", f.read())
        self.assertEqual(cited, [_sha256(PLAN)], "RULINGS.md must cite the plan's sha256 exactly once")

    def test_sealed_over_the_prereg_of_record(self):
        self.assertEqual(_plan()["prereg"]["sha256"], _sha256(PREREG))

    def test_every_registered_cell_once(self):
        with open(PREREG, encoding="utf-8") as f:
            registered = re.findall(r"^\| (C-[A-Z0-9-]+) \| ", f.read(), re.M)
        planned = [c["cell"] for c in _plan()["cells"]]
        self.assertEqual(sorted(planned), sorted(registered))
        self.assertEqual(len(planned), len(set(planned)))

    def test_frozen_floors_follow_the_rule(self):
        frozen = [g for g in _plan()["floors"] if g["state"] == "FROZEN"]
        self.assertTrue(frozen)
        for g in frozen:
            d = g["derivation"]
            lo, hi = d["aa_ci99_pct"]
            self.assertAlmostEqual(d["aa_noise_pct"], max(abs(lo), abs(hi)), places=3)
            self.assertEqual(g["floor_pct"], max(10.0, round(3 * d["aa_noise_pct"], 3)))
            self.assertEqual(d["u4a"], "PASS")
            self.assertEqual(d["u4b"], "PASS")
            for rec in d["records"]:
                self.assertTrue(os.path.exists(os.path.join(ROOT, rec)), rec)

    def test_every_goal_has_one_row(self):
        goals = [g["goal"] for g in _plan()["floors"]]
        self.assertEqual(len(goals), len(set(goals)))
        for g in _plan()["floors"]:
            self.assertIn(g["state"], {"FROZEN", "NOISY-UNFROZEN", "BLOCKED", "n/a"})
            if g["state"] != "FROZEN":
                self.assertIsNone(g["floor_pct"])
                self.assertTrue(g["reason"])


if __name__ == "__main__":
    unittest.main()
