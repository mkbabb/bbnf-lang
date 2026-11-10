"""Falsifier for the pre-registration skeleton (sk-v27 LW1.CON-5).  Run: python3 -m unittest bench/test_prereg.py -v"""
import hashlib
import os
import re
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
PREREG = os.path.join(HERE, "PREREG.md")
DIGEST = os.path.join(HERE, "PREREG.md.sha256")


def _text():
    with open(PREREG, encoding="utf-8") as f:
        return f.read()


class Prereg(unittest.TestCase):
    def test_exists_and_is_hashed(self):
        with open(PREREG, "rb") as f:
            got = hashlib.sha256(f.read()).hexdigest()
        with open(DIGEST) as f:
            want = f.read().split()[0]
        self.assertEqual(got, want, "PREREG.md changed without its digest (an edit after a hash is an addendum)")

    def test_states_the_decision_rule(self):
        t = _text()
        self.assertRegex(t, r"α\s*=\s*0\.01")
        self.assertRegex(t, r"Holm")
        self.assertRegex(t, r"(?i)minimum effect[^\n]*3\s*%")
        self.assertRegex(t, r"≥\s*200 pairs")
        self.assertRegex(t, r"≥\s*10 reps")

    def test_names_families_and_matrices(self):
        t = _text()
        fams = re.findall(r"^\| (F-[A-Z0-9-]+) \|", t, re.M)
        self.assertGreaterEqual(len(fams), 5, fams)
        self.assertEqual(len(fams), len(set(fams)), "a family is declared twice")
        self.assertIn("PORTABLE", t)
        self.assertIn("NATIVE", t)

    def test_root_trials_are_registered(self):
        # LW2.COMP-1: both trials, their cells, deadline and verdict rule are written before Phase 3.
        t = _text()
        cells = re.findall(r"^\| (C-(EG|CSP)-[A-Z0-9-]+) \| (F-TRIAL-(?:EG|CSP)) \|", t, re.M)
        self.assertEqual(len(cells), 14, cells)
        self.assertEqual(len({c[0] for c in cells}), 14, "a cell is registered twice")
        for c, trial, fam in cells:
            self.assertEqual(fam, f"F-TRIAL-{trial}", c)
        self.assertRegex(t, r"72 h per trial")
        self.assertRegex(t, r"trial/lw2-egraph-loser")
        self.assertRegex(t, r"trial/lw2-csp-loser")
        self.assertRegex(t, r"(?s)LOSS and UNDECIDED keep the simpler arm")

    def test_noisy_never_certifies(self):
        self.assertRegex(_text(), r"NOISY[^\n]*never certif")


if __name__ == "__main__":
    unittest.main()
