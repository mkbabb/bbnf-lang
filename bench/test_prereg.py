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

    def test_noisy_never_certifies(self):
        self.assertRegex(_text(), r"NOISY[^\n]*never certif")


if __name__ == "__main__":
    unittest.main()
