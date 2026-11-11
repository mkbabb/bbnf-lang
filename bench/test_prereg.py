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

    def test_consumer_cells_are_registered(self):
        # LW2.USE-1: G-S1's 14 cells (7 value.js entries x accepted/rejected), their ABBA order, seed and corpus
        # hash, the equal-work admission (USE-2), and G-S2's three tiers, written before Phase 3.
        t = _text()
        entries = {"parseCssColor", "parseCssScalar", "parseCssValue", "parseCssValues",
                   "parseKeyframeSelector", "parseTimingFunction", "parseStylesheet"}
        cells = re.findall(r"^\| (C-S1-(ACC|REJ)-[A-Z]+) \| F-VALUE-S1 \| (\w+) \| (acc|rej) \|.*\| PORTABLE \|$", t, re.M)
        self.assertEqual(len(cells), 14, cells)
        self.assertEqual(len({c[0] for c in cells}), 14, "a G-S1 cell is registered twice")
        self.assertEqual({(c[2], c[3]) for c in cells}, {(e, k) for e in entries for k in ("acc", "rej")})
        for cid, k, _e, klass in cells:
            self.assertEqual(k.lower(), klass, cid)
        self.assertRegex(t, r"ABBA[^\n]*BAAB")
        self.assertRegex(t, r"seed `20261109`")
        self.assertIn("2639ac5779ca711beca73e7127c477cbb866d729bc31d692c2f1e2095552fc76", t)
        self.assertIn("4038dce5ac3278eb10441daca1e55d5c4f83cc70", t)
        self.assertRegex(t, r"upper bound[^\n]*< 1\.03")
        self.assertRegex(t, r"NOT-COMPARABLE[^\n]*EQUAL-WORK")
        tiers = set(re.findall(r"^\| C-S2-[A-Z0-9-]+ \| F-VALUE-S2 \| (TOKENIZE|COLOR→RGBA|FULL-VALUE) \|", t, re.M))
        self.assertEqual(tiers, {"TOKENIZE", "COLOR→RGBA", "FULL-VALUE"})
        self.assertRegex(t, r"FULL-VALUE[^\n]*unranked")

    def test_rx4_paired_cure_cells_are_registered(self):
        # LW2.SEAM S12-P4-6 (ACCEPTED IN PART): RX-4 removes FuseAltRegexBranches from the Rust pipeline, which
        # changes hot-path generated code; its parent-vs-cure cells are registered before Phase 3.
        t = _text()
        cells = re.findall(r"^\| (C-RX4-[A-Z0-9-]+) \| F-CURE-RX4 \| [^|]+ \| ([^|]+) \| `63054dba0` \| `88b60839f` \|.*\| PORTABLE \|$", t, re.M)
        self.assertEqual({c[0] for c in cells},
                         {"C-RX4-CSSL4-BOOTSTRAP", "C-RX4-CSSL4-TAILWIND", "C-RX4-PRETTY-BOOTSTRAP"}, cells)
        self.assertRegex(t, r"F-CURE-RX4[^\n]*not a contest")
        self.assertRegex(t, r"(?s)RX-4[^\n]*correctness cure[^\n]*LOSS")

    def test_perf3_paired_cure_cells_are_registered(self):
        # LW2.SEAM S12-P4-6 (ACCEPTED IN PART): PERF-3 changes the span parser's hot-path dispatch inlining
        # (parse-that 92ff470); its parent-vs-cure cells are registered before Phase 3.
        t = _text()
        cells = re.findall(r"^\| (C-PERF3-[A-Z0-9-]+) \| F-CURE-PERF3 \| [^|]+ \| ([^|]+) \| `ad0dc8a` \| `92ff470` \|.*\| PORTABLE \|$", t, re.M)
        self.assertEqual({c[0] for c in cells},
                         {"C-PERF3-CSS-BOOTSTRAP", "C-PERF3-CSS-TAILWIND", "C-PERF3-JSON-TWITTER"}, cells)
        self.assertRegex(t, r"F-CURE-PERF3[^\n]*not a contest")
        self.assertRegex(t, r"(?s)PERF-3[^\n]*correctness cure[^\n]*LOSS")

    def test_or7_parity_cells_are_registered(self):
        # RULINGS 2026-11-10b OR-7 (Route A PROVISIONAL): its closing cells, G-R2 leaf throughput and G-P1 emitted
        # code, Route A against Route B in family F-REGEX, are registered before Phase 3, with the close/reopen rule.
        t = _text()
        cells = re.findall(r"^\| (C-OR7-(GR2|GP1)-[A-Z0-9-]+) \| F-REGEX \| (G-R2|G-P1) \|.*\| PORTABLE \|$", t, re.M)
        self.assertEqual({c[0] for c in cells},
                         {"C-OR7-GR2-FIND", "C-OR7-GR2-ANCHORED", "C-OR7-GP1-CSSL4-BOOTSTRAP",
                          "C-OR7-GP1-CSSL4-TAILWIND", "C-OR7-GP1-JSON-TWITTER"}, cells)
        for c, short, goal in cells:
            self.assertEqual(goal, "G-" + short[1:], c)
        for c in re.findall(r"^\| C-OR7-GR2-[A-Z]+ \|.*$", t, re.M):
            self.assertIn("| `5999f9c` | `2d01be6` |", c)
        self.assertRegex(t, r"(?s)OR-7 closes when every cell reads WIN or TIE for Route A")
        self.assertRegex(t, r"LOSS beyond the frozen G-R2 or G-P1 floor[^\n]*reopens OR-7")
        self.assertRegex(t, r"(?m)^\| F-REGEX \|[^\n]*§11")

    def test_noisy_never_certifies(self):
        self.assertRegex(_text(), r"NOISY[^\n]*never certif")


if __name__ == "__main__":
    unittest.main()
