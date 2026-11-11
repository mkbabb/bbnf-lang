"""Falsifiers for sk-v27 LW2.CON-1 (competition U4 (c) and (d); ROWS classes; units at the source).

Run: python3 -m unittest bench/contest/test_contest.py -v
"""
import csv
import json
import os
import re
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
BENCH = os.path.dirname(HERE)
REPO = os.path.dirname(BENCH)
sys.path.insert(0, HERE)

ORACLE = "python3 bench/contest/oracle_json.py"
NODE = "node bench/contest/arms/node_json_parse.mjs"
PLANES = [f"P{i}" for i in range(1, 8)]
CLASSES = {"CHK", "TOK", "TAPE", "VAL", "CST", "GET", "TYPED", "NONE"}

# The bench sources: every tracked file that measures, drives or reports a contest row.
BENCH_SOURCES = ["bench", "skinny/crates/bbnf-bench", "skinny/xtask", "crates/core/benches",
                 "crates/core/examples", "crates/core/tests/css_o1_checkpoint_bench.rs",
                 "crates/core/tests/css_l4_w6_typed_retime.rs"]


def skip_unescape_mutant(dirpath):
    """A copy of the oracle whose string decoding skips unescaping (the mutant of U4 (c))."""
    with open(os.path.join(HERE, "oracle_json.py"), encoding="utf-8") as f:
        src = f.read()
    head = 'def unescape(raw: bytes, at: int) -> str:\n'
    assert src.count(head) == 1
    src = src.replace(head, head + '    return raw.decode("utf-8")  # MUTANT: skip unescape\n')
    path = os.path.join(dirpath, "oracle_json_skip_unescape.py")
    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    return f"python3 {path}"


def build_plane(**over):
    bp = {"lang": "rust", "toolchain": "rustc 1.99.1 (58cf8f9c3 2026-10-13)", "llvm": "21.1.2",
          "opt_level": "3", "lto": "fat", "codegen_units": "1", "panic": "abort",
          "target_cpu": "apple-m1", "allocator": "mimalloc", "matrix": "PORTABLE"}
    bp.update(over)
    return bp


class PlaneEquivalence(unittest.TestCase):
    """U4 (c): a skip-unescape mutant is refused as plane-inequivalent."""

    def test_reference_arms_agree_on_every_p4_witness(self):
        import admit
        v = admit.plane_parity("P4", "VAL", ORACLE, NODE)
        self.assertTrue(v.admitted, v.text())

    def test_skip_unescape_mutant_is_refused(self):
        import admit
        with tempfile.TemporaryDirectory() as d:
            v = admit.plane_parity("P4", "VAL", skip_unescape_mutant(d), NODE)
        self.assertFalse(v.admitted)
        refused = {re.search(r"P4/(\S+?):", r).group(1) for r in v.reasons}
        self.assertIn("esc-basic.json", refused)
        self.assertTrue(all(r.startswith("plane-inequivalent") for r in v.reasons), v.reasons)

    def test_offset_class_refuses_an_arm_without_offsets(self):
        import admit
        v = admit.plane_parity("P1", "CHK", ORACLE, NODE)
        self.assertFalse(v.admitted)
        self.assertTrue(any("reports no error offset" in r for r in v.reasons))


class BuildPlane(unittest.TestCase):
    """U4 (d): a build-plane mismatch is refused."""

    def test_equal_build_planes_are_admitted(self):
        import admit
        self.assertTrue(admit.build_parity(build_plane(), build_plane()).admitted)

    def test_target_cpu_mismatch_is_refused(self):
        import admit
        v = admit.build_parity(build_plane(), build_plane(target_cpu="native", matrix="NATIVE"))
        self.assertFalse(v.admitted)
        self.assertTrue(any(r.startswith("build-plane mismatch: target_cpu") for r in v.reasons), v.reasons)

    def test_every_i8_field_counts(self):
        import admit
        for k in admit.BUILD_FIELDS["rust"]:
            if k == "matrix":
                continue
            v = admit.build_parity(build_plane(), build_plane(**{k: "other"}))
            self.assertFalse(v.admitted, k)

    def test_a_missing_field_is_refused(self):
        import admit
        bp = build_plane()
        del bp["allocator"]
        self.assertFalse(admit.build_parity(build_plane(), bp).admitted)
        self.assertFalse(admit.build_parity(build_plane(), None).admitted)


class Cells(unittest.TestCase):
    """PREREG §7: registered cells; MISMATCH cells are refused (U4 (c), (d)); nothing unregistered is admitted."""

    def test_every_registered_cell_names_rows_that_exist(self):
        import admit
        cells, table = admit.prereg_cells(), admit.rows()
        self.assertGreater(len(cells), 0)
        for cid, c in cells.items():
            for side in ("arm (bbnf)", "arm (opp)"):
                self.assertIn(c[side], table, cid)

    def test_every_mismatch_cell_is_refused(self):
        import admit
        mm = [cid for cid, c in admit.prereg_cells().items() if c["kind"] == "MISMATCH"]
        self.assertGreaterEqual(len(mm), 4)
        bp = os.path.join(HERE, "build-planes")
        for cid in mm:
            v = admit.admit_cell(cid, os.path.join(bp, "node-default.json"), os.path.join(bp, "node-jitless.json"))
            self.assertFalse(v.admitted, cid)

    def test_build_mismatch_cell_is_refused_for_its_build_plane_alone(self):
        import admit
        bp = os.path.join(HERE, "build-planes")
        refused = admit.admit_cell("C-MM-BUILD", os.path.join(bp, "node-default.json"),
                                   os.path.join(bp, "node-jitless.json"))
        self.assertEqual(refused.reasons, ["build-plane mismatch: runtime_flags 'none' vs '--jitless'"])
        control = admit.admit_cell("C-MM-BUILD", os.path.join(bp, "node-default.json"),
                                   os.path.join(bp, "node-default.json"))
        self.assertTrue(control.admitted, control.text())

    def test_an_unregistered_cell_is_refused(self):
        import admit
        v = admit.admit_cell("C-NOT-REGISTERED")
        self.assertIn("cell C-NOT-REGISTERED is not a registered contest cell in PREREG.md §7", v.reasons)

    def test_an_edited_prereg_is_refused(self):
        import admit
        with tempfile.TemporaryDirectory() as d:
            with open(admit.PREREG, encoding="utf-8") as f:
                text = f.read()
            tampered = os.path.join(d, "PREREG.md")
            with open(tampered, "w", encoding="utf-8") as f:
                f.write(text.replace("| C-J3-AA |", "| C-J3-AB |", 1))
            saved, admit.PREREG = admit.PREREG, tampered
            try:
                v = admit.admit_cell("C-J3-AB")
            finally:
                admit.PREREG = saved
        self.assertTrue(any("does not match PREREG.md.sha256" in r for r in v.reasons), v.reasons)


class Rows(unittest.TestCase):
    def rows(self):
        with open(os.path.join(HERE, "ROWS.tsv"), encoding="utf-8", newline="") as f:
            return list(csv.DictReader([ln for ln in f if not ln.startswith("#")], delimiter="\t"))

    def test_every_row_has_a_class_and_a_checklist(self):
        rs = self.rows()
        self.assertGreater(len(rs), 0)
        for r in rs:
            self.assertIn(r["plane"], PLANES + ["-"], r["row"])
            self.assertIn(r["class"], CLASSES, r["row"])
            if r["class"] == "NONE":
                self.assertRegex(r["checklist"], r"(?i)no comparator", r["row"])
            else:
                self.assertTrue(r["checklist"].strip() and r["api"].strip(), r["row"])

    def test_every_plane_has_a_spec(self):
        for p in {r["plane"] for r in self.rows()} - {"-"}:
            self.assertTrue(os.path.exists(os.path.join(BENCH, "planes", f"{p}.md")), p)


class Units(unittest.TestCase):
    """Units are fixed at the source; the legacy predicate is retired."""

    def test_no_megabit_or_legacy_predicate_in_bench_sources(self):
        out = subprocess.run(["git", "grep", "-n", r"track1_beats_lightningcss\|mbps", "--", *BENCH_SOURCES,
                              ":(exclude)bench/contest/test_contest.py"],
                             cwd=REPO, capture_output=True, text=True)
        self.assertEqual(out.stdout, "", "\n" + out.stdout[:2000])


if __name__ == "__main__":
    unittest.main()
