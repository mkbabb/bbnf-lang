"""Falsifiers for the contest pins (sk-v27 LW1.CON-2).  Run: python3 -m unittest discover -s bench/pins -v"""
import os
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import pins  # noqa: E402

V2 = os.path.join(HERE, "..", "W0-BENCH-PINS.tsv")
HEAD = "\t".join(pins.COLUMNS)
SHA256 = "a" * 64


def _row(**kw):
    base = {c: "-" for c in pins.COLUMNS}
    base.update(kind="crate", name="x", pin="1.0.0", checksum=SHA256, published_at="2026-10-01T00:00:00Z",
                resolved_on="2026-11-05")
    base.update(kw)
    return "\t".join(base[c] for c in pins.COLUMNS)


def _check(*rows):
    with tempfile.NamedTemporaryFile("w", suffix=".tsv", delete=False) as f:
        f.write("# test\n" + HEAD + "\n" + "\n".join(rows) + "\n")
    try:
        return pins.check(f.name)
    finally:
        os.unlink(f.name)


class Pins(unittest.TestCase):
    def test_the_committed_v2_table_holds_the_rule(self):
        self.assertEqual(pins.check(V2), [])

    def test_the_v1_table_cannot_show_the_rule(self):
        errs = pins.check(pins.V1)
        self.assertTrue(any("published_at" in e for e in errs), errs)

    def test_a_release_younger_than_14_days_is_red(self):
        errs = _check(_row(published_at="2026-10-30T00:00:00Z"))
        self.assertTrue(any("under 14" in e for e in errs), errs)

    def test_a_row_without_a_content_address_is_red(self):
        self.assertTrue(_check(_row(checksum="")))
        self.assertTrue(_check(_row(checksum="v1.2.3")))

    def test_sri_sha512_and_commit_sha_are_addresses(self):
        sri = "sha512-" + "A" * 86 + "=="
        self.assertEqual(_check(_row(checksum=sri), _row(checksum="b" * 40)), [])

    def test_every_row_records_a_lock_or_v1_departure(self):
        with open(V2) as f:
            lines = [l.rstrip("\n").split("\t") for l in f if not l.startswith("#")]
        ix = lines[0].index("departure")
        for f in lines[1:]:
            self.assertTrue(f[ix] and not f[ix].startswith("UNRESOLVED"), f)


if __name__ == "__main__":
    unittest.main()
