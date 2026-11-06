"""Falsifiers for the CB-B0 harness (sk-v27 LW1.CON-1).

Run:  python3 -m unittest discover -s bench/cb-b0 -v
"""
import json
import os
import subprocess
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import cbb0  # noqa: E402

PY = sys.executable


def _w(path, text):
    with open(path, "w") as f:
        f.write(text)


def _r(path):
    with open(path) as f:
        return f.read()


def _git(repo, *a):
    subprocess.run(["git", "-C", repo, *a], check=True, capture_output=True)


class ScratchRepo:
    def __enter__(self):
        self.d = tempfile.TemporaryDirectory(prefix="cbb0-test-")
        r = self.d.name
        _git(r, "init", "-q")
        _git(r, "config", "user.email", "t@t")
        _git(r, "config", "user.name", "t")
        os.makedirs(os.path.join(r, cbb0.RECORDS_PREFIX))
        _w(os.path.join(r, "src.txt"), "a\n")
        _w(os.path.join(r, cbb0.RECORDS_PREFIX, "old.json"), "{}\n")
        _w(os.path.join(r, ".gitignore"), "target/\n")
        _git(r, "add", "-A")
        _git(r, "commit", "-qm", "init")
        return r

    def __exit__(self, *exc):
        self.d.cleanup()


class TreeHash(unittest.TestCase):
    def test_records_dir_does_not_move_the_key(self):
        with ScratchRepo() as r:
            h0 = cbb0.tree_hash(r)
            self.assertFalse(h0["dirty"])
            _w(os.path.join(r, cbb0.RECORDS_PREFIX, "new.json"), '{"x":1}\n')
            _w(os.path.join(r, cbb0.RECORDS_PREFIX, "old.json"), '{"changed":1}\n')
            h1 = cbb0.tree_hash(r)
            self.assertEqual(h0["tree_hash"], h1["tree_hash"])
            self.assertFalse(h1["dirty"])

    def test_source_change_moves_the_key(self):
        with ScratchRepo() as r:
            h0 = cbb0.tree_hash(r)
            _w(os.path.join(r, "src.txt"), "b\n")
            h1 = cbb0.tree_hash(r)
            self.assertNotEqual(h0["tree_hash"], h1["tree_hash"])
            self.assertTrue(h1["dirty"])

    def test_untracked_counts_ignored_does_not(self):
        with ScratchRepo() as r:
            h0 = cbb0.tree_hash(r)["tree_hash"]
            os.makedirs(os.path.join(r, "target"))
            _w(os.path.join(r, "target", "junk"), "x")
            self.assertEqual(h0, cbb0.tree_hash(r)["tree_hash"])
            _w(os.path.join(r, "new.rs"), "fn f(){}\n")
            self.assertNotEqual(h0, cbb0.tree_hash(r)["tree_hash"])


class OwnRepo(unittest.TestCase):
    def test_the_harness_bytecode_does_not_move_the_key(self):
        # Running these tests writes __pycache__/ beside the harness; it must be ignored, or every record
        # taken after a test run is keyed by bytecode no clean checkout has.
        repo = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
        for rel in ("bench/cb-b0/__pycache__/cbb0.cpython-3.pyc", "bench/pins/__pycache__/pins.cpython-3.pyc",
                    "bench/__pycache__/test_prereg.cpython-3.pyc"):
            r = subprocess.run(["git", "-C", repo, "check-ignore", "-q", rel])
            self.assertEqual(r.returncode, 0, f"{rel} is not ignored")


class Manifest(unittest.TestCase):
    def test_dry_run_is_schema_valid(self):
        with ScratchRepo() as r, tempfile.TemporaryDirectory() as out:
            path = cbb0.dry_run(type("A", (), {"repo": r, "out": out})())
            rec = json.loads(_r(path))
            self.assertEqual(cbb0.validate_obj(rec, cbb0.load_schema()), [])
            self.assertEqual(rec["record_key"], os.path.basename(path)[:-5])
            self.assertTrue(rec["record_key"].startswith(rec["repo"]["tree_hash"][:16]))

    def test_schema_rejects_a_record_without_tree_identity(self):
        with ScratchRepo() as r, tempfile.TemporaryDirectory() as out:
            rec = json.loads(_r(cbb0.dry_run(type("A", (), {"repo": r, "out": out})())))
            del rec["repo"]["tree_hash"]
            rec["status"] = "CERTIFIED"
            errs = cbb0.validate_obj(rec, cbb0.load_schema())
            self.assertTrue(any("tree_hash" in e for e in errs), errs)
            self.assertTrue(any("CERTIFIED" in e for e in errs), errs)

    def test_unlocked_low_rep_record_is_noisy_with_reasons(self):
        if cbb0.bench_lock_held():
            self.skipTest("a bench seat holds the lock right now")
        with ScratchRepo() as r, tempfile.TemporaryDirectory() as out:
            rec = json.loads(_r(cbb0.dry_run(type("A", (), {"repo": r, "out": out})())))
            self.assertEqual(rec["status"], "NOISY")
            joined = " | ".join(rec["status_reasons"])
            self.assertIn("lock not held", joined)
            self.assertIn("below the programme floor", joined)


class Process(unittest.TestCase):
    def test_one_rep_is_one_fresh_process(self):
        with tempfile.TemporaryDirectory() as d:
            shas = set()
            for i in range(4):
                p = os.path.join(d, f"o{i}")
                r = cbb0.spawn_rep([PY, "-c", "import os;print(os.getpid())"], p)
                self.assertEqual(r["exit"], 0)
                shas.add(_r(p))
            self.assertEqual(len(shas), 4)

    def test_qos_accounting_is_truthful_positive_control(self):
        # The kernel honours a downward clamp; the harness must see it land in the utility bucket.
        work = [PY, "-c", "x=sum(range(3_000_000))"]
        with tempfile.TemporaryDirectory() as d:
            clamped = cbb0.spawn_rep(work, os.path.join(d, "a"), clamp="utility")
            free = cbb0.spawn_rep(work, os.path.join(d, "b"))
        self.assertEqual(clamped["qos_dominant"], "utility")
        self.assertNotEqual(free["qos_dominant"], "utility")

    def test_exit_status_is_recorded(self):
        with tempfile.TemporaryDirectory() as d:
            r = cbb0.spawn_rep([PY, "-c", "raise SystemExit(3)"], os.path.join(d, "o"))
        self.assertEqual(r["exit"], 3)

    @unittest.expectedFailure
    def test_host_grants_user_interactive_qos(self):
        """HOST FACT, not a harness defect: a CLI process in this session's ancestry that raises its own
        thread QoS to user-interactive is still accounted in the legacy bucket (a raise is capped; a
        lower is honoured, see the positive control). Expected to fail until the bench runs from a
        context that grants interactive QoS (X-7). An unexpected success flags that the host changed."""
        work = [PY, "-c", "import ctypes;ctypes.CDLL('/usr/lib/libSystem.B.dylib')"
                          ".pthread_set_qos_class_self_np(0x21,0);x=sum(range(3_000_000))"]
        with tempfile.TemporaryDirectory() as d:
            r = cbb0.spawn_rep(work, os.path.join(d, "o"))
        self.assertGreaterEqual(r["qos_ui_fraction"], cbb0.QOS_UI_MIN_FRACTION)


if __name__ == "__main__":
    unittest.main()
