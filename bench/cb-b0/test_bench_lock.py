"""Falsifiers for the bench window lock (sk-v27 LW1.CON-3).

The question the unit asks: does `guarded --exclusive bench` exist, and what does it exclude?
These tests launch real `guarded` runs (light, sleep-only workloads) and record the answer.

Run:  python3 -m unittest bench/cb-b0/test_bench_lock.py -v
"""
import json
import os
import subprocess
import sys
import tempfile
import time
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import cbb0  # noqa: E402

GUARDED = os.path.expanduser("~/.claude/guard/guarded")
PY = sys.executable
HERE = os.path.dirname(os.path.abspath(__file__))
# The child stamps its own window and asks the harness whether the bench lock is held.
CHILD = ("import json,sys,time;sys.path.insert(0,%r);import cbb0;t0=time.time();held=cbb0.bench_lock_held();"
         "time.sleep(%s);print(json.dumps({'t0':t0,'t1':time.time(),'held':held}))")


def _launch(exclusive, hold_s, out):
    cmd = [GUARDED, "--light", "--timeout", "2"]
    if exclusive:
        cmd += ["--exclusive", "bench"]
    cmd += ["--", PY, "-c", CHILD % (HERE, hold_s)]
    with open(out, "w") as fh:  # the child holds its own dup of the descriptor
        return subprocess.Popen(cmd, stdout=fh, stderr=subprocess.DEVNULL)


def _window(out):
    with open(out) as f:
        return json.loads(f.read().strip().splitlines()[-1])


@unittest.skipUnless(os.access(GUARDED, os.X_OK), "guarded is not installed")
class BenchLock(unittest.TestCase):
    def setUp(self):
        if cbb0.bench_lock_held():
            self.skipTest("a bench seat holds the lock right now; this probe would wait on it")

    def test_two_bench_seats_never_overlap(self):
        with tempfile.TemporaryDirectory() as d:
            a, b = os.path.join(d, "a"), os.path.join(d, "b")
            pa = _launch(True, 2.0, a)
            time.sleep(0.5)
            pb = _launch(True, 0.2, b)
            self.assertEqual(pa.wait(60), 0)
            self.assertEqual(pb.wait(60), 0)
            wa, wb = _window(a), _window(b)
        self.assertTrue(wa["held"] and wb["held"], (wa, wb))
        self.assertGreaterEqual(wb["t0"], wa["t1"] - 0.05, f"bench seats overlapped: {wa} {wb}")

    def test_bench_lock_is_visible_only_inside_a_bench_seat(self):
        with tempfile.TemporaryDirectory() as d:
            a = os.path.join(d, "a")
            pa = _launch(False, 0.2, a)
            self.assertEqual(pa.wait(60), 0)
            self.assertFalse(_window(a)["held"])

    @unittest.expectedFailure
    def test_bench_lock_excludes_a_non_bench_seat(self):
        """RECORDED GAP (X-7): the lock serialises bench seats only. A non-bench seat starts while a bench
        seat holds it, so the quiet window is the orchestrator's to keep, not the lock's."""
        with tempfile.TemporaryDirectory() as d:
            a, b = os.path.join(d, "a"), os.path.join(d, "b")
            pa = _launch(True, 2.0, a)
            time.sleep(0.5)
            pb = _launch(False, 0.2, b)
            pa.wait(60)
            pb.wait(60)
            wa, wb = _window(a), _window(b)
        self.assertGreaterEqual(wb["t0"], wa["t1"] - 0.05)


if __name__ == "__main__":
    unittest.main()
