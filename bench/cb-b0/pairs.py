#!/usr/bin/env python3
"""Paired ABBA runner for registered cells (sk-v27 LW2.CON-2, S11 BENCH).

One invocation = one harness invocation of PREREG §3: `--blocks` ABBA blocks of four fresh
processes (A B B A), each block giving two pairs (A1,B1) and (A2,B2). Every process is spawned
by `cbb0.spawn_rep`, so each carries its wall time, rusage and kernel QoS accounting.

Before each block the runner waits for a 1-minute load below 8, at most 60 min per invocation
(X-7); once that budget is spent the block runs and is labelled NOISY. A block is QUIET only
when its four processes all started at load < 8 and no thermal warning was recorded around it;
user-interactive QoS is recorded per process as a disclosed host condition (LW2-ORDER §5
reading, ruled at SEAM-B), not as a QUIET term.

SLOW (PREREG §7.1): `--slow-a-ns N` makes the runner busy-wait N ns inside arm A's timed window
(after the child exits, before the window closes); the measured binary is untouched.

Each process prints `checksum=<token>`; a pair whose two checksums differ is discarded by
decide.py. `--cell` must be registered in bench/PREREG.md; a §7 contest cell must also be
admitted by bench/contest/admit.py (with the two build-plane records).

Usage:
  pairs.py run --cell C-ID --out DIR --blocks N --a 'CMD' --b 'CMD' [--slow-a-ns N]
               [--build-a P --build-b P] [--note TEXT] [--repo PATH]
"""
from __future__ import annotations

import argparse
import datetime as dt
import json
import os
import re
import shlex
import sys
import tempfile
import time

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
sys.path.insert(0, os.path.join(os.path.dirname(HERE), "contest"))
import cbb0  # noqa: E402

SCHEMA = "cb-b0-pairs/1"
WAIT_BUDGET_S = 60 * 60
CHECKSUM = re.compile(r"^checksum=(\S+)\s*$", re.M)


def _busy(ns: int) -> int:
    t0 = time.perf_counter_ns()
    while time.perf_counter_ns() - t0 < ns:
        pass
    return time.perf_counter_ns() - t0


def _proc(cmd: list[str], d: str, tag: str, slow_ns: int) -> dict:
    out = os.path.join(d, tag + ".out")
    r = cbb0.spawn_rep(cmd, out)
    if slow_ns:
        r["slow_injected_ns"] = _busy(slow_ns)
        r["wall_ns"] += r["slow_injected_ns"]
    text = open(out, encoding="utf-8", errors="replace").read()
    m = CHECKSUM.search(text)
    r["checksum"] = m.group(1) if m else "absent"
    r["stdout_tail"] = text[-300:]
    return r


def run(a) -> str:
    repo = os.path.abspath(a.repo)
    why = cbb0.refuse_unregistered(repo, a.cell, a.cell, "PORTABLE" if a.matrix == "PORTABLE" else a.matrix)
    if why:
        raise SystemExit(f"pairs: refused: {why}")
    import admit  # noqa: E402
    admission = "not a §7 contest cell (registered in another PREREG section)"
    if a.cell in admit.prereg_cells():
        v = admit.admit_cell(a.cell, a.build_a, a.build_b)
        if not v.admitted:
            raise SystemExit("pairs: " + v.text())
        admission = "ADMITTED"
    cmd_a, cmd_b = shlex.split(a.a), shlex.split(a.b)
    ident = cbb0.tree_hash(repo)
    started = time.monotonic()
    lock = cbb0.bench_lock_held()
    blocks = []
    with tempfile.TemporaryDirectory(prefix="cbb0-pairs-") as d:
        for k in range(a.blocks):
            waited = 0.0
            while cbb0.load1() >= cbb0.QUIET_LOAD and time.monotonic() - started < WAIT_BUDGET_S:
                time.sleep(15)
                waited += 15
            th0 = cbb0.therm()
            loads, procs = [], {}
            for tag, cmd, slow in (("a1", cmd_a, a.slow_a_ns), ("b1", cmd_b, 0),
                                   ("b2", cmd_b, 0), ("a2", cmd_a, a.slow_a_ns)):
                loads.append(cbb0.load1())
                procs[tag] = _proc(cmd, d, f"{k}-{tag}", slow)
            th1 = cbb0.therm()
            bad = [t for t, p in procs.items() if p["exit"] != 0]
            if bad:
                raise SystemExit(f"pairs: non-zero exit in block {k} ({bad}): {procs[bad[0]]['stdout_tail']}")
            import math
            pairs = []
            for x, y in (("a1", "b1"), ("a2", "b2")):
                pa, pb = procs[x], procs[y]
                pairs.append({"r": math.log(pa["wall_ns"] / pb["wall_ns"]), "a": pa, "b": pb,
                              "checksum_a": pa["checksum"], "checksum_b": pb["checksum"]})
            quiet = all(l < cbb0.QUIET_LOAD for l in loads) and not th0["warning"] and not th1["warning"]
            blocks.append({"k": k, "order": "ABBA", "waited_s": waited, "load_before": loads,
                           "therm_warning": th0["warning"] or th1["warning"], "quiet": quiet, "pairs": pairs})
    n_quiet = sum(b["quiet"] for b in blocks)
    reasons = []
    if n_quiet < len(blocks):
        reasons.append(f"{len(blocks) - n_quiet}/{len(blocks)} blocks at load1 >= 8 or a thermal warning")
    if not lock:
        reasons.append("guarded --exclusive bench lock not held")
    qmin = min(p[s]["qos_ui_fraction"] for b in blocks for p in b["pairs"] for s in ("a", "b"))
    rec = {"schema": SCHEMA, "cell": a.cell, "matrix": a.matrix, "admission": admission,
           "created_at": dt.datetime.now(dt.timezone.utc).isoformat(timespec="seconds"),
           "repo": ident, "host": cbb0.host(), "note": a.note or "", "arm_a": cmd_a, "arm_b": cmd_b,
           "slow_a_ns": a.slow_a_ns, "bench_lock_held": lock, "guarded_run": os.environ.get("GUARDED_RUN", ""),
           "harness_sha256": {"pairs.py": cbb0._file_sha256(__file__), "cbb0.py": cbb0._file_sha256(cbb0.__file__)},
           "qos": {"ui_fraction_min": qmin, "reading": "disclosed host condition (LW2-ORDER §5; QoS ruling owed)"},
           "status": "QUIET" if not reasons else "NOISY", "status_reasons": reasons, "blocks": blocks}
    os.makedirs(a.out, exist_ok=True)
    key = f"{ident['tree_hash'][:16]}-{ident['commit'][:12]}-{a.cell}-{dt.datetime.now().strftime('%Y%m%dT%H%M%S')}"
    path = os.path.join(a.out, key + ".json")
    with open(path, "w") as f:
        json.dump(rec, f, indent=1)
        f.write("\n")
    return path


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="verb", required=True)
    r = sub.add_parser("run")
    r.add_argument("--cell", required=True)
    r.add_argument("--out", required=True)
    r.add_argument("--blocks", type=int, default=4)
    r.add_argument("--a", required=True)
    r.add_argument("--b", required=True)
    r.add_argument("--slow-a-ns", type=int, default=0)
    r.add_argument("--matrix", default="PORTABLE", choices=["PORTABLE", "NATIVE"])
    r.add_argument("--build-a")
    r.add_argument("--build-b")
    r.add_argument("--note")
    r.add_argument("--repo", default=os.path.dirname(os.path.dirname(HERE)))
    a = ap.parse_args(argv)
    print(run(a))
    return 0


if __name__ == "__main__":
    sys.exit(main())
