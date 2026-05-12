#!/usr/bin/env python3
"""Sonic-rs expanded profile harness.

Runs the full (corpus × path × variant) throughput grid, then a focused
samply profile subset. Outputs JSONL throughput log and samply .json.gz
files per (corpus × path × variant).
"""

import json
import os
import subprocess
import sys
import time
from pathlib import Path

OUT = Path("/Users/mkbabb/Programming/bbnf-lang/skinny/profile/sonic-rs-expanded")
INLINED = Path("/tmp/sonic-research/sonic-rs/benchmarks/target/release/examples/perf_parse")
NOINLINE = Path("/tmp/sonic-research/sonic-rs/benchmarks/target-noinline/release/examples/perf_parse")
DATA_DIR = Path("/Users/mkbabb/Programming/bbnf-lang/data/json")
TEST_DIR = Path("/Users/mkbabb/Programming/bbnf-lang/skinny/test_data")

CORPORA = [
    ("twitter",          DATA_DIR / "twitter.json"),
    ("citm",             DATA_DIR / "citm_catalog.json"),
    ("canada",           DATA_DIR / "canada.json"),
    ("apache_builds",    TEST_DIR / "apache_builds.json"),
    ("github_events",    TEST_DIR / "github_events.json"),
    ("update_center",    TEST_DIR / "update-center.json"),
    ("mesh",             TEST_DIR / "mesh.json"),
    ("unicode_mixed",    TEST_DIR / "unicode_mixed.json"),
    ("unicode_escapes",  TEST_DIR / "unicode_escapes.json"),
]

ITERS_VALUE = {
    "twitter":        8000,
    "citm":           3000,
    "canada":         2500,
    "apache_builds":  40000,
    "github_events":  80000,
    "update_center":  10000,
    "mesh":           7000,
    "unicode_mixed":  5000,
    "unicode_escapes":5000,
}
ITERS_LAZY = {
    # LazyValue is ~6-10× faster than Value-DOM
    "twitter":        40000,
    "citm":           15000,
    "canada":         15000,
    "apache_builds":  200000,
    "github_events":  400000,
    "update_center":  50000,
    "mesh":           40000,
    "unicode_mixed":  25000,
    "unicode_escapes":25000,
}


def run_throughput():
    """Phase 1: Measure throughput for every (corpus × path × variant)."""
    log_path = OUT / "throughput.jsonl"
    log_path.unlink(missing_ok=True)
    f = log_path.open("a")
    for corpus, cp in CORPORA:
        size_bytes = cp.stat().st_size
        for variant_name, bin_path in [("inlined", INLINED), ("noinline", NOINLINE)]:
            for mode in ["value", "lazy"]:
                iters = ITERS_VALUE[corpus] if mode == "value" else ITERS_LAZY[corpus]
                if variant_name == "noinline":
                    iters = max(200, iters // 3)
                # warmup
                subprocess.run([str(bin_path), str(cp), "1", mode],
                               capture_output=True, check=True)
                t0 = time.perf_counter()
                subprocess.run([str(bin_path), str(cp), str(iters), mode],
                               capture_output=True, check=True)
                wall_s = time.perf_counter() - t0
                mbps = (size_bytes * iters) / wall_s / 1e6
                rec = {
                    "corpus":  corpus,
                    "path":    mode,
                    "variant": variant_name,
                    "iters":   iters,
                    "wall_s":  round(wall_s, 4),
                    "bytes":   size_bytes,
                    "mbps":    round(mbps, 1),
                }
                f.write(json.dumps(rec) + "\n")
                f.flush()
                print(f"{corpus:18s} {variant_name:8s} {mode:5s} iters={iters:7d} "
                      f"wall={wall_s:6.3f}s mbps={mbps:7.1f}", flush=True)
    f.close()


def run_samply():
    """Phase 2: Capture samply profiles. Each invocation runs the binary for
    ~5-8 seconds to give ~5000-8000 samples at 1000 Hz.

    Profile every corpus × every path × every variant = 9 × 2 × 2 = 36 profiles.
    To stay inside time budget, only profile the 5 most informative corpora:
      twitter, citm, canada (canonical), unicode_mixed (UTF-8), unicode_escapes (\\u-escape).
    """
    PROFILE_CORPORA = ["twitter", "citm", "canada", "apache_builds", "mesh",
                       "unicode_mixed", "unicode_escapes"]

    # iters tuned for ~3-5s of CPU per profile run (1000 Hz → 3000-5000 samples)
    PROFILE_ITERS_VALUE = {
        "twitter":        4000,
        "citm":           2000,
        "canada":         1500,
        "apache_builds":  20000,
        "mesh":           3500,
        "unicode_mixed":  3000,
        "unicode_escapes":3000,
    }
    PROFILE_ITERS_LAZY = {
        "twitter":        20000,
        "citm":           8000,
        "canada":         10000,
        "apache_builds":  100000,
        "mesh":           20000,
        "unicode_mixed":  15000,
        "unicode_escapes":15000,
    }

    corpora_map = dict(CORPORA)
    for corpus in PROFILE_CORPORA:
        cp = corpora_map[corpus]
        for variant_name, bin_path in [("inlined", INLINED), ("noinline", NOINLINE)]:
            for mode in ["value", "lazy"]:
                iters = (PROFILE_ITERS_VALUE if mode == "value" else PROFILE_ITERS_LAZY)[corpus]
                if variant_name == "noinline":
                    iters = max(200, iters // 3)
                profile_id = f"{corpus}.{mode}.{variant_name}"
                profile_path = OUT / f"{profile_id}.profile.json.gz"
                t0 = time.perf_counter()
                cmd = [
                    "samply", "record", "-r", "1000", "--save-only",
                    "--unstable-presymbolicate",
                    "-o", str(profile_path),
                    str(bin_path), str(cp), str(iters), mode,
                ]
                r = subprocess.run(cmd, capture_output=True)
                wall_s = time.perf_counter() - t0
                ok = "OK " if r.returncode == 0 and profile_path.exists() else "FAIL"
                print(f"{ok} {profile_id:42s} iters={iters:7d} wall={wall_s:5.2f}s", flush=True)
                if r.returncode != 0:
                    sys.stderr.write(r.stderr.decode(errors='replace')[:500] + "\n")


if __name__ == "__main__":
    mode = sys.argv[1] if len(sys.argv) > 1 else "all"
    if mode in ("all", "tput"):
        run_throughput()
    if mode in ("all", "samply"):
        run_samply()
