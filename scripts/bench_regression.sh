#!/usr/bin/env python3
"""Benchmark regression gate — runs key benchmarks and compares against baseline.

Usage:
    ./scripts/bench_regression.sh              # Compare against baseline
    ./scripts/bench_regression.sh --update     # Run and update baseline
    THRESHOLD=10 ./scripts/bench_regression.sh # Custom threshold (%)
"""

import json
import os
import re
import subprocess
import sys

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
ROOT_DIR = os.path.dirname(SCRIPT_DIR)
BASELINE_FILE = os.path.join(ROOT_DIR, "data", "bench_baseline.json")
THRESHOLD = int(os.environ.get("THRESHOLD", "5"))
UPDATE = "--update" in sys.argv

BENCHMARKS = [
    ("json_bbnf", "arena_twitter"),
    ("json_bbnf", "arena_canada"),
    ("css_bbnf", "arena_tailwind"),
    ("css_bbnf", "span_tailwind"),
    ("css_bbnf", "l4_tailwind"),
]

os.chdir(os.path.join(ROOT_DIR, "rust"))

results = {}
for target, filt in BENCHMARKS:
    key = f"{target}/{filt}"
    print(f"Running {key} ...")
    try:
        out = subprocess.run(
            ["cargo", "bench", "--bench", target, "--", filt],
            capture_output=True, text=True, timeout=300,
        )
        line = next((l for l in out.stdout.splitlines() if "bench:" in l), "")
    except Exception as e:
        print(f"  WARNING: {e}")
        continue

    m = re.search(r"bench:\s+([\d,]+)\s+ns/iter", line)
    mb = re.search(r"=\s+(\d+)\s+MB/s", line)
    if m:
        ns = int(m.group(1).replace(",", ""))
        results[key] = ns
        print(f"  {ns} ns/iter ({mb.group(1) if mb else '?'} MB/s)")
    else:
        print(f"  WARNING: No output for {key}")

if UPDATE:
    os.makedirs(os.path.dirname(BASELINE_FILE), exist_ok=True)
    with open(BASELINE_FILE, "w") as f:
        json.dump(results, f, indent=2)
    print(f"\nBaseline saved to {BASELINE_FILE}")
    sys.exit(0)

if not os.path.exists(BASELINE_FILE):
    print(f"\nNo baseline found at {BASELINE_FILE}")
    print("Run with --update to create one.")
    sys.exit(1)

with open(BASELINE_FILE) as f:
    baseline = json.load(f)

print(f"\n=== Regression Check (threshold: {THRESHOLD}%) ===\n")
failed = False
for key, current in results.items():
    base = baseline.get(key)
    if base is None:
        print(f"  {key}: no baseline (skipped)")
        continue
    pct = ((current - base) / base) * 100
    if pct > THRESHOLD:
        print(f"  FAIL  {key}: {current} ns/iter (was {base}, {pct:+.1f}% regression)")
        failed = True
    else:
        print(f"  OK    {key}: {current} ns/iter (was {base}, {pct:+.1f}%)")

print()
if failed:
    print(f"REGRESSION DETECTED — one or more benchmarks exceeded {THRESHOLD}% threshold.")
    sys.exit(1)
else:
    print(f"All benchmarks within {THRESHOLD}% threshold.")
