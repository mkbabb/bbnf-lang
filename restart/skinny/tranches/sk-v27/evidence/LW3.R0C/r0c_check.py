#!/usr/bin/env python3
"""LW3 R0C falsifiers: the R0 critic's rows, recorded by seat S6 KERNEL.

Usage: r0c_check.py <unit> <rev>
  unit: plan | rulings | ledger
  rev:  the commit read (files are read with `git show <rev>:<path>`).
Exit 0 = GREEN, 1 = RED (each reason printed), 2 = usage.

The DIG-G1 half of `plan` (rows G1-P23..P27 against their passages, and the
widened grep) is read by `evidence/LW3.R0/r0_check.py fold <rev>`.
"""
import re, subprocess, sys

SK = "restart/skinny/tranches/sk-v27"


def git(*a, check=True):
    p = subprocess.run(["git", *a], capture_output=True, text=True)
    if check and p.returncode != 0:
        raise RuntimeError(f"git {' '.join(a)}: {p.stderr.strip()}")
    return p


def show(rev, path):
    p = git("show", f"{rev}:{path}", check=False)
    return p.stdout if p.returncode == 0 else ""


def check_plan(rev):
    bad = []
    plan = show(rev, f"{SK}/PLAN.md")
    m = re.search(r"^### 7\.4 2026-11-12, the R0 critic's rows.*?(?=^### |^## |\Z)", plan, re.M | re.S)
    if not m:
        return ["PLAN.md has no §7.4 (the R0 critic's rows)"]
    sec = m.group(0)
    want = {
        "G1-P23": ["designs/programme.md:483", "DIG-G1", "not deleted"],
        "G1-P24": ["designs/pprint.md:206", "designs/pprint.md:207", "DIG-G1", "not selected by default"],
        "G1-P25": ["designs/compiler.md:114", "PLAN.md:59", "DIG-G1", "O-2", "Read, not amended"],
        "G1-P26": ["PLAN.md:56", "DIG-G1", "built on the spine whatever it reads"],
        "G1-P27": ["LW3-ORDER.md:107", "LW3-ORDER.md:290", "LW3-ORDER.md:299",
                   "LW3-ORDER.md:300", "LW2-ORDER.md:88", "DIG-G1", "never existence"],
        "C16-P01": ["PLAN.md:206", "designs/compiler.md:64", "designs/compiler.md:187",
                    "DIG-C16", "off by default", "owner ruling"],
        "A-26": ["LW5.COMP-4", "HM and bidirectional inference", "O-5", "DIG-CSP-B7"],
    }
    for rid, needles in want.items():
        rows = [l for l in sec.split("\n") if l.startswith(f"| {rid} |")]
        if len(rows) != 1:
            bad.append(f"{rid}: {len(rows)} rows in PLAN §7.4 (want 1)")
            continue
        for n in needles:
            if n not in rows[0]:
                bad.append(f"{rid}: row lacks {n!r}")
    base = git("merge-base", rev, "origin/sk-v27-lw3").stdout.strip()
    for p in ("PLAN.md", "LEDGER.md", "RULINGS.md"):
        old, new = show(base, f"{SK}/{p}"), show(rev, f"{SK}/{p}")
        if not new.startswith(old.rstrip("\n")):
            bad.append(f"{p}: sealed text changed (not append-only)")
    return bad


def check_rulings(rev):
    bad = []
    t = show(rev, f"{SK}/RULINGS.md")
    m = re.search(r"^## Addendum, 2026-11-12b\b.*?(?=^## |\Z)", t, re.M | re.S)
    if not m:
        return ["RULINGS.md has no `## Addendum, 2026-11-12b` section"]
    sec = m.group(0)
    want = {
        "E15-W": ["before LW3 Phase 5 opens", "non-discriminating", "LW5"],
        "Z32-W": ["LW2–LW7", "LW3.COMP-1b-W", "LW5.COMP-1-MINE"],
        "ORC-1a": ["cure unit and wave", "born-RED falsifier", "blocks LG-3"],
    }
    for rid, needles in want.items():
        rows = [l for l in sec.split("\n") if l.startswith(f"| {rid} |")]
        if len(rows) != 1:
            bad.append(f"{rid}: {len(rows)} rows in 2026-11-12b (want 1)")
            continue
        if "delegate, owner may overturn" not in rows[0]:
            bad.append(f"{rid}: row is not marked \"delegate, owner may overturn\"")
        for n in needles:
            if n not in rows[0]:
                bad.append(f"{rid}: row lacks {n!r}")
    return bad


STATUS = re.compile(r"\| (RED|GREEN|PLANNED|RULING|OPEN|CARRIED|RECORDED)[^|]*\|\s*$")


def check_ledger(rev):
    bad = []
    led = show(rev, f"{SK}/LEDGER.md")
    rows = [l for l in led.split("\n") if l.startswith("| 2026-")]
    want = {
        "DIG-G1 status corrected": ("RED", ["G1-P23..P27", "r0_check.py", "dig-g1-passages.tsv"]),
        "DIG-CSP-E15 and DIG-EG-D17 re-homed": ("PLANNED", ["E15-W", "before S14", "run at LW5"]),
        "DIG-G2 status corrected": ("RED", ["LW3.COMP-1b-W", "Z32-W", "substrate_audit.rs:99"]),
    }
    for title, (status, needles) in want.items():
        hit = [l for l in rows if f"**{title}" in l]
        if len(hit) != 1:
            bad.append(f"{title}: {len(hit)} LEDGER rows (want 1)")
            continue
        m = STATUS.search(hit[0])
        if not m:
            bad.append(f"{title}: row has no status cell")
        elif m.group(1) != status:
            bad.append(f"{title}: status {m.group(1)} (want {status})")
        for n in needles:
            if n not in hit[0]:
                bad.append(f"{title}: row lacks {n!r}")
    return bad


def main():
    if len(sys.argv) < 3:
        print(__doc__)
        return 2
    unit, rev = sys.argv[1], sys.argv[2]
    fn = {"plan": check_plan, "rulings": check_rulings, "ledger": check_ledger}.get(unit)
    if not fn:
        print(__doc__)
        return 2
    full = git("rev-parse", rev).stdout.strip()
    bad = fn(full)
    print(f"unit={unit} rev={full} reasons={len(bad)}")
    for b in bad:
        print("RED", b)
    print("GREEN" if not bad else "RED")
    return 0 if not bad else 1


if __name__ == "__main__":
    sys.exit(main())
