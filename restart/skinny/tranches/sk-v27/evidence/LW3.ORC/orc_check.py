#!/usr/bin/env python3
"""LW3 ORC falsifiers for the record units of seat S5b ORACLE (rulings ORC-1, ORC-2, ORC-1a).

Usage: orc_check.py <unit> <rev>
  qx1   LEDGER carries the QX-1 rule restated with ORC-1's clause, as a delegate row with a status cell.
  rows  every R-b row of SK/grammar/DIVERGENCE.tsv that reads a G1 case (g1 column not bbnf/ts-frontend)
        names its cure unit and wave and its born-RED falsifier, and that falsifier exists at <rev> and
        carries an #[ignore] naming the row (ORC-1a).
Reads the files at <rev> (git show), never the working tree. rc 0 GREEN, rc 1 RED, rc 2 usage.
"""
import re
import subprocess
import sys

SK = "restart/skinny/tranches/sk-v27"


def show(rev, path):
    r = subprocess.run(["git", "show", f"{rev}:{path}"], capture_output=True, text=True)
    return r.stdout if r.returncode == 0 else None


CLAUSE = "cures a defect an independent oracle in custody names under A-10"


def qx1(rev):
    led = show(rev, f"{SK}/LEDGER.md") or ""
    rows = [l for l in led.splitlines() if l.startswith("| ") and "**LW3.QX1-C" in l]
    if len(rows) != 1:
        return [f"{len(rows)} LW3.QX1-C rows (want 1)"]
    row = rows[0]
    bad = []
    for need in (CLAUSE, "delegate, owner may overturn", "removes more than it adds",
                 "an LW1 or LW2 born-RED test already names", "RULINGS 2026-11-12a ORC-1",
                 "regenerat", "no hand edit"):
        if need not in row:
            bad.append(f"QX1-C row lacks {need!r}")
    cells = [c.strip() for c in row.strip().strip("|").split("|")]
    if cells[-1] not in ("GREEN", "RED", "RULING", "RECORDED"):
        bad.append(f"QX1-C row has no status cell (last cell {cells[-1][:40]!r})")
    return bad


def rows(rev):
    div = show(rev, f"{SK}/grammar/DIVERGENCE.tsv")
    if div is None:
        return ["DIVERGENCE.tsv absent"]
    lines = [l for l in div.splitlines() if l and not l.startswith("#")]
    head = lines[0].split("\t")
    if "g1" not in head:
        return ["DIVERGENCE.tsv has no g1 column: no G1 case row can be read"]
    ix = {c: head.index(c) for c in head}
    bad, n = [], 0
    for l in lines[1:]:
        f = l.split("\t")
        if len(f) != len(head):
            bad.append(f"{f[0]}: {len(f)} cells, header {len(head)}")
            continue
        if f[ix["g1"]] == "bbnf/ts-frontend":
            continue
        n += 1
        fate = f[ix["fate"]]
        m = re.search(r"cure (LW[4-7]\.[A-Z0-9-]+(?:\.[A-Z0-9-]+)?) at (LW[4-7])", fate)
        if not m:
            bad.append(f"{f[0]}: fate names no cure unit and wave: {fate[:80]!r}")
        t = re.search(r"born-RED (crates/[\w/.-]+\.rs)::(\w+)", fate)
        if not t:
            bad.append(f"{f[0]}: fate names no born-RED falsifier: {fate[:80]!r}")
            continue
        src = show(rev, t.group(1))
        if src is None:
            bad.append(f"{f[0]}: falsifier {t.group(1)} absent at {rev}")
            continue
        fn = re.search(r"#\[ignore = \"([^\"]*)\"\]\s*fn " + t.group(2) + r"\b", src)
        if not fn:
            bad.append(f"{f[0]}: {t.group(1)}::{t.group(2)} absent or not #[ignore]d")
        elif f[0] not in fn.group(1):
            bad.append(f"{f[0]}: {t.group(2)}'s ignore reason does not name the row")
    if n == 0:
        bad.append("no G1 case rows")
    return bad


def main():
    if len(sys.argv) != 3 or sys.argv[1] not in ("qx1", "rows"):
        print(__doc__)
        return 2
    bad = {"qx1": qx1, "rows": rows}[sys.argv[1]](sys.argv[2])
    for b in bad:
        print("RED:", b)
    print(f"{sys.argv[1]} at {sys.argv[2]}: {'RED' if bad else 'GREEN'}")
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
