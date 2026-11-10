#!/usr/bin/env python3
"""sk-v27 LW2.RX-1 falsifier: rx/OR7-SPIKE.md has one row per leaf per route.

Usage: or7_check.py <OR7-SPIKE.md> <LOOKAROUND-CENSUS.tsv> <route letters, e.g. AB>
Exit 0 when every census leaf has exactly one row per route in the tier
tables, in its own tier, each with a verdict of AGREE, ENGINE or
SPEC-AMBIGUITY, and no other rows exist. Otherwise prints each gap and
exits 1.
"""
import sys

VERDICTS = {"AGREE", "ENGINE", "SPEC-AMBIGUITY"}


def main():
    md_path, census_path, routes = sys.argv[1], sys.argv[2], sys.argv[3]
    try:
        md = open(md_path, encoding="utf-8").read().splitlines()
    except FileNotFoundError:
        print(f"or7 check: {md_path} absent")
        return 1
    want = {}
    with open(census_path, encoding="utf-8") as f:
        head = f.readline().rstrip("\n").split("\t")
        for line in f:
            row = dict(zip(head, line.rstrip("\n").split("\t")))
            want[f'{row["corpus"]}#{row["n"]}'] = "LOOKAROUND" if row["lookaround"] == "1" else "DFA"
    seen = {}
    problems = []
    tier = None
    for line in md:
        if line.startswith("## Rows: "):
            tier = line.split("## Rows: ")[1].split(" ")[0]
            continue
        if line.startswith("## "):
            tier = None
            continue
        if tier is None or not line.startswith("| ") or line.startswith("| Leaf") or line.startswith("|---"):
            continue
        cells = [c.strip() for c in line.strip("|").split(" | ")]
        leaf, route, verdict = cells[0], cells[1], cells[11]
        key = (leaf, route)
        if key in seen:
            problems.append(f"duplicate row {key}")
        seen[key] = (tier, verdict)
        if leaf not in want:
            problems.append(f"row for unknown leaf {leaf}")
        elif want[leaf] != tier:
            problems.append(f"{leaf} route {route} in tier {tier}, census says {want[leaf]}")
        if verdict not in VERDICTS:
            problems.append(f"{leaf} route {route}: verdict {verdict!r}")
    for leaf in want:
        for r in routes:
            if (leaf, r) not in seen:
                problems.append(f"gap: {leaf} route {r}")
    for p in problems:
        print(p)
    n = len(want) * len(routes)
    print(f"or7 check: {len(seen)} rows for {len(want)} leaves x {len(routes)} routes (want {n}); problems {len(problems)}")
    return 1 if problems or len(seen) != n else 0


if __name__ == "__main__":
    sys.exit(main())
