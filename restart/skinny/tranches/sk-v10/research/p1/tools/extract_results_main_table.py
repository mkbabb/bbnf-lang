#!/usr/bin/env python3
"""Extract the main SK JSON RESULTS table used by SK-V10 S-P1-F."""

from __future__ import annotations

import csv
import sys
from collections import Counter
from pathlib import Path


def parse_markdown_table(path: Path) -> list[dict[str, str]]:
    lines = path.read_text(encoding="utf-8").splitlines()
    table_lines: list[str] = []
    in_table = False
    for line in lines:
        if line.startswith("| Corpus | Workload | Outcome |"):
            in_table = True
        if in_table:
            if not line.startswith("|"):
                break
            table_lines.append(line)
    if len(table_lines) < 3:
        raise SystemExit(f"main RESULTS table not found in {path}")
    header = [cell.strip() for cell in table_lines[0].strip("|").split("|")]
    rows = []
    for line in table_lines[2:]:
        cells = [cell.strip() for cell in line.strip("|").split("|")]
        if len(cells) != len(header):
            raise SystemExit(f"malformed table row: {line}")
        rows.append(dict(zip(header, cells)))
    return rows


def pct(track: str, sonic: str) -> str:
    if sonic == "n/a":
        return "n/a"
    try:
        delta = (float(track) / float(sonic) - 1.0) * 100.0
    except ValueError:
        return "n/a"
    return f"{delta:+.1f}%"


def main() -> int:
    if len(sys.argv) not in (2, 3):
        print("usage: extract_results_main_table.py skinny/RESULTS.md [csv-output]", file=sys.stderr)
        return 2
    rows = parse_markdown_table(Path(sys.argv[1]))
    counts = Counter(row["Workload"] for row in rows)
    print("Main row counts:")
    for workload in ("parse_only", "direct_to_struct", "real_typed_struct"):
        print(f"- {workload}: {counts[workload]}")
    print(f"- total: {len(rows)}")
    print()
    print("| Corpus | Workload | Outcome | Verdict | Track 1 Mbps | Track 2 Mbps | sonic-rs strict Mbps | Delta |")
    print("|---|---|---|---|---:|---:|---:|---:|")
    for row in rows:
        print(
            f"| `{row['Corpus']}` | `{row['Workload']}` | `{row['Outcome']}` | "
            f"`{row['Verdict']}` | {row['Track 1 Mbps']} | {row['Track 2 Mbps']} | "
            f"{row['sonic-rs strict Mbps']} | {pct(row['Track 1 Mbps'], row['sonic-rs strict Mbps'])} |"
        )
    if len(sys.argv) == 3:
        with Path(sys.argv[2]).open("w", newline="", encoding="utf-8") as handle:
            writer = csv.DictWriter(handle, fieldnames=list(rows[0]))
            writer.writeheader()
            writer.writerows(rows)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
