#!/usr/bin/env python3
"""Extract SK-V10 S-P1 Mode III Criterion probe tables."""

from __future__ import annotations

import json
import sys
from pathlib import Path


CORPORA = [
    "twitter",
    "citm_catalog",
    "canada",
    "apache_builds",
    "github_events",
    "update_center",
    "mesh",
    "random",
    "gsoc-2018",
    "marine_ik",
    "instruments",
    "numbers",
    "unicode_mixed",
    "unicode_escapes",
    "unicode_basic",
    "distinct_values",
    "y_string_unicode",
]


def load_json(path: Path) -> dict:
    with path.open("r", encoding="utf-8") as handle:
        return json.load(handle)


def mean_ns(path: Path) -> float:
    return float(load_json(path)["mean"]["point_estimate"])


def bytes_for(path: Path) -> int:
    return int(load_json(path)["throughput"]["Bytes"])


def mbps(byte_count: int, ns: float) -> float:
    # The SK JSON ledger labels megabits per second as Mbps.
    return byte_count * 8000.0 / ns


def print_probe_table(root: Path) -> None:
    print("| Corpus | Bytes | cold first parse Mbps | eager decode Mbps | eager/cold time | alternate scalar Mbps | alternate/cold time |")
    print("|---|---:|---:|---:|---:|---:|---:|")
    for corpus in CORPORA:
        probe_root = root / f"json_probes_{corpus}"
        cold_root = probe_root / "cold_first_parse" / "new"
        byte_count = bytes_for(cold_root / "benchmark.json")
        cold = mean_ns(cold_root / "estimates.json")
        eager = mean_ns(probe_root / "host_call_eager_decode" / "new" / "estimates.json")
        alt = mean_ns(probe_root / "alternate_scalar_plan" / "new" / "estimates.json")
        print(
            f"| `{corpus}` | {byte_count} | {mbps(byte_count, cold):.0f} | "
            f"{mbps(byte_count, eager):.0f} | {eager / cold:.2f}x | "
            f"{mbps(byte_count, alt):.0f} | {alt / cold:.2f}x |"
        )


def print_scan_table(root: Path) -> None:
    scan_root = root / "simd_structural_scan"
    print("| Corpus | SIMD scan Mbps | scalar scan Mbps | SIMD/scalar speedup |")
    print("|---|---:|---:|---:|")
    for corpus in CORPORA:
        simd_root = scan_root / f"{corpus}_simd" / "new"
        scalar_root = scan_root / f"{corpus}_scalar" / "new"
        byte_count = bytes_for(simd_root / "benchmark.json")
        simd = mean_ns(simd_root / "estimates.json")
        scalar = mean_ns(scalar_root / "estimates.json")
        print(
            f"| `{corpus}` | {mbps(byte_count, simd):.0f} | "
            f"{mbps(byte_count, scalar):.0f} | {scalar / simd:.2f}x |"
        )


def main() -> int:
    if len(sys.argv) != 2:
        print("usage: extract_mode3_criterion.py /tmp/skv10-p1/mode3-criterion", file=sys.stderr)
        return 2
    root = Path(sys.argv[1])
    print("## Decode And Scalar Masking")
    print()
    print_probe_table(root)
    print()
    print("## Structural Scan Isolation")
    print()
    print_scan_table(root)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
