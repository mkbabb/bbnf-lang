#!/usr/bin/env python3
"""Reproduce the SK-V13 S-P1 V2/V3 direct and mode-III summary TSVs."""

import csv
import os
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from extract_hotleaf_top20 import OUT, ROOT, top_rows  # noqa: E402

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

DIRECT_RESULT = re.compile(
    r"PROBE_RESULT .*?mbps=(?P<mbps>[0-9.]+) .*?cycles_per_byte=(?P<cpb>[0-9.]+)"
)


def parse_direct_log(corpus, mode):
    path = Path(ROOT) / "samply" / "logs" / f"direct__{corpus}__{mode}.log"
    match = DIRECT_RESULT.search(path.read_text())
    if not match:
        raise RuntimeError(f"missing PROBE_RESULT in {path}")
    return match.group("mbps"), float(match.group("cpb"))


def top_one(profile):
    total, rows = top_rows(str(profile), limit=1)
    if not rows:
        return "0.0", "", "", ""
    (function, file_name, line, _lib), count = rows[0]
    return f"{count / total * 100:.1f}", function, file_name, line


def write_direct_summary():
    Path(OUT).mkdir(parents=True, exist_ok=True)
    with open(Path(OUT) / "direct_summary.tsv", "w", newline="") as f:
        writer = csv.writer(f, delimiter="\t")
        writer.writerow(
            [
                "corpus",
                "track1_mbps",
                "track1_cpb",
                "track1_top_pct",
                "track1_top_function",
                "track1_top_file",
                "track1_top_line",
                "track2_mbps",
                "track2_cpb",
                "track2_top_pct",
                "track2_top_function",
                "track2_top_file",
                "track2_top_line",
            ]
        )
        for corpus in CORPORA:
            t1_mbps, t1_cpb = parse_direct_log(corpus, "track1")
            t2_mbps, t2_cpb = parse_direct_log(corpus, "track2")
            t1_profile = Path(ROOT) / "samply" / "profiles" / f"direct__{corpus}__track1.json.gz"
            t2_profile = Path(ROOT) / "samply" / "profiles" / f"direct__{corpus}__track2.json.gz"
            t1_pct, t1_fn, t1_file, t1_line = top_one(t1_profile)
            t2_pct, t2_fn, t2_file, t2_line = top_one(t2_profile)
            writer.writerow(
                [
                    corpus,
                    t1_mbps,
                    f"{t1_cpb:.3f}",
                    t1_pct,
                    t1_fn,
                    t1_file,
                    t1_line,
                    t2_mbps,
                    f"{t2_cpb:.3f}",
                    t2_pct,
                    t2_fn,
                    t2_file,
                    t2_line,
                ]
            )


def load_mode3_rows():
    rows = {}
    with open(Path(ROOT) / "mode3" / "mode3_rows.tsv", newline="") as f:
        for row in csv.DictReader(f, delimiter="\t"):
            if row["rc"] == "0":
                rows[(row["corpus"], row["mode"])] = row
    return rows


def mode_metric(rows, corpus, mode, field):
    return rows[(corpus, mode)][field]


def write_mode3_summary():
    rows = load_mode3_rows()
    with open(Path(OUT) / "mode3_summary.tsv", "w", newline="") as f:
        writer = csv.writer(f, delimiter="\t")
        writer.writerow(
            [
                "corpus",
                "host_call_mbps",
                "host_call_cpb",
                "alternate_scalar_mbps",
                "alternate_scalar_cpb",
                "cold_first_mbps",
                "cold_first_cpb",
                "struct_scalar_mbps",
                "struct_scalar_cpb",
                "struct_scalar_top",
                "struct_scalar_top_pct",
                "struct_simd_mbps",
                "struct_simd_cpb",
                "struct_simd_top",
                "struct_simd_top_pct",
                "simd_vs_scalar_mbps_ratio",
            ]
        )
        for corpus in CORPORA:
            scalar_profile = Path(ROOT) / "mode3" / "profiles" / f"mode3__{corpus}__structural_scan_scalar.json.gz"
            simd_profile = Path(ROOT) / "mode3" / "profiles" / f"mode3__{corpus}__structural_scan_simd.json.gz"
            scalar_pct, scalar_fn, _scalar_file, _scalar_line = top_one(scalar_profile)
            simd_pct, simd_fn, _simd_file, _simd_line = top_one(simd_profile)
            scalar_mbps = float(mode_metric(rows, corpus, "structural_scan_scalar", "mbps"))
            simd_mbps = float(mode_metric(rows, corpus, "structural_scan_simd", "mbps"))
            writer.writerow(
                [
                    corpus,
                    mode_metric(rows, corpus, "host_call_eager_decode", "mbps"),
                    mode_metric(rows, corpus, "host_call_eager_decode", "cycles_per_byte"),
                    mode_metric(rows, corpus, "alternate_scalar_plan", "mbps"),
                    mode_metric(rows, corpus, "alternate_scalar_plan", "cycles_per_byte"),
                    mode_metric(rows, corpus, "cold_first_parse", "mbps"),
                    mode_metric(rows, corpus, "cold_first_parse", "cycles_per_byte"),
                    f"{scalar_mbps:.3f}",
                    mode_metric(rows, corpus, "structural_scan_scalar", "cycles_per_byte"),
                    scalar_fn,
                    scalar_pct,
                    f"{simd_mbps:.3f}",
                    mode_metric(rows, corpus, "structural_scan_simd", "cycles_per_byte"),
                    simd_fn,
                    simd_pct,
                    f"{simd_mbps / scalar_mbps:.2f}",
                ]
            )


if __name__ == "__main__":
    if not os.path.isdir(ROOT):
        raise SystemExit(f"SKV13_P1_ROOT does not exist: {ROOT}")
    write_direct_summary()
    write_mode3_summary()
