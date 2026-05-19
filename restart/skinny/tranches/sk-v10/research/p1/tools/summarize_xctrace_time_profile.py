#!/usr/bin/env python3
"""Export xctrace Time Profiler traces to per-trace symbol JSON summaries."""

from __future__ import annotations

import argparse
import json
import subprocess
import xml.etree.ElementTree as ET
from collections import Counter
from pathlib import Path


def demangle(symbol: str) -> str:
    try:
        proc = subprocess.run(
            ["rustfilt", symbol],
            check=False,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            text=True,
        )
    except FileNotFoundError:
        return symbol
    result = proc.stdout.strip()
    return result or symbol


def classify(name: str) -> str:
    if "match_tiny_plain_string" in name or "tiny_plain_string_end" in name:
        return "string_tiny_scan"
    if "match_string_at_quote" in name or "skip_string_plain" in name or "skip_plain_string_end" in name:
        return "string_full_scan"
    if "unescape_string" in name or "validate_string_escape" in name:
        return "string_escape"
    if "read_hex_unit_scalar" in name or "hex_nibble" in name:
        return "unicode_escape_hex"
    if "scan_digit_run" in name:
        return "number_digit_scan"
    if "match_number_span_from_first" in name:
        return "number_scan"
    if "skip_ascii_whitespace" in name:
        return "whitespace_skip"
    if "movemask_u8x16" in name:
        return "simd_movemask"
    if "parse_array_element_at_direct" in name or "consume_array_next" in name:
        return "array_walk"
    if "parse_object_value_at_direct" in name:
        return "object_walk"
    if "parse_value_at" in name or "dispatch_value" in name or "parse_pair" in name:
        return "dispatch_walk"
    if "consume_container_next" in name:
        return "structural_rediscovery"
    if "fold_string_scalar" in name or "JsonDigestSink" in name:
        return "direct_struct"
    if "alloc::" in name or "dealloc" in name:
        return "alloc"
    if "copy_nonoverlapping" in name or "memcpy" in name:
        return "memcpy"
    return "other"


def resolve(element: ET.Element | None, by_id: dict[str, ET.Element]) -> ET.Element | None:
    if element is None:
        return None
    ref = element.attrib.get("ref")
    if ref is not None:
        return by_id.get(ref)
    return element


def first_frame(backtrace: ET.Element | None, by_id: dict[str, ET.Element]) -> ET.Element | None:
    trace = resolve(backtrace, by_id)
    if trace is None:
        return None
    for frame in trace:
        if frame.tag == "frame":
            return resolve(frame, by_id)
    return None


def binary_name(frame: ET.Element | None, by_id: dict[str, ET.Element]) -> str:
    if frame is None:
        return ""
    binary = resolve(frame.find("binary"), by_id)
    return "" if binary is None else binary.attrib.get("name", "")


def export_xml(trace: Path) -> ET.Element:
    proc = subprocess.run(
        [
            "xcrun",
            "xctrace",
            "export",
            "--input",
            str(trace),
            "--xpath",
            '/trace-toc/run[@number="1"]/data/table[@schema="time-profile"]',
        ],
        check=True,
        stdout=subprocess.PIPE,
        text=True,
    )
    return ET.fromstring(proc.stdout)


def summarize_trace(trace: Path, process_binary: str) -> dict:
    root = export_xml(trace)
    by_id = {el.attrib["id"]: el for el in root.iter() if "id" in el.attrib}
    weights_total = 0
    weights_process = 0
    by_symbol: Counter[str] = Counter()
    for row in root.iter("row"):
        weight_el = resolve(row.find("weight"), by_id)
        if weight_el is None or weight_el.text is None:
            continue
        weight = int(weight_el.text)
        weights_total += weight
        frame = first_frame(row.find("backtrace"), by_id)
        if process_binary and binary_name(frame, by_id) != process_binary:
            continue
        if frame is None:
            continue
        symbol = frame.attrib.get("name", "")
        if not symbol:
            continue
        weights_process += weight
        by_symbol[symbol] += weight
    top = []
    demangled_cache: dict[str, str] = {}
    for symbol, weight in by_symbol.most_common(20):
        demangled_cache.setdefault(symbol, demangle(symbol))
        top.append(
            {
                "symbol": symbol,
                "demangled": demangled_cache[symbol],
                "class": classify(demangled_cache[symbol]),
                "weight_ns": weight,
                "pct_of_process_time": (weight / weights_process * 100.0) if weights_process else 0.0,
                "pct_of_total_time": (weight / weights_total * 100.0) if weights_total else 0.0,
            }
        )
    return {
        "name": trace.stem,
        "trace": str(trace),
        "samples_total_ns": weights_total,
        "samples_process_ns": weights_process,
        "process_share": (weights_process / weights_total) if weights_total else 0.0,
        "top_process_self_time": top,
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--trace-dir", required=True)
    parser.add_argument("--output-dir", required=True)
    parser.add_argument("--process-binary", required=True)
    args = parser.parse_args()
    trace_dir = Path(args.trace_dir)
    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    summaries = []
    for trace in sorted(trace_dir.glob("*.trace")):
        summary = summarize_trace(trace, args.process_binary)
        summaries.append(summary)
        (output_dir / f"{trace.stem}.symbols.json").write_text(
            json.dumps(summary, indent=2) + "\n",
            encoding="utf-8",
        )
    (output_dir / "summary.json").write_text(
        json.dumps({"source": str(trace_dir), "traces": summaries}, indent=2) + "\n",
        encoding="utf-8",
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
