#!/usr/bin/env python3
"""Reproduce the SK-V13 S-P1 V2/V3 samply sidecar top-20 extraction.

Inputs default to the V2 capture root because V3 is a documentation/provenance
fold over those measurements.
"""

import bisect
import collections
import csv
import glob
import gzip
import json
import os
import re

ROOT = os.environ.get("SKV13_P1_ROOT", "/tmp/skv13-p1-v2")
OUT = os.path.join(ROOT, "summary")


def load_syms(sidecar):
    with open(sidecar) as f:
        data = json.load(f)
    strings = data["string_table"]
    by_debug = {}
    by_code = {}
    for lib in data["data"]:
        syms = []
        for sym in lib.get("symbol_table", []):
            raw_symbol = sym.get("symbol", 0)
            name = strings[raw_symbol] if isinstance(raw_symbol, int) else str(raw_symbol)
            frames = []
            for frame in sym.get("frames") or []:
                raw_function = frame.get("function")
                raw_file = frame.get("file")
                function = strings[raw_function] if isinstance(raw_function, int) else str(raw_function)
                file_name = strings[raw_file] if isinstance(raw_file, int) else ""
                frames.append((function, file_name, frame.get("line")))
            syms.append((sym["rva"], sym.get("size") or 1, name, frames))
        syms.sort()
        by_debug[lib.get("debug_name")] = syms
        by_code[lib.get("code_id")] = syms
    return by_debug, by_code


def resolve(addr, libidx, libs, by_debug, by_code):
    lib = libs[libidx]
    name = lib.get("debugName") or lib.get("name")
    syms = by_code.get(lib.get("codeId")) or by_debug.get(name) or []
    starts = [sym[0] for sym in syms]
    idx = bisect.bisect_right(starts, addr) - 1
    if idx >= 0:
        rva, size, symbol, frames = syms[idx]
        if addr < rva + size:
            if frames:
                function, file_name, line = frames[0]
                return function, file_name, line, name
            return symbol, "", None, name
    return hex(addr), "", None, name


def top_rows(profile, limit=20):
    by_debug, by_code = load_syms(profile.replace(".json.gz", ".json.syms.json"))
    with gzip.open(profile, "rt") as f:
        data = json.load(f)
    libs = data["libs"]
    thread = data["threads"][0]
    funcs = thread["funcTable"]
    frames = thread["frameTable"]
    stacks = thread["stackTable"]
    samples = thread["samples"]
    resources = thread["resourceTable"]
    strings = thread["stringArray"]

    def string(idx):
        return strings[idx] if isinstance(idx, int) and 0 <= idx < len(strings) else str(idx)

    counts = collections.Counter()
    total = 0
    weights = samples.get("weight") or [1] * samples["length"]
    for stack, weight in zip(samples["stack"], weights):
        if stack is None or stack < 0:
            continue
        frame = stacks["frame"][stack]
        fn = frames["func"][frame]
        resource = funcs["resource"][fn]
        if resource is not None:
            libidx = resources["lib"][resource]
            function, file_name, line, lib = resolve(
                frames["address"][frame], libidx, libs, by_debug, by_code
            )
        else:
            function, file_name, line, lib = string(funcs["name"][fn]), "", None, ""
        counts[(function, file_name, line or "", lib)] += weight or 1
        total += weight or 1
    return total, counts.most_common(limit)


def write_hotleaf():
    os.makedirs(OUT, exist_ok=True)
    with open(os.path.join(OUT, "hotleaf_top20.tsv"), "w", newline="") as f:
        writer = csv.writer(f, delimiter="\t")
        writer.writerow(
            [
                "surface",
                "corpus",
                "mode",
                "rank",
                "pct",
                "samples",
                "total",
                "function",
                "file",
                "line",
                "lib",
                "profile",
            ]
        )
        patterns = [
            ("direct", os.path.join(ROOT, "samply/profiles/direct__*.json.gz"), r"direct__(.*)__(.*)\.json\.gz"),
            ("mode3", os.path.join(ROOT, "mode3/profiles/mode3__*.json.gz"), r"mode3__(.*)__(.*)\.json\.gz"),
            ("css", os.path.join(ROOT, "css/profiles/*.json.gz"), None),
        ]
        for surface, pattern, rx in patterns:
            for profile in sorted(glob.glob(pattern)):
                if rx:
                    match = re.match(rx, os.path.basename(profile))
                    corpus, mode = match.group(1), match.group(2)
                else:
                    corpus, mode = "css_l4_declaration_values", "all_modes"
                total, rows = top_rows(profile)
                for rank, ((function, file_name, line, lib), count) in enumerate(rows, 1):
                    writer.writerow(
                        [
                            surface,
                            corpus,
                            mode,
                            rank,
                            f"{count / total * 100:.2f}" if total else "0.00",
                            count,
                            total,
                            function,
                            file_name,
                            line,
                            lib,
                            profile,
                        ]
                    )


if __name__ == "__main__":
    write_hotleaf()
