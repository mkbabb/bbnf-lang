#!/usr/bin/env python3
"""Analyze samply profiles for floor comparators (rapidjson + serde_json).

Mirrors the symbol-table resolver pattern from sonic-rs-v2/analyze.py so leaf
attribution is binary-searched against the presymbolicated `.syms.json` sidecar
rather than relying on the gecko `funcTable.name` (which can be sparse for native
binaries with debug info).

Usage:
  ./analyze.py                # this directory (defaults rapidjson)
  ./analyze.py /path/to/dir   # other directory (e.g. serde_json)
"""

import bisect
import gzip
import json
import sys
from collections import defaultdict
from pathlib import Path

if len(sys.argv) >= 2:
    ROOT = Path(sys.argv[1])
else:
    ROOT = Path(__file__).resolve().parent

CORPORA = ["twitter", "citm", "canada", "apache_builds", "instruments", "random"]

CORPUS_BYTES = {
    "twitter": 631515,
    "citm": 1727204,
    "canada": 2251051,
    "apache_builds": 127275,
    "instruments": 220346,
    "random": 510476,
}


def load_profile(corpus: str):
    profile_path = ROOT / f"{corpus}.profile.json.gz"
    syms_path = ROOT / f"{corpus}.profile.json.syms.json"
    with gzip.open(profile_path, "rb") as f:
        p = json.load(f)
    with open(syms_path) as f:
        syms = json.load(f)
    return p, syms


def load_nm(path: Path):
    """Parse nm output: '<16-hex-rva> <type> <symbol>' (one per line)."""
    rvas = []
    names = []
    with open(path) as f:
        for line in f:
            parts = line.strip().split(maxsplit=2)
            if len(parts) < 3:
                continue
            try:
                rva = int(parts[0], 16)
            except ValueError:
                continue
            t = parts[1]
            if t not in ("t", "T"):
                continue
            rvas.append(rva)
            names.append(parts[2])
    order = sorted(range(len(rvas)), key=lambda i: rvas[i])
    return [rvas[i] for i in order], [names[i] for i in order]


# Auxiliary nm dump (lib_name -> (rvas, names)). Populated lazily by tag.
_NM_DUMPS = {}


def _get_nm_dump(lib_name: str):
    if lib_name in _NM_DUMPS:
        return _NM_DUMPS[lib_name]
    candidate = Path(f"/tmp/{lib_name}.nm.txt")
    if candidate.exists():
        _NM_DUMPS[lib_name] = load_nm(candidate)
    else:
        _NM_DUMPS[lib_name] = ([], [])
    return _NM_DUMPS[lib_name]


def build_resolver(profile, syms):
    libs = profile["libs"]
    lib_name_by_idx = [L["name"] for L in libs]
    sym_strs = syms["string_table"]
    sym_tables_by_lib_name = {}
    for d in syms["data"]:
        ln = d["debug_name"]
        st = sorted(d["symbol_table"], key=lambda s: s["rva"])
        rvas = [s["rva"] for s in st]
        ends = [s["rva"] + s["size"] for s in st]
        names = [
            sym_strs[s["symbol"]] if s.get("symbol") is not None else "<no sym>"
            for s in st
        ]
        sym_tables_by_lib_name[ln] = (rvas, ends, names)

    thread = profile["threads"][0]
    func_resource = thread["funcTable"]["resource"]
    resource_lib = thread["resourceTable"]["lib"]
    addresses = thread["frameTable"]["address"]
    frame_func = thread["frameTable"]["func"]
    func_names = thread["funcTable"]["name"]
    str_arr = thread["stringArray"]

    def resolve(frame_idx):
        addr = addresses[frame_idx]
        f_idx = frame_func[frame_idx]
        lib_idx = resource_lib[func_resource[f_idx]]
        lib_name = lib_name_by_idx[lib_idx] if lib_idx >= 0 else "?"
        # Primary path: binary search syms table by addr
        r = sym_tables_by_lib_name.get(lib_name)
        if r is not None:
            rvas, ends, names = r
            i = bisect.bisect_right(rvas, addr) - 1
            if i >= 0 and addr < ends[i]:
                return (names[i], lib_name)
        # Fallback 1: nm dump for this lib (only for the parser binary)
        if lib_name in ("serde_json_driver", "rapidjson_driver"):
            nm_rvas, nm_names = _get_nm_dump(lib_name)
            if nm_rvas:
                i = bisect.bisect_right(nm_rvas, 0x100000000 + addr) - 1
                if i >= 0:
                    return (nm_names[i], lib_name)
        # Fallback 2: gecko funcTable name if present
        name_idx = func_names[f_idx]
        if name_idx is not None and name_idx >= 0:
            n = str_arr[name_idx]
            # Prefix raw addresses with their lib name for readability
            if n.startswith("0x") and lib_name and lib_name != "?":
                return (f"{lib_name}!{n}", lib_name)
            return (n, lib_name)
        # Fallback 3: tag with library prefix so unresolved addresses are still readable
        return (f"{lib_name}!0x{addr:x}", lib_name)

    return resolve


def compute_times(profile, resolver):
    t = profile["threads"][0]
    stacks = t["stackTable"]
    frames = t["frameTable"]
    samples = t["samples"]

    n_frames = frames["length"]
    frame_resolved = [resolver(i) for i in range(n_frames)]

    stack_prefix = stacks["prefix"]
    stack_frame = stacks["frame"]

    stack_leaf = [None] * stacks["length"]
    stack_lineage = [None] * stacks["length"]

    def leaf(s):
        if stack_leaf[s] is not None:
            return stack_leaf[s]
        f = stack_frame[s]
        stack_leaf[s] = frame_resolved[f][0]
        return stack_leaf[s]

    def lineage(s):
        if stack_lineage[s] is not None:
            return stack_lineage[s]
        chain = set()
        cur = s
        while cur is not None and cur != -1:
            f = stack_frame[cur]
            chain.add(frame_resolved[f][0])
            cur = stack_prefix[cur]
        stack_lineage[s] = chain
        return chain

    self_time = defaultdict(int)
    incl_time = defaultdict(int)

    samples_stack = samples["stack"]
    n = samples["length"]
    for i in range(n):
        s = samples_stack[i]
        if s is None or s == -1:
            continue
        self_time[leaf(s)] += 1
        for sym in lineage(s):
            incl_time[sym] += 1

    return self_time, incl_time, n


def fmt_row(rank, pct, samples, sym, width=110):
    sym_disp = sym if len(sym) <= width else sym[: width - 3] + "..."
    return f"{pct:6.2f}% | {samples:>6} | {sym_disp}"


def section_for(corpus: str, parser_name: str):
    profile, syms = load_profile(corpus)
    resolver = build_resolver(profile, syms)
    self_time, incl_time, total = compute_times(profile, resolver)

    lines = [f"### {corpus} — top 20 by self-time",
             f"",
             f"Samples: {total}",
             f"",
             "```",
             "self%  | samples | symbol",
             "------ | ------- | ----------------------------------------------------------------------"]
    ordered = sorted(self_time.items(), key=lambda kv: -kv[1])[:20]
    for sym, n in ordered:
        pct = 100.0 * n / total
        lines.append(fmt_row(0, pct, n, sym))
    lines.append("```")
    lines.append("")
    return lines, self_time, incl_time, total


def main():
    parser_name = ROOT.name
    print(f"# {parser_name} profile analysis")
    print(f"")
    print(f"Generated from {ROOT}")
    print(f"")
    summary = {}
    for c in CORPORA:
        try:
            lines, self_t, incl_t, total = section_for(c, parser_name)
        except FileNotFoundError:
            continue
        print("\n".join(lines))
        # Hot-leaf count: symbols with >= 1% self time
        hot_leaves = sum(1 for n in self_t.values() if 100.0 * n / total >= 1.0)
        summary[c] = (total, hot_leaves)

    print("## Hot-leaf summary")
    print("")
    print("Corpus | Samples | Hot leaves (>=1% self)")
    print("------ | ------- | ----------------------")
    for c, (total, hot) in summary.items():
        print(f"{c} | {total} | {hot}")


if __name__ == "__main__":
    main()
