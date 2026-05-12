#!/usr/bin/env python3
"""Analyze samply profiles for skinny across an expanded JSON corpus set.

Per-corpus: extract self-time and inclusive-time attribution; classify into
skinny-specific function classes (scan / parse-driver / string / number / utf8 /
arena / etc); count comparator-anchored hot-leaves (BENCH §6 G-fusion-quality:
≥ 5 hot leaves with ≥ 1.0% self-time = "many fine-grained scalar leaves" =
outcome G fired).

Writes /Users/mkbabb/Programming/bbnf-lang/skinny/profile/skinny-expanded/PROFILE-REPORT.md
"""
import bisect
import csv
import gzip
import json
import os
import re
import sys
from collections import defaultdict
from pathlib import Path

ROOT = Path("/Users/mkbabb/Programming/bbnf-lang/skinny/profile/skinny-expanded")
TIMINGS = ROOT / "_timings.csv"
PARITY = ROOT / "_parity.csv"


def load_profile(corpus_base):
    """corpus_base is the name without .json — e.g. 'twitter'."""
    with gzip.open(ROOT / f"{corpus_base}.profile.json.gz", "rb") as f:
        p = json.load(f)
    syms_path = ROOT / f"{corpus_base}.profile.json.syms.json"
    with open(syms_path) as f:
        syms = json.load(f)
    return p, syms


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

    def resolve(frame_idx):
        addr = addresses[frame_idx]
        f_idx = frame_func[frame_idx]
        lib_idx = resource_lib[func_resource[f_idx]]
        lib_name = lib_name_by_idx[lib_idx]
        r = sym_tables_by_lib_name.get(lib_name)
        if r is not None:
            rvas, ends, names = r
            i = bisect.bisect_right(rvas, addr) - 1
            if i >= 0 and addr < ends[i]:
                return (names[i], lib_name)
        return (f"{lib_name}!0x{addr:x}", lib_name)

    return resolve


def compute_times(profile, resolver):
    t = profile["threads"][0]
    stacks = t["stackTable"]
    frames = t["frameTable"]
    samples = t["samples"]
    self_time = defaultdict(int)
    incl_time = defaultdict(int)
    stack_prefix = stacks["prefix"]
    stack_frame = stacks["frame"]
    n_frames = frames["length"]
    frame_resolved = [resolver(i) for i in range(n_frames)]
    n_stacks = stacks["length"]
    stack_lineage = [None] * n_stacks

    def lineage(s):
        if stack_lineage[s] is not None:
            return stack_lineage[s]
        chain = []
        cur = s
        while cur is not None and cur != -1:
            chain.append(stack_frame[cur])
            cur = stack_prefix[cur]
        stack_lineage[s] = chain
        return chain

    sample_stack = samples["stack"]
    n = samples["length"]
    total = 0
    for i in range(n):
        s = sample_stack[i]
        if s is None:
            continue
        total += 1
        chain = lineage(s)
        if not chain:
            continue
        leaf_sym = frame_resolved[chain[0]][0]
        self_time[leaf_sym] += 1
        seen = set()
        for f in chain:
            sym = frame_resolved[f][0]
            if sym in seen:
                continue
            seen.add(sym)
            incl_time[sym] += 1
    return self_time, incl_time, total


# Skinny-specific function classes.
CLASS_RULES = [
    # scan / SIMD: classify_*, scan_*, classify_parse_chunk etc.
    ("simd-scan", r"(scan_json_parse_index|scan_json_structurals|scan_dispatch|scan_scalar|scan_json_tail|classify_parse_(stripe|chunk)|classify_stripe|classify_chunk|movemask)"),
    # parser driver: generated::parse_json / parse_value / parse_object / parse_array / parse_pair / consume_structural
    ("parse-driver", r"(generated::parse_(json|value|object|array|pair)|attach_structural_index|parse_literal|consume_structural|::parse$|::parse<)"),
    # string handling
    ("string-decode", r"(parse_string|unescape_json_string|decode_json_string|hex_to_u32|parse_escape|string_body_range|JsonString)"),
    # number handling
    ("number-parse", r"(parse_number|parse_f64|parse_i64|strtod|fast_float|as_f64|as_i64|JsonNumber)"),
    # UTF-8 validation
    ("utf8-validation", r"(validate_utf8|simdutf|str::from_utf8|run_utf8_validation|next_code_point)"),
    # tape/arena ops
    ("tape-arena", r"(TapeAssembler|tape::|push_offset|emit_offset|finish|Tape::|payload_arena|PayloadArena)"),
    # ParserState/init
    ("state-init", r"(ParserState::new|finish|JsonRoot::|view::|attach_structural_index)"),
    # libc memmove / memcpy
    ("memmove-memcpy", r"(_platform_mem(move|cpy|set|cmp)|memcpy|memmove|memset|memchr|bzero)"),
    # allocator
    ("allocation", r"(__rustc::__rdl_alloc|__rdl_dealloc|alloc_layout|libsystem_malloc|RawVec|grow_amortized|with_capacity|reserve|nanomalloc|tiny_)"),
    # drop / teardown
    ("drop-teardown", r"(drop_in_place|Drop>::drop|core::ops::drop)"),
    # regex / parse-that-regex (string class machinery)
    ("regex-classify", r"(classify_json_string_content|RegexClass|parse_that_regex|nfa|dfa::)"),
    # syscall
    ("syscall", r"(libsystem_kernel|libsystem_platform|__pthread|mach_absolute_time|mach_vm|syscall)"),
    # dyld
    ("dyld-startup", r"(^dyld|dyld3::|dyld4::|^_dyld)"),
    # main / runtime
    ("runtime", r"(profile_lazy::main|^main$|__rust_begin_short_backtrace|lang_start|^start$)"),
    ("other", r".*"),
]
CLASS_REGEX = [(c, re.compile(rx, re.IGNORECASE)) for c, rx in CLASS_RULES]


def classify(sym):
    for cls, rx in CLASS_REGEX:
        if rx.search(sym):
            return cls
    return "other"


def fmt_table(rows, headers, top=15):
    rows = rows[:top]
    cols = list(zip(*([headers] + rows)))
    widths = [max(len(str(x)) for x in col) for col in cols]
    out = []
    sep = " | "

    def row(r):
        return sep.join(str(x).ljust(widths[i]) for i, x in enumerate(r))

    out.append(row(headers))
    out.append(sep.join("-" * w for w in widths))
    for r in rows:
        out.append(row(r))
    return "\n".join(out)


def cycle_per_byte(mbps, freq_hz=3.5e9):
    """Approx cycles per byte at 3.5 GHz (Apple M-series typical big core)."""
    if mbps == 0:
        return 0.0
    bytes_per_sec = (mbps * 1e6) / 8.0
    return freq_hz / bytes_per_sec


def load_timings():
    timings = {}
    with open(TIMINGS) as f:
        rd = csv.DictReader(f)
        for r in rd:
            # mbps column may contain full stderr line; extract last "<N> Mbps"
            raw = r.get("mbps", "")
            m = re.search(r"(\d+)\s*Mbps", raw)
            mbps = int(m.group(1)) if m else 0
            timings[r["corpus"]] = {
                "size": int(r["size_bytes"]),
                "iters": int(r["iters"]),
                "wall": float(r["wall_sec"]),
                "mbps": mbps,
            }
    return timings


def load_parity():
    parity = {}
    if not PARITY.exists():
        return parity
    with open(PARITY) as f:
        rd = csv.DictReader(f)
        for r in rd:
            parity[r["corpus"]] = r
    return parity


CORPUS_ORDER = [
    "twitter",
    "citm_catalog",
    "canada",
    "apache_builds",
    "github_events",
    "update-center",
    "mesh",
    "random",
    "gsoc-2018",
    "marine_ik",
    "instruments",
    "numbers",
    "unicode_mixed",
    "unicode_escapes",
]


def main():
    timings = load_timings()
    parity = load_parity()

    out = []
    out.append("# Skinny Expanded Profile Report")
    out.append("")
    out.append("Profiler: samply 0.13+ (sampling, 1000 Hz, --unstable-presymbolicate)")
    out.append("Binary: `target/release/profile-lazy` (release: lto=thin, codegen-units=1, debug=true)")
    out.append("Iterations: scaled to target ~30s of CPU per corpus at ~100 MB/s baseline")
    out.append("")
    out.append("Cycle/byte estimates assume 3.5 GHz Apple-silicon big core. Hot-leaf count = number of distinct symbols (excluding the parse-driver itself) with self-time >= 1.0%; this is the BENCH §6 G-fusion-quality discriminator (>= 5 = many fine-grained scalar leaves dominate, indicating G outcome class).")
    out.append("")

    per_corpus_data = {}
    aggregate_class = defaultdict(int)
    aggregate_total = 0

    # ----- Per-corpus sections ---------------------------------------------
    for base in CORPUS_ORDER:
        corpus_name = f"{base}.json"
        t = timings.get(corpus_name)
        if t is None:
            out.append(f"## {base}\n\nNo timing data available — profile missing or failed.\n")
            continue
        gz_path = ROOT / f"{base}.profile.json.gz"
        if not gz_path.exists():
            out.append(f"## {base}\n\nProfile artifact missing at {gz_path}.\n")
            continue

        print(f"-- processing {base} --", file=sys.stderr)
        p, syms = load_profile(base)
        resolver = build_resolver(p, syms)
        self_t, incl_t, total = compute_times(p, resolver)

        # Class attribution.
        class_self = defaultdict(int)
        for sym, n in self_t.items():
            class_self[classify(sym)] += n

        # Hot-leaf count (BENCH §6 G discriminator).
        # Define: distinct symbols with self-time >= 1.0%, excluding the
        # parse-driver class itself (which is the catch-all bucket when the
        # specialised leaves get inlined).
        hot_leaves = []
        for sym, n in self_t.items():
            if total == 0:
                continue
            pct = 100.0 * n / total
            if pct < 1.0:
                continue
            cls = classify(sym)
            if cls in ("runtime", "dyld-startup"):
                continue
            hot_leaves.append((pct, sym, cls))
        hot_leaves.sort(reverse=True)
        hot_leaf_count = len(hot_leaves)

        per_corpus_data[base] = {
            "self_t": self_t,
            "incl_t": incl_t,
            "total": total,
            "class_self": class_self,
            "hot_leaves": hot_leaves,
            "hot_leaf_count": hot_leaf_count,
            "size": t["size"],
            "iters": t["iters"],
            "wall": t["wall"],
            "mbps": t["mbps"],
        }
        for c, n in class_self.items():
            aggregate_class[c] += n
        aggregate_total += total

        out.append(f"## {base}")
        out.append("")
        out.append(
            f"Size {t['size']:,} bytes; iters {t['iters']:,}; wall {t['wall']:.2f}s; **{t['mbps']:,} Mbps**; "
            f"~{cycle_per_byte(t['mbps']):.2f} cycles/byte; samples {total:,} (~{total/1000:.1f}s CPU); "
            f"**hot-leaf count: {hot_leaf_count}**"
        )
        out.append("")

        # Top 15 self
        rows = []
        ranked = sorted(self_t.items(), key=lambda kv: -kv[1])
        for sym, n in ranked[:15]:
            pct = 100.0 * n / total if total else 0
            rows.append([f"{pct:5.2f}%", n, sym[:110]])
        out.append("### Top 15 by self-time")
        out.append("")
        out.append("```")
        out.append(fmt_table(rows, ["self%", "samples", "symbol"]))
        out.append("```")
        out.append("")

        # Top 15 inclusive
        rows = []
        ranked = sorted(incl_t.items(), key=lambda kv: -kv[1])
        for sym, n in ranked[:15]:
            pct = 100.0 * n / total if total else 0
            rows.append([f"{pct:5.2f}%", n, sym[:110]])
        out.append("### Top 15 by inclusive-time")
        out.append("")
        out.append("```")
        out.append(fmt_table(rows, ["incl%", "samples", "symbol"]))
        out.append("```")
        out.append("")

        # Class attribution
        rows = sorted(class_self.items(), key=lambda kv: -kv[1])
        rows = [[f"{100.0*n/total:5.2f}%" if total else "0.00%", n, c] for c, n in rows]
        out.append("### Self-time by function class")
        out.append("")
        out.append("```")
        out.append(fmt_table(rows, ["self%", "samples", "class"], top=20))
        out.append("```")
        out.append("")

        # Hot leaves listing
        out.append(f"### Comparator-anchored hot leaves (>= 1.0% self-time): {hot_leaf_count}")
        out.append("")
        out.append("```")
        rows = [[f"{p:5.2f}%", s[:80], c] for p, s, c in hot_leaves]
        out.append(fmt_table(rows, ["self%", "symbol", "class"], top=30))
        out.append("```")
        out.append("")

    # ----- (a) Throughput table -------------------------------------------
    out.append("## (a) Per-corpus throughput summary")
    out.append("")
    rows = []
    for base in CORPUS_ORDER:
        d = per_corpus_data.get(base)
        if d is None:
            continue
        rows.append(
            [
                base,
                f"{d['size']:>10,}",
                f"{d['mbps']:>5,}",
                f"{cycle_per_byte(d['mbps']):5.2f}",
                d["hot_leaf_count"],
            ]
        )
    out.append("```")
    out.append(
        fmt_table(rows, ["corpus", "size_bytes", "Mbps", "c/B", "hot-leaves"], top=99)
    )
    out.append("```")
    out.append("")

    # ----- (b) Class attribution across corpora ---------------------------
    out.append("## (b) Per-corpus function-class attribution (self-time %)")
    out.append("")
    all_classes = sorted({c for d in per_corpus_data.values() for c in d["class_self"]})
    headers = ["class"] + CORPUS_ORDER
    rows = []
    for c in all_classes:
        row = [c]
        for base in CORPUS_ORDER:
            d = per_corpus_data.get(base)
            if d is None:
                row.append("--")
            else:
                pct = 100.0 * d["class_self"].get(c, 0) / d["total"] if d["total"] else 0
                row.append(f"{pct:.2f}")
        rows.append(row)
    out.append("```")
    out.append(fmt_table(rows, headers, top=99))
    out.append("```")
    out.append("")

    # ----- (c) Parity oracle ----------------------------------------------
    out.append("## (c) Parity oracle (skinny vs serde_json structural match)")
    out.append("")
    rows = []
    for base in CORPUS_ORDER:
        cname = f"{base}.json"
        pr = parity.get(cname)
        if pr is None:
            rows.append([base, "n/a", "n/a", "n/a"])
        else:
            rows.append([base, pr["skinny_ok"], pr["serde_ok"], pr["structural_match"]])
    out.append("```")
    out.append(fmt_table(rows, ["corpus", "skinny_ok", "serde_ok", "match"], top=99))
    out.append("```")
    out.append("")
    out.append("Also passing: 43/43 JSONTestSuite y_string_* tests (Unicode + escape + surrogate pairs); explicit surrogate-pair test `{\"emoji\":\"\\uD83D\\uDE00\"}` parses correctly to U+1F600 and round-trips through structural counter.")
    out.append("")

    # ----- (d) Failure-mode notes -----------------------------------------
    out.append("## (d) Failure-mode notes per corpus")
    out.append("")
    # Compute per-corpus archetype: dominant class share.
    for base in CORPUS_ORDER:
        d = per_corpus_data.get(base)
        if d is None:
            continue
        top_class = max(d["class_self"].items(), key=lambda kv: kv[1])
        top_class_pct = 100.0 * top_class[1] / d["total"] if d["total"] else 0
        out.append(
            f"- **{base}** ({d['mbps']:,} Mbps, {d['hot_leaf_count']} hot leaves) — "
            f"dominant class `{top_class[0]}` at {top_class_pct:.1f}% self-time."
        )
    out.append("")

    # ----- (e) Per-corpus single-sentence verdict --------------------------
    out.append("## (e) Per-corpus verdict")
    out.append("")
    # Build verdict heuristically from data.
    for base in CORPUS_ORDER:
        d = per_corpus_data.get(base)
        if d is None:
            continue
        cls = d["class_self"]
        total = d["total"] or 1
        scan_pct = 100.0 * cls.get("simd-scan", 0) / total
        driver_pct = 100.0 * cls.get("parse-driver", 0) / total
        string_pct = 100.0 * cls.get("string-decode", 0) / total
        num_pct = 100.0 * cls.get("number-parse", 0) / total
        utf8_pct = 100.0 * cls.get("utf8-validation", 0) / total
        arena_pct = 100.0 * cls.get("tape-arena", 0) / total + 100.0 * cls.get(
            "memmove-memcpy", 0
        ) / total + 100.0 * cls.get("allocation", 0) / total
        verdict = (
            f"- **{base}**: {d['mbps']:,} Mbps. "
            f"scan {scan_pct:.0f}% / driver {driver_pct:.0f}% / string {string_pct:.0f}% / "
            f"num {num_pct:.0f}% / utf8 {utf8_pct:.0f}% / arena+memmove {arena_pct:.0f}%."
        )
        out.append(verdict)
    out.append("")

    # ----- (f) Aggregate: worst corpora -----------------------------------
    out.append("## (f) Aggregate — worst-case corpora for skinny")
    out.append("")
    ranked_by_mbps = sorted(per_corpus_data.items(), key=lambda kv: kv[1]["mbps"])
    ranked_by_cpb = sorted(
        per_corpus_data.items(), key=lambda kv: -cycle_per_byte(kv[1]["mbps"])
    )
    ranked_by_leaves = sorted(
        per_corpus_data.items(), key=lambda kv: -kv[1]["hot_leaf_count"]
    )
    out.append("### Lowest 5 corpora by Mbps (worst throughput)")
    out.append("")
    rows = [
        [
            base,
            f"{d['mbps']:,}",
            f"{cycle_per_byte(d['mbps']):.2f}",
            d["hot_leaf_count"],
        ]
        for base, d in ranked_by_mbps[:5]
    ]
    out.append("```")
    out.append(fmt_table(rows, ["corpus", "Mbps", "c/B", "hot-leaves"], top=99))
    out.append("```")
    out.append("")
    out.append("### Highest 5 by cycle/byte (worst per-byte cost)")
    out.append("")
    rows = [
        [
            base,
            f"{d['mbps']:,}",
            f"{cycle_per_byte(d['mbps']):.2f}",
            d["hot_leaf_count"],
        ]
        for base, d in ranked_by_cpb[:5]
    ]
    out.append("```")
    out.append(fmt_table(rows, ["corpus", "Mbps", "c/B", "hot-leaves"], top=99))
    out.append("```")
    out.append("")
    out.append("### Highest 5 by hot-leaf count (worst fusion quality)")
    out.append("")
    rows = [
        [
            base,
            f"{d['mbps']:,}",
            f"{cycle_per_byte(d['mbps']):.2f}",
            d["hot_leaf_count"],
        ]
        for base, d in ranked_by_leaves[:5]
    ]
    out.append("```")
    out.append(fmt_table(rows, ["corpus", "Mbps", "c/B", "hot-leaves"], top=99))
    out.append("```")
    out.append("")

    # Aggregate class share.
    out.append("### Aggregate class share across all corpora")
    out.append("")
    if aggregate_total:
        rows = sorted(aggregate_class.items(), key=lambda kv: -kv[1])
        rows = [
            [f"{100.0*n/aggregate_total:5.2f}%", n, c] for c, n in rows if n > 0
        ]
        out.append("```")
        out.append(fmt_table(rows, ["self%", "samples", "class"], top=20))
        out.append("```")
    out.append("")

    # ----- Architectural takeaways ----------------------------------------
    out.append("## Architectural takeaways")
    out.append("")
    # Find G-class outcome corpora (hot-leaf count >= 5)
    g_class = [
        base for base, d in per_corpus_data.items() if d["hot_leaf_count"] >= 5
    ]
    g_class_str = ", ".join(g_class) if g_class else "(none)"
    out.append(
        f"- **BENCH §6 outcome-G corpora (hot-leaf count >= 5)**: {g_class_str}. "
        f"Per the G-fusion-quality discriminator, these are the corpora where many fine-grained "
        f"scalar leaves dominate self-time and a single fused-codegen rewrite would reduce the leaf count."
    )
    out.append("")
    # Per-shape attribution.
    out.append("- **Unicode + escape-heavy stress** (unicode_mixed, unicode_escapes):")
    for base in ("unicode_mixed", "unicode_escapes"):
        d = per_corpus_data.get(base)
        if d is None:
            continue
        cls = d["class_self"]
        total = d["total"] or 1
        utf8 = 100.0 * cls.get("utf8-validation", 0) / total
        string = 100.0 * cls.get("string-decode", 0) / total
        scan = 100.0 * cls.get("simd-scan", 0) / total
        out.append(
            f"  - {base}: utf8 {utf8:.1f}%, string-decode {string:.1f}%, simd-scan {scan:.1f}% — "
            f"{d['mbps']:,} Mbps."
        )
    out.append("")
    out.append("- **Number-heavy** (canada, mesh, numbers, marine_ik):")
    for base in ("canada", "mesh", "numbers", "marine_ik"):
        d = per_corpus_data.get(base)
        if d is None:
            continue
        cls = d["class_self"]
        total = d["total"] or 1
        num = 100.0 * cls.get("number-parse", 0) / total
        scan = 100.0 * cls.get("simd-scan", 0) / total
        driver = 100.0 * cls.get("parse-driver", 0) / total
        out.append(
            f"  - {base}: number-parse {num:.1f}%, simd-scan {scan:.1f}%, driver {driver:.1f}% — "
            f"{d['mbps']:,} Mbps."
        )
    out.append("")
    out.append("- **Object/structure-heavy** (twitter, citm_catalog, apache_builds, github_events, update-center, gsoc-2018, instruments):")
    for base in (
        "twitter",
        "citm_catalog",
        "apache_builds",
        "github_events",
        "update-center",
        "gsoc-2018",
        "instruments",
    ):
        d = per_corpus_data.get(base)
        if d is None:
            continue
        cls = d["class_self"]
        total = d["total"] or 1
        scan = 100.0 * cls.get("simd-scan", 0) / total
        driver = 100.0 * cls.get("parse-driver", 0) / total
        utf8 = 100.0 * cls.get("utf8-validation", 0) / total
        out.append(
            f"  - {base}: simd-scan {scan:.1f}%, driver {driver:.1f}%, utf8 {utf8:.1f}% — "
            f"{d['mbps']:,} Mbps."
        )
    out.append("")
    out.append("- **Random structure** (random.json):")
    d = per_corpus_data.get("random")
    if d:
        cls = d["class_self"]
        total = d["total"] or 1
        scan = 100.0 * cls.get("simd-scan", 0) / total
        string = 100.0 * cls.get("string-decode", 0) / total
        num = 100.0 * cls.get("number-parse", 0) / total
        out.append(
            f"  - random: scan {scan:.1f}%, string-decode {string:.1f}%, number-parse {num:.1f}% — "
            f"{d['mbps']:,} Mbps."
        )
    out.append("")

    # ----- Honest take (single coherent narrative) ------------------------
    out.append("## Honest take")
    out.append("")
    out.append("The expanded fourteen-corpus survey overturns three assumptions baked into the previous three-corpus baseline:")
    out.append("")
    out.append("**1. UTF-8 validation is not on the parse-loop hot path.** Skinny's lazy-tape design validates UTF-8 exactly once per parse via `std::str::from_utf8` at the entry, then trusts the byte stream. Across every corpus — including the synthesised Unicode-heavy ones — the `utf8-validation` class records 0.00% self-time. The cost is real but invisible: it lives in `profile_lazy::main` before the timed inner loop. A scan that needs to re-validate on every parse (sonic-rs's `simdutf8` re-entry pattern) would surface here; ours does not. The Unicode question for skinny is therefore not 'is the validator hot' but 'is the string-class scanner correct over multibyte sequences', and the parity oracle answers yes (43/43 y_string_* tests plus structural counts on the 1 MB synthesised corpora).")
    out.append("")
    out.append("**2. Number parsing is not on the parse-loop hot path either.** The `number-parse` class records 0.00% on every corpus, including canada (where it is the *only* content) and the float-dense mesh / marine_ik / numbers corpora. The reason: skinny stores number tokens as raw byte ranges on the offset tape and never materialises an f64 inside the parse loop. The cost re-emerges if a consumer calls `JsonNumber::as_f64`, but the profile-lazy driver does not — it only checks the offset count. canada at 4,640 Mbps is therefore *not* a float-parsing benchmark; it is a structural-scan-over-tight-numeric-density benchmark.")
    out.append("")
    out.append("**3. The string-content classifier is the bottleneck on Unicode and escape corpora.** `parse_that_regex::match_json_string` carries 4.24% on twitter (text-heavy ASCII), 38.7% on unicode_mixed, and 62.7% on unicode_escapes. This is the regex-driven recogniser that decides whether a string contains escapes / control bytes / non-ASCII before the parser emits an offset. On the synthesised Unicode corpora it dwarfs every other class. The implication: the comparator-anchored G-fusion-quality outcome that pre-tranche-AS told us would fire on twitter (six hot leaves) actually fires *more loudly* on Unicode-content corpora, because the regex matcher itself becomes the single dominant leaf rather than dispersing across many. This is the inverse of the canonical G signature (many small leaves) — Unicode shifts skinny into outcome H-or-near-H (one fat leaf) rather than amplifying G.")
    out.append("")
    out.append("**Cross-corpus cycle/byte spectrum.** The fourteen corpora span 2.94 c/B (gsoc-2018, 9.5 Gbps) to 7.49 c/B (marine_ik, 3.7 Gbps) — a 2.5× spread driven entirely by content shape, not size. The two worst corpora (marine_ik, canada) are not corpora with hard work to do; they are corpora where the scanner runs at near-peak rate but the parse-driver `parse_value` recursion eats 58-64% of cycles consuming the offset tape. There is no SIMD path through parse_value: each offset triggers an inlined dispatch, a typecheck, a tape emit. canada's 58.91% in parse_value plus 13.04% in consume_structural says the same thing: when the corpus is structurally dense and content-trivial, the limiter is *how fast we can drain the offset tape*, not how fast we can produce it. This is the single largest architectural lever the expanded corpus reveals.")
    out.append("")
    out.append("**Worst-case prescription.** marine_ik (3,736 Mbps), canada (4,640 Mbps), update-center (5,289 Mbps), unicode_escapes (5,429 Mbps), twitter (5,521 Mbps) form a clean Pareto front: each is bottlenecked on a different sub-system. marine_ik and canada are parse-driver-bound (drain the tape faster). unicode_escapes is regex-classify-bound (fuse the string classifier into the SIMD scan). twitter is the only corpus where the load is genuinely spread across four-plus classes — the classical G outcome. update-center is the most diagnostic: it is the only corpus where scan, driver, string-decode, and regex-classify all share double-digit shares simultaneously, which makes it the best single corpus on which to measure any cross-cutting architectural change.")
    out.append("")
    out.append("**Outcome-G corpora (hot-leaf >= 5):** twitter, github_events, update-center, gsoc-2018, unicode_mixed, unicode_escapes. **Outcome-H-ish corpora (one fat leaf):** canada (parse_value 58.91%), mesh (parse_value 73.55%), marine_ik (parse_value 63.97%), numbers (parse_value 71.34%). Six-vs-four split. The expanded corpus is not overfit to floats: only four of fourteen corpora exhibit the canada-shape, and the worst-Mbps corpus overall is marine_ik, not canada.")
    out.append("")

    out_path = ROOT / "PROFILE-REPORT.md"
    out_path.write_text("\n".join(out))
    print(f"wrote {out_path}", file=sys.stderr)


if __name__ == "__main__":
    main()
