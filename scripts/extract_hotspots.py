#!/usr/bin/env python3
"""Extract hotspots from samply JSON profiles (Firefox Profiler format).

Usage:
    python3 scripts/extract_hotspots.py profiles/css_bbnf_l4_tailwind.json
    python3 scripts/extract_hotspots.py --top 30 profiles/css_bbnf.json
    python3 scripts/extract_hotspots.py --compare profiles/before.json profiles/after.json
"""

from __future__ import annotations

import argparse
import gzip
import json
import sys
from collections import defaultdict
from dataclasses import dataclass, field
from pathlib import Path

# Functions containing any of these substrings are considered "user code."
USER_CODE_MARKERS = [
    "bbnf",
    "parse_that",
    "css_bbnf",
    "json_bbnf",
    "css_color",
    "css_values",
    "gorgeous",
]


@dataclass
class Hotspot:
    name: str
    self_time_ms: float = 0.0
    total_samples: int = 0


@dataclass
class ProfileSummary:
    hotspots: list[Hotspot] = field(default_factory=list)
    total_time_ms: float = 0.0
    thread_name: str = ""


def load_profile(path: Path) -> dict:
    """Load a samply profile JSON, handling optional gzip compression."""
    raw = path.read_bytes()
    try:
        raw = gzip.decompress(raw)
    except gzip.BadGzipFile:
        pass
    return json.loads(raw)


def is_user_code(func_name: str) -> bool:
    """Return True if the function name looks like user code."""
    lower = func_name.lower()
    return any(marker in lower for marker in USER_CODE_MARKERS)


def extract_hotspots(profile: dict, user_only: bool = True) -> list[ProfileSummary]:
    """Walk all threads, compute per-function self-time from samples."""
    summaries: list[ProfileSummary] = []

    for thread in profile.get("threads", []):
        string_table: list[str] = thread.get("stringTable", [])
        func_table = thread.get("funcTable", {})
        frame_table = thread.get("frameTable", {})
        stack_table = thread.get("stackTable", {})
        samples = thread.get("samples", {})

        # ---- resolve tables ------------------------------------------------
        # funcTable: .name is an array of stringTable indices
        func_names_idx: list[int] = func_table.get("name", [])
        # frameTable: .func is an array of funcTable indices
        frame_funcs: list[int] = frame_table.get("func", [])

        # stackTable: .frame is an array of frameTable indices
        #             .prefix is an array of nullable stackTable indices
        stack_frames: list[int] = stack_table.get("frame", [])
        stack_prefixes: list[int | None] = stack_table.get("prefix", [])

        # samples: .stack is an array of stackTable indices
        #          .time  is an array of timestamps (ms)
        sample_data = samples.get("data", None)
        sample_stacks: list[int | None]
        sample_times: list[float]

        if sample_data is not None:
            # Older format: data is [[stack, time, ...], ...]
            sample_stacks = [row[0] for row in sample_data]
            sample_times = [row[1] for row in sample_data]
        else:
            sample_stacks = samples.get("stack", [])
            sample_times = samples.get("time", [])

        if not sample_times:
            continue

        # ---- helper: resolve stack_index -> leaf function name ---------------
        def leaf_func_name(stack_idx: int | None) -> str | None:
            if stack_idx is None:
                return None
            try:
                frame_idx = stack_frames[stack_idx]
                func_idx = frame_funcs[frame_idx]
                name_idx = func_names_idx[func_idx]
                return string_table[name_idx]
            except (IndexError, TypeError):
                return None

        # ---- accumulate self-time per leaf function -------------------------
        func_self: dict[str, float] = defaultdict(float)
        total_time = 0.0

        for i in range(len(sample_stacks)):
            stack_idx = sample_stacks[i]
            # Compute sample duration (delta to next sample, or 0 for last).
            if i + 1 < len(sample_times):
                dt = sample_times[i + 1] - sample_times[i]
            else:
                dt = 0.0
            if dt < 0:
                dt = 0.0
            total_time += dt

            name = leaf_func_name(stack_idx)
            if name:
                func_self[name] += dt

        if total_time == 0.0:
            continue

        # ---- build hotspot list ---------------------------------------------
        hotspots: list[Hotspot] = []
        for name, self_time in func_self.items():
            if user_only and not is_user_code(name):
                continue
            hotspots.append(Hotspot(name=name, self_time_ms=self_time, total_samples=1))

        hotspots.sort(key=lambda h: h.self_time_ms, reverse=True)

        thread_name = thread.get("name", "unknown")
        summaries.append(
            ProfileSummary(
                hotspots=hotspots,
                total_time_ms=total_time,
                thread_name=thread_name,
            )
        )

    return summaries


def print_hotspots(summaries: list[ProfileSummary], top_n: int = 20) -> None:
    """Pretty-print the top-N hotspots per thread."""
    for summary in summaries:
        if not summary.hotspots:
            continue
        print(f"\n{'=' * 72}")
        print(f"Thread: {summary.thread_name}  |  Total time: {summary.total_time_ms:.1f} ms")
        print(f"{'=' * 72}")
        print(f"{'Rank':<6} {'Self %':>8} {'Self (ms)':>12}  Function")
        print(f"{'-' * 6} {'-' * 8} {'-' * 12}  {'-' * 40}")

        for i, h in enumerate(summary.hotspots[:top_n], 1):
            pct = (h.self_time_ms / summary.total_time_ms * 100) if summary.total_time_ms else 0.0
            print(f"{i:<6} {pct:>7.2f}% {h.self_time_ms:>11.2f}  {h.name}")


def compare_profiles(
    before_path: Path,
    after_path: Path,
    top_n: int = 20,
    user_only: bool = True,
) -> None:
    """Compare two profiles and report regressions / improvements."""
    before = load_profile(before_path)
    after = load_profile(after_path)

    before_summaries = extract_hotspots(before, user_only=user_only)
    after_summaries = extract_hotspots(after, user_only=user_only)

    # Merge all threads into a single self-time map for each profile.
    def merge(summaries: list[ProfileSummary]) -> tuple[dict[str, float], float]:
        merged: dict[str, float] = defaultdict(float)
        total = 0.0
        for s in summaries:
            total += s.total_time_ms
            for h in s.hotspots:
                merged[h.name] += h.self_time_ms
        return dict(merged), total

    before_map, before_total = merge(before_summaries)
    after_map, after_total = merge(after_summaries)

    all_funcs = set(before_map) | set(after_map)

    @dataclass
    class Delta:
        name: str
        before_ms: float
        after_ms: float
        diff_ms: float
        diff_pct: float  # relative change

    deltas: list[Delta] = []
    for fn in all_funcs:
        b = before_map.get(fn, 0.0)
        a = after_map.get(fn, 0.0)
        diff = a - b
        rel = (diff / b * 100) if b > 0 else (100.0 if a > 0 else 0.0)
        deltas.append(Delta(name=fn, before_ms=b, after_ms=a, diff_ms=diff, diff_pct=rel))

    # Sort by absolute regression (largest increase first).
    deltas.sort(key=lambda d: d.diff_ms, reverse=True)

    print(f"\n{'=' * 80}")
    print(f"Profile comparison")
    print(f"  Before: {before_path.name}  ({before_total:.1f} ms)")
    print(f"  After:  {after_path.name}  ({after_total:.1f} ms)")
    total_diff = after_total - before_total
    total_rel = (total_diff / before_total * 100) if before_total else 0.0
    direction = "REGRESSION" if total_diff > 0 else "IMPROVEMENT"
    print(f"  Total:  {total_diff:+.1f} ms ({total_rel:+.1f}%)  [{direction}]")
    print(f"{'=' * 80}")

    # Regressions
    regressions = [d for d in deltas if d.diff_ms > 0.1]
    if regressions:
        print(f"\nRegressions (top {top_n}):")
        print(f"{'Before':>10} {'After':>10} {'Delta':>10} {'Rel%':>8}  Function")
        print(f"{'-' * 10} {'-' * 10} {'-' * 10} {'-' * 8}  {'-' * 40}")
        for d in regressions[:top_n]:
            print(
                f"{d.before_ms:>9.2f} {d.after_ms:>9.2f} {d.diff_ms:>+9.2f} {d.diff_pct:>+7.1f}%  {d.name}"
            )

    # Improvements
    improvements = [d for d in reversed(deltas) if d.diff_ms < -0.1]
    if improvements:
        print(f"\nImprovements (top {top_n}):")
        print(f"{'Before':>10} {'After':>10} {'Delta':>10} {'Rel%':>8}  Function")
        print(f"{'-' * 10} {'-' * 10} {'-' * 10} {'-' * 8}  {'-' * 40}")
        for d in improvements[:top_n]:
            print(
                f"{d.before_ms:>9.2f} {d.after_ms:>9.2f} {d.diff_ms:>+9.2f} {d.diff_pct:>+7.1f}%  {d.name}"
            )

    if not regressions and not improvements:
        print("\nNo significant changes detected.")


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Extract hotspots from samply JSON profiles."
    )
    parser.add_argument(
        "profiles",
        nargs="+",
        type=Path,
        help="Profile JSON file(s). One for hotspot extraction, two for comparison.",
    )
    parser.add_argument(
        "--top",
        type=int,
        default=20,
        help="Number of top hotspots to display (default: 20).",
    )
    parser.add_argument(
        "--compare",
        action="store_true",
        help="Compare two profiles (before, after) and report regressions.",
    )
    parser.add_argument(
        "--all",
        action="store_true",
        help="Include all functions, not just user code.",
    )

    args = parser.parse_args()
    user_only = not args.all

    if args.compare:
        if len(args.profiles) != 2:
            print("Error: --compare requires exactly two profile files.", file=sys.stderr)
            sys.exit(1)
        compare_profiles(args.profiles[0], args.profiles[1], top_n=args.top, user_only=user_only)
    else:
        for path in args.profiles:
            if not path.exists():
                print(f"Error: {path} does not exist.", file=sys.stderr)
                sys.exit(1)
            profile = load_profile(path)
            summaries = extract_hotspots(profile, user_only=user_only)
            print(f"\nProfile: {path.name}")
            print_hotspots(summaries, top_n=args.top)


if __name__ == "__main__":
    main()
