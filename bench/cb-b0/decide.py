#!/usr/bin/env python3
"""The paired decision rule of bench/PREREG.md §2 (sk-v27 LW2.CON-2, S11 BENCH).

Input: pair records written by `pairs.py run`. Each pair is two fresh processes of one ABBA
block; r = ln(t_a / t_b), t = the runner's wall time of the process (the same input on both
arms, so per-byte and per-process ratios are the same number).

Per cell:
  estimate   Hodges-Lehmann (median of the Walsh averages of r)
  interval   BCa bootstrap, 10,000 resamples, two-sided level 1 - alpha (alpha = 0.01)
  p          one-sided Wilcoxon signed-rank: p_win tests r - delta < 0, p_loss tests r + delta > 0
  margin     delta = ln(0.97)
  Holm       within a family, over p_win and over p_loss separately
  verdict    WIN / LOSS / TIE / UNDECIDED, exactly as §2's table

Floors (LW2-ORDER LW2.CON-2): A/A noise = the larger magnitude of the A/A cell's interval
ends, as a percentage of exp(); the proposed floor = max(10 %, 3 x noise).

Usage:
  decide.py cell DIR [--certify-min-pairs N] [--quiet-only]
  decide.py family DIR... [--certify-min-pairs N] [--quiet-only]
"""
from __future__ import annotations

import argparse
import glob
import json
import math
import os
import sys

import numpy as np
from scipy import stats

ALPHA = 0.01
DELTA = math.log(0.97)
RESAMPLES = 10_000
DISCARD_MAX = 0.01


def hodges_lehmann(x: np.ndarray) -> float:
    x = np.asarray(x, dtype=float)
    i, j = np.triu_indices(len(x))
    return float(np.median((x[i] + x[j]) / 2.0))


def _hl_rows(m: np.ndarray) -> np.ndarray:
    """HL of every row of m (rows are resamples)."""
    n = m.shape[1]
    i, j = np.triu_indices(n)
    w = (m[:, i] + m[:, j]) / 2.0
    return np.median(w, axis=1)


def bca_interval(x: np.ndarray, level: float, seed: int, resamples: int = RESAMPLES) -> tuple[float, float]:
    x = np.asarray(x, dtype=float)
    n = len(x)
    theta = hodges_lehmann(x)
    rng = np.random.default_rng(seed)
    chunk = max(1, min(resamples, int(4e7 // max(1, n * (n + 1) // 2))))
    boot = []
    done = 0
    while done < resamples:
        k = min(chunk, resamples - done)
        boot.append(_hl_rows(x[rng.integers(0, n, size=(k, n))]))
        done += k
    boot = np.concatenate(boot)
    prop = (np.sum(boot < theta) + 0.5 * np.sum(boot == theta)) / resamples
    prop = min(max(prop, 1.0 / resamples), 1.0 - 1.0 / resamples)
    z0 = stats.norm.ppf(prop)
    jack = _hl_rows(np.array([np.delete(x, k) for k in range(n)]))
    d = jack.mean() - jack
    den = 6.0 * (np.sum(d ** 2) ** 1.5)
    a = float(np.sum(d ** 3) / den) if den > 0 else 0.0
    out = []
    for q in ((1 - level) / 2, 1 - (1 - level) / 2):
        z = stats.norm.ppf(q)
        adj = stats.norm.cdf(z0 + (z0 + z) / (1 - a * (z0 + z)))
        out.append(float(np.quantile(boot, adj)))
    return out[0], out[1]


def _wilcoxon(x: np.ndarray, alternative: str) -> float:
    if np.all(x == 0):
        return 1.0
    return float(stats.wilcoxon(x, alternative=alternative, zero_method="wilcox").pvalue)


def holm(ps: list[float]) -> list[float]:
    m = len(ps)
    order = sorted(range(m), key=lambda k: ps[k])
    adj = [0.0] * m
    running = 0.0
    for rank, k in enumerate(order):
        running = max(running, min(1.0, (m - rank) * ps[k]))
        adj[k] = running
    return adj


def verdict(lo: float, hi: float, p_win: float, p_loss: float, n: int, certify_min_pairs: int) -> tuple[str, str]:
    if n < certify_min_pairs:
        return "UNDECIDED", f"{n} pairs, below the {certify_min_pairs} a certifying verdict needs"
    if p_win < ALPHA and hi < DELTA:
        return "WIN", ""
    if p_loss < ALPHA and lo > -DELTA:
        return "LOSS", ""
    if DELTA < lo and hi < -DELTA:
        return "TIE", ""
    return "UNDECIDED", "the interval and the adjusted p meet no other row of PREREG §2"


def decide_cell(rs, seed: int = 20261110, certify_min_pairs: int = 0,
                p_win_adj: float | None = None, p_loss_adj: float | None = None) -> dict:
    x = np.asarray(rs, dtype=float)
    hl = hodges_lehmann(x)
    lo, hi = bca_interval(x, 1 - ALPHA, seed)
    lo95, hi95 = bca_interval(x, 0.95, seed + 1)
    p_win = _wilcoxon(x - DELTA, "less")
    p_loss = _wilcoxon(x + DELTA, "greater")
    pw = p_win if p_win_adj is None else p_win_adj
    pl = p_loss if p_loss_adj is None else p_loss_adj
    v, why = verdict(lo, hi, pw, pl, len(x), certify_min_pairs)
    pct = lambda r: 100.0 * (math.exp(r) - 1.0)  # noqa: E731
    return {"n_pairs": int(len(x)), "hl": hl, "ci99": [lo, hi], "ci95": [lo95, hi95],
            "hl_pct": pct(hl), "ci99_pct": [pct(lo), pct(hi)], "ci95_pct": [pct(lo95), pct(hi95)],
            "median_r": float(np.median(x)), "iqr_r": float(np.subtract(*np.percentile(x, [75, 25]))),
            "mad_r": float(np.median(np.abs(x - np.median(x)))),
            "p_win_raw": p_win, "p_loss_raw": p_loss, "p_win_adj": pw, "p_loss_adj": pl,
            "verdict": v, "undecided_reason": why}


def aa_noise_pct(c: dict) -> float:
    lo, hi = c["ci99"]
    return 100.0 * max(abs(math.exp(lo) - 1.0), abs(math.exp(hi) - 1.0))


def proposed_floor_pct(noise_pct: float) -> float:
    return max(10.0, 3.0 * noise_pct)


def within_floor(c: dict, floor_pct: float) -> bool:
    f = math.log(1.0 + floor_pct / 100.0)
    lo, hi = c["ci99"]
    return -f < lo and hi < f and abs(c["hl"]) < f


def slowdown_detected(c: dict) -> bool:
    return c["verdict"] == "LOSS" or c["ci99"][0] > -DELTA


def screen_pairs(pairs: list[dict]) -> tuple[list[dict], int]:
    kept = [p for p in pairs if p["checksum_a"] == p["checksum_b"]]
    return kept, len(pairs) - len(kept)


def not_comparable(kept: int, discarded: int) -> bool:
    total = kept + discarded
    return total == 0 or discarded / total > DISCARD_MAX


# ── records ───────────────────────────────────────────────────────────────────

def load_cell(path: str, quiet_only: bool) -> dict:
    files = sorted(glob.glob(os.path.join(path, "*.json")))
    recs = [json.load(open(f)) for f in files]
    recs = [r for r in recs if r.get("schema") == "cb-b0-pairs/1"]
    if not recs:
        raise SystemExit(f"no pair records under {path}")
    cells = {r["cell"] for r in recs}
    if len(cells) != 1:
        raise SystemExit(f"{path} mixes cells {sorted(cells)}")
    pairs, blocks_dropped = [], 0
    for r in recs:
        for b in r["blocks"]:
            if quiet_only and not b["quiet"]:
                blocks_dropped += 1
                continue
            pairs.extend(b["pairs"])
    kept, discarded = screen_pairs(pairs)
    return {"cell": cells.pop(), "records": [os.path.basename(f) for f in files], "invocations": len(recs),
            "blocks_dropped_noisy": blocks_dropped, "pairs_total": len(pairs), "pairs_discarded": discarded,
            "kept": kept, "statuses": sorted({r["status"] for r in recs}),
            "load_max": max(max(b["load_before"]) for r in recs for b in r["blocks"]),
            "qos_ui_fraction_min": min(p[s]["qos_ui_fraction"] for r in recs for b in r["blocks"]
                                       for p in b["pairs"] for s in ("a", "b"))}


def family(paths: list[str], certify_min_pairs: int, quiet_only: bool) -> list[dict]:
    loaded = [load_cell(p, quiet_only) for p in paths]
    raw = []
    for c in loaded:
        if not c["kept"] or not_comparable(len(c["kept"]), c["pairs_discarded"]):
            raw.append(None)
            continue
        raw.append(decide_cell([p["r"] for p in c["kept"]], certify_min_pairs=certify_min_pairs))
    live = [k for k, d in enumerate(raw) if d is not None]
    pw = holm([raw[k]["p_win_raw"] for k in live])
    pl = holm([raw[k]["p_loss_raw"] for k in live])
    out = []
    for c, d in zip(loaded, raw):
        base = {k: v for k, v in c.items() if k != "kept"}
        if d is None:
            base.update({"verdict": "NOT-COMPARABLE", "n_pairs": len(c["kept"])})
        else:
            k = live.index(loaded.index(c))
            d = decide_cell([p["r"] for p in c["kept"]], certify_min_pairs=certify_min_pairs,
                            p_win_adj=pw[k], p_loss_adj=pl[k])
            base.update(d)
        out.append(base)
    return out


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="verb", required=True)
    for name in ("cell", "family"):
        s = sub.add_parser(name)
        s.add_argument("dirs", nargs="+")
        s.add_argument("--certify-min-pairs", type=int, default=200)
        s.add_argument("--quiet-only", action="store_true")
    a = ap.parse_args(argv)
    res = family(a.dirs, a.certify_min_pairs, a.quiet_only)
    print(json.dumps(res if a.verb == "family" else res[0], indent=1))
    return 0


if __name__ == "__main__":
    sys.exit(main())
