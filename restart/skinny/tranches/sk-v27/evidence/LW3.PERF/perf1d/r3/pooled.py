#!/usr/bin/env python3
"""PERF1D-B2 (b): per case, the minimum divan 'fastest' over a read's rounds for base and cure, their ratio
per read, and the pooled figure (the lower of the reads' ratios). Usage: pooled.py <read dir>... ;
files <bench>-<base|cure>-rNN.txt (bench 'datas10x' is data_s at 10x iterations)."""
import re, glob, os, sys
U = {"ns": 1e-3, "µs": 1, "ms": 1e3, "s": 1e6}
def us(v): n, u = v.split(); return float(n) * U[u]
reads = sys.argv[1:]; R = {}
for d in reads:
    for f in glob.glob(os.path.join(d, "*-r*.txt")):
        m = re.match(r"(.*)-(base|cure)-r(\d+)\.txt", os.path.basename(f))
        if not m: continue
        b, v, r = m.groups()
        for line in open(f):
            x = re.match(r"[├╰]─ (\S+)\s+(\S+ \S+)\s+│", line)
            if x: R.setdefault((b, x.group(1)), {}).setdefault(d, {}).setdefault(v, []).append(us(x.group(2)))
names = [os.path.basename(d.rstrip("/")) for d in reads]
print("\t".join(["bench", "case"] + sum([[f"{n}_rounds", f"{n}_base_min_us", f"{n}_cure_min_us", f"{n}_cure/base"] for n in names], []) + ["pooled"]))
worst = 0
for k in sorted(R):
    row = [k[0], k[1]]; ratios = []
    for d in reads:
        x = R[k].get(d, {}); b, c = x.get("base", []), x.get("cure", [])
        if b and c:
            ratio = min(c) / min(b); ratios.append(ratio)
            row += [str(min(len(b), len(c))), "%.2f" % min(b), "%.2f" % min(c), "%.3f" % ratio]
        else: row += ["0", "", "", ""]
    pooled = min(ratios) if ratios else float("nan"); worst = max(worst, pooled)
    row.append("%.3f" % pooled); print("\t".join(row))
print("# worst pooled %.3f" % worst)
