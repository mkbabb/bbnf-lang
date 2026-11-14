#!/usr/bin/env python3
"""PERF-1d: divan median/fastest per bench per round, base vs cure, from bench/*.txt."""
import re, sys, glob, os, statistics as st
U={"ns":1e-3,"µs":1,"us":1,"ms":1e3,"s":1e6}
def us(v): n,u=v.split(); return float(n)*U[u]
d=sys.argv[1]; R={}
for f in glob.glob(os.path.join(d,"*-r[0-9].txt")):
    b,tag,r=re.match(r"(.*)-(base|cure)-r(\d)\.txt",os.path.basename(f)).groups()
    for line in open(f):
        m=re.match(r"[├╰]─ (\S+)\s+(\S+ \S+)\s+│\s+(\S+ \S+)\s+│\s+(\S+ \S+)\s+│",line)
        if m: R.setdefault((b,m.group(1)),{}).setdefault(tag,[]).append((int(r),us(m.group(2)),us(m.group(4))))
print("bench\tcase\tbase_median_us(r1,r2,r3)\tcure_median_us(r1,r2,r3)\tmedian_of_medians_ratio_cure/base\tmin_fastest_ratio_cure/base")
for k in sorted(R):
    b=sorted(R[k]["base"]); c=sorted(R[k]["cure"])
    bm=[x[2] for x in b]; cm=[x[2] for x in c]
    print("%s\t%s\t%s\t%s\t%.3f\t%.3f"%(k[0],k[1],",".join("%.1f"%x for x in bm),",".join("%.1f"%x for x in cm),
        st.median(cm)/st.median(bm), min(x[1] for x in c)/min(x[1] for x in b)))
