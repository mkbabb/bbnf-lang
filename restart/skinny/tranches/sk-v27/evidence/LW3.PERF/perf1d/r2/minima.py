#!/usr/bin/env python3
"""PERF-1d: per bench case, minimum of divan 'fastest' over all rounds and median of medians,
each variant against base. Usage: minima.py <dir> <variant>... (files <bench>-<variant>-rNN.txt)."""
import re,glob,os,sys,statistics as st
U={"ns":1e-3,"µs":1,"ms":1e3,"s":1e6}
def us(v): n,u=v.split(); return float(n)*U[u]
d=sys.argv[1]; vs=sys.argv[2:]; R={}; n={}
for f in glob.glob(os.path.join(d,"*-r*.txt")):
    m=re.match(r"(.*)-(%s)-r(\d+)\.txt"%"|".join(["base"]+vs),os.path.basename(f))
    if not m: continue
    b,v,r=m.groups()
    for line in open(f):
        x=re.match(r"[├╰]─ (\S+)\s+(\S+ \S+)\s+│\s+(\S+ \S+)\s+│\s+(\S+ \S+)\s+│",line)
        if x: R.setdefault((b,x.group(1)),{}).setdefault(v,[]).append((us(x.group(2)),us(x.group(4))))
hdr=["bench","case","rounds","base_min_us"]+["%s_min/base"%v for v in vs]+["%s_medmed/base"%v for v in vs]
print("\t".join(hdr))
for k in sorted(R):
    b=R[k]["base"]; bm=min(x[0] for x in b); bmm=st.median(x[1] for x in b)
    row=[k[0],k[1],str(min(len(R[k].get(v,[])) for v in ["base"]+vs)),"%.1f"%bm]
    row+=["%.3f"%(min(x[0] for x in R[k][v])/bm) for v in vs]
    row+=["%.3f"%(st.median(x[1] for x in R[k][v])/bmm) for v in vs]
    print("\t".join(row))
