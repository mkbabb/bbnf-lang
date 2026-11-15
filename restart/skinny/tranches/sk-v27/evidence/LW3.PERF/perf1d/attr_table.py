#!/usr/bin/env python3
"""PERF-1d attribution: json_monolithic base/cure/<variant> over rounds given; median of medians and fastest."""
import re,glob,sys,statistics as st
U={"ns":1e-3,"µs":1,"ms":1e3}
def us(v): n,u=v.split(); return float(n)*U[u]
var,rounds=sys.argv[1],sys.argv[2]
R={}
files=[f for f in glob.glob("json_monolithic-*-r[%s].txt"%rounds)]+glob.glob("%s/json_monolithic-%s-r*.txt"%(var,var))
for f in files:
    tag=re.search(r"-(base|cure|%s)-r\d"%var,f).group(1)
    for line in open(f):
        m=re.match(r"[├╰]─ (\S+)\s+(\S+ \S+)\s+│\s+(\S+ \S+)\s+│\s+(\S+ \S+)\s+│",line)
        if m: R.setdefault(m.group(1),{}).setdefault(tag,[]).append((us(m.group(4)),us(m.group(2))))
print("case\tbase_med\tcure_med\t%s_med\tcure/base\t%s/base\tfastest:cure/base\t%s/base"%(var,var,var))
for k in sorted(R):
    b,c,d=[st.median([x[0] for x in R[k][t]]) for t in ("base","cure",var)]
    bf,cf,df=[min(x[1] for x in R[k][t]) for t in ("base","cure",var)]
    print("%s\t%.1f\t%.1f\t%.1f\t%.3f\t%.3f\t%.3f\t%.3f"%(k,b,c,d,c/b,d/b,cf/bf,df/bf))
