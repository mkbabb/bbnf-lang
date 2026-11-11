#!/usr/bin/env python3
"""Per-function stack frame size from `otool -tvV` aarch64 disassembly.
Frame = sum of `sub sp, sp, #imm[, lsl #12]` in the function's first 24 instructions,
plus inline stack-probe loops (`sub x?, sp, #..` used as probe target)."""
import re, sys, subprocess
sym_re = re.compile(r'^(\S.*):$')
sub_re = re.compile(r'\tsub\tsp, sp, #(0x[0-9a-f]+|\d+)(, lsl #12)?')
probe_re = re.compile(r'\tsub\tx\d+, sp, #(0x[0-9a-f]+|\d+)(, lsl #12)?')
frames = {}; cur=None; n=0
for line in open(sys.argv[1], errors='replace'):
    m = sym_re.match(line)
    if m and not line.startswith('0'):
        cur=m.group(1); n=0; frames[cur]=0; probing=False; continue
    if cur is None: continue
    n+=1
    if n>24: continue
    pr=probe_re.search(line)
    if pr:
        v=int(pr.group(1),0)
        if pr.group(2): v<<=12
        frames[cur]+=v; probing=True; continue
    s=sub_re.search(line)
    if s and globals().get('probing') and ', lsl #12' in line:
        continue
    if s:
        v=int(s.group(1),0)
        if s.group(2): v<<=12
        frames[cur]+=v
top=sorted(frames.items(), key=lambda kv:-kv[1])
lim=int(sys.argv[2]) if len(sys.argv)>2 else 40
names=[k for k,_ in top[:lim]]
dem=subprocess.run(['rustfilt'],input='\n'.join(names),capture_output=True,text=True).stdout.split('\n') if subprocess.run(['which','rustfilt'],capture_output=True).returncode==0 else names
for (k,v),d in zip(top[:lim],dem):
    print(f"{v:>9}  {d[:220]}")
