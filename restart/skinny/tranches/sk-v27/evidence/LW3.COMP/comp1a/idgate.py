#!/usr/bin/env python3
"""idgate.py <e31-ids> <base-ids> <head-kernel-list> <head-list>: COMP-1a's gate.
(1) the kernel law ids at the head equal e31fbfedc's; (2) the head's whole
suite minus the kernel's own targets and doc-tests equals the base suite."""
import subprocess, sys
here = __file__.rsplit("/", 1)[0]
def ids(log):
    out = subprocess.run([sys.executable, f"{here}/idset.py", log], capture_output=True, text=True).stdout
    return set(l for l in out.split("\n") if l)
e31 = set(l for l in open(sys.argv[1]).read().split("\n") if l)
base = set(l for l in open(sys.argv[2]).read().split("\n") if l)
hk = ids(sys.argv[3]); head = ids(sys.argv[4])
bad = 0
print(f"kernel laws: e31={len(e31)} head={len(hk)} equal={e31 == hk}")
for x in sorted(e31 ^ hk): print("  KERNEL DIFF", x); bad += 1
KT = ("kernel:", "kernel_allocation:", "kernel_regex:")
own = {x for x in head if x.startswith(KT) or (x.startswith("doc:parse_that\t") and "src/kernel/" in x)}
legacy = head - own
print(f"legacy suite: base={len(base)} head={len(legacy)} (head total {len(head)}, kernel-owned {len(own)}) equal={legacy == base}")
for x in sorted(base ^ legacy): print("  LEGACY DIFF", x); bad += 1
for x in sorted(own): print("  kernel-owned", x)
print("GREEN" if bad == 0 else "RED")
sys.exit(1 if bad else 0)
