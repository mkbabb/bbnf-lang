"""S14 P4 (S13-G4): ci-ok must need a job that runs bbnf-lang's TypeScript
suite (vitest in typescript/) with the parity test's Rust arm built.
usage: python3 -I ts-gate.py <ci.yml text on stdin>; rc 1 = RED."""
import re, sys
src = sys.stdin.read()
jobs = {}
cur = None
for line in src.splitlines():
    m = re.match(r"^  ([A-Za-z0-9_-]+):\s*$", line)
    if m:
        cur = m.group(1); jobs[cur] = []
    elif cur:
        jobs[cur].append(line)
body = {k: "\n".join(v) for k, v in jobs.items()}
needs = re.search(r"needs:\s*\[([^\]]*)\]", body.get("ci-ok", ""))
needs = [n.strip() for n in needs.group(1).split(",")] if needs else []
ts = [k for k, b in body.items()
      if re.search(r"working-directory:\s*typescript", b) and re.search(r"npm (test|run test)|vitest run", b)]
arm = [k for k in ts if "--bin debug_parse" in body[k]]
print("ci-ok needs:", needs)
print("jobs running the TS suite:", ts, "with the debug_parse arm built:", arm)
ok = any(k in needs for k in arm)
print("GREEN" if ok else "RED: no ci-ok arm runs the TypeScript suite with its Rust parity arm")
sys.exit(0 if ok else 1)
