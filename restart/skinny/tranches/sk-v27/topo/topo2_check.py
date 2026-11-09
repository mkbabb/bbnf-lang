#!/usr/bin/env python3
"""LW1.TOPO-2 falsifier. GREEN iff ownership-W0.json
  (1) cites wf_d837f96c's close by a commit that is on value.js origin and
      whose diff adds the close line to COHESION, and
  (2) lists every held glob PLAN §1 names (the bullets under "Ownership holds")."""
import json, pathlib, re, subprocess, sys

TR = pathlib.Path(__file__).resolve().parent.parent
VJ = pathlib.Path.home() / "Programming" / "value.js"
own = pathlib.Path(sys.argv[1])
if not own.is_file():
    print(f"RED\n  no ownership record at {own}"); sys.exit(1)
o = json.loads(own.read_text())
bad = []
plan = (TR / "PLAN.md").read_text()
block = plan.split("**Ownership holds", 1)[1].split("An edit to a held path", 1)[0]
planned = []
for line in block.splitlines():
    m = re.match(r"- (.*)", line)
    if m:
        planned += re.findall(r"`([^`]+)`", m.group(1)) + (
            ["gen-grammar.mjs", "generated grammar modules"] if "gen-grammar" in m.group(1) else [])
listed = {h["plan_glob"] for h in o.get("holds", [])}
bad += [f"held glob not listed: {g}" for g in planned if g not in listed]
c = o.get("close_record", {})
sha, line = c.get("commit", ""), c.get("line", "")
if not sha:
    bad.append("wf_d837f96c close not cited by commit")
else:
    on = subprocess.run(["git", "-C", str(VJ), "branch", "-r", "--contains", sha], capture_output=True, text=True).stdout
    diff = subprocess.run(["git", "-C", str(VJ), "show", "--format=", sha, "--", "docs/tranches/X/COHESION.md"],
                          capture_output=True, text=True).stdout
    if "origin/" not in on: bad.append(f"close commit {sha} not on value.js origin")
    if not line or ("+" + line) not in diff.replace("+- ", "+"): bad.append(f"close commit {sha} does not add the close line")
print("RED" if bad else "GREEN", f"{len(planned)} planned globs, {len(listed)} listed; close {sha[:10] or '-'}", *bad, sep="\n  ")
sys.exit(1 if bad else 0)
