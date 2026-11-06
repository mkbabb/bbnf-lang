#!/usr/bin/env python3
"""LW1.TOPO-1 falsifier: every commit SHA cited in the sk-v27 records resolves
against the heads receipt, i.e. it is a commit in one of the receipted repos
and is reachable from a head that heads-W0.json records for that repo.

usage: topo1_check.py <heads-W0.json> [record.md ...]  (default: PLAN, LEDGER, RULINGS)"""
import json, pathlib, re, subprocess, sys

HOME = pathlib.Path.home() / "Programming"
TR = pathlib.Path(__file__).resolve().parent.parent
receipt_path = pathlib.Path(sys.argv[1])
records = [pathlib.Path(p) for p in sys.argv[2:]] or [TR / n for n in ("PLAN.md", "LEDGER.md", "RULINGS.md")]
if not receipt_path.is_file():
    print(f"RED\n  no heads receipt at {receipt_path}"); sys.exit(1)
receipt = json.loads(receipt_path.read_text())["repos"]
TOK = re.compile(r"`([0-9a-f]{7,40})(?=[`:])")

def git(repo, *a, stdin=None):
    return subprocess.run(["git", "-C", str(HOME / repo), *a], input=stdin,
                          capture_output=True, text=True)

heads = {}
for repo, r in receipt.items():
    hs = set(r["remote"].values()) | set(r["local"].values()) | {w["head"] for w in r["worktrees"] if w["head"]}
    heads[repo] = sorted(h for h in hs if git(repo, "cat-file", "-e", h + "^{commit}").returncode == 0)

cited = sorted({t for p in records for t in TOK.findall(p.read_text())})
bad, ok, objs = [], 0, 0
for tok in cited:
    home = [repo for repo in receipt if git(repo, "rev-parse", "--verify", "-q", tok + "^{commit}").returncode == 0]
    if not home:
        if any(git(repo, "cat-file", "-e", tok).returncode == 0 for repo in receipt):
            objs += 1; continue  # a tree or blob id, not a commit
        bad.append(f"{tok}: not a commit or object in any receipted repo"); continue
    reach = [repo for repo in home if git(repo, "rev-list", "--count", tok, "--stdin",
                                          stdin="".join(f"^{h}\n" for h in heads[repo])).stdout.strip() == "0"]
    if reach: ok += 1
    else: bad.append(f"{tok}: commit in {home} but reachable from no receipted head")
print("RED" if bad else "GREEN", f"{ok}/{len(cited) - objs} cited commits resolve ({objs} non-commit object ids)", *bad, sep="\n  ")
sys.exit(1 if bad else 0)
