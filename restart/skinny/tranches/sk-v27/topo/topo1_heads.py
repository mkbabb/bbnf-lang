#!/usr/bin/env python3
"""LW1.TOPO-1: heads receipt. `git ls-remote origin` plus every local branch
head and every worktree HEAD, for the three programme repos and the three
consumers whose SHAs the records cite. Read-only: no checkout, no fetch here.

Writes <out>/heads-<stem>.json and <out>/CUSTODY-RECEIPT.tsv.
usage: topo1_heads.py <out> [stem]  (stem defaults to W0; LW2.TOPO-0 passes LW2)"""
import datetime, json, pathlib, subprocess, sys

HOME = pathlib.Path.home() / "Programming"
REPOS = {"bbnf-lang": "core", "parse-that": "core", "pprint": "core",
         "value.js": "consumer", "keyframes.js": "consumer", "fourier-analysis": "consumer"}
out = pathlib.Path(sys.argv[1]); out.mkdir(parents=True, exist_ok=True)
stem = sys.argv[2] if len(sys.argv) > 2 else "W0"

def git(repo, *a):
    return subprocess.run(["git", "-C", str(HOME / repo), *a], check=True,
                          capture_output=True, text=True).stdout

receipt = {"taken": datetime.datetime.now().astimezone().isoformat(timespec="seconds"), "repos": {}}
rows = ["repo\trole\tplace\tref\tsha\ton_origin"]
for repo, role in REPOS.items():
    url = git(repo, "remote", "get-url", "origin").strip()
    remote = {}
    for line in git(repo, "ls-remote", "origin").splitlines():
        sha, ref = line.split("\t")
        if not ref.endswith("^{}"):
            remote[ref] = sha
    local = dict(l.split(" ", 1) for l in
                 git(repo, "for-each-ref", "--format=%(refname) %(objectname)", "refs/heads").splitlines())
    wts, cur = [], {}
    for line in git(repo, "worktree", "list", "--porcelain").splitlines() + [""]:
        if not line:
            if cur: wts.append(cur); cur = {}
            continue
        k, _, v = line.partition(" ")
        cur[k] = v or True
    on_origin = set(remote.values())
    receipt["repos"][repo] = {"role": role, "origin": url, "remote": remote, "local": local,
                              "worktrees": [{"path": w.get("worktree"), "head": w.get("HEAD"),
                                             "branch": w.get("branch")} for w in wts]}
    for ref, sha in sorted(remote.items()):
        rows.append(f"{repo}\t{role}\torigin\t{ref}\t{sha}\tyes")
    for ref, sha in sorted(local.items()):
        rows.append(f"{repo}\t{role}\tlocal\t{ref}\t{sha}\t{'yes' if sha in on_origin else 'no'}")
    for w in wts:
        if w.get("HEAD"):
            rows.append(f"{repo}\t{role}\tworktree:{w['worktree']}\t{w.get('branch', 'detached')}\t{w['HEAD']}\t{'yes' if w['HEAD'] in on_origin else 'no'}")
(out / f"heads-{stem}.json").write_text(json.dumps(receipt, indent=1, sort_keys=True) + "\n")
(out / "CUSTODY-RECEIPT.tsv").write_text("\n".join(rows) + "\n")
print(len(rows) - 1, "rows")
